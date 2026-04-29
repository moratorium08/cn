(** Emit a self-contained C harness that runs each predicate qualifier in
    PRE mode against a recorded heap and reports the resulting ghost-state
    delta as the qualifier's footprint.

    Generated C is symbolic: per-qualifier functions look up free
    variables at runtime against a static [ALL_DPS] table.  See the
    .mli for the contract. *)

module CF = Cerb_frontend
module StdList = Stdlib.List
module BT = BaseTypes
module IT = IndexTerms
module Records = Fulminate.Records
module Internal = Fulminate.Internal
module Cn_to_ail = Fulminate.Cn_to_ail

type dp_entry =
  { dp_idx : int;
    dp : Data_point.data_point;
    heap_words : (int64 * int64) list
  }

type input =
  { filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file;
    pred_defs : Definition.Predicate.t Sym.Map.t;
    data_points : dp_entry list;
    qualifiers : (int * Qualifier.t) list;
    output_json_path : string
  }

let buf_add b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'


(* ---------- BaseTypes → C type / cn-conversion mapping ---------- *)

(** [(c_cast, convert_fn_name)] for [bt].  [c_cast] is the cast prefix
    applied to the raw value (whether literal or [fp_var_value(dp,..)]).
    [convert_fn_name] is the runtime helper used to wrap the value as a
    [cn_*]. *)
let bt_to_c_value_info : BT.t -> string * string = function
  | BT.Loc _ -> ("(void*)", "convert_to_cn_pointer")
  | BT.Bits (sign, sz) ->
    let signed = match sign with BT.Signed -> true | BT.Unsigned -> false in
    let prefix = if signed then "i" else "u" in
    let c_int =
      Printf.sprintf "%sint%d_t" (if signed then "" else "u") sz
    in
    let cast = Printf.sprintf "(%s)" c_int in
    (cast, Printf.sprintf "convert_to_cn_bits_%s%d" prefix sz)
  | BT.Bool -> ("(_Bool)", "convert_to_cn_bool")
  | BT.Integer -> ("(int64_t)", "convert_to_cn_integer")
  | bt ->
    failwith
      (Printf.sprintf
         "fp_codegen: unsupported iarg base type: %s"
         (Pp.plain (BT.pp bt)))


(* ---------- Render an IT.t as a C raw-value expression ---------- *)

(** Render an [IT.t] used at base type [bt] as a C expression that
    produces a raw value of the storage type matching [bt].  Free
    symbols become [fp_var_value(dp, "<name>")] calls; constants become
    literals.  Caller has pre-validated that every symbol resolves
    against the dp this expression will run against. *)
let render_raw_value ~(bt : BT.t) (term : IT.t) : string =
  let cast, _ = bt_to_c_value_info bt in
  match term with
  | Terms.IT (Sym sym, _, _) ->
    let name = Sym.pp_string sym in
    Printf.sprintf "%sfp_var_value(dp, \"%s\")" cast name
  | Terms.IT (Const c, _, _) ->
    (match c with
     | Terms.Z z -> Printf.sprintf "%s%sLL" cast (Z.to_string z)
     | Terms.Bits (_, z) -> Printf.sprintf "%s%sLL" cast (Z.to_string z)
     | Terms.Bool b -> if b then "1" else "0"
     | Terms.Null -> "(void*)0"
     | Terms.Unit -> "0"
     | _ ->
       failwith
         (Printf.sprintf
            "fp_codegen: unsupported constant in qualifier iarg: %s"
            (Pp.plain (IT.pp term))))
  | _ ->
    failwith
      (Printf.sprintf
         "fp_codegen: unsupported IT shape in qualifier iarg: %s"
         (Pp.plain (IT.pp term)))


let wrap_convert ~(bt : BT.t) (raw_expr : string) : string =
  let _, convert_fn = bt_to_c_value_info bt in
  Printf.sprintf "%s(%s)" convert_fn raw_expr


(* ---------- Free-symbol set of a qualifier ---------- *)

let free_syms_of_term (term : IT.t) : string list =
  match term with
  | Terms.IT (Sym sym, _, _) -> [ Sym.pp_string sym ]
  | _ -> []


let free_syms_of_qualifier (q : Qualifier.t) : string list =
  match q with
  | Request.P { pointer; iargs; _ } ->
    let names =
      free_syms_of_term pointer @ StdList.concat_map free_syms_of_term iargs
    in
    StdList.sort_uniq Stdlib.String.compare names
  | _ -> []


let dp_has_all_syms (dp : Data_point.data_point) (names : string list) : bool =
  StdList.for_all
    (fun name ->
       StdList.exists
         (fun (v : Data_point.var_binding) -> String.equal v.name name)
         dp.pre_vars)
    names


(* ---------- Per-qualifier C function emission ---------- *)

(** Returns [Some (pred_c_name, args)] when the qualifier is a predicate
    application that the codegen knows how to render; otherwise [None]
    (caller drops the qualifier). *)
let render_qualifier_call
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      (q : Qualifier.t)
  : (string * string list) option
  =
  match q with
  | Request.P { name = PName pred_sym; pointer; iargs } ->
    (match Sym.Map.find_opt pred_sym pred_defs with
     | None -> None
     | Some (pred_def : Definition.Predicate.t) ->
       let iarg_bts = StdList.map snd pred_def.iargs in
       if StdList.length iarg_bts <> StdList.length iargs then
         None
       else (
         let pointer_arg =
           wrap_convert ~bt:(BT.Loc ()) (render_raw_value ~bt:(BT.Loc ()) pointer)
         in
         let iarg_args =
           StdList.map2
             (fun bt it -> wrap_convert ~bt (render_raw_value ~bt it))
             iarg_bts
             iargs
         in
         Some (Sym.pp_string pred_sym, pointer_arg :: iarg_args)))
  | _ -> None


let emit_dp_idxs_array ~(name : string) (idxs : int list) : string =
  let b = Buffer.create 64 in
  Buffer.add_string b (Printf.sprintf "static const int %s[] = {" name);
  StdList.iter (fun i -> Buffer.add_string b (Printf.sprintf " %d," i)) idxs;
  buf_add b " };";
  buf_add
    b
    (Printf.sprintf "static const size_t %s_N = %d;" name (StdList.length idxs));
  Buffer.contents b


let emit_qualifier_fn
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(data_points : dp_entry list)
      ((q_idx, q) : int * Qualifier.t)
  : string
  =
  let needed_syms = free_syms_of_qualifier q in
  let valid_dp_idxs =
    StdList.filter_map
      (fun (e : dp_entry) ->
         if dp_has_all_syms e.dp needed_syms then Some e.dp_idx else None)
      data_points
  in
  let idxs_name = Printf.sprintf "Q%d_DP_IDXS" q_idx in
  let idxs_decl = emit_dp_idxs_array ~name:idxs_name valid_dp_idxs in
  let body =
    match render_qualifier_call ~pred_defs q with
    | Some (pred_c_name, args) ->
      let arg_str = String.concat ",\n          " (args @ [ "PRE"; "(void*)0" ]) in
      Printf.sprintf
        "    fp_setup(dp);\n\
        \    if (sigsetjmp(fp_jmp, 1) == 0) {\n\
        \      (void)%s(\n          %s);\n\
        \      fp_emit_footprint(out, %d, dp->dp_idx, FP_PREDICATE_DEPTH);\n\
        \    } else {\n\
        \      fp_emit_null(out, %d, dp->dp_idx);\n\
        \    }\n\
        \    fp_teardown();\n"
        pred_c_name
        arg_str
        q_idx
        q_idx
    | None ->
      Printf.sprintf "    fp_emit_null(out, %d, dp->dp_idx);\n" q_idx
  in
  Printf.sprintf
    "%s\n\
     static void run_q%d(FILE *out) {\n\
    \  for (size_t i = 0; i < %s_N; i++) {\n\
    \    const fp_dp_t *dp = &ALL_DPS[%s[i]];\n\
     %s\
    \  }\n\
     }\n"
    idxs_decl
    q_idx
    idxs_name
    idxs_name
    body


(* ---------- Static dp / heap / vars tables ---------- *)

let emit_dp_tables (data_points : dp_entry list) : string =
  let b = Buffer.create 1024 in
  buf_add
    b
    "typedef struct fp_heap_word { uintptr_t addr; uint64_t value; } fp_heap_word_t;";
  buf_add
    b
    "typedef struct fp_var { const char *name; uintptr_t value; size_t size; } fp_var_t;";
  buf_add
    b
    "typedef struct fp_dp {\n\
    \  int dp_idx;\n\
    \  const fp_heap_word_t *heap; size_t heap_n;\n\
    \  const fp_var_t       *vars; size_t vars_n;\n\
     } fp_dp_t;";
  StdList.iter
    (fun (e : dp_entry) ->
       Buffer.add_string
         b
         (Printf.sprintf "static const fp_heap_word_t HEAP_DP%d[] = {" e.dp_idx);
       StdList.iter
         (fun (a, v) ->
            Buffer.add_string
              b
              (Printf.sprintf "\n  { 0x%LxUL, 0x%LxUL }," a v))
         e.heap_words;
       buf_add b "\n};";
       buf_add
         b
         (Printf.sprintf
            "static const size_t HEAP_DP%d_N = sizeof HEAP_DP%d / sizeof HEAP_DP%d[0];"
            e.dp_idx
            e.dp_idx
            e.dp_idx);
       Buffer.add_string
         b
         (Printf.sprintf "static const fp_var_t VARS_DP%d[] = {" e.dp_idx);
       StdList.iter
         (fun (v : Data_point.var_binding) ->
            Buffer.add_string
              b
              (Printf.sprintf "\n  { \"%s\", 0x%LxUL, %d }," v.name v.value v.size))
         e.dp.pre_vars;
       buf_add b "\n};";
       buf_add
         b
         (Printf.sprintf
            "static const size_t VARS_DP%d_N = sizeof VARS_DP%d / sizeof VARS_DP%d[0];"
            e.dp_idx
            e.dp_idx
            e.dp_idx))
    data_points;
  Buffer.add_string b "static const fp_dp_t ALL_DPS[] = {";
  StdList.iter
    (fun (e : dp_entry) ->
       Buffer.add_string
         b
         (Printf.sprintf
            "\n  { %d, HEAP_DP%d, HEAP_DP%d_N, VARS_DP%d, VARS_DP%d_N },"
            e.dp_idx
            e.dp_idx
            e.dp_idx
            e.dp_idx
            e.dp_idx))
    data_points;
  buf_add b "\n};";
  buf_add
    b
    "static const size_t ALL_DPS_N = sizeof ALL_DPS / sizeof ALL_DPS[0];";
  Buffer.contents b


(* ---------- Static harness runtime body ---------- *)

let harness_runtime_body =
  {|
extern struct rmap *cn_ownership_global_ghost_state;

static sigjmp_buf fp_jmp;
static const fp_dp_t *fp_active_dp = NULL;

static void fp_failure_cb(enum cn_failure_mode m, enum spec_mode sm) {
  (void)m;
  (void)sm;
  siglongjmp(fp_jmp, 1);
}

static int fp_lookup_byte(const fp_dp_t *dp, uintptr_t addr, unsigned char *out) {
  for (size_t i = 0; i < dp->heap_n; i++) {
    uintptr_t base = dp->heap[i].addr;
    if (addr >= base && addr < base + 8) {
      uint64_t v = dp->heap[i].value;
      *out = ((const unsigned char *)&v)[addr - base];
      return 1;
    }
  }
  return 0;
}

static _Bool fp_load_hook(const void *p, size_t sz, void *dst) {
  if (!fp_active_dp) return 0;
  uintptr_t addr = (uintptr_t)p;
  unsigned char *d = (unsigned char *)dst;
  for (size_t k = 0; k < sz; k++) {
    if (!fp_lookup_byte(fp_active_dp, addr + k, &d[k])) return 0;
  }
  return 1;
}

static uintptr_t fp_var_value(const fp_dp_t *dp, const char *name) {
  for (size_t i = 0; i < dp->vars_n; i++) {
    if (strcmp(dp->vars[i].name, name) == 0) return dp->vars[i].value;
  }
  fprintf(stderr, "fp harness: missing var '%s' for dp %d\n", name, dp->dp_idx);
  exit(2);
}

#define FP_PREDICATE_DEPTH 2

static void fp_setup(const fp_dp_t *dp) {
  fp_active_dp = dp;
  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  ghost_stack_depth_incr(); /* depth=1, caller frame */
  for (size_t i = 0; i < dp->heap_n; i++) {
    cn_assume_ownership((void *)dp->heap[i].addr, 8, "fp");
  }
  ghost_stack_depth_incr(); /* depth=2, predicate frame */
  cn_load_hook = fp_load_hook;
  set_cn_failure_cb(fp_failure_cb);
}

static void fp_teardown(void) {
  cn_load_hook = NULL;
  reset_cn_failure_cb();
  fp_active_dp = NULL;
}

struct fp_collect_ctx {
  FILE *out;
  signed long depth;
  _Bool first;
};

static void fp_collect_cb(rmap_key_t k0, rmap_key_t k1, rmap_value_t v, void *ctx_) {
  struct fp_collect_ctx *ctx = (struct fp_collect_ctx *)ctx_;
  if ((signed long)v != ctx->depth) return;
  for (rmap_key_t addr = k0; addr <= k1; addr++) {
    fprintf(ctx->out, "%s%lu", ctx->first ? "" : ",", (unsigned long)addr);
    ctx->first = 0;
  }
}

static _Bool fp_first;

static void fp_emit_footprint(FILE *out, int q_idx, int dp_idx, signed long depth) {
  if (!fp_first) fputs(",", out);
  fp_first = 0;
  fprintf(out, "{\"q\":%d,\"dp\":%d,\"addrs\":[", q_idx, dp_idx);
  struct fp_collect_ctx ctx = { .out = out, .depth = depth, .first = 1 };
  rmap_foreach(cn_ownership_global_ghost_state, fp_collect_cb, &ctx);
  fputs("]}", out);
}

static void fp_emit_null(FILE *out, int q_idx, int dp_idx) {
  if (!fp_first) fputs(",", out);
  fp_first = 0;
  fprintf(out, "{\"q\":%d,\"dp\":%d,\"addrs\":null}", q_idx, dp_idx);
}
|}


(* ---------- main() ---------- *)

let emit_main (qualifiers : (int * Qualifier.t) list) (output_json_path : string)
  : string
  =
  let b = Buffer.create 256 in
  buf_add b "int main(void) {";
  buf_add b "  fulminate_init();";
  buf_add
    b
    (Printf.sprintf "  FILE* out = fopen(\"%s\", \"w\");" output_json_path);
  buf_add b "  if (!out) { perror(\"fp harness fopen\"); return 2; }";
  buf_add b "  fputs(\"{\\\"results\\\":[\", out);";
  buf_add b "  fp_first = 1;";
  StdList.iter
    (fun (q_idx, _) -> buf_add b (Printf.sprintf "  run_q%d(out);" q_idx))
    qualifiers;
  buf_add b "  fputs(\"]}\", out);";
  buf_add b "  fclose(out);";
  buf_add b "  fulminate_destroy();";
  buf_add b "  return 0;";
  buf_add b "}";
  Buffer.contents b


(* ---------- Top-level emit ---------- *)

let emit (input : input) : string =
  Records.populate_record_map [] input.prog5;
  let sigm = input.ail_prog in
  let c_predicate_defs, c_predicate_decls, _ =
    Internal.generate_c_predicates
      input.filename
      false
      input.cabs_tunit
      input.prog5
      sigm
  in
  let c_function_defs, c_function_decls, _ =
    Internal.generate_c_functions input.filename input.cabs_tunit input.prog5 sigm
  in
  let conversion_function_defs, conversion_function_decls =
    Internal.generate_conversion_and_equality_functions input.filename sigm
  in
  let ownership_function_defs, ownership_function_decls =
    Internal.generate_ownership_functions false !Cn_to_ail.ownership_ctypes
  in
  let ordered_ail_tag_defs = Internal.order_ail_tag_definitions sigm.tag_definitions in
  let c_tag_defs = Internal.generate_c_tag_def_strs ordered_ail_tag_defs in
  let cn_converted_struct_defs = Internal.generate_cn_versions_of_structs ordered_ail_tag_defs in
  let record_fun_defs, record_fun_decls = Records.generate_c_record_funs sigm in
  let record_defs = Records.generate_all_record_strs () in
  let c_datatype_defs = Internal.generate_c_datatypes sigm in
  let datatype_strs = String.concat "\n" (StdList.map snd c_datatype_defs) in
  let q_fns =
    StdList.map
      (emit_qualifier_fn ~pred_defs:input.pred_defs ~data_points:input.data_points)
      input.qualifiers
  in
  let b = Buffer.create 4096 in
  buf_add b "/* Auto-generated bi-abductive footprint harness. */";
  buf_add b "#include <setjmp.h>";
  buf_add b "#include <stddef.h>";
  buf_add b "#include <stdio.h>";
  buf_add b "#include <stdint.h>";
  buf_add b "#include <stdlib.h>";
  buf_add b "#include <string.h>";
  buf_add b "#include <cn-executable/utils.h>";
  buf_add b "#include <cn-executable/bi_abduction.h>";
  buf_add b "#include <cn-executable/rmap.h>";
  buf_add b "_Noreturn void abort(void);";
  buf_add b "/* TAG DEFINITIONS */";
  buf_add b c_tag_defs;
  buf_add b "/* CN VERSIONS OF STRUCTS */";
  buf_add b cn_converted_struct_defs;
  if not (String.length record_defs = 0) then (
    buf_add b "/* RECORDS */";
    buf_add b record_defs);
  buf_add b "/* CN DATATYPES */";
  buf_add b datatype_strs;
  buf_add b "/* DECLARATIONS */";
  buf_add b ownership_function_decls;
  buf_add b conversion_function_decls;
  buf_add b record_fun_decls;
  buf_add b c_function_decls;
  buf_add b c_predicate_decls;
  buf_add b "/* DEFINITIONS */";
  buf_add b record_fun_defs;
  buf_add b conversion_function_defs;
  buf_add b ownership_function_defs;
  buf_add b c_function_defs;
  buf_add b c_predicate_defs;
  buf_add b "/* HARNESS DATA TABLES */";
  buf_add b (emit_dp_tables input.data_points);
  buf_add b "/* HARNESS RUNTIME */";
  buf_add b harness_runtime_body;
  StdList.iter (buf_add b) q_fns;
  buf_add b (emit_main input.qualifiers input.output_json_path);
  Buffer.contents b
