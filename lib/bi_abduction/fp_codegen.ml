(** Emit a self-contained C harness that runs each predicate qualifier in
    PRE mode against a reconstructed heap and reports the resulting
    ghost-state delta as the qualifier's footprint. *)

module CF = Cerb_frontend
module StdList = Stdlib.List
module BT = BaseTypes
module IT = IndexTerms
module Records = Fulminate.Records
module Internal = Fulminate.Internal
module Cn_to_ail = Fulminate.Cn_to_ail

type input =
  { filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file;
    pred_defs : Definition.Predicate.t Sym.Map.t;
    representative_dp : Data_point.data_point;
    heap_words : (int64 * int64) list;
    qualifiers : (int * Qualifier.t) list;
    output_json_path : string
  }

let buf_add b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'


(* ---------- BaseTypes → C type / cn-conversion mapping ---------- *)

(** [(c_storage_type, c_cast, convert_fn_name)] for [bt].
    [c_storage_type] is the local C type used to hold the raw value before
    converting it to a CN value.  [c_cast] is the cast prefix applied to
    the [int64] coming out of the data point (or to a literal).  The
    [convert_fn_name] is the runtime helper used to wrap the value as a
    [cn_*]. *)
let bt_to_c_value_info : BT.t -> string * string * string = function
  | BT.Loc _ -> ("uintptr_t", "(void*)", "convert_to_cn_pointer")
  | BT.Bits (sign, sz) ->
    let signed = (match sign with BT.Signed -> true | BT.Unsigned -> false) in
    let prefix = if signed then "i" else "u" in
    let c_int = Printf.sprintf "%sint%d_t" (if signed then "" else "u") sz in
    let cast = Printf.sprintf "(%s)" c_int in
    (c_int, cast, Printf.sprintf "convert_to_cn_bits_%s%d" prefix sz)
  | BT.Bool -> ("_Bool", "(_Bool)", "convert_to_cn_bool")
  | BT.Integer -> ("int64_t", "(int64_t)", "convert_to_cn_integer")
  | bt ->
    failwith
      (Printf.sprintf
         "fp_codegen: unsupported iarg base type: %s"
         (Pp.plain (BT.pp bt)))


(* ---------- Resolve an IT value against pre_vars to a C-value expression ---------- *)

let pre_var_value (dp : Data_point.data_point) (sym : Sym.t) : int64 option =
  let name = Sym.pp_string sym in
  StdList.find_map
    (fun (v : Data_point.var_binding) ->
       if String.equal v.name name then Some v.value else None)
    dp.pre_vars


(** Render an [IT.t] used at base type [bt] as a C expression that produces a
    raw value of [c_storage_type].  Symbols are resolved via the given data
    point's [pre_vars]; constants are rendered as literals.  Returns [None]
    if the IT cannot be resolved (caller should drop this qualifier). *)
let render_raw_value
      ~(dp : Data_point.data_point)
      ~(bt : BT.t)
      (term : IT.t)
  : string option
  =
  let _, cast, _ = bt_to_c_value_info bt in
  match term with
  | Terms.IT (Sym sym, _, _) ->
    (match pre_var_value dp sym with
     | Some v ->
       (* Render the raw int64 value, then cast to the storage type. *)
       Some (Printf.sprintf "%s%LdLL" cast v)
     | None -> None)
  | Terms.IT (Const c, _, _) ->
    (match c with
     | Terms.Z z -> Some (Printf.sprintf "%s%sLL" cast (Z.to_string z))
     | Terms.Bits (_, z) -> Some (Printf.sprintf "%s%sLL" cast (Z.to_string z))
     | Terms.Bool b -> Some (if b then "1" else "0")
     | Terms.Null -> Some "(void*)0"
     | Terms.Unit -> Some "0"
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


(** Wrap a raw-value expression in the appropriate [convert_to_cn_*] call. *)
let wrap_convert ~(bt : BT.t) (raw_expr : string) : string =
  let _, _, convert_fn = bt_to_c_value_info bt in
  Printf.sprintf "%s(%s)" convert_fn raw_expr


(* ---------- Per-qualifier C function emission ---------- *)

let emit_qualifier_call
      ~(dp : Data_point.data_point)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      (q : Qualifier.t)
  : (string * string list) option
  =
  match q with
  | Request.P { name = PName pred_sym; pointer; iargs } ->
    (match Sym.Map.find_opt pred_sym pred_defs with
     | None -> None
     | Some pred_def ->
       let pointer_raw = render_raw_value ~dp ~bt:(BT.Loc ()) pointer in
       let iarg_bts = StdList.map snd pred_def.iargs in
       let iarg_raws =
         try
           Some (StdList.map2 (fun bt it -> render_raw_value ~dp ~bt it) iarg_bts iargs)
         with Invalid_argument _ -> None
       in
       (match (pointer_raw, iarg_raws) with
        | Some p, Some raws when StdList.for_all Option.is_some raws ->
          let raws = StdList.map Option.get raws in
          let pointer_arg = wrap_convert ~bt:(BT.Loc ()) p in
          let iarg_args =
            StdList.map2
              (fun bt raw -> wrap_convert ~bt raw)
              iarg_bts
              raws
          in
          Some (Sym.pp_string pred_sym, pointer_arg :: iarg_args)
        | _ -> None))
  | _ -> None


let emit_qualifier_fn
      ~(dp : Data_point.data_point)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ((q_idx, q) : int * Qualifier.t)
  : string
  =
  let body =
    match emit_qualifier_call ~dp ~pred_defs q with
    | Some (pred_c_name, args) ->
      let arg_str = String.concat ",\n      " (args @ [ "PRE"; "(void*)0" ]) in
      Printf.sprintf
        "    if (sigsetjmp(fp_jmp, 1) == 0) {\n\
        \      (void)%s(\n      %s);\n\
        \      fp_emit_footprint(out, %d, &fp_first, FP_PREDICATE_DEPTH);\n\
        \    } else {\n\
        \      fp_emit_null(out, %d, &fp_first);\n\
        \    }\n"
        pred_c_name
        arg_str
        q_idx
        q_idx
    | None ->
      Printf.sprintf "    fp_emit_null(out, %d, &fp_first);\n" q_idx
  in
  Printf.sprintf
    "static void run_q%d(FILE* out) {\n\
    \  fp_setup();\n\
    %s\
    \  fp_teardown();\n\
    }\n"
    q_idx
    body


(* ---------- Heap word table ---------- *)

let emit_heap_table (heap_words : (int64 * int64) list) : string =
  let b = Buffer.create 256 in
  buf_add b "typedef struct fp_heap_word { uintptr_t addr; uint64_t value; } fp_heap_word_t;";
  Buffer.add_string b "static const fp_heap_word_t FP_HEAP[] = {";
  StdList.iter
    (fun (a, v) ->
       Buffer.add_string b (Printf.sprintf "\n  { 0x%LxUL, 0x%LxUL }," a v))
    heap_words;
  buf_add b "\n};";
  buf_add
    b
    (Printf.sprintf
       "static const size_t FP_HEAP_N = %d;"
       (StdList.length heap_words));
  Buffer.contents b


(* ---------- Pre-vars table ---------- *)

let emit_pre_vars_table (dp : Data_point.data_point) : string =
  let b = Buffer.create 256 in
  buf_add b "typedef struct fp_var { const char* name; uintptr_t value; size_t size; } fp_var_t;";
  Buffer.add_string b "static const fp_var_t FP_PRE_VARS[] = {";
  StdList.iter
    (fun (v : Data_point.var_binding) ->
       Buffer.add_string
         b
         (Printf.sprintf "\n  { \"%s\", 0x%LxUL, %d }," v.name v.value v.size))
    dp.pre_vars;
  buf_add b "\n};";
  buf_add
    b
    (Printf.sprintf
       "static const size_t FP_PRE_VARS_N = %d;"
       (StdList.length dp.pre_vars));
  Buffer.contents b


(* ---------- Harness static body (hook, callback, setup/teardown, JSON) ---------- *)

let harness_runtime_body =
  {|
extern struct rmap *cn_ownership_global_ghost_state;

static sigjmp_buf fp_jmp;

static void fp_failure_cb(enum cn_failure_mode m, enum spec_mode sm) {
  (void)m;
  (void)sm;
  siglongjmp(fp_jmp, 1);
}

static int fp_lookup_byte(uintptr_t addr, unsigned char *out) {
  /* The dump records 8-byte aligned words. Find the word containing addr. */
  for (size_t i = 0; i < FP_HEAP_N; i++) {
    uintptr_t base = FP_HEAP[i].addr;
    if (addr >= base && addr < base + 8) {
      uint64_t v = FP_HEAP[i].value;
      *out = ((const unsigned char *)&v)[addr - base];
      return 1;
    }
  }
  return 0;
}

static _Bool fp_load_hook(const void *p, size_t sz, void *dst) {
  uintptr_t addr = (uintptr_t)p;
  unsigned char *d = (unsigned char *)dst;
  for (size_t k = 0; k < sz; k++) {
    if (!fp_lookup_byte(addr + k, &d[k])) return 0;
  }
  return 1;
}

#define FP_PREDICATE_DEPTH 2

static void fp_setup(void) {
  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  ghost_stack_depth_incr(); /* depth=1, caller frame */
  for (size_t i = 0; i < FP_HEAP_N; i++) {
    cn_assume_ownership((void *)FP_HEAP[i].addr, 8, "fp");
  }
  ghost_stack_depth_incr(); /* depth=2, predicate frame */
  cn_load_hook = fp_load_hook;
  set_cn_failure_cb(fp_failure_cb);
}

static void fp_teardown(void) {
  cn_load_hook = NULL;
  reset_cn_failure_cb();
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

static void fp_emit_footprint(FILE *out, int q_idx, _Bool *first, signed long depth) {
  if (!*first) fputs(",", out);
  *first = 0;
  fprintf(out, "{\"q\":%d,\"addrs\":[", q_idx);
  struct fp_collect_ctx ctx = { .out = out, .depth = depth, .first = 1 };
  rmap_foreach(cn_ownership_global_ghost_state, fp_collect_cb, &ctx);
  fputs("]}", out);
}

static void fp_emit_null(FILE *out, int q_idx, _Bool *first) {
  if (!*first) fputs(",", out);
  *first = 0;
  fprintf(out, "{\"q\":%d,\"addrs\":null}", q_idx);
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
    (fun (q_idx, _) ->
       buf_add b (Printf.sprintf "  run_q%d(out);" q_idx))
    qualifiers;
  buf_add b "  fputs(\"]}\", out);";
  buf_add b "  fclose(out);";
  buf_add b "  fulminate_destroy();";
  buf_add b "  return 0;";
  buf_add b "}";
  Buffer.contents b


(* ---------- Top-level emit ---------- *)

let emit_inner (input : input) : string =
  Records.populate_record_map [] input.prog5;
  let _, sigm = (None, input.ail_prog) in
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
      (emit_qualifier_fn
         ~dp:input.representative_dp
         ~pred_defs:input.pred_defs)
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
  buf_add b "/* HARNESS */";
  buf_add b (emit_heap_table input.heap_words);
  buf_add b (emit_pre_vars_table input.representative_dp);
  buf_add b harness_runtime_body;
  StdList.iter (buf_add b) q_fns;
  buf_add b (emit_main input.qualifiers input.output_json_path);
  Buffer.contents b


let emit (input : input) : string =
  Cerb_colour.without_colour (fun () -> emit_inner input) ()
