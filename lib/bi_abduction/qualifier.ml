(** Qualifiers for bi-abductive inference.

    A qualifier is a *chain* of named resource bindings — candidate
    [take X = ...] lines to add to a specification:

      take X1 = R1; take X2 = R2; ...

    where each [Ri] is a CN [Request.t] whose terms may mention the names
    bound by earlier steps (IDEA.md 4.4, "qualifier chains").  Today the
    enumerator only produces singleton chains; the type is chain-shaped so
    that dependent qualifiers (Step 3 of PLAN.md) extend the data, not the
    interfaces.  We reuse CN's existing [Request.t], [IndexTerms.t] and
    [Sctypes.t] rather than defining a parallel hierarchy. *)

module StdList = Stdlib.List

type step =
  { name : Sym.t; (** the take-bound name; fresh, only meaningful within the chain *)
    req : Request.t
  }

type t = step list (* nonempty, dependency-ordered *)

(** Create a singleton Owned qualifier: take X = RW<ty>(ptr). *)
let owned ~(ct : Sctypes.t) ~(pointer : IndexTerms.t) : t =
  [ { name = Sym.fresh_anon ();
      req = Request.P { name = Owned (ct, Init); pointer; iargs = [] }
    }
  ]


(** Create a singleton named-predicate qualifier: take X = P(ptr, args...). *)
let predicate ~(name : Sym.t) ~(pointer : IndexTerms.t) ~(iargs : IndexTerms.t list) : t =
  [ { name = Sym.fresh_anon (); req = Request.P { name = PName name; pointer; iargs } } ]


(** The request of a singleton chain, if it is one. *)
let singleton_req : t -> Request.t option = function
  | [ { req; _ } ] -> Some req
  | _ -> None


(** Pretty-print a qualifier as CN [take] line(s), e.g.
    "take _ = RW<int>(p);" for a singleton, one line per step otherwise. *)
let pp_takes (q : t) : Pp.document =
  let open Pp in
  let pp_step ~named { name; req } =
    !^"take"
    ^^^ (if named then Sym.pp name else !^"_")
    ^^^ equals
    ^^^ Request.pp req
    ^^ semi
  in
  match q with
  | [ step ] -> pp_step ~named:false step
  | steps -> separate_map hardline (pp_step ~named:true) steps


(** Render a list of selected qualifiers as take lines, printing steps
    shared between chains (same bound name and request — e.g. a common
    [take W = RW<struct S>(b)] prefix) only once.  Prefix steps keep their
    bound name (later lines reference it); leaf steps print [_]. *)
let pp_takes_merged (qs : t list) : Pp.document list =
  let open Pp in
  let seen : (Sym.t * Request.t) list ref = ref [] in
  let step_seen (s : step) =
    StdList.exists (fun (n, r) -> Sym.equal n s.name && Request.equal r s.req) !seen
  in
  StdList.concat_map
    (fun (q : t) ->
       let n = StdList.length q in
       StdList.mapi (fun i s -> (i = n - 1, s)) q
       |> StdList.filter_map (fun (is_last, s) ->
         if step_seen s then
           None
         else (
           seen := (s.name, s.req) :: !seen;
           Some
             (!^"take"
              ^^^ (if is_last then !^"_" else Sym.pp s.name)
              ^^^ equals
              ^^^ Request.pp s.req
              ^^ semi))))
    qs


(** Compact rendering (no [take]), used in debug output. *)
let pp (q : t) : Pp.document =
  let open Pp in
  match q with
  | [ { req; _ } ] -> Request.pp req
  | steps -> separate_map (semi ^^ space) (fun { req; _ } -> Request.pp req) steps


(** Equality modulo the bound names.
    TODO: once chains longer than one step are produced, occurrences of the
    bound names inside later steps' terms must be alpha-normalised before
    comparison; for singleton chains (all we build today) comparing the
    requests is exact. *)
let equal (a : t) (b : t) : bool =
  StdList.length a = StdList.length b
  && StdList.for_all2 (fun sa sb -> Request.equal sa.req sb.req) a b
