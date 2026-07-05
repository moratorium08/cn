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
