(** Qualifiers for bi-abductive inference.

    A qualifier is a resource request (Request.t) with free variables, representing
    a candidate "take X = ..." binding to add to a specification. We reuse CN's
    existing Request.t, IndexTerms.t, and Sctypes.t rather than defining new types. *)

module StdList = Stdlib.List

(** A single step in a qualifier chain: "take <bind> = <resource>". *)
type chain_step =
  { bind : Sym.t;
    resource : Request.t
  }

(** A qualifier chain is a sequence of take-bindings, ordered so that
    later steps may depend on variables bound by earlier steps. *)
type chain = chain_step list

(** A qualifier is just a Request.t with free variables. *)
type t = Request.t

(** Create an Owned qualifier: take X = Owned<ty>(ptr). *)
let owned ~(ct : Sctypes.t) ~(pointer : IndexTerms.t) : t =
  Request.P
    { name = Owned (ct, Init);
      pointer;
      iargs = []
    }

(** Create a named predicate qualifier: take X = P(ptr, args...). *)
let predicate ~(name : Sym.t) ~(pointer : IndexTerms.t) ~(iargs : IndexTerms.t list) : t =
  Request.P
    { name = PName name;
      pointer;
      iargs
    }

(** Pretty-print a qualifier chain as CN syntax. *)
let pp_chain (chain : chain) : Pp.document =
  let open Pp in
  StdList.map
    (fun step ->
       string "take"
       ^^^ Sym.pp step.bind
       ^^^ string "="
       ^^^ Request.pp step.resource
       ^^ semi)
    chain
  |> separate hardline

(** Pretty-print a single qualifier. *)
let pp (q : t) : Pp.document = Request.pp q

(** Equality check for qualifiers. *)
let equal (a : t) (b : t) : bool = Request.equal a b

(** Free variables in a qualifier. *)
let free_vars (q : t) : Sym.Set.t = Request.free_vars q

(** Substitute in a qualifier. *)
let subst s (q : t) : t = Request.subst s q
