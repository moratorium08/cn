(** Qualifiers for bi-abductive inference.

    A qualifier is a resource request (Request.t) with free variables, representing
    a candidate "take X = ..." binding to add to a specification. We reuse CN's
    existing Request.t, IndexTerms.t, and Sctypes.t rather than defining new types. *)

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

(** Pretty-print a single qualifier. *)
let pp (q : t) : Pp.document = Request.pp q

(** Equality check for qualifiers. *)
let equal (a : t) (b : t) : bool = Request.equal a b
