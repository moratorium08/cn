From Stdlib Require Import ZArith Lia.
From stdpp Require Import countable.
Open Scope Z_scope.

(* A direct model of solver.ml:CN_AllocId and CN_Pointer.  Wrong-constructor
   selectors are parameters, as in SMT datatypes, not arbitrarily fixed to 0. *)
Module Type CONFIG.
  Parameter pointer_bits : nat.
  Parameter bitvectors : bool.
  Parameter vip : bool.
End CONFIG.

Module Make (C : CONFIG).
  Definition modulus : Z := 2 ^ Z.of_nat C.pointer_bits.
  Lemma modulus_pos : 0 < modulus.
  Proof. unfold modulus; apply Z.pow_pos_nonneg; lia. Qed.

  Definition AllocId : Set := if C.vip then Z else unit.
  Definition Address : Set :=
    if C.bitvectors then { z : Z | bool_decide (0 <= z < modulus) = true } else Z.
  Definition Ptr : Set := option (AllocId * Address).
  Global Instance alloc_id_eq_dec : EqDecision AllocId.
  Proof. unfold AllocId; destruct C.vip; apply _. Defined.
  Global Instance alloc_id_countable : Countable AllocId.
  Proof. unfold alloc_id_eq_dec, AllocId; destruct C.vip; apply _. Defined.
  Global Instance address_eq_dec : EqDecision Address.
  Proof. unfold Address; destruct C.bitvectors; apply _. Defined.
  Global Instance address_countable : Countable Address.
  Proof. unfold address_eq_dec, Address; destruct C.bitvectors; apply _. Defined.
  Global Instance ptr_eq_dec : EqDecision Ptr := _.
  Global Instance ptr_countable : Countable Ptr := _.

  Definition address (z : Z) : Address.
  Proof.
    unfold Address; destruct C.bitvectors.
    - refine (exist _ (z mod modulus) _).
      apply bool_decide_eq_true_2, Z.mod_pos_bound, modulus_pos.
    - exact z.
  Defined.
  Definition address_Z : Address -> Z :=
    match C.bitvectors as b return (if b then {z : Z | bool_decide (0 <= z < modulus) = true} else Z) -> Z with
    | true => fun z => proj1_sig z
    | false => fun z => z
    end.
  Definition allocation_id (z : Z) : AllocId :=
    match C.vip as b return (if b then Z else unit) with
    | true => z | false => tt end.

  Class Selectors := {
    null_alloc_id : AllocId;
    null_address : Address;
    default_alloc_id : AllocId
  }.

  Definition null : Ptr := None.
  Definition aia (aid : AllocId) (a : Z) : Ptr := Some (aid, address a).
  Definition has_alloc_id (p : Ptr) : bool := match p with Some _ => true | None => false end.
  Definition ptr_eq (p q : Ptr) : bool := bool_decide (p = q).
  Definition alloc_id_eq (a b : AllocId) : bool := bool_decide (a = b).
  Definition addr_of (p : Ptr) : Z :=
    match p with None => 0 | Some (_, a) => address_Z a end.
  Definition ptr_lt (p q : Ptr) : bool := Z.ltb (addr_of p) (addr_of q).
  Definition ptr_le (p q : Ptr) : bool := Z.leb (addr_of p) (addr_of q).

  Section Selectors.
    Context {S : Selectors}.
    Definition alloc_id_of (p : Ptr) : AllocId :=
      match p with None => null_alloc_id | Some (a, _) => a end.
    Definition raw_address (p : Ptr) : Address :=
      match p with None => null_address | Some (_, a) => a end.
    Definition ptr_shift (p : Ptr) (offset : Z) : Ptr :=
      aia (alloc_id_of p) (address_Z (raw_address p) + offset).
    Definition arrayshift (p : Ptr) (size index : Z) : Ptr :=
      ptr_shift p (size * index).
    Definition shift (p : Ptr) (offset size : Z) : Ptr := ptr_shift p offset.
    Definition copy_alloc_id (p : Ptr) (a : Z) : Ptr := aia (alloc_id_of p) a.
    Definition addr_to_ptr (z : Z) : Ptr :=
      let a := address z in
      if Z.eqb (address_Z a) 0 then null else Some (default_alloc_id, a).

    Lemma shift_has_alloc_id p n : has_alloc_id (ptr_shift p n) = true.
    Proof. reflexivity. Qed.
    Lemma shift_alloc_id p n : alloc_id_of (ptr_shift p n) = alloc_id_of p.
    Proof. reflexivity. Qed.
    Lemma copy_alloc_id_preserves p n : alloc_id_of (copy_alloc_id p n) = alloc_id_of p.
    Proof. reflexivity. Qed.
  End Selectors.

  (* alloc.ml:History and check.ml:in_bounds.  One-past is allowed for pointer
     arithmetic, but a nonempty Owned footprint must fit before the end. *)
  Record Allocation := { allocation_base : Address; allocation_size : Address }.
  Definition History := AllocId -> Allocation.
  Definition allocation_end (a : Allocation) : Z :=
    address_Z (address (address_Z (allocation_base a) + address_Z (allocation_size a))).
  Definition in_bounds `{Selectors} (h : History) (p : Ptr) : Prop :=
    let a := h (alloc_id_of p) in
    address_Z (allocation_base a) <= addr_of p <= allocation_end a.
  Definition footprint_ok `{Selectors} (h : History) (p : Ptr) (n : Z) : Prop :=
    has_alloc_id p = true /\ 0 <= n /\
    let upper := address_Z (address (addr_of p + n)) in
    (if C.bitvectors then addr_of p <= upper else upper <= modulus - 1) /\
    (if C.vip then
       address_Z (allocation_base (h (alloc_id_of p))) <= addr_of p /\
       upper <= allocation_end (h (alloc_id_of p))
     else True).
End Make.
