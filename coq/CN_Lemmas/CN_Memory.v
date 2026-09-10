From Stdlib Require Import ZArith Lia.
From stdpp Require Import countable.
Open Scope Z_scope.

(* A direct model of solver.ml:CN_AllocId and CN_Pointer.

   The two CN switches are separate modules rather than booleans tested inside
   definitions, so that each mode has exactly the structure CN gives it:

   - ADDRESS: integer mode (Z, no wrapping, resource.ml:derived_lc1 bounds the
     extent by Memory.max_pointer) or bitvector mode (addresses reduce modulo
     2^pointer_bits and the extent must not wrap).
   - PROVENANCE: VIP (allocation IDs are Z, bytes record an optional ID,
     owned extents must lie inside their allocation) or no-VIP (allocation IDs
     are unit, bytes record nothing, no allocation bounds).

   Wrong-constructor selectors are parameters, as in SMT datatypes, not
   arbitrarily fixed to 0. *)

Module Type WIDTH.
  Parameter pointer_bits : nat.
End WIDTH.

Module Type ADDRESS.
  Parameter pointer_bits : nat.
  Parameter modulus : Z.
  Parameter modulus_pos : 0 < modulus.
  Parameter Address : Set.
  Declare Instance address_eq_dec : EqDecision Address.
  Declare Instance address_countable : Countable Address.
  Parameter address : Z -> Address.
  Parameter address_Z : Address -> Z.
  (* resource.ml:derived_lc1 "within_addr_space": the owned extent
     [addr, addr + n) is representable. *)
  Parameter extent_fits : Z -> Z -> Prop.
End ADDRESS.

Module IntegerAddress (W : WIDTH) <: ADDRESS.
  Definition pointer_bits : nat := W.pointer_bits.
  Definition modulus : Z := 2 ^ Z.of_nat pointer_bits.
  Lemma modulus_pos : 0 < modulus.
  Proof. unfold modulus; apply Z.pow_pos_nonneg; lia. Qed.
  Definition Address : Set := Z.
  Global Instance address_eq_dec : EqDecision Address := _.
  Global Instance address_countable : Countable Address := _.
  Definition address (z : Z) : Address := z.
  Definition address_Z (a : Address) : Z := a.
  (* integer mode: upper <= Memory.max_pointer = 2^bits - 1 *)
  Definition extent_fits (addr n : Z) : Prop := addr + n <= modulus - 1.
End IntegerAddress.

Module BoundedAddress (W : WIDTH) <: ADDRESS.
  Definition pointer_bits : nat := W.pointer_bits.
  Definition modulus : Z := 2 ^ Z.of_nat pointer_bits.
  Lemma modulus_pos : 0 < modulus.
  Proof. unfold modulus; apply Z.pow_pos_nonneg; lia. Qed.
  Definition Address : Set := { z : Z | bool_decide (0 <= z < modulus) = true }.
  Global Instance address_eq_dec : EqDecision Address.
  Proof. apply _. Defined.
  Global Instance address_countable : Countable Address.
  Proof. apply _. Defined.
  Definition address (z : Z) : Address :=
    exist _ (z mod modulus)
      (bool_decide_eq_true_2 _ (Z.mod_pos_bound z modulus modulus_pos)).
  Definition address_Z (a : Address) : Z := proj1_sig a.
  (* bitvector mode: addr <= upper, where upper wraps *)
  Definition extent_fits (addr n : Z) : Prop := addr <= address_Z (address (addr + n)).
End BoundedAddress.

(* alloc.ml:History entries. *)
Record Allocation := { allocation_base : Z; allocation_size : Z }.

Module Type PROVENANCE.
  Parameter AllocId : Set.
  Declare Instance alloc_id_eq_dec : EqDecision AllocId.
  Declare Instance alloc_id_countable : Countable AllocId.
  (* CN allocation-ID literals *)
  Parameter allocation_id : Z -> AllocId.
  (* what a byte records about the pointer it was stored as part of
     (solver.ml:CN_MemByte) *)
  Parameter ByteProv : Set.
  Declare Instance byte_prov_eq_dec : EqDecision ByteProv.
  Declare Instance byte_prov_countable : Countable ByteProv.
  Parameter tagged : AllocId -> ByteProv.
  (* resource.ml:derived_lc1 allocation bounds for an owned extent
     [addr, upper], given the allocation's (wrapped) end. *)
  Parameter alloc_bounds :
    (AllocId -> Allocation) -> (Allocation -> Z) -> AllocId -> Z -> Z -> Prop.
End PROVENANCE.

Module VIP <: PROVENANCE.
  Definition AllocId : Set := Z.
  Global Instance alloc_id_eq_dec : EqDecision AllocId := _.
  Global Instance alloc_id_countable : Countable AllocId := _.
  Definition allocation_id (z : Z) : AllocId := z.
  Definition ByteProv : Set := option Z.
  Global Instance byte_prov_eq_dec : EqDecision ByteProv := _.
  Global Instance byte_prov_countable : Countable ByteProv := _.
  Definition tagged (a : AllocId) : ByteProv := Some a.
  Definition alloc_bounds (h : AllocId -> Allocation) (end_of : Allocation -> Z)
      (a : AllocId) (addr upper : Z) : Prop :=
    allocation_base (h a) <= addr /\ upper <= end_of (h a).
End VIP.

Module NoVIP <: PROVENANCE.
  Definition AllocId : Set := unit.
  Global Instance alloc_id_eq_dec : EqDecision AllocId := _.
  Global Instance alloc_id_countable : Countable AllocId := _.
  Definition allocation_id (_ : Z) : AllocId := tt.
  Definition ByteProv : Set := unit.
  Global Instance byte_prov_eq_dec : EqDecision ByteProv := _.
  Global Instance byte_prov_countable : Countable ByteProv := _.
  Definition tagged (_ : AllocId) : ByteProv := tt.
  Definition alloc_bounds (_ : AllocId -> Allocation) (_ : Allocation -> Z)
      (_ : AllocId) (_ _ : Z) : Prop := True.
End NoVIP.

Module Make (A : ADDRESS) (P : PROVENANCE).
  Include A.
  Include P.

  Definition Ptr : Set := option (AllocId * Address).
  Global Instance ptr_eq_dec : EqDecision Ptr := _.
  Global Instance ptr_countable : Countable Ptr := _.

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
  Definition History := AllocId -> Allocation.
  Definition allocation_end (a : Allocation) : Z :=
    address_Z (address (allocation_base a + allocation_size a)).
  Definition in_bounds `{Selectors} (h : History) (p : Ptr) : Prop :=
    let a := h (alloc_id_of p) in
    allocation_base a <= addr_of p <= allocation_end a.
  Definition footprint_ok `{Selectors} (h : History) (p : Ptr) (n : Z) : Prop :=
    has_alloc_id p = true /\ 0 <= n /\
    extent_fits (addr_of p) n /\
    alloc_bounds h allocation_end (alloc_id_of p) (addr_of p)
      (address_Z (address (addr_of p + n))).
End Make.
