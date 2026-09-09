From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic.lib Require Export gen_heap.
Require Export CN_Lemmas.CN_Memory.

Module Make (C : CONFIG).
  Module Memory := CN_Memory.Make C.
  Export Memory.
  Global Existing Instance alloc_id_eq_dec.
  Global Existing Instance alloc_id_countable.
  Global Existing Instance address_eq_dec.
  Global Existing Instance address_countable.
  Global Existing Instance ptr_eq_dec.
  Global Existing Instance ptr_countable.
  Open Scope Z_scope.
  (* solver.ml:CN_MemByte: bytes carry optional provenance, even when read as
     integers. None at the outer level means an unspecified byte. *)
  Definition MemByte := (option AllocId * Z)%type.
  Definition Val := option MemByte.
  Class heapGS_gen Σ := HeapGS {
    heapGS_gen_heapGS :> gen_heapGS Address Val Σ;
    heapGS_allocations : gen_heapGS AllocId Allocation Σ;
    allocation_history : History
  }.
  Notation heapGS := heapGS_gen.

  Section Heap.
    Context {S : Selectors} `{!heapGS_gen Σ}.
    Definition history_map (aid : AllocId) : Z * Z :=
      let a := allocation_history aid in
      (address_Z (allocation_base a), address_Z (allocation_size a)).
    (* The Alloc token witnesses liveness independently of byte ownership;
       mere bounds/history membership does not grant the right to free. *)
    Definition Alloc (p : Ptr) (v : Z * Z) : iProp Σ :=
      let a := allocation_history (alloc_id_of p) in
      ⌜v = history_map (alloc_id_of p) /\
        address_Z (allocation_base a) <= allocation_end a⌝ ∗
      @pointsto AllocId alloc_id_eq_dec alloc_id_countable Allocation Σ heapGS_allocations
        (alloc_id_of p) (DfracOwn 1) a.
    Definition bytes_at (p : Ptr) (bs : list Val) : iProp Σ :=
      [∗ list] i ↦ b ∈ bs,
        @pointsto Address address_eq_dec address_countable Val Σ heapGS_gen_heapGS
          (address (addr_of p + Z.of_nat i)) (DfracOwn 1) b.
    Definition byte_values (bs : list Val) (vs : list Z) : Prop :=
      Forall2 (fun b v => exists aid, b = Some (aid, v) /\ 0 <= v < 256) bs vs.
    Fixpoint decode_le (vs : list Z) : Z :=
      match vs with [] => 0 | v :: vs => v + 256 * decode_le vs end.
    Definition Owned_raw (n : nat) (p : Ptr) (bs : list Val) : iProp Σ :=
      ⌜footprint_ok allocation_history p (Z.of_nat n) /\ length bs = n⌝ ∗ bytes_at p bs.
    (* W is unspecified ownership, not a cell forced to contain None. *)
    Definition BlockSized (n : nat) (p : Ptr) : iProp Σ :=
      ∃ bs, Owned_raw n p bs.
    (* Finite-width integer view. Unsigned reinterpretation is validated below.
       The integer-UF check.ml currently equates signed values directly with
       the nonnegative decoded sum, without sign reconstruction. Correspondence
       for negative signed integer-mode values remains unproved (see notes). *)
    Definition Owned_integer (n : nat) (signed : bool) (p : Ptr) (v : Z) : iProp Σ :=
      ∃ bs vs, Owned_raw n p bs ∗
        ⌜byte_values bs vs /\
          (if signed then -(2 ^ (8 * Z.of_nat n - 1)) <= v < 2 ^ (8 * Z.of_nat n - 1)
           else 0 <= v < 2 ^ (8 * Z.of_nat n)) /\
          decode_le vs = v mod (2 ^ (8 * Z.of_nat n))⌝.
    (* This is the To-bytes view of an initialized pointer.  From-bytes has a
       weaker, conditional provenance constraint in check.ml. Do not claim a
       reverse reinterpretation theorem without bridging that difference. *)
    Definition Owned_pointer (p : Ptr) (v : Ptr) : iProp Σ :=
      ∃ bs vs, Owned_raw (C.pointer_bits / 8)%nat p bs ∗
        ⌜byte_values bs vs /\ decode_le vs = addr_of v /\
          0 <= addr_of v < modulus /\
          Forall (fun b => exists z, b = Some (Some (alloc_id_of v), z)) bs⌝.
    Definition Owned_char := Owned_integer 1 true.
    Definition Owned_int := Owned_integer 4 true.
    Definition padding (p : Ptr) (n : nat) : iProp Σ :=
      match n with O => emp | Datatypes.S _ => BlockSized n p end.

    Lemma owned_integer_forget n signed p v :
      Owned_integer n signed p v -∗ BlockSized n p.
    Proof. iIntros "H". iDestruct "H" as (bs vs) "[H _]". iExists bs; iFrame. Qed.
    Lemma owned_pointer_forget p v : Owned_pointer p v -∗ BlockSized (C.pointer_bits / 8)%nat p.
    Proof. iIntros "H". iDestruct "H" as (bs vs) "[H _]". iExists bs; iFrame. Qed.
    Lemma block_footprint n p :
      BlockSized n p -∗ ⌜footprint_ok allocation_history p (Z.of_nat n)⌝.
    Proof. iIntros "H". iDestruct "H" as (bs) "[%H _]". iPureIntro; tauto. Qed.
  End Heap.
End Make.
