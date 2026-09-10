From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Memory_Iris.
Open Scope Z_scope.

Module W64 <: WIDTH.
  Definition pointer_bits := 64%nat.
End W64.
Module Integer64 := IntegerAddress W64.
Module M := CN_Memory_Iris.Make Integer64 VIP.
Import M M.Memory.

Section BranchSemantics.
  Context {S : Selectors}.
  (* cp526/integers: resource.ml uses upper <= max_pointer, not just
     non-wrapping Z addition. Follow that exact bound, including its strict
     exclusion of an extent ending at 2^64; do not silently relax it. *)
  Definition full_history : History := fun _ =>
    {| allocation_base := 0; allocation_size := 2^64 |}.

  Lemma integer_footprint_inside_address_space :
    footprint_ok full_history (aia 1 (2^64 - 2)) 1.
  Proof. vm_compute. intuition discriminate. Qed.

  Lemma integer_footprint_past_max_rejected :
    ~ footprint_ok full_history (aia 1 (2^64 - 1)) 1.
  Proof. vm_compute. intuition discriminate. Qed.

  (* Unlike BV mode, the integer SMT MemByte datatype really contains Z.
     Initialization/Good, not the datatype, constrains it to a byte value. *)
  Lemma integer_raw_byte_is_not_intrinsically_bounded :
    exists b : MemByte, snd b = 256.
  Proof. exists (None, 256). reflexivity. Qed.

  Context `{!M.heapGS_gen Σ}.

  Lemma integer_signed_byte_minus_one p aid :
    Owned_raw 1 p [Some (aid, 255)] -∗ Owned_integer 1 true p (-1).
  Proof.
    iIntros "H". iExists [Some (aid, 255)], [255]. iFrame.
    iPureIntro. split.
    - constructor; last constructor. exists aid. split; [reflexivity|lia].
    - vm_compute. intuition discriminate.
  Qed.
End BranchSemantics.

Print Assumptions integer_footprint_inside_address_space.
Print Assumptions integer_footprint_past_max_rejected.
Print Assumptions integer_raw_byte_is_not_intrinsically_bounded.
Print Assumptions integer_signed_byte_minus_one.
