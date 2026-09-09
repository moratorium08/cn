From Stdlib Require Import ZArith.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import IteratedExport.Iterated.

Module Inst <: Parameters.
End Inst.

Module InstOK <: Lemma_Spec(Inst).
  Module L := Lemma_Defs(Inst).
  Import L L.D L.R.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma each_w_identity : ⊢ each_w_identity_type.
    Proof. iIntros (p n) "H %Halign". iFrame. done. Qed.

    Lemma each_w_sparse_identity : ⊢ each_w_sparse_identity_type.
    Proof.
      iIntros (p) "H %Halign". iExists tt. iSplitL; last done.
      rewrite SparseW_unfold. iSplit; first done. iFrame. done.
    Qed.

    Lemma each_w_sparse_unpack : ⊢ each_w_sparse_unpack_type.
    Proof.
      iIntros (p before) "H". rewrite SparseW_unfold.
      iDestruct "H" as "[_ [H [%Halign _]]]". iFrame. done.
    Qed.

    Lemma each_w_empty_has_id : ⊢ each_w_empty_has_id_type.
    Proof.
      iIntros (p) "[%Hid _] _". iSplitL; last done. iPureIntro.
      destruct p as [[aid addr]|]; simpl in Hid; last contradiction.
      unfold is_null, Memory.ptr_eq, Memory.null. simpl. tauto.
    Qed.

    Lemma each_rw_map_identity : ⊢ each_rw_map_identity_type.
    Proof.
      iIntros (p before) "H %Halign %Hvalue".
      iExists before. iFrame. done.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.each_w_identity.
Print Assumptions InstOK.each_w_sparse_identity.
Print Assumptions InstOK.each_w_sparse_unpack.
Print Assumptions InstOK.each_w_empty_has_id.
Print Assumptions InstOK.each_rw_map_identity.
