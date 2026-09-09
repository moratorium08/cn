From Stdlib Require Import ZArith.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import IteratedExport.IteratedInteger.

Module Inst <: Parameters.
End Inst.

Module InstOK <: Lemma_Spec(Inst).
  Module L := Lemma_Defs(Inst).
  Import L L.D L.R.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma each_w_negative_sparse : ⊢ each_w_negative_sparse_type.
    Proof.
      iIntros (p) "H %Halign". iExists tt. iSplitL; last done.
      rewrite NegativeSparseW_unfold. iSplit; first done. iFrame. done.
    Qed.

    Lemma each_w_integer_identity : ⊢ each_w_integer_identity_type.
    Proof. iIntros (p n) "H %Halign". iFrame. done. Qed.

    Lemma each_rw_signed_map : ⊢ each_rw_signed_map_type.
    Proof.
      iIntros (p before) "H %Halign Hrepresentable %Hvalue".
      iExists before. iFrame. done.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.each_w_negative_sparse.
Print Assumptions InstOK.each_w_integer_identity.
Print Assumptions InstOK.each_rw_signed_map.
