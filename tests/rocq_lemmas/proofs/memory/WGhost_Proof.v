From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import IteratedExport.WGhost.

Module Inst <: Parameters. End Inst.

Module InstOK <: Lemma_Spec Inst.
  Module L := Lemma_Defs Inst.
  Import L L.D L.R.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma w_ghost_preserve : ⊢ w_ghost_preserve_type.
    Proof.
      iIntros (p n before) "H %Halign".
      iExists before. iFrame. done.
    Qed.

    Lemma w_ghost_pack : ⊢ w_ghost_pack_type.
    Proof.
      iIntros (p n before) "H %Halign".
      iExists before. iSplitL; last done.
      rewrite WMap_unfold. iSplit; first done.
      iExists before. iFrame. done.
    Qed.

    Lemma w_ghost_unpack : ⊢ w_ghost_unpack_type.
    Proof.
      iIntros (p n before) "H". rewrite WMap_unfold.
      iDestruct "H" as "[_ H]".
      iDestruct "H" as (values) "(H & %Halign & %Heq)".
      iExists values. iFrame. done.
    Qed.

    Lemma w_ghost_negative : ⊢ w_ghost_negative_type.
    Proof.
      iIntros (p before) "H %Halign %Hvalue".
      iExists before. iFrame. done.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.w_ghost_preserve.
Print Assumptions InstOK.w_ghost_pack.
Print Assumptions InstOK.w_ghost_unpack.
Print Assumptions InstOK.w_ghost_negative.
