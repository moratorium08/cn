From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Memory_Iris IteratedExport.WBitvector.
Open Scope Z_scope.
Module Inst <: Parameters. End Inst.
Module InstOK <: Lemma_Spec Inst.
 Module L := Lemma_Defs Inst.
 Import L L.R Types.
 Section Proof.
 Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
 Local Notation "⊢ P" := (⊢@{iPropI Σ} P).
 Lemma bv_w_range : ⊢ bv_w_range_type.
 Proof.
   iIntros (p before) "[%Hr H] %Ha".
   iExists before. iSplitL "H"; first (iFrame; done).
   iPureIntro. pose proof (Hr 1 ltac:(lia)). tauto.
 Qed.
 Lemma bv_w_pack : ⊢ bv_w_pack_type.
 Proof.
   iIntros (p before) "H %Ha". iExists before. iSplit; last done.
   rewrite GhostWords_unfold. iSplit; first done. iExists before. iFrame. done.
 Qed.
 Lemma bv_w_unpack : ⊢ bv_w_unpack_type.
 Proof.
   iIntros (p before) "H". rewrite GhostWords_unfold.
   iDestruct "H" as "[_ H]". iDestruct "H" as (cells) "[[%Hr H] [%Ha %Heq]]".
   subst before. iExists cells. iSplitL "H"; first (iFrame; done).
   iPureIntro. pose proof (Hr 1 ltac:(lia)). tauto.
 Qed.
 Lemma bv_w_struct_range : ⊢ bv_w_struct_range_type.
 Proof.
   iIntros (p before) "[%Hr H] %Ha".
   iExists before. iSplitL "H"; first (iFrame; done).
   iPureIntro. pose proof (Hr (-1) ltac:(lia)). tauto.
 Qed.
 End Proof.
End InstOK.
Print Assumptions InstOK.bv_w_range.
Print Assumptions InstOK.bv_w_pack.
Print Assumptions InstOK.bv_w_unpack.
Print Assumptions InstOK.bv_w_struct_range.
