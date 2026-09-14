From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Memory_Iris IteratedExport.IteratedNamed.
Module Inst <: Parameters. End Inst.
Module InstOK <: Lemma_Spec Inst.
 Module L := Lemma_Defs Inst.
 Import L L.R.
 Section Proof.
 Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
 Local Notation "⊢ P" := (⊢@{iPropI Σ} P).
 Lemma named_pack : ⊢ named_pack_type.
 Proof.
   iIntros (p n before) "H". iExists before. iSplit; last done.
   rewrite Cells_unfold. iSplit; first done. iExists before. iFrame. done.
 Qed.
 Lemma named_unpack : ⊢ named_unpack_type.
 Proof.
   iIntros (p n before) "H". rewrite Cells_unfold.
   iDestruct "H" as "[_ H]". iDestruct "H" as (cells) "[H %Heq]".
   iExists cells. iFrame. done.
 Qed.
 Lemma named_empty : ⊢ named_empty_type.
 Proof.
   iIntros "_". iExists (fun _ => 0%Z). iSplit; last done.
   iExists ∅. rewrite big_sepS_empty. iSplit; last done.
   iPureIntro. intro i. rewrite elem_of_empty. lia.
 Qed.
 Lemma named_singleton : ⊢ named_singleton_type.
 Proof.
   iIntros (p before) "H". iExists (fun _ => before). iSplit; last done.
   iExists {[0%Z]}. iSplit.
   - iPureIntro. intro i. rewrite elem_of_singleton. lia.
   - rewrite big_sepS_singleton. iExact "H".
 Qed.
 Lemma named_recursive_step : ⊢ named_recursive_step_type.
 Proof.
   iIntros (p n) "%Hn". iIntros (children) "H".
   iExists tt. iSplit; last done. rewrite Nest_unfold. iRight.
   iSplit; first done. iSplit; first (iPureIntro; lia).
   iExists children. iFrame. done.
 Qed.
 End Proof.
End InstOK.
Print Assumptions InstOK.named_pack.
Print Assumptions InstOK.named_unpack.
Print Assumptions InstOK.named_empty.
Print Assumptions InstOK.named_recursive_step.
Print Assumptions InstOK.named_singleton.
