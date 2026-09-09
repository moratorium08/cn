(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

From Stdlib Require Import ZArith Lia.
From stdpp Require Import gmap fin_sets.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import iprop.
Require Import CN_Lemmas.Gen_Spec.
Import CN_Lemmas.Gen_Spec.Types.

Module Inst <: CN_Lemmas.Gen_Spec.Parameters.
End Inst.

Module InstOK <: CN_Lemmas.Gen_Spec.Lemma_Spec (Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Import L L.D L.R.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma each_combine : ⊢ each_combine_type.
    Proof.
      unfold each_combine_type.
      iIntros (p n m A1) "[%Hid H1] %Halign %HA1".
      iIntros (A2) "[_ H2] _ %HA2 %Hn0 %Hn1 %Hm0 %Hm1 %Hmn".
      iDestruct "H1" as (I1) "[%HI1 H1]".
      iDestruct "H2" as (I2) "[%HI2 H2]".
      iExists (fun j => if bool_decide (j < n) then A1 j else A2 j).
      iSplitL.
      - iSplit; first done.
        iExists (I1 ∪ I2).
        iSplit.
        { iPureIntro. intro i. rewrite elem_of_union HI1 HI2. lia. }
        rewrite big_sepS_union.
        + iSplitL "H1".
          * iApply (big_sepS_mono with "H1"). intros i Hi.
            apply HI1 in Hi. rewrite bool_decide_eq_true_2; [done | lia].
          * iApply (big_sepS_mono with "H2"). intros i Hi.
            apply HI2 in Hi. rewrite bool_decide_eq_false_2; [done | lia].
        + apply elem_of_disjoint. intros i Hi1 Hi2.
          apply HI1 in Hi1. apply HI2 in Hi2. lia.
      - iSplit; first done.
        iSplit; last done.
        iPureIntro. intros j Hj.
        destruct (bool_decide (j < n)) eqn:Hb.
        + apply bool_decide_eq_true in Hb. apply HA1. lia.
        + apply bool_decide_eq_false in Hb. apply HA2. lia.
    Qed.

  End Proof.

End InstOK.

Print Assumptions InstOK.each_combine.
