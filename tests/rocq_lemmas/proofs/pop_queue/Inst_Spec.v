(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

Require Import ZArith Bool Lia.
Require Import CN_Lemmas.Gen_Spec.
Import CN_Lemmas.Gen_Spec.Types.
From iris.base_logic.lib Require Import iprop.
From iris.proofmode Require Import proofmode.

Module Inst.
End Inst.

Module InstOK : CN_Lemmas.Gen_Spec.Lemma_Spec (Inst).
  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Import L L.D L.R.
  Open Scope Z.

  Section Proof.
  Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
  Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

  Lemma pop_lemma : ⊢ pop_lemma_type.
  Proof.
    iIntros (front back x Q) "HQ".
    iIntros (B) "HB %Hback %HB".
    iExists Q. iFrame "HQ".
    iExists B. iFrame "HB".
    iPureIntro.
    repeat split; tauto.
  Qed.

  End Proof.
End InstOK.
