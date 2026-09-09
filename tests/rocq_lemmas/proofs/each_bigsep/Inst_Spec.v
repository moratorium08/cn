Require Import ZArith Bool Lia.
From stdpp Require Import gmap.
From iris.bi Require Import big_op.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.Gen_Spec.
Require Import CN_Lemmas.CN_Lib_Iris.

Import CN_Lemmas.Gen_Spec.Types.

Module Inst.
End Inst.

Module Lemma_Defs := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

Module Proofs.

Import Lemma_Defs Inst.
Open Scope Z.

Section proofs.
Context `{!heapGS_gen Σ}.
Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

Lemma each_singleton : ⊢ each_singleton_type.
Proof.
  unfold each_singleton_type.
  iIntros (p V) "HV".
  iExists (fun _ => V).
  iSplitL "HV".
  - unfold cn_each_bool, cn_each.
    iExists ({[0%Z]}).
    iSplit.
    + iPureIntro. intros i.
      rewrite elem_of_singleton Z.eqb_eq. tauto.
    + rewrite big_sepS_singleton.
      replace (arrayshift p 4 0) with p by (unfold arrayshift; lia).
      done.
  - iSplit; done.
Qed.

Lemma each_merge_two : ⊢ each_merge_two_type.
Proof.
  unfold each_merge_two_type.
  iIntros (p A0) "H0".
  iIntros (A1) "H1".
  set (A := fun i => if i =? 0 then A0 i else A1 i).
  iExists A.
  iSplitL "H0 H1".
  - assert (Hdisjoint : forall i : Z,
        (i =? 0) = true -> (i =? 1) = true -> False) by
      (intros i Hi0 Hi1; apply Z.eqb_eq in Hi0, Hi1; lia).
    assert (Hout0 : forall i : Z,
        (i =? 0) = true -> A i = A0 i) by
      (intros i Hi; apply Z.eqb_eq in Hi; subst; rewrite /A; done).
    assert (Hout1 : forall i : Z,
        (i =? 1) = true -> A i = A1 i) by
      (intros i Hi; apply Z.eqb_eq in Hi; subst; rewrite /A; done).
    iApply
      (cn_each_bool_merge
         (fun i : Z => i =? 0)
         (fun i : Z => i =? 1)
         (fun i v => Owned_int (arrayshift p 4 i) v)
         A0 A1 A Hdisjoint Hout0 Hout1 with "H0 H1").
  - iSplit.
    + iPureIntro. rewrite /A. done.
    + iSplit; last done.
      iPureIntro. rewrite /A. done.
Qed.

Lemma each_uninit_identity : ⊢ each_uninit_identity_type.
Proof.
  unfold each_uninit_identity_type.
  iIntros (p A) "HA".
  iExists A. iFrame.
Qed.

Lemma each_named_identity : ⊢ each_named_identity_type.
Proof.
  unfold each_named_identity_type.
  iIntros (p A) "HA".
  iExists A. iFrame.
Qed.

End proofs.
End Proofs.

Module InstOK : CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).
  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Include Proofs.
End InstOK.
