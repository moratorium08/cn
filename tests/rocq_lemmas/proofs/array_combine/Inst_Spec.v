(* Instantiation of the CN-exported specification for arrays/combine.
   Iterated ownership is [each_resource]: an explicit finite index set with a
   big separating conjunction, so extending an array by one cell is a
   [big_sepS_union] with a singleton (plus a re-indexing via [set_map]). *)

From Stdlib Require Import ZArith Lia.
From stdpp Require Import gmap fin_sets.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import iprop.
Require Import CN_Lemmas.Gen_Spec.
Require CN_Lemmas.CN_Lib.
Import CN_Lemmas.Gen_Spec.Types.

Module Inst <: Parameters.
End Inst.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Import L L.D.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    (* Pointer arithmetic in integer mode: addresses are plain [Z]. *)
    Lemma shift_add (p : Ptr) (s n m : Z) :
      arrayshift (arrayshift p s n) s m = arrayshift p s (n + m).
    Proof.
      destruct p as [[aid a]|]; unfold arrayshift, ptr_shift, aia,
        alloc_id_of, raw_address;
        cbv [address address_Z Address CN_ExportConfig.bitvectors];
        f_equal; f_equal; lia.
    Qed.

    Lemma shift_zero (p : Ptr) (s : Z) :
      Is_true (has_alloc_id p) -> arrayshift p s 0 = p.
    Proof.
      destruct p as [[aid a]|]; last (intros []).
      intros _. unfold arrayshift, ptr_shift, aia, alloc_id_of, raw_address.
      cbv [address address_Z Address CN_ExportConfig.bitvectors].
      f_equal; f_equal; lia.
    Qed.

    Lemma owned_integer_alloc_id n s (p : Ptr) v :
      Owned_integer n s p v -∗ ⌜Is_true (has_alloc_id p)⌝.
    Proof.
      iIntros "H". iDestruct "H" as (bs vs) "[[%Hfoot _] _]". iPureIntro.
      destruct Hfoot as [[Hp _] _]. rewrite Hp. exact I.
    Qed.

    Lemma each_lemma : ⊢ each_lemma_type.
    Proof.
      iIntros (p n v) "Hv %Halign %Hv".
      iIntros (A) "[%Hp HA] %_ %HArange %Hn %Hmax".
      iDestruct "HA" as (indices) "[%Hind HA]".
      iExists (fun j => if decide (j = 0) then v else A j).
      iSplitL "Hv HA".
      - iSplit; first done.
        iExists ({[0]} ∪ indices). iSplit.
        { iPureIntro. intros i. rewrite elem_of_union elem_of_singleton Hind. lia. }
        rewrite big_sepS_union; last first.
        { apply disjoint_singleton_l. rewrite Hind. lia. }
        rewrite big_sepS_singleton. iSplitL "Hv".
        + cbv beta. case_decide; last done. rewrite (shift_zero p 4 Hp). done.
        + iApply (big_sepS_mono with "HA"). intros i Hi. apply Hind in Hi.
          cbv beta. case_decide; [lia | done].
      - iSplit; first done. iSplit.
        + iIntros (j). iPureIntro. intros Hj.
          destruct (decide (j = 0)); [lia | apply HArange; lia].
        + iSplit; last done. iPureIntro. cbv beta. case_decide; [done | lia].
    Qed.

    Lemma each_concrete : ⊢ each_concrete_type.
    Proof.
      iIntros (p A) "[%Hp HA] %Halign %HArange".
      iIntros (v) "Hv %Halign2 %Hv".
      iDestruct "HA" as (indices) "[%Hind HA]".
      iExists (fun j => if decide (j = 2) then v else A j).
      iSplitL "Hv HA".
      - iSplit; first done.
        iExists (indices ∪ {[2]}). iSplit.
        { iPureIntro. intros i. rewrite elem_of_union elem_of_singleton Hind. lia. }
        rewrite big_sepS_union; last first.
        { apply disjoint_singleton_r. rewrite Hind. lia. }
        rewrite big_sepS_singleton. iSplitL "HA".
        + iApply (big_sepS_mono with "HA"). intros i Hi. apply Hind in Hi.
          cbv beta. case_decide; [lia | done].
        + cbv beta. case_decide; [done | lia].
      - iSplit; first done. iSplit; last done.
        iIntros (j). iPureIntro. intros Hj.
        destruct (decide (j = 2)); [lia | apply HArange; lia].
    Qed.

    Lemma each_concrete2 : ⊢ each_concrete2_type.
    Proof.
      iIntros (p v) "Hv %Halign %Hv".
      iIntros (A) "[%Hp1 HA] %Halign1 %HArange".
      iDestruct "HA" as (indices) "[%Hind HA]".
      iDestruct (owned_integer_alloc_id with "Hv") as %Hp.
      iExists (fun j => if decide (j = 0) then v else A (j - 1)).
      iSplitL "Hv HA".
      - iSplit; first done.
        iExists ({[0]} ∪ set_map (Z.add 1) indices). iSplit.
        { iPureIntro. intros i.
          rewrite elem_of_union elem_of_singleton elem_of_map. split.
          - intros [-> | (j & -> & Hj)]; [lia | apply Hind in Hj; lia].
          - intros Hi. destruct (decide (i = 0)) as [-> | Hne]; [left; done | right].
            exists (i - 1). rewrite Hind. split; lia. }
        rewrite big_sepS_union; last first.
        { apply disjoint_singleton_l. rewrite elem_of_map.
          intros (j & Hj & Hj'). apply Hind in Hj'. lia. }
        rewrite big_sepS_singleton. iSplitL "Hv".
        + cbv beta. case_decide; last done. rewrite (shift_zero p 4 Hp). done.
        + assert (Inj (=) (=) (Z.add 1)) as Hinj by (intros i j Hij; lia).
          rewrite (big_opS_set_map (o:=bi_sep) (Z.add 1) indices _ Hinj).
          iApply (big_sepS_mono with "HA"). intros i Hi. apply Hind in Hi.
          rewrite shift_add. case_decide; first lia.
          replace (1 + i - 1) with i by lia. done.
      - iSplit; first done. iSplit; last done.
        iIntros (j). iPureIntro. intros Hj.
        destruct (decide (j = 0)); [lia | apply HArange; lia].
    Qed.
  End Proof.

End InstOK.

Print Assumptions InstOK.each_lemma.
Print Assumptions InstOK.each_concrete.
Print Assumptions InstOK.each_concrete2.
