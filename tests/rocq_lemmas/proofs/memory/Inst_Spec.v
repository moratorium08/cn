From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import MemoryExport.Gen_Spec.

Module Inst <: Parameters.
End Inst.

Module InstOK <: Lemma_Spec(Inst).
  Module L := Lemma_Defs(Inst).
  Import L L.D.
  Open Scope Z_scope.

  Lemma address_eta (a : Address) : address (address_Z a) = a.
  Proof.
    destruct a as [z Hz]. apply (sig_eq_pi _). simpl.
    apply Z.mod_small. apply bool_decide_eq_true in Hz. exact Hz.
  Qed.

  Lemma wrap_address (a : Address) :
    CN_Lib.wrapI 0 18446744073709551615 (address_Z a) = address_Z a.
  Proof.
    destruct a as [z Hz].
    pose proof (bool_decide_eq_true_1 _ Hz) as Hrange.
    change (0 <= z < 18446744073709551616) in Hrange.
    change (CN_Lib.wrapI 0 18446744073709551615 z = z).
    apply CN_Lib.wrapI_idem; lia.
  Qed.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma member_container_cancel_wrapped : ⊢ member_container_cancel_wrapped_type.
    Proof.
      iIntros (p) "%Hnonnull". iSplitL; last done. iPureIntro.
      destruct p as [[aid [z Hz]]|].
      - unfold D.ptr_eq, Memory.ptr_eq. apply bool_decide_spec.
        unfold arrayshift, ptr_shift, aia, alloc_id_of, raw_address.
        f_equal. f_equal. apply (sig_eq_pi _). simpl.
        change (((z + 8) mod 18446744073709551616 +
                    18446744073709551608) mod 18446744073709551616 = z).
        rewrite Z.add_mod_idemp_l; [|lia].
        replace (z + 8 + 18446744073709551608) with
          (z + 1 * 18446744073709551616) by ring.
        rewrite Z.mod_add; [|lia]. apply Z.mod_small.
        exact (bool_decide_eq_true_1 _ Hz).
      - exfalso. apply Hnonnull. reflexivity.
    Qed.

    Lemma shift_preserves_provenance : ⊢ shift_preserves_provenance_type.
    Proof.
      iIntros (p offset) "_". iSplitL; last done. iPureIntro.
      unfold alloc_id_eq. apply bool_decide_spec. reflexivity.
    Qed.

    Lemma shift_has_allocation_id : ⊢ shift_has_allocation_id_type.
    Proof. iIntros (p offset) "$". Qed.

    Lemma null_address : ⊢ null_address_type.
    Proof. iIntros "_". iSplitL; last done. done. Qed.

    Lemma equal_pointer_equal_address : ⊢ equal_pointer_equal_address_type.
    Proof.
      iIntros (p q) "%Heq". iSplitL; last done. iPureIntro.
      unfold D.ptr_eq, Memory.ptr_eq in Heq.
      apply bool_decide_spec in Heq. subst q.
      unfold addr_eq. rewrite Z.eqb_refl. exact I.
    Qed.

    Lemma address_and_provenance_identify_nonnull :
      ⊢ address_and_provenance_identify_nonnull_type.
    Proof.
      iIntros (p q) "%Hp %Hq %Ha %Hi". iSplitL; last done. iPureIntro.
      destruct p as [[pa pz]|]; last (exfalso; apply Hp; reflexivity).
      destruct q as [[qa qz]|]; last (exfalso; apply Hq; reflexivity).
      unfold alloc_id_eq in Hi. apply bool_decide_spec in Hi.
      simpl in Hi. subst qa.
      unfold addr_eq in Ha. apply Is_true_eq_true in Ha. apply Z.eqb_eq in Ha.
      cbn [addr_of] in Ha. rewrite !wrap_address in Ha.
      unfold D.ptr_eq, Memory.ptr_eq. apply bool_decide_spec.
      f_equal. f_equal. apply (sig_eq_pi _). exact Ha.
    Qed.

    Lemma same_address_distinct_provenance : ⊢ same_address_distinct_provenance_type.
    Proof.
      iIntros (p q) "_ %Hdifferent". iSplitL; last done. iPureIntro.
      intros Hequal. unfold D.ptr_eq, Memory.ptr_eq in Hequal.
      apply bool_decide_spec in Hequal. subst q.
      apply Hdifferent. unfold alloc_id_eq. apply bool_decide_spec. reflexivity.
    Qed.

    Lemma struct_has_allocation_id : ⊢ struct_has_allocation_id_type.
    Proof.
      unfold struct_has_allocation_id_type.
      iIntros (p before) "H %Hp".
      iDestruct "H" as "[%Hfoot Hfields]".
      iExists before. iSplitL "Hfields".
      - iSplit; [done|iFrame].
      - iFrame. iPureIntro. destruct Hfoot as (Hid & _).
        rewrite Hid. simpl. tauto.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.member_container_cancel_wrapped.
Print Assumptions InstOK.shift_preserves_provenance.
Print Assumptions InstOK.shift_has_allocation_id.
Print Assumptions InstOK.null_address.
Print Assumptions InstOK.equal_pointer_equal_address.
Print Assumptions InstOK.address_and_provenance_identify_nonnull.
Print Assumptions InstOK.same_address_distinct_provenance.
Print Assumptions InstOK.struct_has_allocation_id.
