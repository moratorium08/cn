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

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    (* Integer mode: addresses are Z, so the negated offsetof cancels exactly. *)
    Lemma member_container_cancel : ⊢ member_container_cancel_type.
    Proof.
      iIntros (p) "%Hnonnull". iSplitL; last done. iPureIntro.
      destruct p as [[aid z]|]; last (exfalso; apply Hnonnull; reflexivity).
      unfold D.ptr_eq, Memory.ptr_eq. apply bool_decide_spec.
      unfold arrayshift, ptr_shift, aia, alloc_id_of, raw_address.
      f_equal. f_equal.
      cbv [address address_Z Address] in *. lia.
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
      cbv [addr_of address_Z Address] in *. subst qz.
      unfold D.ptr_eq, Memory.ptr_eq. apply bool_decide_spec. reflexivity.
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
      iIntros (p before) "H %Halign %Hrange".
      iDestruct "H" as "[%Hfoot Hfields]".
      iExists before. iSplitL "Hfields".
      - iSplit; [done|iFrame].
      - iSplit; [done|]. iSplit; [done|]. iSplit; [|done]. iPureIntro.
        destruct Hfoot as (Hid & _). rewrite Hid. simpl. tauto.
    Qed.

    (* Both structs have a member [node]; the projections are distinct. *)
    Lemma shared_member_name : ⊢ shared_member_name_type.
    Proof.
      iIntros (p before) "H %Halign %Hrange".
      iExists before. iFrame. done.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.member_container_cancel.
Print Assumptions InstOK.shift_preserves_provenance.
Print Assumptions InstOK.shift_has_allocation_id.
Print Assumptions InstOK.null_address.
Print Assumptions InstOK.equal_pointer_equal_address.
Print Assumptions InstOK.address_and_provenance_identify_nonnull.
Print Assumptions InstOK.same_address_distinct_provenance.
Print Assumptions InstOK.struct_has_allocation_id.
Print Assumptions InstOK.shared_member_name.
