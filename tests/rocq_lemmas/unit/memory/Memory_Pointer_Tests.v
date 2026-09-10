From Stdlib Require Import ZArith Lia.
From stdpp Require Import countable.
Require Import CN_Lemmas.CN_Memory.
Open Scope Z_scope.

(* These are memory-model regression proofs, not allocator lemma proofs. *)
Module W64 <: WIDTH.
  Definition pointer_bits : nat := 64%nat.
End W64.

Module Bounded64 := BoundedAddress W64.
Module Integer64 := IntegerAddress W64.

Module BVVIP := Make Bounded64 VIP.
Module IntegerVIP := Make Integer64 VIP.
Module BVNoVIP := Make Bounded64 NoVIP.

Lemma offset_cancel_mod (z offset modulus : Z) :
  0 <= z < modulus ->
  (((z + offset) mod modulus + -offset) mod modulus) = z.
Proof.
  intros Hz.
  rewrite (Z.add_mod ((z + offset) mod modulus) (-offset) modulus) by lia.
  rewrite Z.mod_mod by lia.
  rewrite <- Z.add_mod by lia.
  replace (z + offset + -offset) with z by lia.
  apply Z.mod_small; assumption.
Qed.

Section BV_VIP.
  Context {S : BVVIP.Selectors}.

  Lemma bv_vip_shift_cancel p offset :
    BVVIP.has_alloc_id p = true ->
    BVVIP.ptr_shift (BVVIP.ptr_shift p offset) (-offset) = p.
  Proof.
    destruct p as [[aid [z Hz]]|]; last discriminate.
    intros _.
    unfold BVVIP.ptr_shift, BVVIP.aia, BVVIP.alloc_id_of, BVVIP.raw_address.
    cbn [BVVIP.address_Z BVVIP.address].
    f_equal. f_equal.
    apply eq_sig_hprop; first (intros; apply proof_irrel).
    cbn. apply offset_cancel_mod.
    apply bool_decide_eq_true in Hz. exact Hz.
  Qed.

  Lemma bv_vip_container_of_wrap aid :
    BVVIP.ptr_shift (BVVIP.ptr_shift (BVVIP.aia aid 4096) 8)
      (2 ^ 64 - 8) = BVVIP.aia aid 4096.
  Proof.
    unfold BVVIP.ptr_shift, BVVIP.aia, BVVIP.alloc_id_of, BVVIP.raw_address.
    cbn [BVVIP.address_Z BVVIP.address].
    f_equal. f_equal.
    apply eq_sig_hprop; first (intros; apply proof_irrel).
    vm_compute. reflexivity.
  Qed.

  Lemma bv_vip_same_address_distinct_ids :
    BVVIP.addr_of (BVVIP.aia 1 4096) = BVVIP.addr_of (BVVIP.aia 2 4096) /\
    BVVIP.ptr_eq (BVVIP.aia 1 4096) (BVVIP.aia 2 4096) = false.
  Proof. split; first reflexivity. apply bool_decide_eq_false_2. discriminate. Qed.

  Lemma bv_vip_null_is_not_zero_address aid :
    BVVIP.addr_of BVVIP.null = BVVIP.addr_of (BVVIP.aia aid 0) /\
    BVVIP.null <> BVVIP.aia aid 0.
  Proof. split; [reflexivity|discriminate]. Qed.

  Lemma bv_vip_null_shift_uses_selectors offset :
    BVVIP.ptr_shift BVVIP.null offset =
      BVVIP.aia BVVIP.null_alloc_id
        (BVVIP.address_Z BVVIP.null_address + offset).
  Proof. reflexivity. Qed.

  Lemma bv_vip_null_shift_not_null offset :
    BVVIP.ptr_shift BVVIP.null offset <> BVVIP.null.
  Proof. discriminate. Qed.

  Definition bv_history : BVVIP.History := fun _ =>
    {| allocation_base := 4096; allocation_size := 16 |}.

  Lemma bv_vip_one_past_bounds :
    BVVIP.in_bounds bv_history (BVVIP.aia 1 4112).
  Proof. vm_compute. intuition discriminate. Qed.

  Lemma bv_vip_one_past_rejects_nonempty_footprint :
    ~ BVVIP.footprint_ok bv_history (BVVIP.aia 1 4112) 1.
  Proof. vm_compute. intuition discriminate. Qed.
End BV_VIP.

Section Integer_VIP.
  Context {S : IntegerVIP.Selectors}.

  Lemma integer_vip_shift_cancel p offset :
    IntegerVIP.has_alloc_id p = true ->
    IntegerVIP.ptr_shift (IntegerVIP.ptr_shift p offset) (-offset) = p.
  Proof.
    destruct p as [[aid z]|]; last discriminate.
    intros _.
    unfold IntegerVIP.ptr_shift, IntegerVIP.aia, IntegerVIP.alloc_id_of,
      IntegerVIP.raw_address.
    cbn [IntegerVIP.address_Z IntegerVIP.address].
    f_equal. f_equal. change (z + offset + -offset = z). lia.
  Qed.

  Lemma integer_vip_unsigned_offset_does_not_cancel :
    IntegerVIP.ptr_shift (IntegerVIP.ptr_shift (IntegerVIP.aia 1 4096) 8)
      (2 ^ 64 - 8) <> IntegerVIP.aia 1 4096.
  Proof. vm_compute. discriminate. Qed.

  Lemma integer_vip_same_address_distinct_ids :
    IntegerVIP.addr_of (IntegerVIP.aia 1 4096) =
      IntegerVIP.addr_of (IntegerVIP.aia 2 4096) /\
    IntegerVIP.ptr_eq (IntegerVIP.aia 1 4096) (IntegerVIP.aia 2 4096) = false.
  Proof. split; first reflexivity. apply bool_decide_eq_false_2. discriminate. Qed.

  Lemma integer_vip_null_is_not_zero_address aid :
    IntegerVIP.addr_of IntegerVIP.null = IntegerVIP.addr_of (IntegerVIP.aia aid 0) /\
    IntegerVIP.null <> IntegerVIP.aia aid 0.
  Proof. split; [reflexivity|discriminate]. Qed.

  Lemma integer_vip_null_shift_uses_selectors offset :
    IntegerVIP.ptr_shift IntegerVIP.null offset =
      IntegerVIP.aia IntegerVIP.null_alloc_id
        (IntegerVIP.address_Z IntegerVIP.null_address + offset).
  Proof. reflexivity. Qed.

  Lemma integer_vip_null_shift_not_null offset :
    IntegerVIP.ptr_shift IntegerVIP.null offset <> IntegerVIP.null.
  Proof. discriminate. Qed.

  Definition integer_history : IntegerVIP.History := fun _ =>
    {| allocation_base := 4096; allocation_size := 16 |}.

  Lemma integer_vip_one_past_bounds :
    IntegerVIP.in_bounds integer_history (IntegerVIP.aia 1 4112).
  Proof. vm_compute. intuition discriminate. Qed.

  Lemma integer_vip_one_past_rejects_nonempty_footprint :
    ~ IntegerVIP.footprint_ok integer_history (IntegerVIP.aia 1 4112) 1.
  Proof. vm_compute. intuition discriminate. Qed.
End Integer_VIP.

Section BV_NoVIP.
  Context {S : BVNoVIP.Selectors}.

  Lemma bv_novip_shift_cancel p offset :
    BVNoVIP.has_alloc_id p = true ->
    BVNoVIP.ptr_shift (BVNoVIP.ptr_shift p offset) (-offset) = p.
  Proof.
    destruct p as [[aid [z Hz]]|]; last discriminate.
    intros _.
    unfold BVNoVIP.ptr_shift, BVNoVIP.aia, BVNoVIP.alloc_id_of, BVNoVIP.raw_address.
    cbn [BVNoVIP.address_Z BVNoVIP.address].
    f_equal. f_equal.
    apply eq_sig_hprop; first (intros; apply proof_irrel).
    cbn. apply offset_cancel_mod.
    apply bool_decide_eq_true in Hz. exact Hz.
  Qed.

  Lemma bv_novip_allocation_ids_collapse :
    BVNoVIP.aia (BVNoVIP.allocation_id 1) 4096 =
      BVNoVIP.aia (BVNoVIP.allocation_id 2) 4096.
  Proof. reflexivity. Qed.

  Lemma bv_novip_null_is_not_zero_address :
    BVNoVIP.addr_of BVNoVIP.null = BVNoVIP.addr_of (BVNoVIP.aia tt 0) /\
    BVNoVIP.null <> BVNoVIP.aia tt 0.
  Proof. split; [reflexivity|discriminate]. Qed.

  Lemma bv_novip_null_shift_uses_selectors offset :
    BVNoVIP.ptr_shift BVNoVIP.null offset =
      BVNoVIP.aia BVNoVIP.null_alloc_id
        (BVNoVIP.address_Z BVNoVIP.null_address + offset).
  Proof. reflexivity. Qed.

  Lemma bv_novip_null_shift_not_null offset :
    BVNoVIP.ptr_shift BVNoVIP.null offset <> BVNoVIP.null.
  Proof. discriminate. Qed.

  Definition novip_history : BVNoVIP.History := fun _ =>
    {| allocation_base := 4096; allocation_size := 16 |}.

  Lemma bv_novip_one_past_bounds :
    BVNoVIP.in_bounds novip_history (BVNoVIP.aia tt 4112).
  Proof. vm_compute. intuition discriminate. Qed.

  (* No-VIP explicitly omits the allocation-bounds part of footprint_ok.
     Treating this like the VIP rejection theorem would misstate the model. *)
  Lemma bv_novip_footprint_ignores_allocation_end :
    BVNoVIP.footprint_ok novip_history (BVNoVIP.aia tt 4112) 1.
  Proof. vm_compute. intuition discriminate. Qed.
End BV_NoVIP.

Print Assumptions bv_vip_shift_cancel.
Print Assumptions bv_vip_container_of_wrap.
Print Assumptions bv_vip_same_address_distinct_ids.
Print Assumptions bv_vip_null_is_not_zero_address.
Print Assumptions bv_vip_null_shift_uses_selectors.
Print Assumptions bv_vip_null_shift_not_null.
Print Assumptions bv_vip_one_past_bounds.
Print Assumptions bv_vip_one_past_rejects_nonempty_footprint.
Print Assumptions integer_vip_shift_cancel.
Print Assumptions integer_vip_unsigned_offset_does_not_cancel.
Print Assumptions integer_vip_same_address_distinct_ids.
Print Assumptions integer_vip_null_is_not_zero_address.
Print Assumptions integer_vip_null_shift_uses_selectors.
Print Assumptions integer_vip_null_shift_not_null.
Print Assumptions integer_vip_one_past_bounds.
Print Assumptions integer_vip_one_past_rejects_nonempty_footprint.
Print Assumptions bv_novip_shift_cancel.
Print Assumptions bv_novip_allocation_ids_collapse.
Print Assumptions bv_novip_null_is_not_zero_address.
Print Assumptions bv_novip_null_shift_uses_selectors.
Print Assumptions bv_novip_null_shift_not_null.
Print Assumptions bv_novip_one_past_bounds.
Print Assumptions bv_novip_footprint_ignores_allocation_end.
