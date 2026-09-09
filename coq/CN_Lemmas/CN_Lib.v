Require List.
Require Import ZArith Bool.
Require Import Lia.
Require NArith.

Definition wrapI (minInt : Z) (maxInt : Z) x :=
  let delta := ((maxInt - minInt) + 1)%Z in
  let r := Z.modulo x delta in
  (if (r <=? maxInt) then r else r - delta)%Z.

Lemma wrapI_idem:
  forall (minInt maxInt x : Z),
  (minInt <= x <= maxInt)%Z ->
  (minInt <= 0 < maxInt)%Z ->
  wrapI minInt maxInt x = x.
Proof.
  Open Scope Z.
  intros.
  unfold wrapI.
  pose (delta := ((maxInt - minInt) + 1)).
  destruct (0 <=? x) eqn: x_neg.
  - rewrite Z.mod_small by lia.
    rewrite Zle_imp_le_bool by lia.
    reflexivity.
  - rewrite (Znumtheory.Zdivide_mod_minus _ _ (x + delta)).
    + destruct (x + delta <=? maxInt) eqn: leb; lia.
    + lia.
    + exists (-1); lia.
Qed.

(* CN integer division follows the SMT translation in lib/solver.ml.
   SMT-LIB [div]/[mod] are Euclidean: for b <> 0, a = b * div a b + mod a b
   with 0 <= mod a b < |b|.  Rocq's Z.div/Z.modulo agree when b > 0 but
   give a non-positive remainder when b < 0.  [rem] follows Z3, where the
   remainder takes the sign of the divisor (cvc5 has no Int rem).  Division
   by zero is uninterpreted in SMT; here it inherits Rocq's conventions. *)
Definition mod_smt (a b : Z) : Z := Z.modulo a (Z.abs b).
Definition div_smt (a b : Z) : Z := if (b <? 0)%Z then (- (a / (- b)))%Z else (a / b)%Z.
Definition rem_smt (a b : Z) : Z := if (b <? 0)%Z then (- mod_smt a b)%Z else mod_smt a b.

Lemma mod_smt_pos (a b : Z) : (0 < b)%Z -> mod_smt a b = (a mod b)%Z.
Proof. intros. unfold mod_smt. now rewrite Z.abs_eq by lia. Qed.

Lemma div_smt_pos (a b : Z) : (0 < b)%Z -> div_smt a b = (a / b)%Z.
Proof.
  intros. unfold div_smt. destruct (b <? 0)%Z eqn:E; [apply Z.ltb_lt in E; lia | reflexivity].
Qed.

Lemma rem_smt_pos (a b : Z) : (0 < b)%Z -> rem_smt a b = (a mod b)%Z.
Proof.
  intros. unfold rem_smt. rewrite mod_smt_pos by assumption.
  destruct (b <? 0)%Z eqn:E; [apply Z.ltb_lt in E; lia | reflexivity].
Qed.

Lemma mod_smt_bound (a b : Z) : b <> 0%Z -> (0 <= mod_smt a b < Z.abs b)%Z.
Proof. intros. unfold mod_smt. apply Z.mod_pos_bound. lia. Qed.

Lemma div_mod_smt (a b : Z) : b <> 0%Z -> a = (b * div_smt a b + mod_smt a b)%Z.
Proof.
  intros Hb. unfold div_smt, mod_smt.
  destruct (b <? 0)%Z eqn:E.
  - apply Z.ltb_lt in E. rewrite Z.abs_neq by lia.
    pose proof (Z.div_mod a (- b) ltac:(lia)). lia.
  - apply Z.ltb_ge in E. rewrite Z.abs_eq by lia.
    pose proof (Z.div_mod a b Hb). lia.
Qed.

Lemma mod_smt_1_r (a : Z) : mod_smt a 1 = 0%Z.
Proof. unfold mod_smt. apply Z.mod_1_r. Qed.
