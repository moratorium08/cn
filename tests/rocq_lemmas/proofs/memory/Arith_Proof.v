From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import MemoryExport.Arith_Spec.

Module Inst <: Parameters.
End Inst.

Module InstOK <: Lemma_Spec(Inst).
  Module L := Lemma_Defs(Inst).
  Import L L.D.
  Open Scope Z_scope.

  Section Proof.
    Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

    Lemma euclidean_division : ⊢ euclidean_division_type.
    Proof.
      iIntros (a b) "%Hb".
      pose proof (CN_Lib.div_mod_smt a b Hb).
      pose proof (CN_Lib.mod_smt_bound a b Hb).
      iSplit; [iPureIntro; lia|]. iSplit; [iPureIntro; lia|].
      iSplit; [iPureIntro; lia|done].
    Qed.

    (* SMT-LIB div/mod and Z3 rem on negative operands. *)
    Lemma negative_divisor_division : ⊢ negative_divisor_division_type.
    Proof. iIntros "_". repeat iSplit; done. Qed.

    Lemma remainder_sign_follows_divisor : ⊢ remainder_sign_follows_divisor_type.
    Proof.
      iIntros (a b) "%Hb". iSplitL; last done. iPureIntro.
      unfold CN_Lib.rem_smt. apply Z.ltb_lt in Hb. rewrite Hb. reflexivity.
    Qed.

    Lemma shift_scaling : ⊢ shift_scaling_type.
    Proof. iIntros (x n) "_". repeat iSplit; done. Qed.

    Lemma negation_cancels : ⊢ negation_cancels_type.
    Proof.
      iIntros (x) "_". iSplit; [iPureIntro; lia|]. iSplit; [|done].
      iPureIntro. apply Z.abs_opp.
    Qed.
  End Proof.
End InstOK.

Print Assumptions InstOK.euclidean_division.
Print Assumptions InstOK.negative_divisor_division.
Print Assumptions InstOK.remainder_sign_follows_divisor.
Print Assumptions InstOK.shift_scaling.
Print Assumptions InstOK.negation_cancels.
