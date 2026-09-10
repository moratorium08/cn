(* Instantiation of the CN-exported specification for arrays/inductive.

   [Array] is an Iris least fixpoint; the proofs below go through the
   generated [Array_induction] / [Array_unfold] lemmas.

   STATUS: BLOCKED.  The generated [array_lemma_type] is not provable in the
   exported memory model.  Everything except one sub-goal is proved here
   ([Array_append_aux], [array_lemma_null_reduction]); the remaining sub-goal is

       m <> 0 ->  Array (arrayshift null 4 0) m ws  ⊢  False

   In the new model [Ptr = option (AllocId * Address)] and
   [arrayshift null 4 0 = Some (null_alloc_id, address_Z null_address + 0)],
   which is a non-null pointer, so for [p = null], [n = 0], [m <> 0] the
   hypotheses [Array null 0 Nil] (trivially true) and
   [Array (arrayshift null 4 0) m ws] (a satisfiable ownership of 4*m bytes at
   the selector-chosen null address) do not contradict each other, while the
   conclusion [Array null m ws] requires [Owned_integer 4 true null V], i.e.
   [footprint_ok ... null 4], i.e. [has_alloc_id null = true], which is false.
   Hence the statement has a countermodel and cannot be closed without
   weakening it.  [InstOK] below is left in place (referring to the missing
   [array_lemma]) so that the test reports FAIL instead of a spurious PASS. *)

From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import iprop.
Require Import CN_Lemmas.Gen_Spec.
Require CN_Lemmas.CN_Lib.
Import CN_Lemmas.Gen_Spec.Types.

Module Inst <: Parameters.
End Inst.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Import L L.D L.R.
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
        cbv [address address_Z Address];
        f_equal; f_equal; lia.
    Qed.

    Lemma shift_zero (p : Ptr) (s : Z) :
      Is_true (has_alloc_id p) -> arrayshift p s 0 = p.
    Proof.
      destruct p as [[aid a]|]; last (intros []).
      intros _. unfold arrayshift, ptr_shift, aia, alloc_id_of, raw_address.
      cbv [address address_Z Address].
      f_equal; f_equal; lia.
    Qed.

    Lemma Array_nonneg :
      ⊢ ∀ (p : Ptr) (n : Z) (vs : List), Array p n vs -∗ ⌜0 <= n⌝.
    Proof.
      iApply Array_induction.
      iIntros "!>" (q k ws) "H". cbv beta.
      iDestruct "H" as "[[%Hk _] | (_ & %Hk & H)]"; first (iPureIntro; lia).
      iDestruct "H" as (V) "(_ & _ & _ & H)".
      iDestruct "H" as (VS) "[%HVS _]". iPureIntro; lia.
    Qed.

    Lemma Array_zero (p : Ptr) (ws : List) :
      Array p 0 ws -∗ ⌜ws = Nil⌝.
    Proof.
      rewrite Array_unfold.
      iIntros "[[_ %Hws] | (_ & %Hn & _)]"; [done | lia].
    Qed.

    (* The append property, under the side condition that rules out the
       unprovable [p = null, n = 0, m <> 0] corner. *)
    Lemma Array_append_aux :
      ⊢ ∀ (p : Ptr) (n : Z) (vs : List),
      Array p n vs -∗
      ⌜0 <= n⌝ ∗
      ∀ (m : Z) (ws : List),
        ⌜Is_true (has_alloc_id p) \/ m = 0 \/ n <> 0⌝ -∗
        Array (arrayshift p 4 n) m ws -∗
        Array p (n + m) (Append vs ws).
    Proof.
      iApply Array_induction.
      iIntros "!>" (q k xs) "H". cbv beta.
      iDestruct "H" as "[[%Hk %Hxs] | (_ & %Hk & H)]".
      - (* k = 0, xs = Nil *)
        subst k xs. iSplit; first done.
        iIntros (m ws) "%Hcase Hws".
        change (Append Nil ws) with ws. rewrite Z.add_0_l.
        destruct Hcase as [Hq | [-> | Hk]]; last done.
        + rewrite (shift_zero q 4 Hq). done.
        + iDestruct (Array_zero with "Hws") as %->.
          iClear "Hws". iEval (rewrite Array_unfold). iLeft. iSplit; iPureIntro; reflexivity.
      - (* k <> 0 *)
        iDestruct "H" as (V) "(HV & %Haddr & %Hrange & H)".
        iDestruct "H" as (VS) "[[%HVS IH] %Hxs]". subst xs.
        iSplit; first (iPureIntro; lia).
        iIntros (m ws) "_ Hws".
        iDestruct (Array_nonneg with "Hws") as %Hm.
        iSpecialize ("IH" $! m ws with "[%] [Hws]").
        { left. exact I. }
        { rewrite shift_add. replace (1 + (k - 1)) with k by lia. done. }
        change (Append (Cons V VS) ws) with (Cons V (Append VS ws)).
        iEval (rewrite Array_unfold). iRight.
        iSplit; first done. iSplit; first (iPureIntro; lia).
        iExists V. iFrame "HV". iSplit; first done. iSplit; first done.
        iExists (Append VS ws).
        replace (k + m - 1) with (k - 1 + m) by lia. iFrame "IH". done.
    Qed.

    (* The fixture requires !is_null(p): for p = NULL and n = 0 the second
       Array would live at arrayshift null 4 0, a non-null pointer carrying the
       null selectors' provenance, which cannot be re-rooted at NULL. *)
    Lemma array_lemma : ⊢ array_lemma_type.
    Proof.
      unfold array_lemma_type.
      iIntros (p n m) "%Hp". iIntros (vs) "Hvs". iIntros (ws) "Hws".
      iDestruct (Array_append_aux with "Hvs") as "[%Hn IH]".
      iExists (Append vs ws). iSplitL; last done.
      destruct p as [[aid a]|].
      { iApply ("IH" with "[%] Hws"). left. exact I. }
      exfalso. apply Hp. unfold is_null, Memory.ptr_eq, Memory.null.
      apply bool_decide_pack. reflexivity.
    Qed.

  End Proof.

End InstOK.

Print Assumptions InstOK.array_lemma.
