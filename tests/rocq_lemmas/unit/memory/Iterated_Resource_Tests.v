From Stdlib Require Import ZArith Lia.
From stdpp Require Import infinite.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Memory_Iris.
Open Scope Z_scope.

Module IntegerVIP64 <: CONFIG.
  Definition pointer_bits : nat := 64%nat.
  Definition bitvectors := false.
  Definition vip := true.
End IntegerVIP64.
Module M := CN_Memory_Iris.Make IntegerVIP64.
Import M M.Memory.

Section Iteration.
  Context {Σ : gFunctors}.

  Lemma each_set (indices : gset Z) (body : Z -> iProp Σ) :
    each_resource (fun i => i ∈ indices) body ⊣⊢
      ([∗ set] i ∈ indices, body i).
  Proof.
    iSplit.
    - iIntros "H". iDestruct "H" as (actual) "[%Hactual H]".
      assert (actual = indices) as -> by (apply set_eq; exact Hactual).
      iExact "H".
    - iIntros "H". iExists indices. iFrame. done.
  Qed.

  Lemma each_guard_equiv (selected selected' : Z -> Prop) (body : Z -> iProp Σ) :
    (forall i, selected i <-> selected' i) ->
    each_resource selected body ⊣⊢ each_resource selected' body.
  Proof.
    intros Heq. iSplit; iIntros "H";
      iDestruct "H" as (indices) "[%Hindices H]";
      iExists indices; iFrame; iPureIntro; intro i;
      specialize (Hindices i); specialize (Heq i); tauto.
  Qed.

  Lemma each_empty (body : Z -> iProp Σ) :
    each_resource (fun _ => False%type) body ⊣⊢ emp.
  Proof.
    rewrite (each_guard_equiv _ (fun i => i ∈ (∅ : gset Z)) body);
      last (intro i; set_solver).
    rewrite each_set big_sepS_empty. done.
  Qed.

  Lemma each_singleton (index : Z) (body : Z -> iProp Σ) :
    each_resource (fun i => i = index) body ⊣⊢ body index.
  Proof.
    rewrite (each_guard_equiv _ (fun i => i ∈ ({[index]} : gset Z)) body);
      last (intro i; set_solver).
    rewrite each_set big_sepS_singleton. done.
  Qed.

  Lemma each_disjoint_union (left right : gset Z) (body : Z -> iProp Σ) :
    left ## right ->
    each_resource (fun i => i ∈ left ∪ right) body ⊣⊢
      each_resource (fun i => i ∈ left) body ∗
      each_resource (fun i => i ∈ right) body.
  Proof. intros Hdisjoint. rewrite !each_set big_sepS_union //. Qed.

  Lemma each_sparse_negative (body : Z -> iProp Σ) :
    each_resource (fun i => i = -7 \/ i = 11) body ⊣⊢ body (-7) ∗ body 11.
  Proof.
    rewrite (each_guard_equiv _ (fun i => i ∈ ({[-7]} ∪ {[11]} : gset Z)) body);
      last (intro i; set_solver).
    rewrite each_disjoint_union; last set_solver.
    rewrite !each_set !big_sepS_singleton. done.
  Qed.

  Lemma each_sparse_permutation (body : Z -> iProp Σ) :
    each_resource (fun i => i = -7 \/ i = 11) body ⊣⊢
      each_resource (fun i => i = 11 \/ i = -7) body.
  Proof. apply each_guard_equiv. intros; tauto. Qed.

  (* A finite support is not conjured for an arbitrary infinite guard. *)
  Lemma each_infinite_impossible (body : Z -> iProp Σ) :
    each_resource (fun _ => True%type) body ⊢ False.
  Proof.
    iIntros "H". iDestruct "H" as (indices) "[%Hindices _]".
    iPureIntro.
    pose proof (is_fresh indices) as Hfresh.
    apply Hfresh. apply Hindices. done.
  Qed.
End Iteration.

Section ExclusiveOwnership.
  Context `{!M.heapGS_gen Σ}.

  Definition one_byte (location : Address) (value : Val) : iProp Σ :=
    @pointsto Address address_eq_dec address_countable Val Σ heapGS_gen_heapGS
      location (DfracOwn 1) value.

  (* Different iteration indices do not permit aliasing full byte ownership. *)
  Lemma each_aliasing_byte_impossible (location : Address) (value : Val) :
    each_resource (fun i => i = -7 \/ i = 11)
      (fun _ => one_byte location value) ⊢ False.
  Proof.
    rewrite each_sparse_negative. iIntros "[Hleft Hright]".
    iDestruct (pointsto_ne with "Hleft Hright") as %Hinvalid.
    done.
  Qed.
End ExclusiveOwnership.

Print Assumptions each_set.
Print Assumptions each_guard_equiv.
Print Assumptions each_empty.
Print Assumptions each_singleton.
Print Assumptions each_disjoint_union.
Print Assumptions each_sparse_negative.
Print Assumptions each_sparse_permutation.
Print Assumptions each_infinite_impossible.
Print Assumptions each_aliasing_byte_impossible.
