(* proofs/forest/Gen_Spec.v: generated lemma specifications from CN *)

Require Import ZArith Bool.
Require CN_Lemmas.CN_Lib.
Require Import CN_Lemmas.CN_Lib_Iris.
From iris.bi.lib Require Import fixpoint_mono.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Lib_Iris_Fixpoint.


Module Types.

  Inductive
    forest_cn : Type :=
    | Nil : forest_cn
    | Cons : tree_cn -> forest_cn -> forest_cn
    with
    tree_cn : Type :=
    | Node : forest_cn -> Z -> tree_cn.

Inductive cn_predicate_group_0 : Type :=
  | CN_GROUP_Forest : Ptr -> forest_cn -> cn_predicate_group_0
  | CN_GROUP_Tree : Ptr -> tree_cn -> cn_predicate_group_0.
Canonical Structure cn_predicate_group_0O := leibnizO cn_predicate_group_0.

Inductive cn_predicate_group_1 : Type :=
  | CN_GROUP_ForestSeg : Ptr ->
  Ptr -> forest_cn -> cn_predicate_group_1.
Canonical Structure cn_predicate_group_1O := leibnizO cn_predicate_group_1.

End Types.


Module Type Parameters.
  Import Types.
  Open Scope Z.
  (* no parameters required *)

End Parameters.


Module Defs (P : Parameters).
  (* Definitions of functions, structs, and struct ownership predicates *)
  Import Types P.
  Open Scope Z.

  (* Opening Iris mode *)
  Section Defs.
  Context `{!heapGS_gen Σ}.

  Record tree : Type := { 
  value : Z; 

  children : Ptr; 
 }.

  Definition Owned_tree (l: Ptr) (v : tree) : iProp Σ := Owned_int (CN_Lib_Iris.shift l 0 4) v.(value) ∗ padding (arrayshift l 4 1) 4 ∗ Owned_int (CN_Lib_Iris.shift l 8 8) v.(children).

  Record forest : Type := { 
  head : Ptr; 

  tail : Ptr; 
 }.

  Definition Owned_forest (l: Ptr) (v : forest) : iProp Σ := Owned_int (CN_Lib_Iris.shift l 0 8) v.(head) ∗ Owned_int (CN_Lib_Iris.shift l 8 8) v.(tail).

  Parameter Alloc : Ptr -> (Z * Z) -> iProp Σ.


  Definition MINu8 :=
    (0).

  Definition MAXu8 :=
    (255).

  Definition MINu16 :=
    (0).

  Definition MAXu16 :=
    (65535).

  Definition MINu32 :=
    (0).

  Definition MAXu32 :=
    (4294967295).

  Definition MINu64 :=
    (0).

  Definition MAXu64 :=
    (18446744073709551615).

  Definition MINi8 :=
    (-128).

  Definition MAXi8 :=
    (127).

  Definition MINi16 :=
    (-32768).

  Definition MAXi16 :=
    (32767).

  Definition MINi32 :=
    (-2147483648).

  Definition MAXi32 :=
    (2147483647).

  Definition MINi64 :=
    (-9223372036854775808).

  Definition MAXi64 :=
    (9223372036854775807).

  Definition not (arg : bool) :=
    (negb arg).

  Definition is_null (arg : Ptr) :=
    (arg =? 0).

  Definition ptr_eq (arg1 : Ptr) (arg2 : Ptr) :=
    (arg1 =? arg2).

  Definition prov_eq (arg1 : Ptr) (arg2 : Ptr) :=
    (arg1 =? arg2).

  Definition addr_eq (arg1 : Ptr) (arg2 : Ptr) :=
    (arg1 =? arg2).


  (* Closing Iris mode *)
  End Defs.

End Defs.


Module ResourcePredicates (P : Parameters).
  Module D := Defs(P).
  Import Types P D.
  Open Scope Z.
  (* Opening Iris mode *)
  Section Iris_Pred_Defs.
  Context `{!heapGS_gen Σ}.

  Definition Forest_body (Forest : Ptr -> forest_cn -> iProp Σ)
(Tree : Ptr -> tree_cn -> iProp Σ) (p : Ptr) (ν : forest_cn) :
iProp Σ :=
    (⌜ (is_null p) ⌝  ∧  ⌜ ν = (Nil) ⌝) ∨ (⌜
(Is_true true) ⌝ ∧ ⌜ ~(is_null p) ⌝  ∧ 
∃ (node : forest),
Owned_forest p node
 ∗ ∃ (head : tree_cn),
Tree node.(head) head ∗ ∃ (tail : forest_cn),
Forest node.(tail) tail ∗ ⌜ ν = (Cons head tail) ⌝).

  Definition Tree_body (Forest : Ptr -> forest_cn -> iProp Σ)
(Tree : Ptr -> tree_cn -> iProp Σ) (p : Ptr) (ν : tree_cn) :
iProp Σ :=
    (⌜ (Is_true true) ⌝  ∧ 
∃ (node : tree),
Owned_tree p node
 ∗ ∃ (children : forest_cn),
Forest node.(children) children ∗ ⌜
ν = (Node children node.(value)) ⌝).

  Definition cn_predicate_group_pre_0
(rec : cn_predicate_group_0O -> iProp Σ)
(call : cn_predicate_group_0O) : iProp Σ :=
    match call with
      | CN_GROUP_Forest p ν =>
        Forest_body (λ p ν, (rec (CN_GROUP_Forest p ν)))
        (λ p ν, (rec (CN_GROUP_Tree p ν))) p ν
      | CN_GROUP_Tree p ν =>
        Tree_body (λ p ν, (rec (CN_GROUP_Forest p ν)))
        (λ p ν, (rec (CN_GROUP_Tree p ν))) p ν
    end.
  Local Instance cn_predicate_group_0_mono :
    BiMonoPred cn_predicate_group_pre_0.
  Proof.
    solve_bi_mono_pred_with_prepare cn_predicate_group_pre_0
      ltac:(unfold Forest_body,Tree_body).
  Qed.

  Definition Forest (p : Ptr) (ν : forest_cn) : iProp Σ :=
    bi_least_fixpoint cn_predicate_group_pre_0
(CN_GROUP_Forest p ν).

  Definition Tree (p : Ptr) (ν : tree_cn) : iProp Σ :=
    bi_least_fixpoint cn_predicate_group_pre_0 (CN_GROUP_Tree p ν).

Lemma Forest_Tree_induction
    (Φ_Forest : Ptr -> forest_cn -> iProp Σ)
    (Φ_Tree : Ptr -> tree_cn -> iProp Σ) :
  ltac:(let T := constr:(□ (∀ (p : Ptr) (ν : forest_cn),
          (Forest_body Φ_Forest Φ_Tree p ν) -∗
            (Φ_Forest p ν)) -∗
        □ (∀ (p : Ptr) (ν : tree_cn),
          (Tree_body Φ_Forest Φ_Tree p ν) -∗ (Φ_Tree p ν)) -∗
        (∀ (p : Ptr) (ν : forest_cn),
          (Forest p ν) -∗ (Φ_Forest p ν)) ∧
        (∀ (p : Ptr) (ν : tree_cn),
          (Tree p ν) -∗ (Φ_Tree p ν))) in
        let T' := eval unfold Forest_body, Tree_body in T in
        exact T').
  Proof.
    iIntros "#H_Forest #H_Tree".
     solve_cn_predicate_induction
      cn_predicate_group_pre_0
      (fun call =>
         match call with
           | CN_GROUP_Forest p ν => Φ_Forest p ν
           | CN_GROUP_Tree p ν => Φ_Tree p ν
         end)
      ltac:(first [
        iApply ("H_Forest" with "Hbody")
        | iApply ("H_Tree" with "Hbody")])
      ltac:(unfold Forest, Tree).
  Qed.
Lemma Forest_unfold
    (p : Ptr)
    (ν : forest_cn) :
  ltac:(let T := constr:((Forest p ν) ⊣⊢
          (Forest_body Forest Tree p ν)) in
        let T' := eval unfold Forest_body in T in
        exact T').
  Proof.
    rewrite /Forest /Tree least_fixpoint_unfold
    /cn_predicate_group_pre_0 /= //.
  Qed.
Lemma Tree_unfold
    (p : Ptr)
    (ν : tree_cn) :
  ltac:(let T := constr:((Tree p ν) ⊣⊢
          (Tree_body Forest Tree p ν)) in
        let T' := eval unfold Tree_body in T in
        exact T').
  Proof.
    rewrite /Forest /Tree least_fixpoint_unfold
    /cn_predicate_group_pre_0 /= //.
  Qed.
  Definition ForestSeg_body
(ForestSeg : Ptr -> Ptr -> forest_cn -> iProp Σ) (p : Ptr)
(q : Ptr) (ν : forest_cn) : iProp Σ :=
    (⌜ (ptr_eq p q) ⌝  ∧  ⌜ ν = (Nil) ⌝) ∨ (⌜
(Is_true true) ⌝ ∧ ⌜ ~(ptr_eq p q) ⌝  ∧ 
⌜ (~ (is_null p)) ⌝ ∗ ∃ (node : forest),
Owned_forest p node
 ∗ ∃ (head : tree_cn),
Tree node.(head) head ∗ ∃ (tail : forest_cn),
ForestSeg node.(tail) q tail ∗ ⌜ ν = (Cons head tail) ⌝).

  Definition cn_predicate_group_pre_1
(rec : cn_predicate_group_1O -> iProp Σ)
(call : cn_predicate_group_1O) : iProp Σ :=
    match call with
      | CN_GROUP_ForestSeg p q ν =>
        ForestSeg_body
        (λ p q ν, (rec (CN_GROUP_ForestSeg p q ν))) p q ν
    end.
  Local Instance cn_predicate_group_1_mono :
    BiMonoPred cn_predicate_group_pre_1.
  Proof.
    solve_bi_mono_pred_with_prepare cn_predicate_group_pre_1
      ltac:(unfold ForestSeg_body).
  Qed.

  Definition ForestSeg (p : Ptr) (q : Ptr) (ν : forest_cn) :
iProp Σ :=
    bi_least_fixpoint cn_predicate_group_pre_1
(CN_GROUP_ForestSeg p q ν).

Lemma ForestSeg_induction
    (Φ_ForestSeg : Ptr -> Ptr -> forest_cn -> iProp Σ) :
  ltac:(let T := constr:(□ (∀ (p : Ptr) (q : Ptr)
        (ν : forest_cn),
          (ForestSeg_body Φ_ForestSeg p q ν) -∗
            (Φ_ForestSeg p q ν)) -∗
        (∀ (p : Ptr) (q : Ptr) (ν : forest_cn),
          (ForestSeg p q ν) -∗ (Φ_ForestSeg p q ν))) in
        let T' := eval unfold ForestSeg_body in T in
        exact T').
  Proof.
    iIntros "#H_ForestSeg".
     solve_cn_predicate_induction
      cn_predicate_group_pre_1
      (fun call =>
         match call with
           | CN_GROUP_ForestSeg p q ν => Φ_ForestSeg p q ν
         end)
      ltac:(first [
        iApply ("H_ForestSeg" with "Hbody")])
      ltac:(unfold ForestSeg).
  Qed.
Lemma ForestSeg_unfold
    (p : Ptr)
    (q : Ptr)
    (ν : forest_cn) :
  ltac:(let T := constr:((ForestSeg p q ν) ⊣⊢
          (ForestSeg_body ForestSeg p q ν)) in
        let T' := eval unfold ForestSeg_body in T in
        exact T').
  Proof.
    rewrite /ForestSeg least_fixpoint_unfold
    /cn_predicate_group_pre_1 /= //.
  Qed.

  (* Closing Iris mode *)
  End Iris_Pred_Defs.
End ResourcePredicates.


Module Lemma_Defs (P : Parameters).
  Module D := Defs(P).
  Module R := ResourcePredicates(P).
  Import Types D P R.
  Open Scope Z.



  (* Opening Iris mode *)
  Section Iris_Type_Defs.
  Context `{!heapGS_gen Σ}.

  Definition ForestSeg_Forest_type : iProp Σ :=
    ∀ (p : Ptr),
∀ (q : Ptr),
∀ (segment : forest_cn),
ForestSeg p q segment -∗ ∀ (suffix : forest_cn),
Forest q suffix -∗ ∃ (whole : forest_cn),
Forest p whole ∗ ⌜ Is_true true ⌝.


  (* Closing Iris mode *)
  End Iris_Type_Defs.
End Lemma_Defs.


Module Type Lemma_Spec (P : Parameters).

  Module L := Lemma_Defs(P).
  Import L.
  (* Opening Iris mode *)
  Section Lemma_Defs.
  Context `{!heapGS_gen Σ}.

  Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

  Parameter ForestSeg_Forest : ⊢ ForestSeg_Forest_type.


  (* Closing Iris mode *)
  End Lemma_Defs.
End Lemma_Spec.


