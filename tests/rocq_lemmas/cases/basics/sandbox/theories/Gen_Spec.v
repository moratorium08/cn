(* tests/rocq_lemmas/cases/basics/sandbox/theories/Gen_Spec.v: generated lemma specifications from CN *)

Require Import ZArith Bool.
Require CN_Lemmas.CN_Lib.
Require Import CN_Lemmas.CN_Lib_Iris.
From iris.bi.lib Require Import fixpoint_mono.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Lib_Iris_Fixpoint.


Module Types.

  Record data : Type := { 
  x : Z; 
 }.
  Record stct : Type := { 
  first : Z; 

  second : Z; 
 }.
  Inductive
    data_option : Type :=
    | Data_none : data_option
    | Data_some : data -> data_option.

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

  Definition Owned_data (l: Ptr) (v : data) : iProp Σ := Owned_int (CN_Lib_Iris.shift l 0 4) v.(x).

  Definition Owned_stct (l: Ptr) (v : stct) : iProp Σ := Owned_int (CN_Lib_Iris.shift l 0 4) v.(first) ∗ Owned_int (CN_Lib_Iris.shift l 4 4) v.(second).

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
  (* no resource predicates required *)

End ResourcePredicates.


Module Lemma_Defs (P : Parameters).
  Module D := Defs(P).
  Module R := ResourcePredicates(P).
  Import Types D P R.
  Open Scope Z.



  (* Opening Iris mode *)
  Section Iris_Type_Defs.
  Context `{!heapGS_gen Σ}.

  Definition data_option_trivial_type : iProp Σ :=
    ∀ (x : data_option),
⌜ (Is_true true) ⌝ -∗ ⌜ (Is_true true) ⌝ ∗ emp.

  Definition sizeof_offsetof_lemma_type : iProp Σ :=
    ⌜ (Is_true true) ⌝ -∗ (let x := (8) in (let y := (4) in ⌜ (y < x)
⌝ ∗ emp)).


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

  Parameter data_option_trivial : ⊢ data_option_trivial_type.

  Parameter sizeof_offsetof_lemma : ⊢ sizeof_offsetof_lemma_type.


  (* Closing Iris mode *)
  End Lemma_Defs.
End Lemma_Spec.


