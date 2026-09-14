From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
From iris.base_logic Require Import invariants.
Require Import IteratedExport.WOutput.

Module Inst <: Parameters. End Inst.
Module L := Lemma_Defs Inst.
Import L.
Open Scope Z_scope.

Section Proof.
  Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.

  (* An alleged proof that W forces zero contradicts a valid arbitrary
     ghost output. The physical bytes are never assumed to be zero. *)
  Lemma zero_output_obstruction (p : Ptr) :
    Is_true (has_alloc_id p) ->
    (0 <= addr_of p <= 18446744073709551615 /\
     CN_Lib.mod_smt (addr_of p) 4 = 0) ->
    each_resource (fun i : Z => 0 <= i /\ i < 1)
      (fun i => BlockSized 4%nat (arrayshift p 4 i)) -∗
    unsupported_w_output_type -∗ False.
  Proof.
    iIntros (Halloc Hgood) "H Hbad".
    iDestruct ("Hbad" $! p (fun _ => 1%Z) with "[$H] []") as "[%Hzero _]";
      [done|done|].
    simpl in Hzero. discriminate.
  Qed.
End Proof.

Print Assumptions zero_output_obstruction.
