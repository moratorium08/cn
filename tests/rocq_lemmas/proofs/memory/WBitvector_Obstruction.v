From Stdlib Require Import ZArith Lia.
From iris.proofmode Require Import proofmode.
Require Import CN_Lemmas.CN_Memory_Iris IteratedExport.WBitvectorOutput.
Open Scope Z_scope.
Module Inst <: Parameters. End Inst.
Module L := Lemma_Defs Inst.
Section Proof.
 Context {cn_selectors : Selectors} `{!heapGS_gen Σ}.
 Lemma bitvector_zero_output_obstruction p :
   Is_true (has_alloc_id p) ->
   CN_Lib.wrapI 0 18446744073709551615
     ((CN_Lib.wrapI 0 18446744073709551615 (addr_of p)) mod 4) = 0 ->
   each_resource (fun i : Z => (0 <= i /\ i <= 18446744073709551615) /\ i = 0)
     (fun i => BlockSized 4 (arrayshift p 4 i)) -∗
   L.unsupported_w_ghost_type -∗ False.
 Proof.
   iIntros (Hid Ha) "H Hbad".
   iDestruct ("Hbad" $! p (fun _ => 1%Z) with "[$H] []") as "[%Hzero _]".
   - iPureIntro. split; last done. intros i Hi. lia.
   - done.
   - discriminate.
 Qed.
End Proof.
Print Assumptions bitvector_zero_output_obstruction.
