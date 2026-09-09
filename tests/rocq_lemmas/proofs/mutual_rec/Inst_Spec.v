Require Import ZArith.
Require Import CN_Lemmas.Gen_Spec.

Module Inst.

Definition Alloc : (Z * Z) -> Prop := fun _ => True.

End Inst.

Module InstOK : CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).
Module L := CN_Lemmas.Gen_Spec.Lemma_Defs(Inst).
End InstOK.
