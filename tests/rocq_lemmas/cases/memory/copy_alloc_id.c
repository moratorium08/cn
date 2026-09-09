/*@
lemma copy_alloc_id_preserves_provenance(pointer p, u64 address)
  requires true;
  ensures (alloc_id)copy_alloc_id(address, p) == (alloc_id)p;
@*/
