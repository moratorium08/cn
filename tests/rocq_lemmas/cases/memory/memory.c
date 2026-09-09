/* Integer-mode fixtures: C integer types are mathematical integers, so
 * pointer arithmetic does not wrap and negative offsets are plain negation. */
struct memory_probe {
  unsigned long prefix;
  unsigned long node;
};

/* Shares a member name with memory_probe; Rocq projections must not clash. */
struct memory_probe_only {
  unsigned long node;
};

/*@
lemma member_container_cancel(pointer p)
  requires !is_null(p);
  ensures ptr_eq(array_shift<char>(member_shift<struct memory_probe>(p, node),
                    -offsetof(memory_probe, node)), p);

lemma shift_preserves_provenance(pointer p, integer offset)
  requires true;
  ensures (alloc_id)array_shift<char>(p, offset) == (alloc_id)p;

lemma shift_has_allocation_id(pointer p, integer offset)
  requires has_alloc_id(p);
  ensures has_alloc_id(array_shift<char>(p, offset));

lemma null_address()
  requires true;
  ensures (integer)NULL == 0;

lemma equal_pointer_equal_address(pointer p, pointer q)
  requires ptr_eq(p, q);
  ensures addr_eq(p, q);

lemma address_and_provenance_identify_nonnull(pointer p, pointer q)
  requires !is_null(p); !is_null(q);
    addr_eq(p, q); (alloc_id)p == (alloc_id)q;
  ensures ptr_eq(p, q);

lemma same_address_distinct_provenance(pointer p, pointer q)
  requires addr_eq(p, q); (alloc_id)p != (alloc_id)q;
  ensures !ptr_eq(p, q);

lemma struct_has_allocation_id(pointer p)
  requires take before = RW<struct memory_probe>(p);
  ensures take after = RW<struct memory_probe>(p);
    has_alloc_id(p);

lemma shared_member_name(pointer p)
  requires take before = RW<struct memory_probe_only>(p);
  ensures take after = RW<struct memory_probe_only>(p);
    after.node == before.node;
@*/
