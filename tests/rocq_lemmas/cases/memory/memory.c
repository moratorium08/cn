struct memory_probe {
  unsigned long prefix;
  unsigned long node;
};

/*@
lemma member_container_cancel_wrapped(pointer p)
  requires !is_null(p);
  ensures ptr_eq(array_shift<char>(member_shift<struct memory_probe>(p, node),
                    18446744073709551608u64), p);

lemma shift_preserves_provenance(pointer p, u64 offset)
  requires true;
  ensures (alloc_id)array_shift<char>(p, offset) == (alloc_id)p;

lemma shift_has_allocation_id(pointer p, u64 offset)
  requires has_alloc_id(p);
  ensures has_alloc_id(array_shift<char>(p, offset));

lemma null_address()
  requires true;
  ensures (u64)NULL == 0u64;

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
@*/
