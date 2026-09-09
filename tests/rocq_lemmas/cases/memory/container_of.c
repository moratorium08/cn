struct memory_probe {
  unsigned long prefix;
  unsigned long node;
};

/*@
lemma member_container_cancel(pointer p)
  requires !is_null(p);
  ensures ptr_eq(array_shift<char>(member_shift<struct memory_probe>(p, node),
                    -offsetof(memory_probe, node)), p);
@*/
