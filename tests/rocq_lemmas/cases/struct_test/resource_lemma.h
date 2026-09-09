/*@
lemma resource_lemma (pointer p)
  requires 
      take v1 = Owned<int>(p);
      v1 == 0;
  ensures 
      take v2 = Owned<int>(p);
      v2 == 0;
@*/