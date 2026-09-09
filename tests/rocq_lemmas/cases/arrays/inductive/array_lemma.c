#include "./headers.h" 

/* !is_null(p) is needed in the VIP model: for p == NULL and n == 0 the
   second Array lives at array_shift(NULL, 0), a non-null pointer with the
   null selectors' provenance, and cannot be re-rooted at NULL. */
/*@
lemma array_lemma (pointer p, integer n, integer m)
  requires 
    !is_null(p);
    take vs = Array(p,n);
    take ws = Array(array_shift<unsigned int>(p,n),m);
  ensures 
    take xs = Array(p,n+m);
    xs == Append(vs,ws);
@*/