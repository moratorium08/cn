/* Step 2 regression: condition (†) must hold on EVERY data point
   (sec-inference.tex, Definition "Data-relative inference").

   first_val is called both with a one-node list and with NULL.  On the
   non-NULL activation alone, RW<struct node>(p) covers the anti-frame and a
   representative-based inference would suggest it — but that spec fails on
   the NULL activation (W(NULL) has no derivation).  Requiring
   F(q, d_j) ≠ ⊥ for all j forces the guarded predicate IntList(p), whose
   is_null clause gives the empty footprint on the NULL run. */

#include <stddef.h>

struct node {
  int val;
  struct node *next;
};

/*@
datatype seq {
  Seq_Nil {},
  Seq_Cons {i32 head, datatype seq tail}
}

predicate [rec] (datatype seq) IntList(pointer p) {
  if (is_null(p)) {
    return Seq_Nil{};
  } else {
    take H = RW<struct node>(p);
    take tl = IntList(H.next);
    return (Seq_Cons { head: H.val, tail: tl });
  }
}
@*/

int first_val(struct node *p)
/*@ requires true;
    ensures true; @*/
{
  if (p == (void *)0) {
    return 0;
  }
  return p->val;
}

int main(void)
/*@ trusted; @*/
{
  struct node n1 = {.val = 7, .next = (void *)0};
  int a = first_val(&n1);
  int b = first_val((void *)0);
  return a + b - 7;
}
