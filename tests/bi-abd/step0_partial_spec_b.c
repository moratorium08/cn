/* Step 0 regression: the sandwich upper bound B (paper: "Data points and
   sandwich constraints", B_j = dom H \ (user footprint ∪ locals)).

   The user's precondition already owns the first node.  An inferred
   assertion is *-conjoined with the user's, so IntList(p) — whose footprint
   includes the first node — must be rejected (its footprint escapes B).
   The honest result today is "inference failed": expressing the correct
   completion needs a qualifier chain rooted at First.next (PLAN.md Step 3). */

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

int list_length(struct node *p)
/*@ requires !is_null(p);
             take First = RW<struct node>(p);
    ensures take First2 = RW<struct node>(p); @*/
{
  int n = 0;
  struct node *cur = p;
  while (cur != (void *)0) {
    n = n + 1;
    cur = cur->next;
  }
  return n;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  return list_length(&n1) - 3;
}
