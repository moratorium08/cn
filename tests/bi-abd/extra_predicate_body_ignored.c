/* Expected: no suggestion of NegList(p), because the observed list has only
   positive values and cannot satisfy the predicate's assertions.
   Current baseline inference uses only heap shape for recursive predicates and
   ignores the predicate body's semantic constraints. */

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

predicate [rec] (datatype seq) NegList(pointer p) {
  if (is_null(p)) {
    return Seq_Nil{};
  } else {
    take H = RW<struct node>(p);
    assert(H.val < 0i32);
    take T = NegList(H.next);
    return Seq_Cons { head: H.val, tail: T };
  }
}
@*/

int list_length(struct node *p)
/*@ requires true;
    ensures true; @*/
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
  struct node n2 = {.val = 2, .next = (void *)0};
  struct node n1 = {.val = 1, .next = &n2};
  return list_length(&n1);
}
