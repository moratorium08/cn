/* Expected: one symbolic suggestion IntList(p) should explain all calls.
   Current implementation unions concrete addresses across calls but computes
   footprints on one representative heap only, so another call stays uncovered. */

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
    take T = IntList(H.next);
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
  struct node a2 = {.val = 2, .next = (void *)0};
  struct node a1 = {.val = 1, .next = &a2};

  struct node b3 = {.val = 30, .next = (void *)0};
  struct node b2 = {.val = 20, .next = &b3};
  struct node b1 = {.val = 10, .next = &b2};

  return list_length(&a1) + list_length(&b1);
}
