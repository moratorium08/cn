/* Adversarial: predicate root is loaded from a scalar pointer cell.

   The right precondition is
     take P = RW<struct node *>(pp);
     take L = IntList(P);
   but chains through scalar pointer pointees are not enumerated. */

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

int indirect_length(struct node **pp)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = *pp;
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
  struct node *head = &n1;
  return indirect_length(&head) - 2;
}
