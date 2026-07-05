/* Example: sum over a singly linked list (full traversal, reads val and
   next).  Expected: IntList(p) for both pre and post. */

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

int list_sum(struct node *p)
/*@ requires true;
    ensures true; @*/
{
  int s = 0;
  struct node *cur = p;
  while (cur != (void *)0) {
    s = s + cur->val;
    cur = cur->next;
  }
  return s;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  return list_sum(&n1) - 6;
}
