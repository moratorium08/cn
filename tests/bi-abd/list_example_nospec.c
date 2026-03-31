/* Example 2: Linked list (no specifications) */

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
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = p;
  while (cur != (void *)0)
  /*@ inv true; @*/
  {
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

  int len = list_length(&n1);
  return 0;
}
