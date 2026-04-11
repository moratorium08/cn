/* Test: multiple calls to same function with different-length lists */

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
  {
    n = n + 1;
    cur = cur->next;
  }
  return n;
}

int main(void)
/*@ trusted; @*/
{
  /* Call 1: length-1 list */
  struct node a1 = {.val = 10, .next = (void *)0};
  int len1 = list_length(&a1);

  /* Call 2: length-3 list */
  struct node b3 = {.val = 3, .next = (void *)0};
  struct node b2 = {.val = 2, .next = &b3};
  struct node b1 = {.val = 1, .next = &b2};
  int len2 = list_length(&b1);

  /* Call 3: NULL */
  int len3 = list_length((void *)0);

  return 0;
}
