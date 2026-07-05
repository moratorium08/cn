/* Example: in-place list reversal.  The precondition side should infer
   IntList(p).  The postcondition side cannot currently be expressed: the
   reversed list is anchored at the *returned* pointer (the old tail), and
   [return] is not yet a candidate anchor — the expected honest outcome is
   "inference failed". */

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

struct node *list_reverse(struct node *p)
/*@ requires true;
    ensures true; @*/
{
  struct node *prev = (void *)0;
  struct node *cur = p;
  while (cur != (void *)0) {
    struct node *nxt = cur->next;
    cur->next = prev;
    prev = cur;
    cur = nxt;
  }
  return prev;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  struct node *r = list_reverse(&n1);
  return (r == &n3 && n3.next == &n2 && n2.next == &n1) ? 0 : 1;
}
