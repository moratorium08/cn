/* Example: destructive append — walk list a to its last node and link
   list b onto it.  Interesting on both sides: the precondition should only
   demand a's nodes (b is passed by value, never dereferenced), and the
   postcondition leak set is only a's cells, while any predicate rooted at
   [a] evaluated on the exit heap traverses into b — exercising how the
   method handles over-approximation of the frame. */

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

void list_append(struct node *a, struct node *b)
/*@ requires !is_null(a);
    ensures true; @*/
{
  struct node *cur = a;
  while (cur->next != (void *)0) {
    cur = cur->next;
  }
  cur->next = b;
}

int main(void)
/*@ trusted; @*/
{
  struct node b2 = {.val = 4, .next = (void *)0};
  struct node b1 = {.val = 3, .next = &b2};
  struct node a2 = {.val = 2, .next = (void *)0};
  struct node a1 = {.val = 1, .next = &a2};
  list_append(&a1, &b1);
  return (a2.next == &b1) ? 0 : 1;
}
