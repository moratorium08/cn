/* Example: a stack as a wrapper struct holding the top-of-list pointer.
   Reaching the nodes requires a qualifier chain through the wrapper:
   take W = RW<struct stack>(s); take _ = IntList(W.top).
   Expected: exactly that chain, pre and post. */

#include <stddef.h>

struct node {
  int val;
  struct node *next;
};

struct stack {
  struct node *top;
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

int stack_size(struct stack *s)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = s->top;
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
  struct stack s = {.top = &n1};
  return stack_size(&s) - 2;
}
