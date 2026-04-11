/* Expected: a qualifier chain rooted at b, e.g.
     take B = RW<struct list_pair>(b);
     take Xs = IntList(B.xs);
     take Ys = IntList(B.ys);
   Current implementation only enumerates qualifiers rooted at function
   arguments, not selector-based chains, so the list resources stay uncovered. */

#include <stddef.h>

struct node {
  int val;
  struct node *next;
};

struct list_pair {
  struct node *xs;
  struct node *ys;
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

int total_length(struct list_pair *b)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = b->xs;
  while (cur != (void *)0) {
    n = n + 1;
    cur = cur->next;
  }

  cur = b->ys;
  while (cur != (void *)0) {
    n = n + 1;
    cur = cur->next;
  }

  return n;
}

int main(void)
/*@ trusted; @*/
{
  struct node x2 = {.val = 2, .next = (void *)0};
  struct node x1 = {.val = 1, .next = &x2};
  struct node y1 = {.val = 3, .next = (void *)0};
  struct list_pair pair = {.xs = &x1, .ys = &y1};

  return total_length(&pair);
}
