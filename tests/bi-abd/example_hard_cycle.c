/* HARD example (currently unsupported): a cyclic singly linked list.

   The take semantics handles the cycle gracefully — unfolding IntList(p)
   on the cyclic heap re-claims an already-consumed cell and fails (bottom)
   instead of diverging; the same kills chains whose leaf wraps around to
   the prefix node.  But expressing the cycle needs a segment rooted one
   step in and closed at the argument,
     take H = RW<struct node>(p); take S = IntListSeg(H.next, p);
   and no segment predicate is defined here, so the three-node cycle has no
   covering candidate.  (This is a *predicate-vocabulary* failure, not a
   machinery one: adding the standard IntListSeg definition to this file
   makes the machinery infer exactly
     take p_W = RW<struct node>(p); take _ = IntListSeg(p_W.next, p);
   — the depth-2 chain with an argument boundary iarg.) */

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

int cycle_length(struct node *p)
/*@ requires !is_null(p);
    ensures true; @*/
{
  int n = 1;
  struct node *cur = p->next;
  while (cur != p) {
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
  n3.next = &n1; /* close the cycle */
  return cycle_length(&n1) - 3;
}
