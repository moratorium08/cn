/* HARD example (currently unsupported): list segment with a boundary that
   no in-scope term denotes.

   walk3 touches only the first three nodes; the rest of the list lives in
   global storage the function never visits.  The right spec is
     take S = IntListSeg(p, q);
   where q is the address of the fourth node — but q is not the value of
   any argument, so boundary-iarg enumeration (args + NULL) cannot produce
   it, and deriving it from the observed stopping point is PLAN.md Step 4
   territory.  IntList(p) and IntListSeg(p, NULL) unfold past the touched
   prefix into cells missing from the heap snapshot (bottom), IntListSeg(p, p)
   is empty, and depth-2 RW chains reach only two nodes: everything fails. */

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

predicate [rec] (datatype seq) IntListSeg(pointer p, pointer q) {
  if (ptr_eq(p, q)) {
    return Seq_Nil{};
  } else {
    take H = RW<struct node>(p);
    take tl = IntListSeg(H.next, q);
    return (Seq_Cons { head: H.val, tail: tl });
  }
}
@*/

static struct node far_tail2;
static struct node far_tail1;

int walk3(struct node *p)
/*@ requires true;
    ensures true; @*/
{
  return p->val + p->next->val + p->next->next->val;
}

int main(void)
/*@ trusted; @*/
{
  far_tail2.val = 50;
  far_tail2.next = (void *)0;
  far_tail1.val = 40;
  far_tail1.next = &far_tail2;
  struct node n3 = {.val = 3, .next = &far_tail1};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  return walk3(&n1) - 6;
}
