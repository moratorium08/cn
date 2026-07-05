/* HARD example (currently unsupported): a constructor allocating fresh
   memory and returning it.

   Two independent gaps meet here:
   - malloc is not tracked, so the fresh cell's writes land in the
     anti-frame although the closed form excludes in-span allocations
     (A* = (T \ N) \ Own) — and no argument-anchored candidate can cover
     them anyway (the function has no pointer arguments);
   - the postcondition needs  take R = RW<struct node>(return);  but
     [return] is recorded, not yet used as a candidate anchor. */

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

extern void *malloc(unsigned long size);

struct node *mk_node(int v)
/*@ requires true;
    ensures true; @*/
{
  struct node *n = malloc(sizeof(struct node));
  n->val = v;
  n->next = (void *)0;
  return n;
}

int main(void)
/*@ trusted; @*/
{
  struct node *n = mk_node(7);
  return (n != (void *)0 && n->val == 7) ? 0 : 1;
}
