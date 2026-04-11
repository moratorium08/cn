/* Expected: IntListSeg(xs, (void *)0) or equivalent null boundary.
   Current baseline instantiation only draws predicate arguments from in-scope
   variables, so it cannot synthesize the needed NULL constant and instead
   suggests the nonsense segment IntListSeg(xs, xs). */

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

predicate [rec] (datatype seq) IntListSeg(pointer start, pointer stop) {
  if (start == stop) {
    return Seq_Nil{};
  } else {
    take H = RW<struct node>(start);
    take T = IntListSeg(H.next, stop);
    return Seq_Cons { head: H.val, tail: T };
  }
}
@*/

int list_length(struct node *xs)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = xs;
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
  return list_length(&n1);
}
