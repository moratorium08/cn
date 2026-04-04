/* Expected: IntListSeg(xs, end).
   Current implementation reuses predicate-formal iarg symbols directly, so it
   suggests IntListSeg(xs, stop), where stop is not even in the function scope. */

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

int seg_length(struct node *xs, struct node *end)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = xs;
  while (cur != end) {
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

  return seg_length(&n1, &n3);
}
