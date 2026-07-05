/* Example: membership test with early exit.  Two calls: one finds the key
   mid-list (partial traversal), one runs off the end.  Data-relative
   inference must find one qualifier valid for both activations:
   IntList(p) (its footprint over-approximates the partially-traversed
   run, which the sandwich allows). */

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

int list_contains(struct node *p, int key)
/*@ requires true;
    ensures true; @*/
{
  struct node *cur = p;
  while (cur != (void *)0) {
    if (cur->val == key) {
      return 1;
    }
    cur = cur->next;
  }
  return 0;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 30, .next = (void *)0};
  struct node n2 = {.val = 20, .next = &n3};
  struct node n1 = {.val = 10, .next = &n2};
  int found = list_contains(&n1, 20);   /* early exit at node 2 */
  int missing = list_contains(&n1, 99); /* full traversal */
  return found - 1 + missing;
}
