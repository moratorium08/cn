/* Example: compute both min and max in one traversal — a typical
   accumulate-over-list algorithm reading every val field.  Expected:
   IntList(p) pre and post (results returned through out-pointers, which
   need their own RW cells). */

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

void list_min_max(struct node *p, int *out_min, int *out_max)
/*@ requires !is_null(p);
    ensures true; @*/
{
  int mn = p->val;
  int mx = p->val;
  struct node *cur = p->next;
  while (cur != (void *)0) {
    if (cur->val < mn) {
      mn = cur->val;
    }
    if (cur->val > mx) {
      mx = cur->val;
    }
    cur = cur->next;
  }
  *out_min = mn;
  *out_max = mx;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 7, .next = (void *)0};
  struct node n2 = {.val = 1, .next = &n3};
  struct node n1 = {.val = 4, .next = &n2};
  int mn = 0;
  int mx = 0;
  list_min_max(&n1, &mn, &mx);
  return (mn == 1 && mx == 7) ? 0 : 1;
}
