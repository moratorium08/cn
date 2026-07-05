/* Adversarial: the useful predicate needs a derived integer argument.

   The right precondition is
     take L = SizedList(p, 3i32);
   but 3 is not an in-scope argument and the enumerator only tries small
   constants 0, 1, and -1 for integer iargs. */

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

predicate [rec] (datatype seq) SizedList(pointer p, i32 n) {
  if (n == 0i32) {
    assert (is_null(p));
    return Seq_Nil{};
  } else {
    assert (0i32 < n);
    take H = RW<struct node>(p);
    take T = SizedList(H.next, n - 1i32);
    return Seq_Cons { head: H.val, tail: T };
  }
}
@*/

int list_sum3(struct node *p)
/*@ requires true;
    ensures true; @*/
{
  int sum = 0;
  struct node *cur = p;
  while (cur != (void *)0) {
    sum = sum + cur->val;
    cur = cur->next;
  }
  return sum;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  return list_sum3(&n1) - 6;
}
