/* Adversarial: the integer iarg is a simple expression over an argument.

   The right precondition is
     take L = SizedList(p, n + 1i32);
   but iarg choices do not include arithmetic terms such as n + 1. */

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

int length_plus_one_case(struct node *p, int n)
/*@ requires true;
    ensures true; @*/
{
  int count = 0;
  struct node *cur = p;
  while (cur != (void *)0) {
    count = count + 1;
    cur = cur->next;
  }
  return count - n;
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};
  return length_plus_one_case(&n1, 2) - 1;
}
