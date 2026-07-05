/* Adversarial: the only natural predicate root is a global variable.

   The right precondition is
     take L = IntList(head);
   but current enumeration only roots candidates at function arguments. */

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
    take T = IntList(H.next);
    return Seq_Cons { head: H.val, tail: T };
  }
}
@*/

static struct node *head;

int global_length(void)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = head;
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
  head = &n1;
  return global_length() - 2;
}
