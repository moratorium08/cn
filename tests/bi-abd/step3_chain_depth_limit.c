/* Step 3 failure case: ownership nested deeper than the enumerated chain
   depth (2).  Reaching the list requires
     take O = RW<struct outer>(o); take I = RW<struct inner>(O.in);
     take L = IntList(I.head);
   which is a depth-3 chain; the expected honest result is
   "inference failed", never a type-incoherent suggestion. */

#include <stddef.h>

struct node {
  int val;
  struct node *next;
};

struct inner {
  struct node *head;
};

struct outer {
  struct inner *in;
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

int deep_length(struct outer *o)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct node *cur = o->in->head;
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
  struct inner i = {.head = &n1};
  struct outer o = {.in = &i};
  return deep_length(&o) - 2;
}
