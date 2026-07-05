/* Adversarial: a list segment boundary stored in the same wrapper.

   The partial spec already owns the wrapper and the tail from W.stop.
   The missing addition is
     take L = IntListSeg(box.start, box.stop);
   but inference does not enumerate candidates rooted at user-spec bindings
   or predicate iargs taken from earlier take-bound fields. */

#include <stddef.h>

struct node {
  int val;
  struct node *next;
};

struct span_box {
  struct node *start;
  struct node *stop;
};

/*@
datatype seq {
  Seq_Nil {},
  Seq_Cons {i32 head, datatype seq tail}
}

predicate [rec] (datatype seq) IntListSeg(pointer p, pointer q) {
  if (ptr_eq(p, q)) {
    return Seq_Nil{};
  } else {
    take H = RW<struct node>(p);
    take T = IntListSeg(H.next, q);
    return Seq_Cons { head: H.val, tail: T };
  }
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

int segment_sum(struct span_box *s)
/*@
requires
  take box = RW<struct span_box>(s);
  take Tail = IntList(box.stop);
ensures
  take box2 = RW<struct span_box>(s);
  take Tail2 = IntList(box2.stop);
@*/
{
  int sum = 0;
  struct node *cur = s->start;
  while (cur != s->stop) {
    sum = sum + cur->val;
    cur = cur->next;
  }
  return sum;
}

int main(void)
/*@ trusted; @*/
{
  struct node tail = {.val = 100, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &tail};
  struct node n1 = {.val = 1, .next = &n2};
  struct span_box s = {.start = &n1, .stop = &tail};
  return segment_sum(&s) - 3;
}
