/* Example 2: Linked list with recursive predicate */

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

function [rec] (datatype seq) rev(datatype seq xs) {
  match xs {
    Seq_Nil {} => { Seq_Nil {} }
    Seq_Cons {head: h, tail: zs} => {
      snoc(rev(zs), h)
    }
  }
}

function [rec] (datatype seq) snoc(datatype seq xs, i32 y) {
  match xs {
    Seq_Nil {} => { Seq_Cons{head: y, tail: Seq_Nil{}} }
    Seq_Cons {head: x, tail: zs} => {
      Seq_Cons{head: x, tail: snoc(zs, y)}
    }
  }
}

function [rec] (datatype seq) append(datatype seq xs, datatype seq ys) {
  match xs {
    Seq_Nil {} => { ys }
    Seq_Cons {head: h, tail: zs} => {
      Seq_Cons{head: h, tail: append(zs, ys)}
    }
  }
}
@*/

int list_length(struct node *p)
/*@ requires take L = IntList(p);
    ensures take L2 = IntList(p);
            L == L2; @*/
{
  int n = 0;
  struct node *cur = p;
  while (cur != (void *)0)
  /*@ inv take Lcur = IntList(cur);
          {p} unchanged; @*/
  {
    n = n + 1;
    cur = cur->next;
  }
  return n;
}

struct node *list_rev_aux(struct node *xs, struct node *ys)
/*@ requires take L1 = IntList(xs);
             take L2 = IntList(ys);
    ensures take R = IntList(return);
            R == append(rev(L2), L1); @*/
{
  if (ys == (void *)0) {
    return xs;
  } else {
    struct node *tmp = ys->next;
    ys->next = xs;
    return list_rev_aux(ys, tmp);
  }
}

int main(void)
/*@ trusted; @*/
{
  struct node n3 = {.val = 3, .next = (void *)0};
  struct node n2 = {.val = 2, .next = &n3};
  struct node n1 = {.val = 1, .next = &n2};

  int len = list_length(&n1);
  struct node *rev = list_rev_aux((void *)0, &n1);
  return 0;
}
