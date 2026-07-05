/* Example: doubly linked list traversed forward.  The predicate owns each
   node (including its untouched prev field); its footprint
   over-approximates the touched next fields, which the sandwich permits.
   Expected: DList(p) pre and post. */

#include <stddef.h>

struct dnode {
  int val;
  struct dnode *next;
  struct dnode *prev;
};

/*@
predicate [rec] (i32) DList(pointer p) {
  if (is_null(p)) {
    return 0i32;
  } else {
    take N = RW<struct dnode>(p);
    take T = DList(N.next);
    return 1i32;
  }
}
@*/

int dlist_length(struct dnode *p)
/*@ requires true;
    ensures true; @*/
{
  int n = 0;
  struct dnode *cur = p;
  while (cur != (void *)0) {
    n = n + 1;
    cur = cur->next;
  }
  return n;
}

int main(void)
/*@ trusted; @*/
{
  struct dnode n3 = {.val = 3, .next = (void *)0, .prev = (void *)0};
  struct dnode n2 = {.val = 2, .next = &n3, .prev = (void *)0};
  struct dnode n1 = {.val = 1, .next = &n2, .prev = (void *)0};
  n3.prev = &n2;
  n2.prev = &n1;
  return dlist_length(&n1) - 3;
}
