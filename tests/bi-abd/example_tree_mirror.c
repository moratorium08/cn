/* Example: recursively mirror a binary tree in place (mutation +
   recursion).  Every node's left/right fields are written.  Expected:
   Tree(t) for pre and post. */

#include <stddef.h>

struct tnode {
  int val;
  struct tnode *left;
  struct tnode *right;
};

/*@
predicate [rec] (i32) Tree(pointer p) {
  if (is_null(p)) {
    return 0i32;
  } else {
    take N = RW<struct tnode>(p);
    take L = Tree(N.left);
    take R = Tree(N.right);
    return 1i32;
  }
}
@*/

void tree_mirror(struct tnode *t)
/*@ requires true;
    ensures true; @*/
{
  if (t == (void *)0) {
    return;
  }
  struct tnode *tmp = t->left;
  t->left = t->right;
  t->right = tmp;
  tree_mirror(t->left);
  tree_mirror(t->right);
}

int main(void)
/*@ trusted; @*/
{
  struct tnode l = {.val = 1, .left = (void *)0, .right = (void *)0};
  struct tnode r = {.val = 3, .left = (void *)0, .right = (void *)0};
  struct tnode root = {.val = 2, .left = &l, .right = &r};
  tree_mirror(&root);
  return (root.left == &r && root.right == &l) ? 0 : 1;
}
