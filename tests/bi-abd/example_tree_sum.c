/* Example: recursive sum over a binary tree.  Recursion means one data
   point per activation (subtrees and NULLs), exercising the interval rule
   and per-activation leak release interprocedurally.  Expected: Tree(t)
   for both pre and post. */

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

int tree_sum(struct tnode *t)
/*@ requires true;
    ensures true; @*/
{
  if (t == (void *)0) {
    return 0;
  }
  return t->val + tree_sum(t->left) + tree_sum(t->right);
}

int main(void)
/*@ trusted; @*/
{
  struct tnode ll = {.val = 1, .left = (void *)0, .right = (void *)0};
  struct tnode lr = {.val = 3, .left = (void *)0, .right = (void *)0};
  struct tnode l = {.val = 2, .left = &ll, .right = &lr};
  struct tnode r = {.val = 5, .left = (void *)0, .right = (void *)0};
  struct tnode root = {.val = 4, .left = &l, .right = &r};
  return tree_sum(&root) - 15;
}
