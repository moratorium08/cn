/* Example: binary search tree lookup — touches only one root-to-node path,
   not the whole tree.  Tree(t) over-approximates the touched path, which
   the sandwich permits; every activation (including NULL children) must
   accept it.  Expected: Tree(t) for pre and post. */

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

int bst_contains(struct tnode *t, int key)
/*@ requires true;
    ensures true; @*/
{
  if (t == (void *)0) {
    return 0;
  }
  if (t->val == key) {
    return 1;
  }
  if (key < t->val) {
    return bst_contains(t->left, key);
  }
  return bst_contains(t->right, key);
}

int main(void)
/*@ trusted; @*/
{
  struct tnode l = {.val = 2, .left = (void *)0, .right = (void *)0};
  struct tnode r = {.val = 7, .left = (void *)0, .right = (void *)0};
  struct tnode root = {.val = 5, .left = &l, .right = &r};
  int hit = bst_contains(&root, 7);
  int miss = bst_contains(&root, 3);
  return hit - 1 + miss;
}
