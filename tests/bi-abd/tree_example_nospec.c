/* Example 3: Binary tree (no specifications) */

#include <stddef.h>

struct tree_node {
  int v;
  struct tree_node *left;
  struct tree_node *right;
};

/*@
predicate [rec] {integer size} Tree(pointer p) {
  if (is_null(p)) {
    return { size: 0 };
  } else {
    take point = RW<struct tree_node>(p);
    take left = Tree(point.left);
    take right = Tree(point.right);
    return { size: left.size + right.size + 1 };
  }
}
@*/

struct tree_node *rev_tree(struct tree_node *t)
/*@ requires true;
    ensures true; @*/
{
  if (t == (void *)0) {
    return t;
  }

  struct tree_node *tmp = rev_tree(t->left);
  t->left = rev_tree(t->right);
  t->right = tmp;

  return t;
}

int main(void)
/*@ trusted; @*/
{
  struct tree_node l = {.v = 1, .left = (void *)0, .right = (void *)0};
  struct tree_node r2 = {.v = 4, .left = (void *)0, .right = (void *)0};
  struct tree_node r = {.v = 3, .left = (void *)0, .right = &r2};
  struct tree_node root = {.v = 2, .left = &l, .right = &r};

  struct tree_node *reversed = rev_tree(&root);
  return 0;
}
