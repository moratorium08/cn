/* Expected: RW<struct pair>(p) in both pre and post.
   Current implementation records useful missing ownership only after
   cn_abd_mark_post, so it suggests a postcondition only. */

struct pair {
  int x;
  int y;
};

int sum_pair(struct pair *p)
/*@ requires true;
    ensures true; @*/
{
  return p->x + p->y;
}

int main(void)
/*@ trusted; @*/
{
  struct pair p = {.x = 10, .y = 20};
  return sum_pair(&p);
}
