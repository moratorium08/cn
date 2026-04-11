/* Example 1: No ADTs -- simple struct ownership */

struct pair {
  int x;
  int y;
};

void swap(struct pair *p)
/*@ requires take P = RW<struct pair>(p);
    ensures take P2 = RW<struct pair>(p);
            P2.x == P.y;
            P2.y == P.x; @*/
{
  int tmp = p->x;
  p->x = p->y;
  p->y = tmp;
}

int sum(struct pair *p)
/*@ requires take P = RW<struct pair>(p);
    ensures take P2 = RW<struct pair>(p);
            P == P2;
            return == P.x + P.y; @*/
{
  return p->x + p->y;
}

int main(void)
/*@ trusted; @*/
{
  struct pair p = {.x = 10, .y = 20};
  swap(&p);
  int s = sum(&p);
  return 0;
}
