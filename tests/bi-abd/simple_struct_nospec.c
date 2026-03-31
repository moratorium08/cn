/* Example 1: No ADTs -- simple struct ownership (no specifications) */

struct pair {
  int x;
  int y;
};

void swap(struct pair *p)
/*@ requires true;
    ensures true; @*/
{
  int tmp = p->x;
  p->x = p->y;
  p->y = tmp;
}

int sum(struct pair *p)
/*@ requires true;
    ensures true; @*/
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
