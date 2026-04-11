/* Expected: PairCell(p).
   Current implementation only enumerates recursive predicates, so it falls
   back to the raw Owned suggestion even when a user-defined wrapper predicate
   exactly matches the footprint. */

struct pair {
  int x;
  int y;
};

/*@
predicate (void) PairCell(pointer p) {
  take H = RW<struct pair>(p);
  return;
}
@*/

int sum_pair(struct pair *p)
/*@ requires true;
    ensures true; @*/
{
  return p->x + p->y;
}

int main(void)
/*@ trusted; @*/
{
  struct pair p = {.x = 1, .y = 2};
  return sum_pair(&p);
}
