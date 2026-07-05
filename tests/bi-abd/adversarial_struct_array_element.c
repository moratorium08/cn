/* Adversarial: an array element whose type is a struct.

   The right precondition is
     take P = RW<struct pair>(array_shift<struct pair>(ps, 1i32));
   but shifted struct elements are not enumerated as Owned candidates. */

struct pair {
  int x;
  int y;
};

int second_pair_sum(struct pair *ps)
/*@ requires true;
    ensures true; @*/
{
  return ps[1].x + ps[1].y;
}

int main(void)
/*@ trusted; @*/
{
  struct pair ps[2] = {
    {.x = 1, .y = 2},
    {.x = 20, .y = 22}
  };
  return second_pair_sum(ps) - 42;
}
