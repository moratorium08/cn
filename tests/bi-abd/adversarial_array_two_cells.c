/* Adversarial: two fixed array cells.

   The right precondition is just the two cells
     take A0 = RW<int>(a);
     take A1 = RW<int>(array_shift<int>(a, 1i32));
   but inference cannot currently enumerate array_shift-owned cells or each
   resources, so it should fail rather than suggesting only a[0]. */

int sum2(int *a)
/*@ requires true;
    ensures true; @*/
{
  return a[0] + a[1];
}

int main(void)
/*@ trusted; @*/
{
  int xs[2] = { 10, 32 };
  return sum2(xs) - 42;
}
