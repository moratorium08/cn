/* Adversarial: a single array cell at an argument index.

   The right precondition is
     take X = RW<int>(array_shift<int>(a, i));
   but candidate enumeration only owns the base pointer a, not shifted
   locations whose offset is another argument. */

int load_at(int *a, int i)
/*@ requires true;
    ensures true; @*/
{
  return a[i];
}

int main(void)
/*@ trusted; @*/
{
  int xs[4] = { 1, 2, 42, 4 };
  return load_at(xs, 2) - 42;
}
