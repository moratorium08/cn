unsigned long shift_constant(void)
/*@ ensures return == 4096; @*/
{
  return 1UL << 12;
}

unsigned int multiply_small(unsigned int x, unsigned int y)
/*@ requires x <= 10;
             y <= 10;
    ensures return <= 100;
@*/
{
  return x * y;
}

unsigned int divide_positive(unsigned int x, unsigned int y)
/*@ requires 0 < y;
    ensures return <= x;
@*/
{
  return x / y;
}

unsigned int bitwise_constant(void)
/*@ ensures return == 4; @*/
{
  return 6U & 5U;
}

unsigned int shift_variable(unsigned int x, unsigned int y)
/*@ requires x <= 1024;
             y <= 10;
    ensures x <= return;
@*/
{
  return x << y;
}

unsigned int xor_itself(unsigned int x)
/*@ ensures return == 0; @*/
{
  return x ^ x;
}

unsigned int remainder_bound(unsigned int x, unsigned int y)
/*@ requires 0 < y;
    ensures return < y;
@*/
{
  return x % y;
}
