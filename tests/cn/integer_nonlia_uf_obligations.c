unsigned long shift_constant_assumed(void)
/*@ requires shift_left(1, 12) == 4096;
    ensures return == 4096;
@*/
{
  return 1UL << 12;
}

unsigned int multiply_small_assumed(unsigned int x, unsigned int y)
/*@ requires 0 <= x * y;
             x * y <= 100;
    ensures return <= 100;
@*/
{
  return x * y;
}

unsigned int divide_positive_assumed(unsigned int x, unsigned int y)
/*@ requires 0 < y;
             0 <= x / y;
             x / y <= x;
    ensures return <= x;
@*/
{
  return x / y;
}

unsigned int bitwise_constant_assumed(void)
/*@ requires 6 & 5 == 4;
    ensures return == 4;
@*/
{
  return 6U & 5U;
}

unsigned int shift_variable_assumed(unsigned int x, unsigned int y)
/*@ requires x <= 1024;
             y <= 10;
             0 <= shift_left(x, y);
             shift_left(x, y) <= 4294967295;
             x <= shift_left(x, y);
    ensures x <= return;
@*/
{
  return x << y;
}

unsigned int xor_itself_assumed(unsigned int x)
/*@ requires x ^ x == 0;
    ensures return == 0;
@*/
{
  return x ^ x;
}

unsigned int remainder_bound_assumed(unsigned int x, unsigned int y)
/*@ requires 0 < y;
             0 <= x / y;
             x / y <= 4294967295;
             0 <= x % y;
             x % y < y;
    ensures return < y;
@*/
{
  return x % y;
}
