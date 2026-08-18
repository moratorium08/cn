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

int signed_bitwise(void)
/*@ ensures return == -1; @*/
{
  return -1 ^ 0;
}

unsigned int unsigned_complement(void)
/*@ ensures return == 4294967295; @*/
{
  return ~0U;
}

unsigned long large_unsigned_or(void)
/*@ ensures return == 9223372036854775808; @*/
{
  return 0x8000000000000000UL | 0UL;
}

int signed_division_truncates_to_zero(int x)
/*@ requires x == -5;
    ensures return == -2;
@*/
{
  return x / 2;
}

int signed_remainder_follows_dividend(int x)
/*@ requires x == -5;
    ensures return == -1;
@*/
{
  return x % 2;
}
