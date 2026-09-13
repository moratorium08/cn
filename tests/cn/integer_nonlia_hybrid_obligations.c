unsigned long shift_constant_revealed(void)
/*@ ensures return == 4096; @*/
{
  /*@ instantiate reveal_bv, shift_left(1, 12); @*/
  return 1UL << 12;
}

unsigned int multiply_small_exact(unsigned int x, unsigned int y)
/*@ requires x <= 10;
             y <= 10;
    ensures return <= 100;
@*/
{
  return x * y;
}

unsigned int divide_positive_exact(unsigned int x, unsigned int y)
/*@ requires 0 < y;
    ensures return <= x;
@*/
{
  return x / y;
}

unsigned int bitwise_constant_revealed(void)
/*@ ensures return == 4; @*/
{
  /*@ instantiate reveal_bv, 6 & 5; @*/
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

unsigned int xor_itself_revealed(unsigned int x)
/*@ ensures return == 0; @*/
{
  /*@ instantiate reveal_bv, x ^ x; @*/
  return x ^ x;
}

unsigned int remainder_bound_exact(unsigned int x, unsigned int y)
/*@ requires 0 < y;
    ensures return < y;
@*/
{
  return x % y;
}

int signed_bitwise_revealed(void)
/*@ ensures return == -1; @*/
{
  /*@ instantiate reveal_bv, -1 ^ 0; @*/
  return -1 ^ 0;
}

unsigned long large_unsigned_or_revealed(void)
/*@ ensures return == 9223372036854775808; @*/
{
  /*@ instantiate reveal_bv, 0x8000000000000000 | 0; @*/
  return 0x8000000000000000UL | 0UL;
}
