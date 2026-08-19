unsigned int hybrid_xor_itself(unsigned int x)
/*@ ensures return == 0; @*/
{
  /*@ instantiate reveal_bv, x ^ x; @*/
  return x ^ x;
}

unsigned long hybrid_shift_constant(void)
/*@ ensures return == 4096; @*/
{
  /*@ instantiate reveal_bv, shift_left(1, 12); @*/
  return 1UL << 12;
}
