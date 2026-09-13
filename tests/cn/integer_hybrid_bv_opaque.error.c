unsigned int hybrid_xor_without_reveal(unsigned int x)
/*@ ensures return == 0; @*/
{
  return x ^ x;
}
