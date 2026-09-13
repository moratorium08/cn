int integer_ctz(unsigned int x)
/*@ requires x == 8;
    ensures return == 3;
@*/
{
  return __builtin_ctz(x);
}

int integer_ffs(int x)
/*@ requires x == 8;
    ensures return == 4;
@*/
{
  return __builtin_ffs(x);
}
