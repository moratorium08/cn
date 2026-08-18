int integer_ctz_assumed(unsigned int x)
/*@ requires x == 8;
             bw_ctz(x) == 3;
    ensures return == 3;
@*/
{
  return __builtin_ctz(x);
}

int integer_ffs_assumed(int x)
/*@ requires x == 8;
             bw_ffs(x) == 4;
    ensures return == 4;
@*/
{
  return __builtin_ffs(x);
}
