static unsigned int shift_inner_pure(unsigned int x)
/*@
requires
  x <= 1000;
  shift_left(x, 12) == x * 4096;
ensures
  return == x * 4096;
@*/
{
  return x << 12;
}

unsigned int shift_from_forall_obligation(unsigned int x)
/*@
requires
  x <= 1000;
  each (integer n; 0 <= n && n <= 1000) {
    shift_left(n, 12) == n * 4096
  };
ensures
  return == x * 4096;
@*/
{
  /*@ instantiate x; @*/
  return shift_inner_pure(x);
}
