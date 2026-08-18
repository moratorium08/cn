unsigned long add_without_wrap(unsigned long x, unsigned long y)
/*@
requires
  x <= 100;
  y <= 100;
ensures
  return == x + y;
@*/
{
  return x + y;
}

unsigned int add_with_wrap(unsigned int x)
/*@
requires
  x == 4294967295;
ensures
  return == 0;
@*/
{
  return x + 1;
}

unsigned int add_with_possible_wrap(unsigned int x)
/*@
ensures
  return == (x == 4294967295 ? 0 : x + 1);
@*/
{
  return x + 1;
}

unsigned int narrow_without_wrap(unsigned long x)
/*@
requires
  x <= 100;
ensures
  return == x;
@*/
{
  return (unsigned int)x;
}

unsigned int narrow_with_wrap(unsigned long x)
/*@
requires
  x == 4294967296;
ensures
  return == 0;
@*/
{
  return (unsigned int)x;
}
