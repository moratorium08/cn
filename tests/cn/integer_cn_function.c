/*@ function (integer) add_one_cn(integer x) @*/

static int add_one(int x)
/*@ cn_function add_one_cn;
    requires x < 2147483647;
    ensures return == add_one_cn(x);
@*/
{
  return x + 1;
}

/*@ function (integer) constant_shift_cn() @*/

static unsigned long constant_shift(void)
/*@ cn_function constant_shift_cn;
    ensures return == constant_shift_cn();
@*/
{
  return 1UL << 12;
}

int use_integer_cn_functions(void)
/*@ requires constant_shift_cn() == 4096;
    ensures return == 4098;
@*/
{
  return add_one(1) + (int)constant_shift();
}
