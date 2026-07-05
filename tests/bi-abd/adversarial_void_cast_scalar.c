/* Adversarial: the argument type is void*, but the access type is int*.

   A valid precondition can say
     take X = RW<int>(p);
   but current Owned enumeration skips void* arguments because their pointee
   type is unknown from the function signature alone. */

int load_void(void *p)
/*@ requires true;
    ensures true; @*/
{
  int *q = p;
  return *q;
}

int main(void)
/*@ trusted; @*/
{
  int x = 42;
  return load_void(&x) - 42;
}
