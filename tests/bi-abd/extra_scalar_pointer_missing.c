/* Expected: RW<int>(p) or equivalent scalar ownership.
   Current baseline candidate generation only produces Owned qualifiers for
   struct types, so a plain int* access is left completely uncovered. */

int load_int(int *p)
/*@ requires true;
    ensures true; @*/
{
  return *p;
}

int main(void)
/*@ trusted; @*/
{
  int x = 42;
  return load_int(&x);
}
