/* HARD example (currently unsupported): a chain of plain pointers.

   **pp needs the dependent spec
     take P = RW<int*>(pp); take _ = RW<int>(P);
   i.e. a chain whose prefix pointee is a *scalar pointer*, not a struct.
   Chain enumeration currently only walks pointer-typed *fields of struct
   pointees*, so the inner cell is unreachable and the cover fails. */

int deref2(int **pp)
/*@ requires true;
    ensures true; @*/
{
  return **pp;
}

int main(void)
/*@ trusted; @*/
{
  int x = 42;
  int *q = &x;
  return deref2(&q) - 42;
}
