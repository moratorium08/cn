/* Step 0 regression: interval propagation (paper: Definition acq and
   Remark "Why interval propagation, not wholesale propagation").

   caller() owns *p through its own precondition and lends it to the
   unspecified read_cell().  The abduction interval for the access is
   (owner, accessor] = only read_cell, so:
   - read_cell gets  requires/ensures RW<signed int>(p)   (A* = L* = {p})
   - caller   gets   nothing                              (A* = L* = {})
   Wholesale propagation (the rejected design) would also suggest
   RW<signed int>(p) for caller's requires — double ownership with its
   own spec. */

int read_cell(int *p)
/*@ requires true;
    ensures true; @*/
{
  return *p;
}

int caller(int *p)
/*@ requires take X = RW<int>(p);
    ensures take X2 = RW<int>(p); @*/
{
  return read_cell(p);
}

int main(void)
/*@ trusted; @*/
{
  int x = 42;
  return caller(&x) - 42;
}
