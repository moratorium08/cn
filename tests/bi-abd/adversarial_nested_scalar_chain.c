/* Adversarial: a depth-3 selector chain ending in a scalar cell.

   The compact spec is
     take O = RW<struct outer>(o);
     take I = RW<struct inner>(O.in);
     take X = RW<int>(I.p);
   but current chain enumeration stops after one field selection. */

struct inner {
  int *p;
};

struct outer {
  struct inner *in;
};

int nested_load(struct outer *o)
/*@ requires true;
    ensures true; @*/
{
  return *o->in->p;
}

int main(void)
/*@ trusted; @*/
{
  int x = 42;
  struct inner i = {.p = &x};
  struct outer o = {.in = &i};
  return nested_load(&o) - 42;
}
