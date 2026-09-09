#include "./headers.h" 

void do_nothing (struct point *p) 
/*@ requires 
    take P = Owned<struct point>(p);
  ensures 
    take P_post = Owned<struct point>(p);
    P == P_post;
@*/
{
  /*@ apply struct_lemma (p); @*/
  *p;
}

int read (int *p)
/*@ requires 
      take P = Owned<int>(p);
      P == 0;
  ensures 
      take Q = Owned<int>(p);
      Q == 0;
@*/
{
  /*@ apply resource_lemma (p); @*/
  return *p;
}