#include "./headers.h" 

/*@
lemma each_lemma (pointer p, integer n)
  requires 
    take v = Owned<int>(p);
    take A = each(integer j; 1 <= j && j < n) 
              { Owned<int>(array_shift<int>(p,j)) };
    1 <= n; 
    n <= MAXi32();
  ensures 
    take A_post = each(integer j; 0 <= j && j < n) 
              { Owned<int>(array_shift<int>(p,j)) };
    A_post[0] == v;
@*/

/*@
lemma each_concrete (pointer p)
  requires 
    take A = each(integer j; 0 <= j && j < 2) { 
                        Owned<int>(array_shift<int>(p,j)) };
        take v = Owned<int>(array_shift<int>(p,2));
  ensures 
    take A_post = each(integer j; 0 <= j && j < 3) { 
                        Owned<int>(array_shift<int>(p,j)) };
@*/

/*@
lemma each_concrete2 (pointer p)
  requires 
      take v = Owned<int>(p);
      take A = each(integer j; 0 <= j && j < 2) { 
                        Owned<int>(array_shift<int>(array_shift<int>(p,1),j)) };
        
  ensures 
    take A_post = each(integer j; 0 <= j && j < 3) { 
    Owned<int>(array_shift<int>(p,j)) };
@*/
