#include "./headers.h" 

/*@
lemma each_combine (pointer p, integer n, integer m)
  requires 
    take A1 = each(integer j; 0 <= j && j < n) { 
                        Owned<int>(array_shift<int>(p,j)) };
    take A2 = each(integer j; n <= j && j < (m + n)) { 
                        Owned<int>(array_shift<int>(p,j)) };
    n >= 0; n <= MAXi32();
    m >= 0; m <= MAXi32();
    m + n <= MAXi32();
  ensures 
    take A_post = each(integer j; 0 <= j && j < (m + n)) { 
        Owned<int>(array_shift<int>(p,j)) 
        };
@*/

