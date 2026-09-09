/*@
lemma each_singleton (pointer p)
  requires
    take V = RW<int>(p);
  ensures
    take A = each(i32 i; i == 0i32) {
      RW<int>(array_shift<int>(p, i))
    };
    A[0i32] == V;
@*/

/*@
lemma each_merge_two (pointer p)
  requires
    take A0 = each(i32 i; i == 0i32) {
      RW<int>(array_shift<int>(p, i))
    };
    take A1 = each(i32 i; i == 1i32) {
      RW<int>(array_shift<int>(p, i))
    };
  ensures
    take A = each(i32 i; i == 0i32 || i == 1i32) {
      RW<int>(array_shift<int>(p, i))
    };
    A[0i32] == A0[0i32];
    A[1i32] == A1[1i32];
@*/

/*@
lemma each_uninit_identity (pointer p)
  requires
    take A = each(i32 i; i == 0i32 || i == 2i32) {
      W<char>(array_shift<char>(p, i))
    };
  ensures
    take A_post = each(i32 i; i == 0i32 || i == 2i32) {
      W<char>(array_shift<char>(p, i))
    };
@*/

/*@
lemma each_named_identity (pointer p)
  requires
    take A = each(i32 i; i == 0i32 || i == 2i32) {
      Cell(array_shift<int>(p, i))
    };
  ensures
    take A_post = each(i32 i; i == 0i32 || i == 2i32) {
      Cell(array_shift<int>(p, i))
    };
@*/
/*@
predicate (i32) Cell (pointer p) {
  take V = RW<int>(p);
  return V;
}
@*/
