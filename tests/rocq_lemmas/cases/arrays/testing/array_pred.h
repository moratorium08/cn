/*@
predicate [rec] (datatype List) Array (pointer p, integer n) {
  if (n == 0) {
    return Nil{};
  } else {
    take V = Owned<int>(p);
    take VS = Array((array_shift<unsigned int>(p,1)), n-1);
    return (Cons { Head: V, Tail: VS });
  }
}
@*/