struct stct {
  int first;
  int second;
};

/*@
lemma sizeof_offsetof_lemma ()
  requires true;
  ensures
    let x = sizeof<struct stct>;
    let y = offsetof(stct, second);
    y < x;
@*/