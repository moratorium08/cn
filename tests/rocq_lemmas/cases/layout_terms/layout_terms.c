struct layout_probe {
  int first;
  int second;
};

/*@
lemma layout_constants ()
  requires true;
  ensures
    sizeof<struct layout_probe> == 8u64;
    offsetof(layout_probe, second) == 4u64;

lemma split_layout_probe (pointer p)
  requires
    take S = RW<struct layout_probe>(p);
  ensures
    take First = RW<int>(member_shift<struct layout_probe>(p, first));
    take Second = RW<int>(member_shift<struct layout_probe>(p, second));
    First == S.first;
    Second == S.second;
@*/

int main(void)
{
  return 0;
}
