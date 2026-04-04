/* Expected: RW<struct alpha>(p).
   Current implementation generates Owned candidates for every struct type in
   the file, so it can suggest the wrong pointee type entirely. */

struct alpha {
  int x;
  int y;
};

struct beta {
  int x;
  int y;
};

int sum_alpha(struct alpha *p)
/*@ requires true;
    ensures true; @*/
{
  return p->x + p->y;
}

int main(void)
/*@ trusted; @*/
{
  struct alpha a = {.x = 10, .y = 20};
  return sum_alpha(&a);
}
