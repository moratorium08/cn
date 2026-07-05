/* Example: rotate the contents of three int cells (a classic multi-cell
   scalar algorithm).  Expected: three disjoint RW<signed int> qualifiers,
   pre and post. */

int rotate3(int *a, int *b, int *c)
/*@ requires true;
    ensures true; @*/
{
  int tmp = *a;
  *a = *b;
  *b = *c;
  *c = tmp;
  return *a;
}

int main(void)
/*@ trusted; @*/
{
  int x = 1;
  int y = 2;
  int z = 3;
  rotate3(&x, &y, &z);
  return (x == 2 && y == 3 && z == 1) ? 0 : 1;
}
