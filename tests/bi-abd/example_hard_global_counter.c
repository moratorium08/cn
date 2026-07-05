/* HARD example (currently unsupported): state reached through a global
   variable.

   tick() has no arguments; the accessed cell is the global counter, owned
   by the environment.  The right spec mentions the global
   (requires take C = RW<int>(&counter)), but globals are not candidate
   anchors yet, so there is no qualifier to cover the anti-frame. */

static int counter = 0;

void tick(void)
/*@ requires true;
    ensures true; @*/
{
  counter = counter + 1;
}

int main(void)
/*@ trusted; @*/
{
  tick();
  tick();
  return counter - 2;
}
