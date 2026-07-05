/* HARD example (currently unsupported): array initialisation.

   The anti-frame is the contiguous block a[0..n) — 4n bytes.  Expressing it
   needs an iterated resource,
     each (u64 i; 0u64 <= i && i < (u64)n) { RW<int>(array_shift<int>(a, i)) },
   with the bound tied to the integer argument n across runs.  The candidate
   class has no `each` qualifiers (PLAN.md Step 5), so both phases fail. */

void array_fill(int *a, int n)
/*@ requires true;
    ensures true; @*/
{
  int i = 0;
  while (i < n) {
    a[i] = 0;
    i = i + 1;
  }
}

int main(void)
/*@ trusted; @*/
{
  int buf[8];
  array_fill(buf, 8);
  return buf[0];
}
