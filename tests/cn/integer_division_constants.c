/* Constant folding must agree with symbolic SMT integer division. C signed
 * division still truncates towards zero, through check.ml's separate rule. */
void integer_division_constants(int x)
/*@ requires x == -7; @*/
{
    /*@ assert ((-7) / 2 == -4); @*/
    /*@ assert ((-7) / (-2) == 4); @*/
    /*@ assert (7 / (-2) == -3); @*/
    /*@ assert (7 / 2 == 3); @*/
    /*@ assert ((-8) / 2 == -4); @*/
    /*@ assert (0 / (-2) == 0); @*/
    /*@ assert (x / 2 == (-7) / 2); @*/
    /*@ assert (x / (-2) == (-7) / (-2)); @*/
    int q = x / 2;
    /*@ assert (q == -3); @*/
}
