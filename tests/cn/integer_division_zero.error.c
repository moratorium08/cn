/* SMT integer division by zero is unspecified, even with a zero dividend. */
void integer_division_zero(void)
{
    /*@ assert (0 / 0 == 0); @*/
}
