[[cerb::byte]] typedef unsigned char byte;

/* Byte ownership gives no alignment for an eight-byte object. */
void missing_alignment(char *p)
/*@
requires
    take bytes = each (integer i; 0 <= i && i < sizeof<unsigned long long>) {
        W<byte>(array_shift<byte>(p, i))
    };
ensures take value = W<unsigned long long>(p);
@*/
{
    /*@ from_bytes W<unsigned long long>(p); @*/
}
