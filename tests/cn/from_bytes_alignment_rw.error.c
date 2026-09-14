/* Two adjacent u32s need not start at an eight-byte-aligned address. */
[[cerb::byte]] typedef unsigned char byte;
void missing_alignment(unsigned *a, unsigned *b)
/*@
requires
    !is_null(a);
    take x = RW<unsigned>(a);
    take y = RW<unsigned>(b);
    ptr_eq(array_shift<unsigned>(a, 1), b);
ensures take value = RW<unsigned long long>(a);
@*/
{
    /*@ to_bytes RW<unsigned>(a); @*/
    /*@ to_bytes RW<unsigned>(b); @*/
    /*@ focus RW<byte>, 0; @*/
    /*@ focus RW<byte>, 1; @*/
    /*@ focus RW<byte>, 2; @*/
    /*@ focus RW<byte>, 3; @*/
    /*@ focus RW<byte>, 4; @*/
    /*@ focus RW<byte>, 5; @*/
    /*@ focus RW<byte>, 6; @*/
    /*@ focus RW<byte>, 7; @*/
    /*@ from_bytes RW<unsigned long long>(a); @*/
}
