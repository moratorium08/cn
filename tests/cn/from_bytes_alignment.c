[[cerb::byte]] typedef unsigned char byte;

void aligned_from_bytes(unsigned long long *p)
/*@
requires
    good<unsigned long long *>(p);
    take bytes = each (integer i; 0 <= i && i < sizeof<unsigned long long>) {
        W<byte>(array_shift<byte>(p, i))
    };
ensures take value = W<unsigned long long>(p);
@*/
{
    /*@ from_bytes W<unsigned long long>(p); @*/
}

void aligned_roundtrip(unsigned long long *p)
/*@ requires take before = RW<unsigned long long>(p);
    ensures take after = RW<unsigned long long>(p); after == before; @*/
{
    /*@ to_bytes RW<unsigned long long>(p); @*/
    /*@ from_bytes RW<unsigned long long>(p); @*/
}

void aligned_join(unsigned *a, unsigned *b)
/*@
requires
    good<unsigned long long *>(a);
    !is_null(a);
    take x = RW<unsigned>(a);
    take y = RW<unsigned>(b);
    ptr_eq(array_shift<unsigned>(a, 1), b);
ensures take value = RW<unsigned long long>(a);
    value == x + y * 4294967296;
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
