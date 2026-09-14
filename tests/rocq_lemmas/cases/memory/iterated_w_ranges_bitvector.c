struct ghost_fields { signed char small; unsigned short medium; };
struct ghost_record { struct ghost_fields nested; unsigned int large; };
/*@
predicate (map<u64, u32>) GhostWords(pointer p) {
  take cells = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  return cells;
}
lemma bv_w_range(pointer p)
  requires take before = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures take after = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  }; after == before; before[1u64] <= 4294967295u32;
lemma bv_w_pack(pointer p)
  requires take before = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures take after = GhostWords(p); after == before;
lemma bv_w_unpack(pointer p)
  requires take before = GhostWords(p);
  ensures take after = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  }; after == before; before[1u64] <= 4294967295u32;
lemma bv_w_struct_range(pointer p)
  requires take before = each (i32 i; i == 0i32) {
    W<struct ghost_record>(array_shift<struct ghost_record>(p, i))
  };
  ensures take after = each (i32 i; i == 0i32) {
    W<struct ghost_record>(array_shift<struct ghost_record>(p, i))
  }; after == before;
    before[-1i32].nested.small >= -128i8;
    before[-1i32].nested.small <= 127i8;
    before[-1i32].nested.medium <= 65535u16;
    before[-1i32].large <= 4294967295u32;
@*/
