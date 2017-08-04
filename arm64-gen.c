/*
 *  A64 code generator for TCC
 *
 *  Copyright (c) 2014-2015 Edmund Grimley Evans
 *
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */

#如定义 TARGET_DEFS_ONLY

// Number of registers available to allocator:
#定义 NB_REGS 28 // x0-x18, x30, v0-v7

#定义 TREG_R(x) (x) // x = 0..18
#定义 TREG_R30  19
#定义 TREG_F(x) (x + 20) // x = 0..7

// Register classes sorted from more general to more precise:
#定义 RC_INT (1 << 0)
#定义 RC_FLOAT (1 << 1)
#定义 RC_R(x) (1 << (2 + (x))) // x = 0..18
#定义 RC_R30  (1 << 21)
#定义 RC_F(x) (1 << (22 + (x))) // x = 0..7

#定义 RC_IRET (RC_R(0)) // int return register class
#定义 RC_FRET (RC_F(0)) // float return register class

#定义 REG_IRET (TREG_R(0)) // int return register number
#定义 REG_FRET (TREG_F(0)) // float return register number

#定义 PTR_SIZE 8

#定义 LDOUBLE_SIZE 16
#定义 LDOUBLE_ALIGN 16

#定义 MAX_ALIGN 16

#定义 CHAR_IS_UNSIGNED

/******************************************************/
#另 /* ! TARGET_DEFS_ONLY */
/******************************************************/
#包含 "tcc.h"
#包含 <assert.h>

ST_DATA 不变 整 reg_classes[NB_REGS] = {
  RC_INT | RC_R(0),
  RC_INT | RC_R(1),
  RC_INT | RC_R(2),
  RC_INT | RC_R(3),
  RC_INT | RC_R(4),
  RC_INT | RC_R(5),
  RC_INT | RC_R(6),
  RC_INT | RC_R(7),
  RC_INT | RC_R(8),
  RC_INT | RC_R(9),
  RC_INT | RC_R(10),
  RC_INT | RC_R(11),
  RC_INT | RC_R(12),
  RC_INT | RC_R(13),
  RC_INT | RC_R(14),
  RC_INT | RC_R(15),
  RC_INT | RC_R(16),
  RC_INT | RC_R(17),
  RC_INT | RC_R(18),
  RC_R30, // not in RC_INT as we make special use of x30
  RC_FLOAT | RC_F(0),
  RC_FLOAT | RC_F(1),
  RC_FLOAT | RC_F(2),
  RC_FLOAT | RC_F(3),
  RC_FLOAT | RC_F(4),
  RC_FLOAT | RC_F(5),
  RC_FLOAT | RC_F(6),
  RC_FLOAT | RC_F(7)
};

#定义 IS_FREG(x) ((x) >= TREG_F(0))

静态 uint32_t intr(整 r)
{
    assert(TREG_R(0) <= r && r <= TREG_R30);
    返回 r < TREG_R30 ? r : 30;
}

静态 uint32_t fltr(整 r)
{
    assert(TREG_F(0) <= r && r <= TREG_F(7));
    返回 r - TREG_F(0);
}

// Add an instruction to text section:
ST_FUNC 空 o(无符 整 c)
{
    整 ind1 = ind + 4;
    如 (nocode_wanted)
        返回;
    如 (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    write32le(cur_text_section->data + ind, c);
    ind = ind1;
}

静态 整 arm64_encode_bimm64(uint64_t x)
{
    整 neg = x & 1;
    整 rep, pos, len;

    如 (neg)
        x = ~x;
    如 (!x)
        返回 -1;

    如 (x >> 2 == (x & (((uint64_t)1 << (64 - 2)) - 1)))
        rep = 2, x &= ((uint64_t)1 << 2) - 1;
    另 如 (x >> 4 == (x & (((uint64_t)1 << (64 - 4)) - 1)))
        rep = 4, x &= ((uint64_t)1 <<  4) - 1;
    另 如 (x >> 8 == (x & (((uint64_t)1 << (64 - 8)) - 1)))
        rep = 8, x &= ((uint64_t)1 <<  8) - 1;
    另 如 (x >> 16 == (x & (((uint64_t)1 << (64 - 16)) - 1)))
        rep = 16, x &= ((uint64_t)1 << 16) - 1;
    另 如 (x >> 32 == (x & (((uint64_t)1 << (64 - 32)) - 1)))
        rep = 32, x &= ((uint64_t)1 << 32) - 1;
    另
        rep = 64;

    pos = 0;
    如 (!(x & (((uint64_t)1 << 32) - 1))) x >>= 32, pos += 32;
    如 (!(x & (((uint64_t)1 << 16) - 1))) x >>= 16, pos += 16;
    如 (!(x & (((uint64_t)1 <<  8) - 1))) x >>= 8, pos += 8;
    如 (!(x & (((uint64_t)1 <<  4) - 1))) x >>= 4, pos += 4;
    如 (!(x & (((uint64_t)1 <<  2) - 1))) x >>= 2, pos += 2;
    如 (!(x & (((uint64_t)1 <<  1) - 1))) x >>= 1, pos += 1;

    len = 0;
    如 (!(~x & (((uint64_t)1 << 32) - 1))) x >>= 32, len += 32;
    如 (!(~x & (((uint64_t)1 << 16) - 1))) x >>= 16, len += 16;
    如 (!(~x & (((uint64_t)1 << 8) - 1))) x >>= 8, len += 8;
    如 (!(~x & (((uint64_t)1 << 4) - 1))) x >>= 4, len += 4;
    如 (!(~x & (((uint64_t)1 << 2) - 1))) x >>= 2, len += 2;
    如 (!(~x & (((uint64_t)1 << 1) - 1))) x >>= 1, len += 1;

    如 (x)
        返回 -1;
    如 (neg) {
        pos = (pos + len) & (rep - 1);
        len = rep - len;
    }
    返回 ((0x1000 & rep << 6) | (((rep - 1) ^ 31) << 1 & 63) |
            ((rep - pos) & (rep - 1)) << 6 | (len - 1));
}

静态 uint32_t arm64_movi(整 r, uint64_t x)
{
    uint64_t m = 0xffff;
    整 e;
    如 (!(x & ~m))
        返回 0x52800000 | r | x << 5; // movz w(r),#(x)
    如 (!(x & ~(m << 16)))
        返回 0x52a00000 | r | x >> 11; // movz w(r),#(x >> 16),lsl #16
    如 (!(x & ~(m << 32)))
        返回 0xd2c00000 | r | x >> 27; // movz x(r),#(x >> 32),lsl #32
    如 (!(x & ~(m << 48)))
        返回 0xd2e00000 | r | x >> 43; // movz x(r),#(x >> 48),lsl #48
    如 ((x & ~m) == m << 16)
        返回 (0x12800000 | r |
                (~x << 5 & 0x1fffe0)); // movn w(r),#(~x)
    如 ((x & ~(m << 16)) == m)
        返回 (0x12a00000 | r |
                (~x >> 11 & 0x1fffe0)); // movn w(r),#(~x >> 16),lsl #16
    如 (!~(x | m))
        返回 (0x92800000 | r |
                (~x << 5 & 0x1fffe0)); // movn x(r),#(~x)
    如 (!~(x | m << 16))
        返回 (0x92a00000 | r |
                (~x >> 11 & 0x1fffe0)); // movn x(r),#(~x >> 16),lsl #16
    如 (!~(x | m << 32))
        返回 (0x92c00000 | r |
                (~x >> 27 & 0x1fffe0)); // movn x(r),#(~x >> 32),lsl #32
    如 (!~(x | m << 48))
        返回 (0x92e00000 | r |
                (~x >> 43 & 0x1fffe0)); // movn x(r),#(~x >> 32),lsl #32
    如 (!(x >> 32) && (e = arm64_encode_bimm64(x | x << 32)) >= 0)
        返回 0x320003e0 | r | (uint32_t)e << 10; // movi w(r),#(x)
    如 ((e = arm64_encode_bimm64(x)) >= 0)
        返回 0xb20003e0 | r | (uint32_t)e << 10; // movi x(r),#(x)
    返回 0;
}

静态 空 arm64_movimm(整 r, uint64_t x)
{
    uint32_t i;
    如 ((i = arm64_movi(r, x)))
        o(i); // a single MOV
    另 {
        // MOVZ/MOVN and 1-3 MOVKs
        整 z = 0, m = 0;
        uint32_t mov1 = 0xd2800000; // movz
        uint64_t x1 = x;
        对于 (i = 0; i < 64; i += 16) {
            z += !(x >> i & 0xffff);
            m += !(~x >> i & 0xffff);
        }
        如 (m > z) {
            x1 = ~x;
            mov1 = 0x92800000; // movn
        }
        对于 (i = 0; i < 64; i += 16)
            如 (x1 >> i & 0xffff) {
                o(mov1 | r | (x1 >> i & 0xffff) << 5 | i << 17);
                // movz/movn x(r),#(*),lsl #(i)
                跳出;
            }
        对于 (i += 16; i < 64; i += 16)
            如 (x1 >> i & 0xffff)
                o(0xf2800000 | r | (x >> i & 0xffff) << 5 | i << 17);
                // movk x(r),#(*),lsl #(i)
    }
}

// Patch all branches in list pointed to by t to branch to a:
ST_FUNC 空 gsym_addr(整 t_, 整 a_)
{
    uint32_t t = t_;
    uint32_t a = a_;
    当 (t) {
        无符 字 *ptr = cur_text_section->data + t;
        uint32_t next = read32le(ptr);
        如 (a - t + 0x8000000 >= 0x10000000)
            tcc_error("branch out of range");
        write32le(ptr, (a - t == 4 ? 0xd503201f : // nop
                        0x14000000 | ((a - t) >> 2 & 0x3ffffff))); // b
        t = next;
    }
}

// Patch all branches in list pointed to by t to branch to current location:
ST_FUNC 空 gsym(整 t)
{
    gsym_addr(t, ind);
}

静态 整 arm64_type_size(整 t)
{
    转接 (t & VT_BTYPE) {
    事例 VT_INT: 返回 2;
    事例 VT_BYTE: 返回 0;
    事例 VT_SHORT: 返回 1;
    事例 VT_PTR: 返回 3;
    事例 VT_FUNC: 返回 3;
    事例 VT_FLOAT: 返回 2;
    事例 VT_DOUBLE: 返回 3;
    事例 VT_LDOUBLE: 返回 4;
    事例 VT_BOOL: 返回 0;
    事例 VT_LLONG: 返回 3;
    }
    assert(0);
    返回 0;
}

静态 空 arm64_spoff(整 reg, uint64_t off)
{
    uint32_t sub = off >> 63;
    如 (sub)
        off = -off;
    如 (off < 4096)
        o(0x910003e0 | sub << 30 | reg | off << 10);
        // (add|sub) x(reg),sp,#(off)
    另 {
        arm64_movimm(30, off); // use x30 for offset
        o(0x8b3e63e0 | sub << 30 | reg); // (add|sub) x(reg),sp,x30
    }
}

静态 空 arm64_ldrx(整 sg, 整 sz_, 整 dst, 整 bas, uint64_t off)
{
    uint32_t sz = sz_;
    如 (sz >= 2)
        sg = 0;
    如 (!(off & ~((uint32_t)0xfff << sz)))
        o(0x39400000 | dst | bas << 5 | off << (10 - sz) |
          (uint32_t)!!sg << 23 | sz << 30); // ldr(*) x(dst),[x(bas),#(off)]
    另 如 (off < 256 || -off <= 256)
        o(0x38400000 | dst | bas << 5 | (off & 511) << 12 |
          (uint32_t)!!sg << 23 | sz << 30); // ldur(*) x(dst),[x(bas),#(off)]
    另 {
        arm64_movimm(30, off); // use x30 for offset
        o(0x38206800 | dst | bas << 5 | (uint32_t)30 << 16 |
          (uint32_t)(!!sg + 1) << 22 | sz << 30); // ldr(*) x(dst),[x(bas),x30]
    }
}

静态 空 arm64_ldrv(整 sz_, 整 dst, 整 bas, uint64_t off)
{
    uint32_t sz = sz_;
    如 (!(off & ~((uint32_t)0xfff << sz)))
        o(0x3d400000 | dst | bas << 5 | off << (10 - sz) |
          (sz & 4) << 21 | (sz & 3) << 30); // ldr (s|d|q)(dst),[x(bas),#(off)]
    另 如 (off < 256 || -off <= 256)
        o(0x3c400000 | dst | bas << 5 | (off & 511) << 12 |
          (sz & 4) << 21 | (sz & 3) << 30); // ldur (s|d|q)(dst),[x(bas),#(off)]
    另 {
        arm64_movimm(30, off); // use x30 for offset
        o(0x3c606800 | dst | bas << 5 | (uint32_t)30 << 16 |
          sz << 30 | (sz & 4) << 21); // ldr (s|d|q)(dst),[x(bas),x30]
    }
}

静态 空 arm64_ldrs(整 reg_, 整 size)
{
    uint32_t reg = reg_;
    // Use x30 for intermediate value in some cases.
    转接 (size) {
    缺省: assert(0); 跳出;
    事例 1:
        arm64_ldrx(0, 0, reg, reg, 0);
        跳出;
    事例 2:
        arm64_ldrx(0, 1, reg, reg, 0);
        跳出;
    事例 3:
        arm64_ldrx(0, 1, 30, reg, 0);
        arm64_ldrx(0, 0, reg, reg, 2);
        o(0x2a0043c0 | reg | reg << 16); // orr x(reg),x30,x(reg),lsl #16
        跳出;
    事例 4:
        arm64_ldrx(0, 2, reg, reg, 0);
        跳出;
    事例 5:
        arm64_ldrx(0, 2, 30, reg, 0);
        arm64_ldrx(0, 0, reg, reg, 4);
        o(0xaa0083c0 | reg | reg << 16); // orr x(reg),x30,x(reg),lsl #32
        跳出;
    事例 6:
        arm64_ldrx(0, 2, 30, reg, 0);
        arm64_ldrx(0, 1, reg, reg, 4);
        o(0xaa0083c0 | reg | reg << 16); // orr x(reg),x30,x(reg),lsl #32
        跳出;
    事例 7:
        arm64_ldrx(0, 2, 30, reg, 0);
        arm64_ldrx(0, 2, reg, reg, 3);
        o(0x53087c00 | reg | reg << 5); // lsr w(reg), w(reg), #8
        o(0xaa0083c0 | reg | reg << 16); // orr x(reg),x30,x(reg),lsl #32
        跳出;
    事例 8:
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 9:
        arm64_ldrx(0, 0, reg + 1, reg, 8);
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 10:
        arm64_ldrx(0, 1, reg + 1, reg, 8);
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 11:
        arm64_ldrx(0, 2, reg + 1, reg, 7);
        o(0x53087c00 | (reg+1) | (reg+1) << 5); // lsr w(reg+1), w(reg+1), #8
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 12:
        arm64_ldrx(0, 2, reg + 1, reg, 8);
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 13:
        arm64_ldrx(0, 3, reg + 1, reg, 5);
        o(0xd358fc00 | (reg+1) | (reg+1) << 5); // lsr x(reg+1), x(reg+1), #24
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 14:
        arm64_ldrx(0, 3, reg + 1, reg, 6);
        o(0xd350fc00 | (reg+1) | (reg+1) << 5); // lsr x(reg+1), x(reg+1), #16
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 15:
        arm64_ldrx(0, 3, reg + 1, reg, 7);
        o(0xd348fc00 | (reg+1) | (reg+1) << 5); // lsr x(reg+1), x(reg+1), #8
        arm64_ldrx(0, 3, reg, reg, 0);
        跳出;
    事例 16:
        o(0xa9400000 | reg | (reg+1) << 10 | reg << 5);
        // ldp x(reg),x(reg+1),[x(reg)]
        跳出;
    }
}

静态 空 arm64_strx(整 sz_, 整 dst, 整 bas, uint64_t off)
{
    uint32_t sz = sz_;
    如 (!(off & ~((uint32_t)0xfff << sz)))
        o(0x39000000 | dst | bas << 5 | off << (10 - sz) | sz << 30);
        // str(*) x(dst),[x(bas],#(off)]
    另 如 (off < 256 || -off <= 256)
        o(0x38000000 | dst | bas << 5 | (off & 511) << 12 | sz << 30);
        // stur(*) x(dst),[x(bas],#(off)]
    另 {
        arm64_movimm(30, off); // use x30 for offset
        o(0x38206800 | dst | bas << 5 | (uint32_t)30 << 16 | sz << 30);
        // str(*) x(dst),[x(bas),x30]
    }
}

静态 空 arm64_strv(整 sz_, 整 dst, 整 bas, uint64_t off)
{
    uint32_t sz = sz_;
    如 (!(off & ~((uint32_t)0xfff << sz)))
        o(0x3d000000 | dst | bas << 5 | off << (10 - sz) |
          (sz & 4) << 21 | (sz & 3) << 30); // str (s|d|q)(dst),[x(bas),#(off)]
    另 如 (off < 256 || -off <= 256)
        o(0x3c000000 | dst | bas << 5 | (off & 511) << 12 |
          (sz & 4) << 21 | (sz & 3) << 30); // stur (s|d|q)(dst),[x(bas),#(off)]
    另 {
        arm64_movimm(30, off); // use x30 for offset
        o(0x3c206800 | dst | bas << 5 | (uint32_t)30 << 16 |
          sz << 30 | (sz & 4) << 21); // str (s|d|q)(dst),[x(bas),x30]
    }
}

静态 空 arm64_sym(整 r, Sym *sym, 无符 长 addend)
{
    // Currently TCC's linker does not generate COPY relocations for
    // STT_OBJECTs when tcc is invoked with "-run". This typically
    // results in "R_AARCH64_ADR_PREL_PG_HI21 relocation failed" when
    // a program refers to stdin. A workaround is to avoid that
    // relocation and use only relocations with unlimited range.
    整 avoid_adrp = 1;

    如 (avoid_adrp || sym->a.weak) {
        // (GCC uses a R_AARCH64_ABS64 in this case.)
        greloca(cur_text_section, sym, ind, R_AARCH64_MOVW_UABS_G0_NC, addend);
        o(0xd2800000 | r); // mov x(rt),#0,lsl #0
        greloca(cur_text_section, sym, ind, R_AARCH64_MOVW_UABS_G1_NC, addend);
        o(0xf2a00000 | r); // movk x(rt),#0,lsl #16
        greloca(cur_text_section, sym, ind, R_AARCH64_MOVW_UABS_G2_NC, addend);
        o(0xf2c00000 | r); // movk x(rt),#0,lsl #32
        greloca(cur_text_section, sym, ind, R_AARCH64_MOVW_UABS_G3, addend);
        o(0xf2e00000 | r); // movk x(rt),#0,lsl #48
    }
    另 {
        greloca(cur_text_section, sym, ind, R_AARCH64_ADR_PREL_PG_HI21, addend);
        o(0x90000000 | r);
        greloca(cur_text_section, sym, ind, R_AARCH64_ADD_ABS_LO12_NC, addend);
        o(0x91000000 | r | r << 5);
    }
}

ST_FUNC 空 load(整 r, SValue *sv)
{
    整 svtt = sv->type.t;
    整 svr = sv->r & ~VT_LVAL_TYPE;
    整 svrv = svr & VT_VALMASK;
    uint64_t svcul = (uint32_t)sv->c.i;
    svcul = svcul >> 31 & 1 ? svcul - ((uint64_t)1 << 32) : svcul;

    如 (svr == (VT_LOCAL | VT_LVAL)) {
        如 (IS_FREG(r))
            arm64_ldrv(arm64_type_size(svtt), fltr(r), 29, svcul);
        另
            arm64_ldrx(!(svtt & VT_UNSIGNED), arm64_type_size(svtt),
                       intr(r), 29, svcul);
        返回;
    }

    如 ((svr & ~VT_VALMASK) == VT_LVAL && svrv < VT_CONST) {
        如 (IS_FREG(r))
            arm64_ldrv(arm64_type_size(svtt), fltr(r), intr(svrv), 0);
        另
            arm64_ldrx(!(svtt & VT_UNSIGNED), arm64_type_size(svtt),
                       intr(r), intr(svrv), 0);
        返回;
    }

    如 (svr == (VT_CONST | VT_LVAL | VT_SYM)) {
        arm64_sym(30, sv->sym, svcul); // use x30 for address
        如 (IS_FREG(r))
            arm64_ldrv(arm64_type_size(svtt), fltr(r), 30, 0);
        另
            arm64_ldrx(!(svtt & VT_UNSIGNED), arm64_type_size(svtt),
                       intr(r), 30, 0);
        返回;
    }

    如 (svr == (VT_CONST | VT_SYM)) {
        arm64_sym(intr(r), sv->sym, svcul);
        返回;
    }

    如 (svr == VT_CONST) {
        如 ((svtt & VT_BTYPE) != VT_VOID)
            arm64_movimm(intr(r), arm64_type_size(svtt) == 3 ?
                         sv->c.i : (uint32_t)svcul);
        返回;
    }

    如 (svr < VT_CONST) {
        如 (IS_FREG(r) && IS_FREG(svr))
            如 (svtt == VT_LDOUBLE)
                o(0x4ea01c00 | fltr(r) | fltr(svr) << 5);
                    // mov v(r).16b,v(svr).16b
            另
                o(0x1e604000 | fltr(r) | fltr(svr) << 5); // fmov d(r),d(svr)
        另 如 (!IS_FREG(r) && !IS_FREG(svr))
            o(0xaa0003e0 | intr(r) | intr(svr) << 16); // mov x(r),x(svr)
        另
            assert(0);
      返回;
    }

    如 (svr == VT_LOCAL) {
        如 (-svcul < 0x1000)
            o(0xd10003a0 | intr(r) | -svcul << 10); // sub x(r),x29,#...
        另 {
            arm64_movimm(30, -svcul); // use x30 for offset
            o(0xcb0003a0 | intr(r) | (uint32_t)30 << 16); // sub x(r),x29,x30
        }
        返回;
    }

    如 (svr == VT_JMP || svr == VT_JMPI) {
        整 t = (svr == VT_JMPI);
        arm64_movimm(intr(r), t);
        o(0x14000002); // b .+8
        gsym(svcul);
        arm64_movimm(intr(r), t ^ 1);
        返回;
    }

    如 (svr == (VT_LLOCAL | VT_LVAL)) {
        arm64_ldrx(0, 3, 30, 29, svcul); // use x30 for offset
        如 (IS_FREG(r))
            arm64_ldrv(arm64_type_size(svtt), fltr(r), 30, 0);
        另
            arm64_ldrx(!(svtt & VT_UNSIGNED), arm64_type_size(svtt),
                       intr(r), 30, 0);
        返回;
    }

    printf("load(%x, (%x, %x, %llx))\n", r, svtt, sv->r, (长 长)svcul);
    assert(0);
}

ST_FUNC 空 store(整 r, SValue *sv)
{
    整 svtt = sv->type.t;
    整 svr = sv->r & ~VT_LVAL_TYPE;
    整 svrv = svr & VT_VALMASK;
    uint64_t svcul = (uint32_t)sv->c.i;
    svcul = svcul >> 31 & 1 ? svcul - ((uint64_t)1 << 32) : svcul;

    如 (svr == (VT_LOCAL | VT_LVAL)) {
        如 (IS_FREG(r))
            arm64_strv(arm64_type_size(svtt), fltr(r), 29, svcul);
        另
            arm64_strx(arm64_type_size(svtt), intr(r), 29, svcul);
        返回;
    }

    如 ((svr & ~VT_VALMASK) == VT_LVAL && svrv < VT_CONST) {
        如 (IS_FREG(r))
            arm64_strv(arm64_type_size(svtt), fltr(r), intr(svrv), 0);
        另
            arm64_strx(arm64_type_size(svtt), intr(r), intr(svrv), 0);
        返回;
    }

    如 (svr == (VT_CONST | VT_LVAL | VT_SYM)) {
        arm64_sym(30, sv->sym, svcul); // use x30 for address
        如 (IS_FREG(r))
            arm64_strv(arm64_type_size(svtt), fltr(r), 30, 0);
        另
            arm64_strx(arm64_type_size(svtt), intr(r), 30, 0);
        返回;
    }

    printf("store(%x, (%x, %x, %llx))\n", r, svtt, sv->r, (长 长)svcul);
    assert(0);
}

静态 空 arm64_gen_bl_or_b(整 b)
{
    如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
        assert(!b && (vtop->r & VT_SYM));
        greloca(cur_text_section, vtop->sym, ind, R_AARCH64_CALL26, 0);
        o(0x94000000); // bl .
    }
    另
        o(0xd61f0000 | (uint32_t)!b << 21 | intr(gv(RC_R30)) << 5); // br/blr
}

静态 整 arm64_hfa_aux(CType *type, 整 *fsize, 整 num)
{
    如 (is_float(type->t)) {
        整 a, n = type_size(type, &a);
        如 (num >= 4 || (*fsize && *fsize != n))
            返回 -1;
        *fsize = n;
        返回 num + 1;
    }
    另 如 ((type->t & VT_BTYPE) == VT_STRUCT) {
        整 is_struct = 0; // rather than union
        Sym *field;
        对于 (field = type->ref->next; field; field = field->next)
            如 (field->c) {
                is_struct = 1;
                跳出;
            }
        如 (is_struct) {
            整 num0 = num;
            对于 (field = type->ref->next; field; field = field->next) {
                如 (field->c != (num - num0) * *fsize)
                    返回 -1;
                num = arm64_hfa_aux(&field->type, fsize, num);
                如 (num == -1)
                    返回 -1;
            }
            如 (type->ref->c != (num - num0) * *fsize)
                返回 -1;
            返回 num;
        }
        另 { // union
            整 num0 = num;
            对于 (field = type->ref->next; field; field = field->next) {
                整 num1 = arm64_hfa_aux(&field->type, fsize, num0);
                如 (num1 == -1)
                    返回 -1;
                num = num1 < num ? num : num1;
            }
            如 (type->ref->c != (num - num0) * *fsize)
                返回 -1;
            返回 num;
        }
    }
    另 如 (type->t & VT_ARRAY) {
        整 num1;
        如 (!type->ref->c)
            返回 num;
        num1 = arm64_hfa_aux(&type->ref->type, fsize, num);
        如 (num1 == -1 || (num1 != num && type->ref->c > 4))
            返回 -1;
        num1 = num + type->ref->c * (num1 - num);
        如 (num1 > 4)
            返回 -1;
        返回 num1;
    }
    返回 -1;
}

静态 整 arm64_hfa(CType *type, 整 *fsize)
{
    如 ((type->t & VT_BTYPE) == VT_STRUCT || (type->t & VT_ARRAY)) {
        整 sz = 0;
        整 n = arm64_hfa_aux(type, &sz, 0);
        如 (0 < n && n <= 4) {
            如 (fsize)
                *fsize = sz;
            返回 n;
        }
    }
    返回 0;
}

静态 无符 长 arm64_pcs_aux(整 n, CType **type, 无符 长 *a)
{
    整 nx = 0; // next integer register
    整 nv = 0; // next vector register
    无符 长 ns = 32; // next stack offset
    整 i;

    对于 (i = 0; i < n; i++) {
        整 hfa = arm64_hfa(type[i], 0);
        整 size, align;

        如 ((type[i]->t & VT_ARRAY) ||
            (type[i]->t & VT_BTYPE) == VT_FUNC)
            size = align = 8;
        另
            size = type_size(type[i], &align);

        如 (hfa)
            // B.2
            ;
        另 如 (size > 16) {
            // B.3: replace with pointer
            如 (nx < 8)
                a[i] = nx++ << 1 | 1;
            另 {
                ns = (ns + 7) & ~7;
                a[i] = ns | 1;
                ns += 8;
            }
            继续;
        }
        另 如 ((type[i]->t & VT_BTYPE) == VT_STRUCT)
            // B.4
            size = (size + 7) & ~7;

        // C.1
        如 (is_float(type[i]->t) && nv < 8) {
            a[i] = 16 + (nv++ << 1);
            继续;
        }

        // C.2
        如 (hfa && nv + hfa <= 8) {
            a[i] = 16 + (nv << 1);
            nv += hfa;
            继续;
        }

        // C.3
        如 (hfa) {
            nv = 8;
            size = (size + 7) & ~7;
        }

        // C.4
        如 (hfa || (type[i]->t & VT_BTYPE) == VT_LDOUBLE) {
            ns = (ns + 7) & ~7;
            ns = (ns + align - 1) & -align;
        }

        // C.5
        如 ((type[i]->t & VT_BTYPE) == VT_FLOAT)
            size = 8;

        // C.6
        如 (hfa || is_float(type[i]->t)) {
            a[i] = ns;
            ns += size;
            继续;
        }

        // C.7
        如 ((type[i]->t & VT_BTYPE) != VT_STRUCT && size <= 8 && nx < 8) {
            a[i] = nx++ << 1;
            继续;
        }

        // C.8
        如 (align == 16)
            nx = (nx + 1) & ~1;

        // C.9
        如 ((type[i]->t & VT_BTYPE) != VT_STRUCT && size == 16 && nx < 7) {
            a[i] = nx << 1;
            nx += 2;
            继续;
        }

        // C.10
        如 ((type[i]->t & VT_BTYPE) == VT_STRUCT && size <= (8 - nx) * 8) {
            a[i] = nx << 1;
            nx += (size + 7) >> 3;
            继续;
        }

        // C.11
        nx = 8;

        // C.12
        ns = (ns + 7) & ~7;
        ns = (ns + align - 1) & -align;

        // C.13
        如 ((type[i]->t & VT_BTYPE) == VT_STRUCT) {
            a[i] = ns;
            ns += size;
            继续;
        }

        // C.14
        如 (size < 8)
            size = 8;

        // C.15
        a[i] = ns;
        ns += size;
    }

    返回 ns - 32;
}

静态 无符 长 arm64_pcs(整 n, CType **type, 无符 长 *a)
{
    无符 长 stack;

    // Return type:
    如 ((type[0]->t & VT_BTYPE) == VT_VOID)
        a[0] = -1;
    另 {
        arm64_pcs_aux(1, type, a);
        assert(a[0] == 0 || a[0] == 1 || a[0] == 16);
    }

    // Argument types:
    stack = arm64_pcs_aux(n, type + 1, a + 1);

    如 (0) {
        整 i;
        对于 (i = 0; i <= n; i++) {
            如 (!i)
                printf("arm64_pcs return: ");
            另
                printf("arm64_pcs arg %d: ", i);
            如 (a[i] == (无符 长)-1)
                printf("void\n");
            另 如 (a[i] == 1 && !i)
                printf("X8 pointer\n");
            另 如 (a[i] < 16)
                printf("X%lu%s\n", a[i] / 2, a[i] & 1 ? " pointer" : "");
            另 如 (a[i] < 32)
                printf("V%lu\n", a[i] / 2 - 8);
            另
                printf("stack %lu%s\n",
                       (a[i] - 32) & ~1, a[i] & 1 ? " pointer" : "");
        }
    }

    返回 stack;
}

ST_FUNC 空 gfunc_call(整 nb_args)
{
    CType *return_type;
    CType **t;
    无符 长 *a, *a1;
    无符 长 stack;
    整 i;

    return_type = &vtop[-nb_args].type.ref->type;
    如 ((return_type->t & VT_BTYPE) == VT_STRUCT)
        --nb_args;

    t = tcc_malloc((nb_args + 1) * 求长度(*t));
    a = tcc_malloc((nb_args + 1) * 求长度(*a));
    a1 = tcc_malloc((nb_args + 1) * 求长度(*a1));

    t[0] = return_type;
    对于 (i = 0; i < nb_args; i++)
        t[nb_args - i] = &vtop[-i].type;

    stack = arm64_pcs(nb_args, t, a);

    // Allocate space for structs replaced by pointer:
    对于 (i = nb_args; i; i--)
        如 (a[i] & 1) {
            SValue *arg = &vtop[i - nb_args];
            整 align, size = type_size(&arg->type, &align);
            assert((arg->type.t & VT_BTYPE) == VT_STRUCT);
            stack = (stack + align - 1) & -align;
            a1[i] = stack;
            stack += size;
        }

    stack = (stack + 15) >> 4 << 4;

    assert(stack < 0x1000);
    如 (stack)
        o(0xd10003ff | stack << 10); // sub sp,sp,#(n)

    // First pass: set all values on stack
    对于 (i = nb_args; i; i--) {
        vpushv(vtop - nb_args + i);

        如 (a[i] & 1) {
            // struct replaced by pointer
            整 r = get_reg(RC_INT);
            arm64_spoff(intr(r), a1[i]);
            vset(&vtop->type, r | VT_LVAL, 0);
            vswap();
            vstore();
            如 (a[i] >= 32) {
                // pointer on stack
                r = get_reg(RC_INT);
                arm64_spoff(intr(r), a1[i]);
                arm64_strx(3, intr(r), 31, (a[i] - 32) >> 1 << 1);
            }
        }
        另 如 (a[i] >= 32) {
            // value on stack
            如 ((vtop->type.t & VT_BTYPE) == VT_STRUCT) {
                整 r = get_reg(RC_INT);
                arm64_spoff(intr(r), a[i] - 32);
                vset(&vtop->type, r | VT_LVAL, 0);
                vswap();
                vstore();
            }
            另 如 (is_float(vtop->type.t)) {
                gv(RC_FLOAT);
                arm64_strv(arm64_type_size(vtop[0].type.t),
                           fltr(vtop[0].r), 31, a[i] - 32);
            }
            另 {
                gv(RC_INT);
                arm64_strx(arm64_type_size(vtop[0].type.t),
                           intr(vtop[0].r), 31, a[i] - 32);
            }
        }

        --vtop;
    }

    // Second pass: assign values to registers
    对于 (i = nb_args; i; i--, vtop--) {
        如 (a[i] < 16 && !(a[i] & 1)) {
            // value in general-purpose registers
            如 ((vtop->type.t & VT_BTYPE) == VT_STRUCT) {
                整 align, size = type_size(&vtop->type, &align);
                vtop->type.t = VT_PTR;
                gaddrof();
                gv(RC_R(a[i] / 2));
                arm64_ldrs(a[i] / 2, size);
            }
            另
                gv(RC_R(a[i] / 2));
        }
        另 如 (a[i] < 16)
            // struct replaced by pointer in register
            arm64_spoff(a[i] / 2, a1[i]);
        另 如 (a[i] < 32) {
            // value in floating-point registers
            如 ((vtop->type.t & VT_BTYPE) == VT_STRUCT) {
                uint32_t j, sz, n = arm64_hfa(&vtop->type, &sz);
                vtop->type.t = VT_PTR;
                gaddrof();
                gv(RC_R30);
                对于 (j = 0; j < n; j++)
                    o(0x3d4003c0 |
                      (sz & 16) << 19 | -(sz & 8) << 27 | (sz & 4) << 29 |
                      (a[i] / 2 - 8 + j) |
                      j << 10); // ldr ([sdq])(*),[x30,#(j * sz)]
            }
            另
                gv(RC_F(a[i] / 2 - 8));
        }
    }

    如 ((return_type->t & VT_BTYPE) == VT_STRUCT) {
        如 (a[0] == 1) {
            // indirect return: set x8 and discard the stack value
            gv(RC_R(8));
            --vtop;
        }
        另
            // return in registers: keep the address for after the call
            vswap();
    }

    save_regs(0);
    arm64_gen_bl_or_b(0);
    --vtop;
    如 (stack)
        o(0x910003ff | stack << 10); // add sp,sp,#(n)

    {
        整 rt = return_type->t;
        整 bt = rt & VT_BTYPE;
        如 (bt == VT_BYTE || bt == VT_SHORT)
            // Promote small integers:
            o(0x13001c00 | (bt == VT_SHORT) << 13 |
              (uint32_t)!!(rt & VT_UNSIGNED) << 30); // [su]xt[bh] w0,w0
        另 如 (bt == VT_STRUCT && !(a[0] & 1)) {
            // A struct was returned in registers, so write it out:
            gv(RC_R(8));
            --vtop;
            如 (a[0] == 0) {
                整 align, size = type_size(return_type, &align);
                assert(size <= 16);
                如 (size > 8)
                    o(0xa9000500); // stp x0,x1,[x8]
                另 如 (size)
                    arm64_strx(size > 4 ? 3 : size > 2 ? 2 : size > 1, 0, 8, 0);

            }
            另 如 (a[0] == 16) {
                uint32_t j, sz, n = arm64_hfa(return_type, &sz);
                对于 (j = 0; j < n; j++)
                    o(0x3d000100 |
                      (sz & 16) << 19 | -(sz & 8) << 27 | (sz & 4) << 29 |
                      (a[i] / 2 - 8 + j) |
                      j << 10); // str ([sdq])(*),[x8,#(j * sz)]
            }
        }
    }

    tcc_free(a1);
    tcc_free(a);
    tcc_free(t);
}

静态 无符 长 arm64_func_va_list_stack;
静态 整 arm64_func_va_list_gr_offs;
静态 整 arm64_func_va_list_vr_offs;
静态 整 arm64_func_sub_sp_offset;

ST_FUNC 空 gfunc_prolog(CType *func_type)
{
    整 n = 0;
    整 i = 0;
    Sym *sym;
    CType **t;
    无符 长 *a;

    // Why doesn't the caller (gen_function) set func_vt?
    func_vt = func_type->ref->type;
    func_vc = 144; // offset of where x8 is stored

    对于 (sym = func_type->ref; sym; sym = sym->next)
        ++n;
    t = tcc_malloc(n * 求长度(*t));
    a = tcc_malloc(n * 求长度(*a));

    对于 (sym = func_type->ref; sym; sym = sym->next)
        t[i++] = &sym->type;

    arm64_func_va_list_stack = arm64_pcs(n - 1, t, a);

    o(0xa9b27bfd); // stp x29,x30,[sp,#-224]!
    o(0xad0087e0); // stp q0,q1,[sp,#16]
    o(0xad018fe2); // stp q2,q3,[sp,#48]
    o(0xad0297e4); // stp q4,q5,[sp,#80]
    o(0xad039fe6); // stp q6,q7,[sp,#112]
    o(0xa90923e8); // stp x8,x8,[sp,#144]
    o(0xa90a07e0); // stp x0,x1,[sp,#160]
    o(0xa90b0fe2); // stp x2,x3,[sp,#176]
    o(0xa90c17e4); // stp x4,x5,[sp,#192]
    o(0xa90d1fe6); // stp x6,x7,[sp,#208]

    arm64_func_va_list_gr_offs = -64;
    arm64_func_va_list_vr_offs = -128;

    对于 (i = 1, sym = func_type->ref->next; sym; i++, sym = sym->next) {
        整 off = (a[i] < 16 ? 160 + a[i] / 2 * 8 :
                   a[i] < 32 ? 16 + (a[i] - 16) / 2 * 16 :
                   224 + ((a[i] - 32) >> 1 << 1));
        sym_push(sym->v & ~SYM_FIELD, &sym->type,
                 (a[i] & 1 ? VT_LLOCAL : VT_LOCAL) | lvalue_type(sym->type.t),
                 off);

        如 (a[i] < 16) {
            整 align, size = type_size(&sym->type, &align);
            arm64_func_va_list_gr_offs = (a[i] / 2 - 7 +
                                          (!(a[i] & 1) && size > 8)) * 8;
        }
        另 如 (a[i] < 32) {
            uint32_t hfa = arm64_hfa(&sym->type, 0);
            arm64_func_va_list_vr_offs = (a[i] / 2 - 16 +
                                          (hfa ? hfa : 1)) * 16;
        }

        // HFAs of float and double need to be written differently:
        如 (16 <= a[i] && a[i] < 32 && (sym->type.t & VT_BTYPE) == VT_STRUCT) {
            uint32_t j, sz, k = arm64_hfa(&sym->type, &sz);
            如 (sz < 16)
                对于 (j = 0; j < k; j++) {
                    o(0x3d0003e0 | -(sz & 8) << 27 | (sz & 4) << 29 |
                      ((a[i] - 16) / 2 + j) | (off / sz + j) << 10);
                    // str ([sdq])(*),[sp,#(j * sz)]
                }
        }
    }

    tcc_free(a);
    tcc_free(t);

    o(0x910003fd); // mov x29,sp
    arm64_func_sub_sp_offset = ind;
    // In gfunc_epilog these will be replaced with code to decrement SP:
    o(0xd503201f); // nop
    o(0xd503201f); // nop
    loc = 0;
}

ST_FUNC 空 gen_va_start(空)
{
    整 r;
    --vtop; // we don't need the "arg"
    gaddrof();
    r = intr(gv(RC_INT));

    如 (arm64_func_va_list_stack) {
        //xx could use add (immediate) here
        arm64_movimm(30, arm64_func_va_list_stack + 224);
        o(0x8b1e03be); // add x30,x29,x30
    }
    另
        o(0x910383be); // add x30,x29,#224
    o(0xf900001e | r << 5); // str x30,[x(r)]

    如 (arm64_func_va_list_gr_offs) {
        如 (arm64_func_va_list_stack)
            o(0x910383be); // add x30,x29,#224
        o(0xf900041e | r << 5); // str x30,[x(r),#8]
    }

    如 (arm64_func_va_list_vr_offs) {
        o(0x910243be); // add x30,x29,#144
        o(0xf900081e | r << 5); // str x30,[x(r),#16]
    }

    arm64_movimm(30, arm64_func_va_list_gr_offs);
    o(0xb900181e | r << 5); // str w30,[x(r),#24]

    arm64_movimm(30, arm64_func_va_list_vr_offs);
    o(0xb9001c1e | r << 5); // str w30,[x(r),#28]

    --vtop;
}

ST_FUNC 空 gen_va_arg(CType *t)
{
    整 align, size = type_size(t, &align);
    整 fsize, hfa = arm64_hfa(t, &fsize);
    uint32_t r0, r1;

    如 (is_float(t->t)) {
        hfa = 1;
        fsize = size;
    }

    gaddrof();
    r0 = intr(gv(RC_INT));
    r1 = get_reg(RC_INT);
    vtop[0].r = r1 | lvalue_type(t->t);
    r1 = intr(r1);

    如 (!hfa) {
        uint32_t n = size > 16 ? 8 : (size + 7) & -8;
        o(0xb940181e | r0 << 5); // ldr w30,[x(r0),#24] // __gr_offs
        如 (align == 16) {
            assert(0); // this path untested but needed for __uint128_t
            o(0x11003fde); // add w30,w30,#15
            o(0x121c6fde); // and w30,w30,#-16
        }
        o(0x310003c0 | r1 | n << 10); // adds w(r1),w30,#(n)
        o(0x540000ad); // b.le .+20
        o(0xf9400000 | r1 | r0 << 5); // ldr x(r1),[x(r0)] // __stack
        o(0x9100001e | r1 << 5 | n << 10); // add x30,x(r1),#(n)
        o(0xf900001e | r0 << 5); // str x30,[x(r0)] // __stack
        o(0x14000004); // b .+16
        o(0xb9001800 | r1 | r0 << 5); // str w(r1),[x(r0),#24] // __gr_offs
        o(0xf9400400 | r1 | r0 << 5); // ldr x(r1),[x(r0),#8] // __gr_top
        o(0x8b3ec000 | r1 | r1 << 5); // add x(r1),x(r1),w30,sxtw
        如 (size > 16)
            o(0xf9400000 | r1 | r1 << 5); // ldr x(r1),[x(r1)]
    }
    另 {
        uint32_t rsz = hfa << 4;
        uint32_t ssz = (size + 7) & -(uint32_t)8;
        uint32_t b1, b2;
        o(0xb9401c1e | r0 << 5); // ldr w30,[x(r0),#28] // __vr_offs
        o(0x310003c0 | r1 | rsz << 10); // adds w(r1),w30,#(rsz)
        b1 = ind; o(0x5400000d); // b.le lab1
        o(0xf9400000 | r1 | r0 << 5); // ldr x(r1),[x(r0)] // __stack
        如 (fsize == 16) {
            o(0x91003c00 | r1 | r1 << 5); // add x(r1),x(r1),#15
            o(0x927cec00 | r1 | r1 << 5); // and x(r1),x(r1),#-16
        }
        o(0x9100001e | r1 << 5 | ssz << 10); // add x30,x(r1),#(ssz)
        o(0xf900001e | r0 << 5); // str x30,[x(r0)] // __stack
        b2 = ind; o(0x14000000); // b lab2
        // lab1:
        write32le(cur_text_section->data + b1, 0x5400000d | (ind - b1) << 3);
        o(0xb9001c00 | r1 | r0 << 5); // str w(r1),[x(r0),#28] // __vr_offs
        o(0xf9400800 | r1 | r0 << 5); // ldr x(r1),[x(r0),#16] // __vr_top
        如 (hfa == 1 || fsize == 16)
            o(0x8b3ec000 | r1 | r1 << 5); // add x(r1),x(r1),w30,sxtw
        另 {
            // We need to change the layout of this HFA.
            // Get some space on the stack using global variable "loc":
            loc = (loc - size) & -(uint32_t)align;
            o(0x8b3ec000 | 30 | r1 << 5); // add x30,x(r1),w30,sxtw
            arm64_movimm(r1, loc);
            o(0x8b0003a0 | r1 | r1 << 16); // add x(r1),x29,x(r1)
            o(0x4c402bdc | (uint32_t)fsize << 7 |
              (uint32_t)(hfa == 2) << 15 |
              (uint32_t)(hfa == 3) << 14); // ld1 {v28.(4s|2d),...},[x30]
            o(0x0d00801c | r1 << 5 | (fsize == 8) << 10 |
              (uint32_t)(hfa != 2) << 13 |
              (uint32_t)(hfa != 3) << 21); // st(hfa) {v28.(s|d),...}[0],[x(r1)]
        }
        // lab2:
        write32le(cur_text_section->data + b2, 0x14000000 | (ind - b2) >> 2);
    }
}

ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret,
                       整 *align, 整 *regsize)
{
    返回 0;
}

ST_FUNC 空 gfunc_return(CType *func_type)
{
    CType *t = func_type;
    无符 长 a;

    arm64_pcs(0, &t, &a);
    转接 (a) {
    事例 -1:
        跳出;
    事例 0:
        如 ((func_type->t & VT_BTYPE) == VT_STRUCT) {
            整 align, size = type_size(func_type, &align);
            gaddrof();
            gv(RC_R(0));
            arm64_ldrs(0, size);
        }
        另
            gv(RC_IRET);
        跳出;
    事例 1: {
        CType type = *func_type;
        mk_pointer(&type);
        vset(&type, VT_LOCAL | VT_LVAL, func_vc);
        indir();
        vswap();
        vstore();
        跳出;
    }
    事例 16:
        如 ((func_type->t & VT_BTYPE) == VT_STRUCT) {
          uint32_t j, sz, n = arm64_hfa(&vtop->type, &sz);
          gaddrof();
          gv(RC_R(0));
          对于 (j = 0; j < n; j++)
              o(0x3d400000 |
                (sz & 16) << 19 | -(sz & 8) << 27 | (sz & 4) << 29 |
                j | j << 10); // ldr ([sdq])(*),[x0,#(j * sz)]
        }
        另
            gv(RC_FRET);
        跳出;
    缺省:
      assert(0);
    }
    vtop--;
}

ST_FUNC 空 gfunc_epilog(空)
{
    如 (loc) {
        // Insert instructions to subtract size of stack frame from SP.
        无符 字 *ptr = cur_text_section->data + arm64_func_sub_sp_offset;
        uint64_t diff = (-loc + 15) & ~15;
        如 (!(diff >> 24)) {
            如 (diff & 0xfff) // sub sp,sp,#(diff & 0xfff)
                write32le(ptr, 0xd10003ff | (diff & 0xfff) << 10);
            如 (diff >> 12) // sub sp,sp,#(diff >> 12),lsl #12
                write32le(ptr + 4, 0xd14003ff | (diff >> 12) << 10);
        }
        另 {
            // In this case we may subtract more than necessary,
            // but always less than 17/16 of what we were aiming for.
            整 i = 0;
            整 j = 0;
            当 (diff >> 20) {
                diff = (diff + 0xffff) >> 16;
                ++i;
            }
            当 (diff >> 16) {
                diff = (diff + 1) >> 1;
                ++j;
            }
            write32le(ptr, 0xd2800010 | diff << 5 | i << 21);
            // mov x16,#(diff),lsl #(16 * i)
            write32le(ptr + 4, 0xcb3063ff | j << 10);
            // sub sp,sp,x16,lsl #(j)
        }
    }
    o(0x910003bf); // mov sp,x29
    o(0xa8ce7bfd); // ldp x29,x30,[sp],#224

    o(0xd65f03c0); // ret
}

// Generate forward branch to label:
ST_FUNC 整 gjmp(整 t)
{
    整 r = ind;
    如 (nocode_wanted)
        返回 t;
    o(t);
    返回 r;
}

// Generate branch to known address:
ST_FUNC 空 gjmp_addr(整 a)
{
    assert(a - ind + 0x8000000 < 0x10000000);
    o(0x14000000 | ((a - ind) >> 2 & 0x3ffffff));
}

ST_FUNC 整 gtst(整 inv, 整 t)
{
    整 bt = vtop->type.t & VT_BTYPE;
    如 (bt == VT_LDOUBLE) {
        uint32_t a, b, f = fltr(gv(RC_FLOAT));
        a = get_reg(RC_INT);
        vpushi(0);
        vtop[0].r = a;
        b = get_reg(RC_INT);
        a = intr(a);
        b = intr(b);
        o(0x4e083c00 | a | f << 5); // mov x(a),v(f).d[0]
        o(0x4e183c00 | b | f << 5); // mov x(b),v(f).d[1]
        o(0xaa000400 | a | a << 5 | b << 16); // orr x(a),x(a),x(b),lsl #1
        o(0xb4000040 | a | !!inv << 24); // cbz/cbnz x(a),.+8
        --vtop;
    }
    另 如 (bt == VT_FLOAT || bt == VT_DOUBLE) {
        uint32_t a = fltr(gv(RC_FLOAT));
        o(0x1e202008 | a << 5 | (bt != VT_FLOAT) << 22); // fcmp
        o(0x54000040 | !!inv); // b.eq/b.ne .+8
    }
    另 {
        uint32_t ll = (bt == VT_PTR || bt == VT_LLONG);
        uint32_t a = intr(gv(RC_INT));
        o(0x34000040 | a | !!inv << 24 | ll << 31); // cbz/cbnz wA,.+8
    }
    --vtop;
    返回 gjmp(t);
}

静态 整 arm64_iconst(uint64_t *val, SValue *sv)
{
    如 ((sv->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        返回 0;
    如 (val) {
        整 t = sv->type.t;
        整 bt = t & VT_BTYPE;
        *val = ((bt == VT_LLONG || bt == VT_PTR) ? sv->c.i :
                (uint32_t)sv->c.i |
                (t & VT_UNSIGNED ? 0 : -(sv->c.i & 0x80000000)));
    }
    返回 1;
}

静态 整 arm64_gen_opic(整 op, uint32_t l, 整 rev, uint64_t val,
                          uint32_t x, uint32_t a)
{
    如 (op == '-' && !rev) {
        val = -val;
        op = '+';
    }
    val = l ? val : (uint32_t)val;

    转接 (op) {

    事例 '+': {
        uint32_t s = l ? val >> 63 : val >> 31;
        val = s ? -val : val;
        val = l ? val : (uint32_t)val;
        如 (!(val & ~(uint64_t)0xfff))
            o(0x11000000 | l << 31 | s << 30 | x | a << 5 | val << 10);
        另 如 (!(val & ~(uint64_t)0xfff000))
            o(0x11400000 | l << 31 | s << 30 | x | a << 5 | val >> 12 << 10);
        另 {
            arm64_movimm(30, val); // use x30
            o(0x0b1e0000 | l << 31 | s << 30 | x | a << 5);
        }
        返回 1;
      }

    事例 '-':
        如 (!val)
            o(0x4b0003e0 | l << 31 | x | a << 16); // neg
        另 如 (val == (l ? (uint64_t)-1 : (uint32_t)-1))
            o(0x2a2003e0 | l << 31 | x | a << 16); // mvn
        另 {
            arm64_movimm(30, val); // use x30
            o(0x4b0003c0 | l << 31 | x | a << 16); // sub
        }
        返回 1;

    事例 '^':
        如 (val == -1 || (val == 0xffffffff && !l)) {
            o(0x2a2003e0 | l << 31 | x | a << 16); // mvn
            返回 1;
        }
        // fall through
    事例 '&':
    事例 '|': {
        整 e = arm64_encode_bimm64(l ? val : val | val << 32);
        如 (e < 0)
            返回 0;
        o((op == '&' ? 0x12000000 :
           op == '|' ? 0x32000000 : 0x52000000) |
          l << 31 | x | a << 5 | (uint32_t)e << 10);
        返回 1;
    }

    事例 TOK_SAR:
    事例 TOK_SHL:
    事例 TOK_SHR: {
        uint32_t n = 32 << l;
        val = val & (n - 1);
        如 (rev)
            返回 0;
        如 (!val)
            assert(0);
        另 如 (op == TOK_SHL)
            o(0x53000000 | l << 31 | l << 22 | x | a << 5 |
              (n - val) << 16 | (n - 1 - val) << 10); // lsl
        另
            o(0x13000000 | (op == TOK_SHR) << 30 | l << 31 | l << 22 |
              x | a << 5 | val << 16 | (n - 1) << 10); // lsr/asr
        返回 1;
    }

    }
    返回 0;
}

静态 空 arm64_gen_opil(整 op, uint32_t l)
{
    uint32_t x, a, b;

    // Special treatment for operations with a constant operand:
    {
        uint64_t val;
        整 rev = 1;

        如 (arm64_iconst(0, &vtop[0])) {
            vswap();
            rev = 0;
        }
        如 (arm64_iconst(&val, &vtop[-1])) {
            gv(RC_INT);
            a = intr(vtop[0].r);
            --vtop;
            x = get_reg(RC_INT);
            ++vtop;
            如 (arm64_gen_opic(op, l, rev, val, intr(x), a)) {
                vtop[0].r = x;
                vswap();
                --vtop;
                返回;
            }
        }
        如 (!rev)
            vswap();
    }

    gv2(RC_INT, RC_INT);
    assert(vtop[-1].r < VT_CONST && vtop[0].r < VT_CONST);
    a = intr(vtop[-1].r);
    b = intr(vtop[0].r);
    vtop -= 2;
    x = get_reg(RC_INT);
    ++vtop;
    vtop[0].r = x;
    x = intr(x);

    转接 (op) {
    事例 '%':
        // Use x30 for quotient:
        o(0x1ac00c00 | l << 31 | 30 | a << 5 | b << 16); // sdiv
        o(0x1b008000 | l << 31 | x | (uint32_t)30 << 5 |
          b << 16 | a << 10); // msub
        跳出;
    事例 '&':
        o(0x0a000000 | l << 31 | x | a << 5 | b << 16); // and
        跳出;
    事例 '*':
        o(0x1b007c00 | l << 31 | x | a << 5 | b << 16); // mul
        跳出;
    事例 '+':
        o(0x0b000000 | l << 31 | x | a << 5 | b << 16); // add
        跳出;
    事例 '-':
        o(0x4b000000 | l << 31 | x | a << 5 | b << 16); // sub
        跳出;
    事例 '/':
        o(0x1ac00c00 | l << 31 | x | a << 5 | b << 16); // sdiv
        跳出;
    事例 '^':
        o(0x4a000000 | l << 31 | x | a << 5 | b << 16); // eor
        跳出;
    事例 '|':
        o(0x2a000000 | l << 31 | x | a << 5 | b << 16); // orr
        跳出;
    事例 TOK_EQ:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f17e0 | x); // cset wA,eq
        跳出;
    事例 TOK_GE:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9fb7e0 | x); // cset wA,ge
        跳出;
    事例 TOK_GT:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9fd7e0 | x); // cset wA,gt
        跳出;
    事例 TOK_LE:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9fc7e0 | x); // cset wA,le
        跳出;
    事例 TOK_LT:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9fa7e0 | x); // cset wA,lt
        跳出;
    事例 TOK_NE:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f07e0 | x); // cset wA,ne
        跳出;
    事例 TOK_SAR:
        o(0x1ac02800 | l << 31 | x | a << 5 | b << 16); // asr
        跳出;
    事例 TOK_SHL:
        o(0x1ac02000 | l << 31 | x | a << 5 | b << 16); // lsl
        跳出;
    事例 TOK_SHR:
        o(0x1ac02400 | l << 31 | x | a << 5 | b << 16); // lsr
        跳出;
    事例 TOK_UDIV:
    事例 TOK_PDIV:
        o(0x1ac00800 | l << 31 | x | a << 5 | b << 16); // udiv
        跳出;
    事例 TOK_UGE:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f37e0 | x); // cset wA,cs
        跳出;
    事例 TOK_UGT:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f97e0 | x); // cset wA,hi
        跳出;
    事例 TOK_ULT:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f27e0 | x); // cset wA,cc
        跳出;
    事例 TOK_ULE:
        o(0x6b00001f | l << 31 | a << 5 | b << 16); // cmp
        o(0x1a9f87e0 | x); // cset wA,ls
        跳出;
    事例 TOK_UMOD:
        // Use x30 for quotient:
        o(0x1ac00800 | l << 31 | 30 | a << 5 | b << 16); // udiv
        o(0x1b008000 | l << 31 | x | (uint32_t)30 << 5 |
          b << 16 | a << 10); // msub
        跳出;
    缺省:
        assert(0);
    }
}

ST_FUNC 空 gen_opi(整 op)
{
    arm64_gen_opil(op, 0);
}

ST_FUNC 空 gen_opl(整 op)
{
    arm64_gen_opil(op, 1);
}

ST_FUNC 空 gen_opf(整 op)
{
    uint32_t x, a, b, dbl;

    如 (vtop[0].type.t == VT_LDOUBLE) {
        CType type = vtop[0].type;
        整 func = 0;
        整 cond = -1;
        转接 (op) {
        事例 '*': func = TOK___multf3; 跳出;
        事例 '+': func = TOK___addtf3; 跳出;
        事例 '-': func = TOK___subtf3; 跳出;
        事例 '/': func = TOK___divtf3; 跳出;
        事例 TOK_EQ: func = TOK___eqtf2; cond = 1; 跳出;
        事例 TOK_NE: func = TOK___netf2; cond = 0; 跳出;
        事例 TOK_LT: func = TOK___lttf2; cond = 10; 跳出;
        事例 TOK_GE: func = TOK___getf2; cond = 11; 跳出;
        事例 TOK_LE: func = TOK___letf2; cond = 12; 跳出;
        事例 TOK_GT: func = TOK___gttf2; cond = 13; 跳出;
        缺省: assert(0); 跳出;
        }
        vpush_global_sym(&func_old_type, func);
        vrott(3);
        gfunc_call(2);
        vpushi(0);
        vtop->r = cond < 0 ? REG_FRET : REG_IRET;
        如 (cond < 0)
            vtop->type = type;
        另 {
            o(0x7100001f); // cmp w0,#0
            o(0x1a9f07e0 | (uint32_t)cond << 12); // cset w0,(cond)
        }
        返回;
    }

    dbl = vtop[0].type.t != VT_FLOAT;
    gv2(RC_FLOAT, RC_FLOAT);
    assert(vtop[-1].r < VT_CONST && vtop[0].r < VT_CONST);
    a = fltr(vtop[-1].r);
    b = fltr(vtop[0].r);
    vtop -= 2;
    转接 (op) {
    事例 TOK_EQ: 事例 TOK_NE:
    事例 TOK_LT: 事例 TOK_GE: 事例 TOK_LE: 事例 TOK_GT:
        x = get_reg(RC_INT);
        ++vtop;
        vtop[0].r = x;
        x = intr(x);
        跳出;
    缺省:
        x = get_reg(RC_FLOAT);
        ++vtop;
        vtop[0].r = x;
        x = fltr(x);
        跳出;
    }

    转接 (op) {
    事例 '*':
        o(0x1e200800 | dbl << 22 | x | a << 5 | b << 16); // fmul
        跳出;
    事例 '+':
        o(0x1e202800 | dbl << 22 | x | a << 5 | b << 16); // fadd
        跳出;
    事例 '-':
        o(0x1e203800 | dbl << 22 | x | a << 5 | b << 16); // fsub
        跳出;
    事例 '/':
        o(0x1e201800 | dbl << 22 | x | a << 5 | b << 16); // fdiv
        跳出;
    事例 TOK_EQ:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9f17e0 | x); // cset w(x),eq
        跳出;
    事例 TOK_GE:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9fb7e0 | x); // cset w(x),ge
        跳出;
    事例 TOK_GT:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9fd7e0 | x); // cset w(x),gt
        跳出;
    事例 TOK_LE:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9f87e0 | x); // cset w(x),ls
        跳出;
    事例 TOK_LT:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9f57e0 | x); // cset w(x),mi
        跳出;
    事例 TOK_NE:
        o(0x1e202000 | dbl << 22 | a << 5 | b << 16); // fcmp
        o(0x1a9f07e0 | x); // cset w(x),ne
        跳出;
    缺省:
        assert(0);
    }
}

// Generate sign extension from 32 to 64 bits:
ST_FUNC 空 gen_cvt_sxtw(空)
{
    uint32_t r = intr(gv(RC_INT));
    o(0x93407c00 | r | r << 5); // sxtw x(r),w(r)
}

ST_FUNC 空 gen_cvt_itof(整 t)
{
    如 (t == VT_LDOUBLE) {
        整 f = vtop->type.t;
        整 func = (f & VT_BTYPE) == VT_LLONG ?
          (f & VT_UNSIGNED ? TOK___floatunditf : TOK___floatditf) :
          (f & VT_UNSIGNED ? TOK___floatunsitf : TOK___floatsitf);
        vpush_global_sym(&func_old_type, func);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->type.t = t;
        vtop->r = REG_FRET;
        返回;
    }
    另 {
        整 d, n = intr(gv(RC_INT));
        整 s = !(vtop->type.t & VT_UNSIGNED);
        uint32_t l = ((vtop->type.t & VT_BTYPE) == VT_LLONG);
        --vtop;
        d = get_reg(RC_FLOAT);
        ++vtop;
        vtop[0].r = d;
        o(0x1e220000 | (uint32_t)!s << 16 |
          (uint32_t)(t != VT_FLOAT) << 22 | fltr(d) |
          l << 31 | n << 5); // [us]cvtf [sd](d),[wx](n)
    }
}

ST_FUNC 空 gen_cvt_ftoi(整 t)
{
    如 ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
        整 func = (t & VT_BTYPE) == VT_LLONG ?
          (t & VT_UNSIGNED ? TOK___fixunstfdi : TOK___fixtfdi) :
          (t & VT_UNSIGNED ? TOK___fixunstfsi : TOK___fixtfsi);
        vpush_global_sym(&func_old_type, func);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->type.t = t;
        vtop->r = REG_IRET;
        返回;
    }
    另 {
        整 d, n = fltr(gv(RC_FLOAT));
        uint32_t l = ((vtop->type.t & VT_BTYPE) != VT_FLOAT);
        --vtop;
        d = get_reg(RC_INT);
        ++vtop;
        vtop[0].r = d;
        o(0x1e380000 |
          (uint32_t)!!(t & VT_UNSIGNED) << 16 |
          (uint32_t)((t & VT_BTYPE) == VT_LLONG) << 31 | intr(d) |
          l << 22 | n << 5); // fcvtz[su] [wx](d),[sd](n)
    }
}

ST_FUNC 空 gen_cvt_ftof(整 t)
{
    整 f = vtop[0].type.t;
    assert(t == VT_FLOAT || t == VT_DOUBLE || t == VT_LDOUBLE);
    assert(f == VT_FLOAT || f == VT_DOUBLE || f == VT_LDOUBLE);
    如 (t == f)
        返回;

    如 (t == VT_LDOUBLE || f == VT_LDOUBLE) {
        整 func = (t == VT_LDOUBLE) ?
            (f == VT_FLOAT ? TOK___extendsftf2 : TOK___extenddftf2) :
            (t == VT_FLOAT ? TOK___trunctfsf2 : TOK___trunctfdf2);
        vpush_global_sym(&func_old_type, func);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->type.t = t;
        vtop->r = REG_FRET;
    }
    另 {
        整 x, a;
        gv(RC_FLOAT);
        assert(vtop[0].r < VT_CONST);
        a = fltr(vtop[0].r);
        --vtop;
        x = get_reg(RC_FLOAT);
        ++vtop;
        vtop[0].r = x;
        x = fltr(x);

        如 (f == VT_FLOAT)
            o(0x1e22c000 | x | a << 5); // fcvt d(x),s(a)
        另
            o(0x1e624000 | x | a << 5); // fcvt s(x),d(a)
    }
}

ST_FUNC 空 ggoto(空)
{
    arm64_gen_bl_or_b(1);
    --vtop;
}

ST_FUNC 空 gen_clear_cache(空)
{
    uint32_t beg, end, dsz, isz, p, lab1, b1;
    gv2(RC_INT, RC_INT);
    vpushi(0);
    vtop->r = get_reg(RC_INT);
    vpushi(0);
    vtop->r = get_reg(RC_INT);
    vpushi(0);
    vtop->r = get_reg(RC_INT);
    beg = intr(vtop[-4].r); // x0
    end = intr(vtop[-3].r); // x1
    dsz = intr(vtop[-2].r); // x2
    isz = intr(vtop[-1].r); // x3
    p = intr(vtop[0].r);    // x4
    vtop -= 5;

    o(0xd53b0020 | isz); // mrs x(isz),ctr_el0
    o(0x52800080 | p); // mov w(p),#4
    o(0x53104c00 | dsz | isz << 5); // ubfx w(dsz),w(isz),#16,#4
    o(0x1ac02000 | dsz | p << 5 | dsz << 16); // lsl w(dsz),w(p),w(dsz)
    o(0x12000c00 | isz | isz << 5); // and w(isz),w(isz),#15
    o(0x1ac02000 | isz | p << 5 | isz << 16); // lsl w(isz),w(p),w(isz)
    o(0x51000400 | p | dsz << 5); // sub w(p),w(dsz),#1
    o(0x8a240004 | p | beg << 5 | p << 16); // bic x(p),x(beg),x(p)
    b1 = ind; o(0x14000000); // b
    lab1 = ind;
    o(0xd50b7b20 | p); // dc cvau,x(p)
    o(0x8b000000 | p | p << 5 | dsz << 16); // add x(p),x(p),x(dsz)
    write32le(cur_text_section->data + b1, 0x14000000 | (ind - b1) >> 2);
    o(0xeb00001f | p << 5 | end << 16); // cmp x(p),x(end)
    o(0x54ffffa3 | ((lab1 - ind) << 3 & 0xffffe0)); // b.cc lab1
    o(0xd5033b9f); // dsb ish
    o(0x51000400 | p | isz << 5); // sub w(p),w(isz),#1
    o(0x8a240004 | p | beg << 5 | p << 16); // bic x(p),x(beg),x(p)
    b1 = ind; o(0x14000000); // b
    lab1 = ind;
    o(0xd50b7520 | p); // ic ivau,x(p)
    o(0x8b000000 | p | p << 5 | isz << 16); // add x(p),x(p),x(isz)
    write32le(cur_text_section->data + b1, 0x14000000 | (ind - b1) >> 2);
    o(0xeb00001f | p << 5 | end << 16); // cmp x(p),x(end)
    o(0x54ffffa3 | ((lab1 - ind) << 3 & 0xffffe0)); // b.cc lab1
    o(0xd5033b9f); // dsb ish
    o(0xd5033fdf); // isb
}

ST_FUNC 空 gen_vla_sp_save(整 addr) {
    uint32_t r = intr(get_reg(RC_INT));
    o(0x910003e0 | r); // mov x(r),sp
    arm64_strx(3, r, 29, addr);
}

ST_FUNC 空 gen_vla_sp_restore(整 addr) {
    // Use x30 because this function can be called when there
    // is a live return value in x0 but there is nothing on
    // the value stack to prevent get_reg from returning x0.
    uint32_t r = 30;
    arm64_ldrx(0, 3, r, 29, addr);
    o(0x9100001f | r << 5); // mov sp,x(r)
}

ST_FUNC 空 gen_vla_alloc(CType *type, 整 align) {
    uint32_t r = intr(gv(RC_INT));
    o(0x91003c00 | r | r << 5); // add x(r),x(r),#15
    o(0x927cec00 | r | r << 5); // bic x(r),x(r),#15
    o(0xcb2063ff | r << 16); // sub sp,sp,x(r)
    vpop();
}

/* end of A64 code generator */
/*************************************************************/
#了如
/*************************************************************/
