/*
 *  x86-64 code generator for TCC
 *
 *  Copyright (c) 2008 Shinichiro Hamaji
 *
 *  Based on i386-gen.c by Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#如定义 TARGET_DEFS_ONLY

/* number of available registers */
#定义 NB_REGS         25
#定义 NB_ASM_REGS     16
#定义 CONFIG_TCC_ASM

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#定义 RC_INT     0x0001 /* generic integer register */
#定义 RC_FLOAT   0x0002 /* generic float register */
#定义 RC_RAX     0x0004
#定义 RC_RCX     0x0008
#定义 RC_RDX     0x0010
#定义 RC_ST0     0x0080 /* only for long double */
#定义 RC_R8      0x0100
#定义 RC_R9      0x0200
#定义 RC_R10     0x0400
#定义 RC_R11     0x0800
#定义 RC_XMM0    0x1000
#定义 RC_XMM1    0x2000
#定义 RC_XMM2    0x4000
#定义 RC_XMM3    0x8000
#定义 RC_XMM4    0x10000
#定义 RC_XMM5    0x20000
#定义 RC_XMM6    0x40000
#定义 RC_XMM7    0x80000
#定义 RC_IRET    RC_RAX /* function return: integer register */
#定义 RC_LRET    RC_RDX /* function return: second integer register */
#定义 RC_FRET    RC_XMM0 /* function return: float register */
#定义 RC_QRET    RC_XMM1 /* function return: second float register */

/* pretty names for the registers */
枚举 {
    TREG_RAX = 0,
    TREG_RCX = 1,
    TREG_RDX = 2,
    TREG_RSP = 4,
    TREG_RSI = 6,
    TREG_RDI = 7,

    TREG_R8  = 8,
    TREG_R9  = 9,
    TREG_R10 = 10,
    TREG_R11 = 11,

    TREG_XMM0 = 16,
    TREG_XMM1 = 17,
    TREG_XMM2 = 18,
    TREG_XMM3 = 19,
    TREG_XMM4 = 20,
    TREG_XMM5 = 21,
    TREG_XMM6 = 22,
    TREG_XMM7 = 23,

    TREG_ST0 = 24,

    TREG_MEM = 0x20
};

#定义 REX_BASE(reg) (((reg) >> 3) & 1)
#定义 REG_VALUE(reg) ((reg) & 7)

/* return registers for function */
#定义 REG_IRET TREG_RAX /* single word int return register */
#定义 REG_LRET TREG_RDX /* second word return register (for long long) */
#定义 REG_FRET TREG_XMM0 /* float return register */
#定义 REG_QRET TREG_XMM1 /* second float return register */

/* defined if function parameters must be evaluated in reverse order */
#定义 INVERT_FUNC_PARAMS

/* pointer size, in bytes */
#定义 PTR_SIZE 8

/* long double size and alignment, in bytes */
#定义 LDOUBLE_SIZE  16
#定义 LDOUBLE_ALIGN 16
/* maximum alignment (for aligned attribute support) */
#定义 MAX_ALIGN     16

/******************************************************/
#另 /* ! TARGET_DEFS_ONLY */
/******************************************************/
#包含 "tcc.h"
#包含 <assert.h>

ST_DATA 不变 整 reg_classes[NB_REGS] = {
    /* eax */ RC_INT | RC_RAX,
    /* ecx */ RC_INT | RC_RCX,
    /* edx */ RC_INT | RC_RDX,
    0,
    0,
    0,
    0,
    0,
    RC_R8,
    RC_R9,
    RC_R10,
    RC_R11,
    0,
    0,
    0,
    0,
    /* xmm0 */ RC_FLOAT | RC_XMM0,
    /* xmm1 */ RC_FLOAT | RC_XMM1,
    /* xmm2 */ RC_FLOAT | RC_XMM2,
    /* xmm3 */ RC_FLOAT | RC_XMM3,
    /* xmm4 */ RC_FLOAT | RC_XMM4,
    /* xmm5 */ RC_FLOAT | RC_XMM5,
    /* xmm6 an xmm7 are included so gv() can be used on them,
       but they are not tagged with RC_FLOAT because they are
       callee saved on Windows */
    RC_XMM6,
    RC_XMM7,
    /* st0 */ RC_ST0
};

静态 无符 长 func_sub_sp_offset;
静态 整 func_ret_sub;

/* XXX: make it faster ? */
ST_FUNC 空 g(整 c)
{
    整 ind1;
    如 (nocode_wanted)
        返回;
    ind1 = ind + 1;
    如 (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind] = c;
    ind = ind1;
}

ST_FUNC 空 o(无符 整 c)
{
    当 (c) {
        g(c);
        c = c >> 8;
    }
}

ST_FUNC 空 gen_le16(整 v)
{
    g(v);
    g(v >> 8);
}

ST_FUNC 空 gen_le32(整 c)
{
    g(c);
    g(c >> 8);
    g(c >> 16);
    g(c >> 24);
}

ST_FUNC 空 gen_le64(int64_t c)
{
    g(c);
    g(c >> 8);
    g(c >> 16);
    g(c >> 24);
    g(c >> 32);
    g(c >> 40);
    g(c >> 48);
    g(c >> 56);
}

静态 空 orex(整 ll, 整 r, 整 r2, 整 b)
{
    如 ((r & VT_VALMASK) >= VT_CONST)
        r = 0;
    如 ((r2 & VT_VALMASK) >= VT_CONST)
        r2 = 0;
    如 (ll || REX_BASE(r) || REX_BASE(r2))
        o(0x40 | REX_BASE(r) | (REX_BASE(r2) << 2) | (ll << 3));
    o(b);
}

/* output a symbol and patch all calls to it */
ST_FUNC 空 gsym_addr(整 t, 整 a)
{
    当 (t) {
        无符 字 *ptr = cur_text_section->data + t;
        uint32_t n = read32le(ptr); /* next value */
        write32le(ptr, a - t - 4);
        t = n;
    }
}

空 gsym(整 t)
{
    gsym_addr(t, ind);
}


静态 整 is64_type(整 t)
{
    返回 ((t & VT_BTYPE) == VT_PTR ||
            (t & VT_BTYPE) == VT_FUNC ||
            (t & VT_BTYPE) == VT_LLONG);
}

/* instruction + 4 bytes data. Return the address of the data */
静态 整 oad(整 c, 整 s)
{
    整 t;
    如 (nocode_wanted)
        返回 s;
    o(c);
    t = ind;
    gen_le32(s);
    返回 t;
}

/* generate jmp to a label */
#定义 gjmp2(instr,lbl) oad(instr,lbl)

ST_FUNC 空 gen_addr32(整 r, Sym *sym, 整 c)
{
    如 (r & VT_SYM)
        greloca(cur_text_section, sym, ind, R_X86_64_32S, c), c=0;
    gen_le32(c);
}

/* output constant with relocation if 'r & VT_SYM' is true */
ST_FUNC 空 gen_addr64(整 r, Sym *sym, int64_t c)
{
    如 (r & VT_SYM)
        greloca(cur_text_section, sym, ind, R_X86_64_64, c), c=0;
    gen_le64(c);
}

/* output constant with relocation if 'r & VT_SYM' is true */
ST_FUNC 空 gen_addrpc32(整 r, Sym *sym, 整 c)
{
    如 (r & VT_SYM)
        greloca(cur_text_section, sym, ind, R_X86_64_PC32, c-4), c=4;
    gen_le32(c-4);
}

/* output got address with relocation */
静态 空 gen_gotpcrel(整 r, Sym *sym, 整 c)
{
#如定义 TCC_TARGET_PE
    tcc_error("internal error: no GOT on PE: %s %x %x | %02x %02x %02x\n",
        get_tok_str(sym->v, NULL), c, r,
        cur_text_section->data[ind-3],
        cur_text_section->data[ind-2],
        cur_text_section->data[ind-1]
        );
#了如
    greloca(cur_text_section, sym, ind, R_X86_64_GOTPCREL, -4);
    gen_le32(0);
    如 (c) {
        /* we use add c, %xxx for displacement */
        orex(1, r, 0, 0x81);
        o(0xc0 + REG_VALUE(r));
        gen_le32(c);
    }
}

静态 空 gen_modrm_impl(整 op_reg, 整 r, Sym *sym, 整 c, 整 is_got)
{
    op_reg = REG_VALUE(op_reg) << 3;
    如 ((r & VT_VALMASK) == VT_CONST) {
        /* constant memory reference */
        o(0x05 | op_reg);
        如 (is_got) {
            gen_gotpcrel(r, sym, c);
        } 另 {
            gen_addrpc32(r, sym, c);
        }
    } 另 如 ((r & VT_VALMASK) == VT_LOCAL) {
        /* currently, we use only ebp as base */
        如 (c == (字)c) {
            /* short reference */
            o(0x45 | op_reg);
            g(c);
        } 另 {
            oad(0x85 | op_reg, c);
        }
    } 另 如 ((r & VT_VALMASK) >= TREG_MEM) {
        如 (c) {
            g(0x80 | op_reg | REG_VALUE(r));
            gen_le32(c);
        } 另 {
            g(0x00 | op_reg | REG_VALUE(r));
        }
    } 另 {
        g(0x00 | op_reg | REG_VALUE(r));
    }
}

/* generate a modrm reference. 'op_reg' contains the addtional 3
   opcode bits */
静态 空 gen_modrm(整 op_reg, 整 r, Sym *sym, 整 c)
{
    gen_modrm_impl(op_reg, r, sym, c, 0);
}

/* generate a modrm reference. 'op_reg' contains the addtional 3
   opcode bits */
静态 空 gen_modrm64(整 opcode, 整 op_reg, 整 r, Sym *sym, 整 c)
{
    整 is_got;
    is_got = (op_reg & TREG_MEM) && !(sym->type.t & VT_STATIC);
    orex(1, r, op_reg, opcode);
    gen_modrm_impl(op_reg, r, sym, c, is_got);
}


/* load 'r' from value 'sv' */
空 load(整 r, SValue *sv)
{
    整 v, t, ft, fc, fr;
    SValue v1;

#如定义 TCC_TARGET_PE
    SValue v2;
    sv = pe_getimport(sv, &v2);
#了如

    fr = sv->r;
    ft = sv->type.t & ~VT_DEFSIGN;
    fc = sv->c.i;
    如 (fc != sv->c.i && (fr & VT_SYM))
      tcc_error("64 bit addend in load");

    ft &= ~(VT_VOLATILE | VT_CONSTANT);

#如未定义 TCC_TARGET_PE
    /* we use indirect access via got */
    如 ((fr & VT_VALMASK) == VT_CONST && (fr & VT_SYM) &&
        (fr & VT_LVAL) && !(sv->sym->type.t & VT_STATIC)) {
        /* use the result register as a temporal register */
        整 tr = r | TREG_MEM;
        如 (is_float(ft)) {
            /* we cannot use float registers as a temporal register */
            tr = get_reg(RC_INT) | TREG_MEM;
        }
        gen_modrm64(0x8b, tr, fr, sv->sym, 0);

        /* load from the temporal register */
        fr = tr | VT_LVAL;
    }
#了如

    v = fr & VT_VALMASK;
    如 (fr & VT_LVAL) {
        整 b, ll;
        如 (v == VT_LLOCAL) {
            v1.type.t = VT_PTR;
            v1.r = VT_LOCAL | VT_LVAL;
            v1.c.i = fc;
            fr = r;
            如 (!(reg_classes[fr] & (RC_INT|RC_R11)))
                fr = get_reg(RC_INT);
            load(fr, &v1);
        }
        ll = 0;
        /* Like GCC we can load from small enough properly sized
           structs and unions as well.
           XXX maybe move to generic operand handling, but should
           occur only with asm, so tccasm.c might also be a better place */
        如 ((ft & VT_BTYPE) == VT_STRUCT) {
            整 align;
            转接 (type_size(&sv->type, &align)) {
                事例 1: ft = VT_BYTE; 跳出;
                事例 2: ft = VT_SHORT; 跳出;
                事例 4: ft = VT_INT; 跳出;
                事例 8: ft = VT_LLONG; 跳出;
                缺省:
                    tcc_error("invalid aggregate type for register load");
                    跳出;
            }
        }
        如 ((ft & VT_BTYPE) == VT_FLOAT) {
            b = 0x6e0f66;
            r = REG_VALUE(r); /* movd */
        } 另 如 ((ft & VT_BTYPE) == VT_DOUBLE) {
            b = 0x7e0ff3; /* movq */
            r = REG_VALUE(r);
        } 另 如 ((ft & VT_BTYPE) == VT_LDOUBLE) {
            b = 0xdb, r = 5; /* fldt */
        } 另 如 ((ft & VT_TYPE) == VT_BYTE || (ft & VT_TYPE) == VT_BOOL) {
            b = 0xbe0f;   /* movsbl */
        } 另 如 ((ft & VT_TYPE) == (VT_BYTE | VT_UNSIGNED)) {
            b = 0xb60f;   /* movzbl */
        } 另 如 ((ft & VT_TYPE) == VT_SHORT) {
            b = 0xbf0f;   /* movswl */
        } 另 如 ((ft & VT_TYPE) == (VT_SHORT | VT_UNSIGNED)) {
            b = 0xb70f;   /* movzwl */
        } 另 {
            assert(((ft & VT_BTYPE) == VT_INT)
                   || ((ft & VT_BTYPE) == VT_LLONG)
                   || ((ft & VT_BTYPE) == VT_PTR)
                   || ((ft & VT_BTYPE) == VT_FUNC)
                );
            ll = is64_type(ft);
            b = 0x8b;
        }
        如 (ll) {
            gen_modrm64(b, r, fr, sv->sym, fc);
        } 另 {
            orex(ll, fr, r, b);
            gen_modrm(r, fr, sv->sym, fc);
        }
    } 另 {
        如 (v == VT_CONST) {
            如 (fr & VT_SYM) {
#如定义 TCC_TARGET_PE
                orex(1,0,r,0x8d);
                o(0x05 + REG_VALUE(r) * 8); /* lea xx(%rip), r */
                gen_addrpc32(fr, sv->sym, fc);
#另
                如 (sv->sym->type.t & VT_STATIC) {
                    orex(1,0,r,0x8d);
                    o(0x05 + REG_VALUE(r) * 8); /* lea xx(%rip), r */
                    gen_addrpc32(fr, sv->sym, fc);
                } 另 {
                    orex(1,0,r,0x8b);
                    o(0x05 + REG_VALUE(r) * 8); /* mov xx(%rip), r */
                    gen_gotpcrel(r, sv->sym, fc);
                }
#了如
            } 另 如 (is64_type(ft)) {
                orex(1,r,0, 0xb8 + REG_VALUE(r)); /* mov $xx, r */
                gen_le64(sv->c.i);
            } 另 {
                orex(0,r,0, 0xb8 + REG_VALUE(r)); /* mov $xx, r */
                gen_le32(fc);
            }
        } 另 如 (v == VT_LOCAL) {
            orex(1,0,r,0x8d); /* lea xxx(%ebp), r */
            gen_modrm(r, VT_LOCAL, sv->sym, fc);
        } 另 如 (v == VT_CMP) {
            orex(0,r,0,0);
            如 ((fc & ~0x100) != TOK_NE)
              oad(0xb8 + REG_VALUE(r), 0); /* mov $0, r */
            另
              oad(0xb8 + REG_VALUE(r), 1); /* mov $1, r */
            如 (fc & 0x100)
              {
                /* This was a float compare.  If the parity bit is
                   set the result was unordered, meaning false for everything
                   except TOK_NE, and true for TOK_NE.  */
                fc &= ~0x100;
                o(0x037a + (REX_BASE(r) << 8));
              }
            orex(0,r,0, 0x0f); /* setxx %br */
            o(fc);
            o(0xc0 + REG_VALUE(r));
        } 另 如 (v == VT_JMP || v == VT_JMPI) {
            t = v & 1;
            orex(0,r,0,0);
            oad(0xb8 + REG_VALUE(r), t); /* mov $1, r */
            o(0x05eb + (REX_BASE(r) << 8)); /* jmp after */
            gsym(fc);
            orex(0,r,0,0);
            oad(0xb8 + REG_VALUE(r), t ^ 1); /* mov $0, r */
        } 另 如 (v != r) {
            如 ((r >= TREG_XMM0) && (r <= TREG_XMM7)) {
                如 (v == TREG_ST0) {
                    /* gen_cvt_ftof(VT_DOUBLE); */
                    o(0xf0245cdd); /* fstpl -0x10(%rsp) */
                    /* movsd -0x10(%rsp),%xmmN */
                    o(0x100ff2);
                    o(0x44 + REG_VALUE(r)*8); /* %xmmN */
                    o(0xf024);
                } 另 {
                    assert((v >= TREG_XMM0) && (v <= TREG_XMM7));
                    如 ((ft & VT_BTYPE) == VT_FLOAT) {
                        o(0x100ff3);
                    } 另 {
                        assert((ft & VT_BTYPE) == VT_DOUBLE);
                        o(0x100ff2);
                    }
                    o(0xc0 + REG_VALUE(v) + REG_VALUE(r)*8);
                }
            } 另 如 (r == TREG_ST0) {
                assert((v >= TREG_XMM0) && (v <= TREG_XMM7));
                /* gen_cvt_ftof(VT_LDOUBLE); */
                /* movsd %xmmN,-0x10(%rsp) */
                o(0x110ff2);
                o(0x44 + REG_VALUE(r)*8); /* %xmmN */
                o(0xf024);
                o(0xf02444dd); /* fldl -0x10(%rsp) */
            } 另 {
                orex(1,r,v, 0x89);
                o(0xc0 + REG_VALUE(r) + REG_VALUE(v) * 8); /* mov v, r */
            }
        }
    }
}

/* store register 'r' in lvalue 'v' */
空 store(整 r, SValue *v)
{
    整 fr, bt, ft, fc;
    整 op64 = 0;
    /* store the REX prefix in this variable when PIC is enabled */
    整 pic = 0;

#如定义 TCC_TARGET_PE
    SValue v2;
    v = pe_getimport(v, &v2);
#了如

    fr = v->r & VT_VALMASK;
    ft = v->type.t;
    fc = v->c.i;
    如 (fc != v->c.i && (fr & VT_SYM))
      tcc_error("64 bit addend in store");
    ft &= ~(VT_VOLATILE | VT_CONSTANT);
    bt = ft & VT_BTYPE;

#如未定义 TCC_TARGET_PE
    /* we need to access the variable via got */
    如 (fr == VT_CONST && (v->r & VT_SYM)) {
        /* mov xx(%rip), %r11 */
        o(0x1d8b4c);
        gen_gotpcrel(TREG_R11, v->sym, v->c.i);
        pic = is64_type(bt) ? 0x49 : 0x41;
    }
#了如

    /* XXX: incorrect if float reg to reg */
    如 (bt == VT_FLOAT) {
        o(0x66);
        o(pic);
        o(0x7e0f); /* movd */
        r = REG_VALUE(r);
    } 另 如 (bt == VT_DOUBLE) {
        o(0x66);
        o(pic);
        o(0xd60f); /* movq */
        r = REG_VALUE(r);
    } 另 如 (bt == VT_LDOUBLE) {
        o(0xc0d9); /* fld %st(0) */
        o(pic);
        o(0xdb); /* fstpt */
        r = 7;
    } 另 {
        如 (bt == VT_SHORT)
            o(0x66);
        o(pic);
        如 (bt == VT_BYTE || bt == VT_BOOL)
            orex(0, 0, r, 0x88);
        另 如 (is64_type(bt))
            op64 = 0x89;
        另
            orex(0, 0, r, 0x89);
    }
    如 (pic) {
        /* xxx r, (%r11) where xxx is mov, movq, fld, or etc */
        如 (op64)
            o(op64);
        o(3 + (r << 3));
    } 另 如 (op64) {
        如 (fr == VT_CONST || fr == VT_LOCAL || (v->r & VT_LVAL)) {
            gen_modrm64(op64, r, v->r, v->sym, fc);
        } 另 如 (fr != r) {
            /* XXX: don't we really come here? */
            abort();
            o(0xc0 + fr + r * 8); /* mov r, fr */
        }
    } 另 {
        如 (fr == VT_CONST || fr == VT_LOCAL || (v->r & VT_LVAL)) {
            gen_modrm(r, v->r, v->sym, fc);
        } 另 如 (fr != r) {
            /* XXX: don't we really come here? */
            abort();
            o(0xc0 + fr + r * 8); /* mov r, fr */
        }
    }
}

/* 'is_jmp' is '1' if it is a jump */
静态 空 gcall_or_jmp(整 is_jmp)
{
    整 r;
    如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
        ((vtop->r & VT_SYM) || (vtop->c.i-4) == (整)(vtop->c.i-4))) {
        /* constant case */
        如 (vtop->r & VT_SYM) {
            /* relocation case */
#如定义 TCC_TARGET_PE
            greloca(cur_text_section, vtop->sym, ind + 1, R_X86_64_PC32, (整)(vtop->c.i-4));
#另
            greloca(cur_text_section, vtop->sym, ind + 1, R_X86_64_PLT32, (整)(vtop->c.i-4));
#了如
        } 另 {
            /* put an empty PC32 relocation */
            put_elf_reloca(symtab_section, cur_text_section,
                          ind + 1, R_X86_64_PC32, 0, (整)(vtop->c.i-4));
        }
        oad(0xe8 + is_jmp, 0); /* call/jmp im */
    } 另 {
        /* otherwise, indirect call */
        r = TREG_R11;
        load(r, vtop);
        o(0x41); /* REX */
        o(0xff); /* call/jmp *r */
        o(0xd0 + REG_VALUE(r) + (is_jmp << 4));
    }
}

#如 已定义(CONFIG_TCC_BCHECK)
#如未定义 TCC_TARGET_PE
静态 addr_t func_bound_offset;
静态 无符 长 func_bound_ind;
#了如

静态 空 gen_static_call(整 v)
{
    Sym *sym = external_global_sym(v, &func_old_type, 0);
    oad(0xe8, 0);
    greloca(cur_text_section, sym, ind-4, R_X86_64_PC32, -4);
}

/* generate a bounded pointer addition */
ST_FUNC 空 gen_bounded_ptr_add(空)
{
    /* save all temporary registers */
    save_regs(0);

    /* prepare fast x86_64 function call */
    gv(RC_RAX);
    o(0xc68948); // mov  %rax,%rsi ## second arg in %rsi, this must be size
    vtop--;

    gv(RC_RAX);
    o(0xc78948); // mov  %rax,%rdi ## first arg in %rdi, this must be ptr
    vtop--;

    /* do a fast function call */
    gen_static_call(TOK___bound_ptr_add);

    /* returned pointer is in rax */
    vtop++;
    vtop->r = TREG_RAX | VT_BOUNDED;


    /* relocation offset of the bounding function call point */
    vtop->c.i = (cur_text_section->reloc->data_offset - 求长度(ElfW(Rela)));
}

/* patch pointer addition in vtop so that pointer dereferencing is
   also tested */
ST_FUNC 空 gen_bounded_ptr_deref(空)
{
    addr_t func;
    整 size, align;
    ElfW(Rela) *rel;
    Sym *sym;

    size = 0;
    /* XXX: put that code in generic part of tcc */
    如 (!is_float(vtop->type.t)) {
        如 (vtop->r & VT_LVAL_BYTE)
            size = 1;
        另 如 (vtop->r & VT_LVAL_SHORT)
            size = 2;
    }
    如 (!size)
    size = type_size(&vtop->type, &align);
    转接(size) {
    事例  1: func = TOK___bound_ptr_indir1; 跳出;
    事例  2: func = TOK___bound_ptr_indir2; 跳出;
    事例  4: func = TOK___bound_ptr_indir4; 跳出;
    事例  8: func = TOK___bound_ptr_indir8; 跳出;
    事例 12: func = TOK___bound_ptr_indir12; 跳出;
    事例 16: func = TOK___bound_ptr_indir16; 跳出;
    缺省:
        tcc_error("unhandled size when dereferencing bounded pointer");
        func = 0;
        跳出;
    }

    sym = external_global_sym(func, &func_old_type, 0);
    如 (!sym->c)
        put_extern_sym(sym, NULL, 0, 0);

    /* patch relocation */
    /* XXX: find a better solution ? */

    rel = (ElfW(Rela) *)(cur_text_section->reloc->data + vtop->c.i);
    rel->r_info = ELF64_R_INFO(sym->c, ELF64_R_TYPE(rel->r_info));
}
#了如

#如定义 TCC_TARGET_PE

#定义 REGN 4
静态 不变 uint8_t arg_regs[REGN] = {
    TREG_RCX, TREG_RDX, TREG_R8, TREG_R9
};

/* Prepare arguments in R10 and R11 rather than RCX and RDX
   because gv() will not ever use these */
静态 整 arg_prepare_reg(整 idx) {
  如 (idx == 0 || idx == 1)
      /* idx=0: r10, idx=1: r11 */
      返回 idx + 10;
  另
      返回 arg_regs[idx];
}

静态 整 func_scratch, func_alloca;

/* Generate function call. The function address is pushed first, then
   all the parameters in call order. This functions pops all the
   parameters and the function address. */

静态 空 gen_offs_sp(整 b, 整 r, 整 d)
{
    orex(1,0,r & 0x100 ? 0 : r, b);
    如 (d == (字)d) {
        o(0x2444 | (REG_VALUE(r) << 3));
        g(d);
    } 另 {
        o(0x2484 | (REG_VALUE(r) << 3));
        gen_le32(d);
    }
}

静态 整 using_regs(整 size)
{
    返回 !(size > 8 || (size & (size - 1)));
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret, 整 *ret_align, 整 *regsize)
{
    整 size, align;
    *ret_align = 1; // Never have to re-align return values for x86-64
    *regsize = 8;
    size = type_size(vt, &align);
    如 (!using_regs(size))
        返回 0;
    如 (size == 8)
        ret->t = VT_LLONG;
    另 如 (size == 4)
        ret->t = VT_INT;
    另 如 (size == 2)
        ret->t = VT_SHORT;
    另
        ret->t = VT_BYTE;
    ret->ref = NULL;
    返回 1;
}

静态 整 is_sse_float(整 t) {
    整 bt;
    bt = t & VT_BTYPE;
    返回 bt == VT_DOUBLE || bt == VT_FLOAT;
}

静态 整 gfunc_arg_size(CType *type) {
    整 align;
    如 (type->t & (VT_ARRAY|VT_BITFIELD))
        返回 8;
    返回 type_size(type, &align);
}

空 gfunc_call(整 nb_args)
{
    整 size, r, args_size, i, d, bt, struct_size;
    整 arg;

    args_size = (nb_args < REGN ? REGN : nb_args) * PTR_SIZE;
    arg = nb_args;

    /* for struct arguments, we need to call memcpy and the function
       call breaks register passing arguments we are preparing.
       So, we process arguments which will be passed by stack first. */
    struct_size = args_size;
    对于(i = 0; i < nb_args; i++) {
        SValue *sv;
        
        --arg;
        sv = &vtop[-i];
        bt = (sv->type.t & VT_BTYPE);
        size = gfunc_arg_size(&sv->type);

        如 (using_regs(size))
            继续; /* arguments smaller than 8 bytes passed in registers or on stack */

        如 (bt == VT_STRUCT) {
            /* align to stack align size */
            size = (size + 15) & ~15;
            /* generate structure store */
            r = get_reg(RC_INT);
            gen_offs_sp(0x8d, r, struct_size);
            struct_size += size;

            /* generate memcpy call */
            vset(&sv->type, r | VT_LVAL, 0);
            vpushv(sv);
            vstore();
            --vtop;
        } 另 如 (bt == VT_LDOUBLE) {
            gv(RC_ST0);
            gen_offs_sp(0xdb, 0x107, struct_size);
            struct_size += 16;
        }
    }

    如 (func_scratch < struct_size)
        func_scratch = struct_size;

    arg = nb_args;
    struct_size = args_size;

    对于(i = 0; i < nb_args; i++) {
        --arg;
        bt = (vtop->type.t & VT_BTYPE);

        size = gfunc_arg_size(&vtop->type);
        如 (!using_regs(size)) {
            /* align to stack align size */
            size = (size + 15) & ~15;
            如 (arg >= REGN) {
                d = get_reg(RC_INT);
                gen_offs_sp(0x8d, d, struct_size);
                gen_offs_sp(0x89, d, arg*8);
            } 另 {
                d = arg_prepare_reg(arg);
                gen_offs_sp(0x8d, d, struct_size);
            }
            struct_size += size;
        } 另 {
            如 (is_sse_float(vtop->type.t)) {
                如 (tcc_state->nosse)
                  tcc_error("SSE disabled");
                gv(RC_XMM0); /* only use one float register */
                如 (arg >= REGN) {
                    /* movq %xmm0, j*8(%rsp) */
                    gen_offs_sp(0xd60f66, 0x100, arg*8);
                } 另 {
                    /* movaps %xmm0, %xmmN */
                    o(0x280f);
                    o(0xc0 + (arg << 3));
                    d = arg_prepare_reg(arg);
                    /* mov %xmm0, %rxx */
                    o(0x66);
                    orex(1,d,0, 0x7e0f);
                    o(0xc0 + REG_VALUE(d));
                }
            } 另 {
                如 (bt == VT_STRUCT) {
                    vtop->type.ref = NULL;
                    vtop->type.t = size > 4 ? VT_LLONG : size > 2 ? VT_INT
                        : size > 1 ? VT_SHORT : VT_BYTE;
                }
                
                r = gv(RC_INT);
                如 (arg >= REGN) {
                    gen_offs_sp(0x89, r, arg*8);
                } 另 {
                    d = arg_prepare_reg(arg);
                    orex(1,d,r,0x89); /* mov */
                    o(0xc0 + REG_VALUE(r) * 8 + REG_VALUE(d));
                }
            }
        }
        vtop--;
    }
    save_regs(0);
    
    /* Copy R10 and R11 into RCX and RDX, respectively */
    如 (nb_args > 0) {
        o(0xd1894c); /* mov %r10, %rcx */
        如 (nb_args > 1) {
            o(0xda894c); /* mov %r11, %rdx */
        }
    }
    
    gcall_or_jmp(0);

    如 ((vtop->r & VT_SYM) && vtop->sym->v == TOK_alloca) {
        /* need to add the "func_scratch" area after alloca */
        o(0x0548), gen_le32(func_alloca), func_alloca = ind - 4;
    }

    /* other compilers don't clear the upper bits when returning char/short */
    bt = vtop->type.ref->type.t & (VT_BTYPE | VT_UNSIGNED);
    如 (bt == (VT_BYTE | VT_UNSIGNED))
        o(0xc0b60f);  /* movzbl %al, %eax */
    另 如 (bt == VT_BYTE)
        o(0xc0be0f); /* movsbl %al, %eax */
    另 如 (bt == VT_SHORT)
        o(0x98); /* cwtl */
    另 如 (bt == (VT_SHORT | VT_UNSIGNED))
        o(0xc0b70f);  /* movzbl %al, %eax */
#如 0 /* handled in gen_cast() */
    另 如 (bt == VT_INT)
        o(0x9848); /* cltq */
    另 如 (bt == (VT_INT | VT_UNSIGNED))
        o(0xc089); /* mov %eax,%eax */
#了如
    vtop--;
}


#定义 FUNC_PROLOG_SIZE 11

/* generate function prolog of type 't' */
空 gfunc_prolog(CType *func_type)
{
    整 addr, reg_param_index, bt, size;
    Sym *sym;
    CType *type;

    func_ret_sub = 0;
    func_scratch = 0;
    func_alloca = 0;
    loc = 0;

    addr = PTR_SIZE * 2;
    ind += FUNC_PROLOG_SIZE;
    func_sub_sp_offset = ind;
    reg_param_index = 0;

    sym = func_type->ref;

    /* if the function returns a structure, then add an
       implicit pointer parameter */
    func_vt = sym->type;
    func_var = (sym->f.func_type == FUNC_ELLIPSIS);
    size = gfunc_arg_size(&func_vt);
    如 (!using_regs(size)) {
        gen_modrm64(0x89, arg_regs[reg_param_index], VT_LOCAL, NULL, addr);
        func_vc = addr;
        reg_param_index++;
        addr += 8;
    }

    /* define parameters */
    当 ((sym = sym->next) != NULL) {
        type = &sym->type;
        bt = type->t & VT_BTYPE;
        size = gfunc_arg_size(type);
        如 (!using_regs(size)) {
            如 (reg_param_index < REGN) {
                gen_modrm64(0x89, arg_regs[reg_param_index], VT_LOCAL, NULL, addr);
            }
            sym_push(sym->v & ~SYM_FIELD, type, VT_LLOCAL | VT_LVAL, addr);
        } 另 {
            如 (reg_param_index < REGN) {
                /* save arguments passed by register */
                如 ((bt == VT_FLOAT) || (bt == VT_DOUBLE)) {
                    如 (tcc_state->nosse)
                      tcc_error("SSE disabled");
                    o(0xd60f66); /* movq */
                    gen_modrm(reg_param_index, VT_LOCAL, NULL, addr);
                } 另 {
                    gen_modrm64(0x89, arg_regs[reg_param_index], VT_LOCAL, NULL, addr);
                }
            }
            sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | VT_LVAL, addr);
        }
        addr += 8;
        reg_param_index++;
    }

    当 (reg_param_index < REGN) {
        如 (func_type->ref->f.func_type == FUNC_ELLIPSIS) {
            gen_modrm64(0x89, arg_regs[reg_param_index], VT_LOCAL, NULL, addr);
            addr += 8;
        }
        reg_param_index++;
    }
}

/* generate function epilog */
空 gfunc_epilog(空)
{
    整 v, saved_ind;

    o(0xc9); /* leave */
    如 (func_ret_sub == 0) {
        o(0xc3); /* ret */
    } 另 {
        o(0xc2); /* ret n */
        g(func_ret_sub);
        g(func_ret_sub >> 8);
    }

    saved_ind = ind;
    ind = func_sub_sp_offset - FUNC_PROLOG_SIZE;
    /* align local size to word & save local variables */
    v = (func_scratch + -loc + 15) & -16;

    如 (v >= 4096) {
        Sym *sym = external_global_sym(TOK___chkstk, &func_old_type, 0);
        oad(0xb8, v); /* mov stacksize, %eax */
        oad(0xe8, 0); /* call __chkstk, (does the stackframe too) */
        greloca(cur_text_section, sym, ind-4, R_X86_64_PC32, -4);
        o(0x90); /* fill for FUNC_PROLOG_SIZE = 11 bytes */
    } 另 {
        o(0xe5894855);  /* push %rbp, mov %rsp, %rbp */
        o(0xec8148);  /* sub rsp, stacksize */
        gen_le32(v);
    }

    /* add the "func_scratch" area after each alloca seen */
    当 (func_alloca) {
        无符 字 *ptr = cur_text_section->data + func_alloca;
        func_alloca = read32le(ptr);
        write32le(ptr, func_scratch);
    }

    cur_text_section->data_offset = saved_ind;
    pe_add_unwind_data(ind, saved_ind, v);
    ind = cur_text_section->data_offset;
}

#另

静态 空 gadd_sp(整 val)
{
    如 (val == (字)val) {
        o(0xc48348);
        g(val);
    } 另 {
        oad(0xc48148, val); /* add $xxx, %rsp */
    }
}

类型定义 枚举 X86_64_Mode {
  x86_64_mode_none,
  x86_64_mode_memory,
  x86_64_mode_integer,
  x86_64_mode_sse,
  x86_64_mode_x87
} X86_64_Mode;

静态 X86_64_Mode classify_x86_64_merge(X86_64_Mode a, X86_64_Mode b)
{
    如 (a == b)
        返回 a;
    另 如 (a == x86_64_mode_none)
        返回 b;
    另 如 (b == x86_64_mode_none)
        返回 a;
    另 如 ((a == x86_64_mode_memory) || (b == x86_64_mode_memory))
        返回 x86_64_mode_memory;
    另 如 ((a == x86_64_mode_integer) || (b == x86_64_mode_integer))
        返回 x86_64_mode_integer;
    另 如 ((a == x86_64_mode_x87) || (b == x86_64_mode_x87))
        返回 x86_64_mode_memory;
    另
        返回 x86_64_mode_sse;
}

静态 X86_64_Mode classify_x86_64_inner(CType *ty)
{
    X86_64_Mode mode;
    Sym *f;
    
    转接 (ty->t & VT_BTYPE) {
    事例 VT_VOID: 返回 x86_64_mode_none;
    
    事例 VT_INT:
    事例 VT_BYTE:
    事例 VT_SHORT:
    事例 VT_LLONG:
    事例 VT_BOOL:
    事例 VT_PTR:
    事例 VT_FUNC:
        返回 x86_64_mode_integer;
    
    事例 VT_FLOAT:
    事例 VT_DOUBLE: 返回 x86_64_mode_sse;
    
    事例 VT_LDOUBLE: 返回 x86_64_mode_x87;
      
    事例 VT_STRUCT:
        f = ty->ref;

        mode = x86_64_mode_none;
        对于 (f = f->next; f; f = f->next)
            mode = classify_x86_64_merge(mode, classify_x86_64_inner(&f->type));
        
        返回 mode;
    }
    assert(0);
    返回 0;
}

静态 X86_64_Mode classify_x86_64_arg(CType *ty, CType *ret, 整 *psize, 整 *palign, 整 *reg_count)
{
    X86_64_Mode mode;
    整 size, align, ret_t = 0;
    
    如 (ty->t & (VT_BITFIELD|VT_ARRAY)) {
        *psize = 8;
        *palign = 8;
        *reg_count = 1;
        ret_t = ty->t;
        mode = x86_64_mode_integer;
    } 另 {
        size = type_size(ty, &align);
        *psize = (size + 7) & ~7;
        *palign = (align + 7) & ~7;
    
        如 (size > 16) {
            mode = x86_64_mode_memory;
        } 另 {
            mode = classify_x86_64_inner(ty);
            转接 (mode) {
            事例 x86_64_mode_integer:
                如 (size > 8) {
                    *reg_count = 2;
                    ret_t = VT_QLONG;
                } 另 {
                    *reg_count = 1;
                    ret_t = (size > 4) ? VT_LLONG : VT_INT;
                }
                跳出;
                
            事例 x86_64_mode_x87:
                *reg_count = 1;
                ret_t = VT_LDOUBLE;
                跳出;

            事例 x86_64_mode_sse:
                如 (size > 8) {
                    *reg_count = 2;
                    ret_t = VT_QFLOAT;
                } 另 {
                    *reg_count = 1;
                    ret_t = (size > 4) ? VT_DOUBLE : VT_FLOAT;
                }
                跳出;
            缺省: 跳出; /* nothing to be done for x86_64_mode_memory and x86_64_mode_none*/
            }
        }
    }
    
    如 (ret) {
        ret->ref = NULL;
        ret->t = ret_t;
    }
    
    返回 mode;
}

ST_FUNC 整 classify_x86_64_va_arg(CType *ty)
{
    /* This definition must be synced with stdarg.h */
    枚举 __va_arg_type {
        __va_gen_reg, __va_float_reg, __va_stack
    };
    整 size, align, reg_count;
    X86_64_Mode mode = classify_x86_64_arg(ty, NULL, &size, &align, &reg_count);
    转接 (mode) {
    缺省: 返回 __va_stack;
    事例 x86_64_mode_integer: 返回 __va_gen_reg;
    事例 x86_64_mode_sse: 返回 __va_float_reg;
    }
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret, 整 *ret_align, 整 *regsize)
{
    整 size, align, reg_count;
    *ret_align = 1; // Never have to re-align return values for x86-64
    *regsize = 8;
    返回 (classify_x86_64_arg(vt, ret, &size, &align, &reg_count) != x86_64_mode_memory);
}

#定义 REGN 6
静态 不变 uint8_t arg_regs[REGN] = {
    TREG_RDI, TREG_RSI, TREG_RDX, TREG_RCX, TREG_R8, TREG_R9
};

静态 整 arg_prepare_reg(整 idx) {
  如 (idx == 2 || idx == 3)
      /* idx=2: r10, idx=3: r11 */
      返回 idx + 8;
  另
      返回 arg_regs[idx];
}

/* Generate function call. The function address is pushed first, then
   all the parameters in call order. This functions pops all the
   parameters and the function address. */
空 gfunc_call(整 nb_args)
{
    X86_64_Mode mode;
    CType type;
    整 size, align, r, args_size, stack_adjust, i, reg_count;
    整 nb_reg_args = 0;
    整 nb_sse_args = 0;
    整 sse_reg, gen_reg;
    字 _onstack[nb_args], *onstack = _onstack;

    /* calculate the number of integer/float register arguments, remember
       arguments to be passed via stack (in onstack[]), and also remember
       if we have to align the stack pointer to 16 (onstack[i] == 2).  Needs
       to be done in a left-to-right pass over arguments.  */
    stack_adjust = 0;
    对于(i = nb_args - 1; i >= 0; i--) {
        mode = classify_x86_64_arg(&vtop[-i].type, NULL, &size, &align, &reg_count);
        如 (mode == x86_64_mode_sse && nb_sse_args + reg_count <= 8) {
            nb_sse_args += reg_count;
            onstack[i] = 0;
        } 另 如 (mode == x86_64_mode_integer && nb_reg_args + reg_count <= REGN) {
            nb_reg_args += reg_count;
            onstack[i] = 0;
        } 另 如 (mode == x86_64_mode_none) {
            onstack[i] = 0;
        } 另 {
            如 (align == 16 && (stack_adjust &= 15)) {
                onstack[i] = 2;
                stack_adjust = 0;
            } 另
              onstack[i] = 1;
            stack_adjust += size;
        }
    }

    如 (nb_sse_args && tcc_state->nosse)
      tcc_error("SSE disabled but floating point arguments passed");

    /* fetch cpu flag before generating any code */
    如 (vtop >= vstack && (vtop->r & VT_VALMASK) == VT_CMP)
      gv(RC_INT);

    /* for struct arguments, we need to call memcpy and the function
       call breaks register passing arguments we are preparing.
       So, we process arguments which will be passed by stack first. */
    gen_reg = nb_reg_args;
    sse_reg = nb_sse_args;
    args_size = 0;
    stack_adjust &= 15;
    对于 (i = 0; i < nb_args;) {
        mode = classify_x86_64_arg(&vtop[-i].type, NULL, &size, &align, &reg_count);
        如 (!onstack[i]) {
            ++i;
            继续;
        }
        /* Possibly adjust stack to align SSE boundary.  We're processing
           args from right to left while allocating happens left to right
           (stack grows down), so the adjustment needs to happen _after_
           an argument that requires it.  */
        如 (stack_adjust) {
            o(0x50); /* push %rax; aka sub $8,%rsp */
            args_size += 8;
            stack_adjust = 0;
        }
        如 (onstack[i] == 2)
          stack_adjust = 1;

        vrotb(i+1);

        转接 (vtop->type.t & VT_BTYPE) {
            事例 VT_STRUCT:
                /* allocate the necessary size on stack */
                o(0x48);
                oad(0xec81, size); /* sub $xxx, %rsp */
                /* generate structure store */
                r = get_reg(RC_INT);
                orex(1, r, 0, 0x89); /* mov %rsp, r */
                o(0xe0 + REG_VALUE(r));
                vset(&vtop->type, r | VT_LVAL, 0);
                vswap();
                vstore();
                跳出;

            事例 VT_LDOUBLE:
                gv(RC_ST0);
                oad(0xec8148, size); /* sub $xxx, %rsp */
                o(0x7cdb); /* fstpt 0(%rsp) */
                g(0x24);
                g(0x00);
                跳出;

            事例 VT_FLOAT:
            事例 VT_DOUBLE:
                assert(mode == x86_64_mode_sse);
                r = gv(RC_FLOAT);
                o(0x50); /* push $rax */
                /* movq %xmmN, (%rsp) */
                o(0xd60f66);
                o(0x04 + REG_VALUE(r)*8);
                o(0x24);
                跳出;

            缺省:
                assert(mode == x86_64_mode_integer);
                /* simple type */
                /* XXX: implicit cast ? */
                r = gv(RC_INT);
                orex(0,r,0,0x50 + REG_VALUE(r)); /* push r */
                跳出;
        }
        args_size += size;

        vpop();
        --nb_args;
        onstack++;
    }

    /* XXX This should be superfluous.  */
    save_regs(0); /* save used temporary registers */

    /* then, we prepare register passing arguments.
       Note that we cannot set RDX and RCX in this loop because gv()
       may break these temporary registers. Let's use R10 and R11
       instead of them */
    assert(gen_reg <= REGN);
    assert(sse_reg <= 8);
    对于(i = 0; i < nb_args; i++) {
        mode = classify_x86_64_arg(&vtop->type, &type, &size, &align, &reg_count);
        /* Alter stack entry type so that gv() knows how to treat it */
        vtop->type = type;
        如 (mode == x86_64_mode_sse) {
            如 (reg_count == 2) {
                sse_reg -= 2;
                gv(RC_FRET); /* Use pair load into xmm0 & xmm1 */
                如 (sse_reg) { /* avoid redundant movaps %xmm0, %xmm0 */
                    /* movaps %xmm0, %xmmN */
                    o(0x280f);
                    o(0xc0 + (sse_reg << 3));
                    /* movaps %xmm1, %xmmN */
                    o(0x280f);
                    o(0xc1 + ((sse_reg+1) << 3));
                }
            } 另 {
                assert(reg_count == 1);
                --sse_reg;
                /* Load directly to register */
                gv(RC_XMM0 << sse_reg);
            }
        } 另 如 (mode == x86_64_mode_integer) {
            /* simple type */
            /* XXX: implicit cast ? */
            整 d;
            gen_reg -= reg_count;
            r = gv(RC_INT);
            d = arg_prepare_reg(gen_reg);
            orex(1,d,r,0x89); /* mov */
            o(0xc0 + REG_VALUE(r) * 8 + REG_VALUE(d));
            如 (reg_count == 2) {
                d = arg_prepare_reg(gen_reg+1);
                orex(1,d,vtop->r2,0x89); /* mov */
                o(0xc0 + REG_VALUE(vtop->r2) * 8 + REG_VALUE(d));
            }
        }
        vtop--;
    }
    assert(gen_reg == 0);
    assert(sse_reg == 0);

    /* We shouldn't have many operands on the stack anymore, but the
       call address itself is still there, and it might be in %eax
       (or edx/ecx) currently, which the below writes would clobber.
       So evict all remaining operands here.  */
    save_regs(0);

    /* Copy R10 and R11 into RDX and RCX, respectively */
    如 (nb_reg_args > 2) {
        o(0xd2894c); /* mov %r10, %rdx */
        如 (nb_reg_args > 3) {
            o(0xd9894c); /* mov %r11, %rcx */
        }
    }

    如 (vtop->type.ref->f.func_type != FUNC_NEW) /* implies FUNC_OLD or FUNC_ELLIPSIS */
        oad(0xb8, nb_sse_args < 8 ? nb_sse_args : 8); /* mov nb_sse_args, %eax */
    gcall_or_jmp(0);
    如 (args_size)
        gadd_sp(args_size);
    vtop--;
}


#定义 FUNC_PROLOG_SIZE 11

静态 空 push_arg_reg(整 i) {
    loc -= 8;
    gen_modrm64(0x89, arg_regs[i], VT_LOCAL, NULL, loc);
}

/* generate function prolog of type 't' */
空 gfunc_prolog(CType *func_type)
{
    X86_64_Mode mode;
    整 i, addr, align, size, reg_count;
    整 param_addr = 0, reg_param_index, sse_param_index;
    Sym *sym;
    CType *type;

    sym = func_type->ref;
    addr = PTR_SIZE * 2;
    loc = 0;
    ind += FUNC_PROLOG_SIZE;
    func_sub_sp_offset = ind;
    func_ret_sub = 0;

    如 (sym->f.func_type == FUNC_ELLIPSIS) {
        整 seen_reg_num, seen_sse_num, seen_stack_size;
        seen_reg_num = seen_sse_num = 0;
        /* frame pointer and return address */
        seen_stack_size = PTR_SIZE * 2;
        /* count the number of seen parameters */
        sym = func_type->ref;
        当 ((sym = sym->next) != NULL) {
            type = &sym->type;
            mode = classify_x86_64_arg(type, NULL, &size, &align, &reg_count);
            转接 (mode) {
            缺省:
            stack_arg:
                seen_stack_size = ((seen_stack_size + align - 1) & -align) + size;
                跳出;
                
            事例 x86_64_mode_integer:
                如 (seen_reg_num + reg_count > REGN)
                    跳转 stack_arg;
                seen_reg_num += reg_count;
                跳出;
                
            事例 x86_64_mode_sse:
                如 (seen_sse_num + reg_count > 8)
                    跳转 stack_arg;
                seen_sse_num += reg_count;
                跳出;
            }
        }

        loc -= 16;
        /* movl $0x????????, -0x10(%rbp) */
        o(0xf045c7);
        gen_le32(seen_reg_num * 8);
        /* movl $0x????????, -0xc(%rbp) */
        o(0xf445c7);
        gen_le32(seen_sse_num * 16 + 48);
        /* movl $0x????????, -0x8(%rbp) */
        o(0xf845c7);
        gen_le32(seen_stack_size);

        /* save all register passing arguments */
        对于 (i = 0; i < 8; i++) {
            loc -= 16;
            如 (!tcc_state->nosse) {
                o(0xd60f66); /* movq */
                gen_modrm(7 - i, VT_LOCAL, NULL, loc);
            }
            /* movq $0, loc+8(%rbp) */
            o(0x85c748);
            gen_le32(loc + 8);
            gen_le32(0);
        }
        对于 (i = 0; i < REGN; i++) {
            push_arg_reg(REGN-1-i);
        }
    }

    sym = func_type->ref;
    reg_param_index = 0;
    sse_param_index = 0;

    /* if the function returns a structure, then add an
       implicit pointer parameter */
    func_vt = sym->type;
    mode = classify_x86_64_arg(&func_vt, NULL, &size, &align, &reg_count);
    如 (mode == x86_64_mode_memory) {
        push_arg_reg(reg_param_index);
        func_vc = loc;
        reg_param_index++;
    }
    /* define parameters */
    当 ((sym = sym->next) != NULL) {
        type = &sym->type;
        mode = classify_x86_64_arg(type, NULL, &size, &align, &reg_count);
        转接 (mode) {
        事例 x86_64_mode_sse:
            如 (tcc_state->nosse)
                tcc_error("SSE disabled but floating point arguments used");
            如 (sse_param_index + reg_count <= 8) {
                /* save arguments passed by register */
                loc -= reg_count * 8;
                param_addr = loc;
                对于 (i = 0; i < reg_count; ++i) {
                    o(0xd60f66); /* movq */
                    gen_modrm(sse_param_index, VT_LOCAL, NULL, param_addr + i*8);
                    ++sse_param_index;
                }
            } 另 {
                addr = (addr + align - 1) & -align;
                param_addr = addr;
                addr += size;
            }
            跳出;
            
        事例 x86_64_mode_memory:
        事例 x86_64_mode_x87:
            addr = (addr + align - 1) & -align;
            param_addr = addr;
            addr += size;
            跳出;
            
        事例 x86_64_mode_integer: {
            如 (reg_param_index + reg_count <= REGN) {
                /* save arguments passed by register */
                loc -= reg_count * 8;
                param_addr = loc;
                对于 (i = 0; i < reg_count; ++i) {
                    gen_modrm64(0x89, arg_regs[reg_param_index], VT_LOCAL, NULL, param_addr + i*8);
                    ++reg_param_index;
                }
            } 另 {
                addr = (addr + align - 1) & -align;
                param_addr = addr;
                addr += size;
            }
            跳出;
        }
        缺省: 跳出; /* nothing to be done for x86_64_mode_none */
        }
        sym_push(sym->v & ~SYM_FIELD, type,
                 VT_LOCAL | VT_LVAL, param_addr);
    }

#如定义 CONFIG_TCC_BCHECK
    /* leave some room for bound checking code */
    如 (tcc_state->do_bounds_check) {
        func_bound_offset = lbounds_section->data_offset;
        func_bound_ind = ind;
        oad(0xb8, 0); /* lbound section pointer */
        o(0xc78948);  /* mov  %rax,%rdi ## first arg in %rdi, this must be ptr */
        oad(0xb8, 0); /* call to function */
    }
#了如
}

/* generate function epilog */
空 gfunc_epilog(空)
{
    整 v, saved_ind;

#如定义 CONFIG_TCC_BCHECK
    如 (tcc_state->do_bounds_check
        && func_bound_offset != lbounds_section->data_offset)
    {
        addr_t saved_ind;
        addr_t *bounds_ptr;
        Sym *sym_data;

        /* add end of table info */
        bounds_ptr = section_ptr_add(lbounds_section, 求长度(addr_t));
        *bounds_ptr = 0;

        /* generate bound local allocation */
        sym_data = get_sym_ref(&char_pointer_type, lbounds_section, 
                               func_bound_offset, lbounds_section->data_offset);
        saved_ind = ind;
        ind = func_bound_ind;
        greloca(cur_text_section, sym_data, ind + 1, R_X86_64_64, 0);
        ind = ind + 5 + 3;
        gen_static_call(TOK___bound_local_new);
        ind = saved_ind;

        /* generate bound check local freeing */
        o(0x5250); /* save returned value, if any */
        greloca(cur_text_section, sym_data, ind + 1, R_X86_64_64, 0);
        oad(0xb8, 0); /* mov xxx, %rax */
        o(0xc78948);  /* mov %rax,%rdi # first arg in %rdi, this must be ptr */
        gen_static_call(TOK___bound_local_delete);
        o(0x585a); /* restore returned value, if any */
    }
#了如
    o(0xc9); /* leave */
    如 (func_ret_sub == 0) {
        o(0xc3); /* ret */
    } 另 {
        o(0xc2); /* ret n */
        g(func_ret_sub);
        g(func_ret_sub >> 8);
    }
    /* align local size to word & save local variables */
    v = (-loc + 15) & -16;
    saved_ind = ind;
    ind = func_sub_sp_offset - FUNC_PROLOG_SIZE;
    o(0xe5894855);  /* push %rbp, mov %rsp, %rbp */
    o(0xec8148);  /* sub rsp, stacksize */
    gen_le32(v);
    ind = saved_ind;
}

#了如 /* not PE */

/* generate a jump to a label */
整 gjmp(整 t)
{
    返回 gjmp2(0xe9, t);
}

/* generate a jump to a fixed address */
空 gjmp_addr(整 a)
{
    整 r;
    r = a - ind - 2;
    如 (r == (字)r) {
        g(0xeb);
        g(r);
    } 另 {
        oad(0xe9, a - ind - 5);
    }
}

ST_FUNC 空 gtst_addr(整 inv, 整 a)
{
    整 v = vtop->r & VT_VALMASK;
    如 (v == VT_CMP) {
        inv ^= (vtop--)->c.i;
        a -= ind + 2;
        如 (a == (字)a) {
            g(inv - 32);
            g(a);
        } 另 {
            g(0x0f);
            oad(inv - 16, a - 4);
        }
    } 另 如 ((v & ~1) == VT_JMP) {
        如 ((v & 1) != inv) {
            gjmp_addr(a);
            gsym(vtop->c.i);
        } 另 {
            gsym(vtop->c.i);
            o(0x05eb);
            gjmp_addr(a);
        }
        vtop--;
    }
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
ST_FUNC 整 gtst(整 inv, 整 t)
{
    整 v = vtop->r & VT_VALMASK;

    如 (nocode_wanted) {
        ;
    } 另 如 (v == VT_CMP) {
        /* fast case : can jump directly since flags are set */
        如 (vtop->c.i & 0x100)
          {
            /* This was a float compare.  If the parity flag is set
               the result was unordered.  For anything except != this
               means false and we don't jump (anding both conditions).
               For != this means true (oring both).
               Take care about inverting the test.  We need to jump
               to our target if the result was unordered and test wasn't NE,
               otherwise if unordered we don't want to jump.  */
            vtop->c.i &= ~0x100;
            如 (inv == (vtop->c.i == TOK_NE))
              o(0x067a);  /* jp +6 */
            另
              {
                g(0x0f);
                t = gjmp2(0x8a, t); /* jp t */
              }
          }
        g(0x0f);
        t = gjmp2((vtop->c.i - 16) ^ inv, t);
    } 另 如 (v == VT_JMP || v == VT_JMPI) {
        /* && or || optimization */
        如 ((v & 1) == inv) {
            /* insert vtop->c jump list in t */
            uint32_t n1, n = vtop->c.i;
            如 (n) {
                当 ((n1 = read32le(cur_text_section->data + n)))
                    n = n1;
                write32le(cur_text_section->data + n, t);
                t = vtop->c.i;
            }
        } 另 {
            t = gjmp(t);
            gsym(vtop->c.i);
        }
    }
    vtop--;
    返回 t;
}

/* generate an integer binary operation */
空 gen_opi(整 op)
{
    整 r, fr, opc, c;
    整 ll, uu, cc;

    ll = is64_type(vtop[-1].type.t);
    uu = (vtop[-1].type.t & VT_UNSIGNED) != 0;
    cc = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;

    转接(op) {
    事例 '+':
    事例 TOK_ADDC1: /* add with carry generation */
        opc = 0;
    gen_op8:
        如 (cc && (!ll || (整)vtop->c.i == vtop->c.i)) {
            /* constant case */
            vswap();
            r = gv(RC_INT);
            vswap();
            c = vtop->c.i;
            如 (c == (字)c) {
                /* XXX: generate inc and dec for smaller code ? */
                orex(ll, r, 0, 0x83);
                o(0xc0 | (opc << 3) | REG_VALUE(r));
                g(c);
            } 另 {
                orex(ll, r, 0, 0x81);
                oad(0xc0 | (opc << 3) | REG_VALUE(r), c);
            }
        } 另 {
            gv2(RC_INT, RC_INT);
            r = vtop[-1].r;
            fr = vtop[0].r;
            orex(ll, r, fr, (opc << 3) | 0x01);
            o(0xc0 + REG_VALUE(r) + REG_VALUE(fr) * 8);
        }
        vtop--;
        如 (op >= TOK_ULT && op <= TOK_GT) {
            vtop->r = VT_CMP;
            vtop->c.i = op;
        }
        跳出;
    事例 '-':
    事例 TOK_SUBC1: /* sub with carry generation */
        opc = 5;
        跳转 gen_op8;
    事例 TOK_ADDC2: /* add with carry use */
        opc = 2;
        跳转 gen_op8;
    事例 TOK_SUBC2: /* sub with carry use */
        opc = 3;
        跳转 gen_op8;
    事例 '&':
        opc = 4;
        跳转 gen_op8;
    事例 '^':
        opc = 6;
        跳转 gen_op8;
    事例 '|':
        opc = 1;
        跳转 gen_op8;
    事例 '*':
        gv2(RC_INT, RC_INT);
        r = vtop[-1].r;
        fr = vtop[0].r;
        orex(ll, fr, r, 0xaf0f); /* imul fr, r */
        o(0xc0 + REG_VALUE(fr) + REG_VALUE(r) * 8);
        vtop--;
        跳出;
    事例 TOK_SHL:
        opc = 4;
        跳转 gen_shift;
    事例 TOK_SHR:
        opc = 5;
        跳转 gen_shift;
    事例 TOK_SAR:
        opc = 7;
    gen_shift:
        opc = 0xc0 | (opc << 3);
        如 (cc) {
            /* constant case */
            vswap();
            r = gv(RC_INT);
            vswap();
            orex(ll, r, 0, 0xc1); /* shl/shr/sar $xxx, r */
            o(opc | REG_VALUE(r));
            g(vtop->c.i & (ll ? 63 : 31));
        } 另 {
            /* we generate the shift in ecx */
            gv2(RC_INT, RC_RCX);
            r = vtop[-1].r;
            orex(ll, r, 0, 0xd3); /* shl/shr/sar %cl, r */
            o(opc | REG_VALUE(r));
        }
        vtop--;
        跳出;
    事例 TOK_UDIV:
    事例 TOK_UMOD:
        uu = 1;
        跳转 divmod;
    事例 '/':
    事例 '%':
    事例 TOK_PDIV:
        uu = 0;
    divmod:
        /* first operand must be in eax */
        /* XXX: need better constraint for second operand */
        gv2(RC_RAX, RC_RCX);
        r = vtop[-1].r;
        fr = vtop[0].r;
        vtop--;
        save_reg(TREG_RDX);
        orex(ll, 0, 0, uu ? 0xd231 : 0x99); /* xor %edx,%edx : cqto */
        orex(ll, fr, 0, 0xf7); /* div fr, %eax */
        o((uu ? 0xf0 : 0xf8) + REG_VALUE(fr));
        如 (op == '%' || op == TOK_UMOD)
            r = TREG_RDX;
        另
            r = TREG_RAX;
        vtop->r = r;
        跳出;
    缺省:
        opc = 7;
        跳转 gen_op8;
    }
}

空 gen_opl(整 op)
{
    gen_opi(op);
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranteed to have the same floating point type */
/* XXX: need to use ST1 too */
空 gen_opf(整 op)
{
    整 a, ft, fc, swapped, r;
    整 float_type =
        (vtop->type.t & VT_BTYPE) == VT_LDOUBLE ? RC_ST0 : RC_FLOAT;

    /* convert constants to memory references */
    如 ((vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
        vswap();
        gv(float_type);
        vswap();
    }
    如 ((vtop[0].r & (VT_VALMASK | VT_LVAL)) == VT_CONST)
        gv(float_type);

    /* must put at least one value in the floating point register */
    如 ((vtop[-1].r & VT_LVAL) &&
        (vtop[0].r & VT_LVAL)) {
        vswap();
        gv(float_type);
        vswap();
    }
    swapped = 0;
    /* swap the stack if needed so that t1 is the register and t2 is
       the memory reference */
    如 (vtop[-1].r & VT_LVAL) {
        vswap();
        swapped = 1;
    }
    如 ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
        如 (op >= TOK_ULT && op <= TOK_GT) {
            /* load on stack second operand */
            load(TREG_ST0, vtop);
            save_reg(TREG_RAX); /* eax is used by FP comparison code */
            如 (op == TOK_GE || op == TOK_GT)
                swapped = !swapped;
            另 如 (op == TOK_EQ || op == TOK_NE)
                swapped = 0;
            如 (swapped)
                o(0xc9d9); /* fxch %st(1) */
            如 (op == TOK_EQ || op == TOK_NE)
                o(0xe9da); /* fucompp */
            另
                o(0xd9de); /* fcompp */
            o(0xe0df); /* fnstsw %ax */
            如 (op == TOK_EQ) {
                o(0x45e480); /* and $0x45, %ah */
                o(0x40fC80); /* cmp $0x40, %ah */
            } 另 如 (op == TOK_NE) {
                o(0x45e480); /* and $0x45, %ah */
                o(0x40f480); /* xor $0x40, %ah */
                op = TOK_NE;
            } 另 如 (op == TOK_GE || op == TOK_LE) {
                o(0x05c4f6); /* test $0x05, %ah */
                op = TOK_EQ;
            } 另 {
                o(0x45c4f6); /* test $0x45, %ah */
                op = TOK_EQ;
            }
            vtop--;
            vtop->r = VT_CMP;
            vtop->c.i = op;
        } 另 {
            /* no memory reference possible for long double operations */
            load(TREG_ST0, vtop);
            swapped = !swapped;

            转接(op) {
            缺省:
            事例 '+':
                a = 0;
                跳出;
            事例 '-':
                a = 4;
                如 (swapped)
                    a++;
                跳出;
            事例 '*':
                a = 1;
                跳出;
            事例 '/':
                a = 6;
                如 (swapped)
                    a++;
                跳出;
            }
            ft = vtop->type.t;
            fc = vtop->c.i;
            o(0xde); /* fxxxp %st, %st(1) */
            o(0xc1 + (a << 3));
            vtop--;
        }
    } 另 {
        如 (op >= TOK_ULT && op <= TOK_GT) {
            /* if saved lvalue, then we must reload it */
            r = vtop->r;
            fc = vtop->c.i;
            如 ((r & VT_VALMASK) == VT_LLOCAL) {
                SValue v1;
                r = get_reg(RC_INT);
                v1.type.t = VT_PTR;
                v1.r = VT_LOCAL | VT_LVAL;
                v1.c.i = fc;
                load(r, &v1);
                fc = 0;
            }

            如 (op == TOK_EQ || op == TOK_NE) {
                swapped = 0;
            } 另 {
                如 (op == TOK_LE || op == TOK_LT)
                    swapped = !swapped;
                如 (op == TOK_LE || op == TOK_GE) {
                    op = 0x93; /* setae */
                } 另 {
                    op = 0x97; /* seta */
                }
            }

            如 (swapped) {
                gv(RC_FLOAT);
                vswap();
            }
            assert(!(vtop[-1].r & VT_LVAL));
            
            如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE)
                o(0x66);
            如 (op == TOK_EQ || op == TOK_NE)
                o(0x2e0f); /* ucomisd */
            另
                o(0x2f0f); /* comisd */

            如 (vtop->r & VT_LVAL) {
                gen_modrm(vtop[-1].r, r, vtop->sym, fc);
            } 另 {
                o(0xc0 + REG_VALUE(vtop[0].r) + REG_VALUE(vtop[-1].r)*8);
            }

            vtop--;
            vtop->r = VT_CMP;
            vtop->c.i = op | 0x100;
        } 另 {
            assert((vtop->type.t & VT_BTYPE) != VT_LDOUBLE);
            转接(op) {
            缺省:
            事例 '+':
                a = 0;
                跳出;
            事例 '-':
                a = 4;
                跳出;
            事例 '*':
                a = 1;
                跳出;
            事例 '/':
                a = 6;
                跳出;
            }
            ft = vtop->type.t;
            fc = vtop->c.i;
            assert((ft & VT_BTYPE) != VT_LDOUBLE);
            
            r = vtop->r;
            /* if saved lvalue, then we must reload it */
            如 ((vtop->r & VT_VALMASK) == VT_LLOCAL) {
                SValue v1;
                r = get_reg(RC_INT);
                v1.type.t = VT_PTR;
                v1.r = VT_LOCAL | VT_LVAL;
                v1.c.i = fc;
                load(r, &v1);
                fc = 0;
            }
            
            assert(!(vtop[-1].r & VT_LVAL));
            如 (swapped) {
                assert(vtop->r & VT_LVAL);
                gv(RC_FLOAT);
                vswap();
            }
            
            如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                o(0xf2);
            } 另 {
                o(0xf3);
            }
            o(0x0f);
            o(0x58 + a);
            
            如 (vtop->r & VT_LVAL) {
                gen_modrm(vtop[-1].r, r, vtop->sym, fc);
            } 另 {
                o(0xc0 + REG_VALUE(vtop[0].r) + REG_VALUE(vtop[-1].r)*8);
            }

            vtop--;
        }
    }
}

/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
空 gen_cvt_itof(整 t)
{
    如 ((t & VT_BTYPE) == VT_LDOUBLE) {
        save_reg(TREG_ST0);
        gv(RC_INT);
        如 ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
            /* signed long long to float/double/long double (unsigned case
               is handled generically) */
            o(0x50 + (vtop->r & VT_VALMASK)); /* push r */
            o(0x242cdf); /* fildll (%rsp) */
            o(0x08c48348); /* add $8, %rsp */
        } 另 如 ((vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) ==
                   (VT_INT | VT_UNSIGNED)) {
            /* unsigned int to float/double/long double */
            o(0x6a); /* push $0 */
            g(0x00);
            o(0x50 + (vtop->r & VT_VALMASK)); /* push r */
            o(0x242cdf); /* fildll (%rsp) */
            o(0x10c48348); /* add $16, %rsp */
        } 另 {
            /* int to float/double/long double */
            o(0x50 + (vtop->r & VT_VALMASK)); /* push r */
            o(0x2404db); /* fildl (%rsp) */
            o(0x08c48348); /* add $8, %rsp */
        }
        vtop->r = TREG_ST0;
    } 另 {
        整 r = get_reg(RC_FLOAT);
        gv(RC_INT);
        o(0xf2 + ((t & VT_BTYPE) == VT_FLOAT?1:0));
        如 ((vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) ==
            (VT_INT | VT_UNSIGNED) ||
            (vtop->type.t & VT_BTYPE) == VT_LLONG) {
            o(0x48); /* REX */
        }
        o(0x2a0f);
        o(0xc0 + (vtop->r & VT_VALMASK) + REG_VALUE(r)*8); /* cvtsi2sd */
        vtop->r = r;
    }
}

/* convert from one floating point type to another */
空 gen_cvt_ftof(整 t)
{
    整 ft, bt, tbt;

    ft = vtop->type.t;
    bt = ft & VT_BTYPE;
    tbt = t & VT_BTYPE;
    
    如 (bt == VT_FLOAT) {
        gv(RC_FLOAT);
        如 (tbt == VT_DOUBLE) {
            o(0x140f); /* unpcklps */
            o(0xc0 + REG_VALUE(vtop->r)*9);
            o(0x5a0f); /* cvtps2pd */
            o(0xc0 + REG_VALUE(vtop->r)*9);
        } 另 如 (tbt == VT_LDOUBLE) {
            save_reg(RC_ST0);
            /* movss %xmm0,-0x10(%rsp) */
            o(0x110ff3);
            o(0x44 + REG_VALUE(vtop->r)*8);
            o(0xf024);
            o(0xf02444d9); /* flds -0x10(%rsp) */
            vtop->r = TREG_ST0;
        }
    } 另 如 (bt == VT_DOUBLE) {
        gv(RC_FLOAT);
        如 (tbt == VT_FLOAT) {
            o(0x140f66); /* unpcklpd */
            o(0xc0 + REG_VALUE(vtop->r)*9);
            o(0x5a0f66); /* cvtpd2ps */
            o(0xc0 + REG_VALUE(vtop->r)*9);
        } 另 如 (tbt == VT_LDOUBLE) {
            save_reg(RC_ST0);
            /* movsd %xmm0,-0x10(%rsp) */
            o(0x110ff2);
            o(0x44 + REG_VALUE(vtop->r)*8);
            o(0xf024);
            o(0xf02444dd); /* fldl -0x10(%rsp) */
            vtop->r = TREG_ST0;
        }
    } 另 {
        整 r;
        gv(RC_ST0);
        r = get_reg(RC_FLOAT);
        如 (tbt == VT_DOUBLE) {
            o(0xf0245cdd); /* fstpl -0x10(%rsp) */
            /* movsd -0x10(%rsp),%xmm0 */
            o(0x100ff2);
            o(0x44 + REG_VALUE(r)*8);
            o(0xf024);
            vtop->r = r;
        } 另 如 (tbt == VT_FLOAT) {
            o(0xf0245cd9); /* fstps -0x10(%rsp) */
            /* movss -0x10(%rsp),%xmm0 */
            o(0x100ff3);
            o(0x44 + REG_VALUE(r)*8);
            o(0xf024);
            vtop->r = r;
        }
    }
}

/* convert fp to int 't' type */
空 gen_cvt_ftoi(整 t)
{
    整 ft, bt, size, r;
    ft = vtop->type.t;
    bt = ft & VT_BTYPE;
    如 (bt == VT_LDOUBLE) {
        gen_cvt_ftof(VT_DOUBLE);
        bt = VT_DOUBLE;
    }

    gv(RC_FLOAT);
    如 (t != VT_INT)
        size = 8;
    另
        size = 4;

    r = get_reg(RC_INT);
    如 (bt == VT_FLOAT) {
        o(0xf3);
    } 另 如 (bt == VT_DOUBLE) {
        o(0xf2);
    } 另 {
        assert(0);
    }
    orex(size == 8, r, 0, 0x2c0f); /* cvttss2si or cvttsd2si */
    o(0xc0 + REG_VALUE(vtop->r) + REG_VALUE(r)*8);
    vtop->r = r;
}

/* computed goto support */
空 ggoto(空)
{
    gcall_or_jmp(1);
    vtop--;
}

/* Save the stack pointer onto the stack and return the location of its address */
ST_FUNC 空 gen_vla_sp_save(整 addr) {
    /* mov %rsp,addr(%rbp)*/
    gen_modrm64(0x89, TREG_RSP, VT_LOCAL, NULL, addr);
}

/* Restore the SP from a location on the stack */
ST_FUNC 空 gen_vla_sp_restore(整 addr) {
    gen_modrm64(0x8b, TREG_RSP, VT_LOCAL, NULL, addr);
}

/* Subtract from the stack pointer, and push the resulting value onto the stack */
ST_FUNC 空 gen_vla_alloc(CType *type, 整 align) {
#如定义 TCC_TARGET_PE
    /* alloca does more than just adjust %rsp on Windows */
    vpush_global_sym(&func_old_type, TOK_alloca);
    vswap(); /* Move alloca ref past allocation size */
    gfunc_call(1);
#另
    整 r;
    r = gv(RC_INT); /* allocation size */
    /* sub r,%rsp */
    o(0x2b48);
    o(0xe0 | REG_VALUE(r));
    /* We align to 16 bytes rather than align */
    /* and ~15, %rsp */
    o(0xf0e48348);
    vpop();
#了如
}


/* end of x86-64 code generator */
/*************************************************************/
#了如 /* ! TARGET_DEFS_ONLY */
/******************************************************/
