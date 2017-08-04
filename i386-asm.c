/*
 *  i386 specific functions for TCC assembler
 *
 *  Copyright (c) 2001, 2002 Fabrice Bellard
 *  Copyright (c) 2009 Frédéric Feret (x86_64 support)
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

#包含 "tcc.h"

#定义 MAX_OPERANDS 3

#定义 TOK_ASM_first TOK_ASM_clc
#定义 TOK_ASM_last TOK_ASM_emms
#定义 TOK_ASM_alllast TOK_ASM_subps

#定义 OPC_B          0x01  /* only used with OPC_WL */
#定义 OPC_WL         0x02  /* accepts w, l or no suffix */
#定义 OPC_BWL        (OPC_B | OPC_WL) /* accepts b, w, l or no suffix */
#定义 OPC_REG        0x04 /* register is added to opcode */
#定义 OPC_MODRM      0x08 /* modrm encoding */

#定义 OPCT_MASK      0x70
#定义 OPC_FWAIT      0x10 /* add fwait opcode */
#定义 OPC_SHIFT      0x20 /* shift opcodes */
#定义 OPC_ARITH      0x30 /* arithmetic opcodes */
#定义 OPC_FARITH     0x40 /* FPU arithmetic opcodes */
#定义 OPC_TEST       0x50 /* test opcodes */
#定义 OPCT_IS(v,i) (((v) & OPCT_MASK) == (i))

#定义 OPC_0F        0x100 /* Is secondary map (0x0f prefix) */
#定义 OPC_48        0x200 /* Always has REX prefix */
#如定义 TCC_TARGET_X86_64
# 定义 OPC_WLQ     0x1000  /* accepts w, l, q or no suffix */
# 定义 OPC_BWLQ    (OPC_B | OPC_WLQ) /* accepts b, w, l, q or no suffix */
# 定义 OPC_WLX     OPC_WLQ
# 定义 OPC_BWLX    OPC_BWLQ
#另
# 定义 OPC_WLX     OPC_WL
# 定义 OPC_BWLX    OPC_BWL
#了如

#定义 OPC_GROUP_SHIFT 13

/* in order to compress the operand type, we use specific operands and
   we or only with EA  */
枚举 {
    OPT_REG8=0, /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_REG16,  /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_REG32,  /* warning: value is hardcoded from TOK_ASM_xxx */
#如定义 TCC_TARGET_X86_64
    OPT_REG64,  /* warning: value is hardcoded from TOK_ASM_xxx */
#了如
    OPT_MMX,    /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_SSE,    /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_CR,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_TR,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_DB,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_SEG,
    OPT_ST,
#如定义 TCC_TARGET_X86_64
    OPT_REG8_LOW, /* %spl,%bpl,%sil,%dil, encoded like ah,ch,dh,bh, but
                     with REX prefix, not used in insn templates */
#了如
    OPT_IM8,
    OPT_IM8S,
    OPT_IM16,
    OPT_IM32,
#如定义 TCC_TARGET_X86_64
    OPT_IM64,
#了如
    OPT_EAX,    /* %al, %ax, %eax or %rax register */
    OPT_ST0,    /* %st(0) register */
    OPT_CL,     /* %cl register */
    OPT_DX,     /* %dx register */
    OPT_ADDR,   /* OP_EA with only offset */
    OPT_INDIR,  /* *(expr) */
    /* composite types */
    OPT_COMPOSITE_FIRST,
    OPT_IM,     /* IM8 | IM16 | IM32 */
    OPT_REG,    /* REG8 | REG16 | REG32 | REG64 */
    OPT_REGW,   /* REG16 | REG32 | REG64 */
    OPT_IMW,    /* IM16 | IM32 */
    OPT_MMXSSE, /* MMX | SSE */
    OPT_DISP,   /* Like OPT_ADDR, but emitted as displacement (for jumps) */
    OPT_DISP8,  /* Like OPT_ADDR, but only 8bit (short jumps) */
    /* can be ored with any OPT_xxx */
    OPT_EA = 0x80
};

#定义 OP_REG8   (1 << OPT_REG8)
#定义 OP_REG16  (1 << OPT_REG16)
#定义 OP_REG32  (1 << OPT_REG32)
#定义 OP_MMX    (1 << OPT_MMX)
#定义 OP_SSE    (1 << OPT_SSE)
#定义 OP_CR     (1 << OPT_CR)
#定义 OP_TR     (1 << OPT_TR)
#定义 OP_DB     (1 << OPT_DB)
#定义 OP_SEG    (1 << OPT_SEG)
#定义 OP_ST     (1 << OPT_ST)
#定义 OP_IM8    (1 << OPT_IM8)
#定义 OP_IM8S   (1 << OPT_IM8S)
#定义 OP_IM16   (1 << OPT_IM16)
#定义 OP_IM32   (1 << OPT_IM32)
#定义 OP_EAX    (1 << OPT_EAX)
#定义 OP_ST0    (1 << OPT_ST0)
#定义 OP_CL     (1 << OPT_CL)
#定义 OP_DX     (1 << OPT_DX)
#定义 OP_ADDR   (1 << OPT_ADDR)
#定义 OP_INDIR  (1 << OPT_INDIR)
#如定义 TCC_TARGET_X86_64
# 定义 OP_REG64 (1 << OPT_REG64)
# 定义 OP_REG8_LOW (1 << OPT_REG8_LOW)
# 定义 OP_IM64  (1 << OPT_IM64)
# 定义 OP_EA32  (OP_EA << 1)
#另
# 定义 OP_REG64 0
# 定义 OP_REG8_LOW 0
# 定义 OP_IM64  0
# 定义 OP_EA32  0
#了如

#定义 OP_EA     0x40000000
#定义 OP_REG    (OP_REG8 | OP_REG16 | OP_REG32 | OP_REG64)

#如定义 TCC_TARGET_X86_64
# 定义 TREG_XAX   TREG_RAX
# 定义 TREG_XCX   TREG_RCX
# 定义 TREG_XDX   TREG_RDX
#另
# 定义 TREG_XAX   TREG_EAX
# 定义 TREG_XCX   TREG_ECX
# 定义 TREG_XDX   TREG_EDX
#了如

类型定义 结构 ASMInstr {
    uint16_t sym;
    uint16_t opcode;
    uint16_t instr_type;
    uint8_t nb_ops;
    uint8_t op_type[MAX_OPERANDS]; /* see OP_xxx */
} ASMInstr;

类型定义 结构 Operand {
    uint32_t type;
    int8_t  reg; /* register, -1 if none */
    int8_t  reg2; /* second register, -1 if none */
    uint8_t shift;
    ExprValue e;
} Operand;

静态 不变 uint8_t reg_to_size[9] = {
/*
    [OP_REG8] = 0,
    [OP_REG16] = 1,
    [OP_REG32] = 2,
#如定义 TCC_TARGET_X86_64
    [OP_REG64] = 3,
#了如
*/
    0, 0, 1, 0, 2, 0, 0, 0, 3
};

#定义 NB_TEST_OPCODES 30

静态 不变 uint8_t test_bits[NB_TEST_OPCODES] = {
 0x00, /* o */
 0x01, /* no */
 0x02, /* b */
 0x02, /* c */
 0x02, /* nae */
 0x03, /* nb */
 0x03, /* nc */
 0x03, /* ae */
 0x04, /* e */
 0x04, /* z */
 0x05, /* ne */
 0x05, /* nz */
 0x06, /* be */
 0x06, /* na */
 0x07, /* nbe */
 0x07, /* a */
 0x08, /* s */
 0x09, /* ns */
 0x0a, /* p */
 0x0a, /* pe */
 0x0b, /* np */
 0x0b, /* po */
 0x0c, /* l */
 0x0c, /* nge */
 0x0d, /* nl */
 0x0d, /* ge */
 0x0e, /* le */
 0x0e, /* ng */
 0x0f, /* nle */
 0x0f, /* g */
};

静态 不变 uint8_t segment_prefixes[] = {
 0x26, /* es */
 0x2e, /* cs */
 0x36, /* ss */
 0x3e, /* ds */
 0x64, /* fs */
 0x65  /* gs */
};

静态 不变 ASMInstr asm_instrs[] = {
#定义 ALT(x) x
/* This removes a 0x0f in the second byte */
#定义 O(o) ((uint64_t) ((((o) & 0xff00) == 0x0f00) ? ((((o) >> 8) & ~0xff) | ((o) & 0xff)) : (o)))
/* This constructs instr_type from opcode, type and group.  */
#定义 T(o,i,g) ((i) | ((g) << OPC_GROUP_SHIFT) | ((((o) & 0xff00) == 0x0f00) ? OPC_0F : 0))
#定义 DEF_ASM_OP0(name, opcode)
#定义 DEF_ASM_OP0L(name, opcode, group, instr_type) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 0, { 0 } },
#定义 DEF_ASM_OP1(name, opcode, group, instr_type, op0) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 1, { op0 }},
#定义 DEF_ASM_OP2(name, opcode, group, instr_type, op0, op1) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 2, { op0, op1 }},
#定义 DEF_ASM_OP3(name, opcode, group, instr_type, op0, op1, op2) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 3, { op0, op1, op2 }},
#如定义 TCC_TARGET_X86_64
# 包含 "x86_64-asm.h"
#另
# 包含 "i386-asm.h"
#了如
    /* last operation */
    { 0, },
};

静态 不变 uint16_t op0_codes[] = {
#定义 ALT(x)
#定义 DEF_ASM_OP0(x, opcode) opcode,
#定义 DEF_ASM_OP0L(name, opcode, group, instr_type)
#定义 DEF_ASM_OP1(name, opcode, group, instr_type, op0)
#定义 DEF_ASM_OP2(name, opcode, group, instr_type, op0, op1)
#定义 DEF_ASM_OP3(name, opcode, group, instr_type, op0, op1, op2)
#如定义 TCC_TARGET_X86_64
# 包含 "x86_64-asm.h"
#另
# 包含 "i386-asm.h"
#了如
};

静态 内联 整 get_reg_shift(TCCState *s1)
{
    整 shift, v;
    v = asm_int_expr(s1);
    转接(v) {
    事例 1:
        shift = 0;
        跳出;
    事例 2:
        shift = 1;
        跳出;
    事例 4:
        shift = 2;
        跳出;
    事例 8:
        shift = 3;
        跳出;
    缺省:
        expect("1, 2, 4 or 8 constant");
        shift = 0;
        跳出;
    }
    返回 shift;
}

#如定义 TCC_TARGET_X86_64
静态 整 asm_parse_numeric_reg(整 t, 无符 整 *type)
{
    整 reg = -1;
    如 (t >= TOK_IDENT && t < tok_ident) {
        不变 字 *s = table_ident[t - TOK_IDENT]->str;
        字 c;
        *type = OP_REG64;
        如 (*s == 'c') {
            s++;
            *type = OP_CR;
        }
        如 (*s++ != 'r')
          返回 -1;
        /* Don't allow leading '0'.  */
        如 ((c = *s++) >= '1' && c <= '9')
          reg = c - '0';
        另
          返回 -1;
        如 ((c = *s) >= '0' && c <= '5')
          s++, reg = reg * 10 + c - '0';
        如 (reg > 15)
          返回 -1;
        如 ((c = *s) == 0)
          ;
        另 如 (*type != OP_REG64)
          返回 -1;
        另 如 (c == 'b' && !s[1])
          *type = OP_REG8;
        另 如 (c == 'w' && !s[1])
          *type = OP_REG16;
        另 如 (c == 'd' && !s[1])
          *type = OP_REG32;
        另
          返回 -1;
    }
    返回 reg;
}
#了如

静态 整 asm_parse_reg(无符 整 *type)
{
    整 reg = 0;
    *type = 0;
    如 (tok != '%')
        跳转 error_32;
    next();
    如 (tok >= TOK_ASM_eax && tok <= TOK_ASM_edi) {
        reg = tok - TOK_ASM_eax;
        *type = OP_REG32;
#如定义 TCC_TARGET_X86_64
    } 另 如 (tok >= TOK_ASM_rax && tok <= TOK_ASM_rdi) {
        reg = tok - TOK_ASM_rax;
        *type = OP_REG64;
    } 另 如 (tok == TOK_ASM_rip) {
        reg = -2; /* Probably should use different escape code. */
        *type = OP_REG64;
    } 另 如 ((reg = asm_parse_numeric_reg(tok, type)) >= 0
               && (*type == OP_REG32 || *type == OP_REG64)) {
        ;
#了如
    } 另 {
    error_32:
        expect("register");
    }
    next();
    返回 reg;
}

静态 空 parse_operand(TCCState *s1, Operand *op)
{
    ExprValue e;
    整 reg, indir;
    不变 字 *p;

    indir = 0;
    如 (tok == '*') {
        next();
        indir = OP_INDIR;
    }

    如 (tok == '%') {
        next();
        如 (tok >= TOK_ASM_al && tok <= TOK_ASM_db7) {
            reg = tok - TOK_ASM_al;
            op->type = 1 << (reg >> 3); /* WARNING: do not change constant order */
            op->reg = reg & 7;
            如 ((op->type & OP_REG) && op->reg == TREG_XAX)
                op->type |= OP_EAX;
            另 如 (op->type == OP_REG8 && op->reg == TREG_XCX)
                op->type |= OP_CL;
            另 如 (op->type == OP_REG16 && op->reg == TREG_XDX)
                op->type |= OP_DX;
        } 另 如 (tok >= TOK_ASM_dr0 && tok <= TOK_ASM_dr7) {
            op->type = OP_DB;
            op->reg = tok - TOK_ASM_dr0;
        } 另 如 (tok >= TOK_ASM_es && tok <= TOK_ASM_gs) {
            op->type = OP_SEG;
            op->reg = tok - TOK_ASM_es;
        } 另 如 (tok == TOK_ASM_st) {
            op->type = OP_ST;
            op->reg = 0;
            next();
            如 (tok == '(') {
                next();
                如 (tok != TOK_PPNUM)
                    跳转 reg_error;
                p = tokc.str.data;
                reg = p[0] - '0';
                如 ((无符)reg >= 8 || p[1] != '\0')
                    跳转 reg_error;
                op->reg = reg;
                next();
                skip(')');
            }
            如 (op->reg == 0)
                op->type |= OP_ST0;
            跳转 no_skip;
#如定义 TCC_TARGET_X86_64
        } 另 如 (tok >= TOK_ASM_spl && tok <= TOK_ASM_dil) {
            op->type = OP_REG8 | OP_REG8_LOW;
            op->reg = 4 + tok - TOK_ASM_spl;
        } 另 如 ((op->reg = asm_parse_numeric_reg(tok, &op->type)) >= 0) {
            ;
#了如
        } 另 {
        reg_error:
            tcc_error("unknown register %%%s", get_tok_str(tok, &tokc));
        }
        next();
    no_skip: ;
    } 另 如 (tok == '$') {
        /* constant value */
        next();
        asm_expr(s1, &e);
        op->type = OP_IM32;
        op->e = e;
        如 (!op->e.sym) {
            如 (op->e.v == (uint8_t)op->e.v)
                op->type |= OP_IM8;
            如 (op->e.v == (int8_t)op->e.v)
                op->type |= OP_IM8S;
            如 (op->e.v == (uint16_t)op->e.v)
                op->type |= OP_IM16;
#如定义 TCC_TARGET_X86_64
            如 (op->e.v != (int32_t)op->e.v && op->e.v != (uint32_t)op->e.v)
                op->type = OP_IM64;
#了如
        }
    } 另 {
        /* address(reg,reg2,shift) with all variants */
        op->type = OP_EA;
        op->reg = -1;
        op->reg2 = -1;
        op->shift = 0;
        如 (tok != '(') {
            asm_expr(s1, &e);
            op->e = e;
        } 另 {
            next();
            如 (tok == '%') {
                unget_tok('(');
                op->e.v = 0;
                op->e.sym = NULL;
            } 另 {
                /* bracketed offset expression */
                asm_expr(s1, &e);
                如 (tok != ')')
                    expect(")");
                next();
                op->e.v = e.v;
                op->e.sym = e.sym;
            }
            op->e.pcrel = 0;
        }
        如 (tok == '(') {
            无符 整 type = 0;
            next();
            如 (tok != ',') {
                op->reg = asm_parse_reg(&type);
            }
            如 (tok == ',') {
                next();
                如 (tok != ',') {
                    op->reg2 = asm_parse_reg(&type);
                }
                如 (tok == ',') {
                    next();
                    op->shift = get_reg_shift(s1);
                }
            }
            如 (type & OP_REG32)
                op->type |= OP_EA32;
            skip(')');
        }
        如 (op->reg == -1 && op->reg2 == -1)
            op->type |= OP_ADDR;
    }
    op->type |= indir;
}

/* XXX: unify with C code output ? */
ST_FUNC 空 gen_expr32(ExprValue *pe)
{
    如 (pe->pcrel)
        /* If PC-relative, always set VT_SYM, even without symbol,
           so as to force a relocation to be emitted.  */
        gen_addrpc32(VT_SYM, pe->sym, pe->v);
    另
        gen_addr32(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
}

#如定义 TCC_TARGET_X86_64
ST_FUNC 空 gen_expr64(ExprValue *pe)
{
    gen_addr64(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
}
#了如

/* XXX: unify with C code output ? */
静态 空 gen_disp32(ExprValue *pe)
{
    Sym *sym = pe->sym;
    如 (sym && sym->r == cur_text_section->sh_num) {
        /* same section: we can output an absolute value. Note
           that the TCC compiler behaves differently here because
           it always outputs a relocation to ease (future) code
           elimination in the linker */
        gen_le32(pe->v + sym->jnext - ind - 4);
    } 另 {
        如 (sym && sym->type.t == VT_VOID) {
            sym->type.t = VT_FUNC;
            sym->type.ref = NULL;
        }
        gen_addrpc32(VT_SYM, sym, pe->v);
    }
}

/* generate the modrm operand */
静态 内联 整 asm_modrm(整 reg, Operand *op)
{
    整 mod, reg1, reg2, sib_reg1;

    如 (op->type & (OP_REG | OP_MMX | OP_SSE)) {
        g(0xc0 + (reg << 3) + op->reg);
    } 另 如 (op->reg == -1 && op->reg2 == -1) {
        /* displacement only */
#如定义 TCC_TARGET_X86_64
        g(0x04 + (reg << 3));
        g(0x25);
#另
        g(0x05 + (reg << 3));
#了如
        gen_expr32(&op->e);
#如定义 TCC_TARGET_X86_64
    } 另 如 (op->reg == -2) {
        ExprValue *pe = &op->e;
        g(0x05 + (reg << 3));
        gen_addrpc32(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
        返回 ind;
#了如
    } 另 {
        sib_reg1 = op->reg;
        /* fist compute displacement encoding */
        如 (sib_reg1 == -1) {
            sib_reg1 = 5;
            mod = 0x00;
        } 另 如 (op->e.v == 0 && !op->e.sym && op->reg != 5) {
            mod = 0x00;
        } 另 如 (op->e.v == (int8_t)op->e.v && !op->e.sym) {
            mod = 0x40;
        } 另 {
            mod = 0x80;
        }
        /* compute if sib byte needed */
        reg1 = op->reg;
        如 (op->reg2 != -1)
            reg1 = 4;
        g(mod + (reg << 3) + reg1);
        如 (reg1 == 4) {
            /* add sib byte */
            reg2 = op->reg2;
            如 (reg2 == -1)
                reg2 = 4; /* indicate no index */
            g((op->shift << 6) + (reg2 << 3) + sib_reg1);
        }
        /* add offset */
        如 (mod == 0x40) {
            g(op->e.v);
        } 另 如 (mod == 0x80 || op->reg == -1) {
            gen_expr32(&op->e);
        }
    }
    返回 0;
}

#如定义 TCC_TARGET_X86_64
#定义 REX_W 0x48
#定义 REX_R 0x44
#定义 REX_X 0x42
#定义 REX_B 0x41

静态 空 asm_rex(整 width64, Operand *ops, 整 nb_ops, 整 *op_type,
                    整 regi, 整 rmi)
{
  无符 字 rex = width64 ? 0x48 : 0;
  整 saw_high_8bit = 0;
  整 i;
  如 (rmi == -1) {
      /* No mod/rm byte, but we might have a register op nevertheless
         (we will add it to the opcode later).  */
      对于(i = 0; i < nb_ops; i++) {
          如 (op_type[i] & (OP_REG | OP_ST)) {
              如 (ops[i].reg >= 8) {
                  rex |= REX_B;
                  ops[i].reg -= 8;
              } 另 如 (ops[i].type & OP_REG8_LOW)
                  rex |= 0x40;
              另 如 (ops[i].type & OP_REG8 && ops[i].reg >= 4)
                  /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
                  saw_high_8bit = ops[i].reg;
              跳出;
          }
      }
  } 另 {
      如 (regi != -1) {
          如 (ops[regi].reg >= 8) {
              rex |= REX_R;
              ops[regi].reg -= 8;
          } 另 如 (ops[regi].type & OP_REG8_LOW)
              rex |= 0x40;
          另 如 (ops[regi].type & OP_REG8 && ops[regi].reg >= 4)
              /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
              saw_high_8bit = ops[regi].reg;
      }
      如 (ops[rmi].type & (OP_REG | OP_MMX | OP_SSE | OP_CR | OP_EA)) {
          如 (ops[rmi].reg >= 8) {
              rex |= REX_B;
              ops[rmi].reg -= 8;
          } 另 如 (ops[rmi].type & OP_REG8_LOW)
              rex |= 0x40;
          另 如 (ops[rmi].type & OP_REG8 && ops[rmi].reg >= 4)
              /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
              saw_high_8bit = ops[rmi].reg;
      }
      如 (ops[rmi].type & OP_EA && ops[rmi].reg2 >= 8) {
          rex |= REX_X;
          ops[rmi].reg2 -= 8;
      }
  }
  如 (rex) {
      如 (saw_high_8bit)
          tcc_error("can't encode register %%%ch when REX prefix is required",
                    "acdb"[saw_high_8bit-4]);
      g(rex);
  }
}
#了如

静态 空 maybe_print_stats (空)
{
  静态 整 already = 1;
  如 (!already)
    /* print stats about opcodes */
    {
        不变 结构 ASMInstr *pa;
        整 freq[4];
        整 op_vals[500];
        整 nb_op_vals, i, j;

        already = 1;
        nb_op_vals = 0;
        memset(freq, 0, 求长度(freq));
        对于(pa = asm_instrs; pa->sym != 0; pa++) {
            freq[pa->nb_ops]++;
            //对于(i=0;i<pa->nb_ops;i++) {
                对于(j=0;j<nb_op_vals;j++) {
                    //如 (pa->op_type[i] == op_vals[j])
                    如 (pa->instr_type == op_vals[j])
                        跳转 found;
                }
                //op_vals[nb_op_vals++] = pa->op_type[i];
                op_vals[nb_op_vals++] = pa->instr_type;
            found: ;
            //}
        }
        对于(i=0;i<nb_op_vals;i++) {
            整 v = op_vals[i];
            //如 ((v & (v - 1)) != 0)
                printf("%3d: %08x\n", i, v);
        }
        printf("size=%d nb=%d f0=%d f1=%d f2=%d f3=%d\n",
               (整)求长度(asm_instrs),
               (整)求长度(asm_instrs) / (整)求长度(ASMInstr),
               freq[0], freq[1], freq[2], freq[3]);
    }
}

ST_FUNC 空 asm_opcode(TCCState *s1, 整 opcode)
{
    不变 ASMInstr *pa;
    整 i, modrm_index, modreg_index, reg, v, op1, seg_prefix, pc;
    整 nb_ops, s;
    Operand ops[MAX_OPERANDS], *pop;
    整 op_type[3]; /* decoded op type */
    整 alltypes;   /* OR of all operand types */
    整 autosize;
    整 p66;
#如定义 TCC_TARGET_X86_64
    整 rex64;
#了如

    maybe_print_stats();
    /* force synthetic ';' after prefix instruction, so we can handle */
    /* one-line things like "rep stosb" instead of only "rep\nstosb" */
    如 (opcode >= TOK_ASM_wait && opcode <= TOK_ASM_repnz)
        unget_tok(';');

    /* get operands */
    pop = ops;
    nb_ops = 0;
    seg_prefix = 0;
    alltypes = 0;
    对于(;;) {
        如 (tok == ';' || tok == TOK_LINEFEED)
            跳出;
        如 (nb_ops >= MAX_OPERANDS) {
            tcc_error("incorrect number of operands");
        }
        parse_operand(s1, pop);
        如 (tok == ':') {
           如 (pop->type != OP_SEG || seg_prefix)
               tcc_error("incorrect prefix");
           seg_prefix = segment_prefixes[pop->reg];
           next();
           parse_operand(s1, pop);
           如 (!(pop->type & OP_EA)) {
               tcc_error("segment prefix must be followed by memory reference");
           }
        }
        pop++;
        nb_ops++;
        如 (tok != ',')
            跳出;
        next();
    }

    s = 0; /* avoid warning */

    /* optimize matching by using a lookup table (no hashing is needed
       !) */
    对于(pa = asm_instrs; pa->sym != 0; pa++) {
        整 it = pa->instr_type & OPCT_MASK;
        s = 0;
        如 (it == OPC_FARITH) {
            v = opcode - pa->sym;
            如 (!((无符)v < 8 * 6 && (v % 6) == 0))
                继续;
        } 另 如 (it == OPC_ARITH) {
            如 (!(opcode >= pa->sym && opcode < pa->sym + 8*NBWLX))
                继续;
            s = (opcode - pa->sym) % NBWLX;
            如 ((pa->instr_type & OPC_BWLX) == OPC_WLX)
              {
                /* We need to reject the xxxb opcodes that we accepted above.
                   Note that pa->sym for WLX opcodes is the 'w' token,
                   to get the 'b' token subtract one.  */
                如 (((opcode - pa->sym + 1) % NBWLX) == 0)
                    继续;
                s++;
              }
        } 另 如 (it == OPC_SHIFT) {
            如 (!(opcode >= pa->sym && opcode < pa->sym + 7*NBWLX))
                继续;
            s = (opcode - pa->sym) % NBWLX;
        } 另 如 (it == OPC_TEST) {
            如 (!(opcode >= pa->sym && opcode < pa->sym + NB_TEST_OPCODES))
                继续;
            /* cmovxx is a test opcode but accepts multiple sizes.
               TCC doesn't accept the suffixed mnemonic, instead we 
               simply force size autodetection always.  */
            如 (pa->instr_type & OPC_WLX)
                s = NBWLX - 1;
        } 另 如 (pa->instr_type & OPC_B) {
#如定义 TCC_TARGET_X86_64
            /* Some instructions don't have the full size but only
               bwl form.  insb e.g. */
            如 ((pa->instr_type & OPC_WLQ) != OPC_WLQ
                && !(opcode >= pa->sym && opcode < pa->sym + NBWLX-1))
                继续;
#了如
            如 (!(opcode >= pa->sym && opcode < pa->sym + NBWLX))
                继续;
            s = opcode - pa->sym;
        } 另 如 (pa->instr_type & OPC_WLX) {
            如 (!(opcode >= pa->sym && opcode < pa->sym + NBWLX-1))
                继续;
            s = opcode - pa->sym + 1;
        } 另 {
            如 (pa->sym != opcode)
                继续;
        }
        如 (pa->nb_ops != nb_ops)
            继续;
#如定义 TCC_TARGET_X86_64
        /* Special case for moves.  Selecting the IM64->REG64 form
           should only be done if we really have an >32bit imm64, and that
           is hardcoded.  Ignore it here.  */
        如 (pa->opcode == 0xb0 && ops[0].type != OP_IM64
            && (ops[1].type & OP_REG) == OP_REG64
            && !(pa->instr_type & OPC_0F))
            继续;
#了如
        /* now decode and check each operand */
        alltypes = 0;
        对于(i = 0; i < nb_ops; i++) {
            整 op1, op2;
            op1 = pa->op_type[i];
            op2 = op1 & 0x1f;
            转接(op2) {
            事例 OPT_IM:
                v = OP_IM8 | OP_IM16 | OP_IM32;
                跳出;
            事例 OPT_REG:
                v = OP_REG8 | OP_REG16 | OP_REG32 | OP_REG64;
                跳出;
            事例 OPT_REGW:
                v = OP_REG16 | OP_REG32 | OP_REG64;
                跳出;
            事例 OPT_IMW:
                v = OP_IM16 | OP_IM32;
                跳出;
            事例 OPT_MMXSSE:
                v = OP_MMX | OP_SSE;
                跳出;
            事例 OPT_DISP:
            事例 OPT_DISP8:
                v = OP_ADDR;
                跳出;
            缺省:
                v = 1 << op2;
                跳出;
            }
            如 (op1 & OPT_EA)
                v |= OP_EA;
            op_type[i] = v;
            如 ((ops[i].type & v) == 0)
                跳转 next;
            alltypes |= ops[i].type;
        }
        /* all is matching ! */
        跳出;
    next: ;
    }
    如 (pa->sym == 0) {
        如 (opcode >= TOK_ASM_first && opcode <= TOK_ASM_last) {
            整 b;
            b = op0_codes[opcode - TOK_ASM_first];
            如 (b & 0xff00) 
                g(b >> 8);
            g(b);
            返回;
        } 另 如 (opcode <= TOK_ASM_alllast) {
            tcc_error("bad operand with opcode '%s'",
                  get_tok_str(opcode, NULL));
        } 另 {
            tcc_error("unknown opcode '%s'",
                  get_tok_str(opcode, NULL));
        }
    }
    /* if the size is unknown, then evaluate it (OPC_B or OPC_WL case) */
    autosize = NBWLX-1;
#如定义 TCC_TARGET_X86_64
    /* XXX the autosize should rather be zero, to not have to adjust this
       all the time.  */
    如 ((pa->instr_type & OPC_BWLQ) == OPC_B)
        autosize = NBWLX-2;
#了如
    如 (s == autosize) {
        /* Check for register operands providing hints about the size.
           Start from the end, i.e. destination operands.  This matters
           only for opcodes accepting different sized registers, lar and lsl
           are such opcodes.  */
        对于(i = nb_ops - 1; s == autosize && i >= 0; i--) {
            如 ((ops[i].type & OP_REG) && !(op_type[i] & (OP_CL | OP_DX)))
                s = reg_to_size[ops[i].type & OP_REG];
        }
        如 (s == autosize) {
            如 ((opcode == TOK_ASM_push || opcode == TOK_ASM_pop) &&
                (ops[0].type & (OP_SEG | OP_IM8S | OP_IM32)))
                s = 2;
            另 如 ((opcode == TOK_ASM_push || opcode == TOK_ASM_pop) &&
                     (ops[0].type & OP_EA))
                s = NBWLX - 2;
            另
                tcc_error("cannot infer opcode suffix");
        }
    }

#如定义 TCC_TARGET_X86_64
    /* Generate addr32 prefix if needed */
    对于(i = 0; i < nb_ops; i++) {
        如 (ops[i].type & OP_EA32) {
            g(0x67);
            跳出;
        }
    }
#了如
    /* generate data16 prefix if needed */
    p66 = 0;
    如 (s == 1)
        p66 = 1;
    另 {
        /* accepting mmx+sse in all operands --> needs 0x66 to
           switch to sse mode.  Accepting only sse in an operand --> is
           already SSE insn and needs 0x66/f2/f3 handling.  */
        对于 (i = 0; i < nb_ops; i++)
            如 ((op_type[i] & (OP_MMX | OP_SSE)) == (OP_MMX | OP_SSE)
                && ops[i].type & OP_SSE)
                p66 = 1;
    }
    如 (p66)
        g(0x66);
#如定义 TCC_TARGET_X86_64
    rex64 = 0;
    如 (pa->instr_type & OPC_48)
        rex64 = 1;
    另 如 (s == 3 || (alltypes & OP_REG64)) {
        /* generate REX prefix */
        整 default64 = 0;
        对于(i = 0; i < nb_ops; i++) {
            如 (op_type[i] == OP_REG64 && pa->opcode != 0xb8) {
                /* If only 64bit regs are accepted in one operand
                   this is a default64 instruction without need for
                   REX prefixes, except for movabs(0xb8).  */
                default64 = 1;
                跳出;
            }
        }
        /* XXX find better encoding for the default64 instructions.  */
        如 (((opcode != TOK_ASM_push && opcode != TOK_ASM_pop
              && opcode != TOK_ASM_pushw && opcode != TOK_ASM_pushl
              && opcode != TOK_ASM_pushq && opcode != TOK_ASM_popw
              && opcode != TOK_ASM_popl && opcode != TOK_ASM_popq
              && opcode != TOK_ASM_call && opcode != TOK_ASM_jmp))
            && !default64)
            rex64 = 1;
    }
#了如

    /* now generates the operation */
    如 (OPCT_IS(pa->instr_type, OPC_FWAIT))
        g(0x9b);
    如 (seg_prefix)
        g(seg_prefix);

    v = pa->opcode;
    如 (pa->instr_type & OPC_0F)
        v = ((v & ~0xff) << 8) | 0x0f00 | (v & 0xff);
    如 ((v == 0x69 || v == 0x6b) && nb_ops == 2) {
        /* kludge for imul $im, %reg */
        nb_ops = 3;
        ops[2] = ops[1];
        op_type[2] = op_type[1];
    } 另 如 (v == 0xcd && ops[0].e.v == 3 && !ops[0].e.sym) {
        v--; /* int $3 case */
        nb_ops = 0;
    } 另 如 ((v == 0x06 || v == 0x07)) {
        如 (ops[0].reg >= 4) {
            /* push/pop %fs or %gs */
            v = 0x0fa0 + (v - 0x06) + ((ops[0].reg - 4) << 3);
        } 另 {
            v += ops[0].reg << 3;
        }
        nb_ops = 0;
    } 另 如 (v <= 0x05) {
        /* arith case */
        v += ((opcode - TOK_ASM_addb) / NBWLX) << 3;
    } 另 如 ((pa->instr_type & (OPCT_MASK | OPC_MODRM)) == OPC_FARITH) {
        /* fpu arith case */
        v += ((opcode - pa->sym) / 6) << 3;
    }

    /* search which operand will be used for modrm */
    modrm_index = -1;
    modreg_index = -1;
    如 (pa->instr_type & OPC_MODRM) {
        如 (!nb_ops) {
            /* A modrm opcode without operands is a special case (e.g. mfence).
               It has a group and acts as if there's an register operand 0
               (ax).  */
            i = 0;
            ops[i].type = OP_REG;
            ops[i].reg = 0;
            跳转 modrm_found;
        }
        /* first look for an ea operand */
        对于(i = 0;i < nb_ops; i++) {
            如 (op_type[i] & OP_EA)
                跳转 modrm_found;
        }
        /* then if not found, a register or indirection (shift instructions) */
        对于(i = 0;i < nb_ops; i++) {
            如 (op_type[i] & (OP_REG | OP_MMX | OP_SSE | OP_INDIR))
                跳转 modrm_found;
        }
#如定义 ASM_DEBUG
        tcc_error("bad op table");
#了如
    modrm_found:
        modrm_index = i;
        /* if a register is used in another operand then it is
           used instead of group */
        对于(i = 0;i < nb_ops; i++) {
            整 t = op_type[i];
            如 (i != modrm_index &&
                (t & (OP_REG | OP_MMX | OP_SSE | OP_CR | OP_TR | OP_DB | OP_SEG))) {
                modreg_index = i;
                跳出;
            }
        }
    }
#如定义 TCC_TARGET_X86_64
    asm_rex (rex64, ops, nb_ops, op_type, modreg_index, modrm_index);
#了如

    如 (pa->instr_type & OPC_REG) {
        /* mov $im, %reg case */
        如 (v == 0xb0 && s >= 1)
            v += 7;
        对于(i = 0; i < nb_ops; i++) {
            如 (op_type[i] & (OP_REG | OP_ST)) {
                v += ops[i].reg;
                跳出;
            }
        }
    }
    如 (pa->instr_type & OPC_B)
        v += s >= 1;
    如 (nb_ops == 1 && pa->op_type[0] == OPT_DISP8) {
        Sym *sym;
        整 jmp_disp;

        /* see if we can really generate the jump with a byte offset */
        sym = ops[0].e.sym;
        如 (!sym)
            跳转 no_short_jump;
        如 (sym->r != cur_text_section->sh_num)
            跳转 no_short_jump;
        jmp_disp = ops[0].e.v + sym->jnext - ind - 2 - (v >= 0xff);
        如 (jmp_disp == (int8_t)jmp_disp) {
            /* OK to generate jump */
            ops[0].e.sym = 0;
            ops[0].e.v = jmp_disp;
            op_type[0] = OP_IM8S;
        } 另 {
        no_short_jump:
            /* long jump will be allowed. need to modify the
               opcode slightly */
            如 (v == 0xeb) /* jmp */
                v = 0xe9;
            另 如 (v == 0x70) /* jcc */
                v += 0x0f10;
            另
                tcc_error("invalid displacement");
        }
    }
    如 (OPCT_IS(pa->instr_type, OPC_TEST))
        v += test_bits[opcode - pa->sym];
    op1 = v >> 16;
    如 (op1)
        g(op1);
    op1 = (v >> 8) & 0xff;
    如 (op1)
        g(op1);
    g(v);

    如 (OPCT_IS(pa->instr_type, OPC_SHIFT)) {
        reg = (opcode - pa->sym) / NBWLX;
        如 (reg == 6)
            reg = 7;
    } 另 如 (OPCT_IS(pa->instr_type, OPC_ARITH)) {
        reg = (opcode - pa->sym) / NBWLX;
    } 另 如 (OPCT_IS(pa->instr_type, OPC_FARITH)) {
        reg = (opcode - pa->sym) / 6;
    } 另 {
        reg = (pa->instr_type >> OPC_GROUP_SHIFT) & 7;
    }

    pc = 0;
    如 (pa->instr_type & OPC_MODRM) {
        /* if a register is used in another operand then it is
           used instead of group */
        如 (modreg_index >= 0)
            reg = ops[modreg_index].reg;
        pc = asm_modrm(reg, &ops[modrm_index]);
    }

    /* emit constants */
#如未定义 TCC_TARGET_X86_64
    如 (!(pa->instr_type & OPC_0F)
        && (pa->opcode == 0x9a || pa->opcode == 0xea)) {
        /* ljmp or lcall kludge */
        gen_expr32(&ops[1].e);
        如 (ops[0].e.sym)
            tcc_error("cannot relocate");
        gen_le16(ops[0].e.v);
        返回;
    }
#了如
    对于(i = 0;i < nb_ops; i++) {
        v = op_type[i];
        如 (v & (OP_IM8 | OP_IM16 | OP_IM32 | OP_IM64 | OP_IM8S | OP_ADDR)) {
            /* if multiple sizes are given it means we must look
               at the op size */
            如 ((v | OP_IM8 | OP_IM64) == (OP_IM8 | OP_IM16 | OP_IM32 | OP_IM64)) {
                如 (s == 0)
                    v = OP_IM8;
                另 如 (s == 1)
                    v = OP_IM16;
                另 如 (s == 2 || (v & OP_IM64) == 0)
                    v = OP_IM32;
                另
                    v = OP_IM64;
            }

            如 ((v & (OP_IM8 | OP_IM8S | OP_IM16)) && ops[i].e.sym)
                tcc_error("cannot relocate");

            如 (v & (OP_IM8 | OP_IM8S)) {
                g(ops[i].e.v);
            } 另 如 (v & OP_IM16) {
                gen_le16(ops[i].e.v);
#如定义 TCC_TARGET_X86_64
            } 另 如 (v & OP_IM64) {
                gen_expr64(&ops[i].e);
#了如
            } 另 如 (pa->op_type[i] == OPT_DISP || pa->op_type[i] == OPT_DISP8) {
                gen_disp32(&ops[i].e);
            } 另 {
                gen_expr32(&ops[i].e);
            }
        }
    }

    /* after immediate operands, adjust pc-relative address */
    如 (pc)
        add32le(cur_text_section->data + pc - 4, pc - ind);
}

/* return the constraint priority (we allocate first the lowest
   numbered constraints) */
静态 内联 整 constraint_priority(不变 字 *str)
{
    整 priority, c, pr;

    /* we take the lowest priority */
    priority = 0;
    对于(;;) {
        c = *str;
        如 (c == '\0')
            跳出;
        str++;
        转接(c) {
        事例 'A':
            pr = 0;
            跳出;
        事例 'a':
        事例 'b':
        事例 'c':
        事例 'd':
        事例 'S':
        事例 'D':
            pr = 1;
            跳出;
        事例 'q':
            pr = 2;
            跳出;
        事例 'r':
        事例 'R':
        事例 'p':
            pr = 3;
            跳出;
        事例 'N':
        事例 'M':
        事例 'I':
        事例 'e':
        事例 'i':
        事例 'm':
        事例 'g':
            pr = 4;
            跳出;
        缺省:
            tcc_error("unknown constraint '%c'", c);
            pr = 0;
        }
        如 (pr > priority)
            priority = pr;
    }
    返回 priority;
}

静态 不变 字 *skip_constraint_modifiers(不变 字 *p)
{
    当 (*p == '=' || *p == '&' || *p == '+' || *p == '%')
        p++;
    返回 p;
}

/* If T (a token) is of the form "%reg" returns the register
   number and type, otherwise return -1.  */
ST_FUNC 整 asm_parse_regvar (整 t)
{
    不变 字 *s;
    Operand op;
    如 (t < TOK_IDENT)
        返回 -1;
    s = table_ident[t - TOK_IDENT]->str;
    如 (s[0] != '%')
        返回 -1;
    t = tok_alloc(s+1, strlen(s)-1)->tok;
    unget_tok(t);
    unget_tok('%');
    parse_operand(tcc_state, &op);
    /* Accept only integer regs for now.  */
    如 (op.type & OP_REG)
        返回 op.reg;
    另
        返回 -1;
}

#定义 REG_OUT_MASK 0x01
#定义 REG_IN_MASK  0x02

#定义 is_reg_allocated(reg) (regs_allocated[reg] & reg_mask)

ST_FUNC 空 asm_compute_constraints(ASMOperand *operands,
                                    整 nb_operands, 整 nb_outputs,
                                    不变 uint8_t *clobber_regs,
                                    整 *pout_reg)
{
    ASMOperand *op;
    整 sorted_op[MAX_ASM_OPERANDS];
    整 i, j, k, p1, p2, tmp, reg, c, reg_mask;
    不变 字 *str;
    uint8_t regs_allocated[NB_ASM_REGS];

    /* init fields */
    对于(i=0;i<nb_operands;i++) {
        op = &operands[i];
        op->input_index = -1;
        op->ref_index = -1;
        op->reg = -1;
        op->is_memory = 0;
        op->is_rw = 0;
    }
    /* compute constraint priority and evaluate references to output
       constraints if input constraints */
    对于(i=0;i<nb_operands;i++) {
        op = &operands[i];
        str = op->constraint;
        str = skip_constraint_modifiers(str);
        如 (isnum(*str) || *str == '[') {
            /* this is a reference to another constraint */
            k = find_constraint(operands, nb_operands, str, NULL);
            如 ((无符)k >= i || i < nb_outputs)
                tcc_error("invalid reference in constraint %d ('%s')",
                      i, str);
            op->ref_index = k;
            如 (operands[k].input_index >= 0)
                tcc_error("cannot reference twice the same operand");
            operands[k].input_index = i;
            op->priority = 5;
        } 另 如 ((op->vt->r & VT_VALMASK) == VT_LOCAL
                   && op->vt->sym
                   && (reg = op->vt->sym->r & VT_VALMASK) < VT_CONST) {
            op->priority = 1;
            op->reg = reg;
        } 另 {
            op->priority = constraint_priority(str);
        }
    }

    /* sort operands according to their priority */
    对于(i=0;i<nb_operands;i++)
        sorted_op[i] = i;
    对于(i=0;i<nb_operands - 1;i++) {
        对于(j=i+1;j<nb_operands;j++) {
            p1 = operands[sorted_op[i]].priority;
            p2 = operands[sorted_op[j]].priority;
            如 (p2 < p1) {
                tmp = sorted_op[i];
                sorted_op[i] = sorted_op[j];
                sorted_op[j] = tmp;
            }
        }
    }

    对于(i = 0;i < NB_ASM_REGS; i++) {
        如 (clobber_regs[i])
            regs_allocated[i] = REG_IN_MASK | REG_OUT_MASK;
        另
            regs_allocated[i] = 0;
    }
    /* esp cannot be used */
    regs_allocated[4] = REG_IN_MASK | REG_OUT_MASK;
    /* ebp cannot be used yet */
    regs_allocated[5] = REG_IN_MASK | REG_OUT_MASK;

    /* allocate registers and generate corresponding asm moves */
    对于(i=0;i<nb_operands;i++) {
        j = sorted_op[i];
        op = &operands[j];
        str = op->constraint;
        /* no need to allocate references */
        如 (op->ref_index >= 0)
            继续;
        /* select if register is used for output, input or both */
        如 (op->input_index >= 0) {
            reg_mask = REG_IN_MASK | REG_OUT_MASK;
        } 另 如 (j < nb_outputs) {
            reg_mask = REG_OUT_MASK;
        } 另 {
            reg_mask = REG_IN_MASK;
        }
        如 (op->reg >= 0) {
            如 (is_reg_allocated(op->reg))
                tcc_error("asm regvar requests register that's taken already");
            reg = op->reg;
            跳转 reg_found;
        }
    try_next:
        c = *str++;
        转接(c) {
        事例 '=':
            跳转 try_next;
        事例 '+':
            op->is_rw = 1;
            /* FALL THRU */
        事例 '&':
            如 (j >= nb_outputs)
                tcc_error("'%c' modifier can only be applied to outputs", c);
            reg_mask = REG_IN_MASK | REG_OUT_MASK;
            跳转 try_next;
        事例 'A':
            /* allocate both eax and edx */
            如 (is_reg_allocated(TREG_XAX) ||
                is_reg_allocated(TREG_XDX))
                跳转 try_next;
            op->is_llong = 1;
            op->reg = TREG_XAX;
            regs_allocated[TREG_XAX] |= reg_mask;
            regs_allocated[TREG_XDX] |= reg_mask;
            跳出;
        事例 'a':
            reg = TREG_XAX;
            跳转 alloc_reg;
        事例 'b':
            reg = 3;
            跳转 alloc_reg;
        事例 'c':
            reg = TREG_XCX;
            跳转 alloc_reg;
        事例 'd':
            reg = TREG_XDX;
            跳转 alloc_reg;
        事例 'S':
            reg = 6;
            跳转 alloc_reg;
        事例 'D':
            reg = 7;
        alloc_reg:
            如 (is_reg_allocated(reg))
                跳转 try_next;
            跳转 reg_found;
        事例 'q':
            /* eax, ebx, ecx or edx */
            对于(reg = 0; reg < 4; reg++) {
                如 (!is_reg_allocated(reg))
                    跳转 reg_found;
            }
            跳转 try_next;
        事例 'r':
        事例 'R':
        事例 'p': /* A general address, for x86(64) any register is acceptable*/
            /* any general register */
            对于(reg = 0; reg < 8; reg++) {
                如 (!is_reg_allocated(reg))
                    跳转 reg_found;
            }
            跳转 try_next;
        reg_found:
            /* now we can reload in the register */
            op->is_llong = 0;
            op->reg = reg;
            regs_allocated[reg] |= reg_mask;
            跳出;
        事例 'e':
        事例 'i':
            如 (!((op->vt->r & (VT_VALMASK | VT_LVAL)) == VT_CONST))
                跳转 try_next;
            跳出;
        事例 'I':
        事例 'N':
        事例 'M':
            如 (!((op->vt->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST))
                跳转 try_next;
            跳出;
        事例 'm':
        事例 'g':
            /* nothing special to do because the operand is already in
               memory, except if the pointer itself is stored in a
               memory variable (VT_LLOCAL case) */
            /* XXX: fix constant case */
            /* if it is a reference to a memory zone, it must lie
               in a register, so we reserve the register in the
               input registers and a load will be generated
               later */
            如 (j < nb_outputs || c == 'm') {
                如 ((op->vt->r & VT_VALMASK) == VT_LLOCAL) {
                    /* any general register */
                    对于(reg = 0; reg < 8; reg++) {
                        如 (!(regs_allocated[reg] & REG_IN_MASK))
                            跳转 reg_found1;
                    }
                    跳转 try_next;
                reg_found1:
                    /* now we can reload in the register */
                    regs_allocated[reg] |= REG_IN_MASK;
                    op->reg = reg;
                    op->is_memory = 1;
                }
            }
            跳出;
        缺省:
            tcc_error("asm constraint %d ('%s') could not be satisfied",
                  j, op->constraint);
            跳出;
        }
        /* if a reference is present for that operand, we assign it too */
        如 (op->input_index >= 0) {
            operands[op->input_index].reg = op->reg;
            operands[op->input_index].is_llong = op->is_llong;
        }
    }

    /* compute out_reg. It is used to store outputs registers to memory
       locations references by pointers (VT_LLOCAL case) */
    *pout_reg = -1;
    对于(i=0;i<nb_operands;i++) {
        op = &operands[i];
        如 (op->reg >= 0 &&
            (op->vt->r & VT_VALMASK) == VT_LLOCAL  &&
            !op->is_memory) {
            对于(reg = 0; reg < 8; reg++) {
                如 (!(regs_allocated[reg] & REG_OUT_MASK))
                    跳转 reg_found2;
            }
            tcc_error("could not find free output register for reloading");
        reg_found2:
            *pout_reg = reg;
            跳出;
        }
    }

    /* print sorted constraints */
#如定义 ASM_DEBUG
    对于(i=0;i<nb_operands;i++) {
        j = sorted_op[i];
        op = &operands[j];
        printf("%%%d [%s]: \"%s\" r=0x%04x reg=%d\n",
               j,
               op->id ? get_tok_str(op->id, NULL) : "",
               op->constraint,
               op->vt->r,
               op->reg);
    }
    如 (*pout_reg >= 0)
        printf("out_reg=%d\n", *pout_reg);
#了如
}

ST_FUNC 空 subst_asm_operand(CString *add_str,
                              SValue *sv, 整 modifier)
{
    整 r, reg, size, val;
    字 buf[64];

    r = sv->r;
    如 ((r & VT_VALMASK) == VT_CONST) {
        如 (!(r & VT_LVAL) && modifier != 'c' && modifier != 'n' &&
            modifier != 'P')
            cstr_ccat(add_str, '$');
        如 (r & VT_SYM) {
            不变 字 *name = get_tok_str(sv->sym->v, NULL);
            如 (sv->sym->v >= SYM_FIRST_ANOM) {
                /* In case of anonymous symbols ("L.42", used
                   for static data labels) we can't find them
                   in the C symbol table when later looking up
                   this name.  So enter them now into the asm label
                   list when we still know the symbol.  */
                get_asm_sym(tok_alloc(name, strlen(name))->tok, sv->sym);
            }
            cstr_cat(add_str, name, -1);
            如 ((uint32_t)sv->c.i == 0)
                跳转 no_offset;
            cstr_ccat(add_str, '+');
        }
        val = sv->c.i;
        如 (modifier == 'n')
            val = -val;
        snprintf(buf, 求长度(buf), "%d", (整)sv->c.i);
        cstr_cat(add_str, buf, -1);
    no_offset:;
#如定义 TCC_TARGET_X86_64
        如 (r & VT_LVAL)
            cstr_cat(add_str, "(%rip)", -1);
#了如
    } 另 如 ((r & VT_VALMASK) == VT_LOCAL) {
#如定义 TCC_TARGET_X86_64
        snprintf(buf, 求长度(buf), "%d(%%rbp)", (整)sv->c.i);
#另
        snprintf(buf, 求长度(buf), "%d(%%ebp)", (整)sv->c.i);
#了如
        cstr_cat(add_str, buf, -1);
    } 另 如 (r & VT_LVAL) {
        reg = r & VT_VALMASK;
        如 (reg >= VT_CONST)
            tcc_error("internal compiler error");
        snprintf(buf, 求长度(buf), "(%%%s)",
#如定义 TCC_TARGET_X86_64
                 get_tok_str(TOK_ASM_rax + reg, NULL)
#另
                 get_tok_str(TOK_ASM_eax + reg, NULL)
#了如
                 );
        cstr_cat(add_str, buf, -1);
    } 另 {
        /* register case */
        reg = r & VT_VALMASK;
        如 (reg >= VT_CONST)
            tcc_error("internal compiler error");

        /* choose register operand size */
        如 ((sv->type.t & VT_BTYPE) == VT_BYTE ||
            (sv->type.t & VT_BTYPE) == VT_BOOL)
            size = 1;
        另 如 ((sv->type.t & VT_BTYPE) == VT_SHORT)
            size = 2;
#如定义 TCC_TARGET_X86_64
        另 如 ((sv->type.t & VT_BTYPE) == VT_LLONG ||
                 (sv->type.t & VT_BTYPE) == VT_PTR)
            size = 8;
#了如
        另
            size = 4;
        如 (size == 1 && reg >= 4)
            size = 4;

        如 (modifier == 'b') {
            如 (reg >= 4)
                tcc_error("cannot use byte register");
            size = 1;
        } 另 如 (modifier == 'h') {
            如 (reg >= 4)
                tcc_error("cannot use byte register");
            size = -1;
        } 另 如 (modifier == 'w') {
            size = 2;
        } 另 如 (modifier == 'k') {
            size = 4;
#如定义 TCC_TARGET_X86_64
        } 另 如 (modifier == 'q') {
            size = 8;
#了如
        }

        转接(size) {
        事例 -1:
            reg = TOK_ASM_ah + reg;
            跳出;
        事例 1:
            reg = TOK_ASM_al + reg;
            跳出;
        事例 2:
            reg = TOK_ASM_ax + reg;
            跳出;
        缺省:
            reg = TOK_ASM_eax + reg;
            跳出;
#如定义 TCC_TARGET_X86_64
        事例 8:
            reg = TOK_ASM_rax + reg;
            跳出;
#了如
        }
        snprintf(buf, 求长度(buf), "%%%s", get_tok_str(reg, NULL));
        cstr_cat(add_str, buf, -1);
    }
}

/* generate prolog and epilog code for asm statement */
ST_FUNC 空 asm_gen_code(ASMOperand *operands, 整 nb_operands,
                         整 nb_outputs, 整 is_output,
                         uint8_t *clobber_regs,
                         整 out_reg)
{
    uint8_t regs_allocated[NB_ASM_REGS];
    ASMOperand *op;
    整 i, reg;

    /* Strictly speaking %Xbp and %Xsp should be included in the
       call-preserved registers, but currently it doesn't matter.  */
#如定义 TCC_TARGET_X86_64
#如定义 TCC_TARGET_PE
    静态 uint8_t reg_saved[] = { 3, 6, 7, 12, 13, 14, 15 };
#另
    静态 uint8_t reg_saved[] = { 3, 12, 13, 14, 15 };
#了如
#另
    静态 uint8_t reg_saved[] = { 3, 6, 7 };
#了如

    /* mark all used registers */
    memcpy(regs_allocated, clobber_regs, 求长度(regs_allocated));
    对于(i = 0; i < nb_operands;i++) {
        op = &operands[i];
        如 (op->reg >= 0)
            regs_allocated[op->reg] = 1;
    }
    如 (!is_output) {
        /* generate reg save code */
        对于(i = 0; i < 求长度(reg_saved)/求长度(reg_saved[0]); i++) {
            reg = reg_saved[i];
            如 (regs_allocated[reg]) {
                如 (reg >= 8)
                  g(0x41), reg-=8;
                g(0x50 + reg);
            }
        }

        /* generate load code */
        对于(i = 0; i < nb_operands; i++) {
            op = &operands[i];
            如 (op->reg >= 0) {
                如 ((op->vt->r & VT_VALMASK) == VT_LLOCAL &&
                    op->is_memory) {
                    /* memory reference case (for both input and
                       output cases) */
                    SValue sv;
                    sv = *op->vt;
                    sv.r = (sv.r & ~VT_VALMASK) | VT_LOCAL | VT_LVAL;
                    sv.type.t = VT_PTR;
                    load(op->reg, &sv);
                } 另 如 (i >= nb_outputs || op->is_rw) {
                    /* load value in register */
                    load(op->reg, op->vt);
                    如 (op->is_llong) {
                        SValue sv;
                        sv = *op->vt;
                        sv.c.i += 4;
                        load(TREG_XDX, &sv);
                    }
                }
            }
        }
    } 另 {
        /* generate save code */
        对于(i = 0 ; i < nb_outputs; i++) {
            op = &operands[i];
            如 (op->reg >= 0) {
                如 ((op->vt->r & VT_VALMASK) == VT_LLOCAL) {
                    如 (!op->is_memory) {
                        SValue sv;
                        sv = *op->vt;
                        sv.r = (sv.r & ~VT_VALMASK) | VT_LOCAL;
                        sv.type.t = VT_PTR;
                        load(out_reg, &sv);

                        sv = *op->vt;
                        sv.r = (sv.r & ~VT_VALMASK) | out_reg;
                        store(op->reg, &sv);
                    }
                } 另 {
                    store(op->reg, op->vt);
                    如 (op->is_llong) {
                        SValue sv;
                        sv = *op->vt;
                        sv.c.i += 4;
                        store(TREG_XDX, &sv);
                    }
                }
            }
        }
        /* generate reg restore code */
        对于(i = 求长度(reg_saved)/求长度(reg_saved[0]) - 1; i >= 0; i--) {
            reg = reg_saved[i];
            如 (regs_allocated[reg]) {
                如 (reg >= 8)
                  g(0x41), reg-=8;
                g(0x58 + reg);
            }
        }
    }
}

ST_FUNC 空 asm_clobber(uint8_t *clobber_regs, 不变 字 *str)
{
    整 reg;
    TokenSym *ts;
#如定义 TCC_TARGET_X86_64
    无符 整 type;
#了如

    如 (!strcmp(str, "memory") ||
        !strcmp(str, "cc") ||
        !strcmp(str, "flags"))
        返回;
    ts = tok_alloc(str, strlen(str));
    reg = ts->tok;
    如 (reg >= TOK_ASM_eax && reg <= TOK_ASM_edi) {
        reg -= TOK_ASM_eax;
    } 另 如 (reg >= TOK_ASM_ax && reg <= TOK_ASM_di) {
        reg -= TOK_ASM_ax;
#如定义 TCC_TARGET_X86_64
    } 另 如 (reg >= TOK_ASM_rax && reg <= TOK_ASM_rdi) {
        reg -= TOK_ASM_rax;
    } 另 如 ((reg = asm_parse_numeric_reg(reg, &type)) >= 0) {
        ;
#了如
    } 另 {
        tcc_error("invalid clobber register '%s'", str);
    }
    clobber_regs[reg] = 1;
}
