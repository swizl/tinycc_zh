/*
 *  ARMv4 code generator for TCC
 *
 *  Copyright (c) 2003 Daniel Gl?ckner
 *  Copyright (c) 2012 Thomas Preud'homme
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

#如 已定义(TCC_ARM_EABI) && !已定义(TCC_ARM_VFP)
#错误 "Currently TinyCC only supports float computation with VFP instructions"
#了如

/* number of available registers */
#如定义 TCC_ARM_VFP
#定义 NB_REGS            13
#另
#定义 NB_REGS             9
#了如

#如未定义 TCC_CPU_VERSION
# 定义 TCC_CPU_VERSION 5
#了如

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#定义 RC_INT     0x0001 /* generic integer register */
#定义 RC_FLOAT   0x0002 /* generic float register */
#定义 RC_R0      0x0004
#定义 RC_R1      0x0008
#定义 RC_R2      0x0010
#定义 RC_R3      0x0020
#定义 RC_R12     0x0040
#定义 RC_F0      0x0080
#定义 RC_F1      0x0100
#定义 RC_F2      0x0200
#定义 RC_F3      0x0400
#如定义 TCC_ARM_VFP
#定义 RC_F4      0x0800
#定义 RC_F5      0x1000
#定义 RC_F6      0x2000
#定义 RC_F7      0x4000
#了如
#定义 RC_IRET    RC_R0  /* function return: integer register */
#定义 RC_LRET    RC_R1  /* function return: second integer register */
#定义 RC_FRET    RC_F0  /* function return: float register */

/* pretty names for the registers */
枚举 {
    TREG_R0 = 0,
    TREG_R1,
    TREG_R2,
    TREG_R3,
    TREG_R12,
    TREG_F0,
    TREG_F1,
    TREG_F2,
    TREG_F3,
#如定义 TCC_ARM_VFP
    TREG_F4,
    TREG_F5,
    TREG_F6,
    TREG_F7,
#了如
    TREG_SP = 13,
    TREG_LR,
};

#如定义 TCC_ARM_VFP
#定义 T2CPR(t) (((t) & VT_BTYPE) != VT_FLOAT ? 0x100 : 0)
#了如

/* return registers for function */
#定义 REG_IRET TREG_R0 /* single word int return register */
#定义 REG_LRET TREG_R1 /* second word return register (for long long) */
#定义 REG_FRET TREG_F0 /* float return register */

#如定义 TCC_ARM_EABI
#定义 TOK___divdi3 TOK___aeabi_ldivmod
#定义 TOK___moddi3 TOK___aeabi_ldivmod
#定义 TOK___udivdi3 TOK___aeabi_uldivmod
#定义 TOK___umoddi3 TOK___aeabi_uldivmod
#了如

/* defined if function parameters must be evaluated in reverse order */
#定义 INVERT_FUNC_PARAMS

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #定义 FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#定义 PTR_SIZE 4

/* long double size and alignment, in bytes */
#如定义 TCC_ARM_VFP
#定义 LDOUBLE_SIZE  8
#了如

#如未定义 LDOUBLE_SIZE
#定义 LDOUBLE_SIZE  8
#了如

#如定义 TCC_ARM_EABI
#定义 LDOUBLE_ALIGN 8
#另
#定义 LDOUBLE_ALIGN 4
#了如

/* maximum alignment (for aligned attribute support) */
#定义 MAX_ALIGN     8

#定义 CHAR_IS_UNSIGNED

/******************************************************/
#另 /* ! TARGET_DEFS_ONLY */
/******************************************************/
#包含 "tcc.h"

枚举 float_abi float_abi;

ST_DATA 不变 整 reg_classes[NB_REGS] = {
    /* r0 */ RC_INT | RC_R0,
    /* r1 */ RC_INT | RC_R1,
    /* r2 */ RC_INT | RC_R2,
    /* r3 */ RC_INT | RC_R3,
    /* r12 */ RC_INT | RC_R12,
    /* f0 */ RC_FLOAT | RC_F0,
    /* f1 */ RC_FLOAT | RC_F1,
    /* f2 */ RC_FLOAT | RC_F2,
    /* f3 */ RC_FLOAT | RC_F3,
#如定义 TCC_ARM_VFP
 /* d4/s8 */ RC_FLOAT | RC_F4,
/* d5/s10 */ RC_FLOAT | RC_F5,
/* d6/s12 */ RC_FLOAT | RC_F6,
/* d7/s14 */ RC_FLOAT | RC_F7,
#了如
};

静态 整 func_sub_sp_offset, last_itod_magic;
静态 整 leaffunc;

#如 已定义(TCC_ARM_EABI) && 已定义(TCC_ARM_VFP)
静态 CType float_type, double_type, func_float_type, func_double_type;
ST_FUNC 空 arm_init(结构 TCCState *s)
{
    float_type.t = VT_FLOAT;
    double_type.t = VT_DOUBLE;
    func_float_type.t = VT_FUNC;
    func_float_type.ref = sym_push(SYM_FIELD, &float_type, FUNC_CDECL, FUNC_OLD);
    func_double_type.t = VT_FUNC;
    func_double_type.ref = sym_push(SYM_FIELD, &double_type, FUNC_CDECL, FUNC_OLD);

    float_abi = s->float_abi;
#如未定义 TCC_ARM_HARDFLOAT
    tcc_warning("soft float ABI currently not supported: default to softfp");
#了如
}
#另
#定义 func_float_type func_old_type
#定义 func_double_type func_old_type
#定义 func_ldouble_type func_old_type
ST_FUNC 空 arm_init(结构 TCCState *s)
{
#如 0
#如 !已定义 (TCC_ARM_VFP)
    tcc_warning("Support for FPA is deprecated and will be removed in next"
                " release");
#了如
#如 !已定义 (TCC_ARM_EABI)
    tcc_warning("Support for OABI is deprecated and will be removed in next"
                " release");
#了如
#了如
}
#了如

静态 整 two2mask(整 a,整 b) {
  返回 (reg_classes[a]|reg_classes[b])&~(RC_INT|RC_FLOAT);
}

静态 整 regmask(整 r) {
  返回 reg_classes[r]&~(RC_INT|RC_FLOAT);
}

/******************************************************/

#如 已定义(TCC_ARM_EABI) && !已定义(CONFIG_TCC_ELFINTERP)
不变 字 *default_elfinterp(结构 TCCState *s)
{
    如 (s->float_abi == ARM_HARD_FLOAT)
        返回 "/lib/ld-linux-armhf.so.3";
    另
        返回 "/lib/ld-linux.so.3";
}
#了如

空 o(uint32_t i)
{
  /* this is a good place to start adding big-endian support*/
  整 ind1;
  如 (nocode_wanted)
    返回;
  ind1 = ind + 4;
  如 (!cur_text_section)
    tcc_error("compiler error! This happens f.ex. if the compiler\n"
         "can't evaluate constant expressions outside of a function.");
  如 (ind1 > cur_text_section->data_allocated)
    section_realloc(cur_text_section, ind1);
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i;
}

静态 uint32_t stuff_const(uint32_t op, uint32_t c)
{
  整 try_neg=0;
  uint32_t nc = 0, negop = 0;

  转接(op&0x1F00000)
  {
    事例 0x800000: //add
    事例 0x400000: //sub
      try_neg=1;
      negop=op^0xC00000;
      nc=-c;
      跳出;
    事例 0x1A00000: //mov
    事例 0x1E00000: //mvn
      try_neg=1;
      negop=op^0x400000;
      nc=~c;
      跳出;
    事例 0x200000: //xor
      如(c==~0)
        返回 (op&0xF010F000)|((op>>16)&0xF)|0x1E00000;
      跳出;
    事例 0x0: //and
      如(c==~0)
        返回 (op&0xF010F000)|((op>>16)&0xF)|0x1A00000;
    事例 0x1C00000: //bic
      try_neg=1;
      negop=op^0x1C00000;
      nc=~c;
      跳出;
    事例 0x1800000: //orr
      如(c==~0)
        返回 (op&0xFFF0FFFF)|0x1E00000;
      跳出;
  }
  运行 {
    uint32_t m;
    整 i;
    如(c<256) /* catch undefined <<32 */
      返回 op|c;
    对于(i=2;i<32;i+=2) {
      m=(0xff>>i)|(0xff<<(32-i));
      如(!(c&~m))
        返回 op|(i<<7)|(c<<i)|(c>>(32-i));
    }
    op=negop;
    c=nc;
  } 当(try_neg--);
  返回 0;
}


//only add,sub
空 stuff_const_harder(uint32_t op, uint32_t v) {
  uint32_t x;
  x=stuff_const(op,v);
  如(x)
    o(x);
  另 {
    uint32_t a[16], nv, no, o2, n2;
    整 i,j,k;
    a[0]=0xff;
    o2=(op&0xfff0ffff)|((op&0xf000)<<4);;
    对于(i=1;i<16;i++)
      a[i]=(a[i-1]>>2)|(a[i-1]<<30);
    对于(i=0;i<12;i++)
      对于(j=i<4?i+12:15;j>=i+4;j--)
        如((v&(a[i]|a[j]))==v) {
          o(stuff_const(op,v&a[i]));
          o(stuff_const(o2,v&a[j]));
          返回;
        }
    no=op^0xC00000;
    n2=o2^0xC00000;
    nv=-v;
    对于(i=0;i<12;i++)
      对于(j=i<4?i+12:15;j>=i+4;j--)
        如((nv&(a[i]|a[j]))==nv) {
          o(stuff_const(no,nv&a[i]));
          o(stuff_const(n2,nv&a[j]));
          返回;
        }
    对于(i=0;i<8;i++)
      对于(j=i+4;j<12;j++)
        对于(k=i<4?i+12:15;k>=j+4;k--)
          如((v&(a[i]|a[j]|a[k]))==v) {
            o(stuff_const(op,v&a[i]));
            o(stuff_const(o2,v&a[j]));
            o(stuff_const(o2,v&a[k]));
            返回;
          }
    no=op^0xC00000;
    nv=-v;
    对于(i=0;i<8;i++)
      对于(j=i+4;j<12;j++)
        对于(k=i<4?i+12:15;k>=j+4;k--)
          如((nv&(a[i]|a[j]|a[k]))==nv) {
            o(stuff_const(no,nv&a[i]));
            o(stuff_const(n2,nv&a[j]));
            o(stuff_const(n2,nv&a[k]));
            返回;
          }
    o(stuff_const(op,v&a[0]));
    o(stuff_const(o2,v&a[4]));
    o(stuff_const(o2,v&a[8]));
    o(stuff_const(o2,v&a[12]));
  }
}

uint32_t encbranch(整 pos, 整 addr, 整 fail)
{
  addr-=pos+8;
  addr/=4;
  如(addr>=0x1000000 || addr<-0x1000000) {
    如(fail)
      tcc_error("FIXME: function bigger than 32MB");
    返回 0;
  }
  返回 0x0A000000|(addr&0xffffff);
}

整 decbranch(整 pos)
{
  整 x;
  x=*(uint32_t *)(cur_text_section->data + pos);
  x&=0x00ffffff;
  如(x&0x800000)
    x-=0x1000000;
  返回 x*4+pos+8;
}

/* output a symbol and patch all calls to it */
空 gsym_addr(整 t, 整 a)
{
  uint32_t *x;
  整 lt;
  当(t) {
    x=(uint32_t *)(cur_text_section->data + t);
    t=decbranch(lt=t);
    如(a==lt+4)
      *x=0xE1A00000; // nop
    另 {
      *x &= 0xff000000;
      *x |= encbranch(lt,a,1);
    }
  }
}

空 gsym(整 t)
{
  gsym_addr(t, ind);
}

#如定义 TCC_ARM_VFP
静态 uint32_t vfpr(整 r)
{
  如(r<TREG_F0 || r>TREG_F7)
    tcc_error("compiler error! register %i is no vfp register",r);
  返回 r - TREG_F0;
}
#另
静态 uint32_t fpr(整 r)
{
  如(r<TREG_F0 || r>TREG_F3)
    tcc_error("compiler error! register %i is no fpa register",r);
  返回 r - TREG_F0;
}
#了如

静态 uint32_t intr(整 r)
{
  如(r == TREG_R12)
    返回 12;
  如(r >= TREG_R0 && r <= TREG_R3)
    返回 r - TREG_R0;
  如 (r >= TREG_SP && r <= TREG_LR)
    返回 r + (13 - TREG_SP);
  tcc_error("compiler error! register %i is no int register",r);
}

静态 空 calcaddr(uint32_t *base, 整 *off, 整 *sgn, 整 maxoff, 无符 shift)
{
  如(*off>maxoff || *off&((1<<shift)-1)) {
    uint32_t x, y;
    x=0xE280E000;
    如(*sgn)
      x=0xE240E000;
    x|=(*base)<<16;
    *base=14; // lr
    y=stuff_const(x,*off&~maxoff);
    如(y) {
      o(y);
      *off&=maxoff;
      返回;
    }
    y=stuff_const(x,(*off+maxoff)&~maxoff);
    如(y) {
      o(y);
      *sgn=!*sgn;
      *off=((*off+maxoff)&~maxoff)-*off;
      返回;
    }
    stuff_const_harder(x,*off&~maxoff);
    *off&=maxoff;
  }
}

静态 uint32_t mapcc(整 cc)
{
  转接(cc)
  {
    事例 TOK_ULT:
      返回 0x30000000; /* CC/LO */
    事例 TOK_UGE:
      返回 0x20000000; /* CS/HS */
    事例 TOK_EQ:
      返回 0x00000000; /* EQ */
    事例 TOK_NE:
      返回 0x10000000; /* NE */
    事例 TOK_ULE:
      返回 0x90000000; /* LS */
    事例 TOK_UGT:
      返回 0x80000000; /* HI */
    事例 TOK_Nset:
      返回 0x40000000; /* MI */
    事例 TOK_Nclear:
      返回 0x50000000; /* PL */
    事例 TOK_LT:
      返回 0xB0000000; /* LT */
    事例 TOK_GE:
      返回 0xA0000000; /* GE */
    事例 TOK_LE:
      返回 0xD0000000; /* LE */
    事例 TOK_GT:
      返回 0xC0000000; /* GT */
  }
  tcc_error("unexpected condition code");
  返回 0xE0000000; /* AL */
}

静态 整 negcc(整 cc)
{
  转接(cc)
  {
    事例 TOK_ULT:
      返回 TOK_UGE;
    事例 TOK_UGE:
      返回 TOK_ULT;
    事例 TOK_EQ:
      返回 TOK_NE;
    事例 TOK_NE:
      返回 TOK_EQ;
    事例 TOK_ULE:
      返回 TOK_UGT;
    事例 TOK_UGT:
      返回 TOK_ULE;
    事例 TOK_Nset:
      返回 TOK_Nclear;
    事例 TOK_Nclear:
      返回 TOK_Nset;
    事例 TOK_LT:
      返回 TOK_GE;
    事例 TOK_GE:
      返回 TOK_LT;
    事例 TOK_LE:
      返回 TOK_GT;
    事例 TOK_GT:
      返回 TOK_LE;
  }
  tcc_error("unexpected condition code");
  返回 TOK_NE;
}

/* load 'r' from value 'sv' */
空 load(整 r, SValue *sv)
{
  整 v, ft, fc, fr, sign;
  uint32_t op;
  SValue v1;

  fr = sv->r;
  ft = sv->type.t;
  fc = sv->c.i;

  如(fc>=0)
    sign=0;
  另 {
    sign=1;
    fc=-fc;
  }

  v = fr & VT_VALMASK;
  如 (fr & VT_LVAL) {
    uint32_t base = 0xB; // fp
    如(v == VT_LLOCAL) {
      v1.type.t = VT_PTR;
      v1.r = VT_LOCAL | VT_LVAL;
      v1.c.i = sv->c.i;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    } 另 如(v == VT_CONST) {
      v1.type.t = VT_PTR;
      v1.r = fr&~VT_LVAL;
      v1.c.i = sv->c.i;
      v1.sym=sv->sym;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    } 另 如(v < VT_CONST) {
      base=intr(v);
      fc=sign=0;
      v=VT_LOCAL;
    }
    如(v == VT_LOCAL) {
      如(is_float(ft)) {
        calcaddr(&base,&fc,&sign,1020,2);
#如定义 TCC_ARM_VFP
        op=0xED100A00; /* flds */
        如(!sign)
          op|=0x800000;
        如 ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x100;   /* flds -> fldd */
        o(op|(vfpr(r)<<12)|(fc>>2)|(base<<16));
#另
        op=0xED100100;
        如(!sign)
          op|=0x800000;
#如 LDOUBLE_SIZE == 8
        如 ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x8000;
#另
        如 ((ft & VT_BTYPE) == VT_DOUBLE)
          op|=0x8000;
        另 如 ((ft & VT_BTYPE) == VT_LDOUBLE)
          op|=0x400000;
#了如
        o(op|(fpr(r)<<12)|(fc>>2)|(base<<16));
#了如
      } 另 如((ft & (VT_BTYPE|VT_UNSIGNED)) == VT_BYTE
                || (ft & VT_BTYPE) == VT_SHORT) {
        calcaddr(&base,&fc,&sign,255,0);
        op=0xE1500090;
        如 ((ft & VT_BTYPE) == VT_SHORT)
          op|=0x20;
        如 ((ft & VT_UNSIGNED) == 0)
          op|=0x40;
        如(!sign)
          op|=0x800000;
        o(op|(intr(r)<<12)|(base<<16)|((fc&0xf0)<<4)|(fc&0xf));
      } 另 {
        calcaddr(&base,&fc,&sign,4095,0);
        op=0xE5100000;
        如(!sign)
          op|=0x800000;
        如 ((ft & VT_BTYPE) == VT_BYTE || (ft & VT_BTYPE) == VT_BOOL)
          op|=0x400000;
        o(op|(intr(r)<<12)|fc|(base<<16));
      }
      返回;
    }
  } 另 {
    如 (v == VT_CONST) {
      op=stuff_const(0xE3A00000|(intr(r)<<12),sv->c.i);
      如 (fr & VT_SYM || !op) {
        o(0xE59F0000|(intr(r)<<12));
        o(0xEA000000);
        如(fr & VT_SYM)
          greloc(cur_text_section, sv->sym, ind, R_ARM_ABS32);
        o(sv->c.i);
      } 另
        o(op);
      返回;
    } 另 如 (v == VT_LOCAL) {
      op=stuff_const(0xE28B0000|(intr(r)<<12),sv->c.i);
      如 (fr & VT_SYM || !op) {
        o(0xE59F0000|(intr(r)<<12));
        o(0xEA000000);
        如(fr & VT_SYM) // needed ?
          greloc(cur_text_section, sv->sym, ind, R_ARM_ABS32);
        o(sv->c.i);
        o(0xE08B0000|(intr(r)<<12)|intr(r));
      } 另
        o(op);
      返回;
    } 另 如(v == VT_CMP) {
      o(mapcc(sv->c.i)|0x3A00001|(intr(r)<<12));
      o(mapcc(negcc(sv->c.i))|0x3A00000|(intr(r)<<12));
      返回;
    } 另 如 (v == VT_JMP || v == VT_JMPI) {
      整 t;
      t = v & 1;
      o(0xE3A00000|(intr(r)<<12)|t);
      o(0xEA000000);
      gsym(sv->c.i);
      o(0xE3A00000|(intr(r)<<12)|(t^1));
      返回;
    } 另 如 (v < VT_CONST) {
      如(is_float(ft))
#如定义 TCC_ARM_VFP
        o(0xEEB00A40|(vfpr(r)<<12)|vfpr(v)|T2CPR(ft)); /* fcpyX */
#另
        o(0xEE008180|(fpr(r)<<12)|fpr(v));
#了如
      另
        o(0xE1A00000|(intr(r)<<12)|intr(v));
      返回;
    }
  }
  tcc_error("load unimplemented!");
}

/* store register 'r' in lvalue 'v' */
空 store(整 r, SValue *sv)
{
  SValue v1;
  整 v, ft, fc, fr, sign;
  uint32_t op;

  fr = sv->r;
  ft = sv->type.t;
  fc = sv->c.i;

  如(fc>=0)
    sign=0;
  另 {
    sign=1;
    fc=-fc;
  }

  v = fr & VT_VALMASK;
  如 (fr & VT_LVAL || fr == VT_LOCAL) {
    uint32_t base = 0xb; /* fp */
    如(v < VT_CONST) {
      base=intr(v);
      v=VT_LOCAL;
      fc=sign=0;
    } 另 如(v == VT_CONST) {
      v1.type.t = ft;
      v1.r = fr&~VT_LVAL;
      v1.c.i = sv->c.i;
      v1.sym=sv->sym;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    }
    如(v == VT_LOCAL) {
       如(is_float(ft)) {
        calcaddr(&base,&fc,&sign,1020,2);
#如定义 TCC_ARM_VFP
        op=0xED000A00; /* fsts */
        如(!sign)
          op|=0x800000;
        如 ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x100;   /* fsts -> fstd */
        o(op|(vfpr(r)<<12)|(fc>>2)|(base<<16));
#另
        op=0xED000100;
        如(!sign)
          op|=0x800000;
#如 LDOUBLE_SIZE == 8
        如 ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x8000;
#另
        如 ((ft & VT_BTYPE) == VT_DOUBLE)
          op|=0x8000;
        如 ((ft & VT_BTYPE) == VT_LDOUBLE)
          op|=0x400000;
#了如
        o(op|(fpr(r)<<12)|(fc>>2)|(base<<16));
#了如
        返回;
      } 另 如((ft & VT_BTYPE) == VT_SHORT) {
        calcaddr(&base,&fc,&sign,255,0);
        op=0xE14000B0;
        如(!sign)
          op|=0x800000;
        o(op|(intr(r)<<12)|(base<<16)|((fc&0xf0)<<4)|(fc&0xf));
      } 另 {
        calcaddr(&base,&fc,&sign,4095,0);
        op=0xE5000000;
        如(!sign)
          op|=0x800000;
        如 ((ft & VT_BTYPE) == VT_BYTE || (ft & VT_BTYPE) == VT_BOOL)
          op|=0x400000;
        o(op|(intr(r)<<12)|fc|(base<<16));
      }
      返回;
    }
  }
  tcc_error("store unimplemented");
}

静态 空 gadd_sp(整 val)
{
  stuff_const_harder(0xE28DD000,val);
}

/* 'is_jmp' is '1' if it is a jump */
静态 空 gcall_or_jmp(整 is_jmp)
{
  整 r;
  如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
    uint32_t x;
    /* constant case */
    x=encbranch(ind,ind+vtop->c.i,0);
    如(x) {
      如 (vtop->r & VT_SYM) {
        /* relocation case */
        greloc(cur_text_section, vtop->sym, ind, R_ARM_PC24);
      } 另
        put_elf_reloc(symtab_section, cur_text_section, ind, R_ARM_PC24, 0);
      o(x|(is_jmp?0xE0000000:0xE1000000));
    } 另 {
      如(!is_jmp)
        o(0xE28FE004); // add lr,pc,#4
      o(0xE51FF004);   // ldr pc,[pc,#-4]
      如 (vtop->r & VT_SYM)
        greloc(cur_text_section, vtop->sym, ind, R_ARM_ABS32);
      o(vtop->c.i);
    }
  } 另 {
    /* otherwise, indirect call */
    r = gv(RC_INT);
    如(!is_jmp)
      o(0xE1A0E00F);       // mov lr,pc
    o(0xE1A0F000|intr(r)); // mov pc,r
  }
}

静态 整 unalias_ldbl(整 btype)
{
#如 LDOUBLE_SIZE == 8
    如 (btype == VT_LDOUBLE)
      btype = VT_DOUBLE;
#了如
    返回 btype;
}

/* Return whether a structure is an homogeneous float aggregate or not.
   The answer is true if all the elements of the structure are of the same
   primitive float type and there is less than 4 elements.

   type: the type corresponding to the structure to be tested */
静态 整 is_hgen_float_aggr(CType *type)
{
  如 ((type->t & VT_BTYPE) == VT_STRUCT) {
    结构 Sym *ref;
    整 btype, nb_fields = 0;

    ref = type->ref->next;
    btype = unalias_ldbl(ref->type.t & VT_BTYPE);
    如 (btype == VT_FLOAT || btype == VT_DOUBLE) {
      对于(; ref && btype == unalias_ldbl(ref->type.t & VT_BTYPE); ref = ref->next, nb_fields++);
      返回 !ref && nb_fields <= 4;
    }
  }
  返回 0;
}

结构 avail_regs {
  有符 字 avail[3]; /* 3 holes max with only float and double alignments */
  整 first_hole; /* first available hole */
  整 last_hole; /* last available hole (none if equal to first_hole) */
  整 first_free_reg; /* next free register in the sequence, hole excluded */
};

#定义 AVAIL_REGS_INITIALIZER (结构 avail_regs) { { 0, 0, 0}, 0, 0, 0 }

/* Find suitable registers for a VFP Co-Processor Register Candidate (VFP CPRC
   param) according to the rules described in the procedure call standard for
   the ARM architecture (AAPCS). If found, the registers are assigned to this
   VFP CPRC parameter. Registers are allocated in sequence unless a hole exists
   and the parameter is a single float.

   avregs: opaque structure to keep track of available VFP co-processor regs
   align: alignment constraints for the param, as returned by type_size()
   size: size of the parameter, as returned by type_size() */
整 assign_vfpreg(结构 avail_regs *avregs, 整 align, 整 size)
{
  整 first_reg = 0;

  如 (avregs->first_free_reg == -1)
    返回 -1;
  如 (align >> 3) { /* double alignment */
    first_reg = avregs->first_free_reg;
    /* alignment constraint not respected so use next reg and record hole */
    如 (first_reg & 1)
      avregs->avail[avregs->last_hole++] = first_reg++;
  } 另 { /* no special alignment (float or array of float) */
    /* if single float and a hole is available, assign the param to it */
    如 (size == 4 && avregs->first_hole != avregs->last_hole)
      返回 avregs->avail[avregs->first_hole++];
    另
      first_reg = avregs->first_free_reg;
  }
  如 (first_reg + size / 4 <= 16) {
    avregs->first_free_reg = first_reg + size / 4;
    返回 first_reg;
  }
  avregs->first_free_reg = -1;
  返回 -1;
}

/* Returns whether all params need to be passed in core registers or not.
   This is the case for function part of the runtime ABI. */
整 floats_in_core_regs(SValue *sval)
{
  如 (!sval->sym)
    返回 0;

  转接 (sval->sym->v) {
    事例 TOK___floatundisf:
    事例 TOK___floatundidf:
    事例 TOK___fixunssfdi:
    事例 TOK___fixunsdfdi:
#如未定义 TCC_ARM_VFP
    事例 TOK___fixunsxfdi:
#了如
    事例 TOK___floatdisf:
    事例 TOK___floatdidf:
    事例 TOK___fixsfdi:
    事例 TOK___fixdfdi:
      返回 1;

    缺省:
      返回 0;
  }
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret, 整 *ret_align, 整 *regsize) {
#如定义 TCC_ARM_EABI
    整 size, align;
    size = type_size(vt, &align);
    如 (float_abi == ARM_HARD_FLOAT && !variadic &&
        (is_float(vt->t) || is_hgen_float_aggr(vt))) {
        *ret_align = 8;
        *regsize = 8;
        ret->ref = NULL;
        ret->t = VT_DOUBLE;
        返回 (size + 7) >> 3;
    } 另 如 (size <= 4) {
        *ret_align = 4;
        *regsize = 4;
        ret->ref = NULL;
        ret->t = VT_INT;
        返回 1;
    } 另
        返回 0;
#另
    返回 0;
#了如
}

/* Parameters are classified according to how they are copied to their final
   destination for the function call. Because the copying is performed class
   after class according to the order in the union below, it is important that
   some constraints about the order of the members of this union are respected:
   - CORE_STRUCT_CLASS must come after STACK_CLASS;
   - CORE_CLASS must come after STACK_CLASS, CORE_STRUCT_CLASS and
     VFP_STRUCT_CLASS;
   - VFP_STRUCT_CLASS must come after VFP_CLASS.
   See the comment for the main loop in copy_params() for the reason. */
枚举 reg_class {
        STACK_CLASS = 0,
        CORE_STRUCT_CLASS,
        VFP_CLASS,
        VFP_STRUCT_CLASS,
        CORE_CLASS,
        NB_CLASSES
};

结构 param_plan {
    整 start; /* first reg or addr used depending on the class */
    整 end; /* last reg used or next free addr depending on the class */
    SValue *sval; /* pointer to SValue on the value stack */
    结构 param_plan *prev; /*  previous element in this class */
};

结构 plan {
    结构 param_plan *pplans; /* array of all the param plans */
    结构 param_plan *clsplans[NB_CLASSES]; /* per class lists of param plans */
};

#定义 add_param_plan(plan,pplan,class)                        \
    运行 {                                                        \
        pplan.prev = plan->clsplans[class];                     \
        plan->pplans[plan ## _nb] = pplan;                      \
        plan->clsplans[class] = &plan->pplans[plan ## _nb++];   \
    } 当(0)

/* Assign parameters to registers and stack with alignment according to the
   rules in the procedure call standard for the ARM architecture (AAPCS).
   The overall assignment is recorded in an array of per parameter structures
   called parameter plans. The parameter plans are also further organized in a
   number of linked lists, one per class of parameter (see the comment for the
   definition of union reg_class).

   nb_args: number of parameters of the function for which a call is generated
   float_abi: float ABI in use for this function call
   plan: the structure where the overall assignment is recorded
   todo: a bitmap that record which core registers hold a parameter

   Returns the amount of stack space needed for parameter passing

   Note: this function allocated an array in plan->pplans with tcc_malloc. It
   is the responsibility of the caller to free this array once used (ie not
   before copy_params). */
静态 整 assign_regs(整 nb_args, 整 float_abi, 结构 plan *plan, 整 *todo)
{
  整 i, size, align;
  整 ncrn /* next core register number */, nsaa /* next stacked argument address*/;
  整 plan_nb = 0;
  结构 param_plan pplan;
  结构 avail_regs avregs = AVAIL_REGS_INITIALIZER;

  ncrn = nsaa = 0;
  *todo = 0;
  plan->pplans = tcc_malloc(nb_args * 求长度(*plan->pplans));
  memset(plan->clsplans, 0, 求长度(plan->clsplans));
  对于(i = nb_args; i-- ;) {
    整 j, start_vfpreg = 0;
    CType type = vtop[-i].type;
    type.t &= ~VT_ARRAY;
    size = type_size(&type, &align);
    size = (size + 3) & ~3;
    align = (align + 3) & ~3;
    转接(vtop[-i].type.t & VT_BTYPE) {
      事例 VT_STRUCT:
      事例 VT_FLOAT:
      事例 VT_DOUBLE:
      事例 VT_LDOUBLE:
      如 (float_abi == ARM_HARD_FLOAT) {
        整 is_hfa = 0; /* Homogeneous float aggregate */

        如 (is_float(vtop[-i].type.t)
            || (is_hfa = is_hgen_float_aggr(&vtop[-i].type))) {
          整 end_vfpreg;

          start_vfpreg = assign_vfpreg(&avregs, align, size);
          end_vfpreg = start_vfpreg + ((size - 1) >> 2);
          如 (start_vfpreg >= 0) {
            pplan = (结构 param_plan) {start_vfpreg, end_vfpreg, &vtop[-i]};
            如 (is_hfa)
              add_param_plan(plan, pplan, VFP_STRUCT_CLASS);
            另
              add_param_plan(plan, pplan, VFP_CLASS);
            继续;
          } 另
            跳出;
        }
      }
      ncrn = (ncrn + (align-1)/4) & ~((align/4) - 1);
      如 (ncrn + size/4 <= 4 || (ncrn < 4 && start_vfpreg != -1)) {
        /* The parameter is allocated both in core register and on stack. As
         * such, it can be of either class: it would either be the last of
         * CORE_STRUCT_CLASS or the first of STACK_CLASS. */
        对于 (j = ncrn; j < 4 && j < ncrn + size / 4; j++)
          *todo|=(1<<j);
        pplan = (结构 param_plan) {ncrn, j, &vtop[-i]};
        add_param_plan(plan, pplan, CORE_STRUCT_CLASS);
        ncrn += size/4;
        如 (ncrn > 4)
          nsaa = (ncrn - 4) * 4;
      } 另 {
        ncrn = 4;
        跳出;
      }
      继续;
      缺省:
      如 (ncrn < 4) {
        整 is_long = (vtop[-i].type.t & VT_BTYPE) == VT_LLONG;

        如 (is_long) {
          ncrn = (ncrn + 1) & -2;
          如 (ncrn == 4)
            跳出;
        }
        pplan = (结构 param_plan) {ncrn, ncrn, &vtop[-i]};
        ncrn++;
        如 (is_long)
          pplan.end = ncrn++;
        add_param_plan(plan, pplan, CORE_CLASS);
        继续;
      }
    }
    nsaa = (nsaa + (align - 1)) & ~(align - 1);
    pplan = (结构 param_plan) {nsaa, nsaa + size, &vtop[-i]};
    add_param_plan(plan, pplan, STACK_CLASS);
    nsaa += size; /* size already rounded up before */
  }
  返回 nsaa;
}

#消定义 add_param_plan

/* Copy parameters to their final destination (core reg, VFP reg or stack) for
   function call.

   nb_args: number of parameters the function take
   plan: the overall assignment plan for parameters
   todo: a bitmap indicating what core reg will hold a parameter

   Returns the number of SValue added by this function on the value stack */
静态 整 copy_params(整 nb_args, 结构 plan *plan, 整 todo)
{
  整 size, align, r, i, nb_extra_sval = 0;
  结构 param_plan *pplan;
  整 pass = 0;

   /* Several constraints require parameters to be copied in a specific order:
      - structures are copied to the stack before being loaded in a reg;
      - floats loaded to an odd numbered VFP reg are first copied to the
        preceding even numbered VFP reg and then moved to the next VFP reg.

      It is thus important that:
      - structures assigned to core regs must be copied after parameters
        assigned to the stack but before structures assigned to VFP regs because
        a structure can lie partly in core registers and partly on the stack;
      - parameters assigned to the stack and all structures be copied before
        parameters assigned to a core reg since copying a parameter to the stack
        require using a core reg;
      - parameters assigned to VFP regs be copied before structures assigned to
        VFP regs as the copy might use an even numbered VFP reg that already
        holds part of a structure. */
again:
  对于(i = 0; i < NB_CLASSES; i++) {
    对于(pplan = plan->clsplans[i]; pplan; pplan = pplan->prev) {

      如 (pass
          && (i != CORE_CLASS || pplan->sval->r < VT_CONST))
        继续;

      vpushv(pplan->sval);
      pplan->sval->r = pplan->sval->r2 = VT_CONST; /* disable entry */
      转接(i) {
        事例 STACK_CLASS:
        事例 CORE_STRUCT_CLASS:
        事例 VFP_STRUCT_CLASS:
          如 ((pplan->sval->type.t & VT_BTYPE) == VT_STRUCT) {
            整 padding = 0;
            size = type_size(&pplan->sval->type, &align);
            /* align to stack align size */
            size = (size + 3) & ~3;
            如 (i == STACK_CLASS && pplan->prev)
              padding = pplan->start - pplan->prev->end;
            size += padding; /* Add padding if any */
            /* allocate the necessary size on stack */
            gadd_sp(-size);
            /* generate structure store */
            r = get_reg(RC_INT);
            o(0xE28D0000|(intr(r)<<12)|padding); /* add r, sp, padding */
            vset(&vtop->type, r | VT_LVAL, 0);
            vswap();
            vstore(); /* memcpy to current sp + potential padding */

            /* Homogeneous float aggregate are loaded to VFP registers
               immediately since there is no way of loading data in multiple
               non consecutive VFP registers as what is done for other
               structures (see the use of todo). */
            如 (i == VFP_STRUCT_CLASS) {
              整 first = pplan->start, nb = pplan->end - first + 1;
              /* vpop.32 {pplan->start, ..., pplan->end} */
              o(0xECBD0A00|(first&1)<<22|(first>>1)<<12|nb);
              /* No need to write the register used to a SValue since VFP regs
                 cannot be used for gcall_or_jmp */
            }
          } 另 {
            如 (is_float(pplan->sval->type.t)) {
#如定义 TCC_ARM_VFP
              r = vfpr(gv(RC_FLOAT)) << 12;
              如 ((pplan->sval->type.t & VT_BTYPE) == VT_FLOAT)
                size = 4;
              另 {
                size = 8;
                r |= 0x101; /* vpush.32 -> vpush.64 */
              }
              o(0xED2D0A01 + r); /* vpush */
#另
              r = fpr(gv(RC_FLOAT)) << 12;
              如 ((pplan->sval->type.t & VT_BTYPE) == VT_FLOAT)
                size = 4;
              另 如 ((pplan->sval->type.t & VT_BTYPE) == VT_DOUBLE)
                size = 8;
              另
                size = LDOUBLE_SIZE;

              如 (size == 12)
                r |= 0x400000;
              另 如(size == 8)
                r|=0x8000;

              o(0xED2D0100|r|(size>>2)); /* some kind of vpush for FPA */
#了如
            } 另 {
              /* simple type (currently always same size) */
              /* XXX: implicit cast ? */
              size=4;
              如 ((pplan->sval->type.t & VT_BTYPE) == VT_LLONG) {
                lexpand_nr();
                size = 8;
                r = gv(RC_INT);
                o(0xE52D0004|(intr(r)<<12)); /* push r */
                vtop--;
              }
              r = gv(RC_INT);
              o(0xE52D0004|(intr(r)<<12)); /* push r */
            }
            如 (i == STACK_CLASS && pplan->prev)
              gadd_sp(pplan->prev->end - pplan->start); /* Add padding if any */
          }
          跳出;

        事例 VFP_CLASS:
          gv(regmask(TREG_F0 + (pplan->start >> 1)));
          如 (pplan->start & 1) { /* Must be in upper part of double register */
            o(0xEEF00A40|((pplan->start>>1)<<12)|(pplan->start>>1)); /* vmov.f32 s(n+1), sn */
            vtop->r = VT_CONST; /* avoid being saved on stack by gv for next float */
          }
          跳出;

        事例 CORE_CLASS:
          如 ((pplan->sval->type.t & VT_BTYPE) == VT_LLONG) {
            lexpand_nr();
            gv(regmask(pplan->end));
            pplan->sval->r2 = vtop->r;
            vtop--;
          }
          gv(regmask(pplan->start));
          /* Mark register as used so that gcall_or_jmp use another one
             (regs >=4 are free as never used to pass parameters) */
          pplan->sval->r = vtop->r;
          跳出;
      }
      vtop--;
    }
  }

  /* second pass to restore registers that were saved on stack by accident.
     Maybe redundant after the "lvalue_save" patch in tccgen.c:gv() */
  如 (++pass < 2)
    跳转 again;

  /* Manually free remaining registers since next parameters are loaded
   * manually, without the help of gv(int). */
  save_regs(nb_args);

  如(todo) {
    o(0xE8BD0000|todo); /* pop {todo} */
    对于(pplan = plan->clsplans[CORE_STRUCT_CLASS]; pplan; pplan = pplan->prev) {
      整 r;
      pplan->sval->r = pplan->start;
      /* An SValue can only pin 2 registers at best (r and r2) but a structure
         can occupy more than 2 registers. Thus, we need to push on the value
         stack some fake parameter to have on SValue for each registers used
         by a structure (r2 is not used). */
      对于 (r = pplan->start + 1; r <= pplan->end; r++) {
        如 (todo & (1 << r)) {
          nb_extra_sval++;
          vpushi(0);
          vtop->r = r;
        }
      }
    }
  }
  返回 nb_extra_sval;
}

/* Generate function call. The function address is pushed first, then
   all the parameters in call order. This functions pops all the
   parameters and the function address. */
空 gfunc_call(整 nb_args)
{
  整 r, args_size;
  整 def_float_abi = float_abi;
  整 todo;
  结构 plan plan;

#如定义 TCC_ARM_EABI
  整 variadic;

  如 (float_abi == ARM_HARD_FLOAT) {
    variadic = (vtop[-nb_args].type.ref->f.func_type == FUNC_ELLIPSIS);
    如 (variadic || floats_in_core_regs(&vtop[-nb_args]))
      float_abi = ARM_SOFTFP_FLOAT;
  }
#了如
  /* cannot let cpu flags if other instruction are generated. Also avoid leaving
     VT_JMP anywhere except on the top of the stack because it would complicate
     the code generator. */
  r = vtop->r & VT_VALMASK;
  如 (r == VT_CMP || (r & ~1) == VT_JMP)
    gv(RC_INT);

  args_size = assign_regs(nb_args, float_abi, &plan, &todo);

#如定义 TCC_ARM_EABI
  如 (args_size & 7) { /* Stack must be 8 byte aligned at fct call for EABI */
    args_size = (args_size + 7) & ~7;
    o(0xE24DD004); /* sub sp, sp, #4 */
  }
#了如

  nb_args += copy_params(nb_args, &plan, todo);
  tcc_free(plan.pplans);

  /* Move fct SValue on top as required by gcall_or_jmp */
  vrotb(nb_args + 1);
  gcall_or_jmp(0);
  如 (args_size)
      gadd_sp(args_size); /* pop all parameters passed on the stack */
#如 已定义(TCC_ARM_EABI) && 已定义(TCC_ARM_VFP)
  如(float_abi == ARM_SOFTFP_FLOAT && is_float(vtop->type.ref->type.t)) {
    如((vtop->type.ref->type.t & VT_BTYPE) == VT_FLOAT) {
      o(0xEE000A10); /*vmov s0, r0 */
    } 另 {
      o(0xEE000B10); /* vmov.32 d0[0], r0 */
      o(0xEE201B10); /* vmov.32 d0[1], r1 */
    }
  }
#了如
  vtop -= nb_args + 1; /* Pop all params and fct address from value stack */
  leaffunc = 0; /* we are calling a function, so we aren't in a leaf function */
  float_abi = def_float_abi;
}

/* generate function prolog of type 't' */
空 gfunc_prolog(CType *func_type)
{
  Sym *sym,*sym2;
  整 n, nf, size, align, rs, struct_ret = 0;
  整 addr, pn, sn; /* pn=core, sn=stack */
  CType ret_type;

#如定义 TCC_ARM_EABI
  结构 avail_regs avregs = AVAIL_REGS_INITIALIZER;
#了如

  sym = func_type->ref;
  func_vt = sym->type;
  func_var = (func_type->ref->f.func_type == FUNC_ELLIPSIS);

  n = nf = 0;
  如 ((func_vt.t & VT_BTYPE) == VT_STRUCT &&
      !gfunc_sret(&func_vt, func_var, &ret_type, &align, &rs))
  {
    n++;
    struct_ret = 1;
    func_vc = 12; /* Offset from fp of the place to store the result */
  }
  对于(sym2 = sym->next; sym2 && (n < 4 || nf < 16); sym2 = sym2->next) {
    size = type_size(&sym2->type, &align);
#如定义 TCC_ARM_EABI
    如 (float_abi == ARM_HARD_FLOAT && !func_var &&
        (is_float(sym2->type.t) || is_hgen_float_aggr(&sym2->type))) {
      整 tmpnf = assign_vfpreg(&avregs, align, size);
      tmpnf += (size + 3) / 4;
      nf = (tmpnf > nf) ? tmpnf : nf;
    } 另
#了如
    如 (n < 4)
      n += (size + 3) / 4;
  }
  o(0xE1A0C00D); /* mov ip,sp */
  如 (func_var)
    n=4;
  如 (n) {
    如(n>4)
      n=4;
#如定义 TCC_ARM_EABI
    n=(n+1)&-2;
#了如
    o(0xE92D0000|((1<<n)-1)); /* save r0-r4 on stack if needed */
  }
  如 (nf) {
    如 (nf>16)
      nf=16;
    nf=(nf+1)&-2; /* nf => HARDFLOAT => EABI */
    o(0xED2D0A00|nf); /* save s0-s15 on stack if needed */
  }
  o(0xE92D5800); /* save fp, ip, lr */
  o(0xE1A0B00D); /* mov fp, sp */
  func_sub_sp_offset = ind;
  o(0xE1A00000); /* nop, leave space for stack adjustment in epilog */

#如定义 TCC_ARM_EABI
  如 (float_abi == ARM_HARD_FLOAT) {
    func_vc += nf * 4;
    avregs = AVAIL_REGS_INITIALIZER;
  }
#了如
  pn = struct_ret, sn = 0;
  当 ((sym = sym->next)) {
    CType *type;
    type = &sym->type;
    size = type_size(type, &align);
    size = (size + 3) >> 2;
    align = (align + 3) & ~3;
#如定义 TCC_ARM_EABI
    如 (float_abi == ARM_HARD_FLOAT && !func_var && (is_float(sym->type.t)
        || is_hgen_float_aggr(&sym->type))) {
      整 fpn = assign_vfpreg(&avregs, align, size << 2);
      如 (fpn >= 0)
        addr = fpn * 4;
      另
        跳转 from_stack;
    } 另
#了如
    如 (pn < 4) {
#如定义 TCC_ARM_EABI
        pn = (pn + (align-1)/4) & -(align/4);
#了如
      addr = (nf + pn) * 4;
      pn += size;
      如 (!sn && pn > 4)
        sn = (pn - 4);
    } 另 {
#如定义 TCC_ARM_EABI
from_stack:
        sn = (sn + (align-1)/4) & -(align/4);
#了如
      addr = (n + nf + sn) * 4;
      sn += size;
    }
    sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t),
             addr + 12);
  }
  last_itod_magic=0;
  leaffunc = 1;
  loc = 0;
}

/* generate function epilog */
空 gfunc_epilog(空)
{
  uint32_t x;
  整 diff;
  /* Copy float return value to core register if base standard is used and
     float computation is made with VFP */
#如 已定义(TCC_ARM_EABI) && 已定义(TCC_ARM_VFP)
  如 ((float_abi == ARM_SOFTFP_FLOAT || func_var) && is_float(func_vt.t)) {
    如((func_vt.t & VT_BTYPE) == VT_FLOAT)
      o(0xEE100A10); /* fmrs r0, s0 */
    另 {
      o(0xEE100B10); /* fmrdl r0, d0 */
      o(0xEE301B10); /* fmrdh r1, d0 */
    }
  }
#了如
  o(0xE89BA800); /* restore fp, sp, pc */
  diff = (-loc + 3) & -4;
#如定义 TCC_ARM_EABI
  如(!leaffunc)
    diff = ((diff + 11) & -8) - 4;
#了如
  如(diff > 0) {
    x=stuff_const(0xE24BD000, diff); /* sub sp,fp,# */
    如(x)
      *(uint32_t *)(cur_text_section->data + func_sub_sp_offset) = x;
    另 {
      整 addr;
      addr=ind;
      o(0xE59FC004); /* ldr ip,[pc+4] */
      o(0xE04BD00C); /* sub sp,fp,ip  */
      o(0xE1A0F00E); /* mov pc,lr */
      o(diff);
      *(uint32_t *)(cur_text_section->data + func_sub_sp_offset) = 0xE1000000|encbranch(func_sub_sp_offset,addr,1);
    }
  }
}

/* generate a jump to a label */
整 gjmp(整 t)
{
  整 r;
  如 (nocode_wanted)
    返回 t;
  r=ind;
  o(0xE0000000|encbranch(r,t,1));
  返回 r;
}

/* generate a jump to a fixed address */
空 gjmp_addr(整 a)
{
  gjmp(a);
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
整 gtst(整 inv, 整 t)
{
  整 v, r;
  uint32_t op;

  v = vtop->r & VT_VALMASK;
  r=ind;

  如 (nocode_wanted) {
    ;
  } 另 如 (v == VT_CMP) {
    op=mapcc(inv?negcc(vtop->c.i):vtop->c.i);
    op|=encbranch(r,t,1);
    o(op);
    t=r;
  } 另 如 (v == VT_JMP || v == VT_JMPI) {
    如 ((v & 1) == inv) {
      如(!vtop->c.i)
        vtop->c.i=t;
      另 {
        uint32_t *x;
        整 p,lp;
        如(t) {
          p = vtop->c.i;
          运行 {
            p = decbranch(lp=p);
          } 当(p);
          x = (uint32_t *)(cur_text_section->data + lp);
          *x &= 0xff000000;
          *x |= encbranch(lp,t,1);
        }
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
  整 c, func = 0;
  uint32_t opc = 0, r, fr;
  无符 短 retreg = REG_IRET;

  c=0;
  转接(op) {
    事例 '+':
      opc = 0x8;
      c=1;
      跳出;
    事例 TOK_ADDC1: /* add with carry generation */
      opc = 0x9;
      c=1;
      跳出;
    事例 '-':
      opc = 0x4;
      c=1;
      跳出;
    事例 TOK_SUBC1: /* sub with carry generation */
      opc = 0x5;
      c=1;
      跳出;
    事例 TOK_ADDC2: /* add with carry use */
      opc = 0xA;
      c=1;
      跳出;
    事例 TOK_SUBC2: /* sub with carry use */
      opc = 0xC;
      c=1;
      跳出;
    事例 '&':
      opc = 0x0;
      c=1;
      跳出;
    事例 '^':
      opc = 0x2;
      c=1;
      跳出;
    事例 '|':
      opc = 0x18;
      c=1;
      跳出;
    事例 '*':
      gv2(RC_INT, RC_INT);
      r = vtop[-1].r;
      fr = vtop[0].r;
      vtop--;
      o(0xE0000090|(intr(r)<<16)|(intr(r)<<8)|intr(fr));
      返回;
    事例 TOK_SHL:
      opc = 0;
      c=2;
      跳出;
    事例 TOK_SHR:
      opc = 1;
      c=2;
      跳出;
    事例 TOK_SAR:
      opc = 2;
      c=2;
      跳出;
    事例 '/':
    事例 TOK_PDIV:
      func=TOK___divsi3;
      c=3;
      跳出;
    事例 TOK_UDIV:
      func=TOK___udivsi3;
      c=3;
      跳出;
    事例 '%':
#如定义 TCC_ARM_EABI
      func=TOK___aeabi_idivmod;
      retreg=REG_LRET;
#另
      func=TOK___modsi3;
#了如
      c=3;
      跳出;
    事例 TOK_UMOD:
#如定义 TCC_ARM_EABI
      func=TOK___aeabi_uidivmod;
      retreg=REG_LRET;
#另
      func=TOK___umodsi3;
#了如
      c=3;
      跳出;
    事例 TOK_UMULL:
      gv2(RC_INT, RC_INT);
      r=intr(vtop[-1].r2=get_reg(RC_INT));
      c=vtop[-1].r;
      vtop[-1].r=get_reg_ex(RC_INT,regmask(c));
      vtop--;
      o(0xE0800090|(r<<16)|(intr(vtop->r)<<12)|(intr(c)<<8)|intr(vtop[1].r));
      返回;
    缺省:
      opc = 0x15;
      c=1;
      跳出;
  }
  转接(c) {
    事例 1:
      如((vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
        如(opc == 4 || opc == 5 || opc == 0xc) {
          vswap();
          opc|=2; // sub -> rsb
        }
      }
      如 ((vtop->r & VT_VALMASK) == VT_CMP ||
          (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
        gv(RC_INT);
      vswap();
      c=intr(gv(RC_INT));
      vswap();
      opc=0xE0000000|(opc<<20)|(c<<16);
      如((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
        uint32_t x;
        x=stuff_const(opc|0x2000000,vtop->c.i);
        如(x) {
          r=intr(vtop[-1].r=get_reg_ex(RC_INT,regmask(vtop[-1].r)));
          o(x|(r<<12));
          跳转 done;
        }
      }
      fr=intr(gv(RC_INT));
      r=intr(vtop[-1].r=get_reg_ex(RC_INT,two2mask(vtop->r,vtop[-1].r)));
      o(opc|(r<<12)|fr);
done:
      vtop--;
      如 (op >= TOK_ULT && op <= TOK_GT) {
        vtop->r = VT_CMP;
        vtop->c.i = op;
      }
      跳出;
    事例 2:
      opc=0xE1A00000|(opc<<5);
      如 ((vtop->r & VT_VALMASK) == VT_CMP ||
          (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
        gv(RC_INT);
      vswap();
      r=intr(gv(RC_INT));
      vswap();
      opc|=r;
      如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
        fr=intr(vtop[-1].r=get_reg_ex(RC_INT,regmask(vtop[-1].r)));
        c = vtop->c.i & 0x1f;
        o(opc|(c<<7)|(fr<<12));
      } 另 {
        fr=intr(gv(RC_INT));
        c=intr(vtop[-1].r=get_reg_ex(RC_INT,two2mask(vtop->r,vtop[-1].r)));
        o(opc|(c<<12)|(fr<<8)|0x10);
      }
      vtop--;
      跳出;
    事例 3:
      vpush_global_sym(&func_old_type, func);
      vrott(3);
      gfunc_call(2);
      vpushi(0);
      vtop->r = retreg;
      跳出;
    缺省:
      tcc_error("gen_opi %i unimplemented!",op);
  }
}

#如定义 TCC_ARM_VFP
静态 整 is_zero(整 i)
{
  如((vtop[i].r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
    返回 0;
  如 (vtop[i].type.t == VT_FLOAT)
    返回 (vtop[i].c.f == 0.f);
  另 如 (vtop[i].type.t == VT_DOUBLE)
    返回 (vtop[i].c.d == 0.0);
  返回 (vtop[i].c.ld == 0.l);
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
 *    two operands are guaranteed to have the same floating point type */
空 gen_opf(整 op)
{
  uint32_t x;
  整 fneg=0,r;
  x=0xEE000A00|T2CPR(vtop->type.t);
  转接(op) {
    事例 '+':
      如(is_zero(-1))
        vswap();
      如(is_zero(0)) {
        vtop--;
        返回;
      }
      x|=0x300000;
      跳出;
    事例 '-':
      x|=0x300040;
      如(is_zero(0)) {
        vtop--;
        返回;
      }
      如(is_zero(-1)) {
        x|=0x810000; /* fsubX -> fnegX */
        vswap();
        vtop--;
        fneg=1;
      }
      跳出;
    事例 '*':
      x|=0x200000;
      跳出;
    事例 '/':
      x|=0x800000;
      跳出;
    缺省:
      如(op < TOK_ULT || op > TOK_GT) {
        tcc_error("unknown fp op %x!",op);
        返回;
      }
      如(is_zero(-1)) {
        vswap();
        转接(op) {
          事例 TOK_LT: op=TOK_GT; 跳出;
          事例 TOK_GE: op=TOK_ULE; 跳出;
          事例 TOK_LE: op=TOK_GE; 跳出;
          事例 TOK_GT: op=TOK_ULT; 跳出;
        }
      }
      x|=0xB40040; /* fcmpX */
      如(op!=TOK_EQ && op!=TOK_NE)
        x|=0x80; /* fcmpX -> fcmpeX */
      如(is_zero(0)) {
        vtop--;
        o(x|0x10000|(vfpr(gv(RC_FLOAT))<<12)); /* fcmp(e)X -> fcmp(e)zX */
      } 另 {
        x|=vfpr(gv(RC_FLOAT));
        vswap();
        o(x|(vfpr(gv(RC_FLOAT))<<12));
        vtop--;
      }
      o(0xEEF1FA10); /* fmstat */

      转接(op) {
        事例 TOK_LE: op=TOK_ULE; 跳出;
        事例 TOK_LT: op=TOK_ULT; 跳出;
        事例 TOK_UGE: op=TOK_GE; 跳出;
        事例 TOK_UGT: op=TOK_GT; 跳出;
      }

      vtop->r = VT_CMP;
      vtop->c.i = op;
      返回;
  }
  r=gv(RC_FLOAT);
  x|=vfpr(r);
  r=regmask(r);
  如(!fneg) {
    整 r2;
    vswap();
    r2=gv(RC_FLOAT);
    x|=vfpr(r2)<<16;
    r|=regmask(r2);
  }
  vtop->r=get_reg_ex(RC_FLOAT,r);
  如(!fneg)
    vtop--;
  o(x|(vfpr(vtop->r)<<12));
}

#另
静态 uint32_t is_fconst()
{
  长 双精 f;
  uint32_t r;
  如((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
    返回 0;
  如 (vtop->type.t == VT_FLOAT)
    f = vtop->c.f;
  另 如 (vtop->type.t == VT_DOUBLE)
    f = vtop->c.d;
  另
    f = vtop->c.ld;
  如(!ieee_finite(f))
    返回 0;
  r=0x8;
  如(f<0.0) {
    r=0x18;
    f=-f;
  }
  如(f==0.0)
    返回 r;
  如(f==1.0)
    返回 r|1;
  如(f==2.0)
    返回 r|2;
  如(f==3.0)
    返回 r|3;
  如(f==4.0)
    返回 r|4;
  如(f==5.0)
    返回 r|5;
  如(f==0.5)
    返回 r|6;
  如(f==10.0)
    返回 r|7;
  返回 0;
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranted to have the same floating point type */
空 gen_opf(整 op)
{
  uint32_t x, r, r2, c1, c2;
  //fputs("gen_opf\n",stderr);
  vswap();
  c1 = is_fconst();
  vswap();
  c2 = is_fconst();
  x=0xEE000100;
#如 LDOUBLE_SIZE == 8
  如 ((vtop->type.t & VT_BTYPE) != VT_FLOAT)
    x|=0x80;
#另
  如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE)
    x|=0x80;
  另 如 ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE)
    x|=0x80000;
#了如
  转接(op)
  {
    事例 '+':
      如(!c2) {
        vswap();
        c2=c1;
      }
      vswap();
      r=fpr(gv(RC_FLOAT));
      vswap();
      如(c2) {
        如(c2>0xf)
          x|=0x200000; // suf
        r2=c2&0xf;
      } 另 {
        r2=fpr(gv(RC_FLOAT));
      }
      跳出;
    事例 '-':
      如(c2) {
        如(c2<=0xf)
          x|=0x200000; // suf
        r2=c2&0xf;
        vswap();
        r=fpr(gv(RC_FLOAT));
        vswap();
      } 另 如(c1 && c1<=0xf) {
        x|=0x300000; // rsf
        r2=c1;
        r=fpr(gv(RC_FLOAT));
        vswap();
      } 另 {
        x|=0x200000; // suf
        vswap();
        r=fpr(gv(RC_FLOAT));
        vswap();
        r2=fpr(gv(RC_FLOAT));
      }
      跳出;
    事例 '*':
      如(!c2 || c2>0xf) {
        vswap();
        c2=c1;
      }
      vswap();
      r=fpr(gv(RC_FLOAT));
      vswap();
      如(c2 && c2<=0xf)
        r2=c2;
      另
        r2=fpr(gv(RC_FLOAT));
      x|=0x100000; // muf
      跳出;
    事例 '/':
      如(c2 && c2<=0xf) {
        x|=0x400000; // dvf
        r2=c2;
        vswap();
        r=fpr(gv(RC_FLOAT));
        vswap();
      } 另 如(c1 && c1<=0xf) {
        x|=0x500000; // rdf
        r2=c1;
        r=fpr(gv(RC_FLOAT));
        vswap();
      } 另 {
        x|=0x400000; // dvf
        vswap();
        r=fpr(gv(RC_FLOAT));
        vswap();
        r2=fpr(gv(RC_FLOAT));
      }
      跳出;
    缺省:
      如(op >= TOK_ULT && op <= TOK_GT) {
        x|=0xd0f110; // cmfe
/* bug (intention?) in Linux FPU emulator
   doesn't set carry if equal */
        转接(op) {
          事例 TOK_ULT:
          事例 TOK_UGE:
          事例 TOK_ULE:
          事例 TOK_UGT:
            tcc_error("unsigned comparison on floats?");
            跳出;
          事例 TOK_LT:
            op=TOK_Nset;
            跳出;
          事例 TOK_LE:
            op=TOK_ULE; /* correct in unordered case only if AC bit in FPSR set */
            跳出;
          事例 TOK_EQ:
          事例 TOK_NE:
            x&=~0x400000; // cmfe -> cmf
            跳出;
        }
        如(c1 && !c2) {
          c2=c1;
          vswap();
          转接(op) {
            事例 TOK_Nset:
              op=TOK_GT;
              跳出;
            事例 TOK_GE:
              op=TOK_ULE;
              跳出;
            事例 TOK_ULE:
              op=TOK_GE;
              跳出;
            事例 TOK_GT:
              op=TOK_Nset;
              跳出;
          }
        }
        vswap();
        r=fpr(gv(RC_FLOAT));
        vswap();
        如(c2) {
          如(c2>0xf)
            x|=0x200000;
          r2=c2&0xf;
        } 另 {
          r2=fpr(gv(RC_FLOAT));
        }
        vtop[-1].r = VT_CMP;
        vtop[-1].c.i = op;
      } 另 {
        tcc_error("unknown fp op %x!",op);
        返回;
      }
  }
  如(vtop[-1].r == VT_CMP)
    c1=15;
  另 {
    c1=vtop->r;
    如(r2&0x8)
      c1=vtop[-1].r;
    vtop[-1].r=get_reg_ex(RC_FLOAT,two2mask(vtop[-1].r,c1));
    c1=fpr(vtop[-1].r);
  }
  vtop--;
  o(x|(r<<16)|(c1<<12)|r2);
}
#了如

/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
ST_FUNC 空 gen_cvt_itof1(整 t)
{
  uint32_t r, r2;
  整 bt;
  bt=vtop->type.t & VT_BTYPE;
  如(bt == VT_INT || bt == VT_SHORT || bt == VT_BYTE) {
#如未定义 TCC_ARM_VFP
    uint32_t dsize = 0;
#了如
    r=intr(gv(RC_INT));
#如定义 TCC_ARM_VFP
    r2=vfpr(vtop->r=get_reg(RC_FLOAT));
    o(0xEE000A10|(r<<12)|(r2<<16)); /* fmsr */
    r2|=r2<<12;
    如(!(vtop->type.t & VT_UNSIGNED))
      r2|=0x80;                /* fuitoX -> fsituX */
    o(0xEEB80A40|r2|T2CPR(t)); /* fYitoX*/
#另
    r2=fpr(vtop->r=get_reg(RC_FLOAT));
    如((t & VT_BTYPE) != VT_FLOAT)
      dsize=0x80;    /* flts -> fltd */
    o(0xEE000110|dsize|(r2<<16)|(r<<12)); /* flts */
    如((vtop->type.t & (VT_UNSIGNED|VT_BTYPE)) == (VT_UNSIGNED|VT_INT)) {
      uint32_t off = 0;
      o(0xE3500000|(r<<12));        /* cmp */
      r=fpr(get_reg(RC_FLOAT));
      如(last_itod_magic) {
        off=ind+8-last_itod_magic;
        off/=4;
        如(off>255)
          off=0;
      }
      o(0xBD1F0100|(r<<12)|off);    /* ldflts */
      如(!off) {
        o(0xEA000000);              /* b */
        last_itod_magic=ind;
        o(0x4F800000);              /* 4294967296.0f */
      }
      o(0xBE000100|dsize|(r2<<16)|(r2<<12)|r); /* adflt */
    }
#了如
    返回;
  } 另 如(bt == VT_LLONG) {
    整 func;
    CType *func_type = 0;
    如((t & VT_BTYPE) == VT_FLOAT) {
      func_type = &func_float_type;
      如(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundisf;
      另
        func=TOK___floatdisf;
#如 LDOUBLE_SIZE != 8
    } 另 如((t & VT_BTYPE) == VT_LDOUBLE) {
      func_type = &func_ldouble_type;
      如(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundixf;
      另
        func=TOK___floatdixf;
    } 另 如((t & VT_BTYPE) == VT_DOUBLE) {
#另
    } 另 如((t & VT_BTYPE) == VT_DOUBLE || (t & VT_BTYPE) == VT_LDOUBLE) {
#了如
      func_type = &func_double_type;
      如(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundidf;
      另
        func=TOK___floatdidf;
    }
    如(func_type) {
      vpush_global_sym(func_type, func);
      vswap();
      gfunc_call(1);
      vpushi(0);
      vtop->r=TREG_F0;
      返回;
    }
  }
  tcc_error("unimplemented gen_cvt_itof %x!",vtop->type.t);
}

/* convert fp to int 't' type */
空 gen_cvt_ftoi(整 t)
{
  uint32_t r, r2;
  整 u, func = 0;
  u=t&VT_UNSIGNED;
  t&=VT_BTYPE;
  r2=vtop->type.t & VT_BTYPE;
  如(t==VT_INT) {
#如定义 TCC_ARM_VFP
    r=vfpr(gv(RC_FLOAT));
    u=u?0:0x10000;
    o(0xEEBC0AC0|(r<<12)|r|T2CPR(r2)|u); /* ftoXizY */
    r2=intr(vtop->r=get_reg(RC_INT));
    o(0xEE100A10|(r<<16)|(r2<<12));
    返回;
#另
    如(u) {
      如(r2 == VT_FLOAT)
        func=TOK___fixunssfsi;
#如 LDOUBLE_SIZE != 8
      另 如(r2 == VT_LDOUBLE)
        func=TOK___fixunsxfsi;
      另 如(r2 == VT_DOUBLE)
#另
      另 如(r2 == VT_LDOUBLE || r2 == VT_DOUBLE)
#了如
        func=TOK___fixunsdfsi;
    } 另 {
      r=fpr(gv(RC_FLOAT));
      r2=intr(vtop->r=get_reg(RC_INT));
      o(0xEE100170|(r2<<12)|r);
      返回;
    }
#了如
  } 另 如(t == VT_LLONG) { // unsigned handled in gen_cvt_ftoi1
    如(r2 == VT_FLOAT)
      func=TOK___fixsfdi;
#如 LDOUBLE_SIZE != 8
    另 如(r2 == VT_LDOUBLE)
      func=TOK___fixxfdi;
    另 如(r2 == VT_DOUBLE)
#另
    另 如(r2 == VT_LDOUBLE || r2 == VT_DOUBLE)
#了如
      func=TOK___fixdfdi;
  }
  如(func) {
    vpush_global_sym(&func_old_type, func);
    vswap();
    gfunc_call(1);
    vpushi(0);
    如(t == VT_LLONG)
      vtop->r2 = REG_LRET;
    vtop->r = REG_IRET;
    返回;
  }
  tcc_error("unimplemented gen_cvt_ftoi!");
}

/* convert from one floating point type to another */
空 gen_cvt_ftof(整 t)
{
#如定义 TCC_ARM_VFP
  如(((vtop->type.t & VT_BTYPE) == VT_FLOAT) != ((t & VT_BTYPE) == VT_FLOAT)) {
    uint32_t r = vfpr(gv(RC_FLOAT));
    o(0xEEB70AC0|(r<<12)|r|T2CPR(vtop->type.t));
  }
#另
  /* all we have to do on i386 and FPA ARM is to put the float in a register */
  gv(RC_FLOAT);
#了如
}

/* computed goto support */
空 ggoto(空)
{
  gcall_or_jmp(1);
  vtop--;
}

/* Save the stack pointer onto the stack and return the location of its address */
ST_FUNC 空 gen_vla_sp_save(整 addr) {
    SValue v;
    v.type.t = VT_PTR;
    v.r = VT_LOCAL | VT_LVAL;
    v.c.i = addr;
    store(TREG_SP, &v);
}

/* Restore the SP from a location on the stack */
ST_FUNC 空 gen_vla_sp_restore(整 addr) {
    SValue v;
    v.type.t = VT_PTR;
    v.r = VT_LOCAL | VT_LVAL;
    v.c.i = addr;
    load(TREG_SP, &v);
}

/* Subtract from the stack pointer, and push the resulting value onto the stack */
ST_FUNC 空 gen_vla_alloc(CType *type, 整 align) {
    整 r = intr(gv(RC_INT));
    o(0xE04D0000|(r<<12)|r); /* sub r, sp, r */
#如定义 TCC_ARM_EABI
    如 (align < 8)
        align = 8;
#另
    如 (align < 4)
        align = 4;
#了如
    如 (align & (align - 1))
        tcc_error("alignment is not a power of 2: %i", align);
    o(stuff_const(0xE3C0D000|(r<<16), align - 1)); /* bic sp, r, #align-1 */
    vpop();
}

/* end of ARM code generator */
/*************************************************************/
#了如
/*************************************************************/
