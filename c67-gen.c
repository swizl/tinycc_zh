/*
 *  TMS320C67xx code generator for TCC
 * 
 *  Copyright (c) 2001, 2002 Fabrice Bellard
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

/* #定义 ASSEMBLY_LISTING_C67 */

/* number of available registers */
#定义 NB_REGS            24

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#定义 RC_INT     0x0001   /* generic integer register */
#定义 RC_FLOAT   0x0002   /* generic float register */
#定义 RC_EAX     0x0004
#定义 RC_ST0     0x0008
#定义 RC_ECX     0x0010
#定义 RC_EDX     0x0020
#定义 RC_INT_BSIDE  0x00000040    /* generic integer register  on b side */
#定义 RC_C67_A4     0x00000100
#定义 RC_C67_A5     0x00000200
#定义 RC_C67_B4     0x00000400
#定义 RC_C67_B5     0x00000800
#定义 RC_C67_A6     0x00001000
#定义 RC_C67_A7     0x00002000
#定义 RC_C67_B6     0x00004000
#定义 RC_C67_B7     0x00008000
#定义 RC_C67_A8     0x00010000
#定义 RC_C67_A9     0x00020000
#定义 RC_C67_B8     0x00040000
#定义 RC_C67_B9     0x00080000
#定义 RC_C67_A10    0x00100000
#定义 RC_C67_A11    0x00200000
#定义 RC_C67_B10    0x00400000
#定义 RC_C67_B11    0x00800000
#定义 RC_C67_A12    0x01000000
#定义 RC_C67_A13    0x02000000
#定义 RC_C67_B12    0x04000000
#定义 RC_C67_B13    0x08000000
#定义 RC_IRET    RC_C67_A4        /* function return: integer register */
#定义 RC_LRET    RC_C67_A5        /* function return: second integer register */
#定义 RC_FRET    RC_C67_A4        /* function return: float register */

/* pretty names for the registers */
枚举 {
    TREG_EAX = 0,               // really A2
    TREG_ECX,                   // really A3
    TREG_EDX,                   // really B0
    TREG_ST0,                   // really B1
    TREG_C67_A4,
    TREG_C67_A5,
    TREG_C67_B4,
    TREG_C67_B5,
    TREG_C67_A6,
    TREG_C67_A7,
    TREG_C67_B6,
    TREG_C67_B7,
    TREG_C67_A8,
    TREG_C67_A9,
    TREG_C67_B8,
    TREG_C67_B9,
    TREG_C67_A10,
    TREG_C67_A11,
    TREG_C67_B10,
    TREG_C67_B11,
    TREG_C67_A12,
    TREG_C67_A13,
    TREG_C67_B12,
    TREG_C67_B13,
};

/* return registers for function */
#定义 REG_IRET TREG_C67_A4        /* single word int return register */
#定义 REG_LRET TREG_C67_A5        /* second word return register (for long long) */
#定义 REG_FRET TREG_C67_A4        /* float return register */

/* defined if function parameters must be evaluated in reverse order */
/* #定义 INVERT_FUNC_PARAMS */

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #定义 FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#定义 PTR_SIZE 4

/* long double size and alignment, in bytes */
#定义 LDOUBLE_SIZE  12
#定义 LDOUBLE_ALIGN 4
/* maximum alignment (for aligned attribute support) */
#定义 MAX_ALIGN     8

/******************************************************/
#另 /* ! TARGET_DEFS_ONLY */
/******************************************************/
#包含 "tcc.h"

ST_DATA 不变 整 reg_classes[NB_REGS] = {
    /* eax */ RC_INT | RC_FLOAT | RC_EAX, 
    // only allow even regs for floats (allow for doubles)
    /* ecx */ RC_INT | RC_ECX,
    /* edx */ RC_INT | RC_INT_BSIDE | RC_FLOAT | RC_EDX,
    // only allow even regs for floats (allow for doubles)
    /* st0 */ RC_INT | RC_INT_BSIDE | RC_ST0,
    /* A4  */ RC_C67_A4,
    /* A5  */ RC_C67_A5,
    /* B4  */ RC_C67_B4,
    /* B5  */ RC_C67_B5,
    /* A6  */ RC_C67_A6,
    /* A7  */ RC_C67_A7,
    /* B6  */ RC_C67_B6,
    /* B7  */ RC_C67_B7,
    /* A8  */ RC_C67_A8,
    /* A9  */ RC_C67_A9,
    /* B8  */ RC_C67_B8,
    /* B9  */ RC_C67_B9,
    /* A10  */ RC_C67_A10,
    /* A11  */ RC_C67_A11,
    /* B10  */ RC_C67_B10,
    /* B11  */ RC_C67_B11,
    /* A12  */ RC_C67_A10,
    /* A13  */ RC_C67_A11,
    /* B12  */ RC_C67_B10,
    /* B13  */ RC_C67_B11
};

// although tcc thinks it is passing parameters on the stack,
// the C67 really passes up to the first 10 params in special
// regs or regs pairs (for 64 bit params).  So keep track of
// the stack offsets so we can translate to the appropriate 
// reg (pair)

#定义 NoCallArgsPassedOnStack 10
整 NoOfCurFuncArgs;
整 TranslateStackToReg[NoCallArgsPassedOnStack];
整 ParamLocOnStack[NoCallArgsPassedOnStack];
整 TotalBytesPushedOnStack;

#如未定义 FALSE
# 定义 FALSE 0
# 定义 TRUE 1
#了如

#消定义 BOOL
#定义 BOOL 整

#定义 ALWAYS_ASSERT(x) \
运行 {\
   如 (!(x))\
       tcc_error("internal compiler error file at %s:%d", __文件__, __行号__);\
} 当 (0)

/******************************************************/
静态 无符 long func_sub_sp_offset;
静态 整 func_ret_sub;

静态 BOOL C67_invert_test;
静态 整 C67_compare_reg;

#如定义 ASSEMBLY_LISTING_C67
FILE *f = NULL;
#了如

空 C67_g(整 c)
{
    整 ind1;
    如 (nocode_wanted)
        返回;
#如定义 ASSEMBLY_LISTING_C67
    fprintf(f, " %08X", c);
#了如
    ind1 = ind + 4;
    如 (ind1 > (整) cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind] = c & 0xff;
    cur_text_section->data[ind + 1] = (c >> 8) & 0xff;
    cur_text_section->data[ind + 2] = (c >> 16) & 0xff;
    cur_text_section->data[ind + 3] = (c >> 24) & 0xff;
    ind = ind1;
}


/* output a symbol and patch all calls to it */
空 gsym_addr(整 t, 整 a)
{
    整 n, *ptr;
    当 (t) {
        ptr = (整 *) (cur_text_section->data + t);
        {
            Sym *sym;

            // extract 32 bit address from MVKH/MVKL
            n = ((*ptr >> 7) & 0xffff);
            n |= ((*(ptr + 1) >> 7) & 0xffff) << 16;

            // define a label that will be relocated

            sym = get_sym_ref(&char_pointer_type, cur_text_section, a, 0);
            greloc(cur_text_section, sym, t, R_C60LO16);
            greloc(cur_text_section, sym, t + 4, R_C60HI16);

            // clear out where the pointer was

            *ptr &= ~(0xffff << 7);
            *(ptr + 1) &= ~(0xffff << 7);
        }
        t = n;
    }
}

空 gsym(整 t)
{
    gsym_addr(t, ind);
}

// these are regs that tcc doesn't really know about, 
// but assign them unique values so the mapping routines
// can distinguish them

#定义 C67_A0 105
#定义 C67_SP 106
#定义 C67_B3 107
#定义 C67_FP 108
#定义 C67_B2 109
#定义 C67_CREG_ZERO -1    /* Special code for no condition reg test */


整 ConvertRegToRegClass(整 r)
{
    // only works for A4-B13

    返回 RC_C67_A4 << (r - TREG_C67_A4);
}


// map TCC reg to C67 reg number

整 C67_map_regn(整 r)
{
    如 (r == 0)                 // normal tcc regs
        返回 0x2;             // A2
    另 如 (r == 1)            // normal tcc regs
        返回 3;               // A3
    另 如 (r == 2)            // normal tcc regs
        返回 0;               // B0
    另 如 (r == 3)            // normal tcc regs
        返回 1;               // B1
    另 如 (r >= TREG_C67_A4 && r <= TREG_C67_B13)     // these form a pattern of alt pairs
        返回 (((r & 0xfffffffc) >> 1) | (r & 1)) + 2;
    另 如 (r == C67_A0)
        返回 0;               // set to A0 (offset reg)
    另 如 (r == C67_B2)
        返回 2;               // set to B2 (offset reg)
    另 如 (r == C67_B3)
        返回 3;               // set to B3 (return address reg)
    另 如 (r == C67_SP)
        返回 15;              // set to SP (B15) (offset reg)
    另 如 (r == C67_FP)
        返回 15;              // set to FP (A15) (offset reg)
    另 如 (r == C67_CREG_ZERO)
        返回 0;               // Special code for no condition reg test
    另
        ALWAYS_ASSERT(FALSE);

    返回 0;
}

// mapping from tcc reg number to 
// C67 register to condition code field
//
// valid condition code regs are:
//
// tcc reg 2 ->B0 -> 1
// tcc reg 3 ->B1 -> 2
// tcc reg 0 -> A2 -> 5
// tcc reg 1 -> A3 -> X
// tcc reg      B2 -> 3

整 C67_map_regc(整 r)
{
    如 (r == 0)                 // normal tcc regs
        返回 0x5;
    另 如 (r == 2)            // normal tcc regs
        返回 0x1;
    另 如 (r == 3)            // normal tcc regs
        返回 0x2;
    另 如 (r == C67_B2)       // normal tcc regs
        返回 0x3;
    另 如 (r == C67_CREG_ZERO)
        返回 0;               // Special code for no condition reg test
    另
        ALWAYS_ASSERT(FALSE);

    返回 0;
}


// map TCC reg to C67 reg side A or B

整 C67_map_regs(整 r)
{
    如 (r == 0)                 // normal tcc regs
        返回 0x0;
    另 如 (r == 1)            // normal tcc regs
        返回 0x0;
    另 如 (r == 2)            // normal tcc regs
        返回 0x1;
    另 如 (r == 3)            // normal tcc regs
        返回 0x1;
    另 如 (r >= TREG_C67_A4 && r <= TREG_C67_B13)     // these form a pattern of alt pairs
        返回 (r & 2) >> 1;
    另 如 (r == C67_A0)
        返回 0;               // set to A side 
    另 如 (r == C67_B2)
        返回 1;               // set to B side 
    另 如 (r == C67_B3)
        返回 1;               // set to B side
    另 如 (r == C67_SP)
        返回 0x1;             // set to SP (B15) B side 
    另 如 (r == C67_FP)
        返回 0x0;             // set to FP (A15) A side 
    另
        ALWAYS_ASSERT(FALSE);

    返回 0;
}

整 C67_map_S12(字 *s)
{
    如 (strstr(s, ".S1") != NULL)
        返回 0;
    另 如 (strcmp(s, ".S2"))
        返回 1;
    另
        ALWAYS_ASSERT(FALSE);

    返回 0;
}

整 C67_map_D12(字 *s)
{
    如 (strstr(s, ".D1") != NULL)
        返回 0;
    另 如 (strcmp(s, ".D2"))
        返回 1;
    另
        ALWAYS_ASSERT(FALSE);

    返回 0;
}



空 C67_asm(字 *s, 整 a, 整 b, 整 c)
{
    BOOL xpath;

#如定义 ASSEMBLY_LISTING_C67
    如 (!f) {
        f = fopen("TCC67_out.txt", "wt");
    }
    fprintf(f, "%04X ", ind);
#了如

    如 (strstr(s, "MVKL") == s) {
        C67_g((C67_map_regn(b) << 23) |
              ((a & 0xffff) << 7) | (0x0a << 2) | (C67_map_regs(b) << 1));
    } 另 如 (strstr(s, "MVKH") == s) {
        C67_g((C67_map_regn(b) << 23) |
              (((a >> 16) & 0xffff) << 7) |
              (0x1a << 2) | (C67_map_regs(b) << 1));
    } 另 如 (strstr(s, "STW.D SP POST DEC") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //SP B15
              (2 << 13) |       //ucst5 (must keep 8 byte boundary !!)
              (0xa << 9) |      //mode a = post dec ucst
              (0 << 8) |        //r (LDDW bit 0)
              (1 << 7) |        //y D1/D2 use B side
              (7 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STB.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2 A side
              (3 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STH.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2 A side
              (5 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STB.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2 A side
              (3 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STH.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2 A side
              (5 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STW.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2 A side
              (7 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STW.D *") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (C67_map_regn(b) << 18) | //base reg A0
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(b) << 7) |  //y D1/D2 base reg side
              (7 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STH.D *") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (C67_map_regn(b) << 18) | //base reg A0
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(b) << 7) |  //y D1/D2 base reg side
              (5 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STB.D *") == s) {
        C67_g((C67_map_regn(a) << 23) | //src
              (C67_map_regn(b) << 18) | //base reg A0
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(b) << 7) |  //y D1/D2 base reg side
              (3 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "STW.D +*") == s) {
        ALWAYS_ASSERT(c < 32);
        C67_g((C67_map_regn(a) << 23) | //src
              (C67_map_regn(b) << 18) | //base reg A0
              (c << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(b) << 7) |  //y D1/D2 base reg side
              (7 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of src
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDW.D SP PRE INC") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg B15
              (2 << 13) |       //ucst5 (must keep 8 byte boundary)
              (9 << 9) |        //mode 9 = pre inc ucst5
              (0 << 8) |        //r (LDDW bit 0)
              (1 << 7) |        //y D1/D2  B side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDDW.D SP PRE INC") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg B15
              (1 << 13) |       //ucst5 (must keep 8 byte boundary)
              (9 << 9) |        //mode 9 = pre inc ucst5
              (1 << 8) |        //r (LDDW bit 1)
              (1 << 7) |        //y D1/D2  B side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDW.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2  A side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDDW.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (1 << 8) |        //r (LDDW bit 1)
              (0 << 7) |        //y D1/D2  A side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDH.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2  A side
              (4 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDB.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2  A side
              (2 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDHU.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2  A side
              (0 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDBU.D *+SP[A0]") == s) {
        C67_g((C67_map_regn(a) << 23) | //dst
              (15 << 18) |      //base reg A15
              (0 << 13) |       //offset reg A0
              (5 << 9) |        //mode 5 = pos offset, base reg + off reg
              (0 << 8) |        //r (LDDW bit 0)
              (0 << 7) |        //y D1/D2  A side
              (1 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(a) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDW.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDDW.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (1 << 8) |        //r (LDDW bit 1)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDH.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (4 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDB.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (2 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDHU.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (0 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDBU.D *") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (0 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (1 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "LDW.D +*") == s) {
        C67_g((C67_map_regn(b) << 23) | //dst
              (C67_map_regn(a) << 18) | //base reg A15
              (1 << 13) |       //cst5
              (1 << 9) |        //mode 1 = pos cst offset
              (0 << 8) |        //r (LDDW bit 0)
              (C67_map_regs(a) << 7) |  //y D1/D2  src side
              (6 << 4) |        //ldst 3=STB, 5=STH 5, 7=STW, 6=LDW 4=LDH 2=LDB 0=LDHU 1=LDBU 
              (1 << 2) |        //opcode
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPLTSP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x3a << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPGTSP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x39 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPEQSP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x38 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    }

    另 如 (strstr(s, "CMPLTDP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x2a << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPGTDP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x29 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPEQDP") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x28 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPLT") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x57 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPGT") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x47 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPEQ") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x53 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPLTU") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x5f << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "CMPGTU") == s) {
        xpath = C67_map_regs(a) ^ C67_map_regs(b);
        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1
              (xpath << 12) |   //x use cross path for src2
              (0x4f << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side for reg c
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "B DISP") == s) {
        C67_g((0 << 29) |       //creg
              (0 << 28) |       //z
              (a << 7) |        //cnst
              (0x4 << 2) |      //opcode fixed
              (0 << 1) |        //S0/S1
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "B.") == s) {
        xpath = C67_map_regs(c) ^ 1;

        C67_g((C67_map_regc(b) << 29) | //creg
              (a << 28) |       //inv
              (0 << 23) |       //dst
              (C67_map_regn(c) << 18) | //src2
              (0 << 13) |       //
              (xpath << 12) |   //x cross path if !B side
              (0xd << 6) |      //opcode
              (0x8 << 2) |      //opcode fixed
              (1 << 1) |        //must be S2
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "MV.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (0 << 13) |       //src1 (cst5)
              (xpath << 12) |   //x cross path if opposite sides
              (0x2 << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SPTRUNC.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0xb << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "DPTRUNC.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              ((C67_map_regn(b) + 1) << 18) |   //src2   WEIRD CPU must specify odd reg for some reason
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x1 << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "INTSP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2   
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x4a << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "INTSPU.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2  
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x49 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "INTDP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2  
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x39 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "INTDPU.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              ((C67_map_regn(b) + 1) << 18) |   //src2   WEIRD CPU must specify odd reg for some reason
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x3b << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SPDP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (0 << 13) |       //src1 NA
              (xpath << 12) |   //x cross path if opposite sides
              (0x2 << 6) |      //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "DPSP.L") == s) {
        ALWAYS_ASSERT(C67_map_regs(b) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              ((C67_map_regn(b) + 1) << 18) |   //src2 WEIRD CPU must specify odd reg for some reason
              (0 << 13) |       //src1 NA
              (0 << 12) |       //x cross path if opposite sides
              (0x9 << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "ADD.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x3 << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SUB.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x7 << 5) |      //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "OR.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x7f << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "AND.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x7b << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "XOR.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x6f << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "ADDSP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x10 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "ADDDP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x18 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SUBSP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x11 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SUBDP.L") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x19 << 5) |     //opcode
              (0x6 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "MPYSP.M") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x1c << 7) |     //opcode
              (0x0 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "MPYDP.M") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2 (possible x path)
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x0e << 7) |     //opcode
              (0x0 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "MPYI.M") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(a) == C67_map_regs(c));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1 (cst5)
              (xpath << 12) |   //x cross path if opposite sides
              (0x4 << 7) |      //opcode
              (0x0 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SHR.S") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x37 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SHRU.S") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x27 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "SHL.S") == s) {
        xpath = C67_map_regs(b) ^ C67_map_regs(c);

        ALWAYS_ASSERT(C67_map_regs(c) == C67_map_regs(a));

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(c) << 23) | //dst
              (C67_map_regn(b) << 18) | //src2
              (C67_map_regn(a) << 13) | //src1 
              (xpath << 12) |   //x cross path if opposite sides
              (0x33 << 6) |     //opcode
              (0x8 << 2) |      //opcode fixed
              (C67_map_regs(c) << 1) |  //side of dest
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "||ADDK") == s) {
        xpath = 0;              // no xpath required just use the side of the src/dst

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(b) << 23) | //dst
              (a << 07) |       //scst16
              (0x14 << 2) |     //opcode fixed
              (C67_map_regs(b) << 1) |  //side of dst
              (1 << 0));        //parallel
    } 另 如 (strstr(s, "ADDK") == s) {
        xpath = 0;              // no xpath required just use the side of the src/dst

        C67_g((0 << 29) |       //creg
              (0 << 28) |       //inv
              (C67_map_regn(b) << 23) | //dst
              (a << 07) |       //scst16
              (0x14 << 2) |     //opcode fixed
              (C67_map_regs(b) << 1) |  //side of dst
              (0 << 0));        //parallel
    } 另 如 (strstr(s, "NOP") == s) {
        C67_g(((a - 1) << 13) | //no of cycles
              (0 << 0));        //parallel
    } 另
        ALWAYS_ASSERT(FALSE);

#如定义 ASSEMBLY_LISTING_C67
    fprintf(f, " %s %d %d %d\n", s, a, b, c);
#了如

}

//r=reg to load, fr=from reg, symbol for relocation, constant

空 C67_MVKL(整 r, 整 fc)
{
    C67_asm("MVKL.", fc, r, 0);
}

空 C67_MVKH(整 r, 整 fc)
{
    C67_asm("MVKH.", fc, r, 0);
}

空 C67_STB_SP_A0(整 r)
{
    C67_asm("STB.D *+SP[A0]", r, 0, 0); // STB  r,*+SP[A0]
}

空 C67_STH_SP_A0(整 r)
{
    C67_asm("STH.D *+SP[A0]", r, 0, 0); // STH  r,*+SP[A0]
}

空 C67_STW_SP_A0(整 r)
{
    C67_asm("STW.D *+SP[A0]", r, 0, 0); // STW  r,*+SP[A0]
}

空 C67_STB_PTR(整 r, 整 r2)
{
    C67_asm("STB.D *", r, r2, 0);       // STB  r, *r2
}

空 C67_STH_PTR(整 r, 整 r2)
{
    C67_asm("STH.D *", r, r2, 0);       // STH  r, *r2
}

空 C67_STW_PTR(整 r, 整 r2)
{
    C67_asm("STW.D *", r, r2, 0);       // STW  r, *r2
}

空 C67_STW_PTR_PRE_INC(整 r, 整 r2, 整 n)
{
    C67_asm("STW.D +*", r, r2, n);      // STW  r, *+r2
}

空 C67_PUSH(整 r)
{
    C67_asm("STW.D SP POST DEC", r, 0, 0);      // STW  r,*SP--
}

空 C67_LDW_SP_A0(整 r)
{
    C67_asm("LDW.D *+SP[A0]", r, 0, 0); // LDW  *+SP[A0],r
}

空 C67_LDDW_SP_A0(整 r)
{
    C67_asm("LDDW.D *+SP[A0]", r, 0, 0);        // LDDW  *+SP[A0],r
}

空 C67_LDH_SP_A0(整 r)
{
    C67_asm("LDH.D *+SP[A0]", r, 0, 0); // LDH  *+SP[A0],r
}

空 C67_LDB_SP_A0(整 r)
{
    C67_asm("LDB.D *+SP[A0]", r, 0, 0); // LDB  *+SP[A0],r
}

空 C67_LDHU_SP_A0(整 r)
{
    C67_asm("LDHU.D *+SP[A0]", r, 0, 0);        // LDHU  *+SP[A0],r
}

空 C67_LDBU_SP_A0(整 r)
{
    C67_asm("LDBU.D *+SP[A0]", r, 0, 0);        // LDBU  *+SP[A0],r
}

空 C67_LDW_PTR(整 r, 整 r2)
{
    C67_asm("LDW.D *", r, r2, 0);       // LDW  *r,r2
}

空 C67_LDDW_PTR(整 r, 整 r2)
{
    C67_asm("LDDW.D *", r, r2, 0);      // LDDW  *r,r2
}

空 C67_LDH_PTR(整 r, 整 r2)
{
    C67_asm("LDH.D *", r, r2, 0);       // LDH  *r,r2
}

空 C67_LDB_PTR(整 r, 整 r2)
{
    C67_asm("LDB.D *", r, r2, 0);       // LDB  *r,r2
}

空 C67_LDHU_PTR(整 r, 整 r2)
{
    C67_asm("LDHU.D *", r, r2, 0);      // LDHU  *r,r2
}

空 C67_LDBU_PTR(整 r, 整 r2)
{
    C67_asm("LDBU.D *", r, r2, 0);      // LDBU  *r,r2
}

空 C67_LDW_PTR_PRE_INC(整 r, 整 r2)
{
    C67_asm("LDW.D +*", r, r2, 0);      // LDW  *+r,r2
}

空 C67_POP(整 r)
{
    C67_asm("LDW.D SP PRE INC", r, 0, 0);       // LDW  *++SP,r
}

空 C67_POP_DW(整 r)
{
    C67_asm("LDDW.D SP PRE INC", r, 0, 0);      // LDDW  *++SP,r
}

空 C67_CMPLT(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPLT.L1", s1, s2, dst);
}

空 C67_CMPGT(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPGT.L1", s1, s2, dst);
}

空 C67_CMPEQ(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPEQ.L1", s1, s2, dst);
}

空 C67_CMPLTU(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPLTU.L1", s1, s2, dst);
}

空 C67_CMPGTU(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPGTU.L1", s1, s2, dst);
}


空 C67_CMPLTSP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPLTSP.S1", s1, s2, dst);
}

空 C67_CMPGTSP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPGTSP.S1", s1, s2, dst);
}

空 C67_CMPEQSP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPEQSP.S1", s1, s2, dst);
}

空 C67_CMPLTDP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPLTDP.S1", s1, s2, dst);
}

空 C67_CMPGTDP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPGTDP.S1", s1, s2, dst);
}

空 C67_CMPEQDP(整 s1, 整 s2, 整 dst)
{
    C67_asm("CMPEQDP.S1", s1, s2, dst);
}


空 C67_IREG_B_REG(整 inv, 整 r1, 整 r2)    // [!R] B  r2
{
    C67_asm("B.S2", inv, r1, r2);
}


// call with how many 32 bit words to skip
// (0 would branch to the branch instruction)

空 C67_B_DISP(整 disp)       //  B  +2  Branch with constant displacement
{
    // Branch point is relative to the 8 word fetch packet
    //
    // we will assume the text section always starts on an 8 word (32 byte boundary)
    //
    // so add in how many words into the fetch packet the branch is


    C67_asm("B DISP", disp + ((ind & 31) >> 2), 0, 0);
}

空 C67_NOP(整 n)
{
    C67_asm("NOP", n, 0, 0);
}

空 C67_ADDK(整 n, 整 r)
{
    ALWAYS_ASSERT(abs(n) < 32767);

    C67_asm("ADDK", n, r, 0);
}

空 C67_ADDK_PARALLEL(整 n, 整 r)
{
    ALWAYS_ASSERT(abs(n) < 32767);

    C67_asm("||ADDK", n, r, 0);
}

空 C67_Adjust_ADDK(整 *inst, 整 n)
{
    ALWAYS_ASSERT(abs(n) < 32767);

    *inst = (*inst & (~(0xffff << 7))) | ((n & 0xffff) << 7);
}

空 C67_MV(整 r, 整 v)
{
    C67_asm("MV.L", 0, r, v);
}


空 C67_DPTRUNC(整 r, 整 v)
{
    C67_asm("DPTRUNC.L", 0, r, v);
}

空 C67_SPTRUNC(整 r, 整 v)
{
    C67_asm("SPTRUNC.L", 0, r, v);
}

空 C67_INTSP(整 r, 整 v)
{
    C67_asm("INTSP.L", 0, r, v);
}

空 C67_INTDP(整 r, 整 v)
{
    C67_asm("INTDP.L", 0, r, v);
}

空 C67_INTSPU(整 r, 整 v)
{
    C67_asm("INTSPU.L", 0, r, v);
}

空 C67_INTDPU(整 r, 整 v)
{
    C67_asm("INTDPU.L", 0, r, v);
}

空 C67_SPDP(整 r, 整 v)
{
    C67_asm("SPDP.L", 0, r, v);
}

空 C67_DPSP(整 r, 整 v)     // note regs must be on the same side
{
    C67_asm("DPSP.L", 0, r, v);
}

空 C67_ADD(整 r, 整 v)
{
    C67_asm("ADD.L", v, r, v);
}

空 C67_SUB(整 r, 整 v)
{
    C67_asm("SUB.L", v, r, v);
}

空 C67_AND(整 r, 整 v)
{
    C67_asm("AND.L", v, r, v);
}

空 C67_OR(整 r, 整 v)
{
    C67_asm("OR.L", v, r, v);
}

空 C67_XOR(整 r, 整 v)
{
    C67_asm("XOR.L", v, r, v);
}

空 C67_ADDSP(整 r, 整 v)
{
    C67_asm("ADDSP.L", v, r, v);
}

空 C67_SUBSP(整 r, 整 v)
{
    C67_asm("SUBSP.L", v, r, v);
}

空 C67_MPYSP(整 r, 整 v)
{
    C67_asm("MPYSP.M", v, r, v);
}

空 C67_ADDDP(整 r, 整 v)
{
    C67_asm("ADDDP.L", v, r, v);
}

空 C67_SUBDP(整 r, 整 v)
{
    C67_asm("SUBDP.L", v, r, v);
}

空 C67_MPYDP(整 r, 整 v)
{
    C67_asm("MPYDP.M", v, r, v);
}

空 C67_MPYI(整 r, 整 v)
{
    C67_asm("MPYI.M", v, r, v);
}

空 C67_SHL(整 r, 整 v)
{
    C67_asm("SHL.S", r, v, v);
}

空 C67_SHRU(整 r, 整 v)
{
    C67_asm("SHRU.S", r, v, v);
}

空 C67_SHR(整 r, 整 v)
{
    C67_asm("SHR.S", r, v, v);
}



/* load 'r' from value 'sv' */
空 load(整 r, SValue * sv)
{
    整 v, t, ft, fc, fr, size = 0, element;
    BOOL Unsigned = FALSE;
    SValue v1;

    fr = sv->r;
    ft = sv->type.t;
    fc = sv->c.i;

    v = fr & VT_VALMASK;
    如 (fr & VT_LVAL) {
        如 (v == VT_LLOCAL) {
            v1.type.t = VT_INT;
            v1.r = VT_LOCAL | VT_LVAL;
            v1.c.i = fc;
            load(r, &v1);
            fr = r;
        } 另 如 ((ft & VT_BTYPE) == VT_LDOUBLE) {
            tcc_error("long double not supported");
        } 另 如 ((ft & VT_TYPE) == VT_BYTE) {
            size = 1;
        } 另 如 ((ft & VT_TYPE) == (VT_BYTE | VT_UNSIGNED)) {
            size = 1;
            Unsigned = TRUE;
        } 另 如 ((ft & VT_TYPE) == VT_SHORT) {
            size = 2;
        } 另 如 ((ft & VT_TYPE) == (VT_SHORT | VT_UNSIGNED)) {
            size = 2;
            Unsigned = TRUE;
        } 另 如 ((ft & VT_BTYPE) == VT_DOUBLE) {
            size = 8;
        } 另 {
            size = 4;
        }

        // check if fc is a positive reference on the stack, 
        // if it is tcc is referencing what it thinks is a parameter
        // on the stack, so check if it is really in a register.


        如 (v == VT_LOCAL && fc > 0) {
            整 stack_pos = 8;

            对于 (t = 0; t < NoCallArgsPassedOnStack; t++) {
                如 (fc == stack_pos)
                    跳出;

                stack_pos += TranslateStackToReg[t];
            }

            // param has been pushed on stack, get it like a local var

            fc = ParamLocOnStack[t] - 8;
        }

        如 ((fr & VT_VALMASK) < VT_CONST)       // check for pure indirect
        {
            如 (size == 1) {
                如 (Unsigned)
                    C67_LDBU_PTR(v, r); // LDBU  *v,r
                另
                    C67_LDB_PTR(v, r);  // LDB  *v,r
            } 另 如 (size == 2) {
                如 (Unsigned)
                    C67_LDHU_PTR(v, r); // LDHU  *v,r
                另
                    C67_LDH_PTR(v, r);  // LDH  *v,r
            } 另 如 (size == 4) {
                C67_LDW_PTR(v, r);      // LDW  *v,r
            } 另 如 (size == 8) {
                C67_LDDW_PTR(v, r);     // LDDW  *v,r
            }

            C67_NOP(4);         // NOP 4
            返回;
        } 另 如 (fr & VT_SYM) {
            greloc(cur_text_section, sv->sym, ind, R_C60LO16);  // rem the inst need to be patched
            greloc(cur_text_section, sv->sym, ind + 4, R_C60HI16);


            C67_MVKL(C67_A0, fc);       //r=reg to load,  constant
            C67_MVKH(C67_A0, fc);       //r=reg to load,  constant


            如 (size == 1) {
                如 (Unsigned)
                    C67_LDBU_PTR(C67_A0, r);    // LDBU  *A0,r
                另
                    C67_LDB_PTR(C67_A0, r);     // LDB  *A0,r
            } 另 如 (size == 2) {
                如 (Unsigned)
                    C67_LDHU_PTR(C67_A0, r);    // LDHU  *A0,r
                另
                    C67_LDH_PTR(C67_A0, r);     // LDH  *A0,r
            } 另 如 (size == 4) {
                C67_LDW_PTR(C67_A0, r); // LDW  *A0,r
            } 另 如 (size == 8) {
                C67_LDDW_PTR(C67_A0, r);        // LDDW  *A0,r
            }

            C67_NOP(4);         // NOP 4
            返回;
        } 另 {
            element = size;

            // divide offset in bytes to create element index
            C67_MVKL(C67_A0, (fc / element) + 8 / element);     //r=reg to load,  constant
            C67_MVKH(C67_A0, (fc / element) + 8 / element);     //r=reg to load,  constant

            如 (size == 1) {
                如 (Unsigned)
                    C67_LDBU_SP_A0(r);  // LDBU  r, SP[A0]
                另
                    C67_LDB_SP_A0(r);   // LDB  r, SP[A0]
            } 另 如 (size == 2) {
                如 (Unsigned)
                    C67_LDHU_SP_A0(r);  // LDHU  r, SP[A0]
                另
                    C67_LDH_SP_A0(r);   // LDH  r, SP[A0]
            } 另 如 (size == 4) {
                C67_LDW_SP_A0(r);       // LDW  r, SP[A0]
            } 另 如 (size == 8) {
                C67_LDDW_SP_A0(r);      // LDDW  r, SP[A0]
            }


            C67_NOP(4);         // NOP 4
            返回;
        }
    } 另 {
        如 (v == VT_CONST) {
            如 (fr & VT_SYM) {
                greloc(cur_text_section, sv->sym, ind, R_C60LO16);      // rem the inst need to be patched
                greloc(cur_text_section, sv->sym, ind + 4, R_C60HI16);
            }
            C67_MVKL(r, fc);    //r=reg to load, constant
            C67_MVKH(r, fc);    //r=reg to load, constant
        } 另 如 (v == VT_LOCAL) {
            C67_MVKL(r, fc + 8);        //r=reg to load, constant C67 stack points to next free
            C67_MVKH(r, fc + 8);        //r=reg to load, constant
            C67_ADD(C67_FP, r); // MV v,r   v -> r
        } 另 如 (v == VT_CMP) {
            C67_MV(C67_compare_reg, r); // MV v,r   v -> r
        } 另 如 (v == VT_JMP || v == VT_JMPI) {
            t = v & 1;
            C67_B_DISP(4);      //  Branch with constant displacement, skip over this branch, load, nop, load
            C67_MVKL(r, t);     //  r=reg to load, 0 or 1 (do this while branching)
            C67_NOP(4);         //  NOP 4
            gsym(fc);           //  modifies other branches to branch here
            C67_MVKL(r, t ^ 1); //  r=reg to load, 0 or 1
        } 另 如 (v != r) {
            C67_MV(v, r);       // MV v,r   v -> r

            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_MV(v + 1, r + 1);   // MV v,r   v -> r
        }
    }
}


/* store register 'r' in lvalue 'v' */
空 store(整 r, SValue * v)
{
    整 fr, bt, ft, fc, size, t, element;

    ft = v->type.t;
    fc = v->c.i;
    fr = v->r & VT_VALMASK;
    bt = ft & VT_BTYPE;
    /* XXX: incorrect if float reg to reg */

    如 (bt == VT_LDOUBLE) {
        tcc_error("long double not supported");
    } 另 {
        如 (bt == VT_SHORT)
            size = 2;
        另 如 (bt == VT_BYTE)
            size = 1;
        另 如 (bt == VT_DOUBLE)
            size = 8;
        另
            size = 4;

        如 ((v->r & VT_VALMASK) == VT_CONST) {
            /* constant memory reference */

            如 (v->r & VT_SYM) {
                greloc(cur_text_section, v->sym, ind, R_C60LO16);       // rem the inst need to be patched
                greloc(cur_text_section, v->sym, ind + 4, R_C60HI16);
            }
            C67_MVKL(C67_A0, fc);       //r=reg to load,  constant
            C67_MVKH(C67_A0, fc);       //r=reg to load,  constant

            如 (size == 1)
                C67_STB_PTR(r, C67_A0); // STB  r, *A0
            另 如 (size == 2)
                C67_STH_PTR(r, C67_A0); // STH  r, *A0
            另 如 (size == 4 || size == 8)
                C67_STW_PTR(r, C67_A0); // STW  r, *A0

            如 (size == 8)
                C67_STW_PTR_PRE_INC(r + 1, C67_A0, 1);  // STW  r, *+A0[1]
        } 另 如 ((v->r & VT_VALMASK) == VT_LOCAL) {
            // check case of storing to passed argument that
            // tcc thinks is on the stack but for C67 is
            // passed as a reg.  However it may have been
            // saved to the stack, if that reg was required
            // for a call to a child function

            如 (fc > 0)         // argument ??
            {
                // walk through sizes and figure which param

                整 stack_pos = 8;

                对于 (t = 0; t < NoCallArgsPassedOnStack; t++) {
                    如 (fc == stack_pos)
                        跳出;

                    stack_pos += TranslateStackToReg[t];
                }

                // param has been pushed on stack, get it like a local var
                fc = ParamLocOnStack[t] - 8;
            }

            如 (size == 8)
                element = 4;
            另
                element = size;

            // divide offset in bytes to create word index
            C67_MVKL(C67_A0, (fc / element) + 8 / element);     //r=reg to load,  constant
            C67_MVKH(C67_A0, (fc / element) + 8 / element);     //r=reg to load,  constant



            如 (size == 1)
                C67_STB_SP_A0(r);       // STB  r, SP[A0]
            另 如 (size == 2)
                C67_STH_SP_A0(r);       // STH  r, SP[A0]
            另 如 (size == 4 || size == 8)
                C67_STW_SP_A0(r);       // STW  r, SP[A0]

            如 (size == 8) {
                C67_ADDK(1, C67_A0);    //  ADDK 1,A0
                C67_STW_SP_A0(r + 1);   //  STW  r, SP[A0]
            }
        } 另 {
            如 (size == 1)
                C67_STB_PTR(r, fr);     // STB  r, *fr
            另 如 (size == 2)
                C67_STH_PTR(r, fr);     // STH  r, *fr
            另 如 (size == 4 || size == 8)
                C67_STW_PTR(r, fr);     // STW  r, *fr

            如 (size == 8) {
                C67_STW_PTR_PRE_INC(r + 1, fr, 1);      // STW  r, *+fr[1]
            }
        }
    }
}

/* 'is_jmp' is '1' if it is a jump */
静态 空 gcall_or_jmp(整 is_jmp)
{
    整 r;
    Sym *sym;

    如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
        /* constant case */
        如 (vtop->r & VT_SYM) {
            /* relocation case */

            // get add into A0, then start the jump B3

            greloc(cur_text_section, vtop->sym, ind, R_C60LO16);        // rem the inst need to be patched
            greloc(cur_text_section, vtop->sym, ind + 4, R_C60HI16);

            C67_MVKL(C67_A0, 0);        //r=reg to load, constant
            C67_MVKH(C67_A0, 0);        //r=reg to load, constant
            C67_IREG_B_REG(0, C67_CREG_ZERO, C67_A0);   //  B.S2x  A0

            如 (is_jmp) {
                C67_NOP(5);     // simple jump, just put NOP
            } 另 {
                // Call, must load return address into B3 during delay slots

                sym = get_sym_ref(&char_pointer_type, cur_text_section, ind + 12, 0);   // symbol for return address
                greloc(cur_text_section, sym, ind, R_C60LO16);  // rem the inst need to be patched
                greloc(cur_text_section, sym, ind + 4, R_C60HI16);
                C67_MVKL(C67_B3, 0);    //r=reg to load, constant
                C67_MVKH(C67_B3, 0);    //r=reg to load, constant
                C67_NOP(3);     // put remaining NOPs
            }
        } 另 {
            /* put an empty PC32 relocation */
            ALWAYS_ASSERT(FALSE);
        }
    } 另 {
        /* otherwise, indirect call */
        r = gv(RC_INT);
        C67_IREG_B_REG(0, C67_CREG_ZERO, r);    //  B.S2x  r

        如 (is_jmp) {
            C67_NOP(5);         // simple jump, just put NOP
        } 另 {
            // Call, must load return address into B3 during delay slots

            sym = get_sym_ref(&char_pointer_type, cur_text_section, ind + 12, 0);       // symbol for return address
            greloc(cur_text_section, sym, ind, R_C60LO16);      // rem the inst need to be patched
            greloc(cur_text_section, sym, ind + 4, R_C60HI16);
            C67_MVKL(C67_B3, 0);        //r=reg to load, constant
            C67_MVKH(C67_B3, 0);        //r=reg to load, constant
            C67_NOP(3);         // put remaining NOPs
        }
    }
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret, 整 *ret_align, 整 *regsize) {
    *ret_align = 1; // Never have to re-align return values for x86-64
    返回 0;
}

/* generate function call with address in (vtop->t, vtop->c) and free function
   context. Stack entry is popped */
空 gfunc_call(整 nb_args)
{
    整 i, r, size = 0;
    整 args_sizes[NoCallArgsPassedOnStack];

    如 (nb_args > NoCallArgsPassedOnStack) {
        tcc_error("more than 10 function params not currently supported");
        // handle more than 10, put some on the stack
    }

    对于 (i = 0; i < nb_args; i++) {
        如 ((vtop->type.t & VT_BTYPE) == VT_STRUCT) {
            ALWAYS_ASSERT(FALSE);
        } 另 {
            /* simple type (currently always same size) */
            /* XXX: implicit cast ? */


            如 ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
                tcc_error("long long not supported");
            } 另 如 ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
                tcc_error("long double not supported");
            } 另 如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE) {
                size = 8;
            } 另 {
                size = 4;
            }

            // put the parameter into the corresponding reg (pair)

            r = gv(RC_C67_A4 << (2 * i));

            // must put on stack because with 1 pass compiler , no way to tell
            // if an up coming nested call might overwrite these regs

            C67_PUSH(r);

            如 (size == 8) {
                C67_STW_PTR_PRE_INC(r + 1, C67_SP, 3);  // STW  r, *+SP[3] (go back and put the other)
            }
            args_sizes[i] = size;
        }
        vtop--;
    }
    // POP all the params on the stack into registers for the
    // immediate call (in reverse order)

    对于 (i = nb_args - 1; i >= 0; i--) {

        如 (args_sizes[i] == 8)
            C67_POP_DW(TREG_C67_A4 + i * 2);
        另
            C67_POP(TREG_C67_A4 + i * 2);
    }
    gcall_or_jmp(0);
    vtop--;
}


// to be compatible with Code Composer for the C67
// the first 10 parameters must be passed in registers
// (pairs for 64 bits) starting wit; A4:A5, then B4:B5 and
// ending with B12:B13.
//
// When a call is made, if the caller has its parameters
// in regs A4-B13 these must be saved before/as the call 
// parameters are loaded and restored upon return (or if/when needed).

/* generate function prolog of type 't' */
空 gfunc_prolog(CType * func_type)
{
    整 addr, align, size, func_call, i;
    Sym *sym;
    CType *type;

    sym = func_type->ref;
    func_call = sym->f.func_call;
    addr = 8;
    /* if the function returns a structure, then add an
       implicit pointer parameter */
    func_vt = sym->type;
    func_var = (sym->f.func_type == FUNC_ELLIPSIS);
    如 ((func_vt.t & VT_BTYPE) == VT_STRUCT) {
        func_vc = addr;
        addr += 4;
    }

    NoOfCurFuncArgs = 0;

    /* define parameters */
    当 ((sym = sym->next) != NULL) {
        type = &sym->type;
        sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t), addr);
        size = type_size(type, &align);
        size = (size + 3) & ~3;

        // keep track of size of arguments so
        // we can translate where tcc thinks they
        // are on the stack into the appropriate reg

        TranslateStackToReg[NoOfCurFuncArgs] = size;
        NoOfCurFuncArgs++;

#如定义 FUNC_STRUCT_PARAM_AS_PTR
        /* structs are passed as pointer */
        如 ((type->t & VT_BTYPE) == VT_STRUCT) {
            size = 4;
        }
#了如
        addr += size;
    }
    func_ret_sub = 0;
    /* pascal type call ? */
    如 (func_call == FUNC_STDCALL)
        func_ret_sub = addr - 8;

    C67_MV(C67_FP, C67_A0);     //  move FP -> A0
    C67_MV(C67_SP, C67_FP);     //  move SP -> FP

    // place all the args passed in regs onto the stack

    loc = 0;
    对于 (i = 0; i < NoOfCurFuncArgs; i++) {

        ParamLocOnStack[i] = loc;       // remember where the param is
        loc += -8;

        C67_PUSH(TREG_C67_A4 + i * 2);

        如 (TranslateStackToReg[i] == 8) {
            C67_STW_PTR_PRE_INC(TREG_C67_A4 + i * 2 + 1, C67_SP, 3);    // STW  r, *+SP[1] (go back and put the other)
        }
    }

    TotalBytesPushedOnStack = -loc;

    func_sub_sp_offset = ind;   // remember where we put the stack instruction 
    C67_ADDK(0, C67_SP);        //  ADDK.L2 loc,SP  (just put zero temporarily)

    C67_PUSH(C67_A0);
    C67_PUSH(C67_B3);
}

/* generate function epilog */
空 gfunc_epilog(空)
{
    {
        整 local = (-loc + 7) & -8;    // stack must stay aligned to 8 bytes for LDDW instr
        C67_POP(C67_B3);
        C67_NOP(4);             // NOP wait for load
        C67_IREG_B_REG(0, C67_CREG_ZERO, C67_B3);       //  B.S2  B3
        C67_POP(C67_FP);
        C67_ADDK(local, C67_SP);        //  ADDK.L2 loc,SP  
        C67_Adjust_ADDK((整 *) (cur_text_section->data +
                                 func_sub_sp_offset),
                        -local + TotalBytesPushedOnStack);
        C67_NOP(3);             // NOP 
    }
}

/* generate a jump to a label */
整 gjmp(整 t)
{
    整 ind1 = ind;
    如 (nocode_wanted)
        返回 t;

    C67_MVKL(C67_A0, t);        //r=reg to load,  constant
    C67_MVKH(C67_A0, t);        //r=reg to load,  constant
    C67_IREG_B_REG(0, C67_CREG_ZERO, C67_A0);   // [!R] B.S2x  A0
    C67_NOP(5);
    返回 ind1;
}

/* generate a jump to a fixed address */
空 gjmp_addr(整 a)
{
    Sym *sym;
    // I guess this routine is used for relative short
    // local jumps, for now just handle it as the general
    // case

    // define a label that will be relocated

    sym = get_sym_ref(&char_pointer_type, cur_text_section, a, 0);
    greloc(cur_text_section, sym, ind, R_C60LO16);
    greloc(cur_text_section, sym, ind + 4, R_C60HI16);

    gjmp(0);                    // place a zero there later the symbol will be added to it
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
整 gtst(整 inv, 整 t)
{
    整 ind1, n;
    整 v, *p;

    v = vtop->r & VT_VALMASK;
    如 (nocode_wanted) {
        ;
    } 另 如 (v == VT_CMP) {
        /* fast case : can jump directly since flags are set */
        // C67 uses B2 sort of as flags register
        ind1 = ind;
        C67_MVKL(C67_A0, t);    //r=reg to load, constant
        C67_MVKH(C67_A0, t);    //r=reg to load, constant

        如 (C67_compare_reg != TREG_EAX &&      // check if not already in a conditional test reg
            C67_compare_reg != TREG_EDX &&
            C67_compare_reg != TREG_ST0 && C67_compare_reg != C67_B2) {
            C67_MV(C67_compare_reg, C67_B2);
            C67_compare_reg = C67_B2;
        }

        C67_IREG_B_REG(C67_invert_test ^ inv, C67_compare_reg, C67_A0); // [!R] B.S2x  A0
        C67_NOP(5);
        t = ind1;               //return where we need to patch

    } 另 如 (v == VT_JMP || v == VT_JMPI) {
        /* && or || optimization */
        如 ((v & 1) == inv) {
            /* insert vtop->c jump list in t */

            // I guess the idea is to traverse to the
            // null at the end of the list and store t
            // there

            n = vtop->c.i;
            当 (n != 0) {
                p = (整 *) (cur_text_section->data + n);

                // extract 32 bit address from MVKH/MVKL
                n = ((*p >> 7) & 0xffff);
                n |= ((*(p + 1) >> 7) & 0xffff) << 16;
            }
            *p |= (t & 0xffff) << 7;
            *(p + 1) |= ((t >> 16) & 0xffff) << 7;
            t = vtop->c.i;

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
    整 r, fr, opc, t;

    转接 (op) {
    事例 '+':
    事例 TOK_ADDC1:               /* add with carry generation */
        opc = 0;
      gen_op8:


// C67 can't do const compares, must load into a reg
// so just go to gv2 directly - tktk



        如 (op >= TOK_ULT && op <= TOK_GT)
            gv2(RC_INT_BSIDE, RC_INT);  // make sure r (src1) is on the B Side of CPU
        另
            gv2(RC_INT, RC_INT);

        r = vtop[-1].r;
        fr = vtop[0].r;

        C67_compare_reg = C67_B2;


        如 (op == TOK_LT) {
            C67_CMPLT(r, fr, C67_B2);
            C67_invert_test = FALSE;
        } 另 如 (op == TOK_GE) {
            C67_CMPLT(r, fr, C67_B2);
            C67_invert_test = TRUE;
        } 另 如 (op == TOK_GT) {
            C67_CMPGT(r, fr, C67_B2);
            C67_invert_test = FALSE;
        } 另 如 (op == TOK_LE) {
            C67_CMPGT(r, fr, C67_B2);
            C67_invert_test = TRUE;
        } 另 如 (op == TOK_EQ) {
            C67_CMPEQ(r, fr, C67_B2);
            C67_invert_test = FALSE;
        } 另 如 (op == TOK_NE) {
            C67_CMPEQ(r, fr, C67_B2);
            C67_invert_test = TRUE;
        } 另 如 (op == TOK_ULT) {
            C67_CMPLTU(r, fr, C67_B2);
            C67_invert_test = FALSE;
        } 另 如 (op == TOK_UGE) {
            C67_CMPLTU(r, fr, C67_B2);
            C67_invert_test = TRUE;
        } 另 如 (op == TOK_UGT) {
            C67_CMPGTU(r, fr, C67_B2);
            C67_invert_test = FALSE;
        } 另 如 (op == TOK_ULE) {
            C67_CMPGTU(r, fr, C67_B2);
            C67_invert_test = TRUE;
        } 另 如 (op == '+')
            C67_ADD(fr, r);     // ADD  r,fr,r
        另 如 (op == '-')
            C67_SUB(fr, r);     // SUB  r,fr,r
        另 如 (op == '&')
            C67_AND(fr, r);     // AND  r,fr,r
        另 如 (op == '|')
            C67_OR(fr, r);      // OR  r,fr,r
        另 如 (op == '^')
            C67_XOR(fr, r);     // XOR  r,fr,r
        另
            ALWAYS_ASSERT(FALSE);

        vtop--;
        如 (op >= TOK_ULT && op <= TOK_GT) {
            vtop->r = VT_CMP;
            vtop->c.i = op;
        }
        跳出;
    事例 '-':
    事例 TOK_SUBC1:               /* sub with carry generation */
        opc = 5;
        跳转 gen_op8;
    事例 TOK_ADDC2:               /* add with carry use */
        opc = 2;
        跳转 gen_op8;
    事例 TOK_SUBC2:               /* sub with carry use */
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
    事例 TOK_UMULL:
        gv2(RC_INT, RC_INT);
        r = vtop[-1].r;
        fr = vtop[0].r;
        vtop--;
        C67_MPYI(fr, r);        // 32 bit multiply  fr,r,fr
        C67_NOP(8);             // NOP 8 for worst case
        跳出;
    事例 TOK_SHL:
        gv2(RC_INT_BSIDE, RC_INT_BSIDE);        // shift amount must be on same side as dst
        r = vtop[-1].r;
        fr = vtop[0].r;
        vtop--;
        C67_SHL(fr, r);         // arithmetic/logical shift
        跳出;

    事例 TOK_SHR:
        gv2(RC_INT_BSIDE, RC_INT_BSIDE);        // shift amount must be on same side as dst
        r = vtop[-1].r;
        fr = vtop[0].r;
        vtop--;
        C67_SHRU(fr, r);        // logical shift
        跳出;

    事例 TOK_SAR:
        gv2(RC_INT_BSIDE, RC_INT_BSIDE);        // shift amount must be on same side as dst
        r = vtop[-1].r;
        fr = vtop[0].r;
        vtop--;
        C67_SHR(fr, r);         // arithmetic shift
        跳出;

    事例 '/':
        t = TOK__divi;
      call_func:
        vswap();
        /* call generic idiv function */
        vpush_global_sym(&func_old_type, t);
        vrott(3);
        gfunc_call(2);
        vpushi(0);
        vtop->r = REG_IRET;
        vtop->r2 = VT_CONST;
        跳出;
    事例 TOK_UDIV:
    事例 TOK_PDIV:
        t = TOK__divu;
        跳转 call_func;
    事例 '%':
        t = TOK__remi;
        跳转 call_func;
    事例 TOK_UMOD:
        t = TOK__remu;
        跳转 call_func;

    缺省:
        opc = 7;
        跳转 gen_op8;
    }
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranteed to have the same floating point type */
/* XXX: need to use ST1 too */
空 gen_opf(整 op)
{
    整 ft, fc, fr, r;

    如 (op >= TOK_ULT && op <= TOK_GT)
        gv2(RC_EDX, RC_EAX);    // make sure src2 is on b side
    另
        gv2(RC_FLOAT, RC_FLOAT);        // make sure src2 is on b side

    ft = vtop->type.t;
    fc = vtop->c.i;
    r = vtop->r;
    fr = vtop[-1].r;


    如 ((ft & VT_BTYPE) == VT_LDOUBLE)
        tcc_error("long doubles not supported");

    如 (op >= TOK_ULT && op <= TOK_GT) {

        r = vtop[-1].r;
        fr = vtop[0].r;

        C67_compare_reg = C67_B2;

        如 (op == TOK_LT) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPLTDP(r, fr, C67_B2);
            另
                C67_CMPLTSP(r, fr, C67_B2);

            C67_invert_test = FALSE;
        } 另 如 (op == TOK_GE) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPLTDP(r, fr, C67_B2);
            另
                C67_CMPLTSP(r, fr, C67_B2);

            C67_invert_test = TRUE;
        } 另 如 (op == TOK_GT) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPGTDP(r, fr, C67_B2);
            另
                C67_CMPGTSP(r, fr, C67_B2);

            C67_invert_test = FALSE;
        } 另 如 (op == TOK_LE) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPGTDP(r, fr, C67_B2);
            另
                C67_CMPGTSP(r, fr, C67_B2);

            C67_invert_test = TRUE;
        } 另 如 (op == TOK_EQ) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPEQDP(r, fr, C67_B2);
            另
                C67_CMPEQSP(r, fr, C67_B2);

            C67_invert_test = FALSE;
        } 另 如 (op == TOK_NE) {
            如 ((ft & VT_BTYPE) == VT_DOUBLE)
                C67_CMPEQDP(r, fr, C67_B2);
            另
                C67_CMPEQSP(r, fr, C67_B2);

            C67_invert_test = TRUE;
        } 另 {
            ALWAYS_ASSERT(FALSE);
        }
        vtop->r = VT_CMP;       // tell TCC that result is in "flags" actually B2
    } 另 {
        如 (op == '+') {
            如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                C67_ADDDP(r, fr);       // ADD  fr,r,fr
                C67_NOP(6);
            } 另 {
                C67_ADDSP(r, fr);       // ADD  fr,r,fr
                C67_NOP(3);
            }
            vtop--;
        } 另 如 (op == '-') {
            如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                C67_SUBDP(r, fr);       // SUB  fr,r,fr
                C67_NOP(6);
            } 另 {
                C67_SUBSP(r, fr);       // SUB  fr,r,fr
                C67_NOP(3);
            }
            vtop--;
        } 另 如 (op == '*') {
            如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                C67_MPYDP(r, fr);       // MPY  fr,r,fr
                C67_NOP(9);
            } 另 {
                C67_MPYSP(r, fr);       // MPY  fr,r,fr
                C67_NOP(3);
            }
            vtop--;
        } 另 如 (op == '/') {
            如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                // must call intrinsic DP floating point divide
                vswap();
                /* call generic idiv function */
                vpush_global_sym(&func_old_type, TOK__divd);
                vrott(3);
                gfunc_call(2);
                vpushi(0);
                vtop->r = REG_FRET;
                vtop->r2 = REG_LRET;

            } 另 {
                // must call intrinsic SP floating point divide
                vswap();
                /* call generic idiv function */
                vpush_global_sym(&func_old_type, TOK__divf);
                vrott(3);
                gfunc_call(2);
                vpushi(0);
                vtop->r = REG_FRET;
                vtop->r2 = VT_CONST;
            }
        } 另
            ALWAYS_ASSERT(FALSE);


    }
}


/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
空 gen_cvt_itof(整 t)
{
    整 r;

    gv(RC_INT);
    r = vtop->r;

    如 ((t & VT_BTYPE) == VT_DOUBLE) {
        如 (t & VT_UNSIGNED)
            C67_INTDPU(r, r);
        另
            C67_INTDP(r, r);

        C67_NOP(4);
        vtop->type.t = VT_DOUBLE;
    } 另 {
        如 (t & VT_UNSIGNED)
            C67_INTSPU(r, r);
        另
            C67_INTSP(r, r);
        C67_NOP(3);
        vtop->type.t = VT_FLOAT;
    }

}

/* convert fp to int 't' type */
/* XXX: handle long long case */
空 gen_cvt_ftoi(整 t)
{
    整 r;

    gv(RC_FLOAT);
    r = vtop->r;

    如 (t != VT_INT)
        tcc_error("long long not supported");
    另 {
        如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE) {
            C67_DPTRUNC(r, r);
            C67_NOP(3);
        } 另 {
            C67_SPTRUNC(r, r);
            C67_NOP(3);
        }

        vtop->type.t = VT_INT;

    }
}

/* convert from one floating point type to another */
空 gen_cvt_ftof(整 t)
{
    整 r, r2;

    如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE &&
        (t & VT_BTYPE) == VT_FLOAT) {
        // convert double to float

        gv(RC_FLOAT);           // get it in a register pair

        r = vtop->r;

        C67_DPSP(r, r);         // convert it to SP same register
        C67_NOP(3);

        vtop->type.t = VT_FLOAT;
        vtop->r2 = VT_CONST;    // set this as unused
    } 另 如 ((vtop->type.t & VT_BTYPE) == VT_FLOAT &&
               (t & VT_BTYPE) == VT_DOUBLE) {
        // convert float to double

        gv(RC_FLOAT);           // get it in a register

        r = vtop->r;

        如 (r == TREG_EAX) {    // make sure the paired reg is avail
            r2 = get_reg(RC_ECX);
        } 另 如 (r == TREG_EDX) {
            r2 = get_reg(RC_ST0);
        } 另 {
            ALWAYS_ASSERT(FALSE);
            r2 = 0; /* avoid warning */
        }

        C67_SPDP(r, r);         // convert it to DP same register
        C67_NOP(1);

        vtop->type.t = VT_DOUBLE;
        vtop->r2 = r2;          // set this as unused
    } 另 {
        ALWAYS_ASSERT(FALSE);
    }
}

/* computed goto support */
空 ggoto(空)
{
    gcall_or_jmp(1);
    vtop--;
}

/* Save the stack pointer onto the stack and return the location of its address */
ST_FUNC 空 gen_vla_sp_save(整 addr) {
    tcc_error("variable length arrays unsupported for this target");
}

/* Restore the SP from a location on the stack */
ST_FUNC 空 gen_vla_sp_restore(整 addr) {
    tcc_error("variable length arrays unsupported for this target");
}

/* Subtract from the stack pointer, and push the resulting value onto the stack */
ST_FUNC 空 gen_vla_alloc(CType *type, 整 align) {
    tcc_error("variable length arrays unsupported for this target");
}

/* end of C67 code generator */
/*************************************************************/
#了如
/*************************************************************/
