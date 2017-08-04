/*
 *  CIL code generator for TCC
 * 
 *  Copyright (c) 2002 Fabrice Bellard
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

#错误 this code has bit-rotted since 2003

/* number of available registers */
#定义 NB_REGS             3

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#定义 RC_ST      0x0001  /* any stack entry */
#定义 RC_ST0     0x0002  /* top of stack */
#定义 RC_ST1     0x0004  /* top - 1 */

#定义 RC_INT     RC_ST
#定义 RC_FLOAT   RC_ST
#定义 RC_IRET    RC_ST0 /* function return: integer register */
#定义 RC_LRET    RC_ST0 /* function return: second integer register */
#定义 RC_FRET    RC_ST0 /* function return: float register */

/* pretty names for the registers */
枚举 {
    REG_ST0 = 0,
    REG_ST1,
    REG_ST2,
};

不变 整 reg_classes[NB_REGS] = {
    /* ST0 */ RC_ST | RC_ST0,
    /* ST1 */ RC_ST | RC_ST1,
    /* ST2 */ RC_ST,
};

/* return registers for function */
#定义 REG_IRET REG_ST0 /* single word int return register */
#定义 REG_LRET REG_ST0 /* second word return register (for long long) */
#定义 REG_FRET REG_ST0 /* float return register */

/* defined if function parameters must be evaluated in reverse order */
/* #定义 INVERT_FUNC_PARAMS */

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #定义 FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#定义 PTR_SIZE 4

/* long double size and alignment, in bytes */
#定义 LDOUBLE_SIZE  8
#定义 LDOUBLE_ALIGN 8

/* function call context */
类型定义 结构 GFuncContext {
    整 func_call; /* func call type (FUNC_STDCALL or FUNC_CDECL) */
} GFuncContext;

/******************************************************/
/* opcode definitions */

#定义 IL_OP_PREFIX 0xFE

枚举 ILOPCodes {
#定义 OP(name, str, n) IL_OP_ ## name = n,
#包含 "il-opcodes.h"
#消定义 OP
};

字 *il_opcodes_str[] = {
#定义 OP(name, str, n) [n] = str,
#包含 "il-opcodes.h"
#消定义 OP
};

/******************************************************/

/* arguments variable numbers start from there */
#定义 ARG_BASE 0x70000000

静态 FILE *il_outfile;

静态 空 out_byte(整 c)
{
    *(字 *)ind++ = c;
}

静态 空 out_le32(整 c)
{
    out_byte(c);
    out_byte(c >> 8);
    out_byte(c >> 16);
    out_byte(c >> 24);
}

静态 空 init_outfile(空)
{
    如 (!il_outfile) {
        il_outfile = stdout;
        fprintf(il_outfile, 
                ".assembly extern mscorlib\n"
                "{\n"
                ".ver 1:0:2411:0\n"
                "}\n\n");
    }
}

静态 空 out_op1(整 op)
{
    如 (op & 0x100)
        out_byte(IL_OP_PREFIX);
    out_byte(op & 0xff);
}

/* output an opcode with prefix */
静态 空 out_op(整 op)
{
    out_op1(op);
    fprintf(il_outfile, " %s\n", il_opcodes_str[op]);
}

静态 空 out_opb(整 op, 整 c)
{
    out_op1(op);
    out_byte(c);
    fprintf(il_outfile, " %s %d\n", il_opcodes_str[op], c);
}

静态 空 out_opi(整 op, 整 c)
{
    out_op1(op);
    out_le32(c);
    fprintf(il_outfile, " %s 0x%x\n", il_opcodes_str[op], c);
}

/* XXX: not complete */
静态 空 il_type_to_str(字 *buf, 整 buf_size, 
                           整 t, 不变 字 *varstr)
{
    整 bt;
    Sym *s, *sa;
    字 buf1[256];
    不变 字 *tstr;

    t = t & VT_TYPE;
    bt = t & VT_BTYPE;
    buf[0] = '\0';
    如 (t & VT_UNSIGNED)
        pstrcat(buf, buf_size, "unsigned ");
    转接(bt) {
    事例 VT_VOID:
        tstr = "void";
        跳转 add_tstr;
    事例 VT_BOOL:
        tstr = "bool";
        跳转 add_tstr;
    事例 VT_BYTE:
        tstr = "int8";
        跳转 add_tstr;
    事例 VT_SHORT:
        tstr = "int16";
        跳转 add_tstr;
    事例 VT_ENUM:
    事例 VT_INT:
    事例 VT_LONG:
        tstr = "int32";
        跳转 add_tstr;
    事例 VT_LLONG:
        tstr = "int64";
        跳转 add_tstr;
    事例 VT_FLOAT:
        tstr = "float32";
        跳转 add_tstr;
    事例 VT_DOUBLE:
    事例 VT_LDOUBLE:
        tstr = "float64";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        跳出;
    事例 VT_STRUCT:
        tcc_error("structures not handled yet");
        跳出;
    事例 VT_FUNC:
        s = sym_find((无符)t >> VT_STRUCT_SHIFT);
        il_type_to_str(buf, buf_size, s->t, varstr);
        pstrcat(buf, buf_size, "(");
        sa = s->next;
        当 (sa != NULL) {
            il_type_to_str(buf1, 求长度(buf1), sa->t, NULL);
            pstrcat(buf, buf_size, buf1);
            sa = sa->next;
            如 (sa)
                pstrcat(buf, buf_size, ", ");
        }
        pstrcat(buf, buf_size, ")");
        跳转 no_var;
    事例 VT_PTR:
        s = sym_find((无符)t >> VT_STRUCT_SHIFT);
        pstrcpy(buf1, 求长度(buf1), "*");
        如 (varstr)
            pstrcat(buf1, 求长度(buf1), varstr);
        il_type_to_str(buf, buf_size, s->t, buf1);
        跳转 no_var;
    }
    如 (varstr) {
        pstrcat(buf, buf_size, " ");
        pstrcat(buf, buf_size, varstr);
    }
 no_var: ;
}


/* patch relocation entry with value 'val' */
空 greloc_patch1(Reloc *p, 整 val)
{
}

/* output a symbol and patch all calls to it */
空 gsym_addr(t, a)
{
}

/* output jump and return symbol */
静态 整 out_opj(整 op, 整 c)
{
    out_op1(op);
    out_le32(0);
    如 (c == 0) {
        c = ind - (整)cur_text_section->data;
    }
    fprintf(il_outfile, " %s L%d\n", il_opcodes_str[op], c);
    返回 c;
}

空 gsym(整 t)
{
    fprintf(il_outfile, "L%d:\n", t);
}

/* load 'r' from value 'sv' */
空 load(整 r, SValue *sv)
{
    整 v, fc, ft;

    v = sv->r & VT_VALMASK;
    fc = sv->c.i;
    ft = sv->t;

    如 (sv->r & VT_LVAL) {
        如 (v == VT_LOCAL) {
            如 (fc >= ARG_BASE) {
                fc -= ARG_BASE;
                如 (fc >= 0 && fc <= 4) {
                    out_op(IL_OP_LDARG_0 + fc);
                } 另 如 (fc <= 0xff) {
                    out_opb(IL_OP_LDARG_S, fc);
                } 另 {
                    out_opi(IL_OP_LDARG, fc);
                }
            } 另 {
                如 (fc >= 0 && fc <= 4) {
                    out_op(IL_OP_LDLOC_0 + fc);
                } 另 如 (fc <= 0xff) {
                    out_opb(IL_OP_LDLOC_S, fc);
                } 另 {
                    out_opi(IL_OP_LDLOC, fc);
                }
            }
        } 另 如 (v == VT_CONST) {
                /* XXX: handle globals */
                out_opi(IL_OP_LDSFLD, 0);
        } 另 {
            如 ((ft & VT_BTYPE) == VT_FLOAT) {
                out_op(IL_OP_LDIND_R4);
            } 另 如 ((ft & VT_BTYPE) == VT_DOUBLE) {
                out_op(IL_OP_LDIND_R8);
            } 另 如 ((ft & VT_BTYPE) == VT_LDOUBLE) {
                out_op(IL_OP_LDIND_R8);
            } 另 如 ((ft & VT_TYPE) == VT_BYTE)
                out_op(IL_OP_LDIND_I1);
            另 如 ((ft & VT_TYPE) == (VT_BYTE | VT_UNSIGNED))
                out_op(IL_OP_LDIND_U1);
            另 如 ((ft & VT_TYPE) == VT_SHORT)
                out_op(IL_OP_LDIND_I2);
            另 如 ((ft & VT_TYPE) == (VT_SHORT | VT_UNSIGNED))
                out_op(IL_OP_LDIND_U2);
            另
                out_op(IL_OP_LDIND_I4);
        } 
    } 另 {
        如 (v == VT_CONST) {
            /* XXX: handle globals */
            如 (fc >= -1 && fc <= 8) {
                out_op(IL_OP_LDC_I4_M1 + fc + 1); 
            } 另 {
                out_opi(IL_OP_LDC_I4, fc);
            }
        } 另 如 (v == VT_LOCAL) {
            如 (fc >= ARG_BASE) {
                fc -= ARG_BASE;
                如 (fc <= 0xff) {
                    out_opb(IL_OP_LDARGA_S, fc);
                } 另 {
                    out_opi(IL_OP_LDARGA, fc);
                }
            } 另 {
                如 (fc <= 0xff) {
                    out_opb(IL_OP_LDLOCA_S, fc);
                } 另 {
                    out_opi(IL_OP_LDLOCA, fc);
                }
            }
        } 另 {
            /* XXX: do it */
        }
    }
}

/* store register 'r' in lvalue 'v' */
空 store(整 r, SValue *sv)
{
    整 v, fc, ft;

    v = sv->r & VT_VALMASK;
    fc = sv->c.i;
    ft = sv->t;
    如 (v == VT_LOCAL) {
        如 (fc >= ARG_BASE) {
            fc -= ARG_BASE;
            /* XXX: check IL arg store semantics */
            如 (fc <= 0xff) {
                out_opb(IL_OP_STARG_S, fc);
            } 另 {
                out_opi(IL_OP_STARG, fc);
            }
        } 另 {
            如 (fc >= 0 && fc <= 4) {
                out_op(IL_OP_STLOC_0 + fc);
            } 另 如 (fc <= 0xff) {
                out_opb(IL_OP_STLOC_S, fc);
            } 另 {
                out_opi(IL_OP_STLOC, fc);
            }
        }
    } 另 如 (v == VT_CONST) {
        /* XXX: handle globals */
        out_opi(IL_OP_STSFLD, 0);
    } 另 {
        如 ((ft & VT_BTYPE) == VT_FLOAT)
            out_op(IL_OP_STIND_R4);
        另 如 ((ft & VT_BTYPE) == VT_DOUBLE)
            out_op(IL_OP_STIND_R8);
        另 如 ((ft & VT_BTYPE) == VT_LDOUBLE)
            out_op(IL_OP_STIND_R8);
        另 如 ((ft & VT_BTYPE) == VT_BYTE)
            out_op(IL_OP_STIND_I1);
        另 如 ((ft & VT_BTYPE) == VT_SHORT)
            out_op(IL_OP_STIND_I2);
        另
            out_op(IL_OP_STIND_I4);
    }
}

/* start function call and return function call context */
空 gfunc_start(GFuncContext *c, 整 func_call)
{
    c->func_call = func_call;
}

/* push function parameter which is in (vtop->t, vtop->c). Stack entry
   is then popped. */
空 gfunc_param(GFuncContext *c)
{
    如 ((vtop->t & VT_BTYPE) == VT_STRUCT) {
        tcc_error("structures passed as value not handled yet");
    } 另 {
        /* simply push on stack */
        gv(RC_ST0);
    }
    vtop--;
}

/* generate function call with address in (vtop->t, vtop->c) and free function
   context. Stack entry is popped */
空 gfunc_call(GFuncContext *c)
{
    字 buf[1024];

    如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
        /* XXX: more info needed from tcc */
        il_type_to_str(buf, 求长度(buf), vtop->t, "xxx");
        fprintf(il_outfile, " call %s\n", buf);
    } 另 {
        /* indirect call */
        gv(RC_INT);
        il_type_to_str(buf, 求长度(buf), vtop->t, NULL);
        fprintf(il_outfile, " calli %s\n", buf);
    }
    vtop--;
}

/* generate function prolog of type 't' */
空 gfunc_prolog(整 t)
{
    整 addr, u, func_call;
    Sym *sym;
    字 buf[1024];

    init_outfile();

    /* XXX: pass function name to gfunc_prolog */
    il_type_to_str(buf, 求长度(buf), t, funcname);
    fprintf(il_outfile, ".method static %s il managed\n", buf);
    fprintf(il_outfile, "{\n");
    /* XXX: cannot do better now */
    fprintf(il_outfile, " .maxstack %d\n", NB_REGS);
    fprintf(il_outfile, " .locals (int32, int32, int32, int32, int32, int32, int32, int32)\n");
    
    如 (!strcmp(funcname, "main"))
        fprintf(il_outfile, " .entrypoint\n");
        
    sym = sym_find((无符)t >> VT_STRUCT_SHIFT);
    func_call = sym->r;

    addr = ARG_BASE;
    /* if the function returns a structure, then add an
       implicit pointer parameter */
    func_vt = sym->t;
    func_var = (sym->c == FUNC_ELLIPSIS);
    如 ((func_vt & VT_BTYPE) == VT_STRUCT) {
        func_vc = addr;
        addr++;
    }
    /* define parameters */
    当 ((sym = sym->next) != NULL) {
        u = sym->t;
        sym_push(sym->v & ~SYM_FIELD, u,
                 VT_LOCAL | lvalue_type(sym->type.t), addr);
        addr++;
    }
}

/* generate function epilog */
空 gfunc_epilog(空)
{
    out_op(IL_OP_RET);
    fprintf(il_outfile, "}\n\n");
}

/* generate a jump to a label */
整 gjmp(整 t)
{
    返回 out_opj(IL_OP_BR, t);
}

/* generate a jump to a fixed address */
空 gjmp_addr(整 a)
{
    /* XXX: handle syms */
    out_opi(IL_OP_BR, a);
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
整 gtst(整 inv, 整 t)
{
    整 v, *p, c;

    v = vtop->r & VT_VALMASK;
    如 (v == VT_CMP) {
        c = vtop->c.i ^ inv;
        转接(c) {
        事例 TOK_EQ:
            c = IL_OP_BEQ;
            跳出;
        事例 TOK_NE:
            c = IL_OP_BNE_UN;
            跳出;
        事例 TOK_LT:
            c = IL_OP_BLT;
            跳出;
        事例 TOK_LE:
            c = IL_OP_BLE;
            跳出;
        事例 TOK_GT:
            c = IL_OP_BGT;
            跳出;
        事例 TOK_GE:
            c = IL_OP_BGE;
            跳出;
        事例 TOK_ULT:
            c = IL_OP_BLT_UN;
            跳出;
        事例 TOK_ULE:
            c = IL_OP_BLE_UN;
            跳出;
        事例 TOK_UGT:
            c = IL_OP_BGT_UN;
            跳出;
        事例 TOK_UGE:
            c = IL_OP_BGE_UN;
            跳出;
        }
        t = out_opj(c, t);
    } 另 如 (v == VT_JMP || v == VT_JMPI) {
        /* && or || optimization */
        如 ((v & 1) == inv) {
            /* insert vtop->c jump list in t */
            p = &vtop->c.i;
            当 (*p != 0)
                p = (整 *)*p;
            *p = t;
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
    gv2(RC_ST1, RC_ST0);
    转接(op) {
    事例 '+':
        out_op(IL_OP_ADD);
        跳转 std_op;
    事例 '-':
        out_op(IL_OP_SUB);
        跳转 std_op;
    事例 '&':
        out_op(IL_OP_AND);
        跳转 std_op;
    事例 '^':
        out_op(IL_OP_XOR);
        跳转 std_op;
    事例 '|':
        out_op(IL_OP_OR);
        跳转 std_op;
    事例 '*':
        out_op(IL_OP_MUL);
        跳转 std_op;
    事例 TOK_SHL:
        out_op(IL_OP_SHL);
        跳转 std_op;
    事例 TOK_SHR:
        out_op(IL_OP_SHR_UN);
        跳转 std_op;
    事例 TOK_SAR:
        out_op(IL_OP_SHR);
        跳转 std_op;
    事例 '/':
    事例 TOK_PDIV:
        out_op(IL_OP_DIV);
        跳转 std_op;
    事例 TOK_UDIV:
        out_op(IL_OP_DIV_UN);
        跳转 std_op;
    事例 '%':
        out_op(IL_OP_REM);
        跳转 std_op;
    事例 TOK_UMOD:
        out_op(IL_OP_REM_UN);
    std_op:
        vtop--;
        vtop[0].r = REG_ST0;
        跳出;
    事例 TOK_EQ:
    事例 TOK_NE:
    事例 TOK_LT:
    事例 TOK_LE:
    事例 TOK_GT:
    事例 TOK_GE:
    事例 TOK_ULT:
    事例 TOK_ULE:
    事例 TOK_UGT:
    事例 TOK_UGE:
        vtop--;
        vtop[0].r = VT_CMP;
        vtop[0].c.i = op;
        跳出;
    }
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranteed to have the same floating point type */
空 gen_opf(整 op)
{
    /* same as integer */
    gen_opi(op);
}

/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
空 gen_cvt_itof(整 t)
{
    gv(RC_ST0);
    如 (t == VT_FLOAT)
        out_op(IL_OP_CONV_R4);
    另
        out_op(IL_OP_CONV_R8);
}

/* convert fp to int 't' type */
/* XXX: handle long long case */
空 gen_cvt_ftoi(整 t)
{
    gv(RC_ST0);
    转接(t) {
    事例 VT_INT | VT_UNSIGNED:
        out_op(IL_OP_CONV_U4);
        跳出;
    事例 VT_LLONG:
        out_op(IL_OP_CONV_I8);
        跳出;
    事例 VT_LLONG | VT_UNSIGNED:
        out_op(IL_OP_CONV_U8);
        跳出;
    缺省:
        out_op(IL_OP_CONV_I4);
        跳出;
    }
}

/* convert from one floating point type to another */
空 gen_cvt_ftof(整 t)
{
    gv(RC_ST0);
    如 (t == VT_FLOAT) {
        out_op(IL_OP_CONV_R4);
    } 另 {
        out_op(IL_OP_CONV_R8);
    }
}

/* end of CIL code generator */
/*************************************************************/

