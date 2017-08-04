/*
 *  TCC - Tiny C Compiler
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
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

/********************************************************/
/* global variables */

/* loc : local variable index
   ind : output code index
   rsym: return symbol
   anon_sym: anonymous symbol index
*/
ST_DATA 整 rsym, anon_sym, ind, loc;

ST_DATA Sym *sym_free_first;
ST_DATA 空 **sym_pools;
ST_DATA 整 nb_sym_pools;

ST_DATA Sym *global_stack;
ST_DATA Sym *local_stack;
ST_DATA Sym *define_stack;
ST_DATA Sym *global_label_stack;
ST_DATA Sym *local_label_stack;
静态 整 local_scope;
静态 整 in_sizeof;
静态 整 section_sym;

ST_DATA 整 vlas_in_scope; /* number of VLAs that are currently in scope */
ST_DATA 整 vla_sp_root_loc; /* vla_sp_loc for SP before any VLAs were pushed */
ST_DATA 整 vla_sp_loc; /* Pointer to variable holding location to store stack pointer on the stack when modifying stack pointer */

ST_DATA SValue __vstack[1+VSTACK_SIZE], *vtop, *pvtop;

ST_DATA 整 const_wanted; /* true if constant wanted */
ST_DATA 整 nocode_wanted; /* no code generation wanted */
#定义 NODATA_WANTED (nocode_wanted > 0) /* no static data output wanted either */
#定义 STATIC_DATA_WANTED (nocode_wanted & 0xC0000000) /* only static data output */
ST_DATA 整 global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
ST_DATA CType func_vt; /* current function return type (used by return instruction) */
ST_DATA 整 func_var; /* true if current function is variadic (used by return instruction) */
ST_DATA 整 func_vc;
ST_DATA 整 last_line_num, last_ind, func_ind; /* debug last line number and pc */
ST_DATA 不变 字 *funcname;
ST_DATA 整 g_debug;

ST_DATA CType char_pointer_type, func_old_type, int_type, size_type;

ST_DATA 结构 switch_t {
        结构 case_t {
                int64_t v1, v2;
                整 sym;
        } **p; 整 n; /* list of case ranges */
        整 def_sym; /* default symbol */
} *cur_switch; /* current switch */

/* ------------------------------------------------------------------------- */

静态 空 gen_cast(CType *type);
静态 空 gen_cast_s(整 t);
静态 内联 CType *pointed_type(CType *type);
静态 整 is_compatible_types(CType *type1, CType *type2);
静态 整 parse_btype(CType *type, AttributeDef *ad);
静态 CType *type_decl(CType *type, AttributeDef *ad, 整 *v, 整 td);
静态 空 parse_expr_type(CType *type);
静态 空 init_putv(CType *type, Section *sec, 无符 长 c);
静态 空 decl_initializer(CType *type, Section *sec, 无符 长 c, 整 first, 整 size_only);
静态 空 block(整 *bsym, 整 *csym, 整 is_expr);
静态 空 decl_initializer_alloc(CType *type, AttributeDef *ad, 整 r, 整 has_init, 整 v, 整 scope);
静态 空 decl(整 l);
静态 整 decl0(整 l, 整 is_for_loop_init, Sym *);
静态 空 expr_eq(空);
静态 空 vla_runtime_type_size(CType *type, 整 *a);
静态 空 vla_sp_restore(空);
静态 空 vla_sp_restore_root(空);
静态 整 is_compatible_unqualified_types(CType *type1, CType *type2);
静态 内联 int64_t expr_const64(空);
静态 空 vpush64(整 ty, 无符 长 长 v);
静态 空 vpush(CType *type);
静态 整 gvtst(整 inv, 整 t);
静态 空 gen_inline_functions(TCCState *s);
静态 空 skip_or_save_block(TokenString **str);
静态 空 gv_dup(空);

ST_INLN 整 is_float(整 t)
{
        整 bt;
        bt = t & VT_BTYPE;
        返回 bt == VT_LDOUBLE || bt == VT_DOUBLE || bt == VT_FLOAT || bt == VT_QFLOAT;
}

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
ST_FUNC 整 ieee_finite(双精 d)
{
        整 p[4];
        memcpy(p, &d, 求长度(双精));
        返回 ((无符)((p[1] | 0x800fffff) + 1)) >> 31;
}

ST_FUNC 空 test_lvalue(空)
{
        如 (!(vtop->r & VT_LVAL))
                expect("lvalue");
}

ST_FUNC 空 check_vstack(空)
{
        如 (pvtop != vtop)
                tcc_error("internal compiler error: vstack leak (%d)", vtop - pvtop);
}

/* ------------------------------------------------------------------------- */
/* vstack debugging aid */

#如 0
空 pv (不变 字 *lbl, 整 a, 整 b)
{
        整 i;
        对于 (i = a; i < a + b; ++i) {
                SValue *p = &vtop[-i];
                printf("%s vtop[-%d] : type.t:%04x  r:%04x  r2:%04x  c.i:%d\n",
                        lbl, i, p->type.t, p->r, p->r2, (整)p->c.i);
        }
}
#了如

/* ------------------------------------------------------------------------- */
/* start of translation unit info */
ST_FUNC 空 tcc_debug_start(TCCState *s1)
{
        如 (s1->do_debug) {
                字 buf[512];

                /* file info: full path + filename */
                section_sym = put_elf_sym(symtab_section, 0, 0,
                                                                  ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                                                                  text_section->sh_num, NULL);
                getcwd(buf, 求长度(buf));
#如定义 _WIN32
                normalize_slashes(buf);
#了如
                pstrcat(buf, 求长度(buf), "/");
                put_stabs_r(buf, N_SO, 0, 0,
                                        text_section->data_offset, text_section, section_sym);
                put_stabs_r(file->filename, N_SO, 0, 0,
                                        text_section->data_offset, text_section, section_sym);
                last_ind = 0;
                last_line_num = 0;
        }

        /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
           symbols can be safely used */
        put_elf_sym(symtab_section, 0, 0,
                                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0,
                                SHN_ABS, file->filename);
}

/* put end of translation unit info */
ST_FUNC 空 tcc_debug_end(TCCState *s1)
{
        如 (!s1->do_debug)
                返回;
        put_stabs_r(NULL, N_SO, 0, 0,
                text_section->data_offset, text_section, section_sym);

}

/* generate line number info */
ST_FUNC 空 tcc_debug_line(TCCState *s1)
{
        如 (!s1->do_debug)
                返回;
        如 ((last_line_num != file->line_num || last_ind != ind)) {
                put_stabn(N_SLINE, 0, file->line_num, ind - func_ind);
                last_ind = ind;
                last_line_num = file->line_num;
        }
}

/* put function symbol */
ST_FUNC 空 tcc_debug_funcstart(TCCState *s1, Sym *sym)
{
        字 buf[512];

        如 (!s1->do_debug)
                返回;

        /* stabs info */
        /* XXX: we put here a dummy type */
        snprintf(buf, 求长度(buf), "%s:%c1",
                         funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
        put_stabs_r(buf, N_FUN, 0, file->line_num, 0,
                                cur_text_section, sym->c);
        /* //gr gdb wants a line at the function */
        put_stabn(N_SLINE, 0, file->line_num, 0);

        last_ind = 0;
        last_line_num = 0;
}

/* put function size */
ST_FUNC 空 tcc_debug_funcend(TCCState *s1, 整 size)
{
        如 (!s1->do_debug)
                返回;
        put_stabn(N_FUN, 0, 0, size);
}

/* ------------------------------------------------------------------------- */
ST_FUNC 整 tccgen_compile(TCCState *s1)
{
        cur_text_section = NULL;
        funcname = "";
        anon_sym = SYM_FIRST_ANOM;
        section_sym = 0;
        const_wanted = 0;
        nocode_wanted = 0x80000000;

        /* define some often used types */
        int_type.t = VT_INT;
        char_pointer_type.t = VT_BYTE;
        mk_pointer(&char_pointer_type);
#如 PTR_SIZE == 4
        size_type.t = VT_INT;
#另
        size_type.t = VT_LLONG;
#了如
        func_old_type.t = VT_FUNC;
        func_old_type.ref = sym_push(SYM_FIELD, &int_type, 0, 0);
        func_old_type.ref->f.func_call = FUNC_CDECL;
        func_old_type.ref->f.func_type = FUNC_OLD;

        tcc_debug_start(s1);

#如定义 TCC_TARGET_ARM
        arm_init(s1);
#了如

#如定义 INC_DEBUG
        printf("%s: **** new file\n", file->filename);
#了如

        parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | PARSE_FLAG_TOK_STR;
        next();
        decl(VT_CONST);
        如 (tok != TOK_EOF)
                expect("declaration");

        gen_inline_functions(s1);
        check_vstack();
        /* end of translation unit info */
        tcc_debug_end(s1);
        返回 0;
}

/* ------------------------------------------------------------------------- */
/* apply storage attributes to Elf symbol */

静态 空 update_storage(Sym *sym)
{
        ElfW(Sym) *esym;
        如 (0 == sym->c)
                返回;
        esym = &((ElfW(Sym) *)symtab_section->data)[sym->c];
        如 (sym->a.visibility)
                esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
                        | sym->a.visibility;
        如 (sym->a.weak)
                esym->st_info = ELFW(ST_INFO)(STB_WEAK, ELFW(ST_TYPE)(esym->st_info));
#如定义 TCC_TARGET_PE
        如 (sym->a.dllimport)
                esym->st_other |= ST_PE_IMPORT;
        如 (sym->a.dllexport)
                esym->st_other |= ST_PE_EXPORT;
#了如
#如 0
        printf("storage %s: vis=%d weak=%d exp=%d imp=%d\n",
                get_tok_str(sym->v, NULL),
                sym->a.visibility,
                sym->a.weak,
                sym->a.dllexport,
                sym->a.dllimport
                );
#了如
}

/* ------------------------------------------------------------------------- */
/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */

ST_FUNC 空 put_extern_sym2(Sym *sym, Section *section,
                                                        addr_t value, 无符 长 size,
                                                        整 can_add_underscore)
{
        整 sym_type, sym_bind, sh_num, info, other, t;
        ElfW(Sym) *esym;
        不变 字 *name;
        字 buf1[256];
#如定义 CONFIG_TCC_BCHECK
        字 buf[32];
#了如

        如 (section == NULL)
                sh_num = SHN_UNDEF;
        另 如 (section == SECTION_ABS)
                sh_num = SHN_ABS;
        另
                sh_num = section->sh_num;

        如 (!sym->c) {
                name = get_tok_str(sym->v, NULL);
#如定义 CONFIG_TCC_BCHECK
                如 (tcc_state->do_bounds_check) {
                        /* XXX: avoid doing that for statics ? */
                        /* if bound checking is activated, we change some function
                           names by adding the "__bound" prefix */
                        转接(sym->v) {
#如定义 TCC_TARGET_PE
                        /* XXX: we rely only on malloc hooks */
                        事例 TOK_malloc:
                        事例 TOK_free:
                        事例 TOK_realloc:
                        事例 TOK_memalign:
                        事例 TOK_calloc:
#了如
                        事例 TOK_memcpy:
                        事例 TOK_memmove:
                        事例 TOK_memset:
                        事例 TOK_strlen:
                        事例 TOK_strcpy:
                        事例 TOK_alloca:
                                strcpy(buf, "__bound_");
                                strcat(buf, name);
                                name = buf;
                                跳出;
                        }
                }
#了如
                t = sym->type.t;
                如 ((t & VT_BTYPE) == VT_FUNC) {
                        sym_type = STT_FUNC;
                } 另 如 ((t & VT_BTYPE) == VT_VOID) {
                        sym_type = STT_NOTYPE;
                } 另 {
                        sym_type = STT_OBJECT;
                }
                如 (t & VT_STATIC)
                        sym_bind = STB_LOCAL;
                另
                        sym_bind = STB_GLOBAL;
                other = 0;
#如定义 TCC_TARGET_PE
                如 (sym_type == STT_FUNC && sym->type.ref) {
                        Sym *ref = sym->type.ref;
                        如 (ref->f.func_call == FUNC_STDCALL && can_add_underscore) {
                                sprintf(buf1, "_%s@%d", name, ref->f.func_args * PTR_SIZE);
                                name = buf1;
                                other |= ST_PE_STDCALL;
                                can_add_underscore = 0;
                        }
                }
#了如
                如 (tcc_state->leading_underscore && can_add_underscore) {
                        buf1[0] = '_';
                        pstrcpy(buf1 + 1, 求长度(buf1) - 1, name);
                        name = buf1;
                }
                如 (sym->asm_label)
                        name = get_tok_str(sym->asm_label, NULL);
                info = ELFW(ST_INFO)(sym_bind, sym_type);
                sym->c = set_elf_sym(symtab_section, value, size, info, other, sh_num, name);
        } 另 {
                esym = &((ElfW(Sym) *)symtab_section->data)[sym->c];
                esym->st_value = value;
                esym->st_size = size;
                esym->st_shndx = sh_num;
        }
        update_storage(sym);
}

ST_FUNC 空 put_extern_sym(Sym *sym, Section *section,
                                                   addr_t value, 无符 长 size)
{
        put_extern_sym2(sym, section, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
ST_FUNC 空 greloca(Section *s, Sym *sym, 无符 长 offset, 整 type,
                                         addr_t addend)
{
        整 c = 0;

        如 (nocode_wanted && s == cur_text_section)
                返回;

        如 (sym) {
                如 (0 == sym->c)
                        put_extern_sym(sym, NULL, 0, 0);
                c = sym->c;
        }

        /* now we can add ELF relocation info */
        put_elf_reloca(symtab_section, s, offset, type, c, addend);
}

#如 PTR_SIZE == 4
ST_FUNC 空 greloc(Section *s, Sym *sym, 无符 长 offset, 整 type)
{
        greloca(s, sym, offset, type, 0);
}
#了如

/* ------------------------------------------------------------------------- */
/* symbol allocator */
静态 Sym *__sym_malloc(空)
{
        Sym *sym_pool, *sym, *last_sym;
        整 i;

        sym_pool = tcc_malloc(SYM_POOL_NB * 求长度(Sym));
        dynarray_add(&sym_pools, &nb_sym_pools, sym_pool);

        last_sym = sym_free_first;
        sym = sym_pool;
        对于(i = 0; i < SYM_POOL_NB; i++) {
                sym->next = last_sym;
                last_sym = sym;
                sym++;
        }
        sym_free_first = last_sym;
        返回 last_sym;
}

静态 内联 Sym *sym_malloc(空)
{
        Sym *sym;
#如未定义 SYM_DEBUG
        sym = sym_free_first;
        如 (!sym)
                sym = __sym_malloc();
        sym_free_first = sym->next;
        返回 sym;
#另
        sym = tcc_malloc(求长度(Sym));
        返回 sym;
#了如
}

ST_INLN 空 sym_free(Sym *sym)
{
#如未定义 SYM_DEBUG
        sym->next = sym_free_first;
        sym_free_first = sym;
#另
        tcc_free(sym);
#了如
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(Sym **ps, 整 v, 整 t, 整 c)
{
        Sym *s;

        s = sym_malloc();
        memset(s, 0, 求长度 *s);
        s->v = v;
        s->type.t = t;
        s->c = c;
        /* add in stack */
        s->prev = *ps;
        *ps = s;
        返回 s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
ST_FUNC Sym *sym_find2(Sym *s, 整 v)
{
        当 (s) {
                如 (s->v == v)
                        返回 s;
                另 如 (s->v == -1)
                        返回 NULL;
                s = s->prev;
        }
        返回 NULL;
}

/* structure lookup */
ST_INLN Sym *struct_find(整 v)
{
        v -= TOK_IDENT;
        如 ((无符)v >= (无符)(tok_ident - TOK_IDENT))
                返回 NULL;
        返回 table_ident[v]->sym_struct;
}

/* find an identifier */
ST_INLN Sym *sym_find(整 v)
{
        v -= TOK_IDENT;
        如 ((无符)v >= (无符)(tok_ident - TOK_IDENT))
                返回 NULL;
        返回 table_ident[v]->sym_identifier;
}

/* push a given symbol on the symbol stack */
ST_FUNC Sym *sym_push(整 v, CType *type, 整 r, 整 c)
{
        Sym *s, **ps;
        TokenSym *ts;

        如 (local_stack)
                ps = &local_stack;
        另
                ps = &global_stack;
        s = sym_push2(ps, v, type->t, c);
        s->type.ref = type->ref;
        s->r = r;
        /* don't record fields or anonymous symbols */
        /* XXX: simplify */
        如 (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
                /* record symbol in token array */
                ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
                如 (v & SYM_STRUCT)
                        ps = &ts->sym_struct;
                另
                        ps = &ts->sym_identifier;
                s->prev_tok = *ps;
                *ps = s;
                s->sym_scope = local_scope;
                如 (s->prev_tok && s->prev_tok->sym_scope == s->sym_scope)
                        tcc_error("redeclaration of '%s'",
                                get_tok_str(v & ~SYM_STRUCT, NULL));
        }
        返回 s;
}

/* push a global identifier */
ST_FUNC Sym *global_identifier_push(整 v, 整 t, 整 c)
{
        Sym *s, **ps;
        s = sym_push2(&global_stack, v, t, c);
        /* don't record anonymous symbol */
        如 (v < SYM_FIRST_ANOM) {
                ps = &table_ident[v - TOK_IDENT]->sym_identifier;
                /* modify the top most local identifier, so that
                   sym_identifier will point to 's' when popped */
                当 (*ps != NULL)
                        ps = &(*ps)->prev_tok;
                s->prev_tok = NULL;
                *ps = s;
        }
        返回 s;
}

/* pop symbols until top reaches 'b'.  If KEEP is non-zero don't really
   pop them yet from the list, but do remove them from the token array.  */
ST_FUNC 空 sym_pop(Sym **ptop, Sym *b, 整 keep)
{
        Sym *s, *ss, **ps;
        TokenSym *ts;
        整 v;

        s = *ptop;
        当(s != b) {
                ss = s->prev;
                v = s->v;
                /* remove symbol in token array */
                /* XXX: simplify */
                如 (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
                        ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
                        如 (v & SYM_STRUCT)
                                ps = &ts->sym_struct;
                        另
                                ps = &ts->sym_identifier;
                        *ps = s->prev_tok;
                }
                如 (!keep)
                        sym_free(s);
                s = ss;
        }
        如 (!keep)
                *ptop = b;
}

/* ------------------------------------------------------------------------- */

静态 空 vsetc(CType *type, 整 r, CValue *vc)
{
        整 v;

        如 (vtop >= vstack + (VSTACK_SIZE - 1))
                tcc_error("memory full (vstack)");
        /* cannot let cpu flags if other instruction are generated. Also
           avoid leaving VT_JMP anywhere except on the top of the stack
           because it would complicate the code generator.

           Don't do this when nocode_wanted.  vtop might come from
           !nocode_wanted regions (see 88_codeopt.c) and transforming
           it to a register without actually generating code is wrong
           as their value might still be used for real.  All values
           we push under nocode_wanted will eventually be popped
           again, so that the VT_CMP/VT_JMP value will be in vtop
           when code is unsuppressed again.

           Same logic below in vswap(); */
        如 (vtop >= vstack && !nocode_wanted) {
                v = vtop->r & VT_VALMASK;
                如 (v == VT_CMP || (v & ~1) == VT_JMP)
                        gv(RC_INT);
        }

        vtop++;
        vtop->type = *type;
        vtop->r = r;
        vtop->r2 = VT_CONST;
        vtop->c = *vc;
        vtop->sym = NULL;
}

ST_FUNC 空 vswap(空)
{
        SValue tmp;
        /* cannot vswap cpu flags. See comment at vsetc() above */
        如 (vtop >= vstack && !nocode_wanted) {
                整 v = vtop->r & VT_VALMASK;
                如 (v == VT_CMP || (v & ~1) == VT_JMP)
                        gv(RC_INT);
        }
        tmp = vtop[0];
        vtop[0] = vtop[-1];
        vtop[-1] = tmp;
}

/* pop stack value */
ST_FUNC 空 vpop(空)
{
        整 v;
        v = vtop->r & VT_VALMASK;
#如 已定义(TCC_TARGET_I386) || 已定义(TCC_TARGET_X86_64)
        /* for x86, we need to pop the FP stack */
        如 (v == TREG_ST0) {
                o(0xd8dd); /* fstp %st(0) */
        } 另
#了如
        如 (v == VT_JMP || v == VT_JMPI) {
                /* need to put correct jump if && or || without test */
                gsym(vtop->c.i);
        }
        vtop--;
}

/* push constant of type "type" with useless value */
ST_FUNC 空 vpush(CType *type)
{
        vset(type, VT_CONST, 0);
}

/* push integer constant */
ST_FUNC 空 vpushi(整 v)
{
        CValue cval;
        cval.i = v;
        vsetc(&int_type, VT_CONST, &cval);
}

/* push a pointer sized constant */
静态 空 vpushs(addr_t v)
{
  CValue cval;
  cval.i = v;
  vsetc(&size_type, VT_CONST, &cval);
}

/* push arbitrary 64bit constant */
ST_FUNC 空 vpush64(整 ty, 无符 长 长 v)
{
        CValue cval;
        CType ctype;
        ctype.t = ty;
        ctype.ref = NULL;
        cval.i = v;
        vsetc(&ctype, VT_CONST, &cval);
}

/* push long long constant */
静态 内联 空 vpushll(长 长 v)
{
        vpush64(VT_LLONG, v);
}

ST_FUNC 空 vset(CType *type, 整 r, 整 v)
{
        CValue cval;

        cval.i = v;
        vsetc(type, r, &cval);
}

静态 空 vseti(整 r, 整 v)
{
        CType type;
        type.t = VT_INT;
        type.ref = NULL;
        vset(&type, r, v);
}

ST_FUNC 空 vpushv(SValue *v)
{
        如 (vtop >= vstack + (VSTACK_SIZE - 1))
                tcc_error("memory full (vstack)");
        vtop++;
        *vtop = *v;
}

静态 空 vdup(空)
{
        vpushv(vtop);
}

/* rotate n first stack elements to the bottom
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC 空 vrotb(整 n)
{
        整 i;
        SValue tmp;

        tmp = vtop[-n + 1];
        对于(i=-n+1;i!=0;i++)
                vtop[i] = vtop[i+1];
        vtop[0] = tmp;
}

/* rotate the n elements before entry e towards the top
   I1 ... In ... -> In I1 ... I(n-1) ... [top is right]
 */
ST_FUNC 空 vrote(SValue *e, 整 n)
{
        整 i;
        SValue tmp;

        tmp = *e;
        对于(i = 0;i < n - 1; i++)
                e[-i] = e[-i - 1];
        e[-n + 1] = tmp;
}

/* rotate n first stack elements to the top
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
ST_FUNC 空 vrott(整 n)
{
        vrote(vtop, n);
}

/* push a symbol value of TYPE */
静态 内联 空 vpushsym(CType *type, Sym *sym)
{
        CValue cval;
        cval.i = 0;
        vsetc(type, VT_CONST | VT_SYM, &cval);
        vtop->sym = sym;
}

/* Return a static symbol pointing to a section */
ST_FUNC Sym *get_sym_ref(CType *type, Section *sec, 无符 长 offset, 无符 长 size)
{
        整 v;
        Sym *sym;

        v = anon_sym++;
        sym = global_identifier_push(v, type->t | VT_STATIC, 0);
        sym->type.ref = type->ref;
        sym->r = VT_CONST | VT_SYM;
        put_extern_sym(sym, sec, offset, size);
        返回 sym;
}

/* push a reference to a section offset by adding a dummy symbol */
静态 空 vpush_ref(CType *type, Section *sec, 无符 长 offset, 无符 长 size)
{
        vpushsym(type, get_sym_ref(type, sec, offset, size));
}

/* define a new external reference to a symbol 'v' of type 'u' */
ST_FUNC Sym *external_global_sym(整 v, CType *type, 整 r)
{
        Sym *s;

        s = sym_find(v);
        如 (!s) {
                /* push forward reference */
                s = global_identifier_push(v, type->t | VT_EXTERN, 0);
                s->type.ref = type->ref;
                s->r = r | VT_CONST | VT_SYM;
        }
        返回 s;
}

/* Merge some storage attributes.  */
静态 空 patch_storage(Sym *sym, AttributeDef *ad, CType *type)
{
        如 (type && !is_compatible_types(&sym->type, type))
                tcc_error("incompatible types for redefinition of '%s'",
                        get_tok_str(sym->v, NULL));
#如定义 TCC_TARGET_PE
        如 (sym->a.dllimport != ad->a.dllimport)
                tcc_error("incompatible dll linkage for redefinition of '%s'",
                        get_tok_str(sym->v, NULL));
#了如
        sym->a.dllexport |= ad->a.dllexport;
        sym->a.weak |= ad->a.weak;
        如 (ad->a.visibility) {
                整 vis = sym->a.visibility;
                整 vis2 = ad->a.visibility;
                如 (vis == STV_DEFAULT)
                        vis = vis2;
                另 如 (vis2 != STV_DEFAULT)
                        vis = (vis < vis2) ? vis : vis2;
                sym->a.visibility = vis;
        }
        如 (ad->a.aligned)
                sym->a.aligned = ad->a.aligned;
        如 (ad->asm_label)
                sym->asm_label = ad->asm_label;
        update_storage(sym);
}

/* define a new external reference to a symbol 'v' */
静态 Sym *external_sym(整 v, CType *type, 整 r, AttributeDef *ad)
{
        Sym *s;
        s = sym_find(v);
        如 (!s) {
                /* push forward reference */
                s = sym_push(v, type, r | VT_CONST | VT_SYM, 0);
                s->type.t |= VT_EXTERN;
                s->a = ad->a;
                s->sym_scope = 0;
        } 另 {
                如 (s->type.ref == func_old_type.ref) {
                        s->type.ref = type->ref;
                        s->r = r | VT_CONST | VT_SYM;
                        s->type.t |= VT_EXTERN;
                }
                patch_storage(s, ad, type);
        }
        返回 s;
}

/* push a reference to global symbol v */
ST_FUNC 空 vpush_global_sym(CType *type, 整 v)
{
        vpushsym(type, external_global_sym(v, type, 0));
}

/* save registers up to (vtop - n) stack entry */
ST_FUNC 空 save_regs(整 n)
{
        SValue *p, *p1;
        对于(p = vstack, p1 = vtop - n; p <= p1; p++)
                save_reg(p->r);
}

/* save r to the memory stack, and mark it as being free */
ST_FUNC 空 save_reg(整 r)
{
        save_reg_upstack(r, 0);
}

/* save r to the memory stack, and mark it as being free,
   if seen up to (vtop - n) stack entry */
ST_FUNC 空 save_reg_upstack(整 r, 整 n)
{
        整 l, saved, size, align;
        SValue *p, *p1, sv;
        CType *type;

        如 ((r &= VT_VALMASK) >= VT_CONST)
                返回;
        如 (nocode_wanted)
                返回;

        /* modify all stack values */
        saved = 0;
        l = 0;
        对于(p = vstack, p1 = vtop - n; p <= p1; p++) {
                如 ((p->r & VT_VALMASK) == r ||
                        ((p->type.t & VT_BTYPE) == VT_LLONG && (p->r2 & VT_VALMASK) == r)) {
                        /* must save value on stack if not already done */
                        如 (!saved) {
                                /* NOTE: must reload 'r' because r might be equal to r2 */
                                r = p->r & VT_VALMASK;
                                /* store register in the stack */
                                type = &p->type;
                                如 ((p->r & VT_LVAL) ||
                                        (!is_float(type->t) && (type->t & VT_BTYPE) != VT_LLONG))
#如 PTR_SIZE == 8
                                        type = &char_pointer_type;
#另
                                        type = &int_type;
#了如
                                size = type_size(type, &align);
                                loc = (loc - size) & -align;
                                sv.type.t = type->t;
                                sv.r = VT_LOCAL | VT_LVAL;
                                sv.c.i = loc;
                                store(r, &sv);
#如 已定义(TCC_TARGET_I386) || 已定义(TCC_TARGET_X86_64)
                                /* x86 specific: need to pop fp register ST0 if saved */
                                如 (r == TREG_ST0) {
                                        o(0xd8dd); /* fstp %st(0) */
                                }
#了如
#如 PTR_SIZE == 4
                                /* special long long case */
                                如 ((type->t & VT_BTYPE) == VT_LLONG) {
                                        sv.c.i += 4;
                                        store(p->r2, &sv);
                                }
#了如
                                l = loc;
                                saved = 1;
                        }
                        /* mark that stack entry as being saved on the stack */
                        如 (p->r & VT_LVAL) {
                                /* also clear the bounded flag because the
                                   relocation address of the function was stored in
                                   p->c.i */
                                p->r = (p->r & ~(VT_VALMASK | VT_BOUNDED)) | VT_LLOCAL;
                        } 另 {
                                p->r = lvalue_type(p->type.t) | VT_LOCAL;
                        }
                        p->r2 = VT_CONST;
                        p->c.i = l;
                }
        }
}

#如定义 TCC_TARGET_ARM
/* find a register of class 'rc2' with at most one reference on stack.
 * If none, call get_reg(rc) */
ST_FUNC 整 get_reg_ex(整 rc, 整 rc2)
{
        整 r;
        SValue *p;

        对于(r=0;r<NB_REGS;r++) {
                如 (reg_classes[r] & rc2) {
                        整 n;
                        n=0;
                        对于(p = vstack; p <= vtop; p++) {
                                如 ((p->r & VT_VALMASK) == r ||
                                        (p->r2 & VT_VALMASK) == r)
                                        n++;
                        }
                        如 (n <= 1)
                                返回 r;
                }
        }
        返回 get_reg(rc);
}
#了如

/* find a free register of class 'rc'. If none, save one register */
ST_FUNC 整 get_reg(整 rc)
{
        整 r;
        SValue *p;

        /* find a free register */
        对于(r=0;r<NB_REGS;r++) {
                如 (reg_classes[r] & rc) {
                        如 (nocode_wanted)
                                返回 r;
                        对于(p=vstack;p<=vtop;p++) {
                                如 ((p->r & VT_VALMASK) == r ||
                                        (p->r2 & VT_VALMASK) == r)
                                        跳转 notfound;
                        }
                        返回 r;
                }
        notfound: ;
        }

        /* no register left : free the first one on the stack (VERY
           IMPORTANT to start from the bottom to ensure that we don't
           spill registers used in gen_opi()) */
        对于(p=vstack;p<=vtop;p++) {
                /* look at second register (if long long) */
                r = p->r2 & VT_VALMASK;
                如 (r < VT_CONST && (reg_classes[r] & rc))
                        跳转 save_found;
                r = p->r & VT_VALMASK;
                如 (r < VT_CONST && (reg_classes[r] & rc)) {
                save_found:
                        save_reg(r);
                        返回 r;
                }
        }
        /* Should never comes here */
        返回 -1;
}

/* move register 's' (of type 't') to 'r', and flush previous value of r to memory
   if needed */
静态 空 move_reg(整 r, 整 s, 整 t)
{
        SValue sv;

        如 (r != s) {
                save_reg(r);
                sv.type.t = t;
                sv.type.ref = NULL;
                sv.r = s;
                sv.c.i = 0;
                load(r, &sv);
        }
}

/* get address of vtop (vtop MUST BE an lvalue) */
ST_FUNC 空 gaddrof(空)
{
        vtop->r &= ~VT_LVAL;
        /* tricky: if saved lvalue, then we can go back to lvalue */
        如 ((vtop->r & VT_VALMASK) == VT_LLOCAL)
                vtop->r = (vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;


}

#如定义 CONFIG_TCC_BCHECK
/* generate lvalue bound code */
静态 空 gbound(空)
{
        整 lval_type;
        CType type1;

        vtop->r &= ~VT_MUSTBOUND;
        /* if lvalue, then use checking code before dereferencing */
        如 (vtop->r & VT_LVAL) {
                /* if not VT_BOUNDED value, then make one */
                如 (!(vtop->r & VT_BOUNDED)) {
                        lval_type = vtop->r & (VT_LVAL_TYPE | VT_LVAL);
                        /* must save type because we must set it to int to get pointer */
                        type1 = vtop->type;
                        vtop->type.t = VT_PTR;
                        gaddrof();
                        vpushi(0);
                        gen_bounded_ptr_add();
                        vtop->r |= lval_type;
                        vtop->type = type1;
                }
                /* then check for dereferencing */
                gen_bounded_ptr_deref();
        }
}
#了如

静态 空 incr_bf_adr(整 o)
{
        vtop->type = char_pointer_type;
        gaddrof();
        vpushi(o);
        gen_op('+');
        vtop->type.t = (vtop->type.t & ~(VT_BTYPE|VT_DEFSIGN))
                | (VT_BYTE|VT_UNSIGNED);
        vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                | (VT_LVAL_BYTE|VT_LVAL_UNSIGNED|VT_LVAL);
}

/* single-byte load mode for packed or otherwise unaligned bitfields */
静态 空 load_packed_bf(CType *type, 整 bit_pos, 整 bit_size)
{
        整 n, o, bits;
        save_reg_upstack(vtop->r, 1);
        vpush64(type->t & VT_BTYPE, 0); // B X
        bits = 0, o = bit_pos >> 3, bit_pos &= 7;
        运行 {
                vswap(); // X B
                incr_bf_adr(o);
                vdup(); // X B B
                n = 8 - bit_pos;
                如 (n > bit_size)
                        n = bit_size;
                如 (bit_pos)
                        vpushi(bit_pos), gen_op(TOK_SHR), bit_pos = 0; // X B Y
                如 (n < 8)
                        vpushi((1 << n) - 1), gen_op('&');
                gen_cast(type);
                如 (bits)
                        vpushi(bits), gen_op(TOK_SHL);
                vrotb(3); // B Y X
                gen_op('|'); // B X
                bits += n, bit_size -= n, o = 1;
        } 当 (bit_size);
        vswap(), vpop();
        如 (!(type->t & VT_UNSIGNED)) {
                n = ((type->t & VT_BTYPE) == VT_LLONG ? 64 : 32) - bits;
                vpushi(n), gen_op(TOK_SHL);
                vpushi(n), gen_op(TOK_SAR);
        }
}

/* single-byte store mode for packed or otherwise unaligned bitfields */
静态 空 store_packed_bf(整 bit_pos, 整 bit_size)
{
        整 bits, n, o, m, c;

        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        vswap(); // X B
        save_reg_upstack(vtop->r, 1);
        bits = 0, o = bit_pos >> 3, bit_pos &= 7;
        运行 {
                incr_bf_adr(o); // X B
                vswap(); //B X
                c ? vdup() : gv_dup(); // B V X
                vrott(3); // X B V
                如 (bits)
                        vpushi(bits), gen_op(TOK_SHR);
                如 (bit_pos)
                        vpushi(bit_pos), gen_op(TOK_SHL);
                n = 8 - bit_pos;
                如 (n > bit_size)
                        n = bit_size;
                如 (n < 8) {
                        m = ((1 << n) - 1) << bit_pos;
                        vpushi(m), gen_op('&'); // X B V1
                        vpushv(vtop-1); // X B V1 B
                        vpushi(m & 0x80 ? ~m & 0x7f : ~m);
                        gen_op('&'); // X B V1 B1
                        gen_op('|'); // X B V2
                }
                vdup(), vtop[-1] = vtop[-2]; // X B B V2
                vstore(), vpop(); // X B
                bits += n, bit_size -= n, bit_pos = 0, o = 1;
        } 当 (bit_size);
        vpop(), vpop();
}

静态 整 adjust_bf(SValue *sv, 整 bit_pos, 整 bit_size)
{
        整 t;
        如 (0 == sv->type.ref)
                返回 0;
        t = sv->type.ref->auxtype;
        如 (t != -1 && t != VT_STRUCT) {
                sv->type.t = (sv->type.t & ~VT_BTYPE) | t;
                sv->r = (sv->r & ~VT_LVAL_TYPE) | lvalue_type(sv->type.t);
        }
        返回 t;
}

/* store vtop a register belonging to class 'rc'. lvalues are
   converted to values. Cannot be used if cannot be converted to
   register value (such as structures). */
ST_FUNC 整 gv(整 rc)
{
        整 r, bit_pos, bit_size, size, align, rc2;

        /* NOTE: get_reg can modify vstack[] */
        如 (vtop->type.t & VT_BITFIELD) {
                CType type;

                bit_pos = BIT_POS(vtop->type.t);
                bit_size = BIT_SIZE(vtop->type.t);
                /* remove bit field info to avoid loops */
                vtop->type.t &= ~VT_STRUCT_MASK;

                type.ref = NULL;
                type.t = vtop->type.t & VT_UNSIGNED;
                如 ((vtop->type.t & VT_BTYPE) == VT_BOOL)
                        type.t |= VT_UNSIGNED;

                r = adjust_bf(vtop, bit_pos, bit_size);

                如 ((vtop->type.t & VT_BTYPE) == VT_LLONG)
                        type.t |= VT_LLONG;
                另
                        type.t |= VT_INT;

                如 (r == VT_STRUCT) {
                        load_packed_bf(&type, bit_pos, bit_size);
                } 另 {
                        整 bits = (type.t & VT_BTYPE) == VT_LLONG ? 64 : 32;
                        /* cast to int to propagate signedness in following ops */
                        gen_cast(&type);
                        /* generate shifts */
                        vpushi(bits - (bit_pos + bit_size));
                        gen_op(TOK_SHL);
                        vpushi(bits - bit_size);
                        /* NOTE: transformed to SHR if unsigned */
                        gen_op(TOK_SAR);
                }
                r = gv(rc);
        } 另 {
                如 (is_float(vtop->type.t) &&
                        (vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                        无符 长 offset;
                        /* CPUs usually cannot use float constants, so we store them
                           generically in data segment */
                        size = type_size(&vtop->type, &align);
                        如 (NODATA_WANTED)
                                size = 0, align = 1;
                        offset = section_add(data_section, size, align);
                        vpush_ref(&vtop->type, data_section, offset, size);
                        vswap();
                        init_putv(&vtop->type, data_section, offset);
                        vtop->r |= VT_LVAL;
                }
#如定义 CONFIG_TCC_BCHECK
                如 (vtop->r & VT_MUSTBOUND)
                        gbound();
#了如

                r = vtop->r & VT_VALMASK;
                rc2 = (rc & RC_FLOAT) ? RC_FLOAT : RC_INT;
#如未定义 TCC_TARGET_ARM64
                如 (rc == RC_IRET)
                        rc2 = RC_LRET;
#如定义 TCC_TARGET_X86_64
                另 如 (rc == RC_FRET)
                        rc2 = RC_QRET;
#了如
#了如
                /* need to reload if:
                   - constant
                   - lvalue (need to dereference pointer)
                   - already a register, but not in the right class */
                如 (r >= VT_CONST
                 || (vtop->r & VT_LVAL)
                 || !(reg_classes[r] & rc)
#如 PTR_SIZE == 8
                 || ((vtop->type.t & VT_BTYPE) == VT_QLONG && !(reg_classes[vtop->r2] & rc2))
                 || ((vtop->type.t & VT_BTYPE) == VT_QFLOAT && !(reg_classes[vtop->r2] & rc2))
#另
                 || ((vtop->type.t & VT_BTYPE) == VT_LLONG && !(reg_classes[vtop->r2] & rc2))
#了如
                        )
                {
                        r = get_reg(rc);
#如 PTR_SIZE == 8
                        如 (((vtop->type.t & VT_BTYPE) == VT_QLONG) || ((vtop->type.t & VT_BTYPE) == VT_QFLOAT)) {
                                整 addr_type = VT_LLONG, load_size = 8, load_type = ((vtop->type.t & VT_BTYPE) == VT_QLONG) ? VT_LLONG : VT_DOUBLE;
#另
                        如 ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
                                整 addr_type = VT_INT, load_size = 4, load_type = VT_INT;
                                无符 长 长 ll;
#了如
                                整 r2, original_type;
                                original_type = vtop->type.t;
                                /* two register type load : expand to two words
                                   temporarily */
#如 PTR_SIZE == 4
                                如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                                        /* load constant */
                                        ll = vtop->c.i;
                                        vtop->c.i = ll; /* first word */
                                        load(r, vtop);
                                        vtop->r = r; /* save register value */
                                        vpushi(ll >> 32); /* second word */
                                } 另
#了如
                                如 (vtop->r & VT_LVAL) {
                                        /* We do not want to modifier the long long
                                           pointer here, so the safest (and less
                                           efficient) is to save all the other registers
                                           in the stack. XXX: totally inefficient. */
                           #如 0
                                        save_regs(1);
                           #另
                                        /* lvalue_save: save only if used further down the stack */
                                        save_reg_upstack(vtop->r, 1);
                           #了如
                                        /* load from memory */
                                        vtop->type.t = load_type;
                                        load(r, vtop);
                                        vdup();
                                        vtop[-1].r = r; /* save register value */
                                        /* increment pointer to get second word */
                                        vtop->type.t = addr_type;
                                        gaddrof();
                                        vpushi(load_size);
                                        gen_op('+');
                                        vtop->r |= VT_LVAL;
                                        vtop->type.t = load_type;
                                } 另 {
                                        /* move registers */
                                        load(r, vtop);
                                        vdup();
                                        vtop[-1].r = r; /* save register value */
                                        vtop->r = vtop[-1].r2;
                                }
                                /* Allocate second register. Here we rely on the fact that
                                   get_reg() tries first to free r2 of an SValue. */
                                r2 = get_reg(rc2);
                                load(r2, vtop);
                                vpop();
                                /* write second register */
                                vtop->r2 = r2;
                                vtop->type.t = original_type;
                        } 另 如 ((vtop->r & VT_LVAL) && !is_float(vtop->type.t)) {
                                整 t1, t;
                                /* lvalue of scalar type : need to use lvalue type
                                   because of possible cast */
                                t = vtop->type.t;
                                t1 = t;
                                /* compute memory access type */
                                如 (vtop->r & VT_LVAL_BYTE)
                                        t = VT_BYTE;
                                另 如 (vtop->r & VT_LVAL_SHORT)
                                        t = VT_SHORT;
                                如 (vtop->r & VT_LVAL_UNSIGNED)
                                        t |= VT_UNSIGNED;
                                vtop->type.t = t;
                                load(r, vtop);
                                /* restore wanted type */
                                vtop->type.t = t1;
                        } 另 {
                                /* one register type load */
                                load(r, vtop);
                        }
                }
                vtop->r = r;
#如定义 TCC_TARGET_C67
                /* uses register pairs for doubles */
                如 ((vtop->type.t & VT_BTYPE) == VT_DOUBLE)
                        vtop->r2 = r+1;
#了如
        }
        返回 r;
}

/* generate vtop[-1] and vtop[0] in resp. classes rc1 and rc2 */
ST_FUNC 空 gv2(整 rc1, 整 rc2)
{
        整 v;

        /* generate more generic register first. But VT_JMP or VT_CMP
           values must be generated first in all cases to avoid possible
           reload errors */
        v = vtop[0].r & VT_VALMASK;
        如 (v != VT_CMP && (v & ~1) != VT_JMP && rc1 <= rc2) {
                vswap();
                gv(rc1);
                vswap();
                gv(rc2);
                /* test if reload is needed for first register */
                如 ((vtop[-1].r & VT_VALMASK) >= VT_CONST) {
                        vswap();
                        gv(rc1);
                        vswap();
                }
        } 另 {
                gv(rc2);
                vswap();
                gv(rc1);
                vswap();
                /* test if reload is needed for first register */
                如 ((vtop[0].r & VT_VALMASK) >= VT_CONST) {
                        gv(rc2);
                }
        }
}

#如未定义 TCC_TARGET_ARM64
/* wrapper around RC_FRET to return a register by type */
静态 整 rc_fret(整 t)
{
#如定义 TCC_TARGET_X86_64
        如 (t == VT_LDOUBLE) {
                返回 RC_ST0;
        }
#了如
        返回 RC_FRET;
}
#了如

/* wrapper around REG_FRET to return a register by type */
静态 整 reg_fret(整 t)
{
#如定义 TCC_TARGET_X86_64
        如 (t == VT_LDOUBLE) {
                返回 TREG_ST0;
        }
#了如
        返回 REG_FRET;
}

#如 PTR_SIZE == 4
/* expand 64bit on stack in two ints */
静态 空 lexpand(空)
{
        整 u, v;
        u = vtop->type.t & (VT_DEFSIGN | VT_UNSIGNED);
        v = vtop->r & (VT_VALMASK | VT_LVAL);
        如 (v == VT_CONST) {
                vdup();
                vtop[0].c.i >>= 32;
        } 另 如 (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
                vdup();
                vtop[0].c.i += 4;
        } 另 {
                gv(RC_INT);
                vdup();
                vtop[0].r = vtop[-1].r2;
                vtop[0].r2 = vtop[-1].r2 = VT_CONST;
        }
        vtop[0].type.t = vtop[-1].type.t = VT_INT | u;
}
#了如

#如定义 TCC_TARGET_ARM
/* expand long long on stack */
ST_FUNC 空 lexpand_nr(空)
{
        整 u,v;

        u = vtop->type.t & (VT_DEFSIGN | VT_UNSIGNED);
        vdup();
        vtop->r2 = VT_CONST;
        vtop->type.t = VT_INT | u;
        v=vtop[-1].r & (VT_VALMASK | VT_LVAL);
        如 (v == VT_CONST) {
          vtop[-1].c.i = vtop->c.i;
          vtop->c.i = vtop->c.i >> 32;
          vtop->r = VT_CONST;
        } 另 如 (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
          vtop->c.i += 4;
          vtop->r = vtop[-1].r;
        } 另 如 (v > VT_CONST) {
          vtop--;
          lexpand();
        } 另
          vtop->r = vtop[-1].r2;
        vtop[-1].r2 = VT_CONST;
        vtop[-1].type.t = VT_INT | u;
}
#了如

#如 PTR_SIZE == 4
/* build a long long from two ints */
静态 空 lbuild(整 t)
{
        gv2(RC_INT, RC_INT);
        vtop[-1].r2 = vtop[0].r;
        vtop[-1].type.t = t;
        vpop();
}
#了如

/* convert stack entry to register and duplicate its value in another
   register */
静态 空 gv_dup(空)
{
        整 rc, t, r, r1;
        SValue sv;

        t = vtop->type.t;
#如 PTR_SIZE == 4
        如 ((t & VT_BTYPE) == VT_LLONG) {
                如 (t & VT_BITFIELD) {
                        gv(RC_INT);
                        t = vtop->type.t;
                }
                lexpand();
                gv_dup();
                vswap();
                vrotb(3);
                gv_dup();
                vrotb(4);
                /* stack: H L L1 H1 */
                lbuild(t);
                vrotb(3);
                vrotb(3);
                vswap();
                lbuild(t);
                vswap();
        } 另
#了如
        {
                /* duplicate value */
                rc = RC_INT;
                sv.type.t = VT_INT;
                如 (is_float(t)) {
                        rc = RC_FLOAT;
#如定义 TCC_TARGET_X86_64
                        如 ((t & VT_BTYPE) == VT_LDOUBLE) {
                                rc = RC_ST0;
                        }
#了如
                        sv.type.t = t;
                }
                r = gv(rc);
                r1 = get_reg(rc);
                sv.r = r;
                sv.c.i = 0;
                load(r1, &sv); /* move r to r1 */
                vdup();
                /* duplicates value */
                如 (r != r1)
                        vtop->r = r1;
        }
}

/* Generate value test
 *
 * Generate a test for any value (jump, comparison and integers) */
ST_FUNC 整 gvtst(整 inv, 整 t)
{
        整 v = vtop->r & VT_VALMASK;
        如 (v != VT_CMP && v != VT_JMP && v != VT_JMPI) {
                vpushi(0);
                gen_op(TOK_NE);
        }
        如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                /* constant jmp optimization */
                如 ((vtop->c.i != 0) != inv)
                        t = gjmp(t);
                vtop--;
                返回 t;
        }
        返回 gtst(inv, t);
}

#如 PTR_SIZE == 4
/* generate CPU independent (unsigned) long long operations */
静态 空 gen_opl(整 op)
{
        整 t, a, b, op1, c, i;
        整 func;
        无符 短 reg_iret = REG_IRET;
        无符 短 reg_lret = REG_LRET;
        SValue tmp;

        转接(op) {
        事例 '/':
        事例 TOK_PDIV:
                func = TOK___divdi3;
                跳转 gen_func;
        事例 TOK_UDIV:
                func = TOK___udivdi3;
                跳转 gen_func;
        事例 '%':
                func = TOK___moddi3;
                跳转 gen_mod_func;
        事例 TOK_UMOD:
                func = TOK___umoddi3;
        gen_mod_func:
#如定义 TCC_ARM_EABI
                reg_iret = TREG_R2;
                reg_lret = TREG_R3;
#了如
        gen_func:
                /* call generic long long function */
                vpush_global_sym(&func_old_type, func);
                vrott(3);
                gfunc_call(2);
                vpushi(0);
                vtop->r = reg_iret;
                vtop->r2 = reg_lret;
                跳出;
        事例 '^':
        事例 '&':
        事例 '|':
        事例 '*':
        事例 '+':
        事例 '-':
                //pv("gen_opl A",0,2);
                t = vtop->type.t;
                vswap();
                lexpand();
                vrotb(3);
                lexpand();
                /* stack: L1 H1 L2 H2 */
                tmp = vtop[0];
                vtop[0] = vtop[-3];
                vtop[-3] = tmp;
                tmp = vtop[-2];
                vtop[-2] = vtop[-3];
                vtop[-3] = tmp;
                vswap();
                /* stack: H1 H2 L1 L2 */
                //pv("gen_opl B",0,4);
                如 (op == '*') {
                        vpushv(vtop - 1);
                        vpushv(vtop - 1);
                        gen_op(TOK_UMULL);
                        lexpand();
                        /* stack: H1 H2 L1 L2 ML MH */
                        对于(i=0;i<4;i++)
                                vrotb(6);
                        /* stack: ML MH H1 H2 L1 L2 */
                        tmp = vtop[0];
                        vtop[0] = vtop[-2];
                        vtop[-2] = tmp;
                        /* stack: ML MH H1 L2 H2 L1 */
                        gen_op('*');
                        vrotb(3);
                        vrotb(3);
                        gen_op('*');
                        /* stack: ML MH M1 M2 */
                        gen_op('+');
                        gen_op('+');
                } 另 如 (op == '+' || op == '-') {
                        /* XXX: add non carry method too (for MIPS or alpha) */
                        如 (op == '+')
                                op1 = TOK_ADDC1;
                        另
                                op1 = TOK_SUBC1;
                        gen_op(op1);
                        /* stack: H1 H2 (L1 op L2) */
                        vrotb(3);
                        vrotb(3);
                        gen_op(op1 + 1); /* TOK_xxxC2 */
                } 另 {
                        gen_op(op);
                        /* stack: H1 H2 (L1 op L2) */
                        vrotb(3);
                        vrotb(3);
                        /* stack: (L1 op L2) H1 H2 */
                        gen_op(op);
                        /* stack: (L1 op L2) (H1 op H2) */
                }
                /* stack: L H */
                lbuild(t);
                跳出;
        事例 TOK_SAR:
        事例 TOK_SHR:
        事例 TOK_SHL:
                如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                        t = vtop[-1].type.t;
                        vswap();
                        lexpand();
                        vrotb(3);
                        /* stack: L H shift */
                        c = (整)vtop->c.i;
                        /* constant: simpler */
                        /* NOTE: all comments are for SHL. the other cases are
                           done by swapping words */
                        vpop();
                        如 (op != TOK_SHL)
                                vswap();
                        如 (c >= 32) {
                                /* stack: L H */
                                vpop();
                                如 (c > 32) {
                                        vpushi(c - 32);
                                        gen_op(op);
                                }
                                如 (op != TOK_SAR) {
                                        vpushi(0);
                                } 另 {
                                        gv_dup();
                                        vpushi(31);
                                        gen_op(TOK_SAR);
                                }
                                vswap();
                        } 另 {
                                vswap();
                                gv_dup();
                                /* stack: H L L */
                                vpushi(c);
                                gen_op(op);
                                vswap();
                                vpushi(32 - c);
                                如 (op == TOK_SHL)
                                        gen_op(TOK_SHR);
                                另
                                        gen_op(TOK_SHL);
                                vrotb(3);
                                /* stack: L L H */
                                vpushi(c);
                                如 (op == TOK_SHL)
                                        gen_op(TOK_SHL);
                                另
                                        gen_op(TOK_SHR);
                                gen_op('|');
                        }
                        如 (op != TOK_SHL)
                                vswap();
                        lbuild(t);
                } 另 {
                        /* XXX: should provide a faster fallback on x86 ? */
                        转接(op) {
                        事例 TOK_SAR:
                                func = TOK___ashrdi3;
                                跳转 gen_func;
                        事例 TOK_SHR:
                                func = TOK___lshrdi3;
                                跳转 gen_func;
                        事例 TOK_SHL:
                                func = TOK___ashldi3;
                                跳转 gen_func;
                        }
                }
                跳出;
        缺省:
                /* compare operations */
                t = vtop->type.t;
                vswap();
                lexpand();
                vrotb(3);
                lexpand();
                /* stack: L1 H1 L2 H2 */
                tmp = vtop[-1];
                vtop[-1] = vtop[-2];
                vtop[-2] = tmp;
                /* stack: L1 L2 H1 H2 */
                /* compare high */
                op1 = op;
                /* when values are equal, we need to compare low words. since
                   the jump is inverted, we invert the test too. */
                如 (op1 == TOK_LT)
                        op1 = TOK_LE;
                另 如 (op1 == TOK_GT)
                        op1 = TOK_GE;
                另 如 (op1 == TOK_ULT)
                        op1 = TOK_ULE;
                另 如 (op1 == TOK_UGT)
                        op1 = TOK_UGE;
                a = 0;
                b = 0;
                gen_op(op1);
                如 (op == TOK_NE) {
                        b = gvtst(0, 0);
                } 另 {
                        a = gvtst(1, 0);
                        如 (op != TOK_EQ) {
                                /* generate non equal test */
                                vpushi(TOK_NE);
                                vtop->r = VT_CMP;
                                b = gvtst(0, 0);
                        }
                }
                /* compare low. Always unsigned */
                op1 = op;
                如 (op1 == TOK_LT)
                        op1 = TOK_ULT;
                另 如 (op1 == TOK_LE)
                        op1 = TOK_ULE;
                另 如 (op1 == TOK_GT)
                        op1 = TOK_UGT;
                另 如 (op1 == TOK_GE)
                        op1 = TOK_UGE;
                gen_op(op1);
                a = gvtst(1, a);
                gsym(b);
                vseti(VT_JMPI, a);
                跳出;
        }
}
#了如

静态 uint64_t gen_opic_sdiv(uint64_t a, uint64_t b)
{
        uint64_t x = (a >> 63 ? -a : a) / (b >> 63 ? -b : b);
        返回 (a ^ b) >> 63 ? -x : x;
}

静态 整 gen_opic_lt(uint64_t a, uint64_t b)
{
        返回 (a ^ (uint64_t)1 << 63) < (b ^ (uint64_t)1 << 63);
}

/* handle integer constant optimizations and various machine
   independent opt */
静态 空 gen_opic(整 op)
{
        SValue *v1 = vtop - 1;
        SValue *v2 = vtop;
        整 t1 = v1->type.t & VT_BTYPE;
        整 t2 = v2->type.t & VT_BTYPE;
        整 c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        整 c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        uint64_t l1 = c1 ? v1->c.i : 0;
        uint64_t l2 = c2 ? v2->c.i : 0;
        整 shm = (t1 == VT_LLONG) ? 63 : 31;

        如 (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
                l1 = ((uint32_t)l1 |
                          (v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
        如 (t2 != VT_LLONG && (PTR_SIZE != 8 || t2 != VT_PTR))
                l2 = ((uint32_t)l2 |
                          (v2->type.t & VT_UNSIGNED ? 0 : -(l2 & 0x80000000)));

        如 (c1 && c2) {
                转接(op) {
                事例 '+': l1 += l2; 跳出;
                事例 '-': l1 -= l2; 跳出;
                事例 '&': l1 &= l2; 跳出;
                事例 '^': l1 ^= l2; 跳出;
                事例 '|': l1 |= l2; 跳出;
                事例 '*': l1 *= l2; 跳出;

                事例 TOK_PDIV:
                事例 '/':
                事例 '%':
                事例 TOK_UDIV:
                事例 TOK_UMOD:
                        /* if division by zero, generate explicit division */
                        如 (l2 == 0) {
                                如 (const_wanted)
                                        tcc_error("division by zero in constant");
                                跳转 general_case;
                        }
                        转接(op) {
                        缺省: l1 = gen_opic_sdiv(l1, l2); 跳出;
                        事例 '%': l1 = l1 - l2 * gen_opic_sdiv(l1, l2); 跳出;
                        事例 TOK_UDIV: l1 = l1 / l2; 跳出;
                        事例 TOK_UMOD: l1 = l1 % l2; 跳出;
                        }
                        跳出;
                事例 TOK_SHL: l1 <<= (l2 & shm); 跳出;
                事例 TOK_SHR: l1 >>= (l2 & shm); 跳出;
                事例 TOK_SAR:
                        l1 = (l1 >> 63) ? ~(~l1 >> (l2 & shm)) : l1 >> (l2 & shm);
                        跳出;
                        /* tests */
                事例 TOK_ULT: l1 = l1 < l2; 跳出;
                事例 TOK_UGE: l1 = l1 >= l2; 跳出;
                事例 TOK_EQ: l1 = l1 == l2; 跳出;
                事例 TOK_NE: l1 = l1 != l2; 跳出;
                事例 TOK_ULE: l1 = l1 <= l2; 跳出;
                事例 TOK_UGT: l1 = l1 > l2; 跳出;
                事例 TOK_LT: l1 = gen_opic_lt(l1, l2); 跳出;
                事例 TOK_GE: l1 = !gen_opic_lt(l1, l2); 跳出;
                事例 TOK_LE: l1 = !gen_opic_lt(l2, l1); 跳出;
                事例 TOK_GT: l1 = gen_opic_lt(l2, l1); 跳出;
                        /* logical */
                事例 TOK_LAND: l1 = l1 && l2; 跳出;
                事例 TOK_LOR: l1 = l1 || l2; 跳出;
                缺省:
                        跳转 general_case;
                }
                如 (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
                        l1 = ((uint32_t)l1 |
                                (v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
                v1->c.i = l1;
                vtop--;
        } 另 {
                /* if commutative ops, put c2 as constant */
                如 (c1 && (op == '+' || op == '&' || op == '^' ||
                                   op == '|' || op == '*')) {
                        vswap();
                        c2 = c1; //c = c1, c1 = c2, c2 = c;
                        l2 = l1; //l = l1, l1 = l2, l2 = l;
                }
                如 (!const_wanted &&
                        c1 && ((l1 == 0 &&
                                        (op == TOK_SHL || op == TOK_SHR || op == TOK_SAR)) ||
                                   (l1 == -1 && op == TOK_SAR))) {
                        /* treat (0 << x), (0 >> x) and (-1 >> x) as constant */
                        vtop--;
                } 另 如 (!const_wanted &&
                                   c2 && ((l2 == 0 && (op == '&' || op == '*')) ||
                                                  (op == '|' &&
                                                        (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))) ||
                                                  (l2 == 1 && (op == '%' || op == TOK_UMOD)))) {
                        /* treat (x & 0), (x * 0), (x | -1) and (x % 1) as constant */
                        如 (l2 == 1)
                                vtop->c.i = 0;
                        vswap();
                        vtop--;
                } 另 如 (c2 && (((op == '*' || op == '/' || op == TOK_UDIV ||
                                                  op == TOK_PDIV) &&
                                                   l2 == 1) ||
                                                  ((op == '+' || op == '-' || op == '|' || op == '^' ||
                                                        op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) &&
                                                   l2 == 0) ||
                                                  (op == '&' &&
                                                        (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))))) {
                        /* filter out NOP operations like x*1, x-0, x&-1... */
                        vtop--;
                } 另 如 (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
                        /* try to use shifts instead of muls or divs */
                        如 (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                                整 n = -1;
                                当 (l2) {
                                        l2 >>= 1;
                                        n++;
                                }
                                vtop->c.i = n;
                                如 (op == '*')
                                        op = TOK_SHL;
                                另 如 (op == TOK_PDIV)
                                        op = TOK_SAR;
                                另
                                        op = TOK_SHR;
                        }
                        跳转 general_case;
                } 另 如 (c2 && (op == '+' || op == '-') &&
                                   (((vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM))
                                        || (vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_LOCAL)) {
                        /* symbol + constant case */
                        如 (op == '-')
                                l2 = -l2;
                        l2 += vtop[-1].c.i;
                        /* The backends can't always deal with addends to symbols
                           larger than +-1<<31.  Don't construct such.  */
                        如 ((整)l2 != l2)
                                跳转 general_case;
                        vtop--;
                        vtop->c.i = l2;
                } 另 {
                general_case:
                                /* call low level op generator */
                                如 (t1 == VT_LLONG || t2 == VT_LLONG ||
                                        (PTR_SIZE == 8 && (t1 == VT_PTR || t2 == VT_PTR)))
                                        gen_opl(op);
                                另
                                        gen_opi(op);
                }
        }
}

/* generate a floating point operation with constant propagation */
静态 空 gen_opif(整 op)
{
        整 c1, c2;
        SValue *v1, *v2;
#如 已定义 _MSC_VER && 已定义 _AMD64_
        /* avoid bad optimization with f1 -= f2 for f1:-0.0, f2:0.0 */
        易变
#了如
        长 双精 f1, f2;

        v1 = vtop - 1;
        v2 = vtop;
        /* currently, we cannot do computations with forward symbols */
        c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        如 (c1 && c2) {
                如 (v1->type.t == VT_FLOAT) {
                        f1 = v1->c.f;
                        f2 = v2->c.f;
                } 另 如 (v1->type.t == VT_DOUBLE) {
                        f1 = v1->c.d;
                        f2 = v2->c.d;
                } 另 {
                        f1 = v1->c.ld;
                        f2 = v2->c.ld;
                }

                /* NOTE: we only do constant propagation if finite number (not
                   NaN or infinity) (ANSI spec) */
                如 (!ieee_finite(f1) || !ieee_finite(f2))
                        跳转 general_case;

                转接(op) {
                事例 '+': f1 += f2; 跳出;
                事例 '-': f1 -= f2; 跳出;
                事例 '*': f1 *= f2; 跳出;
                事例 '/':
                        如 (f2 == 0.0) {
                                如 (const_wanted)
                                        tcc_error("division by zero in constant");
                                跳转 general_case;
                        }
                        f1 /= f2;
                        跳出;
                        /* XXX: also handles tests ? */
                缺省:
                        跳转 general_case;
                }
                /* XXX: overflow test ? */
                如 (v1->type.t == VT_FLOAT) {
                        v1->c.f = f1;
                } 另 如 (v1->type.t == VT_DOUBLE) {
                        v1->c.d = f1;
                } 另 {
                        v1->c.ld = f1;
                }
                vtop--;
        } 另 {
        general_case:
                gen_opf(op);
        }
}

静态 整 pointed_size(CType *type)
{
        整 align;
        返回 type_size(pointed_type(type), &align);
}

静态 空 vla_runtime_pointed_size(CType *type)
{
        整 align;
        vla_runtime_type_size(pointed_type(type), &align);
}

静态 内联 整 is_null_pointer(SValue *p)
{
        如 ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
                返回 0;
        返回 ((p->type.t & VT_BTYPE) == VT_INT && (uint32_t)p->c.i == 0) ||
                ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.i == 0) ||
                ((p->type.t & VT_BTYPE) == VT_PTR &&
                 (PTR_SIZE == 4 ? (uint32_t)p->c.i == 0 : p->c.i == 0));
}

静态 内联 整 is_integer_btype(整 bt)
{
        返回 (bt == VT_BYTE || bt == VT_SHORT ||
                        bt == VT_INT || bt == VT_LLONG);
}

/* check types for comparison or subtraction of pointers */
静态 空 check_comparison_pointer_types(SValue *p1, SValue *p2, 整 op)
{
        CType *type1, *type2, tmp_type1, tmp_type2;
        整 bt1, bt2;

        /* null pointers are accepted for all comparisons as gcc */
        如 (is_null_pointer(p1) || is_null_pointer(p2))
                返回;
        type1 = &p1->type;
        type2 = &p2->type;
        bt1 = type1->t & VT_BTYPE;
        bt2 = type2->t & VT_BTYPE;
        /* accept comparison between pointer and integer with a warning */
        如 ((is_integer_btype(bt1) || is_integer_btype(bt2)) && op != '-') {
                如 (op != TOK_LOR && op != TOK_LAND )
                        tcc_warning("comparison between pointer and integer");
                返回;
        }

        /* both must be pointers or implicit function pointers */
        如 (bt1 == VT_PTR) {
                type1 = pointed_type(type1);
        } 另 如 (bt1 != VT_FUNC)
                跳转 invalid_operands;

        如 (bt2 == VT_PTR) {
                type2 = pointed_type(type2);
        } 另 如 (bt2 != VT_FUNC) {
        invalid_operands:
                tcc_error("invalid operands to binary %s", get_tok_str(op, NULL));
        }
        如 ((type1->t & VT_BTYPE) == VT_VOID ||
                (type2->t & VT_BTYPE) == VT_VOID)
                返回;
        tmp_type1 = *type1;
        tmp_type2 = *type2;
        tmp_type1.t &= ~(VT_DEFSIGN | VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
        tmp_type2.t &= ~(VT_DEFSIGN | VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
        如 (!is_compatible_types(&tmp_type1, &tmp_type2)) {
                /* gcc-like error if '-' is used */
                如 (op == '-')
                        跳转 invalid_operands;
                另
                        tcc_warning("comparison of distinct pointer types lacks a cast");
        }
}

/* generic gen_op: handles types problems */
ST_FUNC 空 gen_op(整 op)
{
        整 u, t1, t2, bt1, bt2, t;
        CType type1;

redo:
        t1 = vtop[-1].type.t;
        t2 = vtop[0].type.t;
        bt1 = t1 & VT_BTYPE;
        bt2 = t2 & VT_BTYPE;

        如 (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                tcc_error("operation on a struct");
        } 另 如 (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                如 (bt2 == VT_FUNC) {
                        mk_pointer(&vtop->type);
                        gaddrof();
                }
                如 (bt1 == VT_FUNC) {
                        vswap();
                        mk_pointer(&vtop->type);
                        gaddrof();
                        vswap();
                }
                跳转 redo;
        } 另 如 (bt1 == VT_PTR || bt2 == VT_PTR) {
                /* at least one operand is a pointer */
                /* relational op: must be both pointers */
                如 (op >= TOK_ULT && op <= TOK_LOR) {
                        check_comparison_pointer_types(vtop - 1, vtop, op);
                        /* pointers are handled are unsigned */
#如 PTR_SIZE == 8
                        t = VT_LLONG | VT_UNSIGNED;
#另
                        t = VT_INT | VT_UNSIGNED;
#了如
                        跳转 std_op;
                }
                /* if both pointers, then it must be the '-' op */
                如 (bt1 == VT_PTR && bt2 == VT_PTR) {
                        如 (op != '-')
                                tcc_error("cannot use pointers here");
                        check_comparison_pointer_types(vtop - 1, vtop, op);
                        /* XXX: check that types are compatible */
                        如 (vtop[-1].type.t & VT_VLA) {
                                vla_runtime_pointed_size(&vtop[-1].type);
                        } 另 {
                                vpushi(pointed_size(&vtop[-1].type));
                        }
                        vrott(3);
                        gen_opic(op);
                        /* set to integer type */
#如 PTR_SIZE == 8
                        vtop->type.t = VT_LLONG;
#另
                        vtop->type.t = VT_INT;
#了如
                        vswap();
                        gen_op(TOK_PDIV);
                } 另 {
                        /* exactly one pointer : must be '+' or '-'. */
                        如 (op != '-' && op != '+')
                                tcc_error("cannot use pointers here");
                        /* Put pointer as first operand */
                        如 (bt2 == VT_PTR) {
                                vswap();
                                t = t1, t1 = t2, t2 = t;
                        }
#如 PTR_SIZE == 4
                        如 ((vtop[0].type.t & VT_BTYPE) == VT_LLONG)
                                /* XXX: truncate here because gen_opl can't handle ptr + long long */
                                gen_cast_s(VT_INT);
#了如
                        type1 = vtop[-1].type;
                        type1.t &= ~VT_ARRAY;
                        如 (vtop[-1].type.t & VT_VLA)
                                vla_runtime_pointed_size(&vtop[-1].type);
                        另 {
                                u = pointed_size(&vtop[-1].type);
                                如 (u < 0)
                                        tcc_error("unknown array element size");
#如 PTR_SIZE == 8
                                vpushll(u);
#另
                                /* XXX: cast to int ? (long long case) */
                                vpushi(u);
#了如
                        }
                        gen_op('*');
#如 0
/* #如定义 CONFIG_TCC_BCHECK
        The main reason to removing this code:
                #包含 <stdio.h>
                整 main ()
                {
                        整 v[10];
                        整 i = 10;
                        整 j = 9;
                        fprintf(stderr, "v+i-j  = %p\n", v+i-j);
                        fprintf(stderr, "v+(i-j)  = %p\n", v+(i-j));
                }
        When this code is on. then the output looks like
                v+i-j = 0xfffffffe
                v+(i-j) = 0xbff84000
        */
                        /* if evaluating constant expression, no code should be
                           generated, so no bound check */
                        如 (tcc_state->do_bounds_check && !const_wanted) {
                                /* if bounded pointers, we generate a special code to
                                   test bounds */
                                如 (op == '-') {
                                        vpushi(0);
                                        vswap();
                                        gen_op('-');
                                }
                                gen_bounded_ptr_add();
                        } 另
#了如
                        {
                                gen_opic(op);
                        }
                        /* put again type if gen_opic() swaped operands */
                        vtop->type = type1;
                }
        } 另 如 (is_float(bt1) || is_float(bt2)) {
                /* compute bigger type and do implicit casts */
                如 (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                        t = VT_LDOUBLE;
                } 另 如 (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                        t = VT_DOUBLE;
                } 另 {
                        t = VT_FLOAT;
                }
                /* floats can only be used for a few operations */
                如 (op != '+' && op != '-' && op != '*' && op != '/' &&
                        (op < TOK_ULT || op > TOK_GT))
                        tcc_error("invalid operands for binary operation");
                跳转 std_op;
        } 另 如 (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL) {
                t = bt1 == VT_LLONG ? VT_LLONG : VT_INT;
                如 ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (t | VT_UNSIGNED))
                  t |= VT_UNSIGNED;
                跳转 std_op;
        } 另 如 (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                /* cast to biggest op */
                t = VT_LLONG;
                /* check if we need to keep type as long or as long long */
                如 ((t1 & VT_LONG &&  (t2 & (VT_BTYPE | VT_LONG)) != VT_LLONG) ||
                        (t2 & VT_LONG &&  (t1 & (VT_BTYPE | VT_LONG)) != VT_LLONG))
                        t |= VT_LONG;
                /* convert to unsigned if it does not fit in a long long */
                如 ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
                        t |= VT_UNSIGNED;
                跳转 std_op;
        } 另 {
                /* integer operations */
                t = VT_INT;

                如 ((t1 & VT_LONG) || (t2 & VT_LONG))
                        t |= VT_LONG;

                /* convert to unsigned if it does not fit in an integer */
                如 ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
                        t |= VT_UNSIGNED;
        std_op:
                /* XXX: currently, some unsigned operations are explicit, so
                   we modify them here */
                如 (t & VT_UNSIGNED) {
                        如 (op == TOK_SAR)
                                op = TOK_SHR;
                        另 如 (op == '/')
                                op = TOK_UDIV;
                        另 如 (op == '%')
                                op = TOK_UMOD;
                        另 如 (op == TOK_LT)
                                op = TOK_ULT;
                        另 如 (op == TOK_GT)
                                op = TOK_UGT;
                        另 如 (op == TOK_LE)
                                op = TOK_ULE;
                        另 如 (op == TOK_GE)
                                op = TOK_UGE;
                }
                vswap();
                type1.t = t;
                type1.ref = NULL;
                gen_cast(&type1);
                vswap();
                /* special case for shifts and long long: we keep the shift as
                   an integer */
                如 (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
                        type1.t = VT_INT;
                gen_cast(&type1);
                如 (is_float(t))
                        gen_opif(op);
                另
                        gen_opic(op);
                如 (op >= TOK_ULT && op <= TOK_GT) {
                        /* relational op: the result is an int */
                        vtop->type.t = VT_INT;
                } 另 {
                        vtop->type.t = t;
                }
        }
        // Make sure that we have converted to an rvalue:
        如 (vtop->r & VT_LVAL)
                gv(is_float(vtop->type.t & VT_BTYPE) ? RC_FLOAT : RC_INT);
}

#如未定义 TCC_TARGET_ARM
/* generic itof for unsigned long long case */
静态 空 gen_cvt_itof1(整 t)
{
#如定义 TCC_TARGET_ARM64
        gen_cvt_itof(t);
#另
        如 ((vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) ==
                (VT_LLONG | VT_UNSIGNED)) {

                如 (t == VT_FLOAT)
                        vpush_global_sym(&func_old_type, TOK___floatundisf);
#如 LDOUBLE_SIZE != 8
                另 如 (t == VT_LDOUBLE)
                        vpush_global_sym(&func_old_type, TOK___floatundixf);
#了如
                另
                        vpush_global_sym(&func_old_type, TOK___floatundidf);
                vrott(2);
                gfunc_call(1);
                vpushi(0);
                vtop->r = reg_fret(t);
        } 另 {
                gen_cvt_itof(t);
        }
#了如
}
#了如

/* generic ftoi for unsigned long long case */
静态 空 gen_cvt_ftoi1(整 t)
{
#如定义 TCC_TARGET_ARM64
        gen_cvt_ftoi(t);
#另
        整 st;

        如 (t == (VT_LLONG | VT_UNSIGNED)) {
                /* not handled natively */
                st = vtop->type.t & VT_BTYPE;
                如 (st == VT_FLOAT)
                        vpush_global_sym(&func_old_type, TOK___fixunssfdi);
#如 LDOUBLE_SIZE != 8
                另 如 (st == VT_LDOUBLE)
                        vpush_global_sym(&func_old_type, TOK___fixunsxfdi);
#了如
                另
                        vpush_global_sym(&func_old_type, TOK___fixunsdfdi);
                vrott(2);
                gfunc_call(1);
                vpushi(0);
                vtop->r = REG_IRET;
                vtop->r2 = REG_LRET;
        } 另 {
                gen_cvt_ftoi(t);
        }
#了如
}

/* force char or short cast */
静态 空 force_charshort_cast(整 t)
{
        整 bits, dbt;

        /* cannot cast static initializers */
        如 (STATIC_DATA_WANTED)
                返回;

        dbt = t & VT_BTYPE;
        /* XXX: add optimization if lvalue : just change type and offset */
        如 (dbt == VT_BYTE)
                bits = 8;
        另
                bits = 16;
        如 (t & VT_UNSIGNED) {
                vpushi((1 << bits) - 1);
                gen_op('&');
        } 另 {
                如 ((vtop->type.t & VT_BTYPE) == VT_LLONG)
                        bits = 64 - bits;
                另
                        bits = 32 - bits;
                vpushi(bits);
                gen_op(TOK_SHL);
                /* result must be signed or the SAR is converted to an SHL
                   This was not the case when "t" was a signed short
                   and the last value on the stack was an unsigned int */
                vtop->type.t &= ~VT_UNSIGNED;
                vpushi(bits);
                gen_op(TOK_SAR);
        }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
静态 空 gen_cast_s(整 t)
{
        CType type;
        type.t = t;
        type.ref = NULL;
        gen_cast(&type);
}

静态 空 gen_cast(CType *type)
{
        整 sbt, dbt, sf, df, c, p;

        /* special delayed cast for char/short */
        /* XXX: in some cases (multiple cascaded casts), it may still
           be incorrect */
        如 (vtop->r & VT_MUSTCAST) {
                vtop->r &= ~VT_MUSTCAST;
                force_charshort_cast(vtop->type.t);
        }

        /* bitfields first get cast to ints */
        如 (vtop->type.t & VT_BITFIELD) {
                gv(RC_INT);
        }

        dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
        sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

        如 (sbt != dbt) {
                sf = is_float(sbt);
                df = is_float(dbt);
                c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
                p = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
                如 (c) {
                        /* constant case: we can do it now */
                        /* XXX: in ISOC, cannot do it if error in convert */
                        如 (sbt == VT_FLOAT)
                                vtop->c.ld = vtop->c.f;
                        另 如 (sbt == VT_DOUBLE)
                                vtop->c.ld = vtop->c.d;

                        如 (df) {
                                如 ((sbt & VT_BTYPE) == VT_LLONG) {
                                        如 ((sbt & VT_UNSIGNED) || !(vtop->c.i >> 63))
                                                vtop->c.ld = vtop->c.i;
                                        另
                                                vtop->c.ld = -(长 双精)-vtop->c.i;
                                } 另 如(!sf) {
                                        如 ((sbt & VT_UNSIGNED) || !(vtop->c.i >> 31))
                                                vtop->c.ld = (uint32_t)vtop->c.i;
                                        另
                                                vtop->c.ld = -(长 双精)-(uint32_t)vtop->c.i;
                                }

                                如 (dbt == VT_FLOAT)
                                        vtop->c.f = (单精)vtop->c.ld;
                                另 如 (dbt == VT_DOUBLE)
                                        vtop->c.d = (双精)vtop->c.ld;
                        } 另 如 (sf && dbt == (VT_LLONG|VT_UNSIGNED)) {
                                vtop->c.i = vtop->c.ld;
                        } 另 如 (sf && dbt == VT_BOOL) {
                                vtop->c.i = (vtop->c.ld != 0);
                        } 另 {
                                如(sf)
                                        vtop->c.i = vtop->c.ld;
                                另 如 (sbt == (VT_LLONG|VT_UNSIGNED))
                                        ;
                                另 如 (sbt & VT_UNSIGNED)
                                        vtop->c.i = (uint32_t)vtop->c.i;
#如 PTR_SIZE == 8
                                另 如 (sbt == VT_PTR)
                                        ;
#了如
                                另 如 (sbt != VT_LLONG)
                                        vtop->c.i = ((uint32_t)vtop->c.i |
                                                                  -(vtop->c.i & 0x80000000));

                                如 (dbt == (VT_LLONG|VT_UNSIGNED))
                                        ;
                                另 如 (dbt == VT_BOOL)
                                        vtop->c.i = (vtop->c.i != 0);
#如 PTR_SIZE == 8
                                另 如 (dbt == VT_PTR)
                                        ;
#了如
                                另 如 (dbt != VT_LLONG) {
                                        uint32_t m = ((dbt & VT_BTYPE) == VT_BYTE ? 0xff :
                                                                  (dbt & VT_BTYPE) == VT_SHORT ? 0xffff :
                                                                  0xffffffff);
                                        vtop->c.i &= m;
                                        如 (!(dbt & VT_UNSIGNED))
                                                vtop->c.i |= -(vtop->c.i & ((m >> 1) + 1));
                                }
                        }
                } 另 如 (p && dbt == VT_BOOL) {
                        vtop->r = VT_CONST;
                        vtop->c.i = 1;
                } 另 {
                        /* non constant case: generate code */
                        如 (sf && df) {
                                /* convert from fp to fp */
                                gen_cvt_ftof(dbt);
                        } 另 如 (df) {
                                /* convert int to fp */
                                gen_cvt_itof1(dbt);
                        } 另 如 (sf) {
                                /* convert fp to int */
                                如 (dbt == VT_BOOL) {
                                         vpushi(0);
                                         gen_op(TOK_NE);
                                } 另 {
                                        /* we handle char/short/etc... with generic code */
                                        如 (dbt != (VT_INT | VT_UNSIGNED) &&
                                                dbt != (VT_LLONG | VT_UNSIGNED) &&
                                                dbt != VT_LLONG)
                                                dbt = VT_INT;
                                        gen_cvt_ftoi1(dbt);
                                        如 (dbt == VT_INT && (type->t & (VT_BTYPE | VT_UNSIGNED)) != dbt) {
                                                /* additional cast for char/short... */
                                                vtop->type.t = dbt;
                                                gen_cast(type);
                                        }
                                }
#如 PTR_SIZE == 4
                        } 另 如 ((dbt & VT_BTYPE) == VT_LLONG) {
                                如 ((sbt & VT_BTYPE) != VT_LLONG) {
                                        /* scalar to long long */
                                        /* machine independent conversion */
                                        gv(RC_INT);
                                        /* generate high word */
                                        如 (sbt == (VT_INT | VT_UNSIGNED)) {
                                                vpushi(0);
                                                gv(RC_INT);
                                        } 另 {
                                                如 (sbt == VT_PTR) {
                                                        /* cast from pointer to int before we apply
                                                           shift operation, which pointers don't support*/
                                                        gen_cast_s(VT_INT);
                                                }
                                                gv_dup();
                                                vpushi(31);
                                                gen_op(TOK_SAR);
                                        }
                                        /* patch second register */
                                        vtop[-1].r2 = vtop->r;
                                        vpop();
                                }
#另
                        } 另 如 ((dbt & VT_BTYPE) == VT_LLONG ||
                                           (dbt & VT_BTYPE) == VT_PTR ||
                                           (dbt & VT_BTYPE) == VT_FUNC) {
                                如 ((sbt & VT_BTYPE) != VT_LLONG &&
                                        (sbt & VT_BTYPE) != VT_PTR &&
                                        (sbt & VT_BTYPE) != VT_FUNC) {
                                        /* need to convert from 32bit to 64bit */
                                        gv(RC_INT);
                                        如 (sbt != (VT_INT | VT_UNSIGNED)) {
#如 已定义(TCC_TARGET_ARM64)
                                                gen_cvt_sxtw();
#另如 已定义(TCC_TARGET_X86_64)
                                                整 r = gv(RC_INT);
                                                /* x86_64 specific: movslq */
                                                o(0x6348);
                                                o(0xc0 + (REG_VALUE(r) << 3) + REG_VALUE(r));
#另
#错误
#了如
                                        }
                                }
#了如
                        } 另 如 (dbt == VT_BOOL) {
                                /* scalar to bool */
                                vpushi(0);
                                gen_op(TOK_NE);
                        } 另 如 ((dbt & VT_BTYPE) == VT_BYTE ||
                                           (dbt & VT_BTYPE) == VT_SHORT) {
                                如 (sbt == VT_PTR) {
                                        vtop->type.t = VT_INT;
                                        tcc_warning("nonportable conversion from pointer to char/short");
                                }
                                force_charshort_cast(dbt);
#如 PTR_SIZE == 4
                        } 另 如 ((dbt & VT_BTYPE) == VT_INT) {
                                /* scalar to int */
                                如 ((sbt & VT_BTYPE) == VT_LLONG) {
                                        /* from long long: just take low order word */
                                        lexpand();
                                        vpop();
                                }
                                /* if lvalue and single word type, nothing to do because
                                   the lvalue already contains the real type size (see
                                   VT_LVAL_xxx constants) */
#了如
                        }
                }
        } 另 如 ((dbt & VT_BTYPE) == VT_PTR && !(vtop->r & VT_LVAL)) {
                /* if we are casting between pointer types,
                   we must update the VT_LVAL_xxx size */
                vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                                  | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
        }
        vtop->type = *type;
}

/* return type size as known at compile time. Put alignment at 'a' */
ST_FUNC 整 type_size(CType *type, 整 *a)
{
        Sym *s;
        整 bt;

        bt = type->t & VT_BTYPE;
        如 (bt == VT_STRUCT) {
                /* struct/union */
                s = type->ref;
                *a = s->r;
                返回 s->c;
        } 另 如 (bt == VT_PTR) {
                如 (type->t & VT_ARRAY) {
                        整 ts;

                        s = type->ref;
                        ts = type_size(&s->type, a);

                        如 (ts < 0 && s->c < 0)
                                ts = -ts;

                        返回 ts * s->c;
                } 另 {
                        *a = PTR_SIZE;
                        返回 PTR_SIZE;
                }
        } 另 如 (IS_ENUM(type->t) && type->ref->c == -1) {
                返回 -1; /* incomplete enum */
        } 另 如 (bt == VT_LDOUBLE) {
                *a = LDOUBLE_ALIGN;
                返回 LDOUBLE_SIZE;
        } 另 如 (bt == VT_DOUBLE || bt == VT_LLONG) {
#如定义 TCC_TARGET_I386
#如定义 TCC_TARGET_PE
                *a = 8;
#另
                *a = 4;
#了如
#另如 已定义(TCC_TARGET_ARM)
#如定义 TCC_ARM_EABI
                *a = 8;
#另
                *a = 4;
#了如
#另
                *a = 8;
#了如
                返回 8;
        } 另 如 (bt == VT_INT || bt == VT_FLOAT) {
                *a = 4;
                返回 4;
        } 另 如 (bt == VT_SHORT) {
                *a = 2;
                返回 2;
        } 另 如 (bt == VT_QLONG || bt == VT_QFLOAT) {
                *a = 8;
                返回 16;
        } 另 {
                /* char, void, function, _Bool */
                *a = 1;
                返回 1;
        }
}

/* push type size as known at runtime time on top of value stack. Put
   alignment at 'a' */
ST_FUNC 空 vla_runtime_type_size(CType *type, 整 *a)
{
        如 (type->t & VT_VLA) {
                type_size(&type->ref->type, a);
                vset(&int_type, VT_LOCAL|VT_LVAL, type->ref->c);
        } 另 {
                vpushi(type_size(type, a));
        }
}

静态 空 vla_sp_restore(空) {
        如 (vlas_in_scope) {
                gen_vla_sp_restore(vla_sp_loc);
        }
}

静态 空 vla_sp_restore_root(空) {
        如 (vlas_in_scope) {
                gen_vla_sp_restore(vla_sp_root_loc);
        }
}

/* return the pointed type of t */
静态 内联 CType *pointed_type(CType *type)
{
        返回 &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
ST_FUNC 空 mk_pointer(CType *type)
{
        Sym *s;
        s = sym_push(SYM_FIELD, type, 0, -1);
        type->t = VT_PTR | (type->t & VT_STORAGE);
        type->ref = s;
}

/* compare function types. OLD functions match any new functions */
静态 整 is_compatible_func(CType *type1, CType *type2)
{
        Sym *s1, *s2;

        s1 = type1->ref;
        s2 = type2->ref;
        如 (!is_compatible_types(&s1->type, &s2->type))
                返回 0;
        /* check func_call */
        如 (s1->f.func_call != s2->f.func_call)
                返回 0;
        /* XXX: not complete */
        如 (s1->f.func_type == FUNC_OLD || s2->f.func_type == FUNC_OLD)
                返回 1;
        如 (s1->f.func_type != s2->f.func_type)
                返回 0;
        当 (s1 != NULL) {
                如 (s2 == NULL)
                        返回 0;
                如 (!is_compatible_unqualified_types(&s1->type, &s2->type))
                        返回 0;
                s1 = s1->next;
                s2 = s2->next;
        }
        如 (s2)
                返回 0;
        返回 1;
}

/* return true if type1 and type2 are the same.  If unqualified is
   true, qualifiers on the types are ignored.

   - enums are not checked as gcc __builtin_types_compatible_p ()
 */
静态 整 compare_types(CType *type1, CType *type2, 整 unqualified)
{
        整 bt1, t1, t2;

        t1 = type1->t & VT_TYPE;
        t2 = type2->t & VT_TYPE;
        如 (unqualified) {
                /* strip qualifiers before comparing */
                t1 &= ~(VT_CONSTANT | VT_VOLATILE);
                t2 &= ~(VT_CONSTANT | VT_VOLATILE);
        }

        /* Default Vs explicit signedness only matters for char */
        如 ((t1 & VT_BTYPE) != VT_BYTE) {
                t1 &= ~VT_DEFSIGN;
                t2 &= ~VT_DEFSIGN;
        }

        /* XXX: bitfields ? */
        如 (t1 != t2)
                返回 0;
        /* test more complicated cases */
        bt1 = t1 & VT_BTYPE;
        如 (bt1 == VT_PTR) {
                type1 = pointed_type(type1);
                type2 = pointed_type(type2);
                返回 is_compatible_types(type1, type2);
        } 另 如 (bt1 == VT_STRUCT) {
                返回 (type1->ref == type2->ref);
        } 另 如 (bt1 == VT_FUNC) {
                返回 is_compatible_func(type1, type2);
        } 另 {
                返回 1;
        }
}

/* return true if type1 and type2 are exactly the same (including
   qualifiers).
*/
静态 整 is_compatible_types(CType *type1, CType *type2)
{
        返回 compare_types(type1,type2,0);
}

/* return true if type1 and type2 are the same (ignoring qualifiers).
*/
静态 整 is_compatible_unqualified_types(CType *type1, CType *type2)
{
        返回 compare_types(type1,type2,1);
}

/* print a type. If 'varstr' is not NULL, then the variable is also
   printed in the type */
/* XXX: union */
/* XXX: add array and function pointers */
静态 空 type_to_str(字 *buf, 整 buf_size,
                                 CType *type, 不变 字 *varstr)
{
        整 bt, v, t;
        Sym *s, *sa;
        字 buf1[256];
        不变 字 *tstr;

        t = type->t;
        bt = t & VT_BTYPE;
        buf[0] = '\0';
        如 (t & VT_CONSTANT)
                pstrcat(buf, buf_size, "const ");
        如 (t & VT_VOLATILE)
                pstrcat(buf, buf_size, "volatile ");
        如 ((t & (VT_DEFSIGN | VT_UNSIGNED)) == (VT_DEFSIGN | VT_UNSIGNED))
                pstrcat(buf, buf_size, "unsigned ");
        另 如 (t & VT_DEFSIGN)
                pstrcat(buf, buf_size, "signed ");
        如 (t & VT_EXTERN)
                pstrcat(buf, buf_size, "extern ");
        如 (t & VT_STATIC)
                pstrcat(buf, buf_size, "static ");
        如 (t & VT_TYPEDEF)
                pstrcat(buf, buf_size, "typedef ");
        如 (t & VT_INLINE)
                pstrcat(buf, buf_size, "inline ");
        buf_size -= strlen(buf);
        buf += strlen(buf);
        如 (IS_ENUM(t)) {
                tstr = "enum ";
                跳转 tstruct;
        }

        如 (!bt && VT_LONG & t) {
          tstr = "long";
          跳转 add_tstr;
        }

        转接(bt) {
        事例 VT_VOID:
                tstr = "void";
                跳转 add_tstr;
        事例 VT_BOOL:
                tstr = "_Bool";
                跳转 add_tstr;
        事例 VT_BYTE:
                tstr = "char";
                跳转 add_tstr;
        事例 VT_SHORT:
                tstr = "short";
                跳转 add_tstr;
        事例 VT_INT:
                tstr = "int";
                跳转 add_tstr;
        事例 VT_LLONG:
                tstr = "long long";
                跳转 add_tstr;
        事例 VT_FLOAT:
                tstr = "float";
                跳转 add_tstr;
        事例 VT_DOUBLE:
                tstr = "double";
                跳转 add_tstr;
        事例 VT_LDOUBLE:
                tstr = "long double";
        add_tstr:
                pstrcat(buf, buf_size, tstr);
                跳出;
        事例 VT_STRUCT:
                tstr = "struct ";
                如 (IS_UNION(t))
                        tstr = "union ";
        tstruct:
                pstrcat(buf, buf_size, tstr);
                v = type->ref->v & ~SYM_STRUCT;
                如 (v >= SYM_FIRST_ANOM)
                        pstrcat(buf, buf_size, "<anonymous>");
                另
                        pstrcat(buf, buf_size, get_tok_str(v, NULL));
                跳出;
        事例 VT_FUNC:
                s = type->ref;
                type_to_str(buf, buf_size, &s->type, varstr);
                pstrcat(buf, buf_size, "(");
                sa = s->next;
                当 (sa != NULL) {
                        type_to_str(buf1, 求长度(buf1), &sa->type, NULL);
                        pstrcat(buf, buf_size, buf1);
                        sa = sa->next;
                        如 (sa)
                                pstrcat(buf, buf_size, ", ");
                }
                pstrcat(buf, buf_size, ")");
                跳转 no_var;
        事例 VT_PTR:
                s = type->ref;
                如 (t & VT_ARRAY) {
                        snprintf(buf1, 求长度(buf1), "%s[%d]", varstr ? varstr : "", s->c);
                        type_to_str(buf, buf_size, &s->type, buf1);
                        跳转 no_var;
                }
                pstrcpy(buf1, 求长度(buf1), "*");
                如 (t & VT_CONSTANT)
                        pstrcat(buf1, buf_size, "const ");
                如 (t & VT_VOLATILE)
                        pstrcat(buf1, buf_size, "volatile ");
                如 (varstr)
                        pstrcat(buf1, 求长度(buf1), varstr);
                type_to_str(buf, buf_size, &s->type, buf1);
                跳转 no_var;
        }
        如 (varstr) {
                pstrcat(buf, buf_size, " ");
                pstrcat(buf, buf_size, varstr);
        }
 no_var: ;
}

/* verify type compatibility to store vtop in 'dt' type, and generate
   casts if needed. */
静态 空 gen_assign_cast(CType *dt)
{
        CType *st, *type1, *type2, tmp_type1, tmp_type2;
        字 buf1[256], buf2[256];
        整 dbt, sbt;

        st = &vtop->type; /* source type */
        dbt = dt->t & VT_BTYPE;
        sbt = st->t & VT_BTYPE;
        如 (sbt == VT_VOID || dbt == VT_VOID) {
                如 (sbt == VT_VOID && dbt == VT_VOID)
                        ; /*
                          It is Ok if both are void
                          A test program:
                                void func1() {}
                                void func2() {
                                  return func1();
                                }
                          gcc accepts this program
                          */
                另
                        tcc_error("cannot cast from/to void");
        }
        如 (dt->t & VT_CONSTANT)
                tcc_warning("assignment of read-only location");
        转接(dbt) {
        事例 VT_PTR:
                /* special cases for pointers */
                /* '0' can also be a pointer */
                如 (is_null_pointer(vtop))
                        跳转 type_ok;
                /* accept implicit pointer to integer cast with warning */
                如 (is_integer_btype(sbt)) {
                        tcc_warning("assignment makes pointer from integer without a cast");
                        跳转 type_ok;
                }
                type1 = pointed_type(dt);
                /* a function is implicitly a function pointer */
                如 (sbt == VT_FUNC) {
                        如 ((type1->t & VT_BTYPE) != VT_VOID &&
                                !is_compatible_types(pointed_type(dt), st))
                                tcc_warning("assignment from incompatible pointer type");
                        跳转 type_ok;
                }
                如 (sbt != VT_PTR)
                        跳转 error;
                type2 = pointed_type(st);
                如 ((type1->t & VT_BTYPE) == VT_VOID ||
                        (type2->t & VT_BTYPE) == VT_VOID) {
                        /* void * can match anything */
                } 另 {
                        //printf("types %08x %08x\n", type1->t, type2->t);
                        /* exact type match, except for qualifiers */
                        如 (!is_compatible_unqualified_types(type1, type2)) {
                                /* Like GCC don't warn by default for merely changes
                                   in pointer target signedness.  Do warn for different
                                   base types, though, in particular for unsigned enums
                                   and signed int targets.  */
                                如 ((type1->t & VT_BTYPE) != (type2->t & VT_BTYPE)
                                        || IS_ENUM(type1->t) || IS_ENUM(type2->t)
                                        )
                                        tcc_warning("assignment from incompatible pointer type");
                        }
                }
                /* check const and volatile */
                如 ((!(type1->t & VT_CONSTANT) && (type2->t & VT_CONSTANT)) ||
                        (!(type1->t & VT_VOLATILE) && (type2->t & VT_VOLATILE)))
                        tcc_warning("assignment discards qualifiers from pointer target type");
                跳出;
        事例 VT_BYTE:
        事例 VT_SHORT:
        事例 VT_INT:
        事例 VT_LLONG:
                如 (sbt == VT_PTR || sbt == VT_FUNC) {
                        tcc_warning("assignment makes integer from pointer without a cast");
                } 另 如 (sbt == VT_STRUCT) {
                        跳转 case_VT_STRUCT;
                }
                /* XXX: more tests */
                跳出;
        事例 VT_STRUCT:
        case_VT_STRUCT:
                tmp_type1 = *dt;
                tmp_type2 = *st;
                tmp_type1.t &= ~(VT_CONSTANT | VT_VOLATILE);
                tmp_type2.t &= ~(VT_CONSTANT | VT_VOLATILE);
                如 (!is_compatible_types(&tmp_type1, &tmp_type2)) {
                error:
                        type_to_str(buf1, 求长度(buf1), st, NULL);
                        type_to_str(buf2, 求长度(buf2), dt, NULL);
                        tcc_error("cannot cast '%s' to '%s'", buf1, buf2);
                }
                跳出;
        }
 type_ok:
        gen_cast(dt);
}

/* store vtop in lvalue pushed on stack */
ST_FUNC 空 vstore(空)
{
        整 sbt, dbt, ft, r, t, size, align, bit_size, bit_pos, rc, delayed_cast;

        ft = vtop[-1].type.t;
        sbt = vtop->type.t & VT_BTYPE;
        dbt = ft & VT_BTYPE;
        如 ((((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
                 (sbt == VT_INT && dbt == VT_SHORT))
                && !(vtop->type.t & VT_BITFIELD)) {
                /* optimize char/short casts */
                delayed_cast = VT_MUSTCAST;
                vtop->type.t = ft & VT_TYPE;
                /* XXX: factorize */
                如 (ft & VT_CONSTANT)
                        tcc_warning("assignment of read-only location");
        } 另 {
                delayed_cast = 0;
                如 (!(ft & VT_BITFIELD))
                        gen_assign_cast(&vtop[-1].type);
        }

        如 (sbt == VT_STRUCT) {
                /* if structure, only generate pointer */
                /* structure assignment : generate memcpy */
                /* XXX: optimize if small size */
                        size = type_size(&vtop->type, &align);

                        /* destination */
                        vswap();
                        vtop->type.t = VT_PTR;
                        gaddrof();

                        /* address of memcpy() */
#如定义 TCC_ARM_EABI
                        如(!(align & 7))
                                vpush_global_sym(&func_old_type, TOK_memcpy8);
                        另 如(!(align & 3))
                                vpush_global_sym(&func_old_type, TOK_memcpy4);
                        另
#了如
                        /* Use memmove, rather than memcpy, as dest and src may be same: */
                        vpush_global_sym(&func_old_type, TOK_memmove);

                        vswap();
                        /* source */
                        vpushv(vtop - 2);
                        vtop->type.t = VT_PTR;
                        gaddrof();
                        /* type size */
                        vpushi(size);
                        gfunc_call(3);

                /* leave source on stack */
        } 另 如 (ft & VT_BITFIELD) {
                /* bitfield store handling */

                /* save lvalue as expression result (example: s.b = s.a = n;) */
                vdup(), vtop[-1] = vtop[-2];

                bit_pos = BIT_POS(ft);
                bit_size = BIT_SIZE(ft);
                /* remove bit field info to avoid loops */
                vtop[-1].type.t = ft & ~VT_STRUCT_MASK;

                如 ((ft & VT_BTYPE) == VT_BOOL) {
                        gen_cast(&vtop[-1].type);
                        vtop[-1].type.t = (vtop[-1].type.t & ~VT_BTYPE) | (VT_BYTE | VT_UNSIGNED);
                }

                r = adjust_bf(vtop - 1, bit_pos, bit_size);
                如 (r == VT_STRUCT) {
                        gen_cast_s((ft & VT_BTYPE) == VT_LLONG ? VT_LLONG : VT_INT);
                        store_packed_bf(bit_pos, bit_size);
                } 另 {
                        无符 长 长 mask = (1ULL << bit_size) - 1;
                        如 ((ft & VT_BTYPE) != VT_BOOL) {
                                /* mask source */
                                如 ((vtop[-1].type.t & VT_BTYPE) == VT_LLONG)
                                        vpushll(mask);
                                另
                                        vpushi((无符)mask);
                                gen_op('&');
                        }
                        /* shift source */
                        vpushi(bit_pos);
                        gen_op(TOK_SHL);
                        vswap();
                        /* duplicate destination */
                        vdup();
                        vrott(3);
                        /* load destination, mask and or with source */
                        如 ((vtop->type.t & VT_BTYPE) == VT_LLONG)
                                vpushll(~(mask << bit_pos));
                        另
                                vpushi(~((无符)mask << bit_pos));
                        gen_op('&');
                        gen_op('|');
                        /* store result */
                        vstore();
                        /* ... and discard */
                        vpop();
                }
        } 另 {
#如定义 CONFIG_TCC_BCHECK
                        /* bound check case */
                        如 (vtop[-1].r & VT_MUSTBOUND) {
                                vswap();
                                gbound();
                                vswap();
                        }
#了如
                        rc = RC_INT;
                        如 (is_float(ft)) {
                                rc = RC_FLOAT;
#如定义 TCC_TARGET_X86_64
                                如 ((ft & VT_BTYPE) == VT_LDOUBLE) {
                                        rc = RC_ST0;
                                } 另 如 ((ft & VT_BTYPE) == VT_QFLOAT) {
                                        rc = RC_FRET;
                                }
#了如
                        }
                        r = gv(rc);  /* generate value */
                        /* if lvalue was saved on stack, must read it */
                        如 ((vtop[-1].r & VT_VALMASK) == VT_LLOCAL) {
                                SValue sv;
                                t = get_reg(RC_INT);
#如 PTR_SIZE == 8
                                sv.type.t = VT_PTR;
#另
                                sv.type.t = VT_INT;
#了如
                                sv.r = VT_LOCAL | VT_LVAL;
                                sv.c.i = vtop[-1].c.i;
                                load(t, &sv);
                                vtop[-1].r = t | VT_LVAL;
                        }
                        /* two word case handling : store second register at word + 4 (or +8 for x86-64)  */
#如 PTR_SIZE == 8
                        如 (((ft & VT_BTYPE) == VT_QLONG) || ((ft & VT_BTYPE) == VT_QFLOAT)) {
                                整 addr_type = VT_LLONG, load_size = 8, load_type = ((vtop->type.t & VT_BTYPE) == VT_QLONG) ? VT_LLONG : VT_DOUBLE;
#另
                        如 ((ft & VT_BTYPE) == VT_LLONG) {
                                整 addr_type = VT_INT, load_size = 4, load_type = VT_INT;
#了如
                                vtop[-1].type.t = load_type;
                                store(r, vtop - 1);
                                vswap();
                                /* convert to int to increment easily */
                                vtop->type.t = addr_type;
                                gaddrof();
                                vpushi(load_size);
                                gen_op('+');
                                vtop->r |= VT_LVAL;
                                vswap();
                                vtop[-1].type.t = load_type;
                                /* XXX: it works because r2 is spilled last ! */
                                store(vtop->r2, vtop - 1);
                        } 另 {
                                store(r, vtop - 1);
                        }

                vswap();
                vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
                vtop->r |= delayed_cast;
        }
}

/* post defines POST/PRE add. c is the token ++ or -- */
ST_FUNC 空 inc(整 post, 整 c)
{
        test_lvalue();
        vdup(); /* save lvalue */
        如 (post) {
                gv_dup(); /* duplicate value */
                vrotb(3);
                vrotb(3);
        }
        /* add constant */
        vpushi(c - TOK_MID);
        gen_op('+');
        vstore(); /* store value */
        如 (post)
                vpop(); /* if post op, return saved value */
}

ST_FUNC 空 parse_mult_str (CString *astr, 不变 字 *msg)
{
        /* read the string */
        如 (tok != TOK_STR)
                expect(msg);
        cstr_new(astr);
        当 (tok == TOK_STR) {
                /* XXX: add \0 handling too ? */
                cstr_cat(astr, tokc.str.data, -1);
                next();
        }
        cstr_ccat(astr, '\0');
}

/* If I is >= 1 and a power of two, returns log2(i)+1.
   If I is 0 returns 0.  */
静态 整 exact_log2p1(整 i)
{
        整 ret;
        如 (!i)
                返回 0;
        对于 (ret = 1; i >= 1 << 8; ret += 8)
                i >>= 8;
        如 (i >= 1 << 4)
                ret += 4, i >>= 4;
        如 (i >= 1 << 2)
                ret += 2, i >>= 2;
        如 (i >= 1 << 1)
                ret++;
        返回 ret;
}

/* Parse __attribute__((...)) GNUC extension. */
静态 空 parse_attribute(AttributeDef *ad)
{
        整 t, n;
        CString astr;

redo:
        如 (tok != TOK_ATTRIBUTE1 && tok != TOK_ATTRIBUTE2 && tok != TOK_ATTRIBUTE1_CN && tok != TOK_ATTRIBUTE2_CN)
                返回;
        next();
        skip('(');
        skip('(');
        当 (tok != ')') {
                如 (tok < TOK_IDENT)
                        expect("attribute name");
                t = tok;
                next();
                转接(t) {
                事例 TOK_SECTION1:
                事例 TOK_SECTION2:
                        skip('(');
                        parse_mult_str(&astr, "section name");
                        ad->section = find_section(tcc_state, (字 *)astr.data);
                        skip(')');
                        cstr_free(&astr);
                        跳出;
                事例 TOK_ALIAS1:
                事例 TOK_ALIAS2:
                        skip('(');
                        parse_mult_str(&astr, "alias(\"target\")");
                        ad->alias_target = /* save string as token, for later */
                          tok_alloc((字*)astr.data, astr.size-1)->tok;
                        skip(')');
                        cstr_free(&astr);
                        跳出;
                事例 TOK_VISIBILITY1:
                事例 TOK_VISIBILITY2:
                        skip('(');
                        parse_mult_str(&astr,
                                                   "visibility(\"default|hidden|internal|protected\")");
                        如 (!strcmp (astr.data, "default"))
                                ad->a.visibility = STV_DEFAULT;
                        另 如 (!strcmp (astr.data, "hidden"))
                                ad->a.visibility = STV_HIDDEN;
                        另 如 (!strcmp (astr.data, "internal"))
                                ad->a.visibility = STV_INTERNAL;
                        另 如 (!strcmp (astr.data, "protected"))
                                ad->a.visibility = STV_PROTECTED;
                        另
                                expect("visibility(\"default|hidden|internal|protected\")");
                        skip(')');
                        cstr_free(&astr);
                        跳出;
                事例 TOK_ALIGNED1:
                事例 TOK_ALIGNED2:
                        如 (tok == '(') {
                                next();
                                n = expr_const();
                                如 (n <= 0 || (n & (n - 1)) != 0)
                                        tcc_error("alignment must be a positive power of two");
                                skip(')');
                        } 另 {
                                n = MAX_ALIGN;
                        }
                        ad->a.aligned = exact_log2p1(n);
                        如 (n != 1 << (ad->a.aligned - 1))
                                tcc_error("alignment of %d is larger than implemented", n);
                        跳出;
                事例 TOK_PACKED1:
                事例 TOK_PACKED2:
                        ad->a.packed = 1;
                        跳出;
                事例 TOK_WEAK1:
                事例 TOK_WEAK2:
                        ad->a.weak = 1;
                        跳出;
                事例 TOK_UNUSED1:
                事例 TOK_UNUSED2:
                        /* currently, no need to handle it because tcc does not
                           track unused objects */
                        跳出;
                事例 TOK_NORETURN1:
                事例 TOK_NORETURN2:
                        /* currently, no need to handle it because tcc does not
                           track unused objects */
                        跳出;
                事例 TOK_CDECL1:
                事例 TOK_CDECL2:
                事例 TOK_CDECL3:
                        ad->f.func_call = FUNC_CDECL;
                        跳出;
                事例 TOK_STDCALL1:
                事例 TOK_STDCALL2:
                事例 TOK_STDCALL3:
                        ad->f.func_call = FUNC_STDCALL;
                        跳出;
#如定义 TCC_TARGET_I386
                事例 TOK_REGPARM1:
                事例 TOK_REGPARM2:
                        skip('(');
                        n = expr_const();
                        如 (n > 3)
                                n = 3;
                        另 如 (n < 0)
                                n = 0;
                        如 (n > 0)
                                ad->f.func_call = FUNC_FASTCALL1 + n - 1;
                        skip(')');
                        跳出;
                事例 TOK_FASTCALL1:
                事例 TOK_FASTCALL2:
                事例 TOK_FASTCALL3:
                        ad->f.func_call = FUNC_FASTCALLW;
                        跳出;
#了如
                事例 TOK_MODE:
                        skip('(');
                        转接(tok) {
                                事例 TOK_MODE_DI:
                                        ad->attr_mode = VT_LLONG + 1;
                                        跳出;
                                事例 TOK_MODE_QI:
                                        ad->attr_mode = VT_BYTE + 1;
                                        跳出;
                                事例 TOK_MODE_HI:
                                        ad->attr_mode = VT_SHORT + 1;
                                        跳出;
                                事例 TOK_MODE_SI:
                                事例 TOK_MODE_word:
                                        ad->attr_mode = VT_INT + 1;
                                        跳出;
                                缺省:
                                        tcc_warning("__mode__(%s) not supported\n", get_tok_str(tok, NULL));
                                        跳出;
                        }
                        next();
                        skip(')');
                        跳出;
                事例 TOK_DLLEXPORT:
                        ad->a.dllexport = 1;
                        跳出;
                事例 TOK_DLLIMPORT:
                        ad->a.dllimport = 1;
                        跳出;
                缺省:
                        如 (tcc_state->warn_unsupported)
                                tcc_warning("'%s' attribute ignored", get_tok_str(t, NULL));
                        /* skip parameters */
                        如 (tok == '(') {
                                整 parenthesis = 0;
                                运行 {
                                        如 (tok == '(')
                                                parenthesis++;
                                        另 如 (tok == ')')
                                                parenthesis--;
                                        next();
                                } 当 (parenthesis && tok != -1);
                        }
                        跳出;
                }
                如 (tok != ',')
                        跳出;
                next();
        }
        skip(')');
        skip(')');
        跳转 redo;
}

静态 Sym * find_field (CType *type, 整 v)
{
        Sym *s = type->ref;
        v |= SYM_FIELD;
        当 ((s = s->next) != NULL) {
                如 ((s->v & SYM_FIELD) &&
                        (s->type.t & VT_BTYPE) == VT_STRUCT &&
                        (s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM) {
                        Sym *ret = find_field (&s->type, v);
                        如 (ret)
                                返回 ret;
                }
                如 (s->v == v)
                        跳出;
        }
        返回 s;
}

静态 空 struct_add_offset (Sym *s, 整 offset)
{
        当 ((s = s->next) != NULL) {
                如 ((s->v & SYM_FIELD) &&
                        (s->type.t & VT_BTYPE) == VT_STRUCT &&
                        (s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM) {
                        struct_add_offset(s->type.ref, offset);
                } 另
                        s->c += offset;
        }
}

静态 空 struct_layout(CType *type, AttributeDef *ad)
{
        整 size, align, maxalign, offset, c, bit_pos, bit_size;
        整 packed, a, bt, prevbt, prev_bit_size;
        整 pcc = !tcc_state->ms_bitfields;
        整 pragma_pack = *tcc_state->pack_stack_ptr;
        Sym *f;

        maxalign = 1;
        offset = 0;
        c = 0;
        bit_pos = 0;
        prevbt = VT_STRUCT; /* make it never match */
        prev_bit_size = 0;

//#定义 BF_DEBUG

        对于 (f = type->ref->next; f; f = f->next) {
                如 (f->type.t & VT_BITFIELD)
                        bit_size = BIT_SIZE(f->type.t);
                另
                        bit_size = -1;
                size = type_size(&f->type, &align);
                a = f->a.aligned ? 1 << (f->a.aligned - 1) : 0;
                packed = 0;

                如 (pcc && bit_size == 0) {
                        /* in pcc mode, packing does not affect zero-width bitfields */

                } 另 {
                        /* in pcc mode, attribute packed overrides if set. */
                        如 (pcc && (f->a.packed || ad->a.packed))
                                align = packed = 1;

                        /* pragma pack overrides align if lesser and packs bitfields always */
                        如 (pragma_pack) {
                                packed = 1;
                                如 (pragma_pack < align)
                                        align = pragma_pack;
                                /* in pcc mode pragma pack also overrides individual align */
                                如 (pcc && pragma_pack < a)
                                        a = 0;
                        }
                }
                /* some individual align was specified */
                如 (a)
                        align = a;

                如 (type->ref->type.t == VT_UNION) {
                        如 (pcc && bit_size >= 0)
                                size = (bit_size + 7) >> 3;
                        offset = 0;
                        如 (size > c)
                                c = size;

                } 另 如 (bit_size < 0) {
                        如 (pcc)
                                c += (bit_pos + 7) >> 3;
                        c = (c + align - 1) & -align;
                        offset = c;
                        如 (size > 0)
                                c += size;
                        bit_pos = 0;
                        prevbt = VT_STRUCT;
                        prev_bit_size = 0;

                } 另 {
                        /* A bit-field.  Layout is more complicated.  There are two
                           options: PCC (GCC) compatible and MS compatible */
                        如 (pcc) {
                                /* In PCC layout a bit-field is placed adjacent to the
                                   preceding bit-fields, except if:
                                   - it has zero-width
                                   - an individual alignment was given
                                   - it would overflow its base type container and
                                         there is no packing */
                                如 (bit_size == 0) {
                        new_field:
                                        c = (c + ((bit_pos + 7) >> 3) + align - 1) & -align;
                                        bit_pos = 0;
                                } 另 如 (f->a.aligned) {
                                        跳转 new_field;
                                } 另 如 (!packed) {
                                        整 a8 = align * 8;
                                        整 ofs = ((c * 8 + bit_pos) % a8 + bit_size + a8 - 1) / a8;
                                        如 (ofs > size / align)
                                                跳转 new_field;
                                }

                                /* in pcc mode, long long bitfields have type int if they fit */
                                如 (size == 8 && bit_size <= 32)
                                        f->type.t = (f->type.t & ~VT_BTYPE) | VT_INT, size = 4;

                                当 (bit_pos >= align * 8)
                                        c += align, bit_pos -= align * 8;
                                offset = c;

                                /* In PCC layout named bit-fields influence the alignment
                                   of the containing struct using the base types alignment,
                                   except for packed fields (which here have correct align).  */
                                如 (f->v & SYM_FIRST_ANOM
                                        // && bit_size // ??? gcc on ARM/rpi does that
                                        )
                                        align = 1;

                        } 另 {
                                bt = f->type.t & VT_BTYPE;
                                如 ((bit_pos + bit_size > size * 8)
                                        || (bit_size > 0) == (bt != prevbt)
                                        ) {
                                        c = (c + align - 1) & -align;
                                        offset = c;
                                        bit_pos = 0;
                                        /* In MS bitfield mode a bit-field run always uses
                                           at least as many bits as the underlying type.
                                           To start a new run it's also required that this
                                           or the last bit-field had non-zero width.  */
                                        如 (bit_size || prev_bit_size)
                                                c += size;
                                }
                                /* In MS layout the records alignment is normally
                                   influenced by the field, except for a zero-width
                                   field at the start of a run (but by further zero-width
                                   fields it is again).  */
                                如 (bit_size == 0 && prevbt != bt)
                                        align = 1;
                                prevbt = bt;
                                prev_bit_size = bit_size;
                        }

                        f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
                                                | (bit_pos << VT_STRUCT_SHIFT);
                        bit_pos += bit_size;
                }
                如 (align > maxalign)
                        maxalign = align;

#如定义 BF_DEBUG
                printf("set field %s offset %-2d size %-2d align %-2d",
                           get_tok_str(f->v & ~SYM_FIELD, NULL), offset, size, align);
                如 (f->type.t & VT_BITFIELD) {
                        printf(" pos %-2d bits %-2d",
                                        BIT_POS(f->type.t),
                                        BIT_SIZE(f->type.t)
                                        );
                }
                printf("\n");
#了如

                如 (f->v & SYM_FIRST_ANOM && (f->type.t & VT_BTYPE) == VT_STRUCT) {
                        Sym *ass;
                        /* An anonymous struct/union.  Adjust member offsets
                           to reflect the real offset of our containing struct.
                           Also set the offset of this anon member inside
                           the outer struct to be zero.  Via this it
                           works when accessing the field offset directly
                           (from base object), as well as when recursing
                           members in initializer handling.  */
                        整 v2 = f->type.ref->v;
                        如 (!(v2 & SYM_FIELD) &&
                                (v2 & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
                                Sym **pps;
                                /* This happens only with MS extensions.  The
                                   anon member has a named struct type, so it
                                   potentially is shared with other references.
                                   We need to unshare members so we can modify
                                   them.  */
                                ass = f->type.ref;
                                f->type.ref = sym_push(anon_sym++ | SYM_FIELD,
                                                                           &f->type.ref->type, 0,
                                                                           f->type.ref->c);
                                pps = &f->type.ref->next;
                                当 ((ass = ass->next) != NULL) {
                                        *pps = sym_push(ass->v, &ass->type, 0, ass->c);
                                        pps = &((*pps)->next);
                                }
                                *pps = NULL;
                        }
                        struct_add_offset(f->type.ref, offset);
                        f->c = 0;
                } 另 {
                        f->c = offset;
                }

                f->r = 0;
        }

        如 (pcc)
                c += (bit_pos + 7) >> 3;

        /* store size and alignment */
        a = bt = ad->a.aligned ? 1 << (ad->a.aligned - 1) : 1;
        如 (a < maxalign)
                a = maxalign;
        type->ref->r = a;
        如 (pragma_pack && pragma_pack < maxalign && 0 == pcc) {
                /* can happen if individual align for some member was given.  In
                   this case MSVC ignores maxalign when aligning the size */
                a = pragma_pack;
                如 (a < bt)
                        a = bt;
        }
        c = (c + a - 1) & -a;
        type->ref->c = c;

#如定义 BF_DEBUG
        printf("struct size %-2d align %-2d\n\n", c, a), fflush(stdout);
#了如

        /* check whether we can access bitfields by their type */
        对于 (f = type->ref->next; f; f = f->next) {
                整 s, px, cx, c0;
                CType t;

                如 (0 == (f->type.t & VT_BITFIELD))
                        继续;
                f->type.ref = f;
                f->auxtype = -1;
                bit_size = BIT_SIZE(f->type.t);
                如 (bit_size == 0)
                        继续;
                bit_pos = BIT_POS(f->type.t);
                size = type_size(&f->type, &align);
                如 (bit_pos + bit_size <= size * 8 && f->c + size <= c)
                        继续;

                /* try to access the field using a differnt type */
                c0 = -1, s = align = 1;
                对于 (;;) {
                        px = f->c * 8 + bit_pos;
                        cx = (px >> 3) & -align;
                        px = px - (cx << 3);
                        如 (c0 == cx)
                                跳出;
                        s = (px + bit_size + 7) >> 3;
                        如 (s > 4) {
                                t.t = VT_LLONG;
                        } 另 如 (s > 2) {
                                t.t = VT_INT;
                        } 另 如 (s > 1) {
                                t.t = VT_SHORT;
                        } 另 {
                                t.t = VT_BYTE;
                        }
                        s = type_size(&t, &align);
                        c0 = cx;
                }

                如 (px + bit_size <= s * 8 && cx + s <= c) {
                        /* update offset and bit position */
                        f->c = cx;
                        bit_pos = px;
                        f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
                                                | (bit_pos << VT_STRUCT_SHIFT);
                        如 (s != size)
                                f->auxtype = t.t;
#如定义 BF_DEBUG
                        printf("FIX field %s offset %-2d size %-2d align %-2d "
                                "pos %-2d bits %-2d\n",
                                get_tok_str(f->v & ~SYM_FIELD, NULL),
                                cx, s, align, px, bit_size);
#了如
                } 另 {
                        /* fall back to load/store single-byte wise */
                        f->auxtype = VT_STRUCT;
#如定义 BF_DEBUG
                        printf("FIX field %s : load byte-wise\n",
                                 get_tok_str(f->v & ~SYM_FIELD, NULL));
#了如
                }
        }
}

/* enum/struct/union declaration. u is VT_ENUM/VT_STRUCT/VT_UNION */
静态 空 struct_decl(CType *type, 整 u)
{
        整 v, c, size, align, flexible;
        整 bit_size, bsize, bt;
        Sym *s, *ss, **ps;
        AttributeDef ad, ad1;
        CType type1, btype;

        memset(&ad, 0, 求长度 ad);
        next();
        parse_attribute(&ad);
        如 (tok != '{') {
                v = tok;
                next();
                /* struct already defined ? return it */
                如 (v < TOK_IDENT)
                        expect("struct/union/enum name");
                s = struct_find(v);
                如 (s && (s->sym_scope == local_scope || tok != '{')) {
                        如 (u == s->type.t)
                                跳转 do_decl;
                        如 (u == VT_ENUM && IS_ENUM(s->type.t))
                                跳转 do_decl;
                        tcc_error("redefinition of '%s'", get_tok_str(v, NULL));
                }
        } 另 {
                v = anon_sym++;
        }
        /* Record the original enum/struct/union token.  */
        type1.t = u == VT_ENUM ? u | VT_INT | VT_UNSIGNED : u;
        type1.ref = NULL;
        /* we put an undefined size for struct/union */
        s = sym_push(v | SYM_STRUCT, &type1, 0, -1);
        s->r = 0; /* default alignment is zero as gcc */
do_decl:
        type->t = s->type.t;
        type->ref = s;

        如 (tok == '{') {
                next();
                如 (s->c != -1)
                        tcc_error("struct/union/enum already defined");
                /* cannot be empty */
                /* non empty enums are not allowed */
                ps = &s->next;
                如 (u == VT_ENUM) {
                        长 长 ll = 0, pl = 0, nl = 0;
                        CType t;
                        t.ref = s;
                        /* enum symbols have static storage */
                        t.t = VT_INT|VT_STATIC|VT_ENUM_VAL;
                        对于(;;) {
                                v = tok;
                                如 (v < TOK_UIDENT)
                                        expect("identifier");
                                ss = sym_find(v);
                                如 (ss && !local_stack)
                                        tcc_error("redefinition of enumerator '%s'",
                                                          get_tok_str(v, NULL));
                                next();
                                如 (tok == '=') {
                                        next();
                                        ll = expr_const64();
                                }
                                ss = sym_push(v, &t, VT_CONST, 0);
                                ss->enum_val = ll;
                                *ps = ss, ps = &ss->next;
                                如 (ll < nl)
                                        nl = ll;
                                如 (ll > pl)
                                        pl = ll;
                                如 (tok != ',')
                                        跳出;
                                next();
                                ll++;
                                /* NOTE: we accept a trailing comma */
                                如 (tok == '}')
                                        跳出;
                        }
                        skip('}');
                        /* set integral type of the enum */
                        t.t = VT_INT;
                        如 (nl >= 0) {
                                如 (pl != (无符)pl)
                                        t.t = VT_LLONG;
                                t.t |= VT_UNSIGNED;
                        } 另 如 (pl != (整)pl || nl != (整)nl)
                                t.t = VT_LLONG;
                        s->type.t = type->t = t.t | VT_ENUM;
                        s->c = 0;
                        /* set type for enum members */
                        对于 (ss = s->next; ss; ss = ss->next) {
                                ll = ss->enum_val;
                                如 (ll == (整)ll) /* default is int if it fits */
                                        继续;
                                如 (t.t & VT_UNSIGNED) {
                                        ss->type.t |= VT_UNSIGNED;
                                        如 (ll == (无符)ll)
                                                继续;
                                }
                                ss->type.t = (ss->type.t & ~VT_BTYPE) | VT_LLONG;
                        }
                } 另 {
                        c = 0;
                        flexible = 0;
                        当 (tok != '}') {
                                如 (!parse_btype(&btype, &ad1)) {
                                        skip(';');
                                        继续;
                                }
                                当 (1) {
                                        如 (flexible)
                                                tcc_error("flexible array member '%s' not at the end of struct",
                                                          get_tok_str(v, NULL));
                                        bit_size = -1;
                                        v = 0;
                                        type1 = btype;
                                        如 (tok != ':') {
                                                如 (tok != ';')
                                                        type_decl(&type1, &ad1, &v, TYPE_DIRECT);
                                                如 (v == 0) {
                                                        如 ((type1.t & VT_BTYPE) != VT_STRUCT)
                                                                expect("identifier");
                                                        另 {
                                                                整 v = btype.ref->v;
                                                                如 (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
                                                                        如 (tcc_state->ms_extensions == 0)
                                                                                expect("identifier");
                                                                }
                                                        }
                                                }
                                                如 (type_size(&type1, &align) < 0) {
                                                        如 ((u == VT_STRUCT) && (type1.t & VT_ARRAY) && c)
                                                                flexible = 1;
                                                        另
                                                                tcc_error("field '%s' has incomplete type",
                                                                          get_tok_str(v, NULL));
                                                }
                                                如 ((type1.t & VT_BTYPE) == VT_FUNC ||
                                                        (type1.t & VT_STORAGE))
                                                        tcc_error("invalid type for '%s'",
                                                                  get_tok_str(v, NULL));
                                        }
                                        如 (tok == ':') {
                                                next();
                                                bit_size = expr_const();
                                                /* XXX: handle v = 0 case for messages */
                                                如 (bit_size < 0)
                                                        tcc_error("negative width in bit-field '%s'",
                                                                  get_tok_str(v, NULL));
                                                如 (v && bit_size == 0)
                                                        tcc_error("zero width for bit-field '%s'",
                                                                  get_tok_str(v, NULL));
                                                parse_attribute(&ad1);
                                        }
                                        size = type_size(&type1, &align);
                                        如 (bit_size >= 0) {
                                                bt = type1.t & VT_BTYPE;
                                                如 (bt != VT_INT &&
                                                        bt != VT_BYTE &&
                                                        bt != VT_SHORT &&
                                                        bt != VT_BOOL &&
                                                        bt != VT_LLONG)
                                                        tcc_error("bitfields must have scalar type");
                                                bsize = size * 8;
                                                如 (bit_size > bsize) {
                                                        tcc_error("width of '%s' exceeds its type",
                                                                  get_tok_str(v, NULL));
                                                } 另 如 (bit_size == bsize
                                                                        && !ad.a.packed && !ad1.a.packed) {
                                                        /* no need for bit fields */
                                                        ;
                                                } 另 如 (bit_size == 64) {
                                                        tcc_error("field width 64 not implemented");
                                                } 另 {
                                                        type1.t = (type1.t & ~VT_STRUCT_MASK)
                                                                | VT_BITFIELD
                                                                | (bit_size << (VT_STRUCT_SHIFT + 6));
                                                }
                                        }
                                        如 (v != 0 || (type1.t & VT_BTYPE) == VT_STRUCT) {
                                                /* Remember we've seen a real field to check
                                                   for placement of flexible array member. */
                                                c = 1;
                                        }
                                        /* If member is a struct or bit-field, enforce
                                           placing into the struct (as anonymous).  */
                                        如 (v == 0 &&
                                                ((type1.t & VT_BTYPE) == VT_STRUCT ||
                                                 bit_size >= 0)) {
                                                v = anon_sym++;
                                        }
                                        如 (v) {
                                                ss = sym_push(v | SYM_FIELD, &type1, 0, 0);
                                                ss->a = ad1.a;
                                                *ps = ss;
                                                ps = &ss->next;
                                        }
                                        如 (tok == ';' || tok == TOK_EOF)
                                                跳出;
                                        skip(',');
                                }
                                skip(';');
                        }
                        skip('}');
                        parse_attribute(&ad);
                        struct_layout(type, &ad);
                }
        }
}

静态 空 sym_to_attr(AttributeDef *ad, Sym *s)
{
        如 (s->a.aligned && 0 == ad->a.aligned)
                ad->a.aligned = s->a.aligned;
        如 (s->f.func_call && 0 == ad->f.func_call)
                ad->f.func_call = s->f.func_call;
        如 (s->f.func_type && 0 == ad->f.func_type)
                ad->f.func_type = s->f.func_type;
        如 (s->a.packed)
                ad->a.packed = 1;
}

/* Add type qualifiers to a type. If the type is an array then the qualifiers
   are added to the element type, copied because it could be a typedef. */
静态 空 parse_btype_qualify(CType *type, 整 qualifiers)
{
        当 (type->t & VT_ARRAY) {
                type->ref = sym_push(SYM_FIELD, &type->ref->type, 0, type->ref->c);
                type = &type->ref->type;
        }
        type->t |= qualifiers;
}

/* return 0 if no type declaration. otherwise, return the basic type
   and skip it.
 */
静态 整 parse_btype(CType *type, AttributeDef *ad)
{
        整 t, u, bt, st, type_found, typespec_found, g;
        Sym *s;
        CType type1;

        memset(ad, 0, 求长度(AttributeDef));
        type_found = 0;
        typespec_found = 0;
        t = VT_INT;
        bt = st = -1;
        type->ref = NULL;

        当(1) {
                转接(tok) {
                事例 TOK_EXTENSION:
                        /* currently, we really ignore extension */
                        next();
                        继续;

                        /* basic types */
                事例 TOK_CHAR:
                事例 TOK_CHAR_CN:
                        u = VT_BYTE;
                basic_type:
                        next();
                basic_type1:
                        如 (u == VT_SHORT || u == VT_LONG) {
                                如 (st != -1 || (bt != -1 && bt != VT_INT))
                                        tmbt: tcc_error("too many basic types");
                                st = u;
                        } 另 {
                                如 (bt != -1 || (st != -1 && u != VT_INT))
                                        跳转 tmbt;
                                bt = u;
                        }
                        如 (u != VT_INT)
                                t = (t & ~VT_BTYPE) | u;
                        typespec_found = 1;
                        跳出;
                事例 TOK_VOID:
                事例 TOK_VOID_CN:
                        u = VT_VOID;
                        跳转 basic_type;
                事例 TOK_SHORT:
                事例 TOK_SHORT_CN:
                        u = VT_SHORT;
                        跳转 basic_type;
                事例 TOK_INT:
                事例 TOK_INT_CN:
                        u = VT_INT;
                        跳转 basic_type;
                事例 TOK_LONG:
                事例 TOK_LONG_CN:
                        如 ((t & VT_BTYPE) == VT_DOUBLE) {
#如未定义 TCC_TARGET_PE
                                t = (t & ~(VT_LONG | VT_BTYPE)) | VT_LDOUBLE;
#了如
                        } 另 如 (t &  VT_LONG) {
                                t = (t & ~(VT_LONG | VT_BTYPE)) | VT_LLONG;
                        } 另 {
                                u = VT_LONG;
                                跳转 basic_type;
                        }
                        next();
                        跳出;
#如定义 TCC_TARGET_ARM64
                事例 TOK_UINT128:
                事例 TOK_UINT128_CN:
                        /* GCC's __uint128_t appears in some Linux header files. Make it a
                           synonym for long double to get the size and alignment right. */
                        u = VT_LDOUBLE;
                        跳转 basic_type;
#了如
                事例 TOK_BOOL:
                事例 TOK_BOOL_CN:
                        u = VT_BOOL;
                        跳转 basic_type;
                事例 TOK_FLOAT:
                事例 TOK_FLOAT_CN:
                        u = VT_FLOAT;
                        跳转 basic_type;
                事例 TOK_DOUBLE:
                事例 TOK_DOUBLE_CN:
                        如 (t & VT_LONG) {
#如定义 TCC_TARGET_PE
                                t = (t & ~(VT_LONG | VT_BTYPE)) | VT_DOUBLE;
#另
                                t = (t & ~(VT_LONG | VT_BTYPE)) | VT_LDOUBLE;
#了如
                        } 另 {
                                u = VT_DOUBLE;
                                跳转 basic_type;
                        }
                        next();
                        跳出;
                事例 TOK_ENUM:
                事例 TOK_ENUM_CN:
                        struct_decl(&type1, VT_ENUM);
                basic_type2:
                        u = type1.t;
                        type->ref = type1.ref;
                        跳转 basic_type1;
                事例 TOK_STRUCT:
                事例 TOK_STRUCT_CN:
                        struct_decl(&type1, VT_STRUCT);
                        跳转 basic_type2;
                事例 TOK_UNION:
                事例 TOK_UNION_CN:
                        struct_decl(&type1, VT_UNION);
                        跳转 basic_type2;

                        /* type modifiers */
                事例 TOK_CONST1:
                事例 TOK_CONST2:
                事例 TOK_CONST3:
                事例 TOK_CONST1_CN:
                        type->t = t;
                        parse_btype_qualify(type, VT_CONSTANT);
                        t = type->t;
                        next();
                        跳出;
                事例 TOK_VOLATILE1:
                事例 TOK_VOLATILE2:
                事例 TOK_VOLATILE3:
                事例 TOK_VOLATILE1_CN:
                        type->t = t;
                        parse_btype_qualify(type, VT_VOLATILE);
                        t = type->t;
                        next();
                        跳出;
                事例 TOK_SIGNED1:
                事例 TOK_SIGNED2:
                事例 TOK_SIGNED3:
                事例 TOK_SIGNED1_CN:
                        如 ((t & (VT_DEFSIGN|VT_UNSIGNED)) == (VT_DEFSIGN|VT_UNSIGNED))
                                tcc_error("signed and unsigned modifier");
                        t |= VT_DEFSIGN;
                        next();
                        typespec_found = 1;
                        跳出;
                事例 TOK_REGISTER:
                事例 TOK_REGISTER_CN:
                事例 TOK_AUTO:
                事例 TOK_AUTO_CN:
                事例 TOK_RESTRICT1:
                事例 TOK_RESTRICT2:
                事例 TOK_RESTRICT3:
                事例 TOK_RESTRICT1_CN:
                        next();
                        跳出;
                事例 TOK_UNSIGNED:
                事例 TOK_UNSIGNED_CN:
                        如 ((t & (VT_DEFSIGN|VT_UNSIGNED)) == VT_DEFSIGN)
                                tcc_error("signed and unsigned modifier");
                        t |= VT_DEFSIGN | VT_UNSIGNED;
                        next();
                        typespec_found = 1;
                        跳出;

                        /* storage */
                事例 TOK_EXTERN:
                事例 TOK_EXTERN_CN:
                        g = VT_EXTERN;
                        跳转 storage;
                事例 TOK_STATIC:
                事例 TOK_STATIC_CN:
                        g = VT_STATIC;
                        跳转 storage;
                事例 TOK_TYPEDEF:
                事例 TOK_TYPEDEF_CN:
                        g = VT_TYPEDEF;
                        跳转 storage;
                storage:
                        如 (t & (VT_EXTERN|VT_STATIC|VT_TYPEDEF) & ~g)
                                tcc_error("multiple storage classes");
                        t |= g;
                        next();
                        跳出;
                事例 TOK_INLINE1:
                事例 TOK_INLINE2:
                事例 TOK_INLINE3:
                事例 TOK_INLINE1_CN:
                        t |= VT_INLINE;
                        next();
                        跳出;

                        /* GNUC attribute */
                事例 TOK_ATTRIBUTE1:
                事例 TOK_ATTRIBUTE2:
                事例 TOK_ATTRIBUTE1_CN:
                事例 TOK_ATTRIBUTE2_CN:
                        parse_attribute(ad);
                        如 (ad->attr_mode) {
                                u = ad->attr_mode -1;
                                t = (t & ~VT_BTYPE) | u;
                        }
                        跳出;
                        /* GNUC typeof */
                事例 TOK_TYPEOF1:
                事例 TOK_TYPEOF2:
                事例 TOK_TYPEOF3:
                事例 TOK_TYPEOF1_CN:
                        next();
                        parse_expr_type(&type1);
                        /* remove all storage modifiers except typedef */
                        type1.t &= ~(VT_STORAGE&~VT_TYPEDEF);
                        如 (type1.ref)
                                sym_to_attr(ad, type1.ref);
                        跳转 basic_type2;
                缺省:
                        如 (typespec_found)
                                跳转 the_end;
                        s = sym_find(tok);
                        如 (!s || !(s->type.t & VT_TYPEDEF))
                                跳转 the_end;
                        t &= ~VT_BTYPE;
                        u = t & ~(VT_CONSTANT | VT_VOLATILE), t ^= u;
                        type->t = (s->type.t & ~VT_TYPEDEF) | u;
                        type->ref = s->type.ref;
                        如 (t)
                                parse_btype_qualify(type, t);
                        t = type->t;
                        /* get attributes from typedef */
                        sym_to_attr(ad, s);
                        next();
                        typespec_found = 1;
                        st = bt = -2;
                        跳出;
                }
                type_found = 1;
        }
the_end:
        如 (tcc_state->char_is_unsigned) {
                如 ((t & (VT_DEFSIGN|VT_BTYPE)) == VT_BYTE)
                        t |= VT_UNSIGNED;
        }

        /* long is never used as type */
        如 (t & VT_LONG)
#如 PTR_SIZE == 8 && !已定义 TCC_TARGET_PE
                t = (t & ~VT_BTYPE) | VT_LLONG;
#另
                t = (t & ~VT_BTYPE) | VT_INT;
#了如
        type->t = t;
        返回 type_found;
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
静态 内联 空 convert_parameter_type(CType *pt)
{
        /* remove const and volatile qualifiers (XXX: const could be used
           to indicate a const function parameter */
        pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
        /* array must be transformed to pointer according to ANSI C */
        pt->t &= ~VT_ARRAY;
        如 ((pt->t & VT_BTYPE) == VT_FUNC) {
                mk_pointer(pt);
        }
}

ST_FUNC 空 parse_asm_str(CString *astr)
{
        skip('(');
        parse_mult_str(astr, "string constant");
}

/* Parse an asm label and return the token */
静态 整 asm_label_instr(空)
{
        整 v;
        CString astr;

        next();
        parse_asm_str(&astr);
        skip(')');
#如定义 ASM_DEBUG
        printf("asm_alias: \"%s\"\n", (字 *)astr.data);
#了如
        v = tok_alloc(astr.data, astr.size - 1)->tok;
        cstr_free(&astr);
        返回 v;
}

静态 整 post_type(CType *type, AttributeDef *ad, 整 storage, 整 td)
{
        整 n, l, t1, arg_size, align;
        Sym **plast, *s, *first;
        AttributeDef ad1;
        CType pt;

        如 (tok == '(') {
                /* function type, or recursive declarator (return if so) */
                next();
                如 (td && !(td & TYPE_ABSTRACT))
                        返回 0;
                如 (tok == ')')
                        l = 0;
                另 如 (parse_btype(&pt, &ad1))
                        l = FUNC_NEW;
                另 如 (td)
                        返回 0;
                另
                        l = FUNC_OLD;
                first = NULL;
                plast = &first;
                arg_size = 0;
                如 (l) {
                        对于(;;) {
                                /* read param name and compute offset */
                                如 (l != FUNC_OLD) {
                                        如 ((pt.t & VT_BTYPE) == VT_VOID && tok == ')')
                                                跳出;
                                        type_decl(&pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                                        如 ((pt.t & VT_BTYPE) == VT_VOID)
                                                tcc_error("parameter declared as void");
                                        arg_size += (type_size(&pt, &align) + PTR_SIZE - 1) / PTR_SIZE;
                                } 另 {
                                        n = tok;
                                        如 (n < TOK_UIDENT)
                                                expect("identifier");
                                        pt.t = VT_VOID; /* invalid type */
                                        next();
                                }
                                convert_parameter_type(&pt);
                                s = sym_push(n | SYM_FIELD, &pt, 0, 0);
                                *plast = s;
                                plast = &s->next;
                                如 (tok == ')')
                                        跳出;
                                skip(',');
                                如 (l == FUNC_NEW && tok == TOK_DOTS) {
                                        l = FUNC_ELLIPSIS;
                                        next();
                                        跳出;
                                }
                                如 (l == FUNC_NEW && !parse_btype(&pt, &ad1))
                                        tcc_error("invalid type");
                        }
                } 另
                        /* if no parameters, then old type prototype */
                        l = FUNC_OLD;
                skip(')');
                /* NOTE: const is ignored in returned type as it has a special
                   meaning in gcc / C++ */
                type->t &= ~VT_CONSTANT;
                /* some ancient pre-K&R C allows a function to return an array
                   and the array brackets to be put after the arguments, such
                   that "int c()[]" means something like "int[] c()" */
                如 (tok == '[') {
                        next();
                        skip(']'); /* only handle simple "[]" */
                        mk_pointer(type);
                }
                /* we push a anonymous symbol which will contain the function prototype */
                ad->f.func_args = arg_size;
                ad->f.func_type = l;
                s = sym_push(SYM_FIELD, type, 0, 0);
                s->a = ad->a;
                s->f = ad->f;
                s->next = first;
                type->t = VT_FUNC;
                type->ref = s;
        } 另 如 (tok == '[') {
                整 saved_nocode_wanted = nocode_wanted;
                /* array definition */
                next();
                如 (tok == TOK_RESTRICT1 || tok == TOK_RESTRICT1_CN)
                        next();
                n = -1;
                t1 = 0;
                如 (tok != ']') {
                        如 (!local_stack || (storage & VT_STATIC))
                                vpushi(expr_const());
                        另 {
                                /* VLAs (which can only happen with local_stack && !VT_STATIC)
                                   length must always be evaluated, even under nocode_wanted,
                                   so that its size slot is initialized (e.g. under sizeof
                                   or typeof).  */
                                nocode_wanted = 0;
                                gexpr();
                        }
                        如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                                n = vtop->c.i;
                                如 (n < 0)
                                        tcc_error("invalid array size");
                        } 另 {
                                如 (!is_integer_btype(vtop->type.t & VT_BTYPE))
                                        tcc_error("size of variable length array should be an integer");
                                t1 = VT_VLA;
                        }
                }
                skip(']');
                /* parse next post type */
                post_type(type, ad, storage, 0);
                如 (type->t == VT_FUNC)
                        tcc_error("declaration of an array of functions");
                t1 |= type->t & VT_VLA;

                如 (t1 & VT_VLA) {
                        loc -= type_size(&int_type, &align);
                        loc &= -align;
                        n = loc;

                        vla_runtime_type_size(type, &align);
                        gen_op('*');
                        vset(&int_type, VT_LOCAL|VT_LVAL, n);
                        vswap();
                        vstore();
                }
                如 (n != -1)
                        vpop();
                nocode_wanted = saved_nocode_wanted;

                /* we push an anonymous symbol which will contain the array
                   element type */
                s = sym_push(SYM_FIELD, type, 0, n);
                type->t = (t1 ? VT_VLA : VT_ARRAY) | VT_PTR;
                type->ref = s;
        }
        返回 1;
}

/* Parse a type declarator (except basic type), and return the type
   in 'type'. 'td' is a bitmask indicating which kind of type decl is
   expected. 'type' should contain the basic type. 'ad' is the
   attribute definition of the basic type. It can be modified by
   type_decl().  If this (possibly abstract) declarator is a pointer chain
   it returns the innermost pointed to type (equals *type, but is a different
   pointer), otherwise returns type itself, that's used for recursive calls.  */
静态 CType *type_decl(CType *type, AttributeDef *ad, 整 *v, 整 td)
{
        CType *post, *ret;
        整 qualifiers, storage;

        /* recursive type, remove storage bits first, apply them later again */
        storage = type->t & VT_STORAGE;
        type->t &= ~VT_STORAGE;
        post = ret = type;

        当 (tok == '*') {
                qualifiers = 0;
        redo:
                next();
                转接(tok) {
                事例 TOK_CONST1:
                事例 TOK_CONST2:
                事例 TOK_CONST3:
                事例 TOK_CONST1_CN:
                        qualifiers |= VT_CONSTANT;
                        跳转 redo;
                事例 TOK_VOLATILE1:
                事例 TOK_VOLATILE2:
                事例 TOK_VOLATILE3:
                事例 TOK_VOLATILE1_CN:
                        qualifiers |= VT_VOLATILE;
                        跳转 redo;
                事例 TOK_RESTRICT1:
                事例 TOK_RESTRICT2:
                事例 TOK_RESTRICT3:
                事例 TOK_RESTRICT1_CN:
                        跳转 redo;
                /* XXX: clarify attribute handling */
                事例 TOK_ATTRIBUTE1:
                事例 TOK_ATTRIBUTE2:
                事例 TOK_ATTRIBUTE1_CN:
                事例 TOK_ATTRIBUTE2_CN:
                        parse_attribute(ad);
                        跳出;
                }
                mk_pointer(type);
                type->t |= qualifiers;
                如 (ret == type)
                        /* innermost pointed to type is the one for the first derivation */
                        ret = pointed_type(type);
        }

        如 (tok == '(') {
                /* This is possibly a parameter type list for abstract declarators
                   ('int ()'), use post_type for testing this.  */
                如 (!post_type(type, ad, 0, td)) {
                        /* It's not, so it's a nested declarator, and the post operations
                           apply to the innermost pointed to type (if any).  */
                        /* XXX: this is not correct to modify 'ad' at this point, but
                           the syntax is not clear */
                        parse_attribute(ad);
                        post = type_decl(type, ad, v, td);
                        skip(')');
                }
        } 另 如 (tok >= TOK_IDENT && (td & TYPE_DIRECT)) {
                /* type identifier */
                *v = tok;
                next();
        } 另 {
                如 (!(td & TYPE_ABSTRACT))
                  expect("identifier");
                *v = 0;
        }
        post_type(post, ad, storage, 0);
        parse_attribute(ad);
        type->t |= storage;
        返回 ret;
}

/* compute the lvalue VT_LVAL_xxx needed to match type t. */
ST_FUNC 整 lvalue_type(整 t)
{
        整 bt, r;
        r = VT_LVAL;
        bt = t & VT_BTYPE;
        如 (bt == VT_BYTE || bt == VT_BOOL)
                r |= VT_LVAL_BYTE;
        另 如 (bt == VT_SHORT)
                r |= VT_LVAL_SHORT;
        另
                返回 r;
        如 (t & VT_UNSIGNED)
                r |= VT_LVAL_UNSIGNED;
        返回 r;
}

/* indirection with full error checking and bound check */
ST_FUNC 空 indir(空)
{
        如 ((vtop->type.t & VT_BTYPE) != VT_PTR) {
                如 ((vtop->type.t & VT_BTYPE) == VT_FUNC)
                        返回;
                expect("pointer");
        }
        如 (vtop->r & VT_LVAL)
                gv(RC_INT);
        vtop->type = *pointed_type(&vtop->type);
        /* Arrays and functions are never lvalues */
        如 (!(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_VLA)
                && (vtop->type.t & VT_BTYPE) != VT_FUNC) {
                vtop->r |= lvalue_type(vtop->type.t);
                /* if bound checking, the referenced pointer must be checked */
#如定义 CONFIG_TCC_BCHECK
                如 (tcc_state->do_bounds_check)
                        vtop->r |= VT_MUSTBOUND;
#了如
        }
}

/* pass a parameter to a function and do type checking and casting */
静态 空 gfunc_param_typed(Sym *func, Sym *arg)
{
        整 func_type;
        CType type;

        func_type = func->f.func_type;
        如 (func_type == FUNC_OLD ||
                (func_type == FUNC_ELLIPSIS && arg == NULL)) {
                /* default casting : only need to convert float to double */
                如 ((vtop->type.t & VT_BTYPE) == VT_FLOAT) {
                        gen_cast_s(VT_DOUBLE);
                } 另 如 (vtop->type.t & VT_BITFIELD) {
                        type.t = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);
                        type.ref = vtop->type.ref;
                        gen_cast(&type);
                }
        } 另 如 (arg == NULL) {
                tcc_error("too many arguments to function");
        } 另 {
                type = arg->type;
                type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
                gen_assign_cast(&type);
        }
}

/* parse an expression and return its type without any side effect. */
静态 空 expr_type(CType *type, 空 (*expr_fn)(空))
{
        nocode_wanted++;
        expr_fn();
        *type = vtop->type;
        vpop();
        nocode_wanted--;
}

/* parse an expression of the form '(type)' or '(expr)' and return its
   type */
静态 空 parse_expr_type(CType *type)
{
        整 n;
        AttributeDef ad;

        skip('(');
        如 (parse_btype(type, &ad)) {
                type_decl(type, &ad, &n, TYPE_ABSTRACT);
        } 另 {
                expr_type(type, gexpr);
        }
        skip(')');
}

静态 空 parse_type(CType *type)
{
        AttributeDef ad;
        整 n;

        如 (!parse_btype(type, &ad)) {
                expect("type");
        }
        type_decl(type, &ad, &n, TYPE_ABSTRACT);
}

静态 空 parse_builtin_params(整 nc, 不变 字 *args)
{
        字 c, sep = '(';
        CType t;
        如 (nc)
                nocode_wanted++;
        next();
        当 ((c = *args++)) {
                skip(sep);
                sep = ',';
                转接 (c) {
                        事例 'e': expr_eq(); 继续;
                        事例 't': parse_type(&t); vpush(&t); 继续;
                        缺省: tcc_error("internal error"); 跳出;
                }
        }
        skip(')');
        如 (nc)
                nocode_wanted--;
}

ST_FUNC 空 unary(空)
{
        整 n, t, align, size, r, sizeof_caller;
        CType type;
        Sym *s;
        AttributeDef ad;

        sizeof_caller = in_sizeof;
        in_sizeof = 0;
        type.ref = NULL;
        /* XXX: GCC 2.95.3 does not generate a table although it should be
           better here */
 tok_next:
        转接(tok) {
        事例 TOK_EXTENSION:
                next();
                跳转 tok_next;
        事例 TOK_CINT:
        事例 TOK_CCHAR:
        事例 TOK_LCHAR:
                t = VT_INT;
 push_tokc:
                type.t = t;
                vsetc(&type, VT_CONST, &tokc);
                next();
                跳出;
        事例 TOK_CUINT:
                t = VT_INT | VT_UNSIGNED;
                跳转 push_tokc;
        事例 TOK_CLLONG:
                t = VT_LLONG;
                跳转 push_tokc;
        事例 TOK_CULLONG:
                t = VT_LLONG | VT_UNSIGNED;
                跳转 push_tokc;
        事例 TOK_CFLOAT:
                t = VT_FLOAT;
                跳转 push_tokc;
        事例 TOK_CDOUBLE:
                t = VT_DOUBLE;
                跳转 push_tokc;
        事例 TOK_CLDOUBLE:
                t = VT_LDOUBLE;
                跳转 push_tokc;
        事例 TOK_CLONG:
        事例 TOK_CULONG:
                #如定义 TCC_LONG_ARE_64_BIT
                t = VT_LLONG | VT_LONG;
                #另
                t = VT_INT | VT_LONG;
                #了如
                如 (tok == TOK_CULONG)
                        t |= VT_UNSIGNED;
                跳转 push_tokc;
        事例 TOK___FUNCTION__:
        事例 TOK___FUNCTION___CN:
                如 (!gnu_ext)
                        跳转 tok_identifier;
                /* fall thru */
        事例 TOK___FUNC__:
        事例 TOK___FUNC___CN:
                {
                        空 *ptr;
                        整 len;
                        /* special function name identifier */
                        len = strlen(funcname) + 1;
                        /* generate char[len] type */
                        type.t = VT_BYTE;
                        mk_pointer(&type);
                        type.t |= VT_ARRAY;
                        type.ref->c = len;
                        vpush_ref(&type, data_section, data_section->data_offset, len);
                        如 (!NODATA_WANTED) {
                                ptr = section_ptr_add(data_section, len);
                                memcpy(ptr, funcname, len);
                        }
                        next();
                }
                跳出;
        事例 TOK_LSTR:
#如定义 TCC_TARGET_PE
                t = VT_SHORT | VT_UNSIGNED;
#另
                t = VT_INT;
#了如
                跳转 str_init;
        事例 TOK_STR:
                /* string parsing */
                t = VT_BYTE;
                如 (tcc_state->char_is_unsigned)
                        t = VT_BYTE | VT_UNSIGNED;
        str_init:
                如 (tcc_state->warn_write_strings)
                        t |= VT_CONSTANT;
                type.t = t;
                mk_pointer(&type);
                type.t |= VT_ARRAY;
                memset(&ad, 0, 求长度(AttributeDef));
                decl_initializer_alloc(&type, &ad, VT_CONST, 2, 0, 0);
                跳出;
        事例 '(':
                next();
                /* cast ? */
                如 (parse_btype(&type, &ad)) {
                        type_decl(&type, &ad, &n, TYPE_ABSTRACT);
                        skip(')');
                        /* check ISOC99 compound literal */
                        如 (tok == '{') {
                                        /* data is allocated locally by default */
                                如 (global_expr)
                                        r = VT_CONST;
                                另
                                        r = VT_LOCAL;
                                /* all except arrays are lvalues */
                                如 (!(type.t & VT_ARRAY))
                                        r |= lvalue_type(type.t);
                                memset(&ad, 0, 求长度(AttributeDef));
                                decl_initializer_alloc(&type, &ad, r, 1, 0, 0);
                        } 另 {
                                如 (sizeof_caller) {
                                        vpush(&type);
                                        返回;
                                }
                                unary();
                                gen_cast(&type);
                        }
                } 另 如 (tok == '{') {
                        整 saved_nocode_wanted = nocode_wanted;
                        如 (const_wanted)
                                tcc_error("expected constant");
                        /* save all registers */
                        save_regs(0);
                        /* statement expression : we do not accept break/continue
                           inside as GCC does.  We do retain the nocode_wanted state,
                           as statement expressions can't ever be entered from the
                           outside, so any reactivation of code emission (from labels
                           or loop heads) can be disabled again after the end of it. */
                        block(NULL, NULL, 1);
                        nocode_wanted = saved_nocode_wanted;
                        skip(')');
                } 另 {
                        gexpr();
                        skip(')');
                }
                跳出;
        事例 '*':
                next();
                unary();
                indir();
                跳出;
        事例 '&':
                next();
                unary();
                /* functions names must be treated as function pointers,
                   except for unary '&' and sizeof. Since we consider that
                   functions are not lvalues, we only have to handle it
                   there and in function calls. */
                /* arrays can also be used although they are not lvalues */
                如 ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
                        !(vtop->type.t & VT_ARRAY))
                        test_lvalue();
                mk_pointer(&vtop->type);
                gaddrof();
                跳出;
        事例 '!':
                next();
                unary();
                如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                        gen_cast_s(VT_BOOL);
                        vtop->c.i = !vtop->c.i;
                } 另 如 ((vtop->r & VT_VALMASK) == VT_CMP)
                        vtop->c.i ^= 1;
                另 {
                        save_regs(1);
                        vseti(VT_JMP, gvtst(1, 0));
                }
                跳出;
        事例 '~':
                next();
                unary();
                vpushi(-1);
                gen_op('^');
                跳出;
        事例 '+':
                next();
                unary();
                如 ((vtop->type.t & VT_BTYPE) == VT_PTR)
                        tcc_error("pointer not accepted for unary plus");
                /* In order to force cast, we add zero, except for floating point
                   where we really need an noop (otherwise -0.0 will be transformed
                   into +0.0).  */
                如 (!is_float(vtop->type.t)) {
                        vpushi(0);
                        gen_op('+');
                }
                跳出;
        事例 TOK_SIZEOF:
        事例 TOK_SIZEOF_CN:
        事例 TOK_ALIGNOF1:
        事例 TOK_ALIGNOF2:
        事例 TOK_ALIGNOF1_CN:
                t = tok;
                next();
                in_sizeof++;
                expr_type(&type, unary); /* Perform a in_sizeof = 0; */
                s = vtop[1].sym; /* hack: accessing previous vtop */
                size = type_size(&type, &align);
                如 (s && s->a.aligned)
                        align = 1 << (s->a.aligned - 1);
                如 (t == TOK_SIZEOF || t == TOK_SIZEOF_CN) {
                        如 (!(type.t & VT_VLA)) {
                                如 (size < 0)
                                        tcc_error("sizeof applied to an incomplete type");
                                vpushs(size);
                        } 另 {
                                vla_runtime_type_size(&type, &align);
                        }
                } 另 {
                        vpushs(align);
                }
                vtop->type.t |= VT_UNSIGNED;
                跳出;

        事例 TOK_builtin_expect:
                /* __builtin_expect is a no-op for now */
                parse_builtin_params(0, "ee");
                vpop();
                跳出;
        事例 TOK_builtin_types_compatible_p:
                parse_builtin_params(0, "tt");
                vtop[-1].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
                vtop[0].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
                n = is_compatible_types(&vtop[-1].type, &vtop[0].type);
                vtop -= 2;
                vpushi(n);
                跳出;
        事例 TOK_builtin_choose_expr:
                {
                        int64_t c;
                        next();
                        skip('(');
                        c = expr_const64();
                        skip(',');
                        如 (!c) {
                                nocode_wanted++;
                        }
                        expr_eq();
                        如 (!c) {
                                vpop();
                                nocode_wanted--;
                        }
                        skip(',');
                        如 (c) {
                                nocode_wanted++;
                        }
                        expr_eq();
                        如 (c) {
                                vpop();
                                nocode_wanted--;
                        }
                        skip(')');
                }
                跳出;
        事例 TOK_builtin_constant_p:
                parse_builtin_params(1, "e");
                n = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
                vtop--;
                vpushi(n);
                跳出;
        事例 TOK_builtin_frame_address:
        事例 TOK_builtin_return_address:
                {
                        整 tok1 = tok;
                        整 level;
                        next();
                        skip('(');
                        如 (tok != TOK_CINT) {
                                tcc_error("%s only takes positive integers",
                                                  tok1 == TOK_builtin_return_address ?
                                                  "__builtin_return_address" :
                                                  "__builtin_frame_address");
                        }
                        level = (uint32_t)tokc.i;
                        next();
                        skip(')');
                        type.t = VT_VOID;
                        mk_pointer(&type);
                        vset(&type, VT_LOCAL, 0);       /* local frame */
                        当 (level--) {
                                mk_pointer(&vtop->type);
                                indir();                    /* -> parent frame */
                        }
                        如 (tok1 == TOK_builtin_return_address) {
                                // assume return address is just above frame pointer on stack
                                vpushi(PTR_SIZE);
                                gen_op('+');
                                mk_pointer(&vtop->type);
                                indir();
                        }
                }
                跳出;
#如定义 TCC_TARGET_X86_64
#如定义 TCC_TARGET_PE
        事例 TOK_builtin_va_start:
                parse_builtin_params(0, "ee");
                r = vtop->r & VT_VALMASK;
                如 (r == VT_LLOCAL)
                        r = VT_LOCAL;
                如 (r != VT_LOCAL)
                        tcc_error("__builtin_va_start expects a local variable");
                vtop->r = r;
                vtop->type = char_pointer_type;
                vtop->c.i += 8;
                vstore();
                跳出;
#另
        事例 TOK_builtin_va_arg_types:
                parse_builtin_params(0, "t");
                vpushi(classify_x86_64_va_arg(&vtop->type));
                vswap();
                vpop();
                跳出;
#了如
#了如

#如定义 TCC_TARGET_ARM64
        事例 TOK___va_start: {
                parse_builtin_params(0, "ee");
                //xx check types
                gen_va_start();
                vpushi(0);
                vtop->type.t = VT_VOID;
                跳出;
        }
        事例 TOK___va_arg: {
                parse_builtin_params(0, "et");
                type = vtop->type;
                vpop();
                //xx check types
                gen_va_arg(&type);
                vtop->type = type;
                跳出;
        }
        事例 TOK___arm64_clear_cache: {
                parse_builtin_params(0, "ee");
                gen_clear_cache();
                vpushi(0);
                vtop->type.t = VT_VOID;
                跳出;
        }
#了如
        /* pre operations */
        事例 TOK_INC:
        事例 TOK_DEC:
                t = tok;
                next();
                unary();
                inc(0, t);
                跳出;
        事例 '-':
                next();
                unary();
                t = vtop->type.t & VT_BTYPE;
                如 (is_float(t)) {
                        /* In IEEE negate(x) isn't subtract(0,x), but rather
                           subtract(-0, x).  */
                        vpush(&vtop->type);
                        如 (t == VT_FLOAT)
                                vtop->c.f = -1.0 * 0.0;
                        另 如 (t == VT_DOUBLE)
                                vtop->c.d = -1.0 * 0.0;
                        另
                                vtop->c.ld = -1.0 * 0.0;
                } 另
                        vpushi(0);
                vswap();
                gen_op('-');
                跳出;
        事例 TOK_LAND:
                如 (!gnu_ext)
                        跳转 tok_identifier;
                next();
                /* allow to take the address of a label */
                如 (tok < TOK_UIDENT)
                        expect("label identifier");
                s = label_find(tok);
                如 (!s) {
                        s = label_push(&global_label_stack, tok, LABEL_FORWARD);
                } 另 {
                        如 (s->r == LABEL_DECLARED)
                                s->r = LABEL_FORWARD;
                }
                如 (!s->type.t) {
                        s->type.t = VT_VOID;
                        mk_pointer(&s->type);
                        s->type.t |= VT_STATIC;
                }
                vpushsym(&s->type, s);
                next();
                跳出;

        事例 TOK_GENERIC:
        {
                CType controlling_type;
                整 has_default = 0;
                整 has_match = 0;
                整 learn = 0;
                TokenString *str = NULL;

                next();
                skip('(');
                expr_type(&controlling_type, expr_eq);
                controlling_type.t &= ~(VT_CONSTANT | VT_VOLATILE | VT_ARRAY);
                对于 (;;) {
                        learn = 0;
                        skip(',');
                        如 (tok == TOK_DEFAULT || tok == TOK_DEFAULT_CN) {
                                如 (has_default)
                                        tcc_error("too many 'default'");
                                has_default = 1;
                                如 (!has_match)
                                        learn = 1;
                                next();
                        } 另 {
                                AttributeDef ad_tmp;
                                整 itmp;
                                CType cur_type;
                                parse_btype(&cur_type, &ad_tmp);
                                type_decl(&cur_type, &ad_tmp, &itmp, TYPE_ABSTRACT);
                                如 (compare_types(&controlling_type, &cur_type, 0)) {
                                        如 (has_match) {
                                                tcc_error("type match twice");
                                        }
                                        has_match = 1;
                                        learn = 1;
                                }
                        }
                        skip(':');
                        如 (learn) {
                                如 (str)
                                        tok_str_free(str);
                                skip_or_save_block(&str);
                        } 另 {
                                skip_or_save_block(NULL);
                        }
                        如 (tok == ')')
                                跳出;
                }
                如 (!str) {
                        字 buf[60];
                        type_to_str(buf, 求长度 buf, &controlling_type, NULL);
                        tcc_error("type '%s' does not match any association", buf);
                }
                begin_macro(str, 1);
                next();
                expr_eq();
                如 (tok != TOK_EOF)
                        expect(",");
                end_macro();
                next();
                跳出;
        }
        // special qnan , snan and infinity values
        事例 TOK___NAN__:
                vpush64(VT_DOUBLE, 0x7ff8000000000000ULL);
                next();
                跳出;
        事例 TOK___SNAN__:
                vpush64(VT_DOUBLE, 0x7ff0000000000001ULL);
                next();
                跳出;
        事例 TOK___INF__:
                vpush64(VT_DOUBLE, 0x7ff0000000000000ULL);
                next();
                跳出;

        缺省:
        tok_identifier:
                t = tok;
                next();
                如 (t < TOK_UIDENT)
                        expect("identifier");
                s = sym_find(t);
                如 (!s) {
                        不变 字 *name = get_tok_str(t, NULL);
                        如 (tok != '(')
                                tcc_error("'%s' undeclared", name);
                        /* for simple function calls, we tolerate undeclared
                           external reference to int() function */
                        如 (tcc_state->warn_implicit_function_declaration
#如定义 TCC_TARGET_PE
                                /* people must be warned about using undeclared WINAPI functions
                                   (which usually start with uppercase letter) */
                                || (name[0] >= 'A' && name[0] <= 'Z')
#了如
                        )
                                tcc_warning("implicit declaration of function '%s'", name);
                        s = external_global_sym(t, &func_old_type, 0);
                }

                r = s->r;
                /* A symbol that has a register is a local register variable,
                   which starts out as VT_LOCAL value.  */
                如 ((r & VT_VALMASK) < VT_CONST)
                        r = (r & ~VT_VALMASK) | VT_LOCAL;

                vset(&s->type, r, s->c);
                /* Point to s as backpointer (even without r&VT_SYM).
                   Will be used by at least the x86 inline asm parser for
                   regvars.  */
                vtop->sym = s;

                如 (r & VT_SYM) {
                        vtop->c.i = 0;
                } 另 如 (r == VT_CONST && IS_ENUM_VAL(s->type.t)) {
                        vtop->c.i = s->enum_val;
                }
                跳出;
        }

        /* post operations */
        当 (1) {
                如 (tok == TOK_INC || tok == TOK_DEC) {
                        inc(1, tok);
                        next();
                } 另 如 (tok == '.' || tok == TOK_ARROW || tok == TOK_CDOUBLE) {
                        整 qualifiers;
                        /* field */
                        如 (tok == TOK_ARROW)
                                indir();
                        qualifiers = vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
                        test_lvalue();
                        gaddrof();
                        /* expect pointer on structure */
                        如 ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                                expect("struct or union");
                        如 (tok == TOK_CDOUBLE)
                                expect("field name");
                        next();
                        如 (tok == TOK_CINT || tok == TOK_CUINT)
                                expect("field name");
                        s = find_field(&vtop->type, tok);
                        如 (!s)
                                tcc_error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, &tokc));
                        /* add field offset to pointer */
                        vtop->type = char_pointer_type; /* change type to 'char *' */
                        vpushi(s->c);
                        gen_op('+');
                        /* change type to field type, and set to lvalue */
                        vtop->type = s->type;
                        vtop->type.t |= qualifiers;
                        /* an array is never an lvalue */
                        如 (!(vtop->type.t & VT_ARRAY)) {
                                vtop->r |= lvalue_type(vtop->type.t);
#如定义 CONFIG_TCC_BCHECK
                                /* if bound checking, the referenced pointer must be checked */
                                如 (tcc_state->do_bounds_check && (vtop->r & VT_VALMASK) != VT_LOCAL)
                                        vtop->r |= VT_MUSTBOUND;
#了如
                        }
                        next();
                } 另 如 (tok == '[') {
                        next();
                        gexpr();
                        gen_op('+');
                        indir();
                        skip(']');
                } 另 如 (tok == '(') {
                        SValue ret;
                        Sym *sa;
                        整 nb_args, ret_nregs, ret_align, regsize, variadic;

                        /* function call  */
                        如 ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
                                /* pointer test (no array accepted) */
                                如 ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                                        vtop->type = *pointed_type(&vtop->type);
                                        如 ((vtop->type.t & VT_BTYPE) != VT_FUNC)
                                                跳转 error_func;
                                } 另 {
                                error_func:
                                        expect("function pointer");
                                }
                        } 另 {
                                vtop->r &= ~VT_LVAL; /* no lvalue */
                        }
                        /* get return type */
                        s = vtop->type.ref;
                        next();
                        sa = s->next; /* first parameter */
                        nb_args = regsize = 0;
                        ret.r2 = VT_CONST;
                        /* compute first implicit argument if a structure is returned */
                        如 ((s->type.t & VT_BTYPE) == VT_STRUCT) {
                                variadic = (s->f.func_type == FUNC_ELLIPSIS);
                                ret_nregs = gfunc_sret(&s->type, variadic, &ret.type,
                                                                           &ret_align, &regsize);
                                如 (!ret_nregs) {
                                        /* get some space for the returned structure */
                                        size = type_size(&s->type, &align);
#如定义 TCC_TARGET_ARM64
                                /* On arm64, a small struct is return in registers.
                                   It is much easier to write it to memory if we know
                                   that we are allowed to write some extra bytes, so
                                   round the allocated space up to a power of 2: */
                                如 (size < 16)
                                        当 (size & (size - 1))
                                                size = (size | (size - 1)) + 1;
#了如
                                        loc = (loc - size) & -align;
                                        ret.type = s->type;
                                        ret.r = VT_LOCAL | VT_LVAL;
                                        /* pass it as 'int' to avoid structure arg passing
                                           problems */
                                        vseti(VT_LOCAL, loc);
                                        ret.c = vtop->c;
                                        nb_args++;
                                }
                        } 另 {
                                ret_nregs = 1;
                                ret.type = s->type;
                        }

                        如 (ret_nregs) {
                                /* return in register */
                                如 (is_float(ret.type.t)) {
                                        ret.r = reg_fret(ret.type.t);
#如定义 TCC_TARGET_X86_64
                                        如 ((ret.type.t & VT_BTYPE) == VT_QFLOAT)
                                          ret.r2 = REG_QRET;
#了如
                                } 另 {
#如未定义 TCC_TARGET_ARM64
#如定义 TCC_TARGET_X86_64
                                        如 ((ret.type.t & VT_BTYPE) == VT_QLONG)
#另
                                        如 ((ret.type.t & VT_BTYPE) == VT_LLONG)
#了如
                                                ret.r2 = REG_LRET;
#了如
                                        ret.r = REG_IRET;
                                }
                                ret.c.i = 0;
                        }
                        如 (tok != ')') {
                                对于(;;) {
                                        expr_eq();
                                        gfunc_param_typed(s, sa);
                                        nb_args++;
                                        如 (sa)
                                                sa = sa->next;
                                        如 (tok == ')')
                                                跳出;
                                        skip(',');
                                }
                        }
                        如 (sa)
                                tcc_error("too few arguments to function");
                        skip(')');
                        gfunc_call(nb_args);

                        /* return value */
                        对于 (r = ret.r + ret_nregs + !ret_nregs; r-- > ret.r;) {
                                vsetc(&ret.type, r, &ret.c);
                                vtop->r2 = ret.r2; /* Loop only happens when r2 is VT_CONST */
                        }

                        /* handle packed struct return */
                        如 (((s->type.t & VT_BTYPE) == VT_STRUCT) && ret_nregs) {
                                整 addr, offset;

                                size = type_size(&s->type, &align);
                                /* We're writing whole regs often, make sure there's enough
                                   space.  Assume register size is power of 2.  */
                                如 (regsize > align)
                                        align = regsize;
                                loc = (loc - size) & -align;
                                addr = loc;
                                offset = 0;
                                对于 (;;) {
                                        vset(&ret.type, VT_LOCAL | VT_LVAL, addr + offset);
                                        vswap();
                                        vstore();
                                        vtop--;
                                        如 (--ret_nregs == 0)
                                                跳出;
                                        offset += regsize;
                                }
                                vset(&s->type, VT_LOCAL | VT_LVAL, addr);
                        }
                } 另 {
                        跳出;
                }
        }
}

ST_FUNC 空 expr_prod(空)
{
        整 t;

        unary();
        当 (tok == '*' || tok == '/' || tok == '%') {
                t = tok;
                next();
                unary();
                gen_op(t);
        }
}

ST_FUNC 空 expr_sum(空)
{
        整 t;

        expr_prod();
        当 (tok == '+' || tok == '-') {
                t = tok;
                next();
                expr_prod();
                gen_op(t);
        }
}

静态 空 expr_shift(空)
{
        整 t;

        expr_sum();
        当 (tok == TOK_SHL || tok == TOK_SAR) {
                t = tok;
                next();
                expr_sum();
                gen_op(t);
        }
}

静态 空 expr_cmp(空)
{
        整 t;

        expr_shift();
        当 ((tok >= TOK_ULE && tok <= TOK_GT) ||
                   tok == TOK_ULT || tok == TOK_UGE) {
                t = tok;
                next();
                expr_shift();
                gen_op(t);
        }
}

静态 空 expr_cmpeq(空)
{
        整 t;

        expr_cmp();
        当 (tok == TOK_EQ || tok == TOK_NE) {
                t = tok;
                next();
                expr_cmp();
                gen_op(t);
        }
}

静态 空 expr_and(空)
{
        expr_cmpeq();
        当 (tok == '&') {
                next();
                expr_cmpeq();
                gen_op('&');
        }
}

静态 空 expr_xor(空)
{
        expr_and();
        当 (tok == '^') {
                next();
                expr_and();
                gen_op('^');
        }
}

静态 空 expr_or(空)
{
        expr_xor();
        当 (tok == '|') {
                next();
                expr_xor();
                gen_op('|');
        }
}

静态 空 expr_land(空)
{
        expr_or();
        如 (tok == TOK_LAND) {
                整 t = 0;
                对于(;;) {
                        如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                                gen_cast_s(VT_BOOL);
                                如 (vtop->c.i) {
                                        vpop();
                                } 另 {
                                        nocode_wanted++;
                                        当 (tok == TOK_LAND) {
                                                next();
                                                expr_or();
                                                vpop();
                                        }
                                        nocode_wanted--;
                                        如 (t)
                                                gsym(t);
                                        gen_cast_s(VT_INT);
                                        跳出;
                                }
                        } 另 {
                                如 (!t)
                                        save_regs(1);
                                t = gvtst(1, t);
                        }
                        如 (tok != TOK_LAND) {
                                如 (t)
                                        vseti(VT_JMPI, t);
                                另
                                        vpushi(1);
                                跳出;
                        }
                        next();
                        expr_or();
                }
        }
}

静态 空 expr_lor(空)
{
        expr_land();
        如 (tok == TOK_LOR) {
                整 t = 0;
                对于(;;) {
                        如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                                gen_cast_s(VT_BOOL);
                                如 (!vtop->c.i) {
                                        vpop();
                                } 另 {
                                        nocode_wanted++;
                                        当 (tok == TOK_LOR) {
                                                next();
                                                expr_land();
                                                vpop();
                                        }
                                        nocode_wanted--;
                                        如 (t)
                                                gsym(t);
                                        gen_cast_s(VT_INT);
                                        跳出;
                                }
                        } 另 {
                                如 (!t)
                                        save_regs(1);
                                t = gvtst(0, t);
                        }
                        如 (tok != TOK_LOR) {
                                如 (t)
                                        vseti(VT_JMP, t);
                                另
                                        vpushi(0);
                                跳出;
                        }
                        next();
                        expr_land();
                }
        }
}

/* Assuming vtop is a value used in a conditional context
   (i.e. compared with zero) return 0 if it's false, 1 if
   true and -1 if it can't be statically determined.  */
静态 整 condition_3way(空)
{
        整 c = -1;
        如 ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
                (!(vtop->r & VT_SYM) || !vtop->sym->a.weak)) {
                vdup();
                gen_cast_s(VT_BOOL);
                c = vtop->c.i;
                vpop();
        }
        返回 c;
}

静态 空 expr_cond(空)
{
        整 tt, u, r1, r2, rc, t1, t2, bt1, bt2, islv, c, g;
        SValue sv;
        CType type, type1, type2;

        expr_lor();
        如 (tok == '?') {
                next();
                c = condition_3way();
                g = (tok == ':' && gnu_ext);
                如 (c < 0) {
                        /* needed to avoid having different registers saved in
                           each branch */
                        如 (is_float(vtop->type.t)) {
                                rc = RC_FLOAT;
#如定义 TCC_TARGET_X86_64
                                如 ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
                                        rc = RC_ST0;
                                }
#了如
                        } 另
                                rc = RC_INT;
                        gv(rc);
                        save_regs(1);
                        如 (g)
                                gv_dup();
                        tt = gvtst(1, 0);

                } 另 {
                        如 (!g)
                                vpop();
                        tt = 0;
                }

                如 (1) {
                        如 (c == 0)
                                nocode_wanted++;
                        如 (!g)
                                gexpr();

                        type1 = vtop->type;
                        sv = *vtop; /* save value to handle it later */
                        vtop--; /* no vpop so that FP stack is not flushed */
                        skip(':');

                        u = 0;
                        如 (c < 0)
                                u = gjmp(0);
                        gsym(tt);

                        如 (c == 0)
                                nocode_wanted--;
                        如 (c == 1)
                                nocode_wanted++;
                        expr_cond();
                        如 (c == 1)
                                nocode_wanted--;

                        type2 = vtop->type;
                        t1 = type1.t;
                        bt1 = t1 & VT_BTYPE;
                        t2 = type2.t;
                        bt2 = t2 & VT_BTYPE;
                        type.ref = NULL;

                        /* cast operands to correct type according to ISOC rules */
                        如 (is_float(bt1) || is_float(bt2)) {
                                如 (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                                        type.t = VT_LDOUBLE;

                                } 另 如 (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                                        type.t = VT_DOUBLE;
                                } 另 {
                                        type.t = VT_FLOAT;
                                }
                        } 另 如 (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                                /* cast to biggest op */
                                type.t = VT_LLONG;
                                /* convert to unsigned if it does not fit in a long long */
                                如 ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
                                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
                                        type.t |= VT_UNSIGNED;
                        } 另 如 (bt1 == VT_PTR || bt2 == VT_PTR) {
                                /* If one is a null ptr constant the result type
                                   is the other.  */
                                如 (is_null_pointer (vtop))
                                        type = type1;
                                另 如 (is_null_pointer (&sv))
                                        type = type2;
                                /* XXX: test pointer compatibility, C99 has more elaborate
                                   rules here.  */
                                另
                                        type = type1;
                        } 另 如 (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                                /* XXX: test function pointer compatibility */
                                type = bt1 == VT_FUNC ? type1 : type2;
                        } 另 如 (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                                /* XXX: test structure compatibility */
                                type = bt1 == VT_STRUCT ? type1 : type2;
                        } 另 如 (bt1 == VT_VOID || bt2 == VT_VOID) {
                                /* NOTE: as an extension, we accept void on only one side */
                                type.t = VT_VOID;
                        } 另 {
                                /* integer operations */
                                type.t = VT_INT;
                                /* convert to unsigned if it does not fit in an integer */
                                如 ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
                                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
                                        type.t |= VT_UNSIGNED;
                        }
                        /* keep structs lvalue by transforming `(expr ? a : b)` to `*(expr ? &a : &b)` so
                           that `(expr ? a : b).mem` does not error  with "lvalue expected" */
                        islv = (vtop->r & VT_LVAL) && (sv.r & VT_LVAL) && VT_STRUCT == (type.t & VT_BTYPE);
                        islv &= c < 0;

                        /* now we convert second operand */
                        如 (c != 1) {
                                gen_cast(&type);
                                如 (islv) {
                                        mk_pointer(&vtop->type);
                                        gaddrof();
                                } 另 如 (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                                        gaddrof();
                        }

                        rc = RC_INT;
                        如 (is_float(type.t)) {
                                rc = RC_FLOAT;
#如定义 TCC_TARGET_X86_64
                                如 ((type.t & VT_BTYPE) == VT_LDOUBLE) {
                                        rc = RC_ST0;
                                }
#了如
                        } 另 如 ((type.t & VT_BTYPE) == VT_LLONG) {
                                /* for long longs, we use fixed registers to avoid having
                                   to handle a complicated move */
                                rc = RC_IRET;
                        }

                        tt = r2 = 0;
                        如 (c < 0) {
                                r2 = gv(rc);
                                tt = gjmp(0);
                        }
                        gsym(u);

                        /* this is horrible, but we must also convert first
                           operand */
                        如 (c != 0) {
                                *vtop = sv;
                                gen_cast(&type);
                                如 (islv) {
                                        mk_pointer(&vtop->type);
                                        gaddrof();
                                } 另 如 (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                                        gaddrof();
                        }

                        如 (c < 0) {
                                r1 = gv(rc);
                                move_reg(r2, r1, type.t);
                                vtop->r = r2;
                                gsym(tt);
                                如 (islv)
                                        indir();
                        }
                }
        }
}

静态 空 expr_eq(空)
{
        整 t;

        expr_cond();
        如 (tok == '=' ||
                (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
                tok == TOK_A_XOR || tok == TOK_A_OR ||
                tok == TOK_A_SHL || tok == TOK_A_SAR) {
                test_lvalue();
                t = tok;
                next();
                如 (t == '=') {
                        expr_eq();
                } 另 {
                        vdup();
                        expr_eq();
                        gen_op(t & 0x7f);
                }
                vstore();
        }
}

ST_FUNC 空 gexpr(空)
{
        当 (1) {
                expr_eq();
                如 (tok != ',')
                        跳出;
                vpop();
                next();
        }
}

/* parse a constant expression and return value in vtop.  */
静态 空 expr_const1(空)
{
        const_wanted++;
        nocode_wanted++;
        expr_cond();
        nocode_wanted--;
        const_wanted--;
}

/* parse an integer constant and return its value. */
静态 内联 int64_t expr_const64(空)
{
        int64_t c;
        expr_const1();
        如 ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
                expect("constant expression");
        c = vtop->c.i;
        vpop();
        返回 c;
}

/* parse an integer constant and return its value.
   Complain if it doesn't fit 32bit (signed or unsigned).  */
ST_FUNC 整 expr_const(空)
{
        整 c;
        int64_t wc = expr_const64();
        c = wc;
        如 (c != wc && (无符)c != wc)
                tcc_error("constant exceeds 32 bit");
        返回 c;
}

/* return the label token if current token is a label, otherwise
   return zero */
静态 整 is_label(空)
{
        整 last_tok;

        /* fast test first */
        如 (tok < TOK_UIDENT)
                返回 0;
        /* no need to save tokc because tok is an identifier */
        last_tok = tok;
        next();
        如 (tok == ':') {
                返回 last_tok;
        } 另 {
                unget_tok(last_tok);
                返回 0;
        }
}

#如未定义 TCC_TARGET_ARM64
静态 空 gfunc_return(CType *func_type)
{
        如 ((func_type->t & VT_BTYPE) == VT_STRUCT) {
                CType type, ret_type;
                整 ret_align, ret_nregs, regsize;
                ret_nregs = gfunc_sret(func_type, func_var, &ret_type,
                                                           &ret_align, &regsize);
                如 (0 == ret_nregs) {
                        /* if returning structure, must copy it to implicit
                           first pointer arg location */
                        type = *func_type;
                        mk_pointer(&type);
                        vset(&type, VT_LOCAL | VT_LVAL, func_vc);
                        indir();
                        vswap();
                        /* copy structure value to pointer */
                        vstore();
                } 另 {
                        /* returning structure packed into registers */
                        整 r, size, addr, align;
                        size = type_size(func_type,&align);
                        如 ((vtop->r != (VT_LOCAL | VT_LVAL) ||
                                 (vtop->c.i & (ret_align-1)))
                                && (align & (ret_align-1))) {
                                loc = (loc - size) & -ret_align;
                                addr = loc;
                                type = *func_type;
                                vset(&type, VT_LOCAL | VT_LVAL, addr);
                                vswap();
                                vstore();
                                vpop();
                                vset(&ret_type, VT_LOCAL | VT_LVAL, addr);
                        }
                        vtop->type = ret_type;
                        如 (is_float(ret_type.t))
                                r = rc_fret(ret_type.t);
                        另
                                r = RC_IRET;

                        如 (ret_nregs == 1)
                                gv(r);
                        另 {
                                对于 (;;) {
                                        vdup();
                                        gv(r);
                                        vpop();
                                        如 (--ret_nregs == 0)
                                          跳出;
                                        /* We assume that when a structure is returned in multiple
                                           registers, their classes are consecutive values of the
                                           suite s(n) = 2^n */
                                        r <<= 1;
                                        vtop->c.i += regsize;
                                }
                        }
                }
        } 另 如 (is_float(func_type->t)) {
                gv(rc_fret(func_type->t));
        } 另 {
                gv(RC_IRET);
        }
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
}
#了如

静态 整 case_cmp(不变 空 *pa, 不变 空 *pb)
{
        int64_t a = (*(结构 case_t**) pa)->v1;
        int64_t b = (*(结构 case_t**) pb)->v1;
        返回 a < b ? -1 : a > b;
}

静态 空 gcase(结构 case_t **base, 整 len, 整 *bsym)
{
        结构 case_t *p;
        整 e;
        整 ll = (vtop->type.t & VT_BTYPE) == VT_LLONG;
        gv(RC_INT);
        当 (len > 4) {
                /* binary search */
                p = base[len/2];
                vdup();
                如 (ll)
                        vpushll(p->v2);
                另
                        vpushi(p->v2);
                gen_op(TOK_LE);
                e = gtst(1, 0);
                vdup();
                如 (ll)
                        vpushll(p->v1);
                另
                        vpushi(p->v1);
                gen_op(TOK_GE);
                gtst_addr(0, p->sym); /* v1 <= x <= v2 */
                /* x < v1 */
                gcase(base, len/2, bsym);
                如 (cur_switch->def_sym)
                        gjmp_addr(cur_switch->def_sym);
                另
                        *bsym = gjmp(*bsym);
                /* x > v2 */
                gsym(e);
                e = len/2 + 1;
                base += e; len -= e;
        }
        /* linear scan */
        当 (len--) {
                p = *base++;
                vdup();
                如 (ll)
                        vpushll(p->v2);
                另
                        vpushi(p->v2);
                如 (p->v1 == p->v2) {
                        gen_op(TOK_EQ);
                        gtst_addr(0, p->sym);
                } 另 {
                        gen_op(TOK_LE);
                        e = gtst(1, 0);
                        vdup();
                        如 (ll)
                                vpushll(p->v1);
                        另
                                vpushi(p->v1);
                        gen_op(TOK_GE);
                        gtst_addr(0, p->sym);
                        gsym(e);
                }
        }
}

静态 空 block(整 *bsym, 整 *csym, 整 is_expr)
{
        整 a, b, c, d, cond;
        Sym *s;

        /* generate line number info */
        如 (tcc_state->do_debug)
                tcc_debug_line(tcc_state);

        如 (is_expr) {
                /* default return value is (void) */
                vpushi(0);
                vtop->type.t = VT_VOID;
        }

        如 (tok == TOK_IF || tok == TOK_IF_CN) {
                /* if test */
                整 saved_nocode_wanted = nocode_wanted;
                next();
                skip('(');
                gexpr();
                skip(')');
                cond = condition_3way();
                如 (cond == 1)
                        a = 0, vpop();
                另
                        a = gvtst(1, 0);
                如 (cond == 0)
                        nocode_wanted |= 0x20000000;
                block(bsym, csym, 0);
                如 (cond != 1)
                        nocode_wanted = saved_nocode_wanted;
                c = tok;
                如 (c == TOK_ELSE || c == TOK_ELSE_CN) {
                        next();
                        d = gjmp(0);
                        gsym(a);
                        如 (cond == 1)
                                nocode_wanted |= 0x20000000;
                        block(bsym, csym, 0);
                        gsym(d); /* patch else jmp */
                        如 (cond != 0)
                                nocode_wanted = saved_nocode_wanted;
                } 另
                        gsym(a);
        } 另 如 (tok == TOK_WHILE || tok == TOK_WHILE_CN) {
                整 saved_nocode_wanted;
                nocode_wanted &= ~0x20000000;
                next();
                d = ind;
                vla_sp_restore();
                skip('(');
                gexpr();
                skip(')');
                a = gvtst(1, 0);
                b = 0;
                ++local_scope;
                saved_nocode_wanted = nocode_wanted;
                block(&a, &b, 0);
                nocode_wanted = saved_nocode_wanted;
                --local_scope;
                gjmp_addr(d);
                gsym(a);
                gsym_addr(b, d);
        } 另 如 (tok == '{') {
                Sym *llabel;
                整 block_vla_sp_loc = vla_sp_loc, saved_vlas_in_scope = vlas_in_scope;

                next();
                /* record local declaration stack position */
                s = local_stack;
                llabel = local_label_stack;
                ++local_scope;

                /* handle local labels declarations */
                如 (tok == TOK_LABEL || tok == TOK_LABEL_CN) {
                        next();
                        对于(;;) {
                                如 (tok < TOK_UIDENT)
                                        expect("label identifier");
                                label_push(&local_label_stack, tok, LABEL_DECLARED);
                                next();
                                如 (tok == ',') {
                                        next();
                                } 另 {
                                        skip(';');
                                        跳出;
                                }
                        }
                }
                当 (tok != '}') {
                        如 ((a = is_label()))
                                unget_tok(a);
                        另
                                decl(VT_LOCAL);
                        如 (tok != '}') {
                                如 (is_expr)
                                        vpop();
                                block(bsym, csym, is_expr);
                        }
                }
                /* pop locally defined labels */
                label_pop(&local_label_stack, llabel, is_expr);
                /* pop locally defined symbols */
                --local_scope;
                /* In the is_expr case (a statement expression is finished here),
                   vtop might refer to symbols on the local_stack.  Either via the
                   type or via vtop->sym.  We can't pop those nor any that in turn
                   might be referred to.  To make it easier we don't roll back
                   any symbols in that case; some upper level call to block() will
                   do that.  We do have to remove such symbols from the lookup
                   tables, though.  sym_pop will do that.  */
                sym_pop(&local_stack, s, is_expr);

                /* Pop VLA frames and restore stack pointer if required */
                如 (vlas_in_scope > saved_vlas_in_scope) {
                        vla_sp_loc = saved_vlas_in_scope ? block_vla_sp_loc : vla_sp_root_loc;
                        vla_sp_restore();
                }
                vlas_in_scope = saved_vlas_in_scope;

                next();
        } 另 如 (tok == TOK_RETURN || tok == TOK_RETURN_CN) {
                next();
                如 (tok != ';') {
                        gexpr();
                        gen_assign_cast(&func_vt);
                        gfunc_return(&func_vt);
                }
                skip(';');
                /* jump unless last stmt in top-level block */
                如 (tok != '}' || local_scope != 1)
                        rsym = gjmp(rsym);
                nocode_wanted |= 0x20000000;
        } 另 如 (tok == TOK_BREAK || tok == TOK_BREAK_CN) {
                /* compute jump */
                如 (!bsym)
                        tcc_error("cannot break");
                *bsym = gjmp(*bsym);
                next();
                skip(';');
                nocode_wanted |= 0x20000000;
        } 另 如 (tok == TOK_CONTINUE || tok == TOK_CONTINUE_CN) {
                /* compute jump */
                如 (!csym)
                        tcc_error("cannot continue");
                vla_sp_restore_root();
                *csym = gjmp(*csym);
                next();
                skip(';');
        } 另 如 (tok == TOK_FOR || tok == TOK_FOR_CN) {
                整 e;
                整 saved_nocode_wanted;
                nocode_wanted &= ~0x20000000;
                next();
                skip('(');
                s = local_stack;
                ++local_scope;
                如 (tok != ';') {
                        /* c99 for-loop init decl? */
                        如 (!decl0(VT_LOCAL, 1, NULL)) {
                                /* no, regular for-loop init expr */
                                gexpr();
                                vpop();
                        }
                }
                skip(';');
                d = ind;
                c = ind;
                vla_sp_restore();
                a = 0;
                b = 0;
                如 (tok != ';') {
                        gexpr();
                        a = gvtst(1, 0);
                }
                skip(';');
                如 (tok != ')') {
                        e = gjmp(0);
                        c = ind;
                        vla_sp_restore();
                        gexpr();
                        vpop();
                        gjmp_addr(d);
                        gsym(e);
                }
                skip(')');
                saved_nocode_wanted = nocode_wanted;
                block(&a, &b, 0);
                nocode_wanted = saved_nocode_wanted;
                gjmp_addr(c);
                gsym(a);
                gsym_addr(b, c);
                --local_scope;
                sym_pop(&local_stack, s, 0);

        } 另
        如 (tok == TOK_DO || tok == TOK_DO_CN) {
                整 saved_nocode_wanted;
                nocode_wanted &= ~0x20000000;
                next();
                a = 0;
                b = 0;
                d = ind;
                vla_sp_restore();
                saved_nocode_wanted = nocode_wanted;
                block(&a, &b, 0);
                如 (tok == TOK_WHILE)
                        skip(TOK_WHILE);
                另 如 (tok == TOK_WHILE_CN)
                        skip(TOK_WHILE_CN);
                skip('(');
                gsym(b);
                gexpr();
                c = gvtst(0, 0);
                gsym_addr(c, d);
                nocode_wanted = saved_nocode_wanted;
                skip(')');
                gsym(a);
                skip(';');
        } 另
        如 (tok == TOK_SWITCH || tok == TOK_SWITCH_CN) {
                结构 switch_t *saved, sw;
                整 saved_nocode_wanted = nocode_wanted;
                SValue switchval;
                next();
                skip('(');
                gexpr();
                skip(')');
                switchval = *vtop--;
                a = 0;
                b = gjmp(0); /* jump to first case */
                sw.p = NULL; sw.n = 0; sw.def_sym = 0;
                saved = cur_switch;
                cur_switch = &sw;
                block(&a, csym, 0);
                nocode_wanted = saved_nocode_wanted;
                a = gjmp(a); /* add implicit break */
                /* case lookup */
                gsym(b);
                qsort(sw.p, sw.n, 求长度(空*), case_cmp);
                对于 (b = 1; b < sw.n; b++)
                        如 (sw.p[b - 1]->v2 >= sw.p[b]->v1)
                                tcc_error("duplicate case value");
                /* Our switch table sorting is signed, so the compared
                   value needs to be as well when it's 64bit.  */
                如 ((switchval.type.t & VT_BTYPE) == VT_LLONG)
                        switchval.type.t &= ~VT_UNSIGNED;
                vpushv(&switchval);
                gcase(sw.p, sw.n, &a);
                vpop();
                如 (sw.def_sym)
                  gjmp_addr(sw.def_sym);
                dynarray_reset(&sw.p, &sw.n);
                cur_switch = saved;
                /* break label */
                gsym(a);
        } 另
        如 (tok == TOK_CASE || tok == TOK_CASE_CN) {
                结构 case_t *cr = tcc_malloc(求长度(结构 case_t));
                如 (!cur_switch)
                        expect("switch");
                nocode_wanted &= ~0x20000000;
                next();
                cr->v1 = cr->v2 = expr_const64();
                如 (gnu_ext && tok == TOK_DOTS) {
                        next();
                        cr->v2 = expr_const64();
                        如 (cr->v2 < cr->v1)
                                tcc_warning("empty case range");
                }
                cr->sym = ind;
                dynarray_add(&cur_switch->p, &cur_switch->n, cr);
                skip(':');
                is_expr = 0;
                跳转 block_after_label;
        } 另
        如 (tok == TOK_DEFAULT || tok == TOK_DEFAULT_CN) {
                next();
                skip(':');
                如 (!cur_switch)
                        expect("switch");
                如 (cur_switch->def_sym)
                        tcc_error("too many 'default'");
                cur_switch->def_sym = ind;
                is_expr = 0;
                跳转 block_after_label;
        } 另
        如 (tok == TOK_GOTO || tok == TOK_GOTO_CN) {
                next();
                如 (tok == '*' && gnu_ext) {
                        /* computed goto */
                        next();
                        gexpr();
                        如 ((vtop->type.t & VT_BTYPE) != VT_PTR)
                                expect("pointer");
                        ggoto();
                } 另 如 (tok >= TOK_UIDENT) {
                        s = label_find(tok);
                        /* put forward definition if needed */
                        如 (!s) {
                                s = label_push(&global_label_stack, tok, LABEL_FORWARD);
                        } 另 {
                                如 (s->r == LABEL_DECLARED)
                                        s->r = LABEL_FORWARD;
                        }
                        vla_sp_restore_root();
                        如 (s->r & LABEL_FORWARD)
                                s->jnext = gjmp(s->jnext);
                        另
                                gjmp_addr(s->jnext);
                        next();
                } 另 {
                        expect("label identifier");
                }
                skip(';');
        } 另 如 (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3 || tok == TOK_ASM1_CN || tok == TOK_ASM3_CN) {
                asm_instr();
        } 另 {
                b = is_label();
                如 (b) {
                        /* label case */
                        next();
                        s = label_find(b);
                        如 (s) {
                                如 (s->r == LABEL_DEFINED)
                                        tcc_error("duplicate label '%s'", get_tok_str(s->v, NULL));
                                gsym(s->jnext);
                                s->r = LABEL_DEFINED;
                        } 另 {
                                s = label_push(&global_label_stack, b, LABEL_DEFINED);
                        }
                        s->jnext = ind;
                        vla_sp_restore();
                        /* we accept this, but it is a mistake */
                block_after_label:
                        nocode_wanted &= ~0x20000000;
                        如 (tok == '}') {
                                tcc_warning("deprecated use of label at end of compound statement");
                        } 另 {
                                如 (is_expr)
                                        vpop();
                                block(bsym, csym, is_expr);
                        }
                } 另 {
                        /* expression case */
                        如 (tok != ';') {
                                如 (is_expr) {
                                        vpop();
                                        gexpr();
                                } 另 {
                                        gexpr();
                                        vpop();
                                }
                        }
                        skip(';');
                }
        }
}

/* This skips over a stream of tokens containing balanced {} and ()
   pairs, stopping at outer ',' ';' and '}' (or matching '}' if we started
   with a '{').  If STR then allocates and stores the skipped tokens
   in *STR.  This doesn't check if () and {} are nested correctly,
   i.e. "({)}" is accepted.  */
静态 空 skip_or_save_block(TokenString **str)
{
        整 braces = tok == '{';
        整 level = 0;
        如 (str)
          *str = tok_str_alloc();

        当 ((level > 0 || (tok != '}' && tok != ',' && tok != ';' && tok != ')'))) {
                整 t;
                如 (tok == TOK_EOF) {
                        如 (str || level > 0)
                                tcc_error("unexpected end of file");
                        另
                                跳出;
                }
                如 (str)
                        tok_str_add_tok(*str);
                t = tok;
                next();
                如 (t == '{' || t == '(') {
                        level++;
                } 另 如 (t == '}' || t == ')') {
                        level--;
                        如 (level == 0 && braces && t == '}')
                                跳出;
                }
        }
        如 (str) {
                tok_str_add(*str, -1);
                tok_str_add(*str, 0);
        }
}

#定义 EXPR_CONST 1
#定义 EXPR_ANY   2

静态 空 parse_init_elem(整 expr_type)
{
        整 saved_global_expr;
        转接(expr_type) {
        事例 EXPR_CONST:
                /* compound literals must be allocated globally in this case */
                saved_global_expr = global_expr;
                global_expr = 1;
                expr_const1();
                global_expr = saved_global_expr;
                /* NOTE: symbols are accepted, as well as lvalue for anon symbols
                   (compound literals).  */
                如 (((vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST
                        && ((vtop->r & (VT_SYM|VT_LVAL)) != (VT_SYM|VT_LVAL)
                                || vtop->sym->v < SYM_FIRST_ANOM))
#如定义 TCC_TARGET_PE
                                || ((vtop->r & VT_SYM) && vtop->sym->a.dllimport)
#了如
                        )
                        tcc_error("initializer element is not constant");
                跳出;
        事例 EXPR_ANY:
                expr_eq();
                跳出;
        }
}

/* put zeros for variable based init */
静态 空 init_putz(Section *sec, 无符 长 c, 整 size)
{
        如 (sec) {
                /* nothing to do because globals are already set to zero */
        } 另 {
                vpush_global_sym(&func_old_type, TOK_memset);
                vseti(VT_LOCAL, c);
#如定义 TCC_TARGET_ARM
                vpushs(size);
                vpushi(0);
#另
                vpushi(0);
                vpushs(size);
#了如
                gfunc_call(3);
        }
}

/* t is the array or struct type. c is the array or struct
   address. cur_field is the pointer to the current
   field, for arrays the 'c' member contains the current start
   index.  'size_only' is true if only size info is needed (only used
   in arrays).  al contains the already initialized length of the
   current container (starting at c).  This returns the new length of that.  */
静态 整 decl_designator(CType *type, Section *sec, 无符 长 c,
                                                   Sym **cur_field, 整 size_only, 整 al)
{
        Sym *s, *f;
        整 index, index_last, align, l, nb_elems, elem_size;
        无符 长 corig = c;

        elem_size = 0;
        nb_elems = 1;
        如 (gnu_ext && (l = is_label()) != 0)
                跳转 struct_field;
        /* NOTE: we only support ranges for last designator */
        当 (nb_elems == 1 && (tok == '[' || tok == '.')) {
                如 (tok == '[') {
                        如 (!(type->t & VT_ARRAY))
                                expect("array type");
                        next();
                        index = index_last = expr_const();
                        如 (tok == TOK_DOTS && gnu_ext) {
                                next();
                                index_last = expr_const();
                        }
                        skip(']');
                        s = type->ref;
                        如 (index < 0 || (s->c >= 0 && index_last >= s->c) ||
                                index_last < index)
                                tcc_error("invalid index");
                        如 (cur_field)
                                (*cur_field)->c = index_last;
                        type = pointed_type(type);
                        elem_size = type_size(type, &align);
                        c += index * elem_size;
                        nb_elems = index_last - index + 1;
                } 另 {
                        next();
                        l = tok;
                struct_field:
                        next();
                        如 ((type->t & VT_BTYPE) != VT_STRUCT)
                                expect("struct/union type");
                        f = find_field(type, l);
                        如 (!f)
                                expect("field");
                        如 (cur_field)
                                *cur_field = f;
                        type = &f->type;
                        c += f->c;
                }
                cur_field = NULL;
        }
        如 (!cur_field) {
                如 (tok == '=') {
                        next();
                } 另 如 (!gnu_ext) {
                        expect("=");
                }
        } 另 {
                如 (type->t & VT_ARRAY) {
                        index = (*cur_field)->c;
                        如 (type->ref->c >= 0 && index >= type->ref->c)
                                tcc_error("index too large");
                        type = pointed_type(type);
                        c += index * type_size(type, &align);
                } 另 {
                        f = *cur_field;
                        当 (f && (f->v & SYM_FIRST_ANOM) && (f->type.t & VT_BITFIELD))
                                *cur_field = f = f->next;
                        如 (!f)
                                tcc_error("too many field init");
                        type = &f->type;
                        c += f->c;
                }
        }
        /* must put zero in holes (note that doing it that way
           ensures that it even works with designators) */
        如 (!size_only && c - corig > al)
                init_putz(sec, corig + al, c - corig - al);
        decl_initializer(type, sec, c, 0, size_only);

        /* XXX: make it more general */
        如 (!size_only && nb_elems > 1) {
                无符 长 c_end;
                uint8_t *src, *dst;
                整 i;

                如 (!sec) {
                        vset(type, VT_LOCAL|VT_LVAL, c);
                        对于 (i = 1; i < nb_elems; i++) {
                                vset(type, VT_LOCAL|VT_LVAL, c + elem_size * i);
                                vswap();
                                vstore();
                        }
                        vpop();
                } 另 如 (!NODATA_WANTED) {
                        c_end = c + nb_elems * elem_size;
                        如 (c_end > sec->data_allocated)
                                section_realloc(sec, c_end);
                        src = sec->data + c;
                        dst = src;
                        对于(i = 1; i < nb_elems; i++) {
                                dst += elem_size;
                                memcpy(dst, src, elem_size);
                        }
                }
        }
        c += nb_elems * type_size(type, &align);
        如 (c - corig > al)
          al = c - corig;
        返回 al;
}

/* store a value or an expression directly in global data or in local array */
静态 空 init_putv(CType *type, Section *sec, 无符 长 c)
{
        整 bt;
        空 *ptr;
        CType dtype;

        dtype = *type;
        dtype.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */

        如 (sec) {
                整 size, align;
                /* XXX: not portable */
                /* XXX: generate error if incorrect relocation */
                gen_assign_cast(&dtype);
                bt = type->t & VT_BTYPE;

                如 ((vtop->r & VT_SYM)
                        && bt != VT_PTR
                        && bt != VT_FUNC
                        && (bt != (PTR_SIZE == 8 ? VT_LLONG : VT_INT)
                                || (type->t & VT_BITFIELD))
                        && !((vtop->r & VT_CONST) && vtop->sym->v >= SYM_FIRST_ANOM)
                        )
                        tcc_error("initializer element is not computable at load time");

                如 (NODATA_WANTED) {
                        vtop--;
                        返回;
                }

                size = type_size(type, &align);
                section_reserve(sec, c + size);
                ptr = sec->data + c;

                /* XXX: make code faster ? */
                如 ((vtop->r & (VT_SYM|VT_CONST)) == (VT_SYM|VT_CONST) &&
                        vtop->sym->v >= SYM_FIRST_ANOM &&
                        /* XXX This rejects compound literals like
                           '(void *){ptr}'.  The problem is that '&sym' is
                           represented the same way, which would be ruled out
                           by the SYM_FIRST_ANOM check above, but also '"string"'
                           in 'char *p = "string"' is represented the same
                           with the type being VT_PTR and the symbol being an
                           anonymous one.  That is, there's no difference in vtop
                           between '(void *){x}' and '&(void *){x}'.  Ignore
                           pointer typed entities here.  Hopefully no real code
                           will every use compound literals with scalar type.  */
                        (vtop->type.t & VT_BTYPE) != VT_PTR) {
                        /* These come from compound literals, memcpy stuff over.  */
                        Section *ssec;
                        ElfW(Sym) *esym;
                        ElfW_Rel *rel;
                        esym = &((ElfW(Sym) *)symtab_section->data)[vtop->sym->c];
                        ssec = tcc_state->sections[esym->st_shndx];
                        memmove (ptr, ssec->data + esym->st_value, size);
                        如 (ssec->reloc) {
                                /* We need to copy over all memory contents, and that
                                   includes relocations.  Use the fact that relocs are
                                   created it order, so look from the end of relocs
                                   until we hit one before the copied region.  */
                                整 num_relocs = ssec->reloc->data_offset / 求长度(*rel);
                                rel = (ElfW_Rel*)(ssec->reloc->data + ssec->reloc->data_offset);
                                当 (num_relocs--) {
                                        rel--;
                                        如 (rel->r_offset >= esym->st_value + size)
                                                继续;
                                        如 (rel->r_offset < esym->st_value)
                                                跳出;
                                        /* Note: if the same fields are initialized multiple
                                           times (possible with designators) then we possibly
                                           add multiple relocations for the same offset here.
                                           That would lead to wrong code, the last reloc needs
                                           to win.  We clean this up later after the whole
                                           initializer is parsed.  */
                                        put_elf_reloca(symtab_section, sec,
                                                                   c + rel->r_offset - esym->st_value,
                                                                   ELFW(R_TYPE)(rel->r_info),
                                                                   ELFW(R_SYM)(rel->r_info),
#如 PTR_SIZE == 8
                                                                   rel->r_addend
#另
                                                                   0
#了如
                                                                  );
                                }
                        }
                } 另 {
                        如 (type->t & VT_BITFIELD) {
                                整 bit_pos, bit_size, bits, n;
                                无符 字 *p, v, m;
                                bit_pos = BIT_POS(vtop->type.t);
                                bit_size = BIT_SIZE(vtop->type.t);
                                p = (无符 字*)ptr + (bit_pos >> 3);
                                bit_pos &= 7, bits = 0;
                                当 (bit_size) {
                                        n = 8 - bit_pos;
                                        如 (n > bit_size)
                                                n = bit_size;
                                        v = vtop->c.i >> bits << bit_pos;
                                        m = ((1 << n) - 1) << bit_pos;
                                        *p = (*p & ~m) | (v & m);
                                        bits += n, bit_size -= n, bit_pos = 0, ++p;
                                }
                        } 另
                        转接(bt) {
                                /* XXX: when cross-compiling we assume that each type has the
                                   same representation on host and target, which is likely to
                                   be wrong in the case of long double */
                        事例 VT_BOOL:
                                vtop->c.i = vtop->c.i != 0;
                        事例 VT_BYTE:
                                *(字 *)ptr |= vtop->c.i;
                                跳出;
                        事例 VT_SHORT:
                                *(短 *)ptr |= vtop->c.i;
                                跳出;
                        事例 VT_FLOAT:
                                *(单精*)ptr = vtop->c.f;
                                跳出;
                        事例 VT_DOUBLE:
                                *(双精 *)ptr = vtop->c.d;
                                跳出;
                        事例 VT_LDOUBLE:
                                如 (求长度(长 双精) == LDOUBLE_SIZE)
                                        *(长 双精 *)ptr = vtop->c.ld;
                                另 如 (求长度(双精) == LDOUBLE_SIZE)
                                        *(双精 *)ptr = (双精)vtop->c.ld;
#如 (已定义 __i386__ || 已定义 __x86_64__) && (已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64)
                                另 如 (求长度 (长 双精) >= 10)
                                        memcpy(memset(ptr, 0, LDOUBLE_SIZE), &vtop->c.ld, 10);
#如定义 __TINYC__
                                另 如 (求长度 (长 双精) == 求长度 (双精))
                                        __asm__("fldl %1\nfstpt %0\n" : "=m"
                                                (memset(ptr, 0, LDOUBLE_SIZE), ptr) : "m" (vtop->c.ld));
#了如
#了如
                                另
                                        tcc_error("can't cross compile long double constants");
                                跳出;
#如 PTR_SIZE != 8
                        事例 VT_LLONG:
                                *(长 长 *)ptr |= vtop->c.i;
                                跳出;
#另
                        事例 VT_LLONG:
#了如
                        事例 VT_PTR:
                                {
                                        addr_t val = vtop->c.i;
#如 PTR_SIZE == 8
                                        如 (vtop->r & VT_SYM)
                                                greloca(sec, vtop->sym, c, R_DATA_PTR, val);
                                        另
                                                *(addr_t *)ptr |= val;
#另
                                        如 (vtop->r & VT_SYM)
                                                greloc(sec, vtop->sym, c, R_DATA_PTR);
                                        *(addr_t *)ptr |= val;
#了如
                                        跳出;
                                }
                        缺省:
                                {
                                        整 val = vtop->c.i;
#如 PTR_SIZE == 8
                                        如 (vtop->r & VT_SYM)
                                                greloca(sec, vtop->sym, c, R_DATA_PTR, val);
                                        另
                                                *(整 *)ptr |= val;
#另
                                        如 (vtop->r & VT_SYM)
                                                greloc(sec, vtop->sym, c, R_DATA_PTR);
                                        *(整 *)ptr |= val;
#了如
                                        跳出;
                                }
                        }
                }
                vtop--;
        } 另 {
                vset(&dtype, VT_LOCAL|VT_LVAL, c);
                vswap();
                vstore();
                vpop();
        }
}

/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'first' is true if array '{' must be read (multi
   dimension implicit array init handling). 'size_only' is true if
   size only evaluation is wanted (only for arrays). */
静态 空 decl_initializer(CType *type, Section *sec, 无符 长 c,
                                                         整 first, 整 size_only)
{
        整 len, n, no_oblock, nb, i;
        整 size1, align1;
        整 have_elem;
        Sym *s, *f;
        Sym indexsym;
        CType *t1;

        /* If we currently are at an '}' or ',' we have read an initializer
           element in one of our callers, and not yet consumed it.  */
        have_elem = tok == '}' || tok == ',';
        如 (!have_elem && tok != '{' &&
                /* In case of strings we have special handling for arrays, so
                   don't consume them as initializer value (which would commit them
                   to some anonymous symbol).  */
                tok != TOK_LSTR && tok != TOK_STR &&
                !size_only) {
                parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
                have_elem = 1;
        }

        如 (have_elem &&
                !(type->t & VT_ARRAY) &&
                /* Use i_c_parameter_t, to strip toplevel qualifiers.
                   The source type might have VT_CONSTANT set, which is
                   of course assignable to non-const elements.  */
                is_compatible_unqualified_types(type, &vtop->type)) {
                init_putv(type, sec, c);
        } 另 如 (type->t & VT_ARRAY) {
                s = type->ref;
                n = s->c;
                t1 = pointed_type(type);
                size1 = type_size(t1, &align1);

                no_oblock = 1;
                如 ((first && tok != TOK_LSTR && tok != TOK_STR) ||
                        tok == '{') {
                        如 (tok != '{')
                                tcc_error("character array initializer must be a literal,"
                                        " optionally enclosed in braces");
                        skip('{');
                        no_oblock = 0;
                }

                /* only parse strings here if correct type (otherwise: handle
                   them as ((w)char *) expressions */
                如 ((tok == TOK_LSTR &&
#如定义 TCC_TARGET_PE
                         (t1->t & VT_BTYPE) == VT_SHORT && (t1->t & VT_UNSIGNED)
#另
                         (t1->t & VT_BTYPE) == VT_INT
#了如
                        ) || (tok == TOK_STR && (t1->t & VT_BTYPE) == VT_BYTE)) {
                        len = 0;
                        当 (tok == TOK_STR || tok == TOK_LSTR) {
                                整 cstr_len, ch;

                                /* compute maximum number of chars wanted */
                                如 (tok == TOK_STR)
                                        cstr_len = tokc.str.size;
                                另
                                        cstr_len = tokc.str.size / 求长度(nwchar_t);
                                cstr_len--;
                                nb = cstr_len;
                                如 (n >= 0 && nb > (n - len))
                                        nb = n - len;
                                如 (!size_only) {
                                        如 (cstr_len > nb)
                                                tcc_warning("initializer-string for array is too long");
                                        /* in order to go faster for common case (char
                                           string in global variable, we handle it
                                           specifically */
                                        如 (sec && tok == TOK_STR && size1 == 1) {
                                                如 (!NODATA_WANTED)
                                                        memcpy(sec->data + c + len, tokc.str.data, nb);
                                        } 另 {
                                                对于(i=0;i<nb;i++) {
                                                        如 (tok == TOK_STR)
                                                                ch = ((无符 字 *)tokc.str.data)[i];
                                                        另
                                                                ch = ((nwchar_t *)tokc.str.data)[i];
                                                        vpushi(ch);
                                                        init_putv(t1, sec, c + (len + i) * size1);
                                                }
                                        }
                                }
                                len += nb;
                                next();
                        }
                        /* only add trailing zero if enough storage (no
                           warning in this case since it is standard) */
                        如 (n < 0 || len < n) {
                                如 (!size_only) {
                                        vpushi(0);
                                        init_putv(t1, sec, c + (len * size1));
                                }
                                len++;
                        }
                        len *= size1;
                } 另 {
                        indexsym.c = 0;
                        f = &indexsym;

                do_init_list:
                        len = 0;
                        当 (tok != '}' || have_elem) {
                                len = decl_designator(type, sec, c, &f, size_only, len);
                                have_elem = 0;
                                如 (type->t & VT_ARRAY) {
                                        ++indexsym.c;
                                        /* special test for multi dimensional arrays (may not
                                           be strictly correct if designators are used at the
                                           same time) */
                                        如 (no_oblock && len >= n*size1)
                                                跳出;
                                } 另 {
                                        如 (s->type.t == VT_UNION)
                                                f = NULL;
                                        另
                                                f = f->next;
                                        如 (no_oblock && f == NULL)
                                                跳出;
                                }

                                如 (tok == '}')
                                        跳出;
                                skip(',');
                        }
                }
                /* put zeros at the end */
                如 (!size_only && len < n*size1)
                        init_putz(sec, c + len, n*size1 - len);
                如 (!no_oblock)
                        skip('}');
                /* patch type size if needed, which happens only for array types */
                如 (n < 0)
                        s->c = size1 == 1 ? len : ((len + size1 - 1)/size1);
        } 另 如 ((type->t & VT_BTYPE) == VT_STRUCT) {
                size1 = 1;
                no_oblock = 1;
                如 (first || tok == '{') {
                        skip('{');
                        no_oblock = 0;
                }
                s = type->ref;
                f = s->next;
                n = s->c;
                跳转 do_init_list;
        } 另 如 (tok == '{') {
                next();
                decl_initializer(type, sec, c, first, size_only);
                skip('}');
        } 另 如 (size_only) {
                /* If we supported only ISO C we wouldn't have to accept calling
                   this on anything than an array size_only==1 (and even then
                   only on the outermost level, so no recursion would be needed),
                   because initializing a flex array member isn't supported.
                   But GNU C supports it, so we need to recurse even into
                   subfields of structs and arrays when size_only is set.  */
                /* just skip expression */
                skip_or_save_block(NULL);
        } 另 {
                如 (!have_elem) {
                        /* This should happen only when we haven't parsed
                           the init element above for fear of committing a
                           string constant to memory too early.  */
                        如 (tok != TOK_STR && tok != TOK_LSTR)
                                expect("string constant");
                        parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
                }
                init_putv(type, sec, c);
        }
}

/* parse an initializer for type 't' if 'has_init' is non zero, and
   allocate space in local or global data space ('r' is either
   VT_LOCAL or VT_CONST). If 'v' is non zero, then an associated
   variable 'v' of scope 'scope' is declared before initializers
   are parsed. If 'v' is zero, then a reference to the new object
   is put in the value stack. If 'has_init' is 2, a special parsing
   is done to handle string constants. */
静态 空 decl_initializer_alloc(CType *type, AttributeDef *ad, 整 r,
                                                                   整 has_init, 整 v, 整 scope)
{
        整 size, align, addr;
        TokenString *init_str = NULL;

        Section *sec;
        Sym *flexible_array;
        Sym *sym = NULL;
        整 saved_nocode_wanted = nocode_wanted;
#如定义 CONFIG_TCC_BCHECK
        整 bcheck = tcc_state->do_bounds_check && !NODATA_WANTED;
#了如

        如 (type->t & VT_STATIC)
                nocode_wanted |= NODATA_WANTED ? 0x40000000 : 0x80000000;

        flexible_array = NULL;
        如 ((type->t & VT_BTYPE) == VT_STRUCT) {
                Sym *field = type->ref->next;
                如 (field) {
                        当 (field->next)
                                field = field->next;
                        如 (field->type.t & VT_ARRAY && field->type.ref->c < 0)
                                flexible_array = field;
                }
        }

        size = type_size(type, &align);
        /* If unknown size, we must evaluate it before
           evaluating initializers because
           initializers can generate global data too
           (e.g. string pointers or ISOC99 compound
           literals). It also simplifies local
           initializers handling */
        如 (size < 0 || (flexible_array && has_init)) {
                如 (!has_init)
                        tcc_error("unknown type size");
                /* get all init string */
                如 (has_init == 2) {
                        init_str = tok_str_alloc();
                        /* only get strings */
                        当 (tok == TOK_STR || tok == TOK_LSTR) {
                                tok_str_add_tok(init_str);
                                next();
                        }
                        tok_str_add(init_str, -1);
                        tok_str_add(init_str, 0);
                } 另 {
                        skip_or_save_block(&init_str);
                }
                unget_tok(0);

                /* compute size */
                begin_macro(init_str, 1);
                next();
                decl_initializer(type, NULL, 0, 1, 1);
                /* prepare second initializer parsing */
                macro_ptr = init_str->str;
                next();

                /* if still unknown size, error */
                size = type_size(type, &align);
                如 (size < 0)
                        tcc_error("unknown type size");
        }
        /* If there's a flex member and it was used in the initializer
           adjust size.  */
        如 (flexible_array &&
                flexible_array->type.ref->c > 0)
                size += flexible_array->type.ref->c
                        * pointed_size(&flexible_array->type);
        /* take into account specified alignment if bigger */
        如 (ad->a.aligned) {
                整 speca = 1 << (ad->a.aligned - 1);
                如 (speca > align)
                        align = speca;
        } 另 如 (ad->a.packed) {
                align = 1;
        }

        如 (NODATA_WANTED)
                size = 0, align = 1;

        如 ((r & VT_VALMASK) == VT_LOCAL) {
                sec = NULL;
#如定义 CONFIG_TCC_BCHECK
                如 (bcheck && (type->t & VT_ARRAY)) {
                        loc--;
                }
#了如
                loc = (loc - size) & -align;
                addr = loc;
#如定义 CONFIG_TCC_BCHECK
                /* handles bounds */
                /* XXX: currently, since we do only one pass, we cannot track
                   '&' operators, so we add only arrays */
                如 (bcheck && (type->t & VT_ARRAY)) {
                        addr_t *bounds_ptr;
                        /* add padding between regions */
                        loc--;
                        /* then add local bound info */
                        bounds_ptr = section_ptr_add(lbounds_section, 2 * 求长度(addr_t));
                        bounds_ptr[0] = addr;
                        bounds_ptr[1] = size;
                }
#了如
                如 (v) {
                        /* local variable */
#如定义 CONFIG_TCC_ASM
                        如 (ad->asm_label) {
                                整 reg = asm_parse_regvar(ad->asm_label);
                                如 (reg >= 0)
                                        r = (r & ~VT_VALMASK) | reg;
                        }
#了如
                        sym = sym_push(v, type, r, addr);
                        sym->a = ad->a;
                } 另 {
                        /* push local reference */
                        vset(type, r, addr);
                }
        } 另 {
                如 (v && scope == VT_CONST) {
                        /* see if the symbol was already defined */
                        sym = sym_find(v);
                        如 (sym) {
                                patch_storage(sym, ad, type);
                                如 (sym->type.t & VT_EXTERN) {
                                        /* if the variable is extern, it was not allocated */
                                        sym->type.t &= ~VT_EXTERN;
                                        /* set array size if it was omitted in extern
                                           declaration */
                                        如 ((sym->type.t & VT_ARRAY) &&
                                                sym->type.ref->c < 0 &&
                                                type->ref->c >= 0)
                                                sym->type.ref->c = type->ref->c;
                                } 另 如 (!has_init) {
                                        /* we accept several definitions of the same
                                           global variable. this is tricky, because we
                                           must play with the SHN_COMMON type of the symbol */
                                        /* no init data, we won't add more to the symbol */
                                        跳转 no_alloc;
                                } 另 如 (sym->c) {
                                        ElfW(Sym) *esym;
                                        esym = &((ElfW(Sym) *)symtab_section->data)[sym->c];
                                        如 (esym->st_shndx == data_section->sh_num)
                                                tcc_error("redefinition of '%s'", get_tok_str(v, NULL));
                                }
                        }
                }

                /* allocate symbol in corresponding section */
                sec = ad->section;
                如 (!sec) {
                        如 (has_init)
                                sec = data_section;
                        另 如 (tcc_state->nocommon)
                                sec = bss_section;
                }

                如 (sec) {
                        addr = section_add(sec, size, align);
#如定义 CONFIG_TCC_BCHECK
                        /* add padding if bound check */
                        如 (bcheck)
                                section_add(sec, 1, 1);
#了如
                } 另 {
                        addr = align; /* SHN_COMMON is special, symbol value is align */
                        sec = common_section;
                }

                如 (v) {
                        如 (!sym) {
                                sym = sym_push(v, type, r | VT_SYM, 0);
                                patch_storage(sym, ad, NULL);
                        }
                        /* Local statics have a scope until now (for
                           warnings), remove it here.  */
                        sym->sym_scope = 0;
                        /* update symbol definition */
                        put_extern_sym(sym, sec, addr, size);
                } 另 {
                        /* push global reference */
                        sym = get_sym_ref(type, sec, addr, size);
                        vpushsym(type, sym);
                        vtop->r |= r;
                }

#如定义 CONFIG_TCC_BCHECK
                /* handles bounds now because the symbol must be defined
                   before for the relocation */
                如 (bcheck) {
                        addr_t *bounds_ptr;

                        greloca(bounds_section, sym, bounds_section->data_offset, R_DATA_PTR, 0);
                        /* then add global bound info */
                        bounds_ptr = section_ptr_add(bounds_section, 2 * 求长度(addr_t));
                        bounds_ptr[0] = 0; /* relocated */
                        bounds_ptr[1] = size;
                }
#了如
        }

        如 (type->t & VT_VLA) {
                整 a;

                如 (NODATA_WANTED)
                        跳转 no_alloc;

                /* save current stack pointer */
                如 (vlas_in_scope == 0) {
                        如 (vla_sp_root_loc == -1)
                                vla_sp_root_loc = (loc -= PTR_SIZE);
                        gen_vla_sp_save(vla_sp_root_loc);
                }

                vla_runtime_type_size(type, &a);
                gen_vla_alloc(type, a);
                gen_vla_sp_save(addr);
                vla_sp_loc = addr;
                vlas_in_scope++;

        } 另 如 (has_init) {
                size_t oldreloc_offset = 0;
                如 (sec && sec->reloc)
                        oldreloc_offset = sec->reloc->data_offset;
                decl_initializer(type, sec, addr, 1, 0);
                如 (sec && sec->reloc)
                        squeeze_multi_relocs(sec, oldreloc_offset);
                /* patch flexible array member size back to -1, */
                /* for possible subsequent similar declarations */
                如 (flexible_array)
                        flexible_array->type.ref->c = -1;
        }

 no_alloc:
        /* restore parse state if needed */
        如 (init_str) {
                end_macro();
                next();
        }

        nocode_wanted = saved_nocode_wanted;
}

/* parse a function defined by symbol 'sym' and generate its code in
   'cur_text_section' */
静态 空 gen_function(Sym *sym)
{
        nocode_wanted = 0;
        ind = cur_text_section->data_offset;
        /* NOTE: we patch the symbol size later */
        put_extern_sym(sym, cur_text_section, ind, 0);
        funcname = get_tok_str(sym->v, NULL);
        func_ind = ind;
        /* Initialize VLA state */
        vla_sp_loc = -1;
        vla_sp_root_loc = -1;
        /* put debug symbol */
        tcc_debug_funcstart(tcc_state, sym);
        /* push a dummy symbol to enable local sym storage */
        sym_push2(&local_stack, SYM_FIELD, 0, 0);
        local_scope = 1; /* for function parameters */
        gfunc_prolog(&sym->type);
        local_scope = 0;
        rsym = 0;
        block(NULL, NULL, 0);
        nocode_wanted = 0;
        gsym(rsym);
        gfunc_epilog();
        cur_text_section->data_offset = ind;
        label_pop(&global_label_stack, NULL, 0);
        /* reset local stack */
        local_scope = 0;
        sym_pop(&local_stack, NULL, 0);
        /* end of function */
        /* patch symbol size */
        ((ElfW(Sym) *)symtab_section->data)[sym->c].st_size =
                ind - func_ind;
        tcc_debug_funcend(tcc_state, ind - func_ind);
        /* It's better to crash than to generate wrong code */
        cur_text_section = NULL;
        funcname = ""; /* for safety */
        func_vt.t = VT_VOID; /* for safety */
        func_var = 0; /* for safety */
        ind = 0; /* for safety */
        nocode_wanted = 0x80000000;
        check_vstack();
}

静态 空 gen_inline_functions(TCCState *s)
{
        Sym *sym;
        整 inline_generated, i, ln;
        结构 InlineFunc *fn;

        ln = file->line_num;
        /* iterate while inline function are referenced */
        运行 {
                inline_generated = 0;
                对于 (i = 0; i < s->nb_inline_fns; ++i) {
                        fn = s->inline_fns[i];
                        sym = fn->sym;
                        如 (sym && sym->c) {
                                /* the function was used: generate its code and
                                   convert it to a normal function */
                                fn->sym = NULL;
                                如 (file)
                                        pstrcpy(file->filename, 求长度 file->filename, fn->filename);
                                sym->type.t &= ~VT_INLINE;

                                begin_macro(fn->func_str, 1);
                                next();
                                cur_text_section = text_section;
                                gen_function(sym);
                                end_macro();

                                inline_generated = 1;
                        }
                }
        } 当 (inline_generated);
        file->line_num = ln;
}

ST_FUNC 空 free_inline_functions(TCCState *s)
{
        整 i;
        /* free tokens of unused inline functions */
        对于 (i = 0; i < s->nb_inline_fns; ++i) {
                结构 InlineFunc *fn = s->inline_fns[i];
                如 (fn->sym)
                        tok_str_free(fn->func_str);
        }
        dynarray_reset(&s->inline_fns, &s->nb_inline_fns);
}

/* 'l' is VT_LOCAL or VT_CONST to define default storage type, or VT_CMP
   if parsing old style parameter decl list (and FUNC_SYM is set then) */
静态 整 decl0(整 l, 整 is_for_loop_init, Sym *func_sym)
{
        整 v, has_init, r;
        CType type, btype;
        Sym *sym;
        AttributeDef ad;

        当 (1) {
                如 (!parse_btype(&btype, &ad)) {
                        如 (is_for_loop_init)
                                返回 0;
                        /* skip redundant ';' if not in old parameter decl scope */
                        如 (tok == ';' && l != VT_CMP) {
                                next();
                                继续;
                        }
                        如 (l == VT_CONST &&
                                (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3 || tok == TOK_ASM1_CN || tok == TOK_ASM3_CN)) {
                                /* global asm block */
                                asm_global_instr();
                                继续;
                        }
                        /* special test for old K&R protos without explicit int
                           type. Only accepted when defining global data */
                        如 (l != VT_CONST || tok < TOK_UIDENT)
                                跳出;
                        btype.t = VT_INT;
                }
                如 (tok == ';') {
                        如 ((btype.t & VT_BTYPE) == VT_STRUCT) {
                                整 v = btype.ref->v;
                                如 (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) >= SYM_FIRST_ANOM)
                                        tcc_warning("unnamed struct/union that defines no instances");
                                next();
                                继续;
                        }
                        如 (IS_ENUM(btype.t)) {
                                next();
                                继续;
                        }
                }
                当 (1) { /* iterate thru each declaration */
                        type = btype;
                        /* If the base type itself was an array type of unspecified
                           size (like in 'typedef int arr[]; arr x = {1};') then
                           we will overwrite the unknown size by the real one for
                           this decl.  We need to unshare the ref symbol holding
                           that size.  */
                        如 ((type.t & VT_ARRAY) && type.ref->c < 0) {
                                type.ref = sym_push(SYM_FIELD, &type.ref->type, 0, type.ref->c);
                        }
                        type_decl(&type, &ad, &v, TYPE_DIRECT);
#如 0
                        {
                                字 buf[500];
                                type_to_str(buf, 求长度(buf), &type, get_tok_str(v, NULL));
                                printf("type = '%s'\n", buf);
                        }
#了如
                        如 ((type.t & VT_BTYPE) == VT_FUNC) {
                                如 ((type.t & VT_STATIC) && (l == VT_LOCAL)) {
                                        tcc_error("function without file scope cannot be static");
                                }
                                /* if old style function prototype, we accept a
                                   declaration list */
                                sym = type.ref;
                                如 (sym->f.func_type == FUNC_OLD && l == VT_CONST)
                                        decl0(VT_CMP, 0, sym);
                        }

                        如 (gnu_ext && (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3 || tok == TOK_ASM1_CN || tok == TOK_ASM3_CN)) {
                                ad.asm_label = asm_label_instr();
                                /* parse one last attribute list, after asm label */
                                parse_attribute(&ad);
                                如 (tok == '{')
                                        expect(";");
                        }

#如定义 TCC_TARGET_PE
                        如 (ad.a.dllimport || ad.a.dllexport) {
                                如 (type.t & (VT_STATIC|VT_TYPEDEF))
                                        tcc_error("cannot have dll linkage with static or typedef");
                                如 (ad.a.dllimport) {
                                        如 ((type.t & VT_BTYPE) == VT_FUNC)
                                                ad.a.dllimport = 0;
                                        另
                                                type.t |= VT_EXTERN;
                                }
                        }
#了如
                        如 (tok == '{') {
                                如 (l != VT_CONST)
                                        tcc_error("cannot use local functions");
                                如 ((type.t & VT_BTYPE) != VT_FUNC)
                                        expect("function definition");

                                /* reject abstract declarators in function definition
                                   make old style params without decl have int type */
                                sym = type.ref;
                                当 ((sym = sym->next) != NULL) {
                                        如 (!(sym->v & ~SYM_FIELD))
                                                expect("identifier");
                                        如 (sym->type.t == VT_VOID)
                                                sym->type = int_type;
                                }

                                /* XXX: cannot do better now: convert extern line to static inline */
                                如 ((type.t & (VT_EXTERN | VT_INLINE)) == (VT_EXTERN | VT_INLINE))
                                        type.t = (type.t & ~VT_EXTERN) | VT_STATIC;

                                sym = sym_find(v);
                                如 (sym) {
                                        Sym *ref;
                                        如 ((sym->type.t & VT_BTYPE) != VT_FUNC)
                                                跳转 func_error1;

                                        ref = sym->type.ref;

                                        /* use func_call from prototype if not defined */
                                        如 (ref->f.func_call != FUNC_CDECL
                                         && type.ref->f.func_call == FUNC_CDECL)
                                                type.ref->f.func_call = ref->f.func_call;

                                        /* use static from prototype */
                                        如 (sym->type.t & VT_STATIC)
                                                type.t = (type.t & ~VT_EXTERN) | VT_STATIC;

                                        /* If the definition has no visibility use the
                                           one from prototype.  */
                                        如 (!type.ref->a.visibility)
                                                type.ref->a.visibility = ref->a.visibility;
                                        /* apply other storage attributes from prototype */
                                        type.ref->a.dllexport |= ref->a.dllexport;
                                        type.ref->a.weak |= ref->a.weak;

                                        如 (!is_compatible_types(&sym->type, &type)) {
                                        func_error1:
                                                tcc_error("incompatible types for redefinition of '%s'",
                                                          get_tok_str(v, NULL));
                                        }
                                        如 (ref->f.func_body)
                                                tcc_error("redefinition of '%s'", get_tok_str(v, NULL));
                                        /* if symbol is already defined, then put complete type */
                                        sym->type = type;

                                } 另 {
                                        /* put function symbol */
                                        sym = global_identifier_push(v, type.t, 0);
                                        sym->type.ref = type.ref;
                                }

                                sym->type.ref->f.func_body = 1;
                                sym->r = VT_SYM | VT_CONST;
                                patch_storage(sym, &ad, NULL);

                                /* static inline functions are just recorded as a kind
                                   of macro. Their code will be emitted at the end of
                                   the compilation unit only if they are used */
                                如 ((type.t & (VT_INLINE | VT_STATIC)) ==
                                        (VT_INLINE | VT_STATIC)) {
                                        结构 InlineFunc *fn;
                                        不变 字 *filename;

                                        filename = file ? file->filename : "";
                                        fn = tcc_malloc(求长度 *fn + strlen(filename));
                                        strcpy(fn->filename, filename);
                                        fn->sym = sym;
                                        skip_or_save_block(&fn->func_str);
                                        dynarray_add(&tcc_state->inline_fns,
                                                &tcc_state->nb_inline_fns, fn);
                                } 另 {
                                        /* compute text section */
                                        cur_text_section = ad.section;
                                        如 (!cur_text_section)
                                                cur_text_section = text_section;
                                        gen_function(sym);
                                }
                                跳出;
                        } 另 {
                                如 (l == VT_CMP) {
                                        /* find parameter in function parameter list */
                                        对于 (sym = func_sym->next; sym; sym = sym->next)
                                                如 ((sym->v & ~SYM_FIELD) == v)
                                                        跳转 found;
                                        tcc_error("declaration for parameter '%s' but no such parameter",
                                                get_tok_str(v, NULL));
found:
                                        如 (type.t & VT_STORAGE) /* 'register' is okay */
                                                tcc_error("storage class specified for '%s'",
                                                        get_tok_str(v, NULL));
                                        如 (sym->type.t != VT_VOID)
                                                tcc_error("redefinition of parameter '%s'",
                                                        get_tok_str(v, NULL));
                                        convert_parameter_type(&type);
                                        sym->type = type;
                                } 另 如 (type.t & VT_TYPEDEF) {
                                        /* save typedefed type  */
                                        /* XXX: test storage specifiers ? */
                                        sym = sym_find(v);
                                        如 (sym && sym->sym_scope == local_scope) {
                                                如 (!is_compatible_types(&sym->type, &type)
                                                        || !(sym->type.t & VT_TYPEDEF))
                                                        tcc_error("incompatible redefinition of '%s'",
                                                                get_tok_str(v, NULL));
                                                sym->type = type;
                                        } 另 {
                                                sym = sym_push(v, &type, 0, 0);
                                        }
                                        sym->a = ad.a;
                                        sym->f = ad.f;
                                } 另 {
                                        r = 0;
                                        如 ((type.t & VT_BTYPE) == VT_FUNC) {
                                                /* external function definition */
                                                /* specific case for func_call attribute */
                                                type.ref->f = ad.f;
                                        } 另 如 (!(type.t & VT_ARRAY)) {
                                                /* not lvalue if array */
                                                r |= lvalue_type(type.t);
                                        }
                                        has_init = (tok == '=');
                                        如 (has_init && (type.t & VT_VLA))
                                                tcc_error("variable length array cannot be initialized");
                                        如 (((type.t & VT_EXTERN) && (!has_init || l != VT_CONST)) ||
                                                ((type.t & VT_BTYPE) == VT_FUNC) ||
                                                ((type.t & VT_ARRAY) && (type.t & VT_STATIC) &&
                                                 !has_init && l == VT_CONST && type.ref->c < 0)) {
                                                /* external variable or function */
                                                /* NOTE: as GCC, uninitialized global static
                                                   arrays of null size are considered as
                                                   extern */
                                                sym = external_sym(v, &type, r, &ad);
                                                如 (ad.alias_target) {
                                                        Section tsec;
                                                        ElfW(Sym) *esym;
                                                        Sym *alias_target;
                                                        alias_target = sym_find(ad.alias_target);
                                                        如 (!alias_target || !alias_target->c)
                                                                tcc_error("unsupported forward __alias__ attribute");
                                                        esym = &((ElfW(Sym) *)symtab_section->data)[alias_target->c];
                                                        tsec.sh_num = esym->st_shndx;
                                                        /* Local statics have a scope until now (for
                                                           warnings), remove it here.  */
                                                        sym->sym_scope = 0;
                                                        put_extern_sym2(sym, &tsec, esym->st_value, esym->st_size, 0);
                                                }
                                        } 另 {
                                                如 (type.t & VT_STATIC)
                                                        r |= VT_CONST;
                                                另
                                                        r |= l;
                                                如 (has_init)
                                                        next();
                                                decl_initializer_alloc(&type, &ad, r, has_init, v, l);
                                        }
                                }
                                如 (tok != ',') {
                                        如 (is_for_loop_init)
                                                返回 1;
                                        skip(';');
                                        跳出;
                                }
                                next();
                        }
                        ad.a.aligned = 0;
                }
        }
        返回 0;
}

静态 空 decl(整 l)
{
        decl0(l, 0, NULL);
}

/* ------------------------------------------------------------------------- */
