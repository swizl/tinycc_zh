/*
 *  GAS like assembler for TCC
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
#如定义 CONFIG_TCC_ASM

ST_FUNC 整 asm_get_local_label_name(TCCState *s1, 无符 整 n)
{
        字 buf[64];
        TokenSym *ts;

        snprintf(buf, 求长度(buf), "L..%u", n);
        ts = tok_alloc(buf, strlen(buf));
        返回 ts->tok;
}

ST_FUNC 空 asm_expr(TCCState *s1, ExprValue *pe);
静态 整 tcc_assemble_internal(TCCState *s1, 整 do_preprocess, 整 global);
静态 Sym sym_dot;

/* Return a symbol we can use inside the assembler, having name NAME.
   The assembler symbol table is different from the C symbol table
   (and the Sym members are used differently).  But we must be able
   to look up file-global C symbols from inside the assembler, e.g.
   for global asm blocks to be able to refer to defined C symbols.

   This routine gives back either an existing asm-internal
   symbol, or a new one.  In the latter case the new asm-internal
   symbol is initialized with info from the C symbol table.

   If CSYM is non-null we take symbol info from it, otherwise
   we look up NAME in the C symbol table and use that.  */
ST_FUNC Sym* get_asm_sym(整 name, Sym *csym)
{
        Sym *sym = label_find(name);
        如 (!sym) {
                sym = label_push(&tcc_state->asm_labels, name, 0);
                sym->type.t = VT_VOID | VT_EXTERN;
                如 (!csym) {
                        csym = sym_find(name);
                        /* We might be called for an asm block from inside a C routine
                           and so might have local decls on the identifier stack.  Search
                           for the first global one.  */
                        当 (csym && csym->sym_scope)
                                csym = csym->prev_tok;
                }
                /* Now, if we have a defined global symbol copy over
                   section and offset.  */
                如 (csym &&
                        ((csym->r & (VT_SYM|VT_CONST)) == (VT_SYM|VT_CONST)) &&
                        csym->c) {
                        ElfW(Sym) *esym;
                        esym = &((ElfW(Sym) *)symtab_section->data)[csym->c];
                        sym->c = csym->c;
                        sym->r = esym->st_shndx;
                        sym->jnext = esym->st_value;
                        /* XXX can't yet store st_size anywhere.  */
                        sym->type.t &= ~VT_EXTERN;
                        /* Mark that this asm symbol doesn't need to be fed back.  */
                        sym->a.dllimport = 1;
                }
        }
        返回 sym;
}

/* We do not use the C expression parser to handle symbols. Maybe the
   C expression parser could be tweaked to do so. */

静态 空 asm_expr_unary(TCCState *s1, ExprValue *pe)
{
        Sym *sym;
        整 op, label;
        uint64_t n;
        不变 字 *p;

        转接(tok) {
        事例 TOK_PPNUM:
                p = tokc.str.data;
                n = strtoull(p, (字 **)&p, 0);
                如 (*p == 'b' || *p == 'f') {
                        /* backward or forward label */
                        label = asm_get_local_label_name(s1, n);
                        sym = label_find(label);
                        如 (*p == 'b') {
                                /* backward : find the last corresponding defined label */
                                如 (sym && sym->r == 0)
                                        sym = sym->prev_tok;
                                如 (!sym)
                                        tcc_error("local label '%d' not found backward", n);
                        } 另 {
                                /* forward */
                                如 (!sym || sym->r) {
                                        /* if the last label is defined, then define a new one */
                                        sym = label_push(&s1->asm_labels, label, 0);
                                        sym->type.t = VT_STATIC | VT_VOID | VT_EXTERN;
                                }
                        }
                        pe->v = 0;
                        pe->sym = sym;
                        pe->pcrel = 0;
                } 另 如 (*p == '\0') {
                        pe->v = n;
                        pe->sym = NULL;
                        pe->pcrel = 0;
                } 另 {
                        tcc_error("invalid number syntax");
                }
                next();
                跳出;
        事例 '+':
                next();
                asm_expr_unary(s1, pe);
                跳出;
        事例 '-':
        事例 '~':
                op = tok;
                next();
                asm_expr_unary(s1, pe);
                如 (pe->sym)
                        tcc_error("invalid operation with label");
                如 (op == '-')
                        pe->v = -pe->v;
                另
                        pe->v = ~pe->v;
                跳出;
        事例 TOK_CCHAR:
        事例 TOK_LCHAR:
                pe->v = tokc.i;
                pe->sym = NULL;
                pe->pcrel = 0;
                next();
                跳出;
        事例 '(':
                next();
                asm_expr(s1, pe);
                skip(')');
                跳出;
        事例 '.':
                pe->v = 0;
                pe->sym = &sym_dot;
                pe->pcrel = 0;
                sym_dot.type.t = VT_VOID | VT_STATIC;
                sym_dot.r = cur_text_section->sh_num;
                sym_dot.jnext = ind;
                next();
                跳出;
        缺省:
                如 (tok >= TOK_IDENT) {
                        /* label case : if the label was not found, add one */
                        sym = get_asm_sym(tok, NULL);
                        如 (sym->r == SHN_ABS) {
                                /* if absolute symbol, no need to put a symbol value */
                                pe->v = sym->jnext;
                                pe->sym = NULL;
                                pe->pcrel = 0;
                        } 另 {
                                pe->v = 0;
                                pe->sym = sym;
                                pe->pcrel = 0;
                        }
                        next();
                } 另 {
                        tcc_error("bad expression syntax [%s]", get_tok_str(tok, &tokc));
                }
                跳出;
        }
}

静态 空 asm_expr_prod(TCCState *s1, ExprValue *pe)
{
        整 op;
        ExprValue e2;

        asm_expr_unary(s1, pe);
        对于(;;) {
                op = tok;
                如 (op != '*' && op != '/' && op != '%' &&
                        op != TOK_SHL && op != TOK_SAR)
                        跳出;
                next();
                asm_expr_unary(s1, &e2);
                如 (pe->sym || e2.sym)
                        tcc_error("invalid operation with label");
                转接(op) {
                事例 '*':
                        pe->v *= e2.v;
                        跳出;
                事例 '/':
                        如 (e2.v == 0) {
                        div_error:
                                tcc_error("division by zero");
                        }
                        pe->v /= e2.v;
                        跳出;
                事例 '%':
                        如 (e2.v == 0)
                                跳转 div_error;
                        pe->v %= e2.v;
                        跳出;
                事例 TOK_SHL:
                        pe->v <<= e2.v;
                        跳出;
                缺省:
                事例 TOK_SAR:
                        pe->v >>= e2.v;
                        跳出;
                }
        }
}

静态 空 asm_expr_logic(TCCState *s1, ExprValue *pe)
{
        整 op;
        ExprValue e2;

        asm_expr_prod(s1, pe);
        对于(;;) {
                op = tok;
                如 (op != '&' && op != '|' && op != '^')
                        跳出;
                next();
                asm_expr_prod(s1, &e2);
                如 (pe->sym || e2.sym)
                        tcc_error("invalid operation with label");
                转接(op) {
                事例 '&':
                        pe->v &= e2.v;
                        跳出;
                事例 '|':
                        pe->v |= e2.v;
                        跳出;
                缺省:
                事例 '^':
                        pe->v ^= e2.v;
                        跳出;
                }
        }
}

静态 内联 空 asm_expr_sum(TCCState *s1, ExprValue *pe)
{
        整 op;
        ExprValue e2;

        asm_expr_logic(s1, pe);
        对于(;;) {
                op = tok;
                如 (op != '+' && op != '-')
                        跳出;
                next();
                asm_expr_logic(s1, &e2);
                如 (op == '+') {
                        如 (pe->sym != NULL && e2.sym != NULL)
                                跳转 cannot_relocate;
                        pe->v += e2.v;
                        如 (pe->sym == NULL && e2.sym != NULL)
                                pe->sym = e2.sym;
                } 另 {
                        pe->v -= e2.v;
                        /* NOTE: we are less powerful than gas in that case
                           because we store only one symbol in the expression */
                        如 (!e2.sym) {
                                /* OK */
                        } 另 如 (pe->sym == e2.sym) {
                                /* OK */
                                pe->sym = NULL; /* same symbols can be subtracted to NULL */
                        } 另 如 (pe->sym && pe->sym->r == e2.sym->r && pe->sym->r != 0) {
                                /* we also accept defined symbols in the same section */
                                pe->v += pe->sym->jnext - e2.sym->jnext;
                                pe->sym = NULL;
                        } 另 如 (e2.sym->r == cur_text_section->sh_num) {
                                /* When subtracting a defined symbol in current section
                                   this actually makes the value PC-relative.  */
                                pe->v -= e2.sym->jnext - ind - 4;
                                pe->pcrel = 1;
                                e2.sym = NULL;
                        } 另 {
                        cannot_relocate:
                                tcc_error("invalid operation with label");
                        }
                }
        }
}

静态 内联 空 asm_expr_cmp(TCCState *s1, ExprValue *pe)
{
        整 op;
        ExprValue e2;

        asm_expr_sum(s1, pe);
        对于(;;) {
                op = tok;
                如 (op != TOK_EQ && op != TOK_NE
                        && (op > TOK_GT || op < TOK_ULE))
                        跳出;
                next();
                asm_expr_sum(s1, &e2);
                如 (pe->sym || e2.sym)
                        tcc_error("invalid operation with label");
                转接(op) {
                事例 TOK_EQ:
                        pe->v = pe->v == e2.v;
                        跳出;
                事例 TOK_NE:
                        pe->v = pe->v != e2.v;
                        跳出;
                事例 TOK_LT:
                        pe->v = (int64_t)pe->v < (int64_t)e2.v;
                        跳出;
                事例 TOK_GE:
                        pe->v = (int64_t)pe->v >= (int64_t)e2.v;
                        跳出;
                事例 TOK_LE:
                        pe->v = (int64_t)pe->v <= (int64_t)e2.v;
                        跳出;
                事例 TOK_GT:
                        pe->v = (int64_t)pe->v > (int64_t)e2.v;
                        跳出;
                缺省:
                        跳出;
                }
                /* GAS compare results are -1/0 not 1/0.  */
                pe->v = -(int64_t)pe->v;
        }
}

ST_FUNC 空 asm_expr(TCCState *s1, ExprValue *pe)
{
        asm_expr_cmp(s1, pe);
}

ST_FUNC 整 asm_int_expr(TCCState *s1)
{
        ExprValue e;
        asm_expr(s1, &e);
        如 (e.sym)
                expect("constant");
        返回 e.v;
}

/* NOTE: the same name space as C labels is used to avoid using too
   much memory when storing labels in TokenStrings */
静态 Sym* asm_new_label1(TCCState *s1, 整 label, 整 is_local,
                                                   整 sh_num, 整 value)
{
        Sym *sym;

        sym = label_find(label);
        如 (sym) {
                /* A VT_EXTERN symbol, even if it has a section is considered
                   overridable.  This is how we "define" .set targets.  Real
                   definitions won't have VT_EXTERN set.  */
                如 (sym->r && !(sym->type.t & VT_EXTERN)) {
                        /* the label is already defined */
                        如 (!is_local) {
                                tcc_error("assembler label '%s' already defined",
                                          get_tok_str(label, NULL));
                        } 另 {
                                /* redefinition of local labels is possible */
                                跳转 new_label;
                        }
                }
        } 另 {
        new_label:
                sym = label_push(&s1->asm_labels, label, 0);
                /* If we need a symbol to hold a value, mark it as
                   tentative only (for .set).  If this is for a real label
                   we'll remove VT_EXTERN.  */
                sym->type.t = VT_STATIC | VT_VOID | VT_EXTERN;
        }
        sym->r = sh_num;
        sym->jnext = value;
        返回 sym;
}

静态 Sym* asm_new_label(TCCState *s1, 整 label, 整 is_local)
{
        返回 asm_new_label1(s1, label, is_local, cur_text_section->sh_num, ind);
}

/* Set the value of LABEL to that of some expression (possibly
   involving other symbols).  LABEL can be overwritten later still.  */
静态 Sym* set_symbol(TCCState *s1, 整 label)
{
        长 n;
        ExprValue e;
        next();
        asm_expr(s1, &e);
        n = e.v;
        如 (e.sym)
                n += e.sym->jnext;
        返回 asm_new_label1(s1, label, 0, e.sym ? e.sym->r : SHN_ABS, n);
}

静态 空 asm_free_labels(TCCState *st)
{
        Sym *s, *s1;
        Section *sec;

        对于(s = st->asm_labels; s != NULL; s = s1) {
                s1 = s->prev;
                /* define symbol value in object file */
                s->type.t &= ~VT_EXTERN;
                如 (s->r && !s->a.dllimport) {
                        如 (s->r == SHN_ABS)
                                sec = SECTION_ABS;
                        另
                                sec = st->sections[s->r];
                        put_extern_sym2(s, sec, s->jnext, 0, 0);
                }
                /* remove label */
                table_ident[s->v - TOK_IDENT]->sym_label = NULL;
                sym_free(s);
        }
        st->asm_labels = NULL;
}

静态 空 use_section1(TCCState *s1, Section *sec)
{
        cur_text_section->data_offset = ind;
        cur_text_section = sec;
        ind = cur_text_section->data_offset;
}

静态 空 use_section(TCCState *s1, 不变 字 *name)
{
        Section *sec;
        sec = find_section(s1, name);
        use_section1(s1, sec);
}

静态 空 push_section(TCCState *s1, 不变 字 *name)
{
        Section *sec = find_section(s1, name);
        sec->prev = cur_text_section;
        use_section1(s1, sec);
}

静态 空 pop_section(TCCState *s1)
{
        Section *prev = cur_text_section->prev;
        如 (!prev)
                tcc_error(".popsection without .pushsection");
        cur_text_section->prev = NULL;
        use_section1(s1, prev);
}

静态 空 asm_parse_directive(TCCState *s1, 整 global)
{
        整 n, offset, v, size, tok1;
        Section *sec;
        uint8_t *ptr;

        /* assembler directive */
        sec = cur_text_section;
        转接(tok) {
        事例 TOK_ASMDIR_align:
        事例 TOK_ASMDIR_balign:
        事例 TOK_ASMDIR_p2align:
        事例 TOK_ASMDIR_skip:
        事例 TOK_ASMDIR_space:
                tok1 = tok;
                next();
                n = asm_int_expr(s1);
                如 (tok1 == TOK_ASMDIR_p2align)
                {
                        如 (n < 0 || n > 30)
                                tcc_error("invalid p2align, must be between 0 and 30");
                        n = 1 << n;
                        tok1 = TOK_ASMDIR_align;
                }
                如 (tok1 == TOK_ASMDIR_align || tok1 == TOK_ASMDIR_balign) {
                        如 (n < 0 || (n & (n-1)) != 0)
                                tcc_error("alignment must be a positive power of two");
                        offset = (ind + n - 1) & -n;
                        size = offset - ind;
                        /* the section must have a compatible alignment */
                        如 (sec->sh_addralign < n)
                                sec->sh_addralign = n;
                } 另 {
                        如 (n < 0)
                                n = 0;
                        size = n;
                }
                v = 0;
                如 (tok == ',') {
                        next();
                        v = asm_int_expr(s1);
                }
        zero_pad:
                如 (sec->sh_type != SHT_NOBITS) {
                        sec->data_offset = ind;
                        ptr = section_ptr_add(sec, size);
                        memset(ptr, v, size);
                }
                ind += size;
                跳出;
        事例 TOK_ASMDIR_quad:
#如定义 TCC_TARGET_X86_64
                size = 8;
                跳转 asm_data;
#另
                next();
                对于(;;) {
                        uint64_t vl;
                        不变 字 *p;

                        p = tokc.str.data;
                        如 (tok != TOK_PPNUM) {
                        error_constant:
                                tcc_error("64 bit constant");
                        }
                        vl = strtoll(p, (字 **)&p, 0);
                        如 (*p != '\0')
                                跳转 error_constant;
                        next();
                        如 (sec->sh_type != SHT_NOBITS) {
                                /* XXX: endianness */
                                gen_le32(vl);
                                gen_le32(vl >> 32);
                        } 另 {
                                ind += 8;
                        }
                        如 (tok != ',')
                                跳出;
                        next();
                }
                跳出;
#了如
        事例 TOK_ASMDIR_byte:
                size = 1;
                跳转 asm_data;
        事例 TOK_ASMDIR_word:
        事例 TOK_ASMDIR_short:
                size = 2;
                跳转 asm_data;
        事例 TOK_ASMDIR_long:
        事例 TOK_ASMDIR_int:
                size = 4;
        asm_data:
                next();
                对于(;;) {
                        ExprValue e;
                        asm_expr(s1, &e);
                        如 (sec->sh_type != SHT_NOBITS) {
                                如 (size == 4) {
                                        gen_expr32(&e);
#如定义 TCC_TARGET_X86_64
                                } 另 如 (size == 8) {
                                        gen_expr64(&e);
#了如
                                } 另 {
                                        如 (e.sym)
                                                expect("constant");
                                        如 (size == 1)
                                                g(e.v);
                                        另
                                                gen_le16(e.v);
                                }
                        } 另 {
                                ind += size;
                        }
                        如 (tok != ',')
                                跳出;
                        next();
                }
                跳出;
        事例 TOK_ASMDIR_fill:
                {
                        整 repeat, size, val, i, j;
                        uint8_t repeat_buf[8];
                        next();
                        repeat = asm_int_expr(s1);
                        如 (repeat < 0) {
                                tcc_error("repeat < 0; .fill ignored");
                                跳出;
                        }
                        size = 1;
                        val = 0;
                        如 (tok == ',') {
                                next();
                                size = asm_int_expr(s1);
                                如 (size < 0) {
                                        tcc_error("size < 0; .fill ignored");
                                        跳出;
                                }
                                如 (size > 8)
                                        size = 8;
                                如 (tok == ',') {
                                        next();
                                        val = asm_int_expr(s1);
                                }
                        }
                        /* XXX: endianness */
                        repeat_buf[0] = val;
                        repeat_buf[1] = val >> 8;
                        repeat_buf[2] = val >> 16;
                        repeat_buf[3] = val >> 24;
                        repeat_buf[4] = 0;
                        repeat_buf[5] = 0;
                        repeat_buf[6] = 0;
                        repeat_buf[7] = 0;
                        对于(i = 0; i < repeat; i++) {
                                对于(j = 0; j < size; j++) {
                                        g(repeat_buf[j]);
                                }
                        }
                }
                跳出;
        事例 TOK_ASMDIR_rept:
                {
                        整 repeat;
                        TokenString *init_str;
                        next();
                        repeat = asm_int_expr(s1);
                        init_str = tok_str_alloc();
                        当 (next(), tok != TOK_ASMDIR_endr) {
                                如 (tok == CH_EOF)
                                        tcc_error("we at end of file, .endr not found");
                                tok_str_add_tok(init_str);
                        }
                        tok_str_add(init_str, -1);
                        tok_str_add(init_str, 0);
                        begin_macro(init_str, 1);
                        当 (repeat-- > 0) {
                                tcc_assemble_internal(s1, (parse_flags & PARSE_FLAG_PREPROCESS),
                                                                          global);
                                macro_ptr = init_str->str;
                        }
                        end_macro();
                        next();
                        跳出;
                }
        事例 TOK_ASMDIR_org:
                {
                        无符 长 n;
                        ExprValue e;
                        next();
                        asm_expr(s1, &e);
                        n = e.v;
                        如 (e.sym) {
                                如 (e.sym->r != cur_text_section->sh_num)
                                  expect("constant or same-section symbol");
                                n += e.sym->jnext;
                        }
                        如 (n < ind)
                                tcc_error("attempt to .org backwards");
                        v = 0;
                        size = n - ind;
                        跳转 zero_pad;
                }
                跳出;
        事例 TOK_ASMDIR_set:
                next();
                tok1 = tok;
                next();
                /* Also accept '.set stuff', but don't do anything with this.
                   It's used in GAS to set various features like '.set mips16'.  */
                如 (tok == ',')
                        set_symbol(s1, tok1);
                跳出;
        事例 TOK_ASMDIR_globl:
        事例 TOK_ASMDIR_global:
        事例 TOK_ASMDIR_weak:
        事例 TOK_ASMDIR_hidden:
                tok1 = tok;
                运行 {
                        Sym *sym;

                        next();
                        sym = get_asm_sym(tok, NULL);
                如 (tok1 != TOK_ASMDIR_hidden)
                                sym->type.t &= ~VT_STATIC;
                        如 (tok1 == TOK_ASMDIR_weak)
                                sym->a.weak = 1;
                另 如 (tok1 == TOK_ASMDIR_hidden)
                        sym->a.visibility = STV_HIDDEN;
                        next();
                } 当 (tok == ',');
                跳出;
        事例 TOK_ASMDIR_string:
        事例 TOK_ASMDIR_ascii:
        事例 TOK_ASMDIR_asciz:
                {
                        不变 uint8_t *p;
                        整 i, size, t;

                        t = tok;
                        next();
                        对于(;;) {
                                如 (tok != TOK_STR)
                                        expect("string constant");
                                p = tokc.str.data;
                                size = tokc.str.size;
                                如 (t == TOK_ASMDIR_ascii && size > 0)
                                        size--;
                                对于(i = 0; i < size; i++)
                                        g(p[i]);
                                next();
                                如 (tok == ',') {
                                        next();
                                } 另 如 (tok != TOK_STR) {
                                        跳出;
                                }
                        }
                }
                跳出;
        事例 TOK_ASMDIR_text:
        事例 TOK_ASMDIR_data:
        事例 TOK_ASMDIR_bss:
                {
                        字 sname[64];
                        tok1 = tok;
                        n = 0;
                        next();
                        如 (tok != ';' && tok != TOK_LINEFEED) {
                                n = asm_int_expr(s1);
                                next();
                        }
                        如 (n)
                                sprintf(sname, "%s%d", get_tok_str(tok1, NULL), n);
                        另
                                sprintf(sname, "%s", get_tok_str(tok1, NULL));
                        use_section(s1, sname);
        }
        跳出;
        事例 TOK_ASMDIR_file:
                {
                        字 filename[512];

                        filename[0] = '\0';
                        next();

                        如 (tok == TOK_STR)
                                pstrcat(filename, 求长度(filename), tokc.str.data);
                        另
                                pstrcat(filename, 求长度(filename), get_tok_str(tok, NULL));

                        如 (s1->warn_unsupported)
                                tcc_warning("ignoring .file %s", filename);

                        next();
                }
                跳出;
        事例 TOK_ASMDIR_ident:
                {
                        字 ident[256];

                        ident[0] = '\0';
                        next();

                        如 (tok == TOK_STR)
                                pstrcat(ident, 求长度(ident), tokc.str.data);
                        另
                                pstrcat(ident, 求长度(ident), get_tok_str(tok, NULL));

                        如 (s1->warn_unsupported)
                                tcc_warning("ignoring .ident %s", ident);

                        next();
                }
                跳出;
        事例 TOK_ASMDIR_size:
                {
                        Sym *sym;

                        next();
                        sym = label_find(tok);
                        如 (!sym) {
                                tcc_error("label not found: %s", get_tok_str(tok, NULL));
                        }

                        /* XXX .size name,label2-label1 */
                        如 (s1->warn_unsupported)
                                tcc_warning("ignoring .size %s,*", get_tok_str(tok, NULL));

                        next();
                        skip(',');
                        当 (tok != TOK_LINEFEED && tok != ';' && tok != CH_EOF) {
                                next();
                        }
                }
                跳出;
        事例 TOK_ASMDIR_type:
                {
                        Sym *sym;
                        不变 字 *newtype;

                        next();
                        sym = get_asm_sym(tok, NULL);
                        next();
                        skip(',');
                        如 (tok == TOK_STR) {
                                newtype = tokc.str.data;
                        } 另 {
                                如 (tok == '@' || tok == '%')
                                        next();
                                newtype = get_tok_str(tok, NULL);
                        }

                        如 (!strcmp(newtype, "function") || !strcmp(newtype, "STT_FUNC")) {
                                sym->type.t = (sym->type.t & ~VT_BTYPE) | VT_FUNC;
                        }
                        另 如 (s1->warn_unsupported)
                                tcc_warning("change type of '%s' from 0x%x to '%s' ignored",
                                        get_tok_str(sym->v, NULL), sym->type.t, newtype);

                        next();
                }
                跳出;
        事例 TOK_ASMDIR_pushsection:
        事例 TOK_ASMDIR_section:
                {
                        字 sname[256];
                        整 old_nb_section = s1->nb_sections;

                        tok1 = tok;
                        /* XXX: support more options */
                        next();
                        sname[0] = '\0';
                        当 (tok != ';' && tok != TOK_LINEFEED && tok != ',') {
                                如 (tok == TOK_STR)
                                        pstrcat(sname, 求长度(sname), tokc.str.data);
                                另
                                        pstrcat(sname, 求长度(sname), get_tok_str(tok, NULL));
                                next();
                        }
                        如 (tok == ',') {
                                /* skip section options */
                                next();
                                如 (tok != TOK_STR)
                                        expect("string constant");
                                next();
                                如 (tok == ',') {
                                        next();
                                        如 (tok == '@' || tok == '%')
                                                next();
                                        next();
                                }
                        }
                        last_text_section = cur_text_section;
                        如 (tok1 == TOK_ASMDIR_section)
                                use_section(s1, sname);
                        另
                                push_section(s1, sname);
                        /* If we just allocated a new section reset its alignment to
                           1.  new_section normally acts for GCC compatibility and
                           sets alignment to PTR_SIZE.  The assembler behaves different. */
                        如 (old_nb_section != s1->nb_sections)
                                cur_text_section->sh_addralign = 1;
                }
                跳出;
        事例 TOK_ASMDIR_previous:
                {
                        Section *sec;
                        next();
                        如 (!last_text_section)
                                tcc_error("no previous section referenced");
                        sec = cur_text_section;
                        use_section1(s1, last_text_section);
                        last_text_section = sec;
                }
                跳出;
        事例 TOK_ASMDIR_popsection:
                next();
                pop_section(s1);
                跳出;
#如定义 TCC_TARGET_I386
        事例 TOK_ASMDIR_code16:
                {
                        next();
                        s1->seg_size = 16;
                }
                跳出;
        事例 TOK_ASMDIR_code32:
                {
                        next();
                        s1->seg_size = 32;
                }
                跳出;
#了如
#如定义 TCC_TARGET_X86_64
        /* added for compatibility with GAS */
        事例 TOK_ASMDIR_code64:
                next();
                跳出;
#了如
        缺省:
                tcc_error("unknown assembler directive '.%s'", get_tok_str(tok, NULL));
                跳出;
        }
}


/* assemble a file */
静态 整 tcc_assemble_internal(TCCState *s1, 整 do_preprocess, 整 global)
{
        整 opcode;
        整 saved_parse_flags = parse_flags;

        /* XXX: undefine C labels */
        parse_flags = PARSE_FLAG_ASM_FILE | PARSE_FLAG_TOK_STR;
        如 (do_preprocess)
                parse_flags |= PARSE_FLAG_PREPROCESS;
        对于(;;) {
                next();
                如 (tok == TOK_EOF)
                        跳出;
                /* generate line number info */
                如 (global && s1->do_debug)
                        tcc_debug_line(s1);
                parse_flags |= PARSE_FLAG_LINEFEED; /* XXX: suppress that hack */
        redo:
                如 (tok == '#') {
                        /* horrible gas comment */
                        当 (tok != TOK_LINEFEED)
                                next();
                } 另 如 (tok >= TOK_ASMDIR_FIRST && tok <= TOK_ASMDIR_LAST) {
                        asm_parse_directive(s1, global);
                } 另 如 (tok == TOK_PPNUM) {
                        Sym *sym;
                        不变 字 *p;
                        整 n;
                        p = tokc.str.data;
                        n = strtoul(p, (字 **)&p, 10);
                        如 (*p != '\0')
                                expect("':'");
                        /* new local label */
                        sym = asm_new_label(s1, asm_get_local_label_name(s1, n), 1);
                        /* Remove the marker for tentative definitions.  */
                        sym->type.t &= ~VT_EXTERN;
                        next();
                        skip(':');
                        跳转 redo;
                } 另 如 (tok >= TOK_IDENT) {
                        /* instruction or label */
                        opcode = tok;
                        next();
                        如 (tok == ':') {
                                /* handle "extern void vide(void); __asm__("vide: ret");" as
                                "__asm__("globl vide\nvide: ret");" */
                                Sym *sym = sym_find(opcode);
                                如 (sym && (sym->type.t & VT_EXTERN) && global) {
                                        sym = label_find(opcode);
                                        如 (!sym) {
                                                sym = label_push(&s1->asm_labels, opcode, 0);
                                                sym->type.t = VT_VOID | VT_EXTERN;
                                        }
                                }
                                /* new label */
                                sym = asm_new_label(s1, opcode, 0);
                                sym->type.t &= ~VT_EXTERN;
                                next();
                                跳转 redo;
                        } 另 如 (tok == '=') {
                                set_symbol(s1, opcode);
                                跳转 redo;
                        } 另 {
                                asm_opcode(s1, opcode);
                        }
                }
                /* end of line */
                如 (tok != ';' && tok != TOK_LINEFEED)
                        expect("end of line");
                parse_flags &= ~PARSE_FLAG_LINEFEED; /* XXX: suppress that hack */
        }

        asm_free_labels(s1);
        parse_flags = saved_parse_flags;
        返回 0;
}

/* Assemble the current file */
ST_FUNC 整 tcc_assemble(TCCState *s1, 整 do_preprocess)
{
        整 ret;
        tcc_debug_start(s1);
        /* default section is text */
        cur_text_section = text_section;
        ind = cur_text_section->data_offset;
        nocode_wanted = 0;
        ret = tcc_assemble_internal(s1, do_preprocess, 1);
        cur_text_section->data_offset = ind;
        tcc_debug_end(s1);
        返回 ret;
}

/********************************************************************/
/* GCC inline asm support */

/* assemble the string 'str' in the current C compilation unit without
   C preprocessing. NOTE: str is modified by modifying the '\0' at the
   end */
静态 空 tcc_assemble_inline(TCCState *s1, 字 *str, 整 len, 整 global)
{
        不变 整 *saved_macro_ptr = macro_ptr;
        整 dotid = set_idnum('.', IS_ID);

        tcc_open_bf(s1, ":asm:", len);
        memcpy(file->buffer, str, len);
        macro_ptr = NULL;
        tcc_assemble_internal(s1, 0, global);
        tcc_close();

        set_idnum('.', dotid);
        macro_ptr = saved_macro_ptr;
}

/* find a constraint by its number or id (gcc 3 extended
   syntax). return -1 if not found. Return in *pp in char after the
   constraint */
ST_FUNC 整 find_constraint(ASMOperand *operands, 整 nb_operands,
                                                   不变 字 *name, 不变 字 **pp)
{
        整 index;
        TokenSym *ts;
        不变 字 *p;

        如 (isnum(*name)) {
                index = 0;
                当 (isnum(*name)) {
                        index = (index * 10) + (*name) - '0';
                        name++;
                }
                如 ((无符)index >= nb_operands)
                        index = -1;
        } 另 如 (*name == '[') {
                name++;
                p = strchr(name, ']');
                如 (p) {
                        ts = tok_alloc(name, p - name);
                        对于(index = 0; index < nb_operands; index++) {
                                如 (operands[index].id == ts->tok)
                                        跳转 found;
                        }
                        index = -1;
                found:
                        name = p + 1;
                } 另 {
                        index = -1;
                }
        } 另 {
                index = -1;
        }
        如 (pp)
                *pp = name;
        返回 index;
}

静态 空 subst_asm_operands(ASMOperand *operands, 整 nb_operands,
                                                           CString *out_str, CString *in_str)
{
        整 c, index, modifier;
        不变 字 *str;
        ASMOperand *op;
        SValue sv;

        cstr_new(out_str);
        str = in_str->data;
        对于(;;) {
                c = *str++;
                如 (c == '%') {
                        如 (*str == '%') {
                                str++;
                                跳转 add_char;
                        }
                        modifier = 0;
                        如 (*str == 'c' || *str == 'n' ||
                                *str == 'b' || *str == 'w' || *str == 'h' || *str == 'k' ||
                                *str == 'q' ||
                                /* P in GCC would add "@PLT" to symbol refs in PIC mode,
                                   and make literal operands not be decorated with '$'.  */
                                *str == 'P')
                                modifier = *str++;
                        index = find_constraint(operands, nb_operands, str, &str);
                        如 (index < 0)
                                tcc_error("invalid operand reference after %%");
                        op = &operands[index];
                        sv = *op->vt;
                        如 (op->reg >= 0) {
                                sv.r = op->reg;
                                如 ((op->vt->r & VT_VALMASK) == VT_LLOCAL && op->is_memory)
                                        sv.r |= VT_LVAL;
                        }
                        subst_asm_operand(out_str, &sv, modifier);
                } 另 {
                add_char:
                        cstr_ccat(out_str, c);
                        如 (c == '\0')
                                跳出;
                }
        }
}


静态 空 parse_asm_operands(ASMOperand *operands, 整 *nb_operands_ptr,
                                                           整 is_output)
{
        ASMOperand *op;
        整 nb_operands;

        如 (tok != ':') {
                nb_operands = *nb_operands_ptr;
                对于(;;) {
                        CString astr;
                        如 (nb_operands >= MAX_ASM_OPERANDS)
                                tcc_error("too many asm operands");
                        op = &operands[nb_operands++];
                        op->id = 0;
                        如 (tok == '[') {
                                next();
                                如 (tok < TOK_IDENT)
                                        expect("identifier");
                                op->id = tok;
                                next();
                                skip(']');
                        }
                        parse_mult_str(&astr, "string constant");
                        op->constraint = tcc_malloc(astr.size);
                        strcpy(op->constraint, astr.data);
                        cstr_free(&astr);
                        skip('(');
                        gexpr();
                        如 (is_output) {
                                如 (!(vtop->type.t & VT_ARRAY))
                                        test_lvalue();
                        } 另 {
                                /* we want to avoid LLOCAL case, except when the 'm'
                                   constraint is used. Note that it may come from
                                   register storage, so we need to convert (reg)
                                   case */
                                如 ((vtop->r & VT_LVAL) &&
                                        ((vtop->r & VT_VALMASK) == VT_LLOCAL ||
                                         (vtop->r & VT_VALMASK) < VT_CONST) &&
                                        !strchr(op->constraint, 'm')) {
                                        gv(RC_INT);
                                }
                        }
                        op->vt = vtop;
                        skip(')');
                        如 (tok == ',') {
                                next();
                        } 另 {
                                跳出;
                        }
                }
                *nb_operands_ptr = nb_operands;
        }
}

/* parse the GCC asm() instruction */
ST_FUNC 空 asm_instr(空)
{
        CString astr, astr1;
        ASMOperand operands[MAX_ASM_OPERANDS];
        整 nb_outputs, nb_operands, i, must_subst, out_reg;
        uint8_t clobber_regs[NB_ASM_REGS];

        next();
        /* since we always generate the asm() instruction, we can ignore
           volatile */
        如 (tok == TOK_VOLATILE1 || tok == TOK_VOLATILE2 || tok == TOK_VOLATILE3 || tok == TOK_VOLATILE1_CN) {
                next();
        }
        parse_asm_str(&astr);
        nb_operands = 0;
        nb_outputs = 0;
        must_subst = 0;
        memset(clobber_regs, 0, 求长度(clobber_regs));
        如 (tok == ':') {
                next();
                must_subst = 1;
                /* output args */
                parse_asm_operands(operands, &nb_operands, 1);
                nb_outputs = nb_operands;
                如 (tok == ':') {
                        next();
                        如 (tok != ')') {
                                /* input args */
                                parse_asm_operands(operands, &nb_operands, 0);
                                如 (tok == ':') {
                                        /* clobber list */
                                        /* XXX: handle registers */
                                        next();
                                        对于(;;) {
                                                如 (tok != TOK_STR)
                                                        expect("string constant");
                                                asm_clobber(clobber_regs, tokc.str.data);
                                                next();
                                                如 (tok == ',') {
                                                        next();
                                                } 另 {
                                                        跳出;
                                                }
                                        }
                                }
                        }
                }
        }
        skip(')');
        /* NOTE: we do not eat the ';' so that we can restore the current
           token after the assembler parsing */
        如 (tok != ';')
                expect("';'");

        /* save all values in the memory */
        save_regs(0);

        /* compute constraints */
        asm_compute_constraints(operands, nb_operands, nb_outputs,
                                                        clobber_regs, &out_reg);

        /* substitute the operands in the asm string. No substitution is
           done if no operands (GCC behaviour) */
#如定义 ASM_DEBUG
        printf("asm: \"%s\"\n", (字 *)astr.data);
#了如
        如 (must_subst) {
                subst_asm_operands(operands, nb_operands, &astr1, &astr);
                cstr_free(&astr);
        } 另 {
                astr1 = astr;
        }
#如定义 ASM_DEBUG
        printf("subst_asm: \"%s\"\n", (字 *)astr1.data);
#了如

        /* generate loads */
        asm_gen_code(operands, nb_operands, nb_outputs, 0,
                                 clobber_regs, out_reg);

        /* assemble the string with tcc internal assembler */
        tcc_assemble_inline(tcc_state, astr1.data, astr1.size - 1, 0);

        /* restore the current C token */
        next();

        /* store the output values if needed */
        asm_gen_code(operands, nb_operands, nb_outputs, 1,
                                 clobber_regs, out_reg);

        /* free everything */
        对于(i=0;i<nb_operands;i++) {
                ASMOperand *op;
                op = &operands[i];
                tcc_free(op->constraint);
                vpop();
        }
        cstr_free(&astr1);
}

ST_FUNC 空 asm_global_instr(空)
{
        CString astr;
        整 saved_nocode_wanted = nocode_wanted;

        /* Global asm blocks are always emitted.  */
        nocode_wanted = 0;
        next();
        parse_asm_str(&astr);
        skip(')');
        /* NOTE: we do not eat the ';' so that we can restore the current
           token after the assembler parsing */
        如 (tok != ';')
                expect("';'");

#如定义 ASM_DEBUG
        printf("asm_global: \"%s\"\n", (字 *)astr.data);
#了如
        cur_text_section = text_section;
        ind = cur_text_section->data_offset;

        /* assemble the string with tcc internal assembler */
        tcc_assemble_inline(tcc_state, astr.data, astr.size - 1, 1);

        cur_text_section->data_offset = ind;

        /* restore the current C token */
        next();

        cstr_free(&astr);
        nocode_wanted = saved_nocode_wanted;
}
#了如 /* CONFIG_TCC_ASM */
