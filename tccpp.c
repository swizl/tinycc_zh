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

ST_DATA 整 tok_flags;
ST_DATA 整 parse_flags;

ST_DATA 结构 BufferedFile *file;
ST_DATA 整 ch, tok;
ST_DATA CValue tokc;
ST_DATA 不变 整 *macro_ptr;
ST_DATA CString tokcstr; /* current parsed string, if any */

/* display benchmark infos */
ST_DATA 整 total_lines;
ST_DATA 整 total_bytes;
ST_DATA 整 tok_ident;
ST_DATA TokenSym **table_ident;

/* ------------------------------------------------------------------------- */

静态 TokenSym *hash_ident[TOK_HASH_SIZE];
静态 字 token_buf[STRING_MAX_SIZE + 1];
静态 CString cstr_buf;
静态 CString macro_equal_buf;
静态 TokenString tokstr_buf;
静态 无符 字 isidnum_table[256 - CH_EOF];
静态 整 pp_debug_tok, pp_debug_symv;
静态 整 pp_once;
静态 整 pp_expr;
静态 整 pp_counter;
静态 空 tok_print(不变 字 *msg, 不变 整 *str);

静态 结构 TinyAlloc *toksym_alloc;
静态 结构 TinyAlloc *tokstr_alloc;
静态 结构 TinyAlloc *cstr_alloc;

静态 TokenString *macro_stack;

静态 不变 字 tcc_keywords[] =
#定义 DEF(id, str) str "\0"
#包含 "tcctok.h"
#消定义 DEF
;

/* WARNING: the content of this string encodes token numbers */
静态 不变 无符 字 tok_two_chars[] =
/* outdated -- gr
        "<=\236>=\235!=\225&&\240||\241++\244--\242==\224<<\1>>\2+=\253"
        "-=\255*=\252/=\257%=\245&=\246^=\336|=\374->\313..\250##\266";
*/{
        '<','=', TOK_LE,
        '>','=', TOK_GE,
        '!','=', TOK_NE,
        '&','&', TOK_LAND,
        '|','|', TOK_LOR,
        '+','+', TOK_INC,
        '-','-', TOK_DEC,
        '=','=', TOK_EQ,
        '<','<', TOK_SHL,
        '>','>', TOK_SAR,
        '+','=', TOK_A_ADD,
        '-','=', TOK_A_SUB,
        '*','=', TOK_A_MUL,
        '/','=', TOK_A_DIV,
        '%','=', TOK_A_MOD,
        '&','=', TOK_A_AND,
        '^','=', TOK_A_XOR,
        '|','=', TOK_A_OR,
        '-','>', TOK_ARROW,
        '.','.', 0xa8, // C++ token ?
        '#','#', TOK_TWOSHARPS,
        0
};

静态 空 next_nomacro_spc(空);

ST_FUNC 空 skip(整 c)
{
        如 (tok != c)
                tcc_error("'%c' expected (got \"%s\")", c, get_tok_str(tok, &tokc));
        next();
}

ST_FUNC 空 expect(不变 字 *msg)
{
        tcc_error("%s expected", msg);
}

/* ------------------------------------------------------------------------- */
/* Custom allocator for tiny objects */

#定义 USE_TAL

#如未定义 USE_TAL
#定义 tal_free(al, p) tcc_free(p)
#定义 tal_realloc(al, p, size) tcc_realloc(p, size)
#定义 tal_new(a,b,c)
#定义 tal_delete(a)
#另
#如 !已定义(MEM_DEBUG)
#定义 tal_free(al, p) tal_free_impl(al, p)
#定义 tal_realloc(al, p, size) tal_realloc_impl(&al, p, size)
#定义 TAL_DEBUG_PARAMS
#另
#定义 TAL_DEBUG 1
//#定义 TAL_INFO 1 /* collect and dump allocators stats */
#定义 tal_free(al, p) tal_free_impl(al, p, __文件__, __行号__)
#定义 tal_realloc(al, p, size) tal_realloc_impl(&al, p, size, __文件__, __行号__)
#定义 TAL_DEBUG_PARAMS , 不变 字 *file, 整 line
#定义 TAL_DEBUG_FILE_LEN 40
#了如

#定义 TOKSYM_TAL_SIZE     (768 * 1024) /* allocator for tiny TokenSym in table_ident */
#定义 TOKSTR_TAL_SIZE     (768 * 1024) /* allocator for tiny TokenString instances */
#定义 CSTR_TAL_SIZE       (256 * 1024) /* allocator for tiny CString instances */
#定义 TOKSYM_TAL_LIMIT    256 /* prefer unique limits to distinguish allocators debug msgs */
#定义 TOKSTR_TAL_LIMIT    128 /* 32 * sizeof(int) */
#定义 CSTR_TAL_LIMIT      1024

类型定义 结构 TinyAlloc {
        无符  limit;
        无符  size;
        uint8_t *buffer;
        uint8_t *p;
        无符  nb_allocs;
        结构 TinyAlloc *next, *top;
#如定义 TAL_INFO
        无符  nb_peak;
        无符  nb_total;
        无符  nb_missed;
        uint8_t *peak_p;
#了如
} TinyAlloc;

类型定义 结构 tal_header_t {
        无符  size;
#如定义 TAL_DEBUG
        整     line_num; /* negative line_num used for double free check */
        字    file_name[TAL_DEBUG_FILE_LEN + 1];
#了如
} tal_header_t;

/* ------------------------------------------------------------------------- */

静态 TinyAlloc *tal_new(TinyAlloc **pal, 无符 limit, 无符 size)
{
        TinyAlloc *al = tcc_mallocz(求长度(TinyAlloc));
        al->p = al->buffer = tcc_malloc(size);
        al->limit = limit;
        al->size = size;
        如 (pal) *pal = al;
        返回 al;
}

静态 空 tal_delete(TinyAlloc *al)
{
        TinyAlloc *next;

tail_call:
        如 (!al)
                返回;
#如定义 TAL_INFO
        fprintf(stderr, "limit=%5d, size=%5g MB, nb_peak=%6d, nb_total=%8d, nb_missed=%6d, usage=%5.1f%%\n",
                        al->limit, al->size / 1024.0 / 1024.0, al->nb_peak, al->nb_total, al->nb_missed,
                        (al->peak_p - al->buffer) * 100.0 / al->size);
#了如
#如定义 TAL_DEBUG
        如 (al->nb_allocs > 0) {
                uint8_t *p;
                fprintf(stderr, "TAL_DEBUG: memory leak %d chunk(s) (limit= %d)\n",
                                al->nb_allocs, al->limit);
                p = al->buffer;
                当 (p < al->p) {
                        tal_header_t *header = (tal_header_t *)p;
                        如 (header->line_num > 0) {
                                fprintf(stderr, "%s:%d: chunk of %d bytes leaked\n",
                                                header->file_name, header->line_num, header->size);
                        }
                        p += header->size + 求长度(tal_header_t);
                }
#如 MEM_DEBUG-0 == 2
                exit(2);
#了如
        }
#了如
        next = al->next;
        tcc_free(al->buffer);
        tcc_free(al);
        al = next;
        跳转 tail_call;
}

静态 空 tal_free_impl(TinyAlloc *al, 空 *p TAL_DEBUG_PARAMS)
{
        如 (!p)
                返回;
tail_call:
        如 (al->buffer <= (uint8_t *)p && (uint8_t *)p < al->buffer + al->size) {
#如定义 TAL_DEBUG
                tal_header_t *header = (((tal_header_t *)p) - 1);
                如 (header->line_num < 0) {
                        fprintf(stderr, "%s:%d: TAL_DEBUG: double frees chunk from\n",
                                        file, line);
                        fprintf(stderr, "%s:%d: %d bytes\n",
                                        header->file_name, (整)-header->line_num, (整)header->size);
                } 另
                        header->line_num = -header->line_num;
#了如
                al->nb_allocs--;
                如 (!al->nb_allocs)
                        al->p = al->buffer;
        } 另 如 (al->next) {
                al = al->next;
                跳转 tail_call;
        }
        另
                tcc_free(p);
}

静态 空 *tal_realloc_impl(TinyAlloc **pal, 空 *p, 无符 size TAL_DEBUG_PARAMS)
{
        tal_header_t *header;
        空 *ret;
        整 is_own;
        无符 adj_size = (size + 3) & -4;
        TinyAlloc *al = *pal;

tail_call:
        is_own = (al->buffer <= (uint8_t *)p && (uint8_t *)p < al->buffer + al->size);
        如 ((!p || is_own) && size <= al->limit) {
                如 (al->p + adj_size + 求长度(tal_header_t) < al->buffer + al->size) {
                        header = (tal_header_t *)al->p;
                        header->size = adj_size;
#如定义 TAL_DEBUG
                        { 整 ofs = strlen(file) - TAL_DEBUG_FILE_LEN;
                        strncpy(header->file_name, file + (ofs > 0 ? ofs : 0), TAL_DEBUG_FILE_LEN);
                        header->file_name[TAL_DEBUG_FILE_LEN] = 0;
                        header->line_num = line; }
#了如
                        ret = al->p + 求长度(tal_header_t);
                        al->p += adj_size + 求长度(tal_header_t);
                        如 (is_own) {
                                header = (((tal_header_t *)p) - 1);
                                memcpy(ret, p, header->size);
#如定义 TAL_DEBUG
                                header->line_num = -header->line_num;
#了如
                        } 另 {
                                al->nb_allocs++;
                        }
#如定义 TAL_INFO
                        如 (al->nb_peak < al->nb_allocs)
                                al->nb_peak = al->nb_allocs;
                        如 (al->peak_p < al->p)
                                al->peak_p = al->p;
                        al->nb_total++;
#了如
                        返回 ret;
                } 另 如 (is_own) {
                        al->nb_allocs--;
                        ret = tal_realloc(*pal, 0, size);
                        header = (((tal_header_t *)p) - 1);
                        memcpy(ret, p, header->size);
#如定义 TAL_DEBUG
                        header->line_num = -header->line_num;
#了如
                        返回 ret;
                }
                如 (al->next) {
                        al = al->next;
                } 另 {
                        TinyAlloc *bottom = al, *next = al->top ? al->top : al;

                        al = tal_new(pal, next->limit, next->size * 2);
                        al->next = next;
                        bottom->top = al;
                }
                跳转 tail_call;
        }
        如 (is_own) {
                al->nb_allocs--;
                ret = tcc_malloc(size);
                header = (((tal_header_t *)p) - 1);
                memcpy(ret, p, header->size);
#如定义 TAL_DEBUG
                header->line_num = -header->line_num;
#了如
        } 另 如 (al->next) {
                al = al->next;
                跳转 tail_call;
        } 另
                ret = tcc_realloc(p, size);
#如定义 TAL_INFO
        al->nb_missed++;
#了如
        返回 ret;
}

#了如 /* USE_TAL */

/* ------------------------------------------------------------------------- */
/* CString handling */
静态 空 cstr_realloc(CString *cstr, 整 new_size)
{
        整 size;

        size = cstr->size_allocated;
        如 (size < 8)
                size = 8; /* no need to allocate a too small first string */
        当 (size < new_size)
                size = size * 2;
        cstr->data = tal_realloc(cstr_alloc, cstr->data, size);
        cstr->size_allocated = size;
}

/* add a byte */
ST_INLN 空 cstr_ccat(CString *cstr, 整 ch)
{
        整 size;
        size = cstr->size + 1;
        如 (size > cstr->size_allocated)
                cstr_realloc(cstr, size);
        ((无符 字 *)cstr->data)[size - 1] = ch;
        cstr->size = size;
}

ST_FUNC 空 cstr_cat(CString *cstr, 不变 字 *str, 整 len)
{
        整 size;
        如 (len <= 0)
                len = strlen(str) + 1 + len;
        size = cstr->size + len;
        如 (size > cstr->size_allocated)
                cstr_realloc(cstr, size);
        memmove(((无符 字 *)cstr->data) + cstr->size, str, len);
        cstr->size = size;
}

/* add a wide char */
ST_FUNC 空 cstr_wccat(CString *cstr, 整 ch)
{
        整 size;
        size = cstr->size + 求长度(nwchar_t);
        如 (size > cstr->size_allocated)
                cstr_realloc(cstr, size);
        *(nwchar_t *)(((无符 字 *)cstr->data) + size - 求长度(nwchar_t)) = ch;
        cstr->size = size;
}

ST_FUNC 空 cstr_new(CString *cstr)
{
        memset(cstr, 0, 求长度(CString));
}

/* free string and reset it to NULL */
ST_FUNC 空 cstr_free(CString *cstr)
{
        tal_free(cstr_alloc, cstr->data);
        cstr_new(cstr);
}

/* reset string to empty */
ST_FUNC 空 cstr_reset(CString *cstr)
{
        cstr->size = 0;
}

/* XXX: unicode ? */
静态 空 add_char(CString *cstr, 整 c)
{
        如 (c == '\'' || c == '\"' || c == '\\') {
                /* XXX: could be more precise if char or string */
                cstr_ccat(cstr, '\\');
        }
        如 (c >= 32 && c <= 126) {
                cstr_ccat(cstr, c);
        } 另 {
                cstr_ccat(cstr, '\\');
                如 (c == '\n') {
                        cstr_ccat(cstr, 'n');
                } 另 {
                        cstr_ccat(cstr, '0' + ((c >> 6) & 7));
                        cstr_ccat(cstr, '0' + ((c >> 3) & 7));
                        cstr_ccat(cstr, '0' + (c & 7));
                }
        }
}

/* ------------------------------------------------------------------------- */
/* allocate a new token */
静态 TokenSym *tok_alloc_new(TokenSym **pts, 不变 字 *str, 整 len)
{
        TokenSym *ts, **ptable;
        整 i;

        如 (tok_ident >= SYM_FIRST_ANOM)
                tcc_error("memory full (symbols)");

        /* expand token table if needed */
        i = tok_ident - TOK_IDENT;
        如 ((i % TOK_ALLOC_INCR) == 0) {
                ptable = tcc_realloc(table_ident, (i + TOK_ALLOC_INCR) * 求长度(TokenSym *));
                table_ident = ptable;
        }

        ts = tal_realloc(toksym_alloc, 0, 求长度(TokenSym) + len);
        table_ident[i] = ts;
        ts->tok = tok_ident++;
        ts->sym_define = NULL;
        ts->sym_label = NULL;
        ts->sym_struct = NULL;
        ts->sym_identifier = NULL;
        ts->len = len;
        ts->hash_next = NULL;
        memcpy(ts->str, str, len);
        ts->str[len] = '\0';
        *pts = ts;
        返回 ts;
}

#定义 TOK_HASH_INIT 1
#定义 TOK_HASH_FUNC(h, c) ((h) + ((h) << 5) + ((h) >> 27) + (c))


/* find a token and add it if not found */
ST_FUNC TokenSym *tok_alloc(不变 字 *str, 整 len)
{
        TokenSym *ts, **pts;
        整 i;
        无符 整 h;

        h = TOK_HASH_INIT;
        对于(i=0;i<len;i++)
                h = TOK_HASH_FUNC(h, ((无符 字 *)str)[i]);
        h &= (TOK_HASH_SIZE - 1);

        pts = &hash_ident[h];
        对于(;;) {
                ts = *pts;
                如 (!ts)
                        跳出;
                如 (ts->len == len && !memcmp(ts->str, str, len))
                        返回 ts;
                pts = &(ts->hash_next);
        }
        返回 tok_alloc_new(pts, str, len);
}

/* XXX: buffer overflow */
/* XXX: float tokens */
ST_FUNC 不变 字 *get_tok_str(整 v, CValue *cv)
{
        字 *p;
        整 i, len;

        cstr_reset(&cstr_buf);
        p = cstr_buf.data;

        转接(v) {
        事例 TOK_CINT:
        事例 TOK_CUINT:
        事例 TOK_CLONG:
        事例 TOK_CULONG:
        事例 TOK_CLLONG:
        事例 TOK_CULLONG:
                /* XXX: not quite exact, but only useful for testing  */
#如定义 _WIN32
                sprintf(p, "%u", (无符)cv->i);
#另
                sprintf(p, "%llu", (无符 长 长)cv->i);
#了如
                跳出;
        事例 TOK_LCHAR:
                cstr_ccat(&cstr_buf, 'L');
        事例 TOK_CCHAR:
                cstr_ccat(&cstr_buf, '\'');
                add_char(&cstr_buf, cv->i);
                cstr_ccat(&cstr_buf, '\'');
                cstr_ccat(&cstr_buf, '\0');
                跳出;
        事例 TOK_PPNUM:
        事例 TOK_PPSTR:
                返回 (字*)cv->str.data;
        事例 TOK_LSTR:
                cstr_ccat(&cstr_buf, 'L');
        事例 TOK_STR:
                cstr_ccat(&cstr_buf, '\"');
                如 (v == TOK_STR) {
                        len = cv->str.size - 1;
                        对于(i=0;i<len;i++)
                                add_char(&cstr_buf, ((无符 字 *)cv->str.data)[i]);
                } 另 {
                        len = (cv->str.size / 求长度(nwchar_t)) - 1;
                        对于(i=0;i<len;i++)
                                add_char(&cstr_buf, ((nwchar_t *)cv->str.data)[i]);
                }
                cstr_ccat(&cstr_buf, '\"');
                cstr_ccat(&cstr_buf, '\0');
                跳出;

        事例 TOK_CFLOAT:
                cstr_cat(&cstr_buf, "<float>", 0);
                跳出;
        事例 TOK_CDOUBLE:
                cstr_cat(&cstr_buf, "<double>", 0);
                跳出;
        事例 TOK_CLDOUBLE:
                cstr_cat(&cstr_buf, "<long double>", 0);
                跳出;
        事例 TOK_LINENUM:
                cstr_cat(&cstr_buf, "<linenumber>", 0);
                跳出;

        /* above tokens have value, the ones below don't */
        事例 TOK_LT:
                v = '<';
                跳转 addv;
        事例 TOK_GT:
                v = '>';
                跳转 addv;
        事例 TOK_DOTS:
                返回 strcpy(p, "...");
        事例 TOK_A_SHL:
                返回 strcpy(p, "<<=");
        事例 TOK_A_SAR:
                返回 strcpy(p, ">>=");
        事例 TOK_EOF:
                返回 strcpy(p, "<eof>");
        缺省:
                如 (v < TOK_IDENT) {
                        /* search in two bytes table */
                        不变 无符 字 *q = tok_two_chars;
                        当 (*q) {
                                如 (q[2] == v) {
                                        *p++ = q[0];
                                        *p++ = q[1];
                                        *p = '\0';
                                        返回 cstr_buf.data;
                                }
                                q += 3;
                        }
                如 (v >= 127) {
                        sprintf(cstr_buf.data, "<%02x>", v);
                        返回 cstr_buf.data;
                }
                addv:
                        *p++ = v;
                        *p = '\0';
                } 另 如 (v < tok_ident) {
                        返回 table_ident[v - TOK_IDENT]->str;
                } 另 如 (v >= SYM_FIRST_ANOM) {
                        /* special name for anonymous symbol */
                        sprintf(p, "L.%u", v - SYM_FIRST_ANOM);
                } 另 {
                        /* should never happen */
                        返回 NULL;
                }
                跳出;
        }
        返回 cstr_buf.data;
}

/* return the current character, handling end of block if necessary
   (but not stray) */
ST_FUNC 整 handle_eob(空)
{
        BufferedFile *bf = file;
        整 len;

        /* only tries to read if really end of buffer */
        如 (bf->buf_ptr >= bf->buf_end) {
                如 (bf->fd != -1) {
#如 已定义(PARSE_DEBUG)
                        len = 1;
#另
                        len = IO_BUF_SIZE;
#了如
                        len = read(bf->fd, bf->buffer, len);
                        如 (len < 0)
                                len = 0;
                } 另 {
                        len = 0;
                }
                total_bytes += len;
                bf->buf_ptr = bf->buffer;
                bf->buf_end = bf->buffer + len;
                *bf->buf_end = CH_EOB;
        }
        如 (bf->buf_ptr < bf->buf_end) {
                返回 bf->buf_ptr[0];
        } 另 {
                bf->buf_ptr = bf->buf_end;
                返回 CH_EOF;
        }
}

/* read next char from current input file and handle end of input buffer */
ST_INLN 空 inp(空)
{
        ch = *(++(file->buf_ptr));
        /* end of buffer/file handling */
        如 (ch == CH_EOB)
                ch = handle_eob();
}

/* handle '\[\r]\n' */
静态 整 handle_stray_noerror(空)
{
        当 (ch == '\\') {
                inp();
                如 (ch == '\n') {
                        file->line_num++;
                        inp();
                } 另 如 (ch == '\r') {
                        inp();
                        如 (ch != '\n')
                                跳转 fail;
                        file->line_num++;
                        inp();
                } 另 {
                fail:
                        返回 1;
                }
        }
        返回 0;
}

静态 空 handle_stray(空)
{
        如 (handle_stray_noerror())
                tcc_error("stray '\\' in program");
}

/* skip the stray and handle the \\n case. Output an error if
   incorrect char after the stray */
静态 整 handle_stray1(uint8_t *p)
{
        整 c;

        file->buf_ptr = p;
        如 (p >= file->buf_end) {
                c = handle_eob();
                如 (c != '\\')
                        返回 c;
                p = file->buf_ptr;
        }
        ch = *p;
        如 (handle_stray_noerror()) {
                如 (!(parse_flags & PARSE_FLAG_ACCEPT_STRAYS))
                        tcc_error("stray '\\' in program");
                *--file->buf_ptr = '\\';
        }
        p = file->buf_ptr;
        c = *p;
        返回 c;
}

/* handle just the EOB case, but not stray */
#定义 PEEKC_EOB(c, p)\
{\
        p++;\
        c = *p;\
        如 (c == '\\') {\
                file->buf_ptr = p;\
                c = handle_eob();\
                p = file->buf_ptr;\
        }\
}

/* handle the complicated stray case */
#定义 PEEKC(c, p)\
{\
        p++;\
        c = *p;\
        如 (c == '\\') {\
                c = handle_stray1(p);\
                p = file->buf_ptr;\
        }\
}

/* input with '\[\r]\n' handling. Note that this function cannot
   handle other characters after '\', so you cannot call it inside
   strings or comments */
ST_FUNC 空 minp(空)
{
        inp();
        如 (ch == '\\')
                handle_stray();
}

/* single line C++ comments */
静态 uint8_t *parse_line_comment(uint8_t *p)
{
        整 c;

        p++;
        对于(;;) {
                c = *p;
        redo:
                如 (c == '\n' || c == CH_EOF) {
                        跳出;
                } 另 如 (c == '\\') {
                        file->buf_ptr = p;
                        c = handle_eob();
                        p = file->buf_ptr;
                        如 (c == '\\') {
                                PEEKC_EOB(c, p);
                                如 (c == '\n') {
                                        file->line_num++;
                                        PEEKC_EOB(c, p);
                                } 另 如 (c == '\r') {
                                        PEEKC_EOB(c, p);
                                        如 (c == '\n') {
                                                file->line_num++;
                                                PEEKC_EOB(c, p);
                                        }
                                }
                        } 另 {
                                跳转 redo;
                        }
                } 另 {
                        p++;
                }
        }
        返回 p;
}

/* C comments */
ST_FUNC uint8_t *parse_comment(uint8_t *p)
{
        整 c;

        p++;
        对于(;;) {
                /* fast skip loop */
                对于(;;) {
                        c = *p;
                        如 (c == '\n' || c == '*' || c == '\\')
                                跳出;
                        p++;
                        c = *p;
                        如 (c == '\n' || c == '*' || c == '\\')
                                跳出;
                        p++;
                }
                /* now we can handle all the cases */
                如 (c == '\n') {
                        file->line_num++;
                        p++;
                } 另 如 (c == '*') {
                        p++;
                        对于(;;) {
                                c = *p;
                                如 (c == '*') {
                                        p++;
                                } 另 如 (c == '/') {
                                        跳转 end_of_comment;
                                } 另 如 (c == '\\') {
                                        file->buf_ptr = p;
                                        c = handle_eob();
                                        p = file->buf_ptr;
                                        如 (c == CH_EOF)
                                                tcc_error("unexpected end of file in comment");
                                        如 (c == '\\') {
                                                /* skip '\[\r]\n', otherwise just skip the stray */
                                                当 (c == '\\') {
                                                        PEEKC_EOB(c, p);
                                                        如 (c == '\n') {
                                                                file->line_num++;
                                                                PEEKC_EOB(c, p);
                                                        } 另 如 (c == '\r') {
                                                                PEEKC_EOB(c, p);
                                                                如 (c == '\n') {
                                                                        file->line_num++;
                                                                        PEEKC_EOB(c, p);
                                                                }
                                                        } 另 {
                                                                跳转 after_star;
                                                        }
                                                }
                                        }
                                } 另 {
                                        跳出;
                                }
                        }
                after_star: ;
                } 另 {
                        /* stray, eob or eof */
                        file->buf_ptr = p;
                        c = handle_eob();
                        p = file->buf_ptr;
                        如 (c == CH_EOF) {
                                tcc_error("unexpected end of file in comment");
                        } 另 如 (c == '\\') {
                                p++;
                        }
                }
        }
 end_of_comment:
        p++;
        返回 p;
}

ST_FUNC 整 set_idnum(整 c, 整 val)
{
        整 prev = isidnum_table[c - CH_EOF];
        isidnum_table[c - CH_EOF] = val;
        返回 prev;
}

#定义 cinp minp

静态 内联 空 skip_spaces(空)
{
        当 (isidnum_table[ch - CH_EOF] & IS_SPC)
                cinp();
}

静态 内联 整 check_space(整 t, 整 *spc)
{
        如 (t < 256 && (isidnum_table[t - CH_EOF] & IS_SPC)) {
                如 (*spc)
                        返回 1;
                *spc = 1;
        } 另
                *spc = 0;
        返回 0;
}

/* parse a string without interpreting escapes */
静态 uint8_t *parse_pp_string(uint8_t *p,
                                                                整 sep, CString *str)
{
        整 c;
        p++;
        对于(;;) {
                c = *p;
                如 (c == sep) {
                        跳出;
                } 另 如 (c == '\\') {
                        file->buf_ptr = p;
                        c = handle_eob();
                        p = file->buf_ptr;
                        如 (c == CH_EOF) {
                        unterminated_string:
                                /* XXX: indicate line number of start of string */
                                tcc_error("missing terminating %c character", sep);
                        } 另 如 (c == '\\') {
                                /* escape : just skip \[\r]\n */
                                PEEKC_EOB(c, p);
                                如 (c == '\n') {
                                        file->line_num++;
                                        p++;
                                } 另 如 (c == '\r') {
                                        PEEKC_EOB(c, p);
                                        如 (c != '\n')
                                                expect("'\n' after '\r'");
                                        file->line_num++;
                                        p++;
                                } 另 如 (c == CH_EOF) {
                                        跳转 unterminated_string;
                                } 另 {
                                        如 (str) {
                                                cstr_ccat(str, '\\');
                                                cstr_ccat(str, c);
                                        }
                                        p++;
                                }
                        }
                } 另 如 (c == '\n') {
                        file->line_num++;
                        跳转 add_char;
                } 另 如 (c == '\r') {
                        PEEKC_EOB(c, p);
                        如 (c != '\n') {
                                如 (str)
                                        cstr_ccat(str, '\r');
                        } 另 {
                                file->line_num++;
                                跳转 add_char;
                        }
                } 另 {
                add_char:
                        如 (str)
                                cstr_ccat(str, c);
                        p++;
                }
        }
        p++;
        返回 p;
}

/* skip block of text until #else, #elif or #endif. skip also pairs of
   #if/#endif */
静态 空 preprocess_skip(空)
{
        整 a, start_of_line, c, in_warn_or_error;
        uint8_t *p;

        p = file->buf_ptr;
        a = 0;
redo_start:
        start_of_line = 1;
        in_warn_or_error = 0;
        对于(;;) {
        redo_no_start:
                c = *p;
                转接(c) {
                事例 ' ':
                事例 '\t':
                事例 '\f':
                事例 '\v':
                事例 '\r':
                        p++;
                        跳转 redo_no_start;
                事例 '\n':
                        file->line_num++;
                        p++;
                        跳转 redo_start;
                事例 '\\':
                        file->buf_ptr = p;
                        c = handle_eob();
                        如 (c == CH_EOF) {
                                expect("#endif");
                        } 另 如 (c == '\\') {
                                ch = file->buf_ptr[0];
                                handle_stray_noerror();
                        }
                        p = file->buf_ptr;
                        跳转 redo_no_start;
                /* skip strings */
                事例 '\"':
                事例 '\'':
                        如 (in_warn_or_error)
                                跳转 _default;
                        p = parse_pp_string(p, c, NULL);
                        跳出;
                /* skip comments */
                事例 '/':
                        如 (in_warn_or_error)
                                跳转 _default;
                        file->buf_ptr = p;
                        ch = *p;
                        minp();
                        p = file->buf_ptr;
                        如 (ch == '*') {
                                p = parse_comment(p);
                        } 另 如 (ch == '/') {
                                p = parse_line_comment(p);
                        }
                        跳出;
                事例 '#':
                        p++;
                        如 (start_of_line) {
                                file->buf_ptr = p;
                                next_nomacro();
                                p = file->buf_ptr;
                                如 (a == 0 &&
                                        (tok == TOK_ELSE || tok == TOK_ELSE_CN || tok == TOK_ELIF || tok == TOK_ELIF_CN || tok == TOK_ENDIF || tok == TOK_ENDIF_CN))
                                        跳转 the_end;
                                如 (tok == TOK_IF || tok == TOK_IF_CN || tok == TOK_IFDEF || tok == TOK_IFDEF_CN || tok == TOK_IFNDEF || tok == TOK_IFNDEF_CN)
                                        a++;
                                另 如 (tok == TOK_ENDIF || tok == TOK_ENDIF_CN)
                                        a--;
                                另 如(tok == TOK_ERROR || tok == TOK_ERROR_CN || tok == TOK_WARNING || tok == TOK_WARNING_CN)
                                        in_warn_or_error = 1;
                                另 如 (tok == TOK_LINEFEED)
                                        跳转 redo_start;
                                另 如 (parse_flags & PARSE_FLAG_ASM_FILE)
                                        p = parse_line_comment(p - 1);
                        } 另 如 (parse_flags & PARSE_FLAG_ASM_FILE)
                                p = parse_line_comment(p - 1);
                        跳出;
_default:
                缺省:
                        p++;
                        跳出;
                }
                start_of_line = 0;
        }
 the_end: ;
        file->buf_ptr = p;
}

#如 0
/* return the number of additional 'ints' necessary to store the
   token */
静态 内联 整 tok_size(不变 整 *p)
{
        转接(*p) {
                /* 4 bytes */
        事例 TOK_CINT:
        事例 TOK_CUINT:
        事例 TOK_CCHAR:
        事例 TOK_LCHAR:
        事例 TOK_CFLOAT:
        事例 TOK_LINENUM:
#如未定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG;
        事例 TOK_CULONG;
#了如
                返回 1 + 1;
        事例 TOK_STR:
        事例 TOK_LSTR:
        事例 TOK_PPNUM:
        事例 TOK_PPSTR:
                返回 1 + ((求长度(CString) + ((CString *)(p+1))->size + 3) >> 2);
        事例 TOK_CDOUBLE:
        事例 TOK_CLLONG:
        事例 TOK_CULLONG:
#如定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG;
        事例 TOK_CULONG;
#了如
                返回 1 + 2;
        事例 TOK_CLDOUBLE:
                返回 1 + LDOUBLE_SIZE / 4;
        缺省:
                返回 1 + 0;
        }
}
#了如

/* token string handling */
ST_INLN 空 tok_str_new(TokenString *s)
{
        s->str = NULL;
        s->len = s->lastlen = 0;
        s->allocated_len = 0;
        s->last_line_num = -1;
}

ST_FUNC TokenString *tok_str_alloc(空)
{
        TokenString *str = tal_realloc(tokstr_alloc, 0, 求长度 *str);
        tok_str_new(str);
        返回 str;
}

ST_FUNC 整 *tok_str_dup(TokenString *s)
{
        整 *str;

        str = tal_realloc(tokstr_alloc, 0, s->len * 求长度(整));
        memcpy(str, s->str, s->len * 求长度(整));
        返回 str;
}

ST_FUNC 空 tok_str_free_str(整 *str)
{
        tal_free(tokstr_alloc, str);
}

ST_FUNC 空 tok_str_free(TokenString *str)
{
        tok_str_free_str(str->str);
        tal_free(tokstr_alloc, str);
}

ST_FUNC 整 *tok_str_realloc(TokenString *s, 整 new_size)
{
        整 *str, size;

        size = s->allocated_len;
        如 (size < 16)
                size = 16;
        当 (size < new_size)
                size = size * 2;
        如 (size > s->allocated_len) {
                str = tal_realloc(tokstr_alloc, s->str, size * 求长度(整));
                s->allocated_len = size;
                s->str = str;
        }
        返回 s->str;
}

ST_FUNC 空 tok_str_add(TokenString *s, 整 t)
{
        整 len, *str;

        len = s->len;
        str = s->str;
        如 (len >= s->allocated_len)
                str = tok_str_realloc(s, len + 1);
        str[len++] = t;
        s->len = len;
}

ST_FUNC 空 begin_macro(TokenString *str, 整 alloc)
{
        str->alloc = alloc;
        str->prev = macro_stack;
        str->prev_ptr = macro_ptr;
        str->save_line_num = file->line_num;
        macro_ptr = str->str;
        macro_stack = str;
}

ST_FUNC 空 end_macro(空)
{
        TokenString *str = macro_stack;
        macro_stack = str->prev;
        macro_ptr = str->prev_ptr;
        file->line_num = str->save_line_num;
        如 (str->alloc == 2) {
                str->alloc = 3; /* just mark as finished */
        } 另 {
                tok_str_free(str);
        }
}

静态 空 tok_str_add2(TokenString *s, 整 t, CValue *cv)
{
        整 len, *str;

        len = s->lastlen = s->len;
        str = s->str;

        /* allocate space for worst case */
        如 (len + TOK_MAX_SIZE >= s->allocated_len)
                str = tok_str_realloc(s, len + TOK_MAX_SIZE + 1);
        str[len++] = t;
        转接(t) {
        事例 TOK_CINT:
        事例 TOK_CUINT:
        事例 TOK_CCHAR:
        事例 TOK_LCHAR:
        事例 TOK_CFLOAT:
        事例 TOK_LINENUM:
#如未定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG:
        事例 TOK_CULONG:
#了如
                str[len++] = cv->tab[0];
                跳出;
        事例 TOK_PPNUM:
        事例 TOK_PPSTR:
        事例 TOK_STR:
        事例 TOK_LSTR:
                {
                        /* Insert the string into the int array. */
                        size_t nb_words =
                                1 + (cv->str.size + 求长度(整) - 1) / 求长度(整);
                        如 (len + nb_words >= s->allocated_len)
                                str = tok_str_realloc(s, len + nb_words + 1);
                        str[len] = cv->str.size;
                        memcpy(&str[len + 1], cv->str.data, cv->str.size);
                        len += nb_words;
                }
                跳出;
        事例 TOK_CDOUBLE:
        事例 TOK_CLLONG:
        事例 TOK_CULLONG:
#如定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG:
        事例 TOK_CULONG:
#了如
#如 LDOUBLE_SIZE == 8
        事例 TOK_CLDOUBLE:
#了如
                str[len++] = cv->tab[0];
                str[len++] = cv->tab[1];
                跳出;
#如 LDOUBLE_SIZE == 12
        事例 TOK_CLDOUBLE:
                str[len++] = cv->tab[0];
                str[len++] = cv->tab[1];
                str[len++] = cv->tab[2];
#另如 LDOUBLE_SIZE == 16
        事例 TOK_CLDOUBLE:
                str[len++] = cv->tab[0];
                str[len++] = cv->tab[1];
                str[len++] = cv->tab[2];
                str[len++] = cv->tab[3];
#另如 LDOUBLE_SIZE != 8
#错误 add long double size support
#了如
                跳出;
        缺省:
                跳出;
        }
        s->len = len;
}

/* add the current parse token in token string 's' */
ST_FUNC 空 tok_str_add_tok(TokenString *s)
{
        CValue cval;

        /* save line number info */
        如 (file->line_num != s->last_line_num) {
                s->last_line_num = file->line_num;
                cval.i = s->last_line_num;
                tok_str_add2(s, TOK_LINENUM, &cval);
        }
        tok_str_add2(s, tok, &tokc);
}

/* get a token from an integer array and increment pointer
   accordingly. we code it as a macro to avoid pointer aliasing. */
静态 内联 空 TOK_GET(整 *t, 不变 整 **pp, CValue *cv)
{
        不变 整 *p = *pp;
        整 n, *tab;

        tab = cv->tab;
        转接(*t = *p++) {
        事例 TOK_CINT:
        事例 TOK_CUINT:
        事例 TOK_CCHAR:
        事例 TOK_LCHAR:
        事例 TOK_LINENUM:
#如未定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG:
        事例 TOK_CULONG:
#了如
                tab[0] = *p++;
                cv->i = (*t == TOK_CUINT) ? (无符)cv->i : (整)cv->i;
                跳出;
        事例 TOK_CFLOAT:
                tab[0] = *p++;
                跳出;
        事例 TOK_STR:
        事例 TOK_LSTR:
        事例 TOK_PPNUM:
        事例 TOK_PPSTR:
                cv->str.size = *p++;
                cv->str.data = p;
                p += (cv->str.size + 求长度(整) - 1) / 求长度(整);
                跳出;
        事例 TOK_CDOUBLE:
        事例 TOK_CLLONG:
        事例 TOK_CULLONG:
#如定义 TCC_LONG_ARE_64_BIT
        事例 TOK_CLONG:
        事例 TOK_CULONG:
#了如
                n = 2;
                跳转 copy;
        事例 TOK_CLDOUBLE:
#如 LDOUBLE_SIZE == 16
                n = 4;
#另如 LDOUBLE_SIZE == 12
                n = 3;
#另如 LDOUBLE_SIZE == 8
                n = 2;
#另
# 错误 add long double size support
#了如
        copy:
                运行
                        *tab++ = *p++;
                当 (--n);
                跳出;
        缺省:
                跳出;
        }
        *pp = p;
}

静态 整 macro_is_equal(不变 整 *a, 不变 整 *b)
{
        CValue cv;
        整 t;

        如 (!a || !b)
                返回 1;

        当 (*a && *b) {
                /* first time preallocate macro_equal_buf, next time only reset position to start */
                cstr_reset(&macro_equal_buf);
                TOK_GET(&t, &a, &cv);
                cstr_cat(&macro_equal_buf, get_tok_str(t, &cv), 0);
                TOK_GET(&t, &b, &cv);
                如 (strcmp(macro_equal_buf.data, get_tok_str(t, &cv)))
                        返回 0;
        }
        返回 !(*a || *b);
}

/* defines handling */
ST_INLN 空 define_push(整 v, 整 macro_type, 整 *str, Sym *first_arg)
{
        Sym *s, *o;

        o = define_find(v);
        s = sym_push2(&define_stack, v, macro_type, 0);
        s->d = str;
        s->next = first_arg;
        table_ident[v - TOK_IDENT]->sym_define = s;

        如 (o && !macro_is_equal(o->d, s->d))
                tcc_warning("%s redefined", get_tok_str(v, NULL));
}

/* undefined a define symbol. Its name is just set to zero */
ST_FUNC 空 define_undef(Sym *s)
{
        整 v = s->v;
        如 (v >= TOK_IDENT && v < tok_ident)
                table_ident[v - TOK_IDENT]->sym_define = NULL;
}

ST_INLN Sym *define_find(整 v)
{
        v -= TOK_IDENT;
        如 ((无符)v >= (无符)(tok_ident - TOK_IDENT))
                返回 NULL;
        返回 table_ident[v]->sym_define;
}

/* free define stack until top reaches 'b' */
ST_FUNC 空 free_defines(Sym *b)
{
        当 (define_stack != b) {
                Sym *top = define_stack;
                define_stack = top->prev;
                tok_str_free_str(top->d);
                define_undef(top);
                sym_free(top);
        }

        /* restore remaining (-D or predefined) symbols if they were
           #undef'd in the file */
        当 (b) {
                整 v = b->v;
                如 (v >= TOK_IDENT && v < tok_ident) {
                        Sym **d = &table_ident[v - TOK_IDENT]->sym_define;
                        如 (!*d)
                                *d = b;
                }
                b = b->prev;
        }
}

/* label lookup */
ST_FUNC Sym *label_find(整 v)
{
        v -= TOK_IDENT;
        如 ((无符)v >= (无符)(tok_ident - TOK_IDENT))
                返回 NULL;
        返回 table_ident[v]->sym_label;
}

ST_FUNC Sym *label_push(Sym **ptop, 整 v, 整 flags)
{
        Sym *s, **ps;
        s = sym_push2(ptop, v, 0, 0);
        s->r = flags;
        ps = &table_ident[v - TOK_IDENT]->sym_label;
        如 (ptop == &global_label_stack) {
                /* modify the top most local identifier, so that
                   sym_identifier will point to 's' when popped */
                当 (*ps != NULL)
                        ps = &(*ps)->prev_tok;
        }
        s->prev_tok = *ps;
        *ps = s;
        返回 s;
}

/* pop labels until element last is reached. Look if any labels are
   undefined. Define symbols if '&&label' was used. */
ST_FUNC 空 label_pop(Sym **ptop, Sym *slast, 整 keep)
{
        Sym *s, *s1;
        对于(s = *ptop; s != slast; s = s1) {
                s1 = s->prev;
                如 (s->r == LABEL_DECLARED) {
                        tcc_warning("label '%s' declared but not used", get_tok_str(s->v, NULL));
                } 另 如 (s->r == LABEL_FORWARD) {
                                tcc_error("label '%s' used but not defined",
                                          get_tok_str(s->v, NULL));
                } 另 {
                        如 (s->c) {
                                /* define corresponding symbol. A size of
                                   1 is put. */
                                put_extern_sym(s, cur_text_section, s->jnext, 1);
                        }
                }
                /* remove label */
                table_ident[s->v - TOK_IDENT]->sym_label = s->prev_tok;
                如 (!keep)
                        sym_free(s);
        }
        如 (!keep)
                *ptop = slast;
}

/* fake the nth "#if defined test_..." for tcc -dt -run */
静态 空 maybe_run_test(TCCState *s)
{
        不变 字 *p;
        如 (s->include_stack_ptr != s->include_stack)
                返回;
        p = get_tok_str(tok, NULL);
        如 (0 != memcmp(p, "test_", 5))
                返回;
        如 (0 != --s->run_test)
                返回;
        fprintf(s->ppfp, "\n[%s]\n" + !(s->dflag & 32), p), fflush(s->ppfp);
        define_push(tok, MACRO_OBJ, NULL, NULL);
}

/* eval an expression for #if/#elif */
静态 整 expr_preprocess(空)
{
        整 c, t;
        TokenString *str;

        str = tok_str_alloc();
        pp_expr = 1;
        当 (tok != TOK_LINEFEED && tok != TOK_EOF) {
                next(); /* do macro subst */
                如 (tok == TOK_DEFINED || tok == TOK_DEFINED_CN) {
                        next_nomacro();
                        t = tok;
                        如 (t == '(')
                                next_nomacro();
                        如 (tok < TOK_IDENT)
                                expect("identifier");
                        如 (tcc_state->run_test)
                                maybe_run_test(tcc_state);
                        c = define_find(tok) != 0;
                        如 (t == '(') {
                                next_nomacro();
                                如 (tok != ')')
                                        expect("')'");
                        }
                        tok = TOK_CINT;
                        tokc.i = c;
                } 另 如 (tok >= TOK_IDENT) {
                        /* if undefined macro */
                        tok = TOK_CINT;
                        tokc.i = 0;
                }
                tok_str_add_tok(str);
        }
        pp_expr = 0;
        tok_str_add(str, -1); /* simulate end of file */
        tok_str_add(str, 0);
        /* now evaluate C constant expression */
        begin_macro(str, 1);
        next();
        c = expr_const();
        end_macro();
        返回 c != 0;
}


/* parse after #define */
ST_FUNC 空 parse_define(空)
{
        Sym *s, *first, **ps;
        整 v, t, varg, is_vaargs, spc;
        整 saved_parse_flags = parse_flags;

        v = tok;
        如 (v < TOK_IDENT || v == TOK_DEFINED || v == TOK_DEFINED_CN)
                tcc_error("invalid macro name '%s'", get_tok_str(tok, &tokc));
        /* XXX: should check if same macro (ANSI) */
        first = NULL;
        t = MACRO_OBJ;
        /* We have to parse the whole define as if not in asm mode, in particular
           no line comment with '#' must be ignored.  Also for function
           macros the argument list must be parsed without '.' being an ID
           character.  */
        parse_flags = ((parse_flags & ~PARSE_FLAG_ASM_FILE) | PARSE_FLAG_SPACES);
        /* '(' must be just after macro definition for MACRO_FUNC */
        next_nomacro_spc();
        如 (tok == '(') {
                整 dotid = set_idnum('.', 0);
                next_nomacro();
                ps = &first;
                如 (tok != ')') 对于 (;;) {
                        varg = tok;
                        next_nomacro();
                        is_vaargs = 0;
                        如 (varg == TOK_DOTS) {
                                varg = TOK___VA_ARGS__;
                                is_vaargs = 1;
                        } 另 如 (tok == TOK_DOTS && gnu_ext) {
                                is_vaargs = 1;
                                next_nomacro();
                        }
                        如 (varg < TOK_IDENT)
                bad_list:
                                tcc_error("bad macro parameter list");
                        s = sym_push2(&define_stack, varg | SYM_FIELD, is_vaargs, 0);
                        *ps = s;
                        ps = &s->next;
                        如 (tok == ')')
                                跳出;
                        如 (tok != ',' || is_vaargs)
                                跳转 bad_list;
                        next_nomacro();
                }
                next_nomacro_spc();
                t = MACRO_FUNC;
                set_idnum('.', dotid);
        }

        tokstr_buf.len = 0;
        spc = 2;
        parse_flags |= PARSE_FLAG_ACCEPT_STRAYS | PARSE_FLAG_SPACES | PARSE_FLAG_LINEFEED;
        /* The body of a macro definition should be parsed such that identifiers
           are parsed like the file mode determines (i.e. with '.' being an
           ID character in asm mode).  But '#' should be retained instead of
           regarded as line comment leader, so still don't set ASM_FILE
           in parse_flags. */
        当 (tok != TOK_LINEFEED && tok != TOK_EOF) {
                /* remove spaces around ## and after '#' */
                如 (TOK_TWOSHARPS == tok) {
                        如 (2 == spc)
                                跳转 bad_twosharp;
                        如 (1 == spc)
                                --tokstr_buf.len;
                        spc = 3;
                        tok = TOK_PPJOIN;
                } 另 如 ('#' == tok) {
                        spc = 4;
                } 另 如 (check_space(tok, &spc)) {
                        跳转 skip;
                }
                tok_str_add2(&tokstr_buf, tok, &tokc);
        skip:
                next_nomacro_spc();
        }

        parse_flags = saved_parse_flags;
        如 (spc == 1)
                --tokstr_buf.len; /* remove trailing space */
        tok_str_add(&tokstr_buf, 0);
        如 (3 == spc)
bad_twosharp:
                tcc_error("'##' cannot appear at either end of macro");
        define_push(v, t, tok_str_dup(&tokstr_buf), first);
}

静态 CachedInclude *search_cached_include(TCCState *s1, 不变 字 *filename, 整 add)
{
        不变 无符 字 *s;
        无符 整 h;
        CachedInclude *e;
        整 i;

        h = TOK_HASH_INIT;
        s = (无符 字 *) filename;
        当 (*s) {
#如定义 _WIN32
                h = TOK_HASH_FUNC(h, toup(*s));
#另
                h = TOK_HASH_FUNC(h, *s);
#了如
                s++;
        }
        h &= (CACHED_INCLUDES_HASH_SIZE - 1);

        i = s1->cached_includes_hash[h];
        对于(;;) {
                如 (i == 0)
                        跳出;
                e = s1->cached_includes[i - 1];
                如 (0 == PATHCMP(e->filename, filename))
                        返回 e;
                i = e->hash_next;
        }
        如 (!add)
                返回 NULL;

        e = tcc_malloc(求长度(CachedInclude) + strlen(filename));
        strcpy(e->filename, filename);
        e->ifndef_macro = e->once = 0;
        dynarray_add(&s1->cached_includes, &s1->nb_cached_includes, e);
        /* add in hash table */
        e->hash_next = s1->cached_includes_hash[h];
        s1->cached_includes_hash[h] = s1->nb_cached_includes;
#如定义 INC_DEBUG
        printf("adding cached '%s'\n", filename);
#了如
        返回 e;
}

静态 空 pragma_parse(TCCState *s1)
{
        next_nomacro();
        如 (tok == TOK_push_macro || tok == TOK_pop_macro) {
                整 t = tok, v;
                Sym *s;

                如 (next(), tok != '(')
                        跳转 pragma_err;
                如 (next(), tok != TOK_STR)
                        跳转 pragma_err;
                v = tok_alloc(tokc.str.data, tokc.str.size - 1)->tok;
                如 (next(), tok != ')')
                        跳转 pragma_err;
                如 (t == TOK_push_macro) {
                        当 (NULL == (s = define_find(v)))
                                define_push(v, 0, NULL, NULL);
                        s->type.ref = s; /* set push boundary */
                } 另 {
                        对于 (s = define_stack; s; s = s->prev)
                                如 (s->v == v && s->type.ref == s) {
                                        s->type.ref = NULL;
                                        跳出;
                                }
                }
                如 (s)
                        table_ident[v - TOK_IDENT]->sym_define = s->d ? s : NULL;
                另
                        tcc_warning("unbalanced #pragma pop_macro");
                pp_debug_tok = t, pp_debug_symv = v;

        } 另 如 (tok == TOK_once) {
                search_cached_include(s1, file->filename, 1)->once = pp_once;

        } 另 如 (s1->output_type == TCC_OUTPUT_PREPROCESS) {
                /* tcc -E: keep pragmas below unchanged */
                unget_tok(' ');
                unget_tok(TOK_PRAGMA);
                unget_tok('#');
                unget_tok(TOK_LINEFEED);

        } 另 如 (tok == TOK_pack) {
                /* This may be:
                   #pragma pack(1) // set
                   #pragma pack() // reset to default
                   #pragma pack(push,1) // push & set
                   #pragma pack(pop) // restore previous */
                next();
                skip('(');
                如 (tok == TOK_ASM_pop) {
                        next();
                        如 (s1->pack_stack_ptr <= s1->pack_stack) {
                        stk_error:
                                tcc_error("out of pack stack");
                        }
                        s1->pack_stack_ptr--;
                } 另 {
                        整 val = 0;
                        如 (tok != ')') {
                                如 (tok == TOK_ASM_push) {
                                        next();
                                        如 (s1->pack_stack_ptr >= s1->pack_stack + PACK_STACK_SIZE - 1)
                                                跳转 stk_error;
                                        s1->pack_stack_ptr++;
                                        skip(',');
                                }
                                如 (tok != TOK_CINT)
                                        跳转 pragma_err;
                                val = tokc.i;
                                如 (val < 1 || val > 16 || (val & (val - 1)) != 0)
                                        跳转 pragma_err;
                                next();
                        }
                        *s1->pack_stack_ptr = val;
                }
                如 (tok != ')')
                        跳转 pragma_err;

        } 另 如 (tok == TOK_comment) {
                字 *p; 整 t;
                next();
                skip('(');
                t = tok;
                next();
                skip(',');
                如 (tok != TOK_STR)
                        跳转 pragma_err;
                p = tcc_strdup((字 *)tokc.str.data);
                next();
                如 (tok != ')')
                        跳转 pragma_err;
                如 (t == TOK_lib) {
                        dynarray_add(&s1->pragma_libs, &s1->nb_pragma_libs, p);
                } 另 {
                        如 (t == TOK_option)
                                tcc_set_options(s1, p);
                        tcc_free(p);
                }

        } 另 如 (s1->warn_unsupported) {
                tcc_warning("#pragma %s is ignored", get_tok_str(tok, &tokc));
        }
        返回;

pragma_err:
        tcc_error("malformed #pragma directive");
        返回;
}

/* is_bof is true if first non space token at beginning of file */
ST_FUNC 空 preprocess(整 is_bof)
{
        TCCState *s1 = tcc_state;
        整 i, c, n, saved_parse_flags;
        字 buf[1024], *q;
        Sym *s;

        saved_parse_flags = parse_flags;
        parse_flags = PARSE_FLAG_PREPROCESS
                | PARSE_FLAG_TOK_NUM
                | PARSE_FLAG_TOK_STR
                | PARSE_FLAG_LINEFEED
                | (parse_flags & PARSE_FLAG_ASM_FILE)
                ;

        next_nomacro();
 redo:
        转接(tok) {
        事例 TOK_DEFINE:
        事例 TOK_DEFINE_CN:
                pp_debug_tok = tok;
                next_nomacro();
                pp_debug_symv = tok;
                parse_define();
                跳出;
        事例 TOK_UNDEF:
        事例 TOK_UNDEF_CN:
                pp_debug_tok = tok;
                next_nomacro();
                pp_debug_symv = tok;
                s = define_find(tok);
                /* undefine symbol by putting an invalid name */
                如 (s)
                        define_undef(s);
                跳出;
        事例 TOK_INCLUDE:
        事例 TOK_INCLUDE_CN:
        事例 TOK_INCLUDE_NEXT:
        事例 TOK_INCLUDE_NEXT_CN:
                ch = file->buf_ptr[0];
                /* XXX: incorrect if comments : use next_nomacro with a special mode */
                skip_spaces();
                如 (ch == '<') {
                        c = '>';
                        跳转 read_name;
                } 另 如 (ch == '\"') {
                        c = ch;
                read_name:
                        inp();
                        q = buf;
                        当 (ch != c && ch != '\n' && ch != CH_EOF) {
                                如 ((q - buf) < 求长度(buf) - 1)
                                        *q++ = ch;
                                如 (ch == '\\') {
                                        如 (handle_stray_noerror() == 0)
                                                --q;
                                } 另
                                        inp();
                        }
                        *q = '\0';
                        minp();
#如 0
                        /* eat all spaces and comments after include */
                        /* XXX: slightly incorrect */
                        当 (ch1 != '\n' && ch1 != CH_EOF)
                                inp();
#了如
                } 另 {
                        整 len;
                        /* computed #include : concatenate everything up to linefeed,
                           the result must be one of the two accepted forms.
                           Don't convert pp-tokens to tokens here.  */
                        parse_flags = (PARSE_FLAG_PREPROCESS
                                | PARSE_FLAG_LINEFEED
                                | (parse_flags & PARSE_FLAG_ASM_FILE));
                        next();
                        buf[0] = '\0';
                        当 (tok != TOK_LINEFEED) {
                                pstrcat(buf, 求长度(buf), get_tok_str(tok, &tokc));
                                next();
                        }
                        len = strlen(buf);
                        /* check syntax and remove '<>|""' */
                        如 ((len < 2 || ((buf[0] != '"' || buf[len-1] != '"') &&
                                (buf[0] != '<' || buf[len-1] != '>'))))
                                tcc_error("'#include' expects \"FILENAME\" or <FILENAME>");
                        c = buf[len-1];
                        memmove(buf, buf + 1, len - 2);
                        buf[len - 2] = '\0';
                }

                如 (s1->include_stack_ptr >= s1->include_stack + INCLUDE_STACK_SIZE)
                        tcc_error("#include recursion too deep");
                /* store current file in stack, but increment stack later below */
                *s1->include_stack_ptr = file;
                i = (tok == TOK_INCLUDE_NEXT || tok == TOK_INCLUDE_NEXT_CN) ? file->include_next_index : 0;
                n = 2 + s1->nb_include_paths + s1->nb_sysinclude_paths;
                对于 (; i < n; ++i) {
                        字 buf1[求长度 file->filename];
                        CachedInclude *e;
                        不变 字 *path;

                        如 (i == 0) {
                                /* check absolute include path */
                                如 (!IS_ABSPATH(buf))
                                        继续;
                                buf1[0] = 0;

                        } 另 如 (i == 1) {
                                /* search in file's dir if "header.h" */
                                如 (c != '\"')
                                        继续;
                                /* https://savannah.nongnu.org/bugs/index.php?50847 */
                                path = file->true_filename;
                                pstrncpy(buf1, path, tcc_basename(path) - path);

                        } 另 {
                                /* search in all the include paths */
                                整 j = i - 2, k = j - s1->nb_include_paths;
                                path = k < 0 ? s1->include_paths[j] : s1->sysinclude_paths[k];
                                pstrcpy(buf1, 求长度(buf1), path);
                                pstrcat(buf1, 求长度(buf1), "/");
                        }

                        pstrcat(buf1, 求长度(buf1), buf);
                        e = search_cached_include(s1, buf1, 0);
                        如 (e && (define_find(e->ifndef_macro) || e->once == pp_once)) {
                                /* no need to parse the include because the 'ifndef macro'
                                   is defined (or had #pragma once) */
#如定义 INC_DEBUG
                                printf("%s: skipping cached %s\n", file->filename, buf1);
#了如
                                跳转 include_done;
                        }

                        如 (tcc_open(s1, buf1) < 0)
                                继续;

                        file->include_next_index = i + 1;
#如定义 INC_DEBUG
                        printf("%s: including %s\n", file->prev->filename, file->filename);
#了如
                        /* update target deps */
                        dynarray_add(&s1->target_deps, &s1->nb_target_deps,
                                        tcc_strdup(buf1));
                        /* push current file in stack */
                        ++s1->include_stack_ptr;
                        /* add include file debug info */
                        如 (s1->do_debug)
                                put_stabs(file->filename, N_BINCL, 0, 0, 0);
                        tok_flags |= TOK_FLAG_BOF | TOK_FLAG_BOL;
                        ch = file->buf_ptr[0];
                        跳转 the_end;
                }
                tcc_error("include file '%s' not found", buf);
include_done:
                跳出;
        事例 TOK_IFNDEF:
        事例 TOK_IFNDEF_CN:
                c = 1;
                跳转 do_ifdef;
        事例 TOK_IF:
        事例 TOK_IF_CN:
                c = expr_preprocess();
                跳转 do_if;
        事例 TOK_IFDEF:
        事例 TOK_IFDEF_CN:
                c = 0;
        do_ifdef:
                next_nomacro();
                如 (tok < TOK_IDENT)
                        tcc_error("invalid argument for '#if%sdef'", c ? "n" : "");
                如 (is_bof) {
                        如 (c) {
#如定义 INC_DEBUG
                                printf("#ifndef %s\n", get_tok_str(tok, NULL));
#了如
                                file->ifndef_macro = tok;
                        }
                }
                c = (define_find(tok) != 0) ^ c;
        do_if:
                如 (s1->ifdef_stack_ptr >= s1->ifdef_stack + IFDEF_STACK_SIZE)
                        tcc_error("memory full (如定义)");
                *s1->ifdef_stack_ptr++ = c;
                跳转 test_skip;
        事例 TOK_ELSE:
        事例 TOK_ELSE_CN:
                如 (s1->ifdef_stack_ptr == s1->ifdef_stack)
                        tcc_error("#else without matching #if");
                如 (s1->ifdef_stack_ptr[-1] & 2)
                        tcc_error("#else after #else");
                c = (s1->ifdef_stack_ptr[-1] ^= 3);
                跳转 test_else;
        事例 TOK_ELIF:
        事例 TOK_ELIF_CN:
                如 (s1->ifdef_stack_ptr == s1->ifdef_stack)
                        tcc_error("#elif without matching #if");
                c = s1->ifdef_stack_ptr[-1];
                如 (c > 1)
                        tcc_error("#elif after #else");
                /* last #if/#elif expression was true: we skip */
                如 (c == 1) {
                        c = 0;
                } 另 {
                        c = expr_preprocess();
                        s1->ifdef_stack_ptr[-1] = c;
                }
        test_else:
                如 (s1->ifdef_stack_ptr == file->ifdef_stack_ptr + 1)
                        file->ifndef_macro = 0;
        test_skip:
                如 (!(c & 1)) {
                        preprocess_skip();
                        is_bof = 0;
                        跳转 redo;
                }
                跳出;
        事例 TOK_ENDIF:
        事例 TOK_ENDIF_CN:
                如 (s1->ifdef_stack_ptr <= file->ifdef_stack_ptr)
                        tcc_error("#endif without matching #if");
                s1->ifdef_stack_ptr--;
                /* '#ifndef macro' was at the start of file. Now we check if
                   an '#endif' is exactly at the end of file */
                如 (file->ifndef_macro &&
                        s1->ifdef_stack_ptr == file->ifdef_stack_ptr) {
                        file->ifndef_macro_saved = file->ifndef_macro;
                        /* need to set to zero to avoid false matches if another
                           #ifndef at middle of file */
                        file->ifndef_macro = 0;
                        当 (tok != TOK_LINEFEED)
                                next_nomacro();
                        tok_flags |= TOK_FLAG_ENDIF;
                        跳转 the_end;
                }
                跳出;
        事例 TOK_PPNUM:
                n = strtoul((字*)tokc.str.data, &q, 10);
                跳转 _line_num;
        事例 TOK_LINE:
        事例 TOK_LINE_CN:
                next();
                如 (tok != TOK_CINT)
        _line_err:
                        tcc_error("wrong #line format");
                n = tokc.i;
        _line_num:
                next();
                如 (tok != TOK_LINEFEED) {
                        如 (tok == TOK_STR) {
                                如 (file->true_filename == file->filename)
                                        file->true_filename = tcc_strdup(file->filename);
                                pstrcpy(file->filename, 求长度(file->filename), (字 *)tokc.str.data);
                        } 另 如 (parse_flags & PARSE_FLAG_ASM_FILE)
                                跳出;
                        另
                                跳转 _line_err;
                        --n;
                }
                如 (file->fd > 0)
                        total_lines += file->line_num - n;
                file->line_num = n;
                如 (s1->do_debug)
                        put_stabs(file->filename, N_BINCL, 0, 0, 0);
                跳出;
        事例 TOK_ERROR:
        事例 TOK_ERROR_CN:
        事例 TOK_WARNING:
        事例 TOK_WARNING_CN:
                c = tok;
                ch = file->buf_ptr[0];
                skip_spaces();
                q = buf;
                当 (ch != '\n' && ch != CH_EOF) {
                        如 ((q - buf) < 求长度(buf) - 1)
                                *q++ = ch;
                        如 (ch == '\\') {
                                如 (handle_stray_noerror() == 0)
                                        --q;
                        } 另
                                inp();
                }
                *q = '\0';
                如 (c == TOK_ERROR || c == TOK_ERROR_CN)
                        tcc_error("#error %s", buf);
                另
                        tcc_warning("#warning %s", buf);
                跳出;
        事例 TOK_PRAGMA:
        事例 TOK_PRAGMA_CN:
                pragma_parse(s1);
                跳出;
        事例 TOK_LINEFEED:
                跳转 the_end;
        缺省:
                /* ignore gas line comment in an 'S' file. */
                如 (saved_parse_flags & PARSE_FLAG_ASM_FILE)
                        跳转 ignore;
                如 (tok == '!' && is_bof)
                        /* '!' is ignored at beginning to allow C scripts. */
                        跳转 ignore;
                tcc_warning("Ignoring unknown preprocessing directive #%s", get_tok_str(tok, &tokc));
        ignore:
                file->buf_ptr = parse_line_comment(file->buf_ptr - 1);
                跳转 the_end;
        }
        /* ignore other preprocess commands or #! for C scripts */
        当 (tok != TOK_LINEFEED)
                next_nomacro();
 the_end:
        parse_flags = saved_parse_flags;
}

/* evaluate escape codes in a string. */
静态 空 parse_escape_string(CString *outstr, 不变 uint8_t *buf, 整 is_long)
{
        整 c, n;
        不变 uint8_t *p;

        p = buf;
        对于(;;) {
                c = *p;
                如 (c == '\0')
                        跳出;
                如 (c == '\\') {
                        p++;
                        /* escape */
                        c = *p;
                        转接(c) {
                        事例 '0': 事例 '1': 事例 '2': 事例 '3':
                        事例 '4': 事例 '5': 事例 '6': 事例 '7':
                                /* at most three octal digits */
                                n = c - '0';
                                p++;
                                c = *p;
                                如 (isoct(c)) {
                                        n = n * 8 + c - '0';
                                        p++;
                                        c = *p;
                                        如 (isoct(c)) {
                                                n = n * 8 + c - '0';
                                                p++;
                                        }
                                }
                                c = n;
                                跳转 add_char_nonext;
                        事例 'x':
                        事例 'u':
                        事例 'U':
                                p++;
                                n = 0;
                                对于(;;) {
                                        c = *p;
                                        如 (c >= 'a' && c <= 'f')
                                                c = c - 'a' + 10;
                                        另 如 (c >= 'A' && c <= 'F')
                                                c = c - 'A' + 10;
                                        另 如 (isnum(c))
                                                c = c - '0';
                                        另
                                                跳出;
                                        n = n * 16 + c;
                                        p++;
                                }
                                c = n;
                                跳转 add_char_nonext;
                        事例 'a':
                                c = '\a';
                                跳出;
                        事例 'b':
                                c = '\b';
                                跳出;
                        事例 'f':
                                c = '\f';
                                跳出;
                        事例 'n':
                                c = '\n';
                                跳出;
                        事例 'r':
                                c = '\r';
                                跳出;
                        事例 't':
                                c = '\t';
                                跳出;
                        事例 'v':
                                c = '\v';
                                跳出;
                        事例 'e':
                                如 (!gnu_ext)
                                        跳转 invalid_escape;
                                c = 27;
                                跳出;
                        事例 '\'':
                        事例 '\"':
                        事例 '\\':
                        事例 '?':
                                跳出;
                        缺省:
                        invalid_escape:
                                如 (c >= '!' && c <= '~')
                                        tcc_warning("unknown escape sequence: \'\\%c\'", c);
                                另
                                        tcc_warning("unknown escape sequence: \'\\x%x\'", c);
                                跳出;
                        }
                }
                p++;
        add_char_nonext:
                如 (!is_long)
                        cstr_ccat(outstr, c);
                另
                        cstr_wccat(outstr, c);
        }
        /* add a trailing '\0' */
        如 (!is_long)
                cstr_ccat(outstr, '\0');
        另
                cstr_wccat(outstr, '\0');
}

静态 空 parse_string(不变 字 *s, 整 len)
{
        uint8_t buf[1000], *p = buf;
        整 is_long, sep;

        如 ((is_long = *s == 'L'))
                ++s, --len;
        sep = *s++;
        len -= 2;
        如 (len >= 求长度 buf)
                p = tcc_malloc(len + 1);
        memcpy(p, s, len);
        p[len] = 0;

        cstr_reset(&tokcstr);
        parse_escape_string(&tokcstr, p, is_long);
        如 (p != buf)
                tcc_free(p);

        如 (sep == '\'') {
                整 char_size;
                /* XXX: make it portable */
                如 (!is_long)
                        char_size = 1;
                另
                        char_size = 求长度(nwchar_t);
                如 (tokcstr.size <= char_size)
                        tcc_error("empty character constant");
                如 (tokcstr.size > 2 * char_size)
                        tcc_warning("multi-character character constant");
                如 (!is_long) {
                        tokc.i = *(int8_t *)tokcstr.data;
                        tok = TOK_CCHAR;
                } 另 {
                        tokc.i = *(nwchar_t *)tokcstr.data;
                        tok = TOK_LCHAR;
                }
        } 另 {
                tokc.str.size = tokcstr.size;
                tokc.str.data = tokcstr.data;
                如 (!is_long)
                        tok = TOK_STR;
                另
                        tok = TOK_LSTR;
        }
}

/* we use 64 bit numbers */
#定义 BN_SIZE 2

/* bn = (bn << shift) | or_val */
静态 空 bn_lshift(无符 整 *bn, 整 shift, 整 or_val)
{
        整 i;
        无符 整 v;
        对于(i=0;i<BN_SIZE;i++) {
                v = bn[i];
                bn[i] = (v << shift) | or_val;
                or_val = v >> (32 - shift);
        }
}

静态 空 bn_zero(无符 整 *bn)
{
        整 i;
        对于(i=0;i<BN_SIZE;i++) {
                bn[i] = 0;
        }
}

/* parse number in null terminated string 'p' and return it in the
   current token */
静态 空 parse_number(不变 字 *p)
{
        整 b, t, shift, frac_bits, s, exp_val, ch;
        字 *q;
        无符 整 bn[BN_SIZE];
        双精 d;

        /* number */
        q = token_buf;
        ch = *p++;
        t = ch;
        ch = *p++;
        *q++ = t;
        b = 10;
        如 (t == '.') {
                跳转 float_frac_parse;
        } 另 如 (t == '0') {
                如 (ch == 'x' || ch == 'X') {
                        q--;
                        ch = *p++;
                        b = 16;
                } 另 如 (tcc_ext && (ch == 'b' || ch == 'B')) {
                        q--;
                        ch = *p++;
                        b = 2;
                }
        }
        /* parse all digits. cannot check octal numbers at this stage
           because of floating point constants */
        当 (1) {
                如 (ch >= 'a' && ch <= 'f')
                        t = ch - 'a' + 10;
                另 如 (ch >= 'A' && ch <= 'F')
                        t = ch - 'A' + 10;
                另 如 (isnum(ch))
                        t = ch - '0';
                另
                        跳出;
                如 (t >= b)
                        跳出;
                如 (q >= token_buf + STRING_MAX_SIZE) {
                num_too_long:
                        tcc_error("number too long");
                }
                *q++ = ch;
                ch = *p++;
        }
        如 (ch == '.' ||
                ((ch == 'e' || ch == 'E') && b == 10) ||
                ((ch == 'p' || ch == 'P') && (b == 16 || b == 2))) {
                如 (b != 10) {
                        /* NOTE: strtox should support that for hexa numbers, but
                           non ISOC99 libcs do not support it, so we prefer to do
                           it by hand */
                        /* hexadecimal or binary floats */
                        /* XXX: handle overflows */
                        *q = '\0';
                        如 (b == 16)
                                shift = 4;
                        另
                                shift = 1;
                        bn_zero(bn);
                        q = token_buf;
                        当 (1) {
                                t = *q++;
                                如 (t == '\0') {
                                        跳出;
                                } 另 如 (t >= 'a') {
                                        t = t - 'a' + 10;
                                } 另 如 (t >= 'A') {
                                        t = t - 'A' + 10;
                                } 另 {
                                        t = t - '0';
                                }
                                bn_lshift(bn, shift, t);
                        }
                        frac_bits = 0;
                        如 (ch == '.') {
                                ch = *p++;
                                当 (1) {
                                        t = ch;
                                        如 (t >= 'a' && t <= 'f') {
                                                t = t - 'a' + 10;
                                        } 另 如 (t >= 'A' && t <= 'F') {
                                                t = t - 'A' + 10;
                                        } 另 如 (t >= '0' && t <= '9') {
                                                t = t - '0';
                                        } 另 {
                                                跳出;
                                        }
                                        如 (t >= b)
                                                tcc_error("invalid digit");
                                        bn_lshift(bn, shift, t);
                                        frac_bits += shift;
                                        ch = *p++;
                                }
                        }
                        如 (ch != 'p' && ch != 'P')
                                expect("exponent");
                        ch = *p++;
                        s = 1;
                        exp_val = 0;
                        如 (ch == '+') {
                                ch = *p++;
                        } 另 如 (ch == '-') {
                                s = -1;
                                ch = *p++;
                        }
                        如 (ch < '0' || ch > '9')
                                expect("exponent digits");
                        当 (ch >= '0' && ch <= '9') {
                                exp_val = exp_val * 10 + ch - '0';
                                ch = *p++;
                        }
                        exp_val = exp_val * s;

                        /* now we can generate the number */
                        /* XXX: should patch directly float number */
                        d = (双精)bn[1] * 4294967296.0 + (双精)bn[0];
                        d = ldexp(d, exp_val - frac_bits);
                        t = toup(ch);
                        如 (t == 'F') {
                                ch = *p++;
                                tok = TOK_CFLOAT;
                                /* float : should handle overflow */
                                tokc.f = (单精)d;
                        } 另 如 (t == 'L') {
                                ch = *p++;
#如定义 TCC_TARGET_PE
                                tok = TOK_CDOUBLE;
                                tokc.d = d;
#另
                                tok = TOK_CLDOUBLE;
                                /* XXX: not large enough */
                                tokc.ld = (长 双精)d;
#了如
                        } 另 {
                                tok = TOK_CDOUBLE;
                                tokc.d = d;
                        }
                } 另 {
                        /* decimal floats */
                        如 (ch == '.') {
                                如 (q >= token_buf + STRING_MAX_SIZE)
                                        跳转 num_too_long;
                                *q++ = ch;
                                ch = *p++;
                        float_frac_parse:
                                当 (ch >= '0' && ch <= '9') {
                                        如 (q >= token_buf + STRING_MAX_SIZE)
                                                跳转 num_too_long;
                                        *q++ = ch;
                                        ch = *p++;
                                }
                        }
                        如 (ch == 'e' || ch == 'E') {
                                如 (q >= token_buf + STRING_MAX_SIZE)
                                        跳转 num_too_long;
                                *q++ = ch;
                                ch = *p++;
                                如 (ch == '-' || ch == '+') {
                                        如 (q >= token_buf + STRING_MAX_SIZE)
                                                跳转 num_too_long;
                                        *q++ = ch;
                                        ch = *p++;
                                }
                                如 (ch < '0' || ch > '9')
                                        expect("exponent digits");
                                当 (ch >= '0' && ch <= '9') {
                                        如 (q >= token_buf + STRING_MAX_SIZE)
                                                跳转 num_too_long;
                                        *q++ = ch;
                                        ch = *p++;
                                }
                        }
                        *q = '\0';
                        t = toup(ch);
                        errno = 0;
                        如 (t == 'F') {
                                ch = *p++;
                                tok = TOK_CFLOAT;
                                tokc.f = strtof(token_buf, NULL);
                        } 另 如 (t == 'L') {
                                ch = *p++;
#如定义 TCC_TARGET_PE
                                tok = TOK_CDOUBLE;
                                tokc.d = strtod(token_buf, NULL);
#另
                                tok = TOK_CLDOUBLE;
                                tokc.ld = strtold(token_buf, NULL);
#了如
                        } 另 {
                                tok = TOK_CDOUBLE;
                                tokc.d = strtod(token_buf, NULL);
                        }
                }
        } 另 {
                无符 长 长 n, n1;
                整 lcount, ucount, must_64bit;
                不变 字 *p1;

                /* integer number */
                *q = '\0';
                q = token_buf;
                如 (b == 10 && *q == '0') {
                        b = 8;
                        q++;
                }
                n = 0;
                当(1) {
                        t = *q++;
                        /* no need for checks except for base 10 / 8 errors */
                        如 (t == '\0')
                                跳出;
                        另 如 (t >= 'a')
                                t = t - 'a' + 10;
                        另 如 (t >= 'A')
                                t = t - 'A' + 10;
                        另
                                t = t - '0';
                        如 (t >= b)
                                tcc_error("invalid digit");
                        n1 = n;
                        n = n * b + t;
                        /* detect overflow */
                        /* XXX: this test is not reliable */
                        如 (n < n1)
                                tcc_error("integer constant overflow");
                }

                /* Determine the characteristics (unsigned and/or 64bit) the type of
                   the constant must have according to the constant suffix(es) */
                lcount = ucount = must_64bit = 0;
                p1 = p;
                对于(;;) {
                        t = toup(ch);
                        如 (t == 'L') {
                                如 (lcount >= 2)
                                        tcc_error("three 'l's in integer constant");
                                如 (lcount && *(p - 1) != ch)
                                        tcc_error("incorrect integer suffix: %s", p1);
                                lcount++;
                                如 (lcount == 2)
                                        must_64bit = 1;
                                ch = *p++;
                        } 另 如 (t == 'U') {
                                如 (ucount >= 1)
                                        tcc_error("two 'u's in integer constant");
                                ucount++;
                                ch = *p++;
                        } 另 {
                                跳出;
                        }
                }

                /* Whether 64 bits are needed to hold the constant's value */
                如 (n & 0xffffffff00000000LL || must_64bit) {
                        tok = TOK_CLLONG;
                        n1 = n >> 32;
                } 另 如 (lcount) {
#如定义 TCC_LONG_ARE_64_BIT
                        n1 = n >> 32;
#另
                        n1 = n;
#了如
                        tok = TOK_CLONG;
                } 另 {
                        tok = TOK_CINT;
                        n1 = n;
                }

                /* Whether type must be unsigned to hold the constant's value */
                如 (ucount || ((n1 >> 31) && (b != 10))) {
                        如 (tok == TOK_CLLONG)
                                tok = TOK_CULLONG;
                        另 如 (tok == TOK_CLONG)
                                tok = TOK_CULONG;
                        另
                                tok = TOK_CUINT;
                /* If decimal and no unsigned suffix, bump to 64 bits or throw error */
                } 另 如 (n1 >> 31) {
                        如 (tok == TOK_CINT)
                                tok = TOK_CLLONG;
                        另
                                tcc_error("integer constant overflow");
                }

                tokc.i = n;
        }
        如 (ch)
                tcc_error("invalid number\n");
}


#定义 PARSE2(c1, tok1, c2, tok2)              \
        事例 c1:                                    \
                PEEKC(c, p);                            \
                如 (c == c2) {                          \
                        p++;                                \
                        tok = tok2;                         \
                } 另 {                                \
                        tok = tok1;                         \
                }                                       \
                跳出;

/* return next token without macro substitution */
静态 内联 空 next_nomacro1(空)
{
        整 t, c, is_long, len;
        TokenSym *ts;
        uint8_t *p, *p1;
        无符 整 h;

        p = file->buf_ptr;
 redo_no_start:
        c = *p;
        转接(c) {
        事例 ' ':
        事例 '\t':
                tok = c;
                p++;
                如 (parse_flags & PARSE_FLAG_SPACES)
                        跳转 keep_tok_flags;
                当 (isidnum_table[*p - CH_EOF] & IS_SPC)
                        ++p;
                跳转 redo_no_start;
        事例 '\f':
        事例 '\v':
        事例 '\r':
                p++;
                跳转 redo_no_start;
        事例 '\\':
                /* first look if it is in fact an end of buffer */
                c = handle_stray1(p);
                p = file->buf_ptr;
                如 (c == '\\')
                        跳转 parse_simple;
                如 (c != CH_EOF)
                        跳转 redo_no_start;
                {
                        TCCState *s1 = tcc_state;
                        如 ((parse_flags & PARSE_FLAG_LINEFEED)
                                && !(tok_flags & TOK_FLAG_EOF)) {
                                tok_flags |= TOK_FLAG_EOF;
                                tok = TOK_LINEFEED;
                                跳转 keep_tok_flags;
                        } 另 如 (!(parse_flags & PARSE_FLAG_PREPROCESS)) {
                                tok = TOK_EOF;
                        } 另 如 (s1->ifdef_stack_ptr != file->ifdef_stack_ptr) {
                                tcc_error("missing #endif");
                        } 另 如 (s1->include_stack_ptr == s1->include_stack) {
                                /* no include left : end of file. */
                                tok = TOK_EOF;
                        } 另 {
                                tok_flags &= ~TOK_FLAG_EOF;
                                /* pop include file */

                                /* test if previous '#endif' was after a #ifdef at
                                   start of file */
                                如 (tok_flags & TOK_FLAG_ENDIF) {
#如定义 INC_DEBUG
                                        printf("#endif %s\n", get_tok_str(file->ifndef_macro_saved, NULL));
#了如
                                        search_cached_include(s1, file->filename, 1)
                                                ->ifndef_macro = file->ifndef_macro_saved;
                                        tok_flags &= ~TOK_FLAG_ENDIF;
                                }

                                /* add end of include file debug info */
                                如 (tcc_state->do_debug) {
                                        put_stabd(N_EINCL, 0, 0);
                                }
                                /* pop include stack */
                                tcc_close();
                                s1->include_stack_ptr--;
                                p = file->buf_ptr;
                                跳转 redo_no_start;
                        }
                }
                跳出;

        事例 '\n':
                file->line_num++;
                tok_flags |= TOK_FLAG_BOL;
                p++;
maybe_newline:
                如 (0 == (parse_flags & PARSE_FLAG_LINEFEED))
                        跳转 redo_no_start;
                tok = TOK_LINEFEED;
                跳转 keep_tok_flags;

        事例 '#':
                /* XXX: simplify */
                PEEKC(c, p);
                如 ((tok_flags & TOK_FLAG_BOL) &&
                        (parse_flags & PARSE_FLAG_PREPROCESS)) {
                        file->buf_ptr = p;
                        preprocess(tok_flags & TOK_FLAG_BOF);
                        p = file->buf_ptr;
                        跳转 maybe_newline;
                } 另 {
                        如 (c == '#') {
                                p++;
                                tok = TOK_TWOSHARPS;
                        } 另 {
                                如 (parse_flags & PARSE_FLAG_ASM_FILE) {
                                        p = parse_line_comment(p - 1);
                                        跳转 redo_no_start;
                                } 另 {
                                        tok = '#';
                                }
                        }
                }
                跳出;

        /* dollar is allowed to start identifiers when not parsing asm */
        事例 '$':
                如 (!(isidnum_table[c - CH_EOF] & IS_ID)
                 || (parse_flags & PARSE_FLAG_ASM_FILE))
                        跳转 parse_simple;

        事例 'a': 事例 'b': 事例 'c': 事例 'd':
        事例 'e': 事例 'f': 事例 'g': 事例 'h':
        事例 'i': 事例 'j': 事例 'k': 事例 'l':
        事例 'm': 事例 'n': 事例 'o': 事例 'p':
        事例 'q': 事例 'r': 事例 's': 事例 't':
        事例 'u': 事例 'v': 事例 'w': 事例 'x':
        事例 'y': 事例 'z':
        事例 'A': 事例 'B': 事例 'C': 事例 'D':
        事例 'E': 事例 'F': 事例 'G': 事例 'H':
        事例 'I': 事例 'J': 事例 'K':
        事例 'M': 事例 'N': 事例 'O': 事例 'P':
        事例 'Q': 事例 'R': 事例 'S': 事例 'T':
        事例 'U': 事例 'V': 事例 'W': 事例 'X':
        事例 'Y': 事例 'Z':
        事例 '_':
        parse_ident_fast:
                p1 = p;
                h = TOK_HASH_INIT;
                h = TOK_HASH_FUNC(h, c);
                当 (c = *++p, isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
                        h = TOK_HASH_FUNC(h, c);
                len = p - p1;
                如 (c != '\\') {
                        TokenSym **pts;

                        /* fast case : no stray found, so we have the full token
                           and we have already hashed it */
                        h &= (TOK_HASH_SIZE - 1);
                        pts = &hash_ident[h];
                        对于(;;) {
                                ts = *pts;
                                如 (!ts)
                                        跳出;
                                如 (ts->len == len && !memcmp(ts->str, p1, len))
                                        跳转 token_found;
                                pts = &(ts->hash_next);
                        }
                        ts = tok_alloc_new(pts, (字 *) p1, len);
                token_found: ;
                } 另 {
                        /* slower case */
                        cstr_reset(&tokcstr);
                        cstr_cat(&tokcstr, (字 *) p1, len);
                        p--;
                        PEEKC(c, p);
                parse_ident_slow:
                        当 (isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
                        {
                                cstr_ccat(&tokcstr, c);
                                PEEKC(c, p);
                        }
                        ts = tok_alloc(tokcstr.data, tokcstr.size);
                }
                tok = ts->tok;
                跳出;
        事例 'L':
                t = p[1];
                如 (t != '\\' && t != '\'' && t != '\"') {
                        /* fast case */
                        跳转 parse_ident_fast;
                } 另 {
                        PEEKC(c, p);
                        如 (c == '\'' || c == '\"') {
                                is_long = 1;
                                跳转 str_const;
                        } 另 {
                                cstr_reset(&tokcstr);
                                cstr_ccat(&tokcstr, 'L');
                                跳转 parse_ident_slow;
                        }
                }
                跳出;

        事例 '0': 事例 '1': 事例 '2': 事例 '3':
        事例 '4': 事例 '5': 事例 '6': 事例 '7':
        事例 '8': 事例 '9':
                t = c;
                PEEKC(c, p);
                /* after the first digit, accept digits, alpha, '.' or sign if
                   prefixed by 'eEpP' */
        parse_num:
                cstr_reset(&tokcstr);
                对于(;;) {
                        cstr_ccat(&tokcstr, t);
                        如 (!((isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
                                  || c == '.'
                                  || ((c == '+' || c == '-')
                                          && (((t == 'e' || t == 'E')
                                                        && !(parse_flags & PARSE_FLAG_ASM_FILE
                                                                /* 0xe+1 is 3 tokens in asm */
                                                                && ((字*)tokcstr.data)[0] == '0'
                                                                && toup(((字*)tokcstr.data)[1]) == 'X'))
                                                  || t == 'p' || t == 'P'))))
                                跳出;
                        t = c;
                        PEEKC(c, p);
                }
                /* We add a trailing '\0' to ease parsing */
                cstr_ccat(&tokcstr, '\0');
                tokc.str.size = tokcstr.size;
                tokc.str.data = tokcstr.data;
                tok = TOK_PPNUM;
                跳出;

        事例 '.':
                /* special dot handling because it can also start a number */
                PEEKC(c, p);
                如 (isnum(c)) {
                        t = '.';
                        跳转 parse_num;
                } 另 如 ((isidnum_table['.' - CH_EOF] & IS_ID)
                                   && (isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))) {
                        *--p = c = '.';
                        跳转 parse_ident_fast;
                } 另 如 (c == '.') {
                        PEEKC(c, p);
                        如 (c == '.') {
                                p++;
                                tok = TOK_DOTS;
                        } 另 {
                                *--p = '.'; /* may underflow into file->unget[] */
                                tok = '.';
                        }
                } 另 {
                        tok = '.';
                }
                跳出;
        事例 '\'':
        事例 '\"':
                is_long = 0;
        str_const:
                cstr_reset(&tokcstr);
                如 (is_long)
                        cstr_ccat(&tokcstr, 'L');
                cstr_ccat(&tokcstr, c);
                p = parse_pp_string(p, c, &tokcstr);
                cstr_ccat(&tokcstr, c);
                cstr_ccat(&tokcstr, '\0');
                tokc.str.size = tokcstr.size;
                tokc.str.data = tokcstr.data;
                tok = TOK_PPSTR;
                跳出;

        事例 '<':
                PEEKC(c, p);
                如 (c == '=') {
                        p++;
                        tok = TOK_LE;
                } 另 如 (c == '<') {
                        PEEKC(c, p);
                        如 (c == '=') {
                                p++;
                                tok = TOK_A_SHL;
                        } 另 {
                                tok = TOK_SHL;
                        }
                } 另 {
                        tok = TOK_LT;
                }
                跳出;
        事例 '>':
                PEEKC(c, p);
                如 (c == '=') {
                        p++;
                        tok = TOK_GE;
                } 另 如 (c == '>') {
                        PEEKC(c, p);
                        如 (c == '=') {
                                p++;
                                tok = TOK_A_SAR;
                        } 另 {
                                tok = TOK_SAR;
                        }
                } 另 {
                        tok = TOK_GT;
                }
                跳出;

        事例 '&':
                PEEKC(c, p);
                如 (c == '&') {
                        p++;
                        tok = TOK_LAND;
                } 另 如 (c == '=') {
                        p++;
                        tok = TOK_A_AND;
                } 另 {
                        tok = '&';
                }
                跳出;

        事例 '|':
                PEEKC(c, p);
                如 (c == '|') {
                        p++;
                        tok = TOK_LOR;
                } 另 如 (c == '=') {
                        p++;
                        tok = TOK_A_OR;
                } 另 {
                        tok = '|';
                }
                跳出;

        事例 '+':
                PEEKC(c, p);
                如 (c == '+') {
                        p++;
                        tok = TOK_INC;
                } 另 如 (c == '=') {
                        p++;
                        tok = TOK_A_ADD;
                } 另 {
                        tok = '+';
                }
                跳出;

        事例 '-':
                PEEKC(c, p);
                如 (c == '-') {
                        p++;
                        tok = TOK_DEC;
                } 另 如 (c == '=') {
                        p++;
                        tok = TOK_A_SUB;
                } 另 如 (c == '>') {
                        p++;
                        tok = TOK_ARROW;
                } 另 {
                        tok = '-';
                }
                跳出;

        PARSE2('!', '!', '=', TOK_NE)
        PARSE2('=', '=', '=', TOK_EQ)
        PARSE2('*', '*', '=', TOK_A_MUL)
        PARSE2('%', '%', '=', TOK_A_MOD)
        PARSE2('^', '^', '=', TOK_A_XOR)

                /* comments or operator */
        事例 '/':
                PEEKC(c, p);
                如 (c == '*') {
                        p = parse_comment(p);
                        /* comments replaced by a blank */
                        tok = ' ';
                        跳转 keep_tok_flags;
                } 另 如 (c == '/') {
                        p = parse_line_comment(p);
                        tok = ' ';
                        跳转 keep_tok_flags;
                } 另 如 (c == '=') {
                        p++;
                        tok = TOK_A_DIV;
                } 另 {
                        tok = '/';
                }
                跳出;

                /* simple tokens */
        事例 '(':
        事例 ')':
        事例 '[':
        事例 ']':
        事例 '{':
        事例 '}':
        事例 ',':
        事例 ';':
        事例 ':':
        事例 '?':
        事例 '~':
        事例 '@': /* only used in assembler */
        parse_simple:
                tok = c;
                p++;
                跳出;
        缺省:
                如 (c >= 0x80 && c <= 0xFF) /* utf8 identifiers */
                        跳转 parse_ident_fast;
                如 (parse_flags & PARSE_FLAG_ASM_FILE)
                        跳转 parse_simple;
                tcc_error("unrecognized character \\x%02x", c);
                跳出;
        }
        tok_flags = 0;
keep_tok_flags:
        file->buf_ptr = p;
#如 已定义(PARSE_DEBUG)
        printf("token = %s\n", get_tok_str(tok, &tokc));
#了如
}

/* return next token without macro substitution. Can read input from
   macro_ptr buffer */
静态 空 next_nomacro_spc(空)
{
        如 (macro_ptr) {
        redo:
                tok = *macro_ptr;
                如 (tok) {
                        TOK_GET(&tok, &macro_ptr, &tokc);
                        如 (tok == TOK_LINENUM) {
                                file->line_num = tokc.i;
                                跳转 redo;
                        }
                }
        } 另 {
                next_nomacro1();
        }
        //printf("token = %s\n", get_tok_str(tok, &tokc));
}

ST_FUNC 空 next_nomacro(空)
{
        运行 {
                next_nomacro_spc();
        } 当 (tok < 256 && (isidnum_table[tok - CH_EOF] & IS_SPC));
}


静态 空 macro_subst(
        TokenString *tok_str,
        Sym **nested_list,
        不变 整 *macro_str
        );

/* substitute arguments in replacement lists in macro_str by the values in
   args (field d) and return allocated string */
静态 整 *macro_arg_subst(Sym **nested_list, 不变 整 *macro_str, Sym *args)
{
        整 t, t0, t1, spc;
        不变 整 *st;
        Sym *s;
        CValue cval;
        TokenString str;
        CString cstr;

        tok_str_new(&str);
        t0 = t1 = 0;
        当(1) {
                TOK_GET(&t, &macro_str, &cval);
                如 (!t)
                        跳出;
                如 (t == '#') {
                        /* stringize */
                        TOK_GET(&t, &macro_str, &cval);
                        如 (!t)
                                跳转 bad_stringy;
                        s = sym_find2(args, t);
                        如 (s) {
                                cstr_new(&cstr);
                                cstr_ccat(&cstr, '\"');
                                st = s->d;
                                spc = 0;
                                当 (*st >= 0) {
                                        TOK_GET(&t, &st, &cval);
                                        如 (t != TOK_PLCHLDR
                                         && t != TOK_NOSUBST
                                         && 0 == check_space(t, &spc)) {
                                                不变 字 *s = get_tok_str(t, &cval);
                                                当 (*s) {
                                                        如 (t == TOK_PPSTR && *s != '\'')
                                                                add_char(&cstr, *s);
                                                        另
                                                                cstr_ccat(&cstr, *s);
                                                        ++s;
                                                }
                                        }
                                }
                                cstr.size -= spc;
                                cstr_ccat(&cstr, '\"');
                                cstr_ccat(&cstr, '\0');
#如定义 PP_DEBUG
                                printf("\nstringize: <%s>\n", (字 *)cstr.data);
#了如
                                /* add string */
                                cval.str.size = cstr.size;
                                cval.str.data = cstr.data;
                                tok_str_add2(&str, TOK_PPSTR, &cval);
                                cstr_free(&cstr);
                        } 另 {
                bad_stringy:
                                expect("macro parameter after '#'");
                        }
                } 另 如 (t >= TOK_IDENT) {
                        s = sym_find2(args, t);
                        如 (s) {
                                整 l0 = str.len;
                                st = s->d;
                                /* if '##' is present before or after, no arg substitution */
                                如 (*macro_str == TOK_PPJOIN || t1 == TOK_PPJOIN) {
                                        /* special case for var arg macros : ## eats the ','
                                           if empty VA_ARGS variable. */
                                        如 (t1 == TOK_PPJOIN && t0 == ',' && gnu_ext && s->type.t) {
                                                如 (*st <= 0) {
                                                        /* suppress ',' '##' */
                                                        str.len -= 2;
                                                } 另 {
                                                        /* suppress '##' and add variable */
                                                        str.len--;
                                                        跳转 add_var;
                                                }
                                        }
                                } 另 {
                        add_var:
                                        如 (!s->next) {
                                                /* Expand arguments tokens and store them.  In most
                                                   cases we could also re-expand each argument if
                                                   used multiple times, but not if the argument
                                                   contains the __COUNTER__ macro.  */
                                                TokenString str2;
                                                sym_push2(&s->next, s->v, s->type.t, 0);
                                                tok_str_new(&str2);
                                                macro_subst(&str2, nested_list, st);
                                                tok_str_add(&str2, 0);
                                                s->next->d = str2.str;
                                        }
                                        st = s->next->d;
                                }
                                对于(;;) {
                                        整 t2;
                                        TOK_GET(&t2, &st, &cval);
                                        如 (t2 <= 0)
                                                跳出;
                                        tok_str_add2(&str, t2, &cval);
                                }
                                如 (str.len == l0) /* expanded to empty string */
                                        tok_str_add(&str, TOK_PLCHLDR);
                        } 另 {
                                tok_str_add(&str, t);
                        }
                } 另 {
                        tok_str_add2(&str, t, &cval);
                }
                t0 = t1, t1 = t;
        }
        tok_str_add(&str, 0);
        返回 str.str;
}

静态 字 不变 ab_month_name[12][4] =
{
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

静态 整 paste_tokens(整 t1, CValue *v1, 整 t2, CValue *v2)
{
        CString cstr;
        整 n, ret = 1;

        cstr_new(&cstr);
        如 (t1 != TOK_PLCHLDR)
                cstr_cat(&cstr, get_tok_str(t1, v1), -1);
        n = cstr.size;
        如 (t2 != TOK_PLCHLDR)
                cstr_cat(&cstr, get_tok_str(t2, v2), -1);
        cstr_ccat(&cstr, '\0');

        tcc_open_bf(tcc_state, ":paste:", cstr.size);
        memcpy(file->buffer, cstr.data, cstr.size);
        tok_flags = 0;
        对于 (;;) {
                next_nomacro1();
                如 (0 == *file->buf_ptr)
                        跳出;
                如 (is_space(tok))
                        继续;
                tcc_warning("pasting \"%.*s\" and \"%s\" does not give a valid"
                        " preprocessing token", n, cstr.data, (字*)cstr.data + n);
                ret = 0;
                跳出;
        }
        tcc_close();
        //printf("paste <%s>\n", (char*)cstr.data);
        cstr_free(&cstr);
        返回 ret;
}

/* handle the '##' operator. Return NULL if no '##' seen. Otherwise
   return the resulting string (which must be freed). */
静态 内联 整 *macro_twosharps(不变 整 *ptr0)
{
        整 t;
        CValue cval;
        TokenString macro_str1;
        整 start_of_nosubsts = -1;
        不变 整 *ptr;

        /* we search the first '##' */
        对于 (ptr = ptr0;;) {
                TOK_GET(&t, &ptr, &cval);
                如 (t == TOK_PPJOIN)
                        跳出;
                如 (t == 0)
                        返回 NULL;
        }

        tok_str_new(&macro_str1);

        //tok_print(" $$$", ptr0);
        对于 (ptr = ptr0;;) {
                TOK_GET(&t, &ptr, &cval);
                如 (t == 0)
                        跳出;
                如 (t == TOK_PPJOIN)
                        继续;
                当 (*ptr == TOK_PPJOIN) {
                        整 t1; CValue cv1;
                        /* given 'a##b', remove nosubsts preceding 'a' */
                        如 (start_of_nosubsts >= 0)
                                macro_str1.len = start_of_nosubsts;
                        /* given 'a##b', remove nosubsts preceding 'b' */
                        当 ((t1 = *++ptr) == TOK_NOSUBST)
                                ;
                        如 (t1 && t1 != TOK_PPJOIN) {
                                TOK_GET(&t1, &ptr, &cv1);
                                如 (t != TOK_PLCHLDR || t1 != TOK_PLCHLDR) {
                                        如 (paste_tokens(t, &cval, t1, &cv1)) {
                                                t = tok, cval = tokc;
                                        } 另 {
                                                tok_str_add2(&macro_str1, t, &cval);
                                                t = t1, cval = cv1;
                                        }
                                }
                        }
                }
                如 (t == TOK_NOSUBST) {
                        如 (start_of_nosubsts < 0)
                                start_of_nosubsts = macro_str1.len;
                } 另 {
                        start_of_nosubsts = -1;
                }
                tok_str_add2(&macro_str1, t, &cval);
        }
        tok_str_add(&macro_str1, 0);
        //tok_print(" ###", macro_str1.str);
        返回 macro_str1.str;
}

/* peek or read [ws_str == NULL] next token from function macro call,
   walking up macro levels up to the file if necessary */
静态 整 next_argstream(Sym **nested_list, TokenString *ws_str)
{
        整 t;
        不变 整 *p;
        Sym *sa;

        对于 (;;) {
                如 (macro_ptr) {
                        p = macro_ptr, t = *p;
                        如 (ws_str) {
                                当 (is_space(t) || TOK_LINEFEED == t || TOK_PLCHLDR == t)
                                        tok_str_add(ws_str, t), t = *++p;
                        }
                        如 (t == 0) {
                                end_macro();
                                /* also, end of scope for nested defined symbol */
                                sa = *nested_list;
                                当 (sa && sa->v == 0)
                                        sa = sa->prev;
                                如 (sa)
                                        sa->v = 0;
                                继续;
                        }
                } 另 {
                        ch = handle_eob();
                        如 (ws_str) {
                                当 (is_space(ch) || ch == '\n' || ch == '/') {
                                        如 (ch == '/') {
                                                整 c;
                                                uint8_t *p = file->buf_ptr;
                                                PEEKC(c, p);
                                                如 (c == '*') {
                                                        p = parse_comment(p);
                                                        file->buf_ptr = p - 1;
                                                } 另 如 (c == '/') {
                                                        p = parse_line_comment(p);
                                                        file->buf_ptr = p - 1;
                                                } 另
                                                        跳出;
                                                ch = ' ';
                                        }
                                        如 (ch == '\n')
                                                file->line_num++;
                                        如 (!(ch == '\f' || ch == '\v' || ch == '\r'))
                                                tok_str_add(ws_str, ch);
                                        cinp();
                                }
                        }
                        t = ch;
                }

                如 (ws_str)
                        返回 t;
                next_nomacro_spc();
                返回 tok;
        }
}

/* do macro substitution of current token with macro 's' and add
   result to (tok_str,tok_len). 'nested_list' is the list of all
   macros we got inside to avoid recursing. Return non zero if no
   substitution needs to be done */
静态 整 macro_subst_tok(
        TokenString *tok_str,
        Sym **nested_list,
        Sym *s)
{
        Sym *args, *sa, *sa1;
        整 parlevel, t, t1, spc;
        TokenString str;
        字 *cstrval;
        CValue cval;
        CString cstr;
        字 buf[32];

        /* if symbol is a macro, prepare substitution */
        /* special macros */
        如 (tok == TOK___LINE__ || tok == TOK___LINE___CN || tok == TOK___COUNTER__ || tok == TOK___COUNTER___CN) {
                t = (tok == TOK___LINE__ || tok == TOK___LINE___CN) ? file->line_num : pp_counter++;
                snprintf(buf, 求长度(buf), "%d", t);
                cstrval = buf;
                t1 = TOK_PPNUM;
                跳转 add_cstr1;
        } 另 如 (tok == TOK___FILE__ || tok == TOK___FILE___CN) {
                cstrval = file->filename;
                跳转 add_cstr;
        } 另 如 (tok == TOK___DATE__ || tok == TOK___DATE___CN || tok == TOK___TIME__ || tok == TOK___TIME___CN) {
                time_t ti;
                结构 tm *tm;

                time(&ti);
                tm = localtime(&ti);
                如 (tok == TOK___DATE__ || tok == TOK___DATE___CN) {
                        snprintf(buf, 求长度(buf), "%s %2d %d",
                                         ab_month_name[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
                } 另 {
                        snprintf(buf, 求长度(buf), "%02d:%02d:%02d",
                                         tm->tm_hour, tm->tm_min, tm->tm_sec);
                }
                cstrval = buf;
        add_cstr:
                t1 = TOK_STR;
        add_cstr1:
                cstr_new(&cstr);
                cstr_cat(&cstr, cstrval, 0);
                cval.str.size = cstr.size;
                cval.str.data = cstr.data;
                tok_str_add2(tok_str, t1, &cval);
                cstr_free(&cstr);
        } 另 如 (s->d) {
                整 saved_parse_flags = parse_flags;
                整 *joined_str = NULL;
                整 *mstr = s->d;

                如 (s->type.t == MACRO_FUNC) {
                        /* whitespace between macro name and argument list */
                        TokenString ws_str;
                        tok_str_new(&ws_str);

                        spc = 0;
                        parse_flags |= PARSE_FLAG_SPACES | PARSE_FLAG_LINEFEED
                                | PARSE_FLAG_ACCEPT_STRAYS;

                        /* get next token from argument stream */
                        t = next_argstream(nested_list, &ws_str);
                        如 (t != '(') {
                                /* not a macro substitution after all, restore the
                                 * macro token plus all whitespace we've read.
                                 * whitespace is intentionally not merged to preserve
                                 * newlines. */
                                parse_flags = saved_parse_flags;
                                tok_str_add(tok_str, tok);
                                如 (parse_flags & PARSE_FLAG_SPACES) {
                                        整 i;
                                        对于 (i = 0; i < ws_str.len; i++)
                                                tok_str_add(tok_str, ws_str.str[i]);
                                }
                                tok_str_free_str(ws_str.str);
                                返回 0;
                        } 另 {
                                tok_str_free_str(ws_str.str);
                        }
                        运行 {
                                next_nomacro(); /* eat '(' */
                        } 当 (tok == TOK_PLCHLDR);

                        /* argument macro */
                        args = NULL;
                        sa = s->next;
                        /* NOTE: empty args are allowed, except if no args */
                        对于(;;) {
                                运行 {
                                        next_argstream(nested_list, NULL);
                                } 当 (is_space(tok) || TOK_LINEFEED == tok);
        empty_arg:
                                /* handle '()' case */
                                如 (!args && !sa && tok == ')')
                                        跳出;
                                如 (!sa)
                                        tcc_error("macro '%s' used with too many args",
                                                  get_tok_str(s->v, 0));
                                tok_str_new(&str);
                                parlevel = spc = 0;
                                /* NOTE: non zero sa->t indicates VA_ARGS */
                                当 ((parlevel > 0 ||
                                                (tok != ')' &&
                                                 (tok != ',' || sa->type.t)))) {
                                        如 (tok == TOK_EOF || tok == 0)
                                                跳出;
                                        如 (tok == '(')
                                                parlevel++;
                                        另 如 (tok == ')')
                                                parlevel--;
                                        如 (tok == TOK_LINEFEED)
                                                tok = ' ';
                                        如 (!check_space(tok, &spc))
                                                tok_str_add2(&str, tok, &tokc);
                                        next_argstream(nested_list, NULL);
                                }
                                如 (parlevel)
                                        expect(")");
                                str.len -= spc;
                                tok_str_add(&str, -1);
                                tok_str_add(&str, 0);
                                sa1 = sym_push2(&args, sa->v & ~SYM_FIELD, sa->type.t, 0);
                                sa1->d = str.str;
                                sa = sa->next;
                                如 (tok == ')') {
                                        /* special case for gcc var args: add an empty
                                           var arg argument if it is omitted */
                                        如 (sa && sa->type.t && gnu_ext)
                                                跳转 empty_arg;
                                        跳出;
                                }
                                如 (tok != ',')
                                        expect(",");
                        }
                        如 (sa) {
                                tcc_error("macro '%s' used with too few args",
                                          get_tok_str(s->v, 0));
                        }

                        parse_flags = saved_parse_flags;

                        /* now subst each arg */
                        mstr = macro_arg_subst(nested_list, mstr, args);
                        /* free memory */
                        sa = args;
                        当 (sa) {
                                sa1 = sa->prev;
                                tok_str_free_str(sa->d);
                                如 (sa->next) {
                                        tok_str_free_str(sa->next->d);
                                        sym_free(sa->next);
                                }
                                sym_free(sa);
                                sa = sa1;
                        }
                }

                sym_push2(nested_list, s->v, 0, 0);
                parse_flags = saved_parse_flags;
                joined_str = macro_twosharps(mstr);
                macro_subst(tok_str, nested_list, joined_str ? joined_str : mstr);

                /* pop nested defined symbol */
                sa1 = *nested_list;
                *nested_list = sa1->prev;
                sym_free(sa1);
                如 (joined_str)
                        tok_str_free_str(joined_str);
                如 (mstr != s->d)
                        tok_str_free_str(mstr);
        }
        返回 0;
}

/* do macro substitution of macro_str and add result to
   (tok_str,tok_len). 'nested_list' is the list of all macros we got
   inside to avoid recursing. */
静态 空 macro_subst(
        TokenString *tok_str,
        Sym **nested_list,
        不变 整 *macro_str
        )
{
        Sym *s;
        整 t, spc, nosubst;
        CValue cval;

        spc = nosubst = 0;

        当 (1) {
                TOK_GET(&t, &macro_str, &cval);
                如 (t <= 0)
                        跳出;

                如 (t >= TOK_IDENT && 0 == nosubst) {
                        s = define_find(t);
                        如 (s == NULL)
                                跳转 no_subst;

                        /* if nested substitution, do nothing */
                        如 (sym_find2(*nested_list, t)) {
                                /* and mark it as TOK_NOSUBST, so it doesn't get subst'd again */
                                tok_str_add2(tok_str, TOK_NOSUBST, NULL);
                                跳转 no_subst;
                        }

                        {
                                TokenString str;
                                str.str = (整*)macro_str;
                                begin_macro(&str, 2);

                                tok = t;
                                macro_subst_tok(tok_str, nested_list, s);

                                如 (str.alloc == 3) {
                                        /* already finished by reading function macro arguments */
                                        跳出;
                                }

                                macro_str = macro_ptr;
                                end_macro ();
                        }
                        如 (tok_str->len)
                                spc = is_space(t = tok_str->str[tok_str->lastlen]);
                } 另 {
                        如 (t == '\\' && !(parse_flags & PARSE_FLAG_ACCEPT_STRAYS))
                                tcc_error("stray '\\' in program");
no_subst:
                        如 (!check_space(t, &spc))
                                tok_str_add2(tok_str, t, &cval);

                        如 (nosubst) {
                                如 (nosubst > 1 && (spc || (++nosubst == 3 && t == '(')))
                                        继续;
                                nosubst = 0;
                        }
                        如 (t == TOK_NOSUBST)
                                nosubst = 1;
                }
                /* GCC supports 'defined' as result of a macto substitution */
                如 ((t == TOK_DEFINED || t == TOK_DEFINED_CN) && pp_expr)
                        nosubst = 2;
        }
}

/* return next token with macro substitution */
ST_FUNC 空 next(空)
{
 redo:
        如 (parse_flags & PARSE_FLAG_SPACES)
                next_nomacro_spc();
        另
                next_nomacro();

        如 (macro_ptr) {
                如 (tok == TOK_NOSUBST || tok == TOK_PLCHLDR) {
                /* discard preprocessor markers */
                        跳转 redo;
                } 另 如 (tok == 0) {
                        /* end of macro or unget token string */
                        end_macro();
                        跳转 redo;
                }
        } 另 如 (tok >= TOK_IDENT && (parse_flags & PARSE_FLAG_PREPROCESS)) {
                Sym *s;
                /* if reading from file, try to substitute macros */
                s = define_find(tok);
                如 (s) {
                        Sym *nested_list = NULL;
                        tokstr_buf.len = 0;
                        macro_subst_tok(&tokstr_buf, &nested_list, s);
                        tok_str_add(&tokstr_buf, 0);
                        begin_macro(&tokstr_buf, 2);
                        跳转 redo;
                }
        }
        /* convert preprocessor tokens into C tokens */
        如 (tok == TOK_PPNUM) {
                如  (parse_flags & PARSE_FLAG_TOK_NUM)
                        parse_number((字 *)tokc.str.data);
        } 另 如 (tok == TOK_PPSTR) {
                如 (parse_flags & PARSE_FLAG_TOK_STR)
                        parse_string((字 *)tokc.str.data, tokc.str.size - 1);
        }
}

/* push back current token and set current token to 'last_tok'. Only
   identifier case handled for labels. */
ST_INLN 空 unget_tok(整 last_tok)
{

        TokenString *str = tok_str_alloc();
        tok_str_add2(str, tok, &tokc);
        tok_str_add(str, 0);
        begin_macro(str, 1);
        tok = last_tok;
}

ST_FUNC 空 preprocess_start(TCCState *s1, 整 is_asm)
{
        字 *buf;

        s1->include_stack_ptr = s1->include_stack;
        s1->ifdef_stack_ptr = s1->ifdef_stack;
        file->ifdef_stack_ptr = s1->ifdef_stack_ptr;
        pp_expr = 0;
        pp_counter = 0;
        pp_debug_tok = pp_debug_symv = 0;
        pp_once++;
        pvtop = vtop = vstack - 1;
        s1->pack_stack[0] = 0;
        s1->pack_stack_ptr = s1->pack_stack;

        set_idnum('$', s1->dollars_in_identifiers ? IS_ID : 0);
        set_idnum('.', is_asm ? IS_ID : 0);

        buf = tcc_malloc(3 + strlen(file->filename));
        sprintf(buf, "\"%s\"", file->filename);
        tcc_define_symbol(s1, "__BASE_FILE__", buf);
        tcc_free(buf);

        如 (s1->nb_cmd_include_files) {
                CString cstr;
                整 i;
                cstr_new(&cstr);
                对于 (i = 0; i < s1->nb_cmd_include_files; i++) {
                        cstr_cat(&cstr, "#include \"", -1);
                        cstr_cat(&cstr, s1->cmd_include_files[i], -1);
                        cstr_cat(&cstr, "\"\n", -1);
                }
                *s1->include_stack_ptr++ = file;
                tcc_open_bf(s1, "<command line>", cstr.size);
                memcpy(file->buffer, cstr.data, cstr.size);
                cstr_free(&cstr);
        }

        如 (is_asm)
                tcc_define_symbol(s1, "__ASSEMBLER__", NULL);

        parse_flags = is_asm ? PARSE_FLAG_ASM_FILE : 0;
        tok_flags = TOK_FLAG_BOL | TOK_FLAG_BOF;
}

/* cleanup from error/setjmp */
ST_FUNC 空 preprocess_end(TCCState *s1)
{
        当 (macro_stack)
                end_macro();
        macro_ptr = NULL;
}

ST_FUNC 空 tccpp_new(TCCState *s)
{
        整 i, c;
        不变 字 *p, *r;

        /* might be used in error() before preprocess_start() */
        s->include_stack_ptr = s->include_stack;
        s->ppfp = stdout;

        /* init isid table */
        对于(i = CH_EOF; i<128; i++)
                set_idnum(i,
                        is_space(i) ? IS_SPC
                        : isid(i) ? IS_ID
                        : isnum(i) ? IS_NUM
                        : 0);

        对于(i = 128; i<256; i++)
                set_idnum(i, IS_ID);

        /* init allocators */
        tal_new(&toksym_alloc, TOKSYM_TAL_LIMIT, TOKSYM_TAL_SIZE);
        tal_new(&tokstr_alloc, TOKSTR_TAL_LIMIT, TOKSTR_TAL_SIZE);
        tal_new(&cstr_alloc, CSTR_TAL_LIMIT, CSTR_TAL_SIZE);

        memset(hash_ident, 0, TOK_HASH_SIZE * 求长度(TokenSym *));
        cstr_new(&cstr_buf);
        cstr_realloc(&cstr_buf, STRING_MAX_SIZE);
        tok_str_new(&tokstr_buf);
        tok_str_realloc(&tokstr_buf, TOKSTR_MAX_SIZE);

        tok_ident = TOK_IDENT;
        p = tcc_keywords;
        当 (*p) {
                r = p;
                对于(;;) {
                        c = *r++;
                        如 (c == '\0')
                                跳出;
                }
                tok_alloc(p, r - p - 1);
                p = r;
        }
}

ST_FUNC 空 tccpp_delete(TCCState *s)
{
        整 i, n;

        /* free -D and compiler defines */
        free_defines(NULL);

        /* free tokens */
        n = tok_ident - TOK_IDENT;
        对于(i = 0; i < n; i++)
                tal_free(toksym_alloc, table_ident[i]);
        tcc_free(table_ident);
        table_ident = NULL;

        /* free static buffers */
        cstr_free(&tokcstr);
        cstr_free(&cstr_buf);
        cstr_free(&macro_equal_buf);
        tok_str_free_str(tokstr_buf.str);

        /* free allocators */
        tal_delete(toksym_alloc);
        toksym_alloc = NULL;
        tal_delete(tokstr_alloc);
        tokstr_alloc = NULL;
        tal_delete(cstr_alloc);
        cstr_alloc = NULL;
}

/* ------------------------------------------------------------------------- */
/* tcc -E [-P[1]] [-dD} support */

静态 空 tok_print(不变 字 *msg, 不变 整 *str)
{
        FILE *fp;
        整 t, s = 0;
        CValue cval;

        fp = tcc_state->ppfp;
        fprintf(fp, "%s", msg);
        当 (str) {
                TOK_GET(&t, &str, &cval);
                如 (!t)
                        跳出;
                fprintf(fp, " %s" + s, get_tok_str(t, &cval)), s = 1;
        }
        fprintf(fp, "\n");
}

静态 空 pp_line(TCCState *s1, BufferedFile *f, 整 level)
{
        整 d = f->line_num - f->line_ref;

        如 (s1->dflag & 4)
                返回;

        如 (s1->Pflag == LINE_MACRO_OUTPUT_FORMAT_NONE) {
                ;
        } 另 如 (level == 0 && f->line_ref && d < 8) {
                当 (d > 0)
                        fputs("\n", s1->ppfp), --d;
        } 另 如 (s1->Pflag == LINE_MACRO_OUTPUT_FORMAT_STD) {
                fprintf(s1->ppfp, "#line %d \"%s\"\n", f->line_num, f->filename);
        } 另 {
                fprintf(s1->ppfp, "# %d \"%s\"%s\n", f->line_num, f->filename,
                        level > 0 ? " 1" : level < 0 ? " 2" : "");
        }
        f->line_ref = f->line_num;
}

静态 空 define_print(TCCState *s1, 整 v)
{
        FILE *fp;
        Sym *s;

        s = define_find(v);
        如 (NULL == s || NULL == s->d)
                返回;

        fp = s1->ppfp;
        fprintf(fp, "#define %s", get_tok_str(v, NULL));
        如 (s->type.t == MACRO_FUNC) {
                Sym *a = s->next;
                fprintf(fp,"(");
                如 (a)
                        对于 (;;) {
                                fprintf(fp,"%s", get_tok_str(a->v & ~SYM_FIELD, NULL));
                                如 (!(a = a->next))
                                        跳出;
                                fprintf(fp,",");
                        }
                fprintf(fp,")");
        }
        tok_print("", s->d);
}

静态 空 pp_debug_defines(TCCState *s1)
{
        整 v, t;
        不变 字 *vs;
        FILE *fp;

        t = pp_debug_tok;
        如 (t == 0)
                返回;

        file->line_num--;
        pp_line(s1, file, 0);
        file->line_ref = ++file->line_num;

        fp = s1->ppfp;
        v = pp_debug_symv;
        vs = get_tok_str(v, NULL);
        如 (t == TOK_DEFINE || t == TOK_DEFINE_CN) {
                define_print(s1, v);
        } 另 如 (t == TOK_UNDEF || t == TOK_UNDEF_CN) {
                fprintf(fp, "#undef %s\n", vs);
        } 另 如 (t == TOK_push_macro) {
                fprintf(fp, "#pragma push_macro(\"%s\")\n", vs);
        } 另 如 (t == TOK_pop_macro) {
                fprintf(fp, "#pragma pop_macro(\"%s\")\n", vs);
        }
        pp_debug_tok = 0;
}

静态 空 pp_debug_builtins(TCCState *s1)
{
        整 v;
        对于 (v = TOK_IDENT; v < tok_ident; ++v)
                define_print(s1, v);
}

/* Add a space between tokens a and b to avoid unwanted textual pasting */
静态 整 pp_need_space(整 a, 整 b)
{
        返回 'E' == a ? '+' == b || '-' == b
                : '+' == a ? TOK_INC == b || '+' == b
                : '-' == a ? TOK_DEC == b || '-' == b
                : a >= TOK_IDENT ? b >= TOK_IDENT
                : a == TOK_PPNUM ? b >= TOK_IDENT
                : 0;
}

/* maybe hex like 0x1e */
静态 整 pp_check_he0xE(整 t, 不变 字 *p)
{
        如 (t == TOK_PPNUM && toup(strchr(p, 0)[-1]) == 'E')
                返回 'E';
        返回 t;
}

/* Preprocess the current file */
ST_FUNC 整 tcc_preprocess(TCCState *s1)
{
        BufferedFile **iptr;
        整 token_seen, spcs, level;
        不变 字 *p;
        字 white[400];

        parse_flags = PARSE_FLAG_PREPROCESS
                                | (parse_flags & PARSE_FLAG_ASM_FILE)
                                | PARSE_FLAG_LINEFEED
                                | PARSE_FLAG_SPACES
                                | PARSE_FLAG_ACCEPT_STRAYS
                                ;
        /* Credits to Fabrice Bellard's initial revision to demonstrate its
           capability to compile and run itself, provided all numbers are
           given as decimals. tcc -E -P10 will do. */
        如 (s1->Pflag == LINE_MACRO_OUTPUT_FORMAT_P10)
                parse_flags |= PARSE_FLAG_TOK_NUM, s1->Pflag = 1;

#如定义 PP_BENCH
        /* for PP benchmarks */
        运行 next(); 当 (tok != TOK_EOF);
        返回 0;
#了如

        如 (s1->dflag & 1) {
                pp_debug_builtins(s1);
                s1->dflag &= ~1;
        }

        token_seen = TOK_LINEFEED, spcs = 0;
        pp_line(s1, file, 0);
        对于 (;;) {
                iptr = s1->include_stack_ptr;
                next();
                如 (tok == TOK_EOF)
                        跳出;

                level = s1->include_stack_ptr - iptr;
                如 (level) {
                        如 (level > 0)
                                pp_line(s1, *iptr, 0);
                        pp_line(s1, file, level);
                }
                如 (s1->dflag & 7) {
                        pp_debug_defines(s1);
                        如 (s1->dflag & 4)
                                继续;
                }

                如 (is_space(tok)) {
                        如 (spcs < 求长度 white - 1)
                                white[spcs++] = tok;
                        继续;
                } 另 如 (tok == TOK_LINEFEED) {
                        spcs = 0;
                        如 (token_seen == TOK_LINEFEED)
                                继续;
                        ++file->line_ref;
                } 另 如 (token_seen == TOK_LINEFEED) {
                        pp_line(s1, file, 0);
                } 另 如 (spcs == 0 && pp_need_space(token_seen, tok)) {
                        white[spcs++] = ' ';
                }

                white[spcs] = 0, fputs(white, s1->ppfp), spcs = 0;
                fputs(p = get_tok_str(tok, &tokc), s1->ppfp);
                token_seen = pp_check_he0xE(tok, p);
        }
        返回 0;
}

/* ------------------------------------------------------------------------- */
