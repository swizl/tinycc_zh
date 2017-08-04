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

/* use GNU C extensions */
ST_DATA 整 gnu_ext = 1;

/* use TinyCC extensions */
ST_DATA 整 tcc_ext = 1;

/* XXX: get rid of this ASAP */
ST_DATA 结构 TCCState *tcc_state;

静态 整 nb_states;

/********************************************************/

#如 ONE_SOURCE
#包含 "tccpp.c"
#包含 "tccgen.c"
#包含 "tccelf.c"
#包含 "tccrun.c"
#如定义 TCC_TARGET_I386
#包含 "i386-gen.c"
#包含 "i386-link.c"
#包含 "i386-asm.c"
#了如
#如定义 TCC_TARGET_ARM
#包含 "arm-gen.c"
#包含 "arm-link.c"
#包含 "arm-asm.c"
#了如
#如定义 TCC_TARGET_ARM64
#包含 "arm64-gen.c"
#包含 "arm64-link.c"
#了如
#如定义 TCC_TARGET_C67
#包含 "c67-gen.c"
#包含 "c67-link.c"
#包含 "tcccoff.c"
#了如
#如定义 TCC_TARGET_X86_64
#包含 "x86_64-gen.c"
#包含 "x86_64-link.c"
#包含 "i386-asm.c"
#了如
#如定义 CONFIG_TCC_ASM
#包含 "tccasm.c"
#了如
#如定义 TCC_TARGET_PE
#包含 "tccpe.c"
#了如
#了如 /* ONE_SOURCE */

/********************************************************/
#如未定义 CONFIG_TCC_ASM
ST_FUNC 空 asm_instr(空)
{
    tcc_error("inline asm() not supported");
}
ST_FUNC 空 asm_global_instr(空)
{
    tcc_error("inline asm() not supported");
}
#了如

/********************************************************/
#如定义 _WIN32
ST_FUNC 字 *normalize_slashes(字 *path)
{
    字 *p;
    对于 (p = path; *p; ++p)
        如 (*p == '\\')
            *p = '/';
    返回 path;
}

静态 HMODULE tcc_module;

/* on win32, we suppose the lib and includes are at the location of 'tcc.exe' */
静态 空 tcc_set_lib_path_w32(TCCState *s)
{
    字 path[1024], *p;
    GetModuleFileNameA(tcc_module, path, 求长度 path);
    p = tcc_basename(normalize_slashes(strlwr(path)));
    如 (p - 5 > path && 0 == strncmp(p - 5, "/bin/", 5))
        p -= 5;
    另 如 (p > path)
        p--;
    *p = 0;
    tcc_set_lib_path(s, path);
}

#如定义 TCC_TARGET_PE
静态 空 tcc_add_systemdir(TCCState *s)
{
    字 buf[1000];
    GetSystemDirectory(buf, 求长度 buf);
    tcc_add_library_path(s, normalize_slashes(buf));
}
#了如

#如定义 LIBTCC_AS_DLL
BOOL WINAPI DllMain (HINSTANCE hDll, DWORD dwReason, LPVOID lpReserved)
{
    如 (DLL_PROCESS_ATTACH == dwReason)
        tcc_module = hDll;
    返回 TRUE;
}
#了如
#了如

/********************************************************/
/* copy a string and truncate it. */
ST_FUNC 字 *pstrcpy(字 *buf, 整 buf_size, 不变 字 *s)
{
    字 *q, *q_end;
    整 c;

    如 (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        当 (q < q_end) {
            c = *s++;
            如 (c == '\0')
                跳出;
            *q++ = c;
        }
        *q = '\0';
    }
    返回 buf;
}

/* strcat and truncate. */
ST_FUNC 字 *pstrcat(字 *buf, 整 buf_size, 不变 字 *s)
{
    整 len;
    len = strlen(buf);
    如 (len < buf_size)
        pstrcpy(buf + len, buf_size - len, s);
    返回 buf;
}

ST_FUNC 字 *pstrncpy(字 *out, 不变 字 *in, size_t num)
{
    memcpy(out, in, num);
    out[num] = '\0';
    返回 out;
}

/* extract the basename of a file */
PUB_FUNC 字 *tcc_basename(不变 字 *name)
{
    字 *p = strchr(name, 0);
    当 (p > name && !IS_DIRSEP(p[-1]))
        --p;
    返回 p;
}

/* extract extension part of a file
 *
 * (if no extension, return pointer to end-of-string)
 */
PUB_FUNC 字 *tcc_fileextension (不变 字 *name)
{
    字 *b = tcc_basename(name);
    字 *e = strrchr(b, '.');
    返回 e ? e : strchr(b, 0);
}

/********************************************************/
/* memory management */

#消定义 free
#消定义 malloc
#消定义 realloc

#如未定义 MEM_DEBUG

PUB_FUNC 空 tcc_free(空 *ptr)
{
    free(ptr);
}

PUB_FUNC 空 *tcc_malloc(无符 长 size)
{
    空 *ptr;
    ptr = malloc(size);
    如 (!ptr && size)
        tcc_error("memory full (malloc)");
    返回 ptr;
}

PUB_FUNC 空 *tcc_mallocz(无符 长 size)
{
    空 *ptr;
    ptr = tcc_malloc(size);
    memset(ptr, 0, size);
    返回 ptr;
}

PUB_FUNC 空 *tcc_realloc(空 *ptr, 无符 长 size)
{
    空 *ptr1;
    ptr1 = realloc(ptr, size);
    如 (!ptr1 && size)
        tcc_error("memory full (realloc)");
    返回 ptr1;
}

PUB_FUNC 字 *tcc_strdup(不变 字 *str)
{
    字 *ptr;
    ptr = tcc_malloc(strlen(str) + 1);
    strcpy(ptr, str);
    返回 ptr;
}

PUB_FUNC 空 tcc_memcheck(空)
{
}

#另

#定义 MEM_DEBUG_MAGIC1 0xFEEDDEB1
#定义 MEM_DEBUG_MAGIC2 0xFEEDDEB2
#定义 MEM_DEBUG_MAGIC3 0xFEEDDEB3
#定义 MEM_DEBUG_FILE_LEN 40
#定义 MEM_DEBUG_CHECK3(header) \
    ((mem_debug_header_t*)((字*)header + header->size))->magic3
#定义 MEM_USER_PTR(header) \
    ((字 *)header + offsetof(mem_debug_header_t, magic3))
#定义 MEM_HEADER_PTR(ptr) \
    (mem_debug_header_t *)((字*)ptr - offsetof(mem_debug_header_t, magic3))

结构 mem_debug_header {
    无符 magic1;
    无符 size;
    结构 mem_debug_header *prev;
    结构 mem_debug_header *next;
    整 line_num;
    字 file_name[MEM_DEBUG_FILE_LEN + 1];
    无符 magic2;
    ALIGNED(16) 无符 magic3;
};

类型定义 结构 mem_debug_header mem_debug_header_t;

静态 mem_debug_header_t *mem_debug_chain;
静态 无符 mem_cur_size;
静态 无符 mem_max_size;

静态 mem_debug_header_t *malloc_check(空 *ptr, 不变 字 *msg)
{
    mem_debug_header_t * header = MEM_HEADER_PTR(ptr);
    如 (header->magic1 != MEM_DEBUG_MAGIC1 ||
        header->magic2 != MEM_DEBUG_MAGIC2 ||
        MEM_DEBUG_CHECK3(header) != MEM_DEBUG_MAGIC3 ||
        header->size == (无符)-1) {
        fprintf(stderr, "%s check failed\n", msg);
        如 (header->magic1 == MEM_DEBUG_MAGIC1)
            fprintf(stderr, "%s:%u: block allocated here.\n",
                header->file_name, header->line_num);
        exit(1);
    }
    返回 header;
}

PUB_FUNC 空 *tcc_malloc_debug(无符 长 size, 不变 字 *file, 整 line)
{
    整 ofs;
    mem_debug_header_t *header;

    header = malloc(求长度(mem_debug_header_t) + size);
    如 (!header)
        tcc_error("memory full (malloc)");

    header->magic1 = MEM_DEBUG_MAGIC1;
    header->magic2 = MEM_DEBUG_MAGIC2;
    header->size = size;
    MEM_DEBUG_CHECK3(header) = MEM_DEBUG_MAGIC3;
    header->line_num = line;
    ofs = strlen(file) - MEM_DEBUG_FILE_LEN;
    strncpy(header->file_name, file + (ofs > 0 ? ofs : 0), MEM_DEBUG_FILE_LEN);
    header->file_name[MEM_DEBUG_FILE_LEN] = 0;

    header->next = mem_debug_chain;
    header->prev = NULL;
    如 (header->next)
        header->next->prev = header;
    mem_debug_chain = header;

    mem_cur_size += size;
    如 (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;

    返回 MEM_USER_PTR(header);
}

PUB_FUNC 空 tcc_free_debug(空 *ptr)
{
    mem_debug_header_t *header;
    如 (!ptr)
        返回;
    header = malloc_check(ptr, "tcc_free");
    mem_cur_size -= header->size;
    header->size = (无符)-1;
    如 (header->next)
        header->next->prev = header->prev;
    如 (header->prev)
        header->prev->next = header->next;
    如 (header == mem_debug_chain)
        mem_debug_chain = header->next;
    free(header);
}

PUB_FUNC 空 *tcc_mallocz_debug(无符 长 size, 不变 字 *file, 整 line)
{
    空 *ptr;
    ptr = tcc_malloc_debug(size,file,line);
    memset(ptr, 0, size);
    返回 ptr;
}

PUB_FUNC 空 *tcc_realloc_debug(空 *ptr, 无符 长 size, 不变 字 *file, 整 line)
{
    mem_debug_header_t *header;
    整 mem_debug_chain_update = 0;
    如 (!ptr)
        返回 tcc_malloc_debug(size, file, line);
    header = malloc_check(ptr, "tcc_realloc");
    mem_cur_size -= header->size;
    mem_debug_chain_update = (header == mem_debug_chain);
    header = realloc(header, 求长度(mem_debug_header_t) + size);
    如 (!header)
        tcc_error("memory full (realloc)");
    header->size = size;
    MEM_DEBUG_CHECK3(header) = MEM_DEBUG_MAGIC3;
    如 (header->next)
        header->next->prev = header;
    如 (header->prev)
        header->prev->next = header;
    如 (mem_debug_chain_update)
        mem_debug_chain = header;
    mem_cur_size += size;
    如 (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;
    返回 MEM_USER_PTR(header);
}

PUB_FUNC 字 *tcc_strdup_debug(不变 字 *str, 不变 字 *file, 整 line)
{
    字 *ptr;
    ptr = tcc_malloc_debug(strlen(str) + 1, file, line);
    strcpy(ptr, str);
    返回 ptr;
}

PUB_FUNC 空 tcc_memcheck(空)
{
    如 (mem_cur_size) {
        mem_debug_header_t *header = mem_debug_chain;
        fprintf(stderr, "MEM_DEBUG: mem_leak= %d bytes, mem_max_size= %d bytes\n",
            mem_cur_size, mem_max_size);
        当 (header) {
            fprintf(stderr, "%s:%u: error: %u bytes leaked\n",
                header->file_name, header->line_num, header->size);
            header = header->next;
        }
#如 MEM_DEBUG-0 == 2
        exit(2);
#了如
    }
}
#了如 /* MEM_DEBUG */

#定义 free(p) use_tcc_free(p)
#定义 malloc(s) use_tcc_malloc(s)
#定义 realloc(p, s) use_tcc_realloc(p, s)

/********************************************************/
/* dynarrays */

ST_FUNC 空 dynarray_add(空 *ptab, 整 *nb_ptr, 空 *data)
{
    整 nb, nb_alloc;
    空 **pp;

    nb = *nb_ptr;
    pp = *(空 ***)ptab;
    /* every power of two we double array size */
    如 ((nb & (nb - 1)) == 0) {
        如 (!nb)
            nb_alloc = 1;
        另
            nb_alloc = nb * 2;
        pp = tcc_realloc(pp, nb_alloc * 求长度(空 *));
        *(空***)ptab = pp;
    }
    pp[nb++] = data;
    *nb_ptr = nb;
}

ST_FUNC 空 dynarray_reset(空 *pp, 整 *n)
{
    空 **p;
    对于 (p = *(空***)pp; *n; ++p, --*n)
        如 (*p)
            tcc_free(*p);
    tcc_free(*(空**)pp);
    *(空**)pp = NULL;
}

静态 空 tcc_split_path(TCCState *s, 空 *p_ary, 整 *p_nb_ary, 不变 字 *in)
{
    不变 字 *p;
    运行 {
        整 c;
        CString str;

        cstr_new(&str);
        对于 (p = in; c = *p, c != '\0' && c != PATHSEP; ++p) {
            如 (c == '{' && p[1] && p[2] == '}') {
                c = p[1], p += 2;
                如 (c == 'B')
                    cstr_cat(&str, s->tcc_lib_path, -1);
            } 另 {
                cstr_ccat(&str, c);
            }
        }
        如 (str.size) {
            cstr_ccat(&str, '\0');
            dynarray_add(p_ary, p_nb_ary, tcc_strdup(str.data));
        }
        cstr_free(&str);
        in = p+1;
    } 当 (*p);
}

/********************************************************/

静态 空 strcat_vprintf(字 *buf, 整 buf_size, 不变 字 *fmt, va_list ap)
{
    整 len;
    len = strlen(buf);
    vsnprintf(buf + len, buf_size - len, fmt, ap);
}

静态 空 strcat_printf(字 *buf, 整 buf_size, 不变 字 *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strcat_vprintf(buf, buf_size, fmt, ap);
    va_end(ap);
}

静态 空 error1(TCCState *s1, 整 is_warning, 不变 字 *fmt, va_list ap)
{
    字 buf[2048];
    BufferedFile **pf, *f;

    buf[0] = '\0';
    /* use upper file if inline ":asm:" or token ":paste:" */
    对于 (f = file; f && f->filename[0] == ':'; f = f->prev)
     ;
    如 (f) {
        对于(pf = s1->include_stack; pf < s1->include_stack_ptr; pf++)
            strcat_printf(buf, 求长度(buf), "In file included from %s:%d:\n",
                (*pf)->filename, (*pf)->line_num);
        如 (f->line_num > 0) {
            strcat_printf(buf, 求长度(buf), "%s:%d: ",
                f->filename, f->line_num - !!(tok_flags & TOK_FLAG_BOL));
        } 另 {
            strcat_printf(buf, 求长度(buf), "%s: ",
                f->filename);
        }
    } 另 {
        strcat_printf(buf, 求长度(buf), "tcc: ");
    }
    如 (is_warning)
        strcat_printf(buf, 求长度(buf), "warning: ");
    另
        strcat_printf(buf, 求长度(buf), "error: ");
    strcat_vprintf(buf, 求长度(buf), fmt, ap);

    如 (!s1->error_func) {
        /* default case: stderr */
        如 (s1->output_type == TCC_OUTPUT_PREPROCESS && s1->ppfp == stdout)
            /* print a newline during tcc -E */
            printf("\n"), fflush(stdout);
        fflush(stdout); /* flush -v output */
        fprintf(stderr, "%s\n", buf);
        fflush(stderr); /* print error/warning now (win32) */
    } 另 {
        s1->error_func(s1->error_opaque, buf);
    }
    如 (!is_warning || s1->warn_error)
        s1->nb_errors++;
}

LIBTCCAPI 空 tcc_set_error_func(TCCState *s, 空 *error_opaque,
                        空 (*error_func)(空 *opaque, 不变 字 *msg))
{
    s->error_opaque = error_opaque;
    s->error_func = error_func;
}

/* error without aborting current compilation */
PUB_FUNC 空 tcc_error_noabort(不变 字 *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
}

PUB_FUNC 空 tcc_error(不变 字 *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
    /* better than nothing: in some cases, we accept to handle errors */
    如 (s1->error_set_jmp_enabled) {
        longjmp(s1->error_jmp_buf, 1);
    } 另 {
        /* XXX: eliminate this someday */
        exit(1);
    }
}

PUB_FUNC 空 tcc_warning(不变 字 *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    如 (s1->warn_none)
        返回;

    va_start(ap, fmt);
    error1(s1, 1, fmt, ap);
    va_end(ap);
}

/********************************************************/
/* I/O layer */

ST_FUNC 空 tcc_open_bf(TCCState *s1, 不变 字 *filename, 整 initlen)
{
    BufferedFile *bf;
    整 buflen = initlen ? initlen : IO_BUF_SIZE;

    bf = tcc_mallocz(求长度(BufferedFile) + buflen);
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + initlen;
    bf->buf_end[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, 求长度(bf->filename), filename);
    bf->true_filename = bf->filename;
    bf->line_num = 1;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;
    bf->fd = -1;
    bf->prev = file;
    file = bf;
    tok_flags = TOK_FLAG_BOL | TOK_FLAG_BOF;
}

ST_FUNC 空 tcc_close(空)
{
    BufferedFile *bf = file;
    如 (bf->fd > 0) {
        close(bf->fd);
        total_lines += bf->line_num;
    }
    如 (bf->true_filename != bf->filename)
        tcc_free(bf->true_filename);
    file = bf->prev;
    tcc_free(bf);
}

ST_FUNC 整 tcc_open(TCCState *s1, 不变 字 *filename)
{
    整 fd;
    如 (strcmp(filename, "-") == 0)
        fd = 0, filename = "<stdin>";
    另
        fd = open(filename, O_RDONLY | O_BINARY);
    如 ((s1->verbose == 2 && fd >= 0) || s1->verbose == 3)
        printf("%s %*s%s\n", fd < 0 ? "nf":"->",
               (整)(s1->include_stack_ptr - s1->include_stack), "", filename);
    如 (fd < 0)
        返回 -1;
    tcc_open_bf(s1, filename, 0);
#如定义 _WIN32
    normalize_slashes(file->filename);
#了如
    file->fd = fd;
    返回 fd;
}

/* compile the file opened in 'file'. Return non zero if errors. */
静态 整 tcc_compile(TCCState *s1)
{
    Sym *define_start;
    整 filetype, is_asm;

    define_start = define_stack;
    filetype = s1->filetype;
    is_asm = filetype == AFF_TYPE_ASM || filetype == AFF_TYPE_ASMPP;

    如 (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;
        s1->error_set_jmp_enabled = 1;

        preprocess_start(s1, is_asm);
        如 (s1->output_type == TCC_OUTPUT_PREPROCESS) {
            tcc_preprocess(s1);
        } 另 如 (is_asm) {
#如定义 CONFIG_TCC_ASM
            tcc_assemble(s1, filetype == AFF_TYPE_ASMPP);
#另
            tcc_error_noabort("asm not supported");
#了如
        } 另 {
            tccgen_compile(s1);
        }
    }
    s1->error_set_jmp_enabled = 0;

    preprocess_end(s1);
    free_inline_functions(s1);
    /* reset define stack, but keep -D and built-ins */
    free_defines(define_start);
    sym_pop(&global_stack, NULL, 0);
    sym_pop(&local_stack, NULL, 0);
    返回 s1->nb_errors != 0 ? -1 : 0;
}

LIBTCCAPI 整 tcc_compile_string(TCCState *s, 不变 字 *str)
{
    整 len, ret;

    len = strlen(str);
    tcc_open_bf(s, "<string>", len);
    memcpy(file->buffer, str, len);
    ret = tcc_compile(s);
    tcc_close();
    返回 ret;
}

/* define a preprocessor symbol. A value can also be provided with the '=' operator */
LIBTCCAPI 空 tcc_define_symbol(TCCState *s1, 不变 字 *sym, 不变 字 *value)
{
    整 len1, len2;
    /* default value */
    如 (!value)
        value = "1";
    len1 = strlen(sym);
    len2 = strlen(value);

    /* init file structure */
    tcc_open_bf(s1, "<define>", len1 + len2 + 1);
    memcpy(file->buffer, sym, len1);
    file->buffer[len1] = ' ';
    memcpy(file->buffer + len1 + 1, value, len2);

    /* parse with define parser */
    next_nomacro();
    parse_define();
    tcc_close();
}

/* undefine a preprocessor symbol */
LIBTCCAPI 空 tcc_undefine_symbol(TCCState *s1, 不变 字 *sym)
{
    TokenSym *ts;
    Sym *s;
    ts = tok_alloc(sym, strlen(sym));
    s = define_find(ts->tok);
    /* undefine symbol by putting an invalid name */
    如 (s)
        define_undef(s);
}

/* cleanup all static data used during compilation */
静态 空 tcc_cleanup(空)
{
    如 (NULL == tcc_state)
        返回;
    当 (file)
        tcc_close();
    tccpp_delete(tcc_state);
    tcc_state = NULL;
    /* free sym_pools */
    dynarray_reset(&sym_pools, &nb_sym_pools);
    /* reset symbol stack */
    sym_free_first = NULL;
}

LIBTCCAPI TCCState *tcc_new(空)
{
    TCCState *s;

    tcc_cleanup();

    s = tcc_mallocz(求长度(TCCState));
    如 (!s)
        返回 NULL;
    tcc_state = s;
    ++nb_states;

    s->alacarte_link = 1;
    s->nocommon = 1;
    s->warn_implicit_function_declaration = 1;
    s->ms_extensions = 1;

#如定义 CHAR_IS_UNSIGNED
    s->char_is_unsigned = 1;
#了如
#如定义 TCC_TARGET_I386
    s->seg_size = 32;
#了如
    /* enable this if you want symbols with leading underscore on windows: */
#如 0 /* def TCC_TARGET_PE */
    s->leading_underscore = 1;
#了如
#如定义 _WIN32
    tcc_set_lib_path_w32(s);
#另
    tcc_set_lib_path(s, CONFIG_TCCDIR);
#了如
    tccelf_new(s);
    tccpp_new(s);

    /* we add dummy defines for some special macros to speed up tests
       and to have working defined() */
    define_push(TOK___LINE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___FILE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___DATE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___TIME__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___COUNTER__, MACRO_OBJ, NULL, NULL);
    {
        /* define __TINYC__ 92X  */
        字 buffer[32]; 整 a,b,c;
        sscanf(TCC_VERSION, "%d.%d.%d", &a, &b, &c);
        sprintf(buffer, "%d", a*10000 + b*100 + c);
        tcc_define_symbol(s, "__TINYC__", buffer);
    }

    /* standard defines */
    tcc_define_symbol(s, "__STDC__", NULL);
    tcc_define_symbol(s, "__STDC_VERSION__", "199901L");
    tcc_define_symbol(s, "__STDC_HOSTED__", NULL);

    /* target defines */
#如 已定义(TCC_TARGET_I386)
    tcc_define_symbol(s, "__i386__", NULL);
    tcc_define_symbol(s, "__i386", NULL);
    tcc_define_symbol(s, "i386", NULL);
#另如 已定义(TCC_TARGET_X86_64)
    tcc_define_symbol(s, "__x86_64__", NULL);
#另如 已定义(TCC_TARGET_ARM)
    tcc_define_symbol(s, "__ARM_ARCH_4__", NULL);
    tcc_define_symbol(s, "__arm_elf__", NULL);
    tcc_define_symbol(s, "__arm_elf", NULL);
    tcc_define_symbol(s, "arm_elf", NULL);
    tcc_define_symbol(s, "__arm__", NULL);
    tcc_define_symbol(s, "__arm", NULL);
    tcc_define_symbol(s, "arm", NULL);
    tcc_define_symbol(s, "__APCS_32__", NULL);
    tcc_define_symbol(s, "__ARMEL__", NULL);
#如 已定义(TCC_ARM_EABI)
    tcc_define_symbol(s, "__ARM_EABI__", NULL);
#了如
#如 已定义(TCC_ARM_HARDFLOAT)
    s->float_abi = ARM_HARD_FLOAT;
    tcc_define_symbol(s, "__ARM_PCS_VFP", NULL);
#另
    s->float_abi = ARM_SOFTFP_FLOAT;
#了如
#另如 已定义(TCC_TARGET_ARM64)
    tcc_define_symbol(s, "__aarch64__", NULL);
#了如

#如定义 TCC_TARGET_PE
    tcc_define_symbol(s, "_WIN32", NULL);
# 如定义 TCC_TARGET_X86_64
    tcc_define_symbol(s, "_WIN64", NULL);
# 了如
#另
    tcc_define_symbol(s, "__unix__", NULL);
    tcc_define_symbol(s, "__unix", NULL);
    tcc_define_symbol(s, "unix", NULL);
# 如 已定义(__linux__)
    tcc_define_symbol(s, "__linux__", NULL);
    tcc_define_symbol(s, "__linux", NULL);
# 了如
# 如 已定义(__FreeBSD__)
    tcc_define_symbol(s, "__FreeBSD__", "__FreeBSD__");
    /* No 'Thread Storage Local' on FreeBSD with tcc */
    tcc_define_symbol(s, "__NO_TLS", NULL);
# 了如
# 如 已定义(__FreeBSD_kernel__)
    tcc_define_symbol(s, "__FreeBSD_kernel__", NULL);
# 了如
#了如
# 如 已定义(__NetBSD__)
    tcc_define_symbol(s, "__NetBSD__", "__NetBSD__");
# 了如
# 如 已定义(__OpenBSD__)
    tcc_define_symbol(s, "__OpenBSD__", "__OpenBSD__");
# 了如

    /* TinyCC & gcc defines */
#如 已定义(TCC_TARGET_PE) && PTR_SIZE == 8
    /* 64bit Windows. */
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long long");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long long");
    tcc_define_symbol(s, "__LLP64__", NULL);
#另如 PTR_SIZE == 8
    /* Other 64bit systems. */
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long");
    tcc_define_symbol(s, "__LP64__", NULL);
#另
    /* Other 32bit systems. */
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned int");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "int");
    tcc_define_symbol(s, "__ILP32__", NULL);
#了如

#如 已定义(TCC_MUSL)
    tcc_define_symbol(s, "__builtin_va_list", "void *");
#了如 /* TCC_MUSL */

#如定义 TCC_TARGET_PE
    tcc_define_symbol(s, "__WCHAR_TYPE__", "unsigned short");
    tcc_define_symbol(s, "__WINT_TYPE__", "unsigned short");
#另
    tcc_define_symbol(s, "__WCHAR_TYPE__", "int");
    /* wint_t is unsigned int by default, but (signed) int on BSDs
       and unsigned short on windows.  Other OSes might have still
       other conventions, sigh.  */
# 如 已定义(__FreeBSD__) || 已定义 (__FreeBSD_kernel__) \
  || 已定义(__NetBSD__) || 已定义(__OpenBSD__)
    tcc_define_symbol(s, "__WINT_TYPE__", "int");
#  如定义 __FreeBSD__
    /* define __GNUC__ to have some useful stuff from sys/cdefs.h
       that are unconditionally used in FreeBSDs other system headers :/ */
    tcc_define_symbol(s, "__GNUC__", "2");
    tcc_define_symbol(s, "__GNUC_MINOR__", "7");
    tcc_define_symbol(s, "__builtin_alloca", "alloca");
#  了如
# 另
    tcc_define_symbol(s, "__WINT_TYPE__", "unsigned int");
    /* glibc defines */
    tcc_define_symbol(s, "__REDIRECT(name, proto, alias)",
        "name proto __asm__ (#alias)");
    tcc_define_symbol(s, "__REDIRECT_NTH(name, proto, alias)",
        "name proto __asm__ (#alias) __THROW");
# 了如
    /* Some GCC builtins that are simple to express as macros.  */
    tcc_define_symbol(s, "__builtin_extract_return_addr(x)", "x");
#了如 /* ndef TCC_TARGET_PE */
    返回 s;
}

LIBTCCAPI 空 tcc_delete(TCCState *s1)
{
    tcc_cleanup();

    /* free sections */
    tccelf_delete(s1);

    /* free library paths */
    dynarray_reset(&s1->library_paths, &s1->nb_library_paths);
    dynarray_reset(&s1->crt_paths, &s1->nb_crt_paths);

    /* free include paths */
    dynarray_reset(&s1->cached_includes, &s1->nb_cached_includes);
    dynarray_reset(&s1->include_paths, &s1->nb_include_paths);
    dynarray_reset(&s1->sysinclude_paths, &s1->nb_sysinclude_paths);
    dynarray_reset(&s1->cmd_include_files, &s1->nb_cmd_include_files);

    tcc_free(s1->tcc_lib_path);
    tcc_free(s1->soname);
    tcc_free(s1->rpath);
    tcc_free(s1->init_symbol);
    tcc_free(s1->fini_symbol);
    tcc_free(s1->outfile);
    tcc_free(s1->deps_outfile);
    dynarray_reset(&s1->files, &s1->nb_files);
    dynarray_reset(&s1->target_deps, &s1->nb_target_deps);
    dynarray_reset(&s1->pragma_libs, &s1->nb_pragma_libs);
    dynarray_reset(&s1->argv, &s1->argc);

#如定义 TCC_IS_NATIVE
    /* free runtime memory */
    tcc_run_free(s1);
#了如

    tcc_free(s1);
    如 (0 == --nb_states)
        tcc_memcheck();
}

LIBTCCAPI 整 tcc_set_output_type(TCCState *s, 整 output_type)
{
    s->output_type = output_type;

    /* always elf for objects */
    如 (output_type == TCC_OUTPUT_OBJ)
        s->output_format = TCC_OUTPUT_FORMAT_ELF;

    如 (s->char_is_unsigned)
        tcc_define_symbol(s, "__CHAR_UNSIGNED__", NULL);

    如 (!s->nostdinc) {
        /* default include paths */
        /* -isystem paths have already been handled */
        tcc_add_sysinclude_path(s, CONFIG_TCC_SYSINCLUDEPATHS);
    }

#如定义 CONFIG_TCC_BCHECK
    如 (s->do_bounds_check) {
        /* if bound checking, then add corresponding sections */
        tccelf_bounds_new(s);
        /* define symbol */
        tcc_define_symbol(s, "__BOUNDS_CHECKING_ON", NULL);
    }
#了如
    如 (s->do_debug) {
        /* add debug sections */
        tccelf_stab_new(s);
    }

    tcc_add_library_path(s, CONFIG_TCC_LIBPATHS);

#如定义 TCC_TARGET_PE
# 如定义 _WIN32
    如 (!s->nostdlib && output_type != TCC_OUTPUT_OBJ)
        tcc_add_systemdir(s);
# 了如
#另
    /* paths for crt objects */
    tcc_split_path(s, &s->crt_paths, &s->nb_crt_paths, CONFIG_TCC_CRTPREFIX);
    /* add libc crt1/crti objects */
    如 ((output_type == TCC_OUTPUT_EXE || output_type == TCC_OUTPUT_DLL) &&
        !s->nostdlib) {
        如 (output_type != TCC_OUTPUT_DLL)
            tcc_add_crt(s, "crt1.o");
        tcc_add_crt(s, "crti.o");
    }
#了如
    返回 0;
}

LIBTCCAPI 整 tcc_add_include_path(TCCState *s, 不变 字 *pathname)
{
    tcc_split_path(s, &s->include_paths, &s->nb_include_paths, pathname);
    返回 0;
}

LIBTCCAPI 整 tcc_add_sysinclude_path(TCCState *s, 不变 字 *pathname)
{
    tcc_split_path(s, &s->sysinclude_paths, &s->nb_sysinclude_paths, pathname);
    返回 0;
}

ST_FUNC 整 tcc_add_file_internal(TCCState *s1, 不变 字 *filename, 整 flags)
{
    整 ret;

    /* open the file */
    ret = tcc_open(s1, filename);
    如 (ret < 0) {
        如 (flags & AFF_PRINT_ERROR)
            tcc_error_noabort("file '%s' not found", filename);
        返回 ret;
    }

    /* update target deps */
    dynarray_add(&s1->target_deps, &s1->nb_target_deps,
            tcc_strdup(filename));

    如 (flags & AFF_TYPE_BIN) {
        ElfW(Ehdr) ehdr;
        整 fd, obj_type;

        fd = file->fd;
        obj_type = tcc_object_type(fd, &ehdr);
        lseek(fd, 0, SEEK_SET);

        /* do not display line number if error */
        file->line_num = 0;

#如定义 TCC_TARGET_MACHO
        如 (0 == obj_type && 0 == strcmp(tcc_fileextension(filename), "dylib"))
            obj_type = AFF_BINTYPE_DYN;
#了如

        转接 (obj_type) {
        事例 AFF_BINTYPE_REL:
            ret = tcc_load_object_file(s1, fd, 0);
            跳出;
#如未定义 TCC_TARGET_PE
        事例 AFF_BINTYPE_DYN:
            如 (s1->output_type == TCC_OUTPUT_MEMORY) {
                ret = 0;
#如定义 TCC_IS_NATIVE
                如 (NULL == dlopen(filename, RTLD_GLOBAL | RTLD_LAZY))
                    ret = -1;
#了如
            } 另 {
                ret = tcc_load_dll(s1, fd, filename,
                                   (flags & AFF_REFERENCED_DLL) != 0);
            }
            跳出;
#了如
        事例 AFF_BINTYPE_AR:
            ret = tcc_load_archive(s1, fd);
            跳出;
#如定义 TCC_TARGET_COFF
        事例 AFF_BINTYPE_C67:
            ret = tcc_load_coff(s1, fd);
            跳出;
#了如
        缺省:
#如定义 TCC_TARGET_PE
            ret = pe_load_file(s1, filename, fd);
#另
            /* as GNU ld, consider it is an ld script if not recognized */
            ret = tcc_load_ldscript(s1);
#了如
            如 (ret < 0)
                tcc_error_noabort("unrecognized file type");
            跳出;
        }
    } 另 {
        ret = tcc_compile(s1);
    }
    tcc_close();
    返回 ret;
}

LIBTCCAPI 整 tcc_add_file(TCCState *s, 不变 字 *filename)
{
    整 filetype = s->filetype;
    整 flags = AFF_PRINT_ERROR;
    如 (filetype == 0) {
        /* use a file extension to detect a filetype */
        不变 字 *ext = tcc_fileextension(filename);
        如 (ext[0]) {
            ext++;
            如 (!strcmp(ext, "S"))
                filetype = AFF_TYPE_ASMPP;
            另 如 (!strcmp(ext, "s"))
                filetype = AFF_TYPE_ASM;
            另 如 (!PATHCMP(ext, "c") || !PATHCMP(ext, "i"))
                filetype = AFF_TYPE_C;
            另
                flags |= AFF_TYPE_BIN;
        } 另 {
            filetype = AFF_TYPE_C;
        }
        s->filetype = filetype;
    }
    返回 tcc_add_file_internal(s, filename, flags);
}

LIBTCCAPI 整 tcc_add_library_path(TCCState *s, 不变 字 *pathname)
{
    tcc_split_path(s, &s->library_paths, &s->nb_library_paths, pathname);
    返回 0;
}

静态 整 tcc_add_library_internal(TCCState *s, 不变 字 *fmt,
    不变 字 *filename, 整 flags, 字 **paths, 整 nb_paths)
{
    字 buf[1024];
    整 i;

    对于(i = 0; i < nb_paths; i++) {
        snprintf(buf, 求长度(buf), fmt, paths[i], filename);
        如 (tcc_add_file_internal(s, buf, flags | AFF_TYPE_BIN) == 0)
            返回 0;
    }
    返回 -1;
}

/* find and load a dll. Return non zero if not found */
/* XXX: add '-rpath' option support ? */
ST_FUNC 整 tcc_add_dll(TCCState *s, 不变 字 *filename, 整 flags)
{
    返回 tcc_add_library_internal(s, "%s/%s", filename, flags,
        s->library_paths, s->nb_library_paths);
}

ST_FUNC 整 tcc_add_crt(TCCState *s, 不变 字 *filename)
{
    如 (-1 == tcc_add_library_internal(s, "%s/%s",
        filename, 0, s->crt_paths, s->nb_crt_paths))
        tcc_error_noabort("file '%s' not found", filename);
    返回 0;
}

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI 整 tcc_add_library(TCCState *s, 不变 字 *libraryname)
{
#如 已定义 TCC_TARGET_PE
    不变 字 *libs[] = { "%s/%s.def", "%s/lib%s.def", "%s/%s.dll", "%s/lib%s.dll", "%s/lib%s.a", NULL };
    不变 字 **pp = s->static_link ? libs + 4 : libs;
#另如 已定义 TCC_TARGET_MACHO
    不变 字 *libs[] = { "%s/lib%s.dylib", "%s/lib%s.a", NULL };
    不变 字 **pp = s->static_link ? libs + 1 : libs;
#另
    不变 字 *libs[] = { "%s/lib%s.so", "%s/lib%s.a", NULL };
    不变 字 **pp = s->static_link ? libs + 1 : libs;
#了如
    当 (*pp) {
        如 (0 == tcc_add_library_internal(s, *pp,
            libraryname, 0, s->library_paths, s->nb_library_paths))
            返回 0;
        ++pp;
    }
    返回 -1;
}

PUB_FUNC 整 tcc_add_library_err(TCCState *s, 不变 字 *libname)
{
    整 ret = tcc_add_library(s, libname);
    如 (ret < 0)
        tcc_error_noabort("library '%s' not found", libname);
    返回 ret;
}

/* handle #pragma comment(lib,) */
ST_FUNC 空 tcc_add_pragma_libs(TCCState *s1)
{
    整 i;
    对于 (i = 0; i < s1->nb_pragma_libs; i++)
        tcc_add_library_err(s1, s1->pragma_libs[i]);
}

LIBTCCAPI 整 tcc_add_symbol(TCCState *s, 不变 字 *name, 不变 空 *val)
{
#如定义 TCC_TARGET_PE
    /* On x86_64 'val' might not be reachable with a 32bit offset.
       So it is handled here as if it were in a DLL. */
    pe_putimport(s, 0, name, (uintptr_t)val);
#另
    set_elf_sym(symtab_section, (uintptr_t)val, 0,
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE), 0,
        SHN_ABS, name);
#了如
    返回 0;
}

LIBTCCAPI 空 tcc_set_lib_path(TCCState *s, 不变 字 *path)
{
    tcc_free(s->tcc_lib_path);
    s->tcc_lib_path = tcc_strdup(path);
}

#定义 WD_ALL    0x0001 /* warning is activated when using -Wall */
#定义 FD_INVERT 0x0002 /* invert value before storing */

类型定义 结构 FlagDef {
    uint16_t offset;
    uint16_t flags;
    不变 字 *name;
} FlagDef;

静态 整 no_flag(不变 字 **pp)
{
    不变 字 *p = *pp;
    如 (*p != 'n' || *++p != 'o' || *++p != '-')
        返回 0;
    *pp = p + 1;
    返回 1;
}

ST_FUNC 整 set_flag(TCCState *s, 不变 FlagDef *flags, 不变 字 *name)
{
    整 value, ret;
    不变 FlagDef *p;
    不变 字 *r;

    value = 1;
    r = name;
    如 (no_flag(&r))
        value = 0;

    对于 (ret = -1, p = flags; p->name; ++p) {
        如 (ret) {
            如 (strcmp(r, p->name))
                继续;
        } 另 {
            如 (0 == (p->flags & WD_ALL))
                继续;
        }
        如 (p->offset) {
            *(整*)((字 *)s + p->offset) =
                p->flags & FD_INVERT ? !value : value;
            如 (ret)
                返回 0;
        } 另 {
            ret = 0;
        }
    }
    返回 ret;
}

静态 整 strstart(不变 字 *val, 不变 字 **str)
{
    不变 字 *p, *q;
    p = *str;
    q = val;
    当 (*q) {
        如 (*p != *q)
            返回 0;
        p++;
        q++;
    }
    *str = p;
    返回 1;
}

/* Like strstart, but automatically takes into account that ld options can
 *
 * - start with double or single dash (e.g. '--soname' or '-soname')
 * - arguments can be given as separate or after '=' (e.g. '-Wl,-soname,x.so'
 *   or '-Wl,-soname=x.so')
 *
 * you provide `val` always in 'option[=]' form (no leading -)
 */
静态 整 link_option(不变 字 *str, 不变 字 *val, 不变 字 **ptr)
{
    不变 字 *p, *q;
    整 ret;

    /* there should be 1 or 2 dashes */
    如 (*str++ != '-')
        返回 0;
    如 (*str == '-')
        str++;

    /* then str & val should match (potentially up to '=') */
    p = str;
    q = val;

    ret = 1;
    如 (q[0] == '?') {
        ++q;
        如 (no_flag(&p))
            ret = -1;
    }

    当 (*q != '\0' && *q != '=') {
        如 (*p != *q)
            返回 0;
        p++;
        q++;
    }

    /* '=' near eos means ',' or '=' is ok */
    如 (*q == '=') {
        如 (*p == 0)
            *ptr = p;
        如 (*p != ',' && *p != '=')
            返回 0;
        p++;
    } 另 如 (*p) {
        返回 0;
    }
    *ptr = p;
    返回 ret;
}

静态 不变 字 *skip_linker_arg(不变 字 **str)
{
    不变 字 *s1 = *str;
    不变 字 *s2 = strchr(s1, ',');
    *str = s2 ? s2++ : (s2 = s1 + strlen(s1));
    返回 s2;
}

静态 空 copy_linker_arg(字 **pp, 不变 字 *s, 整 sep)
{
    不变 字 *q = s;
    字 *p = *pp;
    整 l = 0;
    如 (p && sep)
        p[l = strlen(p)] = sep, ++l;
    skip_linker_arg(&q);
    pstrncpy(l + (*pp = tcc_realloc(p, q - s + l + 1)), s, q - s);
}

/* set linker options */
静态 整 tcc_set_linker(TCCState *s, 不变 字 *option)
{
    当 (*option) {

        不变 字 *p = NULL;
        字 *end = NULL;
        整 ignoring = 0;
        整 ret;

        如 (link_option(option, "Bsymbolic", &p)) {
            s->symbolic = 1;
        } 另 如 (link_option(option, "nostdlib", &p)) {
            s->nostdlib = 1;
        } 另 如 (link_option(option, "fini=", &p)) {
            copy_linker_arg(&s->fini_symbol, p, 0);
            ignoring = 1;
        } 另 如 (link_option(option, "image-base=", &p)
                || link_option(option, "Ttext=", &p)) {
            s->text_addr = strtoull(p, &end, 16);
            s->has_text_addr = 1;
        } 另 如 (link_option(option, "init=", &p)) {
            copy_linker_arg(&s->init_symbol, p, 0);
            ignoring = 1;
        } 另 如 (link_option(option, "oformat=", &p)) {
#如 已定义(TCC_TARGET_PE)
            如 (strstart("pe-", &p)) {
#另如 PTR_SIZE == 8
            如 (strstart("elf64-", &p)) {
#另
            如 (strstart("elf32-", &p)) {
#了如
                s->output_format = TCC_OUTPUT_FORMAT_ELF;
            } 另 如 (!strcmp(p, "binary")) {
                s->output_format = TCC_OUTPUT_FORMAT_BINARY;
#如定义 TCC_TARGET_COFF
            } 另 如 (!strcmp(p, "coff")) {
                s->output_format = TCC_OUTPUT_FORMAT_COFF;
#了如
            } 另
                跳转 err;

        } 另 如 (link_option(option, "as-needed", &p)) {
            ignoring = 1;
        } 另 如 (link_option(option, "O", &p)) {
            ignoring = 1;
        } 另 如 (link_option(option, "export-all-symbols", &p)) {
            s->rdynamic = 1;
        } 另 如 (link_option(option, "rpath=", &p)) {
            copy_linker_arg(&s->rpath, p, ':');
        } 另 如 (link_option(option, "enable-new-dtags", &p)) {
            s->enable_new_dtags = 1;
        } 另 如 (link_option(option, "section-alignment=", &p)) {
            s->section_align = strtoul(p, &end, 16);
        } 另 如 (link_option(option, "soname=", &p)) {
            copy_linker_arg(&s->soname, p, 0);
#如定义 TCC_TARGET_PE
        } 另 如 (link_option(option, "large-address-aware", &p)) {
            s->pe_characteristics |= 0x20;
        } 另 如 (link_option(option, "file-alignment=", &p)) {
            s->pe_file_align = strtoul(p, &end, 16);
        } 另 如 (link_option(option, "stack=", &p)) {
            s->pe_stack_size = strtoul(p, &end, 10);
        } 另 如 (link_option(option, "subsystem=", &p)) {
#如 已定义(TCC_TARGET_I386) || 已定义(TCC_TARGET_X86_64)
            如 (!strcmp(p, "native")) {
                s->pe_subsystem = 1;
            } 另 如 (!strcmp(p, "console")) {
                s->pe_subsystem = 3;
            } 另 如 (!strcmp(p, "gui") || !strcmp(p, "windows")) {
                s->pe_subsystem = 2;
            } 另 如 (!strcmp(p, "posix")) {
                s->pe_subsystem = 7;
            } 另 如 (!strcmp(p, "efiapp")) {
                s->pe_subsystem = 10;
            } 另 如 (!strcmp(p, "efiboot")) {
                s->pe_subsystem = 11;
            } 另 如 (!strcmp(p, "efiruntime")) {
                s->pe_subsystem = 12;
            } 另 如 (!strcmp(p, "efirom")) {
                s->pe_subsystem = 13;
#另如 已定义(TCC_TARGET_ARM)
            如 (!strcmp(p, "wince")) {
                s->pe_subsystem = 9;
#了如
            } 另
                跳转 err;
#了如
        } 另 如 (ret = link_option(option, "?whole-archive", &p), ret) {
            s->alacarte_link = ret < 0;
        } 另 如 (p) {
            返回 0;
        } 另 {
    err:
            tcc_error("unsupported linker option '%s'", option);
        }

        如 (ignoring && s->warn_unsupported)
            tcc_warning("unsupported linker option '%s'", option);

        option = skip_linker_arg(&p);
    }
    返回 1;
}

类型定义 结构 TCCOption {
    不变 字 *name;
    uint16_t index;
    uint16_t flags;
} TCCOption;

枚举 {
    TCC_OPTION_HELP,
    TCC_OPTION_HELP2,
    TCC_OPTION_v,
    TCC_OPTION_I,
    TCC_OPTION_D,
    TCC_OPTION_U,
    TCC_OPTION_P,
    TCC_OPTION_L,
    TCC_OPTION_B,
    TCC_OPTION_l,
    TCC_OPTION_bench,
    TCC_OPTION_bt,
    TCC_OPTION_b,
    TCC_OPTION_g,
    TCC_OPTION_c,
    TCC_OPTION_dumpversion,
    TCC_OPTION_d,
    TCC_OPTION_static,
    TCC_OPTION_std,
    TCC_OPTION_shared,
    TCC_OPTION_soname,
    TCC_OPTION_o,
    TCC_OPTION_r,
    TCC_OPTION_s,
    TCC_OPTION_traditional,
    TCC_OPTION_Wl,
    TCC_OPTION_Wp,
    TCC_OPTION_W,
    TCC_OPTION_O,
    TCC_OPTION_mfloat_abi,
    TCC_OPTION_m,
    TCC_OPTION_f,
    TCC_OPTION_isystem,
    TCC_OPTION_iwithprefix,
    TCC_OPTION_include,
    TCC_OPTION_nostdinc,
    TCC_OPTION_nostdlib,
    TCC_OPTION_print_search_dirs,
    TCC_OPTION_rdynamic,
    TCC_OPTION_param,
    TCC_OPTION_pedantic,
    TCC_OPTION_pthread,
    TCC_OPTION_run,
    TCC_OPTION_w,
    TCC_OPTION_pipe,
    TCC_OPTION_E,
    TCC_OPTION_MD,
    TCC_OPTION_MF,
    TCC_OPTION_x,
    TCC_OPTION_ar,
    TCC_OPTION_impdef
};

#定义 TCC_OPTION_HAS_ARG 0x0001
#定义 TCC_OPTION_NOSEP   0x0002 /* cannot have space before option and arg */

静态 不变 TCCOption tcc_options[] = {
    { "h", TCC_OPTION_HELP, 0 },
    { "-help", TCC_OPTION_HELP, 0 },
    { "?", TCC_OPTION_HELP, 0 },
    { "hh", TCC_OPTION_HELP2, 0 },
    { "v", TCC_OPTION_v, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "I", TCC_OPTION_I, TCC_OPTION_HAS_ARG },
    { "D", TCC_OPTION_D, TCC_OPTION_HAS_ARG },
    { "U", TCC_OPTION_U, TCC_OPTION_HAS_ARG },
    { "P", TCC_OPTION_P, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "L", TCC_OPTION_L, TCC_OPTION_HAS_ARG },
    { "B", TCC_OPTION_B, TCC_OPTION_HAS_ARG },
    { "l", TCC_OPTION_l, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "bench", TCC_OPTION_bench, 0 },
#如定义 CONFIG_TCC_BACKTRACE
    { "bt", TCC_OPTION_bt, TCC_OPTION_HAS_ARG },
#了如
#如定义 CONFIG_TCC_BCHECK
    { "b", TCC_OPTION_b, 0 },
#了如
    { "g", TCC_OPTION_g, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "c", TCC_OPTION_c, 0 },
    { "dumpversion", TCC_OPTION_dumpversion, 0},
    { "d", TCC_OPTION_d, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "static", TCC_OPTION_static, 0 },
    { "std", TCC_OPTION_std, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "shared", TCC_OPTION_shared, 0 },
    { "soname", TCC_OPTION_soname, TCC_OPTION_HAS_ARG },
    { "o", TCC_OPTION_o, TCC_OPTION_HAS_ARG },
    { "-param", TCC_OPTION_param, TCC_OPTION_HAS_ARG },
    { "pedantic", TCC_OPTION_pedantic, 0},
    { "pthread", TCC_OPTION_pthread, 0},
    { "run", TCC_OPTION_run, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "rdynamic", TCC_OPTION_rdynamic, 0 },
    { "r", TCC_OPTION_r, 0 },
    { "s", TCC_OPTION_s, 0 },
    { "traditional", TCC_OPTION_traditional, 0 },
    { "Wl,", TCC_OPTION_Wl, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "Wp,", TCC_OPTION_Wp, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "W", TCC_OPTION_W, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "O", TCC_OPTION_O, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
#如定义 TCC_TARGET_ARM
    { "mfloat-abi", TCC_OPTION_mfloat_abi, TCC_OPTION_HAS_ARG },
#了如
    { "m", TCC_OPTION_m, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "f", TCC_OPTION_f, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "isystem", TCC_OPTION_isystem, TCC_OPTION_HAS_ARG },
    { "iwithprefix", TCC_OPTION_iwithprefix, TCC_OPTION_HAS_ARG },
    { "include", TCC_OPTION_include, TCC_OPTION_HAS_ARG },
    { "nostdinc", TCC_OPTION_nostdinc, 0 },
    { "nostdlib", TCC_OPTION_nostdlib, 0 },
    { "print-search-dirs", TCC_OPTION_print_search_dirs, 0 },
    { "w", TCC_OPTION_w, 0 },
    { "pipe", TCC_OPTION_pipe, 0},
    { "E", TCC_OPTION_E, 0},
    { "MD", TCC_OPTION_MD, 0},
    { "MF", TCC_OPTION_MF, TCC_OPTION_HAS_ARG },
    { "x", TCC_OPTION_x, TCC_OPTION_HAS_ARG },
    { "ar", TCC_OPTION_ar, 0},
#如定义 TCC_TARGET_PE
    { "impdef", TCC_OPTION_impdef, 0},
#了如
    { NULL, 0, 0 },
};

静态 不变 FlagDef options_W[] = {
    { 0, 0, "all" },
    { offsetof(TCCState, warn_unsupported), 0, "unsupported" },
    { offsetof(TCCState, warn_write_strings), 0, "write-strings" },
    { offsetof(TCCState, warn_error), 0, "error" },
    { offsetof(TCCState, warn_gcc_compat), 0, "gcc-compat" },
    { offsetof(TCCState, warn_implicit_function_declaration), WD_ALL,
      "implicit-function-declaration" },
    { 0, 0, NULL }
};

静态 不变 FlagDef options_f[] = {
    { offsetof(TCCState, char_is_unsigned), 0, "unsigned-char" },
    { offsetof(TCCState, char_is_unsigned), FD_INVERT, "signed-char" },
    { offsetof(TCCState, nocommon), FD_INVERT, "common" },
    { offsetof(TCCState, leading_underscore), 0, "leading-underscore" },
    { offsetof(TCCState, ms_extensions), 0, "ms-extensions" },
    { offsetof(TCCState, dollars_in_identifiers), 0, "dollars-in-identifiers" },
    { 0, 0, NULL }
};

静态 不变 FlagDef options_m[] = {
    { offsetof(TCCState, ms_bitfields), 0, "ms-bitfields" },
#如定义 TCC_TARGET_X86_64
    { offsetof(TCCState, nosse), FD_INVERT, "sse" },
#了如
    { 0, 0, NULL }
};

静态 空 parse_option_D(TCCState *s1, 不变 字 *optarg)
{
    字 *sym = tcc_strdup(optarg);
    字 *value = strchr(sym, '=');
    如 (value)
        *value++ = '\0';
    tcc_define_symbol(s1, sym, value);
    tcc_free(sym);
}

静态 空 args_parser_add_file(TCCState *s, 不变 字* filename, 整 filetype)
{
    结构 filespec *f = tcc_malloc(求长度 *f + strlen(filename));
    f->type = filetype;
    f->alacarte = s->alacarte_link;
    strcpy(f->name, filename);
    dynarray_add(&s->files, &s->nb_files, f);
}

静态 整 args_parser_make_argv(不变 字 *r, 整 *argc, 字 ***argv)
{
    整 ret = 0, q, c;
    CString str;
    对于(;;) {
        当 (c = (无符 字)*r, c && c <= ' ')
            ++r;
        如 (c == 0)
            跳出;
        q = 0;
        cstr_new(&str);
        当 (c = (无符 字)*r, c) {
            ++r;
            如 (c == '\\' && (*r == '"' || *r == '\\')) {
                c = *r++;
            } 另 如 (c == '"') {
                q = !q;
                继续;
            } 另 如 (q == 0 && c <= ' ') {
                跳出;
            }
            cstr_ccat(&str, c);
        }
        cstr_ccat(&str, 0);
        //printf("<%s>\n", str.data), fflush(stdout);
        dynarray_add(argv, argc, tcc_strdup(str.data));
        cstr_free(&str);
        ++ret;
    }
    返回 ret;
}

/* read list file */
静态 空 args_parser_listfile(TCCState *s,
    不变 字 *filename, 整 optind, 整 *pargc, 字 ***pargv)
{
    整 fd, i;
    size_t len;
    字 *p;
    整 argc = 0;
    字 **argv = NULL;

    fd = open(filename, O_RDONLY | O_BINARY);
    如 (fd < 0)
        tcc_error("listfile '%s' not found", filename);

    len = lseek(fd, 0, SEEK_END);
    p = tcc_malloc(len + 1), p[len] = 0;
    lseek(fd, 0, SEEK_SET), read(fd, p, len), close(fd);

    对于 (i = 0; i < *pargc; ++i)
        如 (i == optind)
            args_parser_make_argv(p, &argc, &argv);
        另
            dynarray_add(&argv, &argc, tcc_strdup((*pargv)[i]));

    tcc_free(p);
    dynarray_reset(&s->argv, &s->argc);
    *pargc = s->argc = argc, *pargv = s->argv = argv;
}

PUB_FUNC 整 tcc_parse_args(TCCState *s, 整 *pargc, 字 ***pargv, 整 optind)
{
    不变 TCCOption *popt;
    不变 字 *optarg, *r;
    不变 字 *run = NULL;
    整 last_o = -1;
    整 x;
    CString linker_arg; /* collect -Wl options */
    字 buf[1024];
    整 tool = 0, arg_start = 0, noaction = optind;
    字 **argv = *pargv;
    整 argc = *pargc;

    cstr_new(&linker_arg);

    当 (optind < argc) {
        r = argv[optind];
        如 (r[0] == '@' && r[1] != '\0') {
            args_parser_listfile(s, r + 1, optind, &argc, &argv);
            继续;
        }
        optind++;
        如 (tool) {
            如 (r[0] == '-' && r[1] == 'v' && r[2] == 0)
                ++s->verbose;
            继续;
        }
reparse:
        如 (r[0] != '-' || r[1] == '\0') {
            如 (r[0] != '@') /* allow "tcc file(s) -run @ args ..." */
                args_parser_add_file(s, r, s->filetype);
            如 (run) {
                tcc_set_options(s, run);
                arg_start = optind - 1;
                跳出;
            }
            继续;
        }

        /* find option in table */
        对于(popt = tcc_options; ; ++popt) {
            不变 字 *p1 = popt->name;
            不变 字 *r1 = r + 1;
            如 (p1 == NULL)
                tcc_error("invalid option -- '%s'", r);
            如 (!strstart(p1, &r1))
                继续;
            optarg = r1;
            如 (popt->flags & TCC_OPTION_HAS_ARG) {
                如 (*r1 == '\0' && !(popt->flags & TCC_OPTION_NOSEP)) {
                    如 (optind >= argc)
                arg_err:
                        tcc_error("argument to '%s' is missing", r);
                    optarg = argv[optind++];
                }
            } 另 如 (*r1 != '\0')
                继续;
            跳出;
        }

        转接(popt->index) {
        事例 TCC_OPTION_HELP:
            返回 OPT_HELP;
        事例 TCC_OPTION_HELP2:
            返回 OPT_HELP2;
        事例 TCC_OPTION_I:
            tcc_add_include_path(s, optarg);
            跳出;
        事例 TCC_OPTION_D:
            parse_option_D(s, optarg);
            跳出;
        事例 TCC_OPTION_U:
            tcc_undefine_symbol(s, optarg);
            跳出;
        事例 TCC_OPTION_L:
            tcc_add_library_path(s, optarg);
            跳出;
        事例 TCC_OPTION_B:
            /* set tcc utilities path (mainly for tcc development) */
            tcc_set_lib_path(s, optarg);
            跳出;
        事例 TCC_OPTION_l:
            args_parser_add_file(s, optarg, AFF_TYPE_LIB);
            s->nb_libraries++;
            跳出;
        事例 TCC_OPTION_pthread:
            parse_option_D(s, "_REENTRANT");
            s->option_pthread = 1;
            跳出;
        事例 TCC_OPTION_bench:
            s->do_bench = 1;
            跳出;
#如定义 CONFIG_TCC_BACKTRACE
        事例 TCC_OPTION_bt:
            tcc_set_num_callers(atoi(optarg));
            跳出;
#了如
#如定义 CONFIG_TCC_BCHECK
        事例 TCC_OPTION_b:
            s->do_bounds_check = 1;
            s->do_debug = 1;
            跳出;
#了如
        事例 TCC_OPTION_g:
            s->do_debug = 1;
            跳出;
        事例 TCC_OPTION_c:
            x = TCC_OUTPUT_OBJ;
        set_output_type:
            如 (s->output_type)
                tcc_warning("-%s: overriding compiler action already specified", popt->name);
            s->output_type = x;
            跳出;
        事例 TCC_OPTION_d:
            如 (*optarg == 'D')
                s->dflag = 3;
            另 如 (*optarg == 'M')
                s->dflag = 7;
            另 如 (*optarg == 't')
                s->dflag = 16;
            另 如 (isnum(*optarg))
                g_debug = atoi(optarg);
            另
                跳转 unsupported_option;
            跳出;
        事例 TCC_OPTION_static:
            s->static_link = 1;
            跳出;
        事例 TCC_OPTION_std:
            /* silently ignore, a current purpose:
               allow to use a tcc as a reference compiler for "make test" */
            跳出;
        事例 TCC_OPTION_shared:
            x = TCC_OUTPUT_DLL;
            跳转 set_output_type;
        事例 TCC_OPTION_soname:
            s->soname = tcc_strdup(optarg);
            跳出;
        事例 TCC_OPTION_o:
            如 (s->outfile) {
                tcc_warning("multiple -o option");
                tcc_free(s->outfile);
            }
            s->outfile = tcc_strdup(optarg);
            跳出;
        事例 TCC_OPTION_r:
            /* generate a .o merging several output files */
            s->option_r = 1;
            x = TCC_OUTPUT_OBJ;
            跳转 set_output_type;
        事例 TCC_OPTION_isystem:
            tcc_add_sysinclude_path(s, optarg);
            跳出;
        事例 TCC_OPTION_iwithprefix:
            snprintf(buf, 求长度 buf, "{B}/%s", optarg);
            tcc_add_sysinclude_path(s, buf);
            跳出;
        事例 TCC_OPTION_include:
            dynarray_add(&s->cmd_include_files,
                         &s->nb_cmd_include_files, tcc_strdup(optarg));
            跳出;
        事例 TCC_OPTION_nostdinc:
            s->nostdinc = 1;
            跳出;
        事例 TCC_OPTION_nostdlib:
            s->nostdlib = 1;
            跳出;
        事例 TCC_OPTION_run:
#如未定义 TCC_IS_NATIVE
            tcc_error("-run is not available in a cross compiler");
#了如
            run = optarg;
            x = TCC_OUTPUT_MEMORY;
            跳转 set_output_type;
        事例 TCC_OPTION_v:
            运行 ++s->verbose; 当 (*optarg++ == 'v');
            ++noaction;
            跳出;
        事例 TCC_OPTION_f:
            如 (set_flag(s, options_f, optarg) < 0)
                跳转 unsupported_option;
            跳出;
#如定义 TCC_TARGET_ARM
        事例 TCC_OPTION_mfloat_abi:
            /* tcc doesn't support soft float yet */
            如 (!strcmp(optarg, "softfp")) {
                s->float_abi = ARM_SOFTFP_FLOAT;
                tcc_undefine_symbol(s, "__ARM_PCS_VFP");
            } 另 如 (!strcmp(optarg, "hard"))
                s->float_abi = ARM_HARD_FLOAT;
            另
                tcc_error("unsupported float abi '%s'", optarg);
            跳出;
#了如
        事例 TCC_OPTION_m:
            如 (set_flag(s, options_m, optarg) < 0) {
                如 (x = atoi(optarg), x != 32 && x != 64)
                    跳转 unsupported_option;
                如 (PTR_SIZE != x/8)
                    返回 x;
                ++noaction;
            }
            跳出;
        事例 TCC_OPTION_W:
            如 (set_flag(s, options_W, optarg) < 0)
                跳转 unsupported_option;
            跳出;
        事例 TCC_OPTION_w:
            s->warn_none = 1;
            跳出;
        事例 TCC_OPTION_rdynamic:
            s->rdynamic = 1;
            跳出;
        事例 TCC_OPTION_Wl:
            如 (linker_arg.size)
                --linker_arg.size, cstr_ccat(&linker_arg, ',');
            cstr_cat(&linker_arg, optarg, 0);
            如 (tcc_set_linker(s, linker_arg.data))
                cstr_free(&linker_arg);
            跳出;
        事例 TCC_OPTION_Wp:
            r = optarg;
            跳转 reparse;
        事例 TCC_OPTION_E:
            x = TCC_OUTPUT_PREPROCESS;
            跳转 set_output_type;
        事例 TCC_OPTION_P:
            s->Pflag = atoi(optarg) + 1;
            跳出;
        事例 TCC_OPTION_MD:
            s->gen_deps = 1;
            跳出;
        事例 TCC_OPTION_MF:
            s->deps_outfile = tcc_strdup(optarg);
            跳出;
        事例 TCC_OPTION_dumpversion:
            printf ("%s\n", TCC_VERSION);
            exit(0);
            跳出;
        事例 TCC_OPTION_x:
            如 (*optarg == 'c')
                s->filetype = AFF_TYPE_C;
            另 如 (*optarg == 'a')
                s->filetype = AFF_TYPE_ASMPP;
            另 如 (*optarg == 'n')
                s->filetype = AFF_TYPE_NONE;
            另
                tcc_warning("unsupported language '%s'", optarg);
            跳出;
        事例 TCC_OPTION_O:
            last_o = atoi(optarg);
            跳出;
        事例 TCC_OPTION_print_search_dirs:
            x = OPT_PRINT_DIRS;
            跳转 extra_action;
        事例 TCC_OPTION_impdef:
            x = OPT_IMPDEF;
            跳转 extra_action;
        事例 TCC_OPTION_ar:
            x = OPT_AR;
        extra_action:
            arg_start = optind - 1;
            如 (arg_start != noaction)
                tcc_error("cannot parse %s here", r);
            tool = x;
            跳出;
        事例 TCC_OPTION_traditional:
        事例 TCC_OPTION_pedantic:
        事例 TCC_OPTION_pipe:
        事例 TCC_OPTION_s:
            /* ignored */
            跳出;
        缺省:
unsupported_option:
            如 (s->warn_unsupported)
                tcc_warning("unsupported option '%s'", r);
            跳出;
        }
    }
    如 (last_o > 0)
        tcc_define_symbol(s, "__OPTIMIZE__", NULL);
    如 (linker_arg.size) {
        r = linker_arg.data;
        跳转 arg_err;
    }
    *pargc = argc - arg_start;
    *pargv = argv + arg_start;
    如 (tool)
        返回 tool;
    如 (optind != noaction)
        返回 0;
    如 (s->verbose == 2)
        返回 OPT_PRINT_DIRS;
    如 (s->verbose)
        返回 OPT_V;
    返回 OPT_HELP;
}

LIBTCCAPI 空 tcc_set_options(TCCState *s, 不变 字 *r)
{
    字 **argv = NULL;
    整 argc = 0;
    args_parser_make_argv(r, &argc, &argv);
    tcc_parse_args(s, &argc, &argv, 0);
    dynarray_reset(&argv, &argc);
}

PUB_FUNC 空 tcc_print_stats(TCCState *s, 无符 total_time)
{
    如 (total_time < 1)
        total_time = 1;
    如 (total_bytes < 1)
        total_bytes = 1;
    fprintf(stderr, "* %d idents, %d lines, %d bytes\n"
                    "* %0.3f s, %u lines/s, %0.1f MB/s\n",
           tok_ident - TOK_IDENT, total_lines, total_bytes,
           (双精)total_time/1000,
           (无符)total_lines*1000/total_time,
           (双精)total_bytes/1000/total_time);
#如定义 MEM_DEBUG
    fprintf(stderr, "* %d bytes memory used\n", mem_max_size);
#了如
}
