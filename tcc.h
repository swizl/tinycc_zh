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

#如未定义 _TCC_H
#定义 _TCC_H

#定义 _GNU_SOURCE
#包含 "config.h"

#包含 <stdlib.h>
#包含 <stdio.h>
#包含 <stdarg.h>
#包含 <string.h>
#包含 <errno.h>
#包含 <math.h>
#包含 <fcntl.h>
#包含 <setjmp.h>
#包含 <time.h>

#如未定义 _WIN32
# 包含 <unistd.h>
# 包含 <sys/time.h>
# 如未定义 CONFIG_TCC_STATIC
#  包含 <dlfcn.h>
# 了如
/* XXX: need to define this to use them in non ISOC99 context */
外部 单精 strtof (不变 字 *__nptr, 字 **__endptr);
外部 长 双精 strtold (不变 字 *__nptr, 字 **__endptr);
#了如

#如定义 _WIN32
# 包含 <windows.h>
# 包含 <io.h> /* open, close etc. */
# 包含 <direct.h> /* getcwd */
# 如定义 __GNUC__
#  包含 <stdint.h>
# 了如
# 定义 inline __inline
# 定义 snprintf _snprintf
# 定义 vsnprintf _vsnprintf
# 如未定义 __GNUC__
#  定义 strtold (长 双精)strtod
#  定义 strtof (单精)strtod
#  定义 strtoll _strtoi64
#  定义 strtoull _strtoui64
# 了如
# 如定义 LIBTCC_AS_DLL
#  定义 LIBTCCAPI __declspec(dllexport)
#  定义 PUB_FUNC LIBTCCAPI
# 了如
# 定义 inp next_inp /* inp is an intrinsic on msvc/mingw */
# 如定义 _MSC_VER
#  pragma warning (disable : 4244)  // conversion from 'uint64_t' to 'int', possible loss of data
#  pragma warning (disable : 4267)  // conversion from 'size_t' to 'int', possible loss of data
#  pragma warning (disable : 4996)  // The POSIX name for this item is deprecated. Instead, use the ISO C and C++ conformant name
#  pragma warning (disable : 4018)  // signed/unsigned mismatch
#  pragma warning (disable : 4146)  // unary minus operator applied to unsigned type, result still unsigned
#  定义 ssize_t intptr_t
# 了如
# 消定义 CONFIG_TCC_STATIC
#了如

#如未定义 O_BINARY
# 定义 O_BINARY 0
#了如

#如未定义 offsetof
#定义 offsetof(type, field) ((size_t) &((type *)0)->field)
#了如

#如未定义 countof
#定义 countof(tab) (求长度(tab) / 求长度((tab)[0]))
#了如

#如定义 _MSC_VER
# 定义 NORETURN __declspec(noreturn)
# 定义 ALIGNED(x) __declspec(align(x))
#另
# 定义 NORETURN __attribute__((noreturn))
# 定义 ALIGNED(x) __attribute__((aligned(x)))
#了如

#如定义 _WIN32
# 定义 IS_DIRSEP(c) (c == '/' || c == '\\')
# 定义 IS_ABSPATH(p) (IS_DIRSEP(p[0]) || (p[0] && p[1] == ':' && IS_DIRSEP(p[2])))
# 定义 PATHCMP stricmp
#另
# 定义 IS_DIRSEP(c) (c == '/')
# 定义 IS_ABSPATH(p) IS_DIRSEP(p[0])
# 定义 PATHCMP strcmp
#了如

/* -------------------------------------------- */

/* parser debug */
/* #定义 PARSE_DEBUG */
/* preprocessor debug */
/* #定义 PP_DEBUG */
/* include file debug */
/* #定义 INC_DEBUG */
/* memory leak debug */
/* #定义 MEM_DEBUG */
/* assembler debug */
/* #定义 ASM_DEBUG */

/* target selection */
/* #定义 TCC_TARGET_I386   *//* i386 code generator */
/* #定义 TCC_TARGET_X86_64 *//* x86-64 code generator */
/* #定义 TCC_TARGET_ARM    *//* ARMv4 code generator */
/* #定义 TCC_TARGET_ARM64  *//* ARMv8 code generator */
/* #定义 TCC_TARGET_C67    *//* TMS320C67xx code generator */

/* default target is I386 */
#如 !已定义(TCC_TARGET_I386) && !已定义(TCC_TARGET_ARM) && \
    !已定义(TCC_TARGET_ARM64) && !已定义(TCC_TARGET_C67) && \
    !已定义(TCC_TARGET_X86_64)
# 如 已定义 __x86_64__ || 已定义 _AMD64_
#  定义 TCC_TARGET_X86_64
# 另如 已定义 __arm__
#  定义 TCC_TARGET_ARM
#  定义 TCC_ARM_EABI
#  定义 TCC_ARM_HARDFLOAT
# 另如 已定义 __aarch64__
#  定义 TCC_TARGET_ARM64
# 另
#  定义 TCC_TARGET_I386
# 了如
# 如定义 _WIN32
#  定义 TCC_TARGET_PE 1
# 了如
#了如

/* only native compiler supports -run */
#如 已定义 _WIN32 == 已定义 TCC_TARGET_PE
# 如 (已定义 __i386__ || 已定义 _X86_) && 已定义 TCC_TARGET_I386
#  定义 TCC_IS_NATIVE
# 另如 (已定义 __x86_64__ || 已定义 _AMD64_) && 已定义 TCC_TARGET_X86_64
#  定义 TCC_IS_NATIVE
# 另如 已定义 __arm__ && 已定义 TCC_TARGET_ARM
#  定义 TCC_IS_NATIVE
# 另如 已定义 __aarch64__ && 已定义 TCC_TARGET_ARM64
#  定义 TCC_IS_NATIVE
# 了如
#了如

#如 已定义 TCC_IS_NATIVE && !已定义 CONFIG_TCCBOOT
# 定义 CONFIG_TCC_BACKTRACE
# 如 (已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64) \
  && !已定义 TCC_UCLIBC && !已定义 TCC_MUSL
# 定义 CONFIG_TCC_BCHECK /* enable bound checking code */
# 了如
#了如

/* ------------ path configuration ------------ */

#如未定义 CONFIG_SYSROOT
# 定义 CONFIG_SYSROOT ""
#了如
#如未定义 CONFIG_TCCDIR
# 定义 CONFIG_TCCDIR "/usr/local/lib/tcc"
#了如
#如未定义 CONFIG_LDDIR
# 定义 CONFIG_LDDIR "lib"
#了如
#如定义 CONFIG_TRIPLET
# 定义 USE_TRIPLET(s) s "/" CONFIG_TRIPLET
# 定义 ALSO_TRIPLET(s) USE_TRIPLET(s) ":" s
#另
# 定义 USE_TRIPLET(s) s
# 定义 ALSO_TRIPLET(s) s
#了如

/* path to find crt1.o, crti.o and crtn.o */
#如未定义 CONFIG_TCC_CRTPREFIX
# 定义 CONFIG_TCC_CRTPREFIX USE_TRIPLET(CONFIG_SYSROOT "/usr/" CONFIG_LDDIR)
#了如

/* Below: {B} is substituted by CONFIG_TCCDIR (rsp. -B option) */

/* system include paths */
#如未定义 CONFIG_TCC_SYSINCLUDEPATHS
# 如定义 TCC_TARGET_PE
#  定义 CONFIG_TCC_SYSINCLUDEPATHS "{B}/include;{B}/include/winapi"
# 另
#  定义 CONFIG_TCC_SYSINCLUDEPATHS \
        "{B}/include" \
    ":" ALSO_TRIPLET(CONFIG_SYSROOT "/usr/local/include") \
    ":" ALSO_TRIPLET(CONFIG_SYSROOT "/usr/include")
# 了如
#了如

/* library search paths */
#如未定义 CONFIG_TCC_LIBPATHS
# 如定义 TCC_TARGET_PE
#  定义 CONFIG_TCC_LIBPATHS "{B}/lib"
# 另
#  定义 CONFIG_TCC_LIBPATHS \
        ALSO_TRIPLET(CONFIG_SYSROOT "/usr/" CONFIG_LDDIR) \
    ":" ALSO_TRIPLET(CONFIG_SYSROOT "/" CONFIG_LDDIR) \
    ":" ALSO_TRIPLET(CONFIG_SYSROOT "/usr/local/" CONFIG_LDDIR)
# 了如
#了如

/* name of ELF interpreter */
#如未定义 CONFIG_TCC_ELFINTERP
# 如 已定义 __FreeBSD__
#  定义 CONFIG_TCC_ELFINTERP "/libexec/ld-elf.so.1"
# 另如 已定义 __FreeBSD_kernel__
#  如 已定义(TCC_TARGET_X86_64)
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-kfreebsd-x86-64.so.1"
#  另
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld.so.1"
#  了如
# 另如 已定义 __DragonFly__
#  定义 CONFIG_TCC_ELFINTERP "/usr/libexec/ld-elf.so.2"
# 另如 已定义 __NetBSD__
#  定义 CONFIG_TCC_ELFINTERP "/usr/libexec/ld.elf_so"
# 另如 已定义 __GNU__
#  定义 CONFIG_TCC_ELFINTERP "/lib/ld.so"
# 另如 已定义(TCC_TARGET_PE)
#  定义 CONFIG_TCC_ELFINTERP "-"
# 另如 已定义(TCC_UCLIBC)
#  定义 CONFIG_TCC_ELFINTERP "/lib/ld-uClibc.so.0" /* is there a uClibc for x86_64 ? */
# 另如 已定义 TCC_TARGET_ARM64
#  如 已定义(TCC_MUSL)
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-musl-aarch64.so.1"
#  另
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-linux-aarch64.so.1"
#  了如
# 另如 已定义(TCC_TARGET_X86_64)
#  如 已定义(TCC_MUSL)
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-musl-x86_64.so.1"
#  另
#   定义 CONFIG_TCC_ELFINTERP "/lib64/ld-linux-x86-64.so.2"
#  了如
# 另如 !已定义(TCC_ARM_EABI)
#  如 已定义(TCC_MUSL)
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-musl-arm.so.1"
#  另
#   定义 CONFIG_TCC_ELFINTERP "/lib/ld-linux.so.2"
#  了如
# 了如
#了如

/* var elf_interp dans *-gen.c */
#如定义 CONFIG_TCC_ELFINTERP
# 定义 DEFAULT_ELFINTERP(s) CONFIG_TCC_ELFINTERP
#另
# 定义 DEFAULT_ELFINTERP(s) default_elfinterp(s)
#了如

/* (target specific) libtcc1.a */
#如未定义 TCC_LIBTCC1
# 定义 TCC_LIBTCC1 "libtcc1.a"
#了如

/* library to use with CONFIG_USE_LIBGCC instead of libtcc1.a */
#如 已定义 CONFIG_USE_LIBGCC && !已定义 TCC_LIBGCC
#定义 TCC_LIBGCC USE_TRIPLET(CONFIG_SYSROOT "/" CONFIG_LDDIR) "/libgcc_s.so.1"
#了如

#如定义 TCC_TARGET_PE
#定义 PATHSEP ';'
#另
#定义 PATHSEP ':'
#了如

/* -------------------------------------------- */

#包含 "libtcc.h"
#包含 "elf.h"
#包含 "stab.h"

/* -------------------------------------------- */

#如未定义 PUB_FUNC /* functions used by tcc.c but not in libtcc.h */
# 定义 PUB_FUNC
#了如

#如未定义 ONE_SOURCE
# 定义 ONE_SOURCE 1
#了如

#如 ONE_SOURCE
#定义 ST_INLN 静态 inline
#定义 ST_FUNC 静态
#定义 ST_DATA 静态
#另
#定义 ST_INLN
#定义 ST_FUNC
#定义 ST_DATA 外部
#了如

#如定义 TCC_PROFILE /* profile all functions */
# 定义 静态
#了如

/* -------------------------------------------- */
/* include the target specific definitions */

#定义 TARGET_DEFS_ONLY
#如定义 TCC_TARGET_I386
# 包含 "i386-gen.c"
# 包含 "i386-link.c"
#了如
#如定义 TCC_TARGET_X86_64
# 包含 "x86_64-gen.c"
# 包含 "x86_64-link.c"
#了如
#如定义 TCC_TARGET_ARM
# 包含 "arm-gen.c"
# 包含 "arm-link.c"
# 包含 "arm-asm.c"
#了如
#如定义 TCC_TARGET_ARM64
# 包含 "arm64-gen.c"
# 包含 "arm64-link.c"
#了如
#如定义 TCC_TARGET_C67
# 定义 TCC_TARGET_COFF
# 包含 "coff.h"
# 包含 "c67-gen.c"
# 包含 "c67-link.c"
#了如
#消定义 TARGET_DEFS_ONLY

/* -------------------------------------------- */

#如 PTR_SIZE == 8
# 定义 ELFCLASSW ELFCLASS64
# 定义 ElfW(type) Elf##64##_##type
# 定义 ELFW(type) ELF##64##_##type
# 定义 ElfW_Rel ElfW(Rela)
# 定义 SHT_RELX SHT_RELA
# 定义 REL_SECTION_FMT ".rela%s"
#另
# 定义 ELFCLASSW ELFCLASS32
# 定义 ElfW(type) Elf##32##_##type
# 定义 ELFW(type) ELF##32##_##type
# 定义 ElfW_Rel ElfW(Rel)
# 定义 SHT_RELX SHT_REL
# 定义 REL_SECTION_FMT ".rel%s"
#了如
/* target address type */
#定义 addr_t ElfW(Addr)

/* -------------------------------------------- */

#定义 INCLUDE_STACK_SIZE  32
#定义 IFDEF_STACK_SIZE    64
#定义 VSTACK_SIZE         256
#定义 STRING_MAX_SIZE     1024
#定义 TOKSTR_MAX_SIZE     256
#定义 PACK_STACK_SIZE     8

#定义 TOK_HASH_SIZE       16384 /* must be a power of two */
#定义 TOK_ALLOC_INCR      512  /* must be a power of two */
#定义 TOK_MAX_SIZE        4 /* token max size in int unit when stored in string */

/* token symbol management */
类型定义 结构 TokenSym {
    结构 TokenSym *hash_next;
    结构 Sym *sym_define; /* direct pointer to define */
    结构 Sym *sym_label; /* direct pointer to label */
    结构 Sym *sym_struct; /* direct pointer to structure */
    结构 Sym *sym_identifier; /* direct pointer to identifier */
    整 tok; /* token number */
    整 len;
    字 str[1];
} TokenSym;

#如定义 TCC_TARGET_PE
类型定义 无符 短 nwchar_t;
#另
类型定义 整 nwchar_t;
#了如

类型定义 结构 CString {
    整 size; /* size in bytes */
    空 *data; /* either 'char *' or 'nwchar_t *' */
    整 size_allocated;
} CString;

/* type definition */
类型定义 结构 CType {
    整 t;
    结构 Sym *ref;
} CType;

/* constant value */
类型定义 联合 CValue {
    长 双精 ld;
    双精 d;
    单精 f;
    uint64_t i;
    结构 {
        整 size;
        不变 空 *data;
    } str;
    整 tab[LDOUBLE_SIZE/4];
} CValue;

/* value on stack */
类型定义 结构 SValue {
    CType type;      /* type */
    无符 短 r;      /* register + flags */
    无符 短 r2;     /* second register, used for 'long long'
                              type. If not used, set to VT_CONST */
    CValue c;              /* constant, if VT_CONST */
    结构 Sym *sym;       /* symbol, if (VT_SYM | VT_CONST), or if
    			      result of unary() for an identifier. */
} SValue;

/* symbol attributes */
结构 SymAttr {
    无符 短
    aligned     : 5, /* alignment as log2+1 (0 == unspecified) */
    packed      : 1,
    weak        : 1,
    visibility  : 2,
    dllexport   : 1,
    dllimport   : 1,
    unused      : 5;
};

/* function attributes or temporary attributes for parsing */
结构 FuncAttr {
    无符
    func_call   : 3, /* calling convention (0..5), see below */
    func_type   : 2, /* FUNC_OLD/NEW/ELLIPSIS */
    func_body   : 1, /* body was defined */
    func_args   : 8; /* PE __stdcall args */
};

/* GNUC attribute definition */
类型定义 结构 AttributeDef {
    结构 SymAttr a;
    结构 FuncAttr f;
    结构 Section *section;
    整 alias_target; /* token */
    整 asm_label; /* associated asm label */
    字 attr_mode; /* __attribute__((__mode__(...))) */
} AttributeDef;

/* symbol management */
类型定义 结构 Sym {
    整 v; /* symbol token */
    无符 短 r; /* associated register or VT_CONST/VT_LOCAL and LVAL type */
    结构 SymAttr a; /* symbol attributes */
    联合 {
        结构 {
            整 c; /* associated number or Elf symbol index */
            联合 {
                整 sym_scope; /* scope level for locals */
                整 jnext; /* next jump label */
                结构 FuncAttr f; /* function attributes */
                整 auxtype; /* bitfield access type */
            };
        };
        长 长 enum_val; /* enum constant if IS_ENUM_VAL */
        整 *d; /* define token stream */
    };
    CType type; /* associated type */
    联合 {
        结构 Sym *next; /* next related symbol (for fields and anoms) */
        整 asm_label; /* associated asm label */
    };
    结构 Sym *prev; /* prev symbol in stack */
    结构 Sym *prev_tok; /* previous symbol for this token */
} Sym;

/* section definition */
/* XXX: use directly ELF structure for parameters ? */
/* special flag to indicate that the section should not be linked to
   the other ones */
#定义 SHF_PRIVATE 0x80000000

/* special flag, too */
#定义 SECTION_ABS ((空 *)1)

类型定义 结构 Section {
    无符 长 data_offset; /* current data offset */
    无符 字 *data;       /* section data */
    无符 长 data_allocated; /* used for realloc() handling */
    整 sh_name;             /* elf section name (only used during output) */
    整 sh_num;              /* elf section number */
    整 sh_type;             /* elf section type */
    整 sh_flags;            /* elf section flags */
    整 sh_info;             /* elf section info */
    整 sh_addralign;        /* elf section alignment */
    整 sh_entsize;          /* elf entry size */
    无符 长 sh_size;   /* section size (only used during output) */
    addr_t sh_addr;          /* address at which the section is relocated */
    无符 长 sh_offset; /* file offset */
    整 nb_hashed_syms;      /* used to resize the hash table */
    结构 Section *link;    /* link to another section */
    结构 Section *reloc;   /* corresponding section for relocation, if any */
    结构 Section *hash;    /* hash table for symbols */
    结构 Section *prev;    /* previous section on section stack */
    字 name[1];           /* section name */
} Section;

类型定义 结构 DLLReference {
    整 level;
    空 *handle;
    字 name[1];
} DLLReference;

/* -------------------------------------------------- */

#定义 SYM_STRUCT     0x40000000 /* struct/union/enum symbol space */
#定义 SYM_FIELD      0x20000000 /* struct/union field symbol space */
#定义 SYM_FIRST_ANOM 0x10000000 /* first anonymous sym */

/* stored in 'Sym->f.func_type' field */
#定义 FUNC_NEW       1 /* ansi function prototype */
#定义 FUNC_OLD       2 /* old function prototype */
#定义 FUNC_ELLIPSIS  3 /* ansi function prototype with ... */

/* stored in 'Sym->f.func_call' field */
#定义 FUNC_CDECL     0 /* standard c call */
#定义 FUNC_STDCALL   1 /* pascal c call */
#定义 FUNC_FASTCALL1 2 /* first param in %eax */
#定义 FUNC_FASTCALL2 3 /* first parameters in %eax, %edx */
#定义 FUNC_FASTCALL3 4 /* first parameter in %eax, %edx, %ecx */
#定义 FUNC_FASTCALLW 5 /* first parameter in %ecx, %edx */

/* field 'Sym.t' for macros */
#定义 MACRO_OBJ      0 /* object like macro */
#定义 MACRO_FUNC     1 /* function like macro */

/* field 'Sym.r' for C labels */
#定义 LABEL_DEFINED  0 /* label is defined */
#定义 LABEL_FORWARD  1 /* label is forward defined */
#定义 LABEL_DECLARED 2 /* label is declared but never used */

/* type_decl() types */
#定义 TYPE_ABSTRACT  1 /* type without variable */
#定义 TYPE_DIRECT    2 /* type with variable */

#定义 IO_BUF_SIZE 8192

类型定义 结构 BufferedFile {
    uint8_t *buf_ptr;
    uint8_t *buf_end;
    整 fd;
    结构 BufferedFile *prev;
    整 line_num;    /* current line number - here to simplify code */
    整 line_ref;    /* tcc -E: last printed line */
    整 ifndef_macro;  /* #ifndef macro / #endif search */
    整 ifndef_macro_saved; /* saved ifndef_macro */
    整 *ifdef_stack_ptr; /* ifdef_stack value at the start of the file */
    整 include_next_index; /* next search path */
    字 filename[1024];    /* filename */
    字 *true_filename; /* filename not modified by # line directive */
    无符 字 unget[4];
    无符 字 buffer[1]; /* extra size for CH_EOB char */
} BufferedFile;

#定义 CH_EOB   '\\'       /* end of buffer or '\0' char in file */
#定义 CH_EOF   (-1)   /* end of file */

/* used to record tokens */
类型定义 结构 TokenString {
    整 *str;
    整 len;
    整 lastlen;
    整 allocated_len;
    整 last_line_num;
    整 save_line_num;
    /* used to chain token-strings with begin/end_macro() */
    结构 TokenString *prev;
    不变 整 *prev_ptr;
    字 alloc;
} TokenString;

/* inline functions */
类型定义 结构 InlineFunc {
    TokenString *func_str;
    Sym *sym;
    字 filename[1];
} InlineFunc;

/* include file cache, used to find files faster and also to eliminate
   inclusion if the include file is protected by #ifndef ... #endif */
类型定义 结构 CachedInclude {
    整 ifndef_macro;
    整 once;
    整 hash_next; /* -1 if none */
    字 filename[1]; /* path specified in #include */
} CachedInclude;

#定义 CACHED_INCLUDES_HASH_SIZE 32

#如定义 CONFIG_TCC_ASM
类型定义 结构 ExprValue {
    uint64_t v;
    Sym *sym;
    整 pcrel;
} ExprValue;

#定义 MAX_ASM_OPERANDS 30
类型定义 结构 ASMOperand {
    整 id; /* GCC 3 optional identifier (0 if number only supported */
    字 *constraint;
    字 asm_str[16]; /* computed asm string for operand */
    SValue *vt; /* C value of the expression */
    整 ref_index; /* if >= 0, gives reference to a output constraint */
    整 input_index; /* if >= 0, gives reference to an input constraint */
    整 priority; /* priority, used to assign registers */
    整 reg; /* if >= 0, register number used for this operand */
    整 is_llong; /* true if double register value */
    整 is_memory; /* true if memory operand */
    整 is_rw;     /* for '+' modifier */
} ASMOperand;
#了如

/* extra symbol attributes (not in symbol table) */
结构 sym_attr {
    无符 got_offset;
    无符 plt_offset;
    整 plt_sym;
    整 dyn_index;
#如定义 TCC_TARGET_ARM
    无符 字 plt_thumb_stub:1;
#了如
};

结构 TCCState {

    整 verbose; /* if true, display some information during compilation */
    整 nostdinc; /* if true, no standard headers are added */
    整 nostdlib; /* if true, no standard libraries are added */
    整 nocommon; /* if true, do not use common symbols for .bss data */
    整 static_link; /* if true, static linking is performed */
    整 rdynamic; /* if true, all symbols are exported */
    整 symbolic; /* if true, resolve symbols in the current module first */
    整 alacarte_link; /* if true, only link in referenced objects from archive */

    字 *tcc_lib_path; /* CONFIG_TCCDIR or -B option */
    字 *soname; /* as specified on the command line (-soname) */
    字 *rpath; /* as specified on the command line (-Wl,-rpath=) */
    整 enable_new_dtags; /* ditto, (-Wl,--enable-new-dtags) */

    /* output type, see TCC_OUTPUT_XXX */
    整 output_type;
    /* output format, see TCC_OUTPUT_FORMAT_xxx */
    整 output_format;

    /* C language options */
    整 char_is_unsigned;
    整 leading_underscore;
    整 ms_extensions;	/* allow nested named struct w/o identifier behave like unnamed */
    整 dollars_in_identifiers;	/* allows '$' char in identifiers */
    整 ms_bitfields; /* if true, emulate MS algorithm for aligning bitfields */

    /* warning switches */
    整 warn_write_strings;
    整 warn_unsupported;
    整 warn_error;
    整 warn_none;
    整 warn_implicit_function_declaration;
    整 warn_gcc_compat;

    /* compile with debug symbol (and use them if error during execution) */
    整 do_debug;
#如定义 CONFIG_TCC_BCHECK
    /* compile with built-in memory and bounds checker */
    整 do_bounds_check;
#了如
#如定义 TCC_TARGET_ARM
    枚举 float_abi float_abi; /* float ABI of the generated code*/
#了如
    整 run_test; /* nth test to run with -dt -run */

    addr_t text_addr; /* address of text section */
    整 has_text_addr;

    无符 section_align; /* section alignment */

    字 *init_symbol; /* symbols to call at load-time (not used currently) */
    字 *fini_symbol; /* symbols to call at unload-time (not used currently) */
    
#如定义 TCC_TARGET_I386
    整 seg_size; /* 32. Can be 16 with i386 assembler (.code16) */
#了如
#如定义 TCC_TARGET_X86_64
    整 nosse; /* For -mno-sse support. */
#了如

    /* array of all loaded dlls (including those referenced by loaded dlls) */
    DLLReference **loaded_dlls;
    整 nb_loaded_dlls;

    /* include paths */
    字 **include_paths;
    整 nb_include_paths;

    字 **sysinclude_paths;
    整 nb_sysinclude_paths;

    /* library paths */
    字 **library_paths;
    整 nb_library_paths;

    /* crt?.o object path */
    字 **crt_paths;
    整 nb_crt_paths;

    /* -include files */
    字 **cmd_include_files;
    整 nb_cmd_include_files;

    /* error handling */
    空 *error_opaque;
    空 (*error_func)(空 *opaque, 不变 字 *msg);
    整 error_set_jmp_enabled;
    jmp_buf error_jmp_buf;
    整 nb_errors;

    /* output file for preprocessing (-E) */
    FILE *ppfp;
    枚举 {
	LINE_MACRO_OUTPUT_FORMAT_GCC,
	LINE_MACRO_OUTPUT_FORMAT_NONE,
	LINE_MACRO_OUTPUT_FORMAT_STD,
    LINE_MACRO_OUTPUT_FORMAT_P10 = 11
    } Pflag; /* -P switch */
    字 dflag; /* -dX value */

    /* for -MD/-MF: collected dependencies for this compilation */
    字 **target_deps;
    整 nb_target_deps;

    /* compilation */
    BufferedFile *include_stack[INCLUDE_STACK_SIZE];
    BufferedFile **include_stack_ptr;

    整 ifdef_stack[IFDEF_STACK_SIZE];
    整 *ifdef_stack_ptr;

    /* included files enclosed with #ifndef MACRO */
    整 cached_includes_hash[CACHED_INCLUDES_HASH_SIZE];
    CachedInclude **cached_includes;
    整 nb_cached_includes;

    /* #pragma pack stack */
    整 pack_stack[PACK_STACK_SIZE];
    整 *pack_stack_ptr;
    字 **pragma_libs;
    整 nb_pragma_libs;

    /* inline functions are stored as token lists and compiled last
       only if referenced */
    结构 InlineFunc **inline_fns;
    整 nb_inline_fns;

    /* sections */
    Section **sections;
    整 nb_sections; /* number of sections, including first dummy section */

    Section **priv_sections;
    整 nb_priv_sections; /* number of private sections */

    /* got & plt handling */
    Section *got;
    Section *plt;

    /* temporary dynamic symbol sections (for dll loading) */
    Section *dynsymtab_section;
    /* exported dynamic symbol section */
    Section *dynsym;
    /* copy of the global symtab_section variable */
    Section *symtab;
    /* extra attributes (eg. GOT/PLT value) for symtab symbols */
    结构 sym_attr *sym_attrs;
    整 nb_sym_attrs;
    /* tiny assembler state */
    Sym *asm_labels;

#如定义 TCC_TARGET_PE
    /* PE info */
    整 pe_subsystem;
    无符 pe_characteristics;
    无符 pe_file_align;
    无符 pe_stack_size;
# 如定义 TCC_TARGET_X86_64
    Section *uw_pdata;
    整 uw_sym;
    无符 uw_offs;
# 了如
#了如

#如定义 TCC_IS_NATIVE
    不变 字 *runtime_main;
    空 **runtime_mem;
    整 nb_runtime_mem;
#了如

    /* used by main and tcc_parse_args only */
    结构 filespec **files; /* files seen on command line */
    整 nb_files; /* number thereof */
    整 nb_libraries; /* number of libs thereof */
    整 filetype;
    字 *outfile; /* output filename */
    整 option_r; /* option -r */
    整 do_bench; /* option -bench */
    整 gen_deps; /* option -MD  */
    字 *deps_outfile; /* option -MF */
    整 option_pthread; /* -pthread option */
    整 argc;
    字 **argv;
};

结构 filespec {
    字 type;
    字 alacarte;
    字 name[1];
};

/* The current value can be: */
#定义 VT_VALMASK   0x003f  /* mask for value location, register or: */
#定义 VT_CONST     0x0030  /* constant in vc (must be first non register value) */
#定义 VT_LLOCAL    0x0031  /* lvalue, offset on stack */
#定义 VT_LOCAL     0x0032  /* offset on stack */
#定义 VT_CMP       0x0033  /* the value is stored in processor flags (in vc) */
#定义 VT_JMP       0x0034  /* value is the consequence of jmp true (even) */
#定义 VT_JMPI      0x0035  /* value is the consequence of jmp false (odd) */
#定义 VT_LVAL      0x0100  /* var is an lvalue */
#定义 VT_SYM       0x0200  /* a symbol value is added */
#定义 VT_MUSTCAST  0x0400  /* value must be casted to be correct (used for
                                char/short stored in integer registers) */
#定义 VT_MUSTBOUND 0x0800  /* bound checking must be done before
                                dereferencing value */
#定义 VT_BOUNDED   0x8000  /* value is bounded. The address of the
                                bounding function call point is in vc */
#定义 VT_LVAL_BYTE     0x1000  /* lvalue is a byte */
#定义 VT_LVAL_SHORT    0x2000  /* lvalue is a short */
#定义 VT_LVAL_UNSIGNED 0x4000  /* lvalue is unsigned */
#定义 VT_LVAL_TYPE     (VT_LVAL_BYTE | VT_LVAL_SHORT | VT_LVAL_UNSIGNED)

/* types */
#定义 VT_BTYPE       0x000f  /* mask for basic type */
#定义 VT_VOID             0  /* void type */
#定义 VT_BYTE             1  /* signed byte type */
#定义 VT_SHORT            2  /* short type */
#定义 VT_INT              3  /* integer type */
#定义 VT_LLONG            4  /* 64 bit integer */
#定义 VT_PTR              5  /* pointer */
#定义 VT_FUNC             6  /* function type */
#定义 VT_STRUCT           7  /* struct/union definition */
#定义 VT_FLOAT            8  /* IEEE float */
#定义 VT_DOUBLE           9  /* IEEE double */
#定义 VT_LDOUBLE         10  /* IEEE long double */
#定义 VT_BOOL            11  /* ISOC99 boolean type */
#定义 VT_QLONG           13  /* 128-bit integer. Only used for x86-64 ABI */
#定义 VT_QFLOAT          14  /* 128-bit float. Only used for x86-64 ABI */

#定义 VT_UNSIGNED    0x0010  /* unsigned type */
#定义 VT_DEFSIGN     0x0020  /* explicitly signed or unsigned */
#定义 VT_ARRAY       0x0040  /* array type (also has VT_PTR) */
#定义 VT_BITFIELD    0x0080  /* bitfield modifier */
#定义 VT_CONSTANT    0x0100  /* const modifier */
#定义 VT_VOLATILE    0x0200  /* volatile modifier */
#定义 VT_VLA         0x0400  /* VLA type (also has VT_PTR and VT_ARRAY) */
#定义 VT_LONG	       0x0800

/* storage */
#定义 VT_EXTERN  0x00001000  /* extern definition */
#定义 VT_STATIC  0x00002000  /* static variable */
#定义 VT_TYPEDEF 0x00004000  /* typedef definition */
#定义 VT_INLINE  0x00008000  /* inline definition */
/* currently unused: 0x0800, 0x000[1248]0000  */

#定义 VT_STRUCT_SHIFT 20     /* shift for bitfield shift values (32 - 2*6) */
#定义 VT_STRUCT_MASK (((1 << (6+6)) - 1) << VT_STRUCT_SHIFT | VT_BITFIELD)
#定义 BIT_POS(t) (((t) >> VT_STRUCT_SHIFT) & 0x3f)
#定义 BIT_SIZE(t) (((t) >> (VT_STRUCT_SHIFT + 6)) & 0x3f)

#定义 VT_UNION    (1 << VT_STRUCT_SHIFT | VT_STRUCT)
#定义 VT_ENUM     (2 << VT_STRUCT_SHIFT) /* integral type is an enum really */
#定义 VT_ENUM_VAL (3 << VT_STRUCT_SHIFT) /* integral type is an enum constant really */

#定义 IS_ENUM(t) ((t & VT_STRUCT_MASK) == VT_ENUM)
#定义 IS_ENUM_VAL(t) ((t & VT_STRUCT_MASK) == VT_ENUM_VAL)
#定义 IS_UNION(t) ((t & (VT_STRUCT_MASK|VT_BTYPE)) == VT_UNION)

/* type mask (except storage) */
#定义 VT_STORAGE (VT_EXTERN | VT_STATIC | VT_TYPEDEF | VT_INLINE)
#定义 VT_TYPE (~(VT_STORAGE|VT_STRUCT_MASK))


/* token values */

/* warning: the following compare tokens depend on i386 asm code */
#定义 TOK_ULT 0x92
#定义 TOK_UGE 0x93
#定义 TOK_EQ  0x94
#定义 TOK_NE  0x95
#定义 TOK_ULE 0x96
#定义 TOK_UGT 0x97
#定义 TOK_Nset 0x98
#定义 TOK_Nclear 0x99
#定义 TOK_LT  0x9c
#定义 TOK_GE  0x9d
#定义 TOK_LE  0x9e
#定义 TOK_GT  0x9f

#定义 TOK_LAND  0xa0
#定义 TOK_LOR   0xa1
#定义 TOK_DEC   0xa2
#定义 TOK_MID   0xa3 /* inc/dec, to void constant */
#定义 TOK_INC   0xa4
#定义 TOK_UDIV  0xb0 /* unsigned division */
#定义 TOK_UMOD  0xb1 /* unsigned modulo */
#定义 TOK_PDIV  0xb2 /* fast division with undefined rounding for pointers */

/* tokens that carry values (in additional token string space / tokc) --> */
#定义 TOK_CCHAR   0xb3 /* char constant in tokc */
#定义 TOK_LCHAR   0xb4
#定义 TOK_CINT    0xb5 /* number in tokc */
#定义 TOK_CUINT   0xb6 /* unsigned int constant */
#定义 TOK_CLLONG  0xb7 /* long long constant */
#定义 TOK_CULLONG 0xb8 /* unsigned long long constant */
#定义 TOK_STR     0xb9 /* pointer to string in tokc */
#定义 TOK_LSTR    0xba
#定义 TOK_CFLOAT  0xbb /* float constant */
#定义 TOK_CDOUBLE 0xbc /* double constant */
#定义 TOK_CLDOUBLE 0xbd /* long double constant */
#定义 TOK_PPNUM   0xbe /* preprocessor number */
#定义 TOK_PPSTR   0xbf /* preprocessor string */
#定义 TOK_LINENUM 0xc0 /* line number info */
/* <-- */

#定义 TOK_UMULL    0xc2 /* unsigned 32x32 -> 64 mul */
#定义 TOK_ADDC1    0xc3 /* add with carry generation */
#定义 TOK_ADDC2    0xc4 /* add with carry use */
#定义 TOK_SUBC1    0xc5 /* add with carry generation */
#定义 TOK_SUBC2    0xc6 /* add with carry use */
#定义 TOK_ARROW    0xc7
#定义 TOK_DOTS     0xc8 /* three dots */
#定义 TOK_SHR      0xc9 /* unsigned shift right */
#定义 TOK_TWOSHARPS 0xca /* ## preprocessing token */
#定义 TOK_PLCHLDR  0xcb /* placeholder token as 已定义 in C99 */
#定义 TOK_NOSUBST  0xcc /* means following token has already been pp'd */
#定义 TOK_PPJOIN   0xcd /* A '##' in the right position to mean pasting */

#定义 TOK_CLONG   0xce /* long constant */
#定义 TOK_CULONG  0xcf /* unsigned long constant */


#如 已定义 TCC_TARGET_X86_64 && !已定义 TCC_TARGET_PE
    #定义 TCC_LONG_ARE_64_BIT
#了如


#定义 TOK_SHL   0x01 /* shift left */
#定义 TOK_SAR   0x02 /* signed shift right */
  
/* assignment operators : normal operator or 0x80 */
#定义 TOK_A_MOD 0xa5
#定义 TOK_A_AND 0xa6
#定义 TOK_A_MUL 0xaa
#定义 TOK_A_ADD 0xab
#定义 TOK_A_SUB 0xad
#定义 TOK_A_DIV 0xaf
#定义 TOK_A_XOR 0xde
#定义 TOK_A_OR  0xfc
#定义 TOK_A_SHL 0x81
#定义 TOK_A_SAR 0x82

#定义 TOK_EOF       (-1)  /* end of file */
#定义 TOK_LINEFEED  10    /* line feed */

/* all identifiers and strings have token above that */
#定义 TOK_IDENT 256

#定义 DEF_ASM(x) DEF(TOK_ASM_ ## x, #x)
#定义 TOK_ASM_int TOK_INT
#定义 DEF_ASMDIR(x) DEF(TOK_ASMDIR_ ## x, "." #x)
#定义 TOK_ASMDIR_FIRST TOK_ASMDIR_byte
#定义 TOK_ASMDIR_LAST TOK_ASMDIR_section

#如 已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64
/* only used for i386 asm opcodes definitions */
#定义 DEF_BWL(x) \
 DEF(TOK_ASM_ ## x ## b, #x "b") \
 DEF(TOK_ASM_ ## x ## w, #x "w") \
 DEF(TOK_ASM_ ## x ## l, #x "l") \
 DEF(TOK_ASM_ ## x, #x)
#定义 DEF_WL(x) \
 DEF(TOK_ASM_ ## x ## w, #x "w") \
 DEF(TOK_ASM_ ## x ## l, #x "l") \
 DEF(TOK_ASM_ ## x, #x)
#如定义 TCC_TARGET_X86_64
# 定义 DEF_BWLQ(x) \
 DEF(TOK_ASM_ ## x ## b, #x "b") \
 DEF(TOK_ASM_ ## x ## w, #x "w") \
 DEF(TOK_ASM_ ## x ## l, #x "l") \
 DEF(TOK_ASM_ ## x ## q, #x "q") \
 DEF(TOK_ASM_ ## x, #x)
# 定义 DEF_WLQ(x) \
 DEF(TOK_ASM_ ## x ## w, #x "w") \
 DEF(TOK_ASM_ ## x ## l, #x "l") \
 DEF(TOK_ASM_ ## x ## q, #x "q") \
 DEF(TOK_ASM_ ## x, #x)
# 定义 DEF_BWLX DEF_BWLQ
# 定义 DEF_WLX DEF_WLQ
/* number of sizes + 1 */
# 定义 NBWLX 5
#另
# 定义 DEF_BWLX DEF_BWL
# 定义 DEF_WLX DEF_WL
/* number of sizes + 1 */
# 定义 NBWLX 4
#了如

#定义 DEF_FP1(x) \
 DEF(TOK_ASM_ ## f ## x ## s, "f" #x "s") \
 DEF(TOK_ASM_ ## fi ## x ## l, "fi" #x "l") \
 DEF(TOK_ASM_ ## f ## x ## l, "f" #x "l") \
 DEF(TOK_ASM_ ## fi ## x ## s, "fi" #x "s")

#定义 DEF_FP(x) \
 DEF(TOK_ASM_ ## f ## x, "f" #x ) \
 DEF(TOK_ASM_ ## f ## x ## p, "f" #x "p") \
 DEF_FP1(x)

#定义 DEF_ASMTEST(x,suffix) \
 DEF_ASM(x ## o ## suffix) \
 DEF_ASM(x ## no ## suffix) \
 DEF_ASM(x ## b ## suffix) \
 DEF_ASM(x ## c ## suffix) \
 DEF_ASM(x ## nae ## suffix) \
 DEF_ASM(x ## nb ## suffix) \
 DEF_ASM(x ## nc ## suffix) \
 DEF_ASM(x ## ae ## suffix) \
 DEF_ASM(x ## e ## suffix) \
 DEF_ASM(x ## z ## suffix) \
 DEF_ASM(x ## ne ## suffix) \
 DEF_ASM(x ## nz ## suffix) \
 DEF_ASM(x ## be ## suffix) \
 DEF_ASM(x ## na ## suffix) \
 DEF_ASM(x ## nbe ## suffix) \
 DEF_ASM(x ## a ## suffix) \
 DEF_ASM(x ## s ## suffix) \
 DEF_ASM(x ## ns ## suffix) \
 DEF_ASM(x ## p ## suffix) \
 DEF_ASM(x ## pe ## suffix) \
 DEF_ASM(x ## np ## suffix) \
 DEF_ASM(x ## po ## suffix) \
 DEF_ASM(x ## l ## suffix) \
 DEF_ASM(x ## nge ## suffix) \
 DEF_ASM(x ## nl ## suffix) \
 DEF_ASM(x ## ge ## suffix) \
 DEF_ASM(x ## le ## suffix) \
 DEF_ASM(x ## ng ## suffix) \
 DEF_ASM(x ## nle ## suffix) \
 DEF_ASM(x ## g ## suffix)

#了如 /* 已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64 */

枚举 tcc_token {
    TOK_LAST = TOK_IDENT - 1
#定义 DEF(id, str) ,id
#包含 "tcctok.h"
#消定义 DEF
};

/* keywords: tok >= TOK_IDENT && tok < TOK_UIDENT */
#定义 TOK_UIDENT TOK_DEFINE

/* ------------ libtcc.c ------------ */

/* use GNU C extensions */
ST_DATA 整 gnu_ext;
/* use Tiny C extensions */
ST_DATA 整 tcc_ext;
/* XXX: get rid of this ASAP */
ST_DATA 结构 TCCState *tcc_state;

/* public functions currently used by the tcc main function */
ST_FUNC 字 *pstrcpy(字 *buf, 整 buf_size, 不变 字 *s);
ST_FUNC 字 *pstrcat(字 *buf, 整 buf_size, 不变 字 *s);
ST_FUNC 字 *pstrncpy(字 *out, 不变 字 *in, size_t num);
PUB_FUNC 字 *tcc_basename(不变 字 *name);
PUB_FUNC 字 *tcc_fileextension (不变 字 *name);

#如未定义 MEM_DEBUG
PUB_FUNC 空 tcc_free(空 *ptr);
PUB_FUNC 空 *tcc_malloc(无符 长 size);
PUB_FUNC 空 *tcc_mallocz(无符 长 size);
PUB_FUNC 空 *tcc_realloc(空 *ptr, 无符 长 size);
PUB_FUNC 字 *tcc_strdup(不变 字 *str);
#另
#定义 tcc_free(ptr)           tcc_free_debug(ptr)
#定义 tcc_malloc(size)        tcc_malloc_debug(size, __文件__, __行号__)
#定义 tcc_mallocz(size)       tcc_mallocz_debug(size, __文件__, __行号__)
#定义 tcc_realloc(ptr,size)   tcc_realloc_debug(ptr, size, __文件__, __行号__)
#定义 tcc_strdup(str)         tcc_strdup_debug(str, __文件__, __行号__)
PUB_FUNC 空 tcc_free_debug(空 *ptr);
PUB_FUNC 空 *tcc_malloc_debug(无符 长 size, 不变 字 *file, 整 line);
PUB_FUNC 空 *tcc_mallocz_debug(无符 长 size, 不变 字 *file, 整 line);
PUB_FUNC 空 *tcc_realloc_debug(空 *ptr, 无符 长 size, 不变 字 *file, 整 line);
PUB_FUNC 字 *tcc_strdup_debug(不变 字 *str, 不变 字 *file, 整 line);
#了如

#定义 free(p) use_tcc_free(p)
#定义 malloc(s) use_tcc_malloc(s)
#定义 realloc(p, s) use_tcc_realloc(p, s)
#消定义 strdup
#定义 strdup(s) use_tcc_strdup(s)
PUB_FUNC 空 tcc_memcheck(空);
PUB_FUNC 空 tcc_error_noabort(不变 字 *fmt, ...);
PUB_FUNC NORETURN 空 tcc_error(不变 字 *fmt, ...);
PUB_FUNC 空 tcc_warning(不变 字 *fmt, ...);

/* other utilities */
ST_FUNC 空 dynarray_add(空 *ptab, 整 *nb_ptr, 空 *data);
ST_FUNC 空 dynarray_reset(空 *pp, 整 *n);
ST_INLN 空 cstr_ccat(CString *cstr, 整 ch);
ST_FUNC 空 cstr_cat(CString *cstr, 不变 字 *str, 整 len);
ST_FUNC 空 cstr_wccat(CString *cstr, 整 ch);
ST_FUNC 空 cstr_new(CString *cstr);
ST_FUNC 空 cstr_free(CString *cstr);
ST_FUNC 空 cstr_reset(CString *cstr);

ST_INLN 空 sym_free(Sym *sym);
ST_FUNC Sym *sym_push2(Sym **ps, 整 v, 整 t, 整 c);
ST_FUNC Sym *sym_find2(Sym *s, 整 v);
ST_FUNC Sym *sym_push(整 v, CType *type, 整 r, 整 c);
ST_FUNC 空 sym_pop(Sym **ptop, Sym *b, 整 keep);
ST_INLN Sym *struct_find(整 v);
ST_INLN Sym *sym_find(整 v);
ST_FUNC Sym *global_identifier_push(整 v, 整 t, 整 c);

ST_FUNC 空 tcc_open_bf(TCCState *s1, 不变 字 *filename, 整 initlen);
ST_FUNC 整 tcc_open(TCCState *s1, 不变 字 *filename);
ST_FUNC 空 tcc_close(空);

ST_FUNC 整 tcc_add_file_internal(TCCState *s1, 不变 字 *filename, 整 flags);
/* flags: */
#定义 AFF_PRINT_ERROR     0x10 /* print error if file not found */
#定义 AFF_REFERENCED_DLL  0x20 /* load a referenced dll from another dll */
#定义 AFF_TYPE_BIN        0x40 /* file to add is binary */
/* s->filetype: */
#定义 AFF_TYPE_NONE   0
#定义 AFF_TYPE_C      1
#定义 AFF_TYPE_ASM    2
#定义 AFF_TYPE_ASMPP  3
#定义 AFF_TYPE_LIB    4
/* values from tcc_object_type(...) */
#定义 AFF_BINTYPE_REL 1
#定义 AFF_BINTYPE_DYN 2
#定义 AFF_BINTYPE_AR  3
#定义 AFF_BINTYPE_C67 4


ST_FUNC 整 tcc_add_crt(TCCState *s, 不变 字 *filename);
ST_FUNC 整 tcc_add_dll(TCCState *s, 不变 字 *filename, 整 flags);
ST_FUNC 空 tcc_add_pragma_libs(TCCState *s1);
PUB_FUNC 整 tcc_add_library_err(TCCState *s, 不变 字 *f);
PUB_FUNC 空 tcc_print_stats(TCCState *s, 无符 total_time);
PUB_FUNC 整 tcc_parse_args(TCCState *s, 整 *argc, 字 ***argv, 整 optind);
#如定义 _WIN32
ST_FUNC 字 *normalize_slashes(字 *path);
#了如

/* tcc_parse_args return codes: */
#定义 OPT_HELP 1
#定义 OPT_HELP2 2
#定义 OPT_V 3
#定义 OPT_PRINT_DIRS 4
#定义 OPT_AR 5
#定义 OPT_IMPDEF 6
#定义 OPT_M32 32
#定义 OPT_M64 64

/* ------------ tccpp.c ------------ */

ST_DATA 结构 BufferedFile *file;
ST_DATA 整 ch, tok;
ST_DATA CValue tokc;
ST_DATA 不变 整 *macro_ptr;
ST_DATA 整 parse_flags;
ST_DATA 整 tok_flags;
ST_DATA CString tokcstr; /* current parsed string, if any */

/* display benchmark infos */
ST_DATA 整 total_lines;
ST_DATA 整 total_bytes;
ST_DATA 整 tok_ident;
ST_DATA TokenSym **table_ident;

#定义 TOK_FLAG_BOL   0x0001 /* beginning of line before */
#定义 TOK_FLAG_BOF   0x0002 /* beginning of file before */
#定义 TOK_FLAG_ENDIF 0x0004 /* a endif was found matching starting #ifdef */
#定义 TOK_FLAG_EOF   0x0008 /* end of file */

#定义 PARSE_FLAG_PREPROCESS 0x0001 /* activate preprocessing */
#定义 PARSE_FLAG_TOK_NUM    0x0002 /* return numbers instead of TOK_PPNUM */
#定义 PARSE_FLAG_LINEFEED   0x0004 /* line feed is returned as a
                                        token. line feed is also
                                        returned at eof */
#定义 PARSE_FLAG_ASM_FILE 0x0008 /* we processing an asm file: '#' can be used for line comment, etc. */
#定义 PARSE_FLAG_SPACES     0x0010 /* next() returns space tokens (for -E) */
#定义 PARSE_FLAG_ACCEPT_STRAYS 0x0020 /* next() returns '\\' token */
#定义 PARSE_FLAG_TOK_STR    0x0040 /* return parsed strings instead of TOK_PPSTR */

/* isidnum_table flags: */
#定义 IS_SPC 1
#定义 IS_ID  2
#定义 IS_NUM 4

ST_FUNC TokenSym *tok_alloc(不变 字 *str, 整 len);
ST_FUNC 不变 字 *get_tok_str(整 v, CValue *cv);
ST_FUNC 空 begin_macro(TokenString *str, 整 alloc);
ST_FUNC 空 end_macro(空);
ST_FUNC 整 set_idnum(整 c, 整 val);
ST_INLN 空 tok_str_new(TokenString *s);
ST_FUNC TokenString *tok_str_alloc(空);
ST_FUNC 空 tok_str_free(TokenString *s);
ST_FUNC 空 tok_str_free_str(整 *str);
ST_FUNC 空 tok_str_add(TokenString *s, 整 t);
ST_FUNC 空 tok_str_add_tok(TokenString *s);
ST_INLN 空 define_push(整 v, 整 macro_type, 整 *str, Sym *first_arg);
ST_FUNC 空 define_undef(Sym *s);
ST_INLN Sym *define_find(整 v);
ST_FUNC 空 free_defines(Sym *b);
ST_FUNC Sym *label_find(整 v);
ST_FUNC Sym *label_push(Sym **ptop, 整 v, 整 flags);
ST_FUNC 空 label_pop(Sym **ptop, Sym *slast, 整 keep);
ST_FUNC 空 parse_define(空);
ST_FUNC 空 preprocess(整 is_bof);
ST_FUNC 空 next_nomacro(空);
ST_FUNC 空 next(空);
ST_INLN 空 unget_tok(整 last_tok);
ST_FUNC 空 preprocess_start(TCCState *s1, 整 is_asm);
ST_FUNC 空 preprocess_end(TCCState *s1);
ST_FUNC 空 tccpp_new(TCCState *s);
ST_FUNC 空 tccpp_delete(TCCState *s);
ST_FUNC 整 tcc_preprocess(TCCState *s1);
ST_FUNC 空 skip(整 c);
ST_FUNC NORETURN 空 expect(不变 字 *msg);

/* space excluding newline */
静态 内联 整 is_space(整 ch) {
    返回 ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f' || ch == '\r';
}
静态 内联 整 isid(整 c) {
    返回 (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
静态 内联 整 isnum(整 c) {
    返回 c >= '0' && c <= '9';
}
静态 内联 整 isoct(整 c) {
    返回 c >= '0' && c <= '7';
}
静态 内联 整 toup(整 c) {
    返回 (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
}

/* ------------ tccgen.c ------------ */

#定义 SYM_POOL_NB (8192 / 求长度(Sym))
ST_DATA Sym *sym_free_first;
ST_DATA 空 **sym_pools;
ST_DATA 整 nb_sym_pools;

ST_DATA Sym *global_stack;
ST_DATA Sym *local_stack;
ST_DATA Sym *local_label_stack;
ST_DATA Sym *global_label_stack;
ST_DATA Sym *define_stack;
ST_DATA CType char_pointer_type, func_old_type, int_type, size_type;
ST_DATA SValue __vstack[1+/*to make bcheck happy*/ VSTACK_SIZE], *vtop, *pvtop;
#定义 vstack  (__vstack + 1)
ST_DATA 整 rsym, anon_sym, ind, loc;

ST_DATA 整 const_wanted; /* true if constant wanted */
ST_DATA 整 nocode_wanted; /* true if no code generation wanted for an expression */
ST_DATA 整 global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
ST_DATA CType func_vt; /* current function return type (used by return instruction) */
ST_DATA 整 func_var; /* true if current function is variadic */
ST_DATA 整 func_vc;
ST_DATA 整 last_line_num, last_ind, func_ind; /* debug last line number and pc */
ST_DATA 不变 字 *funcname;
ST_DATA 整 g_debug;

ST_FUNC 空 tcc_debug_start(TCCState *s1);
ST_FUNC 空 tcc_debug_end(TCCState *s1);
ST_FUNC 空 tcc_debug_funcstart(TCCState *s1, Sym *sym);
ST_FUNC 空 tcc_debug_funcend(TCCState *s1, 整 size);
ST_FUNC 空 tcc_debug_line(TCCState *s1);

ST_FUNC 整 tccgen_compile(TCCState *s1);
ST_FUNC 空 free_inline_functions(TCCState *s);
ST_FUNC 空 check_vstack(空);

ST_INLN 整 is_float(整 t);
ST_FUNC 整 ieee_finite(双精 d);
ST_FUNC 空 test_lvalue(空);
ST_FUNC 空 vpushi(整 v);
ST_FUNC Sym *external_global_sym(整 v, CType *type, 整 r);
ST_FUNC 空 vset(CType *type, 整 r, 整 v);
ST_FUNC 空 vswap(空);
ST_FUNC 空 vpush_global_sym(CType *type, 整 v);
ST_FUNC 空 vrote(SValue *e, 整 n);
ST_FUNC 空 vrott(整 n);
ST_FUNC 空 vrotb(整 n);
#如定义 TCC_TARGET_ARM
ST_FUNC 整 get_reg_ex(整 rc, 整 rc2);
ST_FUNC 空 lexpand_nr(空);
#了如
ST_FUNC 空 vpushv(SValue *v);
ST_FUNC 空 save_reg(整 r);
ST_FUNC 空 save_reg_upstack(整 r, 整 n);
ST_FUNC 整 get_reg(整 rc);
ST_FUNC 空 save_regs(整 n);
ST_FUNC 空 gaddrof(空);
ST_FUNC 整 gv(整 rc);
ST_FUNC 空 gv2(整 rc1, 整 rc2);
ST_FUNC 空 vpop(空);
ST_FUNC 空 gen_op(整 op);
ST_FUNC 整 type_size(CType *type, 整 *a);
ST_FUNC 空 mk_pointer(CType *type);
ST_FUNC 空 vstore(空);
ST_FUNC 空 inc(整 post, 整 c);
ST_FUNC 空 parse_mult_str (CString *astr, 不变 字 *msg);
ST_FUNC 空 parse_asm_str(CString *astr);
ST_FUNC 整 lvalue_type(整 t);
ST_FUNC 空 indir(空);
ST_FUNC 空 unary(空);
ST_FUNC 空 expr_prod(空);
ST_FUNC 空 expr_sum(空);
ST_FUNC 空 gexpr(空);
ST_FUNC 整 expr_const(空);
#如 已定义 CONFIG_TCC_BCHECK || 已定义 TCC_TARGET_C67
ST_FUNC Sym *get_sym_ref(CType *type, Section *sec, 无符 长 offset, 无符 长 size);
#了如
#如 已定义 TCC_TARGET_X86_64 && !已定义 TCC_TARGET_PE
ST_FUNC 整 classify_x86_64_va_arg(CType *ty);
#了如

/* ------------ tccelf.c ------------ */

#定义 TCC_OUTPUT_FORMAT_ELF    0 /* default output format: ELF */
#定义 TCC_OUTPUT_FORMAT_BINARY 1 /* binary image output */
#定义 TCC_OUTPUT_FORMAT_COFF   2 /* COFF */

#定义 ARMAG  "!<arch>\012"    /* For COFF and a.out archives */

类型定义 结构 {
    无符 整 n_strx;         /* index into string table of name */
    无符 字 n_type;         /* type of symbol */
    无符 字 n_other;        /* misc info (usually empty) */
    无符 短 n_desc;        /* description field */
    无符 整 n_value;        /* value of symbol */
} Stab_Sym;

ST_DATA Section *text_section, *data_section, *bss_section; /* predefined sections */
ST_DATA Section *common_section;
ST_DATA Section *cur_text_section; /* current section where function code is generated */
#如定义 CONFIG_TCC_ASM
ST_DATA Section *last_text_section; /* to handle .previous asm directive */
#了如
#如定义 CONFIG_TCC_BCHECK
/* bound check related sections */
ST_DATA Section *bounds_section; /* contains global data bound description */
ST_DATA Section *lbounds_section; /* contains local data bound description */
ST_FUNC 空 tccelf_bounds_new(TCCState *s);
#了如
/* symbol sections */
ST_DATA Section *symtab_section, *strtab_section;
/* debug sections */
ST_DATA Section *stab_section, *stabstr_section;

ST_FUNC 空 tccelf_new(TCCState *s);
ST_FUNC 空 tccelf_delete(TCCState *s);
ST_FUNC 空 tccelf_stab_new(TCCState *s);

ST_FUNC Section *new_section(TCCState *s1, 不变 字 *name, 整 sh_type, 整 sh_flags);
ST_FUNC 空 section_realloc(Section *sec, 无符 长 new_size);
ST_FUNC size_t section_add(Section *sec, addr_t size, 整 align);
ST_FUNC 空 *section_ptr_add(Section *sec, addr_t size);
ST_FUNC 空 section_reserve(Section *sec, 无符 长 size);
ST_FUNC Section *find_section(TCCState *s1, 不变 字 *name);
ST_FUNC Section *new_symtab(TCCState *s1, 不变 字 *symtab_name, 整 sh_type, 整 sh_flags, 不变 字 *strtab_name, 不变 字 *hash_name, 整 hash_sh_flags);

ST_FUNC 空 put_extern_sym2(Sym *sym, Section *section, addr_t value, 无符 长 size, 整 can_add_underscore);
ST_FUNC 空 put_extern_sym(Sym *sym, Section *section, addr_t value, 无符 长 size);
#如 PTR_SIZE == 4
ST_FUNC 空 greloc(Section *s, Sym *sym, 无符 长 offset, 整 type);
#了如
ST_FUNC 空 greloca(Section *s, Sym *sym, 无符 长 offset, 整 type, addr_t addend);

ST_FUNC 整 put_elf_str(Section *s, 不变 字 *sym);
ST_FUNC 整 put_elf_sym(Section *s, addr_t value, 无符 长 size, 整 info, 整 other, 整 shndx, 不变 字 *name);
ST_FUNC 整 set_elf_sym(Section *s, addr_t value, 无符 长 size, 整 info, 整 other, 整 shndx, 不变 字 *name);
ST_FUNC 整 find_elf_sym(Section *s, 不变 字 *name);
ST_FUNC 空 put_elf_reloc(Section *symtab, Section *s, 无符 长 offset, 整 type, 整 symbol);
ST_FUNC 空 put_elf_reloca(Section *symtab, Section *s, 无符 长 offset, 整 type, 整 symbol, addr_t addend);

ST_FUNC 空 put_stabs(不变 字 *str, 整 type, 整 other, 整 desc, 无符 长 value);
ST_FUNC 空 put_stabs_r(不变 字 *str, 整 type, 整 other, 整 desc, 无符 长 value, Section *sec, 整 sym_index);
ST_FUNC 空 put_stabn(整 type, 整 other, 整 desc, 整 value);
ST_FUNC 空 put_stabd(整 type, 整 other, 整 desc);

ST_FUNC 空 relocate_common_syms(空);
ST_FUNC 空 relocate_syms(TCCState *s1, Section *symtab, 整 do_resolve);
ST_FUNC 空 relocate_section(TCCState *s1, Section *s);

ST_FUNC 空 tcc_add_linker_symbols(TCCState *s1);
ST_FUNC 整 tcc_object_type(整 fd, ElfW(Ehdr) *h);
ST_FUNC 整 tcc_load_object_file(TCCState *s1, 整 fd, 无符 长 file_offset);
ST_FUNC 整 tcc_load_archive(TCCState *s1, 整 fd);
ST_FUNC 空 tcc_add_bcheck(TCCState *s1);
ST_FUNC 空 tcc_add_runtime(TCCState *s1);

ST_FUNC 空 build_got_entries(TCCState *s1);
ST_FUNC 结构 sym_attr *get_sym_attr(TCCState *s1, 整 index, 整 alloc);
ST_FUNC 空 squeeze_multi_relocs(Section *sec, size_t oldrelocoffset);

ST_FUNC addr_t get_elf_sym_addr(TCCState *s, 不变 字 *name, 整 err);
#如 已定义 TCC_IS_NATIVE || 已定义 TCC_TARGET_PE
ST_FUNC 空 *tcc_get_symbol_err(TCCState *s, 不变 字 *name);
#了如

#如未定义 TCC_TARGET_PE
ST_FUNC 整 tcc_load_dll(TCCState *s1, 整 fd, 不变 字 *filename, 整 level);
ST_FUNC 整 tcc_load_ldscript(TCCState *s1);
ST_FUNC uint8_t *parse_comment(uint8_t *p);
ST_FUNC 空 minp(空);
ST_INLN 空 inp(空);
ST_FUNC 整 handle_eob(空);
#了如

/* ------------ xxx-link.c ------------ */

/* Wether to generate a GOT/PLT entry and when. NO_GOTPLT_ENTRY is first so
   that unknown relocation don't create a GOT or PLT entry */
枚举 gotplt_entry {
    NO_GOTPLT_ENTRY,	/* never generate (eg. GLOB_DAT & JMP_SLOT relocs) */
    BUILD_GOT_ONLY,	/* only build GOT (eg. TPOFF relocs) */
    AUTO_GOTPLT_ENTRY,	/* generate if sym is UNDEF */
    ALWAYS_GOTPLT_ENTRY	/* always generate (eg. PLTOFF relocs) */
};

ST_FUNC 整 code_reloc (整 reloc_type);
ST_FUNC 整 gotplt_entry_type (整 reloc_type);
ST_FUNC 无符 create_plt_entry(TCCState *s1, 无符 got_offset, 结构 sym_attr *attr);
ST_FUNC 空 relocate_init(Section *sr);
ST_FUNC 空 relocate(TCCState *s1, ElfW_Rel *rel, 整 type, 无符 字 *ptr, addr_t addr, addr_t val);
ST_FUNC 空 relocate_plt(TCCState *s1);

/* ------------ xxx-gen.c ------------ */

ST_DATA 不变 整 reg_classes[NB_REGS];

ST_FUNC 空 gsym_addr(整 t, 整 a);
ST_FUNC 空 gsym(整 t);
ST_FUNC 空 load(整 r, SValue *sv);
ST_FUNC 空 store(整 r, SValue *v);
ST_FUNC 整 gfunc_sret(CType *vt, 整 variadic, CType *ret, 整 *align, 整 *regsize);
ST_FUNC 空 gfunc_call(整 nb_args);
ST_FUNC 空 gfunc_prolog(CType *func_type);
ST_FUNC 空 gfunc_epilog(空);
ST_FUNC 整 gjmp(整 t);
ST_FUNC 空 gjmp_addr(整 a);
ST_FUNC 整 gtst(整 inv, 整 t);
#如 已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64
ST_FUNC 空 gtst_addr(整 inv, 整 a);
#另
#定义 gtst_addr(inv, a) gsym_addr(gtst(inv, 0), a)
#了如
ST_FUNC 空 gen_opi(整 op);
ST_FUNC 空 gen_opf(整 op);
ST_FUNC 空 gen_cvt_ftoi(整 t);
ST_FUNC 空 gen_cvt_ftof(整 t);
ST_FUNC 空 ggoto(空);
#如未定义 TCC_TARGET_C67
ST_FUNC 空 o(无符 整 c);
#了如
#如未定义 TCC_TARGET_ARM
ST_FUNC 空 gen_cvt_itof(整 t);
#了如
ST_FUNC 空 gen_vla_sp_save(整 addr);
ST_FUNC 空 gen_vla_sp_restore(整 addr);
ST_FUNC 空 gen_vla_alloc(CType *type, 整 align);

静态 内联 uint16_t read16le(无符 字 *p) {
    返回 p[0] | (uint16_t)p[1] << 8;
}
静态 内联 空 write16le(无符 字 *p, uint16_t x) {
    p[0] = x & 255, p[1] = x >> 8 & 255;
}
静态 内联 uint32_t read32le(无符 字 *p) {
  返回 read16le(p) | (uint32_t)read16le(p + 2) << 16;
}
静态 内联 空 write32le(无符 字 *p, uint32_t x) {
    write16le(p, x), write16le(p + 2, x >> 16);
}
静态 内联 空 add32le(无符 字 *p, int32_t x) {
    write32le(p, read32le(p) + x);
}
静态 内联 uint64_t read64le(无符 字 *p) {
  返回 read32le(p) | (uint64_t)read32le(p + 4) << 32;
}
静态 内联 空 write64le(无符 字 *p, uint64_t x) {
    write32le(p, x), write32le(p + 4, x >> 32);
}
静态 内联 空 add64le(无符 字 *p, int64_t x) {
    write64le(p, read64le(p) + x);
}

/* ------------ i386-gen.c ------------ */
#如 已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64
ST_FUNC 空 g(整 c);
ST_FUNC 空 gen_le16(整 c);
ST_FUNC 空 gen_le32(整 c);
ST_FUNC 空 gen_addr32(整 r, Sym *sym, 整 c);
ST_FUNC 空 gen_addrpc32(整 r, Sym *sym, 整 c);
#了如

#如定义 CONFIG_TCC_BCHECK
ST_FUNC 空 gen_bounded_ptr_add(空);
ST_FUNC 空 gen_bounded_ptr_deref(空);
#了如

/* ------------ x86_64-gen.c ------------ */
#如定义 TCC_TARGET_X86_64
ST_FUNC 空 gen_addr64(整 r, Sym *sym, int64_t c);
ST_FUNC 空 gen_opl(整 op);
#了如

/* ------------ arm-gen.c ------------ */
#如定义 TCC_TARGET_ARM
#如 已定义(TCC_ARM_EABI) && !已定义(CONFIG_TCC_ELFINTERP)
PUB_FUNC 不变 字 *default_elfinterp(结构 TCCState *s);
#了如
ST_FUNC 空 arm_init(结构 TCCState *s);
ST_FUNC 空 gen_cvt_itof1(整 t);
#了如

/* ------------ arm64-gen.c ------------ */
#如定义 TCC_TARGET_ARM64
ST_FUNC 空 gen_cvt_sxtw(空);
ST_FUNC 空 gen_opl(整 op);
ST_FUNC 空 gfunc_return(CType *func_type);
ST_FUNC 空 gen_va_start(空);
ST_FUNC 空 gen_va_arg(CType *t);
ST_FUNC 空 gen_clear_cache(空);
#了如

/* ------------ c67-gen.c ------------ */
#如定义 TCC_TARGET_C67
#了如

/* ------------ tcccoff.c ------------ */

#如定义 TCC_TARGET_COFF
ST_FUNC 整 tcc_output_coff(TCCState *s1, FILE *f);
ST_FUNC 整 tcc_load_coff(TCCState * s1, 整 fd);
#了如

/* ------------ tccasm.c ------------ */
ST_FUNC 空 asm_instr(空);
ST_FUNC 空 asm_global_instr(空);
#如定义 CONFIG_TCC_ASM
ST_FUNC 整 find_constraint(ASMOperand *operands, 整 nb_operands, 不变 字 *name, 不变 字 **pp);
ST_FUNC Sym* get_asm_sym(整 name, Sym *csym);
ST_FUNC 空 asm_expr(TCCState *s1, ExprValue *pe);
ST_FUNC 整 asm_int_expr(TCCState *s1);
ST_FUNC 整 tcc_assemble(TCCState *s1, 整 do_preprocess);
/* ------------ i386-asm.c ------------ */
ST_FUNC 空 gen_expr32(ExprValue *pe);
#如定义 TCC_TARGET_X86_64
ST_FUNC 空 gen_expr64(ExprValue *pe);
#了如
ST_FUNC 空 asm_opcode(TCCState *s1, 整 opcode);
ST_FUNC 整 asm_parse_regvar(整 t);
ST_FUNC 空 asm_compute_constraints(ASMOperand *operands, 整 nb_operands, 整 nb_outputs, 不变 uint8_t *clobber_regs, 整 *pout_reg);
ST_FUNC 空 subst_asm_operand(CString *add_str, SValue *sv, 整 modifier);
ST_FUNC 空 asm_gen_code(ASMOperand *operands, 整 nb_operands, 整 nb_outputs, 整 is_output, uint8_t *clobber_regs, 整 out_reg);
ST_FUNC 空 asm_clobber(uint8_t *clobber_regs, 不变 字 *str);
#了如

/* ------------ tccpe.c -------------- */
#如定义 TCC_TARGET_PE
ST_FUNC 整 pe_load_file(结构 TCCState *s1, 不变 字 *filename, 整 fd);
ST_FUNC 整 pe_output_file(TCCState * s1, 不变 字 *filename);
ST_FUNC 整 pe_putimport(TCCState *s1, 整 dllindex, 不变 字 *name, addr_t value);
#如未定义 TCC_TARGET_ARM
ST_FUNC SValue *pe_getimport(SValue *sv, SValue *v2);
#了如
#如定义 TCC_TARGET_X86_64
ST_FUNC 空 pe_add_unwind_data(无符 start, 无符 end, 无符 stack);
#了如
PUB_FUNC 整 tcc_get_dllexports(不变 字 *filename, 字 **pp);
/* symbol properties stored in Elf32_Sym->st_other */
# 定义 ST_PE_EXPORT 0x10
# 定义 ST_PE_IMPORT 0x20
# 定义 ST_PE_STDCALL 0x40
#了如

/* ------------ tccrun.c ----------------- */
#如定义 TCC_IS_NATIVE
#如定义 CONFIG_TCC_STATIC
#定义 RTLD_LAZY       0x001
#定义 RTLD_NOW        0x002
#定义 RTLD_GLOBAL     0x100
#定义 RTLD_DEFAULT    NULL
/* dummy function for profiling */
ST_FUNC 空 *dlopen(不变 字 *filename, 整 flag);
ST_FUNC 空 dlclose(空 *p);
ST_FUNC 不变 字 *dlerror(空);
ST_FUNC 空 *dlsym(空 *handle, 不变 字 *symbol);
#了如
#如定义 CONFIG_TCC_BACKTRACE
ST_DATA 整 rt_num_callers;
ST_DATA 不变 字 **rt_bound_error_msg;
ST_DATA 空 *rt_prog_main;
ST_FUNC 空 tcc_set_num_callers(整 n);
#了如
ST_FUNC 空 tcc_run_free(TCCState *s1);
#了如

/* ------------ tcctools.c ----------------- */
#如 0 /* included in tcc.c */
ST_FUNC 整 tcc_tool_ar(TCCState *s, 整 argc, 字 **argv);
#如定义 TCC_TARGET_PE
ST_FUNC 整 tcc_tool_impdef(TCCState *s, 整 argc, 字 **argv);
#了如
ST_FUNC 空 tcc_tool_cross(TCCState *s, 字 **argv, 整 option);
ST_FUNC 空 gen_makedeps(TCCState *s, 不变 字 *target, 不变 字 *filename);
#了如

/********************************************************/
#消定义 ST_DATA
#如 ONE_SOURCE
#定义 ST_DATA 静态
#另
#定义 ST_DATA
#了如
/********************************************************/
#了如 /* _TCC_H */
