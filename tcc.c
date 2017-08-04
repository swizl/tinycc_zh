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
#如 ONE_SOURCE
# 包含 "libtcc.c"
#了如
#包含 "tcctools.c"

静态 不变 字 help[] =
    "Tiny C Compiler "TCC_VERSION" - Copyright (C) 2001-2006 Fabrice Bellard\n"
    "Usage: tcc [options...] [-o outfile] [-c] infile(s)...\n"
    "       tcc [options...] -run infile [arguments...]\n"
    "General options:\n"
    "  -c          compile only - generate an object file\n"
    "  -o outfile  set output filename\n"
    "  -run        run compiled source\n"
    "  -fflag      set or reset (with 'no-' prefix) 'flag' (see tcc -hh)\n"
    "  -Wwarning   set or reset (with 'no-' prefix) 'warning' (see tcc -hh)\n"
    "  -w          disable all warnings\n"
    "  -v -vv      show version, show search paths or loaded files\n"
    "  -h -hh      show this, show more help\n"
    "  -bench      show compilation statistics\n"
    "  -           use stdin pipe as infile\n"
    "  @listfile   read arguments from listfile\n"
    "Preprocessor options:\n"
    "  -Idir       add include path 'dir'\n"
    "  -Dsym[=val] define 'sym' with value 'val'\n"
    "  -Usym       undefine 'sym'\n"
    "  -E          preprocess only\n"
    "Linker options:\n"
    "  -Ldir       add library path 'dir'\n"
    "  -llib       link with dynamic or static library 'lib'\n"
    "  -r          generate (relocatable) object file\n"
    "  -shared     generate a shared library/dll\n"
    "  -rdynamic   export all global symbols to dynamic linker\n"
    "  -soname     set name for shared library to be used at runtime\n"
    "  -Wl,-opt[=val]  set linker option (see tcc -hh)\n"
    "Debugger options:\n"
    "  -g          generate runtime debug info\n"
#如定义 CONFIG_TCC_BCHECK
    "  -b          compile with built-in memory and bounds checker (implies -g)\n"
#了如
#如定义 CONFIG_TCC_BACKTRACE
    "  -bt N       show N callers in stack traces\n"
#了如
    "Misc. options:\n"
    "  -x[c|a|n]   specify type of the next infile\n"
    "  -nostdinc   do not use standard system include paths\n"
    "  -nostdlib   do not link with standard crt and libraries\n"
    "  -Bdir       set tcc's private include/library dir\n"
    "  -MD         generate dependency file for make\n"
    "  -MF file    specify dependency file name\n"
    "  -m32/64     defer to i386/x86_64 cross compiler\n"
    "Tools:\n"
    "  create library  : tcc -ar [rcsv] lib.a files\n"
#如定义 TCC_TARGET_PE
    "  create def file : tcc -impdef lib.dll [-v] [-o lib.def]\n"
#了如
    ;

静态 不变 字 help2[] =
    "Tiny C Compiler "TCC_VERSION" - More Options\n"
    "Special options:\n"
    "  -P -P1                        with -E: no/alternative #line output\n"
    "  -dD -dM                       with -E: output #define directives\n"
    "  -pthread                      same as -D_REENTRANT and -lpthread\n"
    "  -On                           same as -D__OPTIMIZE__ for n > 0\n"
    "  -Wp,-opt                      same as -opt\n"
    "  -include file                 include 'file' above each input file\n"
    "  -isystem dir                  add 'dir' to system include path\n"
    "  -iwithprefix dir              set tcc's private include/library subdir\n"
    "  -static                       link to static libraries (not recommended)\n"
    "  -dumpversion                  print version\n"
    "  -print-search-dirs            print search paths\n"
    "  -dt                           with -run/-E: auto-define 'test_...' macros\n"
    "Ignored options:\n"
    "  --param  -pedantic  -pipe  -s  -std  -traditional\n"
    "-W... warnings:\n"
    "  all                           turn on some (*) warnings\n"
    "  error                         stop after first warning\n"
    "  unsupported                   warn about ignored options, pragmas, etc.\n"
    "  write-strings                 strings are const\n"
    "  implicit-function-declaration warn for missing prototype (*)\n"
    "-f[no-]... flags:\n"
    "  unsigned-char                 default char is unsigned\n"
    "  signed-char                   default char is signed\n"
    "  common                        use common section instead of bss\n"
    "  leading-underscore            decorate extern symbols\n"
    "  ms-extensions                 allow anonymous struct in struct\n"
    "  dollars-in-identifiers        allow '$' in C symbols\n"
    "-m... target specific options:\n"
    "  ms-bitfields                  use MSVC bitfield layout\n"
#如定义 TCC_TARGET_ARM
    "  float-abi                     hard/softfp on arm\n"
#了如
#如定义 TCC_TARGET_X86_64
    "  no-sse                        disable floats on x86_64\n"
#了如
    "-Wl,... linker options:\n"
    "  -nostdlib                     do not link with standard crt/libs\n"
    "  -[no-]whole-archive           load lib(s) fully/only as needed\n"
    "  -export-all-symbols           same as -rdynamic\n"
    "  -image-base= -Ttext=          set base address of executable\n"
    "  -section-alignment=           set section alignment in executable\n"
#如定义 TCC_TARGET_PE
    "  -file-alignment=              set PE file alignment\n"
    "  -stack=                       set PE stack reserve\n"
    "  -large-address-aware          set related PE option\n"
    "  -subsystem=[console/windows]  set PE subsystem\n"
    "  -oformat=[pe-* binary]        set executable output format\n"
    "Predefined macros:\n"
    "  tcc -E -dM - < nul\n"
#另
    "  -rpath=                       set dynamic library search path\n"
    "  -enable-new-dtags             set DT_RUNPATH instead of DT_RPATH\n"
    "  -soname=                      set DT_SONAME elf tag\n"
    "  -Bsymbolic                    set DT_SYMBOLIC elf tag\n"
    "  -oformat=[elf32/64-* binary]  set executable output format\n"
    "  -init= -fini= -as-needed -O   (ignored)\n"
    "Predefined macros:\n"
    "  tcc -E -dM - < /dev/null\n"
#了如
    "See also the manual for more details.\n"
    ;

静态 不变 字 version[] =
    "tcc version "TCC_VERSION" ("
#如定义 TCC_TARGET_I386
        "i386"
#另如 已定义 TCC_TARGET_X86_64
        "x86_64"
#另如 已定义 TCC_TARGET_C67
        "C67"
#另如 已定义 TCC_TARGET_ARM
        "ARM"
#另如 已定义 TCC_TARGET_ARM64
        "AArch64"
#了如
#如定义 TCC_ARM_HARDFLOAT
        " Hard Float"
#了如
#如定义 TCC_TARGET_PE
        " Windows"
#另如 已定义(TCC_TARGET_MACHO)
        " Darwin"
#另如 已定义(__FreeBSD__) || 已定义(__FreeBSD_kernel__)
        " FreeBSD"
#另
        " Linux"
#了如
    ")\n"
    ;

静态 空 print_dirs(不变 字 *msg, 字 **paths, 整 nb_paths)
{
    整 i;
    printf("%s:\n%s", msg, nb_paths ? "" : "  -\n");
    对于(i = 0; i < nb_paths; i++)
        printf("  %s\n", paths[i]);
}

静态 空 print_search_dirs(TCCState *s)
{
    printf("install: %s\n", s->tcc_lib_path);
    /* print_dirs("programs", NULL, 0); */
    print_dirs("include", s->sysinclude_paths, s->nb_sysinclude_paths);
    print_dirs("libraries", s->library_paths, s->nb_library_paths);
#如未定义 TCC_TARGET_PE
    print_dirs("crt", s->crt_paths, s->nb_crt_paths);
    printf("libtcc1:\n  %s/"TCC_LIBTCC1"\n", s->tcc_lib_path);
    printf("elfinterp:\n  %s\n",  DEFAULT_ELFINTERP(s));
#了如
}

静态 空 set_environment(TCCState *s)
{
    字 * path;

    path = getenv("C_INCLUDE_PATH");
    如(path != NULL) {
        tcc_add_include_path(s, path);
    }
    path = getenv("CPATH");
    如(path != NULL) {
        tcc_add_include_path(s, path);
    }
    path = getenv("LIBRARY_PATH");
    如(path != NULL) {
        tcc_add_library_path(s, path);
    }
}

静态 字 *default_outputfile(TCCState *s, 不变 字 *first_file)
{
    字 buf[1024];
    字 *ext;
    不变 字 *name = "a";

    如 (first_file && strcmp(first_file, "-"))
        name = tcc_basename(first_file);
    snprintf(buf, 求长度(buf), "%s", name);
    ext = tcc_fileextension(buf);
#如定义 TCC_TARGET_PE
    如 (s->output_type == TCC_OUTPUT_DLL)
        strcpy(ext, ".dll");
    另
    如 (s->output_type == TCC_OUTPUT_EXE)
        strcpy(ext, ".exe");
    另
#了如
    如 (s->output_type == TCC_OUTPUT_OBJ && !s->option_r && *ext)
        strcpy(ext, ".o");
    另
        strcpy(buf, "a.out");
    返回 tcc_strdup(buf);
}

静态 无符 getclock_ms(空)
{
#如定义 _WIN32
    返回 GetTickCount();
#另
    结构 timeval tv;
    gettimeofday(&tv, NULL);
    返回 tv.tv_sec*1000 + (tv.tv_usec+500)/1000;
#了如
}

整 main(整 argc0, 字 **argv0)
{
    TCCState *s;
    整 ret, opt, n = 0, t = 0;
    无符 start_time = 0;
    不变 字 *first_file;
    整 argc; 字 **argv;
    FILE *ppfp = stdout;

redo:
    argc = argc0, argv = argv0;
    s = tcc_new();
    opt = tcc_parse_args(s, &argc, &argv, 1);

    如 ((n | t) == 0) {
        如 (opt == OPT_HELP)
            返回 printf(help), 1;
        如 (opt == OPT_HELP2)
            返回 printf(help2), 1;
        如 (opt == OPT_M32 || opt == OPT_M64)
            tcc_tool_cross(s, argv, opt); /* never returns */
        如 (s->verbose)
            printf(version);
        如 (opt == OPT_AR)
            返回 tcc_tool_ar(s, argc, argv);
#如定义 TCC_TARGET_PE
        如 (opt == OPT_IMPDEF)
            返回 tcc_tool_impdef(s, argc, argv);
#了如
        如 (opt == OPT_V)
            返回 0;
        如 (opt == OPT_PRINT_DIRS) {
            /* initialize search dirs */
            tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
            print_search_dirs(s);
            返回 0;
        }

        n = s->nb_files;
        如 (n == 0)
            tcc_error("no input files\n");

        如 (s->output_type == TCC_OUTPUT_PREPROCESS) {
            如 (s->outfile) {
                ppfp = fopen(s->outfile, "w");
                如 (!ppfp)
                    tcc_error("could not write '%s'", s->outfile);
            }
        } 另 如 (s->output_type == TCC_OUTPUT_OBJ && !s->option_r) {
            如 (s->nb_libraries)
                tcc_error("cannot specify libraries with -c");
            如 (n > 1 && s->outfile)
                tcc_error("cannot specify output file with -c many files");
        } 另 {
            如 (s->option_pthread)
                tcc_set_options(s, "-lpthread");
        }

        如 (s->do_bench)
            start_time = getclock_ms();
    }

    set_environment(s);
    如 (s->output_type == 0)
        s->output_type = TCC_OUTPUT_EXE;
    tcc_set_output_type(s, s->output_type);
    s->ppfp = ppfp;

    如 ((s->output_type == TCC_OUTPUT_MEMORY
      || s->output_type == TCC_OUTPUT_PREPROCESS) && (s->dflag & 16))
        s->dflag |= t ? 32 : 0, s->run_test = ++t, n = s->nb_files;

    /* compile or add each files or library */
    对于 (first_file = NULL, ret = 0;;) {
        结构 filespec *f = s->files[s->nb_files - n];
        s->filetype = f->type;
        s->alacarte_link = f->alacarte;
        如 (f->type == AFF_TYPE_LIB) {
            如 (tcc_add_library_err(s, f->name) < 0)
                ret = 1;
        } 另 {
            如 (1 == s->verbose)
                printf("-> %s\n", f->name);
            如 (!first_file)
                first_file = f->name;
            如 (tcc_add_file(s, f->name) < 0)
                ret = 1;
        }
        s->filetype = 0;
        s->alacarte_link = 1;
        如 (--n == 0 || ret
            || (s->output_type == TCC_OUTPUT_OBJ && !s->option_r))
            跳出;
    }

    如 (s->run_test) {
        t = 0;
    } 另 如 (s->output_type == TCC_OUTPUT_PREPROCESS) {
        ;
    } 另 如 (0 == ret) {
        如 (s->output_type == TCC_OUTPUT_MEMORY) {
#如定义 TCC_IS_NATIVE
            ret = tcc_run(s, argc, argv);
#了如
        } 另 {
            如 (!s->outfile)
                s->outfile = default_outputfile(s, first_file);
            如 (tcc_output_file(s, s->outfile))
                ret = 1;
            另 如 (s->gen_deps)
                gen_makedeps(s, s->outfile, s->deps_outfile);
        }
    }

    如 (s->do_bench && (n | t | ret) == 0)
        tcc_print_stats(s, getclock_ms() - start_time);
    tcc_delete(s);
    如 (ret == 0 && n)
        跳转 redo; /* compile more files with -c */
    如 (t)
        跳转 redo; /* run more tests with -dt -run */
    如 (ppfp && ppfp != stdout)
        fclose(ppfp);
    返回 ret;
}
