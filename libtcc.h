#如未定义 LIBTCC_H
#定义 LIBTCC_H

#如未定义 LIBTCCAPI
# 定义 LIBTCCAPI
#了如

#如定义 __cplusplus
外部 "C" {
#了如

结构 TCCState;

类型定义 结构 TCCState TCCState;

/* create a new TCC compilation context */
LIBTCCAPI TCCState *tcc_new(空);

/* free a TCC compilation context */
LIBTCCAPI 空 tcc_delete(TCCState *s);

/* set CONFIG_TCCDIR at runtime */
LIBTCCAPI 空 tcc_set_lib_path(TCCState *s, 不变 字 *path);

/* set error/warning display callback */
LIBTCCAPI 空 tcc_set_error_func(TCCState *s, 空 *error_opaque,
    空 (*error_func)(空 *opaque, 不变 字 *msg));

/* set options as from command line (multiple supported) */
LIBTCCAPI 空 tcc_set_options(TCCState *s, 不变 字 *str);

/*****************************/
/* preprocessor */

/* add include path */
LIBTCCAPI 整 tcc_add_include_path(TCCState *s, 不变 字 *pathname);

/* add in system include path */
LIBTCCAPI 整 tcc_add_sysinclude_path(TCCState *s, 不变 字 *pathname);

/* define preprocessor symbol 'sym'. Can put optional value */
LIBTCCAPI 空 tcc_define_symbol(TCCState *s, 不变 字 *sym, 不变 字 *value);

/* undefine preprocess symbol 'sym' */
LIBTCCAPI 空 tcc_undefine_symbol(TCCState *s, 不变 字 *sym);

/*****************************/
/* compiling */

/* add a file (C file, dll, object, library, ld script). Return -1 if error. */
LIBTCCAPI 整 tcc_add_file(TCCState *s, 不变 字 *filename);

/* compile a string containing a C source. Return -1 if error. */
LIBTCCAPI 整 tcc_compile_string(TCCState *s, 不变 字 *buf);

/*****************************/
/* linking commands */

/* set output type. MUST BE CALLED before any compilation */
LIBTCCAPI 整 tcc_set_output_type(TCCState *s, 整 output_type);
#定义 TCC_OUTPUT_MEMORY   1 /* output will be run in memory (default) */
#定义 TCC_OUTPUT_EXE      2 /* executable file */
#定义 TCC_OUTPUT_DLL      3 /* dynamic library */
#定义 TCC_OUTPUT_OBJ      4 /* object file */
#定义 TCC_OUTPUT_PREPROCESS 5 /* only preprocess (used internally) */

/* equivalent to -Lpath option */
LIBTCCAPI 整 tcc_add_library_path(TCCState *s, 不变 字 *pathname);

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI 整 tcc_add_library(TCCState *s, 不变 字 *libraryname);

/* add a symbol to the compiled program */
LIBTCCAPI 整 tcc_add_symbol(TCCState *s, 不变 字 *name, 不变 空 *val);

/* output an executable, library or object file. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI 整 tcc_output_file(TCCState *s, 不变 字 *filename);

/* link and run main() function and return its value. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI 整 tcc_run(TCCState *s, 整 argc, 字 **argv);

/* do all relocations (needed before using tcc_get_symbol()) */
LIBTCCAPI 整 tcc_relocate(TCCState *s1, 空 *ptr);
/* possible values for 'ptr':
   - TCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. */
#定义 TCC_RELOCATE_AUTO (空*)1

/* return symbol value or NULL if not found */
LIBTCCAPI 空 *tcc_get_symbol(TCCState *s, 不变 字 *name);

#如定义 __cplusplus
}
#了如

#了如
