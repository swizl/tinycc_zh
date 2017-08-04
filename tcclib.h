/* Simple libc header for TCC 
 * 
 * Add any function you want from the libc there. This file is here
 * only for your convenience so that you do not need to put the whole
 * glibc include files on your floppy disk 
 */
#如未定义 _TCCLIB_H
#定义 _TCCLIB_H

#包含 <stddef.h>
#包含 <stdarg.h>

/* stdlib.h */
空 *calloc(size_t nmemb, size_t size);
空 *malloc(size_t size);
空 free(空 *ptr);
空 *realloc(空 *ptr, size_t size);
整 atoi(不变 字 *nptr);
长 整 strtol(不变 字 *nptr, 字 **endptr, 整 base);
无符 长 整 strtoul(不变 字 *nptr, 字 **endptr, 整 base);
空 exit(整);

/* stdio.h */
类型定义 结构 __FILE FILE;
#定义 EOF (-1)
外部 FILE *stdin;
外部 FILE *stdout;
外部 FILE *stderr;
FILE *fopen(不变 字 *path, 不变 字 *mode);
FILE *fdopen(整 fildes, 不变 字 *mode);
FILE *freopen(不变  字 *path, 不变 字 *mode, FILE *stream);
整 fclose(FILE *stream);
size_t  fread(空 *ptr, size_t size, size_t nmemb, FILE *stream);
size_t  fwrite(空 *ptr, size_t size, size_t nmemb, FILE *stream);
整 fgetc(FILE *stream);
字 *fgets(字 *s, 整 size, FILE *stream);
整 getc(FILE *stream);
整 getchar(空);
字 *gets(字 *s);
整 ungetc(整 c, FILE *stream);
整 fflush(FILE *stream);
整 putchar (整 c);

整 printf(不变 字 *format, ...);
整 fprintf(FILE *stream, 不变 字 *format, ...);
整 sprintf(字 *str, 不变 字 *format, ...);
整 snprintf(字 *str, size_t size, 不变  字  *format, ...);
整 asprintf(字 **strp, 不变 字 *format, ...);
整 dprintf(整 fd, 不变 字 *format, ...);
整 vprintf(不变 字 *format, va_list ap);
整 vfprintf(FILE  *stream,  不变  字 *format, va_list ap);
整 vsprintf(字 *str, 不变 字 *format, va_list ap);
整 vsnprintf(字 *str, size_t size, 不变 字  *format, va_list ap);
整 vasprintf(字  **strp,  不变  字 *format, va_list ap);
整 vdprintf(整 fd, 不变 字 *format, va_list ap);

空 perror(不变 字 *s);

/* string.h */
字 *strcat(字 *dest, 不变 字 *src);
字 *strchr(不变 字 *s, 整 c);
字 *strrchr(不变 字 *s, 整 c);
字 *strcpy(字 *dest, 不变 字 *src);
空 *memcpy(空 *dest, 不变 空 *src, size_t n);
空 *memmove(空 *dest, 不变 空 *src, size_t n);
空 *memset(空 *s, 整 c, size_t n);
字 *strdup(不变 字 *s);
size_t strlen(不变 字 *s);

/* dlfcn.h */
#定义 RTLD_LAZY       0x001
#定义 RTLD_NOW        0x002
#定义 RTLD_GLOBAL     0x100

空 *dlopen(不变 字 *filename, 整 flag);
不变 字 *dlerror(空);
空 *dlsym(空 *handle, 字 *symbol);
整 dlclose(空 *handle);

#了如 /* _TCCLIB_H */
