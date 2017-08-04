/*
 *  TCC - Tiny C Compiler - Support for -run switch
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

/* only native compiler supports -run */
#如定义 TCC_IS_NATIVE

#如未定义 _WIN32
# 包含 <sys/mman.h>
#了如

#如定义 CONFIG_TCC_BACKTRACE
# 如未定义 _WIN32
#  包含 <signal.h>
#  如未定义 __OpenBSD__
#   包含 <sys/ucontext.h>
#  了如
# 另
#  定义 ucontext_t CONTEXT
# 了如
ST_DATA 整 rt_num_callers = 6;
ST_DATA 不变 字 **rt_bound_error_msg;
ST_DATA 空 *rt_prog_main;
静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level);
静态 空 rt_error(ucontext_t *uc, 不变 字 *fmt, ...);
静态 空 set_exception_handler(空);
#了如

静态 空 set_pages_executable(空 *ptr, 无符 长 length);
静态 整 tcc_relocate_ex(TCCState *s1, 空 *ptr);

#如定义 _WIN64
静态 空 *win64_add_function_table(TCCState *s1);
静态 空 win64_del_function_table(空 *);
#了如

// #定义 HAVE_SELINUX

/* ------------------------------------------------------------- */
/* Do all relocations (needed before using tcc_get_symbol())
   Returns -1 on error. */

LIBTCCAPI 整 tcc_relocate(TCCState *s1, 空 *ptr)
{
    整 size;

    如 (TCC_RELOCATE_AUTO != ptr)
        返回 tcc_relocate_ex(s1, ptr);

    size = tcc_relocate_ex(s1, NULL);
    如 (size < 0)
        返回 -1;

#如定义 HAVE_SELINUX
    /* Use mmap instead of malloc for Selinux. */
    ptr = mmap (NULL, size, PROT_READ|PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    如 (ptr == MAP_FAILED)
        tcc_error("tccrun: could not map memory");
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, (空*)(addr_t)size);
#另
    ptr = tcc_malloc(size);
#了如
    tcc_relocate_ex(s1, ptr); /* no more errors expected */
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, ptr);
    返回 0;
}

ST_FUNC 空 tcc_run_free(TCCState *s1)
{
    整 i;

    对于 (i = 0; i < s1->nb_runtime_mem; ++i) {
#如定义 HAVE_SELINUX
        无符 size = (无符)(addr_t)s1->runtime_mem[i++];
        munmap(s1->runtime_mem[i], size);
#另
#如定义 _WIN64
        win64_del_function_table(*(空**)s1->runtime_mem[i]);
#了如
        tcc_free(s1->runtime_mem[i]);
#了如
    }
    tcc_free(s1->runtime_mem);
}

/* launch the compiled program with the given arguments */
LIBTCCAPI 整 tcc_run(TCCState *s1, 整 argc, 字 **argv)
{
    整 (*prog_main)(整, 字 **);

    s1->runtime_main = "main";
    如 ((s1->dflag & 16) && !find_elf_sym(s1->symtab, s1->runtime_main))
        返回 0;
    如 (tcc_relocate(s1, TCC_RELOCATE_AUTO) < 0)
        返回 -1;
    prog_main = tcc_get_symbol_err(s1, s1->runtime_main);

#如定义 CONFIG_TCC_BACKTRACE
    如 (s1->do_debug) {
        set_exception_handler();
        rt_prog_main = prog_main;
    }
#了如

    errno = 0; /* clean errno value */

#如定义 CONFIG_TCC_BCHECK
    如 (s1->do_bounds_check) {
        空 (*bound_init)(空);
        空 (*bound_exit)(空);
        空 (*bound_new_region)(空 *p, addr_t size);
        整  (*bound_delete_region)(空 *p);
        整 i, ret;

        /* set error function */
        rt_bound_error_msg = tcc_get_symbol_err(s1, "__bound_error_msg");
        /* XXX: use .init section so that it also work in binary ? */
        bound_init = tcc_get_symbol_err(s1, "__bound_init");
        bound_exit = tcc_get_symbol_err(s1, "__bound_exit");
        bound_new_region = tcc_get_symbol_err(s1, "__bound_new_region");
        bound_delete_region = tcc_get_symbol_err(s1, "__bound_delete_region");

        bound_init();
        /* mark argv area as valid */
        bound_new_region(argv, argc*求长度(argv[0]));
        对于 (i=0; i<argc; ++i)
            bound_new_region(argv[i], strlen(argv[i]) + 1);

        ret = (*prog_main)(argc, argv);

        /* unmark argv area */
        对于 (i=0; i<argc; ++i)
            bound_delete_region(argv[i]);
        bound_delete_region(argv);
        bound_exit();
        返回 ret;
    }
#了如
    返回 (*prog_main)(argc, argv);
}

#如 已定义 TCC_TARGET_I386 || 已定义 TCC_TARGET_X86_64
 #定义 RUN_SECTION_ALIGNMENT 63
#另
 #定义 RUN_SECTION_ALIGNMENT 15
#了如

/* relocate code. Return -1 on error, required size if ptr is NULL,
   otherwise copy code into buffer passed by the caller */
静态 整 tcc_relocate_ex(TCCState *s1, 空 *ptr)
{
    Section *s;
    无符 offset, length, fill, i, k;
    addr_t mem;

    如 (NULL == ptr) {
        s1->nb_errors = 0;
#如定义 TCC_TARGET_PE
        pe_output_file(s1, NULL);
#另
        tcc_add_runtime(s1);
        relocate_common_syms();
        tcc_add_linker_symbols(s1);
        build_got_entries(s1);
#了如
        如 (s1->nb_errors)
            返回 -1;
    }

    offset = 0, mem = (addr_t)ptr;
    fill = -mem & RUN_SECTION_ALIGNMENT;
#如定义 _WIN64
    offset += 求长度 (空*);
#了如
    对于 (k = 0; k < 2; ++k) {
        对于(i = 1; i < s1->nb_sections; i++) {
            s = s1->sections[i];
            如 (0 == (s->sh_flags & SHF_ALLOC))
                继续;
            如 (k != !(s->sh_flags & SHF_EXECINSTR))
                继续;
            offset += fill;
            s->sh_addr = mem ? mem + offset : 0;
#如 0
            如 (mem)
                printf("%-16s +%02lx %p %04x\n",
                    s->name, fill, (空*)s->sh_addr, (无符)s->data_offset);
#了如
            offset += s->data_offset;
            fill = -(mem + offset) & 15;
        }
#如 RUN_SECTION_ALIGNMENT > 15
        /* To avoid that x86 processors would reload cached instructions each time
           when data is written in the near, we need to make sure that code and data
           do not share the same 64 byte unit */
        fill = -(mem + offset) & RUN_SECTION_ALIGNMENT;
#了如
    }

    /* relocate symbols */
    relocate_syms(s1, s1->symtab, 1);
    如 (s1->nb_errors)
        返回 -1;

    如 (0 == mem)
        返回 offset + RUN_SECTION_ALIGNMENT;

    /* relocate each section */
    对于(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        如 (s->reloc)
            relocate_section(s1, s);
    }
    relocate_plt(s1);

#如定义 _WIN64
    *(空**)ptr = win64_add_function_table(s1);
#了如

    对于(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        如 (0 == (s->sh_flags & SHF_ALLOC))
            继续;
        length = s->data_offset;
        ptr = (空*)s->sh_addr;
        如 (NULL == s->data || s->sh_type == SHT_NOBITS)
            memset(ptr, 0, length);
        另
            memcpy(ptr, s->data, length);
        /* mark executable sections as executable in memory */
        如 (s->sh_flags & SHF_EXECINSTR)
            set_pages_executable(ptr, length);
    }
    返回 0;
}

/* ------------------------------------------------------------- */
/* allow to run code in memory */

静态 空 set_pages_executable(空 *ptr, 无符 长 length)
{
#如定义 _WIN32
    无符 长 old_protect;
    VirtualProtect(ptr, length, PAGE_EXECUTE_READWRITE, &old_protect);
#另
    空 __clear_cache(空 *beginning, 空 *end);
    addr_t start, end;
#如未定义 PAGESIZE
# 定义 PAGESIZE 4096
#了如
    start = (addr_t)ptr & ~(PAGESIZE - 1);
    end = (addr_t)ptr + length;
    end = (end + PAGESIZE - 1) & ~(PAGESIZE - 1);
    如 (mprotect((空 *)start, end - start, PROT_READ | PROT_WRITE | PROT_EXEC))
        tcc_error("mprotect failed: did you mean to configure --with-selinux?");
# 如 已定义 TCC_TARGET_ARM || 已定义 TCC_TARGET_ARM64
    __clear_cache(ptr, (字 *)ptr + length);
# 了如
#了如
}

#如定义 _WIN64
静态 空 *win64_add_function_table(TCCState *s1)
{
    空 *p = NULL;
    如 (s1->uw_pdata) {
        p = (空*)s1->uw_pdata->sh_addr;
        RtlAddFunctionTable(
            (RUNTIME_FUNCTION*)p,
            s1->uw_pdata->data_offset / 求长度 (RUNTIME_FUNCTION),
            text_section->sh_addr
            );
        s1->uw_pdata = NULL;
    }
    返回 p;;
}

静态 空 win64_del_function_table(空 *p)
{
    如 (p) {
        RtlDeleteFunctionTable((RUNTIME_FUNCTION*)p);
    }
}
#了如

/* ------------------------------------------------------------- */
#如定义 CONFIG_TCC_BACKTRACE

ST_FUNC 空 tcc_set_num_callers(整 n)
{
    rt_num_callers = n;
}

/* print the position in the source file of PC value 'pc' by reading
   the stabs debug information */
静态 addr_t rt_printline(addr_t wanted_pc, 不变 字 *msg)
{
    字 func_name[128], last_func_name[128];
    addr_t func_addr, last_pc, pc;
    不变 字 *incl_files[INCLUDE_STACK_SIZE];
    整 incl_index, len, last_line_num, i;
    不变 字 *str, *p;

    Stab_Sym *stab_sym = NULL, *stab_sym_end, *sym;
    整 stab_len = 0;
    字 *stab_str = NULL;

    如 (stab_section) {
        stab_len = stab_section->data_offset;
        stab_sym = (Stab_Sym *)stab_section->data;
        stab_str = (字 *) stabstr_section->data;
    }

    func_name[0] = '\0';
    func_addr = 0;
    incl_index = 0;
    last_func_name[0] = '\0';
    last_pc = (addr_t)-1;
    last_line_num = 1;

    如 (!stab_sym)
        跳转 no_stabs;

    stab_sym_end = (Stab_Sym*)((字*)stab_sym + stab_len);
    对于 (sym = stab_sym + 1; sym < stab_sym_end; ++sym) {
        转接(sym->n_type) {
            /* function start or end */
        事例 N_FUN:
            如 (sym->n_strx == 0) {
                /* we test if between last line and end of function */
                pc = sym->n_value + func_addr;
                如 (wanted_pc >= last_pc && wanted_pc < pc)
                    跳转 found;
                func_name[0] = '\0';
                func_addr = 0;
            } 另 {
                str = stab_str + sym->n_strx;
                p = strchr(str, ':');
                如 (!p) {
                    pstrcpy(func_name, 求长度(func_name), str);
                } 另 {
                    len = p - str;
                    如 (len > 求长度(func_name) - 1)
                        len = 求长度(func_name) - 1;
                    memcpy(func_name, str, len);
                    func_name[len] = '\0';
                }
                func_addr = sym->n_value;
            }
            跳出;
            /* line number info */
        事例 N_SLINE:
            pc = sym->n_value + func_addr;
            如 (wanted_pc >= last_pc && wanted_pc < pc)
                跳转 found;
            last_pc = pc;
            last_line_num = sym->n_desc;
            /* XXX: slow! */
            strcpy(last_func_name, func_name);
            跳出;
            /* include files */
        事例 N_BINCL:
            str = stab_str + sym->n_strx;
        add_incl:
            如 (incl_index < INCLUDE_STACK_SIZE) {
                incl_files[incl_index++] = str;
            }
            跳出;
        事例 N_EINCL:
            如 (incl_index > 1)
                incl_index--;
            跳出;
        事例 N_SO:
            如 (sym->n_strx == 0) {
                incl_index = 0; /* end of translation unit */
            } 另 {
                str = stab_str + sym->n_strx;
                /* do not add path */
                len = strlen(str);
                如 (len > 0 && str[len - 1] != '/')
                    跳转 add_incl;
            }
            跳出;
        }
    }

no_stabs:
    /* second pass: we try symtab symbols (no line number info) */
    incl_index = 0;
    如 (symtab_section)
    {
        ElfW(Sym) *sym, *sym_end;
        整 type;

        sym_end = (ElfW(Sym) *)(symtab_section->data + symtab_section->data_offset);
        对于(sym = (ElfW(Sym) *)symtab_section->data + 1;
            sym < sym_end;
            sym++) {
            type = ELFW(ST_TYPE)(sym->st_info);
            如 (type == STT_FUNC || type == STT_GNU_IFUNC) {
                如 (wanted_pc >= sym->st_value &&
                    wanted_pc < sym->st_value + sym->st_size) {
                    pstrcpy(last_func_name, 求长度(last_func_name),
                            (字 *) strtab_section->data + sym->st_name);
                    func_addr = sym->st_value;
                    跳转 found;
                }
            }
        }
    }
    /* did not find any info: */
    fprintf(stderr, "%s %p ???\n", msg, (空*)wanted_pc);
    fflush(stderr);
    返回 0;
 found:
    i = incl_index;
    如 (i > 0)
        fprintf(stderr, "%s:%d: ", incl_files[--i], last_line_num);
    fprintf(stderr, "%s %p", msg, (空*)wanted_pc);
    如 (last_func_name[0] != '\0')
        fprintf(stderr, " %s()", last_func_name);
    如 (--i >= 0) {
        fprintf(stderr, " (included from ");
        对于 (;;) {
            fprintf(stderr, "%s", incl_files[i]);
            如 (--i < 0)
                跳出;
            fprintf(stderr, ", ");
        }
        fprintf(stderr, ")");
    }
    fprintf(stderr, "\n");
    fflush(stderr);
    返回 func_addr;
}

/* emit a run time error at position 'pc' */
静态 空 rt_error(ucontext_t *uc, 不变 字 *fmt, ...)
{
    va_list ap;
    addr_t pc;
    整 i;

    fprintf(stderr, "Runtime error: ");
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    对于(i=0;i<rt_num_callers;i++) {
        如 (rt_get_caller_pc(&pc, uc, i) < 0)
            跳出;
        pc = rt_printline(pc, i ? "by" : "at");
        如 (pc == (addr_t)rt_prog_main && pc)
            跳出;
    }
}

/* ------------------------------------------------------------- */
#如未定义 _WIN32

/* signal handler for fatal errors */
静态 空 sig_error(整 signum, siginfo_t *siginf, 空 *puc)
{
    ucontext_t *uc = puc;

    转接(signum) {
    事例 SIGFPE:
        转接(siginf->si_code) {
        事例 FPE_INTDIV:
        事例 FPE_FLTDIV:
            rt_error(uc, "division by zero");
            跳出;
        缺省:
            rt_error(uc, "floating point exception");
            跳出;
        }
        跳出;
    事例 SIGBUS:
    事例 SIGSEGV:
        如 (rt_bound_error_msg && *rt_bound_error_msg)
            rt_error(uc, *rt_bound_error_msg);
        另
            rt_error(uc, "dereferencing invalid pointer");
        跳出;
    事例 SIGILL:
        rt_error(uc, "illegal instruction");
        跳出;
    事例 SIGABRT:
        rt_error(uc, "abort() called");
        跳出;
    缺省:
        rt_error(uc, "caught signal %d", signum);
        跳出;
    }
    exit(255);
}

#如未定义 SA_SIGINFO
# 定义 SA_SIGINFO 0x00000004u
#了如

/* Generate a stack backtrace when a CPU exception occurs. */
静态 空 set_exception_handler(空)
{
    结构 sigaction sigact;
    /* install TCC signal handlers to print debug info on fatal
       runtime errors */
    sigact.sa_flags = SA_SIGINFO | SA_RESETHAND;
    sigact.sa_sigaction = sig_error;
    sigemptyset(&sigact.sa_mask);
    sigaction(SIGFPE, &sigact, NULL);
    sigaction(SIGILL, &sigact, NULL);
    sigaction(SIGSEGV, &sigact, NULL);
    sigaction(SIGBUS, &sigact, NULL);
    sigaction(SIGABRT, &sigact, NULL);
}

/* ------------------------------------------------------------- */
#如定义 __i386__

/* fix for glibc 2.1 */
#如未定义 REG_EIP
#定义 REG_EIP EIP
#定义 REG_EBP EBP
#了如

/* return the PC at frame level 'level'. Return negative if not found */
静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level)
{
    addr_t fp;
    整 i;

    如 (level == 0) {
#如 已定义(__APPLE__)
        *paddr = uc->uc_mcontext->__ss.__eip;
#另如 已定义(__FreeBSD__) || 已定义(__FreeBSD_kernel__) || 已定义(__DragonFly__)
        *paddr = uc->uc_mcontext.mc_eip;
#另如 已定义(__dietlibc__)
        *paddr = uc->uc_mcontext.eip;
#另如 已定义(__NetBSD__)
        *paddr = uc->uc_mcontext.__gregs[_REG_EIP];
#另如 已定义(__OpenBSD__)
        *paddr = uc->sc_eip;
#另
        *paddr = uc->uc_mcontext.gregs[REG_EIP];
#了如
        返回 0;
    } 另 {
#如 已定义(__APPLE__)
        fp = uc->uc_mcontext->__ss.__ebp;
#另如 已定义(__FreeBSD__) || 已定义(__FreeBSD_kernel__) || 已定义(__DragonFly__)
        fp = uc->uc_mcontext.mc_ebp;
#另如 已定义(__dietlibc__)
        fp = uc->uc_mcontext.ebp;
#另如 已定义(__NetBSD__)
        fp = uc->uc_mcontext.__gregs[_REG_EBP];
#另如 已定义(__OpenBSD__)
        *paddr = uc->sc_ebp;
#另
        fp = uc->uc_mcontext.gregs[REG_EBP];
#了如
        对于(i=1;i<level;i++) {
            /* XXX: check address validity with program info */
            如 (fp <= 0x1000 || fp >= 0xc0000000)
                返回 -1;
            fp = ((addr_t *)fp)[0];
        }
        *paddr = ((addr_t *)fp)[1];
        返回 0;
    }
}

/* ------------------------------------------------------------- */
#另如 已定义(__x86_64__)

/* return the PC at frame level 'level'. Return negative if not found */
静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level)
{
    addr_t fp;
    整 i;

    如 (level == 0) {
        /* XXX: only support linux */
#如 已定义(__APPLE__)
        *paddr = uc->uc_mcontext->__ss.__rip;
#另如 已定义(__FreeBSD__) || 已定义(__FreeBSD_kernel__) || 已定义(__DragonFly__)
        *paddr = uc->uc_mcontext.mc_rip;
#另如 已定义(__NetBSD__)
        *paddr = uc->uc_mcontext.__gregs[_REG_RIP];
#另
        *paddr = uc->uc_mcontext.gregs[REG_RIP];
#了如
        返回 0;
    } 另 {
#如 已定义(__APPLE__)
        fp = uc->uc_mcontext->__ss.__rbp;
#另如 已定义(__FreeBSD__) || 已定义(__FreeBSD_kernel__) || 已定义(__DragonFly__)
        fp = uc->uc_mcontext.mc_rbp;
#另如 已定义(__NetBSD__)
        fp = uc->uc_mcontext.__gregs[_REG_RBP];
#另
        fp = uc->uc_mcontext.gregs[REG_RBP];
#了如
        对于(i=1;i<level;i++) {
            /* XXX: check address validity with program info */
            如 (fp <= 0x1000)
                返回 -1;
            fp = ((addr_t *)fp)[0];
        }
        *paddr = ((addr_t *)fp)[1];
        返回 0;
    }
}

/* ------------------------------------------------------------- */
#另如 已定义(__arm__)

/* return the PC at frame level 'level'. Return negative if not found */
静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level)
{
    addr_t fp, sp;
    整 i;

    如 (level == 0) {
        /* XXX: only supports linux */
#如 已定义(__linux__)
        *paddr = uc->uc_mcontext.arm_pc;
#另
        返回 -1;
#了如
        返回 0;
    } 另 {
#如 已定义(__linux__)
        fp = uc->uc_mcontext.arm_fp;
        sp = uc->uc_mcontext.arm_sp;
        如 (sp < 0x1000)
            sp = 0x1000;
#另
        返回 -1;
#了如
        /* XXX: specific to tinycc stack frames */
        如 (fp < sp + 12 || fp & 3)
            返回 -1;
        对于(i = 1; i < level; i++) {
            sp = ((addr_t *)fp)[-2];
            如 (sp < fp || sp - fp > 16 || sp & 3)
                返回 -1;
            fp = ((addr_t *)fp)[-3];
            如 (fp <= sp || fp - sp < 12 || fp & 3)
                返回 -1;
        }
        /* XXX: check address validity with program info */
        *paddr = ((addr_t *)fp)[-1];
        返回 0;
    }
}

/* ------------------------------------------------------------- */
#另如 已定义(__aarch64__)

静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level)
{
    如 (level < 0)
        返回 -1;
    另 如 (level == 0) {
        *paddr = uc->uc_mcontext.pc;
        返回 0;
    }
    另 {
        addr_t *fp = (addr_t *)uc->uc_mcontext.regs[29];
        整 i;
        对于 (i = 1; i < level; i++)
            fp = (addr_t *)fp[0];
        *paddr = fp[1];
        返回 0;
    }
}

/* ------------------------------------------------------------- */
#另

#告警 add arch specific rt_get_caller_pc()
静态 整 rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, 整 level)
{
    返回 -1;
}

#了如 /* !__i386__ */

/* ------------------------------------------------------------- */
#另 /* WIN32 */

静态 长 __stdcall cpu_exception_handler(EXCEPTION_POINTERS *ex_info)
{
    EXCEPTION_RECORD *er = ex_info->ExceptionRecord;
    CONTEXT *uc = ex_info->ContextRecord;
    转接 (er->ExceptionCode) {
    事例 EXCEPTION_ACCESS_VIOLATION:
        如 (rt_bound_error_msg && *rt_bound_error_msg)
            rt_error(uc, *rt_bound_error_msg);
        另
            rt_error(uc, "access violation");
        跳出;
    事例 EXCEPTION_STACK_OVERFLOW:
        rt_error(uc, "stack overflow");
        跳出;
    事例 EXCEPTION_INT_DIVIDE_BY_ZERO:
        rt_error(uc, "division by zero");
        跳出;
    缺省:
        rt_error(uc, "exception caught");
        跳出;
    }
    返回 EXCEPTION_EXECUTE_HANDLER;
}

/* Generate a stack backtrace when a CPU exception occurs. */
静态 空 set_exception_handler(空)
{
    SetUnhandledExceptionFilter(cpu_exception_handler);
}

/* return the PC at frame level 'level'. Return non zero if not found */
静态 整 rt_get_caller_pc(addr_t *paddr, CONTEXT *uc, 整 level)
{
    addr_t fp, pc;
    整 i;
#如定义 _WIN64
    pc = uc->Rip;
    fp = uc->Rbp;
#另
    pc = uc->Eip;
    fp = uc->Ebp;
#了如
    如 (level > 0) {
        对于(i=1;i<level;i++) {
            /* XXX: check address validity with program info */
            如 (fp <= 0x1000 || fp >= 0xc0000000)
                返回 -1;
            fp = ((addr_t*)fp)[0];
        }
        pc = ((addr_t*)fp)[1];
    }
    *paddr = pc;
    返回 0;
}

#了如 /* _WIN32 */
#了如 /* CONFIG_TCC_BACKTRACE */
/* ------------------------------------------------------------- */
#如定义 CONFIG_TCC_STATIC

/* dummy function for profiling */
ST_FUNC 空 *dlopen(不变 字 *filename, 整 flag)
{
    返回 NULL;
}

ST_FUNC 空 dlclose(空 *p)
{
}

ST_FUNC 不变 字 *dlerror(空)
{
    返回 "error";
}

类型定义 结构 TCCSyms {
    字 *str;
    空 *ptr;
} TCCSyms;


/* add the symbol you want here if no dynamic linking is done */
静态 TCCSyms tcc_syms[] = {
#如 !已定义(CONFIG_TCCBOOT)
#定义 TCCSYM(a) { #a, &a, },
    TCCSYM(printf)
    TCCSYM(fprintf)
    TCCSYM(fopen)
    TCCSYM(fclose)
#消定义 TCCSYM
#了如
    { NULL, NULL },
};

ST_FUNC 空 *dlsym(空 *handle, 不变 字 *symbol)
{
    TCCSyms *p;
    p = tcc_syms;
    当 (p->str != NULL) {
        如 (!strcmp(p->str, symbol))
            返回 p->ptr;
        p++;
    }
    返回 NULL;
}

#了如 /* CONFIG_TCC_STATIC */
#了如 /* TCC_IS_NATIVE */
/* ------------------------------------------------------------- */
