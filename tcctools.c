/* -------------------------------------------------------------- */
/*
 *  TCC - Tiny C Compiler
 *
 *  tcctools.c - extra tools and and -m32/64 support
 *
 */

/* -------------------------------------------------------------- */
/*
 * This program is for making libtcc1.a without ar
 * tiny_libmaker - tiny elf lib maker
 * usage: tiny_libmaker [lib] files...
 * Copyright (c) 2007 Timppa
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#包含 "tcc.h"

//#定义 ARMAG  "!<arch>\n"
#定义 ARFMAG "`\n"

类型定义 结构 {
    字 ar_name[16];
    字 ar_date[12];
    字 ar_uid[6];
    字 ar_gid[6];
    字 ar_mode[8];
    字 ar_size[10];
    字 ar_fmag[2];
} ArHdr;

静态 无符 长 le2belong(无符 长 ul) {
    返回 ((ul & 0xFF0000)>>8)+((ul & 0xFF000000)>>24) +
        ((ul & 0xFF)<<24)+((ul & 0xFF00)<<8);
}

/* Returns 1 if s contains any of the chars of list, else 0 */
静态 整 contains_any(不变 字 *s, 不变 字 *list) {
  不变 字 *l;
  对于 (; *s; s++) {
      对于 (l = list; *l; l++) {
          如 (*s == *l)
              返回 1;
      }
  }
  返回 0;
}

静态 整 ar_usage(整 ret) {
    fprintf(stderr, "usage: tcc -ar [rcsv] lib file...\n");
    fprintf(stderr, "create library ([abdioptxN] not supported).\n");
    返回 ret;
}

ST_FUNC 整 tcc_tool_ar(TCCState *s1, 整 argc, 字 **argv)
{
    静态 ArHdr arhdr = {
        "/               ",
        "            ",
        "0     ",
        "0     ",
        "0       ",
        "          ",
        ARFMAG
        };

    静态 ArHdr arhdro = {
        "                ",
        "            ",
        "0     ",
        "0     ",
        "0       ",
        "          ",
        ARFMAG
        };

    FILE *fi, *fh = NULL, *fo = NULL;
    ElfW(Ehdr) *ehdr;
    ElfW(Shdr) *shdr;
    ElfW(Sym) *sym;
    整 i, fsize, i_lib, i_obj;
    字 *buf, *shstr, *symtab = NULL, *strtab = NULL;
    整 symtabsize = 0;//, strtabsize = 0;
    字 *anames = NULL;
    整 *afpos = NULL;
    整 istrlen, strpos = 0, fpos = 0, funccnt = 0, funcmax, hofs;
    字 tfile[260], stmp[20];
    字 *file, *name;
    整 ret = 2;
    不变 字 *ops_conflict = "habdioptxN";  // unsupported but destructive if ignored.
    整 verbose = 0;

    i_lib = 0; i_obj = 0;  // will hold the index of the lib and first obj
    对于 (i = 1; i < argc; i++) {
        不变 字 *a = argv[i];
        如 (*a == '-' && strstr(a, "."))
            ret = 1; // -x.y is always invalid (same as gnu ar)
        如 ((*a == '-') || (i == 1 && !strstr(a, "."))) {  // options argument
            如 (contains_any(a, ops_conflict))
                ret = 1;
            如 (strstr(a, "v"))
                verbose = 1;
        } 另 {  // lib or obj files: don't abort - keep validating all args.
            如 (!i_lib)  // first file is the lib
                i_lib = i;
            另 如 (!i_obj)  // second file is the first obj
                i_obj = i;
        }
    }

    如 (!i_obj)  // i_obj implies also i_lib. we require both.
        ret = 1;

    如 (ret == 1)
        返回 ar_usage(ret);

    如 ((fh = fopen(argv[i_lib], "wb")) == NULL)
    {
        fprintf(stderr, "tcc: ar: can't open file %s \n", argv[i_lib]);
        跳转 the_end;
    }

    sprintf(tfile, "%s.tmp", argv[i_lib]);
    如 ((fo = fopen(tfile, "wb+")) == NULL)
    {
        fprintf(stderr, "tcc: ar: can't create temporary file %s\n", tfile);
        跳转 the_end;
    }

    funcmax = 250;
    afpos = tcc_realloc(NULL, funcmax * 求长度 *afpos); // 250 func
    memcpy(&arhdro.ar_mode, "100666", 6);

    // i_obj = first input object file
    当 (i_obj < argc)
    {
        如 (*argv[i_obj] == '-') {  // by now, all options start with '-'
            i_obj++;
            继续;
        }
        如 ((fi = fopen(argv[i_obj], "rb")) == NULL) {
            fprintf(stderr, "tcc: ar: can't open file %s \n", argv[i_obj]);
            跳转 the_end;
        }
        如 (verbose)
            printf("a - %s\n", argv[i_obj]);

        fseek(fi, 0, SEEK_END);
        fsize = ftell(fi);
        fseek(fi, 0, SEEK_SET);
        buf = tcc_malloc(fsize + 1);
        fread(buf, fsize, 1, fi);
        fclose(fi);

        // elf header
        ehdr = (ElfW(Ehdr) *)buf;
        如 (ehdr->e_ident[4] != ELFCLASSW)
        {
            fprintf(stderr, "tcc: ar: Unsupported Elf Class: %s\n", argv[i_obj]);
            跳转 the_end;
        }

        shdr = (ElfW(Shdr) *) (buf + ehdr->e_shoff + ehdr->e_shstrndx * ehdr->e_shentsize);
        shstr = (字 *)(buf + shdr->sh_offset);
        对于 (i = 0; i < ehdr->e_shnum; i++)
        {
            shdr = (ElfW(Shdr) *) (buf + ehdr->e_shoff + i * ehdr->e_shentsize);
            如 (!shdr->sh_offset)
                继续;
            如 (shdr->sh_type == SHT_SYMTAB)
            {
                symtab = (字 *)(buf + shdr->sh_offset);
                symtabsize = shdr->sh_size;
            }
            如 (shdr->sh_type == SHT_STRTAB)
            {
                如 (!strcmp(shstr + shdr->sh_name, ".strtab"))
                {
                    strtab = (字 *)(buf + shdr->sh_offset);
                    //strtabsize = shdr->sh_size;
                }
            }
        }

        如 (symtab && symtabsize)
        {
            整 nsym = symtabsize / 求长度(ElfW(Sym));
            //printf("symtab: info size shndx name\n");
            对于 (i = 1; i < nsym; i++)
            {
                sym = (ElfW(Sym) *) (symtab + i * 求长度(ElfW(Sym)));
                如 (sym->st_shndx &&
                    (sym->st_info == 0x10
                    || sym->st_info == 0x11
                    || sym->st_info == 0x12
                    )) {
                    //printf("symtab: %2Xh %4Xh %2Xh %s\n", sym->st_info, sym->st_size, sym->st_shndx, strtab + sym->st_name);
                    istrlen = strlen(strtab + sym->st_name)+1;
                    anames = tcc_realloc(anames, strpos+istrlen);
                    strcpy(anames + strpos, strtab + sym->st_name);
                    strpos += istrlen;
                    如 (++funccnt >= funcmax) {
                        funcmax += 250;
                        afpos = tcc_realloc(afpos, funcmax * 求长度 *afpos); // 250 func more
                    }
                    afpos[funccnt] = fpos;
                }
            }
        }

        file = argv[i_obj];
        对于 (name = strchr(file, 0);
             name > file && name[-1] != '/' && name[-1] != '\\';
             --name);
        istrlen = strlen(name);
        如 (istrlen >= 求长度(arhdro.ar_name))
            istrlen = 求长度(arhdro.ar_name) - 1;
        memset(arhdro.ar_name, ' ', 求长度(arhdro.ar_name));
        memcpy(arhdro.ar_name, name, istrlen);
        arhdro.ar_name[istrlen] = '/';
        sprintf(stmp, "%-10d", fsize);
        memcpy(&arhdro.ar_size, stmp, 10);
        fwrite(&arhdro, 求长度(arhdro), 1, fo);
        fwrite(buf, fsize, 1, fo);
        tcc_free(buf);
        i_obj++;
        fpos += (fsize + 求长度(arhdro));
    }
    hofs = 8 + 求长度(arhdr) + strpos + (funccnt+1) * 求长度(整);
    fpos = 0;
    如 ((hofs & 1)) // align
        hofs++, fpos = 1;
    // write header
    fwrite("!<arch>\n", 8, 1, fh);
    sprintf(stmp, "%-10d", (整)(strpos + (funccnt+1) * 求长度(整)));
    memcpy(&arhdr.ar_size, stmp, 10);
    fwrite(&arhdr, 求长度(arhdr), 1, fh);
    afpos[0] = le2belong(funccnt);
    对于 (i=1; i<=funccnt; i++)
        afpos[i] = le2belong(afpos[i] + hofs);
    fwrite(afpos, (funccnt+1) * 求长度(整), 1, fh);
    fwrite(anames, strpos, 1, fh);
    如 (fpos)
        fwrite("", 1, 1, fh);
    // write objects
    fseek(fo, 0, SEEK_END);
    fsize = ftell(fo);
    fseek(fo, 0, SEEK_SET);
    buf = tcc_malloc(fsize + 1);
    fread(buf, fsize, 1, fo);
    fwrite(buf, fsize, 1, fh);
    tcc_free(buf);
    ret = 0;
the_end:
    如 (anames)
        tcc_free(anames);
    如 (afpos)
        tcc_free(afpos);
    如 (fh)
        fclose(fh);
    如 (fo)
        fclose(fo), remove(tfile);
    返回 ret;
}

/* -------------------------------------------------------------- */
/*
 * tiny_impdef creates an export definition file (.def) from a dll
 * on MS-Windows. Usage: tiny_impdef library.dll [-o outputfile]"
 *
 *  Copyright (c) 2005,2007 grischka
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#如定义 TCC_TARGET_PE

ST_FUNC 整 tcc_tool_impdef(TCCState *s1, 整 argc, 字 **argv)
{
    整 ret, v, i;
    字 infile[260];
    字 outfile[260];

    不变 字 *file;
    字 *p, *q;
    FILE *fp, *op;

#如定义 _WIN32
    字 path[260];
#了如

    infile[0] = outfile[0] = 0;
    fp = op = NULL;
    ret = 1;
    p = NULL;
    v = 0;

    对于 (i = 1; i < argc; ++i) {
        不变 字 *a = argv[i];
        如 ('-' == a[0]) {
            如 (0 == strcmp(a, "-v")) {
                v = 1;
            } 另 如 (0 == strcmp(a, "-o")) {
                如 (++i == argc)
                    跳转 usage;
                strcpy(outfile, argv[i]);
            } 另
                跳转 usage;
        } 另 如 (0 == infile[0])
            strcpy(infile, a);
        另
            跳转 usage;
    }

    如 (0 == infile[0]) {
usage:
        fprintf(stderr,
            "usage: tcc -impdef library.dll [-v] [-o outputfile]\n"
            "create export definition file (.def) from dll\n"
            );
        跳转 the_end;
    }

    如 (0 == outfile[0]) {
        strcpy(outfile, tcc_basename(infile));
        q = strrchr(outfile, '.');
        如 (NULL == q)
            q = strchr(outfile, 0);
        strcpy(q, ".def");
    }

    file = infile;
#如定义 _WIN32
    如 (SearchPath(NULL, file, ".dll", 求长度 path, path, NULL))
        file = path;
#了如
    ret = tcc_get_dllexports(file, &p);
    如 (ret || !p) {
        fprintf(stderr, "tcc: impdef: %s '%s'\n",
            ret == -1 ? "can't find file" :
            ret ==  1 ? "can't read symbols" :
            ret ==  0 ? "no symbols found in" :
            "unknown file type", file);
        ret = 1;
        跳转 the_end;
    }

    如 (v)
        printf("-> %s\n", file);

    op = fopen(outfile, "w");
    如 (NULL == op) {
        fprintf(stderr, "tcc: impdef: could not create output file: %s\n", outfile);
        跳转 the_end;
    }

    fprintf(op, "LIBRARY %s\n\nEXPORTS\n", tcc_basename(file));
    对于 (q = p, i = 0; *q; ++i) {
        fprintf(op, "%s\n", q);
        q += strlen(q) + 1;
    }

    如 (v)
        printf("<- %s (%d symbol%s)\n", outfile, i, &"s"[i<2]);

    ret = 0;

the_end:
    /* cannot free memory received from tcc_get_dllexports
       if it came from a dll */
    /* 如 (p)
        tcc_free(p); */
    如 (fp)
        fclose(fp);
    如 (op)
        fclose(op);
    返回 ret;
}

#了如 /* TCC_TARGET_PE */

/* -------------------------------------------------------------- */
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

/* re-execute the i386/x86_64 cross-compilers with tcc -m32/-m64: */

#如 !已定义 TCC_TARGET_I386 && !已定义 TCC_TARGET_X86_64

ST_FUNC 空 tcc_tool_cross(TCCState *s, 字 **argv, 整 option)
{
    tcc_error("-m%d not implemented.", option);
}

#另
#如定义 _WIN32
#包含 <process.h>

静态 字 *str_replace(不变 字 *str, 不变 字 *p, 不变 字 *r)
{
    不变 字 *s, *s0;
    字 *d, *d0;
    整 sl, pl, rl;

    sl = strlen(str);
    pl = strlen(p);
    rl = strlen(r);
    对于 (d0 = NULL;; d0 = tcc_malloc(sl + 1)) {
        对于 (d = d0, s = str; s0 = s, s = strstr(s, p), s; s += pl) {
            如 (d) {
                memcpy(d, s0, sl = s - s0), d += sl;
                memcpy(d, r, rl), d += rl;
            } 另
                sl += rl - pl;
        }
        如 (d) {
            strcpy(d, s0);
            返回 d0;
        }
    }
}

静态 整 execvp_win32(不变 字 *prog, 字 **argv)
{
    整 ret; 字 **p;
    /* replace all " by \" */
    对于 (p = argv; *p; ++p)
        如 (strchr(*p, '"'))
            *p = str_replace(*p, "\"", "\\\"");
    ret = _spawnvp(P_NOWAIT, prog, (不变 字 *不变*)argv);
    如 (-1 == ret)
        返回 ret;
    _cwait(&ret, ret, WAIT_CHILD);
    exit(ret);
}
#定义 execvp execvp_win32
#了如 /* _WIN32 */

ST_FUNC 空 tcc_tool_cross(TCCState *s, 字 **argv, 整 target)
{
    字 program[4096];
    字 *a0 = argv[0];
    整 prefix = tcc_basename(a0) - a0;

    snprintf(program, 求长度 program,
        "%.*s%s"
#如定义 TCC_TARGET_PE
        "-win32"
#了如
        "-tcc"
#如定义 _WIN32
        ".exe"
#了如
        , prefix, a0, target == 64 ? "x86_64" : "i386");

    如 (strcmp(a0, program))
        execvp(argv[0] = program, argv);
    tcc_error("could not run '%s'", program);
}

#了如 /* TCC_TARGET_I386 && TCC_TARGET_X86_64 */
/* -------------------------------------------------------------- */
/* enable commandline wildcard expansion (tcc -o x.exe *.c) */

#如定义 _WIN32
整 _CRT_glob = 1;
#如未定义 _CRT_glob
整 _dowildcard = 1;
#了如
#了如

/* -------------------------------------------------------------- */
/* generate xxx.d file */

ST_FUNC 空 gen_makedeps(TCCState *s, 不变 字 *target, 不变 字 *filename)
{
    FILE *depout;
    字 buf[1024];
    整 i;

    如 (!filename) {
        /* compute filename automatically: dir/file.o -> dir/file.d */
        snprintf(buf, 求长度 buf, "%.*s.d",
            (整)(tcc_fileextension(target) - target), target);
        filename = buf;
    }

    如 (s->verbose)
        printf("<- %s\n", filename);

    /* XXX return err codes instead of error() ? */
    depout = fopen(filename, "w");
    如 (!depout)
        tcc_error("could not open '%s'", filename);

    fprintf(depout, "%s: \\\n", target);
    对于 (i=0; i<s->nb_target_deps; ++i)
        fprintf(depout, " %s \\\n", s->target_deps[i]);
    fprintf(depout, "\n");
    fclose(depout);
}

/* -------------------------------------------------------------- */
