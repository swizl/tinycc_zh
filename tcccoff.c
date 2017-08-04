/*
 *  COFF file handling for TCC
 * 
 *  Copyright (c) 2003, 2004 TK
 *  Copyright (c) 2004 Fabrice Bellard
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

#定义 MAXNSCNS 255                /* MAXIMUM NUMBER OF SECTIONS         */
#定义 MAX_STR_TABLE 1000000
AOUTHDR o_filehdr;              /* OPTIONAL (A.OUT) FILE HEADER       */

SCNHDR section_header[MAXNSCNS];

#定义 MAX_FUNCS 1000
#定义 MAX_FUNC_NAME_LENGTH 128

整 nFuncs;
字 Func[MAX_FUNCS][MAX_FUNC_NAME_LENGTH];
字 AssociatedFile[MAX_FUNCS][MAX_FUNC_NAME_LENGTH];
整 LineNoFilePtr[MAX_FUNCS];
整 EndAddress[MAX_FUNCS];
整 LastLineNo[MAX_FUNCS];
整 FuncEntries[MAX_FUNCS];

整 OutputTheSection(Section * sect);
短 整 GetCoffFlags(不变 字 *s);
空 SortSymbolTable(空);
Section *FindSection(TCCState * s1, 不变 字 *sname);

整 C67_main_entry_point;

整 FindCoffSymbolIndex(不变 字 *func_name);
整 nb_syms;

类型定义 结构 {
    长 tag;
    长 size;
    长 fileptr;
    长 nextsym;
    短 整 dummy;
} AUXFUNC;

类型定义 结构 {
    长 regmask;
    无符 短 lineno;
    无符 短 nentries;
    整 localframe;
    整 nextentry;
    短 整 dummy;
} AUXBF;

类型定义 结构 {
    长 dummy;
    无符 短 lineno;
    无符 短 dummy1;
    整 dummy2;
    整 dummy3;
    无符 短 dummy4;
} AUXEF;

ST_FUNC 整 tcc_output_coff(TCCState *s1, FILE *f)
{
    Section *tcc_sect;
    SCNHDR *coff_sec;
    整 file_pointer;
    字 *Coff_str_table, *pCoff_str_table;
    整 CoffTextSectionNo, coff_nb_syms;
    FILHDR file_hdr;            /* FILE HEADER STRUCTURE              */
    Section *stext, *sdata, *sbss;
    整 i, NSectionsToOutput = 0;

    Coff_str_table = pCoff_str_table = NULL;

    stext = FindSection(s1, ".text");
    sdata = FindSection(s1, ".data");
    sbss = FindSection(s1, ".bss");

    nb_syms = symtab_section->data_offset / 求长度(Elf32_Sym);
    coff_nb_syms = FindCoffSymbolIndex("XXXXXXXXXX1");

    file_hdr.f_magic = COFF_C67_MAGIC;  /* magic number */
    file_hdr.f_timdat = 0;      /* time & date stamp */
    file_hdr.f_opthdr = 求长度(AOUTHDR);  /* sizeof(optional hdr) */
    file_hdr.f_flags = 0x1143;  /* flags (copied from what code composer does) */
    file_hdr.f_TargetID = 0x99; /* for C6x = 0x0099 */

    o_filehdr.magic = 0x0108;   /* see magic.h                          */
    o_filehdr.vstamp = 0x0190;  /* version stamp                        */
    o_filehdr.tsize = stext->data_offset;       /* text size in bytes, padded to FW bdry */
    o_filehdr.dsize = sdata->data_offset;       /* initialized data "  "                */
    o_filehdr.bsize = sbss->data_offset;        /* uninitialized data "   "             */
    o_filehdr.entrypt = C67_main_entry_point;   /* entry pt.                          */
    o_filehdr.text_start = stext->sh_addr;      /* base of text used for this file      */
    o_filehdr.data_start = sdata->sh_addr;      /* base of data used for this file      */


    // create all the section headers

    file_pointer = FILHSZ + 求长度(AOUTHDR);

    CoffTextSectionNo = -1;

    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            NSectionsToOutput++;

            如 (CoffTextSectionNo == -1 && tcc_sect == stext)
                CoffTextSectionNo = NSectionsToOutput;  // rem which coff sect number the .text sect is

            strcpy(coff_sec->s_name, tcc_sect->name);   /* section name */

            coff_sec->s_paddr = tcc_sect->sh_addr;      /* physical address */
            coff_sec->s_vaddr = tcc_sect->sh_addr;      /* virtual address */
            coff_sec->s_size = tcc_sect->data_offset;   /* section size */
            coff_sec->s_scnptr = 0;     /* file ptr to raw data for section */
            coff_sec->s_relptr = 0;     /* file ptr to relocation */
            coff_sec->s_lnnoptr = 0;    /* file ptr to line numbers */
            coff_sec->s_nreloc = 0;     /* number of relocation entries */
            coff_sec->s_flags = GetCoffFlags(coff_sec->s_name); /* flags */
            coff_sec->s_reserved = 0;   /* reserved byte */
            coff_sec->s_page = 0;       /* memory page id */

            file_pointer += 求长度(SCNHDR);
        }
    }

    file_hdr.f_nscns = NSectionsToOutput;       /* number of sections */

    // now loop through and determine file pointer locations
    // for the raw data


    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            // put raw data
            coff_sec->s_scnptr = file_pointer;  /* file ptr to raw data for section */
            file_pointer += coff_sec->s_size;
        }
    }

    // now loop through and determine file pointer locations
    // for the relocation data

    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            // put relocations data
            如 (coff_sec->s_nreloc > 0) {
                coff_sec->s_relptr = file_pointer;      /* file ptr to relocation */
                file_pointer += coff_sec->s_nreloc * 求长度(结构 reloc);
            }
        }
    }

    // now loop through and determine file pointer locations
    // for the line number data

    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        coff_sec->s_nlnno = 0;
        coff_sec->s_lnnoptr = 0;

        如 (s1->do_debug && tcc_sect == stext) {
            // count how many line nos data

            // also find association between source file name and function
            // so we can sort the symbol table


            Stab_Sym *sym, *sym_end;
            字 func_name[MAX_FUNC_NAME_LENGTH],
                last_func_name[MAX_FUNC_NAME_LENGTH];
            无符 长 func_addr, last_pc, pc;
            不变 字 *incl_files[INCLUDE_STACK_SIZE];
            整 incl_index, len, last_line_num;
            不变 字 *str, *p;

            coff_sec->s_lnnoptr = file_pointer; /* file ptr to linno */


            func_name[0] = '\0';
            func_addr = 0;
            incl_index = 0;
            last_func_name[0] = '\0';
            last_pc = 0xffffffff;
            last_line_num = 1;
            sym = (Stab_Sym *) stab_section->data + 1;
            sym_end =
                (Stab_Sym *) (stab_section->data +
                              stab_section->data_offset);

            nFuncs = 0;
            当 (sym < sym_end) {
                转接 (sym->n_type) {
                    /* function start or end */
                事例 N_FUN:
                    如 (sym->n_strx == 0) {
                        // end of function

                        coff_sec->s_nlnno++;
                        file_pointer += LINESZ;

                        pc = sym->n_value + func_addr;
                        func_name[0] = '\0';
                        func_addr = 0;
                        EndAddress[nFuncs] = pc;
                        FuncEntries[nFuncs] =
                            (file_pointer -
                             LineNoFilePtr[nFuncs]) / LINESZ - 1;
                        LastLineNo[nFuncs++] = last_line_num + 1;
                    } 另 {
                        // beginning of function

                        LineNoFilePtr[nFuncs] = file_pointer;
                        coff_sec->s_nlnno++;
                        file_pointer += LINESZ;

                        str =
                            (不变 字 *) stabstr_section->data +
                            sym->n_strx;

                        p = strchr(str, ':');
                        如 (!p) {
                            pstrcpy(func_name, 求长度(func_name), str);
                            pstrcpy(Func[nFuncs], 求长度(func_name), str);
                        } 另 {
                            len = p - str;
                            如 (len > 求长度(func_name) - 1)
                                len = 求长度(func_name) - 1;
                            memcpy(func_name, str, len);
                            memcpy(Func[nFuncs], str, len);
                            func_name[len] = '\0';
                        }

                        // save the file that it came in so we can sort later
                        pstrcpy(AssociatedFile[nFuncs], 求长度(func_name),
                                incl_files[incl_index - 1]);

                        func_addr = sym->n_value;
                    }
                    跳出;

                    /* line number info */
                事例 N_SLINE:
                    pc = sym->n_value + func_addr;

                    last_pc = pc;
                    last_line_num = sym->n_desc;

                    /* XXX: slow! */
                    strcpy(last_func_name, func_name);

                    coff_sec->s_nlnno++;
                    file_pointer += LINESZ;
                    跳出;
                    /* include files */
                事例 N_BINCL:
                    str =
                        (不变 字 *) stabstr_section->data + sym->n_strx;
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
                        str =
                            (不变 字 *) stabstr_section->data +
                            sym->n_strx;
                        /* do not add path */
                        len = strlen(str);
                        如 (len > 0 && str[len - 1] != '/')
                            跳转 add_incl;
                    }
                    跳出;
                }
                sym++;
            }
        }

    }

    file_hdr.f_symptr = file_pointer;   /* file pointer to symtab */

    如 (s1->do_debug)
        file_hdr.f_nsyms = coff_nb_syms;        /* number of symtab entries */
    另
        file_hdr.f_nsyms = 0;

    file_pointer += file_hdr.f_nsyms * SYMNMLEN;

    // OK now we are all set to write the file


    fwrite(&file_hdr, FILHSZ, 1, f);
    fwrite(&o_filehdr, 求长度(o_filehdr), 1, f);

    // write section headers
    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            fwrite(coff_sec, 求长度(SCNHDR), 1, f);
        }
    }

    // write raw data
    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            fwrite(tcc_sect->data, tcc_sect->data_offset, 1, f);
        }
    }

    // write relocation data
    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (OutputTheSection(tcc_sect)) {
            // put relocations data
            如 (coff_sec->s_nreloc > 0) {
                fwrite(tcc_sect->reloc,
                       coff_sec->s_nreloc * 求长度(结构 reloc), 1, f);
            }
        }
    }


    // group the symbols in order of filename, func1, func2, etc
    // finally global symbols

    如 (s1->do_debug)
        SortSymbolTable();

    // write line no data

    对于 (i = 1; i < s1->nb_sections; i++) {
        coff_sec = &section_header[i];
        tcc_sect = s1->sections[i];

        如 (s1->do_debug && tcc_sect == stext) {
            // count how many line nos data


            Stab_Sym *sym, *sym_end;
            字 func_name[128], last_func_name[128];
            无符 长 func_addr, last_pc, pc;
            不变 字 *incl_files[INCLUDE_STACK_SIZE];
            整 incl_index, len, last_line_num;
            不变 字 *str, *p;

            LINENO CoffLineNo;

            func_name[0] = '\0';
            func_addr = 0;
            incl_index = 0;
            last_func_name[0] = '\0';
            last_pc = 0;
            last_line_num = 1;
            sym = (Stab_Sym *) stab_section->data + 1;
            sym_end =
                (Stab_Sym *) (stab_section->data +
                              stab_section->data_offset);

            当 (sym < sym_end) {
                转接 (sym->n_type) {
                    /* function start or end */
                事例 N_FUN:
                    如 (sym->n_strx == 0) {
                        // end of function

                        CoffLineNo.l_addr.l_paddr = last_pc;
                        CoffLineNo.l_lnno = last_line_num + 1;
                        fwrite(&CoffLineNo, 6, 1, f);

                        pc = sym->n_value + func_addr;
                        func_name[0] = '\0';
                        func_addr = 0;
                    } 另 {
                        // beginning of function

                        str =
                            (不变 字 *) stabstr_section->data +
                            sym->n_strx;


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
                        last_pc = func_addr;
                        last_line_num = -1;

                        // output a function begin

                        CoffLineNo.l_addr.l_symndx =
                            FindCoffSymbolIndex(func_name);
                        CoffLineNo.l_lnno = 0;

                        fwrite(&CoffLineNo, 6, 1, f);
                    }
                    跳出;

                    /* line number info */
                事例 N_SLINE:
                    pc = sym->n_value + func_addr;


                    /* XXX: slow! */
                    strcpy(last_func_name, func_name);

                    // output a line reference

                    CoffLineNo.l_addr.l_paddr = last_pc;

                    如 (last_line_num == -1) {
                        CoffLineNo.l_lnno = sym->n_desc;
                    } 另 {
                        CoffLineNo.l_lnno = last_line_num + 1;
                    }

                    fwrite(&CoffLineNo, 6, 1, f);

                    last_pc = pc;
                    last_line_num = sym->n_desc;

                    跳出;

                    /* include files */
                事例 N_BINCL:
                    str =
                        (不变 字 *) stabstr_section->data + sym->n_strx;
                  add_incl2:
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
                        str =
                            (不变 字 *) stabstr_section->data +
                            sym->n_strx;
                        /* do not add path */
                        len = strlen(str);
                        如 (len > 0 && str[len - 1] != '/')
                            跳转 add_incl2;
                    }
                    跳出;
                }
                sym++;
            }
        }
    }

    // write symbol table
    如 (s1->do_debug) {
        整 k;
        结构 syment csym;
        AUXFUNC auxfunc;
        AUXBF auxbf;
        AUXEF auxef;
        整 i;
        Elf32_Sym *p;
        不变 字 *name;
        整 nstr;
        整 n = 0;

        Coff_str_table = (字 *) tcc_malloc(MAX_STR_TABLE);
        pCoff_str_table = Coff_str_table;
        nstr = 0;

        p = (Elf32_Sym *) symtab_section->data;


        对于 (i = 0; i < nb_syms; i++) {

            name = symtab_section->link->data + p->st_name;

            对于 (k = 0; k < 8; k++)
                csym._n._n_name[k] = 0;

            如 (strlen(name) <= 8) {
                strcpy(csym._n._n_name, name);
            } 另 {
                如 (pCoff_str_table - Coff_str_table + strlen(name) >
                    MAX_STR_TABLE - 1)
                    tcc_error("String table too large");

                csym._n._n_n._n_zeroes = 0;
                csym._n._n_n._n_offset =
                    pCoff_str_table - Coff_str_table + 4;

                strcpy(pCoff_str_table, name);
                pCoff_str_table += strlen(name) + 1;    // skip over null
                nstr++;
            }

            如 (p->st_info == 4) {
                // put a filename symbol
                csym.n_value = 33;      // ?????
                csym.n_scnum = N_DEBUG;
                csym.n_type = 0;
                csym.n_sclass = C_FILE;
                csym.n_numaux = 0;
                fwrite(&csym, 18, 1, f);
                n++;

            } 另 如 (p->st_info == 0x12) {
                // find the function data

                对于 (k = 0; k < nFuncs; k++) {
                    如 (strcmp(name, Func[k]) == 0)
                        跳出;
                }

                如 (k >= nFuncs) {
                    tcc_error("debug info can't find function: %s", name);
                }
                // put a Function Name

                csym.n_value = p->st_value;     // physical address
                csym.n_scnum = CoffTextSectionNo;
                csym.n_type = MKTYPE(T_INT, DT_FCN, 0, 0, 0, 0, 0);
                csym.n_sclass = C_EXT;
                csym.n_numaux = 1;
                fwrite(&csym, 18, 1, f);

                // now put aux info

                auxfunc.tag = 0;
                auxfunc.size = EndAddress[k] - p->st_value;
                auxfunc.fileptr = LineNoFilePtr[k];
                auxfunc.nextsym = n + 6;        // tktk
                auxfunc.dummy = 0;
                fwrite(&auxfunc, 18, 1, f);

                // put a .bf

                strcpy(csym._n._n_name, ".bf");
                csym.n_value = p->st_value;     // physical address
                csym.n_scnum = CoffTextSectionNo;
                csym.n_type = 0;
                csym.n_sclass = C_FCN;
                csym.n_numaux = 1;
                fwrite(&csym, 18, 1, f);

                // now put aux info

                auxbf.regmask = 0;
                auxbf.lineno = 0;
                auxbf.nentries = FuncEntries[k];
                auxbf.localframe = 0;
                auxbf.nextentry = n + 6;
                auxbf.dummy = 0;
                fwrite(&auxbf, 18, 1, f);

                // put a .ef

                strcpy(csym._n._n_name, ".ef");
                csym.n_value = EndAddress[k];   // physical address  
                csym.n_scnum = CoffTextSectionNo;
                csym.n_type = 0;
                csym.n_sclass = C_FCN;
                csym.n_numaux = 1;
                fwrite(&csym, 18, 1, f);

                // now put aux info

                auxef.dummy = 0;
                auxef.lineno = LastLineNo[k];
                auxef.dummy1 = 0;
                auxef.dummy2 = 0;
                auxef.dummy3 = 0;
                auxef.dummy4 = 0;
                fwrite(&auxef, 18, 1, f);

                n += 6;

            } 另 {
                // try an put some type info

                如 ((p->st_other & VT_BTYPE) == VT_DOUBLE) {
                    csym.n_type = T_DOUBLE;     // int
                    csym.n_sclass = C_EXT;
                } 另 如 ((p->st_other & VT_BTYPE) == VT_FLOAT) {
                    csym.n_type = T_FLOAT;
                    csym.n_sclass = C_EXT;
                } 另 如 ((p->st_other & VT_BTYPE) == VT_INT) {
                    csym.n_type = T_INT;        // int
                    csym.n_sclass = C_EXT;
                } 另 如 ((p->st_other & VT_BTYPE) == VT_SHORT) {
                    csym.n_type = T_SHORT;
                    csym.n_sclass = C_EXT;
                } 另 如 ((p->st_other & VT_BTYPE) == VT_BYTE) {
                    csym.n_type = T_CHAR;
                    csym.n_sclass = C_EXT;
                } 另 {
                    csym.n_type = T_INT;        // just mark as a label
                    csym.n_sclass = C_LABEL;
                }


                csym.n_value = p->st_value;
                csym.n_scnum = 2;
                csym.n_numaux = 1;
                fwrite(&csym, 18, 1, f);

                auxfunc.tag = 0;
                auxfunc.size = 0x20;
                auxfunc.fileptr = 0;
                auxfunc.nextsym = 0;
                auxfunc.dummy = 0;
                fwrite(&auxfunc, 18, 1, f);
                n++;
                n++;

            }

            p++;
        }
    }

    如 (s1->do_debug) {
        // write string table

        // first write the size
        i = pCoff_str_table - Coff_str_table;
        fwrite(&i, 4, 1, f);

        // then write the strings
        fwrite(Coff_str_table, i, 1, f);

        tcc_free(Coff_str_table);
    }

    返回 0;
}



// group the symbols in order of filename, func1, func2, etc
// finally global symbols

空 SortSymbolTable(空)
{
    整 i, j, k, n = 0;
    Elf32_Sym *p, *p2, *NewTable;
    字 *name, *name2;

    NewTable = (Elf32_Sym *) tcc_malloc(nb_syms * 求长度(Elf32_Sym));

    p = (Elf32_Sym *) symtab_section->data;


    // find a file symbol, copy it over
    // then scan the whole symbol list and copy any function
    // symbols that match the file association

    对于 (i = 0; i < nb_syms; i++) {
        如 (p->st_info == 4) {
            name = (字 *) symtab_section->link->data + p->st_name;

            // this is a file symbol, copy it over

            NewTable[n++] = *p;

            p2 = (Elf32_Sym *) symtab_section->data;

            对于 (j = 0; j < nb_syms; j++) {
                如 (p2->st_info == 0x12) {
                    // this is a func symbol

                    name2 =
                        (字 *) symtab_section->link->data + p2->st_name;

                    // find the function data index

                    对于 (k = 0; k < nFuncs; k++) {
                        如 (strcmp(name2, Func[k]) == 0)
                            跳出;
                    }

                    如 (k >= nFuncs) {
                        tcc_error("debug (sort) info can't find function: %s", name2);
                    }

                    如 (strcmp(AssociatedFile[k], name) == 0) {
                        // yes they match copy it over

                        NewTable[n++] = *p2;
                    }
                }
                p2++;
            }
        }
        p++;
    }

    // now all the filename and func symbols should have been copied over
    // copy all the rest over (all except file and funcs)

    p = (Elf32_Sym *) symtab_section->data;
    对于 (i = 0; i < nb_syms; i++) {
        如 (p->st_info != 4 && p->st_info != 0x12) {
            NewTable[n++] = *p;
        }
        p++;
    }

    如 (n != nb_syms)
        tcc_error("Internal Compiler error, debug info");

    // copy it all back

    p = (Elf32_Sym *) symtab_section->data;
    对于 (i = 0; i < nb_syms; i++) {
        *p++ = NewTable[i];
    }

    tcc_free(NewTable);
}


整 FindCoffSymbolIndex(不变 字 *func_name)
{
    整 i, n = 0;
    Elf32_Sym *p;
    字 *name;

    p = (Elf32_Sym *) symtab_section->data;

    对于 (i = 0; i < nb_syms; i++) {

        name = (字 *) symtab_section->link->data + p->st_name;

        如 (p->st_info == 4) {
            // put a filename symbol
            n++;
        } 另 如 (p->st_info == 0x12) {

            如 (strcmp(func_name, name) == 0)
                返回 n;

            n += 6;

            // put a Function Name

            // now put aux info

            // put a .bf

            // now put aux info

            // put a .ef

            // now put aux info

        } 另 {
            n += 2;
        }

        p++;
    }

    返回 n;                   // total number of symbols
}

整 OutputTheSection(Section * sect)
{
    不变 字 *s = sect->name;

    如 (!strcmp(s, ".text"))
        返回 1;
    另 如 (!strcmp(s, ".data"))
        返回 1;
    另
        返回 0;
}

短 整 GetCoffFlags(不变 字 *s)
{
    如 (!strcmp(s, ".text"))
        返回 STYP_TEXT | STYP_DATA | STYP_ALIGN | 0x400;
    另 如 (!strcmp(s, ".data"))
        返回 STYP_DATA;
    另 如 (!strcmp(s, ".bss"))
        返回 STYP_BSS;
    另 如 (!strcmp(s, ".stack"))
        返回 STYP_BSS | STYP_ALIGN | 0x200;
    另 如 (!strcmp(s, ".cinit"))
        返回 STYP_COPY | STYP_DATA | STYP_ALIGN | 0x200;
    另
        返回 0;
}

Section *FindSection(TCCState * s1, 不变 字 *sname)
{
    Section *s;
    整 i;

    对于 (i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];

        如 (!strcmp(sname, s->name))
            返回 s;
    }

    tcc_error("could not find section %s", sname);
    返回 0;
}

ST_FUNC 整 tcc_load_coff(TCCState * s1, 整 fd)
{
// tktk TokenSym *ts;

    FILE *f;
    无符 整 str_size;
    字 *Coff_str_table, *name;
    整 i, k;
    结构 syment csym;
    字 name2[9];
    FILHDR file_hdr;            /* FILE HEADER STRUCTURE              */

    f = fdopen(fd, "rb");
    如 (!f) {
        tcc_error("Unable to open .out file for input");
    }

    如 (fread(&file_hdr, FILHSZ, 1, f) != 1)
        tcc_error("error reading .out file for input");

    如 (fread(&o_filehdr, 求长度(o_filehdr), 1, f) != 1)
        tcc_error("error reading .out file for input");

    // first read the string table

    如 (fseek(f, file_hdr.f_symptr + file_hdr.f_nsyms * SYMESZ, SEEK_SET))
        tcc_error("error reading .out file for input");

    如 (fread(&str_size, 求长度(整), 1, f) != 1)
        tcc_error("error reading .out file for input");


    Coff_str_table = (字 *) tcc_malloc(str_size);

    如 (fread(Coff_str_table, str_size - 4, 1, f) != 1)
        tcc_error("error reading .out file for input");

    // read/process all the symbols

    // seek back to symbols

    如 (fseek(f, file_hdr.f_symptr, SEEK_SET))
        tcc_error("error reading .out file for input");

    对于 (i = 0; i < file_hdr.f_nsyms; i++) {
        如 (fread(&csym, SYMESZ, 1, f) != 1)
            tcc_error("error reading .out file for input");

        如 (csym._n._n_n._n_zeroes == 0) {
            name = Coff_str_table + csym._n._n_n._n_offset - 4;
        } 另 {
            name = csym._n._n_name;

            如 (name[7] != 0) {
                对于 (k = 0; k < 8; k++)
                    name2[k] = name[k];

                name2[8] = 0;

                name = name2;
            }
        }
//              如 (strcmp("_DAC_Buffer",name)==0)  // tktk
//                      name[0]=0;

        如 (((csym.n_type & 0x30) == 0x20 && csym.n_sclass == 0x2) || ((csym.n_type & 0x30) == 0x30 && csym.n_sclass == 0x2) || (csym.n_type == 0x4 && csym.n_sclass == 0x2) || (csym.n_type == 0x8 && csym.n_sclass == 0x2) || // structures
            (csym.n_type == 0x18 && csym.n_sclass == 0x2) ||    // pointer to structure
            (csym.n_type == 0x7 && csym.n_sclass == 0x2) ||     // doubles
            (csym.n_type == 0x6 && csym.n_sclass == 0x2))       // floats
        {
            // strip off any leading underscore (except for other main routine)

            如 (name[0] == '_' && strcmp(name, "_main") != 0)
                name++;

            tcc_add_symbol(s1, name, (空*)(uintptr_t)csym.n_value);
        }
        // skip any aux records

        如 (csym.n_numaux == 1) {
            如 (fread(&csym, SYMESZ, 1, f) != 1)
                tcc_error("error reading .out file for input");
            i++;
        }
    }

    返回 0;
}
