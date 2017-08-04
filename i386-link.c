#如定义 TARGET_DEFS_ONLY

#定义 EM_TCC_TARGET EM_386

/* relocation type for 32 bit data relocation */
#定义 R_DATA_32   R_386_32
#定义 R_DATA_PTR  R_386_32
#定义 R_JMP_SLOT  R_386_JMP_SLOT
#定义 R_GLOB_DAT  R_386_GLOB_DAT
#定义 R_COPY      R_386_COPY
#定义 R_RELATIVE  R_386_RELATIVE

#定义 R_NUM       R_386_NUM

#定义 ELF_START_ADDR 0x08048000
#定义 ELF_PAGE_SIZE  0x1000

#定义 PCRELATIVE_DLLPLT 0
#定义 RELOCATE_DLLPLT 0

#另 /* !TARGET_DEFS_ONLY */

#包含 "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
整 code_reloc (整 reloc_type)
{
    转接 (reloc_type) {
        事例 R_386_RELATIVE:
        事例 R_386_16:
        事例 R_386_32:
        事例 R_386_GOTPC:
        事例 R_386_GOTOFF:
        事例 R_386_GOT32:
        事例 R_386_GOT32X:
        事例 R_386_GLOB_DAT:
        事例 R_386_COPY:
            返回 0;

        事例 R_386_PC16:
        事例 R_386_PC32:
        事例 R_386_PLT32:
        事例 R_386_JMP_SLOT:
            返回 1;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    返回 -1;
}

/* Returns an enumerator to describe whether and when the relocation needs a
   GOT and/or PLT entry to be created. See tcc.h for a description of the
   different values. */
整 gotplt_entry_type (整 reloc_type)
{
    转接 (reloc_type) {
        事例 R_386_RELATIVE:
        事例 R_386_16:
        事例 R_386_GLOB_DAT:
        事例 R_386_JMP_SLOT:
        事例 R_386_COPY:
            返回 NO_GOTPLT_ENTRY;

        事例 R_386_32:
            /* This relocations shouldn't normally need GOT or PLT
               slots if it weren't for simplicity in the code generator.
               See our caller for comments.  */
            返回 AUTO_GOTPLT_ENTRY;

        事例 R_386_PC16:
        事例 R_386_PC32:
            返回 AUTO_GOTPLT_ENTRY;

        事例 R_386_GOTPC:
        事例 R_386_GOTOFF:
            返回 BUILD_GOT_ONLY;

        事例 R_386_GOT32:
        事例 R_386_GOT32X:
        事例 R_386_PLT32:
            返回 ALWAYS_GOTPLT_ENTRY;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    返回 -1;
}

ST_FUNC 无符 create_plt_entry(TCCState *s1, 无符 got_offset, 结构 sym_attr *attr)
{
    Section *plt = s1->plt;
    uint8_t *p;
    整 modrm;
    无符 plt_offset, relofs;

    /* on i386 if we build a DLL, we add a %ebx offset */
    如 (s1->output_type == TCC_OUTPUT_DLL)
        modrm = 0xa3;
    另
        modrm = 0x25;

    /* empty PLT: create PLT0 entry that pushes the library identifier
       (GOT + PTR_SIZE) and jumps to ld.so resolution routine
       (GOT + 2 * PTR_SIZE) */
    如 (plt->data_offset == 0) {
        p = section_ptr_add(plt, 16);
        p[0] = 0xff; /* pushl got + PTR_SIZE */
        p[1] = modrm + 0x10;
        write32le(p + 2, PTR_SIZE);
        p[6] = 0xff; /* jmp *(got + PTR_SIZE * 2) */
        p[7] = modrm;
        write32le(p + 8, PTR_SIZE * 2);
    }
    plt_offset = plt->data_offset;

    /* The PLT slot refers to the relocation entry it needs via offset.
       The reloc entry is created below, so its offset is the current
       data_offset */
    relofs = s1->got->reloc ? s1->got->reloc->data_offset : 0;

    /* Jump to GOT entry where ld.so initially put the address of ip + 4 */
    p = section_ptr_add(plt, 16);
    p[0] = 0xff; /* jmp *(got + x) */
    p[1] = modrm;
    write32le(p + 2, got_offset);
    p[6] = 0x68; /* push $xxx */
    write32le(p + 7, relofs);
    p[11] = 0xe9; /* jmp plt_start */
    write32le(p + 12, -(plt->data_offset));
    返回 plt_offset;
}

/* relocate the PLT: compute addresses and offsets in the PLT now that final
   address for PLT and GOT are known (see fill_program_header) */
ST_FUNC 空 relocate_plt(TCCState *s1)
{
    uint8_t *p, *p_end;

    如 (!s1->plt)
      返回;

    p = s1->plt->data;
    p_end = p + s1->plt->data_offset;

    如 (p < p_end) {
        add32le(p + 2, s1->got->sh_addr);
        add32le(p + 8, s1->got->sh_addr);
        p += 16;
        当 (p < p_end) {
            add32le(p + 2, s1->got->sh_addr);
            p += 16;
        }
    }
}

静态 ElfW_Rel *qrel; /* ptr to next reloc entry reused */

空 relocate_init(Section *sr)
{
    qrel = (ElfW_Rel *) sr->data;
}

空 relocate(TCCState *s1, ElfW_Rel *rel, 整 type, 无符 字 *ptr, addr_t addr, addr_t val)
{
    整 sym_index, esym_index;

    sym_index = ELFW(R_SYM)(rel->r_info);

    转接 (type) {
        事例 R_386_32:
            如 (s1->output_type == TCC_OUTPUT_DLL) {
                esym_index = s1->sym_attrs[sym_index].dyn_index;
                qrel->r_offset = rel->r_offset;
                如 (esym_index) {
                    qrel->r_info = ELFW(R_INFO)(esym_index, R_386_32);
                    qrel++;
                    返回;
                } 另 {
                    qrel->r_info = ELFW(R_INFO)(0, R_386_RELATIVE);
                    qrel++;
                }
            }
            add32le(ptr, val);
            返回;
        事例 R_386_PC32:
            如 (s1->output_type == TCC_OUTPUT_DLL) {
                /* DLL relocation */
                esym_index = s1->sym_attrs[sym_index].dyn_index;
                如 (esym_index) {
                    qrel->r_offset = rel->r_offset;
                    qrel->r_info = ELFW(R_INFO)(esym_index, R_386_PC32);
                    qrel++;
                    返回;
                }
            }
            add32le(ptr, val - addr);
            返回;
        事例 R_386_PLT32:
            add32le(ptr, val - addr);
            返回;
        事例 R_386_GLOB_DAT:
        事例 R_386_JMP_SLOT:
            write32le(ptr, val);
            返回;
        事例 R_386_GOTPC:
            add32le(ptr, s1->got->sh_addr - addr);
            返回;
        事例 R_386_GOTOFF:
            add32le(ptr, val - s1->got->sh_addr);
            返回;
        事例 R_386_GOT32:
        事例 R_386_GOT32X:
            /* we load the got offset */
            add32le(ptr, s1->sym_attrs[sym_index].got_offset);
            返回;
        事例 R_386_16:
            如 (s1->output_format != TCC_OUTPUT_FORMAT_BINARY) {
            output_file:
                tcc_error("can only produce 16-bit binary files");
            }
            write16le(ptr, read16le(ptr) + val);
            返回;
        事例 R_386_PC16:
            如 (s1->output_format != TCC_OUTPUT_FORMAT_BINARY)
                跳转 output_file;
            write16le(ptr, read16le(ptr) + val - addr);
            返回;
        事例 R_386_RELATIVE:
            /* do nothing */
            返回;
        事例 R_386_COPY:
            /* This relocation must copy initialized data from the library
            to the program .bss segment. Currently made like for ARM
            (to remove noise of default case). Is this true?
            */
            返回;
        缺省:
            fprintf(stderr,"FIXME: handle reloc type %d at %x [%p] to %x\n",
                type, (无符)addr, ptr, (无符)val);
            返回;
    }
}

#了如 /* !TARGET_DEFS_ONLY */
