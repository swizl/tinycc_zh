#如定义 TARGET_DEFS_ONLY

#定义 EM_TCC_TARGET EM_AARCH64

#定义 R_DATA_32  R_AARCH64_ABS32
#定义 R_DATA_PTR R_AARCH64_ABS64
#定义 R_JMP_SLOT R_AARCH64_JUMP_SLOT
#定义 R_GLOB_DAT R_AARCH64_GLOB_DAT
#定义 R_COPY     R_AARCH64_COPY
#定义 R_RELATIVE R_AARCH64_RELATIVE

#定义 R_NUM      R_AARCH64_NUM

#定义 ELF_START_ADDR 0x00400000
#定义 ELF_PAGE_SIZE 0x1000

#定义 PCRELATIVE_DLLPLT 1
#定义 RELOCATE_DLLPLT 1

#另 /* !TARGET_DEFS_ONLY */

#包含 "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
整 code_reloc (整 reloc_type)
{
    转接 (reloc_type) {
        事例 R_AARCH64_ABS32:
        事例 R_AARCH64_ABS64:
        事例 R_AARCH64_PREL32:
        事例 R_AARCH64_MOVW_UABS_G0_NC:
        事例 R_AARCH64_MOVW_UABS_G1_NC:
        事例 R_AARCH64_MOVW_UABS_G2_NC:
        事例 R_AARCH64_MOVW_UABS_G3:
        事例 R_AARCH64_ADR_PREL_PG_HI21:
        事例 R_AARCH64_ADD_ABS_LO12_NC:
        事例 R_AARCH64_ADR_GOT_PAGE:
        事例 R_AARCH64_LD64_GOT_LO12_NC:
        事例 R_AARCH64_GLOB_DAT:
        事例 R_AARCH64_COPY:
            返回 0;

        事例 R_AARCH64_JUMP26:
        事例 R_AARCH64_CALL26:
        事例 R_AARCH64_JUMP_SLOT:
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
        事例 R_AARCH64_PREL32:
        事例 R_AARCH64_MOVW_UABS_G0_NC:
        事例 R_AARCH64_MOVW_UABS_G1_NC:
        事例 R_AARCH64_MOVW_UABS_G2_NC:
        事例 R_AARCH64_MOVW_UABS_G3:
        事例 R_AARCH64_ADR_PREL_PG_HI21:
        事例 R_AARCH64_ADD_ABS_LO12_NC:
        事例 R_AARCH64_GLOB_DAT:
        事例 R_AARCH64_JUMP_SLOT:
        事例 R_AARCH64_COPY:
            返回 NO_GOTPLT_ENTRY;

        事例 R_AARCH64_ABS32:
        事例 R_AARCH64_ABS64:
        事例 R_AARCH64_JUMP26:
        事例 R_AARCH64_CALL26:
            返回 AUTO_GOTPLT_ENTRY;

        事例 R_AARCH64_ADR_GOT_PAGE:
        事例 R_AARCH64_LD64_GOT_LO12_NC:
            返回 ALWAYS_GOTPLT_ENTRY;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    返回 -1;
}

ST_FUNC 无符 create_plt_entry(TCCState *s1, 无符 got_offset, 结构 sym_attr *attr)
{
    Section *plt = s1->plt;
    uint8_t *p;
    无符 plt_offset;

    如 (s1->output_type == TCC_OUTPUT_DLL)
        tcc_error("DLLs unimplemented!");

    如 (plt->data_offset == 0) {
        section_ptr_add(plt, 32);
    }
    plt_offset = plt->data_offset;

    p = section_ptr_add(plt, 16);
    write32le(p, got_offset);
    write32le(p + 4, (uint64_t) got_offset >> 32);
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
        uint64_t plt = s1->plt->sh_addr;
        uint64_t got = s1->got->sh_addr;
        uint64_t off = (got >> 12) - (plt >> 12);
        如 ((off + ((uint32_t)1 << 20)) >> 21)
            tcc_error("Failed relocating PLT (off=0x%lx, got=0x%lx, plt=0x%lx)", off, got, plt);
        write32le(p, 0xa9bf7bf0); // stp x16,x30,[sp,#-16]!
        write32le(p + 4, (0x90000010 | // adrp x16,...
                          (off & 0x1ffffc) << 3 | (off & 3) << 29));
        write32le(p + 8, (0xf9400211 | // ldr x17,[x16,#...]
                          (got & 0xff8) << 7));
        write32le(p + 12, (0x91000210 | // add x16,x16,#...
                           (got & 0xfff) << 10));
        write32le(p + 16, 0xd61f0220); // br x17
        write32le(p + 20, 0xd503201f); // nop
        write32le(p + 24, 0xd503201f); // nop
        write32le(p + 28, 0xd503201f); // nop
        p += 32;
        当 (p < p_end) {
            uint64_t pc = plt + (p - s1->plt->data);
            uint64_t addr = got + read64le(p);
            uint64_t off = (addr >> 12) - (pc >> 12);
            如 ((off + ((uint32_t)1 << 20)) >> 21)
                tcc_error("Failed relocating PLT (off=0x%lx, addr=0x%lx, pc=0x%lx)", off, addr, pc);
            write32le(p, (0x90000010 | // adrp x16,...
                          (off & 0x1ffffc) << 3 | (off & 3) << 29));
            write32le(p + 4, (0xf9400211 | // ldr x17,[x16,#...]
                              (addr & 0xff8) << 7));
            write32le(p + 8, (0x91000210 | // add x16,x16,#...
                              (addr & 0xfff) << 10));
            write32le(p + 12, 0xd61f0220); // br x17
            p += 16;
        }
    }
}

空 relocate_init(Section *sr) {}

空 relocate(TCCState *s1, ElfW_Rel *rel, 整 type, 无符 字 *ptr, addr_t addr, addr_t val)
{
    整 sym_index = ELFW(R_SYM)(rel->r_info);
#如定义 DEBUG_RELOC
    ElfW(Sym) *sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];
#了如

    转接(type) {
        事例 R_AARCH64_ABS64:
            write64le(ptr, val);
            返回;
        事例 R_AARCH64_ABS32:
            write32le(ptr, val);
            返回;
        事例 R_AARCH64_PREL32:
            write32le(ptr, val - addr);
            返回;
        事例 R_AARCH64_MOVW_UABS_G0_NC:
            write32le(ptr, ((read32le(ptr) & 0xffe0001f) |
                            (val & 0xffff) << 5));
            返回;
        事例 R_AARCH64_MOVW_UABS_G1_NC:
            write32le(ptr, ((read32le(ptr) & 0xffe0001f) |
                            (val >> 16 & 0xffff) << 5));
            返回;
        事例 R_AARCH64_MOVW_UABS_G2_NC:
            write32le(ptr, ((read32le(ptr) & 0xffe0001f) |
                            (val >> 32 & 0xffff) << 5));
            返回;
        事例 R_AARCH64_MOVW_UABS_G3:
            write32le(ptr, ((read32le(ptr) & 0xffe0001f) |
                            (val >> 48 & 0xffff) << 5));
            返回;
        事例 R_AARCH64_ADR_PREL_PG_HI21: {
            uint64_t off = (val >> 12) - (addr >> 12);
            如 ((off + ((uint64_t)1 << 20)) >> 21)
                tcc_error("R_AARCH64_ADR_PREL_PG_HI21 relocation failed");
            write32le(ptr, ((read32le(ptr) & 0x9f00001f) |
                            (off & 0x1ffffc) << 3 | (off & 3) << 29));
            返回;
        }
        事例 R_AARCH64_ADD_ABS_LO12_NC:
            write32le(ptr, ((read32le(ptr) & 0xffc003ff) |
                            (val & 0xfff) << 10));
            返回;
        事例 R_AARCH64_JUMP26:
        事例 R_AARCH64_CALL26:
#如定义 DEBUG_RELOC
            printf ("reloc %d @ 0x%lx: val=0x%lx name=%s\n", type, addr, val,
                    (字 *) symtab_section->link->data + sym->st_name);
#了如
            如 (((val - addr) + ((uint64_t)1 << 27)) & ~(uint64_t)0xffffffc)
                tcc_error("R_AARCH64_(JUMP|CALL)26 relocation failed"
                          " (val=%lx, addr=%lx)", val, addr);
            write32le(ptr, (0x14000000 |
                            (uint32_t)(type == R_AARCH64_CALL26) << 31 |
                            ((val - addr) >> 2 & 0x3ffffff)));
            返回;
        事例 R_AARCH64_ADR_GOT_PAGE: {
            uint64_t off =
                (((s1->got->sh_addr +
                   s1->sym_attrs[sym_index].got_offset) >> 12) - (addr >> 12));
            如 ((off + ((uint64_t)1 << 20)) >> 21)
                tcc_error("R_AARCH64_ADR_GOT_PAGE relocation failed");
            write32le(ptr, ((read32le(ptr) & 0x9f00001f) |
                            (off & 0x1ffffc) << 3 | (off & 3) << 29));
            返回;
        }
        事例 R_AARCH64_LD64_GOT_LO12_NC:
            write32le(ptr,
                      ((read32le(ptr) & 0xfff803ff) |
                       ((s1->got->sh_addr +
                         s1->sym_attrs[sym_index].got_offset) & 0xff8) << 7));
            返回;
        事例 R_AARCH64_COPY:
            返回;
        事例 R_AARCH64_GLOB_DAT:
        事例 R_AARCH64_JUMP_SLOT:
            /* They don't need addend */
#如定义 DEBUG_RELOC
            printf ("reloc %d @ 0x%lx: val=0x%lx name=%s\n", type, addr,
                    val - rel->r_addend,
                    (字 *) symtab_section->link->data + sym->st_name);
#了如
            write64le(ptr, val - rel->r_addend);
            返回;
        缺省:
            fprintf(stderr, "FIXME: handle reloc type %x at %x [%p] to %x\n",
                    type, (无符)addr, ptr, (无符)val);
            返回;
    }
}

#了如 /* !TARGET_DEFS_ONLY */
