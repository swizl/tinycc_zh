#如定义 TARGET_DEFS_ONLY

#定义 EM_TCC_TARGET EM_ARM

/* relocation type for 32 bit data relocation */
#定义 R_DATA_32   R_ARM_ABS32
#定义 R_DATA_PTR  R_ARM_ABS32
#定义 R_JMP_SLOT  R_ARM_JUMP_SLOT
#定义 R_GLOB_DAT  R_ARM_GLOB_DAT
#定义 R_COPY      R_ARM_COPY
#定义 R_RELATIVE  R_ARM_RELATIVE

#定义 R_NUM       R_ARM_NUM

#定义 ELF_START_ADDR 0x00008000
#定义 ELF_PAGE_SIZE  0x1000

#定义 PCRELATIVE_DLLPLT 1
#定义 RELOCATE_DLLPLT 0

枚举 float_abi {
    ARM_SOFTFP_FLOAT,
    ARM_HARD_FLOAT,
};

#另 /* !TARGET_DEFS_ONLY */

#包含 "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
整 code_reloc (整 reloc_type)
{
    转接 (reloc_type) {
        事例 R_ARM_MOVT_ABS:
        事例 R_ARM_MOVW_ABS_NC:
        事例 R_ARM_THM_MOVT_ABS:
        事例 R_ARM_THM_MOVW_ABS_NC:
        事例 R_ARM_ABS32:
        事例 R_ARM_REL32:
        事例 R_ARM_GOTPC:
        事例 R_ARM_GOTOFF:
        事例 R_ARM_GOT32:
        事例 R_ARM_COPY:
        事例 R_ARM_GLOB_DAT:
        事例 R_ARM_NONE:
            返回 0;

        事例 R_ARM_PC24:
        事例 R_ARM_CALL:
        事例 R_ARM_JUMP24:
        事例 R_ARM_PLT32:
        事例 R_ARM_THM_PC22:
        事例 R_ARM_THM_JUMP24:
        事例 R_ARM_PREL31:
        事例 R_ARM_V4BX:
        事例 R_ARM_JUMP_SLOT:
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
        事例 R_ARM_NONE:
        事例 R_ARM_COPY:
        事例 R_ARM_GLOB_DAT:
        事例 R_ARM_JUMP_SLOT:
            返回 NO_GOTPLT_ENTRY;

        事例 R_ARM_PC24:
        事例 R_ARM_CALL:
        事例 R_ARM_JUMP24:
        事例 R_ARM_PLT32:
        事例 R_ARM_THM_PC22:
        事例 R_ARM_THM_JUMP24:
        事例 R_ARM_MOVT_ABS:
        事例 R_ARM_MOVW_ABS_NC:
        事例 R_ARM_THM_MOVT_ABS:
        事例 R_ARM_THM_MOVW_ABS_NC:
        事例 R_ARM_PREL31:
        事例 R_ARM_ABS32:
        事例 R_ARM_REL32:
        事例 R_ARM_V4BX:
            返回 AUTO_GOTPLT_ENTRY;

        事例 R_ARM_GOTPC:
        事例 R_ARM_GOTOFF:
            返回 BUILD_GOT_ONLY;

        事例 R_ARM_GOT32:
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

    /* when building a DLL, GOT entry accesses must be done relative to
       start of GOT (see x86_64 example above)  */
    如 (s1->output_type == TCC_OUTPUT_DLL)
        tcc_error("DLLs unimplemented!");

    /* empty PLT: create PLT0 entry that push address of call site and
       jump to ld.so resolution routine (GOT + 8) */
    如 (plt->data_offset == 0) {
        p = section_ptr_add(plt, 20);
        write32le(p,    0xe52de004); /* push {lr}         */
        write32le(p+4,  0xe59fe004); /* ldr lr, [pc, #4] */
        write32le(p+8,  0xe08fe00e); /* add lr, pc, lr    */
        write32le(p+12, 0xe5bef008); /* ldr pc, [lr, #8]! */
        /* p+16 is set in relocate_plt */
    }
    plt_offset = plt->data_offset;

    如 (attr->plt_thumb_stub) {
        p = section_ptr_add(plt, 4);
        write32le(p,   0x4778); /* bx pc */
        write32le(p+2, 0x46c0); /* nop   */
    }
    p = section_ptr_add(plt, 16);
    /* Jump to GOT entry where ld.so initially put address of PLT0 */
    write32le(p,   0xe59fc004); /* ldr ip, [pc, #4] */
    write32le(p+4, 0xe08fc00c); /* add ip, pc, ip */
    write32le(p+8, 0xe59cf000); /* ldr pc, [ip] */
    /* p + 12 contains offset to GOT entry once patched by relocate_plt */
    write32le(p+12, got_offset);
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
        整 x = s1->got->sh_addr - s1->plt->sh_addr - 12;
        write32le(s1->plt->data + 16, x - 16);
        p += 20;
        当 (p < p_end) {
            如 (read32le(p) == 0x46c04778) /* PLT Thumb stub present */
                p += 4;
            add32le(p + 12, x + s1->plt->data - p);
            p += 16;
        }
    }
}

空 relocate_init(Section *sr) {}

空 relocate(TCCState *s1, ElfW_Rel *rel, 整 type, 无符 字 *ptr, addr_t addr, addr_t val)
{
    ElfW(Sym) *sym;
    整 sym_index;

    sym_index = ELFW(R_SYM)(rel->r_info);
    sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];

    转接(type) {
        事例 R_ARM_PC24:
        事例 R_ARM_CALL:
        事例 R_ARM_JUMP24:
        事例 R_ARM_PLT32:
            {
                整 x, is_thumb, is_call, h, blx_avail, is_bl, th_ko;
                x = (*(整 *) ptr) & 0xffffff;
#如定义 DEBUG_RELOC
                printf ("reloc %d: x=0x%x val=0x%x ", type, x, val);
#了如
                (*(整 *)ptr) &= 0xff000000;
                如 (x & 0x800000)
                    x -= 0x1000000;
                x <<= 2;
                blx_avail = (TCC_CPU_VERSION >= 5);
                is_thumb = val & 1;
                is_bl = (*(无符 *) ptr) >> 24 == 0xeb;
                is_call = (type == R_ARM_CALL || (type == R_ARM_PC24 && is_bl));
                x += val - addr;
#如定义 DEBUG_RELOC
                printf (" newx=0x%x name=%s\n", x,
                        (字 *) symtab_section->link->data + sym->st_name);
#了如
                h = x & 2;
                th_ko = (x & 3) && (!blx_avail || !is_call);
                如 (th_ko || x >= 0x2000000 || x < -0x2000000)
                    tcc_error("can't relocate value at %x,%d",addr, type);
                x >>= 2;
                x &= 0xffffff;
                /* Only reached if blx is avail and it is a call */
                如 (is_thumb) {
                    x |= h << 24;
                    (*(整 *)ptr) = 0xfa << 24; /* bl -> blx */
                }
                (*(整 *) ptr) |= x;
            }
            返回;
        /* Since these relocations only concern Thumb-2 and blx instruction was
           introduced before Thumb-2, we can assume blx is available and not
           guard its use */
        事例 R_ARM_THM_PC22:
        事例 R_ARM_THM_JUMP24:
            {
                整 x, hi, lo, s, j1, j2, i1, i2, imm10, imm11;
                整 to_thumb, is_call, to_plt, blx_bit = 1 << 12;
                Section *plt;

                /* weak reference */
                如 (sym->st_shndx == SHN_UNDEF &&
                    ELFW(ST_BIND)(sym->st_info) == STB_WEAK)
                    返回;

                /* Get initial offset */
                hi = (*(uint16_t *)ptr);
                lo = (*(uint16_t *)(ptr+2));
                s = (hi >> 10) & 1;
                j1 = (lo >> 13) & 1;
                j2 = (lo >> 11) & 1;
                i1 = (j1 ^ s) ^ 1;
                i2 = (j2 ^ s) ^ 1;
                imm10 = hi & 0x3ff;
                imm11 = lo & 0x7ff;
                x = (s << 24) | (i1 << 23) | (i2 << 22) |
                    (imm10 << 12) | (imm11 << 1);
                如 (x & 0x01000000)
                    x -= 0x02000000;

                /* Relocation infos */
                to_thumb = val & 1;
                plt = s1->plt;
                to_plt = (val >= plt->sh_addr) &&
                         (val < plt->sh_addr + plt->data_offset);
                is_call = (type == R_ARM_THM_PC22);

                如 (!to_thumb && !to_plt && !is_call) {
                    整 index;
                    uint8_t *p;
                    字 *name, buf[1024];
                    Section *text_section;

                    name = (字 *) symtab_section->link->data + sym->st_name;
                    text_section = s1->sections[sym->st_shndx];
                    /* Modify reloc to target a thumb stub to switch to ARM */
                    snprintf(buf, 求长度(buf), "%s_from_thumb", name);
                    index = put_elf_sym(symtab_section,
                                        text_section->data_offset + 1,
                                        sym->st_size, sym->st_info, 0,
                                        sym->st_shndx, buf);
                    to_thumb = 1;
                    val = text_section->data_offset + 1;
                    rel->r_info = ELFW(R_INFO)(index, type);
                    /* Create a thumb stub function to switch to ARM mode */
                    put_elf_reloc(symtab_section, text_section,
                                  text_section->data_offset + 4, R_ARM_JUMP24,
                                  sym_index);
                    p = section_ptr_add(text_section, 8);
                    write32le(p,   0x4778); /* bx pc */
                    write32le(p+2, 0x46c0); /* nop   */
                    write32le(p+4, 0xeafffffe); /* b $sym */
                }

                /* Compute final offset */
                x += val - addr;
                如 (!to_thumb && is_call) {
                    blx_bit = 0; /* bl -> blx */
                    x = (x + 3) & -4; /* Compute offset from aligned PC */
                }

                /* Check that relocation is possible
                   * offset must not be out of range
                   * if target is to be entered in arm mode:
                     - bit 1 must not set
                     - instruction must be a call (bl) or a jump to PLT */
                如 (!to_thumb || x >= 0x1000000 || x < -0x1000000)
                    如 (to_thumb || (val & 2) || (!is_call && !to_plt))
                        tcc_error("can't relocate value at %x,%d",addr, type);

                /* Compute and store final offset */
                s = (x >> 24) & 1;
                i1 = (x >> 23) & 1;
                i2 = (x >> 22) & 1;
                j1 = s ^ (i1 ^ 1);
                j2 = s ^ (i2 ^ 1);
                imm10 = (x >> 12) & 0x3ff;
                imm11 = (x >> 1) & 0x7ff;
                (*(uint16_t *)ptr) = (uint16_t) ((hi & 0xf800) |
                                     (s << 10) | imm10);
                (*(uint16_t *)(ptr+2)) = (uint16_t) ((lo & 0xc000) |
                                (j1 << 13) | blx_bit | (j2 << 11) |
                                imm11);
            }
            返回;
        事例 R_ARM_MOVT_ABS:
        事例 R_ARM_MOVW_ABS_NC:
            {
                整 x, imm4, imm12;
                如 (type == R_ARM_MOVT_ABS)
                    val >>= 16;
                imm12 = val & 0xfff;
                imm4 = (val >> 12) & 0xf;
                x = (imm4 << 16) | imm12;
                如 (type == R_ARM_THM_MOVT_ABS)
                    *(整 *)ptr |= x;
                另
                    *(整 *)ptr += x;
            }
            返回;
        事例 R_ARM_THM_MOVT_ABS:
        事例 R_ARM_THM_MOVW_ABS_NC:
            {
                整 x, i, imm4, imm3, imm8;
                如 (type == R_ARM_THM_MOVT_ABS)
                    val >>= 16;
                imm8 = val & 0xff;
                imm3 = (val >> 8) & 0x7;
                i = (val >> 11) & 1;
                imm4 = (val >> 12) & 0xf;
                x = (imm3 << 28) | (imm8 << 16) | (i << 10) | imm4;
                如 (type == R_ARM_THM_MOVT_ABS)
                    *(整 *)ptr |= x;
                另
                    *(整 *)ptr += x;
            }
            返回;
        事例 R_ARM_PREL31:
            {
                整 x;
                x = (*(整 *)ptr) & 0x7fffffff;
                (*(整 *)ptr) &= 0x80000000;
                x = (x * 2) / 2;
                x += val - addr;
                如((x^(x>>1))&0x40000000)
                    tcc_error("can't relocate value at %x,%d",addr, type);
                (*(整 *)ptr) |= x & 0x7fffffff;
            }
        事例 R_ARM_ABS32:
            *(整 *)ptr += val;
            返回;
        事例 R_ARM_REL32:
            *(整 *)ptr += val - addr;
            返回;
        事例 R_ARM_GOTPC:
            *(整 *)ptr += s1->got->sh_addr - addr;
            返回;
        事例 R_ARM_GOTOFF:
            *(整 *)ptr += val - s1->got->sh_addr;
            返回;
        事例 R_ARM_GOT32:
            /* we load the got offset */
            *(整 *)ptr += s1->sym_attrs[sym_index].got_offset;
            返回;
        事例 R_ARM_COPY:
            返回;
        事例 R_ARM_V4BX:
            /* trade Thumb support for ARMv4 support */
            如 ((0x0ffffff0 & *(整*)ptr) == 0x012FFF10)
                *(整*)ptr ^= 0xE12FFF10 ^ 0xE1A0F000; /* BX Rm -> MOV PC, Rm */
            返回;
        事例 R_ARM_GLOB_DAT:
        事例 R_ARM_JUMP_SLOT:
            *(addr_t *)ptr = val;
            返回;
        事例 R_ARM_NONE:
            /* Nothing to do.  Normally used to indicate a dependency
               on a certain symbol (like for exception handling under EABI).  */
            返回;
        缺省:
            fprintf(stderr,"FIXME: handle reloc type %x at %x [%p] to %x\n",
                type, (无符)addr, ptr, (无符)val);
            返回;
    }
}

#了如 /* !TARGET_DEFS_ONLY */
