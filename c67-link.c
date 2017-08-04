#如定义 TARGET_DEFS_ONLY

#定义 EM_TCC_TARGET EM_C60

/* relocation type for 32 bit data relocation */
#定义 R_DATA_32   R_C60_32
#定义 R_DATA_PTR  R_C60_32
#定义 R_JMP_SLOT  R_C60_JMP_SLOT
#定义 R_GLOB_DAT  R_C60_GLOB_DAT
#定义 R_COPY      R_C60_COPY
#定义 R_RELATIVE  R_C60_RELATIVE

#定义 R_NUM       R_C60_NUM

#定义 ELF_START_ADDR 0x00000400
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
        事例 R_C60_32:
        事例 R_C60LO16:
        事例 R_C60HI16:
        事例 R_C60_GOT32:
        事例 R_C60_GOTOFF:
        事例 R_C60_GOTPC:
        事例 R_C60_COPY:
            返回 0;

        事例 R_C60_PLT32:
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
        事例 R_C60_32:
        事例 R_C60LO16:
        事例 R_C60HI16:
        事例 R_C60_COPY:
            返回 NO_GOTPLT_ENTRY;

        事例 R_C60_GOTOFF:
        事例 R_C60_GOTPC:
            返回 BUILD_GOT_ONLY;

        事例 R_C60_PLT32:
        事例 R_C60_GOT32:
            返回 ALWAYS_GOTPLT_ENTRY;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    返回 -1;
}

ST_FUNC 无符 create_plt_entry(TCCState *s1, 无符 got_offset, 结构 sym_attr *attr)
{
    tcc_error("C67 got not implemented");
    返回 0;
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
        /* XXX: TODO */
        当 (p < p_end) {
            /* XXX: TODO */
        }
   }
}

空 relocate_init(Section *sr) {}

空 relocate(TCCState *s1, ElfW_Rel *rel, 整 type, 无符 字 *ptr, addr_t addr, addr_t val)
{
    转接(type) {
        事例 R_C60_32:
            *(整 *)ptr += val;
            跳出;
        事例 R_C60LO16:
            {
                uint32_t orig;

                /* put the low 16 bits of the absolute address add to what is
                   already there */
                orig  =   ((*(整 *)(ptr  )) >> 7) & 0xffff;
                orig |=  (((*(整 *)(ptr+4)) >> 7) & 0xffff) << 16;

                /* patch both at once - assumes always in pairs Low - High */
                *(整 *) ptr    = (*(整 *) ptr    & (~(0xffff << 7)) ) |
                                   (((val+orig)      & 0xffff) << 7);
                *(整 *)(ptr+4) = (*(整 *)(ptr+4) & (~(0xffff << 7)) ) |
                                  ((((val+orig)>>16) & 0xffff) << 7);
            }
            跳出;
        事例 R_C60HI16:
            跳出;
        缺省:
            fprintf(stderr,"FIXME: handle reloc type %x at %x [%p] to %x\n",
                    type, (无符) addr, ptr, (无符) val);
            跳出;
    }
}

#了如 /* !TARGET_DEFS_ONLY */
