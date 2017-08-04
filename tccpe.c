/*
 *  TCCPE.C - PE file output for the Tiny C Compiler
 *
 *  Copyright (c) 2005-2007 grischka
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

#定义 PE_MERGE_DATA
/* #定义 PE_PRINT_SECTIONS */

#如未定义 _WIN32
#定义 stricmp strcasecmp
#定义 strnicmp strncasecmp
#了如

#如定义 TCC_TARGET_X86_64
# 定义 ADDR3264 ULONGLONG
# 定义 REL_TYPE_DIRECT R_X86_64_64
# 定义 R_XXX_THUNKFIX R_X86_64_PC32
# 定义 R_XXX_RELATIVE R_X86_64_RELATIVE
# 定义 IMAGE_FILE_MACHINE 0x8664
# 定义 RSRC_RELTYPE 3

#另如 已定义 TCC_TARGET_ARM
# 定义 ADDR3264 DWORD
# 定义 REL_TYPE_DIRECT R_ARM_ABS32
# 定义 R_XXX_THUNKFIX R_ARM_ABS32
# 定义 R_XXX_RELATIVE R_ARM_RELATIVE
# 定义 IMAGE_FILE_MACHINE 0x01C0
# 定义 RSRC_RELTYPE 7 /* ??? (not tested) */

#另如 已定义 TCC_TARGET_I386
# 定义 ADDR3264 DWORD
# 定义 REL_TYPE_DIRECT R_386_32
# 定义 R_XXX_THUNKFIX R_386_32
# 定义 R_XXX_RELATIVE R_386_RELATIVE
# 定义 IMAGE_FILE_MACHINE 0x014C
# 定义 RSRC_RELTYPE 7 /* DIR32NB */

#了如

#如 0
#如定义 _WIN32
空 dbg_printf (不变 字 *fmt, ...)
{
    字 buffer[4000];
    va_list arg;
    整 x;
    va_start(arg, fmt);
    x = vsprintf (buffer, fmt, arg);
    strcpy(buffer+x, "\n");
    OutputDebugString(buffer);
}
#了如
#了如

/* ----------------------------------------------------------- */
#如未定义 IMAGE_NT_SIGNATURE
/* ----------------------------------------------------------- */
/* definitions below are from winnt.h */

类型定义 无符 字 BYTE;
类型定义 无符 短 WORD;
类型定义 无符 整 DWORD;
类型定义 无符 长 长 ULONGLONG;
#杂注 pack(push, 1)

类型定义 结构 _IMAGE_DOS_HEADER {  /* DOS .EXE header */
    WORD e_magic;         /* Magic number */
    WORD e_cblp;          /* Bytes on last page of file */
    WORD e_cp;            /* Pages in file */
    WORD e_crlc;          /* Relocations */
    WORD e_cparhdr;       /* Size of header in paragraphs */
    WORD e_minalloc;      /* Minimum extra paragraphs needed */
    WORD e_maxalloc;      /* Maximum extra paragraphs needed */
    WORD e_ss;            /* Initial (relative) SS value */
    WORD e_sp;            /* Initial SP value */
    WORD e_csum;          /* Checksum */
    WORD e_ip;            /* Initial IP value */
    WORD e_cs;            /* Initial (relative) CS value */
    WORD e_lfarlc;        /* File address of relocation table */
    WORD e_ovno;          /* Overlay number */
    WORD e_res[4];        /* Reserved words */
    WORD e_oemid;         /* OEM identifier (for e_oeminfo) */
    WORD e_oeminfo;       /* OEM information; e_oemid specific */
    WORD e_res2[10];      /* Reserved words */
    DWORD e_lfanew;        /* File address of new exe header */
} IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;

#定义 IMAGE_NT_SIGNATURE  0x00004550  /* PE00 */
#定义 SIZE_OF_NT_SIGNATURE 4

类型定义 结构 _IMAGE_FILE_HEADER {
    WORD    Machine;
    WORD    NumberOfSections;
    DWORD   TimeDateStamp;
    DWORD   PointerToSymbolTable;
    DWORD   NumberOfSymbols;
    WORD    SizeOfOptionalHeader;
    WORD    Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;


#定义 IMAGE_SIZEOF_FILE_HEADER 20

类型定义 结构 _IMAGE_DATA_DIRECTORY {
    DWORD   VirtualAddress;
    DWORD   Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;


类型定义 结构 _IMAGE_OPTIONAL_HEADER {
    /* Standard fields. */
    WORD    Magic;
    BYTE    MajorLinkerVersion;
    BYTE    MinorLinkerVersion;
    DWORD   SizeOfCode;
    DWORD   SizeOfInitializedData;
    DWORD   SizeOfUninitializedData;
    DWORD   AddressOfEntryPoint;
    DWORD   BaseOfCode;
#如未定义 TCC_TARGET_X86_64
    DWORD   BaseOfData;
#了如
    /* NT additional fields. */
    ADDR3264 ImageBase;
    DWORD   SectionAlignment;
    DWORD   FileAlignment;
    WORD    MajorOperatingSystemVersion;
    WORD    MinorOperatingSystemVersion;
    WORD    MajorImageVersion;
    WORD    MinorImageVersion;
    WORD    MajorSubsystemVersion;
    WORD    MinorSubsystemVersion;
    DWORD   Win32VersionValue;
    DWORD   SizeOfImage;
    DWORD   SizeOfHeaders;
    DWORD   CheckSum;
    WORD    Subsystem;
    WORD    DllCharacteristics;
    ADDR3264 SizeOfStackReserve;
    ADDR3264 SizeOfStackCommit;
    ADDR3264 SizeOfHeapReserve;
    ADDR3264 SizeOfHeapCommit;
    DWORD   LoaderFlags;
    DWORD   NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[16];
} IMAGE_OPTIONAL_HEADER32, IMAGE_OPTIONAL_HEADER64, IMAGE_OPTIONAL_HEADER;

#定义 IMAGE_DIRECTORY_ENTRY_EXPORT          0   /* Export Directory */
#定义 IMAGE_DIRECTORY_ENTRY_IMPORT          1   /* Import Directory */
#定义 IMAGE_DIRECTORY_ENTRY_RESOURCE        2   /* Resource Directory */
#定义 IMAGE_DIRECTORY_ENTRY_EXCEPTION       3   /* Exception Directory */
#定义 IMAGE_DIRECTORY_ENTRY_SECURITY        4   /* Security Directory */
#定义 IMAGE_DIRECTORY_ENTRY_BASERELOC       5   /* Base Relocation Table */
#定义 IMAGE_DIRECTORY_ENTRY_DEBUG           6   /* Debug Directory */
/*      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       7      (X86 usage) */
#定义 IMAGE_DIRECTORY_ENTRY_ARCHITECTURE    7   /* Architecture Specific Data */
#定义 IMAGE_DIRECTORY_ENTRY_GLOBALPTR       8   /* RVA of GP */
#定义 IMAGE_DIRECTORY_ENTRY_TLS             9   /* TLS Directory */
#定义 IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    10   /* Load Configuration Directory */
#定义 IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   11   /* Bound Import Directory in headers */
#定义 IMAGE_DIRECTORY_ENTRY_IAT            12   /* Import Address Table */
#定义 IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   13   /* Delay Load Import Descriptors */
#定义 IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14   /* COM Runtime descriptor */

/* Section header format. */
#定义 IMAGE_SIZEOF_SHORT_NAME         8

类型定义 结构 _IMAGE_SECTION_HEADER {
    BYTE    Name[IMAGE_SIZEOF_SHORT_NAME];
    联合 {
            DWORD   PhysicalAddress;
            DWORD   VirtualSize;
    } Misc;
    DWORD   VirtualAddress;
    DWORD   SizeOfRawData;
    DWORD   PointerToRawData;
    DWORD   PointerToRelocations;
    DWORD   PointerToLinenumbers;
    WORD    NumberOfRelocations;
    WORD    NumberOfLinenumbers;
    DWORD   Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;

#定义 IMAGE_SIZEOF_SECTION_HEADER     40

类型定义 结构 _IMAGE_EXPORT_DIRECTORY {
    DWORD Characteristics;
    DWORD TimeDateStamp;
    WORD MajorVersion;
    WORD MinorVersion;
    DWORD Name;
    DWORD Base;
    DWORD NumberOfFunctions;
    DWORD NumberOfNames;
    DWORD AddressOfFunctions;
    DWORD AddressOfNames;
    DWORD AddressOfNameOrdinals;
} IMAGE_EXPORT_DIRECTORY,*PIMAGE_EXPORT_DIRECTORY;

类型定义 结构 _IMAGE_IMPORT_DESCRIPTOR {
    联合 {
        DWORD Characteristics;
        DWORD OriginalFirstThunk;
    };
    DWORD TimeDateStamp;
    DWORD ForwarderChain;
    DWORD Name;
    DWORD FirstThunk;
} IMAGE_IMPORT_DESCRIPTOR;

类型定义 结构 _IMAGE_BASE_RELOCATION {
    DWORD   VirtualAddress;
    DWORD   SizeOfBlock;
//  WORD    TypeOffset[1];
} IMAGE_BASE_RELOCATION;

#定义 IMAGE_SIZEOF_BASE_RELOCATION     8

#定义 IMAGE_REL_BASED_ABSOLUTE         0
#定义 IMAGE_REL_BASED_HIGH             1
#定义 IMAGE_REL_BASED_LOW              2
#定义 IMAGE_REL_BASED_HIGHLOW          3
#定义 IMAGE_REL_BASED_HIGHADJ          4
#定义 IMAGE_REL_BASED_MIPS_JMPADDR     5
#定义 IMAGE_REL_BASED_SECTION          6
#定义 IMAGE_REL_BASED_REL32            7

#杂注 pack(pop)

/* ----------------------------------------------------------- */
#了如 /* ndef IMAGE_NT_SIGNATURE */
/* ----------------------------------------------------------- */
#杂注 pack(push, 1)

结构 pe_header
{
    IMAGE_DOS_HEADER doshdr;
    BYTE dosstub[0x40];
    DWORD nt_sig;
    IMAGE_FILE_HEADER filehdr;
#如定义 TCC_TARGET_X86_64
    IMAGE_OPTIONAL_HEADER64 opthdr;
#另
#如定义 _WIN64
    IMAGE_OPTIONAL_HEADER32 opthdr;
#另
    IMAGE_OPTIONAL_HEADER opthdr;
#了如
#了如
};

结构 pe_reloc_header {
    DWORD offset;
    DWORD size;
};

结构 pe_rsrc_header {
    结构 _IMAGE_FILE_HEADER filehdr;
    结构 _IMAGE_SECTION_HEADER sectionhdr;
};

结构 pe_rsrc_reloc {
    DWORD offset;
    DWORD size;
    WORD type;
};

#杂注 pack(pop)

/* ------------------------------------------------------------- */
/* internal temporary structures */

/*
#定义 IMAGE_SCN_CNT_CODE                  0x00000020
#定义 IMAGE_SCN_CNT_INITIALIZED_DATA      0x00000040
#定义 IMAGE_SCN_CNT_UNINITIALIZED_DATA    0x00000080
#定义 IMAGE_SCN_MEM_DISCARDABLE           0x02000000
#定义 IMAGE_SCN_MEM_SHARED                0x10000000
#定义 IMAGE_SCN_MEM_EXECUTE               0x20000000
#定义 IMAGE_SCN_MEM_READ                  0x40000000
#定义 IMAGE_SCN_MEM_WRITE                 0x80000000
*/

枚举 {
    sec_text = 0,
    sec_data ,
    sec_bss ,
    sec_idata ,
    sec_pdata ,
    sec_other ,
    sec_rsrc ,
    sec_stab ,
    sec_reloc ,
    sec_last
};

静态 不变 DWORD pe_sec_flags[] = {
    0x60000020, /* ".text"     , */
    0xC0000040, /* ".data"     , */
    0xC0000080, /* ".bss"      , */
    0x40000040, /* ".idata"    , */
    0x40000040, /* ".pdata"    , */
    0xE0000060, /* < other >   , */
    0x40000040, /* ".rsrc"     , */
    0x42000802, /* ".stab"     , */
    0x42000040, /* ".reloc"    , */
};

结构 section_info {
    整 cls, ord;
    字 name[32];
    DWORD sh_addr;
    DWORD sh_size;
    DWORD sh_flags;
    无符 字 *data;
    DWORD data_size;
    IMAGE_SECTION_HEADER ish;
};

结构 import_symbol {
    整 sym_index;
    整 iat_index;
    整 thk_offset;
};

结构 pe_import_info {
    整 dll_index;
    整 sym_count;
    结构 import_symbol **symbols;
};

结构 pe_info {
    TCCState *s1;
    Section *reloc;
    Section *thunk;
    不变 字 *filename;
    整 type;
    DWORD sizeofheaders;
    ADDR3264 imagebase;
    不变 字 *start_symbol;
    DWORD start_addr;
    DWORD imp_offs;
    DWORD imp_size;
    DWORD iat_offs;
    DWORD iat_size;
    DWORD exp_offs;
    DWORD exp_size;
    整 subsystem;
    DWORD section_align;
    DWORD file_align;
    结构 section_info *sec_info;
    整 sec_count;
    结构 pe_import_info **imp_info;
    整 imp_count;
};

#定义 PE_NUL 0
#定义 PE_DLL 1
#定义 PE_GUI 2
#定义 PE_EXE 3
#定义 PE_RUN 4

/* --------------------------------------------*/

静态 不变 字 *pe_export_name(TCCState *s1, ElfW(Sym) *sym)
{
    不变 字 *name = (字*)symtab_section->link->data + sym->st_name;
    如 (s1->leading_underscore && name[0] == '_' && !(sym->st_other & ST_PE_STDCALL))
        返回 name + 1;
    返回 name;
}

静态 整 pe_find_import(TCCState * s1, ElfW(Sym) *sym)
{
    字 buffer[200];
    不变 字 *s, *p;
    整 sym_index = 0, n = 0;
    整 a, err = 0;

    运行 {
        s = pe_export_name(s1, sym);
        a = 0;
        如 (n) {
            /* second try: */
            如 (sym->st_other & ST_PE_STDCALL) {
                /* try w/0 stdcall deco (windows API convention) */
                p = strrchr(s, '@');
                如 (!p || s[0] != '_')
                    跳出;
                strcpy(buffer, s+1)[p-s-1] = 0;
            } 另 如 (s[0] != '_') { /* try non-ansi function */
                buffer[0] = '_', strcpy(buffer + 1, s);
            } 另 如 (0 == memcmp(s, "__imp_", 6)) { /* mingw 2.0 */
                strcpy(buffer, s + 6), a = 1;
            } 另 如 (0 == memcmp(s, "_imp__", 6)) { /* mingw 3.7 */
                strcpy(buffer, s + 6), a = 1;
            } 另 {
                继续;
            }
            s = buffer;
        }
        sym_index = find_elf_sym(s1->dynsymtab_section, s);
        // printf("find (%d) %d %s\n", n, sym_index, s);
        如 (sym_index
            && ELFW(ST_TYPE)(sym->st_info) == STT_OBJECT
            && 0 == (sym->st_other & ST_PE_IMPORT)
            && 0 == a
            ) err = -1, sym_index = 0;
    } 当 (0 == sym_index && ++n < 2);
    返回 n == 2 ? err : sym_index;
}

/*----------------------------------------------------------------------------*/

静态 整 dynarray_assoc(空 **pp, 整 n, 整 key)
{
    整 i;
    对于 (i = 0; i < n; ++i, ++pp)
    如 (key == **(整 **) pp)
        返回 i;
    返回 -1;
}

#如 0
ST_FN DWORD umin(DWORD a, DWORD b)
{
    返回 a < b ? a : b;
}
#了如

静态 DWORD umax(DWORD a, DWORD b)
{
    返回 a < b ? b : a;
}

静态 DWORD pe_file_align(结构 pe_info *pe, DWORD n)
{
    返回 (n + (pe->file_align - 1)) & ~(pe->file_align - 1);
}

静态 DWORD pe_virtual_align(结构 pe_info *pe, DWORD n)
{
    返回 (n + (pe->section_align - 1)) & ~(pe->section_align - 1);
}

静态 空 pe_align_section(Section *s, 整 a)
{
    整 i = s->data_offset & (a-1);
    如 (i)
        section_ptr_add(s, a - i);
}

静态 空 pe_set_datadir(结构 pe_header *hdr, 整 dir, DWORD addr, DWORD size)
{
    hdr->opthdr.DataDirectory[dir].VirtualAddress = addr;
    hdr->opthdr.DataDirectory[dir].Size = size;
}

静态 整 pe_fwrite(空 *data, 无符 len, FILE *fp, DWORD *psum)
{
    如 (psum) {
        DWORD sum = *psum;
        WORD *p = data;
        整 i;
        对于 (i = len; i > 0; i -= 2) {
            sum += (i >= 2) ? *p++ : *(BYTE*)p;
            sum = (sum + (sum >> 16)) & 0xFFFF;
        }
        *psum = sum;
    }
    返回 len == fwrite(data, 1, len, fp) ? 0 : -1;
}

静态 空 pe_fpad(FILE *fp, DWORD new_pos)
{
    DWORD pos = ftell(fp);
    当 (++pos <= new_pos)
        fputc(0, fp);
}

/*----------------------------------------------------------------------------*/
静态 整 pe_write(结构 pe_info *pe)
{
    静态 不变 结构 pe_header pe_template = {
    {
    /* IMAGE_DOS_HEADER doshdr */
    0x5A4D, /*WORD e_magic;         Magic number */
    0x0090, /*WORD e_cblp;          Bytes on last page of file */
    0x0003, /*WORD e_cp;            Pages in file */
    0x0000, /*WORD e_crlc;          Relocations */

    0x0004, /*WORD e_cparhdr;       Size of header in paragraphs */
    0x0000, /*WORD e_minalloc;      Minimum extra paragraphs needed */
    0xFFFF, /*WORD e_maxalloc;      Maximum extra paragraphs needed */
    0x0000, /*WORD e_ss;            Initial (relative) SS value */

    0x00B8, /*WORD e_sp;            Initial SP value */
    0x0000, /*WORD e_csum;          Checksum */
    0x0000, /*WORD e_ip;            Initial IP value */
    0x0000, /*WORD e_cs;            Initial (relative) CS value */
    0x0040, /*WORD e_lfarlc;        File address of relocation table */
    0x0000, /*WORD e_ovno;          Overlay number */
    {0,0,0,0}, /*WORD e_res[4];     Reserved words */
    0x0000, /*WORD e_oemid;         OEM identifier (for e_oeminfo) */
    0x0000, /*WORD e_oeminfo;       OEM information; e_oemid specific */
    {0,0,0,0,0,0,0,0,0,0}, /*WORD e_res2[10];      Reserved words */
    0x00000080  /*DWORD   e_lfanew;        File address of new exe header */
    },{
    /* BYTE dosstub[0x40] */
    /* 14 code bytes + "This program cannot be run in DOS mode.\r\r\n$" + 6 * 0x00 */
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x0d,0x0d,0x0a,0x24,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    },
    0x00004550, /* DWORD nt_sig = IMAGE_NT_SIGNATURE */
    {
    /* IMAGE_FILE_HEADER filehdr */
    IMAGE_FILE_MACHINE, /*WORD    Machine; */
    0x0003, /*WORD    NumberOfSections; */
    0x00000000, /*DWORD   TimeDateStamp; */
    0x00000000, /*DWORD   PointerToSymbolTable; */
    0x00000000, /*DWORD   NumberOfSymbols; */
#如 已定义(TCC_TARGET_X86_64)
    0x00F0, /*WORD    SizeOfOptionalHeader; */
    0x022F  /*WORD    Characteristics; */
#定义 CHARACTERISTICS_DLL 0x222E
#另如 已定义(TCC_TARGET_I386)
    0x00E0, /*WORD    SizeOfOptionalHeader; */
    0x030F  /*WORD    Characteristics; */
#定义 CHARACTERISTICS_DLL 0x230E
#另如 已定义(TCC_TARGET_ARM)
    0x00E0, /*WORD    SizeOfOptionalHeader; */
    0x010F, /*WORD    Characteristics; */
#定义 CHARACTERISTICS_DLL 0x230F
#了如
},{
    /* IMAGE_OPTIONAL_HEADER opthdr */
    /* Standard fields. */
#如定义 TCC_TARGET_X86_64
    0x020B, /*WORD    Magic; */
#另
    0x010B, /*WORD    Magic; */
#了如
    0x06, /*BYTE    MajorLinkerVersion; */
    0x00, /*BYTE    MinorLinkerVersion; */
    0x00000000, /*DWORD   SizeOfCode; */
    0x00000000, /*DWORD   SizeOfInitializedData; */
    0x00000000, /*DWORD   SizeOfUninitializedData; */
    0x00000000, /*DWORD   AddressOfEntryPoint; */
    0x00000000, /*DWORD   BaseOfCode; */
#如未定义 TCC_TARGET_X86_64
    0x00000000, /*DWORD   BaseOfData; */
#了如
    /* NT additional fields. */
#如 已定义(TCC_TARGET_ARM)
    0x00100000,     /*DWORD   ImageBase; */
#另
    0x00400000,     /*DWORD   ImageBase; */
#了如
    0x00001000, /*DWORD   SectionAlignment; */
    0x00000200, /*DWORD   FileAlignment; */
    0x0004, /*WORD    MajorOperatingSystemVersion; */
    0x0000, /*WORD    MinorOperatingSystemVersion; */
    0x0000, /*WORD    MajorImageVersion; */
    0x0000, /*WORD    MinorImageVersion; */
    0x0004, /*WORD    MajorSubsystemVersion; */
    0x0000, /*WORD    MinorSubsystemVersion; */
    0x00000000, /*DWORD   Win32VersionValue; */
    0x00000000, /*DWORD   SizeOfImage; */
    0x00000200, /*DWORD   SizeOfHeaders; */
    0x00000000, /*DWORD   CheckSum; */
    0x0002, /*WORD    Subsystem; */
    0x0000, /*WORD    DllCharacteristics; */
    0x00100000, /*DWORD   SizeOfStackReserve; */
    0x00001000, /*DWORD   SizeOfStackCommit; */
    0x00100000, /*DWORD   SizeOfHeapReserve; */
    0x00001000, /*DWORD   SizeOfHeapCommit; */
    0x00000000, /*DWORD   LoaderFlags; */
    0x00000010, /*DWORD   NumberOfRvaAndSizes; */

    /* IMAGE_DATA_DIRECTORY DataDirectory[16]; */
    {{0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
     {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}}
    }};

    结构 pe_header pe_header = pe_template;

    整 i;
    FILE *op;
    DWORD file_offset, sum;
    结构 section_info *si;
    IMAGE_SECTION_HEADER *psh;

    op = fopen(pe->filename, "wb");
    如 (NULL == op) {
        tcc_error_noabort("could not write '%s': %s", pe->filename, strerror(errno));
        返回 -1;
    }

    pe->sizeofheaders = pe_file_align(pe,
        求长度 (结构 pe_header)
        + pe->sec_count * 求长度 (IMAGE_SECTION_HEADER)
        );

    file_offset = pe->sizeofheaders;

    如 (2 == pe->s1->verbose)
        printf("-------------------------------"
               "\n  virt   file   size  section" "\n");
    对于 (i = 0; i < pe->sec_count; ++i) {
        DWORD addr, size;
        不变 字 *sh_name;

        si = pe->sec_info + i;
        sh_name = si->name;
        addr = si->sh_addr - pe->imagebase;
        size = si->sh_size;
        psh = &si->ish;

        如 (2 == pe->s1->verbose)
            printf("%6x %6x %6x  %s\n",
                (无符)addr, (无符)file_offset, (无符)size, sh_name);

        转接 (si->cls) {
            事例 sec_text:
                pe_header.opthdr.BaseOfCode = addr;
                跳出;

            事例 sec_data:
#如未定义 TCC_TARGET_X86_64
                pe_header.opthdr.BaseOfData = addr;
#了如
                跳出;

            事例 sec_bss:
                跳出;

            事例 sec_reloc:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_BASERELOC, addr, size);
                跳出;

            事例 sec_rsrc:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_RESOURCE, addr, size);
                跳出;

            事例 sec_pdata:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_EXCEPTION, addr, size);
                跳出;

            事例 sec_stab:
                跳出;
        }

        如 (pe->thunk == pe->s1->sections[si->ord]) {
            如 (pe->imp_size) {
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_IMPORT,
                    pe->imp_offs + addr, pe->imp_size);
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_IAT,
                    pe->iat_offs + addr, pe->iat_size);
            }
            如 (pe->exp_size) {
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_EXPORT,
                    pe->exp_offs + addr, pe->exp_size);
            }
        }

        strncpy((字*)psh->Name, sh_name, 求长度 psh->Name);

        psh->Characteristics = pe_sec_flags[si->cls];
        psh->VirtualAddress = addr;
        psh->Misc.VirtualSize = size;
        pe_header.opthdr.SizeOfImage =
            umax(pe_virtual_align(pe, size + addr), pe_header.opthdr.SizeOfImage);

        如 (si->data_size) {
            psh->PointerToRawData = file_offset;
            file_offset = pe_file_align(pe, file_offset + si->data_size);
            psh->SizeOfRawData = file_offset - psh->PointerToRawData;
            如 (si->cls == sec_text)
                pe_header.opthdr.SizeOfCode += psh->SizeOfRawData;
            另
                pe_header.opthdr.SizeOfInitializedData += psh->SizeOfRawData;
        }
    }

    //pe_header.filehdr.TimeDateStamp = time(NULL);
    pe_header.filehdr.NumberOfSections = pe->sec_count;
    pe_header.opthdr.AddressOfEntryPoint = pe->start_addr;
    pe_header.opthdr.SizeOfHeaders = pe->sizeofheaders;
    pe_header.opthdr.ImageBase = pe->imagebase;
    pe_header.opthdr.Subsystem = pe->subsystem;
    如 (pe->s1->pe_stack_size)
        pe_header.opthdr.SizeOfStackReserve = pe->s1->pe_stack_size;
    如 (PE_DLL == pe->type)
        pe_header.filehdr.Characteristics = CHARACTERISTICS_DLL;
    pe_header.filehdr.Characteristics |= pe->s1->pe_characteristics;

    sum = 0;
    pe_fwrite(&pe_header, 求长度 pe_header, op, &sum);
    对于 (i = 0; i < pe->sec_count; ++i)
        pe_fwrite(&pe->sec_info[i].ish, 求长度(IMAGE_SECTION_HEADER), op, &sum);
    pe_fpad(op, pe->sizeofheaders);
    对于 (i = 0; i < pe->sec_count; ++i) {
        si = pe->sec_info + i;
        psh = &si->ish;
        如 (si->data_size) {
            pe_fwrite(si->data, si->data_size, op, &sum);
            file_offset = psh->PointerToRawData + psh->SizeOfRawData;
            pe_fpad(op, file_offset);
        }
    }

    pe_header.opthdr.CheckSum = sum + file_offset;
    fseek(op, offsetof(结构 pe_header, opthdr.CheckSum), SEEK_SET);
    pe_fwrite(&pe_header.opthdr.CheckSum, 求长度 pe_header.opthdr.CheckSum, op, NULL);
    fclose (op);

    如 (2 == pe->s1->verbose)
        printf("-------------------------------\n");
    如 (pe->s1->verbose)
        printf("<- %s (%u bytes)\n", pe->filename, (无符)file_offset);

    返回 0;
}

/*----------------------------------------------------------------------------*/

静态 结构 import_symbol *pe_add_import(结构 pe_info *pe, 整 sym_index)
{
    整 i;
    整 dll_index;
    结构 pe_import_info *p;
    结构 import_symbol *s;
    ElfW(Sym) *isym;

    isym = (ElfW(Sym) *)pe->s1->dynsymtab_section->data + sym_index;
    dll_index = isym->st_size;

    i = dynarray_assoc ((空**)pe->imp_info, pe->imp_count, dll_index);
    如 (-1 != i) {
        p = pe->imp_info[i];
        跳转 found_dll;
    }
    p = tcc_mallocz(求长度 *p);
    p->dll_index = dll_index;
    dynarray_add(&pe->imp_info, &pe->imp_count, p);

found_dll:
    i = dynarray_assoc ((空**)p->symbols, p->sym_count, sym_index);
    如 (-1 != i)
        返回 p->symbols[i];

    s = tcc_mallocz(求长度 *s);
    dynarray_add(&p->symbols, &p->sym_count, s);
    s->sym_index = sym_index;
    返回 s;
}

空 pe_free_imports(结构 pe_info *pe)
{
    整 i;
    对于 (i = 0; i < pe->imp_count; ++i) {
        结构 pe_import_info *p = pe->imp_info[i];
        dynarray_reset(&p->symbols, &p->sym_count);
    }
    dynarray_reset(&pe->imp_info, &pe->imp_count);
}

/*----------------------------------------------------------------------------*/
静态 空 pe_build_imports(结构 pe_info *pe)
{
    整 thk_ptr, ent_ptr, dll_ptr, sym_cnt, i;
    DWORD rva_base = pe->thunk->sh_addr - pe->imagebase;
    整 ndlls = pe->imp_count;

    对于 (sym_cnt = i = 0; i < ndlls; ++i)
        sym_cnt += pe->imp_info[i]->sym_count;

    如 (0 == sym_cnt)
        返回;

    pe_align_section(pe->thunk, 16);

    pe->imp_offs = dll_ptr = pe->thunk->data_offset;
    pe->imp_size = (ndlls + 1) * 求长度(IMAGE_IMPORT_DESCRIPTOR);
    pe->iat_offs = dll_ptr + pe->imp_size;
    pe->iat_size = (sym_cnt + ndlls) * 求长度(ADDR3264);
    section_ptr_add(pe->thunk, pe->imp_size + 2*pe->iat_size);

    thk_ptr = pe->iat_offs;
    ent_ptr = pe->iat_offs + pe->iat_size;

    对于 (i = 0; i < pe->imp_count; ++i) {
        IMAGE_IMPORT_DESCRIPTOR *hdr;
        整 k, n, dllindex;
        ADDR3264 v;
        结构 pe_import_info *p = pe->imp_info[i];
        不变 字 *name;
        DLLReference *dllref;

        dllindex = p->dll_index;
        如 (dllindex)
            name = (dllref = pe->s1->loaded_dlls[dllindex-1])->name;
        另
            name = "", dllref = NULL;

        /* put the dll name into the import header */
        v = put_elf_str(pe->thunk, name);
        hdr = (IMAGE_IMPORT_DESCRIPTOR*)(pe->thunk->data + dll_ptr);
        hdr->FirstThunk = thk_ptr + rva_base;
        hdr->OriginalFirstThunk = ent_ptr + rva_base;
        hdr->Name = v + rva_base;

        对于 (k = 0, n = p->sym_count; k <= n; ++k) {
            如 (k < n) {
                整 iat_index = p->symbols[k]->iat_index;
                整 sym_index = p->symbols[k]->sym_index;
                ElfW(Sym) *imp_sym = (ElfW(Sym) *)pe->s1->dynsymtab_section->data + sym_index;
                ElfW(Sym) *org_sym = (ElfW(Sym) *)symtab_section->data + iat_index;
                不变 字 *name = (字*)pe->s1->dynsymtab_section->link->data + imp_sym->st_name;
                整 ordinal;

                org_sym->st_value = thk_ptr;
                org_sym->st_shndx = pe->thunk->sh_num;

                如 (dllref)
                    v = 0, ordinal = imp_sym->st_value; /* ordinal from pe_load_def */
                另
                    ordinal = 0, v = imp_sym->st_value; /* address from tcc_add_symbol() */

#如定义 TCC_IS_NATIVE
                如 (pe->type == PE_RUN) {
                    如 (dllref) {
                        如 ( !dllref->handle )
                            dllref->handle = LoadLibrary(dllref->name);
                        v = (ADDR3264)GetProcAddress(dllref->handle, ordinal?(字*)0+ordinal:name);
                    }
                    如 (!v)
                        tcc_error_noabort("can't build symbol '%s'", name);
                } 另
#了如
                如 (ordinal) {
                    v = ordinal | (ADDR3264)1 << (求长度(ADDR3264)*8 - 1);
                } 另 {
                    v = pe->thunk->data_offset + rva_base;
                    section_ptr_add(pe->thunk, 求长度(WORD)); /* hint, not used */
                    put_elf_str(pe->thunk, name);
                }

            } 另 {
                v = 0; /* last entry is zero */
            }

            *(ADDR3264*)(pe->thunk->data+thk_ptr) =
            *(ADDR3264*)(pe->thunk->data+ent_ptr) = v;
            thk_ptr += 求长度 (ADDR3264);
            ent_ptr += 求长度 (ADDR3264);
        }
        dll_ptr += 求长度(IMAGE_IMPORT_DESCRIPTOR);
    }
}

/* ------------------------------------------------------------- */

结构 pe_sort_sym
{
    整 index;
    不变 字 *name;
};

静态 整 sym_cmp(不变 空 *va, 不变 空 *vb)
{
    不变 字 *ca = (*(结构 pe_sort_sym**)va)->name;
    不变 字 *cb = (*(结构 pe_sort_sym**)vb)->name;
    返回 strcmp(ca, cb);
}

静态 空 pe_build_exports(结构 pe_info *pe)
{
    ElfW(Sym) *sym;
    整 sym_index, sym_end;
    DWORD rva_base, func_o, name_o, ord_o, str_o;
    IMAGE_EXPORT_DIRECTORY *hdr;
    整 sym_count, ord;
    结构 pe_sort_sym **sorted, *p;

    FILE *op;
    字 buf[260];
    不变 字 *dllname;
    不变 字 *name;

    rva_base = pe->thunk->sh_addr - pe->imagebase;
    sym_count = 0, sorted = NULL, op = NULL;

    sym_end = symtab_section->data_offset / 求长度(ElfW(Sym));
    对于 (sym_index = 1; sym_index < sym_end; ++sym_index) {
        sym = (ElfW(Sym)*)symtab_section->data + sym_index;
        name = pe_export_name(pe->s1, sym);
        如 ((sym->st_other & ST_PE_EXPORT)
            /* export only symbols from actually written sections */
            && pe->s1->sections[sym->st_shndx]->sh_addr) {
            p = tcc_malloc(求长度 *p);
            p->index = sym_index;
            p->name = name;
            dynarray_add(&sorted, &sym_count, p);
        }
#如 0
        如 (sym->st_other & ST_PE_EXPORT)
            printf("export: %s\n", name);
        如 (sym->st_other & ST_PE_STDCALL)
            printf("stdcall: %s\n", name);
#了如
    }

    如 (0 == sym_count)
        返回;

    qsort (sorted, sym_count, 求长度 *sorted, sym_cmp);

    pe_align_section(pe->thunk, 16);
    dllname = tcc_basename(pe->filename);

    pe->exp_offs = pe->thunk->data_offset;
    func_o = pe->exp_offs + 求长度(IMAGE_EXPORT_DIRECTORY);
    name_o = func_o + sym_count * 求长度 (DWORD);
    ord_o = name_o + sym_count * 求长度 (DWORD);
    str_o = ord_o + sym_count * 求长度(WORD);

    hdr = section_ptr_add(pe->thunk, str_o - pe->exp_offs);
    hdr->Characteristics        = 0;
    hdr->Base                   = 1;
    hdr->NumberOfFunctions      = sym_count;
    hdr->NumberOfNames          = sym_count;
    hdr->AddressOfFunctions     = func_o + rva_base;
    hdr->AddressOfNames         = name_o + rva_base;
    hdr->AddressOfNameOrdinals  = ord_o + rva_base;
    hdr->Name                   = str_o + rva_base;
    put_elf_str(pe->thunk, dllname);

#如 1
    /* automatically write exports to <output-filename>.def */
    pstrcpy(buf, 求长度 buf, pe->filename);
    strcpy(tcc_fileextension(buf), ".def");
    op = fopen(buf, "w");
    如 (NULL == op) {
        tcc_error_noabort("could not create '%s': %s", buf, strerror(errno));
    } 另 {
        fprintf(op, "LIBRARY %s\n\nEXPORTS\n", dllname);
        如 (pe->s1->verbose)
            printf("<- %s (%d symbol%s)\n", buf, sym_count, &"s"[sym_count < 2]);
    }
#了如

    对于 (ord = 0; ord < sym_count; ++ord)
    {
        p = sorted[ord], sym_index = p->index, name = p->name;
        /* insert actual address later in pe_relocate_rva */
        put_elf_reloc(symtab_section, pe->thunk,
            func_o, R_XXX_RELATIVE, sym_index);
        *(DWORD*)(pe->thunk->data + name_o)
            = pe->thunk->data_offset + rva_base;
        *(WORD*)(pe->thunk->data + ord_o)
            = ord;
        put_elf_str(pe->thunk, name);
        func_o += 求长度 (DWORD);
        name_o += 求长度 (DWORD);
        ord_o += 求长度 (WORD);
        如 (op)
            fprintf(op, "%s\n", name);
    }
    pe->exp_size = pe->thunk->data_offset - pe->exp_offs;
    dynarray_reset(&sorted, &sym_count);
    如 (op)
        fclose(op);
}

/* ------------------------------------------------------------- */
静态 空 pe_build_reloc (结构 pe_info *pe)
{
    DWORD offset, block_ptr, addr;
    整 count, i;
    ElfW_Rel *rel, *rel_end;
    Section *s = NULL, *sr;

    offset = addr = block_ptr = count = i = 0;
    rel = rel_end = NULL;

    对于(;;) {
        如 (rel < rel_end) {
            整 type = ELFW(R_TYPE)(rel->r_info);
            addr = rel->r_offset + s->sh_addr;
            ++ rel;
            如 (type != REL_TYPE_DIRECT)
                继续;
            如 (count == 0) { /* new block */
                block_ptr = pe->reloc->data_offset;
                section_ptr_add(pe->reloc, 求长度(结构 pe_reloc_header));
                offset = addr & 0xFFFFFFFF<<12;
            }
            如 ((addr -= offset)  < (1<<12)) { /* one block spans 4k addresses */
                WORD *wp = section_ptr_add(pe->reloc, 求长度 (WORD));
                *wp = addr | IMAGE_REL_BASED_HIGHLOW<<12;
                ++count;
                继续;
            }
            -- rel;

        } 另 如 (i < pe->sec_count) {
            sr = (s = pe->s1->sections[pe->sec_info[i++].ord])->reloc;
            如 (sr) {
                rel = (ElfW_Rel *)sr->data;
                rel_end = (ElfW_Rel *)(sr->data + sr->data_offset);
            }
            继续;
        }

        如 (count) {
            /* store the last block and ready for a new one */
            结构 pe_reloc_header *hdr;
            如 (count & 1) /* align for DWORDS */
                section_ptr_add(pe->reloc, 求长度(WORD)), ++count;
            hdr = (结构 pe_reloc_header *)(pe->reloc->data + block_ptr);
            hdr -> offset = offset - pe->imagebase;
            hdr -> size = count * 求长度(WORD) + 求长度(结构 pe_reloc_header);
            count = 0;
        }

        如 (rel >= rel_end)
            跳出;
    }
}

/* ------------------------------------------------------------- */
静态 整 pe_section_class(Section *s)
{
    整 type, flags;
    不变 字 *name;

    type = s->sh_type;
    flags = s->sh_flags;
    name = s->name;
    如 (flags & SHF_ALLOC) {
        如 (type == SHT_PROGBITS) {
            如 (flags & SHF_EXECINSTR)
                返回 sec_text;
            如 (flags & SHF_WRITE)
                返回 sec_data;
            如 (0 == strcmp(name, ".rsrc"))
                返回 sec_rsrc;
            如 (0 == strcmp(name, ".iedat"))
                返回 sec_idata;
            如 (0 == strcmp(name, ".pdata"))
                返回 sec_pdata;
            返回 sec_other;
        } 另 如 (type == SHT_NOBITS) {
            如 (flags & SHF_WRITE)
                返回 sec_bss;
        }
    } 另 {
        如 (0 == strcmp(name, ".reloc"))
            返回 sec_reloc;
        如 (0 == strncmp(name, ".stab", 5)) /* .stab and .stabstr */
            返回 sec_stab;
    }
    返回 -1;
}

静态 整 pe_assign_addresses (结构 pe_info *pe)
{
    整 i, k, o, c;
    DWORD addr;
    整 *section_order;
    结构 section_info *si;
    Section *s;

    如 (PE_DLL == pe->type)
        pe->reloc = new_section(pe->s1, ".reloc", SHT_PROGBITS, 0);

    // pe->thunk = new_section(pe->s1, ".iedat", SHT_PROGBITS, SHF_ALLOC);

    section_order = tcc_malloc(pe->s1->nb_sections * 求长度 (整));
    对于 (o = k = 0 ; k < sec_last; ++k) {
        对于 (i = 1; i < pe->s1->nb_sections; ++i) {
            s = pe->s1->sections[i];
            如 (k == pe_section_class(s)) {
                // printf("%s %d\n", s->name, k);
                s->sh_addr = pe->imagebase;
                section_order[o++] = i;
            }
        }
    }

    pe->sec_info = tcc_mallocz(o * 求长度 (结构 section_info));
    addr = pe->imagebase + 1;

    对于 (i = 0; i < o; ++i)
    {
        k = section_order[i];
        s = pe->s1->sections[k];
        c = pe_section_class(s);
        si = &pe->sec_info[pe->sec_count];

#如定义 PE_MERGE_DATA
        如 (c == sec_bss && pe->sec_count && si[-1].cls == sec_data) {
            /* append .bss to .data */
            s->sh_addr = addr = ((addr-1) | (s->sh_addralign-1)) + 1;
            addr += s->data_offset;
            si[-1].sh_size = addr - si[-1].sh_addr;
            继续;
        }
#了如
        如 (c == sec_stab && 0 == pe->s1->do_debug)
            继续;

        strcpy(si->name, s->name);
        si->cls = c;
        si->ord = k;
        si->sh_addr = s->sh_addr = addr = pe_virtual_align(pe, addr);
        si->sh_flags = s->sh_flags;

        如 (c == sec_data && NULL == pe->thunk)
            pe->thunk = s;

        如 (s == pe->thunk) {
            pe_build_imports(pe);
            pe_build_exports(pe);
        }

        如 (c == sec_reloc)
            pe_build_reloc (pe);

        如 (s->data_offset)
        {
            如 (s->sh_type != SHT_NOBITS) {
                si->data = s->data;
                si->data_size = s->data_offset;
            }

            addr += s->data_offset;
            si->sh_size = s->data_offset;
            ++pe->sec_count;
        }
        // printf("%08x %05x %s\n", si->sh_addr, si->sh_size, si->name);
    }

#如 0
    对于 (i = 1; i < pe->s1->nb_sections; ++i) {
        Section *s = pe->s1->sections[i];
        整 type = s->sh_type;
        整 flags = s->sh_flags;
        printf("section %-16s %-10s %5x %s,%s,%s\n",
            s->name,
            type == SHT_PROGBITS ? "progbits" :
            type == SHT_NOBITS ? "nobits" :
            type == SHT_SYMTAB ? "symtab" :
            type == SHT_STRTAB ? "strtab" :
            type == SHT_RELX ? "rel" : "???",
            s->data_offset,
            flags & SHF_ALLOC ? "alloc" : "",
            flags & SHF_WRITE ? "write" : "",
            flags & SHF_EXECINSTR ? "exec" : ""
            );
    }
    pe->s1->verbose = 2;
#了如

    tcc_free(section_order);
    返回 0;
}

/* ------------------------------------------------------------- */
静态 空 pe_relocate_rva (结构 pe_info *pe, Section *s)
{
    Section *sr = s->reloc;
    ElfW_Rel *rel, *rel_end;
    rel_end = (ElfW_Rel *)(sr->data + sr->data_offset);
    对于(rel = (ElfW_Rel *)sr->data; rel < rel_end; rel++) {
        如 (ELFW(R_TYPE)(rel->r_info) == R_XXX_RELATIVE) {
            整 sym_index = ELFW(R_SYM)(rel->r_info);
            DWORD addr = s->sh_addr;
            如 (sym_index) {
                ElfW(Sym) *sym = (ElfW(Sym) *)symtab_section->data + sym_index;
                addr = sym->st_value;
            }
            // printf("reloc rva %08x %08x %s\n", (DWORD)rel->r_offset, addr, s->name);
            *(DWORD*)(s->data + rel->r_offset) += addr - pe->imagebase;
        }
    }
}

/*----------------------------------------------------------------------------*/

静态 整 pe_isafunc(整 sym_index)
{
    Section *sr = text_section->reloc;
    ElfW_Rel *rel, *rel_end;
    Elf32_Word info = ELF32_R_INFO(sym_index, R_386_PC32);
    如 (!sr)
        返回 0;
    rel_end = (ElfW_Rel *)(sr->data + sr->data_offset);
    对于 (rel = (ElfW_Rel *)sr->data; rel < rel_end; rel++)
        如 (rel->r_info == info)
            返回 1;
    返回 0;
}

/*----------------------------------------------------------------------------*/
静态 整 pe_check_symbols(结构 pe_info *pe)
{
    ElfW(Sym) *sym;
    整 sym_index, sym_end;
    整 ret = 0;

    pe_align_section(text_section, 8);

    sym_end = symtab_section->data_offset / 求长度(ElfW(Sym));
    对于 (sym_index = 1; sym_index < sym_end; ++sym_index) {

        sym = (ElfW(Sym) *)symtab_section->data + sym_index;
        如 (sym->st_shndx == SHN_UNDEF) {

            不变 字 *name = (字*)symtab_section->link->data + sym->st_name;
            无符 type = ELFW(ST_TYPE)(sym->st_info);
            整 imp_sym = pe_find_import(pe->s1, sym);
            结构 import_symbol *is;

            如 (imp_sym <= 0)
                跳转 not_found;

            如 (type == STT_NOTYPE) {
                /* symbols from assembler have no type, find out which */
                如 (pe_isafunc(sym_index))
                    type = STT_FUNC;
                另
                    type = STT_OBJECT;
            }

            is = pe_add_import(pe, imp_sym);

            如 (type == STT_FUNC) {
                无符 长 offset = is->thk_offset;
                如 (offset) {
                    /* got aliased symbol, like stricmp and _stricmp */

                } 另 {
                    字 buffer[100];
                    WORD *p;

                    offset = text_section->data_offset;
                    /* add the 'jmp IAT[x]' instruction */
#如定义 TCC_TARGET_ARM
                    p = section_ptr_add(text_section, 8+4); // room for code and address
                    (*(DWORD*)(p)) = 0xE59FC000; // arm code ldr ip, [pc] ; PC+8+0 = 0001xxxx
                    (*(DWORD*)(p+2)) = 0xE59CF000; // arm code ldr pc, [ip]
#另
                    p = section_ptr_add(text_section, 8);
                    *p = 0x25FF;
#如定义 TCC_TARGET_X86_64
                    *(DWORD*)(p+1) = (DWORD)-4;
#了如
#了如
                    /* add a helper symbol, will be patched later in
                       pe_build_imports */
                    sprintf(buffer, "IAT.%s", name);
                    is->iat_index = put_elf_sym(
                        symtab_section, 0, 求长度(DWORD),
                        ELFW(ST_INFO)(STB_GLOBAL, STT_OBJECT),
                        0, SHN_UNDEF, buffer);
#如定义 TCC_TARGET_ARM
                    put_elf_reloc(symtab_section, text_section,
                        offset + 8, R_XXX_THUNKFIX, is->iat_index); // offset to IAT position
#另
                    put_elf_reloc(symtab_section, text_section, 
                        offset + 2, R_XXX_THUNKFIX, is->iat_index);
#了如
                    is->thk_offset = offset;
                }

                /* tcc_realloc might have altered sym's address */
                sym = (ElfW(Sym) *)symtab_section->data + sym_index;

                /* patch the original symbol */
                sym->st_value = offset;
                sym->st_shndx = text_section->sh_num;
                sym->st_other &= ~ST_PE_EXPORT; /* do not export */
                继续;
            }

            如 (type == STT_OBJECT) { /* data, ptr to that should be */
                如 (0 == is->iat_index) {
                    /* original symbol will be patched later in pe_build_imports */
                    is->iat_index = sym_index;
                    继续;
                }
            }

        not_found:
            如 (ELFW(ST_BIND)(sym->st_info) == STB_WEAK)
                /* STB_WEAK undefined symbols are accepted */
                继续;
            tcc_error_noabort("undefined symbol '%s'%s", name,
                imp_sym < 0 ? ", missing __declspec(dllimport)?":"");
            ret = -1;

        } 另 如 (pe->s1->rdynamic
                   && ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
            /* if -rdynamic option, then export all non local symbols */
            sym->st_other |= ST_PE_EXPORT;
        }
    }
    返回 ret;
}

/*----------------------------------------------------------------------------*/
#如定义 PE_PRINT_SECTIONS
静态 空 pe_print_section(FILE * f, Section * s)
{
    /* just if you're curious */
    BYTE *p, *e, b;
    整 i, n, l, m;
    p = s->data;
    e = s->data + s->data_offset;
    l = e - p;

    fprintf(f, "section  \"%s\"", s->name);
    如 (s->link)
        fprintf(f, "\nlink     \"%s\"", s->link->name);
    如 (s->reloc)
        fprintf(f, "\nreloc    \"%s\"", s->reloc->name);
    fprintf(f, "\nv_addr   %08X", (无符)s->sh_addr);
    fprintf(f, "\ncontents %08X", (无符)l);
    fprintf(f, "\n\n");

    如 (s->sh_type == SHT_NOBITS)
        返回;

    如 (0 == l)
        返回;

    如 (s->sh_type == SHT_SYMTAB)
        m = 求长度(ElfW(Sym));
    另 如 (s->sh_type == SHT_RELX)
        m = 求长度(ElfW_Rel);
    另
        m = 16;

    fprintf(f, "%-8s", "offset");
    对于 (i = 0; i < m; ++i)
        fprintf(f, " %02x", i);
    n = 56;

    如 (s->sh_type == SHT_SYMTAB || s->sh_type == SHT_RELX) {
        不变 字 *fields1[] = {
            "name",
            "value",
            "size",
            "bind",
            "type",
            "other",
            "shndx",
            NULL
        };

        不变 字 *fields2[] = {
            "offs",
            "type",
            "symb",
            NULL
        };

        不变 字 **p;

        如 (s->sh_type == SHT_SYMTAB)
            p = fields1, n = 106;
        另
            p = fields2, n = 58;

        对于 (i = 0; p[i]; ++i)
            fprintf(f, "%6s", p[i]);
        fprintf(f, "  symbol");
    }

    fprintf(f, "\n");
    对于 (i = 0; i < n; ++i)
        fprintf(f, "-");
    fprintf(f, "\n");

    对于 (i = 0; i < l;)
    {
        fprintf(f, "%08X", i);
        对于 (n = 0; n < m; ++n) {
            如 (n + i < l)
                fprintf(f, " %02X", p[i + n]);
            另
                fprintf(f, "   ");
        }

        如 (s->sh_type == SHT_SYMTAB) {
            ElfW(Sym) *sym = (ElfW(Sym) *) (p + i);
            不变 字 *name = s->link->data + sym->st_name;
            fprintf(f, "  %04X  %04X  %04X   %02X    %02X    %02X   %04X  \"%s\"",
                    (无符)sym->st_name,
                    (无符)sym->st_value,
                    (无符)sym->st_size,
                    (无符)ELFW(ST_BIND)(sym->st_info),
                    (无符)ELFW(ST_TYPE)(sym->st_info),
                    (无符)sym->st_other,
                    (无符)sym->st_shndx,
                    name);

        } 另 如 (s->sh_type == SHT_RELX) {
            ElfW_Rel *rel = (ElfW_Rel *) (p + i);
            ElfW(Sym) *sym =
                (ElfW(Sym) *) s->link->data + ELFW(R_SYM)(rel->r_info);
            不变 字 *name = s->link->link->data + sym->st_name;
            fprintf(f, "  %04X   %02X   %04X  \"%s\"",
                    (无符)rel->r_offset,
                    (无符)ELFW(R_TYPE)(rel->r_info),
                    (无符)ELFW(R_SYM)(rel->r_info),
                    name);
        } 另 {
            fprintf(f, "   ");
            对于 (n = 0; n < m; ++n) {
                如 (n + i < l) {
                    b = p[i + n];
                    如 (b < 32 || b >= 127)
                        b = '.';
                    fprintf(f, "%c", b);
                }
            }
        }
        i += m;
        fprintf(f, "\n");
    }
    fprintf(f, "\n\n");
}

静态 空 pe_print_sections(TCCState *s1, 不变 字 *fname)
{
    Section *s;
    FILE *f;
    整 i;
    f = fopen(fname, "w");
    对于 (i = 1; i < s1->nb_sections; ++i) {
        s = s1->sections[i];
        pe_print_section(f, s);
    }
    pe_print_section(f, s1->dynsymtab_section);
    fclose(f);
}
#了如

/* ------------------------------------------------------------- */
/* helper function for load/store to insert one more indirection */

#如未定义 TCC_TARGET_ARM
ST_FUNC SValue *pe_getimport(SValue *sv, SValue *v2)
{
    整 r2;
    如 ((sv->r & (VT_VALMASK|VT_SYM)) != (VT_CONST|VT_SYM) || (sv->r2 != VT_CONST))
        返回 sv;
    如 (!sv->sym->a.dllimport)
        返回 sv;
    // printf("import %04x %04x %04x %s\n", sv->type.t, sv->sym->type.t, sv->r, get_tok_str(sv->sym->v, NULL));
    memset(v2, 0, 求长度 *v2);
    v2->type.t = VT_PTR;
    v2->r = VT_CONST | VT_SYM | VT_LVAL;
    v2->sym = sv->sym;

    r2 = get_reg(RC_INT);
    load(r2, v2);
    v2->r = r2;
    如 ((uint32_t)sv->c.i) {
        vpushv(v2);
        vpushi(sv->c.i);
        gen_opi('+');
        *v2 = *vtop--;
    }
    v2->type.t = sv->type.t;
    v2->r |= sv->r & VT_LVAL;
    返回 v2;
}
#了如

ST_FUNC 整 pe_putimport(TCCState *s1, 整 dllindex, 不变 字 *name, addr_t value)
{
    返回 set_elf_sym(
        s1->dynsymtab_section,
        value,
        dllindex, /* st_size */
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE),
        0,
        value ? SHN_ABS : SHN_UNDEF,
        name
        );
}

静态 整 add_dllref(TCCState *s1, 不变 字 *dllname)
{
    DLLReference *dllref;
    整 i;
    对于 (i = 0; i < s1->nb_loaded_dlls; ++i)
        如 (0 == strcmp(s1->loaded_dlls[i]->name, dllname))
            返回 i + 1;
    dllref = tcc_mallocz(求长度(DLLReference) + strlen(dllname));
    strcpy(dllref->name, dllname);
    dynarray_add(&s1->loaded_dlls, &s1->nb_loaded_dlls, dllref);
    返回 s1->nb_loaded_dlls;
}

/* ------------------------------------------------------------- */

静态 整 read_mem(整 fd, 无符 offset, 空 *buffer, 无符 len)
{
    lseek(fd, offset, SEEK_SET);
    返回 len == read(fd, buffer, len);
}

/* ------------------------------------------------------------- */

PUB_FUNC 整 tcc_get_dllexports(不变 字 *filename, 字 **pp)
{
    整 l, i, n, n0, ret;
    字 *p;
    整 fd;

    IMAGE_SECTION_HEADER ish;
    IMAGE_EXPORT_DIRECTORY ied;
    IMAGE_DOS_HEADER dh;
    IMAGE_FILE_HEADER ih;
    DWORD sig, ref, addr, ptr, namep;

    整 pef_hdroffset, opt_hdroffset, sec_hdroffset;

    n = n0 = 0;
    p = NULL;
    ret = -1;

    fd = open(filename, O_RDONLY | O_BINARY);
    如 (fd < 0)
        跳转 the_end_1;
    ret = 1;
    如 (!read_mem(fd, 0, &dh, 求长度 dh))
        跳转 the_end;
    如 (!read_mem(fd, dh.e_lfanew, &sig, 求长度 sig))
        跳转 the_end;
    如 (sig != 0x00004550)
        跳转 the_end;
    pef_hdroffset = dh.e_lfanew + 求长度 sig;
    如 (!read_mem(fd, pef_hdroffset, &ih, 求长度 ih))
        跳转 the_end;
    opt_hdroffset = pef_hdroffset + 求长度 ih;
    如 (ih.Machine == 0x014C) {
        IMAGE_OPTIONAL_HEADER32 oh;
        sec_hdroffset = opt_hdroffset + 求长度 oh;
        如 (!read_mem(fd, opt_hdroffset, &oh, 求长度 oh))
            跳转 the_end;
        如 (IMAGE_DIRECTORY_ENTRY_EXPORT >= oh.NumberOfRvaAndSizes)
            跳转 the_end_0;
        addr = oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    } 另 如 (ih.Machine == 0x8664) {
        IMAGE_OPTIONAL_HEADER64 oh;
        sec_hdroffset = opt_hdroffset + 求长度 oh;
        如 (!read_mem(fd, opt_hdroffset, &oh, 求长度 oh))
            跳转 the_end;
        如 (IMAGE_DIRECTORY_ENTRY_EXPORT >= oh.NumberOfRvaAndSizes)
            跳转 the_end_0;
        addr = oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    } 另
        跳转 the_end;

    //printf("addr: %08x\n", addr);
    对于 (i = 0; i < ih.NumberOfSections; ++i) {
        如 (!read_mem(fd, sec_hdroffset + i * 求长度 ish, &ish, 求长度 ish))
            跳转 the_end;
        //printf("vaddr: %08x\n", ish.VirtualAddress);
        如 (addr >= ish.VirtualAddress && addr < ish.VirtualAddress + ish.SizeOfRawData)
            跳转 found;
    }
    跳转 the_end_0;

found:
    ref = ish.VirtualAddress - ish.PointerToRawData;
    如 (!read_mem(fd, addr - ref, &ied, 求长度 ied))
        跳转 the_end;

    namep = ied.AddressOfNames - ref;
    对于 (i = 0; i < ied.NumberOfNames; ++i) {
        如 (!read_mem(fd, namep, &ptr, 求长度 ptr))
            跳转 the_end;
        namep += 求长度 ptr;
        对于 (l = 0;;) {
            如 (n+1 >= n0)
                p = tcc_realloc(p, n0 = n0 ? n0 * 2 : 256);
            如 (!read_mem(fd, ptr - ref + l++, p + n, 1)) {
                tcc_free(p), p = NULL;
                跳转 the_end;
            }
            如 (p[n++] == 0)
                跳出;
        }
    }
    如 (p)
        p[n] = 0;
the_end_0:
    ret = 0;
the_end:
    close(fd);
the_end_1:
    *pp = p;
    返回 ret;
}

/* -------------------------------------------------------------
 *  This is for compiled windows resources in 'coff' format
 *  as generated by 'windres.exe -O coff ...'.
 */

静态 整 pe_load_res(TCCState *s1, 整 fd)
{
    结构 pe_rsrc_header hdr;
    Section *rsrc_section;
    整 i, ret = -1;
    BYTE *ptr;
    无符 offs;

    如 (!read_mem(fd, 0, &hdr, 求长度 hdr))
        跳转 quit;

    如 (hdr.filehdr.Machine != IMAGE_FILE_MACHINE
        || hdr.filehdr.NumberOfSections != 1
        || strcmp((字*)hdr.sectionhdr.Name, ".rsrc") != 0)
        跳转 quit;

    rsrc_section = new_section(s1, ".rsrc", SHT_PROGBITS, SHF_ALLOC);
    ptr = section_ptr_add(rsrc_section, hdr.sectionhdr.SizeOfRawData);
    offs = hdr.sectionhdr.PointerToRawData;
    如 (!read_mem(fd, offs, ptr, hdr.sectionhdr.SizeOfRawData))
        跳转 quit;
    offs = hdr.sectionhdr.PointerToRelocations;
    对于 (i = 0; i < hdr.sectionhdr.NumberOfRelocations; ++i)
    {
        结构 pe_rsrc_reloc rel;
        如 (!read_mem(fd, offs, &rel, 求长度 rel))
            跳转 quit;
        // printf("rsrc_reloc: %x %x %x\n", rel.offset, rel.size, rel.type);
        如 (rel.type != RSRC_RELTYPE)
            跳转 quit;
        put_elf_reloc(symtab_section, rsrc_section,
            rel.offset, R_XXX_RELATIVE, 0);
        offs += 求长度 rel;
    }
    ret = 0;
quit:
    返回 ret;
}

/* ------------------------------------------------------------- */

静态 字 *trimfront(字 *p)
{
    当 (*p && (无符 字)*p <= ' ')
        ++p;
    返回 p;
}

静态 字 *trimback(字 *a, 字 *e)
{
    当 (e > a && (无符 字)e[-1] <= ' ')
        --e;
    *e = 0;;
    返回 a;
}

/* ------------------------------------------------------------- */
静态 整 pe_load_def(TCCState *s1, 整 fd)
{
    整 state = 0, ret = -1, dllindex = 0, ord;
    字 line[400], dllname[80], *p, *x;
    FILE *fp;

    fp = fdopen(dup(fd), "rb");
    当 (fgets(line, 求长度 line, fp))
    {
        p = trimfront(trimback(line, strchr(line, 0)));
        如 (0 == *p || ';' == *p)
            继续;

        转接 (state) {
        事例 0:
            如 (0 != strnicmp(p, "LIBRARY", 7))
                跳转 quit;
            pstrcpy(dllname, 求长度 dllname, trimfront(p+7));
            ++state;
            继续;

        事例 1:
            如 (0 != stricmp(p, "EXPORTS"))
                跳转 quit;
            ++state;
            继续;

        事例 2:
            dllindex = add_dllref(s1, dllname);
            ++state;
            /* fall through */
        缺省:
            /* get ordinal and will store in sym->st_value */
            ord = 0;
            x = strchr(p, ' ');
            如 (x) {
                *x = 0, x = strrchr(x + 1, '@');
                如 (x) {
                    字 *d;
                    ord = (整)strtol(x + 1, &d, 10);
                    如 (*d)
                        ord = 0;
                }
            }
            pe_putimport(s1, dllindex, p, ord);
            继续;
        }
    }
    ret = 0;
quit:
    fclose(fp);
    返回 ret;
}

/* ------------------------------------------------------------- */
静态 整 pe_load_dll(TCCState *s1, 不变 字 *filename)
{
    字 *p, *q;
    整 index, ret;

    ret = tcc_get_dllexports(filename, &p);
    如 (ret) {
        返回 -1;
    } 另 如 (p) {
        index = add_dllref(s1, tcc_basename(filename));
        对于 (q = p; *q; q += 1 + strlen(q))
            pe_putimport(s1, index, q, 0);
        tcc_free(p);
    }
    返回 0;
}

/* ------------------------------------------------------------- */
ST_FUNC 整 pe_load_file(结构 TCCState *s1, 不变 字 *filename, 整 fd)
{
    整 ret = -1;
    字 buf[10];
    如 (0 == strcmp(tcc_fileextension(filename), ".def"))
        ret = pe_load_def(s1, fd);
    另 如 (pe_load_res(s1, fd) == 0)
        ret = 0;
    另 如 (read_mem(fd, 0, buf, 4) && 0 == memcmp(buf, "MZ\220", 4))
        ret = pe_load_dll(s1, filename);
    返回 ret;
}

/* ------------------------------------------------------------- */
#如定义 TCC_TARGET_X86_64
静态 无符 pe_add_uwwind_info(TCCState *s1)
{
    如 (NULL == s1->uw_pdata) {
        s1->uw_pdata = find_section(tcc_state, ".pdata");
        s1->uw_pdata->sh_addralign = 4;
        s1->uw_sym = put_elf_sym(symtab_section, 0, 0, 0, 0, text_section->sh_num, NULL);
    }

    如 (0 == s1->uw_offs) {
        /* As our functions all have the same stackframe, we use one entry for all */
        静态 不变 无符 字 uw_info[] = {
            0x01, // UBYTE: 3 Version , UBYTE: 5 Flags
            0x04, // UBYTE Size of prolog
            0x02, // UBYTE Count of unwind codes
            0x05, // UBYTE: 4 Frame Register (rbp), UBYTE: 4 Frame Register offset (scaled)
            // USHORT * n Unwind codes array
            // 0x0b, 0x01, 0xff, 0xff, // stack size
            0x04, 0x03, // set frame ptr (mov rsp -> rbp)
            0x01, 0x50  // push reg (rbp)
        };

        Section *s = text_section;
        无符 字 *p;

        section_ptr_add(s, -s->data_offset & 3); /* align */
        s1->uw_offs = s->data_offset;
        p = section_ptr_add(s, 求长度 uw_info);
        memcpy(p, uw_info, 求长度 uw_info);
    }

    返回 s1->uw_offs;
}

ST_FUNC 空 pe_add_unwind_data(无符 start, 无符 end, 无符 stack)
{
    TCCState *s1 = tcc_state;
    Section *pd;
    无符 o, n, d;
    结构 /* _RUNTIME_FUNCTION */ {
      DWORD BeginAddress;
      DWORD EndAddress;
      DWORD UnwindData;
    } *p;

    d = pe_add_uwwind_info(s1);
    pd = s1->uw_pdata;
    o = pd->data_offset;
    p = section_ptr_add(pd, 求长度 *p);

    /* record this function */
    p->BeginAddress = start;
    p->EndAddress = end;
    p->UnwindData = d;

    /* put relocations on it */
    对于 (n = o + 求长度 *p; o < n; o += 求长度 p->BeginAddress)
        put_elf_reloc(symtab_section, pd, o,  R_X86_64_RELATIVE, s1->uw_sym);
}
#了如
/* ------------------------------------------------------------- */
#如定义 TCC_TARGET_X86_64
#定义 PE_STDSYM(n,s) n
#另
#定义 PE_STDSYM(n,s) "_" n s
#了如

静态 空 pe_add_runtime(TCCState *s1, 结构 pe_info *pe)
{
    不变 字 *start_symbol;
    整 pe_type = 0;
    整 unicode_entry = 0;

    如 (find_elf_sym(symtab_section, PE_STDSYM("WinMain","@16")))
        pe_type = PE_GUI;
    另
    如 (find_elf_sym(symtab_section, PE_STDSYM("wWinMain","@16"))) {
        pe_type = PE_GUI;
        unicode_entry = PE_GUI;
    }
    另
    如 (TCC_OUTPUT_DLL == s1->output_type) {
        pe_type = PE_DLL;
        /* need this for 'tccelf.c:relocate_section()' */
        s1->output_type = TCC_OUTPUT_EXE;
    }
    另 {
        pe_type = PE_EXE;
        如 (find_elf_sym(symtab_section, "wmain"))
            unicode_entry = PE_EXE;
    }

    start_symbol =
        TCC_OUTPUT_MEMORY == s1->output_type
        ? PE_GUI == pe_type ? (unicode_entry ? "__runwwinmain" : "__runwinmain")
            : (unicode_entry ? "__runwmain" : "__runmain")
        : PE_DLL == pe_type ? PE_STDSYM("__dllstart","@12")
            : PE_GUI == pe_type ? (unicode_entry ? "__wwinstart": "__winstart")
                : (unicode_entry ? "__wstart" : "__start")
        ;

    如 (!s1->leading_underscore || strchr(start_symbol, '@'))
        ++start_symbol;

    /* grab the startup code from libtcc1 */
#如定义 TCC_IS_NATIVE
    如 (TCC_OUTPUT_MEMORY != s1->output_type || s1->runtime_main)
#了如
    set_elf_sym(symtab_section,
        0, 0,
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE), 0,
        SHN_UNDEF, start_symbol);

    tcc_add_pragma_libs(s1);

    如 (0 == s1->nostdlib) {
        静态 不变 字 *libs[] = {
            TCC_LIBTCC1, "msvcrt", "kernel32", "", "user32", "gdi32", NULL
        };
        不变 字 **pp, *p;
        对于 (pp = libs; 0 != (p = *pp); ++pp) {
            如 (0 == *p) {
                如 (PE_DLL != pe_type && PE_GUI != pe_type)
                    跳出;
            } 另 如 (pp == libs && tcc_add_dll(s1, p, 0) >= 0) {
                继续;
            } 另 {
                tcc_add_library_err(s1, p);
            }
        }
    }

    如 (TCC_OUTPUT_MEMORY == s1->output_type)
        pe_type = PE_RUN;
    pe->type = pe_type;
    pe->start_symbol = start_symbol;
}

静态 空 pe_set_options(TCCState * s1, 结构 pe_info *pe)
{
    如 (PE_DLL == pe->type) {
        /* XXX: check if is correct for arm-pe target */
        pe->imagebase = 0x10000000;
    } 另 {
#如 已定义(TCC_TARGET_ARM)
        pe->imagebase = 0x00010000;
#另
        pe->imagebase = 0x00400000;
#了如
    }

#如 已定义(TCC_TARGET_ARM)
    /* we use "console" subsystem by default */
    pe->subsystem = 9;
#另
    如 (PE_DLL == pe->type || PE_GUI == pe->type)
        pe->subsystem = 2;
    另
        pe->subsystem = 3;
#了如
    /* Allow override via -Wl,-subsystem=... option */
    如 (s1->pe_subsystem != 0)
        pe->subsystem = s1->pe_subsystem;

    /* set default file/section alignment */
    如 (pe->subsystem == 1) {
        pe->section_align = 0x20;
        pe->file_align = 0x20;
    } 另 {
        pe->section_align = 0x1000;
        pe->file_align = 0x200;
    }

    如 (s1->section_align != 0)
        pe->section_align = s1->section_align;
    如 (s1->pe_file_align != 0)
        pe->file_align = s1->pe_file_align;

    如 ((pe->subsystem >= 10) && (pe->subsystem <= 12))
        pe->imagebase = 0;

    如 (s1->has_text_addr)
        pe->imagebase = s1->text_addr;
}

ST_FUNC 整 pe_output_file(TCCState *s1, 不变 字 *filename)
{
    整 ret;
    结构 pe_info pe;
    整 i;

    memset(&pe, 0, 求长度 pe);
    pe.filename = filename;
    pe.s1 = s1;

    tcc_add_bcheck(s1);
    pe_add_runtime(s1, &pe);
    relocate_common_syms(); /* assign bss addresses */
    tcc_add_linker_symbols(s1);
    pe_set_options(s1, &pe);

    ret = pe_check_symbols(&pe);
    如 (ret)
        ;
    另 如 (filename) {
        pe_assign_addresses(&pe);
        relocate_syms(s1, s1->symtab, 0);
        对于 (i = 1; i < s1->nb_sections; ++i) {
            Section *s = s1->sections[i];
            如 (s->reloc) {
                relocate_section(s1, s);
                pe_relocate_rva(&pe, s);
            }
        }
        pe.start_addr = (DWORD)
            ((uintptr_t)tcc_get_symbol_err(s1, pe.start_symbol)
                - pe.imagebase);
        如 (s1->nb_errors)
            ret = -1;
        另
            ret = pe_write(&pe);
        tcc_free(pe.sec_info);
    } 另 {
#如定义 TCC_IS_NATIVE
        pe.thunk = data_section;
        pe_build_imports(&pe);
        s1->runtime_main = pe.start_symbol;
#了如
    }

    pe_free_imports(&pe);

#如定义 PE_PRINT_SECTIONS
    pe_print_sections(s1, "tcc.log");
#了如
    返回 ret;
}

/* ------------------------------------------------------------- */
