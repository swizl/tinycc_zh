/**************************************************************************/
/*  COFF.H                                                                */
/*     COFF data structures and related definitions used by the linker    */
/**************************************************************************/

/*------------------------------------------------------------------------*/
/*  COFF FILE HEADER                                                      */
/*------------------------------------------------------------------------*/
结构 filehdr {
        无符 短  f_magic;        /* magic number */
        无符 短  f_nscns;        /* number of sections */
        长            f_timdat;       /* time & date stamp */
        长            f_symptr;       /* file pointer to symtab */
        长            f_nsyms;        /* number of symtab entries */
        无符 短  f_opthdr;       /* 求长度(optional hdr) */
        无符 短  f_flags;        /* flags */
        无符 短  f_TargetID;     /* for C6x = 0x0099 */
        };

/*------------------------------------------------------------------------*/
/*  File header flags                                                     */
/*------------------------------------------------------------------------*/
#定义  F_RELFLG   0x01       /* relocation info stripped from file       */
#定义  F_EXEC     0x02       /* file is executable (no unresolved refs)  */
#定义  F_LNNO     0x04       /* line numbers stripped from file          */
#定义  F_LSYMS    0x08       /* local symbols stripped from file         */
#定义  F_GSP10    0x10       /* 34010 version                            */
#定义  F_GSP20    0x20       /* 34020 version                            */
#定义  F_SWABD    0x40       /* bytes swabbed (in names)                 */
#定义  F_AR16WR   0x80       /* byte ordering of an AR16WR (PDP-11)      */
#定义  F_LITTLE   0x100      /* byte ordering of an AR32WR (vax)         */
#定义  F_BIG      0x200      /* byte ordering of an AR32W (3B, maxi)     */
#定义  F_PATCH    0x400      /* contains "patch" list in optional header */
#定义  F_NODF     0x400   

#定义 F_VERSION    (F_GSP10  | F_GSP20)   
#定义 F_BYTE_ORDER (F_LITTLE | F_BIG)
#定义 FILHDR  结构 filehdr

/* #定义 FILHSZ  求长度(FILHDR)  */
#定义 FILHSZ  22                /* above rounds to align on 4 bytes which causes problems */

#定义 COFF_C67_MAGIC 0x00c2

/*------------------------------------------------------------------------*/
/*  Macros to recognize magic numbers                                     */
/*------------------------------------------------------------------------*/
#定义 ISMAGIC(x)      (((无符 短)(x))==(无符 短)magic)
#定义 ISARCHIVE(x)    ((((无符 短)(x))==(无符 短)ARTYPE))
#定义 BADMAGIC(x)     (((无符 短)(x) & 0x8080) && !ISMAGIC(x))


/*------------------------------------------------------------------------*/
/*  OPTIONAL FILE HEADER                                                  */
/*------------------------------------------------------------------------*/
类型定义 结构 aouthdr {
        短   magic;          /* see magic.h                          */
        短   vstamp;         /* version stamp                        */
        长    tsize;          /* text size in bytes, padded to FW bdry*/
        长    dsize;          /* initialized data "  "                */
        长    bsize;          /* uninitialized data "   "             */
        长    entrypt;        /* entry pt.                            */
        长    text_start;     /* base of text used for this file      */
        长    data_start;     /* base of data used for this file      */
} AOUTHDR;

#定义 AOUTSZ  求长度(AOUTHDR)

/*----------------------------------------------------------------------*/
/*      When a UNIX aout header is to be built in the optional header,  */
/*      the following magic numbers can appear in that header:          */ 
/*                                                                      */
/*              AOUT1MAGIC : default : readonly sharable text segment   */
/*              AOUT2MAGIC:          : writable text segment            */
/*              PAGEMAGIC  :         : configured for paging            */
/*----------------------------------------------------------------------*/
#定义 AOUT1MAGIC 0410
#定义 AOUT2MAGIC 0407
#定义 PAGEMAGIC  0413


/*------------------------------------------------------------------------*/
/*  COMMON ARCHIVE FILE STRUCTURES                                        */
/*                                                                        */
/*       ARCHIVE File Organization:                                       */
/*       _______________________________________________                  */
/*       |__________ARCHIVE_MAGIC_STRING_______________|                  */
/*       |__________ARCHIVE_FILE_MEMBER_1______________|                  */
/*       |                                             |                  */
/*       |       Archive File Header "ar_hdr"          |                  */
/*       |.............................................|                  */
/*       |       Member Contents                       |                  */
/*       |               1. External symbol directory  |                  */
/*       |               2. Text file                  |                  */
/*       |_____________________________________________|                  */
/*       |________ARCHIVE_FILE_MEMBER_2________________|                  */
/*       |               "ar_hdr"                      |                  */
/*       |.............................................|                  */
/*       |       Member Contents (.o or text file)     |                  */
/*       |_____________________________________________|                  */
/*       |       .               .               .     |                  */
/*       |       .               .               .     |                  */
/*       |       .               .               .     |                  */
/*       |_____________________________________________|                  */
/*       |________ARCHIVE_FILE_MEMBER_n________________|                  */
/*       |               "ar_hdr"                      |                  */
/*       |.............................................|                  */
/*       |               Member Contents               |                  */
/*       |_____________________________________________|                  */
/*                                                                        */
/*------------------------------------------------------------------------*/

#定义 COFF_ARMAG   "!<arch>\n"
#定义 SARMAG  8
#定义 ARFMAG  "`\n"

结构 ar_hdr           /* archive file member header - printable ascii */
{
        字    ar_name[16];    /* file member name - `/' terminated */
        字    ar_date[12];    /* file member date - decimal */
        字    ar_uid[6];      /* file member user id - decimal */
        字    ar_gid[6];      /* file member group id - decimal */
        字    ar_mode[8];     /* file member mode - octal */
        字    ar_size[10];    /* file member size - decimal */
        字    ar_fmag[2];     /* ARFMAG - string to end header */
};


/*------------------------------------------------------------------------*/
/*  SECTION HEADER                                                        */
/*------------------------------------------------------------------------*/
结构 scnhdr {
        字            s_name[8];      /* section name */
        长            s_paddr;        /* physical address */
        长            s_vaddr;        /* virtual address */
        长            s_size;         /* section size */
        长            s_scnptr;       /* file ptr to raw data for section */
        长            s_relptr;       /* file ptr to relocation */
        长            s_lnnoptr;      /* file ptr to line numbers */
        无符 整 s_nreloc;       /* number of relocation entries */
        无符 整 s_nlnno;        /* number of line number entries */
        无符 整 s_flags;        /* flags */
                无符 短   s_reserved;     /* reserved byte */
                无符 短  s_page;         /* memory page id */
        };

#定义 SCNHDR  结构 scnhdr
#定义 SCNHSZ  求长度(SCNHDR)

/*------------------------------------------------------------------------*/
/* Define constants for names of "special" sections                       */
/*------------------------------------------------------------------------*/
/* #定义 _TEXT    ".text" */
#定义 _DATA    ".data"
#定义 _BSS     ".bss"
#定义 _CINIT   ".cinit"
#定义 _TV      ".tv"

/*------------------------------------------------------------------------*/
/* The low 4 bits of s_flags is used as a section "type"                  */
/*------------------------------------------------------------------------*/
#定义 STYP_REG    0x00  /* "regular" : allocated, relocated, loaded */
#定义 STYP_DSECT  0x01  /* "dummy"   : not allocated, relocated, not loaded */
#定义 STYP_NOLOAD 0x02  /* "noload"  : allocated, relocated, not loaded */
#定义 STYP_GROUP  0x04  /* "grouped" : formed of input sections */
#定义 STYP_PAD    0x08  /* "padding" : not allocated, not relocated, loaded */
#定义 STYP_COPY   0x10  /* "copy"    : used for C init tables - 
                                                not allocated, relocated,
                                                loaded;  reloc & lineno
                                                entries processed normally */
#定义 STYP_TEXT   0x20   /* section contains text only */
#定义 STYP_DATA   0x40   /* section contains data only */
#定义 STYP_BSS    0x80   /* section contains bss only */

#定义 STYP_ALIGN  0x100  /* align flag passed by old version assemblers */
#定义 ALIGN_MASK  0x0F00 /* part of s_flags that is used for align vals */
#定义 ALIGNSIZE(x) (1 << ((x & ALIGN_MASK) >> 8))


/*------------------------------------------------------------------------*/
/*  RELOCATION ENTRIES                                                    */
/*------------------------------------------------------------------------*/
结构 reloc
{
   长            r_vaddr;        /* (virtual) address of reference */
   短           r_symndx;       /* index into symbol table */
   无符 短  r_disp;         /* additional bits for address calculation */
   无符 短  r_type;         /* relocation type */
};

#定义 RELOC   结构 reloc
#定义 RELSZ   10                 /* 求长度(RELOC) */

/*--------------------------------------------------------------------------*/
/*   define all relocation types                                            */
/*--------------------------------------------------------------------------*/

#定义 R_ABS           0         /* absolute address - no relocation       */
#定义 R_DIR16         01        /* UNUSED                                 */
#定义 R_REL16         02        /* UNUSED                                 */
#定义 R_DIR24         04        /* UNUSED                                 */
#定义 R_REL24         05        /* 24 bits, direct                        */
#定义 R_DIR32         06        /* UNUSED                                 */
#定义 R_RELBYTE      017        /* 8 bits, direct                         */
#定义 R_RELWORD      020        /* 16 bits, direct                        */
#定义 R_RELLONG      021        /* 32 bits, direct                        */
#定义 R_PCRBYTE      022        /* 8 bits, PC-relative                    */
#定义 R_PCRWORD      023        /* 16 bits, PC-relative                   */
#定义 R_PCRLONG      024        /* 32 bits, PC-relative                   */
#定义 R_OCRLONG      030        /* GSP: 32 bits, one's complement direct  */
#定义 R_GSPPCR16     031        /* GSP: 16 bits, PC relative (in words)   */
#定义 R_GSPOPR32     032        /* GSP: 32 bits, direct big-endian        */
#定义 R_PARTLS16     040        /* Brahma: 16 bit offset of 24 bit address*/
#定义 R_PARTMS8      041        /* Brahma: 8 bit page of 24 bit address   */
#定义 R_PARTLS7      050        /* DSP: 7 bit offset of 16 bit address    */
#定义 R_PARTMS9      051        /* DSP: 9 bit page of 16 bit address      */
#定义 R_REL13        052        /* DSP: 13 bits, direct                   */


/*------------------------------------------------------------------------*/
/*  LINE NUMBER ENTRIES                                                   */
/*------------------------------------------------------------------------*/
结构 lineno
{
        联合
        {
                长    l_symndx ;      /* sym. table index of function name
                                                iff l_lnno == 0      */
                长    l_paddr ;       /* (physical) address of line number */
        }               l_addr ;
        无符 短  l_lnno ;        /* line number */
};

#定义 LINENO  结构 lineno
#定义 LINESZ  6       /* 求长度(LINENO) */


/*------------------------------------------------------------------------*/
/*   STORAGE CLASSES                                                      */
/*------------------------------------------------------------------------*/
#定义  C_EFCN          -1    /* physical end of function */
#定义  C_NULL          0
#定义  C_AUTO          1     /* automatic variable */
#定义  C_EXT           2     /* external symbol */
#定义  C_STAT          3     /* static */
#定义  C_REG           4     /* register variable */
#定义  C_EXTDEF        5     /* external definition */
#定义  C_LABEL         6     /* label */
#定义  C_ULABEL        7     /* undefined label */
#定义  C_MOS           8     /* member of structure */
#定义  C_ARG           9     /* function argument */
#定义  C_STRTAG        10    /* structure tag */
#定义  C_MOU           11    /* member of union */
#定义  C_UNTAG         12    /* union tag */
#定义  C_TPDEF         13    /* type definition */
#定义 C_USTATIC        14    /* undefined static */
#定义  C_ENTAG         15    /* enumeration tag */
#定义  C_MOE           16    /* member of enumeration */
#定义  C_REGPARM       17    /* register parameter */
#定义  C_FIELD         18    /* bit field */

#定义  C_BLOCK         100   /* ".bb" or ".eb" */
#定义  C_FCN           101   /* ".bf" or ".ef" */
#定义  C_EOS           102   /* end of structure */
#定义  C_FILE          103   /* file name */
#定义  C_LINE          104   /* dummy sclass for line number entry */
#定义  C_ALIAS         105   /* duplicate tag */
#定义  C_HIDDEN        106   /* special storage class for external */
                               /* symbols in dmert public libraries  */

/*------------------------------------------------------------------------*/
/*  SYMBOL TABLE ENTRIES                                                  */
/*------------------------------------------------------------------------*/

#定义  SYMNMLEN   8      /*  Number of characters in a symbol name */
#定义  FILNMLEN   14     /*  Number of characters in a file name */
#定义  DIMNUM     4      /*  Number of array dimensions in auxiliary entry */


结构 syment
{
        联合
        {
                字            _n_name[SYMNMLEN];      /* old COFF version */
                结构
                {
                        长    _n_zeroes;      /* new == 0 */
                        长    _n_offset;      /* offset into string table */
                } _n_n;
                字            *_n_nptr[2];    /* allows for overlaying */
        } _n;
        长                    n_value;        /* value of symbol */
        短                   n_scnum;        /* section number */
        无符 短          n_type;         /* type and derived type */
        字                    n_sclass;       /* storage class */
        字                    n_numaux;       /* number of aux. entries */
};

#定义 n_name          _n._n_name
#定义 n_nptr          _n._n_nptr[1]
#定义 n_zeroes        _n._n_n._n_zeroes
#定义 n_offset        _n._n_n._n_offset

/*------------------------------------------------------------------------*/
/* Relocatable symbols have a section number of the                       */
/* section in which they are defined.  Otherwise, section                 */
/* numbers have the following meanings:                                   */
/*------------------------------------------------------------------------*/
#定义  N_UNDEF  0                     /* undefined symbol */
#定义  N_ABS    -1                    /* value of symbol is absolute */
#定义  N_DEBUG  -2                    /* special debugging symbol  */
#定义  N_TV     (无符 短)-3    /* needs transfer vector (preload) */
#定义  P_TV     (无符 短)-4    /* needs transfer vector (postload) */


/*------------------------------------------------------------------------*/
/* The fundamental type of a symbol packed into the low                   */
/* 4 bits of the word.                                                    */
/*------------------------------------------------------------------------*/
#定义  _EF    ".ef"

#定义  T_NULL     0          /* no type info */
#定义  T_ARG      1          /* function argument (only used by compiler) */
#定义  T_CHAR     2          /* character */
#定义  T_SHORT    3          /* short integer */
#定义  T_INT      4          /* integer */
#定义  T_LONG     5          /* long integer */
#定义  T_FLOAT    6          /* floating point */
#定义  T_DOUBLE   7          /* double word */
#定义  T_STRUCT   8          /* structure  */
#定义  T_UNION    9          /* union  */
#定义  T_ENUM     10         /* enumeration  */
#定义  T_MOE      11         /* member of enumeration */
#定义  T_UCHAR    12         /* unsigned character */
#定义  T_USHORT   13         /* unsigned short */
#定义  T_UINT     14         /* unsigned integer */
#定义  T_ULONG    15         /* unsigned long */

/*------------------------------------------------------------------------*/
/* derived types are:                                                     */
/*------------------------------------------------------------------------*/
#定义  DT_NON      0          /* no derived type */
#定义  DT_PTR      1          /* pointer */
#定义  DT_FCN      2          /* function */
#定义  DT_ARY      3          /* array */

#定义 MKTYPE(basic, d1,d2,d3,d4,d5,d6) \
       ((basic) | ((d1) <<  4) | ((d2) <<  6) | ((d3) <<  8) |\
                  ((d4) << 10) | ((d5) << 12) | ((d6) << 14))

/*------------------------------------------------------------------------*/
/* type packing constants and macros                                      */
/*------------------------------------------------------------------------*/
#定义  N_BTMASK_COFF     017
#定义  N_TMASK_COFF      060
#定义  N_TMASK1_COFF     0300
#定义  N_TMASK2_COFF     0360
#定义  N_BTSHFT_COFF     4
#定义  N_TSHIFT_COFF     2

#定义  BTYPE_COFF(x)  ((x) & N_BTMASK_COFF)  
#定义  ISINT(x)  (((x) >= T_CHAR && (x) <= T_LONG) ||   \
                    ((x) >= T_UCHAR && (x) <= T_ULONG) || (x) == T_ENUM)
#定义  ISFLT_COFF(x)  ((x) == T_DOUBLE || (x) == T_FLOAT)
#定义  ISPTR_COFF(x)  (((x) & N_TMASK_COFF) == (DT_PTR << N_BTSHFT_COFF)) 
#定义  ISFCN_COFF(x)  (((x) & N_TMASK_COFF) == (DT_FCN << N_BTSHFT_COFF))
#定义  ISARY_COFF(x)  (((x) & N_TMASK_COFF) == (DT_ARY << N_BTSHFT_COFF))
#定义  ISTAG_COFF(x)  ((x)==C_STRTAG || (x)==C_UNTAG || (x)==C_ENTAG)

#定义  INCREF_COFF(x) ((((x)&~N_BTMASK_COFF)<<N_TSHIFT_COFF)|(DT_PTR<<N_BTSHFT_COFF)|(x&N_BTMASK_COFF))
#定义  DECREF_COFF(x) ((((x)>>N_TSHIFT_COFF)&~N_BTMASK_COFF)|((x)&N_BTMASK_COFF))


/*------------------------------------------------------------------------*/
/*  AUXILIARY SYMBOL ENTRY                                                */
/*------------------------------------------------------------------------*/
联合 auxent
{
        结构
        {
                长            x_tagndx;       /* str, un, or enum tag indx */
                联合
                {
                        结构
                        {
                                无符 短  x_lnno; /* declaration line number */
                                无符 短  x_size; /* str, union, array size */
                        } x_lnsz;
                        长    x_fsize;        /* size of function */
                } x_misc;
                联合
                {
                        结构                  /* if ISFCN, tag, or .bb */
                        {
                                长    x_lnnoptr;      /* ptr to fcn line # */
                                长    x_endndx;       /* entry ndx past block end */
                        }       x_fcn;
                        结构                  /* if ISARY, up to 4 dimen. */
                        {
                                无符 短  x_dimen[DIMNUM];
                        }       x_ary;
                }               x_fcnary;
                无符 短  x_regcount;   /* number of registers used by func */
        }       x_sym;
        结构
        {
                字    x_fname[FILNMLEN];
        }       x_file;
        结构
        {
                长    x_scnlen;          /* section length */
                无符 短  x_nreloc;  /* number of relocation entries */
                无符 短  x_nlinno;  /* number of line numbers */
        }       x_scn;
};

#定义 SYMENT  结构 syment
#定义 SYMESZ  18      /* 求长度(SYMENT) */

#定义 AUXENT  联合 auxent
#定义 AUXESZ  18      /* 求长度(AUXENT) */

/*------------------------------------------------------------------------*/
/*  NAMES OF "SPECIAL" SYMBOLS                                            */
/*------------------------------------------------------------------------*/
#定义 _STEXT          ".text"
#定义 _ETEXT          "etext"
#定义 _SDATA          ".data"
#定义 _EDATA          "edata"
#定义 _SBSS           ".bss"
#定义 _END            "end"
#定义 _CINITPTR       "cinit"

/*--------------------------------------------------------------------------*/
/*  ENTRY POINT SYMBOLS                                                     */
/*--------------------------------------------------------------------------*/
#定义 _START          "_start"
#定义 _MAIN           "_main"
    /*  _CSTART         "_c_int00"          (defined in params.h)  */


#定义 _TVORIG         "_tvorig"
#定义 _TORIGIN        "_torigin"
#定义 _DORIGIN        "_dorigin"

#定义 _SORIGIN        "_sorigin"
