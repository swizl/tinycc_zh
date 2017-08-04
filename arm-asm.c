/*************************************************************/
/*
 *  ARM dummy assembler for TCC
 *
 */

#如定义 TARGET_DEFS_ONLY

#定义 CONFIG_TCC_ASM
#定义 NB_ASM_REGS 16

ST_FUNC 空 g(整 c);
ST_FUNC 空 gen_le16(整 c);
ST_FUNC 空 gen_le32(整 c);

/*************************************************************/
#另
/*************************************************************/

#包含 "tcc.h"

静态 空 asm_error(空)
{
    tcc_error("ARM asm not implemented.");
}

/* XXX: make it faster ? */
ST_FUNC 空 g(整 c)
{
    整 ind1;
    如 (nocode_wanted)
        返回;
    ind1 = ind + 1;
    如 (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind] = c;
    ind = ind1;
}

ST_FUNC 空 gen_le16 (整 i)
{
    g(i);
    g(i>>8);
}

ST_FUNC 空 gen_le32 (整 i)
{
    gen_le16(i);
    gen_le16(i>>16);
}

ST_FUNC 空 gen_expr32(ExprValue *pe)
{
    gen_le32(pe->v);
}

ST_FUNC 空 asm_opcode(TCCState *s1, 整 opcode)
{
    asm_error();
}

ST_FUNC 空 subst_asm_operand(CString *add_str, SValue *sv, 整 modifier)
{
    asm_error();
}

/* generate prolog and epilog code for asm statement */
ST_FUNC 空 asm_gen_code(ASMOperand *operands, 整 nb_operands,
                         整 nb_outputs, 整 is_output,
                         uint8_t *clobber_regs,
                         整 out_reg)
{
}

ST_FUNC 空 asm_compute_constraints(ASMOperand *operands,
                                    整 nb_operands, 整 nb_outputs,
                                    不变 uint8_t *clobber_regs,
                                    整 *pout_reg)
{
}

ST_FUNC 空 asm_clobber(uint8_t *clobber_regs, 不变 字 *str)
{
    asm_error();
}

ST_FUNC 整 asm_parse_regvar (整 t)
{
    asm_error();
    返回 -1;
}

/*************************************************************/
#了如 /* ndef TARGET_DEFS_ONLY */
