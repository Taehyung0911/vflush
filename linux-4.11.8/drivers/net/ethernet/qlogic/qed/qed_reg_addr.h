/* QLogic qed NIC Driver
 * Copyright (c) 2015-2017  QLogic Corporation
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and /or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef REG_ADDR_H
#define REG_ADDR_H

#define  CDU_REG_CID_ADDR_PARAMS_CONTEXT_SIZE_SHIFT \
	0

#define  CDU_REG_CID_ADDR_PARAMS_CONTEXT_SIZE		( \
		0xfff << 0)

#define  CDU_REG_CID_ADDR_PARAMS_BLOCK_WASTE_SHIFT \
	12

#define  CDU_REG_CID_ADDR_PARAMS_BLOCK_WASTE		( \
		0xfff << 12)

#define  CDU_REG_CID_ADDR_PARAMS_NCIB_SHIFT \
	24

#define  CDU_REG_CID_ADDR_PARAMS_NCIB			( \
		0xff << 24)

#define CDU_REG_SEGMENT0_PARAMS	\
	0x580904UL
#define CDU_REG_SEGMENT0_PARAMS_T0_NUM_TIDS_IN_BLOCK \
	(0xfff << 0)
#define CDU_REG_SEGMENT0_PARAMS_T0_NUM_TIDS_IN_BLOCK_SHIFT \
	0
#define CDU_REG_SEGMENT0_PARAMS_T0_TID_BLOCK_WASTE \
	(0xff << 16)
#define CDU_REG_SEGMENT0_PARAMS_T0_TID_BLOCK_WASTE_SHIFT \
	16
#define CDU_REG_SEGMENT0_PARAMS_T0_TID_SIZE \
	(0xff << 24)
#define CDU_REG_SEGMENT0_PARAMS_T0_TID_SIZE_SHIFT \
	24
#define CDU_REG_SEGMENT1_PARAMS	\
	0x580908UL
#define CDU_REG_SEGMENT1_PARAMS_T1_NUM_TIDS_IN_BLOCK \
	(0xfff << 0)
#define CDU_REG_SEGMENT1_PARAMS_T1_NUM_TIDS_IN_BLOCK_SHIFT \
	0
#define CDU_REG_SEGMENT1_PARAMS_T1_TID_BLOCK_WASTE \
	(0xff << 16)
#define CDU_REG_SEGMENT1_PARAMS_T1_TID_BLOCK_WASTE_SHIFT \
	16
#define CDU_REG_SEGMENT1_PARAMS_T1_TID_SIZE \
	(0xff << 24)
#define CDU_REG_SEGMENT1_PARAMS_T1_TID_SIZE_SHIFT \
	24

#define  XSDM_REG_OPERATION_GEN \
	0xf80408UL
#define  NIG_REG_RX_BRB_OUT_EN \
	0x500e18UL
#define  NIG_REG_STORM_OUT_EN \
	0x500e08UL
#define  PSWRQ2_REG_L2P_VALIDATE_VFID \
	0x240c50UL
#define  PGLUE_B_REG_USE_CLIENTID_IN_TAG	\
	0x2aae04UL
#define  PGLUE_B_REG_INTERNAL_PFID_ENABLE_MASTER	\
	0x2aa16cUL
#define PGLUE_B_REG_WAS_ERROR_VF_31_0_CLR \
	0x2aa118UL
#define PSWHST_REG_ZONE_PERMISSION_TABLE \
	0x2a0800UL
#define  BAR0_MAP_REG_MSDM_RAM \
	0x1d00000UL
#define  BAR0_MAP_REG_USDM_RAM \
	0x1d80000UL
#define  BAR0_MAP_REG_PSDM_RAM \
	0x1f00000UL
#define  BAR0_MAP_REG_TSDM_RAM \
	0x1c80000UL
#define BAR0_MAP_REG_XSDM_RAM \
	0x1e00000UL
#define BAR0_MAP_REG_YSDM_RAM \
	0x1e80000UL
#define  NIG_REG_RX_LLH_BRB_GATE_DNTFWD_PERPF \
	0x5011f4UL
#define PRS_REG_SEARCH_RESP_INITIATOR_TYPE \
	0x1f0164UL
#define  PRS_REG_SEARCH_TCP \
	0x1f0400UL
#define  PRS_REG_SEARCH_UDP \
	0x1f0404UL
#define  PRS_REG_SEARCH_FCOE \
	0x1f0408UL
#define  PRS_REG_SEARCH_ROCE \
	0x1f040cUL
#define  PRS_REG_SEARCH_OPENFLOW	\
	0x1f0434UL
#define PRS_REG_SEARCH_TAG1 \
	0x1f0444UL
#define PRS_REG_PKT_LEN_STAT_TAGS_NOT_COUNTED_FIRST \
	0x1f0a0cUL
#define PRS_REG_SEARCH_TCP_FIRST_FRAG \
	0x1f0410UL
#define  TM_REG_PF_ENABLE_CONN \
	0x2c043cUL
#define  TM_REG_PF_ENABLE_TASK \
	0x2c0444UL
#define  TM_REG_PF_SCAN_ACTIVE_CONN \
	0x2c04fcUL
#define  TM_REG_PF_SCAN_ACTIVE_TASK \
	0x2c0500UL
#define  IGU_REG_LEADING_EDGE_LATCH \
	0x18082cUL
#define  IGU_REG_TRAILING_EDGE_LATCH \
	0x180830UL
#define  QM_REG_USG_CNT_PF_TX \
	0x2f2eacUL
#define  QM_REG_USG_CNT_PF_OTHER	\
	0x2f2eb0UL
#define  DORQ_REG_PF_DB_ENABLE \
	0x100508UL
#define DORQ_REG_VF_USAGE_CNT \
	0x1009c4UL
#define  QM_REG_PF_EN \
	0x2f2ea4UL
#define TCFC_REG_WEAK_ENABLE_VF \
	0x2d0704UL
#define  TCFC_REG_STRONG_ENABLE_PF \
	0x2d0708UL
#define  TCFC_REG_STRONG_ENABLE_VF \
	0x2d070cUL
#define CCFC_REG_WEAK_ENABLE_VF \
	0x2e0704UL
#define  CCFC_REG_STRONG_ENABLE_PF \
	0x2e0708UL
#define  PGLUE_B_REG_PGL_ADDR_88_F0 \
	0x2aa404UL
#define  PGLUE_B_REG_PGL_ADDR_8C_F0 \
	0x2aa408UL
#define  PGLUE_B_REG_PGL_ADDR_90_F0 \
	0x2aa40cUL
#define  PGLUE_B_REG_PGL_ADDR_94_F0 \
	0x2aa410UL
#define  PGLUE_B_REG_WAS_ERROR_PF_31_0_CLR \
	0x2aa138UL
#define  PGLUE_B_REG_INTERNAL_PFID_ENABLE_TARGET_READ \
	0x2aa174UL
#define  MISC_REG_GEN_PURP_CR0 \
	0x008c80UL
#define  MCP_REG_SCRATCH	\
	0xe20000UL
#define  CNIG_REG_NW_PORT_MODE_BB_B0 \
	0x218200UL
#define  MISCS_REG_CHIP_NUM \
	0x00976cUL
#define  MISCS_REG_CHIP_REV \
	0x009770UL
#define  MISCS_REG_CMT_ENABLED_FOR_PAIR \
	0x00971cUL
#define  MISCS_REG_CHIP_TEST_REG	\
	0x009778UL
#define  MISCS_REG_CHIP_METAL \
	0x009774UL
#define MISCS_REG_FUNCTION_HIDE \
	0x0096f0UL
#define  BRB_REG_HEADER_SIZE \
	0x340804UL
#define  BTB_REG_HEADER_SIZE \
	0xdb0804UL
#define  CAU_REG_LONG_TIMEOUT_THRESHOLD \
	0x1c0708UL
#define  CCFC_REG_ACTIVITY_COUNTER \
	0x2e8800UL
#define CCFC_REG_STRONG_ENABLE_VF \
	0x2e070cUL
#define  CDU_REG_CID_ADDR_PARAMS	\
	0x580900UL
#define  DBG_REG_CLIENT_ENABLE \
	0x010004UL
#define  DMAE_REG_INIT \
	0x00c000UL
#define  DORQ_REG_IFEN \
	0x100040UL
#define DORQ_REG_DB_DROP_REASON \
	0x100a2cUL
#define DORQ_REG_DB_DROP_DETAILS \
	0x100a24UL
#define DORQ_REG_DB_DROP_DETAILS_ADDRESS \
	0x100a1cUL
#define  GRC_REG_TIMEOUT_EN \
	0x050404UL
#define GRC_REG_TIMEOUT_ATTN_ACCESS_VALID \
	0x050054UL
#define GRC_REG_TIMEOUT_ATTN_ACCESS_DATA_0 \
	0x05004cUL
#define GRC_REG_TIMEOUT_ATTN_ACCESS_DATA_1 \
	0x050050UL
#define  IGU_REG_BLOCK_CONFIGURATION \
	0x180040UL
#define  MCM_REG_INIT \
	0x1200000UL
#define  MCP2_REG_DBG_DWORD_ENABLE \
	0x052404UL
#define  MISC_REG_PORT_MODE \
	0x008c00UL
#define  MISCS_REG_CLK_100G_MODE	\
	0x009070UL
#define  MSDM_REG_ENABLE_IN1 \
	0xfc0004UL
#define  MSEM_REG_ENABLE_IN \
	0x1800004UL
#define  NIG_REG_CM_HDR \
	0x500840UL
#define NIG_REG_LLH_TAGMAC_DEF_PF_VECTOR \
	0x50196cUL
#define NIG_REG_LLH_CLS_TYPE_DUALMODE \
	0x501964UL
#define NIG_REG_LLH_FUNC_FILTER_VALUE \
	0x501a00UL
#define NIG_REG_LLH_FUNC_FILTER_VALUE_SIZE \
	32
#define NIG_REG_LLH_FUNC_FILTER_EN \
	0x501a80UL
#define NIG_REG_LLH_FUNC_FILTER_EN_SIZE	\
	16
#define NIG_REG_LLH_FUNC_FILTER_MODE \
	0x501ac0UL
#define NIG_REG_LLH_FUNC_FILTER_MODE_SIZE \
	16
#define NIG_REG_LLH_FUNC_FILTER_PROTOCOL_TYPE \
	0x501b00UL
#define NIG_REG_LLH_FUNC_FILTER_PROTOCOL_TYPE_SIZE \
	16
#define NIG_REG_LLH_FUNC_FILTER_HDR_SEL	\
	0x501b40UL
#define NIG_REG_LLH_FUNC_FILTER_HDR_SEL_SIZE \
	16
#define  NCSI_REG_CONFIG	\
	0x040200UL
#define  PBF_REG_INIT \
	0xd80000UL
#define PBF_REG_NUM_BLOCKS_ALLOCATED_PROD_VOQ0 \
	0xd806c8UL
#define PBF_REG_NUM_BLOCKS_ALLOCATED_CONS_VOQ0 \
	0xd806ccUL
#define  PTU_REG_ATC_INIT_ARRAY \
	0x560000UL
#define  PCM_REG_INIT \
	0x1100000UL
#define  PGLUE_B_REG_ADMIN_PER_PF_REGION	\
	0x2a9000UL
#define PGLUE_B_REG_TX_ERR_WR_DETAILS2 \
	0x2aa150UL
#define PGLUE_B_REG_TX_ERR_WR_ADD_31_0 \
	0x2aa144UL
#define PGLUE_B_REG_TX_ERR_WR_ADD_63_32 \
	0x2aa148UL
#define PGLUE_B_REG_TX_ERR_WR_DETAILS \
	0x2aa14cUL
#define PGLUE_B_REG_TX_ERR_RD_ADD_31_0 \
	0x2aa154UL
#define PGLUE_B_REG_TX_ERR_RD_ADD_63_32 \
	0x2aa158UL
#define PGLUE_B_REG_TX_ERR_RD_DETAILS \
	0x2aa15cUL
#define PGLUE_B_REG_TX_ERR_RD_DETAILS2 \
	0x2aa160UL
#define PGLUE_B_REG_TX_ERR_WR_DETAILS_ICPL \
	0x2aa164UL
#define PGLUE_B_REG_MASTER_ZLR_ERR_DETAILS \
	0x2aa54cUL
#define PGLUE_B_REG_MASTER_ZLR_ERR_ADD_31_0 \
	0x2aa544UL
#define PGLUE_B_REG_MASTER_ZLR_ERR_ADD_63_32 \
	0x2aa548UL
#define PGLUE_B_REG_VF_ILT_ERR_ADD_31_0 \
	0x2aae74UL
#define PGLUE_B_REG_VF_ILT_ERR_ADD_63_32 \
	0x2aae78UL
#define PGLUE_B_REG_VF_ILT_ERR_DETAILS \
	0x2aae7cUL
#define PGLUE_B_REG_VF_ILT_ERR_DETAILS2 \
	0x2aae80UL
#define PGLUE_B_REG_LATCHED_ERRORS_CLR \
	0x2aa3bcUL
#define  PRM_REG_DISABLE_PRM \
	0x230000UL
#define  PRS_REG_SOFT_RST \
	0x1f0000UL
#define PRS_REG_MSG_INFO \
	0x1f0a1cUL
#define PRS_REG_ROCE_DEST_QP_MAX_PF \
	0x1f0430UL
#define PRS_REG_USE_LIGHT_L2 \
	0x1f096cUL
#define  PSDM_REG_ENABLE_IN1 \
	0xfa0004UL
#define  PSEM_REG_ENABLE_IN \
	0x1600004UL
#define  PSWRQ_REG_DBG_SELECT \
	0x280020UL
#define  PSWRQ2_REG_CDUT_P_SIZE \
	0x24000cUL
#define PSWRQ2_REG_ILT_MEMORY \
	0x260000UL
#define  PSWHST_REG_DISCARD_INTERNAL_WRITES \
	0x2a0040UL
#define  PSWHST2_REG_DBGSYN_ALMOST_FULL_THR \
	0x29e050UL
#define PSWHST_REG_INCORRECT_ACCESS_VALID \
	0x2a0070UL
#define PSWHST_REG_INCORRECT_ACCESS_ADDRESS \
	0x2a0074UL
#define PSWHST_REG_INCORRECT_ACCESS_DATA \
	0x2a0068UL
#define PSWHST_REG_INCORRECT_ACCESS_LENGTH \
	0x2a006cUL
#define  PSWRD_REG_DBG_SELECT \
	0x29c040UL
#define  PSWRD2_REG_CONF11 \
	0x29d064UL
#define  PSWWR_REG_USDM_FULL_TH \
	0x29a040UL
#define  PSWWR2_REG_CDU_FULL_TH2	\
	0x29b040UL
#define  QM_REG_MAXPQSIZE_0 \
	0x2f0434UL
#define  RSS_REG_RSS_INIT_EN \
	0x238804UL
#define  RDIF_REG_STOP_ON_ERROR \
	0x300040UL
#define  SRC_REG_SOFT_RST \
	0x23874cUL
#define  TCFC_REG_ACTIVITY_COUNTER \
	0x2d8800UL
#define  TCM_REG_INIT \
	0x1180000UL
#define  TM_REG_PXP_READ_DATA_FIFO_INIT \
	0x2c0014UL
#define  TSDM_REG_ENABLE_IN1 \
	0xfb0004UL
#define  TSEM_REG_ENABLE_IN \
	0x1700004UL
#define  TDIF_REG_STOP_ON_ERROR \
	0x310040UL
#define  UCM_REG_INIT \
	0x1280000UL
#define  UMAC_REG_IPG_HD_BKP_CNTL_BB_B0 \
	0x051004UL
#define  USDM_REG_ENABLE_IN1 \
	0xfd0004UL
#define  USEM_REG_ENABLE_IN \
	0x1900004UL
#define  XCM_REG_INIT \
	0x1000000UL
#define  XSDM_REG_ENABLE_IN1 \
	0xf80004UL
#define  XSEM_REG_ENABLE_IN \
	0x1400004UL
#define  YCM_REG_INIT \
	0x1080000UL
#define  YSDM_REG_ENABLE_IN1 \
	0xf90004UL
#define  YSEM_REG_ENABLE_IN \
	0x1500004UL
#define  XYLD_REG_SCBD_STRICT_PRIO \
	0x4c0000UL
#define  TMLD_REG_SCBD_STRICT_PRIO \
	0x4d0000UL
#define  MULD_REG_SCBD_STRICT_PRIO \
	0x4e0000UL
#define  YULD_REG_SCBD_STRICT_PRIO \
	0x4c8000UL
#define  MISC_REG_SHARED_MEM_ADDR \
	0x008c20UL
#define  DMAE_REG_GO_C0 \
	0x00c048UL
#define  DMAE_REG_GO_C1 \
	0x00c04cUL
#define  DMAE_REG_GO_C2 \
	0x00c050UL
#define  DMAE_REG_GO_C3 \
	0x00c054UL
#define  DMAE_REG_GO_C4 \
	0x00c058UL
#define  DMAE_REG_GO_C5 \
	0x00c05cUL
#define  DMAE_REG_GO_C6 \
	0x00c060UL
#define  DMAE_REG_GO_C7 \
	0x00c064UL
#define  DMAE_REG_GO_C8 \
	0x00c068UL
#define  DMAE_REG_GO_C9 \
	0x00c06cUL
#define  DMAE_REG_GO_C10	\
	0x00c070UL
#define  DMAE_REG_GO_C11	\
	0x00c074UL
#define  DMAE_REG_GO_C12	\
	0x00c078UL
#define  DMAE_REG_GO_C13	\
	0x00c07cUL
#define  DMAE_REG_GO_C14	\
	0x00c080UL
#define  DMAE_REG_GO_C15	\
	0x00c084UL
#define  DMAE_REG_GO_C16	\
	0x00c088UL
#define  DMAE_REG_GO_C17	\
	0x00c08cUL
#define  DMAE_REG_GO_C18	\
	0x00c090UL
#define  DMAE_REG_GO_C19	\
	0x00c094UL
#define  DMAE_REG_GO_C20	\
	0x00c098UL
#define  DMAE_REG_GO_C21	\
	0x00c09cUL
#define  DMAE_REG_GO_C22	\
	0x00c0a0UL
#define  DMAE_REG_GO_C23	\
	0x00c0a4UL
#define  DMAE_REG_GO_C24	\
	0x00c0a8UL
#define  DMAE_REG_GO_C25	\
	0x00c0acUL
#define  DMAE_REG_GO_C26	\
	0x00c0b0UL
#define  DMAE_REG_GO_C27	\
	0x00c0b4UL
#define  DMAE_REG_GO_C28	\
	0x00c0b8UL
#define  DMAE_REG_GO_C29	\
	0x00c0bcUL
#define  DMAE_REG_GO_C30	\
	0x00c0c0UL
#define  DMAE_REG_GO_C31	\
	0x00c0c4UL
#define  DMAE_REG_CMD_MEM \
	0x00c800UL
#define  QM_REG_MAXPQSIZETXSEL_0	\
	0x2f0440UL
#define  QM_REG_SDMCMDREADY \
	0x2f1e10UL
#define  QM_REG_SDMCMDADDR \
	0x2f1e04UL
#define  QM_REG_SDMCMDDATALSB \
	0x2f1e08UL
#define  QM_REG_SDMCMDDATAMSB \
	0x2f1e0cUL
#define  QM_REG_SDMCMDGO	\
	0x2f1e14UL
#define  QM_REG_RLPFCRD \
	0x2f4d80UL
#define  QM_REG_RLPFINCVAL \
	0x2f4c80UL
#define  QM_REG_RLGLBLCRD \
	0x2f4400UL
#define  QM_REG_RLGLBLINCVAL \
	0x2f3400UL
#define  IGU_REG_ATTENTION_ENABLE \
	0x18083cUL
#define  IGU_REG_ATTN_MSG_ADDR_L	\
	0x180820UL
#define  IGU_REG_ATTN_MSG_ADDR_H	\
	0x180824UL
#define  MISC_REG_AEU_GENERAL_ATTN_0 \
	0x008400UL
#define  CAU_REG_SB_ADDR_MEMORY \
	0x1c8000UL
#define  CAU_REG_SB_VAR_MEMORY \
	0x1c6000UL
#define  CAU_REG_PI_MEMORY \
	0x1d0000UL
#define  IGU_REG_PF_CONFIGURATION \
	0x180800UL
#define IGU_REG_VF_CONFIGURATION \
	0x180804UL
#define  MISC_REG_AEU_ENABLE1_IGU_OUT_0 \
	0x00849cUL
#define MISC_REG_AEU_AFTER_INVERT_1_IGU	\
	0x0087b4UL
#define  MISC_REG_AEU_MASK_ATTN_IGU \
	0x008494UL
#define  IGU_REG_CLEANUP_STATUS_0 \
	0x180980UL
#define  IGU_REG_CLEANUP_STATUS_1 \
	0x180a00UL
#define  IGU_REG_CLEANUP_STATUS_2 \
	0x180a80UL
#define  IGU_REG_CLEANUP_STATUS_3 \
	0x180b00UL
#define  IGU_REG_CLEANUP_STATUS_4 \
	0x180b80UL
#define  IGU_REG_COMMAND_REG_32LSB_DATA \
	0x180840UL
#define  IGU_REG_COMMAND_REG_CTRL \
	0x180848UL
#define  IGU_REG_BLOCK_CONFIGURATION_VF_CLEANUP_EN	( \
		0x1 << 1)
#define  IGU_REG_BLOCK_CONFIGURATION_PXP_TPH_INTERFACE_EN	( \
		0x1 << 0)
#define  IGU_REG_MAPPING_MEMORY \
	0x184000UL
#define IGU_REG_STATISTIC_NUM_VF_MSG_SENT \
	0x180408UL
#define IGU_REG_WRITE_DONE_PENDING \
	0x180900UL
#define  MISCS_REG_GENERIC_POR_0	\
	0x0096d4UL
#define  MCP_REG_NVM_CFG4 \
	0xe0642cUL
#define  MCP_REG_NVM_CFG4_FLASH_SIZE	( \
		0x7 << 0)
#define  MCP_REG_NVM_CFG4_FLASH_SIZE_SHIFT \
	0
#define MCP_REG_CPU_STATE \
	0xe05004UL
#define MCP_REG_CPU_EVENT_MASK \
	0xe05008UL
#define PGLUE_B_REG_PF_BAR0_SIZE \
	0x2aae60UL
#define PGLUE_B_REG_PF_BAR1_SIZE \
	0x2aae64UL
#define PRS_REG_ENCAPSULATION_TYPE_EN	0x1f0730UL
#define PRS_REG_GRE_PROTOCOL		0x1f0734UL
#define PRS_REG_VXLAN_PORT		0x1f0738UL
#define PRS_REG_OUTPUT_FORMAT_4_0	0x1f099cUL
#define NIG_REG_ENC_TYPE_ENABLE		0x501058UL

#define NIG_REG_ENC_TYPE_ENABLE_ETH_OVER_GRE_ENABLE		(0x1 << 0)
#define NIG_REG_ENC_TYPE_ENABLE_ETH_OVER_GRE_ENABLE_SHIFT	0
#define NIG_REG_ENC_TYPE_ENABLE_IP_OVER_GRE_ENABLE		(0x1 << 1)
#define NIG_REG_ENC_TYPE_ENABLE_IP_OVER_GRE_ENABLE_SHIFT	1
#define NIG_REG_ENC_TYPE_ENABLE_VXLAN_ENABLE			(0x1 << 2)
#define NIG_REG_ENC_TYPE_ENABLE_VXLAN_ENABLE_SHIFT		2

#define NIG_REG_VXLAN_CTRL		0x50105cUL
#define PBF_REG_VXLAN_PORT		0xd80518UL
#define PBF_REG_NGE_PORT		0xd8051cUL
#define PRS_REG_NGE_PORT		0x1f086cUL
#define NIG_REG_NGE_PORT		0x508b38UL

#define DORQ_REG_L2_EDPM_TUNNEL_GRE_ETH_EN	0x10090cUL
#define DORQ_REG_L2_EDPM_TUNNEL_GRE_IP_EN	0x100910UL
#define DORQ_REG_L2_EDPM_TUNNEL_VXLAN_EN	0x100914UL
#define DORQ_REG_L2_EDPM_TUNNEL_NGE_IP_EN	0x10092cUL
#define DORQ_REG_L2_EDPM_TUNNEL_NGE_ETH_EN	0x100930UL

#define NIG_REG_NGE_IP_ENABLE			0x508b28UL
#define NIG_REG_NGE_ETH_ENABLE			0x508b2cUL
#define NIG_REG_NGE_COMP_VER			0x508b30UL
#define PBF_REG_NGE_COMP_VER			0xd80524UL
#define PRS_REG_NGE_COMP_VER			0x1f0878UL

#define QM_REG_WFQPFWEIGHT	0x2f4e80UL
#define QM_REG_WFQVPWEIGHT	0x2fa000UL

#define PGLCS_REG_DBG_SELECT \
	0x001d14UL
#define PGLCS_REG_DBG_DWORD_ENABLE \
	0x001d18UL
#define PGLCS_REG_DBG_SHIFT \
	0x001d1cUL
#define PGLCS_REG_DBG_FORCE_VALID \
	0x001d20UL
#define PGLCS_REG_DBG_FORCE_FRAME \
	0x001d24UL
#define MISC_REG_RESET_PL_PDA_VMAIN_1 \
	0x008070UL
#define MISC_REG_RESET_PL_PDA_VMAIN_2 \
	0x008080UL
#define MISC_REG_RESET_PL_PDA_VAUX \
	0x008090UL
#define MISCS_REG_RESET_PL_UA \
	0x009050UL
#define MISCS_REG_RESET_PL_HV \
	0x009060UL
#define MISCS_REG_RESET_PL_HV_2	\
	0x009150UL
#define DMAE_REG_DBG_SELECT \
	0x00c510UL
#define DMAE_REG_DBG_DWORD_ENABLE \
	0x00c514UL
#define DMAE_REG_DBG_SHIFT \
	0x00c518UL
#define DMAE_REG_DBG_FORCE_VALID \
	0x00c51cUL
#define DMAE_REG_DBG_FORCE_FRAME \
	0x00c520UL
#define NCSI_REG_DBG_SELECT \
	0x040474UL
#define NCSI_REG_DBG_DWORD_ENABLE \
	0x040478UL
#define NCSI_REG_DBG_SHIFT \
	0x04047cUL
#define NCSI_REG_DBG_FORCE_VALID \
	0x040480UL
#define NCSI_REG_DBG_FORCE_FRAME \
	0x040484UL
#define GRC_REG_DBG_SELECT \
	0x0500a4UL
#define GRC_REG_DBG_DWORD_ENABLE \
	0x0500a8UL
#define GRC_REG_DBG_SHIFT \
	0x0500acUL
#define GRC_REG_DBG_FORCE_VALID	\
	0x0500b0UL
#define GRC_REG_DBG_FORCE_FRAME	\
	0x0500b4UL
#define UMAC_REG_DBG_SELECT \
	0x051094UL
#define UMAC_REG_DBG_DWORD_ENABLE \
	0x051098UL
#define UMAC_REG_DBG_SHIFT \
	0x05109cUL
#define UMAC_REG_DBG_FORCE_VALID \
	0x0510a0UL
#define UMAC_REG_DBG_FORCE_FRAME \
	0x0510a4UL
#define MCP2_REG_DBG_SELECT \
	0x052400UL
#define MCP2_REG_DBG_DWORD_ENABLE \
	0x052404UL
#define MCP2_REG_DBG_SHIFT \
	0x052408UL
#define MCP2_REG_DBG_FORCE_VALID \
	0x052440UL
#define MCP2_REG_DBG_FORCE_FRAME \
	0x052444UL
#define PCIE_REG_DBG_SELECT \
	0x0547e8UL
#define PCIE_REG_DBG_DWORD_ENABLE \
	0x0547ecUL
#define PCIE_REG_DBG_SHIFT \
	0x0547f0UL
#define PCIE_REG_DBG_FORCE_VALID \
	0x0547f4UL
#define PCIE_REG_DBG_FORCE_FRAME \
	0x0547f8UL
#define DORQ_REG_DBG_SELECT \
	0x100ad0UL
#define DORQ_REG_DBG_DWORD_ENABLE \
	0x100ad4UL
#define DORQ_REG_DBG_SHIFT \
	0x100ad8UL
#define DORQ_REG_DBG_FORCE_VALID \
	0x100adcUL
#define DORQ_REG_DBG_FORCE_FRAME \
	0x100ae0UL
#define IGU_REG_DBG_SELECT \
	0x181578UL
#define IGU_REG_DBG_DWORD_ENABLE \
	0x18157cUL
#define IGU_REG_DBG_SHIFT \
	0x181580UL
#define IGU_REG_DBG_FORCE_VALID	\
	0x181584UL
#define IGU_REG_DBG_FORCE_FRAME	\
	0x181588UL
#define CAU_REG_DBG_SELECT \
	0x1c0ea8UL
#define CAU_REG_DBG_DWORD_ENABLE \
	0x1c0eacUL
#define CAU_REG_DBG_SHIFT \
	0x1c0eb0UL
#define CAU_REG_DBG_FORCE_VALID	\
	0x1c0eb4UL
#define CAU_REG_DBG_FORCE_FRAME	\
	0x1c0eb8UL
#define PRS_REG_DBG_SELECT \
	0x1f0b6cUL
#define PRS_REG_DBG_DWORD_ENABLE \
	0x1f0b70UL
#define PRS_REG_DBG_SHIFT \
	0x1f0b74UL
#define PRS_REG_DBG_FORCE_VALID	\
	0x1f0ba0UL
#define PRS_REG_DBG_FORCE_FRAME	\
	0x1f0ba4UL
#define CNIG_REG_DBG_SELECT_K2 \
	0x218254UL
#define CNIG_REG_DBG_DWORD_ENABLE_K2 \
	0x218258UL
#define CNIG_REG_DBG_SHIFT_K2 \
	0x21825cUL
#define CNIG_REG_DBG_FORCE_VALID_K2 \
	0x218260UL
#define CNIG_REG_DBG_FORCE_FRAME_K2 \
	0x218264UL
#define PRM_REG_DBG_SELECT \
	0x2306a8UL
#define PRM_REG_DBG_DWORD_ENABLE \
	0x2306acUL
#define PRM_REG_DBG_SHIFT \
	0x2306b0UL
#define PRM_REG_DBG_FORCE_VALID	\
	0x2306b4UL
#define PRM_REG_DBG_FORCE_FRAME	\
	0x2306b8UL
#define SRC_REG_DBG_SELECT \
	0x238700UL
#define SRC_REG_DBG_DWORD_ENABLE \
	0x238704UL
#define SRC_REG_DBG_SHIFT \
	0x238708UL
#define SRC_REG_DBG_FORCE_VALID	\
	0x23870cUL
#define SRC_REG_DBG_FORCE_FRAME	\
	0x238710UL
#define RSS_REG_DBG_SELECT \
	0x238c4cUL
#define RSS_REG_DBG_DWORD_ENABLE \
	0x238c50UL
#define RSS_REG_DBG_SHIFT \
	0x238c54UL
#define RSS_REG_DBG_FORCE_VALID	\
	0x238c58UL
#define RSS_REG_DBG_FORCE_FRAME	\
	0x238c5cUL
#define RPB_REG_DBG_SELECT \
	0x23c728UL
#define RPB_REG_DBG_DWORD_ENABLE \
	0x23c72cUL
#define RPB_REG_DBG_SHIFT \
	0x23c730UL
#define RPB_REG_DBG_FORCE_VALID	\
	0x23c734UL
#define RPB_REG_DBG_FORCE_FRAME	\
	0x23c738UL
#define PSWRQ2_REG_DBG_SELECT \
	0x240100UL
#define PSWRQ2_REG_DBG_DWORD_ENABLE \
	0x240104UL
#define PSWRQ2_REG_DBG_SHIFT \
	0x240108UL
#define PSWRQ2_REG_DBG_FORCE_VALID \
	0x24010cUL
#define PSWRQ2_REG_DBG_FORCE_FRAME \
	0x240110UL
#define PSWRQ_REG_DBG_SELECT \
	0x280020UL
#define PSWRQ_REG_DBG_DWORD_ENABLE \
	0x280024UL
#define PSWRQ_REG_DBG_SHIFT \
	0x280028UL
#define PSWRQ_REG_DBG_FORCE_VALID \
	0x28002cUL
#define PSWRQ_REG_DBG_FORCE_FRAME \
	0x280030UL
#define PSWWR_REG_DBG_SELECT \
	0x29a084UL
#define PSWWR_REG_DBG_DWORD_ENABLE \
	0x29a088UL
#define PSWWR_REG_DBG_SHIFT \
	0x29a08cUL
#define PSWWR_REG_DBG_FORCE_VALID \
	0x29a090UL
#define PSWWR_REG_DBG_FORCE_FRAME \
	0x29a094UL
#define PSWRD_REG_DBG_SELECT \
	0x29c040UL
#define PSWRD_REG_DBG_DWORD_ENABLE \
	0x29c044UL
#define PSWRD_REG_DBG_SHIFT \
	0x29c048UL
#define PSWRD_REG_DBG_FORCE_VALID \
	0x29c04cUL
#define PSWRD_REG_DBG_FORCE_FRAME \
	0x29c050UL
#define PSWRD2_REG_DBG_SELECT \
	0x29d400UL
#define PSWRD2_REG_DBG_DWORD_ENABLE \
	0x29d404UL
#define PSWRD2_REG_DBG_SHIFT \
	0x29d408UL
#define PSWRD2_REG_DBG_FORCE_VALID \
	0x29d40cUL
#define PSWRD2_REG_DBG_FORCE_FRAME \
	0x29d410UL
#define PSWHST2_REG_DBG_SELECT \
	0x29e058UL
#define PSWHST2_REG_DBG_DWORD_ENABLE \
	0x29e05cUL
#define PSWHST2_REG_DBG_SHIFT \
	0x29e060UL
#define PSWHST2_REG_DBG_FORCE_VALID \
	0x29e064UL
#define PSWHST2_REG_DBG_FORCE_FRAME \
	0x29e068UL
#define PSWHST_REG_DBG_SELECT \
	0x2a0100UL
#define PSWHST_REG_DBG_DWORD_ENABLE \
	0x2a0104UL
#define PSWHST_REG_DBG_SHIFT \
	0x2a0108UL
#define PSWHST_REG_DBG_FORCE_VALID \
	0x2a010cUL
#define PSWHST_REG_DBG_FORCE_FRAME \
	0x2a0110UL
#define PGLUE_B_REG_DBG_SELECT \
	0x2a8400UL
#define PGLUE_B_REG_DBG_DWORD_ENABLE \
	0x2a8404UL
#define PGLUE_B_REG_DBG_SHIFT \
	0x2a8408UL
#define PGLUE_B_REG_DBG_FORCE_VALID \
	0x2a840cUL
#define PGLUE_B_REG_DBG_FORCE_FRAME \
	0x2a8410UL
#define TM_REG_DBG_SELECT \
	0x2c07a8UL
#define TM_REG_DBG_DWORD_ENABLE	\
	0x2c07acUL
#define TM_REG_DBG_SHIFT \
	0x2c07b0UL
#define TM_REG_DBG_FORCE_VALID \
	0x2c07b4UL
#define TM_REG_DBG_FORCE_FRAME \
	0x2c07b8UL
#define TCFC_REG_DBG_SELECT \
	0x2d0500UL
#define TCFC_REG_DBG_DWORD_ENABLE \
	0x2d0504UL
#define TCFC_REG_DBG_SHIFT \
	0x2d0508UL
#define TCFC_REG_DBG_FORCE_VALID \
	0x2d050cUL
#define TCFC_REG_DBG_FORCE_FRAME \
	0x2d0510UL
#define CCFC_REG_DBG_SELECT \
	0x2e0500UL
#define CCFC_REG_DBG_DWORD_ENABLE \
	0x2e0504UL
#define CCFC_REG_DBG_SHIFT \
	0x2e0508UL
#define CCFC_REG_DBG_FORCE_VALID \
	0x2e050cUL
#define CCFC_REG_DBG_FORCE_FRAME \
	0x2e0510UL
#define QM_REG_DBG_SELECT \
	0x2f2e74UL
#define QM_REG_DBG_DWORD_ENABLE	\
	0x2f2e78UL
#define QM_REG_DBG_SHIFT \
	0x2f2e7cUL
#define QM_REG_DBG_FORCE_VALID \
	0x2f2e80UL
#define QM_REG_DBG_FORCE_FRAME \
	0x2f2e84UL
#define RDIF_REG_DBG_SELECT \
	0x300500UL
#define RDIF_REG_DBG_DWORD_ENABLE \
	0x300504UL
#define RDIF_REG_DBG_SHIFT \
	0x300508UL
#define RDIF_REG_DBG_FORCE_VALID \
	0x30050cUL
#define RDIF_REG_DBG_FORCE_FRAME \
	0x300510UL
#define TDIF_REG_DBG_SELECT \
	0x310500UL
#define TDIF_REG_DBG_DWORD_ENABLE \
	0x310504UL
#define TDIF_REG_DBG_SHIFT \
	0x310508UL
#define TDIF_REG_DBG_FORCE_VALID \
	0x31050cUL
#define TDIF_REG_DBG_FORCE_FRAME \
	0x310510UL
#define BRB_REG_DBG_SELECT \
	0x340ed0UL
#define BRB_REG_DBG_DWORD_ENABLE \
	0x340ed4UL
#define BRB_REG_DBG_SHIFT \
	0x340ed8UL
#define BRB_REG_DBG_FORCE_VALID	\
	0x340edcUL
#define BRB_REG_DBG_FORCE_FRAME	\
	0x340ee0UL
#define XYLD_REG_DBG_SELECT \
	0x4c1600UL
#define XYLD_REG_DBG_DWORD_ENABLE \
	0x4c1604UL
#define XYLD_REG_DBG_SHIFT \
	0x4c1608UL
#define XYLD_REG_DBG_FORCE_VALID \
	0x4c160cUL
#define XYLD_REG_DBG_FORCE_FRAME \
	0x4c1610UL
#define YULD_REG_DBG_SELECT \
	0x4c9600UL
#define YULD_REG_DBG_DWORD_ENABLE \
	0x4c9604UL
#define YULD_REG_DBG_SHIFT \
	0x4c9608UL
#define YULD_REG_DBG_FORCE_VALID \
	0x4c960cUL
#define YULD_REG_DBG_FORCE_FRAME \
	0x4c9610UL
#define TMLD_REG_DBG_SELECT \
	0x4d1600UL
#define TMLD_REG_DBG_DWORD_ENABLE \
	0x4d1604UL
#define TMLD_REG_DBG_SHIFT \
	0x4d1608UL
#define TMLD_REG_DBG_FORCE_VALID \
	0x4d160cUL
#define TMLD_REG_DBG_FORCE_FRAME \
	0x4d1610UL
#define MULD_REG_DBG_SELECT \
	0x4e1600UL
#define MULD_REG_DBG_DWORD_ENABLE \
	0x4e1604UL
#define MULD_REG_DBG_SHIFT \
	0x4e1608UL
#define MULD_REG_DBG_FORCE_VALID \
	0x4e160cUL
#define MULD_REG_DBG_FORCE_FRAME \
	0x4e1610UL
#define NIG_REG_DBG_SELECT \
	0x502140UL
#define NIG_REG_DBG_DWORD_ENABLE \
	0x502144UL
#define NIG_REG_DBG_SHIFT \
	0x502148UL
#define NIG_REG_DBG_FORCE_VALID	\
	0x50214cUL
#define NIG_REG_DBG_FORCE_FRAME	\
	0x502150UL
#define BMB_REG_DBG_SELECT \
	0x540a7cUL
#define BMB_REG_DBG_DWORD_ENABLE \
	0x540a80UL
#define BMB_REG_DBG_SHIFT \
	0x540a84UL
#define BMB_REG_DBG_FORCE_VALID	\
	0x540a88UL
#define BMB_REG_DBG_FORCE_FRAME	\
	0x540a8cUL
#define PTU_REG_DBG_SELECT \
	0x560100UL
#define PTU_REG_DBG_DWORD_ENABLE \
	0x560104UL
#define PTU_REG_DBG_SHIFT \
	0x560108UL
#define PTU_REG_DBG_FORCE_VALID	\
	0x56010cUL
#define PTU_REG_DBG_FORCE_FRAME	\
	0x560110UL
#define CDU_REG_DBG_SELECT \
	0x580704UL
#define CDU_REG_DBG_DWORD_ENABLE \
	0x580708UL
#define CDU_REG_DBG_SHIFT \
	0x58070cUL
#define CDU_REG_DBG_FORCE_VALID	\
	0x580710UL
#define CDU_REG_DBG_FORCE_FRAME	\
	0x580714UL
#define WOL_REG_DBG_SELECT \
	0x600140UL
#define WOL_REG_DBG_DWORD_ENABLE \
	0x600144UL
#define WOL_REG_DBG_SHIFT \
	0x600148UL
#define WOL_REG_DBG_FORCE_VALID	\
	0x60014cUL
#define WOL_REG_DBG_FORCE_FRAME	\
	0x600150UL
#define BMBN_REG_DBG_SELECT \
	0x610140UL
#define BMBN_REG_DBG_DWORD_ENABLE \
	0x610144UL
#define BMBN_REG_DBG_SHIFT \
	0x610148UL
#define BMBN_REG_DBG_FORCE_VALID \
	0x61014cUL
#define BMBN_REG_DBG_FORCE_FRAME \
	0x610150UL
#define NWM_REG_DBG_SELECT \
	0x8000ecUL
#define NWM_REG_DBG_DWORD_ENABLE \
	0x8000f0UL
#define NWM_REG_DBG_SHIFT \
	0x8000f4UL
#define NWM_REG_DBG_FORCE_VALID	\
	0x8000f8UL
#define NWM_REG_DBG_FORCE_FRAME	\
	0x8000fcUL
#define PBF_REG_DBG_SELECT \
	0xd80060UL
#define PBF_REG_DBG_DWORD_ENABLE \
	0xd80064UL
#define PBF_REG_DBG_SHIFT \
	0xd80068UL
#define PBF_REG_DBG_FORCE_VALID	\
	0xd8006cUL
#define PBF_REG_DBG_FORCE_FRAME	\
	0xd80070UL
#define PBF_PB1_REG_DBG_SELECT \
	0xda0728UL
#define PBF_PB1_REG_DBG_DWORD_ENABLE \
	0xda072cUL
#define PBF_PB1_REG_DBG_SHIFT \
	0xda0730UL
#define PBF_PB1_REG_DBG_FORCE_VALID \
	0xda0734UL
#define PBF_PB1_REG_DBG_FORCE_FRAME \
	0xda0738UL
#define PBF_PB2_REG_DBG_SELECT \
	0xda4728UL
#define PBF_PB2_REG_DBG_DWORD_ENABLE \
	0xda472cUL
#define PBF_PB2_REG_DBG_SHIFT \
	0xda4730UL
#define PBF_PB2_REG_DBG_FORCE_VALID \
	0xda4734UL
#define PBF_PB2_REG_DBG_FORCE_FRAME \
	0xda4738UL
#define BTB_REG_DBG_SELECT \
	0xdb08c8UL
#define BTB_REG_DBG_DWORD_ENABLE \
	0xdb08ccUL
#define BTB_REG_DBG_SHIFT \
	0xdb08d0UL
#define BTB_REG_DBG_FORCE_VALID	\
	0xdb08d4UL
#define BTB_REG_DBG_FORCE_FRAME	\
	0xdb08d8UL
#define XSDM_REG_DBG_SELECT \
	0xf80e28UL
#define XSDM_REG_DBG_DWORD_ENABLE \
	0xf80e2cUL
#define XSDM_REG_DBG_SHIFT \
	0xf80e30UL
#define XSDM_REG_DBG_FORCE_VALID \
	0xf80e34UL
#define XSDM_REG_DBG_FORCE_FRAME \
	0xf80e38UL
#define YSDM_REG_DBG_SELECT \
	0xf90e28UL
#define YSDM_REG_DBG_DWORD_ENABLE \
	0xf90e2cUL
#define YSDM_REG_DBG_SHIFT \
	0xf90e30UL
#define YSDM_REG_DBG_FORCE_VALID \
	0xf90e34UL
#define YSDM_REG_DBG_FORCE_FRAME \
	0xf90e38UL
#define PSDM_REG_DBG_SELECT \
	0xfa0e28UL
#define PSDM_REG_DBG_DWORD_ENABLE \
	0xfa0e2cUL
#define PSDM_REG_DBG_SHIFT \
	0xfa0e30UL
#define PSDM_REG_DBG_FORCE_VALID \
	0xfa0e34UL
#define PSDM_REG_DBG_FORCE_FRAME \
	0xfa0e38UL
#define TSDM_REG_DBG_SELECT \
	0xfb0e28UL
#define TSDM_REG_DBG_DWORD_ENABLE \
	0xfb0e2cUL
#define TSDM_REG_DBG_SHIFT \
	0xfb0e30UL
#define TSDM_REG_DBG_FORCE_VALID \
	0xfb0e34UL
#define TSDM_REG_DBG_FORCE_FRAME \
	0xfb0e38UL
#define MSDM_REG_DBG_SELECT \
	0xfc0e28UL
#define MSDM_REG_DBG_DWORD_ENABLE \
	0xfc0e2cUL
#define MSDM_REG_DBG_SHIFT \
	0xfc0e30UL
#define MSDM_REG_DBG_FORCE_VALID \
	0xfc0e34UL
#define MSDM_REG_DBG_FORCE_FRAME \
	0xfc0e38UL
#define USDM_REG_DBG_SELECT \
	0xfd0e28UL
#define USDM_REG_DBG_DWORD_ENABLE \
	0xfd0e2cUL
#define USDM_REG_DBG_SHIFT \
	0xfd0e30UL
#define USDM_REG_DBG_FORCE_VALID \
	0xfd0e34UL
#define USDM_REG_DBG_FORCE_FRAME \
	0xfd0e38UL
#define XCM_REG_DBG_SELECT \
	0x1000040UL
#define XCM_REG_DBG_DWORD_ENABLE \
	0x1000044UL
#define XCM_REG_DBG_SHIFT \
	0x1000048UL
#define XCM_REG_DBG_FORCE_VALID	\
	0x100004cUL
#define XCM_REG_DBG_FORCE_FRAME	\
	0x1000050UL
#define YCM_REG_DBG_SELECT \
	0x1080040UL
#define YCM_REG_DBG_DWORD_ENABLE \
	0x1080044UL
#define YCM_REG_DBG_SHIFT \
	0x1080048UL
#define YCM_REG_DBG_FORCE_VALID	\
	0x108004cUL
#define YCM_REG_DBG_FORCE_FRAME	\
	0x1080050UL
#define PCM_REG_DBG_SELECT \
	0x1100040UL
#define PCM_REG_DBG_DWORD_ENABLE \
	0x1100044UL
#define PCM_REG_DBG_SHIFT \
	0x1100048UL
#define PCM_REG_DBG_FORCE_VALID	\
	0x110004cUL
#define PCM_REG_DBG_FORCE_FRAME	\
	0x1100050UL
#define TCM_REG_DBG_SELECT \
	0x1180040UL
#define TCM_REG_DBG_DWORD_ENABLE \
	0x1180044UL
#define TCM_REG_DBG_SHIFT \
	0x1180048UL
#define TCM_REG_DBG_FORCE_VALID	\
	0x118004cUL
#define TCM_REG_DBG_FORCE_FRAME	\
	0x1180050UL
#define MCM_REG_DBG_SELECT \
	0x1200040UL
#define MCM_REG_DBG_DWORD_ENABLE \
	0x1200044UL
#define MCM_REG_DBG_SHIFT \
	0x1200048UL
#define MCM_REG_DBG_FORCE_VALID	\
	0x120004cUL
#define MCM_REG_DBG_FORCE_FRAME	\
	0x1200050UL
#define UCM_REG_DBG_SELECT \
	0x1280050UL
#define UCM_REG_DBG_DWORD_ENABLE \
	0x1280054UL
#define UCM_REG_DBG_SHIFT \
	0x1280058UL
#define UCM_REG_DBG_FORCE_VALID	\
	0x128005cUL
#define UCM_REG_DBG_FORCE_FRAME	\
	0x1280060UL
#define XSEM_REG_DBG_SELECT \
	0x1401528UL
#define XSEM_REG_DBG_DWORD_ENABLE \
	0x140152cUL
#define XSEM_REG_DBG_SHIFT \
	0x1401530UL
#define XSEM_REG_DBG_FORCE_VALID \
	0x1401534UL
#define XSEM_REG_DBG_FORCE_FRAME \
	0x1401538UL
#define YSEM_REG_DBG_SELECT \
	0x1501528UL
#define YSEM_REG_DBG_DWORD_ENABLE \
	0x150152cUL
#define YSEM_REG_DBG_SHIFT \
	0x1501530UL
#define YSEM_REG_DBG_FORCE_VALID \
	0x1501534UL
#define YSEM_REG_DBG_FORCE_FRAME \
	0x1501538UL
#define PSEM_REG_DBG_SELECT \
	0x1601528UL
#define PSEM_REG_DBG_DWORD_ENABLE \
	0x160152cUL
#define PSEM_REG_DBG_SHIFT \
	0x1601530UL
#define PSEM_REG_DBG_FORCE_VALID \
	0x1601534UL
#define PSEM_REG_DBG_FORCE_FRAME \
	0x1601538UL
#define TSEM_REG_DBG_SELECT \
	0x1701528UL
#define TSEM_REG_DBG_DWORD_ENABLE \
	0x170152cUL
#define TSEM_REG_DBG_SHIFT \
	0x1701530UL
#define TSEM_REG_DBG_FORCE_VALID \
	0x1701534UL
#define TSEM_REG_DBG_FORCE_FRAME \
	0x1701538UL
#define MSEM_REG_DBG_SELECT \
	0x1801528UL
#define MSEM_REG_DBG_DWORD_ENABLE \
	0x180152cUL
#define MSEM_REG_DBG_SHIFT \
	0x1801530UL
#define MSEM_REG_DBG_FORCE_VALID \
	0x1801534UL
#define MSEM_REG_DBG_FORCE_FRAME \
	0x1801538UL
#define USEM_REG_DBG_SELECT \
	0x1901528UL
#define USEM_REG_DBG_DWORD_ENABLE \
	0x190152cUL
#define USEM_REG_DBG_SHIFT \
	0x1901530UL
#define USEM_REG_DBG_FORCE_VALID \
	0x1901534UL
#define USEM_REG_DBG_FORCE_FRAME \
	0x1901538UL
#define PCIE_REG_DBG_COMMON_SELECT \
	0x054398UL
#define PCIE_REG_DBG_COMMON_DWORD_ENABLE \
	0x05439cUL
#define PCIE_REG_DBG_COMMON_SHIFT \
	0x0543a0UL
#define PCIE_REG_DBG_COMMON_FORCE_VALID	\
	0x0543a4UL
#define PCIE_REG_DBG_COMMON_FORCE_FRAME	\
	0x0543a8UL
#define MISC_REG_RESET_PL_UA \
	0x008050UL
#define MISC_REG_RESET_PL_HV \
	0x008060UL
#define XCM_REG_CTX_RBC_ACCS \
	0x1001800UL
#define XCM_REG_AGG_CON_CTX \
	0x1001804UL
#define XCM_REG_SM_CON_CTX \
	0x1001808UL
#define YCM_REG_CTX_RBC_ACCS \
	0x1081800UL
#define YCM_REG_AGG_CON_CTX \
	0x1081804UL
#define YCM_REG_AGG_TASK_CTX \
	0x1081808UL
#define YCM_REG_SM_CON_CTX \
	0x108180cUL
#define YCM_REG_SM_TASK_CTX \
	0x1081810UL
#define PCM_REG_CTX_RBC_ACCS \
	0x1101440UL
#define PCM_REG_SM_CON_CTX \
	0x1101444UL
#define TCM_REG_CTX_RBC_ACCS \
	0x11814c0UL
#define TCM_REG_AGG_CON_CTX \
	0x11814c4UL
#define TCM_REG_AGG_TASK_CTX \
	0x11814c8UL
#define TCM_REG_SM_CON_CTX \
	0x11814ccUL
#define TCM_REG_SM_TASK_CTX \
	0x11814d0UL
#define MCM_REG_CTX_RBC_ACCS \
	0x1201800UL
#define MCM_REG_AGG_CON_CTX \
	0x1201804UL
#define MCM_REG_AGG_TASK_CTX \
	0x1201808UL
#define MCM_REG_SM_CON_CTX \
	0x120180cUL
#define MCM_REG_SM_TASK_CTX \
	0x1201810UL
#define UCM_REG_CTX_RBC_ACCS \
	0x1281700UL
#define UCM_REG_AGG_CON_CTX \
	0x1281704UL
#define UCM_REG_AGG_TASK_CTX \
	0x1281708UL
#define UCM_REG_SM_CON_CTX \
	0x128170cUL
#define UCM_REG_SM_TASK_CTX \
	0x1281710UL
#define XSEM_REG_SLOW_DBG_EMPTY	\
	0x1401140UL
#define XSEM_REG_SYNC_DBG_EMPTY	\
	0x1401160UL
#define XSEM_REG_SLOW_DBG_ACTIVE \
	0x1401400UL
#define XSEM_REG_SLOW_DBG_MODE \
	0x1401404UL
#define XSEM_REG_DBG_FRAME_MODE	\
	0x1401408UL
#define XSEM_REG_DBG_MODE1_CFG \
	0x1401420UL
#define XSEM_REG_FAST_MEMORY \
	0x1440000UL
#define YSEM_REG_SYNC_DBG_EMPTY	\
	0x1501160UL
#define YSEM_REG_SLOW_DBG_ACTIVE \
	0x1501400UL
#define YSEM_REG_SLOW_DBG_MODE \
	0x1501404UL
#define YSEM_REG_DBG_FRAME_MODE	\
	0x1501408UL
#define YSEM_REG_DBG_MODE1_CFG \
	0x1501420UL
#define YSEM_REG_FAST_MEMORY \
	0x1540000UL
#define PSEM_REG_SLOW_DBG_EMPTY	\
	0x1601140UL
#define PSEM_REG_SYNC_DBG_EMPTY	\
	0x1601160UL
#define PSEM_REG_SLOW_DBG_ACTIVE \
	0x1601400UL
#define PSEM_REG_SLOW_DBG_MODE \
	0x1601404UL
#define PSEM_REG_DBG_FRAME_MODE	\
	0x1601408UL
#define PSEM_REG_DBG_MODE1_CFG \
	0x1601420UL
#define PSEM_REG_FAST_MEMORY \
	0x1640000UL
#define TSEM_REG_SLOW_DBG_EMPTY	\
	0x1701140UL
#define TSEM_REG_SYNC_DBG_EMPTY	\
	0x1701160UL
#define TSEM_REG_SLOW_DBG_ACTIVE \
	0x1701400UL
#define TSEM_REG_SLOW_DBG_MODE \
	0x1701404UL
#define TSEM_REG_DBG_FRAME_MODE	\
	0x1701408UL
#define TSEM_REG_DBG_MODE1_CFG \
	0x1701420UL
#define TSEM_REG_FAST_MEMORY \
	0x1740000UL
#define MSEM_REG_SLOW_DBG_EMPTY	\
	0x1801140UL
#define MSEM_REG_SYNC_DBG_EMPTY	\
	0x1801160UL
#define MSEM_REG_SLOW_DBG_ACTIVE \
	0x1801400UL
#define MSEM_REG_SLOW_DBG_MODE \
	0x1801404UL
#define MSEM_REG_DBG_FRAME_MODE	\
	0x1801408UL
#define MSEM_REG_DBG_MODE1_CFG \
	0x1801420UL
#define MSEM_REG_FAST_MEMORY \
	0x1840000UL
#define USEM_REG_SLOW_DBG_EMPTY	\
	0x1901140UL
#define USEM_REG_SYNC_DBG_EMPTY	\
	0x1901160UL
#define USEM_REG_SLOW_DBG_ACTIVE \
	0x1901400UL
#define USEM_REG_SLOW_DBG_MODE \
	0x1901404UL
#define USEM_REG_DBG_FRAME_MODE	\
	0x1901408UL
#define USEM_REG_DBG_MODE1_CFG \
	0x1901420UL
#define USEM_REG_FAST_MEMORY \
	0x1940000UL
#define SEM_FAST_REG_INT_RAM \
	0x020000UL
#define SEM_FAST_REG_INT_RAM_SIZE \
	20480
#define GRC_REG_TRACE_FIFO_VALID_DATA \
	0x050064UL
#define GRC_REG_NUMBER_VALID_OVERRIDE_WINDOW \
	0x05040cUL
#define GRC_REG_PROTECTION_OVERRIDE_WINDOW \
	0x050500UL
#define IGU_REG_ERROR_HANDLING_MEMORY \
	0x181520UL
#define MCP_REG_CPU_MODE \
	0xe05000UL
#define MCP_REG_CPU_MODE_SOFT_HALT \
		(0x1 << 10)
#define BRB_REG_BIG_RAM_ADDRESS \
	0x340800UL
#define BRB_REG_BIG_RAM_DATA \
	0x341500UL
#define SEM_FAST_REG_STALL_0 \
	0x000488UL
#define SEM_FAST_REG_STALLED \
	0x000494UL
#define BTB_REG_BIG_RAM_ADDRESS \
	0xdb0800UL
#define BTB_REG_BIG_RAM_DATA \
	0xdb0c00UL
#define BMB_REG_BIG_RAM_ADDRESS \
	0x540800UL
#define BMB_REG_BIG_RAM_DATA \
	0x540f00UL
#define SEM_FAST_REG_STORM_REG_FILE \
	0x008000UL
#define RSS_REG_RSS_RAM_ADDR \
	0x238c30UL
#define MISCS_REG_BLOCK_256B_EN \
	0x009074UL
#define MCP_REG_SCRATCH_SIZE \
	57344
#define MCP_REG_CPU_REG_FILE \
	0xe05200UL
#define MCP_REG_CPU_REG_FILE_SIZE \
	32
#define DBG_REG_DEBUG_TARGET \
	0x01005cUL
#define DBG_REG_FULL_MODE \
	0x010060UL
#define DBG_REG_CALENDAR_OUT_DATA \
	0x010480UL
#define GRC_REG_TRACE_FIFO \
	0x050068UL
#define IGU_REG_ERROR_HANDLING_DATA_VALID \
	0x181530UL
#define DBG_REG_DBG_BLOCK_ON \
	0x010454UL
#define DBG_REG_FRAMING_MODE \
	0x010058UL
#define SEM_FAST_REG_VFC_DATA_WR \
	0x000b40UL
#define SEM_FAST_REG_VFC_ADDR \
	0x000b44UL
#define SEM_FAST_REG_VFC_DATA_RD \
	0x000b48UL
#define RSS_REG_RSS_RAM_DATA \
	0x238c20UL
#define MISC_REG_BLOCK_256B_EN \
	0x008c14UL
#define NWS_REG_NWS_CMU	\
	0x720000UL
#define PHY_NW_IP_REG_PHY0_TOP_TBUS_ADDR_7_0 \
	0x000680UL
#define PHY_NW_IP_REG_PHY0_TOP_TBUS_ADDR_15_8 \
	0x000684UL
#define PHY_NW_IP_REG_PHY0_TOP_TBUS_DATA_7_0 \
	0x0006c0UL
#define PHY_NW_IP_REG_PHY0_TOP_TBUS_DATA_11_8 \
	0x0006c4UL
#define MS_REG_MS_CMU \
	0x6a4000UL
#define PHY_SGMII_IP_REG_AHB_CMU_CSR_0_X130 \
	0x000208UL
#define PHY_SGMII_IP_REG_AHB_CMU_CSR_0_X132 \
	0x000210UL
#define PHY_SGMII_IP_REG_AHB_CMU_CSR_0_X131 \
	0x00020cUL
#define PHY_SGMII_IP_REG_AHB_CMU_CSR_0_X133 \
	0x000214UL
#define PHY_PCIE_IP_REG_AHB_CMU_CSR_0_X130 \
	0x000208UL
#define PHY_PCIE_IP_REG_AHB_CMU_CSR_0_X131 \
	0x00020cUL
#define PHY_PCIE_IP_REG_AHB_CMU_CSR_0_X132 \
	0x000210UL
#define PHY_PCIE_IP_REG_AHB_CMU_CSR_0_X133 \
	0x000214UL
#define PHY_PCIE_REG_PHY0 \
	0x620000UL
#define PHY_PCIE_REG_PHY1 \
	0x624000UL
#define NIG_REG_ROCE_DUPLICATE_TO_HOST 0x5088f0UL
#define PRS_REG_LIGHT_L2_ETHERTYPE_EN 0x1f0968UL
#define NIG_REG_LLH_ENG_CLS_ENG_ID_TBL 0x501b90UL
#define DORQ_REG_PF_DPM_ENABLE 0x100510UL
#define DORQ_REG_PF_ICID_BIT_SHIFT_NORM	0x100448UL
#define DORQ_REG_PF_MIN_ADDR_REG1 0x100400UL
#define DORQ_REG_PF_DPI_BIT_SHIFT 0x100450UL
#define NIG_REG_RX_PTP_EN 0x501900UL
#define NIG_REG_TX_PTP_EN 0x501904UL
#define NIG_REG_LLH_PTP_TO_HOST	0x501908UL
#define NIG_REG_LLH_PTP_TO_MCP 0x50190cUL
#define NIG_REG_PTP_SW_TXTSEN 0x501910UL
#define NIG_REG_LLH_PTP_ETHERTYPE_1 0x501914UL
#define NIG_REG_LLH_PTP_MAC_DA_2_LSB 0x501918UL
#define NIG_REG_LLH_PTP_MAC_DA_2_MSB 0x50191cUL
#define NIG_REG_LLH_PTP_PARAM_MASK 0x501920UL
#define NIG_REG_LLH_PTP_RULE_MASK 0x501924UL
#define NIG_REG_TX_LLH_PTP_PARAM_MASK 0x501928UL
#define NIG_REG_TX_LLH_PTP_RULE_MASK 0x50192cUL
#define NIG_REG_LLH_PTP_HOST_BUF_SEQID 0x501930UL
#define NIG_REG_LLH_PTP_HOST_BUF_TS_LSB 0x501934UL
#define NIG_REG_LLH_PTP_HOST_BUF_TS_MSB	0x501938UL
#define NIG_REG_LLH_PTP_MCP_BUF_SEQID 0x50193cUL
#define NIG_REG_LLH_PTP_MCP_BUF_TS_LSB 0x501940UL
#define NIG_REG_LLH_PTP_MCP_BUF_TS_MSB 0x501944UL
#define NIG_REG_TX_LLH_PTP_BUF_SEQID 0x501948UL
#define NIG_REG_TX_LLH_PTP_BUF_TS_LSB 0x50194cUL
#define NIG_REG_TX_LLH_PTP_BUF_TS_MSB 0x501950UL
#define NIG_REG_RX_PTP_TS_MSB_ERR 0x501954UL
#define NIG_REG_TX_PTP_TS_MSB_ERR 0x501958UL
#define NIG_REG_TSGEN_SYNC_TIME_LSB 0x5088c0UL
#define NIG_REG_TSGEN_SYNC_TIME_MSB 0x5088c4UL
#define NIG_REG_TSGEN_RST_DRIFT_CNTR 0x5088d8UL
#define NIG_REG_TSGEN_DRIFT_CNTR_CONF 0x5088dcUL
#define NIG_REG_TS_OUTPUT_ENABLE_PDA 0x508870UL
#define NIG_REG_TIMESYNC_GEN_REG_BB 0x500d00UL
#define NIG_REG_TSGEN_FREE_CNT_VALUE_LSB 0x5088a8UL
#define NIG_REG_TSGEN_FREE_CNT_VALUE_MSB 0x5088acUL
#endif
