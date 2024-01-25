Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  TRI7BOX                       share/modules/tri7box.F       
Chd|-- called by -----------
Chd|        ASSPARXX                      source/assembly/assparxx.F    
Chd|        CHECK_FI                      source/mpi/interfaces/spmd_i25slide.F
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        DEALLOCATE_FI1_TMP            source/mpi/interfaces/spmd_i25slide.F
Chd|        FRICTIONPARTS_MODEL_ORTHO     source/interfaces/int07/frictionparts_model.F
Chd|        I10CORP3                      source/interfaces/int10/i10corp3.F
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I10OPTCD                      source/interfaces/intsort/i10optcd.F
Chd|        I10SMS2                       source/interfaces/int10/i10sms2.F
Chd|        I10TRI                        source/interfaces/intsort/i10tri.F
Chd|        I11ASS0                       source/interfaces/int11/i11ass3.F
Chd|        I11ASS05                      source/interfaces/int11/i11ass3.F
Chd|        I11ASS2                       source/interfaces/int11/i11ass3.F
Chd|        I11ASS25                      source/interfaces/int11/i11ass3.F
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|        I11COR3                       source/interfaces/int11/i11cor3.F
Chd|        I11CORP3                      source/interfaces/int11/i11corp3.F
Chd|        I11FOR3                       source/interfaces/int11/i11for3.F
Chd|        I11KEG3                       source/interfaces/int11/i11keg3.F
Chd|        I11KFOR3                      source/interfaces/int11/i11keg3.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I11OPTCD                      source/interfaces/intsort/i11optcd.F
Chd|        I11PEN3                       source/interfaces/intsort/i11pen3.F
Chd|        I11PEN3_VOX                   source/interfaces/intsort/i11pen3.F
Chd|        I11SMS2                       source/interfaces/int11/i11sms2.F
Chd|        I11STO                        source/interfaces/intsort/i11sto.F
Chd|        I11STO_VOX                    source/interfaces/intsort/i11sto.F
Chd|        I11TRC                        source/interfaces/intsort/i11trc.F
Chd|        I11TRI                        source/interfaces/intsort/i11tri.F
Chd|        I11TRIVOX                     source/interfaces/intsort/i11trivox.F
Chd|        I17CUT                        source/interfaces/int17/i17tri.F
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|        I17LLL4_PENA                  source/interfaces/int17/i17for3.F
Chd|        I17VIT_PENA                   source/interfaces/int17/i17for3.F
Chd|        I18DST3                       source/interfaces/int18/i18dst3.F
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        I18KINE_F                     source/interfaces/int18/i18main_kine.F
Chd|        I18KINE_I                     source/interfaces/int18/i18main_kine.F
Chd|        I18KINE_S                     source/interfaces/int18/i18main_kine.F
Chd|        I18KINE_V                     source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|        I18TRI                        source/interfaces/int18/i18tri.F
Chd|        I20ASS0                       source/interfaces/int20/i20for3.F
Chd|        I20ASS05                      source/interfaces/int20/i20for3.F
Chd|        I20ASS2                       source/interfaces/int20/i20for3.F
Chd|        I20ASS25                      source/interfaces/int20/i20for3.F
Chd|        I20COR3                       source/interfaces/int20/i20cor3.F
Chd|        I20COR3E                      source/interfaces/int20/i20cor3.F
Chd|        I20COR3T                      source/interfaces/int20/i20cor3t.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I20FOR3C                      source/interfaces/int20/i20for3.F
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|        I20OPTCD                      source/interfaces/intsort/i20optcd.F
Chd|        I20OPTCDE                     source/interfaces/intsort/i20optcd.F
Chd|        I20PEN3_EDGE                  source/interfaces/intsort/i20sto.F
Chd|        I20SMS2E                      source/interfaces/int20/i20sms2.F
Chd|        I20STO                        source/interfaces/intsort/i20sto.F
Chd|        I20TRI                        source/interfaces/intsort/i20tri.F
Chd|        I20TRI_EDGE                   source/interfaces/intsort/i20tri.F
Chd|        I20XSINIR                     source/interfaces/intsort/i20main_tri.F
Chd|        I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|        I21BUCE                       source/interfaces/intsort/i21buce.F
Chd|        I21COR3                       source/interfaces/int21/i21cor3.F
Chd|        I21SMS2                       source/interfaces/int21/i21sms2.F
Chd|        I22ASS0                       source/interfaces/int22/i22assembly.F
Chd|        I22ASS2                       source/interfaces/int22/i22assembly.F
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|        I22COR3                       source/interfaces/int22/i22cor3.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I22SHELL_GETMINMAX            source/interfaces/intsort/i22main_tri.F
Chd|        I23BUCE                       source/interfaces/intsort/i23buce.F
Chd|        I23COR3                       source/interfaces/int23/i23cor3.F
Chd|        I23COR3T                      source/interfaces/int23/i23cor3t.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I23OPTCD                      source/interfaces/int23/i23optcd.F
Chd|        I23TRIVOX                     source/interfaces/intsort/i23trivox.F
Chd|        I24ASS0                       source/interfaces/int24/i24for3.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
Chd|        I24BUCE                       source/interfaces/intsort/i24buce.F
Chd|        I24COR3                       source/interfaces/int24/i24cor3.F
Chd|        I24COR3T                      source/interfaces/intsort/i24cor3t.F
Chd|        I24CORK3                      source/interfaces/int24/i24cork3.F
Chd|        I24CORKM                      source/interfaces/int24/i24cork3.F
Chd|        I24CORP3                      source/interfaces/int24/i24cork3.F
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|        I24DST3E                      source/interfaces/int24/i24dst3e.F
Chd|        I24EDGT                       source/interfaces/intsort/i24sto.F
Chd|        I24FOR1_FIC                   source/interfaces/int24/i24for3e.F
Chd|        I24FOR1_FICR                  source/interfaces/int24/i24for3e.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I24GAP_PXFEM                  source/interfaces/int24/i24gap_pxfem.F
Chd|        I24IPLY_PXFEM                 source/interfaces/int24/i24iply_pxfem.F
Chd|        I24ISPT2_INI                  source/interfaces/int24/i24cor3.F
Chd|        I24ISPT2_INI_OPTTRI           source/interfaces/int24/i24cor3.F
Chd|        I24KEG3                       source/interfaces/int24/i24ke3.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I24NEXTTRIA                   source/interfaces/int24/i24dst3.F
Chd|        I24NEXTTRIA2                  source/interfaces/int24/i24dst3.F
Chd|        I24NITSCHFOR3                 source/interfaces/int24/i24nitschfor3.F
Chd|        I24OPTCD                      source/interfaces/intsort/i24optcd.F
Chd|        I24S1S2                       source/interfaces/intsort/i24sto.F
Chd|        I24SMS2                       source/interfaces/int24/i24for3.F
Chd|        I24STO                        source/interfaces/intsort/i24sto.F
Chd|        I24TRC                        source/interfaces/intsort/i7trc.F
Chd|        I24TRIVOX                     source/interfaces/intsort/i24trivox.F
Chd|        I24_SAVE_SUB                  source/interfaces/int24/i24_save_sub.F
Chd|        I25ASS0                       source/interfaces/int25/i25ass3.F
Chd|        I25ASS2                       source/interfaces/int25/i25ass3.F
Chd|        I25ASS3                       source/interfaces/int25/i25ass3.F
Chd|        I25ASSE0                      source/interfaces/int25/i25asse.F
Chd|        I25ASSE05                     source/interfaces/int25/i25asse.F
Chd|        I25ASSE05_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE0_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE2                      source/interfaces/int25/i25asse.F
Chd|        I25ASSE25                     source/interfaces/int25/i25asse.F
Chd|        I25ASSE25_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE2_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|        I25BUCE                       source/interfaces/intsort/i25buce.F
Chd|        I25BUCE_EDG                   source/interfaces/intsort/i25buce_edg.F
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|        I25COR3E                      source/interfaces/int25/i25cor3e.F
Chd|        I25COR3T                      source/interfaces/intsort/i25cor3t.F
Chd|        I25COR3_1                     source/interfaces/int25/i25cor3.F
Chd|        I25COR3_21                    source/interfaces/int25/i25cor3.F
Chd|        I25COR3_22                    source/interfaces/int25/i25cor3.F
Chd|        I25COR3_3                     source/interfaces/int25/i25cor3.F
Chd|        I25COR3_E2S                   source/interfaces/int25/i25cor3_e2s.F
Chd|        I25DST3_1                     source/interfaces/int25/i25dst3_1.F
Chd|        I25DST3_21                    source/interfaces/int25/i25dst3_21.F
Chd|        I25DST3_22                    source/interfaces/int25/i25dst3_22.F
Chd|        I25DST3_3                     source/interfaces/int25/i25dst3_3.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        I25FOR3E                      source/interfaces/int25/i25for3e.F
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|        I25GLOB                       source/interfaces/int25/i25dst3_1.F
Chd|        I25GLOB_22                    source/interfaces/int25/i25dst3_22.F
Chd|        I25IRTLM                      source/interfaces/int25/i25irtlm.F
Chd|        I25KEEPF                      source/interfaces/int25/i25slid.F
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I25OPTCD                      source/interfaces/intsort/i25optcd.F
Chd|        I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|        I25OPTCD_EDG                  source/interfaces/intsort/i25optcd_edg.F
Chd|        I25PEN3_E2S                   source/interfaces/intsort/i25pen3_e2s.F
Chd|        I25PEN3_EDG                   source/interfaces/intsort/i25pen3_edg.F
Chd|        I25PREP_ADD                   source/interfaces/int25/i25slid.F
Chd|        I25PREP_NINDEX                source/interfaces/int25/i25slid.F
Chd|        I25PREP_SEND                  source/interfaces/int25/i25slid.F
Chd|        I25PREP_SIZBUFS               source/interfaces/int25/i25slid.F
Chd|        I25PREP_SLID_1                source/interfaces/int25/i25slid.F
Chd|        I25PREP_SLID_2                source/interfaces/int25/i25slid.F
Chd|        I25S1S2                       source/interfaces/intsort/i25sto.F
Chd|        I25SMS0E                      source/interfaces/int25/i25smse.F
Chd|        I25SMS2                       source/interfaces/int25/i25for3.F
Chd|        I25SMS2E                      source/interfaces/int25/i25smse.F
Chd|        I25SMS_E2S                    source/interfaces/int25/i25sms_e2s.F
Chd|        I25STO                        source/interfaces/intsort/i25sto.F
Chd|        I25STO_E2S                    source/interfaces/intsort/i25sto_e2s.F
Chd|        I25STO_EDG                    source/interfaces/intsort/i25sto_edg.F
Chd|        I25TAGN                       source/interfaces/int25/i25norm.F
Chd|        I25TRC_E2S                    source/interfaces/intsort/i25trc_e2s.F
Chd|        I25TRC_EDG                    source/interfaces/intsort/i25trc_edg.F
Chd|        I25TRIVOX                     source/interfaces/intsort/i25trivox.F
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|        I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS05                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|        I7ASSIGEO0                    source/interfaces/int07/i7ass3.F
Chd|        I7BUCE                        source/interfaces/intsort/i7buce.F
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|        I7COR3                        source/interfaces/int07/i7cor3.F
Chd|        I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|        I7CORK3                       source/interfaces/int07/i7cork3.F
Chd|        I7CORP3                       source/interfaces/int07/i7cork3.F
Chd|        I7FOR3                        source/interfaces/int07/i7for3.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        I7OPTCD                       source/interfaces/intsort/i7optcd.F
Chd|        I7SMS2                        source/interfaces/int07/i7sms2.F
Chd|        I7TRC                         source/interfaces/intsort/i7trc.F
Chd|        I7TRI                         source/interfaces/intsort/i7tri.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|        IMPL_SAV                      source/interfaces/int24/i24main.F
Chd|        IMPL_SAV0                     source/interfaces/int24/i24main.F
Chd|        IMPL_SAV1                     source/interfaces/int24/i24main.F
Chd|        IMP_FR7I                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_STIF24                    source/implicit/imp_solv.F    
Chd|        INIT_TRIM                     source/interfaces/intsort/init_trim.F
Chd|        INT18_ALLOC                   source/interfaces/int18/int18_alloc.F
Chd|        INT18_LAW151_OMP_ACCUMULATION source/interfaces/int18/int18_law151_omp_accumulation.F
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|        INTER7_COLLISION_DETECTION    source/interfaces/inter7_collision_detection.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTMASS_UPDATE                source/interfaces/interf/intmass_update.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        I_CORPFIT3                    source/interfaces/int24/i24cor3.F
Chd|        I_COR_EPFIT3                  source/interfaces/int24/i24cor3.F
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        MULTI_I18_FORCE_POFF          source/interfaces/int18/multi_i18_force_poff.F
Chd|        MULTI_I18_FORCE_PON           source/interfaces/int18/multi_i18_force_pon.F
Chd|        PRINT_STIF                    source/implicit/imp_solv.F    
Chd|        REALLOCATE_FI1                source/mpi/interfaces/spmd_i25slide.F
Chd|        REALLOCATE_FI2                source/mpi/interfaces/spmd_i25slide.F
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_CELL_LIST_EXCHANGE       source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|        SPMD_CELL_SIZE_EXCHANGE       source/mpi/interfaces/spmd_cell_size_exchange.F
Chd|        SPMD_CELL_SIZE_EXCHANGE_INIT  source/mpi/interfaces/spmd_cell_size_exchange_init.F
Chd|        SPMD_CELL_SIZE_POST_RCV       source/mpi/interfaces/spmd_cell_size_post_rcv.F
Chd|        SPMD_CHECK_TAG                source/mpi/ams/spmd_check_tag.F
Chd|        SPMD_EXCH_EFRIC               source/mpi/interfaces/spmd_exch_efric.F
Chd|        SPMD_EXCH_I24                 source/mpi/interfaces/spmd_exch_i24.F
Chd|        SPMD_EXCH_I25                 source/mpi/interfaces/spmd_exch_i25.F
Chd|        SPMD_EXCH_INTER_18            source/mpi/interfaces/spmd_exch_inter_18.F
Chd|        SPMD_EXCH_PRESS               source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_EXCH_SMST2               source/mpi/ams/spmd_exch_smst2.F
Chd|        SPMD_EXCH_SORTING_EFRIC       source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|        SPMD_GET_PENIS                source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_PENIS20              source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF                 source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF11               source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF20               source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF20E              source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF25               source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF25_EDG           source/mpi/interfaces/spmd_getstif25_edg.F
Chd|        SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|        SPMD_I18KINE_COM_A            source/mpi/interfaces/spmd_i18kine_com_a.F
Chd|        SPMD_I18KINE_COM_ACC          source/mpi/interfaces/spmd_i18kine_com_acc.F
Chd|        SPMD_I18KINE_COM_MS           source/mpi/interfaces/spmd_i18kine_com_ms.F
Chd|        SPMD_I18KINE_COM_V            source/mpi/interfaces/spmd_i18kine_com_v.F
Chd|        SPMD_I18KINE_MACC_COM_POFF    source/mpi/interfaces/spmd_i18kine_macc_com_poff.F
Chd|        SPMD_I18KINE_MSF_COM_POFF     source/mpi/interfaces/spmd_i18kine_msf_com_poff.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        SPMD_I20NORMF                 source/mpi/interfaces/spmd_i20normf.F
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I24_PREPARE              source/interfaces/int24/i24_prepare.F
Chd|        SPMD_I25FRONT_INIT            source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25FRONT_NOR             source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25_PREPARE              source/interfaces/int25/i25_prepare.F
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7ITIED_CAND             source/mpi/interfaces/spmd_i7itied_cand.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_INT18_LAW151_PON         source/mpi/forces/spmd_int18_law151_pon.F
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUM25                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUM25_EDGE              source/mpi/interfaces/spmd_rnum25_edge.F
Chd|        SPMD_RNUMCD                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUMCD10                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUMCD11                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUMCD20                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SD_STFA20                source/mpi/interfaces/send_cand.F
Chd|        SPMD_SD_STFN                  source/mpi/interfaces/send_cand.F
Chd|        SPMD_SD_STFN11                source/mpi/interfaces/send_cand.F
Chd|        SPMD_SD_STFN20E               source/mpi/interfaces/send_cand.F
Chd|        SPMD_SD_STFN25                source/mpi/interfaces/send_cand.F
Chd|        SPMD_SPHVOX                   source/mpi/sph/spmd_sphvox.F  
Chd|        SPMD_SPHVOX0                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHVOX_OLD               source/mpi/sph/spmd_sphvox.F  
Chd|        SPMD_TRI10BOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI10GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11VOX0                source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI17BOX                 source/mpi/interfaces/spmd_tri17box.F
Chd|        SPMD_TRI17GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|        SPMD_TRI20BOXE                source/mpi/interfaces/spmd_tri20boxe.F
Chd|        SPMD_TRI20GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI20GATE                source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|        SPMD_TRI22VOX0                source/mpi/interfaces/spmd_tri22vox0.F
Chd|        SPMD_TRI23VOX0                source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24VOX0                source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI25EGAT                source/mpi/interfaces/spmd_tri25egat.F
Chd|        SPMD_TRI25GAT                 source/mpi/interfaces/spmd_tri25gat.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI25VOX0                source/mpi/interfaces/spmd_tri25vox0.F
Chd|        SPMD_TRI25VOX0_EDGE           source/mpi/interfaces/spmd_tri25vox0.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX0                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|        SPMD_WAIT_NB                  source/mpi/interfaces/spmd_wait_nb.F
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|        UPDATE_STRUCT_INT21           source/interfaces/int21/update_struct_int21.F
Chd|        UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|        POINTERDEF                    share/modules/pointerdef_mod.F
Chd|====================================================================
      MODULE TRI7BOX
      USE POINTERDEF
#include "my_real.inc"
      my_real, DIMENSION(:,:), ALLOCATABLE :: XREM
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IREM
C tableau temporaire boite secnd remote pour interface type 7, 10, 11 ou 20
C declare DOUBLE PRECISION pour tranfert entier
C XREM(1) : X(1)
C XREM(2) : X(2)
C XREM(3) : X(3)
C IREM(1) : num local
C XREM(4) : V(1)
C XREM(5) : V(2)
C XREM(6) : V(3)
C XREM(7) : MS
C XREM(8) : STFN
C IREM(2): ITAB
C IREM(3): KINET
C XREM(9): GAP_S (optionnel)
C tableau temporaire boite secnd remote pour interface type 11
C IREM(1) : num local
C XREM(1) : X1(1)
C XREM(2) : X1(2)
C XREM(3) : X1(3)
C XREM(4) : V1(1)
C XREM(5) : V1(2)
C XREM(6) : V1(3)
C XREM(7) : MS1   
C IREM(2):  ITAB1
C XREM(8) : X2(1)
C XREM(9) : X2(2)
C XREM(10) : X2(3)
C XREM(11) : V2(1)
C XREM(12) : V2(2)
C XREM(13) : V2(3)
C XREM(14) : MS2    
C IREM(2):  ITAB2
C XREM(15) : STFS
C IGAP>0
C XREM(16): GAP_S    (optionnel)
C IGAP=3
C XREM(17): GAP_S_L    (optionnel)
C INACTI=5 or 6
C XREM(18): PENIS(1) (optionnel)
C XREM(19): PENIS(2) (optionnel)
C IDTMINS=2
C IREM(4): NODNX_SMS(1) (optionnel)
C IREM(5): N1 (optionnel)
C IREM(6): NODNX_SMS(2) (optionnel)
C IREM(7): N2 (optionnel)
C IREM(4): NODNX_SMS(1) (optionnel)
C IDTMINS_INT/=0
C IREM(4): N1 (optionnel)
C IREM(5): N2 (optionnel)
C tableau temporaire boite secnd remote pour interface type 17
C XREM(1) : EMINXS(1)
C XREM(2) : EMINXS(2)
C XREM(3) : EMINXS(3)
C XREM(4) : EMINXS(4)
C XREM(5) : EMINXS(5)
C XREM(6) : EMINXS(6)
C XREM(7) : num local element
C XREM(8) : XFI17(1,1)  
C XREM(9):  XFI17(2,1)
C XREM(10) :XFI17(3,1)
C XREM(11) :VFI17(1,1) 
C XREM(12) :VFI17(2,1) 
C XREM(13) :VFI17(3,1) 
C XREM(14) :... 
C XREM(98) : XFI17(1,16)
C XREM(99) : XFI17(2,16)    
C XREM(100): XFI17(3,16)
C XREM(101) :VFI17(1,16)
C XREM(102): VFI17(2,16)
C XREM(103): VFI17(3,16)
C XREM(104): FROTSFI(1)
C XREM(105): FROTSFI(2)
C XREM(106): FROTSFI(3)
C XREM(107): FROTSFI(4)
C XREM(108): FROTSFI(5)
C XREM(109): FROTSFI(6)
C XREM(110): FROTSFI(7)
C XREM(111): KSFI(1)
C XREM(112): KSFI(2)

      LOGICAL, DIMENSION(:), ALLOCATABLE :: TO_TRIM, FIRST_TEST
C Pointeur dans XREM vers le debut des valeurs de l interface 24 & 25
C On utlise PANE_OLD
      INTEGER I24XREMP,IGAPXREMP,I24IREMP,I24IREMPNSNE
       
      INTEGER, DIMENSION(:), ALLOCATABLE :: CURRENT_NODFI
      INTEGER, DIMENSION(:), ALLOCATABLE :: CURRENT_FI_SIZE

C buffer envoi/reception forces interfaces
C tableau de pointeurs NSVFI de taille NINTER
C indiquant liste noeuds remote a recevoir du "proc main"
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSVFI

C Proc main of the secnd node. Assumes that only the main processor sends
C secnds nodes
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: PMAINFI

C tableau de pointeurs NSNFI de taille NINTER
C indiquant pour chaque interface type7 le nb de noeuds secnd dans NSVFI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: 
     .  NSNFI, NSNFI_SAV, ITAFI, KINFI, MATSFI,IPARTFRICSFI,IF_ADHFI,
     .  IPARTFRIC_FIE,
C tableau i20 specifique (flag nodal)
     .  NBINFLFI,
     .  NODNXFI, NODAMSFI, PROCAMSFI,
C tableau i20 partie edge
     .  NSNFIE,NSVFIE,ITAFIE,NODNXFIE, NODAMSFIE, PROCAMSFIE
C tableaux frontieres interfaces pour MS, STFN, STIFN
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: 
     .  MSFI, STIFI, STNFI, GAPFI,VSCFI, FTHEFI, FTHESKYFI,
     .  AREASFI, TEMPFI, ALPHAKFI, DIAG_SMSFI,GAP_LFI,CONDNFI,
     .  CONDNSKYFI,
C tableau i20 partie edge
     .  MSFIE, STIFIE, STNFIE, GAPFIE, VSCFIE, ALPHAKFIE,
     .  DIAG_SMSFIE,
C tableau I18 Kine
     .  MTFI_PENE,MTFI_PENEMIN
C tableaux frontieres interfaces pour X, V A ou FSKY
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  XFI, VFI, AFI, FSKYFI, PENFI, PENFIA,MTFI_V,MTFI_A,
     .  I18KAFI,MTFI_N,DAANCFI,
C tableau i20 partie edge
     .  XFIE, VFIE, AFIE, FSKYFIE, PENFIE, PENFIAE, DAANCFIE,
C NITSCHE METHOD
     .  FORNEQSFI
C tableaux frontieres interfaces 17 pour
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  EMINXFI, KSFI, FROTSFI, STNFI17
C tableaux frontieres interfaces 17 pour XFI17, VFI17, AFI17
      TYPE(real_pointer3), DIMENSION(:), ALLOCATABLE :: 
     .  XFI17, VFI17, AFI17
C tableaux frontieres interfaces 20 pour DXANC,DVANC
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  DXANCFI, DVANCFI, DXANCFIE, DVANCFIE
C tableaux frontieres interfaces 20 pour DAANC6
      TYPE(r8_pointer3), DIMENSION(:), ALLOCATABLE :: 
     .  DAANC6FI, DAANC6FIE
C tableau de pointeurs ISKYFI de taille NINTER indiquant pour chaque interface
C la liste des noeuds remote associe a une force dans FSKYI
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ISKYFI,
C tableau i20 partie edge
     .  ISKYFIE
C tableau de pointeurs NSVSI de taille NINTER
C indiquant liste noeuds remote a envoyer au "proc secnd"
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSVSI,
C tableau i20 partie edge
     .  NSVSIE
C tableau de pointeurs NSNSI de taille NINTER
C indiquant pour chaque interface type7, 10, 11 le nb de noeuds secnd dans NSVSI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSNSI,
C tableau i20 partie edge
     .  NSNSIE
C tableau de pointeurs NISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10 le nbre d elt dans LISUBSFI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NISUBSFI
C tableau de pointeurs LISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25 et pour chaque noeud remote de cette interface
C la liste des sous interfaces
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: LISUBSFI
C tableau de pointeurs INFLG_SUBSFI de taille NINTER
C indiquant pour chaque interface type25 et pour chaque noeud remote de cette interface
C l appartenance a S1, S2, ou GRNOD
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: INFLG_SUBSFI ,TAGNCONTFI
C tableau de pointeurs ADDSUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25, et pour chaque noeud remote de cette interface
C son adresse de depart dans LISUBSFI et INFLG_SUBSFI (tableau dimensionne a NSNR+1 (noeud remote + 1)
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ADDSUBSFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: FNCONTI, FTCONTI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: EFRICFI, EFRICGFI
      INTEGER , DIMENSION(:), ALLOCATABLE :: NLSKYFI, NLSKYFIE
C Temporary array for VOXEL SORTING
      INTEGER , DIMENSION(:), ALLOCATABLE :: NEXT_NOD
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_ELEM,CHAIN_NEXT
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_LAST 
      INTEGER , DIMENSION(:), ALLOCATABLE :: PREV_REMOTE_NUMBER 
      INTEGER LVOXEL
      PARAMETER(LVOXEL = 8000000)
      INTEGER INIVOXEL, VOXEL1(LVOXEL)
      DATA INIVOXEL /1/
      INTEGER LRVOXEL,LRVOXELP
C NB do not oversize  31
      PARAMETER(LRVOXEL = 31)
      PARAMETER(LRVOXELP = 8192)
C NB do not oversize PARASIZ
      INTEGER CRVOXEL(0:LRVOXEL,0:LRVOXEL,LRVOXELP)
C
C Flag array to tag if NSNFI was frozen into NSNFI_SAV - Case INACTI + TSTART
      INTEGER , DIMENSION(:), ALLOCATABLE :: NSNFI_FLAG
C Type 24 & 25 Interface
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ICONT_I_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IRTLM_FI
C T24 E2E
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IRTSE_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IS2SE_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: IS2PT_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISPT2_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISEGPT_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISEDGE_FI

      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: TIME_SFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: SECND_FRFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: PENE_OLDFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: STIF_OLDFI
C Type 25 Interface
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: ISLIDE_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: REMNOR_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: KREMNOR_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: ISKEW_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: ICODT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: STIF_MSDT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: STIFE_MSDT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: INTAREANFI

C AMS TYPE2 + Contact
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: T2FAC_SMS_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: T2MAIN_SMS_FI

      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: CANDF_SI
cc      my_real
cc     .     BGAPSMXG
      END MODULE TRI7BOX
