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
      !||====================================================================
      !||    tri7box                         ../engine/share/modules/tri7box.F
      !||--- called by ------------------------------------------------------
      !||    assparxx                        ../engine/source/assembly/assparxx.F
      !||    check_fi                        ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    cp_ifront                       ../engine/source/implicit/produt_v.F
      !||    deallocate_fi1_tmp              ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    frictionparts_model_ortho       ../engine/source/interfaces/int07/frictionparts_model.F
      !||    i10corp3                        ../engine/source/interfaces/int10/i10corp3.F
      !||    i10for3                         ../engine/source/interfaces/int10/i10for3.F
      !||    i10optcd                        ../engine/source/interfaces/intsort/i10optcd.F
      !||    i10sms2                         ../engine/source/interfaces/int10/i10sms2.F
      !||    i10tri                          ../engine/source/interfaces/intsort/i10tri.F
      !||    i11ass0                         ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass05                        ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass2                         ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass25                        ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass3                         ../engine/source/interfaces/int11/i11ass3.F
      !||    i11buce_vox                     ../engine/source/interfaces/intsort/i11buce.F
      !||    i11cor3                         ../engine/source/interfaces/int11/i11cor3.F
      !||    i11corp3                        ../engine/source/interfaces/int11/i11corp3.F
      !||    i11for3                         ../engine/source/interfaces/int11/i11for3.F
      !||    i11keg3                         ../engine/source/interfaces/int11/i11keg3.F
      !||    i11kfor3                        ../engine/source/interfaces/int11/i11keg3.F
      !||    i11main_tri                     ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11optcd                        ../engine/source/interfaces/intsort/i11optcd.F
      !||    i11pen3                         ../engine/source/interfaces/intsort/i11pen3.F
      !||    i11pen3_vox                     ../engine/source/interfaces/intsort/i11pen3.F
      !||    i11sms2                         ../engine/source/interfaces/int11/i11sms2.F
      !||    i11sto                          ../engine/source/interfaces/intsort/i11sto.F
      !||    i11sto_vox                      ../engine/source/interfaces/intsort/i11sto.F
      !||    i11trc                          ../engine/source/interfaces/intsort/i11trc.F
      !||    i11tri                          ../engine/source/interfaces/intsort/i11tri.F
      !||    i11trivox                       ../engine/source/interfaces/intsort/i11trivox.F
      !||    i17cut                          ../engine/source/interfaces/int17/i17tri.F
      !||    i17for3                         ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll4_pena                    ../engine/source/interfaces/int17/i17for3.F
      !||    i17vit_pena                     ../engine/source/interfaces/int17/i17for3.F
      !||    i18dst3                         ../engine/source/interfaces/int18/i18dst3.F
      !||    i18for3                         ../engine/source/interfaces/int18/i18for3.F
      !||    i18kine_f                       ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18kine_i                       ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18kine_s                       ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18kine_v                       ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_1                  ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_2                  ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18tri                          ../engine/source/interfaces/int18/i18tri.F
      !||    i20ass0                         ../engine/source/interfaces/int20/i20for3.F
      !||    i20ass05                        ../engine/source/interfaces/int20/i20for3.F
      !||    i20ass2                         ../engine/source/interfaces/int20/i20for3.F
      !||    i20ass25                        ../engine/source/interfaces/int20/i20for3.F
      !||    i20cor3                         ../engine/source/interfaces/int20/i20cor3.F
      !||    i20cor3e                        ../engine/source/interfaces/int20/i20cor3.F
      !||    i20cor3t                        ../engine/source/interfaces/int20/i20cor3t.F
      !||    i20for3                         ../engine/source/interfaces/int20/i20for3.F
      !||    i20for3c                        ../engine/source/interfaces/int20/i20for3.F
      !||    i20for3e                        ../engine/source/interfaces/int20/i20for3.F
      !||    i20optcd                        ../engine/source/interfaces/intsort/i20optcd.F
      !||    i20optcde                       ../engine/source/interfaces/intsort/i20optcd.F
      !||    i20pen3_edge                    ../engine/source/interfaces/intsort/i20sto.F
      !||    i20sms2e                        ../engine/source/interfaces/int20/i20sms2.F
      !||    i20sto                          ../engine/source/interfaces/intsort/i20sto.F
      !||    i20tri                          ../engine/source/interfaces/intsort/i20tri.F
      !||    i20tri_edge                     ../engine/source/interfaces/intsort/i20tri.F
      !||    i20xsinir                       ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i21ass3                         ../engine/source/interfaces/int21/i21ass3.F
      !||    i21buce                         ../engine/source/interfaces/intsort/i21buce.F
      !||    i21cor3                         ../engine/source/interfaces/int21/i21cor3.F
      !||    i21sms2                         ../engine/source/interfaces/int21/i21sms2.F
      !||    i22ass0                         ../engine/source/interfaces/int22/i22assembly.F
      !||    i22ass2                         ../engine/source/interfaces/int22/i22assembly.F
      !||    i22buce                         ../engine/source/interfaces/intsort/i22buce.F
      !||    i22cor3                         ../engine/source/interfaces/int22/i22cor3.F
      !||    i22main_tri                     ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22shell_getminmax              ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i23buce                         ../engine/source/interfaces/intsort/i23buce.F
      !||    i23cor3                         ../engine/source/interfaces/int23/i23cor3.F
      !||    i23cor3t                        ../engine/source/interfaces/int23/i23cor3t.F
      !||    i23for3                         ../engine/source/interfaces/int23/i23for3.F
      !||    i23main_tri                     ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23optcd                        ../engine/source/interfaces/int23/i23optcd.F
      !||    i23trivox                       ../engine/source/interfaces/intsort/i23trivox.F
      !||    i24_save_sub                    ../engine/source/interfaces/int24/i24_save_sub.F
      !||    i24ass0                         ../engine/source/interfaces/int24/i24for3.F
      !||    i24ass2                         ../engine/source/interfaces/int24/i24for3.F
      !||    i24buce                         ../engine/source/interfaces/intsort/i24buce.F
      !||    i24cor3                         ../engine/source/interfaces/int24/i24cor3.F
      !||    i24cor3t                        ../engine/source/interfaces/intsort/i24cor3t.F
      !||    i24cork3                        ../engine/source/interfaces/int24/i24cork3.F
      !||    i24corkm                        ../engine/source/interfaces/int24/i24cork3.F
      !||    i24corp3                        ../engine/source/interfaces/int24/i24cork3.F
      !||    i24dst3                         ../engine/source/interfaces/int24/i24dst3.F
      !||    i24dst3e                        ../engine/source/interfaces/int24/i24dst3e.F
      !||    i24edgt                         ../engine/source/interfaces/intsort/i24sto.F
      !||    i24for1_fic                     ../engine/source/interfaces/int24/i24for3e.F
      !||    i24for1_ficr                    ../engine/source/interfaces/int24/i24for3e.F
      !||    i24for3                         ../engine/source/interfaces/int24/i24for3.F
      !||    i24gap_pxfem                    ../engine/source/interfaces/int24/i24gap_pxfem.F
      !||    i24iply_pxfem                   ../engine/source/interfaces/int24/i24iply_pxfem.F
      !||    i24ispt2_ini                    ../engine/source/interfaces/int24/i24cor3.F
      !||    i24ispt2_ini_opttri             ../engine/source/interfaces/int24/i24cor3.F
      !||    i24keg3                         ../engine/source/interfaces/int24/i24ke3.F
      !||    i24main_tri                     ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24nexttria                     ../engine/source/interfaces/int24/i24dst3.F
      !||    i24nexttria2                    ../engine/source/interfaces/int24/i24dst3.F
      !||    i24nitschfor3                   ../engine/source/interfaces/int24/i24nitschfor3.F
      !||    i24optcd                        ../engine/source/interfaces/intsort/i24optcd.F
      !||    i24s1s2                         ../engine/source/interfaces/intsort/i24sto.F
      !||    i24sms2                         ../engine/source/interfaces/int24/i24for3.F
      !||    i24sto                          ../engine/source/interfaces/intsort/i24sto.F
      !||    i24trc                          ../engine/source/interfaces/intsort/i7trc.F
      !||    i24trivox                       ../engine/source/interfaces/intsort/i24trivox.F
      !||    i25ass0                         ../engine/source/interfaces/int25/i25ass3.F
      !||    i25ass2                         ../engine/source/interfaces/int25/i25ass3.F
      !||    i25ass3                         ../engine/source/interfaces/int25/i25ass3.F
      !||    i25asse0                        ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse05                       ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse05_e2s                   ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25asse0_e2s                    ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25asse2                        ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25                       ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25_e2s                   ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25asse2_e2s                    ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25buce                         ../engine/source/interfaces/intsort/i25buce.F
      !||    i25buce_edg                     ../engine/source/interfaces/intsort/i25buce_edg.F
      !||    i25comp_1                       ../engine/source/interfaces/int25/i25comp_1.F
      !||    i25comp_2                       ../engine/source/interfaces/int25/i25comp_2.F
      !||    i25cor3_1                       ../engine/source/interfaces/int25/i25cor3.F
      !||    i25cor3_21                      ../engine/source/interfaces/int25/i25cor3.F
      !||    i25cor3_22                      ../engine/source/interfaces/int25/i25cor3.F
      !||    i25cor3_3                       ../engine/source/interfaces/int25/i25cor3.F
      !||    i25cor3_e2s                     ../engine/source/interfaces/int25/i25cor3_e2s.F
      !||    i25cor3e                        ../engine/source/interfaces/int25/i25cor3e.F
      !||    i25cor3t                        ../engine/source/interfaces/intsort/i25cor3t.F
      !||    i25dst3_1                       ../engine/source/interfaces/int25/i25dst3_1.F
      !||    i25dst3_21                      ../engine/source/interfaces/int25/i25dst3_21.F
      !||    i25dst3_22                      ../engine/source/interfaces/int25/i25dst3_22.F
      !||    i25dst3_3                       ../engine/source/interfaces/int25/i25dst3_3.F
      !||    i25for3                         ../engine/source/interfaces/int25/i25for3.F
      !||    i25for3_e2s                     ../engine/source/interfaces/int25/i25for3_e2s.F
      !||    i25for3e                        ../engine/source/interfaces/int25/i25for3e.F
      !||    i25glob                         ../engine/source/interfaces/int25/i25dst3_1.F
      !||    i25glob_22                      ../engine/source/interfaces/int25/i25dst3_22.F
      !||    i25irtlm                        ../engine/source/interfaces/int25/i25irtlm.F
      !||    i25keepf                        ../engine/source/interfaces/int25/i25slid.F
      !||    i25main_free                    ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_opt_tri                 ../engine/source/interfaces/intsort/i25main_opt_tri.F
      !||    i25main_slid                    ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                     ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25maind_2                      ../engine/source/interfaces/int25/i25maind_2.F
      !||    i25mainf                        ../engine/source/interfaces/int25/i25mainf.F
      !||    i25optcd                        ../engine/source/interfaces/intsort/i25optcd.F
      !||    i25optcd_e2s                    ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25optcd_edg                    ../engine/source/interfaces/intsort/i25optcd_edg.F
      !||    i25pen3_e2s                     ../engine/source/interfaces/intsort/i25pen3_e2s.F
      !||    i25pen3_edg                     ../engine/source/interfaces/intsort/i25pen3_edg.F
      !||    i25prep_add                     ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_nindex                  ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_send                    ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_sizbufs                 ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_slid_1                  ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_slid_2                  ../engine/source/interfaces/int25/i25slid.F
      !||    i25s1s2                         ../engine/source/interfaces/intsort/i25sto.F
      !||    i25sms0e                        ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms2                         ../engine/source/interfaces/int25/i25for3.F
      !||    i25sms2e                        ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms_e2s                      ../engine/source/interfaces/int25/i25sms_e2s.F
      !||    i25sto                          ../engine/source/interfaces/intsort/i25sto.F
      !||    i25sto_e2s                      ../engine/source/interfaces/intsort/i25sto_e2s.F
      !||    i25sto_edg                      ../engine/source/interfaces/intsort/i25sto_edg.F
      !||    i25tagn                         ../engine/source/interfaces/int25/i25norm.F
      !||    i25trc_e2s                      ../engine/source/interfaces/intsort/i25trc_e2s.F
      !||    i25trc_edg                      ../engine/source/interfaces/intsort/i25trc_edg.F
      !||    i25trivox                       ../engine/source/interfaces/intsort/i25trivox.F
      !||    i25trivox_edg                   ../engine/source/interfaces/intsort/i25trivox_edg.F
      !||    i7ass0                          ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass05                         ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass2                          ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass25                         ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass33                         ../engine/source/interfaces/int07/i7ass3.F
      !||    i7assigeo0                      ../engine/source/interfaces/int07/i7ass3.F
      !||    i7buce                          ../engine/source/interfaces/intsort/i7buce.F
      !||    i7buce_vox                      ../engine/source/interfaces/intsort/i7buce.F
      !||    i7cor3                          ../engine/source/interfaces/int07/i7cor3.F
      !||    i7cor3t                         ../engine/source/interfaces/int07/i7cor3t.F
      !||    i7cork3                         ../engine/source/interfaces/int07/i7cork3.F
      !||    i7corp3                         ../engine/source/interfaces/int07/i7cork3.F
      !||    i7for3                          ../engine/source/interfaces/int07/i7for3.F
      !||    i7main_tri                      ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7optcd                         ../engine/source/interfaces/intsort/i7optcd.F
      !||    i7sms2                          ../engine/source/interfaces/int07/i7sms2.F
      !||    i7trc                           ../engine/source/interfaces/intsort/i7trc.F
      !||    i7tri                           ../engine/source/interfaces/intsort/i7tri.F
      !||    i7trivox                        ../engine/source/interfaces/intsort/i7trivox.F
      !||    i_cor_epfit3                    ../engine/source/interfaces/int24/i24cor3.F
      !||    i_corpfit3                      ../engine/source/interfaces/int24/i24cor3.F
      !||    imp_fr7i                        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_stif24                      ../engine/source/implicit/imp_solv.F
      !||    impl_sav                        ../engine/source/interfaces/int24/i24main.F
      !||    impl_sav0                       ../engine/source/interfaces/int24/i24main.F
      !||    impl_sav1                       ../engine/source/interfaces/int24/i24main.F
      !||    init_trim                       ../engine/source/interfaces/intsort/init_trim.F
      !||    int18_alloc                     ../engine/source/interfaces/int18/int18_alloc.F
      !||    int18_law151_omp_accumulation   ../engine/source/interfaces/int18/int18_law151_omp_accumulation.F
      !||    intcrit                         ../engine/source/interfaces/intsort/intcrit.F
      !||    inter7_collision_detection      ../engine/source/interfaces/inter7_collision_detection.F
      !||    inter_sort_07                   ../engine/source/interfaces/int07/inter_sort_07.F
      !||    intmass_update                  ../engine/source/interfaces/interf/intmass_update.F
      !||    inttri                          ../engine/source/interfaces/intsort/inttri.F
      !||    mpp_init                        ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    multi_i18_force_poff            ../engine/source/interfaces/int18/multi_i18_force_poff.F
      !||    multi_i18_force_pon             ../engine/source/interfaces/int18/multi_i18_force_pon.F
      !||    print_stif                      ../engine/source/implicit/imp_solv.F
      !||    reallocate_fi1                  ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    reallocate_fi2                  ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    spbuc3                          ../engine/source/elements/sph/spbuc3.F
      !||    sphtri0                         ../engine/source/elements/sph/sphtri0.F
      !||    spmd_cell_exchange              ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spmd_cell_list_exchange         ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
      !||    spmd_cell_size_exchange         ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
      !||    spmd_cell_size_exchange_init    ../engine/source/mpi/interfaces/spmd_cell_size_exchange_init.F
      !||    spmd_cell_size_post_rcv         ../engine/source/mpi/interfaces/spmd_cell_size_post_rcv.F
      !||    spmd_check_tag                  ../engine/source/mpi/ams/spmd_check_tag.F
      !||    spmd_exch_efric                 ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_i24                   ../engine/source/mpi/interfaces/spmd_exch_i24.F
      !||    spmd_exch_i25                   ../engine/source/mpi/interfaces/spmd_exch_i25.F
      !||    spmd_exch_inter_18              ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||    spmd_exch_press                 ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_smst2                 ../engine/source/mpi/ams/spmd_exch_smst2.F
      !||    spmd_exch_sorting_efric         ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_get_penis                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_penis20                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif                   ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif11                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif20                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif20e                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif25                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif25_edg             ../engine/source/mpi/interfaces/spmd_getstif25_edg.F
      !||    spmd_i17frots_pon               ../engine/source/mpi/interfaces/spmd_i17frots_pon.F
      !||    spmd_i18kine_com_a              ../engine/source/mpi/interfaces/spmd_i18kine_com_a.F
      !||    spmd_i18kine_com_acc            ../engine/source/mpi/interfaces/spmd_i18kine_com_acc.F
      !||    spmd_i18kine_com_ms             ../engine/source/mpi/interfaces/spmd_i18kine_com_ms.F
      !||    spmd_i18kine_com_v              ../engine/source/mpi/interfaces/spmd_i18kine_com_v.F
      !||    spmd_i18kine_macc_com_poff      ../engine/source/mpi/interfaces/spmd_i18kine_macc_com_poff.F
      !||    spmd_i18kine_msf_com_poff       ../engine/source/mpi/interfaces/spmd_i18kine_msf_com_poff.F
      !||    spmd_i18kine_pene_com_poff      ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_i20normf                   ../engine/source/mpi/interfaces/spmd_i20normf.F
      !||    spmd_i21fthecom                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i24_prepare                ../engine/source/interfaces/int24/i24_prepare.F
      !||    spmd_i25_prepare                ../engine/source/interfaces/int25/i25_prepare.F
      !||    spmd_i25_slide_gat              ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    spmd_i25front_init              ../engine/source/mpi/interfaces/spmd_i25front.F
      !||    spmd_i25front_nor               ../engine/source/mpi/interfaces/spmd_i25front.F
      !||    spmd_i7fcom_poff                ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                 ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7itied_cand               ../engine/source/mpi/interfaces/spmd_i7itied_cand.F
      !||    spmd_i7xvcom2                   ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                     ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_initfi                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_int18_law151_pon           ../engine/source/mpi/forces/spmd_int18_law151_pon.F
      !||    spmd_oldnumcd                   ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnum25                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnum25_edge                ../engine/source/mpi/interfaces/spmd_rnum25_edge.F
      !||    spmd_rnumcd                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnumcd10                   ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnumcd11                   ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnumcd20                   ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_savefi                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_sd_stfa20                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn11                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn20e                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn25                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sphvox                     ../engine/source/mpi/sph/spmd_sphvox.F
      !||    spmd_sphvox0                    ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphvox_old                 ../engine/source/mpi/sph/spmd_sphvox.F
      !||    spmd_tri10box                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri10gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri17box                   ../engine/source/mpi/interfaces/spmd_tri17box.F
      !||    spmd_tri17gat                   ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri18_151vox               ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri20box                   ../engine/source/mpi/interfaces/spmd_tri20box.F
      !||    spmd_tri20boxe                  ../engine/source/mpi/interfaces/spmd_tri20boxe.F
      !||    spmd_tri20gat                   ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri20gate                  ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri22vox                   ../engine/source/mpi/interfaces/spmd_tri22vox.F
      !||    spmd_tri22vox0                  ../engine/source/mpi/interfaces/spmd_tri22vox0.F
      !||    spmd_tri23vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri25egat                  ../engine/source/mpi/interfaces/spmd_tri25egat.F
      !||    spmd_tri25gat                   ../engine/source/mpi/interfaces/spmd_tri25gat.F
      !||    spmd_tri25vox                   ../engine/source/mpi/interfaces/spmd_tri25vox.F
      !||    spmd_tri25vox0                  ../engine/source/mpi/interfaces/spmd_tri25vox0.F
      !||    spmd_tri25vox0_edge             ../engine/source/mpi/interfaces/spmd_tri25vox0.F
      !||    spmd_tri7gat                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox0                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox_optimized          ../engine/source/mpi/interfaces/spmd_tri7vox_optimized.F
      !||    spmd_wait_nb                    ../engine/source/mpi/interfaces/spmd_wait_nb.F
      !||    sptrivox                        ../engine/source/elements/sph/sptrivox.F
      !||    update_struct_int21             ../engine/source/interfaces/int21/update_struct_int21.F
      !||    upgrade_rem_2ry                 ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    pointerdef                      ../engine/share/modules/pointerdef_mod.F
      !||====================================================================
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
