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
      !||    h3d_mod                                      ../engine/share/modules/h3d_mod.F
      !||--- called by ------------------------------------------------------
      !||    alemain                                      ../engine/source/ale/alemain.F
      !||    ani_pcont                                    ../engine/source/output/anim/generate/ani_pcont.F
      !||    ani_pcont21                                  ../engine/source/output/anim/generate/ani_pcont.F
      !||    arret                                        ../engine/source/system/arret.F
      !||    ass18                                        ../engine/source/interfaces/int18/ass18.F
      !||    clusterf                                     ../engine/source/output/cluster/clusterf.F
      !||    create_h3d_1d_scalar                         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
      !||    create_h3d_1d_tensor                         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
      !||    create_h3d_1d_torsor                         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
      !||    create_h3d_1d_vector                         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
      !||    create_h3d_arg_keyword                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
      !||    create_h3d_input                             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_input.F
      !||    create_h3d_nodal_scalar                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
      !||    create_h3d_nodal_tensor                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
      !||    create_h3d_nodal_vector                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
      !||    create_h3d_parts                             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts.F
      !||    create_h3d_parts_all                         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
      !||    create_h3d_quad_scalar                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
      !||    create_h3d_quad_tensor                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
      !||    create_h3d_quad_vector                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
      !||    create_h3d_shell_scalar                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
      !||    create_h3d_shell_tensor                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
      !||    create_h3d_shell_vector                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
      !||    create_h3d_skin_scalar                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
      !||    create_h3d_skin_tensor                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
      !||    create_h3d_skin_vector                       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
      !||    create_h3d_solid_scalar                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
      !||    create_h3d_solid_tensor                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
      !||    create_h3d_solid_vector                      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
      !||    create_h3d_sph_scalar                        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
      !||    create_h3d_sph_tensor                        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
      !||    dmi_rbe3                                     ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    dtnoda                                       ../engine/source/time_step/dtnoda.F
      !||    dtnodams                                     ../engine/source/time_step/dtnodams.F
      !||    dyna_ina                                     ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex                                     ../engine/source/implicit/imp_dyna.F
      !||    ecrit                                        ../engine/source/output/ecrit.F
      !||    eig                                          ../engine/stub/eig.F
      !||    eig1                                         ../engine/stub/eig1.F
      !||    eigcond                                      ../engine/stub/eigcond.F
      !||    eigp                                         ../engine/stub/eigp.F
      !||    forani1                                      ../engine/source/output/anim/generate/forani1.F
      !||    forani2                                      ../engine/source/output/anim/generate/forani2.F
      !||    forani3                                      ../engine/source/output/anim/generate/forani3.F
      !||    force                                        ../engine/source/loads/general/force.F90
      !||    force_imp                                    ../engine/source/loads/general/force_imp.F
      !||    forcefingeo                                  ../engine/source/loads/general/forcefingeo.F
      !||    forcepinch                                   ../engine/source/loads/general/forcepinch.F
      !||    forint                                       ../engine/source/elements/forint.F
      !||    freform                                      ../engine/source/input/freform.F
      !||    fvbag0                                       ../engine/source/airbag/fvbag0.F
      !||    fvbag1                                       ../engine/source/airbag/fvbag1.F
      !||    fvbag2                                       ../engine/source/airbag/fvbag2.F
      !||    genani                                       ../engine/source/output/anim/generate/genani.F
      !||    genh3d                                       ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    h3d_create_datatype                          ../engine/source/output/h3d/h3d_build_fortran/h3d_create_datatype.F
      !||    h3d_gene_keyword                             ../engine/source/output/h3d/input_list/h3d_gene_keyword.F
      !||    h3d_ini                                      ../engine/source/output/h3d/h3d_build_fortran/h3d_ini.F
      !||    h3d_list_1d_scalar                           ../engine/source/output/h3d/input_list/h3d_list_1d_scalar.F
      !||    h3d_list_1d_tensor                           ../engine/source/output/h3d/input_list/h3d_list_1d_tensor.F
      !||    h3d_list_1d_torsor                           ../engine/source/output/h3d/input_list/h3d_list_1d_torsor.F
      !||    h3d_list_1d_vector                           ../engine/source/output/h3d/input_list/h3d_list_1d_vector.F
      !||    h3d_list_nodal_scalar                        ../engine/source/output/h3d/input_list/h3d_list_noda_scalar.F
      !||    h3d_list_nodal_tensor                        ../engine/source/output/h3d/input_list/h3d_list_noda_tensor.F
      !||    h3d_list_nodal_vector                        ../engine/source/output/h3d/input_list/h3d_list_noda_vector.F
      !||    h3d_list_quad_scalar                         ../engine/source/output/h3d/input_list/h3d_list_quad_scalar.F
      !||    h3d_list_quad_tensor                         ../engine/source/output/h3d/input_list/h3d_list_quad_tensor.F
      !||    h3d_list_quad_vector                         ../engine/source/output/h3d/input_list/h3d_list_quad_vector.F
      !||    h3d_list_shell_scalar                        ../engine/source/output/h3d/input_list/h3d_list_shell_scalar.F
      !||    h3d_list_shell_tensor                        ../engine/source/output/h3d/input_list/h3d_list_shell_tensor.F
      !||    h3d_list_shell_vector                        ../engine/source/output/h3d/input_list/h3d_list_shell_vector.F
      !||    h3d_list_solid_scalar                        ../engine/source/output/h3d/input_list/h3d_list_solid_scalar.F
      !||    h3d_list_solid_tensor                        ../engine/source/output/h3d/input_list/h3d_list_solid_tensor.F
      !||    h3d_list_solid_vector                        ../engine/source/output/h3d/input_list/h3d_list_solid_vector.F
      !||    h3d_list_sph_scalar                          ../engine/source/output/h3d/input_list/h3d_list_sph_scalar.F
      !||    h3d_list_sph_tensor                          ../engine/source/output/h3d/input_list/h3d_list_sph_tensor.F
      !||    h3d_nodal_scalar                             ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    h3d_pre_skin_scalar                          ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_read                                     ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    h3d_skin_scalar                              ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector                              ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    h3d_solid_scalar                             ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_scalar_1                           ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_solid_vector                             ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    i10for3                                      ../engine/source/interfaces/int10/i10for3.F
      !||    i10main_tri                                  ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i10mainf                                     ../engine/source/interfaces/int10/i10mainf.F
      !||    i11ass3                                      ../engine/source/interfaces/int11/i11ass3.F
      !||    i11mainf                                     ../engine/source/interfaces/int11/i11mainf.F
      !||    i14cmp                                       ../engine/source/interfaces/int14/i14cmp.F
      !||    i14frt                                       ../engine/source/interfaces/int14/i14frt.F
      !||    i15ass                                       ../engine/source/interfaces/int15/i15ass.F
      !||    i15cmp                                       ../engine/source/interfaces/int15/i15cmp.F
      !||    i17for3                                      ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll4_pena                                 ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll_pena                                  ../engine/source/interfaces/int17/i17for3.F
      !||    i17main_pena                                 ../engine/source/interfaces/int17/i17main_pena.F
      !||    i18for3                                      ../engine/source/interfaces/int18/i18for3.F
      !||    i18kine_s                                    ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_1                               ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_2                               ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_s                               ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20buce_crit                                 ../engine/source/interfaces/intsort/i20buce_crit.F
      !||    i20for3                                      ../engine/source/interfaces/int20/i20for3.F
      !||    i20for3e                                     ../engine/source/interfaces/int20/i20for3.F
      !||    i20main_crit_tri                             ../engine/source/interfaces/intsort/i20main_crit_tri.F
      !||    i20main_tri                                  ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                                     ../engine/source/interfaces/int20/i20mainf.F
      !||    i21ass3                                      ../engine/source/interfaces/int21/i21ass3.F
      !||    i21mainf                                     ../engine/source/interfaces/int21/i21mainf.F
      !||    i22for3                                      ../engine/source/interfaces/int22/i22for3.F
      !||    i22main_tri                                  ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                                     ../engine/source/interfaces/int22/i22mainf.F
      !||    i23for3                                      ../engine/source/interfaces/int23/i23for3.F
      !||    i23main_tri                                  ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                                     ../engine/source/interfaces/int23/i23mainf.F
      !||    i24dst3e                                     ../engine/source/interfaces/int24/i24dst3e.F
      !||    i24for3                                      ../engine/source/interfaces/int24/i24for3.F
      !||    i24main_tri                                  ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                                     ../engine/source/interfaces/int24/i24main.F
      !||    i25for3                                      ../engine/source/interfaces/int25/i25for3.F
      !||    i25for3_e2s                                  ../engine/source/interfaces/int25/i25for3_e2s.F
      !||    i25for3e                                     ../engine/source/interfaces/int25/i25for3e.F
      !||    i25main_slid                                 ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                                  ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                                     ../engine/source/interfaces/int25/i25mainf.F
      !||    i2curvf                                      ../engine/source/interfaces/interf/i2curvf.F
      !||    i2curvfp                                     ../engine/source/interfaces/interf/i2curvfp.F
      !||    i2fomo3                                      ../engine/source/interfaces/interf/i2for3.F
      !||    i2fomo3p                                     ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for25                                      ../engine/source/interfaces/interf/i2for25.F
      !||    i2for25p                                     ../engine/source/interfaces/interf/i2for25p.F
      !||    i2for26                                      ../engine/source/interfaces/interf/i2for26.F
      !||    i2for26p                                     ../engine/source/interfaces/interf/i2for26p.F
      !||    i2for27                                      ../engine/source/interfaces/interf/i2for27.F
      !||    i2for27_cin                                  ../engine/source/interfaces/interf/i2for27_cin.F
      !||    i2for27_pen                                  ../engine/source/interfaces/interf/i2for27_pen.F
      !||    i2for27p                                     ../engine/source/interfaces/interf/i2for27p.F
      !||    i2for27p_cin                                 ../engine/source/interfaces/interf/i2for27p_cin.F
      !||    i2for27p_pen                                 ../engine/source/interfaces/interf/i2for27p_pen.F
      !||    i2for28                                      ../engine/source/interfaces/interf/i2for28.F
      !||    i2for28_cin                                  ../engine/source/interfaces/interf/i2for28_cin.F
      !||    i2for28_pen                                  ../engine/source/interfaces/interf/i2for28_pen.F
      !||    i2for28p                                     ../engine/source/interfaces/interf/i2for28p.F
      !||    i2for28p_cin                                 ../engine/source/interfaces/interf/i2for28p_cin.F
      !||    i2for28p_pen                                 ../engine/source/interfaces/interf/i2for28p_pen.F
      !||    i2for3                                       ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3n                                      ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3o                                      ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3p                                      ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for3pn                                     ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for3po                                     ../engine/source/interfaces/interf/i2for3p.F
      !||    i2forces                                     ../engine/source/interfaces/interf/i2forces.F
      !||    i2forces_2d                                  ../engine/source/interfaces/interf/i2forces_2D.F
      !||    i2mom27_cin                                  ../engine/source/interfaces/interf/i2mom27_cin.F
      !||    i2mom27p_cin                                 ../engine/source/interfaces/interf/i2mom27p_cin.F
      !||    i2mom3n                                      ../engine/source/interfaces/interf/i2for3.F
      !||    i2mom3pn                                     ../engine/source/interfaces/interf/i2for3p.F
      !||    i2rupt                                       ../engine/source/interfaces/interf/int2rupt.F
      !||    i3for2                                       ../engine/source/interfaces/inter2d/i3for2.F
      !||    i3for3                                       ../engine/source/interfaces/inter3d/i3for3.F
      !||    i3fri3                                       ../engine/source/interfaces/inter3d/i3fri3.F
      !||    i5for3                                       ../engine/source/interfaces/inter3d/i5for3.F
      !||    i5fri3                                       ../engine/source/interfaces/inter3d/i3fri3.F
      !||    i6ass3                                       ../engine/source/interfaces/inter3d/i6ass3.F
      !||    i6main                                       ../engine/source/interfaces/inter3d/i6main.F
      !||    i7ass33                                      ../engine/source/interfaces/int07/i7ass3.F
      !||    i7assigeo0                                   ../engine/source/interfaces/int07/i7ass3.F
      !||    i7for3                                       ../engine/source/interfaces/int07/i7for3.F
      !||    i7main_tri                                   ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                                      ../engine/source/interfaces/int07/i7mainf.F
      !||    i8for3                                       ../engine/source/interfaces/inter3d/i8for3.F
      !||    imp_buck                                     ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                                     ../engine/source/implicit/imp_solv.F
      !||    imp_fanie                                    ../engine/source/implicit/imp_solv.F
      !||    imp_fanii                                    ../engine/source/implicit/imp_solv.F
      !||    imp_fout                                     ../engine/source/implicit/imp_solv.F
      !||    imp_inttd0                                   ../engine/source/implicit/imp_int_k.F
      !||    imp_solv                                     ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                                    ../engine/source/implicit/imp_int_k.F
      !||    iniresa                                      ../engine/source/output/restart/rdresa.F
      !||    int2rupt                                     ../engine/source/interfaces/interf/int2rupt.F
      !||    intal1                                       ../engine/source/ale/inter/intal1.F
      !||    intal2                                       ../engine/source/ale/inter/intal2.F
      !||    inter_deallocate_wait                        ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_sort                                   ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07                                ../engine/source/interfaces/int07/inter_sort_07.F
      !||    intfop1                                      ../engine/source/interfaces/interf/intfop1.F
      !||    intfop2                                      ../engine/source/interfaces/interf/intfop2.F
      !||    intfop8                                      ../engine/source/interfaces/interf/intfop8.F
      !||    intti1                                       ../engine/source/interfaces/interf/intti1.F
      !||    intti2f                                      ../engine/source/interfaces/interf/intti2f.F
      !||    inttri                                       ../engine/source/interfaces/intsort/inttri.F
      !||    intvo2                                       ../engine/source/interfaces/inter2d/intvo2.F
      !||    intvo3                                       ../engine/source/interfaces/inter3d/intvo3.F
      !||    intvo8                                       ../engine/source/interfaces/inter3d/intvo8.F
      !||    iqela1                                       ../engine/source/ale/inter/iqela1.F
      !||    lag_anith                                    ../engine/source/tools/lagmul/lag_anith.F
      !||    lag_anithp                                   ../engine/source/tools/lagmul/lag_anith.F
      !||    lag_mult                                     ../engine/source/tools/lagmul/lag_mult.F
      !||    lag_multp                                    ../engine/source/tools/lagmul/lag_mult.F
      !||    lech3d                                       ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lecinp                                       ../engine/source/input/lecinp.F
      !||    lectur                                       ../engine/source/input/lectur.F
      !||    load_pressure                                ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    manctr                                       ../engine/source/input/manctr.F
      !||    monvol0                                      ../engine/source/airbag/monvol0.F
      !||    multi_allocate                               ../engine/source/multifluid/multi_allocate.F
      !||    pbilan                                       ../engine/source/elements/beam/pbilan.F
      !||    pblast_1                                     ../engine/source/loads/pblast/pblast_1.F
      !||    pblast_2                                     ../engine/source/loads/pblast/pblast_2.F
      !||    pblast_3                                     ../engine/source/loads/pblast/pblast_3.F
      !||    pblast_load_computation                      ../engine/source/loads/pblast/pblast.F
      !||    pfluid                                       ../engine/source/loads/general/pfluid/pfluid.F
      !||    pforc3                                       ../engine/source/elements/beam/pforc3.F
      !||    pressure_cyl                                 ../engine/source/loads/general/load_pcyl/pressure_cyl.F
      !||    r1tors                                       ../engine/source/elements/spring/r1tors.F
      !||    r23forc3                                     ../engine/source/elements/spring/r23forc3.F
      !||    r23law108                                    ../engine/source/elements/spring/r23law108.F
      !||    r23law113                                    ../engine/source/elements/spring/r23law113.F
      !||    r23law114                                    ../engine/source/elements/spring/r23law114.F
      !||    r2tors                                       ../engine/source/elements/spring/r2tors.F
      !||    r3tors                                       ../engine/source/elements/spring/r3tors.F
      !||    r4tors                                       ../engine/source/elements/spring/r4tors.F
      !||    radioss2                                     ../engine/source/engine/radioss2.F
      !||    ranim33                                      ../engine/source/elements/joint/ranim33.F
      !||    rbe3t1                                       ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rdcomi                                       ../engine/source/output/restart/rdcomm.F
      !||    rdresa                                       ../engine/source/output/restart/rdresa.F
      !||    rdresb                                       ../engine/source/output/restart/rdresb.F
      !||    reaction_forces_check_for_requested_output   ../engine/source/output/reactions.F
      !||    reallocate_fi2                               ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    resol                                        ../engine/source/engine/resol.F
      !||    resol_head                                   ../engine/source/engine/resol_head.F
      !||    resol_init                                   ../engine/source/engine/resol_init.F
      !||    restalloc                                    ../engine/source/output/restart/arralloc.F
      !||    rforc3                                       ../engine/source/elements/spring/rforc3.F
      !||    rgjoint                                      ../engine/source/elements/joint/rgjoint.F
      !||    ruptint2                                     ../engine/source/interfaces/interf/ruptint2.F
      !||    sortie_main                                  ../engine/source/output/sortie_main.F
      !||    spmd_exch2_a_pon                             ../engine/source/mpi/forces/spmd_exch2_a_pon.F
      !||    spmd_exch_a                                  ../engine/source/mpi/forces/spmd_exch_a.F
      !||    spmd_exch_a_ams_poff                         ../engine/source/mpi/forces/spmd_exch_a_ams_poff.F
      !||    spmd_exch_a_int2                             ../engine/source/mpi/forces/spmd_exch_a_int2.F
      !||    spmd_exch_a_int2_ams                         ../engine/source/mpi/forces/spmd_exch_a_int2_ams.F
      !||    spmd_exch_a_int2_pon                         ../engine/source/mpi/forces/spmd_exch_a_int2_pon.F
      !||    spmd_exch_a_int2h                            ../engine/source/mpi/forces/spmd_exch_a_int2h.F
      !||    spmd_exch_a_int2h_ams                        ../engine/source/mpi/forces/spmd_exch_a_int2h_ams.F
      !||    spmd_exch_efric                              ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_sorting_efric                      ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_fiadd11_poff                            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd11_pon                             ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd17_poff                            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd17_pon                             ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20_poff                            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20e_poff                           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20f_pon                            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20fe_pon                           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd25e_poff                           ../engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
      !||    spmd_fiadd25e_pon                            ../engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
      !||    spmd_fiadd_poff                              ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd_pon                               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_i18kine_pene_com_poff                   ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_i25_slide_gat                           ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    spmd_i7fcom_poff                             ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                              ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_initfi                                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_tri10gat                                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri20gat                                ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri24gat                                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri25gat                                ../engine/source/mpi/interfaces/spmd_tri25gat.F
      !||    spmd_tri7gat                                 ../engine/source/mpi/interfaces/spmd_int.F
      !||    stop_sensor                                  ../engine/source/tools/sensor/stop_sensor.F
      !||    tbilan                                       ../engine/source/elements/truss/tbilan.F
      !||    tforc3                                       ../engine/source/elements/truss/tforc3.F
      !||    tmax_ipart                                   ../engine/source/output/tmax_ipart.F
      !||    volpre                                       ../engine/source/airbag/volpres.F
      !||    volprep                                      ../engine/source/airbag/volpresp.F
      !||    wrcomi                                       ../engine/source/output/restart/wrcomm.F
      !||    wrrestp                                      ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE H3D_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
c
      LOGICAL :: IS_H3D_USED = .FALSE. 
C-----------------------------------------------
C   H3D_KEYWORD : LIST OF AVAILABLE /H3D OPTIONS
C-----------------------------------------------
      TYPE H3D_KEYWORD
          CHARACTER(LEN=100) ::  KEY3 = ''
          CHARACTER(LEN=100) ::  KEY4 = ''
          CHARACTER(LEN=100) ::  KEY5 = ''
          CHARACTER(LEN=100) ::  KEY6 = ''
          CHARACTER(LEN=100) ::  KEY7 = ''
          CHARACTER(LEN=100) ::  KEY8 = ''
          INTEGER ::  ID = 0 
          CHARACTER(LEN=50) ::   TEXT1 = ''
          CHARACTER(LEN=50) ::   TEXT2 = ''
          CHARACTER(LEN=80) ::   COMMENT = ''
c
          INTEGER ::  IS_PLY = 0 
          INTEGER ::  IS_PLY_ALL = 0 
          INTEGER ::  IS_LAYER = 0 
          INTEGER ::  IS_LAYER_ALL = 0 
          INTEGER ::  IS_IPT = 0 
          INTEGER ::  IS_IPT_ALL = 0 
          INTEGER ::  IS_GAUSS = 0 
          INTEGER ::  IS_GAUSS_ALL = 0 
          INTEGER ::  IS_UVAR = 0 
          INTEGER ::  IS_UVAR_ALL = 0 
          INTEGER ::  IS_IR = 0 
          INTEGER ::  IS_IR_ALL = 0 
          INTEGER ::  IS_IS = 0 
          INTEGER ::  IS_IS_ALL = 0 
          INTEGER ::  IS_IT = 0 
          INTEGER ::  IS_IT_ALL = 0 
          INTEGER ::  IS_ID = 0 
          INTEGER ::  IS_ID_ALL = 0 
          INTEGER ::  IS_INTER = 0 
          INTEGER ::  IS_INTER_ALL = 0 
          INTEGER ::  IS_SKIN = 0 
          INTEGER ::  IS_CORNER_DATA = 0
          INTEGER ::  IS_MDSVAR = 0 
          INTEGER ::  IS_MDSVAR_ALL = 0 
          INTEGER ::  IS_MDSVAR_DEF = 0  
          INTEGER ::  IS_MODE = 0 
          INTEGER ::  IS_MODE_ALL = 0 
c
      END TYPE H3D_KEYWORD
C-----------------------------------------------
C   H3D_INPUT_LIST : LIST OF /H3D KEYWORDS READ IN 1.rad
C-----------------------------------------------
      TYPE H3D_INPUT
          CHARACTER(LEN=20) :: KEY2 = ''
          CHARACTER(LEN=20) ::  KEY3 = ''
          CHARACTER(LEN=20) ::  KEY4 = ''
          CHARACTER(LEN=20) ::  KEY5 = ''
          CHARACTER(LEN=20) ::  KEY6 = ''
          CHARACTER(LEN=20) ::  KEY7 = ''
          CHARACTER(LEN=20) ::  KEY8 = ''
          INTEGER ::  NB_PART=0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
      END TYPE H3D_INPUT
C-----------------------------------------------
C   H3D_PARTS : LIST OF OUTPUTTED PARTS
C-----------------------------------------------
      TYPE H3D_PART_LIST
          INTEGER :: NB_PART=0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
          INTEGER, DIMENSION(:), ALLOCATABLE :: NODES
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART
      END TYPE H3D_PART_LIST
C-----------------------------------------------
C   H3D_OUTPUT_LIST : OUTPUT DATABASE
C-----------------------------------------------
      TYPE H3D_OUTPUT
          INTEGER ::  ID = 0
          INTEGER ::  OK = 0
          INTEGER ::  N_OUTP = 0
          CHARACTER(LEN=100) :: KEYWORD = ''
          INTEGER ::  ETYPE = 0
          INTEGER ::  OUTP_TYPE = 0
          INTEGER ::  IS_INFO1 = 0
          INTEGER ::  INFO1 = 0
          INTEGER ::  IS_INFO2 = 0
          INTEGER ::  INFO2 = 0
          INTEGER ::  PLY = 0
          INTEGER ::  LAYER = 0
          INTEGER ::  LAYER_UPPER = 0
          INTEGER ::  LAYER_LOWER = 0
          INTEGER ::  LAYER_MEMB = 0
          INTEGER ::  LAYER_BEND = 0
          INTEGER ::  IPT = 0
          INTEGER ::  IS_IPT_UPPER = 0
          INTEGER ::  IS_IPT_LOWER = 0
          INTEGER ::  IS_IPT_MEMB = 0
          INTEGER ::  IS_IPT_BEND = 0
          INTEGER ::  IUVAR = 0
          INTEGER ::  IMDSVAR = 0
          INTEGER ::  IDMDS = 0
          INTEGER ::  IDMATMDS = 0
          CHARACTER(LEN=64) ::  MDSVAR_NAME = ''
          INTEGER ::  SMDSVAR_NAME = 0
          INTEGER ::  GAUSS = 0
          INTEGER ::  IR = 0
          INTEGER ::  IS = 0
          INTEGER ::  IT = 0
          INTEGER ::  OBJECT_ID = 0
          INTEGER ::  MODE = 0
          INTEGER ::  IS_CORNER_DATA = 0
          CHARACTER(LEN=80) ::  STRING1 = ''
          INTEGER ::   S_STRING1 = 0
          CHARACTER(LEN=80) ::  STRING2 = ''
          INTEGER ::   S_STRING2 = 0
          CHARACTER(LEN=120) ::  COMMENT = ''
          INTEGER ::   S_COMMENT = 0
          INTEGER ::  N_H3D_PART_LIST = 0
          INTEGER ::  INTER = 0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART
      END TYPE H3D_OUTPUT
C-----------------------------------------------
C   H3D_DATABASE 
C-----------------------------------------------
      TYPE H3D_DATABASE

          
        INTEGER ::  N_TITLE = 0
        INTEGER ::  N_SENS_H3D = 0
        INTEGER ::  N_OUTP_H3D = 0
        INTEGER ::  IH3D = 0
        INTEGER ::  IH3D_RUN = 0
        INTEGER ::  N_INPUT_H3D = 0
        INTEGER ::  IPART_SELECT = 0
        INTEGER ::  MH3D = 0

        INTEGER ::  N_SCAL_DT = 0               !ANIM_N(1)
        INTEGER ::  N_SCAL_DMAS   = 0           !ANIM_N(2)
        INTEGER ::  N_SCAL_DINER = 0            !ANIM_N(12)
        INTEGER ::  N_SCAL_DAMA2 = 0            !ANIM_N(15&16)
        INTEGER ::  N_SCAL_SKID = 0               
        INTEGER ::  N_SCAL_STIFR = 0            !ANIM_N(18)
        INTEGER ::  N_SCAL_STIFN = 0            !ANIM_N(19)
        INTEGER ::  N_SCAL_CSE_FRIC = 0      
        INTEGER ::  N_SCAL_CSE_FRICINT = 0          

        INTEGER ::  N_VECT_CONT = 0             !ANIM_V(4)
        INTEGER ::  N_VECT_FINT = 0             !ANIM_V(5)
        INTEGER ::  N_VECT_FEXT = 0             !ANIM_V(6)
        INTEGER ::  N_VECT_PCONT = 0            !ANIM_V(12)
        INTEGER ::  N_VECT_CONT2 = 0            !ANIM_V(13)
        INTEGER ::  N_VECT_DROT = 0             !ANIM_V(14)
        INTEGER ::  N_VECT_DXANC  = 0           !ANIM_V(15)
        INTEGER ::  N_VECT_FREAC = 0            !ANIM_V(17)
        INTEGER ::  N_VECT_MREAC = 0            !ANIM_V(18)
        INTEGER ::  N_VECT_CLUST_FORCE = 0      !ANIM_V(19)
        INTEGER ::  N_VECT_CLUST_MOM = 0        !ANIM_V(20)
        INTEGER ::  N_VECT_CONT_MAX  = 0        !ANIM_V(26)
        INTEGER ::  N_VECT_PCONT2 = 0           !ANIM_V(27)
        INTEGER ::  N_VECT_PCONT_MAX = 0     
        INTEGER ::  N_VECT_CONT2_MAX = 0             
        INTEGER ::  N_VECT_PCONT2_MAX = 0     
        INTEGER ::  N_VECT_CONT2_MIN  = 0            
        INTEGER ::  N_VECT_PCONT2_MIN  = 0
        INTEGER ::  N_VECT_CONT2M  = 0
        INTEGER ::  N_VECT_ACC = 0  !FVM acceleration (law151, colocated scheme)

        INTEGER ::  SH_SCAL_ERR_THK = 0         !ANIM_CE(2156)

        INTEGER ::  SOL_SCAL_VORTX = 0          !ANIM_SE(10)
        INTEGER ::  SOL_SCAL_VORTY = 0          !ANIM_SE(4960)
        INTEGER ::  SOL_SCAL_VORTZ = 0          !ANIM_SE(4961)

        INTEGER ::  SH_IEPSDOT  = 0             !ANIM_C(...)
        INTEGER ::  UND_FORC = 0                !ANIM_FT(1)
        INTEGER ::  STRAIN   = 0                !ANIM STRAIN TENSOR

        INTEGER ::  IS_CORNER_DATA = 0
        INTEGER ::  RBODY_SINGLE = 0
        INTEGER ::  RBE2_SINGLE = 0
        INTEGER ::  RBE3_SINGLE = 0

        INTEGER ::  COMP_LEVEL = 0

        my_real  :: TH3D  = 0.
        my_real  :: DTH3D= 0.
        my_real  :: TH3D_STOP= 0.
        my_real  :: TH3D0= 0.
        my_real  :: DTH3D0= 0.
        my_real  :: TH3D_STOP0= 0.
        my_real  :: PERCENTAGE_ERROR= 0.

        INTEGER, DIMENSION(:), ALLOCATABLE :: N_SKID_INTER   
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITITLE 
        INTEGER, DIMENSION(:), ALLOCATABLE :: LSENS_H3D      
        INTEGER, DIMENSION(:), ALLOCATABLE :: N_CSE_FRIC_INTER   

        CHARACTER(LEN=80), DIMENSION(:), ALLOCATABLE ::TITLE        

        TYPE (H3D_INPUT), DIMENSION(:), ALLOCATABLE :: INPUT_LIST

        TYPE (H3D_PART_LIST), DIMENSION(:), ALLOCATABLE :: PARTS

        TYPE (H3D_OUTPUT), DIMENSION(:), ALLOCATABLE :: OUTPUT_LIST

      END TYPE H3D_DATABASE
      
C-----------------------------------------------
      TYPE H3D_NUMBER_OF_KEY
        INTEGER :: NODAL_SCALAR =0
        INTEGER ::  NODAL_VECTOR =0
        INTEGER ::  NODAL_TENSOR =0
        INTEGER ::  ONED_SCALAR =0
        INTEGER ::  ONED_VECTOR =0
        INTEGER ::  ONED_TENSOR =0
        INTEGER ::  ONED_TORSOR =0
        INTEGER ::  SHELL_SCALAR =0
        INTEGER ::  SHELL_VECTOR =0
        INTEGER ::  SHELL_TENSOR =0
        INTEGER ::  SOLID_SCALAR =0
        INTEGER ::  SOLID_VECTOR =0
        INTEGER ::  SOLID_TENSOR =0
        INTEGER ::  SPH_SCALAR =0
        INTEGER ::  SPH_TENSOR =0
        INTEGER ::  QUAD_SCALAR =0
        INTEGER ::  QUAD_VECTOR =0
        INTEGER ::  QUAD_TENSOR =0
      END TYPE H3D_NUMBER_OF_KEY
C-----------------------------------------------
      
      END MODULE H3D_MOD
