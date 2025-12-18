Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    resol                                        ../engine/source/engine/resol.F
!||--- called by ------------------------------------------------------
!||    resol_head                                   ../engine/source/engine/resol_head.F
!||--- calls      -----------------------------------------------------
!||    accdtdc                                      ../engine/source/elements/thickshell/solidec/accdtdc.F
!||    accel1                                       ../engine/source/tools/accele/accel1.F
!||    accele                                       ../engine/source/assembly/accele.F
!||    accelepinch                                  ../engine/source/assembly/accelepinch.F
!||    add_elapsed_time_mon_off                     ../engine/source/system/timer.F
!||    admdiv                                       ../engine/source/model/remesh/admdiv.F
!||    admerr                                       ../engine/source/model/remesh/admerr.F
!||    admfor0                                      ../engine/source/model/remesh/admfor0.F
!||    admgvid                                      ../engine/source/model/remesh/admgvid.F
!||    admordr                                      ../engine/source/model/remesh/admordr.F
!||    admregul                                     ../engine/source/model/remesh/admregul.F
!||    admvit                                       ../engine/source/model/remesh/admvit.F
!||    agauge0                                      ../engine/source/ale/agauge.F
!||    alefvm_accele                                ../engine/source/ale/alefvm/alefvm_accele.F
!||    alefvm_main                                  ../engine/source/ale/alefvm/alefvm_main.F
!||    alemain                                      ../engine/source/ale/alemain.F
!||    alemuscl_deallocate                          ../engine/source/ale/alemuscl/alemuscl_deallocate.F
!||    alesub2                                      ../engine/source/ale/subcycling/alesub2.F
!||    alewdx                                       ../engine/source/ale/grid/alewdx.F
!||    alloc_elbuf_imp                              ../engine/source/elements/elbuf/alloc_elbuf_imp.F
!||    allocate_sph_work                            ../common_source/modules/mat_elem/sph_work.F90
!||    ams_prepare_poff_assembly                    ../engine/source/mpi/ams/spmd_sms.F
!||    ancmsg                                       ../engine/source/output/message/message.F
!||    arret                                        ../engine/source/system/arret.F
!||    assign_ptrx                                  ../common_source/modules/nodal_arrays.F90
!||    asspar                                       ../engine/source/assembly/asspar.F
!||    asspar3                                      ../engine/source/assembly/asspar3.F
!||    asspar4                                      ../engine/source/assembly/asspar4.F
!||    asspar5                                      ../engine/source/assembly/asspar5.F
!||    asspar_crk                                   ../engine/source/elements/xfem/asspar_crk.F
!||    asspar_sub                                   ../engine/source/assembly/asspar_sub.F
!||    asspar_sub_poff                              ../engine/source/assembly/asspar_sub_poff.F
!||    asspart                                      ../engine/source/assembly/asspart.F
!||    assparxx                                     ../engine/source/assembly/assparxx.F
!||    bcs10                                        ../engine/source/constraints/general/bcs/bcs10.F
!||    bcs_nrf                                      ../engine/source/boundary_conditions/bcs_nrf.F90
!||    bcscyc                                       ../engine/source/constraints/general/bcs/bcscyc.F
!||    bcsdtth_copy                                 ../engine/source/constraints/general/bcs/bcsdtth.F
!||    bcsn                                         ../engine/source/constraints/general/bcs/bcsn.F
!||    bmultn                                       ../engine/source/ale/bimat/bmultn.F
!||    cfield_1                                     ../engine/source/loads/general/load_centri/cfield.F
!||    check_ale_comm                               ../engine/source/ale/check_ale_comm.F
!||    check_edge_state                             ../engine/source/interfaces/interf/check_edge_state.F
!||    check_nan_acc                                ../engine/source/output/outfile/check_nan_acc.F
!||    check_nodal_state                            ../engine/source/interfaces/interf/check_nodal_state.F
!||    check_remote_surface_state                   ../engine/source/interfaces/interf/check_remote_surface_state.F
!||    check_surface_state                          ../engine/source/interfaces/interf/check_surface_state.F
!||    chkload                                      ../engine/source/interfaces/chkload.F
!||    chkmsin                                      ../engine/source/output/outfile/chkmsin.F
!||    chkstfn3n                                    ../engine/source/interfaces/interf/chkstfn3.F
!||    chkstifn                                     ../engine/source/interfaces/inter2d/chkstifn.F
!||    cjoint                                       ../engine/source/constraints/general/cyl_joint/cjoint.F
!||    close_sock_c                                 ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    clusterf                                     ../engine/source/output/cluster/clusterf.F
!||    cnd_dmasi2                                   ../engine/source/elements/solid/solide10/s10cndf.F
!||    cndint                                       ../engine/source/model/remesh/cndint.F
!||    cndordr                                      ../engine/source/model/remesh/cndordr.F
!||    collect                                      ../engine/source/output/outfile/collect.F
!||    collectm                                     ../engine/source/output/outfile/collect.F
!||    collectt                                     ../engine/source/output/outfile/collect.F
!||    convec                                       ../engine/source/constraints/thermic/convec.F
!||    coupling_advance                             ../engine/source/coupling/coupling_adapter.F90
!||    coupling_finalize                            ../engine/source/coupling/coupling_adapter.F90
!||    coupling_initialize                          ../engine/source/coupling/coupling_adapter.F90
!||    coupling_ongoing                             ../engine/source/coupling/coupling_adapter.F90
!||    coupling_set_interface                       ../engine/source/coupling/coupling_adapter.F90
!||    coupling_sync                                ../engine/source/coupling/coupling_adapter.F90
!||    cp_dm                                        ../engine/source/implicit/produt_v.F
!||    crk_accele                                   ../engine/source/elements/xfem/accele_crk.F
!||    crk_coord_ini                                ../engine/source/elements/xfem/crk_coord_ini.F
!||    crk_velocity                                 ../engine/source/elements/xfem/crk_velocity.F
!||    crk_velocity2                                ../engine/source/elements/xfem/crk_velocity2.F
!||    crk_zero_accel                               ../engine/source/elements/xfem/crk_velocity.F
!||    crk_zero_fsky                                ../engine/source/elements/xfem/crk_velocity.F
!||    cumultime_mp                                 ../engine/source/system/timer.F
!||    damping                                      ../engine/source/assembly/damping.F
!||    damping44                                    ../engine/source/assembly/damping.F
!||    damping51                                    ../engine/source/assembly/damping.F
!||    damping_funct_ini                            ../engine/source/assembly/damping_funct_ini.F90
!||    damping_vref_compute_dampa                   ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    dealloc_impbuf                               ../engine/source/implicit/imp_solv.F
!||    dealloc_shoot_inter                          ../engine/source/interfaces/interf/dealloc_shoot_inter.F
!||    deallocate_joint                             ../engine/source/constraints/general/cyl_joint/deallocate_joint.F
!||    deallocm_imp                                 ../engine/source/implicit/imp_solv.F
!||    depla                                        ../engine/source/assembly/displacement.F
!||    deplafakeige                                 ../engine/source/assembly/displfakeige.F
!||    deplapinch                                   ../engine/source/assembly/displpinch.F
!||    desacti                                      ../engine/source/elements/desacti.F
!||    disp_vel_saved_cload                         ../engine/source/assembly/disp_vel_saved_cload.F
!||    dtnoda                                       ../engine/source/time_step/dtnoda.F
!||    dtnodamp                                     ../engine/source/time_step/dtnodamp.F
!||    dtnodams                                     ../engine/source/time_step/dtnodams.F
!||    dtnodarayl                                   ../engine/source/time_step/dtnodarayl.F
!||    ebcclap                                      ../engine/source/boundary_conditions/ebcs/ebcclap.F
!||    ebcs_extrapol                                ../engine/source/boundary_conditions/ebcs/ebcs_extrapol.F
!||    elapstime                                    ../engine/source/system/timer.F
!||    ener_w0                                      ../engine/source/general_controls/damping/static.F
!||    err_thk                                      ../engine/source/elements/shell/err_thk.F
!||    euldx                                        ../engine/source/ale/grid/euldx.F
!||    fail_wind_frwave_init                        ../engine/source/materials/fail/alter/fail_wind_frwave_init.F
!||    fequilibre                                   ../engine/source/elements/shell/fequilibre.F
!||    find_dt_for_targeted_added_mass              ../engine/source/time_step/find_dt_for_targeted_added_mass.F
!||    find_edge_inter                              ../engine/source/interfaces/interf/find_edge_inter.F
!||    find_surface_inter                           ../engine/source/interfaces/interf/find_surface_inter.F
!||    fixfingeo                                    ../engine/source/constraints/general/impvel/fixfingeo.F
!||    fixflux                                      ../engine/source/constraints/thermic/fixflux.F
!||    fixtemp                                      ../engine/source/constraints/thermic/fixtemp.F
!||    fixvel                                       ../engine/source/constraints/general/impvel/fixvel.F
!||    flow0                                        ../engine/source/fluid/flow0.F
!||    flow1                                        ../engine/source/fluid/flow1.F
!||    flow_accele                                  ../engine/source/assembly/flow_accele.F
!||    flow_depla                                   ../engine/source/assembly/flow_displ.F
!||    flow_velocity                                ../engine/source/assembly/flow_velocity.F
!||    forani1                                      ../engine/source/output/anim/generate/forani1.F
!||    forani2                                      ../engine/source/output/anim/generate/forani2.F
!||    forani3                                      ../engine/source/output/anim/generate/forani3.F
!||    force                                        ../engine/source/loads/general/force.F90
!||    forcefingeo                                  ../engine/source/loads/general/forcefingeo.F
!||    forcepinch                                   ../engine/source/loads/general/forcepinch.F
!||    forint                                       ../engine/source/elements/forint.F
!||    forintc                                      ../engine/source/elements/forintc.F
!||    forintp                                      ../engine/source/elements/forintp.F
!||    forints                                      ../engine/source/elements/forints.F
!||    free_pinch                                   ../common_source/modules/pinchtype_mod.F
!||    funct_python_update_elements                 ../engine/source/tools/curve/funct_python_update_elements.F90
!||    fv_switch_crit                               ../engine/source/airbag/fvdim.F
!||    fvbag0                                       ../engine/source/airbag/fvbag0.F
!||    fvcopy                                       ../engine/source/airbag/fvcopy.F
!||    fvdim                                        ../engine/source/airbag/fvdim.F
!||    fvmesh0                                      ../engine/source/airbag/fvmesh0.F
!||    fvrezone0                                    ../engine/source/airbag/fvrezone.F
!||    fvstats                                      ../engine/source/airbag/fvstats.F
!||    fvupd0                                       ../engine/source/airbag/fvupd.F
!||    fxbyfor                                      ../engine/source/constraints/fxbody/fxbyfor.F
!||    fxbypid                                      ../engine/source/constraints/fxbody/fxbypid.F
!||    fxbyvit                                      ../engine/source/constraints/fxbody/fxbyvit.F
!||    fxgrvcor                                     ../engine/source/constraints/fxbody/fxgrvcor.F
!||    get_fbuf_c                                   ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    get_fbufdp_c                                 ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    get_ibuf_c                                   ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    get_neighbour_surface                        ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_shmbuf_c                                 ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    gravit                                       ../engine/source/loads/general/grav/gravit.F
!||    gravit_fvm_fem                               ../engine/source/loads/general/grav/gravit_fvm_fem.F
!||    i14ist                                       ../engine/source/interfaces/int14/i14ist.F
!||    i14wfs                                       ../engine/source/interfaces/int14/i14wfs.F
!||    i18main_kine_1                               ../engine/source/interfaces/int18/i18main_kine.F
!||    i18main_kine_2                               ../engine/source/interfaces/int18/i18main_kine.F
!||    i24e2e_fictive_nodes_update                  ../engine/source/interfaces/int24/i24for3e.F
!||    i24nitschfor3                                ../engine/source/interfaces/int24/i24nitschfor3.F
!||    i24pxfem                                     ../engine/source/interfaces/int24/i24pxfem.F
!||    imp_buck                                     ../engine/source/implicit/imp_buck.F
!||    imp_chkm                                     ../engine/source/implicit/imp_solv.F
!||    imp_dt2                                      ../engine/source/implicit/imp_dt.F
!||    imp_fanie                                    ../engine/source/implicit/imp_solv.F
!||    imp_fanii                                    ../engine/source/implicit/imp_solv.F
!||    imp_fout                                     ../engine/source/implicit/imp_solv.F
!||    imp_restarcp                                 ../engine/source/implicit/imp_sol_init.F
!||    imp_sol_init                                 ../engine/source/implicit/imp_sol_init.F
!||    imp_solv                                     ../engine/source/implicit/imp_solv.F
!||    init_ale                                     ../engine/source/ale/init_ale.F90
!||    init_ghost_shells                            ../engine/source/engine/node_spliting/ghost_shells.F90
!||    init_global_frontier_monvol                  ../engine/source/airbag/init_global_monvol_frontier.F90
!||    init_i25_edge                                ../engine/source/interfaces/int25/init_i25_edge.F
!||    init_interf_sorting_strategy                 ../engine/source/interfaces/init_interf_sorting_strategy.F
!||    init_monvol_omp_structure                    ../engine/source/airbag/init_monvol_omp_structure.F90
!||    init_nodal_state                             ../engine/source/interfaces/interf/init_nodal_state.F
!||    init_trim                                    ../engine/source/interfaces/intsort/init_trim.F
!||    inivel_dt2                                   ../engine/source/loads/general/inivel/inivel_dt2.F90
!||    inivel_start                                 ../engine/source/loads/general/inivel/inivel_start.F90
!||    inixfem                                      ../engine/source/elements/xfem/inixfem.F
!||    int18_alloc                                  ../engine/source/interfaces/int18/int18_alloc.F
!||    int18_law151_init                            ../engine/source/interfaces/int18/int18_law151_init.F
!||    int18_law151_omp_accumulation                ../engine/source/interfaces/int18/int18_law151_omp_accumulation.F
!||    int18_law151_update                          ../engine/source/interfaces/int18/int18_law151_update.F
!||    intal1                                       ../engine/source/ale/inter/intal1.F
!||    inter_init_component                         ../engine/source/interfaces/generic/inter_init_component.F90
!||    inter_nodal_areas                            ../engine/source/output/inter_nodal_areas.F
!||    inter_struct_init                            ../engine/source/interfaces/generic/inter_struct_init.F
!||    intfop1                                      ../engine/source/interfaces/interf/intfop1.F
!||    intfop2                                      ../engine/source/interfaces/interf/intfop2.F
!||    intfop8                                      ../engine/source/interfaces/interf/intfop8.F
!||    intstamp_ass                                 ../engine/source/interfaces/int21/intstamp_ass.F
!||    intstamp_dt                                  ../engine/source/interfaces/int21/intstamp_dt.F
!||    intstamp_init                                ../engine/source/interfaces/int21/intstamp_init.F
!||    intstamp_move                                ../engine/source/interfaces/int21/intstamp_move.F
!||    intti1                                       ../engine/source/interfaces/interf/intti1.F
!||    intti2                                       ../engine/source/interfaces/interf/intti2.F
!||    inttri                                       ../engine/source/interfaces/intsort/inttri.F
!||    joint_block_stiffness                        ../engine/source/elements/joint/joint_block_stiffness.F
!||    joint_elem_timestep                          ../engine/source/elements/joint/joint_elem_timestep.F
!||    kine_seatbelt_force                          ../engine/source/tools/seatbelts/kine_seatbelt_force.F
!||    kine_seatbelt_vel                            ../engine/source/tools/seatbelts/kine_seatbelt_vel.F
!||    lag_mult                                     ../engine/source/tools/lagmul/lag_mult.F
!||    lag_multp                                    ../engine/source/tools/lagmul/lag_mult.F
!||    load_pressure                                ../engine/source/loads/general/load_pressure/load_pressure.F
!||    manctr                                       ../engine/source/input/manctr.F
!||    modsti                                       ../engine/source/time_step/modsti.F
!||    monvol0                                      ../engine/source/airbag/monvol0.F
!||    movfra1                                      ../engine/source/tools/skew/movfram.F
!||    movfra2                                      ../engine/source/tools/skew/movfram.F
!||    movfra_imp                                   ../engine/source/tools/skew/movfram.F
!||    mpi_min_real_begin                           ../engine/share/modules/mpi_tools_mod.F
!||    mpi_min_real_end                             ../engine/share/modules/mpi_tools_mod.F
!||    multi_deallocate                             ../engine/source/multifluid/multi_deallocate.F
!||    multi_velocity_backup                        ../engine/source/multifluid/multi_velocity_backup.F
!||    mvoludt                                      ../engine/source/airbag/mvoludt.F
!||    my_barrier                                   ../engine/source/system/machine.F
!||    newskw                                       ../engine/source/tools/skew/newskw.F
!||    newskw_init                                  ../engine/source/tools/skew/newskw.F
!||    nlocal_acc                                   ../engine/source/assembly/nlocal_acc.F
!||    nlocal_dtnoda                                ../engine/source/time_step/nlocal_dtnoda.F
!||    nlocal_incr                                  ../engine/source/assembly/nlocal_incr.F
!||    nlocal_vel                                   ../engine/source/assembly/nlocal_vel.F
!||    offset_nproj                                 ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||    omp_get_thread_num                           ../engine/source/engine/openmp_stub.F90
!||    omp_set_num_threads                          ../engine/source/engine/openmp_stub.F90
!||    output_allocate_noda_pext                    ../common_source/modules/output/output_mod.F90
!||    output_deallocate_noda_pext                  ../common_source/modules/output/output_mod.F90
!||    pblast_deallocate                            ../common_source/modules/loads/pblast_mod.F90
!||    pblast_load_computation                      ../engine/source/loads/pblast/pblast.F
!||    pfluid                                       ../engine/source/loads/general/pfluid/pfluid.F
!||    ply_accele                                   ../engine/source/assembly/ply_accele.F
!||    ply_bcs                                      ../engine/source/constraints/general/bcs/ply_bcs.F
!||    ply_velocity                                 ../engine/source/assembly/ply_velocity.F
!||    poro                                         ../engine/source/ale/porous/poro.F
!||    prerbe3p0                                    ../engine/source/constraints/general/rbe3/rbe3f.F
!||    pressure_cyl                                 ../engine/source/loads/general/load_pcyl/pressure_cyl.F
!||    printime                                     ../engine/source/system/timer.F
!||    printime_interf                              ../engine/source/system/timer_interf.F
!||    printimeg                                    ../engine/source/system/timer.F
!||    python_begin_openmp                          ../common_source/modules/python_mod.F90
!||    python_end_openmp                            ../common_source/modules/python_mod.F90
!||    python_monvol                                ../engine/source/coupling/python/python_monvol.F90
!||    python_update_nodal_entities                 ../common_source/modules/python_mod.F90
!||    r2r_block_c                                  ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    r2r_exchange                                 ../engine/source/coupling/rad2rad/r2r_exchange.F
!||    r2r_getdata                                  ../engine/source/coupling/rad2rad/r2r_getdata.F
!||    r2r_sem_c                                    ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    r2r_sendkine                                 ../engine/source/coupling/rad2rad/r2r_getdata.F
!||    r2r_unlock_threads_c                         ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    radiation                                    ../engine/source/constraints/thermic/radiation.F
!||    radiossviper_receiveaccelerations            ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_receivesenddt                   ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendkill                        ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendxve                         ../engine/source/coupling/viper/viper_interface_mod.F90
!||    rbagdt                                       ../engine/source/airbag/rbagdt.F
!||    rbe2cor                                      ../engine/source/constraints/general/rbody/rgbcor.F
!||    rbe2t1                                       ../engine/source/constraints/general/rbe2/rbe2f.F
!||    rbe2v                                        ../engine/source/constraints/general/rbe2/rbe2v.F
!||    rbe3t1                                       ../engine/source/constraints/general/rbe3/rbe3f.F
!||    rbe3v                                        ../engine/source/constraints/general/rbe3/rbe3v.F
!||    rbycor                                       ../engine/source/constraints/general/rbody/rbycor.F
!||    rbyfor                                       ../engine/source/constraints/general/rbody/rbyfor.F
!||    rbysens                                      ../engine/source/constraints/general/rbody/rbyonf.F
!||    rbyvit                                       ../engine/source/constraints/general/rbody/rbyvit.F
!||    re2int5                                      ../engine/source/implicit/imp_solv.F
!||    re2int7                                      ../engine/source/implicit/imp_solv.F
!||    reaction_forces_1                            ../engine/source/output/reactions.F
!||    reaction_forces_2                            ../engine/source/output/reactions.F
!||    reaction_forces_3                            ../engine/source/output/reactions.F
!||    reaction_forces_check_for_requested_output   ../engine/source/output/reactions.F
!||    reaction_forces_th                           ../engine/source/output/reaction_forces_th.F
!||    reallocate_i_skyline                         ../engine/source/system/reallocate_skyline.F
!||    renum_siz                                    ../engine/source/interfaces/interf/renum_siz.F
!||    resol_alloc_nitsche                          ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase1                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase10                          ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase11                          ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase2                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase3                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase4                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase5                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase6                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase7                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase8                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase9                           ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_python                           ../engine/source/engine/resol_alloc.F90
!||    resol_init                                   ../engine/source/engine/resol_init.F
!||    rgwal0                                       ../engine/source/constraints/general/rwall/rgwal0.F
!||    rgwal0_pen                                   ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    rgwalf                                       ../engine/source/constraints/general/rwall/rgwal0.F
!||    rivet1                                       ../engine/source/elements/rivet/rivet1.F
!||    rlink10                                      ../engine/source/constraints/general/rlink/rlink10.F
!||    rlink11                                      ../engine/source/constraints/general/rlink/rlink10.F
!||    rmatacce                                     ../engine/source/materials/mat/mat013/rmatacce.F
!||    rmatforp                                     ../engine/source/materials/mat/mat013/rmatforp.F
!||    s10cndf1                                     ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cndf2                                     ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cndfnd                                    ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cndi2a                                    ../engine/source/elements/solid/solide10/s10cndv.F
!||    s10cndi2a1                                   ../engine/source/elements/solid/solide10/s10cndv.F
!||    s10cndv                                      ../engine/source/elements/solid/solide10/s10cndv.F
!||    s10getvdm                                    ../engine/source/elements/solid/solide10/s10cndv.F
!||    s10stfe_poff                                 ../engine/source/elements/solid/solide10/s10cndf.F
!||    s4lagsfem                                    ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!||    sav_buf_point                                ../engine/source/user_interface/eng_callback_c.c
!||    section_fio                                  ../engine/source/tools/sect/section_fio.F
!||    section_io                                   ../engine/source/tools/sect/section_io.F
!||    send_fbuf_c                                  ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    send_fbufdp_c                                ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    send_ibuf_c                                  ../engine/source/coupling/rad2rad/rad2rad_c.c
!||    sensor_base                                  ../engine/source/tools/sensor/sensor_base.F
!||    sensor_dist_surf0                            ../engine/source/tools/sensor/sensor_dist_surf0.F
!||    sensor_ener_sav                              ../engine/source/tools/sensor/sensor_ener_sav.F
!||    sensor_init                                  ../engine/source/tools/sensor/sensor_init.F
!||    sensor_logical                               ../engine/source/tools/sensor/sensor_logical.F
!||    sensor_spmd                                  ../engine/source/tools/sensor/sensor_spmd.F
!||    sensor_temp0                                 ../engine/source/tools/sensor/sensor_temp0.F
!||    smp_init                                     ../engine/source/engine/resol_init.F
!||    sms_build_mat_2                              ../engine/source/ams/sms_build_mat_2.F
!||    sms_encin_2                                  ../engine/source/ams/sms_encin_2.F
!||    sms_ini_err                                  ../engine/source/ams/sms_init.F
!||    sms_ini_int                                  ../engine/source/ams/sms_init.F
!||    sms_ini_jad_1                                ../engine/source/ams/sms_init.F
!||    sms_ini_jad_2                                ../engine/source/ams/sms_init.F
!||    sms_ini_jad_3                                ../engine/source/ams/sms_init.F
!||    sms_ini_kad                                  ../engine/source/ams/sms_init.F
!||    sms_ini_kdi                                  ../engine/source/ams/sms_init.F
!||    sms_ini_kin_2                                ../engine/source/ams/sms_init.F
!||    sms_ini_part                                 ../engine/source/ams/sms_init.F
!||    sms_ini_rby                                  ../engine/source/ams/sms_init.F
!||    sms_mass_scale_2                             ../engine/source/ams/sms_mass_scale_2.F
!||    soltospha                                    ../engine/source/elements/sph/soltospha.F
!||    soltosphf                                    ../engine/source/elements/sph/soltosph.F
!||    sortie_error                                 ../engine/source/output/sortie_error.F
!||    sortie_main                                  ../engine/source/output/sortie_main.F
!||    spgauge_f                                    ../engine/source/elements/sph/spgauge.F
!||    sphprep                                      ../engine/source/elements/sph/sphprep.F
!||    sphres44b                                    ../engine/source/elements/sph/sphres44b.F
!||    splissv                                      ../engine/source/elements/sph/splissv.F
!||    split_asspar4                                ../engine/source/assembly/split_asspar4.F
!||    spmd_allglob_isum9                           ../engine/source/mpi/generic/spmd_allglob_isum9.F
!||    spmd_barrier                                 ../engine/source/mpi/spmd_mod.F90
!||    spmd_chkw                                    ../engine/source/mpi/generic/spmd_chkw.F
!||    spmd_collect                                 ../engine/source/mpi/output/spmd_collect.F
!||    spmd_collect_multi_fvm                       ../engine/source/mpi/output/spmd_collect_multi_fvm.F
!||    spmd_collect_nlocal                          ../engine/source/mpi/output/spmd_collect_nlocal.F
!||    spmd_collectm                                ../engine/source/mpi/output/spmd_collectm.F
!||    spmd_collectt                                ../engine/source/mpi/output/spmd_collectt.F
!||    spmd_crk_adv                                 ../engine/source/elements/xfem/xfemfsky.F
!||    spmd_exch2_a_pon                             ../engine/source/mpi/forces/spmd_exch2_a_pon.F
!||    spmd_exch_a                                  ../engine/source/mpi/forces/spmd_exch_a.F
!||    spmd_exch_a_ams_poff                         ../engine/source/mpi/forces/spmd_exch_a_ams_poff.F
!||    spmd_exch_crkvel                             ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_da20                               ../engine/source/mpi/interfaces/spmd_exch_da20.F
!||    spmd_exch_deleted_surf_edge                  ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
!||    spmd_exch_efric                              ../engine/source/mpi/interfaces/spmd_exch_efric.F
!||    spmd_exch_fa                                 ../engine/source/mpi/elements/spmd_exch_dttsh.F
!||    spmd_exch_i24                                ../engine/source/mpi/interfaces/spmd_exch_i24.F
!||    spmd_exch_i25                                ../engine/source/mpi/interfaces/spmd_exch_i25.F
!||    spmd_exch_icodt                              ../engine/source/mpi/nodes/spmd_exch_icodt.F
!||    spmd_exch_icont                              ../engine/source/mpi/nodes/spmd_exch_icont.F
!||    spmd_exch_inter_18                           ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
!||    spmd_exch_press                              ../engine/source/mpi/interfaces/spmd_exch_press.F
!||    spmd_exch_r2r_sph                            ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_sphoff                         ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_sec                                ../engine/source/mpi/sections/spmd_section.F
!||    spmd_exch_sub_poff                           ../engine/source/mpi/spmd_exch_sub.F
!||    spmd_exch_sub_pon                            ../engine/source/mpi/spmd_exch_sub.F
!||    spmd_exch_tagncont                           ../engine/source/mpi/forces/spmd_exch_tagncont.F
!||    spmd_exch_thknod                             ../engine/source/mpi/nodes/spmd_exch_thknod.F
!||    spmd_exch_vmax                               ../engine/source/mpi/elements/spmd_exch_dttsh.F
!||    spmd_exch_wave                               ../engine/source/mpi/nodes/spmd_exch_wave.F
!||    spmd_exsum_fb6                               ../engine/source/mpi/generic/spmd_exsum_fb6.F
!||    spmd_flush_accel                             ../engine/source/mpi/output/spmd_flush_accel.F
!||    spmd_fvb_switch                              ../engine/source/mpi/airbags/spmd_fvb_switch.F
!||    spmd_glob_isum9                              ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_glob_min5                               ../engine/source/mpi/generic/spmd_glob_min5.F
!||    spmd_glob_minv                               ../engine/source/mpi/generic/spmd_glob_minv.F
!||    spmd_i21fthecom                              ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_i21tempcom                              ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_i24_prepare                             ../engine/source/interfaces/int24/i24_prepare.F
!||    spmd_i25_prepare                             ../engine/source/interfaces/int25/i25_prepare.F
!||    spmd_i25front_init                           ../engine/source/mpi/interfaces/spmd_i25front.F
!||    spmd_i7fcom_poff                             ../engine/source/mpi/forces/spmd_i7fcom_poff.F
!||    spmd_i7fcom_pon                              ../engine/source/mpi/forces/spmd_i7fcom_pon.F
!||    spmd_i7xvcom2                                ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
!||    spmd_ibcast                                  ../engine/source/mpi/generic/spmd_ibcast.F
!||    spmd_int18_law151_pon                        ../engine/source/mpi/forces/spmd_int18_law151_pon.F
!||    spmd_max_xfe_i                               ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_mumps_ini                               ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_nlist_sms                               ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_rbcast                                  ../engine/source/mpi/generic/spmd_rbcast.F
!||    spmd_sd_xv                                   ../engine/source/mpi/nodes/spmd_sd_xv.F
!||    spmd_sort_sms                                ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_wiout                                   ../engine/source/mpi/generic/spmd_wiout.F
!||    spmd_xv_inter_type1                          ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||    sponfv                                       ../engine/source/elements/sph/sponfv.F
!||    spwfvis                                      ../engine/source/elements/sph/spwfvis.F
!||    srfvit                                       ../engine/source/constraints/general/rbody/srfvit.F
!||    startime                                     ../engine/source/system/timer_mod.F90
!||    static                                       ../engine/source/general_controls/damping/static.F
!||    stop_sensor                                  ../engine/source/tools/sensor/stop_sensor.F
!||    stoptime                                     ../engine/source/system/timer_mod.F90
!||    tagoff3n                                     ../engine/source/interfaces/interf/chkstfn3.F
!||    tempur                                       ../engine/source/constraints/thermic/tempur.F
!||    th_time_output                               ../engine/source/output/th/th_time_output.F
!||    thbcs                                        ../engine/source/output/th/thbcs.F
!||    thbcs_imp                                    ../engine/source/output/th/thbcs_imp.F
!||    thermbilan                                   ../engine/source/constraints/thermic/thermbilan.F
!||    thickvar                                     ../engine/source/elements/shell/coque/thickvar.F
!||    timfun                                       ../engine/source/tools/curve/timfun.F
!||    trace_in                                     ../engine/source/system/trace_back.F
!||    trace_out                                    ../engine/source/system/trace_back.F
!||    upd_tmax                                     ../engine/source/output/upd_outmax.F
!||    update_failwave                              ../engine/source/materials/fail/failwave/update_failwave.F
!||    update_slipring                              ../engine/source/tools/seatbelts/update_slipring.F
!||    upenr_crk                                    ../engine/source/elements/xfem/upenr_crk.F
!||    upgrade_rem_2ry                              ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    upxfem1                                      ../engine/source/elements/xfem/upxfem1.F
!||    upxfem2                                      ../engine/source/elements/xfem/upxfem2.F
!||    upxfem_tagxp                                 ../engine/source/elements/xfem/upxfem_tagxp.F
!||    user_windows_routine                         ../engine/source/user_interface/user_windows.F
!||    velocity                                     ../engine/source/assembly/velocity.F
!||    velocitypinch                                ../engine/source/assembly/velocitypinch.F
!||    viper_coupling_initialize                    ../engine/source/coupling/viper/viper_interface_mod.F90
!||    wrrestp                                      ../engine/source/output/restart/wrrestp.F
!||    xfeoff                                       ../engine/source/elements/xfem/xfeoff.F
!||    zero1                                        ../engine/source/system/zero.F
!||    zeror                                        ../engine/source/system/zero.F
!||--- uses       -----------------------------------------------------
!||    acceleration_dp_mod                          ../engine/share/modules/acceleration_dp_mod.F
!||    ale_connectivity_mod                         ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                                      ../common_source/modules/ale/ale_mod.F
!||    aleanim_mod                                  ../common_source/modules/aleanim_mod.F
!||    aleflow_mod                                  ../engine/share/modules/aleflow_mod.F
!||    alefvm_mod                                   ../common_source/modules/ale/alefvm_mod.F
!||    alemuscl_mod                                 ../common_source/modules/ale/alemuscl_mod.F
!||    ams_work_mod                                 ../engine/source/modules/ams_work_mod.F90
!||    array_mod                                    ../common_source/modules/array_mod.F
!||    bcs_mod                                      ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    bcs_nrf_mod                                  ../engine/source/boundary_conditions/bcs_nrf.F90
!||    check_mod                                    ../common_source/modules/check_mod.F
!||    cluster_mod                                  ../engine/share/modules/cluster_mod.F
!||    connectivity_mod                             ../common_source/modules/connectivity.F90
!||    coupling_adapter_mod                         ../engine/source/coupling/coupling_adapter.F90
!||    crackxfem_mod                                ../engine/share/modules/crackxfem_mod.F
!||    damping_funct_ini_mod                        ../engine/source/assembly/damping_funct_ini.F90
!||    damping_vref_compute_dampa_mod               ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    debug_chksm_mod                              ../engine/source/mpi/output/spmd_flush_accel.F
!||    debug_mod                                    ../engine/share/modules/debug_mod.F
!||    detach_node_mod                              ../engine/source/engine/node_spliting/detach_node.F90
!||    diffusion_mod                                ../engine/share/modules/diffusion_mod.F
!||    drape_mod                                    ../engine/share/modules/drape_mod.F
!||    dsgraph_mod                                  ../engine/share/modules/dsgraph_mod.F
!||    dt_mod                                       ../engine/source/modules/dt_mod.F
!||    dtdc_mod                                     ../engine/share/modules/dtdc_mod.F
!||    dynlib_mod                                   ../engine/share/modules/dynlib_mod.F
!||    ebcs_mod                                     ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    ecnd_mod                                     ../engine/share/modules/ecdn_mod.F
!||    elbufdef_mod                                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                                  ../common_source/modules/elements/element_mod.F90
!||    error_mod                                    ../engine/share/modules/error_mod.F
!||    failwave_mod                                 ../common_source/modules/failwave_mod.F
!||    force_mod                                    ../engine/source/loads/general/force.F90
!||    funct_python_update_elements_mod             ../engine/source/tools/curve/funct_python_update_elements.F90
!||    fvbag_mod                                    ../engine/share/modules/fvbag_mod.F
!||    get_neighbour_surface_mod                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    ghost_shells_mod                             ../engine/source/engine/node_spliting/ghost_shells.F90
!||    glob_therm_mod                               ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    groupdef_mod                                 ../common_source/modules/groupdef_mod.F
!||    h3d_mod                                      ../engine/share/modules/h3d_mod.F
!||    heat_mod                                     ../engine/share/modules/heat_mod.F
!||    i22bufbric_mod                               ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22edge_mod                                  ../common_source/modules/interfaces/cut-cell-buffer_mod.F
!||    i22tri_mod                                   ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    icontact_mod                                 ../engine/share/modules/icontact_mod.F
!||    imp_intbuf                                   ../engine/share/modules/imp_mod_def.F90
!||    impbufdef_mod                                ../engine/share/modules/impbufdef_mod.F
!||    init_ale_mod                                 ../engine/source/ale/init_ale.F90
!||    init_global_frontier_monvol_mod              ../engine/source/airbag/init_global_monvol_frontier.F90
!||    init_monvol_omp_structure_mod                ../engine/source/airbag/init_monvol_omp_structure.F90
!||    inivel_dt2_mod                               ../engine/source/loads/general/inivel/inivel_dt2.F90
!||    inivel_start_mod                             ../engine/source/loads/general/inivel/inivel_start.F90
!||    intbufdef_mod                                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_init_component_mod                     ../engine/source/interfaces/generic/inter_init_component.F90
!||    inter_sh_offset_mod                          ../engine/source/modules/interfaces/sh_offset_mod.F90
!||    inter_sorting_mod                            ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod                             ../engine/share/modules/inter_struct_mod.F
!||    interface_table_mod                          ../engine/share/modules/table_mod.F
!||    interfaces_mod                               ../common_source/modules/interfaces/interfaces_mod.F90
!||    intstamp_glob_mod                            ../engine/share/modules/intstamp_glob_mod.F
!||    loads_mod                                    ../common_source/modules/loads/loads_mod.F90
!||    mat_elem_mod                                 ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mds_restart_mod                              ../common_source/modules/mds_rest.mod.F
!||    message_mod                                  ../engine/share/message_module/message_mod.F
!||    monvol_struct_mod                            ../engine/share/modules/monvol_struct_mod.F
!||    mpi_tools_mod                                ../engine/share/modules/mpi_tools_mod.F
!||    multi_fvm_mod                                ../common_source/modules/ale/multi_fvm_mod.F90
!||    multimat_param_mod                           ../common_source/modules/multimat_param_mod.F90
!||    my_alloc_mod                                 ../common_source/tools/memory/my_alloc.F90
!||    names_and_titles_mod                         ../common_source/modules/names_and_titles_mod.F
!||    nlocal_reg_mod                               ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod                             ../common_source/modules/nodal_arrays.F90
!||    offset_nproj_mod                             ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||    outmax_mod                                   ../common_source/modules/outmax_mod.F
!||    output_mod                                   ../common_source/modules/output/output_mod.F90
!||    pblast_mod                                   ../common_source/modules/loads/pblast_mod.F90
!||    pinchtype_mod                                ../common_source/modules/pinchtype_mod.F
!||    plyxfem_mod                                  ../engine/share/modules/plyxfem_mod.F
!||    python_call_funct_cload_mod                  ../engine/source/loads/general/python_call_funct_cload.F90
!||    python_funct_mod                             ../common_source/modules/python_mod.F90
!||    python_monvol_mod                            ../engine/source/coupling/python/python_monvol.F90
!||    python_register_mod                          ../engine/source/tools/curve/python_register.F90
!||    python_share_memory_mod                      ../engine/source/coupling/python/python_share_memory.F90
!||    rad2r_mod                                    ../engine/share/modules/rad2r.F
!||    rbe3_mod                                     ../common_source/modules/constraints/rbe3_mod.F90
!||    remesh_mod                                   ../engine/share/modules/remesh_mod.F
!||    resol_alloc_mod                              ../engine/source/engine/resol_alloc.F90
!||    resolsav_mod                                 ../engine/share/modules/resol_mod.F
!||    rigmat_mod                                   ../engine/share/modules/rigmat_mod.F
!||    rwall_mod                                    ../common_source/modules/constraints/rwall_mod.F90
!||    rwall_pen_mod                                ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    seatbelt_mod                                 ../common_source/modules/seatbelt_mod.F
!||    segvar_mod                                   ../engine/share/modules/segvar_mod.F
!||    sensor_mod                                   ../common_source/modules/sensor_mod.F90
!||    shooting_node_mod                            ../engine/share/modules/shooting_node_mod.F90
!||    skew_mod                                     ../common_source/modules/skew_mod.F90
!||    sms_mod                                      ../engine/share/modules/sms_mod.F
!||    sms_pcg_proj                                 ../engine/share/modules/sms_mod.F
!||    sph_work_mod                                 ../common_source/modules/mat_elem/sph_work.F90
!||    spmd_mod                                     ../engine/source/mpi/spmd_mod.F90
!||    spmd_xv_inter_type1_mod                      ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||    stack_mod                                    ../engine/share/modules/stack_mod.F
!||    state_mod                                    ../common_source/modules/state_mod.F
!||    table_mod                                    ../engine/share/modules/table_mod.F
!||    th_surf_mod                                  ../common_source/modules/interfaces/th_surf_mod.F
!||    thgrelem_mod                                 ../engine/share/modules/thgrelem_mod.F
!||    thk_mod                                      ../engine/share/modules/thk_mod.F
!||    threac_mod                                   ../engine/share/modules/threac_mod.F
!||    time_mod                                     ../engine/share/modules/time_mod.F
!||    timer_mod                                    ../engine/source/system/timer_mod.F90
!||    unitab_mod                                   ../common_source/modules/unitab_mod.F
!||    update_pon_mod                               ../engine/source/engine/node_spliting/update_pon.F90
!||    user_windows_mod                             ../common_source/modules/user_windows_mod.F
!||    viper_mod                                    ../engine/source/coupling/viper/viper_interface_mod.F90
!||    xfem2vars_mod                                ../engine/share/modules/xfem2vars_mod.F
!||====================================================================
            SUBROUTINE RESOL(TIMERS, ELEMENT, NODES, coupling, AF,IAF,
     1        ISKWN   ,NETH   ,IPART  ,NOM_OPT,KXX    ,IXX    ,
     2        IXTG   ,IXS     ,IXQ    ,IXT    ,IXP    ,IXR    ,
     3        IFILL  ,MAT_ELEM,
     4        IMS    ,NPC    ,IBCL    ,
     5        IBFV   ,IDUM   ,LAS     ,LACCELM,NNLINK ,LNLINK ,
     6        IPARG  ,DD_IAD  ,IGRV   ,IEXLNK ,
     7        IPARI  ,ICONX   ,NPBY   ,
     8        LPBY   ,LRIVET ,NSTRF   ,LJOINT ,NODPOR ,MONVOL ,
     9        ILINK  ,
     A        LLINK  ,LINALE ,NEFLSW  ,NNFLSW ,ICUT   ,CLUSTER,
     B                ITASK  ,INOISE  ,
     C        THKE   ,DAMP      ,
     D        PM     ,SKEWS    ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     E        W      ,VEUL   ,FILL    ,DFILL  ,ALPH   ,WB     ,DSAVE  ,ASAVE  ,
     .        MSNF   ,
     F        TF     ,FORC   ,VEL     ,FSAV   ,FZERO  ,XLAS   ,ACCELM ,
     G        AGRV   ,FR_WAVE,FAILWAVE,PARTS0 ,ELBUF  ,SENSORS,
     H        RBY    ,RIVET  ,SECBUF ,VOLMON ,LAMBDA ,
     I        WA     ,FV     ,PARTSAV
     J               ,UWA    ,VAL2    ,PHI    ,SEGVAR ,R      ,CRFLSW ,
     K        FLSW   ,XCUT    ,TANI   ,SECFCUM,BUFNOIS,
     L        IDATA  ,RDATA   ,
     M        IFRAME ,KXSP   ,IXSP    ,NOD2SP ,ISPSYM ,ISPCOND,
     N        XFRAME ,SPBUF  ,XSPSYM  ,VSPSYM ,PV     ,
     O        FSAVD  ,IBVEL  ,LBVEL   ,WASPH  ,W16    ,
     P        ISPHIO ,LPRTSPH,LONFSPH ,VSPHIO ,FBVEL  ,LAGBUF ,IBCSLAG,
     Q        IACTIV ,DAMPR  ,GJBUFI  ,GJBUFR , RBMPC , IBMPC ,SPHVELN,
     T        NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,NERCVOIS,NESDVOIS,LERCVOIS,
     U        LESDVOIS,NPSEGCOM,LSEGCOM ,NPORGEO ,
     V        IXTG1   ,NPBYL   ,LPBYL   ,RBYL    ,IGEO    ,IPM     ,
     W        MADPRT  ,MADSH4  ,MADSH3  ,MADSOL  ,MADNOD  ,MADFAIL ,
     X        IAD_RBY ,FR_RBY  ,IAD_RBY2,
     Y        FR_RBY2 ,IAD_I2M ,FR_I2M  ,ADDCNI2 ,PROCNI2 ,IADI2   ,FR_MV   ,
     Z        IADMV2  ,FR_LL   ,FR_RL   ,IADCJ   ,
     a        FR_CJ   ,FR_SEC  ,
     b        IAD_SEC ,IAD_CUT ,FR_CUT  ,RG_CUT  ,NEWFRONT,FR_MAD  ,
     c        FXBIPM  ,FXBRPM  ,FXBNOD  ,FXBMOD  ,FXBGLM  ,FXBCPM  ,FXBCPS  ,
     d        FXBLM   ,FXBFLS  ,FXBDLS  ,FXBDEP  ,FXBVIT  ,FXBACC  ,FXBELM  ,
     e        FXBSIG  ,FXBGRVI ,FXBGRVR ,EIGIPM  ,EIGIBUF ,EIGRPM  ,
     f        LNODPOR ,
     g        FR_I18  ,GRAPHE  ,IFLOW   ,RFLOW   ,
     h        LGRAV   ,DD_R2R  ,FASOLFR ,FR_LAGF ,
     j        LLAGF   ,ICONTACT,RCONTACT,SH4TREE ,SH3TREE ,
     k        IPADMESH,PADMESH ,MSC     ,MSTG    ,INC     ,INTG    ,PTG     ,
     l        ISKWP   ,NSKWP   ,ISENSP  ,NSENSP  ,IACCP   ,NACCP,IPART_STATE,
     m        ACONTACT,PCONTACT,FACTIV
     p                         ,SH4TRIM ,SH3TRIM ,MSCND   ,INCND   ,
     q        IBFFLUX ,FBFFLUX ,RBYM    ,IRBYM   ,LNRBYM  ,ICODRBYM,
     r        IBCV    ,FCONV   ,IBFTEMP ,FBFTEMP ,IAD_RBYM,FR_RBYM,
     t        WEIGHT_RM,MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,IADC_PXFEM,
     u        ADSKY_PXFEM,ICODE_PLY,ICODT_PLY,ISKEW_PLY  ,ADMSMS  ,
     v        MADCLNOD,NOM_SECT,MCPC    ,MCPTG   ,DMELC   ,DMELTG  ,MSSA    ,
     w        DMELS   ,MSTR    ,DMELTR  ,MSP     ,DMELP   ,MSRT    ,DMELRT  ,
     x        IBCR    ,FRADIA  ,RES_SMS ,TABLE   ,IRBE2   ,LRBE2  ,IAD_RBE2 ,
     y        FR_RBE2 ,PHIE    ,MSF     ,
     z        PROCNE_PXFEM  ,IADSDP_PXFEM,IADRCP_PXFEM,ICFIELD,LCFIELD,CFIELD,
     1        MSZ2 ,DIAG_SMS,ILOADP  ,LLOADP  ,LOADP   ,
     2        INOD_CRK,IEL_CRK ,IADC_CRK,ADSKY_CRK,
     3        CNE_CRK,PROCNE_CRK,IADSDP_CRK,IADRCP_CRK, IBUFSSG_IO,
     4        IBC_PLY ,DMINT2  ,IBORDNODE,
     5        ELBUF_TAB,POR    ,NODEDGE  ,IAD_EDGE,
     6        FR_EDGE  ,FR_NBEDGE,CRKNODIAD,LGAUGE ,GAUGE  ,
     7        IGAUP    ,NGAUP  ,NODLEVXF,DD_R2R_ELEM,
     8        NODGLOBXFE,SPH2SOL ,SOL2SPH ,IRST ,
     9        DMSPH     ,WAGAP   ,XFEM_TAB,ELCUTC  ,NODENR  ,
     A        KXFENOD2ELC,ENRTAG,
     B        RTHBU    F,KXIG3D ,IXIG3D ,KNOT, WIGE,
     C        WSMCOMP,STACK,
     D        CPUTIME_MP_GLOB, CPUTIME_MP ,TAB_UMP,POIN_UMP ,SOL2SPH_TYP,
     E        IRUNN_BIS,ADDCSRECT, IAD_FRNOR,FR_NOR  ,PROCNOR,
     F        IAD_FREDG,FR_EDG   ,DRAPE_SH4N  , DRAPE_SH3N ,TAB_MAT,
     G        NATIV0_SMS,MULTI_FVM,SEGQUADFR,MS_2D  ,
     H        H3D_DATA   ,SUBSETS,IGRNOD,IGRBRIC,
     I        IGRQUAD,IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,
     J        IGRSPRING,IGRPART,IGRSURF,FORNEQS,
     K        NLOC_DMG ,ISKWP_L,KNOTLOCPC,KNOTLOCEL,PINCH_DATA,TAG_SKINS6, ALE_CONNECTIVITY,
     L        XCELL, XFACE, NE_NERCVOIS, NE_NESDVOIS, NE_LERCVOIS, NE_LESDVOIS,IBCSCYC  ,LBCSCYC,
     M        T_MONVOL,ID_GLOBAL_VOIS,FACE_VOIS,DYNAIN_DATA,FCONT_MAX,EBCS_TAB,DIFFUSION,
     N        KLOADPINTER,LOADPINTER,DGAPLOADINT,DRAPEG,USER_WINDOWS,OUTPUT,INTERFACES,
     O        DT        ,LOADS     , PYTHON, DPL0CLD,VEL0CLD,
     P        NDAMP_VREL,ID_DAMP_VREL,FR_DAMP_VREL,NDAMP_VREL_RBYG,NAMES_AND_TITLES,UNITAB,LIFLOW,LRFLOW ,
     R        GLOB_THERM ,PBLAST,RBE3,RWALL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
              USE GHOST_SHELLS_MOD
              USE CONNECTIVITY_MOD
              USE NODAL_ARRAYS_MOD
              USE detach_node_mod
              USE DSGRAPH_MOD
              USE ERROR_MOD
              USE RESOLSAV_MOD
              USE ICONTACT_MOD
              USE REMESH_MOD
              USE HEAT_MOD
              USE SMS_MOD
              USE SMS_PCG_PROJ
              USE THK_MOD
              USE INTSTAMP_GLOB_MOD
              USE RIGMAT_MOD
              USE PLYXFEM_MOD
              USE THREAC_MOD
              USE THGRELEM_MOD
              USE TABLE_MOD
              USE INTERFACE_TABLE_MOD
              USE ALEFLOW_MOD
              USE RAD2R_MOD
              USE IMP_INTBUF
              USE MESSAGE_MOD
              USE CRACKXFEM_MOD
              USE I22BUFBRIC_MOD
              USE i22EDGE_MOD
              USE CLUSTER_MOD
              USE INTBUFDEF_MOD
              USE XFEM2VARS_MOD
              USE STACK_MOD
              USE ALEFVM_MOD
              USE I22TRI_MOD
              USE FVBAG_MOD
              USE MPI_TOOLS_MOD
              USE ALEMUSCL_MOD
              USE ECND_MOD
              USE MULTI_FVM_MOD
              USE H3D_MOD
              USE PBLAST_MOD
              USE GROUPDEF_MOD
              USE FAILWAVE_MOD
              USE NLOCAL_REG_MOD
              USE MAT_ELEM_MOD
              USE TIME_MOD
              USE PINCHTYPE_MOD
              USE CHECK_MOD
              USE ACCELERATION_DP_MOD
              USE ALE_CONNECTIVITY_MOD
              USE ALEANIM_MOD
              USE DYNLIB_MOD
              USE MONVOL_STRUCT_MOD
              USE DRAPE_MOD
              USE SENSOR_MOD
              USE MDS_RESTART_MOD
              USE EBCS_MOD
              USE SEATBELT_MOD
              USE DIFFUSION_MOD
              USE SEGVAR_MOD
              USE DTDC_MOD
              USE IMPBUFDEF_MOD
              USE INTER_STRUCT_MOD
              USE INTER_SORTING_MOD
              USE SHOOTING_NODE_MOD
              USE LOADS_MOD
              USE STATE_MOD
              USE USER_WINDOWS_MOD
              USE ALE_MOD
              USE OUTPUT_MOD
              USE INTERFACES_MOD
              USE DEBUG_CHKSM_MOD ,  ONLY : SPMD_FLUSH_ACCEL
              USE DT_MOD
              USE PYTHON_FUNCT_MOD
              USE PYTHON_SHARE_MEMORY_MOD
              USE PYTHON_REGISTER_MOD, ONLY : PYTHON_REGISTER
              USE FUNCT_PYTHON_UPDATE_ELEMENTS_MOD, ONLY : FUNCT_PYTHON_UPDATE_ELEMENTS
              USE python_call_funct_cload_mod
              USE python_monvol_mod , ONLY : python_monvol
              USE OUTMAX_MOD
              USE FORCE_MOD , ONLY : FORCE
              USE ARRAY_MOD , ONLY : array_type
              USE NAMES_AND_TITLES_MOD
              USE UNITAB_MOD, ONLY : UNIT_TYPE_
              USE BCS_MOD , ONLY : BCS
              USE INTER_SH_OFFSET_MOD , only:SH_OFFSET_
              USE OFFSET_NPROJ_MOD, only : OFFSET_NPROJ
              USE get_neighbour_surface_mod , only : get_neighbour_surface
              USE spmd_mod , only : SPMD_MAX,spmd_allreduce,SPMD_BARRIER
              USE SKEW_MOD
              USE ELBUFDEF_MOD
              USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
              use init_global_frontier_monvol_mod , only : init_global_frontier_monvol
              use init_monvol_omp_structure_mod , only : init_monvol_omp_structure
              USE inivel_dt2_mod ,   only : inivel_dt2
              USE inivel_start_mod , only : inivel_start
              use glob_therm_mod
              USE MY_ALLOC_MOD
              USE SPH_WORK_MOD
              USE spmd_xv_inter_type1_mod, only : spmd_xv_inter_type1
              USE TIMER_MOD
              USE rbe3_mod
              USE TH_SURF_MOD , ONLY : TH_SURF_NUM_CHANNEL
              USE ams_work_mod
              USE UPDATE_PON_MOD
              USE DEBUG_MOD
              use inter_init_component_mod , only : inter_init_component
              use damping_vref_compute_dampa_mod
              use coupling_adapter_mod
              use damping_funct_ini_mod    , only : damping_funct_ini
              USE VIPER_MOD
              USE BCS_NRF_MOD , ONLY : BCS_NRF
              USE rwall_mod
              USE rwall_pen_mod, only : rgwal0_pen
              use element_mod , only : nixs,nixq,nixc,nixt,nixp,nixr,nixtg
              use resol_alloc_mod
              use init_ale_mod , only : init_ale
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "macro.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "intstamp_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "scr23_c.inc"
#include      "units_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "cong2_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "couple_c.inc"
#include      "rad2r_c.inc"
#include      "chara_c.inc"
#include      "lagmult.inc"
#include      "warn_c.inc"
#include      "impl1_c.inc"
#include      "fxbcom.inc"
#include      "eigcom.inc"
#include      "spmd_c.inc"
#include      "remesh_c.inc"
#include      "com_xfem1.inc"
#include      "tabsiz_c.inc"
#include      "sms_c.inc"
#include      "filescount_c.inc"
#include      "inter22.inc"
#include      "userlib.inc"
#include      "drape_c.inc"
C-----------------------------------------------
              COMMON /VGLOB/DMAS,DINER
              my_real DMAS,DINER
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
              INTEGER ITASK
              INTEGER  ISKWN(LISKN,*), NETH(*),
     .           IPART(*),NOM_OPT(LNOPT1,*),IXS(*),
     .           IXQ(NIXQ,*), IXT(NIXT,*), IXP(NIXP,*),
     .           IXR(NIXR,*),IXTG(NIXTG,*), IXTG1(4,*),
     .           IFILL(NUMNOD,*), IMS(*), NPC(*), IBCL(*), IBFV(*),
     .           IDUM(*), LAS(*),IPARG(NPARG,*),
     .           ICONX(*), NPBY(NNPBY,*),
     .           LPBY(*),
     .           LRIVET(*), NSTRF(*), LJOINT(*),  ILINK(*),
     .           LLINK(*), LINALE(*), NEFLSW(*), NNFLSW(*),
     .           NODPOR(*),ICUT(*) , INOISE(*),MONVOL(*),
     .           LACCELM(3,*),DD_IAD(NSPMD+1,*),
     .           IAD_RBY(*),FR_RBY(*),NNLINK(10,*),LNLINK(*),
     .           IAF(*),IGRV(*),
     .           KXX(NIXX,*),IXX(*),IEXLNK(NR2R,*),
     .           IFRAME(LISKN,*),KXSP(NISP,*),IXSP(*),NOD2SP(*),
     .           ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),IBVEL(NBVELP,*),LBVEL(*),
     .           ISPHIO(NISPHIO,*),LPRTSPH(*),LONFSPH(*),LAGBUF(*),IBCSLAG(*),
     .           IACTIV(*),GJBUFI(LKJNI,*) ,IBMPC(*),NPBYL(NNPBY,*), LPBYL(*),
     .           NBRCVOIS(*),NBSDVOIS(*),LNRCVOIS(*),LNSDVOIS(*),
     .           NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*), NPORGEO(*),
     .           NPSEGCOM(*), LSEGCOM(*), IGEO(NPROPGI,*),
     .           IPM(NPROPMI,*),MADPRT(*), MADSH4(*), MADSH3(*), MADSOL(*),
     .           MADNOD(*),MADFAIL(*),
     .           IAD_RBY2(*),
     .           FR_RBY2(*),IAD_I2M(*),FR_I2M(*),ADDCNI2(*),PROCNI2(*),IADI2(*),
     .           FR_MV(*), IADMV2(*),  FR_LL(*),  FR_RL(*),
     .           IADCJ(*), FR_CJ(*),
     .           FR_SEC(*), IAD_SEC(4,*),
     .           IAD_CUT(NSPMD+2,*), FR_CUT(*), RG_CUT(*), NEWFRONT(*),
     .           FR_MAD(5,*), LNODPOR(*), FR_I18(*),
     .           FXBIPM(NBIPM,*),FXBNOD(*),FXBELM(*), FXBGRVI(*),
     .           EIGIPM(NEIPM,*), EIGIBUF(*), IFLOW(*), FASOLFR(*),
     .           DD_R2R(NSPMD+1,*), LGRAV(*), FR_LAGF(3,*), LLAGF(*),
     .           ICONTACT(*), SH4TREE(*), SH3TREE(*), IPADMESH(*),
     .           ISKWP(*), NSKWP(*), ISENSP(2,*), NSENSP(*), IACCP(*), NACCP(*),
     .           IPART_STATE(*),SH4TRIM(*), SH3TRIM(*),IRBYM(*) ,LNRBYM(*),
     .           ICODRBYM(*),IAD_RBYM(*),
     .           FR_RBYM(*),NOM_SECT(*), IBCR(*),IRBE2(*),LRBE2(*),
     .           IAD_RBE2(*),FR_RBE2(*),IADSDP_PXFEM(*),
     .           IADRCP_PXFEM(*),ICFIELD(*),LCFIELD(*),ILOADP(*),LLOADP(*),
     .           IADSDP_CRK(*),IADRCP_CRK(*),INOD_CRK(*),IEL_CRK(*),IADC_CRK(*),
     .           ADSKY_CRK(*),PROCNE_CRK(*),CNE_CRK(*),IBORDNODE(*),
     .           NODEDGE(*),IAD_EDGE(*),FR_EDGE(*),FR_NBEDGE(*),
     .           CRKNODIAD(*), LGAUGE(3,*), IGAUP(*), NGAUP(*),NODLEVXF(*),
     .           NODGLOBXFE(*),ELCUTC(2,*),NODENR(*),KXFENOD2ELC(*),
     .           ENRTAG(NUMNOD,*),KXIG3D(*),IXIG3D(*),
     .           NATIV0_SMS(*), SEGQUADFR(*),
     .           KLOADPINTER(NINTER+1)  ,LOADPINTER(NINTER*NLOADP_HYD)
              INTEGER IDATA(*), IBFTEMP(*), IBCV(*), IBFFLUX(*), WEIGHT_RM(*),
     .                ICODT_PLY(*), ISKEW_PLY(*),INOD_PXFEM(*),IEL_PXFEM(*),
     .                IADC_PXFEM(*),ADSKY_PXFEM(*),ICODE_PLY(*),MADCLNOD(*),
     .                PROCNE_PXFEM(*),
     .                IBUFSSG_IO(*),IBC_PLY(*),DD_R2R_ELEM(*),
     .                SPH2SOL(*), SOL2SPH(2,*), IRST(3,*), SOL2SPH_TYP(*),IRUNN_BIS,
     .                ADDCSRECT(*), IAD_FRNOR(*), FR_NOR(*), PROCNOR(*),
     .                IAD_FREDG(*), FR_EDG(*),TAG_SKINS6(*),IBCSCYC(*),LBCSCYC(*)
              integer ::  IPARI(NPARI,ninter)
              INTEGER, DIMENSION(NUMSKW), INTENT(IN) :: ISKWP_L
              INTEGER, DIMENSION(*), INTENT(in) :: ID_GLOBAL_VOIS,FACE_VOIS
              my_real
     .           DAMP(*),
     .           PM(NPROPM,*),GEO(NPROPG,*),
     .           BUFMAT(*) ,W(3,*)    ,VEUL(*),FILL(NUMNOD,*),DFILL(NUMNOD,*),
     .           ALPH(*)   ,WB(3,*)   ,TF(*)       ,FORC(*)  ,VEL(*),
     .           FSAV(NTHVKI,*) ,FZERO(3,*),XLAS(*)     ,ELBUF(*) ,
     .           RBY(NRBY,*),RIVET(*),WA(*),
     .           FV(*)      ,VAL2(*)  ,PHI(*),
     .           R(3,*)    ,CRFLSW(*),FLSW(*),
     .           UWA(*)    ,PARTSAV(*)   ,
     .           DSAVE(3,*),ASAVE(3,*),XCUT(*)   ,BUFNOIS(*),
     .           ACCELM(LLACCELM,*),
     .           TANI(*),VOLMON(*),EANI(*),AGRV(*), THKE(*), BUFSF(*),AF(*),
     .           SECBUF(*),SECFCUM(7,NUMNOD,NSECT),LAMBDA(*),
     .           FR_WAVE(*),PARTS0(*),BUFGEO(*),
     .           SPBUF(NSPBUF,*),XFRAME(NXFRAME,*),
     .           WASPH(*),W16(*),VSPHIO(*),FBVEL(*),DAMPR(NRDAMP,*),
     .           RDATA(*),PV(*),FSAVD(NTHVKI,*),GJBUFR(LKJNR,*),RBMPC(*),
     .           SPHVELN(*),RBYL(NRBY,*), MSNF(*),
     .           FXBRPM(*), FXBMOD(*), FXBGLM(*), FXBCPM(*), FXBCPS(*),
     .           FXBLM(*),  FXBFLS(*), FXBDLS(*), FXBDEP(*), FXBVIT(*),
     .           FXBACC(*), FXBSIG(*), FXBGRVR(*), EIGRPM(NERPM,*),
     .           DMSPH(*),KNOT(*),WIGE(*),MS_2D(*),
     .           KNOTLOCPC(*),KNOTLOCEL(*),XCELL(*),XFACE(*),FCONT_MAX(3,*)
              my_real
     .           RFLOW(*), RCONTACT(*),
     .           PADMESH(*), MSC(*), MSTG(*), INC(*) , INTG(*), PTG(3,*),
     .           ACONTACT(*), PCONTACT(*), FACTIV(*),
     .           MSCND(*), INCND(*), RBYM(*), FBFFLUX(*),
     .           FCONV(*), FBFTEMP(*), MS_PLY(*), ZI_PLY(*),  ADMSMS(*),
     .           MCPC(*), MCPTG(*), DMELC(*), DMELTG(*), MSSA(*), DMELS(*),
     .           MSTR(*), DMELTR(*), MSP(*), DMELP(*), MSRT(*), DMELRT(*),
     .           FRADIA(*), RES_SMS(*), PHIE(*),MSF(*),
     .           CFIELD(*),MSZ2(*), DIAG_SMS(*),LOADP(*), DMINT2(*),POR(*),
     .           GAUGE(LLGAUGE,*),WAGAP(2,*),RTHBUF(*),FORNEQS(3,*),
     .           DPL0CLD(*),VEL0CLD(*)
              my_real , INTENT(IN) :: DGAPLOADINT(NINTER*NLOADP_HYD)
              TYPE(timer_) :: TIMERS
              TYPE(connectivity_), INTENT(INOUT) :: ELEMENT
              TYPE(NODAL_ARRAYS_), INTENT(INOUT) :: NODES
              TYPE (CLUSTER_) ,DIMENSION(*) :: CLUSTER
              TYPE(PRGRAPH) :: GRAPHE(*)
              TYPE(TTABLE) :: TABLE(*)
              TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP) :: ELBUF_TAB
              TYPE(ELBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: ELBUF_IMP
              TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP,NXEL) :: XFEM_TAB
              TYPE SPSYM_STRUCT
                my_real, DIMENSION(:)  , POINTER :: BUF
              END TYPE SPSYM_STRUCT
              TYPE (SPSYM_STRUCT) :: XSPSYM,VSPSYM,WSMCOMP
              TYPE (STACK_PLY) :: STACK
              TYPE (MPI_MIN_REAL_STRUCT) :: MPI_BUF
              TYPE(UNIT_TYPE_) :: UNITAB !structure containing units conversion ratios
              INTEGER MIN_TAB(4)
              my_real :: DT2R
              TYPE(coupling_type), intent(inout) :: coupling 
C
C     Mat + Prop timers
              INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
              my_real, DIMENSION(NBR_GPMP,NSPMD+1) ::  CPUTIME_MP_GLOB
              my_real, DIMENSION(TAILLE) ::  CPUTIME_MP
              INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
              my_real TAB_MAT(NGROUP)
              TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
              TYPE(H3D_DATABASE) :: H3D_DATA
              TYPE (FAILWAVE_STR_) :: FAILWAVE
!
              TYPE(SUBSET_) ,DIMENSION(NSUBS)   :: SUBSETS
              TYPE(GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
              TYPE(GROUP_)  ,DIMENSION(NGRBRIC) :: IGRBRIC
              TYPE(GROUP_)  ,DIMENSION(NGRQUAD) :: IGRQUAD
              TYPE(GROUP_)  ,DIMENSION(NGRSHEL) :: IGRSH4N
              TYPE(GROUP_)  ,DIMENSION(NGRSH3N) :: IGRSH3N
              TYPE(GROUP_)  ,DIMENSION(NGRTRUS) :: IGRTRUSS
              TYPE(GROUP_)  ,DIMENSION(NGRBEAM) :: IGRBEAM
              TYPE(GROUP_)  ,DIMENSION(NGRSPRI) :: IGRSPRING
              TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
              TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
c
              TYPE(NLOCAL_STR_)  :: NLOC_DMG
              TYPE(PINCH) :: PINCH_DATA
              TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
              INTEGER, INTENT(IN) :: NE_NERCVOIS(*), NE_NESDVOIS(*), NE_LERCVOIS(*), NE_LESDVOIS(*)
              TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
              TYPE (DRAPE_)                                      :: DRAPE_SH4N(NUMELC_DRAPE)  , DRAPE_SH3N(NUMELTG_DRAPE)
              TYPE(DRAPEG_)                                      :: DRAPEG
              TYPE (SENSORS_)   ,INTENT(INOUT)                   :: SENSORS
              TYPE(t_ebcs_tab)  ,INTENT(INOUT)                   :: EBCS_TAB
              TYPE(T_DIFFUSION) ,INTENT(INOUT)                   :: DIFFUSION
              TYPE(t_segvar)                                     :: SEGVAR
              TYPE(inter_struct_type), DIMENSION(:), ALLOCATABLE :: INTER_STRUCT   !   structure for interface
              TYPE(sorting_comm_type), DIMENSION(:), ALLOCATABLE :: SORT_COMM   ! structure for interface sorting comm
              TYPE (LOADS_) ,INTENT(INOUT) :: LOADS
              TYPE (DYNAIN_DATABASE), INTENT(INOUT)              :: DYNAIN_DATA
              TYPE(USER_WINDOWS_),INTENT(INOUT)                  :: USER_WINDOWS
              TYPE(OUTPUT_),INTENT(INOUT)                        :: OUTPUT
              TYPE (INTERFACES_) ,INTENT(INOUT)                  :: INTERFACES
              TYPE (DT_) ,INTENT(INOUT)                          :: DT
              TYPE(MAT_ELEM_)   ,INTENT(INOUT)                   :: MAT_ELEM
              TYPE(PYTHON_)  ,INTENT(INOUT)                      :: PYTHON
              TYPE(NAMES_AND_TITLES_),INTENT(INOUT)              :: NAMES_AND_TITLES !< NAMES_AND_TITLES host the input deck names and titles for outputs
              type (glob_therm_) ,intent(inout)                  :: glob_therm
              type (PBLAST_) ,intent(inout)                      :: PBLAST
              type (rbe3_) ,intent(inout)                        :: RBE3
              type (rwall_) ,intent(inout)                       :: RWALL
C
              INTEGER ,INTENT(IN) :: NDAMP_VREL,NDAMP_VREL_RBYG
              INTEGER ,INTENT(IN) :: ID_DAMP_VREL(NDAMP_VREL),FR_DAMP_VREL(NSPMD+2,NDAMP_VREL)
              TYPE(SKEW_),INTENT(INOUT) :: SKEWS
              INTEGER, INTENT(IN) :: LIFLOW  !< Size of IFLOW
              INTEGER, INTENT(IN) :: LRFLOW  !< Size of RFLOW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
              LOGICAL LOUT
              CHARACTER FILNAM*100
              INTEGER NODFT, NODLT,  I,J, N,
     .           K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K, ISK, KK1,
     .           N0, N1, N2, NN,  NNOD, NSENSOR,
     .           ISYNC, TWO_INTS(2),
     .           NELTST,ITYPTST,NWAFT, NBNCL, NBIKL,
     .           NBNODL, NBNODLR,GREFTSK,GRELTSK,
     .           ISTOP,  NFIA, NFEA, NFOA, NDMA, NDIN,
     .           NFNCA, NFTCA,NDMA2, NFNCA2, NFTCA2,
     .           L1,L2,L3,LL1,LL2,LL3,NFT2,LISENDP_PXFEM,
     .           LIRECVP_PXFEM,NDAMA2,FLG_KJ2,NCONT,BID,K12,IVAD,IAD_GREL,
     .           FLG_KJ2_RAZ,I_EXCH_FLG_RAZ
              INTEGER :: LISENDP_CRK,LIRECVP_CRK
              INTEGER :: I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I13I
              INTEGER :: I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K
              INTEGER :: I15ATH,I35ATH
              INTEGER :: I87A,I87B,I87C,I87D,I87E,I87F,I87G,I87H,I87I,I87J,I87K,
     .                I87L,I87M,I87N,IMSCH,IAD1B,IAD1,IAD2,I2MSCH, ISMSCH, ONOFP
              INTEGER CPLXIT,  ADRNOD, ONOF, ONFELT, ICH,IFLGADM, MADENDREQUEST
              CHARACTER*13 CDUM
              INTEGER IDUM1, DIM6, DIM_EXCH

              my_real
     .                RDUM1,MADUF,RBUF(10)
              INTEGER LLT1,IWIOUT,IWIOUT_RESULT
              INTEGER KSPH1,KSPH21,KSPH22,KSPH23,
     .                KSPACTIV,KSP2SORT,NELTSA,ITYPTSA,IDTNOD
              INTEGER NEGMAS
              INTEGER :: LENGTH
              INTEGER NBINTC, LENR, LENS, LENI, SIZI, ISIZXV ,ILENXV,
     .                I2SIZE, LSEND1, LRECV1, LSEND2, LRECV2, NPARTL,
     .                ISLEN7, IRLEN7, ISLEN11, IRLEN11, LISENDP, LIRECVP,
     .                ISLEN17, IRLEN17,IRLEN7T,ISLEN7T,LINDIDEL,LBUFIDEL,LBUFSEGLO,
     .                ISLEN20, IRLEN20, ISLEN20T, IRLEN20T, NBINT20,
     .                ISLEN20E, IRLEN20E,LAG_SEC,LENS1,LENR1,INT18KINE,
     .                NRBYKIN_L, INT24USE, NELEML,I24MAXNSNE,INT24E2EUSE,NHIER_RBY
              INTEGER :: INT7ITIED
              INTEGER, DIMENSION(:,:),ALLOCATABLE ::
     .                ISENDTO,IRCVFROM,FR_NBCC,FR_NBCCI2,FR_NBCC1
              INTEGER, DIMENSION(:),ALLOCATABLE ::
     .                INTLIST,NISKYFI,ISENDP,IRECVP, IRBKIN_L,
     .                IPARTL, NISKYFIE, TAGEL,
     .                ISENDP_PXFEM,IRECVP_PXFEM,COUNT_REMSLV,COUNT_REMSLVE,
     .                INTLIST25
              INTEGER, DIMENSION(:), ALLOCATABLE, TARGET :: CNEL,ADDCNEL,ADDTMPL
              INTEGER, DIMENSION(:),ALLOCATABLE ::
     .                ISENDP_CRK,IRECVP_CRK
              INTEGER NSPHACTG,SPH_IORD1
              INTEGER, DIMENSION(1), TARGET :: IMP_DUM
C----
              my_real,DIMENSION(:),ALLOCATABLE :: DRETRI, XSEC,QFRICINT,ICODR0,ICODT0,
     .                                            STIFN_TMP,STIFR_TMP
              SAVE ISENDTO, IRCVFROM, INTLIST, FR_NBCC, NISKYFI, FR_NBCCI2,
     .             NBINTC, I2SIZE, ISLEN7   ,IRLEN7   ,ISLEN11  ,IRLEN11,
     .             ILENXV, ISIZXV, ISLEN17, IRLEN17,IRLEN7T,ISLEN7T,
     .             ISLEN20, IRLEN20, ISLEN20T, IRLEN20T, NBINT20, NISKYFIE,
     .             ISLEN20E, IRLEN20E, NRBYKIN_L, IRBKIN_L, QFRICINT,
     .             INTLIST25,NHIER_RBY
              SAVE DRETRI, XSEC, LISENDP, LIRECVP, ISENDP, IRECVP,
     .             CNEL, ADDCNEL, ADDTMPL, IPARTL, NPARTL, LINDIDEL, LBUFIDEL,
     .             LBUFSEGLO
              my_real DT2T,
     .                DT2SAVE,  MAS, BB, WFEXC,TREST,DTREST,
     .                DMAST, DINERT, FACTB,DAMPT,
     .                XSENS(12,SENSORS%NSENSOR),DAMPA3
              my_real FXBMVN(LENCP),FXBSV(LENLM),FXBSE(15*NFXBODY),
     .                FXBMCD(LENMCD),FXBFP(LENVAR),FXBFC(LENLM),FXBEFW(NFXBODY),
     .                FXBEDP(NFXBODY),FXBGRP(LENVAR),FXBGRW(NFXBODY)
              my_real, DIMENSION(:,:), ALLOCATABLE :: DXANCG
              my_real DT2PREV, DTMIN1_SAVE, TARGET_DT
              SAVE IMSCH, I2MSCH, ISMSCH
              SAVE DT2PREV
              SAVE NFIA, NFEA, NFOA, NDMA, NDIN, NDMA2,
     .             I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I13I,
     .             I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K,
     .             I87A,I87B,I87C,I87D,I87E,I87F,I87G,I87H,I87I,I87J,I87K,I87L,
     .             I87M,I87N,I15ATH,I35ATH,LAG_SEC,NFT2,NDAMA2
              INTEGER NDDL0,NNZK0,NUM_IMP(NINTER),NINT7,NT_IMP,
     .                NNDL,IT,NMC2,LI13,DIRUL(NFXVEL),NUM_IMP1(NINTER),
     .                FR_RBE2M(SFR_RBE2),R2SIZE,
     .                NUM_IMPL(NINTER,NTHREAD), I_OPT_STOK(NINTER),IT_T,NTMP
              INTEGER IER1,NDS,IBUCK,NMRBE2,NTHOLD
              SAVE NMRBE2,R2SIZE,NINT7
              INTEGER, DIMENSION(:),POINTER :: NS_IMP,NE_IMP,IND_IMP
              TYPE (IMPBUF_STRUCT_), TARGET :: IMPBUF_TAB
              my_real, DIMENSION(:)  , POINTER :: FEXT_IMP,R_IMP
              my_real TTMP,DMCP(NUMGEO)
              data TTMP /0.0/
              INTEGER LWIBEM, LWRBEM, IERROR, IERROR2, INTER_ERRORS
              INTEGER, DIMENSION(:), ALLOCATABLE :: WIBEM
              my_real, DIMENSION(:), ALLOCATABLE :: WRBEM
              SAVE LWIBEM, LWRBEM, WIBEM, WRBEM
              INTEGER NKCOND, NDDLG, NRP, NCP, NKCOND_INI, NT_IMP_OLD
              INTEGER LWIFLOW, LWRFLOW
              INTEGER, DIMENSION(:), ALLOCATABLE :: WIFLOW
              my_real, DIMENSION(:), ALLOCATABLE :: WRFLOW
              SAVE LWIFLOW, LWRFLOW, WIFLOW, WRFLOW
              INTEGER SIZ, R2R_ON
              INTEGER, DIMENSION(:), ALLOCATABLE :: CDDLP
#if defined(MUMPS5)
              TYPE(DMUMPS_STRUC) MUMPS_PAR
#endif
              INTEGER IFVMESH
C
              my_real, DIMENSION(:,:), ALLOCATABLE :: MADCLFRECV,PARTSAV2
C
              INTEGER MADYMO_DEL,MADYMO_DEL_GLOBAL
              INTEGER, DIMENSION(:),  ALLOCATABLE ::MAD_TAG_SOL, MAD_TAG_SH,MAD_TAG_TG,MAD_FAIL_ELEMENTS
C
C OpenMP specific
              INTEGER ITSK, NODFTSK, NODLTSK, NUMNTSK, NDTSK, NDTSKR, IPMTSK,
     .                PARTFTSK, PARTLTSK, NWAFTSK, I16TSK,
     .                NELTSTT, ITYPTSTT,IGMTSK,NGROUC, NGROUNC,
     .                NSNFIOLD(NSPMD),
     .                KINDRBY(NRBYKIN), NINDEX1(NINTER), NINDEX2(NINTER),
     .                NINDEX3(NINTER), NINDEX4(NINTER), KINDRBYM(NRBYM),
     .                OMP_GET_THREAD_NUM,IADISK,RNUM_SIZ,RNS,NINDEXP,NI,LENC,ITHOUT
              integer :: omp_address
              INTEGER, DIMENSION(:),ALLOCATABLE :: IGROUC
              INTEGER, DIMENSION(:),ALLOCATABLE :: IGROUNC
              EXTERNAL OMP_GET_THREAD_NUM
              INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFIDEL, INDIDEL,RENUM,IBUFSEGLO,INDSEGLO
              my_real DT2TT,D_TSTART,D_TSTOP
              my_real XSLV(18,NINTER),XMSR(12,NINTER),
     .                VSLV(6,NINTER),VMSR(6,NINTER), SIZE_T(NINTER)
C End OpenMP specific
C Parith/ON specific
              INTEGER FR_RBY6(SFR_RBY)
              DOUBLE PRECISION
     .                FRL6(15,6,NRLINK),FNL6(15,6,NLINK),
     .                FRWL6(7,6,NRWALL),RBYM6(6,6,NRBYM),SPHG_F6(4,6,NBGAUGE)
              DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE :: RBY6
              my_real FRL(4,NRLINK), FNL(4,NLINK)
              INTEGER NGRTH,NELEM
              INTEGER RESTSIZE
C End Parith/ON specific
              INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1)
              INTEGER, POINTER, DIMENSION(:) :: PTR_SMS
              INTEGER NMT0
              INTEGER, DIMENSION(:), ALLOCATABLE :: IKINE
              my_real, DIMENSION(:), ALLOCATABLE :: STK_SN,STK_SR,FCLUSTER,MCLUSTER
              my_real, DIMENSION(:), ALLOCATABLE :: NODA_FEXT
              INTEGER LNZM
              INTEGER, DIMENSION(:),ALLOCATABLE :: INT18ADD,TAGPENE
              INTEGER, DIMENSION(:),ALLOCATABLE :: IDAMP_RDOF_TAB
              my_real, DIMENSION(:,:),ALLOCATABLE :: MTF, CAND_SAV
              INTEGER FLG_DTNODAMP,FLG_DAMP_FUNCT
              my_real DT3
              INTEGER  IGROUPFLG(2)
              INTEGER, DIMENSION(:),ALLOCATABLE :: IGROUPC,IGROUPTG,IGROUPS
              INTEGER  IOLDSECT
              ! TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
              my_real, DIMENSION(:),ALLOCATABLE :: SFEM_NODVAR     !lagrange framework
              my_real, DIMENSION(:),ALLOCATABLE :: SFEM_NODVAR_ALE !ALE framework
              INTEGER S_SFEM_NODVAR ! same size for ALE and Lagrange frameworks
C-----------------------------------------------------------------------------------
C Parith/OFF + AMS
              INTEGER, DIMENSION(:),ALLOCATABLE :: UNCOMP_FR,FR_LOC,
     *                                             UNCOMP_FRI2M,FR_LOCI2M
              INTEGER NB_FR,NB_FRI2M
C-----------------------------------------------------------------------------------
C ADDITIVE MANUFACTURING
              my_real, DIMENSION(:), ALLOCATABLE :: MCP_OFF
C-----------------------------------------------------------------------------------
C TEMPI I24
              INTEGER SFR_I24
              INTEGER, DIMENSION(:), ALLOCATABLE :: FR_I24
              INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_I24
              my_real
     .                DELTA_PMAX_GAP(NINTER)
              INTEGER DELTA_PMAX_GAP_NODE(NINTER)
              INTEGER S_LOADPINTER, NPRESLOAD
              INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGNCONT
              INTEGER, DIMENSION(:), ALLOCATABLE :: LOADP_HYD_INTER, LOADP_TAGDEL
C-----------------------------------------------------------------------------------
C Debug CAND INTERFACE TYPE25
              INTEGER,DIMENSION(:),ALLOCATABLE :: NB25_CANDT
              INTEGER,DIMENSION(:),ALLOCATABLE :: NB25_IMPCT
              INTEGER,DIMENSION(:),ALLOCATABLE :: NB25_DST1
              INTEGER,DIMENSION(:),ALLOCATABLE :: NB25_DST2
              SAVE NB25_CANDT,NB25_IMPCT,NB25_DST1,NB25_DST2
C-----------------------------------------------------------------------------------
C Communication INTERFACE TYPE25
              INTEGER SFR_I25
              INTEGER, DIMENSION(:), ALLOCATABLE :: FR_I25
              INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_I25
C Assembling normals INTERFACE TYPE25
              REAL(kind=4), DIMENSION(:,:), ALLOCATABLE :: FSKYN25
C time varying gap T25
              my_real MAXDGAP(NINTER)
              INTEGER :: FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE
C-----------------------------------------------------------------------------------
C Multidomains
              INTEGER IDEL7NOK_R2R,IDEL7NOK_SAV,NUMSPH_GLO_R2R,FLG_SPHINOUT_R2R
              INTEGER, DIMENSION(:),ALLOCATABLE :: OFF_SPH_R2R
C-----------------------------------------------------------------------------------
C Sensors inter
              INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ISENSINT
              INTEGER NISUBMAX

              INTEGER, ALLOCATABLE, DIMENSION(:) :: ICONTACT_OLD
C-----------------------------------------------------------------------------------
              my_real
     .         ,  DIMENSION(:),ALLOCATABLE :: WASPSYM
C AMS
              INTEGER LSKYI_SMS_NEW
              INTEGER, ALLOCATABLE, DIMENSION(:,:) :: T2MAIN_SMS
              my_real, DIMENSION(:),ALLOCATABLE :: T2FAC_SMS
              my_real, DIMENSION(:), ALLOCATABLE :: MSKYI_FI_SMS
              INTEGER, DIMENSION(:),ALLOCATABLE :: LIST_SMS
              INTEGER, DIMENSION(:),ALLOCATABLE :: LIST_RMS
              my_real, DIMENSION(:,:), ALLOCATABLE :: CJWORK
              my_real, DIMENSION(:,:), ALLOCATABLE :: FREA
              INTEGER, DIMENSION(:), ALLOCATABLE :: IRWL_WORK
              my_real, DIMENSION(:,:), ALLOCATABLE :: SMS_VFI
              DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: MW6
              integer sz_mw6
C-----------------------------------------------------------------------------------
C     FVMBAG: switch to UP
              INTEGER :: NFVBAG0
              LOGICAL :: CHECK_NPOLH
C---ADMESH + TRIMMING----
              INTEGER, DIMENSION(:),ALLOCATABLE :: TAGTRIMC,TAGTRIMTG
              INTEGER :: IFTHE, ICONDN, IDX_FTHE, IDX_CONDN, IDX_PINCH,MSTOP_DT_THERM
C---Nitsche Method----
              INTEGER NFACNIT
              my_real, DIMENSION(:,:),ALLOCATABLE :: STRESSMEAN
              my_real, DIMENSION(:),ALLOCATABLE :: FORNEQSKY
              INTEGER :: NUMSKW_L,NUMSKW_L_SEND,NODFTSK_2,NODLTSK_2
              INTEGER, DIMENSION(NUMSKW) :: ISKWP_L_SEND
              INTEGER, DIMENSION(NSPMD) :: RECVCOUNT
              INTEGER, DIMENSION(NTHREAD) :: NODFT_ASSPAR, NODLT_ASSPAR
              INTEGER NODFT_NL,NODLT_NL
              REAL(kind=8) :: SECS
              REAL(kind=8) :: TT_DOUBLE
              my_real
     .                 DTNOD_NLOCAL,T_KIN
              INTEGER, DIMENSION(:),ALLOCATABLE ::IBUFPDEL, NINDEXPDEL
              DOUBLE PRECISION :: argin,argout
              DOUBLE PRECISION, DIMENSION(:,:,:),ALLOCATABLE :: RBY6_C
C----------------------------------------------------------------------------------
C IMPVEL/FGEO
              INTEGER FXVEL_FGEO
C----------------------------------------------------------------------------------
C /DAMP/VREL
              INTEGER SIZE_RBY6_C,FL_VREL
              my_real DAMP_A(3)
C----------------------------------------------------------------------------------
C /VIPER/ON
              type(viper_coupling_) :: VIPER
C----------------------------------------------------------------------------------
C Type2 output - PCONT2
              INTEGER :: SZ_NPCONT2
              my_real, DIMENSION(:,:),ALLOCATABLE :: NPCONT2
C----------------------------------------------------------------------------------
!  contact w/ offset
              my_real, TARGET,  DIMENSION(:,:),ALLOCATABLE :: XYZ
              my_real, POINTER,   DIMENSION(:,:), contiguous :: ptrX, ptrX_offset
              TYPE(sh_offset_)                   :: SH_OFFSET_TAB
C-----------------------------------------------------------------------------------
              INTEGER IFLAG, COMPTREAC ! Flag for computing reaction forces
!     1. Storing Freac=Fint+Fext+Fcont
!     2. Add acceleration fields (gravity, etc) to Freac
!        Downloading velocities (cf so-called main/secnd formulation)
!        In RD, accelerations are computed so that the resulting velocity after integration
!                                                      will correspond to the expected velocity
!               because sortie_main (output) is done before integration of accelerations
!     3. Finalize Freac=MS*A-Freac
C----------------------------------------------------------------------------------
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NSKWP : integer ; dimension = NSPMD
!               number of skew per processor
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!               communicated
!       NUMSKW_L : integer
!                  number of local SKEW
!       NUMSKW_L_SEND : integer
!                       number of sent SKEW
!       ISKWP_L_SEND : integer ; dimension=NUMSKW_L_SEND
!                      index of sent SKEW
!       RECVCOUNT : integer ; dimension=NSPMD
!                   number of received SKEW
!       NODFT_ASSPAR : integer ; dimension=NTHREADS
!                      lower bound for asspar4 splitting
!       NODLT_ASSPAR : integer ; dimension=NTHREADS
!                      upper bound for asspar4 splitting
!       TT_DOUBLE : real(kind8)
!                   current time in double precision --> with simple precision version,
!                   the current time can be wrong (due to error's accumulation) if the
!                   number of cycle is huge AND the time step is small
!                   (ie. when NCYCLE > 300 000 and DT = 10^-7)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
              INTEGER :: STATE_H3D, STATE_ANIM
              LOGICAL :: BOOL_RESTART
              INTEGER :: NATIV_SMS_SIZ ! size of NATIV_SMS array
              INTEGER :: TEMP_SIZ      ! size of TEMP
              TYPE(shooting_node_type) :: SHOOT_STRUCT
              INTEGER :: S_ELEM_STATE     ! size of ELEM_STATE array
              LOGICAL, DIMENSION(:), ALLOCATABLE :: ELEM_STATE    ! boolean : true if element is ON, false if element is OFF
              INTEGER :: SIZE_ADDCNEL ! size of addcnel array
              INTEGER :: SIZE_CNEL ! size of cnel array
              LOGICAL :: GLOBAL_ACTIVE_ALE_ELEMENT
              INTEGER :: SIZE_NPBY
              ! ----------
              LOGICAL :: NEED_COMM_INTER18 !< true if the mpi comm "exchange of remote XCELL data" is mandatory
              INTEGER :: NUMBER_INTER18 ! number of interface 18
              INTEGER, DIMENSION(NINTER) :: INTER18_LIST ! list of interface 18
              TYPE(array_type), DIMENSION(:), ALLOCATABLE :: XCELL_REMOTE ! remote data structure for interface 18
              ! ----------
              my_real, DIMENSION(:), ALLOCATABLE :: FSKY_L
              LOGICAL, DIMENSION(NSPMD) :: NEED_COMM_INT25_SOLID_EROSION !< boolean, true if the proc needs to comm some values related to interface type 25 with solid erosion
              INTEGER :: COMM_INT25_SOLID_EROSION !< integer, sub-communicator related to interface type 25 with solid erosion
              INTEGER :: CHECK_NEIGH_FLAG,CHECK_NEIGH_FLAG_RES
              integer, dimension(nspmd+2) :: frontier_global_mv !< frontier fo monitored volume
              integer :: sporo
              ! Restart File Writing
              INTEGER STOP_OR_ADD_CYCLE  !< Check if additional cycle must be done after restart at termination
              INTEGER, PARAMETER :: MAXFUNC = 100
              integer :: numnod_old,numnodm_old
              integer :: new_crack,nsect_offset
C-----------------------------------------------------------------------------------
C Err THK
C-----------------------------------------------------------------------------------
              my_real, DIMENSION(:), ALLOCATABLE :: THICK_SH4
              my_real, DIMENSION(:), ALLOCATABLE :: THICK_SH3
              my_real, DIMENSION(:), ALLOCATABLE :: THICK_NOD
              my_real, DIMENSION(:), ALLOCATABLE :: AREA_NOD
              my_real, DIMENSION(:), ALLOCATABLE :: AREA_SH4
              my_real, DIMENSION(:), ALLOCATABLE :: AREA_SH3
C-----------------------------------------------------------------------------------
C admerr
C-----------------------------------------------------------------------------------
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_THICK_SH4
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_THICK_SH3
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_THICK_NOD
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_AREA_NOD
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_AREA_SH4
              my_real, DIMENSION(:), ALLOCATABLE :: admerr_AREA_SH3
C-----------------------------------------------------------------------------------
              type(SPH_WORK_)  :: SPH_WORK   !< working Areas for SPLISSV and SPHPREP
              type (ams_work_) :: ams_work   !< Working areas for AMS

              type(component_), dimension(:), allocatable :: component
C-----------------------------------------------------------------------------------
C coupling coupling
              logical :: ongoing
              double precision :: dt2max_coupling
C-----------------------------------------------------------------------------------
              call resol_alloc_phase1(rby6,dxancg,nb25_candt,nb25_impct,nb25_dst1,nb25_dst2,igrouc,igrounc, 
     .                                interfaces,int18add,idamp_rdof_tab,icontact_old, 
     .                                nrbykin,numnod,parasiz,ngroup,ninter,sicontact,nodes,
     .                                isendto,ircvfrom,intlist,intlist25,niskyfi,niskyfie,
     .                                count_remslv,count_remslve,fr_nbcc,fr_nbcci2,
     .                                dretri,xsec,nsect,nspmd,ispmd,ninter25, IRBKIN_L,FR_NBCC1)

              interfaces%ninter = ninter
              interfaces%npari = npari
              element%shell%offset = numels + numelq
              BOOL_RESTART = .TRUE.
              LOUT = ISPMD==0
              STATE_H3D = 0
              STATE_ANIM = 0
              NINTER22 = INT22
              IDAMP_RDOF_TAB = 0
              FLG_DTNODAMP = 0
              FLG_KJ2 = 0
              I_EXCH_FLG_RAZ = 0
              NELTST  = 0
              ITYPTST = 0
              DT2T    = 0
              MSTOP_DT_THERM = 0
C initialize int24 presence flag
              INT24USE = 0
              INT24E2EUSE = 0
C-----------------------------------------------
C exit flag for MaDyMo coupling
              CPLXIT=0
              ILASTANIM=0
              ILASTH3D=0
              IOLDSECT = -1
              ILASTDYNAIN=0
              NODFT   = 1
              NODLT   = NUMNOD
              NWAFT   = 1
              IWIOUT  = 0
              LMPC    = 0
              TT_DOUBLE = TT
              LSKYI_COUNT = 0
              FLAG_SLIPRING_UPDATE = 0
              FLAG_RETRACTOR_UPDATE = 0
              FLG_DTNODAMP = 0
              IF ((NODADT==1).AND.(IDAMP_RDOF==NDAMP .OR. NDAMP>0 .OR. ISTAT==3)) FLG_DTNODAMP = 1
              FLG_DAMP_FUNCT = 0
              IF (NDAMP>0) THEN
                DO I=1,NDAMP
                  IF(NINT(DAMPR(21,I))==4) FLG_DAMP_FUNCT=1
                END DO
              END IF
              NSENSOR = SENSORS%NSENSOR
              TEMP_SIZ = MERGE(NUMNOD, 0, GLOB_THERM%ITHERM_FE>0)
              SIZE_RBY6_C = MERGE(NRBYKIN, 0, NDAMP_VREL_RBYG > 0)
              ALLOCATE(RBY6_C(2,6,SIZE_RBY6_C))
              IF(ISPMD==0) LMPC = SUM(IBMPC(1:NUMMPC))
C----------------------------------------------
C     I N I T I A L I S A T I O N S
C----------------------------------------------
              SIZE_NPBY = SNPBY/NNPBY
C -------------------------------------------------------------------
C User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
              GETUNOD_NOCOM=0
C----------------------------------------------
C       Allocations Phase1
C----------------------------------------------
              IMSCH  = 0
              I2MSCH = 0
              DT2PREV= ZERO
              LISENDP_PXFEM = 0 ; LIRECVP_PXFEM = 0
              LISENDP_CRK = 0 ; LIRECVP_CRK = 0
              PBLAST%PBLAST_DT%IDT   = 0
              NUMSPH_GLO_R2R = 0
              FLG_SPHINOUT_R2R = 0
              call resol_alloc_phase2(element,nodes,iplyxfem,icrack3d,nspmd,lisendp, lirecvp, 
     .                               lisendp_pxfem, lirecvp_pxfem, lisendp_crk, lirecvp_crk, 
     .                               isendp_pxfem, irecvp_pxfem, isendp_crk, irecvp_crk, adsky_crk,inod_crk,
     .                               procne_crk,procne_pxfem,adsky_pxfem,inod_pxfem)
C----------------------------------------------------------
C      Allocation of tables for /LOAD/PRESSURE
C-----------------------------------------------------------
              call resol_alloc_phase3(NLOADP_HYD_INTER,NINTLOADP,NLOADP_HYD,ILOADP,SIZLOADP,NUMNOD, 
     .                            LOADP_HYD_INTER,TAGNCONT,NINTER, 
     .                            S_LOADPINTER,NPRESLOAD,LOADP_TAGDEL)
C----------------------------------------------------------
              ALLOCATE(INTERFACES%PARAMETERS%INTAREAN(MERGE(NUMNOD, 0, INTERFACES%PARAMETERS%INTCAREA>0)))
C       ! /TH/SURF output
             CALL resol_alloc_phase4(nsurf,output,th_surf_num_channel,nloadp, 
     .                                 nloadp_f,pblast,iloadp,sizloadp, cnel, addcnel,size_cnel,size_addcnel, 
     .                                 idel7ng,irad2r,alemuscl_param,alemuscl_buffer,pdel, addtmpl,tagel,
     .                                 numnod,lcnel,numels,numelq,numelc,numelt,numelp,numelr,numeltg, 
     .                                 npart,partsav2,ipartl,elem_state,s_elem_state)
C
C   Working arrays for thermal analysis
C
             call resol_alloc_phase5(GLOB_THERM,NUMNOD,NINTER,NODADT,NTHREAD,IPARIT,LSKY,LSKYI,
     .                               ICODT0,ICODR0,MCP_OFF,FTHE,FTHESKY,QFRICINT,CONDN,CONDNSKY, 
     .                               FTHESKYI,CONDNSKYI,ICONDN,IFTHE)
C
C  ply xfem for composite shell
C
              call resol_alloc_phase6(intplyxfem,iplyxfem,nplymax,ply,plysky,iparit,nthread, 
     .                                nplyxfe,anim_ply,vn_nod,irigid_mat,nrbym, 
     .                                vrbym,vrrbym,arbym,arrbym,plyskyi,lskyi,lskypxfem)
 
C ----------------------------------------------
C    SIMPLIFIED ALE FORMULATION
C ----------------------------------------------
               call resol_alloc_phase7(ialelag,numnod,nthread,iparit,lsky,msnf,msf,
     .                                aflow,vflow,dflow,wflow,ffsky,ifoam,ifoam_cont)
C--------------------------------------------
C Adaptive Meshing
C--------------------------------------------
               CALL resol_alloc_phase8(NADMESH,NUMELC,NUMELTG,NUMNOD,LEVELMAX,IPARIT,ISTATCND,ANIM_N, 
     .                                H3D_DATA,IRODDL,LSH4ACT,LSH4KIN,PSH4ACT,PSH4KIN,LSH3ACT,LSH3KIN, 
     .                                PSH3ACT,PSH3KIN,MSH4SKY,MSH3SKY,ILEVNOD,LSH4UPL,LSH3UPL,PSH4UPL,PSH3UPL,
     .                                ACND,ARCND,STCND,STRCND,STIFR_TMP,STIFN_TMP,NTHREAD)
C--------------------------------------------
C       Error estimation / not on Restart file.
C--------------------------------------------
             call resol_alloc_phase9(ANIM_CE,NUMELC,NUMELTG,NUMNOD,IADMERRT,H3D_DATA, 
     .                                INTER_ITHKNOD,ERR_THK_SH4,ERR_THK_SH3,THKSH4,THKSH3,THKNOD, 
     .                                AREA_SH4,AREA_SH3,AREA_NOD,THICK_SH4,THICK_SH3,THICK_NOD, 
     .                                admerr_AREA_SH4,admerr_AREA_SH3,admerr_AREA_NOD, 
     .                                admerr_THICK_SH4,admerr_THICK_SH3,admerr_THICK_NOD)
 
C----------------------------------------------------------
C    SELECTIVE MASS SCALING
C----------------------------------------------------------
C for chkpt :: ISMSCH=1
              ISMSCH  =1
             call resol_alloc_phase10(NGROUP,IDTMINS, IDTMINS_INT,TAGNOD_SMS,NATIV_SMS,TAGPRT_SMS,TAGREL_SMS,INDX1_SMS,INDX2_SMS, 
     .                                 TAGSLV_RBY_SMS,TAGMSR_RBY_SMS,KAD_SMS,JAD_SMS,IAD_SMS,LAD_SMS, 
     .                                 JADC_SMS,JADS_SMS,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS,JADTG_SMS, 
     .                                 X_SMS,P_SMS,Y_SMS,Z_SMS,PREC_SMS,XMOM_SMS,PREC_SMS3,DIAG_SMS3, 
     .                                 T2MAIN_SMS,T2FAC_SMS,FR_RMS,FR_SMS,PTR_SMS,NSPMD,NINTSTAMP, 
     .                                 JADI21_SMS,TAGSLV_I21_SMS,TAGMSR_I21_SMS,NODXI_SMS,NODII_SMS, 
     .                                 MSKYI_SMS,ISKYI_SMS,JADI_SMS,JDII_SMS,LTI_SMS,LSKYI_SMS,
     .                                 npart,numelc,numels,numels10,numelt,numelp,numelr,numeltg,numnod)
C------------------------------------------
             call resol_alloc_phase11(IREAC,IGRELEM,NUMNOD,NUMELSG,NUMELS16G,NUMSPHG, 
     .                                NUMELCG,NUMELTGG,NUMELQG,NUMELTG,NUMELPG,NUMELRG,NTHPART, 
     .                                NGPE,NGRTH,NELEM,NODREAC,GRTH,IGRTH,DXANCG, 
     .                                IGROUPFLG,IGROUPC,IGROUPTG,IGROUPS,NUMMAT,IPM,NPROPMI, 
     .                                NVOLU,NUMELC,NUMELS)
 
C-----------------------------------------------
C /IMPDISP/FGEO
              FXVEL_FGEO = 0
C-----------------------------------------------
C     End Allocations Phase 1
C-----------------------------------------------
              !initialisation timer resol
              IF (IMON>0) THEN
                CALL STARTIME(TIMERS,TIMER_RESOL)
              ENDIF
C INTERFACES RENUM
              RNUM_SIZ=NUMNOD
              ALLOCATE(RENUM(RNUM_SIZ))
C-----------------------------------------------
C     Sensor Inter
C-----------------------------------------------
              NISUBMAX = 0
              DO I=1,NINTER
                NISUBMAX = MAX(NISUBMAX,IPARI(36,I))
              ENDDO
              ALLOCATE(ISENSINT(NISUBMAX+1,NINTER))
              ISENSINT(1:NISUBMAX+1,1:NINTER) = 0
C-----------------------------------------------
              IDEL7NOK_SAV=IDEL7NOK
C-----------------------------------------------
              CALL TRACE_IN(6,0,ZERO)
C-----------------------------------------------
C     Initialisations
C-----------------------------------------------

              ALLOCATE(TAGTRIMC(MERGE(NUMELC, 0, NADMESH/=0.AND.IDEL7NG>=1)))
              ALLOCATE(TAGTRIMTG(MERGE(NUMELTG, 0, NADMESH/=0.AND.IDEL7NG>=1)))

              call resol_alloc_nitsche(NFACNIT, NITSCHE, IPARIT, NUMELS, NUMELS10G, LSKY, STRESSMEAN, FORNEQSKY)

              CALL NEWSKW_INIT(ISKWP,NUMSKW_L,NSKWP,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT)
              PARTSAV2(1:2,1:NPART) = ZERO

              ! Initialization of INTER_STRUCT structure + allocation
              ALLOCATE( INTER_STRUCT(NINTER) )
              ALLOCATE( SORT_COMM(NINTER) )
              CALL INTER_STRUCT_INIT(INTER_STRUCT,SORT_COMM)
C========================================================================================

C save the Python functions into the Python interpreter dictionarry
              call resol_alloc_python(PYTHON, NODES, NUMNOD, IXS, NIXS, NUMELS, 
     .            NIXC, NUMELC, IXP, NIXP, NUMELP, IXT, NIXT, NUMELT, 
     .            IXQ, NIXQ, NUMELQ, IXTG, NIXTG, NUMELTG, 
     .            IXR, NIXR, NUMELR, IPARG, NGROUP, NPARG, MVSIZ, 
     .            IPART, LIPART1, NPART, NTHPART, ISPMD, N2D, NUMGEO, NUMMAT, 
     .            ELBUF_TAB, GEO, PM, BUFMAT, EANI, 
     .            IPM, IGEO, THKE, STHKE, ERR_THK_SH4, ERR_THK_SH3, 
     .            W, ALE_CONNECTIVITY, NPROPG, NPROPGI, NPROPM, NPROPMI, SEANI, 
     .            snercvois, snesdvois, slercvois, slesdvois, 
     .            NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .            M51_N0PHAS, M51_NVPHAS, STACK, 
     .            MULTI_FVM, MAT_ELEM, OUTPUT, GLOB_THERM, ELEMENT)
C========================================================================================
C Initialize SENSORS & communication buffers.
C Play Time & Logical sensors.
C Reinitialise sensor variables defined in /SENS/RESET

              K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
              K2=K1+NUMELS
              K3=K2+NUMELQ
              K4=K3+NUMELC
              K5=K4+NUMELT
              K6=K5+NUMELP
              K7=K6+NUMELR
              K8=K7
              K9=K8+NUMELTG
              K10=K9+NUMELX
              K11=K10+NUMSPH
              K12=K11+NUMELIG3D
              CALL SENSOR_INIT(SUBSETS,IPARG,NGROUC,
     .                         IPART(K3),IPART(K8),IPART(K1),IPART(K2),IPART(K4),
     .                         IPART(K5) ,IPART(K6),SENSORS,TT ,DT2 ,IOUT, PYTHON ,NTHREAD)


C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              NEED_COMM_INT25_SOLID_EROSION(1:NSPMD) = .FALSE.
              COMM_INT25_SOLID_EROSION = 0
              L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
              L2 = L1+6*NUMELS10
              L3 = L2+12*NUMELS20
              LL1 = 1+8*NUMELS
              LL2 = LL1+6*NUMELS10
              LL3 = LL2+12*NUMELS20
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,N1,GREFTSK,GRELTSK)
C Init var parallel SMP
              CALL SMP_INIT(
     1         ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2         IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3            GREFTSK,GRELTSK)
C
              CALL RESOL_INIT(
     1         ITSK     ,FR_NBCC  ,
     2         ISENDTO  ,IRCVFROM ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY  ,NODES%ITABM1   ,
     3         IPARI    ,IPARG    ,NODES%ITAB     ,IXS(L1)  ,IXS(L2)  ,
     4         I13A     ,I13B     ,I13C     ,I13D     ,I13E     ,
     5         I13F     ,I13G     ,I13H     ,I13I     ,I15A     ,
     6         I15B     ,I15C     ,I15D     ,I15E     ,I15F     ,
     7         I15G     ,I15H     ,I15I     ,I87A     ,I87B     ,
     8         I87C     ,I87D     ,I87E     ,I87F     ,I87G     ,
     9         NFIA     ,NFEA     ,NFOA     ,NDMA     ,NDMA2    ,
     A         NODFTSK  ,NODLTSK  ,NDTSK    ,NUMNTSK  ,IXS(L3)  ,
     B         IXS      ,IXQ      ,ELEMENT%SHELL%IXC      ,IXT      ,IXP      ,
     C         IXR      ,IXTG     ,ELEMENT%PON, NODES%IKINE    ,
     D         NODES%A        ,NODES%AR       ,NODES%V        ,NODES%VR       ,
     E         NODES%X        ,NODES%D        ,NODES%MS       ,NODES%IN ,NODES%STIFN    ,
     F         NODES%STIFR    ,DMAS     ,DINER    ,
     G                   WA       ,UWA      ,PM       ,GEO      ,
     H         PARTSAV  ,PARTS0   ,MONVOL   ,
     I         I87H     ,I87I     ,I87J     ,I87K     ,
     J         I15J     ,KXX      ,
     K         SECBUF   ,SECFCUM  ,NSTRF    ,IGRNOD   ,IEXLNK   ,
     L         XFRAME   ,
     M         IXTG1    ,IBCL     ,NODES%VISCN    ,DD_R2R   ,
     O         ELBUF    ,IPART    ,MADPRT   ,MADSH4   ,
     P         MADSH3   ,MADSOL   ,MADNOD   ,MADFAIL  ,IGEO     ,
     Q         INTLIST  ,NBINTC   ,ELEMENT%PON%PROCNE   ,NISKYFI  ,NODES%WEIGHT   ,
     R         ISIZXV   ,ILENXV   ,ADDCNI2  ,PROCNI2  ,IAD_I2M  ,
     S         FR_I2M   ,FR_NBCCI2,I2SIZE   ,FR_MAD   ,LWIBEM   ,
     T         LWRBEM   ,FXBFP    ,FXBEFW   ,FXBEDP   ,FXBGRP   ,
     U         FXBGRW   ,NDIN    ,
     V         ISLEN7   ,IRLEN7   ,ISLEN11  ,IRLEN11  ,
     W         LWIFLOW  ,LWRFLOW  ,IFLOW    ,ADDCNEL  ,CNEL     ,
     X         ADDTMPL  ,IPARTL   ,NPARTL   ,NFNCA    ,NFTCA    ,
     Y         I15ATH   ,I35ATH   ,IPM      ,SH4TREE  ,IPADMESH ,
     Z         MSC      ,INC      ,SH3TREE  ,MSTG     ,INTG     ,
     a         PTG      ,FTHE     ,FTHESKY  ,FTHESKYI ,NME17    ,
     b         ISLEN17  ,IRLEN17  ,IRLEN7T  ,ISLEN7T  ,LINDIDEL ,
     c         LBUFIDEL ,SH4TRIM  ,SH3TRIM  ,MSCND    ,INCND    ,
     d         IRLEN20  ,ISLEN20  ,IRLEN20T ,ISLEN20T ,NBINT20  ,
     e         IRLEN20E ,ISLEN20E ,NISKYFIE ,
     f         NODES%MCP      ,NODES%MS0      ,INOD_PXFEM,IEL_PXFEM,IADC_PXFEM,
     g         ADSKY_PXFEM,NODES%ICODT  ,NODES%ICODR    ,IBFV     ,ADMSMS    ,
     h         NODREAC  ,IGROUC   ,NGROUC   ,IGROUNC  ,NGROUNC  ,
     i         FR_RBY   ,FR_RBY6  ,NPBY     ,
     j         NOM_SECT ,MCPC     ,MCPTG    ,GRTH     ,IGRTH     ,
     k         NELEM    ,LAG_SEC  ,RWALL%NPRW     ,DIAG_SMS ,DMELC     ,
     l         DMELTG   ,NGRTH    ,NFT2     ,DMELS    ,DMELTR    ,
     m         DMELP    ,DMELRT   ,RES_SMS  ,I87L     ,IRBE2     ,
     n         LRBE2    ,NMRBE2   ,IAD_RBE2 ,FR_RBE2  ,FR_RBE2M  ,
     o         R2SIZE   ,LPBY     ,PROCNE_PXFEM,ISENDP_PXFEM,IRECVP_PXFEM ,
     p         IADSDP_PXFEM,IADRCP_PXFEM,FR_NBCC1,RBY ,INT18KINE ,
     q         NODES%XDP      ,I87M     ,INOD_CRK  ,IEL_CRK ,IADC_CRK,
     r         ADSKY_CRK,PROCNE_CRK,ISENDP_CRK,IRECVP_CRK,
     s         IADSDP_CRK,IADRCP_CRK ,INT24USE,NDAMA2    ,
     t         IGROUPC  ,IGROUPTG ,IGROUPS   ,IGROUPFLG ,DMINT2  ,IRBKIN_L ,
     u         NRBYKIN_L,KINDRBY  ,ELBUF_TAB ,SENSORS ,DD_R2R_ELEM,
     v         SDD_R2R_ELEM,NODES%KINET, NODES%WEIGHT_MD ,DMSPH   ,IOLDSECT,LBUFSEGLO,
     w         INTERFACES%INTBUF_TAB  ,NUMSPH_GLO_R2R, FLG_SPHINOUT_R2R,I15K,
     y         CONDN       ,CONDNSKY,KXFENOD2ELC  ,ELCUTC ,NODEDGE,
     z         IAD_EDGE ,CRKNODIAD,FR_EDGE     ,FR_NBEDGE ,NODLEVXF,
     x         CRKEDGE  ,XFEM_TAB  ,ISENSINT ,NISUBMAX  ,
     1         INTLIST25 ,INT24E2EUSE,TABMP_L ,
     2         I87N ,TAB_MAT,H3D_DATA,TAGTRIMC,TAGTRIMTG ,
     3         IGRBRIC ,IGRQUAD  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS   ,
     4         IGRBEAM ,IGRSPRING,IGRPART  ,FORNEQS  ,INT7ITIED,
     5         FXVEL_FGEO,FAILWAVE,NLOC_DMG,PINCH_DATA ,SLLOADP,
     6         NODES%TAG_S_RBY,NFNCA2  ,NFTCA2  ,NODES%IN0 ,SORT_COMM,STACK,OUTPUT,
     7         THKE   ,NODES%BOUNDARY_SIZE  ,SH_OFFSET_TAB,
     8         NEED_COMM_INT25_SOLID_EROSION,COMM_INT25_SOLID_EROSION ,
     9         ISKWN  ,IFRAME  ,LOADS ,GLOB_THERM,PBLAST,RBE3,NHIER_RBY)

!$OMP END PARALLEL
C     End Initialisations //     
              CALL PYTHON_END_OPENMP(PYTHON)
              IF (ALEMUSCL_Param%IALEMUSCL == 0) DEALLOCATE(ADDTMPL)
C========================================================================================
C                                  NON PARALLEL SECTION (SMP)
C========================================================================================
              CALL SPLIT_ASSPAR4(ELEMENT%PON%ADSKY,NUMNOD,NTHREAD,NODFT_ASSPAR,NODLT_ASSPAR,ELEMENT%PON%SADSKY)
!  shell offset for contact (penalty), should use XYZ instead of X to take into account the offset
              IF (SH_OFFSET_TAB%NNSH_OSET > 0 .AND. IMPL_S==0) THEN
                ALLOCATE(XYZ(3,NUMNOD))
                XYZ(1:3,1:NUMNOD) = NODES%X(1:3,1:NUMNOD)
                CALL offset_nPROJ(NSPMD,NUMNOD,XYZ,SH_OFFSET_TAB,IPARIT)
              ELSE
                ALLOCATE(XYZ(3,1))
!--- deactivate contact w/ offset
                SH_OFFSET_TAB%NNSH_OSET = 0
              ENDIF

              IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
              ELSE
                CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
              ENDIF


              ! Allocation and initialization of /INT18 + LAW151
              ALLOCATE( XCELL_REMOTE(NINTER) )
              CALL INT18_ALLOC(NUMBER_INTER18,INTER18_LIST,MULTI_FVM,IPARI,XCELL_REMOTE,NSPMD)
              CALL INT18_LAW151_INIT(MULTI_FVM,IGRBRIC,IPARI    ,IXS,
     1                               IGROUPS  ,IPARG  ,ELBUF_TAB,MULTI_FVM%FORCE_INT    ,
     2                   ptrX     ,     NODES%V       ,          NODES%MS         ,    NODES%KINET   ,
     3           MULTI_FVM%X_APPEND,MULTI_FVM%V_APPEND,MULTI_FVM%MASS_APPEND,MULTI_FVM%KINET_APPEND,
     4           MULTI_FVM%FORCE_INT_PON)


              ! allocation & initialization for /FAIL/ALTER
              CALL FAIL_WIND_FRWAVE_INIT(NGROUP)
              ! Check if ALE elements are deactivated to avoid some mpi comm. in the ALE solver
              GLOBAL_ACTIVE_ALE_ELEMENT = .FALSE.
              CALL CHECK_ALE_COMM(IPARG,ELBUF_TAB,GLOBAL_ACTIVE_ALE_ELEMENT,GLOB_THERM%ITHERM)
              ! initialization of some size and arrays for ALE solver
              call init_ale(global_active_ale_element,n2d,numels,numelq,nmult,
     .                      iale,ieuler,trimat,glob_therm%itherm,numnod,    
     .                      nspmd,nsvois,nqvois,nparg,ngroup,slesdvois,slercvois,        
     .                      nesdvois,nercvois,lesdvois,lercvois,nodes%itab, 
     .                      nodes%itabm1,ixs,ixq,iparg,ale,ale_connectivity)

              IF(NADMESH/=0.AND.IDEL7NG>=1) IDEL7NOK = 0
C     THERMAL TIME STEP : we don t check incompatible kinematic conditions anymore
c      we constraint all DDLs
              IF (GLOB_THERM%IDT_THERM == 1) THEN
                CALL BCSDTTH_COPY(NODES%ICODT, NODES%ICODR, ICODT0, ICODR0 ,1 )
              ENDIF

              ! -----------------
              ! Monitored volume : initialization of global frontier array for mpi comm
              if(nvolu/=0) then
                call init_global_frontier_monvol(ispmd,nspmd,nvolu,nsurf,monvol,
     .                               nimv,volmon,nrvolu,
     .                          fr_mv,frontier_global_mv, t_monvol,igrsurf )
              else
                frontier_global_mv(1:nspmd+2) = 0
              endif
              ! Monitored volume : initialization of omp array
              call init_monvol_omp_structure(ispmd,nspmd,nvolu,nsurf,monvol,
     .                               nimv,numnod,
     .                          fr_mv,t_monvol,igrsurf )
              ! -----------------
              I24MAXNSNE=0
              IF (INT24USE == 1)THEN
                ALLOCATE(IAD_I24(NBINTC+1,NSPMD))
                SFR_I24=0
                CALL  SPMD_I24_PREPARE(1,IPARI, INTERFACES%INTBUF_TAB,
     *                                NODES%BOUNDARY_ADD, NODES%BOUNDARY,INTLIST,NBINTC,
     *                                IAD_I24 , SFR_I24, IDUM,I24MAXNSNE)
                ALLOCATE(FR_I24(SFR_I24))
                CALL  SPMD_I24_PREPARE(2,IPARI, INTERFACES%INTBUF_TAB,
     *                                NODES%BOUNDARY_ADD, NODES%BOUNDARY,INTLIST,NBINTC,
     *                                IAD_I24 , SFR_I24, FR_I24,I24MAXNSNE)
C E2E Fictive Node Position, Velocity, Mass
                CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTERFACES%INTBUF_TAB,
     *                NODES%X,NODES%V,NODES%MS,NODES%ITAB,XYZ,NUMNOD,SH_OFFSET_TAB%NNSH_OSET)

              ELSE
                ALLOCATE(IAD_I24(1,1))
              ENDIF

              CALL INIT_I25_EDGE(NLEDGE,NINTER,NPARI,IPARI,INTERFACES%INTBUF_TAB )
              IF(NINTER25 /= 0)THEN
                ALLOCATE(IAD_I25(NBINTC+1,NSPMD))
                CALL  SPMD_I25_PREPARE(1,IPARI, INTERFACES%INTBUF_TAB,
     *                                NODES%BOUNDARY_ADD, NODES%BOUNDARY,INTLIST,NBINTC,
     *                                IAD_I25 , SFR_I25, IDUM)

                ALLOCATE(FR_I25(SFR_I25))
                CALL  SPMD_I25_PREPARE(2,IPARI, INTERFACES%INTBUF_TAB,
     *                                NODES%BOUNDARY_ADD, NODES%BOUNDARY,INTLIST,NBINTC,
     *                                IAD_I25 , SFR_I25, FR_I25)

              ELSE
                ALLOCATE(IAD_I25(1,1))
              ENDIF
              ALLOCATE(FSKYN25 (3,NBCCNOR))
              FSKYN25=0.
C     Multidomains + SPH
              IF ((NUMSPH_GLO_R2R>0).AND.(FLG_SPHINOUT_R2R==1)) THEN
                ALLOCATE(OFF_SPH_R2R(NUMNOD))
                OFF_SPH_R2R(:) = 0
              ENDIF

C     THERMAL EXPANSION and IMPTEMP : Initialisation of nodal temperatures
              IF (GLOB_THERM%NFXTEMP  > 0 .AND. GLOB_THERM%ITHERM_FE > 0.AND.TT==ZERO) THEN
                CALL FIXTEMP(python,IBFTEMP  ,FBFTEMP    ,NODES%TEMP    ,NPC    ,TF  ,
     .               NSENSOR  ,SENSORS%SENSOR_TAB,GLOB_THERM,SNPC )
              ENDIF
C ---------------------
C Allocating Array for PCONT2 - average normal
C ---------------------
              IF (ANIM_V(27)+H3D_DATA%N_VECT_PCONT2 > 0) THEN
                SZ_NPCONT2 = NUMNOD
                ALLOCATE(NPCONT2(3,NUMNOD))
                NPCONT2 = ZERO
              ELSE
                SZ_NPCONT2 = 0
                ALLOCATE(NPCONT2(3,0))
              ENDIF
C ---------------------
C Allocating FEXT Array
C ---------------------
              IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .           ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0 .or. ViperCoupling) THEN
                ALLOCATE(NODA_FEXT(3*NUMNOD))
                NODA_FEXT(1:3*NUMNOD)=ZERO
              ELSE
                ALLOCATE(NODA_FEXT(3))
                NODA_FEXT(1:3)=ZERO
              ENDIF
C ---------------------
C Allocating PEXT Array
C ---------------------
              CALL OUTPUT_ALLOCATE_NODA_PEXT(OUTPUT%DATA,NUMNOD, NUMNODG)
C ---------------------
              IF (ANIM_V(19) + H3D_DATA%N_VECT_CLUST_FORCE > 0) THEN
                ALLOCATE(FCLUSTER(3*NUMNOD))
                FCLUSTER(1:3*NUMNOD)=ZERO
              ELSE
                ALLOCATE(FCLUSTER(3))
                FCLUSTER(1:3)=ZERO
              ENDIF
              IF (ANIM_V(20) + H3D_DATA%N_VECT_CLUST_MOM > 0) THEN
                ALLOCATE(MCLUSTER(3*NUMNOD))
                MCLUSTER(1:3*NUMNOD)=0
              ELSE
                ALLOCATE(MCLUSTER(3))
                MCLUSTER(1:3)=0
              ENDIF
              ! -------------------------
              ! initialization of SHOOT_STRUCT for
              ! the deactivation node algo
              CALL INIT_NODAL_STATE( IPARI,SHOOT_STRUCT,INTERFACES%INTBUF_TAB,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
     .                               NODES%ITAB,NODES,GEO,ADDCNEL,CNEL,
     .                               IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,
     .                               SIZE_ADDCNEL,SIZE_CNEL ,
     .                               numelsg,numelqg,numelcg,numeltrg,numelpg,
     .                               numelrg,numeltgg ,IXS(L1))
              ! -------------------------
              allocate(component(ninter))
              call inter_init_component(ninter,npari,numnod,ispmd,nspmd,ipari,nodes%x,interfaces%intbuf_tab,component)
              ! -------------------------
              ! initialization of SPH_WORK
              call ALLOCATE_SPH_WORK(SPH_WORK,
     *                               NUMSPH,NUMSPH,          ! Flag & size for WREDUCE
     *                               SOL2SPH_FLAG, NUMNOD,   ! Flag for other arrays
     *                               NSPHACT,NUMNOD,NSPHACT, ! Size of as6, a6, as
     *                               NUMSPHG )
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              CALL TRACE_OUT(6)
c-----------------------------------------------
c     INITIALISATION XFEM
c-----------------------------------------------
              IF (ICRACK3D > 0 .and. TT == ZERO) THEN
                CALL INIXFEM(ELBUF_TAB   ,XFEM_TAB   ,
     .            IPARG     ,ELEMENT%SHELL%IXC         ,IXTG       ,NGROUC      ,IGROUC       ,
     .            ELCUTC    ,IADC_CRK    ,IEL_CRK    ,INOD_CRK    ,ADSKY_CRK    ,
     .            NODES%X         ,KXFENOD2ELC ,NODEDGE    ,CRKNODIAD   ,IAD_EDGE     ,
     .            FR_EDGE   ,FR_NBEDGE   ,NODLEVXF   ,CRKEDGE     ,XEDGE4N      ,
     .            XEDGE3N   )
              END IF
C-----------------------------------------------
C       Allocations Phase 2
C-----------------------------------------------
              ALLOCATE(FTHREAC(6*CPTREAC))
              IF (IMPL_S==1) THEN
                ALLOCATE(FTHDTM(6*CPTREAC))
              ELSE
                ALLOCATE(FTHDTM(0))
              ENDIF
C
              ! OUTPUT (ANIM,OUTP,H3D,TH) set COMPTREAC TO 1 IF REQUESTED
              COMPTREAC = 0
              CALL REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT(NPBY,H3D_DATA,COMPTREAC) ! Look for options for anim, ... and rbody with failure
              IF(COMPTREAC == 1)THEN
                ALLOCATE(FREAC(6*NUMNOD))
                FREAC(1:6*NUMNOD)=ZERO
              ELSE
                ALLOCATE(FREAC(0))
              ENDIF
C
              FTHREAC = ZERO
              FTHDTM  = ZERO
              IF (NTHPART > 0) THEN
                ALLOCATE(GRESAV(NPSAV*NGPE*NTHREAD))
              ELSE
                ALLOCATE(GRESAV(1))
              ENDIF
              GRESAV = ZERO
C       ALLOCATION Flying Nodes if IDEL
              IF(IDEL7NG>0)THEN
                IERROR = 0
                ALLOCATE(IBUFIDEL(LBUFIDEL))
                ALLOCATE(INDIDEL(LINDIDEL))
                IF(INT24USE==1.OR.NINTER25/=0)THEN
                  ALLOCATE (IBUFSEGLO(LBUFSEGLO))
                  ALLOCATE (INDSEGLO(NINTER+1))
                ENDIF
              ELSE
                ALLOCATE(IBUFIDEL(0) )
                ALLOCATE(INDIDEL(0)  )
              END IF

              IF(NSPMD > 1.AND.PDEL > 0)  THEN
                ALLOCATE(IBUFPDEL(4*NCONLD+4*NPRESLOAD))
                ALLOCATE(NINDEXPDEL(NCONLD+NPRESLOAD))
              ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C------------IF adaptive Mesh : check if there are elements eroded after trimming to remove nodes from interface------
              IF(NADMESH/=0.AND.IDEL7NG>=1)THEN

                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
                IF((INT24USE==1.OR.NINTER25/=0).AND.IDEL7NOK==1)THEN
                  INDSEGLO(2:NINTER+1)=0
                  INDSEGLO(1)=1
                ENDIF
                CHECK_NEIGH_FLAG_RES = 0
                IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF
c         allocate(nodes%deleted_node(2*numnod)) ! working array to mark nodes connected to deleted element
c         allocate(nodes%work_array_node(nthread*numnod)) ! working array to mark nodes (connected to active element or deleted element)
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,N1,GREFTSK,GRELTSK,omp_address)
C Init var parallel SMP
                CALL SMP_INIT(
     1           ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2           IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)
                omp_address = 1 + itsk * numnod
                CALL TAGOFF3N(NODES,
     1             GEO        ,IXS       ,IXS(L1)   ,IXS(L1)   ,IXS(L3)  ,IXQ    ,
     2             ELEMENT%SHELL%IXC         ,IXT      ,IXP       ,IXR       ,IXTG     ,
     3             nodes%deleted_node,NODFTSK  ,NODLTSK   ,IPARG     ,ELBUF    ,ITSK   ,
     4             IXTG1    ,NODES%BOUNDARY_ADD  ,NODES%BOUNDARY   ,NODES%ITAB     ,
     5             ADDCNEL     ,CNEL     ,KXSP      ,ELBUF_TAB ,TAGEL    ,IEXLNK ,
     6             IGRNOD      ,DD_R2R   ,DD_R2R_ELEM,SDD_R2R_ELEM,IDEL7NOK_SAV ,
     7             IDEL7NOK_R2R,TAGTRIMC,TAGTRIMTG,S_ELEM_STATE,ELEM_STATE,
     8             SHOOT_STRUCT,SHOOT_STRUCT%GLOBAL_NB_ELEM_OFF)
                ! ---------------------
                ! check if a node is deactivated and deactivate all the corresponding secondary nodes
                CALL CHECK_NODAL_STATE( ITSK,nodes%deleted_node,NEWFRONT,INTERFACES%INTBUF_TAB,SHOOT_STRUCT%SIZE_SEC_NODE,
     .                 SHOOT_STRUCT%SHIFT_S_NODE,SHOOT_STRUCT%INTER_SEC_NODE,SHOOT_STRUCT%SEC_NODE_ID)
                ! ---------------------

                ! ---------------------
                ! check if a surface/edge must be deactivated and save the surface/edge id
                IF(ITSK==0) THEN
                  CALL FIND_SURFACE_INTER( NODES%ITAB  ,SHOOT_STRUCT  ,IXS  ,IXS(L1)  ,ELEMENT%SHELL%IXC  ,
     .                                     IXTG  ,
     .                                     NGROUP,NPARG,IGROUPS,IPARG )

                  CALL FIND_EDGE_INTER( NODES%ITAB,SHOOT_STRUCT,IXS,IXS(L1),
     1                                  ELEMENT%SHELL%IXC,IXTG,IXQ,IXT,IXP,
     2                                  IXR,GEO,NGROUP,IGROUPS,IPARG )
                ENDIF
                CALL MY_BARRIER( )
                ! ---------------------

                ! ---------------------
                ! exchange of surfaces (ie. 4 nodes) to deactivate and deactivation
                ! ONLY FOR LOCAL SURFACE / REMOTE ELEMENT
                IF(NSPMD>1) THEN
                  IF(ITSK==0) CALL SPMD_EXCH_DELETED_SURF_EDGE( NODES%BOUNDARY_ADD,NODES,SHOOT_STRUCT,
     .                                 INTERFACES%INTBUF_TAB,NEWFRONT,
     .                                 IPARI,GEO,
     .                                 IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                 ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL )
                  CALL MY_BARRIER()
                ENDIF
                ! ---------------------

                ! ---------------------
                ! loop over the surface id and deactivate the surface
                ! ONLY FOR LOCAL SURFACE / LOCAL ELEMENT
                CALL CHECK_SURFACE_STATE( ITSK,SHOOT_STRUCT%SAVE_SURFACE_NB,SHOOT_STRUCT%SAVE_SURFACE,
     .                                    SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,
     .                                    IPARI,GEO,
     .                                    IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                    ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL,SHOOT_STRUCT )

                ! loop over the edge id and deactivate the edge
                ! ONLY FOR LOCAL EDGE / LOCAL ELEMENT
                CALL CHECK_EDGE_STATE( ITSK,SHOOT_STRUCT%SAVE_M_EDGE_NB,SHOOT_STRUCT%SAVE_S_EDGE_NB,
     .                                 SHOOT_STRUCT%SAVE_M_EDGE,SHOOT_STRUCT%SAVE_S_EDGE,
     .                                 SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                                 IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                 ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL,SHOOT_STRUCT )
                ! ---------------------

                ! ---------------------
                ! exchange of deactivated surfaces (ie. 4 nodes) to deactivate to deactivate the neighbourhood
                ! ONLY FOR REMOTE SURFACE + interface type 24 or 25
                IF(INT24USE>0.OR.NINTER25/=0) THEN
                  IF(ITSK==0) CALL CHECK_REMOTE_SURFACE_STATE( SHOOT_STRUCT%NUMBER_REMOTE_SURF,SHOOT_STRUCT%REMOTE_SURF,
     .                                                         SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,
     .                                                         IPARI,NODES%BOUNDARY_ADD,SHOOT_STRUCT )
                  CALL MY_BARRIER()
                  IF(NINTER25/=0.AND.INTERFACES%PARAMETERS%INT25_EROSION_SOLID > 0) THEN
                    IF(ITSK==0) THEN
                      CHECK_NEIGH_FLAG = SHOOT_STRUCT%NUMBER_NEW_SURF + shoot_struct%NUMBER_REMOTE_SURF
                      IF(NEED_COMM_INT25_SOLID_EROSION(ISPMD+1)) THEN
                        CALL SPMD_ALLREDUCE(CHECK_NEIGH_FLAG,CHECK_NEIGH_FLAG_RES,1,SPMD_MAX,COMM_INT25_SOLID_EROSION)
                      ELSEIF(NSPMD==1) THEN
                        CHECK_NEIGH_FLAG_RES = CHECK_NEIGH_FLAG
                      ENDIF
                      IF(CHECK_NEIGH_FLAG_RES > 0 ) THEN
                        CALL GET_NEIGHBOUR_SURFACE( ISPMD,NSPMD,NINTER25,NPARI,NINTER,
     .                                                NBINTC,NIXS,NIXC,NIXTG,NUMNOD,
     .                                                NUMELS,NUMELC,NUMELTG,S_ELEM_STATE,
     .                                                NBDDEDGT,NBDDEDG_MAX,
     .                                                ELEM_STATE,IPARI,INTLIST,NODES,
     .                                                NEWFRONT,IXS,ELEMENT%SHELL%IXC,IXTG,
     .                                                NODES%BOUNDARY_ADD,ptrX,
     .                                                INTERFACES%INTBUF_TAB,INTERFACES%SPMD_ARRAYS,SHOOT_STRUCT,
     .                                                NEED_COMM_INT25_SOLID_EROSION )
                      ENDIF
                    ENDIF
                    CALL MY_BARRIER()
                  ENDIF
                ENDIF
                ! ---------------------

                CALL CHKSTFN3N(NODES,
     1             IPARI   ,GEO         ,IXS      ,IXQ         ,ELEMENT%SHELL%IXC     ,IXT       ,
     2             IXP     ,IXR         ,IXTG     ,nodes%deleted_node,IPARG   ,ITSK      ,
     3             NEWFRONT,nodes%work_array_node(omp_address) ,NODES%MS       ,NODES%IN ,OUTPUT%DATA%SCAL_DMAS,NODES%ITAB  ,
     4             NODES%ITABM1  ,ADDCNEL     , CNEL    ,INDIDEL     ,NINDEX1 ,NINDEX2   ,
     5             NINDEX3 ,NINDEX4     ,TAGEL    ,INT24USE    ,IBUFSEGLO ,INDSEGLO,
     6             IBUFIDEL ,INTERFACES%INTBUF_TAB,NODES%BOUNDARY_ADD)


!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                CALL DEALLOC_SHOOT_INTER( SHOOT_STRUCT )
              ENDIF


              ! TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
              IF(ISFEM >= 1) THEN
                S_SFEM_NODVAR = 2*NUMNOD
              ELSE
                S_SFEM_NODVAR = 1
              ENDIF
              ALLOCATE(SFEM_NODVAR(S_SFEM_NODVAR))
              ALLOCATE(SFEM_NODVAR_ALE(S_SFEM_NODVAR))

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------------------------------
C    SELECTIVE MASS SCALING
C----------------------------------------------------------
              IF(IDTMINS_INT /= 0)THEN
C
C           /DT/INTER/AMS
                CALL SMS_INI_ERR(RWALL%NPRW ,RWALL%LPRW ,NODES%KINET )
C
              END IF
C----------------------------------------------------------
              IF(IDTMINS /= 0)THEN
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20

C NATIV_SMS read from starter state
                NATIV_SMS(1:NUMNOD) = NATIV0_SMS(1:NUMNOD)
C
                CALL SMS_INI_PART(IGRPART ,TAGPRT_SMS)

                IF(IDTMINS_INT==0)
     .          CALL SMS_INI_RBY(
     1          NODES%KINET   ,RWALL%NPRW ,RWALL%LPRW ,NPBY    , LPBY     ,
     2          TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)

                CALL SMS_INI_KAD(
     1          IXS     ,IXQ      ,ELEMENT%SHELL%IXC     ,IXT     ,IXP       ,
     2          IXR     ,IXTG     ,IXTG1   ,IXS(L1) ,IXS(L3)   ,
     3          IXS(L2) ,IPARG    ,NODES%MS      ,NODES%MS0     ,TAGNOD_SMS,
     4          NODES%ICODT    ,NODES%ICODR   ,NODES%KINET   ,INDX1_SMS,
     5          KAD_SMS ,IPART(I15A),IPART(I15B),
     6          IPART(I15C),IPART(I15D),IPART(I15E),IPART(I15F),IPART(I15G),
     7          IPART(I15H),IPART(I15I),TAGPRT_SMS ,TAGREL_SMS ,NODES%ITAB       ,
     8          NODES%WEIGHT     ,IRBE2      ,RBE3%IRBE3      ,LRBE2      ,RBE3%LRBE3      ,
     9          NODES%BOUNDARY_ADD   ,NODES%BOUNDARY    ,RWALL%NPRW       ,RWALL%LPRW       ,IPART      ,
     A          IGEO       ,NATIV_SMS  )

                ALLOCATE(KDI_SMS(KNZ_SMS),PK_SMS(KNZ_SMS))
                NSGDONE=1
                CALL SMS_INI_KDI(
     2                 ELEMENT%SHELL%IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3                 IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,KAD_SMS ,
     4                 KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS ,
     5                 JADT_SMS ,JADP_SMS,
     6                 JADR_SMS,JADTG_SMS,INDX1_SMS,TAGPRT_SMS,IAD_SMS ,
     7           TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     8          IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     9                 NODES%BOUNDARY_ADD  ,NODES%BOUNDARY,NPBY     ,LPBY     ,NODES%KINET    ,
     A                 TAGSLV_RBY_SMS,IPARI  ,INTERFACES%INTBUF_TAB,IADI2,
     B                 LAD_SMS   ,IPART,IGEO     ,NODES%WEIGHT    ,
     C                 NATIV_SMS)

                ALLOCATE(IDI_SMS(NNZ_SMS),JDI_SMS(NNZ_SMS))

                NSGDONE=1
                CALL SMS_INI_JAD_1(
     2                 ELEMENT%SHELL%IXC      ,IPARG   ,IXS      ,IXT       ,IXP      ,
     3                 IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS ,
     4                 JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS ,JADR_SMS ,
     5                 JADTG_SMS,INDX1_SMS,TAGPRT_SMS,
     6                 KAD_SMS,KDI_SMS    ,PK_SMS     ,
     7           TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     8          IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     9                 NODES%BOUNDARY_ADD  ,NODES%BOUNDARY,NPBY     ,LPBY     ,NODES%KINET    ,
     A                 TAGSLV_RBY_SMS,IPARI  ,INTERFACES%INTBUF_TAB,IADI2,
     B                 LAD_SMS   ,IPART      ,IGEO      ,NODES%WEIGHT  ,NATIV_SMS,
     C                 IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS   ,T2MAIN_SMS)
                DEALLOCATE(JDI_SMS)
                ALLOCATE(JDI_SMS(NNZ_SMS))
                NSGDONE=1
                CALL SMS_INI_JAD_2(
     2                 ELEMENT%SHELL%IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3                 IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS ,
     4                 JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5                 JADTG_SMS,INDX1_SMS,TAGPRT_SMS,KAD_SMS ,KDI_SMS  ,
     6           TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     7          IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     8                 NODES%BOUNDARY_ADD  ,NODES%BOUNDARY,NPBY     ,LPBY     ,NODES%KINET    ,
     9                 TAGSLV_RBY_SMS,IPARI  ,INTERFACES%INTBUF_TAB,IADI2    ,
     A                 LAD_SMS   ,RWALL%NPRW ,RWALL%LPRW,TAGMSR_RBY_SMS,
     B             TAGSLV_I21_SMS ,TAGMSR_I21_SMS, JADI21_SMS,INTSTAMP  ,
     .                                                        IPART     ,
     C             IGEO    ,NODES%WEIGHT  ,NATIV_SMS,IRBE2      ,LRBE2     ,
     D             IAD_SMS ,IDI_SMS ,JAD_SMS  ,JDI_SMS    ,T2MAIN_SMS)
                DEALLOCATE(JDI_SMS)
                CALL MY_ALLOC(JDI_SMS,NNZ_SMS)
                CALL MY_ALLOC(JSM_SMS,NNZ_SMS)

                NSGDONE=1
                CALL SMS_INI_JAD_3(
     2                 ELEMENT%SHELL%IXC      ,IPARG   ,IXS      ,IXT      ,IXP      ,
     3                 IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS,
     4                 JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS ,
     5                 JADTG_SMS,INDX1_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     6           TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     7          IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     8                 NODES%BOUNDARY_ADD  ,NODES%BOUNDARY,NPBY     ,LPBY     ,NODES%KINET    ,
     9                 TAGSLV_RBY_SMS,IPARI  ,INTERFACES%INTBUF_TAB,IADI2    ,
     A                 LAD_SMS ,JSM_SMS ,TAGSLV_I21_SMS,INTSTAMP  ,IPART    ,
     B                 IGEO    ,TAGMSR_RBY_SMS,NODES%WEIGHT,NATIV_SMS,
     C                 IAD_SMS ,IDI_SMS ,JAD_SMS ,JDI_SMS    ,T2MAIN_SMS)

                DEALLOCATE(IAD_SMS,IDI_SMS)
                ALLOCATE(LTK_SMS(KNZ_SMS),LT_SMS(NNZ_SMS))
                ALLOCATE(TAG_LNK_SMS(NRLINK+NLINK+NJOINT),NRWL_SMS(SLPRW))

                IF(IDTMINS==1)THEN
C             Obsolete
                ELSE
                  CALL SMS_INI_KIN_2(
     1            ILINK     ,LLINK    ,NNLINK    ,LNLINK    ,TAG_LNK_SMS,
     2            FR_LL     ,FR_RL    ,NODES%WEIGHT    ,NODES%ITAB      ,LJOINT     ,
     3            IADCJ     ,FR_CJ    ,RWALL%NPRW      ,RWALL%LPRW      ,RWALL%FR_WALL    ,
     4            NRWL_SMS  ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY   )
                END IF

                IF(IDTMINS_INT/=0)THEN
C re-tag rigid bodies (all nodes)
                  CALL SMS_INI_RBY(
     1            NODES%KINET   ,RWALL%NPRW     ,RWALL%LPRW    ,NPBY    , LPBY     ,
     2            TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)
                END IF

                CALL SMS_INI_INT(
     1          IPARI    ,INTERFACES%INTBUF_TAB    ,NODES%BOUNDARY_ADD   ,NODES%BOUNDARY  ,INTLIST,
     2          NBINTC)

              ELSEIF(IDTMINS_INT/=0)THEN
C
C           /DT/INTER/AMS
                ALLOCATE(KDI_SMS(0),JDI_SMS(0),PK_SMS(0),LT_SMS(0),LTK_SMS(0),
     .                   JSM_SMS(0),
     .                   TAG_LNK_SMS(NRLINK+NLINK+NJOINT),NRWL_SMS(SLPRW))

                NINDX1_SMS=0
                NSMSPCG=MIN(NSMSPCG,NUMNODG)

                CALL SMS_INI_KIN_2(
     1          ILINK     ,LLINK    ,NNLINK    ,LNLINK    ,TAG_LNK_SMS,
     2          FR_LL     ,FR_RL    ,NODES%WEIGHT    ,NODES%ITAB      ,LJOINT     ,
     3          IADCJ     ,FR_CJ    ,RWALL%NPRW      ,RWALL%LPRW      ,RWALL%FR_WALL    ,
     4          NRWL_SMS  ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY   )

                CALL SMS_INI_RBY(
     1          NODES%KINET   ,RWALL%NPRW     ,RWALL%LPRW    ,NPBY    , LPBY     ,
     2          TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)

                CALL SMS_INI_INT(
     1          IPARI    ,INTERFACES%INTBUF_TAB   ,NODES%BOUNDARY_ADD   ,NODES%BOUNDARY  ,INTLIST,
     2          NBINTC)

              ELSE
                ALLOCATE(KDI_SMS(0),JDI_SMS(0),PK_SMS(0),LT_SMS(0),LTK_SMS(0),
     .                   JSM_SMS(0),TAG_LNK_SMS(0),NRWL_SMS(0))
              END IF

C
              IF (M_VS_SMS>0) THEN
                LNZM=M_VS_SMS+3
                ALLOCATE(PROJ_S(NUMNOD,LNZM),PROJ_T(NUMNOD,LNZM),
     .                   PROJ_LA_1(LNZM),PROJ_W(3*LNZM),PROJ_K(LNZM,LNZM))
                PROJ_S = ZERO
                NCG_RUN_SMS = 0
              ELSE
                ALLOCATE(PROJ_S(0,0),PROJ_T(0,0),PROJ_LA_1(0),PROJ_W(0),PROJ_K(0,0))
              END IF
C---------------------------------------------------------------------
C DEBUG MATRIX AMS
C---------------------------------------------------------------------
              IF (IDTMINS/=0.AND.DEBUG(MACRO_DEBUG_AMS)==1) THEN
                IF(NSPMD > 1) THEN
                  IF (ISPMD==0) THEN
                    SIZ = NUMNODG
                  ELSE
                    SIZ = 0
                  END IF
                  CALL SPMD_COLLECTM(TAGNOD_SMS,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB,SIZ)
                ELSE
                  CALL COLLECTM(TAGNOD_SMS,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB)
                END IF
              END IF
C---------------------------------------------------------------------
C AMS + POFF
C---------------------------------------------------------------------
              IF(IDTMINS/=0.AND.IPARIT==0)THEN
                ALLOCATE(UNCOMP_FR(NUMNOD))
                ALLOCATE(UNCOMP_FRI2M(NUMNOD))

                CALL AMS_PREPARE_POFF_ASSEMBLY(
     *                     NODES%BOUNDARY_ADD,NODES%BOUNDARY,NB_FR,UNCOMP_FR,
     *                      IAD_I2M,FR_I2M,NB_FRI2M,UNCOMP_FRI2M)

                ALLOCATE(FR_LOC(NB_FR))
                FR_LOC(1:NB_FR) = UNCOMP_FR(1:NB_FR)

                ALLOCATE(FR_LOCI2M(NB_FRI2M))
                FR_LOCI2M(1:NB_FRI2M) = UNCOMP_FRI2M(1:NB_FRI2M)


                DEALLOCATE(UNCOMP_FR)
                DEALLOCATE(UNCOMP_FRI2M)

              ELSE
                ALLOCATE(FR_LOC(1))
                ALLOCATE(FR_LOCI2M(1))
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C ALLOCATION OF WORKING ARRAYS FOR AIRBAG BEM
C
              ALLOCATE(WIBEM(LWIBEM), WRBEM(LWRBEM))
              DO I=1,LWIBEM
                WIBEM(I)=0
              ENDDO
              ALLOCATE(WIFLOW(LWIFLOW), WRFLOW(LWRFLOW))
              DO I=1,LWIFLOW
                WIFLOW(I)=0
              ENDDO
C
C         Allocation interface 17
C
              ALLOCATE(EMINX(NME17*6))
C
              IF (NFXBODY>0) THEN
                DO N=1,NFXBODY
                  ADRNOD=FXBIPM(6,N)
                  ONOF=1
                  ONFELT=0
                  CALL FXBYPID(OUTPUT,
     .                IPARG         ,         IXS , IXQ    , ELEMENT%SHELL%IXC        ,
     .                IXT           , IXP   , IXR , IXTG   , FXBIPM(1,N),
     .                FXBNOD(ADRNOD), ONOF  , WA  , ONFELT ,ELBUF_TAB   )
                ENDDO
              END IF
C

C       dt2s=0 => shells computation at 1st cycle next to restart
              IF(MCHECK==0) DT2S=0.0
              IF(MCHECK/=0) DT2SAVE = DT2
C------------RBE3----allocation-------------
              IF(NRBE3>0)THEN
                NMT0 = RBE3%lrbe3_sz/2
                IF (NMT0>0) THEN
                  CALL PRERBE3P0(RBE3)
                END IF
              END IF
C----------------------------------
C       IMPLICIT SIZES [MONO THREAD]
C       Double precision only
C----------------------------------
              IF (IMPL_S==1.OR.NEIG>0) THEN
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
                ALLOCATE (ELBUF_IMP(NGROUP))
                CALL ALLOC_ELBUF_IMP(ELBUF_TAB,ELBUF_IMP,NGROUP, IPARG)
                IF (IMON>0) CALL STARTIME(TIMERS,34)
                IF (IMON>0) CALL STARTIME(TIMERS,31)
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
                CALL IMP_SOL_INIT(
     1          GEO       ,NPBY      ,LPBY      ,NODES%ITAB      ,
     2          IPARI     ,IXS       ,IXQ       ,ELEMENT%SHELL%IXC       ,IXT       ,
     4          IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     5          IXS(L2)   ,IXS(L3)   ,IPARG     ,
     6          ELBUF     ,NINT7     ,NBINTC    ,NODES%X         ,DMCP      ,
     7          NODES%BOUNDARY   ,NODES%BOUNDARY_ADD  ,FR_I2M    ,IAD_I2M   ,
     8          RWALL%NPRW      ,NUM_IMP1  ,NUM_IMPL  ,MONVOL    ,IGRSURF   ,
     9          FR_MV     ,IPM       ,IGEO      ,IAD_RBY   ,
     A          FR_RBY    ,SH4TREE   ,SH3TREE   ,RBE3%IRBE3     ,RBE3%LRBE3     ,
     B          RBE3%mpi%FR_RBE3  ,RBE3%mpi%IAD_RBE3 ,IRBE2     ,LRBE2     ,IBFV      ,
     C          VEL       ,ELBUF_TAB ,IFRAME    ,INTERFACES%INTBUF_TAB,
     D          NDDL0     ,NNZK0     ,IMPBUF_TAB)
                IF (IMON>0) CALL STOPTIME(TIMERS,31)
                IF (IMON>0) CALL STOPTIME(TIMERS,34)
C
                NS_IMP=>IMPBUF_TAB%CAND_N
                NE_IMP=>IMPBUF_TAB%CAND_E
                IND_IMP=>IMPBUF_TAB%INDSUBT
                FEXT_IMP=>IMPBUF_TAB%AC
                R_IMP=>IMPBUF_TAB%R_IMP
                ALLOCATE(FAC_K(0), IPIV_K(0))
                IF (IMUMPSV >0.OR.(ISOLV==7.AND.NSPMD>1)) THEN
#if defined(MUMPS5)
                  CALL SPMD_MUMPS_INI(MUMPS_PAR, 1)
#else
                  WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
                  CALL FLUSH(6)
                  CALL ARRET(5)
#endif
                  ALLOCATE(CDDLP(NDDL0))
                ENDIF
                IBUCK= IBUCKL
C----------------------------------
C       EIGENSOLVER  [MONO THREAD]
C----------------------------------
C------ one routine for EIG
                IF (NEIG>0) THEN
                  CALL TRACE_IN(3,NCYCLE,ZERO)
#ifdef DNC
                  CALL IMP_EIGSOL(
     1                            EIGIPM      , EIGRPM      , NODES%MS        ,NODES%IN         , EIGIBUF   ,
     2                            NODES%X           ,IXTG1        ,TF         , NPC       , FR_WAVE   ,
     3                            W16         , WA        ,
     4                            NODES%ICODT       , NODES%ICODR       , NODES%ISKEW     ,IBFV       , VEL       ,
     4                            NODES%V           , NODES%A           , ELBUF    , IXS       , IXQ     ,
     5                            ELEMENT%SHELL%IXC         , IXT         , IXP      , IXR       , IXTG    ,
     6                            PM          , GEO         ,OUTPUT%DATA%VECT_CONT     , ICUT      , SKEWS%SKEW    ,
     7                            XCUT        ,OUTPUT%DATA%VECT_FINT, NODES%ITAB    ,OUTPUT%DATA%VECT_FEXT,OUTPUT%DATA%FOPT,
     8                            OUTPUT%DATA%SCAL_DT, LPBY        , NPBY     , NSTRF     , RWALL%RWBUF   ,
     9                            RWALL%NPRW        , TANI        , ELBUF_TAB ,MAT_ELEM%MAT_PARAM, DD_IAD  ,
     A                            NODES%BOUNDARY_ADD    , NODES%BOUNDARY     , NODES%WEIGHT   , EANI      , IPART   ,
     B                            RBY         , NOM_OPT     , IGRSURF  ,
     C                            BUFSF       , IDATA       , RDATA    , BUFMAT    , BUFGEO  ,
     D                            KXX         , IXX         , KXSP     , IXSP      , NOD2SP  ,
     E                            SPBUF       , IXS(L1)     , IXS(L2)  , IXS(L3)   , NODES%VR      ,
     F                            MONVOL      , VOLMON      , IPM      , IGEO      , IPARG   ,
     G                            NODES%NODGLOB     , NODES%BOUNDARY_ADD    , NODES%BOUNDARY  , FR_SEC    , FR_RBY2 ,
     H                            IAD_RBY2    , RWALL%FR_WALL     ,  IPARI   ,
     I                            INTERFACES%INTBUF_TAB  , NODES%D           ,PARTSAV   ,
     J                            FSAV(1,NFNCA+1),FSAV(1,NFTCA+1), NODES%TEMP      , THKE    ,
     K                            ERR_THK_SH4 , ERR_THK_SH3 , IRBE2    , RBE3%IRBE3     ,LRBE2    ,
     L                            RBE3%LRBE3       , RBE3%FRBE3    ,FR_RBE2  , RBE3%mpi%fr_rbe3  , IAD_RBE2,
     M                            NODES%WEIGHT_MD   , CLUSTER     , FCLUSTER , MCLUSTER  , XFEM_TAB,
     O                            W           , ale%global%nv46   , NERCVOIS  , NESDVOIS,
     P                            LERCVOIS    , LESDVOIS    ,CRKEDGE   , INDX_CRK  , XEDGE4N ,
     Q                            XEDGE3N     ,STACK        ,SPH2SOL   ,NODES%STIFN      ,NODES%STIFR    ,
     R                            DRAPE_SH4N    , DRAPE_SH3N    ,H3D_DATA  ,SUBSETS    ,IGRNOD   ,
     S                            FCONT_MAX   ,OUTPUT%DATA%VECT_PCONT2,OUTPUT%DATA%VECT_PCONT2_2, ALE_CONNECTIVITY ,
     T                            ITASK       ,NDDL0        ,NNZK0     ,IMPBUF_TAB , DRAPEG,
     U                            GLOB_THERM, OUTPUT      ,MULTI_FVM)
#endif
                  DT2 = MAX(EM20,TSTOP-TT)
                  MSTOP=2
                  GOTO 300
                END IF !IF (NEIG>0) THEN
#endif
              ELSE
                IMPL_S0 = 0
                IMP_DUM(1)=0
                NS_IMP => IMP_DUM
                NE_IMP => IMP_DUM
                IND_IMP=> IMP_DUM
                NUM_IMP=0
                ISMDISP=0
              END IF !IF (IMPL_S==1.OR.NEIG>0) THEN
C------------------------------------------------------
C  End of Implicit [MONO THREAD]
C------------------------------------------------------
C------------------------------------------------------
C  other initializations [MONO THREAD]
C------------------------------------------------------
              ONOFP=0
              IAD1 = 0
              IAD2 = 0
              IAD1B= 0
              IF (IPARIT>0) THEN
                IAD1 = 1
                IF(IVECTOR==1)THEN
                  IAD1B = IAD1+NUMNOD+1
                  IAD2 = IAD1B+NUMNOD
                ELSE
                  IAD1B = IAD1
                  IAD2 = IAD1B+NUMNOD+1
                ENDIF
              ENDIF
              IF(NUMSPH/=0)THEN
                IF(SCODVER>=44.AND.SMINVER<3)
     .           CALL SPHRES44B(KXSP ,IXSP ,NOD2SP ,IPARG ,SPBUF )
              ENDIF
C------------------------------------------------------
C SPH Pointers
C------------------------------------------------------
              KSPH1 =1+3*NUMSPH
              KSPH21 =NUMSPH+1
              KSPH22 =KSPH21+16*NUMSPH
              KSPH23 =KSPH22+MIN(1,NSPHIO)*3*NUMSPH
              KSPACTIV=1
              IF(NSPHSOL==0)THEN
C     pointer to list of particles to be sorted (to be sorted == active)
                KSP2SORT=1
              ELSE
C     to be sorted > active
                KSP2SORT=KSPH23
              END IF

C-------------------------------
C       POINTERS FOR USER ROUTINES
C-------------------------------
              CALL SAV_BUF_POINT(PM,1)
              CALL SAV_BUF_POINT(BUFMAT,2)
              CALL SAV_BUF_POINT(GEO,3)
              CALL SAV_BUF_POINT(BUFGEO,4)
              CALL SAV_BUF_POINT(NPC,5)
              CALL SAV_BUF_POINT(TF,6)
              CALL SAV_BUF_POINT(ISKWN,9)
              CALL SAV_BUF_POINT(SKEWS%SKEW,10)
              CALL SAV_BUF_POINT(LACCELM,11)
              CALL SAV_BUF_POINT(ACCELM,12)
              CALL SAV_BUF_POINT(NODES%ITABM1,13)
              CALL SAV_BUF_POINT(NODES%X,14)
              CALL SAV_BUF_POINT(NODES%D,15)
              CALL SAV_BUF_POINT(NODES%V,16)
              CALL SAV_BUF_POINT(NODES%A,17)
              CALL SAV_BUF_POINT(NODES%V,16)
              CALL SAV_BUF_POINT(NODES%A,17)
              CALL SAV_BUF_POINT(NODES%WEIGHT,18)
              CALL SAV_BUF_POINT(IPM,19)
              CALL SAV_BUF_POINT(IGEO,20)

C ----------------------------------------------
C Specific Initializations
C ----------------------------------------------

#ifdef DNC
              MADYMO_DEL_GLOBAL=0
              MADYMO_DEL=0
              IF(IMADCPL==1)THEN

                ALLOCATE(MAD_TAG_SOL(NUMELS))
                ALLOCATE(MAD_TAG_SH (NUMELC))
                ALLOCATE(MAD_TAG_TG (NUMELTG))
                ALLOCATE(MAD_FAIL_ELEMENTS (NMADSOL+NMADSH4+NMADSH3))
                MAD_TAG_SOL(1:NUMELS)=0
                MAD_TAG_SH(1:NUMELC)=0
                MAD_TAG_TG(1:NUMELTG)=0
                MAD_FAIL_ELEMENTS(1:NMADSOL+NMADSH4+NMADSH3) = 0

                ALLOCATE(MADCLFRECV(3,MADCLNODS))

                CALL INITIAL_DATA_EXCH_MADCPL(NODES%X,NODES%A,NODES%V,NODES%MS,MADCLNOD)

C Implementing a dummy Madymo cycle
                CALL DUMMY_CYCLE_MADCPL(NODES%X,MADCLNOD)

              ENDIF
#endif

              IF(NINTER25 /= 0) THEN
                CALL SPMD_I25FRONT_INIT(NODES%ITAB,NODES%MAIN_PROC,INTERFACES%INTBUF_TAB,IPARI)
              ELSE
                NINTER25E = 0
              ENDIF

              NFVBAG = 0
              CHECK_NPOLH = .FALSE.
              IF(NVOLU>0)THEN
                CALL FVDIM(MONVOL)
                CALL FV_SWITCH_CRIT(MONVOL,CHECK_NPOLH)
              ENDIF
C Save the number of FVMBAG before switches to UP
              NFVBAG0 = NFVBAG


              IF( NINTER /= 0 ) THEN
C Bucket or voxel, depending on /PERF/ in 1.rad
                CALL INIT_INTERF_SORTING_STRATEGY(INTERFACES%INTBUF_TAB,NINTER)
C First: try with reduced bounding box
                CALL INIT_TRIM(NINTER)
              ENDIF

              ! ----------------------
              ! user library : initialization
              IF(DLIB_STRUCT(ID_ENGINE_USER_INITIALIZE)%DLIB_BOOL) THEN
                NSPMD_USER = NSPMD
                NTASK_USER = NTHREAD
                ISPMD_USER = ISPMD
                CALL ENGINE_USER_INITIALIZE(NSPMD_USER,NTASK_USER,ISPMD_USER)
              ENDIF
              ! ----------------------
              CALL PYTHON_UPDATE_TIME(TT,DT2)
              CALL PYTHON_UPDATE_NODAL_ENTITIES(NUMNOD, NODES, X=NODES%X,A=NODES%A,V=NODES%V,D=NODES%D,DR=NODES%DR,VR=NODES%VR)
              CALL PYTHON_SYNC(PYTHON%CONTEXT)
              IF(NVOLU > 0)THEN
                IF(PYTHON%NB_FUNCTS > 0) THEN
                  K1 = 1
                  KK1 = 0
                  DO I=1,NVOLU
                    T_MONVOL(I)%pressure = VOLMON(KK1+12)
                    T_MONVOL(I)%temperature = VOLMON(KK1+13)
                    T_MONVOL(I)%area = VOLMON(KK1+18)
                    T_MONVOL(I)%volume = VOLMON(KK1+16)
                    K1  = K1  + NIMV
                    KK1 = KK1 + NRVOLU
                  END DO
                   CALL PYTHON_MONVOL(T_MONVOL)
                ENDIF
              ENDIF
C

              IF(coupling%active) THEN
              ! ALLOCATION AND INITIALIZATION OF COUPLING COUPLING
                ALLOCATE(NODES%FORCES(3,NUMNOD))
                NODES%FORCES = ZERO
                call coupling_set_interface(coupling, igrnod, ngrnod, igrsurf, nsurf,  nodes)
                CALL COUPLING_INITIALIZE(coupling,NODES%X,NUMNOD,ISPMD,NSPMD)
                CALL COUPLING_ONGOING(coupling,ongoing)
              ENDIF

              ! Node Splitting
              CALL init_ghost_shells(nodes, element,ispmd,nspmd,nodes%boundary_add,nodes%boundary_size,nodes%boundary)

              ! ----------------------
              ! Initialize coupling to Viper
              IF (ViperCoupling) THEN
                 call viper_coupling_initialize(VIPER, NODES, ELEMENT, NUMNOD,
     .                                          NIXS, NUMELS, IXS, NIXC, NUMELC,NIXTG, NUMELTG,IXTG,
     .                                          ISTDO, NUMELQ, NUMELT, NUMELP, NUMELR,
     .                                          TSTOP, OUTPUT%DTANIM, TT, NPARG, NGROUP, IPARG, ELBUF_TAB,
     .                                          TT_DOUBLE, OUTPUT%TANIM, DTMIN)
              ENDIF

C===========================================================================
C                 BEGINNING OF EXPLICIT ITERATION LOOP
C===========================================================================
 100          CONTINUE

              NCYCLE_DEBUG = NCYCLE
              IF (ViperCoupling) THEN
                print*, "Radioss: start of loop with time ",TT
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C INTERFACE
C Reallocate RENUM array
              RNS = 0
              CALL RENUM_SIZ(IPARI,RNS)
              IF (RNS > RNUM_SIZ)THEN
                DEALLOCATE(RENUM)
                RNUM_SIZ=RNS
                ALLOCATE(RENUM(RNUM_SIZ))
              ENDIF

#ifdef DNC
              IF (IMADCPL>0)THEN

                CALL DATA_SEND_MADCPL(NODES%X,MADCLNOD,
     *                                MADYMO_DEL_GLOBAL,MAD_FAIL_ELEMENTS)
              ENDIF
#endif

C-----------------------------------------------
C     TRACE BACK
C-----------------------------------------------
              CALL TRACE_IN(3,NCYCLE,ZERO)

              IF(IMON>0) CALL STARTIME(TIMERS,6)
              IF(IMONM > 0) CALL STARTIME(TIMERS,47)
C
              CALL MANCTR(OUTPUT,SENSORS,H3D_DATA)
C----------------------------
C       MOVING SKEW  [MONO THREAD]
C----------------------------------
              IF(NUMSKW/=0) CALL NEWSKW(SKEWS%SKEW    ,ISKWN        ,NODES%X           ,ISKWP_L  ,NSKWP,
     1                                  NUMSKW_L,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT,ISKWP)
C----------------------------------
              ECONT=ZERO
              EDAMP=ZERO
              ENINT=ZERO
              XMASS=ZERO
              XMOMT=ZERO
              YMOMT=ZERO
              ZMOMT=ZERO
              WPLAST=ZERO
C
              DT1=DT2
              DT2=EP06
              GLOB_THERM%DT_THERM = EP06
              NT_IMP=0
!
              IF(IMPL_S>0) THEN
                IF(NCYCLE>0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)
                  CALL IMP_FANIE(OUTPUT ,FEXT_IMP,NFIA  ,NFEA  ,NODFT ,NODLT, H3D_DATA)
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)
                ENDIF
              END IF


              IF(NCYCLE==1.AND.INTERFACES%PARAMETERS%ISTIF_DT>0)
     .                         INTERFACES%PARAMETERS%DT_STIFINT = DT1
C----------------------------------
              IF(IMONM > 0) CALL STOPTIME(TIMERS,47)
              IF(IMON>0) CALL STOPTIME(TIMERS,6)

              IMADFSH4=0

C----------------------------------
C       Gather actual thickness of shells
C----------------------------------
              IF(INTER_ITHKNOD/=0)THEN
                NSGDONE=1
                THKNOD(NODFT:NODLT)=ZERO
C       /---------------/
                CALL THICKVAR(IPARG,ELBUF_TAB,ELEMENT%SHELL%IXC,IXTG,THKSH4,
     .                        THKSH3,THKNOD,THKE,SH4TREE,SH3TREE)

                IF(NSPMD>1) THEN
                  LENGTH = 1
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  CALL SPMD_EXCH_THKNOD(
     +              THKNOD,NODES%BOUNDARY_ADD ,NODES%BOUNDARY,LENGTH,LENR)
                ENDIF
              END IF

C-----------------------------
C    Not pure thermal case
C-----------------------------

              IF(ILAG+IALE+IEULER/=0)THEN
C--- // N/3 -------------------------------------
C         CANCEL Mass matrix in 2D
C------------------------------------------------
                ISYNC = 0
                IF(N2D/=0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
                  CALL SMP_INIT(
     1              ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2              IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3            GREFTSK,GRELTSK)
                  CALL ZERO1(NODES%MS(NODFTSK),NUMNTSK)
!$OMP END PARALLEL

                ENDIF
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
              IF(ALLOCATED(OUTPUT%DATA%FOPT)) THEN
                OUTPUT%DATA%FOPT = ZERO
              ENDIF
              IF(ALLOCATED(OUTPUT%DATA%VECT_CONT2)) THEN
                OUTPUT%DATA%VECT_CONT2 = ZERO
              ENDIF
              IF(H3D_DATA%N_VECT_CONT2M==1)THEN
                DO I=1,NUMNOD
                  MCONT2(1,I) = ZERO
                  MCONT2(2,I) = ZERO
                  MCONT2(3,I) = ZERO
                ENDDO
              ENDIF
              NGDONE = 1
              NSGDONE = 1
C------------------------
C       INTERFACES 14 & 15 :
C         Initialisation buffers: forces, moments, stifness of surfaces.
C--- //0 ----------------
              IF (ISPMD==0) THEN
                IF (NINTER/=0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTSORT)
                  CALL I14IST(IPARI,INTERFACES%INTBUF_TAB,IGRSURF,BUFSF)
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                ENDIF
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------------
              DT2T    = DT2
C--------------------------------------------------------
              K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
              K2=K1+NUMELS
              K3=K2+NUMELQ
              K4=K3+NUMELC
              K5=K4+NUMELT
              K6=K5+NUMELP
              K7=K6+NUMELR
              K8=K7
              K9=K8+NUMELTG
              K10=K9+NUMELX
              K11=K10+NUMSPH
              K12=K11+NUMELIG3D
C-----------------------------------------------
C      pressure gauges, position calculation
C-----------------------------------------------
              IF (IALE+IEULER+GLOB_THERM%ITHERM+NUMSPH/=0) THEN
                IF(NBGAUGE/=0)CALL AGAUGE0(LGAUGE,GAUGE,NODES%X,ELEMENT%SHELL%IXC,IGAUP,NGAUP)
              END IF
C----------------------------------------
C
              IF (IMON>0) CALL STARTIME(TIMERS,6)
              IF (IMONM > 0) CALL STARTIME(TIMERS,49)
C----------------------------
              IF(NUMGEO>0.AND.NODADT==0)THEN
                DO I=1,NUMGEO
                  IF(GEO(5,I)>ZERO.AND.DTFAC1(3)*GEO(5,I)<DT2T)THEN
                    DT2T= DTFAC1(3)*GEO(5,I)
                    NELTST = 0
                    ITYPTST= 3
                  ENDIF
                ENDDO
              ENDIF
C------------------------
C       USER WINDOW
C------------------------
              IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN
                CALL TRACE_IN(9,2,ZERO)

                CALL USER_WINDOWS_ROUTINE( ISPMD        ,NSPMD         ,USERL_AVAIL      ,
     1                                     USER_WINDOWS ,RAD_INPUTNAME ,LEN_RAD_INPUTNAME,
     2                                     NUMNOD       ,NCYCLE        ,NODES%ITAB       ,
     3                                     TT           ,DT1           ,OUTPUT%TH%WFEXT     ,
     4                                     NODES%D      ,NODES%X       ,NODES%V          ,
     5                                     NODES%VR     ,NODES%MS      ,NODES%IN         ,
     6                                     NODES%STIFN  ,NODES%STIFR   ,NODES%A          ,
     7                                     NODES%AR     ,DT2)

                CALL TRACE_OUT(9)

              ENDIF
C----------------------------------
C       FUNCTIONS
C----------------------------------

              IF(NFUNCT /= 0.AND.IALE+IEULER+GLOB_THERM%ITHERM+NEBCS>0) THEN
                CALL TIMFUN(PYTHON,FV, NPC, TF)
                IF(EBCS_TAB%nebcs_loc/=0) THEN
                  !!! Need to "extrapolate values" whenever current time
                  !!! is lower than minimum defined time function or greater than
                  !!! maximum defined time function
                  CALL EBCS_EXTRAPOL(FV, NPC, TF, EBCS_TAB)
                ENDIF
              ENDIF
C----------------------------------
c     /STOP/LSENSOR
C-----------------------------------------------
              CALL STOP_SENSOR(SENSORS,H3D_DATA,DYNAIN_DATA,OUTPUT)
C
C-------------------------------------------------------------------
C       ACTIVATION-DEACTIVATION of groups of elements
C-------------------------------------------------------------------
              IF (NACTIV>0) THEN
                IF(GLOB_THERM%ITHERM_FE > 0 .AND. NSPMD > 1 ) THEN
                  DO I = 1,NUMNOD
                    NODES%MCP(I)   = NODES%MCP(I)   * NODES%WEIGHT(I)
                    NODES%STIFN(I) = NODES%STIFN(I) * NODES%WEIGHT(I)
                  ENDDO
                ENDIF
                CALL DESACTI(IXS     ,IXQ     ,ELEMENT%SHELL%IXC     ,IXP     ,IXT      ,
     .                       IXR     ,IXTG    ,IPARG   ,IACTIV  ,
     .                       NSENSOR ,SENSORS%SENSOR_TAB,ELEMENT%PON%FSKY    ,NODES%X       ,ELBUF_TAB,
     .                       IBCV    ,FCONV   ,IBCR    ,FRADIA  ,IGROUPS  ,
     .                       FACTIV  ,NODES%TEMP     ,NODES%MCP     ,PM      ,MCP_OFF  ,
     .                       IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N ,IGRTRUSS ,
     .                       IGRBEAM ,IGRSPRING,GLOB_THERM)
              ELSE
                IF(GLOB_THERM%ITHERM_FE > 0 .AND. NSPMD > 1 .AND. IPARIT == 0) THEN
                  DO I = 1,NUMNOD
                    NODES%MCP(I)   = NODES%MCP(I)   * NODES%WEIGHT(I)
                  ENDDO
                ENDIF
              ENDIF
              IF (IMONM > 0) CALL STOPTIME(TIMERS,49)

C-------------------------------------------------------------------
C       EXTERNAL FORCES
C-------------------------------------------------------------------
              IF (NCONLD/=0 .AND. IMPL_S/=1) THEN
                CALL TRACE_IN(10,0,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                CALL FORCE(
     1                     NIBCLD  ,IBCL     ,LFACCLD  ,FORC     ,SNPC                      ,
     2                     NPC     ,STF      ,TF       ,NODES%A        ,NODES%V                         ,
     3                     NODES%X       ,SKEWS    ,NODES%AR                        ,
     4                     NODES%VR      ,NSENSOR  ,SENSORS%SENSOR_TAB ,WFEXC          ,ELEMENT%PON%IAD_CONLD ,
     5                     LSKY    ,ELEMENT%PON%FSKY     ,NODA_FEXT     ,H3D_DATA ,CPTREAC                   ,
     6                     FTHREAC ,NODREAC  ,OUTPUT%TH%TH_SURF  ,
     7                     DPL0CLD ,VEL0CLD  ,NODES%D        ,NODES%DR       ,NCONLD                    ,
     8                     NUMNOD  ,NFUNCT   ,ANIM_V   ,OUTP_V                    ,
     9                     IPARIT  ,TT       ,DT1      ,N2D      ,OUTPUT%TH%WFEXT    ,
     A                     IMPL_S  ,PYTHON=PYTHON, NODES=NODES)
C
                IF (NPINCH > 0) THEN
                  CALL FORCEPINCH(IBCL   ,FORC    ,NPC    ,TF      ,NODES%A         ,
     2                            NODES%V      ,NODES%X       ,SKEWS%SKEW   ,NODES%AR      ,NODES%VR        ,
     3                            NSENSOR,SENSORS%SENSOR_TAB ,NODES%WEIGHT ,WFEXC   ,ELEMENT%PON%IAD_CONLD,
     4                            ELEMENT%PON%FSKY   , ELEMENT%PON%FSKY   ,NODA_FEXT   ,H3D_DATA,
     5                            PINCH_DATA%APINCH, PINCH_DATA%VPINCH, PYTHON, OUTPUT%TH%WFEXT)
                ENDIF
C
                IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                CALL TRACE_OUT(10)
              ENDIF
C
              IF(NFXVEL/=0.AND.IMPL_S/=1) THEN
                IF(IMON>0) THEN
                  CALL STARTIME(TIMERS,6)
                  CALL STARTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STARTIME(TIMERS,44)
                ENDIF
                CALL FORCEFINGEO(IBFV ,NPC    ,TF      ,NODES%A    ,NODES%V    ,NODES%X     ,
     2                           VEL  ,SENSORS%SENSOR_TAB ,ELEMENT%PON%FSKY ,NODA_FEXT ,NODES%ITABM1,
     3                           H3D_DATA,NSENSOR,PYTHON,OUTPUT%TH%WFEXT,NODES)
                IF(IMON>0) THEN
                  IF(IMONM > 0) CALL STOPTIME(TIMERS,44)
                  CALL STOPTIME(TIMERS,TIMER_KIN)
                  CALL STOPTIME(TIMERS,6)
                ENDIF
              ENDIF
C-------------------------------------------------------------------
C       LOAD PFLUID
C-------------------------------------------------------------------
              IF(NLOADP_F/=0.AND.IMPL_S/=1) THEN
                CALL TRACE_IN(10,0,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,41)
               CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
                CALL PFLUID(ILOADP  ,LOADP             ,NPC        ,TF          ,NODES%A               ,
     2                      NODES%V       ,NODES%X                 ,XFRAME      ,NODES%MS              ,
     3                      NSENSOR ,SENSORS%SENSOR_TAB,WFEXC      ,OUTPUT%TH%WFEXT,ELEMENT%PON%IAD_LOADP ,
     4                      ELEMENT%PON%FSKY , ELEMENT%PON%FSKY                 ,LLOADP                ,NODA_FEXT   ,H3D_DATA  ,
     5                      OUTPUT%TH%TH_SURF, PYTHON)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                CALL TRACE_OUT(10)
              ENDIF

C-------------------------------------------------------------------
C       LOAD PBLAST
C----------------------------------
              IF(PBLAST%NLOADP_B/=0.AND.IMPL_S/=1) THEN
                CALL TRACE_IN(10,0,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                CALL PBLAST_LOAD_COMPUTATION(OUTPUT,
     1         PBLAST,ILOADP,LOADP,NODES%A      ,NODES%V   ,NODES%X,
     2        ELEMENT%PON%IAD_LOADP ,ELEMENT%PON%FSKY ,LLOADP,NODA_FEXT ,OUTPUT%DATA%NODA_SURF ,OUTPUT%DATA%NODA_PEXT,
     3         NODES%ITAB,H3D_DATA, OUTPUT%TH%TH_SURF ,OUTPUT%TH%WFEXT)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                CALL TRACE_OUT(10)
                IF(PBLAST%PBLAST_DT%DT<DT2T)THEN
                  !inter22 kinematic time step
                  DT2T         = PBLAST%PBLAST_DT%DT
                  ITYPTST      = 12
                  NELTST       = PBLAST%PBLAST_DT%IDT
                  PBLAST%PBLAST_DT%DT = EP20
                ENDIF
              ENDIF
C-------------------------------------------------------------------
C       LOAD PCYL
C----------------------------------
              IF (LOADS%NLOAD_CYL > 0) THEN
                CALL PRESSURE_CYL(
     .               LOADS     ,TABLE     ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB,IFRAME         ,
     .               DT1       ,NODES%X         ,NODES%V         ,NODES%A         ,NODA_FEXT ,
     .               H3D_DATA  ,CPTREAC   ,FTHREAC   ,NODREAC   ,ELEMENT%PON%FSKY ,OUTPUT%TH%WFEXT     )
              ENDIF
!
C----------------------
C         /BCS/NRF
C----------------------
                IF(BCS%NUM_NRF > 0)THEN
                  CALL BCS_NRF(N2D            , NUMNOD   ,
     .                         NODES%X        , NODES%V  , NODES%A   ,
     .                         NIXS           , NIXTG    , NIXQ      ,
     .                         NUMELS         , NUMELTG  , NUMELQ    ,
     .                         IXS            , IXTG     , IXQ       ,
     .                         IPARIT         , LSKY     , ELEMENT%PON%FSKY,
     .                         OUTPUT%TH%WFEXT, NODA_FEXT, DT1       ,
     .                         ANIM_V         , OUTP_V   , H3D_DATA  )
                ENDIF
C----------------------

              IF( GLOB_THERM%NUMCONV + GLOB_THERM%NUMRADIA > 0 .AND. GLOB_THERM%ITHERM_FE > 0 )THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
C-------------------------------------------------------------------
C   BC -- CONVECTION  for heat_transfert by FEM
C----------------------------------------------
                IF (GLOB_THERM%NUMCONV > 0 .AND. GLOB_THERM%ITHERM_FE > 0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                  CALL CONVEC(IBCV  ,FCONV   ,NPC   ,TF   , NODES%X     ,
     1                      NODES%TEMP    ,NSENSOR,SENSORS%SENSOR_TAB,FTHE, ELEMENT%PON%IAD_CONV,
     2                      FTHESKY, PYTHON,GLOB_THERM)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                ENDIF
C-------------------------------------------------------------------
C   BC -- RADIATION to environment for heat_transfert by FEM
C-----------------------------------------------------------
                IF (GLOB_THERM%NUMRADIA > 0 .AND. GLOB_THERM%ITHERM_FE > 0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                  CALL RADIATION(IBCR,   FRADIA, NPC,     TF,   NODES%X         ,
     1                           NODES%TEMP,   NSENSOR,SENSORS%SENSOR_TAB, FTHE, ELEMENT%PON%IAD_RADIA,
     2                           FTHESKY, PYTHON,GLOB_THERM)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                ENDIF
!$OMP END PARALLEL
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C-------------------------------------------------------------------
C   BC -- THERMAL FLUX  for heat_transfert by FEM
C-------------------------------------------------
              IF (GLOB_THERM%NFXFLUX > 0 .AND. GLOB_THERM%ITHERM_FE > 0) THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                CALL FIXFLUX(IBFFLUX, FBFFLUX,  NPC,  TF,     NODES%X,     IXS,
     .                    NSENSOR,SENSORS%SENSOR_TAB, FTHE, ELEMENT%PON%IAD_FXFLUX, FTHESKY, PYTHON,
     .                    GLOB_THERM)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
              ENDIF
C Airbags
              ICONTACT_OLD(1:SICONTACT) = ICONTACT(1:SICONTACT)
              IF(NVOLU/=0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,50)
                CALL TRACE_IN(11,0,ZERO)
                NN = NUMELC+NUMELTG+IBAGSURF
                N0 = 1 + 3*NN
                IF(INTBAG/=0)THEN
                  N1 = N0+ NN
                ELSE
                  N1 = N0
                ENDIF
                SPORO = NUMELC+NUMELTG+IBAGSURF
C
                N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
                IF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF
                CALL MONVOL0(
     1            MONVOL          ,VOLMON             ,ptrX               ,NODES%A   ,
     2            NPC             ,TF                 ,NODES%V            ,WA        ,
     3            FSAV(1,N)       ,NSENSOR            ,SENSORS%SENSOR_TAB ,IGRSURF   ,
     4            FR_MV           ,ELEMENT%PON%IAD_MV ,SICONTACT          ,SPORO     ,
     5            ELEMENT%PON%FSKY,ICONTACT           ,WA(N0)             ,IPARG     ,
     6            ELBUF_TAB       ,GEO                ,IGEO               ,
     7            PM              ,IPM                ,IPART              ,IPART(K3) ,
     8            IPART(K8)       ,IGROUPC            ,IGROUPTG           ,NODA_FEXT ,
     9            1               ,H3D_DATA           ,T_MONVOL           ,frontier_global_mv,
     A            OUTPUT, PYTHON )
                CALL TRACE_OUT(11)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,50)
              ENDIF
              IF (IMON>0) CALL STOPTIME(TIMERS,6)
C
              IF (NFLOW>0) THEN
                CALL FLOW0(OUTPUT,IFLOW, RFLOW,   WIFLOW, WRFLOW, NODES%X,
     .                     NODES%V,     NODES%A,       NPC,    TF,     SENSORS%SENSOR_TAB,
     .                     NBGAUGE,LGAUGE,  GAUGE , NSENSOR,
     .                     IGRV,   AGRV  ,NFUNCT  ,PYTHON, OUTPUT%TH%WFEXT)
              ENDIF

C----------------------------------------
C      MPI COMMUNICATION BEFORE SORTING
C----------------------------------------
              IF (IMON>0) CALL STARTIME(TIMERS,13)
              IF (IMONM > 0) CALL STARTIME(TIMERS,24)
              IF(NSPMD>1)THEN
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
                CALL SPMD_I7XVCOM2(
     1             IPARI  ,NODES%X      ,NODES%V       ,NODES%MS      ,
     2             IMSCH  ,I2MSCH   ,DT2PREV ,INTLIST ,NBINTC  ,
     3             ISLEN7 ,IRLEN7   ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4             IRLEN17 ,IXS     ,IXS(L3) ,NSENSOR ,
     5             IGRBRIC ,NODES%TEMP    ,2       ,IRLEN7T ,ISLEN7T ,
     6             IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E,
     7             ISLEN20E,NODES%IKINE   ,DIAG_SMS,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB ,INT24E2EUSE,
     8             FORNEQS ,MULTI_FVM,INTERFACES,SH_OFFSET_TAB%NNSH_OSET)
              END IF
              IF (IMONM > 0) CALL STOPTIME(TIMERS,24)
              IF (IMON>0) CALL STOPTIME(TIMERS,13)


C--------------------------------------------------------
C Interface 24 - Communication Part 4/4
C--------------------------------------------------------

              IF (INT24USE == 1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
                CALL SPMD_EXCH_I24(IPARI    ,INTERFACES%INTBUF_TAB       ,NODES%ITAB  ,
     *                             NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                             IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,4,
     *                             INT24E2EUSE)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
              ENDIF

C
C Section processing in SPMD comm before FORINT
C
              IF(NSECT>0.AND.NSPMD>1) THEN
                LSEND1 = IAD_SEC(1,NSPMD+1)
                LRECV1 = IAD_SEC(2,NSPMD+1)
                LSEND2 = IAD_SEC(3,NSPMD+1)
                LRECV2 = IAD_SEC(4,NSPMD+1)
                CALL SPMD_EXCH_SEC(NSTRF ,NODES%X      ,NODES%MS    ,NODES%WEIGHT,XSEC  ,
     2                             FR_SEC,IAD_SEC,LSEND1,LRECV1,LSEND2,
     3                             LRECV2,NODES%WEIGHT_MD)
              END IF
C----------------------------------------------------------
C       INTER/TYPE21 ROTATION
C----------------------------------------------------------
              IF(NINTSTAMP/=0)THEN
                CALL INTSTAMP_INIT(INTSTAMP,NODES%ICODR)
              END IF
C---------------------------------------------
C TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
C---------------------------------------------
              IF(ISFEM >= 1) THEN
                IF(GLOB_THERM%ITHERM == 0)THEN
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  CALL S4LAGSFEM(IPARG, IXS, NODES%X, NODES%V, ELBUF_TAB, SFEM_NODVAR, S_SFEM_NODVAR,
     .                           NODES%BOUNDARY_ADD, NODES%BOUNDARY, IXS(L1), NODES%XDP, SXDP,
     .                           NUMNOD, NODES%BOUNDARY_SIZE , NSPMD, NUMELS, NUMELS8, NUMELS10,  NPARG, NGROUP, IRESP)
                ENDIF
              ENDIF

C----------------------------------------
C      COLLISION DETECTION FOR INTERFACES 7 (CALL BARRIER IN I7BUCE_CRIT)
C----------------------------------------
              IF(NINTER/=0) THEN
                CALL TRACE_IN(8,2,ZERO)
                IF (IMON > 0) CALL STARTIME(TIMERS,TIMER_CONTSORT)
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
                LSKYI_COUNT = 0
                LSKYI_SMS_NEW = 0

                IF(IDTMINS/=0) THEN
                  NATIV_SMS_SIZ = NUMNOD
                ELSE
                  NATIV_SMS_SIZ = 0
                ENDIF

                IF(coupling%active .AND. TT > ZERO) DT2T = MIN(DT2T,coupling%DT_LIMIT)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF
                INTER_ERRORS = 0

!$OMP PARALLEL PRIVATE(ITSK,DT2TT,NELTSTT,ITYPTSTT)
                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST
                ITSK = OMP_GET_THREAD_NUM()
                CALL INTTRI(OUTPUT ,TIMERS,
     1             IPARI   ,ptrX     ,W     , INTER_ERRORS,
     2             NODES%V       ,NODES%MS      ,NODES%IN      ,NODES%BOUNDARY_ADD  ,
     3             NODES%BOUNDARY ,NODES%VR      ,ISENDTO ,IRCVFROM  ,
     4             NEWFRONT  ,ITSK    ,WA      ,DT2TT     ,
     5             NODES%ITAB      ,NELTSTT ,ITYPTSTT,NODES%WEIGHT    ,
     6             INTLIST ,NBINTC  ,NODES%KINET   ,DRETRI    ,
     7             ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11   ,
     8             NODES%TEMP    ,IGRBRIC ,IGRSH3N ,EMINX     ,
     9             IXS     ,IXS(L3) ,IXS(L2) ,ISLEN17   ,
     A             IRLEN17 ,IRLEN7T ,ISLEN7T ,NUM_IMP1  ,
     B             IND_IMP ,INTSTAMP,THKNOD  ,IRLEN20   ,
     C             ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E  ,
     D             ISLEN20E  ,RENUM   ,NSNFIOLD,XSLV      ,
     E             XMSR      ,VSLV    ,VMSR    ,SIZE_T    ,
     F             NATIV_SMS ,DXANCG  ,NODES%IKINE   ,DIAG_SMS  ,
     G             COUNT_REMSLV,COUNT_REMSLVE  ,ALE_CONNECTIVITY,
     H             IXTG      ,SENSORS,DELTA_PMAX_GAP,INTERFACES%INTBUF_TAB,
     I             DELTA_PMAX_GAP_NODE,IAD_FRNOR,FR_NOR,
     J             NB25_CANDT,NB25_IMPCT,NB25_DST1,NB25_DST2,INTLIST25,
     K             INTERFACES%SPMD_ARRAYS%IAD_FREDG,INTERFACES%SPMD_ARRAYS%FR_EDG,NODES%MAIN_PROC,NATIV_SMS,I_OPT_STOK ,
     L             MULTI_FVM,IPARG  ,ELBUF_TAB, H3D_DATA, T2MAIN_SMS,
     M             LSKYI_SMS_NEW    ,FORNEQS  ,INT7ITIED,IDEL7NOK_SAV,MAXDGAP,
     N             T2FAC_SMS,NODES%ICODT,NODES%ISKEW ,FSKYN25  ,ADDCSRECT,PROCNOR,
     O             INTER_STRUCT,SORT_COMM,RNUM_SIZ,NATIV_SMS_SIZ,TEMP_SIZ,
     P             INTERFACES,GLOB_THERM,component)
C
#include "lockon.inc"
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL
                IF(INTER_ERRORS > 0) THEN
                  MSTOP = 2
                ENDIF


C========================================================================================

C ----------------------------------------------------
C Check if ISKY & FSKYI are sufficiently allocate
C If not reallocate them
C ----------------------------------------------------

                IF(IPARIT >0)THEN
                  IF(SIZE(INTERFACES%PON%ADSKYI,1) < NUMNOD+2) then
                    deallocate(INTERFACES%PON%ADSKYI)
                    allocate(INTERFACES%PON%ADSKYI(0:NUMNOD+1))
                  endif
                  IF ( NISKY+LSKYI_COUNT > SISKY) THEN
                    CALL REALLOCATE_I_SKYLINE(LSKYI_COUNT,1,GLOB_THERM%INTHEAT,GLOB_THERM%nodadt_therm,INTERFACES%PON)
                  ENDIF

C
C Remote Secnd Nodes
                  IF (NSPMD >1)THEN
                    CALL UPGRADE_REM_2RY(IPARI,COUNT_REMSLV,COUNT_REMSLVE,glob_therm%nodadt_therm)
                    COUNT_REMSLV(1:NINTER)= 0
                    COUNT_REMSLVE(1:NINTER)= 0
                  ENDIF

                ENDIF

                IF (IMON > 0) CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                CALL TRACE_OUT(8)
              ENDIF

C ----------------------------------------------------
C SMS - Check if ISKYI_SMS & MSKYI_SMS are sufficiently allocate
C If not reallocate them
C ----------------------------------------------------

              IF ((IDTMINS == 2.OR.IDTMINS_INT/=0).AND.(NINTER > 0)) THEN
C
                IF (LSKYI_SMS_NEW > LSKYI_SMS) THEN
C
                  DEALLOCATE(ISKYI_SMS,MSKYI_SMS,JDII_SMS,LTI_SMS)
C
                  LSKYI_SMS = NINT(LSKYI_SMS_NEW*1.2)
                  ALLOCATE(MSKYI_SMS(LSKYI_SMS),ISKYI_SMS(LSKYI_SMS,3),
     .                     JDII_SMS(2*LSKYI_SMS),LTI_SMS(2*LSKYI_SMS))
C
                ENDIF
              ENDIF

C--------------------------------------------------------
C INTERFACE 25 - Communication IRTLM, TIME_S, etc :: send
C--------------------------------------------------------
              IF(NINTER25 /= 0)THEN

                IF(NSPMD > 1)THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_EXFOR)
                  CALL SPMD_EXCH_I25(IPARI    ,INTERFACES%INTBUF_TAB       ,NODES%ITAB  ,
     *                               NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                               IAD_I25  ,FR_I25  ,SFR_I25 ,1     )
                  CALL SPMD_EXCH_I25(IPARI    ,INTERFACES%INTBUF_TAB     ,NODES%ITAB  ,
     *                               NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                               IAD_I25  ,FR_I25  ,SFR_I25 ,2     )
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_EXFOR)
                END IF

              END IF

C-------------------------------------------------------
C     ADAPTIVE MESHING
C   Get ICONTACT After sort, After Contact forces if SPMD
C                              Before Contact forces if SMP
C-------------------------------------------------------
              IF(NADMESH > 0.AND.IMPL_S==0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,36)
C
                NSH4OLD=NSH4ACT
                NSH3OLD=NSH3ACT
                IADMESH=0
                ICHKADM=0
                IF(TT >= TADMESH+DTADMESH)THEN
                  TADMESH=TADMESH+DTADMESH
                  ICHKADM=1
                END IF
C
                IF(ICHKADM/=0 .AND. IADMERRT/=0)THEN
C
                  CALL ADMERR(
     .             ELEMENT%SHELL%IXC     ,IXTG     ,NODES%X        ,IPARG       ,ELBUF_TAB   ,
     .             IPART   ,IPART(K3),IPART(K8),ERR_THK_SH4 ,ERR_THK_SH3 ,
     .             NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,NODES%WEIGHT   ,SH4TREE     ,SH3TREE     ,
     .             admerr_AREA_SH4, admerr_AREA_SH3, admerr_AREA_NOD,
     .             admerr_THICK_SH4,admerr_THICK_SH3,admerr_THICK_NOD  )
                END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                CALL ADMDIV(ELEMENT%SHELL%IXC  ,IPART(K3),IXTG ,IPART(K8),IPART,
     .                      ITSK ,ICONTACT ,IPARG,NODES%X        ,NODES%MS     ,
     .                      NODES%IN   ,RCONTACT ,ELBUF_TAB,NODFTSK  ,NODLTSK,
     .                       IGEO ,IPM      ,SH4TREE,PADMESH,MSC  ,
     .                       INC  ,SH3TREE  ,MSTG   ,INTG   ,PTG  ,
     .                    ACONTACT,PCONTACT,ERR_THK_SH4,ERR_THK_SH3,MSCND,
     .                       INCND,PM      ,NODES%MCP     ,MCPC   ,MCPTG,
     .                    GLOB_THERM%ITHERM_FE)
                NGDONE = 1
C       /---------------/
                CALL MY_BARRIER
C       /---------------/
                IF(IADMRULE /= 0)THEN
                  IF(IADMESH > 0)THEN
                    CALL ADMREGUL(ELEMENT%SHELL%IXC  ,IPART(K3),IXTG ,IPART(K8),IPART,
     .                            ITSK   ,IPARG    ,NODES%X      ,NODES%MS       ,NODES%IN   ,
     .                            ELBUF_TAB,NODFTSK  ,NODLTSK,IGEO     ,IPM  ,
     .                            SH4TREE,MSC  ,INC   ,SH3TREE,MSTG    ,
     .                            INTG   ,PTG  ,MSCND ,INCND  ,PM      ,
     .                            NODES%MCP    ,MCPC ,MCPTG ,GLOB_THERM%ITHERM_FE)
C           /---------------/
                    CALL MY_BARRIER
C           /---------------/
                  END IF
                END IF

                IF(IADMESH > 0)THEN
                  IF(ITSK==0) THEN
                    CALL ADMORDR(SH4TREE,SH3TREE,ELEMENT%SHELL%IXC,IXTG)
                    IF(ISTATCND /= 0) CALL CNDORDR(IPART,IPART(K3),IPART(K8),
     .                                             SH4TREE,SH3TREE)
                  END IF
                  IFLGADM=1
                  CALL ADMGVID(
     1              IPARG     ,ELBUF_TAB ,ELEMENT%PON%FSKY   ,ELEMENT%PON%FSKY  ,FTHESKY,
     2              ELEMENT%PON%IADC,ELEMENT%PON%IAD_TG,IFLGADM,IGROUC,NGROUC ,
     3              CONDNSKY  ,GLOB_THERM%NODADT_THERM)
                  CALL MY_BARRIER
                  NGDONE = 1
                END IF
                RCONTACT(NODFTSK:NODLTSK)=EP30
                ACONTACT(NODFTSK:NODLTSK)=EP30
                PCONTACT(NODFTSK:NODLTSK)=ZERO
!$OMP END PARALLEL

                IF (IMON>0) CALL STOPTIME(TIMERS,36)
              END IF
C--- // N/3 --------------------------------------------------------
C       EXTERNAL AND INTERNAL FORCES (ANIM)
C--------------------------------

              IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .         ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT>0
     .          .AND.IMPL_S==0)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
C--- // ----------------------------------------
C     EXTERNAL FORCES (ANIM, OUTP, H3D)
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL FORANI1(OUTPUT,NODES%A ,NFIA,NFEA,NFOA,NODFTSK,NODLTSK,NODA_FEXT,H3D_DATA)
!$OMP END PARALLEL
C
              ENDIF
C--------------------------------------------------------
C       ALE flux + forces
C--------------------------------------------------------
              !wa(1:numels+numelq) = -huge(wa(1))
              
              NEED_COMM_INTER18 = .FALSE.
              IF (IALE+IEULER+GLOB_THERM%ITHERM/=0.AND.GLOBAL_ACTIVE_ALE_ELEMENT) THEN
                CALL STARTIME(TIMERS,MACRO_TIMER_ALEMAIN)
C-----------------------------
C        SPMD : MS=0 boundary nodes if  weight/=1  (for amas03)
C        only for PARITH/OFF
C-----------------------------
                CALL TRACE_IN(12,0,ZERO)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C     Init var parallel SMP
                CALL SMP_INIT(
     1               ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2               IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3               GREFTSK,GRELTSK)
                DT2TT    = DT2T
                NELTSTT  = NELTST
                ITYPTSTT = ITYPTST
                IF(IPARIT == 1) NDTSK = 1
C
                CALL ALEMAIN(TIMERS,
     1               PM                    ,GEO      ,NODES%X       ,NODES%A(1,NDTSK)  ,NODES%V ,
     2               NODES%MS              ,WA       ,ELBUF_TAB     ,BUFMAT            ,PARTSAV(IPMTSK)     ,TF,
     3               VAL2                  ,VEUL     ,FV            ,NODES%STIFN(NDTSK),ELEMENT%PON%FSKY,EANI,
     4               PHI                   ,FILL     ,DFILL         ,ALPH              ,SKEWS%SKEW          ,W,
     5               NODES%D               ,DSAVE    ,ASAVE         ,DT2TT             ,DT2SAVE             ,XCELL,
     6               IPARG                 ,NPC      ,IXS           ,IXQ,   IXTG             ,ELEMENT%PON%IADS,
     7               IFILL                 ,NODES%ICODT,NODES%ISKEW ,IMS               ,ELEMENT%PON%IADQ     ,
     8               NELTSTT               ,ITYPTSTT ,IPART(K1)     ,IPART(K2)        ,ITSK                  ,
     A               NODFTSK               ,NODLTSK  ,NBRCVOIS      ,NODES%TEMP              ,OUTPUT%TH%TH_SURF%CHANNELS,
     B               NBSDVOIS              ,LNRCVOIS ,LNSDVOIS      ,NERCVOIS          ,NESDVOIS           ,LERCVOIS  ,
     C               LESDVOIS              ,ISIZXV   ,NODES%BOUNDARY_ADD   ,NODES%BOUNDARY,ELEMENT%PON%FSKYM,MSNF      ,
     D               IPARI                 ,SEGVAR   ,NODES%ITAB    ,ISKWN             ,DIFFUSION          ,IRESP,
     E               VOLMON                ,FSAV     ,IGRSURF       ,NELTSA            ,
     F               ITYPTSA               ,NODES%WEIGHT ,NPSEGCOM  ,LSEGCOM           ,IPM                ,IGEO,
     G               NODES%ITABM1          ,NODES%A           ,GRESAV             ,
     H               GRTH                  ,IGRTH    ,LGAUGE        ,GAUGE             ,MSSA               ,
     I               DMELS                 ,IGAUP    ,NGAUP         ,TABLE             ,NODES%MS0          ,
     J               NODES%XDP        ,IGRNOD   ,SFEM_NODVAR_ALE ,INTERFACES%PON%FSKYI,INTERFACES%PON%ISKY, S_SFEM_NODVAR,
     K               INTERFACES%INTBUF_TAB ,IXT      ,IGRV          ,AGRV              ,SENSORS            ,
     L               LGRAV                 ,CONDNSKY ,CONDN         ,MS_2D             ,MULTI_FVM          ,IGRTRUSS        ,
     M               IGRBRIC               ,NLOC_DMG ,ID_GLOBAL_VOIS,FACE_VOIS         ,EBCS_TAB           ,ALE_CONNECTIVITY,
     N               MAT_ELEM              ,H3D_DATA ,DT            ,OUTPUT            ,NEED_COMM_INTER18  ,IDTMINS         ,
     O               IDTMIN                ,MAXFUNC  ,IMON_MAT      ,USERL_AVAIL       ,
     P               impl_s                ,idyna    ,PYTHON        ,MAT_ELEM%MAT_PARAM,GLOB_THERM)

                IF(INT22 /=0)  call my_barrier !INTER22in input files  - get also IDT_INT22
#include "lockon.inc"
                IF(INT22 == 0)THEN
                  IF(DT2TT<DT2T)THEN
                    DT2T   = DT2TT
                    ITYPTST= ITYPTSTT
                    NELTST = NELTSTT
                  ENDIF
                ELSE
                  IF(IDT_INT22 /= 0)THEN
!inter22 kinematic time step
                    DT2T   = dt22_min
                    ITYPTST= 10
                    NELTST = 1
                  ELSE
!cell time step
                    IF(DT2TT<DT2T)THEN
                      DT2T   = DT2TT
                      ITYPTST= ITYPTSTT
                      NELTST = NELTSTT
                    ENDIF
                  ENDIF
                ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL
               CALL PYTHON_END_OPENMP(PYTHON)

                IF(NEED_COMM_INTER18) THEN
                  CALL SPMD_EXCH_INTER_18(NINTER,NSPMD,NUMBER_INTER18,SXCELL,INTER18_LIST,
     .                        XCELL,MULTI_FVM,XCELL_REMOTE,INTERFACES%INTBUF_TAB,ALE_CONNECTIVITY)
                ENDIF
                CALL TRACE_OUT(12)
                CALL STOPTIME(TIMERS,MACRO_TIMER_ALEMAIN)
              ELSE
                ALE%SUB%IFSUB=0
                ALE%SUB%IFSUBM=0
                T1S=TT
              ENDIF

             ! wa(1:numels+numelq) = -huge(wa(1))

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF(ALE%SUB%IALESUB ==2 .AND. ALE%SUB%IFSUB==2)GOTO 22
 21           CONTINUE
C
              IF (IMON>0) CALL STARTIME(TIMERS,6)
              IF (IMONM > 0) CALL STARTIME(TIMERS,51)
C----------------------------------
C       INTERNAL FORCES:S8FORC3
C----------------------------------
              CALL FORINTS(
     1          PM        ,GEO       ,NODES%X         ,NODES%A         ,NODES%AR        ,
     2          NODES%V         ,NODES%VR        ,NODES%MS        ,NODES%IN        ,W         ,
     3          ELBUF                ,VAL2      ,VEUL      ,FV        ,
     4          NODES%STIFN     ,NODES%STIFR     ,ELEMENT%PON%FSKY      ,TF        ,BUFMAT    ,
     5          PARTSAV   ,OUTPUT%DATA%FOPT,FSAV  ,
     6          SKEWS%SKEW,DT2T      ,
     7          ELEMENT%PON%IADS      ,IPARG     ,NPC       ,IXS       ,
     8          NELTST    ,ITYPTST   ,IPART     ,IPART(K1) ,NODES%ITAB      ,
     9          INTERFACES%PON%FSKYI     ,BUFGEO    ,KXX       ,IXX       ,INTERFACES%PON%ISKY      ,
     A          IPART(K9) ,GRESAV    ,GRTH      ,
     B          IGRTH     ,ELBUF_TAB )
              IF (IMONM > 0) CALL STOPTIME(TIMERS,51)
              IF (IMON>0) CALL STOPTIME(TIMERS,6)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
              CALL SMP_INIT(
     1          ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2          IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)

C-------------------------------------------------------
C     RESET ICONTACT(1:NUMNOD)
C-------------------------------------------------------
              IF(KCONTACT/=0)THEN
                IF(IALELAG >0)
     .          IFOAM_CONT(NODFTSK:NODLTSK)= ICONTACT(NODFTSK:NODLTSK)
C
                ICONTACT(NODFTSK:NODLTSK)=0
              END IF

              IF(ISTATCND /= 0)THEN
                IF(IPARIT==0)THEN
                  DO N=1,NUMNOD
                    STCND(NDTSK+N-1) = ZERO
                  END DO
                ELSE
                  DO N=NODFTSK,NODLTSK
                    STCND(N) = ZERO
                  END DO
                END IF
              END IF
C int 24+pxfem
              IF(INTPLYXFEM > 0) THEN
                DO N=NODFTSK,NODLTSK
                  WAGAP(1,N) = ZERO
                  WAGAP(2,N) = ZERO
                END DO
              ENDIF
C
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

              IF(ANIM_PLY > 0) VN_NOD = ZERO


C--------------move to here to keep TAGNCONT for output
              IF(NLOADP_HYD_INTER > 0 )THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                DO K=1,NLOADP_HYD_INTER
                  TAGNCONT(K,NODFTSK:NODLTSK) = 0
                ENDDO

!$OMP END PARALLEL

              ENDIF
C--- // 3N ------------------------------
C       INTERFACES WITH VOID OPENING
C----------------------------------------
              IF(NINTER/=0) THEN

                CALL TRACE_IN(4,1,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTSORT)

                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST
                IF (IMPL_S>0) THEN
                  IADISK   = 1
                ELSE
                  IADISK   = 1
                ENDIF
                IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF
                CALL INTFOP8(OUTPUT,
     1              IPARI  ,ptrX ,NODES%A ,
     2              NODES%ICODT  ,FSAV    ,NODES%V         ,NODES%MS             ,
     3              DT2TT  ,NELTSTT ,ITYPTSTT   ,NODES%ITAB    ,NODES%STIFN   ,
     4              NPC    ,TF      ,INTERFACES%PON%FSKYI     ,INTERFACES%PON%ISKY      ,NODES%VR              ,
     5              OUTPUT%DATA%VECT_CONT   ,NODES%IN      ,BUFSF     ,OUTPUT%DATA%VECT_PCONT ,NSENSOR,
     6              OUTPUT%DATA%VECT_PCONT_2 ,ICONTACT  ,RCONTACT  ,NUM_IMPL(1,1),
     7              NS_IMP(IADISK),NE_IMP(IADISK),NT_IMP  ,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB ,
     8              H3D_DATA      ,PSKIDS      ,TAGNCONT,KLOADPINTER,LOADPINTER,
     9              LOADP_HYD_INTER)

                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                ENDIF


C=======================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,IADISK)
!$OMP+ PRIVATE(DT2TT,NELTSTT,ITYPTSTT,GREFTSK,GRELTSK)

C Init var parallel SMP
                CALL SMP_INIT(
     1            ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2            IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)

                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST


                IF(ISTATCND /= 0 .AND. IPARIT == 0)THEN
                  DO N=1,NUMNOD
                    STCND (NDTSK+N-1)=-NODES%STIFN (NDTSK+N-1)
                  END DO
                END IF

                IF (IMPL_S>0) THEN
                  IADISK   = 1+ITSK*NINT7
                ELSE
                  IADISK   = 1
                ENDIF
                IF(IPARIT == 1) NDTSK = 1

                CALL INTFOP1(OUTPUT,
     1              IPARI  ,NODES%X       ,NODES%A(1,NDTSK) ,
     2              NODES%ICODT  ,FSAV    ,WA(NWAFTSK),NODES%V         ,NODES%MS             ,
     3              DT2TT  ,NELTSTT ,ITYPTSTT   ,NODES%ITAB      ,NODES%STIFN(NDTSK)   ,
     4              NPC    ,TF      ,INTERFACES%PON%FSKYI     ,INTERFACES%PON%ISKY      ,NODES%VR              ,
     6              OUTPUT%DATA%VECT_CONT   ,NODES%IN      ,IGRSURF   ,BUFSF     ,OUTPUT%DATA%VECT_PCONT ,
     7              OUTPUT%DATA%VECT_PCONT_2 ,ICONTACT  ,RCONTACT  ,NUM_IMPL(1,ITSK+1),
     8              NS_IMP(IADISK),NE_IMP(IADISK),NT_IMP  ,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB,
     9              H3D_DATA   ,NSENSOR)

#include "lockon.inc"
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF(IMPL_S>0) THEN
                  CALL RE2INT5(NT_IMP,NUM_IMP,NS_IMP,NE_IMP,NUM_IMPL,IPARI,NINT7)
                  NT_IMP5=NT_IMP
                END IF
C
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                CALL TRACE_OUT(4)

C----------------------------------------
C       INTERFACES: computation of forces
C----------------------------------------
                CALL TRACE_IN(4,2,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)

                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20

C--------------------------------------------------------
C INTERFACE 25 - Communication IRTLM, TIME_S, etc :: Reception
C--------------------------------------------------------
                IF(NSPMD > 1)THEN
                  IF(NINTER25 /= 0)THEN
                    IF (IMON>0) CALL STARTIME(TIMERS,TIMER_EXFOR)
                    CALL SPMD_EXCH_I25(IPARI    ,INTERFACES%INTBUF_TAB,NODES%ITAB  ,
     *                                 NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                                 IAD_I25  ,FR_I25  ,SFR_I25 ,3     )
                    CALL SPMD_EXCH_I25(IPARI    ,INTERFACES%INTBUF_TAB,NODES%ITAB  ,
     *                                 NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                                 IAD_I25  ,FR_I25  ,SFR_I25 ,4     )
                    IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_EXFOR)
                  ENDIF
                END IF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                NCONT = 0 ! Initialisation number of nodes tagged for FCONT storage
                IF (IMPL_S>0) THEN
                  NTMP = NT_IMP5 + NT_IMP1
                ENDIF
                IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IADISK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN,IDX_PINCH)
C Init var parallel SMP
                CALL SMP_INIT(
     1            ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2            IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)
                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST

                IF (IMPL_S>0) THEN
                  IADISK   = 1+NTMP+ITSK*(NINT7-NTMP)
                ELSE
                  IADISK   = 1
                ENDIF
                IDX_FTHE = NDTSK
                IF(NDTSK>IFTHE) IDX_FTHE  = 1
                IDX_CONDN = NDTSK
                IF(NDTSK>ICONDN)IDX_CONDN = 1
                IDX_PINCH = NDTSK
                IF(NPINCH == 0 )IDX_PINCH = 1
                IF(IPARIT == 1) NDTSK = 1
                CALL INTFOP2(OUTPUT, TIMERS,
     1            IPARI             ,ptrX            ,NODES%A(1,NDTSK)     ,IGROUPS         ,ALE_CONNECTIVITY,
     2            NODES%ICODT             ,FSAV           ,NODES%V        ,NODES%MS              ,DT2TT           ,
     3            NELTSTT           ,ITYPTSTT       ,NODES%ITAB     ,NODES%STIFN(NDTSK)    ,TF              ,
     4            INTERFACES%PON%FSKYI             ,INTERFACES%PON%ISKY           ,NODES%VR       ,OUTPUT%DATA%VECT_CONT,SECFCUM,
     5            ITSK+1            ,NISKYFI        ,NODES%KINET          ,NEWFRONT        ,NSTRF           ,
     6            ICONTACT          ,NODES%VISCN(NDTSK),XCELL       ,
     8            NUM_IMPL(1,ITSK+1),NS_IMP(IADISK) ,NE_IMP(IADISK) ,IND_IMP(IADISK) ,NT_IMP          ,
     9            FR_I18            ,IGRBRIC        ,EMINX          ,
     A            IXS               ,IXS(L3)        ,IXS(L2)        ,OUTPUT%DATA%VECT_PCONT ,OUTPUT%DATA%VECT_PCONT_2 ,
     B            NODES%BOUNDARY_ADD          ,NODES%BOUNDARY        ,RCONTACT       ,ACONTACT        ,PCONTACT        ,
     C            NODES%TEMP              ,FTHE(IDX_FTHE) ,FTHESKYI       ,IPARG           ,NSENSOR         ,
     D            PM                ,INTSTAMP       ,NODES%WEIGHT   ,NISKYFIE        ,IRLEN20         ,
     E            ISLEN20           ,IRLEN20T       ,ISLEN20T       ,IRLEN20E        ,ISLEN20E        ,
     F            MSKYI_SMS         ,ISKYI_SMS      ,NATIV_SMS      ,INT18ADD        ,FCONTG          ,
     G            FNCONTG           ,FTCONTG        ,NODES%NODGLOB        ,NODES%MS0       ,NPC             ,
     H            WA                ,SENSORS%SENSOR_TAB,QFRICINT    ,NCONT           ,INDEXCONT       ,
     I            TAGCONT           ,INOD_PXFEM     ,MS_PLY         ,WAGAP           ,ELBUF_TAB       ,
     J            CONDN(IDX_CONDN)  ,CONDNSKYI      ,ale%global%nv46,
     K            SENSORS%SFSAV     ,SENSORS%FSAV   ,GLOB_THERM%NODADT_THERM,GLOB_THERM%THEACCFACT,
     L            ISENSINT          ,NISUBMAX       ,NB25_CANDT     ,NB25_IMPCT      ,
     M            NB25_DST1         ,NB25_DST2      ,IXIG3D         ,KXIG3D          ,WIGE            ,
     N            KNOT              ,IGEO           ,MULTI_FVM      ,H3D_DATA        ,
     P            PSKIDS            ,T2MAIN_SMS     ,FORNEQS        ,KNOTLOCPC       ,KNOTLOCEL      ,
     Q            PINCH_DATA%APINCH(1,IDX_PINCH),PINCH_DATA%STIFPINCH(IDX_PINCH),T2FAC_SMS,TAGNCONT   ,
     R            KLOADPINTER       ,LOADPINTER     ,LOADP_HYD_INTER ,DGAPLOADINT    ,S_LOADPINTER    ,
     S            INTERFACES        ,XCELL_REMOTE)

                IF(NINTSTAMP/=0.AND.(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0.OR.
     .                 ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0))THEN
                  CALL MY_BARRIER
                  IF(NCONT/=0) THEN
                    DO I=ITSK+1,NCONT,NTHREAD
                      TAGCONT(INDEXCONT(I)) = 0
                    ENDDO
                  ENDIF
                ENDIF

#include "lockon.inc"
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                END IF
#include "lockoff.inc"

                IF(ISTATCND /= 0 .AND. IPARIT == 0)THEN
                  DO N=1,NUMNOD
                    STCND (NDTSK+N-1) = STCND (NDTSK+N-1) + NODES%STIFN (NDTSK+N-1)
                  END DO
                ENDIF
                IF (IPARIT == 0 .AND. NSPMD > 1 .AND. NTHREAD > 1) THEN
                  CALL MY_BARRIER()
                  CALL ASSPARXX(ITSK, INTLIST,NBINTC,IPARI,GLOB_THERM%NODADT_THERM)
                ENDIF
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                IF(IMPL_S>0) CALL RE2INT7(NT_IMP,NUM_IMP,NS_IMP,NE_IMP,
     1                                      IND_IMP,NUM_IMPL,IPARI,NINT7 )
C
                IF (IMON>0) THEN
                  CALL STOPTIME(TIMERS,TIMER_CONTFOR)
                  CALL STARTIME(TIMERS,TIMER_EXFOR)
                ENDIF
                IF (IMONM > 0) CALL STARTIME(TIMERS,21)

C--------------------------------------------------------
C Communication contact forces (SPMD), Part1 : Send
C--------------------------------------------------------

                IF(NSPMD>1)THEN
C
                  IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0.AND.
     .             ((TT>=OUTPUT%TANIM .AND. TT<=OUTPUT%TANIM_STOP).OR.TT>=TOUTP.OR.
     .             (TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP) .OR.
     .             (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .             .OR.H3D_DATA%N_VECT_PCONT_MAX >0)THEN        ! comm to do before i7fcom
                    CALL SPMD_EXCH_PRESS(OUTPUT,
     1                IPARI          ,INTLIST ,NBINTC  ,OUTPUT%DATA%VECT_PCONT,
     2                OUTPUT%DATA%VECT_PCONT_2,ISLEN7 ,IRLEN7  ,IRLEN7T ,ISLEN7T        ,
     3                IRLEN20        ,ISLEN20,IRLEN20T,ISLEN20T,INTERFACES%INTBUF_TAB    ,
     4                H3D_DATA%N_CSE_FRIC_INTER,H3D_DATA%N_SCAL_CSE_FRIC)
                  ELSEIF((H3D_DATA%N_SCAL_CSE_FRIC+OUTPUT%DATA%NINEFRIC>0.AND.
     .             ((TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP) .OR.
     .              H3D_DATA%MH3D/=0 ))
     .             .OR.(H3D_DATA%N_SCAL_CSE_FRIC+OUTPUT%DATA%NINEFRIC >0.AND.NINTER25>0))THEN
                    CALL SPMD_EXCH_EFRIC(OUTPUT,
     1                IPARI          ,INTLIST ,NBINTC  ,ISLEN7 ,IRLEN7  ,
     2                IRLEN7T        ,ISLEN7T ,IRLEN20 ,ISLEN20,IRLEN20T,
     3                ISLEN20T       ,INTERFACES%INTBUF_TAB ,H3D_DATA)

                  END IF
C
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  IF(IPARIT==0) THEN
                    CALL SPMD_I7FCOM_POFF(output,
     1                IPARI     ,NODES%A      ,NODES%STIFN   ,NODES%VISCN   ,
     2                INTLIST   ,NBINTC  ,NODES%ICODT  ,SECFCUM ,NSTRF   ,
     3                ICONTACT  ,OUTPUT%DATA%VECT_CONT    ,ISLEN7 ,IRLEN7  ,ISLEN11 ,
     4                IRLEN11   ,ISLEN17 ,IRLEN17,IGRBRIC ,
     5                IXS       ,IXS(L3) ,FTHE    ,IRLEN7T ,
     6                ISLEN7T   ,IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,
     7                IRLEN20E  ,ISLEN20E,CONDN  ,1, INTERFACES%INTBUF_TAB  ,
     8                H3D_DATA, MULTI_FVM,TAGNCONT,KLOADPINTER,LOADPINTER,
     9                LOADP_HYD_INTER,FSAV    ,INTERFACES,GLOB_THERM%NODADT_THERM)
                  ELSE

                    CALL SPMD_I7FCOM_PON(output,
     1                IPARI     ,INTLIST  ,NBINTC    ,NISKYFI   ,NODES%ICODT   ,
     2                SECFCUM   ,NSTRF    ,ICONTACT  ,OUTPUT%DATA%VECT_CONT      ,IGRBRIC ,
     3                IXS       ,IXS(L3)  ,NISKYFIE  ,NBINT20   ,1       ,
     4                INTERFACES%INTBUF_TAB,SFSKYI   ,SISKY     ,H3D_DATA  ,MULTI_FVM ,
     5                TAGNCONT  ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,FSAV,
     6                INTERFACES,glob_therm)

                    IF(MULTI_FVM%IS_INT18_LAW151) THEN
                      IF(NTHREAD>1) CALL INT18_LAW151_OMP_ACCUMULATION( MULTI_FVM )
                      CALL SPMD_INT18_LAW151_PON( IPARI,ISLEN7,IRLEN7,1,INTERFACES%INTBUF_TAB,
     1                                         MULTI_FVM )
                    ENDIF
                  END IF
                ENDIF
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_EXFOR)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,21)

                CALL TRACE_OUT(4)
              ENDIF   ! fin NINTER > 0
C--------------------------------------------------------
C Interface 24 - Communication Part 1/4
C--------------------------------------------------------
              IF (INT24USE == 1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
                CALL SPMD_EXCH_I24(IPARI    ,INTERFACES%INTBUF_TAB,NODES%ITAB  ,
     *                             NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                             IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,1,
     *                             INT24E2EUSE )

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
              ENDIF
C--------------------------------------------------------
C INTERFACE 21 - Communication of heat flux
C--------------------------------------------------------
              IF(NSPMD>1)THEN
                IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1) THEN
                  CALL SPMD_I21FTHECOM(IPARI  ,FTHE  ,INTERFACES%INTBUF_TAB,SENSORS%SENSOR_TAB,NISKYFI ,
     .            FTHESKYI,INTERFACES%PON%ISKY ,INTERFACES%PON%FSKYI ,CONDNSKYI,NSENSOR,GLOB_THERM%NODADT_THERM)
                ENDIF
              ENDIF
C--------------------------------------------------------
C Itet=2 STIFND <- STIFN part of interface only
C--------------------------------------------------------
              IF (NS10E > 0.AND.IPARIT == 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                GREFTSK   = 1+ITSK*NS10E/ NTHREAD
                GRELTSK   = (ITSK+1)*NS10E/NTHREAD
                CALL S10CNDFND(ICNDS10,NODES%WEIGHT ,IAD_CNDS,FR_CNDS,NODES%ITAB   ,
     2                         NODFTSK,NODLTSK,GREFTSK,GRELTSK,ITSK   ,
     3                         NODES%STIFN ,STIFND)
!$OMP END PARALLEL
              END IF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------------
C       VOLUMES MONITORES
C--------------------------------------------------------

              IF(NVOLU/=0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,50)
                CALL TRACE_IN(11,0,ZERO)
                NN = NUMELC+NUMELTG+IBAGSURF
                N0 = 1 + 3*NN
                IF(INTBAG/=0)THEN
                  N1 = N0+ NN
                ELSE
                  N1 = N0
                ENDIF
C
                CALL FVDIM(MONVOL)
                CALL FVCOPY(MONVOL)
                CALL FVMESH0(MONVOL,  NODES%X, VOLMON, IXS)
                CALL FVREZONE0(MONVOL, NODES%X)
                CALL FVUPD0(MONVOL, NODES%X, NODES%V, VOLMON, SMONVOL, SVOLMON)
                N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
                CALL FVBAG0(OUTPUT, MONVOL   , VOLMON,   NODES%X,            SENSORS%SENSOR_TAB, NODES%V   ,
     .                      NODES%A        , NPC,      TF,           NSENSOR           ,
     .                      FSAV(1,N), IFVMESH,  ICONTACT_OLD, LGAUGE            ,
     .                      GAUGE    , IGEO,     GEO,          PM                , IPM ,
     .                      IPARG    , IGROUPTG, IGROUPC,      ELBUF_TAB         , NODA_FEXT,
     .                      1        , H3D_DATA, NODES%ITAB,   NODES%WEIGHT      , OUTPUT%TH%WFEXT, PYTHON)
C
                CALL TRACE_OUT(11)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,50)
              ENDIF


              IF(NSPMD > 1 .AND. NVOLU > 0 .AND. NFVBAG0 > 0) THEN
                ! DT2R : value to be minimized over the processor
                DT2R = DT2

                ! Fill MIN_TAB with integer
                MIN_TAB(1) = NELTS
                MIN_TAB(2) = ITYPTS
                MIN_TAB(3) = 0
                MIN_TAB(4) = ISPMD
                !
                ! Begin Asynchronous communication
                !
                CALL MPI_MIN_REAL_BEGIN(DT2R,MIN_TAB,4,MPI_BUF)
                !                        |     |     |    |
                !Value to be minimized --*     |     |    |
                !Integers array ---------------*     |    |
                !Size of Integers array -------------*    |
                !Internal Structure ----------------------*
                !
                ! After this call DT2R and MIN_TAB are unchanged
                ! The minimum value of DT2R and the corresponding MIN_TAB
                ! Will be received after a call to  MPI_MIN_REAL_END
              ENDIF

              T1SH=TT
C
C-----------------------------------------------------
C     UPDATE OF SLIPRING AND RETRACTOR
C-----------------------------------------------------

              IF (NSLIPRING + NRETRACTOR> 0) CALL UPDATE_SLIPRING(IXR,ELEMENT%SHELL%IXC,IPARG,ELBUF_TAB,FLAG_SLIPRING_UPDATE,
     .                                                            FLAG_RETRACTOR_UPDATE,NODES%X,NPBY)
C
C---- // GROUPS ----------------
C     INTERNAL FORCES OF SHELLS, 3-NODE SHELLS
C-------------------------------
              CALL TRACE_IN(14,0,ZERO)
              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_ELEMENT)

C -------------------------------------------------------------------
C   User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
              GETUNOD_NOCOM=1
C -------------------------------------------------------------------
              LLT1 = I87G+3*NUMELTG
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,NDTSKR,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN,IDX_PINCH)
C Init var parallel SMP
              CALL SMP_INIT(
     1                      ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK,
     2                      IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3                      GREFTSK,GRELTSK)
              DT2TT = DT2T
              NELTSTT = NELTST
              ITYPTSTT= ITYPTST
              IAD_GREL = 1
              IF(IGRELEM == 1)IAD_GREL = K3-K1+1
              IDX_FTHE = NDTSK
              IF(NDTSK>IFTHE)IDX_FTHE=1
              IDX_CONDN = NDTSK
              IF(NDTSK>ICONDN)IDX_CONDN=1
              IDX_PINCH = NDTSK
              IF(NPINCH == 0 )IDX_PINCH = 1
              IF(IPARIT == 1) NDTSK = 1

              NDTSKR=NDTSK ; IF(IRODDL == 0)NDTSKR=1

              CALL FORINTC(TIMERS,
     1           PM           ,GEO             ,NODES%X               ,NODES%A(1,NDTSK)     ,NODES%AR(1,NDTSK)  ,
     2           NODES%V            ,NODES%VR              ,NODES%MS              ,NODES%IN             ,NLOC_DMG     ,
     3           WA(NWAFTSK)  ,NODES%STIFN(NDTSK)    ,NODES%STIFR(NDTSKR)    ,ELEMENT%PON%FSKY           ,CRKSKY       ,
     4           TF           ,BUFMAT          ,PARTSAV(IPMTSK) ,NODES%D              ,MAT_ELEM     ,
     5           NODES%DR           ,EANI            ,TANI            ,OUTPUT%DATA%FOPT ,
     6           FSAV         ,SENSORS         ,SKEWS%SKEW      ,FAILWAVE     ,
     7           DT2TT        ,THKE            ,BUFGEO          ,ELEMENT%PON%IADC     ,ELEMENT%PON%IAD_TG   ,
     8           IPARG        ,NPC             ,ELEMENT%SHELL%IXC             ,IXTG           ,NELTSTT      ,
     9           IPARI        ,ITYPTSTT        ,NSTRF           ,
     A           IPART        ,IPART(K3)       ,IPART(K8)       ,SECFCUM        ,
     B           FSAVD        ,MAT_ELEM%GROUP_PARAM ,
     E           FZERO        ,IXTG1           ,ELEMENT%PON%IAD_TG6      ,IGEO           ,IPM          ,
     F           MADFAIL      ,XSEC            ,ITSK            ,NODES%MCP          ,
     G           NODES%TEMP         ,FTHE(IDX_FTHE)  ,FTHESKY         ,
     H           MS_PLY       ,ZI_PLY          ,INOD_PXFEM      ,XEDGE4N       ,XEDGE3N       ,
     I           IEL_PXFEM    ,IADC_PXFEM      ,IGROUC          ,NGROUC        ,GRESAV(IGMTSK),
     J           GRTH         ,IGRTH(IAD_GREL) ,MSTG            ,DMELTG        ,MSC           ,
     K           DMELC        ,TABLE           ,KXFENOD2ELC     ,PTG           ,MSZ2          ,
     L           INOD_CRK     ,IEL_CRK         ,IADC_CRK        ,ELCUTC        ,NODENR        ,
     M           IBORDNODE    ,NODEDGE         ,CRKNODIAD       ,ELBUF_TAB     ,
     N           XFEM_TAB     ,CONDN(IDX_CONDN),CONDNSKY        ,CRKEDGE       ,
     O           STACK        ,NODES%ITAB            ,GLOB_THERM,
     Q           DRAPE_SH4N   ,DRAPE_SH3N      ,SUBSETS, NODES%XDP    ,PINCH_DATA%VPINCH   ,
     R           PINCH_DATA%APINCH(1,IDX_PINCH),PINCH_DATA%STIFPINCH(IDX_PINCH),DRAPEG  ,
     S           OUTPUT       ,DT             ,SNPC      ,  STF ,USERL_AVAIL                    ,MAXFUNC ,
     S           SBUFMAT    )
#include "lockon.inc"
              IF(DT2TT<DT2T)THEN
                DT2T = DT2TT
                NELTST = NELTSTT
                ITYPTST= ITYPTSTT
              END IF
#include "lockoff.inc"
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)
              CALL TRACE_OUT(14)
              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_ELEMENT)




C---- // GROUPS ----------------
C     INTERNAL FORCES OF  TRUSSES, POUTRES, RESSORTS,
C                         SOLIDES, QUAD
C-------------------------------

              CALL TRACE_IN(14,0,ZERO)
              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_ELEMENT)
              L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
              L2 = L1+6*NUMELS10
              L3 = L2+12*NUMELS20
              LL1 = 1+8*NUMELS
              LL2 = LL1+6*NUMELS10
              LL3 = LL2+12*NUMELS20

C
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
           CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,NDTSKR,IPMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(I16TSK,IGMTSK,GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN)
C Init var parallel SMP
              CALL SMP_INIT(
     1           ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK,
     2           IPMTSK,PARTFTSK ,PARTLTSK ,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)
              IF(IPARIT == 1) NDTSK = 1

              DT2TT = DT2T
              NELTSTT = NELTST
              ITYPTSTT= ITYPTST
              I16TSK = 1+ITSK*(SW16/NTHREAD)
              IDX_FTHE = NDTSK
              IF(NDTSK>IFTHE) IDX_FTHE  = 1
              IDX_CONDN = NDTSK
              IF(NDTSK>ICONDN)IDX_CONDN = 1

              NDTSKR = NDTSK ; IF(IRODDL == 0)NDTSKR = 1
C
              CALL FORINT(TIMERS, PYTHON,
     1         PM             ,GEO           ,NODES%X           ,NODES%A(1,NDTSK) ,NODES%AR(1,NDTSK)     ,
     2         NODES%V              ,NODES%VR            ,NODES%MS          ,NODES%IN         ,W               ,
     3         ELBUF          ,WA(NWAFTSK)   ,VAL2        ,VEUL       ,FV              ,
     4         NODES%STIFN(NDTSK)   ,NODES%STIFR(NDTSKR)  ,ELEMENT%PON%FSKY        ,TF         ,BUFMAT          ,
     5         PARTSAV(IPMTSK),NODES%D             ,NODES%DR          ,EANI       ,ELBUF_TAB       ,
     6         TANI           ,OUTPUT%DATA%FOPT,FSAV        ,SENSORS    ,NLOC_DMG        ,
     7         SKEWS%SKEW     ,OUTPUT%DATA%SCAL_SPRING ,DT2TT       ,BUFGEO     ,NODES%ITAB            ,
     8         ELEMENT%PON%IADS    ,ELEMENT%PON%IADQ    ,ELEMENT%PON%IAD_TRUSS  ,ELEMENT%PON%IAD_BEAM ,MAT_ELEM        ,
     9         ELEMENT%PON%IAD_SPRING     ,IPARG         ,ALE_CONNECTIVITY,NPC    ,
     A         IXS            ,IXQ           ,IXT         ,IXP        ,
     B         IXR            ,NELTSTT       ,IPARI       ,
     C         ITYPTSTT       ,NSTRF         ,IPART       ,
     D         IPART(K1)      ,IPART(K2)     ,IPART(K4)   ,IPART(K5)  ,
     E         IPART(K6)      ,IPART(K7)     ,FR_WAVE     ,RBY           ,
     F         SECFCUM        ,AGRV        ,IGRV       ,LGRAV           ,
     G         IXS(L1)        ,
     H         IXS(L2)        ,ELEMENT%PON%IADS10     ,ELEMENT%PON%IADS20   ,IXS(L3)    ,ELEMENT%PON%IADS16       ,
     I         W16(I16TSK)    ,ELEMENT%PON%FSKYM         ,MSNF        ,IGEO       ,IPM             ,
     J         XSEC           ,ITSK          ,NODES%TEMP           ,
     K         FTHE(IDX_FTHE) ,FTHESKY                    ,IGROUNC    ,NGROUNC         ,
     M         GRESAV(IGMTSK) ,GRTH          ,IGRTH       ,NODES%XDP        ,MSSA            ,
     N         DMELS          ,MSTR          ,DMELTR      ,MSP        ,DMELP           ,
     O         MSRT           ,DMELRT        ,TABLE       ,VFLOW      ,AFLOW           ,
     P         DFLOW          ,WFLOW         ,FFSKY       ,AFLOW      ,NBSDVOIS        ,
     Q         NERCVOIS       ,NESDVOIS      ,LERCVOIS    ,LESDVOIS   ,PHI             ,
     R         PHIE           ,MSF           ,NODFTSK     ,NODLTSK    ,
     S         FLG_KJ2        ,POR           ,IFOAM_CONT  ,IFOAM      ,SFEM_NODVAR     ,
     T         KXIG3D         ,IXIG3D        ,KNOT        ,WIGE       ,CONDN(IDX_CONDN),
     U         CONDNSKY       ,S_SFEM_NODVAR,
     V         TAGPRT_SMS     ,ITAGND        ,MS_2D       ,ALE_CONNECTIVITY%NALE       ,STRESSMEAN      ,
     W         KNOTLOCPC      ,KNOTLOCEL     ,SUBSETS     ,FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE  ,
     Y         H3D_DATA       ,IFTHE         ,ICONDN      ,DT      ,OUTPUT,
     Z         SBUFMAT        ,SNPC          ,STF         ,NODADT  ,DTFAC1,
     .         DTMIN1         ,IDTMIN        ,IOUT        ,ISTDO   ,IDTMINS,DTFACS,NSVOIS,
     .         IRESP          ,MAXFUNC       ,USERL_AVAIL ,GLOB_THERM,IMON_MAT,DTMINS,SANIN,
     .         NGRTH          ,NELEM         )
C
#include "lockon.inc"
              IF(DT2TT<DT2T)THEN
                DT2T = DT2TT
                NELTST = NELTSTT
                ITYPTST= ITYPTSTT
              END IF
#include "lockoff.inc"
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)
              CALL TRACE_OUT(14)

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_ELEMENT)
C-----
              IF(NUMSPHG/=0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                IF (IMONM > 0) CALL STARTIME(TIMERS,87)
                CALL TRACE_IN(13,0,ZERO)
C-----------------------------------------------
C     SPH SORT, SYMMETRIZATION AND CSPH PREPARATION & SPH INLETS/OUTLETS (After forint)
C-----------------------------------------------
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)
C Init var parallel SMP
                CALL SMP_INIT(
     1           ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2           IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)


                CALL SPHPREP(TIMERS,
     1            PM      ,GEO     ,NODES%X       ,NODES%V       ,NODES%MS      ,
     2            ELBUF_TAB,WA      ,TF      ,BUFMAT  ,PARTSAV ,
     3            IPARG   ,NPC     ,IPART   ,NODES%ITAB    ,BUFGEO  ,
     4            XFRAME  ,KXSP    ,IXSP    ,NOD2SP  ,IPART(K10),
     5            SPBUF   ,ISPCOND ,ISPSYM  ,XSPSYM  ,VSPSYM    ,
     6            WASPH(KSPH21) ,LPRTSPH ,LONFSPH ,WASPH(KSP2SORT) ,
     7            ISPHIO  ,VSPHIO  ,IGRSURF ,NODES%D        ,
     8            SPHVELN ,ITSK    ,NODES%XDP     ,IBUFSSG_IO,LGAUGE  ,
     9            GAUGE   ,NGROUNC ,IGROUNC ,SOL2SPH ,SPH2SOL   ,
     A            IXS     ,ELEMENT%PON%IADS    ,ELEMENT%PON%ADSKY   ,ELEMENT%PON%FSKYD   ,DMSPH(NDTSK),
     B            WASPH(KSPACTIV),ICONTACT_OLD,OFF_SPH_R2R,WSMCOMP,IRUNN_BIS,
     C            SPH_IORD1,SPH_WORK,OUTPUT%TH%WFEXT)
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMONM > 0) CALL STOPTIME(TIMERS,87)
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                IF (IMONM > 0) CALL STARTIME(TIMERS,88)

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK)
!$OMP+ PRIVATE(DT2TT,NELTSTT,ITYPTSTT,IGMTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
                CALL SMP_INIT(
     1            ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2            IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)

                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST

                IAD_GREL = 1
                IF(IGRELEM == 1)IAD_GREL = K10-K1+1
                IF(IPARIT == 1) NDTSK = 1

C----------------------------------
C       SPH: Internal forces
C----------------------------------
                CALL FORINTP(TIMERS,
     1            PM            ,GEO            ,NODES%X          ,NODES%A(1,NDTSK)      ,NODES%V             ,
     2            NODES%MS            ,W              ,ELBUF_TAB  ,WA              ,FV            ,
     3            NODES%STIFN(NDTSK)  ,TF             ,BUFMAT     ,PARTSAV(IPMTSK) ,NLOC_DMG      ,
     4            FSAV          ,DT2TT          ,ELEMENT%PON%IADS       ,IPARG           ,NPC           ,
     5            NELTSTT       ,ITYPTSTT       ,IPART      ,NODES%ITAB            ,INTERFACES%PON%ISKY          ,
     6            BUFGEO        ,INTERFACES%PON%FSKYI          ,XFRAME     ,KXSP            ,IXSP          ,
     7            NOD2SP        ,IPART(K10)     ,SPBUF      ,ISPCOND         ,ISPSYM        ,
     8            XSPSYM%BUF    ,VSPSYM%BUF     ,
     9            WASPH(KSPH21) ,LPRTSPH        ,LONFSPH    ,WASPH(KSPACTIV) ,ISPHIO        ,
     A            VSPHIO        ,SPHVELN        ,ITSK       ,IPM             ,GRESAV(IGMTSK),
     B            GRTH          ,IGRTH(IAD_GREL),TABLE      ,LGAUGE          ,GAUGE         ,
     C            NGROUNC       ,IGROUNC        ,IXS        ,IRST            ,SOL2SPH       ,
     D            SPH2SOL       ,ELEMENT%PON%FSKY           ,ELEMENT%PON%FSKY       ,IGEO            ,NODES%TEMP          ,
     E            FTHE          ,FTHESKYI       ,SPHG_F6    ,WSMCOMP%BUF     ,SOL2SPH_TYP   ,
     F            MAt_ELEM      ,OUTPUT         ,SPH_IORD1  ,SNPC            ,STF           ,
     G            SBUFMAT       ,NSVOIS         ,IDTMINS    ,IRESP           ,MAXFUNC       ,
     .            IMON_MAT      ,USERL_AVAIL    ,IMPL_S     ,IDYNA         ,
     .            DT            ,GLOB_THERM     ,SPH_WORK   ,OUTPUT%TH%WFEXT         ,SENSORS        )
#include "lockon.inc"
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                END IF
#include "lockoff.inc"

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
C -------------------------------------------------------------------
C   User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
                GETUNOD_NOCOM=0
C -------------------------------------------------------------------

C=======================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                CALL TRACE_OUT(13)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,88)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
              ENDIF

C-----------------------------------------------
C Multidomains : synchro proc of flag_activation
C----------------------------------------------
              IF (NSPMD>1) THEN
                IF ((SDD_R2R_ELEM>0).AND.(FLG_SPHINOUT_R2R>0)) THEN
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  CALL SPMD_EXCH_R2R_SPHOFF(OFF_SPH_R2R,NODES%BOUNDARY_ADD,NODES%BOUNDARY,DD_R2R,DD_R2R_ELEM,LENR)
                  CALL SPMD_EXCH_R2R_SPH(NODES%X,NODES%BOUNDARY_ADD,NODES%BOUNDARY,DD_R2R,DD_R2R_ELEM,LENR)
                  CALL SPMD_EXCH_R2R_SPH(NODES%D,NODES%BOUNDARY_ADD,NODES%BOUNDARY,DD_R2R,DD_R2R_ELEM,LENR)
                  CALL SPMD_EXCH_R2R_SPH(NODES%V,NODES%BOUNDARY_ADD,NODES%BOUNDARY,DD_R2R,DD_R2R_ELEM,LENR)
                ENDIF
              ENDIF


              IF(NVOLU/=0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,50)
                CALL TRACE_IN(11,0,ZERO)
                NN = NUMELC+NUMELTG+IBAGSURF
                N0 = 1 + 3*NN
                IF(INTBAG/=0)THEN
                  N1 = N0+ NN
                ELSE
                  N1 = N0
                ENDIF
                SPORO = NUMELC+NUMELTG+IBAGSURF
C
                N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
                CALL FVBAG0(OUTPUT, MONVOL,    VOLMON,  NODES%X,   SENSORS%SENSOR_TAB,  NODES%V,
     .                      NODES%A,      NPC,     TF,     NSENSOR           ,
     .                      FSAV(1,N), IFVMESH, ICONTACT_OLD,LGAUGE,
     .                      GAUGE    , IGEO,    GEO,         PM,                  IPM,
     .                      IPARG    , IGROUPTG,IGROUPC,     ELBUF_TAB,           NODA_FEXT,
     .                      2        , H3D_DATA,NODES%ITAB, NODES%WEIGHT, OUTPUT%TH%WFEXT, PYTHON)
                IF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrx,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrx,NODES%X,NUMNOD)
                ENDIF
                CALL MONVOL0(
     1            MONVOL            ,VOLMON             ,ptrX               ,NODES%A   ,
     2            NPC               ,TF                 ,NODES%V            ,WA        ,
     3            FSAV(1,N)         ,NSENSOR            ,SENSORS%SENSOR_TAB ,IGRSURF   ,
     4            FR_MV             ,ELEMENT%PON%IAD_MV ,SICONTACT          ,SPORO     ,
     5            ELEMENT%PON%FSKY  ,ICONTACT           ,WA(N0)             ,IPARG     ,
     6            ELBUF_TAB         ,GEO                ,IGEO               ,
     7            PM                ,IPM                ,IPART              ,IPART(K3) ,
     8            IPART(K8)         ,IGROUPC            ,IGROUPTG           ,NODA_FEXT      ,
     9            2                 ,H3D_DATA           ,T_MONVOL           ,frontier_global_mv,
     A            OUTPUT, PYTHON)

                CALL TRACE_OUT(11)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,50)
              ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (NS10E > 0 .AND. IPARIT==0) THEN
C must be done before the reception of remote contact stif
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                GREFTSK   = 1+ITSK*NS10E/ NTHREAD
                GRELTSK   = (ITSK+1)*NS10E/NTHREAD
                CALL S10STFE_POFF(ICNDS10,NODES%WEIGHT ,IAD_CNDS,FR_CNDS,NODES%ITAB   ,
     2                   NODFTSK,NODLTSK,GREFTSK,GRELTSK,ITSK   ,
     3                   NODES%STIFN ,STIFND)
!$OMP END PARALLEL
              END IF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------------
C Communication of interface forces (SPMD), Part2 : Reception
C----------------------------------------
              IF(NINTER/=0) THEN
                IF(NSPMD>1)THEN
                  IF(IMONM == 2)THEN
                    CALL STARTIME(TIMERS,59)
                    CALL SPMD_BARRIER()
                    CALL STOPTIME(TIMERS,59)
                  ENDIF
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_EXFOR)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,22)
C
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  IF(IPARIT==0)THEN

                    CALL SPMD_I7FCOM_POFF(OUTPUT,
     1                 IPARI     ,NODES%A    ,NODES%STIFN   ,NODES%VISCN   ,
     2                 INTLIST   ,NBINTC  ,NODES%ICODT   ,SECFCUM ,NSTRF   ,
     3                 ICONTACT  ,OUTPUT%DATA%VECT_CONT    ,ISLEN7  ,IRLEN7  ,ISLEN11 ,
     4                 IRLEN11   ,ISLEN17 ,IRLEN17 ,IGRBRIC ,
     5                 IXS       ,IXS(L3) ,FTHE    ,IRLEN7T ,
     6                 ISLEN7T   ,IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,
     7                 IRLEN20E  ,ISLEN20E,CONDN   ,2, INTERFACES%INTBUF_TAB,
     8                 H3D_DATA, MULTI_FVM,TAGNCONT,KLOADPINTER,LOADPINTER,
     9                 LOADP_HYD_INTER,FSAV    ,INTERFACES,glob_therm%nodadt_therm)
                  ELSE

                    CALL SPMD_I7FCOM_PON(OUTPUT,
     1                IPARI     ,INTLIST  ,NBINTC    ,NISKYFI   ,NODES%ICODT   ,
     2                SECFCUM   ,NSTRF    ,ICONTACT  ,OUTPUT%DATA%VECT_CONT      ,IGRBRIC ,
     3                IXS       ,IXS(L3)  ,NISKYFIE  ,NBINT20   ,2       ,
     4                INTERFACES%INTBUF_TAB,SFSKYI   ,SISKY     ,H3D_DATA  ,MULTI_FVM ,
     5                TAGNCONT  ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,FSAV,
     6                INTERFACES,glob_therm)
                  END IF

C
                  IF (IMON>0) THEN
                    CALL STOPTIME(TIMERS,TIMER_EXFOR)
                    IF (IMONM > 0) CALL STOPTIME(TIMERS,22)
                  ENDIF
                ENDIF
              ENDIF

 22           CONTINUE

!   Mpi communication for Nlocal option : parith/off
              IF(IPARIT /= 0.AND.NSPMD > 1.AND. NLOC_DMG%IMOD > 0)THEN
                CALL SPMD_EXCH_SUB_PON(NLOC_DMG)
              ENDIF
              IF(NSPMD > 1.AND. NINTLOADP > 0)THEN
                LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                CALL SPMD_EXCH_TAGNCONT(TAGNCONT,NODES%BOUNDARY_ADD ,NODES%BOUNDARY,LENR     )
              ENDIF
!   ----------------------------------

              IF(NLOADP_HYD/=0.AND.IMPL_S/=1) THEN
                NODA_FEXT = ZERO
                CALL TRACE_IN(10,0,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,41)
                CALL LOAD_PRESSURE(ILOADP    ,LOADP     ,LLOADP  ,NPC    ,TF       ,
     2                              NODES%A         ,NODES%V         ,NODES%X       ,SKEWS%SKEW   ,SENSORS%SENSOR_TAB,
     3                              ELEMENT%PON%IAD_LOADP,ELEMENT%PON%FSKY      ,OUTPUT%DATA%VECT_FEXT,TAGNCONT ,NSENSOR   ,
     4                              LOADP_HYD_INTER,H3D_DATA , PYTHON,
     5                              NPRESLOAD ,LOADP_TAGDEL,OUTPUT%TH%TH_SURF,PBLAST,OUTPUT%TH%WFEXT)


                IF (IMONM > 0) CALL STOPTIME(TIMERS,41)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                CALL TRACE_OUT(10)
              ENDIF
C--- //------------------------------
C       FORCE ASSEMBLY
C-------------------------------------
             IF(coupling%active) THEN
               NODES%FORCES(1:3,1:NUMNOD) = nodes%A(1:3,1:NUMNOD)
             ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              DTNOD_NLOCAL = EP20
              IF(IPARIT == 0 .AND. NTHREAD > 1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_ASM)

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,PARTFTSK,PARTLTSK,GREFTSK,GRELTSK,NODFT_NL,NODLT_NL)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                PARTFTSK  = 1+ITSK*NPSAV*NPART/NTHREAD
                PARTLTSK  = (ITSK+1)*NPSAV*NPART/NTHREAD
                GREFTSK  = 1+ITSK*NPSAV*NGPE/NTHREAD
                GRELTSK  = (ITSK+1)*NPSAV*NGPE/NTHREAD

C Parith/OFF assembly necessary before boundary communication if multi-thread

                CALL ASSPAR(
     1            NTHREAD    ,NUMNOD,NODFTSK,NODLTSK,IRODDL,
     2            NPART    ,PARTFTSK,PARTLTSK ,NODES%A      ,NODES%AR    ,
     3            PARTSAV  ,NODES%STIFN ,NODES%STIFR  ,NODES%VISCN  , FTHE ,
     4            GLOB_THERM%ITHERM_FE,GLOB_THERM%NODADT_THERM,STCND ,GREFTSK,GRELTSK ,
     5            GRESAV   ,NGPE  ,NTHPART ,IALELAG, AFLOW,
     6            DMSPH    ,CONDN ,
     7            PINCH_DATA%APINCH,PINCH_DATA%STIFPINCH)


!   ----------------------------------
!   Accumulation of acceleration for Nlocal option : parith/off
                IF (NLOC_DMG%IMOD > 0) THEN
                  NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
                  NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
                  CALL ASSPAR_SUB_POFF(NLOC_DMG%FNL ,NODFT_NL,NODLT_NL,
     .                                 NLOC_DMG%POSI,NLOC_DMG%L_NLOC,NTHREAD )
                  ! Non-local nodal stiffness
                  IF (NODADT > 0) THEN
                    CALL ASSPAR_SUB_POFF(NLOC_DMG%STIFNL,NODFT_NL,NODLT_NL,
     .                                   NLOC_DMG%POSI,NLOC_DMG%L_NLOC,NTHREAD )
                    CALL NLOCAL_DTNODA(NODFT_NL,NODLT_NL,NLOC_DMG,DTNOD_NLOCAL,DT2T)
                  ENDIF
                ENDIF
!$OMP END PARALLEL
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_ASM)
              ENDIF
C Transfer of contact force from sleeping particles to solid nodes
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              IF(NUMSPH /= 0 .AND. NSPHSOL /= 0 )THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                IF (IMONM > 0) CALL STARTIME(TIMERS,89)
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                CALL SOLTOSPHF(
     1           NODES%A      ,SPBUF   ,IXS     ,KXSP    ,IPART(K10),
     2           NOD2SP ,IRST   ,NGROUNC  ,IGROUNC ,IPARG    ,
     3           NODES%STIFN  ,SOL2SPH,SPH2SOL  ,ELBUF_TAB,ITSK    ,
     4           NODFTSK,NODLTSK,INTERFACES%PON%ISKY     ,INTERFACES%PON%FSKYI    ,IGEO    ,
     5           SOL2SPH_TYP)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMONM > 0) CALL STOPTIME(TIMERS,89)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
              ENDIF

C===== Nitsche Method Reinit FORNEQS every cycle

              IF(NITSCHE/=0) THEN
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK)

C Init var parallel SMP
                CALL SMP_INIT(
     1            ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2            IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3          GREFTSK,GRELTSK)

                CALL ZEROR(FORNEQS(1,NODFTSK),NUMNTSK)

!$OMP END PARALLEL

C====== Nitsche equivalent nodal force computation FORNEQS ===========

                IF (INT24USE == 1)THEN
                  IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                    CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                  ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                    CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                  ELSE
                    CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                  ENDIF
                  CALL  I24NITSCHFOR3 (IPARI   ,INTERFACES%INTBUF_TAB,IPARIT  ,STRESSMEAN ,
     2                                INTLIST ,NBINTC     ,ptrX,ELEMENT%PON%IADS,
     3                                FORNEQS ,FORNEQSKY  ,NODES%ITAB    ,IXS        ,
     4                                ELEMENT%PON%IADS10,ELEMENT%PON%IADS20 ,ELEMENT%PON%IADS16,NFACNIT   )
                ENDIF
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF(INT18KINE== 1)THEN
                ALLOCATE(MTF(14,NUMNOD))
                ALLOCATE(CAND_SAV(8,INT18ADD(NINTER+1)-1))
                IF (NSPMD > 1)THEN
                  ALLOCATE(TAGPENE(NUMNOD))
                ELSE
                  ALLOCATE(TAGPENE(1))
                ENDIF
              ENDIF
              INT18ADD(NINTER+1) = -IABS(INT18ADD(NINTER+1))
              INT18KINE=-IABS(INT18KINE)

              IF(NSPMD>1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_EXFOR)
                IF (IPARIT==0) THEN
                  LENGTH =  4 + IRODDL*4
                  IF (N2D/=0) THEN
                    LENGTH = LENGTH + 1
                    IF(ALE%SUB%IFSUBM == 1) LENGTH = LENGTH + 1
                  ELSEIF(ALE%SUB%IFSUBM==1)THEN
                    LENGTH = LENGTH + 2
                  ENDIF
C
                  IF(GLOB_THERM%ITHERM_FE > 0  )THEN
                    LENGTH = LENGTH + 3
                    IF (GLOB_THERM%NODADT_THERM == 1 ) LENGTH = LENGTH + 1
                  ENDIF
C
                  IF(IALELAG > 0 )THEN
                    LENGTH = LENGTH + 4
                  ENDIF
C
                  IF(SOL2SPH_FLAG/=0) LENGTH = LENGTH + 1
C
                  IF(NITSCHE > 0 )THEN
                    NFACNIT = 3
                  ENDIF
C
                  LENC = 0
                  IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) LENC = 3
                  IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) LENC = LENC+6
C
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  IF(IDTMINS /= 0)THEN

                    CALL SPMD_EXCH_A_AMS_POFF(
     1               NODES%A       ,NODES%AR     ,NODES%STIFN, NODES%STIFR ,NODES%MS   ,
     2               NODES%BOUNDARY_ADD,NODES%BOUNDARY,MSNF ,ALE%SUB%IFSUBM,LENGTH ,
     3               LENR    ,FTHE , NODES%MCP,FR_LOC,NB_FR   ,
     4               MS_2D ,MCP_OFF,FORNEQS ,NFACNIT    ,
     5               LENC ,OUTPUT%DATA%VECT_CONT ,H3D_DATA,OUTPUT%DATA%VECT_PCONT,
     6               OUTPUT%DATA%VECT_PCONT_2 ,GLOB_THERM)
                  ELSE
                    CALL SPMD_EXCH_A(
     1               NODES%A , NODES%ACC_DP      ,NODES%AR     ,NODES%STIFN,NODES%STIFR ,NODES%MS   ,
     2               NODES%BOUNDARY_ADD,NODES%BOUNDARY,MSNF ,ALE%SUB%IFSUBM,LENGTH ,
     3               LENR    ,FTHE , NODES%MCP, DMSPH,CONDN,
     4               MS_2D,MCP_OFF,
     5               FORNEQS ,NFACNIT,LENC ,OUTPUT%DATA%VECT_CONT ,H3D_DATA,
     6               OUTPUT%DATA%VECT_PCONT ,OUTPUT%DATA%VECT_PCONT_2 ,GLOB_THERM)

                  ENDIF
C
                ELSE
                  LENGTH =  4 + IRODDL*4
                  IF(ALE%SUB%IFSUBM==1)THEN
                    LENGTH = LENGTH + 1
                  ENDIF
                  IF(N2D /= 0.AND.ALE%SUB%IFSUBM == 1) LENGTH = LENGTH + 1
                  SIZI = NFSKYI+1
C
                  IF (GLOB_THERM%ITHERM_FE > 0 )THEN
                    LENGTH = LENGTH + 1
                    SIZI = SIZI + 1
                    IF (GLOB_THERM%NODADT_THERM == 1 ) THEN
                      LENGTH = LENGTH + 1
                      SIZI = SIZI + 1
                    ENDIF
                  ENDIF
                  IF(INTPLYXFEM >  0) SIZI = SIZI + 5
C
                  IF(IALELAG > 0 )THEN
                    LENGTH = LENGTH + 4
                  ENDIF
C
                  LENS = FR_NBCC(1,NSPMD+1)
                  LENR = FR_NBCC(2,NSPMD+1)
                  LENI = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
C
                  LENS1 = 0
                  LENR1 = 0
C
                  IF(IPLYXFEM > 0) THEN
                    LENS1 = FR_NBCC1(1,NSPMD+1)
                    LENR1 = FR_NBCC1(2,NSPMD+1)
                  ENDIF
C
                  IF(ICRACK3D > 0) THEN
                    LENS1 = FR_NBCC1(1,NSPMD+1)
                    LENR1 = FR_NBCC1(2,NSPMD+1)
                  ENDIF
C
                  IF(SOL2SPH_FLAG/=0)THEN
                    LENGTH = LENGTH + 1
                  ENDIF
C
                  LENC = 0
                  IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) LENC = 3*LENI
                  IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) LENC = LENC+6*LENI

                  CALL SPMD_EXCH2_A_PON(INTERFACES,
     1             NODES%BOUNDARY_ADD    ,NODES%BOUNDARY   ,ELEMENT%PON%ADSKY   ,ELEMENT%PON%PROCNE    ,FR_NBCC     ,
     2             LENGTH        ,LENR      ,LENS    ,ELEMENT%PON%FSKY      ,ELEMENT%PON%FSKY        ,
     3             ELEMENT%PON%FSKYM       ,ALE%SUB%IFSUBM    ,SIZI    ,LENI      ,ELEMENT%PON%IADSDP      ,
     4             ELEMENT%PON%IADRCP      ,ELEMENT%PON%ISENDP    ,ELEMENT%PON%IRECVP  ,FFSKY     ,PROCNE_PXFEM,
     5             FR_NBCC1    ,IADSDP_PXFEM,IADRCP_PXFEM     ,ISENDP_PXFEM,
     6             IRECVP_PXFEM,LENR1     ,LENS1   ,IADSDP_CRK,IADRCP_CRK  ,
     7             ISENDP_CRK  ,IRECVP_CRK,ELEMENT%PON%FSKYD   ,CRKNODIAD ,CRKSKY      ,
     8             FORNEQSKY   ,NFACNIT  ,LENC     , OUTPUT%DATA%VECT_CONT     ,H3D_DATA    ,
     9             OUTPUT%DATA%VECT_PCONT,OUTPUT%DATA%VECT_PCONT_2 ,GLOB_THERM)
C
                ENDIF
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_EXFOR)
              ENDIF

C--- //------------------------------
C       PARITH/ON ASSEMBLY OF FORCES AFTER COMMUNICATION
C-------------------------------------
              CALL TRACE_IN(15,0,ZERO)

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_ASM)
c     Parallel update of crack
              IF(ICRACK3D > 0 .AND. NSPMD > 1) CALL SPMD_MAX_XFE_I(NLEVSET)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              IF(IPARIT==1) ALLOCATE( FSKY_L(NISKY) )
              DTNOD_NLOCAL = EP20
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,PARTFTSK,PARTLTSK,GREFTSK,GRELTSK,NODFTSK_2,NODLTSK_2,NODFT_NL,NODLT_NL)
              ITSK = OMP_GET_THREAD_NUM()

              NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
              NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
              PARTFTSK  = 1+ITSK*NPSAV*NPART/NTHREAD
              PARTLTSK  = (ITSK+1)*NPSAV*NPART/NTHREAD
              GREFTSK  = 1+ITSK*NPSAV*NGPE/NTHREAD
              GRELTSK  = (ITSK+1)*NPSAV*NGPE/NTHREAD
              NODFTSK_2   = NODFT_ASSPAR(ITSK+1)
              NODLTSK_2   = NODLT_ASSPAR(ITSK+1)

              IF(IPARIT==1)THEN

C------------------------
C Assembly Parith/ON spmd+multi-thread
C------------------------
                CALL ASSPAR4(NODES,
     2               ELEMENT%PON%FSKY   ,ELEMENT%PON%FSKY    ,ELEMENT%PON%ADSKY  ,ELEMENT%PON%FSKYM ,
     3               MSNF   ,INTERFACES%PON%ISKY    ,INTERFACES%PON%FSKYI  ,FTHE  ,
     4               FTHESKY,FTHESKYI,NODFTSK,NODLTSK     ,INTERFACES%PON%ADSKYI,
     5               PARTSAV,PARTFTSK ,PARTLTSK ,ITSK     ,GREFTSK ,
     6               GRELTSK  ,GRESAV   ,AFLOW    ,FFSKY    ,MSF   ,
     7               ADSKY_PXFEM, INOD_PXFEM    ,ELEMENT%PON%FSKYD ,
     8               DMSPH  ,CONDN   ,CONDNSKY  ,CONDNSKYI,
     9               MS_2D,ICNDS10  ,
     A               STIFND ,FORNEQS ,FORNEQSKY ,NFACNIT,NODFTSK_2,
     B               NODLTSK_2,FSKY_L,GLOB_THERM)

!   ----------------------------------
!   Accumulation of acceleration for Nlocal option : parith/on
                IF (NLOC_DMG%IMOD>0) THEN
                  NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
                  NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
                  CALL ASSPAR_SUB(NLOC_DMG%FNL(:,1) ,NLOC_DMG%FSKY,NLOC_DMG%ADDCNE,NODFT_NL     ,
     .                            NODLT_NL    ,NLOC_DMG%POSI  ,NLOC_DMG%NNOD, NLOC_DMG%L_NLOC)
                  ! Non-local nodal stiffness
                  IF (NODADT > 0) THEN
                    CALL ASSPAR_SUB(NLOC_DMG%STIFNL(:,1),NLOC_DMG%STSKY,NLOC_DMG%ADDCNE,NODFT_NL,
     .                              NODLT_NL,NLOC_DMG%POSI,NLOC_DMG%NNOD,NLOC_DMG%L_NLOC)
                    CALL NLOCAL_DTNODA(NODFT_NL,NODLT_NL,NLOC_DMG,DTNOD_NLOCAL,DT2T)
                  ENDIF
                ENDIF
C------------------------
C       Assembly of Parith/ON forces on Phantom Xfem
C------------------------
                IF(ICRACK3D > 0)THEN
C         /---------------/
                  CALL MY_BARRIER
C         /---------------/
                  CALL ASSPAR_CRK(
     .                 ADSKY_CRK,INOD_CRK ,CRKSKY  ,NODFTSK  ,NODLTSK  ,
     .                 NODENR   ,NODLEVXF ,NODES%ITAB    )
                ENDIF
              ELSEIF(IPARIT==2)THEN

                IF(KDTINT/=0)THEN

!$OMP SINGLE
                  CALL ANCMSG(MSGID=165,ANMODE=ANINFO)
!$OMP END SINGLE

                  CALL ARRET(1)
                ENDIF
C
                CALL ASSPAR3(
     2               NODES%A          ,NODES%AR         ,ITSK       ,NODFTSK   ,
     3               NODLTSK    ,NODES%STIFN      ,NODES%STIFR      ,NODES%ITAB      ,ELEMENT%PON%FSKY       ,
     4               ELEMENT%PON%FSKY       ,INTERFACES%PON%ISKY       ,ELEMENT%PON%ADSKY      ,INTERFACES%PON%FSKYI     ,
     5               WA         ,PARTFTSK   ,PARTLTSK   ,PARTSAV   ,NODES%MS         ,
     6               FTHE       ,FTHESKY    ,FTHESKYI   ,GREFTSK     ,GRELTSK      ,
     7               GRESAV     ,GLOB_THERM%ITHERM_FE   ,GLOB_THERM%INTHEAT    )

              ELSEIF(IPARIT==3)THEN

C Assemblage Parith/ON
                N1 =  1 + NUMNOD
                CALL ASSPAR5(
     1               NTHREAD   ,NUMNOD    ,NODFTSK  ,NODLTSK  ,IRODDL   ,
     2               NPART     ,PARTFTSK  ,PARTLTSK ,NODES%A        ,NODES%AR       ,
     3              PARTSAV    ,NODES%STIFN      ,NODES%STIFR    ,NODES%A(1,N1)  ,NODES%AR(1,N1) ,
     4              NODES%STIFN(N1)  ,NODES%STIFR(N1)  ,NODES%VISCN    ,NODES%VISCN(N1),GREFTSK    ,
     5               GRELTSK     ,GRESAV     ,NGPE     ,NTHPART)
C
              ENDIF

              IF(KDTINT/=0) CALL MODSTI(NODFTSK,NODLTSK,NODES%STIFN,NODES%VISCN,NODES%MS)

!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)
C
              IF(IPARIT==1) DEALLOCATE( FSKY_L )
              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_ASM)
              CALL TRACE_OUT(15)
C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

!       -------------------------------------------
!       check if a NaN appears in acc vectors (only available with /DEBUG/NAN option)
              IF( DEBUG(MACRO_DEBUG_NAN)/=0 )CALL CHECK_NAN_ACC(NCYCLE,NODES)
!       write *.adb files for NON-LOCAL option
              IF (DEBUG(MACRO_DEBUG_ACC)==1.AND.(NLOC_DMG%IMOD>0)) THEN
                IF (ISPMD==0) THEN
                  SIZ = NUMNODG
                ELSE
                  SIZ = 0
                END IF
                IF ( NCYCLE>=DEBSTART .AND.
     .               MOD(NCYCLE-DEBSTART,RSTFREQ)==0 ) THEN
                  CALL SPMD_COLLECT_NLOCAL(NLOC_DMG%FNL(:,1),NLOC_DMG%L_NLOC ,NLOC_DMG%NNOD,
     .                                     NLOC_DMG%POSI    ,NLOC_DMG,SIZ,NODES%NODGLOB,NODES%ITAB     )
                ENDIF
              ENDIF

C----------------------------------
C       ITET2 of S10 Forces condensation; pass 1
C----------------------------------

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (NS10E > 0) THEN
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                GREFTSK   = 1+ITSK*NS10E/ NTHREAD
                GRELTSK   = (ITSK+1)*NS10E/NTHREAD
                CALL S10CNDF1(ICNDS10,NODES%WEIGHT ,IAD_CNDM1,FR_CNDM1,FR_NBCCCND1,
     1                       ADDCNCND,PROCNCND,NODES%A   ,IADCND,FSKYCND,
     2                       ITAGND  ,NODFTSK,NODLTSK,GREFTSK,GRELTSK,
     3                       ITSK  ,NODES%ITAB   ,NODES%STIFN, STIFND)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              END IF
c--------------------------------------
              IF(SOL2SPH_FLAG/=0)THEN
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                DO I=NODFTSK,NODLTSK
                  IF(NODES%MS(I)/=ZERO)THEN
                    IF(NODES%MS(I)-DMSPH(I) < EM03*NODES%MS(I))THEN
                      NODES%MS(I)=ZERO
                    ELSE
                      NODES%MS(I)=MAX(ZERO,NODES%MS(I)-DMSPH(I))
                    END IF
                  END IF
                  DMSPH(I)=ZERO
                ENDDO
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF (INT24USE == 1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
                CALL SPMD_EXCH_I24(IPARI    ,INTERFACES%INTBUF_TAB,NODES%ITAB  ,
     *                             NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                             IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,2,
     *                             INT24E2EUSE )
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
              ENDIF

C
C Communication Interface type20 DAANC6
C
              IF(NBINT20>0.AND.NSPMD>1) THEN
                LENGTH = 21
                LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                CALL SPMD_EXCH_DA20(
     1           INTERFACES%INTBUF_TAB,IPARI,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
     2           LENGTH ,NBINT20,LENR ,INTLIST ,NBINTC )
              ENDIF
C
C Communication ICONTACT AIRBAG
C
              IF(KCONTACT/=0.AND.NSPMD>1) THEN
                LENGTH = 1
                LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                CALL SPMD_EXCH_ICONT(ICONTACT
     +            ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY,LENGTH,LENR)
              ENDIF
C
C Communication IFOAM
C
              IF(IALELAG > 0.AND.NSPMD>1) THEN
                LENGTH = 1
                LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                CALL SPMD_EXCH_ICONT(IFOAM,NODES%BOUNDARY_ADD ,NODES%BOUNDARY,LENGTH,LENR)
              ENDIF
C
#ifdef DNC
              IF (IMADCPL>0)THEN
C   at every cycle except cycle 0, send the Nodes coordinates
                CALL DATA_RECV_MADCPL(NODES%X,NODES%A,NODES%V,NODES%MS,
     .                OUTPUT%DATA%VECT_CONT,MADCLNOD,MADCLFRECV,H3D_DATA,OUTPUT%TH%WFEXT)
              ENDIF
#endif

              IF (ViperCoupling) THEN
C                Viper's contribution to force
                 CALL RadiossViper_ReceiveAccelerations(NUMNOD,NODES%A,NODA_FEXT,VIPER%ITABM1)
              endif

CFP SKIP KINEMATIC FORCES
              IF(ALE%SUB%IALESUB==2 .AND. ALE%SUB%IFSUB==2) GOTO 23

C
C POROUS MEDIA (not parallel)
C---------------------------------------------------------------------
              IF(NUMPOR>0) THEN
                CALL PORO(
     1             GEO    ,NODPOR ,NODES%MS,NODES%X   ,NODES%V     ,
     2             W      ,NODES%A      ,NODES%AR,SKEWS%SKEW,NODES%WEIGHT,
     3             NPORGEO)
              ENDIF
C---------------------------------------------------------------------
C DEBUG OUTPUT ACCELERATION
C---------------------------------------------------------------------
              IF (DEBUG(MACRO_DEBUG_ACC)==1) THEN
                IF (NCYCLE>=DEBSTART .AND.MOD(NCYCLE-DEBSTART,RSTFREQ)==0) THEN
                  IF(NSPMD > 1) THEN
                    IF (ISPMD==0) THEN
                      SIZ = NUMNODG
                    ELSE
                      SIZ = 0
                    END IF
                    CALL SPMD_COLLECT(NODES%A,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB,SIZ)
                  ELSE
                    CALL COLLECT(NODES%A,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB)
                  END IF
                END IF
              END IF

C-----------------------------------------------
C       COMMUNICATION BETWEEN BOUNDARY ELEMENTS AFTER ASSEMBLY
C-----------------------------------------------
              IF (IFRWV > 0) THEN
                IF (NSPMD > 1) THEN
                  LENGTH = 1
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
C FR_WAVE boundary exchange
                  CALL SPMD_EXCH_WAVE(FR_WAVE,NODES%BOUNDARY_ADD ,NODES%BOUNDARY,LENGTH,LENR)
                END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                DO I=NODFTSK,NODLTSK
                  FR_WAVE(I)=ABS(FR_WAVE(I))
                ENDDO

!$OMP END PARALLEL

              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C     Failure front wave
C-----------------------------------------------
              IF (FAILWAVE%WAVE_MOD > 0) CALL UPDATE_FAILWAVE(FAILWAVE)
C----------------------------------
C     PROCESSING SHOOTING NODES
C----------------------------------
              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
              IF (N2D/=0.AND.IDEL7==2) THEN
                IF (IMON>0) CALL STARTIME(TIMERS,6)
                CALL CHKSTIFN(IPARI,NODES%MS,INTERFACES%INTBUF_TAB)
                IF (IMON>0) CALL STOPTIME(TIMERS,6)
C
C IDEL7NG : global flag deleted segments/nodes int. type7, type2
C IDEL7NG = 0 => nothing
C IDEL7NG = 1 | 2 => deleted segments/nodes (for at least 1 interface)
C IDEL7NOK = 1 : need all interface such as IPARI(17)= 1 | 2
              ELSEIF(IDEL7NG>=1.OR.PDEL>0) THEN
C        IF (NCYCLE==97) IDEL7NOK = 1
C        IF (NCYCLE==98) IDEL7NOK = 1
                IF (NSPMD>1.AND.(IDEL7NG>=1.OR.PDEL>0)) THEN
C recuperation IDEL7NOK global
                  IF (IMONM > 0 ) CALL STARTIME(TIMERS,76)
                  CALL SPMD_ALLGLOB_ISUM9(IDEL7NOK,1)
                  IF (IMONM > 0 ) CALL STOPTIME(TIMERS,76)
                  IDEL7NOK = MIN(1,IDEL7NOK)
                ENDIF
                IDEL7NOK_SAV = IDEL7NOK
                IF (R2R_SIU==1.AND.IDEL7NG>=1) THEN
                  CALL GET_SHMBUF_C(IDEL7NOK_R2R,2)
                  IDEL7NOK = IDEL7NOK+IDEL7NOK_R2R
                  IDEL7NOK = MIN(1,IDEL7NOK)
                ENDIF
C Warning WA used on 2*NUMNOD (NUMNOD + NUMNOD SPECIFIC SPMD)
                IF ((IDEL7NG>=1.AND.IDEL7NOK==1).OR.(PDEL>0.AND.IDEL7NOK==1)) THEN
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  IF((INT24USE==1.OR.NINTER25/=0).AND.(IDEL7NG>=1.AND.IDEL7NOK==1))THEN
                    INDSEGLO(2:NINTER+1)=0
                    INDSEGLO(1)=1
                  ENDIF


C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                  IF (IMONM > 0 ) CALL STARTIME(TIMERS,29)
                  NINDEXP = 0
                  CHECK_NEIGH_FLAG_RES = 0

c         allocate(nodes%deleted_node(2*numnod)) ! working array to mark nodes connected to deleted element
c         allocate(nodes%work_array_node(nthread*numnod)) ! working array to mark nodes (connected to active element or deleted element)
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK,omp_address)
C Init var parallel SMP
                  CALL SMP_INIT(
     1               ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2               IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3               GREFTSK,GRELTSK)
                  omp_address = 1 + itsk*numnod

                  IF ((IDEL7NG>=1.AND.IDEL7NOK==1).OR.(PDEL>0.AND.IDEL7NOK==1)) THEN
                    CALL TAGOFF3N(NODES,
     1                 GEO        ,IXS       ,IXS(L1)   ,IXS(L1)   ,IXS(L3)  ,IXQ    ,
     2                 ELEMENT%SHELL%IXC         ,IXT      ,IXP       ,IXR       ,IXTG     ,
     3                 nodes%deleted_node,NODFTSK  ,NODLTSK   ,IPARG     ,ELBUF    ,ITSK   ,
     4                 IXTG1    ,NODES%BOUNDARY_ADD  ,NODES%BOUNDARY   ,NODES%ITAB     ,
     5                 ADDCNEL     ,CNEL     ,KXSP      ,ELBUF_TAB ,TAGEL    ,IEXLNK ,
     6                 IGRNOD      ,DD_R2R   ,DD_R2R_ELEM,SDD_R2R_ELEM,IDEL7NOK_SAV  ,
     7                 IDEL7NOK_R2R,TAGTRIMC,TAGTRIMTG,S_ELEM_STATE,ELEM_STATE,
     8                 SHOOT_STRUCT,SHOOT_STRUCT%GLOBAL_NB_ELEM_OFF)
                    ! ---------------------
                    ! check if a node is deactivated and deactivate all the corresponding secondary nodes
                    CALL CHECK_NODAL_STATE( ITSK,nodes%deleted_node,NEWFRONT,INTERFACES%INTBUF_TAB,SHOOT_STRUCT%SIZE_SEC_NODE,
     .                     SHOOT_STRUCT%SHIFT_S_NODE,SHOOT_STRUCT%INTER_SEC_NODE,SHOOT_STRUCT%SEC_NODE_ID)
                    ! ---------------------

                    ! ---------------------
                    ! check if a surface/edge must be deactivated and save the surface/edge id

                    IF(ITSK==0) THEN
                      CALL FIND_SURFACE_INTER( NODES%ITAB  ,SHOOT_STRUCT  ,IXS  ,IXS(L1)  ,ELEMENT%SHELL%IXC  ,
     .                                         IXTG  ,
     .                                         NGROUP,NPARG,IGROUPS,IPARG )
                      CALL FIND_EDGE_INTER( NODES%ITAB,SHOOT_STRUCT,IXS,IXS(L1),
     1                                      ELEMENT%SHELL%IXC,IXTG,IXQ,IXT,IXP,
     2                                      IXR,GEO,NGROUP,IGROUPS,IPARG )
                    ENDIF
                    CALL MY_BARRIER( )
                    ! ---------------------

                    ! ---------------------
                    ! exchange of surfaces (ie. 4 nodes) to deactivate and deactivation
                    ! ONLY FOR LOCAL SURFACE / REMOTE ELEMENT
                    IF(NSPMD>1) THEN
                      IF(ITSK==0) CALL SPMD_EXCH_DELETED_SURF_EDGE( NODES%BOUNDARY_ADD,NODES,SHOOT_STRUCT,
     .                                     INTERFACES%INTBUF_TAB,NEWFRONT,
     .                                     IPARI,GEO,
     .                                     IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                     ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL )
                      CALL MY_BARRIER()
                    ENDIF
                    ! ---------------------

                    ! ---------------------
                    ! loop over the surface id and deactivate the surface
                    ! ONLY FOR LOCAL SURFACE / LOCAL ELEMENT

                    CALL CHECK_SURFACE_STATE( ITSK,SHOOT_STRUCT%SAVE_SURFACE_NB,SHOOT_STRUCT%SAVE_SURFACE,
     .                                        SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,
     .                                        IPARI,GEO,
     .                                        IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                        ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL,SHOOT_STRUCT )

                    ! loop over the edge id and deactivate the edge
                    ! ONLY FOR LOCAL EDGE / LOCAL ELEMENT
                    CALL CHECK_EDGE_STATE( ITSK,SHOOT_STRUCT%SAVE_M_EDGE_NB,SHOOT_STRUCT%SAVE_S_EDGE_NB,
     .                                     SHOOT_STRUCT%SAVE_M_EDGE,SHOOT_STRUCT%SAVE_S_EDGE,
     .                                     SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                                     IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,IXS(L1),
     .                                     ADDCNEL,CNEL,nodes%work_array_node(omp_address),TAGEL,SHOOT_STRUCT )
                    ! ---------------------

                    ! ---------------------
                    ! exchange of deactivated surfaces (ie. 4 nodes) to deactivate to the neighbourhood
                    ! ONLY FOR REMOTE SURFACE + interface type 24 or 25
                    IF(INT24USE>0.OR.NINTER25/=0) THEN
                      IF(ITSK==0) CALL CHECK_REMOTE_SURFACE_STATE( SHOOT_STRUCT%NUMBER_REMOTE_SURF,SHOOT_STRUCT%REMOTE_SURF,
     .                                                             SHOOT_STRUCT%SHIFT_INTERFACE,INTERFACES%INTBUF_TAB,
     .                                                             IPARI,NODES%BOUNDARY_ADD,SHOOT_STRUCT  )
                      CALL MY_BARRIER()
                      IF(NINTER25/=0.AND.INTERFACES%PARAMETERS%INT25_EROSION_SOLID > 0) THEN
                        IF(ITSK==0) THEN
                          CHECK_NEIGH_FLAG = SHOOT_STRUCT%NUMBER_NEW_SURF + shoot_struct%NUMBER_REMOTE_SURF
                          IF(NEED_COMM_INT25_SOLID_EROSION(ISPMD+1)) THEN
                            CHECK_NEIGH_FLAG = SHOOT_STRUCT%NUMBER_NEW_SURF + shoot_struct%NUMBER_REMOTE_SURF
                            CALL SPMD_ALLREDUCE(CHECK_NEIGH_FLAG,CHECK_NEIGH_FLAG_RES,1,SPMD_MAX,COMM_INT25_SOLID_EROSION)
                          ELSEIF(NSPMD==1) THEN
                            CHECK_NEIGH_FLAG_RES = CHECK_NEIGH_FLAG 
                          ENDIF
                          IF(CHECK_NEIGH_FLAG_RES > 0 ) THEN
                            CALL GET_NEIGHBOUR_SURFACE( ISPMD,NSPMD,NINTER25,NPARI,NINTER,
     .                                                  NBINTC,NIXS,NIXC,NIXTG,NUMNOD,
     .                                                  NUMELS,NUMELC,NUMELTG,S_ELEM_STATE,
     .                                                  NBDDEDGT,NBDDEDG_MAX,
     .                                                  ELEM_STATE,IPARI,INTLIST,NODES,
     .                                                  NEWFRONT,IXS,ELEMENT%SHELL%IXC,IXTG,
     .                                                  NODES%BOUNDARY_ADD,NODES%X,
     .                                                  INTERFACES%INTBUF_TAB,INTERFACES%SPMD_ARRAYS,SHOOT_STRUCT,
     .                                                  NEED_COMM_INT25_SOLID_EROSION)
                          ENDIF
                        ENDIF
                        CALL MY_BARRIER()
                      ENDIF
                    ENDIF
                    ! ---------------------
                  ENDIF

                  IF (IDEL7NG>=1.AND.IDEL7NOK==1) THEN
                    CALL CHKSTFN3N(NODES,
     1                 IPARI   ,GEO         ,IXS      ,IXQ         ,ELEMENT%SHELL%IXC     ,IXT       ,
     2                 IXP     ,IXR         ,IXTG     ,nodes%deleted_node,IPARG   ,ITSK      ,
     3                 NEWFRONT,nodes%work_array_node(omp_address) ,NODES%MS       ,NODES%IN ,OUTPUT%DATA%SCAL_DMAS,NODES%ITAB  ,
     4                 NODES%ITABM1  ,ADDCNEL     , CNEL    ,INDIDEL     ,NINDEX1 ,NINDEX2   ,
     5                 NINDEX3 ,NINDEX4     ,TAGEL    ,INT24USE    ,IBUFSEGLO ,INDSEGLO,
     6                 IBUFIDEL ,INTERFACES%INTBUF_TAB,NODES%BOUNDARY_ADD)

                  ENDIF

                  IF (PDEL>0.AND.IDEL7NOK==1) THEN
                    CALL CHKLOAD(
     1                 IBCL    ,IXS     ,IXQ         ,ELEMENT%SHELL%IXC    ,IXT        ,IXP     ,
     2                 IXR     ,IXTG    ,nodes%deleted_node,ITSK   ,nodes%work_array_node(omp_address),NODES%ITAB    ,
     3                 NODES  ,ADDCNEL ,CNEL        ,TAGEL  ,IPARG      ,GEO     ,
     4                 IBUFPDEL,NINDEXPDEL,NINDEXP   ,NPRESLOAD,LOADP_TAGDEL      ,
     5                 ILOADP  ,LLOADP  ,NODES%BOUNDARY_ADD)

                  ENDIF

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
                  IF (IMONM > 0 ) CALL STOPTIME(TIMERS,29)
                  CALL DEALLOC_SHOOT_INTER( SHOOT_STRUCT )
                ENDIF
              ENDIF


c ========================================================================================
C                                     node splitting
C  uncomment the following code for an example of node splitting (using non-physical deformation critera)

C            numnod_old = numnod
C            numnodm_old = numnodm
C            call test_jc_shell_detach(nodes, element, interfaces, npari, ninter, ipari, numnod,
C    .                numnodg, elbuf_tab, ngroup, ngrouc, nparg, iparg, igrouc, numelc, ispmd, nspmd,
C    .                new_crack)


C            if(new_crack > 0) then
C              ! if at least 1 node has been detached, some data must be re-initialized
C              numnodm = numnodm_old + new_crack
C              CALL INIT_NODAL_STATE( IPARI,SHOOT_STRUCT,INTERFACES%INTBUF_TAB,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
C    .                              NODES%ITAB,NODES,GEO,ADDCNEL,CNEL,
C    .                              IXS,ELEMENT%SHELL%IXC,IXT,IXP,IXR,IXTG,
C    .                              SIZE_ADDCNEL,SIZE_CNEL ,
C    .                              numelsg,numelqg,numelcg,numeltrg,numelpg,
C    .                              numelrg,numeltgg , IXS(L1))


C              IF(IDEL7NG>0.OR.IRAD2R>0.OR.ALEMUSCL_Param%IALEMUSCL>0.OR.PDEL>0) THEN
C                SIZE_ADDCNEL = NUMNOD+1
C                SIZE_CNEL = LCNEL
C                NELEML = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
C                S_ELEM_STATE = NELEML
C                DEALLOCATE(CNEL)
C                ALLOCATE(CNEL(0:SIZE_CNEL))
C                DEALLOCATE(ADDCNEL)
C                ALLOCATE(ADDCNEL(0:SIZE_ADDCNEL))
C                DEALLOCATE(ADDTMPL)
C                ALLOCATE(ADDTMPL(0:NUMNOD+1))
C                NELEML = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
C                ALEMUSCL_Buffer%pCNEL => CNEL
C                ALEMUSCL_Buffer%pADDCNEL => ADDCNEL
C                ALEMUSCL_Buffer%pADDTMPL => ADDTMPL
C                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
C                L2 = L1+6*NUMELS10
C                L3 = L2+12*NUMELS20

C                CALL CHKINIT(
C    2              IXS        ,IXQ       ,ELEMENT%SHELL%IXC       ,IXT         ,IXP       ,
C    3              IXR        ,IXTG      ,IXS(L1)       ,IXS(L2)     ,
C    4              IXS(L3)      ,IXTG1     ,GEO       ,ADDCNEL     ,CNEL      ,
C    5              ADDTMPL    ,IPARG     )

C              ENDIF
C              CALL ALLOCATE_OUTPUT_DATA(OUTPUT,NUMNOD)
C              LCNE0 = SIZE(ELEMENT%PON%PROCNE)
C            endif

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C elt deletion => need to rebuild ams mass matrix ...
              IF(IDTMINS>=1.OR.IDTMINS_INT/=0)THEN
                IF(MCHECK==0)THEN
                  ISMSNOK=0
                  IF(IDEL7NG==0)THEN
                    IF (NSPMD>1) THEN
C Get IDEL7NOK global
                      IF (IMONM > 0 ) CALL STARTIME(TIMERS,76)
                      CALL SPMD_ALLGLOB_ISUM9(IDEL7NOK,1)
                      IF (IMONM > 0 ) CALL STOPTIME(TIMERS,76)
                      IDEL7NOK = MIN(1,IDEL7NOK)
                    ENDIF
                  ENDIF
                  ISMSNOK=IDEL7NOK
                ELSE
                  ISMSNOK=1
                END IF
              END IF

#ifdef DNC
              IF(IMADCPL /=0)THEN
C  ---------------------------------------------------------
C   Radioss Madymo Coupling
C  ---------------------------------------------------------
C   IF IDEL7NOK is > Deleted elements were found
C   MAD_FAIL_ELEMENTS is upgraded
C
C   If one domain has new deleted elements, MADYMO_DEL is set to 1
C   MADYMO_DEL is globalized with SPMD_ALLGLOB_ISUM9
C   if Positive, all domains send the info during Time Step exchange
C
C   --------------------------------------------------------
C   Do not communicate IDEL7NOK if already done. (IDEL7NG >0)
C  ---------------------------------------------------------

                MADYMO_DEL_GLOBAL=0
                MADYMO_DEL=0

                IF (IDEL7NOK > 0 ) THEN
                  CALL MAD_ELFAIL( ELBUF_TAB,IPARG,
     *                             MADSOL,MADSH3,MADSH4,
     *                             MAD_TAG_SOL, MAD_TAG_SH,MAD_TAG_TG,
     *                             MADYMO_DEL,
     *                             MAD_FAIL_ELEMENTS)

                ENDIF

                MADYMO_DEL_GLOBAL = MADYMO_DEL

                CALL SPMD_ALLGLOB_ISUM9(MADYMO_DEL_GLOBAL,1)
              ENDIF
#endif
C  ---------------------------------------------------------
              IDEL7NOK=0
              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
C
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------
C       EXTERNAL FORCES FROM SECTIONS
C----------------------------------
              IF(ISECUT/=0)CALL SECTION_FIO (
     1                NSTRF ,NODES%V,NODES%VR,
     2                NODES%A     ,NODES%AR     ,SECBUF,NODES%MS,NODES%IN,
     3                NODES%WEIGHT,IAD_CUT,FR_CUT, OUTPUT%TH%WFEXT)
C-----------------------------------------------------
C     SPOTWELD ELEMENT CLUSTERS
C-----------------------------------------------------
              IF (NCLUSTER > 0) THEN
                CALL CLUSTERF(CLUSTER ,ELBUF_TAB,NODES%X  ,NODES%A  ,NODES%AR  ,
     .                        SKEWS%SKEW    ,IXS      ,IPARG ,FCLUSTER,MCLUSTER,
     .                        H3D_DATA,GEO      )
              ENDIF

C-----------------------------------------------------
C         KINEMATIC CONDITIONS FOR SEATBELTS
C-----------------------------------------------------

              IF (NSLIPRING + NRETRACTOR + N_ANCHOR_REMOTE > 0) THEN
                CALL KINE_SEATBELT_FORCE(NODES%A,NODES%STIFN,FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE)
              ENDIF

C-----------------------------------------------------
C         INTERFACES 18 KINE
C-----------------------------------------------------
              IF(NINTER /= 0.and.IALE+IEULER /= 0.and.
     .           INT18KINE == -1)THEN

C========================================================================================
C                    PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                ITSK = OMP_GET_THREAD_NUM()

C        /---------------/
                CALL MY_BARRIER
C        /---------------/
                CALL I18MAIN_KINE_1(OUTPUT, IPARI,INTERFACES%INTBUF_TAB,NODES%X       ,NODES%V       ,
     2                   NODES%A          ,NODES%ISKEW     ,SKEWS%SKEW     ,NODES%ICODT   ,WA      ,
     3                   NODES%MS         ,NODES%ITAB      ,ITSK+1   ,NODES%KINET   ,NODES%STIFN   ,
     4                   MTF        ,CAND_SAV  ,INT18ADD ,NODES%BOUNDARY_ADD,NODES%BOUNDARY ,
     5                   TAGPENE    ,H3D_DATA  ,MULTI_FVM,ALE_CONNECTIVITY%NE_CONNECT,XCELL,XCELL_REMOTE)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              ENDIF
C------------------------
C         INTERFACES TIED
C--- //0 ----------------
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
              IF(NINTER/=0)THEN
                IF (IMON>0) THEN
                  CALL STARTIME(TIMERS,6)
                  CALL STARTIME(TIMERS,TIMER_CONTSORT)
                ENDIF
                IF (IMONM > 0)CALL STARTIME(TIMERS,28)
!0.
                DO K=0,NHIN2
                  CALL INTTI1(nodes,
     1               IPARI  ,NODES%X      ,NODES%V        ,NODES%A      ,
     2               NODES%VR     ,NODES%AR     ,WA       ,NODES%MS     ,NODES%IN     ,NODES%WEIGHT      ,
     3               NODES%STIFN  ,NODES%STIFR  ,K        ,NODES%ITAB   ,FR_I2M ,IAD_I2M     ,
     4               ADDCNI2,PROCNI2,IADI2    ,I2MSCH ,DMAS   ,OUTPUT%DATA%SCAL_DMAS,
     5               SKEWS%SKEW   ,I2SIZE ,FR_NBCCI2,OUTPUT%DATA%SCAL_DINER   ,IGEO,BUFGEO ,
     6               FSAV   ,NPC    ,TF       ,OUTPUT%DATA%VECT_CONT2 ,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
     7               NATIV_SMS,DMINT2,OUTPUT%DATA%SCAL_DAMA2,NB_FRI2M ,FR_LOCI2M,
     8               DT2T  ,NELTST ,ITYPTST   ,INTERFACES%INTBUF_TAB ,NODES%TEMP     ,NODES%MCP   ,
     9               FTHE  ,CONDN  ,GLOB_THERM,
     A               H3D_DATA,T2FAC_SMS,OUTPUT%DATA%VECT_PCONT2,NPCONT2)
                ENDDO
                IF (IMONM > 0) CALL STOPTIME(TIMERS,28)
                IF (IMON>0) THEN
                  CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                  CALL STOPTIME(TIMERS,6)
                ENDIF
C
                IF((IDTMINS/=0.OR.IDTMINS_INT/=0).AND.NCYCLE==0)ISMSCH=1

              ENDIF
C
C----------------------------------
C       ITET2 of S10 Forces condensation; pass 2
C----------------------------------

C========================================================================================
C                    PARALLEL SECTION (SMP)
C========================================================================================

              IF (NS10E > 0) THEN
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                GREFTSK   = 1+ITSK*NS10E/ NTHREAD
                GRELTSK   = (ITSK+1)*NS10E/NTHREAD
                CALL S10CNDF2(ICNDS10,NODES%WEIGHT ,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1                       ADDCNCND,PROCNCND,NODES%A   ,IADCND,FSKYCND,
     2                       ITAGND  ,NODFTSK,NODLTSK,GREFTSK,GRELTSK,
     3                       ITSK  ,NODES%ITAB   ,NODES%STIFN  , STIFND)
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                IF (NCYCLE==0.OR.MCHECK/=0)
     1             CALL CND_DMASI2(ICNDS10,NKEND,IMAP2ND,MASI2ND0,NODES%MS  ,NODES%WEIGHT)
              END IF
C
              IF(INTPLYXFEM > 0) THEN
                CALL I24PXFEM(
     1             IPARI  ,INTERFACES%INTBUF_TAB ,WAGAP,NODES%BOUNDARY_ADD,NODES%BOUNDARY)
              ENDIF
C
C----------------------------------------------------------
C         RBE2 - FORCES and MOMENTS (Torque)
C----------------------------------------------------------
              IF (NRBE2>0.OR.R2SIZE>0) THEN
                CALL MY_BARRIER
                IF(ITASK==0)THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  CALL RBE2T1(IRBE2 ,LRBE2 ,NODES%X      ,NODES%A      ,NODES%AR     ,
     1                        NODES%MS    ,NODES%IN    ,SKEWS%SKEW   ,NODES%WEIGHT ,IAD_RBE2,
     2                        FR_RBE2M,NMRBE2,NODES%STIFN ,NODES%STIFR ,R2SIZE )
                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)

                END IF
              ENDIF
C----------------------------------------------------------
C         RBE3 - FORCES AND MOMENTS
C----------------------------------------------------------
              IF (NRBE3>0) THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,45)

                CALL RBE3T1(RBE3         ,NODES        ,SKEWS%SKEW,
     1                      DMAS         ,OUTPUT%DATA%SCAL_DMAS ,DINER,
     2                      OUTPUT%DATA%SCAL_DINER ,H3D_DATA     ,  DT1,
     3                      TT           ,IMPL_S    )

                IF (IMONM > 0) CALL STOPTIME(TIMERS,45)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
              ENDIF

C------------------------
C     test of mass and inertia on main nodes of interf. type 2
C------------------------
              IF(TT==ZERO.AND.IALE+IEULER+GLOB_THERM%ITHERM==0)THEN

                NEGMAS=0
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

               CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL CHKMSIN(NODFTSK,NODLTSK,NODES%ITAB,NODES%MS,NODES%IN,NEGMAS)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
C Implicit barrier on NEGMAS
                IF(NEGMAS/=0) CALL ARRET(2)
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C
C Assemblage TYPE21
C
              IF(NINTSTAMP/=0)THEN
                CALL INTSTAMP_ASS(INTSTAMP,NODES%MS ,NODES%IN ,NODES%A ,NODES%AR ,
     .                            NODES%STIFN   ,NODES%STIFR  ,NODES%WEIGHT, OUTPUT%TH%WFEXT)
              END IF
C-----------------------------------------------------
C         RIGID BODY: SUM forces, stiff.
C-----------------------------------------------------

              IF(NRBYKIN>0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,40)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                CALL RBYSENS(
     1           IPARG,IPARI       ,NODES%MS    ,NODES%IN   ,
     2           IXS  ,IXQ  ,ELEMENT%SHELL%IXC   ,IXT   ,IXP  ,
     3           IXR  ,SKEWS%SKEW ,NODES%ITAB  ,NODES%ITABM1,ISKWN,
     4           NPBY ,WA   ,LPBY  ,ELEMENT%PON%FSKY  ,NSENSOR,
     5           RBY  ,NODES%X    ,NODES%V     ,NODES%VR    ,IXTG ,
     6           IGRV ,LGRAV,SENSORS%SENSOR_TAB,NODES%A     ,NODES%AR   ,
     7           FSAV  ,NODES%STIFN ,NODES%STIFR,OUTPUT%DATA%FOPT,NODES%WEIGHT,
     8           DMAS  ,DINER ,BUFSF,FR_RBY2,PARTSAV ,
     9           IPART ,ELBUF_TAB,ICFIELD,LCFIELD,NODES%TAG_S_RBY)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL

                CALL RBYFOR(TIMERS,
     1            RBY              ,NODES%A      ,NODES%AR        ,NODES%X         ,NODES%VR       ,
     2            FSAV             ,NODES%IN     ,NODES%STIFN     ,NODES%STIFR     ,OUTPUT%DATA%FOPT ,
     3            LPBY             ,NPBY         ,NODES%WEIGHT    ,NODES%MS        ,NODES%V        ,
     4            IGRSURF          ,BUFSF        ,NODES%ICODR     ,NODES%ISKEW     ,SKEWS%SKEW     ,
     5            KINDRBY          ,IAD_RBY      ,FR_RBY6,RBY6    ,IRBKIN_L        ,
     6            NRBYKIN_L        ,NATIV_SMS    ,SENSORS%SFSAV   ,SENSORS%FSAV    ,SENSORS%STABSEN,
     7            SENSORS%TABSENSOR,NODREAC      ,FTHREAC         ,CPTREAC         ,DAMPR,
     8            SDAMP            ,DAMP         ,NDAMP_VREL      ,ID_DAMP_VREL    ,IGRNOD         ,
     9            NODES%TAG_S_RBY       ,IPARIT       ,OUTPUT%TH%WFEXT    ,NDAMP_VREL_RBYG ,SIZE_RBY6_C    ,
     A            RBY6_C,NHIER_RBY)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,40)
C-----------------------------------------------------
              ENDIF
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C-----------------------------------------------------
C         FORCES  FLEXIBLE BODIES
C-----------------------------------------------------
              IF (NFXBODY>0)  THEN
                CALL FXBYFOR(OUTPUT, FXBIPM, FXBRPM,  FXBNOD,  FXBMOD,  FXBGLM,
     .                       FXBCPM, FXBCPS,  FXBLM ,  FXBFLS,  FXBDLS,
     .                       FXBDEP, FXBVIT,  FXBACC,  NODES%A,       NODES%AR,
     .                       NODES%X,      FXBMVN,  FXBMCD,  FXBSE,   FXBSV,
     .                       FXBELM, FXBSIG,  ELBUF,   PARTSAV, ELBUF_TAB,
     .                       FSAV,   FXBFP,   FXBEFW,  FXBFC,   NODES%D,
     .                       DT2T,   ITYPTST, NELTST,  FXBGRVI, FXBGRVR,
     .                       IGRV,   NPC,     TF    ,  FXBGRP,  FXBGRW ,
     .                       IPARG , NSENSOR,SENSORS%SENSOR_TAB,NODES%BOUNDARY_ADD, NODES%BOUNDARY,
     .                       AGRV  ,PYTHON)
              END IF

C----------------------------------------------------------
C         ADAPTIVE MESHING : FORCES AND STIFNESS FROM SECND TO MAIN
C-----------------------------------------------------
              IF(NADMESH/=0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,37)
                CALL ADMFOR0(ELEMENT%SHELL%IXC, IPART(K3), IXTG, IPART(K8), IPART,
     1                       NODES%A  , NODES%STIFN    ,    NODES%AR, NODES%STIFR   ,NODES%X    ,
     2                       SH4TREE,SH3TREE,STCND,     FTHE,CONDN,
     .                       GLOB_THERM%NODADT_THERM,GLOB_THERM%ITHERM_FE)
                IF (IMON>0) CALL STOPTIME(TIMERS,37)
              END IF
C----------------------------------------------------------
C         RIGID MATERIAL
C----------------------------------------------------------
              IF (IRIGID_MAT > 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
                CALL RMATFORP(TIMERS,
     1          NODES%A        ,NODES%AR        ,NODES%X           ,NODES%VR          ,NODES%IN      ,
     2          NODES%STIFN    ,NODES%STIFR     ,IRBYM       ,LNRBYM      ,RBYM    ,
     3          ICODRBYM ,NODES%WEIGHT ,NODES%MS     ,NODES%V      ,FR_RBYM ,
     4          IAD_RBYM ,ARBYM  ,VRBYM  ,ARRBYM ,VRRBYM  ,
     5          KINDRBYM ,RBYM6  )

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

              ENDIF

C----------------------------------------------------------
C     Rigid wall - Force node (moving RWALL)
C----------------------------------------------------------
              IF (NRWALL>0) THEN 
                CALL RGWALF(NODES%A ,RWALL%RWBUF ,RWALL%NPRW ,NODES%MS )
                IF (RWALL%nrwall_pen>0 .AND. IMPL_S==0) THEN 
                   nsect_offset = nsect + nintsub + ninter 
!$OMP PARALLEL
                  CALL RGWAL0_PEN(
     1            NODES%X           ,NODES%A        ,NODES%V      ,NODES%MS       ,NUMNOD            ,
     2            FSAV(1,NINTER+1)  ,NTHVKI         ,FRWL6        ,NODES%WEIGHT_MD,NCYCLE            ,
     3            OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)),SENSORS%SFSAV,SENSORS%FSAV ,SENSORS%STABSEN   ,
     4            SENSORS%TABSENSOR,nsect_offset,NODES%STIFN   ,DT1               ,NSPMD             , 
     5            RWALL%NRWALL,RWALL        )
!$OMP END PARALLEL
                END IF 
              END IF 

C-----------------------------------------------
C     SELECTIVE MASS SCALING
C-----------------------------------------------
              IF( IDTMINS == 1 .AND.
     .           (ISMSCH/=0.OR.NCYCLE==0.OR.ISMSNOK/=0.OR.IADMESH/=0))THEN
C
C       Obsolete
              ELSEIF(IDTMINS == 2.OR.IDTMINS_INT/=0)THEN
C
                NSGDONE=1
                NRBDONE=1

                IF (IMON>0) CALL STARTIME(TIMERS,39)
                IF (IMON>0) CALL STARTIME(TIMERS,75)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS

C       Sorting of ISKYI_SMS and additional connections for TYPE2 + contact
                CALL SPMD_SORT_SMS(ISKYI_SMS,MSKYI_SMS,FR_SMS)
C
                IF(NSPMD > 1)THEN
                  CALL SPMD_NLIST_SMS(FR_SMS,FR_RMS)
                END IF
                IF (ALLOCATED(MSKYI_FI_SMS)) DEALLOCATE(MSKYI_FI_SMS)
                IF (ALLOCATED(LIST_SMS)) DEALLOCATE(LIST_SMS)
                IF (ALLOCATED(LIST_RMS)) DEALLOCATE(LIST_RMS)
                IF (ALLOCATED(LIST_RMS)) DEALLOCATE(LIST_RMS)
                IF (ALLOCATED(SMS_VFI)) DEALLOCATE(SMS_VFI)
                CALL MY_ALLOC(MSKYI_FI_SMS,FR_RMS(NSPMD+1))
                CALL MY_ALLOC(LIST_SMS,FR_SMS(NSPMD+1))
                CALL MY_ALLOC(LIST_RMS,FR_RMS(NSPMD+1))
                CALL MY_ALLOC( SMS_VFI,3,FR_RMS(NSPMD+1)+FR_SMS(NSPMD+1) )

                IF (ALLOCATED(MW6)) DEALLOCATE(MW6)
                if (IPARIT /=0) then
                  sz_mw6 = 3*NUMNOD
                  CALL MY_ALLOC(MW6,6,sz_mw6)
                else
                  sz_mw6 = 1
                  CALL MY_ALLOC(MW6,6,1)
                endif
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C
                CALL SMS_BUILD_MAT_2(
     1                     ITSK     ,NODFTSK ,NODLTSK   ,
     2                     ELEMENT%SHELL%IXC      ,IPARG   ,IXS       ,IXT       ,IXP       ,
     3                     IXR      ,IXTG    ,TAGNOD_SMS,NODES%MS        ,NODES%MS0       ,
     4                     INDX1_SMS,INDX2_SMS,JAD_SMS  ,JDI_SMS   ,LT_SMS    ,
     .                     KAD_SMS ,KDI_SMS  ,LTK_SMS   ,PK_SMS    ,NODII_SMS ,
     5                     JADC_SMS ,JADS_SMS ,JADT_SMS ,JADP_SMS ,JADR_SMS  ,
     6                     JADTG_SMS,DIAG_SMS ,TAGPRT_SMS,TAGREL_SMS,
     7             IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),IPART(I15E),
     8             IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),NODES%BOUNDARY_ADD   ,
     9                     NODES%BOUNDARY  ,NPBY    ,LPBY  ,TAGSLV_RBY_SMS ,LAD_SMS  ,
     A                     JSM_SMS ,DMELTG    ,DMELC      ,MSKYI_SMS,
     B                     ISKYI_SMS,JADI_SMS,JDII_SMS  ,LTI_SMS    ,NODXI_SMS,
     C                     DMELS    ,DMELTR  ,DMELP     ,DMELRT     ,IGEO     ,
     D                     FR_SMS   ,FR_RMS  ,ELBUF     ,IPARI      ,INTERFACES%INTBUF_TAB,
     E                     NODES%KINET    ,TAGSLV_I21_SMS,JADI21_SMS,INTSTAMP,
     F                     IXS(L1),JADS10_SMS,ILINK     ,LLINK      ,NNLINK   ,
     G                     LNLINK   ,TAG_LNK_SMS,LJOINT ,IADCJ      ,FR_CJ    ,
     H                     NODES%ITAB  ,NODES%WEIGHT  ,DMINT2    ,ELBUF_TAB,TAGMSR_RBY_SMS,
     I                     RWALL%NPRW  ,RWALL%LPRW    ,RWALL%FR_WALL   ,NRWL_SMS      ,RBY      ,
     J                     NODES%X     ,NODES%A       ,NODES%AR        ,NODES%IN            ,NODES%V        ,
     K                     NODES%VR    ,IRBE2   ,LRBE2     ,RBE3%IRBE3         ,RBE3%LRBE3    ,
     L                     RBE3%mpi%IAD_RBE3 ,RBE3%mpi%FR_RBE3 ,NATIV_SMS,T2MAIN_SMS,T2FAC_SMS,
     M                     MSKYI_FI_SMS, LIST_SMS,LIST_RMS,SZ_mw6,MW6)
c
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                PTR_SMS => NODXI_SMS

                ISMSCH=0
                IF (IMON>0) CALL STOPTIME(TIMERS,39)
                IF (IMON>0) CALL STOPTIME(TIMERS,75)

              ENDIF
C----------------------------------------------------------
C     SCALE TIME STEP
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
C--- COUPLAGE RADIOSS 2 RADIOSS
              IF (IRAD2R /= 0) THEN
                IF (NSPMD>1) CALL SPMD_BARRIER()
                CALL R2R_EXCHANGE(
     1            IEXLNK   ,IGRNOD  ,NODES%D       ,NODES%V       ,NODES%VR     ,
     2            NODES%A        ,NODES%AR      ,NODES%MS      ,NODES%IN     ,NODES%STIFN   ,NODES%STIFR  ,
     3            R2R_ON   ,DD_R2R  ,NODES%WEIGHT ,NODES%BOUNDARY_ADD,NODES%BOUNDARY ,RBY    ,
     4            NODES%XDP      ,NODES%X       ,DD_R2R_ELEM, SDD_R2R_ELEM,OFF_SPH_R2R,
     5            NUMSPH_GLO_R2R,NLOC_DMG)
C
                IF (FLG_SPHINOUT_R2R/=0) THEN
                  DO I=1,NUMNOD
                    IF (OFF_SPH_R2R(I)==2) THEN
                      OFF_SPH_R2R(I) = 1
                    ELSE
                      OFF_SPH_R2R(I) = 0
                    ENDIF
                  END DO
                ENDIF
C
              ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------
  23          CONTINUE

C----------------------------------------------------------
C       INTER/TYPE21 TIME STEP
C----------------------------------------------------------
              IF(NINTSTAMP/=0)THEN
                CALL INTSTAMP_DT(INTSTAMP,IPARI,NELTST,ITYPTST,DT2T,
     .                           PTR_SMS ,DIAG_SMS,NODES%MS ,NODES%V      ,NODES%STIFN,
     .                           NODES%STIFR   )
              END IF

              IMSCH=0
!              
              IF (FLG_DAMP_FUNCT==1) THEN
                CALL DAMPING_FUNCT_INI(DAMPR, NRDAMP, NDAMP,   TT,IRODDL)
              ENDIF
              IF (FLG_DTNODAMP==1) THEN
C---------------NODAL TIME STEP FOR DAMPING-----------------
                IF (IDAMP_RDOF==NDAMP)
     +          CALL DTNODAMP(NODES%ITAB  ,NODES%MS  ,NODES%IN  ,NODES%STIFN  ,NODES%STIFR  ,DT2T ,
     1                        NODES%WEIGHT        ,IGRNOD   ,DAMPR ,ISTOP ,
     2                      IDAMP_RDOF_TAB,ICONTACT,ELEMENT%SHELL%IXC,NODES%X)
                IF (NDAMP>0 .OR. ISTAT==3)
     +          CALL DTNODARAYL(NODES%MS       ,NODES%IN    ,NODES%STIFN ,NODES%STIFR ,DT2T     ,
     1                          IGRNOD   ,DAMPR )
              ENDIF
C --------------------------
              
              IF (I_EXCH_FLG_RAZ==0) THEN
                ! Flag for reset of stifn and stifr - must be set only at first cycle - ncycle=0 not enough because of chkpt
                ! I_EXCH_FLG_RAZ ensures that it is set only once

                FLG_KJ2_RAZ = FLG_KJ2
                IF (NSPMD > 1) call spmd_allreduce(FLG_KJ2,FLG_KJ2_RAZ,1,SPMD_SUM)
                I_EXCH_FLG_RAZ = 1
              ENDIF
C
              IF (FLG_KJ2==1) THEN
C
                IF (NCYCLE==0) THEN
                  ALLOCATE(STK_SN(NUMNOD),STK_SR(NUMNOD))
                  STK_SN(1:NUMNOD)=NODES%STIFN(1:NUMNOD)
                  STK_SR(1:NUMNOD)=NODES%STIFR(1:NUMNOD)
                ENDIF
C
                CALL JOINT_ELEM_TIMESTEP(NODES%MS,NODES%IN,NODES%STIFN,NODES%STIFR,IXR,IPART,
     1                           IPART(K6),IGEO,GEO,NPBY,IPARG,ELBUF_TAB,
     2                           DT2T,NELTST,ITYPTST,NRBODY,NODES%ITAB)
              ENDIF

C----------------------------------------------------------
C  FIND TARGET_DT FOR DEFINED % OF ADDED MASS
C----------------------------------------------------------
              IF ((NCYCLE==0).AND.(IDT_PERCENT_ADDMASS > 0).AND.(IDTMIN(11)==3.OR.IDTMIN(11)==8)) THEN
                CALL FIND_DT_FOR_TARGETED_ADDED_MASS(NODES%MS,NODES%STIFN,DTFAC1(11),IDTGR(11),TARGET_DT,
     .                                               PERCENT_ADDMASS,PERCENT_ADDMASS_OLD,MASS0_START,NODES%WEIGHT_MD,IGRNOD,
     .                                               ICNDS10)
                DTMIN1(11) = MAX(DTMIN1(11),TARGET_DT)
              ELSEIF ((IDT_PERCENT_ADDMASS == 2).AND.(IDTMIN(11) == 8)) THEN
C--     For /DT/NODA/STOP + % added mass - IDTMIN switch back to 1 after first cycle
                IF (IDT_PERCENT_ADDMASS == 2) THEN
                  IDTMIN(11) = 1
                  DTMIN1(11) = DT_STOP_PERCENT_ADDMASS
                ENDIF
              ENDIF

C--------------------------------------------------
C  UPDATE MINIMUM NODAL DT IF DT IS GIVEN BY FVMBAG
C--------------------------------------------------
              IF(NSPMD > 1 .AND. NVOLU > 0 .AND. NFVBAG0 > 0) THEN
                !
                ! End asynchronous communication
                ! This is an implicit Barrier
                !
                CALL MPI_MIN_REAL_END(DT2R,MIN_TAB,4,MPI_BUF)
                ! This call changes DT2R and MIN_TAB the to minimum value of
                ! DT2R and the corresponding MIN_TAB.

                ! NELTS = MIN_TAB(1)
                ! ITYPTS= MIN_TAB(2)
                ! ISPMD = MIN_TAB(4) ! ID of the proc. that has the mini. value of DT2R

                ! DT2  = DT2R ! Min over the proc. of the value of DT2

                DTMIN1_SAVE = DTMIN1(11)
                IF(MIN_TAB(2) == 52) DTMIN1(11) = MIN(DTMIN1_SAVE,DT2R,1.1*DT2OLD)

              ELSE

                DTMIN1_SAVE = DTMIN1(11)
                IF(ITYPTS == 52) DTMIN1(11) = MIN(DTMIN1_SAVE,DT2,1.1*DT2OLD)

              ENDIF

C

C------------------------------------
C  THERMAL TIME STEP COMPUTATION
C-----------------------------
              IF (GLOB_THERM%IDT_THERM == 1)THEN
                DT2 = GLOB_THERM%DT_THERM
                DT2T = DT2
              ENDIF
C

              IF((ANIM_N(18) /= 0 .OR. H3D_DATA%N_SCAL_STIFR /= 0) .AND. IRODDL /= 0)
     .           STIFR_TMP(1:NUMNOD)=NODES%STIFR(1:NUMNOD)
              IF(ANIM_N(19) /= 0 .OR. H3D_DATA%N_SCAL_STIFN /= 0)
     .           STIFN_TMP(1:NUMNOD)=NODES%STIFN(1:NUMNOD)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,DT2TT,NELTSTT,ITYPTSTT,NODFTSK,NODLTSK)
!$OMP+PRIVATE(DMAST,DINERT)

              DT2TT = DT2T
              NELTSTT = NELTST
              ITYPTSTT= ITYPTST
              DMAST = ZERO
              DINERT = ZERO
              ITSK = OMP_GET_THREAD_NUM()
              NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
              NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C----------------------------------------------------------
C       NODAL TIME STEP
C----------------------------------------------------------
              IF(ISTATCND/=0)THEN
C       additional storage due to reset of stifn stifr
                STCND (NODFTSK:NODLTSK)=NODES%STIFN(NODFTSK:NODLTSK)
                STRCND(NODFTSK:NODLTSK)=NODES%STIFR(NODFTSK:NODLTSK)
              ENDIF

              IF(IDTMINS==0)THEN
C     IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                CALL DTNODA(
     1                      NODFTSK,NODLTSK ,NELTSTT,ITYPTSTT   ,NODES%ITAB   ,
     2                      NODES%MS   ,NODES%IN    ,NODES%STIFN ,NODES%STIFR     ,DT2TT   ,
     3                      DMAST,DINERT,OUTPUT%DATA%SCAL_DT,OUTPUT%DATA%SCAL_DMAS,IMSCH  ,
     4                      NODES%WEIGHT,NODES%A    ,NODES%AR    ,IGRNOD    ,glob_therm%nodadt_therm,
     5                      OUTPUT%DATA%SCAL_DINER,RBYM  ,ARBYM ,ARRBYM,NODES%WEIGHT_MD,
     6                      NODES%MCP         ,MCP_OFF,CONDN ,ALE_CONNECTIVITY%NALE  ,H3D_DATA  )
              ELSEIF(IDTMINS/=0)THEN
                CALL DTNODAMS(
     1                      NODFTSK,NODLTSK ,NELTSTT,ITYPTSTT   ,NODES%ITAB   ,
     2                      NODES%MS   ,NODES%IN    ,NODES%STIFN ,NODES%STIFR     ,DT2TT   ,
     3                      DMAST,DINERT,OUTPUT%DATA%SCAL_DT,OUTPUT%DATA%SCAL_DMAS,IMSCH  ,
     4                      NODES%WEIGHT,NODES%A    ,NODES%AR    ,IGRNOD    ,
     5                      OUTPUT%DATA%SCAL_DINER,RBYM  ,ARBYM ,ARRBYM ,ISMSCH ,
     6                      NATIV_SMS   ,DIAG_SMS ,NPBY,TAGMSR_RBY_SMS,
     7                      H3D_DATA    )
              END IF

#include "lockon.inc"
              DMAS  = DMAS  + DMAST
              DINER = DINER + DINERT
              IF (GLOB_THERM%IDT_THERM == 1)THEN
                IF(DT2TT<=DT2T)THEN
C---------Check remaining time for end simulation and correct time step----
C------------Need to stop computation at Tstop for /DT/THERM----------

                  TREST=MAX(TSTOP-TT,ZERO)

                  DTREST = TREST*(ONE+EM10)

                  DT2T = MIN(DT2TT,DTREST)
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                ENDIF
              ELSE
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                END IF
              ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL
             CALL PYTHON_END_OPENMP(PYTHON)

C ----RAZ of NODES%STIFN AND NODES%STIFR for kjoints with element time step----------------------
              IF ((FLG_KJ2_RAZ==1).AND.(I7KGLO==0).AND.(IDTMINS==0).AND.(NODADT==0)) THEN
                NODES%STIFN(1:NUMNOD) = EM20
                IF (IRODDL > 0) NODES%STIFR(1:NUMNOD) = EM20
              ENDIF
C
              DTMIN1(11) = DTMIN1_SAVE

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C
              IF(DT2T<DT2)THEN
                NELTS  = NELTST
                ITYPTS = ITYPTST
                DT2    = DT2T
              ENDIF
C----------------------------
C    IMPLICIT SYNCHRONISATION On DT2 FBIG
C----------------------------


C-----------------------------
              IF(ALE%SUB%IALESUB==2 .AND.ALE%SUB%IFSUB==2)THEN
                IF(NSPMD>1) THEN
                  IWIOUT = 0
                  IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
                  CALL SPMD_GLOB_MIN5(DT2  ,ITYPTS,NELTS ,NODES%ICODT ,IMSCH,
     .                                TSTOP,IWIOUT,MSTOP, ISMSCH,
     .                                INT24USE,NBINTC,INTLIST,IPARI,INTERFACES%INTBUF_TAB)

                  IF(NSPMD>1.AND.IWIOUT>0) THEN
                      CALL SPMD_WIOUT(IOUT,IWIOUT)
                      IWIOUT = 0
                  ENDIF

                  IF(IEXICODT>0) THEN
                    LENGTH = 1
                    LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                    CALL SPMD_EXCH_ICODT(NODES%ICODT,NODES%BOUNDARY_ADD,NODES%BOUNDARY,LENGTH,LENR)
                    IEXICODT = 0
                  ENDIF
                ENDIF


C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
               CALL PYTHON_BEGIN_OPENMP(PYTHON) 
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)
C Init var parallel SMP
                CALL SMP_INIT(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK,GREFTSK,GRELTSK)
                DT2TT = DT2T
                NELTSTT = NELTST
                ITYPTSTT= ITYPTST
                CALL ALESUB2(
     1             ALE_CONNECTIVITY%NALE,NODES%V ,DSAVE     ,NODES%ICODT  ,NODES%ISKEW,
     2             SKEWS%SKEW           ,ASAVE   ,NODES%A   ,NODES%D      ,NELTSTT,
     3             ITYPTSTT             ,ITSK    ,NODFTSK   ,NODLTSK      ,DT2SAVE,
     4             DT2TT                ,NELTSA  ,ITYPTSA   ,NELTS        ,
     5             NODES%WEIGHT         ,ELEMENT%PON%FSKY   ,ELEMENT%PON%FSKY   )
#include "lockon.inc"
                IF(DT2TT<DT2T)THEN
                  DT2T = DT2TT
                  NELTST = NELTSTT
                  ITYPTST= ITYPTSTT
                END IF
#include "lockoff.inc"
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
                ALE%SUB%IFSUB=0
                ALE%SUB%IFSUBM=0
C-----------------------------
C  SPMD update restart writing
C-----------------------------
                GOTO 21
              ENDIF
              IF (IMON>0) CALL STARTIME(TIMERS,6)
C----------------------------------
C       SCALE TIME STEP FOR OLD AIRBAGS
C----------------------------------
              IF(NRBAG > 0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,50)
                CALL RBAGDT(GEO,IGEO)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,50)
              ENDIF
C----------------------------------
C       SCALE TIME STEP FOR
C       - MONITORED VOLUMES
C       - a M + b K DAMPING
C----------------------------------
              IF(NVOLU > 0)THEN
                IF (IMONM > 0) CALL STARTIME(TIMERS,50)
                IF(NSPMD>1)CALL SPMD_GLOB_MINV(T_MONVOL,DT2,ITYPTS,NELTS,VOLMON, FR_MV)
                IF(PYTHON%NB_FUNCTS > 0) CALL PYTHON_MONVOL(T_MONVOL)
C========================================================================================
C                                   DOMAIN 0
C========================================================================================
                IF(ISPMD == 0) CALL MVOLUDT(MONVOL,VOLMON)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,50)
              ENDIF
C----------------------------------
              IF (IMONM > 0) CALL STARTIME(TIMERS,52)
              IF (NODADT==0) THEN
                IF (ISTAT==3) THEN
                  DAMPA3 = TWO*BETATE/(ONE + BETATE * DT12)
                  IF (DT2>=EP06) DAMPA3=ZERO
                ELSE
                  DAMPA3 = ZERO
                END IF

                IF(IDAMP>0)THEN
                  IF(DAMPB>=ZERO)THEN
                    BB  = (MIN(DAMPB,DT1,DT2) + HALF*(DAMPA+DAMPA3)*DT2*DT2)
C--       IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
                    IF (DT2>=EP06) BB = ZERO
                    DT2 = SQRT(BB*BB + DT2*DT2) - BB
                  ELSE
                    BB  = ONE - DAMPB - DAMPB
                    DT2 = DT2/SQRT(BB)
                  ENDIF
                ELSEIF(NDAMP>0) THEN
                  IF(NRDAMP==4)THEN
                    BB = ZERO
                    DO I=1,NDAMP
                      DAMPA = DAMPR(3,I)
                      DAMPB = DAMPR(4,I)
                      D_TSTART = DAMPR(17,I)
                      D_TSTOP  = DAMPR(18,I)
C--           IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
                      IF ((TT>=D_TSTART).AND.(TT<=D_TSTOP).AND.(DT2 < EP06))
     .                  BB=MAX(BB,(MIN(DAMPB,DT1,DT2)+HALF*(DAMPA+DAMPA3)*DT2*DT2))
                    ENDDO
                  ELSE
                    BB = ZERO
                    IF (FLG_DTNODAMP==1) GOTO 600
                    DO I=1,NDAMP
                      DAMPA = MAX(DAMPR(3,I),DAMPR(5,I),DAMPR(7,I))
                      DAMPA = MAX(DAMPA,DAMPR(9,I),DAMPR(11,I),DAMPR(13,I))
                      DAMPB = MAX(DAMPR(4,I),DAMPR(6,I),DAMPR(8,I))
                      DAMPB = MAX(DAMPB,DAMPR(10,I),DAMPR(12,I),DAMPR(14,I))
C--                   /DAMP/VREL - recompute damping parameters at current time
                      FL_VREL = NINT(DAMPR(21,I))
                      IF (FL_VREL==2) THEN
                        call damping_vref_compute_dampa(i,ndamp,nrdamp,dampr,dt1,tt,damp_a)
                        DAMPA = MAX(DAMP_A(1),DAMP_A(2),DAMP_A(3))
                        DAMPB = ZERO
                      ENDIF
                      FACTB = DAMPR(16,I)
                      D_TSTART = DAMPR(17,I)
                      D_TSTOP  = DAMPR(18,I)
C--           IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
                      IF ((TT>=D_TSTART).AND.(TT<=D_TSTOP).AND.(DT2 < EP06)) THEN
                        DAMPT = MIN(DT1,DT2)*FACTB
                        BB=MAX(BB,(MIN(DAMPB,DAMPT)+HALF*(DAMPA+DAMPA3)*DT2*DT2))
                      ENDIF
                    ENDDO
600                 CONTINUE
                  END IF
                  DT2 = SQRT(BB*BB + DT2*DT2) - BB
                ELSEIF(ISTAT==3) THEN
                  BB  = HALF*DAMPA3*DT2*DT2
                  DT2 = SQRT(BB*BB + DT2*DT2) - BB
                ENDIF
              END IF !(NODADT==0) THEN

              IF (IMONM > 0) CALL STOPTIME(TIMERS,52)


              ! ----------------------
              ! user library : check out
              IF(DLIB_STRUCT(ID_ENGINE_USER_CHECK)%DLIB_BOOL) THEN
                TSTOP_USER = TSTOP
                TT_USER = TT
                MSTOP_USER = 0
                NCYCLE_USER = NCYCLE
                CALL ENGINE_USER_CHECK(ISPMD_USER,TSTOP_USER,NCYCLE_USER,TT_USER,MSTOP_USER)
                IF(MSTOP_USER > 0) THEN
                  MSTOP=1
                  MREST=1
                ENDIF
              ENDIF
              ! ----------------------
#ifdef DNC
              IF(MDS_AVAIL==1) THEN
                TSTOP_USER = TSTOP
                TT_USER = TT
                MSTOP_USER = 0
                NCYCLE_USER = NCYCLE
                CALL MDS_ENGINE_USER_CHECK(ISPMD_USER,TSTOP_USER,NCYCLE_USER,TT_USER,MSTOP_USER)
                IF(MSTOP_USER > 0) THEN
                  MSTOP=1
                  MREST=1
                ENDIF
              ENDIF
#endif

              ! ----------------------
              ! *.out file check tmp files for other domains

              IF (IMONM > 0) CALL STARTIME(TIMERS,53)
              IF(NSPMD>1) THEN
                IWIOUT = 0
                IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
              ENDIF

              IF(NSPMD>1)THEN
                CALL SPMD_GLOB_MIN5(DT2  ,ITYPTS,NELTS ,NODES%ICODT ,IMSCH,
     .                              TSTOP,IWIOUT,MSTOP ,ISMSCH,
     .                              INT24USE,NBINTC,INTLIST,IPARI,INTERFACES%INTBUF_TAB)

              ! -------------------------------------------------------------------
              !  Writing content of tmp file in output file for other MPI domains
              ! -------------------------------------------------------------------
                IF(IWIOUT>0) THEN
                   CALL SPMD_WIOUT(IOUT,IWIOUT)
                   IWIOUT = 0
                ENDIF

C If FVMBAGS switch to UP using NPOLH criterion, then
C an SPMD communication must be made to warn all processors (only
C processes in charge of the FVMBAGS know NPOLH)
                IF(NFVBAG0 >0 .AND. CHECK_NPOLH) CALL SPMD_FVB_SWITCH(MONVOL)

                IF(IEXICODT>0) THEN
                  LENGTH = 1
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  CALL SPMD_EXCH_ICODT(NODES%ICODT,NODES%BOUNDARY_ADD,NODES%BOUNDARY,LENGTH,LENR)
                  IEXICODT = 0
                ENDIF
              ENDIF

              IF (GLOB_THERM%IDT_THERM == 0) DT2= MIN(DT2,1.1*DT2OLD,DTMX)
              IF (IMPL_S==1)  CALL IMP_DT2(DT2)

              IF (IMONM > 0) CALL STOPTIME(TIMERS,53)

C --------------------------
              IF ( NCYCLE == 0 .AND. FLG_KJ2_RAZ > 0  ) THEN     ! All processes should go in this block

                if (ispmd==0)then
                   write(iout,'(A)') ' '
                   write(iout,'(A/)') ' AUTOMATIC STIFFNESS COMPUTATION FOR JOINTS'
                   write(iout,'(A)') '  JOINT ID      TYPE                KNN                KNR'
                endif
                
                IF (FLG_KJ2 == 1) then                        
                    CALL JOINT_BLOCK_STIFFNESS(NODES%ITAB,NODES%MS,NODES%IN,STK_SN,STK_SR,
     1                    NODES%WEIGHT,IXR,IPART,NODES%X,IPART(K6),
     2                    IGEO,GEO,NPBY,IPARG,ELBUF_TAB,DMAST,DINERT)
                    DEALLOCATE(STK_SN,STK_SR)
                ENDIF

                ! After joint_block_stiffness : Flush .tmp files in *.out for all domains except ispmd=0

                IWIOUT = 0
                IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
                call spmd_allreduce(IWIOUT,IWIOUT_RESULT,1,SPMD_SUM)
                IWIOUT = IWIOUT_RESULT
                IF(IWIOUT>0) CALL SPMD_WIOUT(IOUT,IWIOUT) 
                IWIOUT = 0

                if (ispmd==0)then
                   write(iout,'(A)') ' '
                endif

              ENDIF
C--------------------------------------------------------------C
C      RADIOSS 2 RADIOSS COUPLING
C--------------------------------------------------------------C
              IF (IRAD2R /= 0 .AND. R2R_ACTIV == 1) THEN
                CALL R2R_SENDKINE(IEXLNK,IGRNOD,NODES%MS,NODES%IN)
                IF (NSPMD>1) CALL SPMD_BARRIER()
C--------------------------------------------------------------C
             CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
!$OMP MASTER
C--------------------------------------------------------------C
C--------------------------------------------------------------C
                IF (IMONM > 0) CALL STARTIME(TIMERS,54)
                IF (NCYCLE == ZERO) TT_DP = TT
C--------------------------------------------------------------C
                IF(ISPMD==0)THEN
                  CALL R2R_SEM_C()
                  IF (IRESP==1) CALL SEND_FBUFDP_C(TT_DP,1)
                  IF (IRESP/=1) CALL SEND_FBUFDP_C(TT,1)
                  IF ((R2R_SIU==1).AND.(IDDOM/=0)) THEN
                    CALL GET_IBUF_C(R2R_TH_MAIN,10)
                  ENDIF
                  CALL SEND_FBUF_C(DT2,1)
                  CALL SEND_IBUF_C(R2R_MFILR,1)
                  CALL SEND_IBUF_C(R2R_MSTOP,1)
                  IF (R2R_MFILR==1) THEN
                    CALL SEND_FBUF_C(TMAN_R2R,1)
                    CALL SEND_IBUF_C(R2R_CTR,3)
                  ENDIF
                  IF (IRESP==1) CALL GET_FBUFDP_C(TT_DP,1)
                  IF (IRESP/=1) CALL GET_FBUFDP_C(TT,1)
                  CALL GET_IBUF_C(MREST,1)
                  CALL GET_IBUF_C(R2R_MFILR,1)
                  CALL GET_IBUF_C(R2R_MSTOP,1)
                  IF (R2R_MFILR==2) THEN
                    CALL GET_FBUF_C(TMAN_R2R,1)
                    CALL GET_IBUF_C(R2R_CTR,3)
                  ENDIF
                  CALL GET_FBUF_C(DT2,1)
                END IF
C--------------------------------------------------------------C
                IF(NSPMD>1)THEN
                  IF(ISPMD==0) THEN
                    RBUF(1) = TT
                    RBUF(2) = DT2
                  END IF
                  CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
                  CALL SPMD_IBCAST(MREST,MREST,1,1,0,2)
                  CALL SPMD_IBCAST(R2R_TH_MAIN,R2R_TH_MAIN,10,1,0,2)
                  IF(ISPMD/=0) THEN
                    TT = RBUF(1)
                    DT2 = RBUF(2)
                  END IF
                END IF
C--------------------------------------------------------------C
                IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN
                  CALL R2R_SEM_C()
                  CALL GET_IBUF_C(BID,1)
                  CALL R2R_UNLOCK_THREADS_C(NTHREAD)
                ELSEIF (ISPMD==0) THEN
                  CALL R2R_SEM_C()
                  CALL GET_IBUF_C(BID,1)
                  CALL R2R_UNLOCK_THREADS_C(NTHREAD*NSPMD)
                ENDIF
!$OMP END MASTER
                CALL R2R_BLOCK_C()
C--------------------------------------------------------------C
!$OMP END PARALLEL
               CALL PYTHON_END_OPENMP(PYTHON)
C--------------------------------------------------------------C
C--------------------------------------------------------------C

                CALL R2R_GETDATA(IEXLNK  ,IGRNOD  ,NODES%X       ,NODES%V       ,
     .                           NODES%VR      ,NODES%A       ,NODES%AR      ,NODES%MS      ,NODES%IN      ,
     .                           NODES%XDP     ,NODES%D       ,R2R_ON  ,DD_R2R  ,NODES%WEIGHT  ,
     .                           NODES%BOUNDARY_ADD,NODES%BOUNDARY ,NODES%STIFN   ,NODES%STIFR   ,DD_R2R_ELEM  ,
     .                           SDD_R2R_ELEM,NLOC_DMG, OUTPUT%TH%WFEXT, OUTPUT%TH%WFEXT_MD)

                IF (IMONM > 0) CALL STOPTIME(TIMERS,54)

              ENDIF

#ifdef DNC
C----------------------------------------------
C Radioss Madymo coupling - Exchange Timesteps
C----------------------------------------------
              IF(IMADCPL==1)THEN
                MADENDREQUEST = 0
                IF (IMONM > 0) CALL STARTIME(TIMERS,55)
C Exchange Time Step
                CALL TSTP_EXCH_MADCPL(MADENDREQUEST,MADCLNOD,MADCLFRECV,NODES%V,NODES%A,NODES%MS ,MADYMO_DEL_GLOBAL )
                IF (MADENDREQUEST == -1)THEN
                  MSTOP = 2
                  CALL TRACE_OUT(3)
                  RETURN
                ENDIF
                IF (IMONM > 0) CALL STOPTIME(TIMERS,55)
              ENDIF
#endif
C-----------------------------------------------------
              IF (ViperCoupling) THEN
C                Compare timesteps from Viper and Radioss & select the smallest
                 CALL RadiossViper_ReceiveSendDT(VIPER%io_dt,TT,VIPER%DT_MIN,DT2,MSTOP,MREST)
              ENDIF
C-----------------------------------------------------
! reducing dt2 due to /INIVEL
              IF(LOADS%NINIVELT_G>0) CALL INIVEL_DT2(LOADS%NINIVELT,LOADS%INIVELT,SENSORS,TT , DT2 ,NSPMD)
C-----------------------------
              IF (IMONM > 0) CALL STARTIME(TIMERS,53)
              DT2OLD=DT2
              IF (INCONV==1) THEN
                DT12=HALF*(DT1+DT2)
                DT3=DT1
              ENDIF
              IF(ALE%SUB%IALESUB==0)DT2S=DT2

              IF (IRAD2R /= 0) THEN
                IF(ISPMD==0)THEN
                  R2RFX1 = R2RFX1*DT2 + R2RFX2*DT12*DT2
                ENDIF
              ENDIF
C       EXTERNAL WORK OF CONCENTRATED LOADS
              IF (NCONLD/=0.AND.IMPL_S/=1) THEN
                OUTPUT%TH%WFEXT  = OUTPUT%TH%WFEXT  + WFEXC*DT2
              ENDIF

              IF (IMONM > 0) CALL STOPTIME(TIMERS,53)
              IF (IMON>0) CALL STOPTIME(TIMERS,6)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

              ITSK = OMP_GET_THREAD_NUM()
              NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
              NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 1 ON 3 TO GET FREAC/MREAC
C      adding FEXT+FINT
C-----------------------------------------------
              IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN
                CALL REACTION_FORCES_1(NODFTSK,NODLTSK,NODES%A,NODES%AR,FREAC)
              END IF

C--- // ----------------------------------------
C     INTERNAL FORCES (ANIM, OUTP, H3D)
C-----------------------------------------------
              CALL FORANI2(OUTPUT,NODES%A,NODFTSK,NODLTSK,H3D_DATA)

!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
C-----------------------------------------------
C     SELECTIVE MASS SCALING
C-----------------------------------------------
              IF(IDTMINS == 1)THEN
C
C       Obsolete
              ELSEIF(IDTMINS == 2.OR.IDTMINS_INT /= 0)THEN

                IF (IMON>0) CALL STARTIME(TIMERS,39)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL MY_ALLOC(CJWORK,18,NJOINT)
                CALL MY_ALLOC(FREA,3,NUMNOD)
                CALL MY_ALLOC(IRWL_WORK,SLPRW)
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL SMS_MASS_SCALE_2(TIMERS,PYTHON,
     1                ITSK     ,NODFTSK   ,NODLTSK  ,NODII_SMS ,INDX2_SMS ,
     2                NODXI_SMS,NODES%MS        ,NODES%MS0      ,NODES%A         ,NODES%ICODT     ,
     3                NODES%ICODR    ,NODES%ISKEW     ,SKEWS%SKEW,JAD_SMS   ,JDI_SMS   ,
     4                LT_SMS   ,X_SMS     ,P_SMS    ,Z_SMS     ,Y_SMS     ,
     5                PREC_SMS ,INDX1_SMS ,DIAG_SMS ,NODES%BOUNDARY_ADD  ,NODES%BOUNDARY   ,
     6                NODES%WEIGHT   ,NPBY      ,LPBY      ,
     7                TAGSLV_RBY_SMS,LAD_SMS ,KAD_SMS ,JSM_SMS ,IBFV      ,
     8                VEL      ,NPC      ,TF       ,NODES%V         ,NODES%X          ,
     9                NODES%D        ,SENSORS%SENSOR_TAB,NSENSOR,IFRAME,XFRAME,
     A                JADI_SMS ,JDII_SMS ,LTI_SMS  ,FR_SMS    ,FR_RMS     ,
     B                ISKYI_SMS,MSKYI_SMS,RES_SMS  ,IGRV      ,AGRV       ,
     C                LGRAV    ,ILINK    ,LLINK    ,FR_RL     ,FRL6       ,
     D                NNLINK   ,LNLINK   ,FR_LL    ,FNL6      ,TAG_LNK_SMS,
     E                NODES%ITAB     ,FSAV     ,LJOINT   ,IADCJ     ,FR_CJ      ,
     F                NODES%AR       ,NODES%VR       ,NODES%IN       ,FRL       ,FNL        ,
     G                RWALL%NPRW     ,RWALL%LPRW     ,RWALL%RWBUF    ,RWALL%RWSAV     ,
     H                OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)),RWALL%FR_WALL ,NRWL_SMS   ,
     I                INTSTAMP ,NODES%KINET    ,ELEMENT%SHELL%IXC      ,IXTG      ,SH4TREE    ,
     J                SH3TREE  ,CPTREAC  ,NODREAC  ,FTHREAC   ,
     K                FRWL6    ,3+IRODDL*3,NODES%TAG_S_RBY,DAMPR   , DAMP      ,
     L                IGRNOD   ,NODES%DR       ,RBY      ,TAGMSR_RBY_SMS,
     M                JSM_SMS  ,IRBE2     ,LRBE2   ,IAD_RBE2  ,FR_RBE2M   ,
     N                NMRBE2   ,R2SIZE    ,RBE3%IRBE3    ,RBE3%LRBE3      ,RBE3%FRBE3    ,
     O                RBE3%mpi%IAD_RBE3 ,RBE3%mpi%FR_RBE3,RBE3%mpi%FR_RBE3MP ,RBE3%RRBE3      ,RBE3%RRBE3_PON,
     P                PREC_SMS3 ,DIAG_SMS3,IAD_RBY  ,FR_RBY6    ,RBY6     ,
     Q                RBE3%irotg_sz    ,BETATE   ,IBCSCYC    ,LBCSCYC,
     R                MSKYI_FI_SMS, LIST_SMS,LIST_RMS,CJWORK,FREA,
     S                IRWL_WORK,SMS_VFI,sz_mw6,MW6,OUTPUT%TH%WFEXT,ams_work)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (ALLOCATED(CJWORK)) DEALLOCATE(CJWORK)
                IF (ALLOCATED(FREA)) DEALLOCATE(FREA)
                IF (ALLOCATED(IRWL_WORK)) DEALLOCATE(IRWL_WORK)

                IF (IMON>0) CALL STOPTIME(TIMERS,39)

              ENDIF

              IDUM1=0
              RDUM1=ZERO
C
              K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
              K2=K1+NUMELS
              K3=K2+NUMELQ
              K4=K3+NUMELC
              K5=K4+NUMELT
              K6=K5+NUMELP
              K7=K6+NUMELR
              K8=K7
              K9=K8+NUMELTG
              K10=K9+NUMELX
              K11=K10+NUMSPH

C-----------------------------------------------------------------
C     IMPLICIT  SYNCHRONIZATION (FORCES), FV(FUNC) AND SKEW(MOVING)
C-----------------------------------------------------------------

              IF (IMPL_S == 1) THEN
                ENCIN = ZERO
                ENROT = ZERO
                ENCIN2 = ZERO
                ENROT2 = ZERO
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)
C-----------------------
                IF (IMPL_S==1 .AND. INCONV==1) THEN
                  CALL THBCS_IMP(NODFT,NODLT ,NODES%A,NODES%AR,
     &                           FTHREAC,NODREAC,CPTREAC,FTHDTM,DT3)
                ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                IF (ILAG+IALE+IEULER/=0) THEN
C-----------------------
C         MULTIMATERIAL
C-----------------------
                  IF (NMULT>0) THEN
                    CALL BMULTN(FILL,DFILL,IMS,NODFTSK,NODLTSK)
                  ENDIF
                  CALL IMP_FANII(OUTPUT,NODES%A     ,NFIA      ,NODFT    ,NODLT    ,
     .                         H3D_DATA )
                  IF (IMPDEB==1.AND.IMCONV==0) THEN
                    IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
                      CALL IMP_FOUT(
     1                OUTPUT     ,NODES%A        ,NODES%AR       ,NFIA      ,NFEA       ,
     2                NODFTSK  ,NODLTSK  ,H3D_DATA ,IMPBUF_TAB)
                    ENDIF
                  ENDIF
C---      // N/3 ---------------------------------------
C         INTERNAL FORCES (ANIM)
C-----------------------------------------------
                  IF (ISECUT/=0) THEN
                    IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)
                    CALL SECTION_IO (
     1              NSTRF,NODES%D,NODES%DR,NODES%V,NODES%VR,FSAV(1,1+NINTER+NRWALL+NRBODY),
     2              SECFCUM,NODES%A             ,NODES%AR    ,SECBUF,NODES%MS      ,NODES%IN     ,
     3              NODES%X      ,OUTPUT%DATA%FOPT,NODES%WEIGHT,XSEC  ,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
     4              RG_CUT ,IAD_CUT       ,FR_CUT,NODES%WEIGHT_MD,IOLDSECT,
     5              SENSORS%STABSEN,SENSORS%SFSAV,SENSORS%TABSENSOR,SENSORS%FSAV, OUTPUT%TH%WFEXT)
                    IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
                  ENDIF
                ENDIF ! ILAG+IALE+IEULER/=0

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)
C-----------------------------------------------
                IF (ILAG+IALE+IEULER/=0)THEN
                  IF(IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STARTIME(TIMERS,40)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL

                  CALL RBYCOR(
     1                 RBY ,NODES%X   ,NODES%V    ,NODES%VR     ,SKEWS%SKEW    ,FSAV     ,
     2                 LPBY,NPBY,NODES%ISKEW,NODES%ITAB   ,NODES%WEIGHT  ,NODES%A        ,
     3                 NODES%AR  ,NODES%MS  ,NODES%IN   ,KINDRBY,IRBKIN_L,NRBYKIN_L ,
     4                 NODES%WEIGHT_MD,MS_2D)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                  IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STOPTIME(TIMERS,40)
                ENDIF  ! ILAG+IALE+IEULER/=0
C========================================================================================
C-----  RBE2 Bilan crrection
C========================================================================================
                IF (NRBE2>0)THEN
                  IF(IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STARTIME(TIMERS,40)
C

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
                  CALL RBE2COR(IRBE2 ,LRBE2 ,NODES%X    ,NODES%V     ,NODES%VR    ,
     2                         SKEWS%SKEW  ,NODES%ISKEW ,NODES%ITAB ,NODES%WEIGHT,NODES%A     ,
     3                         NODES%AR    ,NODES%MS0   ,NODES%IN   ,NODES%WEIGHT_MD)
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

                  IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STOPTIME(TIMERS,40)
                ENDIF
C--------------------
C       SENSOR //  IN IMPLICIT
C-----------------------------------------------
C       Exchange SENSORS%FSAV for sensors 6-13
C-----------------------------------------------
                IF (NSENSOR > 0) THEN
                  IF (NSPMD > 1 .AND. SENSORS%STABSEN > 0) THEN
                    DIM6=12
                    DIM_EXCH = SENSORS%SFSAV
                    CALL SPMD_EXSUM_FB6(DIM6,DIM_EXCH,SENSORS%FSAV)
                  ENDIF
                ENDIF
c
                IF (NSENSOR > 0 .AND. INCONV == 1) THEN
                  CALL SENSOR_ENER_SAV(NSENSOR,SENSORS%SENSOR_TAB ,PARTSAV ,PARTSAV2)
                  ! -------------------------------
                  ! pre-computation and mpi communication for type 16 sensor
                  IF (SENSORS%COMM_SENS16%BOOL) THEN
                    CALL SENSOR_DIST_SURF0(NSENSOR,SENSORS%SENSOR_TAB,NODES%X,
     *                                     IGRSURF,SENSORS%COMM_SENS16)
                  ENDIF
                  ! -------------------------------
                  ! pre-computation and mpi communication for type 17 sensor
                  IF(SENSORS%COMM_SENS17%BOOL) THEN
                    CALL SENSOR_TEMP0(NSENSOR,SENSORS%SENSOR_TAB,IGRNOD,NODES%TEMP,NODES%WEIGHT,SENSORS%COMM_SENS17,
     *                                SENSORS%SENSOR_STRUCT)
                  ENDIF
                  ! -------------------------------
                  IF (NSPMD > 1) THEN
                    CALL SENSOR_SPMD(SENSORS%SENSOR_TAB,IPARI   ,RWALL%NPRW     ,ISENSP  ,NSENSP  ,
     .                           XSENS     ,NODES%X       ,ACCELM   ,IACCP   ,NACCP   ,
     .                           GAUGE     ,IGAUP   ,NGAUP    ,PARTSAV2,NSENSOR,
     .                           SENSORS%COMM_SENS14,SENSORS%SENSOR_STRUCT)
                  ENDIF
c
                  ! check activation condition of base sensors
                  CALL SENSOR_BASE(SENSORS  ,NSENSOR   ,TT        ,DT2       ,
     .                 XSENS     ,IPARI     ,PARTSAV2  ,GAUGE     ,FSAV      ,
     .                 NODES%X         ,NODES%V         ,NODES%A         ,ACCELM    ,RWALL%NPRW      ,
     .                 SUBSETS   ,IGRSURF   ,IGRNOD    , PYTHON)
c
                  ! check activation condition of logical sensors hierarchy
                  CALL SENSOR_LOGICAL(SENSORS)

                ENDIF
                GOTO 111
              ENDIF
C-----------------------------
C       Non pure thermal case
C-----------------------------
              IF (ILAG + IALE + IEULER /= 0) THEN

                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- //   ----------------
C         MULTIMATERIAL
C-------  ----------------
                IF(NMULT>0) THEN
                  CALL BMULTN(FILL,DFILL,IMS,NODFTSK,NODLTSK)
                ENDIF

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
C-----------------------------------------------
C         SPH SMOOTHING OF VELOCITIES
C-----------------------------------------------
                IF ((NUMSPH/=0).OR.(SOL2SPH_FLAG==1)) THEN
C------------------------
C           Conservative smoothing of velocities
C------------------------
                  IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,89)

                  ALLOCATE(WASPSYM(3*NSPHSYM+1))
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                 CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,IPMTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  IPMTSK = 1 + ITSK*NPSAV*NPART

                  CALL SPLISSV(
     1            NODES%X         ,NODES%V        ,NODES%MS       ,NODES%A         ,SPBUF     ,
     2            WA        ,NODES%ITAB     ,KXSP     ,IXSP      ,NOD2SP    ,
     3            NODES%D         ,ISPSYM   ,XSPSYM%BUF   ,VSPSYM%BUF    ,BUFMAT    ,
     4            BUFGEO    ,NPC      ,TF       ,PM        ,GEO       ,
     5            ISPCOND   ,XFRAME   ,WASPSYM,IPART(K10),PARTSAV(IPMTSK),
     6            WASPH(KSPH21) ,WSMCOMP%BUF ,WASPH(KSPACTIV) ,IPART,ITSK,
     7            SPH2SOL   ,SOL2SPH  ,IRST    ,IXS        ,IPARG     ,
     8            NGROUNC   ,IGROUNC  ,ELBUF_TAB,NODES%BOUNDARY_ADD  ,NODES%BOUNDARY   ,
     9            IGEO      ,SOL2SPH_TYP,SPH_WORK)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
                  DEALLOCATE(WASPSYM)
C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

                  IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,89)
C------------------------
C           Second Part of artificial forces work.
C--- //0 ----------------
                  IF (IMON>0) CALL STARTIME(TIMERS,6)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                  CALL SPWFVIS(SPBUF,IPART(K10),PARTSAV,IPARG,ELBUF_TAB, KXSP ,WASPH(KSPACTIV))
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
                  IF (IMON>0) CALL STOPTIME(TIMERS,6)
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
                  CALL SMP_INIT(
     1              ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2              IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)

                  CALL ASSPART(
     2               PARTFTSK,PARTLTSK,PARTSAV,GREFTSK,GRELTSK,GRESAV)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

                ENDIF  ! (NUMSPH/=0).OR.(SOL2SPH_FLAG==1)
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C       BALANCING FORCES COMPUTED AT TT=0 NODES%IN GLOBAL REFERENCE SYSTEM
C----------------------------------------------------------------------------------------
                IF (TT==ZERO.AND.IABS(ISIGI)==5) THEN
C--- //0 ----------------
                  IF (IMON>0) CALL STARTIME(TIMERS,6)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,49)
                  CALL FEQUILIBRE(NODES%A,FZERO,ELEMENT%SHELL%IXC,IXTG)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,49)
                  IF (IMON>0) CALL STOPTIME(TIMERS,6)
                ENDIF
C========================================================================================
C MPI communication for NLOC_DMG : parith/off
C========================================================================================
                IF(IPARIT == 0.AND.NSPMD > 1.AND. NLOC_DMG%IMOD > 0)THEN
                  CALL SPMD_EXCH_SUB_POFF(NLOC_DMG)
                ENDIF

                IF(coupling%active) THEN
                  ! FORCES WAS INITIALIZED TO THE ACCELERTION, BEFORE ASSEMBLY
                  ! BEFORE THE CALL TO ACCELE, A CONTAINS THE FORCES
                  ! AFTER THE CALL TO ACCELE, A CONTAINS THE ACCELERATION
                  !NODES%FORCES(1:3,1:NUMNOD) = nodes%A(1:3,1:NUMNOD) - NODES%FORCES(1:3,1:NUMNOD) 
                  CALL COUPLING_SYNC(coupling,DT2,nodes,COUPLING_FORCES)
                ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C--- // ---------------------------------------
C       ACCELERATIONS (TRANSLATIONS)
C-----------------------------------------------

                !-----------------------------
                ! FINITE VOLUME METHOD FOR ALE
                !-----------------------------
                IF(ALEFVM_Param%IEnabled>0)THEN
                  CALL ALEFVM_ACCELE(NODES%A, NODES%AR, NODFTSK, NODLTSK, ALE_CONNECTIVITY%NALE)
                ENDIF

                CALL ACCELE(NODES%A       ,NODES%AR      ,NODES%V    ,NODES%MS     ,NODES%IN     ,
     2                      ALE%GLOBAL%SNALE ,ALE_CONNECTIVITY%NALE   ,MS_2D  ,
     3                      SIZE_NPBY,NPBY )
C
                IF(IPLYXFEM > 0 )
     .            CALL PLY_ACCELE(INOD_PXFEM,MS_PLY,ZI_PLY,NODES%MS,
     .                      NODFTSK,NODLTSK,NPLYMAX,NPLYXFE,NUMNOD,MSZ2 )

C
                IF(IALELAG > 0) THEN
                  CALL FLOW_ACCELE(ALE_CONNECTIVITY%NALE, MSF    ,AFLOW        ,VFLOW    ,
     2                             NODFTSK,NODLTSK)
                ENDIF
c
                IF (NLOC_DMG%IMOD > 0) THEN
                  NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
                  NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
                  CALL NLOCAL_ACC(NLOC_DMG, NODFT_NL ,NODLT_NL)
                ENDIF
c
                IF (ICRACK3D > 0) THEN
C        /---------------/
                  CALL MY_BARRIER
C        /---------------/
                  CALL CRK_ACCELE(ADSKY_CRK,INOD_CRK,NODLEVXF ,NODFTSK ,NODLTSK ,
     .                            NODENR   ,CRKSKY  ,NODES%MS       ,NODES%IN      ,NODES%ITAB    )
C        /---------------/
                  CALL MY_BARRIER
C        /---------------/
                  CALL CRK_ZERO_FSKY(CRKSKY,ADSKY_CRK,INOD_CRK,NODFTSK,NODLTSK,
     .                               NODLEVXF)
C        /---------------/
                  CALL MY_BARRIER
C        /---------------/
                ENDIF
C
                IF(NPINCH > 0) THEN
                  CALL ACCELEPINCH(
     1                             PINCH_DATA%APINCH,      NODES%MS, PINCH_DATA%MSPINCH,
     2                          PINCH_DATA%STIFPINCH, NODFTSK,            NODLTSK,
     3                                          DT2T,   DTFAC)
                ENDIF
C--- //  ---------------------------------------
C       TEMPERATURES COMPUTATION
C-----------------------------------------------
                IF (GLOB_THERM%ITHERM_FE > 0 )
     .             CALL TEMPUR(NODES%TEMP ,NODES%MCP,FTHE,NODFTSK,NODLTSK,NODES%WEIGHT,MCP_OFF,GLOB_THERM%HEAT_STORED)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

C------------------------------------------
C       DT_DC for thick-shell
C-----------------------------------------------
                IF(NTSHEGG>0.AND.NSPMD > 1) CALL SPMD_EXCH_VMAX(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,ALPHA_DC )
                IF(NTSHEG > 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  GREFTSK   = 1+ITSK*NTSHEG/ NTHREAD
                  GRELTSK   = (ITSK+1)*NTSHEG/NTHREAD
                  CALL ACCDTDC(GREFTSK,GRELTSK,IENUNL ,ALPHA_DC,NODES%A       ,NODES%MS   ,NODES%ITAB )
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                ENDIF

                IF(NTSHEGG>0.AND.NSPMD > 1)
     .              CALL SPMD_EXCH_FA(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,NODES%A   )
C---------------------------------------------------------------------
C DEBUG TEMPERATURES OUTPUT
C---------------------------------------------------------------------
                IF (DEBUG(MACRO_DEBUG_TEMP)==1) THEN
                  IF (NCYCLE>=TDEBSTART .AND.MOD(NCYCLE-TDEBSTART,TRSTFREQ)==0) THEN
                    IF(NSPMD > 1) THEN
                       SIZ = MERGE(NUMNODG, 0, ISPMD==0)
                      CALL SPMD_COLLECTT(NODES%TEMP,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB,SIZ)
                    ELSE
                      CALL COLLECTT(NODES%TEMP,NODES%ITAB,NODES%WEIGHT,NODES%NODGLOB)
                    END IF
                  END IF
                END IF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)
C-----------------------------------------------
C  INLETS,OUTLETS.
C-----------------------------------------------
                IF(NSPHIO/=0)THEN
C globalize NSPHACT in NSPHACTG
                  NSPHACTG = NSPHACT

                  IF(NSPMD>1)THEN
                    CALL SPMD_GLOB_ISUM9(NSPHACTG,1)
                    CALL SPMD_IBCAST(NSPHACTG,NSPHACTG,1,1,0,2)
                  END IF
                  IF(NSPHACTG/=0)THEN
C--- //0 ----------------
                    IF (IMON>0) CALL STARTIME(TIMERS,6)
                    IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                    IF (IMONM > 0) CALL STARTIME(TIMERS,89)

                    CALL SPONFV (NODES%X   ,NODES%V     ,NODES%A       ,NODES%D       ,NODES%MS      ,
     2                     SPBUF   ,NODES%ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3                     NPC     ,TF      ,ISPHIO  ,VSPHIO  ,IPART   ,
     4                    IPART(K10),WASPH(KSPACTIV) ,WA,WASPH(KSPH22) ,SPH_WORK, OUTPUT%TH%WFEXT)

                    IF (IMONM > 0) CALL STOPTIME(TIMERS,89)
                    IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
                    IF (IMON>0) CALL STOPTIME(TIMERS,6)
                  ENDIF
                END IF
                IF(NSPHIO/=0.AND.NSPHACTG/=0)THEN
C--- //0 ----------------
                  IF (IMON>0) CALL STARTIME(TIMERS,6)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,89)

                  CALL SPONFV (NODES%X   ,NODES%V     ,NODES%A       ,NODES%D       ,NODES%MS      ,
     2                   SPBUF   ,NODES%ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3                   NPC     ,TF      ,ISPHIO  ,VSPHIO  ,IPART   ,
     4                   IPART(K10),WASPH(KSPACTIV),WA,WASPH(KSPH22) ,SPH_WORK, OUTPUT%TH%WFEXT)

                  IF (IMONM > 0) CALL STOPTIME(TIMERS,89)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
                  IF (IMON>0) CALL STOPTIME(TIMERS,6)
                ENDIF

C-----------------------------------------------
C Multidomains : resynchronization of accelerations in SPH
C----------------------------------------------
                IF (NSPMD>1) THEN
                  IF ((SDD_R2R_ELEM>0).AND.(NUMSPH_GLO_R2R>0)) THEN
                    LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                    CALL SPMD_EXCH_R2R_SPH(NODES%A,NODES%BOUNDARY_ADD,NODES%BOUNDARY,DD_R2R,DD_R2R_ELEM,LENR)
                  ENDIF
                ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

                IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

                   CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 2 ON 3 TO GET FREAC/MREAC
C     additional forces
C      when called with iflag=1 it will add (Fgrav+Fbcs_cyclic+Fcentrif)
C-----------------------------------------------
                  IFLAG = -1
                  CALL REACTION_FORCES_2(NODFTSK,NODLTSK,NODES%A,NODES%AR,NODES%MS,NODES%IN,FREAC,IFLAG)

!$OMP END PARALLEL
                    CALL PYTHON_END_OPENMP(PYTHON)  
                END IF

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
                CALL PYTHON_UPDATE_NODAL_ENTITIES(NUMNOD,NODES,A=NODES%A,V=NODES%V,AR=NODES%AR,VR=NODES%VR)

C------------------------------
C         GRAVITE / ACCEL. BASE
C------------------------------
                IF(NGRAV/=0.AND.IDTMINS==0.AND.IDTMINS_INT==0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                  IF (IMONM > 0) CALL STARTIME(TIMERS,46)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  IF(MULTI_FVM%IS_USED)THEN
                    CALL GRAVIT_FVM_FEM(
     1                          IGRV    ,AGRV   ,NPC   ,TF       ,NODES%A     ,
     2                          NODES%V       ,NODES%X      ,SKEWS%SKEW  ,NODES%MS       ,SENSORS%SENSOR_TAB,
     3                          NODES%WEIGHT ,LGRAV ,ITSK     ,ALE_CONNECTIVITY%NALE,NSENSOR, PYTHON, OUTPUT%TH%WFEXT)
                  ELSE
                    CALL GRAVIT(
     1                          IGRV    ,AGRV   ,NPC   ,TF       ,NODES%A     ,
     2                          NODES%V       ,NODES%X      ,SKEWS%SKEW  ,NODES%MS       ,SENSORS%SENSOR_TAB,
     3                          NODES%WEIGHT ,LGRAV   ,ITSK  ,NSENSOR, PYTHON, OUTPUT%TH%WFEXT)
                  ENDIF
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------
C         GRAVITY FLEXIBLE BODIES
C----------------------
                  IF (NFXBODY>0) THEN
                    CALL FXGRVCOR(FXBIPM, FXBGRVI, NODES%A,  IGRV, AGRV,
     .                            NPC,    TF,      NODES%MS, NODES%V   , SKEWS%SKEW,
     .                            FXBGRW, NODES%BOUNDARY_ADD, NODES%BOUNDARY, OUTPUT%TH%WFEXT, PYTHON)
                  END IF
                  IF (IMON>0) CALL STOPTIME(TIMERS,46)
                  IF (IMONM > 0) CALL STOPTIME(TIMERS,TIMER_KIN)
                ENDIF
C----------------------
C         /BCS/CYCLIC
C----------------------
                IF(NBCSCYC > 0)THEN
                  CALL BCSCYC(IBCSCYC,LBCSCYC,SKEWS%SKEW,NODES%X,NODES%V,NODES%A,NODES%ITAB)
                ENDIF
C----------------------
C         CENTRIFUGAL FORCES
C----------------------
                IF (NLOADC>0) THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  CALL CFIELD_1(PYTHON,ICFIELD  ,CFIELD,NPC   ,TF    ,NODES%A,
     2                            NODES%V     ,NODES%X     ,XFRAME  ,NODES%MS,SENSORS%SENSOR_TAB,
     3                            NODES%WEIGHT,LCFIELD,ITSK ,IFRAME,NSENSOR, OUTPUT%TH%WFEXT)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

                IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN
                    CALL PYTHON_BEGIN_OPENMP(PYTHON)  
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 2 ON 3 TO GET FREAC/MREAC
C     additional forces
C      FREAC is now : FEXT+FINT + (Fgrav+Fbcs_cyclic+Fcentrif)
C-----------------------------------------------
                  IFLAG =  1
                  CALL REACTION_FORCES_2(NODFTSK,NODLTSK,NODES%A,NODES%AR,NODES%MS,NODES%IN,FREAC,IFLAG)

!$OMP END PARALLEL
                    CALL PYTHON_END_OPENMP(PYTHON)

                END IF

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C-----------------------------------------------
C       EXTERNAL FORCES ADDITIONAL CONTRIBUTIONS  before adding in A other kinematic conditions (BCS, ...)
C         (ANIM, OUTP, H3D)
C-----------------------------------------------
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL FORANI3(OUTPUT,NODES%A,NODES%MS,NODFTSK,NODLTSK,H3D_DATA)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------
C         INTERFACE ALE
C----------------------
                IERR=0
                IF(NINTER/=0.AND.IALE/=0)THEN
                  IF(ISPMD==0)THEN
                    IF (IMON>0) THEN
                      CALL STARTIME(TIMERS,6)
                      CALL STARTIME(TIMERS,TIMER_CONTSORT)
                    ENDIF
                    CALL INTAL1(OUTPUT,IPARI      ,NODES%X         ,NODES%V      ,
     2                          NODES%A    ,NODES%ISKEW     ,SKEWS%SKEW   ,NODES%ICODT,WA,
     3                          NODES%MS   ,NODES%ITAB      ,FSAV         ,INTERFACES%INTBUF_TAB ,
     4                          OUTPUT%DATA%VECT_CONT       ,OUTPUT%DATA%VECT_PCONT ,H3D_DATA   )
                    IF (IMON>0) THEN
                      CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                      CALL STOPTIME(TIMERS,6)
                    ENDIF
                  ENDIF
                  IF(NSPMD > 1)THEN
                    ! transmit updated values on domain ispmd=0 to other domains (values may be used by parallelized ebcs options)
                    call spmd_xv_inter_type1(numnod, NODES%BOUNDARY_SIZE, ispmd, nspmd, NODES%BOUNDARY_ADD, NODES%BOUNDARY,
     1                                       nodes%a, nodes%v, ninter, ipari, npari)
                  ENDIF
                ENDIF
                IF(IERR == 1)CALL ARRET(2)

                IF(NINTER /= 0 .and. IALE+IEULER /= 0 .and. INT18KINE == -1)THEN
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                  ITSK = OMP_GET_THREAD_NUM()
C        /---------------/
                  CALL MY_BARRIER
C        /---------------/
                  CALL I18MAIN_KINE_2(OUTPUT, IPARI,INTERFACES%INTBUF_TAB,NODES%X  ,NODES%V  ,
     2                          NODES%A    ,NODES%ISKEW,SKEWS%SKEW ,NODES%ICODT,WA,
     3                          NODES%MS   ,NODES%ITAB ,FSAV ,ITSK+1,NODES%KINET,
     4                          NODES%STIFN,MTF,CAND_SAV,OUTPUT%DATA%VECT_CONT,INT18ADD,
     5                          NODES%BOUNDARY_ADD,NODES%BOUNDARY,H3D_DATA )
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
                ENDIF
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------
C  Activating valve (in/out or imposed velocity)
C----------------------

                IF(EBCS_TAB%nebcs_loc>0)THEN
                  IF (IMON>0) THEN
                    CALL STARTIME(TIMERS,6)
                    CALL STARTIME(TIMERS,TIMER_CONTSORT)
                  END IF
                  IF(ISPMD == 0)THEN
                    CALL EBCCLAP(NODES%V,NODES%A,FV,EBCS_TAB)
                  ENDIF
                  IF (IMON>0) THEN
                    CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                    CALL STOPTIME(TIMERS,6)
                  END IF
                ENDIF

C----- // ----------------------
C         BOUNDARY CONDITIONS
C-------------------------------
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,42)

                CALL THBCS(NODFT  ,NODLT  ,NODES%ICODT  ,NODES%ICODR,NODES%ISKEW,
     2                     SKEWS%SKEW   ,NODES%A      ,NODES%AR     ,NODES%MS   ,NODES%IN   ,
     3                     FTHREAC,NODREAC,CPTREAC)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL BCS10(NODFTSK,NODLTSK  ,NODES%ICODT  ,NODES%ICODR,NODES%ISKEW,
     2                     SKEWS%SKEW   ,NODES%A        ,NODES%AR     ,NODES%MS   ,NODES%V    ,
     3                     NODES%VR     )
C---

                IF(IPLYBCS > 0 .AND. IPLYXFEM > 0 )
     .            CALL PLY_BCS(NODFTSK, NODLTSK,ICODT_PLY,ISKEW_PLY,SKEWS%SKEW,
     .                         INOD_PXFEM,MS_PLY,IBC_PLY)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMONM > 0) THEN
                  CALL STOPTIME(TIMERS,42)
                  CALL STARTIME(TIMERS,43)
                END IF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
C-------------------
C      [RLINK1][RLINK2]...   (ON FREE TASK)
C      [RIVETS]                     -
C      [JOINT1][JOINT2]...          -

C-------------------
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                ITSK = OMP_GET_THREAD_NUM()

C-----------------------------------
C         RIGID LINKS BETWEEN NODES
C---- // ----------------------------
                IF(NRLINK>0)CALL RLINK10(
     1             NODES%MS   ,NODES%IN    ,NODES%A    ,NODES%AR  ,NODES%V    ,
     2             NODES%VR   ,ILINK ,LLINK,SKEWS%SKEW,FR_RL,
     3             NODES%WEIGHT,FRL6)
                IF(NLINK>0) CALL RLINK11(
     1             NODES%MS   ,NODES%IN    ,NODES%A     ,NODES%AR    ,NODES%V    ,
     2             NODES%VR   ,NNLINK,LNLINK,SKEWS%SKEW  ,FR_LL,
     3             NODES%WEIGHT,FNL6  ,NODES%X     ,XFRAME)
C---------------
C         RIVETS
C---------------
                IF(NRIVET>0) THEN
                  CALL RIVET1(
     +               NODES%MS    ,NODES%IN   ,NODES%A  ,NODES%AR,NODES%X ,
     +               LRIVET,RIVET,GEO,NODES%V ,NODES%VR,
     +               ITSK  )
                ENDIF

C-------------
C       JOINTS
C-- // -----------
                IF(NJOINT>0) THEN
                  CALL CJOINT(NODES%A    ,NODES%AR    ,NODES%V ,NODES%VR,NODES%X    ,
     2                        FSAV ,LJOINT,NODES%MS,NODES%IN,IADCJ,
     3                        FR_CJ,TAG_LNK_SMS(NRLINK+NLINK+1),ITSK)
                ENDIF

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,43)
              ENDIF
C--------------------------- END OF NON-THERMAL ONLY CASE

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
              ITSK = OMP_GET_THREAD_NUM()
              NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
              NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C--- //-----------------------------------------
C         SAVE FORCES FROM INITIAL STATE
C---------------------------------------
              IF(TT==ZERO.AND.(ISIGI==2.OR.ISIGI==4)) THEN
#include      "vectorize.inc"
                DO I=NODFTSK,NODLTSK
                  FZERO(1,I)=-NODES%A(1,I)
                  NODES%A(1,I)=ZERO
                  FZERO(2,I)=-NODES%A(2,I)
                  NODES%A(2,I)=ZERO
                  FZERO(3,I)=-NODES%A(3,I)
                  NODES%A(3,I)=ZERO
                ENDDO
              ENDIF
!$OMP END PARALLEL


              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C-----------------------------------------------
C          DAMPING alpha M + beta K
C-----------------------------------------------

              IF (NS10E>0.AND.(IDAMP/=0.OR.NDAMP>0.OR.ISTAT/=0)) THEN
                CALL S10GETVDM(ICNDS10,NODES%V,VND,VMD)
              END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

                    CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 2 ON 3 TO GET FREAC/MREAC
C     damping forces
C      when called with iflag=1 it will add Fdamp
C-----------------------------------------------
                IFLAG = -1
                CALL REACTION_FORCES_2(NODFTSK,NODLTSK,NODES%A,NODES%AR,NODES%MS,NODES%IN,FREAC,IFLAG)

!$OMP END PARALLEL
                    CALL PYTHON_END_OPENMP(PYTHON)
              END IF

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
C-----------------------------------------------
              IF (IMONM > 0) CALL STARTIME(TIMERS,52)

              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

              ITSK = OMP_GET_THREAD_NUM()
              NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
              NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

              IF(IDAMP/=0)THEN
                IF (NS10E>0) THEN
                  CALL DAMPING(NODFTSK,NODLTSK,VMD,NODES%VR,NODES%A ,NODES%AR ,DAMP,NODES%MS,NODES%IN,
     .                       IGRNOD,3+IRODDL*3,ITSK,NODES%WEIGHT,NODES%TAG_S_RBY,OUTPUT%TH%WFEXT)
                ELSE
                  CALL DAMPING(NODFTSK,NODLTSK,NODES%V ,NODES%VR,NODES%A ,NODES%AR ,DAMP,NODES%MS,NODES%IN,
     .                        IGRNOD,3+IRODDL*3,ITSK,NODES%WEIGHT,NODES%TAG_S_RBY,OUTPUT%TH%WFEXT)
                END IF !(NS10E>0) THEN
              ELSEIF(NDAMP>0)THEN
                IF(NRDAMP==4)THEN

!$OMP SINGLE
                  CALL DAMPING44(
     .                         3+IRODDL*3,NODES%V       ,
     .                         NODES%VR      ,NODES%A       ,NODES%AR      ,NODES%MS        ,NODES%IN      ,
     .                         DAMPR   ,DAMP    ,IGRNOD  ,NODES%WEIGHT    ,NODES%TAG_S_RBY       ,OUTPUT%TH%WFEXT)
!$OMP END SINGLE

                ELSE

!$OMP SINGLE
                  IF (NS10E>0) THEN
                    CALL DAMPING51(
     .                          3+IRODDL*3,VMD     ,
     .                          NODES%VR      ,NODES%A       ,NODES%AR      ,NODES%MS        ,NODES%IN      ,
     .                          DAMPR   ,DAMP    ,IGRNOD  ,NODES%WEIGHT    ,NODES%TAG_S_RBY,
     .                          SKEWS%SKEW    ,ICONTACT,IDAMP_RDOF_TAB     ,NDAMP_VREL,ID_DAMP_VREL,
     .                          FR_DAMP_VREL,IPARIT,ISPMD,OUTPUT%TH%WFEXT)
                  ELSE
                    CALL DAMPING51(
     .                          3+IRODDL*3,NODES%V       ,
     .                          NODES%VR      ,NODES%A       ,NODES%AR      ,NODES%MS        ,NODES%IN      ,
     .                          DAMPR   ,DAMP    ,IGRNOD  ,NODES%WEIGHT    ,NODES%TAG_S_RBY,
     .                          SKEWS%SKEW    ,ICONTACT,IDAMP_RDOF_TAB     ,NDAMP_VREL,ID_DAMP_VREL,
     .                          FR_DAMP_VREL,IPARIT,ISPMD,OUTPUT%TH%WFEXT)
                  END IF
!$OMP END SINGLE

                END IF
              END IF

!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

              IF(IMON>0) CALL STOPTIME(TIMERS,52)
C-----------------------------------------------
              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 2 ON 3 TO GET FREAC/MREAC
C     damping forces
C      FREAC is now : (FEXT+FINT) + (Fgrav+Fbcs_cyclic+Fcentrif) + (Fdamp)
C-----------------------------------------------
                IFLAG =  1
                CALL REACTION_FORCES_2(NODFTSK,NODLTSK,NODES%A,NODES%AR,NODES%MS,NODES%IN,FREAC,IFLAG)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

              END IF

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              IF(CPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C    /TH/NODE (REAC) FIRST CALL WITH IFLAG=-1
C      imposed loads
C-----------------------------------------------
                IFLAG = -1
                CALL REACTION_FORCES_TH(NODFTSK,NODLTSK ,NODES%A    ,NODES%AR     ,NODES%MS    ,
     .                                    NODES%IN     ,FTHREAC ,IFLAG,NODREAC)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              END IF

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C========================================================================================
C                                          NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------
C          STATIC
C--------------------------
              IF(ISTAT/=0) THEN
                IF (IMON>0) CALL STARTIME(TIMERS,6)
                IF (IMONM > 0) CALL STARTIME(TIMERS,49)
                IF (NS10E>0) THEN
                  CALL STATIC(VMD    ,NODES%VR,NODES%A,NODES%AR,NODES%MS,NODES%IN,IGRNOD,NODES%WEIGHT_MD,OUTPUT%TH%WFEXT)
                ELSE
                  CALL STATIC(NODES%V,NODES%VR,NODES%A,NODES%AR,NODES%MS,NODES%IN,IGRNOD,NODES%WEIGHT_MD,OUTPUT%TH%WFEXT)
                END IF
                IF (IMONM > 0) CALL STOPTIME(TIMERS,49)
                IF (IMON>0) CALL STOPTIME(TIMERS,6)
              ENDIF
              ENCIN = ZERO
              ENROT = ZERO
              ENCIN2 = ZERO
              ENROT2 = ZERO
C--------------------------
C          IMPOSED VELOCITIES & DISPLACEMENTS 
C--------------------------
              IF(NFXVEL/=0) THEN
                IF(IMON>0) THEN
                  CALL STARTIME(TIMERS,6)
                  CALL STARTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STARTIME(TIMERS,44)
                ENDIF
                CALL FIXVEL(IBFV  ,NODES%A     ,NODES%V      ,NPC    ,TF  ,
     2                      VEL   ,NODES%MS    ,NODES%X      ,SKEWS%SKEW   ,NODES%AR  ,
     3                      NODES%VR    ,NODES%IN    ,NSENSOR,SENSORS%SENSOR_TAB,
     4                      NODES%WEIGHT,NODES%D     ,RBY    ,IFRAME ,
     5                      XFRAME,NODES%DR    ,PTR_SMS, NODES,
     6                      TT_DOUBLE,NODES%DDP,PYTHON ,OUTPUT%TH%WFEXT)

                IF (FXVEL_FGEO ==1) THEN
                  CALL FIXFINGEO(PYTHON, NODES, IBFV   ,NPC    ,TF     ,
     2                         VEL    ,SENSORS%SENSOR_TAB ,
     3                         CPTREAC,NODREAC,PTR_SMS,NSENSOR    ,
     4                         FTHREAC, OUTPUT%TH%WFEXT   )
                ENDIF
C
                IF(IMON>0) THEN
                  IF(IMONM > 0) CALL STOPTIME(TIMERS,44)
                  CALL STOPTIME(TIMERS,TIMER_KIN)
                  CALL STOPTIME(TIMERS,6)
                ENDIF
              ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              IF(CPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C    /TH/NODE (REAC) SECOND CALL WITH IFLAG=+1
C      imposed loads
C----------------------------------------------
                IFLAG = 1
                CALL REACTION_FORCES_TH(NODFTSK,NODLTSK ,NODES%A  ,NODES%AR   ,NODES%MS  ,NODES%IN   ,FTHREAC ,IFLAG,NODREAC)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

              END IF

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
              IF (IMONM > 0) CALL STARTIME(TIMERS,43)

              CALL PYTHON_BEGIN_OPENMP(PYTHON)  
!$OMP PARALLEL

C-------------------
C         RIGID WALL
C---- // -----------
C               write(6,*) "FOPT size:", size(OUTPUT%DATA%FOPT,1), size(OUTPUT%DATA%FOPT,2)
C               write(6,*) "NSECT, NRBODY, NRWALL:", NSECT, NRBODY,NRWALL
C               call flush(6)

              IF(NRWALL>0.AND.IDTMINS==1)THEN
C
C         Obsolete
              ELSEIF(NRWALL>0.AND.(IDTMINS==2.OR.IDTMINS_INT/=0))THEN
                CALL RGWAL0(
     1          NODES%X           ,NODES%A        ,NODES%V      ,RWALL%RWBUF   ,RWALL%LPRW,
     2          RWALL%NPRW        ,NODES%MS       ,FSAV(1,NINTER+1),RWALL%FR_WALL ,
     3          OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)),
     4          RWALL%RWSAV       ,NODES%WEIGHT   ,FRWL6   ,NODXI_SMS, NODES%WEIGHT_MD,
     5          SENSORS%SFSAV,SENSORS%FSAV,SENSORS%STABSEN,SENSORS%TABSENSOR, OUTPUT%TH%WFEXT, OUTPUT%TH%WFEXT_MD)
              ELSE
                CALL RGWAL0(
     1          NODES%X           ,NODES%A        ,NODES%V      ,RWALL%RWBUF   ,RWALL%LPRW,
     2          RWALL%NPRW        ,NODES%MS       ,FSAV(1,NINTER+1),RWALL%FR_WALL ,
     3          OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)),
     4          RWALL%RWSAV       ,NODES%WEIGHT   ,FRWL6   ,NATIV_SMS, NODES%WEIGHT_MD,
     5          SENSORS%SFSAV,SENSORS%FSAV,SENSORS%STABSEN,SENSORS%TABSENSOR, OUTPUT%TH%WFEXT, OUTPUT%TH%WFEXT_MD)
              ENDIF

!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
              IF (IMONM > 0) CALL STOPTIME(TIMERS,43)

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------
C         IMPOSED TEMPERATURES
C--------------------------
              IF (GLOB_THERM%NFXTEMP  > 0 .AND. GLOB_THERM%ITHERM_FE > 0) THEN
                IF (IMON>0) THEN
                  CALL STARTIME(TIMERS,6)
                  CALL STARTIME(TIMERS,TIMER_KIN)
                  IF(IMONM > 0) CALL STARTIME(TIMERS,44)
                ENDIF
                CALL FIXTEMP(PYTHON,IBFTEMP  ,FBFTEMP    ,NODES%TEMP    ,NPC    ,TF  ,
     1                       NSENSOR  ,SENSORS%SENSOR_TAB,GLOB_THERM,SNPC)
                IF (IMON>0) THEN
                  IF(IMONM > 0) CALL STOPTIME(TIMERS,44)
                  CALL STOPTIME(TIMERS,TIMER_KIN)
                  CALL STOPTIME(TIMERS,6)
                ENDIF
              ENDIF
C---------------
              IF(ISECUT/=0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)
                CALL SECTION_IO (
     1            NSTRF,NODES%D,NODES%DR,NODES%V,NODES%VR,FSAV(1,1+NINTER+NRWALL+NRBODY),
     2            SECFCUM,NODES%A             ,NODES%AR    ,SECBUF,NODES%MS      ,NODES%IN     ,
     3            NODES%X      ,OUTPUT%DATA%FOPT,NODES%WEIGHT,XSEC  ,NODES%BOUNDARY_ADD,NODES%BOUNDARY,
     4            RG_CUT ,IAD_CUT       ,FR_CUT,NODES%WEIGHT_MD,IOLDSECT,
     5            SENSORS%STABSEN,SENSORS%SFSAV,SENSORS%TABSENSOR ,SENSORS%FSAV ,OUTPUT%TH%WFEXT )
                IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
              ENDIF

C-----------------------------------------------------
c     adaptive meshing + static condensation : velocities on static nodes.
C-----------------------------------------------------

C========================================================================================
C                                    PARALLEL SECTION (SMP)
C========================================================================================

              IF(ISTATCND/=0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,38)
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                CALL CNDINT(ELEMENT%SHELL%IXC, IPART(K3), IXTG, IPART(K8), IPART,
     2                    ITSK   ,NODES%A        ,NODES%V      ,NODES%AR       ,NODES%VR    ,
     3                    NODES%MS     ,NODES%IN       ,NODFTSK,NODLTSK  ,NODES%X     ,
     4                    SH4TREE,SH3TREE ,NODES%ITAB  ,NODES%STIFN ,NODES%STIFR ,
     5                    MSCND  ,INCND   )
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMON>0) CALL STOPTIME(TIMERS,38)
C
C         reimpose bcs.
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF (IMONM > 0) CALL STARTIME(TIMERS,42)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                CALL BCS10(NODFTSK,NODLTSK ,NODES%ICODT  ,NODES%ICODR,NODES%ISKEW,
     2                       SKEWS%SKEW ,NODES%A      ,NODES%AR     ,NODES%MS   ,NODES%V    ,
     3                       NODES%VR  )

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMONM > 0) CALL STOPTIME(TIMERS,42)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
              END IF


C-----------------------------------------------------
C     adaptive meshing : velocities on secnd nodes
C-----------------------------------------------------

              IF(NADMESH/=0)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,38)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                ITSK = OMP_GET_THREAD_NUM()

                CALL ADMVIT(ELEMENT%SHELL%IXC, IPART(K3), IXTG, IPART(K8), IPART,
     1                      ITSK   ,NODES%A        , NODES%V   , NODES%AR       , NODES%VR   ,
     2                    SH4TREE,SH3TREE ,NODES%TEMP ,GLOB_THERM%ITHERM_FE)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMON>0) CALL STOPTIME(TIMERS,38)
              END IF

C--------------------------------------------------
c     rigid bodies velocities on secondary nodes
C--------------------------------------------------
              IF(NRBYKIN>0)THEN

                IF(IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
                IF(IMONM > 0) CALL STARTIME(TIMERS,40)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------
C  PARALLEL COMPUTATION
C      [NRBODY1][NRBODY2]...   (ON FREE TASK)
C // --------------------------

             CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL

                CALL RBYVIT(
     1            RBY    ,NODES%X       ,NODES%V        ,NODES%VR   ,SKEWS%SKEW ,
     2            FSAV   ,LPBY    ,NPBY     ,NODES%ISKEW,NODES%ITAB ,
     3            NODES%WEIGHT ,NODES%A       ,NODES%AR       ,NODES%MS   ,NODES%IN   ,
     4            KINDRBY,IRBKIN_L,NRBYKIN_L,NODREAC,FTHREAC,
     5            FREAC  ,NHIER_RBY)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

C--------------------------------------------------
                IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
                IF(IMONM > 0) CALL STOPTIME(TIMERS,40)

              ENDIF
C---------------------------------------------------
C  rigid material
C--------------------------------------------------
C
              IF(IRIGID_MAT > 0 ) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL

                CALL RMATACCE(RBYM , ARBYM, ARRBYM, VRBYM, VRRBYM ,
     1                        IRBYM,LNRBYM,NODES%X       ,NODES%A    ,NODES%AR    ,
     2                        NODES%V    ,NODES%VR    ,KINDRBYM)

!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)

              ENDIF

C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------
C         Flexible bodies solution and velocities on secondary nodes
C--------------------------------------------------
              IF (NFXBODY>0) THEN
                CALL FXBYVIT(output,FXBIPM, FXBNOD, FXBMOD, FXBGLM, FXBLM ,
     .                       FXBMVN, FXBMCD, FXBSE , FXBSV , FXBVIT,
     .                       FXBACC, FXBRPM, NODES%V     , NODES%VR    , NODES%A     ,
     .                       NODES%AR    , NODES%MS    , NODES%IN    , NODES%WEIGHT, FSAV  ,
     .                       FXBFC , FXBEDP, NODES%BOUNDARY_ADD, NODES%BOUNDARY)
              END IF
C--------------------
C      RBE3
C--------------------
              IF (NRBE3>0) THEN
                CALL RBE3V(RBE3,NODES,SKEWS%SKEW  )
              ENDIF
C--------------------
C      RBE2
C--------------------
              IF (NRBE2>0) THEN
                IF (ITASK==0) THEN
                  CALL RBE2V(IRBE2 ,LRBE2 ,NODES%X    ,NODES%A     ,NODES%AR    ,
     1                       NODES%V     ,NODES%VR    ,SKEWS%SKEW )
                ENDIF
              ENDIF
C--------------------------------------------------
C  DAA  normal accelerations
C--------------------------------------------------
              IF (NFLOW>0) CALL FLOW1(IFLOW, RFLOW, NBGAUGE, NODES%A)

C========================================================================================
C                                   DOMAIN 0
C========================================================================================

              IF (ISPMD==0) THEN
C------------------------
C       INTERFACES 14 & 15
C--- //0 ----------------
                IF(NINTER/=0)THEN
                  IF (IMON>0) THEN
                    CALL STARTIME(TIMERS,TIMER_CONTSORT)
                  ENDIF
                  CALL I14WFS(OUTPUT,IPARI,INTERFACES%INTBUF_TAB,IGRSURF,FSAV)
                  IF (IMON>0) THEN
                    CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                  ENDIF
                ENDIF
              ENDIF
C----
              IF (NS10E>0) CALL S10CNDI2A(ICNDS10 ,ITAGND ,NODES%A      )
C------------------------
C         INTERFACES TIED
C--- //0 ----------------

              IF(NINTER/=0)THEN
                IF (IMON>0) THEN
                  CALL STARTIME(TIMERS,6)
                  CALL STARTIME(TIMERS,TIMER_CONTSORT)
                ENDIF
                IF (IMONM > 0) CALL STARTIME(TIMERS,28)
                DO K=NHIN2,0,-1
                  CALL INTTI2(IPARI,NODES%X    ,NODES%V    ,NODES%A    ,
     2                        NODES%VR   ,NODES%AR   ,K    ,NODES%MS   ,NODES%IN   ,NODES%WEIGHT,WA,SKEWS%SKEW,
     3                        INTERFACES%INTBUF_TAB)
                ENDDO
                IF (IMONM > 0) CALL STOPTIME(TIMERS,28)
                IF (IMON>0) THEN
                  CALL STOPTIME(TIMERS,TIMER_CONTSORT)
                  CALL STOPTIME(TIMERS,6)
                ENDIF
              ENDIF
C
C-----------------------------------------------------
C         KINEMATIC CONDITIONS FOR SEATBELTS
C-----------------------------------------------------

              IF (NSLIPRING + NRETRACTOR > 0) CALL KINE_SEATBELT_VEL(NODES%A,NODES%V,NODES%X,NODES%XDP)
C
C-----------------------------------------------------

              IF (NS10E>0) CALL S10CNDI2A1(ICNDS10 ,ITAGND ,NODES%A      )

              IF(NUMFRAM /= 0 .AND. N2D == 0)THEN
C----------------------------
C       MOVING FRAME - RETRIEVE ACCELERATION.
C--- //0 ----------------
                IF (IMON>0)CALL STARTIME(TIMERS,6)
                IF (IMONM > 0) CALL STARTIME(TIMERS,49)
                CALL MOVFRA1(XFRAME,IFRAME ,NODES%X, NODES%V ,NODES%A ,NODES%AR)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,49)
                IF (IMON>0)CALL STOPTIME(TIMERS,6)
              ENDIF

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_KIN)
              IF (IMONM > 0) CALL STARTIME(TIMERS,40)

              IF(NUMSPH/=0.AND.NSPHSOL/=0)THEN
C------------------------
C         Solids impose velocity to SPH
C--- // ----------------
                IF (IMONM > 0) CALL STARTIME(TIMERS,48)
                IF (IMONM > 0) CALL STARTIME(TIMERS,89)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
                ITSK = OMP_GET_THREAD_NUM()

                CALL SOLTOSPHA(
     1          ITSK    ,NODES%V       ,NODES%A         ,NODES%MS             ,PM         ,
     2          IPART   ,IXS     ,IPART(K1) ,KXSP           ,IPART(K10) ,
     3          IRST    ,SPBUF   ,PARTSAV   ,SOL2SPH ,IPARG       ,
     4          NGROUNC ,IGROUNC ,ELBUF_TAB ,IGEO)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

                IF (IMONM > 0) CALL STOPTIME(TIMERS,89)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,48)
              ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C--------------------
C       ACCELEROMETRE
C-- // --------------
              IF (NACCELM > 0) THEN
                 CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL DO PRIVATE(K,N,ISK)
                DO K=1,NACCELM
C check that proc is concerned. Otherwise necessary exchange for sensor, th
                  IF(IACCP(K)==ISPMD+1)THEN
                    N  = LACCELM(1,K)
                    IF(N > 0 .AND. N/=2*NUMNODG )THEN
                      ISK= LACCELM(3,K)
                      CALL ACCEL1(
     .                  NODES%A(1,N),ACCELM(1,K),ACCELM(2,K),ACCELM(8,K),ACCELM(14,K),
     .                  ACCELM(20,K),ACCELM(23,K),SKEWS%SKEW(1,ISK))
                    END IF
                  END IF
                ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO
                  CALL PYTHON_END_OPENMP(PYTHON)
              END IF ! Fin NACCELM > 0

C--------------------
C       SPH Gauges
C-- // --------------
              IF (NBGAUGE > 0) THEN
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(N)
                DO K=1,NBGAUGE
                  IF(IGAUP(K)==ISPMD+1)THEN
                    N  = LGAUGE(1,K)
C SPH Gauges
                    IF(N < -(NUMELS+NUMELQ+NUMELTG))THEN
                      CALL SPGAUGE_F(GAUGE(10,K),GAUGE(9,K),GAUGE(14,K),GAUGE(22,K),GAUGE(30,K),4)
                    END IF
                  END IF
                ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO
              CALL PYTHON_END_OPENMP(PYTHON)
              END IF ! Fin NBGAUGE > 0

C--------------------
C       SPH Flow
C-- // --------------
              IF (NSPHIO > 0) THEN
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(IVAD)
                DO K=1,NSPHIO
                  IF (ISPHIO(1,K)>1) THEN
                    IVAD = ISPHIO(4,K)
                    CALL SPGAUGE_F(
     .                VSPHIO(IVAD+13),VSPHIO(IVAD+15),VSPHIO(IVAD+18),VSPHIO(IVAD+20),VSPHIO(IVAD+16),1)
                  END IF
                ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO
                CALL PYTHON_END_OPENMP(PYTHON)
              END IF
            CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
              IF(NRBYKIN>0)THEN
                CALL RBYCOR(
     1            RBY ,NODES%X   ,NODES%V    ,NODES%VR     ,SKEWS%SKEW    ,FSAV     ,
     2            LPBY,NPBY,NODES%ISKEW,NODES%ITAB   ,NODES%WEIGHT  ,NODES%A        ,
     3            NODES%AR  ,NODES%MS  ,NODES%IN   ,KINDRBY,IRBKIN_L,NRBYKIN_L,
     4            NODES%WEIGHT_MD,MS_2D)
              ENDIF
C---------RBE2
              IF(NRBE2>0)THEN
C
                CALL RBE2COR(IRBE2 ,LRBE2 ,NODES%X    ,NODES%V     ,NODES%VR    ,
     2                       SKEWS%SKEW  ,NODES%ISKEW ,NODES%ITAB ,NODES%WEIGHT,NODES%A     ,
     3                       NODES%AR    ,NODES%MS0   ,NODES%IN   ,NODES%WEIGHT_MD)
              ENDIF
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C------------------------------------------
C       SYNCHRONIZATION:  A  V  (FOR OUTPUTS)
C------------------------------------------
C       SENSORS
C-- // --------------
C-----------------------------------------------
C     Exchange SENSORS%FSAV for sensors 6-13
C-----------------------------------------------
              IF (NSENSOR> 0 ) THEN
                IF (NSPMD > 1 .AND. SENSORS%STABSEN > 0) THEN
                  DIM6=12
                  DIM_EXCH = SENSORS%SFSAV
                  CALL SPMD_EXSUM_FB6(DIM6,DIM_EXCH,SENSORS%FSAV)
                ENDIF
              ENDIF
c
              IF (NSENSOR > 0) THEN

                CALL SENSOR_ENER_SAV(NSENSOR,SENSORS%SENSOR_TAB,PARTSAV ,PARTSAV2)

                ! pre-computation and mpi communication for type 16 sensor
                IF (SENSORS%COMM_SENS16%BOOL) THEN
                  CALL SENSOR_DIST_SURF0(NSENSOR,SENSORS%SENSOR_TAB,NODES%X,
     *                                   IGRSURF,SENSORS%COMM_SENS16)
                ENDIF

                ! pre-computation and mpi communication for type 17 sensor
                IF (SENSORS%COMM_SENS17%BOOL) THEN
                  CALL SENSOR_TEMP0(NSENSOR,SENSORS%SENSOR_TAB,IGRNOD,NODES%TEMP,NODES%WEIGHT,SENSORS%COMM_SENS17,
     *                              SENSORS%SENSOR_STRUCT)
                ENDIF

                IF (NSPMD > 1) THEN
                  CALL SENSOR_SPMD(SENSORS%SENSOR_TAB,IPARI ,RWALL%NPRW      ,ISENSP    ,NSENSP    ,
     .                         XSENS     ,NODES%X       ,ACCELM   ,IACCP     ,NACCP     ,
     .                         GAUGE     ,IGAUP   ,NGAUP    ,PARTSAV2  ,NSENSOR,
     .                         SENSORS%COMM_SENS14,SENSORS%SENSOR_STRUCT  )
                ENDIF
c
                ! check activation condition of base sensors
                CALL SENSOR_BASE(SENSORS  ,NSENSOR   ,TT        ,DT2       ,
     .               XSENS     ,IPARI     ,PARTSAV2  ,GAUGE     ,FSAV      ,
     .               NODES%X         ,NODES%V         ,NODES%A         ,ACCELM    ,RWALL%NPRW      ,
     .               SUBSETS   ,IGRSURF   ,IGRNOD    ,PYTHON)
c
                ! check activation condition of logical sensor hierarchy
                CALL SENSOR_LOGICAL(SENSORS)

              ENDIF  !  NSENSOR > 0
C========================================================================================

              IF (IMONM > 0) CALL STOPTIME(TIMERS,40)
              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_KIN)
C-------------------------------------------------------
C     Kinematic conditions by Lagrange Multipliers
C-------------------------------------------------------

              IF(LAG_NCF+LAG_NCL>0)THEN
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
C case NSPMD == 1 and options supported only in SMP
                IF(LAG_SEC == 1 .AND. NSPMD == 1)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
                  CALL SMP_INIT(
     1              ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2              IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3              GREFTSK,GRELTSK)

                  CALL LAG_MULT(output,
     1               IPARI    ,NODES%X        ,NODES%A        ,
     2               WA(NWAFTSK), NODES%V        ,NODES%MS       ,NODES%IN       ,NODES%VR       ,
     3               ITSK     ,WA  ,NODES%ITAB     ,IXS      ,IXS(L2)  ,
     4               IXS(L3)  ,IGRNOD     ,OUTPUT%DATA%VECT_CONT,FSAV     ,
     5               SKEWS%SKEW     ,NODES%AR       ,LAMBDA   ,LAGBUF   ,IBCSLAG  ,
     6               IXS(L1)  ,GJBUFI   ,GJBUFR   ,IBMPC    ,RBMPC    ,
     7               NPBYL    ,LPBYL    ,IBFV     ,VEL      ,NPC      ,
     8               TF         ,NEWFRONT ,ICONTACT ,RWALL%RWBUF    ,RWALL%LPRW     ,
     9               RWALL%NPRW     ,RBYL     ,NODES%D        ,NODES%DR       ,NODES%KINET    ,
     A               NSENSOR  ,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB, H3D_DATA ,IGRBRIC,
     B               PYTHON,NODES)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

                ELSE
                  IF(ISPMD==0) THEN
                    NBNCL  = FR_LAGF(1,NSPMD+1)
                    NBIKL  = FR_LAGF(2,NSPMD+1)
                    NBNODL = FR_LAGF(3,NSPMD+1)
                    NBNODLR= NBNODL*MAX(1,IRODDL)
                  ELSE
                    NBNCL = FR_LAGF(1,ISPMD+1)
                    NBIKL = FR_LAGF(2,ISPMD+1)
                    NBNODL=0
                    NBNODLR=0
                  END IF
                  CALL LAG_MULTP(output,
     1               IPARI    ,NODES%X        ,NODES%A        ,
     2               WA(NWAFTSK), NODES%V        ,NODES%MS       ,NODES%IN       ,NODES%VR       ,
     3               WA, NODES%ITAB     ,IXS      ,IXS(L2)  ,
     4               IXS(L3)  ,OUTPUT%DATA%VECT_CONT     ,FSAV     ,
     5               SKEWS%SKEW     ,NODES%AR       ,LAMBDA   ,LAGBUF   ,IBCSLAG  ,
     6               IXS(L1)  ,GJBUFI   ,GJBUFR   ,IBMPC    ,RBMPC    ,
     7               NPBYL    ,LPBYL    ,IBFV     ,VEL      ,NPC      ,
     8               TF       ,NEWFRONT ,ICONTACT ,RWALL%RWBUF    ,RWALL%LPRW     ,
     9               RWALL%NPRW     ,RBYL     ,NODES%D        ,NODES%DR       ,NODES%KINET    ,
     A               NODES%NODGLOB  ,NODES%WEIGHT   ,NBNCL    ,NBIKL    ,NBNODL   ,
     B               NBNODLR  ,FR_LAGF  ,LLAGF    ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY  ,
     C               INTERFACES%INTBUF_TAB ,H3D_DATA, PYTHON, NODES)
                END IF
              ENDIF
c--------------
 111          CONTINUE
c--------------

              NTMP = IMCONV
              IF (IMPDEB==1.AND.IMCONV==0) THEN
                IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
                  TTMP=TTMP+OUTPUT%DTANIM
                  IF ((IRAD2R==1).AND.(IRESP==1)) THEN
                    TT_DP=TT_DP+TTMP
                    TT = TT_DP
                  ELSE
                    ! double precision accumulation and then cast in simple precision
                    TT_DOUBLE = TT_DOUBLE + TTMP
                    IF (IMPL_S==1.OR.NEIG>0) THEN
                      TT = TT + TTMP
                    ELSE
                      IF(IRESP == 1)THEN
                        TT = SNGL(TT_DOUBLE)
                      ELSE
                        TT = TT_DOUBLE
                      ENDIF
                    ENDIF
                  ENDIF
                  NTMP = 1
                ENDIF
              ENDIF
C--------------------------------------------------
              IF( ( ANIM_CE(2156)/=0 .OR. H3D_DATA%SH_SCAL_ERR_THK /=0)
     .             .AND.((TT>=OUTPUT%TANIM .AND. TT<=OUTPUT%TANIM_STOP).OR.TT>=TOUTP.OR.
     .                  (MANIM>=4.AND.MANIM<=15))) THEN

C========================================================================================
C                                         PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                CALL ERR_THK(ELEMENT%SHELL%IXC     ,IXTG    ,IPARG   ,NODES%BOUNDARY_ADD,NODES%BOUNDARY    ,
     .                       NODES%WEIGHT  ,NODES%X       ,ELBUF_TAB,IPART   ,IPART(K3)  ,
     .                       IPART(K8)  ,ITSK   ,NODFTSK ,NODLTSK ,ERR_THK_SH4,
     .                       ERR_THK_SH3,SH4TREE,SH3TREE,
     .                       AREA_SH4,  AREA_SH3,  AREA_NOD,
     .                       THICK_SH4, THICK_SH3, THICK_NOD)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              END IF

              K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
              K2=K1+NUMELS
              K3=K2+NUMELQ
              K4=K3+NUMELC
              K5=K4+NUMELT
              K6=K5+NUMELP
              K7=K6+NUMELR
              K8=K7
              K9=K8+NUMELTG
              CALL FUNCT_PYTHON_UPDATE_ELEMENTS(PYTHON, ISPMD,
     .                 N2D,  NGROUP,  NIXC, NIXTG, NIXS,NIXQ,
     .                 NUMGEO, NUMELC, NUMELTG, NUMELS, NUMELQ, NUMMAT, NUMNOD,
     .                 NPARG, NPROPG, NPROPM, NPROPMI, NPROPGI,
     .                 SNERCVOIS, SNESDVOIS, SLERCVOIS, SLESDVOIS,
     .                 STHKE, SEANI, NPART,
     .                 ELBUF_TAB   ,IPARG       ,GEO        ,
     .                 ELEMENT%SHELL%IXC,IXTG, IXS  ,IXQ ,PM          ,BUFMAT     ,
     .                 EANI,
     .                 IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                 NODES         ,W           ,ALE_CONNECTIVITY,
     .                 NERCVOIS    ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                 M51_N0PHAS, M51_NVPHAS, STACK       ,
     .                 IPART(K3:K4-1),IPART(K1:K2-1) ,IPART(K8:K9-1),IPART(K1:K2-1),
     .                 MULTI_FVM ,
     .                 MAT_ELEM%MAT_PARAM, OUTPUT%DATA%FANI_CELL,GLOB_THERM%ITHERM)


C
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              IF(NTMP==1)THEN
                IF(IDTMINS==1)THEN
C
C        Obsolete
                ELSEIF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C--------------------------------------------------
                  IF (IMON>0) CALL STARTIME(TIMERS,39)
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                  CALL SMS_ENCIN_2(TIMERS,
     1                  ITSK     ,NODFTSK  ,NODLTSK  ,NODXI_SMS ,
     2                  NODES%MS       ,JAD_SMS  ,JDI_SMS  ,LT_SMS    ,INDX1_SMS,
     3                  DIAG_SMS ,NODES%BOUNDARY_ADD ,NODES%BOUNDARY  ,NODES%WEIGHT    ,NODES%V        ,
     4                  NODES%A        ,X_SMS    ,Y_SMS    ,Z_SMS     ,XMOM_SMS ,
     5                  NODES%ICODT    ,NODES%ICODR    ,NODES%ISKEW    ,SKEWS%SKEW      ,IBFV      ,
     6                  VEL      ,NPC      ,TF       ,NODES%X         ,NODES%D         ,
     7                  SENSORS  ,IFRAME   ,XFRAME    ,JADI_SMS ,
     8                  JDII_SMS ,LTI_SMS  ,ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,
     9                  FR_RMS   ,NPBY     ,TAGSLV_RBY_SMS,INTSTAMP,CPTREAC,
     A                  NODREAC  ,FTHREAC  ,NODES%AR        ,NODES%VR        ,
     B                  NODES%DR       ,NODES%IN       ,RBY      ,IRBE2     ,LRBE2     ,
     C                  IAD_RBE2 ,FR_RBE2M ,NMRBE2   ,R2SIZE    ,RBE3%IRBE3     ,
     D                  RBE3%LRBE3    ,RBE3%FRBE3    ,RBE3%mpi%IAD_RBE3,RBE3%mpi%FR_RBE3  ,RBE3%mpi%FR_RBE3MP ,
     E                  RBE3%RRBE3    ,RBE3%RRBE3_PON,IAD_RBY  ,FR_RBY6   ,RBY6      ,
     F                  LPBY     ,TAGMSR_RBY_SMS,RBE3%irotg_sz,NODII_SMS,INDX2_SMS,
     G                  IBCSCYC  ,LBCSCYC ,OUTPUT, MSKYI_FI_SMS,LIST_SMS,
     H                  LIST_RMS ,SMS_VFI,sz_mw6,mw6)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

                  IF (IMON>0) CALL STOPTIME(TIMERS,39)

                END IF
              END IF
C---
              NISKY_SMS=0

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF (ICRACK3D > 0 .AND. NLEVSET > 0) THEN
                CALL XFEOFF(XFEM_TAB ,
     .                      IPARG  ,ELEMENT%SHELL%IXC      ,NGROUC      ,IGROUC  ,IEL_CRK    ,
     .                      ELCUTC ,IXTG     ,IADC_CRK    ,NODES%BOUNDARY_ADD,IAD_EDGE   ,
     .                      FR_EDGE,FR_NBEDGE,NODES%BOUNDARY     ,NXLAYMAX,INOD_CRK   ,
     .                      CRKEDGE,XEDGE4N  ,XEDGE3N    )
              ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C-----------------------------
C FINITE VOLUME METHOD FOR ALE
C-----------------------------

              IF(ALEFVM_Param%IEnabled>0)THEN
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C Init var parallel SMP
                CALL SMP_INIT(
     1           ITSK   , NODFTSK , NODLTSK , NUMNTSK, NDTSK   ,
     2           IPMTSK , PARTFTSK, PARTLTSK, NWAFTSK, IGMTSK  ,
     3           GREFTSK, GRELTSK)
                DT2TT    = DT2T
                NELTSTT  = NELTST
                ITYPTSTT = ITYPTST
                IF(IPARIT == 1) NDTSK = 1
                CALL ALEFVM_MAIN(
     1           NODES%X               , NODES%V     ,
     2           ELBUF_TAB             , NODES%VR    ,
     3           ALE_CONNECTIVITY      , IPARG       , IXS        ,
     4           ALE_CONNECTIVITY%NALE ,
     5           ITSK                  ,NODFTSK      , NODLTSK    ,IPM , ale%global%nv46,MSNF     )

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

              IF (IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

              IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C--- // ---------------------------------------
C     OUTPUT (ANIM,OUTP,H3D,TH) STEP 3 ON 3 TO GET FREAC/MREAC
C       FREAC is now : FTOTAL - (FEXT+FINT) - (Fgrav+Fbcs_cyclic+Fcentrif) - (Fdamp)
C-----------------------------------------------
                CALL REACTION_FORCES_3(NODFTSK,NODLTSK,NODES%A,NODES%AR,NODES%MS,NODES%IN,FREAC)

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
              END IF

              IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
              IF(NCYCLE > 0) THEN

                IF (IMON>0) CALL STARTIME(TIMERS,MACRO_TIMER_GENH3D1)
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
c
                ITSK = OMP_GET_THREAD_NUM()
C--------------------------------------------------
C       /H3D/TMAX
C--------------------------------------------------
                CALL UPD_TMAX(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .                   IXS  ,IXS(L1) ,IXS(L3) ,IXS(L2) ,IXQ     ,
     .                   ELEMENT%SHELL%IXC  ,IXTG   ,IXT    ,IXP     ,IXR     ,
     .                   NODES%X  ,NODES%D       ,NODES%V       ,NODES%BOUNDARY_ADD,NODES%BOUNDARY ,
     .                     NODES%WEIGHT ,IPM   ,IGEO    ,STACK   ,ITSK    )

!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)
                IF (IMON>0) CALL STOPTIME(TIMERS,MACRO_TIMER_GENH3D1)
              END IF

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================
C--------------------------------------------------
              IF(NTMP==1)THEN
C--------------------------------------------------
                IF (IMON>0) THEN
                  CALL STARTIME(TIMERS,TIMER_IO)
                ENDIF
                CALL TRACE_IN(5,0,ZERO)
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20
C Do not stop yet
                IF (TT<=TSTOP.AND.ILASTANIM==3) THEN
                  ILASTANIM=0
                END IF
                IF (TT<=TSTOP.AND.ILASTDYNAIN==3) THEN
                  ILASTDYNAIN=0
                END IF
                IF (TT<=TSTOP.AND.ILASTH3D==3) THEN
                  ILASTH3D=0
                END IF
C Regular animation
                IF ((TT>OUTPUT%TANIM .AND. TT<=OUTPUT%TANIM_STOP).AND.ILASTANIM==0) THEN
                  ILASTANIM=3
                  LASTANIMCYCLE=NCYCLE
                END IF

                IF (TT>TSTAT.AND.ILASTANIM==0) THEN
                  ILASTANIM=3
                  LASTSTATCYCLE=NCYCLE
                END IF
                IF (TT>DYNAIN_DATA%TDYNAIN.AND.ILASTDYNAIN==0) THEN
                  ILASTDYNAIN=3
                  LASTDYNCYCLE=NCYCLE
                END IF
                IF (TT>H3D_DATA%TH3D.AND.ILASTH3D==0) THEN
                  ILASTH3D=3
                  LASTH3DCYCLE=NCYCLE
                END IF

                IF(DEBUG(MACRO_DEBUG_CHKSM) >0) THEN
                  IF(MOD(NCYCLE,DEBUG(MACRO_DEBUG_CHKSM)) == 0  ) THEN
                    CALL SPMD_FLUSH_ACCEL(NCYCLE,  ISPMD,   NSPMD, NUMNOD,
     .                                    NUMNODG, NUMNODM, NODES%A,     NODES%ITAB,
     .                                    NODES%WEIGHT,  NODES%NODGLOB)
                  ENDIF
                ENDIF

                IF(SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(PTRX, NODES%X,NUMNOD)
                  CALL ASSIGN_PTRX(PTRX_OFFSET, XYZ,NUMNOD)
                ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(PTRX, IMPBUF_TAB%X_A,NUMNOD)
                  CALL ASSIGN_PTRX(PTRX_OFFSET, XYZ ,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(PTRX, NODES%X,NUMNOD)
                  CALL ASSIGN_PTRX(PTRX_OFFSET, NODES%X,NUMNOD)
                ENDIF

                CALL SORTIE_MAIN(TIMERS,
     1          PM            ,NODES%D        ,NODES%V        ,ALE_CONNECTIVITY   ,W                ,
     2          ELBUF         ,IPARG          ,IXS            ,IXQ                ,ELEMENT%SHELL%IXC,
     3          IXT           ,IXP            ,IXR            ,IXTG               ,WA               ,
     4          NODES%ITAB    ,ptrX           ,GEO            ,NODES%MS           ,NODES%A          ,
     5          PARTSAV        ,ICUT           ,XCUT               ,
     6          LPBY             ,
     7          NPBY          ,NSTRF          ,RWALL%RWBUF          ,RWALL%NPRW               ,EBCS_TAB         ,
     8          TANI          ,INOISE         ,BUFNOIS        ,RBY                ,NEFLSW           ,
     9          NNFLSW        ,CRFLSW         ,FLSW           ,LOUT               ,NODES            ,
     B          FSAV          ,SKEWS%SKEW     ,ELBUF_TAB      ,CLUSTER            ,
     C          NODES%VR      ,NODES%IN       ,NODES%WEIGHT   ,FCLUSTER           ,MCLUSTER         ,
     D          DD_IAD        ,DMAS           ,ACCELM         ,GAUGE              ,
     E          IPARI         ,EANI           ,IPART          ,MAT_ELEM%MAT_PARAM ,
     F          IGRNOD        ,SUBSETS        ,
     G          NOM_OPT       ,NODES%AR       ,IGRSURF        ,BUFSF              ,IDATA            ,
     H          RDATA         ,KXX            ,IXX            ,BUFMAT             ,BUFGEO           ,
     I          KXSP          ,IXSP           ,NOD2SP         ,SPBUF              ,NODES%DR,
     J          FSAVD         ,LRIVET         ,RIVET          ,ISKWN              ,IFRAME           ,
     M          XFRAME        ,IXS(L1)        ,IXS(L2)        ,IXS(L3)            ,NDMA             ,
     N          MONVOL        ,VOLMON         ,IPM            ,IGEO               ,NODES%NODGLOB       ,
     .          NODES%BOUNDARY_ADD      ,NODES%BOUNDARY        ,
     U          FR_RBY2       ,IAD_RBY2       ,RWALL%FR_WALL        ,FR_SEC             ,FXBIPM        ,FXBRPM   ,
     V          NDIN          ,FXBDEP         ,FXBVIT         ,FXBACC             ,IFLOW         ,
     W          RFLOW         ,IPARTL         ,NPARTL         ,IACCP              ,NACCP         ,
     X          FASOLFR       ,IPART(I15ATH)      ,
     Y          FR_MV         ,IPART_STATE    ,SH4TREE        ,SH3TREE            ,NODES%TEMP    ,
     Z          THKE          ,ERR_THK_SH4    ,ERR_THK_SH3,
     a          INOD_PXFEM    ,FTHREAC        ,NODREAC        ,GRESAV             ,
     b          DIAG_SMS      ,SH4TRIM        ,SH3TRIM        ,XMOM_SMS      ,
     c          IRBE2         ,RBE3%IRBE3     ,LRBE2          ,RBE3%LRBE3         ,FR_RBE2       ,
     d          RBE3%mpi%FR_RBE3 ,IAD_RBE2    ,DXANCG         ,IEL_PXFEM          ,ZI_PLY        ,
     e          VFLOW         ,FCONTG         ,FNCONTG        ,FTCONTG            ,FREAC         ,
     f          INOD_CRK      ,IEL_CRK        ,ELCUTC         ,IADC_CRK           ,
     g          RES_SMS       ,SENSORS        ,
     h          QFRICINT      ,IGAUP          ,NGAUP          ,NODES%WEIGHT_MD    ,NCONT         ,
     i          INDEXCONT     ,NODGLOBXFE     ,NODEDGE        ,XFEM_TAB           ,
     j          ale%global%nv46,RTHBUF        ,KXIG3D         ,IXIG3D             ,KNOT          ,
     k          WIGE          ,NERCVOIS       ,NESDVOIS       ,LERCVOIS           ,LESDVOIS      ,
     l          CRKEDGE       ,STACK          ,ISPHIO         ,VSPHIO             ,NODES%ICODE   ,
     m          INDX_CRK      ,XEDGE4N        ,XEDGE3N        ,SPH2SOL            ,STIFN_TMP     ,
     n          STIFR_TMP     ,DRAPE_SH4N     ,DRAPE_SH3N     ,MS_2D              ,MULTI_FVM     ,
     o          SEGQUADFR     ,H3D_DATA       ,NODES%ISKEW    ,PSKIDS             ,ISKWP         ,
     p          KNOTLOCPC     ,KNOTLOCEL      ,PINCH_DATA     ,TAG_SKINS6         ,IRUNN_BIS     ,
     q          TF            ,NPC            ,DYNAIN_DATA    ,FCONT_MAX          ,MDS_MATID     ,
     r          IBCL          ,ILOADP         ,LLOADP        ,
     s          LOADP         ,TAGNCONT       ,LOADP_HYD_INTER,FORC               ,DRAPEG        ,
     t          USER_WINDOWS  ,OUTPUT         ,DT             ,OUTPUT%TH%TH_SURF%CHANNELS        ,
     u          TABLE         ,LOADS          ,SFANI          ,IPARIT             ,ptrX_offset   ,
     v          SZ_NPCONT2    ,NPCONT2        ,GLOB_THERM     ,PBLAST             ,OUTPUT%TH%WFEXT,
     w          MASS0_START)

                IF((MSTOP == 1 .AND. ICTLSTOP == 0) .OR. MSTOP == 2 .OR. DT2<=ZERO)THEN
                  CALL SORTIE_ERROR(
     1             NODES%V        ,NODES%NODGLOB    ,NODES%WEIGHT     ,NODES%ITAB       ,NODES%MS          ,
     2             NODES%MS0      ,10         ,PARTSAV    ,IPART      ,PM          ,
     3             IGEO     )
                END IF
C
                CALL TRACE_OUT(5)
C-------ADYREL----
                IF (ISTAT==3) CALL ENER_W0
                IF(coupling%active) CALL COUPLING_ADVANCE(coupling,DT2)
C------------
C       TIME
C------------
C     ILASTANIM=0 No additional animation
C     ILASTANIM=1 One more cycle needed
C     ILASTANIM=2 Additional cycle done
C     ILASTANIM=3 regular animation
                IF (ILASTANIM==1) THEN
                  ILASTANIM=2
                END IF
                IF (ILASTH3D==1) THEN
                  ILASTH3D=2
                END IF
                IF(T1S==TT)NCYCLE=NCYCLE+1
C-------> Multidomain single precision:  double precision time scale----
                IF ((IRAD2R==1).AND.(IRESP==1)) THEN
                  TT_DP=TT_DP+DT2
                  TT=TT_DP
                ELSE
                  ! double precision accumulation and then cast in simple precision
                  TT_DOUBLE = TT_DOUBLE + DT2
                  IF (IMPL_S==1.OR.NEIG>0) THEN
                    TT = TT + DT2
                  ELSE
                    IF(IRESP == 1)THEN
                      TT = SNGL(TT_DOUBLE)
                    ELSE
                      TT = TT_DOUBLE
                    ENDIF
                  ENDIF
                ENDIF
C May do 1 more cycle to write animation if TT = TOUTPUT%TANIM - 10%
                IF (IMADCPL /= 1) THEN
                  IF (OUTPUT%DTANIM>ZERO) THEN
                    IF((output%NB_ANIM_FRAME >0.AND.IANIM < output%NB_ANIM_FRAME).OR.output%NB_ANIM_FRAME ==0) THEN
                      IF (TT>TSTOP.AND.ILASTANIM==0
     .                  .AND.ABS((TT-OUTPUT%TANIM)/OUTPUT%DTANIM)<EM03) THEN
                        ILASTANIM=1
                        OUTPUT%TANIM=TT-EM10
                      END IF

                      IF (TT>TSTOP.AND.ILASTANIM==0.AND.
     .                    (NCYCLE-LASTANIMCYCLE)>=100) THEN
                        ILASTANIM=1
                        OUTPUT%TANIM=TT-EM10
                      END IF
                    ENDIF
                  END IF
c
                  IF (DTSTAT>ZERO) THEN
                    IF (TT>TSTOP.AND.ILASTANIM==0
     .                  .AND.ABS((TT-TSTAT)/DTSTAT)<EM03) THEN
                      ILASTANIM=1
                      TSTAT=TT-EM10
                    END IF
                    IF (TT>TSTOP.AND.ILASTANIM==0.AND.
     .                  (NCYCLE-LASTSTATCYCLE)>=100) THEN
                      ILASTANIM=1
                      TSTAT=TT-EM10
                    END IF
                  END IF
c
                  IF (DYNAIN_DATA%DTDYNAIN>ZERO) THEN
                    IF (TT>TSTOP.AND.ILASTDYNAIN==0
     .                  .AND.ABS((TT-DYNAIN_DATA%TDYNAIN)/DYNAIN_DATA%DTDYNAIN)<EM03) THEN
                      ILASTDYNAIN=1
                      DYNAIN_DATA%TDYNAIN=TT-EM10
                    END IF
                    IF (TT>TSTOP.AND.ILASTDYNAIN==0.AND.
     .                  (NCYCLE-LASTDYNCYCLE)>=100) THEN
                      ILASTDYNAIN=1
                      DYNAIN_DATA%TDYNAIN=TT-EM10
                    END IF
                  END IF
c
                  IF (H3D_DATA%DTH3D>ZERO) THEN
                    IF((H3D_DATA%NB_H3D_FRAME >0.AND.H3D_DATA%IH3D < H3D_DATA%NB_H3D_FRAME).OR.(H3D_DATA%NB_H3D_FRAME ==0)) THEN
                      IF (TT>TSTOP.AND.ILASTH3D==0
     .                    .AND.ABS((TT-H3D_DATA%TH3D)/H3D_DATA%DTH3D)<EM03) THEN
                        ILASTH3D=1
                        H3D_DATA%TH3D=TT-EM10
                      END IF
                      IF (TT>TSTOP.AND.ILASTH3D==0.AND.
     .                    (NCYCLE-LASTH3DCYCLE)>=100) THEN
                        ILASTH3D=1
                        H3D_DATA%TH3D=TT-EM10
                      END IF
                    ENDIF
                  END IF
                ENDIF
C------------
                IF (IMON>0) THEN
                  CALL STOPTIME(TIMERS,TIMER_IO)
                ENDIF
              ENDIF
              IF (IMPDEB==1.AND.IMCONV==0) THEN
                IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
                  IF ((IRAD2R==1).AND.(IRESP==1)) THEN
                    TT_DP=TT_DP-TTMP-DT2
                    TT=TT_DP
                  ELSE
                    ! double precision accumulation and then cast in simple precision
                    TT_DOUBLE = TT_DOUBLE - TTMP - DT2
                    IF (IMPL_S==1.OR.NEIG>0) THEN
                      TT = TT - TTMP - DT2
                    ELSE
                      IF(IRESP == 1)THEN
                        TT = SNGL(TT_DOUBLE)
                      ELSE
                        TT = TT_DOUBLE
                      ENDIF
                    ENDIF
                    !TT=TT-TTMP-DT2
                  ENDIF
                ENDIF
              ENDIF
C ---------------------------------
              CALL PYTHON_UPDATE_TIME(TT,DT2)

C----------------------------------
C       IMPLICIT SOLVER
C----------------------------------
              IF (IMPL_S==1) THEN
                CALL TRACE_IN(3,NCYCLE,ZERO)
                IF (IMON>0) CALL STARTIME(TIMERS,34)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                IF (IMP_CHK > 0) THEN
                  CALL IMP_CHKM(TIMERS, PYTHON,
     1     NODES%ICODE      ,NODES%ISKEW          ,ISKWN       ,IPART             ,IXTG       ,IXS              ,IXQ         ,
     2     ELEMENT%SHELL%IXC,IXT                  ,IXP         ,IXR               ,IXTG1      ,NODES%ITAB       ,NODES%ITABM1,
     3     NPC              ,IBCL                 ,IBFV        ,SENSORS%SENSOR_TAB,NNLINK     ,LNLINK      ,IPARG       ,IGRV,
     4     IPARI            ,INTERFACES%INTBUF_TAB,RWALL%NPRW        ,ICONX             ,NPBY       ,LPBY             ,LRIVET      ,
     5     NSTRF            ,LJOINT,NODES%ICODT ,NODES%ICODR,INTERFACES%PON%ISKY,ELEMENT%PON%ADSKY,ELEMENT%PON%IAD_CONLD,
     6     ILINK            ,LLINK                ,NODES%WEIGHT,ITASK             ,IBVEL      ,LBVEL            ,FBVEL,
     7     NODES%X          ,NODES%D              ,NODES%V     ,NODES%VR          ,NODES%DR   ,THKE     ,DAMP        ,NODES%MS,
     8     NODES%IN         ,PM                   ,SKEWS%SKEW  ,GEO               ,EANI       ,BUFMAT      ,BUFGEO      ,BUFSF,
     9     TF               ,FORC                 ,VEL         ,FSAV              ,AGRV       ,FR_WAVE,PARTS0   ,
     A     ELBUF            ,RBY                  ,RIVET       ,NODES%BOUNDARY           ,NODES%BOUNDARY_ADD   ,NSENSOR          ,
     B     WA               ,NODES%A              ,NODES%AR    ,NODES%STIFN       ,NODES%STIFR,PARTSAV,ELEMENT%PON%FSKY,
     C     INTERFACES%PON%FSKYI,IFRAME            ,XFRAME      ,W16               ,IACTIV     ,ELEMENT%PON%FSKYM,IGEO,IPM     ,
     D     OUTPUT%TH%WFEXT     ,NODFT                ,NODLT       ,NT_IMP            ,NUM_IMP    ,NS_IMP           ,NE_IMP,IND_IMP,
     L     IT               ,RWALL%RWBUF                ,RWALL%LPRW        ,RWALL%FR_WALL           ,NBINTC     ,INTLIST          ,
     M     OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)),RWALL%RWSAV  ,FSAVD       ,
     N     DIRUL            ,LGRAV                ,RBE3%IRBE3  ,RBE3%LRBE3        ,RBE3%FRBE3 ,
     O     FRWL6            ,IRBE2                ,LRBE2       ,ICFIELD           ,LCFIELD    ,CFIELD           ,ELBUF_TAB,
     P     NODES%WEIGHT_MD  ,STACK,SENSORS%SFSAV  ,SENSORS%FSAV,SENSORS%STABSEN   ,SENSORS%TABSENSOR            ,DRAPE_SH4N  ,
     Q     DRAPE_SH3N       ,H3D_DATA             ,NDDL0       ,NNZK0             ,IMPBUF_TAB ,CPTREAC,FTHREAC,NODREAC ,
     R     DRAPEG           ,OUTPUT%TH%TH_SURF    ,DPL0CLD     ,VEL0CLD           ,SNPC       ,STF , OUTPUT%TH%WFEXT_MD,IGRSURF)
                  MSTOP=2
                ELSEIF ((TT<=TSTOP.OR.(TT-TSTOP)<EM10).AND.IBUCK==0) THEN
C-----integer : 1:IKC,2:IKUD,3:W_DDL,4:IADM,5:JDIM,6:NDOFI,7:IDDLI
C-----reel : 1,2,3,4:DIAG_K,LT_K,DIAG_M,LT_M,5,6:LB,DB,7:BKUD,8,9:D_IMP,DR_IMP
C----       10,11,12:ELBUF_C,BUFMAT_C,X_C,13,14:DD,DDR,15,16:X_ac,V_zero,23,24:AC,ACR
#if defined(MUMPS5)
                  CALL IMP_SOLV(OUTPUT,TIMERS, PYTHON,
     1            NODES%ICODE  ,NODES%ISKEW  ,ISKWN  ,IPART  ,IXTG   ,IXS    ,IXQ    ,
     2            ELEMENT%SHELL%IXC    ,IXT    ,IXP    ,IXR    ,IXTG1          ,NODES%ITAB   ,NODES%ITABM1 ,
     3            NPC    ,IBCL   ,IBFV   ,SENSORS%SENSOR_TAB,NNLINK ,LNLINK ,IPARG  ,IGRV   ,
     4            IPARI  ,INTERFACES%INTBUF_TAB,RWALL%NPRW   ,ICONX  ,NPBY,LPBY   ,LRIVET ,
     5            NSTRF  ,LJOINT ,NODES%ICODT  ,NODES%ICODR  ,INTERFACES%PON%ISKY  ,ELEMENT%PON%ADSKY,ELEMENT%PON%IAD_CONLD,
     6            ILINK  ,LLINK  ,NODES%WEIGHT         ,ITASK ,IBVEL  ,LBVEL  ,FBVEL  ,
     7            NODES%X      ,NODES%D      ,NODES%V      ,NODES%VR     ,NODES%DR     ,THKE   ,DAMP   ,NODES%MS     ,
     8            NODES%IN     ,PM     ,SKEWS   ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     9            TF     ,FORC   ,VEL    ,FSAV   ,AGRV   ,FR_WAVE,PARTS0 ,
     A            ELBUF  ,RBY   ,RIVET,NODES%BOUNDARY,NODES%BOUNDARY_ADD,
     B            WA     ,NODES%A      ,NODES%AR     ,NODES%STIFN  ,NODES%STIFR  ,PARTSAV,ELEMENT%PON%FSKY   ,
     C            INTERFACES%PON%FSKYI  ,IFRAME ,XFRAME ,W16    ,IACTIV ,ELEMENT%PON%FSKYM  ,IGEO   ,IPM    ,
     D            OUTPUT%TH%WFEXT  ,NODFT  ,NODLT  ,NT_IMP ,NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,
     L            IT     ,RWALL%RWBUF  ,RWALL%LPRW   ,RWALL%FR_WALL,NBINTC ,INTLIST,
     M            OUTPUT%DATA%FOPT(1,1+(NSECT+NRBODY)) ,RWALL%RWSAV  ,FSAVD ,
     N            GRAPHE , FAC_K  ,IPIV_K ,NKCOND,NSENSOR,
     O            MONVOL ,IGRSURF,FR_MV  ,VOLMON ,DIRUL,
     P            NODES%NODGLOB,MUMPS_PAR,CDDLP ,ISENDTO,IRCVFROM,NEWFRONT,IMSCH  ,
     Q            I2MSCH ,ISIZXV,ILENXV ,ISLEN7  ,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     R            IRLEN17,IRLEN7T,ISLEN7T,NODES%KINET  ,NUM_IMP1,NODES%TEMP  ,DT2PREV,WA     ,
     S            LGRAV  ,SH4TREE,SH3TREE,IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,
     T            IRLEN20E,ISLEN20E,RBE3%IRBE3,RBE3%LRBE3  ,RBE3%FRBE3  ,FR_I2M,IAD_I2M,rbe3%mpi%FR_RBE3,
     U            RBE3%mpi%IAD_RBE3,FRWL6,IRBE2  ,LRBE2,INTBUF_TAB_CP,
     W            NODES%IKINE  ,DIAG_SMS,ICFIELD,LCFIELD,CFIELD,COUNT_REMSLV,
     X            COUNT_REMSLVE,ELBUF_TAB,ELBUF_IMP,NODES%XDP,NODES%WEIGHT_MD , STACK  ,
     Y            SENSORS%SFSAV,SENSORS%FSAV,SENSORS%STABSEN,SENSORS%TABSENSOR,DRAPE_SH4N  , DRAPE_SH3N,
     Z            H3D_DATA,MULTI_FVM,IGRBRIC,IGRSH4N,IGRSH3N,IGRBEAM,FORNEQS,MAXDGAP,
     A            NDDL0   ,NNZK0 ,IT_T   ,IMPBUF_TAB,CPTREAC,FTHREAC,NODREAC, DRAPEG,
     B            INTERFACES,OUTPUT%TH%TH_SURF,DPL0CLD,VEL0CLD,SNPC,STF,GLOB_THERM,OUTPUT%TH%WFEXT_MD)
#else
                  WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
                  CALL FLUSH(6)
                  CALL ARRET(5)
#endif
C Modes buckling
                  IF (NBUCK>0) IBUCK=1
                ELSEIF (IBUCK>0) THEN
C
#if defined(MUMPS5) && defined(DNC)
                  IF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                    call assign_ptrX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                  ELSE
                    call assign_ptrX(ptrX,NODES%X,NUMNOD)
                  ENDIF
                  CALL IMP_BUCK(
     2            PM,             GEO,             IPM,       IGEO,           ELBUF,
     3            IXS,            IXQ,             ELEMENT%SHELL%IXC,       IXT,            IXP,
     4            IXR,            IXTG,            IXTG1,     IPARG,
     5            TF,             NPC,             FR_WAVE,   W16,            BUFMAT,
     6            THKE,           BUFGEO,          NSENSOR,   SENSORS%SENSOR_TAB,RBY,
     7            SKEWS%SKEW,     WA,                NODES%ICODT,   NODES%ICODR,         NODES%ISKEW,
     9            IBFV,           VEL,             LPBY,      NPBY,         NODES%ITAB,
     A            NODES%WEIGHT,         NODES%MS,       NODES%IN,        IPARI,    INTERFACES%INTBUF_TAB,
     B            ptrX, ITSK ,
     E            OUTPUT%DATA%VECT_CONT,          ICUT,            XCUT,      OUTPUT%DATA%VECT_FINT, OUTPUT%DATA%VECT_FEXT,
     F            OUTPUT%DATA%FOPT, OUTPUT%DATA%SCAL_DT,            NSTRF,     RWALL%RWBUF,          RWALL%NPRW,
     G            TANI,           DD_IAD,          EANI,      IPART,
     H            NOM_OPT,        IGRSURF,         BUFSF,     IDATA,
     I            RDATA,          KXX,             IXX,       KXSP,           IXSP,
     J            NOD2SP,         SPBUF,           IXS(L1),   IXS(L2),        IXS(L3),
     K            NODES%VR,             MONVOL,          VOLMON,    NODES%NODGLOB,  NODES%BOUNDARY_ADD,
     L            NODES%BOUNDARY,        FR_SEC,          FR_RBY2,   IAD_RBY2,       RWALL%FR_WALL,
     M            NODES%V,              NODES%A,               GRAPHE,    PARTSAV ,       XFRAME  ,
     N            DIRUL,
     O            FSAV(1,NFNCA+1), FSAV(1,NFTCA+1),NODES%TEMP      ,SH4TREE,        SH3TREE,
     P            ERR_THK_SH4,    ERR_THK_SH3    , IFRAME    ,RWALL%LPRW      ,     ELBUF_TAB,
     Q            FSAV           ,FSAVD          , RWALL%RWSAV     ,NODES%AR        ,   RBE3%IRBE3   ,
     R            RBE3%LRBE3     ,RBE3%FRBE3     , FR_I2M    ,IAD_I2M   ,     RBE3%mpi%FR_RBE3,
     S            RBE3%mpi%IAD_RBE3,FRWL6          , IBCL      ,FORC      ,     IRBE2   ,
     T            LRBE2          ,IAD_RBE2       , FR_RBE2   ,NODES%WEIGHT_MD,
     U            CLUSTER        ,FCLUSTER       , MCLUSTER  ,XFEM_TAB  ,
     V            ALE_CONNECTIVITY    ,W              , ale%global%nv46 ,NERCVOIS  ,     NESDVOIS ,
     W            LERCVOIS       ,LESDVOIS       ,CRKEDGE    ,STACK     ,SENSORS%SFSAV   ,
     X            SENSORS%FSAV   ,SENSORS%STABSEN,SENSORS%TABSENSOR,INDX_CRK  ,XEDGE4N       ,
     Y            XEDGE3N        ,SPH2SOL        ,STIFN_TMP  ,STIFR_TMP ,     DRAPE_SH4N ,
     Z            DRAPE_SH3N     ,H3D_DATA       ,SUBSETS    ,IGRNOD    ,     FCONT_MAX,
     A            OUTPUT%DATA%VECT_PCONT2,OUTPUT%DATA%VECT_PCONT2_2,NDDL0    ,NNZK0     ,IMPBUF_TAB ,
     B            DRAPEG         ,MAT_ELEM%MAT_PARAM  ,GLOB_THERM, OUTPUT )
#else
                  WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
                  CALL FLUSH(6)
                  CALL ARRET(5)
#endif

                  IF (IDYNA==0.AND.ITSK==0) CALL CP_DM(NUMGEO,GEO,IGEO,DMCP,2)
                  MSTOP=2
                ELSE
                  IF (ILASTANIM/=1) THEN
                    IF (ILINE/=1.AND.ISPMD==0) THEN
                      WRITE(IOUT,*)
                      WRITE(ISTDO,*)
                      WRITE(IOUT,1002)IT_T
                      WRITE(ISTDO,1002)IT_T
                      WRITE(IOUT,1003)IT_BCS,IT_PCG
                      WRITE(ISTDO,1003)IT_BCS,IT_PCG
                    ENDIF
                  END IF !(ILASTANIM==2) THEN
                  TT = MIN(TT,TSTOP+EM10)
                  DT2 = DT2T
                ENDIF !IF (IMP_CHK>0)
                IF (TT>TSTOP.AND.INCONV==1) THEN
                  CALL IMP_RESTARCP(NODES%X,NODES%V,NODES%VR,GEO,IGEO,DMCP,IMPBUF_TAB)
                ENDIF !IF (TT>TSTOP.AND.INCONV==1)

                CALL TRACE_OUT(3)
                IF (IMON>0) CALL STOPTIME(TIMERS,34)
C---------------------------
C     Not pure thermal case
C---------------------------
              ELSEIF(ILAG+IALE+IEULER/=0)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

           CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL

C----------------------------
C    RBODIES TO RIGIDE SURFACE.
C--- //  ----------------
                IF (NSURF/=0) THEN

C========================================================================================
C                                   DOMAIN 0
C========================================================================================
                  IF(ISPMD==0) THEN    ! int14 processing on p0
                    CALL SRFVIT(NODES%X,NODES%V,NODES%VR,NODES%A,NODES%AR,
     .                  NPBY  ,RBY  ,NODES%MS  ,NODES%IN   ,
     .                  IGRSURF ,BUFSF)
                  END IF
                ENDIF

!$OMP END PARALLEL
            CALL PYTHON_END_OPENMP(PYTHON)
C----------------------------------------------------------------
C         CLOAD - Save Displacements and Velocities for concentrated loads and pressure loads
C----------------------------------------------------------------
                IF (NCONLD > 0) THEN
                  CALL DISP_VEL_SAVED_CLOAD(NODES%V      ,NODES%D      ,NODES%VR    ,NODES%DR    ,IBCL   ,
     .                                      DPL0CLD,VEL0CLD,NIBCLD,NCONLD,IRODDL ,
     .                                      NUMNOD )
                ENDIF
C----------------------------------------------------------------
C         USER WINDOWS - Save Accelerations before reset to zero
C----------------------------------------------------------------
                IF(USER_WINDOWS%HAS_USER_WINDOW /= 0)THEN
                  IF(ISPMD == 0) THEN
                    DO I=1,NUMNOD
                      USER_WINDOWS%A_SAV(1,I)=NODES%A(1,I)*NODES%MS(I)
                      USER_WINDOWS%A_SAV(2,I)=NODES%A(2,I)*NODES%MS(I)
                      USER_WINDOWS%A_SAV(3,I)=NODES%A(3,I)*NODES%MS(I)
                    ENDDO
                    IF(IRODDL/=0)THEN
                      DO I=1,NUMNOD
                        USER_WINDOWS%AR_SAV(1,I)=NODES%AR(1,I)*NODES%IN(I)
                        USER_WINDOWS%AR_SAV(2,I)=NODES%AR(2,I)*NODES%IN(I)
                        USER_WINDOWS%AR_SAV(3,I)=NODES%AR(3,I)*NODES%IN(I)
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
C------------------------------------------
C         SYNCHRONIZATION ON V TT AND NCYCLE (implicit)
C----------------------------

                IF(IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)
                ITSK = OMP_GET_THREAD_NUM()
                NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C-----------------
C         VELOCITIES
C-----------------
                CALL VELOCITY(
     1                       NODES%A      , NODES%AR     , NODES%V   , NODES%VR   , FZERO,
     2                        NODES%ITAB,ALE_CONNECTIVITY%NALE             )
c
                IF (NLOC_DMG%IMOD > 0) THEN
c
                  NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
                  NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
                  CALL NLOCAL_VEL(NLOC_DMG, NODFT_NL ,NODLT_NL)
                ENDIF
c
                IF(IALELAG > 0) THEN
                  CALL FLOW_VELOCITY(ALE_CONNECTIVITY%NALE,AFLOW   ,VFLOW   , FZERO  ,
     2                              NODFTSK,NODLTSK  ,WFLOW, NODES%V,IFOAM)
                ENDIF
C
                IF(NPINCH > 0) THEN
                  CALL VELOCITYPINCH(
     1                              PINCH_DATA%APINCH, PINCH_DATA%VPINCH,
     2                              NODFTSK          , NODLTSK          )
                ENDIF
C
CC
!$OMP END PARALLEL
            CALL PYTHON_END_OPENMP(PYTHON)

! inivel w/ Tstart or sensor
                IF (LOADS%NINIVELT > 0) THEN
                  IF (N2D == 0) THEN
                    LENGTH = NUMELS + NSVOIS
                  ELSE
                    LENGTH = NUMELQ + NUMELTG
                  ENDIF
                  CALL INIVEL_START(
     .                            NGRNOD,  NGRBRIC,    NGRQUAD,       NGRSH3N,
     .                            IGRNOD,  IGRBRIC,    IGRQUAD,       IGRSH3N,
     .                            NUMSKW,    LSKEW,    NUMFRAM,       SENSORS,
     .                            XFRAME,SKEWS%SKEW,         NODES%X,             NODES%V,
     .                                NODES%VR,   NUMNOD,      VFLOW,         WFLOW,
     .                                 W,MULTI_FVM,      IALE ,       IALELAG,
     .                                TT,   IRODDL, LOADS%NINIVELT,LOADS%INIVELT,
     .                             NPARG,   NGROUP,       LENGTH,         IPARG,
     .                         ELBUF_TAB,       NODES%MS,         NODES%IN,        NODES%WEIGHT,
     .                           NXFRAME,   T_KIN )
                  OUTPUT%TH%WFEXT = OUTPUT%TH%WFEXT + T_KIN
                END IF
!
C----------------------------------
C       ITET2 of S10 Kinematic
C----------------------------------
                IF (NS10E > 0) CALL S10CNDV(ICNDS10,VND   ,NODES%V   )

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C==========================================================================================

                IF(IPLYXFEM > 0) THEN
                  NTHOLD= NTHREAD
                  NTHREAD=1
                  call omp_set_num_threads(NTHREAD)
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c for smp parallel is not parit on
                  CALL PLY_VELOCITY(NODFTSK,NODLTSK,NPLYMAX,INOD_PXFEM,NUMNOD)
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
                  NTHREAD= NTHOLD
                  call omp_set_num_threads(NTHREAD)
                ENDIF
C
                IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)

                IF(IALE+IEULER==0) THEN
C----------------------
C          LAGRANGE PUR
C----------------------

                  IF(IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                  CALL DEPLA(NODES%V      ,NODES%D      ,NODES%X  ,NODES%VR   ,NODES%DR    ,
     2                       NODES%XDP,NODES%DDP,NUMNOD)
C
                  CALL DEPLAFAKEIGE(NODES%X ,NODES%V ,INTERFACES%INTBUF_TAB, KXIG3D,
     2                              IXIG3D,IGEO, KNOT, WIGE,
     3                              KNOTLOCPC,KNOTLOCEL)

c
                  IF (NLOC_DMG%IMOD > 0) THEN
c
                    NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
                    NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
                    CALL NLOCAL_INCR(NLOC_DMG, NODFT_NL,NODLT_NL)
                  ENDIF
c
                  IF(IALELAG > 0) THEN
                    CALL FLOW_DEPLA(ALE_CONNECTIVITY%NALE, VFLOW      ,DFLOW    ,
     2                            NODFTSK,NODLTSK)
                  ENDIF
C
                  IF(NPINCH > 0) THEN
                    CALL DEPLAPINCH(PINCH_DATA%VPINCH,  PINCH_DATA%DPINCH,
     2                              PINCH_DATA%XPINCH,  NODFTSK          , NODLTSK)
                  ENDIF
C
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

                  IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)
C
                ELSEIF(IALE/=0)THEN
C-------------------------------------
C          -A.L.E.-(+LAGRANGE)(+EULER)
C-------------------------------------

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                  CALL ALEWDX(TIMERS,
     1              GEO             ,NODES%X               ,NODES%D     ,NODES%V     ,NODES%VR   ,
     2              W               ,WA                    ,WB          ,SKEWS%SKEW  ,
     3              PM              ,XLAS                  ,NODES%MS    ,FSAV        ,
     4              NODES%A         ,TF                    ,RWALL%RWBUF       ,
     5              DT2SAVE         ,PYTHON                ,
     6              IPARG           ,IXS                   ,IXQ         ,NODPOR      ,
     7              NODES%ISKEW     ,NODES%ICODT           ,ELBUF_TAB   ,
     8              NPC             ,LINALE                ,RWALL%NPRW        ,LAS         ,
     9              IPARI           ,NODFTSK               ,NODLTSK     ,ITSK        ,
     A              NODES%BOUNDARY_ADD ,NODES%BOUNDARY               ,NBRCVOIS    ,NBSDVOIS    ,LNRCVOIS   ,
     B              LNSDVOIS        ,NODES%WEIGHT          ,ELEMENT%PON%ADSKY,ELEMENT%PON%FSKY,ELEMENT%PON%IADS,
     C              RWALL%FR_WALL         ,NPORGEO               ,ELEMENT%PON%PROCNE      ,
     D              FR_NBCC         ,ELEMENT%PON%IADQ      ,NODES%XDP   ,IGRNOD      ,
     E              NODES%DR        ,INTERFACES%INTBUF_TAB ,MULTI_FVM   ,
     F              ALE_CONNECTIVITY,NODES%DDP             ,NE_NERCVOIS ,NE_NESDVOIS ,
     G              NE_LERCVOIS     ,NE_LESDVOIS           ,XCELL       ,XFACE       , OUTPUT%TH%WFEXT)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

C       -EULER+LAGRANGE-
C No displacements computed if pure Euler
C-------------------------
                ELSEIF(ILAG==1)THEN
C // -----------------------------------
                  IF(IMON>0) CALL STARTIME(TIMERS,TIMER_INTEG)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

                  CALL EULDX(NODES%V,NODES%D,NODES%X,NODES%DDP,ALE_CONNECTIVITY%NALE,NODFTSK,NODLTSK)

!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

                  IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_INTEG)
                ENDIF
              ENDIF
              CALL PYTHON_SYNC(PYTHON%CONTEXT)
              CALL PYTHON_UPDATE_NODAL_ENTITIES(NUMNOD,NODES,X=NODES%X, D=NODES%D, DR=NODES%DR)
              IF(coupling%active) THEN
                DT2MAX_COUPLING = DT2 
                ! Read and write coupling positions
                CALL COUPLING_SYNC(coupling,DT2,NODES,COUPLING_POSITIONS)
              ENDIF

              IF (ViperCoupling) THEN
C                Send positions to Viper
                 CALL RadiossViper_SendXVE(NUMNOD,VIPER%NUMELEr,VIPER%NUMELEv,NPARG,NGROUP,viper%NUMON,
     .                 NODES%X,NODES%V,VIPER%ITABM1,VIPER%IXEM1,IPARG,ELBUF_TAB)
                 CALL RadiossViper_SendKill(MSTOP,TSTOP,VIPER%TSTOP)
              ENDIF

C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================
!-------------------------------------------------------------------
!       offset projection for contact ! add smp // after
!----------------------------------
              IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                XYZ(1:3,1:NUMNOD) = NODES%X(1:3,1:NUMNOD)
                CALL offset_nPROJ(NSPMD,NUMNOD,XYZ,SH_OFFSET_TAB,IPARIT)
              ENDIF

C-------------------------------------
C     UPDATE XFEM CONFIGURATION
C-------------------------------------
              IF (ICRACK3D > 0) THEN
                IF (NLEVSET > 0)THEN
c
                  CALL UPXFEM1(XFEM_TAB,
     .                 IPARG        ,ELEMENT%SHELL%IXC        ,NGROUC      ,IGROUC  ,IXTG    ,
     .                 IADC_CRK     ,IEL_CRK    ,INOD_CRK    ,ELCUTC  ,NODEDGE ,
     .                 ENRTAG       ,CRKEDGE    ,XEDGE4N     ,XEDGE3N )
C
                  IF (NSPMD > 1)       ! exchange ENRTAG
     .              CALL SPMD_CRK_ADV(NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INOD_CRK ,ENRTAG)

C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c         Use enrtag => positive set enrichments
                  CALL UPENR_CRK(ADSKY_CRK,INOD_CRK ,NODFTSK    ,NODLTSK       ,
     .                           NODENR   ,ENRTAG   ,NODLEVXF   ,PROCNE_CRK    )
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

c         set TAGXP after updating enrichments
                  CALL UPXFEM_TAGXP(XFEM_TAB,
     .                 IPARG        ,ELEMENT%SHELL%IXC        ,NGROUC      ,IGROUC  ,IXTG    ,
     .                 IADC_CRK     ,IEL_CRK    ,INOD_CRK    ,ELCUTC  ,NODEDGE ,
     .                 ENRTAG       ,CRKEDGE    ,XEDGE4N     ,XEDGE3N ,NODES%ITAB    )
                 CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c         update velocities of phantom elements
                  CALL CRK_VELOCITY(ADSKY_CRK,INOD_CRK ,NODLEVXF ,NODFTSK  ,NODLTSK  ,
     .                             NODES%X        ,NODES%V        ,NODES%VR       ,NODES%A        ,NODES%AR       ,
     .                             NODES%ITAB     )
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)

C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

                  CALL CRK_VELOCITY2(IPARG    ,NGROUC   ,IGROUC   ,ELCUTC   ,CRKEDGE  ,
     .                              NODEDGE  ,ELEMENT%SHELL%IXC      ,IXTG     ,XEDGE4N  ,XEDGE3N  ,
     .                              IADC_CRK ,IEL_CRK  ,INOD_CRK ,NODES%ITAB     )
c----------------------------------------------------------------------
c        spmd xfem velocity exchange
c----------------------------------------------------------------------
                  IF (NSPMD > 1) THEN
                    CALL SPMD_EXCH_CRKVEL(NODES%BOUNDARY_ADD ,NODES%BOUNDARY ,INOD_CRK ,NODES%ITAB   ,
     .                                    NODES%X        ,NODES%V       ,NODES%VR       )
                  ENDIF
C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================

                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                  CALL CRK_ZERO_ACCEL(ADSKY_CRK,INOD_CRK ,NODFTSK  ,NODLTSK  ,NODLEVXF )
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
c----------------------------------------------------------------------

C========================================================================================
C                                NON PARALLEL SECTION (SMP)
C========================================================================================

                  CALL UPXFEM2(IPARG      ,ELEMENT%SHELL%IXC    ,NGROUC  ,IGROUC  ,IADC_CRK    ,
     .                         IEL_CRK    ,ELCUTC ,IXTG    ,ENRTAG  ,INOD_CRK    ,
     .                         NODES%BOUNDARY_ADD   ,NODES%BOUNDARY,IAD_EDGE,FR_EDGE ,FR_NBEDGE   ,
     .                         CRKEDGE    )
                ELSE  ! NLEVSET = 0

C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================
                  CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
                  ITSK = OMP_GET_THREAD_NUM()
                  NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
                  NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
                  CALL CRK_COORD_INI(ADSKY_CRK,INOD_CRK ,NODFTSK,NODLTSK,NODES%X        ,
     .                               NODLEVXF )
!$OMP END PARALLEL
                  CALL PYTHON_END_OPENMP(PYTHON)
                END IF
C---
              END IF  ! XFEM

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF(NINTSTAMP/=0)THEN
                CALL INTSTAMP_MOVE(INTSTAMP ,NPC   ,TF   ,SKEWS%SKEW ,PTR_SMS,
     .                                NODES%V     ,NODES%VR    ,NODES%MS   ,NODES%X    ,NODES%D      ,
     .                             NPBY     ,RBY   )
              END IF
              ! --------------------------------
              ! velocity update for /INT18 + LAW151
              IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
                IF(NSPMD>1.AND. IPARIT/=0) THEN
                  CALL SPMD_INT18_LAW151_PON( IPARI,ISLEN7,IRLEN7,2,INTERFACES%INTBUF_TAB,
     1                                       MULTI_FVM )
                ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
                CALL PYTHON_BEGIN_OPENMP(PYTHON) 
!$OMP PARALLEL PRIVATE(ITSK)
                ITSK = OMP_GET_THREAD_NUM()
                CALL INT18_LAW151_UPDATE(ITSK   ,MULTI_FVM,IGRBRIC  ,IPARI,IXS,
     1                                   IGROUPS,IPARG    ,ELBUF_TAB,MULTI_FVM%FORCE_INT         ,
     2                   NODES%X,          NODES%V,          NODES%MS,          NODES%KINET       ,
     3          MULTI_FVM%X_APPEND,MULTI_FVM%V_APPEND,MULTI_FVM%MASS_APPEND,MULTI_FVM%KINET_APPEND)
!$OMP END PARALLEL
                CALL PYTHON_END_OPENMP(PYTHON)

              ENDIF
              CALL PYTHON_BEGIN_OPENMP(PYTHON)
!$OMP PARALLEL PRIVATE(ITSK)
              ITSK = OMP_GET_THREAD_NUM()
              CALL MULTI_VELOCITY_BACKUP(ITSK,MULTI_FVM,N2D,NUMELS,NUMELQ,NUMELTG)
!$OMP END PARALLEL
              CALL PYTHON_END_OPENMP(PYTHON)
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

              IF( MULTI_FVM%IS_USED ) THEN
!       write *.adb files for FVM solver option
                IF (DEBUG(MACRO_DEBUG_ACC)==1) THEN
                  IF (ISPMD==0) THEN
                    SIZ = NUMELSG
                  ELSE
                    SIZ = 0
                  END IF
                  IF ( NCYCLE>=DEBSTART .AND. MOD(NCYCLE-DEBSTART,RSTFREQ)==0 ) THEN
                    CALL SPMD_COLLECT_MULTI_FVM(IXS,MULTI_FVM,1)
                  ENDIF
                ENDIF
                IF(DEBUG(MACRO_DEBUG_CHKSM) >0) THEN
                  IF(MOD(NCYCLE,DEBUG(MACRO_DEBUG_CHKSM)) == 0  ) THEN
                    CALL SPMD_COLLECT_MULTI_FVM(IXS,MULTI_FVM,2)
                  ENDIF
                ENDIF
              ENDIF
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C---------Computation of node areas for contact area output-----
              IF(INTERFACES%PARAMETERS%INTCAREA > 0) THEN
C  IOUTPRT for assembly synthesis, not need for spring which call *bilan each cycle
                ITHOUT = 0
                CALL TH_TIME_OUTPUT(ITHOUT, SENSORS,OUTPUT)
                IF(ITHOUT > 0) THEN
                  CALL INTER_NODAL_AREAS(IXS     ,ELEMENT%SHELL%IXC     ,IXTG      ,FASOLFR   ,NODES%X          ,
     .                                   NODES%BOUNDARY_ADD,NODES%BOUNDARY ,NODES%WEIGHT    ,IXQ       ,SEGQUADFR  ,
     .                                   IXS(L1) ,INTERFACES%PARAMETERS%INTAREAN)
                ENDIF

              ENDIF


              IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
                CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTERFACES%INTBUF_TAB,
     .                 NODES%X,NODES%V,NODES%MS,NODES%ITAB,XYZ,NUMNOD,SH_OFFSET_TAB%nnsh_oset)
              ENDIF

              IF (NSPMD>1) THEN
                IF(IMONM == 2)THEN
                  CALL STARTIME(TIMERS,60)
                  CALL SPMD_BARRIER()
                  CALL STOPTIME(TIMERS,60)
                END IF
                IF(IMON>0) CALL STARTIME(TIMERS,13)
                IF(ISIZXV>0) CALL SPMD_SD_XV(OUTPUT,
     1             NODES%X     ,NODES%D       ,NODES%V      ,NODES%VR    ,NODES%MS   ,
     2             NODES%IN    ,NODES%BOUNDARY_ADD,NODES%BOUNDARY,NODES%WEIGHT,IMSCH,
     3             W     ,ISIZXV ,ILENXV  ,NODES%XDP)
                IF (IMONM > 0) CALL STARTIME(TIMERS,23)
                L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                L2 = L1+6*NUMELS10
                L3 = L2+12*NUMELS20

                IF (SH_OFFSET_TAB%NNSH_OSET > 0) THEN
                  CALL ASSIGN_PTRX(ptrX,XYZ,NUMNOD)
                ELSEIF (IMPL_S > 0 .AND. ISMDISP >0) THEN
                  CALL ASSIGN_PTRX(ptrX,IMPBUF_TAB%X_A,NUMNOD)
                ELSE
                  CALL ASSIGN_PTRX(ptrX,NODES%X,NUMNOD)
                ENDIF

                CALL SPMD_I7XVCOM2(
     1             IPARI   ,ptrX    ,NODES%V ,NODES%MS      ,
     2             IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3             ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4             IRLEN17 ,IXS    ,IXS(L3) ,NSENSOR ,
     5             IGRBRIC ,NODES%TEMP   ,1       ,IRLEN7T ,ISLEN7T ,
     6             IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7             ISLEN20E,NODES%IKINE  ,DIAG_SMS,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB ,INT24E2EUSE,
     8             FORNEQS ,MULTI_FVM,INTERFACES,SH_OFFSET_TAB%NNSH_OSET)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,23)
                IF (IMON>0) CALL STOPTIME(TIMERS,13)
              ENDIF

              IF (INT24USE == 1)THEN
                IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
                CALL SPMD_EXCH_I24(IPARI,    INTERFACES%INTBUF_TAB       ,NODES%ITAB  ,
     *                             NODES%BOUNDARY_ADD, NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                             IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,3,
     *                             INT24E2EUSE)
                IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
              ENDIF

              IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
                CALL SPMD_I21TEMPCOM(IPARI,NODES%TEMP,INTERFACES%INTBUF_TAB,NSENSOR,SENSORS%SENSOR_TAB)
              ENDIF

              IF(NUMFRAM/=0.AND.N2D==0)THEN
C----------------------------
C      MOVING FRAME: RETRIEVE POSITION & VELOCITY.
C--- //0 ----------------
                IF (IMON>0)CALL STARTIME(TIMERS,6)
                IF (IMONM > 0) CALL STARTIME(TIMERS,49)
                IF (IMPL_S >0) THEN
                  CALL MOVFRA_IMP(XFRAME ,IFRAME ,NODES%X      ,NODES%V      ,NODES%A      ,
     .                            NODES%VR     ,NODES%AR     ,NODES%D      )
                ELSE
                  CALL MOVFRA2(XFRAME ,IFRAME ,NODES%X      ,NODES%V      ,NODES%VR     ,
     .                         NODES%D      )
                END IF !(IMPL_S >0)
                IF (IMONM > 0) CALL STOPTIME(TIMERS,49)
                IF (IMON>0) CALL STOPTIME(TIMERS,6)
              ENDIF

              IF (GLOB_THERM%IDT_THERM == 1.AND.(TSTOP-TT)<=EM20)THEN
                MSTOP_DT_THERM = 1
              ENDIF

#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
 300          CONTINUE
#endif

              ! ---------------------------------------------
              !     END OF RUN
              ! ---------------------------------------------

              ! --------------
              ! Restart Files
              ! --------------

              ! Restart File criteria
              STOP_OR_ADD_CYCLE = 0
              BOOL_RESTART=.FALSE.

              ! End of computation
              IF (((TT>TSTOP).OR.(MSTOP_DT_THERM==1)).AND.IMCONV==1) THEN
                STOP_OR_ADD_CYCLE = 1   ! check if additional cycle is need with animation
                IF (IRAD2R==0) THEN     ! No Restart writing at end of Run when Rad2rad is active
                  BOOL_RESTART = ((ILASTANIM==0.OR.ILASTANIM==1.OR.ILASTANIM==3).AND. RESTART_FILE==1) ! Restart writing criteria
                ENDIF
              ENDIF

              ! Restart within Run
              IF (ALE%SUB%IFSUB==0.AND.IMCONV==1 .AND. RESTART_FILE==1.AND.
     .            (NCYCLE/NCRST)*NCRST==NCYCLE.AND. IRAD2R==0 .OR.MREST==1.OR.(WMCHECK==1.AND.NCYCLE/=1) ) THEN
                STOP_OR_ADD_CYCLE = 0
                BOOL_RESTART = .TRUE.
              ENDIF

              IF(WMCHECK==1)THEN        ! Checkpoint Restart : write CHECK_DATA File
                IF(ISPMD==0)THEN
                  FILNAM = 'CHECK_DATA'
                  OPEN(UNIT=ICHECKD,FILE=FILNAM,ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')
                  WRITE(ICHECKD, FMT='(3A, I1)') '/RERUN/',ROOTNAM(1:ROOTLEN),'/',IRUN
                  CLOSE(ICHECKD)
                ENDIF
              ENDIF

              IF (BOOL_RESTART) THEN
                IF(IMON>0) CALL STARTIME(TIMERS,TIMER_IO)

                IF (GLOB_THERM%IDT_THERM == 1)CALL BCSDTTH_COPY(NODES%ICODT, NODES%ICODR, ICODT0, ICODR0, 2)

                CALL BCSN(NODES%ICODE,NODES%ICODT,NODES%ICODR,PARTS0,PARTSAV)

                IF (INT24USE == 1)THEN
                  ! E2E Update Fictive Node Position, Velocity, Mass
                  ! To do before SPMD_I7XVCOM2
                  CALL I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTERFACES%INTBUF_TAB,
     .                                          NODES%X,NODES%V,NODES%MS,NODES%ITAB,
     .                                              XYZ,NUMNOD,SH_OFFSET_TAB%nnsh_oset)
                ENDIF

                ! Interface communication : Send updates to remote nodes - finalizatoin
                ! Need here for coherent Restart writing
                IF(NSPMD>1)THEN
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  CALL SPMD_I7XVCOM2(
     1                    IPARI   ,NODES%X       ,NODES%V       ,NODES%MS      ,
     2                    IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3                    ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4                    IRLEN17 ,IXS    ,IXS(L3) ,NSENSOR ,
     5                    IGRBRIC ,NODES%TEMP   ,2       ,IRLEN7T ,ISLEN7T ,
     6                    IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7                    ISLEN20E,NODES%IKINE  ,DIAG_SMS,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB ,INT24E2EUSE,
     8                    FORNEQS ,MULTI_FVM,INTERFACES,SH_OFFSET_TAB%NNSH_OSET)
                END IF

                ! Finalize T24 Communication to fill Buffers before restarts
                IF (INT24USE == 1)THEN
                  IF (IMON>0) CALL STARTIME(TIMERS,TIMER_CONTFOR)
                  CALL SPMD_EXCH_I24(IPARI,    INTERFACES%INTBUF_TAB ,NODES%ITAB  ,
     *                               NODES%BOUNDARY_ADD, NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                               IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,3,
     *                               INT24E2EUSE)

                  CALL SPMD_EXCH_I24(IPARI,    INTERFACES%INTBUF_TAB ,NODES%ITAB  ,
     *                               NODES%BOUNDARY_ADD, NODES%BOUNDARY ,INTLIST ,NBINTC,
     *                               IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,4,
     *                               INT24E2EUSE)

                  IF (IMON>0) CALL STOPTIME(TIMERS,TIMER_CONTFOR)
                ENDIF

                ! INTERFACE 21 - Communication of nodal temperature
                IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
                  CALL SPMD_I21TEMPCOM(IPARI,NODES%TEMP,INTERFACES%INTBUF_TAB,NSENSOR,SENSORS%SENSOR_TAB)
                ENDIF
                ICH = 0
                ! Engine time for restart
                CALL ELAPSTIME(TIMERS,SECS)
                GLOBAL_COMP_TIME%ENGINE_TIME(GLOBAL_COMP_TIME%RUN_NBR) = SECS

                CALL WRRESTP(ELEMENT, NODES, AF         ,IAF      ,ICH             ,ELEMENT%PON%ADSKY            ,
     .                       ELBUF_TAB  ,XFEM_TAB        ,INTERFACES%INTBUF_TAB     ,MULTI_FVM       ,MAT_ELEM         ,
     .                       H3D_DATA   ,INTERFACES%INTBUF_FRIC_TAB ,SUBSETS        ,PINCH_DATA      ,ALE_CONNECTIVITY ,
     .                       T_MONVOL   ,SENSORS         , EBCS_TAB      ,DYNAIN_DATA     ,USER_WINDOWS     ,
     .                       OUTPUT     ,INTERFACES      ,LOADS          ,PYTHON          ,NAMES_AND_TITLES ,
     .                       EIGIPM     ,EIGIBUF         ,EIGRPM         ,NEIPM           ,LEIBUF           ,
     .                       NERPM      ,IFLOW           ,RFLOW          ,LIFLOW          ,LRFLOW           ,
     .                       IMPBUF_TAB ,IMPL_S          ,IMPL_S0        ,NODES%MCP             ,NODES%TEMP             ,
     .                       FORNEQS    ,UNITAB          ,STACK          ,NDRAPE          ,DRAPE_SH3N       ,
     .                       DRAPE_SH4N ,DRAPEG          ,RESTSIZE       ,SKEWS           ,GLOB_THERM       ,
     .                       PBLAST     ,RBE3            ,RWALL )

                ! Restart file size
                IF (MULTIREST >0)THEN
                  IF (RESTSIZE > MULTIRESTS(IRPREV))MULTIRESTS(IRPREV)=RESTSIZE
                ELSE
                  IF (RESTSIZE > RESTARTFILESIZE) RESTARTFILESIZE=RESTSIZE
                ENDIF

                IF((IDDW/=0).AND.(MSTOP/=0.OR.TT+DT2>=TSTOP)) THEN
                  CALL CUMULTIME_MP(
     1                  TAILLE,IPARG,
     2                  ELEMENT%SHELL%IXC,IXQ,IXT,IXP,IXTG,
     3                  IXR,IXS,KXIG3D,IPM,
     4                  IGEO,GEO,POIN_UMP,CPUTIME_MP,
     5                  NBR_GPMP,CPUTIME_MP_GLOB,TAB_UMP,PM,
     6                  BUFMAT,TABMP_L ,TAB_MAT )
                  IF(IDDWSTAT/=0) THEN
                    CALL PRINTIMEG(IPARG,PM,IPM,ELEMENT%SHELL%IXC,IXTG,IXS)
                  ENDIF
                ENDIF

                IF(IMON>0) CALL STOPTIME(TIMERS,TIMER_IO)
              END IF

              IF (STOP_OR_ADD_CYCLE==1)   THEN

                ! End of Run / criteria if additional cycle is need to write Anim or H3D state
                !
                ! ILASTANIM=0 No additional animation
                ! ILASTANIM=1 One more cycle needed
                ! ILASTANIM=2 Additional cycle done
                ! ILASTANIM=3 regular animation

                IF (ILASTANIM==2.OR.IRAD2R==1)THEN ! We are done / No magic - No additional Restart with Rad2rad run
                  MSTOP=2
                  GOTO 500
                ENDIF

                STATE_ANIM = 0
                IF(MSTOP_DT_THERM==0) THEN
                  IF (ILASTANIM==1) THEN
                    STATE_ANIM = 1
                  ELSEIF (ILASTANIM==0.OR.ILASTANIM==3) THEN
                    STATE_ANIM = 2
                  END IF
                ELSE
                  STATE_ANIM = 2
                ENDIF

                STATE_H3D = 0
                IF(MSTOP_DT_THERM==0) THEN
                  IF (ILASTH3D==1) THEN
                    STATE_H3D = 1
                  ELSEIF (ILASTH3D==0.OR.ILASTH3D==3) THEN
                    STATE_H3D = 2
                  END IF
                ELSE
                  STATE_H3D = 2
                ENDIF
                ! Warning: an extra loop will cause hanging if coupled to Viper
                IF( (STATE_ANIM == 1 .OR. STATE_H3D == 1) .and. .not.ViperCoupling ) THEN
                  CALL TRACE_OUT(3)
                  GOTO 100
                ELSEIF( STATE_ANIM == 2 .OR. STATE_H3D == 2 ) THEN
                  MSTOP=2
                  GOTO 500
                ENDIF
              ENDIF

              ! ------------------
              ! Negative Timestep
              ! ------------------
              IF(DT2<=ZERO) THEN
                WRITE(IOUT,*)' **ERROR : TIME STEP LESS OR EQUAL ZERO'
                WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL ZERO'

                IF ( ISTAMPING == 1) THEN
                  WRITE(ISTDO,'(A)')' The run has gone to divergence.'
                  WRITE(ISTDO,'(A)')' It could be due to a wrong definition of the interfaces between the tools and the blank.'
                  WRITE(ISTDO,'(A)')' You may need to check if there is enough clearance between the tools,'
                  WRITE(ISTDO,'(A)')' and that they do not penetrate each other during their travel.'
                  WRITE(IOUT, '(A)')' The run has gone to divergence.'
                  WRITE(IOUT, '(A)')' It could be due to a wrong definition of the interfaces between the tools and the blank.'
                  WRITE(IOUT, '(A)')' You may need to check if there is enough clearance between the tools,'
                  WRITE(IOUT, '(A)')' and that they do not penetrate each other during their travel.'
                ENDIF

                ! Finalize MPI communication for clean exit when dt < 0
                IF (INT24USE == 1) THEN
                  ! E2E Update Fictive Node Position, Velocity, Mass
                  ! To do before SPMD_I7XVCOM2
                  CALL I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTERFACES%INTBUF_TAB,
     .             NODES%X,NODES%V,NODES%MS,NODES%ITAB,
     1             XYZ,NUMNOD,SH_OFFSET_TAB%nnsh_oset)
                ENDIF
                ! Finalize Interface communication from Node to remote node for coherent restart
                IF(NSPMD>1)THEN
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  CALL SPMD_I7XVCOM2(
     1                IPARI   ,NODES%X       ,NODES%V       ,NODES%MS      ,
     2                IMSCH   ,I2MSCH  ,DT2PREV ,INTLIST ,NBINTC  ,
     3                ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4                IRLEN17 ,IXS     ,IXS(L3) ,NSENSOR ,
     5                IGRBRIC ,NODES%TEMP    ,2       ,IRLEN7T ,ISLEN7T ,
     6                IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E,
     7                ISLEN20E,NODES%IKINE   ,DIAG_SMS,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB ,INT24E2EUSE ,
     8                FORNEQS ,MULTI_FVM,INTERFACES,SH_OFFSET_TAB%NNSH_OSET)
                END IF

                IF(coupling%active) THEN
                  CALL COUPLING_ONGOING(coupling, ONGOING)
                    ! FAKE TIME STEPS TO FINALIZE COUPLING
                    DO WHILE (ONGOING)
                      DT2 = HUGE(DT2)
                      CALL COUPLING_ADVANCE(coupling,DT2)
                      CALL COUPLING_ONGOING(coupling, ONGOING)
                    END DO
                    CALL COUPLING_FINALIZE(coupling)
                ENDIF

                IF(ALLOCATED(ISENDTO))DEALLOCATE(ISENDTO)
                IF(ALLOCATED(IRCVFROM))DEALLOCATE(IRCVFROM)
                IF(ALLOCATED(INTLIST))DEALLOCATE(INTLIST)
                IF(ALLOCATED(INTLIST25))DEALLOCATE(INTLIST25)
                IF(ALLOCATED(NISKYFI))DEALLOCATE(NISKYFI)
                IF(ALLOCATED(NISKYFIE))DEALLOCATE(NISKYFIE)
                IF(ALLOCATED(FR_NBCC))DEALLOCATE(FR_NBCC)
                IF(ALLOCATED(FR_NBCCI2))DEALLOCATE(FR_NBCCI2)
                IF(ALLOCATED(DRETRI))DEALLOCATE(DRETRI)
                IF(ALLOCATED(XSEC))DEALLOCATE(XSEC)
                IF(ALLOCATED(IRBKIN_L))DEALLOCATE(IRBKIN_L)
                IF(ALLOCATED(ICODT0))DEALLOCATE (ICODT0)
                IF(ALLOCATED(ICODR0))DEALLOCATE(ICODR0)
                IF(ALLOCATED(ELEMENT%PON%ISENDP))DEALLOCATE(ELEMENT%PON%ISENDP)
                IF(ALLOCATED(ELEMENT%PON%IRECVP))DEALLOCATE(ELEMENT%PON%IRECVP)
                IF(ALLOCATED(IRECVP_PXFEM))DEALLOCATE(IRECVP_PXFEM)
                IF(ALLOCATED(ISENDP_CRK))DEALLOCATE(ISENDP_CRK)
                IF(ALLOCATED(IRECVP_CRK))DEALLOCATE(IRECVP_CRK)
                IF(ALLOCATED(CNEL))DEALLOCATE(CNEL)
                IF(ALLOCATED(ADDCNEL))DEALLOCATE(ADDCNEL)
                IF(ALLOCATED(ADDTMPL))DEALLOCATE(ADDTMPL)
                IF(ALLOCATED(TAGEL))DEALLOCATE(TAGEL)
                IF(ALLOCATED(IBUFIDEL))DEALLOCATE(IBUFIDEL)
                IF(ALLOCATED(INDIDEL))DEALLOCATE(INDIDEL)
                IF(ALLOCATED(IPARTL))DEALLOCATE(IPARTL)
                IF(ALLOCATED(EMINX))DEALLOCATE(EMINX)
                IF(NADMESH/=0)THEN
                  DEALLOCATE(LSH4ACT,LSH4KIN,PSH4ACT,PSH4KIN,
     .                       LSH3ACT,LSH3KIN,PSH3ACT,PSH3KIN,
     .                       MSH4SKY,MSH3SKY)
                  IF(IDEL7NG>=1) DEALLOCATE(TAGTRIMC,TAGTRIMTG)
                END IF
                IF(ALLOCATED(STIFN_TMP))DEALLOCATE(STIFN_TMP)
                IF(ALLOCATED(STIFR_TMP))DEALLOCATE(STIFR_TMP)
                IF(ALLOCATED(ACND))DEALLOCATE(ACND)
                IF(ALLOCATED(ARCND))DEALLOCATE(ARCND)
                IF(ALLOCATED(STCND))DEALLOCATE(STCND)
                IF(ALLOCATED(STRCND))DEALLOCATE(STRCND)
                IF(ALLOCATED(LSH4UPL))DEALLOCATE(LSH4UPL)
                IF(ALLOCATED(LSH3UPL))DEALLOCATE(LSH3UPL)
                IF(ALLOCATED(PSH4UPL))DEALLOCATE(PSH4UPL)
                IF(ALLOCATED(PSH3UPL))DEALLOCATE(PSH3UPL)
                IF(ALLOCATED(FTHREAC))DEALLOCATE(FTHREAC)
                IF(ALLOCATED(FTHDTM))DEALLOCATE(FTHDTM)
                IF(ALLOCATED(FREAC))DEALLOCATE(FREAC)
                IF(ALLOCATED(NODREAC))DEALLOCATE(NODREAC)
                IF(ALLOCATED(GRTH))DEALLOCATE(GRTH)
                IF(ALLOCATED(IGRTH))DEALLOCATE(IGRTH)
                IF(ALLOCATED(IGROUPC))DEALLOCATE(IGROUPC)
                IF(ALLOCATED(IGROUPTG))DEALLOCATE(IGROUPTG)
                IF(ALLOCATED(IGROUPS))DEALLOCATE(IGROUPS)
                IF(ALLOCATED(GRESAV))DEALLOCATE(GRESAV)
                IF(ALLOCATED(SFEM_NODVAR))DEALLOCATE(SFEM_NODVAR)
                IF(ALLOCATED(SFEM_NODVAR_ALE))DEALLOCATE(SFEM_NODVAR_ALE)
                IF(ALLOCATED(PARTSAV2))DEALLOCATE(PARTSAV2)
                IF(IMON>0) THEN
                  IF(NVOLU > 0)  CALL FVSTATS(MONVOL)
                  CALL STOPTIME(TIMERS,TIMER_RESOL)
                  IF(IMONM>0) CALL PRINTIME_INTERF(INTERFACES%INTBUF_TAB,IPARI,INTLIST,NBINTC,TIMERS%REALTIME(1)*0.01D0)
                  CALL PRINTIME(TIMERS,GLOB_THERM%ITHERM,OUTPUT)
                ELSE
                  CALL ADD_ELAPSED_TIME_MON_OFF(TIMERS)
                ENDIF
                CALL TRACE_OUT(3)
                RETURN
              ENDIF
C-------------------------------
 500          CONTINUE
C-------------------------------
              IF(MSTOP/=0) THEN
                PARALLEL_SECTION = 0
C Multidomain -> close sockets
                IF (IRAD2R==1) THEN
                  DO ITSK=1,NTHREAD
                    CALL CLOSE_SOCK_C(SOCKET(ITSK))
                  END DO
                ENDIF
C /KILL
                IF((NCYCLE/NCRST)*NCRST/=NCYCLE.AND.
     2             MREST/=1.AND.WMCHECK/=1.AND.NSPMD>1)THEN
                  L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
                  L2 = L1+6*NUMELS10
                  L3 = L2+12*NUMELS20
                  CALL SPMD_I7XVCOM2(
     1               IPARI   ,NODES%X       ,NODES%V       ,NODES%MS      ,
     2               IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3               ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4               IRLEN17 ,IXS    ,IXS(L3) ,NSENSOR ,
     5               IGRBRIC ,NODES%TEMP   ,2       ,IRLEN7T ,ISLEN7T ,
     6               IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7               ISLEN20E,NODES%IKINE  ,DIAG_SMS,SENSORS%SENSOR_TAB,INTERFACES%INTBUF_TAB,INT24E2EUSE,
     8               FORNEQS ,MULTI_FVM,INTERFACES,SH_OFFSET_TAB%NNSH_OSET)
                ENDIF

                IF(ALLOCATED(ISENDTO))DEALLOCATE(ISENDTO)
                IF(ALLOCATED(IRCVFROM))DEALLOCATE(IRCVFROM)
                IF(ALLOCATED(INTLIST25))DEALLOCATE(INTLIST25)
                IF(ALLOCATED(NISKYFI))DEALLOCATE(NISKYFI)
                IF(ALLOCATED(NISKYFIE))DEALLOCATE(NISKYFIE)
                IF(ALLOCATED(FR_NBCC))DEALLOCATE(FR_NBCC)
                IF(ALLOCATED(FR_NBCCI2))DEALLOCATE(FR_NBCCI2)
                IF(ALLOCATED(DRETRI))DEALLOCATE(DRETRI)
                IF(ALLOCATED(XSEC))DEALLOCATE(XSEC)
                IF(ALLOCATED(IRBKIN_L))DEALLOCATE(IRBKIN_L)
                IF(ALLOCATED(ISENDP))DEALLOCATE(ISENDP)
                IF(ALLOCATED(ELEMENT%PON%ISENDP))DEALLOCATE(ELEMENT%PON%ISENDP)
                IF(ALLOCATED(ELEMENT%PON%IRECVP))DEALLOCATE(ELEMENT%PON%IRECVP)
                IF(ALLOCATED(IRECVP_PXFEM))DEALLOCATE(IRECVP_PXFEM)
                IF(ALLOCATED(ISENDP_CRK))DEALLOCATE(ISENDP_CRK)
                IF(ALLOCATED(IRECVP_CRK))DEALLOCATE(IRECVP_CRK)
                !IF(IDEL7NG>0)THEN
                IF(ALLOCATED(CNEL))DEALLOCATE(CNEL)
                IF(ALLOCATED(ADDCNEL))DEALLOCATE(ADDCNEL)
                IF(ALLOCATED(ADDTMPL))DEALLOCATE(ADDTMPL)
                IF(ALLOCATED(TAGEL))DEALLOCATE(TAGEL)
                IF(ALLOCATED(IBUFIDEL))DEALLOCATE(IBUFIDEL)
                IF(ALLOCATED(INDIDEL))DEALLOCATE(INDIDEL)
                !ENDIF
                IF(ALLOCATED(IPARTL))DEALLOCATE(IPARTL)
                IF(ALLOCATED(EMINX))DEALLOCATE(EMINX)
                IF(NADMESH/=0)THEN
                  DEALLOCATE(LSH4ACT,LSH4KIN,PSH4ACT,PSH4KIN,
     .                       LSH3ACT,LSH3KIN,PSH3ACT,PSH3KIN,
     .                       MSH4SKY,MSH3SKY)
                  IF(IDEL7NG>=1) DEALLOCATE(TAGTRIMC,TAGTRIMTG)
                END IF
C
                !interface22
                IF(ALLOCATED(BRICK_LIST))       DEALLOCATE (BRICK_LIST)
                IF(ALLOCATED(LIST_B_OLD))       DEALLOCATE (LIST_B_OLD)
                IF(ALLOCATED(NBOLD))            DEALLOCATE (NBOLD)
                IF(ALLOCATED(EDGE_LIST))        DEALLOCATE (EDGE_LIST)
                IF(ALLOCATED(UVARL))            DEALLOCATE (UVARL)
                IF(ALLOCATED(SuperCellVOL_L))   DEALLOCATE (SuperCellVOL_L)
                IF(ALLOCATED(EINT_L))           DEALLOCATE (EINT_L)
                IF(ALLOCATED(RHO_L))            DEALLOCATE (RHO_L)
                IF(ALLOCATED(MOM_L))            DEALLOCATE (MOM_L)
                IF(ALLOCATED(SIG_L))            DEALLOCATE (SIG_L)
                IF(ALLOCATED(VOLD_L))           DEALLOCATE (VOLD_L)
                IF(ALLOCATED(IMERGEL))          DEALLOCATE (IMERGEL)
                IF(ALLOCATED(OLD_SecndList))    DEALLOCATE (OLD_SecndList)
                IF(ALLOCATED(UNLINKED_CELLS_L)) DEALLOCATE (UNLINKED_CELLS_L)
                IF(ALLOCATED(N_UNLINKED_L))     DEALLOCATE (N_UNLINKED_L)
                IF(ALLOCATED(V22MAX_L))         DEALLOCATE (V22MAX_L)
                IF(ALLOCATED(DX22MIN_L))        DEALLOCATE (DX22MIN_L)
                IF(ALLOCATED(INT22_FCELL_ANIM)) DEALLOCATE (INT22_FCELL_ANIM)

                !ALEFVM
                IF(ALLOCATED(ALEFVM_Buffer%WFEXT_CELL)) DEALLOCATE (ALEFVM_Buffer%WFEXT_CELL)
                IF(ALLOCATED(ALEFVM_Buffer%FEXT_CELL))  DEALLOCATE (ALEFVM_Buffer%FEXT_CELL)
                IF(ALLOCATED(ALEFVM_Buffer%FCELL))      DEALLOCATE (ALEFVM_Buffer%FCELL)
                IF(ALLOCATED(ALEFVM_Buffer%FINT_CELL))  DEALLOCATE (ALEFVM_Buffer%FINT_CELL)
                IF(ALLOCATED(ALEFVM_Buffer%VERTEX))     DEALLOCATE (ALEFVM_Buffer%VERTEX)
                IF(ALLOCATED(ALEFVM_Buffer%F_FACE))     DEALLOCATE (ALEFVM_Buffer%F_FACE)

                !/LOAD/PBLAST
                CALL PBLAST_DEALLOCATE(PBLAST)

C--     Rad2rad deallocation
                IF(ALLOCATED(IADD_NL))  DEALLOCATE (IADD_NL)
                IF(ALLOCATED(NBDOF_NL)) DEALLOCATE (NBDOF_NL)
                IF(ALLOCATED(NLLNK))    DEALLOCATE (NLLNK)

C--     Seatblet stuctures deallocation
                IF (NSLIPRING > 0) THEN
                  DO I=1,NSLIPRING
                    DEALLOCATE(SLIPRING(I)%FRAM)
                  ENDDO
                  DEALLOCATE(SLIPRING)
                ENDIF
                IF (NRETRACTOR > 0) THEN
                  DO I=1,NRETRACTOR
                    DEALLOCATE(RETRACTOR(I)%INACTI_NODE)
                    DO K=1,2
                      IF (RETRACTOR(I)%IFUNC(K) > 0) THEN
                        DEALLOCATE(RETRACTOR(I)%TABLE(K)%X(1)%VALUES)
                        DEALLOCATE(RETRACTOR(I)%TABLE(K)%X)
                        DEALLOCATE(RETRACTOR(I)%TABLE(K)%Y%VALUES)
                        DEALLOCATE(RETRACTOR(I)%TABLE(K)%Y)
                      ENDIF
                    ENDDO
                  ENDDO
                  DEALLOCATE(RETRACTOR)
                ENDIF

                !ALEMUSCL
                CALL ALEMUSCL_DEALLOCATE()
                !Multifluid law
                CALL MULTI_DEALLOCATE(MULTI_FVM)
                IF (MULTI_FVM%NS_DIFF) THEN
                  CALL DIFFUSION%TERMINATE_DIFFUSION()
                ENDIF

                !EBCS
                IF(NEBCS > 0)CALL SEGVAR%destroy()
C
                IF(ALLOCATED(STIFN_TMP))DEALLOCATE(STIFN_TMP)
                IF(ALLOCATED(STIFR_TMP))DEALLOCATE(STIFR_TMP)
                IF(ALLOCATED(ACND))DEALLOCATE(ACND)
                IF(ALLOCATED(ARCND))DEALLOCATE(ARCND)
                IF(ALLOCATED(STCND))DEALLOCATE(STCND)
                IF(ALLOCATED(STRCND))DEALLOCATE(STRCND)
                IF(ALLOCATED(LSH4UPL))DEALLOCATE(LSH4UPL)
                IF(ALLOCATED(LSH3UPL))DEALLOCATE(LSH3UPL)
                IF(ALLOCATED(PSH4UPL))DEALLOCATE(PSH4UPL)
                IF(ALLOCATED(PSH3UPL))DEALLOCATE(PSH3UPL)
                IF(ALLOCATED(ERR_THK_SH3))DEALLOCATE(ERR_THK_SH3)
                IF(ALLOCATED(ERR_THK_SH4))DEALLOCATE(ERR_THK_SH4)
                DEALLOCATE(TAGNOD_SMS,NATIV_SMS,NODXI_SMS,TAGPRT_SMS,TAGREL_SMS,
     .                     INDX1_SMS, TAGSLV_RBY_SMS, TAG_LNK_SMS, NRWL_SMS,
     .                     JAD_SMS, JDI_SMS, JSM_SMS,
     .                     JADC_SMS,JADS_SMS,JADS10_SMS,JADT_SMS,
     .                     JADP_SMS,JADR_SMS,JADTG_SMS,
     .                     X_SMS, P_SMS,Y_SMS, Z_SMS, PREC_SMS, PREC_SMS3,
     .                     DIAG_SMS3, LT_SMS,
     .                     KAD_SMS, KDI_SMS , PK_SMS, LTK_SMS,
     .                     JADI_SMS, JDII_SMS, LTI_SMS, MSKYI_SMS, ISKYI_SMS,
     .                     XMOM_SMS, TAGMSR_RBY_SMS, T2MAIN_SMS)
C
                IF(ALLOCATED(FTHREAC))DEALLOCATE(FTHREAC)
                IF(ALLOCATED(FTHDTM))DEALLOCATE(FTHDTM)
                IF(ALLOCATED(FREAC))DEALLOCATE(FREAC)
                IF(ALLOCATED(NODREAC))DEALLOCATE(NODREAC)
                IF(ALLOCATED(GRTH))DEALLOCATE(GRTH)
                IF(ALLOCATED(IGRTH))DEALLOCATE(IGRTH)
                IF(ALLOCATED(GRESAV))DEALLOCATE(GRESAV)
                IF(NRBE3>0)THEN
                  DEALLOCATE(RBE3%RRBE3)
                  IF (IPARIT>0) DEALLOCATE(RBE3%RRBE3_PON)
                END IF
                IF(IMPL_S>0.OR.NEIG>0)THEN
                  CALL DEALLOC_IMPBUF(IMPBUF_TAB)
#if defined(MUMPS5)
                  CALL DEALLOCM_IMP(MUMPS_PAR)
#endif
                END IF
                IF(IMON>0) THEN
                  IF(NVOLU > 0)  CALL FVSTATS(MONVOL)
                  CALL STOPTIME(TIMERS,TIMER_RESOL)
                  IF(IMONM>0) CALL PRINTIME_INTERF(INTERFACES%INTBUF_TAB,IPARI,INTLIST,NBINTC,TIMERS%REALTIME(1)*0.01D0)
                  CALL PRINTIME(TIMERS,GLOB_THERM%ITHERM,OUTPUT)
                ELSE
                  CALL ADD_ELAPSED_TIME_MON_OFF(TIMERS)
                ENDIF
                IF (GLOB_THERM%ITHERM_FE > 0 ) CALL THERMBILAN(GLOB_THERM)
                CALL TRACE_OUT(3)

                DEALLOCATE(WIBEM, WRBEM)
                IF(GLOB_THERM%ITHERM_FE > 0  ) DEALLOCATE(FTHE,FTHESKY)
                IF(GLOB_THERM%INTHEAT > 0  )   DEALLOCATE(FTHESKYI)
                IF(GLOB_THERM%NODADT_THERM > 0  ) DEALLOCATE(CONDN,CONDNSKY)
                IF(GLOB_THERM%NODADT_THERM > 0.AND.GLOB_THERM%INTHEAT > 0) DEALLOCATE(CONDNSKYI)
!
                DEALLOCATE(VRBYM,VRRBYM,ARBYM,ARRBYM)
                IF(IPLYXFEM > 0) THEN
                  DO I=1,NPLYMAX
                    DEALLOCATE(PLY(I)%A)
                    DEALLOCATE(PLY(I)%V)
                    DEALLOCATE(PLY(I)%U)
                    DEALLOCATE(PLYSKY(I)%FSKY)
                  ENDDO
                ENDIF
                IF(INTPLYXFEM > 0)DEALLOCATE(PLYSKYI%FSKYI)
                DEALLOCATE(PLY, PLYSKY)
                IF(ALLOCATED(NODA_FEXT))DEALLOCATE(NODA_FEXT)
                CALL OUTPUT_DEALLOCATE_NODA_PEXT(OUTPUT%DATA)
                DEALLOCATE(NPCONT2)
C Deallocte AMS / POFF
                IF(ALLOCATED(FR_LOC))DEALLOCATE (FR_LOC)
                IF(ALLOCATED(FR_LOCI2M))DEALLOCATE (FR_LOCI2M)
                IF(ALLOCATED(ICODT0))DEALLOCATE (ICODT0)
                IF(ALLOCATED(ICODR0))DEALLOCATE (ICODR0)
                IF(ALLOCATED(ISENSINT))DEALLOCATE(ISENSINT)
C
                CALL FREE_PINCH(PINCH_DATA)
                CALL DEALLOCATE_JOINT()
                CALL BCS%DEALLOCATE()
                IF (ALLOCATED(RBY6)) DEALLOCATE(RBY6)
                IF (ALLOCATED(DXANCG)) DEALLOCATE(DXANCG)
                IF (ALLOCATED(NB25_CANDT)) DEALLOCATE(NB25_CANDT)
                IF (ALLOCATED(NB25_IMPCT)) DEALLOCATE(NB25_IMPCT)
                IF (ALLOCATED(NB25_DST1)) DEALLOCATE(NB25_DST1)
                IF (ALLOCATED(NB25_DST2)) DEALLOCATE(NB25_DST2)
                DEALLOCATE(IGROUC)
                DEALLOCATE(IGROUNC)
                IF (ALLOCATED(SPH_WORK%VOXEL%NNOD)) DEALLOCATE(SPH_WORK%VOXEL%NNOD)
                IF (ALLOCATED(SPH_WORK%VOXEL%DXMIN)) DEALLOCATE(SPH_WORK%VOXEL%DXMIN)
                IF (ALLOCATED(SPH_WORK%VOXEL%DYMIN)) DEALLOCATE(SPH_WORK%VOXEL%DYMIN)
                IF (ALLOCATED(SPH_WORK%VOXEL%DZMIN)) DEALLOCATE(SPH_WORK%VOXEL%DZMIN)
                IF (ALLOCATED(SPH_WORK%VOXEL%DXMAX)) DEALLOCATE(SPH_WORK%VOXEL%DXMAX)                
                IF (ALLOCATED(SPH_WORK%VOXEL%DYMAX)) DEALLOCATE(SPH_WORK%VOXEL%DYMAX)
                IF (ALLOCATED(SPH_WORK%VOXEL%DZMAX)) DEALLOCATE(SPH_WORK%VOXEL%DZMAX)
                RETURN
              ENDIF    ! MSTOP/=0

              CALL TRACE_OUT(3)

C=============================
C END OF EXPLICIT ITERATIVE LOOP
C=============================
              GO TO 100
C
 1002         FORMAT(3X,'* IMPLICIT COMPUTATION TERMINATED WITH ',3X,
     .                  'TOTAL NONLINEAR ITERATIONS:',I8)
 1003         FORMAT(3X,'* TOTAL NUM.OF MATRIX FACTORIZATION AND PCG ITERATION:'
     .                  ,2X,I5,2x,I8)
c-----------
              IF (ViperCoupling) THEN
C                Deallocate indexing arrays
                 IF(ALLOCATED(VIPER%ITABM1))DEALLOCATE(VIPER%ITABM1)
                 IF(ALLOCATED(VIPER%IXEM1))DEALLOCATE(VIPER%IXEM1)
                 CLOSE(VIPER%io_dt)
              ENDIF
c-----------
              RETURN
            END
