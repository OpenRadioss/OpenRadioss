Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    lectur                              ../starter/source/starter/lectur.F
!||--- called by ------------------------------------------------------
!||    starter0                            ../starter/source/starter/starter0.F
!||--- calls      -----------------------------------------------------
!||    add_mass_stat                       ../starter/source/tools/admas/add_mass_stat.F
!||    addmast10                           ../starter/source/tools/admas/addmast10.F
!||    admbcs                              ../starter/source/model/remesh/admbcs.F
!||    alelec                              ../starter/source/ale/alelec.F
!||    alloc_constraint_struct             ../starter/source/modules/constaint_mod.F90
!||    allocxfem                           ../starter/source/elements/xfem/allocxfem.F
!||    ancmsg                              ../starter/source/output/message/message.F
!||    ani_fasolfr1                        ../starter/source/output/anim/ani_fasolfr.F
!||    ani_fasolfr2                        ../starter/source/output/anim/ani_fasolfr.F
!||    ani_segquadfr1                      ../starter/source/output/anim/ani_segquadfr.F
!||    ani_segquadfr2                      ../starter/source/output/anim/ani_segquadfr.F
!||    anodin                              ../starter/source/output/analyse/analyse_node.c
!||    apartin                             ../starter/source/output/analyse/analyse_part.c
!||    applysort2flux                      ../starter/source/restart/ddsplit/w_ithflux.F
!||    applysort2fvm                       ../starter/source/airbag/fvmesh0.F
!||    arret                               ../starter/source/system/arret.F
!||    bcscycmodif_nd                      ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    bcsmodif_nd                         ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    brokmann_elem_renum                 ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
!||    build_addcnel_sub                   ../starter/source/model/mesh/build_addcnel_sub.F
!||    build_admesh                        ../starter/source/model/remesh/build_admesh.F
!||    build_cnel                          ../starter/source/model/mesh/build_cnel.F
!||    build_cnel_sub                      ../starter/source/model/mesh/build_cnel_sub.F
!||    build_csrect                        ../starter/source/model/mesh/build_cnel.F
!||    bulkfakeigeo3                       ../starter/source/elements/ige3d/bulkfakeigeo3.F
!||    c3grhead                            ../starter/source/elements/sh3n/coque3n/c3grhead.F
!||    c3grtails                           ../starter/source/elements/sh3n/coque3n/c3grtails.F
!||    cdk6inx                             ../starter/source/elements/sh3n/coquedk6/cdk6inx.F
!||    cgrhead                             ../starter/source/elements/shell/coque/cgrhead.F
!||    cgrtails                            ../starter/source/elements/shell/coque/cgrtails.F
!||    check_dynain                        ../starter/source/starter/check_dynain.F
!||    check_mat_elem_prop_compatibility   ../starter/source/materials/mat/check_mat_elem_prop_compatibility.F
!||    check_pthickfail                    ../starter/source/materials/fail/check_pthickfail.F
!||    check_qeph_stra                     ../starter/source/starter/check_qeph_stra.F
!||    check_surf                          ../starter/source/groups/check_surf.F
!||    checkrby                            ../starter/source/constraints/general/rbody/checkrby.F
!||    chk_dttsh                           ../starter/source/elements/thickshell/solidec/scdtchk3.F
!||    chk_shell_offset                    ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
!||    chkfunct                            ../starter/source/tools/curve/lecfun.F
!||    chktyp2                             ../starter/source/interfaces/interf1/chktyp2.F
!||    compute_connect_partelm             ../starter/source/model/sets/compute_connect_partelm.F
!||    copy_to_monvol                      ../starter/share/modules1/monvol_struct_mod.F
!||    copy_to_volmon                      ../starter/share/modules1/monvol_struct_mod.F
!||    create_map_tables                   ../starter/source/model/sets/map_tables.F
!||    create_seatbelt                     ../starter/source/tools/seatbelts/create_seatbelt.F
!||    dampdtnoda                          ../starter/source/general_controls/damping/dampdtnoda.F
!||    damping_rby_spmdset                 ../starter/source/general_controls/damping/damping_rby_spmdset.F90
!||    ddprint                             ../starter/source/spmd/domdec2.F
!||    ddsplit                             ../starter/source/restart/ddsplit/ddsplit.F
!||    dealloc_constraint_struct           ../starter/source/modules/constaint_mod.F90
!||    deallocate_detonators               ../starter/share/modules1/detonators_mod.F
!||    deallocate_elbuf                    ../starter/source/elements/elbuf_init/deallocate_buffer.F
!||    deallocate_igrsurf_split            ../starter/source/spmd/deallocate_igrsurf_split.F
!||    deallocate_joint                    ../starter/source/constraints/general/cyl_joint/deallocate_joint.F
!||    deallocate_split_cfd_solide         ../starter/source/spmd/split_cfd_solide.F
!||    desout                              ../starter/source/output/outp/desout.F
!||    dim_s10edg                          ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    domdec1                             ../starter/source/spmd/domain_decomposition/domdec1.F
!||    domdec2                             ../starter/source/spmd/domdec2.F
!||    dometis                             ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    dsdim0                              ../starter/source/implicit/dsolve/dsdim.F
!||    elbuf_ini                           ../starter/source/elements/elbuf_init/elbuf_ini.F
!||    failwave_init                       ../starter/source/materials/fail/failwave_init.F
!||    fictivmassigeo                      ../starter/source/groups/ssurftagigeo.F
!||    fill_intercep                       ../starter/source/spmd/node/ddtools.F
!||    fillcncnd                           ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    fillcne                             ../starter/source/spmd/domdec2.F
!||    fillcne_pxfem                       ../starter/source/properties/composite_options/stack/preplyxfem.F
!||    fillcne_xfem                        ../starter/source/elements/xfem/fillcne_xfem.F
!||    fillcni2                            ../starter/source/spmd/domdec2.F
!||    fixmodif_nd                         ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    flowdec                             ../starter/source/fluid/flowdec.F
!||    fractal_elem_renum                  ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
!||    fsdcod                              ../starter/source/system/fsdcod.F
!||    fvdim                               ../starter/source/airbag/fvmesh.F
!||    fvmesh0                             ../starter/source/airbag/fvmesh0.F
!||    fxbelnum                            ../starter/source/constraints/fxbody/fxbelnum.F
!||    fxbgrav                             ../starter/source/constraints/fxbody/fxbgrav.F
!||    fxbtagn                             ../starter/source/constraints/fxbody/fxbtagn.F
!||    genani1                             ../starter/source/output/anim/genani1.F
!||    get_element_group                   ../starter/source/elements/get_element_group.F90
!||    get_size_inter24                    ../starter/source/spmd/get_size_tag.F
!||    get_size_numnod_local               ../starter/source/spmd/get_size_tag.F
!||    globmat                             ../starter/source/materials/globmat.F
!||    globvars                            ../starter/source/spmd/globvars.F
!||    group_ini                           ../starter/source/model/group/group_ini.F
!||    hierarchy_rbody                     ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hierarchy_rbody_ddm                 ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hm_grogronod                        ../starter/source/groups/hm_grogronod.F
!||    hm_lecgrn                           ../starter/source/groups/hm_lecgrn.F
!||    hm_lines_of_lines                   ../starter/source/groups/hm_lines_of_lines.F
!||    hm_option_count                     ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_pre_read_link                    ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
!||    hm_pre_read_preload                 ../starter/source/loads/general/preload/hm_pre_read_preload.F
!||    hm_pre_read_preload_axial           ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    hm_prelecgrns                       ../starter/source/groups/hm_prelecgrns.F
!||    hm_prelecjoi                        ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
!||    hm_preread_bcscyc                   ../starter/source/constraints/general/bcs/lecbcscyc.F
!||    hm_preread_bem                      ../starter/source/loads/bem/hm_read_bem.F
!||    hm_preread_cload                    ../starter/source/loads/general/cload/hm_preread_cload.F
!||    hm_preread_convec                   ../starter/source/loads/thermic/hm_preread_convec.F
!||    hm_preread_eig                      ../starter/source/general_controls/computation/hm_read_eig.F
!||    hm_preread_grav                     ../starter/source/loads/general/grav/hm_preread_grav.F
!||    hm_preread_impacc                   ../starter/source/constraints/general/impvel/hm_preread_impacc.F
!||    hm_preread_impdisp                  ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
!||    hm_preread_impflux                  ../starter/source/constraints/thermic/hm_preread_impflux.F
!||    hm_preread_imptemp                  ../starter/source/constraints/thermic/hm_preread_imptemp.F
!||    hm_preread_impvel                   ../starter/source/constraints/general/impvel/hm_preread_impvel.F
!||    hm_preread_impvel0                  ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
!||    hm_preread_inivel                   ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
!||    hm_preread_load_centri              ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
!||    hm_preread_load_pressure            ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
!||    hm_preread_merge                    ../starter/source/constraints/general/merge/hm_preread_merge.F
!||    hm_preread_part                     ../starter/source/model/assembling/hm_read_part.F
!||    hm_preread_pblast                   ../starter/source/loads/pblast/hm_preread_pblast.F
!||    hm_preread_pfluid                   ../starter/source/loads/general/pfluid/hm_preread_pfluid.F
!||    hm_preread_pload                    ../starter/source/loads/general/pload/hm_preread_pload.F
!||    hm_preread_radiation                ../starter/source/loads/thermic/hm_preread_radiation.F
!||    hm_preread_rbe2                     ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
!||    hm_preread_rbe3                     ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
!||    hm_preread_rbody                    ../starter/source/constraints/general/rbody/hm_preread_rbody.F
!||    hm_preread_skw                      ../starter/source/tools/skew/hm_preread_skw.F90
!||    hm_preread_sphio                    ../starter/source/loads/sph/hm_preread_sphio.F
!||    hm_preread_xelem                    ../starter/source/elements/reader/hm_preread_xelem.F
!||    hm_print_inter_guided_cable         ../starter/source/tools/seatbelts/hm_print_inter_guided_cable.F90
!||    hm_read_activ                       ../starter/source/tools/activ/hm_read_activ.F
!||    hm_read_adinertia                   ../starter/source/tools/adinertia/hm_read_adinertia.F90
!||    hm_read_admas                       ../starter/source/tools/admas/hm_read_admas.F
!||    hm_read_ale_link                    ../starter/source/constraints/ale/hm_read_ale_link_vel.F
!||    hm_read_alebcs                      ../starter/source/constraints/ale/hm_read_alebcs.F
!||    hm_read_bcs                         ../starter/source/constraints/general/bcs/hm_read_bcs.F
!||    hm_read_bcs_nrf                     ../starter/source/boundary_conditions/hm_read_bcs_nrf.F90
!||    hm_read_bcs_wall                    ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
!||    hm_read_beam                        ../starter/source/elements/reader/hm_read_beam.F
!||    hm_read_bem                         ../starter/source/loads/bem/hm_read_bem.F
!||    hm_read_box                         ../starter/source/model/box/hm_read_box.F
!||    hm_read_cload                       ../starter/source/loads/general/cload/hm_read_cload.F
!||    hm_read_cluster                     ../starter/source/output/cluster/hm_read_cluster.F
!||    hm_read_convec                      ../starter/source/loads/thermic/hm_read_convec.F
!||    hm_read_cyljoint                    ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
!||    hm_read_damp                        ../starter/source/general_controls/damping/hm_read_damp.F
!||    hm_read_drape                       ../starter/source/properties/composite_options/drape/hm_read_drape.F
!||    hm_read_eig                         ../starter/source/general_controls/computation/hm_read_eig.F
!||    hm_read_eref                        ../starter/source/loads/reference_state/eref/hm_read_eref.F
!||    hm_read_friction_models             ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
!||    hm_read_friction_orientations       ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
!||    hm_read_frm                         ../starter/source/tools/skew/hm_read_frm.F
!||    hm_read_func2d                      ../starter/source/tools/curve/hm_read_func2d.F
!||    hm_read_funct                       ../starter/source/tools/curve/hm_read_funct.F
!||    hm_read_funct_python                ../starter/source/tools/curve/hm_read_funct_python.F90
!||    hm_read_fxb1                        ../starter/source/constraints/fxbody/hm_read_fxb.F
!||    hm_read_fxb2                        ../starter/source/constraints/fxbody/hm_read_fxb.F
!||    hm_read_gauge                       ../starter/source/output/gauge/hm_read_gauge.F
!||    hm_read_gjoint                      ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
!||    hm_read_grav                        ../starter/source/loads/general/grav/hm_read_grav.F
!||    hm_read_grpart                      ../starter/source/groups/hm_read_grpart.F
!||    hm_read_impacc                      ../starter/source/constraints/general/impvel/hm_read_impacc.F
!||    hm_read_impflux                     ../starter/source/constraints/thermic/hm_read_impflux.F
!||    hm_read_imptemp                     ../starter/source/constraints/thermic/hm_read_imptemp.F
!||    hm_read_impvel                      ../starter/source/constraints/general/impvel/hm_read_impvel.F
!||    hm_read_inicrack                    ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
!||    hm_read_inigrav                     ../starter/source/initial_conditions/inigrav/hm_read_inigrav.F
!||    hm_read_inimap1d                    ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
!||    hm_read_inimap2d                    ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
!||    hm_read_initemp                     ../starter/source/initial_conditions/thermic/hm_read_initemp.F
!||    hm_read_inivel                      ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
!||    hm_read_inivol                      ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
!||    hm_read_inter_guided_cable          ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||    hm_read_interfaces                  ../starter/source/interfaces/reader/hm_read_interfaces.F
!||    hm_read_intsub                      ../starter/source/output/subinterface/hm_read_intsub.F
!||    hm_read_lines                       ../starter/source/groups/hm_read_lines.F
!||    hm_read_link                        ../starter/source/constraints/rigidlink/hm_read_rlink.F
!||    hm_read_load_centri                 ../starter/source/loads/general/load_centri/hm_read_load_centri.F
!||    hm_read_load_pressure               ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
!||    hm_read_merge                       ../starter/source/constraints/general/merge/hm_read_merge.F
!||    hm_read_merge_node                  ../starter/source/elements/reader/hm_read_merge_node.F
!||    hm_read_move_funct                  ../starter/source/tools/curve/hm_read_move_funct.F
!||    hm_read_mpc                         ../starter/source/constraints/general/mpc/hm_read_mpc.F
!||    hm_read_mpc0                        ../starter/source/constraints/general/mpc/hm_read_mpc.F
!||    hm_read_nbcs                        ../starter/source/constraints/general/bcs/hm_read_nbcs.F
!||    hm_read_node                        ../starter/source/elements/reader/hm_read_node.F
!||    hm_read_part                        ../starter/source/model/assembling/hm_read_part.F
!||    hm_read_pblast                      ../starter/source/loads/pblast/hm_read_pblast.F
!||    hm_read_pcyl                        ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
!||    hm_read_perturb                     ../starter/source/general_controls/computation/hm_read_perturb.F
!||    hm_read_pfluid                      ../starter/source/loads/general/pfluid/hm_read_pfluid.F
!||    hm_read_pload                       ../starter/source/loads/general/pload/hm_read_pload.F
!||    hm_read_prelecdrape                 ../starter/source/properties/composite_options/drape/hm_read_drape.F
!||    hm_read_preload                     ../starter/source/loads/general/preload/hm_read_preload.F
!||    hm_read_preload_axial               ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    hm_read_prethgrou                   ../starter/source/output/th/hm_read_prethgrou.F
!||    hm_read_properties                  ../starter/source/properties/hm_read_properties.F
!||    hm_read_quad                        ../starter/source/elements/reader/hm_read_quad.F
!||    hm_read_radiation                   ../starter/source/loads/thermic/hm_read_radiation.F
!||    hm_read_rand                        ../starter/source/general_controls/computation/hm_read_rand.F
!||    hm_read_rbe2                        ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
!||    hm_read_rbe3                        ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||    hm_read_rbody                       ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    hm_read_rbody_lagmul                ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
!||    hm_read_retractor                   ../starter/source/tools/seatbelts/hm_read_retractor.F
!||    hm_read_rivet                       ../starter/source/elements/reader/hm_read_rivet.F
!||    hm_read_sensors                     ../starter/source/tools/sensor/hm_read_sensors.F
!||    hm_read_sh3n                        ../starter/source/elements/reader/hm_read_sh3n.F
!||    hm_read_shell                       ../starter/source/elements/reader/hm_read_shell.F
!||    hm_read_skw                         ../starter/source/tools/skew/hm_read_skw.F
!||    hm_read_slipring                    ../starter/source/tools/seatbelts/hm_read_slipring.F
!||    hm_read_solid                       ../starter/source/elements/reader/hm_read_solid.F
!||    hm_read_spcnd                       ../starter/source/constraints/sph/hm_read_spcnd.F
!||    hm_read_sphcel                      ../starter/source/elements/reader/hm_read_sphcel.F
!||    hm_read_sphio                       ../starter/source/loads/sph/hm_read_sphio.F
!||    hm_read_spring                      ../starter/source/elements/reader/hm_read_spring.F
!||    hm_read_subset                      ../starter/source/model/assembling/hm_read_subset.F
!||    hm_read_surf                        ../starter/source/groups/hm_read_surf.F
!||    hm_read_surfsurf                    ../starter/source/groups/hm_read_surfsurf.F
!||    hm_read_table1                      ../starter/source/tools/curve/hm_read_table.F
!||    hm_read_table2                      ../starter/source/tools/curve/hm_read_table.F
!||    hm_read_thgrou                      ../starter/source/output/th/hm_read_thgrou.F
!||    hm_read_thpart                      ../starter/source/output/thpart/hm_read_thpart.F
!||    hm_read_tria                        ../starter/source/elements/reader/hm_read_tria.F
!||    hm_read_truss                       ../starter/source/elements/reader/hm_read_truss.F
!||    hm_read_window_user                 ../starter/source/tools/userwi/hm_read_window_user.F
!||    hm_read_xelem                       ../starter/source/elements/reader/hm_read_xelem.F
!||    hm_read_xref                        ../starter/source/loads/reference_state/xref/hm_read_xref.F
!||    hm_set                              ../starter/source/model/sets/hm_set.F
!||    hm_setfxrbyon                       ../starter/source/constraints/fxbody/hm_setfxrbyon.F
!||    hm_yctrl                            ../starter/source/elements/initia/hm_yctrl.F
!||    i24setnodes                         ../starter/source/interfaces/inter3d1/i24setnodes.F
!||    iedge_xfem                          ../starter/source/elements/xfem/iedge_xfem.F
!||    ifrontplus                          ../starter/source/spmd/node/frontplus.F
!||    ig3dgrhead                          ../starter/source/elements/ige3d/ig3dgrhead.F
!||    ig3dgrtails                         ../starter/source/elements/ige3d/ig3dgrtails.F
!||    igrsurf_split                       ../starter/source/spmd/igrsurf_split.F
!||    ind_s10edg                          ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    ini_bcscyc                          ../starter/source/constraints/general/bcs/lecbcscyc.F
!||    ini_h3dtmax_engine                  ../starter/source/starter/contrl.F
!||    ini_ifront                          ../starter/source/spmd/node/ddtools.F
!||    inicrkfill                          ../starter/source/elements/xfem/inicrkfill.F
!||    iniebcs                             ../starter/source/boundary_conditions/ebcs/iniebcs.F
!||    iniebcs_propellant                  ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||    iniebcsp0                           ../starter/source/boundary_conditions/ebcs/iniebcsp0.F
!||    inimu2                              ../starter/source/ale/bimat/inimu2.F
!||    inimu3                              ../starter/source/ale/bimat/inimu3.F
!||    inimul                              ../starter/source/ale/bimat/inimul.F
!||    inintr                              ../starter/source/interfaces/interf1/inintr.F
!||    inintr1                             ../starter/source/interfaces/interf1/inintr1.F
!||    inintr2                             ../starter/source/interfaces/inter3d1/inintr2.F
!||    inintr_orthdirfric                  ../starter/source/interfaces/interf1/inintr_orthdirfric.F
!||    inintr_thkvar                       ../starter/source/interfaces/interf1/inintr_thkvar.F
!||    inintsub                            ../starter/source/interfaces/interf1/inintsub.F
!||    inirbe3                             ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||    inisen                              ../starter/source/tools/sensor/inisen.F
!||    inisms                              ../starter/source/general_controls/computation/hm_read_sms.F
!||    init_bcs_nrf                        ../starter/source/boundary_conditions/init_bcs_nrf.F90
!||    init_joint                          ../starter/source/constraints/general/cyl_joint/init_joint.F
!||    init_mat_weight                     ../starter/source/user_interface/set_dd_mat_weight.F
!||    init_mlaw_tag                       ../starter/source/elements/elbuf_init/init_mlaw_tag.F
!||    init_monvol                         ../starter/source/airbag/init_monvol.F
!||    init_permutation                    ../starter/source/starter/lectur.F
!||    init_random                         ../starter/source/general_controls/computation/init_random.F
!||    initia                              ../starter/source/elements/initia/initia.F
!||    inivchk                             ../starter/source/constraints/general/kinchk.F
!||    inivel                              ../starter/source/initial_conditions/general/inivel/inivel.F
!||    int18_law151_alloc                  ../starter/source/interfaces/int18/int18_law151_alloc.F
!||    int18_law151_init                   ../starter/source/interfaces/int18/int18_law151_init.F
!||    int8_ini                            ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
!||    intbuf_fric_copy                    ../starter/source/interfaces/interf1/intbuf_fric_copy.F
!||    intbuf_fric_ini_starter             ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
!||    intbuf_ini_starter                  ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
!||    inter_offset_itag                   ../starter/source/elements/shell/shell_offset/inter_offset_itag.F90
!||    intstamp_zero                       ../starter/share/modules1/intstamp_mod.F
!||    inverted_group_dealloc              ../starter/source/model/sets/inverted_group_dealloc.F
!||    inverted_group_init                 ../starter/source/model/sets/inverted_group_init.F
!||    islin_ini                           ../starter/source/model/group/islin_ini.F
!||    isurf_ini                           ../starter/source/model/group/isurf_ini.F
!||    kinchk                              ../starter/source/constraints/general/kinchk.F
!||    kinini                              ../starter/source/constraints/general/kinini.F
!||    kinrem                              ../starter/source/constraints/general/kinchk.F
!||    lagm_ini                            ../starter/source/tools/lagmul/lagm_ini.F
!||    lagm_nhf                            ../starter/source/tools/lagmul/lagm_nhf.F
!||    laserp1                             ../starter/source/loads/laser/laserp.F
!||    laserp3                             ../starter/source/loads/laser/laserp.F
!||    lce16q3                             ../starter/source/elements/solid_2d/quad/lce16q.F
!||    lce16s3                             ../starter/source/elements/reader/hm_read_solid.F
!||    lce16s4                             ../starter/source/elements/reader/hm_read_solid.F
!||    lec_ddw                             ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    lec_ddw_poin                        ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    lec_inistate                        ../starter/source/elements/initia/lec_inistate.F
!||    lecacc                              ../starter/source/tools/accele/lecacc.F
!||    lecextlnk                           ../starter/source/coupling/rad2rad/lecextlnk.F
!||    lecfill                             ../starter/source/elements/initia/lecfill.F
!||    lecggroup                           ../starter/source/groups/lecggroup.F
!||    lecgroup                            ../starter/source/groups/lecgroup.F
!||    lecig3d                             ../starter/source/elements/ige3d/lecig3d.F
!||    lecins                              ../starter/source/interfaces/interf1/lecins.F
!||    lecint                              ../starter/source/interfaces/interf1/lecint.F
!||    leclas                              ../starter/source/loads/laser/leclas.F
!||    lecrefsta                           ../starter/source/loads/reference_state/refsta/lecrefsta.F
!||    lecsec0                             ../starter/source/tools/sect/hm_read_sect.F
!||    lecsec42                            ../starter/source/tools/sect/hm_read_sect.F
!||    lecsec4bolt                         ../starter/source/tools/sect/lecsec4bolt.F
!||    lecstack_ply                        ../starter/source/properties/composite_options/stack/lecstack_ply.F
!||    lecstamp                            ../starter/source/interfaces/interf1/lecstamp.F
!||    lecsubmod                           ../starter/source/model/submodel/lecsubmod.F
!||    lectrans                            ../starter/source/model/transformation/lectrans.F
!||    lectranssub                         ../starter/source/model/submodel/lectranssub.F
!||    lgmini_i7                           ../starter/source/tools/lagmul/lgmini_i7.F
!||    lgmini_rwl                          ../starter/source/tools/lagmul/lgmini_rwl.F
!||    line_decomp                         ../starter/source/model/group/line_decomp.F
!||    m20dcod                             ../starter/source/system/fsdcod.F
!||    merge                               ../starter/source/model/submodel/merge.F
!||    merge_node                          ../starter/source/elements/nodes/merge_node.F
!||    monvol_allocate                     ../starter/share/modules1/monvol_struct_mod.F
!||    monvol_deallocate                   ../starter/share/modules1/monvol_struct_mod.F
!||    multi_connectivity                  ../starter/source/multifluid/multi_connectivity.F
!||    multi_unplug_neighbors              ../starter/source/multifluid/multi_unplug_neighbors.F
!||    my_real_tot_int_transfer            ../starter/source/system/my_real_to_int_transfer.F90
!||    nbfunct                             ../starter/source/tools/curve/nbfunc.F
!||    nlocal                              ../starter/source/spmd/node/ddtools.F
!||    nodm11                              ../starter/source/materials/mat/mat011/nodm11.F
!||    outri                               ../starter/source/materials/time_step/outri.F
!||    outrin                              ../starter/source/materials/time_step/outri.F
!||    paroi                               ../starter/source/materials/mat/mat006/paroi.F
!||    pgrhead                             ../starter/source/elements/beam/pgrhead.F
!||    pgrtails                            ../starter/source/elements/beam/pgrtails.F
!||    pornod                              ../starter/source/ale/pornod.F
!||    pre_cndpon                          ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    pre_stackgroup                      ../starter/source/stack/pres_stackgroup.F
!||    precrkxfem                          ../starter/source/elements/xfem/precrkxfem.F
!||    prelec_ddw                          ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    prelec_ddw_poin                     ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    prelecig3d                          ../starter/source/elements/ige3d/prelecig3d.F
!||    prelecsec                           ../starter/source/tools/sect/prelecsec.F
!||    prelecsec4bolt                      ../starter/source/tools/sect/prelecsec4bolt.F
!||    prepare_int25                       ../starter/source/model/mesh/build_cnel.F
!||    prepare_split_i25e2e                ../starter/source/spmd/prepare_split_i25e2e.F
!||    preplyxfem                          ../starter/source/properties/composite_options/stack/preplyxfem.F
!||    prerafig3d                          ../starter/source/elements/ige3d/prerafig3d.F
!||    preread_rbody_lagmul                ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
!||    preread_rbody_set                   ../starter/source/model/sets/preread_rbody_set.F
!||    prescrint                           ../starter/source/interfaces/interf1/prescrint.F
!||    presearchigeo3d                     ../starter/source/elements/ige3d/searchigeo3d.F
!||    pretag_xfem                         ../starter/source/elements/xfem/pretag_xfem.F
!||    printbcs                            ../starter/source/constraints/general/bcs/printbcs.F
!||    printgroup                          ../starter/source/output/outfile/printgroup.F
!||    printstsz                           ../starter/source/spmd/node/st_array_size.F
!||    python_duplicate_nodes              ../starter/source/spmd/domain_decomposition/python_duplicate_nodes.F90
!||    qgrhead                             ../starter/source/elements/solid_2d/quad/qgrhead.F
!||    qgrtails                            ../starter/source/elements/solid_2d/quad/qgrtails.F
!||    r2r_check                           ../starter/source/coupling/rad2rad/r2r_check.F
!||    r2r_clean_inter                     ../starter/source/coupling/rad2rad/r2r_clean_inter.F
!||    r2r_domdec                          ../starter/source/coupling/rad2rad/r2r_domdec.F
!||    r2r_group                           ../starter/source/coupling/rad2rad/r2r_group.F
!||    r2r_matparam_copy                   ../starter/source/elements/elbuf_init/r2r_matparam_copy.F
!||    r2r_nom_opt                         ../starter/source/coupling/rad2rad/routines_r2r.F
!||    r2r_speedup                         ../starter/source/coupling/rad2rad/r2r_speedup.F
!||    r2r_split                           ../starter/source/coupling/rad2rad/r2r_split.F
!||    r2r_void                            ../starter/source/coupling/rad2rad/r2r_void.F
!||    rbe2modif1_nd                       ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    rbody_part_check                    ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    read_detonators                     ../starter/source/initial_conditions/detonation/read_detonators.F
!||    read_ebcs                           ../starter/source/boundary_conditions/ebcs/read_ebcs.F
!||    read_engine_driver                  ../starter/source/general_controls/engine/read_engine_driver.F
!||    read_material_models                ../starter/source/materials/read_material_models.F
!||    read_monvol                         ../starter/source/airbag/read_monvol.F
!||    read_rwall                          ../starter/source/constraints/general/rwall/read_rwall.F
!||    reconnect                           ../starter/source/elements/nodes/reconnect.F
!||    reini_matprop                       ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    reini_matprop2                      ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    reord_icnd                          ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    rgrhead                             ../starter/source/elements/spring/rgrhead.F
!||    rgrtails                            ../starter/source/elements/spring/rgrtails.F
!||    rigid_mat                           ../starter/source/materials/mat/mat019/rigid_mat.F
!||    rigmodif1_nd                        ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    rpart_inivel_check                  ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    s10edg_rlink                        ../starter/source/elements/solid/solide10/s10edg_rlink.F
!||    scrint                              ../starter/source/interfaces/interf1/scrint.F
!||    searchigeo3d                        ../starter/source/elements/ige3d/searchigeo3d.F
!||    sensor_user_convert_local_id        ../starter/source/tools/sensor/sensor_user_convert_local_id.F
!||    sensor_user_init                    ../starter/source/tools/sensor/sensor_user_init.F
!||    set_admesh                          ../starter/source/model/remesh/set_admesh.F
!||    set_elgroup_param                   ../starter/source/elements/shell/coque/set_elgroup_param.F
!||    set_front8                          ../starter/source/spmd/node/ddtools.F
!||    set_ibufssg_io                      ../starter/source/starter/lectur.F
!||    set_intercep                        ../starter/source/spmd/node/ddtools.F
!||    set_poin_ump                        ../starter/source/system/set_poin_ump.F
!||    set_usrtos                          ../starter/source/model/sets/ipartm1.F
!||    seteloff                            ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    seteloff2                           ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
!||    setlenwa                            ../starter/source/restart/ddsplit/setlenwa.F
!||    setmulti                            ../starter/source/restart/ddsplit/setlenwa.F
!||    setrb2on                            ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
!||    setrbyon                            ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    sfem_exclude_dim                    ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sfem_exclude_ini                    ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sgrhead                             ../starter/source/elements/solid/solide/sgrhead.F
!||    sgrtails                            ../starter/source/elements/solid/solide/sgrtails.F
!||    shell_offset_ini                    ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
!||    shell_offsetp                       ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||    shellthk_upd                        ../starter/source/properties/composite_options/drape/shellthk_upd.F
!||    sms_ini_jad_1                       ../starter/source/ams/sms_init.F
!||    sms_ini_jad_2                       ../starter/source/ams/sms_init.F
!||    sms_ini_jad_3                       ../starter/source/ams/sms_init.F
!||    sms_ini_kad                         ../starter/source/ams/sms_init.F
!||    sms_ini_kdi                         ../starter/source/ams/sms_init.F
!||    sms_init                            ../starter/source/ams/sms_init.F
!||    sort_surf                           ../starter/source/groups/sort_surf.F
!||    sortgroup                           ../starter/source/system/nintrr.F
!||    spdometis                           ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    spgrhead                            ../starter/source/elements/sph/spgrhead.F
!||    spgrtails                           ../starter/source/elements/sph/spgrtails.F
!||    sphdcod                             ../starter/source/elements/sph/sphdcod.F
!||    sphonf0                             ../starter/source/elements/sph/sphonf0.F
!||    spinih                              ../starter/source/elements/sph/spinih.F
!||    split_bcs_nrf                       ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||    split_bcs_wall                      ../starter/source/restart/ddsplit/split_bcs_wall.F90
!||    split_cfd_solide                    ../starter/source/spmd/split_cfd_solide.F
!||    split_pcyl                          ../starter/source/loads/general/load_pcyl/split_pcyl.F
!||    split_rwall                         ../starter/source/constraints/general/rwall/split_rwall.F90
!||    spmd_anim_ply_init                  ../starter/source/spmd/spmd_anim_ply_init.F
!||    sptri                               ../starter/source/elements/sph/sptri.F
!||    st_qaprint_driver                   ../starter/source/output/qaprint/st_qaprint_driver.F
!||    stackgroup                          ../starter/source/stack/stackgroup.F
!||    stackgroup_drape                    ../starter/source/stack/stackgroup_drape.F
!||    startime                            ../starter/source/system/timer.F
!||    state_admesh                        ../starter/source/model/remesh/state_admesh.F
!||    stifint_icontrol                    ../starter/source/interfaces/interf1/stifint_icontrol.F90
!||    stifn0_nd                           ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    stifn1_nd                           ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    stoptime                            ../starter/source/system/timer.F
!||    subset_ini                          ../starter/source/model/group/subset_ini.F
!||    surfext_tagn                        ../starter/source/groups/ssurftag.F
!||    t3grhead                            ../starter/source/elements/solid_2d/tria/t3grhead.F
!||    t3grtails                           ../starter/source/elements/solid_2d/tria/t3grtails.F
!||    table_zero                          ../starter/source/tools/curve/table_tools.F
!||    tet4_10                             ../starter/source/starter/lectur.F
!||    tgrhead                             ../starter/source/elements/truss/tgrhead.F
!||    tgrtails                            ../starter/source/elements/truss/tgrtails.F
!||    th_surf_load_pressure               ../starter/source/output/th/th_surf_load_pressure.F
!||    thpinit                             ../starter/source/output/th/thpinit.F
!||    thskewc                             ../starter/source/output/th/thskewc.F
!||    titre2                              ../starter/source/output/outfile/titre2.F
!||    titre3                              ../starter/source/output/outfile/titre3.F
!||    trace_in1                           ../starter/source/system/trace_back.F
!||    trace_out1                          ../starter/source/system/trace_back.F
!||    triintfric                          ../starter/source/interfaces/interf1/trintfric.F
!||    update_weight_rbe3                  ../starter/source/spmd/domain_decomposition/update_weight_rbe3.F
!||    updfail                             ../starter/source/materials/updfail.F90
!||    updmat                              ../starter/source/materials/updmat.F
!||    upgrade_ixint                       ../starter/source/interfaces/interf1/upgrade_ixint.F
!||    xfem_crack_init                     ../starter/source/elements/xfem/xfem_crack_init.F
!||    xgrhead                             ../starter/source/elements/xelem/xgrhead.F
!||    xgrtails                            ../starter/source/elements/xelem/xgrtails.F
!||    yctrl                               ../starter/source/initial_conditions/inista/yctrl.F
!||--- uses       -----------------------------------------------------
!||    adinertia_mod                       ../starter/share/modules1/adinertia_mod.F90
!||    apply_permutation_mod               ../starter/source/spmd/tools/apply_permutation.F90
!||    bpreload_mod                        ../starter/share/modules1/bpreload_mod.F
!||    brokmann_elem_renum_mod             ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
!||    chk_shell_offset_mod                ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
!||    cluster_mod                         ../starter/share/modules1/cluster_mod.F
!||    constraint_mod                      ../starter/source/modules/constaint_mod.F90
!||    damping_rby_spmdset_mod             ../starter/source/general_controls/damping/damping_rby_spmdset.F90
!||    ddweights_mod                       ../starter/share/modules1/ddweights_mod.F
!||    defaults_mod                        ../starter/source/modules/defaults_mod.F90
!||    detonators_mod                      ../starter/share/modules1/detonators_mod.F
!||    drape_mod                           ../starter/share/modules1/drape_mod.F
!||    dsgraph_mod                         ../starter/share/modules1/dsgraph_mod.F
!||    elbuftag_mod                        ../starter/share/modules1/elbuftag_mod.F
!||    elm_group_mod                       ../starter/source/modules/elm_group_mod.F90
!||    fractal_elem_renum_mod              ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
!||    front_mod                           ../starter/share/modules1/front_mod.F
!||    func2d_mod                          ../starter/share/modules1/func2d_mod.F
!||    fvbag_mod                           ../starter/share/modules1/fvbag_mod.F
!||    get_element_group_mod               ../starter/source/elements/get_element_group.F90
!||    group_mod                           ../starter/share/modules1/group_mod.F
!||    hierarchy_rbody_mod                 ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hm_option_read_mod                  ../starter/share/modules1/hm_option_read_mod.F
!||    hm_preread_inivel_mod               ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
!||    hm_preread_skw_mod                  ../starter/source/tools/skew/hm_preread_skw.F90
!||    hm_print_inter_guided_cable_mod     ../starter/source/tools/seatbelts/hm_print_inter_guided_cable.F90
!||    hm_read_adinertia_mod               ../starter/source/tools/adinertia/hm_read_adinertia.F90
!||    hm_read_bcs_nrf_mod                 ../starter/source/boundary_conditions/hm_read_bcs_nrf.F90
!||    hm_read_bcs_wall_mod                ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
!||    hm_read_inivol_mod                  ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
!||    hm_read_inter_guided_cable_mod      ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||    hm_read_preload_axial_mod           ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    iniebcs_propellant_                 ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||    inigrav                             ../starter/share/modules1/inigrav_mod.F
!||    inimap1d_mod                        ../starter/share/modules1/inimap1d_mod.F
!||    inimap2d_mod                        ../starter/share/modules1/inimap2d_mod.F
!||    init_bcs_nrf_mod                    ../starter/source/boundary_conditions/init_bcs_nrf.F90
!||    init_monvol_mod                     ../starter/source/airbag/init_monvol.F
!||    inivol_def_mod                      ../starter/share/modules1/inivol_mod.F
!||    intbufmod                           ../starter/share/modules1/restart_mod.F
!||    intbufscratch_mod                   ../starter/source/interfaces/interf1/intbufscratch_mod.F
!||    inter_cand_mod                      ../starter/share/modules1/inter_cand_mod.F
!||    inter_offset_itag_mod               ../starter/source/elements/shell/shell_offset/inter_offset_itag.F90
!||    intstamp_glob_mod                   ../starter/share/modules1/intstamp_glob_mod.F
!||    inverted_group_mod                  ../starter/share/modules1/inverted_group_mod.F
!||    joint_mod                           ../starter/share/modules1/joint_mod.F
!||    mapping_option_mod                  ../starter/share/modules1/dichotomy_mod.F
!||    meshsurfig3d_mod                    ../starter/source/elements/ige3d/meshsurfig3d_mod.F
!||    message_mod                         ../starter/share/message_module/message_mod.F
!||    mid_pid_mod                         ../starter/share/modules1/mid_pid_mod.F
!||    monvol_struct_mod                   ../starter/share/modules1/monvol_struct_mod.F
!||    my_real_tot_int_transfer_mod        ../starter/source/system/my_real_to_int_transfer.F90
!||    nod2el_mod                          ../starter/share/modules1/nod2el_mod.F
!||    option_mod                          ../starter/share/modules1/options_mod.F
!||    python_duplicate_nodes_mod          ../starter/source/spmd/domain_decomposition/python_duplicate_nodes.F90
!||    r2r_mod                             ../starter/share/modules1/r2r_mod.F
!||    random_mod                          ../starter/share/modules1/random_mod.F
!||    rbody_part_modif_mod                ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    read_funct_python_mod               ../starter/source/tools/curve/hm_read_funct_python.F90
!||    reorder_mod                         ../starter/share/modules1/reorder_mod.F
!||    restmod                             ../starter/share/modules1/restart_mod.F
!||    set_mod                             ../starter/share/modules1/set_mod.F
!||    sfem_exclude_mod                    ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    shell_offset_ini_mod                ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
!||    shell_offsetp_mod                   ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||    sms_mod                             ../starter/share/modules1/sms_mod.F
!||    sph_mod                             ../starter/share/modules1/sph_mod.F
!||    split_bcs_nrf_mod                   ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||    split_bcs_wall_mod                  ../starter/source/restart/ddsplit/split_bcs_wall.F90
!||    split_cfd_mod                       ../starter/share/modules1/split_cfd_mod.F
!||    split_rwall_mod                     ../starter/source/constraints/general/rwall/split_rwall.F90
!||    stack_mod                           ../starter/share/modules1/stack_mod.F
!||    stack_var_mod                       ../starter/share/modules1/stack_var_mod.F
!||    stifint_icontrol_mod                ../starter/source/interfaces/interf1/stifint_icontrol.F90
!||    submodel_mod                        ../starter/share/modules1/submodel_mod.F
!||    table_glob_mod                      ../starter/share/modules1/table_glob_mod.F
!||    updfail_mod                         ../starter/source/materials/updfail.F90
!||    user_sensor_mod                     ../starter/source/modules/user_interface_mod.F90
!||====================================================================
      SUBROUTINE LECTUR(MULTI_FVM                   ,LSUBMODEL          ,IS_DYNA         ,DETONATORS   ,EBCS_TAB,
     .                  SEATBELT_CONVERTED_ELEMENTS ,NB_SEATBELT_SHELLS ,NB_DYNA_INCLUDE ,USER_WINDOWS ,OUTPUT  ,
     .                  MAT_ELEM,NAMES_AND_TITLES,DEFAULTS,glob_therm,pblast,
     .                  SENSOR_USER_STRUCT,CHECK_USED,N_NRBODY,NRBODY_TO_PART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE DSGRAPH_MOD
      USE FVBAG_MOD
      USE RESTMOD
      USE INTBUFMOD
      USE NOD2EL_MOD
      USE INTSTAMP_GLOB_MOD
      USE SUBMODEL_MOD
      USE SMS_MOD
      USE TABLE_GLOB_MOD
      USE R2R_MOD
      USE ELBUFDEF_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE SPH_MOD
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE INTBUFSCRATCH_MOD
      USE DDWEIGHTS_MOD
      USE XFEM2DEF_MOD
      USE STACK_MOD
      USE ALEFVM_MOD
      USE INT8_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE MULTI_FVM_MOD
      USE STACK_VAR_MOD
      USE BPRELOAD_MOD
      USE REORDER_MOD
      USE INIGRAV
      USE INTBUF_FRIC_MOD
      USE INIMAP1D_MOD
      USE INIMAP2D_MOD
      USE FUNC2D_MOD
      USE GROUPDEF_MOD
      USE GROUP_MOD
      USE OPTIONDEF_MOD
      USE OPTION_MOD
      USE OPTIONDEF_MOD
      USE MID_PID_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SKEW_MOD
      USE MESHSURFIG3D_MOD
      USE MAT_ELEM_MOD
      USE SPLIT_CFD_MOD
      USE PINCHTYPE_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE MONVOL_STRUCT_MOD
      USE SETDEF_MOD
      USE SET_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE DRAPE_MOD
      USE INIVOL_DEF_MOD
      USE SENSOR_MOD
      USE RANDOM_MOD
      USE ALE_EBCS_MOD
      USE INVERTED_GROUP_MOD
      USE MAPPING_OPTION_MOD
      USE EBCS_MOD
      USE JOINT_MOD
      USE HM_OPTION_READ_MOD
      USE SEATBELT_MOD
      USE LOADS_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE USER_SENSOR_MOD
      USE ALE_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
      USE READ_FUNCT_PYTHON_MOD
      USE PYTHON_FUNCT_MOD
      USE INTER_CAND_MOD
      USE NAMES_AND_TITLES_MOD
      USE damping_rby_spmdset_mod
      USE HM_READ_PRELOAD_AXIAL_MOD
      USE BCS_MOD, ONLY : BCS, BCS_STRUCT_
      USE DEFAULTS_MOD
      USE SHELL_OFFSET_INI_MOD,ONLY: SHELL_OFFSET_INI
      USE INTER_OFFSET_ITAG_MOD, ONLY:INTER_OFFSET_ITAG
      USE CHK_SHELL_OFFSET_MOD, ONLY:CHK_SHELL_OFFSET
      USE SHELL_OFFSETP_MOD, ONLY:SHELL_OFFSETP
      USE UPDFAIL_MOD
      USE RANDOM_WALK_DEF_MOD
      use constraint_mod , only : constraint_,alloc_constraint_struct,dealloc_constraint_struct
      use split_rwall_mod , only : split_rwall
      use stifint_icontrol_mod, only : stifint_icontrol
      use fractal_elem_renum_mod
      use hm_preread_inivel_mod, only : hm_preread_inivel
      use brokmann_random_def_mod
      use brokmann_elem_renum_mod
      use glob_therm_mod
      use hm_read_inivol_mod
      use pblast_mod
      use iniebcs_propellant_
      use init_monvol_mod ,only : init_monvol
      use python_duplicate_nodes_mod
      use split_bcs_wall_mod , only : split_bcs_wall
      use split_bcs_nrf_mod , only : split_bcs_nrf
      use hm_read_bcs_wall_mod , only : hm_read_bcs_wall
      use hm_read_bcs_nrf_mod , only : hm_read_bcs_nrf
      use element_mod , only : nixs,nixq,nixc,nixp,nixt,nixr,nixtg
      use hm_preread_skw_mod, ONLY : hm_preread_skw
      use hierarchy_rbody_mod
      use sfem_exclude_mod
      use hm_read_inter_guided_cable_mod, ONLY : hm_read_inter_guided_cable
      use hm_print_inter_guided_cable_mod, ONLY : hm_print_inter_guided_cable
      use get_element_group_mod , only : get_element_group
      use elm_group_mod , only : elm_group_
      use my_real_tot_int_transfer_mod , only : my_real_tot_int_transfer
      use rbody_part_modif_mod,     ONLY : rbody_part_check,rpart_inivel_check
      use init_bcs_nrf_mod , only : init_bcs_nrf
      use apply_permutation_mod , only : apply_permutation
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT)                :: MULTI_FVM
      TYPE(SUBMODEL_DATA)                   :: LSUBMODEL(NSUBMOD)
      INTEGER,INTENT(IN)                    :: IS_DYNA
      TYPE(DETONATORS_STRUCT_),TARGET       :: DETONATORS
      TYPE(T_EBCS_TAB), INTENT(INOUT)       :: EBCS_TAB
      INTEGER,INTENT(IN)                    :: NB_SEATBELT_SHELLS
      INTEGER,INTENT(INOUT)                 :: SEATBELT_CONVERTED_ELEMENTS(3,NB_SEATBELT_SHELLS)
      INTEGER,INTENT(IN)                    :: NB_DYNA_INCLUDE
      TYPE(USER_WINDOWS_), INTENT(INOUT)    :: USER_WINDOWS
      TYPE(OUTPUT_),  INTENT(INOUT)         :: OUTPUT
      TYPE(NAMES_AND_TITLES_),INTENT(INOUT) :: NAMES_AND_TITLES !< NAMES_AND_TITLES host the input deck names and titles for outputs
      TYPE(DEFAULTS_),INTENT(INOUT)         :: DEFAULTS
      type(glob_therm_) ,intent(inout)      :: glob_therm
      TYPE(PBLAST_),INTENT(INOUT)           :: PBLAST
      TYPE(SENSOR_USER_STRUCT_)              :: SENSOR_USER_STRUCT
      INTEGER,INTENT(IN)                     :: CHECK_USED                          !< Flag to check if node is used
      INTEGER,INTENT(IN)                     :: N_NRBODY                            !< Number of rigid bodies by part
      INTEGER,DIMENSION(N_NRBODY),INTENT(IN) :: NRBODY_TO_PART                      !< rigid body to part mapping
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "hash_id.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "com_engcards_c.inc"
#include      "com_xfem1.inc"
#include      "eigcom.inc"
#include      "flowcom.inc"
#include      "fxbcom.inc"
#include      "intstamp_c.inc"
#include      "lagmult.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "tabsiz_c.inc"
#include      "tablen_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr10_c.inc"
#include      "scr12_c.inc" 
#include      "scr14_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "scr23_c.inc"
#include      "scry_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "ddspmd_c.inc"
#include      "sysunit.inc"
#include      "titr_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "r2r_c.inc"
#include      "intread_c.inc"
#include      "elbuf_c.inc"
#include      "userlib.inc"
#include      "drape_c.inc"
#include      "boltpr_c.inc"
#include      "inigrav_c.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "ige3d_c.inc"
#include      "random_c.inc"
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
      INTEGER  SET_USRTOS
      EXTERNAL SET_USRTOS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE INTERMASURFEP
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE INTERMASURFEP

      INTEGER II,I,J,KK,N, NPTS, NMNT, NRTMT_25,MLW,
     .   NUMEL, IFIP, IS_EULER,NB_EULER_GROUPS,
     .   NAIRWA,NTHWA, LWASPIO, LEN_G,LEN_M,LEN,
     .   IADBUF, IADGEO, NUVAR, NUVARI,
     .   NS_I7,NEL,ND,AUX,NS_I21,
     .   NPT,NS_I11,
     .   SVOLMON0,FLAG_GOTO,
     .   INNOD,INSEG,NSIGI, NSIGS, NSIGSH, NSIGSPH,
     .   LB_MAX, P, NG,
     .   IINU ,NEL3D,NEL2D,NEL1D,IMAX,JMAX,
     .   OFF, NELEM, IDDLEVEL, NELEMINT,
     .   IFIXIN,IFIEND,ICO,
     .   IDS,IUN,L_MUL_LAG,NCMAX,NKMAX,
     .   MAXRTMS,MAXNSNE,
     .   MAXRTM,LWAT, L_MUL_LAG1,ISHIF,LIBAGALE,
     .   LENTHG, LBUFMAT, LBUFGEO, LBUFSF,
     .   LNOM_OPT, LENVOLU, ILEN, LCNE, LCNI2G,LENPOR,
     .   PM1SHF, NFX, AIPM, ANOD, AMOD, NBNO, NBMO,
     .   ALM, NELS, NELC, NELTG, NLGRAV, AGRVI, AGRVR,
     .   NNT, RCLEN,PM1SPH, STAT, NELDMAX, VERSDD,
     .   DSNISM, NSLEVEL,NSDEC,NSVMAX,NSPRI,DSARCH,NELT,NELP,NSEGS,
     .   NNFT, NDOFMIN, NMANIM, DSANIM,NRCVVOIS0,NSIGRS,
     .   LRBAGALE,FLAGG,ICOUNT,SWAFT, SWA4, SMATER, SEL2FA,
     .   SNFACPTX,SIXEDGE,SOFFX1,SNUMX1,SXNORM,SINVERT,SFUNC1,SIAD,
     .   SMAS,LEN_RM,LAG_NCF0,LAG_NKF0,
     .   LAG_NHF0,LAG_NCL0,LAG_NKL0,MAXNNOD, IBID,
     .   SRTRANS,LCNE_CRKXFEM,NSEGSMAX,XFEMON,
     .   IN10,IN20,SNOM_OPT_OLD,LENI,FLAG_ALLOCATE,
     .   PROC_BID,FLG_R2R_ERR,NSPCOND0,LENTHGR,FLAG_XFEM,
     .   IADTABIGE,NDOUBLONIGE,DECALIGEO,HM_NSENSOR,
     .   IPARSENS,NBT8,
     .   TAB_SOL(6),ISTR_24,IDEL_SOLID,
     .   LCNCND,I24MAXNSNE,NSIGBEAM,NSIGTRUSS,S_LOADPINTER,
     .   FLAGF,ITHFLAG,MAXRTM_T2,NS_I2,SITAGE,NCTRLMAX,INLIN,SVR_1
      INTEGER (KIND=8) EMAX
      INTEGER (KIND=8) K0,K1,K2,K3,K4,K5,K6,K7,K8
! integer 8 version of the variables NUMELC etc.
      INTEGER(KIND=8) NUMELCK8
      INTEGER(KIND=8) NUMELTGK8
      INTEGER(KIND=8) NUMELSK8
      INTEGER(KIND=8) NUMELRK8
      INTEGER(KIND=8) NUMELPK8
      INTEGER(KIND=8) NUMELTK8
      INTEGER(KIND=8) NUMELQK8
      INTEGER(KIND=8) NUMELXK8
      INTEGER(KIND=8) NUMELIG3DK8
      INTEGER(KIND=8) NUMSPHK8
      INTEGER(KIND=8) SVEUL8

      INTEGER
     .   ISUBMOD(NSUBMOD),
     .   IUPARAM(100),DDSTAT(50,PARASIZ),IGRNRB2(NRBE2),
     .   LCNE_PXFEM
      INTEGER NSNT,NMNT_2

      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_UMP_OLD
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_UMP_OLD
      my_real, DIMENSION(:), ALLOCATABLE :: CPUTIME_MP_OLD

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(:), ALLOCATABLE :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(:,:), ALLOCATABLE :: MID_PID_SOL
      LOGICAL MARQUEUR3

      INTEGER, DIMENSION(:),ALLOCATABLE ::
     *        IWCONT, IWCIN2      , IKINE1LAG ,DSDOF, TAGXREF,
     *        ADDCNE, ADDCNE_PXFEM, FXBTAG    ,ADDCNE_CRKXFEM,
     *        TAGREFSTA,CSRECT

      INTEGER, DIMENSION(:),ALLOCATABLE ::
     *        ISOLNOD,ISOLOFF,ISHEOFF,ITRUOFF,IPOUOFF,
     *        IRESOFF,ITRIOFF,IGRNRBY,IQUAOFF
      integer, dimension(:), allocatable :: isolnod0,ishell3nod0
C
      INTEGER(KIND=8) :: KVOISPH8,NUMSPH8,SIXSP8,LIMIT8
      INTEGER :: INTEGER_LIMIT32

      INTEGER, DIMENSION(:),ALLOCATABLE :: CEP,CEL,CNE,
     .                                     CNI2, CELI2, CEPI2,
     .                                     CEPSP, CELSPH, ITAGSH,
     .                                     CNE_PXFEM,CEL_PXFEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: FXBIPM, FXBNOD, FXBELM,
     .                                      FXBGRVI, EIGIPM, EIGIBUF,
     .                                      IMERGE,INTIDS,
     .                                      IMERGE2,IADMERGE2,
     .                                      NSLNRBM,  SLNRBM,
     .                                      IGEO_STACK
      INTEGER, DIMENSION(:,:,:),  ALLOCATABLE :: ELDOM
      INTEGER, DIMENSION(:),      ALLOCATABLE :: CEPTMP, NELDOM
      INTEGER, DIMENSION(:),      ALLOCATABLE ::  LLL
      INTEGER, DIMENSION(:,:),    ALLOCATABLE :: ELSUB
      INTEGER(KIND=8) ,TARGET :: DSMEMORY(7,NSPMD)
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: FXANI,IWORKSH, FXB_MATRIX_ADD
      INTEGER, DIMENSION(:),     ALLOCATABLE :: FASTAG,SEGTAG
      INTEGER(KIND=8),TARGET :: MEMFLOW(2,NSPMD)
      INTEGER, DIMENSION(:),     ALLOCATABLE :: IFLOW
C
      INTEGER, DIMENSION(:),     ALLOCATABLE :: KINWORK

      INTEGER, DIMENSION(:),     ALLOCATABLE :: CNE_CRKXFEM,CEL_CRKXFEM,ITAGN,ITAGE,CEP_CRKXFEM,IEDGE_TMP0,CRKNODIAD
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: IEDGE_TMP
C
      INTEGER, DIMENSION(:),     ALLOCATABLE :: NALE_R2R
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: FRONTB_R2R
c elem sorting
      INTEGER, DIMENSION(:), ALLOCATABLE :: IXS_S ,IXS_S_IND,
     2     IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     3     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     4     IXTG_S,IXTG_S_IND

c tab IBUFSSG_IO specific inlet/outlet
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFSSG_IO, RESERVEP

      INTEGER, DIMENSION(:), ALLOCATABLE :: IXR_KJ,R_SKEW
C
C  initial direction for beams
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBEAM_VECTOR
      my_real, DIMENSION(:,:), ALLOCATABLE :: RBEAM_VECTOR
C
C Sol2sph
      INTEGER, DIMENSION(:), ALLOCATABLE :: SOL2SPH_TYP
c tab /BCS/CYCLIC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBCSCYC,LBCSCYC,ITAGCYC
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: QP_IPERTURB,RBY_MSN
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: QP_RPERTURB,RBY_INIAXIS
C
      my_real
     .  EANIT2(10),COST_R2R,TOTMAS,TOTMAS1
      TARGET :: EANIT2
C OpenMP specific
       INTEGER ITASK, NP
#if defined(_OPENMP)
       INTEGER OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
       EXTERNAL OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
#endif

      my_real
     .       , DIMENSION(:), ALLOCATABLE ::
     .   FXBRPM, FXBMOD, FXBGLM, FXBCPM, FXBCPS, FXBLM,
     .   FXBFLS, FXBDLS, FXBDEP, FXBVIT, FXBACC, FXBSIG,
     .   FXBGRVR, EIGRPM ,RMSTIFN, RMSTIFR,STIFFN,
     .   MS_PLY0, ZI_PLY0,MSZ20,MSZ2,LELX,FXB_MATRIX
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: MBUFEL, MDEPL,RNOISE
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RFLOW,CMERGE,DNULL
      my_real
     .   PROBINT, FLREC6(6), DSCUTFRQ
      my_real, DIMENSION(:), ALLOCATABLE ::
     .   XFILTR,STFAC,FRIC_P,FRIGAP,I2RUPT,AREASL,THK_PART,
     .   GEO_STACK

      my_real, DIMENSION(:,:,:), ALLOCATABLE :: XREFC,XREFTG,XREFS
      my_real, DIMENSION(:), ALLOCATABLE :: XYZREF

      my_real, DIMENSION(:), ALLOCATABLE :: DT_R2R
      TYPE(INTERSURFP)   , DIMENSION(:,:), ALLOCATABLE :: INTERCEP
      my_real DTNODA
      my_real, DIMENSION(:), ALLOCATABLE :: SH4ANG,SH3ANG
      my_real, DIMENSION(:), ALLOCATABLE :: MS_B,IN_B,DTELEM
C
      TYPE (STACK_PLY)  :: STACK
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDRAPEID,PERTURB
      TYPE (FVM_INIVEL_STRUCT), DIMENSION(:), ALLOCATABLE :: FVM_INIVEL
      TYPE (FAILWAVE_STR_) :: FAILWAVE
      TYPE (NLOCAL_STR_)   :: NLOC_DMG
      TYPE (PINCH)         :: PINCH_DATA
      TYPE (DRAPE_)   ,DIMENSION(:), ALLOCATABLE   :: DRAPE,DRAPE_WRK
      TYPE (DRAPEG_)                               :: DRAPEG
      TYPE(DRAPE_WORK_) ,DIMENSION(:), ALLOCATABLE   :: IWORK_T
C    ! DEF_SHELL / ioffset=1 treatment for contact  
      INTEGER, DIMENSION(:),     ALLOCATABLE         :: ITAGOSET !Offset  
      my_real, DIMENSION(:),     ALLOCATABLE, TARGET :: XYZ    
      my_real, DIMENSION(:),     POINTER             :: X_C ! points to X or XYZ 
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: EBCS_TAG_CELL_SPMD 
C
      TYPE (DYNAIN_DATABASE)   :: DYNAIN_DATA
      TYPE (INTERFACES_)       :: INTERFACES
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      INTEGER LWAMP_L     !< Size of work array in Engine (WA)
      INTEGER LWANMP_L    !< Size of work array in Engine (WA)
C-----------------------------------------------
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CWIN
      LOGICAL :: IS_AVAILABLE
      INTEGER NLINES,NUSERWI,USERWI_ID
      INTEGER SCR_FILE_NAME_LEN
      CHARACTER(LEN=ncharkey) :: KEY
      INTEGER NUSPHCEL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IXSPS
      LOGICAL MAT20_DISCRETE_FILL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FLAG_ELEM_INTER25     
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_NIN25
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISKEW_TMP
      my_real, DIMENSION(:), ALLOCATABLE :: SKEW_TMP
      INTEGER NE_SFEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: IN_SFEM
C-----------------------------------------------
C Model Checker Memory
      DATA IUN/1/
C======================================================================|
C   Allocations MA (Entiers)
C======================================================================|
      TYPE :: int_ptr_array
        INTEGER, DIMENSION(:), POINTER :: ptr
      END TYPE int_ptr_array
      TYPE(int_ptr_array) :: IBUFTMP(0:1),NIGE_TMP(0:1)
      TYPE :: real_ptr_array
      my_real, DIMENSION(:), POINTER :: ptr2
      END TYPE real_ptr_array
      TYPE(real_ptr_array) :: RIGE_TMP(0:1),XIGE_TMP(0:1),
     .                        VIGE_TMP(0:1)
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .    FR_IAD,FUNCRYPT,
     .    IWORK,ITRI,KSYSUSR,PTSHEL,PTSH3N,PTSOL,PTQUAD,
     .    PTSPH,ISPTAG,DD_TMP,ITAG,ITAGND_SHXFEM,
     .    ITHPART,ITHSUB,ITHBUFTMP,DD_TMP2,
     .    IADHF,JCIHF,JLL,IWA,WEIGHT_RM,
     .    PTSPRI,PTBEAM,PTTRUSS
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPARGTMP
      TARGET :: IWORK

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_UMP_LOC
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: TAB_UMP_LOC2
      INTEGER, DIMENSION(:), POINTER ::
     .    IPARTTH,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,IPARTR,
     .    IPARTG,IPARTX,IPARTSP,NPC1,IXS10,IXS20,IXS16,IPRES,
     .    IBMPC2,IBMPC3,IBMPC4,IWORK2,
     .    ITRI1,ITRI2,ITRI3,INDEX,
     .    INDEX1,INDEX2,IWD,IWEIG,INUM,EADD,ITR1,ITR2,XEP,
     .    IPARTTHI,IPARTIG3D,IEDGESH4,IEDGESH3,IELCRK4,IELCRK3
      INTEGER BID13(1),SNPC1
      INTEGER L0,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,SIXTG0,
     .    SIXS0,SIXS10,SIXS20,SIXS16,SIWORK,SIWORK2,SIEXTAG,
     .    NUMCLD,NUMPRES,NUMLINK,NUMGRAV,NUMRBYMOU,
     .    SNRBODY,SLRBODY,LITHPART,LITHSUB,IDX,IDX1,IDX2,IDXCNT,
     .    LITHBUFI,LITHBUFMX,NTHGRPMX,SINDEX,SITRI,LDD_IAD,
     .    LSIGI,LSIGSH,LSIGSP,LSIGSPH,SINSCR,
     .    SIPART0,SIPARTTH,SIPARTS,SIPARTQ,SIPARTC,SIPARTT,SIPARTP,
     .    SIPARTR,SIPARTG,SIPARTX,SIPARTSP,ITER,
     .    LIXINT,SBUFALE,NVARTOT,
     .    NVARTOT0,NVARTOTMAX,NVARABF,
     .    PIXS10,PIXS16,PIXS20,NUMCFIELD,NUMLOADP,IXEL,
     .    SIPARTIG3D,IDXIGE1,IDXIGE2,
     .    IDXIGECNT,TAGSURFIGE,LSIGRS,LSIGBEAM,LSIGTRUSS,NSETFRICTOT,
     .    IORTHFRICMAX,COEFSLEN,NPFRICORTH,NGRPF,LENG,
     .    NIMPDISP,NIMPVEL,NIMPACC,NIMPV_LAGM,NFV0,NSETMAX,NFXVEL0
      TYPE(CLUSTER_),DIMENSION(:),ALLOCATABLE :: CLUSTERS
      TYPE(INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB
      TYPE(SCRATCH_STRUCT_),DIMENSION(:),ALLOCATABLE :: INSCR
C---  Bolt preloading
      INTEGER SIPRELOAD, SPRELOAD  !NUMPRELOAD,

c---  Element Buffer  --------------------------------------------------
      TYPE(ELBUF_STRUCT_),DIMENSION(:)  ,ALLOCATABLE :: ELBUF_TAB
      TYPE(ELBUF_STRUCT_),DIMENSION(:,:),ALLOCATABLE :: XFEM_TAB
      TYPE(MLAW_TAG_)    ,DIMENSION(:)  ,ALLOCATABLE, TARGET :: MTAG_INI,MTAG_R2R
      TYPE(MLAW_TAG_)    ,DIMENSION(:)  ,POINTER     :: MLAW_TAG
      TYPE(EOS_TAG_)     ,DIMENSION(:)  ,ALLOCATABLE :: EOS_TAG
      TYPE(PROP_TAG_)    ,DIMENSION(0:MAXPROP)       :: PROP_TAG
      TYPE(FAIL_TAG_)    ,DIMENSION(:,:),ALLOCATABLE :: FAIL_TAG
c---- Xfem ------------------------------------------------------------
      TYPE (XFEM_SHELL_) ,DIMENSION(:), ALLOCATABLE :: CRKSHELL     ! NLEVMAX
      TYPE (XFEM_LVSET_) ,DIMENSION(:), ALLOCATABLE :: CRKLVSET     ! NLEVMAX
      TYPE (XFEM_SKY_)   ,DIMENSION(:), ALLOCATABLE :: CRKSKY       ! NLEVMAX
      TYPE (XFEM_AVX_)   ,DIMENSION(:), ALLOCATABLE :: CRKAVX       ! NLEVMAX
      TYPE (XFEM_EDGE_)  ,DIMENSION(:), ALLOCATABLE :: CRKEDGE      ! NXLAYMAX
      TYPE(XFEM_PHANTOM_),DIMENSION(:), ALLOCATABLE :: XFEM_PHANTOM ! NXLAYMAX
C     NCRKPART & IND_CRK are global values shared by all processors (only for ANIM)
C ---- Interface t8
      TYPE(INT8_STRUCT_)      , DIMENSION(:,:), ALLOCATABLE :: INTERT8
      INTEGER  NCRKPART
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_CRK
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERMIGE
      TYPE(TABCONPATCH_IG3D_), DIMENSION(:), ALLOCATABLE :: TABCONPATCH
c---  Material data  --------------------------------------------------
      TYPE(MATPARAM_STRUCT_) , DIMENSION(:), ALLOCATABLE , TARGET :: MPARAM_INI,MPARAM_R2R
c---  Element group parameter table  --------------------------------------------------
      TYPE(GROUP_PARAM_) , DIMENSION(:), ALLOCATABLE :: GROUP_PARAM_TAB   ! NGROUP
C AMS
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: T2MAIN_SMS
C
      INTEGER :: SRNOISE1,SRNOISE2
C T2 SPT 27/28
      INTEGER  NSN_MULTI_CONNEC
      INTEGER, ALLOCATABLE, DIMENSION(:) :: T2_NB_CONNEC
C MERGE RBODY
      INTEGER, DIMENSION(:), ALLOCATABLE :: MGRBY
C PINCHING
      INTEGER SPINCH
C For /H3D/STRESS/TENS/OUTER
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TAG_SKINS6
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: SEATBELT_SHELL_TO_SPRING
C---  Bolt preload/axial
      INTEGER                                  :: NPRELOAD_A
      INTEGER, ALLOCATABLE, DIMENSION(:)       :: ITAGPRLD_SPRING
      INTEGER, ALLOCATABLE, DIMENSION(:)       :: ITAGPRLD_BEAM
      INTEGER, ALLOCATABLE, DIMENSION(:)       :: ITAGPRLD_TRUSS
      TYPE(PREL1D_), DIMENSION(:), ALLOCATABLE :: PRELOAD_A
      INTEGER, ALLOCATABLE, DIMENSION(:)       :: IPRELOAD_FUN    !  workarray fun_id,sen_id
C======================================================================|
C   Allocations AM (Reels)
C======================================================================|
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DBRWORK
      my_real, DIMENSION(:), ALLOCATABLE ::
     .    RWORK,STIFINT,MWA,MSS,MSSX,MSSF,MSQ,MSR,
     .    INP,INR,INS,VNS,VNSX,STC,STT,STP,STR,STTG,STUR,
     .    BNS,BNSX,VOLNOD,BVOLNOD,ETNOD,NSHNOD,XELEMWA,
     .    XNUM,XTMP,RTRANS,MSIG3D,STIFINTR,STRC,STRR,STRP,STRTG,
     .    VNIGE,BNIGE
      TARGET ::
     .    RWORK
      my_real, DIMENSION(:), POINTER ::
     .    THKEC,EANIT,PRES,WMA
      INTEGER SRWORK
      INTEGER LXINTD
      my_real TOTADDMAS
      CHARACTER(LEN=NCHARLINE) ::ERR_MSG
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBORDEDGE,INOM_OPT
      DOUBLE PRECISION RSIBUFSSG,RNIGE,RRIGE,RXIGE,RVIGE
      INTEGER INTMAX, LLINAL,ITET4_10
C---------      Itet=2 of S10
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICNDS10,ITAGND,ADDCNCND,
     .                                      CNCND, CELCND, CEPCND

      !Pointer to send a valid explicit address as argument in cas of not allocated
      INTEGER(KIND=8)   ,POINTER :: pMEMFLOW

      DATA INTMAX /2147483647/
      my_real,
     .         DIMENSION(:), ALLOCATABLE :: FILLSOL
C
C Dynamical User Library
      CHARACTER FILNAM*512,CLAW*4
      INTEGER LEN_FILNAM
      INTEGER IADBOXMAX
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADBOXMAX_NODE,IADBOXMAX_SURF,
     .   IADBOXMAX_LINE,IADBOXMAX_ELEM
C
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .    SIGI,SIGSH,SIGSP,SIGSPH,SIGRS,SIGBEAM,SIGTRUSS
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .    STRSGLOB,STRAGLOB,ORTHOGLOB
      INTEGER  ISIGSH,IYLDINI,KSIGSH3,FAIL_INI(5),IUSOLYLD,IUSERL
C
      INTEGER FVMAIN(NVOLU + NMONVOL),NBSUBMAT
C GROUPS OF GROUPS
      INTEGER :: MEM_MARGIN
      PARAMETER (MEM_MARGIN = 250000)
C--- INterface Friction model
      TYPE(INTBUF_FRIC_STRUCT_), DIMENSION(:), ALLOCATABLE ::  INTBUF_FRIC_TAB
      INTEGER, DIMENSION(:), ALLOCATABLE ::TABCOUPLEPARTS_FRIC_TMP,TABPARTS_FRIC_TMP,
     .                                     TAGPRT_FRIC,NSETINIT,IFRICORTH_TMP,
     .                                      PFRICORTH ,IREPFORTH ,LENGRPF
      my_real, DIMENSION(:), ALLOCATABLE ::TABCOEF_FRIC_TMP , VFORTH ,PHIFORTH
      TYPE(INIMAP1D_STRUCT), DIMENSION(:), ALLOCATABLE :: INIMAP1D
      TYPE(INIMAP2D_STRUCT), DIMENSION(:), ALLOCATABLE :: INIMAP2D
      TYPE(FUNC2D_STRUCT), DIMENSION(:), ALLOCATABLE :: FUNC2D
      TYPE(PYTHON_) :: PYTHON 
!       DDSPLIT local arrays :
      LOGICAL :: FLAG_24_25
      INTEGER :: NINDX_NM,NINDX_SCRT,I24MAXNSNE2
      INTEGER, DIMENSION(NSPMD) :: NUMNOD_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_NM,INDX_NM
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SCRATCH,INDX_SCRT
C FLEXIBLE BODY
      INTEGER  FXB_LAST_ADDRESS(10)
      CHARACTER, DIMENSION(:), ALLOCATABLE :: FXBFILE_TAB*2148
C
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_XXX : size = NUMNOD
!                 index of non-zero TAG_XXX value
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_XXX : number of non-zero TAG_XXX value
!       TAG_XXX : size = NUMNOD
!                array used to tag an element for
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       FLAG_24_25 : logical, flag for interface 24 or 25
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SKN
      TYPE(SKEW_) :: SKEWS
      TYPE (SENSOR_STR_) ,DIMENSION(:) ,ALLOCATABLE :: SENSOR_TMP
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       TAG_SKN : integer ; dimension=NUMSKW+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING
!                 tag array>1 --> the SKEW is used by several SPRING
!                 tag array <0 --> the SKEW is used by several options (has to be duplicated to all domains that have the nodes)
!       SKEWS : SKEW_ ; SKEW Type 
      !                 SKEWS%MULTIPLE_SKEW : dimension=NUMSKW+1
!                       %MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stick
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(SURF_), DIMENSION(:,:), ALLOCATABLE :: IGRSURF_PROC
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER :: GRNOD_UID
        INTEGER, DIMENSION(NSPMD) :: SIZE_ALE_ELM
        TYPE(split_cfd_type), DIMENSION(:),ALLOCATABLE :: ALE_ELM
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       SIZE_ALE_ELM : integer ; dimension=NSPMD ; size of ALE_ELM%SOL_ID array
!       ALE_ELM : split_cfd_type ; dimension=NSPMD ; solid element ID used
!                 during the domain splitting (ALE part)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER :: LEN_TMP_NAME
        CHARACTER(len=4096) :: TMP_NAME
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       LEN_TMP_NAME : integer ; size of TMP_NAME
!       TMP_NAME : character ; local name of file, when -outfile or
!                  -infile cdl are used, need to define the folder paths
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(MONVOL_STRUCT_), DIMENSION(:), ALLOCATABLE :: T_MONVOL
        TYPE(MONVOL_METADATA_) :: T_MONVOL_METADATA
        TYPE(t_ale_connectivity) :: ALE_CONNECTIVITY

        INTEGER :: NBR_TH_MONVOL,NBR_TH_MONVOL01(9)! number of /TH/MONV
c For /RANDOM --------------------------------------------------
      INTEGER,DIMENSION(:),ALLOCATABLE :: IRAND
      my_real,DIMENSION(:),ALLOCATABLE :: ALEA,XSEED

!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       BOOL_ALE_TG : logical, true only if 2d model + MULTI_FVM used
!       INDX_xxx : integer ; dimension=NUMELxxx ; index for the surface
!                  of the remote connected element
!       FACE_ELM_xxx : integer ; dimension=(6/4/3*NUMELxxx,2) ; surface
!                  of the remote connected element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        LOGICAL :: BOOL_ALE_TG
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_S,INDX_Q,INDX_TG
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       INV_GROUP : structure ; connectivity ELEMENT -> PART
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      ! -------------
      ! Load structure
      TYPE (LOADS_) :: LOADS ! global structure for /LOAD
      TYPE (LOADS_), DIMENSION(NSPMD) :: LOADS_PER_PROC ! local structurfsdcod(e for each processor --> used for the restart operation
      ! -------------
      ! BCS wall structure
      TYPE (bcs_struct_), DIMENSION(NSPMD) :: BCS_PER_PROC
      ! -------------
      INTEGER NINIVELT
      INTEGER NINTEMP
      INTEGER ALE_EULER

C MERGE NODES
      INTEGER NMERGE_NODE_CAND,NMERGE_NODE_DEST,NMERGE_TOT
      INTEGER, DIMENSION(:), ALLOCATABLE :: MERGE_NODE_TAB
      my_real,DIMENSION(:),ALLOCATABLE :: MERGE_NODE_TOL
C
      my_real,DIMENSION(:),ALLOCATABLE :: DGAPINT, INTGAPLOADP ,DGAPLOADINT
C SEATBELTS
      INTEGER NB_MAT_SEATBELT,NB_MAT
C   
      INTEGER  :: NUMSH3,NUMSH4, NSLICE,NPT_DRAPE,JJ,ISL, IP,IE, IDSHEL
      INTEGER , DIMENSION(:), ALLOCATABLE :: INDXSH  
      INTEGER :: NUMBER_LOAD_CYL
      INTEGER :: S_NOD2ELS,S_NOD2ELTG,S_NOD2EL1D
C DAMPING   
      INTEGER :: NDAMP_VREL_RBY
      INTEGER :: NDAMP_FREQ_RANGE
      INTEGER , DIMENSION(:), ALLOCATABLE :: DAMP_RANGE_PART  
C SPH
      INTEGER :: PRE_SEARCH,SZ_INTP_DIST 
      my_real :: MAX_INTP_DIST_PART

      TYPE(INTER_CAND_) :: INTER_CAND
      TYPE (FAIL_FRACTAL_)  :: FAIL_FRACTAL
      TYPE (FAIL_BROKMANN_) :: FAIL_BROKMANN
      INTEGER DEF_INTER(100)
      ! constraint structure
      type(constraint_) :: constraint_struct
C RWALL PENALTY   
      INTEGER :: SLN_PEN
      my_real, DIMENSION(:), ALLOCATABLE :: RWSTIF_PEN 
      ! drape flag
      INTEGER :: ITHIN_DRAPE

      integer, parameter :: nb_key = 8 
      integer :: iwarnhb,n_entity
      integer :: sh4tree_dim1,sh4tree_dim2,sh3tree_dim1,sh3tree_dim2
      type(elm_group_) :: elm_group
c=======================================================================
      IREAC = 0 
      PYTHON%NB_FUNCTS = 0
      PYTHON%NB_SENSORS = 0
!     domain decomposition statistic
      DDSTAT(1:50,1:PARASIZ)=0
      I22LEN_L = 0 
      FVMAIN(1:NVOLU + NMONVOL) = 0
      M51_IFLG6 = 0
      NVARTOTMAX = 0
      ERR_MSG='BEGINNING'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      FLAG_GOTO = 0
      CALL TITRE2
      NVARABF = 1
      INTBAG=0
      L_MUL_LAG=0
      LAG_NCF = 0
      LAG_NKF = 0
      LAG_NHF = 0
      LAG_NCL = 0
      LAG_NKL = 0
      LAG_NHL = 0
      NUMELS8A = 0
      NAIRWA = 0
      NMANIM=0
      DSANIM=0
      IMPL_S0 = 0
      FLG_SPLIT = 0
      NVARTOTMAX = 0
      NXLAYMAX = 0   ! max layer nb in parts xfem
C     /DAMP/VREL + RBY      
      NDAMP_VREL_RBY = 0
C     /DAMP/FREQUENCY_RANGE      
      NDAMP_FREQ_RANGE = 0
C ply xfem
      IPLYXFEM  = 0
      NPLYXFE = 0
      EPLYXFE = 0
      INTPLYXFEM = 0
C
      INTER_ITHKNOD=0  !defined in interface module (common_source directory)
      IRIGID_MAT = 0
C
!      IKINE1LAG = 0
      IALELAG = 0
C  added nodal mass
      TOTADDMAS = ZERO
      IPART_STACK = 0
      IPART_PCOMPP = 0
C
      SFRONTB_R2R = 1
C Flag to set for Domain Decomposition and Additional nodes
      USER_GRP_DOMAIN=0
      NSNT=0
      NMNT_2=0
      DEF_INTER(1:100) = DEFAULTS%interface%DEF_INTER(1:100)
c OpenMP specific
      ITASK=0
C     flag need generalize BUFINTI in DDPSLIT with Interface type 11.
C     BEFORE ININTR / I11STO KD(11)=KD(10)+4*NRTS
C     After ININTR :         KD(11)=KD(10)+2*NRTS
C     Idem with  (KD(12) = KD(11) + 4*NRTM )
      I11FLAG=0
      INTER18_AUTOPARAM = 0
      INTER18_IS_VARIABLE_GAP_DEFINED = .FALSE.
      NBPRELD = 10
      CALL MY_ALLOC(EBCS_TAG_CELL_SPMD,NUMELQ+NUMELTG+NUMELS,"EBCS_TAG_CELL_SPMD")
C
      ALLOCATE(EOS_TAG(0:MAXEOS))
C Interface give CPU to Main surface - INITIALIZE ARRAY
C new initiation for XFEM CRACK
C
      allocate(INTERCEP(3, NINTER))

      DO I=1,NINTER
        NULLIFY(INTERCEP(1,I)%P)
        NULLIFY(INTERCEP(2,I)%P)
        NULLIFY(INTERCEP(3,I)%P)
      ENDDO
C------------------------------------------------------------------
C Initializations of NNOISE variables (for Rad2noise Engine files
C------------------------------------------------------------------
      NNOISE_SAV = 0
      SINOISE = 0
      SFNOISE = 0
      CALL MY_ALLOC(INOISE,0,"INOISE")
      CALL MY_ALLOC(FNOISE,0,"FNOISE")
C------------------------------------------------------------------
C Initialization size for INISHCEL
C------------------------------------------------------------------
      NUSPHCEL = 0
C----------------------------------------------
C     ALLOCATION TO REDUCE STACKSIZE
C----------------------------------------------
C INTEGER
      CALL MY_ALLOC(IWCONT,5*NUMNOD,"IWCONT")

      CALL MY_ALLOC(IWCIN2,2*NUMNOD,"IWCIN2")

C
      CALL MY_ALLOC(IKINE1LAG,3*NUMNOD,"IKINE1LAG")
        IKINE1LAG(1:3*NUMNOD)=0

      CALL MY_ALLOC(DSDOF,NUMNOD,"DSDOF")
      DSDOF(1:NUMNOD)=0

      ALLOCATE( ADDCNE(0:NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ADDCNE')
C ADDCNE_PXFEM needed when IPLYXFEM used
      ALLOCATE(ADDCNE_PXFEM(0:NUMNOD +1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='ADDCNE_PXFEM')
C
      IF(NFXBODY>0) THEN
        NBIPM = 45
        call my_alloc(FXBTAG, NUMNOD, "FXBTAG", stat=stat)
        call my_alloc(FXBIPM, NBIPM*NFXBODY, "FXBIPM", stat=stat)
        FXBIPM(1:NBIPM*NFXBODY) = ZERO
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FXBTAG')
        ! Table of FXBODY file name for QAPRINT
        allocate(FXBFILE_TAB(NFXBODY))
      ELSE
        NBIPM = 1
        call my_alloc(FXBTAG, 1, "FXBTAG", stat=stat)
        call my_alloc(FXBIPM, 1, "FXBIPM", stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FXBTAG')
        allocate(FXBFILE_TAB(0))
      ENDIF
C
      CALL MY_ALLOC(ISOLNOD,NUMELS,"ISOLNOD")
      CALL MY_ALLOC(ISOLOFF,NUMELS,"ISOLOFF")
      CALL MY_ALLOC(ISHEOFF,NUMELC,"ISHEOFF")
      CALL MY_ALLOC(ITRUOFF,NUMELT,"ITRUOFF")
      CALL MY_ALLOC(IPOUOFF,NUMELP,"IPOUOFF")
      CALL MY_ALLOC(IRESOFF,NUMELR,"IRESOFF")
      CALL MY_ALLOC(ITRIOFF,NUMELTG,"ITRIOFF")
      CALL MY_ALLOC(IGRNRBY,NRBODY,"IGRNRBY")
      IGRNRBY(1:NRBODY) = 0
      CALL MY_ALLOC(IQUAOFF,NUMELQ,"IQUAOFF")
C Float
C----------------------------------------------
C     ALLOC INTEGER TABLES IGEO and IPM
C----------------------------------------------
      LEN_G = NPROPGI*NUMGEO
      LEN_M = NPROPMI*NUMMAT
      CALL MY_ALLOC(IGEO,LEN_G,"IGEO")
      CALL MY_ALLOC(IPM,LEN_M,"IPM")
      IGEO = 0
      IPM  = 0
C----------------------------------------------
C      ALLOC DDWEIGHTS ARRAY FROM MODULE
C----------------------------------------------
      CALL INIT_MAT_WEIGHT(NUMMAT)
C----------------------------------------------
      CALL MY_ALLOC(IPART_STATE,NPART,"IPART_STATE")
        IPART_STATE=0
      CALL TRACE_OUT1()
C----------------------------------------------
C     ALLOC AND INIT OF TAG TABLE FOR NODES USED ON P0
C----------------------------------------------
      ERR_CATEGORY='INTERNAL'
c obsolete code, replaced by new chained-list IFRONT
C     coding of different types of boundary nodes:
C     0 node not on proc
C     1 acceleration boundary
C     10 kinematic boundary
C     100 interface boundary
C     possible combinations


c SIFRONT minimum size NUMNOD. Value set to 2*NUMNOD
      SIFRONT = 2*NUMNOD

c Linked-list IFRONT
C IFRONT%IENTRY : entry in IFRONT for node N
C IFRONT%P(1,N) : SPMD domain for node N
C IFRONT%P(2,N) : next index in IFRONT for node N
      CALL MY_ALLOC(IFRONT%P,2,SIFRONT,"IFRONT%P")
      CALL MY_ALLOC(IFRONT%IENTRY,NUMNOD,"IFRONT%IENTRY")

c IENTRY2 use to save IENTRY
      CALL MY_ALLOC(IENTRY2,NUMNOD,"IENTRY2")
c FLAGKIN array to identify boundary nodes with kinematic constraints
c (FLAGKIN(N)=1 <=> old FRONT TAG=10)
      CALL MY_ALLOC(FLAGKIN,NUMNOD,"FLAGKIN")
C----------------------------------------------
      CALL INI_IFRONT()
      IENTRY2(1:NUMNOD) = -1
      FLAGKIN(1:NUMNOD) = 0

c      IF(FLAG_GOTO == 1) GOTO 207 !!go to traitement rad2rad
C--------------------------------------------
C     CALCULATE ISECUT
C--------------------------------------------
      ISECUT=0
      CALL LECSEC0(LSUBMODEL)
C--------------------------------------------
C     IMPOSED VELOCITIES : Check rotational DOFs : IMPOSE_DR
C--------------------------------------------
      IMPOSE_DR=0
      CALL HM_PREREAD_IMPVEL0(IMPOSE_DR,UNITAB,LSUBMODEL) !read /IMPDISP
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (GENERAL) REEL
C--------------------------------------------
      IFRWV=0
208   SX  = 3*NUMNOD
      SD  = 5*NUMNOD
      SV  = 3*NUMNOD
      SVR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
      SVR_1 = NUMNOD*MAX(IRODDL,IRODDL0)
      STHKE = NUMELC+NUMELTG
      SMS = NUMNOD
      SPINCH= NPINCH
      SIN = NUMNOD*MAX(IRODDL,IRODDL0)
      IF(ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR>0 .OR. IDROT == 1) THEN
        SDR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
      ELSE
        SDR = 0
      ENDIF 
      IF(FLAG_GOTO == 1) GOTO 258
C--------------------------------------------
      IF(NDAMP > 0) THEN
        SDAMPR = NRDAMP*NDAMP
        SDAMP  = 3*(1+MAX(IRODDL,IRODDL0))*NUMNOD
        CALL HM_OPTION_COUNT('/DAMP/FREQUENCY_RANGE',NDAMP_FREQ_RANGE)
        IF (NDAMP == NDAMP_FREQ_RANGE) SDAMP = 0
        CALL MY_ALLOC(DAMPR,SDAMPR+SDAMP,"DAMPR")
        DAMP  => DAMPR(SDAMPR+1:SDAMPR+SDAMP)
        DAMPR = 0
      ELSE
        SDAMPR = 0
        SDAMP  = 0
        CALL MY_ALLOC(DAMPR,SDAMPR,"DAMPR")
        CALL MY_ALLOC(DAMP,SDAMP,"DAMP")
      ENDIF
      CALL MY_ALLOC(DAMP_RANGE_PART,NPART,"DAMP_RANGE_PART")
      DAMP_RANGE_PART = 0
      CALL MY_ALLOC(X,SX,"X")
      CALL MY_ALLOC(D,SD,"D")
      CALL MY_ALLOC(V,SV,"V")
      CALL MY_ALLOC(VR,SVR,"VR")
      CALL MY_ALLOC(DR,SDR,"DR")
      CALL MY_ALLOC(THKE,STHKE,"THKE")
      CALL MY_ALLOC(MS,SMS,"MS")
      CALL MY_ALLOC(IN,SIN,"IN")
      CALL MY_ALLOC(XYZREF,SX,"XYZREF")
      CALL MY_ALLOC(SH4ANG,NUMELC,"SH4ANG")
      CALL MY_ALLOC(SH3ANG,NUMELTG,"SH3ANG")
      IF(NUMNOD > 0) THEN
        X  = 0
        D  = 0
        V  = 0
        MS = 0
      ENDIF
      IF(SVR > 0) VR = 0
      IF(SDR > 0) DR = 0
      IF(SIN > 0) IN = 0
C--------------------------------------------
258   IF(NUMELC<STHKE) THEN
        THKEC => THKE(NUMELC+1:STHKE)
      ELSE
        THKEC => THKE
      END IF
      IF ((STHKE > 0).AND.(FLAG_GOTO==0)) THKE = 0
      IF(NUMELC > 0) SH4ANG = 0
      IF(NUMELTG > 0) SH3ANG = 0
c
      CALL NBFUNCT(NFUNCT,NTABLE,NPTS,LSUBMODEL)
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (GENERALE) ENTIER
C--------------------------------------------
C---  Longueurs
      SICODE  = NUMNOD
      SISKEW  = NUMNOD
      SISKWN   = LISKN*((NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+(NUMFRAM+1)+NSUBMOD)
      SIFRAME = LISKN*(NUMFRAM+1)
c     SNETH = 2*NSNOD+NSELS+NSELQ+NSELC+NSELT+NSELP+NSELR+NSELTG = 0
      SIBCSLAG= 5*NBCSLAG
      SIPART0 = LIPART1*NPART+LIPART1*NTHPART
      SIPARTTH= 2*9*NPART+2*9*NTHPART
      SIPARTS = NUMELS
      SIPARTQ = NUMELQ
      SIPARTC = NUMELC
      SIPARTT = NUMELT
      SIPARTP = NUMELP
      SIPARTR = NUMELR
      SIPARTG = NUMELTG
      SIPARTX = NUMELX
      SIPARTSP= NUMSPH
      SIPARTIG3D = NUMELIG3D
      SIPART  = SIPART0+SIPARTTH+SIPARTS+SIPARTQ+SIPARTC+SIPARTT+SIPARTP
     .        + SIPARTR+SIPARTG+SIPARTX+SIPARTIG3D+SIPARTSP
      NUMEL   = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
     .        + NUMELTG+NUMELX+NUMSPH+NUMELIG3D
c
      SNPC    = 3*NFUNCT+1
      SIXTG0  = NIXTG*NUMELTG
      SIXTG   = SIXTG0
      SIXS0   = NIXS*NUMELS
      SIXS10  = NUMELS10*6
      SIXS20  = NUMELS20*12
      SIXS16  = NUMELS16*8
      SIXS    = SIXS0+SIXS10+SIXS20+SIXS16
      SIXQ    = NIXQ*NUMELQ
      SIXC    = NIXC*NUMELC
      SIXT    = NIXT*NUMELT
      SIXP    = NIXP*NUMELP
      SIXR    = NIXR*NUMELR
      SITAB   = NUMNOD
      SITABM1 = 2*NUMNOD
      SGJBUFI = LKJNI*NGJOINT
!---------
      SLACCELM= 3*NACCELM
      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NMONVOL+NINTER+NINTSUB+
     +          NRWALL+NJOINT+NSECT+NLINK+
     +          NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
     +          NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
     +          NGJOINT+NUNIT0+NFUNCT+NADMESH+
     +          NSPHIO+NSPCOND+NRBYKIN+NEBCS+
     +          NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
     +          NRBMERGE+NUMBCSN+NSLIPRING+NRETRACTOR+
     +          NGUIDED_CABLE
      SNOM_OPT = SNOM_OPT1*LNOPT1+1
      SINOM_OPT= 33
      SNOM_SECT= ncharline*NSECT
      IF(FLAG_GOTO==1) GOTO 209
C
C---  Allocations
      CALL MY_ALLOC(ICODE,SICODE,"ICODE")
      CALL MY_ALLOC(ISKEW,SISKEW,"ISKEW")
      CALL MY_ALLOC(ISKWN,SISKWN,"ISKWN")
      CALL MY_ALLOC(IBCSLAG,SIBCSLAG,"IBCSLAG")
      CALL MY_ALLOC(IPART,SIPART,"IPART")
      CALL MY_ALLOC(NPC,SNPC,"NPC")
      CALL MY_ALLOC(IXTG,SIXTG,"IXTG")
      CALL MY_ALLOC(IXS,SIXS,"IXS")
      CALL MY_ALLOC(IXQ,SIXQ,"IXQ")
      CALL MY_ALLOC(IXC,SIXC,"IXC")
      CALL MY_ALLOC(IXT,SIXT,"IXT")
      CALL MY_ALLOC(IXP,SIXP,"IXP")
      CALL MY_ALLOC(IXR,SIXR,"IXR")
      CALL MY_ALLOC(ITAB,SITAB,"ITAB")
      CALL MY_ALLOC(ITABM1,SITABM1,"ITABM1")
      CALL MY_ALLOC(GJBUFI,SGJBUFI,"GJBUFI")
      CALL MY_ALLOC(LACCELM,SLACCELM,"LACCELM")

      CALL MY_ALLOC(NOM_OPT,SNOM_OPT,"NOM_OPT")
      ALLOCATE(INOM_OPT(0:SINOM_OPT) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='INOM_OPT')
      CALL MY_ALLOC(NOM_SECT,SNOM_SECT,"NOM_SECT")
      CALL MY_ALLOC(IXR_KJ,5*(NUMELR+1),"IXR_KJ")
      CALL MY_ALLOC(IWORKSH,3,NUMELC+NUMELTG,"IWORKSH")
      IF(NUMELIG3D > 0) THEN
        CALL MY_ALLOC(WIGE,NUMNOD,"WIGE")
        DEG_MAX=0
      ELSE
        CALL MY_ALLOC(WIGE,0,"WIGE")
      ENDIF
      CALL MY_ALLOC(R_SKEW,NUMELR,"R_SKEW")
C
      CALL MY_ALLOC(IBEAM_VECTOR,NUMELP,"IBEAM_VECTOR")
      CALL MY_ALLOC(RBEAM_VECTOR,3,NUMELP,"RBEAM_VECTOR")
C
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)== 4.OR.ABS(ISIGI)==5)THEN
         call my_alloc(PTSHEL, NUMELC, "PTSHEL", stat=stat)
         PTSHEL = 0
         call my_alloc(PTSH3N, NUMELTG, "PTSH3N", stat=stat)
          PTSH3N = 0  
      ELSE
        call my_alloc(PTSHEL, 0, "PTSHEL", stat=stat)
        call my_alloc(PTSH3N, 0, "PTSH3N", stat=stat)
      END IF
C --- Initialisations
      IF(SICODE > 0) ICODE   = 0
      IF(SISKEW > 0) ISKEW   = 0
      IF(SISKWN > 0) ISKWN   = 0
      IF(SIBCSLAG > 0) IBCSLAG = 0
      IF(SIPART > 0) IPART   = 0
      IF(SNPC > 0) NPC     = 0
      IF(SITAB > 0) ITAB    = 0
      IF(SITABM1 > 0) ITABM1  = 0
      IF(SGJBUFI > 0) GJBUFI  = 0
      IF(SLACCELM > 0) LACCELM = 0
      IF(SNOM_OPT > 0) NOM_OPT = 0
      IF(SINOM_OPT > 0) INOM_OPT = 0
      IF(NUMELR > 0) IXR_KJ = 0
      IF(NUMELC+NUMELTG > 0) IWORKSH(:,:) = 0
      IF(NUMELR > 0) R_SKEW = 0
      IF(NUMELP > 0) IBEAM_VECTOR(1:NUMELP) = ZERO
      IF(NUMELP > 0) RBEAM_VECTOR(1:3,1:NUMELP) = ZERO
C
C---  Pointers: sub-arrays
      IF(SISKWN-SIFRAME<SISKWN) THEN
      IFRAME => ISKWN(SISKWN-SIFRAME+1:SISKWN)
      ELSE
        IFRAME => ISKWN
      END IF

      ! ---------------------
      INTER_CAND%S_IXINT_2 = 0
      CALL MY_ALLOC(INTER_CAND%ADDRESS,NINTER+1,"INTER_CAND%ADDRESS")
      INTER_CAND%ADDRESS(1:NINTER+1) = 0
      ! ---------------------

!--- NEW DATA STRUCTE ALLOCATION FOR GROUPS OF ENTITIES
      allocate(SUBSETS(NSUBS))
      SUBSETS(1:NSUBS)%ID=0
      allocate(IGRNOD(NGRNOD+NSETS))
      allocate(IGRBRIC(NGRBRIC+NSETS))
      allocate(IGRQUAD(NGRQUAD+NSETS))
      allocate(IGRSH4N(NGRSHEL+NSETS))
      allocate(IGRSH3N(NGRSH3N+2*NSETS))
      allocate(IGRTRUSS(NGRTRUS+NSETS))
      allocate(IGRBEAM(NGRBEAM+NSETS))
      allocate(IGRSPRING(NGRSPRI+NSETS))
      allocate(IGRPART(NGRPART+NSETS))
      allocate(IGRSURF(NSURF+NSETS))
      allocate(IGRSURF_PROC(NSURF+NSETS, NSPMD))
      allocate(IGRSLIN(NSLIN+NSETS))
      IGRNOD(1:NGRNOD+NSETS)%ID      = 0
      IGRNOD(1:NGRNOD+NSETS)%NENTITY = 0
      IGRNOD(1:NGRNOD+NSETS)%GRTYPE  = 0
      IGRNOD(1:NGRNOD+NSETS)%SORTED  = 0
      IGRNOD(1:NGRNOD+NSETS)%GRPGRP  = 0
      IGRNOD(1:NGRNOD+NSETS)%LEVEL   = 0
      IGRNOD(1:NGRNOD+NSETS)%R2R_ALL   = 0
      IGRNOD(1:NGRNOD+NSETS)%R2R_SHARE   = 0



      IGRBRIC(1:NGRBRIC+NSETS)%NENTITY = 0
      IGRQUAD(1:NGRQUAD+NSETS)%NENTITY = 0
      IGRSH4N(1:NGRSHEL+NSETS)%NENTITY = 0
      IGRSH3N(1:NGRSH3N+2*NSETS)%NENTITY = 0
      IGRTRUSS(1:NGRTRUS+NSETS)%NENTITY = 0
      IGRBEAM(1:NGRBEAM+NSETS)%NENTITY = 0
      IGRSPRING(1:NGRSPRI+NSETS)%NENTITY = 0
      IGRPART(1:NGRPART+NSETS)%NENTITY = 0
!--  variable initialization to not printout the empty SET groups
      IGRNOD(1:NGRNOD+NSETS)%SET_GROUP = 0
      IGRBRIC(1:NGRBRIC+NSETS)%SET_GROUP = 0
      IGRQUAD(1:NGRQUAD+NSETS)%SET_GROUP = 0
      IGRSH4N(1:NGRSHEL+NSETS)%SET_GROUP = 0
      IGRSH3N(1:NGRSH3N+2*NSETS)%SET_GROUP = 0
      IGRTRUSS(1:NGRTRUS+NSETS)%SET_GROUP = 0
      IGRBEAM(1:NGRBEAM+NSETS)%SET_GROUP = 0
      IGRSPRING(1:NGRSPRI+NSETS)%SET_GROUP = 0
      IGRPART(1:NGRPART+NSETS)%SET_GROUP = 0
      IGRSURF(1:NSURF+NSETS)%SET_GROUP = 0
      IGRSLIN(1:NSLIN+NSETS)%SET_GROUP = 0

      IGRSURF(1:NSURF+NSETS)%NSEG = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_IGE = 0
      IGRSURF(1:NSURF+NSETS)%SET_GROUP = 0
      IGRSURF(1:NSURF+NSETS)%NB_MADYMO = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_R2R_ALL = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_R2R_SHARE = 0
      IGRSURF(1:NSURF+NSETS)%EXT_ALL = 0
!--
!      IF(NBOX > 0)    CALL MY_ALLOC(IGRBOX,NBOX,"IGRBOX")
C     ipart
216   L0 = SIPART0
      L1 = L0 + SIPARTTH
      L2 = L1 + SIPARTS
      L3 = L2 + SIPARTQ
      L4 = L3 + SIPARTC
      L5 = L4 + SIPARTT
      L6 = L5 + SIPARTP
      L7 = L6 + SIPARTR
      L8 = L7 + 0
      L9 = L8 + SIPARTG
      L10= L9 + SIPARTX
      L11= L10+ SIPARTSP
      L12= L11+ SIPARTIG3D
      IF(L1>L0) THEN
      IPARTTH => IPART(L0+1:L1)
      ELSE
        IPARTTH => IPART
      END IF
      IF(L2>L1) THEN
      IPARTS => IPART(L1+1:L2)
      ELSE
        IPARTS => IPART
      END IF
      IF(L3>L2) THEN
      IPARTQ => IPART(L2+1:L3)
      ELSE
        IPARTQ => IPART
      END IF
      IF(L4>L3) THEN
      IPARTC => IPART(L3+1:L4)
      ELSE
        IPARTC => IPART
      END IF
      IF(L5>L4) THEN
      IPARTT => IPART(L4+1:L5)
      ELSE
        IPARTT => IPART
      END IF
      IF(L6>L5) THEN
      IPARTP => IPART(L5+1:L6)
      ELSE
        IPARTP => IPART
      END IF
      IF(L7>L6) THEN
      IPARTR => IPART(L6+1:L7)
      ELSE
        IPARTR => IPART
      END IF
      IF(L9>L8) THEN
      IPARTG => IPART(L8+1:L9)
      ELSE
        IPARTG => IPART
      END IF
      IF(L10>L9) THEN
      IPARTX => IPART(L9+1:L10)
      ELSE
        IPARTX => IPART
      END IF
      IF(L11>L10) THEN
      IPARTSP=> IPART(L10+1:L11)
      ELSE
        IPARTSP => IPART
      END IF
      IF(L12>L11) THEN
      IPARTIG3D=> IPART(L11+1:L12)
      ELSE
        IPARTIG3D => IPART
      END IF
      IF(FLAG_GOTO==1) GOTO 217
c
      IF(NFUNCT+2<=SNPC-NFUNCT) THEN
        NPC1   => NPC(NFUNCT+2:SNPC-NFUNCT)
        SNPC1 = SNPC-2*NFUNCT+1
      ELSE
        NPC1   => NPC
        SNPC1 = SNPC
      END IF

212      IF(SIXS0+SIXS10>SIXS0) THEN
      IXS10  => IXS(SIXS0+1:SIXS0+SIXS10)
      ELSE
c        IXS10  => IXS
c          NULLIFY(IXS10)
           CALL MY_ALLOC(IXS10,1,"IXS10")
      END IF
      IF(SIXS0+SIXS10+SIXS20>SIXS0+SIXS10) THEN
      IXS20  => IXS(SIXS0+SIXS10+1:SIXS0+SIXS10+SIXS20)
      ELSE
c        IXS20  => IXS
          CALL MY_ALLOC(IXS20,1,"IXS20")
      END IF
      IF(SIXS>SIXS0+SIXS10+SIXS20) THEN
      IXS16  => IXS(SIXS0+SIXS10+SIXS20+1:SIXS)
      ELSE
c        IXS16  => IXS
c         NULLIFY(IXS16)
          CALL MY_ALLOC(IXS16,1,"IXS16")
      END IF
      PIXS10 = MIN(SIXS,SIXS0+1              )
      PIXS20 = MIN(SIXS,SIXS0+SIXS10+1       )
      PIXS16 = MIN(SIXS,SIXS0+SIXS10+SIXS20+1)

      IF(FLAG_GOTO==1) GOTO 213
C
C     .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
210   INOM_OPT(1) = NRBODY
      INOM_OPT(2) = INOM_OPT(1) + NACCELM
      INOM_OPT(3) = INOM_OPT(2) + NVOLU + NMONVOL
      INOM_OPT(4) = INOM_OPT(3) + NINTER
      INOM_OPT(5) = INOM_OPT(4) + NINTSUB + NGUIDED_CABLE
      INOM_OPT(6) = INOM_OPT(5) + NRWALL
      INOM_OPT(7) = INOM_OPT(6) + 0 !obsolete option removed
      INOM_OPT(8) = INOM_OPT(7) + NJOINT
      INOM_OPT(9) = INOM_OPT(8) + NSECT
      INOM_OPT(10)= INOM_OPT(9) + NLINK
      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C
      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS + NUMBCSN
      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
      INOM_OPT(32)= INOM_OPT(31)+ NSLIPRING
      INOM_OPT(33)= INOM_OPT(32)+ NRETRACTOR
C
      IF(FLAG_GOTO==1) GOTO 211

      CALL ANODIN( NUMNOD)
      IF(NPART==0) THEN
        CALL APARTIN(NPART+1)
      ELSE
      CALL APARTIN(NPART)
      END IF
C--------------------------------------------
C     READING FUNCTIONS & TABLES
C--------------------------------------------
      ERR_MSG='FUNCTIONS & TABLES'
      ERR_CATEGORY='FUNCTIONS & TABLES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      PYTHON%nb_functs = 0
      IF(NFUNCT > 0 .OR. NTABLE > 0) THEN
C
C NTABLE = NFUNCT + ...
        WRITE(ISTDO,'(A)')' .. FUNCTIONS & TABLES'
        allocate(TABLE(NTABLE), stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TABLE')
        CALL TABLE_ZERO(TABLE)
        IF(NFUNCT > 0) THEN
          ERR_MSG='FUNCTIONS'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
          CALL MY_ALLOC(TF,NPTS,"TF")
          CALL MY_ALLOC(FUNCRYPT,NFUNCT,"FUNCRYPT")
          FUNCRYPT = 0
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TF')
          IF(NPTS > 0) TF = 0
          CALL HM_READ_FUNCT(NPC    ,TF     ,NFUNCT ,TABLE, NPTS,
     .                NOM_OPT(LNOPT1*INOM_OPT(20)+1) ,FUNCRYPT, UNITAB, LSUBMODEL)
          CALL TRACE_OUT1()
        END IF
C       TABLES
        ERR_MSG='TABLES 1'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        CALL HM_READ_TABLE1 (NTABLE, TABLE ,NFUNCT ,NPC ,TF ,
     .                NOM_OPT(LNOPT1*INOM_OPT(20)+1), UNITAB, LSUBMODEL)

        CALL HM_READ_FUNCT_PYTHON(PYTHON,NPC,SNPC,NFUNCT,LSUBMODEL,NSUBMOD,TF,NPTS,TABLE, NTABLE)
        CALL CHKFUNCT (NFUNCT, NPC,NOM_OPT(LNOPT1*INOM_OPT(20)+1))
        IF(NFUNCT > 0) THEN
          CALL HM_READ_MOVE_FUNCT(NPC    ,TF     ,NFUNCT ,TABLE, NTABLE,FUNCRYPT, UNITAB, LSUBMODEL)
          CALL MY_DEALLOC(FUNCRYPT)
        END IF
        CALL HM_READ_TABLE2 (NTABLE, TABLE ,NFUNCT , UNITAB, LSUBMODEL )
        CALL TRACE_OUT1()
      ELSE
C       TABLES
        ERR_MSG='TABLES 0'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        NPTS = 0
        call my_alloc(TF, NPTS, "TF", stat=stat)
        allocate(TABLE(0), stat=stat)
        CALL TRACE_OUT1()
      ENDIF


      STF =  NPTS

      CALL TRACE_OUT1()
C--------------------------------------------
C     READING 2D FUNCTIONS
C--------------------------------------------
      allocate(FUNC2D(NFUNC2D))
      IF(NFUNC2D > 0) THEN
         CALL HM_READ_FUNC2D(FUNC2D, LSUBMODEL, UNITAB)
      ENDIF
C--------------------------------------------
C     Dynamic storage (General) Reel - Suite
C-------------------------------------------
      ERR_MSG='DYNAMIC STORAGE'
      ERR_MSG='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SPM = NUMMAT*NPROPM
      SSKEW = LSKEW*(NUMSKW+1)
      IF(NSPCOND > 0) SSKEW = SSKEW + LSKEW*NUMSPH
      IF(NSUBMOD > 0) SSKEW = SSKEW + LSKEW*NSUBMOD
      SXFRAME = NXFRAME*(NUMFRAM+1)
      SSKEW = SSKEW + SXFRAME
      SGEO  = NUMGEO*NPROPG
      SEANI = NUMELS+NUMELQ+NUMELC+NUMELTG
      ISHIF = NUMELS+NUMELQ+NUMELC

      CALL MY_ALLOC(PM,SPM,"PM")
      CALL MY_ALLOC(GEO,SGEO,"GEO")
c in the skew, we put all the skews of the model
      CALL MY_ALLOC(SKEW,SSKEW,"SKEW")
      IF(SSKEW-SXFRAME<SSKEW) THEN
      XFRAME => SKEW(SSKEW-SXFRAME+1:SSKEW)
      ELSE
        XFRAME => SKEW
      END IF
c      ALLOCATE(XFRAME(SXFRAME) ,STAT=stat)
      CALL MY_ALLOC(EANI,SEANI,"EANI")
      IF(SEANI > 0) EANI = 0
      IF(NUMELTG > 0) THEN
        EANIT => EANI(ISHIF+1:SEANI)
      ELSE
        EANIT => EANIT2
      ENDIF
      call my_alloc(isolnod0, numels, "isolnod0")
      call my_alloc(ishell3nod0, numeltg, "ishell3nod0")
      PM   = 0
      SKEW = 0
      GEO  = 0
      ISHIF =NUMELS+NUMELQ+NUMELC
      CALL TRACE_OUT1()
C--------------------------------------------
      ERR_MSG='KINEMATIC INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL KININI(D      )
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING MATERIALS
C--------------------------------------------
      ERR_MSG='MATERIALS'
      ERR_CATEGORY='MATERIALS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SAV_BUF_POINT(NPC,5)
      CALL SAV_BUF_POINT(TF,6)
      WRITE(ISTDO,'(A)')TITRE(11)
      SRWORK  = MAX(NUMMAT*10000,1000000)
      SBUFMAT = 0
      call my_alloc(RWORK, SRWORK, "RWORK", stat=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='RWORK')
      IF(SRWORK > 0) RWORK = ZERO
c------------------------------------
      allocate(MTAG_INI(NUMMAT))
      allocate(MPARAM_INI(NUMMAT))
      CALL INIT_MLAW_TAG(MTAG_INI,NUMMAT)
      MAT_ELEM%MAT_PARAM(1:NUMMAT) => MPARAM_INI(1:NUMMAT)
      MLAW_TAG(1:NUMMAT) => MTAG_INI(1:NUMMAT)
      NLOC_DMG%IMOD = 0
      ALLOCATE(FAIL_TAG(NUMMAT,MAXFAIL))
c---------------------------------------------------------------
      CALL READ_MATERIAL_MODELS(
     .     MAT_ELEM    ,MLAW_TAG ,FAIL_TAG ,EOS_TAG   ,
     .     RWORK       ,SRWORK   ,IPM      ,PM         ,UNITAB    ,
     .     MULTI_FVM   ,FAILWAVE ,NLOC_DMG ,LSUBMODEL  ,TABLE     ,
     .     LTITR       ,USERL_AVAIL,MAT_NUMBER,
     .     NPC         ,TF       ,SNPC     ,NPTS       ,SBUFMAT   ,
     .     IRESP       )
c---------------------------------------------------------------
      call my_alloc(BUFMAT, SBUFMAT, "BUFMAT", stat=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='BUFMAT')
      BUFMAT = RWORK(1:SBUFMAT)
      IF(ALLOCATED(RWORK)) CALL MY_DEALLOC(RWORK)
      CALL TRACE_OUT1()
c
      ERR_MSG='STORAGE'
      ERR_MSG='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      CALL TITRE3
C--------------------------------------------
C     INITIALIZATION OF USER POINTERS
C--------------------------------------------
        CALL SAV_BUF_POINT(PM     ,1)
        CALL SAV_BUF_POINT(BUFMAT ,2)
        CALL SAV_BUF_POINT(GEO    ,3)
C        CALL SAV_BUF_POINT(AM(M26),4)
cma53a1  !!!!!!! a faire
        CALL SAV_BUF_POINT(ISKWN  ,7)
        CALL SAV_BUF_POINT(SKEW   ,8)
        CALL SAV_BUF_POINT(IPM ,11)
        CALL SAV_BUF_POINT(IGEO,12)
      CALL TRACE_OUT1()
C--------------------------------------------
C       READ NODES / CNODES / BUILD GHOST NODES
C--------------------------------------------
C     NODES
      ERR_MSG='NODES'
      ERR_CATEGORY='NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_ALLOC(CMERGE,NUMCNOD,"CMERGE")
      CMERGE = ZERO
C
      WRITE(ISTDO,'(A)')TITRE(12)
      CALL HM_READ_NODE(X       ,ITAB    ,ITABM1 ,CMERGE ,UNITAB  ,
     .                  WIGE    ,LSUBMODEL,IS_DYNA)

      CALL TRACE_OUT1()
C--------------------------------------------
C     PRE-READING SKEW DEFINITIONS
C--------------------------------------------
      IF(NUMSKW > 0) THEN
        CALL MY_ALLOC(ISKEW_TMP,SISKWN,"ISKEW_TMP")
        CALL MY_ALLOC(SKEW_TMP,SSKEW,"SKEW_TMP")
        SKEW_TMP = 0
        CALL HM_PREREAD_SKW(SKEW_TMP  ,ISKEW_TMP ,X      ,ITAB   ,ITABM1    ,
     .                      LSUBMODEL ,UNITAB    ,NUMNOD ,NUMSKW ,CHECK_USED,
     .                      LISKN     ,LSKEW     ,N2D    ,SISKWN ,SSKEW     )
      ELSE
        CALL MY_ALLOC(ISKEW_TMP,0,"ISKEW_TMP")
        CALL MY_ALLOC(SKEW_TMP,0,"SKEW_TMP")
      ENDIF
C--------------------------------------------
C     READING SUBMODELS & SUBMODEL TRANSFORMATION
C--------------------------------------------
      ERR_MSG='SUBMODELS'
      ERR_CATEGORY='SUBMODELS'
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR)
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SRTRANS = NRTRANS * NTRANSF
      CALL MY_ALLOC(RTRANS,SRTRANS,"RTRANS")
      IF(SRTRANS > 0) RTRANS = ZERO

      IF(NSUBMOD > 0)THEN
        WRITE(ISTDO,'(A)')' .. SUBMODELS'
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='RTRANS')
        CALL LECTRANSSUB(X      ,IGRNOD   ,ITAB   ,ITABM1   ,UNITAB,
     .                 RTRANS   ,LSUBMODEL,IS_DYNA,ISKEW_TMP,LISKN ,
     .                 NSPCOND  ,NUMSPH   ,SISKWN )
        CALL LECSUBMOD(ISUBMOD  ,X         ,UNITAB  ,ITABM1    ,RTRANS,
     .                 ITAB     ,LSUBMODEL ,IS_DYNA ,ISKEW_TMP ,LISKN ,
     .                 SKEW_TMP ,LSKEW     ,SISKWN  ,SSKEW     )
      ENDIF

      CALL TRACE_OUT1()
C--------------------------------------------
C     READING OBLIQUE COORDINATE SYSTEMS
C   + READING REFERENCE FRAMES
C--------------------------------------------
C     SKEWS
      IF(ALLOCATED(ISKEW_TMP)) CALL MY_DEALLOC(ISKEW_TMP)
      IF(ALLOCATED(SKEW_TMP)) CALL MY_DEALLOC(SKEW_TMP)
      ERR_MSG='SKEWS'
      ERR_CATEGORY='SKEWS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMSKW/=0)WRITE(ISTDO,'(A)')TITRE(14)
        CALL HM_READ_SKW(SKEW   ,ISKWN  ,X      ,
     .                   ITAB   ,ITABM1 ,BID13  ,
     .                   LSUBMODEL,RTRANS,
     .                   NOM_OPT(LNOPT1*INOM_OPT(10)+1),UNITAB)
C
        CALL HM_READ_FRM(ISKWN  ,X      ,ITAB   ,ITABM1 ,XFRAME ,
     .                   LSUBMODEL,RTRANS,
     .                   NOM_OPT(LNOPT1*INOM_OPT(10)+1),UNITAB)
      CALL TRACE_OUT1()
C--------------------------------------------
C     PRE-READING PLY DEFINITIONS
C--------------------------------------------
      IF(NDRAPE > 0) THEN
        CALL MY_ALLOC(IDRAPEID,NDRAPE,"IDRAPEID")
        IDRAPEID = 0
        CALL HM_READ_PRELECDRAPE(IDRAPEID,LSUBMODEL)
      ELSE
        CALL MY_ALLOC(IDRAPEID,0,"IDRAPEID")
      ENDIF
C--------------------------------------------
C     READING PROPERTIES
C--------------------------------------------
      IF(NSPHSOL/=0)THEN
        CALL HM_PREREAD_PART(IPART,IGEO,LSUBMODEL)
      END IF
C--------------------------------------------
      NRBAG=0
      SBUFGEO = 0
C     PROPERTIES
      ERR_MSG='PROPERTIES'
      ERR_CATEGORY='PROPERTIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      IF(NUMGEO > 0)THEN
        WRITE(ISTDO,'(A)')TITRE(31)
        SRWORK  = NUMGEO*(BGEOSIZE+MAXFUNC+MAXMAT+MAXPID+MAXTAB)
        SBUFGEO = 0
        CALL MY_ALLOC(DBRWORK,SRWORK,"DBRWORK")
        DBRWORK = ZERO
        CALL MY_ALLOC(KNOT,SKNOT,"KNOT")
        CALL SAV_BUF_POINT(DBRWORK,4)
C
        IADGEO=1
        CALL HM_READ_PROPERTIES(GEO        , X           , IXC      , PM       ,ITABM1    ,
     .                          DBRWORK    , SBUFGEO     , ISKWN    , IGEO     ,IPM       ,
     .                          NPC        , TF          , UNITAB   , RTRANS   ,LSUBMODEL ,
     .                          PROP_TAG   , IPART       , KNOT     , IDRAPEID ,STACK_INFO,
     .                          NUMGEOSTACK, NPROP_STACK , MULTI_FVM, IADGEO   ,DEFAULTS  ,
     .                          MAT_ELEM%MAT_PARAM,  ISMS)
C
        CALL MY_ALLOC(BUFGEO,SBUFGEO,"BUFGEO")
        BUFGEO(1:SBUFGEO) = DBRWORK(1:SBUFGEO)
        CALL MY_DEALLOC(DBRWORK)
        CALL SAV_BUF_POINT(BUFGEO,4)
      ELSE
        CALL MY_ALLOC(BUFGEO,SBUFGEO,"BUFGEO")
      ENDIF
C
      IF(NUMPLY /= 0) THEN
        CALL MY_ALLOC(PLY_INFO,3,NUMPLY,"PLY_INFO")
        PLY_INFO = 0
      ELSE
        CALL MY_ALLOC(PLY_INFO,0,0,"PLY_INFO")
      ENDIF
c
      IF(NUMSTACK > 0) THEN
C pre-reading
         LEN_G = NPROPGI*(NUMSTACK + NUMPLY)
         CALL MY_ALLOC(IGEO_STACK,LEN_G,"IGEO_STACK")
         IGEO_STACK = 0
         LEN_G = NPROPG*(NUMSTACK + NUMPLY )
         CALL MY_ALLOC(GEO_STACK,LEN_G,"GEO_STACK")
         GEO_STACK = ZERO
         CALL LECSTACK_PLY(GEO_STACK ,X          ,IXC       ,PM   ,ITABM1  ,
     .                     ISKWN     ,IGEO_STACK ,IPM       ,NPC  ,TF      ,
     .                     UNITAB    , RTRANS    ,LSUBMODEL,IPART ,IDRAPEID,
     .                     PLY_INFO  ,STACK_INFO,NUMGEOSTACK, NPROP_STACK  ,
     .                     DEFAULTS )
        ELSE
             CALL MY_ALLOC(IGEO_STACK,0,"IGEO_STACK")
             CALL MY_ALLOC(GEO_STACK,0,"GEO_STACK")
      ENDIF
C-----allocate DR if necessary------
      IF(SDR==0 .AND. IDROT == 1) THEN
       SDR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
       IF(ALLOCATED(DR)) CALL MY_DEALLOC(DR)
       CALL MY_ALLOC(DR,SDR,"DR")
       DR = ZERO
      ENDIF
C

C      print*, 'NPINCH', NPINCH, 'NUMNOD', NUMNOD
      SPINCH= NPINCH
C      print*, SPINCH
        CALL MY_ALLOC(PINCH_DATA%XPINCH,3,SPINCH,"PINCH_DATA%XPINCH")
        CALL MY_ALLOC(PINCH_DATA%DPINCH,3,SPINCH,"PINCH_DATA%DPINCH")
        CALL MY_ALLOC(PINCH_DATA%VPINCH,3,SPINCH,"PINCH_DATA%VPINCH")
        CALL MY_ALLOC(PINCH_DATA%MSPINCH,SPINCH,"PINCH_DATA%MSPINCH")
C
      IF(NPINCH > 0) THEN
        PINCH_DATA%XPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%DPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%VPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%MSPINCH(1:SPINCH) = ZERO
      ENDIF
C
      CALL TRACE_OUT1()
C-----------------------------------------------------
C     READING PARTS
C     REPLACEMENT OF EXTERNAL MATERIAL NUMBERS
C     REPLACEMENT OF EXTERNAL PROPERTY NUMBERS
C     BY INTERNAL NUMBERS
C-----------------------------------------------------
      ERR_MSG='PARTS'
      ERR_CATEGORY='PARTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIWORK  = MAX(2*NUMELS,2*NUMELQ,3*(NPART+NTHPART),3*NUMSKW,NUMELS,
     *              NUMELC,NUMELTG,NUMMAT+NUMGEO,NUMELT+NUMELP+NUMELR+
     *              NUMELX+NUMELIG3D)
      CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
      IWORK = 0
      CALL MY_ALLOC(THK_PART,NPART,"THK_PART")
C
      CALL HM_READ_PART(IPART  ,PM      ,GEO       ,IPM     ,IGEO ,IWORK ,THK_PART,
     .                  UNITAB,LSUBMODEL,MULTI_FVM ,MLAW_TAG,MAT_ELEM%MAT_PARAM,GLOB_THERM)

      CALL TRACE_OUT1()
      
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (A.L.E.) ENTIER
C--------------------------------------------
      ALE%GLOBAL%SNALE  = MAX(IALE,IEULER,IALELAG)*NUMNOD
      ALE%GLOBAL%SIELVS = 6*NUMELS+MAX(IALE,GLOB_THERM%ITHERM,IEULER,IALELAG)* (4 * NUMELQ + 3 * NUMELTG)
      SIFILL = NMULT*NUMNOD
      SIMS   = NMULT*NUMNOD
C---
      CALL MY_ALLOC(IFILL,SIFILL,"IFILL")

      CALL MY_ALLOC(IMS,SIMS,"IMS")


      IF(SIFILL > 0) IFILL = 0
      IF(SIMS   > 0) IMS   = 0

      CALL MY_ALLOC(DFLOW,3*NUMNOD*IALELAG,"DFLOW")
      CALL MY_ALLOC(VFLOW,3*NUMNOD*IALELAG,"VFLOW")
      CALL MY_ALLOC(WFLOW,3*NUMNOD*IALELAG,"WFLOW")

      IF(IALELAG > 0) THEN
       DFLOW = ZERO
       VFLOW = ZERO
       WFLOW = ZERO
      ENDIF

      IF(ALEFVM_Param%IEnabled > 0)THEN
        CALL MY_ALLOC(ALEFVM_Buffer%FCELL,6,NUMELS,"ALEFVM_Buffer%FCELL")
        ALEFVM_Buffer%FCELL(:,:) = ZERO
      ENDIF

C----------------------------------      
C--------------------------------------------
C     MULTIDOMAINS
C--------------------------------------------
      ERR_MSG='MULTIDOMAINS'
      ERR_CATEGORY='MULTIDOMAINS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NR2R = 5
      R2R_SIU = 0
      SIEXLNK = NR2R*NR2RLNK
      IF((NR2RLNK+NSUBDOM)>0) THEN
        CALL MY_ALLOC(IEXLNK,SIEXLNK,"IEXLNK")
        IEXLNK = 0
        WRITE(ISTDO,'(A)')  ' .. EXTERNAL COUPLING'
        CALL LECEXTLNK(IEXLNK,IPART,LSUBMODEL)
        NL_DDR2R = NR2RLNK
      ELSE
        CALL MY_ALLOC(IEXLNK,0,"IEXLNK")
      ENDIF
      IF(NSUBDOM > 0) THEN
        call my_alloc(TAG_PART, NPART, "TAG_PART")
        call my_alloc(IPART_R2R, 4, NPART, "IPART_R2R")
        TAG_PART(:)=0
        CALL R2R_VOID(IPART)
        NL_DDR2R = 4
        R2R_SIU = 1
c       complete mlaw_tag for new void materials
        !Material Buffer copy for R2R (rad2rad) with R2R_MATPARAM_COPY
        IF(NUMMAT > NUMMAT0) THEN
          allocate(MTAG_R2R(NUMMAT))
          allocate(MPARAM_R2R(NUMMAT))
          CALL INIT_MLAW_TAG(MTAG_R2R   ,NUMMAT)
          MTAG_R2R(1:NUMMAT0) = MTAG_INI(1:NUMMAT0)
          CALL R2R_MATPARAM_COPY(MPARAM_R2R, MPARAM_INI   ,NUMMAT0 ,NUMMAT)
          MLAW_TAG(1:NUMMAT)     => MTAG_R2R(1:NUMMAT)
          MAT_ELEM%MAT_PARAM(1:NUMMAT) => MPARAM_R2R(1:NUMMAT)
          MAT_ELEM%NUMMAT = NUMMAT
          DEALLOCATE(MTAG_INI)
          IF(ALLOCATED(MPARAM_INI))THEN
            DO I=1,NUMMAT0 ; CALL MPARAM_INI(I)%DESTRUCT() ; ENDDO
            DEALLOCATE(MPARAM_INI)
          ENDIF
        ENDIF
      ELSE
        call my_alloc(TAG_PART, 0, "TAG_PART")
        call my_alloc(IPART_R2R, 4, 0, "IPART_R2R")
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C--------------------------------------------
C     POIN UMP
C--------------------------------------------
!     TABMP_L defined in tabsiz_c
      TABMP_L = 10
C
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='PARTS')

      CALL MY_ALLOC(POIN_PART_SHELL,2,NPART,"POIN_PART_SHELL")
      CALL MY_ALLOC(POIN_PART_TRI,2,NPART,"POIN_PART_TRI")
      CALL MY_ALLOC(POIN_PART_SOL,2,NPART,7,"POIN_PART_SOL")
      allocate(MID_PID_SHELL(NUMMAT))
      allocate(MID_PID_TRI(NUMMAT))
      allocate(MID_PID_SOL(NUMMAT, 7))
      POIN_PART_SHELL(1:2,1:NPART) = 0
      POIN_PART_TRI(1:2,1:NPART) = 0
      POIN_PART_SOL(1:2,1:NPART,1:7) = 0

      CALL MY_ALLOC(POIN_UMP,NUMMAT,"POIN_UMP")
      POIN_UMP(1:NUMMAT) = 0
      CALL MY_ALLOC(TAB_UMP_LOC,5,NPART,"TAB_UMP_LOC")
      TAB_UMP_LOC(1:5,1:NPART) = 0
C
      CALL SET_POIN_UMP(IPART,IPM,TAB_UMP_LOC,POIN_UMP,TAILLE2)
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING SOLIDS
C--------------------------------------------
      ERR_MSG='ELEMENTS'
      ERR_CATEGORY='ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELS/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(15)

       CALL HM_READ_SOLID(IXS    ,PM      ,ITAB    ,ITABM1  ,
     .             IPART  ,IPARTS  ,EANI    ,IXS10   ,IXS20   ,IXS16   ,
     .             IGEO   ,LSUBMODEL,IS_DYNA,X     )

      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING 2D ELEMENTS
C--------------------------------------------
      IF(NUMELQ/=0)THEN
        WRITE(ISTDO,'(A)')TITRE(16)
        CALL HM_READ_QUAD(IXQ    ,ITAB   ,ITABM1 ,IPART  ,IPARTQ ,
     .             IPM     ,IGEO   ,UNITAB ,LSUBMODEL)
      ENDIF
C--------------------------------------------
C     READING SHELLS
C--------------------------------------------
       CALL MY_ALLOC(ITAG,NUMNOD,"ITAG")
      ITAG=0
C
      IF(NUMELC/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(17)
       CALL HM_READ_SHELL(IXC    ,ITAB   ,ITABM1 ,IPART  ,IPARTC ,
     .             THKE   ,IPM     ,IGEO   ,UNITAB ,ITAG   ,SH4ANG, LSUBMODEL)
      ENDIF
C
C          WARNING, SHELL ELEMENTS ARE PERMUTED
C          TO GENERATE OPTIMIZED GROUPS
C          AFTER READING PIDs
C
C--------------------------------------------
C     READING TRUSSES
C--------------------------------------------
      IF(NUMELT/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(18)
       CALL HM_READ_TRUSS(IXT    ,ITAB   ,ITABM1 ,IPART  ,IPARTT  ,
     .             IPM    ,IGEO    ,LSUBMODEL)
      ENDIF
C--------------------------------------------
C     READING BEAMS
C--------------------------------------------
      IF(NUMELP/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(19)
       CALL HM_READ_BEAM(IXP    ,ITAB   ,ITABM1 ,IPART  ,IPARTP  ,
     .                   IPM    ,IGEO   ,LSUBMODEL,IBEAM_VECTOR,RBEAM_VECTOR)
      ENDIF
C--------------------------------------------
C     READING SPRINGS
C--------------------------------------------
      REINT = ZERO
      IF(NUMELR/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(20)
       CALL HM_READ_SPRING(IXR    ,ITAB   ,ITABM1 ,IPART  ,IPARTR  ,
     .             IGEO  ,IXR_KJ ,LSUBMODEL,ISKWN,R_SKEW,IPM)
      ENDIF
C--------------------------------------------
C     READING TRIANGULAR SHELLS
C--------------------------------------------
      IF(NUMELTG/=0)THEN
        IF(N2D==0 .AND. NUMELTRIA==0)THEN
           WRITE(ISTDO,'(A)')TITRE(21)
            CALL HM_READ_SH3N(  IXTG ,ITAB   ,ITABM1  ,IPART  ,IPARTG  ,
     .                     THKEC  ,PM      ,GEO    ,EANIT   ,IGEO   ,
     .                     IPM    ,UNITAB  ,SH3ANG , LSUBMODEL)
        ELSEIF(NUMELTRIA==NUMELTG)THEN
           WRITE(ISTDO,'(A)')TITRE(23)
            CALL HM_READ_TRIA(IXTG   ,ITAB   ,ITABM1  ,IPART  ,IPARTG  ,
     .                     PM     ,GEO    ,EANIT   ,IGEO   ,IPM     ,
     .                     UNITAB    , LSUBMODEL)
        ELSE
          !check is IXTG array is used with proper modeling : 2D-TRIA or 3D-SH3N
          IF(NUMELTG>0 .AND. N2D>0 .AND. NUMELTRIA==0)THEN
               CALL ANCMSG(MSGID=66,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1='SH3N',
     .                     C2='2D-ANALYSIS'
     .                     )
          ELSEIF(NUMELTG>0 .AND. N2D==0 .AND. NUMELTRIA==NUMELTG)THEN
               CALL ANCMSG(MSGID=66,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1='TRIA',
     .                     C2='3D-ANALYSIS'
     .                     )
          ENDIF
          NUMELTG = 0
        ENDIF
      ENDIF
C--------------------------------------------
C     Check XFEM FLAG
      IF(NUMELTG + NUMELC == 0) ICRACK3D = 0
C--------------------------------------------
C     READING ISO-GEOMETRIC ELEMENTS
C--------------------------------------------
      NCTRLMAX = 0
      IF(NUMELIG3D/=0)THEN

       SKXIG3D = NIXIG3D*NUMELIG3D
       WRITE(ISTDO,'(A)')TITRE(22)

       CALL PRELECIG3D(SIXIG3D)

       CALL MY_ALLOC(KXIG3D,SKXIG3D,"KXIG3D")
       CALL MY_ALLOC(IXIG3D,SIXIG3D+ADDSIXIG3D,"IXIG3D")
       allocate(TABCONPATCH(NBPART_IG3D))
       KXIG3D = 0
       IXIG3D = 0
       CALL LECIG3D(
     .           ITAB    ,IPART   ,IPARTIG3D  ,IPM     ,IGEO    ,
     .           KXIG3D  ,IXIG3D  ,ITABM1     ,NCTRLMAX,TABCONPATCH)

       SKNOTLOCPC = DEG_MAX*3*(NUMNOD+L_TAB_NEWFCT)*NUMGEO  ! THERE SHOULD BE A NUMBER OF IGE POINTS AND INDICES
cSknotlocpc is slightly oversized as it takes into account temporary work points
       IF(SKNOTLOCPC > INTMAX .OR. SKNOTLOCPC < ZERO) THEN ! L_TAB_NEWFCT to allow for workspace size
         SKNOTLOCPC = INTMAX
       ELSE
         SKNOTLOCPC = INT(SKNOTLOCPC)
       ENDIF
       CALL MY_ALLOC(KNOTLOCPC,SKNOTLOCPC,"KNOTLOCPC")
       KNOTLOCPC(:)=0

       SKNOTLOCEL = 2*3*NUMELIG3D
       IF(SKNOTLOCEL > INTMAX .OR. SKNOTLOCEL < ZERO) THEN
         SKNOTLOCEL = INTMAX
       ELSE
         SKNOTLOCEL = INT(SKNOTLOCEL)
       ENDIF
       CALL MY_ALLOC(KNOTLOCEL,SKNOTLOCEL,"KNOTLOCEL")
       KNOTLOCEL(:)=0

       CALL PRERAFIG3D(KNOT,KNOTLOCPC,KNOTLOCEL,
     .                 KXIG3D,IXIG3D,IGEO,
     .                 IPARTIG3D,
     .                 X,V,D,MS,WIGE,TABCONPATCH,1)
c
       SIXIG3D=SIXIG3D+ADDSIXIG3D
       CALL MY_ALLOC(MSIG3D,NUMELIG3D*NCTRLMAX,"MSIG3D")
       MSIG3D(1:NUMELIG3D*NCTRLMAX) = ZERO
      ELSE
        CALL MY_ALLOC(KXIG3D,0,"KXIG3D")
        CALL MY_ALLOC(MSIG3D,0,"MSIG3D")
        CALL MY_ALLOC(IXIG3D,0,"IXIG3D")
        CALL MY_ALLOC(KNOTLOCEL,0,"KNOTLOCEL")
        CALL MY_ALLOC(KNOTLOCPC,0,"KNOTLOCPC")
      ENDIF
C--------------------------------------------
C     PRE-READING GRNOD/NODENS
C--------------------------------------------
      IF(NUMELX > 0 .AND. NGRNOD > 0 )THEN
        WRITE(ISTDO,'(A)')' .. NODENS GROUP '
        CALL HM_PRELECGRNS(ITABM1 ,IGRNOD, LSUBMODEL)
      ENDIF
C---------------------------------------------
C      PRE-READING MULTI-PURPOSE ELEMENTS.
C---------------------------------------------
      ERR_MSG='MULTI-PURPOSE ELEMENTS'
      ERR_CATEGORY='MULTI-PURPOSE ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELX > 0) THEN
        SKXX = NIXX*NUMELX
        CALL HM_PREREAD_XELEM(SIXX, IGRNOD,LSUBMODEL)
        CALL MY_ALLOC(KXX,SKXX,"KXX")
        CALL MY_ALLOC(IXX,SIXX+150,"IXX")
        KXX = 0
        IXX = 0
        CALL MY_ALLOC(LELX,NUMELX,"LELX")
        LELX(1:NUMELX) = ZERO
        CALL HM_READ_XELEM(IGRNOD  ,ITAB   ,ITABM1 ,IPART ,IPARTX,
     .              IPM     ,IGEO   ,KXX    ,IXX   ,LSUBMODEL)
      ELSE
        SKXX = 0
        SIXX = 0
        CALL MY_ALLOC(KXX,SKXX,"KXX")
        CALL MY_ALLOC(IXX,SIXX,"IXX")
        CALL MY_ALLOC(LELX,NUMELX,"LELX")
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     ADAPTIVE MESHING
C--------------------------------------------
      ERR_MSG='ADAPTIVE MESHING'
      ERR_CATEGORY='ADAPTIVE MESHING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      LEVELMAX=0
      LSH4TRIM=0
      LSH3TRIM=0

      if(nadmesh==0) then
        sh4tree_dim1 = 0
        sh4tree_dim2 = 0
        sh3tree_dim1 = 0
        sh3tree_dim2 = 0
      else
        sh4tree_dim1 = ksh4tree
        sh4tree_dim2 = numelc
        sh3tree_dim1 = ksh3tree
        sh3tree_dim2 = numeltg
      endif

      IF(NADMESH/=0)THEN

        CALL MY_ALLOC(SH4TREE,KSH4TREE,NUMELC,"SH4TREE")
        SH4TREE=0
        CALL MY_ALLOC(SH3TREE,KSH3TREE,NUMELTG,"SH3TREE")
        SH3TREE=0
        CALL MY_ALLOC(IPADMESH,KIPADMESH,NPART,"IPADMESH")
        IPADMESH=0

        CALL MY_ALLOC(PADMESH,KPADMESH,NPART,"PADMESH")
        PADMESH=ZERO

        CALL SET_ADMESH(IPART ,IPADMESH,PADMESH,UNITAB,LSUBMODEL )

        IF(IADMSTAT/=0)THEN
          LSH4TRIM=NUMELC
          CALL MY_ALLOC(SH4TRIM,LSH4TRIM,"SH4TRIM")
          SH4TRIM=0
          LSH3TRIM=NUMELTG
          CALL MY_ALLOC(SH3TRIM,LSH3TRIM,"SH3TRIM")
          SH3TRIM=0
          CALL STATE_ADMESH(
     .                IPART ,IPARTC ,IPARTG ,IXC ,IXTG ,
     .                SH4TREE, SH3TREE, SH4TRIM, SH3TRIM,
     .                LSUBMODEL)
        END IF
        CALL BUILD_ADMESH(
     .               IPART ,IPARTC ,IPARTG ,IXC ,IXTG ,
     .               X     ,ITAB   ,ITABM1  ,SH4TREE, SH3TREE,
     .               IPADMESH,PADMESH)
      ELSE
        CALL MY_ALLOC(SH4TREE,0,0,"SH4TREE")
        CALL MY_ALLOC(SH3TREE,0,0,"SH3TREE")
        CALL MY_ALLOC(IPADMESH,0,0,"IPADMESH")
        CALL MY_ALLOC(PADMESH,0,0,"PADMESH")
        CALL MY_ALLOC(SH4TRIM,0,"SH4TRIM")
        CALL MY_ALLOC(SH3TRIM,0,"SH3TRIM")
      END IF

      

      IF(ISTATCND/=0)THEN
        call my_alloc(MSCND, NUMNOD, "MSCND", stat=stat)
        call my_alloc(INCND, NUMNOD, "INCND", stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='MSCND')
        MSCND=ZERO
        INCND=ZERO
      ELSE
        call my_alloc(MSCND, 0, "MSCND")
        call my_alloc(INCND, 0, "INCND")
      END IF
      CALL TRACE_OUT1()

C--------------------------------------------
C     REINITIALISATION MAT/PROP
C--------------------------------------------

      IF(TAILLE2>0) THEN
       CALL MY_ALLOC(TAB_UMP_LOC2,7+6,TAILLE2,2,"TAB_UMP_LOC2")
       TAB_UMP_LOC2 = 0
       CALL REINI_MATPROP(TAILLE,TAILLE2,TAB_UMP_LOC,TAB_UMP_LOC2,
     .           IXS,IXQ,IXC,IXT,IXP,IXR,
     .           IXTG,EANI,POIN_UMP)

       CALL MY_ALLOC(TAB_UMP,7,TAILLE,"TAB_UMP")
       TAB_UMP = 0
       IF(TAILLE>0) THEN
        CALL REINI_MATPROP2(TAILLE,TAILLE2,
     .          TAB_UMP_LOC,TAB_UMP_LOC2,TAB_UMP,TAB_SOL,
     .           POIN_UMP)
       ENDIF
       CALL MY_DEALLOC(TAB_UMP_LOC2)
      ENDIF
      CALL MY_DEALLOC(TAB_UMP_LOC)
C--------------------------------------------
C     TABLEAUX X-FEM (SHELL 4-N + SHELL 3-N)
C     xfem for crack propagation (mono + multi layer shells)
C--------------------------------------------
      IF(ICRACK3D > 0) THEN
        NXEL   = 3  ! nb of phantom elements within one layer (change to NXEL=3)
        XFEMON = 1
        IF(IPARI0 /= 1) IPARI0=1 ! force flag parith/on pour XFEM (pareil engine)
      ELSE
        NXEL   = 0
        XFEMON = 0
      ENDIF
C-----
      ERR_MSG='XFEM FOR SHELLS - ALLOCATIONS'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C-----
      LEN = XFEMON*NUMNOD
      ALLOCATE(ADDCNE_CRKXFEM(0:LEN+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='ADDCNE_CRKXFEM')
      ADDCNE_CRKXFEM(0:LEN+1) = 0
c
      CALL MY_ALLOC(ITAGN,LEN,"ITAGN")
      CALL MY_ALLOC(INOD_CRKXFEM,LEN,"INOD_CRKXFEM")
      CALL MY_ALLOC(IBORDNODE,LEN,"IBORDNODE")
      LEN = XFEMON*(NUMELC+NUMELTG)
      SITAGE=LEN
      CALL MY_ALLOC(ITAGE,LEN,"ITAGE")
      CALL MY_ALLOC(IEL_CRKXFEM,LEN,"IEL_CRKXFEM")
c
      ITAGN          = 0
      ITAGE          = 0
      INOD_CRKXFEM   = 0
      IEL_CRKXFEM    = 0
      IBORDNODE      = 0
c
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING GLOBAL PARAMETERS + SPH PARTICLES.
C---------------------------------------------
      ERR_MSG='SPH'
      ERR_CATEGORY='SPH'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSPHSOL/=0)THEN
        CALL MY_ALLOC(SPH2SOL,NUMSPH,"SPH2SOL")
        SPH2SOL=0
        CALL MY_ALLOC(SOL2SPH,2*NUMELS8,"SOL2SPH")
        SOL2SPH=0
        CALL MY_ALLOC(IRST,3*NSPHSOL,"IRST")
        IRST=0
        CALL MY_ALLOC(SOL2SPH_TYP,NUMELS8,"SOL2SPH_TYP")
        SOL2SPH_TYP=0
      ELSE
        CALL MY_ALLOC(SPH2SOL,0,"SPH2SOL")
        CALL MY_ALLOC(SOL2SPH,0,"SOL2SPH")
        CALL MY_ALLOC(IRST,0,"IRST")
        CALL MY_ALLOC(SOL2SPH_TYP,0,"SOL2SPH_TYP")
      END IF
      IF(NUMSPH > 0) THEN
        SSPBUF  = NSPBUF*NUMSPH
        SKXSP   = NISP*NUMSPH
        SIXSP   = KVOISPH*NUMSPH
c test with INTEGER 64 bits to avoid integer 32 bits overflow with huge cases (10 Millions SPH cells)
        KVOISPH8 = KVOISPH
        NUMSPH8 = NUMSPH

c limit INTEGER 32 bits ((2^31)-1), we add a security marge of 5%
        SIXSP8 = (NUMSPH8/(NSPMD))*KVOISPH8
        LIMIT8 = (HUGE(INTEGER_LIMIT32)-1)*0.95!((2**31)-1)*0.95
        IF(SIXSP8>LIMIT8)THEN
          CALL ANCMSG(MSGID=981,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP)
        ENDIF

        SNOD2SP = NUMNOD
        CALL MY_ALLOC(KXSP,SKXSP,"KXSP")
        CALL MY_ALLOC(IXSP,KVOISPH,NUMSPH,"IXSP")
        CALL MY_ALLOC(NOD2SP,SNOD2SP,"NOD2SP")
        CALL MY_ALLOC(SPBUF,SSPBUF,"SPBUF")

        IF(NSPHIO>0.AND.NBPARTINLET>0)THEN
          CALL MY_ALLOC(RESERVEP,NBPARTINLET,"RESERVEP")
          RESERVEP(1:NBPARTINLET) = 0 
        ELSE
          CALL MY_ALLOC(RESERVEP,1,"RESERVEP")
          RESERVEP(1) = 0
        ENDIF
        KXSP   = 0
        IXSP   = 0
        NOD2SP = 0
        SPBUF  = ZERO
        WRITE(ISTDO,'(A)')' .. SPH PARTICLES DEFINITION'
        CALL HM_READ_SPHCEL(ITAB    ,ITABM1   ,IPART   ,
     2              IPARTSP ,IPM    ,IGEO    ,KXSP    ,IXSP    ,
     3              NOD2SP, RESERVEP,IXS     ,IPARTS  ,EANI    ,
     4              SPH2SOL,SOL2SPH ,IRST    ,X       ,SOL2SPH_TYP,
     5              LSUBMODEL,SPBUF ,UNITAB  ,IPRI   )

      ELSE
        SSPBUF  = 0
        SKXSP   = 0
        SIXSP   = 0
        SNOD2SP = 0
        CALL MY_ALLOC(KXSP,SKXSP,"KXSP")
        CALL MY_ALLOC(IXSP,0,0,"IXSP")
        CALL MY_ALLOC(NOD2SP,SNOD2SP,"NOD2SP")
        CALL MY_ALLOC(SPBUF,SSPBUF,"SPBUF")
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     INVERSE CONNECTIVITY (Starter only)
C--------------------------------------------
C Create IXTG1 array and set to 0
      ERR_MSG='CONNECTIVITY'
      ERR_CATEGORY='CONNECTIVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ! -------------------
      ! initialisation of invert_group structure, used in
      ! HM_READ_SET and in HM_READ_SENSORS for user sensor
      CALL INVERTED_GROUP_INIT(0,INV_GROUP,NUMSPH)
      ! example :
      CALL COMPUTE_CONNECT_PARTELM(IPARTS ,IPARTQ ,IPARTC   ,IPARTT ,IPARTP,
     .                             IPARTG ,IPARTR ,IPARTSP  ,INV_GROUP,NUMSPH,
     .                             NISP   ,KXSP   )
      ! -------------------
      IF(NUMELTG6 ==0 )THEN
        SIXTG1  = 0
      ELSE
        SIXTG1 = 4*NUMELTG
      ENDIF
      CALL MY_ALLOC(IXTG1,SIXTG1,"IXTG1")
      IXTG1 = 0

214   ALLOCATE(KNOD2ELS(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2ELS')
      KNOD2ELS=0
      CALL MY_ALLOC(KNOD2ELC,NUMNOD+1,"KNOD2ELC")
      KNOD2ELC=0
      CALL MY_ALLOC(KNOD2ELTG,NUMNOD+1,"KNOD2ELTG")
      KNOD2ELTG=0
      CALL MY_ALLOC(KNOD2EL1D,NUMNOD+1,"KNOD2EL1D")
      KNOD2EL1D=0

      CALL MY_ALLOC(KNOD2ELQ,NUMNOD+1,"KNOD2ELQ")
      KNOD2ELQ=0


      S_NOD2ELS = 8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
      CALL MY_ALLOC(NOD2ELS,S_NOD2ELS,"NOD2ELS")
      NOD2ELS=0
      CALL MY_ALLOC(NOD2ELC,4*NUMELC,"NOD2ELC")
      NOD2ELC=0
      S_NOD2ELTG = 3*NUMELTG+3*NUMELTG6
      CALL MY_ALLOC(NOD2ELTG,S_NOD2ELTG,"NOD2ELTG")
      NOD2ELTG=0
      S_NOD2EL1D=2*NUMELT+2*NUMELP+3*NUMELR+2*SIXX
      CALL MY_ALLOC(NOD2EL1D,S_NOD2EL1D,"NOD2EL1D")
      NOD2EL1D=0
      CALL MY_ALLOC(KNOD2ELIG3D,NUMNOD+1,"KNOD2ELIG3D")
      KNOD2ELIG3D=0
      call my_alloc(NOD2ELIG3D, NCTRLMAX*NUMELIG3D, "NOD2ELIG3D", stat=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELIG3D')
      NOD2ELIG3D=0
      CALL MY_ALLOC(NOD2ELQ,4*NUMELQ,"NOD2ELQ")
      NOD2ELQ=0

      IF(FLAG_GOTO==1) GOTO 215

C--------------------------------------------
      CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
      IF(ALLOCATED(IWORK)) CALL MY_DEALLOC(IWORK)
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING SUBSETS
C--------------------------------------------
      TAGSURFIGE = 0
      SIBUFSSG = 0  ! to be removed
!
      ERR_MSG='SUBSETS'
      ERR_CATEGORY='SUBSETS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSUBS > 0) THEN
        WRITE(ISTDO,'(A)')' .. SUBSETS'
!
        CALL HM_READ_SUBSET(SUBSETS,IPART,NSUBS,NPART,LSUBMODEL)
        CALL SUBSET_INI(SUBSETS)
!
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING BOXES (BOX and BOX of BOX)
C--------------------------------------------
      CALL STARTIME(19,1)
C--------------------------------------------
      ERR_MSG='BOXES'
      ERR_CATEGORY='BOXES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      allocate(IBOX(NBBOX))
      IF(NBBOX > 0) THEN
        WRITE(ISTDO,'(A)')' .. BOX '
c
        CALL HM_READ_BOX(IBOX   ,UNITAB ,ITABM1   ,ISKWN    ,SKEW     ,
     .                   X      ,RTRANS ,LSUBMODEL)
C
      ENDIF
C--------------------
      CALL TRACE_OUT1()
C--------------------------
C    ELEMENT GROUT READING
C--------------------------
      ERR_MSG='GROUPS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
!
      IDXIGECNT= 1
!
      WRITE(ISTDO,'(A)')' .. ELEMENT GROUPS'
      ERR_CATEGORY='ELEMENT GROUPS'
      FLAGG = 0
      IADBOXMAX = 1
C     count group elements
      call my_alloc(IXS_S, NUMELS, "IXS_S")
      call my_alloc(IXS_S_IND, NUMELS, "IXS_S_IND")
      call my_alloc(IXQ_S, NUMELQ, "IXQ_S")
      call my_alloc(IXQ_S_IND, NUMELQ, "IXQ_S_IND")
      call my_alloc(IXC_S, NUMELC, "IXC_S")
      call my_alloc(IXC_S_IND, NUMELC, "IXC_S_IND")
      call my_alloc(IXT_S, NUMELT, "IXT_S")
      call my_alloc(IXT_S_IND, NUMELT, "IXT_S_IND")
      call my_alloc(IXP_S, NUMELP, "IXP_S")
      call my_alloc(IXP_S_IND, NUMELP, "IXP_S_IND")
      call my_alloc(IXR_S, NUMELR, "IXR_S")
      call my_alloc(IXR_S_IND, NUMELR, "IXR_S_IND")
      call my_alloc(IXTG_S, NUMELTG, "IXTG_S")
      call my_alloc(IXTG_S_IND, NUMELTG, "IXTG_S_IND")

      CALL LECGROUP(
     1     ITAB    ,ITABM1            ,ISUBMOD ,
     2     X       ,IXS      ,IXQ     ,IXC     ,IXT     ,IXP     ,
     3     IXR     ,IXTG     ,                           IPART   ,
     4     IPARTS  ,IPARTQ   ,IPARTC  ,IPARTT  ,IPARTP  ,IPARTR  ,
     5     IPARTG  ,FLAGG    ,SH4TREE ,SH3TREE ,
     6     SKEW    ,ISKWN    ,UNITAB  ,IBOX    ,
     7     IXS10  ,IXS16   ,IXS20   ,RTRANS,LSUBMODEL,
     8     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     9     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     A     IXTG_S,IXTG_S_IND,IADBOXMAX,SUBSETS,IGRBRIC,IGRQUAD,
     B     IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING)
C---
      LENI=MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,NUMELTG)
!
      CALL SORTGROUP(
     1     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     2     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     3     IXTG_S,IXTG_S_IND,IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,LENI)
!
      FLAGG = 1
      CALL LECGROUP(
     1     ITAB    ,ITABM1           ,ISUBMOD ,
     2     X       ,IXS     ,IXQ     ,IXC     ,IXT     ,IXP     ,
     3     IXR     ,IXTG    ,                           IPART   ,
     4     IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,IPARTR  ,
     5     IPARTG  ,FLAGG   ,SH4TREE ,SH3TREE ,
     6     SKEW    ,ISKWN   ,UNITAB  ,IBOX    ,
     7     IXS10 ,IXS16,IXS20,RTRANS,LSUBMODEL,
     8     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     9     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     A     IXTG_S,IXTG_S_IND,IADBOXMAX,SUBSETS,IGRBRIC,IGRQUAD,
     B     IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING)
!
      CALL MY_DEALLOC(IXS_S)
      CALL MY_DEALLOC(IXS_S_IND)
      CALL MY_DEALLOC(IXQ_S)
      CALL MY_DEALLOC(IXQ_S_IND)
      CALL MY_DEALLOC(IXC_S)
      CALL MY_DEALLOC(IXC_S_IND)
      CALL MY_DEALLOC(IXT_S)
      CALL MY_DEALLOC(IXT_S_IND)
      CALL MY_DEALLOC(IXP_S)
      CALL MY_DEALLOC(IXP_S_IND)
      CALL MY_DEALLOC(IXR_S)
      CALL MY_DEALLOC(IXR_S_IND)
      CALL MY_DEALLOC(IXTG_S)
      CALL MY_DEALLOC(IXTG_S_IND)
C--------------------------------------------
C    READING PART GROUPS (1st LEVEL)
C--------------------------------------------
      WRITE(ISTDO,'(A)')' .. PART GROUPS'
      ERR_CATEGORY='PART GROUPS'

      FLAGG = 0 !TAG ENTITY & ALLOCATE
      CALL HM_READ_GRPART(IGRPART , IPART, ISUBMOD, FLAGG ,NGRPART,LSUBMODEL, SUBSETS )

      FLAGG = 1 !BUILD GROUPS
      CALL HM_READ_GRPART(IGRPART , IPART, ISUBMOD, FLAGG ,NGRPART,LSUBMODEL, SUBSETS )
C--------------------------------------------
C    READING GROUPS OF GROUPS
C--------------------------------------------
      ERR_CATEGORY='GROUP OF GROUPS'
      ICOUNT = 1
      ITER   = 0
      DO WHILE (ICOUNT > 0)
        ITER  = ITER  + 1
        FLAGG = 0
C---
        CALL LECGGROUP(
     .       FLAGG   ,
     .       ICOUNT  ,ITER    ,IGRBRIC,IGRQUAD  ,IGRSH4N,
     .       IGRSH3N ,IGRTRUSS,IGRBEAM,IGRSPRING,IGRPART,
     .       LSUBMODEL)
C---
        FLAGG = 1
C---
        CALL LECGGROUP(
     .       FLAGG   ,
     .       ICOUNT  ,ITER    ,IGRBRIC,IGRQUAD  ,IGRSH4N,
     .       IGRSH3N ,IGRTRUSS,IGRBEAM,IGRSPRING,IGRPART,
     .       LSUBMODEL)
      ENDDO
C--------------------------------------------
C    READING SURFACES
C--------------------------------------------

      ! allocation for pre-read of Rbody needed for /SET
      IF(NRBODY > 0) THEN
        CALL MY_ALLOC(RBY_MSN,2,NRBODY,"RBY_MSN")
        CALL PREREAD_RBODY_SET(LSUBMODEL,ITABM1,RBY_MSN)
      ELSE
        CALL MY_ALLOC(RBY_MSN,0,0,"RBY_MSN")
      ENDIF

      ! PART UID to Internal ID conversion

      CALL CREATE_MAP_TABLES ( MAP_TABLES ,1     ,
     *                         LSUBMODEL ,SUBSETS,
     *                         IPART,
     *                         IXS  ,IXQ  ,IXC   ,IXTG ,
     *                         IXT  ,IXP  ,IXR   ,KXSP,IBID,
     *                         RBY_MSN)


      SBUFSF = 0
      IF(NSURF+NSETS > 0)THEN
        CALL MY_ALLOC(RWORK,LISURF1*(NSURF+NSETS),"RWORK")
        RWORK = ZERO
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                            C1='RWORK')
      ENDIF

      IF(NSURF > 0)THEN
        WRITE(ISTDO,'(A)')' .. SURFACES '
        ERR_CATEGORY='SURFACES DEFINITION'
c
C- Isogeometric Elements
        IADTABIGE = 0
        DECALIGEO  = 0
        IDXIGE1 = 0
        IDXIGE2 = 1
        RNIGE=(1+NSURF*NUMELIG3D*16*6)
        IF(RNIGE > INTMAX .OR. RNIGE < ZERO) THEN
          SNIGE = INTMAX
        ELSE
        SNIGE = INT(RNIGE)
        ENDIF
        call my_alloc(NIGE_TMP(IDXIGE1)%ptr, SNIGE, "NIGE_TMP(IDXIGE1)%ptr", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF
        RRIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RRIGE > INTMAX .OR. RRIGE < ZERO) THEN
          SRIGE = INTMAX
        ELSE
          SRIGE = INT(RRIGE)
        ENDIF
        call my_alloc(RIGE_TMP(IDXIGE1)%ptr2, SRIGE, "RIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        RXIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RXIGE > INTMAX .OR. RXIGE < ZERO) THEN
          SXIGE = INTMAX
        ELSE
          SXIGE = INT(RXIGE)
        ENDIF
        call my_alloc(XIGE_TMP(IDXIGE1)%ptr2, SXIGE, "XIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGEO')
        ENDIF
        RVIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RVIGE > INTMAX .OR. RVIGE < ZERO) THEN
          SVIGE = INTMAX
        ELSE
          SVIGE = INT(RVIGE)
        ENDIF
        call my_alloc(VIGE_TMP(IDXIGE1)%ptr2, SVIGE, "VIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGEO')
        ENDIF
C
        SNIGE = 0
        SRIGE = 0
        SXIGE = 0
        SVIGE = 0
        IDXIGECNT = IDXIGECNT + 1
        IDXIGE1   = MOD(IDXIGECNT,2)
        IDXIGE2   = MOD(IDXIGECNT+1,2)
        FLAGG  = 0
        INSEG    = 0
        NUMFAKENODIGEO = 0
        IADBOXMAX = 1
C
        CALL HM_READ_SURF(
     1       ITAB      ,ITABM1    ,
     2       IGRSURF   ,IXS       ,IXQ       ,IXC       ,IXT       ,
     3       IXP       ,IXR       ,IXTG
     4                 ,IPART     ,IPARTS    ,IPARTQ    ,IPARTC    ,
     5       IPARTT    ,IPARTP    ,IPARTR    ,IPARTG    ,X         ,
     6       SBUFSF    ,ISKWN     ,SKEW      ,
     7       RWORK     ,KNOD2ELS  ,NOD2ELS   ,SH4TREE   ,SH3TREE   ,
     8       ISUBMOD   ,FLAGG     ,UNITAB    ,IBOX      ,
     9       IXS10     ,IXS16     ,IXS20     ,RTRANS,
     A       LSUBMODEL ,KNOD2ELC  ,NOD2ELC   ,KNOD2ELTG  ,NOD2ELTG,
     B       KXIG3D    ,IXIG3D    ,IPARTIG3D ,
     C       KNOT      ,IGEO      ,WIGE      ,KNOD2ELIG3D,NOD2ELIG3D,
     D       V         ,NIGE_TMP(IDXIGE1)%ptr,
     E       RIGE_TMP(IDXIGE1)%ptr2,XIGE_TMP(IDXIGE1)%ptr2,
     F       VIGE_TMP(IDXIGE1)%ptr2,IADTABIGE,DECALIGEO,IADBOXMAX,KNOD2ELQ,
     G       NOD2ELQ   ,SUBSETS   ,IGRBRIC   ,IGRSH4N   ,IGRSH3N   ,
     F       KNOTLOCPC ,KNOTLOCEL ,NSETS     ,MAP_TABLES)
C
C- Isogeometric Elements
        call my_alloc(NIGE_TMP(IDXIGE2)%ptr, SNIGE+NUMFAKENODIGEO, "NIGE_TMP(IDXIGE2)%ptr", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='NIGE')
        ENDIF
        SNIGE = SNIGE + NUMFAKENODIGEO
        CALL MY_DEALLOC(NIGE_TMP(IDXIGE1)%ptr)

        call my_alloc(RIGE_TMP(IDXIGE2)%ptr2, SRIGE+3*NUMFAKENODIGEO, "RIGE_TMP(IDXIGE2)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        SRIGE = SRIGE + 3*NUMFAKENODIGEO
        CALL MY_DEALLOC(RIGE_TMP(IDXIGE1)%ptr2)

        call my_alloc(XIGE_TMP(IDXIGE2)%ptr2, SXIGE+3*NUMFAKENODIGEO, "XIGE_TMP(IDXIGE2)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF
        SXIGE = SXIGE + 3*NUMFAKENODIGEO
        CALL MY_DEALLOC(XIGE_TMP(IDXIGE1)%ptr2)

        call my_alloc(VIGE_TMP(IDXIGE2)%ptr2, SVIGE+3*NUMFAKENODIGEO, "VIGE_TMP(IDXIGE2)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
        SVIGE = SVIGE + 3*NUMFAKENODIGEO
        CALL MY_DEALLOC(VIGE_TMP(IDXIGE1)%ptr2)
C
C       fill, 1st level surfaces
        FLAGG = 1
        INSEG = 0
        CALL HM_READ_SURF(
     1       ITAB      ,ITABM1    ,
     2       IGRSURF   ,IXS       ,IXQ       ,IXC       ,IXT       ,
     3       IXP       ,IXR       ,IXTG
     4                 ,IPART     ,IPARTS    ,IPARTQ    ,IPARTC    ,
     5       IPARTT    ,IPARTP    ,IPARTR    ,IPARTG    ,X         ,
     6       SBUFSF    ,ISKWN     ,SKEW      ,
     7       RWORK     ,KNOD2ELS  ,NOD2ELS   ,SH4TREE   ,SH3TREE   ,
     8       ISUBMOD   ,FLAGG     ,UNITAB    ,IBOX      ,
     9       IXS10     ,IXS16     ,IXS20     ,RTRANS,
     A       LSUBMODEL ,KNOD2ELC  ,NOD2ELC   ,KNOD2ELTG  ,NOD2ELTG ,
     B       KXIG3D    ,IXIG3D    ,IPARTIG3D ,
     C       KNOT      ,IGEO      ,WIGE      ,KNOD2ELIG3D,NOD2ELIG3D,
     D       V         ,NIGE_TMP(IDXIGE2)%ptr,
     E       RIGE_TMP(IDXIGE2)%ptr2,XIGE_TMP(IDXIGE2)%ptr2,
     F       VIGE_TMP(IDXIGE2)%ptr2,IADTABIGE,DECALIGEO,IADBOXMAX,KNOD2ELQ,
     G       NOD2ELQ   ,SUBSETS   ,IGRBRIC   ,IGRSH4N   ,IGRSH3N   ,
     H       KNOTLOCPC ,KNOTLOCEL ,NSETS     ,MAP_TABLES)
C
c        IF(NUMELIG3D>0) THEN
        IF(NUMFAKENODIGEO>0) THEN
        CALL MY_ALLOC(PERMIGE,NUMFAKENODIGEO,"PERMIGE")
c        ALLOCATE(PERMIGE(IADTABIGE)   ,STAT=stat)
c
        CALL PRESEARCHIGEO3D(IGRSURF,XIGE_TMP(IDXIGE2)%ptr2,PERMIGE)
c        CALL MYQSORT3D(IADTABIGE,XIGE_TMP(IDXIGE2)%ptr2,PERMIGE)
c
        call my_alloc(NIGE_TMP(IDXIGE1)%ptr, SNIGE, "NIGE_TMP(IDXIGE1)%ptr", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF

        call my_alloc(RIGE_TMP(IDXIGE1)%ptr2, SRIGE, "RIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF

        call my_alloc(XIGE_TMP(IDXIGE1)%ptr2, SXIGE, "XIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF

        call my_alloc(VIGE_TMP(IDXIGE1)%ptr2, SVIGE, "VIGE_TMP(IDXIGE1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
c
c        CALL SEARCHIGEO3D2(IGRSURF   ,IADTABIGE   ,PERMIGE  ,
c     .                    NIGE_TMP(IDXIGE2)%ptr   ,NIGE_TMP(IDXIGE1)%ptr,
c     .                    RIGE_TMP(IDXIGE2)%ptr2  ,RIGE_TMP(IDXIGE1)%ptr2,
c     .                    XIGE_TMP(IDXIGE2)%ptr2  ,XIGE_TMP(IDXIGE1)%ptr2,
c     .                    VIGE_TMP(IDXIGE2)%ptr2  ,VIGE_TMP(IDXIGE1)%ptr2,
c     .                    NDOUBLONIGE)

        CALL SEARCHIGEO3D(IGRSURF   ,IADTABIGE   ,PERMIGE  ,
     .                    NIGE_TMP(IDXIGE2)%ptr   ,NIGE_TMP(IDXIGE1)%ptr,
     .                    RIGE_TMP(IDXIGE2)%ptr2  ,RIGE_TMP(IDXIGE1)%ptr2,
     .                    XIGE_TMP(IDXIGE2)%ptr2  ,XIGE_TMP(IDXIGE1)%ptr2,
     .                    VIGE_TMP(IDXIGE2)%ptr2  ,VIGE_TMP(IDXIGE1)%ptr2,
     .                    NDOUBLONIGE)

c
        CALL MY_DEALLOC(RIGE_TMP(IDXIGE2)%ptr2)
        CALL MY_DEALLOC(XIGE_TMP(IDXIGE2)%ptr2)
        CALL MY_DEALLOC(VIGE_TMP(IDXIGE2)%ptr2)
c
        SNIGE = NUMFAKENODIGEO
        SRIGE = 3*NUMFAKENODIGEO
        SXIGE = 3*NUMFAKENODIGEO
        SVIGE = 3*NUMFAKENODIGEO

c        SNIGE = SNIGE - NDOUBLONIGE
c        SRIGE = SRIGE - 3*NDOUBLONIGE
c        SXIGE = SXIGE - 3*NDOUBLONIGE
c        SVIGE = SVIGE - 3*NDOUBLONIGE
c
        call my_alloc(NIGE_TMP(IDXIGE2)%ptr, SNIGE, "NIGE_TMP(IDXIGE2)%ptr", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF
        DO I=1,SNIGE
           NIGE_TMP(IDXIGE2)%ptr(I) = NIGE_TMP(IDXIGE1)%ptr(I)
        ENDDO
c
        call my_alloc(RIGE_TMP(IDXIGE2)%ptr2, SRIGE, "RIGE_TMP(IDXIGE2)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        DO I=1,SRIGE
           RIGE_TMP(IDXIGE2)%ptr2(I) = RIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        call my_alloc(XIGE_TMP(IDXIGE2)%ptr2, SXIGE, "XIGE_TMP(IDXIGE2)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF
        DO I=1,SXIGE
           XIGE_TMP(IDXIGE2)%ptr2(I) = XIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        call my_alloc(VIGE_TMP(1)%ptr2, SVIGE, "VIGE_TMP(1)%ptr2", stat=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
        DO I=1,SVIGE
           VIGE_TMP(IDXIGE2)%ptr2(I) = VIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        TAGSURFIGE=1
        CALL MY_DEALLOC(RIGE_TMP(IDXIGE1)%ptr2)
        CALL MY_DEALLOC(XIGE_TMP(IDXIGE1)%ptr2)
        CALL MY_DEALLOC(VIGE_TMP(IDXIGE1)%ptr2)
        CALL MY_DEALLOC(PERMIGE)
c
      ENDIF
c
C-------
C       READING SURFACES OF SURFACES
C-------
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          FLAGG = 0
          ITER  = ITER  + 1
          INSEG = 0
C---      count next level
          CALL HM_READ_SURFSURF(IGRSURF, INSEG, FLAGG, ICOUNT, ITER, NSETS, LSUBMODEL)
C---
C-------------------------------------------------
          FLAGG = 1
C---      fill next level
          CALL HM_READ_SURFSURF(IGRSURF, INSEG, FLAGG, ICOUNT, ITER, NSETS, LSUBMODEL)
C---
        ENDDO
      ENDIF

      NSEGS=NPART
!
      NSEGSMAX=0
      DO I = 1,NSURF
        NSEGS=NSEGS+IGRSURF(I)%NSEG
      ENDDO
      DO I = 1,NGRNOD
        NSEGSMAX= MAX(NSEGSMAX,IGRNOD(I)%NENTITY)
      ENDDO
      DO I = 1,NGRBRIC
        NSEGSMAX= MAX(NSEGSMAX,IGRBRIC(I)%NENTITY)
      ENDDO
      DO I = 1,NGRQUAD
        NSEGSMAX= MAX(NSEGSMAX,IGRQUAD(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSHEL
        NSEGSMAX= MAX(NSEGSMAX,IGRSH4N(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSH3N
        NSEGSMAX= MAX(NSEGSMAX,IGRSH3N(I)%NENTITY)
      ENDDO
      DO I = 1,NGRTRUS
        NSEGSMAX= MAX(NSEGSMAX,IGRTRUSS(I)%NENTITY)
      ENDDO
      DO I = 1,NGRBEAM
        NSEGSMAX= MAX(NSEGSMAX,IGRBEAM(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSPRI
        NSEGSMAX= MAX(NSEGSMAX,IGRSPRING(I)%NENTITY)
      ENDDO
      DO I = 1,NGRPART
        NSEGSMAX= MAX(NSEGSMAX,IGRPART(I)%NENTITY)
      ENDDO
      NSEGS=NSEGS+NSEGSMAX
C--------------------------------------------
C     READING LINES
C--------------------------------------------
      IF(NSLIN > 0) THEN
        WRITE(ISTDO,'(A)')' .. LINES '
        ERR_CATEGORY='LINES'
        FLAGG = 0
        IADBOXMAX = 1
!
        CALL HM_READ_LINES(
     1           ITAB    ,ITABM1  ,
     2           ISUBMOD ,IGRSLIN ,IGRSURF ,X       ,IXS    ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR    ,
     4           IXTG    ,IPART   ,IPARTS  ,IPARTQ  ,IPARTC ,
     5           IPARTT  ,IPARTP  ,IPARTR  ,IPARTG  ,
     6           NSEGS   ,         FLAGG   ,SKEW    ,ISKWN  ,
     7           UNITAB  ,IBOX    ,RTRANS  ,LSUBMODEL,
     8           IPARTX  ,KXX     ,IXX     ,IADBOXMAX,SUBSETS,
     9           IGRTRUSS,IGRBEAM,IGRSPRING,NSETS   ,MAP_TABLES)
C---
        FLAGG = 1
C---
        CALL HM_READ_LINES(
     1           ITAB    ,ITABM1  ,
     2           ISUBMOD ,IGRSLIN ,IGRSURF ,X       ,IXS    ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR    ,
     4           IXTG    ,IPART   ,IPARTS  ,IPARTQ  ,IPARTC ,
     5           IPARTT  ,IPARTP  ,IPARTR  ,IPARTG  ,
     6           NSEGS   ,         FLAGG   ,SKEW    ,ISKWN  ,
     7           UNITAB  ,IBOX    ,RTRANS  ,LSUBMODEL,
     8           IPARTX  ,KXX     ,IXX     ,IADBOXMAX,SUBSETS,
     9           IGRTRUSS,IGRBEAM,IGRSPRING,NSETS   ,MAP_TABLES)
C-------
C       READING LINES OF LINES
C-------
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          INSEG = 0
          FLAGG = 0
C---      count next level
          CALL HM_LINES_OF_LINES(IGRSLIN ,INSEG   ,FLAGG ,ICOUNT ,ITER ,NSETS, LSUBMODEL)
C---      fill next level
          FLAGG = 1
          CALL HM_LINES_OF_LINES(IGRSLIN ,INSEG   ,FLAGG ,ICOUNT ,ITER ,NSETS, LSUBMODEL)
C---
        ENDDO
      ENDIF
C--------------------------------------------
C     READING INITIAL CRACKS
C--------------------------------------------
      IF(NINICRACK > 0) THEN
        WRITE(ISTDO,'(A)')' .. INITIAL CRACK '
        ERR_CATEGORY='INITIAL CRACKS'
        SILEVSET = NINICRACK
        allocate(INICRACK(SILEVSET), stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='INICRACK')
        ! Reading /INICRACK cards
        CALL HM_READ_INICRACK(ITABM1   ,INICRACK ,UNITAB   ,LSUBMODEL)
C---
      ELSE
        SILEVSET = 0
        allocate(INICRACK(SILEVSET))
      ENDIF
C
C--------------------------------------------
C     READING NODE GROUPS
C--------------------------------------------
      IF(NGRNOD > 0)THEN
        WRITE(ISTDO,'(A)')' .. NODE GROUP'
        ERR_CATEGORY='NODE GROUPS'
        INNOD = 0
        MAXNNOD = 1
        IADBOXMAX = 1

        FLAGG = 0 !TAGNODES & ALLOCATE
        CALL HM_LECGRN(
     1           ITAB    ,ITABM1  ,IGRNOD  ,
     2           ISUBMOD ,X       ,GEO     ,IXS     ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR     ,
     4           IXTG                               ,IPART   ,
     5           IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,
     6           IPARTR  ,IPARTG  ,IPARTSP ,KXSP    ,
     7           FLAGG   ,MAXNNOD ,SKEW    ,ISKWN   ,
     8           UNITAB  ,IBOX    ,IXS10   ,IXS20   ,
     9           IXS16   ,RTRANS ,LSUBMODEL,IXX,
     A           KXX     ,IPARTX  ,IADBOXMAX,IGRSLIN,SUBSETS ,
     B           IGRBRIC ,IGRQUAD ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     C           IGRBEAM ,IGRSPRING,IGRSURF,NSETS   )
!
        FLAGG = 1 !BUILD GROUPS
        CALL HM_LECGRN(
     1           ITAB    ,ITABM1  ,IGRNOD  ,
     2           ISUBMOD ,X       ,GEO     ,IXS     ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR     ,
     4           IXTG                               ,IPART   ,
     5           IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,
     6           IPARTR  ,IPARTG  ,IPARTSP ,KXSP    ,
     7           FLAGG   ,MAXNNOD ,SKEW    ,ISKWN   ,
     8           UNITAB  ,IBOX    ,IXS10   ,IXS20   ,
     9           IXS16   ,RTRANS ,LSUBMODEL,IXX,
     A           KXX     ,IPARTX  ,IADBOXMAX,IGRSLIN,SUBSETS ,
     B           IGRBRIC ,IGRQUAD ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     C           IGRBEAM ,IGRSPRING,IGRSURF,NSETS   )

C-------
C       READING GROUPS OF GROUPS
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          FLAGG = 0
          CALL HM_GROGRONOD(IGRNOD ,ICOUNT ,FLAGG  ,ITER,'NODE',LSUBMODEL)
C---      fill next level
          FLAGG =1
          CALL HM_GROGRONOD(IGRNOD ,ICOUNT ,FLAGG  ,ITER,'NODE',LSUBMODEL)
C---
        ENDDO
      ENDIF

C--------------------------------------------
      CALL STOPTIME(19,1)
C--------------------------------------------
C     /SET
C--------------------------------------------
      CALL STARTIME(17,1)

      allocate(SET(NSETS))
      IF(NSETS > 0)THEN
        WRITE(ISTDO,'(A)')' .. SET'
        ERR_CATEGORY='SET'

        CALL HM_SET(SET      ,LSUBMODEL ,INV_GROUP   ,MAP_TABLES ,IPART     ,
     *              IGRSURF  ,IGRNOD    ,IGRSLIN     ,IGRPART    ,IGRBRIC   ,
     *              IGRQUAD  ,IGRSH4N   ,IGRSH3N     ,IGRTRUSS   ,IGRBEAM   ,
     *              IGRSPRING,IXS       ,IXS10       ,IXC        ,IXTG      ,
     *              KNOD2ELS ,NOD2ELS   ,KNOD2ELC    ,NOD2ELC    ,KNOD2ELTG ,
     *              NOD2ELTG ,IPARTC    ,IPARTG      ,IPARTS     ,SH4TREE   ,
     *              SH3TREE  ,IXQ       ,KNOD2ELQ    ,NOD2ELQ    ,X         ,
     *              IXT      ,IXP       ,IXR         ,IXX        ,KXX       ,
     *              KXSP     ,IXS20     ,IXS16       ,GEO        ,ITABM1    ,
     *              IBOX     ,SKEW      ,IPARTQ      ,IPARTT     ,IPARTP    ,
     *              IPARTR   ,SUBSETS   ,RBY_MSN     ,ISKWN      ,RTRANS    ,
     *              UNITAB   ,RWORK     ,SBUFSF      ,SISKWN     ,SSKEW     ,
     *              ROOTNAM  ,ROOTLEN   ,INFILE_NAME ,INFILE_NAME_LEN   )
     
     
C        IF(ALLOCATED(RBY_MSN)) DEALLOCATE(RBY_MSN)
      ENDIF
      
      IF (NSURF+NSETS > 0) THEN
        IF(SBUFSF > 0) THEN
          CALL MY_ALLOC(BUFSF,SBUFSF,"BUFSF")
          BUFSF = RWORK(1:SBUFSF)
          IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='BUFSF')
          ENDIF
        ENDIF
      ELSE
        CALL MY_ALLOC(BUFSF,0,"BUFSF")
      ENDIF
      IF(ALLOCATED(RWORK)) CALL MY_DEALLOC(RWORK)
C--------------------------------------------
      CALL STOPTIME(17,1)
C--------------------------------------------
C     READING DRAPES
C--------------------------------------------
      ERR_MSG='DRAPE'
      ERR_CATEGORY='DRAPE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NUMELC_DRAPE = 0
      NUMELTG_DRAPE = 0
      stack%s_pm2 = 0      
      IF(NDRAPE > 0) THEN
        WRITE(ISTDO,'(A)')' .. DRAPE'
        allocate(DRAPE_WRK(NUMELC + NUMELTG))
        call my_alloc(DRAPEG%INDX, NUMELC + NUMELTG, "DRAPEG%INDX")
        DRAPEG%INDX = 0  
        CALL MY_ALLOC(INDXSH,NUMELC + NUMELTG,"INDXSH")
        INDXSH = 0
C-----------------
C     Stack part Pre orginisation
C-------------------------
        NUMELC_DRAPE = 0
        NUMELTG_DRAPE = 0
        STDRAPE = 0
        SCDRAPE = 0
        IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
          allocate(IWORK_T(NUMELC+NUMELTG))
          CALL PRE_STACKGROUP(
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,IGEO_STACK ,IWORKSH    ,
     .                   IWORK_T  )
        ENDIF        
      !!   
       
         ITHIN_DRAPE = DEFAULTS%SHELL%ithin_drape
         CALL HM_READ_DRAPE(DRAPE_WRK ,IWORK_T       ,IWORKSH   ,IGRSH3N   ,IGRSH4N  ,
     .                     IXC       ,IXTG           ,IGEO       ,IGEO_STACK,LSUBMODEL,
     .                     UNITAB    ,INDXSH         ,ITHIN_DRAPE )
         IF( NUMELC_DRAPE  > 0) SCDRAPE = NUMELC
         IF( NUMELTG_DRAPE > 0) STDRAPE = NUMELTG
         allocate(DRAPE(NUMELC_DRAPE +NUMELTG_DRAPE))
         IF( (NUMELC_DRAPE + NUMELTG_DRAPE )> 0) THEN
           DRAPEG%NUMSH4 = NUMELC_DRAPE
           DRAPEG%NUMSH3 = NUMELTG_DRAPE 
           DO I=1,NUMELC_DRAPE + NUMELTG_DRAPE
             IDSHEL = INDXSH(I)  
             NPT_DRAPE = DRAPE_WRK(IDSHEL)%NPLY_DRAPE  
             DRAPE(I)%NPLY_DRAPE = NPT_DRAPE 
             NPT = IWORKSH(1,IDSHEL) 
             DRAPE(I)%NPLY = NPT 
             DRAPEG%INDX(IDSHEL) = I
             allocate(DRAPE(I)%DRAPE_PLY(NPT_DRAPE))
             call my_alloc(DRAPE(I)%INDX_PLY, NPT, "DRAPE(I)%INDX_PLY")
             DRAPE(I)%INDX_PLY = 0
             DO JJ =1,NPT_DRAPE                                                                                    
               IP = DRAPE_WRK(IDSHEL)%INDX_PLY(JJ)                                                                 
               DRAPE(I)%INDX_PLY(IP) = JJ                                                                      
               NSLICE = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE   
               DRAPE(I)%DRAPE_PLY(JJ)%NSLICE = NSLICE   
               DRAPE(I)%DRAPE_PLY(JJ)%IPID = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID 
               call my_alloc(DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE, NSLICE, 2, "DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE")
               call my_alloc(DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE, NSLICE, 2, "DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE")
               DO ISL = 1,NSLICE                                                                                     
                 DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(ISL,1)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1)           
                 DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(ISL,2)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2)           
                 DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(ISL,1)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1)           
                 DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(ISL,2)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) 
               ENDDO ! nbre of slice
             ENDDO                                                                                  
           ENDDO 
           !! Deallocation of work drape memory 
           DO I=1,NUMELC_DRAPE + NUMELTG_DRAPE
             IDSHEL = INDXSH(I)                                                 
             NPT = IWORKSH(1,IDSHEL)  
             IF(ALLOCATED(DRAPE_WRK(IDSHEL)%DRAPE_PLY)) THEN  
               NPT_DRAPE = DRAPE_WRK(IDSHEL)%NPLY_DRAPE                                                                         
               DO JJ=1,NPT_DRAPE 
                 IP = DRAPE_WRK(IDSHEL)%INDX_PLY(JJ)                          
                 DEALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE,DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE)
               ENDDO
               DEALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY) 
             ENDIF     
           ENDDO 
           DEALLOCATE(DRAPE_WRK)
           CALL MY_DEALLOC(INDXSH)
       ENDIF 
       IF(ALLOCATED(IDRAPEID)) CALL MY_DEALLOC(IDRAPEID)
     
         IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
            CALL STACKGROUP_DRAPE(DRAPE, DRAPEG , IWORK_T   , IWORKSH  ,
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,THKE       , STACK    ,
     .                   IGEO_STACK ,GEO_STACK , STACK_INFO ,
     .                   NUMGEOSTACK,NPROP_STACK,PLY_INFO)
            stack%s_pm2 = size(stack%pm,dim=2)
         ELSE
            CALL MY_ALLOC(STACK%GEO,0,0,"STACK%GEO")
            CALL MY_ALLOC(STACK%IGEO,0,0,"STACK%IGEO")
            CALL MY_ALLOC(STACK%PM,stack_s_pm1,stack%s_pm2,"STACK%PM")
         ENDIF        
      ELSE ! with out drape
        allocate(DRAPE(0))
        CALL MY_ALLOC(DRAPEG%INDX,0,"DRAPEG%INDX")
        IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
            CALL STACKGROUP(
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,IWORKSH    ,THKE      ,
     .                   STACK      ,IPM        ,IGEO_STACK ,GEO_STACK ,
     .                   STACK_INFO ,NUMGEOSTACK,NPROP_STACK)
            stack%s_pm2 = size(stack%pm,dim=2)     
         ELSE
            CALL MY_ALLOC(STACK%GEO,0,0,"STACK%GEO")
            CALL MY_ALLOC(STACK%IGEO,0,0,"STACK%IGEO")
            CALL MY_ALLOC(STACK%PM,stack_s_pm1,stack%s_pm2,"STACK%PM")
         ENDIF  
      ENDIF
C--------------------------------------------
      IF(NSUBDOM==0) GOTO 218
C--------------------------------------------
C     MULTIDOMAINS - INTERFACES
C--------------------------------------------
         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS INTERFACES DETECTION '
         IDXCNT= 1  ! used by temporary local array "IBUFTMP"
         IADBUF= 1
         IDXCNT= IDXCNT + 1
         IDX1  = MOD(IDXCNT,2)
         IDX2  = MOD(IDXCNT+1,2)
         FLAGG = 0
         INNOD = 0
         FLG_R2R_ERR = 0
         ALE_EULER = 0
         NSPCOND0 = NSPCOND
! tmp +++
         RSIBUFSSG=NUMNOD+NSUBDOM
         IF(RSIBUFSSG > INTMAX .OR. RSIBUFSSG < ZERO) THEN
           SIBUFSSG = INTMAX
         ELSE
           SIBUFSSG = INT(RSIBUFSSG)
         ENDIF
         call my_alloc(IBUFTMP(IDX1)%ptr, SIBUFSSG, "IBUFTMP(IDX1)%ptr", stat=stat)
          IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='BUFFSG')
         ENDIF
! tmp ---
C---
         CALL MY_ALLOC(TAGNO,2*NUMNOD+NPART,"TAGNO")
         TAGNO(:) = 0
         CALL MY_ALLOC(NALE_R2R,ALE%GLOBAL%SNALE,"NALE_R2R")
         NALE_R2R(:) = 1
         CALL MY_ALLOC(DT_R2R,4*NSUBDOM,"DT_R2R")
         DT_R2R(:) = 0
C--- First pass-> count
         CALL R2R_GROUP(NGRNOD,
     1        INNOD,FLAGG,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,
     2        IPARTR,IPARTG,IPARTSP,IXS10,IXS20,IXS16,1,
     3        IBUFTMP(IDX1)%ptr,IXR_KJ,INOM_OPT,IPART,
     4        IADBUF,NALE_R2R,FLG_R2R_ERR ,
     5        STACK%PM ,IWORKSH  ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,
     6        IGRSH3N  ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRNOD  ,
     7        IGRSURF  ,IGRSLIN, LSUBMODEL,ALE_EULER ,IGEO    ,
     8        NLOC_DMG ,DETONATORS,SEATBELT_SHELL_TO_SPRING,
     9        NB_SEATBELT_SHELLS,MAT_ELEM%MAT_PARAM,NEBCS)
C---
         call my_alloc(IBUFTMP(IDX2)%ptr, SIBUFSSG+INNOD, "IBUFTMP(IDX2)%ptr", stat=stat)
         IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='BUFFSG')
         ENDIF
         IBUFTMP(IDX2)%ptr = 0
         DO I=1,SIBUFSSG
            IBUFTMP(IDX2)%ptr(I) = IBUFTMP(IDX1)%ptr(I)
         ENDDO
         SIBUFSSG = SIBUFSSG+INNOD
         CALL MY_DEALLOC(IBUFTMP(IDX1)%ptr)

         FLAGG = 1

C---  Second Pass -> interface creation
         CALL R2R_GROUP(NGRNOD,
     1        INNOD,FLAGG,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,
     2        IPARTR,IPARTG,IPARTSP,IXS10,IXS20,IXS16,2,
     3        IBUFTMP(IDX2)%ptr,IXR_KJ,INOM_OPT,IPART,
     4        IADBUF,NALE_R2R,FLG_R2R_ERR ,
     5        STACK%PM ,IWORKSH  ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,
     6        IGRSH3N  ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRNOD  ,
     7        IGRSURF  ,IGRSLIN, LSUBMODEL,ALE_EULER ,IGEO    ,
     8        NLOC_DMG ,DETONATORS,SEATBELT_SHELL_TO_SPRING,
     9        NB_SEATBELT_SHELLS,MAT_ELEM%MAT_PARAM,NEBCS)
C--------------------------------------------
C     MULTIDOMAINS - ARRAY SPLITTING
C--------------------------------------------
         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DATA SPLIT '
C--- First pass-> count
         CALL R2R_SPLIT(
     1             NSLIN,
     2             NSURF,0,EANI,IBUFTMP(IDX2)%ptr,IXR_KJ,
     3             INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4             SUBSETS,IGRSURF,IGRNOD,IGRBRIC,IGRQUAD,
     5             IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6             IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN,IWORKSH,
     7             SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
C--- second pass-> split
         CALL R2R_SPLIT(
     1             NSLIN,
     2             NSURF,1,EANI,IBUFTMP(IDX2)%ptr,IXR_KJ,
     3             INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4             SUBSETS,IGRSURF,IGRNOD,IGRBRIC,IGRQUAD,
     5             IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6             IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN,IWORKSH,
     7             SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
         CALL MY_DEALLOC(IBUFTMP(IDX2)%ptr)
C--------------------------------------------
C     MULTIDOMAINS - DATA STRUCTURE UPDATE
C--------------------------------------------

         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DATA UPDATE '
         CALL MY_DEALLOC(KNOD2ELC)
         CALL MY_DEALLOC(KNOD2ELTG)
         CALL MY_DEALLOC(KNOD2EL1D)
         CALL MY_DEALLOC(KNOD2ELIG3D)
         CALL MY_DEALLOC(KNOD2ELQ)
         DEALLOCATE(KNOD2ELS)
         CALL MY_DEALLOC(NOD2ELS)
         CALL MY_DEALLOC(NOD2ELC)
         CALL MY_DEALLOC(NOD2ELTG)
         CALL MY_DEALLOC(NOD2EL1D)
         CALL MY_DEALLOC(NOD2ELIG3D)
         CALL MY_DEALLOC(NOD2ELQ)
         CALL MY_DEALLOC(XYZREF)
         CALL MY_ALLOC(XYZREF,3*NUMNOD,"XYZREF")
         FLAG_GOTO = 1

C-----reallocating FRONT array-------------
         CALL INI_IFRONT()
         IENTRY2(1:NUMNOD) = -1
         FLAGKIN(1:NUMNOD) = 0

cc r2r with new IFRONT
         DO I=1,NUMNOD
           IF(FRONT_R2R(I)==1)THEN
             CALL IFRONTPLUS(I,1)
             !FLAGKIN array to identify boundary nodes with
             !kinematic constraints (old FRONT TAG=10)
             IF(FLAGKIN_R2R(I)==1)FLAGKIN(I)=1
           ENDIF
         ENDDO

         DEALLOCATE(FRONT_R2R,FLAGKIN_R2R)
         SFRONTB_R2R = NUMNOD
         SNOM_OPT_OLD = SNOM_OPT
         IN10 = INOM_OPT(10)
         IN20 = INOM_OPT(20)

C-----ARRAY size update & rebuild THKEC--
         GOTO 208
209      CONTINUE

C----- OPTION name update ----------
         GOTO 210
211      CONTINUE
         CALL R2R_NOM_OPT(NOM_OPT,INOM_OPT,IN10,IN20,SNOM_OPT_OLD)

C-----repointing IXS10,20,16 arrays--
         GOTO 212
213      CONTINUE

C-----reallocating NOD2EL----------------
         GOTO 214
215      CONTINUE

C-----repointing IPART arrays--------
         GOTO 216
217      CONTINUE

C-----recalculating inverse connectivities--------

         CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
         IF(ALLOCATED(IWORK)) CALL MY_DEALLOC(IWORK)

C-----reinitializing user pointers-------
         CALL SAV_BUF_POINT(PM     ,1)
         CALL SAV_BUF_POINT(BUFMAT ,2)
         CALL SAV_BUF_POINT(GEO    ,3)
         CALL SAV_BUF_POINT(ISKWN  ,7)
         CALL SAV_BUF_POINT(SKEW   ,8)
         CALL SAV_BUF_POINT(IPM ,11)
         CALL SAV_BUF_POINT(IGEO,12)

C--------------------------------------------

218   CONTINUE
         IF(.NOT. ALLOCATED(TAGNO)) ALLOCATE(TAGNO(0))
         IF(.NOT. ALLOCATED(NALE_R2R)) ALLOCATE(NALE_R2R(0))
         IF(.NOT. ALLOCATED(DT_R2R)) ALLOCATE(DT_R2R(0))
C--------------------------------------------
C     MULTIDOMAINS - INTERFACE CHECK
C--------------------------------------------

C---  Check multidomains datas
      IF(NR2RLNK/=0) THEN
         CALL R2R_CHECK(IEXLNK,IGRNOD,IPART)
      ENDIF
C--- Allocation of FRONTB_R2R
      CALL MY_ALLOC(FRONTB_R2R,SFRONTB_R2R,NSPMD,"FRONTB_R2R")
      FRONTB_R2R = 0

C ------------------------------------------------
C Sorting of surfaces for airbags and modification files
C ------------------------------------------------
      CALL SORT_SURF(IGRSURF,IXS,IXC,IXTG,IXQ,IXP,IXR,IXT, KXX,NIXX)
C--------------------------------------------
      CALL PRINTGROUP(
     1                ITAB    ,ITABM1   ,IGRNOD   ,NINICRACK,BUFSF   ,
     2                IGRSURF ,IGRSLIN  ,INICRACK ,IXS      ,IXQ     ,
     3                IXC     ,IXT     ,IXP     ,IXR     ,IXTG    ,
     4                IXS10   ,IXS20    ,IXS16    ,IPART    ,
     5                KXX     ,IXIG3D   ,KXIG3D   ,
     6                IGRQUAD ,IGRBRIC  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS,
     7                IGRBEAM ,IGRSPRING,IGRPART  )
      CALL TRACE_OUT1()
      CALL MY_DEALLOC(KXX)
      CALL MY_DEALLOC(IXX)
      CALL MY_DEALLOC(LELX)
C--------------------------------------------
C     TRANSFORMATIONS
C--------------------------------------------
      ERR_MSG='TRANSFORMATIONS'
      ERR_CATEGORY='TRANSFORMATIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL LECTRANS(X        ,IGRNOD  ,ITAB   ,ITABM1 ,UNITAB,
     .              LSUBMODEL,RTRANS  ,IGRSURF,ISKWN  ,SKEW  ,
     .              LISKN    ,LSKEW   ,NSPCOND,NUMSPH ,SISKWN,
     .              SSKEW    )
      CALL TRACE_OUT1()
      CLOSE(UNIT=IUSBM)
C--------------------------------------------
C     READING OF /MERGE/NODE
C--------------------------------------------
      ERR_MSG='/MERGE/NODE'
      ERR_CATEGORY='/MERGE/NODE'
C
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      CALL MY_ALLOC(MERGE_NODE_TAB,4*NB_MERGE_NODE,"MERGE_NODE_TAB")
      CALL MY_ALLOC(MERGE_NODE_TOL,NB_MERGE_NODE,"MERGE_NODE_TOL")
      MERGE_NODE_TAB = 0
      MERGE_NODE_TOL = ZERO
      NMERGE_NODE_CAND = 0
      NMERGE_NODE_DEST = 0
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='IMERGE')
      ENDIF
      IF(NB_MERGE_NODE > 0) THEN
        CALL HM_READ_MERGE_NODE(X,LSUBMODEL,UNITAB,IGRNOD,MERGE_NODE_TAB,
     .                          MERGE_NODE_TOL,NMERGE_NODE_CAND,NMERGE_NODE_DEST)
      ENDIF
C
      CALL TRACE_OUT1()
C--------------------------------------------
C     MERGE OF NODES (cnodes + /MERGE/NODE)
C--------------------------------------------
      ERR_MSG='MERGING NODES'
      ERR_CATEGORY='MERGING NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      NMERGE_TOT = NUMCNOD + NMERGE_NODE_DEST
      CALL MY_ALLOC(IMERGE,3*NMERGE_TOT,"IMERGE")
      CALL MY_ALLOC(IMERGE2,NUMNOD+1,"IMERGE2")
      CALL MY_ALLOC(IADMERGE2,NUMNOD+1,"IADMERGE2")
C
      IMERGE = 0
      IMERGE2 = 0
      IADMERGE2 =0
      NMERGED = 0
C
C--   CNODE merging
      IF(NUMCNOD > 0)
     .    CALL MERGE(X    ,ITAB   ,ITABM1  ,CMERGE ,IMERGE,
     .               IMERGE2,IADMERGE2,NMERGE_TOT)
      CALL MY_DEALLOC(CMERGE)
C
C--   /MERGE/NODE merging
      IF(NB_MERGE_NODE > 0)
     .    CALL MERGE_NODE(X    ,ITAB   ,ITABM1  ,IMERGE,IMERGE2,
     .                    IADMERGE2,NMERGE_TOT,MERGE_NODE_TAB,MERGE_NODE_TOL,
     .                    NMERGE_NODE_CAND,NMERGE_NODE_DEST,IXS,IXS10,IXS20,
     .                    IXS16,IXQ,IXC,IXT,IXP,
     .                    IXR,IXTG,EANI,IGRNOD)
C
      CALL TRACE_OUT1()
C--------------------------------------------
C     Reinitialize merged connectivities / groups
C--------------------------------------------
      ERR_MSG='REINIT CONNECTIVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NMERGED > 0) THEN
        CALL RECONNECT(
     .           IXS      ,IXS10    ,IXS20    ,IXS16    ,IXQ      ,
     .           IXC      ,IXT      ,IXP      ,IXR      ,IXTG     ,
     .           IGRNOD   ,IGRSURF  ,IGRSLIN  ,
     .           ISKWN    ,IMERGE   ,NMERGE_TOT)
C
        KNOD2ELS  = 0
        KNOD2ELC  = 0
        KNOD2ELTG = 0
        KNOD2EL1D  = 0
        KNOD2ELIG3D  = 0
        NOD2ELS   = 0
        NOD2ELC   = 0
        NOD2ELTG  = 0
        NOD2EL1D   = 0
        NOD2ELIG3D   = 0
        KNOD2ELQ  = 0
        NOD2ELQ  = 0
        CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING RANDOM NOISE
C--------------------------------------------
      ERR_MSG='RANDOM NOISE'
      ERR_CATEGORY='RANDOM NOISE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      CALL INIT_RANDOM( )

      IF(RAND_STRUCT%CMD) NRAND = NRAND + 1
      CALL MY_ALLOC(IRAND,NRAND,"IRAND")
      CALL MY_ALLOC(ALEA,NRAND,"ALEA")
      CALL MY_ALLOC(XSEED,NRAND,"XSEED")

      CALL HM_READ_RAND(X     ,IGRNOD   ,ITAB,IRAND,ALEA,XSEED,
     .                  UNITAB,LSUBMODEL)

      CALL TRACE_OUT1()
C--------------------------------------------
C     READING SLIPRINGS AND RETRACTORS AND /INTER/GUIDED_CABLE
C--------------------------------------------
      ERR_MSG='SEATBELTS'
      ERR_CATEGORY='SEATBELTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      NB_MAT_SEATBELT = 0
      CALL HM_OPTION_COUNT('/MAT/LAW114',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/SPR_SEATBELT',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/LAW119',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/SH_SEATBELT',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
C
      IF(NSLIPRING + NRETRACTOR > 0) WRITE(ISTDO,'(A)')' .. SLIPRING/RETRACTOR'
      IF(NSLIPRING > 0) CALL HM_READ_SLIPRING(
     1                                          LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     2                                          X,NPC1,NOM_OPT(LNOPT1*INOM_OPT(31)+1),ALEA,IGRNOD,
     2                                          IGRSH4N,IXC,IPM)
      IF(NRETRACTOR > 0) CALL HM_READ_RETRACTOR(
     1                                           LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     2                                           X,NPC1,NOM_OPT(LNOPT1*INOM_OPT(32)+1),ALEA,IPM)
C     
      IF (NGUIDED_CABLE > 0) CALL HM_READ_INTER_GUIDED_CABLE(LSUBMODEL,IGRNOD,NGRNOD,IGRPART,NGRPART,
     .                                                       NPART,UNITAB,NUMELR,IPARTR,IXR,
     .                                                       NIXR,NUMELP,IPARTP,IXP,NIXP,
     .                                                       NUMELT,IPARTT,IXT,NIXT,NSPMD,
     .                                                       KNOD2EL1D,NOD2EL1D,S_NOD2EL1D,NUMNOD,X,
     .                                                       NUMELS,NUMELQ,NUMELC,NINTSUB,LTITR,
     .                                                       LNOPT1,NOM_OPT(LNOPT1*INOM_OPT(4)+1),ITAB)
C      
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING BOUNDARY CONDITIONS
C--------------------------------------------
      ERR_MSG='BCS'
      ERR_CATEGORY='BCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NBCSCYC>0) THEN
        CALL HM_PREREAD_BCSCYC(IGRNOD ,NOM_OPT(LNOPT1*INOM_OPT(16)+1),LSUBMODEL,SLBCSCYC)
      END IF
      SIBCSCYC = 4*NBCSCYC
      call my_alloc(IBCSCYC, SIBCSCYC, "IBCSCYC", stat=stat)
      call my_alloc(LBCSCYC, SLBCSCYC, "LBCSCYC", stat=stat)
      LBCSCYC = 0
      IF(NUMBCS /= 0 .OR. NALEBCS /= 0 .OR. NUMBCSN /= 0) THEN
        WRITE(ISTDO,'(A)')TITRE(13)
C READ /BCS & /BCS/LAGMUL
        CALL HM_READ_BCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),
     .              UNITAB   ,LSUBMODEL,IBCSCYC,LBCSCYC)
C READ /ALE/BCS
        CALL HM_READ_ALEBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),
     .              LSUBMODEL)
C READ /NBCS
        CALL HM_READ_NBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .                    IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .                    IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),LSUBMODEL)
C PRINT /BCS
        CALL PRINTBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1), NBCSLAG)
      ENDIF
C--------------------------------------
C
      call my_alloc(ICODEP, 0, "ICODEP")
      call my_alloc(ISKEWP, 0, "ISKEWP")
      CALL TRACE_OUT1()
C     Adaptive meshing : Sending down the bcs
      ERR_MSG='ADAPTIVE MESHING BCS'
      ERR_CATEGORY='ADAPTIVE MESHING BCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NADMESH/=0)THEN
        CALL ADMBCS(IXC ,IPARTC,IXTG,IPARTG,IPART  ,
     .              ICODE,ISKEW,ITAB,SH4TREE,SH3TREE)
      END IF
      CALL TRACE_OUT1()
C--------------------------------------------------------------------------
C     SORTING BRICKS AND QUADS, CLASSIFICATION BY LAW
C--------------------------------------------------------------------------
      ERR_MSG='SOLIDS SORT'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIWORK = 2*MAX(NUMELS,NUMELQ)
      CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
      IF(NUMELS /= 0)
     .  CALL LCE16S3(
     .            IXS    ,BID13   ,PM      ,IWORK   ,ITAB    ,ITABM1  ,
     .            ICODE  ,IPARTS  ,IGRBRIC ,GEO     ,EANI    ,
     .            IXS10  ,IPART   ,IXS20   ,IXS16   ,KNOD2ELS,NOD2ELS ,
     .            IGRSURF,SPH2SOL ,SOL2SPH )
      IF(NUMELQ /= 0)
     .  CALL LCE16Q3(
     .            IXQ    ,BID13   ,PM      ,IWORK   ,ITAB    ,ITABM1  ,
     .            ICODE  ,IPARTQ  ,IGRQUAD ,IPM     ,IGEO    )
      IF(ALLOCATED(IWORK)) CALL MY_DEALLOC(IWORK)
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTI-POINT CONSTRAINTS (1)
C--------------------------------------------
      ERR_MSG='MPCS 1'
      ERR_CATEGORY='MPCS 1'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMMPC > 0) THEN
        CALL HM_READ_MPC0 (LMPC,LSUBMODEL)
      ELSE
        LMPC=0
      ENDIF
      SRBMPC = LMPC
      CALL MY_ALLOC(RBMPC,SRBMPC,"RBMPC")
      IF(SRBMPC > 0) RBMPC = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     DYNAMIC REAL STORAGE
C--------------------------------------------
      ERR_MSG='DYNAMIC STORAGE REAL'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SGJBUFR = LKJNR*NGJOINT
      SW      = 3*NUMNOD*IALE
      NUMELSK8     =    NUMELS
      NUMELQK8     =    NUMELQ
      SVEUL   = (LVEUL*NUMELS+10*NUMELQ)*IEULER
      SVEUL8  = (INT(LVEUL,KIND(SVEUL8))*NUMELSK8+10*NUMELQK8)*INT(IEULER,KIND(SVEUL8))
      SFILL   = NMULT*NUMNOD
      SDFILL  = NMULT*NUMNOD
      SALPH   = 2*NMULT*(NUMELQ+NUMELS)
      SWB     = 0
      IF(ALE%GRID%NWALE == 2) THEN
        SWB  = 3*NUMNOD
      ELSEIF(ALE%GRID%NWALE == 4) THEN
        SWB  = 4*NUMNOD
      ENDIF
      CALL MY_ALLOC(WB,SWB,"WB")
      IF(SWB > 0) WB = ZERO
      IF(ALE%GRID%NWALE == 4) THEN
        WMA => WB(3*NUMNOD+1:SWB)
      ELSE
        WMA => WB
      ENDIF
C
      SDSAVE   = 0
      SASAVE   = 0
      IF(ILAG == 1 .AND. (IALE+IEULER) > 0) THEN
        SDSAVE  = 3*NUMNOD
        SASAVE  = 3*NUMNOD
      ENDIF
      CALL MY_ALLOC(GJBUFR,SGJBUFR,"GJBUFR")
      CALL MY_ALLOC(W,SW,"W")
      CALL MY_ALLOC(VEUL,SVEUL8,"VEUL")
      CALL MY_ALLOC(FILL,SFILL,"FILL")
      CALL MY_ALLOC(DFILL,SDFILL,"DFILL")
      CALL MY_ALLOC(ALPH,SALPH,"ALPH")
      CALL MY_ALLOC(DSAVE,SDSAVE,"DSAVE")
      CALL MY_ALLOC(ASAVE,SASAVE,"ASAVE")
      IF(SGJBUFR > 0)  GJBUFR  = ZERO
      IF(SW > 0)       W       = ZERO
      IF(SVEUL8 > 0)    VEUL    = ZERO
      IF(SFILL > 0)    FILL    = ZERO
      IF(SDFILL > 0)   DFILL   = ZERO
      IF(SALPH > 0)    ALPH    = ZERO
      IF(SDSAVE > 0)   DSAVE   = ZERO
      IF(SASAVE > 0)   ASAVE   = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING COMPLEX JOINTS (GJOINT)
C--------------------------------------------
      ERR_MSG='GJOINTS'
      ERR_CATEGORY='GJOINTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      JOINT_SMS = .FALSE.
      IF(ISMS/=0) JOINT_SMS = .TRUE.
      IF(NGJOINT/=0) CALL HM_READ_GJOINT(
     1           GJBUFI  ,GJBUFR  ,ITAB    ,ITABM1  ,X       ,
     2           MS      ,IN      ,LAG_NCF ,LAG_NKF ,LAG_NHF ,
     3           D       ,UNITAB  ,IKINE1LAG,NOM_OPT(LNOPT1*INOM_OPT(18)+1),LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     READER FOR DETONATOR OPTIONS (/DFS/DET*)
C--------------------------------------------
      ERR_MSG='DETONATORS'
      ERR_CATEGORY='DETONATORS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      !new Reader
      CALL READ_DETONATORS(ITABM1   ,ITAB      ,IGRNOD  ,
     .                     PM       ,IPM       ,X       ,UNITAB ,
     .                     LSUBMODEL,DETONATORS,XFRAME  ,IFRAME)
      CALL TRACE_OUT1()
C--------------------------------------------
C     A.L.E.
C     ELEMENT NEIGHBOR (OR FACET) ARRAYS
C     NEIGHBOR NODE ARRAYS
C--------------------------------------------
      CALL ALE_CONNECTIVITY%ALE_CONNECTIVITY_INIT()
      ERR_MSG='ALE LINKS'
      ERR_CATEGORY='ALE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NALELK/=0) THEN
        WRITE(ISTDO,'(A)')TITRE(29)
        LLINAL = 7 * NALELK
        SLINALE=LLINAL
        CALL MY_ALLOC(LINALE,SLINALE,"LINALE")
        IF(IERR/=0) THEN
          WRITE(IOUT,*) ' ** ERROR IN MEMORY ALLOCATION'
          WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
        ENDIF
        CALL HM_READ_ALE_LINK(ICODE, ISKEW, ITAB, ITABM1, D,
     .       IGRNOD, IBCSLAG, LAG_NCF, LAG_NKF, LAG_NHF,
     .       IKINE1LAG, LINALE, LSUBMODEL, UNITAB,NSUBDOM)
      ELSE
        CALL MY_ALLOC(LINALE,0,"LINALE")
      ENDIF
      ERR_MSG='ALE NEIGHBOURS'
      IF(IALE+IEULER+IALELAG /= 0)THEN
       WRITE(ISTDO,'(A)')TITRE(30)
       CALL ALELEC(
     1     ICODE    ,IXS      ,IXQ       ,IXC               ,IXT     ,
     2     IXTG     ,PM       ,IGEO      ,ITAB              ,GEO     ,
     3     NALE_R2R ,NSUBDOM  ,MULTI_FVM ,ALE_CONNECTIVITY,GLOB_THERM%ITHERM,DETONATORS%IS_SHADOWING_REQUIRED)
C     ---------------------------------------------------------------
C     Unplug neighbor elements in case of direct lagrangian coupling
C     ---------------------------------------------------------------
       IF(MULTI_FVM%IS_USED) THEN
          CALL MULTI_UNPLUG_NEIGHBORS(ALE_CONNECTIVITY, IXS, IXQ, IXTG)
       ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTIMATERIALS
C     INITIALIZATION OF NODAL PERCENTAGES
C--------------------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
       IF(NMULT>0)THEN
        WRITE(ISTDO,'('' .. MULTIMATERIALS'')')
        IF(NUMELS>0)
     .      CALL INIMU3(PM     ,IXS    ,FILL   ,DFILL  )
        IF(NUMELQ>0)
     .      CALL INIMU2(PM     ,IXQ    ,FILL   ,DFILL  )
          CALL INIMUL  (PM     ,FILL   ,DFILL  ,MAT20_DISCRETE_FILL)
       ENDIF
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------------
C     DETECTION OF ELEMENT LAW 6 WALL---->LAW 17
C---------------------------------------------------
      ERR_MSG='CFD BOUNDARY ELEMENTS'
      ERR_CATEGORY='CFD BOUNDARY ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER/=0)
     +   CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
      CALL TRACE_OUT1()
C--------------------------------------------
C     DYNAMIC INTEGER STORAGE (LOADING)
C--------------------------------------------
C      READING MULTI-PURPOSE ELEMENTS.
C---------------------------------------------
      ERR_MSG='MULTI-PURPOSE ELEMENTS'
      ERR_CATEGORY='MULTI-PURPOSE ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELX > 0) THEN
        SKXX = NIXX*NUMELX
        CALL HM_PREREAD_XELEM(SIXX, IGRNOD,LSUBMODEL)
        CALL MY_ALLOC(KXX,SKXX,"KXX")
        CALL MY_ALLOC(IXX,SIXX+150,"IXX")
        KXX = 0
        IXX = 0
        CALL MY_ALLOC(LELX,NUMELX,"LELX")
        LELX(1:NUMELX) = 0
        CALL HM_READ_XELEM(IGRNOD  ,ITAB   ,ITABM1 ,IPART ,IPARTX,
     .              IPM     ,IGEO   ,KXX    ,IXX   ,LSUBMODEL)
      ELSE
        SKXX = 0
        SIXX = 0
        CALL MY_ALLOC(KXX,SKXX,"KXX")
        CALL MY_ALLOC(IXX,SIXX,"IXX")
        CALL MY_ALLOC(LELX,NUMELX,"LELX")
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING SPH SYMMETRY CONDITIONS.
C---------------------------------------------
      ERR_MSG='SPH SYM'
      ERR_CATEGORY='SPH SYM'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SISPSYM  = NSPCOND*NUMSPH
      SISPCOND = NSPCOND*NISPCOND
      CALL MY_ALLOC(ISPSYM,SISPSYM,"ISPSYM")
      CALL MY_ALLOC(ISPCOND,SISPCOND,"ISPCOND")
      IF(NSPCOND > 0) THEN
        ISPSYM  = 0
        ISPCOND = 0
         WRITE(ISTDO,'(A)')' .. SPH SYMMETRY CONDITIONS'
        CALL HM_READ_SPCND(ISPCOND ,ISKEW   ,ITAB    ,ITABM1  ,D       ,
     .               IGRNOD  ,NOD2SP ,IFRAME ,NOM_OPT(LNOPT1*INOM_OPT(23)+1),
     .               LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------
C      READING SPH INLET/OUTLET.
C---------------------------------------------
      ERR_MSG='SPH I/O'
      ERR_CATEGORY='SPH I/O'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      LWASPIO=0
      SISPHIO = NISPHIO*NSPHIO
      SSPHVELN= NSPHIO*NUMSPH*2
      CALL MY_ALLOC(ISPHIO,SISPHIO,"ISPHIO")
      CALL MY_ALLOC(SPHVELN,SSPHVELN,"SPHVELN")
      LVSPHIO = 0
      IF(NSPHIO > 0)THEN
         WRITE(ISTDO,'(A)')' .. SPH INLET/OUTLET DEFINITION'
         ISPHIO = 0
         NSEG_IO = 0
         CALL HM_PREREAD_SPHIO(IGRSURF   ,SVSPHIO      ,
     .                   NOM_OPT(LNOPT1*INOM_OPT(22)+1),
     .                   LSUBMODEL)
         CALL MY_ALLOC(VSPHIO,SVSPHIO,"VSPHIO")
         VSPHIO(1:SVSPHIO)=ZERO

c         ALLOCATE(VSPHIO2(SVSPHIO2)  ,STAT=stat)
c         VSPHIO2(1:SVSPHIO2)=ZERO

         CALL HM_READ_SPHIO(ISPHIO   ,VSPHIO   ,IPART    ,IGRSURF  ,
     .                      NOD2SP   ,IPARTSP  ,ITAB     ,X        ,
     .                      LVSPHIO  ,LWASPIO  ,ITABM1   ,UNITAB   ,
     .                      LSUBMODEL,RTRANS   ,NRTRANS )
C---- -----------------------------------------
C        PREPARING SORTED LIST of ON/OFF PARTICLES by PART.
C---------------------------------------------
         SLPRTSPH = 2*(NPART+1)
         SLONFSPH = NUMSPH
         CALL MY_ALLOC(LPRTSPH,SLPRTSPH,"LPRTSPH")
         CALL MY_ALLOC(LONFSPH,SLONFSPH,"LONFSPH")
         LPRTSPH = 0
         LONFSPH = 0
         CALL SPHONF0(KXSP    ,IXSP    ,NOD2SP  ,IPART  ,IPARTSP ,
     .                LPRTSPH ,LONFSPH )
         SPHVELN = ZERO
         IF(NSPMD > 1)THEN
c           CALL ANSTCKC(27,'SPH INLET/OUTLET DEFINITION')
c           CALL ANCERR(755,ANINFO)
         END IF
      ELSE
         SLPRTSPH = 0
         SLONFSPH = 0
         SVSPHIO  = 0
c         SVSPHIO2  = 0
         CALL MY_ALLOC(LPRTSPH,SLPRTSPH,"LPRTSPH")
         CALL MY_ALLOC(LONFSPH,SLONFSPH,"LONFSPH")
         CALL MY_ALLOC(VSPHIO,SVSPHIO,"VSPHIO")
c         ALLOCATE(VSPHIO2(SVSPHIO2)      ,STAT=stat)
      ENDIF
      IF(NUMSPH > 0)THEN
C---------------------------------------------
C       FILLING SPBUF(2) = H
C---------------------------------------------
        CALL SPINIH(KXSP    ,IPART  ,IPARTSP ,SPBUF  ,PM,
     .              IXSP    ,NOD2SP ,X       ,LPRTSPH,LONFSPH,
     .              SNOD2SP ,SLONFSPH,NUMNOD,NPART,ITAB)
C---------------------------------------------
C        STARTER SORTING : Filling IXSP
C---------------------------------------------
        PRE_SEARCH = 0
        SZ_INTP_DIST = 1     ! Array MAX_INTP_DIST_PART not used for full search siz=1
        CALL SPTRI(KXSP    ,IXSP    ,NOD2SP  ,X      ,SPBUF  ,
     .             LPRTSPH ,LONFSPH ,IPARTSP ,SZ_INTP_DIST,MAX_INTP_DIST_PART,
     .             PRE_SEARCH)
      END IF
      CALL TRACE_OUT1()
C---------------------------------------------
C       Fluid nodal masses
C---------------------------------------------
      ERR_MSG='FLUID NODAL MASSES'
      ERR_CATEGORY='FLUID NODAL MASSES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SMSNF = NUMNOD*MAX(IALE,IEULER,IALELAG)
      CALL MY_ALLOC(MSNF,SMSNF,"MSNF")
      MSNF = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     PRE-READING CONCENTRATED FORCES & PRESSURES
C--------------------------------------------
      ERR_MSG='CONCENTRED LOADS'
      ERR_CATEGORY='CONCENTRED LOADS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NUMCLD  = NCONLD
      NUMPRES = NPRELD
      LOADS%NLOAD_CLOAD = 0
      LOADS%NLOAD_PLOAD = 0
      IF(NSUBDOM>0) ALLOCATE(NNCL(NCONLD+NPRELD))
      CALL HM_PREREAD_CLOAD(NUMCLD, IGRNOD ,IGRSURF,LSUBMODEL)
      CALL HM_PREREAD_PLOAD(NUMPRES,IGRNOD ,IGRSURF,LSUBMODEL)
      SIBCL = (NUMCLD + NUMPRES)*NIBCLD
      SFORC = (NUMCLD + NUMPRES)*LFACCLD
      CALL MY_ALLOC(IBCL,SIBCL,"IBCL")
      CALL MY_ALLOC(FORC,SFORC,"FORC")
      CALL MY_ALLOC(DPL0CLD,6*(NUMCLD+NUMPRES),"DPL0CLD")
      CALL MY_ALLOC(VEL0CLD,6*(NUMCLD+NUMPRES),"VEL0CLD")
      IF(NUMPRES>0) THEN
        IPRES  => IBCL(NUMCLD*NIBCLD+1:SIBCL)
      ELSE
        IPRES  => IBCL
      END IF
      IBCL = 0
      FORC = ZERO
      DPL0CLD = ZERO
      VEL0CLD = ZERO
C--------------------------------------------
C     READING CONCENTRATED FORCES
C--------------------------------------------
      IF(NCONLD/=0) THEN
        WRITE(ISTDO,'(A)')TITRE(33)
C       NCONLD AND NPRELD are modified in LECCLD and LECPRE
        CALL HM_READ_CLOAD(IBCL   ,FORC   ,NCONLD,ITAB   ,ITABM1 ,
     .                     IGRNOD ,IBCL   ,UNITAB,ISKWN ,LSUBMODEL,
     .                     LOADS  )
        IF(NCONLD*LFACCLD<SFORC) THEN
        PRES => FORC(NCONLD*LFACCLD+1:SFORC)
        ELSE
          PRES => FORC
      ENDIF
      ENDIF
      IF(NCONLD*LFACCLD<SFORC) THEN
      PRES => FORC(NCONLD*LFACCLD+1:SFORC)
      ELSE
        PRES => FORC
      END IF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING PRESSURES
C--------------------------------------------
      ERR_MSG='PRESSURE LOADS'
      ERR_CATEGORY='PRESSURE LOADS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NPRELD/=0)THEN
        WRITE(ISTDO,'(A)')TITRE(34)
        CALL HM_READ_PLOAD(IPRES   ,PRES   ,NPRELD    ,ITAB    ,ITABM1,
     .                     IGRSURF ,UNITAB ,LSUBMODEL ,LOADS   )
        NCONLD=NCONLD+NPRELD
      ENDIF
C NCONLD AND NPRELD are modified in LECCLD and LECPRE
      CALL TRACE_OUT1()
      IF(NSUBDOM>0) DEALLOCATE(NNCL)
c
c     pressure load in cylindrical coordinates
c
      CALL HM_READ_PCYL(LOADS   ,IGRSURF ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB ,TABLE   ,
     .                  IFRAME  ,UNITAB  ,LSUBMODEL,NUMBER_LOAD_CYL  )
C--------------------------------------------
C     "LOAD FIELDS"
C--------------------------------------------
      ERR_MSG      = 'LOAD FIELDS'
      ERR_CATEGORY = 'LOAD FIELDS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C     CENTRIFUGAL LOADS
      CALL HM_PREREAD_LOAD_CENTRI(NUMCFIELD,IGRNOD,IGRSURF,LSUBMODEL)
      SCFIELD      = LFACLOAD*NLOADC
      SICFIELD     = SIZFIELD*NLOADC
      SLCFIELD     = NUMCFIELD
C
C     PFLUID & PBLAST
      NUMLOADP=0
      NINTLOADP=0
      CALL HM_PREREAD_PFLUID(NUMLOADP,IGRNOD,IGRSURF,LSUBMODEL)
      CALL HM_PREREAD_PBLAST(PBLAST,NUMLOADP,IGRSURF,LSUBMODEL,NSURF)
      CALL HM_PREREAD_LOAD_PRESSURE(NUMLOADP,IGRSURF,LSUBMODEL)
      NLOADP       = NLOADP_F+PBLAST%NLOADP_B+NLOADP_HYD
      SLOADP       = LFACLOAD*NLOADP
      SILOADP      = SIZLOADP*NLOADP
      SLLOADP      = NUMLOADP
C
      CALL TRACE_OUT1()
c------------------------------------------------------
c     IMPOSED DISPLACEMENTS, VELOCITIES AND ACCELERATIONS
c------------------------------------------------------
      ERR_MSG='IMPOSED VELOCITIES'
      ERR_CATEGORY='IMPOSED VELOCITIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      NFVLAG = 0    ! Lagrangian multiplier flag
C---
c     Input  : NFXVEL = number of input cards : /IMDISP + /IMPVEL + /IMPACC
c     Output : NFXVEL = number of imposed nodes (disp + vel + acc)
c
c---  Calculate number of nodes with imposed disp, vel, acc for allocation
c
      IF(NFXVEL > 0) THEN
        NFV0 = NFXVEL
c
        CALL HM_PREREAD_IMPDISP(NIMPDISP  ,IGRNOD  ,IPART   ,IPARTR   ,
     .                          UNITAB    ,LSUBMODEL)
c
        CALL HM_PREREAD_IMPVEL(NIMPVEL  ,IGRNOD  ,IPART   ,IPARTR   , NFVLAG,
     .                         UNITAB   ,LSUBMODEL)
c
        CALL HM_PREREAD_IMPACC(NIMPACC ,IGRNOD ,LSUBMODEL)
c
        NFXVEL = NIMPDISP + NIMPVEL + NIMPACC
      ELSE
        NFV0 = 0
        NIMPDISP = 0
        NIMPVEL = 0
        NIMPACC = 0
        NIMPV_LAGM  = 0
      ENDIF
c---
      SIBFV   = NFXVEL * NIFV
      SVEL    = NFXVEL * LFXVELR
      NFXVEL0 = NFXVEL
      CALL MY_ALLOC(IBFV,SIBFV,"IBFV")
      CALL MY_ALLOC(VEL,SVEL,"VEL")
      IBFV(1:SIBFV) = 0
      VEL (1:SVEL)  = ZERO
c------------------------------------------------------
c
      IF(NFXVEL > 0) THEN
c
        WRITE(ISTDO,'(A)')TITRE(44)
c
        CALL HM_READ_IMPVEL(
     .       VEL      ,IBFV     ,D        ,IKINE1LAG,
     .       ITAB     ,ITABM1   ,IGRNOD   ,X        ,IXR      ,
     .       IPART    ,IPARTR   ,ISKWN    ,NOM_OPT(LNOPT1*INOM_OPT(15)+1),
     .       NIMPDISP ,NIMPVEL  ,UNITAB   ,LSUBMODEL)

      ENDIF
c------------------------------------------------------
c     IMPOSED ACCELERATIONS
c------------------------------------------------------
      IF(NIMPACC > 0) THEN
        CALL HM_READ_IMPACC(
     .             VEL      ,IBFV     ,NFXVEL0  ,ITAB     ,ITABM1   ,
     .             D        ,IGRNOD   ,ISKWN    ,UNITAB   ,LSUBMODEL,
     .             NFXVEL   ,NIMPACC  )
      ENDIF
c
C /BCS/CYCLIC ini&check
      IF(NBCSCYC > 0) THEN
        CALL MY_ALLOC(ITAGCYC,NUMNOD,"ITAGCYC")
        CALL INI_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,ITAB,ICODE,IBFV,ITAGCYC)
      ELSE
        CALL MY_ALLOC(ITAGCYC,0,"ITAGCYC")
      END IF
C--------------------------------------------
      SFSAV = NTHVKI * (NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU+NMONVOL+NFXBODY
     .                 +NINTSUB+NGUIDED_CABLE)
      CALL MY_ALLOC(FSAV,SFSAV,"FSAV")
      FSAV = ZERO
      CALL TRACE_OUT1()
C     *****************************************************************    C
C     Check if ALE or EULER materials are used with lagrangian thermics
C     *****************************************************************    C
C
C--------------------------------------------
C     READ INITIAL TEMPERATURE
C--------------------------------------------
      NINTEMP = GLOB_THERM%NINTEMP
      IF (IALE + IEULER /= 0) THEN
        IF (GLOB_THERM%ITHERM_FE == 0 .AND. (GLOB_THERM%NIMTEMP /= 0 .OR. NINTEMP /= 0)) THEN
          CALL ANCMSG(MSGID=1724, ANMODE=ANINFO, MSGTYPE=MSGWARNING)
        ENDIF
      ENDIF
      ERR_MSG='INITIAL TEMPERATURES'
      ERR_CATEGORY='INITIAL TEMPERATURES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
!
      IF (GLOB_THERM%NINTEMP > 0) THEN
        WRITE(ISTDO,'(A)')TITRE(35)
        CALL MY_ALLOC(TEMP,NUMNOD,"TEMP")
        TEMP(1:NUMNOD) = ZERO
        CALL MY_ALLOC(INTIDS,NINTEMP,"INTIDS")
        CALL HM_READ_INITEMP(TEMP    ,NINTEMP ,GLOB_THERM%ITHERM_FE,ITAB  ,ITABM1  ,
     .                       IGRNOD  ,INTIDS  ,UNITAB,LSUBMODEL )
        CALL MY_DEALLOC(INTIDS)
      END IF
!
      IF (GLOB_THERM%ITHERM_FE > 0 ) THEN
        CALL MY_ALLOC(MCP,NUMNOD,"MCP")
        IF (.NOT.ALLOCATED(TEMP)) THEN
          CALL MY_ALLOC(TEMP,NUMNOD,"TEMP")
          TEMP(1:NUMNOD) = ZERO
        END IF
        MCP (1:NUMNOD) = ZERO
C
        CALL HM_PREREAD_IMPTEMP(IGRSURF, IGRNOD, IGRBRIC, UNITAB, LSUBMODEL,
     .                          GLOB_THERM%NIMTEMP,GLOB_THERM%NFXTEMP)
c
        CALL HM_PREREAD_CONVEC(IGRSURF, IGRNOD, IGRBRIC, UNITAB, LSUBMODEL,
     .                         GLOB_THERM%NCONVEC ,GLOB_THERM%NUMCONV )
c
        CALL HM_PREREAD_RADIATION(IGRSURF, IGRNOD, IGRBRIC,UNITAB, LSUBMODEL,
     .                            GLOB_THERM%NRADIA ,GLOB_THERM%NUMRADIA)
c
        CALL HM_PREREAD_IMPFLUX(IGRSURF, IGRNOD, IGRBRIC,UNITAB, LSUBMODEL,
     .                          GLOB_THERM%NIMPFLUX,GLOB_THERM%NFXFLUX)
c

        CALL MY_ALLOC(IBCV,GLOB_THERM%NICONV*GLOB_THERM%NUMCONV,"IBCV")
        CALL MY_ALLOC(FCONV,GLOB_THERM%LFACTHER*GLOB_THERM%NUMCONV,"FCONV")
        CALL MY_ALLOC(IBFTEMP,GLOB_THERM%NIFT*GLOB_THERM%NFXTEMP,"IBFTEMP")
        CALL MY_ALLOC(FBFTEMP,GLOB_THERM%LFACTHER*GLOB_THERM%NFXTEMP,"FBFTEMP")
        CALL MY_ALLOC(IBFFLUX,GLOB_THERM%NITFLUX*GLOB_THERM%NFXFLUX,"IBFFLUX")
        CALL MY_ALLOC(FBFFLUX,GLOB_THERM%LFACTHER*GLOB_THERM%NFXFLUX,"FBFFLUX")
        CALL MY_ALLOC(IBCR,GLOB_THERM%NIRADIA*GLOB_THERM%NUMRADIA,"IBCR")
        call my_alloc(FRADIA, GLOB_THERM%LFACTHER*GLOB_THERM%NUMRADIA, "FRADIA", stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='THERMAL DATA')
         IBCR(1:GLOB_THERM%NIRADIA*GLOB_THERM%NUMRADIA) = 0
C
        IF (GLOB_THERM%NUMCONV > 0 ) THEN
          IBCV = 0
          FCONV = ZERO
          CALL HM_READ_CONVEC(IBCV,FCONV,ITAB,IXS,IGRSURF,UNITAB,LSUBMODEL,
     .         GLOB_THERM%NCONVEC ,GLOB_THERM%NICONV,GLOB_THERM%NUMCONV,GLOB_THERM%LFACTHER)
        ENDIF
C
        IF (GLOB_THERM%NUMRADIA > 0 ) THEN
          CALL HM_READ_RADIATION(IBCR,FRADIA,ITAB,IXS,IGRSURF,UNITAB,LSUBMODEL,
     .         GLOB_THERM%NRADIA ,GLOB_THERM%NUMRADIA,GLOB_THERM%NIRADIA,GLOB_THERM%LFACTHER)
        ENDIF
C
        IF (GLOB_THERM%NFXTEMP > 0) THEN
          IBFTEMP = 0
          FBFTEMP = ZERO
          CALL HM_READ_IMPTEMP(IBFTEMP,FBFTEMP,GLOB_THERM%NFXTEMP,ITABM1,
     .                   IGRNOD ,IBFTEMP,ITAB   ,UNITAB,LSUBMODEL,
     .                   GLOB_THERM%NIMTEMP,GLOB_THERM%NIFT,GLOB_THERM%LFACTHER)
        ENDIF
C
        IF (GLOB_THERM%NFXFLUX > 0 ) THEN
          IBFFLUX = 0
          FBFFLUX = ZERO
          CALL HM_READ_IMPFLUX(IBFFLUX ,FBFFLUX ,ITAB   ,IXS    ,IGRSURF   ,
     .                   UNITAB  ,IGRNOD  ,IGRBRIC, LSUBMODEL,
     .                   GLOB_THERM%NIMPFLUX,GLOB_THERM%NITFLUX,GLOB_THERM%LFACTHER)
        ENDIF
C
      ELSE
        CALL MY_ALLOC(MCP,0,"MCP")
        call my_alloc(IBCV, 0, "IBCV")
        call my_alloc(FCONV, 0, "FCONV")
        call my_alloc(IBFTEMP, 0, "IBFTEMP")
        call my_alloc(FBFTEMP, 0, "FBFTEMP")
        call my_alloc(IBFFLUX, 0, "IBFFLUX")
        call my_alloc(FBFFLUX, 0, "FBFFLUX")
        call my_alloc(IBCR, 0, "IBCR")
        call my_alloc(FRADIA, 0, "FRADIA")
        IF (.NOT. ALLOCATED(TEMP)) ALLOCATE(TEMP(0))
      ENDIF
C
      CALL TRACE_OUT1()
C--------------------------------------------
      IF(ISIGI==2 .OR. ISIGI==4) THEN
        SFZERO = 3*NUMNOD
      ELSEIF(IABS(ISIGI)==5) THEN
        SFZERO = 3*4*(NUMELC+NUMELTG)
      ELSE
        SFZERO = 0
      ENDIF
      CALL MY_ALLOC(FZERO,SFZERO,"FZERO")
      IF(SFZERO > 0) FZERO = ZERO
C--------------------------------------------
C     READING LASER IMPACTS
C--------------------------------------------
      ERR_MSG='LASER IMPACTS'
      ERR_CATEGORY='LASER IMPACTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL LECLAS(LSUBMODEL)
      CALL TRACE_OUT1()
C-------------------------------------------------
C     GENERATING EXTERNAL FACETS (ANIM FILES)
C     ALE-EULER SEULEMENT
C-------------------------------------------------
      NFACX=0
      ERR_MSG='ELEMENTARY BOUNDARY CONDITIONS'
      ERR_CATEGORY='ELEMENTARY BOUNDARY CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      EBCS_TAG_CELL_SPMD(1:NUMELQ+NUMELTG+NUMELS)=0
      CALL READ_EBCS(IGRSURF,MULTI_FVM,NPC1,LSUBMODEL,EBCS_TAB,N2D)
      IF(NEBCS > 0)THEN
        !allocate & count
        IF(.NOT. ALLOCATED(SENSOR_TMP)) ALLOCATE( SENSOR_TMP(0) )
        CALL INIEBCS(ALE_CONNECTIVITY, 0, IGRSURF, IXS, IXQ, IXTG,
     .               PM, IGEO, X, SENSOR_TMP, MONVOl, MULTI_FVM%IS_USED,
     .               EBCS_TAB, EBCS_TAG_CELL_SPMD, ITAB)
        DEALLOCATE(SENSOR_TMP)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING ACCELEROMETERS
C--------------------------------------------
      ERR_MSG='ACCELEROMETERS'
      ERR_CATEGORY='ACCELEROMETERS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SACCELM = NACCELM * LLACCELM
      CALL MY_ALLOC(ACCELM,SACCELM,"ACCELM")
      IF(SACCELM > 0) ACCELM = ZERO
      IF(NACCELM > 0) CALL LECACC(
     1          LACCELM,ACCELM ,ITABM1 ,UNITAB,IXC,
     2          ISKWN,NOM_OPT(LNOPT1*INOM_OPT(1)+1), LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING GAUGES
C--------------------------------------------
      ERR_MSG='GAUGES'
      ERR_CATEGORY='GAUGES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_ALLOC(LGAUGE,3*NBGAUGE,"LGAUGE")
      IF(NBGAUGE > 0) LGAUGE=0
      CALL MY_ALLOC(GAUGE,LLGAUGE*NBGAUGE,"GAUGE")
      IF(NBGAUGE > 0) GAUGE=ZERO
      IF(NBGAUGE > 0) CALL HM_READ_GAUGE(LGAUGE,GAUGE,ITABM1,UNITAB,IXC,NOM_OPT(LNOPT1*INOM_OPT(27)+1),LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING SENSORS
C--------------------------------------------
      ERR_MSG='SENSORS'
      ERR_CATEGORY='SENSORS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      ! -------------
      ! size initialization for user sensor
      CALL SENSOR_USER_INIT(SENSOR_USER_STRUCT)
      ! -------------
c
      PYTHON%NB_SENSORS = 0
      CALL HM_READ_SENSORS(PYTHON,
     .     SENSORS   ,LACCELM  ,ITABM1    ,IPART     ,LGAUGE    ,
     .     SUBSETS   ,NSETS    ,IGRNOD    ,
     .     ISKWN    ,UNITAB    ,LSUBMODEL ,HM_NSENSOR,
     .     SENSOR_USER_STRUCT)
      ! -----------------
      ! check if a user sensor is used with a list of node
      ! convert the list of User node ID into Local node ID

      IF(SENSOR_USER_STRUCT%IS_USED) THEN
          IF(SENSOR_USER_STRUCT%POINTER_NODE > 0) THEN
              CALL SENSOR_USER_CONVERT_LOCAL_ID(
     .         ITABM1,SENSOR_USER_STRUCT%POINTER_NODE,SENSOR_USER_STRUCT%NUMBER_NODE,
     .         SENSOR_USER_STRUCT%NODE_LIST,1,IPART)
          ENDIF
      ENDIF
        ! -----------------
      CALL TRACE_OUT1()

C--------------------------------------------
C     READING INITIAL VELOCITIES
C--------------------------------------------
      ERR_MSG='INITIAL VELOCITIES'
      ERR_CATEGORY='INITIAL VELOCITIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(NRBODY > 0) THEN
        CALL MY_ALLOC(RBY_INIAXIS,7,NRBODY,"RBY_INIAXIS")
        RBY_INIAXIS = ZERO
      ELSE
        CALL MY_ALLOC(RBY_INIAXIS,0,0,"RBY_INIAXIS")
      ENDIF
C
      NINIVELT = 0  ! /INIVEL w/ T_start
      IF(NINVEL > 0 ) CALL HM_PREREAD_INIVEL(LSUBMODEL,UNITAB,HM_NINVEL,NINIVELT)
      LOADS%NINIVELT = NINIVELT
      IF(NINVEL/=0.OR.ISIGI>=3)THEN
        SIWORK = NINVEL
        CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
        IWORK = 0
       WRITE(ISTDO,'(A)')TITRE(35)
       allocate(FVM_INIVEL(NINVEL))
       DO I = 1, NINVEL
          FVM_INIVEL(I)%FLAG = .FALSE.
       ENDDO
       allocate(LOADS%INIVELT(NINIVELT))
C
        CALL HM_READ_INIVEL(V         , W      , ITAB     , ITABM1 , VR         ,
     .                      IGRNOD    , IGRBRIC, ISKWN    , SKEW   , IWORK      ,
     .                      X         , UNITAB , LSUBMODEL, RTRANS , XFRAME     ,
     .                      IFRAME    , VFLOW  , WFLOW    , KXSP   , MULTI_FVM  ,
     .                      FVM_INIVEL, IGRQUAD, IGRSH3N  , RBY_MSN, RBY_INIAXIS,
     .                      SENSORS   ,NINIVELT,LOADS%INIVELT)
        CALL INIVEL(V, VR, SVR_1, ITABM1)
C
        IF(ALLOCATED(IWORK)) CALL MY_DEALLOC(IWORK)
        NINVEL  = SIWORK
C
      ELSE
        ALLOCATE(FVM_INIVEL(0:0))
      ENDIF
C
      IF(ALLOCATED(RBY_MSN)) CALL MY_DEALLOC(RBY_MSN)
C
      CALL TRACE_OUT1()

C--------------------------------------------
C     /BCS/WALL - SLIDING BOUNDARY CONDITION
C        (COLLOCATED SCHEME / LAW 151)
C--------------------------------------------
      bcs%nrf_cont_nb = 0
      IF(BCS%NUM_WALL /= 0) THEN
C READ /BCS/WALL
        CALL HM_READ_BCS_WALL(UNITAB, LSUBMODEL, IGRNOD, NGRNOD, SENSORS, ITABM1, NUMNOD, MULTI_FVM)
      ENDIF
C--------------------------------------------
C     /BCS/NRF - NON REFLECTING BOUNDARY CONDITION
C--------------------------------------------
      IF(BCS%NUM_NRF /= 0) THEN
C READ /BCS/WALL
        CALL HM_READ_BCS_NRF(LSUBMODEL, IGRNOD, NGRNOD, ITABM1, NUMNOD, MULTI_FVM)
        call my_real_tot_int_transfer(numels,eani,isolnod0)
        call init_bcs_nrf( igrnod,ngrnod,numnod,multi_fvm,
     .     ixs,nixs,numels,ixq,nixq,numelq,ixtg,nixtg,numeltg,n2d,
     .     ipri,itab,nummat,mat_elem%mat_param,isolnod0,
     .     npropgi,numgeo,npropm,igeo,pm)     
      ENDIF
C--------------------------------------------
C     READING PRELOADS
C--------------------------------------------
      ERR_MSG='BOLT PRELOADING'
      ERR_CATEGORY='BOLT PRELOADING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      NPRELOAD_A = NPRELOAD
      CALL HM_PRE_READ_PRELOAD_AXIAL(NGRSPRI,IGRSPRING,NPRELOAD_A,LSUBMODEL)
      NUMPRELOAD  = NPRELOAD
      IF(NPRELOAD > NPRELOAD_A) THEN
       IF(NSECT /= 0)THEN
        CALL PRELECSEC4BOLT(SNSTRF,SSECBUF,IGRNOD,ITABM1,0,
     .                      NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC,LSUBMODEL)
        CALL MY_ALLOC(NSTRF,SNSTRF,"NSTRF")
        CALL MY_ALLOC(SECBUF,SSECBUF,"SECBUF")
        NSTRF  = 0
        SECBUF = ZERO
        CALL LECSEC4BOLT(IXS    ,IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                   IXTG   ,X      ,NSTRF  ,ITAB   ,ITABM1 ,
     3                   IGRNOD ,SECBUF ,
     4                   IPARI  ,IXS10  ,IXS20  ,IXS16  ,UNITAB ,
     5                   ISKWN  ,XFRAME ,EANI,NOM_SECT,RTRANS,
     6                   LSUBMODEL,NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC)
       ENDIF
       CALL HM_PRE_READ_PRELOAD(NSTRF,LSUBMODEL) !to calculate NUMPRELOAD = NUMPRELOAD + NN (NN = NSTRF(K0+7) = NSEGS)
       SIPRELOAD = 3*NUMPRELOAD               !! sb - A ajuster
       SPRELOAD  = 6*NUMPRELOAD               !! sb - A ajuster
       CALL MY_ALLOC(IPRELOAD,SIPRELOAD,"IPRELOAD")
       CALL MY_ALLOC(PRELOAD,SPRELOAD,"PRELOAD")
       CALL MY_ALLOC(IFLAG_BPRELOAD,NUMELS,"IFLAG_BPRELOAD")
       CALL MY_ALLOC(IPRELOAD_FUN,2*NUMPRELOAD,"IPRELOAD_FUN")
       IPRELOAD       = 0
       IFLAG_BPRELOAD = 0
       PRELOAD        = ZERO
       IPRELOAD_FUN   = 0

       WRITE(ISTDO,'(A)')TITRE(34)
       CALL HM_READ_PRELOAD(IXS   ,IXS10     ,IPRELOAD ,PRELOAD,IFLAG_BPRELOAD,
     .                      NSTRF ,SENSORS   ,UNITAB ,X        ,
     .                      EANI  ,ITAB      ,LSUBMODEL,
     .                     SNPC   ,NPC       ,IPRELOAD_FUN)
c
       IF(ALLOCATED(NSTRF))  CALL MY_DEALLOC(NSTRF)
       IF(ALLOCATED(SECBUF)) CALL MY_DEALLOC(SECBUF)
       SNSTRF  = 0
       SSECBUF = 0
      ELSE
        CALL MY_ALLOC(IPRELOAD,0,"IPRELOAD")
        CALL MY_ALLOC(PRELOAD,0,"PRELOAD")
        CALL MY_ALLOC(IFLAG_BPRELOAD,0,"IFLAG_BPRELOAD")
        CALL MY_ALLOC(IPRELOAD_FUN,0,"IPRELOAD_FUN")
      ENDIF
!      /PRELOAD/AXIAL  2 int 2 real per preload, itag_spring...
       allocate(PRELOAD_A(NPRELOAD_A))
       CALL MY_ALLOC(ITAGPRLD_SPRING,NUMELR,"ITAGPRLD_SPRING")
       CALL MY_ALLOC(ITAGPRLD_BEAM,NUMELP,"ITAGPRLD_BEAM")
       CALL MY_ALLOC(ITAGPRLD_TRUSS,NUMELT,"ITAGPRLD_TRUSS")
! itagprld_spring(nsprint), itagpre_beam(nbeam) : id of preload/axial; used for element grouping
        ITAGPRLD_SPRING = 0
        ITAGPRLD_BEAM   = 0
        ITAGPRLD_TRUSS  = 0
       IF (NPRELOAD_A > 0) THEN
         CALL HM_READ_PRELOAD_AXIAL(                                     
     .        NPRELOAD_A,    NGRSPRI,    IGRSPRING, ITAGPRLD_SPRING,  
     .        UNITAB    ,  LSUBMODEL,    PRELOAD_A, NUMELR         ,
     .        SNPC      ,  NPC      ,    NFUNCT   , SENSORS        ,
     .        NGRBEAM   , IGRBEAM   ,    NUMELP   , ITAGPRLD_BEAM  ,
     .        NGRTRUS   , IGRTRUSS  ,    NUMELT   , ITAGPRLD_TRUSS ,
     .        IOUT     )
         NPRELOAD = NPRELOAD - NPRELOAD_A
       END IF 
      CALL TRACE_OUT1()
C-------------------------------------------------
C     READING RIGID LINKS
C--------------------------------------------
      ERR_MSG='RIGID LINKS'
      ERR_CATEGORY='RIGID LINKS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PRE_READ_LINK(NUMLINK, IGRNOD,LSUBMODEL)
      SNNLINK = 10*NLINK
      SLNLINK = NUMLINK
      CALL MY_ALLOC(NNLINK,MAX(1,SNNLINK),"NNLINK")
      CALL MY_ALLOC(LNLINK,MAX(1,SLNLINK),"LNLINK")
      IF(SNNLINK > 0) NNLINK = 0
      IF(SLNLINK > 0) LNLINK = 0
      IF(NLINK > 0) THEN
      CALL HM_READ_LINK(
     1        NNLINK  ,LNLINK ,ITAB    ,ITABM1  ,D       ,
     2        IGRNOD  ,ISKWN  ,IFRAME  ,NOM_OPT(LNOPT1*INOM_OPT(9)+1),LSUBMODEL)
      ENDIF
C
      SFBVEL   = 3*NIBVEL
      SGRAV    = LFACGRV*NGRAV
      SFR_WAVE = NUMNOD*IFRWV
      SPARTS0  = NPART
      CALL MY_ALLOC(FBVEL,SFBVEL,"FBVEL")
      CALL MY_ALLOC(GRAV,SGRAV,"GRAV")
      CALL MY_ALLOC(FR_WAVE,SFR_WAVE,"FR_WAVE")
      CALL MY_ALLOC(PARTS0,SPARTS0,"PARTS0")
      IF(SFBVEL > 0) FBVEL   = ZERO
      IF(SGRAV  > 0) GRAV    = ZERO
      IF(SFR_WAVE > 0) FR_WAVE = ZERO
      IF(SPARTS0  > 0) PARTS0  = ZERO
c--------------------------------------------
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING OLD WEIGHTS
C--------------------------------------------
      CALL PRELEC_DDW(FILNAM,LEN_FILNAM,MARQUEUR3)
      IF(MARQUEUR3) THEN
        WRITE(IOUT,'(A)')' '
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
        WRITE(IOUT,'(A)')
     .  ' DDW OPTION FOR THE DOMAIN DECOMPOSITION'
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
       WRITE(ISTDO,*)
     . '.. DDW OPTION FOR THE DOMAIN DECOMPOSITION'

       CALL MY_ALLOC(TAB_UMP_OLD,7,TAILLE_OLD,"TAB_UMP_OLD")
       CALL MY_ALLOC(CPUTIME_MP_OLD,TAILLE_OLD,"CPUTIME_MP_OLD")
       TAB_UMP_OLD = 0 
       CPUTIME_MP_OLD = ZERO

       CALL LEC_DDW(FILNAM,LEN_FILNAM,TAB_UMP_OLD,CPUTIME_MP_OLD)

       CALL PRELEC_DDW_POIN(FILNAM,LEN_FILNAM)
       CALL MY_ALLOC(POIN_UMP_OLD,NUMMAT_OLD,"POIN_UMP_OLD")
       POIN_UMP_OLD = 0

        CALL LEC_DDW_POIN(FILNAM,LEN_FILNAM,POIN_UMP_OLD)
      ELSE
       CALL MY_ALLOC(TAB_UMP_OLD,0,0,"TAB_UMP_OLD")
       CALL MY_ALLOC(CPUTIME_MP_OLD,0,"CPUTIME_MP_OLD")
       CALL MY_ALLOC(POIN_UMP_OLD,0,"POIN_UMP_OLD")
      ENDIF
C--------------------------------------------
C     USER S WINDOW
C--------------------------------------------
      ERR_MSG='USER WINDOWS'
      ERR_CATEGORY='USER WINDOWS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN
!
!==============
        CALL HM_READ_WINDOW_USER(USER_WINDOWS,LSUBMODEL,ITAB,
     *                           X, V, VR, MS, IN)
!==============
!
      ENDIF
      CALL TRACE_OUT1()
C---------------------------
C Calculate ELEM RBY ON/OFF for domdec
C---------------------------
      ERR_MSG='RIGID BODIES ON'
      ERR_CATEGORY='RIGID BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELS /=0) ISOLOFF = 0
      IF(NUMELC /=0) ISHEOFF = 0
      IF(NUMELT /=0) ITRUOFF = 0
      IF(NUMELP /=0) IPOUOFF = 0
      IF(NUMELR /=0) IRESOFF = 0
      IF(NUMELTG /=0) ITRIOFF = 0
      IF(NUMELQ /=0) IQUAOFF = 0
      CALL SETRBYON(
     1   IXS      ,IXC     ,IXTG   ,IGRNOD  ,IGRNRBY ,
     2   ISOLOFF  ,ISHEOFF ,ITRIOFF,KNOD2ELS,KNOD2ELC,
     5   KNOD2ELTG,NOD2ELS ,NOD2ELC,NOD2ELTG,IXQ     ,
     6   IQUAOFF  ,KNOD2ELQ,NOD2ELQ,LSUBMODEL)
      CALL TRACE_OUT1()
C---------------------------
C Calculate ELEM OFF (RBE2) for domdec
C---------------------------
      ERR_MSG='RBE2 ON'
      ERR_CATEGORY='RBE2'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SETRB2ON(
     1   IXS    ,IXC    ,IXTG   ,IGRNOD ,
     2   IGRNRB2,ISOLOFF,ISHEOFF,ITRIOFF,ITABM1,
     3   LSUBMODEL)
      CALL TRACE_OUT1()
C---------------------------------------------
C Calculate FLEXIBLE BODY - ELEM OFF for domdec
C---------------------------------------------
      ERR_MSG='FLEXIBLE BODIES ON'
      ERR_CATEGORY='FLEXIBLE BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFXBODY > 0)THEN
        LENMOD=0
        CALL HM_SETFXRBYON(ITABM1,IXS,ISOLOFF,IXC,ISHEOFF,
     1                     IXT,ITRUOFF,IXP,IPOUOFF,IXR,IRESOFF,
     2                     IXTG,ITRIOFF,FXBIPM,LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     READING ELEMENT CLUSTERS
C--------------------------------------------
      ERR_MSG='CLUSTERS'
      ERR_CATEGORY='CLUSTERS'
      allocate(CLUSTERS(NCLUSTER))
      CALL HM_READ_CLUSTER(
     .       CLUSTERS ,UNITAB   ,ISKWN    ,IGRBRIC  ,IGRSPRING,
     .       IXS      ,IXR      ,NOM_OPT(LNOPT1*INOM_OPT(28)+1),
     .       LSUBMODEL)
C--------------------------------------------
C     READING INITIAL VOLUME FRACTIONS
C--------------------------------------------
      CALL HM_READ_INIVOL(INIVOL, KVOL, IGRSURF ,IPART, MULTI_FVM, BUFMAT, 
     *                    IPM, NBSUBMAT, LSUBMODEL, UNITAB,
     *                    N2D ,NUMELTG,NUMELS,NUMELQ,NUMMAT,
     *                    NPART,NSURF,LIPART1,NPROPMI,SIPART,SINIVOL,
     *                    NSUBDOM,SBUFMAT,IGRNOD,NGRNOD)
C--------------------------------------------
C     Surfaces fictives IGE
C--------------------------------------------
      IF(TAGSURFIGE>0) THEN
        SNIGE = IADTABIGE
        NIGE => NIGE_TMP(IDXIGE2)%ptr
C--------------------------------------------
        SRIGE = IADTABIGE
        RIGE => RIGE_TMP(IDXIGE2)%ptr2
C--------------------------------------------
        SXIGE = IADTABIGE
        XIGE => XIGE_TMP(IDXIGE2)%ptr2
C--------------------------------------------
        SVIGE = IADTABIGE
        VIGE => VIGE_TMP(IDXIGE2)%ptr2
      ENDIF
      CALL TRACE_OUT1()


C -------------------------------------------------
C Check surfaces for airbags
C -------------------------------------------------
      CALL CHECK_SURF(IGRSURF)
C----------------------------------
C     READER FOR MONITORED VOLUMES
C----------------------------------
      ERR_MSG='MONITORED VOLUMES'
      ERR_CATEGORY='MONITORED VOLUMES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IBAGSURF = 0
      LIBAGALE = 0
      LRBAGALE = 0
      NVENTTOT = 0
      allocate(T_MONVOL(NVOLU + NMONVOL))
      CALL MONVOL_ALLOCATE(NVOLU + NMONVOL, T_MONVOL, T_MONVOL_METADATA)
      IF(NVOLU + NMONVOL> 0) THEN
        WRITE(ISTDO,'(A)') ' .. MONITORED VOLUMES '

        CALL READ_MONVOL(T_MONVOL, T_MONVOL_METADATA, ITAB, ITABM1, IPM, IGEO,
     .       X, PM, GEO, IXC, IXTG, SENSORS,
     .       UNITAB, NPC1, NPC, TF, IGRSURF, IGRBRIC, NOM_OPT(LNOPT1*INOM_OPT(2)+1),IFRAME, XFRAME,
     .       LSUBMODEL)

        CALL INIT_MONVOL(T_MONVOL, T_MONVOL_METADATA,
     3                   IXC     , IXTG             ,X       ,NPC1    ,
     4                   ITAB    , IGRSURF          ,
     5                   SENSORS , IGRBRIC          ,SBUFALE ,
     6                   IXS     , V                ,LIBAGALE,
     7                   LRBAGALE      )

        LIBAGJET=0
        LRBAGJET=0
        LIBAGHOL=0
        LRBAGHOL=0
        LRCBAG  = 0
        LICBAG = NICBAG * NVOLU * NVOLU
        DO II = 1, NVOLU
           LIBAGJET = LIBAGJET + NIBJET * T_MONVOL(II)%NJET
           LRBAGJET = LRBAGJET + NRBJET * T_MONVOL(II)%NJET
           LIBAGHOL = LIBAGHOL + NIBHOL * T_MONVOL(II)%NVENT
           LRBAGHOL = LRBAGHOL + NRBHOL * T_MONVOL(II)%NVENT
           IF(T_MONVOL(II)%TYPE == 5 .OR. T_MONVOL(II)%TYPE == 9) THEN
              LRCBAG = LRCBAG + T_MONVOL(II)%NCA * NRCBAG
           ENDIF
        ENDDO

        SVOLMON0 = NRVOLU * NVOLU + LRCBAG + LRBAGJET + LRBAGHOL
        SVOLMON = SVOLMON0 + SBUFALE + 1
        CALL MY_ALLOC(VOLMON,SVOLMON,"VOLMON")
        VOLMON(1:SVOLMON) = ZERO

        SMONVOL = NIMV * NVOLU + LICBAG + LIBAGJET + LIBAGHOL + LIBAGALE
        CALL MY_ALLOC(MONVOL,SMONVOL,"MONVOL")
        MONVOL(1:SMONVOL) = 0
      ELSE
        CALL MY_ALLOC(MONVOL,0,"MONVOL")
        CALL MY_ALLOC(VOLMON,0,"VOLMON")
      ENDIF
      CALL TRACE_OUT1()
C---------------------------
C Calculate DOF for implicit domdec
C---------------------------
      ERR_MSG='IMPLICIT DOMAIN DECOMPOSITION'
      ERR_CATEGORY='IMPLICIT DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL DSDIM0(
     1    DSDOF,IXS , IXQ,  IXC , IXT,
     2    IXP  ,IXR , IXTG, KXX,
     3    IXX  ,GEO )
C---------------------------
C DOMAIN DECOMPOSITION ON COMPLETE MODEL
C CEP array giving the associated processor for each element.
C Allocation of size NELEM
C---------------------------
C IDDLEVEL indicates the domdec level :
C 0 == level 1) interfaces not taken into account (input v31)
C 1 == level 2) interfaces taken into account in dd (input v41)
C---------------------------
      IDDLEVEL = 0
      NELEMINT = 0
      IFIXIN = 1
      IFIEND = 1
      DO I = 1, NUMNOD
        IWCONT(I) = 0
        IWCONT(NUMNOD+I) = 0
        IWCIN2(I)= 0
        IWCIN2(NUMNOD+I)= 0
c save IENTRY
c replace save of old FRONT
c FRONT(I,NSPMD+1) = FRONT(I,1)
        IENTRY2(I) = IFRONT%IENTRY(I)
      ENDDO
        DO I=0,MAXLAW
           SOL1TNL(I,1)=ZERO
           SOL1TNL(I,2)=ZERO
           SOL1TNL(I,3)=ZERO

           SOL8TNL(I,1)=ZERO
           SOL8TNL(I,2)=ZERO
           SOL8TNL(I,3)=ZERO

           DO J=0,3
            SHTNL(I,J,1)=ZERO
            SHTNL(I,J,2)=ZERO
            SHTNL(I,J,3)=ZERO

            TRITNL(I,J,1)=ZERO
            TRITNL(I,J,2)=ZERO
            TRITNL(I,J,3)=ZERO
           ENDDO
        ENDDO
        DO I=1,10
          SOLTELT(I)=ZERO
          SHTELT(I)=ZERO
          TRITELT(I)=ZERO
        ENDDO
        TPSREF = ZERO
C       Save lagmult array lengths
        LAG_NCF0 = LAG_NCF
        LAG_NKF0 = LAG_NKF
        LAG_NHF0 = LAG_NHF
        LAG_NCL0 = LAG_NCL
        LAG_NKL0 = LAG_NKL
      CALL TRACE_OUT1()
C-----------------
C  Update stack Due to DRAPE
C-------------------------
      IF(NDRAPE > 0 .AND. (IPART_STACK > 0 .OR. IPART_PCOMPP > 0)) THEN
         CALL SHELLTHK_UPD(DRAPE     ,STACK    ,THKE     ,IXC       ,IXTG      ,
     .                     IGEO      ,IWORKSH     ,DRAPEG%INDX)
      ENDIF
C-----------------------------------------------------
C     READING /PERTURB( random noise on shell/part thicknesses )
C-----------------------------------------------------
      ERR_MSG='PERTURB'
      ERR_CATEGORY='PERTURB'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      IF(IPERTURB /= 0) THEN
        SRNOISE1=NPERTURB
        SRNOISE2=NUMELC+NUMELTG+NUMELS+NUMSPH
        CALL MY_ALLOC(RNOISE,NPERTURB,NUMELC+NUMELTG+NUMELS+NUMSPH,"RNOISE")
        RNOISE(1:NPERTURB,1:NUMELC+NUMELTG+NUMELS+NUMSPH) = ZERO
        CALL MY_ALLOC(PERTURB,NPERTURB,"PERTURB")
        PERTURB(1:NPERTURB) = 0
        CALL MY_ALLOC(QP_IPERTURB,NPERTURB,6,"QP_IPERTURB")
        QP_IPERTURB(1:NPERTURB,1:6) = 0
        CALL MY_ALLOC(QP_RPERTURB,NPERTURB,4,"QP_RPERTURB")
        QP_RPERTURB(1:NPERTURB,1:4) = ZERO
        CALL HM_READ_PERTURB(MAT_ELEM%MAT_PARAM,
     .                       IPART   ,RNOISE   ,IPARTC   ,IPARTG  ,IPARTSP  ,
     .                       IGRPART ,IPM      ,IPARTS   ,PERTURB ,QP_IPERTURB,
     .                       QP_RPERTURB       ,LSUBMODEL,UNITAB  )
      ELSE
        SRNOISE1=1
        SRNOISE2=1
        CALL MY_ALLOC(RNOISE,1,1,"RNOISE")
        RNOISE(1,1) = ZERO
        CALL MY_ALLOC(PERTURB,1,"PERTURB")
        PERTURB(1) = 0
        CALL MY_ALLOC(QP_IPERTURB,0,0,"QP_IPERTURB")
        CALL MY_ALLOC(QP_RPERTURB,0,0,"QP_RPERTURB")
      ENDIF

      CALL TRACE_OUT1()
C-----------------
C  Global Mat for PID 11 and PID51 (for shell)
C-------------------------

      CALL GLOBMAT(IGEO  , GEO   ,PM ,STACK%PM, STACK%GEO,STACK%IGEO,
     .             MAT_ELEM%MAT_PARAM)

C-------------------------
C Fill index to renumber Solid elements after Domain Decomposition
C Array has 2 parts :
C PERMUTATION%SOLID(1:NUMELS) : INDEX(NEW ID)=OLD_ID
C PERMUTATION%SOLID(NUMELS+1:2*NUMELS) : INDEX(OLD)=NEW_ID
      CALL MY_ALLOC(PERMUTATION%SOLID,MAX(2*NUMELS,1),"PERMUTATION%SOLID")
      CALL MY_ALLOC(PERMUTATION%SHELL,MAX(2*NUMELC,1),"PERMUTATION%SHELL")
      CALL MY_ALLOC(PERMUTATION%TRIANGLE,MAX(2*NUMELTG,1),"PERMUTATION%TRIANGLE")
      CALL MY_ALLOC(PERMUTATION%QUAD,MAX(2*NUMELQ,1),"PERMUTATION%QUAD")
      PERMUTATION%TRIANGLE = 0
      PERMUTATION%SHELL = 0
      PERMUTATION%SOLID = 0
      permutation%quad = 0

C------------------------------------------------------------------------
C     REPLACING EXTERNAL IDS OF FCTS AND SKEW BY SYSTEM IDS
C------------------------------------------------------------------------
      ERR_MSG='USER TO SYSTEM RENUMBERING'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL FSDCOD(PYTHON, BUFMAT   ,PM       ,GEO      ,IBCL      ,IPRES              ,
     .            IBFV     ,ISKEW    ,ISKWN    ,SENSORS   ,MAT_ELEM%MAT_PARAM ,
     .            ITABM1   ,SKEW     ,LACCELM  ,BID13     ,BUFGEO             ,
     .            IBCSLAG  ,IGEO     ,IPM      ,
     .            IBFTEMP  ,IBCV     ,IBFV      ,
     .            IBCR     ,TABLE    ,NPC1     ,NPC       ,TF                 ,
     .            NOM_OPT(LNOPT1*INOM_OPT(3)+1),IBFFLUX   ,GLOB_THERM,NIMPVEL,NIMPDISP,
     .            NIMPACC)
C
c------------------------------------------------------------------------
c     Update & check parameters of material laws
c------------------------------------------------------------------------
      CALL UPDMAT(BUFMAT   ,PM      ,IPM       ,TABLE    ,NPC1     ,
     .            NPC      ,TF      ,SENSORS   ,NLOC_DMG ,MLAW_TAG ,
     .            MAT_ELEM%MAT_PARAM)
c
      CALL UPDFAIL(MAT_ELEM%MAT_PARAM ,NUMMAT ,NFUNCT ,NTABLE ,NPC1 ,TABLE ,
     .             FAIL_FRACTAL,NGRSHEL   ,NGRSH3N,IGRSH4N ,IGRSH3N ,
     .             NIXC    ,IXC   ,NIXTG ,IXTG    ,NUMELC  ,NUMELTG ,
     .             IWORKSH ,STACK ,IGEO  ,NPROPGI ,NUMGEO  ,FAIL_BROKMANN)
C
      CALL TRACE_OUT1()
C------------------------------------------------------------------------
C     OPTIONS SPH:
C     REPLACING EXTERNAL IDS OF FCTS
C------------------------------------------------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSPHIO/=0)
     .  CALL SPHDCOD(NPC1,ISPHIO,NOM_OPT(LNOPT1*INOM_OPT(22)+1))
      CALL TRACE_OUT1()
C
C------------------------------------------------------------------------
C
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
C 100 return address for domain decomposition after reading of contact interfaces or AMS element selection
C
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

 100  CONTINUE

      CALL INIT_PERMUTATION()
C
      IF(IDDLEVEL==1) THEN
        TOTADDMAS = ZERO
        MS = ZERO
        IN = ZERO
        MCP = ZERO
        MSNF = ZERO
C
        IF((NSUBDOM>0)) THEN
C       For multidomains - Mass and inertia must be nonzero for coupled nodes
          DO I=1,NUMNOD
            IF(TAGNO(I+N_PART) > 1) THEN
              MS(I)=1e-20
              IF(IRODDL==1) IN(I)=1e-20
            ENDIF
          END DO
        ENDIF
C
      ENDIF
C
c-----------------------------------------------------------------------
c     Create seatbelt entities + domdec
c-----------------------------------------------------------------------
C
      N_SEATBELT = 0
      IF(NB_MAT_SEATBELT > 0) THEN
        ERR_MSG='SEATBELTS'
        ERR_CATEGORY='SEATBELTS'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        WRITE(ISTDO,'(A)')' .. SEATBELT INITIALIZATION'
        CALL CREATE_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,IPM,
     .                       X,SENSORS,BUFMAT,PM,GEO,
     .                       IDDLEVEL,KNOD2ELC,NOD2ELC,IXC,IGEO,
     .                       ISKWN   ,TF      ,NPC)
        CALL TRACE_OUT1()
      ENDIF
C      
C------------------------------------------------------------------
C     RAYLEIGH DAMPING
C--------------------------------------------
      IF (NDAMP > 0) THEN
        IF(IDDLEVEL==0)THEN
          ERR_MSG='DAMPING'
          ERR_CATEGORY='DAMPING'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))             
          IF(NDAMP > 0) CALL HM_READ_DAMP(DAMPR,IGRNOD,ISKWN,LSUBMODEL,UNITAB,
     .                                    SNPC1,NPC1,NDAMP_VREL_RBY,IGRPART,DAMP_RANGE_PART)
          CALL TRACE_OUT1()
        ENDIF     
      ENDIF      
C
c-----------------------------------------------------------------------
C
      IF(ISMS == 0) THEN
        IF(.NOT. ALLOCATED(TAGPRT_SMS)) THEN
          CALL MY_ALLOC(TAGPRT_SMS,0,"TAGPRT_SMS")
          CALL MY_ALLOC(NATIV_SMS,0,"NATIV_SMS")
          CALL MY_ALLOC(T2MAIN_SMS,4,0,"T2MAIN_SMS")
        ENDIF
      ELSEIF(ISMS/=0)THEN
        ERR_MSG='AMS'
        ERR_CATEGORY='AMS'
        WRITE(ISTDO,'(A)')' .. AMS INITIALIZATION'
        IF(IDDLEVEL==0) THEN
C
          ERR_MSG='AMS INITIALIZATION PHASE I'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
          call my_alloc(TAGPRT_SMS, NPART, "TAGPRT_SMS", stat=stat)
          call my_alloc(NATIV_SMS, NUMNOD, "NATIV_SMS", stat=stat)
          call my_alloc(T2MAIN_SMS, 4, NUMNOD, "T2MAIN_SMS", stat=stat)
C
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TAGPRT_SMS / NATIV_SMS / T2MAIN_SMS')
          TAGPRT_SMS=0
          NATIV_SMS(1:NUMNOD)=0
          T2MAIN_SMS=0
C
          CALL INISMS(IGRPART  ,IPARTS     ,IPARTQ   ,IPARTC  ,
     .            IPARTT   ,IPARTP    ,IPARTR      ,IPARTG  ,
     .                IPARTX   ,TAGPRT_SMS )
C
          CALL TRACE_OUT1()
C
        ELSE
C
C         AMS Prepare DOMETIS
C
          ERR_MSG='AMS INITIALIZATION PHASE II'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
          CALL MY_ALLOC(KINWORK,NUMNOD,"KINWORK")
          CALL KINREM(D       ,KINWORK ,RWBUF   ,ITAB   ,NPRW   ,
     .                LPRW    ,NPBY    ,LPBY    )
C
          call my_alloc(TAGREL_SMS, NGROUP, "TAGREL_SMS", stat=STAT)
          call my_alloc(TAGSLV_RBY_SMS, NUMNOD, "TAGSLV_RBY_SMS", stat=STAT)
          call my_alloc(TAGMSR_RBY_SMS, NUMNOD, "TAGMSR_RBY_SMS", stat=STAT)
          call my_alloc(KAD_SMS, NUMNOD+1, "KAD_SMS", stat=STAT)
          call my_alloc(JAD_SMS, NUMNOD+1, "JAD_SMS", stat=STAT)
          call my_alloc(IAD_SMS, NUMNOD+1, "IAD_SMS", stat=STAT)
          call my_alloc(LAD_SMS, NUMNOD+1, "LAD_SMS", stat=STAT)
          call my_alloc(JADC_SMS, 4*NUMELC, "JADC_SMS", stat=STAT)
          call my_alloc(JADS_SMS, 8*NUMELS, "JADS_SMS", stat=STAT)
          call my_alloc(JADS10_SMS, 6*NUMELS10, "JADS10_SMS", stat=STAT)
          call my_alloc(JADT_SMS, 2*NUMELT, "JADT_SMS", stat=STAT)
          call my_alloc(JADP_SMS, 2*NUMELP, "JADP_SMS", stat=STAT)
          call my_alloc(JADR_SMS, 3*NUMELR, "JADR_SMS", stat=STAT)
          call my_alloc(JADTG_SMS, 3*NUMELTG, "JADTG_SMS", stat=STAT)
          call my_alloc(JADRB_SMS, NRBODY, "JADRB_SMS", stat=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TAGREL_SMS')
C
C
          CALL SMS_INIT(
     1     IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2     IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3     IXS20   ,IPARG    ,DSDOF   ,
     4     ICODT   ,ICODR    ,KINWORK ,
     5                        IPARTS     ,IPARTQ   ,IPARTC  ,
     6     IPARTT   ,IPARTP   ,IPARTR      ,IPARTG  ,
     7     IPARTX   ,TAGPRT_SMS ,ITAB   ,IRBE2   ,
     8     IRBE3    ,LRBE2      ,LRBE3      ,NPRW   ,LPRW    ,
     9     IPART    ,IGEO       ,IPM        ,NATIV_SMS,NPBY  ,
     A     LPBY       ,TAGMSR_RBY_SMS,TAGSLV_RBY_SMS,NOM_OPT)
C
          CALL SMS_INI_KAD(
     1     IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2     IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3     IXS20   ,IPARG    ,MS      ,MS0     ,DSDOF     ,
     4     ICODT   ,ICODR    ,KINET   ,
     5     KAD_SMS                    ,IPARTS   ,IPARTQ   ,
     6     IPARTC  ,IPARTT  ,IPARTP   ,IPARTR    ,
     7     IPARTG  ,IPARTX  ,TAGPRT_SMS,TAGREL_SMS,ITAB    ,
     8     IRBE2   ,IRBE3     ,LRBE2    ,LRBE3    ,
     9     NPRW    ,LPRW      ,IPART    ,IGEO     ,NATIV_SMS)

C
          call my_alloc(KDI_SMS, KNZ_SMS, "KDI_SMS", stat=STAT)
          call my_alloc(PK_SMS, KNZ_SMS, "PK_SMS", stat=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='KDI_SMS')
C
          CALL SMS_INI_KDI(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,DSDOF    ,KAD_SMS ,
     4             KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS ,
     5             JADT_SMS ,JADP_SMS,
     6             JADR_SMS,JADTG_SMS,TAGPRT_SMS,IAD_SMS ,
     7             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8             IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     9             NPBY     ,LPBY     ,KINET   ,TAGSLV_RBY_SMS,IPARI  ,
     A             INTBUF_TAB,LAD_SMS ,IPART   ,IGEO     ,NATIV_SMS)
C
          call my_alloc(IDI_SMS, NNZ_SMS, "IDI_SMS", stat=STAT)
          call my_alloc(JDI_SMS, NNZ_SMS, "JDI_SMS", stat=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='JDI_SMS')
C
          CALL SMS_INI_JAD_1(
     2           IXC      ,IPARG   ,IXS      ,IXT       ,IXP      ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF     ,JADC_SMS ,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,PK_SMS    ,
     6           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT    ,
     7           IPARTP    ,IPARTR ,IPARTG   ,IPARTX    ,
     8           NPBY     ,LPBY     ,KINET   ,TAGSLV_RBY_SMS,IPARI ,
     9           INTBUF_TAB,LAD_SMS ,IPART   ,IGEO      ,NATIV_SMS ,
     A           IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)

          CALL MY_DEALLOC(JDI_SMS)

          CALL MY_ALLOC(JDI_SMS,NNZ_SMS,"JDI_SMS")

          CALL SMS_INI_JAD_2(
     2           IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF    ,JADC_SMS ,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     7           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8           IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     9           NPBY     ,LPBY  ,KINET      ,TAGSLV_RBY_SMS,IPARI   ,
     A           INTBUF_TAB,LAD_SMS   ,NPRW  ,LPRW    ,TAGMSR_RBY_SMS,
     C           INTSTAMP  ,IPART     ,IGEO  ,NATIV_SMS,IRBE2        ,
     D           LRBE2     ,IAD_SMS   ,IDI_SMS,JAD_SMS ,JDI_SMS  ,
     E           T2MAIN_SMS)

          CALL MY_DEALLOC(JDI_SMS)

          CALL MY_ALLOC(JDI_SMS,NNZ_SMS,"JDI_SMS")
          CALL MY_ALLOC(JSM_SMS,NNZ_SMS,"JSM_SMS")
C
          CALL SMS_INI_JAD_3(
     2           IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF    ,JADC_SMS,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS  ,KDI_SMS ,
     6           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7           IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     8           NPBY     ,LPBY     ,KINET    ,
     9           TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,
     A           LAD_SMS ,JSM_SMS ,INTSTAMP  ,IPART    ,
     B           IGEO    ,TAGMSR_RBY_SMS,NATIV_SMS,
     C           IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
C
          CALL MY_DEALLOC(KINWORK)
          CALL MY_DEALLOC(T2MAIN_SMS)
          CALL TRACE_OUT1()
C
C         If no element selected AMS is deactivated
          IF((ISMS_SELEC >= 2).AND.(NNZ_SMS == 0)) ISMS_SELEC = 0
C
        END IF
C
      END IF
C-----
      NELEM = NUMELC+NUMELTG+NUMELS+NUMELR
     +      + NUMELP+NUMELT+NUMELQ+NUMELX+NUMELIG3D
C

      CALL MY_ALLOC(IELEM21,NELEM,"IELEM21")
      IELEM21=0
C
      ERR_MSG='DOMAIN DECOMPOSITION'
      ERR_CATEGORY='DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      iwarnhb = 0 
      call my_real_tot_int_transfer(numels,eani,isolnod0)
      call my_real_tot_int_transfer(numeltg,eanit,ishell3nod0)

      call get_element_group(numels,numelc,numeltg,nb_key, 
     .                       nummat,numgeo,npart,npropgi, 
     .                       npropmi,npropm,npropg,lipart1, 
     .                       isms,idtgrs,npreload,icrack3d, 
     .                       sh4tree_dim1,sh4tree_dim2,sh3tree_dim1,sh3tree_dim2, 
     .                       ltitr,iwarnhb,trimat,nadmesh,iddlevel, 
     .                       iparts,ipartc,ipartg,ipart,
     .                       isoloff,isheoff,itrioff,isolnod0,ishell3nod0,
     .                       iflag_bpreload,damp_range_part,tagprt_sms, 
     .                       iworksh,ixs,ixc,ixtg, 
     .                       igeo,ipm,sh4tree,sh3tree, 
     .                       pm,geo,stack,mat_elem%mat_param,elm_group)
      
      IF(IDDLEVEL==1) THEN
         NGROUP = 0
         LBUFEL = 0
         SEGINDX=0
c treatment for new IFRONT
c reinit
         CALL INI_IFRONT()
c reset with savec IENTRY2
         DO I=1,NUMNOD
           IF(IENTRY2(I)/=-1)THEN
             CALL IFRONTPLUS(I,1)
           ENDIF
         ENDDO
C        Update of FRONT for processors different from 0
C        You have to take into account the options or front (i, p) = 1, p <> 1
C        Type 2 sensors modify front on p<>1
      ELSEIF(IDDLEVEL==0) THEN
         IF(NELEM+NCONLD+GLOB_THERM%NUMCONV+GLOB_THERM%NUMRADIA+GLOB_THERM%NFXFLUX+SLCFIELD>0) THEN
           SCEP = NELEM+NCONLD+GLOB_THERM%NUMCONV+GLOB_THERM%NUMRADIA+GLOB_THERM%NFXFLUX+
     .             SLLOADP+NUMBER_LOAD_CYL+bcs%nrf_cont_nb
           SCEL = NELEM+NCONLD+GLOB_THERM%NUMCONV+GLOB_THERM%NUMRADIA+GLOB_THERM%NFXFLUX+
     .             SLLOADP+NUMBER_LOAD_CYL+bcs%nrf_cont_nb
           CALL MY_ALLOC(CEP,SCEP,"CEP")
           CALL MY_ALLOC(CEL,SCEL,"CEL")
           CEP(:) = -10
           CEL(:) = -10
         ELSE
           SCEP = 1
           SCEL = 1
           CALL MY_ALLOC(CEP,SCEP,"CEP")
           CALL MY_ALLOC(CEL,SCEL,"CEL")
         ENDIF
      ENDIF
      EMAX = MAX(NUMELC,NUMELTG,NUMELS,NUMELR,
     .             NUMELP,NUMELT,NUMELQ,NUMELX,NUMELIG3D)
      K1 = 1
      K2 = K1+EMAX
      K3 = K2+EMAX
      K4 = K3+2*EMAX
      K5 = K4+2*EMAX
      K6 = K5 + NELEM
      K7 = K6 + NELEM
      K8 = K7 + NELEM
C Average allowance normally sufficient if average group size> NVSIZ/2
      LDD_IAD =  ((NELEM+NUMSPH)/NVSIZ/2)*(NSPMD+1)
      CALL MY_ALLOC(DD_TMP,LDD_IAD,"DD_TMP")
      DD_TMP = 0
      IDX    = 1

      CALL MY_ALLOC(IWORK,K8,"IWORK")
      IF(EMAX>0) THEN
        ITRI1  => IWORK(1:K2)
        ITRI2  => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        INDEX2 => IWORK(K4+1:K5)
      ELSE
        ITRI1  => IWORK
        ITRI2  => IWORK
        INDEX1 => IWORK
        INDEX2 => IWORK
      END IF
      IF(NELEM>0) THEN
        INUM   => IWORK(K5+1:K6)
        IWD    => IWORK(K6+1:K7)
        IWEIG  => IWORK(K7+1:K8)
      ELSE
          INUM   => IWORK
          IWD    => IWORK
          IWEIG  => IWORK
      END IF

      IF(.NOT.ALLOCATED(INTER_CAND%IXINT)) ALLOCATE(INTER_CAND%IXINT(INTER_CAND%S_IXINT_1,INTER_CAND%S_IXINT_2))
      IF(.NOT. ALLOCATED(NPBY)) ALLOCATE(NPBY(0))
      IF(.NOT. ALLOCATED(LPBY)) ALLOCATE(LPBY(0))
      IF(.NOT. ALLOCATED( RBY)) ALLOCATE( RBY(0))
          CALL DOMETIS(
     1     IXS       ,IXQ      ,IXC      ,IXT     ,IXP      ,
     2     IXR       ,IXTG     ,CEP     ,GEO      ,
     3     ITRI1     ,ITRI2    ,INDEX1   ,INDEX2  ,INUM     ,
     4     IWD       ,IWCONT   ,NELEM    ,IDDLEVEL,NELEMINT ,
     5     INTER_CAND,PM ,X        ,KXX     ,IXX      ,
     6     ADDCNE    ,IGEO     ,EANI     ,IWCIN2  ,DSDOF    ,
     7     ISOLOFF   ,ISHEOFF  ,ITRIOFF  ,ITRUOFF ,IPOUOFF  ,
     8     IRESOFF   ,IELEM21  ,IPM      ,IXS10   ,D        ,
     9     CLUSTERS  ,KXIG3D   ,IXIG3D   ,COST_R2R,BUFMAT,
     1     TAILLE   ,POIN_UMP,TAB_UMP  ,
     2     POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     3     NSNT, NMNT_2,TABMP_L,IQUAOFF,
     4     IGRSURF  , FVMAIN,
     5     ITAB      ,IPART    ,IPARTC   ,IPARTG   ,IPARTS,
     .     IPARTT, IPARTP  ,IPARTR   ,IPARTX   , IPARTQ,
     6     POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     7     MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,T_MONVOL,
     8     EBCS_TAG_CELL_SPMD,NPBY,LPBY,MAT_ELEM%MAT_PARAM,
     9     iworksh,elm_group)
      CALL MY_DEALLOC(IWORK)
C---------------------------
C     Domdec SPH
C---------------------------
      IF(NUMSPH > 0)THEN
          IF(IDDLEVEL==0) THEN
            CALL MY_ALLOC(CEPSP,NUMSPH,"CEPSP")

            CALL MY_ALLOC(CELSPH,NUMSPH,"CELSPH")
          END IF

            CALL SPDOMETIS(KXSP, IXSP, NOD2SP, CEPSP, RESERVEP,
     .                     SPH2SOL, CEP)
      ELSE
          IF(IDDLEVEL==0) ALLOCATE(CELSPH(1))
          IF(.NOT.(ALLOCATED(CEPSP))) ALLOCATE(CEPSP(0),STAT=stat)
      END IF
C---------------------------
C     IMPACTS LASER TRAITEMENT SPMD 1ere Phase
C---------------------------
      IF(NLASER>0) THEN
        CALL LASERP1(ILAS   ,CEP,IXQ    )
      ENDIF
      OFF = 1
      CALL TRACE_OUT1()
C---------------------------
C     DEFINE ELEMENT GROUPS
C---------------------------
      ERR_MSG='ELEMENTS GROUPS'
      ERR_CATEGORY='ELEM/PROP/MAT COMPATIBILITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      NUMELCK8     =    NUMELC
      NUMELTGK8    =    NUMELTG
      NUMELSK8     =    NUMELS
      NUMELRK8     =    NUMELR
      NUMELPK8     =    NUMELP
      NUMELTK8     =    NUMELT
      NUMELQK8     =    NUMELQ
      NUMELXK8     =    NUMELX
      NUMELIG3DK8  =    NUMELIG3D
      NUMSPHK8     =    NUMSPH
! working int8 to avoid integer overflow for large models
      EMAX = MAX(24*NUMELCK8,25*NUMELTGK8+1,30*NUMELSK8+1,19*NUMELRK8,
     .           19*NUMELPK8+1,17*NUMELTK8,19*NUMELQK8,
     .           15*NUMELXK8+1,24*NUMELIG3DK8+1,NUMSPHK8) + 1

      CALL MY_ALLOC(IPARGTMP,NPARG,NUMEL,"IPARGTMP")
      IPARGTMP = 0
      CALL MY_ALLOC(IWORK,EMAX,"IWORK")
C     REPLACEMENT OF OUR EXTERNALS OF SS-MATERIAUX BY OUR SYSTEMS
      IF(IDDLEVEL == 0) CALL M20DCOD(MLAW_TAG,IPM, PM, MAT_ELEM%MAT_PARAM)
C
C Temporary address of DD_IAD
C Number of super groups
      NSPGROUP = 0
C Buffer Max (super group)
      LB_MAX = 0
C
      WRITE(ISTDO,'(A)')TITRE(37)
C---------------------------------
C-  PRE-SORTING + SUPER GROUPS
C---------------------------------
      NGR_SOL = 0
      IF(NUMELS/=0) THEN
        K0 = 1
        K1 = K0 + NUMELS*16
        K2 = K1 + NUMELS
        K3 = K2 + NUMELS+1
        K4 = K3 + NUMELS*2
        K5 = K4 + NUMELS*8
        !K5B= K4 + NUMELS
        K6 = K5 + NUMELS
        !warning: please also update any index change
        !         for MODIF option (MODIF_SPMD.F)
        IWORK  = 0
        INUM   => IWORK(1:K1)
        ITRI1  => IWORK(K1+1:K2)
        EADD   => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        ITRI2  => IWORK(K4+1:K5)
        ITRI3  => IWORK(K5+1:K6)
C
C
        CALL SGRHEAD(
     1       IXS     ,PM     ,GEO    ,INUM   ,
     2       ITRI1   ,EADD   ,INDEX1 ,ITRI2  ,IPARTS  ,
     3       ND      ,IGRSURF,IGRBRIC,EANI   ,
     4       CEP(OFF),ITRI3  ,IXS10  ,IXS20  ,IXS16   ,
     5       IGEO   ,IPM    ,NOD2ELS,ISOLOFF ,
     6       TAGPRT_SMS,SPH2SOL,SOL2SPH,MAT_ELEM%MAT_PARAM,
     7       SOL2SPH_TYP    ,IFLAG_BPRELOAD, CLUSTERS ,
     8       RNOISE(1,MIN(SRNOISE2,NUMELC+NUMELTG+1)),
     9       DAMP_RANGE_PART,TRIMAT)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
        IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
          CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
          DO I = 1, IDX-1
            DD_TMP2(I)=DD_TMP(I)
          END DO
          CALL MY_DEALLOC(DD_TMP)
          CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
          DO I = 1, IDX-1
            DD_TMP(I)=DD_TMP2(I)
          END DO
          CALL MY_DEALLOC(DD_TMP2)
        END IF
C 
        GRSOL_ID1 = NGROUP
        CALL SGRTAILS(
     1       IXS      ,PM       ,IPARGTMP ,GEO      ,
     2       EADD     ,ND       ,IPARTS   ,DD_TMP(IDX),
     3       IDX      ,EANI     ,INUM     ,INDEX1   ,
     4       CEP(OFF) ,ITRI1    ,IXS10    ,IGRSURF  ,IGRBRIC  ,
     5       IXS20    ,IXS16    ,IGEO     ,IDDLEVEL,
     6       IPM      ,NOD2ELS  ,ISOLOFF  ,ISOLNOD  ,
     7       TAGPRT_SMS,INIVOL  ,SPH2SOL  ,SOL2SPH  ,SOL2SPH_TYP,
     8       IFLAG_BPRELOAD, CLUSTERS ,MAT_ELEM%MAT_PARAM ,RNOISE(1,MIN(SRNOISE2,NUMELC+NUMELTG+1)),
     9       IPRI     ,DAMP_RANGE_PART,IPRELOAD_FUN)
        GRSOL_ID2 = NGROUP
        NGR_SOL   = GRSOL_ID2 - GRSOL_ID1
C
        OFF = OFF + NUMELS
C After IDDLEVEL Finish Indexes - Fill PERMUTATION%SOLID(NUMELS+1,PERMUTATION%SOLID(2*NUMELS)
        call apply_permutation(numels,permutation%solid)
!       already done in SGRHEAD / SGRTAILS
!        CALL APPLYSORT2CLUSTER(CLUSTERS,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBFFLUX,GLOB_THERM%NITFLUX,GLOB_THERM%NFXFLUX,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBCR,GLOB_THERM%NIRADIA,GLOB_THERM%NUMRADIA,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBCV,GLOB_THERM%NICONV,GLOB_THERM%NUMCONV,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        if(bcs%num_nrf>0) then
          do ii=1,bcs%num_nrf            
            call apply_permutation(1,bcs%nrf(ii)%list%size,bcs%nrf(ii)%list%elem_type,
     .                             bcs%nrf(ii)%list%elem,numels,permutation%solid)
          enddo
        endif
      ENDIF

C
C---- QUADS
C
      IF(NUMELQ/=0) THEN
          K1 = 9*NUMELQ
          K2 = 10*NUMELQ
          K3 = 11*NUMELQ+1
          K4 = 13*NUMELQ+1
          K5 = 18*NUMELQ+1
          K6 = 19*NUMELQ+1
          !warning: please also update any index change
          !         for MODIF option (MODIF_SPMD.F)
          IWORK  = 0
          INUM   => IWORK(1:K1)
          ITR1   => IWORK(K1+1:K2)
          EADD   => IWORK(K2+1:K3)
          INDEX1 => IWORK(K3+1:K4)
          ITRI1  => IWORK(K4+1:K5)
          XEP    => IWORK(K5+1:K6)
C
          CALL QGRHEAD(
     1      IXQ       ,PM       ,GEO       ,INUM     ,BID13    ,
     2      ITR1      ,EADD     ,INDEX1    ,ITRI1    ,IPARTQ   ,
     4      ND        ,IGRSURF  ,IGRQUAD   ,CEP(OFF) ,MAT_ELEM%MAT_PARAM,
     5      XEP       ,IGEO      ,IPM      ,IQUAOFF  ,TRIMAT)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL QGRTAILS(
     1      IXQ       ,PM       ,IPARGTMP   ,GEO     ,
     2      EADD      ,ND       ,DD_TMP(IDX),IDX     ,
     3      INUM      ,INDEX1   ,CEP(OFF)   ,IPARTQ  ,
     4      ITR1      ,IGRSURF  ,IGRQUAD    ,MAT_ELEM%MAT_PARAM,
     5      IGEO      ,IPM      ,IQUAOFF    ,INIVOL, IPRI)
          OFF = OFF + NUMELQ

        call apply_permutation(numelq,permutation%quad)

          if(bcs%num_nrf>0) then
            do ii=1,bcs%num_nrf            
              call apply_permutation(2,bcs%nrf(ii)%list%size,bcs%nrf(ii)%list%elem_type,
     .                               bcs%nrf(ii)%list%elem,numelq,permutation%quad)
            enddo
          endif          
       ENDIF
C
C---- COQUES
C
      IF(NUMELC/=0) THEN
C---------------------------------
C-  PRE-SORTING + SUPER GROUPS
C---------------------------------
       K0 = 1
       K1 = 9*NUMELC
       K2 = 11*NUMELC
       K3 = 12*NUMELC+1
       K4 = 14*NUMELC+1
       K5 = 22*NUMELC+1
       K6 = 23*NUMELC+1
       K7 = 24*NUMELC+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K1+NUMELC)
       ITR2   => IWORK(K1+NUMELC+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
       CALL MY_ALLOC(XNUM,NUMELC,"XNUM")
       XNUM = ZERO
C
       CALL CGRHEAD(
     1   IXC     ,PM      ,GEO     ,INUM    ,
     2   ITR1    ,EADD    ,INDEX1  ,ITRI1   ,XNUM    ,
     3   IPARTC  ,ND      ,THKE    ,IGRSURF ,IGRSH4N ,
     4   CEP(OFF),XEP     ,IGEO    ,IPM     ,
     5   IPART   ,SH4TREE ,NOD2ELC ,ISHEOFF ,SH4TRIM ,
     6   TAGPRT_SMS,LGAUGE,IWORKSH ,MAT_ELEM%MAT_PARAM,
     7   STACK   ,DRAPE   ,RNOISE  ,SH4ANG,DRAPEG, PTSHEL,
     8   DAMP_RANGE_PART)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
c
C LDD_IAD Non -Department Test
         IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
           CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
           DO I = 1, IDX-1
             DD_TMP2(I)=DD_TMP(I)
           END DO
           CALL MY_DEALLOC(DD_TMP)
           CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
           DO I = 1, IDX-1
             DD_TMP(I)=DD_TMP2(I)
           END DO
           CALL MY_DEALLOC(DD_TMP2)
         END IF

C
         CALL CGRTAILS(
     1        IXC      ,PM       ,IPARGTMP ,GEO     ,
     2        EADD     ,ND       ,IPARTC  ,DD_TMP     ,
     3        IDX      ,INUM     ,ITR1    ,
     4        INDEX1   ,CEP(OFF) ,THKE    ,XNUM       ,
     5        IGRSURF  ,IGRSH4N  ,IGEO     ,IPM     ,
     6        IPART    ,SH4TREE  ,NOD2ELC  ,ISHEOFF ,
     7        SH4TRIM  ,TAGPRT_SMS, LGAUGE,IWORKSH    ,
     8        STACK    ,DRAPE   ,RNOISE  ,MAT_ELEM%MAT_PARAM,
     9        SH4ANG, IDDLEVEL , DRAPEG,IPRI, PTSHEL,DAMP_RANGE_PART)

         OFF = OFF + NUMELC
  
        call apply_permutation(numelc,permutation%shell)

         CALL MY_DEALLOC(XNUM)
      ENDIF
C------
      IF(NUMELT/=0) THEN
       K1 = 7*NUMELT
       K2 = 8*NUMELT
       K3 = 9*NUMELT+1
       K4 = 11*NUMELT+1
       K5 = 16*NUMELT+1
       K6 = 17*NUMELT+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
C
       CALL TGRHEAD(
     1      IXT     ,PM     ,GEO    ,INUM   ,BID13   ,
     2      ITR1    ,EADD   ,INDEX1 ,ITRI1  ,
     3      IPARTT  ,ND     ,IGRSURF,IGRTRUSS,
     4      CEP(OFF),XEP    ,ITRUOFF,
     5      TAGPRT_SMS,ITAGPRLD_TRUSS)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL TGRTAILS(
     1      IXT    ,IPARGTMP,PM       ,GEO     ,
     2      EADD   ,ND      ,DD_TMP   ,IDX     ,
     3      INUM   ,INDEX1  ,CEP(OFF) ,IPARTT  ,
     4      ITR1   ,IGRSURF ,IGRTRUSS ,ITRUOFF  ,
     5      TAGPRT_SMS,NOD2EL1D,IPRI,ITAGPRLD_TRUSS,
     6      PRELOAD_A,NPRELOAD_A)
          OFF = OFF + NUMELT
      ENDIF
C
C-----
      IF(NUMELP > 0) THEN
       K1 = 9*NUMELP
       K2 = 10*NUMELP
       K3 = 11*NUMELP+1
       K4 = 13*NUMELP+1
       K5 = 18*NUMELP+1
       K6 = 19*NUMELP+1
      !warning: please also update any index change
      !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
C      
        CALL MY_ALLOC(XNUM,3*NUMELP,"XNUM")
        XNUM(1:3*NUMELP) = ZERO
C
       CALL PGRHEAD(
     1   IXP      ,PM       ,GEO      ,INUM     ,
     2   ITR1     ,EADD     ,INDEX1   ,ITRI1    ,IPARTP   ,
     3   ND       ,IGRSURF  ,IGRBEAM  ,CEP(OFF) ,
     4   XEP      ,IGEO     ,IPOUOFF  ,TAGPRT_SMS , IPM   ,
     5   ITAGPRLD_BEAM,IBEAM_VECTOR,RBEAM_VECTOR,XNUM)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL PGRTAILS(MAT_ELEM%MAT_PARAM,
     1      IXP    ,IPARGTMP,PM         ,GEO     ,
     2      EADD   ,ND      ,DD_TMP     ,IDX     ,
     3      INUM   ,INDEX1  ,CEP(OFF)   ,IPARTP  ,
     4      ITR1   ,IGRSURF ,IGRBEAM    ,IGEO    ,
     5      IPM    ,IPOUOFF ,TAGPRT_SMS,
     6      NOD2EL1D, IPRI  ,ITAGPRLD_BEAM,PRELOAD_A,
     7      NPRELOAD_A,IBEAM_VECTOR,RBEAM_VECTOR,XNUM)
C    
          OFF = OFF + NUMELP
C          
          CALL MY_DEALLOC(XNUM)
        ENDIF
C
C-----
C
      IF(NUMELR/=0) THEN
        K1 = 9*NUMELR
        K2 = 10*NUMELR
        K3 = 11*NUMELR+1
        K4 = 13*NUMELR+1
        K5 = 18*NUMELR+1
        K6 = 19*NUMELR+1
        !warning: please also update any index change
        !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
C
          CALL RGRHEAD(
     1      IXR     ,GEO    ,INUM   ,BID13   ,IGEO   ,
     2      ITR1    ,EADD   ,INDEX1 ,ITRI1   ,
     4      IPARTR  ,ND     ,IGRSURF,IGRSPRING,
     5      CEP(OFF),XEP    ,IRESOFF,
     6      TAGPRT_SMS, CLUSTERS,IPM,R_SKEW,ITAGPRLD_SPRING)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL RGRTAILS(
     1      IXR      ,IPARGTMP   ,GEO      ,EADD      ,IGEO     ,
     2      ND       ,DD_TMP     ,IDX      ,INUM     ,
     3      INDEX1   ,CEP(OFF)   ,IPARTR   ,ITR1     ,
     4      IGRSURF  ,IGRSPRING  ,IRESOFF  ,TAGPRT_SMS ,NOD2EL1D,
     5      IPM , CLUSTERS,R_SKEW,IPRI     ,ITAGPRLD_SPRING,
     6      PRELOAD_A,NPRELOAD_A)
          OFF = OFF + NUMELR
        ENDIF
C
      IF(NUMELTG/=0) THEN
C---------------------------------
C-  PRE-SORTING + SUPER GROUPS
C---------------------------------
       K1 = 10*NUMELTG
       K2 = 12*NUMELTG
       K3 = 13*NUMELTG+1
       K4 = 15*NUMELTG+1
       K5 = 23*NUMELTG+1
       K6 = 24*NUMELTG+1
       K7 = 25*NUMELTG+1
       K8 = 26*NUMELTG+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)

       CALL MY_ALLOC(XNUM,NUMELTG,"XNUM")
       XNUM = ZERO

       IF(NUMELTG6>0) THEN
         CALL CDK6INX(IXTG   ,IXTG1   ,EANIT   )
       ENDIF
       IF(N2D==0)THEN
         CALL C3GRHEAD(
     1     IXTG    ,PM      ,GEO      ,INUM    ,
     2     ITR1    ,EADD    ,INDEX1   ,ITRI1   ,XNUM   ,
     3     IPARTG  ,ND      ,THKEC    ,IGRSURF ,IGRSH3N ,
     4     CEP(OFF),XEP      ,IXTG1   ,EANIT   ,
     5     IGEO    ,IPM     ,IPART    ,SH3TREE ,NOD2ELTG,
     6     ITRIOFF ,SH3TRIM ,TAGPRT_SMS,
     7     IWORKSH , STACK  ,DRAPE  ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)),
     8     SH3ANG,DRAPEG,PTSH3N,MAT_ELEM%MAT_PARAM,
     9     DAMP_RANGE_PART)
       ELSE
         CALL T3GRHEAD(
     1     IXTG    ,PM      ,GEO      ,INUM    ,BID13   ,
     2     ITR1    ,EADD    ,INDEX1   ,ITRI1   ,XNUM   ,
     3     IPARTG  ,ND      ,THKEC    ,IGRSURF ,IGRSH3N ,
     4     CEP(OFF),XEP     ,IXTG1    ,EANIT   ,
     5     IGEO    ,IPM     ,IPART    ,SH3TREE ,NOD2ELTG,
     6     ITRIOFF ,SH3TRIM ,TAGPRT_SMS,MAT_ELEM%MAT_PARAM,
     7     IWORKSH , STACK  ,DRAPE  ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)),
     8     MULTI_FVM ,SH3ANG,DRAPEG,PTSH3N)
       ENDIF
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
         IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
           CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
           DO I = 1, IDX-1
             DD_TMP2(I)=DD_TMP(I)
           END DO
           CALL MY_DEALLOC(DD_TMP)
           CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
           DO I = 1, IDX-1
             DD_TMP(I)=DD_TMP2(I)
           END DO
           CALL MY_DEALLOC(DD_TMP2)
         END IF
C
         IF(N2D==0)THEN
           CALL C3GRTAILS(
     1          IXTG     ,PM       ,IPARGTMP ,GEO      ,
     2          EADD     ,ND       ,IPARTG   ,DD_TMP     ,
     3          IDX      ,INUM     ,INDEX1   ,CEP(OFF)   ,
     4          THKEC    ,XNUM     ,ITR1     ,IGRSURF  ,IGRSH3N    ,
     5          EANIT    ,IGEO     ,IPM      ,IXTG1    ,
     6          IPART    ,SH3TREE  ,NOD2ELTG ,ITRIOFF  ,
     7          SH3TRIM  ,TAGPRT_SMS,IWORKSH ,STACK      ,
     8          DRAPE   ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)) ,
     9          MAT_ELEM%MAT_PARAM,SH3ANG,DRAPEG,IPRI ,PTSH3N,DAMP_RANGE_PART)
         ELSE
           CALL T3GRTAILS(
     1          IXTG     ,PM       ,IPARGTMP ,GEO      ,
     2          EADD     ,ND       ,IPARTG   ,DD_TMP     ,
     3          IDX      ,INUM     ,INDEX1   ,CEP(OFF)   ,
     4          THKEC    ,XNUM     ,ITR1     ,IGRSURF  ,IGRSH3N    ,
     5          EANIT    ,IGEO     ,IPM      ,IXTG1    ,
     6          IPART    ,SH3TREE  ,NOD2ELTG ,ITRIOFF  ,
     7          SH3TRIM  ,TAGPRT_SMS,IWORKSH ,STACK      ,
     8          DRAPE   ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)) ,INIVOL,
     9          MAT_ELEM%MAT_PARAM,SH3ANG ,DRAPEG,IPRI,PTSH3N)
         ENDIF
         OFF = OFF + NUMELTG
 
        call apply_permutation(numeltg,permutation%triangle)

          if(bcs%num_nrf>0) then
            do ii=1,bcs%num_nrf            
              call apply_permutation(7,bcs%nrf(ii)%list%size,bcs%nrf(ii)%list%elem_type,
     .                               bcs%nrf(ii)%list%elem,numeltg,permutation%triangle)
            enddo
          endif            

       CALL MY_DEALLOC(XNUM)
      ENDIF

      CALL APPLYSORT2FVM(T_MONVOL)

C---------------------------------
      IF(NUMSPH/=0) THEN
            if ( .NOT. ALLOCATED(IXSPS) ) ALLOCATE(IXSPS(KVOISPH,NUMSPH),STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                 MSGTYPE=MSGERROR,
     .                                 C1='IXSPS')
      
          IWORK  = 0
          EADD   => IWORK(1:NUMSPH+1)
          CALL SPGRHEAD(KXSP    ,IXSP    ,IPARGTMP,PM     ,IPART  ,
     2                  IPARTSP ,EADD    ,CEPSP   ,ND     ,IPM    ,
     3                  IGEO    ,SPBUF   ,SPH2SOL,
     4                  SOL2SPH ,IRST    ,MAT_ELEM%MAT_PARAM,IXSPS)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL SPGRTAILS(KXSP    ,IPARGTMP,PM      ,IPART      ,
     2                   IPARTSP ,EADD    ,ND      ,CEPSP,DD_TMP     ,
     3                   IDX     ,IXSP    ,IPM  , IGEO      ,
     4                   SPBUF   ,SPH2SOL,SOL2SPH  ,
     5                   IRST    ,NOD2SP  ,IPRI    ,MAT_ELEM%MAT_PARAM,
     6                   IXSPS)
        IF (ALLOCATED(IXSPS)) DEALLOCATE(IXSPS)
        ENDIF
C---------------------------------
      IF(NUMELX>0) THEN
!
          K1=6*NUMELX
          K2=K1+NUMELX
          K3=K2+NUMELX+1
          K4=K3+NUMELX*2
          K5=K4+NUMELX*4
          K6=K5+NUMELX
          !warning: please also update any index change
          !         for MODIF option (MODIF_SPMD.F)
          IWORK  = 0
          INUM   => IWORK(1:K1)
          ITR1   => IWORK(K1+1:K2)
          EADD   => IWORK(K2+1:K3)
          INDEX1 => IWORK(K3+1:K4)
          ITRI1  => IWORK(K4+1:K5)
          XEP    => IWORK(K5+1:K6)
!
          CALL XGRHEAD(
     1     KXX,      GEO,    INUM,    ITR1,
     2     EADD,     INDEX1, ITRI1,   IPARTX,
     3     ND,       IGRSURF,
     4     CEP(OFF), XEP,IPM)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            CALL MY_DEALLOC(DD_TMP)
            CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            CALL MY_DEALLOC(DD_TMP2)
          END IF
C
          CALL XGRTAILS(
     1      KXX    ,IPARGTMP   ,GEO     ,EADD     ,
     2      ND     ,DD_TMP     ,IDX     ,LB_MAX  ,INUM     ,
     3      INDEX1 ,CEP(OFF)   ,IPARTX  ,ITR1    ,IGRSURF  ,
     4      IXX    ,IGEO)
           OFF = OFF + NUMELX
       ENDIF
      CALL TRACE_OUT1()
C
C build Inverse connectivity - update after all element/sph grouping
C
        KNOD2ELS  = 0
        KNOD2ELC  = 0
        KNOD2ELTG = 0
        KNOD2EL1D  = 0
        KNOD2ELIG3D  = 0
        NOD2ELS   = 0
        NOD2ELC   = 0
        NOD2ELTG  = 0
        NOD2EL1D   = 0
        NOD2ELIG3D   = 0
        KNOD2ELQ  = 0
        NOD2ELQ  = 0
        CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )

C---------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELIG3D>0) THEN

        K1=(NIXIG3D+1)*NUMELIG3D
        K2=K1+NUMELIG3D
        K3=K2+NUMELIG3D+1
        K4=K3+NUMELIG3D*2
        K5=K4+NUMELIG3D*4
        K6=K5+NUMELIG3D

        IWORK  = 0
        INUM   => IWORK(1:K1)
        ITR1   => IWORK(K1+1:K2)
        EADD   => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        ITRI1  => IWORK(K4+1:K5)
        XEP    => IWORK(K5+1:K6)
C
        CALL IG3DGRHEAD(
     1    KXIG3D    ,GEO       ,INUM      ,ITR1      ,EADD      ,
     2    INDEX1    ,ITRI1     ,IPARTIG3D ,ND        ,IGRSURF   ,
     3    CEP(OFF)  ,XEP       ,IGEO      ,
     4    IPM       ,PM        ,NIGE      ,KNOTLOCEL)
C--------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C LDD_IAD Non -Department Test
        IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
          CALL MY_ALLOC(DD_TMP2,IDX-1,"DD_TMP2")
          DO I = 1, IDX-1
            DD_TMP2(I)=DD_TMP(I)
          END DO
          CALL MY_DEALLOC(DD_TMP)
          CALL MY_ALLOC(DD_TMP,IDX+ND*(NSPMD+1),"DD_TMP")
          DO I = 1, IDX-1
            DD_TMP(I)=DD_TMP2(I)
          END DO
          CALL MY_DEALLOC(DD_TMP2)
        END IF
C
        CALL IG3DGRTAILS(
     1    KXIG3D    ,IPARGTMP  ,GEO       ,EADD      ,ND        ,
     2    DD_TMP    ,IDX       ,LB_MAX    ,INUM      ,INDEX1    ,
     3    CEP(OFF)  ,IPARTIG3D ,ITR1      ,IGRSURF   ,
     4    IXIG3D    ,IGEO      ,
     5    PM        ,NIGE      ,KNOTLOCEL, MAT_ELEM%MAT_PARAM)
        OFF = OFF + NUMELIG3D
      ENDIF
      CALL MY_DEALLOC(IWORK)
      CALL TRACE_OUT1()

C--------------------------------------------
C     REFERENCE METRIQUE
C--------------------------------------------
      ERR_MSG='REFERENCE METRICS'
      ERR_CATEGORY='REFERENCE METRICS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      XYZREF = X
C
      IF(IDDLEVEL==0)THEN

        IF(NXREF > 0 .OR. NEREF > 0 .OR. IREFSTA > 0) THEN
          CALL MY_ALLOC(XREFC,4,3,NUMELC,"XREFC")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFC')
          CALL MY_ALLOC(XREFTG,3,3,NUMELTG,"XREFTG")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFTG')
          CALL MY_ALLOC(XREFS,8,3,NUMELS8,"XREFS")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFS')
          CALL MY_ALLOC(TAGXREF,NUMNOD,"TAGXREF")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGXREF')
          CALL MY_ALLOC(TAGREFSTA,NUMNOD,"TAGREFSTA")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGREFSTA')
        ELSE
          CALL MY_ALLOC(XREFC,1,1,1,"XREFC")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFC')
          CALL MY_ALLOC(XREFTG,1,1,1,"XREFTG")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFTG')
          CALL MY_ALLOC(XREFS,1,1,1,"XREFS")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFS')
          CALL MY_ALLOC(TAGXREF,1,"TAGXREF")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGXREF')
          CALL MY_ALLOC(TAGREFSTA,1,"TAGREFSTA")
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGREFSTA')
        ENDIF
      ENDIF
C
      TAGXREF = 0
      TAGREFSTA = 0
C
      IF(NXREF > 0) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (XREF)'
        CALL HM_READ_XREF(ITABM1   ,IPART    ,IPARTC   ,IPARTG   ,IPARTS    ,
     .                    UNITAB   ,IXC      ,IXTG     ,IXS      ,X         ,
     .                    XREFC    ,XREFTG   ,XREFS    ,RTRANS   ,LSUBMODEL ,
     .                    TAGXREF  ,IDDLEVEL ,EANI     ,IPM      ,IGEO )
      ENDIF
      IF(IREFSTA > 0) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (REFSTA)'
        CALL LECREFSTA(ITABM1  ,UNITAB  ,IXC     ,IXTG  ,IXS     ,
     .                 XYZREF  ,XREFC   ,XREFTG  ,XREFS ,TAGXREF ,
     .                 IDDLEVEL,TAGREFSTA )
C
        IF(IDDLEVEL==0 .AND. ((NINTER > 0).OR.(ISMS == 1))) THEN
          REWIND(IIN6)
        ELSE
          IF(IPID /= 0) CLOSE(IIN6)
          IREFSTA = 0
C          NXREF   = NXREF + 1
          NXREF   = 1
        ENDIF
      ENDIF
      IF(NEREF > 0 ) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (EREF)'
        CALL HM_READ_EREF(ITABM1  ,IPART   ,IPARTC  ,IPARTG  ,IPARTS  ,
     .                    IXC     ,IXTG    ,IXS     ,X       ,XREFC   ,
     .                    XREFTG  ,XREFS   ,LSUBMODEL,IDDLEVEL,ITAB    ,
     .                    TAGXREF ,TAGREFSTA )
C
        IF(IDDLEVEL ==1 .OR. ((NINTER == 0).AND.(ISMS == 0))) NXREF = 1
      ENDIF
C-------------------------------------------------
      !check if a law 151 is associated to any PART
      MULTI_FVM%IS_ASSOCIATED_TO_A_PART = .FALSE.
      DO NG=1,NGROUP
        MLW = IPARGTMP(1,NG)
        IF(MLW == 151)THEN
          MULTI_FVM%IS_ASSOCIATED_TO_A_PART = .TRUE.
          EXIT
        ENDIF
      ENDDO
C-------------------------------------------------
      !check if all part are using law 151
      MULTI_FVM%ARE_ALL_PARTS_151 = .TRUE.
      NB_EULER_GROUPS = 0
      DO NG=1,NGROUP
        MLW = IPARGTMP(1,NG)
        IS_EULER = IPARGTMP(11,NG)
        IF(IS_EULER == 1 ) NB_EULER_GROUPS=NB_EULER_GROUPS+1
        IF(MLW /= 151 .AND. IS_EULER == 1)THEN
          MULTI_FVM%ARE_ALL_PARTS_151 = .FALSE.
          EXIT
        ENDIF
      ENDDO
      IF(NB_EULER_GROUPS == 0)MULTI_FVM%ARE_ALL_PARTS_151 = .FALSE.
C-------------------------------------------------
      !copy IPARG <- IPARGTMP, and Deallocate IPARGTMP
      SIPARG = NPARG*NGROUP
      IF(ALLOCATED(IPARG))CALL MY_DEALLOC(IPARG)
      CALL MY_ALLOC(IPARG,SIPARG,"IPARG")
      DO J=1,NPARG
       DO I=1,NGROUP
        IPARG((I-1)*NPARG + J) = IPARGTMP(J,I)
       ENDDO
      ENDDO
      CALL MY_DEALLOC(IPARGTMP)
C-------------------------------------------------
C provisional
      IF(ISMS/=0)THEN
        DO N=1,NGROUP
          IPARG(NPARG*(N-1)+52)=1
        END DO
      END IF
      NBR_GPMP = NGROUP
c-----------------------------------------------------------------------
!
      !---------------------------------------------------
      ! element renumbering after domdec for /fail/fractal 
      !---------------------------------------------------
      call fractal_elem_renum(fail_fractal,numelc,numeltg)
!
      !---------------------------------------------------
      ! element renumbering after domdec for /fail/alter + brokmann 
      !---------------------------------------------------
      if (iddlevel==1) call brokmann_elem_renum(fail_brokmann,numelc,numeltg)
c-----------------------------------------------------------------------
c     set default material/property parameters by element group
c-----------------------------------------------------------------------
      IF(ALLOCATED(GROUP_PARAM_TAB)) DEALLOCATE(GROUP_PARAM_TAB)
      allocate(GROUP_PARAM_TAB(NGROUP))
c
      CALL SET_ELGROUP_PARAM(GROUP_PARAM_TAB  ,IPARG    ,NGROUP   ,N2D       ,
     .                       IPM     ,IGEO    ,PM       ,GEO      ,BUFMAT    )
C-------------------------------------------------
      CALL TRACE_OUT1()
C--------------------------------------------
C     Itet=2 of S10 : dynamic condensation
C--------------------------------------------
      IF(NUMELS10>0) THEN
       IF(ALLOCATED(ITAGND)) CALL MY_DEALLOC(ITAGND)
       CALL MY_ALLOC(ITAGND,NUMNOD,"ITAGND")
       ITAGND(1:NUMNOD)=0
       CALL DIM_S10EDG(NS10E, IXS10 ,IPARG,ITAGND)
       IF(NS10E>0) THEN
        IF(ALLOCATED(ICNDS10)) CALL MY_DEALLOC(ICNDS10)
        CALL MY_ALLOC(ICNDS10,3*NS10E,"ICNDS10")
        ICNDS10(1:3*NS10E)=0
        ITAGND(1:NUMNOD)=0
        CALL IND_S10EDG(ICNDS10, IXS, IXS10 ,IPARG,ITAGND)
        IF(IPARI0/=0) CALL REORD_ICND(ICNDS10, ITAGND)
        CALL S10EDG_RLINK(NLINK, NUMLINK,NNLINK,LNLINK,
     .                    ITAGND,ICNDS10,ITAB,IPRI,NUMNOD,NS10E)
       END IF
      ELSE
       IF(ALLOCATED(ITAGND)) CALL MY_DEALLOC(ITAGND)
       CALL MY_ALLOC(ITAGND,0,"ITAGND")
      END IF

!     replace old /MAT/RIGID by RBODY by part	  
      IF (N_NRBODY>0) IRIGID_MAT = 0
C--------------------------------------------
C     Domain Decomposition 1 (Reconstruction of Tables)
C--------------------------------------------
C If nspmd = 1 you have to build dd_iad and fr_iad
      ERR_MSG='DOMAIN DECOMPOSITION ARRAYS'
      ERR_CATEGORY='DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SDD_IAD = (NSPMD+1)*NSPGROUP
      CALL MY_ALLOC(DD_IAD,SDD_IAD,"DD_IAD")
      DD_IAD = 0
      CALL DOMDEC1(
     1       IPARG  ,IXS    ,IXQ       ,IXC     ,IXT     ,
     2       IXP    ,IXR    ,IXTG    ,DD_IAD  ,
     3       X      ,DD_TMP    ,IXS10   ,IXS20   ,
     4       IXS16  ,KXX    ,IXX       ,KXSP    ,IXSP    ,
     5       CEPSP  ,IXTG1)
C
      CALL MY_DEALLOC(DD_TMP)
      CALL TRACE_OUT1()
C--------------------------------------------
C     Multidomains -> modif domdec
C--------------------------------------------
      ERR_MSG='MULTIDOMAINS'
      ERR_CATEGORY='MULTIDOMAINS'
      IF((NSUBDOM>0).AND.(IDDOM==0).AND.(FLG_R2R_ERR==0)) THEN
        WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DOMDEC SYNCHRONIZATION '
        CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,0)
      ENDIF
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (RESOLUTION) REEL
C--------------------------------------------
      ERR_MSG='ELEMENT BUFFER ALLOCATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SELBUF = LBUFEL
      CALL MY_ALLOC(ELBUF,SELBUF,"ELBUF")
      ELBUF = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     GRAVITY
C--------------------------------------------
      ERR_MSG='GRAVITY'
      ERR_CATEGORY='GRAVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c      CALL PRELECGRAV(NUMGRAV ,IGRNOD)
      CALL HM_PREREAD_GRAV(NUMGRAV ,IGRNOD , LSUBMODEL,N_NRBODY)
      SIGRV = NIGRV*NGRAV
      SLGRAV = NUMGRAV
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(IGRV,SIGRV,"IGRV")
        CALL MY_ALLOC(LGRAV,SLGRAV,"LGRAV")
      END IF
      IGRV = 0
      CALL HM_READ_GRAV(IGRV   ,LGRAV   ,GRAV      ,ITAB    ,ITABM1  ,
     .                 IGRNOD  ,NPC     ,SENSORS   ,UNITAB  ,ISKWN   ,
     .                 ITAGND  ,LSUBMODEL)
      CALL TRACE_OUT1()
C----------------------------------
C     READING OF INIGRAV MAPS
C----------------------------------
      ERR_MSG='INIGRAV'
      ERR_CATEGORY='GRAVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINIGRAV > 0) SINIGRAV = NINIGRAV
      IF(IDDLEVEL == 0) THEN
        CALL MY_ALLOC(INIGRV,04,SINIGRAV,"INIGRV")
        CALL MY_ALLOC(LINIGRAV,11,SINIGRAV,"LINIGRAV")
      END IF
      IF(NINIGRAV > 0) THEN
        IF(IDDLEVEL == 0) THEN
          INIGRV = 0
          WRITE(ISTDO,'(A)') ' .. INITIAL GRAVITY LOADING'
          CALL HM_READ_INIGRAV(IGRV     ,LGRAV    ,GRAV     ,ITAB     ,ITABM1   ,
     .                         IGRPART  ,NPC      ,UNITAB   ,ISKWN    ,
     .                         ITAGND   ,IGRSURF  ,TF       ,BUFSF    ,LSUBMODEL)
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     READING OF INIMAP1D MAPS
C----------------------------------
      ERR_MSG = 'INIMAP1D'
      ERR_CATEGORY= 'INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      IF(IDDLEVEL==0 .AND. NINIMAP1D+NINIMAP2D>0)WRITE(ISTDO,'(A)')TITRE(54)

      IF(IDDLEVEL==0)THEN
        allocate(INIMAP1D(NINIMAP1D))
        IF(NINIMAP1D > 0) THEN
          CALL HM_READ_INIMAP1D(INIMAP1D ,NPC     , ITABM1, X, IGRBRIC,
     .                      IGRQUAD  ,IGRSH3N, MULTI_FVM, UNITAB, LSUBMODEL)
           IF(.NOT. MULTI_FVM%IS_USED) THEN
              DO KK = 1, NINIMAP1D
                 call my_alloc(INIMAP1D(KK)%TAGNODE, NUMNOD, "INIMAP1D(KK)%TAGNODE")
                 INIMAP1D(KK)%TAGNODE(1:NUMNOD) = 0
              ENDDO
           ENDIF
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     Reading INIMAP2D cards
C----------------------------------
      ERR_MSG = 'INIMAP2D'
      ERR_CATEGORY= 'INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IDDLEVEL==0)THEN
        allocate(INIMAP2D(NINIMAP2D))
        IF(NINIMAP2D > 0) THEN
          CALL HM_READ_INIMAP2D(INIMAP2D, FUNC2D, ITABM1, X, IGRBRIC,
     .                      IGRQUAD , IGRSH3N, UNITAB, LSUBMODEL)
           IF(.NOT. MULTI_FVM%IS_USED) THEN
              DO KK = 1, NINIMAP2D
                 call my_alloc(INIMAP2D(KK)%TAGNODE, NUMNOD, "INIMAP2D(KK)%TAGNODE")
                 INIMAP2D(KK)%TAGNODE(1:NUMNOD) = 0
              ENDDO
           ENDIF
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     "LOAD FIELDS" : CENTRIFUGAL,FLUID,BLAST
C--------------------------------------------
      !ALLOCATIONS
      ERR_MSG='LOAD FIELDS'
      ERR_CATEGORY='LOAD FIELDS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IDDLEVEL == 0)THEN
C
C        Centrifugal Loads
         CALL MY_ALLOC(ICFIELD,SICFIELD,"ICFIELD")
         CALL MY_ALLOC(LCFIELD,SLCFIELD,"LCFIELD")
         CALL MY_ALLOC(CFIELD,SCFIELD,"CFIELD")
!INITIALIZATIONS
         IF(ALLOCATED(ICFIELD)) ICFIELD(:) = 0
         IF(ALLOCATED(LCFIELD)) LCFIELD(:) = 0
         IF(ALLOCATED(CFIELD )) CFIELD(:)  = ZERO
!READING CARDS & STORING DATA
         IF(NLOADC/=0)THEN
           CALL HM_READ_LOAD_CENTRI(ICFIELD ,LCFIELD ,CFIELD    ,ITAB    ,ITABM1  ,
     .                              IGRNOD  ,NPC     ,SENSORS   ,UNITAB  ,IFRAME  ,
     .                              LSUBMODEL)
         END IF
C
C        PFLUID & PBLAST & LOAD PRESSURE
         CALL MY_ALLOC(ILOADP,SILOADP,"ILOADP")
         CALL MY_ALLOC(LLOADP,SLLOADP,"LLOADP")
         CALL MY_ALLOC(LOADP,SLOADP,"LOADP")
         CALL MY_ALLOC(INTERLOADP,NINTLOADP,"INTERLOADP")
         CALL MY_ALLOC(INTGAPLOADP,NINTLOADP,"INTGAPLOADP")

         S_LOADPINTER = 0
         IF(NINTLOADP > 0) THEN
            S_LOADPINTER = NINTER*NLOADP_HYD            
            CALL MY_ALLOC(KLOADPINTER,NINTER + 1,"KLOADPINTER")
            CALL MY_ALLOC(LOADPINTER,S_LOADPINTER,"LOADPINTER")
            CALL MY_ALLOC(DGAPINT,NINTER,"DGAPINT")
            CALL MY_ALLOC(DGAPLOADINT,S_LOADPINTER,"DGAPLOADINT")
         ELSE
            CALL MY_ALLOC(KLOADPINTER,0,"KLOADPINTER")
            CALL MY_ALLOC(LOADPINTER,0,"LOADPINTER")
            CALL MY_ALLOC(DGAPINT,0,"DGAPINT")
            CALL MY_ALLOC(DGAPLOADINT,0,"DGAPLOADINT")
         ENDIF

!INITIALIZATIONS
         IF(ALLOCATED(ILOADP )) ILOADP(:)  = 0
         IF(ALLOCATED(LLOADP )) LLOADP(:)  = 0
         IF(ALLOCATED(LOADP  )) LOADP(:)   = ZERO
         IF(ALLOCATED(INTERLOADP )) INTERLOADP(:)  = 0
         IF(ALLOCATED(KLOADPINTER )) KLOADPINTER(:)  = 0
         IF(ALLOCATED(LOADPINTER ))  LOADPINTER(:)  = 0
         IF(ALLOCATED( INTGAPLOADP ))  INTGAPLOADP(:)=ZERO
         IF(ALLOCATED( DGAPINT ))  DGAPINT(:)=ZERO
         IF(ALLOCATED( DGAPLOADINT )) DGAPLOADINT(:)=ZERO
!READING CARDS & STORING DATA
         NUMLOADP=0
         NINTLOADP = 0
         NINTLOADP21 = 0
         IF(NLOADP_F/=0)THEN
           CALL HM_READ_PFLUID(NUMLOADP ,ILOADP  ,LLOADP  ,LOADP   ,NPC      ,
     .                        SENSORS   ,IGRSURF ,UNITAB  ,IFRAME  ,LSUBMODEL)
         END IF
         IF(PBLAST%NLOADP_B/=0)THEN
           CALL HM_READ_PBLAST( PBLAST,
     .                          ITAB    ,ITABM1  ,UNITAB  ,IGRSURF,  NUMLOADP,
     .                          ILOADP   ,LLOADP  ,LOADP   ,X      , BUFSF    ,
     .                          LSUBMODEL,RTRANS)
         ENDIF
         IF(NLOADP_HYD/=0)THEN
           CALL HM_READ_LOAD_PRESSURE(
     .                       NUMLOADP   ,ILOADP     ,LLOADP       ,INTERLOADP ,LOADP      ,
     .                       KLOADPINTER,LOADPINTER ,NPC          ,SENSORS    ,IGRSURF    ,
     .                       UNITAB     ,ISKWN      ,LSUBMODEL    ,DGAPINT    ,INTGAPLOADP,
     .                       DGAPLOADINT,S_LOADPINTER,PBLAST)

         END IF

         CALL MY_DEALLOC(INTERLOADP)
         CALL MY_DEALLOC(INTGAPLOADP)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     Reading RBE2 constraints
C--------------------------------------------
      ERR_MSG='RBE2'
      ERR_CATEGORY='RBE2'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PREREAD_RBE2(SIRBE2,SLRBE2,IGRNOD,LSUBMODEL)
       IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(IRBE2,SIRBE2,"IRBE2")
       END IF
       IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(LRBE2,SLRBE2,"LRBE2")
       END IF
       IF(SIRBE2 > 0) THEN
        IRBE2 = 0
        LRBE2 = 0
        CALL HM_READ_RBE2(
     .        IRBE2   ,LRBE2 ,ITAB      ,ITABM1    ,IGRNOD,
     .        ISKWN   ,D     ,IDDLEVEL  ,NOM_OPT(LNOPT1*INOM_OPT(13)+1),ITAGND,
     .        ICNDS10 ,LSUBMODEL)
       ENDIF


      CALL C_NEW_HASH(GRNOD_UID,NGRNOD)
      DO I=1,NGRNOD
         CALL C_HASH_INSERT(GRNOD_UID,IGRNOD(I)%ID,I)
      ENDDO

C--------------------------------------------
C     READING RBE3 Interpolation Constraints
C--------------------------------------------
      CALL TRACE_OUT1()
      ERR_MSG='RBE3'
      ERR_CATEGORY='RBE3'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PREREAD_RBE3(SIRBE3,SLRBE3,IGRNOD,GRNOD_UID,LSUBMODEL)
       LXINTD = 0
       SLRBE3 = 2*SLRBE3
       SFRBE3 = (3+1)*SLRBE3
       IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(IRBE3,SIRBE3,"IRBE3")
        call my_alloc(LRBE3, SLRBE3, "LRBE3", stat=stat)
        call my_alloc(FRBE3, SFRBE3, "FRBE3", stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LRBE3')
       END IF
       IF(SIRBE3 > 0) THEN
        IRBE3 = 0
        LRBE3 = 0
        FRBE3 = ZERO
          CALL HM_READ_RBE3(IRBE3  ,LRBE3  ,FRBE3  ,ITAB   ,ITABM1  ,
     .                      IGRNOD ,ISKWN  ,LXINTD ,D    ,IDDLEVEL,
     .                      NOM_OPT(LNOPT1*INOM_OPT(14)+1),ITAGND ,
     .                      GRNOD_UID,UNITAB,LSUBMODEL)
       ENDIF

      CALL C_DELETE_HASH(GRNOD_UID)

      CALL TRACE_OUT1()

C---------------------------------------------
C    CHECK ENGINE FILE : DYNAIN FILE
C-------------------------------------------
      DYNAIN_DATA%DYNAIN_CHECK = 0
      CALL CHECK_DYNAIN(IPART,IPARTC,IPARTG,IXC,IXTG,DYNAIN_DATA%DYNAIN_CHECK)

C---------------------------------------------
C    CHECK ENGINE FILE /H3D/?/TMAX initialization
C-------------------------------------------
      CALL INI_H3DTMAX_ENGINE(IPARG,IPART,IPARTS,IPARTC,IPARTG,IDDLEVEL)
C---------------------------------------------
C    CHECK ENGINE FILE DYNAIN or STATE if to use F.I. total strain for QEPH
C-------------------------------------------
      ISTR_24 = 0
      IF(NUMELC/=0) CALL CHECK_QEPH_STRA(ISTR_24)
      IF (ISIGI < 0) ISTR_24=1 
C--------------------------------------------
C     Reading deactivatable elements
C--------------------------------------------
      ERR_MSG='ELEMENTS DEACTIVATION'
      ERR_CATEGORY='ELEMENTS DEACTIVATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIACTIV = LACTIV*NACTIV
      IF(IDDLEVEL==0)THEN
        call my_alloc(IACTIV, SIACTIV, "IACTIV", stat=stat)
        call my_alloc(FACTIV, LRACTIV*NACTIV, "FACTIV", stat=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR, C1='IACTIV')
      END IF
      IACTIV = 0
      IF(NACTIV > 0) CALL HM_READ_ACTIV(IACTIV   ,FACTIV  ,SENSORS,IGRBRIC,
     .                             IGRQUAD  ,IGRSH4N ,IGRSH3N ,IGRTRUSS,IGRBEAM,
     .                             IGRSPRING,LSUBMODEL,UNITAB)
C--------------------------------------------
      SIBMPC = NUMMPC + LMPC*3
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(IBMPC,SIBMPC,"IBMPC")
      END IF
      IBMPC  = 0
      IF(LMPC>0) THEN
      IBMPC2 => IBMPC(NUMMPC+1:NUMMPC+LMPC)
      IBMPC3 => IBMPC(NUMMPC+LMPC+1:NUMMPC+LMPC*2)
      IBMPC4 => IBMPC(NUMMPC+LMPC*2+1:SIBMPC)
      ELSE
        IBMPC2 => IBMPC
        IBMPC3 => IBMPC
        IBMPC4 => IBMPC
      END IF

      SKINET = NUMNOD
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(KINET,SKINET,"KINET")
      END IF
      KINET  = 0

      SIPARI = NPARI*NINTER
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(IPARI,SIPARI,"IPARI")
      END IF
      IPARI = 0
      CALL TRACE_OUT1()
C--------------------------------------------
C     External faces of solid elements
C--------------------------------------------
      ERR_MSG='SOLID ELEMENTS FACES'
      ERR_CATEGORY='SOLID ELEMENTS FACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_ALLOC(FASTAG,NUMELS,"FASTAG")
      CALL ANI_FASOLFR1(IXS,IXC,IXTG,FASTAG,ISOLNOD)
      SFASOLFR = 2*NFASOLFR
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(FASOLFR,SFASOLFR,"FASOLFR")
      END IF
      CALL ANI_FASOLFR2(FASTAG,FASOLFR,ISOLNOD)
      CALL MY_DEALLOC(FASTAG)
      CALL TRACE_OUT1()
C--------------------------------------------
C     External Segs of quad elements
C--------------------------------------------
      ERR_MSG='QUAD ELEMENTS SEGS'
      ERR_CATEGORY='QUAD ELEMENTS SEGS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_ALLOC(SEGTAG,4*NUMELQ,"SEGTAG")
       SEGTAG (1:4*NUMELQ) = 0
      CALL ANI_SEGQUADFR1(IXQ ,SEGTAG ,KNOD2ELQ ,NOD2ELQ ,X ,NSEGQUADFR)
      SSEGQUADFR = 2*NSEGQUADFR
      IF(IDDLEVEL==0)THEN
        CALL MY_ALLOC(SEGQUADFR,SSEGQUADFR,"SEGQUADFR")
      END IF
      CALL ANI_SEGQUADFR2(SEGTAG,SEGQUADFR)
      CALL MY_DEALLOC(SEGTAG)
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTI-POINT CONSTRAINTS (2)
C--------------------------------------------
      ERR_MSG='MPCS 2'
      ERR_CATEGORY='MPCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMMPC > 0) CALL HM_READ_MPC (
     1           RBMPC   ,IBMPC  ,IBMPC2  ,IBMPC3  ,IBMPC4  ,
     2           ISKWN   ,ITAB   ,ITABM1  ,LAG_NCF ,LAG_NKF ,
     3           LAG_NHF ,D      ,IKINE1LAG,
     4           NOM_OPT(LNOPT1*INOM_OPT(17)+1),ITAGND,
     5           LSUBMODEL,UNITAB)
      CALL TRACE_OUT1()
C--------------------------------------------
C     OPTIMIZATION (Part 3 & 4)
C--------------------------------------------
       IF(ALLOCATED(TAGPRT_FRIC)) CALL MY_DEALLOC(TAGPRT_FRIC)
       CALL MY_ALLOC(TAGPRT_FRIC,NPART,"TAGPRT_FRIC")
       TAGPRT_FRIC(1:NPART) = 0

C--------------------------------------------!
C    FRICTION MODEL :  BUFFER STRUCTURE ALLOCATION, Lectur of option
C--------------------------------------------!
        IORTHFRICMAX = 0
        NPFRICORTH = 0
        IF(NINTERFRIC > 0) THEN

           IF(IDDLEVEL == 0) ALLOCATE(INTBUF_FRIC_TAB(NINTERFRIC), STAT=Stat)

C  1st step : counting number of set of parts in friction models
            CALL MY_ALLOC(TABCOUPLEPARTS_FRIC_TMP,1,"TABCOUPLEPARTS_FRIC_TMP")
            CALL MY_ALLOC(TABCOEF_FRIC_TMP,1,"TABCOEF_FRIC_TMP")
            CALL MY_ALLOC(IFRICORTH_TMP,1,"IFRICORTH_TMP")
            CALL MY_ALLOC(LENGRPF,NPART,"LENGRPF")
            LENGRPF(1:NPART) = 0

            LENG = 0
            DO N=1,NGRPART
               LENG = MAX (LENG,IGRPART(N)%NENTITY)
            ENDDO

            FLAGF = 0
            NSETFRICTOT = 0
            COEFSLEN = 0
            NGRPF = 0
            NSETMAX = 0

            CALL HM_READ_FRICTION_MODELS(
     1        NOM_OPT(LNOPT1*INOM_OPT(29)+1),UNITAB,IGRPART    ,IPART  ,TAGPRT_FRIC,
     2        TABCOUPLEPARTS_FRIC_TMP ,TABCOEF_FRIC_TMP ,INTBUF_FRIC_TAB,NSETFRICTOT ,
     3        FLAGF    ,COEFSLEN      , IORTHFRICMAX    ,IFRICORTH_TMP   ,NGRPF      ,
     4        LENGRPF  ,LENG          , NSETMAX         ,LSUBMODEL )

            CALL MY_DEALLOC(TABCOUPLEPARTS_FRIC_TMP)
            CALL MY_DEALLOC(TABCOEF_FRIC_TMP)
            CALL MY_DEALLOC(IFRICORTH_TMP)

C  2nd step : storing parts ids and coefficients in temperarly tabs  :
            CALL MY_ALLOC(TABCOUPLEPARTS_FRIC_TMP,2*NINTERFRIC*NSETMAX,"TABCOUPLEPARTS_FRIC_TMP")
            COEFSLEN = NINTERFRIC*(2*NSETMAX+1)
            CALL MY_ALLOC(TABCOEF_FRIC_TMP,8*COEFSLEN,"TABCOEF_FRIC_TMP")
            TABCOUPLEPARTS_FRIC_TMP(1:2*NINTERFRIC*NSETMAX) = 0
            TABCOEF_FRIC_TMP(1:8*COEFSLEN) = ZERO

            CALL MY_ALLOC(IFRICORTH_TMP,NINTERFRIC*NSETMAX,"IFRICORTH_TMP")
            IFRICORTH_TMP(1:NINTERFRIC*NSETMAX) = 0

            FLAGF = 1
            NSETFRICTOT = 0
            COEFSLEN = 0
            NSETMAX = 0

            CALL HM_READ_FRICTION_MODELS(
     1        NOM_OPT(LNOPT1*INOM_OPT(29)+1),UNITAB,IGRPART    ,IPART  ,TAGPRT_FRIC,
     2        TABCOUPLEPARTS_FRIC_TMP ,TABCOEF_FRIC_TMP ,INTBUF_FRIC_TAB,NSETFRICTOT ,
     3        FLAGF    ,COEFSLEN    , IORTHFRICMAX    ,IFRICORTH_TMP   ,NGRPF        ,
     4        LENGRPF  ,LENG        , NSETMAX         , LSUBMODEL )

C  3rd step : Tri of tabs

            COEFSLEN = NINTERFRIC*(2*NSETMAX+1)
            CALL MY_ALLOC(NSETINIT,NINTERFRIC,"NSETINIT")
            CALL MY_ALLOC(TABPARTS_FRIC_TMP,2*NINTERFRIC*NSETMAX,"TABPARTS_FRIC_TMP")
            NSETINIT(1:NINTERFRIC) = 0
            TABPARTS_FRIC_TMP (1:2*NINTERFRIC*NSETMAX) = 0

            CALL TRIINTFRIC(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP  ,INTBUF_FRIC_TAB   ,
     .          TABPARTS_FRIC_TMP,NSETFRICTOT,NSETINIT,IORTHFRICMAX,IFRICORTH_TMP,
     .          NSETMAX        )

C   4th step :  ALLOCATION OF NEW BUFFER FOR INTERFACE FRICTION

             IF(IDDLEVEL == 0) CALL INTBUF_FRIC_INI_STARTER(INTBUF_FRIC_TAB )

C    4th step :    Final storing of structures in buffer
             CALL INTBUF_FRIC_COPY(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP,TABPARTS_FRIC_TMP   ,
     .           NSETINIT                ,IFRICORTH_TMP   , INTBUF_FRIC_TAB    )

             CALL MY_DEALLOC(TABCOUPLEPARTS_FRIC_TMP)
             CALL MY_DEALLOC(TABCOEF_FRIC_TMP)
             CALL MY_DEALLOC(TABPARTS_FRIC_TMP)

             CALL MY_DEALLOC(NSETINIT)
             CALL MY_DEALLOC(IFRICORTH_TMP)
             CALL MY_DEALLOC(LENGRPF)

C------/FRICTION/ORIENTATION READING FOR ORTHOTROPIC FRICTION

             IF(IORTHFRICMAX > 0) THEN

                FLAGF = 0
                NPFRICORTH = 0
c               KFRICORIENT = 0
                IF(.NOT.ALLOCATED(PFRICORTH))ALLOCATE(PFRICORTH(NPART),STAT=stat)
                IF(.NOT.ALLOCATED(IREPFORTH))ALLOCATE(IREPFORTH(1),STAT=stat)
                IF(.NOT.ALLOCATED(VFORTH))ALLOCATE(VFORTH(1),STAT=stat)
                IF(.NOT.ALLOCATED(PHIFORTH))ALLOCATE(PHIFORTH(1),STAT=stat)

                PFRICORTH(1:NPART) = 0

                CALL HM_READ_FRICTION_ORIENTATIONS (INTBUF_FRIC_TAB ,
     1                 NPFRICORTH ,IGRPART     ,IPART       ,PFRICORTH  ,
     2                 IREPFORTH  ,ISKWN       ,PHIFORTH    ,VFORTH    ,SKEW    ,
     3                 FLAGF      ,TAGPRT_FRIC ,RTRANS      ,LSUBMODEL ,UNITAB  )

                CALL MY_DEALLOC(IREPFORTH)
                CALL MY_DEALLOC(VFORTH)
                CALL MY_DEALLOC(PHIFORTH)

                CALL MY_ALLOC(IREPFORTH,NPFRICORTH,"IREPFORTH")
                CALL MY_ALLOC(VFORTH,3*NPFRICORTH,"VFORTH")
                CALL MY_ALLOC(PHIFORTH,NPFRICORTH,"PHIFORTH")

                IREPFORTH(1:NPFRICORTH) = 0
                VFORTH(1:3*NPFRICORTH) = ZERO
                PHIFORTH(1:NPFRICORTH) = ZERO

                FLAGF = 1
                CALL HM_READ_FRICTION_ORIENTATIONS (INTBUF_FRIC_TAB ,
     1                 NPFRICORTH ,IGRPART     ,IPART      ,PFRICORTH  ,
     2                 IREPFORTH  ,ISKWN       ,PHIFORTH   ,VFORTH     ,SKEW    ,
     3                 FLAGF      ,TAGPRT_FRIC ,RTRANS     ,LSUBMODEL  ,UNITAB  )
              ENDIF

         ELSEIF(IDDLEVEL == 0) THEN !NINTERFRIC = 0
              allocate(INTBUF_FRIC_TAB(0))
         ENDIF
         IF(.NOT.ALLOCATED(PFRICORTH))ALLOCATE(PFRICORTH(0))
         IF(.NOT.ALLOCATED(IREPFORTH))ALLOCATE(IREPFORTH(1))
         IF(.NOT.ALLOCATED(VFORTH))   ALLOCATE(VFORTH(1))
         IF(.NOT.ALLOCATED(PHIFORTH)) ALLOCATE(PHIFORTH(1))


C--------------------------------------------
C     ALE CONNECTIVITY
C--------------------------------------------
      CALL ALE_CONNECTIVITY%ALE_CONNECTIVITY_INIT()
      IF(ALE_CONNECTIVITY%has_ne_connect) THEN
         CALL ALE_CONNECTIVITY%ALE_COMPUTE_CONNECTIVITY(NUMNOD, NUMELQ, NUMELTG, NUMELS,
     .        NIXQ, NIXTG, NIXS,
     .        IXQ, IXTG, IXS)
      ENDIF
      CALL ALE_CONNECTIVITY%ALE_COMPUTE_EE_CONNECTIVITY(PM,IGEO,
     .     NPROPGI,NUMGEO, NPROPM, NUMMAT , NUMNOD, NUMELQ, NUMELTG, NUMELS, N2D,
     .     IALE  , IEULER, GLOB_THERM%ITHERM, IALELAG,DETONATORS%IS_SHADOWING_REQUIRED,
     .     NIXQ  , NIXTG , NIXS  ,
     .     IXQ   , IXTG  , IXS   )
C--------------------------------------------
      IF(NSUBDOM > 0) THEN
C---------------Deactivation of ALE flags if no more ALE elements in domain -------------C
        IF(ALE_EULER == 0) THEN
          IALE = 0
          IEULER = 0
        ENDIF
      ENDIF
C
C--------------------------------------------
C     shell offset projection  thke could be overwritten by /INI
C--------------------------------------------
C--------------------------------------------
C     check if need offset treatment
C--------------------------------------------
      CALL CHK_SHELL_OFFSET(                                        
     .                      NGROUP,    NPARG,      IPARG,        NPROPG,
     .                      NUMGEO,      GEO,     DEFAULTS%SHELL%IOFFSET)
      IF (DEFAULTS%SHELL%IOFFSET>0) THEN

        IF (IDDLEVEL == 0) THEN 
          NULLIFY(X_C) 
          IF (DEFAULTS%SHELL%IOFFSET==1) THEN
            CALL MY_ALLOC(ITAGOSET,NUMELC+NUMELTG,"ITAGOSET")
            CALL MY_ALLOC(XYZ,3*NUMNOD,"XYZ")
          ELSE
            CALL MY_ALLOC(ITAGOSET,0,"ITAGOSET")
          END IF
        END IF

          NULLIFY(X_C) 
          IF (DEFAULTS%SHELL%IOFFSET==1) THEN
            XYZ(1:3*NUMNOD) = X(1:3*NUMNOD)
            X_C=>XYZ
          ELSE
            X_C=>X
          END IF


          ITAGOSET = 0
          CALL SHELL_OFFSETP(
     .                NGROUP,    NPARG,      IPARG,        NPROPG,
     .                NUMGEO,      GEO,     NUMELC,          NIXC,
     .                   IXC,  NUMELTG,      NIXTG,          IXTG,
     .                NUMNOD,      X_C,       THKE,      ITAGOSET,
     .                DEFAULTS%SHELL)
      ELSEIF (IDDLEVEL == 0) THEN
        NULLIFY(X_C) 
        X_C=>X
      END IF
C
C--------------------------------------------
C     Reading interfaces
C--------------------------------------------
      ERR_MSG='INTERFACES'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NINTSTAMP=0
      NMNT   = 0
      INTERFACES%PARAMETERS%ISTIF_DT = 0
C Also initialized in Engine (RDRESA)
      PROBINT=HALF
      INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 0

      IF(NINTER == 0.AND.NINTERFRIC > 0 )THEN
            CALL ANCMSG(MSGID=1593,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1)
      ENDIF
C--------------------------------------------------------------   
      IF(NINTER + NGUIDED_CABLE> 0) WRITE(ISTDO,'(A)')TITRE(38)   
C      
      IF(NINTER > 0)THEN
       IF(IDDLEVEL == 0) THEN
         CALL MY_ALLOC(XFILTR,NINTER,"XFILTR")
         CALL MY_ALLOC(STFAC,NINTER,"STFAC")
         CALL MY_ALLOC(FRIC_P,10*NINTER,"FRIC_P")
         CALL MY_ALLOC(I2RUPT,6*NINTER,"I2RUPT")
         CALL MY_ALLOC(AREASL,NINTER,"AREASL")
         CALL MY_ALLOC(FRIGAP,NPARIR*NINTER,"FRIGAP")
       END IF
       XFILTR = ZERO
       STFAC  = ZERO
       FRIC_P = ZERO
       FRIGAP = ZERO
       I2RUPT = ZERO
       AREASL = ZERO
C
       IF(IDDLEVEL == 0) CALL STARTIME(10,1)
       IF(IDDLEVEL == 1) CALL STARTIME(11,1)
C 
       IF(NINTSUB/=0)THEN
          CALL HM_READ_INTSUB(IGRNOD ,IGRSURF,NOM_OPT(LNOPT1*INOM_OPT(3)+1),IGRSLIN,LSUBMODEL)
       END IF
       IDS = 117
       I = 0
c      CALL ANCNTS(IDS, I)
C
       NSN_MULTI_CONNEC = 0
       CALL MY_ALLOC(T2_NB_CONNEC,NUMNOD,"T2_NB_CONNEC")
       T2_NB_CONNEC(1:NUMNOD) = 0
C------------------------------------------------------------
C     INTERFACE READING
C--------------------------------------------------------------
       NINTER25 = 0
       CALL HM_READ_INTERFACES(
     1           IPARI           ,FRIGAP   ,ITAB       ,ITABM1     ,
     2           IGRNOD          ,IGRSURF  ,IGRSLIN    ,IGRBRIC    ,IGRSH3N                       ,
     3           IGRTRUSS        ,NPC      ,ISKWN      ,XFILTR     ,STFAC                         ,
     4           FRIC_P          ,I2RUPT   ,AREASL     ,UNITAB     ,NOM_OPT(LNOPT1*INOM_OPT(3)+1) ,
     5           DEF_INTER       ,NPC1     ,SENSORS    ,MULTI_FVM  ,NOM_OPT(LNOPT1*INOM_OPT(29)+1),
     6           INTBUF_FRIC_TAB ,LSUBMODEL,TF         ,NPTS       ,NPARI                         ,
     7           KLOADPINTER     ,DGAPINT  ,INTERFACES ,SITAB      ,NPARIR                        ,
     8           SITABM1         ,SISKWN   ,LISKN      ,SNPC       ,SNPC1      ,
     9           GLOB_THERM%ITHERM_FE,GLOB_THERM%INTHEAT)
C
       IF(.NOT. ALLOCATED(ALE_CONNECTIVITY%NALE)) ALLOCATE(ALE_CONNECTIVITY%NALE(0))

       IF(NINTER > 0) THEN 
         CALL MY_ALLOC(LIST_NIN25,NINTER,"LIST_NIN25")
         LIST_NIN25(1:NINTER) = 0
       ENDIF
       IF(NINTER25 >0.AND.NUMELS > 0)  THEN
         CALL MY_ALLOC(FLAG_ELEM_INTER25,NINTER25,NUMELS,"FLAG_ELEM_INTER25")
         FLAG_ELEM_INTER25 (1:NINTER25,1:NUMELS) = 0
       ELSE
         CALL MY_ALLOC(FLAG_ELEM_INTER25,0,0,"FLAG_ELEM_INTER25")
       ENDIF

       CALL LECINT (IPARI       ,NINTER    ,IPM                           ,BUFMAT         ,
     .              NMNT        ,ITAB      ,ITABM1                        ,GEO            ,
     .              PM          ,X         ,IGRNOD                        ,IGRSURF        ,IGRSLIN  ,
     .              NPC         ,PROBINT   ,LAG_NCF                       ,
     .              LAG_NKF     ,LAG_NCL   ,LAG_NKL                       ,LAG_NHF        ,MAXRTM   ,
     .              ISKWN       ,MAXRTMS   ,IGEO                          ,
     .              XFILTR      ,STFAC     ,FRIC_P                        ,FRIGAP         ,
     .              I2RUPT      ,AREASL    ,UNITAB                        ,IXS            ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     .              ITAG        ,IXC       ,IXTG                          ,KNOD2ELC       ,KNOD2ELTG,
     .              NOD2ELC     ,NOD2ELTG  ,KNOD2ELS                      ,NOD2ELS        ,IXS10   ,
     .              IXS16       ,IXS20     ,DEF_INTER                     ,MAXNSNE        ,
     .              NPC1        ,MULTI_FVM ,NOM_OPT(LNOPT1*INOM_OPT(29)+1),INTBUF_FRIC_TAB,            
     .              IGRBRIC     ,IGRSH3N   ,IGRTRUSS                      ,MAXRTM_T2      ,NSN_MULTI_CONNEC,
     .              T2_NB_CONNEC,IDDLEVEL  ,ALE_CONNECTIVITY%NALE         ,INTERFACES     ,SNPC1   ,
     .              FLAG_ELEM_INTER25      ,LIST_NIN25)

       !need to allocate only once at first passage in lectur
       FLAG_ALLOCATE = 1
       !PROC argument is used only for call in ddsplit
       PROC_BID = 0

       IF(IDDLEVEL == 0) THEN
         !--------------------------------------------!
         !   NEW INTERFACE BUFFER STRUCTURE ALLOCATION
         !--------------------------------------------!
         allocate(INTBUF_TAB(NINTER))
         !--------------------------------------------!

         !--------------------------------------------!
         !NEW INTERFACE BUFFER STRUCTURE INITIALIZATION
         !--------------------------------------------!
         CALL INTBUF_INI_STARTER(INTBUF_TAB, IPARI, NUMNOD,
     .                           I11FLAG, FLAG_ALLOCATE, PROC_BID ,INTBUF_FRIC_TAB)
         !--------------------------------------------!

         CALL INT8_INI(INTBUF_TAB,IPARI,NBT8)
         allocate(INTERT8(NSPMD, NBT8))
         DO P = 1,NSPMD
           DO I = 1,NBT8
              allocate(INTERT8(P,I)%BUFFER(NSPMD))
              DO J=1,NSPMD
              INTERT8(P,I)%BUFFER(J)%NBMAIN = -1
              INTERT8(P,I)%BUFFER(J)%NBSECND_TOT = 0
              ENDDO
           ENDDO
         ENDDO
         ! -------------------
         ! allocation of arrays for the interface 18 with law 151
         CALL INT18_LAW151_ALLOC(NPARI,NINTER,NUMNOD,NUMELS,MULTI_FVM,IPARI)
         ! -------------------
       END IF
C
C-----Allocation structures INTSTAMP
C
       IF(NINTSTAMP/=0)THEN
         IF(IDDLEVEL == 0) THEN
           allocate(INTSTAMP(NINTSTAMP))
           CALL INTSTAMP_ZERO(INTSTAMP)
         END IF
       ELSE
         IF(IDDLEVEL == 0) THEN
           allocate(INTSTAMP(0))
         ENDIF
       END IF
C
       CALL LECINS(IPARI    ,ITAB      ,PM       ,IPM                          ,BUFMAT ,
     .             IGRNOD   ,IGRSURF   ,IGRSLIN  ,XFILTR                       ,STFAC  ,
     .             FRIC_P   ,FRIGAP    ,I2RUPT   ,AREASL                       ,LIXINT ,
     .             X        ,NINTER    ,IXS      ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     .             IXC      ,IXTG      ,KNOD2ELC ,KNOD2ELTG                    ,NOD2ELC,
     .             NOD2ELTG ,INTBUF_TAB,KNOD2ELS ,NOD2ELS                      ,IXS10  ,
     .             IXS16    ,IXS20     ,NIGE     ,RIGE                         ,XIGE   ,
     .             VIGE     ,IGRBRIC   ,MULTI_FVM,ALE_CONNECTIVITY%NALE        ,IGEO   ,
     .             INTERFACES,S_NOD2ELS,S_NOD2ELTG,FLAG_ELEM_INTER25           ,LIST_NIN25)
C
cc     DEALLOCATE(XFILTR)
cc     DEALLOCATE(STFAC)
cc     DEALLOCATE(FRIC_P)
C      DEALLOCATE(FRIGAP)
cc     DEALLOCATE(I2RUPT)
cc     DEALLOCATE(AREASL)
C----
c      CALL ANCNTG(IDS, I, J)
       IDS =  60
c      CALL ANCHECK(IDS)
       IF(NINTSUB/=0)THEN
         CALL ININTSUB(
     .          ITAB      ,IGRNOD  ,IGRSURF   ,
     .          IPARI  ,MAXRTM,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     .          INTBUF_TAB,MAXRTMS ,IGRSLIN   ,MAXNSNE)
       ENDIF
C----
       IF(IDDLEVEL == 0) THEN
         allocate(INSCR(NINTER))

       ENDIF

       CALL PRESCRINT(IPARI,INTBUF_TAB,INSCR)

       IF(IDDLEVEL == 0) THEN

         DO I=1,NINTER
           call my_alloc(INSCR(I)%WA, INSCR(I)%SINSCR, "INSCR(I)%WA", stat=stat)
           IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                MSGTYPE=MSGERROR,
     .                           C1='INSCR')
           INSCR(I)%WA = 0
         ENDDO
       ELSE
         DO I=1,NINTER
           INSCR(I)%WA = 0
         ENDDO
       END IF
       CALL SCRINT(IPARI   ,INSCR,  INTBUF_TAB)

      IF(IDDLEVEL == 0) CALL STOPTIME(10,1)
      IF(IDDLEVEL == 1) CALL STOPTIME(11,1)

      WRITE(ISTDO,'(A)')TITRE(68)
C-----
       AUX = MAX( NUMNOD , NUMELT+NUMELP+NUMELR+NUMELTG+NUMELC+100 ,
     .            MAXRTM+100 )
       NS_I7 = 2*NUMNOD + 2002 + 4*AUX
C
       NS_I11 = 2002 + NMNT
       AUX = 2002 + 8*MAXRTMS
       NS_I11 = MAX(NS_I11,AUX)
C
C      Max size for interface type2 - i2buc1
       AUX = MAX( NUMNOD , MAXRTM_T2+100 )
       NS_I2 = 2*NUMNOD + 2002 + 4*AUX
C
       IFIP=MAX(NS_I7,NS_I11,
     .   NUMNOD+2+4*NUMELC+4*NUMELTG+8*NUMELS
     .  +2*NUMELT+2*NUMELP+2*NUMELR)
C-----
       SIWORK = MAX(NS_I7,NS_I11,NUMNOD+2+4*NUMELC+4*NUMELTG+8*NUMELS
     .        + 2*NUMELT+2*NUMELP+2*NUMELR+16*NUMELS10+ 2*(SIXX-1)
     .        + MAXNSNE,NS_I2)
C
       SRWORK = MAX(6000,NUMNOD)
       CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
       CALL MY_ALLOC(RWORK,SRWORK,"RWORK")
       IWORK = 0
       RWORK = ZERO
C-----
       IF(LXINTD>0.AND.NSPMD>1) LIXINT = LIXINT + LXINTD

       INTER_CAND%S_IXINT_2 = LIXINT
       IF(IDDLEVEL==0)THEN
         IF( ALLOCATED(INTER_CAND%IXINT) ) CALL MY_DEALLOC(INTER_CAND%IXINT)
         CALL MY_ALLOC(INTER_CAND%IXINT,INTER_CAND%S_IXINT_1,INTER_CAND%S_IXINT_2,"INTER_CAND%IXINT")
         CALL MY_ALLOC(XTMP,3*NUMNOD,"XTMP")
         XTMP = D(1:3*NUMNOD)
       ENDIF
       IFIXIN = IFIP
       IFIEND = IFIXIN
       NELEMINT = 0
       LIXINT = 0
C
C read /INIBRI/FILL before interfaces stiffness
       IF(.NOT.ALLOCATED(FILLSOL)) ALLOCATE(FILLSOL(NUMELS),STAT=stat)
       IF(STAT/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                      C1='FILLSOL')
       IF(NUMELS/=0) CALL LECFILL(IXS,FILLSOL,UNITAB,LSUBMODEL)

      IF(IDDLEVEL == 0) CALL STARTIME(12,1)
      IF(IDDLEVEL == 1) CALL STARTIME(13,1)

        CALL INT18_LAW151_INIT(MULTI_FVM%S_APPEND_ARRAY,NINTER,NPARI,
     1                         NUMNOD,NUMELS,NGRBRIC,
     2                         MULTI_FVM,IGRBRIC,IPARI,IXS,
     4                         X       ,V       ,MS         ,KINET       ,
     5                         MULTI_FVM%X_APPEND,MULTI_FVM%V_APPEND,MULTI_FVM%MASS_APPEND,MULTI_FVM%KINET_APPEND)

C
C fill interface structure to be used by sorting 
c set INTERCEP for INT7 only to avoid bug with INT20 due to renumbering
c in I20NLG (other interface types still done in SET_INTERCEP)
        CALL FILL_INTERCEP(IPARI,INTBUF_TAB,INTERCEP)
C
        CALL ININTR(IPARI      ,INSCR      ,X_C                          ,V                ,IXS         ,IXQ      ,
     2               IXC       ,PM         ,GEO                          ,ITAB             ,MS          ,
     3               IWORK     ,RWORK      ,IXTG                         ,D                ,IXT         ,
     4               IXP       ,IXR        ,ALE_CONNECTIVITY             ,NELEMINT         ,IDDLEVEL    ,
     5               LIXINT    ,IGRBRIC    ,IWCONT                       ,IWCIN2           ,KNOD2ELS    ,
     7               KNOD2ELC  ,KNOD2ELTG  ,NOD2ELS                      ,NOD2ELC          ,NOD2ELTG    ,
     8               IGRSURF   ,IELEM21    ,SH4TREE                      ,SH3TREE          ,IPART       ,
     9               IPARTC    ,IPARTG     ,THKE                         ,THK_PART         ,NOD2EL1D    ,
     A               KNOD2EL1D ,IXS10      ,INTER_CAND                   ,FRIGAP           ,IXS16       ,
     B               IXS20     ,IPM        ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),IPARTS           ,SISKWN      ,
     C               KXX       ,IXX        ,IGEO                         ,INTERCEP         ,LELX        ,
     D               INTBUF_TAB,FILLSOL    ,STACK                        ,IWORKSH          ,NSNT        ,
     E               NMNT_2    ,KXIG3D     ,IXIG3D                       ,KNOD2ELQ         ,NOD2ELQ     ,
     F               SEGQUADFR ,TAGPRT_FRIC,INTBUF_FRIC_TAB              ,IPARTT           ,
     G               IPARTP    ,IPARTX     ,IPARTR                       ,NSN_MULTI_CONNEC ,T2_NB_CONNEC,
     H               SICODE    ,ICODE      ,ISKEW                        ,MULTI_FVM        ,S_NOD2ELS   ,
     I               SITAB     ,SITABM1    ,FLAG_ELEM_INTER25            ,LIST_NIN25       ,IRESP       ,
     J               NIXX      ,S_NOD2EL1D )
      IF(IDDLEVEL == 0) CALL STOPTIME(12,1)
      IF(IDDLEVEL == 1) CALL STOPTIME(13,1)

      IDEL_SOLID = 0
      DO I=1,NINTER 
         IF(IPARI(NPARI*(I-1)+7)==25.AND.IPARI(NPARI*(I-1)+100)>0) THEN
            IDEL_SOLID = IDEL_SOLID + 1
         ENDIF
      ENDDO
      IF(IDEL_SOLID == 0) INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 0
 
        CALL MY_DEALLOC(FLAG_ELEM_INTER25)
        CALL MY_DEALLOC(LIST_NIN25)
      
        CALL MY_DEALLOC(T2_NB_CONNEC)
        CALL MY_DEALLOC(RWORK)
        CALL MY_DEALLOC(IWORK)

        IF(IDDLEVEL==1)  THEN
          CALL MY_DEALLOC(XFILTR)
          CALL MY_DEALLOC(FRIC_P)
          CALL MY_DEALLOC(FRIGAP)
        END IF
C------------ RBE3 uses IXINT-POUR opt.
        IF(LXINTD>0.AND.NSPMD>1)THEN
         IF(LIXINT+LXINTD > INTER_CAND%S_IXINT_2)THEN
           CALL UPGRADE_IXINT(INTER_CAND,NELEMINT,LXINTD)
         ENDIF

         CALL UPDATE_WEIGHT_RBE3(NELEMINT,LIXINT,SLRBE3,NRBE3L,NRBE3,
     .                           LRBE3,IRBE3,INTER_CAND)
        ENDIF
        CALL TRACE_OUT1()
C-----
        IF(IDDLEVEL==0)THEN
          D(1:3*NUMNOD) = XTMP(1:3*NUMNOD)
          CALL MY_DEALLOC(XTMP)
        END IF
C
      ELSEIF(IDDLEVEL == 0) THEN !NINTER = 0

        SINSCR = 0
        allocate(INTSTAMP(0))

        !--------------------------------------------!
        !   NEW INTERFACE BUFFER STRUCTURE ALLOCATION
        !--------------------------------------------!
        allocate(INTBUF_TAB(0))
        !--------------------------------------------!
C
      ENDIF
C--------------------------------------------------------------
C     Printout for /INTER/GUIDED_CABLE
C--------------------------------------------------------------
      IF ((NGUIDED_CABLE > 0).AND.(IDDLEVEL==0)) THEN
        CALL HM_PRINT_INTER_GUIDED_CABLE(NINTER,NINTSUB,NGRNOD,NGRPART,IGRNOD,IGRPART,
     .                                   NPARI,IPARI,LTITR,LNOPT1,NOM_OPT(LNOPT1*INOM_OPT(4)+1))
      ENDIF

C--------------------------------------------
C     Reading rigid walls
C--------------------------------------------
        ERR_MSG='RIGID WALLS'
        ERR_CATEGORY='RIGID WALLS'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        SNPRW  = NRWALL*NNPRW
        SIWORK = NRWALL*NUMNOD
        SLPRW  = 0
        IF(IDDLEVEL==0) THEN
          CALL MY_ALLOC(NPRW,SNPRW,"NPRW")
        END IF
        CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
        NPRW  = 0
        IWORK = 0
C
        SRWBUF = NRWLP*NRWALL
        SRWSAV = 0
        SRWORK = 3*NUMNOD*NRWALL+SRWBUF
        CALL MY_ALLOC(RWORK,SRWORK,"RWORK")
        RWORK = ZERO
        NRWLAG = 0
        SLN_PEN = 0
c
        IF(NRWALL > 0) THEN
          WRITE(ISTDO,'(A)')TITRE(39)
          CALL READ_RWALL(
     1         RWORK    ,NPRW     ,IWORK    ,SLPRW    ,MS       ,
     2         V        ,ITAB     ,ITABM1   ,X        ,IXS      ,
     3         IXQ      ,NPC1     ,D        ,IGRNOD   ,
     4         SRWBUF   ,IMERGE   ,UNITAB   ,
     5         IKINE1LAG,IDDLEVEL ,LSUBMODEL ,RTRANS  ,
     6         NOM_OPT(LNOPT1*INOM_OPT(5)+1),ITAGND   ,SLN_PEN )
        ENDIF
C

        IF(IDDLEVEL==0) THEN
          CALL MY_ALLOC(LPRW,SLPRW,"LPRW")
          CALL MY_ALLOC(RWSTIF_PEN,SLN_PEN,"RWSTIF_PEN")
          RWSTIF_PEN = ZERO
        END IF
        LPRW = IWORK(1:SLPRW)
        CALL MY_DEALLOC(IWORK)
        IF(IDDLEVEL==0) THEN
          CALL MY_ALLOC(RWBUF,SRWBUF,"RWBUF")
        END IF
        RWBUF = RWORK(1:SRWBUF)
        CALL MY_DEALLOC(RWORK)
        IF(ALLOCATED(RWSAV)) CALL MY_DEALLOC(RWSAV)
        CALL MY_ALLOC(RWSAV,SRWSAV,"RWSAV")
        CALL TRACE_OUT1()
C
C--------------------------------------------
C     Reading added masses
C--------------------------------------------
        ERR_MSG='ADDED MASSES'
        ERR_CATEGORY='ADDED MASSES'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        IF(NODMAS > 0)THEN
           WRITE(ISTDO,'(A)')TITRE(43)
          IF(IDDLEVEL==0) THEN
            allocate(IPMAS(NODMAS))
            IPMAS(1:NODMAS)%NPART = 0
            IPMAS(1:NODMAS)%WEIGHT_FLAG = 0
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                 MSGTYPE=MSGERROR,
     .                                 C1='IPMAS')
          ENDIF
!---
           FLAGG = 0
!---
           CALL HM_READ_ADMAS(
     .                  MS      ,ITABM1   ,IGRNOD   ,UNITAB ,IGRSURF,
     .                  IPART   ,IPMAS    ,TOTADDMAS,FLAGG  ,IGRPART,
     .                  X       ,LSUBMODEL)
!---
           FLAGG = 1
!---
           CALL HM_READ_ADMAS(
     .                  MS      ,ITABM1   ,IGRNOD   ,UNITAB ,IGRSURF,
     .                  IPART   ,IPMAS    ,TOTADDMAS,FLAGG  ,IGRPART,
     .                  X       ,LSUBMODEL)
!---
           IF(NS10E>0) CALL ADDMAST10(ICNDS10,  MS    )
C---
        ELSE
          IF(IDDLEVEL==0) THEN
            allocate(IPMAS(0))
          ENDIF
        ENDIF
        CALL TRACE_OUT1()
C--------------------------------------------
C     Reading rigid structures
C--------------------------------------------
        ERR_MSG='RIGID ENTITIES'
        ERR_CATEGORY='RIGID BODY'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        CALL HM_PREREAD_RBODY    (SLPBY  ,IGRNOD  ,LSUBMODEL)
        CALL PREREAD_RBODY_LAGMUL(SLPBYL ,IGRNOD  ,LSUBMODEL)
        CALL HM_PREREAD_MERGE(SMGRBY, SLPBY, IGRNOD, LSUBMODEL)
        SNPBY   = NNPBY*NRBYKIN
        SNPBYL  = NNPBY*NRBYLAG
        SNRBODY = SNPBY + SNPBYL
        SLRBODY = SLPBY + SLPBYL
        SRBY    = NRBY*NRBODY
        IF(IDDLEVEL==0) THEN
          IF(ALLOCATED(NPBY)) CALL MY_DEALLOC(NPBY)
          IF(ALLOCATED(LPBY)) CALL MY_DEALLOC(LPBY)
          IF(ALLOCATED(RBY)) CALL MY_DEALLOC(RBY)
          CALL MY_ALLOC(NPBY,SNRBODY,"NPBY")
          CALL MY_ALLOC(LPBY,SLRBODY,"LPBY")
          CALL MY_ALLOC(RBY,SRBY,"RBY")
        END IF
        IF(ALLOCATED(MGRBY)) CALL MY_DEALLOC(MGRBY)
        IF(NRBMERGE > 0) THEN
          CALL MY_ALLOC(MGRBY,NMGRBY*SMGRBY,"MGRBY")
        ELSE
          CALL MY_ALLOC(MGRBY,0,"MGRBY")
        ENDIF
        NPBY  = 0
        LPBY  = 0
        MGRBY = 0
        RBY   = ZERO
        IF(SNPBY<SNRBODY) THEN
          NPBYL => NPBY(SNPBY+1:SNRBODY)
        ELSE
          NPBYL => NPBY
        END IF
        IF(SLPBY<SLRBODY) THEN
        LPBYL => LPBY(SLPBY+1:SLRBODY)
        ELSE
          LPBYL => LPBY
        END IF
        IF(NRBY *NRBYKIN<SRBY) THEN
          RBYL  => RBY(NRBY *NRBYKIN+1:SRBY)
        ELSE
          RBYL  => RBY
        END IF
C
        IF(NRBODY  > 0) WRITE(ISTDO,'(A)')TITRE(41)
        IF(NRBYKIN > 0) THEN
            CALL HM_READ_RBODY(
     1       RBY       ,NPBY    ,LPBY    ,ITAB    ,ITABM1    ,
     2       IGRNOD    ,IGRSURF ,IBFV    ,IGRV    ,LGRAV     ,
     3       SENSORS   ,IMERGE  ,UNITAB  ,ISKWN   ,NOM_OPT   ,
     4       SLRBODY   ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,KNOD2EL1D,
     5       KNOD2ELQ  ,ITAGND  ,ICNDS10 ,LSUBMODEL,ICFIELD  ,
     6       LCFIELD   ,SLRBODY ,N_NRBODY,NRBODY_TO_PART,IPART)
        ENDIF
C--------------------------------------------
C     Reading rigid body merges
C--------------------------------------------
      IF(NRBMERGE > 0) THEN
        CALL HM_READ_MERGE(
     .      MGRBY,SMGRBY ,NPBY,LPBY   ,SLRBODY,
     .      RBY  ,NOM_OPT,INOM_OPT(30),IGRNOD ,
     .      ITAB ,ITABM1 ,LGRAV       ,IGRV   ,
     .      LSUBMODEL,IPART)
      ENDIF
C--------------------------------------------
C     rigid body hierarchy
C--------------------------------------------
      IF(NRBYKIN > 0) THEN
        call hierarchy_rbody(NRBYKIN ,NNPBY ,NPBY  ,SLRBODY ,LPBY  ,
     .                       NRBY    ,RBY   ,NUMNOD,IOUT    ,LNOPT1,
     .                       NOM_OPT )
      ENDIF
C--------------------------------------------
C     Checking rigid structures
C--------------------------------------------
      IF(NRBYKIN > 0) THEN
        CALL CHECKRBY(
     1       RBY    ,NPBY    ,LPBY   ,ITAB   ,
     2       D      ,IDDLEVEL,NOM_OPT,SLRBODY,
     3       ROOTNAM,ROOTLEN )
      ENDIF
C--------------------------------------------
C     Checking rigid part
C--------------------------------------------
      IF(N_NRBODY > 0) THEN
        CALL RBODY_PART_CHECK(NRBYKIN ,NNPBY ,NPBY  ,SLRBODY ,LPBY  ,       
     .                        NUMNOD  ,ITAB  ,NPART ,IPART   ,LIPART1,       
     .                        ICODE   ,ISKEW ,NFXVEL,NIFV    ,IBFV ,
     .                        NGRAV   ,NIGRV ,IGRV  ,SLGRAV  ,LGRAV,
     .                        NUMSKW  ,X  ) 
        CALL RPART_INIVEL_CHECK(NRBYKIN ,NNPBY ,NPBY  ,SLPBY    ,LPBY   ,        
     .                        HM_NINVEL ,NUMNOD,ITABM1,LSUBMODEL,UNITAB ,        
     .                        NTRANSF   ,NRTRANS,RTRANS,RBY_INIAXIS,ISKWN,     
     .                         LISKN    ,NUMSKW ,LSKEW  ,SKEW   ,IFRAME  ,        
     .                         NUMFRAM  ,NXFRAME,XFRAME ,NGRNOD ,IGRNOD  , 
     .                         N2D      ,X      ,V     ,VR  )

      END IF
C--------------------------------------------
C     READING RB LAGRANGE
C--------------------------------------------
      IF(NRBYLAG > 0) THEN
         CALL HM_READ_RBODY_LAGMUL(RBYL    ,NPBYL   ,LPBYL   ,IGRNOD   ,LSUBMODEL ,
     .                             ITAB    ,ITABM1  ,D       ,IKINE1LAG,NOM_OPT)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
      IF(NINTER > 0) THEN
C--------------------------------------------
C
        IF(NINTSTAMP/=0)THEN
          ERR_MSG='INTERFACES TYPE21'
          ERR_CATEGORY='INTERFACES'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c          IF(IDDLEVEL==0)THEN
            CALL LECSTAMP(IPARI  ,INTSTAMP ,UNITAB, NPBY,
     .                    ICODE  ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),LSUBMODEL)
c          ENDIF
          CALL TRACE_OUT1()
        END IF
C--------------------------------------------
C OPTIMISATION INTERFACE SPMD
C--------------------------------------------
        ERR_CATEGORY='INTERNAL'
        IF(IDDLEVEL==0)THEN
          IF(IALE+IEULER/=0)
     +       CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
          IF(NUMELS/=0)
     +       CALL LCE16S4(IXS    ,PM     ,ICODE  )
C
          LAG_NCF = LAG_NCF0
          LAG_NKF = LAG_NKF0
          LAG_NHF = LAG_NHF0
          LAG_NCL = LAG_NCL0
          LAG_NKL = LAG_NKL0
        ENDIF
      ENDIF

C--------------------------------------------
C
C     After IDDLEVEL - we do not enter inintr anymore
C     set I11FLAG to 1
      I11FLAG = 1

      CALL MY_DEALLOC(IELEM21)
C
C---------------------------------
      IF((SEANI > 0).AND.(IDDLEVEL==1))  EANI = 0
C---------------------------------
#ifdef DNC
C--------------------------------------------
C     READING AND PREPARATION OF FINITE ELEMENTS TO SEND TO MADYMO:
C     "EXTENDED COUPLING".
C--------------------------------------------
      ERR_MSG='MADYMO INTERFACED FEM'
      ERR_CATEGORY='MADYMO INTERFACED FEM'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NEXMAD/=0) THEN
        SIWORK2 = MAX(NPART,NUMNOD,2*NUMNOD+NUMELC+NUMELTG+NUMELS)
        SIWORK  = NPART+NUMNOD+2*(NUMELC+NUMELTG+NUMELS)

        CALL MY_ALLOC(IWORK,SIWORK+SIWORK2,"IWORK")
        IWORK  = 0
        IF(SIWORK<SIWORK+SIWORK2) THEN
        IWORK2 => IWORK(SIWORK+1:SIWORK+SIWORK2)
        ELSE
          IWORK2 => IWORK
        ENDIF
      ENDIF
      IF(ALLOCATED(ICONX)) CALL MY_DEALLOC(ICONX)
      IF(NEXMAD/=0) THEN
       WRITE(ISTDO,'(A)')' .. FEM INTERFACED TO MADYMO'
        CALL HM_READ_MADYMO_EXFEM(IWORK(7*NCONX+1),ITAB ,ITABM1   ,IPART   ,IPARTC,
     .               IPARTG   ,IPARTS   ,IXC      ,IXTG    ,IXS     ,
     .               IWORK2   ,GEO      ,PM       ,IWORK   ,IGEO    ,
     .               IPM      ,LSUBMODEL)

        SMADPRT = NMADPRT
        SMADSH4 = NMADSH4
        SMADSH3 = NMADSH3
        SMADSOL = NMADSOL
        SMADNOD = NMADNOD

        SMADFAIL= NUMELC+NUMELTG+NUMELS

        SIEXTAG  = 2*NMADNOD+NMADSH4+NMADSH3+NMADSOL

        SIEXMAD  =  NMADPRT+NMADSH4+NMADSH3+NMADSOL+NMADNOD
     .              + NUMELC+NUMELTG+NUMELS

        SICONX =   7*NCONX+SIEXMAD+SIEXTAG
        CALL MY_ALLOC(ICONX,SICONX,"ICONX")
        ICONX(1:SICONX) = 0
        DO I=1,7*NCONX+SIEXMAD
            ICONX(I) = IWORK(I)
        ENDDO
      ELSE
        SICONX  = 7*NCONX
        CALL MY_ALLOC(ICONX,SICONX,"ICONX")
        IF(SICONX > 0)THEN
          ICONX(1:7*NCONX)=IWORK(1:7*NCONX)
        ENDIF
      ENDIF
      IF(ALLOCATED(IWORK)) CALL MY_DEALLOC(IWORK)
      CALL TRACE_OUT1()
#else
      IF(.NOT. ALLOCATED(ICONX)) ALLOCATE(ICONX(0))
#endif
C---------------------------------------------
C     Reading flexible bodies
C---------------------------------------------
      ERR_MSG='FLEXIBLE BODIES'
      ERR_CATEGORY='FLEXIBLE BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C

      IF(NFXBODY == 0) THEN
         IF(IDDLEVEL==0) THEN
           call my_alloc(FXBNOD, 0, "FXBNOD")
           call my_alloc(FXB_MATRIX, 0, "FXB_MATRIX")
           call my_alloc(FXB_MATRIX_ADD, 4, 0, "FXB_MATRIX_ADD")
           call my_alloc(FXBGLM, 0, "FXBGLM")
           call my_alloc(FXBCPM, 0, "FXBCPM")
           call my_alloc(FXBRPM, 0, "FXBRPM")
           call my_alloc(FXBCPS, 0, "FXBCPS")
           call my_alloc(FXBLM, 0, "FXBLM")
           call my_alloc(FXBFLS, 0, "FXBFLS")
           call my_alloc(FXBDLS, 0, "FXBDLS")
           call my_alloc(FXBDEP, 0, "FXBDEP")
           call my_alloc(FXBVIT, 0, "FXBVIT")
           call my_alloc(FXBACC, 0, "FXBACC")
           call my_alloc(FXBMOD, 0, "FXBMOD")
           call my_alloc(FXBELM, 0, "FXBELM")
           call my_alloc(FXBSIG, 0, "FXBSIG")
           call my_alloc(FXBGRVI, 0, "FXBGRVI")
           call my_alloc(FXBGRVR, 0, "FXBGRVR")
         ENDIF
      ELSE IF(NFXBODY>0) THEN
C
         IF(IDDLEVEL==1) THEN
C--      length must be reset for second pass
           LENGLM=0
           LENCP=0
           LENLM=0
           LENFLS=0
           LENDLS=0
           LENVAR=0
           LENRPM=0
           LENMCD=0
           LENELM=0
           LENSIG=0
           LENGRVI=0
           LENGRVR=0
         ENDIF
C
         IF(IDDLEVEL==0) THEN
           INQUIRE(IOLENGTH=RCLEN) FLREC6
           OPEN(UNIT=IFXM,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
           OPEN(UNIT=IFXS,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
           WRITE(ISTDO,'(A)')TITRE(51)
           call my_alloc(FXBNOD, LENNOD, "FXBNOD")
           call my_alloc(FXB_MATRIX, LENMAT, "FXB_MATRIX")
           call my_alloc(FXB_MATRIX_ADD, 4, LENMAT, "FXB_MATRIX_ADD")
         ENDIF
C
         CALL HM_READ_FXB1(NOM_OPT(LNOPT1*INOM_OPT(11)+1),FXBNOD,FXBIPM,FXB_MATRIX,FXB_MATRIX_ADD,
     .                     NMANIM,ITAB,ITABM1,FXBFILE_TAB,LSUBMODEL)
C
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NBNO=FXBIPM(AIPM+3)
            NBMO=FXBIPM(AIPM+4)+FXBIPM(AIPM+17)
            FXBIPM(AIPM+19)=LENELM+1
            FXBIPM(AIPM+20)=LENSIG+1
            FXBIPM(AIPM+26)=LENGRVI+1
            FXBIPM(AIPM+27)=LENGRVR+1
C
            CALL FXBTAGN(
     .      FXBNOD(ANOD), NBNO,    FXBIPM(AIPM+18), IBCL   ,      IPRES   ,
     .      IXS    ,      IXC    , IXT    ,         IXP    ,      IXR    ,
     .      IXTG   ,      IPARG  , FXBTAG,          NBMO,     FXBIPM(AIPM+4),
     .      NELS   ,      NELC,    NELTG,           IGRV   ,LGRAV  ,
     .      NLGRAV ,      IPARI  , INTBUF_TAB     , FXBIPM(AIPM+29), NELT,
     .      NELP)
            FXBIPM(AIPM+21)=NELS
            FXBIPM(AIPM+22)=NELC
            FXBIPM(AIPM+23)=NELTG
            FXBIPM(AIPM+34)=NELT
            FXBIPM(AIPM+35)=NELP
            FXBIPM(AIPM+24)=0
            FXBIPM(AIPM+25)=NLGRAV
         ENDDO
C
         IF(IDDLEVEL==0) THEN
           call my_alloc(FXBGLM, LENGLM, "FXBGLM")
           call my_alloc(FXBCPM, LENCP, "FXBCPM")
           call my_alloc(FXBRPM, LENRPM, "FXBRPM")
           call my_alloc(FXBCPS, LENCP, "FXBCPS")
           call my_alloc(FXBLM, LENLM, "FXBLM")
           call my_alloc(FXBFLS, LENFLS, "FXBFLS")
           call my_alloc(FXBDLS, LENDLS, "FXBDLS")
           call my_alloc(FXBDEP, LENVAR, "FXBDEP")
           call my_alloc(FXBVIT, LENVAR, "FXBVIT")
           call my_alloc(FXBACC, LENVAR, "FXBACC")
           call my_alloc(FXBMOD, LENMOD*6, "FXBMOD")
           call my_alloc(FXBELM, LENELM, "FXBELM")
           call my_alloc(FXBSIG, LENSIG, "FXBSIG")
           call my_alloc(FXBGRVI, LENGRVI, "FXBGRVI")
           call my_alloc(FXBGRVR, LENGRVR, "FXBGRVR")
         ENDIF
C
         FXBELM(1:LENELM)= 0
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NBNO=FXBIPM(AIPM+3)
            ALM=FXBIPM(AIPM+19)
            IF(FXBIPM(AIPM+4)>0) CALL FXBELNUM(
     .              FXBNOD(ANOD), NBNO,    IPARG  , FXBTAG, FXBELM(ALM),
     .              IXS    ,      IXC    , IXTG   , IPARTS ,IPARTC   ,
     .              IPARTG  ,     IXT    , IXP    , IPARTT  ,IPARTP    )
         ENDDO
C
         CALL HM_READ_FXB2(FXBIPM,  FXBRPM, FXBNOD, FXBGLM,
     .                FXBCPM,   FXBCPS,  FXBLM,  FXBFLS, FXBDLS,
     .                FXBMOD, ITAB    , ITABM1 , NOM_OPT(LNOPT1*INOM_OPT(11)+1),FXB_LAST_ADDRESS,
     .                LSUBMODEL)
C
C
      ELSEIF(IDDLEVEL==0) THEN
         call my_alloc(FXBNOD, 0, "FXBNOD")
         call my_alloc(FXBMOD, 0, "FXBMOD")
         call my_alloc(FXBGLM, 0, "FXBGLM")
         call my_alloc(FXBGRVI, 0, "FXBGRVI")
         call my_alloc(FXBCPM, 0, "FXBCPM")
         call my_alloc(FXBCPS, 0, "FXBCPS")
         call my_alloc(FXBLM, 0, "FXBLM")
         call my_alloc(FXBFLS, 0, "FXBFLS")
         call my_alloc(FXBDLS, 0, "FXBDLS")
         call my_alloc(FXBDEP, 0, "FXBDEP")
         call my_alloc(FXBVIT, 0, "FXBVIT")
         call my_alloc(FXBACC, 0, "FXBACC")
         call my_alloc(FXBRPM, 0, "FXBRPM")
         call my_alloc(FXBELM, 0, "FXBELM")
         call my_alloc(FXBSIG, 0, "FXBSIG")
         call my_alloc(FXBGRVR, 0, "FXBGRVR")
      ENDIF
C
      CALL TRACE_OUT1()
      ERR_MSG='EIGEN MODES'
      ERR_CATEGORY='EIGEN MODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NEIG>0) THEN
C
         INQUIRE(IOLENGTH=RCLEN) FLREC6
         OPEN(UNIT=IEIGM,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
C
         WRITE(ISTDO,'(A)')TITRE(52)
         CALL HM_PREREAD_EIG(IGRNOD   ,NNT   ,LSUBMODEL)
         NEIPM=17
         NERPM=4
         LEIBUF = NNT
         IF(IDDLEVEL==0) THEN
           call my_alloc(EIGIPM, NEIPM*NEIG, "EIGIPM")
           call my_alloc(EIGIBUF, NNT, "EIGIBUF")
           CALL MY_ALLOC(EIGRPM,NERPM*NEIG,"EIGRPM")
           EIGIPM = 0
           EIGIBUF = 0
           EIGRPM = ZERO
         ENDIF
C
         CALL HM_READ_EIG(EIGIPM, EIGIBUF, EIGRPM, IGRNOD   ,ITABM1   ,
     .                       UNITAB, LSUBMODEL)
      ELSEIF(IDDLEVEL==0) THEN
         call my_alloc(EIGIPM, 0, "EIGIPM")
         call my_alloc(EIGIBUF, 0, "EIGIBUF")
         CALL MY_ALLOC(EIGRPM,0,"EIGRPM")
      ENDIF
      CALL TRACE_OUT1()
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NDSOLV==1) THEN
        WRITE(6,*) "ERROR Deprecated Linear solver"
        CALL ARRET(5)
      ELSEIF(IDDLEVEL==0) THEN
         NSLEVEL=0
         call my_alloc(CEPTMP, 0, "CEPTMP")
         call my_alloc(NELDOM, 0, "NELDOM")
         call my_alloc(ELDOM, 0, 0, 0, "ELDOM")
         call my_alloc(ELSUB, 0, 0, "ELSUB")
      ENDIF
      CALL TRACE_OUT1()
C
C  shell composite xfem
C
      ERR_MSG='COMPOSITE SHELLS'
      ERR_CATEGORY='COMPOSITE SHELLS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(IDDLEVEL==0) THEN
       IF(IPLYXFEM > 0) THEN
         CALL MY_ALLOC(MS_PLY0,NUMNOD*NPLYMAX,"MS_PLY0")
         MS_PLY0=ZERO
         CALL MY_ALLOC(ZI_PLY0,NUMNOD*NPLYMAX,"ZI_PLY0")
         ZI_PLY0=ZERO
         CALL MY_ALLOC(MSZ20,NUMNOD,"MSZ20")
         MSZ20=ZERO
         CALL MY_ALLOC(ITAGND_SHXFEM,NUMNOD,"ITAGND_SHXFEM")
         ITAGND_SHXFEM=0
         CALL MY_ALLOC(ITAGSH,NUMELC,"ITAGSH")
         ITAGSH=0
         CALL MY_ALLOC(INOD_PXFEM,NUMNOD,"INOD_PXFEM")
         INOD_PXFEM=0
         CALL MY_ALLOC(IEL_PXFEM,NUMELC,"IEL_PXFEM")
         IEL_PXFEM=0
       ELSE
          call my_alloc(MS_PLY0, 0, "MS_PLY0")
          call my_alloc(ZI_PLY0, 0, "ZI_PLY0")
          call my_alloc(ITAGND_SHXFEM, 0, "ITAGND_SHXFEM")
          call my_alloc(ITAGSH, 0, "ITAGSH")
          call my_alloc(INOD_PXFEM, 0, "INOD_PXFEM")
          call my_alloc(IEL_PXFEM, 0, "IEL_PXFEM")
          call my_alloc(MS_PLY, 0, "MS_PLY")
          call my_alloc(ZI_PLY, 0, "ZI_PLY")
          call my_alloc(MSZ20, 0, "MSZ20")
       ENDIF
      ENDIF
C
      CALL TRACE_OUT1()
C
C-----------------------------------------------------------
C
      ERR_MSG='ARRAYS ALLOCATION FOR INTIA'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C     tab masse
      IF(IDDLEVEL == 0) THEN
        CALL MY_ALLOC(MSC,NUMELC,"MSC")
        CALL MY_ALLOC(MSTG,NUMELTG,"MSTG")
        CALL MY_ALLOC(INC,NUMELC,"INC")
        CALL MY_ALLOC(INTG,NUMELTG,"INTG")
        CALL MY_ALLOC(PTG,3,NUMELTG,"PTG")
        IF(GLOB_THERM%ITHERM_FE > 0)THEN
          CALL MY_ALLOC(MCPC,NUMELC,"MCPC")
          CALL MY_ALLOC(MCPTG,NUMELTG,"MCPTG")
        ELSE
          call my_alloc(MCPC, 0, "MCPC")
          call my_alloc(MCPTG, 0, "MCPTG")
        END IF
      ENDIF
C
      MSC   = ZERO
      MSTG  = ZERO
      INC   = ZERO
      INTG  = ZERO
      PTG   = ZERO
      MCPC  = ZERO
      MCPTG = ZERO
C
C-------------------------------------------------------
C
      IF(IDDLEVEL == 0) THEN
        IF(IREST_MSELT/=0)THEN
          CALL MY_ALLOC(MSSA,NUMELS,"MSSA")
          CALL MY_ALLOC(MSRT,NUMELR,"MSRT")
        ELSE
          CALL MY_ALLOC(MSSA,0,"MSSA")
          CALL MY_ALLOC(MSRT,0,"MSRT")
        ENDIF
C-------------------------------------------------------
        IF(I7STIFS/=0)THEN
          CALL MY_ALLOC(STIFINT,NUMNOD+NUMFAKENODIGEO,"STIFINT")
          CALL MY_ALLOC(STIFINTR,NUMNOD,"STIFINTR")
        ELSE
          CALL MY_ALLOC(STIFINT,0,"STIFINT")
          CALL MY_ALLOC(STIFINTR,0,"STIFINTR")
        ENDIF
C-------------------------------------------------------
        IF(IRIGID_MAT > 0) THEN
           call my_alloc(SLNRBM, NUMNOD, "SLNRBM")
           call my_alloc(NSLNRBM, NUMNOD, "NSLNRBM")
           call my_alloc(RMSTIFN, NUMNOD, "RMSTIFN")
           call my_alloc(RMSTIFR, NUMNOD, "RMSTIFR")
         ELSE
           call my_alloc(SLNRBM, 0, "SLNRBM")
           call my_alloc(NSLNRBM, 0, "NSLNRBM")
           call my_alloc(RMSTIFN, 0, "RMSTIFN")
           call my_alloc(RMSTIFR, 0, "RMSTIFR")
           CALL MY_ALLOC(FRONT_RM,0,"FRONT_RM")
        ENDIF
C-------------------------------------------------------
        call my_alloc(FXANI, 2, NMANIM, "FXANI")
        call my_alloc(MBUFEL, LBUFEL, NMANIM, "MBUFEL")
        call my_alloc(MDEPL, 3*NUMNOD, NMANIM, "MDEPL")
        CALL MY_ALLOC(STIFFN,NUMNOD*2,"STIFFN")
      ENDIF
C
      STIFINT = ZERO
      STIFINTR = ZERO
      SLNRBM= 0
      NSLNRBM=0
      IF(NUMNOD  > 0) STIFFN  = EM20
C
      CALL TRACE_OUT1()
C--------------------------------------------
      ! still need for *Y00, *sty files - not yet covered by CFG files (hm_reader)
      IF(IDDLEVEL == 0) CALL YCTRL(IGRBRIC)
!
      IF (IDDLEVEL == 0) CALL HM_YCTRL(UNITAB,LSUBMODEL,IGRBRIC,IXC,IXTG, PTSHEL,PTSH3N,NUSPHCEL)
C
C----------------------------------------------------------
C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  The following is executed with iddlevel=0 only for AMS with automatic element selection
C  or if no contact interfaces and no ams
      IF((IDDLEVEL == 1).OR.(ISMS_SELEC >= 3).OR.((NINTER == 0).AND.(ISMS == 0))) THEN
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
C--------------------------------------------
C     ELEMENT BUFFER INTIALIZATION
C--------------------------------------------
      ERR_MSG='ELEMENT BUFFER INITIALIZATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      WRITE(ISTDO,'(A)')TITRE(45)
      NUMEL=2*(NUMELC+NUMELQ+NUMELT+NUMELS+NUMELP+NUMELR+
     &         NUMELTG+NUMELX+NUMSPH+NUMELIG3D)
C
C--------------------------------------------
C     NEW ELEMENT BUFFER STRUCTURE ALLOCATION
C--------------------------------------------
c
      FLAG_XFEM = 0
      allocate(ELBUF_TAB(NGROUP))
C
      CALL ELBUF_INI(ELBUF_TAB,MAT_ELEM%MAT_PARAM,
     .               MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .               IGEO     ,IPM      ,IPARG      ,IPART     ,
     .               IPARTSP  ,IXS      ,IXQ      ,IXC        ,IXTG      ,
     .               FLAG_XFEM,IPARTIG3D,STACK    ,IGEO_STACK ,
     .               IXT      ,IXP      ,IXR      ,KXX        ,GEO       ,
     .               EOS_TAG  ,ISTR_24  ,IPRI     ,DEFAULTS)
C---
C  if xfem
c---
      IF(ICRACK3D > 0) THEN
        FLAG_XFEM = 1

        IF(ALLOCATED(XFEM_TAB)) DEALLOCATE(XFEM_TAB)
        allocate(XFEM_TAB(NGROUP, NXEL))
c
        DO IXEL=1,NXEL
          CALL ELBUF_INI(XFEM_TAB(1:NGROUP,IXEL),MAT_ELEM%MAT_PARAM,
     .         MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .         IGEO                   ,IPM      ,IPARG    ,IPART     ,
     .         IPARTSP                ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .         FLAG_XFEM              ,IPARTIG3D,STACK    ,IGEO_STACK,
     .         IXT                    ,IXP      ,IXR      ,KXX      ,GEO       ,
     .         EOS_TAG                ,ISTR_24  ,IPRI     ,DEFAULTS)
        ENDDO
      ELSE
        IF(ALLOCATED(XFEM_TAB)) DEALLOCATE(XFEM_TAB)
        allocate(XFEM_TAB(0, 0))
      ENDIF
C--------------------------------------------
C     WARNING FOR PTHICKFAIL
C--------------------------------------------
      CALL CHECK_PTHICKFAIL(ELBUF_TAB,MAT_ELEM%MAT_PARAM ,IPARG    ,GEO      ,
     .                      IPM      ,STACK    ,IGEO     ,NUMMAT   ,NUMGEO   ,
     .                      NGROUP   ,NPARG    ,NPROPMI  ,NPROPGI  ,NPROPG   )
C--------------------------------------------
C     CHECK MATERIAL / PROPERTY COMPATIBILITY
C--------------------------------------------
      CALL CHECK_MAT_ELEM_PROP_COMPATIBILITY(
     .    ELBUF_TAB,IPARG    ,IPM      ,IGEO     ,NUMMAT   ,NUMGEO   ,
     .    NGROUP   ,NPARG    ,NPROPMI  ,NPROPGI  ,MAT_ELEM%MAT_PARAM ,
     .    N2D      ,IXT      ,NUMELT   ,IXP      ,NUMELP   ,IXR      ,
     .    NUMELR   ,KXX      ,NUMELX   )
C
C--------------------------------------------
C     CHECK COMPATIBILITY /DTTSH
C--------------------------------------------
      IF(NUMELS>0) THEN
       CALL CHK_DTTSH(ELBUF_TAB,IXS ,IPARG ,D   )
      END IF
!-------ini of shell offset treatment      
      IF (DEFAULTS%SHELL%IOFFSET>0) THEN
        IF (DEFAULTS%SHELL%IOFFSET==1) CALL INTER_OFFSET_ITAG(                                 
     .                 NINTER,    IPARI,      NPARI,       IGRSURF,
     .                  NSURF,   NUMELC,    NUMELTG,      ITAGOSET)
        CALL SHELL_OFFSET_INI(
     .                NGROUP,    NPARG,      IPARG,        NPROPG,
     .                NUMGEO,      GEO,     NUMELC,       NUMELTG,       
     .               NPROPGI,     IGEO,   ITAGOSET,       ELBUF_TAB,
     .               DEFAULTS%SHELL   )
      END IF 
C-----------------------------------------------------------
C     ALLOCATION OF ARRAYS FOR INITIA - DEALLOCATED AFTER INTIA
C-----------------------------------------------------------
C
      NSIGS   =11
      LSIGSH  = 0
      LSIGSP  = 0
      LSIGSPH = 0
      LSIGI   = MAX (NUMELS+NUMELQ,NUMSOL+NUMQUAD)
      LSIGRS  = 0
      LSIGBEAM = 0
      LSIGTRUSS = 0
      NSIGI = 0
      NSIGSPH= 12
      NSIGSH = 0
      NSIGRS = 0
      NSIGBEAM = 0
      NSIGTRUSS = 0
      IF(ISIGI/=0)THEN
        NSIGSH = NVSHELL
        NSIGRS = NVSPRI  
        NSIGBEAM = NVBEAM
        NSIGTRUSS = NVTRUSS
        IF(NUBEAM > 0) NSIGBEAM = NSIGBEAM + NUBEAM
        IF(IUSHELL/=0) NSIGSH = NSIGSH + NUSHELL
        IF(IORTSHEL/=0) NSIGSH = NSIGSH + NORTSHEL
        IF(NVSHELL1/=0)NSIGSH = NSIGSH + NVSHELL1
        IF(NVSHELL2 /= 0)NSIGSH = NSIGSH + NVSHELL2 + 3
        IF(NUSPHCEL /= 0)NSIGSPH = NSIGSPH + NUSPHCEL
        NSIGI= NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID + 4 + NVSOLID4 +
     .         NVSOLID5 + NVSOLID6 + 7


        IF(IABS(ISIGI) == 3 .OR. IABS(ISIGI) == 4 .OR.
     .      IABS(ISIGI) == 5) THEN
          LSIGSH  = NUMSHEL+NUMSH3N
          LSIGSP  = MAX(NUMSOL+NUMQUAD,NUMELS+NUMELQ)
          LSIGSPH = NUMSPHY
          LSIGRS = NUMSPRI
          LSIGBEAM = NUMBEAM
          LSIGTRUSS = NUMTRUS
        ELSE
          LSIGSH  = NUMELC+NUMELTG
          LSIGSP  = NUMELS+NUMELQ
          LSIGSPH = NUMSPH
          LSIGRS  = NUMELR
          LSIGBEAM = NUMELP
          LSIGTRUSS = NUMELT
        END IF
      END IF
C
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
         IMAX  = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,
     .               NUMELTG,NUMSOL,NUMQUAD,NUMSHEL,NUMSH3N,
     .               NUMSPHY,NUMSPRI,NUMBEAM,NUMTRUS)
      ELSE
         IMAX  = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,
     .               NUMELTG,NUMELIG3D)
      END IF
      SINDEX = 2*IMAX
      SITRI  = IMAX
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
        JMAX=MAX(NUMELC,NUMSOL,NUMQUAD,NUMSHEL,NUMSH3N,NUMSPHY,
     .           NUMELR,NUMSPRI,NUMELP,NUMBEAM,NUMTRUS)
      ELSE
        JMAX=0
      END IF
C
C------- refsta
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
        call my_alloc(PTSOL, NUMELS, "PTSOL", stat=stat)
        call my_alloc(PTQUAD, NUMELQ, "PTQUAD", stat=stat)
        call my_alloc(PTSPH, NUMSPH, "PTSPH", stat=stat)
        call my_alloc(PTSPRI, NUMELR, "PTSPRI", stat=stat)
        call my_alloc(PTBEAM, NUMELP, "PTBEAM", stat=stat)
        call my_alloc(PTTRUSS, NUMELT, "PTTRUSS", stat=stat)
        PTSOL  = 0
        PTQUAD = 0
        PTSPH  = 0
        PTSPRI = 0
        PTBEAM = 0
        PTTRUSS= 0
      ELSE
        call my_alloc(PTSOL, 0, "PTSOL", stat=stat)
        call my_alloc(PTQUAD, 0, "PTQUAD", stat=stat)
        call my_alloc(PTSPH, 0, "PTSPH", stat=stat)
        call my_alloc(PTSPRI, 0, "PTSPRI", stat=stat)
        call my_alloc(PTBEAM, 0, "PTBEAM", stat=stat)
        call my_alloc(PTTRUSS, 0, "PTTRUSS", stat=stat)
      END IF
C
      ICO = 0
      ITET4_10=0
      CALL TET4_10(IGEO,ITET4_10)
      IF(NUMELS10/=0.OR.NUMELS16/=0.OR.NUMELS20/=0.OR.ITET4_10/=0) THEN
        ICO=12
      ENDIF
C
C non optimise (12 max(8,10,12)
      CALL MY_ALLOC(MSS,8*NUMELS,"MSS")
      CALL MY_ALLOC(MSSX,ICO*NUMELS,"MSSX")
      CALL MY_ALLOC(MSSF,8*NUMELS*MAX(IALE,IEULER,IALELAG),"MSSF")
      CALL MY_ALLOC(MSQ,NUMELQ,"MSQ")
      IF(.NOT.ALLOCATED(MSTR)) ALLOCATE(MSTR(NUMELT))
      IF(.NOT.ALLOCATED(MSP)) ALLOCATE(MSP(NUMELP))
      CALL MY_ALLOC(MSR,NUMELR*3,"MSR")
      CALL MY_ALLOC(INP,NUMELP,"INP")
      CALL MY_ALLOC(INR,NUMELR*3,"INR")
      CALL MY_ALLOC(INS,NUMELS*8,"INS")
      MSS   = ZERO
      MSSX  = ZERO
      MSSF  = ZERO
      MSQ   = ZERO
      MSTR  = ZERO
      MSP   = ZERO
      MSR   = ZERO
      INP   = ZERO
      INR   = ZERO
      INS   = ZERO
C------
C      ELSE
C        ALLOCATE(MSS(0)     ,STAT=stat)
C        ALLOCATE(MSSX(0)    ,STAT=stat)
C        ALLOCATE(MSSF(0)    ,STAT=stat)
C        ALLOCATE(MSQ(0)     ,STAT=stat)
C        ALLOCATE(MSTR(0)    ,STAT=stat)
C        ALLOCATE(MSP(0)     ,STAT=stat)
C        ALLOCATE(MSR(0)     ,STAT=stat)
C        ALLOCATE(INP(0)     ,STAT=stat)
C        ALLOCATE(INR(0)     ,STAT=stat)
C        ALLOCATE(INS(0)     ,STAT=stat)
C      ENDIF
C----
      CALL MY_ALLOC(XELEMWA,MAXNX*16,"XELEMWA")
      XELEMWA = ZERO
      IF(I7STIFS/=0) THEN
        ICO = 0
        IF(NUMELS10/=0.OR.NUMELS16/=0.OR.NUMELS20/=0.OR.ITET4_10/=0) THEN
          ICO=12
        ENDIF
        CALL MY_ALLOC(VNS,NUMELS*8+NUMELIG3D*NCTRLMAX,"VNS")
        CALL MY_ALLOC(VNSX,NUMELS*ICO,"VNSX")
        CALL MY_ALLOC(STC,NUMELC,"STC")
        CALL MY_ALLOC(STT,NUMELT,"STT")
        CALL MY_ALLOC(STP,NUMELP,"STP")
        CALL MY_ALLOC(STR,NUMELR,"STR")
        CALL MY_ALLOC(STTG,NUMELTG,"STTG")
        CALL MY_ALLOC(STUR,0,"STUR")
        CALL MY_ALLOC(BNS,NUMELS*8+NUMELIG3D*NCTRLMAX,"BNS")
        CALL MY_ALLOC(BNSX,NUMELS*ICO,"BNSX")
        CALL MY_ALLOC(VNIGE,NUMELIG3D*NCTRLMAX,"VNIGE")
        CALL MY_ALLOC(BNIGE,NUMELIG3D*NCTRLMAX,"BNIGE")
        VNS = ZERO
        VNSX = ZERO
        STC = ZERO
        STT = ZERO
        STP = ZERO
        STR = ZERO
        STTG = ZERO
        STUR = ZERO
        BNS = ZERO
        BNSX = ZERO
        VNIGE = ZERO
        BNIGE = ZERO
      ELSE
        CALL MY_ALLOC(VNS,0,"VNS")
        CALL MY_ALLOC(VNSX,0,"VNSX")
        CALL MY_ALLOC(STC,0,"STC")
        CALL MY_ALLOC(STT,0,"STT")
        CALL MY_ALLOC(STP,0,"STP")
        CALL MY_ALLOC(STR,0,"STR")
        CALL MY_ALLOC(STTG,0,"STTG")
        CALL MY_ALLOC(STUR,0,"STUR")
        CALL MY_ALLOC(BNS,0,"BNS")
        CALL MY_ALLOC(BNSX,0,"BNSX")
        CALL MY_ALLOC(VNIGE,0,"VNIGE")
        CALL MY_ALLOC(BNIGE,0,"BNIGE")
      ENDIF
      IF(I7STIFS/=0)THEN
        CALL MY_ALLOC(VOLNOD,NUMNOD+NUMFAKENODIGEO,"VOLNOD")
        CALL MY_ALLOC(BVOLNOD,NUMNOD+NUMFAKENODIGEO,"BVOLNOD")
        CALL MY_ALLOC(ETNOD,NUMNOD,"ETNOD")
        CALL MY_ALLOC(NSHNOD,NUMNOD,"NSHNOD")
        VOLNOD  = ZERO
        BVOLNOD = ZERO
        ETNOD   = ZERO
        NSHNOD  = ZERO
        STIFINT = ZERO
        STIFINTR = ZERO
      ELSE
        CALL MY_ALLOC(VOLNOD,0,"VOLNOD")
        CALL MY_ALLOC(BVOLNOD,0,"BVOLNOD")
        CALL MY_ALLOC(ETNOD,0,"ETNOD")
        CALL MY_ALLOC(NSHNOD,0,"NSHNOD")
      ENDIF

C-- Rot. Stiffness parith/on computation -> allocated even if no interfaces, to avoid "if" in element routines
      CALL MY_ALLOC(STRC,NUMELC,"STRC")
      CALL MY_ALLOC(STRP,NUMELP,"STRP")
      CALL MY_ALLOC(STRR,NUMELR,"STRR")
      CALL MY_ALLOC(STRTG,NUMELTG,"STRTG")
      STRC = ZERO
      STRP = ZERO
      STRR = ZERO
      STRTG = ZERO
C---
      CALL MY_ALLOC(INDEX,SINDEX,"INDEX")
      CALL MY_ALLOC(ITRI,SITRI,"ITRI")
      CALL MY_ALLOC(KSYSUSR,2*JMAX,"KSYSUSR")
      CALL MY_ALLOC(ISPTAG,NUMSPH,"ISPTAG")
      IF(SINDEX > 0) INDEX = 0
      IF(SITRI  > 0) ITRI  = 0
      IF(JMAX   > 0) KSYSUSR = 0
      IF(NUMSPH > 0) ISPTAG  = 0
C
      IF(NRBYKIN>0) THEN
         CALL MY_ALLOC(IWA,NUMNOD,"IWA")
      ELSE
         CALL MY_ALLOC(IWA,0,"IWA")
      ENDIF
C
      CALL TRACE_OUT1()

      ERR_MSG='INITIALIZATION'
      ERR_CATEGORY='ELEMENT INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C----------------------------------
C -- READING OF INITIAL STATE DATA
C----------------------------------
C
      IF(ALLOCATED(SIGI))    CALL MY_DEALLOC(SIGI)
      CALL MY_ALLOC(SIGI,NSIGS,LSIGI,"SIGI")
      IF(ALLOCATED(SIGSH))   CALL MY_DEALLOC(SIGSH)
      call MY_ALLOC(SIGSH, MAX(1,NSIGSH), MAX(1,LSIGSH), "SIGSH")
      IF(ALLOCATED(SIGSP))   CALL MY_DEALLOC(SIGSP)
      CALL MY_ALLOC(SIGSP,NSIGI,LSIGSP,"SIGSP")
      IF(ALLOCATED(SIGSPH)) CALL MY_DEALLOC(SIGSPH)
      CALL MY_ALLOC(SIGSPH,NSIGSPH,LSIGSPH,"SIGSPH")
      IF(ALLOCATED(SIGRS))   CALL MY_DEALLOC(SIGRS)
      CALL MY_ALLOC(SIGRS,NSIGRS,LSIGRS,"SIGRS")
      IF(ALLOCATED(SIGBEAM)) CALL MY_DEALLOC(SIGBEAM)
      CALL MY_ALLOC(SIGBEAM,NSIGBEAM,LSIGBEAM,"SIGBEAM")
      IF(ALLOCATED(SIGTRUSS)) CALL MY_DEALLOC(SIGTRUSS)
      CALL MY_ALLOC(SIGTRUSS,NSIGTRUSS,LSIGTRUSS,"SIGTRUSS")
      IF(ALLOCATED(STRSGLOB)) CALL MY_DEALLOC(STRSGLOB)
      CALL MY_ALLOC(STRSGLOB,NUMELS,"STRSGLOB")
      IF(ALLOCATED(STRAGLOB)) CALL MY_DEALLOC(STRAGLOB)
      CALL MY_ALLOC(STRAGLOB,NUMELS,"STRAGLOB")
      IF(ALLOCATED(ORTHOGLOB)) CALL MY_DEALLOC(ORTHOGLOB)
      CALL MY_ALLOC(ORTHOGLOB,NUMELS,"ORTHOGLOB")
C
      IF(LSIGI   > 0) SIGI   = ZERO
      IF(LSIGSH  > 0) SIGSH  = ZERO
      IF(LSIGSP  > 0) SIGSP  = ZERO
      IF(LSIGSPH > 0) SIGSPH = ZERO
      IF(LSIGRS  > 0) SIGRS  = ZERO
      IF(LSIGBEAM > 0)SIGBEAM= ZERO
      IF(LSIGTRUSS > 0)SIGTRUSS= ZERO
      IF(NUMELS  > 0) STRSGLOB  = -1
      IF(NUMELS  > 0) STRAGLOB  = -1
      IF(NUMELS  > 0) ORTHOGLOB  = 0
C

      IF(.NOT. ALLOCATED(IDRAPE)) ALLOCATE(IDRAPE(0))

      CALL LEC_INISTATE(   IXS     ,IXQ     ,IXC     ,IXT     ,
     1            IXP     ,IXR     ,GEO     ,PM      ,KXSP    ,
     2            IXTG    ,INDEX   ,ITRI    ,
     3            NSIGSH  ,IGEO    ,IPM     ,NSIGS   ,NSIGSPH ,
     4            KSYSUSR ,PTSHEL  ,PTSH3N  ,PTSOL   ,PTQUAD  ,
     5            PTSPH   ,NUMEL   ,NSIGRS  ,UNITAB  ,ISOLNOD ,
     6            LSUBMODEL,RTRANS ,IDRAPE  ,NSIGI   ,
     7            PTSPRI  ,NSIGBEAM,PTBEAM  ,NSIGTRUSS,PTTRUSS ,
     8            SIGI    ,SIGSH   ,SIGSP   ,SIGSPH  ,SIGRS   ,
     9            SIGBEAM ,SIGTRUSS,STRSGLOB,STRAGLOB,ORTHOGLOB,
     A            ISIGSH  ,IYLDINI ,KSIGSH3 ,FAIL_INI,IUSOLYLD,
     B            IUSERL  ,IGRBRIC ,MAP_TABLES,IPARG ,STACK ,IWORKSH,
     C            MAT_ELEM%MAT_PARAM,NUMSPH,NISP)
C
C----------------------------------
C -- ELEMENT INITIALIZATION
C----------------------------------
C
      CALL MY_ALLOC(DTELEM,2*NUMEL,"DTELEM")
      IF(NUMEL   > 0) DTELEM = ZERO
C
      CALL STARTIME(14,1)
        
      CALL INITIA(IPARG      , ELBUF          , MS               , IN                 , V        ,
     &            X          , IXS            , IXQ              , IXC                , IXT      ,
     &            IXP        , IXR            , DETONATORS       , GEO                , PM       ,
     &            RBY        , NPBY           , LPBY             , NPC                , NPTS     ,
     &            TF         , VEUL           , ALE_CONNECTIVITY , SKEW               , FILL     ,
     &            IPART      , ITAB           , SENSORS          , SKVOL              ,
     &            IXTG       , THKE           , NLOC_DMG         , GROUP_PARAM_TAB    ,GLOB_THERM,
     &            IGRNOD     , IGRSURF        , BUFSF            , VR                 ,
     &            BUFMAT     , XLAS           , ILAS             , DTELEM             , MSS      ,
     &            MSQ        , MSC            , MSTR             , MSP                , MSR      ,
     &            MSTG       , PTG            , INC              , NOD2ELTG           , KNOD2ELTG,
     &            INP        , INR            , INTG             , INDEX              ,
     &            ITRI       , KXX            , IXX              , XELEMWA            ,
     &            IWA        , NOD2ELQ        , KNOD2ELQ         , NOD2ELS            , KNOD2ELS ,
     &            KXSP       , IXSP           , NOD2SP           , ISPCOND            , ICODE    ,
     &            ISKEW      , ISKWN          , ISPSYM           , XFRAME             , ISPTAG   ,
     &            SPBUF      , MSSX           , NSIGI            ,
     &            NPBYL      , LPBYL          , RBYL             , MSNF               , MSSF     ,
     &            NSIGSH     , IGEO           , IPM              , NSIGS              ,
     &            NSIGSPH    , VNS            , VNSX             , STC                , STT      ,
     &            STP        , STR            , STTG             , STUR               , BNS      ,
     &            BNSX       , VOLNOD         , BVOLNOD          , ETNOD              , NSHNOD   ,
     &            STIFINT    , FXBDEP         , FXBVIT           , FXBACC             , FXBIPM   ,
     &            FXBRPM     , FXBELM         , FXBSIG           , FXBMOD             , INS      ,
     &            PTSHEL     , PTSH3N         , PTSOL            , PTQUAD             ,
     &            WMA        , PTSPH          , FXBNOD           , MBUFEL             , MDEPL    ,
     &            FXANI      , NUMEL          , NSIGRS           ,
     &            SH4TREE    , SH3TREE        , MCP              , TEMP               ,
     &            IMERGE2    , IADMERGE2               ,
     &            SLNRBM     , NSLNRBM        , RMSTIFN          , RMSTIFR            ,
     &            MS_PLY0    , ZI_PLY0        , ITAGND_SHXFEM    , ITAGSH             , MCPC     ,
     &            MCPTG      , XREFC          , XREFTG           , XREFS              , MSSA     ,
     &            MSRT       , IRBE2          , LRBE2            , INIVOL        , KVOL     , NBSUBMAT,
     &            IXS10      , IXS16          , IXS20            , TOTADDMAS          ,
     &            IPMAS      , STIFFN         , MSZ20            , ITAGN              , SITAGE   ,
     &            ITAGE      , IXR_KJ         , ELBUF_TAB        ,
     &            NOM_OPT    , INOM_OPT(13)   , INOM_OPT(21)     , INOM_OPT(20),
     &            SOL2SPH    , IRST           , SH3TRIM          , XFEM_TAB           ,
     &            KXIG3D     , IXIG3D         , MSIG3D           , KNOT               , NCTRLMAX,
     &            WIGE       , STACK          ,
     &            RNOISE     , DRAPE          , SH4ANG           , SH3ANG             ,
     &            GEO_STACK  , IGEO_STACK     , STIFINTR         , STRC               , STRP     ,
     &            STRR       , STRTG          , PERTURB          , ITAGND             , NATIV_SMS,
     &            ILOADP     , LOADP          , PTSPRI           , NSIGBEAM           ,
     &            PTBEAM     , NSIGTRUSS      , PTTRUSS          ,
     &            MULTI_FVM  , SIGI           , SIGSH            , SIGSP              ,
     &            SIGSPH     , SIGRS          , SIGBEAM          , SIGTRUSS           , STRSGLOB ,
     &            STRAGLOB   , ORTHOGLOB      , ISIGSH           , IYLDINI            , KSIGSH3  ,
     &            FAIL_INI   , IUSOLYLD       , IUSERL           , IDDLEVEL           , INIMAP1D ,
     &            INIMAP2D   , FUNC2D         , FVM_INIVEL       , TAGPRT_SMS         , IGRBRIC  ,
     &            IGRQUAD    , IGRSH4N        , IGRSH3N          , IGRPART            , TOTMAS   ,
     &            KNOTLOCPC  , KNOTLOCEL      , VNIGE            , BNIGE              , FXBGLM   ,
     &            FXBCPM     , FXBCPS         , FXBLM            , FXBFLS             , FXBDLS   ,
     &            FXB_MATRIX , FXB_MATRIX_ADD , FXB_LAST_ADDRESS  , INOM_OPT(11)       , R_SKEW   ,
     &            KNOD2EL1D  , NOD2EL1D       , EBCS_TAB         , RBY_INIAXIS        , ALEA     ,
     &            KNOD2ELC   , NOD2ELC        , DR               , SLRBODY            , DRAPEG   ,
     &            IPARI      , INTBUF_TAB     , INTERFACES       , MAT_ELEM%MAT_PARAM ,
     &            NPRELOAD_A , PRELOAD_A      , FAIL_FRACTAL     ,FAIL_BROKMANN       ,DEFAULTS  ,
     &            NDAMP_FREQ_RANGE,DAMPR      , IBEAM_VECTOR     , RBEAM_VECTOR       ,D         ,
     &            LSIGI      ,LSIGSP          , SRNOISE2         ,NPRW               ,LPRW       ,
     &            RWSTIF_PEN ,SLN_PEN         ,ROOTLEN           ,ROOTNAM          ,NGUIDED_CABLE,
     &            TOTMAS1   )


      IF(NINTER>0.AND.NUMELIG3D>0) THEN
        CALL FICTIVMASSIGEO(INTBUF_TAB,NCTRLMAX,MSIG3D  ,KXIG3D)
        IF(I7STIFS/=0)THEN
          CALL BULKFAKEIGEO3(ELBUF_TAB,IPARG,PM,KXIG3D,IGRSURF,STIFINT)
        ENDIF
      ENDIF
C----------------------------------
C -- SFEM (nodal pressure) exception initialization
C----------------------------------
      NE_SFEM = 0
      IF(ISFEM>0) THEN
        CALL SFEM_EXCLUDE_DIM(
     &             NUMNOD,  NPARG,  NGROUP,  IPARG,      IXS,                   
     &             NUMELS,NE_SFEM)
      ENDIF
      IF(NE_SFEM > 0) THEN
          call my_alloc(IN_SFEM,NE_SFEM) 
      ELSEIF(.NOT. ALLOCATED(IN_SFEM)) THEN
          CALL MY_ALLOC(IN_SFEM,0,"IN_SFEM")
      END IF
      CALL SFEM_EXCLUDE_INI(
     &             NUMNOD,  NPARG,  NGROUP,  IPARG,      IXS,                   
     &             NUMELS,IN_SFEM, NE_SFEM)
C
C----------------------------------
C
      CALL STOPTIME(14,1)


      CALL TRACE_OUT1()
!---
C--------------------------------------------
C     INITIALIZATION OF BUFFERS  --- IGRNOD, IGRBRIC, ..., IGRSURF, ... ---
C--------------------------------------------
      ERR_MSG='GROUP ENTITIES BUFFER INITIALIZATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
!!      WRITE(ISTDO,'(A)')TITRE(45)
!
      CALL GROUP_INI(IGRNOD   ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N,
     .               IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRPART )
      CALL ISURF_INI(IGRSURF)
      CALL ISLIN_INI(IGRSLIN)
!
      CALL TRACE_OUT1()
C---
C
!!      DEALLOCATE(SIGI)
!!      DEALLOCATE(SIGSH)
!!      DEALLOCATE(SIGSP)
      CALL MY_DEALLOC(SIGSPH)
!!      DEALLOCATE(SIGRS)
!!      DEALLOCATE(SIGBEAM)
!!      DEALLOCATE(SIGTRUSS)
      CALL MY_DEALLOC(XELEMWA)
      CALL MY_DEALLOC(STRSGLOB)
      CALL MY_DEALLOC(STRAGLOB)
      CALL MY_DEALLOC(ORTHOGLOB)
C
      IF(ALLOCATED(PTSHEL))CALL MY_DEALLOC(PTSHEL)
      IF(ALLOCATED(PTSH3N))CALL MY_DEALLOC(PTSH3N)
      CALL MY_DEALLOC(PTSOL)
      CALL MY_DEALLOC(PTQUAD)
      CALL MY_DEALLOC(PTSPH)
      CALL MY_DEALLOC(PTSPRI)
      CALL MY_DEALLOC(PTBEAM)
      CALL MY_DEALLOC(PTTRUSS)
      CALL MY_DEALLOC(MSS)
      CALL MY_DEALLOC(MSSX)
      CALL MY_DEALLOC(MSSF)
      CALL MY_DEALLOC(MSQ)
      CALL MY_DEALLOC(MSR)
      IF(ALLOCATED(MSIG3D)) CALL MY_DEALLOC(MSIG3D)
      IF(ALLOCATED(TABCONPATCH)) DEALLOCATE(TABCONPATCH)
      CALL MY_DEALLOC(INP)
      CALL MY_DEALLOC(INR)
      CALL MY_DEALLOC(INS)
      CALL MY_DEALLOC(VNS)
      CALL MY_DEALLOC(VNSX)
      CALL MY_DEALLOC(STC)
      CALL MY_DEALLOC(STT)
      CALL MY_DEALLOC(STP)
      CALL MY_DEALLOC(STR)
      CALL MY_DEALLOC(STTG)
      CALL MY_DEALLOC(STUR)
      CALL MY_DEALLOC(BNS)
      CALL MY_DEALLOC(BNSX)
      CALL MY_DEALLOC(VOLNOD)
      CALL MY_DEALLOC(BVOLNOD)
      CALL MY_DEALLOC(ETNOD)
      CALL MY_DEALLOC(NSHNOD)
      CALL MY_DEALLOC(VNIGE)
      CALL MY_DEALLOC(BNIGE)
      CALL MY_DEALLOC(STRC)
      CALL MY_DEALLOC(STRP)
      CALL MY_DEALLOC(STRR)
      CALL MY_DEALLOC(STRTG)
      CALL MY_DEALLOC(ISPTAG)
      CALL MY_DEALLOC(INDEX)
      CALL MY_DEALLOC(ITRI)
      CALL MY_DEALLOC(KSYSUSR)
      CALL MY_DEALLOC(IWA)
      IF (DEFAULTS%SHELL%IOFFSET==1) THEN
        CALL MY_DEALLOC(ITAGOSET)
        CALL MY_DEALLOC(XYZ)
      END IF

C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
      ENDIF !IF((IDDLEVEL == 1).OR.(ISMS_SELEC >= 3))
C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
      IF((IDDLEVEL == 0).AND.((NINTER > 0).OR.(ISMS == 1))) THEN
        IDDLEVEL = 1
        WRITE(ISTDO,*)
     .  '.. RETURNS TO DOMAIN DECOMPOSITION FOR OPTIMIZATION'
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C return to domain decomposition
C   ---> for interface and AMS  - small loop, without INITIA.F
C-  ---> for AMS with automatic element selection - big loop with INITIA.F
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
        IF(IALE+IEULER/=0) CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
        IF(NUMELS/=0) CALL LCE16S4(IXS    ,PM     ,ICODE  )
C
        LAG_NCF = LAG_NCF0
        LAG_NKF = LAG_NKF0
        LAG_NHF = LAG_NHF0
        LAG_NCL = LAG_NCL0
        LAG_NKL = LAG_NKL0
C
        CALL MY_DEALLOC(ELBUF)
        CALL MY_DEALLOC(DD_IAD)
C
        IF(ISMS_SELEC >= 3) THEN
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C       Additional treatments for big loop - element buffer deallocation
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          CALL MY_DEALLOC(DTELEM)
          FLAG_XFEM = 0
          CALL DEALLOCATE_ELBUF(
     .               ELBUF_TAB, IGEO     ,IPARG    ,IXS      ,IXC      ,IXTG     ,
     .               FLAG_XFEM, IXT      ,IXP      ,IXR      ,KXX       )
          DEALLOCATE(ELBUF_TAB)
C       XFEM buffer deallocation
          IF(ICRACK3D > 0) THEN
            FLAG_XFEM = 1
            DO IXEL=1,NXEL
              CALL DEALLOCATE_ELBUF(
     .               XFEM_TAB(1:NGROUP,IXEL),IGEO     ,IPARG    ,IXS      ,IXC      ,IXTG    ,
     .               FLAG_XFEM              ,IXT      ,IXP      ,IXR      ,KXX        )
            ENDDO
          ENDIF
C
          REWIND(IIN4)
          REWIND(IIN5)
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        ENDIF
C
        GOTO 100
C
      ENDIF
C
C------------------------------------------------------------------------
C
C  Check tied contacts -> hierarchy + warnings AMS
      ERR_CATEGORY='INTERFACES'
      CALL CHKTYP2 (IPARI,   ITAB    ,
     .              NOM_OPT(LNOPT1*INOM_OPT(3)+1),INTBUF_TAB,NATIV_SMS)
c-----------------------------------------------------------------------------------
c     Initialization of frontwave structure for failure propagation
c
      CALL FAILWAVE_INIT(FAILWAVE,IPARG,IXC,IXTG,NUMNOD)
c
c-----------------------------------------------------------------------------------
c
C rigid material
C

      ERR_MSG='RIGID MATERIALS'
      ERR_CATEGORY='RIGID MATERIALS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
       IF(IRIGID_MAT > 0) THEN
C
C  NFRBYM = 28 (
C  NIRBYM = 2
         call my_alloc(RBYM, NFRBYM*NRBYM, "RBYM")
         call my_alloc(IRBYM, NRBYM*NIRBYM, "IRBYM")
         call my_alloc(LNRBYM, NGSLNRBYM, "LNRBYM")
         RBYM = 0
         IRBYM = 0
         LNRBYM = 0
C
         CALL RIGID_MAT(NRBYM   ,NGSLNRBYM ,SLNRBM, NSLNRBM ,RMSTIFN,
     .                  RMSTIFR ,X     ,V       ,MS      , IN       ,
     .                  RBYM  ,IRBYM    ,LNRBYM ,NOM_OPT)
C
           LEN_RM = NRBYM*NSPMD
           CALL MY_ALLOC(FRONT_RM,LEN_RM,"FRONT_RM")
           FRONT_RM = 0
           CALL MY_ALLOC(WEIGHT_RM,NRBYM,"WEIGHT_RM")
           WEIGHT_RM = 1
       ELSE
          call my_alloc(RBYM, 0, "RBYM")
          call my_alloc(IRBYM, 0, "IRBYM")
          call my_alloc(LNRBYM, 0, "LNRBYM")
          call my_alloc(WEIGHT_RM, 0, "WEIGHT_RM")
       ENDIF
      CALL TRACE_OUT1()
      ERR_MSG='DEALLOCATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_DEALLOC(SLNRBM)
      CALL MY_DEALLOC(NSLNRBM)
      CALL MY_DEALLOC(RMSTIFN)
      CALL MY_DEALLOC(RMSTIFR)
C----
      IF(ALLOCATED(MSIG3D)) CALL MY_DEALLOC(MSIG3D)
      IF(ALLOCATED(ITAG)) CALL MY_DEALLOC(ITAG)
C
C xfem for compostie
C
      CALL TRACE_OUT1()
      ERR_MSG='XFEM FOR COMPOSIT'
      ERR_CATEGORY='XFEM FOR COMPOSIT'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IPLYXFEM > 0) THEN
        NPLYXFE = 0
        EPLYXFE = 0
        DO I=1,NUMNOD
         IF(ITAGND_SHXFEM(I) > 0 )THEN
           NPLYXFE = NPLYXFE + 1
           INOD_PXFEM(I) = NPLYXFE
         ENDIF
        ENDDO
C
        DO I=1,NUMELC
          IF(ITAGSH(I) > 0) THEN
           EPLYXFE = EPLYXFE + 1
           IEL_PXFEM(I) = EPLYXFE
          ENDIF
        ENDDO
          CALL MY_ALLOC(MS_PLY,NPLYXFE*NPLYMAX,"MS_PLY")
          MS_PLY=ZERO
          CALL MY_ALLOC(ZI_PLY,NPLYXFE*NPLYMAX,"ZI_PLY")
          ZI_PLY=ZERO
C
          CALL MY_ALLOC(MSZ2,NPLYXFE,"MSZ2")
          MSZ2=ZERO
C
          CALL PREPLYXFEM(MS_PLY0,ZI_PLY0,IEL_PXFEM,INOD_PXFEM,IXC,
     .                  MS_PLY,ZI_PLY,ADDCNE_PXFEM,MSZ20,MSZ2)
          LCNE_PXFEM = ADDCNE_PXFEM(NPLYXFE+1) - 1
          call my_alloc(CNE_PXFEM, LCNE_PXFEM, "CNE_PXFEM")
          call my_alloc(CEL_PXFEM, EPLYXFE, "CEL_PXFEM")
          CNE_PXFEM = 0
          CEL_PXFEM = 0
C
          CALL FILLCNE_PXFEM(IEL_PXFEM,INOD_PXFEM,IXC,CEP,ADDCNE_PXFEM,
     .                       CNE_PXFEM, CEL_PXFEM)
C
      ENDIF
C
      CALL MY_DEALLOC(MS_PLY0)
      CALL MY_DEALLOC(ZI_PLY0)
      CALL MY_DEALLOC(MSZ20)
      CALL MY_DEALLOC(ITAGSH)
      CALL MY_DEALLOC(ITAGND_SHXFEM)
      CALL TRACE_OUT1()
C-------------------------------------
C     /DAMP/VREL with RBY
C-------------------------------------
      IF (NDAMP_VREL_RBY > 0) THEN
        CALL damping_rby_spmdset(IGRNOD,NGRNOD,NDAMP,NRDAMP,DAMPR,NNPBY,
     .                           NRBODY,NPBY,NRBMERGE) 
      ENDIF       
c-------------------------------------------------------------------
c-------------------------------------------------------------------
C     XFEM for crack propagation within shell (mono + multi layers)
c-------------------------------------------------------------------
      ERR_MSG='XFEM FOR SHELLS'
      ERR_CATEGORY='XFEM FOR SHELLS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
c------------------------------
      allocate(CRKLVSET(NLEVMAX))
      allocate(CRKSHELL(NLEVMAX))
      allocate(CRKSKY(NLEVMAX))
      allocate(CRKAVX(NLEVMAX))
      CALL MY_ALLOC(INDX_CRK,NLEVMAX,"INDX_CRK")
c
      INDX_CRK = 0  ! For Anim
      NCRKPART = 0  ! Nombre des parts xfem  (local proc)
      NCRKXFE  = 0  ! number of xfem nodes
      ECRKXFE  = 0  ! number of xfem elements
      ECRKXFEC = 0  ! Nombre des shells 4N xfem
      ECRKXFETG= 0  ! Nombre des shells 3N xfem
c------------------------------
      IF(ICRACK3D > 0) THEN
c-----
        IF(ICRACK3D == 1) THEN
          WRITE(ISTDO,'(A)')' .. XFEM MULTI-LAYER SHELL'
        ELSEIF(ICRACK3D == 2)THEN
          WRITE(ISTDO,'(A)')' .. XFEM MONO-LAYER SHELL'
        ELSEIF(ICRACK3D == 3)THEN
          WRITE(ISTDO,'(A)')' .. XFEM MIXED MONO/MULTI-LAYER SHELL'
        ENDIF
c-----
c       local numbering of ghost nodes and elements of xfem parts
c       IEL_CRKXFEM  : local system numbering of Xfem shells
c       INOD_CRKXFEM : local system numbering of Xfem nodes
c--------------------------------------------------
c       build local Xfem node and element tables
        CALL PRETAG_XFEM(IPARG  ,ITAGE   ,IEL_CRKXFEM,ITAGN   ,INOD_CRKXFEM)
c
c       build xfem sky address table
        CALL PRECRKXFEM(IPARG ,IXC  ,IXTG   ,NCRKXFE ,
     .                  IEL_CRKXFEM ,INOD_CRKXFEM    ,ADDCNE_CRKXFEM)
c--------------------------------------------------
c       Addcne_crkxfem = Sky Xfem addresses table
        LCNE_CRKXFEM = ADDCNE_CRKXFEM(NCRKXFE+1) - 1    ! Table length Sky CNE_CRKXFEM
        CALL MY_ALLOC(CRKNODIAD,LCNE_CRKXFEM,"CRKNODIAD")
        CALL MY_ALLOC(CNE_CRKXFEM,LCNE_CRKXFEM,"CNE_CRKXFEM")
        CALL MY_ALLOC(CEL_CRKXFEM,ECRKXFE,"CEL_CRKXFEM")
        CALL MY_ALLOC(CEP_CRKXFEM,ECRKXFE,"CEP_CRKXFEM")
        CALL MY_ALLOC(NODLEVXF,NCRKXFE,"NODLEVXF")
        allocate(CRKEDGE(NXLAYMAX))
        allocate(XFEM_PHANTOM(NXLAYMAX))
        CRKNODIAD   = 0
        CNE_CRKXFEM = 0
        CEL_CRKXFEM = 0
        CEP_CRKXFEM = 0   ! Proc num of each xfem element
        NUMELCRK    = 0
        NODLEVXF    = 0
c--------------------------------------------------
        CALL FILLCNE_XFEM(LCNE_CRKXFEM,IPARG,
     .       IEL_CRKXFEM   ,INOD_CRKXFEM ,IXC        ,IXTG       ,CEP      ,
     .       ADDCNE_CRKXFEM,CNE_CRKXFEM  ,CEL_CRKXFEM,CEP_CRKXFEM,CRKNODIAD)
c
        CALL XFEM_CRACK_INIT(IPARG   ,IXC     ,IXTG    ,INOD_CRKXFEM,NODLEVXF,
     .                       INDX_CRK,NCRKPART,CRKSHELL)
c--------------------------------------------------
        CALL MY_DEALLOC(ITAGN)
        CALL MY_DEALLOC(ITAGE)
C       NODGLOBXFE
        SNODGLOBXFE = 4*ECRKXFE*NLEVMAX  ! NB max of Phant nodes, all folds.fake
C
        NUMEDGES = 0   ! number of global edges (same for all plies)
        SIEDGESH = 4*ECRKXFEC + 3*ECRKXFETG
c
        CALL MY_ALLOC(IEDGESH,SIEDGESH,"IEDGESH")
        CALL MY_ALLOC(IBORDEDGE,SIEDGESH,"IBORDEDGE")
        CALL MY_ALLOC(NODEDGE,2*SIEDGESH,"NODEDGE")
        CALL MY_ALLOC(IEDGE,SIEDGESH,"IEDGE")
        CALL MY_ALLOC(IEDGE_TMP0,SIEDGESH,"IEDGE_TMP0")
        IEDGESH   = 0
        IBORDEDGE = 0
        NODEDGE   = 0
        IEDGE     = 0
        IEDGE_TMP0= 0
!
        IF(ECRKXFEC > 0) THEN
          IEDGESH4 => IEDGESH(1:4*ECRKXFEC)
          IELCRK4  => IEL_CRKXFEM(1:NUMELC)
        ELSE
          IEDGESH4 => IEDGESH
          IELCRK4  => IEL_CRKXFEM
        ENDIF
!
        IF(ECRKXFETG > 0) THEN
          IEDGESH3 => IEDGESH(1+4*ECRKXFEC:SIEDGESH)
          IELCRK3  => IEL_CRKXFEM(1+NUMELC:NUMELC+NUMELTG)
        ELSE
          IEDGESH3 => IEDGESH
          IELCRK3  => IEL_CRKXFEM
        ENDIF
c--------------------------------------------------
        CALL IEDGE_XFEM(
     .       IBORDNODE ,IXC        ,IXTG       ,IEDGESH4  ,IEDGESH3  ,
     .       IBORDEDGE ,NODEDGE    ,IELCRK4    ,IELCRK3   ,IEDGE     ,
     .       CEP_CRKXFEM,IEDGE_TMP0)
c
        CALL ALLOCXFEM(IXC    ,IXTG  ,IPARG  ,LCNE_CRKXFEM,CRKLVSET,
     .                 CRKSKY ,CRKAVX,CRKEDGE,XFEM_PHANTOM)
c--------------------------------------------------
        IF(NINICRACK > 0)   ! initial cracks
     .    CALL INICRKFILL (ELBUF_TAB,XFEM_TAB,
     .         IXC     ,IXTG       ,IPARG       ,INICRACK,
     .         X       ,IEL_CRKXFEM,INOD_CRKXFEM,XREFC   ,XREFTG  ,
     .         IEDGESH4  ,IEDGESH3,NODEDGE ,CRKLVSET,
     .         CRKSHELL,CRKEDGE    ,XFEM_PHANTOM         ,ITAB  )
c--------------------------------------------------
        CALL MY_ALLOC(IEDGE_TMP,3,NUMEDGES,"IEDGE_TMP")
        IF(NUMEDGES > 0) THEN
          DO I=1,NUMEDGES
            IEDGE_TMP(1,I) = 0
            IEDGE_TMP(2,I) = 0
            IEDGE_TMP(3,I) = IEDGE_TMP0(I)
          ENDDO
        ENDIF
        CALL MY_DEALLOC(IEDGE_TMP0)
C---
        CALL MY_ALLOC(ELCUTC,2*(NUMELC+NUMELTG),"ELCUTC")
        ELCUTC = 0
C---
        CALL MY_ALLOC(NODENR,NCRKXFE,"NODENR")
        NODENR = 0
C---
        CALL MY_ALLOC(KXFENOD2ELC,NCRKXFE,"KXFENOD2ELC")
        KXFENOD2ELC = 0
C---
        CALL MY_ALLOC(ENRTAG,NUMNOD*IENRNOD,"ENRTAG")
        ENRTAG = 0
C---
      ELSE
        NUMEDGES = 0
        SIEDGESH = 0
        NUMELCRK= 0
        call my_alloc(CNE_CRKXFEM, 0, "CNE_CRKXFEM")
        call my_alloc(CEL_CRKXFEM, 0, "CEL_CRKXFEM")
        call my_alloc(CEP_CRKXFEM, 0, "CEP_CRKXFEM")
        CALL MY_ALLOC(IEDGESH,0,"IEDGESH")
        CALL MY_ALLOC(IBORDEDGE,0,"IBORDEDGE")
        CALL MY_ALLOC(NODEDGE,0,"NODEDGE")
        CALL MY_ALLOC(IEDGE,0,"IEDGE")
        CALL MY_ALLOC(IEDGE_TMP,0,0,"IEDGE_TMP")
        CALL MY_ALLOC(CRKNODIAD,0,"CRKNODIAD")
        CALL MY_ALLOC(NODLEVXF,0,"NODLEVXF")
        allocate(CRKEDGE(0))
C
        CALL MY_ALLOC(ELCUTC,0,"ELCUTC")
        CALL MY_ALLOC(NODENR,0,"NODENR")
        CALL MY_ALLOC(KXFENOD2ELC,0,"KXFENOD2ELC")
        CALL MY_ALLOC(ENRTAG,0,"ENRTAG")
      ENDIF   !   ICRACK3D > 0  (Xfem)
C----------------------------------
      CALL TRACE_OUT1()
C----------------------------------
C     RBE2 Desactivation des elements initialization for ITRUOFF ...
C----------------------------------
      ERR_MSG='RIGID BODY ELEMENT DEACTIVATION'
      ERR_CATEGORY='RIGID BODY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SETELOFF2(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2               IXTG   ,IPARG  ,ISOLOFF,ISHEOFF,
     3              ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRB2,
     4               IGRNOD ,IRBE2  )
C----------------------------------
C     rbody deactivation of rigid body elements (on by default)
C----------------------------------
      CALL SETELOFF(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2              IXTG   ,IPARG  ,        ISOLOFF,ISHEOFF,
     3              ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRBY,
     4              IGRNOD ,ELBUF_TAB,IQUAOFF,IXQ  )
      CALL TRACE_OUT1()
C----------------------------------
C     Interf Stamp. Tri et initialisations
C----------------------------------
      ERR_MSG='STAMPING INITIALIZATION'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINTER/=0)THEN
C-----
        AUX = MAX( NUMNOD , NUMELT+NUMELP+NUMELR+NUMELTG+NUMELC+100 ,
     .             MAXRTM+100 )
        NS_I21 = 2*NUMNOD + 2002 + 4*AUX
        SIWORK = NS_I21
        SRWORK = MAX(6000,NUMNOD)
        CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
        CALL MY_ALLOC(RWORK,SRWORK,"RWORK")
        IWORK = 0
        RWORK = ZERO
C-----
        CALL ININTR_THKVAR(ELBUF_TAB,
     1               IPARI   ,INTBUF_TAB       ,INSCR    ,X       ,
     2               IXS     ,IXC     ,PM      ,GEO      ,ITAB    ,
     3               IWORK   ,RWORK   ,IXTG    ,D        ,
     4               IPARG   ,KNOD2ELS ,
     5               KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC  ,NOD2ELTG,
     6               INTSTAMP,SKEW     ,MS     ,IN       ,V       ,
     7               VR      ,RBY      ,NPBY   ,LPBY     ,IPARTS  ,
     8               IPARTC  ,IPARTG,THK_PART,NOM_OPT,INOM_OPT(3))
        CALL MY_DEALLOC(RWORK)
        CALL MY_DEALLOC(IWORK)
C-----
      END IF
C-----

      CALL MY_DEALLOC(THK_PART)
      CALL TRACE_OUT1()
C-------------------------------------------------------------
C     Set INTERCEP only for INTERFACE24 (flag=0)
C-------------------------------------------------------------
      CALL SET_INTERCEP(IPARI,INTERCEP,0,INTBUF_TAB,ITAB,CEP)      ! this call is maintained here to avoid a bug
C-------------------------------------------------------------
C     Interface type 24 - set FRONTPLUS to neighboug surfaces
C-------------------------------------------------------------
      I24MAXNSNE = 0
      CALL I24SETNODES(IPARI,INTBUF_TAB,INTERCEP,ITAB,I24MAXNSNE)

C-------------------------------------------------------------
C----------------------------------
C     Interf.Type 7 and 21: Assignment of rigidites on the second side
C----------------------------------
      ERR_MSG='INTERFACES STIFFNESS'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINTER>0)THEN
        IF (I7STIFS/=0) THEN
          CALL STIFINT_ICONTROL(
     1                 NUMNOD,    STIFINT,      NPARI,        NINTER,
     2                  IPARI,    NPROPGI,     NUMGEO,          IGEO, 
     3                 NUMELS,       NIXS,        IXS,       NUMELS8, 
     4               NUMELS10,      IXS10,   NUMELS16,         IXS16, 
     5               NUMELS20,      IXS20,     NPROPM,        NUMMAT, 
     6                     PM, INTBUF_TAB)
          CALL ININTR1 (IPARI   ,STIFINT, INTBUF_TAB ,STFAC)
        END IF
        CALL MY_DEALLOC(STFAC)
      ENDIF
      CALL TRACE_OUT1()

C--------FRICTION OROTHTROPIC DIRECTIONS COMPUTATION -----
      IF(NINTER > 0 .AND.NINTERFRIC >0.AND. IORTHFRICMAX > 0) THEN

        CALL ININTR_ORTHDIRFRIC(
     A       IPARI   ,INTBUF_TAB,INTBUF_FRIC_TAB,IGEO  ,GEO      ,
     B       X       , IXTG     ,IXC          ,IPARTG  , IPARTC  ,
     C       PFRICORTH,IREPFORTH,PHIFORTH     , VFORTH ,KNOD2ELC ,
     D       KNOD2ELTG,NOD2ELTG ,NOD2ELC      ,IWORKSH  ,PM      ,
     E       STACK%PM ,THKE    ,SKEW          ,ITAB     ,IPART   )

c        DEALLOCATE(PFRICORTH ,IREPFORTH , VFORTH ,PHIFORTH  )

      ENDIF

        CALL MY_DEALLOC(TAGPRT_FRIC)
C---------------------------
C     IMPACTS LASER TRAITEMENT SPMD 2eme Phase
C---------------------------
      ERR_MSG='LASER IMPACT PHASE 2'
      ERR_MSG='LASER'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NLASER>0) THEN
          CALL LASERP3(ILAS   ,IPARG  )
        ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     reading rivets
C----------------------------------
      ERR_MSG='RIVETS'
      ERR_CATEGORY='RIVETS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SLRIVET = NRIVET*4
      SRIVET  = NRIVET*NRIVF
      CALL MY_ALLOC(LRIVET,SLRIVET,"LRIVET")
      CALL MY_ALLOC(RIVET,SRIVET,"RIVET")
      LRIVET = 0
      RIVET  = ZERO
C
      IF(NRIVET/=0)THEN
        WRITE(ISTDO,'(A)') ' .. RIVETS '
        CALL HM_READ_RIVET(LRIVET ,V      ,VR     ,MS     ,IN     ,
     2                     RIVET  ,GEO    ,ITAB   ,ITABM1 ,D      ,
     3                     IPART  ,IGEO   ,LSUBMODEL)
      ENDIF
c      CALL ANCHECK(81)
      CALL TRACE_OUT1()
C----------------------------------
C     SEATBELT 2D->1D for SECTIONS
C----------------------------------
        IF(NB_SEATBELT_SHELLS /= 0)THEN
          CALL MY_ALLOC(SEATBELT_SHELL_TO_SPRING,NUMELC,2,"SEATBELT_SHELL_TO_SPRING")
          IF(NUMELC > 0)THEN
            SEATBELT_SHELL_TO_SPRING(1:NUMELC,1) = 0
            SEATBELT_SHELL_TO_SPRING(1:NUMELC,2) = 0
          ENDIF
c
          DO I=1,NB_SEATBELT_SHELLS

            L0 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(2,I) /= 0) THEN
              L0 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(1,I),MAP_TABLES%ISH4NM,NUMELC)
            ENDIF

            L1 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(2,I) /= 0) THEN
              L1 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(2,I),MAP_TABLES%ISPRINGM,NUMELR)
            ENDIF

            L2 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(3,I) /= 0) THEN
              L2 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(3,I),MAP_TABLES%ISPRINGM,NUMELR)
            ENDIF

            IF(L0 /= 0) THEN
              SEATBELT_SHELL_TO_SPRING(L0,1) = L1
              SEATBELT_SHELL_TO_SPRING(L0,2) = L2
            ENDIF
            
          ENDDO
        ELSE
          CALL MY_ALLOC(SEATBELT_SHELL_TO_SPRING,1,2,"SEATBELT_SHELL_TO_SPRING")
          SEATBELT_SHELL_TO_SPRING(1,1) = 0
          SEATBELT_SHELL_TO_SPRING(1,2) = 0
        ENDIF
C----------------------------------
C     reading sections
C----------------------------------
      ERR_MSG='SECTIONS'
      ERR_CATEGORY='SECTIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSECT/=0)THEN
       WRITE(ISTDO,'(A)')  ' .. SECTIONS'
        CALL PRELECSEC(
     1      SNSTRF  ,SSECBUF   ,ITABM1  ,0       ,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2      IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N ,IGRTRUSS ,
     3      IGRBEAM ,IGRSPRING ,IGRNOD  ,LSUBMODEL, SEATBELT_SHELL_TO_SPRING,
     4      NB_SEATBELT_SHELLS)
        CALL MY_ALLOC(NSTRF,SNSTRF,"NSTRF")
        CALL MY_ALLOC(SECBUF,SSECBUF,"SECBUF")
        NSTRF  = 0
        SECBUF = ZERO
        CALL LECSEC42(IXS    ,IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                IXTG   ,X      ,ITAB   ,ITABM1 ,
     3                IGRNOD   ,SECBUF ,
     4                IPARI  ,IXS10  ,IXS20  ,IXS16  ,UNITAB ,
     5                ISKWN  ,XFRAME ,ISOLNOD,NOM_SECT,RTRANS,
     6                LSUBMODEL,NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC,IGRQUAD,IGRSH4N,
     7                IGRTRUSS ,IGRBEAM,IGRSPRING,IGRSH3N,SEATBELT_SHELL_TO_SPRING,
     8                NB_SEATBELT_SHELLS)
       SNSTRF = SIZE(NSTRF)
      ELSE
        SNSTRF  = 0
        SSECBUF = 0
        CALL MY_ALLOC(NSTRF,SNSTRF,"NSTRF")
        CALL MY_ALLOC(SECBUF,SSECBUF,"SECBUF")
      ENDIF
      IF(ALLOCATED(SEATBELT_SHELL_TO_SPRING)) CALL MY_DEALLOC(SEATBELT_SHELL_TO_SPRING)
      CALL TRACE_OUT1()
C--------------------------------------------
C     SENSORS INITIALIZATION
C--------------------------------------------
c
      CALL INISEN(SENSORS ,IPARI,NOM_OPT,INOM_OPT(5),
     .            INOM_OPT(8),INOM_OPT(4),IXR ,R_SKEW ,NUMELR,
     .            NSECT   ,NINTER ,NINTSUB ,NRWALL   ,NRBODY  )
c
C----------------------------------
C     reading joints
C----------------------------------
      ERR_MSG='JOINTS'
      ERR_CATEGORY='JOINTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PRELECJOI(SLJOINT ,IGRNOD,LSUBMODEL)
      CALL MY_ALLOC(LJOINT,SLJOINT,"LJOINT")
      LJOINT  = 0

      allocate(CYL_JOIN(NJOINT))

      IF(NJOINT/=0)THEN
        WRITE(ISTDO,'(A)') ' .. CYLINDRICAL JOINTS'
        CALL INIT_JOINT(NJOINT)
        CALL HM_READ_CYLJOINT(LJOINT ,ITABM1 ,D      ,ITAB   ,IGRNOD ,
     .                        NOM_OPT(LNOPT1*INOM_OPT(7)+1),LSUBMODEL)
      ENDIF
c      CALL ANCHECK(83)
      CALL TRACE_OUT1()
C-------------------------------------------------
C     BLOCKING MAT 11 NODES OUTSIDE COMPUTED DOMAIN
C-------------------------------------------------
      ERR_MSG='BLOCK BOUNDARY MATERIAL NODES'
      ERR_CATEGORY='BLOCK BOUNDARY MATERIAL NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER /= 0 .AND. NUMELQ+NUMELS  > 0) THEN
        CALL NODM11(PM,IXS,IXQ,ICODE)
      ENDIF
      CALL TRACE_OUT1()
C-------------------------------------------------
C     LISTE DES NOEUDS CORRESPONDANT A ONE MILIEU POREUX
C-------------------------------------------------
      ERR_MSG='POROUS NODES'
      ERR_CATEGORY='POROUS NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER /=0 .AND. NUMELQ+NUMELS >0)THEN
        SIWORK = NUMNOD+4*NFACX
        CALL MY_ALLOC(IWORK,SIWORK,"IWORK")
        SIWORK = 0
        CALL PORNOD(GEO    ,IXS    ,IXQ    ,IWORK  ,ICODE  ,
     +              ITAB   ,NPBY   ,LPBY   ,IGEO)
        SNODPOR = NUMPOR
        CALL MY_ALLOC(NODPOR,SNODPOR,"NODPOR")
        NODPOR = IWORK(1:SNODPOR)
        CALL MY_DEALLOC(IWORK)
      ELSE
        SNODPOR = 0
        CALL MY_ALLOC(NODPOR,SNODPOR,"NODPOR")
      ENDIF
      CALL TRACE_OUT1()
C---
      IF(KCONTACT/=0)THEN
        KCONTACT=1
        SICONTACT=NUMNOD
        CALL MY_ALLOC(ICONTACT,SICONTACT,"ICONTACT")
        ICONTACT = 0
      ELSE
        CALL MY_ALLOC(ICONTACT,0,"ICONTACT")
      END IF
      IF(NADMESH/=0)THEN
        SRCONTACT=NUMNOD
        CALL MY_ALLOC(RCONTACT,SRCONTACT,"RCONTACT")
        RCONTACT = EP30
        CALL MY_ALLOC(ACONTACT,SRCONTACT,"ACONTACT")
        ACONTACT = EP30
        CALL MY_ALLOC(PCONTACT,SRCONTACT,"PCONTACT")
        PCONTACT = ZERO
      ELSE
        CALL MY_ALLOC(RCONTACT,0,"RCONTACT")
        CALL MY_ALLOC(ACONTACT,0,"ACONTACT")
        CALL MY_ALLOC(PCONTACT,0,"PCONTACT")
      END IF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      CALL FVDIM(T_MONVOL)
      allocate(FVDATA(NFVBAG))

C
      ERR_MSG='FVMBAG MESHING'
      ERR_CATEGORY='FVMBAG MESHING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(TETRAMESHER_USED) THEN
         CALL FVMESH0(T_MONVOL, NODE_COORD, IXS_TEMP, IXC, IXTG, PM,IPM, IGRSURF, XYZREF ,NB_TOTAL_NODE)
      ELSE
         CALL FVMESH0(T_MONVOL, X,          IXS,      IXC, IXTG, PM,IPM, IGRSURF, XYZREF ,NUMNOD)
      ENDIF

      CALL COPY_TO_VOLMON(T_MONVOL, LRCBAG, T_MONVOL_METADATA%RCBAG, SVOLMON, VOLMON)

      CALL COPY_TO_MONVOL(T_MONVOL, LICBAG, T_MONVOL_METADATA%ICBAG, SMONVOL, MONVOL)

      CALL TRACE_OUT1()
      ERR_MSG='BEM FLOW'
      ERR_CATEGORY='BEM FLOW'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C----------------------------------
C     reading flows (incompressible fluid or thermal)
C----------------------------------
      IF(NFLOW>0) THEN
         CALL HM_PREREAD_BEM(IGRSURF, IGRNOD , NNFT  ,
     .                 UNITAB , NOM_OPT(LNOPT1*INOM_OPT(12)+1), LSUBMODEL)
C
         call my_alloc(IFLOW, LIFLOW, "IFLOW")
         call my_alloc(RFLOW, LRFLOW, "RFLOW")
         IFLOW(1:LIFLOW) = 0
         RFLOW(1:LRFLOW) = ZERO
C
         DO I=1,NSPMD
            MEMFLOW(1,I)=0
            MEMFLOW(2,I)=0
         ENDDO
C
         CALL HM_READ_BEM(IGRSURF, IFLOW,   RFLOW,
     .                 NPC1   , IGRNOD , MEMFLOW(1,1),UNITAB,
     .                 X, NOM_OPT(LNOPT1*INOM_OPT(12)+1),LGAUGE, IGRV, LSUBMODEL,IRESP)
C
      ELSE
         call my_alloc(IFLOW, 0, "IFLOW")
         call my_alloc(RFLOW, 0, "RFLOW")
      ENDIF
      CALL TRACE_OUT1()
      ERR_MSG='EULERIAN BOUNDARY CONDITIONS'
      ERR_CATEGORY='EULERIAN BOUNDARY CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      IF(NEBCS > 0)THEN
         SEGINDX = 0
         !update due to domain decomposition
         CALL INIEBCS(ALE_CONNECTIVITY, 1,IGRSURF, IXS, IXQ, IXTG,
     .        PM, IGEO, X, SENSORS, MONVOl, MULTI_FVM%IS_USED, EBCS_TAB, EBCS_TAG_CELL_SPMD,ITAB)
         !initialization
        CALL INIEBCSP0(X, IPARG, ELBUF_TAB, EBCS_TAB, IXS, IXQ, IXTG, IPARTS, IPARTQ, IPARTG, PM, IPM, MAT_ELEM%MAT_PARAM)
        CALL INIEBCS_PROPELLANT(IXS,IXQ,IXTG,MULTI_FVM%IS_USED,EBCS_TAB,MAT_ELEM%MAT_PARAM,SIXS,SIXQ,SIXTG,NUMMAT)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     Multiplier Lagrange
C--------------------------------------------
      ERR_MSG='LAGRANGE MULTIPLIERS'
      ERR_CATEGORY='LAGRANGE MULTIPLIERS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NCMAX   = LAG_NCF + LAG_NCL
      NKMAX   = LAG_NKF + LAG_NKL
      LAG_NHL = LAG_NCL * 10

      IF( ALLOCATED(IADHF) )  CALL MY_DEALLOC(IADHF)
      IF( ALLOCATED(IADLL) )  CALL MY_DEALLOC(IADLL)
      IF( ALLOCATED(LLL)   )  CALL MY_DEALLOC(LLL)
      IF( ALLOCATED(JLL)   )  CALL MY_DEALLOC(JLL)
      CALL MY_ALLOC(IADHF,LAG_NCF + 1,"IADHF")
      CALL MY_ALLOC(IADLL,LAG_NCF + 1,"IADLL")
      CALL MY_ALLOC(LLL,LAG_NKF,"LLL")
      CALL MY_ALLOC(JLL,LAG_NKF,"JLL")
      IF(LAG_NCF > 0) THEN
C---
        CALL LAGM_INI(LAG_NHF ,IADHF   ,IADLL   ,JLL     ,LLL     ,
     2                IPARI   ,INTBUF_TAB,IGRNOD,         IBCSLAG ,
     3                MS      ,IN      ,GJBUFI  ,IBMPC   ,IBMPC2  ,
     4                IBMPC3  ,IBMPC4  ,IBFV    ,VEL     ,ITAB    ,
     5                NOM_OPT,INOM_OPT(3),INOM_OPT(15),INOM_OPT(16),
     6                INOM_OPT(17),INOM_OPT(18))
        CALL MY_ALLOC(JCIHF,LAG_NHF,"JCIHF")
        CALL LAGM_NHF(LAG_NCF, IADLL  ,JLL    ,LLL    ,JCIHF  )
C---
        SLAGBUF = LAG_NHF + 3*LAG_NCF+2
        CALL MY_ALLOC(LAGBUF,SLAGBUF,"LAGBUF")
        L1 = LAG_NCF + 1
        L2 = L1 + LAG_NHF
        L3 = L2 + LAG_NCF + 1
        L4 = L3 + LAG_NKF
        L5 = L4 + LAG_NKF
        LAGBUF = 0
        LAGBUF(1:L1)    = IADHF(1:LAG_NCF + 1)
        LAGBUF(L1+1:L2) = JCIHF(1:LAG_NHF)
        CALL MY_DEALLOC(JLL)
        CALL MY_DEALLOC(IADHF)
        CALL MY_DEALLOC(JCIHF)
        ELSE
        SLAGBUF = 0
        CALL MY_ALLOC(LAGBUF,SLAGBUF,"LAGBUF")
      ENDIF
C---
      IF(NRWLAG>0)
     .  CALL LGMINI_RWL(NPRW   , LPRW     , MS     , ITAB,
     .                  NOM_OPT(LNOPT1*INOM_OPT(5)+1))
      IF(NINTER>0)
     .  CALL LGMINI_I7(IPARI   ,INTBUF_TAB   , MS     , ITAB   , IGRNOD,
     .                 NOM_OPT(LNOPT1*INOM_OPT(3)+1))
      NHMAX = LAG_NHF + LAG_NHL
      LWAT  = 0
      IF(LAG_NCL/=0) LWAT  = MAX(6*(NUMELS16+NUMELS20),6*NUMNOD)
      L_MUL_LAG1 = 2*NCMAX + 4*NKMAX + LWAT +  2
      IF(NCMAX>0) THEN
          L_MUL_LAG = MAX(L_MUL_LAG1+NUMNOD,
     .                11*NCMAX + 4*NKMAX + 3*NHMAX + 6*NUMNOD + 2)
      ENDIF

      SLAMBDA = NCMAX
      CALL MY_ALLOC(LAMBDA,SLAMBDA,"LAMBDA")
      IF(SLAMBDA > 0)  LAMBDA = ZERO
      CALL TRACE_OUT1()
C----------------------------------
C     COMPUTATION OF MODAL GRAVITY FORCES
C----------------------------------
      ERR_MSG='GRAVITY NODAL FORCES'
      ERR_CATEGORY='GRAVITY NODAL FORCES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFXBODY>0) THEN
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NLGRAV=FXBIPM(AIPM+25)
            AGRVI=FXBIPM(AIPM+26)
            AGRVR=FXBIPM(AIPM+27)
            AMOD=FXBIPM(AIPM+7)
            IF(NLGRAV>0)
     .         CALL FXBGRAV(
     .               IGRV   , LGRAV  , FXBIPM(AIPM+18), FXBNOD(ANOD),
     .               FXBGRVI(AGRVI), FXBGRVR(AGRVR), FXBIPM(AIPM+3), FXBMOD(AMOD),
     .               FXBIPM(AIPM+4), FXBIPM(AIPM+17), MS     , GRAV   ,
     .               SKEW          , FXBIPM(AIPM+29), NFX    , FXBIPM(AIPM+30))
         ENDDO
      ENDIF
      CALL TRACE_OUT1()

C--------------------------------------------
C     THPARTS TREADING
C--------------------------------------------
      ERR_MSG='TIME HISTORY PARTS'
      ERR_CATEGORY='TIME HISTORY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NTHPART >0) THEN
        CALL HM_READ_THPART(IPART    ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N,
     .                      IGRTRUSS ,IGRBEAM ,IGRSPRING, LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()

      CALL CREATE_MAP_TABLES ( MAP_TABLES ,2     ,
     *                         LSUBMODEL ,SUBSETS,
     *                         IPART,
     *                         IXS  ,IXQ  ,IXC   ,IXTG ,
     *                         IXT  ,IXP  ,IXR   ,KXSP,LRIVET,
     *                         IBID )
C----------------------------------
C     TH GROUP READING
C----------------------------------
      ERR_MSG='TIME HISTORY GROUPS'
      ERR_CATEGORY='TIME HISTORY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      ! Number of /TH read by hm reader
      CALL HM_OPTION_COUNT('/TH' ,NTHGRP0)
      CALL HM_OPTION_COUNT('/ATH',NTHGRP01(1))
      CALL HM_OPTION_COUNT('/BTH',NTHGRP01(2))
      CALL HM_OPTION_COUNT('/CTH',NTHGRP01(3))
      CALL HM_OPTION_COUNT('/DTH',NTHGRP01(4))
      CALL HM_OPTION_COUNT('/ETH',NTHGRP01(5))
      CALL HM_OPTION_COUNT('/FTH',NTHGRP01(6))
      CALL HM_OPTION_COUNT('/GTH',NTHGRP01(7))
      CALL HM_OPTION_COUNT('/HTH',NTHGRP01(8))
      CALL HM_OPTION_COUNT('/ITH',NTHGRP01(9))
      DO I=1,9
        NTHGRPMX = MAX(NTHGRP0,NTHGRP01(I))
      ENDDO
c
      ! Number of /TH/MONV
      NBR_TH_MONVOL        = 0
      NBR_TH_MONVOL01(1:9) = 0
      CALL HM_OPTION_COUNT('/TH/MONV' ,NBR_TH_MONVOL)
      CALL HM_OPTION_COUNT('/ATH/MONV',NBR_TH_MONVOL01(1))
      CALL HM_OPTION_COUNT('/BTH/MONV',NBR_TH_MONVOL01(2))
      CALL HM_OPTION_COUNT('/CTH/MONV',NBR_TH_MONVOL01(3))
      CALL HM_OPTION_COUNT('/DTH/MONV',NBR_TH_MONVOL01(4))
      CALL HM_OPTION_COUNT('/ETH/MONV',NBR_TH_MONVOL01(5))
      CALL HM_OPTION_COUNT('/FTH/MONV',NBR_TH_MONVOL01(6))
      CALL HM_OPTION_COUNT('/GTH/MONV',NBR_TH_MONVOL01(7))
      CALL HM_OPTION_COUNT('/HTH/MONV',NBR_TH_MONVOL01(8))
      CALL HM_OPTION_COUNT('/ITH/MONV',NBR_TH_MONVOL01(9))
      DO I=1,9
        NBR_TH_MONVOL = MAX(NBR_TH_MONVOL,NBR_TH_MONVOL01(I))
      ENDDO
c
      OUTPUT%TH%SITHGRP = (NTHGRP0+NBR_TH_MONVOL)*NITHGR
      LITHPART  = NTHGRPMX*(NPART+NTHPART)
      LITHSUB   = NTHGRPMX*NSUBS
      LITHBUFMX = 0
      LITHBUFI  = 0
      NVARTOT   = 0
      NVARTOT0  = 0
c
      ! New routine to pre-read /TH with hm_reader and old reader
      ! (needed to estimate sizes of buffers)
      CALL HM_READ_PRETHGROU(LITHBUFMX,NVARTOT0,LSUBMODEL,0,OUTPUT)
      DO I=1,9
        CALL HM_READ_PRETHGROU(LITHBUFI,NVARTOT,LSUBMODEL,I,OUTPUT)
        LITHBUFMX  = MAX(LITHBUFMX,LITHBUFI,NVARTOT,NVARTOT0)
        NVARTOTMAX = MAX(NVARTOTMAX,NVARTOT,NVARTOT0)
      ENDDO
c
      SITHVAR = NVARTOT0*10+NVARTOT*10+NVOLU*10
      CALL MY_ALLOC(ITHPART,LITHPART,"ITHPART")
      CALL MY_ALLOC(ITHSUB,LITHSUB,"ITHSUB")
      CALL MY_ALLOC(ITHBUFTMP,LITHBUFMX,"ITHBUFTMP")
      CALL MY_ALLOC(ITHVAR,SITHVAR,"ITHVAR")
      IF(SITHVAR > 0) ITHVAR(1:SITHVAR) = 0
      CALL MY_ALLOC(OUTPUT%TH%ITHGRP,OUTPUT%TH%SITHGRP,"OUTPUT%TH%ITHGRP")
c
      OUTPUT%TH%ITHGRP(1:OUTPUT%TH%SITHGRP)    = 0
      ITHPART   = 0
      ITHSUB    = 0
      ITHBUFTMP = 0
      OUTPUT%TH%SITHBUF = 0
      ITHFLAG   = 10
c-----
      INTERFACES%PARAMETERS%INTCAREA =0
      ! New routine to read /TH with hm_reader and old reader
      CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRP   ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUF,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP   ,ITHPART  ,ITHSUB   ,FXBIPM   ,IPART   ,LIPART1  ,
     6       8        ,12       ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 0,INOM_OPT(31),INOM_OPT(32),SENSORS   ,
     F       INTERFACES,IPARI   ,OUTPUT%TH%DUMP_THNMS1_FILE,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
      CALL MY_ALLOC(OUTPUT%TH%ITHBUF,OUTPUT%TH%SITHBUF,"OUTPUT%TH%ITHBUF")
      OUTPUT%TH%ITHBUF(1:OUTPUT%TH%SITHBUF) = ITHBUFTMP(1:OUTPUT%TH%SITHBUF)

      If (LITHBUFMX < OUTPUT%TH%SITHBUF) then
        print*,'Allocation error :LITHBUFMX, SITHBUF=',LITHBUFMX,OUTPUT%TH%SITHBUF
      endif
      CALL MY_ALLOC(RTHBUF,SRTHBUF,"RTHBUF")
      IF(SRTHBUF > 0) CALL THSKEWC(
     1       RTHBUF   ,OUTPUT%TH%ITHGRP ,OUTPUT%TH%ITHBUF,X     ,IXC     ,IXTG   ,SKEW,NTHGRP)

C--------
C ithgrpa
C--------
      IF(NTHGRP01(1) > 0) THEN
       OUTPUT%TH%SITHGRPA = (NTHGRP01(1)+NBR_TH_MONVOL)*NITHGR
       CALL MY_ALLOC(OUTPUT%TH%ITHGRPA,OUTPUT%TH%SITHGRPA,"OUTPUT%TH%ITHGRPA")
       OUTPUT%TH%ITHGRPA   = 0
       ITHPART   = 0
       ITHSUB    = 0
       ITHBUFTMP = 0
       OUTPUT%TH%SITHBUFA  = 0
       ITHFLAG   = 1
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1:2*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPA  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFA ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(1),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 1,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFA,OUTPUT%TH%SITHBUFA,"OUTPUT%TH%ITHBUFA")
        OUTPUT%TH%ITHBUFA = ITHBUFTMP(1:OUTPUT%TH%SITHBUFA)
      ELSE
            OUTPUT%TH%SITHGRPA = 0
            OUTPUT%TH%SITHBUFA = 0
            CALL MY_ALLOC(OUTPUT%TH%ITHGRPA,OUTPUT%TH%SITHGRPA,"OUTPUT%TH%ITHGRPA")
            CALL MY_ALLOC(OUTPUT%TH%ITHBUFA,OUTPUT%TH%SITHBUFA,"OUTPUT%TH%ITHBUFA")
      ENDIF
C--------
C ithgrpb
C--------
      IF(NTHGRP01(2) > 0) THEN
        OUTPUT%TH%SITHGRPB = (NTHGRP01(2)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPB,OUTPUT%TH%SITHGRPB,"OUTPUT%TH%ITHGRPB")
        OUTPUT%TH%ITHGRPB   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFB  = 0
        ITHFLAG   = 2
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+2*(NPART+NTHPART):4*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPB  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFB ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(2),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 2,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFB,OUTPUT%TH%SITHBUFB,"OUTPUT%TH%ITHBUFB")
        OUTPUT%TH%ITHBUFB = ITHBUFTMP(1:OUTPUT%TH%SITHBUFB)
      ELSE
            OUTPUT%TH%SITHGRPB = 0
            OUTPUT%TH%SITHBUFB = 0
            CALL MY_ALLOC(OUTPUT%TH%ITHGRPB,OUTPUT%TH%SITHGRPB,"OUTPUT%TH%ITHGRPB")
            CALL MY_ALLOC(OUTPUT%TH%ITHBUFB,OUTPUT%TH%SITHBUFB,"OUTPUT%TH%ITHBUFB")
      ENDIF
C--------
C ithgrpc
C--------
      IF(NTHGRP01(3) > 0) THEN
        OUTPUT%TH%SITHGRPC = (NTHGRP01(3)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPC,OUTPUT%TH%SITHGRPC,"OUTPUT%TH%ITHGRPC")
        OUTPUT%TH%ITHGRPC   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFC  = 0
        ITHFLAG   = 3
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+4*(NPART+NTHPART):6*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPC  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFC ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(3),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 3,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFC, OUTPUT%TH%SITHBUFC,"OUTPUT%TH%ITHBUFC")
        OUTPUT%TH%ITHBUFC(1:OUTPUT%TH%SITHBUFC) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFC)
      ELSE
        OUTPUT%TH%SITHGRPC = 0
        OUTPUT%TH%SITHBUFC = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFC, OUTPUT%TH%SITHBUFC,"OUTPUT%TH%ITHBUFC")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPC, OUTPUT%TH%SITHGRPC,"OUTPUT%TH%ITHGRPC")
      ENDIF
C--------
C ithgrpd
C--------
      IF(NTHGRP01(4) > 0) THEN
        OUTPUT%TH%SITHGRPD = (NTHGRP01(4)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPD,OUTPUT%TH%SITHGRPD,"OUTPUT%TH%ITHGRPD")
        OUTPUT%TH%ITHGRPD   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFD  = 0
        ITHFLAG   = 4
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+6*(NPART+NTHPART):8*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPD  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFD ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(4),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 4,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFD,OUTPUT%TH%SITHBUFD,"OUTPUT%TH%ITHBUFD")
        OUTPUT%TH%ITHBUFD(1:OUTPUT%TH%SITHBUFD) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFD)
      ELSE
        OUTPUT%TH%SITHGRPD = 0
        OUTPUT%TH%SITHBUFD = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFD,OUTPUT%TH%SITHBUFD,"OUTPUT%TH%ITHBUFD")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPD,OUTPUT%TH%SITHGRPD,"OUTPUT%TH%ITHGRPD")
      ENDIF
C--------
C ithgrpe
C--------
      IF(NTHGRP01(5) > 0) THEN
        OUTPUT%TH%SITHGRPE = (NTHGRP01(5)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPE,OUTPUT%TH%SITHGRPE,"OUTPUT%TH%ITHGRPE")
        OUTPUT%TH%ITHGRPE   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFE  = 0
        ITHFLAG   = 5
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+8*(NPART+NTHPART):10*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
c-----
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPE  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFE ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(5),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 5,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFE,OUTPUT%TH%SITHBUFE,"OUTPUT%TH%ITHBUFE")
        OUTPUT%TH%ITHBUFE = ITHBUFTMP(1:OUTPUT%TH%SITHBUFE)
      ELSE
        OUTPUT%TH%SITHGRPE = 0
        OUTPUT%TH%SITHBUFE = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFE,OUTPUT%TH%SITHBUFE,"OUTPUT%TH%ITHBUFE")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPE,OUTPUT%TH%SITHGRPE,"OUTPUT%TH%ITHGRPE")
      ENDIF
C--------
C ithgrpf
C--------
      IF(NTHGRP01(6) > 0) THEN
        OUTPUT%TH%SITHGRPF = (NTHGRP01(6)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPF,OUTPUT%TH%SITHGRPF,"OUTPUT%TH%ITHGRPF")
        OUTPUT%TH%ITHGRPF   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFF  = 0
        ITHFLAG   = 6
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+10*(NPART+NTHPART):12*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPF  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFF ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(6),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 6,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFF,OUTPUT%TH%SITHBUFF,"OUTPUT%TH%ITHBUFF")
        OUTPUT%TH%ITHBUFF(1:OUTPUT%TH%SITHBUFF) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFF)
      ELSE
        OUTPUT%TH%SITHGRPF = 0
        OUTPUT%TH%SITHBUFF = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFF,OUTPUT%TH%SITHBUFF,"OUTPUT%TH%ITHBUFF")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPF,OUTPUT%TH%SITHGRPF,"OUTPUT%TH%ITHGRPF")
      ENDIF
C--------
C ithgrpg
C--------
      IF(NTHGRP01(7) > 0) THEN
        OUTPUT%TH%SITHGRPG = (NTHGRP01(7)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPG,OUTPUT%TH%SITHGRPG,"OUTPUT%TH%ITHGRPG")
        OUTPUT%TH%ITHGRPG = 0
        OUTPUT%TH%SITHBUFG = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        ITHFLAG   = 7
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+12*(NPART+NTHPART):14*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPG  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFG ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(7),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 7,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFG,OUTPUT%TH%SITHBUFG,"OUTPUT%TH%ITHBUFG")
        OUTPUT%TH%ITHBUFG(1:OUTPUT%TH%SITHBUFG) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFG)
      ELSE
        OUTPUT%TH%SITHGRPG = 0
        OUTPUT%TH%SITHBUFG = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFG,OUTPUT%TH%SITHBUFG,"OUTPUT%TH%ITHBUFG")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPG,OUTPUT%TH%SITHGRPG,"OUTPUT%TH%ITHGRPG")
      ENDIF
C--------
C ithgrph
C--------
      IF(NTHGRP01(8) > 0) THEN
        OUTPUT%TH%SITHGRPH = (NTHGRP01(8)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPH,OUTPUT%TH%SITHGRPH,"OUTPUT%TH%ITHGRPH")
        OUTPUT%TH%ITHGRPH   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFH  = 0
        ITHFLAG   = 8
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+14*(NPART+NTHPART):16*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPH  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFH ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(8),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 8,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFH,OUTPUT%TH%SITHBUFH,"OUTPUT%TH%ITHBUFH")
        OUTPUT%TH%ITHBUFH(1:OUTPUT%TH%SITHBUFH) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFH)
      ELSE
        OUTPUT%TH%SITHGRPH = 0
        OUTPUT%TH%SITHBUFH = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFH,OUTPUT%TH%SITHBUFH,"OUTPUT%TH%ITHBUFH")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPH,OUTPUT%TH%SITHGRPH,"OUTPUT%TH%ITHGRPH")
      ENDIF
C--------
C ithgrpi
C--------
      IF(NTHGRP01(9) > 0) THEN
        OUTPUT%TH%SITHGRPI = (NTHGRP01(9)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPI,OUTPUT%TH%SITHGRPI,"OUTPUT%TH%ITHGRPI")
        OUTPUT%TH%ITHGRPI   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFI  = 0
        ITHFLAG   = 9
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+16*(NPART+NTHPART):18*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPI  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFI ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(9),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 9,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   ,0,GLOB_THERM%ITHERM_FE,OUTPUT%CHECKSUM,
     G       NSUBDOM,IPRI)
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFI,OUTPUT%TH%SITHBUFI,"OUTPUT%TH%ITHBUFI")
        OUTPUT%TH%ITHBUFI = ITHBUFTMP(1:OUTPUT%TH%SITHBUFI)
      ELSE
        OUTPUT%TH%SITHGRPI = 0
        OUTPUT%TH%SITHBUFI = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFI,OUTPUT%TH%SITHBUFI,"OUTPUT%TH%ITHBUFI")
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPI,OUTPUT%TH%SITHGRPI,"OUTPUT%TH%ITHGRPI")
      ENDIF
C

      IF(ALLOCATED(ITHPART))   CALL MY_DEALLOC(ITHPART)
      IF(ALLOCATED(ITHSUB))    CALL MY_DEALLOC(ITHSUB)
      IF(ALLOCATED(ITHBUFTMP)) CALL MY_DEALLOC(ITHBUFTMP)
C
C -------------------------------------------------
C /TH/SURF : outputting Pressure and Area : 
C -------------------------------------------------
      CALL TH_SURF_LOAD_PRESSURE(IGRSURF  , OUTPUT%TH%TH_SURF , IPRES  ,ILOADP  ,LLOADP  ,
     .                           SIZLOADP ,NLOADP             ,SLLOADP ,NIBCLD  ,NPRELD  ,
     .                           NSURF    ,NUMNOD   )
C
C------------------------- CHECK -------------------
Cthe nsmat variable (number of materials in th)
C     is available only after thgrou in block version
c      CALL ANCHECK(4)
C
c      CALL ANCHECK(8)
C
c      CALL ANCHECK(11)
C
c      CALL ANCHECK(12)
C
c      CALL ANCHECK(16)
C
c      CALL ANCHECK(18)
C
c      CALL ANCHECK(20)
C
c      CALL ANCHECK(24)
C
c      CALL ANCHECK(27)
C
c      CALL ANCHECK(31)
C
c      CALL ANCHECK(34)
C
c      CALL ANCHECK(38)
C
c      CALL ANCHECK(48)
C
c      CALL ANCHECK(45)
C
c      CALL ANCHECK(50)
C
c      CALL ANCHECK(55)
C
c      CALL ANCHECK(57)
C
c      CALL ANCHECK(58)
C
c      CALL ANCHECK(59)
C
c      CALL ANCHECK(61)
C
c      CALL ANCHECK(80)
C
C
C assignment of processor number (spmd)
C
      CALL THPINIT(OUTPUT%TH%ITHGRP,OUTPUT%TH%ITHBUF,IPARG  ,DD_IAD ,LRIVET ,
     .             0       ,NTHGRP )
      IF(NTHGRP01(1) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPA,OUTPUT%TH%ITHBUFA,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(1)   )
      IF(NTHGRP01(2) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPB,OUTPUT%TH%ITHBUFB,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(2)   )
      IF(NTHGRP01(3) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPC,OUTPUT%TH%ITHBUFC,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(3)   )
      IF(NTHGRP01(4) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPD,OUTPUT%TH%ITHBUFD,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(4)   )
      IF(NTHGRP01(5) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPE,OUTPUT%TH%ITHBUFE,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(5)   )
      IF(NTHGRP01(6) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPF,OUTPUT%TH%ITHBUFF,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(6)   )
      IF(NTHGRP01(7) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPG,OUTPUT%TH%ITHBUFG,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(7)   )
      IF(NTHGRP01(8) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPH,OUTPUT%TH%ITHBUFH,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(8)   )
      IF(NTHGRP01(9) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPI,OUTPUT%TH%ITHBUFI,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(9)   )
      CALL TRACE_OUT1()
C--------------------------------------------
C     multidomains -> deallocation of arrays
C--------------------------------------------
      IF(NSUBDOM>0) THEN
        CALL MY_DEALLOC(TAG_PART)
        DEALLOCATE(TAGMON, TAGRBY, TAGINT, TAGCYL, TAGLNK)
        DEALLOCATE(TAGRB3,TAGRB2,TAGJOIN,TAGMPC,TAG_MAT,TAG_ALELINK)
      ENDIF
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (CONNEXIONS RIGIDES a MADYMO).
C--------------------------------------------
      ERR_MSG='DYNAMIC STORAGE MADYMO LINK'
      ERR_CATEGORY='DYNAMIC STORAGE MADYMO LINK'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SRCONX = NCONX*NRCNX
      CALL MY_ALLOC(RCONX,SRCONX,"RCONX")
      RCONX = ZERO
C--------------------------------------------
C     RECOMPUTATION OF NRBODY (NRBYKIN UPDATED UPSTREAM)
C---------------------------------------------------------------------
      IF(NRBMERGE > 0) THEN
        NRBODY = NRBYKIN + NRBYLAG
      ENDIF
C--------------------------------------------
C     TABLEAU DE TRAVAIL WA(LENWA)
C     part not saved in the restart file
C--------------------------------------------
      NRCVVOIS0 = 0
C call to generic routine here and in ddsplit
      CALL SETLENWA(
     1      LENWA ,NTHWA  ,NAIRWA    ,NUMELS   ,NUMELQ,
     2      NUMELC,NUMELTG,NUMELT    ,NUMELP   ,NUMELR,
     3      NUMNOD,NMNT   ,L_MUL_LAG1,L_MUL_LAG,MAXNX ,
     4      LWASPH,NUMSPH    ,LWASPIO, NRCVVOIS0,NGROUP,
     5      LWAMP_L,LWANMP_L ,GLOB_THERM%ITHERM)
      LWAMP  = LWAMP_L   !< Copy values in common
      LWANMP = LWANMP_L  !< Copy values in common
C init MULTIMAX
      CALL MY_ALLOC(MWA,LENWA,"MWA")
      MWA = ZERO
      CALL SETMULTI(IPARI  )
      CALL TRACE_OUT1()
C---------------------------------------------------------------------
C      INITIALIZATION OF INTERFACES SECOND PART
C     INIT INTERFACE TYPE 6 + BUCKET SORT TYPE 4 STATISTICS
C     PUT INTERFACE NODES INTO FRONTIERE(1)
C---------------------------------------------------------------------
      ERR_MSG='INTERFACE INITIALIZATION PHASE 2'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      I2NSNT = 0
C     Mass and inertia are not modified - a specific array is used
      CALL MY_ALLOC(MS_B,NUMNOD,"MS_B")
      MS_B(1:NUMNOD)=MS(1:NUMNOD)
      IF(IRODDL==1) THEN
        CALL MY_ALLOC(IN_B,NUMNOD,"IN_B")
        IN_B(1:NUMNOD)=IN(1:NUMNOD)
      ELSE
        CALL MY_ALLOC(IN_B,1,"IN_B")
      ENDIF
C
      IF(NS10E>0.AND.N2D==0) CALL STIFN0_ND(ICNDS10,STIFFN)
      IF(NINTER > 0) THEN
       CALL ININTR2(IPARI   ,INSCR   ,X       ,
     .               IXS     ,IXQ     ,IXC     ,PM      ,GEO     ,
     .               INSCR   ,ITAB    ,MS      ,NPBY    ,LPBY    ,
     .               MWA     ,D       ,I2NSNT  ,IN      ,
     .               STIFFN,STIFINT ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),INOD_PXFEM ,MS_PLY,
     .               INTBUF_TAB,STIFINTR,ITAGND,ICNDS10,MS_B,IN_B,NSTRF,ITAGCYC,
     .               IRBE2   ,IRBE3  ,LRBE3    ,
     .               KNOD2ELS ,NOD2ELS , IXS10   ,IXS16  ,IXS20,
     .               S_NOD2ELS )    
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------
C     Update of STIFFN for TETRA10 for time step estimation
C--------------------------------------------
      IF(NS10E>0.AND.N2D==0) CALL STIFN1_ND(ICNDS10,STIFFN)
      IF(NDAMP>0) CALL DAMPDTNODA(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),
     1                            IGRNOD,DAMPR )
C--------------------------------------------
C     sorting and printing of element dt
C--------------------------------------------
      CALL OUTRI(DTELEM,IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,
     .           KXX,KXSP,KXIG3D,IGEO,NUMEL)
C--------------------------------------------
C     sorting and printing of nodal dt
C--------------------------------------------
      CALL OUTRIN(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),ITAB,DTNODA)
C---------------------------------------------
C     Target time step estimation - (type2 effect on nodal time step is taken into account in ININTR2)
C--------------------------------------------
      ERR_MSG='ADDED MASS ESTIMATION'
      ERR_CATEGORY='ADDED MASS ESTIMATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL ADD_MASS_STAT(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),ITAB,TOTMAS,TOTMAS1)
C---------------------------------
C     MULTIDOMAINS SPEEDUP ESTIMATION
C---------------------------------
      IF(NSUBDOM>0) THEN
        CALL R2R_SPEEDUP(DTELEM,DTNODA,DT_R2R,COST_R2R,ISOLOFF,
     .                   ISHEOFF,ITRUOFF ,IPOUOFF ,IRESOFF ,ITRIOFF,
     .                   IQUAOFF)
      ENDIF
C--------------------------------------------
      CALL MY_DEALLOC(STIFINT)
      CALL MY_DEALLOC(STIFINTR)
      CALL MY_DEALLOC(MS_B)
      CALL MY_DEALLOC(IN_B)
      CALL MY_DEALLOC(DTELEM)
      CALL TRACE_OUT1()
C--------------------------------------------
C     INI & CHECK RBE3
C--------------------------------------------
      ERR_MSG='RBE3 INITIALIZATION'
      ERR_CATEGORY='RBE3'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(SIRBE3 > 0) THEN
        CALL INIRBE3(IRBE3  ,LRBE3  ,FRBE3  ,SKEW   ,X     ,
     .               MS     ,IN     ,NOM_OPT(LNOPT1*INOM_OPT(14)+1),
     .               STIFFN,STIFFN(NUMNOD+1),ICODE,ITAB )
      ENDIF
      CALL MY_DEALLOC(STIFFN)
      CALL TRACE_OUT1()
C--------------------------------------------
      ERR_MSG='KINEMATIC CONDITIONS CHECK'
      ERR_CATEGORY='KINEMATIC CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C--------------------------------------------
C     traitement for 2nd pass /RBODY/RBE2 /BCS /IMPVEL w/ Itet2 of S10
C--------------------------------------------
      IF(NS10E>0) THEN
       CALL RIGMODIF1_ND(NPBY,LPBY,ITAGND)
       CALL RBE2MODIF1_ND(IRBE2,LRBE2,ITAGND)
       CALL BCSMODIF_ND(ICODE, ITAGND,ICNDS10,ITAB,
     .                  NNPBY,SLRBODY,NRBE2L ,SLRBE2,
     .                  NPBY ,LPBY   ,IRBE2  ,LRBE2 )
       CALL FIXMODIF_ND(IBFV , ITAGND,ICNDS10,ITAB)
       CALL BCSCYCMODIF_ND(IBCSCYC,LBCSCYC,ITAGND,ITAB)
      END IF
C--------------------------------------------
C     checking kinematic conditions
C--------------------------------------------
C d(3,numnod) used in the starter as a flag
C DE CONDITION CINEMATIQUE IKINE(NUMNOD)
C--------------------------------------------
      CALL KINCHK(D       ,RWBUF   ,ITAB   ,NPRW   ,LPRW    ,KINET  ,
     .            NPBY    , LPBY   ,IRBE2  ,LRBE2  ,IRBE3   ,LRBE3  ,
     .            NOM_OPT ,INOM_OPT(5),INOM_OPT(13),INOM_OPT(14)    ,
     .            ITAGCYC )
      IF(NINVEL/=0)
     . CALL INIVCHK(D  ,RWBUF,ITAB,NPRW,LPRW,KINET,
     1              NPBY, LPBY,IRBE2,LRBE2,IRBE3,LRBE3,
     2              FRBE3,X   ,SKEW ,V   ,VR   )
#ifdef DNC
      IF(NEXMAD/=0)
     .  CALL MADCHK(D      ,ITAB   ,ICONX(7*NCONX+1))
#endif
      CALL TRACE_OUT1()
C--------------------------------------------
C     Initial mass
C--------------------------------------------
      ERR_MSG='MASS ARRAY ALLOCATION'
      ERR_CATEGORY='INIIAL MASS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL MY_ALLOC(MS0,NUMNOD,"MS0")
      MS0(1:NUMNOD)=MS(1:NUMNOD)
      CALL TRACE_OUT1()
C--------------------------------------------
C     Inlet / Outlet
C--------------------------------------------
c build structure surfaces specific Inlet Outlet
      IF(NSPHIO > 0)THEN
         SIBUFSSG_IO = 4*NSEG_IO
         CALL MY_ALLOC(IBUFSSG_IO,SIBUFSSG_IO,"IBUFSSG_IO")
         IBUFSSG_IO(1:SIBUFSSG_IO) = 0
         CALL SET_IBUFSSG_IO(ISPHIO, IGRSURF, IBUFSSG_IO)
      ELSE
         CALL MY_ALLOC(IBUFSSG_IO,1,"IBUFSSG_IO")
         SIBUFSSG_IO = 0
      ENDIF

!--------------------------------------------
!     Split the surface & add the MONVOL nodes
!     on a given processor
!--------------------------------------------
      CALL PYTHON_DUPLICATE_NODES(ITAB,NUMNOD,NSPMD) ! nodes used in the python functions must be on all processors
      CALL IGRSURF_SPLIT(SCEP,CEP,T_MONVOL,IGRSURF,IGRSURF_PROC)
C--------------------------------------------
C     DOMAIN DECOMPOSITION 2 (DEFINITION DES FRONTIERES)
C--------------------------------------------
      ERR_MSG='DOMAIN DECOMPOSITION PHASE 2'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        SFR_IAD = (NSPMD+1)*2
        CALL MY_ALLOC(FR_IAD,SFR_IAD,"FR_IAD")
        IF(I2NSNT>0) THEN
          CALL MY_ALLOC(CELI2,I2NSNT,"CELI2")
          CALL MY_ALLOC(CEPI2,I2NSNT,"CEPI2")
          ALLOCATE(ADDCNI2(0:NUMNOD+1))
        ENDIF
        CALL MY_ALLOC(ISKWP,NUMSKW+1,"ISKWP")
        CALL MY_ALLOC(NSKWP,NSPMD,"NSKWP")
        CALL MY_ALLOC(ISENSP,2*SENSORS%NSENSOR,"ISENSP")
        CALL MY_ALLOC(NSENSP,NSPMD,"NSENSP")
        CALL MY_ALLOC(IACCP,NACCELM,"IACCP")
        CALL MY_ALLOC(NACCP,NSPMD,"NACCP")
        CALL MY_ALLOC(IGAUP,NBGAUGE,"IGAUP")
        CALL MY_ALLOC(NGAUP,NSPMD,"NGAUP")
        CALL MY_ALLOC(TAG_SKN,NUMSKW+NSUBMOD+1,"TAG_SKN")
        allocate(SKEWS%MULTIPLE_SKEW(NUMSKW+1))
        ISKWP(1:NUMSKW+1) = 0
        TAG_SKN(1:NUMSKW+NSUBMOD+1) = 0
        NSKWP(1:NSPMD) = 0
C
        IF(.NOT. ALLOCATED(IBVEL)) ALLOCATE(IBVEL(0)) ! Deprecated option?
        IF(.NOT. ALLOCATED(LBVEL)) ALLOCATE(LBVEL(0)) ! Deprecated option?

        CALL DOMDEC2(
     1    DD_IAD ,IPARI  ,IBCL    ,NPBY      ,
     2    LPBY   ,LRIVET ,IBVEL   ,LBVEL     ,
     3    IPARG  ,CEL     ,IXS    ,IXS10     ,IXS20   ,
     4    IXS16  ,IXQ     ,IXC    ,IXT       ,IXP     ,
     5    IXR    ,IXTG   ,IXTG1     ,T_MONVOL  ,
     6    IGRSURF,ADDCNE ,LCNE      ,GEO     ,
     7    NPRW   ,LPRW    ,LCNI2G ,ADDCNI2   ,CEPI2   ,
     8    CELI2  ,I2NSNT  ,ISKWN  ,ISKWP     ,NSKWP   ,
     9    ISENSP  ,NSENSP ,IACCP  ,NACCP     ,
     A    LACCELM,IBCV    ,IRBE3  ,LRBE3     ,FRONT_RM,
     B    IRBYM  ,LNRBYM  ,CEP    ,IBCR      ,IRBE2   ,
     C    LRBE2  ,CEPSP   ,CELSPH ,ILOADP    ,LLOADP  ,
     D    LGAUGE ,IGAUP   ,NGAUP  ,INTBUF_TAB,IBFFLUX ,
     E    ICNDS10,ITAGND  ,IGEO   ,TAG_SKN   ,SKEWS%MULTIPLE_SKEW,
     F    IBFV   ,IBCSCYC ,LBCSCYC,R_SKEW    ,IPM,
     G    SENSORS,SCEP    ,EBCS_TAB,LOADS,IFRAME,
     H    GLOB_THERM%NICONV ,GLOB_THERM%NIRADIA ,GLOB_THERM%NITFLUX,
     I    GLOB_THERM%NUMCONV,GLOB_THERM%NUMRADIA,GLOB_THERM%NFXFLUX,
     J    SENSOR_USER_STRUCT,bcs)

C      ELSE
C       set front to 1 i.e. all nodes on proc1, weight 1
C In SMP Front => Weight has the same address initialized at 1
C        SFR_IAD = 0
C        SDD_FRONT = 0
C        ALLOCATE(FR_IAD(SFR_IAD))
C        ALLOCATE(DD_FRONT(SDD_FRONT))
C        SWEIGHT = NUMNOD
C        ALLOCATE(WEIGHT(SWEIGHT))
C        WEIGHT = 1
C      ENDIF
C--------------------------------------------
C     traitement for DOMDEC, P/ON w/ Itet2 of S10
C--------------------------------------------
      IF(NS10E>0) THEN
       IF(IPARI0/=0) THEN
        CALL MY_ALLOC(CELCND,NS10E,"CELCND")
        CALL MY_ALLOC(CEPCND,NS10E,"CEPCND")
        ALLOCATE(ADDCNCND(0:NUMNOD+1))
        CALL PRE_CNDPON(ICNDS10,ADDCNCND,CEPCND,CELCND ,ITAGND )
        LCNCND = ADDCNCND(NUMNOD+1)-ADDCNCND(1)
        IF(LCNCND>0) THEN
         CALL MY_ALLOC(CNCND,LCNCND,"CNCND")
         CNCND(1:LCNCND)=0
         CALL FILLCNCND(CNCND  ,ADDCNCND,ICNDS10,ITAGND)
        END IF
       END IF
      END IF
C--------------------------------------------
C     traitement for hierarchy_rbody
C--------------------------------------------
      IF(NRBYKIN > 0) THEN
        call hierarchy_rbody_ddm(NRBYKIN ,NNPBY ,NPBY  ,SLRBODY ,LPBY  ,
     .                           NUMNOD  ,NSPMD )
      ENDIF
C   preparation of spmd processing of flows by bem
      CALL TRACE_OUT1()
      ERR_MSG='PROCESS BEM FOR SPMD'
      ERR_CATEGORY='PROCESS BEM FOR SPMD'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFLOW>0 .AND. NSPMD > 1) CALL FLOWDEC(IFLOW)
      CALL TRACE_OUT1()
C--------------------------------------------
C     FERMETURE DU FICHIER INPUT TMP
C--------------------------------------------
      ERR_MSG='CLOSING TMP INPUT FILE'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IPID/=0) CLOSE (UNIT=IIN)
      CALL TRACE_OUT1()
C--------------------------------------------
C     ECRITURE FICHIER DESSIN
C--------------------------------------------
      ERR_MSG='ANIMATION FILE WRITING'
      ERR_CATEGORY='ANIMATION FILE WRITING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      WRITE(ISTDO,'(A)')TITRE(46)
      IF(IOUTPUT>0) CALL DESOUT(
     .         X      ,IXS    ,IXQ    ,IXC    ,IXT    ,
     .         IXP    ,IXR    ,IXTG   ,ITAB   ,PM     ,
     .         GEO    ,MS     ,IXS10  ,IGEO  ,IPM    ,
     .         KXSP    ,IPART  ,IPARTSP,NAMES_AND_TITLES )
      MWA = ZERO
C--------------------------------------------
C     ECRITURE FICHIER ANIM
C--------------------------------------------
      IFVANI=0
      IF(ANIM_VERS>=40.AND.(DSANIM==1.OR.
     .                         DECANI==1.OR.
     .                         NMANIM>0.OR.IFVANI>0))   THEN
         NEL3D = NUMELS + NUMSPH + 3*NUMELS16 + 27*NUMELIG3D
         NEL2D = NUMELC + NUMELTG + NUMELQ 
         NEL1D = NUMELT + NUMELP  + 2*NUMELR
         NEL = MAX(NEL1D,NEL2D,NEL3D)
C
         SIAD=NPART+1
         SWAFT=MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D)
         SMAS=NEL+3*NUMELS16
         SWA4=3*NUMNOD+2*NUMELS16
         SMATER=NPART
         SEL2FA=NEL+1
         SXNORM=3*NUMNOD+2*NUMELS16
         SINVERT=NEL2D
         IF(NUMELX>0) THEN
            SNFACPTX=NPART
            SIXEDGE=2*NANIM1D
            SOFFX1=NANIM1D
            SNUMX1=NANIM1D
            SFUNC1=10*NANIM1D
         ELSE
            SNFACPTX=1
            SIXEDGE=1
            SOFFX1=1
            SNUMX1=1
            SFUNC1=1
         ENDIF
C
         IANIM=0
         NELEM=NUMELC+NUMELTG+NUMELS+NUMELR +
     .         NUMELP+NUMELT +NUMELQ+NUMELX
C
         CALL MY_ALLOC(DNULL,3*NUMNOD,"DNULL")
         DO I=1,3*NUMNOD
            DNULL(I)=ZERO
         ENDDO
C
         DO I=1,MX_ANI
            ANIM_N(I)=0
            ANIM_V(I)=0
            ANIM_CE(I)=0
            ANIM_CT(I)=0
            ANIM_SE(I)=0
            ANIM_ST(I)=0
            ANIM_FE(I)=0
            ANIM_FT(I)=0
         ENDDO
         ANIM_M=1
         NN_ANI=0
         NV_ANI=NMANIM
         NCE_ANI=9*NMANIM
         NCT_ANI=2*NMANIM
         NSE_ANI=9*NMANIM
         NST_ANI=1*NMANIM
         NFE_ANI=8*NMANIM
C
         CALL GENANI1(
     1      X        ,ELBUF    ,IXS     ,IXQ      ,IXC      ,
     2      IXT      ,IXP      ,IXR     ,IXTG     ,SWAFT    ,
     3      IPARG    ,PM       ,GEO     ,SKEW     ,ITAB     ,
     4      LPBY     ,NPBY     ,NSTRF   ,RWBUF    ,NPRW     ,
     5      IPART    ,IPARTS  ,IPARTQ   ,IPARTC   ,
     6      IPARTT   ,IPARTP   ,IPARTR  ,IPARTG   ,
     7      RBY      ,SWA4    ,
     8      IGRSURF  ,BUFSF    ,IPARTX  ,KXSP     ,IXSP     ,
     9      IPARTSP  ,SPBUF    ,IXS10   ,IXS20    ,IXS16    ,
     A      IPM,     IGEO,    SMATER, SEL2FA,  SNFACPTX,
     B      SIXEDGE, SOFFX1,  SNUMX1, SXNORM,  SINVERT,
     C      SFUNC1,  SIAD  ,  NMANIM, DNULL,   SMAS,
     D      MS       ,FXANI    ,MBUFEL  ,MDEPL    ,NSLEVEL  ,
     E      ELSUB,   DSANIM,  NELEM,  CEP,     CEPSP,
     F      NOM_OPT  ,INOM_OPT(5),INOM_OPT(8),
     G      ELBUF_TAB,SPH2SOL  ,SUBSETS )
      CALL MY_DEALLOC(DNULL)

      ENDIF
C----------------------------------------------
C     Driver to reader of so-called "engine cards"
C----------------------------------------------
C      NGINE = 0 ! Number of Engine "cards", to be counted in contrl.F
      IF((IS_DYNA /= 0 .OR. NB_DYNA_INCLUDE /= 0) .AND. (NGINE+NANIM_ENG /= 0))THEN
        CALL READ_ENGINE_DRIVER(IGRPART,IS_DYNA,NB_DYNA_INCLUDE)
      END IF
C----------------------------------------------
C     Driver to QAPRINT
C----------------------------------------------
      CALL ST_QAPRINT_DRIVER(
     1        IGEO        ,GEO        ,BUFGEO          ,IPM           ,PM               ,
     2        BUFMAT      ,NOM_OPT    ,INOM_OPT(1)     ,NUMLOADP      ,ILOADP           ,
     3        LLOADP      ,LOADP      ,IBCL            ,FORC          ,IPRES            ,
     4        PRES        ,NPBY       ,LPBY            ,RBY           ,IBCR             ,
     5        FRADIA      ,IBCV       ,FCONV           ,IBFTEMP       ,FBFTEMP          ,
     6        IGRV        ,LGRAV      ,GRAV            ,IBFFLUX       ,FBFFLUX          ,
     7        ITAB        ,V          , VR             ,W             ,ICODE            ,
     8        ISKEW       ,ICFIELD    ,LCFIELD         ,CFIELD        ,DAMPR            ,
     9        TEMP        ,IBCSLAG    ,IPARI           ,INTBUF_TAB    ,CLUSTERS         ,
     A        IBOX        ,IPMAS      ,IBFV            ,VEL           ,NIMPACC          ,
     B        LACCELM     ,ACCELM     ,NOM_SECT        ,NSTRF         ,SECBUF           ,
     C        SKEW        ,ISKWN      ,XFRAME          ,T_MONVOL      ,T_MONVOL_METADATA,
     D        I2RUPT      ,AREASL     ,INTBUF_FRIC_TAB ,NPFRICORTH    ,MAT_ELEM         ,
     E        PFRICORTH   ,IREPFORTH  ,PHIFORTH        ,VFORTH        ,XREFC            ,
     F        XREFTG      ,XREFS      ,TAGXREF         ,IXS           ,IXC              ,
     G        IXTG        ,RWBUF      ,NPRW            ,LPRW          ,ITHVAR           ,
     H        IPART      ,SUBSETS     ,IPARTTH         ,NTHGRPMX      ,NIMPDISP         ,
     M        NIMPVEL     ,DETONATORS ,IBCSCYC         ,NPC           ,TF               ,
     N        TABLE       ,NPTS       ,IRBE3           ,LRBE3         ,FRBE3            ,
     P        MGRBY       ,IXS10      ,ISOLNOD         ,IXR           ,R_SKEW           ,
     O        IXP         ,IXT        ,X               ,THKE          ,SH4ANG           ,
     Q        THKEC       ,SH3ANG     ,SET             ,LSUBMODEL     ,IGRNOD           ,
     R        IGRPART     ,IGRBRIC    ,IGRSH4N         ,IGRSH3N       ,IGRQUAD          ,
     S        IGRBEAM     ,IGRTRUSS   ,IGRSPRING       ,IGRSURF       ,IGRSLIN          ,
     T        IXQ         ,ISPCOND    ,RTRANS          ,IRAND         ,ALEA             ,
     U        XSEED       ,XLAS       ,ILAS            ,IRBE2         ,LRBE2            ,
     V        KXSP        ,IPARTSP    ,DRAPE           ,IXR_KJ        ,IACTIV           ,
     W        FACTIV      ,UNITAB     ,NPBYL           ,LPBYL         ,RBYL             ,
     X        XYZREF      ,SENSORS    ,FUNC2D          ,
     Y        INICRACK    ,IPRELOAD   ,PRELOAD         ,IFLAG_BPRELOAD,IBMPC            ,
     Z        IBMPC2      ,IBMPC3     ,IBMPC4          ,RBMPC         ,LJOINT           ,
     A        NNLINK      ,LNLINK     ,BUFSF           ,SBUFSF        ,STACK%PM         ,
     B        STACK%GEO   ,STACK%IGEO ,IPARG           ,IPADMESH      ,PADMESH          ,
     C        LIFLOW      ,LRFLOW     ,IFLOW           ,RFLOW         ,
     D        SH4TREE     ,SH3TREE    ,SH4TRIM         ,SH3TRIM       ,QP_IPERTURB      ,
     E        QP_RPERTURB ,LLINAL     ,LINALE          ,FVM_INIVEL    ,GJBUFI           ,
     F        GJBUFR      ,MS         ,IN              ,LGAUGE        ,GAUGE            ,
     G        KXX         ,IXX        ,IPARTX          ,LRIVET        ,IXS16            ,
     H        ICONX       ,FXBIPM     ,FXBFILE_TAB     ,EIGIPM        ,EIGRPM           ,
     I        ISPHIO      ,VSPHIO     ,EBCS_TAB        ,INIMAP1D      ,INIMAP2D         ,
     J        NSIGSH      ,SIGSH      ,NSIGI           ,SIGSP         ,NSIGS            ,
     K        SIGI        ,NSIGBEAM   ,SIGBEAM         ,NSIGTRUSS     ,SIGTRUSS         ,
     L        NSIGRS      ,SIGRS      ,MERGE_NODE_TAB  ,MERGE_NODE_TOL,
     M        IMERGE      ,NMERGE_TOT ,IEXLNK ,DRAPEG  ,USER_WINDOWS  ,OUTPUT           ,
     N        DEFAULTS    ,GLOB_THERM ,PBLAST          ,IBEAM_VECTOR  ,RBEAM_VECTOR     ,
     O        DAMP_RANGE_PART)
!
      CALL MY_DEALLOC(SIGI)
      CALL MY_DEALLOC(SIGSH)
      CALL MY_DEALLOC(SIGSP)
      CALL MY_DEALLOC(SIGRS)
      CALL MY_DEALLOC(SIGBEAM)
      CALL MY_DEALLOC(SIGTRUSS)
      CALL MY_DEALLOC(IBEAM_VECTOR)
      CALL MY_DEALLOC(RBEAM_VECTOR)
C--------------------------------------------
C     DELETE HM_MODEL IN MEMORY
C--------------------------------------------
      CALL CPP_DELETE_MODEL()
C
      IF(NINTER > 0) THEN
        CALL MY_DEALLOC(I2RUPT)
        CALL MY_DEALLOC(AREASL)
      ENDIF
      IF(NRBMERGE > 0) THEN
        CALL MY_DEALLOC(MGRBY)
      ENDIF
C     -------------------
C     Memory deallocation
C     -------------------
      IF(NFUNC2D > 0) THEN
         DO KK = 1, NFUNC2D
            DEALLOCATE(FUNC2D(KK)%XVAL, FUNC2D(KK)%FVAL)
         ENDDO
         DEALLOCATE(FUNC2D)
      ENDIF
      IF(ALLOCATED(RNOISE))  CALL MY_DEALLOC(RNOISE)
      IF(ALLOCATED(PERTURB)) CALL MY_DEALLOC(PERTURB)
      IF(ALLOCATED(QP_IPERTURB)) CALL MY_DEALLOC(QP_IPERTURB)
      IF(ALLOCATED(QP_RPERTURB)) CALL MY_DEALLOC(QP_RPERTURB)

C--------FRICTION OROTHTROPIC DIRECTIONS dealloc now after qa print -----
      IF(NINTER > 0 .AND.NINTERFRIC >0.AND. IORTHFRICMAX > 0) THEN

        CALL MY_DEALLOC(IREPFORTH)
        CALL MY_DEALLOC(VFORTH)
        CALL MY_DEALLOC(PHIFORTH)
        DEALLOCATE(PFRICORTH)

      ENDIF
C
C----------------------------------------------
      CALL TRACE_OUT1()
      ERR_MSG='RESTART FILE(S) WRITING'
      ERR_CATEGORY='RESTART FILE(S) WRITING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IERR==0) THEN
C--------------------------------------------m
C     SPMD : SPLIT +  ECRITURE FICHIER RESTART PAR PROC
C--------------------------------------------
          IF(RESTART_FILE==1) WRITE(ISTDO,'(A)')TITRE(50)
          IF(RESTART_FILE==0) WRITE(ISTDO,'(A)')CHECK_MESSAGE(1)( 1:len_trim(CHECK_MESSAGE(1)) )
C--------------------------------------------
C global connectivity matrix
C--------------------------------------------
          ILEN = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,NUMELTG)
          IF(LCNE>0) ALLOCATE(CNE(LCNE),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='CNE')
          CALL FILLCNE(
     1      CNE    ,LCNE   ,IXS    ,IXS10   ,IXS20   ,
     2      IXS16  ,IXQ    ,IXC    ,IXT     ,IXP     ,
     3      IXR    ,IXTG   ,IXTG1   ,T_MONVOL  ,
     4      IGRSURF,IBCL   ,ADDCNE  ,CEP     ,
     5      ILEN   ,GEO    ,IBCV   ,IBCR    ,IBFFLUX ,
     6      ILOADP ,LLOADP ,CEL    ,EBCS_TAB,LOADS,
     7      GLOB_THERM%NICONV ,GLOB_THERM%NIRADIA ,GLOB_THERM%NITFLUX,
     8      GLOB_THERM%NUMCONV,GLOB_THERM%NUMRADIA,GLOB_THERM%NFXFLUX,
     9      bcs)
          IF(I2NSNT>0) THEN
            IF(LCNI2G>0) ALLOCATE(CNI2(LCNI2G))
            CALL FILLCNI2(
     1        CNI2   ,LCNI2G,ADDCNI2,IPARI, INTBUF_TAB )
          ENDIF
C--------------------------------------------
C     XDP ARRAY IN STARTER FOR SINGLE PRECISION
      CALL MY_ALLOC(XDP,1,"XDP")

          LENVOLU = NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL+LIBAGALE
C
          LNOM_OPT=SNOM_OPT
          LENPOR = SNODPOR

          LENTHG = OUTPUT%TH%SITHBUF
          LENTHGR = SRTHBUF
Clength of bufmat and bufgeo
          LBUFMAT = SBUFMAT
          LBUFGEO = SBUFGEO
          LBUFSF  = SBUFSF
          PM1SHF = 1
          PM1SPH = 1

C----------------------------------------------------------------------
          ! Allocation and filling of specific ADDCNE and CNE for non-local
          IF(NLOC_DMG%IMOD>0) THEN
            ! Allocation of ADDCNE for non-local nodes
            IF(.NOT.ALLOCATED(NLOC_DMG%ADDCNE)) ALLOCATE(NLOC_DMG%ADDCNE(0:NLOC_DMG%NNOD+1))
            NLOC_DMG%ADDCNE(0:NLOC_DMG%NNOD+1) = 0
            ! Filling ADDCNE for non-local nodes
            CALL BUILD_ADDCNEL_SUB(ADDCNE  ,CNE      ,NLOC_DMG%ADDCNE,NLOC_DMG%INDX,NLOC_DMG%NNOD)
            ! Allocation of CNE for non-local nodes
            IF(.NOT.ALLOCATED(NLOC_DMG%CNE)) ALLOCATE(NLOC_DMG%CNE(NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1))
            NLOC_DMG%CNE(1:NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1) = 0
            ! Filling CNE for non-local nodes
            CALL BUILD_CNEL_SUB(NLOC_DMG%CNE,NLOC_DMG%ADDCNE,CNE,ADDCNE,NLOC_DMG%INDX,NLOC_DMG%NNOD)
          ENDIF
C--------------------------------------------
C     multidomains -> transfer of domdec
C--------------------------------------------
          IF((NSUBDOM>0).AND.(FLG_R2R_ERR==0)) THEN
            CALL R2R_CLEAN_INTER(IPARI,INTBUF_TAB,IPARTC,IPARTG,IPARTS,ISOLNOD)
            IF(IDDOM>0) THEN
           WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DOMDEC SYNCHRONIZATION '
              CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,1)
            ELSE
              CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,2)
            ENDIF
          ENDIF

C    deallocation of arrays that are not needed anymore
C    The memory peak is in ddsplit: we need to deallocate everything that is not
C    needed anmyre before ddsplit.

       CALL MY_DEALLOC(IKINE1LAG)
       CALL MY_DEALLOC(IWCONT)
       CALL MY_DEALLOC(IWCIN2)
       CALL MY_DEALLOC(DSDOF)




C-------------------------------------------------------------
C INTERFACE ROUTINES CALLED BEFORE DOMAIN DECOMPOSITION
C-------------------------------------------------------------

C-------------------------------------------------------------
C         Set INTERCEP (for all INTERFACES except TYPE24) (flag=1)
C-------------------------------------------------------------

          CALL SET_INTERCEP(IPARI,INTERCEP,1,INTBUF_TAB,ITAB,CEP)

C ! this call is maintened here to avoid a bug
C-------------------------------------------------------------
          IF(NSPMD > 1 .AND. IDDLEVEL > 0) THEN
            CALL SET_FRONT8(IPARI,INTERCEP,INTBUF_TAB,INTERT8,NBT8,ITAB)
          ENDIF
C--------------------------------------------
C         /Inter/Type25 Connectivit Sommets => Segments
C-------------------------------------------------------------
C
C         Dimensioning (computes NUMNOR == Nb of normals or vertices wrt ALL Interfaces TYPE25)
C         and Initialization of IRT>LM(3:4,1:NSN)
          CALL PREPARE_INT25(INTBUF_TAB, IPARI, INTERCEP, NRTMT_25)
          CALL PREPARE_SPLIT_I25E2E(NSPMD,INTBUF_TAB,IPARI,INTERCEP,CEP,SCEP)

C
          call my_alloc(ADDCSRECT, NUMNOR+1, "ADDCSRECT", stat=stat)
          call my_alloc(CSRECT, 4*NRTMT_25, "CSRECT", stat=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                          C1='CSRECT')
          ADDCSRECT(1:NUMNOR+1)=0
C
          IF(NINTER25 /= 0)
     .      CALL BUILD_CSRECT(INTBUF_TAB,IPARI,CSRECT,ADDCSRECT)
C
C--------------------------------------------
C LINES : SET A CPU for splitting
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        /\
!       /  \
!      / |  \
!     /  |   \
!    /   o    \
!   /__________\
!
!       /LINE are not used in the engine and the split is wrong in
!       case of useless line (ie. when a line is defined but not
!       used by an interface or other stuffs) -->
!       2 nodes (defining a segment) can be on 2 different processors
!       in this case, the segment is not written in the restart file
!       one could also define the nodes on the same processor but
!       it will increase the comm.
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          CALL LINE_DECOMP(IGRSLIN)
!--------------------------------------------
C--------------------------------------------
C Computation of SPMD global variables
C--------------------------------------------
C CARE TO be computed right before DDSPLIT
C This routine computes array sizes for Animation file.
C There should not be any call to frontplus beside this point.
C--------------------------------------------
          CALL GLOBVARS(IGEO,IXR    ,NSTRF  )
          IF(NPLYMAX > 0)THEN
             CALL SPMD_ANIM_PLY_INIT(IGEO, GEO  ,IPARG  ,IXC  ,IXTG ,
     .                          IPARTC,IPARTQ,IPARTG ,STACK )
          ENDIF
C--------------------------------------------

c         start CPU timer for ddsplit
          CALL STARTIME(3,1)
!       compute the size of TAG_SCRATCH and check if /INTER/24 or /25 is used
          CALL GET_SIZE_INTER24(I24MAXNSNE2,NINTER,NPARI,IPARI,FLAG_24_25)
!       compute the local number of element
          CALL GET_SIZE_NUMNOD_LOCAL(NUMNOD,NUMNOD_L)

          allocate(ALE_ELM(NSPMD))
          IF( (NUMELS>0).AND.(IALE+IEULER+GLOB_THERM%ITHERM+IALELAG/=0) ) THEN
                CALL SPLIT_CFD_SOLIDE(NUMELS,ALE_CONNECTIVITY,IXS,ALE_ELM,SIZE_ALE_ELM)
          ELSE
                SIZE_ALE_ELM(1:NSPMD) = 0
          ENDIF
          ! -----------------------------------------
          ! reverse connectivity for FVM solver : useful to ensure the parith/on
          IF((IALE+IEULER+GLOB_THERM%ITHERM+IALELAG/=0).OR.(DETONATORS%IS_SHADOWING_REQUIRED)) THEN
                CALL MY_ALLOC(INDX_S,NUMELS,"INDX_S")
                CALL MY_ALLOC(INDX_Q,NUMELQ,"INDX_Q")
                CALL MY_ALLOC(INDX_TG,NUMELTG,"INDX_TG")
                CALL MY_ALLOC(FACE_ELM_S,6*NUMELS,2,"FACE_ELM_S")
                CALL MY_ALLOC(FACE_ELM_Q,4*NUMELQ,2,"FACE_ELM_Q")
                CALL MY_ALLOC(FACE_ELM_TG,3*NUMELTG,2,"FACE_ELM_TG")

                INDX_S(1:NUMELS) = 0
                INDX_Q(1:NUMELQ) = 0
                INDX_TG(1:NUMELTG) = 0
                FACE_ELM_S(1:6*NUMELS,1:2) = 0
                FACE_ELM_Q(1:4*NUMELQ,1:2) = 0
                FACE_ELM_TG(1:3*NUMELTG,1:2) = 0
                BOOL_ALE_TG = (N2D/=0.AND.MULTI_FVM%IS_USED)
                CALL MULTI_CONNECTIVITY( INDX_S,INDX_Q,INDX_TG,
     1                                   FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,
     2                                   IXS,IXQ,IXTG,CEP,ALE_CONNECTIVITY,BOOL_ALE_TG)

          ELSE
                CALL MY_ALLOC(INDX_S,0,"INDX_S")
                CALL MY_ALLOC(INDX_Q,0,"INDX_Q")
                CALL MY_ALLOC(INDX_TG,0,"INDX_TG")
                CALL MY_ALLOC(FACE_ELM_S,0,0,"FACE_ELM_S")
                CALL MY_ALLOC(FACE_ELM_Q,0,0,"FACE_ELM_Q")
                CALL MY_ALLOC(FACE_ELM_TG,0,0,"FACE_ELM_TG")
          ENDIF
          ! -----------------------------------------

          ! -----------------------------------------
          ! split the LOADS structure on the different processors
          CALL SPLIT_PCYL(LOADS%NLOAD_CYL,LOADS,LOADS_PER_PROC)
          ! -----------------------------------------            

          ! -----------------------------------------
          ! split the BCs data structure
          CALL SPLIT_BCS_WALL(BCS_PER_PROC, CEP, SCEP, NSPMD)
          CALL SPLIT_BCS_NRF(BCS_PER_PROC, CEP, SCEP, NSPMD)
          ! -----------------------------------------

          ! -----------------------------------------
          ! prepare the rwall splitting
          call alloc_constraint_struct(nrwall,nspmd,constraint_struct)
          call split_rwall(nrwall,nspmd,nnprw,slprw,nprw,lprw,constraint_struct)
          ! -----------------------------------------

C Parallel

!   -------------------------------------------------------------
!       RESTART FILE WRITING
!       if -norst cdl is used or if /RFILE/OFF is used, then
!       restart files are not generated
!   -------------------------------------
          IF(RESTART_FILE==1) THEN
C            CALL PREPARE_INT25_EDGE(INTBUF_TAB,INTERCEP,NSPMD,IPARI)
C----- create TAG_SKINS6 for /H3D/STRESS/TENS/OUTER
             CALL MY_ALLOC(TAG_SKINS6,NUMELS,"TAG_SKINS6")
             CALL SURFEXT_TAGN(IXS    ,KNOD2ELS,NOD2ELS  ,IXS10   ,TAG_SKINS6,itab)
          P=0
          NP=P
          IF(.NOT. ALLOCATED(PARTSAV)) ALLOCATE(PARTSAV(0))
          IF(.NOT. ALLOCATED(ADMSMS)) ALLOCATE(ADMSMS(0))
          IF(.NOT. ALLOCATED(DMELC)) ALLOCATE(DMELC(0))
          IF(.NOT. ALLOCATED(DMELS)) ALLOCATE(DMELS(0))
          IF(.NOT. ALLOCATED(DMELTG)) ALLOCATE(DMELTG(0))
          IF(.NOT. ALLOCATED(DMELTR)) ALLOCATE(DMELTR(0))
          IF(.NOT. ALLOCATED(DMELP)) ALLOCATE(DMELP(0))
          IF(.NOT. ALLOCATED(DMELRT)) ALLOCATE(DMELRT(0))
          IF(.NOT. ALLOCATED(RES_SMS)) ALLOCATE(RES_SMS(0))
          IF(.NOT. ALLOCATED(DIAG_SMS)) ALLOCATE(DIAG_SMS(0))
          IF(.NOT. ALLOCATED(CNE_PXFEM)) ALLOCATE(CNE_PXFEM(0))
          IF(.NOT. ALLOCATED(CEL_PXFEM)) ALLOCATE(CEL_PXFEM(0))
          IF(.NOT. ALLOCATED(MSZ2)) ALLOCATE(MSZ2(0))
          IF(.NOT. ALLOCATED(XFEM_PHANTOM)) ALLOCATE(XFEM_PHANTOM(0))
!$OMP PARALLEL PRIVATE(ITASK,P,pMEMFLOW,INDX_NM,NINDX_NM,TAG_NM)
!$OMP+ PRIVATE(NINDX_SCRT,INDX_SCRT,TAG_SCRATCH)
          NINDX_NM = 0
          NINDX_SCRT = 0
          IF(NINTER>0) THEN
                CALL MY_ALLOC(TAG_NM,NUMNOD,"TAG_NM")
                CALL MY_ALLOC(INDX_NM,NUMNOD,"INDX_NM")
                CALL MY_ALLOC(TAG_SCRATCH,I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO,"TAG_SCRATCH")
                CALL MY_ALLOC(INDX_SCRT,I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO,"INDX_SCRT")
                TAG_NM(1:NUMNOD) = 0
                INDX_NM(1:NUMNOD) = 0
                TAG_SCRATCH(1:I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) = 0
                INDX_SCRT(1:I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) = 0
          ELSE
                CALL MY_ALLOC(TAG_NM,0,"TAG_NM")
                CALL MY_ALLOC(INDX_NM,0,"INDX_NM")
                CALL MY_ALLOC(TAG_SCRATCH,0,"TAG_SCRATCH")
                CALL MY_ALLOC(INDX_SCRT,0,"INDX_SCRT")
          ENDIF

 220      CONTINUE

!$OMP CRITICAL
          NP=NP+1
          P=NP
!$OMP END CRITICAL

#if defined(_OPENMP)
          ITASK = OMP_GET_THREAD_NUM()
#endif
c
          IF(P > NSPMD) GOTO 221
          !Sending clean addresses for unallocated arrays in case P=0
          NULLIFY(pMEMFLOW) ; IF(NSPMD  > 0)  pMEMFLOW => MEMFLOW(1,P)    !MEMFLOW(2,1:NSPMD)   ; NSPMD =0 => MEMFLOW(1,0) is undefined


          CALL DDSPLIT(
     1         P            ,CEP         ,CEL            ,IGEO        ,MAT_ELEM   ,
     2         IPM          ,ICODE       ,ISKEW          ,ISKWN       ,BID13      ,
     3         IBCSLAG      ,IPART       ,IPARTS         ,IPARTQ      ,IPARTC     ,
     4         IPARTT       ,IPARTP      ,IPARTR         ,IPARTG      ,DETONATORS ,
     5         IPARTX       ,NPC         ,IXTG           ,GROUP_PARAM_TAB,
     6         IXTG1        ,IXS         ,IXS10          ,IXS20       ,IXS16      ,
     7         IXQ          ,IXC         ,IXT            ,IXP         ,IXR        ,
     8         ITAB         ,ITABM1      ,GJBUFI         ,ALE_CONNECTIVITY%NALE   ,
     9         ALE_CONNECTIVITY,
     A         KXX          ,IXX         ,IBCL           ,IBFV        ,
     B         ILAS         ,LACCELM     ,NNLINK         ,LNLINK      ,    
     C         IPARG        ,IGRV        ,LGRAV          ,IBVEL       ,LBVEL      ,    
     D         IACTIV       ,FACTIV      ,KINET          ,IPARI       ,NPRW       ,              
     E         LPRW         ,ICONX       ,NPBY           ,    
     F         LPBY         ,LRIVET      ,NSTRF          ,LJOINT      ,NODPOR     ,    
     G         MONVOL       ,ICONTACT    ,LAGBUF         ,    
     H         FR_IAD       ,X           ,D              ,V           ,VR         ,    
     I         DR           ,THKE        ,DAMPR          ,DAMP        ,MS         ,    
     J         IN           ,TF          ,PM             ,SKEW        ,XFRAME     ,    
     K         GEO          ,EANI        ,BUFMAT         ,BUFGEO      ,BUFSF      ,    
     L         RBMPC        ,GJBUFR      ,W              ,VEUL        ,FILL       ,    
     M         DFILL        ,WB          ,DSAVE          ,ASAVE       ,MSNF       ,    
     N         SPBUF        ,FORC        ,VEL            ,FSAV        ,FZERO      ,    
     O         XLAS         ,ACCELM      ,FBVEL          ,GRAV       ,    
     P         FR_WAVE      ,FAILWAVE    ,PARTS0         ,ELBUF      ,                
     Q         RWBUF        ,RWSAV       ,RBY            ,RIVET       ,    
     R         SECBUF       ,VOLMON      ,RCONX          ,NLOC_DMG    ,                             
     S         FVMAIN       ,LIBAGALE    ,LENTHG         ,LBUFMAT     ,LBUFGEO    ,
     T         LBUFSF       ,SXLAS                       ,LNOM_OPT    ,SILAS      ,    
     U         LENVOLU                   ,NPTS           ,CNE         ,LCNE       ,    
     V         ADDCNE       ,CNI2        ,LCNI2G         ,ADDCNI2     ,CEPI2      ,    
     W         CELI2        ,I2NSNT      ,PROBINT        ,DDSTAT(1,P) ,PM1SHF,         
     X         DD_IAD       ,
     Z                       KXSP        ,IXSP           ,NOD2SP      ,CEPSP      ,    
     a         NTHWA        ,NAIRWA      ,NMNT           ,L_MUL_LAG1  ,L_MUL_LAG  ,    
     b         LWASPIO      ,IPARTSP     ,ISPCOND        ,PM1SPH      ,                
     c         WMA          ,
     d         EIGIPM       ,EIGIBUF     ,EIGRPM         ,    
     e         IFLOW        ,RFLOW       ,pMEMFLOW       ,IEXLNK      ,FASOLFR    ,    
     f         IPARTTH      ,    
     j         FXBIPM       ,FXBRPM      ,FXBNOD         ,FXBMOD      ,FXBGLM     ,    
     k         FXBCPM       ,FXBCPS      ,FXBLM          ,FXBFLS      ,FXBDLS     ,    
     l         FXBDEP       ,FXBVIT      ,FXBACC         ,FXBELM      ,FXBSIG     ,    
     m         FXBGRVI      ,FXBGRVR     ,IADLL          ,LLL         ,IBMPC      ,    
     n         LAMBDA       ,LRBAGALE    ,ISKWP          ,NSKWP       ,ISENSP     ,    
     o         NSENSP       ,IACCP       ,NACCP          ,IPART_STATE ,MCP        ,    
     p         TEMP         ,UNITAB      ,INTSTAMP       ,IFRAME      ,CLUSTERS   ,    
     q                       PARTSAV     ,IBFTEMP        ,FBFTEMP     ,IBCV       ,       
     r         FCONV        ,IRBE3       ,LRBE3          ,FRBE3       ,FRONT_RM   ,    
     s         RBYM         ,IRBYM       ,LNRBYM         ,INOISE      ,FNOISE     ,    
     t         MS0          ,ADMSMS      ,NOM_SECT       ,ISPSYM      ,                
     u         SH4TREE      ,SH3TREE     ,IPADMESH       ,IBFFLUX     ,FBFFLUX    ,                             
     v         SH4TRIM      ,SH3TRIM     ,PADMESH        ,MSC         ,MSTG       ,    
     w         INC          ,INTG        ,PTG            ,MCPC        ,MCPTG      ,    
     x         RCONTACT     ,ACONTACT    ,PCONTACT       ,MSCND       ,INCND      ,    
     y         MSSA         ,MSTR        ,MSP            ,MSRT        ,IBCR       ,    
     z         FRADIA       ,DMELC       ,DMELTG         ,DMELS       ,DMELTR     ,    
     1         DMELP        ,DMELRT      ,RES_SMS                     ,ISPHIO     ,    
     2         LPRTSPH      ,LONFSPH     ,VSPHIO         ,SPHVELN     ,ALPH       ,    
     3         IFILL        ,IMS         ,IRBE2          ,LRBE2       ,    
     8         MS_PLY,
     9         ZI_PLY       ,INOD_PXFEM  ,IEL_PXFEM      ,ICODEP      ,ISKEWP     ,
     A         ADDCNE_PXFEM ,CNE_PXFEM   ,CEL_PXFEM      ,ITHVAR      ,XDP,TABLE  ,
     B         CELSPH       ,ICFIELD     ,LCFIELD        ,CFIELD      ,
     C         MSZ2         ,ITASK       ,DIAG_SMS,
     D         ILOADP       ,LLOADP      ,LOADP,
     E         INOD_CRKXFEM ,IEL_CRKXFEM ,ADDCNE_CRKXFEM ,CNE_CRKXFEM ,CEL_CRKXFEM,
     F         IBUFSSG_IO   ,INTERCEP    ,IBORDNODE      ,IEDGESH     ,IBORDEDGE  ,
     G         LINALE       ,NODEDGE     ,IEDGE          ,CEP_CRKXFEM ,IEDGE_TMP  ,
     H         CRKNODIAD    ,ELBUF_TAB   ,NOM_OPT        ,LGAUGE      ,GAUGE      ,
     I         IGAUP        ,NGAUP       ,NODLEVXF       ,FRONTB_R2R  ,DFLOW      ,
     J         VFLOW        ,WFLOW       ,SPH2SOL        ,SOL2SPH     ,IRST       ,
     K         ELCUTC       ,NODENR      ,KXFENOD2ELC    ,ENRTAG      ,INTBUF_TAB ,
     M         I11FLAG      ,XFEM_TAB    ,LENTHGR        ,RTHBUF      ,
     N         IXIG3D       ,KXIG3D      ,KNOT           ,IPARTIG3D   ,WIGE       ,
     O         NCRKPART     ,INDX_CRK    ,CRKLVSET       ,CRKSHELL    ,CRKSKY     ,
     P         CRKAVX       ,CRKEDGE     ,SENSORS        ,
     Q         STACK        ,XFEM_PHANTOM, INTERT8       ,TAB_UMP     ,POIN_UMP   ,
     R         SOL2SPH_TYP  ,ADDCSRECT   ,CSRECT         ,DRAPE       ,LOADS      ,
     S         ITAGND         ,ICNDS10     ,ADDCNCND   ,
     T         CEPCND       ,CELCND      ,CNCND          ,NATIV_SMS   ,I24MAXNSNE ,
     U         MULTI_FVM    ,SEGQUADFR   ,INTBUF_FRIC_TAB,SUBSETS     ,IGRNOD     ,
     V         IGRBRIC      ,IGRQUAD     ,IGRSH4N        ,IGRSH3N     ,IGRTRUSS   ,
     W         IGRBEAM      ,IGRSPRING   ,IGRPART        ,IGRSURF     ,IGRSLIN    ,
     X         POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,MID_PID_SHELL,MID_PID_TRI ,
     Y         MID_PID_SOL  , TAG_NM      ,NINDX_NM       ,INDX_NM      ,TAG_SCRATCH ,
     Z         NINDX_SCRT   , INDX_SCRT   ,FLAG_24_25     ,NUMNOD_L(P)  ,TAG_SKN     ,
     A         SKEWS%MULTIPLE_SKEW, IGRSURF_PROC,KNOTLOCPC     ,KNOTLOCEL     ,ALE_ELM(P),
     B         SIZE_ALE_ELM(P),PINCH_DATA ,TAG_SKINS6    ,IBCSCYC      ,LBCSCYC   ,T_MONVOL,
     C         INDX_S,INDX_Q,INDX_TG,FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,NBR_TH_MONVOL, EBCS_TAB,
     D         KLOADPINTER  ,LOADPINTER   ,DGAPLOADINT   ,S_LOADPINTER, SCEP,DYNAIN_DATA,
     E         DRAPEG       ,USER_WINDOWS ,OUTPUT        ,INTERFACES  ,NUMBER_LOAD_CYL ,
     F         LOADS_PER_PROC(P), PYTHON,DPL0CLD,VEL0CLD ,NAMES_AND_TITLES,
     G         BCS_PER_PROC(P),constraint_struct,glob_therm,PBLAST,RWSTIF_PEN ,SLN_PEN ,
     H         IN_SFEM, NE_SFEM , bcs)
      GOTO 220
 221  CONTINUE

      CALL MY_DEALLOC(TAG_NM)
      CALL MY_DEALLOC(TAG_SCRATCH)
      CALL MY_DEALLOC(INDX_NM)
      CALL MY_DEALLOC(INDX_SCRT)
!$OMP END PARALLEL
      CALL MY_DEALLOC(TAG_SKINS6)
      ENDIF     ! <-- end of restart file writing
!   -------------------------------------------------------------
C
      CALL DEALLOCATE_IGRSURF_SPLIT(T_MONVOL,IGRSURF_PROC)
      DEALLOCATE( IGRSURF_PROC )

      IF( (NUMELS>0).AND.(IALE+IEULER+GLOB_THERM%ITHERM+IALELAG/=0) ) CALL DEALLOCATE_SPLIT_CFD_SOLIDE(ALE_ELM)

      CALL BCS%DEALLOCATE()
      DO P=1,NSPMD
        CALL BCS_PER_PROC(P)%DEALLOCATE()
      ENDDO

      DEALLOCATE( ALE_ELM )

      CALL MY_DEALLOC(INDX_S)
      CALL MY_DEALLOC(INDX_Q)
      CALL MY_DEALLOC(INDX_TG)
      CALL MY_DEALLOC(FACE_ELM_S)
      CALL MY_DEALLOC(FACE_ELM_Q)
      CALL MY_DEALLOC(FACE_ELM_TG)
c     stop CPU timer for ddsplit
      CALL STOPTIME(3,1)

      CALL TRACE_OUT1()
      ERR_MSG='CLOSING STARTER'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C----
C
C Deallocation
C
          CALL MY_DEALLOC(ICONTACT)
C
          IF(ALLOCATED(CEP)) CALL MY_DEALLOC(CEP)
          IF(ALLOCATED(CEL)) CALL MY_DEALLOC(CEL)
C
          IF(LCNE>0)THEN
            DEALLOCATE(CNE)
          END IF
C
          IF(I2NSNT>0) THEN
            CALL MY_DEALLOC(CELI2)
            CALL MY_DEALLOC(CEPI2)
            DEALLOCATE(ADDCNI2)
          END IF
          CALL MY_DEALLOC(ISKWP)
          CALL MY_DEALLOC(NSKWP)
          CALL MY_DEALLOC(ISENSP)
          CALL MY_DEALLOC(NSENSP)
          CALL MY_DEALLOC(IACCP)
          CALL MY_DEALLOC(NACCP)
          CALL MY_DEALLOC(IGAUP)
          CALL MY_DEALLOC(NGAUP)
          CALL MY_DEALLOC(IPART_STATE)
          CALL MY_DEALLOC(EIGIPM)
          CALL MY_DEALLOC(EIGIBUF)
          CALL MY_DEALLOC(EIGRPM)
          CALL MY_DEALLOC(TAG_SKN)
          DEALLOCATE(SKEWS%MULTIPLE_SKEW)
          IF(IDDLEVEL ==1 .OR. ((NINTER == 0).AND.(ISMS == 0))) THEN
            CALL MY_DEALLOC(TAGXREF)
            CALL MY_DEALLOC(TAGREFSTA)
          ENDIF
C
          IF(NUMSPH>0) THEN
            CALL MY_DEALLOC(CEPSP)
          END IF
          IF(NSPHIO>0)THEN
            CALL MY_DEALLOC(IBUFSSG_IO)
            CALL MY_DEALLOC(RESERVEP)
          ENDIF
          CALL MY_DEALLOC(CELSPH)
C
          IF(LAG_NCF > 0) THEN
            CALL MY_DEALLOC(IADLL)
            CALL MY_DEALLOC(LLL)
          END IF
C
C          IF(ALLOCATED(FVSPMD)) DEALLOCATE(FVSPMD)
C
          CALL MY_DEALLOC(ADDCSRECT)
          CALL MY_DEALLOC(CSRECT)
          CALL MY_DEALLOC(IGEO_STACK)
          CALL MY_DEALLOC(GEO_STACK)
          IF(ALLOCATED(PLY_INFO))CALL MY_DEALLOC(PLY_INFO)
          IF(ALLOCATED(FXBFILE_TAB)) DEALLOCATE(FXBFILE_TAB)

          IF(ALLOCATED(TAB_UMP)) CALL MY_DEALLOC(TAB_UMP)
          IF(ALLOCATED(TAB_UMP_OLD)) CALL MY_DEALLOC(TAB_UMP_OLD)
          IF(ALLOCATED(POIN_UMP)) CALL MY_DEALLOC(POIN_UMP)
          IF(ALLOCATED(POIN_UMP_OLD)) CALL MY_DEALLOC(POIN_UMP_OLD)

          CALL MY_DEALLOC(POIN_PART_SHELL)
          CALL MY_DEALLOC(POIN_PART_TRI)
          CALL MY_DEALLOC(POIN_PART_SOL)
          DO I=1,NUMMAT
                IF(ALLOCATED(MID_PID_SHELL(I)%PID1D))DEALLOCATE( MID_PID_SHELL(I)%PID1D )
                IF(ALLOCATED(MID_PID_SHELL(I)%COST1D))DEALLOCATE( MID_PID_SHELL(I)%COST1D )

                IF(ALLOCATED(MID_PID_SHELL(I)%PID1D))DEALLOCATE( MID_PID_TRI(I)%PID1D )
                IF(ALLOCATED(MID_PID_TRI(I)%COST1D))DEALLOCATE( MID_PID_TRI(I)%COST1D )
                DO J=1,7
                        IF(ALLOCATED(MID_PID_SOL(I,J)%PID1D)) DEALLOCATE( MID_PID_SOL(I,J)%PID1D )
                        IF(ALLOCATED(MID_PID_SOL(I,J)%COST1D)) DEALLOCATE( MID_PID_SOL(I,J)%COST1D )
                ENDDO
          ENDDO
          DEALLOCATE( MID_PID_SHELL,MID_PID_TRI )
          DEALLOCATE( MID_PID_SOL )


          IF(ALLOCATED(IXIG3D)) CALL MY_DEALLOC(IXIG3D)
          IF(ALLOCATED(KXIG3D)) CALL MY_DEALLOC(KXIG3D)
          IF(ALLOCATED(MSIG3D)) CALL MY_DEALLOC(MSIG3D)
          IF(NS10E>0.AND.IPARI0/=0) THEN
            CALL MY_DEALLOC(CELCND)
            CALL MY_DEALLOC(CEPCND)
            DEALLOCATE(ADDCNCND)
            IF(LCNCND>0) CALL MY_DEALLOC(CNCND)
          END IF
          IF(ALLOCATED(ITAGND)) CALL MY_DEALLOC(ITAGND)
          IF(NS10E>0) CALL MY_DEALLOC(ICNDS10)
          CALL MONVOL_DEALLOCATE(NVOLU, T_MONVOL)
          IF(ALLOCATED(T_MONVOL)) DEALLOCATE(T_MONVOL)
          CALL MY_DEALLOC(IBCSCYC)
          CALL MY_DEALLOC(LBCSCYC)
          IF(NBCSCYC>0) CALL MY_DEALLOC(ITAGCYC)
          IF(ALLOCATED(FVM_INIVEL)) DEALLOCATE(FVM_INIVEL)
C
C--       Seatblet structures deallocation
          IF(N_SEATBELT > 0) THEN
            DO I=1,N_SEATBELT
              DEALLOCATE(SEATBELT_TAB(I)%SPRING)
            ENDDO
            DEALLOCATE(SEATBELT_TAB)
          ENDIF
C
          IF(NSLIPRING > 0) THEN
            DO I=1,NSLIPRING
              DEALLOCATE(SLIPRING(I)%FRAM)
            ENDDO
            DEALLOCATE(SLIPRING)
          ENDIF
C
          IF(NRETRACTOR > 0) THEN
            DO I=1,NRETRACTOR
              DEALLOCATE(RETRACTOR(I)%INACTI_NODE)
              DO J=1,2   
                IF (RETRACTOR(I)%IFUNC(J) > 0) THEN                                    
                  DEALLOCATE(RETRACTOR(I)%TABLE(J)%X(1)%VALUES)
                  DEALLOCATE(RETRACTOR(I)%TABLE(J)%X)
                  DEALLOCATE(RETRACTOR(I)%TABLE(J)%Y%VALUES)
                  DEALLOCATE(RETRACTOR(I)%TABLE(J)%Y)
                ENDIF  
              ENDDO  
            ENDDO
            DEALLOCATE(RETRACTOR)
          ENDIF
C
          IF (NGUIDED_CABLE > 0) THEN
            DO I=1,NGUIDED_CABLE
               IF (NSPMD > 1) DEALLOCATE(GUIDE(I)%NODES)
               DEALLOCATE(GUIDE(I)%CONT)      
            ENDDO
            DEALLOCATE(GUIDE)
          ENDIF
C          
          IF((IPART_STACK > 0 .OR. IPART_PCOMPP > 0) .AND. NDRAPE > 0) DEALLOCATE(IWORK_T)

          IF(ALLOCATED(MPARAM_R2R))THEN
            DO I=1,NUMMAT  ; CALL MPARAM_R2R(I)%DESTRUCT() ; ENDDO
            DEALLOCATE(MPARAM_R2R)
          ENDIF

          ! -----------------------------------------
          ! deallocation of constraint_struct
          call dealloc_constraint_struct(nrwall,constraint_struct)
          ! -----------------------------------------

C --------------------------------------
C Starter Memory Printout
C --------------------------------------
          CALL PRINTSTSZ(DETONATORS)
C --------------------------------------
C Stat domdec + evaluation memoire SPMD
C --------------------------------------
C
          IF(RESTART_FILE==1) THEN
            CALL DDPRINT(DDSTAT, MEMFLOW)
          ELSE
            WRITE(IOUT,*)
            WRITE(IOUT,*) CHECK_MESSAGE(3)(1:len_trim(CHECK_MESSAGE(3)))
          ENDIF
C
c        ENDIF
      ELSE
        WRITE(ISTDO,'(A)')TITRE(48)
      ENDIF
C
      CALL MY_DEALLOC(KNOD2ELC)
      CALL MY_DEALLOC(KNOD2ELTG)
      CALL MY_DEALLOC(NOD2ELS)
      CALL MY_DEALLOC(NOD2ELC)
      CALL MY_DEALLOC(NOD2ELTG)
      CALL MY_DEALLOC(KNOD2EL1D)
      CALL MY_DEALLOC(NOD2EL1D)
      CALL MY_DEALLOC(KNOD2ELIG3D)
      CALL MY_DEALLOC(NOD2ELIG3D)
      CALL MY_DEALLOC(KNOD2ELQ)
      CALL MY_DEALLOC(NOD2ELQ)
      DEALLOCATE(KNOD2ELS)
C
      CALL MY_DEALLOC(MSC)
      CALL MY_DEALLOC(MSTG)
      CALL MY_DEALLOC(MSSA)
      CALL MY_DEALLOC(MSRT)
      DEALLOCATE(MSTR, MSP)
C
      CALL MY_DEALLOC(MCP)
      CALL MY_DEALLOC(TEMP)
      CALL MY_DEALLOC(IBCV)
      CALL MY_DEALLOC(FCONV)
      CALL MY_DEALLOC(IBCR)
      CALL MY_DEALLOC(FRADIA)
      CALL MY_DEALLOC(IBFTEMP)
      CALL MY_DEALLOC(FBFTEMP)
      CALL MY_DEALLOC(IBFFLUX)
      CALL MY_DEALLOC(FBFFLUX)
C
      CALL MY_DEALLOC(RBYM)
      CALL MY_DEALLOC(IRBYM)
      CALL MY_DEALLOC(LNRBYM)
      CALL MY_DEALLOC(WEIGHT_RM)
      CALL MY_DEALLOC(MS_PLY)
      CALL MY_DEALLOC(ZI_PLY)
      CALL MY_DEALLOC(ICODE)
      CALL MY_DEALLOC(ISKEW)
c
      IF(ALLOCATED(KNOTLOCPC))CALL MY_DEALLOC(KNOTLOCPC)
      IF(ALLOCATED(KNOTLOCEL))CALL MY_DEALLOC(KNOTLOCEL)
c
      CALL TRACE_OUT1()
      IF(ALLOCATED(IPMAS))DEALLOCATE(IPMAS)
c---------------------------
      IF(ICRACK3D > 0) THEN
        CALL MY_DEALLOC(INOD_CRKXFEM)
        CALL MY_DEALLOC(IEL_CRKXFEM)
        CALL MY_DEALLOC(CNE_CRKXFEM)
        CALL MY_DEALLOC(CEL_CRKXFEM)
        CALL MY_DEALLOC(CEP_CRKXFEM)
        CALL MY_DEALLOC(IEDGESH)
        CALL MY_DEALLOC(IBORDEDGE)
        CALL MY_DEALLOC(NODEDGE)
        CALL MY_DEALLOC(IEDGE)
        CALL MY_DEALLOC(IBORDNODE)
      END IF
      IF(ALLOCATED(IEDGE_TMP))     CALL MY_DEALLOC(IEDGE_TMP)
      IF(ALLOCATED(ELCUTC))        CALL MY_DEALLOC(ELCUTC)
      IF(ALLOCATED(NODENR))        CALL MY_DEALLOC(NODENR)
      IF(ALLOCATED(KXFENOD2ELC))   CALL MY_DEALLOC(KXFENOD2ELC)
      IF(ALLOCATED(ENRTAG))        CALL MY_DEALLOC(ENRTAG)
      IF(ALLOCATED(ADDCNE_CRKXFEM))DEALLOCATE(ADDCNE_CRKXFEM)
C----------------------------------------------
C     ALLOCATION TO REDUCE STACKSIZE
C----------------------------------------------
      DEALLOCATE(ADDCNE)
      DEALLOCATE(ADDCNE_PXFEM)
      CALL MY_DEALLOC(FXBTAG)
C
      CALL MY_DEALLOC(ISOLNOD)
      CALL MY_DEALLOC(ISOLOFF)
      CALL MY_DEALLOC(ISHEOFF)
      CALL MY_DEALLOC(ITRUOFF)
      CALL MY_DEALLOC(IPOUOFF)
      CALL MY_DEALLOC(IRESOFF)
      CALL MY_DEALLOC(ITRIOFF)
      CALL MY_DEALLOC(IGRNRBY)
      CALL MY_DEALLOC(IQUAOFF)
C
      CALL MY_DEALLOC(XREFC)
      CALL MY_DEALLOC(XREFTG)
      CALL MY_DEALLOC(XREFS)
      CALL MY_DEALLOC(IFRONT%P)
      CALL MY_DEALLOC(IFRONT%IENTRY)
      CALL MY_DEALLOC(IENTRY2)
      CALL MY_DEALLOC(DFLOW)
      CALL MY_DEALLOC(VFLOW)
      CALL MY_DEALLOC(WFLOW)
      CALL MY_DEALLOC(PERMUTATION%SOLID)
      IF(ALLOCATED(FILLSOL)) DEALLOCATE(FILLSOL)
      IF(ALLOCATED(SH3ANG))  CALL MY_DEALLOC(SH3ANG)
      IF(ALLOCATED(SH4ANG))  CALL MY_DEALLOC(SH4ANG)
      IF(ALLOCATED(NATIV_SMS))  CALL MY_DEALLOC(NATIV_SMS)
      IF(ALLOCATED(MULTI_FVM%VEL)) DEALLOCATE(MULTI_FVM%VEL)
      IF(ALLOCATED(MULTI_FVM%ACC)) DEALLOCATE(MULTI_FVM%ACC)
      CALL ALE_CONNECTIVITY%ALE_DEALLOCATE_CONNECTIVITY()
      IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) DEALLOCATE(STACK_INFO)
      IF(ALLOCATED(IPRELOAD)) CALL MY_DEALLOC(IPRELOAD)
      IF(ALLOCATED(PRELOAD))  CALL MY_DEALLOC(PRELOAD)
      IF(ALLOCATED(IFLAG_BPRELOAD))  CALL MY_DEALLOC(IFLAG_BPRELOAD)
      IF(ALLOCATED(IPRELOAD_FUN)) CALL MY_DEALLOC(IPRELOAD_FUN)
      IF(ALLOCATED(EOS_TAG))DEALLOCATE(EOS_TAG)
      CALL C_DELETE_HASH(H_NODE)
      CALL DEALLOCATE_DETONATORS(DETONATORS)
      IF(ALLOCATED(XSEED)) CALL MY_DEALLOC(XSEED)
      IF(ALLOCATED(ALEA))  CALL MY_DEALLOC(ALEA)
      IF(ALLOCATED(IRAND)) CALL MY_DEALLOC(IRAND)
      IF(ALLOCATED(SENSORS%SENSOR_TAB)) DEALLOCATE(SENSORS%SENSOR_TAB )
      IF(ALLOCATED(SENSORS%LOGICAL_SENSORS_LIST)) DEALLOCATE(SENSORS%LOGICAL_SENSORS_LIST)
      IF(ALLOCATED(DAMP_RANGE_PART)) CALL MY_DEALLOC(DAMP_RANGE_PART)
C
      CALL EBCS_TAB%destroy()
      IF(NINIMAP1D > 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
         DO KK = 1, NINIMAP1D
            CALL MY_DEALLOC(INIMAP1D(KK)%TAGNODE)
         ENDDO
      ENDIF
      DEALLOCATE(INIMAP1D)

      IF(NINIMAP2D > 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
         DO KK = 1, NINIMAP2D
            CALL MY_DEALLOC(INIMAP2D(KK)%TAGNODE)
         ENDDO
      ENDIF
      DEALLOCATE(INIMAP2D)


      CALL INVERTED_GROUP_DEALLOC(INV_GROUP)

      CALL DEALLOCATE_JOINT( )

      IF(NFXBODY>0) THEN
         CLOSE(IFXM)
         CLOSE(IFXS) 
      ENDIF
      call my_dealloc(isolnod0)
      call my_dealloc(ishell3nod0)

      IF(ALLOCATED( DGAPINT ))  CALL MY_DEALLOC(DGAPINT)

      IF(ALLOCATED(DPL0CLD)) CALL MY_DEALLOC(DPL0CLD)
      IF(ALLOCATED(VEL0CLD)) CALL MY_DEALLOC(VEL0CLD)
      IF(ALLOCATED(EBCS_TAG_CELL_SPMD)) CALL MY_DEALLOC(EBCS_TAG_CELL_SPMD)
      IF(ALLOCATED(FAIL_TAG)) DEALLOCATE(FAIL_TAG)
      call report_alloc()

C----------------------------------------------

      RETURN
      END
!||====================================================================
!||    set_ibufssg_io   ../starter/source/starter/lectur.F
!||--- called by ------------------------------------------------------
!||    lectur           ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ifrontplus       ../starter/source/spmd/node/frontplus.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE SET_IBUFSSG_IO(ISPHIO, IGRSURF, IBUFSSG_IO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPHIO(NISPHIO,*),
     .        IBUFSSG_IO(SIBUFSSG_IO),
     .        N,J,NSEG,IN1,IN2,IN3,IN4,
     .        ISU,PROC,IAD2,ITYPE
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      IAD2 = 1

      DO N=1,NSPHIO
        ITYPE = ISPHIO(1,N)
        IF(ISPHIO(12,N)==0) THEN
          ISU = ISPHIO(3,N)
          NSEG= IGRSURF(ISU)%NSEG
          ISPHIO(10,N) = NSEG
          ISPHIO(11,N) = IAD2
          DO J=0,NSEG-1
            IN1=IGRSURF(ISU)%NODES(J+1,1)
            IN2=IGRSURF(ISU)%NODES(J+1,2)
            IN3=IGRSURF(ISU)%NODES(J+1,3)
            IN4=IGRSURF(ISU)%NODES(J+1,4)
            IBUFSSG_IO(IAD2+NIBSPH*J)   = IN1
            IBUFSSG_IO(IAD2+NIBSPH*J+1) = IN2
            IBUFSSG_IO(IAD2+NIBSPH*J+2) = IN3
            IBUFSSG_IO(IAD2+NIBSPH*J+3) = IN4
            DO PROC=1,NSPMD
              CALL IFRONTPLUS(IN1,PROC)
              CALL IFRONTPLUS(IN2,PROC)
              CALL IFRONTPLUS(IN3,PROC)
              CALL IFRONTPLUS(IN4,PROC)
            ENDDO
          ENDDO
          IAD2 = IAD2 + 4*NSEG
        ELSEIF(ISPHIO(12,N)==2) THEN
          IN1 = ISPHIO(13,N)
          IN2 = ISPHIO(14,N)
          IN3 = ISPHIO(15,N)
          DO PROC=1,NSPMD
            CALL IFRONTPLUS(IN1,PROC)
            CALL IFRONTPLUS(IN2,PROC)
            CALL IFRONTPLUS(IN3,PROC)
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
      RETURN
      END
!||====================================================================
!||    init_permutation   ../starter/source/starter/lectur.F
!||--- called by ------------------------------------------------------
!||    lectur             ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||    reorder_mod        ../starter/share/modules1/reorder_mod.F
!||====================================================================
      SUBROUTINE INIT_PERMUTATION()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE REORDER_MOD

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I

        DO I=1,NUMELS
           PERMUTATION%SOLID(I)=I
        ENDDO
        DO I=1,NUMELC
           PERMUTATION%SHELL(I)=I
        ENDDO
        DO I=1,NUMELTG
           PERMUTATION%TRIANGLE(I)=I
        ENDDO
        do i=1,numelq
          permutation%quad(i)=i
        enddo

      RETURN
      END
!||====================================================================
!||    tet4_10   ../starter/source/starter/lectur.F
!||--- called by ------------------------------------------------------
!||    lectur    ../starter/source/starter/lectur.F
!||====================================================================
      SUBROUTINE TET4_10(IGEO,ITET4_10)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),ITET4_10
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,IGTYP,ITET4

        DO I=1,NUMGEO
         IGTYP =IGEO(11,I)
         ITET4 =IGEO(20,I)
         IF((IGTYP==14.OR.IGTYP==6).AND.ITET4==1) ITET4_10 = 1
        ENDDO

      RETURN
      END
