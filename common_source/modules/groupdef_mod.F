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
      !||    groupdef_mod                      ../common_source/modules/groupdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    airbagb                           ../engine/source/airbag/airbag2.F
      !||    airbagb1                          ../engine/source/airbag/airbagb1.F
      !||    ale_box_coloration                ../starter/source/initial_conditions/inivol/ale_box_coloration.F
      !||    alelin                            ../engine/source/ale/grid/alelin.F
      !||    alemain                           ../engine/source/ale/alemain.F
      !||    aletg                             ../starter/source/airbag/fvmbag1.F
      !||    alewdx                            ../engine/source/ale/grid/alewdx.F
      !||    alloc_group_str                   ../engine/source/groups/alloc_group_str.F
      !||    alloc_line_str                    ../engine/source/groups/alloc_line_str.F
      !||    alloc_subset_str                  ../engine/source/groups/alloc_subset_str.F
      !||    alloc_surf_str                    ../engine/source/groups/alloc_surf_str.F
      !||    anim_nodal_contour_fvmbags        ../engine/source/output/anim/generate/anim_nodal_contour_fvmbags.F
      !||    anim_nodal_vector_fvmbags         ../engine/source/output/anim/generate/anim_nodal_vector_fvmbags.F
      !||    bigbox                            ../starter/source/model/box/bigbox.F
      !||    bigbox2                           ../starter/source/model/box/bigbox.F
      !||    bigsbox                           ../starter/source/model/box/bigbox.F
      !||    bulkfakeigeo3                     ../starter/source/elements/ige3d/bulkfakeigeo3.F
      !||    c3grhead                          ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                         ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                           ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c_dampvrel                        ../starter/source/restart/ddsplit/c_dampvrel.F
      !||    c_gr_entity_e                     ../starter/source/model/group/c_gr_entity.F
      !||    c_gr_entity_n                     ../starter/source/model/group/c_gr_entity.F
      !||    c_gr_entity_p                     ../starter/source/model/group/c_gr_entity.F
      !||    c_group_str                       ../starter/source/restart/ddsplit/c_group_str.F
      !||    c_islin_str                       ../starter/source/restart/ddsplit/c_islin_str.F
      !||    c_isurf_str                       ../starter/source/restart/ddsplit/c_isurf_str.F
      !||    c_pon                             ../starter/source/restart/ddsplit/c_pon.F
      !||    cbainit3                          ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cgrhead                           ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                          ../starter/source/elements/shell/coque/cgrtails.F
      !||    check_surf                        ../starter/source/groups/check_surf.F
      !||    cinit3                            ../starter/source/elements/shell/coque/cinit3.F
      !||    create_map_tables                 ../starter/source/model/sets/map_tables.F
      !||    create_subs_clause                ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list                  ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list_g                ../starter/source/model/sets/create_subs_clause.F
      !||    dampdtnoda                        ../starter/source/general_controls/damping/dampdtnoda.F
      !||    damping                           ../engine/source/assembly/damping.F
      !||    damping44                         ../engine/source/assembly/damping.F
      !||    damping51                         ../engine/source/assembly/damping.F
      !||    damping_rby_spmdset               ../starter/source/general_controls/damping/damping_rby_spmdset.F90
      !||    damping_vref                      ../engine/source/assembly/damping_vref.F
      !||    damping_vref_rby                  ../engine/source/assembly/damping_vref_rby.F90
      !||    damping_vref_sum6_rby             ../engine/source/assembly/damping_vref_sum6_rby.F90
      !||    dampvref_sum6                     ../engine/source/assembly/dampvref_sum6.F
      !||    ddsplit                           ../starter/source/restart/ddsplit/ddsplit.F
      !||    deallocate_igrsurf_split          ../starter/source/spmd/deallocate_igrsurf_split.F
      !||    desacti                           ../engine/source/elements/desacti.F
      !||    dim_glob_k                        ../engine/source/implicit/ind_glob_k.F
      !||    domdec2                           ../starter/source/spmd/domdec2.F
      !||    dometis                           ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    dsrgcnt                           ../engine/source/output/anim/generate/dsrgcnt.F
      !||    dsrgnor                           ../engine/source/output/anim/generate/dsrgnor.F
      !||    dtnoda                            ../engine/source/time_step/dtnoda.F
      !||    dtnodamp                          ../engine/source/time_step/dtnodamp.F
      !||    dtnodams                          ../engine/source/time_step/dtnodams.F
      !||    dtnodarayl                        ../engine/source/time_step/dtnodarayl.F
      !||    dxyzsrg                           ../engine/source/output/anim/generate/dxyzsrg.F
      !||    ebcs_main                         ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    ecrsub2                           ../starter/source/model/assembling/hm_read_subset.F
      !||    eig                               ../engine/stub/eig.F
      !||    eig1                              ../engine/stub/eig1.F
      !||    eigcond                           ../engine/stub/eigcond.F
      !||    eigp                              ../engine/stub/eigp.F
      !||    elegror                           ../starter/source/groups/elegror.F
      !||    elegror_seatbelt                  ../starter/source/groups/elegror_seatbelt.F
      !||    eloff                             ../engine/source/elements/eloff.F
      !||    fail_windshield_init              ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||    fill_gr                           ../starter/source/model/sets/fill_gr.F
      !||    fill_igr                          ../starter/source/model/sets/fill_igr.F
      !||    fill_line                         ../starter/source/model/sets/fill_gr.F
      !||    fill_surf                         ../starter/source/model/sets/fill_gr.F
      !||    fill_surf_ellipse                 ../starter/source/model/sets/fill_gr_surf_ellipse.F
      !||    fill_surf_plane                   ../starter/source/model/sets/fill_gr_surf_plane.F90
      !||    fillcne                           ../starter/source/spmd/domdec2.F
      !||    filter_sae211                     ../engine/source/tools/sensor/filter_sae211.F
      !||    find_dt_for_targeted_added_mass   ../engine/source/time_step/find_dt_for_targeted_added_mass.F
      !||    forint                            ../engine/source/elements/forint.F
      !||    forintc                           ../engine/source/elements/forintc.F
      !||    fractal_dmg_init                  ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
      !||    fractal_element_neighbor          ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
      !||    fvbag_tagele                      ../starter/source/airbag/fvmbag1.F
      !||    fvbag_vertex                      ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    fvbric0                           ../starter/source/airbag/fvbric0.F
      !||    fvbric01                          ../starter/source/airbag/fvbric01.F
      !||    fvinject                          ../starter/source/airbag/fvinject.F
      !||    fvinjectint                       ../starter/source/airbag/fvinjectint.F
      !||    fvmesh0                           ../engine/source/airbag/fvmesh0.F
      !||    fvtgi                             ../starter/source/airbag/fvmbag1.F
      !||    fvventhole                        ../starter/source/airbag/fvventhole.F
      !||    fvventholeint                     ../starter/source/airbag/fvventholeint.F
      !||    genani                            ../engine/source/output/anim/generate/genani.F
      !||    genani1                           ../starter/source/output/anim/genani1.F
      !||    genh3d                            ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    gr_entity_ini                     ../starter/source/model/group/gr_entity_ini.F
      !||    grfind                            ../starter/source/starter/freform.F
      !||    group_ini                         ../engine/source/groups/group_ini.F
      !||    group_mod                         ../engine/share/modules/group_mod.F
      !||    grsize                            ../starter/source/system/nintrr.F
      !||    grsize_ele                        ../starter/source/system/nintrr.F
      !||    grsize_ele_trans                  ../starter/source/system/nintrr.F
      !||    grsize_r2r                        ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    grsizen                           ../starter/source/system/nintrr.F
      !||    h3d_create_fvmbag_centroids       ../engine/source/output/h3d/h3d_build_fortran/h3d_create_fvmbag_centroids.F
      !||    h3d_nodal_scalar                  ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    h3d_nodal_vector                  ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
      !||    h3d_update_fvmbag_centroids       ../engine/source/output/h3d/h3d_build_fortran/h3d_update_fvmbag_centroids.F
      !||    h3d_velvecz22                     ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
      !||    hist1                             ../engine/source/output/th/hist1.F
      !||    hist2                             ../engine/source/output/th/hist2.F
      !||    hm_bigbox                         ../starter/source/model/box/hm_bigbox.F
      !||    hm_bigbox2                        ../starter/source/model/box/hm_bigbox2.F
      !||    hm_bigsbox                        ../starter/source/groups/hm_bigsbox.F
      !||    hm_debug_print_option             ../starter/source/devtools/hm_reader/hm_debug_print_option.F
      !||    hm_elngr                          ../starter/source/groups/hm_elngr.F
      !||    hm_elngrr                         ../starter/source/groups/hm_elngrr.F
      !||    hm_elngrs                         ../starter/source/groups/hm_elngr.F
      !||    hm_grogro                         ../starter/source/groups/hm_grogro.F
      !||    hm_grogronod                      ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                         ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                         ../starter/source/groups/hm_lecgrn.F
      !||    hm_linengr                        ../starter/source/groups/hm_linengr.F
      !||    hm_lines_of_lines                 ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_pre_read_link                  ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_pre_read_preload_axial         ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_prelecgrns                     ../starter/source/groups/hm_prelecgrns.F
      !||    hm_prelecjoi                      ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
      !||    hm_preread_bcscyc                 ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    hm_preread_bem                    ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_cload                  ../starter/source/loads/general/cload/hm_preread_cload.F
      !||    hm_preread_convec                 ../starter/source/loads/thermic/hm_preread_convec.F
      !||    hm_preread_eig                    ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_preread_grav                   ../starter/source/loads/general/grav/hm_preread_grav.F
      !||    hm_preread_impacc                 ../starter/source/constraints/general/impvel/hm_preread_impacc.F
      !||    hm_preread_impdisp                ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
      !||    hm_preread_impflux                ../starter/source/constraints/thermic/hm_preread_impflux.F
      !||    hm_preread_imptemp                ../starter/source/constraints/thermic/hm_preread_imptemp.F
      !||    hm_preread_impvel                 ../starter/source/constraints/general/impvel/hm_preread_impvel.F
      !||    hm_preread_load_centri            ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
      !||    hm_preread_load_pressure          ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
      !||    hm_preread_merge                  ../starter/source/constraints/general/merge/hm_preread_merge.F
      !||    hm_preread_pblast                 ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_preread_pfluid                 ../starter/source/loads/general/pfluid/hm_preread_pfluid.F
      !||    hm_preread_pload                  ../starter/source/loads/general/pload/hm_preread_pload.F
      !||    hm_preread_radiation              ../starter/source/loads/thermic/hm_preread_radiation.F
      !||    hm_preread_rbe2                   ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_rbe3                   ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_preread_rbody                  ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_preread_sphio                  ../starter/source/loads/sph/hm_preread_sphio.F
      !||    hm_preread_xelem                  ../starter/source/elements/reader/hm_preread_xelem.F
      !||    hm_read_activ                     ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_admas                     ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_ale_link                  ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_alebcs                    ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_bcs                       ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_bcs_wall                  ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_bem                       ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_cload                     ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cluster                   ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_convec                    ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_cyljoint                  ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_damp                      ../starter/source/general_controls/damping/hm_read_damp.F
      !||    hm_read_drape                     ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_dt                        ../starter/source/general_controls/engine/hm_read_dt.F
      !||    hm_read_ebcs_fluxout              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0               ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol               ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf                  ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_valvin               ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel                  ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_eig                       ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_friction                  ../starter/source/interfaces/friction/reader/hm_read_friction.F
      !||    hm_read_friction_models           ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations     ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_grav                      ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_grpart                    ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_impacc                    ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impflux                   ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_imptemp                   ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_impvel                    ../starter/source/constraints/general/impvel/hm_read_impvel.F
      !||    hm_read_inigrav                   ../starter/source/initial_conditions/inigrav/hm_read_inigrav.F
      !||    hm_read_inimap1d                  ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d                  ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inistate_d00              ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_initemp                   ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                    ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inivol                    ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_read_inter_fsi                 ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||    hm_read_inter_hertz_type17        ../starter/source/interfaces/int17/hm_read_inter_hertz_type17.F
      !||    hm_read_inter_lagdt_type07        ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_lagmul              ../starter/source/interfaces/reader/hm_read_inter_lagmul.F
      !||    hm_read_inter_lagmul_type02       ../starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
      !||    hm_read_inter_lagmul_type07       ../starter/source/interfaces/int07/hm_read_inter_lagmul_type07.F
      !||    hm_read_inter_lagmul_type16       ../starter/source/interfaces/int16/hm_read_inter_lagmul_type16.F
      !||    hm_read_inter_lagmul_type17       ../starter/source/interfaces/int17/hm_read_inter_lagmul_type17.F
      !||    hm_read_inter_struct              ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type01              ../starter/source/interfaces/int01/hm_read_inter_type01.F
      !||    hm_read_inter_type02              ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_inter_type03              ../starter/source/interfaces/int03/hm_read_inter_type03.F
      !||    hm_read_inter_type05              ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type06              ../starter/source/interfaces/int06/hm_read_inter_type06.F
      !||    hm_read_inter_type07              ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type08              ../starter/source/interfaces/int08/hm_read_inter_type08.F
      !||    hm_read_inter_type09              ../starter/source/interfaces/int09/hm_read_inter_type09.F
      !||    hm_read_inter_type10              ../starter/source/interfaces/int10/hm_read_inter_type10.F
      !||    hm_read_inter_type11              ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type12              ../starter/source/interfaces/int12/hm_read_inter_type12.F
      !||    hm_read_inter_type14              ../starter/source/interfaces/int14/hm_read_inter_type14.F
      !||    hm_read_inter_type15              ../starter/source/interfaces/int15/hm_read_inter_type15.F
      !||    hm_read_inter_type18              ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_inter_type20              ../starter/source/interfaces/int20/hm_read_inter_type20.F
      !||    hm_read_inter_type21              ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type22              ../starter/source/interfaces/int22/hm_read_inter_type22.F
      !||    hm_read_inter_type23              ../starter/source/interfaces/int23/hm_read_inter_type23.F
      !||    hm_read_inter_type24              ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25              ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces                ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub                    ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_lines                     ../starter/source/groups/hm_read_lines.F
      !||    hm_read_link                      ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_load_centri               ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure             ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_merge                     ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_merge_node                ../starter/source/elements/reader/hm_read_merge_node.F
      !||    hm_read_monvol_type1              ../starter/source/airbag/hm_read_monvol_type1.F
      !||    hm_read_monvol_type10             ../starter/source/airbag/hm_read_monvol_type10.F
      !||    hm_read_monvol_type11             ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type2              ../starter/source/airbag/hm_read_monvol_type2.F
      !||    hm_read_monvol_type3              ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4              ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5              ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6              ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7              ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8              ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9              ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_nbcs                      ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_pblast                    ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pcyl                      ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_perturb                   ../starter/source/general_controls/computation/hm_read_perturb.F
      !||    hm_read_perturb_fail              ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_perturb_part_shell        ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_perturb_part_solid        ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
      !||    hm_read_pfluid                    ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                     ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_preload_axial             ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_prethgrou                 ../starter/source/output/th/hm_read_prethgrou.F
      !||    hm_read_radiation                 ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_rand                      ../starter/source/general_controls/computation/hm_read_rand.F
      !||    hm_read_rbe2                      ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                      ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                     ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul              ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_retractor                 ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rwall_cyl                 ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul              ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral               ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane               ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher               ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm               ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_sensors                   ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_slipring                  ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_spcnd                     ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_sphio                     ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_subset                    ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                      ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                  ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_thgrou                    ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrpa_sub                ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrsens                  ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf                  ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_thpart                    ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_xelem                     ../starter/source/elements/reader/hm_read_xelem.F
      !||    hm_set                            ../starter/source/model/sets/hm_set.F
      !||    hm_submodgre                      ../starter/source/groups/hm_submodgr.F
      !||    hm_submodgrn                      ../starter/source/groups/hm_submodgr.F
      !||    hm_surfgr2                        ../starter/source/groups/hm_surfgr2.F
      !||    hm_surfnod                        ../starter/source/groups/hm_surfnod.F
      !||    hm_tagpart                        ../starter/source/groups/hm_tagpart.F
      !||    hm_tagpart2                       ../starter/source/groups/hm_tagpart2.F
      !||    hm_yctrl                          ../starter/source/elements/initia/hm_yctrl.F
      !||    i14can                            ../engine/source/interfaces/int14/i14can.F
      !||    i14cmp                            ../engine/source/interfaces/int14/i14cmp.F
      !||    i14dmp                            ../engine/source/interfaces/int14/i14dmp.F
      !||    i14ela                            ../engine/source/interfaces/int14/i14ela.F
      !||    i14frt                            ../engine/source/interfaces/int14/i14frt.F
      !||    i14ist                            ../engine/source/interfaces/int14/i14ist.F
      !||    i14wfs                            ../engine/source/interfaces/int14/i14wfs.F
      !||    i15ass                            ../engine/source/interfaces/int15/i15ass.F
      !||    i15can                            ../engine/source/interfaces/int15/i15can.F
      !||    i15cmp                            ../engine/source/interfaces/int15/i15cmp.F
      !||    i15for1                           ../engine/source/interfaces/int15/i15for1.F
      !||    i15fort1                          ../engine/source/interfaces/int15/i15fort1.F
      !||    i15tot1                           ../engine/source/interfaces/int15/i15tot1.F
      !||    i15tott1                          ../engine/source/interfaces/int15/i15tott1.F
      !||    i16main                           ../engine/source/interfaces/int16/i16main.F
      !||    i17main                           ../engine/source/interfaces/int17/i17main.F
      !||    i17main_crit_tri                  ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17main_pena                      ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17main_tri                       ../engine/source/interfaces/int17/i17main_pena.F
      !||    i1chk3                            ../starter/source/interfaces/inter3d1/i1chk3.F
      !||    i20ini3                           ../starter/source/interfaces/inter3d1/i20ini3.F
      !||    i20sti3                           ../starter/source/interfaces/inter3d1/i20sti3.F
      !||    i20surfi                          ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i21els3                           ../starter/source/interfaces/inter3d1/i21els3.F
      !||    i22main_tri                       ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                          ../engine/source/interfaces/int22/i22mainf.F
      !||    i22subvol                         ../engine/source/interfaces/int22/i22subvol.F
      !||    i24gapm                           ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24sti3                           ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24surfi                          ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i25surfi                          ../starter/source/interfaces/inter3d1/i25surfi.F
      !||    i2chk3                            ../starter/source/interfaces/inter3d1/i2chk3.F
      !||    i3sti3                            ../starter/source/interfaces/inter3d1/i3sti3.F
      !||    i7mainf                           ../engine/source/interfaces/int07/i7mainf.F
      !||    i7sti3                            ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    i9sti3                            ../starter/source/interfaces/int09/i9sti3.F
      !||    ig3dgrhead                        ../starter/source/elements/ige3d/ig3dgrhead.F
      !||    ig3dgrtails                       ../starter/source/elements/ige3d/ig3dgrtails.F
      !||    igrsurf_split                     ../starter/source/spmd/igrsurf_split.F
      !||    imp_buck                          ../engine/source/implicit/imp_buck.F
      !||    imp_inisi                         ../engine/source/implicit/imp_pcg.F
      !||    imp_inist                         ../engine/source/implicit/imp_pcg.F
      !||    imp_inttd0                        ../engine/source/implicit/imp_int_k.F
      !||    imp_lanzp                         ../engine/source/implicit/imp_lanz.F
      !||    imp_pcgh                          ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh                         ../engine/source/implicit/imp_pcg.F
      !||    imp_sol_init                      ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv                          ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                         ../engine/source/implicit/imp_int_k.F
      !||    imp_updst                         ../engine/source/implicit/imp_pcg.F
      !||    imp_updv2                         ../engine/source/implicit/imp_pcg.F
      !||    ini_fvminivel                     ../starter/source/elements/initia/ini_fvminivel.F
      !||    ini_inimap1d                      ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    ini_inimap2d                      ../starter/stub/ini_inimap2d.F
      !||    iniebcs                           ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    inigrav_load                      ../starter/source/initial_conditions/inigrav/inigrav_load.F
      !||    inigrav_part_list                 ../starter/source/initial_conditions/inigrav/inigrav_part_list.F
      !||    inint3                            ../starter/source/interfaces/inter3d1/inint3.F
      !||    inintr                            ../starter/source/interfaces/interf1/inintr.F
      !||    inintsub                          ../starter/source/interfaces/interf1/inintsub.F
      !||    inintsub_11                       ../starter/source/output/subinterface/inintsub_11.F
      !||    inintsub_25                       ../starter/source/output/subinterface/inintsub_25.F
      !||    inintsub_7                        ../starter/source/output/subinterface/inintsub_7.F
      !||    inisms                            ../starter/source/general_controls/computation/hm_read_sms.F
      !||    inisoldist                        ../starter/source/initial_conditions/inivol/inisoldist.F
      !||    inisrf                            ../starter/source/constraints/general/rbody/inisrf.F
      !||    init_bcs_wall                     ../starter/source/boundary_conditions/init_bcs_wall.F90
      !||    init_inivol                       ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_inivol_2d_polygons           ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||    init_monvol                       ../starter/source/airbag/init_monvol.F
      !||    init_th                           ../engine/source/output/th/init_th.F
      !||    init_th0                          ../engine/source/output/th/init_th0.F
      !||    init_th_group                     ../engine/source/output/th/init_th_group.F
      !||    initia                            ../starter/source/elements/initia/initia.F
      !||    inivel                            ../starter/source/initial_conditions/general/inivel/inivel.F
      !||    inivol_interf_mod                 ../starter/share/modules1/inivol_mod.F
      !||    inspcnd                           ../starter/source/elements/sph/inspcnd.F
      !||    int18_alloc                       ../engine/source/interfaces/int18/int18_alloc.F
      !||    int18_law151_alloc                ../starter/source/interfaces/int18/int18_law151_alloc.F
      !||    int18_law151_init                 ../engine/source/interfaces/int18/int18_law151_init.F
      !||    int18_law151_nsv_shift            ../engine/source/interfaces/int18/int18_law151_nsv_shift.F
      !||    int18_law151_update               ../engine/source/interfaces/int18/int18_law151_update.F
      !||    inter_offset_itag                 ../starter/source/elements/shell/shell_offset/inter_offset_itag.F90
      !||    intfop1                           ../engine/source/interfaces/interf/intfop1.F
      !||    intfop2                           ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                            ../engine/source/interfaces/intsort/inttri.F
      !||    ipari_l_ini                       ../starter/source/restart/ddsplit/ipari_l_ini.F
      !||    islin_ini                         ../starter/source/model/group/islin_ini.F
      !||    isurf_ini                         ../starter/source/model/group/isurf_ini.F
      !||    lag_bcs                           ../engine/source/tools/lagmul/lag_bcs.F
      !||    lag_mult                          ../engine/source/tools/lagmul/lag_mult.F
      !||    lagm_ini                          ../starter/source/tools/lagmul/lagm_ini.F
      !||    lce16q3                           ../starter/source/elements/solid_2d/quad/lce16q.F
      !||    lce16s3                           ../starter/source/elements/reader/hm_read_solid.F
      !||    lec_inimap1d_file                 ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lec_inimap2d_file                 ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    lec_inistate                      ../starter/source/elements/initia/lec_inistate.F
      !||    lecdamp                           ../engine/source/input/lecdamp.F
      !||    lecggroup                         ../starter/source/groups/lecggroup.F
      !||    lecgroup                          ../starter/source/groups/lecgroup.F
      !||    lecins                            ../starter/source/interfaces/interf1/lecins.F
      !||    lecint                            ../starter/source/interfaces/interf1/lecint.F
      !||    lecinv                            ../engine/source/input/lecinv.F
      !||    lecsec42                          ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                       ../starter/source/tools/sect/lecsec4bolt.F
      !||    lectrans                          ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                       ../starter/source/model/submodel/lectranssub.F
      !||    lectur                            ../engine/source/input/lectur.F
      !||    lgmini_bc                         ../starter/source/tools/lagmul/lgmini_bc.F
      !||    lgmini_i7                         ../starter/source/tools/lagmul/lgmini_i7.F
      !||    lin_solv                          ../engine/source/implicit/lin_solv.F
      !||    lin_solvh0                        ../engine/source/implicit/lin_solv.F
      !||    lin_solvh1                        ../engine/source/implicit/lin_solv.F
      !||    lin_solvhm                        ../engine/source/implicit/lin_solv.F
      !||    lin_solvih2                       ../engine/source/implicit/lin_solv.F
      !||    line_decomp                       ../starter/source/model/group/line_decomp.F
      !||    line_ini                          ../engine/source/groups/line_ini.F
      !||    ltag_bcs                          ../engine/source/tools/lagmul/lag_ntag.F
      !||    mav_lt2                           ../engine/source/implicit/produt_v.F
      !||    mav_lth                           ../engine/source/implicit/produt_v.F
      !||    mav_lth0                          ../engine/source/implicit/produt_v.F
      !||    mav_ltp                           ../engine/source/implicit/produt_v.F
      !||    merge_node                        ../starter/source/elements/nodes/merge_node.F
      !||    mmav_lth                          ../engine/source/implicit/produt_v.F
      !||    monv_imp                          ../engine/source/airbag/monv_imp0.F
      !||    monv_kd                           ../engine/source/airbag/monv_imp0.F
      !||    monv_m3                           ../engine/source/airbag/monv_imp0.F
      !||    monv_prem                         ../engine/source/airbag/monv_imp0.F
      !||    monvol0                           ../engine/source/airbag/monvol0.F
      !||    monvol_build_edges                ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_check_surfclose            ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_check_venthole_surf        ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_compute_volume             ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_orient_surf                ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_reverse_normals            ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_triangulate_surface        ../starter/source/airbag/monvol_triangulate_surface.F
      !||    mpp_init                          ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    mv_matv                           ../engine/source/airbag/monv_imp0.F
      !||    ngr2usr                           ../engine/source/input/freform.F
      !||    ngr2usrn                          ../starter/source/system/nintrr.F
      !||    nintrigr                          ../starter/source/system/nintrr.F
      !||    nl_solv                           ../engine/source/implicit/nl_solv.F
      !||    nodald                            ../engine/source/output/anim/generate/nodald.F
      !||    nodaldt                           ../engine/source/output/anim/generate/nodaldt.F
      !||    nodalp                            ../engine/source/output/anim/generate/nodalp.F
      !||    nodalssp                          ../engine/source/output/anim/generate/nodalssp.F
      !||    nodalt                            ../engine/source/output/anim/generate/nodalt.F
      !||    nodalvol                          ../engine/source/output/anim/generate/nodalvol.F
      !||    nodgr_r2r                         ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    nodgrnr5                          ../starter/source/starter/freform.F
      !||    nodgrnr6                          ../starter/source/starter/freform.F
      !||    pblast                            ../engine/source/loads/pblast/pblast.F
      !||    pblast_1                          ../engine/source/loads/pblast/pblast_1.F
      !||    pblast_2                          ../engine/source/loads/pblast/pblast_2.F
      !||    pblast_3                          ../engine/source/loads/pblast/pblast_3.F
      !||    pgrhead                           ../starter/source/elements/beam/pgrhead.F
      !||    pgrtails                          ../starter/source/elements/beam/pgrtails.F
      !||    pre_stackgroup                    ../starter/source/stack/pres_stackgroup.F
      !||    prelecdet                         ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    prelecsec                         ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt                    ../starter/source/tools/sect/prelecsec4bolt.F
      !||    prepare_split_i17                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    preread_rbody_lagmul              ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||    presearchigeo3d                   ../starter/source/elements/ige3d/searchigeo3d.F
      !||    printbcs                          ../starter/source/constraints/general/bcs/printbcs.F
      !||    printgroup                        ../starter/source/output/outfile/printgroup.F
      !||    qa_print_groups                   ../starter/source/output/qaprint/st_qaprint_groups.F
      !||    qa_print_surf                     ../starter/source/output/qaprint/st_qaprint_surf.F
      !||    qgrhead                           ../starter/source/elements/solid_2d/quad/qgrhead.F
      !||    qgrtails                          ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    qsurftag                          ../starter/source/groups/qsurftag.F
      !||    r2r_check                         ../starter/source/coupling/rad2rad/r2r_check.F
      !||    r2r_count                         ../starter/source/coupling/rad2rad/r2r_count.F
      !||    r2r_domdec                        ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_exch_itag                     ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    r2r_exchange                      ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    r2r_exist                         ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_getdata                       ../engine/source/coupling/rad2rad/r2r_getdata.F
      !||    r2r_group                         ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_init                          ../engine/source/coupling/rad2rad/r2r_init.F
      !||    r2r_monvol                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_sendkine                      ../engine/source/coupling/rad2rad/r2r_getdata.F
      !||    r2r_split                         ../starter/source/coupling/rad2rad/r2r_split.F
      !||    radioss2                          ../engine/source/engine/radioss2.F
      !||    random_walk_dmg                   ../starter/source/materials/fail/fractal/random_walk_dmg.F90
      !||    ratio_fill                        ../starter/source/initial_conditions/inivol/ratio_fill.F
      !||    rbyfor                            ../engine/source/constraints/general/rbody/rbyfor.F
      !||    read_detonators                   ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_dfs_detcord                  ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline                  ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan                  ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint                 ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper              ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_ebcs                         ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_engine_driver                ../starter/source/general_controls/engine/read_engine_driver.F
      !||    read_impdisp                      ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impdisp_fgeo                 ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                       ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo                  ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul                ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_monvol                       ../starter/source/airbag/read_monvol.F
      !||    read_rwall                        ../starter/source/constraints/general/rwall/read_rwall.F
      !||    read_sensor_energy                ../starter/source/tools/sensor/read_sensor_energy.F
      !||    read_sensor_nic                   ../starter/source/tools/sensor/read_sensor_nic.F
      !||    read_sensor_temp                  ../starter/source/tools/sensor/read_sensor_temp.F
      !||    reconnect                         ../starter/source/elements/nodes/reconnect.F
      !||    reordr                            ../starter/source/elements/solid_2d/quad/reordr.F
      !||    resol                             ../engine/source/engine/resol.F
      !||    resol_head                        ../engine/source/engine/resol_head.F
      !||    resol_init                        ../engine/source/engine/resol_init.F
      !||    rgrhead                           ../starter/source/elements/spring/rgrhead.F
      !||    rgrtails                          ../starter/source/elements/spring/rgrtails.F
      !||    s10cnidamp                        ../engine/source/elements/solid/solide10/s10cndf.F
      !||    s10cnistat                        ../engine/source/elements/solid/solide10/s10cndf.F
      !||    sboxboxsurf                       ../starter/source/model/box/bigbox.F
      !||    searchigeo3d                      ../starter/source/elements/ige3d/searchigeo3d.F
      !||    searchigeo3d2                     ../starter/source/elements/ige3d/searchigeo3d.F
      !||    searchigeo3d3                     ../starter/source/elements/ige3d/searchigeo3d.F
      !||    searchigeo3dold                   ../starter/source/elements/ige3d/searchigeo3d.F
      !||    sec_nodes                         ../starter/source/tools/sect/hm_read_sect.F
      !||    sec_nodes_sol                     ../starter/source/tools/sect/hm_read_sect.F
      !||    sensor_base                       ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_dist_surf                  ../engine/source/tools/sensor/sensor_dist_surf.F
      !||    sensor_dist_surf0                 ../engine/source/tools/sensor/sensor_dist_surf0.F
      !||    sensor_energy                     ../engine/source/tools/sensor/sensor_energy.F
      !||    sensor_energy_part                ../engine/source/tools/sensor/sensor_energy_part.F
      !||    sensor_hic                        ../engine/source/tools/sensor/sensor_hic.F
      !||    sensor_init                       ../engine/source/tools/sensor/sensor_init.F
      !||    sensor_nic                        ../engine/source/tools/sensor/sensor_nic.F
      !||    sensor_spmd                       ../engine/source/tools/sensor/sensor_spmd.F
      !||    sensor_temp                       ../engine/source/tools/sensor/sensor_temp.F
      !||    sensor_temp0                      ../engine/source/tools/sensor/sensor_temp0.F
      !||    set_ibufssg_io                    ../starter/source/starter/lectur.F
      !||    seteloff                          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    seteloff2                         ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    setrb2on                          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    setrbyon                          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sgrhead                           ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                          ../starter/source/elements/solid/solide/sgrtails.F
      !||    sinit22_fvm                       ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    sms_ini_part                      ../engine/source/ams/sms_init.F
      !||    sms_mass_scale_2                  ../engine/source/ams/sms_mass_scale_2.F
      !||    sort_surf                         ../starter/source/groups/sort_surf.F
      !||    sortie_main                       ../engine/source/output/sortie_main.F
      !||    sphprep                           ../engine/source/elements/sph/sphprep.F
      !||    split_ebcs                        ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    split_interfaces                  ../starter/source/restart/ddsplit/split_interfaces.F
      !||    spmd_i7fcom_poff                  ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                   ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2                     ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    sponof1                           ../engine/source/elements/sph/sponof1.F
      !||    sponof2                           ../engine/source/elements/sph/sponof2.F
      !||    srfvit                            ../engine/source/constraints/general/rbody/srfvit.F
      !||    ssurfigeo                         ../starter/source/groups/ssurftagigeo.F
      !||    ssurftag                          ../starter/source/groups/ssurftag.F
      !||    ssurftagigeo                      ../starter/source/groups/ssurftagigeo.F
      !||    st_qaprint_dfs_detonators         ../starter/source/output/qaprint/st_qaprint_dfs_detonators.F
      !||    st_qaprint_dfs_lasers             ../starter/source/output/qaprint/st_qaprint_dfs_lasers.F
      !||    st_qaprint_driver                 ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_groups                 ../starter/source/output/qaprint/st_qaprint_groups.F
      !||    st_qaprint_inivol                 ../starter/source/output/qaprint/st_qaprint_inivol.F
      !||    st_qaprint_internal_groups        ../starter/source/output/qaprint/st_qaprint_internal_groups.F
      !||    st_qaprint_madymo                 ../starter/source/output/qaprint/st_qaprint_madymo.F
      !||    st_qaprint_multidomains           ../starter/source/output/qaprint/st_qaprint_multidomains.F
      !||    st_qaprint_set                    ../starter/source/output/qaprint/st_qaprint_set.F
      !||    st_qaprint_surf                   ../starter/source/output/qaprint/st_qaprint_surf.F
      !||    st_qaprint_thgrou                 ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    st_qaprint_time_histories         ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    st_qaprint_transformations        ../starter/source/output/qaprint/st_qaprint_transformations.F
      !||    stackgroup                        ../starter/source/stack/stackgroup.F
      !||    stackgroup_drape                  ../starter/source/stack/stackgroup_drape.F
      !||    static                            ../engine/source/general_controls/damping/static.F
      !||    subset_ini                        ../engine/source/groups/subset_ini.F
      !||    surf_ini                          ../engine/source/groups/surf_ini.F
      !||    surf_mass_monv                    ../engine/source/output/th/surf_mass.F
      !||    surface_min_max_computation       ../starter/source/initial_conditions/inivol/surface_min_max_computation.F
      !||    surftag                           ../starter/source/groups/surftag.F
      !||    surftagadm                        ../starter/source/groups/surftag.F
      !||    surftage                          ../starter/source/groups/surftage.F
      !||    surftagx                          ../starter/source/groups/surftag.F
      !||    switch_to_dtnoda                  ../engine/source/time_step/switch_to_dtnoda.F
      !||    t3grhead                          ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                         ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tag_elem_void_r2r                 ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tag_elem_void_r2r_lin             ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tagint_r2r                        ../starter/source/coupling/rad2rad/tagint_r2r.F
      !||    tagoff3n                          ../engine/source/interfaces/interf/chkstfn3.F
      !||    tgrhead                           ../starter/source/elements/truss/tgrhead.F
      !||    tgrtails                          ../starter/source/elements/truss/tgrtails.F
      !||    th_surf_load_pressure             ../starter/source/output/th/th_surf_load_pressure.F
      !||    thprin_sub                        ../starter/source/output/th/thprin.F
      !||    tsurftag                          ../starter/source/groups/tsurftag.F
      !||    updfail                           ../starter/source/materials/updfail.F90
      !||    velvecz22                         ../engine/source/output/anim/generate/velvecz22.F
      !||    volpvgb                           ../engine/source/airbag/volpvg.F
      !||    w_dampvrel                        ../starter/source/restart/ddsplit/w_dampvrel.F
      !||    w_front                           ../starter/source/restart/ddsplit/w_front.F
      !||    w_gr_entity                       ../engine/source/output/restart/w_gr_entity.F
      !||    w_gr_entity_e                     ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_gr_entity_n                     ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_gr_entity_p                     ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_group_str                       ../engine/source/output/restart/w_group_str.F
      !||    w_islin_str                       ../starter/source/restart/ddsplit/w_islin_str.F
      !||    w_isurf_str                       ../starter/source/restart/ddsplit/w_isurf_str.F
      !||    w_line_str                        ../engine/source/output/restart/w_line_str.F
      !||    w_pon                             ../starter/source/restart/ddsplit/w_pon.F
      !||    w_subset_str                      ../engine/source/output/restart/w_subset_str.F
      !||    w_surf_str                        ../engine/source/output/restart/w_surf_str.F
      !||    wrrestp                           ../engine/source/output/restart/wrrestp.F
      !||    xgrhead                           ../starter/source/elements/xelem/xgrhead.F
      !||    xgrtails                          ../starter/source/elements/xelem/xgrtails.F
      !||    yctrl                             ../starter/source/initial_conditions/inista/yctrl.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod              ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      MODULE GROUPDEF_MOD
C----------------------------------------------------------------------- 
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
c=======================================================================      
!---------
!   NEW DATA STRUCTURE
!---------
c=======================================================================   
c=======================================================================   
!                               SUBSETS
c=======================================================================   
C=======================================================================
      TYPE SUBSET_
        INTEGER   :: ID       ! - SUBSET identifier
        INTEGER   :: LEVEL    ! - Hierarchy level
        INTEGER   :: TH_FLAG  ! - TIME HISTORY flag
        INTEGER   :: PARENT   ! - SUBSET PARENT
        INTEGER   :: NCHILD   ! - SUBSETS number of childs
        INTEGER   :: NPART    ! - SUBSET number of part (within one subset)
        INTEGER   :: NTPART   ! - Total number of parts (within parent subset)
        INTEGER   :: THIAD    ! - IAD for global ITHBUF storage variables
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: CHILD ! - SUBSET's child
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: PART  ! - SUBSET's part
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: TPART ! - PARTS  (parts of the parent SUBSET)
!---TIME HISTORY ( /TH et /ATH )
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: NVARTH ! - ! nb of subset TH/ATH variable
!---
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! SUBSET title
      END TYPE SUBSET_
C-----------------------------------------------------------------------

c=======================================================================   
c=======================================================================   
!                               GROUPS
c=======================================================================   
C=======================================================================

C-----------------------------------------------------------------------
      TYPE GROUP_
        INTEGER   :: ID ! - GROUP identifier
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! GROUP title
        INTEGER   :: SET_GROUP ! need to not printout empty set groups
        INTEGER   :: NENTITY ! - Entities' number of one the GROUP (NODES, ELEMENTS, PARTS)
        INTEGER, DIMENSION(:),ALLOCATABLE :: ENTITY   ! -Entity identifier (node, element or part)
        INTEGER   :: GRTYPE ! TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                    4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N )
!!                   GRTYPE --- > OBSOLETE
        INTEGER   :: SORTED ! FLAG for sorted/unsorted nodes
!                    = 0 ! sorted
!                    = 1 ! unsorted
        INTEGER   :: GRPGRP ! TYPE OF nodal (element, part) GROUP
!  For node group:
!                                 = 1 FOR /GRNOD/NOD
!                                 = 2 FOR /GRNOD/GNROD
!                                 = 0 ALL THE REST
!  For element (part) group:
!                                 = 1 ELEM
!                                 = 2 GRELEM
!                                 = 3 SUBSET,PART,MAT,PROP
!                                 = 4 BOX,GENE
!                                 = 5 SUBMOD
        INTEGER   :: LEVEL ! Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
!--------------
!---
!  - R2R -
!                              ! R2R_ALL <--- IGROUP2(8,I) = IGROUP2(2,I) -- before splitting
!                              ! R2R_SHARE <--- IGROUP2(9,I)   (shared on boundary subdomain)
        INTEGER   :: R2R_ALL   ! Multidomaines -> number of node(elems, parts) before split
        INTEGER   :: R2R_SHARE ! shared on boundary subdomain
!--------------
      END TYPE GROUP_
C-----------------------------------------------------------------------

c=======================================================================   
c=======================================================================   
!                               SURFS
c=======================================================================   
C=======================================================================

C-----------------------------------------------------------------------
      TYPE SURF_
        INTEGER   :: ID ! - SURFACE (or LINE) identifier
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! SURF title
        INTEGER   :: NSEG ! - Number of surfaces (lines) within  /SURF
        INTEGER   :: NSEG_IGE    ! Number of iso-surfaces
        INTEGER   :: IAD_IGE    ! Pointer to fictive node in IGA tabulars
        INTEGER   :: SET_GROUP ! need to not printout empty set groups
        INTEGER   :: TYPE ! OPEN / CLOSED surface flag
!                        SURF_TYPE = 0         : SEGMENTS
!                        SURF_TYPE = 100       : HYPER-ELLIPSOIDE MADYMO.
!                        SURF_TYPE = 101       : HYPER-ELLIPSOIDE RADIOSS.
!                        SURF_TYPE = 200       : INFINITE PLANE
        INTEGER   :: ID_MADYMO  ! Coupled madimo surface identifier
!                                 (computed in Radioss Engine, when receiving Datas from MaDyMo).
!                                 ID MaDyMo - for entity type which impose surface movement:
!                                 No systeme MaDyMo for entity type which impose surface movement
        INTEGER   :: IAD_BUFR   ! Analytical Surfaces address (reals BUFSF - temp)
        INTEGER   :: NB_MADYMO  ! No de l'entite qui impose le mvt de la surface.
!                                --> No systeme Radioss ou MaDyMO.
        INTEGER   :: TYPE_MADYMO  ! Entity type which impose surface movement.
!                                  = 1 : Rigid Body.
!                                  = 2 : MADYMO Hyper-ellipsoide.
        INTEGER   :: LEVEL        ! FLAG "SUBLEVEL DONE" FOR SURFACES OF SURFACES
!                                 = 0 ! initialized surface
!                                 = 1 ! uninitialized surface
        INTEGER   :: TH_SURF      ! FLAG for /TH/SURF
!                                 = 0 ! unsaved surface for /TH/SURF
!                                 = 1 ! saved surface for /TH/SURF
        INTEGER   :: ISH4N3N      ! FLAG = 1 (only SH4N and SH3N considered - for airbags)
!
        INTEGER   :: NSEG_R2R_ALL   ! Multidomaines -> number of segments before split
        INTEGER   :: NSEG_R2R_SHARE ! shared on boundary subdomain segments
!
        INTEGER       , DIMENSION(:) , ALLOCATABLE :: REVERSED
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELTYP  ! dim = (NSEG)
 ! - type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELEM   ! dim = (NSEG)
        INTEGER   , DIMENSION(:,:), ALLOCATABLE :: NODES  ! dim =(NSEG,4) -SURF
!                                                         ! dim =(NSEG,2) -LINE
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: PROC   ! dim = (NSEG)
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: LOCAL_SEG   ! dim = (NSEG)
!                               field to store the processor ID  (/LINE only)
!
!   1. Set processor only when no element is set in lines
!   2. Split lines accordingly
!
!  ISOGEOMETRIQUE :
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELTYP_IGE  ! dim = (NSEG)
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELEM_IGE   ! dim = (NSEG)
        INTEGER   , DIMENSION(:,:), ALLOCATABLE :: NODES_IGE  ! dim =(NSEG,4) -SURF
!                                                             ! dim =(NSEG,2) -LINE
!!        TYPE (SEG_)   , DIMENSION(:) , ALLOCATABLE :: SEG     ! - segments of the surface
!!        TYPE (SEG_)   , DIMENSION(:) , ALLOCATABLE :: SEG_IGE ! - ISOGEOMETRIQUES segments of the surface
!--------------
      END TYPE SURF_
!--------------
!!      TYPE SEG_
!!        INTEGER   :: ELTYP ! - type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
!!        INTEGER   :: ELEM ! - element attached to the segment of the surface
!!        INTEGER, DIMENSION(:),ALLOCATABLE :: NODES   ! - 4 nodes of the segment for /SURF
!                                                    ! - 2 nodes of the segment for /LINE
!
!!        INTEGER   :: PROC  ! field to store the processor ID  (/LINE only)
!
!   1. Set processor only when no element is set in lines
!   2. Split lines accordingly
!
!!      END TYPE SEG_
c---------------
      END MODULE GROUPDEF_MOD
