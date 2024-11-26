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
      !||    hm_option_start                   ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- called by ------------------------------------------------------
      !||    contrbe2                          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    contrbe3                          ../starter/source/constraints/general/rbe3/contrbe3.F
      !||    contrl                            ../starter/source/starter/contrl.F
      !||    create_map_tables                 ../starter/source/model/sets/map_tables.F
      !||    hm_convert_2d_elements_seatbelt   ../starter/source/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
      !||    hm_convert_fail_tab               ../starter/source/devtools/hm_reader/hm_convert_fail_tab.F
      !||    hm_convert_inter_type19           ../starter/source/devtools/hm_reader/hm_convert_inter_type19.F
      !||    hm_count_2d_element_seatbelt      ../starter/source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
      !||    hm_debug_print_option             ../starter/source/devtools/hm_reader/hm_debug_print_option.F
      !||    hm_get_max_id                     ../starter/source/devtools/hm_reader/hm_get_max_id.F
      !||    hm_grogronod                      ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                         ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                         ../starter/source/groups/hm_lecgrn.F
      !||    hm_lines_of_lines                 ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_pre_read_link                  ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_pre_read_preload               ../starter/source/loads/general/preload/hm_pre_read_preload.F
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
      !||    hm_preread_impvel0                ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
      !||    hm_preread_inivel                 ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
      !||    hm_preread_load_centri            ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
      !||    hm_preread_load_pressure          ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
      !||    hm_preread_merge                  ../starter/source/constraints/general/merge/hm_preread_merge.F
      !||    hm_preread_part                   ../starter/source/model/assembling/hm_read_part.F
      !||    hm_preread_pblast                 ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_preread_pfluid                 ../starter/source/loads/general/pfluid/hm_preread_pfluid.F
      !||    hm_preread_pload                  ../starter/source/loads/general/pload/hm_preread_pload.F
      !||    hm_preread_properties             ../starter/source/properties/hm_preread_properties.F
      !||    hm_preread_radiation              ../starter/source/loads/thermic/hm_preread_radiation.F
      !||    hm_preread_rbe2                   ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_rbe3                   ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_preread_rbody                  ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_preread_sphio                  ../starter/source/loads/sph/hm_preread_sphio.F
      !||    hm_read_activ                     ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_admas                     ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_ale_grid                  ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
      !||    hm_read_ale_link                  ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_ale_muscl                 ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_ale_solver                ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
      !||    hm_read_alebcs                    ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_analy                     ../starter/source/general_controls/computation/hm_read_analy.F
      !||    hm_read_bcs                       ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_bcs_wall                  ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_bem                       ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_caa                       ../starter/source/general_controls/computation/hm_read_caa.F
      !||    hm_read_cload                     ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cluster                   ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_convec                    ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_cyljoint                  ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_damp                      ../starter/source/general_controls/damping/hm_read_damp.F
      !||    hm_read_definter                  ../starter/source/general_controls/default_values/hm_read_definter.F
      !||    hm_read_defshell                  ../starter/source/general_controls/default_values/hm_read_defshell.F
      !||    hm_read_defsolid                  ../starter/source/general_controls/default_values/hm_read_defsolid.F
      !||    hm_read_drape                     ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_eig                       ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos                       ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eref                      ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_fail                      ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_friction_models           ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations     ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_frm                       ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_func2d                    ../starter/source/tools/curve/hm_read_func2d.F
      !||    hm_read_funct                     ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_funct_python              ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_fxb1                      ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_fxb2                      ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_gauge                     ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_gjoint                    ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_grav                      ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_grpart                    ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_impacc                    ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impflux                   ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_implicit                  ../starter/source/general_controls/computation/hm_read_implicit.F
      !||    hm_read_imptemp                   ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_inicrack                  ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inigrav                   ../starter/source/initial_conditions/inigrav/hm_read_inigrav.F
      !||    hm_read_inimap1d                  ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d                  ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inista                    ../starter/source/initial_conditions/inista/hm_read_inista.F
      !||    hm_read_inistate_d00              ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_initemp                   ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                    ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_interfaces                ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub                    ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_ioflag                    ../starter/source/general_controls/inputoutput/hm_read_ioflag.F
      !||    hm_read_lagmul                    ../starter/source/tools/lagmul/hm_read_lagmul.F
      !||    hm_read_leak                      ../starter/source/airbag/hm_read_leak.F
      !||    hm_read_lines                     ../starter/source/groups/hm_read_lines.F
      !||    hm_read_link                      ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_load_centri               ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure             ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_merge                     ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_merge_node                ../starter/source/elements/reader/hm_read_merge_node.F
      !||    hm_read_move_funct                ../starter/source/tools/curve/hm_read_move_funct.F
      !||    hm_read_mpc                       ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_mpc0                      ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_nbcs                      ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_node                      ../starter/source/elements/reader/hm_read_node.F
      !||    hm_read_nonlocal                  ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_part                      ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_pblast                    ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pcyl                      ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_perturb_fail              ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_perturb_part_shell        ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_perturb_part_solid        ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
      !||    hm_read_pfluid                    ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                     ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_prelecdrape               ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_preload                   ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_preload_axial             ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_prethgrou                 ../starter/source/output/th/hm_read_prethgrou.F
      !||    hm_read_properties                ../starter/source/properties/hm_read_properties.F
      !||    hm_read_radiation                 ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_rand                      ../starter/source/general_controls/computation/hm_read_rand.F
      !||    hm_read_rbe2                      ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                      ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                     ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul              ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_refsta                    ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_retractor                 ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rwall_cyl                 ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul              ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral               ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane               ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher               ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm               ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_sensors                   ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_skw                       ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring                  ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_sms                       ../starter/source/general_controls/computation/hm_read_sms.F
      !||    hm_read_spcnd                     ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_sphcel                    ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphglo                    ../starter/source/general_controls/computation/hm_read_sphglo.F
      !||    hm_read_sphio                     ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_spmd                      ../starter/source/general_controls/computation/hm_read_spmd.F
      !||    hm_read_submodel                  ../starter/source/model/assembling/hm_read_submodel.F
      !||    hm_read_subset                    ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                      ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                  ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_table1_0                  ../starter/source/tools/curve/hm_read_table1_0.F
      !||    hm_read_table1_1                  ../starter/source/tools/curve/hm_read_table1_1.F
      !||    hm_read_table2_0                  ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1                  ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_therm                     ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress              ../starter/source/materials/therm/hm_read_therm_stress.F
      !||    hm_read_thgrou                    ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thpart                    ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_unit                      ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_read_upwind                    ../starter/source/general_controls/computation/hm_read_upwind.F
      !||    hm_read_visc                      ../starter/source/materials/visc/hm_read_visc.F
      !||    hm_read_window_user               ../starter/source/tools/userwi/hm_read_window_user.F
      !||    hm_read_xref                      ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_setfxrbyon                     ../starter/source/constraints/fxbody/hm_setfxrbyon.F
      !||    hm_yctrl                          ../starter/source/elements/initia/hm_yctrl.F
      !||    lecacc                            ../starter/source/tools/accele/lecacc.F
      !||    lecextlnk                         ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecfill                           ../starter/source/elements/initia/lecfill.F
      !||    lecggroup                         ../starter/source/groups/lecggroup.F
      !||    leclas                            ../starter/source/loads/laser/leclas.F
      !||    lecsec0                           ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec42                          ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                       ../starter/source/tools/sect/lecsec4bolt.F
      !||    lecstack_ply                      ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lecstamp                          ../starter/source/interfaces/interf1/lecstamp.F
      !||    lecsubmod                         ../starter/source/model/submodel/lecsubmod.F
      !||    lectrans                          ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                       ../starter/source/model/submodel/lectranssub.F
      !||    nbadmesh                          ../starter/source/model/remesh/nbadmesh.F
      !||    nbfunct                           ../starter/source/tools/curve/nbfunc.F
      !||    nbsph                             ../starter/source/elements/sph/nbsph.F
      !||    prelecdet                         ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    preleclas                         ../starter/source/loads/laser/lpreleclas.F
      !||    prelecsec                         ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt                    ../starter/source/tools/sect/prelecsec4bolt.F
      !||    preread_rbody_lagmul              ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||    preread_rbody_set                 ../starter/source/model/sets/preread_rbody_set.F
      !||    r2r_monvol                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec_name                   ../starter/source/coupling/rad2rad/r2r_prelec_name.F
      !||    read_ale_mat                      ../starter/source/materials/ale/read_ale_mat.F
      !||    read_box_box                      ../starter/source/model/box/read_box_box.F
      !||    read_box_cyl                      ../starter/source/model/box/read_box_cyl.F
      !||    read_box_rect                     ../starter/source/model/box/read_box_rect.F
      !||    read_box_spher                    ../starter/source/model/box/read_box_spher.F
      !||    read_detonators                   ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_dfs_detcord                  ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline                  ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan                  ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint                 ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper              ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_ebcs                         ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_engine_driver                ../starter/source/general_controls/engine/read_engine_driver.F
      !||    read_euler_mat                    ../starter/source/materials/ale/read_euler_mat.F
      !||    read_impdisp                      ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impdisp_fgeo                 ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                       ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo                  ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul                ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_monvol                       ../starter/source/airbag/read_monvol.F
      !||    set_admesh                        ../starter/source/model/remesh/set_admesh.F
      !||    setrb2on                          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    setrbyon                          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sort_set                          ../starter/source/model/sets/sort_sets.F
      !||    st_qaprint_set                    ../starter/source/output/qaprint/st_qaprint_set.F
      !||    state_admesh                      ../starter/source/model/remesh/state_admesh.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                       ../starter/share/message_module/message_mod.F
      !||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_OPTION_START(ENTITY_TYPE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   START READING /ENTITY_TYPE OPTIONS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   =================== 
C
C     NAME               DESCRIPTION                         
C
C     ENTITY_TYPE        KEYWORD YOU WANT TO START THE READING SEQUENCE
C          'MATERIAL'
C          'PROPERTY'
C          'PART'
C          'TRANSFORM'
C          'SUBSET'
C          'SENSOR'
C          'INTER'
C          'RWALL'
C          'FAILURE'
C          'DAMP'
C          'GAUGE'
C          'RBODY'
C          'ACCEL'
C          'BCS'
C          'AIRBAG'
C          'FRICTION'
C          'UNIT'
C          ..........
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::ENTITY_TYPE
C-----------------------------------------------
      CALL CPP_OPTION_START(ENTITY_TYPE(1:LEN_TRIM(ENTITY_TYPE)),LEN_TRIM(ENTITY_TYPE)) 
      RETURN
C
      END



      !||====================================================================
      !||    hm_option_start_list    ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- called by ------------------------------------------------------
      !||    hm_set                  ../starter/source/model/sets/hm_set.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||    submodel_mod            ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_OPTION_START_LIST(ENTITY_TYPE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   START READING / ENTITY_TYPE OPTIONS USING HM_READER
C   Creates a list of Entities, to be able to read in different order.
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   =================== 
C
C     NAME               DESCRIPTION                         
C
C     ENTITY_TYPE        KEYWORD YOU WANT TO START THE READING SEQUENCE
C          'MATERIAL'
C          'PROPERTY'
C          'PART'
C          'TRANSFORM'
C          'SUBSET'
C          'SENSOR'
C          'INTER'
C          'RWALL'
C          'FAILURE'
C          'DAMP'
C          'GAUGE'
C          'RBODY'
C          'ACCEL'
C          'BCS'
C          'AIRBAG'
C          'FRICTION'
C          'UNIT'
C          ..........
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::ENTITY_TYPE
C-----------------------------------------------
      CALL CPP_OPTION_START_LIST(ENTITY_TYPE(1:LEN_TRIM(ENTITY_TYPE)),LEN_TRIM(ENTITY_TYPE)) 
      RETURN
C 
      END




