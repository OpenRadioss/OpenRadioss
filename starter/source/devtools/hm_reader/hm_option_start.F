Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|-- called by -----------
Chd|        CONTRBE2                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|        HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|        HM_CONVERT_FAIL_TAB           source/devtools/hm_reader/hm_convert_fail_tab.F
Chd|        HM_CONVERT_INTER_TYPE19       source/devtools/hm_reader/hm_convert_inter_type19.F
Chd|        HM_COUNT_2D_ELEMENT_SEATBELT  source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
Chd|        HM_DEBUG_PRINT_OPTION         source/devtools/hm_reader/hm_debug_print_option.F
Chd|        HM_GET_MAX_ID                 source/devtools/hm_reader/hm_get_max_id.F
Chd|        HM_GROGRONOD                  source/groups/hm_grogronod.F  
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|        HM_PRELECGRNS                 source/groups/hm_prelecgrns.F 
Chd|        HM_PRELECJOI                  source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_CLOAD              source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_CONVEC             source/loads/thermic/hm_preread_convec.F
Chd|        HM_PREREAD_EIG                source/general_controls/computation/hm_read_eig.F
Chd|        HM_PREREAD_GRAV               source/loads/general/grav/hm_preread_grav.F
Chd|        HM_PREREAD_IMPACC             source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPFLUX            source/constraints/thermic/hm_preread_impflux.F
Chd|        HM_PREREAD_IMPTEMP            source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_IMPVEL0            source/constraints/general/impvel/hm_preread_impvel0.F
Chd|        HM_PREREAD_INIVEL             source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_PREREAD_LOAD_CENTRI        source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|        HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|        HM_PREREAD_PFLUID             source/loads/general/pfluid/hm_preread_pfluid.F
Chd|        HM_PREREAD_PLOAD              source/loads/general/pload/hm_preread_pload.F
Chd|        HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|        HM_PREREAD_RADIATION          source/loads/thermic/hm_preread_radiation.F
Chd|        HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_RBE3               source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PREREAD_RBODY              source/constraints/general/rbody/hm_preread_rbody.F
Chd|        HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|        HM_PRE_READ_LINK              source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        HM_PRE_READ_PRELOAD           source/loads/general/preload/hm_pre_read_preload.F
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALEBCS                source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_ANALY                 source/general_controls/computation/hm_read_analy.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_CAA                   source/general_controls/computation/hm_read_caa.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|        HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|        HM_READ_DEFSOLID              source/general_controls/default_values/hm_read_defsolid.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNC2D                source/tools/curve/hm_read_func2d.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPLICIT              source/general_controls/computation/hm_read_implicit.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INISTA                source/initial_conditions/inista/hm_read_inista.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_IOFLAG                source/general_controls/inputoutput/hm_read_ioflag.F
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_MPC0                  source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELECDRAPE           source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PRETHGROU             source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|        HM_READ_SUBMODEL              source/model/assembling/hm_read_submodel.F
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECGGROUP                     source/groups/lecggroup.F     
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECSEC0                       source/tools/sect/hm_read_sect.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        NBFUNCT                       source/tools/curve/nbfunc.F   
Chd|        NBSPH                         source/elements/sph/nbsph.F   
Chd|        PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|        PRELECLAS                     source/loads/laser/lpreleclas.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|        PREREAD_RBODY_SET             source/model/sets/preread_rbody_set.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        READ_ALE_MAT                  source/materials/ale/read_ale_mat.F
Chd|        READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_ENGINE_DRIVER            source/general_controls/engine/read_engine_driver.F
Chd|        READ_EULER_MAT                source/materials/ale/read_euler_mat.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::ENTITY_TYPE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      CALL CPP_OPTION_START(ENTITY_TYPE(1:LEN_TRIM(ENTITY_TYPE)),LEN_TRIM(ENTITY_TYPE)) 
      RETURN
C
      END



Chd|====================================================================
Chd|  HM_OPTION_START_LIST          source/devtools/hm_reader/hm_option_start.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::ENTITY_TYPE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      CALL CPP_OPTION_START_LIST(ENTITY_TYPE(1:LEN_TRIM(ENTITY_TYPE)),LEN_TRIM(ENTITY_TYPE)) 
      RETURN
C 
      END




