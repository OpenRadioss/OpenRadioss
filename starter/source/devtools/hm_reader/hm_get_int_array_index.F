Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|-- called by -----------
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|        CREATE_NODE_BOX               source/model/sets/create_node_box.F
Chd|        CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_LIST_G            source/model/sets/create_node_clause.F
Chd|        CREATE_PART_LIST              source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST_G            source/model/sets/create_part_clause.F
Chd|        CREATE_RBODY_BOX              source/model/sets/create_rbody_box.F
Chd|        CREATE_RBODY_LIST             source/model/sets/create_rbody_clause.F
Chd|        CREATE_RBODY_LIST_G           source/model/sets/create_rbody_clause.F
Chd|        CREATE_SEG_CLAUSE             source/model/sets/create_seg_clause.F
Chd|        CREATE_SETCOL_LIST            source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST_G          source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_LIST               source/model/sets/create_set_clause.F
Chd|        CREATE_SET_LIST_G             source/model/sets/create_set_clause.F
Chd|        CREATE_SUBM_LIST              source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST_G            source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBS_LIST              source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST_G            source/model/sets/create_subs_clause.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|        HM_ELNGR                      source/groups/hm_elngr.F      
Chd|        HM_ELNGRR                     source/groups/hm_elngrr.F     
Chd|        HM_ELNGRS                     source/groups/hm_elngr.F      
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_LINENGR                    source/groups/hm_linengr.F    
Chd|        HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|        HM_PRELECGRNS                 source/groups/hm_prelecgrns.F 
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|        HM_PREREAD_RBE3               source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FAIL_INIEVO           source/materials/fail/inievo/hm_read_fail_inievo.F
Chd|        HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT51_IFORM11         source/materials/mat/mat051/hm_read_mat51_iform11.F
Chd|        HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROP28                source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGRPA                source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRPA_SUB            source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_SURFNOD                    source/groups/hm_surfnod.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECLAS1                       source/loads/laser/leclas1.F  
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_SENSOR_GAUGE             source/tools/sensor/read_sensor_gauge.F
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_INT_ARRAY_INDEX(NAME,IVAL,INDEX,IS_AVAILABLE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE IN A LIST OF VALUES
C   ASK INDEX_TH (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY SUBMODEL OFFSETS USING DATATYPE DEFINED IN .cfg FILE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     IVAL         INTEGER VALUE OF THE FIELD 
C     INDEX        INDEX NUMBER OF THE VALUE 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C     LSUBMODEL    SUBMODEL STRUCTURE
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
      INTEGER,INTENT(IN)::INDEX
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IVAL
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
C--------------------------------------------------
      VALUE_TYPE = 0
      SUB_ID = 0
C--------------------------------------------------
      CALL CPP_GET_INTV_INDEX(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),IVAL,INDEX,SUB_ID,
     .                        IS_AVAILABLE,VALUE_TYPE)
C--------------------------------------------------
      RETURN
C
      END
