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
      !||    hm_get_int_array_index          ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||--- called by ------------------------------------------------------
      !||    create_elt_box                  ../starter/source/model/sets/create_elt_box.F
      !||    create_elt_list                 ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list_g               ../starter/source/model/sets/create_element_clause.F
      !||    create_node_box                 ../starter/source/model/sets/create_node_box.F
      !||    create_node_list                ../starter/source/model/sets/create_node_clause.F
      !||    create_node_list_g              ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause            ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_part_list                ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list_g              ../starter/source/model/sets/create_part_clause.F
      !||    create_rbody_box                ../starter/source/model/sets/create_rbody_box.F
      !||    create_rbody_list               ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list_g             ../starter/source/model/sets/create_rbody_clause.F
      !||    create_seg_clause               ../starter/source/model/sets/create_seg_clause.F
      !||    create_set_list                 ../starter/source/model/sets/create_set_clause.F
      !||    create_set_list_g               ../starter/source/model/sets/create_set_clause.F
      !||    create_setcol_list              ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list_g            ../starter/source/model/sets/create_setcol_clause.F
      !||    create_subm_list                ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list_g              ../starter/source/model/sets/create_subm_clause.F
      !||    create_subs_list                ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list_g              ../starter/source/model/sets/create_subs_clause.F
      !||    groups_get_elem_list            ../starter/source/groups/groups_get_elem_list.F
      !||    hm_admlist                      ../starter/source/groups/hm_admlist.F
      !||    hm_bigsbox                      ../starter/source/groups/hm_bigsbox.F
      !||    hm_elngr                        ../starter/source/groups/hm_elngr.F
      !||    hm_elngrr                       ../starter/source/groups/hm_elngrr.F
      !||    hm_elngrs                       ../starter/source/groups/hm_elngr.F
      !||    hm_lecgre                       ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                       ../starter/source/groups/hm_lecgrn.F
      !||    hm_linengr                      ../starter/source/groups/hm_linengr.F
      !||    hm_lines_of_lines               ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_prelecgrns                   ../starter/source/groups/hm_prelecgrns.F
      !||    hm_preread_load_pressure        ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
      !||    hm_preread_merge                ../starter/source/constraints/general/merge/hm_preread_merge.F
      !||    hm_preread_properties           ../starter/source/properties/hm_preread_properties.F
      !||    hm_preread_rbe3                 ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_read_admas                   ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_bem                     ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_drape                   ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_ebcs_inlet              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_eref                    ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_fail_inievo             ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
      !||    hm_read_fail_tab_old            ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_friction                ../starter/source/interfaces/friction/reader/hm_read_friction.F
      !||    hm_read_friction_orientations   ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_grpart                  ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_inicrack                ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inimap1d                ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d                ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inistate_d00            ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_initemp                 ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                  ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inject1                 ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2                 ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_intsub                  ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_lines                   ../starter/source/groups/hm_read_lines.F
      !||    hm_read_load_pressure           ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mat100                  ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat151                  ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_mat36                   ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat38                   ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat43                   ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat51                   ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat51_iform11           ../starter/source/materials/mat/mat051/hm_read_mat51_iform11.F
      !||    hm_read_mat52                   ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat57                   ../starter/source/materials/mat/mat057/hm_read_mat57.F
      !||    hm_read_mat59                   ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat65                   ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66                   ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat70                   ../starter/source/materials/mat/mat070/hm_read_mat70.F
      !||    hm_read_mat77                   ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat87                   ../starter/source/materials/mat/mat087/hm_read_mat87.F
      !||    hm_read_mat88                   ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90                   ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat93                   ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_merge                   ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_monvol_type11           ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type3            ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4            ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5            ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6            ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7            ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8            ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9            ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_mpc                     ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_nbcs                    ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_prop11                  ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop16                  ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17                  ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop22                  ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_prop26                  ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop28                  ../starter/source/properties/xelem/hm_read_prop28.F
      !||    hm_read_prop51                  ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_rbe3                    ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_stack                   ../starter/source/stack/hm_read_stack.F
      !||    hm_read_subset                  ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                    ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_table2_1                ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_thgrki                  ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody            ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrne                  ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns                  ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrpa                  ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrpa_sub              ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrsens                ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf                ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_xref                    ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_set                          ../starter/source/model/sets/hm_set.F
      !||    hm_surfnod                      ../starter/source/groups/hm_surfnod.F
      !||    hm_tagpart2                     ../starter/source/groups/hm_tagpart2.F
      !||    hm_thgrki_vent                  ../starter/source/output/th/hm_thgrki_vent.F
      !||    hm_thvarvent                    ../starter/source/output/th/hm_thvarent.F
      !||    hm_yctrl                        ../starter/source/elements/initia/hm_yctrl.F
      !||    lecextlnk                       ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecfill                         ../starter/source/elements/initia/lecfill.F
      !||    leclas1                         ../starter/source/loads/laser/leclas1.F
      !||    lecsec42                        ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                     ../starter/source/tools/sect/lecsec4bolt.F
      !||    nbadmesh                        ../starter/source/model/remesh/nbadmesh.F
      !||    r2r_monvol                      ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                      ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    read_box_box                    ../starter/source/model/box/read_box_box.F
      !||    read_impdisp_fgeo               ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel_fgeo                ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_sensor_gauge               ../starter/source/tools/sensor/read_sensor_gauge.F
      !||    sboxboxsurf                     ../starter/source/model/box/bigbox.F
      !||    set_admesh                      ../starter/source/model/remesh/set_admesh.F
      !||    sort_set                        ../starter/source/model/sets/sort_sets.F
      !||    st_qaprint_set                  ../starter/source/output/qaprint/st_qaprint_set.F
      !||    state_admesh                    ../starter/source/model/remesh/state_admesh.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                     ../starter/share/message_module/message_mod.F
      !||    submodel_mod                    ../starter/share/modules1/submodel_mod.F
      !||====================================================================
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
