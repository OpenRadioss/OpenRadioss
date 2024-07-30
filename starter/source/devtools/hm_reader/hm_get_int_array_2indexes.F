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
      !||    hm_get_int_array_2indexes   ../starter/source/devtools/hm_reader/hm_get_int_array_2indexes.F
      !||--- called by ------------------------------------------------------
      !||    create_elt_list             ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list_g           ../starter/source/model/sets/create_element_clause.F
      !||    create_node_list            ../starter/source/model/sets/create_node_clause.F
      !||    create_node_list_g          ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause        ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_part_list            ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list_g          ../starter/source/model/sets/create_part_clause.F
      !||    create_rbody_list           ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list_g         ../starter/source/model/sets/create_rbody_clause.F
      !||    create_seg_clause           ../starter/source/model/sets/create_seg_clause.F
      !||    create_set_list             ../starter/source/model/sets/create_set_clause.F
      !||    create_set_list_g           ../starter/source/model/sets/create_set_clause.F
      !||    create_setcol_list          ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list_g        ../starter/source/model/sets/create_setcol_clause.F
      !||    create_subm_list            ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list_g          ../starter/source/model/sets/create_subm_clause.F
      !||    create_subs_list            ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list_g          ../starter/source/model/sets/create_subs_clause.F
      !||    hm_preread_properties       ../starter/source/properties/hm_preread_properties.F
      !||    hm_read_drape               ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_prop17              ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop51              ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_stack               ../starter/source/stack/hm_read_stack.F
      !||    read_clause_box             ../starter/source/model/sets/read_clause_box.F
      !||    st_qaprint_set              ../starter/source/output/qaprint/st_qaprint_set.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                 ../starter/share/message_module/message_mod.F
      !||    submodel_mod                ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_GET_INT_ARRAY_2INDEXES(NAME,IVAL,INDEX1,INDEX2,IS_AVAILABLE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE IN A LIST OF VALUES
C   ASK (INDEX1_TH,INDEX2)_TH (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY SUBMODEL OFFSETS USING DATATYPE DEFINED IN .cfg FILE
C   (TEMPORARY FOR SET NEED TO OFFSET MANUALLY : SEG & GENE IDs) 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     IVAL         INTEGER VALUE OF THE FIELD 
C     INDEX1       INDEX NUMBER OF THE VALUE (ROW)
C     INDEX2       INDEX NUMBER OF THE VALUE (COLUMN)
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
      INTEGER,INTENT(IN)::INDEX1
      INTEGER,INTENT(IN)::INDEX2
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IVAL
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
C-----------------------------------------------
      VALUE_TYPE = 0
      SUB_ID = 0
C--------------------------------------------------
      CALL CPP_GET_INTV_2INDEX(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),IVAL,INDEX1,SUB_ID,
     .                        IS_AVAILABLE,VALUE_TYPE,INDEX2)
C--------------------------------------------------
      RETURN
C
      END
