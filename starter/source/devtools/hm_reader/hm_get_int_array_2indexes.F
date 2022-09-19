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
Chd|  HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|-- called by -----------
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|        CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_LIST_G            source/model/sets/create_node_clause.F
Chd|        CREATE_PART_LIST              source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST_G            source/model/sets/create_part_clause.F
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
Chd|        HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        READ_CLAUSE_BOX               source/model/sets/read_clause_box.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_INT_ARRAY_2INDEXES(NAME,IVAL,INDEX1,INDEX2,IS_AVAILABLE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE IN A LIST OF VALUES
C   ASK (INDEX1_TH,INDEX2)_TH (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY SUBMODEL OFFSETS USING DATATYPE DEFINED IN .cfg FILE
C   (TEMPORARY FOR SET NEED TO OFFSET MANUALY : SEG & GENE IDs) 
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "submod_c.inc"
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------
      VALUE_TYPE = 0
      SUB_ID = 0
C--------------------------------------------------
      CALL CPP_GET_INTV_2INDEX(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),IVAL,INDEX1,SUB_ID,
     .                        IS_AVAILABLE,VALUE_TYPE,INDEX2)
C--------------------------------------------------
C      ID OFFSETS FOR //SUBMODEL 
C--------------------------------------------------
      IF(SUB_ID /= 0 .AND. IVAL /= 0)THEN
        IF(VALUE_TYPE == 1)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_EL
        ELSEIF(VALUE_TYPE == 2)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_NOD
        ELSEIF(VALUE_TYPE == 3)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_PART
        ELSEIF(VALUE_TYPE == 4)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_MAT
        ELSEIF(VALUE_TYPE == 5)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_PROP
        ELSEIF(VALUE_TYPE == 6)THEN
c          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_SUBMOD
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_DEF
        ELSEIF(VALUE_TYPE == 7)THEN
          IVAL = IVAL + LSUBMODEL(SUB_ID)%OFF_DEF
        ENDIF
      ENDIF
C--------------------------------------------------
      RETURN
C
      END
