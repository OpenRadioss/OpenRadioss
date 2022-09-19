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
Chd|  HM_GET_MAX_ID                 source/devtools/hm_reader/hm_get_max_id.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_MAX_ID(NAME,IDMAX,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST MAX ID OF A GIVEN KEY MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   APPLY AUTOMATICALLY SUBMODEL OFFSETS USING DATATYPE DEFINED IN .cfg FILE
C   IF NO OPTION IN THE MODEL MAX_ID MAX SUBMODEL OFFSET IN THE MODEL
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     IDMAX        MAX ID 
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
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IDMAX
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID,NBOPTION,OPTION_ID,UNIT_ID,SUBMODEL_INDEX,
     .        S_KEYWORD,S_TITR,OFFSET_CATEGORY,MY_POSITION,
     .        OFF_EL,OFF_NOD,OFF_PART,OFF_MAT,OFF_PROP,OFF_DEF
      CHARACTER*ncharline KEYWORD,TITR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------

      OFFSET_CATEGORY = 0
      VALUE_TYPE = 0
      SUB_ID = 0
      KEYWORD=''
      OPTION_ID = 0
      UNIT_ID = 0
      SUBMODEL_INDEX = 0
      MY_POSITION = 0
      IDMAX = 0
C--------------------------------------------------
C      START OPTION READING 
C--------------------------------------------------
      CALL HM_OPTION_COUNT(NAME(1:LEN_TRIM(NAME)), NBOPTION)
      CALL HM_OPTION_START(NAME(1:LEN_TRIM(NAME)))
C--------------------------------------------------
C      READ EACH OCCURENCE OF OPTION 
C--------------------------------------------------
      IF(NBOPTION > 0)THEN
        DO I=1,NBOPTION
          CALL CPP_OPTION_READ(OPTION_ID,UNIT_ID,SUBMODEL_INDEX,KEYWORD,S_KEYWORD,
     .                      TITR,S_TITR,OFFSET_CATEGORY,MY_POSITION)
C--------------------------------------------------
C      ID OFFSETS FOR //SUBMODEL 
C--------------------------------------------------
          IF(SUBMODEL_INDEX /= 0 .AND. OPTION_ID /= 0)THEN
            IF(OFFSET_CATEGORY == 1)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_EL
            ELSEIF(OFFSET_CATEGORY == 2)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_NOD
            ELSEIF(OFFSET_CATEGORY == 3)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_PART
            ELSEIF(OFFSET_CATEGORY == 4)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_MAT
            ELSEIF(OFFSET_CATEGORY == 5)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_PROP
            ELSEIF(OFFSET_CATEGORY == 6)THEN
c a prendre en compte qd dispo
c            OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_SUBMOD
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_DEF
            ELSEIF(OFFSET_CATEGORY == 7)THEN
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_DEF
            ELSE
              OPTION_ID = OPTION_ID + LSUBMODEL(SUBMODEL_INDEX)%OFF_DEF
            ENDIF
          ENDIF
          IF (OPTION_ID > IDMAX) IDMAX = OPTION_ID
        ENDDO
      ELSE
        DO I=1,NSUBMOD
          OFF_EL   = LSUBMODEL(I)%OFF_EL
          IF (OFF_EL > IDMAX)   IDMAX = OFF_EL
          OFF_NOD  = LSUBMODEL(I)%OFF_NOD
          IF (OFF_NOD > IDMAX)  IDMAX = OFF_NOD
          OFF_PART = LSUBMODEL(I)%OFF_PART
          IF (OFF_PART > IDMAX) IDMAX = OFF_PART
          OFF_MAT  = LSUBMODEL(I)%OFF_MAT
          IF (OFF_MAT > IDMAX)  IDMAX = OFF_MAT
          OFF_PROP = LSUBMODEL(I)%OFF_PROP
          IF (OFF_PROP > IDMAX) IDMAX = OFF_PROP
          OFF_DEF  = LSUBMODEL(I)%OFF_DEF
          IF (OFF_DEF > IDMAX)  IDMAX = OFF_DEF
        ENDDO
      ENDIF
C--------------------------------------------------
      RETURN
C
      END
