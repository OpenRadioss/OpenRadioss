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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IDMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID,NBOPTION,OPTION_ID,UNIT_ID,SUBMODEL_INDEX,
     .        S_KEYWORD,S_TITR,OFFSET_CATEGORY,MY_POSITION,
     .        OFF_EL,OFF_NOD,OFF_PART,OFF_MAT,OFF_PROP,OFF_DEF
      CHARACTER*ncharline KEYWORD,TITR
C-----------------------------------------------
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
C      READ EACH OCCURRENCE OF OPTION 
C--------------------------------------------------
      IF(NBOPTION > 0)THEN
        DO I=1,NBOPTION
          CALL CPP_OPTION_READ(OPTION_ID,UNIT_ID,SUBMODEL_INDEX,KEYWORD,S_KEYWORD,
     .                      TITR,S_TITR,OFFSET_CATEGORY,MY_POSITION) 
          IF (OPTION_ID > IDMAX) IDMAX = OPTION_ID
        ENDDO
      ENDIF
C--------------------------------------------------
      RETURN
C
      END
