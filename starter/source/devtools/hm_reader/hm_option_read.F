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
Chd|  HM_OPTION_READ                source/devtools/hm_reader/hm_option_read.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_OPTION_READ(ID,UID,SUB_INDEX,TITR,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   IN CARD :  
C   /OPTION/ID/UID
C   TITR
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     ID           ID OF THE OPTION         
C     UID          UNIT ID OF THE OPTION    
C     SUB_INDEX    SUBMODEL INDEX OF THE OPTION
C     TITR         TITLE OF THE OPTION      
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
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::ID
      INTEGER,INTENT(OUT)::UID
      INTEGER,INTENT(OUT)::SUB_INDEX
      CHARACTER*(*),INTENT(OUT)::TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OFFSET_CATEGORY,S_KEYWORD,S_TITR_TMP,POS
      CHARACTER*ncharline KEYWORD,TITR_TMP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      ID = 0
      SUB_INDEX = 0
      POS=0
C--------------------------------------------------
      CALL CPP_OPTION_READ(ID,UID,SUB_INDEX,KEYWORD,S_KEYWORD,TITR_TMP,S_TITR_TMP,OFFSET_CATEGORY,POS)
C--------------------------------------------------
      TITR(1:S_TITR_TMP) = TITR_TMP(1:S_TITR_TMP)

      IF(SUB_INDEX /= 0 )THEN
        IF(LSUBMODEL(SUB_INDEX)%UID /= 0)THEN
          UID = LSUBMODEL(SUB_INDEX)%UID
        ENDIF
      ENDIF
C--------------------------------------------------
      RETURN
C
      END
