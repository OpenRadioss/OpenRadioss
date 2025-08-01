Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    hm_get_current_option   ../starter/source/devtools/hm_reader/hm_get_current_option.F
!||--- called by ------------------------------------------------------
!||    ngr2usr                 ../starter/source/system/nintrr.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod             ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE HM_GET_CURRENT_OPTION(OPTION_ID,UNIT_ID,OPTION_TITR,
     .                              KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   GENERIC ROUTINE USING OPTIONAL FIELDS
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   IN CARD :  
C   /KEYWORD1/KEYWORD2/KEYWORD3/KEYWORD4/OPTION_ID/UNIT_ID
C   OPTION_TITR
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C               
C     OPTION_ID       OPTIONAL : ID OF THE OPTION         
C     UNIT_ID         OPTIONAL : UNIT ID OF THE OPTION  
C     OPTION_TITR     OPTIONAL : TITLE OF THE OPTION      
C     KEYWORD1        OPTIONAL : KEYWORD1 OF THE OPTION   
C     KEYWORD2        OPTIONAL : KEYWORD2 OF THE OPTION   
C     KEYWORD3        OPTIONAL : KEYWORD3 OF THE OPTION   
C     KEYWORD4        OPTIONAL : KEYWORD4 OF THE OPTION   
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE, NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: OPTION_ID,UNIT_ID
      CHARACTER(len=ncharkey),INTENT(OUT) :: KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4
      CHARACTER(len=nchartitle),INTENT(OUT) :: OPTION_TITR
      OPTIONAL :: OPTION_ID,UNIT_ID,KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4,OPTION_TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: CPT, CPT1,CPT2,IOK,S_KEYWORD,S_TITR_TMP,TMP_OPTION_ID,TMP_UNIT_ID,TMP_SUBMODEL_INDEX,MY_POSITION
      CHARACTER(LEN=NCHARLINE) :: KEYWORD,TITR_TMP
C-----------------------------------------------
      KEYWORD=''
      TMP_OPTION_ID = 0
      TMP_UNIT_ID = 0
      TMP_SUBMODEL_INDEX = 0
      IF(PRESENT(OPTION_TITR)) OPTION_TITR=''
      IF(PRESENT(KEYWORD1)) KEYWORD1=''
      IF(PRESENT(KEYWORD2)) KEYWORD2=''
      IF(PRESENT(KEYWORD3)) KEYWORD3=''
      IF(PRESENT(KEYWORD4)) KEYWORD4=''
      MY_POSITION = 0
C--------------------------------------------------
      CALL CPP_CURRENT_OPTION(TMP_OPTION_ID,TMP_UNIT_ID,TMP_SUBMODEL_INDEX,KEYWORD,S_KEYWORD,
     .                      TITR_TMP,S_TITR_TMP) 
C--------------------------------------------------
      IF(PRESENT(OPTION_ID)) OPTION_ID = TMP_OPTION_ID
      IF(PRESENT(UNIT_ID)) UNIT_ID = TMP_UNIT_ID
      IF(PRESENT(OPTION_TITR)) OPTION_TITR(1:S_TITR_TMP) = TITR_TMP(1:S_TITR_TMP)
C--------------------------------------------------
      CPT  = 2
      CPT1 = 2
      CPT2 = 2 
      IOK = 0

      DO WHILE (CPT < ncharline .AND. IOK == 0)
        IF(KEYWORD(CPT:CPT) == '/' .OR. KEYWORD(CPT:CPT) == ' ') THEN
          CPT2 = CPT
          IOK = 1
        ENDIF
        CPT = CPT + 1
      ENDDO
      IF(PRESENT(KEYWORD1)) KEYWORD1(1:CPT2-CPT1) = KEYWORD(CPT1:MIN((CPT2-1),S_KEYWORD))
      CPT1 = CPT2+1
      IOK = 0
      DO WHILE (CPT < ncharline .AND. IOK == 0)
        IF(KEYWORD(CPT:CPT) == '/' .OR. KEYWORD(CPT:CPT) == ' ') THEN
          CPT2 = CPT
          IOK = 1
        ENDIF
        CPT = CPT + 1
      ENDDO
      IF(PRESENT(KEYWORD2)) KEYWORD2(1:CPT2-CPT1) = KEYWORD(CPT1:MIN((CPT2-1),S_KEYWORD))
      CPT1 = CPT2+1
      IOK = 0
      DO WHILE (CPT < ncharline .AND. IOK == 0)
        IF(KEYWORD(CPT:CPT) == '/' .OR. KEYWORD(CPT:CPT) == ' ') THEN
          CPT2 = CPT
          IOK = 1
        ENDIF
        CPT = CPT + 1
      ENDDO
      IF(PRESENT(KEYWORD3)) KEYWORD3(1:CPT2-CPT1) = KEYWORD(CPT1:MIN((CPT2-1),S_KEYWORD))
      CPT1 = CPT2+1
      IOK = 0
      DO WHILE (CPT < ncharline .AND. IOK == 0)
        IF(KEYWORD(CPT:CPT) == '/' .OR. KEYWORD(CPT:CPT) == ' ') THEN
          CPT2 = CPT
          IOK = 1
        ENDIF
        CPT = CPT + 1
      ENDDO
      IF(PRESENT(KEYWORD4)) KEYWORD4(1:CPT2-CPT1) = KEYWORD(CPT1:MIN((CPT2-1),S_KEYWORD))
C--------------------------------------------------
      RETURN
C
      END

