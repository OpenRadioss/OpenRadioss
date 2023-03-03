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
Chd|  HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV_WITHOUT_UID     source/devtools/hm_reader/hm_get_floatv_without_uid.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_LAGMUL(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /LAGMUL USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_LAGMUL,SUB_ID,TAG_READ
      LOGICAL IS_AVAILABLE
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
C
      TAG_READ=0
      LAGMOD = 1
      LAGOPT = 1
      LAGM_TOL = EM11
      LAG_ALPH = FIVEEM4
      LAG_ALPHS= ZERO
C
      CALL HM_OPTION_COUNT('/LAGMUL',NB_LAGMUL)
      CALL HM_OPTION_START('/LAGMUL')
      DO I=1,NB_LAGMUL
        CALL HM_OPTION_READ_KEY(LSUBMODEL, SUBMODEL_ID = SUB_ID)
        IF(SUB_ID == 0) THEN
          CALL HM_GET_INTV('LAGMUL_Lagmod',LAGMOD,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('LAGMUL_Lagopt',LAGOPT,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOATV_WITHOUT_UID('LAGMUL_Tol',LAGM_TOL,IS_AVAILABLE)
          CALL HM_GET_FLOATV_WITHOUT_UID('LAGMUL_Alpha',LAG_ALPH,IS_AVAILABLE)
          CALL HM_GET_FLOATV_WITHOUT_UID('LAGMUL_Alpha_s',LAG_ALPHS,IS_AVAILABLE)
          TAG_READ=1
          EXIT
        ENDIF          
      ENDDO
C
      IF(TAG_READ == 1) THEN
        IF(LAGMOD == 0) LAGMOD = 1
        IF(LAGM_TOL == 0) LAGM_TOL = EM11
        IF(LAG_ALPH == 0) LAG_ALPH = EM3
        IF(LAG_ALPHS == 0) LAG_ALPHS = ZERO 
      ENDIF 
C
C--------------------------------------------------------------
      RETURN
      END 


