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
Chd|  HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFSHELL(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /DEF_SHELL USING HM_READER
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
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_DEFSHELL,SUB_ID,NPTS_DD
      LOGICAL IS_AVAILABLE
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
      NPTS_DD = 0
      CALL HM_OPTION_COUNT('/DEF_SHELL',NB_DEFSHELL)
      CALL HM_OPTION_START('/DEF_SHELL')
      DO I=1,NB_DEFSHELL
        CALL HM_OPTION_READ_KEY(LSUBMODEL, SUBMODEL_ID = SUB_ID)
        IF(SUB_ID == 0) THEN
          CALL HM_GET_INTV('ISHELL',IHBE_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Ismstr',ISST_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Ithick',ITHK_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Iplas',IPLA_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Istrain',ISTR_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('ISH3N',ISH3N_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Idrill',IDRIL_D,IS_AVAILABLE,LSUBMODEL)
          EXIT
        ENDIF            
      ENDDO
C          
C     IF(ISTR_D==0)ISTR_D = 1
      ISTR_D = 1
      IF(NPTS_D==-1)NPTS_D=NPTS_DD
C--------------------------------------------------------------
      RETURN
      END 
