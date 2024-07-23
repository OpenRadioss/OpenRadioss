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
      !||    hm_read_defshell     ../starter/source/general_controls/default_values/hm_read_defshell.F
      !||--- called by ------------------------------------------------------
      !||    contrl               ../starter/source/starter/contrl.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_intv          ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_count      ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key   ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start      ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    defaults_mod         ../starter/source/modules/defaults_mod.F90
      !||    hm_option_read_mod   ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod          ../starter/share/message_module/message_mod.F
      !||    submodel_mod         ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_DEFSHELL(LSUBMODEL,DEFAULTS_SHELL)
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
      USE DEFAULTS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),   INTENT(IN)    :: LSUBMODEL(*)
      TYPE(SHELL_DEFAULTS_), INTENT(INOUT) :: DEFAULTS_SHELL
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_DEFSHELL,SUB_ID,NPTS_DD,IOFFSET_D
      LOGICAL IS_AVAILABLE
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
      CALL HM_OPTION_COUNT('/DEF_SHELL',NB_DEFSHELL)
      CALL HM_OPTION_START('/DEF_SHELL')
      DO I=1,NB_DEFSHELL
        CALL HM_OPTION_READ_KEY(LSUBMODEL, SUBMODEL_ID = SUB_ID)
        IF(SUB_ID == 0) THEN
          CALL HM_GET_INTV('ISHELL',DEFAULTS_SHELL%ishell,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Ismstr',DEFAULTS_SHELL%ismstr,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Ithick',DEFAULTS_SHELL%ithick,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Iplas',DEFAULTS_SHELL%iplas,IS_AVAILABLE,LSUBMODEL)
!          CALL HM_GET_INTV('Istrain',ISTR_D,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('ISH3N',DEFAULTS_SHELL%ish3n,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Idrill',DEFAULTS_SHELL%idrill,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Ioffset',DEFAULTS_SHELL%ioffset,IS_AVAILABLE,LSUBMODEL)
          EXIT
        ENDIF            
      ENDDO
C          
      IF (DEFAULTS_SHELL%IOFFSET>1) DEFAULTS_SHELL%IOFFSET = 0
C--------------------------------------------------------------
      RETURN
      END 
