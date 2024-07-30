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
      !||    hm_read_upwind       ../starter/source/general_controls/computation/hm_read_upwind.F
      !||--- called by ------------------------------------------------------
      !||    contrl               ../starter/source/starter/contrl.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg               ../starter/source/output/message/message.F
      !||    hm_get_floatv        ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_option_count      ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_next       ../starter/source/devtools/hm_reader/hm_option_next.F
      !||    hm_option_start      ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod   ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod          ../starter/share/message_module/message_mod.F
      !||    submodel_mod         ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_UPWIND(JUPW, ETA1, ETA2, ETA3, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: JUPW
      my_real, INTENT(INOUT) :: ETA1, ETA2, ETA3
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NUPWIND
      LOGICAL :: IS_AVAILABLE

      JUPW = 0
      ETA1 = ONE
      ETA2 = ONE
      ETA3 = ONE
      
      CALL HM_OPTION_COUNT('/UPWIND', NUPWIND)
      
      IF (NUPWIND > 0) THEN
         CALL HM_OPTION_START('/UPWIND')
         CALL HM_OPTION_NEXT()
         JUPW = 1
         CALL HM_GET_FLOATV('ENG_UPWIND_eta1', ETA1, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('ENG_UPWIND_eta2', ETA2, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('ENG_UPWIND_eta3', ETA3, IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF (ETA1 == ZERO) ETA1 = ONE
         IF (ETA2 == ZERO) ETA2 = ONE
         IF (ETA3 == ZERO) ETA3 = ONE
         
         IF (ETA3 /= ONE) THEN
            CALL ANCMSG(MSGID = 1628, MSGTYPE = MSGWARNING, ANMODE = ANINFO)
         ENDIF
      ENDIF
      END SUBROUTINE HM_READ_UPWIND
