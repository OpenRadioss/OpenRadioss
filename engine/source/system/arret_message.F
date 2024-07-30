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
      !||    arret_message       ../engine/source/system/arret_message.F
      !||--- called by ------------------------------------------------------
      !||    upgrade_lcand_e2s   ../common_source/interf/upgrade_multimp.F
      !||    upgrade_lcand_edg   ../common_source/interf/upgrade_multimp.F
      !||    upgrade_multimp     ../common_source/interf/upgrade_multimp.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg              ../engine/source/output/message/message.F
      !||    arret               ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod         ../engine/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE ARRET_MESSAGE(ITY,INTID,IPENMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITY,INTID
      my_real
     *  IPENMAX
      character MESS*512
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      WRITE(MESS,'(A,I10,A)') 'INTERFACE ',INTID,' SORTING'

        CALL ANCMSG(MSGID=19,ANMODE=ANINFO, 
     .              C1=MESS)    
      CALL ARRET(2)

      END
      !||====================================================================
      !||    arret_message_slid   ../engine/source/system/arret_message.F
      !||--- called by ------------------------------------------------------
      !||    upgrade_cand_opt     ../common_source/interf/upgrade_multimp.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg               ../engine/source/output/message/message.F
      !||    arret                ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod          ../engine/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE ARRET_MESSAGE_SLID(ITY,INTID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITY,INTID
      character MESS*512
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      WRITE(MESS,'(A,I10,A)') 'INTERFACE ',INTID,' SLIDING'

        CALL ANCMSG(MSGID=19,ANMODE=ANINFO, 
     .              C1=MESS)    
      CALL ARRET(2)

      END

C======================================================================|
