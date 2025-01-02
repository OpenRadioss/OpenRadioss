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
      !||    errmsg        ../engine/source/input/errmsg.F
      !||--- called by ------------------------------------------------------
      !||    frebcs        ../engine/source/input/frebcs.F
      !||    frecpl        ../engine/source/input/frecpl.F
      !||    fredli7       ../engine/source/input/fredli7.F
      !||    freform       ../engine/source/input/freform.F
      !||    freiniv       ../engine/source/input/freiniv.F
      !||    frelnk        ../engine/source/input/frelnk.F
      !||    ixyz          ../engine/source/input/ixyz.F
      !||    redkey0       ../engine/source/input/redkey0.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../engine/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../engine/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE ERRMSG(KEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER KEY*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESG*80
C-----------------------------------------------
        CALL ANCMSG(MSGID=189,ANMODE=ANINFO,
     .              C1=KEY)
        IERR=IERR+1
      RETURN
      END
