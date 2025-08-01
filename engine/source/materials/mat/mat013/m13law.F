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
!||    m13law        ../engine/source/materials/mat/mat013/m13law.F
!||--- called by ------------------------------------------------------
!||    mmain         ../engine/source/materials/mat_share/mmain.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE M13LAW(RDUM1,RDUM2,RDUM3,RDUM4,RDUM5,
     .                  RDUM6,RDUM7,RDUM8,RDUM9)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT
      CHARACTER MESG*80, CHAR*5
      DOUBLE PRECISION
     .          RDUM1,RDUM2,RDUM3,RDUM4,RDUM5,
     .          RDUM6,RDUM7,RDUM8,RDUM9
C-----------------------------------------------
      CHAR = ''
      MAT  = 13
      MESG = 'MATERIAL LAW TYPE'//CHAR
      CALL ANCMSG(MSGID=180,ANMODE=ANINFO,C1=MESG)
      CALL ARRET(2)
      RETURN
C---------------------------
      END
