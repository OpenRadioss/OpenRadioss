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
      !||    wciusc2                ../engine/source/input/wciusc2.F
      !||--- called by ------------------------------------------------------
      !||    frebcs                 ../engine/source/input/frebcs.F
      !||    read10                 ../engine/source/input/read10.F
      !||    read10p                ../engine/source/input/read10p.F
      !||    read5p                 ../engine/source/input/read5p.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||    nvar                   ../engine/source/input/nvar.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE WCIUSC2(IREC,NBC,N,KEY0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREC, NBC, N
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real REC, ERR
      CHARACTER(LEN=NCHARLINE100)::CARTE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
      REWIND(IUSC2)
      N=0
      DO I=1,NBC
        READ(IUSC1,'(A)',REC=IREC,ERR=999)CARTE
        N=N+NVAR(CARTE)
        WRITE(IUSC2,'(A)')CARTE
      ENDDO
      REWIND(IUSC2)
      RETURN
 999  CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0)
      CALL ARRET(0)
      END
