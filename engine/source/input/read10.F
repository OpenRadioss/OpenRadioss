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
!||    read10    ../engine/source/input/read10.F
!||--- called by ------------------------------------------------------
!||    fralnk    ../engine/source/input/fralnk.F
!||    fralnk2   ../engine/source/input/fralnk.F
!||    frelnk    ../engine/source/input/frelnk.F
!||    frenois   ../engine/source/input/frenois.F
!||--- calls      -----------------------------------------------------
!||    wciusc2   ../engine/source/input/wciusc2.F
!||====================================================================
      SUBROUTINE READ10(IREC,NBC,KEY0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREC, NBC
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IV1(40), IV2(10), K, JJ, I, N, J, NC1, NC2
C-----------------------------------------------
       K=0
       JJ=0
       DO I=1,NBC
        CALL WCIUSC2(IREC+K,1,N,KEY0)
        READ(IUSC2,*)(IV1(J),J=1,N)
        K=K+1
        NC1=1
        DO
         NC2=MIN(NC1+9-JJ,N)
         DO J=NC1,NC2
           JJ=JJ+1
           IV2(JJ)=IV1(J)
         ENDDO
         IF(JJ<10)EXIT
         WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
         NC1=NC2+1
         JJ=0
         IF(NC2==N)EXIT
        ENDDO
       ENDDO ! I
       IF(JJ/=0)WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
      RETURN
      END
