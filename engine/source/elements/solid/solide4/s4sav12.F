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
!||    s4sav12   ../engine/source/elements/solid/solide4/s4sav12.F
!||--- called by ------------------------------------------------------
!||    s4forc3   ../engine/source/elements/solid/solide4/s4forc3.F
!||====================================================================
      SUBROUTINE S4SAV12(
     1   OFFG,    OFFG0,   SAV,     XD1,
     2   XD2,     XD3,     XD4,     YD1,
     3   YD2,     YD3,     YD4,     ZD1,
     4   ZD2,     ZD3,     ZD4,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*),OFFG0(*)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),SAV(NEL,9)
           
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C
       DO I=1,NEL
       IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE)THEN
        SAV(I,1)=XD1(I)-XD4(I)
        SAV(I,2)=YD1(I)-YD4(I)
        SAV(I,3)=ZD1(I)-ZD4(I)
        SAV(I,4)=XD2(I)-XD4(I)
        SAV(I,5)=YD2(I)-YD4(I)
        SAV(I,6)=ZD2(I)-ZD4(I)
        SAV(I,7)=XD3(I)-XD4(I)
        SAV(I,8)=YD3(I)-YD4(I)
        SAV(I,9)=ZD3(I)-ZD4(I)
       ENDIF
       ENDDO
C
      RETURN
      END
