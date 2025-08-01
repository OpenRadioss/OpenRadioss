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
!||    e4pxle3   ../engine/source/elements/solid/solide4/e4pxle3.F
!||--- called by ------------------------------------------------------
!||    s4forc3   ../engine/source/elements/solid/solide4/s4forc3.F
!||====================================================================
      SUBROUTINE E4PXLE3(
     1   VOL,     VEUL,    X1,      X2,
     2   X3,      X4,      Y1,      Y2,
     3   Y3,      Y4,      Z1,      Z2,
     4   Z3,      Z4,      PX1,     PX2,
     5   PX3,     PX4,     PY1,     PY2,
     6   PY3,     PY4,     PZ1,     PZ2,
     7   PZ3,     PZ4,     DET,     DELTAX,
     8   NEL,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      my_real
     .   VOL(*), VEUL(LVEUL,*),
     .   X1(*), X2(*), X3(*), X4(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), 
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   DET(*),DELTAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C     REAL
C-----------------------------------------------
C
      DO  I=1,NEL
        II=I+NFT
        PX1(I)=VEUL(1,II)
        PX2(I)=VEUL(2,II)
        PX3(I)=VEUL(3,II)
        PX4(I)=VEUL(4,II)
C
        PY1(I)=VEUL(5,II)
        PY2(I)=VEUL(6,II)
        PY3(I)=VEUL(7,II)
        PY4(I)=VEUL(8,II)
C
        PZ1(I)=VEUL(9,II)
        PZ2(I)=VEUL(10,II)
        PZ3(I)=VEUL(11,II)
        PZ4(I)=VEUL(12,II)
      ENDDO
C
      DO I=1,NEL
       II=I+NFT
       DELTAX(I)=VEUL(13,II)
       DET(I)=VOL(I)
      ENDDO
C
      RETURN
C
      END
