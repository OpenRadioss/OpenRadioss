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
!||    i3cor3   ../engine/source/interfaces/inter3d/i3cor3.F
!||--- called by ------------------------------------------------------
!||    i6main   ../engine/source/interfaces/inter3d/i6main.F
!||    intvo3   ../engine/source/interfaces/inter3d/intvo3.F
!||====================================================================
      SUBROUTINE I3COR3(
     1   X,       IRECT,   MSR,     NSV,
     2   IRTL,    IX1,     IX2,     IX3,
     3   IX4,     X1,      X2,      X3,
     4   X4,      Y1,      Y2,      Y3,
     5   Y4,      Z1,      Z2,      Z3,
     6   Z4,      LFT,     LLT,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: NFT
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT):: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Z1,Z2,Z3,Z4
      my_real
     .   X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, L, NN
C-----------------------------------------------
      DO 100 I=LFT,LLT
      IL=I+NFT
      IG=NSV(IL)
      L=IRTL(IL)
C
      NN=MSR(IRECT(1,L))
      IX1(I)=NN
      X1(I)=X(1,NN)
      Y1(I)=X(2,NN)
      Z1(I)=X(3,NN)
C
      NN=MSR(IRECT(2,L))
      IX2(I)=NN
      X2(I)=X(1,NN)
      Y2(I)=X(2,NN)
      Z2(I)=X(3,NN)
C
      NN=MSR(IRECT(3,L))
      IX3(I)=NN
      X3(I)=X(1,NN)
      Y3(I)=X(2,NN)
      Z3(I)=X(3,NN)
C
      NN=MSR(IRECT(4,L))
      IX4(I)=NN
      X4(I)=X(1,NN)
      Y4(I)=X(2,NN)
      Z4(I)=X(3,NN)
C
 100  CONTINUE
C
      RETURN
      END
