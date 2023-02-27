Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
      SUBROUTINE S10DVM12(
     .   PX, PY, PZ, VX, VY, VZ, 
     .   DVM ,OFFG,NPT ,NEL)
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
      INTEGER  NEL,NPT
C     REAL
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFFG
      my_real ,DIMENSION(MVSIZ,10) ,INTENT(IN) :: VX,VY,VZ
      my_real ,DIMENSION(MVSIZ,10,5),INTENT(IN) :: PX,PY,PZ
      my_real ,DIMENSION(MVSIZ) ,INTENT(OUT) :: DVM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N,IP
C     REAL
      my_real 
     .   DXX,DYY,DZZ,FAC
C-----------------------------------------------
      DVM(1:NEL) =ZERO
      DO IP=1,NPT
        DO I=1,NEL
          IF(OFFG(I)<=ONE) CYCLE
          DXX=PX(I,1,IP)*VX(I,1)+PX(I,2,IP)*VX(I,2)+PX(I,3,IP)*VX(I,3)+PX(I,4,IP)*VX(I,4)+PX(I,5,IP)*VX(I,5)
     .       +PX(I,6,IP)*VX(I,6)+PX(I,7,IP)*VX(I,7)+PX(I,8,IP)*VX(I,8)+PX(I,9,IP)*VX(I,9)+PX(I,10,IP)*VX(I,10)
          DYY=PY(I,1,IP)*VY(I,1)+PY(I,2,IP)*VY(I,2)+PY(I,3,IP)*VY(I,3)+PY(I,4,IP)*VY(I,4)+PY(I,5,IP)*VY(I,5)
     .       +PY(I,6,IP)*VY(I,6)+PY(I,7,IP)*VY(I,7)+PY(I,8,IP)*VY(I,8)+PY(I,9,IP)*VY(I,9)+PY(I,10,IP)*VY(I,10)
          DZZ=PZ(I,1,IP)*VZ(I,1)+PZ(I,2,IP)*VZ(I,2)+PZ(I,3,IP)*VZ(I,3)+PZ(I,4,IP)*VZ(I,4)+PZ(I,5,IP)*VZ(I,5)
     .       +PZ(I,6,IP)*VZ(I,6)+PZ(I,7,IP)*VZ(I,7)+PZ(I,8,IP)*VZ(I,8)+PZ(I,9,IP)*VZ(I,9)+PZ(I,10,IP)*VZ(I,10)
          DVM(I) = DVM(I)+ DXX + DYY + DZZ
        END DO
      END DO
      FAC = ONE/NPT
      DVM(1:NEL) = DVM(1:NEL)*FAC
C
      RETURN
C
      END SUBROUTINE S10DVM12
