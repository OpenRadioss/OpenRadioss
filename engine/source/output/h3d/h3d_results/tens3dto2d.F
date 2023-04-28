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
Chd|  TENS3DTO2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        ROT3DTO2D                     source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|====================================================================
      SUBROUTINE TENS3DTO2D(NEL,IXC,X,TEN3,TEN2)
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
      INTEGER NEL,IXC(5,*)
      my_real
     .   X(3,*),TEN3(6,*),TEN2(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N , K
      INTEGER 
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
C     REAL
      my_real 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,
     .   SX(MVSIZ) ,SY(MVSIZ) ,SZ(MVSIZ) ,
     .   TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),
     .   DETA1(MVSIZ),OFFG(MVSIZ),TENS(6,MVSIZ)  
C-----------------------------------------------
        DO I=1,NEL
          IX1(I)=IXC(2,I)
          IX2(I)=IXC(3,I)
          IX3(I)=IXC(4,I)
          IX4(I)=IXC(5,I)
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
        ENDDO
        DO I=1,NEL
          RX(I)=X2(I)+X3(I)-X1(I)-X4(I)
          SX(I)=X3(I)+X4(I)-X1(I)-X2(I)
          RY(I)=Y2(I)+Y3(I)-Y1(I)-Y4(I)
          SY(I)=Y3(I)+Y4(I)-Y1(I)-Y2(I)
          RZ(I)=Z2(I)+Z3(I)-Z1(I)-Z4(I)
          SZ(I)=Z3(I)+Z4(I)-Z1(I)-Z2(I)
        ENDDO
       K = 0
       OFFG(1:NEL) = ONE
       CALL CLSKEW3(1,NEL,K,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z,
     .   DETA1,OFFG )
        DO I=1,NEL
          IF (IX3(I)==IX4(I)) THEN
           TENS(1:6,I) = THIRD*(TEN3(1:6,IX1(I))+TEN3(1:6,IX2(I))+TEN3(1:6,IX3(I)))
          ELSE
           TENS(1:6,I) = FOURTH*(TEN3(1:6,IX1(I))+TEN3(1:6,IX2(I))
     .                         +TEN3(1:6,IX3(I))+TEN3(1:6,IX4(I)))
          END IF
        ENDDO
        CALL ROT3DTO2D(NEL,TENS,TEN2,E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z)
C-----------
      RETURN
      END
