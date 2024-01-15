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
Chd|====================================================================
      SUBROUTINE SFOR_VISN4(VC ,    FLD,   TOL_V,    IFC1,
     .                      VX1,    VX2,     VX3,     VX4,
     .                      VY1,    VY2,     VY3,     VY4,
     .                      VZ1,    VZ2,     VZ3,     VZ4,
     .                   FOR_T1, FOR_T2,  FOR_T3,  FOR_T4,
     .                    IFCTL, STIF  ,     MU ,  NEL  )
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
      INTEGER, INTENT (IN)  :: NEL
      INTEGER, INTENT (OUT) :: IFCTL
      INTEGER, DIMENSION(MVSIZ),INTENT (INOUT) :: IFC1
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: STIF
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: FLD ,
     4                VX1,     VX2,     VX3,     VX4,
     6                VY1,     VY2,     VY3,     VY4,
     8                VZ1,     VZ2,     VZ3,     VZ4
      my_real, DIMENSION(MVSIZ,3), INTENT (IN)  :: VC
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: 
     .                     FOR_T1, FOR_T2, FOR_T3, FOR_T4
      my_real, INTENT (IN) ::  TOL_V,MU
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C                                                                     12
      my_real
     .   VXC,VYC,VZC,FX,FY,FZ,FAC,VNJ(4),VL,TOL_V2,
     .   VXMAX,VYMAX,VZMAX,V2MAX,VC2,LAM_MIN,LAM_MIN2,LAM_MAX
C----------------------------
        TOL_V2 = TOL_V*TOL_V
        IFCTL = 0
         DO I=1,NEL
           VC2 = VC(I,1)*VC(I,1)+VC(I,2)*VC(I,2)+VC(I,3)*VC(I,3)
           IF (VC2 <EM20.OR.STIF(I)==ZERO) CYCLE
           VL = TOL_V2*VC2
           VNJ(1) = VX1(I)*VX1(I) + VY1(I)*VY1(I) + VZ1(I)*VZ1(I)
           VNJ(2) = VX2(I)*VX2(I) + VY2(I)*VY2(I) + VZ2(I)*VZ2(I)
           VNJ(3) = VX3(I)*VX3(I) + VY3(I)*VY3(I) + VZ3(I)*VZ3(I)
           VNJ(4) = VX4(I)*VX4(I) + VY4(I)*VY4(I) + VZ4(I)*VZ4(I)
           V2MAX = MAX(VNJ(1),VNJ(2),VNJ(3),VNJ(4))
           IF (V2MAX > VL) THEN
              IFC1(I) = 1
              IFCTL=1
           END IF
         END DO
C
       IF (IFCTL==1) THEN
         FAC = ONE + TWO*MU
         DO I=1,NEL
           IF (IFC1(I)==0) CYCLE
           FOR_T1(I,1) = FOR_T1(I,1) - FLD(I)*(VX1(I)-VC(I,1))
           FOR_T1(I,2) = FOR_T1(I,2) - FLD(I)*(VY1(I)-VC(I,2))
           FOR_T1(I,3) = FOR_T1(I,3) - FLD(I)*(VZ1(I)-VC(I,3))
           FOR_T2(I,1) = FOR_T2(I,1) - FLD(I)*(VX2(I)-VC(I,1))
           FOR_T2(I,2) = FOR_T2(I,2) - FLD(I)*(VY2(I)-VC(I,2))
           FOR_T2(I,3) = FOR_T2(I,3) - FLD(I)*(VZ2(I)-VC(I,3))
           FOR_T3(I,1) = FOR_T3(I,1) - FLD(I)*(VX3(I)-VC(I,1))
           FOR_T3(I,2) = FOR_T3(I,2) - FLD(I)*(VY3(I)-VC(I,2))
           FOR_T3(I,3) = FOR_T3(I,3) - FLD(I)*(VZ3(I)-VC(I,3))
           FOR_T4(I,1) = FOR_T4(I,1) - FLD(I)*(VX4(I)-VC(I,1))
           FOR_T4(I,2) = FOR_T4(I,2) - FLD(I)*(VY4(I)-VC(I,2))
           FOR_T4(I,3) = FOR_T4(I,3) - FLD(I)*(VZ4(I)-VC(I,3))
           STIF(I)      = FAC*STIF(I)
         ENDDO
       END IF 
C
      RETURN
      END
