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
Chd|  SFOR_VISN10                   source/elements/solid/solide10/sfor_visn10.F
Chd|-- called by -----------
Chd|        S10FOR_DISTOR                 source/elements/solid/solide10/s10for_distor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SFOR_VISN10(VC ,    FLD,   TOL_V,     MU ,    
     .                       VX ,     VY,      VZ,   IFCTL,
     .                    FOR_T1, FOR_T2,  FOR_T3,  FOR_T4,
     .                    FOR_T5, FOR_T6,  FOR_T7,  FOR_T8,
     .                    FOR_T9, FOR_T10, STIF  ,    IFC1,
     .                    NEL  )
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
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: FLD 
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: STIF
      my_real, DIMENSION(MVSIZ,10), INTENT (IN) :: 
     .                            VX,     VY,     VZ
      my_real, DIMENSION(MVSIZ,3), INTENT (IN)  :: VC
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: 
     .                     FOR_T1, FOR_T2, FOR_T3, FOR_T4,
     .                     FOR_T5, FOR_T6, FOR_T7, FOR_T8,
     .                     FOR_T9, FOR_T10
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
     .   VRE(10),VXC,VYC,VZC,FX,FY,FZ,FAC,VNJ,VL,TOL_V2,
     .   VXM,VYM,VZM,V2MAX(MVSIZ),VC2
C----------------------------
        TOL_V2 = TOL_V*TOL_V
        IFCTL = 0
        V2MAX(1:NEL) = ZERO
        FAC = ONE + TWO*MU
         DO J =1,10
           DO I=1,NEL
             IF (STIF(I)==ZERO) CYCLE
             VNJ = VX(I,J)*VX(I,J) + VY(I,J)*VY(I,J) + VZ(I,J)*VZ(I,J) 
             V2MAX(I) = MAX(V2MAX(I),VNJ)
           ENDDO
         END DO 
         DO I=1,NEL
           VC2 = VC(I,1)*VC(I,1)+VC(I,2)*VC(I,2)+VC(I,3)*VC(I,3)
           IF (STIF(I)==ZERO.OR.VC2 <EM20) CYCLE
           VL = TOL_V2*VC2
           IF (V2MAX(I) > VL) THEN
              IFC1(I) = 1
              IFCTL=1
           END IF
         END DO
C
       IF (IFCTL==1) THEN
         DO I=1,NEL
           IF (IFC1(I)==0) CYCLE
           FOR_T1(I,1) = FOR_T1(I,1) - FLD(I)*(VX(I,1)-VC(I,1))
           FOR_T1(I,2) = FOR_T1(I,2) - FLD(I)*(VY(I,1)-VC(I,2))
           FOR_T1(I,3) = FOR_T1(I,3) - FLD(I)*(VZ(I,1)-VC(I,3))
           FOR_T2(I,1) = FOR_T2(I,1) - FLD(I)*(VX(I,2)-VC(I,1))
           FOR_T2(I,2) = FOR_T2(I,2) - FLD(I)*(VY(I,2)-VC(I,2))
           FOR_T2(I,3) = FOR_T2(I,3) - FLD(I)*(VZ(I,2)-VC(I,3))
           FOR_T3(I,1) = FOR_T3(I,1) - FLD(I)*(VX(I,3)-VC(I,1))
           FOR_T3(I,2) = FOR_T3(I,2) - FLD(I)*(VY(I,3)-VC(I,2))
           FOR_T3(I,3) = FOR_T3(I,3) - FLD(I)*(VZ(I,3)-VC(I,3))
           FOR_T4(I,1) = FOR_T4(I,1) - FLD(I)*(VX(I,4)-VC(I,1))
           FOR_T4(I,2) = FOR_T4(I,2) - FLD(I)*(VY(I,4)-VC(I,2))
           FOR_T4(I,3) = FOR_T4(I,3) - FLD(I)*(VZ(I,4)-VC(I,3))
           FOR_T5(I,1) = FOR_T5(I,1) - FLD(I)*(VX(I,5)-VC(I,1))
           FOR_T5(I,2) = FOR_T5(I,2) - FLD(I)*(VY(I,5)-VC(I,2))
           FOR_T5(I,3) = FOR_T5(I,3) - FLD(I)*(VZ(I,5)-VC(I,3))
           FOR_T6(I,1) = FOR_T6(I,1) - FLD(I)*(VX(I,6)-VC(I,1))
           FOR_T6(I,2) = FOR_T6(I,2) - FLD(I)*(VY(I,6)-VC(I,2))
           FOR_T6(I,3) = FOR_T6(I,3) - FLD(I)*(VZ(I,6)-VC(I,3))
           FOR_T7(I,1) = FOR_T7(I,1) - FLD(I)*(VX(I,7)-VC(I,1))
           FOR_T7(I,2) = FOR_T7(I,2) - FLD(I)*(VY(I,7)-VC(I,2))
           FOR_T7(I,3) = FOR_T7(I,3) - FLD(I)*(VZ(I,7)-VC(I,3))
           FOR_T8(I,1) = FOR_T8(I,1) - FLD(I)*(VX(I,8)-VC(I,1))
           FOR_T8(I,2) = FOR_T8(I,2) - FLD(I)*(VY(I,8)-VC(I,2))
           FOR_T8(I,3) = FOR_T8(I,3) - FLD(I)*(VZ(I,8)-VC(I,3))
           FOR_T9(I,1) = FOR_T9(I,1) - FLD(I)*(VX(I,9)-VC(I,1))
           FOR_T9(I,2) = FOR_T9(I,2) - FLD(I)*(VY(I,9)-VC(I,2))
           FOR_T9(I,3) = FOR_T9(I,3) - FLD(I)*(VZ(I,9)-VC(I,3))
           FOR_T10(I,1) = FOR_T10(I,1) - FLD(I)*(VX(I,10)-VC(I,1))
           FOR_T10(I,2) = FOR_T10(I,2) - FLD(I)*(VY(I,10)-VC(I,2))
           FOR_T10(I,3) = FOR_T10(I,3) - FLD(I)*(VZ(I,10)-VC(I,3))
           STIF(I)      = FAC*STIF(I)
         ENDDO
       END IF 
C
      RETURN
      END
