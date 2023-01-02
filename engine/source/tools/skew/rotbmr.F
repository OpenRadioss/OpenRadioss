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
Chd|  ROTBMR                        source/tools/skew/rotbmr.F    
Chd|-- called by -----------
Chd|        IMP_DYCRB                     source/implicit/imp_dyna.F    
Chd|        LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|        MOVFRA2                       source/tools/skew/movfram.F   
Chd|        MOVFRA_IMP                    source/tools/skew/movfram.F   
Chd|        RGBODFP                       source/constraints/general/rbody/rgbodfp.F
Chd|        RMATPON                       source/materials/mat/mat013/rmatpon.F
Chd|        SRFVIT                        source/constraints/general/rbody/srfvit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTBMR (VR,RBY,DT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   DT
C     REAL
      my_real
     .   VR(3), RBY(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   RX, RY, RZ, RX2, RY2, RZ2, R2, R, C, CM1, S, SZ, CZ, E11,
     .   E22, E12, E21, E13, E23, BL11, BL21, BL31, BL12, BL22, BL32,
     .   BN
C-----------------------------------------------
       RX = DT*VR(1)
       RY = DT*VR(2)
       RZ = DT*VR(3)
C
       RX2 = RX*RX
       RY2 = RY*RY
       RZ2 = RZ*RZ
C
       R2 = MAX(EM10,RX2+RY2+RZ2)
       R  = SQRT(R2)
C
       C = COS(R)
       CM1 = (1-C)
       C = C*R2
       S = SIN(R)*R
       SZ = RZ*S
       CZ = RZ*CM1
C
       E11 = RX2*CM1 + C
       E22 = RY2*CM1 + C
       E12 = RX*RY*CM1
       E21 = E12 - SZ
       E12 = E12 + SZ
       E13 = RX*CZ - RY*S
       E23 = RY*CZ + RX*S
C
       BL11 = RBY(1)*E11 + RBY(4)*E12 + RBY(7)*E13
       BL21 = RBY(2)*E11 + RBY(5)*E12 + RBY(8)*E13
       BL31 = RBY(3)*E11 + RBY(6)*E12 + RBY(9)*E13
C
       BL12 = RBY(1)*E21 + RBY(4)*E22 + RBY(7)*E23
       BL22 = RBY(2)*E21 + RBY(5)*E22 + RBY(8)*E23
       BL32 = RBY(3)*E21 + RBY(6)*E22 + RBY(9)*E23
C
       BN = SQRT(BL11*BL11+BL21*BL21+BL31*BL31)
       BL11=BL11/BN
       BL21=BL21/BN
       BL31=BL31/BN
       BN = SQRT(BL12*BL12+BL22*BL22+BL32*BL32)
       BL12=BL12/BN
       BL22=BL22/BN
       BL32=BL32/BN
C
       RBY(1) = BL11
       RBY(2) = BL21
       RBY(3) = BL31
C
       RBY(4) = BL12
       RBY(5) = BL22
       RBY(6) = BL32
C
       RBY(7) = BL21*BL32 - BL31*BL22
       RBY(8) = BL31*BL12 - BL11*BL32
       RBY(9) = BL11*BL22 - BL21*BL12
C
C---------------------------
      RETURN
      END
