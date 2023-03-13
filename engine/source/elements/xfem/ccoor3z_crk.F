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
Chd|  CCOOR3Z_CRK                   source/elements/xfem/ccoor3z_crk.F
Chd|-- called by -----------
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CCOOR3Z_CRK(JFT  ,JLT  ,NFT  ,IEL_CRK,IADC_CRK,
     2                       VL1  ,VL2  ,VL3  ,VL4    ,VRL1    ,
     3                       VRL2 ,VRL3 ,VRL4 ,X1     ,X2      ,
     4                       X3   ,X4   ,Y1   ,Y2     ,Y3      ,
     5                       Y4   ,Z1   ,Z2   ,Z3     ,Z4      ,
     6                       VX1  ,VX2  ,VX3  ,VX4    ,VY1     ,
     7                       VY2  ,VY3  ,VY4  ,VZ1    ,VZ2     ,
     8                       VZ3  ,VZ4  ,VRX1 ,VRX2   ,VRX3    ,
     9                       VRX4 ,VRY1 ,VRY2 ,VRY3   ,VRY4    ,
     A                       VRZ1 ,VRZ2 ,VRZ3 ,VRZ4   ,ILEV    ,
     B                       OFFG )
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
      INTEGER JFT,JLT,NFT,IEL_CRK(*),IADC_CRK(4,*),ILEV
C     REAL
      my_real
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),Y1(MVSIZ),
     .   Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .   VRX1(MVSIZ),VRX2(MVSIZ),VRX3(MVSIZ),VRX4(MVSIZ),
     .   VRY1(MVSIZ),VRY2(MVSIZ),VRY3(MVSIZ),VRY4(MVSIZ),
     .   VRZ1(MVSIZ),VRZ2(MVSIZ),VRZ3(MVSIZ),VRZ4(MVSIZ),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ELCRK,IADC1,IADC2,IADC3,IADC4
      my_real
     .   OFF_L
C=======================================================================
C get elem from activ level: (superposed element)
C-----------------------------------------------
        DO I=JFT,JLT
          ELCRK = IEL_CRK(I+NFT)
          IADC1 = IADC_CRK(1,ELCRK)
          IADC2 = IADC_CRK(2,ELCRK)
          IADC3 = IADC_CRK(3,ELCRK)
          IADC4 = IADC_CRK(4,ELCRK)
C--------------
C  COORDINATES:  coord nodaled des elements phantomes
C--------------
C  node 1:
          X1(I) = CRKAVX(ILEV)%X(1,IADC1)
          Y1(I) = CRKAVX(ILEV)%X(2,IADC1)
          Z1(I) = CRKAVX(ILEV)%X(3,IADC1)
C  node 2:
          X2(I) = CRKAVX(ILEV)%X(1,IADC2)
          Y2(I) = CRKAVX(ILEV)%X(2,IADC2)
          Z2(I) = CRKAVX(ILEV)%X(3,IADC2)
C  node 3:
          X3(I) = CRKAVX(ILEV)%X(1,IADC3)
          Y3(I) = CRKAVX(ILEV)%X(2,IADC3)
          Z3(I) = CRKAVX(ILEV)%X(3,IADC3)
C  node 4:
          X4(I) = CRKAVX(ILEV)%X(1,IADC4)
          Y4(I) = CRKAVX(ILEV)%X(2,IADC4)
          Z4(I) = CRKAVX(ILEV)%X(3,IADC4)
C--------------
C  VELOCITIES :
C--------------
C  node 1:
          VL1(I,1)  = CRKAVX(ILEV)%V(1,IADC1)
          VL1(I,2)  = CRKAVX(ILEV)%V(2,IADC1)
          VL1(I,3)  = CRKAVX(ILEV)%V(3,IADC1)
          VRL1(I,1) = CRKAVX(ILEV)%VR(1,IADC1)
          VRL1(I,2) = CRKAVX(ILEV)%VR(2,IADC1)
          VRL1(I,3) = CRKAVX(ILEV)%VR(3,IADC1)
C  node 2:
          VL2(I,1)  = CRKAVX(ILEV)%V(1,IADC2)
          VL2(I,2)  = CRKAVX(ILEV)%V(2,IADC2)
          VL2(I,3)  = CRKAVX(ILEV)%V(3,IADC2)
          VRL2(I,1) = CRKAVX(ILEV)%VR(1,IADC2)
          VRL2(I,2) = CRKAVX(ILEV)%VR(2,IADC2)
          VRL2(I,3) = CRKAVX(ILEV)%VR(3,IADC2)
C  node 3:
          VL3(I,1)  = CRKAVX(ILEV)%V(1,IADC3)
          VL3(I,2)  = CRKAVX(ILEV)%V(2,IADC3)
          VL3(I,3)  = CRKAVX(ILEV)%V(3,IADC3)
          VRL3(I,1) = CRKAVX(ILEV)%VR(1,IADC3)
          VRL3(I,2) = CRKAVX(ILEV)%VR(2,IADC3)
          VRL3(I,3) = CRKAVX(ILEV)%VR(3,IADC3)
C  node 4:
          VL4(I,1)  = CRKAVX(ILEV)%V(1,IADC4)
          VL4(I,2)  = CRKAVX(ILEV)%V(2,IADC4)
          VL4(I,3)  = CRKAVX(ILEV)%V(3,IADC4)
          VRL4(I,1) = CRKAVX(ILEV)%VR(1,IADC4)
          VRL4(I,2) = CRKAVX(ILEV)%VR(2,IADC4)
          VRL4(I,3) = CRKAVX(ILEV)%VR(3,IADC4)
        END DO
C----------------------------------
        OFF_L = ZERO
        DO I=JFT,JLT
          OFF_L  = MIN(OFF_L,OFFG(I))
        ENDDO
C----------------------------------
        IF (OFF_L < ZERO) THEN
          DO I=JFT,JLT
           IF (OFFG(I) < ZERO) THEN
             VL1(I,1)=ZERO
             VL1(I,2)=ZERO
             VL1(I,3)=ZERO
             VL2(I,1)=ZERO
             VL2(I,2)=ZERO
             VL2(I,3)=ZERO
             VL3(I,1)=ZERO
             VL3(I,2)=ZERO
             VL3(I,3)=ZERO
             VL4(I,1)=ZERO
             VL4(I,2)=ZERO
             VL4(I,3)=ZERO
             VRL1(I,1)=ZERO
             VRL1(I,2)=ZERO
             VRL1(I,3)=ZERO
             VRL2(I,1)=ZERO
             VRL2(I,2)=ZERO
             VRL2(I,3)=ZERO
             VRL3(I,1)=ZERO
             VRL3(I,2)=ZERO
             VRL3(I,3)=ZERO
             VRL4(I,1)=ZERO
             VRL4(I,2)=ZERO
             VRL4(I,3)=ZERO
           ENDIF
          ENDDO
        ENDIF
C----------------------------------
C
        DO I=JFT,JLT
          VX1(I)=VL1(I,1)
          VY1(I)=VL1(I,2)
          VZ1(I)=VL1(I,3)
          VX2(I)=VL2(I,1)
          VY2(I)=VL2(I,2)
          VZ2(I)=VL2(I,3)
          VX3(I)=VL3(I,1)
          VY3(I)=VL3(I,2)
          VZ3(I)=VL3(I,3)
          VX4(I)=VL4(I,1)
          VY4(I)=VL4(I,2)
          VZ4(I)=VL4(I,3)
C
          VRX1(I)=VRL1(I,1)
          VRY1(I)=VRL1(I,2)
          VRZ1(I)=VRL1(I,3)
          VRX2(I)=VRL2(I,1)
          VRY2(I)=VRL2(I,2)
          VRZ2(I)=VRL2(I,3)
          VRX3(I)=VRL3(I,1)
          VRY3(I)=VRL3(I,2)
          VRZ3(I)=VRL3(I,3)
          VRX4(I)=VRL4(I,1)
          VRY4(I)=VRL4(I,2)
          VRZ4(I)=VRL4(I,3)
        END DO
C-----------------------------------------------
      RETURN
      END
