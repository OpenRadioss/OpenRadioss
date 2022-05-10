Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  CCOOR3_CRK                    source/elements/xfem/ccoor3_crk.F
Chd|-- called by -----------
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CCOOR3_CRK(JFT  ,JLT  ,NFT  ,IXC    ,NGL     ,
     2                      PID  ,MAT  ,ILEV ,IEL_CRK,IADC_CRK,
     3                      OFFG ,OFF  ,SIGY ,THKE   ,THK0    ,
     4                      DT1C ,VL1  ,VL2  ,VL3    ,VL4     ,
     5                      VRL1 ,VRL2 ,VRL3 ,VRL4   ,X1      ,
     6                      X2   ,X3   ,X4   ,Y1     ,Y2      ,
     7                      Y3   ,Y4   ,Z1   ,Z2     ,Z3      ,
     8                      Z4   )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,ILEV
      INTEGER NGL(MVSIZ),IXC(NIXC,*),MAT(MVSIZ),PID(MVSIZ),
     .   IEL_CRK(*),IADC_CRK(4,*),NFT
C     REAL ou REAL*8
      my_real
     .   OFFG(*),OFF(*),SIGY(*),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),DT1C(*),THKE(*),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),Y1(MVSIZ),
     .   Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),THK0(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ELCRK,IADC1,IADC2,IADC3,IADC4
C     REAL ou REAL*8
      my_real
     .   OFF_L
C-----------------------------------------------
C get elem from activ level: (superposed element)
C-----------------------------------------------
C-----------------------------------------------
        DO I=JFT,JLT
          ELCRK = IEL_CRK(I+NFT)
          IADC1 = IADC_CRK(1,ELCRK)
          IADC2 = IADC_CRK(2,ELCRK)
          IADC3 = IADC_CRK(3,ELCRK)
          IADC4 = IADC_CRK(4,ELCRK)
C--------------
C  COORDINATES:
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
C-----------------------------------------------
      DO I=JFT,JLT
        MAT(I)=IXC(1,I)
        NGL(I)=IXC(7,I)
        PID(I)=IXC(6,I)
        THK0(I) = THKE(I)
      END DO
C----------------------------------
      OFF_L = ZERO
      DO I=JFT,JLT
        DT1C(I) = DT1
        OFF(I) = MIN(UN,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
        SIGY(I) = 1.E30
      ENDDO
C----------------------------------
      IF(OFF_L.LT.ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I).LT.ZERO)THEN
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
      RETURN
      END
