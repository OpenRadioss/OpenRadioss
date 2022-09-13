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
Chd|  C3COOR3_CRK                   source/elements/xfem/c3coor3_crk.F
Chd|-- called by -----------
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE C3COOR3_CRK(JFT    ,JLT    ,OFFG   ,OFF    ,SIGY    ,
     .                       DT1C   ,THKE   ,VL1    ,VL2    ,VL3     ,
     .                       VRL1   ,VRL2   ,VRL3   ,X1     ,X2      ,
     .                       X3     ,Y1     ,Y2     ,Y3     ,Z1      ,
     .                       Z2     ,Z3     ,ILEV   ,IEL_CRK,IADC_CRK,
     .                       NFT    )
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
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      INTEGER ILEV,IEL_CRK(*),IADC_CRK(3,*),NFT
      my_real OFFG(*), OFF(*),DT1C(*),THKE(*),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),SIGY(*)
!       SP issue :
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::X1,X2,X3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::Y1,Y2,Y3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::Z1,Z2,Z3  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NC1,NC2,NC3,ELCRK,ELCRK0,IADC1,IADC2,IADC3
      my_real OFF_L    
C=======================================================================
C get elem from activ level: (superposed element)
C-----------------------------------------------
        DO I=JFT,JLT
          ELCRK0 = IEL_CRK(I+NFT)
          ELCRK = ELCRK0 + ECRKXFEC
          IADC1 = IADC_CRK(1,ELCRK0)
          IADC2 = IADC_CRK(2,ELCRK0)
          IADC3 = IADC_CRK(3,ELCRK0)
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
C--------------
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
        END DO
C-----------------------------------------------
      OFF_L = ZERO
      DO 40 I=JFT,JLT
        DT1C(I) = DT1
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
        SIGY(I) = EP30
 40   CONTINUE
C---
      IF(OFF_L<ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I)<ZERO)THEN
           VL1(I,1)=ZERO
           VL1(I,2)=ZERO
           VL1(I,3)=ZERO
           VL2(I,1)=ZERO
           VL2(I,2)=ZERO
           VL2(I,3)=ZERO
           VL3(I,1)=ZERO
           VL3(I,2)=ZERO
           VL3(I,3)=ZERO
           VRL1(I,1)=ZERO
           VRL1(I,2)=ZERO
           VRL1(I,3)=ZERO
           VRL2(I,1)=ZERO
           VRL2(I,2)=ZERO
           VRL2(I,3)=ZERO
           VRL3(I,1)=ZERO
           VRL3(I,2)=ZERO
           VRL3(I,3)=ZERO
         ENDIF
        ENDDO
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  C3COOR3_CRK2                  source/elements/xfem/c3coor3_crk.F
Chd|-- called by -----------
Chd|        C3BILAN                       source/elements/sh3n/coque3n/c3bilan.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE C3COOR3_CRK2(
     1              JFT  ,JLT  ,NFT  ,ILEV     ,IEL_CRK,
     2              X    ,V    ,VR   ,VL1      ,VL2    ,
     3              VL3  ,VRL1 ,VRL2 ,VRL3     ,X1     ,
     4              X2   ,X3   ,Y1   ,Y2       ,Y3     ,
     5              Z1   ,Z2   ,Z3   ,IADTG_CRK)
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
      INTEGER JFT,JLT,NFT,ILEV
      INTEGER IEL_CRK(*),IADTG_CRK(3,*)
C     REAL
      my_real
     .   X(3,*),V(3,*),VR(3,*),VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ELCRK,IADC1,IADC2,IADC3,ELCRK0
C-----------------------------------------------
        DO I=JFT,JLT
          ELCRK0 = IEL_CRK(I+NFT)
          ELCRK = ELCRK0 + ECRKXFEC
          IADC1 = IADTG_CRK(1,ELCRK0)
          IADC2 = IADTG_CRK(2,ELCRK0)
          IADC3 = IADTG_CRK(3,ELCRK0)
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
        END DO
C-----------------------------------------------
C
      RETURN
      END
