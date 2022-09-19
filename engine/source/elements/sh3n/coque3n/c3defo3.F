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
Chd|  C3DEFO3                       source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3DEFO3(JFT,JLT,VL1,VL2,VL3,
     .                   IXTG,ISH3N,PX1  ,PY1    ,PY2   ,
     .                   EXX  ,EYY  ,EXY  ,EYZ   ,EZX   ,
     .                   VX13 ,VX23  ,VY12  ,
     .                   E1X      ,E1Y      ,E1Z     ,E2X      ,
     .                   E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IXTG(NIXTG,*), ISH3N
      my_real VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .     EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EYZ(MVSIZ),EZX(MVSIZ),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .     PX1(MVSIZ), PY1(MVSIZ), PY2(MVSIZ), VX13(MVSIZ), VX23(MVSIZ),
     .     VY12(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC1, NC2, NC3, I, J
      my_real
     .     VX1(MVSIZ),  VX2(MVSIZ),  VX3(MVSIZ),
     .     VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ),
     .     VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ),
     .     VX12(MVSIZ), VY13(MVSIZ), VY23(MVSIZ),
     .     VZ12(MVSIZ), VZ13(MVSIZ), VZ23(MVSIZ)
      my_real  DT1V4B,TMP11,TMP22,VX10,VX20,VX30,DT1V4,TMP1,TMP2
C=======================================================================
C
      DO I=JFT,JLT
        VX1(I)=E1X(I)*VL1(I,1)+E1Y(I)*VL1(I,2)+E1Z(I)*VL1(I,3)
        VX2(I)=E1X(I)*VL2(I,1)+E1Y(I)*VL2(I,2)+E1Z(I)*VL2(I,3)
        VX3(I)=E1X(I)*VL3(I,1)+E1Y(I)*VL3(I,2)+E1Z(I)*VL3(I,3)
C
        VY3(I)=E2X(I)*VL3(I,1)+E2Y(I)*VL3(I,2)+E2Z(I)*VL3(I,3)
        VY2(I)=E2X(I)*VL2(I,1)+E2Y(I)*VL2(I,2)+E2Z(I)*VL2(I,3)
        VY1(I)=E2X(I)*VL1(I,1)+E2Y(I)*VL1(I,2)+E2Z(I)*VL1(I,3)
C
        VZ1(I)=E3X(I)*VL1(I,1)+E3Y(I)*VL1(I,2)+E3Z(I)*VL1(I,3)
        VZ2(I)=E3X(I)*VL2(I,1)+E3Y(I)*VL2(I,2)+E3Z(I)*VL2(I,3)
        VZ3(I)=E3X(I)*VL3(I,1)+E3Y(I)*VL3(I,2)+E3Z(I)*VL3(I,3)
      ENDDO
c
      DT1V4 = FOURTH*DT1
      IF (IMPL_S > 0 .AND. IMP_LR == 0 ) DT1V4=ZERO
      DT1V4B= DT1V4
      IF(ISH3N < 2) DT1V4B=ZERO
C
      DO 40 I=JFT,JLT
      VZ12(I)=VZ1(I) - VZ2(I)
      VZ13(I)=VZ1(I) - VZ3(I)
      VZ23(I)=VZ2(I) - VZ3(I)
C
      TMP1 = DT1V4 * VZ12(I) / (PY1(I)+PY2(I))
      TMP2 = (PY1(I) * VZ1(I) + PY2(I) * VZ2(I)) / (PY1(I)+PY2(I))
      TMP2 = DT1V4 * (TMP2 - VZ3(I)) / PX1(I)
      VY12(I)=VY1(I) - VY2(I)
      TMP11 = DT1V4B * VY12(I) / (PY1(I)+PY2(I))
      TMP22 = (PY1(I) * VX1(I) + PY2(I) * VX2(I)) / (PY1(I)+PY2(I))
      TMP22 = DT1V4B * (TMP22 - VX3(I)) / PX1(I)
      VX10 = VX1(I)
      VX20 = VX2(I)
      VX30 = VX3(I)
      VX1(I) = VX1(I) - VZ1(I) * TMP1 - VY1(I) * TMP11
      VX2(I) = VX2(I) - VZ2(I) * TMP1 - VY2(I) * TMP11
      VX3(I) = VX3(I) - VZ3(I) * TMP1 - VY3(I) * TMP11
      VY1(I) = VY1(I) - VZ1(I) * TMP2 - VX10 * TMP22
      VY2(I) = VY2(I) - VZ2(I) * TMP2 - VX20 * TMP22
      VY3(I) = VY3(I) - VZ3(I) * TMP2 - VX30 * TMP22
C
      VX12(I)=VX1(I) - VX2(I)
      VY12(I)=VY1(I) - VY2(I)
      VX13(I)=VX1(I) - VX3(I)
      VY13(I)=VY1(I) - VY3(I)
      VX23(I)=VX2(I) - VX3(I)
      VY23(I)=VY2(I) - VY3(I)
C
      EXX(I)=PX1(I)*VX12(I)
      EYY(I)=PY1(I)*VY13(I) + PY2(I)*VY23(I)
C
      EXY(I)=PY1(I)*VX13(I) + PY2(I)*VX23(I) + PX1(I)*VY12(I)
      EYZ(I)=PY1(I)*VZ13(I) + PY2(I)*VZ23(I)
      EZX(I)=PX1(I)*VZ12(I)
   40 CONTINUE
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3BRZ3                        source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|        C3DERIRZ                      source/elements/sh3n/coque3n/c3defo3.F
Chd|====================================================================
      SUBROUTINE C3BRZ3(JFT  ,JLT  ,AREA ,X2   ,X3    ,
     .                  Y3   ,BM0RZ,B0RZ ,BKRZ ,BERZ  )
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real AREA(*), X2(MVSIZ), X3(MVSIZ), Y3(MVSIZ),
     .   BM0RZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2)
C=======================================================================
c FUNCTION: [B] Matrix :derivation of Alman shape function for Tria
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   AREA(NEL)         - element area A
c  O   BM0RZ(I,J,NEL)   - constant terms of derivations for membrane
C                       I=1:A*Nx,x;I=2:A*Ny,y;I=3:A*(Nx,y+Ny,x); J=1,2(node)
C                        only store J=1,2 as f(j=3)=-f(j=1)-f(j=2)
C  O   B0RZ(J,NEL)       A*(-Nx,y+Ny,x -2Ni) for asymmetric rotation
c  O   BKRZ(J,NEL)     - Ksi terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   BERZ(J,NEL)    - Eta terms of derivation : A*(-Nx,y+Ny,x -2Ni)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I
      my_real Y2(MVSIZ)
C=======================================================================
      Y2(JFT:JLT)=ZERO
      CALL C3DERIRZ(JFT  ,JLT  ,AREA ,X2   ,X3     ,
     .              Y2   ,Y3   ,BM0RZ,B0RZ ,BKRZ   ,
     .              BERZ )
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3DEFRZ                       source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE C3DEFRZ(JFT  ,JLT  ,RLZ   ,BM0RZ ,B0RZ ,
     1                     BKRZ ,BERZ ,E3X   ,E3Y   ,E3Z  ,
     2                     VRL1 ,VRL2 ,VRL3  ,EXX   ,EYY  ,
     3                     EXY  ,PX1  ,PY1   ,PY2   ,WXY  ,
     4                     AREA ,VX13 ,VX23  ,VY12  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .   AREA(*),RLZ(MVSIZ,3),VX13(MVSIZ), VX23(MVSIZ),VY12(MVSIZ),
     .   VRL1(MVSIZ,3), VRL2(MVSIZ,3), VRL3(MVSIZ,3),
     .   E3X(MVSIZ), E3Y(MVSIZ),E3Z(MVSIZ), EXX(*),EYY(*),EXY(*),
     .   BM0RZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2),WXY(*),
     .   PX1(*),PY1(*),PY2(*)
C=======================================================================
c FUNCTION: strains relative to the drilling dof for Tria
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   AREA(NEL)         - element area A
c  I   E3X,E3Y,E3Z(NEL)  - normal vector of Tria
c  I   VRL1,VRL2,VRL3(NEL,J)nodal rotation velocity in global system
C  I   PX1,PY1,PX2=-PX1,PY2(NEL): standard [B] of Tria
c  O   RLZ(J,NEL)          - nodal Rz rotation velocity (J=1-3)
c  O   BM0RZ(I,J,NEL)   - constant terms of derivations for membrane
C                       I=1:A*Nx,x;I=2:A*Ny,y;I=3:A*(Nx,y+Ny,x); J=1,2(node)
C                        only store J=1,2 as f(j=3)=-f(j=1)-f(j=2)
C  O   B0RZ(J,NEL)       A*(-Nx,y+Ny,x -2Ni) for asymmetric rotation
c  O   BKRZ(J,NEL)     - Ksi terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   BERZ(J,NEL)    - Eta terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   WXY(NEL)        - asymmetric strain : 0.5*(-NxJ,y*VxJ+NyJ,x*VyJ)
c  IO  EXX,EYY,EXY(NEL)- membrance strains
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I
      my_real
     .     BXV2,BYV1,A05,RZ13(MVSIZ), RZ23(MVSIZ)
C-----------------------------------------------
       DO I=JFT,JLT
        RLZ(I,1)=E3X(I)*VRL1(I,1)+E3Y(I)*VRL1(I,2)+E3Z(I)*VRL1(I,3)
        RLZ(I,2)=E3X(I)*VRL2(I,1)+E3Y(I)*VRL2(I,2)+E3Z(I)*VRL2(I,3)
        RLZ(I,3)=E3X(I)*VRL3(I,1)+E3Y(I)*VRL3(I,2)+E3Z(I)*VRL3(I,3)
        RZ13(I)=RLZ(I,1)-RLZ(I,3)
        RZ23(I)=RLZ(I,2)-RLZ(I,3)
       END DO
C---------------
C  MEMBRANE CONSTANT
C---------------
      DO I=JFT,JLT
       EXX(I)=EXX(I)+BM0RZ(I,1,1)*RZ13(I)+BM0RZ(I,1,2)*RZ23(I)
       EYY(I)=EYY(I)+BM0RZ(I,2,1)*RZ13(I)+BM0RZ(I,2,2)*RZ23(I)
       EXY(I)=EXY(I)+BM0RZ(I,3,1)*RZ13(I)+BM0RZ(I,3,2)*RZ23(I)
      ENDDO
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ--
      DO I=JFT,JLT
        BYV1= PY1(I)*VX13(I) + PY2(I)*VX23(I)
        BXV2= PX1(I)*VY12(I)
        A05=HALF/AREA(I)
        WXY(I)=(-BYV1+BXV2)*A05
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C3DERIRZ                      source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3BRZ3                        source/elements/sh3n/coque3n/c3defo3.F
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|        C3BM0RZ                       source/elements/sh3n/coque3n/c3defo3.F
Chd|====================================================================
        SUBROUTINE C3DERIRZ(JFT ,JLT  ,AREA ,X2    ,X3    ,
     2                      Y2  ,Y3   ,BMRZ ,B0RZ  ,BKRZ  ,
     3                      BERZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .   X2(MVSIZ),X3(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),AREA(*)
      my_real
     .   BMRZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2)
C=======================================================================
c FUNCTION: derivation of Alman shape function for Tria
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   AREA(NEL)         - element area A
c  I   X2,X3,Y2,Y3(NEL)  - relative local nodal co-ordinates: X2->X21
c  O   BM0RZ(I,J,NEL)   - constant terms of derivations for membrane
C                       I=1:A*Nx,x;I=2:A*Ny,y;I=3:A*(Nx,y+Ny,x); J=1,2(node)
C                        only store J=1,2 as f(j=3)=-f(j=1)-f(j=2)
C  O   B0RZ(NEL,J)       A*(-Nx,y+Ny,x -2Ni) for asymmetric rotation
c  O   BKRZ(NEL,J)     - Ksi terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   BERZ(NEL,J)    - Eta terms of derivation : A*(-Nx,y+Ny,x -2Ni)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   A2,BXV2,BYV1,A05,NXY,NYX,
     .   X2L(MVSIZ),X3L(MVSIZ),Y2L(MVSIZ),Y3L(MVSIZ),BM0RZ(MVSIZ,4,2),
     .   BMKRZ(MVSIZ,4,3),BMERZ(MVSIZ,4,3),Y31_2,Y21_2,X31_2,X21_2,
     .   Y21Y31,X21X31,X21Y31,X31Y21,X21Y21,X31Y31
C--------X2L=(X2-X1)/4---------------------------------------
      DO I=JFT,JLT
       X2L(I)=FOURTH*X2(I)
       X3L(I)=FOURTH*X3(I)
       Y2L(I)=FOURTH*Y2(I)
       Y3L(I)=FOURTH*Y3(I)
      ENDDO
C
      DO I=JFT,JLT
       Y31_2=Y3L(I)*Y3L(I)
       Y21_2=Y2L(I)*Y2L(I)
       Y21Y31 = Y2L(I)*Y3L(I)
       X31_2=X3L(I)*X3L(I)
       X21_2=X2L(I)*X2L(I)
       X21X31=X3L(I)*X2L(I)
       X31Y21=X3L(I)*Y2L(I)
       X21Y31=X2L(I)*Y3L(I)
       X21Y21=X2L(I)*Y2L(I)
       X31Y31=X3L(I)*Y3L(I)
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ/A--
       BM0RZ(I,1,1) = ZERO
       BM0RZ(I,1,2) = Y21Y31
C       BM0RZ(I,1,3) = -BM0RZ(I,1,1)-BM0RZ(I,1,2)
C-------- NxI,x KSI -
       BMKRZ(I,1,2) = -Y21Y31
       BMKRZ(I,1,3) =  Y21_2
       BMKRZ(I,1,1) = -BMKRZ(I,1,2)-BMKRZ(I,1,3)
C-------- - NxI,x ETA
       BMERZ(I,1,2) = -Y31_2
       BMERZ(I,1,3) = -Y21Y31
       BMERZ(I,1,1) = -BMERZ(I,1,2)-BMERZ(I,1,3)
C -------NyI,y *A-------------
       BM0RZ(I,2,1) = ZERO
       BM0RZ(I,2,2) = X21X31
C       BM0RZ(I,2,3) = -BM0RZ(I,2,1)-BM0RZ(I,2,2)
C-------- NyI,y KSI -
       BMKRZ(I,2,2) =-X21X31
       BMKRZ(I,2,3) = X21_2
       BMKRZ(I,2,1) = -BMKRZ(I,2,2)-BMKRZ(I,2,3)
C-------- -NyI,y ETA +
       BMERZ(I,2,2) = -X31_2
       BMERZ(I,2,3) = X21X31
       BMERZ(I,2,1) = -BMERZ(I,2,2)-BMERZ(I,2,3)
C--------NxI,y *A
       BM0RZ(I,3,1) = X31Y21-X21Y31
       BM0RZ(I,3,2) = -X31Y21
C       BM0RZ(I,3,3) = -BM0RZ(I,3,1)-BM0RZ(I,3,2)
C-------- -NxI,y KSI +
       BMKRZ(I,3,2) = TWO*X31Y21-X21Y31
       BMKRZ(I,3,3) = -X21Y21
       BMKRZ(I,3,1) = -BMKRZ(I,3,2)-BMKRZ(I,3,3)
C-------- NxI,y ETA -
       BMERZ(I,3,2) = X31Y31
       BMERZ(I,3,3) = X31Y21-TWO*X21Y31
       BMERZ(I,3,1) = -BMERZ(I,3,2)-BMERZ(I,3,3)
C--------NyI,x *A
       BM0RZ(I,4,1) = X21Y31-X31Y21
       BM0RZ(I,4,2) = -X21Y31
C       BM0RZ(I,4,3) = -BM0RZ(I,4,1)-BM0RZ(I,4,2)
C-------- -NyI,x KSI
       BMKRZ(I,4,2) = TWO*X21Y31-X31Y21
       BMKRZ(I,4,3) = -X21Y21
       BMKRZ(I,4,1) = -BMKRZ(I,4,2)-BMKRZ(I,4,3)
C-------- NyI,x ETA -
       BMERZ(I,4,2) = X31Y31
       BMERZ(I,4,3) = X21Y31-TWO*X31Y21
       BMERZ(I,4,1) = -BMERZ(I,4,2)-BMERZ(I,4,3)
      ENDDO
C------------------ voir--
      DO I=JFT,JLT
C--------BM0RZ(I,3,1)->(NxI,y+NyI,x) *A-;BM0RZ(I,4,1)->(-NxI,y+NyI,x -2NI) *A-----
      DO J=1,2
       NXY=BM0RZ(I,3,J)
       NYX=BM0RZ(I,4,J)
       BMRZ(I,3,J)=NXY+NYX
       B0RZ(I,J)=-NXY+NYX
       NXY=BMKRZ(I,3,J)
       NYX=BMKRZ(I,4,J)
       BMKRZ(I,3,J)=NXY+NYX
       BKRZ(I,J)=-NXY+NYX
       NXY=BMERZ(I,3,J)
       NYX=BMERZ(I,4,J)
       BMERZ(I,3,J)=NXY+NYX
       BERZ(I,J)=-NXY+NYX
      ENDDO
C-------should do here as Ni has no more f(j=3)=-f(j=1)-f(j=2)
       B0RZ(I,3)=-B0RZ(I,1)-B0RZ(I,2)
C------add 2Ni------
       A2= TWO*AREA(I)
       B0RZ(I,1)=B0RZ(I,1)-A2
       BKRZ(I,1)=BKRZ(I,1)+A2
       BERZ(I,1)=BERZ(I,1)+A2
       BKRZ(I,2)=BKRZ(I,2)-A2
C       BMERZ1(I,3)=BMERZ1(I,3)-A2
      ENDDO
C-----------necessary for stabilization (explicit)
       CALL C3BM0RZ(JFT ,JLT  ,BMRZ,BM0RZ,BMKRZ ,BMERZ )
C
      RETURN
      END
Chd|====================================================================
Chd|  C3BM0RZ                       source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3DERIRZ                      source/elements/sh3n/coque3n/c3defo3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE C3BM0RZ(JFT ,JLT  ,BMRZ ,BM0RZ,BMKRZ ,BMERZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .   BMRZ(MVSIZ,3,2),BM0RZ(MVSIZ,4,2),BMKRZ(MVSIZ,4,3),
     .   BMERZ(MVSIZ,4,3)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,K
      my_real
     .   SCAL
C-------crrection to pass patch test	 
      BMRZ(JFT:JLT,1:3,1:2)=ZERO
      RETURN
C------- moyen ksi,eta=0.5 for BMRZ(,J,I)
c       SCAL=HALF
C------- moyen volume (analytical integration)
       SCAL=ONE_OVER_6
      DO J=1,2
      DO I=JFT,JLT
       BMRZ(I,1,J)=BM0RZ(I,1,J)+SCAL*(BMKRZ(I,1,J)+BMERZ(I,1,J))
       BMRZ(I,2,J)=BM0RZ(I,2,J)+SCAL*(BMKRZ(I,2,J)+BMERZ(I,2,J))
       BMRZ(I,3,J)=BM0RZ(I,3,J)+SCAL*(BMKRZ(I,3,J)+BMERZ(I,3,J))
      ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C3DEFT3                       source/elements/sh3n/coque3n/c3defo3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3DEFT3(JFT,JLT,X2,Y2,X3,Y3,V21X,V21Y,V31X,V31Y,
     2                   BM0RZ,RZ13,RZ23,AREA,VDEF,IDRIL )
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
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IDRIL
      my_real
     .  X2(*),Y2(*),X3(*),Y3(*),V21X(*),V21Y(*),V31X(*),V31Y(*),
     .  BM0RZ(MVSIZ,4,2),RZ13(*),RZ23(*),AREA(*),VDEF(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC1, NC2, NC3, I, J
C     REAL
      my_real
     .   PX2(MVSIZ), PY2(MVSIZ), PX3(MVSIZ), PY3(MVSIZ) ,AREAI
C-----------------------------------------------
C-------------anticipe for system change (later)
      DO I=JFT,JLT
       AREAI=HALF/MAX(EM20,AREA(I))
       PX2(I)=Y3(I)*AREAI
       PY2(I)=-X3(I)*AREAI
       PX3(I)=-Y2(I)*AREAI
       PY3(I)=X2(I)*AREAI
      ENDDO
      DO I=JFT,JLT
       VDEF(I,1)=PX2(I)*V21X(I)+PX3(I)*V31X(I)
       VDEF(I,2)=PY2(I)*V21Y(I)+PY3(I)*V31Y(I)
       VDEF(I,3)=PY2(I)*V21X(I)+PY3(I)*V31X(I)
       VDEF(I,4)=PX2(I)*V21Y(I)+PX3(I)*V31Y(I)
      END DO
C---------after the patch, as explicit, EMRZ=0
c      IF (IDRIL>0) THEN
c       DO I=JFT,JLT
c        VDEF(1,I)=VDEF(1,I)+BM0RZ(1,1,I)*RZ13(I)+BM0RZ(1,2,I)*RZ23(I)
c        VDEF(2,I)=VDEF(2,I)+BM0RZ(2,1,I)*RZ13(I)+BM0RZ(2,2,I)*RZ23(I)
c        VDEF(3,I)=VDEF(3,I)+BM0RZ(3,1,I)*RZ13(I)+BM0RZ(3,2,I)*RZ23(I)
c        VDEF(4,I)=VDEF(4,I)+BM0RZ(4,1,I)*RZ13(I)+BM0RZ(4,2,I)*RZ23(I)
c       ENDDO
c      END IF !(IDRIL>0) THEN
C	  
      RETURN
C
      END

