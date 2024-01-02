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
Chd|  SDLENSH                       source/elements/thickshell/solidec/sdlensh.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE SDLENSH(
     1   VOLN,    LLSH, AREA, X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      NEL)
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: AREA 
      my_real
     .   VOLN(*),LLSH(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   VQ(3,3,MVSIZ), LXYZ0(3),DETA1(MVSIZ),XX,YY,ZZ,
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),ZL1(MVSIZ),
     .   XN(4,MVSIZ) , YN(4,MVSIZ) , ZN(4,MVSIZ) 
      my_real
     .   AL1,AL2,LL(MVSIZ),COREL(2,4)
      my_real
     .   X13,X24,Y13,Y24,L13,L24,C1,C2,THKLY,POSLY,
     .   FAC,VISCE,RX1,RY1,SX1,SY1,S1,FAC1,FAC2,FACI,FAC11,FACDT
C=======================================================================
        DO I=1,NEL
          XN(1,I) = HALF*(X1(I)+X5(I))
          YN(1,I) = HALF*(Y1(I)+Y5(I))
          ZN(1,I) = HALF*(Z1(I)+Z5(I))
          XN(2,I) = HALF*(X2(I)+X6(I))
          YN(2,I) = HALF*(Y2(I)+Y6(I))
          ZN(2,I) = HALF*(Z2(I)+Z6(I))
          XN(3,I) = HALF*(X3(I)+X7(I))
          YN(3,I) = HALF*(Y3(I)+Y7(I))
          ZN(3,I) = HALF*(Z3(I)+Z7(I))
          XN(4,I) = HALF*(X4(I)+X8(I))
          YN(4,I) = HALF*(Y4(I)+Y8(I))
          ZN(4,I) = HALF*(Z4(I)+Z8(I))
        ENDDO 
C------g1,g2 :
        DO I=1,NEL
          RX(I)=XN(2,I)+XN(3,I)-XN(1,I)-XN(4,I)
          RY(I)=YN(2,I)+YN(3,I)-YN(1,I)-YN(4,I)
          RZ(I)=ZN(2,I)+ZN(3,I)-ZN(1,I)-ZN(4,I)
          SX(I)=XN(3,I)+XN(4,I)-XN(1,I)-XN(2,I)
          SY(I)=YN(3,I)+YN(4,I)-YN(1,I)-YN(2,I)
          SZ(I)=ZN(3,I)+ZN(4,I)-ZN(1,I)-ZN(2,I)
        ENDDO 
C------Local elem. base:
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,NEL)
C------ Global -> Local Coordinate  FOURTH=0.25 ;
        DO I=1,NEL
          LXYZ0(1)=FOURTH*(XN(1,I)+XN(2,I)+XN(3,I)+XN(4,I))
          LXYZ0(2)=FOURTH*(YN(1,I)+YN(2,I)+YN(3,I)+YN(4,I))
          LXYZ0(3)=FOURTH*(ZN(1,I)+ZN(2,I)+ZN(3,I)+ZN(4,I))
          XX=XN(2,I)-XN(1,I)
          YY=YN(2,I)-YN(1,I)
          ZZ=ZN(2,I)-ZN(1,I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          XX=XN(2,I)-LXYZ0(1)
          YY=YN(2,I)-LXYZ0(2)
          ZZ=ZN(2,I)-LXYZ0(3)
          ZL1(I)=VQ(1,3,I)*XX+VQ(2,3,I)*YY+VQ(3,3,I)*ZZ
C          
          XX=XN(3,I)-XN(1,I)
          YY=YN(3,I)-YN(1,I)
          ZZ=ZN(3,I)-ZN(1,I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C
          XX=XN(4,I)-XN(1,I)
          YY=YN(4,I)-YN(1,I)
          ZZ=ZN(4,I)-ZN(1,I)
          XL4(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL4(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=FOURTH*DETA1(I)
        ENDDO 
      FAC = TWO
      FACDT = FIVE_OVER_4
C-------same than QBAT       
      IF (IDT1SOL>0) FACDT =FOUR_OVER_3
C---- compute COREL(2,4) mean surface and area     
      DO I=1,NEL
        LXYZ0(1)=FOURTH*(XL2(I)+XL3(I)+XL4(I))
        LXYZ0(2)=FOURTH*(YL2(I)+YL3(I)+YL4(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=XL2(I)-LXYZ0(1)
        COREL(1,3)=XL3(I)-LXYZ0(1)
        COREL(1,4)=XL4(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=YL2(I)-LXYZ0(2)
        COREL(2,3)=YL3(I)-LXYZ0(2)
        COREL(2,4)=YL4(I)-LXYZ0(2)
        X13=(COREL(1,1)-COREL(1,3))*HALF
        X24=(COREL(1,2)-COREL(1,4))*HALF
        Y13=(COREL(2,1)-COREL(2,3))*HALF
        Y24=(COREL(2,2)-COREL(2,4))*HALF
C
        L13=X13*X13+Y13*Y13
        L24=X24*X24+Y24*Y24
        AL1=MAX(L13,L24)
        C1 =COREL(1,2)*COREL(2,4)-COREL(2,2)*COREL(1,4)
        C2 =COREL(1,1)*COREL(2,3)-COREL(2,1)*COREL(1,3)
        AL2 =MAX(ABS(C1),ABS(C2))/AREA(I)
        RX1=X24-X13
        RY1=Y24-Y13
        SX1=-X24-X13
        SY1=-Y24-Y13
        C1=SQRT(RX1*RX1+RY1*RY1)
        C2=SQRT(SX1*SX1+SY1*SY1)
        S1=FOURTH*(MAX(C1,C2)/MIN(C1,C2)-ONE)
        FAC1=MIN(HALF,S1)+ONE
        FAC2=AREA(I)/(C1*C2)
        FAC2=3.413*MAX(ZERO,FAC2-0.7071)
        FAC2=0.78+0.22*FAC2*FAC2*FAC2
        FACI=TWO*FAC1*FAC2
        S1 = SQRT(FACI*(FACDT+AL2)*AL1)
        S1 = MAX(S1,EM20)
        LLSH(I) = AREA(I)/S1
      ENDDO
C
      RETURN
      END
