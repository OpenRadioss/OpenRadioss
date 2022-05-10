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
Chd|  GJNT_DIFF                     source/tools/lagmul/gjnt_diff.F
Chd|-- called by -----------
Chd|        LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GJNT_DIFF(SK     ,L1     ,L2     ,L3     ,ALPHA  ,
     2                     IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                     X      ,N0     ,N1     ,N2     ,N3     ,
     4                     NC     )  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N0,N1,N2,N3,NC, LLL(*),JLL(*),SLL(*),IADLL(*)
      my_real 
     .        X(3,*),XLL(*),SK(9),L1(3),L2(3),L3(3),ALPHA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,JJ,IK,IC,IAD,INOD(4)
      my_real 
     .        UX(3),UY(3),UZ(3),VX(3),VY(3),VZ(3),WX(3),WY(3),WZ(3),
     .        RX(3),RY(3),RZ(3),
     .        X0,Y0,Z0,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,XS,YS,ZS,
     .        X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .        XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .        LX,LY,LZ,NORM,UNT,DEUT,B1,B2,B3,C1,C2,C3,DET
C-----------------------------------------------
C        NC : nombre de condition cinematique 
C        IC : numero de la condition cinematique (1,NC)
C        IK : 
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3,4,5,6
C------
C        IADLL(NC)        : IAD = IADLL(IC)
C        IK = IAD,IAD+1,IAD+2,...
C        LLL(LAG_NKF)  : I = LLL(IK)
C        JLL(LAG_NKF)  : J = JLL(IK)
C======================================================================|
      UNT = THIRD
      DEUT= DTHIRD
C---  Node 1 skew
      UX(1) = SK(1)*L1(1)+SK(4)*L1(2)+SK(7)*L1(3)
      UY(1) = SK(2)*L1(1)+SK(5)*L1(2)+SK(8)*L1(3)
      UZ(1) = SK(3)*L1(1)+SK(6)*L1(2)+SK(9)*L1(3)
      NORM = UN/SQRT(UX(1)*UX(1)+UY(1)*UY(1)+UZ(1)*UZ(1))
      UX(1) = UX(1)*NORM
      UY(1) = UY(1)*NORM
      UZ(1) = UZ(1)*NORM
      IF (ABS(UX(1)).GT.ZEP99) THEN
        WX(1) =-UZ(1)
        WY(1) = ZERO
        WZ(1) = UX(1)
      ELSE
        WX(1) = ZERO
        WY(1) =-UZ(1)
        WZ(1) = UY(1)
      ENDIF
      NORM = UN/SQRT(WX(1)*WX(1)+WY(1)*WY(1)+WZ(1)*WZ(1))
      WX(1) = WX(1)*NORM
      WY(1) = WY(1)*NORM
      WZ(1) = WZ(1)*NORM
      VX(1) = WY(1)*UZ(1)-WZ(1)*UY(1)
      VY(1) = WZ(1)*UX(1)-WX(1)*UZ(1)
      VZ(1) = WX(1)*UY(1)-WY(1)*UX(1)
C---  Node 2 skew
      UX(2) = SK(1)*L2(1)+SK(4)*L2(2)+SK(7)*L2(3)
      UY(2) = SK(2)*L2(1)+SK(5)*L2(2)+SK(8)*L2(3)
      UZ(2) = SK(3)*L2(1)+SK(6)*L2(2)+SK(9)*L2(3)
      NORM = UN/SQRT(UX(2)*UX(2)+UY(2)*UY(2)+UZ(2)*UZ(2))
      UX(2) = UX(2)*NORM
      UY(2) = UY(2)*NORM
      UZ(2) = UZ(2)*NORM
      IF (ABS(UX(2)).GT.ZEP99) THEN
        WX(2) =-UZ(2)
        WY(2) = ZERO
        WZ(2) = UX(2)
      ELSE
        WX(2) = ZERO
        WY(2) =-UZ(2)
        WZ(2) = UY(2)
      ENDIF
      NORM = UN/SQRT(WX(1)*WX(1)+WY(1)*WY(1)+WZ(1)*WZ(1))
      WX(1) = WX(1)*NORM
      WY(1) = WY(1)*NORM
      WZ(1) = WZ(1)*NORM
      VX(2) = WY(2)*UZ(2)-WZ(2)*UY(2)
      VY(2) = WZ(2)*UX(2)-WX(2)*UZ(2)
      VZ(2) = WX(2)*UY(2)-WY(2)*UX(2)
C---  Node 3 skew
      UX(3) = SK(1)*L3(1)+SK(4)*L3(2)+SK(7)*L3(3)
      UY(3) = SK(2)*L3(1)+SK(5)*L3(2)+SK(8)*L3(3)
      UZ(3) = SK(3)*L3(1)+SK(6)*L3(2)+SK(9)*L3(3)
      NORM = UN/SQRT(UX(3)*UX(3)+UY(3)*UY(3)+UZ(3)*UZ(3))
      UX(3) = UX(3)*NORM
      UY(3) = UY(3)*NORM
      UZ(3) = UZ(3)*NORM
      IF (ABS(UX(3)).GT.ZEP99) THEN
        WX(3) =-UZ(3)
        WY(3) = ZERO
        WZ(3) = UX(3)
      ELSE
        WX(3) = ZERO
        WY(3) =-UZ(3)
        WZ(3) = UY(3)
      ENDIF
      NORM = UN/SQRT(WX(1)*WX(1)+WY(1)*WY(1)+WZ(1)*WZ(1))
      WX(1) = WX(1)*NORM
      WY(1) = WY(1)*NORM
      WZ(1) = WZ(1)*NORM
      VX(3) = WY(3)*UZ(3)-WZ(3)*UY(3)
      VY(3) = WZ(3)*UX(3)-WX(3)*UZ(3)
      VZ(3) = WX(3)*UY(3)-WY(3)*UX(3)
C---------------------------
      X1=X(1,N1)
      Y1=X(2,N1)
      Z1=X(3,N1)
      X2=X(1,N2)
      Y2=X(2,N2)
      Z2=X(3,N2)
      X3=X(1,N3)
      Y3=X(2,N3)
      Z3=X(3,N3)
      X0=(X1+X2+X3)*THIRD
      Y0=(Y1+Y2+Y3)*THIRD
      Z0=(Z1+Z2+Z3)*THIRD
C---------------------------
      INOD(1) = N1
      INOD(2) = N2
      INOD(3) = N3
      INOD(4) = N0
C---------------------------
      X1=X1-X0
      Y1=Y1-Y0
      Z1=Z1-Z0
      X2=X2-X0
      Y2=Y2-Y0
      Z2=Z2-Z0
      X3=X3-X0
      Y3=Y3-Y0
      Z3=Z3-Z0
      XS=X(1,N0)-X0
      YS=X(2,N0)-Y0
      ZS=X(3,N0)-Z0
      RX(1) = X1
      RX(2) = X2
      RX(3) = X3
      RY(1) = Y1
      RY(2) = Y2
      RY(3) = Y3
      RZ(1) = Z1
      RZ(2) = Z2
      RZ(3) = Z3
C---------------------------
      X12=X1*X1
      X22=X2*X2
      X32=X3*X3
      Y12=Y1*Y1
      Y22=Y2*Y2
      Y32=Y3*Y3
      Z12=Z1*Z1 
      Z22=Z2*Z2
      Z32=Z3*Z3 
      XX=X12 + X22 + X32
      YY=Y12 + Y22 + Y32
      ZZ=Z12 + Z22 + Z32
      XY=X1*Y1 + X2*Y2 + X3*Y3 
      YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 
      ZX=Z1*X1 + Z2*X2 + Z3*X3
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET=XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - 2.*XY*YZ*ZX
      DET=UN/DET
      B1=ZZZ*YYY-YZ2
      B2=XXX*ZZZ-ZX2
      B3=YYY*XXX-XY2
      C3=ZZZ*XY+YZ*ZX
      C1=XXX*YZ+ZX*XY
      C2=YYY*ZX+XY*YZ
C---------------------------  
C --- (vx)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = QUART
     .          + DET*ZS*(B2*RZ(JJ) - C1*RY(JJ))
     .          - DET*YS*(C1*RZ(JJ) - B3*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = DET*ZS*(C1*RX(JJ) - C3*RZ(JJ))
     .          - DET*YS*(B3*RX(JJ) - C2*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = DET*ZS*(C3*RY(JJ) - B2*RX(JJ))
     .          - DET*YS*(C2*RY(JJ) - C1*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) = -1.0
C --- (vy)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = DET*XS*(C1*RZ(JJ) - B3*RY(JJ))
     .          - DET*ZS*(C3*RZ(JJ) - C2*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = QUART
     .          + DET*XS*(B3*RX(JJ) - C2*RZ(JJ))
     .          - DET*ZS*(C2*RX(JJ) - B1*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = DET*XS*(C2*RY(JJ) - C1*RX(JJ))
     .          - DET*ZS*(B1*RY(JJ) - C3*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) = -UN
C --- (vz)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = DET*YS*(C3*RZ(JJ) - C2*RY(JJ))
     .          - DET*XS*(B2*RZ(JJ) - C1*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = DET*YS*(C2*RX(JJ) - B1*RZ(JJ))
     .          - DET*XS*(C1*RX(JJ) - C3*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = QUART
     .          + DET*YS*(B1*RY(JJ) - C3*RX(JJ))
     .          - DET*XS*(C3*RY(JJ) - B2*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 3
      SLL(IK) = 0
2      XLL(IK) = -UN
C
c      rotational dof of slave
C ---  (wx)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = DET*(C3*RZ(JJ) - C2*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = DET*(C2*RX(JJ) - B1*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = DET*(B1*RY(JJ) - C3*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = -1.0
C --- (wy)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = DET*(B2*RZ(JJ) - C1*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = DET*(C1*RX(JJ) - C3*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = DET*(C3*RY(JJ) - B2*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = -1.0
C --- (wz)
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 10
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
        SLL(IK) = 0
        XLL(IK) = DET*(C1*RZ(JJ) - B3*RY(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
        SLL(IK) = 0
        XLL(IK) = DET*(B3*RX(JJ) - C2*RZ(JJ))
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
        SLL(IK) = 0
        XLL(IK) = DET*(C2*RY(JJ) - C1*RX(JJ))
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = -UN
C
C --- Constraints
C     X local
C
      INOD(4) = N0
C
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 12
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = ALPHA*UX(1)
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = ALPHA*UY(1)
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = ALPHA*UZ(1)
      DO I=2,3
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        SLL(IK) = 0
        XLL(IK) = UX(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        SLL(IK) = 0
        XLL(IK) = UY(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
        SLL(IK) = 0
        XLL(IK) = UZ(I)
      ENDDO
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) =-ALPHA*UX(1)-UX(2)-UX(3)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) =-ALPHA*UY(1)-UY(2)-UY(3)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) =-ALPHA*UZ(1)-UZ(2)-UZ(3)
C
C     Y local
      DO I=1,3
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + 6
        IK = IADLL(NC)
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        SLL(IK) = 0
        XLL(IK) = VX(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        SLL(IK) = 0
        XLL(IK) = VY(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
        SLL(IK) = 0
        XLL(IK) = VZ(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 4
        SLL(IK) = 0
        XLL(IK) =-VX(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 5
        SLL(IK) = 0
        XLL(IK) =-VY(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 6
        SLL(IK) = 0
        XLL(IK) =-VZ(I)
      ENDDO
C
C     Z local
      DO I=1,3
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + 6
        IK = IADLL(NC)
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        SLL(IK) = 0
        XLL(IK) = WX(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        SLL(IK) = 0
        XLL(IK) = WY(I)
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
        SLL(IK) = 0
        XLL(IK) = WZ(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 4
        SLL(IK) = 0
        XLL(IK) =-WX(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 5
        SLL(IK) = 0
        XLL(IK) =-WY(I)
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 6
        SLL(IK) = 0
        XLL(IK) =-WZ(I)
      ENDDO
C---
      RETURN
      END
