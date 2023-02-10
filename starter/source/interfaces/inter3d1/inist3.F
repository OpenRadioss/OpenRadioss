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
Chd|  INIST3                        source/interfaces/inter3d1/inist3.F
Chd|-- called by -----------
Chd|        I12TID3                       source/interfaces/inter3d1/i12tid3.F
Chd|        I1TID3                        source/interfaces/inter3d1/i1tid3.F
Chd|        I3PEN3                        source/interfaces/inter3d1/i3pen3.F
Chd|        I5PWR3                        source/interfaces/inter3d1/i3pen3.F
Chd|        I6PEN3                        source/interfaces/inter3d1/i6pen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIST3(N1,N2,N3,SSC,TTC,IER,ALP,XX1,XX2,XX3,XS1,YS1,ZS1,XC,YC,ZC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT)  :: IER
      my_real, INTENT(INOUT) :: N1, N2, N3, SSC, TTC
      my_real, INTENT(IN)    :: ALP
      my_real,INTENT(INOUT)  :: XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   H(4), X0, Y0, Z0, XL1, XL2, XL3, XL4, YY1, YY2, YY3, YY4,
     .   ZZ1, ZZ2, ZZ3, ZZ4, XI1, XI2, XI3, XI4, YI1, YI2, YI3, YI4,
     .   ZI1, ZI2, ZI3, ZI4, XN1, YN1, ZN1, XN2, YN2, ZN2, XN3, YN3,
     .   ZN3, XN4, YN4, ZN4, AN, AREA, A12, A23, A34, A41, B12, B23,
     .   B34, B41, AB1, AB2, TP, TM, SP, SM, X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XI,YI,ZI
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !SECONDARY node coordinates
      XI = XS1
      YI = YS1
      ZI = ZS1
      
      !MAIN segment coordinates
      X1 = XX1(1)
      X2 = XX1(2)
      X3 = XX1(3)
      X4 = XX1(4)
      Y1 = XX2(1)
      Y2 = XX2(2)
      Y3 = XX2(3)
      Y4 = XX2(4)
      Z1 = XX3(1)
      Z2 = XX3(2)
      Z3 = XX3(3)
      Z4 = XX3(4)
      !MAIN segment centroid
      X0 = FOURTH*(X1+X2+X3+X4)
      Y0 = FOURTH*(Y1+Y2+Y3+Y4)
      Z0 = FOURTH*(Z1+Z2+Z3+Z4)
      !length from centroid
      XL1 = X1-X0
      XL2 = X2-X0
      XL3 = X3-X0
      XL4 = X4-X0      
      YY1 = Y1-Y0
      YY2 = Y2-Y0
      YY3 = Y3-Y0
      YY4 = Y4-Y0
      ZZ1 = Z1-Z0
      ZZ2 = Z2-Z0
      ZZ3 = Z3-Z0
      ZZ4 = Z4-Z0
      !length from SECONDARY node
      XI1 = X1-XI
      XI2 = X2-XI
      XI3 = X3-XI
      XI4 = X4-XI
      YI1 = Y1-YI
      YI2 = Y2-YI
      YI3 = Y3-YI
      YI4 = Y4-YI
      ZI1 = Z1-ZI
      ZI2 = Z2-ZI
      ZI3 = Z3-ZI
      ZI4 = Z4-ZI
      
      !1st mater triangle
      XN1 = YY1*ZZ2 - YY2*ZZ1
      YN1 = ZZ1*XL2 - ZZ2*XL1
      ZN1 = XL1*YY2 - XL2*YY1
      N1  = XN1
      N2  = YN1
      N3  = ZN1
      !2nd MAIN triangle
      XN2 = YY2*ZZ3 - YY3*ZZ2
      YN2 = ZZ2*XL3 - ZZ3*XL2
      ZN2 = XL2*YY3 - XL3*YY2
      N1  = N1+XN2
      N2  = N2+YN2
      N3  = N3+ZN2
      !3rd MAIN triangle
      XN3 = YY3*ZZ4 - YY4*ZZ3
      YN3 = ZZ3*XL4 - ZZ4*XL3
      ZN3 = XL3*YY4 - XL4*YY3
      N1  = N1+XN3
      N2  = N2+YN3
      N3  = N3+ZN3
      !4th MAIN triangle
      XN4 = YY4*ZZ1 - YY1*ZZ4
      YN4 = ZZ4*XL1 - ZZ1*XL4
      ZN4 = XL4*YY1 - XL1*YY4
      N1  = N1+XN4
      N2  = N2+YN4
      N3  = N3+ZN4
      !average normal of MAIN segment
      AN  = MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1  = N1/AN
      N2  = N2/AN
      N3  = N3/AN
      !in case of degenerated segment
      IF(AN<=EM19) THEN
        IER=-1
        RETURN
      ENDIF
      AREA=HALF*AN
      A12 = (N1*XN1+N2*YN1+N3*ZN1)
      A23 = (N1*XN2+N2*YN2+N3*ZN2)
      A34 = (N1*XN3+N2*YN3+N3*ZN3)
      A41 = (N1*XN4+N2*YN4+N3*ZN4)

      !1st triangle based on SECONDARY node
      XN1 = YI1*ZI2 - YI2*ZI1
      YN1 = ZI1*XI2 - ZI2*XI1
      ZN1 = XI1*YI2 - XI2*YI1
      B12 = (N1*XN1+N2*YN1+N3*ZN1)
      !2nd triangle based on SECONDARY node
      XN2 = YI2*ZI3 - YI3*ZI2
      YN2 = ZI2*XI3 - ZI3*XI2
      ZN2 = XI2*YI3 - XI3*YI2
      B23 = (N1*XN2+N2*YN2+N3*ZN2)
      !3rd triangle based on SECONDARY node
      XN3 = YI3*ZI4 - YI4*ZI3
      YN3 = ZI3*XI4 - ZI4*XI3
      ZN3 = XI3*YI4 - XI4*YI3
      B34 = (N1*XN3+N2*YN3+N3*ZN3)
      !4th triangle based on SECONDARY node
      XN4 = YI4*ZI1 - YI1*ZI4
      YN4 = ZI4*XI1 - ZI1*XI4
      ZN4 = XI4*YI1 - XI1*YI4
      B41 = (N1*XN4+N2*YN4+N3*ZN4)
      !SSC
      AB1 = A23*B41
      AB2 = B23*A41
      IF(ABS(AB1+AB2)/AREA>EM10)THEN
       SSC = (AB1-AB2)/(AB1+AB2)
      ELSE
       SSC = ZERO
      ENDIF
      !TTC
      IF(ABS(A34/AREA)>EM10)THEN
       AB1 = B12*A34
       AB2 = B34*A12
       TTC = (AB1-AB2)/(AB1+AB2)
      ELSE
       TTC = (B12-A12)/A12
       IF(B23<=ZERO .AND. B41<=ZERO)THEN
         IF(-B23/A12<=ALP.AND.-B41/A12<=ALP)SSC=ZERO
       ELSEIF(B23<=ZERO)THEN
         IF(-B23/A12<=ALP)SSC=ONE
       ELSEIF(B41<=ZERO)THEN
         IF(-B41/A12<=ALP)SSC=-ONE
       ENDIF
      ENDIF
      !lecipari : GAP=TWO*EM02 GAP->FRIGAP(2)->ALP (hardcoded tolerance)

      IF(ABS(SSC)>ONE+ALP.OR.ABS(TTC)>ONE+ALP) THEN
        IER = 1  !SECONDARY node not on MAIN segment
      ELSE
        IER = 0
        IF(ABS(SSC)>ONE)SSC=SSC/ABS(SSC)
        IF(ABS(TTC)>ONE)TTC=TTC/ABS(TTC)
      ENDIF

      TP   = FOURTH*(ONE+TTC)
      TM   = FOURTH*(ONE-TTC)
      SP   = ONE+SSC
      SM   = ONE-SSC
      H(1) = TM*SM
      H(2) = TM*SP
      H(3) = TP*SP
      H(4) = TP*SM
      XC   = H(1)*X1+H(2)*X2+H(3)*X3+H(4)*X4
      YC   = H(1)*Y1+H(2)*Y2+H(3)*Y3+H(4)*Y4
      ZC   = H(1)*Z1+H(2)*Z2+H(3)*Z3+H(4)*Z4

      RETURN
      END
