Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i8cst3   ../engine/source/interfaces/inter3d/i8cst3.F
!||--- called by ------------------------------------------------------
!||    intvo8   ../engine/source/interfaces/inter3d/intvo8.F
!||====================================================================
      SUBROUTINE I8CST3(
     1   X1,      Y1,      Z1,      X2,
     2   Y2,      Z2,      X3,      Y3,
     3   Z3,      X4,      Y4,      Z4,
     4   XI,      YI,      ZI,      N1,
     5   N2,      N3,      ANS,     SSC,
     6   TTC,     XFACE,   X0,      Y0,
     7   Z0,      XX1,     YY1,     ZZ1,
     8   XX2,     YY2,     ZZ2,     XX3,
     9   YY3,     ZZ3,     XX4,     YY4,
     A   ZZ4,     XI1,     YI1,     ZI1,
     B   XI2,     YI2,     ZI2,     XI3,
     C   YI3,     ZI3,     XI4,     YI4,
     D   ZI4,     XN1,     YN1,     ZN1,
     E   XN2,     YN2,     ZN2,     XN3,
     F   YN3,     ZN3,     XN4,     YN4,
     G   ZN4,     AREA,    LFT,     LLT)


C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      my_real
     .   X1(*), X2(*), X3(*), X4(*),
     .     Y1(*), Y2(*), Y3(*), Y4(*),
     .     Z1(*), Z2(*), Z3(*), Z4(*),
     .     XI(*), YI(*), ZI(*), ANS(*),
     .     N1(*), N2(*), N3(*), SSC(*), TTC(*),
     .     X0(*), Y0(*), Z0(*), XFACE(*)
      my_real
     .     XX1(*), XX2(*), XX3(*), XX4(*),
     .     YY1(*), YY2(*), YY3(*), YY4(*),
     .     ZZ1(*), ZZ2(*), ZZ3(*), ZZ4(*),
     .     XI1(*), XI2(*), XI3(*), XI4(*),
     .     YI1(*), YI2(*), YI3(*), YI4(*),
     .     ZI1(*), ZI2(*), ZI3(*), ZI4(*),
     .     XN1(*), XN2(*), XN3(*), XN4(*),
     .     YN1(*), YN2(*), YN3(*), YN4(*),
     .     ZN1(*), ZN2(*), ZN3(*), ZN4(*),
     .     AREA(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real AN     
C=======================================================================
      DO 100 I=LFT,LLT
      X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
      Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
      Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
C
      XX1(I) = X1(I)-X0(I)
      XX2(I) = X2(I)-X0(I)
      XX3(I) = X3(I)-X0(I)
      XX4(I) = X4(I)-X0(I)
      YY1(I) = Y1(I)-Y0(I)
      YY2(I) = Y2(I)-Y0(I)
      YY3(I) = Y3(I)-Y0(I)
      YY4(I) = Y4(I)-Y0(I)
      ZZ1(I) = Z1(I)-Z0(I)
      ZZ2(I) = Z2(I)-Z0(I)
      ZZ3(I) = Z3(I)-Z0(I)
      ZZ4(I) = Z4(I)-Z0(I)
C
      XI1(I) = X1(I)-XI(I)
      XI2(I) = X2(I)-XI(I)
      XI3(I) = X3(I)-XI(I)
      XI4(I) = X4(I)-XI(I)
      YI1(I) = Y1(I)-YI(I)
      YI2(I) = Y2(I)-YI(I)
      YI3(I) = Y3(I)-YI(I)
      YI4(I) = Y4(I)-YI(I)
      ZI1(I) = Z1(I)-ZI(I)
      ZI2(I) = Z2(I)-ZI(I)
      ZI3(I) = Z3(I)-ZI(I)
      ZI4(I) = Z4(I)-ZI(I)
 100  CONTINUE
C
      DO 120 I=LFT,LLT
      XN1(I) = YY1(I)*ZZ2(I) - YY2(I)*ZZ1(I)
      YN1(I) = ZZ1(I)*XX2(I) - ZZ2(I)*XX1(I)
      ZN1(I) = XX1(I)*YY2(I) - XX2(I)*YY1(I)
      N1(I)=XN1(I)
      N2(I)=YN1(I)
      N3(I)=ZN1(I)
 120  CONTINUE
C
      DO 140 I=LFT,LLT
      XN2(I) = YY2(I)*ZZ3(I) - YY3(I)*ZZ2(I)
      YN2(I) = ZZ2(I)*XX3(I) - ZZ3(I)*XX2(I)
      ZN2(I) = XX2(I)*YY3(I) - XX3(I)*YY2(I)
      N1(I)=N1(I)+XN2(I)
      N2(I)=N2(I)+YN2(I)
      N3(I)=N3(I)+ZN2(I)
 140  CONTINUE
C
      DO 160 I=LFT,LLT
      XN3(I) = YY3(I)*ZZ4(I) - YY4(I)*ZZ3(I)
      YN3(I) = ZZ3(I)*XX4(I) - ZZ4(I)*XX3(I)
      ZN3(I) = XX3(I)*YY4(I) - XX4(I)*YY3(I)
      N1(I)=N1(I)+XN3(I)
      N2(I)=N2(I)+YN3(I)
      N3(I)=N3(I)+ZN3(I)
 160  CONTINUE
C
      DO 180 I=LFT,LLT
      XN4(I) = YY4(I)*ZZ1(I) - YY1(I)*ZZ4(I)
      YN4(I) = ZZ4(I)*XX1(I) - ZZ1(I)*XX4(I)
      ZN4(I) = XX4(I)*YY1(I) - XX1(I)*YY4(I)
      N1(I)=N1(I)+XN4(I)
      N2(I)=N2(I)+YN4(I)
      N3(I)=N3(I)+ZN4(I)
 180  CONTINUE
C
      DO 200 I=LFT,LLT
      AN= MAX(EM20,SQRT(N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)))
      N1(I)=N1(I)/AN
      N2(I)=N2(I)/AN
      N3(I)=N3(I)/AN
      AREA(I)=HALF*AN
 200  CONTINUE
C
      DO 210 I=LFT,LLT
      X0(I)=(N1(I)*XN1(I)+N2(I)*YN1(I)+N3(I)*ZN1(I))
      Y0(I)=(N1(I)*XN2(I)+N2(I)*YN2(I)+N3(I)*ZN2(I))
      Z0(I)=(N1(I)*XN3(I)+N2(I)*YN3(I)+N3(I)*ZN3(I))
      XX1(I)=(N1(I)*XN4(I)+N2(I)*YN4(I)+N3(I)*ZN4(I))
 210  CONTINUE
C
      DO 220 I=LFT,LLT
      XN1(I) = YI1(I)*ZI2(I) - YI2(I)*ZI1(I)
      YN1(I) = ZI1(I)*XI2(I) - ZI2(I)*XI1(I)
      ZN1(I) = XI1(I)*YI2(I) - XI2(I)*YI1(I)
      YY1(I)=(N1(I)*XN1(I)+N2(I)*YN1(I)+N3(I)*ZN1(I))
 220  CONTINUE
C
      DO 240 I=LFT,LLT
      XN2(I) = YI2(I)*ZI3(I) - YI3(I)*ZI2(I)
      YN2(I) = ZI2(I)*XI3(I) - ZI3(I)*XI2(I)
      ZN2(I) = XI2(I)*YI3(I) - XI3(I)*YI2(I)
      ZZ1(I)=(N1(I)*XN2(I)+N2(I)*YN2(I)+N3(I)*ZN2(I))
 240  CONTINUE
C
      DO 260 I=LFT,LLT
      XN3(I) = YI3(I)*ZI4(I) - YI4(I)*ZI3(I)
      YN3(I) = ZI3(I)*XI4(I) - ZI4(I)*XI3(I)
      ZN3(I) = XI3(I)*YI4(I) - XI4(I)*YI3(I)
      XX2(I)=(N1(I)*XN3(I)+N2(I)*YN3(I)+N3(I)*ZN3(I))
 260  CONTINUE
C
      DO 280 I=LFT,LLT
      XN4(I) = YI4(I)*ZI1(I) - YI1(I)*ZI4(I)
      YN4(I) = ZI4(I)*XI1(I) - ZI1(I)*XI4(I)
      ZN4(I) = XI4(I)*YI1(I) - XI1(I)*YI4(I)
      YY2(I)=(N1(I)*XN4(I)+N2(I)*YN4(I)+N3(I)*ZN4(I))
 280  CONTINUE
C
      DO 300 I=LFT,LLT
      ZZ2(I)=Y0(I)*YY2(I)
      XX3(I)=ZZ1(I)*XX1(I)
 300  CONTINUE
C
      DO 320 I=LFT,LLT
      IF(XFACE(I)==ZERO)GOTO 320
      IF(ZZ2(I)+XX3(I)/=ZERO)THEN
       SSC(I)=(ZZ2(I)-XX3(I))/(ZZ2(I)+XX3(I))
      ELSE
       SSC(I)=ZERO
      ENDIF
      IF(Z0(I)/=ZERO)THEN
       ZZ2(I)=YY1(I)*Z0(I)
       XX3(I)=XX2(I)*X0(I)
       IF(ZZ2(I)+XX3(I)/=ZERO)THEN
        TTC(I)=(ZZ2(I)-XX3(I))/(ZZ2(I)+XX3(I))
       ELSE
        TTC(I)=ZERO
       ENDIF
      ELSE
       TTC(I)=(YY1(I)-X0(I))/X0(I)
      ENDIF
 320  CONTINUE
C
      RETURN
      END
