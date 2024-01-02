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
Chd|  SDLEN8                        source/elements/solid/solidez/sdlen8.F
Chd|-- called by -----------
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen_dege.F
Chd|====================================================================
      SUBROUTINE SDLEN8(
     1   DELTAX,  VOLG,    IXS,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      NEL)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER  IXS(NIXS,*)
C     REAL
      my_real
     .   DELTAX(*),VOLG(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, IPT, IDEGE(MVSIZ)
C     REAL
      my_real
     .   AJ11, AJ12, AJ13, AJ21,
     .   AJ22, AJ23, AJ31, AJ32,
     .   AJ33, AI11, AI12, AI13,
     .   AI21, AI22, AI23, AI31,
     .   AI32, AI33
C     REAL
      my_real
     .   X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
     .   X78(MVSIZ), Y12(MVSIZ), Y34(MVSIZ), Y56(MVSIZ), Y78(MVSIZ),
     .   Z12(MVSIZ), Z34(MVSIZ), Z56(MVSIZ), Z78(MVSIZ), X14(MVSIZ),
     .   X23(MVSIZ), X58(MVSIZ), X67(MVSIZ), Y14(MVSIZ), Y23(MVSIZ),
     .   Y58(MVSIZ), Y67(MVSIZ), Z14(MVSIZ), Z23(MVSIZ), Z58(MVSIZ),
     .   Z67(MVSIZ), X15(MVSIZ), X26(MVSIZ), X37(MVSIZ), X48(MVSIZ),
     .   Y15(MVSIZ), Y26(MVSIZ), Y37(MVSIZ), Y48(MVSIZ), Z15(MVSIZ),
     .   Z26(MVSIZ), Z37(MVSIZ), Z48(MVSIZ), H(8), VLINV,
     .   DELTA1,DELTA2,DELTA3,
     .   SPX1,SPX2,SPX3,SPX4,SPX5,SPX6,SPX7,SPX8,
     .   SPY1,SPY2,SPY3,SPY4,SPY5,SPY6,SPY7,SPY8,
     .   SPZ1,SPZ2,SPZ3,SPZ4,SPZ5,SPZ6,SPZ7,SPZ8,
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,SMAX(MVSIZ),VMIN,
     .   P1(8), P2(8), P3(8),VLINC(MVSIZ,8)
C-----------------------------------------------
C      
      IF (IDTS6>0) THEN
       CALL SDLEN_DEGE(
     1   VOLG,    DELTAX,  X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      IXS,     IDEGE,
     8   NEL)
      ELSE
       IDEGE(1:NEL)=0
      END IF
C
      DO I=1,NEL
       X12(I)=X1(I)-X2(I)
       Y12(I)=Y1(I)-Y2(I)
       Z12(I)=Z1(I)-Z2(I)
       X34(I)=X3(I)-X4(I)
       Y34(I)=Y3(I)-Y4(I)
       Z34(I)=Z3(I)-Z4(I)
       X56(I)=X5(I)-X6(I)
       Y56(I)=Y5(I)-Y6(I)
       Z56(I)=Z5(I)-Z6(I)
       X78(I)=X7(I)-X8(I)
       Y78(I)=Y7(I)-Y8(I)
       Z78(I)=Z7(I)-Z8(I)
       X14(I)=X1(I)-X4(I)
       Y14(I)=Y1(I)-Y4(I)
       Z14(I)=Z1(I)-Z4(I)
       X23(I)=X2(I)-X3(I)
       Y23(I)=Y2(I)-Y3(I)
       Z23(I)=Z2(I)-Z3(I)
       X58(I)=X5(I)-X8(I)
       Y58(I)=Y5(I)-Y8(I)
       Z58(I)=Z5(I)-Z8(I)
       X67(I)=X6(I)-X7(I)
       Y67(I)=Y6(I)-Y7(I)
       Z67(I)=Z6(I)-Z7(I)
       X15(I)=X1(I)-X5(I)
       Y15(I)=Y1(I)-Y5(I)
       Z15(I)=Z1(I)-Z5(I)
       X26(I)=X2(I)-X6(I)
       Y26(I)=Y2(I)-Y6(I)
       Z26(I)=Z2(I)-Z6(I)
       X37(I)=X3(I)-X7(I)
       Y37(I)=Y3(I)-Y7(I)
       Z37(I)=Z3(I)-Z7(I)
       X48(I)=X4(I)-X8(I)
       Y48(I)=Y4(I)-Y8(I)
       Z48(I)=Z4(I)-Z8(I)
      END DO 
C      
      DO IPT=1,8
       CALL BASISF (H,P1,P2,P3,IPT)
C
       DO I=1,NEL
          AJ11=P1(1)*X12(I)+P1(3)*X34(I)+P1(5)*X56(I)+P1(7)*X78(I)
          AJ12=P1(1)*Y12(I)+P1(3)*Y34(I)+P1(5)*Y56(I)+P1(7)*Y78(I)
          AJ13=P1(1)*Z12(I)+P1(3)*Z34(I)+P1(5)*Z56(I)+P1(7)*Z78(I)
          AJ21=P2(1)*X14(I)+P2(2)*X23(I)+P2(5)*X58(I)+P2(6)*X67(I)
          AJ22=P2(1)*Y14(I)+P2(2)*Y23(I)+P2(5)*Y58(I)+P2(6)*Y67(I)
          AJ23=P2(1)*Z14(I)+P2(2)*Z23(I)+P2(5)*Z58(I)+P2(6)*Z67(I)
          AJ31=P3(1)*X15(I)+P3(2)*X26(I)+P3(3)*X37(I)+P3(4)*X48(I)
          AJ32=P3(1)*Y15(I)+P3(2)*Y26(I)+P3(3)*Y37(I)+P3(4)*Y48(I)
          AJ33=P3(1)*Z15(I)+P3(2)*Z26(I)+P3(3)*Z37(I)+P3(4)*Z48(I)

          AI11= AJ22*AJ33-AJ23*AJ32
          AI21=-AJ21*AJ33+AJ23*AJ31
          AI31= AJ21*AJ32-AJ22*AJ31
          VLINC(I,IPT)=AJ11*AI11+AJ12*AI21+AJ13*AI31
       ENDDO
      END DO ! IPT=1,8
C      
      DO I=1,NEL
c mediane * 4
        XX1 = X1(I) + X2(I) + X3(I) + X4(I)
     .      - X5(I) - X6(I) - X7(I) - X8(I)
        YY1 = Y1(I) + Y2(I) + Y3(I) + Y4(I)
     .      - Y5(I) - Y6(I) - Y7(I) - Y8(I)
        ZZ1 = Z1(I) + Z2(I) + Z3(I) + Z4(I)
     .      - Z5(I) - Z6(I) - Z7(I) - Z8(I)
        XX2 = X1(I) + X2(I) + X5(I) + X6(I)
     .      - X3(I) - X4(I) - X7(I) - X8(I)
        YY2 = Y1(I) + Y2(I) + Y5(I) + Y6(I)
     .      - Y3(I) - Y4(I) - Y7(I) - Y8(I)
        ZZ2 = Z1(I) + Z2(I) + Z5(I) + Z6(I)
     .      - Z3(I) - Z4(I) - Z7(I) - Z8(I)
        XX3 = X1(I) + X4(I) + X5(I) + X8(I)
     .      - X3(I) - X2(I) - X7(I) - X6(I)
        YY3 = Y1(I) + Y4(I) + Y5(I) + Y8(I)
     .      - Y3(I) - Y2(I) - Y7(I) - Y6(I)
        ZZ3 = Z1(I) + Z4(I) + Z5(I) + Z8(I)
     .      - Z3(I) - Z2(I) - Z7(I) - Z6(I)
C surface * 16
        SMAX(I) =          (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
        SMAX(I) = MAX(SMAX(I),(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
        SMAX(I) = MAX(SMAX(I),(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
      ENDDO
C volume / 8
      IF (IDTS6>0) THEN
                DO I=1,NEL
                        IF(IDEGE(I)==0)THEN
                                VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                                     VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
C
                                DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                        ENDIF
                ENDDO
      ELSE
                DO I=1,NEL
                        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
C
                        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                ENDDO
      ENDIF
                        
        
C-----------
      RETURN
      END
