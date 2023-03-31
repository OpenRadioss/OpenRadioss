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
Chd|  S6RCOORK                      source/elements/thickshell/solide6c/s6rcoork.F
Chd|-- called by -----------
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        S6CORTHO3                     source/elements/thickshell/solide6c/s6cortho3.F
Chd|        VRROTA3                       source/elements/thickshell/solide6c/vrrota3.F
Chd|====================================================================
      SUBROUTINE S6RCOORK(X,IXS,
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   OFFG,OFF,SAV,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K22,K23,
     .   K24,K25,K26,K33,K34,K35,K36,K44,
     .   K45,K46,K55,K56,K66,NEL)
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
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), 
     .   OFFG(*), OFF(*),  
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      DOUBLE PRECISION
     .  SAV(NEL,15)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K22(9,*)    ,K23(9,*)   ,K24(9,*)   ,K25(9,*)   ,
     .   K26(9,*)   ,K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,
     .   K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K55(9,*)   ,K56(9,*)    ,
     .   K66(9,*)   
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*), MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   XL,YL,ZL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C-----------------------------------------------
C
      DO I=1,NEL
      NGEO(I)=IXS(10,I)
      NGL(I)=IXS(11,I)
      MXT(I)=IXS(1,I)
      NC1(I)=IXS(2,I)
      NC2(I)=IXS(3,I)
      NC3(I)=IXS(4,I)
      NC4(I)=IXS(6,I)
      NC5(I)=IXS(7,I)
      NC6(I)=IXS(8,I)
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=1,NEL
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
      ENDDO
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=1,NEL
        XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
      ENDDO
       CALL S6CORTHO3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      Y1,      Y2,
     3   Y3,      Y4,      Y5,      Y6,
     4   Z1,      Z2,      Z3,      Z4,
     5   Z5,      Z6,      R11,     R12,
     6   R13,     R21,     R22,     R23,
     7   R31,     R32,     R33,     RX,
     8   RY,      RZ,      SX,      SY,
     9   SZ,      TX,      TY,      TZ,
     A   NEL)
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
      IF((ISMSTR==1.OR.ISMSTR==3).OR.
     .   (ISMSTR==2.AND.IDTMIN(1)==3))THEN
       DO I=1,NEL
       IF(OFFG(I)==TWO)THEN
        X1(I)=SAV(I,1)
        Y1(I)=SAV(I,2)
        Z1(I)=SAV(I,3)
        X2(I)=SAV(I,4)
        Y2(I)=SAV(I,5)
        Z2(I)=SAV(I,6)
        X3(I)=SAV(I,7)
        Y3(I)=SAV(I,8)
        Z3(I)=SAV(I,9)
        X4(I)=SAV(I,10)
        Y4(I)=SAV(I,11)
        Z4(I)=SAV(I,12)
        X5(I)=SAV(I,13)
        Y5(I)=SAV(I,14)
        Z5(I)=SAV(I,15)
        X6(I)=ZERO
        Y6(I)=ZERO
        Z6(I)=ZERO
        OFF(I) = ONE
        XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
       ELSE
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
C       SAV dans rep. local.
C       SAV(I,1)=X1(I)-X6(I)
C       SAV(I,2)=Y1(I)-Y6(I)
C       SAV(I,3)=Z1(I)-Z6(I)
C       SAV(I,4)=X2(I)-X6(I)
C       SAV(I,5)=Y2(I)-Y6(I)
C       SAV(I,6)=Z2(I)-Z6(I)
C       SAV(I,7)=X3(I)-X6(I)
C       SAV(I,8)=Y3(I)-Y6(I)
C       SAV(I,9)=Z3(I)-Z6(I)
C       SAV(I,10)=X4(I)-X6(I)
C       SAV(I,11)=Y4(I)-Y6(I)
C       SAV(I,12)=Z4(I)-Z6(I)
C       SAV(I,13)=X5(I)-X6(I)
C       SAV(I,14)=Y5(I)-Y6(I)
C       SAV(I,15)=Z5(I)-Z6(I)
        OFF(I) = OFFG(I)
       ENDIF
       ENDDO
C
      ELSE
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X1,      Y1,      Z1,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X2,      Y2,      Z2,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X3,      Y3,      Z3,
     4   NEL)
       DO I=1,NEL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
        OFF(I) = MIN(ONE,OFFG(I))
       ENDDO
C
       ENDIF
C-----------
      DO J=1,9      
      DO I=1,NEL
        K11(J,I)=ZERO
        K12(J,I)=ZERO
        K13(J,I)=ZERO
        K14(J,I)=ZERO
        K15(J,I)=ZERO
        K16(J,I)=ZERO
        K22(J,I)=ZERO
        K23(J,I)=ZERO
        K24(J,I)=ZERO
        K25(J,I)=ZERO
        K26(J,I)=ZERO
        K33(J,I)=ZERO
        K34(J,I)=ZERO
        K35(J,I)=ZERO
        K36(J,I)=ZERO
        K44(J,I)=ZERO
        K45(J,I)=ZERO
        K46(J,I)=ZERO
        K55(J,I)=ZERO
        K56(J,I)=ZERO
        K66(J,I)=ZERO
      ENDDO
      ENDDO
C-----------
      RETURN
      END
