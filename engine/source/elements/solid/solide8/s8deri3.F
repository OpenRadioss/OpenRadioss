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
Chd|  S8DERI3                       source/elements/solid/solide8/s8deri3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8DERI3(
     1   X,       Y,       Z,       PX1,
     2   PX2,     PX3,     PX4,     PX5,
     3   PX6,     PX7,     PX8,     PY1,
     4   PY2,     PY3,     PY4,     PY5,
     5   PY6,     PY7,     PY8,     PZ1,
     6   PZ2,     PZ3,     PZ4,     PZ5,
     7   PZ6,     PZ7,     PZ8,     VLINC,
     8   VOL,     DELTAX,  NGL,     OFF,
     9   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   X(MVSIZ,8), Y(MVSIZ,8), Z(MVSIZ,8),
     .   PX1(MVSIZ,8),PX2(MVSIZ,8),PX3(MVSIZ,8),PX4(MVSIZ,8),      
     .   PX5(MVSIZ,8),PX6(MVSIZ,8),PX7(MVSIZ,8),PX8(MVSIZ,8),      
     .   PY1(MVSIZ,8),PY2(MVSIZ,8),PY3(MVSIZ,8),PY4(MVSIZ,8),      
     .   PY5(MVSIZ,8),PY6(MVSIZ,8),PY7(MVSIZ,8),PY8(MVSIZ,8),      
     .   PZ1(MVSIZ,8),PZ2(MVSIZ,8),PZ3(MVSIZ,8),PZ4(MVSIZ,8),      
     .   PZ5(MVSIZ,8),PZ6(MVSIZ,8),PZ7(MVSIZ,8),PZ8(MVSIZ,8),
     .   VLINC(MVSIZ,8),VOL(MVSIZ),DELTAX(MVSIZ) ,OFF(MVSIZ)     
      INTEGER  NGL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, IPT
C     REAL
      my_real
     .   AJ11(MVSIZ), AJ12(MVSIZ), AJ13(MVSIZ), AJ21(MVSIZ),
     .   AJ22(MVSIZ), AJ23(MVSIZ), AJ31(MVSIZ), AJ32(MVSIZ),
     .   AJ33(MVSIZ), AI11(MVSIZ), AI12, AI13,
     .   AI21(MVSIZ), AI22, AI23, AI31(MVSIZ),
     .   AI32, AI33, 
     .   A1111, A1113,
     .   A1115, A1117, A2111, A2113,
     .   A2115, A2117, A3111, A3113,
     .   A3115, A3117, A1221, A1222,
     .   A1225, A1226, A2221, A2222,
     .   A2225, A2226, A3221, A3222 
C     REAL
      my_real
     .   A3225, A3226, A1331, A1332,
     .   A1333, A1334, A2331, A2332,
     .   A2333, A2334, A3331, A3332,
     .   A3333, A3334, X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
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
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,SMAX,VMIN,
     .   P1(8), P2(8), P3(8)
      DO 5 I=1,NEL
      X12(I)=X(I,1)-X(I,2)
      Y12(I)=Y(I,1)-Y(I,2)
      Z12(I)=Z(I,1)-Z(I,2)
      X34(I)=X(I,3)-X(I,4)
      Y34(I)=Y(I,3)-Y(I,4)
      Z34(I)=Z(I,3)-Z(I,4)
      X56(I)=X(I,5)-X(I,6)
      Y56(I)=Y(I,5)-Y(I,6)
      Z56(I)=Z(I,5)-Z(I,6)
      X78(I)=X(I,7)-X(I,8)
      Y78(I)=Y(I,7)-Y(I,8)
      Z78(I)=Z(I,7)-Z(I,8)
      X14(I)=X(I,1)-X(I,4)
      Y14(I)=Y(I,1)-Y(I,4)
      Z14(I)=Z(I,1)-Z(I,4)
      X23(I)=X(I,2)-X(I,3)
      Y23(I)=Y(I,2)-Y(I,3)
      Z23(I)=Z(I,2)-Z(I,3)
      X58(I)=X(I,5)-X(I,8)
      Y58(I)=Y(I,5)-Y(I,8)
      Z58(I)=Z(I,5)-Z(I,8)
      X67(I)=X(I,6)-X(I,7)
      Y67(I)=Y(I,6)-Y(I,7)
      Z67(I)=Z(I,6)-Z(I,7)
      X15(I)=X(I,1)-X(I,5)
      Y15(I)=Y(I,1)-Y(I,5)
      Z15(I)=Z(I,1)-Z(I,5)
      X26(I)=X(I,2)-X(I,6)
      Y26(I)=Y(I,2)-Y(I,6)
      Z26(I)=Z(I,2)-Z(I,6)
      X37(I)=X(I,3)-X(I,7)
      Y37(I)=Y(I,3)-Y(I,7)
      Z37(I)=Z(I,3)-Z(I,7)
      X48(I)=X(I,4)-X(I,8)
      Y48(I)=Y(I,4)-Y(I,8)
      Z48(I)=Z(I,4)-Z(I,8)
C
      VOL(I)=0.
 5    CONTINUE
C
      DO 100 IPT=1,8
C
C     VALEUR DES FONCTIONS DE FORMES AU POINT D 'INTEGRATION
C
      CALL BASISF (H,P1,P2,P3,IPT)
C
      DO I=1,NEL
        AJ11(I)=P1(1)*X12(I)+P1(3)*X34(I)+P1(5)*X56(I)+P1(7)*X78(I)
        AJ12(I)=P1(1)*Y12(I)+P1(3)*Y34(I)+P1(5)*Y56(I)+P1(7)*Y78(I)
        AJ13(I)=P1(1)*Z12(I)+P1(3)*Z34(I)+P1(5)*Z56(I)+P1(7)*Z78(I)
        AJ21(I)=P2(1)*X14(I)+P2(2)*X23(I)+P2(5)*X58(I)+P2(6)*X67(I)
        AJ22(I)=P2(1)*Y14(I)+P2(2)*Y23(I)+P2(5)*Y58(I)+P2(6)*Y67(I)
        AJ23(I)=P2(1)*Z14(I)+P2(2)*Z23(I)+P2(5)*Z58(I)+P2(6)*Z67(I)
        AJ31(I)=P3(1)*X15(I)+P3(2)*X26(I)+P3(3)*X37(I)+P3(4)*X48(I)
        AJ32(I)=P3(1)*Y15(I)+P3(2)*Y26(I)+P3(3)*Y37(I)+P3(4)*Y48(I)
        AJ33(I)=P3(1)*Z15(I)+P3(2)*Z26(I)+P3(3)*Z37(I)+P3(4)*Z48(I)
      ENDDO
C
      DO 20 I=1,NEL
      AI11(I)= AJ22(I)*AJ33(I)-AJ23(I)*AJ32(I)
      AI21(I)=-AJ21(I)*AJ33(I)+AJ23(I)*AJ31(I)
      AI31(I)= AJ21(I)*AJ32(I)-AJ22(I)*AJ31(I)
      VLINC(I,IPT)=AJ11(I)*AI11(I)+AJ12(I)*AI21(I)+AJ13(I)*AI31(I)
   20 CONTINUE
C
      DO 130 I=1,NEL
C  053 +1
      IF(OFF(I)==ZERO)VLINC(I,IPT)=ONE
      VOL(I)=VOL(I)+VLINC(I,IPT)
      IF(VLINC(I,IPT)>ZERO) GO TO 130
        CALL ANCMSG(MSGID=172,ANMODE=ANINFO,
     .              I1=NGL(I),I2=IPT)
      CALL ARRET(2)
  130 CONTINUE
C
      DO I=1,NEL
        VLINV=ONE/VLINC(I,IPT)
        AI11(I)=VLINV*AI11(I)
        AI21(I)=VLINV*AI21(I)
        AI31(I)=VLINV*AI31(I)
        AI12=VLINV*(-AJ12(I)*AJ33(I)+AJ13(I)*AJ32(I))
        AI22=VLINV*( AJ11(I)*AJ33(I)-AJ13(I)*AJ31(I))
        AI32=VLINV*(-AJ11(I)*AJ32(I)+AJ12(I)*AJ31(I))
        AI13=VLINV*( AJ12(I)*AJ23(I)-AJ13(I)*AJ22(I))
        AI23=VLINV*(-AJ11(I)*AJ23(I)+AJ13(I)*AJ21(I))
        AI33=VLINV*( AJ11(I)*AJ22(I)-AJ12(I)*AJ21(I))
C
        A1111=AI11(I)*P1(1)
        A1113=AI11(I)*P1(3)
        A1115=AI11(I)*P1(5)
        A1117=AI11(I)*P1(7)
        A2111=AI21(I)*P1(1)
        A2113=AI21(I)*P1(3)
        A2115=AI21(I)*P1(5)
        A2117=AI21(I)*P1(7)
        A3111=AI31(I)*P1(1)
        A3113=AI31(I)*P1(3)
        A3115=AI31(I)*P1(5)
        A3117=AI31(I)*P1(7)
        A1221=AI12*P2(1)
        A1222=AI12*P2(2)
        A1225=AI12*P2(5)
        A1226=AI12*P2(6)
        A2221=AI22*P2(1)
        A2222=AI22*P2(2)
C
        A2225=AI22*P2(5)
        A2226=AI22*P2(6)
        A3221=AI32*P2(1)
        A3222=AI32*P2(2)
        A3225=AI32*P2(5)
        A3226=AI32*P2(6)
        A1331=AI13*P3(1)
        A1332=AI13*P3(2)
        A1333=AI13*P3(3)
        A1334=AI13*P3(4)
        A2331=AI23*P3(1)
        A2332=AI23*P3(2)
        A2333=AI23*P3(3)
        A2334=AI23*P3(4)
        A3331=AI33*P3(1)
        A3332=AI33*P3(2)
        A3333=AI33*P3(3)
        A3334=AI33*P3(4)
        PX1(I,IPT)= A1111+A1221+A1331
        PX2(I,IPT)=-A1111+A1222+A1332
        PX3(I,IPT)= A1113-A1222+A1333
        PX4(I,IPT)=-A1113-A1221+A1334
        PX5(I,IPT)= A1115+A1225-A1331
        PX6(I,IPT)=-A1115+A1226-A1332
        PX7(I,IPT)= A1117-A1226-A1333
        PX8(I,IPT)=-A1117-A1225-A1334
        PY1(I,IPT)= A2111+A2221+A2331
        PY2(I,IPT)=-A2111+A2222+A2332
        PY3(I,IPT)= A2113-A2222+A2333
        PY4(I,IPT)=-A2113-A2221+A2334
        PY5(I,IPT)= A2115+A2225-A2331
        PY6(I,IPT)=-A2115+A2226-A2332
        PY7(I,IPT)= A2117-A2226-A2333
        PY8(I,IPT)=-A2117-A2225-A2334
        PZ1(I,IPT)= A3111+A3221+A3331
        PZ2(I,IPT)=-A3111+A3222+A3332
        PZ3(I,IPT)= A3113-A3222+A3333
        PZ4(I,IPT)=-A3113-A3221+A3334
        PZ5(I,IPT)= A3115+A3225-A3331
        PZ6(I,IPT)=-A3115+A3226-A3332
        PZ7(I,IPT)= A3117-A3226-A3333
        PZ8(I,IPT)=-A3117-A3225-A3334
      ENDDO
C
  100 CONTINUE
C
      DO I=1,NEL
c mediane * 4
        XX1 = X(I,1) + X(I,2) + X(I,3) + X(I,4)
     .      - X(I,5) - X(I,6) - X(I,7) - X(I,8)
        YY1 = Y(I,1) + Y(I,2) + Y(I,3) + Y(I,4)
     .      - Y(I,5) - Y(I,6) - Y(I,7) - Y(I,8)
        ZZ1 = Z(I,1) + Z(I,2) + Z(I,3) + Z(I,4)
     .      - Z(I,5) - Z(I,6) - Z(I,7) - Z(I,8)
        XX2 = X(I,1) + X(I,2) + X(I,5) + X(I,6)
     .      - X(I,3) - X(I,4) - X(I,7) - X(I,8)
        YY2 = Y(I,1) + Y(I,2) + Y(I,5) + Y(I,6)
     .      - Y(I,3) - Y(I,4) - Y(I,7) - Y(I,8)
        ZZ2 = Z(I,1) + Z(I,2) + Z(I,5) + Z(I,6)
     .      - Z(I,3) - Z(I,4) - Z(I,7) - Z(I,8)
        XX3 = X(I,1) + X(I,4) + X(I,5) + X(I,8)
     .      - X(I,3) - X(I,2) - X(I,7) - X(I,6)
        YY3 = Y(I,1) + Y(I,4) + Y(I,5) + Y(I,8)
     .      - Y(I,3) - Y(I,2) - Y(I,7) - Y(I,6)
        ZZ3 = Z(I,1) + Z(I,4) + Z(I,5) + Z(I,8)
     .      - Z(I,3) - Z(I,2) - Z(I,7) - Z(I,6)
C surface * 16
        SMAX =          (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
        SMAX = MAX(SMAX,(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
        SMAX = MAX(SMAX,(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
C volume / 8
        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX)
      ENDDO
      RETURN
      END
