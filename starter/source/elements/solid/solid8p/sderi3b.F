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
Chd|  SDERI3B                       source/elements/solid/solid8p/sderi3b.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BASISF                        source/elements/solid/solid8p/basisf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SDERI3B(VOL,VEUL,LVLOC,GEO ,IGEO ,NGL  ,NGEO ,
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              DET, DELTAX,NEL  ,JEUL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "pt8_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IGEO(NPROPGI,*),NGL(*),NGEO(*),LVLOC,NEL,JEUL
      my_real
     .   VOL(*),DET(*), VEUL(LVLOC,*),GEO(NPROPG,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   DELTAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JPT, IPT
      my_real
     .   AJ11(MVSIZ), AJ12(MVSIZ), AJ13(MVSIZ), AJ21(MVSIZ),
     .   AJ22(MVSIZ), AJ23(MVSIZ), AJ31(MVSIZ), AJ32(MVSIZ),
     .   AJ33(MVSIZ), AI11(MVSIZ), AI12(MVSIZ), AI13(MVSIZ),
     .   AI21(MVSIZ), AI22(MVSIZ), AI23(MVSIZ), AI31(MVSIZ),
     .   AI32(MVSIZ), AI33(MVSIZ), A1111(MVSIZ), A1113(MVSIZ),
     .   A1115(MVSIZ), A1117(MVSIZ), A2111(MVSIZ), A2113(MVSIZ),
     .   A2115(MVSIZ), A2117(MVSIZ), A3111(MVSIZ), A3113(MVSIZ),
     .   A3115(MVSIZ), A3117(MVSIZ), A1221(MVSIZ), A1222(MVSIZ),
     .   A1225(MVSIZ), A1226(MVSIZ), A2221(MVSIZ), A2222(MVSIZ),
     .   A2225(MVSIZ), A2226(MVSIZ), A3221(MVSIZ), A3222(MVSIZ),
     .   A3225(MVSIZ), A3226(MVSIZ), A1331(MVSIZ), A1332(MVSIZ),
     .   A1333(MVSIZ), A1334(MVSIZ), A2331(MVSIZ), A2332(MVSIZ),
     .   A2333(MVSIZ), A2334(MVSIZ), A3331(MVSIZ), A3332(MVSIZ),
     .   A3333(MVSIZ), A3334(MVSIZ), X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
     .   X78(MVSIZ), Y12(MVSIZ), Y34(MVSIZ), Y56(MVSIZ), Y78(MVSIZ),
     .   Z12(MVSIZ), Z34(MVSIZ), Z56(MVSIZ), Z78(MVSIZ), X14(MVSIZ),
     .   X23(MVSIZ), X58(MVSIZ), X67(MVSIZ), Y14(MVSIZ), Y23(MVSIZ),
     .   Y58(MVSIZ), Y67(MVSIZ), Z14(MVSIZ), Z23(MVSIZ), Z58(MVSIZ),
     .   Z67(MVSIZ), X15(MVSIZ), X26(MVSIZ), X37(MVSIZ), X48(MVSIZ),
     .   Y15(MVSIZ), Y26(MVSIZ), Y37(MVSIZ), Y48(MVSIZ), Z15(MVSIZ),
     .   Z26(MVSIZ), Z37(MVSIZ), Z48(MVSIZ),
     .   VLINV(MVSIZ),VLINC(MVSIZ,8), H(8),
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),
     .   PX5(MVSIZ), PX6(MVSIZ), PX7(MVSIZ), PX8(MVSIZ),
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),
     .   PY5(MVSIZ), PY6(MVSIZ), PY7(MVSIZ), PY8(MVSIZ),
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),
     .   PZ5(MVSIZ), PZ6(MVSIZ), PZ7(MVSIZ), PZ8(MVSIZ)
      my_real
     .   PORO, SMAX, VMIN, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3
C=======================================================================
      DO I=1,NEL
        X12(I)=X1(I)-X2(I)
        X34(I)=X3(I)-X4(I)
        X56(I)=X5(I)-X6(I)
        X78(I)=X7(I)-X8(I)
        X14(I)=X1(I)-X4(I)
        X23(I)=X2(I)-X3(I)
        X58(I)=X5(I)-X8(I)
        X67(I)=X6(I)-X7(I)
        X15(I)=X1(I)-X5(I)
        X26(I)=X2(I)-X6(I)
        X37(I)=X3(I)-X7(I)
        X48(I)=X4(I)-X8(I)
C
        Y12(I)=Y1(I)-Y2(I)
        Y34(I)=Y3(I)-Y4(I)
        Y56(I)=Y5(I)-Y6(I)
        Y78(I)=Y7(I)-Y8(I)
        Y14(I)=Y1(I)-Y4(I)
        Y23(I)=Y2(I)-Y3(I)
        Y58(I)=Y5(I)-Y8(I)
        Y67(I)=Y6(I)-Y7(I)
        Y15(I)=Y1(I)-Y5(I)
        Y26(I)=Y2(I)-Y6(I)
        Y37(I)=Y3(I)-Y7(I)
        Y48(I)=Y4(I)-Y8(I)
C
        Z12(I)=Z1(I)-Z2(I)
        Z34(I)=Z3(I)-Z4(I)
        Z56(I)=Z5(I)-Z6(I)
        Z78(I)=Z7(I)-Z8(I)
        Z14(I)=Z1(I)-Z4(I)
        Z23(I)=Z2(I)-Z3(I)
        Z58(I)=Z5(I)-Z8(I)
        Z67(I)=Z6(I)-Z7(I)
        Z15(I)=Z1(I)-Z5(I)
        Z26(I)=Z2(I)-Z6(I)
        Z37(I)=Z3(I)-Z7(I)
        Z48(I)=Z4(I)-Z8(I)
C
        DET(I)=ZERO
        DO J=1,12
          VEUL(J,I)=ZERO
        ENDDO
        DO J=32,51
          VEUL(J,I)=ZERO
        ENDDO
C
      ENDDO

      DO JPT=1,8
        IPT=JPT
C Loop on integration points
C Get basis functions for integration point IPT
        CALL BASISF (H,P11,P21,P31,IPT)
C
        DO I=1,NEL
          AJ11(I)=P11*X12(I)+P13*X34(I)+P15*X56(I)+P17*X78(I)
          AJ12(I)=P11*Y12(I)+P13*Y34(I)+P15*Y56(I)+P17*Y78(I)
          AJ13(I)=P11*Z12(I)+P13*Z34(I)+P15*Z56(I)+P17*Z78(I)
          AJ21(I)=P21*X14(I)+P22*X23(I)+P25*X58(I)+P26*X67(I)
          AJ22(I)=P21*Y14(I)+P22*Y23(I)+P25*Y58(I)+P26*Y67(I)
          AJ23(I)=P21*Z14(I)+P22*Z23(I)+P25*Z58(I)+P26*Z67(I)
          AJ31(I)=P31*X15(I)+P32*X26(I)+P33*X37(I)+P34*X48(I)
          AJ32(I)=P31*Y15(I)+P32*Y26(I)+P33*Y37(I)+P34*Y48(I)
          AJ33(I)=P31*Z15(I)+P32*Z26(I)+P33*Z37(I)+P34*Z48(I)
        ENDDO
C
        DO I=1,NEL
          AI11(I)= AJ22(I)*AJ33(I)-AJ23(I)*AJ32(I)
          AI21(I)=-AJ21(I)*AJ33(I)+AJ23(I)*AJ31(I)
          AI31(I)= AJ21(I)*AJ32(I)-AJ22(I)*AJ31(I)
          VLINC(I,JPT)=AJ11(I)*AI11(I)+AJ12(I)*AI21(I)+AJ13(I)*AI31(I)
        ENDDO
C
        DO I=1,NEL
          IF(VLINC(I,JPT)>ZERO) CYCLE
          CALL ANCMSG(MSGID=245,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
        ENDDO
C
        DO I=1,NEL
          VLINV(I)=ONE/VLINC(I,JPT)
          AI11(I)=VLINV(I)*AI11(I)
          AI21(I)=VLINV(I)*AI21(I)
          AI31(I)=VLINV(I)*AI31(I)
          AI12(I)=VLINV(I)*(-AJ12(I)*AJ33(I)+AJ13(I)*AJ32(I))
          AI22(I)=VLINV(I)*( AJ11(I)*AJ33(I)-AJ13(I)*AJ31(I))
          AI32(I)=VLINV(I)*(-AJ11(I)*AJ32(I)+AJ12(I)*AJ31(I))
          AI13(I)=VLINV(I)*( AJ12(I)*AJ23(I)-AJ13(I)*AJ22(I))
          AI23(I)=VLINV(I)*(-AJ11(I)*AJ23(I)+AJ13(I)*AJ21(I))
          AI33(I)=VLINV(I)*( AJ11(I)*AJ22(I)-AJ12(I)*AJ21(I))
        ENDDO
C
        DO I=1,NEL
          A1111(I)=AI11(I)*P11
          A1113(I)=AI11(I)*P13
          A1115(I)=AI11(I)*P15
          A1117(I)=AI11(I)*P17
          A2111(I)=AI21(I)*P11
          A2113(I)=AI21(I)*P13
          A2115(I)=AI21(I)*P15
          A2117(I)=AI21(I)*P17
          A3111(I)=AI31(I)*P11
          A3113(I)=AI31(I)*P13
          A3115(I)=AI31(I)*P15
          A3117(I)=AI31(I)*P17
          A1221(I)=AI12(I)*P21
          A1222(I)=AI12(I)*P22
          A1225(I)=AI12(I)*P25
          A1226(I)=AI12(I)*P26
          A2221(I)=AI22(I)*P21
          A2222(I)=AI22(I)*P22
        ENDDO
C
        DO I=1,NEL
          A2225(I)=AI22(I)*P25
          A2226(I)=AI22(I)*P26
          A3221(I)=AI32(I)*P21
          A3222(I)=AI32(I)*P22
          A3225(I)=AI32(I)*P25
          A3226(I)=AI32(I)*P26
          A1331(I)=AI13(I)*P31
          A1332(I)=AI13(I)*P32
          A1333(I)=AI13(I)*P33
          A1334(I)=AI13(I)*P34
          A2331(I)=AI23(I)*P31
          A2332(I)=AI23(I)*P32
          A2333(I)=AI23(I)*P33
          A2334(I)=AI23(I)*P34
          A3331(I)=AI33(I)*P31
          A3332(I)=AI33(I)*P32
          A3333(I)=AI33(I)*P33
          A3334(I)=AI33(I)*P34
        ENDDO
C
        DO I=1,NEL
          PX1(I)= A1111(I)+A1221(I)+A1331(I)
          PX2(I)=-A1111(I)+A1222(I)+A1332(I)
          PX3(I)= A1113(I)-A1222(I)+A1333(I)
          PX4(I)=-A1113(I)-A1221(I)+A1334(I)
          PX5(I)= A1115(I)+A1225(I)-A1331(I)
          PX6(I)=-A1115(I)+A1226(I)-A1332(I)
          PX7(I)= A1117(I)-A1226(I)-A1333(I)
          PX8(I)=-A1117(I)-A1225(I)-A1334(I)
          PY1(I)= A2111(I)+A2221(I)+A2331(I)
          PY2(I)=-A2111(I)+A2222(I)+A2332(I)
          PY3(I)= A2113(I)-A2222(I)+A2333(I)
          PY4(I)=-A2113(I)-A2221(I)+A2334(I)
          PY5(I)= A2115(I)+A2225(I)-A2331(I)
          PY6(I)=-A2115(I)+A2226(I)-A2332(I)
          PY7(I)= A2117(I)-A2226(I)-A2333(I)
          PY8(I)=-A2117(I)-A2225(I)-A2334(I)
          PZ1(I)= A3111(I)+A3221(I)+A3331(I)
          PZ2(I)=-A3111(I)+A3222(I)+A3332(I)
          PZ3(I)= A3113(I)-A3222(I)+A3333(I)
          PZ4(I)=-A3113(I)-A3221(I)+A3334(I)
          PZ5(I)= A3115(I)+A3225(I)-A3331(I)
          PZ6(I)=-A3115(I)+A3226(I)-A3332(I)
          PZ7(I)= A3117(I)-A3226(I)-A3333(I)
          PZ8(I)=-A3117(I)-A3225(I)-A3334(I)
        ENDDO
C
        DO I=1,NEL
          DET(I)=DET(I)+VLINC(I,JPT)
          VEUL(1,I)=PX1(I)*VLINC(I,JPT)+VEUL(1,I)
          VEUL(2,I)=PX2(I)*VLINC(I,JPT)+VEUL(2,I)
          VEUL(3,I)=PX3(I)*VLINC(I,JPT)+VEUL(3,I)
          VEUL(4,I)=PX4(I)*VLINC(I,JPT)+VEUL(4,I)
          VEUL(5,I)=PX5(I)*VLINC(I,JPT)+VEUL(5,I)
          VEUL(6,I)=PX6(I)*VLINC(I,JPT)+VEUL(6,I)
          VEUL(7,I)=PX7(I)*VLINC(I,JPT)+VEUL(7,I)
          VEUL(8,I)=PX8(I)*VLINC(I,JPT)+VEUL(8,I)
          VEUL(9,I)=PY1(I)*VLINC(I,JPT)+VEUL(9,I)
          VEUL(10,I)=PY2(I)*VLINC(I,JPT)+VEUL(10,I)
          VEUL(11,I)=PY3(I)*VLINC(I,JPT)+VEUL(11,I)
          VEUL(12,I)=PY4(I)*VLINC(I,JPT)+VEUL(12,I)
          VEUL(32,I)=PY5(I)*VLINC(I,JPT)+VEUL(32,I)
          VEUL(33,I)=PY6(I)*VLINC(I,JPT)+VEUL(33,I)
          VEUL(34,I)=PY7(I)*VLINC(I,JPT)+VEUL(34,I)
          VEUL(35,I)=PY8(I)*VLINC(I,JPT)+VEUL(35,I)
          VEUL(36,I)=PZ1(I)*VLINC(I,JPT)+VEUL(36,I)
          VEUL(37,I)=PZ2(I)*VLINC(I,JPT)+VEUL(37,I)
          VEUL(38,I)=PZ3(I)*VLINC(I,JPT)+VEUL(38,I)
          VEUL(39,I)=PZ4(I)*VLINC(I,JPT)+VEUL(39,I)
          VEUL(40,I)=PZ5(I)*VLINC(I,JPT)+VEUL(40,I)
          VEUL(41,I)=PZ6(I)*VLINC(I,JPT)+VEUL(41,I)
          VEUL(42,I)=PZ7(I)*VLINC(I,JPT)+VEUL(42,I)
          VEUL(43,I)=PZ8(I)*VLINC(I,JPT)+VEUL(43,I)
          VEUL(44,I)=VEUL(44,I)+VLINC(I,JPT)*H(1)
          VEUL(45,I)=VEUL(45,I)+VLINC(I,JPT)*H(2)
          VEUL(46,I)=VEUL(46,I)+VLINC(I,JPT)*H(3)
          VEUL(47,I)=VEUL(47,I)+VLINC(I,JPT)*H(4)
          VEUL(48,I)=VEUL(48,I)+VLINC(I,JPT)*H(5)
          VEUL(49,I)=VEUL(49,I)+VLINC(I,JPT)*H(6)
          VEUL(50,I)=VEUL(50,I)+VLINC(I,JPT)*H(7)
          VEUL(51,I)=VEUL(51,I)+VLINC(I,JPT)*H(8)
        ENDDO
C
      ENDDO !JPT=1,8
C
      DO I=1,NEL
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
        SMAX =          (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
        SMAX = MAX(SMAX,(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
        SMAX = MAX(SMAX,(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX)
      ENDDO
C
      DO J=1,12
        DO I=1,NEL
          VEUL(J,I)=VEUL(J,I)/DET(I)
        ENDDO
      ENDDO
C
      DO J=32,43
        DO I=1,NEL
          VEUL(J,I)=VEUL(J,I)/DET(I)
        ENDDO
      ENDDO
C
      DO I=1,NEL
        VOL(I)=DET(I)
      ENDDO
C      
      IF(JEUL /= 0)THEN
        DO I=1,NEL
          VEUL(52,I) = VOL(I)
        ENDDO
      ENDIF
C
      IF (NINT(GEO(12,NGEO(1))) == 15) THEN
        DO I=1,NEL
          PORO=GEO(1,NGEO(I))
          VEUL(44,I)=VEUL(44,I)*PORO
          VEUL(45,I)=VEUL(45,I)*PORO
          VEUL(46,I)=VEUL(46,I)*PORO
          VEUL(47,I)=VEUL(47,I)*PORO
          VEUL(48,I)=VEUL(48,I)*PORO
          VEUL(49,I)=VEUL(49,I)*PORO
          VEUL(50,I)=VEUL(50,I)*PORO
          VEUL(51,I)=VEUL(51,I)*PORO
          VOL(I)=VOL(I)*PORO
        ENDDO
      ENDIF
C-----------
      RETURN
      END
