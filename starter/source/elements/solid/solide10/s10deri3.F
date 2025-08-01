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
!||    s10deri3       ../starter/source/elements/solid/solide10/s10deri3.F
!||--- called by ------------------------------------------------------
!||    inirig_mat     ../starter/source/elements/initia/inirig_mat.F
!||    s10init3       ../starter/source/elements/solid/solide10/s10init3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg         ../starter/source/output/message/message.F
!||    s10jacob       ../starter/source/elements/solid/solide10/s10deri3.F
!||    s10pijsav      ../starter/source/elements/solid/solide10/s10deri3.F
!||--- uses       -----------------------------------------------------
!||    message_mod    ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE S10DERI3(VOL,NGL,
     .   XX, YY, ZZ, PX,PY,PZ, NX,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,VOLU,VOLN,
     .   ELBUF_STR,VOLG)
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD            
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL 
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
      my_real
     .   VOL(MVSIZ,5),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*), TX(*),TY(*),TZ(*),
     .   NX(MVSIZ,10,5),VOLU(*),VOLN(*),VOLG(MVSIZ),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I,IP,N,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     .        M,IPERM(10,4),
     .        NBAD(MVSIZ)
      DOUBLE PRECISION
     .   XA(MVSIZ,10),YA(MVSIZ,10),ZA(MVSIZ,10),
     .   XB(MVSIZ,10),YB(MVSIZ,10),ZB(MVSIZ,10),
     .   A4,B4,A4M1,B4M1,AA
      my_real
     .   ALPH,BETA,W
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      DATA IPERM/
     .            2, 4, 3, 1, 9,10, 6, 5, 8, 7,
     .            4, 1, 3, 2, 8, 7,10, 9, 5, 6,
     .            1, 4, 2, 3, 8, 9, 5, 7,10, 6,
     .            1, 2, 3, 4, 5, 6, 7, 8, 9,10/
C-----------------------------------------------
      IF(NPT==4)THEN
        ALPH = ZEP5854102
        BETA  = ZEP1381966
        W = FOURTH
      ELSEIF(NPT==5)THEN
        ALPH = HALF
        BETA  = ONE_OVER_6
        W = NINE_OVER_20
      ELSE
        ALPH = -HUGE(ALPH)
        BETA  = -HUGE(BETA)
        W = -HUGE(W)
      ENDIF
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4- ONE
      B4M1  = B4- ONE
C
      DO I=LFT,LLT
        RX(I) = XX(I,1) - XX(I,4)
        RY(I) = YY(I,1) - YY(I,4)
        RZ(I) = ZZ(I,1) - ZZ(I,4)
        SX(I) = XX(I,2) - XX(I,4)
        SY(I) = YY(I,2) - YY(I,4)
        SZ(I) = ZZ(I,2) - ZZ(I,4)
        TX(I) = XX(I,3) - XX(I,4)
        TY(I) = YY(I,3) - YY(I,4)
        TZ(I) = ZZ(I,3) - ZZ(I,4)
        VOLG(I) =ZERO
      ENDDO
C
      DO N=1,4
        DO I=LFT,LLT
          XA(I,N) = A4M1*XX(I,N)
          YA(I,N) = A4M1*YY(I,N)
          ZA(I,N) = A4M1*ZZ(I,N)
C
          XB(I,N) = B4M1*XX(I,N)
          YB(I,N) = B4M1*YY(I,N)
          ZB(I,N) = B4M1*ZZ(I,N)
        ENDDO
      ENDDO
C
      DO N=5,10
        DO I=LFT,LLT
          XA(I,N) = A4*XX(I,N)
          YA(I,N) = A4*YY(I,N)
          ZA(I,N) = A4*ZZ(I,N)
C
          XB(I,N) = B4*XX(I,N)
          YB(I,N) = B4*YY(I,N)
          ZB(I,N) = B4*ZZ(I,N)
        ENDDO
      ENDDO
C
      DO IP=1,4
        K1 = IPERM(1,IP)
        K2 = IPERM(2,IP)
        K3 = IPERM(3,IP)
        K4 = IPERM(4,IP)
        K5 = IPERM(5,IP)
        K6 = IPERM(6,IP)
        K7 = IPERM(7,IP)
        K8 = IPERM(8,IP)
        K9 = IPERM(9,IP)
        K10= IPERM(10,IP)
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
        CALL S10JACOB(ALPH,BETA,W,
     .   XB(1,K1),XB(1,K2),XB(1,K3),XA(1,K4),XB(1,K5),
     .   XB(1,K6),XB(1,K7),XB(1,K8),XB(1,K9),XB(1,K10),
     .   XA(1,K8),XA(1,K9),XA(1,K10),
     .   YB(1,K1),YB(1,K2),YB(1,K3),YA(1,K4),YB(1,K5),
     .   YB(1,K6),YB(1,K7),YB(1,K8),YB(1,K9),YB(1,K10),
     .   YA(1,K8),YA(1,K9),YA(1,K10),
     .   ZB(1,K1),ZB(1,K2),ZB(1,K3),ZA(1,K4),ZB(1,K5),
     .   ZB(1,K6),ZB(1,K7),ZB(1,K8),ZB(1,K9),ZB(1,K10),
     .   ZA(1,K8),ZA(1,K9),ZA(1,K10),
     .   PX(1,K1,IP) ,PX(1,K2,IP),PX(1,K3,IP),PX(1,K4,IP),PX(1,K5,IP),
     .   PX(1,K6,IP) ,PX(1,K7,IP),PX(1,K8,IP),PX(1,K9,IP),PX(1,K10,IP),
     .   PY(1,K1,IP) ,PY(1,K2,IP),PY(1,K3,IP),PY(1,K4,IP),PY(1,K5,IP),
     .   PY(1,K6,IP) ,PY(1,K7,IP),PY(1,K8,IP),PY(1,K9,IP),PY(1,K10,IP),
     .   PZ(1,K1,IP) ,PZ(1,K2,IP),PZ(1,K3,IP),PZ(1,K4,IP),PZ(1,K5,IP),
     .   PZ(1,K6,IP) ,PZ(1,K7,IP),PZ(1,K8,IP),PZ(1,K9,IP),PZ(1,K10,IP),
     .   NX(1,K1,IP) ,NX(1,K2,IP),NX(1,K3,IP),NX(1,K4,IP),NX(1,K5,IP),
     .   NX(1,K6,IP) ,NX(1,K7,IP),NX(1,K8,IP),NX(1,K9,IP),NX(1,K10,IP),
     .   VOL(1,IP)   ,LBUF%VOL0DP)
c
      ENDDO
C
C
      IF(NPT==5)THEN
        ALPH = FOURTH
        BETA  = FOURTH
        A4 = ONE
        B4 = ONE
        A4M1  = ZERO
        B4M1  = ZERO
        W = - FOUR_OVER_5
        IP = 5
C
        DO N=1,4
         DO I=LFT,LLT
          XA(I,N) = ZERO
          YA(I,N) = ZERO
          ZA(I,N) = ZERO
         ENDDO
        ENDDO
CC
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(NPT,1,1)
        CALL S10JACOB(ALPH,BETA,W,
     .   XA(1,K1),XA(1,K2),XA(1,K3),XA(1,K4),XX(1,K5),
     .   XX(1,K6),XX(1,K7),XX(1,K8),XX(1,K9),XX(1,K10),
     .   XX(1,K8),XX(1,K9),XX(1,K10),
     .   YA(1,K1),YA(1,K2),YA(1,K3),YA(1,K4),YY(1,K5),
     .   YY(1,K6),YY(1,K7),YY(1,K8),YY(1,K9),YY(1,K10),
     .   YY(1,K8),YY(1,K9),YY(1,K10),
     .   ZA(1,K1),ZA(1,K2),ZA(1,K3),ZA(1,K4),ZZ(1,K5),
     .   ZZ(1,K6),ZZ(1,K7),ZZ(1,K8),ZZ(1,K9),ZZ(1,K10),
     .   ZZ(1,K8),ZZ(1,K9),ZZ(1,K10),
     .   PX(1,K1,IP) ,PX(1,K2,IP),PX(1,K3,IP),PX(1,K4,IP),PX(1,K5,IP),
     .   PX(1,K6,IP) ,PX(1,K7,IP),PX(1,K8,IP),PX(1,K9,IP),PX(1,K10,IP),
     .   PY(1,K1,IP) ,PY(1,K2,IP),PY(1,K3,IP),PY(1,K4,IP),PY(1,K5,IP),
     .   PY(1,K6,IP) ,PY(1,K7,IP),PY(1,K8,IP),PY(1,K9,IP),PY(1,K10,IP),
     .   PZ(1,K1,IP) ,PZ(1,K2,IP),PZ(1,K3,IP),PZ(1,K4,IP),PZ(1,K5,IP),
     .   PZ(1,K6,IP) ,PZ(1,K7,IP),PZ(1,K8,IP),PZ(1,K9,IP),PZ(1,K10,IP),
     .   NX(1,K1,IP) ,NX(1,K2,IP),NX(1,K3,IP),NX(1,K4,IP),NX(1,K5,IP),
     .   NX(1,K6,IP) ,NX(1,K7,IP),NX(1,K8,IP),NX(1,K9,IP),NX(1,K10,IP),
     .   VOL(1,IP)   ,LBUF%VOL0DP)
      ENDIF
C      
      IF (ISMSTR==10.OR.ISMSTR==12) THEN
       DO IP=1,NPT
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
        CALL S10PIJSAV(PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),LBUF%PIJ,LLT)
       ENDDO
      ENDIF
C
      DO I=LFT,LLT
       NBAD(I)=0
      ENDDO

      DO IP=1,NPT
        DO I=LFT,LLT

          IF(IP<=4.AND.VOL(I,IP)<=ZERO)THEN
              NBAD(I)=NBAD(I)+1
              VOL(I,IP)=ZERO
          ENDIF

          VOLG(I) =VOLG(I) + VOL(I,IP)
        ENDDO
      ENDDO

      DO I=LFT,LLT
       IF(NBAD(I)>0)THEN
         CALL ANCMSG(MSGID=489,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=NGL(I))
       ENDIF
      ENDDO

      DO I=LFT,LLT
        VOLU(I) =VOLG(I)/NPT
        VOLN(I) =VOLG(I)
      END DO
C
      RETURN
C
C  1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
C  2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
!||====================================================================
!||    s10jacob   ../starter/source/elements/solid/solide10/s10deri3.F
!||--- called by ------------------------------------------------------
!||    s10deri3   ../starter/source/elements/solid/solide10/s10deri3.F
!||    s10jaci3   ../starter/source/elements/solid/solide10/s10jaci3.F
!||====================================================================
      SUBROUTINE S10JACOB(ALPH,BETA,W,
     .   X1B,X2B,X3B,X4A,X5B,X6B,X7B,X8B,X9B,X10B,X8A,X9A,X10A,
     .   Y1B,Y2B,Y3B,Y4A,Y5B,Y6B,Y7B,Y8B,Y9B,Y10B,Y8A,Y9A,Y10A,
     .   Z1B,Z2B,Z3B,Z4A,Z5B,Z6B,Z7B,Z8B,Z9B,Z10B,Z8A,Z9A,Z10A,
     .   PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,PX9,PX10,
     .   PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,PY9,PY10,
     .   PZ1,PZ2,PZ3,PZ4,PZ5,PZ6,PZ7,PZ8,PZ9,PZ10,
     .   NX1,NX2,NX3,NX4,NX5,NX6,NX7,NX8,NX9,NX10,
     .   VOL,VOLDP)
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
C     REAL
      DOUBLE PRECISION
     .   X1B(MVSIZ),X2B(MVSIZ),X3B(MVSIZ),X4A(MVSIZ),X5B(MVSIZ),
     .   X6B(MVSIZ),X7B(MVSIZ),X8B(MVSIZ),X9B(MVSIZ),X10B(MVSIZ),
     .   X8A(MVSIZ),X9A(MVSIZ),X10A(MVSIZ),
     .   Y1B(MVSIZ),Y2B(MVSIZ),Y3B(MVSIZ),Y4A(MVSIZ),Y5B(MVSIZ),
     .   Y6B(MVSIZ),Y7B(MVSIZ),Y8B(MVSIZ),Y9B(MVSIZ),Y10B(MVSIZ),
     .   Y8A(MVSIZ),Y9A(MVSIZ),Y10A(MVSIZ),
     .   Z1B(MVSIZ),Z2B(MVSIZ),Z3B(MVSIZ),Z4A(MVSIZ),Z5B(MVSIZ),
     .   Z6B(MVSIZ),Z7B(MVSIZ),Z8B(MVSIZ),Z9B(MVSIZ),Z10B(MVSIZ),
     .   Z8A(MVSIZ),Z9A(MVSIZ),Z10A(MVSIZ),VOLDP(*)
      my_real
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),PX5(MVSIZ),
     .   PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),PX9(MVSIZ),PX10(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),PY5(MVSIZ),
     .   PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),PY9(MVSIZ),PY10(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),PZ5(MVSIZ),
     .   PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),PZ9(MVSIZ),PZ10(MVSIZ),
     .   NX1(MVSIZ),NX2(MVSIZ),NX3(MVSIZ),NX4(MVSIZ),NX5(MVSIZ),
     .   NX6(MVSIZ),NX7(MVSIZ),NX8(MVSIZ),NX9(MVSIZ),NX10(MVSIZ),
     .   VOL(MVSIZ),ALPH,BETA,W
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      DOUBLE PRECISION
     .   A4M1,B4M1,AA
c      my_real
      DOUBLE PRECISION
     .   D,A4MB4,A4,B4,BB,AB,B2,
     .   DXDR,DXDS,DXDT,DYDR,DYDS,DYDT,DZDR,DZDS,DZDT
c      my_real
      DOUBLE PRECISION
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   DET(MVSIZ)
C-----------------------------------------------
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4 - ONE
      B4M1  = B4 - ONE
      AA = (TWO*ALPH - ONE)*ALPH
      BB = (TWO*BETA - ONE)*BETA
      B2 = FOUR*BETA*BETA
      AB = FOUR*ALPH*BETA      
C-----------------------------------------------
      DO I=LFT,LLT
        NX1(I) = BB
        NX2(I) = BB
        NX3(I) = BB
        NX4(I) = AA
        NX5(I) = B2
        NX6(I) = B2
        NX7(I) = B2
        NX8(I) = AB
        NX9(I) = AB
        NX10(I)= AB
      ENDDO
C
      DO I=LFT,LLT
       AA = X5B(I) + X6B(I) + X7B(I)
     .    - X4A(I) - X8B(I) - X9B(I) - X10B(I)
       DXDR = X1B(I) +X8A(I) - X6B(I) + AA
       DXDS = X2B(I) +X9A(I) - X7B(I) + AA
       DXDT = X3B(I) +X10A(I)- X5B(I) + AA
C
       AA = Y5B(I) + Y6B(I) + Y7B(I)
     .    - Y4A(I) - Y8B(I) - Y9B(I) - Y10B(I)
       DYDR = Y1B(I) +Y8A(I) - Y6B(I) + AA
       DYDS = Y2B(I) +Y9A(I) - Y7B(I) + AA
       DYDT = Y3B(I) +Y10A(I)- Y5B(I) + AA
C
       AA = Z5B(I) + Z6B(I) + Z7B(I)
     .    - Z4A(I) - Z8B(I) - Z9B(I) - Z10B(I)
       DZDR = Z1B(I) +Z8A(I) - Z6B(I) + AA
       DZDS = Z2B(I) +Z9A(I) - Z7B(I) + AA
       DZDT = Z3B(I) +Z10A(I)- Z5B(I) + AA
C
       DRDX(I)=DYDS*DZDT-DZDS*DYDT
       DSDX(I)=DYDT*DZDR-DZDT*DYDR
       DTDX(I)=DYDR*DZDS-DZDR*DYDS
C
       DRDY(I)=DZDS*DXDT-DXDS*DZDT
       DSDY(I)=DZDT*DXDR-DXDT*DZDR
       DTDY(I)=DZDR*DXDS-DXDR*DZDS
C
       DRDZ(I)=DXDS*DYDT-DYDS*DXDT
       DSDZ(I)=DXDT*DYDR-DYDT*DXDR
       DTDZ(I)=DXDR*DYDS-DYDR*DXDS
C
       DET(I) = DXDR * DRDX(I)
     .        + DYDR * DRDY(I)
     .        + DZDR * DRDZ(I)
       VOLDP(I) = W * DET(I) * ONE_OVER_6
       VOL(I) = VOLDP(I)
C
      ENDDO
C      IF(IDTMIN(1)==1)THEN
C       DO I=LFT,LLT
C        IF(OFF(I) ==0.)THEN
C          DET(I)=1.
C        ELSEIF(DET(I)<=VOLMIN)THEN
C          DET(I)=1.
C          OFF(I)=0.
C          WRITE(ISTDO,2000) NGL(I)
C          WRITE(IOUT ,2000) NGL(I)
C        ELSEIF(DET(I)<=0.0)THEN
C          WRITE(ISTDO,1000) NGL(I)
C          WRITE(IOUT ,1000) NGL(I)
C        ENDIF
C       ENDDO
C      ELSEIF(IDTMIN(1)==2)THEN
C       DO I=LFT,LLT
C        IF(OFF(I) ==0.)THEN
C           DET(I)=1.
C        ELSEIF(DET(I)<=VOLMIN.OR.DET(I)<=0.0)THEN
C           DET(I)=1.
C           OFF(I)=0.
C           WRITE(ISTDO,2000) NGL(I)
C           WRITE(IOUT ,2000) NGL(I)
C        ENDIF
C       ENDDO
C      ELSE
C        DO I=LFT,LLT
C          IF(OFF(I) ==0.)DET(I)=1.
C        ENDDO
C      ENDIF
CC
      DO I=LFT,LLT
        A4 = FOUR * ALPH
        B4 = FOUR * BETA
        A4M1  = A4 - ONE
        B4M1  = B4 - ONE
        D = ONE/MAX(EM30,DET(I))
C
        B4M1 = D * B4M1
        A4M1 = D * A4M1
        B4 = D * B4
        A4 = D * A4
        A4MB4  = A4 - B4
C
        PX1(I) = B4M1 * DRDX(I)
        PY1(I) = B4M1 * DRDY(I)
        PZ1(I) = B4M1 * DRDZ(I)
C
        PX2(I) = B4M1 * DSDX(I)
        PY2(I) = B4M1 * DSDY(I)
        PZ2(I) = B4M1 * DSDZ(I)
C
        PX3(I) = B4M1 * DTDX(I)
        PY3(I) = B4M1 * DTDY(I)
        PZ3(I) = B4M1 * DTDZ(I)
C
        PX4(I) =-A4M1 * (DRDX(I)+DSDX(I)+DTDX(I))
        PY4(I) =-A4M1 * (DRDY(I)+DSDY(I)+DTDY(I))
        PZ4(I) =-A4M1 * (DRDZ(I)+DSDZ(I)+DTDZ(I))
C
        PX5(I) = B4 * (DRDX(I)+DSDX(I))
        PY5(I) = B4 * (DRDY(I)+DSDY(I))
        PZ5(I) = B4 * (DRDZ(I)+DSDZ(I))
C
        PX6(I) = B4 * (DSDX(I)+DTDX(I))
        PY6(I) = B4 * (DSDY(I)+DTDY(I))
        PZ6(I) = B4 * (DSDZ(I)+DTDZ(I))
C
        PX7(I) = B4 * (DTDX(I)+DRDX(I))
        PY7(I) = B4 * (DTDY(I)+DRDY(I))
        PZ7(I) = B4 * (DTDZ(I)+DRDZ(I))
C
        PX8(I) = A4MB4  * DRDX(I) - PX6(I)
        PY8(I) = A4MB4  * DRDY(I) - PY6(I)
        PZ8(I) = A4MB4  * DRDZ(I) - PZ6(I)
C
        PX9(I) = A4MB4  * DSDX(I) - PX7(I)
        PY9(I) = A4MB4  * DSDY(I) - PY7(I)
        PZ9(I) = A4MB4  * DSDZ(I) - PZ7(I)
C
        PX10(I)= A4MB4  * DTDX(I) - PX5(I)
        PY10(I)= A4MB4  * DTDY(I) - PY5(I)
        PZ10(I)= A4MB4  * DTDZ(I) - PZ5(I)
C
      ENDDO
C
      RETURN
      END
!||====================================================================
!||    s10pijsav   ../starter/source/elements/solid/solide10/s10deri3.F
!||--- called by ------------------------------------------------------
!||    s10deri3    ../starter/source/elements/solid/solide10/s10deri3.F
!||    s10jaci3    ../starter/source/elements/solid/solide10/s10jaci3.F
!||====================================================================
      SUBROUTINE S10PIJSAV(PX,PY,PZ, PIJ,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .   PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   PIJ(NEL,3,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
      DO J=1,10
       DO I=1,NEL
        PIJ(I,1,J) = PX(I,J) 
        PIJ(I,2,J) = PY(I,J) 
        PIJ(I,3,J) = PZ(I,J)  
       END DO
      END DO
C-----------
      RETURN
      END
