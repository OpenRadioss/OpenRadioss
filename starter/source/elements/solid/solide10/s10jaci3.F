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
Chd|  S10JACI3                      source/elements/solid/solide10/s10jaci3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        S10JACOB                      source/elements/solid/solide10/s10deri3.F
Chd|        S10PIJSAV                     source/elements/solid/solide10/s10deri3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S10JACI3(ELBUF_STR,SAV,NPT,NEL)
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NPT,NEL
      DOUBLE PRECISION , DIMENSION(NEL,30),INTENT(IN) :: SAV     
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,N,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     .        M,IPERM(10,4)
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
      DOUBLE PRECISION
     .   XA(MVSIZ,10),YA(MVSIZ,10),ZA(MVSIZ,10),
     .   XB(MVSIZ,10),YB(MVSIZ,10),ZB(MVSIZ,10),
     .   A4,B4,A4M1,B4M1,AA
      my_real
     .   VOL(MVSIZ,5),NX(MVSIZ,10,5),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5)
      my_real
     .   ALPH,BETA,W
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      DATA IPERM/
     .            2, 4, 3, 1, 9,10, 6, 5, 8, 7,
     .            4, 1, 3, 2, 8, 7,10, 9, 5, 6,
     .            1, 4, 2, 3, 8, 9, 5, 7,10, 6,
     .            1, 2, 3, 4, 5, 6, 7, 8, 9,10/
C-----------------------------------------------
      DO N=1,10
       DO I=1,NEL
        XX(I,N) = SAV(I,N)   
        YY(I,N) = SAV(I,N+10)
        ZZ(I,N) = SAV(I,N+20)
       ENDDO
      END DO 
      IF(NPT==4)THEN
        ALPH = ZEP5854102
        BETA  = ZEP1381966
        W = FOURTH
      ELSEIF(NPT==5)THEN
        ALPH = HALF
        BETA  = ONE_OVER_6
        W = NINE_OVER_20
      ENDIF
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4- ONE
      B4M1  = B4- ONE
C
      DO N=1,4
        DO I=1,NEL
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
        DO I=1,NEL
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
         DO I=1,NEL
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
       DO IP=1,NPT
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
        CALL S10PIJSAV(PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),LBUF%PIJ,NEL)
        LBUF%VOL(1:NEL)=VOL(1:NEL,IP)
       ENDDO
C
      RETURN
C
      END
