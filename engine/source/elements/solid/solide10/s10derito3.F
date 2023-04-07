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
Chd|  S10DERITO3                    source/elements/solid/solide10/s10deri3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S10JACOBT                     source/elements/solid/solide10/s10jacob.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10DERITO3(
     1   VOL,      XX,       YY,       ZZ,
     2   PX,       PY,       PZ,       NX,
     3   RX,       RY,       RZ,       SX,
     4   SY,       SZ,       TX,       TY,
     5   TZ,       WIP,      ALPH,     BETA,
     6   VOLN,     ELBUF_STR,NEL,      NPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD            
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NPT
C     REAL
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
      my_real
     .   VOL(MVSIZ,5),
     .   NX(MVSIZ,10,5),VOLN(*),
     .   RX(*),RY(*),RZ(*), SX(*),SY(*),SZ(*), TX(*),TY(*),TZ(*),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .   WIP(5),ALPH(5),BETA(5)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,N,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     .        M,IPERM(10,4),ICOR,J,NN,IBID

      DOUBLE PRECISION
     .   A4, B4, A4M1, B4M1

      my_real
     .   VOLG(MVSIZ),VOLM
      my_real
     .   D,AA,BB,A1,A2,A3,
     .   A1X,A2X,A3X,A4X,A1Y,A2Y,A3Y,A4Y,A1Z,A2Z,A3Z,A4Z,AA0
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      DATA IPERM/
     .            2, 4, 3, 1, 9,10, 6, 5, 8, 7,
     .            4, 1, 3, 2, 8, 7,10, 9, 5, 6,
     .            1, 4, 2, 3, 8, 9, 5, 7,10, 6,
     .            1, 2, 3, 4, 5, 6, 7, 8, 9,10/
C-----------------------------------------------
      A4 = FOUR * ALPH(1)
      B4 = FOUR * BETA(1)
      A4M1  = A4 - ONE
      B4M1  = B4 - ONE      
C
      DO I=1,NEL
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
      IBID =1
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
        LBUF => ELBUF_STR%BUFLY(IBID)%LBUF(IP,IBID,IBID)
         CALL S10JACOBT(
     1   ALPH(IP),    BETA(IP),    WIP(IP),     PX(1,K1,IP),
     2   PX(1,K2,IP), PX(1,K3,IP), PX(1,K4,IP), PX(1,K5,IP),
     3   PX(1,K6,IP), PX(1,K7,IP), PX(1,K8,IP), PX(1,K9,IP),
     4   PX(1,K10,IP),PY(1,K1,IP), PY(1,K2,IP), PY(1,K3,IP),
     5   PY(1,K4,IP), PY(1,K5,IP), PY(1,K6,IP), PY(1,K7,IP),
     6   PY(1,K8,IP), PY(1,K9,IP), PY(1,K10,IP),PZ(1,K1,IP),
     7   PZ(1,K2,IP), PZ(1,K3,IP), PZ(1,K4,IP), PZ(1,K5,IP),
     8   PZ(1,K6,IP), PZ(1,K7,IP), PZ(1,K8,IP), PZ(1,K9,IP),
     9   PZ(1,K10,IP),NX(1,K1,IP), NX(1,K2,IP), NX(1,K3,IP),
     A   NX(1,K4,IP), NX(1,K5,IP), NX(1,K6,IP), NX(1,K7,IP),
     B   NX(1,K8,IP), NX(1,K9,IP), NX(1,K10,IP),VOL(1,IP),
     C   LBUF%JAC_I,  NEL)
c
      ENDDO
C
C
      IF(NPT==5)THEN
        IP = 5
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
         CALL S10JACOBT(
     1   ALPH(IP),    BETA(IP),    WIP(IP),     PX(1,K1,IP),
     2   PX(1,K2,IP), PX(1,K3,IP), PX(1,K4,IP), PX(1,K5,IP),
     3   PX(1,K6,IP), PX(1,K7,IP), PX(1,K8,IP), PX(1,K9,IP),
     4   PX(1,K10,IP),PY(1,K1,IP), PY(1,K2,IP), PY(1,K3,IP),
     5   PY(1,K4,IP), PY(1,K5,IP), PY(1,K6,IP), PY(1,K7,IP),
     6   PY(1,K8,IP), PY(1,K9,IP), PY(1,K10,IP),PZ(1,K1,IP),
     7   PZ(1,K2,IP), PZ(1,K3,IP), PZ(1,K4,IP), PZ(1,K5,IP),
     8   PZ(1,K6,IP), PZ(1,K7,IP), PZ(1,K8,IP), PZ(1,K9,IP),
     9   PZ(1,K10,IP),NX(1,K1,IP), NX(1,K2,IP), NX(1,K3,IP),
     A   NX(1,K4,IP), NX(1,K5,IP), NX(1,K6,IP), NX(1,K7,IP),
     B   NX(1,K8,IP), NX(1,K9,IP), NX(1,K10,IP),VOL(1,IP),
     C   LBUF%JAC_I,  NEL)
      ENDIF
C
C-----------
      RETURN
      END
