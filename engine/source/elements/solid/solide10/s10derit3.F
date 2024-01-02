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
Chd|  S10DERIT3                     source/elements/solid/solide10/s10derit3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        S10JACOB                      source/elements/solid/solide10/s10jacob.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10DERIT3(
     1   VOL,     DELTAX,  DELTAX2, XX,
     2   YY,      ZZ,      PX,      PY,
     3   PZ,      NX,      RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      WIP,
     6   ALPH,    BETA,    VOLN,    VOLG,
     7   VOLDP,   NEL,     OFFG,    NPT)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPT
C     REAL
      INTEGER, INTENT(IN) :: NEL ! number of element in the current group
      my_real, DIMENSION(NEL), INTENT(IN) :: OFFG ! off array : 0 if the element is deleted
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),VOLDP(MVSIZ,5)
      my_real
     .   VOL(MVSIZ,5),DELTAX(*),DELTAX2(*),
     .   NX(MVSIZ,10,5),VOLN(*),VOLG(MVSIZ),
     .   RX(*),RY(*),RZ(*), SX(*),SY(*),SZ(*), TX(*),TY(*),TZ(*),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .   WIP(5),ALPH(5),BETA(5)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,N,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     .        M,IPERM(10,4),ICOR,NNEGA,INDEX(MVSIZ),J,NN
      INTEGER ITER,NITER
      DOUBLE PRECISION
     .   XA(MVSIZ,10),YA(MVSIZ,10),ZA(MVSIZ,10),
     .   XB(MVSIZ,10),YB(MVSIZ,10),ZB(MVSIZ,10),   
     .   A4, B4, A4M1, B4M1
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
        CALL S10JACOB(
     1   ALPH(IP),    BETA(IP),    WIP(IP),     XB(1,K1),
     2   XB(1,K2),    XB(1,K3),    XA(1,K4),    XB(1,K5),
     3   XB(1,K6),    XB(1,K7),    XB(1,K8),    XB(1,K9),
     4   XB(1,K10),   XA(1,K8),    XA(1,K9),    XA(1,K10),
     5   YB(1,K1),    YB(1,K2),    YB(1,K3),    YA(1,K4),
     6   YB(1,K5),    YB(1,K6),    YB(1,K7),    YB(1,K8),
     7   YB(1,K9),    YB(1,K10),   YA(1,K8),    YA(1,K9),
     8   YA(1,K10),   ZB(1,K1),    ZB(1,K2),    ZB(1,K3),
     9   ZA(1,K4),    ZB(1,K5),    ZB(1,K6),    ZB(1,K7),
     A   ZB(1,K8),    ZB(1,K9),    ZB(1,K10),   ZA(1,K8),
     B   ZA(1,K9),    ZA(1,K10),   PX(1,K1,IP), PX(1,K2,IP),
     C   PX(1,K3,IP), PX(1,K4,IP), PX(1,K5,IP), PX(1,K6,IP),
     D   PX(1,K7,IP), PX(1,K8,IP), PX(1,K9,IP), PX(1,K10,IP),
     E   PY(1,K1,IP), PY(1,K2,IP), PY(1,K3,IP), PY(1,K4,IP),
     F   PY(1,K5,IP), PY(1,K6,IP), PY(1,K7,IP), PY(1,K8,IP),
     G   PY(1,K9,IP), PY(1,K10,IP),PZ(1,K1,IP), PZ(1,K2,IP),
     H   PZ(1,K3,IP), PZ(1,K4,IP), PZ(1,K5,IP), PZ(1,K6,IP),
     I   PZ(1,K7,IP), PZ(1,K8,IP), PZ(1,K9,IP), PZ(1,K10,IP),
     J   NX(1,K1,IP), NX(1,K2,IP), NX(1,K3,IP), NX(1,K4,IP),
     K   NX(1,K5,IP), NX(1,K6,IP), NX(1,K7,IP), NX(1,K8,IP),
     L   NX(1,K9,IP), NX(1,K10,IP),VOL(1,IP),   VOLDP(1,IP),
     M   NEL,         OFFG)
c
      ENDDO
C
C
      IF(NPT==5)THEN
        IP = 5
C
        DO I=1,NEL
          XA(I,1) = ZERO
        ENDDO 
CC
        CALL S10JACOB(
     1   ALPH(IP),    BETA(IP),    WIP(IP),     XA(1,1),
     2   XA(1,1),     XA(1,1),     XA(1,1),     XX(1,K5),
     3   XX(1,K6),    XX(1,K7),    XX(1,K8),    XX(1,K9),
     4   XX(1,K10),   XX(1,K8),    XX(1,K9),    XX(1,K10),
     5   XA(1,1),     XA(1,1),     XA(1,1),     XA(1,1),
     6   YY(1,K5),    YY(1,K6),    YY(1,K7),    YY(1,K8),
     7   YY(1,K9),    YY(1,K10),   YY(1,K8),    YY(1,K9),
     8   YY(1,K10),   XA(1,1),     XA(1,1),     XA(1,1),
     9   XA(1,1),     ZZ(1,K5),    ZZ(1,K6),    ZZ(1,K7),
     A   ZZ(1,K8),    ZZ(1,K9),    ZZ(1,K10),   ZZ(1,K8),
     B   ZZ(1,K9),    ZZ(1,K10),   PX(1,K1,IP), PX(1,K2,IP),
     C   PX(1,K3,IP), PX(1,K4,IP), PX(1,K5,IP), PX(1,K6,IP),
     D   PX(1,K7,IP), PX(1,K8,IP), PX(1,K9,IP), PX(1,K10,IP),
     E   PY(1,K1,IP), PY(1,K2,IP), PY(1,K3,IP), PY(1,K4,IP),
     F   PY(1,K5,IP), PY(1,K6,IP), PY(1,K7,IP), PY(1,K8,IP),
     G   PY(1,K9,IP), PY(1,K10,IP),PZ(1,K1,IP), PZ(1,K2,IP),
     H   PZ(1,K3,IP), PZ(1,K4,IP), PZ(1,K5,IP), PZ(1,K6,IP),
     I   PZ(1,K7,IP), PZ(1,K8,IP), PZ(1,K9,IP), PZ(1,K10,IP),
     J   NX(1,K1,IP), NX(1,K2,IP), NX(1,K3,IP), NX(1,K4,IP),
     K   NX(1,K5,IP), NX(1,K6,IP), NX(1,K7,IP), NX(1,K8,IP),
     L   NX(1,K9,IP), NX(1,K10,IP),VOL(1,IP),   VOLDP(1,IP),
     M   NEL,         OFFG)
      ENDIF
      
      DO IP=1,NPT
        DO I=1,NEL
          VOLG(I) =VOLG(I) + VOL(I,IP)
        ENDDO
      ENDDO
C     
      DO I=1,NEL
        VOLN(I) =VOLG(I)/NPT
      ENDDO
C-----------
      RETURN
      END
