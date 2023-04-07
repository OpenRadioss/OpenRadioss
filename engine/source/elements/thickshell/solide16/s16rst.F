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
Chd|  S16RST                        source/elements/thickshell/solide16/s16deri3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S16RST(R,S,T,
     2   NI   ,DNIDR ,DNIDS ,DNIDT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     1   R,S,T,
     2   NI(16) ,DNIDR(16),DNIDS(16),DNIDT(16)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A
C-----------------------------------------------
C
C
C
C                            ^ S       _ T
C                            |         /|
C                            |        /
C                   7        | 14    /
C                    O-------|--O----------O 6
C                   /.       |     /      /|
C                  / .       |           / |
C              15 O  .           /   13 O  |
C                /   .       |         /   |
C               /    .    16   /    5 /    |
C            8 O----------O----------O     |
C     R <------|- - -.- - - -+  10   |     |
C              |     O..........O....|.....O 2
C              |    . 3              |    / 
C              |   .                 |   /    
C              |  O 11               |  O     
C              | .                   | / 9    
C              |.                    |/    
C              O----------O----------O     
C             4          12           1
C
C
C
C
C-----------------------------------------------
C ro = r ri    so = s si    to = t ti
C
C i=1,8  
C    ri=+-1  si=+-1 ti=+-1
C    Ni = 1/8 (1+ro)(1+so)(1+to)(ro+to-1)
C    dNi/dr = ri/8 (1+so)(1+to)(2ro+to)
C    dNi/ds = si/8 (1+to)(1+ro)(to+ro-1)
C    dNi/dt = ti/8 (1+ro)(1+so)(2to+ro)
C
C i=10;12;14;16
C    ri=0    si=+-1 ti=+-1
C    Ni = 1/4 (1-r^2)(1+so)(1+to)
C    dNi/dr = -r/2 (1+so)(1+to)
C    dNi/ds = si/4 (1-r^2)(1+to)
C    dNi/dt = ti/4 (1-r^2)(1+so)
C
C
C i=9;11;13;15
C    ri=+-1  si=+-1 ti=0
C    Ni = 1/4 (1-t^2)(1+ro)(1+so)
C    dNi/dr = ri/4 (1-t^2)(1+so)
C    dNi/ds = si/4 (1-t^2)(1+ro)
C    dNi/dt = -t/2 (1+ro)(1+so)
C-----------------------------------------------
C  i    ri   si   ti       Ni                                  
C--------------------------------------------------------------------
C  1   -1   -1   -1     1/8(1-r)(1-s)(1-t)(-r-t-1)
C  2   -1   -1   +1     1/8(1-r)(1-s)(1+t)(-r+t-1)
C  3   +1   -1   +1     1/8(1+r)(1-s)(1+t)(+r+t-1)
C  4   +1   -1   -1     1/8(1+r)(1-s)(1-t)(+r-t-1)
C  5   -1   +1   -1     1/8(1-r)(1+s)(1-t)(-r-t-1)
C  6   -1   +1   +1     1/8(1-r)(1+s)(1+t)(-r+t-1)
C  7   +1   +1   +1     1/8(1+r)(1+s)(1+t)(+r+t-1)    
C  8   +1   +1   -1     1/8(1+r)(1+s)(1-t)(+r-t-1)
C  9   -1   -1    0     1/4(1-t^2)(1-r)(1-s)
C 10    0   -1   +1     1/4(1-r^2)     (1-s)(1+t)
C 11   +1   -1    0     1/4(1-t^2)(1+r)(1-s)
C 12    0   -1   -1     1/4(1-r^2)     (1-s)(1-t)
C 13   -1   +1    0     1/4(1-t^2)(1-r)(1+s)
C 14    0   +1   +1     1/4(1-r^2)     (1+s)(1+t)
C 15   +1   +1    0     1/4(1-t^2)(1+r)(1+s)
C 16    0   +1   -1     1/4(1-r^2)     (1+s)(1-t)
C-----------------------------------------------
C  i    ri   si   ti       dNi/dr                  dNi/ds                               
C--------------------------------------------------------------------
C  1   -1   -1   -1     -1/8(1-s)(1-t)(-2r-t)     -1/8 (1-t)(1-r)(-t-r-1)
C  2   -1   -1   +1     -1/8(1-s)(1+t)(-2r+t)     -1/8 (1+t)(1-r)( t-r-1)
C  3   +1   -1   +1      1/8(1-s)(1+t)(+2r+t)     -1/8 (1+t)(1+r)( t+r-1)
C  4   +1   -1   -1      1/8(1-s)(1-t)(+2r-t)     -1/8 (1-t)(1+r)(-t+r-1)
C  5   -1   +1   -1     -1/8(1+s)(1-t)(-2r-t)      1/8 (1-t)(1-r)(-t-r-1)
C  6   -1   +1   +1     -1/8(1+s)(1+t)(-2r+t)      1/8 (1+t)(1-r)( t-r-1)
C  7   +1   +1   +1      1/8(1+s)(1+t)(+2r+t)      1/8 (1+t)(1+r)( t+r-1)   
C  8   +1   +1   -1      1/8(1+s)(1-t)(+2r-t)      1/8 (1-t)(1+r)(-t+r-1)
C  9   -1   -1    0     -1/4 (1-t^2)(1-s)         -1/4 (1-t^2)(1-r)
C 10    0   -1   +1     -1/2 r(1-s)(1+t)          -1/4 (1-r^2)(1+t)
C 11   +1   -1    0      1/4 (1-t^2)(1-s)         -1/4 (1-t^2)(1+r)
C 12    0   -1   -1     -1/2 r(1-s)(1-t)          -1/4 (1-r^2)(1-t)
C 13   -1   +1    0     -1/4 (1-t^2)(1+s)          1/4 (1-t^2)(1-r)
C 14    0   +1   +1     -1/2 r(1+s)(1+t)           1/4 (1-r^2)(1+t)
C 15   +1   +1    0      1/4 (1-t^2)(1+s)          1/4 (1-t^2)(1+r)
C 16    0   +1   -1     -1/2 r(1+s)(1-t)           1/4 (1-r^2)(1-t)
C-----------------------------------------------
C
      U_M_R = HALF*(ONE - R)
      U_P_R = HALF*(ONE + R)
C
      U_M_S = HALF*(ONE - S)
      U_P_S = HALF*(ONE + S)
C
      U_M_T = HALF*(ONE - T)
      U_P_T = HALF*(ONE + T)
C
      UMS_UMT = U_M_S * U_M_T
      UMS_UPT = U_M_S * U_P_T
      UPS_UMT = U_P_S * U_M_T
      UPS_UPT = U_P_S * U_P_T
C
      UMR_UMS = U_M_R * U_M_S
      UMR_UPS = U_M_R * U_P_S
      UPR_UMS = U_P_R * U_M_S
      UPR_UPS = U_P_R * U_P_S
C
      UMT_UMR = U_M_T * U_M_R
      UMT_UPR = U_M_T * U_P_R
      UPT_UMR = U_P_T * U_M_R
      UPT_UPR = U_P_T * U_P_R
C
      NI(1) = U_M_R * UMS_UMT * (-R-T-ONE)
      NI(2) = U_M_R * UMS_UPT * (-R+T-ONE)
      NI(3) = U_P_R * UMS_UPT * ( R+T-ONE)
      NI(4) = U_P_R * UMS_UMT * ( R-T-ONE)
      NI(5) = U_M_R * UPS_UMT * (-R-T-ONE)
      NI(6) = U_M_R * UPS_UPT * (-R+T-ONE)
      NI(7) = U_P_R * UPS_UPT * ( R+T-ONE)
      NI(8) = U_P_R * UPS_UMT * ( R-T-ONE)     
C
      DNIDR(1) = -UMS_UMT * (-HALF*T - R) 
      DNIDR(2) = -UMS_UPT * ( HALF*T - R)
      DNIDR(3) =  UMS_UPT * ( HALF*T + R)
      DNIDR(4) =  UMS_UMT * (-HALF*T + R)
      DNIDR(5) = -UPS_UMT * (-HALF*T - R)
      DNIDR(6) = -UPS_UPT * ( HALF*T - R)
      DNIDR(7) =  UPS_UPT * ( HALF*T + R)
      DNIDR(8) =  UPS_UMT * (-HALF*T + R)      
C
      DNIDS(1) = -UMT_UMR * (-R-T-ONE)*HALF
      DNIDS(2) = -UPT_UMR * (-R+T-ONE)*HALF
      DNIDS(3) = -UPT_UPR * ( R+T-ONE)*HALF
      DNIDS(4) = -UMT_UPR * ( R-T-ONE)*HALF
      DNIDS(5) =  UMT_UMR * (-R-T-ONE)*HALF
      DNIDS(6) =  UPT_UMR * (-R+T-ONE)*HALF
      DNIDS(7) =  UPT_UPR * ( R+T-ONE)*HALF
      DNIDS(8) =  UMT_UPR * ( R-T-ONE)*HALF      
C
      DNIDT(1) = -UMR_UMS * (-HALF*R - T)
      DNIDT(2) =  UMR_UMS * (-HALF*R + T)
      DNIDT(3) =  UPR_UMS * (+HALF*R + T)
      DNIDT(4) = -UPR_UMS * (+HALF*R - T)
      DNIDT(5) = -UMR_UPS * (-HALF*R - T)
      DNIDT(6) =  UMR_UPS * (-HALF*R + T)
      DNIDT(7) =  UPR_UPS * (+HALF*R + T)
      DNIDT(8) = -UPR_UPS * (+HALF*R - T)      
C------------------------------------
      A = (ONE - R*R)
      NI(10) = A * UMS_UPT
      NI(12) = A * UMS_UMT
      NI(14) = A * UPS_UPT
      NI(16) = A * UPS_UMT
C    
      A = HALF*A
      DNIDT(10) =  A * U_M_S
      DNIDT(14) =  A * U_P_S
      DNIDS(10) = -A * U_P_T
      DNIDS(12) = -A * U_M_T
C    
      A = -TWO*R
      DNIDR(10) = A * UMS_UPT
      DNIDR(12) = A * UMS_UMT
      DNIDR(14) = A * UPS_UPT
      DNIDR(16) = A * UPS_UMT
C------------------------------------
      A = (ONE - T*T)
      NI(9)  = A * UMR_UMS
      NI(11) = A * UPR_UMS
      NI(13) = A * UMR_UPS
      NI(15) = A * UPR_UPS
C
      A = HALF*A
C      DNIDR(11) =  A * U_M_S
C      DNIDR(15) =  A * U_P_S
C      DNIDR(9)  = -DNIDR(11)
C      DNIDR(13) = -DNIDR(15)
      DNIDR(9)  = -A * U_M_S
      DNIDR(13) = -A * U_P_S
C
C      DNIDS(13) =  A * U_M_R
C      DNIDS(15) =  A * U_P_R
C      DNIDS(9)  = -DNIDS(13)
C      DNIDS(11) = -DNIDS(15)
      DNIDS(9)  = -A * U_M_R
      DNIDS(11) = -A * U_P_R
C
      A = -TWO*T
      DNIDT(9)  = A * UMR_UMS
      DNIDT(11) = A * UPR_UMS
      DNIDT(13) = A * UMR_UPS
      DNIDT(15) = A * UPR_UPS
C
C-----------------------------------------------
      RETURN
      END
