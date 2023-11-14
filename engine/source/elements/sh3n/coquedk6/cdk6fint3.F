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
          SUBROUTINE CDK6FINT3(JFT,JLT,VOL,THK0,FOR,MOM,
     1                      PX2,PY2,PX3,PY3,PB1,PB2,PB3,
     2                      F11,F12,F13,F21,F22,F23,F31,F32,F33,
     3                      F14,F15,F16,F24,F25,F26,F34,F35,F36,
     4                      N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     5                      NEL)
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NEL
      MY_REAL 
     .   FOR(NEL,5),MOM(NEL,3), 
     .   PX2(*), PY2(*), PX3(*), PY3(*), 
     .   PB1(MVSIZ,3,3),PB2(MVSIZ,3,3),PB3(MVSIZ,3,6),
     .   F11(*), F12(*), F13(*),
     .   F21(*), F22(*), F23(*), F31(*), F32(*), F33(*),
     .   N4X(*), N4Y(*), N4Z(*),N5X(*), N5Y(*), N5Z(*),
     .   N6X(*), N6Y(*), N6Z(*),
     .   F14(*), F15(*), F16(*),F24(*), F25(*), F26(*),
     .   F34(*), F35(*), F36(*),THK0(*),VOL(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I
      MY_REAL 
     .    C2,FXX,FYY,FXY,FZ4,FZ5,FZ6,MXX,MYY,MXY,FXN,FYN,FXYN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      DO I=JFT,JLT
        F12(I) =VOL(I)*(PX2(I)*FOR(I,1)+PY2(I)*FOR(I,3))
        F22(I) =VOL(I)*(PY2(I)*FOR(I,2)+PX2(I)*FOR(I,3))
        F13(I) =VOL(I)*(PX3(I)*FOR(I,1)+PY3(I)*FOR(I,3))
        F23(I) =VOL(I)*(PY3(I)*FOR(I,2)+PX3(I)*FOR(I,3))
        F11(I)=-F12(I)-F13(I)
        F21(I)=-F22(I)-F23(I)
      ENDDO     
      DO I=JFT,JLT
          C2=VOL(I)*THK0(I)
        MXX=C2*MOM(I,1)
        MYY=C2*MOM(I,2)
        MXY=C2*MOM(I,3)
        F31(I) = PB1(I,1,1)*MXX+PB1(I,2,1)*MYY+PB1(I,3,1)*MXY
        F32(I) = PB1(I,1,2)*MXX+PB1(I,2,2)*MYY+PB1(I,3,2)*MXY
        F33(I) = PB1(I,1,3)*MXX+PB1(I,2,3)*MYY+PB1(I,3,3)*MXY
C
        FZ4    = PB2(I,1,1)*MXX+PB2(I,2,1)*MYY+PB2(I,3,1)*MXY
        F14(I) = FZ4*N4X(I)
        F24(I) = FZ4*N4Y(I)
        F34(I) = FZ4*N4Z(I)
        FZ5    = PB2(I,1,2)*MXX+PB2(I,2,2)*MYY+PB2(I,3,2)*MXY
        F15(I) = FZ5*N5X(I)
        F25(I) = FZ5*N5Y(I)
        F35(I) = FZ5*N5Z(I)
        FZ6    = PB2(I,1,3)*MXX+PB2(I,2,3)*MYY+PB2(I,3,3)*MXY
        F16(I) = FZ6*N6X(I)
        F26(I) = FZ6*N6Y(I)
        F36(I) = FZ6*N6Z(I)
C
        FXX    = PB3(I,1,1)*MXX + PB3(I,2,1)*MYY + PB3(I,3,1)*MXY
        FYY    = PB3(I,1,2)*MXX + PB3(I,2,2)*MYY + PB3(I,3,2)*MXY
        FXY    = PB3(I,1,3)*MXX + PB3(I,2,3)*MYY + PB3(I,3,3)*MXY
        FXN  = N5X(I)*FXX
        FYN  = N6X(I)*FYY
        FXYN = N4X(I)*FXY
        F11(I) = F11(I) + PB3(I,1,4)*FXN+PB3(I,2,4)*FYN+PB3(I,3,4)*FXYN
        F12(I) = F12(I) + PB3(I,1,5)*FXN+PB3(I,2,5)*FYN+PB3(I,3,5)*FXYN
        F13(I) = F13(I) + PB3(I,1,6)*FXN+PB3(I,2,6)*FYN+PB3(I,3,6)*FXYN
        FXN  = N5Y(I)*FXX
        FYN  = N6Y(I)*FYY
        FXYN = N4Y(I)*FXY
        F21(I) = F21(I) + PB3(I,1,4)*FXN+PB3(I,2,4)*FYN+PB3(I,3,4)*FXYN
        F22(I) = F22(I) + PB3(I,1,5)*FXN+PB3(I,2,5)*FYN+PB3(I,3,5)*FXYN
        F23(I) = F23(I) + PB3(I,1,6)*FXN+PB3(I,2,6)*FYN+PB3(I,3,6)*FXYN
        FXN  = N5Z(I)*FXX
        FYN  = N6Z(I)*FYY
        FXYN = N4Z(I)*FXY
        F31(I) = F31(I) + PB3(I,1,4)*FXN+PB3(I,2,4)*FYN+PB3(I,3,4)*FXYN
        F32(I) = F32(I) + PB3(I,1,5)*FXN+PB3(I,2,5)*FYN+PB3(I,3,5)*FXYN
        F33(I) = F33(I) + PB3(I,1,6)*FXN+PB3(I,2,6)*FYN+PB3(I,3,6)*FXYN
      ENDDO     
C    
      RETURN
      END
