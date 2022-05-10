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
Chd|  S4DEFO3                       source/elements/solid/solide4/s4defo3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4DEFO3(
     .   PX1, PX2, PX3, PX4, PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, VX1, VX2, VX3, VX4,
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY,
     .   DZZ, D4, D5, D6, WXX, WYY, WZZ)
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
C     REAL ou REAL*8
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C     REAL ou REAL*8
      my_real
     .   DT1D2
C-----------------------------------------------
C                                                                     12
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX1(I)+PX2(I)*VX2(I)+
     .         PX3(I)*VX3(I)+PX4(I)*VX4(I)
        DYY(I)=PY1(I)*VY1(I)+PY2(I)*VY2(I)+
     .         PY3(I)*VY3(I)+PY4(I)*VY4(I)
        DZZ(I)=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+
     .         PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
        DXY(I)=PY1(I)*VX1(I)+PY2(I)*VX2(I)+
     .         PY3(I)*VX3(I)+PY4(I)*VX4(I)
        DXZ(I)=PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+
     .         PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
        DYX(I)=PX1(I)*VY1(I)+PX2(I)*VY2(I)+
     .         PX3(I)*VY3(I)+PX4(I)*VY4(I)
        DYZ(I)=PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+
     .         PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
        DZX(I)=PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+
     .         PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
        DZY(I)=PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+
     .         PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
      ENDDO
C
      DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
        D5(I)   = DYZ(I)+DZY(I)
        D6(I)   = DXZ(I)+DZX(I)
        WXX(I)  = ZERO
        WYY(I)  = ZERO
        WZZ(I)  = ZERO
      ENDDO
C
      RETURN
C
      END
