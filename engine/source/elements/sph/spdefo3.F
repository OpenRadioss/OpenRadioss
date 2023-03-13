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
Chd|  SPDEFO3                       source/elements/sph/spdefo3.F 
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPDEFO3(
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, 
     .   D4, D5, D6,WXX, WYY, WZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), 
     .   D4(*), D5(*), D6(*),
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
C     REAL
      my_real
     .       DT1D2
C-----------------------------------------------
C
      DT1D2=.5*DT1

      DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
        D5(I)   = DYZ(I)+DZY(I)
        D6(I)   = DXZ(I)+DZX(I)
        WZZ(I)=DT1D2*(DYX(I)-DXY(I))
        WYY(I)=DT1D2*(DXZ(I)-DZX(I))
        WXX(I)=DT1D2*(DZY(I)-DYZ(I))
      ENDDO
C
      RETURN
      END
