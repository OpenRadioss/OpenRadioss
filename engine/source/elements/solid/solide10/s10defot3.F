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
Chd|  S10DEFOT3                     source/elements/solid/solide10/s10defot3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S10VOLNODT3                   source/elements/solid/solide4_sfem/s10volnodt3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10DEFOT3(
     1   PX,      PY,      PZ,      VX,
     2   VY,      VZ,      DXX,     DXY,
     3   DXZ,     DYX,     DYY,     DYZ,
     4   DZX,     DZY,     DZZ,     NEL)
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
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N
C     REAL
C-----------------------------------------------
C   


        DO I=1,NEL
       DXX(I)=PX(I,1)*VX(I,1)+PX(I,2)*VX(I,2)+PX(I,3)*VX(I,3)+PX(I,4)*VX(I,4)+PX(I,5)*VX(I,5)
     .         +PX(I,6)*VX(I,6)+PX(I,7)*VX(I,7)+PX(I,8)*VX(I,8)+PX(I,9)*VX(I,9)+PX(I,10)*VX(I,10)
       DYY(I)=PY(I,1)*VY(I,1)+PY(I,2)*VY(I,2)+PY(I,3)*VY(I,3)+PY(I,4)*VY(I,4)+PY(I,5)*VY(I,5)
     .         +PY(I,6)*VY(I,6)+PY(I,7)*VY(I,7)+PY(I,8)*VY(I,8)+PY(I,9)*VY(I,9)+PY(I,10)*VY(I,10)
       DZZ(I)=PZ(I,1)*VZ(I,1)+PZ(I,2)*VZ(I,2)+PZ(I,3)*VZ(I,3)+PZ(I,4)*VZ(I,4)+PZ(I,5)*VZ(I,5)
     .         +PZ(I,6)*VZ(I,6)+PZ(I,7)*VZ(I,7)+PZ(I,8)*VZ(I,8)+PZ(I,9)*VZ(I,9)+PZ(I,10)*VZ(I,10)
       DXY(I)=PY(I,1)*VX(I,1)+PY(I,2)*VX(I,2)+PY(I,3)*VX(I,3)+PY(I,4)*VX(I,4)+PY(I,5)*VX(I,5)
     .         +PY(I,6)*VX(I,6)+PY(I,7)*VX(I,7)+PY(I,8)*VX(I,8)+PY(I,9)*VX(I,9)+PY(I,10)*VX(I,10)
       DXZ(I)=PZ(I,1)*VX(I,1)+PZ(I,2)*VX(I,2)+PZ(I,3)*VX(I,3)+PZ(I,4)*VX(I,4)+PZ(I,5)*VX(I,5)
     .         +PZ(I,6)*VX(I,6)+PZ(I,7)*VX(I,7)+PZ(I,8)*VX(I,8)+PZ(I,9)*VX(I,9)+PZ(I,10)*VX(I,10)
       DYX(I)=PX(I,1)*VY(I,1)+PX(I,2)*VY(I,2)+PX(I,3)*VY(I,3)+PX(I,4)*VY(I,4)+PX(I,5)*VY(I,5)
     .         +PX(I,6)*VY(I,6)+PX(I,7)*VY(I,7)+PX(I,8)*VY(I,8)+PX(I,9)*VY(I,9)+PX(I,10)*VY(I,10)
       DYZ(I)=PZ(I,1)*VY(I,1)+PZ(I,2)*VY(I,2)+PZ(I,3)*VY(I,3)+PZ(I,4)*VY(I,4)+PZ(I,5)*VY(I,5)
     .         +PZ(I,6)*VY(I,6)+PZ(I,7)*VY(I,7)+PZ(I,8)*VY(I,8)+PZ(I,9)*VY(I,9)+PZ(I,10)*VY(I,10)
       DZX(I)=PX(I,1)*VZ(I,1)+PX(I,2)*VZ(I,2)+PX(I,3)*VZ(I,3)+PX(I,4)*VZ(I,4)+PX(I,5)*VZ(I,5)
     .         +PX(I,6)*VZ(I,6)+PX(I,7)*VZ(I,7)+PX(I,8)*VZ(I,8)+PX(I,9)*VZ(I,9)+PX(I,10)*VZ(I,10)
       DZY(I)=PY(I,1)*VZ(I,1)+PY(I,2)*VZ(I,2)+PY(I,3)*VZ(I,3)+PY(I,4)*VZ(I,4)+PY(I,5)*VZ(I,5)
     .         +PY(I,6)*VZ(I,6)+PY(I,7)*VZ(I,7)+PY(I,8)*VZ(I,8)+PY(I,9)*VZ(I,9)+PY(I,10)*VZ(I,10)
      ENDDO
C
      RETURN
C
      END
