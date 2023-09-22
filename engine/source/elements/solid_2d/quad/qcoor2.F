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
Chd|  QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QCOOR2(
     1   X,     NCP,  Y1,   Y2,
     2   Y3,    Y4,   Z1,   Z2,
     3   Z3,    Z4,   NC1,  NC2,
     4   NC3,   NC4,  NGL,  MAT,
     5   NGEO,  VD2,  VIS,  NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), MAT(*), NGL(*), NGEO(*),NCP(7,*)
      my_real X(3,*),Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),VD2(*),VIS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NEL
        NGEO(I)=NCP(6,I)
        NGL(I)=NCP(7,I)
        MAT(I)=NCP(1,I)
        NC1(I)=NCP(2,I)
        NC2(I)=NCP(3,I)
        NC3(I)=NCP(4,I)
        NC4(I)=NCP(5,I)
      ENDDO
C----------------------------
C     NODE COORDINATES
C----------------------------
      DO I=1,NEL
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
      END DO
C----------------------------
      DO I=1,NEL
        VD2(I)=ZERO
        VIS(I)=ZERO
      ENDDO
C----------------------------
      RETURN
      END
