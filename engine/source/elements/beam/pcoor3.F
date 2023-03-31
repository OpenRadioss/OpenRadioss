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
Chd|  PCOOR3                        source/elements/beam/pcoor3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PCOOR3(
     1   X,       NCC,     MAT,     PID,
     2   NGL,     NC1,     NC2,     NC3,
     3   X1,      X2,      X3,      Y1,
     4   Y2,      Y3,      Z1,      Z2,
     5   Z3,      NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NCC(6,*),NGL(MVSIZ),MAT(MVSIZ),PID(MVSIZ),
     .   NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
      my_real
     .   X(3,*)
      my_real
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .   Y3(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,NEL
        MAT(I)=NCC(1,I)
        PID(I)=NCC(5,I)
        NGL(I)=NCC(6,I)
        NC1(I)=NCC(2,I)
        NC2(I)=NCC(3,I)
        NC3(I)=NCC(4,I)
      ENDDO
C----------------------------
C     COORDONNEES
C----------------------------
      DO I=1,NEL
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
      ENDDO
C----------------------------
      RETURN
      END
