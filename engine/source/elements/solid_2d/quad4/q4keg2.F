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
Chd|  Q4KEG2                        source/elements/solid_2d/quad4/q4keg2.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        Q4KEGIJ2                      source/elements/solid_2d/quad4/q4keg2.F
Chd|====================================================================
      SUBROUTINE Q4KEG2(
     1   PY1,     PY2,     PY3,     PY4,
     2   PZ1,     PZ2,     PZ3,     PZ4,
     3   AY,      K11,     K12,     K13,
     4   K14,     K22,     K23,     K24,
     5   K33,     K34,     K44,     SIG,
     6   AIR,     OFF,     NEL)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .    PY1(*), PY2(*), PY3(*), PY4(*), 
     .    PZ1(*), PZ2(*), PZ3(*), PZ4(*), AY(*), 
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*),
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*),
     .    SIG(6,*), AIR(*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .    FYY(MVSIZ),FZZ(MVSIZ),FXX(MVSIZ),FYZ(MVSIZ),AYS(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=1,NEL
        FYY(I)=SIG(1,I)*AIR(I)
        FZZ(I)=SIG(2,I)*AIR(I)
        FYZ(I)=SIG(4,I)*AIR(I)
      ENDDO
      IF (N2D==1) THEN
        DO I=1,NEL
          FXX(I)=SIG(3,I)*AIR(I)
          AYS(I)=AY(I)*AY(I)
        ENDDO
      ENDIF
C-----K11,K13,K33
      CALL Q4KEGIJ2(1, NEL, 
     .              PY1, PZ1, PY1, PZ1, AYS, K11, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY1, PZ1, PY3, PZ3, AYS, K13, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY3, PZ3, PY3, PZ3, AYS, K33, 
     .              FYY, FZZ, FXX, FYZ)
C-----K22,K24,K44
      CALL Q4KEGIJ2(1, NEL, 
     .              PY2, PZ2, PY2, PZ2, AYS, K22, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY2, PZ2, PY4, PZ4, AYS, K24, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY4, PZ4, PY4, PZ4, AYS, K44, 
     .              FYY, FZZ, FXX, FYZ)
C-----K12,K14,K34,K23
      CALL Q4KEGIJ2(1, NEL, 
     .              PY1, PZ1, PY2, PZ2, AYS, K12, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY1, PZ1, PY4, PZ4, AYS, K14, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY2, PZ2, PY3, PZ3, AYS, K23, 
     .              FYY, FZZ, FXX, FYZ)
      CALL Q4KEGIJ2(1, NEL, 
     .              PY3, PZ3, PY4, PZ4, AYS, K34, 
     .              FYY, FZZ, FXX, FYZ)
C
      RETURN
      END
