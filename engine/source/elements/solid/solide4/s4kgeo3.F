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
Chd|  S4KGEO3                       source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|====================================================================
      SUBROUTINE S4KGEO3(
     1   SIG,     VOL,     PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     K11,     K12,
     5   K13,     K14,     K22,     K23,
     6   K24,     K33,     K34,     K44,
     7   NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*) ,
     .   K22(3,3,*) ,K23(3,3,*)  ,K24(3,3,*),
     .   K33(3,3,*),K34(3,3,*),
     .   K44(3,3,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ)
C-----------------------------------------------
       DO I=1,NEL
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C -----------------------------------------------
      CALL S4KEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K11 )
      CALL S4KEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K22 )
      CALL S4KEH3(1,NEL,
     .   PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K33 )
      CALL S4KEH3(1,NEL,
     .   PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K44 )
      CALL S4KEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K12 )
      CALL S4KEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K13 )
      CALL S4KEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K14 )
      CALL S4KEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K23 )
      CALL S4KEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K24 )
      CALL S4KEH3(1,NEL,
     .   PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K34 )
C
      RETURN
      END
