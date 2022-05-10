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
Chd|  S4VOLUME                      source/elements/solid/solide4/s4volume.F
Chd|-- called by -----------
Chd|        ELOFF                         source/elements/eloff.F       
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4VOLUME(X, VOL, NEL, NC1, NC2, NC3, NC4)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NC1(*), NC2(*), NC3(*), NC4(*)
C
      my_real
     .   X(3,*),VOL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C                                                                     12
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .   B1(MVSIZ), C1(MVSIZ), D1(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
C---------------------------------------------------
C     NODAL COORDINATES     
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
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
      ENDDO
C----------------------------
C     TETRAHEDRON VOLUME
C----------------------------
      DO I=1,NEL
        X43 = X4(I) - X3(I)
        Y43 = Y4(I) - Y3(I)
        Z43 = Z4(I) - Z3(I)
        X41 = X4(I) - X1(I)
        Y41 = Y4(I) - Y1(I)
        Z41 = Z4(I) - Z1(I)
        X42 = X4(I) - X2(I)
        Y42 = Y4(I) - Y2(I)
        Z42 = Z4(I) - Z2(I)
C
        B1(I) =  Y43*Z42 - Y42*Z43
        C1(I) =  Z43*X42 - Z42*X43
        D1(I) =  X43*Y42 - X42*Y43
C
        VOL(I)= (X41*B1(I) + Y41*C1(I) + Z41*D1(I))*UNSIX
      ENDDO
C
      RETURN
      END
