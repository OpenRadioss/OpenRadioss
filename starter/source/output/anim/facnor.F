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
Chd|  FACNOR                        source/output/anim/facnor.F   
Chd|-- called by -----------
Chd|        PARSOR                        source/output/anim/parsor.F   
Chd|        PARSORC                       source/output/anim/parsorc.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FACNOR(X,D,II,XNORM,CDG,INVERT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(3,*),D(3,*),XNORM(3,*),CDG(*)
      INTEGER II(*),INVERT
C     REAL
      my_real
     .   X0(4),Y0(4),Z0(4),NX,NY,NZ,XC,YC,ZC,S
      INTEGER I,I1,I2,I3,I4
C-----------------------------------------------
C       FACE NORMAL
C-----------------------------------------------
      DO 100 I = 1,4
            X0(I) = X(1,II(I))-D(1,II(I))
            Y0(I) = X(2,II(I))-D(2,II(I))
            Z0(I) = X(3,II(I))-D(3,II(I))
 100  CONTINUE
C
      NX = (Y0(3) - Y0(1)) * (Z0(4) - Z0(2)) -
     .     (Z0(3) - Z0(1)) * (Y0(4) - Y0(2))
      NY = (Z0(3) - Z0(1)) * (X0(4) - X0(2)) -
     .     (X0(3) - X0(1)) * (Z0(4) - Z0(2))
      NZ = (X0(3) - X0(1)) * (Y0(4) - Y0(2)) -
     .     (Y0(3) - Y0(1)) * (X0(4) - X0(2))
C
      XC = .25*(X0(1) + X0(2) + X0(3) + X0(4))
      YC = .25*(Y0(1) + Y0(2) + Y0(3) + Y0(4))
      ZC = .25*(Z0(1) + Z0(2) + Z0(3) + Z0(4))
      XC = XC - CDG(1)
      YC = YC - CDG(2)
      ZC = ZC - CDG(3)
      S  = NX * XC + NY * YC + NZ * ZC
      INVERT = 1
      IF(S<ZERO)THEN
             INVERT = -1
             I1    = II(2)
             I2    = II(1)
             I3    = II(4)
             I4    = II(3)
             II(1) = I1
             II(2) = I2
             II(3) = I3
             II(4) = I4
      ENDIF
C
      DO 200 I = 1,4
            X0(I) = X(1,II(I))
            Y0(I) = X(2,II(I))
            Z0(I) = X(3,II(I))
 200  CONTINUE
C
      NX = (Y0(3) - Y0(1)) * (Z0(4) - Z0(2)) -
     .     (Z0(3) - Z0(1)) * (Y0(4) - Y0(2))
      NY = (Z0(3) - Z0(1)) * (X0(4) - X0(2)) -
     .     (X0(3) - X0(1)) * (Z0(4) - Z0(2))
      NZ = (X0(3) - X0(1)) * (Y0(4) - Y0(2)) -
     .     (Y0(3) - Y0(1)) * (X0(4) - X0(2))
C
      DO 300 I = 1,4
          XNORM(1,II(I)) = XNORM(1,II(I)) + NX
          XNORM(2,II(I)) = XNORM(2,II(I)) + NY
          XNORM(3,II(I)) = XNORM(3,II(I)) + NZ
 300  CONTINUE
C
C-----------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  FACNOR2                       source/output/anim/facnor.F   
Chd|-- called by -----------
Chd|        PARSOR                        source/output/anim/parsor.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FACNOR2(X,II,XNORM,INVERT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(3,*),XNORM(3,*)
      INTEGER II(*),INVERT
C     REAL
      my_real
     .   X0(4),Y0(4),Z0(4),NX,NY,NZ
      INTEGER I
C-----------------------------------------------
C       FACE NORMAL
C-----------------------------------------------
C
      INVERT = 1
C
      DO 200 I = 1,4
            X0(I) = X(1,II(I))
            Y0(I) = X(2,II(I))
            Z0(I) = X(3,II(I))
 200  CONTINUE
C
      NX = (Y0(3) - Y0(1)) * (Z0(4) - Z0(2)) -
     .     (Z0(3) - Z0(1)) * (Y0(4) - Y0(2))
      NY = (Z0(3) - Z0(1)) * (X0(4) - X0(2)) -
     .     (X0(3) - X0(1)) * (Z0(4) - Z0(2))
      NZ = (X0(3) - X0(1)) * (Y0(4) - Y0(2)) -
     .     (Y0(3) - Y0(1)) * (X0(4) - X0(2))
C
      DO 300 I = 1,4
          XNORM(1,II(I)) = XNORM(1,II(I)) + NX
          XNORM(2,II(I)) = XNORM(2,II(I)) + NY
          XNORM(3,II(I)) = XNORM(3,II(I)) + NZ
 300  CONTINUE
C
C-----------------------------------------------
      RETURN
      END
