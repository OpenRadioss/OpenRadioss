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
C
Cgw119      SUBROUTINE I11GMX3(X,IRECT,I,GAPMAX)
Chd|====================================================================
Chd|  I11GMX3                       source/interfaces/inter3d1/i11gmx3.F
Chd|-- called by -----------
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11GMX3(X,IRECT,I,GAPMAX,XL2)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   GAPMAX
      INTEGER IRECT(2,*)
C     REAL
      my_real
     .   X(3,*),XL2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JPERM(4), JJ, N1, N2
C
      DATA JPERM/2,3,4,1/
C-----------------------------------------------------------------
C     LONGUEUR MINI DES COTES DE SEGMENT (CALCUL DU GAP MAX)
C-----------------------------------------------------------------
      N1=IRECT(1,I)
      N2=IRECT(2,I)
      XL2=(X(1,N1)-X(1,N2))**2
     .   +(X(2,N1)-X(2,N2))**2
     .   +(X(3,N1)-X(3,N2))**2
      IF(XL2.GT.ZERO)GAPMAX= MIN(GAPMAX,XL2)
C
      RETURN
      END
