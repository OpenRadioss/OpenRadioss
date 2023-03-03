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
Chd|  SPPART3                       source/elements/sph/sppart3.F 
Chd|-- called by -----------
Chd|        SPINIT3                       source/elements/sph/spinit3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPPART3(MS,PARTSAV,NC1,MASS,X,V,IPART)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*),NC1(*)
      my_real
     .   MS(*),X(3,*),V(3,*),PARTSAV(20,*),MASS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,I1
C     REAL
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
C-----------------------------------------------------------------------
       DO I=LFT,LLT
        I1 = NC1(I)
        MS(I1)=MASS(I)+MS(I1)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + MASS(I)*X(1,I1)
        PARTSAV(3,IP)=PARTSAV(3,IP) + MASS(I)*X(2,I1)
        PARTSAV(4,IP)=PARTSAV(4,IP) + MASS(I)*X(3,I1)
        XX = X(1,I1)*X(1,I1)
        XY = X(1,I1)*X(2,I1)
        YY = X(2,I1)*X(2,I1)
        YZ = X(2,I1)*X(3,I1)
        ZZ = X(3,I1)*X(3,I1)
        ZX = X(3,I1)*X(1,I1)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - MASS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - MASS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*V(1,I1)
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*V(2,I1)
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*V(3,I1)
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
       ENDDO
C
      RETURN
      END
