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
Chd|  VOISIN1                       source/interfaces/inter3d1/voisin1.F
Chd|-- called by -----------
Chd|        REMOVEIC1                     source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VOISIN1(NC1,NC2 ,I1 ,I2 ,INV)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NC1(*),NC2(*),INV,I1 ,I2
C-----------------------------------------------
c FUNCTION: find the same orientation of two segments (4n)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NC1(1:4),NC2(1:4) - connectivity
c  I   I1,I2             - commun nodes of two seg
c  O   INV               - flag : 0 same orientation; 1 inverse one
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2
C----------change the method, just look at the connectivity  
        J1=0
        DO I= 1,4
         IF (I1==NC2(I)) J1=I
        END DO
        IF (J1==4) J1=0
        IF (NC2(J1+1)==I2) THEN
          INV = 1
        ELSE
          INV = 0
        END IF 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
