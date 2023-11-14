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
Chd|  IDEGE8                        source/elements/solid/solide/idege8.F
Chd|-- called by -----------
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen_dege.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/sdlen_sms.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDEGE8(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AMAX,FAC,IT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,AMAX,FAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDE,IT
      my_real
     .   X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41,A,
     .   RX,RY,RZ,SX,SY,SZ,NX,NY,NZ
C-----------------------------------------------
       RX = X2+X3-X1-X4
       RY = Y2+Y3-Y1-Y4
       RZ = Z2+Z3-Z1-Z4
       SX = X3+X4-X1-X2
       SY = Y3+Y4-Y1-Y2
       SZ = Z3+Z4-Z1-Z2
       NX = RY * SZ - RZ * SY 
       NY = RZ * SX - RX * SZ 
       NZ = RX * SY - RY * SX 
C--------A: (2*AREA)^2-----       
       A = NX*NX+NY*NY+NZ*NZ
       IDE = 0
       X12 = X2 - X1
       Y12 = Y2 - Y1
       Z12 = Z2 - Z1
       IF (X12==ZERO.AND.Y12==ZERO.AND.Z12==ZERO) THEN
        IDE = 1
          GOTO 10
       ENDIF
C       
       X23 = X2 - X3
       Y23 = Y2 - Y3
       Z23 = Z2 - Z3
       IF (X23==ZERO.AND.Y23==ZERO.AND.Z23==ZERO) THEN
        IDE = 1
          GOTO 10
       ENDIF
C       
       X34 = X4 - X3
       Y34 = Y4 - Y3
       Z34 = Z4 - Z3
       IF (X34==ZERO.AND.Y34==ZERO.AND.Z34==ZERO) THEN
        IDE = 1
          GOTO 10
       ENDIF
C       
       X41 = X4 - X1
       Y41 = Y4 - Y1
       Z41 = Z4 - Z1
       IF (X41==ZERO.AND.Y41==ZERO.AND.Z41==ZERO) THEN
        IDE = 1
          GOTO 10
       ENDIF
C       
   10   CONTINUE
C     
       IF (A > AMAX )THEN
              IT=IDE
          AMAX = A
             END IF
      RETURN
      END
