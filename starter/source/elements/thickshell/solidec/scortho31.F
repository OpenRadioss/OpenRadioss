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
Chd|  SCORTHO31                     source/elements/thickshell/solidec/scortho31.F
Chd|-- called by -----------
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCORTHO31(
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,  
     .   E1X, E1Y, E1Z,
     .   E2X, E2Y, E2Z,
     .   E3X, E3Y, E3Z
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C                                                                     12
      my_real
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   RX , RY , RZ ,
     .   TX , TY , TZ ,
     .   DET,C1,C2, sign,
     .   SX , SY , SZ ,
     .   RSX , RSY , RSZ
C-----------------------------------------------
      X17=X7-X1
      X28=X8-X2
      X35=X5-X3
      X46=X6-X4
      Y17=Y7-Y1
      Y28=Y8-Y2
      Y35=Y5-Y3
      Y46=Y6-Y4
      Z17=Z7-Z1
      Z28=Z8-Z2
      Z35=Z5-Z3
      Z46=Z6-Z4
C
      RX=X17+X28-X35-X46
      RY=Y17+Y28-Y35-Y46
      RZ=Z17+Z28-Z35-Z46
      A17=X17+X46
      A28=X28+X35
      B17=Y17+Y46
      B28=Y28+Y35
      C17=Z17+Z46
      C28=Z28+Z35
C
      TX=A17-A28
      TY=B17-B28
      TZ=C17-C28
C
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
       E3X = TY * RZ - TZ * RY 
       E3Y = TZ * RX - TX * RZ 
       E3Z = TX * RY - TY * RX 
C
       DET = SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
       IF ( DET.NE.ZERO) DET = ONE / DET
       E3X = E3X * DET
       E3Y = E3Y * DET
       E3Z = E3Z * DET
C
       C1=SQRT(TX*TX+TY*TY+TZ*TZ)
       C2=SQRT(RX*RX+RY*RY+RZ*RZ)
       E1X=TX*C2 +(RY * E3Z - RZ * E3Y)*C1  
       E1Y=TY*C2 +(RZ * E3X - RX * E3Z)*C1  
       E1Z=TZ*C2 +(RX * E3Y - RY * E3X)*C1
       DET = SQRT(E1X*E1X + E1Y*E1Y + E1Z*E1Z)
       IF ( DET.NE.ZERO) DET = ONE / DET
       E1X = E1X*DET
       E1Y = E1Y*DET
       E1Z = E1Z*DET
C
       E2X = E3Y * E1Z - E3Z * E1Y
       E2Y = E3Z * E1X - E3X * E1Z
       E2Z = E3X * E1Y - E3Y * E1X
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
