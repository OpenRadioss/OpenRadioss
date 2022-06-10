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
Chd|  SCOOR431                      source/elements/solid/sconnect/scoor431.F
Chd|-- called by -----------
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        THSOL                         source/output/th/thsol.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCOOR431(
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,  
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   P1X,P2X,P3X, P4X,P1Y,P2Y,P3Y,P4Y,P1Z,P2Z,P3Z, P4Z,
     .   RX,RY,RZ,SX,SY,SZ, CC,C1,C2
C-----------------------------------------------
      P1X = X1+X5    
      P1Y = Y1+Y5    
      P1Z = Z1+Z5    
      P2X = X2+X6    
      P2Y = Y2+Y6    
      P2Z = Z2+Z6    
      P3X = X3+X7    
      P3Y = Y3+Y7    
      P3Z = Z3+Z7    
      P4X = X4+X8    
      P4Y = Y4+Y8    
      P4Z = Z4+Z8    
C
      RX  = P2X + P3X - P1X - P4X  
      RY  = P2Y + P3Y - P1Y - P4Y  
      RZ  = P2Z + P3Z - P1Z - P4Z  
      SX  = P3X + P4X - P1X - P2X  
      SY  = P3Y + P4Y - P1Y - P2Y  
      SZ  = P3Z + P4Z - P1Z - P2Z  
C
      E3X = RY * SZ - RZ * SY                 
      E3Y = RZ * SX - RX * SZ                 
      E3Z = RX * SY - RY * SX                 
      CC  = ONE / MAX(EM20,SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z))                      
      E3X = E3X * CC                          
      E3Y = E3Y * CC                          
      E3Z = E3Z * CC                          
C
      C1 = RX*RX + RY*RY + RZ*RZ            
      C2 = SX*SX + SY*SY + SZ*SZ            
      IF (C1 /= ZERO) THEN                      
        CC  = SQRT(C2/C1)                  
        E1X = RX*CC+(SY*E3Z-SZ*E3Y)           
        E1Y = RY*CC+(SZ*E3X-SX*E3Z)           
        E1Z = RZ*CC+(SX*E3Y-SY*E3X)           
      ELSEIF (C2 /= ZERO) THEN                   
        CC  = SQRT(C1/C2)                  
        E1X = RX+(SY*E3Z-SZ*E3Y)*CC           
        E1Y = RY+(SZ*E3X-SX*E3Z)*CC           
        E1Z = RZ+(SX*E3Y-SY*E3X)*CC           
      END IF                                  
C
      CC = SQRT(E1X*E1X + E1Y*E1Y + E1Z*E1Z)  
      IF (CC /= ZERO) CC = ONE / CC            
      E1X = E1X*CC                            
      E1Y = E1Y*CC                            
      E1Z = E1Z*CC                            
      E2X = E3Y * E1Z - E3Z * E1Y             
      E2Y = E3Z * E1X - E3X * E1Z             
      E2Z = E3X * E1Y - E3Y * E1X             
C-----------
      RETURN
      END
