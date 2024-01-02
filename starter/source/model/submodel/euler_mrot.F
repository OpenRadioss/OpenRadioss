Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C   This function computes a 3d rotation matrix.
C   The rotation is about the axis <rx,ry,rz> .
C   The rotation angle [sqrt(rx**2+ry**2+rz**2)] is specified in degrees. 
C   These rotation is counter-clockwise looking from the positive direction.
Chd|====================================================================
Chd|  EULER_MROT                    source/model/submodel/euler_mrot.F
Chd|-- called by -----------
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EULER_MROT (RX,RY,RZ,ROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   RX,RY,RZ, ROT(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   RX2, RY2, RZ2, R2, R, C, CM1, S, SX, SY, SZ, CZ, 
     .   E11, E22, E33, E12, E13, E31, E32, E21, E23
C=======================================================================
      RX2 = RX*RX                 
      RY2 = RY*RY                 
      RZ2 = RZ*RZ                 
      R2  = MAX(EM20,RX2+RY2+RZ2)
      R   = SQRT(R2)        
C
      C = COS(R)                  
      S = SIN(R)                
      CM1 = ONE - C      
C
      RX  = RX / R
      RY  = RY / R
      RZ  = RZ / R
      RX2 = RX2 / R2                       
      RY2 = RY2 / R2                         
      RZ2 = RZ2 / R2                        
C
      SX = S*RX                   
      SY = S*RY                   
      SZ = S*RZ                   
      CZ = RZ*CM1                 
C-----
      E11 = RX2*CM1 + C           
      E22 = RY2*CM1 + C           
      E33 = RZ2*CM1 + C           
C
      E12 = RX*RY*CM1             
      E21 = E12 - SZ              
      E12 = E12 + SZ              
C
      E13 = RX*CZ                 
      E31 = E13 + SY              
      E13 = E13 - SY              
C                                  
      E23 = RY *CZ                
      E32 = E23 - SX              
      E23 = E23 + SX              
C-----
      ROT(1) = E11              
      ROT(2) = E12              
      ROT(3) = E13              
      ROT(4) = E21              
      ROT(5) = E22              
      ROT(6) = E23              
      ROT(7) = E31              
      ROT(8) = E32              
      ROT(9) = E33              
C---
      RETURN
      END
