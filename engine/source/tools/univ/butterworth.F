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
Chd|  BUTTERWORTH                   source/tools/univ/butterworth.F
Chd|-- called by -----------
Chd|        E_PERIOD                      source/general_controls/damping/static.F
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUTTERWORTH (DT,FREQ,X2,X1,X,FX2,FX1,FX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT (IN) :: 
     .   DT,FREQ,X2,X1,X,FX2,FX1
      my_real, INTENT (OUT) ::
     .   FX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DT2,WD,WA,WA2,A0,A1,A2,B1,B2,C1
C=======================================================================
C    INPUT variables:
C    DT   - actual timestep
C    FREQ - filtering frequency
C    X    - current unfiltered variable
C    X1   - unfiltered variable (timestep -1)
C    X2   - unfiltered variable (timestep -2)
C    FX1  - filtered variable (timestep -1)
C    FX2  - filtered variable (timestep -2)
C-----------------------------------------------------------------------
C    OUTPUT variables:
C    FX - current filtered variable
C=======================================================================
      DT2  = DT / DEUX                           
      PI   = ATAN2(ZERO,-UN)
      WD = SQR2*PI*FREQ*5.0/3.0  
      WA = SIN(WD*DT2) / COS(WD*DT2)                     
      WA2 = WA*WA                                      
      C1 = UN + SQR2*WA + WA2                           
      A0 = WA2 / C1                                    
      A1 = DEUX * A0                                   
      A2 = A0                                          
      B1 = -DEUX*(WA2 - UN) / C1                       
      B2 = (-UN + SQR2*WA - WA2) / C1                   
      FX = A0*X + A1*X1 + A2*X2 + B1*FX1 + B2*FX2           
C---
      RETURN
      END SUBROUTINE BUTTERWORTH
