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
Chd|  SMOM43                        source/elements/solid/sconnect/smom43.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMOM43(NEL   ,
     .     F1X   ,F2X   ,F3X   ,F4X   ,F5X   ,F6X   ,F7X   ,F8X   ,
     .     F1Y   ,F2Y   ,F3Y   ,F4Y   ,F5Y   ,F6Y   ,F7Y   ,F8Y   ,
     .     F1Z   ,F2Z   ,F3Z   ,F4Z   ,F5Z   ,F6Z   ,F7Z   ,F8Z   ,
     .     R1X   ,R2X   ,R3X   ,R4X   ,R5X   ,R6X   ,R7X   ,R8X   ,
     .     R1Y   ,R2Y   ,R3Y   ,R4Y   ,R5Y   ,R6Y   ,R7Y   ,R8Y   ,
     .     R1Z   ,R2Z   ,R3Z   ,R4Z   ,R5Z   ,R6Z   ,R7Z   ,R8Z   ,
     .     RXX   ,RYY   ,RZZ   ,TTHICK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real 
     .  TTHICK
      my_real 
     .  F1X(*),F2X(*),F3X(*),F4X(*),F5X(*),F6X(*),F7X(*),F8X(*),         
     .  F1Y(*),F2Y(*),F3Y(*),F4Y(*),F5Y(*),F6Y(*),F7Y(*),F8Y(*),         
     .  F1Z(*),F2Z(*),F3Z(*),F4Z(*),F5Z(*),F6Z(*),F7Z(*),F8Z(*),      
     .  R1X(NEL),R2X(NEL),R3X(NEL),R4X(NEL),                  
     .  R5X(NEL),R6X(NEL),R7X(NEL),R8X(NEL),                  
     .  R1Y(NEL),R2Y(NEL),R3Y(NEL),R4Y(NEL),                  
     .  R5Y(NEL),R6Y(NEL),R7Y(NEL),R8Y(NEL),                  
     .  R1Z(NEL),R2Z(NEL),R3Z(NEL),R4Z(NEL),                  
     .  R5Z(NEL),R6Z(NEL),R7Z(NEL),R8Z(NEL),RXX(NEL),RYY(NEL),RZZ(NEL) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .  MXX(NEL),MYY(NEL),MZZ(NEL)
      my_real
     .  MYZ,MZY,MZX,MXZ,MXY,MYX,FXX,FYY,FZZ,FA,FB,RAX,RBX,RAY,RBY,
     .  D1,D2,DD
C=======================================================================
      DO I=1,NEL
        MXY = R1X(I)*F1Y(I)+R2X(I)*F2Y(I)+R3X(I)*F3Y(I)+R4X(I)*F4Y(I)
     .       +R5X(I)*F5Y(I)+R6X(I)*F6Y(I)+R7X(I)*F7Y(I)+R8X(I)*F8Y(I)
        MYX = R1Y(I)*F1X(I)+R2Y(I)*F2X(I)+R3Y(I)*F3X(I)+R4Y(I)*F4X(I)
     .       +R5Y(I)*F5X(I)+R6Y(I)*F6X(I)+R7Y(I)*F7X(I)+R8Y(I)*F8X(I)
        MZZ(I) = MXY - MYX
c
C---    Moment Z
c
        FYY = MXY/RXX(I)         
        FXX =-MYX/RYY(I)         
c
        F1X(I) = F1X(I) - FXX    
        F2X(I) = F2X(I) - FXX    
        F3X(I) = F3X(I) + FXX    
        F4X(I) = F4X(I) + FXX    
        F5X(I) = F5X(I) - FXX    
        F6X(I) = F6X(I) - FXX    
        F7X(I) = F7X(I) + FXX    
        F8X(I) = F8X(I) + FXX    
c
        F1Y(I) = F1Y(I) + FYY    
        F2Y(I) = F2Y(I) - FYY    
        F3Y(I) = F3Y(I) - FYY    
        F4Y(I) = F4Y(I) + FYY    
        F5Y(I) = F5Y(I) + FYY    
        F6Y(I) = F6Y(I) - FYY    
        F7Y(I) = F7Y(I) - FYY    
        F8Y(I) = F8Y(I) + FYY    
c
c---    Moments Mx, MY
c
        IF(TTHICK >ZERO)THEN
        MYZ = R1Y(I)*F1Z(I)+R2Y(I)*F2Z(I)+R3Y(I)*F3Z(I)+R4Y(I)*F4Z(I)
     .       +R5Y(I)*F5Z(I)+R6Y(I)*F6Z(I)+R7Y(I)*F7Z(I)+R8Y(I)*F8Z(I)
        MZY = TTHICK*HALF*(-F1Y(I)-F2Y(I)-F3Y(I)-F4Y(I) !car R1Z,R2Z,R3Z,R4Z <0 
     .                      +F5Y(I)+F6Y(I)+F7Y(I)+F8Y(I))
        MXX(I) = MYZ - MZY

        MZX = TTHICK*HALF*(-F1X(I)-F2X(I)-F3X(I)-F4X(I)!car R1Z,R2Z,R3Z,R4Z <0
     .                       +F5X(I)+F6X(I)+F7X(I)+F8X(I))
        MXZ = R1X(I)*F1Z(I)+R2X(I)*F2Z(I)+R3X(I)*F3Z(I)+R4X(I)*F4Z(I)
     .       +R5X(I)*F5Z(I)+R6X(I)*F6Z(I)+R7X(I)*F7Z(I)+R8X(I)*F8Z(I)
        MYY(I) = MZX - MXZ
        ELSE
        MYZ = R1Y(I)*F1Z(I)+R2Y(I)*F2Z(I)+R3Y(I)*F3Z(I)+R4Y(I)*F4Z(I)
     .       +R5Y(I)*F5Z(I)+R6Y(I)*F6Z(I)+R7Y(I)*F7Z(I)+R8Y(I)*F8Z(I)
        MZY = R1Z(I)*F1Y(I)+R2Z(I)*F2Y(I)+R3Z(I)*F3Y(I)+R4Z(I)*F4Y(I)
     .       +R5Z(I)*F5Y(I)+R6Z(I)*F6Y(I)+R7Z(I)*F7Y(I)+R8Z(I)*F8Y(I)
        MXX(I) = MYZ - MZY
        MZX = R1Z(I)*F1X(I)+R2Z(I)*F2X(I)+R3Z(I)*F3X(I)+R4Z(I)*F4X(I)
     .       +R5Z(I)*F5X(I)+R6Z(I)*F6X(I)+R7Z(I)*F7X(I)+R8Z(I)*F8X(I)
        MXZ = R1X(I)*F1Z(I)+R2X(I)*F2Z(I)+R3X(I)*F3Z(I)+R4X(I)*F4Z(I)
     .       +R5X(I)*F5Z(I)+R6X(I)*F6Z(I)+R7X(I)*F7Z(I)+R8X(I)*F8Z(I)
        MYY(I) = MZX - MXZ
        ENDIF
c
        RAX = R1X(I) + R5X(I) - R3X(I) - R7X(I)       
        RBX = R4X(I) + R8X(I) - R2X(I) - R6X(I)       
        RAY = R1Y(I) + R5Y(I) - R3Y(I) - R7Y(I)       
        RBY = R4Y(I) + R8Y(I) - R2Y(I) - R6Y(I)       
        D1 = -MXX(I)*RBX - MYY(I)*RBY            
        D2 =  MXX(I)*RAX + MYY(I)*RAY            
        DD =  RAY*RBX - RAX*RBY                   
        FA = D1 / DD                             
        FB = D2 / DD                             
C
        F1Z(I) = F1Z(I) + FA                     
        F2Z(I) = F2Z(I) - FB                     
        F3Z(I) = F3Z(I) - FA                     
        F4Z(I) = F4Z(I) + FB                     
        F5Z(I) = F5Z(I) + FA                     
        F6Z(I) = F6Z(I) - FB                     
        F7Z(I) = F7Z(I) - FA                     
        F8Z(I) = F8Z(I) + FB                     
C---
      ENDDO
C-----------
      RETURN
      END
