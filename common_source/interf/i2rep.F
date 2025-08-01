Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i2rep           ../common_source/interf/i2rep.F
!||--- called by ------------------------------------------------------
!||    i2_dtn_25       ../starter/source/interfaces/inter3d1/i2_dtn.F
!||    i2_dtn_27_cin   ../starter/source/interfaces/inter3d1/i2_dtn_27.F
!||    i2_dtn_27_pen   ../starter/source/interfaces/inter3d1/i2_dtn_27.F
!||    i2_dtn_28_pen   ../starter/source/interfaces/inter3d1/i2_dtn_28.F
!||    i2for25         ../engine/source/interfaces/interf/i2for25.F
!||    i2for25p        ../engine/source/interfaces/interf/i2for25p.F
!||    i2for26         ../engine/source/interfaces/interf/i2for26.F
!||    i2for26p        ../engine/source/interfaces/interf/i2for26p.F
!||    i2for27_cin     ../engine/source/interfaces/interf/i2for27_cin.F
!||    i2for27_pen     ../engine/source/interfaces/interf/i2for27_pen.F
!||    i2for27p_cin    ../engine/source/interfaces/interf/i2for27p_cin.F
!||    i2for27p_pen    ../engine/source/interfaces/interf/i2for27p_pen.F
!||    i2for28_pen     ../engine/source/interfaces/interf/i2for28_pen.F
!||    i2for28p_pen    ../engine/source/interfaces/interf/i2for28p_pen.F
!||====================================================================
      SUBROUTINE I2REP(
     .                 X1     ,X2     ,X3     ,X4     ,
     .                 Y1     ,Y2     ,Y3     ,Y4     ,
     .                 Z1     ,Z2     ,Z3     ,Z4     ,
     .                 E1X    ,E1Y    ,E1Z    ,
     .                 E2X    ,E2Y    ,E2Z    ,
     .                 E3X    ,E3Y    ,E3Z    ,NIR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real X1, X2, X3, X4, Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4, E1X, E1Y, E1Z, E2X,E2Y, E2Z, E3X, E3Y, E3Z
      INTEGER NIR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: SUM_,S1,S2
C-----------------------------------------------

        IF(NIR==4)THEN
          E1X = (X2 + X3 - X1 - X4)                          
          E1Y = (Y2 + Y3 - Y1 - Y4)                          
          E1Z = (Z2 + Z3 - Z1 - Z4)                          
          E2X = (X3 + X4 - X1 - X2)                          
          E2Y = (Y3 + Y4 - Y1 - Y2)                          
          E2Z = (Z3 + Z4 - Z1 - Z2)                          
        ELSE
          E1X = (X2 + X3 - TWO * X1)                          
          E1Y = (Y2 + Y3 - TWO * Y1)                          
          E1Z = (Z2 + Z3 - TWO * Z1)                          
          E2X = (TWO * X3 - X1 - X2)                          
          E2Y = (TWO * Y3 - Y1 - Y2)                          
          E2Z = (TWO * Z3 - Z1 - Z2)                          
        END IF
        E3X = E1Y*E2Z - E1Z*E2Y                           
        E3Y = E1Z*E2X - E1X*E2Z                           
        E3Z = E1X*E2Y - E1Y*E2X                           
C
        SUM_ = E3X*E3X+E3Y*E3Y+E3Z*E3Z                      
        SUM_ = ONE / MAX(SQRT(SUM_),EM20)                     
        E3X = E3X * SUM_                                    
        E3Y = E3Y * SUM_                                    
        E3Z = E3Z * SUM_                                    
C
        IF (NIR == 4) THEN
          S1  = E1X*E1X+E1Y*E1Y+E1Z*E1Z                      
          S2  = E2X*E2X+E2Y*E2Y+E2Z*E2Z                      
          SUM_ = SQRT(S1/S2)                                  
          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUM_                  
          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUM_                  
          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUM_
        ENDIF                  
C
        SUM_ = E1X*E1X+E1Y*E1Y+E1Z*E1Z                      
        SUM_ = ONE / MAX(SQRT(SUM_),EM20)                     
        E1X = E1X * SUM_                                    
        E1Y = E1Y * SUM_                                    
        E1Z = E1Z * SUM_                                    
C
        E2X = E3Y * E1Z - E3Z * E1Y                        
        E2Y = E3Z * E1X - E3X * E1Z                        
        E2Z = E3X * E1Y - E3Y * E1X                
C---
      RETURN
      END SUBROUTINE I2REP
