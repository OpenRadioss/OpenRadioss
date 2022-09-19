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
Chd|  CR_SPLINE_INTERPOL
Chd|-- called by -----------
Chd|        CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CR_SPLINE_INTERPOL( PTS , KNOTS, T, C, C_D, C_DD)
C-----------------------------------------------
       !FOR A GIVEN POSITION T in [0,1] which is position on [P1,P2], GET VALUE ON SPLINE P0-P1-P2-P3
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
         ! INPUT  - PTS(0:3,:) are 4 control points
         ! INPUT  - T is in [0,1]  : T=0 => VAL = P(2,:)
         ! OUTPUT - C is interpolated value.
         ! OUTPUT - C_D is 1st derivative
         ! OUTPUT - C_DD is 2nd derivatice
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         my_real, INTENT(IN)    :: PTS(0:3,3),KNOTS(0:3),T
         my_real, INTENT(INOUT) :: C(3),C_D(3),C_DD(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         my_real :: A1(3),A2(3),A3(3)
         my_real :: B1(3),B2(3)
         my_real :: A1P(3),A2P(3),A3P(3), B1P(3),B2P(3),B1PP(3),B2PP(3)
         my_real :: TT  !in [T1,T2]=[KNOT(1),KNOT(2)]
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
         TT=KNOTS(1)+T*(KNOTS(2)-KNOTS(1))
         A1(1:3)   = ((KNOTS(1)-TT)*PTS(0,1:3)+(TT-KNOTS(0))*PTS(1,1:3)) /(KNOTS(1)-KNOTS(0))
         A2(1:3)   = ((KNOTS(2)-TT)*PTS(1,1:3)+(TT-KNOTS(1))*PTS(2,1:3)) /(KNOTS(2)-KNOTS(1))
         A3(1:3)   = ((KNOTS(3)-TT)*PTS(2,1:3)+(TT-KNOTS(2))*PTS(3,1:3)) /(KNOTS(3)-KNOTS(2))
         B1(1:3)   = ((KNOTS(2)-TT)*A1(1:3) + (TT-KNOTS(0))*A2(1:3)) / (KNOTS(2)-KNOTS(0))
         B2(1:3)   = ((KNOTS(3)-TT)*A2(1:3) + (TT-KNOTS(1))*A3(1:3)) / (KNOTS(3)-KNOTS(1))
         C(1:3)    = ((KNOTS(2)-TT)*B1(1:3) + (TT-KNOTS(1))*B2(1:3)) / (KNOTS(2)-KNOTS(1))
         A1P(1:3)  = (PTS(1,1:3)-PTS(0,1:3))/(KNOTS(1)-KNOTS(0))
         A2P(1:3)  = (PTS(2,1:3)-PTS(1,1:3))/(KNOTS(2)-KNOTS(1))
         A3P(1:3)  = (PTS(3,1:3)-PTS(2,1:3))/(KNOTS(3)-KNOTS(2))
         B1P(1:3)  =  (A2(1:3)-A1(1:3))/(KNOTS(2)-KNOTS(0))
     .             + (KNOTS(2)-TT)/(KNOTS(2)-KNOTS(0))*A1P(1:3)
     .             + (TT-KNOTS(0))/(KNOTS(2)-KNOTS(0))*A2P(1:3)
         B2P(1:3)  =  (A3(1:3)-A2(1:3))/(KNOTS(3)-KNOTS(1))
     .             + (KNOTS(3)-TT)/(KNOTS(3)-KNOTS(1))*A2P(1:3)
     .             + (TT-KNOTS(1))/(KNOTS(3)-KNOTS(1))*A3P(1:3)
         C_D(1:3)  =  (B2(1:3)-B1(1:3))/(KNOTS(2)-KNOTS(1))
     .             + (KNOTS(2)-TT)/(KNOTS(2)-KNOTS(1))*B1P(1:3)
     .             + (TT-KNOTS(1))/(KNOTS(2)-KNOTS(1))*B2P(1:3)
         B1PP(1:3) =  (2.*A2P(1:3)-2.*A1P(1:3))/(KNOTS(2)-KNOTS(0))
         B2PP(1:3) =  (2.*A3P(1:3)-2.*A2P(1:3))/(KNOTS(3)-KNOTS(1))
         C_DD(1:3) = (2.*B2P(1:3)-2.*B1P(1:3)+(KNOTS(2)-TT)*B1PP(1:3)+(TT-KNOTS(1))*B2PP(1:3))/(KNOTS(2)-KNOTS(1))
         RETURN
       END SUBROUTINE CR_SPLINE_INTERPOL
