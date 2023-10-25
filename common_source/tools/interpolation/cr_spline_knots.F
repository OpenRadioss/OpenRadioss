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
Chd|  CR_SPLINE_KNOTS
Chd|-- called by -----------
Chd|        CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CR_SPLINE_KNOTS( PTS , KNOTS, ALPHA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       ! computes knots for solving 4-point spline path. 
       ! CATMULL-ROM PARAMETER
       !  ALPHA = 0.0 : uniform
       !  ALPHA = 0.5 : centripetal
       !  ALPHA = 1.0 : chordal   
       !      INPUT  - PTS(0:3,:) are 4 control points
       !      INPUT  - ALPHA CCR SPLINE PARAMETER
       !      OUTPU  - KNOTS() : t0,t1,t2,t3
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         my_real, INTENT(IN)    :: PTS(0:3,3),  ALPHA
         my_real, INTENT(INOUT) :: KNOTS(0:3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         my_real :: X,Y,Z
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
          KNOTS(0)=0.0
          X=PTS(1-1,1)-PTS(1,1)
          Y=PTS(1-1,2)-PTS(1,2)
          Z=PTS(1-1,3)-PTS(1,3)  
          KNOTS(1)=KNOTS(0)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
          X=PTS(1,1)-PTS(1+1,1)
          Y=PTS(1,2)-PTS(1+1,2)
          Z=PTS(1,3)-PTS(1+1,3)  
          KNOTS(2)=KNOTS(1)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
          X=PTS(1+1,1)-PTS(1+2,1)
          Y=PTS(1+1,2)-PTS(1+2,2)
          Z=PTS(1+1,3)-PTS(1+2,3)  
          KNOTS(3)=KNOTS(2)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
       END SUBROUTINE CR_SPLINE_KNOTS     
