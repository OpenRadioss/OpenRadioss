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
Chd|  CR_SPLINE_POINT_PROJ
Chd|-- called by -----------
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_INTERPOL            source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|====================================================================
       SUBROUTINE CR_SPLINE_POINT_PROJ(LOCAL_PT,Z,ALPHA,Zh,h,T)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       !COMPUTE PROJECTION ZH ON A GIVEN POINT Z ALONG THE SPLINE INTERPOLATION
       !  INPUT  - LOCAL_PT : 4 control points P0,P1,P2,P3
       !  INPUT  - Z : TEST POINt to project
       !  INPUT  - ALPHA : CCR SPLINE PARAMETER
       !  OUTPUT - Zh : projected point
       !  OUTPUT - H : distance from spline
       !  OUTPUT - T : position [0,1] on SPLINE [T1,T2]
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real,INTENT(IN)    :: LOCAL_PT(4,3),Z(3),ALPHA
        my_real,INTENT(INOUT) :: Zh(3),h
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real               :: KNOTS(4),C(3),C_D(3),C_DD(3),T
        my_real               :: F,F_D
        INTEGER            :: ITER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        T = 0.5
        CALL CR_SPLINE_KNOTS(LOCAL_PT, KNOTS, ALPHA)
        DO ITER=1,20
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T,C,C_D,C_DD)
          F   = DOT_PRODUCT(C(1:3)-Z(1:3), C_D(1:3))
          F_D = DOT_PRODUCT(C_D(1:3), C_D(1:3)) + DOT_PRODUCT(C(1:3)-Z(1:3), C_DD(1:3))
          IF(F_D /= 0.)THEN
            T=T-F/F_D
          ELSE
            IF(F == 0.)EXIT
            print *, "/DFS/DETCORD : error, convergence issue"
            stop 220022
          ENDIF
          IF(T<0.)T=1E-10
          IF(T>1.)T=1.-1E-10
        ENDDO
        CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T,C,C_D,C_DD)
        Zh(1:3) = C(1:3)
        h=SQRT(DOT_PRODUCT(C(1:3)-Z(1:3),C(1:3)-Z(1:3)))
       END SUBROUTINE CR_SPLINE_POINT_PROJ
