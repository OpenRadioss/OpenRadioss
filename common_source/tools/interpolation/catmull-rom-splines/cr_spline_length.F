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
Chd|  CR_SPLINE_LENGTH
Chd|-- called by -----------
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_INTERPOL            source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|====================================================================
       SUBROUTINE CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       !compute SPLINE length with third order method (SIMPSON)
       !  INPUT  - LOCAL_PT : 4 control points P0,P1,P2,P3
       !  INPUT  - ALPHA : CCR SPLINE PARAMETER
       !  INPUT  - T : position [0,1] on SPLINE [T1,T2].  1.0 means full spline length
       !  OUTPUT - LEN : length of the curve parametrised with t in [0,T]  T<=1.0
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real,INTENT(IN)    :: LOCAL_PT(4,3),ALPHA ,T
        my_real,INTENT(INOUT) :: LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real               :: KNOTS(4),C(3),C_D(3),C_DD(3),T_,DT, COLD(3)
        my_real               :: F,F_D,VAL(6),K(2)
        INTEGER            :: ITER,NITER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        CALL CR_SPLINE_KNOTS(LOCAL_PT, KNOTS, ALPHA)
        LEN = 0.
        NITER=20
         DO ITER=1,NITER-1
          !first point
          T_=T*(1.*ITER-1.)/(NITER-1.)
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(1)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          K(1)=KNOTS(2)+T_*(KNOTS(3)-KNOTS(2))
          !middle point
          T_=(1.*ITER-1.)/(1.*NITER-1.) + 0.5*1./(NITER-1.)
          T_=T_*T
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(2)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          !last point
          T_=T*(1.*ITER)/(NITER-1.)
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(3)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          K(2)=KNOTS(2)+T_*(KNOTS(3)-KNOTS(2))
          !length
          LEN = LEN+(K(2)-K(1))/6. * (VAL(1)+4.*VAL(2)+VAL(3))
        ENDDO
       END SUBROUTINE CR_SPLINE_LENGTH
