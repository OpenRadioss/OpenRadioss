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
Chd|  FILTER_SAE211                 source/tools/sensor/filter_sae211.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FILTER_SAE211(NPT,NT,NH,A0,A1,A2,B1,B2,VAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPT,NT,NH
      my_real :: A0,A1,A2,B1,B2,X0,X1,X2,Y0,Y1,Y2,YNF
      my_real ,DIMENSION(NPT) :: VAR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,NPH,NP
      my_real :: YF
c=======================================================================
C    X0   - current unfiltered variable
C    X1   - unfiltered variable (timestep -1)
C    X2   - unfiltered variable (timestep -2)
C    Y1  - filtered variable (timestep -1)  ! forward
C    Y2  - filtered variable (timestep -2)
C    Z1  - double filtered variable (timestep -1))  ! backward
C    Z2  - double filtered variable (timestep -2)
c=======================================================================
C     OUTPUT variables:
C     FX - current filtered variable
c
c     setup pre-start array for forward filter - inverted mirror
c
      YNF = VAR(1)*TWO
      X1  = YNF - VAR(NT)
      X0  = YNF - VAR(NT-1)
      Y1  = ZERO
      DO I=1,NH
        Y1 = Y1 + VAR(I)
      END DO
      Y1 = YNF - Y1 / (NT-NH+1)
      Y0 = Y1

      DO I = NT,1,-1
        X2 = X1
        X1 = X0
        X0 = YNF - VAR(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2           
      END DO
c
c     Filter forward
c
      DO I=1,NPT
        X2 = X1
        X1 = X0
        X0 = VAR(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2           
        VAR(I) = Y0  ! replace initial raw data by filtered value
      END DO
c
c     setup pre-start array for backward filter
c
      YNF = VAR(NPT)*TWO
      X1  = YNF - VAR(NPT-NT)
      X0  = YNF - VAR(NPT-NT-1)
      Y1  = ZERO
      DO I=NH,NT
        Y1 = Y1 + VAR(NPT-I)
      END DO
      Y1 = YNF - Y1 / (NT-NH+1)
      Y0 = Y1

      DO I = NPT-NT+2,NPT
        X2 = X1
        X1 = X0
        X0 = YNF - VAR(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2           
      END DO
c
c     Filter backward
c
      DO I = NPT,1,-1
        X2 = X1
        X1 = X0
        X0 = VAR(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2           
        VAR(I) = Y0  ! replace initial raw data by filtered value
      END DO
     
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
