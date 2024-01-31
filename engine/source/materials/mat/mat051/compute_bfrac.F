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
Chd|====================================================================
Chd|  COMPUTE_BFRAC                 source/materials/mat/mat051/compute_bfrac.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COMPUTE_BFRAC(TIME,XL,TBURN,UPARAM,
     .          V1,
     .          RHO1,RHO10,BFRAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .          TIME,XL,TBURN,
     .          V1, RHO1, RHO10,
     .          UPARAM(*),BFRAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug, IBFRAC
      my_real 
     .   VDET,BHE
C-----------------------------------------------
      VDET  = UPARAM(42)
      BHE   = UPARAM(44)
      IBFRAC= UPARAM(68)

      !--------------------------------!
      ! Calculation of BFRAC in [0,1]  !
      !--------------------------------!
      IF(BFRAC < ONE) THEN
        BFRAC = ZERO
        IF(IBFRAC/=1 .AND. TIME > -TBURN)THEN
          XL = V1**THIRD
          BFRAC = VDET*(TIME+TBURN)*TWO_THIRD/XL 
        ENDIF
        IF(IBFRAC/=2) BFRAC  = MAX( BFRAC , BHE * (ONE - RHO10/RHO1) )
        IF(BFRAC < EM03) THEN
          BFRAC = ZERO
        ELSEIF(BFRAC > ONE) THEN
          BFRAC = ONE
        ENDIF
      ENDIF
      RETURN
      END
