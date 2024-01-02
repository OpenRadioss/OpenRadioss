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
Chd|  NEWMAN_RAJU                   source/materials/fail/alter/newman_raju.F
Chd|-- called by -----------
Chd|        BROKMANN_CRACK_INIT           source/materials/fail/alter/brokmann_crack_init.F
Chd|        FAIL_BROKMANN                 source/materials/fail/alter/fail_brokmann.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NEWMAN_RAJU(C,A,T,B,FPI,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real C, A, T, B, FPI, Y
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real Q,M1,M2,M3,G,FPHI,FB,FW,F,AT,AC,SINP,COSP
c--------------------------------------------------------------------------
c     Calculate the Geometry-correction-factor according to NewmanRaju 1981
c==========================================================================
      IF (FPI == HALF) THEN
        SINP = ONE
        COSP = ZERO
      ELSE IF (FPI == ZERO) THEN
        SINP = ZERO
        COSP = ONE
      ELSE
        SINP = SIN(FPI*PI)
        COSP = COS(FPI*PI)
      END IF

      AC   = A / C
      AT   = A / T
      Q    = ONE + 1.464*AC**1.65

      M1   = 1.13-0.09*AC
      M2   = -0.54+0.89/(0.2 + AC)
      M3   = HALF - ONE/(0.65 +AC) + 14.*(ONE-AC)**24
      G    = ONE +(0.1+ 0.35*(AT)**2)*(ONE-SINP)**2
     
      FPHI = (AC**2 * COSP**2 + SINP**2 )**0.25
      FB   = PI*C*SQRT(AT) 

      FW   = COS(PI*C/(TWO*B)*SQRT(AT))
      
      F    = (M1+M2*(AT)**2 + M3*(AT)**4)*FPHI*G / SQRT(ABS(FW))
      Y    = SQRT(ONE/Q)*F
c-----------
      RETURN
      END
