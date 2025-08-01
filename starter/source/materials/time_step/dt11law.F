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
!||    dt11law   ../starter/source/materials/time_step/dt11law.F
!||--- called by ------------------------------------------------------
!||    dtmain    ../starter/source/materials/time_step/dtmain.F
!||====================================================================
      SUBROUTINE DT11LAW(DELTAX, DTX)
C----------------------------------------------------------------
C     CALCUL DT ELEM LOI NO 11 COND.LIM. FLUIDE
C
C     DTEL N'A PAS DE SENS AUSSI EST IL MIS ARBITRAIREMENT A 1000
C------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .     DELTAX(*), DTX(*)
      INTEGER I
C---------------------------------------------------------------------
      DO I=LFT,LLT
      DELTAX(I)=EP03
      DTX(I) = DELTAX(I)
      END DO
c-----------
      RETURN
      END
