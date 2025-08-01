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
!||    anicon0       ../engine/source/output/anim/generate/anicon0.F
!||--- called by ------------------------------------------------------
!||    sortie_main   ../engine/source/output/sortie_main.F
!||====================================================================
      SUBROUTINE ANICON0(CONT, N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real  CONT(3,*)
      INTEGER N
      INTEGER I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
#include "lockon.inc"
         DO I = 1,N
          CONT(1,I) = ZERO
          CONT(2,I) = ZERO
          CONT(3,I) = ZERO  
         ENDDO
#include "lockoff.inc"
C-----------------------------------------------
      RETURN
      END
      


