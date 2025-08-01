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
!||    cutfunc     ../engine/source/tools/sect/cutfunc.F
!||--- called by ------------------------------------------------------
!||    genani      ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_r_c   ../common_source/tools/input_output/write_routtines.c
!||====================================================================
      SUBROUTINE CUTFUNC(FUNC,IVOIS,AL,NODCUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real FUNC(*), AL(*)
      INTEGER IVOIS(2,*),NODCUT,I 
C-----------------------------------------------
      REAL R4
C-----------------------------------------------
      DO I=1,NODCUT
         R4 =AL(I)*FUNC(IVOIS(2,I))+(ONE-AL(I))*FUNC(IVOIS(1,I)) 
         CALL WRITE_R_C(R4,1)
      ENDDO
C-----------------------------------------------
      RETURN
      END
