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
      !||====================================================================
      !||    tempcg        ../engine/source/materials/mat_share/tempcg.F
      !||--- called by ------------------------------------------------------
      !||    cforc3        ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk    ../engine/source/elements/xfem/cforc3_crk.F
      !||    czforc3       ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk   ../engine/source/elements/xfem/czforc3_crk.F
      !||====================================================================
      SUBROUTINE TEMPCG(NUMNOD, NEL  ,IXC ,TEMP ,TEMPEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMNOD
      INTEGER ,INTENT(IN) :: NEL
      INTEGER IXC(NIXC,*)
      my_real :: TEMP(NUMNOD)
      my_real :: TEMPEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C --- calculate mean element temperature in Gauss point
      DO I=1,NEL
         TEMPEL(I) = FOURTH * (TEMP(IXC(2,I)) + TEMP(IXC(3,I)) + 
     .                         TEMP(IXC(4,I)) + TEMP(IXC(5,I)) )
      ENDDO
!-----------
      RETURN
      END
