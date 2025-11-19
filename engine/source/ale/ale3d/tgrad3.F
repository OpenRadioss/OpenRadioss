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
!||    tgrad3   ../engine/source/ale/ale3d/tgrad3.F
!||--- called by ------------------------------------------------------
!||    agrad0   ../engine/source/ale/agrad0.F
!||====================================================================
      SUBROUTINE TGRAD3(GRD,GRAD,NEL,s_flux,offset)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of flux array
      integer, intent(in) :: offset !< address of the first element of the current group
      INTEGER  NEL
      my_real GRD(NEL,6), GRAD(s_flux,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ii
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NEL
        ii = i + offset
        GRAD(ii,1)= GRD(I,1)
        GRAD(ii,2)= GRD(I,2)
        GRAD(ii,3)= GRD(I,3)
        GRAD(ii,4)= GRD(I,4)
        GRAD(ii,5)= GRD(I,5)
        GRAD(ii,6)= GRD(I,6)
      ENDDO! next I
C----------------------------------------------- 
      RETURN
      END
