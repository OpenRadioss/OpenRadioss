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
!||    h3d_list_quad_vector   ../engine/source/output/h3d/input_list/h3d_list_quad_vector.F
!||--- called by ------------------------------------------------------
!||    lech3d                 ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod                ../engine/share/modules/h3d_mod.F
!||    message_mod            ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE H3D_LIST_QUAD_VECTOR(H3D_KEYWORD_QUAD_VECTOR,GEO,IGEO,NKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_QUAD_VECTOR(*)
      my_real GEO(NPROPG,NUMGEO)
      INTEGER IGEO(NPROPGI,NUMGEO)
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NIPMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_VECTOR(I)%KEY3  = 'VECT/VEL'
      H3D_KEYWORD_QUAD_VECTOR(I)%TEXT1  = 'Element Velocity'
      H3D_KEYWORD_QUAD_VECTOR(I)%COMMENT  = 'Element velocity for FVM solver'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_VECTOR(I)%KEY3  = 'VECT/ACC'
      H3D_KEYWORD_QUAD_VECTOR(I)%TEXT1  = 'Element Acceleration'
      H3D_KEYWORD_QUAD_VECTOR(I)%COMMENT  = 'Element acceleration for FVM solver'
c-----------------------------------------------
      NKEY=I
      END
