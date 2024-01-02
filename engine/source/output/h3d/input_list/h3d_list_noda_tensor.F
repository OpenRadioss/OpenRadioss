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
Chd|  H3D_LIST_NODAL_TENSOR         source/output/h3d/input_list/h3d_list_noda_tensor.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_NODAL_TENSOR(H3D_KEYWORD_NODAL_TENSOR,GEO,NKEY)
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
      TYPE (H3D_KEYWORD) H3D_KEYWORD_NODAL_TENSOR(*)
      my_real GEO(NPROPG,NUMGEO)
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NIPMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPS'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Grid Point Stress'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'mean stress of elements connected to node(computed using element shape fcts)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPS1'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'GPS1'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'mean stress of elements connected to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPS2'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'GPS2'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'mean (using relative element volume) stress of elements connected to node '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPSTRAIN'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Grid Point Strain'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'mean strain of elements connected to node(computed using element shape fcts)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPS/TMAX'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Max_Grid Point Stress'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'Maximum (P1) Grid Point Stress tensor over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPS/TMIN'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Min_Grid Point Stress'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'Minimum (P3) Grid Point Stress tensor over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPSTRAIN/TMAX'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Max_Grid Point Strain'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'Maximum (P1) Grid Point Strain tensor over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_TENSOR(I)%KEY3  = 'GPSTRAIN/TMIN'
      H3D_KEYWORD_NODAL_TENSOR(I)%TEXT1  = 'Min_Grid Point Strain'
      H3D_KEYWORD_NODAL_TENSOR(I)%COMMENT  = 'Minimum (P3) Grid Point Strain tensor over time'
c-----------------------------------------------

      NKEY=I
      END
