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
Chd|  H3D_LIST_1D_SCALAR            source/output/h3d/input_list/h3d_list_1d_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_1D_SCALAR(H3D_KEYWORD_ONED_SCALAR,GEO,NKEY)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_ONED_SCALAR(*)
      my_real
     .      GEO(NPROPG,*)
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NIPMAX
C===============================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'EPSP'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Plastic Strain'
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT_ALL = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Specific Energy'
      H3D_KEYWORD_ONED_SCALAR(I)%COMMENT  = 
     .'Specific energy density (internal energy divided by the element mass)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'VONM'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Von Mises'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'DAM1'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Damage 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'DAM2'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Damage 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'DAM3'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Damage 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGX'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress X '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGY'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress Y '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGZ'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress Z '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGXY'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGYZ'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'SIGZX'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Stress ZX'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'DT'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Time Step'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'AMS'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'AMS selection'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'OFF'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Element status'
      H3D_KEYWORD_ONED_SCALAR(I)%COMMENT  = 
     .'0.0:deleted  >0.&<1.:under failure process 1.0:activated  <0:Standby'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'EINT'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Internal Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'FRACTION/MARTENSITE'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Martensite fraction'
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT_ALL = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'FRACTION/MARTENSITE/TMAX'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Maximum of martensite fraction'
      H3D_KEYWORD_ONED_SCALAR(I)%COMMENT  = 
     .' - over time and over integration points'
c-----------------------------------------------
       I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'TEPS'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Transformation strain'
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_ONED_SCALAR(I)%IS_IPT_ALL = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'GROUP'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Group identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Internal identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'LOCAL.ID'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Local identifier within the group'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'EPSD'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Strain rate'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3    = 'DAMA'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1   = 'MAX DAMAGE'
      H3D_KEYWORD_ONED_SCALAR(I)%COMMENT = 'maximum damage over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_ONED_SCALAR(I)%KEY3  = 'EINTV'
      H3D_KEYWORD_ONED_SCALAR(I)%TEXT1  = 'Internal Energy per unit volume'
c-----------------------------------------------
      NKEY=I
      END
