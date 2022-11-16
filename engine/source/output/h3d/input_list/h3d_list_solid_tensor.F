Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  H3D_LIST_SOLID_TENSOR         source/output/h3d/input_list/h3d_list_solid_tensor.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_SOLID_TENSOR(H3D_KEYWORD_SOLID_TENSOR,GEO,IGEO,NKEY)
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
      TYPE (H3D_KEYWORD) H3D_KEYWORD_SOLID_TENSOR(*)
      my_real
     .      GEO(NPROPG,*)
      INTEGER IGEO(NPROPGI,*)
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NIPMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRESS'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Stress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Strain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/DAMA'
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Damage'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Cracks (LAW24 only)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/EPSP'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Plastic Strain Tensor'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Plastic Strain tensor is output only for law 24'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRESS/OUTER'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Outer Stress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN/OUTER'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Outer Strain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN_ENG'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Infinitesimal Strain'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Total strain tensor is calculated from 2(initial&actual) configurations'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRESS/CORNER_DATA'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_CORNER_DATA  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'CornerStress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN/CORNER_DATA'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_CORNER_DATA  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'CornerStrain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/BSTRESS'
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_ID  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%IS_ID_ALL  = 1
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'BackStress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRESS/TMAX'
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Max_Stress'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Maximum (P1) Stress tensor over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN/TMAX'
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Max_Strain'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Maximum (P1) Strain tensor over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRESS/TMIN'
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Min_Stress'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Minimum (P3) Stress tensor over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_TENSOR(I)%KEY3  = 'TENS/STRAIN/TMIN'
      H3D_KEYWORD_SOLID_TENSOR(I)%TEXT1  = 'Min_Strain'
      H3D_KEYWORD_SOLID_TENSOR(I)%COMMENT  = 
     .'Minimum (P3) Strain tensor over time&ipt'
c-----------------------------------------------


      NKEY=I
      END
