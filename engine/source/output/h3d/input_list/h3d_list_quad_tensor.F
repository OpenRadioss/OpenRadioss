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
Chd|  H3D_LIST_QUAD_TENSOR          source/output/h3d/input_list/h3d_list_quad_tensor.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_LIST_QUAD_TENSOR(H3D_KEYWORD_QUAD_TENSOR,GEO,IGEO,NKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_QUAD_TENSOR(*)
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
      INTEGER I
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_TENSOR(I)%KEY3  = 'TENS/STRESS'
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%TEXT1  = 'Stress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_TENSOR(I)%KEY3  = 'TENS/STRAIN'
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%TEXT1  = 'Strain'
      H3D_KEYWORD_QUAD_TENSOR(I)%COMMENT  = 
     .'Strain tensor is output only if Istrain =1; except laws 14, 15, 19, 24 > 28'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_TENSOR(I)%KEY3  = 'TENS/DAMA'
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%TEXT1  = 'Damage'
      H3D_KEYWORD_QUAD_TENSOR(I)%COMMENT  = 
     .'Cracks (LAW24 only)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_TENSOR(I)%KEY3  = 'TENS/EPSP'
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IR  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IS  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%IS_IT  = 1
      H3D_KEYWORD_QUAD_TENSOR(I)%TEXT1  = 'Plastic Strain Tensor'
      H3D_KEYWORD_QUAD_TENSOR(I)%COMMENT  = 
     .'Plastic Strain tensor is output only for law 24'
c-----------------------------------------------

      NKEY=I
      END
