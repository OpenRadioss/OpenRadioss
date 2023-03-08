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
Chd|  STACK_MOD                     share/modules1/stack_mod.F    
Chd|-- called by -----------
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|        CORTHDIR                      source/elements/shell/coque/corthdir.F
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DEALLOCATE_ELBUF              source/elements/elbuf_init/deallocate_buffer.F
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|        HM_READ_ANIM                  source/general_controls/engine/hm_read_anim.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|        LAYINI_XFE                    source/elements/xfem/cbufxfe.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        SHELLTHK_UPD                  source/properties/composite_options/drape/shellthk_upd.F
Chd|        SPMD_ANIM_PLY_INIT            source/spmd/spmd_anim_ply_init.F
Chd|        STACKGROUP                    source/stack/stackgroup.F     
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        STACK_VAR_MOD                 share/modules1/stack_var_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE STACK_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C---------------------------------------------
C Structures needed for stack (PID17) properties 
C---------------------------------------------
C PLY  Nodes structures for animations
      TYPE STACK_PLY
         INTEGER , DIMENSION (:,:), ALLOCATABLE :: IGEO        ! IGEO TABLE
         my_real , DIMENSION (:,:), ALLOCATABLE :: GEO  , PM       ! GEO, PM 
      END TYPE STACK_PLY        
C      
      TYPE STACK_INFO_ 
         INTEGER, DIMENSION (:), POINTER :: PID
         INTEGER, DIMENSION (:), POINTER :: MID
         INTEGER, DIMENSION (:), POINTER :: MID_IP  
         INTEGER, DIMENSION (:), POINTER :: SUB    
         INTEGER, DIMENSION (:), POINTER :: ISUB
         my_real, DIMENSION (:), POINTER :: ANG
         my_real, DIMENSION (:), POINTER :: THK  !
         my_real, DIMENSION (:), POINTER :: POS  !
         my_real, DIMENSION (:), POINTER :: THKLY
         my_real, DIMENSION (:), POINTER :: WEIGHT 
         my_real, DIMENSION (:), POINTER :: DIR     !           
      END TYPE STACK_INFO_   
C      
      END MODULE STACK_MOD

