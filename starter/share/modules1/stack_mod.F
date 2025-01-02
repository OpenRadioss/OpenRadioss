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
      !||    stack_mod                           ../starter/share/modules1/stack_mod.F
      !||--- called by ------------------------------------------------------
      !||    brokmann_random                     ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
      !||    c3grhead                            ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                           ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                             ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c3inmas                             ../starter/source/elements/sh3n/coque3n/c3inmas.F
      !||    cbainit3                            ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cbufxfe                             ../starter/source/elements/xfem/cbufxfe.F
      !||    cdkinit3                            ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cgrhead                             ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                            ../starter/source/elements/shell/coque/cgrtails.F
      !||    check_mat_elem_prop_compatibility   ../starter/source/materials/mat/check_mat_elem_prop_compatibility.F
      !||    check_pthickfail                    ../starter/source/materials/fail/check_pthickfail.F
      !||    cinit3                              ../starter/source/elements/shell/coque/cinit3.F
      !||    cinmas                              ../starter/source/elements/shell/coque/cinmas.F
      !||    cmaini3                             ../starter/source/elements/sh3n/coquedk/cmaini3.F
      !||    corthdir                            ../starter/source/elements/shell/coque/corthdir.F
      !||    corthini                            ../starter/source/elements/shell/coque/corthini.F
      !||    ddsplit                             ../starter/source/restart/ddsplit/ddsplit.F
      !||    elbuf_ini                           ../starter/source/elements/elbuf_init/elbuf_ini.F
      !||    fractal_element_neighbor            ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
      !||    hm_read_anim                        ../starter/source/general_controls/engine/hm_read_anim.F
      !||    hm_read_drape                       ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_inistate_d00                ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_prop17                      ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop51                      ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_properties                  ../starter/source/properties/hm_read_properties.F
      !||    hm_read_stack                       ../starter/source/stack/hm_read_stack.F
      !||    inirig_mat                          ../starter/source/elements/initia/inirig_mat.F
      !||    initia                              ../starter/source/elements/initia/initia.F
      !||    inivoid                             ../starter/source/elements/initia/inivoid.F
      !||    layini1                             ../starter/source/elements/shell/coqueba/layini1.F
      !||    layini_xfe                          ../starter/source/elements/xfem/cbufxfe.F
      !||    lec_inistate                        ../starter/source/elements/initia/lec_inistate.F
      !||    lecstack_ply                        ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lectur                              ../starter/source/starter/lectur.F
      !||    pre_stackgroup                      ../starter/source/stack/pres_stackgroup.F
      !||    random_walk_dmg                     ../starter/source/materials/fail/fractal/random_walk_dmg.F90
      !||    shellthk_upd                        ../starter/source/properties/composite_options/drape/shellthk_upd.F
      !||    spmd_anim_ply_init                  ../starter/source/spmd/spmd_anim_ply_init.F
      !||    stack_var_mod                       ../starter/share/modules1/stack_var_mod.F
      !||    stackgroup                          ../starter/source/stack/stackgroup.F
      !||    stackgroup_drape                    ../starter/source/stack/stackgroup_drape.F
      !||    t3grhead                            ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                           ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    updfail                             ../starter/source/materials/updfail.F90
      !||====================================================================
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

