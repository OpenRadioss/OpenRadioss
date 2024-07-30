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
      !||    alefvm_mod             ../common_source/modules/ale/alefvm_mod.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3               ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconv3                 ../engine/source/ale/ale3d/aconv3.F
      !||    aconve                 ../engine/source/ale/aconve.F
      !||    aflux0                 ../engine/source/ale/aflux0.F
      !||    aflux3_int22_fvm       ../engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
      !||    ale_euler_init         ../starter/source/materials/ale/ale_euler_init.F
      !||    alefvm_aflux3          ../engine/source/ale/alefvm/alefvm_aflux3.F
      !||    alefvm_eflux3          ../engine/source/ale/alefvm/alefvm_eflux3.F
      !||    alefvm_epsdot          ../engine/source/ale/alefvm/alefvm_epsdot.F
      !||    alefvm_expand_mom2     ../engine/source/ale/alefvm/alefvm_expand_mom2.F
      !||    alefvm_freset          ../engine/source/ale/alefvm/alefvm_freset.F
      !||    alefvm_grav_init       ../engine/source/ale/alefvm/alefvm_grav_init.F
      !||    alefvm_gravity         ../engine/source/ale/alefvm/alefvm_gravity.F
      !||    alefvm_gravity_int22   ../engine/source/ale/alefvm/alefvm_gravity_int22.F
      !||    alefvm_init            ../engine/source/ale/alefvm/alefvm_init.F
      !||    alefvm_main            ../engine/source/ale/alefvm/alefvm_main.F
      !||    alefvm_scheme          ../engine/source/ale/alefvm/alefvm_scheme.F
      !||    alefvm_sfint3          ../engine/source/ale/alefvm/alefvm_sfint3.F
      !||    alefvm_sfint3_int22    ../engine/source/ale/alefvm/alefvm_sfint3_int22.F
      !||    alefvm_stress          ../engine/source/ale/alefvm/alefvm_stress.F
      !||    alefvm_stress_int22    ../engine/source/ale/alefvm/alefvm_stress_int22.F
      !||    alefvm_tfext           ../engine/source/ale/alefvm/alefvm_tfext.F
      !||    alemain                ../engine/source/ale/alemain.F
      !||    contrl                 ../starter/source/starter/contrl.F
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||    dfunc0                 ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                 ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncs                 ../engine/source/output/anim/generate/dfunc6.F
      !||    eflux3_int22_fvm       ../engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
      !||    epxle3                 ../engine/source/elements/solid/solide/epxle3.F
      !||    h3d_quad_scalar        ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_vector        ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_shell_scalar_1     ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_vector_1     ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_solid_scalar_1     ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    hm_read_mat11_k_eps    ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    ini_inimap1d           ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    inimom_fvm             ../starter/source/elements/solid/solide/inimom_fvm.F
      !||    lectur                 ../engine/source/input/lectur.F
      !||    m11law                 ../engine/source/materials/mat/mat011/m11law.F
      !||    m11vs2                 ../engine/source/materials/mat/mat011/m11vs2.F
      !||    m11vs3                 ../engine/source/materials/mat/mat011/m11vs3.F
      !||    multi_timeevolution    ../engine/source/multifluid/multi_timeevolution.F
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||    rdcomi                 ../engine/source/output/restart/rdcomm.F
      !||    rdresb                 ../engine/source/output/restart/rdresb.F
      !||    read_ale_mat           ../starter/source/materials/ale/read_ale_mat.F
      !||    read_euler_mat         ../starter/source/materials/ale/read_euler_mat.F
      !||    resol                  ../engine/source/engine/resol.F
      !||    restalloc              ../engine/source/output/restart/arralloc.F
      !||    sfint3                 ../engine/source/elements/solid/solide/sfint3.F
      !||    sigeps37_single_cell   ../engine/source/interfaces/int22/sigeps37_single_cell.F
      !||    sigeps51               ../engine/source/materials/mat/mat051/sigeps51.F
      !||    sinit22_fvm            ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    sinit3                 ../starter/source/elements/solid/solide/sinit3.F
      !||    starter0               ../starter/source/starter/starter0.F
      !||    thquad                 ../engine/source/output/th/thquad.F
      !||    thsol                  ../engine/source/output/th/thsol.F
      !||    wrcomi                 ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrrestp                ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE ALEFVM_MOD
C-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
C-----------------------------------------------

        TYPE ALEFVM_BUFFER_
          my_real, DIMENSION(:)    , ALLOCATABLE :: WFEXT_CELL
          my_real, DIMENSION(:,:)  , ALLOCATABLE :: FCELL, FEXT_CELL
          my_real, DIMENSION(:,:)  , ALLOCATABLE :: VERTEX, FINT_CELL      
          my_real, DIMENSION(:,:,:), ALLOCATABLE :: F_FACE
        END TYPE ALEFVM_BUFFER_
        
        TYPE ALEFVM_PARAM_
          INTEGER IEnabled     
          INTEGER ISOLVER
          INTEGER IOUTP
          INTEGER IOUTP_GRAV
          INTEGER IOUTP_STRESS
          INTEGER IOUTP_FINT
          INTEGER IOUTP_FLUX
          INTEGER IOUTP_SCHEME
          INTEGER IOUTP_CONV
          INTEGER IFORM
          INTEGER IOUTP_EPSDOT
          INTEGER IOUTP_BCS
          INTEGER IOUTP_TFEXT
          INTEGER IWFEXT
          INTEGER IPRINT_1
          INTEGER IPRINT_2                
        END TYPE ALEFVM_PARAM_
        
        TYPE(ALEFVM_BUFFER_),TARGET :: ALEFVM_Buffer
        TYPE(ALEFVM_PARAM_),TARGET :: ALEFVM_Param      
      
      END MODULE ALEFVM_MOD
