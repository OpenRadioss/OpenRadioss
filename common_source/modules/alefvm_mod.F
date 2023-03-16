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
Chd|  ALEFVM_MOD                    modules/alefvm_mod.F          
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        INIMOM_FVM                    starter/source/elements/solid/solide/inimom_fvm.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        ACONV3                        engine/source/ale/ale3d/aconv3.F
Chd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
Chd|        ALEFVM_AFLUX3                 engine/source/ale/alefvm/alefvm_aflux3.F
Chd|        ALEFVM_EFLUX3                 engine/source/ale/alefvm/alefvm_eflux3.F
Chd|        ALEFVM_EPSDOT                 engine/source/ale/alefvm/alefvm_epsdot.F
Chd|        ALEFVM_EXPAND_MOM2            engine/source/ale/alefvm/alefvm_expand_mom2.F
Chd|        ALEFVM_FRESET                 engine/source/ale/alefvm/alefvm_freset.F
Chd|        ALEFVM_GRAVITY                engine/source/ale/alefvm/alefvm_gravity.F
Chd|        ALEFVM_GRAVITY_INT22          engine/source/ale/alefvm/alefvm_gravity_int22.F
Chd|        ALEFVM_GRAV_INIT              engine/source/ale/alefvm/alefvm_grav_init.F
Chd|        ALEFVM_INIT                   engine/source/ale/alefvm/alefvm_init.F
Chd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
Chd|        ALEFVM_SCHEME                 engine/source/ale/alefvm/alefvm_scheme.F
Chd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
Chd|        ALEFVM_SFINT3_INT22           engine/source/ale/alefvm/alefvm_sfint3_int22.F
Chd|        ALEFVM_STRESS                 engine/source/ale/alefvm/alefvm_stress.F
Chd|        ALEFVM_STRESS_INT22           engine/source/ale/alefvm/alefvm_stress_int22.F
Chd|        ALEFVM_TFEXT                  engine/source/ale/alefvm/alefvm_tfext.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        DFUNC0                        engine/source/output/anim/generate/dfunc0.F
Chd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
Chd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|        H3D_QUAD_VECTOR               engine/source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_VECTOR_1            engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        M11VS2                        engine/source/materials/mat/mat011/m11vs2.F
Chd|        M11VS3                        engine/source/materials/mat/mat011/m11vs3.F
Chd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SIGEPS37_SINGLE_CELL          engine/source/interfaces/int22/sigeps37_single_cell.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALEFVM_MOD
C-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
C-----------------------------------------------

        TYPE ALEFVM_BUFFER_t
          my_real, DIMENSION(:)    , ALLOCATABLE :: WFEXT_CELL
          my_real, DIMENSION(:,:)  , ALLOCATABLE :: FCELL, FEXT_CELL
          my_real, DIMENSION(:,:)  , ALLOCATABLE :: VERTEX, FINT_CELL      
          my_real, DIMENSION(:,:,:), ALLOCATABLE :: F_FACE
        END TYPE ALEFVM_BUFFER_t
        
        TYPE ALEFVM_PARAM_t
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
        END TYPE ALEFVM_PARAM_t
        
        TYPE(ALEFVM_BUFFER_t),TARGET :: ALEFVM_Buffer
        TYPE(ALEFVM_PARAM_t),TARGET :: ALEFVM_Param      
      
      END MODULE ALEFVM_MOD
