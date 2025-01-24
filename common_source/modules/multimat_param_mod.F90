!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    multimat_param_mod               ../common_source/modules/multimat_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    a22conv3                         ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconve                           ../engine/source/ale/aconve.F
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_upwind3_int22              ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    arezon                           ../engine/source/ale/arezon.F
      !||    atherm                           ../engine/source/ale/atherm.F
      !||    dfuncc                           ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncs                           ../engine/source/output/anim/generate/dfunc6.F
      !||    ebcs10                           ../engine/source/boundary_conditions/ebcs/ebcs10.F
      !||    h3d_quad_scalar                  ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_shell_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_solid_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_sph_scalar                   ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    hm_read_mat                      ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat20                    ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat51                    ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    i22datainit                      ../engine/source/interfaces/int22/i22datainit.F
      !||    i22err3                          ../starter/source/interfaces/inter3d1/i22err3.F
      !||    ini_inimap1d                     ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    inigrav_m51                      ../starter/source/initial_conditions/inigrav/inigrav_m51.F
      !||    initia                           ../starter/source/elements/initia/initia.F
      !||    inivol_set                       ../starter/source/initial_conditions/inivol/inivol_set.F
      !||    m51init                          ../starter/source/materials/mat/mat051/m51init.F
      !||    m51vois2                         ../engine/source/materials/mat/mat051/m51vois2.F
      !||    m51vois3                         ../engine/source/materials/mat/mat051/m51vois3.F
      !||    matparam_def_mod                 ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    nodalvfrac                       ../engine/source/output/anim/generate/nodalvfrac.F
      !||    nrf51ini                         ../starter/source/materials/mat/mat051/nrf51ini.F
      !||    r2r_matparam_copy                ../starter/source/elements/elbuf_init/r2r_matparam_copy.F
      !||    r_bufbric_22                     ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    rdcomr                           ../engine/source/output/restart/rdcomm.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    sigeps51                         ../engine/source/materials/mat/mat051/sigeps51.F90
      !||    sigeps51_boundary_material       ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
      !||    sinit22_fvm                      ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    spmd_l51vois                     ../engine/source/mpi/fluid/spmd_cfd.F
      !||    stat_inimap1d_file_spmd          ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd               ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd          ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd               ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||    wrcomr                           ../engine/source/output/restart/wrcomm.F
      !||    write_buf_law51                  ../engine/source/materials/mat/mat051/write_buf_law51.F
      !||====================================================================
      MODULE MULTIMAT_PARAM_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!  [ the module names in use must be in uppercase for now, it will change latter]
!  [ ONLY is mandatory, note the space before the ,]      
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! [ no comment on the same line as #include #define #ifdef, #endif ]
! [ my_real.inc must be included, it was included in "implicit_f.inc"]
#include "my_real.inc"      
        INTEGER, PARAMETER :: M51_N0PHAS = 04
        INTEGER, PARAMETER :: M51_NVPHAS = 23
        INTEGER, PARAMETER :: M51_IFLG6_SIZE = 37
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ---------------------------------------------------------------------------------------------------------------------- 
        TYPE MULTIMAT_PARAM_                                 !< data structure for MAT_PARAM buffer
          integer :: nb = 0                                  !< number of submaterial
          integer,allocatable,dimension(:) :: mid            !< material internal identifier for each submaterial
          my_real,allocatable,dimension(:) :: vfrac          !< volume fraction for each submaterial

          contains
            procedure :: destruct => destruct_multimat_param

        END TYPE MULTIMAT_PARAM_
     
        logical :: M20_DISCRETE_FILL = .false.               !< LAW20 global parameters
        my_real :: M51_SSP0MAX, M51_LC0MAX, M51_TCP_REF      !< LAW51 global parameters
        INTEGER :: M51_IFLG6 = 0                             !< LAW51 global parameters
        INTEGER :: M51_lSET_IFLG6 = 0                        !< LAW51 global parameters
        INTEGER :: M51_ILOOP_NRF = 0                         !< LAW51 global parameters

        contains

      !||====================================================================
      !||    destruct_multimat_param   ../common_source/modules/multimat_param_mod.F90
      !||====================================================================
          subroutine destruct_multimat_param(this)
            implicit none
            class(MULTIMAT_PARAM_) :: this
            if (allocated(this%mid))   deallocate(this%mid)
            if (allocated(this%vfrac)) deallocate(this%vfrac)
          end subroutine destruct_multimat_param

      END MODULE MULTIMAT_PARAM_MOD

      
