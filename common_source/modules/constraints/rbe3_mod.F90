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
!===================================================================================================

      !||====================================================================
      !||    rbe3_mod         ../common_source/modules/constraints/rbe3_mod.F90
      !||--- called by ------------------------------------------------------
      !||    get_nrbe3pen_l   ../engine/source/output/restart/restart_rbe3pen.F90
      !||    prerbe3p0        ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    radioss2         ../engine/source/engine/radioss2.F
      !||    rbe3f            ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3pen_init     ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
      !||    rbe3t1           ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3v            ../engine/source/constraints/general/rbe3/rbe3v.F
      !||    rdcomi           ../engine/source/output/restart/rdcomm.F
      !||    rdresa           ../engine/source/output/restart/rdresa.F
      !||    rdresb           ../engine/source/output/restart/rdresb.F
      !||    read_rrbe3pen    ../engine/source/output/restart/restart_rbe3pen.F90
      !||    resol            ../engine/source/engine/resol.F
      !||    resol_head       ../engine/source/engine/resol_head.F
      !||    resol_init       ../engine/source/engine/resol_init.F
      !||    restalloc        ../engine/source/output/restart/arralloc.F
      !||    write_rrbe3pen   ../engine/source/output/restart/restart_rbe3pen.F90
      !||    wrrestp          ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    precision_mod    ../common_source/modules/precision_mod.F90
      !||====================================================================
      module rbe3_mod
         use precision_mod, only: WP
         implicit none
         private :: WP
         ! ----------------------------------------------------------------------------------------------------------------------
         
                  integer, parameter :: irbe3_variables = 10      !< first dimension of irbe3
                  integer, parameter :: rrpen_variables = 10      !< dimension of all rrbe3pen_* variables
         
         ! ----------------------------------------------------------------------------------------------------------------------
         ! T y p e s
         ! ----------------------------------------------------------------------------------------------------------------------
         
                  type rbe3_mpi
                     integer :: fr_rbe3_sz
                     integer, DIMENSION(:), ALLOCATABLE :: iad_rbe3  !< #entities to communicate / points to fr_rbe3
                     integer, DIMENSION(:), ALLOCATABLE :: fr_rbe3   !< entities to communicate
                     real(kind=WP) ,dimension(:), allocatable :: fr_rbe3mp !< entities to communicate

                  end type rbe3_mpi
                  type rbe3_pen
                    integer :: nrbe3_lp                                        !< number of RBE3 of penalty formulation (local)                           
                    real(kind=WP) , dimension(:)   , allocatable ::  rrbe3pen_vi  !< damp(nrbe3_lp)
                    real(kind=WP) , dimension(:,:) , allocatable ::  rrbe3pen_d   !< disp(3*nrbe3_lp)
                    real(kind=WP) , dimension(:,:) , allocatable ::  rrbe3pen_m   !< mom(3*nrbe3_lp)
                    real(kind=WP) , dimension(:,:) , allocatable ::  rrbe3pen_stf !< stif(2*nrbe3_lp)
                    real(kind=WP) , dimension(:)   , allocatable ::  rrbe3pen_fac !< stif_fac(nrbe3_lp) stif factor stability

                  end type rbe3_pen
         
                  type rbe3_
                    integer :: nrbe3                                  !< Number of RBE3
                    integer :: lrbe3_sz                               !< size of lrbe3
                    integer :: frbe3_sz                               !< size of frbe3
!                    integer nrbe3_gp                               !< number of RBE3 of penalty formulation (global)
                    integer,dimension(:,:),allocatable ::  irbe3   !< irbe3(irbe3_variables,nrbe3)  IRBE3 main array
                    integer,dimension(:),allocatable   ::  lrbe3   !< lrbe3 array IRBE3 main array
                    real(kind=WP) ,dimension(:), allocatable ::  frbe3   !< RBE3 Float variables
                    integer :: irotg                               !< Global Rotational flag, >0 if one RBE3 has rot option, 0 else.
                    integer :: irotg_sz                            !< Number of values to communicate : if irotg==0 -> 5,  else -> 10.
                    ! Buffers for RBE3
                    integer :: nmt                                    !< Number of unique main nodes
                    integer :: rrbe3_sz
                    real(kind=WP), dimension(:), allocatable :: rrbe3
                    integer :: rrbe3_pon_sz
                    double precision, dimension(:), allocatable :: rrbe3_pon
!
                    ! MPI structures
                    type (rbe3_mpi) :: mpi
                    ! penalty structures
                    type (rbe3_pen) :: pen
                  end type rbe3_
         
               contains
         
               !! \brief allocate rbe3 type
               !! \details
      !||====================================================================
      !||    allocate_rbe3   ../common_source/modules/constraints/rbe3_mod.F90
      !||--- called by ------------------------------------------------------
      !||    restalloc       ../engine/source/output/restart/arralloc.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    my_alloc_mod    ../common_source/tools/memory/my_alloc.F90
      !||====================================================================
               subroutine allocate_rbe3( rbe3,nspmd )
                  use my_alloc_mod
                  implicit none
                  type(rbe3_),INTENT(INOUT) :: rbe3
                  integer,INTENT(IN) :: nspmd

                  ! General RBE3 Buffer
                  call my_alloc( rbe3%irbe3,irbe3_variables,rbe3%nrbe3)
                  call my_alloc( rbe3%lrbe3,rbe3%lrbe3_sz)
                  call my_alloc( rbe3%frbe3,rbe3%frbe3_sz)
                  ! MPI RBE3 Buffer
                  call my_alloc( rbe3%mpi%iad_rbe3,nspmd+1)
                  call my_alloc( rbe3%mpi%fr_rbe3,rbe3%mpi%fr_rbe3_sz)
                  call my_alloc( rbe3%mpi%fr_rbe3mp,rbe3%mpi%fr_rbe3_sz)

               end subroutine allocate_rbe3
!
               !! \brief allocate rbe3%pen type
      !||====================================================================
      !||    allocate_rbe3pen   ../common_source/modules/constraints/rbe3_mod.F90
      !||--- called by ------------------------------------------------------
      !||    read_rrbe3pen      ../engine/source/output/restart/restart_rbe3pen.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    my_alloc_mod       ../common_source/tools/memory/my_alloc.F90
      !||====================================================================
               subroutine allocate_rbe3pen(rbe3pen)
                  use my_alloc_mod
                  implicit none
                  type(rbe3_pen),INTENT(INOUT) :: rbe3pen

                  call my_alloc( rbe3pen%rrbe3pen_vi,rbe3pen%nrbe3_lp)
                  call my_alloc( rbe3pen%rrbe3pen_d,3,rbe3pen%nrbe3_lp)
                  call my_alloc( rbe3pen%rrbe3pen_m,3,rbe3pen%nrbe3_lp)
                  call my_alloc( rbe3pen%rrbe3pen_stf,2,rbe3pen%nrbe3_lp)
                  call my_alloc( rbe3pen%rrbe3pen_fac,rbe3pen%nrbe3_lp)

               end subroutine allocate_rbe3pen
!         
               end module rbe3_mod
         
