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
      !||    rbe3_mod     ../common_source/modules/constraints/rbe3_mod.F90
      !||--- called by ------------------------------------------------------
      !||    prerbe3p0    ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    radioss2     ../engine/source/engine/radioss2.F
      !||    rbe3t1       ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3v        ../engine/source/constraints/general/rbe3/rbe3v.F
      !||    rdcomi       ../engine/source/output/restart/rdcomm.F
      !||    rdresa       ../engine/source/output/restart/rdresa.F
      !||    rdresb       ../engine/source/output/restart/rdresb.F
      !||    resol        ../engine/source/engine/resol.F
      !||    resol_head   ../engine/source/engine/resol_head.F
      !||    resol_init   ../engine/source/engine/resol_init.F
      !||    restalloc    ../engine/source/output/restart/arralloc.F
      !||    wrrestp      ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      module rbe3_mod
         ! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
         
                  integer, parameter :: irbe3_variables = 10      !< first dimension of irbe3
         
         ! ----------------------------------------------------------------------------------------------------------------------
         ! T y p e s
         ! ----------------------------------------------------------------------------------------------------------------------
         
                  type rbe3_mpi
                     integer fr_rbe3_sz
                     integer, DIMENSION(:), ALLOCATABLE :: iad_rbe3  !< #entities to communicate / points to fr_rbe3
                     integer, DIMENSION(:), ALLOCATABLE :: fr_rbe3   !< entities to communicate
                     my_real ,dimension(:), allocatable :: fr_rbe3mp !< entities to communicate

                  end type rbe3_mpi
         
                  type rbe3_
                    integer nrbe3                                  !< Number of RBE3
                    integer lrbe3_sz                               !< size of lrbe3
                    integer frbe3_sz                               !< size of frbe3
                    integer,dimension(:,:),allocatable ::  irbe3   !< irbe3(irbe3_variables,nrbe3)  IRBE3 main array
                    integer,dimension(:),allocatable   ::  lrbe3   !< lrbe3 array IRBE3 main array
                    my_real ,dimension(:), allocatable ::  frbe3   !< RBE3 Float variables
                    integer :: irotg                               !< Global Rotational flag, >0 if one RBE3 has rot option, 0 else.
                    integer :: irotg_sz                            !< Number of values to communicate : if irotg==0 -> 5,  else -> 10.
                    ! Buffers for RBE3
                    integer nmt                                    !< Number of unique main nodes
                    integer :: rrbe3_sz
                    my_real, dimension(:), allocatable :: rrbe3
                    integer :: rrbe3_pon_sz
                    double precision, dimension(:), allocatable :: rrbe3_pon
                    ! MPI structures
                    type (rbe3_mpi) :: mpi
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
         
               end module rbe3_mod
         