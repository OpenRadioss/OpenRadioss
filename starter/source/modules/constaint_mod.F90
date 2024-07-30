!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    constraint_mod   ../starter/source/modules/constaint_mod.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit          ../starter/source/restart/ddsplit/ddsplit.F
      !||    lectur           ../starter/source/starter/lectur.F
      !||    split_rwall      ../starter/source/constraints/general/rwall/split_rwall.F90
      !||    w_front          ../starter/source/restart/ddsplit/w_front.F
      !||====================================================================
      module constraint_mod
!=======================================================================================================================
!!\brief 
!=======================================================================================================================

        ! --------------------------------
        ! Structure for domdec 
        type spmd_
          integer ::  pmain !< main processor of the current rwall/rbody/rbe3/...
          integer ::  s_node_number !< number of S nodes
          integer, dimension(:), allocatable ::  m_proc_list !< list of proc where M node is defined
        end type  spmd_
        ! --------------------------------

        ! --------------------------------
        ! Structure for /RWALL
        type rwall_
          integer, dimension(:,:), allocatable :: dd !< domain decomposition data (number of node per proc, main proc,...)
          type(spmd_), dimension(:), allocatable ::  spmd    !< data per rwall
        end type  rwall_
        ! --------------------------------

        ! --------------------------------
        ! Structure for /RBE3
        type rbe3_
          integer, dimension(:,:), allocatable :: dd !< domain decomposition data (number of node per proc, main proc,...)
          type(spmd_), dimension(:), allocatable ::  spmd    !< data per rwall
        end type  rbe3_
        ! --------------------------------

        ! --------------------------------
        ! Structure for /RBODY
        type rbody_
          integer, dimension(:,:), allocatable :: dd !< domain decomposition data (number of node per proc, main proc,...)
          type(spmd_), dimension(:), allocatable ::  spmd    !< data per rwall
        end type  rbody_
        ! --------------------------------

        type constraint_
          type(rwall_) ::  rwall    !< 
          type(rbody_) ::  rbody    !< 
          type(rbe3_) ::  rbe3    !<        
        end type constraint_


      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Allocation of constraint_struct
      !||====================================================================
      !||    alloc_constraint_struct   ../starter/source/modules/constaint_mod.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                    ../starter/source/starter/lectur.F
      !||====================================================================
        subroutine alloc_constraint_struct(nrwall,nspmd,constraint_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrwall !< number of RWALL
          integer, intent(in) :: nspmd !< number of processor
          type(constraint_), intent(inout) :: constraint_struct
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate( constraint_struct%rwall%dd(nspmd+2,nrwall) )
          constraint_struct%rwall%dd(1:nspmd+2,1:nrwall) = 0

          allocate( constraint_struct%rwall%spmd(nrwall) )
          ! ------------
          do n=1,nrwall
            allocate( constraint_struct%rwall%spmd(n)%m_proc_list(nspmd) )
            constraint_struct%rwall%spmd(n)%m_proc_list(1:nspmd) = 0
          enddo
          ! ------------

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine alloc_constraint_struct


! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Deallocation of constraint_struct
      !||====================================================================
      !||    dealloc_constraint_struct   ../starter/source/modules/constaint_mod.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                      ../starter/source/starter/lectur.F
      !||====================================================================
        subroutine dealloc_constraint_struct(nrwall,constraint_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrwall !< number of RWALL
          type(constraint_), intent(inout) :: constraint_struct
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! ------------
          do n=1,nrwall
            deallocate( constraint_struct%rwall%spmd(n)%m_proc_list )
          enddo
          ! ------------
          deallocate( constraint_struct%rwall%spmd )

          deallocate( constraint_struct%rwall%dd )

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine dealloc_constraint_struct



      end module constraint_mod
