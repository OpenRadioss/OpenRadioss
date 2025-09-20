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
!||====================================================================
!||    split_bcs_nrf_mod   ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur              ../starter/source/starter/lectur.F
!||====================================================================
      module split_bcs_nrf_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Data structure must be updated after domain decomposition
!! \details  after domain decomposition the global data scruture must be split to keep relevant local data on each domain
!
!||====================================================================
!||    split_bcs_nrf   ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur          ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine split_bcs_nrf(bcs_per_proc, cep, scep, nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs, bcs_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: scep                                         !< size for array definition
          integer,intent(in) :: nspmd                                         !< number of domains
          integer,intent(in) :: cep(scep)                                     !< indicator function for local elements [1:numel] -> [1,2,..,nspmd]
          type(bcs_struct_),dimension(nspmd),intent(inout) :: bcs_per_proc    !< data structure to be filled with relevant local data only (on each domain)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii,jj,p
          integer :: proc_index(nspmd)     ! index for working array
          integer :: size_on_proc(nspmd)   ! total number of entity on each domain
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          bcs_per_proc(1:nspmd)%num_nrf = bcs%num_nrf
          if(bcs%num_nrf == 0)return !nothing to allocate and nothing to initialize
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          do p=1,nspmd
            allocate( bcs_per_proc(p)%nrf(bcs%num_nrf) )
          end do

          ! --- filling global parameters for bcs nrf data structure on each domain
          do p=1,nspmd
            do ii=1,bcs%num_nrf
              bcs_per_proc(p)%nrf(ii)%user_id = bcs%nrf(ii)%user_id
              bcs_per_proc(p)%nrf(ii)%set_id = bcs%nrf(ii)%set_id
            end do
          end do

          ! --- filling list of elems : only relevant elems on each domain
          do ii=1,bcs%num_nrf

            proc_index(1:nspmd) = 0
            size_on_proc(1:nspmd) = 0

            !---numbering
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep( bcs%nrf(ii)%list%elem(jj) )
              size_on_proc(p) = size_on_proc(p) + 1
            end do

            !---allcoation of local data structure (on each domain)
            do p=1,nspmd
              bcs_per_proc(p)%nrf(ii)%list%size = size_on_proc(p)
              allocate( bcs_per_proc(p)%nrf(ii)%list%elem(size_on_proc(p)))
              allocate( bcs_per_proc(p)%nrf(ii)%list%face(size_on_proc(p)))
              allocate( bcs_per_proc(p)%nrf(ii)%list%rCp(size_on_proc(p)))
              allocate( bcs_per_proc(p)%nrf(ii)%list%rCs(size_on_proc(p)))
            end do

            !--filling local data structure
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep( bcs%nrf(ii)%list%elem(jj) )
              proc_index(p) = proc_index(p) + 1
              bcs_per_proc(p)%nrf(ii)%list%elem( proc_index(p) ) = bcs%nrf(ii)%list%elem(jj) ! global elem id
              bcs_per_proc(p)%nrf(ii)%list%face( proc_index(p) ) = bcs%nrf(ii)%list%face(jj)
              bcs_per_proc(p)%nrf(ii)%list%rCp( proc_index(p) ) = bcs%nrf(ii)%list%rCp(jj)
              bcs_per_proc(p)%nrf(ii)%list%rCs( proc_index(p) ) = bcs%nrf(ii)%list%rCs(jj)
            end do

          end do

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine split_bcs_nrf
      end module split_bcs_nrf_mod
