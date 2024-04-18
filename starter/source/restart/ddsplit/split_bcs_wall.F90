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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Data structure must be updated after domain decomposition
!! \details  after domain decomposition the global data scruture must be split to keep relevant local data on each domain
!
      subroutine split_bcs_wall(bcs_per_proc, cep, scep, nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use bcs_mod , only : bcs, bcs_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
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
      integer ii,jj,p
      integer proc_index(nspmd)     ! index for working array
      integer size_on_proc(nspmd)   ! total number of entity on each domain
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ---------------------------------------------------------------------------------------------------------------------- 
      bcs_per_proc(1:nspmd)%num_wall = bcs%num_wall
      if(bcs%num_wall == 0)return !nothing to allocate and nothing to initialize
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   

      do p=1,nspmd
       allocate( bcs_per_proc(p)%wall(bcs%num_wall) )
      end do

      ! --- filling global parameters for bcs wall data structure on each domain
      do p=1,nspmd
        do ii=1,bcs%num_wall
          bcs_per_proc(p)%wall(ii)%is_enabled = bcs%wall(ii)%is_enabled
          bcs_per_proc(p)%wall(ii)%is_depending_on_time = bcs%wall(ii)%is_depending_on_time
          bcs_per_proc(p)%wall(ii)%is_depending_on_sensor = bcs%wall(ii)%is_depending_on_sensor
          bcs_per_proc(p)%wall(ii)%tstart = bcs%wall(ii)%tstart
          bcs_per_proc(p)%wall(ii)%tstop = bcs%wall(ii)%tstop
          bcs_per_proc(p)%wall(ii)%user_id = bcs%wall(ii)%user_id
          bcs_per_proc(p)%wall(ii)%grnod_id = bcs%wall(ii)%grnod_id
          bcs_per_proc(p)%wall(ii)%sensor_id = bcs%wall(ii)%sensor_id  
        enddo
      end do      

      ! --- filling list of elems : only relevant elems on each domain
      do ii=1,bcs%num_wall

        proc_index(1:nspmd) = 0
        size_on_proc(1:nspmd) = 0
        
        !---numbering
        do jj=1,bcs%wall(ii)%list%size
          p = 1 + cep( bcs%wall(ii)%list%elem(jj) )
          size_on_proc(p) = size_on_proc(p) + 1
        enddo

        !---allcoation of local data structure (on each domain)      
        do p=1,nspmd
          bcs_per_proc(p)%wall(ii)%list%size = size_on_proc(p)
          allocate( bcs_per_proc(p)%wall(ii)%list%elem(size_on_proc(p)))
          allocate( bcs_per_proc(p)%wall(ii)%list%face(size_on_proc(p)))
          allocate( bcs_per_proc(p)%wall(ii)%list%adjacent_elem(size_on_proc(p)))
        enddo
                                                                                   
        !--filling local data structure
        do jj=1,bcs%wall(ii)%list%size
          p = 1 + cep( bcs%wall(ii)%list%elem(jj) )
          proc_index(p) = proc_index(p) + 1                                              
          bcs_per_proc(p)%wall(ii)%list%elem( proc_index(p) ) = bcs%wall(ii)%list%elem(jj) ! id global de l element
          bcs_per_proc(p)%wall(ii)%list%face( proc_index(p) ) = bcs%wall(ii)%list%face(jj)
          bcs_per_proc(p)%wall(ii)%list%adjacent_elem( proc_index(p) ) = 0
        enddo                         

      enddo                                                                              

! ----------------------------------------------------------------------------------------------------------------------
      return
      end subroutine split_bcs_wall
