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
      !||    split_rwall_mod   ../starter/source/constraints/general/rwall/split_rwall.F90
      !||--- called by ------------------------------------------------------
      !||    lectur            ../starter/source/starter/lectur.F
      !||====================================================================
      module split_rwall_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    split_rwall      ../starter/source/constraints/general/rwall/split_rwall.F90
      !||--- called by ------------------------------------------------------
      !||    lectur           ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    plist_ifront     ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    constraint_mod   ../starter/source/modules/constaint_mod.F90
      !||====================================================================
        subroutine split_rwall(nrwall,nspmd,nnprw ,slprw,nprw,lprw,constraint_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constraint_mod , only : constraint_
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
          integer, intent(in) :: nnprw !< 2nd dim of nprw
          integer, intent(in) :: slprw !< dim of lprw
          integer, dimension(nrwall*nnprw), intent(in) :: nprw !<  rwall data
          integer, dimension(slprw), intent(in) :: lprw !< list of S node 
          type(constraint_), intent(inout) :: constraint_struct !< constraint structure for the splitting
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,n,j
          integer :: index_lprw,index_m_node
          integer :: s_node_number,proc_number,max_node_number
          integer :: m_node_id,node_id,proc_id
          integer :: proc_main
          integer, dimension(nspmd) :: proc_list
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          constraint_struct%rwall%dd(1:nspmd+2,1:nrwall) = 0
          index_lprw = 0 ! index for the S node
          ! ------------
          do n=1,nrwall
            index_m_node = 2*nrwall + n ! index for the M node
            m_node_id = nprw(index_m_node) ! get the M node id
            s_node_number = nprw(n) ! get the number of S node id
            constraint_struct%rwall%spmd(n)%m_proc_list(1:nspmd) = 0
            if(m_node_id/=0) then
              proc_number = 0
              call plist_ifront(proc_list,m_node_id,proc_number) ! get the list of proce where M node is defined
              ! ---------
              ! save the list
              do i =1,proc_number
                proc_id = proc_list(i)
                constraint_struct%rwall%spmd(n)%m_proc_list(proc_id) = 1
              enddo
              ! ---------

              ! ---------            
              do i=1,s_node_number
                proc_number = 0
                node_id = lprw(index_lprw+i)
                call plist_ifront(proc_list,node_id,proc_number) ! get the list of proce where S node is defined
                do j=1,proc_number
                  proc_id = proc_list(j)
                  if(constraint_struct%rwall%spmd(n)%m_proc_list(proc_id) == 0) cycle
                  constraint_struct%rwall%dd(proc_id,n) = constraint_struct%rwall%dd(proc_id,n) + 1
                enddo
              enddo
              ! ---------              
            endif
        
            ! ---------
            ! get the main processor of the current rwall
            proc_main = 1
            max_node_number = 0
            do i=1,nspmd
              if(constraint_struct%rwall%spmd(n)%m_proc_list(i) == 0) cycle
              if(constraint_struct%rwall%dd(i,n)>max_node_number) then
                max_node_number = constraint_struct%rwall%dd(i,n)
                proc_main = i
              elseif(constraint_struct%rwall%dd(i,n)==0.and.constraint_struct%rwall%spmd(n)%m_proc_list(i)==1) then
                constraint_struct%rwall%dd(i,n) = -1
                if(s_node_number==0) proc_main = i
              endif
            enddo
            ! ---------
 

            constraint_struct%rwall%dd(nspmd+1,n) = s_node_number
            constraint_struct%rwall%dd(nspmd+2,n) = proc_main
            ! ---------
            do i=1,nspmd
              constraint_struct%rwall%spmd(n)%pmain = 0
              constraint_struct%rwall%spmd(n)%s_node_number = 0
              if(constraint_struct%rwall%spmd(n)%m_proc_list(i)==1) then
                constraint_struct%rwall%spmd(n)%pmain = proc_main
                constraint_struct%rwall%spmd(n)%s_node_number = s_node_number
              endif
            enddo
            ! ---------

            index_lprw = index_lprw + s_node_number
          enddo
          ! ------------

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine split_rwall
      end module split_rwall_mod
