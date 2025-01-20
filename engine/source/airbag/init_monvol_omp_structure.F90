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
      !||    init_monvol_omp_structure_mod   ../engine/source/airbag/init_monvol_omp_structure.F90
      !||--- called by ------------------------------------------------------
      !||    resol                           ../engine/source/engine/resol.F
      !||====================================================================
      module init_monvol_omp_structure_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine initializes the monitored volume structure for !$OMP
!! \details the !$OMP monitored volume structure is similar to the FSKY array
      !||====================================================================
      !||    init_monvol_omp_structure   ../engine/source/airbag/init_monvol_omp_structure.F90
      !||--- called by ------------------------------------------------------
      !||    resol                       ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    groupdef_mod                ../common_source/modules/groupdef_mod.F
      !||    monvol_struct_mod           ../engine/share/modules/monvol_struct_mod.F
      !||====================================================================
        subroutine init_monvol_omp_structure(ispmd,nspmd,nvolu,nsurf,monvol,  &
                                             nimv,numnod,                     & 
                                             fr_mv,t_monvoln,igrsurf )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use monvol_struct_mod , only : monvol_struct_
          use groupdef_mod , only : surf_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ispmd !< mpi task id
          integer, intent(in) :: nspmd !< total number of mpi tasks
          integer, intent(in) :: nvolu !< number of monitored volume
          integer, intent(in) :: nsurf !< number of surface    
          integer, intent(in) :: nimv
          integer, intent(in) :: numnod !< number of node
          integer, dimension(nspmd+2,nvolu), intent(in) :: fr_mv !< mpi frontier per monitored volume
          integer, dimension(*), intent(in) :: monvol !< monitored volume data
          type(monvol_struct_), dimension(nvolu), intent(inout) :: t_monvoln !< monvol data
          type(surf_), dimension(nsurf), intent(in) :: igrsurf !< surface data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: computation_needed
          integer :: segment_number,node_number,surf_id,ityp
          integer :: i,j
          integer :: monvol_address
          integer :: ijk,nod,shift,total_contribution_number
          integer, dimension(:), allocatable :: w_array,node_id,node_shift
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
          monvol_address = 1
          ! --------------
          ! loop over the monitored volume
          do ijk=1,nvolu
            ityp = monvol(monvol_address+1) ! get the type of the airbag
            surf_id = monvol(monvol_address+3) ! get the id of the surface
            segment_number = igrsurf(surf_id)%nseg ! get the number of segment of the surface "surf_id"
            ! check if the computation for this ijk monvol is mandatory
            computation_needed = .true.
            if(ityp==6) computation_needed = .false.
            if(fr_mv(ispmd+1,ijk)==0.and.fr_mv(nspmd+2,ijk)/=ispmd+1) computation_needed = .false.

            if(computation_needed) then
              allocate( t_monvoln(ijk)%normal(3,segment_number) )
              allocate( w_array(numnod) )
              allocate( node_id(numnod) )
              w_array(1:numnod) = 0
              total_contribution_number = 0
              node_number = 0
              allocate( t_monvoln(ijk)%omp_output%contribution_index(segment_number,4) )
              ! ---------------
              do i=1,segment_number ! loop over the segment of the monvol
                do j=1,3 ! loop over the nodes
                  nod = igrsurf(surf_id)%nodes(i,j) ! get the node id
                  if(w_array(nod)==0) then ! the node is not yet taken into account
                    node_number = node_number + 1
                    node_id( node_number ) = nod ! save the node id
                  endif
                  w_array(nod) = w_array(nod) + 1 ! add 1 contribution for this node
                  t_monvoln(ijk)%omp_output%contribution_index(i,j) = w_array(nod)
                  total_contribution_number = total_contribution_number + 1 ! get the total number of contribution
                enddo
                if(igrsurf(surf_id)%eltyp(i)/=7) then ! check if the segment has 4 node
                  nod = igrsurf(surf_id)%nodes(i,4) ! get the node id
                  if(w_array(nod)==0) then ! the node is not yet taken into account
                    node_number = node_number + 1
                    node_id( node_number ) = nod ! save the node id
                  endif
                  w_array(nod) = w_array(nod) + 1 ! add 1 contribution for this node
                  t_monvoln(ijk)%omp_output%contribution_index(i,4) = w_array(nod)
                  total_contribution_number = total_contribution_number + 1 ! get the total number of contribution
                endif                
              enddo
              ! ---------------

              ! ---------------
              ! allocation & initialization
              allocate( t_monvoln(ijk)%omp_output%contribution(total_contribution_number,3) )
              allocate( t_monvoln(ijk)%omp_output%contribution_number(node_number+1) )
              allocate( t_monvoln(ijk)%omp_output%node_id(node_number) )
              t_monvoln(ijk)%omp_output%node_number = node_number
              t_monvoln(ijk)%omp_output%total_contribution_number = total_contribution_number
              t_monvoln(ijk)%omp_output%node_id(1:node_number) = node_id(1:node_number)
              allocate( node_shift(node_number+1) )
              node_shift( 1:node_number+1 ) = 0
              ! ---------------

              ! ---------------
              ! save the number of contribution + the address
              shift = 0
              do i=2,node_number+1
                nod = node_id(i-1)
                node_shift( i ) = node_shift( i-1 ) + w_array(nod) ! get the shift for the node "i"
              enddo
              ! node :              1  |  2  |  3  |  4  | ...
              ! #contribution :     4  |  1  |  0  |  4  |
              ! shift :             0  |  4  |  5  |  5  | 9 ...
              ! shift(i+1) - shit(i) = #contribution of "i" node
              ! shift(i) = address for the "i" node    
              t_monvoln(ijk)%omp_output%contribution_number( 1:node_number+1 ) = node_shift( 1:node_number+1 ) ! save the 
              ! ---------------

              ! ---------------
              ! save the index (to point to the good place in CONTRIBUTION array)
              do i=1,node_number
                nod = node_id(i)
                w_array(nod) = node_shift( i )
              enddo
              ! loop over the segment
              do i=1,segment_number
                do j=1,3
                  nod = igrsurf(surf_id)%nodes(i,j) ! get the node id
                  t_monvoln(ijk)%omp_output%contribution_index(i,j) = t_monvoln(ijk)%omp_output%contribution_index(i,j) + &
                                                                      w_array(nod) ! save the index for the segment "i" and the edge "j)
                enddo
                if(igrsurf(surf_id)%eltyp(i)/=7) then ! the segment has 4 node
                  nod = igrsurf(surf_id)%nodes(i,4) ! get the node id
                  t_monvoln(ijk)%omp_output%contribution_index(i,4) = t_monvoln(ijk)%omp_output%contribution_index(i,4) + &
                                                                      w_array(nod) ! save the index for the segment "i" and the edge "j)
                endif
              enddo
              ! ---------------
              deallocate( node_shift )
              deallocate( w_array )
              deallocate( node_id )
            else
              allocate( t_monvoln(ijk)%normal(3,0) )
              t_monvoln(ijk)%omp_output%node_number = 0
              t_monvoln(ijk)%omp_output%total_contribution_number = 0
              allocate( t_monvoln(ijk)%omp_output%contribution_number(0) )
              allocate( t_monvoln(ijk)%omp_output%contribution_index(0,0) )
              allocate( t_monvoln(ijk)%omp_output%contribution(0,0) )
              allocate( t_monvoln(ijk)%omp_output%node_id(0) )
            endif
            monvol_address = monvol_address + nimv
          enddo
          ! --------------

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_monvol_omp_structure
      end module init_monvol_omp_structure_mod
