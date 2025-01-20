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
      !||    init_global_frontier_monvol_mod   ../engine/source/airbag/init_global_monvol_frontier.F90
      !||--- called by ------------------------------------------------------
      !||    resol                             ../engine/source/engine/resol.F
      !||====================================================================
      module init_global_frontier_monvol_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the frontier between processor for a monitored volume mpi comm
!! \details loop over the monitored volumes to find a main processor
!!          --> the main processor has at least 1 monitored volume
!!          --> the main processor has the lowest number of segment
      !||====================================================================
      !||    init_global_frontier_monvol   ../engine/source/airbag/init_global_monvol_frontier.F90
      !||--- called by ------------------------------------------------------
      !||    resol                         ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    groupdef_mod                  ../common_source/modules/groupdef_mod.F
      !||    monvol_struct_mod             ../engine/share/modules/monvol_struct_mod.F
      !||====================================================================
        subroutine init_global_frontier_monvol(ispmd,nspmd,nvolu,nsurf,monvol, &
                       nimv,    & 
                       fr_mv,frontier_global_mv, t_monvoln,igrsurf )
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
          integer :: ispmd !< mpi task id
          integer, intent(in) :: nspmd !< total number of mpi tasks
          integer, intent(in) :: nvolu !< number of monitored volume
          integer, intent(in) :: nsurf !< number of surface    
          integer, intent(in) :: nimv !< first dim of monvol
          integer, dimension(nspmd+2,nvolu), intent(in) :: fr_mv !< mpi frontier per monitored volume
          integer, dimension(nspmd+2), intent(inout) :: frontier_global_mv !< global mpi frontier 
          integer, dimension(nimv*nvolu), intent(in) :: monvol !< monitored volume data
          type(monvol_struct_), dimension(nvolu), intent(in) :: t_monvoln
          type(surf_), dimension(nsurf), intent(in) :: igrsurf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: computation_needed
          integer :: segment_number,surf_id,ityp
          integer :: p_main,min_segment
          integer :: monvol_address
          integer :: ijk
          integer, dimension(2) :: s_buffer
          integer, dimension(2,nspmd) :: r_buffer
          integer :: ierror
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
          s_buffer(1:2) = 0
          monvol_address = 1
          do ijk=1,nvolu
            ityp = monvol(monvol_address+1) ! get the type of the airbag
            surf_id = monvol(monvol_address+3) ! get the id of the surface
            segment_number = igrsurf(surf_id)%nseg ! get the number of segment of the surface "surf_id"
            computation_needed = .true.
            if(ityp==6) computation_needed = .false.
            if(fr_mv(ispmd+1,ijk)==0.and.fr_mv(nspmd+2,ijk)/=ispmd+1) computation_needed = .false.

            if(computation_needed) then
              s_buffer(1) = s_buffer(1) + 6
              s_buffer(2) = s_buffer(2) + segment_number
              if((t_monvoln(ijk)%nb_fill_tri>0).and.(ispmd + 1 == fr_mv(nspmd+2,ijk))) then
                s_buffer(2) = s_buffer(2) + t_monvoln(ijk)%nb_fill_tri               
              endif
            endif
            monvol_address = monvol_address + nimv
          enddo
 
          if(nspmd>1) then
#ifdef MPI
            call mpi_gather(s_buffer,2,MPI_INTEGER,r_buffer,2,MPI_INTEGER,0,SPMD_COMM_WORLD,ierror)
#endif
          else
            r_buffer(1:2,1) = s_buffer(1:2)
          endif

          p_main = -1
          min_segment = HUGE(min_segment)
          if(ispmd==0) then
            r_buffer(1:2,1) = s_buffer(1:2)
            frontier_global_mv(1:nspmd+2) = 0
            do ijk=1,nspmd
              frontier_global_mv(ijk) = r_buffer(1,ijk)
              if(r_buffer(1,ijk)>0.and.r_buffer(2,ijk)<min_segment) then
                p_main = ijk
                min_segment = r_buffer(2,ijk)
              endif
            enddo
            if(p_main==-1) then
              p_main = 1
            endif
            frontier_global_mv(nspmd+2) = p_main
          endif
          if(nspmd>1) then    
#ifdef MPI
            call mpi_bcast(frontier_global_mv,nspmd+2,MPI_INTEGER,0,SPMD_COMM_WORLD,ierror)
#endif  
          endif

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_global_frontier_monvol
      end module init_global_frontier_monvol_mod
