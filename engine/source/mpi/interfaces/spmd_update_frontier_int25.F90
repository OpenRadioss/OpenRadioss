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
      !||    spmd_update_frontier_int25_mod   ../engine/source/mpi/interfaces/spmd_update_frontier_int25.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface            ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||====================================================================
      module spmd_update_frontier_int25_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine updates the frontiers for mpi comm / interface type25
      !||====================================================================
      !||    spmd_update_frontier_int25   ../engine/source/mpi/interfaces/spmd_update_frontier_int25.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface        ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||--- calls      -----------------------------------------------------
      !||    my_orders                    ../common_source/tools/sort/my_orders.c
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                 ../common_source/modules/constant_mod.F
      !||    spmd_arrays_mod              ../common_source/modules/interfaces/spmd_arrays_mod.F
      !||====================================================================
        subroutine spmd_update_frontier_int25( ispmd,nspmd,ninter25,npari,ninter,nbintc, &
                                               numnod,nbddedgt,nbddedg_max, &
                                               ipari,intlist,itab,  &
                                               intbuf_tab,spmd_arrays )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero
          use spmd_arrays_mod , only : spmd_arrays_
          use intbufdef_mod , only : intbuf_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ispmd !< processor id
          integer, intent(in) :: nspmd !< number of mpi processors
          integer, intent(in) :: ninter25 !< total number of interface /TYPE25
          integer, intent(in) :: npari !< 1rst dim of "ipari" array
          integer, intent(in) :: ninter !< total number of interface
          integer, intent(in) :: nbintc !< reduced number of interface (without /TYPE02)
          integer, intent(in) :: numnod !< total number of node
          integer, intent(inout) :: nbddedgt !< number of frontier edges
          integer, intent(inout) :: nbddedg_max !< number of frontier edges
          integer, dimension(npari,ninter), intent(in) :: ipari !< interface data
          integer, dimension(nbintc), intent(in) :: intlist
          integer, dimension(numnod), intent(in) :: itab !< local to global node id array
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
          type(spmd_arrays_), intent(inout) :: spmd_arrays !< structure for interface spmd arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,p
          integer :: ni25,nin,nty,nrtm
          integer :: n1,n2,ni
          integer :: lshift
          integer :: nbddedg
          integer, dimension(nspmd) :: isom
          integer, dimension(:), allocatable :: proc_rem25,index25,work
          integer, dimension(:,:), allocatable :: fr_sav,itri25
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          ! frontiers vs edges   
          ! first calculate frontier edges
          ni25=0
          lshift=0
          nbddedg_max = 0
          nbddedgt    = 0
          do ni=1,nbintc
            nin = intlist(ni)
            nty = ipari(7,nin)
            nrtm = ipari(4,nin)
            if(nty==25) then                   
              nbddedg = 0           
              ni25=ni25+1           
              do i=1,nrtm  
                if(intbuf_tab(nin)%stfm(i)>zero ) then ! neighboor external surfaces      
                  do j=1,4
                    k = intbuf_tab(nin)%mvoisin(4*(i-1)+j)
                    if(k<0)then
                      p = intbuf_tab(nin)%proc_mvoisin(4*(i-1)+j)
                      if(p /= ispmd+1) then
                        nbddedg = nbddedg + 1
                      endif
                    end if
                  enddo
                endif
              enddo
              nbddedg_max = max(nbddedg_max,nbddedg)
              nbddedgt    = nbddedgt+nbddedg
            endif
          enddo
          !     frontiers vs edges  : update fr_edg array 
          if(allocated(spmd_arrays%fr_edg))  deallocate(spmd_arrays%fr_edg)
          allocate(spmd_arrays%fr_edg(2*nbddedgt))
          allocate(fr_sav(2,nbddedg_max))
          allocate(proc_rem25(nbddedg_max))
          allocate(itri25(5,nbddedg_max))
          allocate(index25(2*nbddedg_max))
          allocate(work(70000))

          spmd_arrays%iad_fredg(1:ninter25*(nspmd+1))=0
          ni25=0
          lshift=0
          do nin=1,ninter
            nty = ipari(7,nin)
            nrtm = ipari(4,nin)
            if(nty==25) then                   
              nbddedg = 0           
              ni25=ni25+1           
              do i = 1, nrtm  
                if(intbuf_tab(nin)%stfm(i)>zero ) then ! neighboor external surfaces
                  do j = 1,4
                    k = intbuf_tab(nin)%mvoisin(4*(i-1)+j)
                    if(k<0)then
                      p = intbuf_tab(nin)%proc_mvoisin(4*(i-1)+j)
                      if(p /= ispmd+1) then
                        nbddedg = nbddedg + 1
                        fr_sav(1,nbddedg) = i ! sorting 
                        fr_sav(2,nbddedg) = j
                        proc_rem25(nbddedg) = p
                        ! sorting of edges frontiers : unique order
                        itri25(1,nbddedg) =  p
                        n1=intbuf_tab(nin)%irectm(4*(i-1)+j)
                        n2=intbuf_tab(nin)%irectm(4*(i-1)+mod(j,4)+1)
                        itri25(2,nbddedg) =  min(-k,intbuf_tab(nin)%mseglo(i))
                        itri25(3,nbddedg) =  max(-k,intbuf_tab(nin)%mseglo(i))
                        itri25(4,nbddedg) =  min(itab(n1),itab(n2))
                        itri25(5,nbddedg) =  max(itab(n1),itab(n2))
                      endif
                    end if
                  enddo
                endif
              enddo

              do i=1,nbddedg
                index25(i) = i
              enddo
              call my_orders(0,work,itri25,index25,nbddedg,5)
              do i=1,nbddedg
                proc_rem25(i) = itri25(1,index25(i))
                spmd_arrays%fr_edg(2*(lshift+i-1)+1) = fr_sav(1,index25(i))
                spmd_arrays%fr_edg(2*(lshift+i-1)+2) = fr_sav(2,index25(i))
              enddo

              do p=1,nspmd
                isom(p) = 0
              enddo
              do i=1,nbddedg
                p = proc_rem25(i)
                isom(p) = isom(p) + 1
              enddo

              spmd_arrays%iad_fredg(ni25) = lshift + 1
              do p=1,nspmd
                spmd_arrays%iad_fredg(p*ninter25+ni25) = spmd_arrays%iad_fredg((p-1)*ninter25+ni25) + isom(p)
              enddo 
              lshift=lshift+nbddedg
            endif
          end do

          deallocate(fr_sav)
          deallocate(proc_rem25)
          deallocate(itri25)
          deallocate(index25)
          deallocate(work)
          ! --------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_update_frontier_int25
      end module spmd_update_frontier_int25_mod
