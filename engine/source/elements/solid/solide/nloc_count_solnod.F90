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
      module nloc_count_solnod_mod
      contains
! ======================================================================================================================
! \brief count non-local effectives nodes for 8 nodes solid elements
! \details isolate only effective nodes to consider 8 nodes degenerated bricks
! ======================================================================================================================
        subroutine nloc_count_solnod(                                 &
          ngroup   ,nparg    ,iparg    ,elbuf_tab,ixs      ,    &
          nixs     ,numels   )
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
          use elbufdef_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: ngroup                                       !< number of groups
          integer, intent(in) :: nparg                                        !< number of group parameters
          integer, dimension(nel), intent(in) :: iparg(nparg,ngroup)          !< failure counter in 1st direction
          type (elbuf_struct_), dimension(ngroup), intent(inout) :: elbuf_tab !< element buffer structure
          integer, dimension(nixs,numels), intent(in) :: ixs                  !< solid element related data table
          integer, intent(in) :: nixs                                         !< number of solid element parameters
          integer, intent(in) :: numels                                       !< total number of solid elements
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,j,ng,nel,nft,ity,isolnod,inloc,error
          integer, dimension(8) :: node_id,index
          logical, dimension(8) :: bool
!=======================================================================
!
          ! Loop over all groups
          do ng = 1,ngroup
            nel = iparg(2,ng) ! -> Number of elements in the group
            nft = iparg(3,ng) ! -> Address of the first element of the group
            ity = iparg(5,ng) ! -> Element type of the group
            isolnod = iparg(28,ng) ! -> Number of nodes for each element
            inloc = iparg(78,ng) ! -> Non-local flag for the group
            ! Looking for non-local solid elements with 8 nodes
            if ((ity == 1).and.(isolnod == 8).and.(inloc > 0)) then
              elbuf_tab(ng)%nlocs%nl_isolnod(1:nel) = 0
              elbuf_tab(ng)%nlocs%nl_solnod(1:8,1:nel) = 0
              ! Loop over elements of the group
              do i = 1,nel
                do j=1,8
                  node_id(j) = ixs(1+j,i+nft)
                enddo
                call myqsort_int(8,node_id,index,error)
                bool(1:8) = .false.
                bool(index(1)) = .true.
                do j=2,8
                  if (node_id(j) /= node_id(j-1)) then
                    bool(index(j))=.true.
                  endif
                enddo
                ! Loop over the nodes of the element
                do j = 1,8
                  if (bool(j)) then
                    elbuf_tab(ng)%nlocs%nl_isolnod(i) = elbuf_tab(ng)%nlocs%nl_isolnod(i) + 1
                    elbuf_tab(ng)%nlocs%nl_solnod(elbuf_tab(ng)%nlocs%nl_isolnod(i),i) = ixs(1+j,i+nft)
                  endif
                enddo
              enddo
            endif
          enddo
!
        end subroutine nloc_count_solnod
!-------------------
      end module nloc_count_solnod_mod
