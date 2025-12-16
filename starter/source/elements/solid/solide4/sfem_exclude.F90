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
      module sfem_exclude_mod

      implicit none

      contains
!=======================================================================================================================
!!\brief This subroutine get number of nodes excluded for nodal pressure
!=======================================================================================================================
        subroutine sfem_exclude_dim(                                             &
                   numnod,  nparg,  ngroup,  iparg,      ixs,                    &
                   numels, ne_sfem)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use element_mod,            only: nixs
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   )                          :: nparg            !< 1er dim of iparg
          integer, intent (in   )                          :: ngroup           !< number of element groups
          integer, intent (in   )                          :: numels           !< number solid element
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< element group data
          integer, intent (in   ) ,dimension(nixs,numels)  :: ixs              !< solid connectivity
          integer, intent (inout)                          :: ne_sfem          !< number node of excluded sfem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,ii,mid,ity,ng,nft,isfem,ne,isolnod,isrot,icpre,nel
          integer, dimension(:)  ,        allocatable :: imid
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(imid,numnod)
          imid = 0
          ne_sfem = 0
          do ng=1,ngroup
            ity = iparg(5,ng)
            nel = iparg(2,ng)
            nft = iparg(3,ng)
            isolnod = iparg(28,ng)
            isrot  = iparg(41,ng)
            icpre  = iparg(10,ng)
            if(iparg(8, ng) == 1 .or. ity /= 1) cycle
            isfem =0
            if(isolnod==4.and.isrot == 3) isfem=1
            if(icpre>0.and.(isolnod==10.or.(isolnod==4.and.isrot == 1))) isfem=1
            if (isfem==1) then 
              mid = ixs(1,nft + 1)
              do i = 1, nel
                ii = nft + i
                do j = 1, 4
                  n = ixs(1+j,ii)
                  if (imid(n)==0) then 
                      imid(n) = mid
                  else if (imid(n) /= mid.and.imid(n) >0) then
                      ne_sfem = ne_sfem + 1
                      imid(n) = -mid
                  end if
                end do
              end do
            end if !(isfem==1) then 
          end do !ng=1,ngroup
          deallocate(imid)
!
        end subroutine sfem_exclude_dim
!=======================================================================================================================
!!\brief This subroutine do the initialization of list of nodes excluded for nodal pressure
!=======================================================================================================================
        subroutine sfem_exclude_ini(                                             &
                   numnod,  nparg,  ngroup,  iparg,      ixs,                    &
                   numels,in_sfem, ne_sfem)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use element_mod,            only: nixs
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   )                          :: nparg            !< 1er dim of iparg
          integer, intent (in   )                          :: ngroup           !< number of rwall groups
          integer, intent (in   )                          :: numels           !< number solid element
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< element group data
          integer, intent (in   ) ,dimension(nixs,numels)  :: ixs              !< solid connectivity
          integer, intent (in   )                          :: ne_sfem          !< number node of excluded sfem
          integer, intent (inout) ,dimension(ne_sfem)      :: in_sfem          !< list of excluded nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,ii,mid,ity,ng,nft,isfem,ne,isolnod,isrot,icpre,nel
          integer, dimension(:)  ,        allocatable :: imid
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(imid,numnod)
          imid = 0
          ne = 0
          do ng=1,ngroup
            ity = iparg(5,ng)
            nel = iparg(2,ng)
            nft = iparg(3,ng)
            isolnod = iparg(28,ng)
            isrot  = iparg(41,ng)
            icpre  = iparg(10,ng)
            if (iparg(8,ng) == 1 .or. ity /= 1) cycle
            isfem =0
            if (isolnod==4.and.isrot == 3) isfem=1
            if (icpre>0.and.(isolnod==10.or.(isolnod==4.and.isrot == 1))) isfem=1
            if (isfem==1) then 
                mid = ixs(1,nft + 1)
                do i = 1, nel
                  ii = nft + i
                  do j = 1, 4
                    n = ixs(1+j,ii)
                    if (imid(n)==0) then 
                      imid(n) = mid
                    else if (imid(n) /= mid .and. imid(n) >0) then
                      ne = ne + 1
                      in_sfem(ne) = n
                      imid(n) = -mid
                    end if
                  end do
                end do
            end if !(isfem==1) then 
           end do !ng=1,ngroup
          deallocate(imid)
!
        end subroutine sfem_exclude_ini
!
      end module sfem_exclude_mod
