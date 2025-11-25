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
      module hierarchy_rbody_mod
      contains
! ======================================================================================================================
! \brief rbody hierarchy initialization
! ======================================================================================================================
        subroutine hierarchy_rbody(nrbykin ,nnpby ,npby  ,slpby ,lpby  ,            &
                                   nrby    ,rby   ,numnod,iout )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: iout            !< out file unit
          integer, intent(in)                                      :: nrbykin         !< number of rbody
          integer, intent(in)                                      :: nnpby           !< 1er dimension of npby
          integer, intent(in)                                      :: nrby            !< 1er dimension of rby
          integer, intent(in)                                      :: slpby           !< dimesion of lpby
          integer, dimension(nnpby,nrbykin),    intent(inout)      :: npby            !< rbody data
          integer, dimension(slpby),            intent(inout)      :: lpby            !< rbodysecondary node data
          real(kind=WP),dimension(nrby,nrbykin),intent(inout)      :: rby             !< rbody data
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,m,iad,nhier,ih,parent_idx,nsn,ns,iter,nh_max,iad_n
          logical :: changed,is_hier
          integer, dimension(nrbykin) :: index,nlev
          integer, dimension(:,:), allocatable :: npby_copy
          integer, dimension(:), allocatable :: itag,lpby_copy
          integer, dimension(nrbykin) :: parent_of    !< parent index for each rbody (0 = no parent)
          real(kind=WP),dimension(:,:),allocatable   :: rby_copy             
! ======================================================================================================================
      call my_alloc(itag,numnod)
!--------supposing after merging : no m in multi rbody---------------------------------------
      itag = 0
      do i=1,nrbykin
        m = npby(1,i)
        if (m >= 1 .and. m <= numnod) then
          if (itag(m)==0) itag(m) = i
        end if
      enddo

!--------finding parent rbody---------------------------------------
      parent_of = 0
      do i=1,nrbykin
        iad = npby(11,i)
        nsn = npby(2,i)
        do j = 1,nsn
          ns = lpby(iad+j)
          parent_idx = itag(ns)
          if (parent_idx > 0 .and. parent_idx /= i) then
            if (parent_of(i) == 0) then
              parent_of(i) = parent_idx       ! assign first found parent
            end if
          end if
        end do
      end do

!------ initialize levels: roots (no parent) -> level 0, others unknown (-1) ------
      nlev = -1
      is_hier = .false.
      do i=1,nrbykin
        if (parent_of(i) == 0) then
          nlev(i) = 0
        else 
          is_hier = .true.
        end if
      end do
      if (is_hier) then
!------ propagate levels up the parent chain until stable -------------------------
        changed = .true.
        iter = 0
        nh_max = nrbykin -1
        do while (changed .and. iter < nh_max)
          changed = .false.
          iter = iter + 1
          do i = 1, nrbykin
            if (nlev(i) == -1) then
              parent_idx = parent_of(i)
            ! normally parent_idx > 0 here
              if (nlev(parent_idx) /= -1) then
                nlev(i) = nlev(parent_idx) + 1
                changed = .true.
              end if
            end if
          end do
        end do
!------ break cycles / unresolved entries by setting level 0 (safe fallback) -----
        nhier = 0
        do i=1,nrbykin
          if (nlev(i) == -1) nlev(i) = 0
          nhier = max(nhier,nlev(i))
        end do
!------ build index array ordered by increasing hierarchy level -------------------
        k = 0
        do ih = 0 ,nhier
          do i = 1, nrbykin
            if (nlev(i) == ih) then
              k = k + 1
              index(k) = i
            end if
          end do
        end do 
!------ reorder npby and lpby according to hierarchy -------------------------------
        call my_alloc(npby_copy,nnpby,nrbykin)
        call my_alloc(lpby_copy,slpby)
        call my_alloc(rby_copy,nrby,nrbykin)
        npby_copy = npby
        lpby_copy = lpby
        rby_copy = rby
        iad_n = 0
        do j=1,nrbykin
           i = index(j)
           npby(1:nnpby,j) = npby_copy(1:nnpby,i)
           rby(1:nrby,j)  = rby_copy(1:nrby,i)
           nsn = npby_copy(2,i)
           iad = npby_copy(11,i)
           lpby(iad_n+1:iad_n+nsn) = lpby_copy(iad+1:iad+nsn)
           npby(11,j) = iad_n
           npby(20,j) =nlev(i)  ! store level in npby(20,:)
           iad_n = iad_n + nsn
        end do
        write(iout,1000) nhier
        deallocate(npby_copy)
        deallocate(lpby_copy)
        deallocate(rby_copy)
      end if !(is_hier) then

      deallocate(itag)
1000  FORMAT(/10X,'RIGID BODY HIERARCHY LEVEL. . . . . . . . . . . :',I10        & 
             /10X,'RIGID BODY IS REORDERED  ')

        end subroutine hierarchy_rbody
! ======================================================================================================================
! \brief rbody hierarchy domain decomposition initialization
! ======================================================================================================================
        subroutine hierarchy_rbody_ddm(nrbykin ,nnpby ,npby  ,slpby ,lpby  ,numnod,nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: nspmd           !< number of domains
          integer, intent(in)                                      :: nrbykin         !< number of rbody
          integer, intent(in)                                      :: nnpby           !< 1er dimension of npby
          integer, intent(in)                                      :: slpby           !< dimesion of lpby
          integer, dimension(nnpby,nrbykin),    intent(in   )      :: npby            !< rbody data
          integer, dimension(slpby),            intent(in   )      :: lpby            !< rbodysecondary node data
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
         integer ::  nlocal
         external nlocal    
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,m,iad,nhier,p,nsn,ns
          logical :: changed,is_hier
          integer, dimension(nrbykin) :: index,nlev
          integer, dimension(:,:), allocatable :: npby_copy
          integer, dimension(:), allocatable :: itag,lpby_copy
          integer, dimension(nrbykin) :: parent_of    !< parent index for each rbody (0 = no parent)
          real(kind=WP),dimension(:,:),allocatable   :: rby_copy             
! ======================================================================================================================
      nhier = 0
      do i=1,nrbykin
        nhier = max(nhier,npby(20,i))
      enddo
      if (nhier > 0) then
        call my_alloc(itag,numnod)
        itag = 0
        do i=1,nrbykin
          m = npby(1,i)
          if (itag(m)==0) itag(m) = i
        enddo        
!-----  --m of high level should be in the same p than his son---------------------------------------
        do i=1,nrbykin
          m = npby(1,i)
          iad = npby(11,i)
          nsn = npby(2,i)
          do j = 1,nsn
            ns = lpby(iad+j)
            if (itag(ns) > 0 ) then
               do p = 1,nspmd
                 if(nlocal(ns,p)/=0.and.nlocal(m,p)==0) call ifrontplus(m,p)
               end do
            end if
          end do
        end do
        deallocate(itag)
      end if
!
        end subroutine hierarchy_rbody_ddm
      end module hierarchy_rbody_mod
