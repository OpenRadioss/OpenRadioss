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
!hd|====================================================================
!hd|  edge_connect
!hd|-- called by -----------
!hd|-- calls ---------------
!hd|====================================================================
      module fractal_element_neighbor_mod
      contains
! ========================================================================================
! \brief creates element neighbor connections through common edges for random walk algo
!! \details 

! ========================================================================================

      subroutine fractal_element_neighbor(fractal, nixc ,ixc ,nixtg ,ixtg ,numelc ,numeltg)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use fail_param_mod
      use message_mod
      use groupdef_mod
      use setdef_mod
      use random_walk_def_mod
      use constant_mod ,only : zero,one
! ---------------------------------------------------------------------------------------------
      implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
      integer             ,intent(in)    :: numelc           !< total number of 4n shells
      integer             ,intent(in)    :: numeltg          !< total number of 3n shells
      integer             ,intent(in)    :: nixc             !< size of 4n shell conectivity table
      integer             ,intent(in)    :: nixtg            !< size of 3n shell conectivity table
      integer ,dimension(nixc,numelc)  ,intent(in) :: ixc    !< 4n shell connectivity table
      integer ,dimension(nixtg,numeltg),intent(in) :: ixtg   !< 3n shell connectivity table
      type (fractal_)  ,intent(inout) :: fractal             !< fractal structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,k,k1,k2,i1,i2,i1m,i2m
      integer :: ielem,iedge
      integer :: nedge,max_nedge
      integer :: nshell,nshell_3n,nshell_4n,nix
      integer :: elem1,elem2
      integer ,dimension(4) :: nextk4
      integer ,dimension(3) :: nextk3
      integer ,dimension(70000)  :: iwork
      integer ,dimension(:,:)  ,allocatable :: edge1,edge2
      integer ,dimension(:)    ,allocatable :: indx,nixel,elmat
      integer :: mid,ibid
      data nextk4/2,3,4,1/
      data nextk3/2,3,1/
!=======================================================================
      if (numeltg > 0) ibid = ixtg(1,1)
      
!-----------------------------------------------------------------------
      allocate (elmat(numelc+numeltg))
      allocate (nixel(numelc+numeltg))
      nixel(:)  = 0
      
      ! create list of shell elements with material law
      nshell = 0
      do i = 1,numelc
        mid = ixc(1,i)
        if (mid == fractal%imat) then
          nshell = nshell + 1
          nixel(nshell) = 4
          elmat(nshell) = i
        end if
      end do
      nshell_4n = nshell
      do i = 1,numeltg
        mid = ixtg(1,i)
        if (mid == fractal%imat) then
          nshell = nshell + 1
          nixel(nshell) = 3
          elmat(nshell) = i
        end if
      end do
      nshell_3n = nshell - nshell_4n

      ! initialize random_walk structure
      
      fractal%nelem = nshell 
      allocate (fractal%random_walk(nshell))
      do i = 1,nshell
        fractal%random_walk(i)%elnum = elmat(i)
        nix = nixel(i)
        fractal%random_walk(i)%nix    = nix
        fractal%random_walk(i)%damage = zero
        if (nix == 4) then
          fractal%random_walk(i)%id =ixc(nixc,i) 
        else
          fractal%random_walk(i)%id =ixtg(nixtg,i-nshell_4n) 
        end if
        allocate (fractal%random_walk(i)%neighbor(nix))
        fractal%random_walk(i)%neighbor(:) = 0
      end do
      
!-------------------------------------
      ! build list of edges
!-------------------------------------
      max_nedge = 4*nshell_4n + 3*nshell_3n
      allocate (indx  (2*max_nedge))
      allocate (edge1(2,max_nedge))
      allocate (edge2(2,max_nedge))
      indx(:)     = 0
      edge1(:,:) = 0
      edge2(:,:) = 0
      nedge = 0
!
      do i = 1,nshell
        ielem = elmat(i)
        nix = fractal%random_walk(i)%nix
        if (nix == 4) then
          do k = 1,nix
            i1 = ixc(k+1,ielem)
            i2 = ixc(nextk4(k)+1,ielem)
            nedge = nedge + 1
            if (i2 > i1) then
              edge1(1,nedge) = i1
              edge1(2,nedge) = i2
            else
              edge1(1,nedge) = i2
              edge1(2,nedge) = i1
            end if      
            edge2(1,nedge) = i
            edge2(2,nedge) = k
          end do         
        else if (nix == 3) then
          do k = 1,nix
            i1 = ixtg(k+1,ielem)
            i2 = ixtg(nextk3(k)+1,ielem)
            nedge = nedge + 1
            if (i2 > i1) then
              edge1(1,nedge) = i1
              edge1(2,nedge) = i2
            else
              edge1(1,nedge) = i2
              edge1(2,nedge) = i1
            end if      
            edge2(1,nedge) = i
            edge2(2,nedge) = k
          end do         
        end if
      end do   
!------------------------------------------------
      ! sort edges with increasing node order
!------------------------------------------------

      call my_orders(0,iwork,edge1,indx,nedge,2)
      
!------------------------------------------------
!     create element neighbors connectivity through common edges
!------------------------------------------------
      iedge = indx(1)
      i1m   = edge1(1,iedge)
      i2m   = edge1(2,iedge)
      elem1 = edge2(1,iedge)
      k1    = edge2(2,iedge)
!----
      do i=2,nedge
        iedge = indx(i)       
        i1    = edge1(1,iedge)
        i2    = edge1(2,iedge)        
        k2    = edge2(2,iedge)
        elem2 = edge2(1,iedge)
        
        if (i2 /= i2m .or. i1 /= i1m) then    ! border edge, no neighbor
          elem1 = edge2(1,iedge)
        else                                  ! common edge, tag neighbor elements
          elem2 = edge2(1,iedge)
          fractal%random_walk(elem1)%neighbor(k1) = elem2
          fractal%random_walk(elem2)%neighbor(k2) = elem1
        endif
        i1m = i1
        i2m = i2
        k1  = k2
        elem1 = elem2
      enddo
!
      deallocate(edge2)
      deallocate(edge1)
      deallocate(indx)
      deallocate(nixel)
      deallocate(elmat)
!-----------
      return
      end
!-----------
      end module fractal_element_neighbor_mod
