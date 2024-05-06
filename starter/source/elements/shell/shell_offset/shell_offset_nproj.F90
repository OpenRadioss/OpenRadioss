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
      module sh_offset_nproj_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do nodal offset projection for shell
!=======================================================================================================================
        subroutine sh_offset_nproj(nshoset,ix_offset,numnod,xyz,shoset_n,itagn)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one,em20
!
          implicit none
!
#include "my_real.inc"
! ------------------------------------------------------------------------------
! Arguments
! ------------------------------------------------------------------------------
!
          integer, intent(in   )                      :: numnod     !< number of node
          integer, intent(in   ),dimension(numnod)    :: itagn      !< itag work array
          integer, intent(in   )                      :: nshoset    !< number of offset shell
          integer, intent(in   ),dimension(4,nshoset) :: ix_offset  !< shell connectivity
          my_real, intent(in   ),dimension(numnod)    :: shoset_n   !< nodal offset
          my_real, intent(inout),dimension(3,numnod)  :: xyz        !< node coordinates
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer i,j,k,n,nnod
          my_real  shelloff,r(3),s(3),t(3),xv(3,4),nnorme(3,4),norm2
          double precision  dx(3)
          my_real, dimension(:,:), allocatable   :: norm_nod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(norm_nod(3,numnod))
          norm_nod = zero
          do i=1,nshoset
            do k = 1,4
              n = ix_offset(k,i)
              xv(1:3,k) = xyz(1:3,n)
            end do
            if (ix_offset(4,i)/=ix_offset(3,i)) then
              nnod = 4
              do j=1,3
                r(j) = xv(j,2)+xv(j,3)-xv(j,1)-xv(j,4)
                s(j) = xv(j,3)+xv(j,4)-xv(j,1)-xv(j,2)
              end do
            else
              nnod = 3
              do j=1,3
                r(j) = xv(j,3)-xv(j,2)
                s(j) = xv(j,1)-xv(j,2)
              end do
            end if
            call normvec(r,s,t)
            do k = 1,nnod
              n = ix_offset(k,i)
              norm_nod(1:3,n) = norm_nod(1:3,n) + t(1:3)
            end do
          enddo
!
          do n = 1, numnod
            if (itagn(n) == 0) cycle
            norm2 = norm_nod(1,n)*norm_nod(1,n)+norm_nod(2,n)*norm_nod(2,n)      &
              +norm_nod(3,n)*norm_nod(3,n)
            norm_nod(1:3,n) = norm_nod(1:3,n)/sqrt(max(em20,norm2))
            dx(1:3) = norm_nod(1:3,n)*shoset_n(n)
            xyz(1:3,n) = xyz(1:3,n) + dx(1:3)
          end do
          deallocate(norm_nod)
!
        end subroutine sh_offset_nproj
      end module sh_offset_nproj_mod
