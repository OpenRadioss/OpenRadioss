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
      module offset_nproj_mod
      contains
!=======================================================================================================================
!!\brief This subroutine do nodal offset projection for shell
!=======================================================================================================================
        subroutine offset_nproj(nspmd,numnod,xyz,sh_offset_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                  s                                 Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,em20
          use inter_sh_offset_mod , only:sh_offset_
!
          implicit none
!
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
          integer, intent(in   )                      :: numnod          !< number of node
          integer, intent(in   )                      :: nspmd           !< number domains
          my_real, intent(inout),dimension(3,numnod)  :: xyz             !< node coordinates
          type (sh_offset_)                           :: sh_offset_tab   !< offset struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,k,n,nnod,lenr
          my_real  shelloff,r(3),s(3),t(3),xv(3,4),norm2
          double precision  dx(3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          sh_offset_tab%norm_n = zero
          do i=1,sh_offset_tab%nsh_oset
            do k = 1,4
              n = sh_offset_tab%ix_offset(k,i)
              xv(1:3,k) = xyz(1:3,n)
            end do
            if (sh_offset_tab%ix_offset(4,i)/=sh_offset_tab%ix_offset(3,i)) then
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
            t(1) = r(2) * s(3) - r(3) * s(2)
            t(2) = r(3) * s(1) - r(1) * s(3)
            t(3) = r(1) * s(2) - r(2) * s(1)
            norm2 = t(1)*t(1)+t(2)*t(2)+t(3)*t(3)
            t(1:3) = t(1:3)/sqrt(max(em20,norm2))
            do k = 1,nnod
              n = sh_offset_tab%ix_offset(k,i)
              sh_offset_tab%norm_n(1:3,n) = sh_offset_tab%norm_n(1:3,n) + t(1:3)
            end do
          enddo
!
          if (nspmd>1) then
            lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
            call spmd_exch_n(sh_offset_tab%norm_n,sh_offset_tab%iad_offset,sh_offset_tab%fr_offset,  &
              lenr)
          end if
!
          do n = 1, numnod
            if (sh_offset_tab%intag(n) == 0) cycle
            norm2 = sh_offset_tab%norm_n(1,n)*sh_offset_tab%norm_n(1,n)+                   &
              sh_offset_tab%norm_n(2,n)*sh_offset_tab%norm_n(2,n)+                   &
              sh_offset_tab%norm_n(3,n)*sh_offset_tab%norm_n(3,n)
            sh_offset_tab%norm_n(1:3,n) = sh_offset_tab%norm_n(1:3,n)/sqrt(max(em20,norm2))
            dx(1:3) = sh_offset_tab%norm_n(1:3,n)*sh_offset_tab%offset_n(n)
            xyz(1:3,n) = xyz(1:3,n) + dx(1:3)
          end do
!
        end subroutine offset_nproj
!
      end module offset_nproj_mod
