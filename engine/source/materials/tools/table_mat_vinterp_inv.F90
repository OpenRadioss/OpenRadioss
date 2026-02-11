!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    table_mat_vinterp_inv_mod   ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||--- called by ------------------------------------------------------
!||    sigeps123                   ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c                  ../engine/source/materials/mat/mat123/sigeps123c.F90
!||    strainrate_dependency       ../engine/source/materials/mat/mat123/strainrate_dependency.F90
!||====================================================================
      module table_mat_vinterp_inv_mod
      contains
!||====================================================================
!||    table_mat_vinterp_inv   ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||--- called by ------------------------------------------------------
!||    sigeps123               ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c              ../engine/source/materials/mat/mat123/sigeps123c.F90
!||    strainrate_dependency   ../engine/source/materials/mat/mat123/strainrate_dependency.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table4d_mod             ../common_source/modules/table4d_mod.F
!||====================================================================
      subroutine table_mat_vinterp_inv(table,dimx,nel,ipos,xx,yy,dydx)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use table4d_mod
      use precision_mod, only : WP
      use constant_mod , only : one, two, zero, three
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(table_4d_)                    ,intent(in)    :: table
      integer                            ,intent(in)    :: dimx
      integer                            ,intent(in)    :: nel
      real(kind=wp), dimension(nel,dimx),intent(in)  :: xx
      integer, dimension(nel,dimx),intent(inout) :: ipos
      real(kind=wp), dimension(nel)            ,intent(inout) :: yy
      real(kind=wp), dimension(nel)            ,intent(inout) :: dydx
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      logical, dimension(nel) :: need_to_compute
      integer  i,j,k,m,n,i1,i2,ndim
      integer :: mindx_1,mindx_2
      integer :: nindx_1,nindx_2
      integer, dimension(nel) :: indx_1,indx_2
      integer, dimension(4)   :: ldim
      real(kind=wp) :: dx, alpha, alphai
      real(kind=wp), dimension(nel,4) :: fac
! ----------------------------------------------------------------------------------------------------------------------
!                                                   
! ----------------------------------------------------------------------------------------------------------------------
      ndim = table%ndim
! -----
      do k=1,ndim
        ldim(k) = size(table%y1d)
      end do
      !
      do k=1,ndim
        ipos(1:nel,k) = max(ipos(1:nel,k),1)
        nindx_1 = 0
        mindx_1 = 0
        nindx_2 = 0
        mindx_2 = ldim(k) + 1
#include "vectorize.inc"
        do i=1,nel
          m  = ipos(i,k) 
          dx = table%y1d(m) - xx(i,k)
          if (dx >= zero)then
            nindx_1 = nindx_1 + 1
            indx_1(nindx_1) = i
            mindx_1 = max(mindx_1,m)
          else
            nindx_2 = nindx_2 + 1
            indx_2(nindx_2) = i
            mindx_2 = min(mindx_2,m)
          endif
        enddo

        need_to_compute(1:nindx_1) = .true.
        do n = mindx_1,1,-1
#include "vectorize.inc"
          do j=1,nindx_1
            if(need_to_compute(j)) then
              i = indx_1(j)
              m = ipos(i,k)
              dx = table%y1d(n)  - xx(i,k)
              if (dx < zero .or. n <= 1) then 
                ipos(i,k)=max(n,1)
                need_to_compute(j) = .false.
              endif
            endif
          enddo
        enddo
        !
        need_to_compute(1:nindx_2) = .true.
        !
        do n=mindx_2,ldim(k)
#include "vectorize.inc"
          do j=1,nindx_2
            if (need_to_compute(j)) then
              i = indx_2(j)
              m = ipos(i,k) 
              dx = table%y1d(n)  - xx(i,k)
              if (dx >= zero .or. n == ldim(k)) then
                ipos(i,k) = n-1
                need_to_compute(j) = .false.
              endif
            endif
          enddo
        enddo

      enddo ! k=1,ndim
      !
      do k=1,ndim
#include "vectorize.inc"
        do i=1,nel
          n = ipos(i,k)
          fac(i,k) = (table%y1d(n+1)  - xx(i,k))                    &
                   / (table%y1d(n+1)  - table%y1d(n) )
        end do
      end do
!----------------------------------------------
      select case(ndim)
       case(1)
!
#include "vectorize.inc"
        do i=1,nel
          i1 = ipos(i,1)
          i2 = i1 + 1
          alpha  = fac(i,1)
          alphai = one - alpha                                                                 
!
          yy(i)   = alpha*table%x(1)%values(i1)+ alphai*table%x(1)%values(i2)
          dydx(i) =  (table%x(1)%values(i2) - table%x(1)%values(i1))  &
                      / (table%y1d(i2) - table%y1d(i1))
        end do
!----
      end select
!-----------
      return
      end subroutine table_mat_vinterp_inv
      !-----------
      end module table_mat_vinterp_inv_mod