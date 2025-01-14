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
      !||    law190_upd_mod   ../starter/source/materials/mat/mat190/law190_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat           ../starter/source/materials/updmat.F
      !||====================================================================
      module law190_upd_mod
      contains
!! \brief update material law 190
      !||====================================================================
      !||    law190_upd         ../starter/source/materials/mat/mat190/law190_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine law190_upd(  matparam ,numtabl ,itable   ,table    ,pm ,  &
        &                    npropm   ,ntable )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use matparam_def_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: npropm
          integer, intent(in) :: ntable
          integer, intent(in) :: numtabl
          integer, dimension(numtabl) :: itable
          type(matparam_struct_), target :: matparam
          type(ttable), dimension(ntable) ,intent(inout) ::  table
          my_real, dimension(npropm), intent(inout) :: pm
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          ! integer variables
          integer :: i,j,k,kk                             !< iterators
          integer :: ndim                                 !< dimension
          integer :: npt,nptmax                           !< number of integration points / max number of integration points
          integer :: fun_1,func_id,func_t,func_c,func_s   !< function identifiers
          integer :: iconv,sizetozero,ifx,ify,stat,len2,len3

          ! real variables
          my_real :: xfac
          my_real :: epdt_min
          my_real :: epdt_max,epdc_min,epdc_max,epds_min,epds_max
          my_real :: xint,yint
          my_real :: x_i,x_ii,y_i,y_ii
          my_real :: ener,scalefac,dx,dy,dydx,stiffini,nu,g,c1
          my_real ,dimension(:)  ,allocatable :: x_ener,y_ener
          type(table_4d_), dimension(:) ,pointer ::  table_mat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------
!     copy global functions/tables to matparam data structure
!--------------------------------------------------------------------------
!
          ! loading table
          fun_1 = itable(1)                       ! identifier
          ndim  = table(fun_1)%ndim               ! number of dimensions
          npt   = size(table(fun_1)%x(1)%values)  ! number of points
          matparam%ntable = numtabl               ! number of tables
          allocate (matparam%table(numtabl))      ! allocate table array
          table_mat =>  matparam%table(1:numtabl) ! material table pointer
          table_mat(1:numtabl)%notable = 0        ! initialization of table identifiers array
!
          ! scale factor
          scalefac = matparam%uparam(5)
!
          ! table number 1 - loading function
          stiffini = zero
          if (fun_1 > 0) then
            table_mat(1)%notable = fun_1
            ndim = table(fun_1)%ndim
            table_mat(1)%ndim = ndim
            allocate (table_mat(1)%x(ndim),stat=stat)
            ! loop over dimensions to copy abscissa
            do i = 1,ndim
              npt = size(table(fun_1)%x(i)%values)
              allocate (table_mat(1)%x(i)%values(npt),stat=stat)
              table_mat(1)%x(i)%values(1:npt) = table(fun_1)%x(i)%values(1:npt)
            end do
            ! 1 dimension, stress vs strain loading
            if (ndim == 1) then
              npt = size(table(fun_1)%x(1)%values)
              allocate (table_mat(1)%y1d(npt),stat=stat)
              table_mat(1)%y1d(1:npt) = table(fun_1)%y%values(1:npt)
              ! 2 dimensions, stress vs strain vs strain rate
            else if (ndim == 2) then
              npt  = size(table(fun_1)%x(1)%values)
              len2 = size(table(fun_1)%x(2)%values)
              allocate (table_mat(1)%y2d(npt,len2),stat=stat)
              do i=1,npt
                do j=1,len2
                  table_mat(1)%y2d(i,j) = table(fun_1)%y%values((j-1)*npt+i)
                end do
              end do
              ! 3 dimensions, stress vs strain vs strain rate vs volume fraction
            else if (ndim == 3) then
              npt  = size(table(fun_1)%x(1)%values)
              len2 = size(table(fun_1)%x(2)%values)
              len3 = size(table(fun_1)%x(3)%values)
              allocate (table_mat(1)%y3d(npt,len2,len3),stat=stat)
              do i=1,npt
                do j=1,len2
                  do k=1,len3
                    table_mat(1)%y3d(i,j,k) = table(fun_1)%y%values((k-1)*npt*len2+(j-1)*npt+i)
                  end do
                end do
              end do
            end if
!
            ! compute initial slope
            do i = 1,npt-1
              dx = table_mat(1)%x(1)%values(i+1) - table_mat(1)%x(1)%values(i)
              dy = -huge(dy)
              if (ndim == 1) then
                dy = table_mat(1)%y1d(i+1) - table_mat(1)%y1d(i)
              elseif (ndim == 2) then
                dy = table_mat(1)%y2d(i+1,1) - table_mat(1)%y2d(i,1)
              elseif (ndim == 3) then
                dy = table_mat(1)%y3d(i+1,1,1) - table_mat(1)%y3d(i,1,1)
              endif
              dydx = scalefac*dy/dx
              if (table_mat(1)%x(1)%values(i+1) == zero)then
                stiffini = max(stiffini,dydx)
              elseif (table_mat(1)%x(1)%values(i) == zero) then
                stiffini = max(stiffini,dydx)
              elseif (table_mat(1)%x(1)%values(1) >= zero) then
                dx = table_mat(1)%x(1)%values(2) - table_mat(1)%x(1)%values(1)
                if (ndim == 1) then
                  dy = table_mat(1)%y1d(2) - table_mat(1)%y1d(1)
                elseif (ndim == 2) then
                  dy = table_mat(1)%y2d(2,1) - table_mat(1)%y2d(1,1)
                elseif (ndim == 3) then
                  dy = table_mat(1)%y3d(2,1,1) - table_mat(1)%y3d(1,1,1)
                endif
                stiffini = max(stiffini, scalefac*dy/dx)
              endif
            enddo
          end if
!
          stiffini = max(stiffini,matparam%uparam(1))
          nu = zero
          c1 = stiffini/three/(one - two*nu)
          g  = half*stiffini/(one + nu)
          matparam%uparam(1) = stiffini
          matparam%uparam(2) = c1
          matparam%uparam(3) = g
          pm(20) = stiffini
          pm(22) = g
          pm(24) = stiffini
          pm(32) = c1
!
          ! table number 2 - loading function
          table_mat(2)%notable = 2
          table_mat(2)%ndim = 1
          npt = size(table(fun_1)%x(1)%values)
          allocate(x_ener(npt),stat=stat)
          allocate(y_ener(npt),stat=stat)
          x_ener(1:npt) = zero
          y_ener(1:npt) = zero
          sizetozero = 1
          do i = 1, npt-1
            x_i  = table(fun_1)%x(1)%values(i+1)
            x_ii = table(fun_1)%x(1)%values(i)
            if(x_i >= zero .and. x_ii < zero) then
              sizetozero = i+1
              exit
            endif
          enddo
          do k = 1, npt
            x_ener(k) = table(fun_1)%x(1)%values(k)
          enddo
          y_ener(sizetozero) = zero
          do k = sizetozero+1, npt
            x_i  = table(fun_1)%x(1)%values(k)
            x_ii = table(fun_1)%x(1)%values(k-1)
            y_i  = table(fun_1)%y%values(k)*scalefac
            y_ii = table(fun_1)%y%values(k-1)*scalefac
            ener = (x_i-x_ii)*(y_i + y_ii)/two
            y_ener(k) = y_ener(k-1) + ener
          enddo
          do k = 1, sizetozero-1
            kk   = sizetozero - k
            x_i  = table(fun_1)%x(1)%values(kk)
            x_ii = table(fun_1)%x(1)%values(kk+1)
            y_i  = table(fun_1)%y%values(kk)*scalefac
            y_ii = table(fun_1)%y%values(k+1)*scalefac
            ener = (x_i-x_ii)*(y_i + y_ii)/two
            y_ener(kk) = y_ener(kk+1) + ener
          enddo
          allocate (table_mat(2)%x(ndim)          ,stat=stat)
          allocate (table_mat(2)%x(1)%values(npt) ,stat=stat)
          allocate (table_mat(2)%y1d(npt)         ,stat=stat)
          do i = 1,npt
            table_mat(2)%x(1)%values(i) = x_ener(i)
            table_mat(2)%y1d(i) = y_ener(i)
          end do
          deallocate(x_ener,y_ener)
!
        end
      end module

