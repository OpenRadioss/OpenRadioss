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
!||    table2d_deintersection_mod   ../starter/source/materials/tools/table2d_deintersection.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76                ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||====================================================================
       module table2d_deintersection_mod
         implicit none
         private
         public :: table2d_deintersection

       contains
        !----------------------------------------------------------------------
        ! \brief  Takes a 2D piecewise-linear curves y = f(x1,x2) as input
        ! \brief  Looks for intersections between functions y(:,i) and y(:,j), j=i+1 
        ! \brief  and corrects them automatically, imposing y(:,j) > y(:,i)
        ! \details  First abscissa vector x1 is supposed to be common for all x2 values
        ! \details  due to construction of material function tables
        ! \details  Output :
        ! \details  Corrected deintersected function table and number of intersection found
        !----------------------------------------------------------------------

!||====================================================================
!||    table2d_deintersection   ../starter/source/materials/tools/table2d_deintersection.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76            ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine table2d_deintersection(mat_table ,intersections)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use table4d_mod
          use constant_mod  ,only : ep06
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global Arguments
! --------------------------------------------------------------------------------------------------
          integer                   ,intent(out)   :: intersections
          type(table_4d_)           ,intent(inout) :: mat_table !< target material table structure
!-----------------------------------------------
!         L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: i,j       
        integer :: npx1,npx2
        real(kind=WP) :: y1,y2,ymean,yrange,epsy
!===================================================================================================
        if (mat_table%ndim /=2) return   ! works only for table 2d
!        
        intersections = 0
        npx1   = size(mat_table%x(1)%values)
        npx2   = size(mat_table%x(2)%values)
        ymean  = sum(mat_table%y2d(1:npx1,1))/dble(npx1)
        yrange = maxval(abs(mat_table%y2d(1:npx1,1) - ymean))
        epsy = ep06 * epsilon(yrange)   ! calculated small y value increment
!
        y1 = mat_table%y2d(1,1)
        do i = 2,npx2
          do j = 1,npx1
            y1 = mat_table%y2d(j,i-1)
            y2 = mat_table%y2d(j,i)
            if (y1 >= y2) then
              intersections = intersections + 1
              y2 = y1 + epsy
            endif
          end do
        end do
!-----------------------------------------------
        end subroutine table2d_deintersection
        end module table2d_deintersection_mod
