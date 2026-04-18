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
!||    mat_func_deintersect_mod   ../starter/source/materials/tools/mat_func_deintersect.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76              ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||====================================================================
       module mat_func_deintersect_mod
       contains
       
!||====================================================================
!||    mat_func_deintersect        ../starter/source/materials/tools/mat_func_deintersect.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76               ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||--- calls      -----------------------------------------------------
!||    polyline_intersection       ../starter/source/materials/tools/polyline_intersection.F90
!||    table_values_2d             ../starter/source/materials/tools/table_values_2d.F
!||--- uses       -----------------------------------------------------
!||    polyline_intersection_mod   ../starter/source/materials/tools/polyline_intersection.F90
!||====================================================================
       subroutine mat_func_deintersect(func1 ,func2  ,ierror, intersections)

 ! \brief check intersection between 1d or 2d function tables
 ! \details in case of 1d function intersection, the functions are deintersected automatically 
 ! \details if one of the functions is 2d,  only the 1st dim is deintersected 
 ! \details Input:
 ! \details   func1 ,func2 : two function tables (1d or 2d) 
 ! \details Output:
 ! \details   deintersected functions replace the original one in table structure 

          ! ---------------------------------------------------------------------------------
          !                modules
          ! ---------------------------------------------------------------------------------
          use table4d_mod
          use polyline_intersection_mod
          use precision_mod, only : WP
          use constant_mod , only : zero,one,em9
! -------------------------------------------------------------------------------------------
          implicit none
! -------------------------------------------------------------------------------------------
!                                              D u m m y a r g u m e n t s
! -------------------------------------------------------------------------------------------
          type(table_4d_) ,intent(inout) :: func1,func2
          integer         ,intent(out)   :: ierror
          integer         ,intent(out)   :: intersections
! -------------------------------------------------------------------------------------------
!         Local variables
! -------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: ndim1,ndim2
          integer :: npt,nptx,npt1,npt2,nxd1,nxd2
          integer , dimension(:) ,allocatable :: perm
          real(kind=WP) :: xint,yint
          real(kind=WP) :: yr1,yr2,epsy
          real(kind=WP), dimension(:)   ,allocatable :: xtmp,x1,x2,xf,y1,y2
          real(kind=WP), dimension(:,:) ,allocatable :: yf1,yf2
          logical :: found
! ==================================================================================================
          ierror = 0
          intersections = 0
          ndim1 = func1%ndim
          ndim2 = func2%ndim
          npt1  = size(func1%x(1)%values)
          npt2  = size(func2%x(1)%values)
!                    
          allocate (x1(npt1))
          allocate (y1(npt1))
          allocate (x2(npt2))
          allocate (y2(npt2))
          x1(1:npt1) = func1%x(1)%values(1:npt1)
          x2(1:npt2) = func2%x(1)%values(1:npt2)
          if (ndim1 == 1) then
            y1(1:npt1) = func1%y1d(1:npt1)
            nxd1 = 1
          else
            y1(1:npt1) = func1%y2d(1:npt1,1)
            nxd1 = size(func1%x(2)%values)
          end if
          if (ndim2 == 1) then
            y2(1:npt2) = func2%y1d(1:npt2)
            nxd2 = 1
          else
            y2(1:npt2) = func2%y2d(1:npt2,1)
            nxd2 = size(func2%x(2)%values)
          end if
          yr1 = maxval(abs(y1 - sum(y1)/dble(npt1)))
          yr2 = maxval(abs(y2 - sum(y2)/dble(npt2)))
!
          ! check intersections between static func1 and func2
!          
          call polyline_intersection(npt1, npt2, x1, y1, x2, y2, xint, yint, found)
!
!---------------------------------------------------------------------------------------------------            
          if (found) then         ! deintersect functions func1 and func2
!
            ! create sorted common absissa
            nptx  = npt1 + npt2
            allocate (xf(nptx))
            allocate (xtmp(nptx))
            allocate (perm(nptx))
            xtmp(1:npt1)      = x1(1:npt1)
            xtmp(npt1+1:nptx) = x2(1:npt2)
            call myqsort(nptx,xtmp,perm,ierror)
            npt = 1
            xf(1) = xtmp(1)
            do i = 1,nptx
              if (xtmp(i) > xf(npt)) then
                npt = npt + 1
                xf(npt) = xtmp(i)
              end if
            end do
            deallocate(perm)
            deallocate(xtmp)
!---------------------------------------------------------          
            ! reallocate both functions and interpolate all values using common abscissa
            deallocate(func2%x(1)%values)
            deallocate(func1%x(1)%values)
            allocate(func1%x(1)%values(npt))
            allocate(func2%x(1)%values(npt))
            allocate (yf1(npt,nxd1))
            allocate (yf2(npt,nxd2))
            func1%x(1)%values(1:npt) = xf(1:npt)
            func2%x(1)%values(1:npt) = xf(1:npt)
!
            if (ndim1 == 1) then
              call table_values_2d(npt1 ,npt ,x1 ,y1 ,xf ,yf1(1:npt,1))
            else
              do j = 1,nxd1
                y1(1:npt1) = func1%y2d(1:npt1,j)
                call table_values_2d(npt1 ,npt ,x1 ,y1 ,xf ,yf1(1:npt,j))
              end do              
            end if
!
            if (ndim2 == 1) then
              call table_values_2d(npt2 ,npt ,x2 ,y2 ,xf ,yf2(1:npt,1))
            else
              do j = 1,nxd2
                y2(1:npt2) = func2%y2d(1:npt2,j)
                call table_values_2d(npt2 ,npt ,x2 ,y2 ,xf ,yf2(1:npt,j))
              end do              
            end if
!---------------------------------------------------------          
            ! deintersect 1 dim of functions func1 and func2
                
            epsy = em9 * max(yr1, yr2)  ! estimated range of y values * 1.e-9
!
            do j = 1,npt
              if (yf1(j,1) /= yf2(j,1)) exit
            end do
            j = min(j,npt)
!
            if (yf1(j,1) > yf2(j,1)) then
              do i = j,npt
                if (yf1(i,1) < yf2(i,1)) then
                  yf1(i,1) = yf2(i,1) + epsy
                  intersections = 1
                end if
              end do 
            else
              do i = j,npt
                if (yf2(i,1) < yf1(i,1)) then
                  yf2(i,1) = yf1(i,1) + epsy
                  intersections = 1
                end if
              end do 
            end if
!
            if (ndim1 == 1) then
              deallocate(func1%y1d)
              allocate(func1%y1d(npt))
              func1%y1d(1:npt) = yf1(1:npt,1)
            else
              deallocate(func1%y2d)
              allocate(func1%y2d(npt,nxd1))
              func1%y2d(1:npt,1:nxd1) = yf1(1:npt,1:nxd1)
            end if                            
            if (ndim2 == 1) then
              deallocate(func2%y1d)
              allocate(func2%y1d(npt))
              func2%y1d(1:npt) = yf2(1:npt,1)
            else
              deallocate(func2%y2d)
              allocate(func2%y2d(npt,nxd2))
              func2%y2d(1:npt,1:nxd2) = yf1(1:npt,1:nxd2)
            end if                            
!
            deallocate(yf1)
            deallocate(yf2)
            deallocate(xf)
            deallocate(y2)
            deallocate(x2)
            deallocate(x1)
            deallocate(y1)
!---------------------------------------------------------          
          end if  ! found intersection
!-----------------------------------------------------------         
          end subroutine mat_func_deintersect
          end module     mat_func_deintersect_mod
