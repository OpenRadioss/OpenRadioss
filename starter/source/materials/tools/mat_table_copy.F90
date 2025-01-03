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
! ----------------------------------------------------------------------------------------------------------------------
!
       module mat_table_copy_mod
       contains

!! \brief  copy tabulated function input to local table storage in mat_param
!! \detail needs mat_param with allocated table array : mat_param%ntable > 0
!! \detail                 with initialized table Ids : mat_param%table(i)%notable
!! \detail input functions/tables will be copied to local mat_param table storage

       subroutine mat_table_copy(mat_param ,ntable ,table  ,ierr   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use matparam_def_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
!-----------------------------------------------
!   included files
! ----------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer, intent(in)  :: ntable                         !< number of function tables in input deck
      type(ttable), dimension(ntable) ,intent(in) :: table   !< input table array
      type(matparam_struct_) ,intent(inout) :: mat_param     !< material model data structure
      integer, intent(out) :: ierr                           !< output error flag : no error=0 , error=1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,j,k,l,itab                     
      integer :: func_id,func_n                              
      integer :: nfunc,ndim                             
      integer :: lx1,lx2,lx3,lx4
      integer, dimension(:) ,allocatable :: ifunc
!=========================================================================================
!     copy global functions/tables to mat_param data structure
!--------------------------------------------------------------------------
      nfunc = mat_param%ntable
      allocate (ifunc(nfunc))
      ierr = 0
      do itab = 1,nfunc
        ifunc(itab) = mat_param%table(itab)%notable
        call mattab_usr2sys(mat_param%title,mat_param%mat_id,ntable,table,nfunc,ifunc)
        if (ifunc(itab) == 0) then
          ierr = 1
          mat_param%table(itab)%notable = 0
        end if
      end do
!   
      if (ierr == 0) then
!
        mat_param%ntable = nfunc
!
        do itab = 1,nfunc
          func_id = mat_param%table(itab)%notable     ! input table Id
          func_n  = ifunc(itab)                       ! internal input table number
          ndim = table(func_n)%ndim                   ! table dimension
          mat_param%table(itab)%notable = func_id
          mat_param%table(itab)%ndim = ndim
          allocate (mat_param%table(itab)%x(ndim))
!          
          if (ndim == 1) then
            lx1 = size(table(func_n)%x(1)%values)      ! number of abscissa points
            allocate (mat_param%table(itab)%x(1)%values(lx1))
            allocate (mat_param%table(itab)%y1d(lx1))
            mat_param%table(itab)%x(1)%values(1:lx1) = table(func_n)%x(1)%values(1:lx1)
            mat_param%table(itab)%y1d(1:lx1) = table(func_n)%y%values(1:lx1)
            
          else if (ndim == 2) then
            lx1 = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            allocate (mat_param%table(itab)%x(1)%values(lx1))
            allocate (mat_param%table(itab)%x(2)%values(lx2))
            allocate (mat_param%table(itab)%y2d(lx1,lx2))
            mat_param%table(itab)%x(1)%values(1:lx1) = table(func_n)%x(1)%values(1:lx1)
            mat_param%table(itab)%x(2)%values(1:lx2) = table(func_n)%x(2)%values(1:lx2)
            do i=1,lx1
              do j=1,lx2
                 mat_param%table(itab)%y2d(i,j) = table(func_n)%y%values((j-1)*lx1+i)
              end do
            end do
            
          else if (ndim == 3) then
            lx1  = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            lx3 = size(table(func_n)%x(3)%values)
            allocate (mat_param%table(itab)%x(1)%values(lx1))
            allocate (mat_param%table(itab)%x(2)%values(lx2))
            allocate (mat_param%table(itab)%x(3)%values(lx3))
            allocate (mat_param%table(itab)%y3d(lx1,lx2,lx3))
            mat_param%table(itab)%x(1)%values(1:lx1) = table(func_n)%x(1)%values(1:lx1)
            mat_param%table(itab)%x(2)%values(1:lx2) = table(func_n)%x(2)%values(1:lx2)
            mat_param%table(itab)%x(3)%values(1:lx3) = table(func_n)%x(3)%values(1:lx3)
            do i=1,lx1
              do j=1,lx2
                do k=1,lx3
                  mat_param%table(itab)%y3d(i,j,k) =                  &
                  table(func_n)%y%values((k-1)*lx1*lx2+(j-1)*lx1+i)
                end do
              end do
            end do
            
          else if (ndim == 4) then
            lx1 = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            lx3 = size(table(func_n)%x(3)%values)
            lx4 = size(table(func_n)%x(4)%values)
            allocate (mat_param%table(itab)%x(1)%values(lx1))
            allocate (mat_param%table(itab)%x(2)%values(lx2))
            allocate (mat_param%table(itab)%x(3)%values(lx3))
            allocate (mat_param%table(itab)%x(4)%values(lx4))
            allocate (mat_param%table(itab)%y4d(lx1,lx2,lx3,lx4))
            mat_param%table(itab)%x(1)%values(1:lx1) = table(func_n)%x(1)%values(1:lx1)
            mat_param%table(itab)%x(2)%values(1:lx2) = table(func_n)%x(2)%values(1:lx2)
            mat_param%table(itab)%x(3)%values(1:lx3) = table(func_n)%x(3)%values(1:lx3)
            mat_param%table(itab)%x(4)%values(1:lx4) = table(func_n)%x(4)%values(1:lx4)
            do i=1,lx1
              do j=1,lx2
                do k=1,lx3
                  do l=1,lx4
                    mat_param%table(itab)%y4d(i,j,k,l) =                  &
                    table(func_n)%y%values((l-1)*lx1*lx2*lx3+(k-1)*lx1*lx2+(j-1)*lx1+i)
                  end do
                end do
              end do
            end do
!
          end if   ! ndim      
        end do     ! nfunc
      end if       ! ierr
!
      deallocate (ifunc)
!------------------------------
      return
      end
      end module mat_table_copy_mod
