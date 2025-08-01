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
! ----------------------------------------------------------------------------------------------------------------------
!
!||====================================================================
!||    mat_table_table_copy_mod   ../starter/source/materials/tools/mat_table_table_copy.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat129             ../starter/source/materials/mat/mat129/hm_read_mat129.F90
!||====================================================================
      module mat_table_table_copy_mod
      contains

!! \brief  make a private copy of input function table to material table stored in mat_param
!! \detail one to one copy of a single input table

!||====================================================================
!||    mat_table_table_copy   ../starter/source/materials/tools/mat_table_table_copy.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat129         ../starter/source/materials/mat/mat129/hm_read_mat129.F90
!||--- calls      -----------------------------------------------------
!||    mattab_usr2sys         ../starter/source/materials/tools/mattab_usr2sys.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine mat_table_table_copy(mat_table ,tab_id,   mat_title ,mat_id  ,     &
          x1scale   ,x2scale  ,x3scale  ,x4scale  ,     &
          fscale    ,ntable   ,table    ,ierr     )
! ----------------------------------------------------------------------------------------------------------------------
!     M o d u l e s
! ----------------------------------------------------------------------------------------------------------------------
          use table4d_mod
          use names_and_titles_mod , only : nchartitle
          use constant_mod         , only : zero
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!     Included files
! ----------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!         Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer                         ,intent(in)    :: tab_id    !< input table Id
          integer                         ,intent(in)    :: ntable    !< number of function tables in input deck
          character(len=nchartitle)       ,intent(in)    :: mat_title !< material law title
          integer                         ,intent(in)    :: mat_id    !< material law Id
          my_real                         ,intent(in)    :: x1scale   !< x1 scale factor
          my_real                         ,intent(in)    :: x2scale   !< x2 scale factor
          my_real                         ,intent(in)    :: x3scale   !< x3 scale factor
          my_real                         ,intent(in)    :: x4scale   !< x4 scale factor
          my_real                         ,intent(in)    :: fscale    !< function scale factor
          type(ttable), dimension(ntable) ,intent(in)    :: table     !< input table array
          type(table_4d_)                 ,intent(inout) :: mat_table !< target material table structure
          integer                         ,intent(out)   :: ierr      !< output error flag : no error=0 , error=1
! ----------------------------------------------------------------------------------------------------------------------
!         Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,l,idebug
          integer :: func_n,nfunc
          integer :: lx1,lx2,lx3,lx4
          integer, dimension(1) :: ifunc
!=========================================================================================
          idebug = 0
          ierr   = 0
!--------------------------------------------------------
!     check the input table Id and convert into internal number
!--------------------------------------------------------
          nfunc    = 1
          ifunc(1) = tab_id
          call mattab_usr2sys(mat_title,mat_id,ntable,table,nfunc,ifunc)
!--------------------------------------------------------
          if (ifunc(1) > 0) then
            func_n = ifunc(1)
            mat_table%notable = tab_id
            mat_table%ndim    = table(func_n)%ndim
            allocate (mat_table%x(mat_table%ndim))
!
            if (mat_table%ndim == 1) then
              lx1 = size(table(func_n)%x(1)%values)      ! number of abscissa points
              allocate (mat_table%x(1)%values(lx1))
              allocate (mat_table%y1d(lx1))
              mat_table%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
              mat_table%y1d(1:lx1) = fscale*table(func_n)%y%values(1:lx1)

              if (idebug == 1) then
                do i=1,lx1
                  print*,mat_table%x(1)%values(i),mat_table%y1d(i)
                end do
              end if

            else if (mat_table%ndim == 2) then
              lx1 = size(table(func_n)%x(1)%values)
              lx2 = size(table(func_n)%x(2)%values)
              allocate (mat_table%x(1)%values(lx1))
              allocate (mat_table%x(2)%values(lx2))
              allocate (mat_table%y2d(lx1,lx2))
              mat_table%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
              mat_table%x(2)%values(1:lx2) = x2scale*table(func_n)%x(2)%values(1:lx2)
              do i=1,lx1
                do j=1,lx2
                  mat_table%y2d(i,j) = fscale*table(func_n)%y%values((j-1)*lx1+i)
                end do
              end do

            else if (mat_table%ndim == 3) then
              lx1  = size(table(func_n)%x(1)%values)
              lx2 = size(table(func_n)%x(2)%values)
              lx3 = size(table(func_n)%x(3)%values)
              allocate (mat_table%x(1)%values(lx1))
              allocate (mat_table%x(2)%values(lx2))
              allocate (mat_table%x(3)%values(lx3))
              allocate (mat_table%y3d(lx1,lx2,lx3))
              mat_table%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
              mat_table%x(2)%values(1:lx2) = x2scale*table(func_n)%x(2)%values(1:lx2)
              mat_table%x(3)%values(1:lx3) = x3scale*table(func_n)%x(3)%values(1:lx3)
              do i=1,lx1
                do j=1,lx2
                  do k=1,lx3
                    mat_table%y3d(i,j,k) =                  &
                      fscale*table(func_n)%y%values((k-1)*lx1*lx2+(j-1)*lx1+i)
                  end do
                end do
              end do

            else if (mat_table%ndim == 4) then
              lx1 = size(table(func_n)%x(1)%values)
              lx2 = size(table(func_n)%x(2)%values)
              lx3 = size(table(func_n)%x(3)%values)
              lx4 = size(table(func_n)%x(4)%values)
              allocate (mat_table%x(1)%values(lx1))
              allocate (mat_table%x(2)%values(lx2))
              allocate (mat_table%x(3)%values(lx3))
              allocate (mat_table%x(4)%values(lx4))
              allocate (mat_table%y4d(lx1,lx2,lx3,lx4))
              mat_table%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
              mat_table%x(2)%values(1:lx2) = x2scale*table(func_n)%x(2)%values(1:lx2)
              mat_table%x(3)%values(1:lx3) = x3scale*table(func_n)%x(3)%values(1:lx3)
              mat_table%x(4)%values(1:lx4) = x4scale*table(func_n)%x(4)%values(1:lx4)
              do i=1,lx1
                do j=1,lx2
                  do k=1,lx3
                    do l=1,lx4
                      mat_table%y4d(i,j,k,l) =                  &
                        fscale*table(func_n)%y%values((l-1)*lx1*lx2*lx3+(k-1)*lx1*lx2+(j-1)*lx1+i)
                    end do
                  end do
                end do
              end do
!
            end if   ! ndim
          end if     ! ifunc > 0
!------------------------------
          return
        end
      end module mat_table_table_copy_mod
