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
      !||====================================================================
      !||    eos_table_copy_mod           ../starter/source/materials/tools/eos_table_copy.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos_compaction_tab   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||====================================================================
       module eos_table_copy_mod
       contains

!! \brief  see mat_table_copy  (adapted for type eosparam_struct_)

      !||====================================================================
      !||    eos_table_copy               ../starter/source/materials/tools/eos_table_copy.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos_compaction_tab   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||--- calls      -----------------------------------------------------
      !||    mattab_usr2sys               ../starter/source/materials/tools/mattab_usr2sys.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod                    ../starter/share/modules1/table_mod.F
      !||====================================================================
       subroutine eos_table_copy(eos_param ,x2vect   ,x3vect   ,x4vect   , &
                                 x1scale   ,x2scale  ,x3scale  ,x4scale  , &
                                 fscale    ,ntable   ,table    ,ierr     , uid)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use eos_param_mod , only : eos_param_
      use table_mod , only : ttable
      use constant_mod , only : one, ep10
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
      type(eos_param_)              ,intent(inout)       :: eos_param !< material model data structure
      my_real, dimension(eos_param%ntable),intent(in)    :: x2vect    !< x2 vector flag
      my_real, dimension(eos_param%ntable),intent(in)    :: x3vect    !< x3 vector flag
      my_real, dimension(eos_param%ntable),intent(in)    :: x4vect    !< x4 vector flag
      my_real                             ,intent(in)    :: x1scale   !< x1 scale factor
      my_real                             ,intent(in)    :: x2scale   !< x2 scale factor
      my_real                             ,intent(in)    :: x3scale   !< x3 scale factor
      my_real                             ,intent(in)    :: x4scale   !< x4 scale factor
      my_real, dimension(eos_param%ntable),intent(in)    :: fscale    !< function scale factor
      integer                             ,intent(in)    :: ntable    !< number of function tables in input deck
      type(ttable), dimension(ntable)     ,intent(in)    :: table     !< input table array
      integer                             ,intent(out)   :: ierr      !< output error flag : no error=0 , error=1
      integer                             ,intent(in)    :: uid       !< user identifier
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,j,k,l,itab,idebug                     
      integer :: func_id,func_n                              
      integer :: nfunc,ndim                             
      integer :: lx1,lx2,lx3,lx4
      integer, dimension(:) ,allocatable :: ifunc
      integer, dimension(:) ,allocatable :: ierr_f
!=========================================================================================
!     copy global functions/tables to eos_param data structure
!--------------------------------------------------------------------------
      idebug = 0
      nfunc = eos_param%ntable
      allocate (ifunc(nfunc))
      allocate (ierr_f(nfunc))
      ierr = 0
      ierr_f(1:nfunc) = 0
      do itab = 1,nfunc
        ifunc(itab) = eos_param%table(itab)%notable
        call mattab_usr2sys(eos_param%title,uid,ntable,table,1,ifunc(itab))
        if (ifunc(itab) == 0) then
          ierr_f(itab) = 1
          ierr = 1
          eos_param%table(itab)%notable = 0
        end if
      end do
!   
      !if (ierr == 0) then
!
        eos_param%ntable = nfunc
!
        do itab = 1,nfunc
          func_id = eos_param%table(itab)%notable     ! input table Id
          func_n  = ifunc(itab)                       ! internal input table number
          if(ierr_f(itab) == 0)then
            ndim = table(func_n)%ndim                 ! table dimension
           else
            ndim = 1 !creating unity function f(x)=1
           end if
          eos_param%table(itab)%notable = func_id
          eos_param%table(itab)%ndim = ndim
          allocate (eos_param%table(itab)%x(ndim))
!          
          if (ndim == 1) then

            if(ierr_f(itab) == 0)then
              lx1 = size(table(func_n)%x(1)%values)      ! number of abscissa points
              allocate (eos_param%table(itab)%x(1)%values(lx1))
              allocate (eos_param%table(itab)%y1d(lx1))
              eos_param%table(itab)%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
              eos_param%table(itab)%y1d(1:lx1) = fscale(itab)*table(func_n)%y%values(1:lx1)
            else
              ! function unity if user did not provide function identifier f[-1e10:+1e10] = [1:1]
              lx1 = 2
              allocate (eos_param%table(itab)%x(1)%values(lx1))
              allocate (eos_param%table(itab)%y1d(lx1))
              eos_param%table(itab)%x(1)%values(1) = -x1scale*ep10
              eos_param%table(itab)%x(1)%values(2) = +x1scale*ep10
              eos_param%table(itab)%y1d(1:lx1) = fscale(itab)*one
            end if
            
            if (idebug == 1) then
              do i=1,lx1
                print*,eos_param%table(itab)%x(1)%values(i),eos_param%table(itab)%y1d(i)
              end do
            end if
            
          else if (ndim == 2) then
            lx1 = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            allocate (eos_param%table(itab)%x(1)%values(lx1))
            allocate (eos_param%table(itab)%x(2)%values(lx2))
            allocate (eos_param%table(itab)%y2d(lx1,lx2))
            eos_param%table(itab)%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
            eos_param%table(itab)%x(2)%values(1:lx2) = x2scale*x2vect(itab)*table(func_n)%x(2)%values(1:lx2)
            do i=1,lx1
              do j=1,lx2
                 eos_param%table(itab)%y2d(i,j) = fscale(itab)*table(func_n)%y%values((j-1)*lx1+i)
              end do
            end do
            
          else if (ndim == 3) then
            lx1  = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            lx3 = size(table(func_n)%x(3)%values)
            allocate (eos_param%table(itab)%x(1)%values(lx1))
            allocate (eos_param%table(itab)%x(2)%values(lx2))
            allocate (eos_param%table(itab)%x(3)%values(lx3))
            allocate (eos_param%table(itab)%y3d(lx1,lx2,lx3))
            eos_param%table(itab)%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
            eos_param%table(itab)%x(2)%values(1:lx2) = x2scale*x2vect(itab)*table(func_n)%x(2)%values(1:lx2)
            eos_param%table(itab)%x(3)%values(1:lx3) = x3scale*x3vect(itab)*table(func_n)%x(3)%values(1:lx3)
            do i=1,lx1
              do j=1,lx2
                do k=1,lx3
                  eos_param%table(itab)%y3d(i,j,k) =                  &
                  fscale(itab)*table(func_n)%y%values((k-1)*lx1*lx2+(j-1)*lx1+i)
                end do
              end do
            end do
            
          else if (ndim == 4) then
            lx1 = size(table(func_n)%x(1)%values)
            lx2 = size(table(func_n)%x(2)%values)
            lx3 = size(table(func_n)%x(3)%values)
            lx4 = size(table(func_n)%x(4)%values)
            allocate (eos_param%table(itab)%x(1)%values(lx1))
            allocate (eos_param%table(itab)%x(2)%values(lx2))
            allocate (eos_param%table(itab)%x(3)%values(lx3))
            allocate (eos_param%table(itab)%x(4)%values(lx4))
            allocate (eos_param%table(itab)%y4d(lx1,lx2,lx3,lx4))
            eos_param%table(itab)%x(1)%values(1:lx1) = x1scale*table(func_n)%x(1)%values(1:lx1)
            eos_param%table(itab)%x(2)%values(1:lx2) = x2scale*x2vect(itab)*table(func_n)%x(2)%values(1:lx2)
            eos_param%table(itab)%x(3)%values(1:lx3) = x3scale*x3vect(itab)*table(func_n)%x(3)%values(1:lx3)
            eos_param%table(itab)%x(4)%values(1:lx4) = x4scale*x4vect(itab)*table(func_n)%x(4)%values(1:lx4)
            do i=1,lx1
              do j=1,lx2
                do k=1,lx3
                  do l=1,lx4
                    eos_param%table(itab)%y4d(i,j,k,l) =                  &
                    fscale(itab)*table(func_n)%y%values((l-1)*lx1*lx2*lx3+(k-1)*lx1*lx2+(j-1)*lx1+i)
                  end do
                end do
              end do
            end do
!
          end if   ! ndim      
        end do     ! nfunc
      !end if       ! ierr
!
      deallocate (ifunc)
!------------------------------
      return
      end subroutine eos_table_copy
      end module eos_table_copy_mod
