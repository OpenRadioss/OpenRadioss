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
! --------------------------------------------------------------------------------------------------------------
!
      !||====================================================================
      !||    func_table_copy_mod   ../starter/source/materials/tools/func_table_copy.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat50         ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat57         ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat87         ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||====================================================================
       module func_table_copy_mod
       contains

!! \brief  creates local 2d table in material parameter structure from input function list
!! \detail mat_param table array : mat_param%ntable > 0 should be already allocated

      !||====================================================================
      !||    func_table_copy        ../starter/source/materials/tools/func_table_copy.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat50          ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat57          ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat87          ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||--- calls      -----------------------------------------------------
      !||    mattab_usr2sys         ../starter/source/materials/tools/mattab_usr2sys.F
      !||    table_values_2d        ../starter/source/materials/tools/table_values_2d.F
      !||    unify_abscissa_2d      ../starter/source/materials/tools/unify_abscissas_2d.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
       subroutine func_table_copy(mat_table,mat_title,mat_id   ,     &
                                  nfunc    ,ifunc    ,x2vect   ,x1scale  ,x2scale  ,fscale   ,               &
                                  ntable   ,table    ,ierr     )
! --------------------------------------------------------------------------------------------------------------
!     M o d u l e s
! --------------------------------------------------------------------------------------------------------------
      use table4d_mod
      use names_and_titles_mod , only : nchartitle
      use constant_mod         , only : zero
! --------------------------------------------------------------------------------------------------------------
      implicit none
!-----------------------------------------------
!     included files
! ----------------------------------------------
#include "my_real.inc"
! --------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! --------------------------------------------------------------------------------------------------------------
      character(len=nchartitle)       ,intent(in)    :: mat_title  !< material law title
      integer                         ,intent(in)    :: mat_id     !< material law Id
      integer                         ,intent(in)    :: ntable     !< number of function tables in input deck
      integer                         ,intent(in)    :: nfunc      !< number of functions to convert
      my_real                         ,intent(in)    :: x1scale    !< scale factor for function abscissa  
      my_real                         ,intent(in)    :: x2scale    !< scale factor for second abscissa dimension
      integer      ,dimension(nfunc)  ,intent(in)    :: ifunc      !< liste of functions Ids
      my_real      ,dimension(nfunc)  ,intent(in)    :: x2vect     !< second variable values for each function  
      my_real      ,dimension(nfunc)  ,intent(in)    :: fscale     !< scale factor for values of each function  
      type(ttable) ,dimension(ntable) ,intent(in)    :: table      !< input table array
      type(table_4d_)                 ,intent(inout) :: mat_table  !< target material table structure
      integer                         ,intent(out)   :: ierr       !< output error flag : no error=0 , error=1
! --------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! --------------------------------------------------------------------------------------------------------------
      integer :: i,j
      integer :: ndim,nptx,npi,lmax,idebug
      integer :: func_n                              
      integer ,dimension(:)   ,allocatable :: len
      my_real ,dimension(:)   ,allocatable :: xf
      my_real ,dimension(:,:) ,allocatable :: yf
      my_real ,dimension(:,:) ,allocatable :: xi
      my_real ,dimension(:,:) ,allocatable :: yi
!=========================================================================================
      idebug = 0
      ierr   = 0
!--------------------------------------------------------
!     check the input function Ids and convert them into internal function numbers
!--------------------------------------------------------
      call mattab_usr2sys(mat_title,mat_id,ntable,table,nfunc,ifunc)
      do i = 1,nfunc
        if (ifunc(i) == 0) then
          ierr = 1
          mat_table%notable = 0
        end if
      end do
!   
!--------------------------------------------------------
      ! exit when there are errors in function identification
      if (ierr == 1) return
!--------------------------------------------------------
      mat_table%notable = nfunc
      if (nfunc == 1) then
        ndim = 1
      else
        ndim = 2
      end if
      mat_table%ndim = ndim
      allocate (mat_table%x(ndim))
!--------------------------------------------------------
      if (ndim == 1) then                       !  just need to copy original function to mat_table
        func_n = ifunc(1)
        npi = size(table(func_n)%x(1)%values)
        allocate (mat_table%x(1)%values(npi) )
        allocate (mat_table%y1d(npi) )
        mat_table%x(1)%values(1:npi) = x1scale   * table(func_n)%x(1)%values(1:npi)
        mat_table%y1d(1:npi)         = fscale(1) * table(func_n)%y%values(1:npi)
! 
      else   ! table 2d : need to unify all abscissas and create full value matrix
! 
        !--------------------------------------------------------
        !     create X,Y vectors for all curves and unify all abscissas
        !--------------------------------------------------------
        allocate (len(nfunc))              
        nptx = 0                         
        lmax = 0                         
        do i = 1,nfunc
          func_n = ifunc(i)
          len(i) = size(table(func_n)%x(1)%values)
          nptx = nptx + len(i)         
          lmax = max(lmax,len(i))
        end do                           
        allocate (xf(nptx))              
        allocate (yf(nptx,nfunc))        
        allocate (xi(lmax,nfunc))        
        allocate (yi(lmax,nfunc))
        xi(:,:) = zero
        yi(:,:) = zero
!
        do i = 1,nfunc
          func_n = ifunc(i)
          npi    = len(i)
          xi(1:npi,i) = x1scale   * table(func_n)%x(1)%values(1:npi)
          yi(1:npi,i) = fscale(i) * table(func_n)%y%values(1:npi)
        end do                           
!
        call unify_abscissa_2d(nfunc,len,lmax,nptx ,xi  ,xf  )

        do i = 1,nfunc
          call table_values_2d(len(i) ,nptx ,xf ,yi(1,i) ,xf ,yf(1,i) )
        end do
        len(1:nfunc) = nptx
! 
        allocate (mat_table%x(1)%values(nptx) )
        allocate (mat_table%x(2)%values(nfunc) )
        allocate (mat_table%y2d(nptx,nfunc) )
        mat_table%x(1)%values(1:nptx)  = xf(1:nptx)
        mat_table%x(2)%values(1:nfunc) = x2scale * x2vect(1:nfunc)
        do i = 1,nfunc
          mat_table%y2d(1:nptx,i) = yf(1:nptx,i)
        end do
        !--------------------
        deallocate (yi)
        deallocate (xi)
        deallocate (yf)
        deallocate (xf)
        deallocate (len)
        !--------------------
!
      end if  ! end of 2d table treatment
!----------------------------------------
      ! print mat_tables
      if (idebug == 1) then
        print*,' '
        if (mat_table%ndim == 1) then
            do j = 1,size(mat_table%x(1)%values)
              print*,mat_table%x(1)%values(j),mat_table%y1d(j)
            end do
        else if (mat_table%ndim == 2) then
          do i = 1, size(mat_table%x(2)%values)
            print*,' dimension, epsp', i,mat_table%x(2)%values(i)
            do j = 1,size(mat_table%x(1)%values)
              print*,mat_table%x(1)%values(j),mat_table%y2d(j,i)
            end do
          end do
        end if
      end if
!------------------------------
      return
      end
      end module func_table_copy_mod
