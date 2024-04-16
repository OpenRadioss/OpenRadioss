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
!chd|====================================================================
!chd|  LAW50_TABLE                   source/materials/mat/mat050/law50_table.F
!chd|-- called by -----------
!chd|-- calls ---------------
!chd|====================================================================

      module law50_table_mod
      contains

! ======================================================================================================================
! \brief unifies abscissas of input function and creates equivalent 2D tables
!! \details 

! ======================================================================================================================

      subroutine law50_table(table  ,nfunc  ,len     ,lmax   ,rate   ,xi     ,yi     )

!----------------------------------------------- 
!     M o d u l e s
!-----------------------------------------------
      use table4d_mod
! ----------------------------------------------------------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in)    :: nfunc                     !< number of function in 2D table
      integer ,intent(inout) :: lmax                      !< max size of function abscissa
      integer ,dimension(nfunc),intent(inout)    :: len   !< size table of all functions
      my_real ,dimension(nfunc) ,intent(in)      :: rate  !< second dimension values
      my_real ,dimension(lmax,nfunc) ,intent(in) :: xi    !< x vectors by dimension
      my_real ,dimension(lmax,nfunc) ,intent(in) :: yi    !< y vectors by dimension
      type(table_4d_) ,intent(inout) ::  table            !< table structure
!-----------------------------------------------
!     L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,nptx,idebug
      my_real ,dimension(:)   ,allocatable :: xf
      my_real ,dimension(:,:) ,allocatable :: yf
!=======================================================================
!     create X,Y vectors for all curves and unify all abscissas
!--------------------------------------------------------
      idebug = 0 
      nptx = 0
      do i = 1,nfunc
        nptx = nptx + len(i)
      end do
      allocate (xf(nptx))
      allocate (yf(nptx,nfunc))
!
      if (nfunc == 1) then
        xf(1:nptx)   = xi(1:nptx,1)
        yf(1:nptx,1) = yi(1:nptx,1)
!
      else
        ! unify abscissas
!
        call unify_abscissa_2d(nfunc,len,lmax,nptx ,xi  ,xf  )
!
        do i = 1,nfunc
          call table_values_2d(len(i) ,nptx ,xi(1,i) ,yi(1,i) ,xf ,yf(1,i) )
        end do      
        len(1:nfunc) = nptx
        
      end if      
!--------------------------------------------------------------------------
       ! create 2d function table
!--------------------------------------------------------
      if (nfunc == 1) then
        table%ndim = 1
        allocate (table%x(table%ndim) )            
        allocate (table%x(1)%values(nptx) )      
        allocate (table%y1d(nptx) )
        table%x(1)%values(1:nptx) = xf(1:nptx)
        table%y1d(1:nptx) = yf(1:nptx,1)
      else
        table%ndim = 2
        allocate (table%x(table%ndim) )            
        allocate (table%x(1)%values(nptx) )      
        allocate (table%x(2)%values(nfunc) )
        allocate (table%y2d(nptx,nfunc) )
        table%x(1)%values(1:nptx)  = xf(1:nptx)   
        table%x(2)%values(1:nfunc) = rate(1:nfunc)
        do i = 1,nfunc
          table%y2d(1:nptx,i) = yf(1:nptx,i)
        end do
      end if 
!--------------------
      ! print tables
      if (idebug == 1) then
          print*,' '
        if (table%ndim == 2) then
          do i = 1, size(table%x(2)%values)
            print*,' dimension, epsp', i,table%x(2)%values(i)
            do j = 1,size(table%x(1)%values)
              print*,table%x(1)%values(j),table%y2d(j,i)
            end do
          end do     
        end if
      end if
!--------------------
      deallocate (xf)
      deallocate (yf)
!--------------------
      return
      end subroutine law50_table
      end module law50_table_mod
