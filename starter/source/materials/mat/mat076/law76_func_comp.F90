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
!||    law76_func_comp_mod   ../starter/source/materials/mat/mat076/law76_func_comp.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76         ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||====================================================================
       module law76_func_comp_mod
         implicit none
         private
         public :: law76_func_comp

       contains
        !----------------------------------------------------------------------
        ! \brief  Creates tabulated hardening curve in compression using tension and shear functions
        !----------------------------------------------------------------------

!||====================================================================
!||    law76_func_comp   ../starter/source/materials/mat/mat076/law76_func_comp.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat76     ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine law76_func_comp(func_tens,func_shear,func_comp,nptmax,nup)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use table4d_mod
          use constant_mod  ,only : zero,half,one,two,three,em20
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global Arguments
! --------------------------------------------------------------------------------------------------
          integer ,intent(in) :: nptmax
          real(kind=WP) ,intent(in) :: nup
          type(table_4d_) ,intent(in)    :: func_tens
          type(table_4d_) ,intent(in)    :: func_shear
          type(table_4d_) ,intent(inout) :: func_comp
!-----------------------------------------------
!         L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: i,j,k       
        integer :: ndim,npt_tens,npt_shear,npt_comp
        real(kind=wp) :: scale_x_s
        real(kind=wp) :: xi, xj , num, den ,alphamax ,betamin
        real(kind=wp) ,dimension(:) ,allocatable :: ytens,yshear
        real(kind=wp) ,dimension(nptmax) :: x_t_shear
        real(kind=wp) ,dimension(nptmax) :: x_comp,y_comp
        real(kind=wp) ,dimension(nptmax) :: slope_tens,slope_shea,y_t, y_s,alpha,beta
!===================================================================================================
        npt_tens  = size(func_tens%x(1)%values)
        npt_shear = size(func_shear%x(1)%values)      
        allocate(ytens(npt_tens))
        allocate(yshear(npt_shear))
        if (func_tens%ndim == 1) then
          ytens(:) = func_tens%y1d(1:npt_tens)
        else
          ytens(:) = func_tens%y2d(1:npt_tens,1)
        end if
        if (func_shear%ndim == 1) then
          yshear(:) = func_shear%y1d(1:npt_shear)
        else
          yshear(:) = func_shear%y2d(1:npt_shear,1)
        end if
        scale_x_s = sqrt(three)/(one+nup)
        do i = 1,npt_shear
           x_t_shear(i) = scale_x_s* func_shear%x(1)%values(i) 
        enddo
!---------------------------------------------------------------------------------------------------
        i = 1
        j = 1 
        k = 1
        do while (i <= npt_tens .or. j <= npt_shear)  
          if (i <= npt_tens .and. j <= npt_shear)then
            xi = func_tens%x(1)%values(i) 
            xj = x_t_shear(j) 
            if (xi < xj) then    
              x_comp(k) = xi
              y_t(k)    = ytens(i)
              if (j ==1) then
                y_s(k)    = yshear(1) +                                                    &
                          (xi - x_t_shear(1) )*                                            &
                          (yshear(2)   - yshear(1))/                                       &
                          (x_t_shear(2)- x_t_shear(1))                                      
             else                                                                           
               y_s(k)    = yshear(j-1) +                                                   &
                          (xi - x_t_shear(j-1) )*                                          &
                          (yshear(j)   - yshear(j-1))/                                     &
                          (x_t_shear(j)- x_t_shear(j-1)) 
              endif
              i = i + 1
              k = k + 1     
            elseif (xj < xi) then                            
              x_comp(k) = xj
              y_s(k)    = yshear(j)
              if (i ==1) then
                y_t(k) = ytens(1)                                                           &
                      + (xj - func_tens%x(1)%values(1)) * (ytens(2) - ytens(1))             &
                      / (func_tens%x(1)%values(2)- func_tens%x(1)%values(1))                 
             else                                                                            
               y_t(k) = ytens(i-1)                                                          &
                      + (xj - func_tens%x(1)%values(i-1)) * (ytens(i) - ytens(i-1))         &
                      / (func_tens%x(1)%values(i)- func_tens%x(1)%values(i-1)) 
              endif
              j = j + 1
              k = k + 1                             
            elseif (xi == xj) then
              x_comp(k) = xi
              y_t(k)    = ytens(i)
              y_s(k)    = yshear(j)
              i = i + 1
              j = j + 1
              k = k + 1                             
            endif   
          elseif (i > npt_tens .and. j <= npt_shear) then
               xj=x_t_shear(j) 
               x_comp(k) = xj
               y_s(k)    = yshear(j)
               y_t(k)    = ytens(i-2)                                                       &
                        + (xj - func_tens%x(1)%values(i-2)) * (ytens(i-1) - ytens(i-2))     &
                        / (func_tens%x(1)%values(i-1)- func_tens%x(1)%values(i-2))           
              j = j + 1                                                                      
              k = k + 1                                                                      
         elseif (i <= npt_tens .and. j > npt_shear) then                                     
           xi=func_tens%x(1)%values(i)                                                       
           x_comp(k) = xi                                                                    
           y_t(k)    = ytens(i)                                                      
           y_s(k)    = yshear(j-2)                                                          &
                     + (xi - x_t_shear(j-2)) * (yshear(j-1) - yshear(j-2))                  &
                     / (x_t_shear(j-1)- x_t_shear(j-2)) 
            i = i + 1
            k = k + 1     
          else
            exit     
          endif
        end do   
!---------------------------------------------------------------------------------------------------
        npt_comp = k - 1

        alphamax = one
        do k= 2,npt_comp
           slope_tens(k) = (y_t(k)-y_t(k-1)) / (x_comp(k)-x_comp(k-1))
           slope_shea(k) = (y_s(k)-y_s(k-1)) / (x_comp(k)-x_comp(k-1)) 
           if( slope_tens(k)>zero .and. slope_shea(k)>zero)then
             alpha(k) = sqrt(three)*half *(slope_tens(k)/slope_shea(k)) * (y_s(k)/y_t(k))**2
             alphamax = max (alphamax,alpha(k))
           endif
        end do 
        do k= 1,npt_comp
           num = sqrt(three) *alphamax *y_t(k)*y_s(k)
           den = two * alphamax * y_t(k) - sqrt(three) * y_s(k)
           y_comp(k) = num / max(em20, den)
        end do 
!---------------------------------------------------------------------------------------------------
        allocate (func_comp%x(1))      
        allocate (func_comp%x(1)%values(npt_comp))
        allocate (func_comp%y1d(npt_comp))
        func_comp%ndim=1
        func_comp%notable=1
        func_comp%x(1)%values(1:npt_comp) = x_comp(1:npt_comp)
        func_comp%y1d(1:npt_comp)         = y_comp(1:npt_comp)
!---------------------------------------------------------------------------------------------------
        deallocate(yshear)
        deallocate(ytens)
!===================================================================================================
        end subroutine law76_func_comp
        end module law76_func_comp_mod
