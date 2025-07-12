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
      !||    vinter_mixed_mod   ../engine/source/tools/curve/vinter_mixed.F90
      !||--- called by ------------------------------------------------------
      !||====================================================================
      module table_mat_vinterp_c1_mod
      contains
! ====================================================================================================
!                                                   procedures
! ====================================================================================================
!! \brief  interpolate a table of values with smooth first derivative
!! \details This subroutine is proceeding to table interpolation.
!! \details    example with case dim=1 (table <=> function)
!! \details     nel is number interpolatation
!! \details     ipos is index backup to prevent from starting the loop from 1 to npt during each cycle
!! \details     XX(nel) are abscissa on which the interpolation is required (input)
!! \details     YY(nel) are the interpolated value (output)
!! \details     DYDX(nel) is the slope (output)
      !||====================================================================
      !||    table_mat_vinterp_c1         ../engine/source/materials/tools/table_mat_vinterp_c1.F90
      !||--- called by ------------------------------------------------------
      !||====================================================================

      subroutine table_mat_vinterp_c1(table,dimx,nel,ipos,xx,yy,dydx, opt_extrapolate)

!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use table4d_mod
      use message_mod
      use constant_mod  , only : zero,half,one
      use precision_mod , only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      type(table_4d_)                    ,intent(in)    :: table
      integer, value                     ,intent(in)    :: dimx
      integer                            ,intent(in)    :: nel
      real(kind=WP), dimension(dimx,table%ndim),intent(in)    :: xx
      integer, dimension(dimx,table%ndim),intent(inout) :: ipos
      real(kind=WP), dimension(dimx)           ,intent(inout) :: yy
      real(kind=WP), dimension(dimx)           ,intent(inout) :: dydx
      logical, optional, intent(in)                     :: opt_extrapolate
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      logical :: need_to_compute
      integer  i,j,k,m,n,i1,i2,i3,i4,j1,j2,k1,k2,l1,l2,ndim
      integer :: nindx_1,nindx_2
      integer, dimension(nel) :: indx_1,indx_2
      integer, dimension(4)   :: ldim
      real(kind=WP) :: dx,dy,alpha,alphai,beta,betai,gamma,gammai,delta,deltai
      real(kind=WP), dimension(nel,4) :: fac
      logical do_extrapolation
!
      integer, dimension(nel)         :: idx
      real(kind=WP) :: x1,x2,x3,x4,y1,y2,y3,y4
      real(kind=WP), dimension(2,2,2) :: h1,h2,dydx_smooth
      real(kind=WP), dimension(nel) :: dxx,xs1,xs2,dyy
!-----------------------------------------------
!   source lines
!===================================================================================================
      do_extrapolation = .true.
      if(present(opt_extrapolate)) then
        do_extrapolation = opt_extrapolate
      endif

      ndim = table%ndim
      if (size(xx,2) < ndim ) then
        call ancmsg(msgid=36,anmode=aninfo,c1='table interpolation')
        call arret(2)
      end if

      do k=1,ndim
        ldim(k) = size(table%x(k)%values)
      end do

      do k=1,ndim
        ipos(1:nel,k) = max(ipos(1:nel,k),1)
        nindx_1 = 0
        nindx_2 = 0
#include "vectorize.inc"
        do i=1,nel
          m  = ipos(i,k) 
          dx = table%x(k)%values(m) - xx(i,k)
          if (dx >= zero)then
            nindx_1 = nindx_1 + 1
            indx_1(nindx_1) = i
          else
            nindx_2 = nindx_2 + 1
            indx_2(nindx_2) = i
          endif
        enddo

        do j=1,nindx_1
          i = indx_1(j)
          m = ipos(i,k)
          need_to_compute = .true.
          do while (need_to_compute )
             dx = table%x(k)%values(m) - xx(i,k)
             if (dx < zero .or. m <= 1 ) then
                ipos(i,k) = max(m,1)
                need_to_compute = .false.
             else
                m=m-1
             endif
          enddo
        enddo

        do j=1,nindx_2
          i = indx_2(j)
          m = ipos(i,k) 
          need_to_compute = .true.
          do while (need_to_compute )
             dx = table%x(k)%values(m) - xx(i,k)
             if (dx >= zero .or. m == ldim(k)) then
                ipos(i,k) = m-1
                need_to_compute = .false.
             else
                m=m+1
             endif
          enddo
        enddo
      enddo ! k=1,ndim

      do k=1,ndim
#include "vectorize.inc"
        do i=1,nel
          n = ipos(i,k)
          fac(i,k) = (table%x(k)%values(n+1) - xx(i,k)) / (table%x(k)%values(n+1) - table%x(k)%values(n))
        end do
      end do

      if(.not. do_extrapolation)then
      do k=1,ndim
#include "vectorize.inc"
        do i=1,nel
          n = ipos(i,k)
          fac(i,k) = min(one,max(fac(i,k),zero))
        end do
      end do
      endif
!----------------------------------------------

      select case(ndim)

       case(4)
#include "vectorize.inc"
        do i=1,nel                                                                         
          i1 = ipos(i,1)                                                                   
          i2 = i1 + 1                                                                      
          j1 = ipos(i,2)                                                                   
          j2 = j1 + 1                                                                      
          k1 = ipos(i,3)                                                                   
          k2 = k1 + 1                                                                      
          l1 = ipos(i,4)                                                                   
          l2 = k1 + 1
          alpha  = fac(i,1)
          beta   = fac(i,2)
          gamma  = fac(i,3)
          delta  = fac(i,4)
          alphai = one - alpha                                                                 
          betai  = one - beta                                                                 
          gammai = one - gamma                                                                 
          deltai = one - delta
          yy(i)  =                                                                          &
             delta* (gamma*(beta * (alpha  * table%y4d(i1,j1,k1,l1)                         &
                                 +  alphai * table%y4d(i2,j1,k1,l1))                        &
                          + betai* (alpha  * table%y4d(i1,j2,k1,l1)                         &
                                 +  alphai * table%y4d(i2,j2,k1,l1)) )                      & 
                  + gammai*( beta* (alpha  * table%y4d(i1,j1,k2,l1)                         &
                                 +  alphai * table%y4d(i2,j1,k2,l1))                        &
                          + betai* (alpha  * table%y4d(i1,j2,k2,l1)                         &
                                 +  alphai * table%y4d(i2,j2,k2,l1))))                      &
           + deltai*(gamma*( beta* (alpha  * table%y4d(i1,j1,k1,l2)                         &
                                   +alphai * table%y4d(i2,j1,k1,l2))                        &
                          + betai* (alpha  * table%y4d(i1,j2,k1,l2)                         &
                                 +  alphai * table%y4d(i2,j2,k1,l2)))                       &
                  + gammai*(beta * (alpha  * table%y4d(i1,j1,k2,l2)                         &
                                 +  alphai * table%y4d(i2,j1,k2,l2))                        &
                          + betai* (alpha  * table%y4d(i1,j2,k2,l2)                         &
                                 +  alphai * table%y4d(i2,j2,k2,l2))))                       
        end do                                                                             
!
        ! smooth derivative in 4d
        do i=1,nel
          i1 = ipos(i,1)-1
          i2 = i1 + 1
          i3 = i2 + 1
          i4 = i3 + 1
          j1 = ipos(i,2)
          j2 = j1 + 1
          k1 = ipos(i,3)
          k2 = k1 + 1
          l1 = ipos(i,4)                                                                   
          l2 = k1 + 1
          xs2(i)    = (table%x(1)%values(i3) + table%x(1)%values(i2)) * half
          dx        = table%x(1)%values(i3)  - table%x(1)%values(i2)                                                                     
          h2(1,1,1) = (table%y4d(i3,j1,k1,l1) - table%y4d(i2,j1,k1,l1)) / dx
          h2(2,1,1) = (table%y4d(i3,j2,k1,l1) - table%y4d(i2,j2,k1,l1)) / dx
          h2(1,2,1) = (table%y4d(i3,j1,k2,l1) - table%y4d(i2,j1,k2,l1)) / dx 
          h2(2,2,1) = (table%y4d(i3,j2,k2,l1) - table%y4d(i2,j2,k2,l1)) / dx
          h2(1,1,2) = (table%y4d(i3,j1,k1,l2) - table%y4d(i2,j1,k1,l2)) / dx
          h2(2,1,2) = (table%y4d(i3,j2,k1,l2) - table%y4d(i2,j2,k1,l2)) / dx
          h2(1,2,2) = (table%y4d(i3,j1,k2,l2) - table%y4d(i2,j1,k2,l2)) / dx 
          h2(2,2,2) = (table%y4d(i3,j2,k2,l2) - table%y4d(i2,j2,k2,l2)) / dx
          dxx(i)  = xs2(i) - xx(i,1)

          alpha     = zero
          beta      = fac(i,2)
          gamma     = fac(i,2)
          delta     = fac(i,4)
          h1(:,:,:) = zero
!
          if (ipos(i,1) == 1) then                  ! first point
            if (dxx(i) <= zero) then
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)                                                                    
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y4d(i4,j1,k1,l1) - table%y4d(i3,j1,k1,l1)) / dx
              h1(2,1,1) = (table%y4d(i4,j2,k1,l1) - table%y4d(i3,j2,k1,l1)) / dx
              h1(1,2,1) = (table%y4d(i4,j1,k2,l1) - table%y4d(i3,j1,k2,l1)) / dx
              h1(2,2,1) = (table%y4d(i4,j2,k2,l1) - table%y4d(i3,j2,k2,l1)) / dx
              h1(1,1,2) = (table%y4d(i4,j1,k1,l2) - table%y4d(i3,j1,k1,l2)) / dx
              h1(2,1,2) = (table%y4d(i4,j2,k1,l2) - table%y4d(i3,j2,k1,l2)) / dx
              h1(1,2,2) = (table%y4d(i4,j1,k2,l2) - table%y4d(i3,j1,k2,l2)) / dx
              h1(2,2,2) = (table%y4d(i4,j2,k2,l2) - table%y4d(i3,j2,k2,l2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else if (ipos(i,1) == ldim(1) - 1) then    ! last point     
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)                                                                    
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y4d(i2,j1,k1,l1) - table%y4d(i1,j1,k1,l1)) / dx
              h1(2,1,1) = (table%y4d(i2,j2,k1,l1) - table%y4d(i1,j2,k1,l1)) / dx
              h1(1,2,1) = (table%y4d(i2,j1,k2,l1) - table%y4d(i1,j1,k2,l1)) / dx
              h1(2,2,1) = (table%y4d(i2,j2,k2,l1) - table%y4d(i1,j2,k2,l1)) / dx
              h1(1,1,2) = (table%y4d(i2,j1,k1,l2) - table%y4d(i1,j1,k1,l2)) / dx
              h1(2,1,2) = (table%y4d(i2,j2,k1,l2) - table%y4d(i1,j2,k1,l2)) / dx
              h1(1,2,2) = (table%y4d(i2,j1,k2,l2) - table%y4d(i1,j1,k2,l2)) / dx
              h1(2,2,2) = (table%y4d(i2,j2,k2,l2) - table%y4d(i1,j2,k2,l2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)                                                          
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y4d(i2,j1,k1,l1) - table%y4d(i1,j1,k1,l1)) / dx
              h1(2,1,1) = (table%y4d(i2,j2,k1,l1) - table%y4d(i1,j2,k1,l1)) / dx
              h1(1,2,1) = (table%y4d(i2,j1,k2,l1) - table%y4d(i1,j1,k2,l1)) / dx
              h1(2,2,1) = (table%y4d(i2,j2,k2,l1) - table%y4d(i1,j2,k2,l1)) / dx
              h1(1,1,2) = (table%y4d(i2,j1,k1,l2) - table%y4d(i1,j1,k1,l2)) / dx
              h1(2,1,2) = (table%y4d(i2,j2,k1,l2) - table%y4d(i1,j2,k1,l2)) / dx
              h1(1,2,2) = (table%y4d(i2,j1,k2,l2) - table%y4d(i1,j1,k2,l2)) / dx
              h1(2,2,2) = (table%y4d(i2,j2,k2,l2) - table%y4d(i1,j2,k2,l2)) / dx
            else
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)                                                                
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y4d(i4,j1,k1,l1) - table%y4d(i3,j1,k1,l1)) / dx
              h1(2,1,1) = (table%y4d(i4,j2,k1,l1) - table%y4d(i3,j2,k1,l1)) / dx
              h1(1,2,1) = (table%y4d(i4,j1,k2,l1) - table%y4d(i3,j1,k2,l1)) / dx
              h1(2,2,1) = (table%y4d(i4,j2,k2,l1) - table%y4d(i3,j2,k2,l1)) / dx
              h1(1,1,2) = (table%y4d(i4,j1,k1,l2) - table%y4d(i3,j1,k1,l2)) / dx
              h1(2,1,2) = (table%y4d(i4,j2,k1,l2) - table%y4d(i3,j2,k1,l2)) / dx
              h1(1,2,2) = (table%y4d(i4,j1,k2,l2) - table%y4d(i3,j1,k2,l2)) / dx
              h1(2,2,2) = (table%y4d(i4,j2,k2,l2) - table%y4d(i3,j2,k2,l2)) / dx
            end if
            alpha   = dxx(i) / (xs2(i) - xs1(i))
          end if
          dydx_smooth(1,1,1) = alpha * h1(1,1,1) + (one-alpha) * h2(1,1,1)
          dydx_smooth(2,1,1) = alpha * h1(2,1,1) + (one-alpha) * h2(2,1,1)
          dydx_smooth(1,2,1) = alpha * h1(1,2,1) + (one-alpha) * h2(1,2,1)
          dydx_smooth(2,2,1) = alpha * h1(2,2,1) + (one-alpha) * h2(2,2,1)
          dydx_smooth(1,1,2) = alpha * h1(1,1,2) + (one-alpha) * h2(1,1,2)
          dydx_smooth(2,1,2) = alpha * h1(2,1,2) + (one-alpha) * h2(2,1,2)
          dydx_smooth(1,2,2) = alpha * h1(1,2,2) + (one-alpha) * h2(1,2,2)
          dydx_smooth(2,2,2) = alpha * h1(2,2,2) + (one-alpha) * h2(2,2,2)

          dydx(i) = delta *(                                                                        &
                          gamma      * (beta*dydx_smooth(1,1,1) + (one-beta)*dydx_smooth(2,1,1))    &
                        + (one-gamma)* (beta*dydx_smooth(1,2,1) + (one-beta)*dydx_smooth(2,2,1)))   &
                  + (one-delta) * (                                                                 &
                          gamma      * (beta*dydx_smooth(1,1,2) + (one-beta)*dydx_smooth(2,1,2))    &
                        + (one-gamma)* (beta*dydx_smooth(1,2,2) + (one-beta)*dydx_smooth(2,2,2)))  
        end do

!-----
      case(3)
#include "vectorize.inc"
        do i=1,nel
          i1 = ipos(i,1)
          i2 = i1 + 1
          j1 = ipos(i,2)
          j2 = j1 + 1
          k1 = ipos(i,3)
          k2 = k1 + 1
          alpha  = fac(i,1)
          beta   = fac(i,2)
          gamma  = fac(i,3)
          alphai = one - alpha                                                                 
          betai  = one - beta                                                                 
          gammai = one - gamma
          yy(i)=(gamma * (beta* (alpha*table%y3d(i1,j1,k1) + alphai*table%y3d(i2,j1,k1))        &
                       + betai* (alpha*table%y3d(i1,j2,k1) + alphai*table%y3d(i2,j2,k1)) )      &
            + gammai *  (beta* (alpha*table%y3d(i1,j1,k2)  + alphai*table%y3d(i2,j1,k2))        &
                       + betai* (alpha*table%y3d(i1,j2,k2) + alphai*table%y3d(i2,j2,k2))))                                                                                                 
        end do
!
        ! smooth derivative in 3d
        do i=1,nel
          i1 = ipos(i,1)-1
          i2 = i1 + 1
          i3 = i2 + 1
          i4 = i3 + 1
          j1 = ipos(i,2)
          j2 = j1 + 1
          k1 = ipos(i,3)
          k2 = k1 + 1
          dx = table%x(1)%values(i3) - table%x(1)%values(i2)                                                                     
          xs2(i)  = (table%x(1)%values(i3) + table%x(1)%values(i2)) * half
          h2(1,1,1) = (table%y3d(i3,j1,k1) - table%y3d(i2,j1,k1)) / dx
          h2(2,1,1) = (table%y3d(i3,j2,k1) - table%y3d(i2,j2,k1)) / dx
          h2(1,2,1) = (table%y3d(i3,j1,k2) - table%y3d(i2,j1,k2)) / dx 
          h2(2,2,1) = (table%y3d(i3,j2,k2) - table%y3d(i2,j2,k2)) / dx
          dxx(i)  = xs2(i) - xx(i,1)

          alpha     = zero
          beta      = fac(i,2)
          gamma     = fac(i,2)
          h1(:,:,1) = zero
          if (ipos(i,1) == 1) then                  ! first point
            if (dxx(i) <= zero) then
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)                                                                    
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y3d(i4,j1,k1) - table%y3d(i3,j1,k1)) / dx
              h1(2,1,1) = (table%y3d(i4,j2,k1) - table%y3d(i3,j2,k1)) / dx
              h1(1,2,1) = (table%y3d(i4,j1,k2) - table%y3d(i3,j1,k2)) / dx
              h1(2,2,1) = (table%y3d(i4,j2,k2) - table%y3d(i3,j2,k2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else if (ipos(i,1) == ldim(1) - 1) then    ! last point     
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)                                                                    
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y3d(i2,j1,k1) - table%y3d(i1,j1,k1)) / dx
              h1(2,1,1) = (table%y3d(i2,j2,k1) - table%y3d(i1,j2,k1)) / dx
              h1(1,2,1) = (table%y3d(i2,j1,k2) - table%y3d(i1,j1,k2)) / dx
              h1(2,2,1) = (table%y3d(i2,j2,k2) - table%y3d(i1,j2,k2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)                                                          
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y3d(i2,j1,k1) - table%y3d(i1,j1,k1)) / dx
              h1(2,1,1) = (table%y3d(i2,j2,k1) - table%y3d(i1,j2,k1)) / dx
              h1(1,2,1) = (table%y3d(i2,j1,k2) - table%y3d(i1,j1,k2)) / dx
              h1(2,2,1) = (table%y3d(i2,j2,k2) - table%y3d(i1,j2,k2)) / dx
            else
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)                                                                
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y3d(i4,j1,k1) - table%y3d(i3,j1,k1)) / dx
              h1(2,1,1) = (table%y3d(i4,j2,k1) - table%y3d(i3,j2,k1)) / dx
              h1(1,2,1) = (table%y3d(i4,j1,k2) - table%y3d(i3,j1,k2)) / dx
              h1(2,2,1) = (table%y3d(i4,j2,k2) - table%y3d(i3,j2,k2)) / dx
            end if
            alpha   = dxx(i) / (xs2(i) - xs1(i))
          end if
          dydx_smooth(1,1,1) = alpha * h1(1,1,1) + (one-alpha) * h2(1,1,1)
          dydx_smooth(2,1,1) = alpha * h1(2,1,1) + (one-alpha) * h2(2,1,1)
          dydx_smooth(1,2,1) = alpha * h1(1,2,1) + (one-alpha) * h2(1,2,1)
          dydx_smooth(2,2,1) = alpha * h1(2,2,1) + (one-alpha) * h2(2,2,1)

          dydx(i) = gamma      * (beta*dydx_smooth(1,1,1) + (one-beta)*dydx_smooth(2,1,1)) &
                  + (one-gamma)* (beta*dydx_smooth(1,2,1) + (one-beta)*dydx_smooth(2,2,1))
        end do

      case(2)
#include "vectorize.inc"
        do i=1,nel
          i1 = ipos(i,1)
          i2 = i1 + 1
          j1 = ipos(i,2)
          j2 = j1 + 1
          alpha  = fac(i,1)
          beta   = fac(i,2)
          alphai = one - alpha                                                                 
          betai  = one - beta
          yy(i)  = (beta * (alpha*table%y2d(i1,j1) + alphai*table%y2d(i2,j1))               &
                 + betai * (alpha*table%y2d(i1,j2) + alphai*table%y2d(i2,j2)) )               
        end do
!
        ! smooth derivative in 2d
        do i=1,nel
          i1 = ipos(i,1)-1
          i2 = i1 + 1
          i3 = i2 + 1
          i4 = i3 + 1
          j1 = ipos(i,2)
          j2 = j1 + 1
          dx = table%x(1)%values(i3) - table%x(1)%values(i2)
          xs2(i)  = (table%x(1)%values(i3) + table%x(1)%values(i2)) * half
          h2(1,1,1) = (table%y2d(i3,j1) - table%y2d(i2,j1)) / dx
          h2(2,1,1) = (table%y2d(i3,j2) - table%y2d(i2,j2)) / dx
          dxx(i)  = xs2(i) - xx(i,1)

          alpha     = zero
          beta      = fac(i,2)
          h1(:,1,1) = zero
          if (ipos(i,1) == 1) then                  ! first point
            if (dxx(i) <= zero) then
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y2d(i4,j1) - table%y2d(i3,j1)) / dx
              h1(2,1,1) = (table%y2d(i4,j2) - table%y2d(i3,j2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else if (ipos(i,1) == ldim(1) - 1) then    ! last point     
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y2d(i2,j1) - table%y2d(i1,j1)) / dx
              h1(2,1,1) = (table%y2d(i2,j2) - table%y2d(i1,j2)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y2d(i2,1) - table%y2d(i1,1)) / dx
              h1(2,1,1) = (table%y2d(i2,2) - table%y2d(i1,2)) / dx
            else
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y2d(i4,j1) - table%y2d(i3,j1)) / dx
              h1(2,1,1) = (table%y2d(i4,j2) - table%y2d(i3,j2)) / dx
            end if
            alpha   = dxx(i) / (xs2(i) - xs1(i))
          end if
          dydx_smooth(1,1,1) = alpha * h1(1,1,1) + (one-alpha) * h2(1,1,1)
          dydx_smooth(2,1,1) = alpha * h1(2,1,1) + (one-alpha) * h2(2,1,1)
          dydx(i) = beta*dydx_smooth(1,1,1) + (one-beta)*dydx_smooth(2,1,1)
        end do

      case(1)
!#include "vectorize.inc"
        do i=1,nel
          i1 = ipos(i,1)
          i2 = i1 + 1
          alpha   = fac(i,1)
          yy(i)   = alpha*table%y1d(i1) + (one-alpha)*table%y1d(i2)
        end do
!
        ! smooth derivative in 1d
        do i=1,nel
          i1 = ipos(i,1)-1
          i2 = i1 + 1
          i3 = i2 + 1
          i4 = i3 + 1
          dx = table%x(1)%values(i3) - table%x(1)%values(i2)
          alpha   = zero
          h1(1,1,1) = zero
          h2(1,1,1) = (table%y1d(i3) - table%y1d(i2)) / dx
          xs2(i)  = (table%x(1)%values(i3) + table%x(1)%values(i2)) * half
          dxx(i)  = xs2(i) - xx(i,1)

          if (ipos(i,1) == 1) then                  ! first point
            if (dxx(i) <= zero) then
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y1d(i4) - table%y1d(i3)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else if (ipos(i,1) == ldim(1) - 1) then    ! last point     
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y1d(i2) - table%y1d(i1)) / dx
              alpha   = dxx(i) / (xs2(i) - xs1(i))
            end if
          else
            if (dxx(i) > zero) then
              dx = table%x(1)%values(i2) - table%x(1)%values(i1)
              xs1(i)  = (table%x(1)%values(i2) + table%x(1)%values(i1)) * half
              h1(1,1,1) = (table%y1d(i2) - table%y1d(i1)) / dx
            else
              dx = table%x(1)%values(i4) - table%x(1)%values(i3)
              xs1(i)  = (table%x(1)%values(i4) + table%x(1)%values(i3)) * half
              h1(1,1,1) = (table%y1d(i4) - table%y1d(i3)) / dx
            end if
            alpha   = dxx(i) / (xs2(i) - xs1(i))
          end if
          dydx_smooth(1,1,1) = alpha * h1(1,1,1) + (one-alpha) * h2(1,1,1)
          dydx(i) = dydx_smooth(1,1,1)
        end do

      end select
!-----------
      return
      end subroutine table_mat_vinterp_c1
!-----------
      end module table_mat_vinterp_c1_mod     
