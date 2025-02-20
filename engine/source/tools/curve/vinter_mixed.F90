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
      !||    redef3             ../engine/source/elements/spring/redef3.F90
      !||    redef3_law113      ../engine/source/elements/spring/redef3_law113.F
      !||====================================================================
      module vinter_mixed_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief  interpolate a table of values, or evaluate a python function
      !||====================================================================
      !||    vinter_mixed           ../engine/source/tools/curve/vinter_mixed.F90
      !||--- called by ------------------------------------------------------
      !||    redef3                 ../engine/source/elements/spring/redef3.F90
      !||    redef3_law113          ../engine/source/elements/spring/redef3_law113.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    python_funct_mod       ../common_source/modules/python_mod.F90
      !||====================================================================
        subroutine vinter_mixed(python, tf,iad,ipos ,ilen,nel0,x,dydx,y)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use python_funct_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_) :: python
          integer, intent(in) :: nel0
          integer :: ilen(nel0) !
          integer :: ipos(nel0) !< if > 0: index of the first point of the interval, if < 0: index of the python function
          integer :: iad(nel0)
          my_real :: x(nel0)
          my_real :: y(nel0)
          my_real :: tf(2,*)
          my_real :: dydx(nel0)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision :: ydp
          double precision :: tf2j2
          double precision :: tf2j1
          double precision :: tf1j2
          double precision :: tf1j1
          double precision :: xdp
          double precision :: yydp
          double precision :: dydxdp
          logical :: cond1
          logical :: cond2
          integer :: i
          integer :: j1
          integer :: j
          integer :: icont
          integer :: j2

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          j = 0
          icont = 1
          do while(icont==1)
            j = j+1
            icont = 0
            do i=1,nel0
              if(ilen(i) < 0) cycle
              j1 = ipos(i)+iad(i)+1
              cond1 = j<=ilen(i)-1
              if(cond1) cond1 = x(i)>tf(1,j1)
              cond2 = ipos(i)>=1
              if(cond2) cond2 = x(i)<tf(1,j1-1)
              if(cond1)then
                ipos(i)=ipos(i)+1
                icont = 1
              elseif(cond2)then
                ipos(i)=ipos(i)-1
                icont = 1
              endif
            enddo
          enddo
          do i=1,nel0
            if(ilen(i) < 0) cycle
            j1   =ipos(i)+iad(i)
            j2   = j1+1
            tf2j2 = tf(2,j2)
            tf2j1 = tf(2,j1)
            tf1j2 = tf(1,j2)
            tf1j1 = tf(1,j1)
            xdp = x(i)
            dydxdp = (tf2j2-tf2j1)/(tf1j2-tf1j1)
            dydx(i) = dydxdp ! possible cast here in sp
            yydp = tf2j1 + dydxdp * (xdp - tf1j1)
            y(i) = yydp
          enddo

          do i=1,nel0
            if(ilen(i) < 0) then 
              call python_call_funct1d(python, -ilen(i),x(i), y(i)) 
              call python_deriv_funct1D(python, -ilen(i),x(i), dydx(i)) 
            endif
          enddo
          return
        end subroutine vinter_mixed
      end module vinter_mixed_mod
