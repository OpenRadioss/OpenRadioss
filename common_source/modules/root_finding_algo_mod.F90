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

        module root_finding_algo_mod
        contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief root finding algo based on Brent's algo
!! \details 
          function brent_algo( a,b,tolerance,funct,funct_parameter_size,funct_parameter)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,half,one,two,three
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
          my_real, intent(in) :: a !< lower bound of the interval 
          my_real, intent(in) :: b !< upper bound of the interval 
          my_real, intent(in) :: tolerance !< tolerance
          my_real, external :: funct !< function
          integer, intent(in) :: funct_parameter_size !< size of funct_parameter array
          my_real, dimension(funct_parameter_size), intent(inout) :: funct_parameter !< parameter of the function funct
          my_real :: brent_algo !< root value 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: condition
          my_real :: save_a,save_b,c,delta,d,s,length
          my_real :: f_a,f_b,f_c
          my_real :: r1,r2,r3
          my_real :: new_tol
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!   -----------------------------------------------

          ! -----------------
          ! save a and f(a)
          save_a = a
          f_a = funct(save_a,funct_parameter)
          ! -----------------

          ! -----------------
          ! save b and f(b)
          save_b = b
          f_b = funct(save_b,funct_parameter)
          ! -----------------        

          condition = .true.
          ! -----------------        
          ! check if the signs of f(a) & f(b) : if there is no sign change, f(x)=0 does not exist in the [a,b] interval
          if(sign(one, f_a) == sign(one, f_b)) then
            condition = .false.
            stop
          endif
          ! -----------------

          c = save_a
          f_c = f_a
          delta = save_b - save_a
          d = delta

          do while(condition)
            ! -----------------           
            ! check the value of f(c) & f(b)
            ! change the 2 bounds if f(b)>f(c)
            if(abs(f_c)<abs(f_b)) then
              save_a = save_b
              save_b = c
              c = save_a
              f_a = f_b
              f_b = f_c
              f_c = f_a
            endif
            ! compute the tolerance
            new_tol = two * epsilon (save_b) * abs(save_b) + tolerance
            length = half * ( c - save_b )

            if((new_tol<abs(length)).and.f_b/=zero) then

              if( (abs( delta )>=new_tol).and.(abs(f_a)>abs(f_b)) ) then
                ! -----------------       
                ! interpolation algo                
                s = f_b / f_a
                if( save_a /= c ) then
                  ! inverse quadratic interpolation
                  r1 = f_a / f_c
                  r2 = f_b / f_c
                  r3 = s * ( two * length * r1 * ( r1 - r2 ) - ( save_b - save_a ) * ( r2 - one ) )
                  r1 = ( r1 - one ) * ( r2 - one ) * ( s - one )
                else
                  ! linear interpolation secant
                  ! p = 2 * 1/2 * (c-b) * f(b)/f(a)
                  ! q = 1 - f(b)/f(a)
                  r3 = two * length * s
                  r1 = one - s
                endif

                if (r3<=zero) then
                  r3 = - r3
                else
                  r1 = -r1
                endif

                s = delta
                delta = d
                ! check if interpolation is good enought
                if ( (two*r3>=(three*length*r1-abs(new_tol*r1)) ).or.( r3>=abs(half*s*r1) ) ) then
                  delta = length
                  d = delta
                else
                  d = r3 / r1
                endif
              else
                ! -----------------       
                ! bisection algo  
                delta = length
                d = delta
              endif

              save_a = save_b
              f_a = f_b
              ! next point
              if ( new_tol<abs(d) ) then
                save_b = save_b + d
              else if ( zero<length ) then
                save_b = save_b + new_tol
              else
                save_b = save_b - new_tol
              endif
      
              f_b = funct(save_b,funct_parameter)
      
              if ( ( zero < f_b .and. zero < f_c ) .or. ( f_b <= zero .and. f_c <= zero ) ) then
                c = save_a
                f_c = f_a
                delta = save_b - save_a
                d = delta
              endif
            else
              condition=.false.
            endif
          enddo


          brent_algo = save_b

          return
          end function brent_algo
! ----------------------------------------------------------------------------------------------------------------------
        end module root_finding_algo_mod
