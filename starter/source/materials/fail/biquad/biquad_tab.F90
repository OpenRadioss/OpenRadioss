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
!||    biquad_tab_mod   ../starter/source/materials/fail/biquad/biquad_tab.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd       ../starter/source/materials/fail/biquad/biquad_upd.F90
!||====================================================================
      module biquad_tab_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief generate tabulated function of failure strain vs triaxiality using biquad equations
        ! ==========================================================================================
!||====================================================================
!||    biquad_tab      ../starter/source/materials/fail/biquad/biquad_tab.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd      ../starter/source/materials/fail/biquad/biquad_upd.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
          subroutine biquad_tab(npt, nuparam, uparam, eta, epsf)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use constant_mod  ,only : zero,one,two,three,four,third,two_third,three_half,sqr3
          use constant_mod  ,only : em10
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global arguments
! --------------------------------------------------------------------------------------------------
          integer ,intent(in) :: npt
          integer ,intent(in) :: nuparam
          real(kind=WP) ,dimension(nuparam) ,intent(in)  :: uparam   !< biquad parameter table
          real(kind=WP) ,dimension(npt)     ,intent(out) :: eta      ! triaxiality table <0,2/3>
          real(kind=WP) ,dimension(npt)     ,intent(out) :: epsf     ! failure plastic strain
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer       :: i,bflag
          real(kind=WP) :: inst0,sqr23
          real(kind=WP) :: x,a,b,c,d,e,f,a1,b1,c1,b2,a2,c2
          real(kind=WP) :: p1x,p1y,s1x,s1y,s2y
!===================================================================================================
          c      = uparam(1)
          b      = uparam(2)
          a      = uparam(3)
          f      = uparam(4)
          e      = uparam(5)
          d      = uparam(6)
          inst0  = uparam(12)
          bflag  = nint(uparam(11))
          sqr23  = (one/sqr3)**2
!
          do i = 1,npt
            x = eta(i)
             if (x <= third) then     ! triax < 1/3
               epsf(i) = c +  b * x + a * x**2
             else                     ! triax > 1/3
              select case (bflag)
               case(1)
                 epsf(i) = f +  e * x + d * x**2
               case(2)
                 if (x <= one/sqr3) then                      ! triax < 0.57735
                   p1x  = third
                   p1y  = c + b * p1x + a * p1x**2
                   s1x  = one/sqr3
                   s1y  = f + e / sqr3  + d * sqr23
                   a1   = (p1y - s1y) / (p1x - s1x)**2
                   b1   = -two * a1 * s1x
                   c1   = a1 * s1x**2 + s1y 
                   epsf(i) = c1 + b1 * x + a1 * x**2
                 else                                         ! triax > 0.57735
                   p1x  = two * third
                   p1y  = f + e * p1x + d * p1x**2
                   s1x  = one/sqr3
                   s1y  = f + e / sqr3  + d * sqr23
                   a1   = (p1y - s1y) / (p1x - s1x)**2
                   b1   = -two * a1 * s1x
                   c1   = a1 * s1x**2 + s1y 
                   epsf(i) = c1 + b1 * x + a1 * x**2
                 endif
               case(3)
                 if (x <= one/sqr3) then                      ! triax < 0.57735
                   p1x  = third
                   p1y  = c + b * p1x + a * p1x**2
                   s1x  = one/sqr3
                   s1y  = f + e / sqr3  + d * sqr23
                   s2y  = inst0
                   a1   = (p1y - s1y) / (p1x - s1x)**2
                   a2   = (p1y - s2y) / (p1x - s1x)**2
                   b1   = -two * a1 * s1x
                   b2   = -two * a2 * s1x
                   c1   = a1 * s1x**2 + s1y 
                   c2   = a2 * s1x**2 + s2y 
                   epsf(i) = c1 + b1 * x + a1 * x**2
                 else                                          ! triax > 0.57735
                   p1x  = two * third
                   p1y  = f + e * p1x + d * p1x**2
                   s1x  = one/sqr3
                   s1y  = f + e / sqr3  + d * sqr23
                   s2y  = inst0
                   a1   = (p1y - s1y) / (p1x - s1x)**2
                   a2   = (p1y - s2y) / (p1x - s1x)**2
                   b1   = -two * a1 * s1x
                   b2   = -two * a2 * s1x
                   c1   = a1 * s1x**2 + s1y 
                   c2   = a2 * s1x**2 + s2y 
                   epsf(i) = c1 + b1 * x + a1 * x**2
                 endif
               end select
             endif
          end do
!-----------------------------------------------
          return 
          end subroutine biquad_tab
          end module biquad_tab_mod


