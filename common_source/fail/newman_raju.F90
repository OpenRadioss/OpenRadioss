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
!Chd|====================================================================
!Chd|  newman_raju          common_source/fail/newman_raju.F
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|====================================================================
      !||====================================================================
      !||    newman_raju_mod       ../common_source/fail/newman_raju.F90
      !||--- called by ------------------------------------------------------
      !||    brokmann_crack_init   ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||    fail_brokmann         ../engine/source/materials/fail/alter/fail_brokmann.F
      !||====================================================================
      module newman_raju_mod
      contains
! ========================================================================================
! \brief calculates geometry correction factor according to NewmanRaju 1981 for /fail/alter
! \details
! ========================================================================================

      !||====================================================================
      !||    newman_raju           ../common_source/fail/newman_raju.F90
      !||--- called by ------------------------------------------------------
      !||    brokmann_crack_init   ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||    fail_brokmann         ../engine/source/materials/fail/alter/fail_brokmann.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||====================================================================
      subroutine newman_raju(c,a,t,b,fpi,y)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod ,only : zero,half,one,two,pi
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      my_real c, a, t, b, fpi, y
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      my_real q,m1,m2,m3,g,fphi,fb,fw,f,at,ac,sinp,cosp
!==========================================================================
      if (fpi == half) then
        sinp = one
        cosp = zero
      else if (fpi == zero) then
        sinp = zero
        cosp = one
      else
        sinp = sin(fpi*pi)
        cosp = cos(fpi*pi)
      end if

      ac   = a / c
      at   = a / t
      q    = one + 1.464*ac**1.65

      m1   = 1.13-0.09*ac
      m2   = -0.54+0.89/(0.2 + ac)
      m3   = half - one/(0.65 +ac) + 14.*(one-ac)**24
      g    = one +(0.1+ 0.35*(at)**2)*(one-sinp)**2
     
      fphi = (ac**2 * cosp**2 + sinp**2 )**0.25
      fb   = pi*c*sqrt(at) 

      fw   = cos(pi*c/(two*b)*sqrt(at))
      
      f    = (m1+m2*(at)**2 + m3*(at)**4)*fphi*g / sqrt(ABS(FW))
      y    = sqrt(one/q)*f
!-----------
      return
      end subroutine newman_raju
!-----------
      end module newman_raju_mod
