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
!Chd|====================================================================
!Chd|  crack_depth_init          source/materials/fail/windshield_alter/crack_depth_init.F
!Chd|-- called by -----------
!Chd|    fail_windshield_init   ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
!
!Chd|        c3init3                       source/elements/sh3n/coque3n/c3init3.F
!Chd|        cinit3                        source/elements/shell/coque/cinit3.F
!Chd|        cbainit3                      source/elements/shell/coque/cbainit3.F
!Chd|-- calls ---------------
!Chd|====================================================================
      !||====================================================================
      !||    crack_depth_init_mod   ../starter/source/materials/fail/windshield_alter/crack_depth_init.F90
      !||--- called by ------------------------------------------------------
      !||    fail_windshield_init   ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||====================================================================
      module crack_depth_init_mod
      contains
! ========================================================================================
! \brief initialize crack depth for /fail/alter
! \details crack depth is different in edge elements as well as in top and bottom integration points

! ========================================================================================

      !||====================================================================
      !||    crack_depth_init       ../starter/source/materials/fail/windshield_alter/crack_depth_init.F90
      !||--- called by ------------------------------------------------------
      !||    fail_windshield_init   ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      subroutine crack_depth_init(nel,ipt,npt,nuparam,nuvar,uparam,uvar,dfmax,dadv)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod ,only : zero,one,two,pi
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!    D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in)  :: nel                             !< size of element group
      integer ,intent(in)  :: ipt                             !< current integration point in thickness
      integer ,intent(in)  :: npt                             !< number of integration points in thickness
      integer ,intent(in)  :: nuparam                         !< number of failure model parameters
      integer ,intent(in)  :: nuvar                           !< number of state variables
      my_real ,dimension(nuparam)   ,intent(in)    :: uparam  !< failure model parameter table
      my_real ,dimension(nel,nuvar) ,intent(inout) :: uvar    !< state variables of failure model
      my_real ,dimension(nel)       ,intent(in)    :: dfmax   !< initial damage
      my_real ,dimension(nel)       ,intent(inout) :: dadv    !< reduction factor for crack propagation
!-----------------------------------------------
!    L o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,iedge
      my_real :: aa,bb,ai,formf
      my_real :: cr_foil,cr_air,cr_core,cr_edge
      my_real :: k_ic,k_th,v0,exp_n,exp_m
      my_real :: sigp_akt,sigp_min,sigp_max
!=======================================================================
      cr_foil  = uparam(2)
      cr_air   = uparam(3)
      cr_core  = uparam(4)
      cr_edge  = uparam(5)
      exp_n    = uparam(1)
      k_ic     = uparam(6)
      k_th     = uparam(7)
      v0       = uparam(8)
      exp_m    = one / (one + exp_n)
!
      do i =1,nel
        iedge = uvar(i,10)
        if (iedge == 1) then   ! edge element
          ai    = cr_edge
          formf = 1.12
        else
          formf = one
          if (ipt == 1) then
            ai = cr_foil  ! crack depth at foil side in mm (unit system: length)
          elseif (ipt == npt) then
            ai = cr_air   ! crack depth at top side in mm (unit system: length)
          else
            ai = cr_core  ! crack depth inside glass
          endif
        end if
        ai = ai * (one - dfmax(i))    ! take into account the initial damage
!
        aa = two*(exp_n + one)*k_ic**exp_n
        bb = (exp_n - two)*v0*(formf*sqrt(pi))**exp_n*ai**((EXP_N-TWO)/TWO)
        sigp_akt = (aa / bb)**exp_m
        sigp_min = k_th / (formf*sqrt(pi*ai))
        sigp_max = k_ic / (formf*sqrt(pi*ai))
!
        uvar(i,5) = sigp_min
        uvar(i,6) = sigp_max
        uvar(i,7) = formf
        uvar(i,8) = ai
        uvar(i,9) = sigp_akt      
        uvar(i,11)= one   ! dam1
        uvar(i,12)= one   ! dam2
        uvar(i,15)= one   ! fail alter formulation flag
        dadv(i)   = one
      end do
!-----------
      return
      end subroutine crack_depth_init
!-----------
      end module crack_depth_init_mod
