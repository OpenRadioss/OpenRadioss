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
      !||====================================================================
      !||    jwl_eos_mod           ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
      !||--- called by ------------------------------------------------------
      !||    mixture_equilibrium   ../engine/source/materials/mat/mat041/sigeps41.F
      !||====================================================================
      module jwl_eos_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief jwl function for law 41
      !||====================================================================
      !||    jwl_eos_state         ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
      !||--- called by ------------------------------------------------------
      !||    jwl_eos_delta         ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
      !||    mixture_equilibrium   ../engine/source/materials/mat/mat041/sigeps41.F
      !||====================================================================
        subroutine jwl_eos_state(a,b,r1,r2 ,r3,cv ,eta,tmp,dedv,p,bth,dpdt,en)
!  JWL EoS
!  p = a*exp(-r1/eta) + b*exp(-r2/eta) + r3*eta*tmp
!  cv = volumetric heat capacity (cte)
!  eta : rho/rho0 = V0/V = 1/v  (v relative volume)
!
!  input : eta, tmp
!  output : p, en, dedv, bth, dpdT (JWL derivative)
!
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
          my_real, intent(in) :: a  !< a
          my_real, intent(in) :: b !< b
          my_real, intent(in) :: r1 !< r1
          my_real, intent(in) :: r2 !< r2
          my_real, intent(in) :: r3 !< r3
          my_real, intent(in) :: cv !< volume heat capacity (cte)
          my_real, intent(in) :: eta !< rho/rho0 = V0/V = 1/v  (v relative volume)
          my_real, intent(in) :: tmp !< tmp
          my_real, intent(out) :: dedv !< dedv
          my_real, intent(out) :: p !< p
          my_real, intent(out) :: bth !< bth
          my_real, intent(out) :: dpdt !< dpdt
          my_real, intent(out) :: en !< en
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: trans1,trans2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          trans1 = a * exp(-r1/eta)
          trans2 = b * exp(-r2/eta)
          dedv = -trans1 -trans2
          p = -dedv + r3*eta*tmp
          bth = r3*tmp + (r1*trans1+r2*trans2)/eta**2
          dpdT = r3*eta
          en = trans1/r1 + trans2/r2 + cv*tmp
          return
        end



!! \brief jwl function to be called in sigeps41
      !||====================================================================
      !||    jwl_eos_delta         ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
      !||--- called by ------------------------------------------------------
      !||    mixture_equilibrium   ../engine/source/materials/mat/mat041/sigeps41.F
      !||--- calls      -----------------------------------------------------
      !||    jwl_eos_state         ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||====================================================================
        function jwl_eos_delta(beta,funct_parameter)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : ONE
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
          my_real, intent(in) :: beta !< argument
          my_real, dimension(25), intent(inout) :: funct_parameter !< function parameters
          my_real :: jwl_eos_delta !< return value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: tmp
          my_real :: ar,br,r1r,r2r,r3r,cvr,dedvr,preac,bthr,dpdtr,enr
          my_real :: ap,bp,r1p,r2p,r3p,cvp,dedvp,pprod,bthp,dpdtp,enp
          my_real :: etac,fc,fc1,etar,etap
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ar = funct_parameter(1)
          br = funct_parameter(2)
          r1r = funct_parameter(3)
          r2r = funct_parameter(4)
          r3r = funct_parameter(5)
          cvr = funct_parameter(6)
          tmp = funct_parameter(7)
          dedvr = funct_parameter(8)
          preac = funct_parameter(9)
          bthr = funct_parameter(10)
          dpdtr = funct_parameter(11)
          enr = funct_parameter(12)

          ap = funct_parameter(13)
          bp = funct_parameter(14)
          r1p = funct_parameter(15)
          r2p = funct_parameter(16)
          r3p = funct_parameter(17)
          cvp = funct_parameter(18)
          dedvp = funct_parameter(19)
          pprod = funct_parameter(20)
          bthp = funct_parameter(21)
          dpdtp = funct_parameter(22)
          enp = funct_parameter(23)

          etac = funct_parameter(24)
          fc = funct_parameter(25)
          fc1 = ONE - fc

          etar = fc1 * etac/beta
          call jwl_eos_state(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,tmp,dedvr,preac,bthr,dpdtr,enr )
          etap = fc * etac/(ONE -beta)
          call jwl_eos_state(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,tmp,dedvp,pprod,bthp,dpdtp,enp )
          jwl_eos_delta = preac-pprod
          return
        end function jwl_eos_delta
      end module jwl_eos_mod
