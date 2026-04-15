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
!||    work_hardening_powerlaw_mod   ../engine/source/materials/mat/mat131/work_hardening/work_hardening_powerlaw.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module work_hardening_powerlaw_mod
! \brief Compute power law work hardening for /MAT/LAW131
! \details Compute the isotropic work hardening stress using the power law
!          (Hollomon/Ludwik) model for /MAT/LAW131.
      contains
!||====================================================================
!||    work_hardening_powerlaw       ../engine/source/materials/mat/mat131/work_hardening/work_hardening_powerlaw.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine work_hardening_powerlaw(                                      &
        matparam ,nel      ,sigy     ,pla      ,dsigy_dpla,offset   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),        intent(in)    :: matparam   !< Material parameters data
        integer,                       intent(in)    :: nel        !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: sigy       !< Equivalent stress
        real(kind=WP), dimension(nel), intent(inout) :: pla        !< Cumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: dsigy_dpla !< Derivative of eq. stress w.r.t. cumulated plastic strain
        integer,                       intent(in)    :: offset     !< Offset in the material parameters array for work hardening parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i
        real(kind=WP) :: ca,cb,cn,eps0,sigmax
        real(kind=WP), dimension(nel) :: pla_plus_eps0_pow_cn_minus_1
!===============================================================================
!
        !=======================================================================
        !< - Power law work hardening model
        !=======================================================================
        !< Recover work hardening parameters
        ca     = matparam%uparam(offset + 1) !< Initial yield stress
        cb     = matparam%uparam(offset + 2) !< Hardening modulus
        cn     = matparam%uparam(offset + 3) !< Hardening exponent
        eps0   = matparam%uparam(offset + 4) !< Initial plastic strain
        sigmax = matparam%uparam(offset + 5) !< Maximum yield stress
        pla_plus_eps0_pow_cn_minus_1(1:nel) = (pla(1:nel) + eps0)**(cn - one)
        sigy(1:nel) = ca + cb*(pla(1:nel) + eps0)*                             &
                              pla_plus_eps0_pow_cn_minus_1(1:nel)
        dsigy_dpla(1:nel) = cn*cb*pla_plus_eps0_pow_cn_minus_1(1:nel)
        where (sigy(1:nel) > sigmax) 
          sigy(1:nel) = sigmax
          dsigy_dpla(1:nel) = zero
        end where
!
        end subroutine work_hardening_powerlaw
      end module work_hardening_powerlaw_mod
