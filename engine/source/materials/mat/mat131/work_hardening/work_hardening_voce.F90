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
!||    work_hardening_voce_mod       ../engine/source/materials/mat/mat131/work_hardening/work_hardening_voce.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module work_hardening_voce_mod
! \brief Compute Voce work hardening for /MAT/LAW131
! \details Compute the isotropic work hardening stress using the Voce
!          (exponential saturation) model for /MAT/LAW131.
      contains
!||====================================================================
!||    work_hardening_voce           ../engine/source/materials/mat/mat131/work_hardening/work_hardening_voce.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine work_hardening_voce(                                          &
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
        real(kind=WP) :: r0,q1,b1,q2,b2,q3,b3
        real(kind=WP), dimension(nel) :: exp_b1_pla, exp_b2_pla, exp_b3_pla             
!===============================================================================
!
        !=======================================================================
        !< - Voce work hardening model
        !=======================================================================
        !< Recover work hardening parameters
        r0 = matparam%uparam(offset + 1) !< Initial yield stress
        q1 = matparam%uparam(offset + 2) !< Voce 1 saturation stress
        b1 = matparam%uparam(offset + 3) !< Voce 1 saturation rate
        q2 = matparam%uparam(offset + 4) !< Voce 2 saturation stress
        b2 = matparam%uparam(offset + 5) !< Voce 2 saturation rate
        q3 = matparam%uparam(offset + 6) !< Voce 3 saturation stress
        b3 = matparam%uparam(offset + 7) !< Voce 3 saturation rate
        exp_b1_pla(1:nel) = exp(-b1*pla(1:nel))
        exp_b2_pla(1:nel) = exp(-b2*pla(1:nel))
        exp_b3_pla(1:nel) = exp(-b3*pla(1:nel))
        sigy(1:nel) = r0 + q1*(one - exp_b1_pla(1:nel)) +                      &
                           q2*(one - exp_b2_pla(1:nel)) +                      &
                           q3*(one - exp_b3_pla(1:nel))
        dsigy_dpla(1:nel) = q1*b1*exp_b1_pla(1:nel) +                          &
                            q2*b2*exp_b2_pla(1:nel) +                          &
                            q3*b3*exp_b3_pla(1:nel)
!
        end subroutine work_hardening_voce
      end module work_hardening_voce_mod
