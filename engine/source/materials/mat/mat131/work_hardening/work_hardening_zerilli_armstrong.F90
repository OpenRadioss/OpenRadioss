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
      module work_hardening_zerilli_armstrong_mod
! \brief Compute Zerilli work hardening for /MAT/LAW131
! \details Compute the isotropic work hardening stress using the Zerilli-Armstrong model for /MAT/LAW131.
      contains
        subroutine work_hardening_zerilli_armstrong(                           &
          matparam ,nel      ,sigy     ,pla      ,dsigy_dpla,offset   ,epsd   ,&
          temp     ,dtemp_dpla)
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
          real(kind=WP), dimension(nel), intent(in)    :: epsd       !< Strain rate
          real(kind=WP), dimension(nel), intent(in)    :: temp       !< Temperature
          real(kind=WP), dimension(nel), intent(in)    :: dtemp_dpla !< Derivative of temperature w.r.t. cumulated plastic strain
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: iflag
          real(kind=WP) :: c0,c1,c2,c3,c4,c5,n,eref
          real(kind=WP), dimension(nel) :: exp_term,lograte
!===============================================================================
!
          !=====================================================================
          !< - Zerilli-Armstrong hardening model
          !=====================================================================
          iflag = int(matparam%uparam(offset + 1))
          !< FCC materials hardening
          if (iflag == 1) then
            !< Recover work hardening parameters
            c0   = matparam%uparam(offset + 2) !< Initial yield stress
            c2   = matparam%uparam(offset + 3) !< Linear hardening modulus
            c3   = matparam%uparam(offset + 4) !< Thermal softening parameter
            c4   = matparam%uparam(offset + 5) !< Thermal-strain rate parameter
            n    = matparam%uparam(offset + 6) !< Hardening exponent
            eref = matparam%uparam(offset + 7) !< Reference strain rate
            !< Yield stress and derivative of yield stress w.r.t. cumulated plastic strain
            lograte(1:nel) = log(max(epsd(1:nel),em20)/eref)
            exp_term(1:nel) = exp(-c3*temp(1:nel) + c4*temp(1:nel)*lograte(1:nel))
            sigy(1:nel) = c0 + (c2*(pla(1:nel))**n)*exp_term(1:nel)
            dsigy_dpla(1:nel) = c2*n*(max(pla(1:nel),em20))**(n-1)*            &
              exp_term(1:nel) + (c2*(pla(1:nel))**n)*(-c3 +                    &
              c4*lograte(1:nel))*exp_term(1:nel)*dtemp_dpla(1:nel)
          !< BCC materials hardening
          elseif (iflag == 2) then
            !< Recover work hardening parameters
            c0   = matparam%uparam(offset + 2) !< Initial yield stress
            c1   = matparam%uparam(offset + 3) !< Thermal-rate dependency modulus
            c3   = matparam%uparam(offset + 4) !< Thermal softening parameter
            c4   = matparam%uparam(offset + 5) !< Thermal-strain rate parameter
            c5   = matparam%uparam(offset + 6) !< Hardening modulus
            n    = matparam%uparam(offset + 7) !< Hardening exponent
            eref = matparam%uparam(offset + 8) !< Reference strain rate
            !< Yield stress and derivative of yield stress w.r.t. cumulated plastic strain
            lograte(1:nel) = log(max(epsd(1:nel),em20)/eref)
            exp_term(1:nel) = exp(-c3*temp(1:nel) + c4*temp(1:nel)*lograte(1:nel))
            sigy(1:nel) = c0 + c1*exp_term(1:nel) + c5*(pla(1:nel))**n
            dsigy_dpla(1:nel) = c5*n*(max(pla(1:nel),em20))**(n-1) +           &
                         (c1*(-c3 + c4*lograte(1:nel))*                        &
                                exp_term(1:nel)*dtemp_dpla(1:nel))
          endif
!
        end subroutine work_hardening_zerilli_armstrong
      end module work_hardening_zerilli_armstrong_mod
