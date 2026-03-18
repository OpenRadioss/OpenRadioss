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
      module elasto_plastic_yield_stress_mod
      contains
      subroutine elasto_plastic_yield_stress(                                  &
        matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,nvartmp  ,&
        vartmp   ,temp     ,dtemp_dpla)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use precision_mod, only : WP
        use work_hardening_powerlaw_mod
        use work_hardening_voce_mod
        use work_hardening_tabulated_mod
        use work_hardening_linearvoce_mod
        use srate_dependency_johnsoncook_mod
        use srate_dependency_cowpersymonds_mod
        use srate_dependency_tabulated_mod
        use srate_dependency_nonlinear_mod
        use therm_softening_johnsoncook_mod
        use therm_softening_zhao_mod
        use therm_softening_tabulated_mod
        use self_heating_taylor_mod
        use self_heating_tabulated_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),          intent(in)    :: matparam   !< Material parameters data
        integer,                         intent(in)    :: nel        !< Number of elements in the group
        real(kind=WP), dimension(nel),   intent(inout) :: sigy       !< Equivalent stress
        real(kind=WP), dimension(nel),   intent(inout) :: pla        !< Cumulated plastic strain
        real(kind=WP), dimension(nel),   intent(in)    :: epsd       !< Strain rate
        real(kind=WP), dimension(nel),   intent(inout) :: dsigy_dpla !< Derivative of eq. stress w.r.t. cumulated plastic strain
        integer,                         intent(in)    :: nvartmp    !< Number of variables used in tabulated hardening
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp     !< Temporary variables for tabulated hardening
        real(kind=WP), dimension(nel),   intent(inout) :: temp       !< Temperature
        real(kind=WP), dimension(nel),   intent(out)   :: dtemp_dpla !< Derivative of temperature w.r.t. cumulated plastic strain
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: ihard,iratedep,itherm,iheat
!===============================================================================
!
        !=======================================================================
        !< - Select isotropic work hardening model
        !=======================================================================
        ihard = matparam%iparam(5)
        select case(ihard)
          !---------------------------------------------------------------------
          !< Power Law work hardening
          !---------------------------------------------------------------------
          case(1)
            call work_hardening_powerlaw(                                      &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla)
          !---------------------------------------------------------------------
          !< Voce work hardening
          !---------------------------------------------------------------------
          case(2)
            call work_hardening_voce(                                          &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla)
          !---------------------------------------------------------------------
          !< Tabulated work hardening
          !---------------------------------------------------------------------
          case(3)
            call work_hardening_tabulated(                                     &
              matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,    &
              nvartmp  ,vartmp   )
          !---------------------------------------------------------------------
          !< Linear-Voce work hardening
          !---------------------------------------------------------------------
          case(4)
            call work_hardening_linearvoce(                                    &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla)
        end select
!
        !=======================================================================
        !< - Select strain rate dependency model
        !=======================================================================
        iratedep = matparam%iparam(9)   
        select case (iratedep)
          !---------------------------------------------------------------------
          !< Johnson-Cook strain rate dependency
          !---------------------------------------------------------------------
          case(1)
            call srate_dependency_johnsoncook(                                 &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla)
          !---------------------------------------------------------------------
          !< Cowper-Symonds strain rate dependency
          !---------------------------------------------------------------------
          case(2)
            call srate_dependency_cowpersymonds(                               &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla)
          !---------------------------------------------------------------------
          !< Tabulated strain rate dependency
          !---------------------------------------------------------------------
          case(3)
            call srate_dependency_tabulated(                                   &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,nvartmp  ,    &
              vartmp   )
          !---------------------------------------------------------------------
          !< Non-linear strain rate dependency
          !---------------------------------------------------------------------
          case(4)
            call srate_dependency_nonlinear(                                   &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla)
        end select 
!
        !=======================================================================
        !< - Self-heating model
        !=======================================================================
        iheat = matparam%iparam(18)
        select case (iheat)
          !---------------------------------------------------------------------
          !< Taylor Quinney (Extended) self heating
          !---------------------------------------------------------------------
          case (1)
            call self_heating_taylor(                                          &
              matparam ,nel     ,sigy    ,dtemp_dpla,epsd   )
          !---------------------------------------------------------------------
          !< Tabulated self heating
          !---------------------------------------------------------------------
          case (2)
            call self_heating_tabulated(                                       &
              matparam ,nel     ,sigy    ,dtemp_dpla,epsd   ,nvartmp ,vartmp  )
        end select
!
        !=======================================================================
        !< - Select thermal softening model
        !=======================================================================
        itherm = matparam%iparam(14)
        select case (itherm)
          !---------------------------------------------------------------------
          !< Johnson-Cook thermal softening
          !---------------------------------------------------------------------
          case(1)
            call therm_softening_johnsoncook(                                  &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla   )
          !---------------------------------------------------------------------
          !< Zhao thermal softening
          !---------------------------------------------------------------------
          case(2)
            call therm_softening_zhao(                                         &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla   )
          !---------------------------------------------------------------------
          !< Tabulated thermal softening
          !---------------------------------------------------------------------
          case(3)
            call therm_softening_tabulated(                                    &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla   ,&
              nvartmp  ,vartmp   )
        end select
!
      end subroutine elasto_plastic_yield_stress
      end module elasto_plastic_yield_stress_mod
