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
!||    elasto_plastic_yield_stress_mod   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                       ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                       ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells              ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids              ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                       ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                       ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||====================================================================
      module elasto_plastic_yield_stress_mod
! \brief Compute elasto-plastic yield stress for /MAT/LAW131
! \details Compute the yield stress including work hardening, strain rate
!          dependency, and thermal softening for /MAT/LAW131.
      contains
!||====================================================================
!||    elasto_plastic_yield_stress          ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                          ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                          ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells                 ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids                 ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                          ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                          ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- calls      -----------------------------------------------------
!||    self_heating_tabulated               ../engine/source/materials/mat/mat131/self_heating/self_heating_tabulated.F90
!||    self_heating_taylor                  ../engine/source/materials/mat/mat131/self_heating/self_heating_taylor.F90
!||    srate_dependency_cowpersymonds       ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_cowpersymonds.F90
!||    srate_dependency_johnsoncook         ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_johnsoncook.F90
!||    srate_dependency_nonlinear           ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_nonlinear.F90
!||    srate_dependency_tabulated           ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_tabulated.F90
!||    therm_softening_johnsoncook          ../engine/source/materials/mat/mat131/therm_softening/therm_softening_johnsoncook.F90
!||    therm_softening_tabulated            ../engine/source/materials/mat/mat131/therm_softening/therm_softening_tabulated.F90
!||    therm_softening_zhao                 ../engine/source/materials/mat/mat131/therm_softening/therm_softening_zhao.F90
!||    work_hardening_linearvoce            ../engine/source/materials/mat/mat131/work_hardening/work_hardening_linearvoce.F90
!||    work_hardening_powerlaw              ../engine/source/materials/mat/mat131/work_hardening/work_hardening_powerlaw.F90
!||    work_hardening_tabulated             ../engine/source/materials/mat/mat131/work_hardening/work_hardening_tabulated.F90
!||    work_hardening_voce                  ../engine/source/materials/mat/mat131/work_hardening/work_hardening_voce.F90
!||--- uses       -----------------------------------------------------
!||    matparam_def_mod                     ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                        ../common_source/modules/precision_mod.F90
!||    self_heating_tabulated_mod           ../engine/source/materials/mat/mat131/self_heating/self_heating_tabulated.F90
!||    self_heating_taylor_mod              ../engine/source/materials/mat/mat131/self_heating/self_heating_taylor.F90
!||    srate_dependency_cowpersymonds_mod   ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_cowpersymonds.F90
!||    srate_dependency_johnsoncook_mod     ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_johnsoncook.F90
!||    srate_dependency_nonlinear_mod       ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_nonlinear.F90
!||    srate_dependency_tabulated_mod       ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_tabulated.F90
!||    therm_softening_johnsoncook_mod      ../engine/source/materials/mat/mat131/therm_softening/therm_softening_johnsoncook.F90
!||    therm_softening_tabulated_mod        ../engine/source/materials/mat/mat131/therm_softening/therm_softening_tabulated.F90
!||    therm_softening_zhao_mod             ../engine/source/materials/mat/mat131/therm_softening/therm_softening_zhao.F90
!||    work_hardening_linearvoce_mod        ../engine/source/materials/mat/mat131/work_hardening/work_hardening_linearvoce.F90
!||    work_hardening_powerlaw_mod          ../engine/source/materials/mat/mat131/work_hardening/work_hardening_powerlaw.F90
!||    work_hardening_tabulated_mod         ../engine/source/materials/mat/mat131/work_hardening/work_hardening_tabulated.F90
!||    work_hardening_voce_mod              ../engine/source/materials/mat/mat131/work_hardening/work_hardening_voce.F90
!||====================================================================
      subroutine elasto_plastic_yield_stress(                                  &
        matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,nvartmp  ,&
        vartmp   ,temp     ,dtemp_dpla,jthe    )
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
        integer,                         intent(in)    :: jthe       !< /HEAT/MAT flag
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: ihard,iratedep,itherm,iheat,offset
!===============================================================================
!
        !=======================================================================
        !< - Select isotropic work hardening model
        !=======================================================================
        ihard  = matparam%iparam(9)
        offset = matparam%iparam(7)
        select case(ihard)
          !---------------------------------------------------------------------
          !< Power Law work hardening
          !---------------------------------------------------------------------
          case(1)
            call work_hardening_powerlaw(                                      &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla,offset   )
          !---------------------------------------------------------------------
          !< Voce work hardening
          !---------------------------------------------------------------------
          case(2)
            call work_hardening_voce(                                          &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla,offset   )
          !---------------------------------------------------------------------
          !< Tabulated work hardening
          !---------------------------------------------------------------------
          case(3)
            call work_hardening_tabulated(                                     &
              matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,    &
              nvartmp  ,vartmp   ,offset   )
          !---------------------------------------------------------------------
          !< Linear-Voce work hardening
          !---------------------------------------------------------------------
          case(4)
            call work_hardening_linearvoce(                                    &
              matparam ,nel      ,sigy     ,pla      ,dsigy_dpla,offset   )
        end select
!
        !=======================================================================
        !< - Select strain rate dependency model
        !=======================================================================
        iratedep = matparam%iparam(14) 
        offset   = matparam%iparam(11)
        select case (iratedep)
          !---------------------------------------------------------------------
          !< Johnson-Cook strain rate dependency
          !---------------------------------------------------------------------
          case(1)
            call srate_dependency_johnsoncook(                                 &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,offset   )
          !---------------------------------------------------------------------
          !< Cowper-Symonds strain rate dependency
          !---------------------------------------------------------------------
          case(2)
            call srate_dependency_cowpersymonds(                               &
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,offset   )
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
              matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,offset   )
        end select 
!
        !=======================================================================
        !< - Select thermal softening model
        !=======================================================================
        itherm = matparam%iparam(20)
        offset = matparam%iparam(17)
        select case (itherm)
          !---------------------------------------------------------------------
          !< Johnson-Cook thermal softening
          !---------------------------------------------------------------------
          case(1)
            call therm_softening_johnsoncook(                                  &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla   ,&
              offset   )
          !---------------------------------------------------------------------
          !< Zhao thermal softening
          !---------------------------------------------------------------------
          case(2)
            call therm_softening_zhao(                                         &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla   ,&
              offset   )
          !---------------------------------------------------------------------
          !< Tabulated thermal softening
          !---------------------------------------------------------------------
          case(3)
            call therm_softening_tabulated(                                    &
              matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,              &
              nvartmp  ,vartmp   ,pla      )
        end select
!
        !=======================================================================
        !< - Self-heating model
        !=======================================================================
        iheat  = matparam%iparam(25)
        offset = matparam%iparam(22)
        if (jthe == 0) then 
          select case (iheat)
            !-------------------------------------------------------------------
            !< Taylor Quinney (Extended) self heating
            !-------------------------------------------------------------------
            case (1)
              call self_heating_taylor(                                        &
                matparam ,nel     ,sigy    ,dtemp_dpla,epsd   ,offset   )
            !-------------------------------------------------------------------
            !< Tabulated self heating
            !-------------------------------------------------------------------
            case (2)
              call self_heating_tabulated(                                     &
                matparam ,nel     ,sigy    ,dtemp_dpla,epsd   ,nvartmp ,vartmp,&
                temp     ,pla     ,offset  )
          end select
        endif
!
      end subroutine elasto_plastic_yield_stress
      end module elasto_plastic_yield_stress_mod
