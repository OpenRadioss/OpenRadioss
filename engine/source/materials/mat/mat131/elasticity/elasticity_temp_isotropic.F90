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
!||    elasticity_temp_isotropic_mod   ../engine/source/materials/mat/mat131/elasticity/elasticity_temp_isotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress     ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||====================================================================
      module elasticity_temp_isotropic_mod
! \brief Compute temperature-dependent isotropic elastic stress for /MAT/LAW131
! \details Compute the elastic stress tensor using temperature-dependent
!          isotropic elasticity for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasticity_temp_isotropic     ../engine/source/materials/mat/mat131/elasticity/elasticity_temp_isotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp             ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod         ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine elasticity_temp_isotropic(                                    &
        matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,           &
        temp     ,nvartmp  ,vartmp   ,young    ,nuvar    ,uvar     )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use table_mat_vinterp_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),            intent(in)    :: matparam !< Material parameters data
        integer,                           intent(in)    :: nel      !< Number of elements in the group
        integer,                           intent(in)    :: eltype   !< Element type (1 for solids, 2 for shells)
        integer,                           intent(in)    :: ieos     !< Equation of state type
        real(kind=WP), dimension(nel),     intent(in)    :: rho      !< Density
        real(kind=WP), dimension(nel),     intent(in)    :: dpdm     !< Derivative of pressure with respect to volumetric strain
        real(kind=WP), dimension(nel),     intent(in)    :: depsxx   !< Strain increment component xx
        real(kind=WP), dimension(nel),     intent(in)    :: depsyy   !< Strain increment component yy
        real(kind=WP), dimension(nel),     intent(in)    :: depszz   !< Strain increment component zz
        real(kind=WP), dimension(nel),     intent(in)    :: depsxy   !< Strain increment component xy
        real(kind=WP), dimension(nel),     intent(in)    :: depsyz   !< Strain increment component yz
        real(kind=WP), dimension(nel),     intent(in)    :: depszx   !< Strain increment component zx
        real(kind=WP), dimension(nel),     intent(in)    :: sigoxx   !< Stress tensor component xx at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoyy   !< Stress tensor component yy at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigozz   !< Stress tensor component zz at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoxy   !< Stress tensor component xy at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoyz   !< Stress tensor component yz at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigozx   !< Stress tensor component zx at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(inout) :: signxx   !< Stress tensor component xx at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signyy   !< Stress tensor component yy at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signzz   !< Stress tensor component zz at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signxy   !< Stress tensor component xy at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signyz   !< Stress tensor component yz at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signzx   !< Stress tensor component zx at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(in)    :: shf      !< Shell thickness
        real(kind=WP), dimension(nel,6,6), intent(inout) :: cstf     !< Elastic stiffness matrix
        real(kind=WP), dimension(nel),     intent(inout) :: soundsp  !< Sound speed
        real(kind=WP), dimension(nel),     intent(inout) :: s13      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(inout) :: s23      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(inout) :: s43      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(in)    :: temp     !< Temperature
        integer,                           intent(in)    :: nvartmp  !< Number of temporary variables for table interpolation
        integer,dimension(nel,nvartmp),    intent(inout) :: vartmp   !< Temporary variable array for table interpolation
        real(kind=WP), dimension(nel),     intent(inout) :: young    !< Young modulus
        integer,                           intent(in)    :: nuvar    !< Number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar   !< Internal variable array
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,offset_var
        real(kind=WP), dimension(nel) :: eheat,ecool,nutemp,dedt,dnudt,temp0,nu
        real(kind=WP), dimension(nel,1) :: xvec
        real(kind=WP), dimension(nel) :: epsxxe,epsyye,epszze,epsxye,epsyze,epszxe
        real(kind=WP), dimension(nel) :: shear,lam,aii,aij,nu0,young0,bulk
!===============================================================================
!
        !=======================================================================
        !< - Temperature-dependent isotropic elasticity
        !=======================================================================
        !< Young modulus
        young(1:nel) = matparam%young
        nu(1:nel)    = matparam%nu
        offset_var   = matparam%iparam(5)
!
        !< Interpolation of Young modulus and Poisson ratio
        !  at current temperature for all elements
        xvec(1:nel,1) = temp(1:nel)
        if (matparam%table(1)%notable > 0) then
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1:nel,1),&
            xvec(1:nel,1),eheat,dedt)
        else
          eheat(1:nel) = young(1:nel)
        endif
        if (matparam%table(2)%notable > 0) then
          call table_mat_vinterp(matparam%table(2),nel,nel,vartmp(1:nel,2),    &
            xvec(1:nel,1),ecool,dedt)
        else
          ecool(1:nel) = young(1:nel)
        endif
        if (matparam%table(3)%notable > 0) then
          call table_mat_vinterp(matparam%table(3),nel,nel,vartmp(1:nel,3),    &
            xvec(1:nel,1),nutemp,dnudt)
        else
          nutemp(1:nel) = nu(1:nel)
        endif
        !< Compute the current young modulus and Poisson ratio
        if (matparam%table(1)%notable > 0) then
          if (matparam%table(2)%notable == 0) then 
            young(1:nel) = eheat(1:nel)
          elseif (matparam%table(2)%notable > 0) then
            do i = 1,nel
              if (temp(i) > uvar(i,offset_var + 3)) then
                young(i) = eheat(i)
              else
                young(i) = ecool(i)
              endif
            enddo
          endif
        endif
        if (matparam%table(3)%notable > 0) then
          nu(1:nel) = min(nutemp(1:nel),0.495_WP)
        endif
!
        !< Save initial Young modulus
        where (uvar(1:nel,offset_var + 1) == zero)
          uvar(1:nel,offset_var + 1) = young(1:nel)
        end where
        !< Save initial Poisson ratio
        where (uvar(1:nel,offset_var + 2) == zero)
          uvar(1:nel,offset_var + 2) = nu(1:nel)
        end where
        !< Save current temperature
        uvar(1:nel,offset_var + 3) = temp(1:nel)
!
        !< Recover old Young modulus and Poisson ratio
        young0(1:nel) = uvar(1:nel,offset_var + 1)
        nu0(1:nel)    = uvar(1:nel,offset_var + 2)
!
        !< Solids
        if (eltype == 1) then
          !< Elastic strain at previous time step 
          epsxxe(1:nel) = sigoxx(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoyy(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigozz(1:nel)/young0(1:nel)
          epsyye(1:nel) = sigoyy(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoxx(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigozz(1:nel)/young0(1:nel)
          epszze(1:nel) = sigozz(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoxx(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoyy(1:nel)/young0(1:nel)
          epsxye(1:nel) = sigoxy(1:nel)*two*(one + nu0(1:nel))/young0(1:nel)
          epsyze(1:nel) = sigoyz(1:nel)*two*(one + nu0(1:nel))/young0(1:nel)
          epszxe(1:nel) = sigozx(1:nel)*two*(one + nu0(1:nel))/young0(1:nel)
          !< Elastic stiffness matrix
          bulk(1:nel)  = young(1:nel)/(three*(one - two*nu(1:nel)))
          shear(1:nel) = young(1:nel)/(two*(one + nu(1:nel)))
          lam(1:nel)   = young(1:nel)*nu(1:nel)/(one + nu(1:nel))/             &
                                            (one - two*nu(1:nel))
          cstf(1:nel,1,1) = lam(1:nel) + two*shear(1:nel)
          cstf(1:nel,2,2) = lam(1:nel) + two*shear(1:nel)
          cstf(1:nel,3,3) = lam(1:nel) + two*shear(1:nel)
          cstf(1:nel,1,2) = lam(1:nel)
          cstf(1:nel,1,3) = lam(1:nel)
          cstf(1:nel,2,1) = lam(1:nel)
          cstf(1:nel,2,3) = lam(1:nel)
          cstf(1:nel,3,1) = lam(1:nel)
          cstf(1:nel,3,2) = lam(1:nel)
          cstf(1:nel,4,4) = shear(1:nel)
          cstf(1:nel,5,5) = shear(1:nel)
          cstf(1:nel,6,6) = shear(1:nel)
          !< Sound speed
          bulk(1:nel)    = max(bulk(1:nel) ,matparam%bulk)
          shear(1:nel)   = max(shear(1:nel),matparam%shear)
          if (ieos > 0) then 
            soundsp(1:nel) = sqrt((dpdm(1:nel) +                               &
                                    four_over_3*shear(1:nel))/rho(1:nel))
          else
            soundsp(1:nel) = sqrt((bulk(1:nel) + four_over_3*shear(1:nel))/    &
                                                                rho(1:nel))
          endif
        !< Shells
        elseif (eltype == 2) then 
          !< Elastic strain at previous time step
          epsxxe(1:nel) = sigoxx(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoyy(1:nel)/young0(1:nel)
          epsyye(1:nel) = sigoyy(1:nel)/young0(1:nel) -                        &
               nu0(1:nel)*sigoxx(1:nel)/young0(1:nel)
          epszze(1:nel) = zero
          epsxye(1:nel) = sigoxy(1:nel)*two*(one + nu0(1:nel))/young0(1:nel)
          epsyze(1:nel) = sigoyz(1:nel)*two*(one + nu0(1:nel))/                &
                                    max((young0(1:nel)*shf(1:nel)),em20)
          epszxe(1:nel) = sigozx(1:nel)*two*(one + nu0(1:nel))/                &
                                    max((young0(1:nel)*shf(1:nel)),em20)
          !< Elastic stiffness matrix
          aii(1:nel) = young(1:nel)/(one - nu(1:nel)*nu(1:nel))
          aij(1:nel) = nu(1:nel)*aii(1:nel)
          shear(1:nel) = young(1:nel)/(two*(one + nu(1:nel)))
          cstf(1:nel,1,1) = aii(1:nel)
          cstf(1:nel,2,2) = aii(1:nel)
          cstf(1:nel,1,2) = aij(1:nel)
          cstf(1:nel,2,1) = aij(1:nel)
          cstf(1:nel,4,4) = shear(1:nel)
          cstf(1:nel,5,5) = shear(1:nel)*shf(1:nel)
          cstf(1:nel,6,6) = shear(1:nel)*shf(1:nel)
          !< Compliance matrix components for thickness update
          s13(1:nel) = - nu(1:nel) / young(1:nel)
          s23(1:nel) = - nu(1:nel) / young(1:nel)
          s43(1:nel) = zero
          !< Sound speed
          aii(1:nel) = max(aii(1:nel),matparam%young/                          &
                                      (one - matparam%nu*matparam%nu))
          soundsp(1:nel) = sqrt(aii(1:nel)/rho(1:nel))
        endif
!
        !< Save elastic constants
        uvar(1:nel,offset_var + 1) = young(1:nel)
        uvar(1:nel,offset_var + 2) = nu(1:nel)
!
        !=======================================================================
        !< Elastic trial stress computation
        !=======================================================================
#include "vectorize.inc"
        do i = 1,nel
          signxx(i) = cstf(i,1,1)*(epsxxe(i) + depsxx(i)) +                    &
                      cstf(i,1,2)*(epsyye(i) + depsyy(i)) +                    &
                      cstf(i,1,3)*(epszze(i) + depszz(i))
          signyy(i) = cstf(i,2,1)*(epsxxe(i) + depsxx(i)) +                    &
                      cstf(i,2,2)*(epsyye(i) + depsyy(i)) +                    &
                      cstf(i,2,3)*(epszze(i) + depszz(i))
          signzz(i) = cstf(i,3,1)*(epsxxe(i) + depsxx(i)) +                    & 
                      cstf(i,3,2)*(epsyye(i) + depsyy(i)) +                    & 
                      cstf(i,3,3)*(epszze(i) + depszz(i))
          signxy(i) = cstf(i,4,4)*(epsxye(i) + depsxy(i))
          signyz(i) = cstf(i,5,5)*(epsyze(i) + depsyz(i))
          signzx(i) = cstf(i,6,6)*(epszxe(i) + depszx(i))
        enddo
!
      end subroutine elasticity_temp_isotropic
      end module elasticity_temp_isotropic_mod
