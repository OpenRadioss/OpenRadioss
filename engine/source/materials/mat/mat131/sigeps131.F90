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
!||    sigeps131_mod   ../engine/source/materials/mat/mat131/sigeps131.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps131_mod
! \brief Main stress computation for /MAT/LAW131 (solids)
! \details Main routine for computing stresses and internal variables
!          for solid elements using /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    sigeps131                  ../engine/source/materials/mat/mat131/sigeps131.F90
!||--- called by ------------------------------------------------------
!||    mulaw                      ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    cppm_solids                ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_solids       ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_solids                ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod               ../common_source/modules/constant_mod.F
!||    cppm_solids_mod            ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_solids_mod   ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    matparam_def_mod           ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    nice_solids_mod            ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine sigeps131(                                                    &
        nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,           &     
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,           &
        sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,           &
        nuvar    ,uvar     ,l_sigb   ,sigb     ,ieos     ,dpdm     ,           &
        jthe     ,fheat    ,voln     ,inloc    ,dplanl   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use nice_solids_mod
        use cutting_plane_solids_mod
        use cppm_solids_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel        !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam   !< Material parameters data
        real(kind=WP), dimension(nel), intent(in)    :: rho        !< Density at current time
        real(kind=WP), dimension(nel), intent(in)    :: depsxx     !< Strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsyy     !< Strain increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depszz     !< Strain increment zz
        real(kind=WP), dimension(nel), intent(in)    :: depsxy     !< Strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depsyz     !< Strain increment yz
        real(kind=WP), dimension(nel), intent(in)    :: depszx     !< Strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: epspxx     !< Strain rate component xx
        real(kind=WP), dimension(nel), intent(in)    :: epspyy     !< Strain rate component yy
        real(kind=WP), dimension(nel), intent(in)    :: epspzz     !< Strain rate component zz
        real(kind=WP), dimension(nel), intent(in)    :: epspxy     !< Strain rate component xy
        real(kind=WP), dimension(nel), intent(in)    :: epspyz     !< Strain rate component yz
        real(kind=WP), dimension(nel), intent(in)    :: epspzx     !< Strain rate component zx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx     !< Previous stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy     !< Previous stress yy
        real(kind=WP), dimension(nel), intent(in)    :: sigozz     !< Previous stress zz
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy     !< Previous stress xy
        real(kind=WP), dimension(nel), intent(in)    :: sigoyz     !< Previous stress yz
        real(kind=WP), dimension(nel), intent(in)    :: sigozx     !< Previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx     !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy     !< Current stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signzz     !< Current stress zz
        real(kind=WP), dimension(nel), intent(inout) :: signxy     !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signyz     !< Current stress yz
        real(kind=WP), dimension(nel), intent(inout) :: signzx     !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: soundsp    !< Current sound speed
        real(kind=WP), dimension(nel), intent(inout) :: off        !< Element failure flag
        real(kind=WP), dimension(nel), intent(inout) :: pla        !< Accumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: dpla       !< Plastic strain increment
        real(kind=WP), dimension(nel), intent(inout) :: seq        !< Equivalent stress
        real(kind=WP), dimension(nel), intent(inout) :: et         !< Hourglass stabilization variable
        real(kind=WP), dimension(nel), intent(inout) :: sigy       !< Current yield stress
        real(kind=WP), intent(in)                    :: timestep   !< Time step
        real(kind=WP), dimension(nel), intent(inout) :: epsd       !< Plastic strain rate
        integer,                       intent(in)    :: iresp      !< Precision flag
        integer,                       intent(in)    :: nvartmp    !< Number of variables used in tabulated variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables for tabulated hardening
        real(kind=WP), dimension(nel), intent(inout) :: temp       !< Temperature
        integer,                       intent(in)    :: israte     !< Strain rate filtering flag
        real(kind=WP),                 intent(in)    :: asrate     !< Strain rate filtering weighting factor
        integer,                       intent(in)    :: nuvar      !< Number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables
        integer,                       intent(in)    :: l_sigb     !< Size of backstress array
        real(kind=WP),dimension(nel,l_sigb),intent(inout) :: sigb  !< Backstress components for kinematic hardening
        integer,                       intent(in)    :: ieos       !< Equation of state flag
        real(kind=WP), dimension(nel), intent(inout) :: dpdm       !< Pressure derivative of the shear modulus for EOS coupling
        integer,                       intent(in)    :: jthe       !< /HEAT/MAT flag 
        real(kind=WP), dimension(nel), intent(inout) :: fheat      !< Heat energy accumulated for /HEAT/MAT
        real(kind=WP), dimension(nel), intent(in)    :: voln       !< Current element volume
        integer,                       intent(in)    :: inloc      !< Non-local regularization flag
        real(kind=WP), dimension(nel), intent(in)    :: dplanl     !< Non-local plastic strain increment
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: ires,vpflag,ikine
        real(kind=WP) :: chard
!===============================================================================
!
        !< Return mapping algorithm flag
        ires = matparam%iparam(33)
        !< Viscoplastic formulation flag
        vpflag = matparam%iparam(15)
        !< Kinematic hardening flag
        ikine = matparam%iparam(30)
        !< Mixed kinematic/isotropic hardening parameter
        chard = matparam%uparam(matparam%iparam(27) + 1)
!
        !=======================================================================
        !< - Select return mapping algorithm
        !=======================================================================
        select case (ires)
          !---------------------------------------------------------------------
          !< - Next Increment Correct Error (NICE) algorithm
          !---------------------------------------------------------------------
          case(1) 
            call nice_solids(                                                  &        
              nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,     &     
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,     &
              sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,     &
              l_sigb   ,sigb     ,nuvar    ,uvar     ,ieos     ,dpdm     ,     &
              jthe     ,fheat    ,voln     ,vpflag   ,ikine    ,chard    ,     &
              inloc    ,dplanl   )
          !---------------------------------------------------------------------
          !< - Cutting Plane algorithm
          !---------------------------------------------------------------------
          case(2)
            call cutting_plane_solids(                                         &        
              nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,     &     
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,     &
              sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,     &
              l_sigb   ,sigb     ,nuvar    ,uvar     ,ieos     ,dpdm     ,     &
              jthe     ,fheat    ,voln     ,vpflag   ,ikine    ,chard    ,     &
              inloc    ,dplanl   )
          !---------------------------------------------------------------------
          !< - Closest Point Projection algorithm
          !---------------------------------------------------------------------     
          case(3)
            call cppm_solids(                                                  &        
              nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,     &     
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,     &
              sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,     &
              l_sigb   ,sigb     ,nuvar    ,uvar     ,ieos     ,dpdm     ,     &
              jthe     ,fheat    ,voln     ,vpflag   ,ikine    ,chard    ,     &
              inloc    ,dplanl   )
        end select
!
       end subroutine sigeps131
       end module sigeps131_mod
