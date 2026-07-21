!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    sigeps131pi_mod   ../engine/source/materials/mat/mat131/sigeps131pi.F90
!||--- called by ------------------------------------------------------
!||    mulaw_ib          ../engine/source/elements/beam/mulaw_ib.F
!||====================================================================
      module sigeps131pi_mod
! \brief Main stress computation for /MAT/LAW131 (beams)
! \details Main routine for computing stresses and internal variables
!          for beam elements using /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    sigeps131pi               ../engine/source/materials/mat/mat131/sigeps131pi.F90
!||--- called by ------------------------------------------------------
!||    mulaw_ib                  ../engine/source/elements/beam/mulaw_ib.F
!||--- calls      -----------------------------------------------------
!||    cppm_beams                ../engine/source/materials/mat/mat131/return_mapping/cppm_beams.F90
!||    cutting_plane_beams       ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_beams.F90
!||    nice_beams                ../engine/source/materials/mat/mat131/return_mapping/nice_beams.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod              ../common_source/modules/constant_mod.F
!||    cppm_beams_mod            ../engine/source/materials/mat/mat131/return_mapping/cppm_beams.F90
!||    cutting_plane_beams_mod   ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_beams.F90
!||    matparam_def_mod          ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    nice_beams_mod            ../engine/source/materials/mat/mat131/return_mapping/nice_beams.F90
!||    precision_mod             ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine sigeps131pi(                                                  &
        nel      ,matparam ,nvartmp  ,vartmp   ,timestep ,off      ,           &        
        depsxx   ,depsxy   ,depszx   ,sigoxx   ,sigoxy   ,sigozx   ,           &   
        signxx   ,signxy   ,signzx   ,pla      ,seq      ,et       ,           &    
        sigy     ,epsd     ,temp     ,asrate   ,l_sigb   ,sigb     ,           &
        nuvar    ,uvar     ,jthe     )    
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use nice_beams_mod
        use cutting_plane_beams_mod
        use cppm_beams_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        integer,                       intent(in)    :: nvartmp  !< Number of variables used in tabulated variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< Temporary variables for tabulated hardening
        real(kind=WP), intent(in)                    :: timestep !< Time step
        real(kind=WP), dimension(nel), intent(inout) :: off       !< Element failure flag
        real(kind=WP), dimension(nel), intent(in)    :: depsxx   !< Strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsxy   !< Strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depszx   !< Strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx   !< Previous stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy   !< Previous stress xy
        real(kind=WP), dimension(nel), intent(in)    :: sigozx   !< Previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signzx   !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: pla      !< Accumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
        real(kind=WP), dimension(nel), intent(inout) :: et       !< Hourglass stabilization variable
        real(kind=WP), dimension(nel), intent(inout) :: sigy     !< Current yield stress
        real(kind=WP), dimension(nel), intent(inout) :: epsd     !< Plastic strain rate
        real(kind=WP), dimension(nel), intent(inout) :: temp     !< Temperature
        real(kind=WP),                 intent(in)    :: asrate   !< Asrate parameter for rate-dependent plasticity
        integer,                       intent(in)    :: l_sigb   !< Number of backstress components for kinematic hardening
        real(kind=WP), dimension(nel,l_sigb),intent(inout) :: sigb  !< Backstress components for kinematic hardening
        integer,                       intent(in)    :: nuvar    !< Number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables array
        integer,                       intent(in)    :: jthe      !< /HEAT/MAT flag
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
            call nice_beams(                                                   &        
              nel      ,matparam ,nvartmp  ,vartmp   ,off      ,timestep ,     &
              depsxx   ,depsxy   ,depszx   ,sigoxx   ,sigoxy   ,sigozx   ,     &
              signxx   ,signxy   ,signzx   ,pla      ,seq      ,et       ,     &
              sigy     ,epsd     ,temp     ,asrate   ,l_sigb   ,sigb     ,     &
              nuvar    ,uvar     ,jthe     ,vpflag   ,ikine    ,chard    )
          !---------------------------------------------------------------------
          !< - Cutting Plane algorithm
          !---------------------------------------------------------------------
          case(2)
            call cutting_plane_beams(                                          &        
              nel      ,matparam ,nvartmp  ,vartmp   ,off      ,timestep ,     &
              depsxx   ,depsxy   ,depszx   ,sigoxx   ,sigoxy   ,sigozx   ,     &
              signxx   ,signxy   ,signzx   ,pla      ,seq      ,et       ,     &
              sigy     ,epsd     ,temp     ,asrate   ,l_sigb   ,sigb     ,     &
              nuvar    ,uvar     ,jthe     ,vpflag   ,ikine    ,chard    )
          !---------------------------------------------------------------------
          !< - Closest Point Projection algorithm
          !---------------------------------------------------------------------     
          case(3)
            call cppm_beams(                                                   &        
              nel      ,matparam ,nvartmp  ,vartmp   ,off      ,timestep ,     &
              depsxx   ,depsxy   ,depszx   ,sigoxx   ,sigoxy   ,sigozx   ,     &
              signxx   ,signxy   ,signzx   ,pla      ,seq      ,et       ,     &
              sigy     ,epsd     ,temp     ,asrate   ,l_sigb   ,sigb     ,     &
              nuvar    ,uvar     ,jthe     ,vpflag   ,ikine    ,chard    )
        end select
!
       end subroutine sigeps131pi
       end module sigeps131pi_mod
