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
      !||====================================================================
      !||    sigeps87c_mod   ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc          ../engine/source/materials/mat_share/mulawc.F90
      !||====================================================================
      module sigeps87c_mod
      contains
! ======================================================================================================================
! \brief Barlat 2000 elastoplastic material law 87
! \details Barlat 2000 elastoplastic material law 87
! ======================================================================================================================
      !||====================================================================
      !||    sigeps87c                         ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                            ../engine/source/materials/mat_share/mulawc.F90
      !||--- calls      -----------------------------------------------------
      !||    mat87c_hansel                     ../engine/source/materials/mat/mat087/mat87c_hansel.F90
      !||    mat87c_swift_voce                 ../engine/source/materials/mat/mat087/mat87c_swift_voce.F90
      !||    mat87c_tabulated                  ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
      !||    mat87c_tabulated_3dir_ortho       ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                      ../common_source/modules/constant_mod.F
      !||    interface_table_mod               ../engine/share/modules/table_mod.F
      !||    mat87c_hansel_mod                 ../engine/source/materials/mat/mat087/mat87c_hansel.F90
      !||    mat87c_swift_voce_mod             ../engine/source/materials/mat/mat087/mat87c_swift_voce.F90
      !||    mat87c_tabulated_3dir_ortho_mod   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    mat87c_tabulated_mod              ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
      !||    matparam_def_mod                  ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod                     ../common_source/modules/precision_mod.F90
      !||    table_mod                         ../engine/share/modules/table_mod.F
      !||====================================================================
      subroutine sigeps87c(                                                    &
        nel      ,matparam ,nuvar    ,uvar     ,                               &
        time     ,timestep ,rho0     ,thkly    ,thk      ,                     &
        epspxx   ,epspyy   ,epspxy   ,                                         &
        depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,                     &
        sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,                     &
        signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,                     &
        soundsp  ,pla      ,dpla     ,epsd_pg  ,yld      ,                     &
        etse     ,gs       ,israte   ,asrate   ,epsd     ,                     &
        temp     ,l_sigb   ,sigb     ,inloc    ,dplanl   ,                     &
        seq      ,jthe     ,off      ,loff     ,nvartmp  ,                     &
        vartmp   )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use matparam_def_mod 
        use constant_mod   
        use table_mod
        use interface_table_mod
        use mat87c_tabulated_mod
        use mat87c_swift_voce_mod
        use mat87c_hansel_mod
        use mat87c_tabulated_3dir_ortho_mod
        use precision_mod, only: WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                            :: nel      !< Number of elements
        type(matparam_struct_), intent(in)             :: matparam !< Material parameters data structure
        integer, intent(in)                            :: nuvar    !< Number of user variables 
        real(kind=WP), dimension(nel,nuvar), intent(inout)   :: uvar     !< User variables
        real(kind=WP), intent(in)                            :: time     !< Current time
        real(kind=WP), intent(in)                            :: timestep !< Time step
        real(kind=WP), dimension(nel), intent(in)            :: rho0     !< Initial density
        real(kind=WP), dimension(nel), intent(in)            :: thkly    !< Current layer thickness
        real(kind=WP), dimension(nel), intent(inout)         :: thk      !< ELement total thickness
        real(kind=WP), dimension(nel), intent(in)            :: epspxx   !< Strain rate component xx
        real(kind=WP), dimension(nel), intent(in)            :: epspyy   !< Strain rate component yy
        real(kind=WP), dimension(nel), intent(in)            :: epspxy   !< Strain rate component xy
        real(kind=WP), dimension(nel), intent(in)            :: depsxx   !< Strain increment component xx
        real(kind=WP), dimension(nel), intent(in)            :: depsyy   !< Strain increment component yy
        real(kind=WP), dimension(nel), intent(in)            :: depsxy   !< Strain increment component xy
        real(kind=WP), dimension(nel), intent(in)            :: depsyz   !< Strain increment component yz
        real(kind=WP), dimension(nel), intent(in)            :: depszx   !< Strain increment component zx
        real(kind=WP), dimension(nel), intent(in)            :: sigoxx   !< Old stress component xx
        real(kind=WP), dimension(nel), intent(in)            :: sigoyy   !< Old stress component yy
        real(kind=WP), dimension(nel), intent(in)            :: sigoxy   !< Old stress component xy
        real(kind=WP), dimension(nel), intent(in)            :: sigoyz   !< Old stress component yz
        real(kind=WP), dimension(nel), intent(in)            :: sigozx   !< Old stress component zx
        real(kind=WP), dimension(nel), intent(inout)         :: signxx   !< New stress component xx
        real(kind=WP), dimension(nel), intent(inout)         :: signyy   !< New stress component yy
        real(kind=WP), dimension(nel), intent(inout)         :: signxy   !< New stress component xy
        real(kind=WP), dimension(nel), intent(inout)         :: signyz   !< New stress component yz
        real(kind=WP), dimension(nel), intent(inout)         :: signzx   !< New stress component zx
        real(kind=WP), dimension(nel), intent(inout)         :: soundsp  !< Sound speed
        real(kind=WP), dimension(nel), intent(inout)         :: pla      !< Equivalent plastic strain
        real(kind=WP), dimension(nel), intent(inout)         :: dpla     !< Increment of equivalent plastic strain
        real(kind=WP), dimension(nel), intent(in)            :: epsd_pg  !< Global equivalent strain rate
        real(kind=WP), dimension(nel), intent(inout)         :: yld      !< Yield stress
        real(kind=WP), dimension(nel), intent(inout)         :: etse     !< Hourglass control stiffness
        real(kind=WP), dimension(nel), intent(in)            :: gs       !< Transverse shear modulus 
        integer, intent(in)                            :: israte   !< Flag for strain rate filtering
        real(kind=WP), intent(in)                            :: asrate   !< Strain rate filtering factor
        real(kind=WP), dimension(nel), intent(inout)         :: temp     !< Element temperature
        integer, intent(in)                            :: l_sigb   !< Number of backstress components
        real(kind=WP), dimension(nel,l_sigb), intent(inout)  :: sigb     !< Backstress components
        integer, intent(in)                            :: inloc    !< Flag for non-local regularization
        real(kind=WP), dimension(nel), intent(in)            :: dplanl   !< Non-local plastic strain increment
        real(kind=WP), dimension(nel), intent(inout)         :: seq      !< Equivalent stress
        integer, intent(in)                            :: jthe     !< Flag for thermal effects
        real(kind=WP), dimension(nel), intent(in)            :: off      !< Flag for element deletion
        real(kind=WP), dimension(nel), intent(in)            :: loff     !< Flag for Gauss point deletion
        integer, intent(in)                            :: nvartmp  !< Number of temporary variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables
        real(kind=WP), dimension(nel), intent(inout)         :: epsd     !< local strain rate
!-----------------------------------------------
!  L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: iflag
!-----------------------------------------------
!
        !< Yield stress formulation flag
        iflag = matparam%iparam(1) 
!
        !< Select corresponding material routine
        select case (iflag)
          !< Tabulated
          case(0)
            call mat87c_tabulated(                                             &
              nel    ,matparam,nvartmp ,vartmp  ,timestep ,                    &
              rho0   ,thkly   ,thk     ,epsd_pg ,                              &
              epspxx ,epspyy  ,epspxy  ,                                       &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsd    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )
          !< Swift-Voce with Cowper-Symonds
          case(1)
            call mat87c_swift_voce(                                            &
              nel    ,matparam,timestep,                                       &
              rho0   ,thkly   ,thk     ,epsd_pg ,                              &
              epspxx ,epspyy  ,epspxy  ,                                       &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsd    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )    
          !< Hansel 
          case(2)
            call mat87c_hansel(                                                &
              nel    ,matparam,nuvar   ,uvar    ,                              &
              rho0   ,thkly   ,thk     ,epsd_pg ,time     ,                    &
              temp   ,jthe    ,                                                &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsd    ,yld      ,                    &
              etse   ,gs      ,off     ,                                       &
              l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )
          !< Tabulated 3 directions orthotropic yield stress
          case(3)
            call mat87c_tabulated_3dir_ortho(                                  &
              nel    ,matparam,nvartmp ,vartmp  ,timestep ,                    &
              rho0   ,thkly   ,thk     ,epsd_pg ,                              &
              epspxx ,epspyy  ,epspxy  ,                                       &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsd    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )
        end select
      end subroutine sigeps87c
! ======================================================================================================================
      end module sigeps87c_mod
