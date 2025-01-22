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
      !||    mat87c_hansel                     ../engine/source/materials/mat/mat087/mat87c_hansel.F
      !||    mat87c_swift_voce                 ../engine/source/materials/mat/mat087/mat87c_swift_voce.F
      !||    mat87c_tabulated_3dir_ortho       ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    mat87c_tabulated_plas_sr          ../engine/source/materials/mat/mat087/mat87c_tabulated_plas_sr.F
      !||    mat87c_tabulated_totalsr          ../engine/source/materials/mat/mat087/mat87c_tabulated_totalsr.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                      ../common_source/modules/constant_mod.F
      !||    interface_table_mod               ../engine/share/modules/table_mod.F
      !||    mat87c_tabulated_3dir_ortho_mod   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    matparam_def_mod                  ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    table_mod                         ../engine/share/modules/table_mod.F
      !||====================================================================
      subroutine sigeps87c(                                                    &
        nel      ,matparam ,nuvar    ,uvar     ,nfunc    ,                     &
        ifunc    ,snpc     ,npf      ,stf      ,tf       ,                     &
        time     ,timestep ,rho0     ,thkly    ,thk      ,                     &
        epspxx   ,epspyy   ,epspxy   ,epspyz   ,epspzx   ,                     &
        depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,                     &
        sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,                     &
        signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,                     &
        soundsp  ,pla      ,dpla     ,epsp     ,yld      ,                     &
        etse     ,gs       ,israte   ,asrate   ,facyldi  ,                     &
        tempel   ,sigb     ,inloc    ,dplanl   ,seq      ,                     &
        jthe     ,off      ,loff     ,numtabl  ,itable   ,                     &
        ntable   ,table    ,nvartmp  ,vartmp   ,epsd     )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use matparam_def_mod 
        use constant_mod   
        use table_mod
        use interface_table_mod
        use mat87c_tabulated_3dir_ortho_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
#include  "my_real.inc"
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                            :: nel      !< Number of elements
        type(matparam_struct_), intent(in)             :: matparam !< Material parameters data structure
        integer, intent(in)                            :: nuvar    !< Number of user variables 
        my_real, dimension(nel,nuvar), intent(inout)   :: uvar     !< User variables
        integer, intent(in)                            :: nfunc    !< Number of functions
        integer, dimension(nfunc), intent(in)          :: ifunc    !< Function index
        integer, intent(in)                            :: snpc     !< Size of npf table
        integer, dimension(snpc), intent(in)           :: npf      !< Number of points in the function
        integer, intent(in)                            :: stf      !< Size of stf table
        my_real, dimension(stf), intent(in)            :: tf       !< Points of the function
        my_real, intent(in)                            :: time     !< Current time
        my_real, intent(in)                            :: timestep !< Time step
        my_real, dimension(nel), intent(in)            :: rho0     !< Initial density
        my_real, dimension(nel), intent(in)            :: thkly    !< Current layer thickness
        my_real, dimension(nel), intent(inout)         :: thk      !< ELement total thickness
        my_real, dimension(nel), intent(in)            :: epspxx   !< Strain rate component xx
        my_real, dimension(nel), intent(in)            :: epspyy   !< Strain rate component yy
        my_real, dimension(nel), intent(in)            :: epspxy   !< Strain rate component xy
        my_real, dimension(nel), intent(in)            :: epspyz   !< Strain rate component yz
        my_real, dimension(nel), intent(in)            :: epspzx   !< Strain rate component zx
        my_real, dimension(nel), intent(in)            :: depsxx   !< Strain increment component xx
        my_real, dimension(nel), intent(in)            :: depsyy   !< Strain increment component yy
        my_real, dimension(nel), intent(in)            :: depsxy   !< Strain increment component xy
        my_real, dimension(nel), intent(in)            :: depsyz   !< Strain increment component yz
        my_real, dimension(nel), intent(in)            :: depszx   !< Strain increment component zx
        my_real, dimension(nel), intent(in)            :: sigoxx   !< Old stress component xx
        my_real, dimension(nel), intent(in)            :: sigoyy   !< Old stress component yy
        my_real, dimension(nel), intent(in)            :: sigoxy   !< Old stress component xy
        my_real, dimension(nel), intent(in)            :: sigoyz   !< Old stress component yz
        my_real, dimension(nel), intent(in)            :: sigozx   !< Old stress component zx
        my_real, dimension(nel), intent(inout)         :: signxx   !< New stress component xx
        my_real, dimension(nel), intent(inout)         :: signyy   !< New stress component yy
        my_real, dimension(nel), intent(inout)         :: signxy   !< New stress component xy
        my_real, dimension(nel), intent(inout)         :: signyz   !< New stress component yz
        my_real, dimension(nel), intent(inout)         :: signzx   !< New stress component zx
        my_real, dimension(nel), intent(inout)         :: soundsp  !< Sound speed
        my_real, dimension(nel), intent(inout)         :: pla      !< Equivalent plastic strain
        my_real, dimension(nel), intent(inout)         :: dpla     !< Increment of equivalent plastic strain
        my_real, dimension(nel), intent(inout)         :: epsp     !< Total equivalent and filtered strain rate
        my_real, dimension(nel), intent(inout)         :: yld      !< Yield stress
        my_real, dimension(nel), intent(inout)         :: etse     !< Hourglass control stiffness
        my_real, dimension(nel), intent(in)            :: gs       !< Transverse shear modulus 
        integer, intent(in)                            :: israte   !< Flag for strain rate filtering
        my_real, intent(in)                            :: asrate   !< Strain rate filtering factor
        my_real, dimension(nel), intent(inout)         :: facyldi  !< Factor for equivalent plastic strain rate
        my_real, dimension(nel), intent(in)            :: tempel   !< Element temperature
        my_real, dimension(nel,12), intent(inout)      :: sigb     !< Backstress components
        integer, intent(in)                            :: inloc    !< Flag for non-local regularization
        my_real, dimension(nel), intent(in)            :: dplanl   !< Non-local plastic strain increment
        my_real, dimension(nel), intent(inout)         :: seq      !< Equivalent stress
        integer, intent(in)                            :: jthe     !< Flag for thermal effects
        my_real, dimension(nel), intent(in)            :: off      !< Flag for element deletion
        my_real, dimension(nel), intent(in)            :: loff     !< Flag for Gauss point deletion
        integer, intent(in)                            :: numtabl  !< Number of tables
        integer, dimension(numtabl), intent(in)        :: itable   !< Table index
        integer, intent(in)                            :: ntable   !< Number of tables
        type(ttable), dimension(ntable), intent(in)    :: table    !< Tables data
        integer, intent(in)                            :: nvartmp  !< Number of temporary variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables
        my_real, dimension(nel), intent(inout)         :: epsd     !< Output strain rate
!-----------------------------------------------
!  L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer iyield
!-----------------------------------------------
!
        !< Yield stress formulation flag
        iyield  = matparam%iparam(2) 
!
        !< Select corresponding material routine
        select case (iyield)
          !< Swift-Voce with Cowper-Symonds
          case(1)
            call mat87c_swift_voce(                                            &
              nel    ,matparam,time    ,timestep,                              &
              nuvar  ,uvar    ,rho0    ,thkly   ,thk      ,                    &
              epspxx ,epspyy  ,epspxy  ,epspyz  ,epspzx   ,                    &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsp    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              sigb   ,inloc   ,dplanl  ,seq     ,loff     )    
          !< Hansel 
          case(2)
            call mat87c_hansel(                                                &
              nel    ,matparam,time    ,timestep,                              &
              nuvar  ,uvar    ,rho0    ,thkly   ,thk      ,                    &
              epspxx ,epspyy  ,epspxy  ,epspyz  ,epspzx   ,                    &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsp    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              tempel ,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              jthe   ,loff    )
          !< Tabulated with plastic strain rate
          case(3)
            call mat87c_tabulated_plas_sr(                                     &
              nel    ,matparam,nfunc   ,ifunc   ,snpc     ,                    &
              npf    ,stf     ,tf      ,time    ,timestep ,                    &
              nuvar  ,uvar    ,rho0    ,thkly   ,thk      ,                    &
              epspxx ,epspyy  ,epspxy  ,epspyz  ,epspzx   ,                    &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsp    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              facyldi,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )
          !< Tabulated with total strain rate
          case(4)
            call mat87c_tabulated_totalsr(                                     &
              nel    ,matparam,nfunc   ,ifunc   ,snpc     ,                    &
              npf    ,stf     ,tf      ,time    ,timestep ,                    &
              nuvar  ,uvar    ,rho0    ,thkly   ,thk      ,                    &
              epspxx ,epspyy  ,epspxy  ,epspyz  ,epspzx   ,                    &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsp    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              facyldi,sigb    ,inloc   ,dplanl  ,seq      ,                    &
              loff   )
          !< Tabulated 3 directions orthotropic yield stress
          case(5)
            call mat87c_tabulated_3dir_ortho(                                  &
              nel    ,matparam,numtabl ,itable  ,ntable   ,                    &
              table  ,nvartmp ,vartmp  ,timestep,                              &
              rho0   ,thkly   ,thk     ,epsp    ,                              &
              epspxx ,epspyy  ,epspxy  ,                                       &
              depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                    &
              sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                    &
              signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                    &
              soundsp,pla     ,dpla    ,epsd    ,yld      ,                    &
              etse   ,gs      ,israte  ,asrate  ,off      ,                    &
              sigb   ,inloc   ,dplanl  ,seq     ,loff     )                   
        end select
      end subroutine sigeps87c
! ======================================================================================================================
      end module sigeps87c_mod
