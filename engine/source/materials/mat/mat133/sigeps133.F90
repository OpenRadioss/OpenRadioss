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
      !||    sigeps133_mod   ../engine/source/materials/mat/mat133/sigeps133.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps133_mod
        contains
  ! ======================================================================================================================
  ! \brief material/MAT/LAW133 subroutine
  ! \details 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps133               ../engine/source/materials/mat/mat133/sigeps133.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    debug_mod               ../engine/share/modules/debug_mod.F
      !||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
        subroutine sigeps133(                                          &
          nel      ,matparam ,et       , &
          sigy     ,dpla     ,defp     , &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   , &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   , &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   , &
          ssp      ,off      ,pnew     , &
          dpdm     ,rho      ,rho0     ,nvartmp  ,vartmp )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod , only : matparam_struct_
          use constant_mod , only : zero, em20, third, half, one, four_over_3, two, three
          use table_mat_vinterp_mod , only : table_mat_vinterp
          !use debug_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          !integer, intent(in) :: nuvar !< number of user variables
          !real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
          real(kind=WP), dimension(nel), intent(inout) :: et ! Coefficient for hourglass
          real(kind=WP), dimension(nel), intent(in) :: rho !< mass density
          real(kind=WP), dimension(nel), intent(in) :: rho0 !< mass density
          real(kind=WP), dimension(nel), intent(inout) :: sigy !< yield stress
          real(kind=WP), dimension(nel), intent(inout) :: dpla !< cumulated plastic strain increment 
          real(kind=WP), dimension(nel), intent(inout) :: defp !< cumulated plastic strain
          real(kind=WP), dimension(nel), intent(in) :: depsxx !< strain increment xx
          real(kind=WP), dimension(nel), intent(in) :: depsyy !< strain increment yy
          real(kind=WP), dimension(nel), intent(in) :: depszz !< strain increment zz 
          real(kind=WP), dimension(nel), intent(in) :: depsxy !< strain increment xy 
          real(kind=WP), dimension(nel), intent(in) :: depsyz !< strain increment yz 
          real(kind=WP), dimension(nel), intent(in) :: depszx !< strain increment zx 
          real(kind=WP), dimension(nel), intent(in) :: sigoxx !< initial stress xx 
          real(kind=WP), dimension(nel), intent(in) :: sigoyy !< initial stress yy
          real(kind=WP), dimension(nel), intent(in) :: sigozz !< initial stress zz 
          real(kind=WP), dimension(nel), intent(in) :: sigoxy !< initial stress xy 
          real(kind=WP), dimension(nel), intent(in) :: sigoyz !< initial stress yz 
          real(kind=WP), dimension(nel), intent(in) :: sigozx !< initial stress zx 
          real(kind=WP), dimension(nel), intent(out) :: signxx !< new stress xx 
          real(kind=WP), dimension(nel), intent(out) :: signyy !< new stress yy
          real(kind=WP), dimension(nel), intent(out) :: signzz !< new stress zz 
          real(kind=WP), dimension(nel), intent(out) :: signxy !< new stress xy 
          real(kind=WP), dimension(nel), intent(out) :: signyz !< new stress yz 
          real(kind=WP), dimension(nel), intent(out) :: signzx !< new stress zx
          real(kind=WP), dimension(nel), intent(inout) :: ssp !< sound speed
          real(kind=WP), dimension(nel), intent(inout) :: off !< element deletion flag
          real(kind=WP), dimension(nel), intent(inout) :: dpdm !< pressure total derivative
          real(kind=WP), dimension(nel), intent(in) :: pnew !< current pressure from mmain > eosmain
          integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: pold(nel) !< old pressure
          real(kind=WP) :: pmin  !< minimum pressure (or fracture pressure)
          real(kind=WP) :: nu    !< Poisson's ratio
          integer :: i !< loop index
          real(kind=WP), dimension(nel,1) :: xvec1 !<temporary array for table interpolation
          real(kind=WP) :: shear(nel) !< Shear modulus G
          real(kind=WP) :: slope(nel,1) !<required for table interpolation
          real(kind=WP) :: G2(nel)  !< G2 = 2*G
          real(kind=WP) :: j2, vm(nel), g0, yield2, ratio  !< variables for yield function projection
          real(kind=WP) :: dav
          logical, parameter :: opt_extrapolate = .false.

!         integer nc
!         NC = nc_debug  !debug_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          !========================================================================
          !< Recovering constant parameters
          !========================================================================

          nu = matparam%nu
          pmin = matparam%uparam(1)

          !========================================================================
          !< Recovering shear modulus for each element : G = G(rho)
          !========================================================================
          !
          xvec1(1:nel,1) = rho(1:nel)
          ! matparam%table(1) : G(rho) function
          ! ivartmp(1,1) : backup index poisition to optimize search during next cycle
          ! xvec1 : abscissa for table interpolation (in)
          ! shear : ordinate for table interpolation (out)
          ! slope : slope for table interpolation (out)
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1,1),xvec1,shear,slope,opt_extrapolate)
          g2(1:nel) = two*shear(1:nel)

          !========================================================================
          !< Computation of elastic deviatoric stresses and equivalent stress
          !========================================================================
          do i=1,nel
            dav       =  (depsxx(i) + depsyy(i) + depszz(i))*third
            pold(i)   = -(sigoxx(i) + sigoyy(i) + sigozz(i))*third
            signxx(i) = sigoxx(i) + pold(i) + g2(i)*(depsxx(i)-dav)
            signyy(i) = sigoyy(i) + pold(i) + g2(i)*(depsyy(i)-dav)
            signzz(i) = sigozz(i) + pold(i) + g2(i)*(depszz(i)-dav)
            signxy(i) = sigoxy(i) + shear(i)*depsxy(i)
            signyz(i) = sigoyz(i) + shear(i)*depsyz(i)
            signzx(i) = sigozx(i) + shear(i)*depszx(i)
            j2        = half*(signxx(i)**2+signyy(i)**2+signzz(i)**2) + signxy(i)**2+signyz(i)**2+signzx(i)**2
            vm(i)     = sqrt(three*j2)
          enddo

           !========================================================================
          !< Solid sound speed
          !========================================================================
          do i=1,nel
            ssp(i) = sqrt(  (dpdm(i) + four_over_3*shear(i)) / rho0(i) )
          end do

          !========================================================================
          !< Recovering Yield function for each element : sigy=Y(P)
          !========================================================================
          xvec1(1:nel,1) = pold(1:nel)
          call table_mat_vinterp(matparam%table(2),nel,nel,vartmp(1,2),xvec1,sigy,slope,opt_extrapolate)

          !========================================================================
          !< Apply the yield function with user function Y=Y(P)
          !========================================================================
          do i=1,nel
            g0 = sigy(i)
            g0 = max(zero,g0)
            if( pnew(i) <= pmin ) g0 = zero
            ratio = one
            if(g0 == zero)then
              ratio = zero
            elseif( vm(i) > g0 )then     ! at the yield surface or within the yield surface
              ratio = g0/vm(i)
              dpla(i) = (one-ratio)*vm(i) / max(em20,three*shear(i))
              defp(i) = defp(i) + dpla(i)
            endif
            signxx(i) = ratio*signxx(i)*off(i) - pnew(i)
            signyy(i) = ratio*signyy(i)*off(i) - pnew(i)
            signzz(i) = ratio*signzz(i)*off(i) - pnew(i)
            signxy(i) = ratio*signxy(i)*off(i)
            signyz(i) = ratio*signyz(i)*off(i)
            signzx(i) = ratio*signzx(i)*off(i)
          end do

          !========================================================================
          !< Coefficient for Hourglass Control
          !========================================================================
          et(1:nel) = one

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine sigeps133
      end module sigeps133_mod  
