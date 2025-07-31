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
!||    sigeps163_mod   ../engine/source/materials/mat/mat163/sigeps163.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps163_mod
        contains
! ======================================================================================================================
! \brief Computation of stress tensor with /MAT/LAW163 (CRUSHABLE_FOAM) theory
! \details Realise a stress scaling according to /MAT/LAW163 theory, using the tabulated 
!          yield stress and the volumetric strain rate. The volumetric strain rate is filtered
!          and the change of volumetric strain rate is capped. The volumetric strain rate is
!          computed according to the user choice (engineering or true strain rate).
!          A viscous damping is added to the stress tensor.
! ======================================================================================================================       
!||====================================================================
!||    sigeps163               ../engine/source/materials/mat/mat163/sigeps163.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||    valpvec_v               ../engine/source/materials/mat/mat033/sigeps33.F
!||    valpvecdp_v             ../engine/source/materials/mat/mat033/sigeps33.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps163(                                                  &
          nel      ,nuvar    ,uvar     ,matparam ,timestep ,et       ,         &
          rho0     ,sigy     ,ssp      ,nvartmp  ,vartmp   ,mvsiz    ,         &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
          epsxx    ,epsyy    ,epszz    ,epsxy    ,epsyz    ,epszx    ,         &
          epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,         &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          sigvxx   ,sigvyy   ,sigvzz   ,sigvxy   ,sigvyz   ,sigvzx   ,         &
          aldt     ,rho      ,iresp    ,plas     ,epsd     )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod 
          use constant_mod      
          use table_mat_vinterp_mod
          use precision_mod, only : WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in)                    :: nel        !< number of elements in the group
          integer, intent(in)                    :: nuvar      !< number of user variables
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in)     :: matparam   !< material parameters data
          real(kind=WP), intent(in)                    :: timestep   !< time step
          real(kind=WP), dimension(nel), intent(inout) :: et         !< coefficient for hourglass
          real(kind=WP), dimension(nel), intent(in)    :: rho0       !< material initial density
          real(kind=WP), dimension(nel), intent(inout) :: sigy       !< yield stress
          real(kind=WP), dimension(nel), intent(inout) :: ssp        !< sound speed
          integer, intent(in)                    :: nvartmp    !< number of temporary variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< temporary variables
          integer, intent(in)                    :: mvsiz      !< maximum size of the arrays
          real(kind=WP), dimension(nel), intent(in)    :: depsxx     !< strain increment xx 
          real(kind=WP), dimension(nel), intent(in)    :: depsyy     !< strain increment yy
          real(kind=WP), dimension(nel), intent(in)    :: depszz     !< strain increment zz 
          real(kind=WP), dimension(nel), intent(in)    :: depsxy     !< strain increment xy 
          real(kind=WP), dimension(nel), intent(in)    :: depsyz     !< strain increment yz 
          real(kind=WP), dimension(nel), intent(in)    :: depszx     !< strain increment zx 
          real(kind=WP), dimension(nel), intent(in)    :: epsxx      !< strain xx
          real(kind=WP), dimension(nel), intent(in)    :: epsyy      !< strain yy
          real(kind=WP), dimension(nel), intent(in)    :: epszz      !< strain zz
          real(kind=WP), dimension(nel), intent(in)    :: epsxy      !< strain xy
          real(kind=WP), dimension(nel), intent(in)    :: epsyz      !< strain yz
          real(kind=WP), dimension(nel), intent(in)    :: epszx      !< strain zx
          real(kind=WP), dimension(nel), intent(in)    :: epspxx     !< strain rate xx
          real(kind=WP), dimension(nel), intent(in)    :: epspyy     !< strain rate yy
          real(kind=WP), dimension(nel), intent(in)    :: epspzz     !< strain rate zz
          real(kind=WP), dimension(nel), intent(in)    :: epspxy     !< strain rate xy
          real(kind=WP), dimension(nel), intent(in)    :: epspyz     !< strain rate yz
          real(kind=WP), dimension(nel), intent(in)    :: epspzx     !< strain rate zx
          real(kind=WP), dimension(nel), intent(in)    :: sigoxx     !< initial stress xx 
          real(kind=WP), dimension(nel), intent(in)    :: sigoyy     !< initial stress yy
          real(kind=WP), dimension(nel), intent(in)    :: sigozz     !< initial stress zz 
          real(kind=WP), dimension(nel), intent(in)    :: sigoxy     !< initial stress xy 
          real(kind=WP), dimension(nel), intent(in)    :: sigoyz     !< initial stress yz 
          real(kind=WP), dimension(nel), intent(in)    :: sigozx     !< initial stress zx 
          real(kind=WP), dimension(nel), intent(inout) :: signxx     !< new stress xx 
          real(kind=WP), dimension(nel), intent(inout) :: signyy     !< new stress yy
          real(kind=WP), dimension(nel), intent(inout) :: signzz     !< new stress zz 
          real(kind=WP), dimension(nel), intent(inout) :: signxy     !< new stress xy 
          real(kind=WP), dimension(nel), intent(inout) :: signyz     !< new stress yz 
          real(kind=WP), dimension(nel), intent(inout) :: signzx     !< new stress zx 
          real(kind=WP), dimension(nel), intent(inout) :: sigvxx     !< viscous stress xx
          real(kind=WP), dimension(nel), intent(inout) :: sigvyy     !< viscous stress yy
          real(kind=WP), dimension(nel), intent(inout) :: sigvzz     !< viscous stress zz
          real(kind=WP), dimension(nel), intent(inout) :: sigvxy     !< viscous stress xy
          real(kind=WP), dimension(nel), intent(inout) :: sigvyz     !< viscous stress yz
          real(kind=WP), dimension(nel), intent(inout) :: sigvzx     !< viscous stress zx
          real(kind=WP), dimension(nel), intent(in)    :: aldt       !< element characteristic length
          real(kind=WP), dimension(nel), intent(in)    :: rho        !< material current density
          integer, intent(in)                    :: iresp      !< precision flag
          real(kind=WP), dimension(nel), intent(inout) :: plas       !< effective volumetric true strain
          real(kind=WP), dimension(nel), intent(inout) :: epsd       !< effective volumetric strain rate
!-----------------------------------------------
!  L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j,ncycle,nrs,nindx,indx(nel),iter,ndim
          real(kind=WP) :: young,nu,g,bulk,cii,cij,tsc,damp,srclmt,alpha,ldav,a
          real(kind=WP), dimension(nel) :: dgamdt,gama,dsdgam,le,seq,epst
          real(kind=WP), dimension(mvsiz,6) :: sig,eps
          real(kind=WP), dimension(mvsiz,3) :: sigp,epsp
          real(kind=WP), dimension(mvsiz,3,3) :: dirp
          real(kind=WP), dimension(nel,2) :: xvec
!
          !=====================================================================
          !< - Initialisation of computation on time step
          !=====================================================================
          !< Recovering integer model parameter
          ncycle = matparam%iparam(1) !< Number of cycles for filtering
          nrs    = matparam%iparam(2) !< Strain rate type flag
          !< Recovering real model parameters
          young  = matparam%young     !< Young modulus
          nu     = matparam%nu        !< Poisson ratio
          g      = matparam%shear     !< Shear modulus
          bulk   = matparam%bulk      !< Bulk modulus
          cii    = matparam%uparam(1) !< Diagonal coefficient of stiffness matrix
          cij    = matparam%uparam(2) !< Non-diagonal coefficient of stiffness matrix
          tsc    = matparam%uparam(3) !< Tensile stress cut-off
          damp   = matparam%uparam(4) !< Damping coefficient
          srclmt = matparam%uparam(5) !< Strain-rate change limit
          !< Volumetric strain rate filtering coefficient
          alpha = two*pi/(two*pi + ncycle)
!
          !< Save the initial element length
          if (uvar(1,2) == zero) then
            do i = 1,nel
              uvar(i,2) = aldt(i)
            enddo
          endif
          le(1:nel) = uvar(1:nel,2) 
          !< Coefficient for hourglass control
          et(1:nel) = zero
! 
          !=====================================================================
          !< - Computation of trial stress tensor and principal stresses
          !=====================================================================
          !< Trial stress tensor
          do i = 1,nel
            signxx(i) = sigoxx(i) + cii*depsxx(i) + cij*depsyy(i) + cij*depszz(i)
            signyy(i) = sigoyy(i) + cij*depsxx(i) + cii*depsyy(i) + cij*depszz(i)
            signzz(i) = sigozz(i) + cij*depsxx(i) + cij*depsyy(i) + cii*depszz(i)
            signxy(i) = sigoxy(i) +   g*depsxy(i)
            signyz(i) = sigoyz(i) +   g*depsyz(i)
            signzx(i) = sigozx(i) +   g*depszx(i)
          enddo
          !< Principal strains  and directions
          eps(1:nel,1) = epsxx(1:nel)
          eps(1:nel,2) = epsyy(1:nel)
          eps(1:nel,3) = epszz(1:nel)
          eps(1:nel,4) = half*epsxy(1:nel)
          eps(1:nel,5) = half*epsyz(1:nel)
          eps(1:nel,6) = half*epszx(1:nel)
          if (iresp == 1) then
            call valpvecdp_v(eps,epsp,dirp,nel)
          else
            call valpvec_v(eps,epsp,dirp,nel)
          endif
          dirp(1:mvsiz,1:3,1:3) = zero
          !< Principal stresses and directions
          sig(1:nel,1) = signxx(1:nel)
          sig(1:nel,2) = signyy(1:nel)
          sig(1:nel,3) = signzz(1:nel)
          sig(1:nel,4) = signxy(1:nel)
          sig(1:nel,5) = signyz(1:nel)
          sig(1:nel,6) = signzx(1:nel)
          if (iresp == 1) then
            call valpvecdp_v(sig,sigp,dirp,nel)
          else
            call valpvec_v(sig,sigp,dirp,nel)
          endif
!
          !=====================================================================
          !< - Volumetric strain and strain rate filtering
          !=====================================================================
          do i = 1,nel
            !< Volumetric strain
            gama(i) = one - rho0(i)/rho(i)
            !< Volumetric strain rate
            ! -> Engineering strain rate
            if (nrs == 1) then 
              dgamdt(i) = (gama(i) - uvar(i,1))/max(timestep,em20)
            ! -> True strain rate
            elseif (nrs == 0) then
              dgamdt(i) = -(epspxx(i)+epspyy(i)+epspzz(i))
            endif
            !< Filtering of the volumetric strain rate
            dgamdt(i) = alpha*dgamdt(i) + (one-alpha)*epsd(i)
            !< Cap the change of volumetric strain rate
            if (abs(dgamdt(i)-epsd(i)) > srclmt*timestep) then
              dgamdt(i) = epsd(i) + sign(one,dgamdt(i)-epsd(i))*srclmt*timestep
            endif
            !< Effective volumetric true strain (output purpose)
            plas(i) = log(rho0(i)/rho(i))
            !< Volumetric strain rate (output purpose)
            epsd(i) = dgamdt(i)
          enddo
!
          !=====================================================================
          !< - Yield stress computation
          !=====================================================================
          do i = 1,nel
            xvec(i,1) = max(gama(i)  ,zero)
            xvec(i,2) = max(dgamdt(i),zero)
          enddo
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp,xvec,sigy,dsdgam)
          do i = 1,nel
            sigy(i) = -abs(sigy(i))
          enddo
!
          !=====================================================================
          !< - Stress scaling procedure
          !=====================================================================
          do i = 1,nel
            !< Scaling of principal stresses
            do j = 1,3
              if (sigp(i,j) < sigy(i)) then
                sigp(i,j) = sigy(i)
                et(i) = max(dsdgam(i)/(dsdgam(i) + bulk),et(i))
              elseif (sigp(i,j) > tsc) then
                sigp(i,j) = tsc
                dsdgam(i) = zero
                et(i) = one
              else
                dsdgam(i) = zero
                et(i) = one
              endif
            enddo
            !< equivalent stress for hourglass control
            seq(i)  = sqrt(sigp(i,1)**2 + sigp(i,2)**2 + sigp(i,3)**2)
            !< equivalent strain for hourglass control
            epst(i) = sqrt(epsp(i,1)**2 + epsp(i,2)**2 + epsp(i,3)**2)
            !< Update the global stress tensor
            signxx(i) = dirp(i,1,1)*dirp(i,1,1)*sigp(i,1)                      &
                      + dirp(i,1,2)*dirp(i,1,2)*sigp(i,2)                      &
                      + dirp(i,1,3)*dirp(i,1,3)*sigp(i,3)                      
            signyy(i) = dirp(i,2,2)*dirp(i,2,2)*sigp(i,2)                      &
                      + dirp(i,2,3)*dirp(i,2,3)*sigp(i,3)                      &
                      + dirp(i,2,1)*dirp(i,2,1)*sigp(i,1)                      
            signzz(i) = dirp(i,3,3)*dirp(i,3,3)*sigp(i,3)                      &
                      + dirp(i,3,1)*dirp(i,3,1)*sigp(i,1)                      &
                      + dirp(i,3,2)*dirp(i,3,2)*sigp(i,2)                      
            signxy(i) = dirp(i,1,1)*dirp(i,2,1)*sigp(i,1)                      &
                      + dirp(i,1,2)*dirp(i,2,2)*sigp(i,2)                      &
                      + dirp(i,1,3)*dirp(i,2,3)*sigp(i,3)                      
            signyz(i) = dirp(i,2,2)*dirp(i,3,2)*sigp(i,2)                      &
                      + dirp(i,2,3)*dirp(i,3,3)*sigp(i,3)                      &
                      + dirp(i,2,1)*dirp(i,3,1)*sigp(i,1)                      
            signzx(i) = dirp(i,3,3)*dirp(i,1,3)*sigp(i,3)                      &
                      + dirp(i,3,1)*dirp(i,1,1)*sigp(i,1)                      &
                      + dirp(i,3,2)*dirp(i,1,2)*sigp(i,2)  
          enddo 
!
          !=====================================================================
          !< - Update user variables and sound speed
          !=====================================================================
          do i=1,nel
            !< Sound speed
            ssp(i) = sqrt((bulk + four_over_3*g)/min(rho(i),rho0(i)))
            !< User variables
            uvar(i,1) = gama(i) !< Volumetric strain
          enddo     
!
          !=====================================================================
          !< - Viscous damping
          !===================================================================== 
          do i = 1,nel
            !< Viscous damping coefficient
            a = ssp(i)*rho(i)*damp*le(i)/(one + gama(i))
            ldav = third*(epspxx(i) + epspyy(i) + epspzz(i))
            !< Viscous stresses
            sigvxx(i) = a*((epspxx(i) - ldav)/(one + nu) + ldav/(one - two*nu))
            sigvyy(i) = a*((epspyy(i) - ldav)/(one + nu) + ldav/(one - two*nu))
            sigvzz(i) = a*((epspzz(i) - ldav)/(one + nu) + ldav/(one - two*nu))
            sigvxy(i) = a*epspxy(i)/(two*(one + nu))
            sigvyz(i) = a*epspyz(i)/(two*(one + nu))
            sigvzx(i) = a*epspzx(i)/(two*(one + nu))
            !< Update the soundspeed to include the viscous damping stiffness
            if (timestep > zero) then 
              ssp(i) = sqrt((bulk + four_over_3*g + a/timestep)/min(rho(i),rho0(i)))
            else
              ssp(i) = sqrt((bulk + four_over_3*g)/min(rho(i),rho0(i)))          
            endif
          enddo
!
        end subroutine sigeps163
      end module sigeps163_mod  
