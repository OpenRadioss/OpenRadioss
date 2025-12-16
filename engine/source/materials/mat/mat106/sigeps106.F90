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
!||    sigeps106_mod   ../engine/source/materials/mat/mat106/sigeps106.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps106_mod
      contains
!||====================================================================
!||    sigeps106               ../engine/source/materials/mat/mat106/sigeps106.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    file_descriptor_mod     ../engine/source/modules/file_descriptor_mod.F90
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps106(                                                    &
          nel      ,matparam ,nuvar    ,time     ,rho      ,volume   ,           &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
          soundsp  ,uvar     ,off      ,pla      ,dpla     ,seq      ,           &
          temp     ,jthe     ,jlag     ,fheat    ,et       ,sigy     ,           &
          nvartmp  ,vartmp   ,timestep ,epsd     ,inloc    ,dplanl   ,           &
          ngl      )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use table_mat_vinterp_mod
          use file_descriptor_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                       intent(in)    :: nel      !< Number of elements in the group
          type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
          integer,                       intent(in)    :: nuvar    !< Number of user variables
          real(kind=WP), intent(in)                    :: time     !< Current time
          real(kind=WP), dimension(nel), intent(in)    :: rho      !< Density at current time
          real(kind=WP), dimension(nel), intent(in)    :: volume   !< Volume at current time
          real(kind=WP), dimension(nel), intent(in)    :: depsxx   !< Strain increment xx
          real(kind=WP), dimension(nel), intent(in)    :: depsyy   !< Strain increment yy
          real(kind=WP), dimension(nel), intent(in)    :: depszz   !< Strain increment zz
          real(kind=WP), dimension(nel), intent(in)    :: depsxy   !< Strain increment xy
          real(kind=WP), dimension(nel), intent(in)    :: depsyz   !< Strain increment yz
          real(kind=WP), dimension(nel), intent(in)    :: depszx   !< Strain increment zx
          real(kind=WP), dimension(nel), intent(in)    :: sigoxx   !< Previous stress xx
          real(kind=WP), dimension(nel), intent(in)    :: sigoyy   !< Previous stress yy
          real(kind=WP), dimension(nel), intent(in)    :: sigozz   !< Previous stress zz
          real(kind=WP), dimension(nel), intent(in)    :: sigoxy   !< Previous stress xy
          real(kind=WP), dimension(nel), intent(in)    :: sigoyz   !< Previous stress yz
          real(kind=WP), dimension(nel), intent(in)    :: sigozx   !< Previous stress zx
          real(kind=WP), dimension(nel), intent(inout) :: signxx   !< Current stress xx
          real(kind=WP), dimension(nel), intent(inout) :: signyy   !< Current stress yy
          real(kind=WP), dimension(nel), intent(inout) :: signzz   !< Current stress zz
          real(kind=WP), dimension(nel), intent(inout) :: signxy   !< Current stress xy
          real(kind=WP), dimension(nel), intent(inout) :: signyz   !< Current stress yz
          real(kind=WP), dimension(nel), intent(inout) :: signzx   !< Current stress zx
          real(kind=WP), dimension(nel), intent(inout) :: soundsp  !< Current sound speed
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables
          real(kind=WP), dimension(nel), intent(inout) :: off      !< Element failure flag
          real(kind=WP), dimension(nel), intent(inout) :: pla      !< Accumulated plastic strain
          real(kind=WP), dimension(nel), intent(inout) :: dpla     !< Plastic strain increment
          real(kind=WP), dimension(nel), intent(inout) :: temp     !< Temperature at current time
          integer,                       intent(in)    :: jthe     !< Thermal flag
          integer,                       intent(in)    :: jlag     !< Lagangian flag
          real(kind=WP), dimension(nel), intent(inout) :: fheat    !< Heat energy at current time
          real(kind=WP), dimension(nel), intent(inout) :: et       !< Hourglass stabilization variable
          real(kind=WP), dimension(nel), intent(inout) :: sigy     !< Current yield stress
          integer,                       intent(in)    :: nvartmp  !< Number of temporary variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< Temporary variables
          real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
          real(kind=WP), intent(in)                    :: timestep !< Time step
          real(kind=WP), dimension(nel), intent(inout) :: epsd     !< Plastic strain rate
          integer,                       intent(in)    :: inloc    !< Non-local regularization flag
          real(kind=WP), dimension(nel), intent(inout) :: dplanl   !< Non-local plastic strain increment
          integer,       dimension(nel), intent(in)    :: ngl      !< Global element numbers
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: i,ii,nmax,vp,iter,nindx,indx(nel)
          real(kind = WP) :: young(nel),nu(nel),shear(nel),bulk(nel),a,b,cm,cn,    &
            tmelt,tref,epsm,sigm,cs,cjc,deps0,eta,tol,t0
          real(kind = WP) :: ddep,dfdsig2,tempr,dphi_dlam
          real(kind=WP), dimension(nel) :: pla0,hardp,dlam,normxx,normyy,normzz,   &
            normxy,normyz,normzx,dpxx,dpyy,dpzz,dpxy,dpyz,dpzx,temp0,sxx,syy,szz,  &
            sxy,syz,szx,thsoft,phi,dpla_dlam,eheat,ecool,nutemp,dedt,dnudt,srdep,  &
            hard,off0,young0,nu0,pold,sxx0,syy0,szz0,sxy0,syz0,szx0,devxx,devyy,   &
            devzz,pnew,shear0,bulk0
          real(kind=WP), dimension(nel,1) :: xvec
          logical, dimension(nel) :: converged
!
          !=========================================================================
          !< - Initialisation of computation on time step
          !=========================================================================
!
          !< Integer material parameters
          nmax = matparam%iparam(1)
          vp   = matparam%iparam(2)
!
          !< Recovering real model parameters
          young(1:nel) = matparam%young      !< Young modulus
          nu(1:nel)    = matparam%nu         !< Poisson ratio
          shear(1:nel) = matparam%shear      !< Shear modulus
          bulk(1:nel)  = matparam%bulk       !< Bulk modulus
          a            = matparam%uparam(1)  !< Initial yield stress
          b            = matparam%uparam(2)  !< Hardening modulus
          cn           = matparam%uparam(3)  !< Hardening exponent
          sigm         = matparam%uparam(4)  !< Maximum stress
          cm           = matparam%uparam(5)  !< Temperature exponent
          eta          = matparam%uparam(6)  !< Taylor-Quinney coefficient
          cjc          = matparam%uparam(7)  !< Strain rate coefficient
          deps0        = matparam%uparam(8)  !< Reference strain rate
          epsm         = matparam%uparam(9)  !< Maximum plastic strain
          tol          = matparam%uparam(10) !< Tolerance on return mapping
!
          !< Thermal properties
          cs    = matparam%therm%rhocp
          tref  = matparam%therm%tref
          t0    = matparam%therm%tini
          tmelt = matparam%therm%tmelt
!
          !< Recovering internal variables and initializations of local variables
          do i = 1,nel
            pla0(i)  = pla(i) !< Initial plastic strain
            dpla(i)  = zero   !< Plastic strain increment initialization
            if (vp == 1) epsd(i) = zero !< Plastic strain rate initialization
            et(i)    = one    !< Hourglass stabilization variable initialization
            hardp(i) = zero   !< Hardening modulus initialization
            !< Update failure flag
            off0(i) = off(i)
            if (off(i) < em01) off(i) = zero
            if (off(i) <  one) off(i) = off(i)*four_over_5
            !< Initial temperature
            if (time == zero) then
              if (jthe == 0) temp(i) = t0
              uvar(i,1) = temp(i)
            endif
            temp0(i) = uvar(i,1) !< Recovering previous temperature
          enddo
!
          !< Update the non-local temperature due to plastic dissipation
          if (inloc > 0) then
            do i = 1,nel
              if (off(i) == one) then
                if (jthe /= 0 .and. jlag /= 0) then
                  fheat(i) = fheat(i) + sigy(i)*dplanl(i)*volume(i)
                else if (cs > zero) then
                  temp(i)  = temp(i) + eta*sigy(i)*dplanl(i)/cs
                endif
              endif
            enddo
          endif
!
          !=========================================================================
          !< - Computation of elastic temperature dependent material properties
          !=========================================================================
          !< Interpolation of Young modulus and Poisson ratio
          !  at current temperature for all elements
          xvec(1:nel,1) = temp(1:nel)
          if (matparam%table(1)%notable > 0) then
            call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1,1),          &
              xvec(1,1),eheat,dedt)
          else
            eheat(1:nel) = young(1:nel)
          endif
          if (matparam%table(2)%notable > 0) then
            call table_mat_vinterp(matparam%table(2),nel,nel,vartmp(1,2),          &
              xvec(1,1),ecool,dedt)
          else
            ecool(1:nel) = young(1:nel)
          endif
          if (matparam%table(3)%notable > 0) then
            call table_mat_vinterp(matparam%table(3),nel,nel,vartmp(1,3),          &
              xvec(1,1),nutemp,dnudt)
          else
            nutemp(1:nel) = nu(1:nel)
          endif
          !< Compute the current young modulus and Poisson ratio
          do i = 1,nel
            if (matparam%table(1)%notable > 0) then
              if (matparam%table(2)%notable == 0 .or. temp(i) > temp0(i)) then
                young(i) = eheat(i)
              else if (matparam%table(2)%notable > 0) then
                young(i) = ecool(i)
              endif
            endif
            if (matparam%table(3)%notable > 0) then
              nu(i) = min(nutemp(i),0.495d0)
            endif
            !< Save initial Young modulus
            if (uvar(i,3) == zero) then
              uvar(i,3) = young(i)
            endif
            !< Save initial Poisson ratio
            if (uvar(i,4) == zero) then
              uvar(i,4) = nu(i)
            endif
          enddo
          !< Elastic stiffness constants
          do i = 1,nel
            !< Old elastic constants
            young0(i) = uvar(i,3)
            nu0(i)    = uvar(i,4)
            shear0(i) = young0(i)/(two*(one+nu0(i)))
            bulk0(i)  = young0(i)/(three*(one - two*nu0(i)))
            !< Current elastic constants
            shear(i)  = young(i)/(two*(one+nu(i)))
            bulk(i)   = young(i)/(three*(one - two*nu(i)))
            uvar(i,1) = temp(i)
          enddo
!
          !=========================================================================
          !< - Computation of trial stress tensor and Von Mises stress
          !=========================================================================
          do i = 1,nel
            !< Old hydrostatic stress
            pold(i) = (sigoxx(i) + sigoyy(i) + sigozz(i)) * third
            !< Old deviatoric stress tensor
            sxx0(i) = sigoxx(i) - pold(i)
            syy0(i) = sigoyy(i) - pold(i)
            szz0(i) = sigozz(i) - pold(i)
            sxy0(i) = sigoxy(i)
            syz0(i) = sigoyz(i)
            szx0(i) = sigozx(i)
            !< Deviatoric strain tensor
            devxx(i) = depsxx(i) - third*(depsxx(i)+depsyy(i)+depszz(i))
            devyy(i) = depsyy(i) - third*(depsxx(i)+depsyy(i)+depszz(i))
            devzz(i) = depszz(i) - third*(depsxx(i)+depsyy(i)+depszz(i))
            !< Trial deviatoric stress tensor
            sxx(i) = sxx0(i)*(shear(i)/shear0(i)) + two*shear(i)* devxx(i)
            syy(i) = syy0(i)*(shear(i)/shear0(i)) + two*shear(i)* devyy(i)
            szz(i) = szz0(i)*(shear(i)/shear0(i)) + two*shear(i)* devzz(i)
            sxy(i) = sxy0(i)*(shear(i)/shear0(i)) +     shear(i)*depsxy(i)
            syz(i) = syz0(i)*(shear(i)/shear0(i)) +     shear(i)*depsyz(i)
            szx(i) = szx0(i)*(shear(i)/shear0(i)) +     shear(i)*depszx(i)
            !< Trial pressure
            pnew(i) = pold(i)*(bulk(i)/bulk0(i)) +                                 &
              bulk(i)*(depsxx(i)+depsyy(i)+depszz(i))
            !< Von Mises stress
            seq(i) = sxx(i)**2 + syy(i)**2 + szz(i)**2 +                           &
              two*(sxy(i)**2 + syz(i)**2 + szx(i)**2)
            seq(i) = sqrt(three_half*seq(i))
          enddo
!
          !=========================================================================
          !< - Computation of Johnson-Cook temperature dep. yield stress
          !=========================================================================
          do i = 1,nel
            !< Plastic hardening
            hard(i) = a + b*exp(cn*log(pla(i)+ em20))
            hard(i) = min(sigm,hard(i))
            !< Strain rate dependency
            srdep(i) = one + cjc*log(one + (epsd(i)/deps0))
            !< Thermal softening
            tempr = max(temp(i),tref)
            tempr = min(tempr,tmelt)
            thsoft(i) = (one-((tempr - tref)/(tmelt - tref))**cm)
            !< Assembling the yield stress
            sigy(i) = hard(i)*srdep(i)*thsoft(i)
          enddo
!
          !=========================================================================
          !< - Computation of yield function and check element behavior
          !=========================================================================
          nindx  = 0
          do i=1,nel
            phi(i) = seq(i) - sigy(i)
            if (phi(i) >= zero .and. off(i) == one) then
              nindx = nindx + 1
              indx(nindx) = i
            endif
          enddo
!
          !=========================================================================
          !< - Return mapping procedure (Plastic Correction)
          !=========================================================================
          if (nindx > 0) then
!
            !< Initialisation of the convergence flag
            converged(1:nel) = .false.
!
            !< Loop over the iterations
            do iter = 1, nmax
              !< Loop over yielding elements
              do ii = 1, nindx
                i = indx(ii)
                if (.not.converged(i)) then
!
                  !< Note: in this part, the purpose is to compute for each
                  !  iteration a plastic multiplier allowing to update internal
                  !  variables to satisfy the consistency condition using the
                  !  cutting plane method within an iterative procedure.
                  !  Its expression at each iteration is : dlam = - phi/dphi_dlam
                  ! -> phi       : current value of yield function (known)
                  ! -> dphi_dlam : derivative of f with respect to dlambda by taking
                  !                into account of internal variables kinetic :
                  !                plasticity, damage ... (to be computed)
!
                  !< 1 - Computation of the normal to the yield surface
                  !-----------------------------------------------------------------
                  normxx(i) = three_half*sxx(i)/(max(seq(i),em20))
                  normyy(i) = three_half*syy(i)/(max(seq(i),em20))
                  normzz(i) = three_half*szz(i)/(max(seq(i),em20))
                  normxy(i) =      three*sxy(i)/(max(seq(i),em20))
                  normyz(i) =      three*syz(i)/(max(seq(i),em20))
                  normzx(i) =      three*szx(i)/(max(seq(i),em20))
!
                  !< 2 - Computation of DPHI_DLAMBDA
                  !-----------------------------------------------------------------
!
                  !   a) Derivative with respect stress increments tensor DSIG
                  !   --------------------------------------------------------------
                  dfdsig2 = normxx(i) * normxx(i) * two*shear(i) +                 &
                    normyy(i) * normyy(i) * two*shear(i) +                 &
                    normzz(i) * normzz(i) * two*shear(i) +                 &
                    normxy(i) * normxy(i) *     shear(i) +                 &
                    normyz(i) * normyz(i) *     shear(i) +                 &
                    normzx(i) * normzx(i) *     shear(i)
!
                  !   b) Derivative of dPLA with respect to DLAM
                  !   --------------------------------------------------------------
                  dpla_dlam(i) = seq(i) / max(sigy(i),em20)
!
                  !   c) Derivative of the yield stress with respect to PLA
                  !   --------------------------------------------------------------
                  ! -> Hardening derivarive w.r.t PLA
                  if (sigy(i) < sigm) then
                    hardp(i) = cn*b*exp((cn-one)*log(pla(i)+em20))
                  else
                    hardp(i) = zero
                  endif
                  ! -> Strain rate dependency (if needed)
                  hardp(i) = hardp(i)*srdep(i)
                  ! -> Strain rate dependency derivarive w.r.t PLA (if needed)
                  if (vp == 1) then
                    hardp(i) = hardp(i) + hard(i)*(cjc/timestep)/(deps0 + epsd(i))
                  endif
                  ! -> Add thermal softening effect
                  hardp(i) = hardp(i)*thsoft(i)
!
                  !   d) Assemble the derivation of the yield function w.r.t. lambda
                  !   --------------------------------------------------------------
                  dphi_dlam = - dfdsig2 - hardp(i)*dpla_dlam(i)
                  dphi_dlam = sign(max(abs(dphi_dlam),em20),dphi_dlam)
!
                  !< 3 - Computation of the plastic multiplier
                  !-----------------------------------------------------------------
                  dlam(i) = - phi(i) / dphi_dlam
!
                  !< 4 - Update the plastic strain related variables
                  !-----------------------------------------------------------------
                  !< Plastic strain increment on the iteration
                  ddep    = dpla_dlam(i)*dlam(i)
                  !< Plastic strain increment on the time step
                  dpla(i) = max(dpla(i) + ddep,zero)
                  !< Update the plastic strain
                  pla(i)  = pla0(i) + dpla(i)
                  !< Plastic strain rate (if needed)
                  if (vp == 1) epsd(i) = dpla(i) / timestep
                  !< Plastic strain tensor increment on the iteration
                  dpxx(i) = dlam(i)*normxx(i)
                  dpyy(i) = dlam(i)*normyy(i)
                  dpzz(i) = dlam(i)*normzz(i)
                  dpxy(i) = dlam(i)*normxy(i)
                  dpyz(i) = dlam(i)*normyz(i)
                  dpzx(i) = dlam(i)*normzx(i)
!
                  !< 5 - Update the stress tensor
                  !-----------------------------------------------------------------
                  sxx(i) = sxx(i) - two*shear(i)*dpxx(i)
                  syy(i) = syy(i) - two*shear(i)*dpyy(i)
                  szz(i) = szz(i) - two*shear(i)*dpzz(i)
                  sxy(i) = sxy(i) -     shear(i)*dpxy(i)
                  syz(i) = syz(i) -     shear(i)*dpyz(i)
                  szx(i) = szx(i) -     shear(i)*dpzx(i)
!
                  !< 6 - Recompute the Von Mises stress
                  !-----------------------------------------------------------------
                  seq(i) = sxx(i)**2 + syy(i)**2 + szz(i)**2 +                     &
                    two*(sxy(i)**2 + syz(i)**2 + szx(i)**2)
                  seq(i) = sqrt(three_half*seq(i))
!
                  !< 7 - Update yield stress
                  !-----------------------------------------------------------------
                  ! -> Plastic hardening
                  hard(i) = a + b*exp(cn*log(pla(i)+ em20))
                  hard(i) = min(sigm,hard(i))
                  ! -> Strain rate dependency (if needed)
                  srdep(i) = one + cjc*log(one + (epsd(i)/deps0))
                  ! -> Assembling the yield stress
                  sigy(i) = hard(i)*srdep(i)*thsoft(i)
!
                  !< 8 - Recompute the yield function
                  !-----------------------------------------------------------------
                  phi(i) = seq(i) - sigy(i)
                  if (abs(phi(i)) < tol*max(sigy(i),one)) converged(i) = .true.
!
                endif
              enddo
            enddo
!
            !< Update the local thermal heat due to plastic dissipation
            if (inloc == 0) then
              if (jthe /= 0) then
                do ii = 1, nindx
                  i = indx(ii)
                  fheat(i) = fheat(i) + sigy(i)*dpla(i)*volume(i)
                enddo
              else if (cs > zero) then
                do ii = 1, nindx
                  i = indx(ii)
                  temp(i)  = temp(i) + eta*sigy(i)*dpla(i)/cs
                enddo
              endif
            endif
!
            !< Update the hourglass stabilization variable
            do ii = 1, nindx
              i = indx(ii)
              et(i) = hardp(i) / (hardp(i) + young(i))
            enddo
!
          endif
!
          !< Ductile rupture test
          !-------------------------------------------------------------------------
          do i = 1,nel
            if (off(i) == one .and. pla(i) >= epsm) then
              off(i) = four_over_5
            endif
          enddo
!
          !< Save elastic constant and compute new stress tensor
          !-------------------------------------------------------------------------
          do i = 1,nel
            !< Save elastic constants
            uvar(i,3) = young(i)
            uvar(i,4) = nu(i)
            !< New stress tensor
            signxx(i) = sxx(i) + pnew(i)
            signyy(i) = syy(i) + pnew(i)
            signzz(i) = szz(i) + pnew(i)
            signxy(i) = sxy(i)
            signyz(i) = syz(i)
            signzx(i) = szx(i)
          enddo
!
          !< Update the soundspeed
          !-------------------------------------------------------------------------
          do i = 1,nel
            bulk(i)    = max(bulk(i) ,matparam%bulk)
            shear(i)   = max(shear(i),matparam%shear)
            soundsp(i) = sqrt((bulk(i) + four_over_3*shear(i)) / rho(i))
          enddo
!
          !< Print rupture information
          !-------------------------------------------------------------------------
          do i = 1,nel
            if ((off(i) == zero) .and. (off0(i) > zero) .and. (pla(i) > epsm)) then
              write(iout ,1000) ngl(i),time
              write(istdo,1000) ngl(i),time
            endif
          enddo
!
1000      format(1X,'-- RUPTURE OF SOLID ELEMENT NUMBER ',I10, ' AT TIME :',G11.4)
          !=========================================================================
        end subroutine sigeps106
      end module sigeps106_mod
