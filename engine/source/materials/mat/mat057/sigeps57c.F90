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
      !||    sigeps57c_mod   ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc          ../engine/source/materials/mat_share/mulawc.F90
      !||====================================================================
      module sigeps57c_mod
      contains
      !||====================================================================
      !||    sigeps57c               ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod           ../common_source/modules/precision_mod.F90
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
        subroutine sigeps57c(                                                    &
          nel     ,matparam,rho0    ,time    ,timestep,                          &
          ngl     ,thkly   ,thk     ,soundsp ,pla     ,dpla    ,                 &
          epsxx   ,epsyy   ,epsxy   ,epspxx  ,epspyy  ,epspxy  ,                 &
          depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx  ,                          &
          sigoxx  ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx  ,                          &
          signxx  ,signyy  ,signxy  ,signyz  ,signzx  ,                          &
          off     ,etse    ,yld     ,seq     ,israte  ,asrate  ,                 &
          epsd_pg ,epsd    ,inloc   ,dplanl  ,loff    ,nvartmp ,                 &
          vartmp  ,shf     ,sigb    ,l_dmg   ,dmg     ,                          &
          l_planl ,planl   )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use table_mat_vinterp_mod
          use precision_mod, only : WP
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
          implicit none
#include  "units_c.inc"
!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer, intent(in)                            :: nel      !< Number of elements
          type(matparam_struct_), intent(in)             :: matparam !< Material parameters data structure
          real(kind=WP), dimension(nel), intent(in)            :: rho0     !< Density
          real(kind=WP), intent(in)                            :: time     !< Current time
          real(kind=WP), intent(in)                            :: timestep !< Time step
          integer, dimension(nel), intent(in)            :: ngl      !< Element user ids
          real(kind=WP), dimension(nel), intent(in)            :: thkly    !< Layer thickness
          real(kind=WP), dimension(nel), intent(inout)         :: thk      !< Shell Thickness
          real(kind=WP), dimension(nel), intent(inout)         :: soundsp  !< Sound speed
          real(kind=WP), dimension(nel), intent(inout)         :: pla      !< Plastic strain
          real(kind=WP), dimension(nel), intent(inout)         :: dpla     !< Plastic strain increment
          real(kind=WP), dimension(nel), intent(in)            :: epsxx    !< Strain xx
          real(kind=WP), dimension(nel), intent(in)            :: epsyy    !< Strain yy
          real(kind=WP), dimension(nel), intent(in)            :: epsxy    !< Strain xy
          real(kind=WP), dimension(nel), intent(in)            :: epspxx   !< Strain rate component xx
          real(kind=WP), dimension(nel), intent(in)            :: epspyy   !< Strain rate component yy
          real(kind=WP), dimension(nel), intent(in)            :: epspxy   !< Strain rate component xy
          real(kind=WP), dimension(nel), intent(in)            :: depsxx   !< Strain increment xx
          real(kind=WP), dimension(nel), intent(in)            :: depsyy   !< Strain increment yy
          real(kind=WP), dimension(nel), intent(in)            :: depsxy   !< Strain increment xy
          real(kind=WP), dimension(nel), intent(in)            :: depsyz   !< Strain increment yz
          real(kind=WP), dimension(nel), intent(in)            :: depszx   !< Strain increment zx
          real(kind=WP), dimension(nel), intent(in)            :: sigoxx   !< Old stress xx
          real(kind=WP), dimension(nel), intent(in)            :: sigoyy   !< Old stress yy
          real(kind=WP), dimension(nel), intent(in)            :: sigoxy   !< Old stress xy
          real(kind=WP), dimension(nel), intent(in)            :: sigoyz   !< Old stress yz
          real(kind=WP), dimension(nel), intent(in)            :: sigozx   !< Old stress zx
          real(kind=WP), dimension(nel), intent(inout)         :: signxx   !< New stress xx
          real(kind=WP), dimension(nel), intent(inout)         :: signyy   !< New stress yy
          real(kind=WP), dimension(nel), intent(inout)         :: signxy   !< New stress xy
          real(kind=WP), dimension(nel), intent(inout)         :: signyz   !< New stress yz
          real(kind=WP), dimension(nel), intent(inout)         :: signzx   !< New stress zx
          real(kind=WP), dimension(nel), intent(inout)         :: off      !< Element deletion status
          real(kind=WP), dimension(nel), intent(inout)         :: etse     !< Hourglass control coefficient
          real(kind=WP), dimension(nel), intent(inout)         :: yld      !< Yield stress
          real(kind=WP), dimension(nel), intent(inout)         :: seq      !< Equivalent stress
          integer, intent(in)                                  :: israte   !< Strain rate filtering flag
          real(kind=WP), intent(in)                            :: asrate   !< Strain rate filtering coefficient
          real(kind=WP), dimension(nel), intent(in)            :: epsd_pg  !< global strain rate in Gauss pt
          real(kind=WP), dimension(nel), intent(inout)         :: epsd     !< local strain rate used equations
          integer, intent(in)                            :: inloc    !< Non-local thickness variation flag
          real(kind=WP), dimension(nel), intent(inout)         :: dplanl   !< Non-local plastic strain increment
          real(kind=WP), dimension(nel), intent(in)            :: loff     !< Gauss point deletion status
          integer, intent(in)                            :: nvartmp  !< Number of temporary variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables
          real(kind=WP), dimension(nel), intent(in)            :: shf      !< Transverse shear correction factor
          real(kind=WP), dimension(nel,3), intent(inout)       :: sigb     !< Backstress components
          integer, intent(in)                            :: l_dmg    !< Size of the damage array
          real(kind=WP), dimension(nel,l_dmg), intent(inout)   :: dmg      !< Damage array
          integer, intent(in)                            :: l_planl  !< Size of the non-local plastic strain array
          real(kind=WP), dimension(nel*l_planl), intent(in)    :: planl    !< Non-local plastic strain array
!-------------------------------------------------------------------------------
!  L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i,ii,nindx,indx(nel),ipos(nel,2),opte,vp,iter,nindxf,       &
            indxf(nel)
          real(kind=WP) :: nu,c,a,h,p,m,epsmax,epsr1,epsr2,fisokin,einf,ce
          real(kind=WP) :: young(nel),a11(nel),a12(nel),shear(nel)
          real(kind=WP) :: dseq_dk1,dseq_dk2,dk1_dsigxx,dk1_dsigyy,dk2_dsigxx,         &
            dk2_dsigyy,dk2_dsigxy,dseq_dsigxx,dseq_dsigyy,dseq_dsigxy,           &
            normxx,normyy,normxy,dphi_dseq,dphidsig_dsigdlam,dphi_dyld,dphi_dpla,&
            sig_dphidsig,dpla_dlam,dphi_dlam,dlam,ddep,dpdt,alpha,dsigxx_dlam,   &
            dsigyy_dlam,dsigxy_dlam,dphidsig_dsigbdlam,dphi_dsigbxx,dphi_dsigbyy,&
            dphi_dsigbxy,dsigbxx_dlam,dsigbyy_dlam,dsigbxy_dlam,epst
          real(kind=WP) :: deelzz(nel),deplzz(nel),normsig(nel),xvec(nel,2),yld_i(nel),&
            dyld_dp(nel),dyld_dp_i(nel),hardr(nel),phi(nel),depszz(nel),k1(nel), &
            k2(nel),dyoung_dp(nel),yld0(nel),dyld0_dp(nel),hk(nel)
          !< Number of plastic return mapping iterations
          integer, parameter :: niter = 3
!
          !=====================================================================
          ! - INITIALISATION OF COMPUTATION ON TIME STEP
          !=====================================================================
          !< Recovering integer model parameter
          opte         = matparam%iparam(1)  !< Flag for non-linear Young modulus
          vp           = matparam%iparam(2)  !< Flag for plastic strain rate
          !< Recovering real model parameters
          young(1:nel) = matparam%young      !< Young modulus
          nu           = matparam%nu         !< Poisson ratio
          shear(1:nel) = matparam%shear      !< Shear modulus
          a11(1:nel)   = matparam%uparam(1)  !< First term of stifness matrix
          a12(1:nel)   = matparam%uparam(2)  !< Second term of stifness matrix
          c            = matparam%uparam(3)  !< c Barlat 89 parameter
          a            = matparam%uparam(4)  !< a Barlat 89 parameter
          h            = matparam%uparam(5)  !< h Barlat 89 parameter
          p            = matparam%uparam(6)  !< p Barlat 89 parameter
          m            = matparam%uparam(7)  !< m Barlat 89 exponent
          epsmax       = matparam%uparam(8)  !< Maximum equivalent strain
          epsr1        = matparam%uparam(9)  !< Strain at beginning of softening
          epsr2        = matparam%uparam(10) !< Strain at end of softening
          fisokin      = matparam%uparam(11) !< Kinematic hardening parameter
          einf         = matparam%uparam(12) !< Non-linear Young modulus saturation value
          ce           = matparam%uparam(13) !< Non-linear Young modulus evolution rate
!
          !< Check and update element deletion status
          do i = 1,nel
            if (off(i) < em01) off(i) = zero
            if (off(i) <  one) off(i) = off(i)*four_over_5
          enddo
!
          !< Total strain-rate computation
          if (vp == 0) then
            if (israte == 0) then
              do i = 1,nel
                epsd(i) = half*(abs(epspxx(i)+epspyy(i))                &
                  + sqrt((epspxx(i)-epspyy(i))*(epspxx(i)-epspyy(i))    &
                  + epspxy(i)*epspxy(i)))
              enddo
            else
              epsd(1:nel) = asrate*epsd_pg(1:nel) + (one-asrate)*epsd(1:nel)
            endif
          endif
!
          !< Save the initial yield stress in case of kinematic hardening
          xvec(1:nel,1) = zero
          xvec(1:nel,2) = epsd(1:nel)
          ipos(1:nel,1) = 1
          ipos(1:nel,2) = 1
          call table_mat_vinterp(matparam%table(1),nel,nel,ipos,xvec,yld0,dyld0_dp)
!
          !=======================================================================
          !< - RECOVERING USER VARIABLES AND STATE VARIABLES
          !=======================================================================
          dpla(1:nel)   = zero !< Cumulated plastic strain increment
          deplzz(1:nel) = zero !< Plastic strain increment component zz
          etse(1:nel)   = one  !< Coefficient for hourglass control
!
          !=======================================================================
          !< - ELASTIC PARAMETERS COMPUTATION
          !=======================================================================
          !< Tabulated Young modulus evolution
          if (opte > 0) then
            xvec(1:nel,1) = pla(1:nel)
            ipos(1:nel,1) = vartmp(1:nel,3)
            call table_mat_vinterp(matparam%table(2),nel,nel,ipos,xvec,young,dyoung_dp)
            vartmp(1:nel,3) = ipos(1:nel,1)
            a11(1:nel)   = young(1:nel)/(one - nu*nu)
            a12(1:nel)   = young(1:nel)*nu/(one - nu*nu)
            shear(1:nel) = young(1:nel)/(two*(one + nu))
            !< Non-linear Young modulus evolution
          elseif ((ce > zero).and.(einf > zero)) then
            do i = 1,nel
              young(i) = young(i) - (young(i) - einf)*(one - exp(-ce*pla(i)))
              a11(i)   = young(i)/(one - nu*nu)
              a12(i)   = young(i)*nu/(one - nu*nu)
              shear(i) = young(i)/(two*(one + nu))
            enddo
          endif
!
          !=======================================================================
          !< - COMPUTATION OF TRIAL STRESS TENSOR, VON MISES AND PRESSURE
          !=======================================================================
          !< Trial stress tensor computation
          do i=1,nel
            signxx(i) = sigoxx(i)/max(one-dmg(i,3),em20) +   a11(i)*depsxx(i)    &
              +   a12(i)*depsyy(i)
            signyy(i) = sigoyy(i)/max(one-dmg(i,3),em20) +   a12(i)*depsxx(i)    &
              +   a11(i)*depsyy(i)
            signxy(i) = sigoxy(i)/max(one-dmg(i,3),em20) + shear(i)*depsxy(i)
            signyz(i) = sigoyz(i)/max(one-dmg(i,3),em20) + shear(i)*depsyz(i)*shf(i)
            signzx(i) = sigozx(i)/max(one-dmg(i,3),em20) + shear(i)*depszx(i)*shf(i)
          enddo
          !< Kinematic hardening (if activated)
          do i = 1, nel
            signxx(i) = signxx(i) - sigb(i,1)
            signyy(i) = signyy(i) - sigb(i,2)
            signxy(i) = signxy(i) - sigb(i,3)
          enddo
!
          !=======================================================================
          !< - COMPUTATION OF TRIAL BARLAT 2000 EQUIVALENT STRESS
          !=======================================================================
          do i=1,nel
!
            !< Norm of the stress tensor
            normsig(i) = signxx(i)*signxx(i)                                     &
              + signyy(i)*signyy(i)                                     &
              + two*signxy(i)*signxy(i)
            normsig(i) = sqrt(normsig(i))
            normsig(i) =  max(normsig(i),one)
!
            !< Equivalent stress
            k1(i) = half*(signxx(i) + h*signyy(i))/normsig(i)
            k2(i) = sqrt((half*(signxx(i)-h*signyy(i)))**2 +(p*signxy(i))**2)/   &
              normsig(i)
            seq(i) = a*(abs(k1(i)+k2(i)))**m +                                   &
              a*(abs(k1(i)-k2(i)))**m +                                   &
              c*(abs(two*k2(i)))**m
            if (seq(i) > zero) then
              seq(i) = exp((one/m)*log(half*seq(i)))
            else
              seq(i) = zero
            endif
!
            !< Unnormalized equivalent stress
            seq(i) = seq(i)*normsig(i)
!
          enddo
!
          !=======================================================================
          !< - YIELD STRESS COMPUTATION
          !=======================================================================
          xvec(1:nel,1) = pla(1:nel)
          xvec(1:nel,2) = epsd(1:nel)
          ipos(1:nel,1) = vartmp(1:nel,1)
          ipos(1:nel,2) = vartmp(1:nel,2)
          call table_mat_vinterp(matparam%table(1),nel,nel,ipos,xvec,yld,dyld_dp)
          xvec(1:nel,1:2) = zero
          ipos(1:nel,1:2) = 0
          yld(1:nel) = (one - fisokin)*yld(1:nel) + fisokin*yld0(1:nel)
          hk(1:nel)  = fisokin*dyld_dp(1:nel)
          dyld_dp(1:nel) = (one - fisokin)*dyld_dp(1:nel)
!
          !=======================================================================
          !< - COMPUTATION OF YIELD FUNCTION AND CHECK ELEMENT BEHAVIOR
          !=======================================================================
          nindx  = 0
          nindxf = 0
          do i=1,nel
            phi(i) = (seq(i)/yld(i))**2 - one
            if (phi(i) >= zero .and. off(i) == one) then
              nindx = nindx + 1
              indx(nindx) = i
            endif
          enddo
!
          !=======================================================================
          !< - RETURN MAPPING PROCEDURES (PLASTIC CORRECTION)
          !=======================================================================
          if (nindx > 0) then
!
            !< Loop over the iterations
            do iter = 1, niter
#include "vectorize.inc"
              !< Loop over yielding elements
              do ii = 1, nindx
                i = indx(ii)
!
                ! Note: in this part, the purpose is to compute for each iteration
                ! a plastic multiplier allowing to update internal variables to
                ! satisfy the consistency condition using the cutting plane method
                ! within an iterative procedure.
                ! Its expression at each iteration is : dlam = - phi/dphi_dlam
                ! -> phi       : current value of yield function (known)
                ! -> dphi_dlam : derivative of f with respect to dlambda by taking
                !                into account of internal variables kinetic :
                !                plasticity, damage ... (to be computed)
!
                !< 1 - Derivative of yield criterion w.r.t plastic multiplier
                !      Contribution of the stress tensor
                !-----------------------------------------------------------------
!
                !< Derivative of equivalent stress w.r.t k1 and k2
                dseq_dk1 = ((seq(i)/normsig(i))**(one-m))*(a/two)*(              &
                  sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)         &
                  + sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))
                dseq_dk2 = ((seq(i)/normsig(i))**(one-m))*((a/two)*(             &
                  sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)         &
                  - sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))        &
                  + c*(abs(two*k2(i)))**(m-one))
!
                !< Derivative of k1 w.r.t stress tensor components
                dk1_dsigxx = half
                dk1_dsigyy = h/two
!
                !< Derivative of k2 w.r.t stress tensor components
                dk2_dsigxx = (signxx(i)-h*signyy(i))/                            &
                  (max(normsig(i)*four*k2(i),em20))
                dk2_dsigyy = -h*(signxx(i)-h*signyy(i))/                         &
                  (max(normsig(i)*four*k2(i),em20))
                dk2_dsigxy = (p**2)*signxy(i)/max(normsig(i)*k2(i),em20)
!
                !< Assembling the derivative of the eq. stress w.r.t stress tensor
                dseq_dsigxx = dseq_dk1*dk1_dsigxx + dseq_dk2*dk2_dsigxx
                dseq_dsigyy = dseq_dk1*dk1_dsigyy + dseq_dk2*dk2_dsigyy
                dseq_dsigxy = dseq_dk2*dk2_dsigxy
!
                !< Derivative of the yield function w.r.t equivalent stress
                dphi_dseq = two*(seq(i)/(yld(i)**2))
!
                !< Derivative of the yield function w.r.t stress tensor components
                normxx = dphi_dseq*dseq_dsigxx
                normyy = dphi_dseq*dseq_dsigyy
                normxy = dphi_dseq*dseq_dsigxy
!
                !< Derivative of stress tensor w.r.t plastic multiplier
                dsigxx_dlam =   -a11(i)*normxx - a12(i)*normyy
                dsigyy_dlam =   -a11(i)*normyy - a12(i)*normxx
                dsigxy_dlam = -shear(i)*normxy
!
                !< Contribution of strs tensor to derivative w.r.t pl. multiplier
                dphidsig_dsigdlam = normxx*dsigxx_dlam +                         &
                  normyy*dsigyy_dlam +                         &
                  normxy*dsigxy_dlam
!
                !< 2 - Derivative of yield criterion w.r.t plastic multiplier
                !      Contribution of the plastic strain
                !-----------------------------------------------------------------
                !< Derivative of yield function w.r.t yield stress
                dphi_dyld = -two*(seq(i)**2/(yld(i)**3))
!
                !< Derivative of yield function w.r.t plastic strain
                dphi_dpla = dphi_dyld*dyld_dp(i)
!
                !< Derivative of plastic strain w.r.t plastic multiplier
                sig_dphidsig = signxx(i)*normxx +                                &
                  signyy(i)*normyy +                                &
                  signxy(i)*normxy
                dpla_dlam = sig_dphidsig/yld(i)
!
                !< 3 - Derivative of yield criterion w.r.t plastic multiplier
                !      Contribution of the kinematic hardening
                !---------------------------------------------------------------
                !< Derivative of yield function w.r.t backstress tensor
                dphi_dsigbxx = -normxx
                dphi_dsigbyy = -normyy
                dphi_dsigbxy = -normxy
!
                !< Derivative of backstress tensor w.r.t plastic multiplier
                dsigbxx_dlam = two_third*hk(i)*(two*normxx + normyy)
                dsigbyy_dlam = two_third*hk(i)*(two*normyy + normxx)
                dsigbxy_dlam = two_third*hk(i)*normxy
!
                !< Contribution of backstrs tensor to derivative w.r.t pl. mult.
                dphidsig_dsigbdlam = dphi_dsigbxx*dsigbxx_dlam +                 &
                  dphi_dsigbyy*dsigbyy_dlam +                 &
                  dphi_dsigbxy*dsigbxy_dlam
!
                !< 3 - Derivative of yield criterion w.r.t plastic multiplier
                !-----------------------------------------------------------------
                !< Computation of the plastic multiplier
                dphi_dlam = dphidsig_dsigdlam   +                                &
                  dphi_dpla*dpla_dlam +                                &
                  dphidsig_dsigbdlam
                dphi_dlam = sign(max(abs(dphi_dlam),em20),dphi_dlam)
!
                !< 4 - Computation of plastic multiplier
                !-----------------------------------------------------------------
                dlam = -phi(i)/dphi_dlam
!
                !< 5 - Update plastic strain related variables
                !------------------------------------------------------------------
                !< Cumulated plastic strain update
                ddep    = dpla_dlam*dlam
                dpla(i) = max(zero, dpla(i) + ddep)
                pla(i)  = pla(i) + ddep
!
                !< Out-of-plane plastic strain increment
                deplzz(i) = deplzz(i) - dlam*normxx - dlam*normyy
!
                !< 6 - Update stress tensor and related variable
                !------------------------------------------------------------------
                signxx(i) = signxx(i) + sigb(i,1)
                signyy(i) = signyy(i) + sigb(i,2)
                signxy(i) = signxy(i) + sigb(i,3)
!
                !< Update of the stress tensor
                signxx(i) = signxx(i) + dsigxx_dlam*dlam
                signyy(i) = signyy(i) + dsigyy_dlam*dlam
                signxy(i) = signxy(i) + dsigxy_dlam*dlam
!
                !< Update of the backstress tensor
                sigb(i,1) = sigb(i,1) + dsigbxx_dlam*dlam
                sigb(i,2) = sigb(i,2) + dsigbyy_dlam*dlam
                sigb(i,3) = sigb(i,3) + dsigbxy_dlam*dlam
!
                !< Retrieve shifted stress tensor
                signxx(i) = signxx(i) - sigb(i,1)
                signyy(i) = signyy(i) - sigb(i,2)
                signxy(i) = signxy(i) - sigb(i,3)
!
                !< Norm of the stress tensor
                normsig(i) = signxx(i)*signxx(i)                                 &
                  + signyy(i)*signyy(i)                                 &
                  + two*signxy(i)*signxy(i)
                normsig(i) = sqrt(normsig(i))
                normsig(i) = max(normsig(i),one)
!
                !< Update equivalent stress
                k1(i) = half*(signxx(i)+h*signyy(i))/normsig(i)
                k2(i) = sqrt((half*(signxx(i)-h*signyy(i)))**2                   &
                  +(p*signxy(i))**2)/normsig(i)
                seq(i) = a*(abs(k1(i)+k2(i)))**m +                               &
                  a*(abs(k1(i)-k2(i)))**m +                               &
                  c*(abs(two*k2(i)))**m
                if (seq(i) > zero) then
                  seq(i) = exp((one/m)*log(half*seq(i)))
                else
                  seq(i) = zero
                endif
                seq(i) = seq(i)*normsig(i)
!
                !< Save variables for yield stress in plastic index order
                xvec(ii,1) = pla(i)
                xvec(ii,2) = epsd(i)
                ipos(ii,1) = vartmp(i,1)
                ipos(ii,2) = vartmp(i,2)
              enddo
!
              !< Update of the yield stress
              call table_mat_vinterp(matparam%table(1),nindx,nindx,ipos,xvec,    &
                yld_i,dyld_dp_i)
!
#include "vectorize.inc"
              !< Loop over yielding elements
              do ii = 1, nindx
                i = indx(ii)
!
                !< Reverse plastic index order
                vartmp(i,1) = ipos(ii,1)
                vartmp(i,2) = ipos(ii,2)
                yld(i)      = yld_i(ii)
                dyld_dp(i)  = dyld_dp_i(ii)
!
                !< Apply kinematic hardening coefficient
                yld(i) = (one - fisokin)*yld(i) + fisokin*yld0(i)
                hk(i) = fisokin*dyld_dp(i)
                dyld_dp(i) = (one - fisokin)*dyld_dp(i)
!
                !< Compute the new yield function
                phi(i) = (seq(i)/yld(i))**2 - one
!
              enddo
              !< End of the loop over yielding elements
            enddo
            !< End of the loop over the iterations
!
#include "vectorize.inc"
            !< Update the coefficient for hourglass control & kinematic hardening
            do ii = 1,nindx
              i = indx(ii)
              !< Hourglass stiffness parameter
              etse(i)  = (dyld_dp(i)+hk(i)) / ((dyld_dp(i)+hk(i)) + young(i))
              dmg(i,2) = pla(i)/epsmax
              dmg(i,2) = min(one,dmg(i,2))
              if (pla(i) > epsmax .and. off(i) == one .and. inloc == 0) then
                off(i) = four_over_5
                nindxf = nindxf + 1
                indxf(nindxf) = i
              endif
            enddo
          endif
          !=======================================================================
          !< - END OF PLASTIC RETURN MAPPING PROCEDURE
          !=======================================================================
!
          !< Plastic strain rate if activated
          if (vp == 1) then
            do i = 1,nel
              dpdt    = dpla(i)/max(timestep,em20)
              epsd(i) = asrate*dpdt + (one - asrate)*epsd(i)
            enddo
          endif
!
          !< Remove backstress contribution to the stress tensor
          do i = 1,nel
            signxx(i) = signxx(i) + sigb(i,1)
            signyy(i) = signyy(i) + sigb(i,2)
            signxy(i) = signxy(i) + sigb(i,3)
          enddo
!
          !< Damage softening activated
          if ((epsr1 > zero).and.(epsr2 > zero)) then
            do i = 1,nel
              !< Compute the maximum principal strain
              epst = half*(epsxx(i)+epsyy(i)                                     &
                + sqrt((epsxx(i)-epsyy(i))*(epsxx(i)-epsyy(i))                &
                + epsxy(i)*epsxy(i)))
              !< Damage parameter
              dmg(i,3) = max(dmg(i,3),one-(epsr2-epst)/(epsr2-epsr1))
              dmg(i,3) = min(one,dmg(i,3))
            enddo
          endif
!
          !< Add damage stoftenin to the stress tensor
          do i = 1,nel
            signxx(i) = (one - dmg(i,3))*signxx(i)
            signyy(i) = (one - dmg(i,3))*signyy(i)
            signxy(i) = (one - dmg(i,3))*signxy(i)
            signyz(i) = (one - dmg(i,3))*signyz(i)
            signzx(i) = (one - dmg(i,3))*signzx(i)
          enddo
!
          !< Non-local variable update if needed
          if (inloc > 0) then
            do i = 1,nel
              if (loff(i) == one) then
                !< Non-local plastic strain failure
                dmg(i,2) = planl(i)/epsmax
                dmg(i,2) = min(one,dmg(i,2))
                !< Non-local thickness variation
                if (seq(i) > zero) then
                  dseq_dk1 = ((seq(i)/normsig(i))**(one-m))*(a/two)*(            &
                    sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)       &
                    + sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))
                  dseq_dk2 = ((seq(i)/normsig(i))**(one-m))*((a/two)*(           &
                    sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)       &
                    - sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))      &
                    + c*(abs(two*k2(i)))**(m-one))
                  dk1_dsigxx   = half
                  dk1_dsigyy   = h/two
                  dk2_dsigxx   = (signxx(i)-h*signyy(i))/                        &
                    (max(normsig(i)*four*k2(i),em20))
                  dk2_dsigyy   = -h*(signxx(i)-h*signyy(i))/                     &
                    (max(normsig(i)*four*k2(i),em20))
                  dk2_dsigxy   = (p**2)*signxy(i)/max(normsig(i)*k2(i),em20)
                  dseq_dsigxx  = dseq_dk1*dk1_dsigxx + dseq_dk2*dk2_dsigxx
                  dseq_dsigyy  = dseq_dk1*dk1_dsigyy + dseq_dk2*dk2_dsigyy
                  dseq_dsigxy  = dseq_dk2*dk2_dsigxy
                  sig_dphidsig = dseq_dsigxx*signxx(i) +                         &
                    dseq_dsigyy*signyy(i) +                         &
                    dseq_dsigxy*signxy(i)
                else
                  sig_dphidsig = zero
                endif
                if (sig_dphidsig /= zero) then
                  deplzz(i) = - dplanl(i)*(seq(i)/sig_dphidsig)*(dseq_dsigxx     &
                    + dseq_dsigyy)
                else
                  deplzz(i) = zero
                endif
              endif
            enddo
          endif
!
          !< Update the user variable, soundspeed and thickness
          do i=1,nel
            !< Elastic strain increment in the z direction
            deelzz(i) = -nu*(signxx(i)-sigoxx(i)+signyy(i)-sigoyy(i))/young(i)
            !< Assembling total strain increment in the z direction
            depszz(i) = deelzz(i) + deplzz(i)
            !< Update of the thickness
            thk(i) = thk(i) + depszz(i)*thkly(i)*off(i)
            !< Update of the soundspeed
            soundsp(i) = sqrt(a11(i)/rho0(i))
            !< Global damage output
            dmg(i,1) = max(dmg(i,2),dmg(i,3))
          enddo
!
          !=======================================================================
          !< - PRINTING OUT ELEMENT DELETION FAILURE
          !=======================================================================
          if (nindxf > 0) then
            do ii = 1,nindxf
              write(iout ,1000) ngl(indxf(ii))
              write(istdo,1100) ngl(indxf(ii)),time
            enddo
          endif
!
1000      format(1X,'FAILURE (BARLAT3) OF SHELL ELEMENT ',I10)
1100      format(1X,'FAILURE (BARLAT3) OF SHELL ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
!
        end subroutine sigeps57c
      end module sigeps57c_mod
