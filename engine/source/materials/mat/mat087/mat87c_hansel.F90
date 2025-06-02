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
      !||    mat87c_hansel_mod   ../engine/source/materials/mat/mat087/mat87c_hansel.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps87c           ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||====================================================================
      module mat87c_hansel_mod
      contains
      !||====================================================================
      !||    mat87c_hansel      ../engine/source/materials/mat/mat087/mat87c_hansel.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps87c          ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod      ../common_source/modules/precision_mod.F90
      !||====================================================================
      subroutine mat87c_hansel(                                                &
        nel    ,matparam,nuvar   ,uvar    ,                                    &
        rho0   ,thkly   ,thk     ,epsp    ,time     ,                          &
        temp   ,jthe    ,                                                      &
        depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                          &
        sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                          &
        signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                          &
        soundsp,pla     ,dpla    ,epsd    ,yld      ,                          &
        etse   ,gs      ,off     ,                                             &
        l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                          &
        loff   )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use matparam_def_mod
      use constant_mod
      use precision_mod, only : WP
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y  A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)                            :: nel      !< Number of elements
      type(matparam_struct_), intent(in)             :: matparam !< Material parameters
      integer, intent(in)                            :: nuvar    !< Number of user variables
      real(kind=WP), dimension(nel,nuvar), intent(inout)   :: uvar     !< User variables
      real(kind=WP), dimension(nel), intent(in)            :: rho0     !< Density
      real(kind=WP), dimension(nel), intent(in)            :: thkly    !< Layer thickness
      real(kind=WP), dimension(nel), intent(inout)         :: thk      !< Thickness
      real(kind=WP), dimension(nel), intent(in)            :: epsp     !< Equivalent and filtered total strain rate
      real(kind=WP), intent(in)                            :: time     !< Current time
      real(kind=WP), dimension(nel), intent(inout)         :: temp     !< Element or Gauss point Temperature
      integer, intent(in)                            :: jthe     !< Flag for thermal computation
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
      real(kind=WP), dimension(nel), intent(inout)         :: pla      !< Plastic strain
      real(kind=WP), dimension(nel), intent(inout)         :: dpla     !< Plastic strain increment
      real(kind=WP), dimension(nel), intent(inout)         :: epsd     !< Output strain rate
      real(kind=WP), dimension(nel), intent(inout)         :: yld      !< Yield stress
      real(kind=WP), dimension(nel), intent(inout)         :: etse     !< Coefficient for hourglass control
      real(kind=WP), dimension(nel), intent(in)            :: gs       !< Transverse shear modulus
      real(kind=WP), dimension(nel), intent(in)            :: off      !< Flag for element deletion status
      integer, intent(in)                            :: l_sigb   !< Size of the backstress tensor
      real(kind=WP), dimension(nel,l_sigb), intent(inout)  :: sigb     !< Backstress tensor
      integer, intent(in)                            :: inloc    !< Flag for non-local regularisation
      real(kind=WP), dimension(nel), intent(in)            :: dplanl   !< Non-local plastic strain increment
      real(kind=WP), dimension(nel), intent(inout)         :: seq      !< Equivalent stress
      real(kind=WP), dimension(nel), intent(in)            :: loff     !< Flag for layer deletion status
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,j,k,nindx,indx(nel),iter,iflagsr,ikin
      real(kind=WP) ::                                                               &
        young,nu,a1,a2,g,al1,al2,al3,al4,al5,al6,al7,al8,fisokin,expa,ckh(4),  &
        akh(4),lp11,lp12,lp21,lp22,lp66,lpp11,lpp12,lpp21,lpp22,lpp66,akck,    &
        ahs,bhs,mhs,eps0hs,nhs,hmart,temp0,eta,cp,am,bm,cm,dm,ppm,qm,e0mart,   &
        vm0,k1,k2
      real(kind=WP) ::                                                               &
        mohr_radius,mohr_center,dxp1dxpxx,dxp2dxpxx,dxp1dxpyy,                 &
        dxp2dxpyy,dxp1dxpxy,dxp2dxpxy,dxpp1dxppxx,dxpp2dxppxx,dxpp1dxppyy,     &
        dxpp2dxppyy,dxpp1dxppxy,dxpp2dxppxy,dxp1dsigxx,dxp1dsigyy,dxp1dsigxy,  &
        dxp2dsigxx,dxp2dsigyy,dxp2dsigxy,dxpp1dsigxx,dxpp1dsigyy,dxpp1dsigxy,  &
        dxpp2dsigxx,dxpp2dsigyy,dxpp2dsigxy,dphipdxp1,dphipdxp2,dphippdxpp1,   &
        dphippdxpp2,dphipdsigxx,dphipdsigyy,dphipdsigxy,dphippdsigxx,          &
        dphippdsigyy,dphippdsigxy,dseqdphip,dseqdphipp,dseqdsigxx,dseqdsigyy,  &
        dseqdsigxy,normxx,normyy,normxy,dsigxxdlam,dsigyydlam,dsigxydlam,      &
        dphidsig_dsigdlam,dphidpla,sig_dphidsig,dphidlam,dlam,                 &
        ddep,dpladlam,dphidyld,dphidseq,sig1,sig2,dpdt,                        &
        dphidsigb_dsigbdlam,dsigbxxdlam,dsigbyydlam,dsigbxydlam,dvmdpla
      real(kind=WP) ::                                                               &
        deplzz(nel),sigbxx(nel),sigbyy(nel),sigbxy(nel),normsig(nel),          &
        xp1(nel),xp2(nel),xpp1(nel),xpp2(nel),phip(nel),phipp(nel),phi(nel),   &
        deelzz(nel),dylddp(nel),xpxx(nel),xpyy(nel),xpxy(nel),xppxx(nel),      &
        xppyy(nel),xppxy(nel),depszz(nel),yld0(nel),dyld0dp(nel),voce(nel),    &
        dvocedp(nel),swift(nel),dswiftdp(nel),frate(nel),vm(nel),expo(nel),    &
        dexpo(nel),aexp(nel),atemp(nel),dsigbxxdp(nel),dsigbyydp(nel),         &
        dsigbxydp(nel),hk(nel)
      integer, parameter :: niter = 3 !< Number of return mapping iterations
!===============================================================================
!
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Recovering integer model parameter
      ikin     = matparam%iparam(4)  !< Flag for kinematic hardening formulation
      !< Recovering real model parameters
      young    = matparam%young      !< Young modulus
      nu       = matparam%nu         !< Poisson ratio
      a1       = matparam%uparam(1)  !< First term of elastic stiffness matrix
      a2       = matparam%uparam(2)  !< Second term of elastic stiffness matrix
      g        = matparam%shear      !< Shear modulus
      al1      = matparam%uparam(3)  !< First linear projection parameter
      al2      = matparam%uparam(4)  !< Second linear projection parameter
      al3      = matparam%uparam(5)  !< Third linear projection parameter
      al4      = matparam%uparam(6)  !< Fourth linear projection parameter
      al5      = matparam%uparam(7)  !< Fifth linear projection parameter
      al6      = matparam%uparam(8)  !< Sixth linear projection parameter
      al7      = matparam%uparam(9)  !< Seventh linear projection parameter
      al8      = matparam%uparam(10) !< Eighth linear projection parameter
      fisokin  = matparam%uparam(11) !< Kinematic hardening flag
      expa     = matparam%uparam(12) !< Exponent of the yield function
      k1       = matparam%uparam(13) !< Hansel first temperature dependency parameter 
      k2       = matparam%uparam(14) !< Hansel second temperature dependency parameter
      ahs      = matparam%uparam(15) !< Hansel first isotropic hardening parameter
      bhs      = matparam%uparam(16) !< Hansel second isotropic hardening parameter
      mhs      = matparam%uparam(17) !< Hansel isotropic hardening parameter rate
      eps0hs   = matparam%uparam(18) !< Hansel initial plastic strain
      nhs      = matparam%uparam(19) !< Hansel exponent for isotropic hardening
      hmart    = matparam%uparam(20) !< Hansel martensite dependent hardening parameter
      temp0    = matparam%uparam(21) !< Initial temperature
      eta      = matparam%uparam(23) !< Taylor-Quinney coefficient
      cp       = matparam%uparam(24) !< Thermal mass capacity
      am       = matparam%uparam(25) !< Martensite volume fraction evolution first parameter
      bm       = matparam%uparam(26) !< Martensite volume fraction evolution second parameter
      cm       = matparam%uparam(27) !< Martensite volume fraction evolution third parameter
      dm       = matparam%uparam(28) !< Martensite volume fraction evolution fourth parameter
      ppm      = matparam%uparam(29) !< 
      qm       = matparam%uparam(30) !< 
      e0mart   = matparam%uparam(31) !< 
      vm0      = matparam%uparam(32) !< Initial volume fraction of martensite
      !< Kinematic hardening parameters
      if ((ikin == 1).and.(fisokin > zero)) then                  
        ckh(1) = matparam%uparam(33) 
        akh(1) = matparam%uparam(34) 
        ckh(2) = matparam%uparam(35) 
        akh(2) = matparam%uparam(36) 
        ckh(3) = matparam%uparam(37) 
        akh(3) = matparam%uparam(38) 
        ckh(4) = matparam%uparam(39) 
        akh(4) = matparam%uparam(40) 
        akck   = akh(1)*ckh(1) + akh(2)*ckh(2) + akh(3)*ckh(3) + akh(4)*ckh(4)
      endif
!
      !< Barlat linear projection parameters
      !< - For xprime tensor
      lp11  = two*al1/three
      lp12  = -al1/three
      lp21  = -al2/three
      lp22  = two*al2/three
      lp66  = al7
      !< - For xprimeprime tensor
      lpp11 = (-two*al3 +   two*al4 + eight*al5 -  two*al6)/nine
      lpp12 = (     al3 -  four*al4 -  four*al5 + four*al6)/nine
      lpp21 = (four*al3 -  four*al4 -  four*al5 +      al6)/nine
      lpp22 = (-two*al3 + eight*al4 +   two*al5 -  two*al6)/nine
      lpp66 =  al8
!
      !< Initialization of martensite volume fraction
      if (uvar(1,1) == zero) then
        uvar(1:nel,1) = vm0
      endif
      !< Initialization of temperature
      if ((time == zero).and.(jthe == 0)) then
        temp(1:nel) = temp0
      endif 
!
      !=========================================================================
      !< - RECOVERING USER VARIABLES AND STATE VARIABLES
      !=========================================================================
      dpla(1:nel)   = zero !< Cumulated plastic strain increment
      deplzz(1:nel) = zero !< Plastic strain increment component zz
      etse(1:nel)   = one  !< Coefficient for hourglass control
      vm(1:nel)     = uvar(1:nel,1) !< Martensite volume fraction
!
      !=========================================================================
      !< - COMPUTATION OF TRIAL STRESS TENSOR, VON MISES AND PRESSURE
      !=========================================================================
      !< Trial stress tensor computation
      do i=1,nel
        signxx(i) = sigoxx(i) +    a1*depsxx(i) + a2*depsyy(i)
        signyy(i) = sigoyy(i) +    a2*depsxx(i) + a1*depsyy(i)
        signxy(i) = sigoxy(i) +     g*depsxy(i)               
        signyz(i) = sigoyz(i) + gs(i)*depsyz(i)
        signzx(i) = sigozx(i) + gs(i)*depszx(i)
      enddo
      !< Backstress tensor computation
      if (fisokin > zero) then 
        do i=1,nel
          sigbxx(i) = zero
          sigbyy(i) = zero
          sigbxy(i) = zero
          !< Compute the backstress tensor from all C-R kinematic hardenings
          do j = 1, l_sigb/3
            sigbxx(i) = sigbxx(i) + sigb(i,3*(j-1) + 1)
            sigbyy(i) = sigbyy(i) + sigb(i,3*(j-1) + 2)
            sigbxy(i) = sigbxy(i) + sigb(i,3*(j-1) + 3)
          enddo
          !< Add the kinematic hardening contribution to stress tensor
          signxx(i) = signxx(i) - sigbxx(i)
          signyy(i) = signyy(i) - sigbyy(i)
          signxy(i) = signxy(i) - sigbxy(i)
        enddo
      endif
!
      !=========================================================================
      !< - COMPUTATION OF TRIAL BARLAT 2000 EQUIVALENT STRESS
      !=========================================================================      
      do i=1,nel
!
        !< Norm of the stress tensor
        normsig(i) = signxx(i)*signxx(i)                                       & 
                   + signyy(i)*signyy(i)                                       &
               + two*signxy(i)*signxy(i)
        normsig(i) = sqrt(normsig(i))
        normsig(i) =  max(normsig(i),one)
!
        !< Computation of the xprime and xprimeprime tensors
        xpxx(i)  = (lp11*signxx(i) + lp12*signyy(i))/normsig(i)
        xpyy(i)  = (lp21*signxx(i) + lp22*signyy(i))/normsig(i)
        xpxy(i)  =  lp66*signxy(i)/normsig(i)
!       
        xppxx(i) = (lpp11*signxx(i) + lpp12*signyy(i))/normsig(i)
        xppyy(i) = (lpp21*signxx(i) + lpp22*signyy(i))/normsig(i)
        xppxy(i) =  lpp66*signxy(i)/normsig(i)
!       
        !< Computation of the xprime and xprimeprime principal stresses
        mohr_center = (xpxx(i)+xpyy(i))/two
        mohr_radius = sqrt(((xpxx(i)-xpyy(i))/two)**2 + xpxy(i)**2)
        xp1(i) = mohr_center + mohr_radius
        xp2(i) = mohr_center - mohr_radius
!   
        mohr_center = (xppxx(i)+xppyy(i))/two
        mohr_radius = sqrt(((xppxx(i)-xppyy(i))/two)**2 + xppxy(i)**2)
        xpp1(i) = mohr_center + mohr_radius
        xpp2(i) = mohr_center - mohr_radius
!     
        !< Computation of the phiprime and phiprimeprime functions
        phip(i)  = (abs(xp1(i) - xp2(i)))**expa
        phipp(i) = (abs(two*xpp2(i) + xpp1(i)))**expa +                        &
                   (abs(two*xpp1(i) + xpp2(i)))**expa
!
        !< Equivalent stress
        seq(i) = half*(phip(i)+phipp(i))     
        if (seq(i) > zero) then
          seq(i) = exp((one/expa)*log(seq(i)))
        else
          seq(i) = zero
        endif
        seq(i) = seq(i)*normsig(i)
!
      enddo
!
      !=========================================================================
      !< - YIELD STRESS COMPUTATION
      !=========================================================================
      do i = 1,nel
        !< Initial yield stress (for kinematic hardening)
        if (eps0hs > zero) then
          expo(i)  = exp(nhs*log(eps0hs))
        else
          expo(i)  = zero
        endif
        aexp(i)  = (bhs - ahs)*exp(-mhs*expo(i))
        atemp(i) = (k1 + k2*temp(i))
        yld0(i)  = (bhs-aexp(i))*atemp(i) + hmart*vm(i)
        !< Current yield stress
        if (pla(i) + eps0hs > zero) then 
          expo(i)  = exp(nhs*log(pla(i) + eps0hs))
          dexpo(i) = nhs*exp((nhs-one)*log(pla(i) + eps0hs))
        else
          expo(i)  = zero
          dexpo(i) = zero
        endif
        aexp(i)   = (bhs - ahs)*exp(-mhs*expo(i))
        yld(i)    = (bhs-aexp(i))*atemp(i) + hmart*vm(i)
        dylddp(i) = mhs*dexpo(i)*aexp(i)*atemp(i)
        yld(i)    = (one - fisokin)*yld(i) + fisokin*yld0(i)
        hk(i)     = fisokin*dylddp(i)
        dylddp(i) = (one - fisokin)*dylddp(i)
      enddo
!
      !=========================================================================
      !< - COMPUTATION OF YIELD FUNCTION AND CHECK ELEMENT BEHAVIOR
      !=========================================================================
      nindx  = 0
      do i=1,nel
        phi(i) = (seq(i)/yld(i))**2 - one
        if (phi(i) >= zero .and. off(i) == one) then
          nindx = nindx + 1
          indx(nindx)  = i 
        endif
      enddo
!
      !=========================================================================
      !< - RETURN MAPPING PROCEDURES (PLASTIC CORRECTION)
      !=========================================================================
      if (nindx > 0) then 
!
        !< Computation of the derivative of backstress tensor w.r.t pl. strain
        if ((ikin == 1).and.(fisokin > zero)) then
#include "vectorize.inc" 
          do ii = 1, nindx
            i = indx(ii)
            dsigbxxdp(i) = ckh(1)*sigb(i,1) + ckh(2)*sigb(i,4) +               &
                           ckh(3)*sigb(i,7) + ckh(4)*sigb(i,10)
            dsigbyydp(i) = ckh(1)*sigb(i,2) + ckh(2)*sigb(i,5) +               &
                           ckh(3)*sigb(i,8) + ckh(4)*sigb(i,11)
            dsigbxydp(i) = ckh(1)*sigb(i,3) + ckh(2)*sigb(i,6) +               &
                           ckh(3)*sigb(i,9) + ckh(4)*sigb(i,12)
          enddo
        endif
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
            ! Its expression at each iteration is : dlambda = - f/df_dlambda
            ! -> phi       : current value of yield function (known)
            ! -> dphi_dlam : derivative of f with respect to dlambda by taking
            !                into account of internal variables kinetic :
            !                plasticity, damage ... (to be computed)    
!
            !< 1 - Derivative of yield criterion w.r.t plastic multiplier
            !      Contribution of the stress tensor
            !-------------------------------------------------------------------
            !< Derivative of xprime 1 w.r.t xprime tensor
            mohr_radius = sqrt(((xpxx(i)-xpyy(i))/two)**2 + xpxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            dxp1dxpxx = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpyy = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpxy = xpxy(i)/mohr_radius
!
            !< Derivative of xprime 2 w.r.t xprime tensor
            dxp2dxpxx = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpyy = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpxy = -xpxy(i)/mohr_radius
!
            !< Derivative of xprimeprime 1 w.r.t xprimeprime tensor
            mohr_radius = sqrt(((xppxx(i)-xppyy(i))/two)**2 + xppxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            dxpp1dxppxx = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppyy = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppxy = xppxy(i)/mohr_radius
!
            !< Derivative of xprimeprime 2 w.r.t xprimeprime tensor
            dxpp2dxppxx = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppyy = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppxy = -xppxy(i)/mohr_radius
!
            !< Assembling derivative of xprime 1 w.r.t stress tensor
            dxp1dsigxx = dxp1dxpxx*lp11 + dxp1dxpyy*lp21          
            dxp1dsigyy = dxp1dxpxx*lp12 + dxp1dxpyy*lp22
            dxp1dsigxy = dxp1dxpxy*lp66
!
            !< Assembling derivative of xprime 2 w.r.t stress tensor
            dxp2dsigxx = dxp2dxpxx*lp11 + dxp2dxpyy*lp21
            dxp2dsigyy = dxp2dxpxx*lp12 + dxp2dxpyy*lp22
            dxp2dsigxy = dxp2dxpxy*lp66
!
            !< Assembling derivative of xprimeprime 1 w.r.t stress tensor
            dxpp1dsigxx = dxpp1dxppxx*lpp11 + dxpp1dxppyy*lpp21 
            dxpp1dsigyy = dxpp1dxppxx*lpp12 + dxpp1dxppyy*lpp22
            dxpp1dsigxy = dxpp1dxppxy*lpp66
!
            !< Assembling derivative of xprimeprime 2 w.r.t stress tensor
            dxpp2dsigxx = dxpp2dxppxx*lpp11 + dxpp2dxppyy*lpp21
            dxpp2dsigyy = dxpp2dxppxx*lpp12 + dxpp2dxppyy*lpp22
            dxpp2dsigxy = dxpp2dxppxy*lpp66
!
            !< Derivative of phiprime w.r.t xprime 1
            dphipdxp1 = expa*(abs(xp1(i)-xp2(i)))**(expa-1)*sign(one,xp1(i)-xp2(i))
            !< Derivative of phiprime w.r.t xprime 2
            dphipdxp2 = -dphipdxp1
!
            !< Derivative of phiprimeprime w.r.t xprimeprime 1
            dphippdxpp1 = expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*           &
                                           sign(one,two*xpp2(i)+xpp1(i)) +     &
                      two*expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*           &
                                           sign(one,two*xpp1(i)+xpp2(i))
            !< Derivative of phiprimeprime w.r.t xprimeprime 2
            dphippdxpp2 = expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*           &
                                           sign(one,two*xpp1(i)+xpp2(i)) +     &
                      two*expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*           &
                                           sign(one,two*xpp2(i)+xpp1(i))
!
            !< Assembling derivative of phiprime w.r.t stress tensor
            dphipdsigxx = dphipdxp1*dxp1dsigxx + dphipdxp2*dxp2dsigxx
            dphipdsigyy = dphipdxp1*dxp1dsigyy + dphipdxp2*dxp2dsigyy
            dphipdsigxy = dphipdxp1*dxp1dsigxy + dphipdxp2*dxp2dsigxy   
!
            !< Assembling derivative of phiprimeprime w.r.t stress tensor
            dphippdsigxx = dphippdxpp1*dxpp1dsigxx + dphippdxpp2*dxpp2dsigxx
            dphippdsigyy = dphippdxpp1*dxpp1dsigyy + dphippdxpp2*dxpp2dsigyy
            dphippdsigxy = dphippdxpp1*dxpp1dsigxy + dphippdxpp2*dxpp2dsigxy
!
            !< Derivative of equivalent stress w.r.t phiprime
            dseqdphip  =                                                       &
               (half/expa)*exp((one/expa - one)*log(half*(phip(i)+phipp(i))))
            !< Derivative of equivalent stress w.r.t phiprimeprime
            dseqdphipp = dseqdphip
!
            !< Assembling derivative of equivalent stress w.r.t stress tensor
            dseqdsigxx = dseqdphip*dphipdsigxx + dseqdphipp*dphippdsigxx
            dseqdsigyy = dseqdphip*dphipdsigyy + dseqdphipp*dphippdsigyy
            dseqdsigxy = dseqdphip*dphipdsigxy + dseqdphipp*dphippdsigxy
!
            !< Derivative of yield function w.r.t equivalent stress
            dphidseq = two*(seq(i)/(yld(i)**2))
!
            !< Assembling derivative of yield function w.r.t stress tensor
            normxx = dphidseq*dseqdsigxx
            normyy = dphidseq*dseqdsigyy
            normxy = dphidseq*dseqdsigxy
!
            !< Derivative of stress tensor w.r.t plastic multiplier
            dsigxxdlam = -a1*normxx - a2*normyy
            dsigyydlam = -a1*normyy - a2*normxx
            dsigxydlam =  -g*normxy
!
            !< Contribution of stress tensor to derivative w.r.t pl. multiplier
            dphidsig_dsigdlam = normxx*dsigxxdlam +                            &
                                normyy*dsigyydlam +                            &
                                normxy*dsigxydlam
!
            !< 2 - Derivative of yield criterion w.r.t plastic multiplier
            !      Contribution of the plastic strain
            !-------------------------------------------------------------------
!
            !< Derivative of yield function w.r.t yield stress
            dphidyld = -two*(seq(i)**2/yld(i)**3)   

            !< Derivative of yield function w.r.t plastic strain
            dphidpla = dphidyld*dylddp(i)
!
            !< Derivative of plastic strain w.r.t plastic multiplier
            sig_dphidsig = signxx(i)*normxx +                                  &
                           signyy(i)*normyy +                                  &
                           signxy(i)*normxy
            dpladlam = sig_dphidsig/yld(i)
!
            !< 3 - Derivative of yield criterion w.r.t plastic multiplier
            !      Contribution of the kinematic hardening
            !-------------------------------------------------------------------
            if (fisokin > zero) then 
              !<  -> Chaboche-Rousselier kinematic hardening
              if (ikin == 1) then  
                dsigbxxdlam = fisokin*(akck*normxx - dsigbxxdp(i)*dpladlam)
                dsigbyydlam = fisokin*(akck*normyy - dsigbyydp(i)*dpladlam)
                dsigbxydlam = fisokin*(akck*normxy - dsigbxydp(i)*dpladlam)
              !<  -> Prager kinematic hardening
              elseif (ikin == 2) then 
                dsigbxxdlam = two_third*hk(i)*(two*normxx + normyy)
                dsigbyydlam = two_third*hk(i)*(two*normyy + normxx)
                dsigbxydlam = two_third*hk(i)*normxy
              endif
              !< Assembling derivative
              dphidsigb_dsigbdlam = -normxx*dsigbxxdlam -                     &
                                     normyy*dsigbyydlam -                     &
                                     normxy*dsigbxydlam
            !<  -> No kinematic hardening
            else 
              dphidsigb_dsigbdlam = zero
            endif
!
            !< 4 - Derivative of yield criterion w.r.t plastic multiplier
            !-------------------------------------------------------------------
            !< Computation of the plastic multiplier
            dphidlam = dphidsig_dsigdlam                                       &
                     + dphidpla*dpladlam                                       &
                     + dphidsigb_dsigbdlam
            dphidlam = sign(max(abs(dphidlam),em20),dphidlam)
!
            !< 5 - Computation of plastic multiplier
            !-------------------------------------------------------------------
            dlam = -phi(i)/dphidlam
!         
            !< 6 - Update plastic strain related variables
            !------------------------------------------------------------------- 
            !< Cumulated plastic strain update           
            ddep    = dpladlam*dlam
            dpla(i) = max(zero, dpla(i) + ddep)
            pla(i)  = pla(i) + ddep 
!
            !< Out of plane plastic strain increment
            deplzz(i) = deplzz(i) - dlam*normxx - dlam*normyy
! 
            !< 7 - Update stress tensor and related variable
            !-------------------------------------------------------------------
            !< Update of the stress tensor
            signxx(i) = signxx(i) + dsigxxdlam*dlam
            signyy(i) = signyy(i) + dsigyydlam*dlam
            signxy(i) = signxy(i) + dsigxydlam*dlam
!
            !< Update of the backstress tensor (if kinematic hardening)
            if (fisokin > zero) then 
              ! -> Remove kinematic hardening contribution
              signxx(i) = signxx(i) + sigbxx(i)
              signyy(i) = signyy(i) + sigbyy(i)
              signxy(i) = signxy(i) + sigbxy(i)
              ! -> Add the evolution of backstress tensor
              sigbxx(i) = sigbxx(i) + dsigbxxdlam*dlam
              sigbyy(i) = sigbyy(i) + dsigbyydlam*dlam
              sigbxy(i) = sigbxy(i) + dsigbxydlam*dlam
              ! -> Add the kinematic hardening contribution
              signxx(i) = signxx(i) - sigbxx(i)
              signyy(i) = signyy(i) - sigbyy(i)
              signxy(i) = signxy(i) - sigbxy(i)
              !<  -> Chaboche-Rousselier kinematic hardening 
              if (ikin == 1) then
                ! -> Update the all set of backstresses components
                sigb(i, 1) = sigb(i, 1) +                                      &
                  fisokin*(akh(1)*ckh(1)*normxx*dlam  - ckh(1)*sigb(i,1)*ddep) 
                sigb(i, 2) = sigb(i, 2) +                                      &
                  fisokin*(akh(1)*ckh(1)*normyy*dlam  - ckh(1)*sigb(i,2)*ddep)  
                sigb(i, 3) = sigb(i, 3) +                                      &
                  fisokin*(akh(1)*ckh(1)*normxy*dlam  - ckh(1)*sigb(i,3)*ddep)    
                sigb(i, 4) = sigb(i, 4) +                                      &
                  fisokin*(akh(2)*ckh(2)*normxx*dlam  - ckh(2)*sigb(i,4)*ddep)     
                sigb(i, 5) = sigb(i, 5) +                                      &
                  fisokin*(akh(2)*ckh(2)*normyy*dlam  - ckh(2)*sigb(i,5)*ddep) 
                sigb(i, 6) = sigb(i, 6) +                                      &
                  fisokin*(akh(2)*ckh(2)*normxy*dlam  - ckh(2)*sigb(i,6)*ddep)  
                sigb(i, 7) = sigb(i, 7) +                                      & 
                  fisokin*(akh(3)*ckh(3)*normxx*dlam  - ckh(3)*sigb(i,7)*ddep)
                sigb(i, 8) = sigb(i, 8) +                                      &
                  fisokin*(akh(3)*ckh(3)*normyy*dlam  - ckh(3)*sigb(i,8)*ddep)  
                sigb(i, 9) = sigb(i, 9) +                                      &
                  fisokin*(akh(3)*ckh(3)*normxy*dlam  - ckh(3)*sigb(i,9)*ddep)  
                sigb(i,10) = sigb(i,10) +                                      &
                  fisokin*(akh(4)*ckh(4)*normxx*dlam - ckh(4)*sigb(i,10)*ddep)
                sigb(i,11) = sigb(i,11) +                                      &
                  fisokin*(akh(4)*ckh(4)*normyy*dlam - ckh(4)*sigb(i,11)*ddep) 
                sigb(i,12) = sigb(i,12) +                                      &
                  fisokin*(akh(4)*ckh(4)*normxy*dlam - ckh(4)*sigb(i,12)*ddep)
              !<  -> Prager kinematic hardening
              elseif (ikin == 2) then
                sigb(i, 1) = sigb(i,1) + dsigbxxdlam*dlam
                sigb(i, 2) = sigb(i,2) + dsigbyydlam*dlam
                sigb(i, 3) = sigb(i,3) + dsigbxydlam*dlam
              endif
            endif
!
            !< Norm of the stress tensor
            normsig(i) = signxx(i)*signxx(i)                                   & 
                       + signyy(i)*signyy(i)                                   &
                   + two*signxy(i)*signxy(i)
            normsig(i) = sqrt(normsig(i))
            normsig(i) = max(normsig(i),one)
!
            !< Update of the equivalent stress
            !< - Computation of xprime tensor
            xpxx(i)  = (lp11*signxx(i) + lp12*signyy(i))/normsig(i)
            xpyy(i)  = (lp21*signxx(i) + lp22*signyy(i))/normsig(i)
            xpxy(i)  =  lp66*signxy(i)/normsig(i)
            !< - Computation of xprimeprime tensor
            xppxx(i) = (lpp11*signxx(i) + lpp12*signyy(i))/normsig(i)
            xppyy(i) = (lpp21*signxx(i) + lpp22*signyy(i))/normsig(i)
            xppxy(i) =  lpp66*signxy(i)/normsig(i)
            !< - Computation of xprime tensor principal values
            mohr_center = (xpxx(i)+xpyy(i))/two
            mohr_radius = sqrt(((xpxx(i)-xpyy(i))/two)**2 + xpxy(i)**2)
            xp1(i) = mohr_center + mohr_radius
            xp2(i) = mohr_center - mohr_radius
            !< - Computation of xprimeprime tensor principal values
            mohr_center = (xppxx(i)+xppyy(i))/two
            mohr_radius = sqrt(((xppxx(i)-xppyy(i))/two)**2 + xppxy(i)**2)
            xpp1(i) = mohr_center + mohr_radius
            xpp2(i) = mohr_center - mohr_radius                   
            !< - Computation of phiprime and phiprimeprime
            phip(i)  = (abs(xp1(i) - xp2(i)))**expa
            phipp(i) = (abs(two*xpp2(i) + xpp1(i)))**expa +                    &
                       (abs(two*xpp1(i) + xpp2(i)))**expa
            !< - Computation of the equivalent stress
            seq(i) = half*(phip(i)+phipp(i))     
            if (seq(i) > zero) then
              seq(i) = exp((one/expa)*log(seq(i)))
            else
              seq(i) = zero
            endif 
            seq(i) = seq(i)*normsig(i)
!
            !< Update the yield stress
            if (pla(i) + eps0hs > zero) then 
              expo(i)  = exp(nhs*log(pla(i) + eps0hs))
              dexpo(i) = nhs*exp((nhs-one)*log(pla(i) + eps0hs))
            else
              expo(i)  = zero
              dexpo(i) = zero
            endif
            aexp(i)   = (bhs - ahs)*exp(-mhs*expo(i))
            yld(i)    = (bhs-aexp(i))*atemp(i) + hmart*vm(i)
            dylddp(i) = mhs*dexpo(i)*aexp(i)*atemp(i)
            yld(i)    = (one - fisokin)*yld(i) + fisokin*yld0(i)    
            hk(i)     = fisokin*dylddp(i)
            dylddp(i) = (one - fisokin)*dylddp(i)
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
        !< Update the coefficient for hourglass control
        do ii = 1,nindx
          i = indx(ii)
          !< Hourglass stiffness parameter
          etse(i) = (dylddp(i)+hk(i)) / ((dylddp(i)+hk(i)) + young)
        enddo
!
        !< Update the temperature if needed
        if (jthe == 0) then 
#include "vectorize.inc" 
          do ii = 1,nindx 
            i = indx(ii)   
            temp(i) = temp(i) + yld(i)*dpla(i)*eta/(rho0(i)*cp)
          enddo
        endif
!
        !< Update the martensite volume fraction
#include "vectorize.inc" 
        do ii = 1,nindx 
          i = indx(ii) 
          if ((pla(i) >= e0mart) .and. (temp(i) > zero) .and.                  &
              ( vm(i) > zero     .and.    vm(i) <  one)) then
            dvmdpla = (bm/am)*exp(qm/temp(i))*half*(one - tanh(cm + dm*temp(i)))
            dvmdpla = dvmdpla*exp(ppm*log(vm(i)))
            dvmdpla = dvmdpla*exp(((bm+one)/bm)*log((one-vm(i))/vm(i)))                                                                            
            vm(i) = vm(i) + max(dvmdpla*dpla(i),zero)
            vm(i) = max(min(vm(i),one),zero)
            uvar(i,1) = vm(i)
          endif
        enddo
      endif
      !=========================================================================
      !< - END OF PLASTIC RETURN MAPPING PROCEDURE
      !=========================================================================
!
      !< Non-local thickness variation (if activated)
      if (inloc > 0) then
        do i = 1,nel
          if ((loff(i) == one).and.(seq(i) > zero)) then 
            !< Derivative of xprime 1 w.r.t xprime tensor
            mohr_radius = sqrt(((xpxx(i)-xpyy(i))/two)**2 + xpxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            dxp1dxpxx = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpyy = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpxy = xpxy(i)/mohr_radius
            !< Derivative of xprime 2 w.r.t xprime tensor
            dxp2dxpxx = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpyy = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpxy = -xpxy(i)/mohr_radius
            !< Derivative of xprimeprime 1 w.r.t xprimeprime tensor
            mohr_radius = sqrt(((xppxx(i)-xppyy(i))/two)**2 + xppxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            dxpp1dxppxx = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppyy = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppxy = xppxy(i)/mohr_radius
            !< Derivative of xprimeprime 2 w.r.t xprimeprime tensor
            dxpp2dxppxx = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppyy = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppxy = -xppxy(i)/mohr_radius
            !< Assembling derivative of xprime 1 w.r.t stress tensor
            dxp1dsigxx = dxp1dxpxx*lp11 + dxp1dxpyy*lp21          
            dxp1dsigyy = dxp1dxpxx*lp12 + dxp1dxpyy*lp22
            dxp1dsigxy = dxp1dxpxy*lp66
            !< Assembling derivative of xprime 2 w.r.t stress tensor
            dxp2dsigxx = dxp2dxpxx*lp11 + dxp2dxpyy*lp21
            dxp2dsigyy = dxp2dxpxx*lp12 + dxp2dxpyy*lp22
            dxp2dsigxy = dxp2dxpxy*lp66
            !< Assembling derivative of xprimeprime 1 w.r.t stress tensor
            dxpp1dsigxx = dxpp1dxppxx*lpp11 + dxpp1dxppyy*lpp21 
            dxpp1dsigyy = dxpp1dxppxx*lpp12 + dxpp1dxppyy*lpp22
            dxpp1dsigxy = dxpp1dxppxy*lpp66
            !< Assembling derivative of xprimeprime 2 w.r.t stress tensor
            dxpp2dsigxx = dxpp2dxppxx*lpp11 + dxpp2dxppyy*lpp21
            dxpp2dsigyy = dxpp2dxppxx*lpp12 + dxpp2dxppyy*lpp22
            dxpp2dsigxy = dxpp2dxppxy*lpp66
            !< Derivative of phiprime w.r.t xprime 1
            dphipdxp1 = (abs(xp1(i)-xp2(i)))**(expa-1)*sign(one,xp1(i)-xp2(i))
            !< Derivative of phiprime w.r.t xprime 2
            dphipdxp2 = -dphipdxp1
            !< Derivative of phiprimeprime w.r.t xprimeprime 1
            dphippdxpp1 = expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*           &
                                           sign(one,two*xpp2(i)+xpp1(i)) +     &
                      two*expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*           &
                                           sign(one,two*xpp1(i)+xpp2(i))
            !< Derivative of phiprimeprime w.r.t xprimeprime 2
            dphippdxpp2 = expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*           &
                                           sign(one,two*xpp1(i)+xpp2(i)) +     &
                      two*expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*           &
                                           sign(one,two*xpp2(i)+xpp1(i))
            !< Assembling derivative of phiprime w.r.t stress tensor
            dphipdsigxx = dphipdxp1*dxp1dsigxx + dphipdxp2*dxp2dsigxx
            dphipdsigyy = dphipdxp1*dxp1dsigyy + dphipdxp2*dxp2dsigyy
            dphipdsigxy = dphipdxp1*dxp1dsigxy + dphipdxp2*dxp2dsigxy   
            !< Assembling derivative of phiprimeprime w.r.t stress tensor
            dphippdsigxx = dphippdxpp1*dxpp1dsigxx + dphippdxpp2*dxpp2dsigxx
            dphippdsigyy = dphippdxpp1*dxpp1dsigyy + dphippdxpp2*dxpp2dsigyy
            dphippdsigxy = dphippdxpp1*dxpp1dsigxy + dphippdxpp2*dxpp2dsigxy
            !< Derivative of equivalent stress w.r.t phiprime
            dseqdphip  =                                                       &
               (half/expa)*exp((one/expa - one)*log(half*(phip(i)+phipp(i))))
            !< Derivative of equivalent stress w.r.t phiprimeprime
            dseqdphipp = dseqdphip
            !< Assembling derivative of equivalent stress w.r.t stress tensor
            dseqdsigxx = dseqdphip*dphipdsigxx + dseqdphipp*dphippdsigxx
            dseqdsigyy = dseqdphip*dphipdsigyy + dseqdphipp*dphippdsigyy
            dseqdsigxy = dseqdphip*dphipdsigxy + dseqdphipp*dphippdsigxy
            !< Derivative of yield function w.r.t equivalent stress
            dphidseq = two*(seq(i)/(yld(i)**2))
            !< Assembling derivative of yield function w.r.t stress tensor
            normxx = dphidseq*dseqdsigxx
            normyy = dphidseq*dseqdsigyy
            normxy = dphidseq*dseqdsigxy
            !< Derivative of stress tensor w.r.t plastic multiplier
            sig_dphidsig = signxx(i)*normxx +                                  &
                           signyy(i)*normyy +                                  &
                           signxy(i)*normxy
            !< Non-local out-of-plane plastic strain increment
            deplzz(i) = -dplanl(i)*(yld(i)/max(sig_dphidsig,em20))*            &
                                                 (normxx + normyy)
          endif
        enddo
      endif
!
      !< Remove backstress contribution to the stress tensor
      if (fisokin > zero) then
        do i = 1,nel
          !< Remove the kinematic hardening contribution to stress tensor
          signxx(i) = signxx(i) + sigbxx(i)
          signyy(i) = signyy(i) + sigbyy(i)
          signxy(i) = signxy(i) + sigbxy(i)
        enddo
      endif
!
      !< Update the user variable, soundspeed and thickness
      do i=1,nel
        !< Strain rate 
        epsd(i) = epsp(i) 
        !< Elastic strain increment in the z direction 
        deelzz(i) = -nu*(signxx(i)-sigoxx(i)+signyy(i)-sigoyy(i))/young
        !< Assembling total strain increment in the z direction
        depszz(i) = deelzz(i) + deplzz(i)
        !< Update of the thickness
        thk(i) = thk(i) + depszz(i)*thkly(i)*off(i)
        !< Update of the soundspeed
        soundsp(i) = sqrt(a1/rho0(i))
      enddo
!
      end subroutine mat87c_hansel
      end module mat87c_hansel_mod
