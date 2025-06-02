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
      !||    mat87c_tabulated_3dir_ortho_mod   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps87c                         ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||====================================================================
      module mat87c_tabulated_3dir_ortho_mod
      contains
      !||====================================================================
      !||    mat87c_tabulated_3dir_ortho   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps87c                     ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp             ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                  ../common_source/modules/constant_mod.F
      !||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod                 ../common_source/modules/precision_mod.F90
      !||    table_mat_vinterp_mod         ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
      subroutine mat87c_tabulated_3dir_ortho(                                  &
        nel    ,matparam,nvartmp ,vartmp  ,timestep ,                          &
        rho0   ,thkly   ,thk     ,epsp    ,                                    &
        epspxx ,epspyy  ,epspxy  ,                                             &
        depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                          &
        sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                          &
        signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                          &
        soundsp,pla     ,dpla    ,epsd    ,yld      ,                          &
        etse   ,gs      ,israte  ,asrate  ,off      ,                          &
        l_sigb ,sigb    ,inloc   ,dplanl  ,seq      ,                          &
        loff   )
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
!-----------------------------------------------
!   D u m m y  A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)                            :: nel      !< Number of elements
      type(matparam_struct_), intent(in)             :: matparam !< Material parameters
      integer, intent(in)                            :: nvartmp  !< Number of temporary variables
      integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables
      real(kind=WP), intent(in)                            :: timestep !< Time step
      real(kind=WP), dimension(nel), intent(in)            :: rho0     !< Density
      real(kind=WP), dimension(nel), intent(in)            :: thkly    !< Layer thickness
      real(kind=WP), dimension(nel), intent(inout)         :: thk      !< Thickness
      real(kind=WP), dimension(nel), intent(in)            :: epsp     !< global strain rate
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
      real(kind=WP), dimension(nel), intent(inout)         :: pla      !< Plastic strain
      real(kind=WP), dimension(nel), intent(inout)         :: dpla     !< Plastic strain increment
      real(kind=WP), dimension(nel), intent(inout)         :: epsd     !< local strain rate
      real(kind=WP), dimension(nel), intent(inout)         :: yld      !< Yield stress
      real(kind=WP), dimension(nel), intent(inout)         :: etse     !< Coefficient for hourglass control
      real(kind=WP), dimension(nel), intent(in)            :: gs       !< Transverse shear modulus
      integer, intent(in)                            :: israte   !< Flag for strain rate filtering
      real(kind=WP), intent(in)                            :: asrate   !< Coefficient for strain rate filtering
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
      integer :: i,ii,j,k,nindx,indx(nel),iter,iflagsr,ipos(nel,6),ikin
      real(kind=WP) ::                                                               &
        young,nu,a1,a2,g,al1,al2,al3,al4,al5,al6,al7,al8,fisokin,expa,ckh(4),  &
        akh(4),lp11,lp12,lp21,lp22,lp66,lpp11,lpp12,lpp21,lpp22,lpp66,akck
      real(kind=WP) ::                                                               &
        mohr_radius,mohr_center,q1,q2,q3,dxp1dxpxx,dxp2dxpxx,dxp1dxpyy,        &
        dxp2dxpyy,dxp1dxpxy,dxp2dxpxy,dxpp1dxppxx,dxpp2dxppxx,dxpp1dxppyy,     &
        dxpp2dxppyy,dxpp1dxppxy,dxpp2dxppxy,dxp1dsigxx,dxp1dsigyy,dxp1dsigxy,  &
        dxp2dsigxx,dxp2dsigyy,dxp2dsigxy,dxpp1dsigxx,dxpp1dsigyy,dxpp1dsigxy,  &
        dxpp2dsigxx,dxpp2dsigyy,dxpp2dsigxy,dphipdxp1,dphipdxp2,dphippdxpp1,   &
        dphippdxpp2,dphipdsigxx,dphipdsigyy,dphipdsigxy,dphippdsigxx,          &
        dphippdsigyy,dphippdsigxy,dseqdphip,dseqdphipp,dseqdsigxx,dseqdsigyy,  &
        dseqdsigxy,normxx,normyy,normxy,dsigxxdlam,dsigyydlam,dsigxydlam,      &
        dphidsig_dsigdlam,dphidpla,sig_dphidsig,dphidlam,dlam,                 &
        ddep,dpladlam,dylddyld0,dylddyld45,dylddyld90,dphidyld,                &
        dcs2thetadsigxx,dcs2thetadsigyy,dcs2thetadsigxy,dylddsigxx,            &
        dylddsigyy,dylddsigxy,dphidseq,sig1,sig2,dpdt,alpha,                   &
        dphidsigb_dsigbdlam,dsigbxxdlam,dsigbyydlam,dsigbxydlam
      real(kind=WP) ::                                                               &
        deplzz(nel),sigbxx(nel),sigbyy(nel),sigbxy(nel),cos2theta(nel),        &
        cos4theta(nel),sin2theta(nel),normsig(nel),xp1(nel),xp2(nel),xpp1(nel),&
        xpp2(nel),phip(nel),phipp(nel),yld0(nel),dyld0dp(nel),yld45(nel),      &
        dyld45dp(nel),yld90(nel),dyld90dp(nel),phi(nel),xvec(nel,6),           &
        deelzz(nel),dylddp(nel),dylddcs2theta(nel),xpxx(nel),xpyy(nel),        &
        xpxy(nel),xppxx(nel),xppyy(nel),xppxy(nel),depszz(nel),yld_0(nel),     &
        yld0_0(nel),dyld0_0(nel),yld45_0(nel),dyld45_0(nel),yld90_0(nel),      &
        dyld90_0(nel),dyld_0dcs2theta(nel),yld0_i(nel),dyld0dp_i(nel),         &
        yld45_i(nel),dyld45dp_i(nel),yld90_i(nel),dyld90dp_i(nel),             &
        q1_0(nel),q2_0(nel),q3_0(nel),dsigbxxdp(nel),dsigbyydp(nel),           &
        dsigbxydp(nel),hk(nel)
      integer, parameter :: niter = 3 !< Number of return mapping iterations
!===============================================================================
!
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Recovering integer model parameter
      iflagsr  = matparam%iparam(2)  !< Flag for strain rate computation
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
      !< Kinematic hardening parameters
      if ((ikin == 1).and.(fisokin > zero)) then                  
        ckh(1) = matparam%uparam(13) 
        akh(1) = matparam%uparam(14) 
        ckh(2) = matparam%uparam(15) 
        akh(2) = matparam%uparam(16) 
        ckh(3) = matparam%uparam(17) 
        akh(3) = matparam%uparam(18)
        ckh(4) = matparam%uparam(19) 
        akh(4) = matparam%uparam(20) 
        akck   = akh(1)*ckh(1) + akh(2)*ckh(2) + akh(3)*ckh(3) + akh(4)*ckh(4)
      endif
!
      !< Total strain-rate computation
      if (iflagsr == 0) then 
        if (israte == 0) then 
          do i = 1,nel
            epsd(i) = half*(abs(epspxx(i)+epspyy(i))                           &
                        + sqrt((epspxx(i)-epspyy(i))*(epspxx(i)-epspyy(i))     &
                              + epspxy(i)*epspxy(i)))
          enddo
        else
          epsd(1:nel) = asrate*epsp(1:nel) + (one-asrate)*epsd(1:nel)
        endif
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
      !=========================================================================
      !< - RECOVERING USER VARIABLES AND STATE VARIABLES
      !=========================================================================
      dpla(1:nel)   = zero !< Cumulated plastic strain increment
      deplzz(1:nel) = zero !< Plastic strain increment component zz
      etse(1:nel)   = one  !< Coefficient for hourglass control
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
      !< Computation of loading orientation angle theta
      do i = 1, nel
        mohr_radius = sqrt(((signxx(i)-signyy(i))/two)**2 + signxy(i)**2)
        mohr_center = (signxx(i)+signyy(i))/two
        sig1 = mohr_center + mohr_radius
        sig2 = mohr_center - mohr_radius
        if (mohr_radius > em20) then
          cos2theta(i) = ((signxx(i)-signyy(i))/two)/mohr_radius
          sin2theta(i) = signxy(i)/mohr_radius
        else
          cos2theta(i) = one
          sin2theta(i) = zero
        endif
        if (sig1 < zero.or. ((sig2 < zero).and.(sig2 < -sig1))) then 
          cos2theta(i) = -cos2theta(i)
          sin2theta(i) = -sin2theta(i)
        endif
        cos4theta(i) = two*(cos2theta(i)**2) - one   
      enddo 
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
      !< Save the initial yield stress in case of kinematic hardening
      if (fisokin > zero) then
        xvec(1:nel,1) = zero
        xvec(1:nel,2) = epsd(1:nel)
        ipos(1:nel,1:6) = 1
        call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1,1),xvec,       &
                               yld0_0,dyld0dp )
        call table_mat_vinterp(matparam%table(2),nel,nel,ipos(1,3),xvec,       &
                               yld45_0,dyld45dp)
        call table_mat_vinterp(matparam%table(3),nel,nel,ipos(1,5),xvec,       &
                               yld90_0,dyld90dp)
        !< Assembling the initial yield stress
        do i = 1,nel
          !< Compute interpolation factors
          q1_0(i) = (yld0_0(i) + two*yld45_0(i) + yld90_0(i))/four
          q2_0(i) = (yld0_0(i) - yld90_0(i))/two
          q3_0(i) = (yld0_0(i) - two*yld45_0(i) + yld90_0(i))/four
          !< Directional yield stress
          yld_0(i) = q1_0(i) + q2_0(i)*cos2theta(i) + q3_0(i)*cos4theta(i)
          !< - Derivative of the yield stress w.r.t the loading orientation
          dyld_0dcs2theta(i) = q2_0(i) + four*q3_0(i)*cos2theta(i)
        enddo
      else
        yld_0(1:nel) = zero
        q1_0(1:nel)  = zero
        q2_0(1:nel)  = zero
        q3_0(1:nel)  = zero
        dyld_0dcs2theta(1:nel) = zero
      endif
!
      !< Recovering the abscissas: plastic strain and strain rate
      xvec(1:nel,1) = pla(1:nel)
      xvec(1:nel,2) = epsd(1:nel)
      ipos(1:nel,1:6) = vartmp(1:nel,1:6)
      !< Tabulated yield stress in direction 0
      call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1,1),xvec,yld0 ,   &
                             dyld0dp  )
      !< Tabulated yield stress in direction 45
      call table_mat_vinterp(matparam%table(2),nel,nel,ipos(1,3),xvec,yld45,   &
                             dyld45dp )
      !< Tabulated yield stress in direction 90
      call table_mat_vinterp(matparam%table(3),nel,nel,ipos(1,5),xvec,yld90,   & 
                             dyld90dp )
      !< Reset all tables
      xvec(1:nel,1:2) = zero
      ipos(1:nel,1:6) = 0
!
      !< Assembling the yield stress and its derivative
      do i = 1,nel
        !< Compute interpolation factors
        q1 = (yld0(i) + two*yld45(i) + yld90(i))/four
        q2 = (yld0(i) - yld90(i))/two
        q3 = (yld0(i) - two*yld45(i) + yld90(i))/four
        !< Directional yield stress
        yld(i) = q1 + q2*cos2theta(i) + q3*cos4theta(i)
        yld(i) = (one-fisokin)*yld(i) + fisokin*yld_0(i)
        !< Derivative of the directional yield stress
        !< - Derivative of the yield stress w.r.t yield stress in direction 0
        dylddyld0  = fourth + half*cos2theta(i) + fourth*cos4theta(i)
        !< - Derivative of the yield stress w.r.t yield stress in direction 45
        dylddyld45 = half   - half*cos4theta(i)
        !< - Derivative of the yield stress w.r.t yield stress in direction 90
        dylddyld90 = fourth - half*cos2theta(i) + fourth*cos4theta(i)
        !< - Derivative of the yield stress w.r.t the plastic strain
        dylddp(i) = dylddyld0*dyld0dp(i)   +                                   &
                    dylddyld45*dyld45dp(i) +                                   &
                    dylddyld90*dyld90dp(i)
        hk(i) = fisokin*dylddp(i)
        dylddp(i) = (one-fisokin)*dylddp(i)
        !< - Derivative of the yield stress w.r.t the loading orientation
        dylddcs2theta(i) = q2 + four*q3*cos2theta(i)
        dylddcs2theta(i) = dylddcs2theta(i)*(one - fisokin) +                  &
                           dyld_0dcs2theta(i)*fisokin
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
            !< Derivative of cos(2*theta) w.r.t stress tensor
            mohr_radius = sqrt(((signxx(i)-signyy(i))/two)**2 + signxy(i)**2) 
            dcs2thetadsigxx =  (sin2theta(i)**2)/(two*mohr_radius)
            dcs2thetadsigyy = -(sin2theta(i)**2)/(two*mohr_radius)
            dcs2thetadsigxy = -sin2theta(i)*cos2theta(i)/mohr_radius
!
            !< Derivative of yield stress w.r.t stress tensor
            dylddsigxx = dylddcs2theta(i)*dcs2thetadsigxx
            dylddsigyy = dylddcs2theta(i)*dcs2thetadsigyy
            dylddsigxy = dylddcs2theta(i)*dcs2thetadsigxy
!
            !< Derivative of yield function w.r.t yield stress
            dphidyld = -two*(seq(i)**2/yld(i)**3)            
!
            !< Assembling derivative of yield function w.r.t stress tensor
            normxx = dphidseq*dseqdsigxx + dphidyld*dylddsigxx
            normyy = dphidseq*dseqdsigyy + dphidyld*dylddsigyy
            normxy = dphidseq*dseqdsigxy + dphidyld*dylddsigxy
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
            !< Update the loading orientation angle theta
            mohr_radius = sqrt(((signxx(i)-signyy(i))/two)**2 + signxy(i)**2)
            mohr_center = (signxx(i)+signyy(i))/two
            sig1 = mohr_center + mohr_radius
            sig2 = mohr_center - mohr_radius
            if (mohr_radius > em20) then
              cos2theta(i) = ((signxx(i)-signyy(i))/two)/mohr_radius
              sin2theta(i) = signxy(i)/mohr_radius
            else
              cos2theta(i) = one
              sin2theta(i) = zero
            endif
            if (sig1 < zero .or. ((sig2 < zero).and.(sig2 < -sig1))) then 
              cos2theta(i) = -cos2theta(i)
              sin2theta(i) = -sin2theta(i)
            endif
            cos4theta(i) = two*(cos2theta(i)**2) - one       
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
            !< Save variables for yield stress in plastic index order    
            xvec(ii,1) = pla(i)
            xvec(ii,2) = epsd(i) 
            ipos(ii,1:6) = vartmp(i,1:6)
!
          enddo
!
          !< 8 - Update yield stress
          !---------------------------------------------------------------------
          !< Tabulated yield stress in direction 0
          call table_mat_vinterp(matparam%table(1),nindx,nindx,ipos(1,1),xvec, & 
                                 yld0_i,dyld0dp_i)
          !< Tabulated yield stress in direction 45
          call table_mat_vinterp(matparam%table(2),nindx,nindx,ipos(1,3),xvec, &
                                 yld45_i,dyld45dp_i)
          !< Tabulated yield stress in direction 90
          call table_mat_vinterp(matparam%table(3),nindx,nindx,ipos(1,5),xvec, &
                                 yld90_i,dyld90dp_i)
!
#include "vectorize.inc" 
          !< Loop over yielding elements
          do ii = 1, nindx
            i = indx(ii)
!
            !< Reverse plastic index order
            vartmp(i,1:6) = ipos(ii,1:6)
            yld0(i)  = yld0_i(ii)
            yld45(i) = yld45_i(ii) 
            yld90(i) = yld90_i(ii)
            dyld0dp(i)  = dyld0dp_i(ii)
            dyld45dp(i) = dyld45dp_i(ii)
            dyld90dp(i) = dyld90dp_i(ii)
!
            !< Compute interpolation factors
            q1 = (yld0(i) + two*yld45(i) + yld90(i))/four
            q2 = (yld0(i) - yld90(i))/two
            q3 = (yld0(i) - two*yld45(i) + yld90(i))/four
            !< Directional yield stress
            yld(i) = q1 + q2*cos2theta(i) + q3*cos4theta(i)
            !< Derivative
            !< - Derivative of the yield stress w.r.t yield stress in dir. 0
            dylddyld0  = fourth + half*cos2theta(i) + fourth*cos4theta(i)
            !< - Derivative of the yield stress w.r.t yield stress in dir. 45
            dylddyld45 = half   - half*cos4theta(i)
            !< - Derivative of the yield stress w.r.t yield stress in dir. 90
            dylddyld90 = fourth - half*cos2theta(i) + fourth*cos4theta(i)
            !< - Derivative of the yield stress w.r.t the plastic strain
            dylddp(i) = dylddyld0*dyld0dp(i)   +                               &
                        dylddyld45*dyld45dp(i) +                               &
                        dylddyld90*dyld90dp(i)
            !< - Derivative of the yield stress w.r.t the loading orientation
            dylddcs2theta(i) = q2 + four*q3*cos2theta(i)
            !< - Initial yield stress derivative w.r.t theta 
            !  (used for kinematic hardening)
            dyld_0dcs2theta(i) = q2_0(i) + four*q3_0(i)*cos2theta(i)
!
            !< Apply kinematic hardening coefficient
            yld(i) = (one - fisokin)*yld(i) + fisokin*yld0(i)
            hk(i) = fisokin*dylddp(i)
            dylddp(i) = (one - fisokin)*dylddp(i)
            dylddcs2theta(i) = dylddcs2theta(i)*(one - fisokin) +              &
                               dyld_0dcs2theta(i)*fisokin
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
      endif
      !=======================================================================
      !< - END OF PLASTIC RETURN MAPPING PROCEDURE
      !=======================================================================
!
      !< Plastic strain rate if activated
      if (iflagsr == 1) then
        do i = 1,nel
          dpdt    = dpla(i)/max(timestep,em20)
          epsd(i) = asrate*dpdt + (one - asrate)*epsd(i)
        enddo
      endif
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
            !< Derivative of cos(2*theta) w.r.t stress tensor
            mohr_radius = sqrt(((signxx(i)-signyy(i))/two)**2 + signxy(i)**2) 
            dcs2thetadsigxx =  (sin2theta(i)**2)/(two*mohr_radius)
            dcs2thetadsigyy = -(sin2theta(i)**2)/(two*mohr_radius)
            dcs2thetadsigxy = -sin2theta(i)*cos2theta(i)/mohr_radius
            !< Derivative of yield stress w.r.t stress tensor
            dylddsigxx = dylddcs2theta(i)*dcs2thetadsigxx
            dylddsigyy = dylddcs2theta(i)*dcs2thetadsigyy
            dylddsigxy = dylddcs2theta(i)*dcs2thetadsigxy
            !< Derivative of yield function w.r.t yield stress
            dphidyld = -two*(seq(i)**2/yld(i)**3)            
            !< Assembling derivative of yield function w.r.t stress tensor
            normxx = dphidseq*dseqdsigxx + dphidyld*dylddsigxx
            normyy = dphidseq*dseqdsigyy + dphidyld*dylddsigyy
            normxy = dphidseq*dseqdsigxy + dphidyld*dylddsigxy
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
      end subroutine mat87c_tabulated_3dir_ortho
      end module mat87c_tabulated_3dir_ortho_mod
