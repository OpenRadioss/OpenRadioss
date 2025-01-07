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
    module mat87c_tabulated_3dir_ortho_mod
      contains
      subroutine mat87c_tabulated_3dir_ortho(                                  &
        nel    ,matparam,numtabl ,itable  ,ntable   ,                          &
        table  ,nvartmp ,vartmp  ,timestep,                                    &
        rho0   ,thkly   ,thk     ,epsp    ,                                    &
        epspxx ,epspyy  ,epspxy  ,                                             &
        depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx   ,                          &
        sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,                          &
        signxx ,signyy  ,signxy  ,signyz  ,signzx   ,                          &
        soundsp,pla     ,dpla    ,epsd    ,yld      ,                          &
        etse   ,gs      ,israte  ,asrate  ,off      ,                          &
        sigb   ,inloc   ,dplanl  ,seq     ,loff     )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
        implicit none
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y  A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                            :: nel      !< Number of elements
        type(matparam_struct_), intent(in)             :: matparam !< Material parameters data structure
        integer, intent(in)                            :: numtabl  !< Number of material tables
        integer, dimension(numtabl), intent(in)        :: itable   !< Material table index
        integer, intent(in)                            :: ntable   !< Number of points in the material table
        type(ttable), dimension(ntable), intent(in)    :: table    !< Material table
        integer, intent(in)                            :: nvartmp  !< Number of temporary variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables
        my_real, intent(in)                            :: timestep !< Time step
        my_real, dimension(nel), intent(in)            :: rho0     !< Density
        my_real, dimension(nel), intent(in)            :: thkly    !< Layer thickness
        my_real, dimension(nel), intent(inout)         :: thk      !< Thickness
        my_real, dimension(nel), intent(inout)         :: epsp     !< Equivalent and filtered total strain rate
        my_real, dimension(nel), intent(in)            :: epspxx   !< Strain rate component xx
        my_real, dimension(nel), intent(in)            :: epspyy   !< Strain rate component yy
        my_real, dimension(nel), intent(in)            :: epspxy   !< Strain rate component xy
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
        my_real, dimension(nel), intent(inout)         :: pla      !< Plastic strain
        my_real, dimension(nel), intent(inout)         :: dpla     !< Plastic strain increment
        my_real, dimension(nel), intent(inout)         :: epsd     !< Output strain rate 
        my_real, dimension(nel), intent(inout)         :: yld      !< Yield stress
        my_real, dimension(nel), intent(inout)         :: etse     !< Equivalent stress
        my_real, dimension(nel), intent(in)            :: gs       !< Transverse shear modulus
        integer, intent(in)                            :: israte   !< Strain rate filtering flag
        my_real, intent(in)                            :: asrate   !< Strain rate filtering factor
        my_real, dimension(nel), intent(in)            :: off      !< Flag for element status
        my_real, dimension(nel,12), intent(inout)      :: sigb     !< Backstress
        integer, intent(in)                            :: inloc    !< Flag for non-local regularization
        my_real, dimension(nel), intent(in)            :: dplanl   !< Non-local plastic strain increment
        my_real, dimension(nel), intent(inout)         :: seq      !< Equivalent stress
        my_real, dimension(nel), intent(in)            :: loff     !< Flag for Gauss point status
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer i,ii,j,k,nindx,indx(nel),iter,iflagsr,ismooth
      my_real ::                                                               &
        young,nu,a1,a2,g,al1,al2,al3,al4,al5,al6,al7,al8,fisokin,expa,ckh(4),  &
        akh(4),lp11,lp12,lp21,lp22,lp66,lpp11,lpp12,lpp21,lpp22,lpp66,akck,    &
        epsd0,epsd45,epsd90,yscale0,yscale45,yscale90
      my_real ::                                                               &
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
        dylddsigyy,dylddsigxy,dphidseq,sig1,sig2,dpdt,alpha
      my_real ::                                                               &
        deplzz(nel),sigbxx(nel),sigbyy(nel),sigbxy(nel),cos2theta(nel),        &
        cos4theta(nel),sin2theta(nel),normsig(nel),xp1(nel),xp2(nel),xpp1(nel),&
        xpp2(nel),phip(nel),phipp(nel),yld0(nel),dyld0dp(nel),yld45(nel),      &
        dyld45dp(nel),yld90(nel),dyld90dp(nel),phi(nel),xvec(nel,6),           &
        deelzz(nel),dylddp(nel),dylddcs2theta(nel),hardr(nel),xpxx(nel),       &
        xpyy(nel),xpxy(nel),xppxx(nel),xppyy(nel),xppxy(nel),depszz(nel)
      integer, parameter :: niter = 3
!===============================================================================
!
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Recovering integer model parameter
      iflagsr  = matparam%iparam(3)  !< Flag for strain rate computation
      ismooth  = matparam%iparam(7)  !< Flag for strain rate interpolation
      !< Recovering real model paramter
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
      yscale0  = matparam%uparam(18) !< Scale factor for yield stress in dir. 0
      yscale45 = matparam%uparam(19) !< Scale factor for yield stress in dir. 45
      yscale90 = matparam%uparam(20) !< Scale factor for yield stress in dir. 90
      epsd0    = matparam%uparam(21) !< Reference strain for yield stress in dir. 0
      epsd45   = matparam%uparam(22) !< Reference strain for yield stress in dir. 45
      epsd90   = matparam%uparam(23) !< Reference strain for yield stress in dir. 90
      !< Kinematic hardening parameters
      if (fisokin >zero) then                  
        ckh(1) = matparam%uparam(24)
        akh(1) = matparam%uparam(25)
        ckh(2) = matparam%uparam(26)
        akh(2) = matparam%uparam(27)
        ckh(3) = matparam%uparam(28)
        akh(3) = matparam%uparam(29)
        ckh(4) = matparam%uparam(30)
        akh(4) = matparam%uparam(31)
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
            epsp(i) = epsd(i)
          enddo
        else
          epsd(1:nel) = epsp(1:nel)
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
        signxx(i) = sigoxx(i) + a1*depsxx(i) + a2*depsyy(i)
        signyy(i) = sigoyy(i) + a2*depsxx(i) + a1*depsyy(i)
        signxy(i) = sigoxy(i) +  g*depsxy(i)               
        signyz(i) = sigoyz(i) + gs(i)*depsyz(i)
        signzx(i) = sigozx(i) + gs(i)*depszx(i)
      enddo
      !< Backstress tensor computation
      if (fisokin > zero) then 
        do i=1,nel
          sigbxx(i) = sigb(i,1) + sigb(i,4) + sigb(i,7) + sigb(i,10)
          sigbyy(i) = sigb(i,2) + sigb(i,5) + sigb(i,8) + sigb(i,11)
          sigbxy(i) = sigb(i,3) + sigb(i,6) + sigb(i,9) + sigb(i,12)
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
        phip(i)  = exp(expa*log(abs(xp1(i) - xp2(i))))
        phipp(i) = exp(expa*log(abs(two*xpp2(i) + xpp1(i)))) +                 &
                   exp(expa*log(abs(two*xpp1(i) + xpp2(i))))
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
      !< Tabulated yield stress in direction 0
      xvec(1:nel,1) = pla(1:nel)
      xvec(1:nel,2) = epsd(1:nel)/epsd0
      call table2d_vinterp_log(table(itable(1)),ismooth,nel,nel,vartmp(1,1),   &
                               xvec,yld0,dyld0dp,hardr)
      yld0(1:nel)    = yld0(1:nel)*yscale0
      dyld0dp(1:nel) = dyld0dp(1:nel)*yscale0
      !< Tabulated yield stress in direction 45
      xvec(1:nel,2) = epsd(1:nel)/epsd45
      call table2d_vinterp_log(table(itable(2)),ismooth,nel,nel,vartmp(1,3),   &
                               xvec,yld45,dyld45dp,hardr)
      yld45(1:nel)    = yld45(1:nel)*yscale45
      dyld45dp(1:nel) = dyld45dp(1:nel)*yscale45
      !< Tabulated yield stress in direction 90
      xvec(1:nel,2) = epsd(1:nel)/epsd90
      call table2d_vinterp_log(table(itable(3)),ismooth,nel,nel,vartmp(1,5),   &
                               xvec,yld90,dyld90dp,hardr)
      yld90(1:nel)    = yld90(1:nel)*yscale90
      dyld90dp(1:nel) = dyld90dp(1:nel)*yscale90
      !< Assembling the yield stress and its derivative
      do i = 1,nel
        !< Compute interpolation factors
        q1 = (yld0(i) + two*yld45(i) + yld90(i))/four
        q2 = (yld0(i) - yld90(i))/two
        q3 = (yld0(i) - two*yld45(i) + yld90(i))/four
        !< Directional yield stress
        yld(i) = q1 + q2*cos2theta(i) + q3*cos4theta(i)
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
        !< - Derivative of the yield stress w.r.t the loading orientation
        dylddcs2theta(i) = q2 + four*q3*cos2theta(i)
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
            dphipdxp1 =  expa*exp((expa-1)*log(abs(xp1(i)-xp2(i))))*           &
                                          sign(one,xp1(i)-xp2(i))
            !< Derivative of phiprime w.r.t xprime 2
            dphipdxp2 = -dphipdxp1
!
            !< Derivative of phiprimeprime w.r.t xprimeprime 1
            dphippdxpp1 = expa*exp((expa-1)*log(abs(two*xpp2(i)+xpp1(i))))*    &
                                           sign(one,two*xpp2(i)+xpp1(i)) +     &
                      two*expa*exp((expa-1)*log(abs(two*xpp1(i)+xpp2(i))))*    &
                                           sign(one,two*xpp1(i)+xpp2(i))
            !< Derivative of phiprimeprime w.r.t xprimeprime 2
            dphippdxpp2 = expa*exp((expa-1)*log(abs(two*xpp1(i)+xpp2(i))))*    &
                                           sign(one,two*xpp1(i)+xpp2(i)) +     &
                      two*expa*exp((expa-1)*log(abs(two*xpp2(i)+xpp1(i))))*    &
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
            !-------------------------------------------------------------------
            !< Computation of the plastic multiplier
            dphidlam = dphidsig_dsigdlam + dphidpla*dpladlam
            dphidlam = sign(max(abs(dphidlam),em20),dphidlam)

            !< 4 - Computation of plastic multiplier
            !-------------------------------------------------------------------
            dlam = -phi(i)/dphidlam
!         
            !< 5 - Update plastic strain related variables
            !------------------------------------------------------------------- 
            !< Cumulated plastic strain update           
            ddep    = dpladlam*dlam
            dpla(i) = max(zero, dpla(i) + ddep)
            pla(i)  = pla(i) + ddep 
!
            !< Out of plane plastic strain increment
            deplzz(i) = deplzz(i) - dlam*normxx - dlam*normyy
! 
            !< 6 - Update stress tensor and related variable
            !-------------------------------------------------------------------            
            !< Update of the stress tensor
            signxx(i) = signxx(i) + dsigxxdlam*dlam
            signyy(i) = signyy(i) + dsigyydlam*dlam
            signxy(i) = signxy(i) + dsigxydlam*dlam
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
            phip(i)  = exp(expa*log(abs(xp1(i) - xp2(i))))
            phipp(i) = exp(expa*log(abs(two*xpp2(i) + xpp1(i)))) +             &
                       exp(expa*log(abs(two*xpp1(i) + xpp2(i))))
            !< - Computation of the equivalent stress
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
          !< 7 - Update yield stress
          !---------------------------------------------------------------------
          !< Tabulated yield stress in direction 0
          xvec(1:nel,1) = pla(1:nel)
          xvec(1:nel,2) = epsd(1:nel)/epsd0
          call table2d_vinterp_log(table(itable(1)),ismooth,nel,nel,           & 
                                   vartmp(1,1),xvec,yld0,dyld0dp,hardr)
          yld0(1:nel)     = yld0(1:nel)*yscale0
          dyld0dp(1:nel)  = dyld0dp(1:nel)*yscale0
          !< Tabulated yield stress in direction 45
          xvec(1:nel,2) = epsd(1:nel)/epsd45
          call table2d_vinterp_log(table(itable(2)),ismooth,nel,nel,           &
                                   vartmp(1,3),xvec,yld45,dyld45dp,hardr)
          yld45(1:nel)    = yld45(1:nel)*yscale45
          dyld45dp(1:nel) = dyld45dp(1:nel)*yscale45
          !< Tabulated yield stress in direction 90
          xvec(1:nel,2) = epsd(1:nel)/epsd90
          call table2d_vinterp_log(table(itable(3)),ismooth,nel,nel,           & 
                                   vartmp(1,5),xvec,yld90,dyld90dp,hardr)
          yld90(1:nel)    = yld90(1:nel)*yscale90
          dyld90dp(1:nel) = dyld90dp(1:nel)*yscale90
!
#include "vectorize.inc" 
          !< Loop over yielding elements
          do ii = 1, nindx
            i = indx(ii)  
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
!
            !< 8 - Update yield function value
            !-------------------------------------------------------------------
            phi(i) = (seq(i)/yld(i))**2 - one
!
          enddo
          ! End of the loop over yielding elements 
        enddo
        ! End of the loop over the iterations 
!
#include "vectorize.inc" 
        !< Update the coefficient for hourglass control
        do ii = 1,nindx
          i = indx(ii)
          etse(i) = dylddp(i) / (dylddp(i) + young)
        enddo
!
      endif
      !=========================================================================
      !< - END OF PLASTIC RETURN MAPPING PROCEDURE
      !=========================================================================
!
      !< Plastic strain rate if activated
      if (iflagsr == 1) then
        do i = 1,nel
          dpdt = dpla(i)/max(timestep,em20)
          alpha = min(one,asrate*timestep)
          epsd(i) = alpha*dpdt + (one - alpha)*epsd(i)
          epsp(i) = epsd(i)
        enddo
      endif
!
      !< Non-local thickness variation (if activated)
      if (inloc > 0) then
        do i = 1,nel
          if (loff(i) == one) then 
            sig_dphidsig = signxx(i)*signxx(i)                                 & 
                         + signyy(i)*signyy(i)                                 &
                         + signxy(i)*signxy(i)
            if (sig_dphidsig /= zero) then 
              deplzz(i) = -max(dplanl(i),zero)*(yld(i)/sig_dphidsig)
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