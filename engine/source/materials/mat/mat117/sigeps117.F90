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
!||    sigeps117_mod   ../engine/source/materials/mat/mat117/sigeps117.F90
!||--- called by ------------------------------------------------------
!||    suser43         ../engine/source/elements/solid/sconnect/suser43.F
!||====================================================================
      module sigeps117_mod
        contains
!||====================================================================
!||    sigeps117               ../engine/source/materials/mat/mat117/sigeps117.F90
!||--- called by ------------------------------------------------------
!||    suser43                 ../engine/source/elements/solid/sconnect/suser43.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine sigeps117(                                                    &
        nel     ,matparam,nuvar   ,uvar    ,jsms    ,time    ,                 &
        area    ,off     ,offl    ,ipg     ,nfail   ,ngl     ,                 &
        epszz   ,epsyz   ,epszx   ,depszz  ,depsyz  ,depszx  ,                 &
        signzz  ,signyz  ,signzx  ,stifm   ,dmels   ,dmg     ,                 &
        idtmins ,dtfacs  ,dtmins  ,nvartmp ,vartmp  )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use table_mat_vinterp_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
#include  "units_c.inc"
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
      integer ,intent(in)                                :: nel      !< Number of elements in the group
      type(matparam_struct_), intent(in)                 :: matparam !< Material parameters data structure
      integer ,intent(in)                                :: nuvar    !< Number of user variables
      real(kind=WP) ,dimension(nel,nuvar) ,intent(inout) :: uvar     !< User variables
      integer ,intent(in)                                :: jsms     !< Job step number
      real(kind=WP) ,intent(in)                          :: time     !< Current time
      real(kind=WP) ,dimension(nel) ,intent(in)          :: area     !< Element area
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: off      !< Element status (1=active, 0=failed)
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: offl     !< Integration point status (1=active, 0=failed)
      integer, intent(in)                                :: ipg      !< Integration point number
      integer, intent(inout)                             :: nfail    !< Number of failed intetegration points
      integer, dimension(nel) ,intent(in)                :: ngl      !< Element global number
      real(kind=WP) ,dimension(nel) ,intent(in)          :: epszz    !< Normal strain
      real(kind=WP) ,dimension(nel) ,intent(in)          :: epsyz    !< Shear strain yz
      real(kind=WP) ,dimension(nel) ,intent(in)          :: epszx    !< Shear strain zx
      real(kind=WP) ,dimension(nel) ,intent(in)          :: depszz   !< Normal strain increment
      real(kind=WP) ,dimension(nel) ,intent(in)          :: depsyz   !< Shear strain increment
      real(kind=WP) ,dimension(nel) ,intent(in)          :: depszx   !< Shear strain increment
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: signzz   !< Normal stress
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: signyz   !< Shear stress yz
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: signzx   !< Shear stress zx
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: stifm    !< Element stiffness
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: dmels    !< Element mass
      real(kind=WP) ,dimension(nel) ,intent(inout)       :: dmg      !< Element damage
      integer, intent(in)                                :: idtmins  !< Minimum time step
      real(kind=WP) , intent(in)                         :: dtfacs   !< Time step factor
      real(kind=WP) ,intent(in)                          :: dtmins   !< Minimum time step
      integer, intent(in)                                :: nvartmp  !< Number of temporary user variables
      integer, dimension(nel,nvartmp), intent(inout)     :: vartmp   !< Temporary user variables
!----------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!----------------------------------------------------------------
      integer :: i,ii, irupt, nindxf, icrittyp, ievoshap, isym
      integer ,dimension(nel)  :: indxf
      real(kind=WP) :: dm,stf,dtb,alpha,denom1,e_elas_n,t_n,t_s,exp_alpha,     &
        func_alpha,exp_g,exp_bk,delta0s_cst,delta0n_cst,gic,giic,       &
        e_elas_s,und_cst,utd_cst,gama,term1,term2,epsmmax_prev, dam_enr
      real(kind=WP), dimension(nel) :: epsm,delta0m,  beta, tmax_mix,          &
        deltafmax,epst,fac1, fac2,fac3,epsmmax ,epsn,                          &
        delta0n,delta0s,und,utd,dydx1,dydx2,length,tmax_n,tmax_s,              &
        gn,gs,g0,gc, dgi, dgii, dgiii, gtot, ratio, dam, u_norm, d_f,          &
        denom2,cos2,ddelta, sin2, delta_prev, e_elas, xi, a_coef, fade,        &
        coef0
      real(kind=WP), dimension(nel,1) :: xvec
!----------------------------------------------------------------     
!
      !=========================================================================
      !< - Initialisation of computation on time step
      !=========================================================================
      !< Recovering integer model parameters
      irupt       = matparam%iparam(1)   ! model (1 = power law, 2 = bk method) (irupt)
      nfail       = matparam%iparam(2)   ! idel (dft = 1 int)
      icrittyp    = matparam%iparam(3)   ! 1=displacement-based, 2=energy-based
      ievoshap    = matparam%iparam(4)   ! 1=linear, 2=exponential
      isym        = matparam%iparam(5)   ! 0=asymmetric (tension only), 1=symmetric tension/compression
      !< Recovering real model parameters
      e_elas_n    = matparam%uparam(1)   ! normal elastic stiffness (en)
      e_elas_s    = matparam%uparam(2)   ! tangential elastic stiffness (et)
      gama        = matparam%uparam(3)   ! mode coupling parameter (gamma)
      t_n         = matparam%uparam(4)   ! normal stress (tn)
      t_s         = matparam%uparam(5)   ! tangential shear stress (tt)
      gic         = matparam%uparam(6)   ! critical fracture energy - mode i (gic)
      giic        = matparam%uparam(7)   ! critical fracture energy - mode ii (giic)
      exp_g       = matparam%uparam(8)   !
      exp_bk      = matparam%uparam(9)   ! benzeggage-kenane exponent for the mixed mode (exp_bk)
      delta0n_cst = matparam%uparam(10)  !         disp puremode n - initial normal displacement
      delta0s_cst = matparam%uparam(11)  !         disp puremode s - initial tangential displacement
      und_cst     = matparam%uparam(12)  ! ultimate displacement in normal direction (und)
      utd_cst     = matparam%uparam(13)  ! ultimate displacement in tangential direction (utd)
      alpha       = matparam%uparam(14)  ! exponential law parameter (alpha)
      !< Compute some constants
      stf         = e_elas_n + e_elas_s
      exp_alpha   = exp(-alpha)
      denom1      = one - exp_alpha
      func_alpha  = (one - (one + alpha) * exp_alpha)/(alpha * denom1) 
      !< Save initial length for element size regularization
      where (uvar(1:nel,5) == zero) uvar(1:nel,5) = sqrt(area(1:nel))
      nindxf      = 0
!
      !< Recovering user variables and initialization of local variables
      signzz(1:nel)  = uvar(1:nel,1)
      signzx(1:nel)  = uvar(1:nel,2)
      signyz(1:nel)  = uvar(1:nel,3)
      epsmmax(1:nel) = uvar(1:nel,4)
      length(1:nel)  = uvar(1:nel,5)
      gn(1:nel)      = uvar(1:nel,6)
      gs(1:nel)      = uvar(1:nel,7)
!      
      !=========================================================================
      !< COMPUTE DELTA KNOWN AS EPSMMAX (i.e. MAX STRAIN)
      !=========================================================================
      ! compute norm and tang strain
      if (isym == 1) then
        epsn(1:nel) = abs(epszz(1:nel)) ! symmetric: compression drives damage like tension
      else
        epsn(1:nel) = max(epszz(1:nel) , zero) ! ignore compressive state
      endif
      epst(1:nel) = sqrt(epsyz(1:nel)**2  + epszx(1:nel)**2) ! combine both plans
      ! compute mixt stain state
      epsm(1:nel) = sqrt( epsn(1:nel)**2  + epst(1:nel)**2) ! mixt strain
      ! compute delta_max
      epsmmax(1:nel) = max(epsm(1:nel), epsmmax(1:nel)) !
!
      !=========================================================================
      !< COMPUTE DELTA0 (INITATION DISPLACMENT) and DELTAFMAX (FAILURE DISPLACEMENT)
      !=========================================================================
      if (matparam%table(1)%notable /= 0) then
        xvec(1:nel,1) = length(1:nel)
        call table_mat_vinterp(matparam%table(1),nel,nel,                      &
            vartmp(1:nel,1),xvec(1:nel,1),tmax_n(1:nel),dydx1(1:nel))
        !< Linear 
        if (ievoshap == 1) then
          delta0n(1:nel) = tmax_n(1:nel) / e_elas_n
          und(1:nel) = two*gic/tmax_n(1:nel)
        !< Exponential
        else if (ievoshap == 2) then
          delta0n(1:nel) = tmax_n(1:nel) / e_elas_n
          coef0(1:nel)   = gic - tmax_n(1:nel) * delta0n(1:nel) * half
          und(1:nel)     = delta0n(1:nel) + coef0(1:nel) /                     &
                              (func_alpha*tmax_n(1:nel))
        !< Xu-Needleman exponential
        else if (ievoshap == 3) then
          delta0n(1:nel) = gic / (exp(one) * tmax_n(1:nel))
          und(1:nel) = -delta0n(1:nel) * log(em04)
        endif
      else
        delta0n(1:nel) = delta0n_cst
        und(1:nel)     = und_cst
        tmax_n(1:nel)  = t_n
      endif
!      
      if (matparam%table(2)%notable /= 0) then
        xvec(1:nel,1) = length(1:nel)
        call table_mat_vinterp(matparam%table(2),nel,nel,                      &
          vartmp(1:nel,2),xvec(1:nel,1),tmax_s(1:nel),dydx2(1:nel))
        !< Linear
        if (ievoshap == 1) then
          delta0s(1:nel) = tmax_s(1:nel) / e_elas_s
          utd(1:nel) = two*giic/tmax_s(1:nel)
        !< Exponential
        elseif (ievoshap == 2) then
          delta0s(1:nel) = tmax_s(1:nel) / e_elas_s
          coef0(1:nel) = giic - tmax_s(1:nel) * delta0s(1:nel) * half
          utd(1:nel) = delta0s(1:nel) + coef0(1:nel) /                         &
                          (func_alpha*tmax_s(1:nel))
        !< Xu-Needleman exponential
        elseif (ievoshap == 3) then
          delta0s(1:nel) = giic / (exp(one) * tmax_s(1:nel))
          utd(1:nel)     = -delta0s(1:nel) * log(em03)
        endif     
      else      
        delta0s(1:nel) = delta0s_cst
        utd(1:nel)     = utd_cst
        tmax_s(1:nel)  = t_s
      endif
      
      ! Compute DELTA0M (i.e. DELTA0 for the mix mode)
      where (epst(1:nel) == zero) 
        delta0m(1:nel)  = delta0n(1:nel)
        tmax_mix(1:nel) = tmax_n(1:nel)
        beta(1:nel)     = zero
      elsewhere (epsn(1:nel) == zero)
        delta0m(1:nel)  = delta0s(1:nel)
        tmax_mix(1:nel) = tmax_s(1:nel)
        beta(1:nel)     = zero
      elsewhere
        beta(1:nel)     = abs(epst(1:nel) / epsn(1:nel))
        delta0m(1:nel)  = delta0s(1:nel) * delta0n(1:nel) *                    &
                                       sqrt((one + beta(1:nel)**2) /           &
                                       ((delta0s(1:nel)**2) + (beta(1:nel) *   &
                                                      delta0n(1:nel))**2))
        tmax_mix(1:nel) = delta0m(1:nel)*(e_elas_n + e_elas_s*beta(1:nel)**2)/ &
                                                       (one + beta(1:nel)**2)
      endwhere
!      
      !< Compute DELTA0FMAX
      if (ievoshap == 3) then
        ! Xu-Needleman: delta_f = 9.21 * delta_p_mix (D_crit = 0.9999)
        deltafmax(1:nel) = -delta0m(1:nel) * log(em04)
      elseif (irupt == 2) then ! IRUPT =2  = BK method
        where (epst(1:nel) == zero) 
          deltafmax(1:nel) = und(1:nel)
        elsewhere (epsn(1:nel) == zero)
          deltafmax(1:nel) = utd(1:nel)
        elsewhere
          fac1(1:nel) = (e_elas_n**gama)/(one + beta(1:nel)**2)
          fac2(1:nel) = (e_elas_s**gama)*(beta(1:nel)**2)/(one + beta(1:nel)**2)
          fac3(1:nel) = (fac1(1:nel) + fac2(1:nel) )**(one/gama)
          deltafmax(1:nel) = two/(delta0m(1:nel)*  fac3(1:nel))*               &
                             (gic + (giic - gic)*((e_elas_s*beta(1:nel)**2)/   &
                             (e_elas_n + e_elas_s*beta(1:nel)**2))**abs(exp_bk))
        endwhere
      else ! irupt =1  = power law method-- default
        where (epst(1:nel) == zero)
          deltafmax(1:nel) = und(1:nel)
        elsewhere (epsn(1:nel) == zero)
          deltafmax(1:nel) = utd(1:nel)
        elsewhere
          fac1(1:nel) = two*((one + beta(1:nel)**2))/delta0m(1:nel)
          deltafmax(1:nel) = fac1(1:nel)*((e_elas_n/gic)**exp_g +              &
                  (e_elas_s*(beta(1:nel)**2)/giic)**exp_g)**(-one/exp_g)
        endwhere
      endif
!      
      !=========================================================================
      !< ENERGY COMPUTATION (ONLY FOR THE ENERGY-BASED CRITERION)
      !=========================================================================
      if (icrittyp == 2) then
        !< Compute the elastic-energy
        g0(1:nel) = half * tmax_mix(1:nel) * delta0m(1:nel)
        !< Compute the energy-increment with forward Euler
        dgi(1:nel) = zero
        if (isym == 1) then
          dgi(1:nel) = signzz(1:nel) * depszz(1:nel)
        else
          where (epszz(1:nel) > zero) 
            dgi(1:nel) = signzz(1:nel) * depszz(1:nel)
          endwhere
        endif
        !< Shear energy increment is always accumulated (as in the legacy version)
        dgii(1:nel) = signyz(1:nel)*depsyz(1:nel) + signzx(1:nel)*depszx(1:nel)
        !< Update the energy
        gn(1:nel) = max(gn(1:nel) + dgi(1:nel), zero)
        gs(1:nel) = max(gs(1:nel) + dgii(1:nel), zero)
        ! Compute the total energy
        gtot(1:nel) = gn(1:nel) + gs(1:nel)
        ! Compute RATIO 
        where (gtot(1:nel) > zero)
          ratio(1:nel) = gs(1:nel) / gtot(1:nel)
        elsewhere
          ratio(1:nel) = zero
        endwhere
        ratio(1:nel) = max(zero, min(one, ratio(1:nel))) 
        ! Critical energy - GC
        if (irupt == 1) then ! power law model
          gc(1:nel) = (((one - ratio(1:nel)) / gic) ** exp_g +                 &
                       (ratio(1:nel)/giic)**exp_g)**(-one/exp_g)
        else if (irupt == 2) then ! bk model
          gc(1:nel) = gic + (giic - gic) * (ratio(1:nel) ** exp_bk)
        else
          gc(1:nel) = gic
        endif
      endif
!
      !=======================================================================
      !< COMPUTE DAMAGE (2nd part of the Traction-separation curve
      !=======================================================================    
      if (icrittyp == 1) then ! Displacement-based
        dam(1:nel) = dmg(1:nel)
        if (ievoshap == 1) then
          where (epsmmax(1:nel) > delta0m(1:nel) .and. epsmmax(1:nel) /= zero)
            dam(1:nel) = (deltafmax(1:nel) / epsmmax(1:nel)) *                 &
                         (epsmmax(1:nel)   - delta0m(1:nel)) /                 &
                      max((deltafmax(1:nel) - delta0m(1:nel)), em20)
          endwhere
        elseif (ievoshap == 2) then
          where (epsmmax(1:nel) > delta0m(1:nel) .and. epsmmax(1:nel) /= zero)
            u_norm(1:nel) = (epsmmax(1:nel) - delta0m(1:nel)) /                &
                         max((deltafmax(1:nel) - delta0m(1:nel)), em20)
            dam(1:nel) = one - (delta0m(1:nel)/epsmmax(1:nel)) *               &
                        (exp(-alpha*u_norm(1:nel)) - exp_alpha) / denom1
          endwhere
        elseif (ievoshap == 3) then
          dam(1:nel) = one - exp(-epsmmax(1:nel)/max(delta0m(1:nel), em20))
        endif
        dmg(1:nel) = max(dmg(1:nel), dam(1:nel)) ! enforce damage monotonicity
        dmg(1:nel) = min(dmg(1:nel), one)        ! saturated at 1 by definition
        ! Deactivate EF (Displament Criterion)
        do i = 1, nel
          if (offl(i) == one.and.epsmmax(i) > deltafmax(i)) then
            nindxf = nindxf+1 ! count
            indxf(nindxf) = i
            offl(i) = zero
          endif
        enddo
      elseif (icrittyp == 2) then ! energy-based     
        d_f(1:nel) = two * gc(1:nel) / tmax_mix(1:nel) 
        dam(1:nel) = dmg(1:nel)
        if (ievoshap == 1) then
          where (epsmmax(1:nel) > delta0m(1:nel) .and. epsmmax(1:nel) /= zero)
            dam(1:nel) = (d_f(1:nel)/epsmmax(1:nel)) *                         &
                           (epsmmax(1:nel) - delta0m(1:nel)) /                 &
                        max((d_f(1:nel) - delta0m(1:nel)), em20)
          endwhere
        else if (ievoshap == 2) then ! exponential law - integration
          denom2(1:nel) = max(gc(1:nel) - g0(1:nel), em20)
          where (epsmmax(1:nel) > zero) 
            cos2(1:nel) = epsn(1:nel)**2 / epsmmax(1:nel)**2
            sin2(1:nel) = epst(1:nel)**2 / epsmmax(1:nel)**2
            e_elas(1:nel) = e_elas_n * cos2(1:nel) + e_elas_s * sin2(1:nel)
          elsewhere
            e_elas(1:nel) = e_elas_n
          endwhere
          ! Implicit ODE
          delta_prev(1:nel) = uvar(1:nel,4)
          ddelta(1:nel) = max(epsmmax(1:nel) - delta_prev(1:nel), zero)
          where (ddelta(1:nel) > zero .and. epsmmax(1:nel) > delta0m(1:nel))
            ! A = K * delta^(n+1) / (Gc^(n+1) - G0) * Ddelta
            a_coef(1:nel) = e_elas(1:nel) * epsmmax(1:nel) * ddelta(1:nel) / &
                            max(gc(1:nel) - g0(1:nel), em20)
            ! Schema implicite exact : D = (D^n + A) / (1 + A)
            dam(1:nel) = (dmg(1:nel) + a_coef(1:nel)) / (one + a_coef(1:nel))
          elsewhere
            dam(1:nel) = dmg(1:nel)
          endwhere
        endif
        dmg(1:nel) = max(dmg(1:nel), dam(1:nel))
        dmg(1:nel) = min(dmg(1:nel), one)   
        do i = 1, nel
          if (offl(i) == one .and. (gtot(i).ge.gc(i).or.dmg(i).ge.one)) then
            nindxf = nindxf + 1
            indxf(nindxf) = i
            offl(i) = zero
          endif 
        enddo
      endif ! icrittyp == 1 or 2
!
      !=========================================================================
      !< STRESS FIELD UPDATE
      !=========================================================================
      if (isym == 1) then
        ! symmetric: damage degrades both tension and compression
        signzz(1:nel)  = (one-dmg(1:nel))*e_elas_n*epszz(1:nel)
      else
        where (epszz(1:nel) < zero) 
          signzz(1:nel) = e_elas_n*epszz(1:nel) 
        elsewhere
          signzz(1:nel) = (one-dmg(1:nel))*e_elas_n*epszz(1:nel)
        endwhere
      endif
      signyz(1:nel) = (one-dmg(1:nel))*e_elas_s*epsyz(1:nel)
      signzx(1:nel) = (one-dmg(1:nel))*e_elas_s*epszx(1:nel)
!
      ! if (icrittyp == 2) then
      !   ! smooth c^1 fade-out near deletion (energy-based criterion)
      !   ! 3-2 hermite step on d in [d_fade=0.95, d_crit=0.999]
      !   where (dmg(1:nel) > zep95)
      !     xi(1:nel) = min(max((dmg(1:nel) - zep95) /                           &
      !                             (zep999 - zep95), zero), one)
      !     fade(1:nel) = one - xi(1:nel)*xi(1:nel)*(three - two*xi(1:nel))
      !     signzz(1:nel) = signzz(1:nel)*fade(1:nel)
      !     signyz(1:nel) = signyz(1:nel)*fade(1:nel)
      !     signzx(1:nel) = signzx(1:nel)*fade(1:nel)
      !   endwhere
      ! endif
!      
      !=========================================================================
      !< SAVE UVAR FIELDS
      !========================================================================= 
      uvar(1:nel,1)  = signzz(1:nel)  ! normal stress
      uvar(1:nel,2)  = signzx(1:nel)  ! stress x
      uvar(1:nel,3)  = signyz(1:nel)  ! stress y
      uvar(1:nel,4)  = epsmmax(1:nel) ! maximum strain (history)
      uvar(1:nel,6)  = gn(1:nel)      !
      uvar(1:nel,7)  = gs(1:nel)      !
      uvar(1:nel,8)  = delta0m(1:nel) ! delta_mix_0
      uvar(1:nel,9)  = beta(1:nel)    ! mixed-mode ratio
      uvar(1:nel,10) = deltafmax(1:nel) ! delta_f_max
      uvar(1:nel,11) = gc(1:nel)      !
!     
      if (idtmins==2 .and. jsms/=0) then
        ! dtb = (target dt / scaling factor)**2
        dtb = (dtmins/dtfacs)**2
        ! for each fe, scale mass by dmels(i) or half*dtb*stf*area(i)*off(i)
        ! off(i) is included to avoid scaling failed elements
        ! half*dtb*stf*area(i) unit: s2.n/m3 * m2 -> s2 n/m = mass
        dmels(1:nel) = max(dmels(1:nel),half*dtb*stf*area(1:nel)*off(1:nel))
      end if
!
      ! Update FE stiffness accounting for cohesive stiffness.
      stifm(1:nel) = stifm(1:nel) + stf*area(1:nel)*off(1:nel)    
!-----------------------------------------------------     
      !< If elements have failed, print a message.
      if (nindxf > 0) then
        do ii = 1,nindxf
          i = indxf(ii)
          write(iout ,1000) ngl(i),ipg,epsm(i)
          write(istdo,1100) ngl(i),ipg,epsm(i),time
        enddo
       endif
!-----------------------------------------------------      
 1000 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,                               &
                ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9)
 1100 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,                               &
                ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9,        &
                ' AT TIME ',1PE16.9)
!-----------------------------------------------------
      end subroutine sigeps117
      end module sigeps117_mod
          