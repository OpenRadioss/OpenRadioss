!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    sigeps123c_mod   ../engine/source/materials/mat/mat123/sigeps123c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F
      !||====================================================================
      module sigeps123c_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW123
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps123c         ../engine/source/materials/mat/mat123/sigeps123c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc             ../engine/source/materials/mat_share/mulawc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
         SUBROUTINE sigeps123c(&
           nel     ,mat_param  , nuvar   ,nvartmp   , uvar,        &
           vartmp  ,rho        ,thk      ,thkly     , shf ,        &
           area    , epsp      ,npg       ,time     ,npttot    ,   &
           epsxx   ,epsyy      ,epsxy    ,epsyz     ,epszx ,       &    
           depsxx, depsyy      ,depsxy   ,sigoxx    ,sigoyy  ,     &
           &sigoxy,                                                &
           signxx  ,signyy     ,signxy  ,signzx     ,signyz  ,     &
           off     ,offl       ,sigy       ,etse    ,ssp     ,     &
           dmg    ,dmg_g       ,ioff_duct    ) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use matparam_def_mod 
      use constant_mod 
      use precision_mod, only : WP 
      use table_mat_vinterp_inv_mod , only : table_mat_vinterp_inv
      use table_mat_vinterp_mod , only : table_mat_vinterp
      use analyze_failure_mod
      use analyze_failure_trial_mod
      use strainrate_dependency_mod
      
!! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include  "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables
          integer, intent(in) :: nvartmp !< number of user variables
          integer, intent(in) :: npg !< number of gauss points per element
          integer, intent(in) :: npttot !< total number of integration points
          integer, dimension(nel), intent(inout) :: ioff_duct !< ductility indicator

          real(kind=wp), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< user variables temporairy 
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          real(kind=wp), dimension(nel), intent(in) :: rho !< material density
          real(kind=wp), dimension(nel), intent(inout) :: sigy !< yield stress
          real(kind=wp), dimension(nel), intent(inout) :: shf !< shear factor correction 
          real(kind=wp), dimension(nel), intent(inout) :: thk !< shell thikness 
          real(kind=wp), dimension(nel), intent(in)    :: thkly !< ply thikness  
          real(kind=wp), dimension(nel), intent(inout) :: etse !< ratio of rigidity  
          real(kind=wp), dimension(nel), intent(in) :: epsxx !< total strain xx 
          real(kind=wp), dimension(nel), intent(in) :: epsyy !< total strain yy
          real(kind=wp), dimension(nel), intent(in) :: epsxy !< total strain xy 
          real(kind=wp), dimension(nel), intent(in) :: epsyz !< total strain yz 
          real(kind=wp), dimension(nel), intent(in) :: epszx !< total strain zx 
          real(kind=wp), dimension(nel), intent(in) :: depsxx !< strain increment xx
          real(kind=wp), dimension(nel), intent(in) :: depsyy !< strain increment yy
          real(kind=wp), dimension(nel), intent(in) :: depsxy !< strain increment xy

          real(kind=wp), dimension(nel), intent(in) :: sigoxx !< old stress xx 
          real(kind=wp), dimension(nel), intent(in) :: sigoyy !< old stress yy
          real(kind=wp), dimension(nel), intent(in) :: sigoxy !< old stress xy
          real(kind=wp), dimension(nel), intent(out) :: signxx !< new stress xx 
          real(kind=wp), dimension(nel), intent(out) :: signyy !< new stress yy
          real(kind=wp), dimension(nel), intent(out) :: signxy !< new stress xy 
          real(kind=wp), dimension(nel), intent(out) :: signyz !< new stress yz 
          real(kind=wp), dimension(nel), intent(out) :: signzx !< new stress zx 
          real(kind=wp), dimension(nel), intent(inout) :: ssp !< sound speed
          real(kind=wp), dimension(nel), intent(inout) :: off !< element deletion flag
          real(kind=wp), dimension(nel), intent(inout) :: offl !< local element deletion flag
          real(kind=wp), dimension(nel),intent(in) :: area !< element area
          real(kind=wp) ,DIMENSION(nel,8), intent(inout) :: dmg
          real(kind=wp) ,DIMENSION(nel), intent(inout) :: dmg_g
          real(kind=wp), intent(in) :: time !< current time
          real(kind=wp), dimension(nel), intent(in) :: epsp !< equiv strain rate
       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------          
      integer :: i,nfail,nkey,ipos1(1,1),ipos(nel,1),type,ndx,n,int_ratio,nfunc
      integer :: indx(nel),ndx_print,indx_print(nel)
      integer, parameter :: debug = 0  ! debug flag for checking failure plane
      !
      real(kind=wp) :: e1, e2, nu12, nu21, xt, xc,ratio
      real(kind=wp) :: yt, yc, sc, d, scale,eps_failure, eps_eq
      real(kind=wp) :: invd, aa, bb, phi0, beta,cos2p,cos2t,sin2p,sin2t,cosp,sinp,cost,sint
      real(kind=wp) :: g12, a11, g13, g23, dam, dfiber, dkink, dmat, dmac
      real(kind=wp) :: enkink, ena, enb, enl, ent, gammal
      real(kind=wp) :: sigma_a, sigma_b, tau_ab, tau_ca, tau_bc,sigma_c,sigma_a_m,sigma_b_m
      real(kind=wp) :: fkink, fmat, fmac, ffiber, mul, mut, twophi,theta
      real(kind=wp) :: sl, st, tau_t, tau_l, yld, phi, en_mat
      real(kind=wp) :: eps0, sig0, eps_mat, sig_mat, epsa, epsb, epsc
      real(kind=wp) :: epsn, gamal, gamat, epsf, gam_mat, gamab, gambc, gamca
      real(kind=wp) :: lamda, nu13, nu23, omega, psi,sigma_b_psi,sigma_c_psi
      real(kind=wp) :: sigma_c_phi, sigma_n_p,sigma_n,twopsi,bidon,eps_matf
      real(kind=wp) :: tau_bc_m, tau_bc_phi, tau_bc_psi, tau_ca_m, tau_ca_phi, tau_ca_psi
      real(kind=wp) :: tau_mat,tau_ab_psi,tau_ab_m ,thetai,tau_bm_cpsi,tau_cpsi_am
      real(kind=wp) :: gamai,eps_kink,sigb_psi,fac,gama_inel,xx(1,1),max_f,critical_phi
      real(kind=wp) :: c,s,c2,s2,cs,phi_deg,psi_deg,theta_deg, la_m,lb_psi,lc_psi,l_car
      real(kind=wp) :: denom,deint,eint,thetac,eps0_kink,sig0_kink,epsf_kink

      real(kind=wp), dimension(nel) ::  dezz,check
      real(kind=wp), dimension(nel) ::  epsfc1,epsfc2,epsf12,epsf13, yy,dydx 
      real(kind=wp), dimension(nel) ::  xt_1, xc_1, yt_1, yc_1, sl_1
      real(kind=wp), dimension(nel) ::  thetai_1,mul_1,st_1
      real(kind=wp), dimension(nel) ::  enkink_1, ena_1, enb_1, ent_1, enl_1
      real(kind=wp), dimension(nel,1) :: xvec
!!======================================================================
          ! Material parameters
       e1    = mat_param%uparam(1)  
       e2    = mat_param%uparam(2)  
       g12   = mat_param%uparam(4)  
       g13   = mat_param%uparam(5) 
       g23   = mat_param%uparam(6)  
       nu12  = mat_param%uparam(7) 
       nu21  = mat_param%uparam(8) 
       nu13  = mat_param%uparam(9)
       nu23 = mat_param%uparam(11)
       !! nu32 = mat_param%uparam(12)
      ! strengh direction
       xt_1(1:nel)     = mat_param%uparam(13) 
       xc_1(1:nel)     = mat_param%uparam(14)
       yt_1(1:nel)     = mat_param%uparam(15) 
       yc_1(1:nel)     = mat_param%uparam(16) 
       sl_1(1:nel)     = mat_param%uparam(17)
        !
       enkink_1(1:nel)  = mat_param%uparam(18) 
       ena_1(1:nel)     = mat_param%uparam(19)
       enb_1(1:nel)     = mat_param%uparam(20) 
       ent_1(1:nel)     = mat_param%uparam(21) 
       enl_1(1:nel)     = mat_param%uparam(22)
       !
       st_1(1:nel)      = mat_param%uparam(23)  
       mut              = mat_param%uparam(24)  
       mul_1(1:nel)     = mat_param%uparam(25)
      !
       phi0    = mat_param%uparam(26)  
       thetac = mat_param%uparam(27)
       thetai_1(1:nel)  = mat_param%uparam(28)
       yld     = mat_param%uparam(29)  
       beta    = mat_param%uparam(30)  
       ratio   = mat_param%uparam(37)
       eps_failure = mat_param%uparam(38)

       ioff_duct(1:nel) = one
       !
       if(time == zero) uvar(1:nel,16) = sqrt(npg*area(1:nel))  ! initial characteristic length 
       ! plane shear behavior 
       ipos(:,1) = vartmp(:,1)
       do i=1,nel
        xvec(i,1)  = abs(epsxy(i))
        uvar(i,11)  = max(xvec(i,1), uvar(i,11)) ! epsxy
       enddo 
        !
       call table_mat_vinterp(mat_param%table(1),nel,nel,ipos,xvec,yy,dydx)
        ! 
       vartmp(1:nel,1) = ipos(1:nel,1)
       do i= 1,nel
         if(xvec(i,1) == uvar(i,11) ) then
             signxy(i) = yy(i)*epsxy(i)/max(em20, xvec(i,1))  
             gama_inel = uvar(1,11) - yy(i)/g12
             uvar(i,12) = gama_inel
         else
            gama_inel = uvar(i,12)
            fac = epsxy(i)/max(em20, xvec(i,1))
            signxy(i) = g12*fac*(xvec(i,1) -  gama_inel)
         endif
       enddo
          ! element deletion check
       ndx = 0
       indx(:)=0
       do i=1,nel
          eps_eq =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2  ) 
          eps_eq = sqrt(eps_eq)
        if(off(i) >= one) then
          if((dmg(i,1) >= zep99 .or. eps_eq >= eps_failure ).and. offl(i) == one)  then
           dmg_g(i) = dmg_g(i) + 1/npttot
           offl(i ) = zero  ! local deletion stress set to zero
           int_ratio = int(dmg_g(i))
           off(i) = zero
         endif  
        endif  
       enddo
       do i=1,nel
          if(off(i) == zero .or. offl(i) == zero ) then
            signxx(i) = zero
            signyy(i) = zero
            signxy(i) = zero
            signyz(i) = zero
            signzx(i) = zero
          else 
            ndx = ndx + 1
            indx(ndx) = i
         endif
       end do
       !------------------------------------------------------------------
       ! strain rate dependency xt,xc,yc,yt,enkink,enb;ena,ent,enl
       !-------------------------------------------------------------------
      if(ndx == 0) return
       nfunc = mat_param%ntable 
      if(nfunc > 1) call strainrate_dependency(nel, mat_param , epsp, vartmp, nvartmp ,  &
                                               xt_1,    xc_1,  yt_1,    yc_1,  sl_1, &
                                               enkink_1, ena_1, enb_1,ent_1, enl_1,&
                                               mul_1, st_1, thetai_1) 
      do n =1,ndx
            i = indx(n)
            ! retrieving material parameters 
            xt = xt_1(i)
            xc = xc_1(i)
            yt = yt_1(i)
            yc = yc_1(i)
            sl = sl_1(i)
            enkink = enkink_1(i)
            ena    = ena_1(i)
            enb    = enb_1(i)
            ent    = ent_1(i)
            enl    = enl_1(i)
            !
            st = st_1(i)
            mul = mul_1(i)
            thetai = thetai_1(i)
            ! initialization of damage viriable
            check(i) = zero
             dfiber  = dmg(i,2)
             dkink   = dmg(i,3)
             dmat    = dmg(i,4)
             max_f   = zero
             ! computing undamaged stress
             d = (one - nu12*nu21)
             invd = one/d
             ! total effective stress
             sigma_a   =   invd*(e1*epsxx(i) + nu21*e1*epsyy(i))
             sigma_b   =   invd*(nu12*e2*epsxx(i)+ e2*epsyy(i))
             tau_ab    =   signxy(i) ! g12*epsxy(i)
             tau_ca    =   shf(i)*g13*epszx(i) 
             tau_bc    =   shf(i)*g23*epsyz(i) 
             sigma_c   = zero  ! normal condition 
             ! checking loading and unloading
             deint = half*(depsxx(i)*(sigma_a + sigoxx(i))  +   &
                           depsyy(i)*(sigma_b  + sigoyy(i)) +   &
                           depsxy(i)*(tau_ab  + sigoxy(i))) 
             eint = uvar(i,1) + deint
             uvar(i,1) = eint
             if(deint < ZERO ) then
              check(i) = -one
             else
              check(i) = one
             end if
             ! total strain 
             epsa  = epsxx(i)
             epsb  = epsyy(i)
             gamab = epsxy(i)
             gamca = epszx(i)
             gambc = epsyz(i)
             ! computing ezz ! under normal plane stress condition
             epsc  = -nu13*epsxx(i) - nu23*epsyy(i)
             ! thickness change
             dezz(i)    = -nu13*depsxx(i) - nu23*depsyy(i)
             thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i) 
             if( check(i) >= zero ) then
                ! loading 
             ! Fiber  check failure  ! dfiber
              if(dmg(i,5) == one .and. dfiber < one ) then  ! fiber failure in tension
                sig0 = uvar(i,2)
                eps0 = uvar(i,3)
                epsf = uvar(i,4) !! epsf = two*ena/sig0/aldt(i)
                dfiber = epsf*(epsxx(i) - eps0)/epsxx(i)/(epsf - eps0)
                dfiber = max(dfiber, dmg(i,2)) 
                dfiber = min(one, dfiber )
                dmg(i,2) = dfiber
              elseif(sigma_a >= xt .and. dmg(i,2) == zero ) then
                  sig0= sigma_a
                  eps0 = epsxx(i) ! epsa 
                  l_car = uvar(i,16)
                  epsf =  two*ena/sig0/l_car
                  if(epsf < eps0 ) epsf = onep1*eps0
                  uvar(i,2) =  sig0  ! onset stress
                  uvar(i,3) =  eps0  ! onset strain
                  uvar(i,4) = epsf
                  dmg(i,5)  = one 
              endif
             ! Kinking  check of failure 
              if( dmg(i,6) == one .and. dkink < one  ) then ! Kinking failure
                  ! computing eps_kink ! 
                  ! rotation in plan (b,c)  (angle psi) ===> (b_psi, c_psi) frame
                    psi = uvar(i,13)
                    c = cos(psi)
                    s = sin(psi)
                    c2 = c*c
                    s2 = s*s
                    cs = c*s
                  ! =============================================
                  ! Stress Transformation in (b,c) Plane
                  ! =============================================
                    sigma_b_psi = c2 * sigma_b  + two * cs * tau_bc
                    sigma_c_psi = s2 * sigma_b  - two * cs * tau_bc
                    tau_bc_psi = -sigma_b * cs + (c2 - s2) * tau_bc
                    tau_ab_psi = c * tau_ab + s * tau_ca
                    tau_ca_psi = -s * tau_ab + c * tau_ca
                   ! ===============================================================================
                   ! rotation of plane (a,b_psi) - with thetai (misalignment angle) ===>(am, bm)
                   !  theta  = sign(one, tau_ab_psi) *(thetai + gamai) ! 
                   ! ===============================================================================
                    ! Precompute trigonometric terms
                     theta = uvar(i,14) 
                     c = cos(theta)
                     s = sin(theta)
                     c2 = c*c
                     s2 = s*s
                     cs = c*s
                     tau_ab_m = (sigma_b_psi - sigma_a) * cs + (c2 - s2) * tau_ab_psi
                     tau_ab_m = abs(tau_ab_m)
                     eps_kink= tau_ab_m/g12
                     sig0 = uvar(i,5)  ! onset kingking stress 
                     eps0 = uvar(i,6)  ! onset kinking strain
                     epsf = uvar(i,7)  !! final strain : epsf = two*enkink/sig0/aldt(i)
                     dkink=  epsf*(abs(eps_kink) - eps0)/abs(eps_kink)/(epsf - eps0)  ! dkink
                     dkink = max(dmg(i,3), dkink)
                     dkink = min(dkink, one)
                     dmg(i,3) = dkink 
                 elseif(sigma_a < zero .and. dmg(i,6) == zero ) then
                  !!=========================================================================
                  ! Step 1: Calculate kink band orientation angle psi
                  ! rotation in plan (b,c)  (angle psi) ===> (b_psi, c_psi) frame
                  !!=========================================================================
                   psi = zero
                   if (abs(sigma_b) > em10) then
                       psi = half*atan2(two* tau_bc, sigma_b)
                   elseif(abs(sigma_b) /= zero .and. abs(tau_bc) > em10) then
                       psi = half*pi
                   endif 
                   !================================= 
                   ! Precompute trigonometric terms
                   !=================================
                    c = cos(psi)
                    s = sin(psi)
                    c2 = c*c
                    s2 = s*s
                    cs = c*s
                  ! =============================================
                  ! Stress Transformation in (b,c) Plane
                  ! =============================================
                    sigma_b_psi = c2 * sigma_b  + two * cs * tau_bc
                    sigma_c_psi = s2 * sigma_b  - two * cs * tau_bc
                    tau_bc_psi =  - sigma_b * cs + (c2 - s2) * tau_bc
                    tau_ab_psi = c * tau_ab + s * tau_ca
                    tau_ca_psi = -s * tau_ab + c * tau_ca
                    !=======================================
                    ! computing gamai from the curve
                    !=======================================
                     xvec(1,1) = abs(half*(sigma_a - sigma_b_psi)*sin(two*thetai) + abs(tau_ab_psi)*cos(two*thetai))
                     ipos(1,1)= 1
                     call table_mat_vinterp_inv(mat_param%table(1),1,1,ipos(1,1),xvec,yy,dydx)
                     gamai = yy(1)
                     theta  = sign(one, tau_ab_psi)*(thetai + gamai)
                    !=====================================================================
                    ! step 2 : rotation of plane (a,b_psi) - with theta (misallignement angle) ===> 
                    ! ============================================================================
                     ! Precompute trigonometric terms
                     c = cos(theta)
                     s = sin(theta)
                     c2 = c*c
                     s2 = s*s
                     cs = c*s
                     ! =============================================
                     ! Stress Transformation in (am,bm) Plane
                     ! =============================================
                     sigma_a_m = c2 * sigma_a + s2 * sigma_b_psi + two * cs * tau_ab_psi
                     sigma_b_m = s2 * sigma_a + c2 * sigma_b_psi - two * cs * tau_ab_psi
                     tau_ab_m = (sigma_b_psi - sigma_a) * cs + (c2 - s2) * tau_ab_psi
                     tau_cpsi_am = c * tau_ca_psi + s * tau_bc_psi
                     tau_bm_cpsi = -s * tau_ca_psi + c * tau_bc_psi
                    ! ============================================================================                    
                    ! Step 3 :  Step 3: Search for critical fracture plane angle phi
                    !                    Angle of Kinking plane phi 
                    ! rotation of plane (bm ,c_psi) - with phi ===> (b_m, c_phi)
                    ! ============================================================================
                     type = 1 ! kinking
                     if(debug == 0) then
                        call   analyze_failure(st, sl, yt, mut, muL, type, &
                                                  sigma_b_m, sigma_c_psi, tau_bm_cpsi, tau_ab_m, tau_cpsi_am, &
                                                  critical_phi, max_f)
                     else
                        call   analyze_failure_trial(st, sl, yt, mut, muL, type, &
                                                  sigma_b_m, sigma_c_psi, tau_bm_cpsi, tau_ab_m, tau_cpsi_am, &
                                                  critical_phi, max_f)
                     endif                            
                     phi = critical_phi
                     if(max_f >= one) then
                        tau_ab_m =  abs(tau_ab_m)
                        uvar(i,13) = psi
                        uvar(i,14) = theta
                        psi_deg = psi*hundred80/pi
                        if(abs(psi_deg) <= ten) then ! not used 
                            lb_psi= uvar(i,16) /cos(psi)
                          else
                            lb_psi = min(uvar(i,16) /cos(psi),ten/sin(psi))
                        endif
                        theta_deg = theta*hundred80/pi
                        if(abs(theta_deg) <= ten) then ! not used 
                            la_m = uvar(i,16)/cos(theta)
                        else
                            la_m = min(uvar(i,16)/cos(theta),lb_psi/sin(theta))
                        endif
                        l_car = uvar(i,16)
                        sig0_kink  = tau_ab_m  ! onset kinking stress
                        eps0_kink  = tau_ab_m/g12  ! onset kinking strain
                        epsf_kink  = two*enkink/max(em20,tau_ab_m)/l_car ! final kinking strain
                        if(epsf_kink < eps0_kink) epsf_kink = onep1*eps0_kink
                        uvar(i,5) = sig0_kink
                        uvar(i,6) = eps0_kink
                        uvar(i,7) = epsf_kink
                        dmg(i,6) = one
                      endif
                 end if      
                ! matrix failure 
                if( dmg(i,7) == one .and. dmat < one  ) then
                   sig0  = uvar(i,8) 
                   eps0  = uvar(i,9) 
                   eps_matf = uvar(i,10)
                   ! =========================================================
                   ! Step 3: Calculate driving strain for damage if fracture plane is known
                    ! =========================================================
                      phi = uvar(i,15) ! saved fracture plane angle
                      cos2p = cos(two*phi)
                      sin2p = sin(two*phi)
                      cosp  = cos(phi)
                      sinp  = sin(phi)
                      aa    = half*sigma_b
                      sigma_n   = aa + aa*cos2p  + tau_bc*sin2p
                      tau_t     = -aa*sin2p    +  tau_bc*cos2p
                      tau_l     = tau_ab*cosp  +  tau_ca*sinp
                      tau_mat   = sqrt(tau_t**2 + tau_l**2)
                      sigma_n_p = max(sigma_n, zero)
                      sig_mat   = sqrt(sigma_n_p**2 + tau_mat**2)  
                    ! ===========================================================
                    ! Calculate elastic strain components on fracture plane 
                    !===========================================================  
                      epsn      = half*(epsb + epsc  + (epsb- epsc )*cos(two*phi) + gambc*sin2p )
                      gamat     = -(epsb - epsc)*sin2p   +   gambc*sin2p
                      gamal     =  gamab*cosp + gamca*sinp
                      !!gammal_el = gamma_ab * cosp + gamma_ca * sinp
                      !===========================================================
                      ! Calculate angles lambda and omega (for Eq. 29)
                      !===========================================================
                      omega = zero
                      lamda = zero
                      if (tau_mat > em10) then
                           omega =  atan2(sigma_n_p, tau_mat) ! Only positive sigma_n
                      elseif(tau_mat > zero .and. sigma_n_p > em10) then
                          omega = half*pi
                      endif
                      if(abs(tau_t) > em10 ) then
                           lamda = atan2(tau_l, tau_t)
                      elseif( abs(tau_t) > zero .and. abs(tau_l) > em10) then
                           lamda = half*pi
                      endif
                      gam_mat = abs(gamat*cos(lamda) + gamal*sin(lamda))
                      eps_mat = sigma_n_p*epsn*sin(omega)/sigma_n + gam_mat*cos(omega)
                      en_mat = enb*(sigma_n_p/sig0)**2 + ent*(tau_t/sig0)**2 + enl*(tau_l/sig0)**2  ! coupling (i)
                      dmat = eps_matf*(eps_mat - eps0)/eps_mat/(eps_matf - eps0)
                      dmat = max(dmat, dmg(i,4)) 
                      dmat = min(one,dmat)
                      dmg(i,4) = dmat 
                  elseif(dmg(i,7) == zero)then
                   type = 2
                   ! =============================================================
                   ! Step 1: Search for critical fracture plane
                   ! =============================================================
                   sigma_c = zero ! plane stress condition !
                   if(debug == 0) then
                      call  analyze_failure(st, sl, yt, mut, muL, type, &
                                                  sigma_b, sigma_c,tau_bc, tau_ab, tau_ca, &
                                                  critical_phi, max_f )
                    else
                      call   analyze_failure_trial(st, sl, yt, mut, muL, type, &
                                                  sigma_b, sigma_c, tau_bc, tau_ab, tau_ca, &
                                                  critical_phi, max_f)
                    endif                              
                    phi = critical_phi
                    if(max_f >= one ) then
                     ! =========================================================
                     ! Step 3: Calculate driving strain for damage if fracture plane is known
                     ! =========================================================
                      cos2p = cos(two*phi)
                      sin2p = sin(two*phi)
                      cosp  = cos(phi)
                      sinp  = sin(phi)
                      aa    = half*sigma_b
                      sigma_n   = aa + aa*cos2p  + tau_bc*sin2p
                      tau_t     = -aa*sin2p    +  tau_bc*cos2p
                      tau_l     = tau_ab*cosp  +  tau_ca*sinp
                      tau_mat   = sqrt(tau_t**2 + tau_l**2)
                      sigma_n_p = max(sigma_n, zero)
                      sig_mat   = sqrt(sigma_n_p**2 + tau_mat**2)
                      
                    ! ===========================================================
                    ! Calculate elastic strain components on fracture plane 
                    !===========================================================  
                      epsn      = half*(epsb + epsc  + (epsb- epsc )*cos(two*phi) + gambc*sin2p )
                      gamat     = -(epsb - epsc)*sin2p   +   gambc*sin2p
                      gamal     = gamab*cosp + gamca*sinp
                      !!gammal_el = gamma_ab * cosp + gamma_ca * sinp
                      !===========================================================
                      ! Calculate angles lambda and omega (for Eq. 29)
                      !===========================================================
                      omega = zero
                      lamda = zero
                      if (tau_mat > 1.0d-10) then
                           omega =  atan2(sigma_n_p, tau_mat) ! Only positive sigma_n
                      elseif(tau_mat < em10 .and. sigma_n_p > em10) then
                          omega = half*pi
                      endif
                      if(abs(tau_t) > em10 ) then
                           lamda = atan2(tau_l, tau_t)
                      elseif( abs(tau_t) > zero .and. abs(tau_l) > em10) then
                           lamda = half*pi
                      endif
                      gam_mat = abs(gamat*cos(lamda) + gamal*sin(lamda))
                      !!gam_mat = abs(gamat*cos(lamda) + gammal_el*sin(lamda))
                      eps_mat = sigma_n_p*epsn*sin(omega)/sigma_n + gam_mat*cos(omega)
                      ! =============================================
                      ! MIXED-MODE FRACTURE TOUGHNESS CALCULATION
                      ! =============================================
                      en_mat = enb*(sigma_n_p/sig_mat)**2 + ent*(tau_t/sig_mat)**2 + enl*(tau_l/sig_mat)**2  ! coupling
                      phi_deg = phi*hundred80/pi
                      if(phi_deg <= ten) then
                         l_car = l_car/cos(phi)
                      else
                         l_car = min(l_car/cos(phi),l_car/sin(phi))
                      endif
                      l_car = uvar(i,16)
                      eps_matf = two*max(en_mat,enb)/sig_mat/l_car  
                      if(eps_matf < eps_mat) eps_matf =two* eps_mat
                      uvar(i,8) = sig_mat
                      uvar(i,9) = eps_mat
                      uvar(i,10) = eps_matf
                      uvar(i,15) = critical_phi
                      dmg(i,7) = one  
                    endif  
                endif ! 
              endif  
              ! damage appliction 
              dmg(i,1) = max(dfiber, dkink, dmat)  
             if(dfiber > zero .or. dkink > zero .or. dmat > zero) then
               dam = one - max(dfiber, dkink,dmat)
               dam = max (em02, dam)
               signxx(i) = dam*sigma_a
               signyy(i) = dam*sigma_b
               signxy(i) = dam*tau_ab
               signzx(i) = dam*tau_ca
               signyz(i) = dam*tau_bc         
             else
               signxx(i) =  sigma_a
               signyy(i) =  sigma_b
               signxy(i) =  tau_ab
               signzx(i) =  tau_ca
               signyz(i) =  tau_bc 
             endif 
            !
              a11       = max(e1,e2)/(one - nu12**2) 
              a11       = max(e1,e2)
              ssp(i) = sqrt(a11/rho(i))
              etse(i) = one
              sigy(i) = yld
            enddo ! nel loop
!-------------------------------------------------------------------------------------------
         end subroutine sigeps123c
      end module sigeps123c_mod 
