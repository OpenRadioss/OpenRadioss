!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
      !||    sigeps132c_mod   ../engine/source/materials/mat/mat132/sigeps132c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F
      !||====================================================================
      module sigeps132c_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW132
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps132c         ../engine/source/materials/mat/mat132/sigeps132c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc             ../engine/source/materials/mat_share/mulawc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
         SUBROUTINE sigeps132c(&
           nel     ,mat_param  , nuvar   ,nvartmp   , uvar,        &
           vartmp  ,rho        ,thk      ,thkly     , shf ,        &
           area    , epsp      ,npg       ,time     ,npttot    ,   &
           epsxx   ,epsyy      ,epsxy    ,epsyz     ,epszx ,       &    
           depsxx, depsyy      ,depsxy   ,sigoxx    ,sigoyy  ,     &
           &sigoxy,                                                &
           signxx  ,signyy     ,signxy  ,signzx     ,signyz  ,     &
           off     ,offl       ,sigy       ,etse    ,ssp     ,     &
           dmg    ,dmg_g       ,ioff_duct  ) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use matparam_def_mod 
      use constant_mod 
      use precision_mod, only : WP 
      use table_mat_vinterp_inv_mod , only : table_mat_vinterp_inv
      use table_mat_vinterp_mod , only : table_mat_vinterp
      use rate_dependency_parameters_mod
      
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
          !
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
      integer :: i,ipos1(1,1),ipos(nel,1),ndx,n,int_ratio,nfunc,ndel_ply
      integer :: indx(nel),ndx_print,indx_print(nel)
      !
      real(kind=wp) :: e1, e2, nu12, nu21, xt, xc,ratio,xc0,xt0
      real(kind=wp) :: d11, d22, d12,d66, inv_det,det,nu12_dam
      real(kind=wp) :: yt, yc,g12, a11, g13, g23,nu21_dam
      real(kind=wp) :: gxt, gxc, gyt, gyc, gsl,gxt0,gxc0
      real(kind=wp) :: dam(7),r(5),phi(4),d_active(6)
      real(kind=wp) :: sigma_a, sigma_b, tau_ab, tau_ca, tau_bc
      real(kind=wp) ::  mul, mut, theta, cos_theta, sin_theta
      real(kind=wp) :: sl, st, tau_t, tau_l,sigma_bm, sigma_ab_m
      real(kind=wp) :: lamda, nu13, nu23,cf23,cf31,cf12
      real(kind=wp) :: deint,eint,thetac,eps0,alpha,cos_alpha,sin_alpha
      real(kind=wp) :: epsf,eps0_p,eps_p,eps_f,d_po,beta
      real(kind=wp) :: g1p_vol, g1n_vol, g2p_vol, g2n_vol, g6_vol
      real(kind=wp) :: g1p0_vol, g1n0_vol,ratio_ndelply
      real(kind=wp) :: l_char,etan,phi_c, g_ratio,eta_l,eta_t
      real(kind=wp) :: fac, gama_inel,cos2_phi,sin2_phi,sin2phi,cos_phi,sin_phi
      real(kind=wp) :: e1_dam,e2_dam,g12_dam,eps_eq
      real(kind=wp) :: ef11c, ef11t, ef22c, ef22t, ef12, ef31, ef23
      real(kind=wp) :: tsmd23, tsmd31, epsf23, epsr23, epsf31, epsr31
      real(kind=wp) :: epsa,epsb,epsc,gamab,gamca,gambc,phi0,tmp1,tmp2
      real(kind=wp) :: gama0,gamaf,sig0,dam_shear,tau0,gama_ab_m,k1
      real(kind=wp) :: sigyld,dam_p, dam_2


      real(kind=wp), dimension(nel) ::  dezz,check,yld
      real(kind=wp), dimension(nel) ::  yy,dydx 
      real(kind=wp), dimension(nel) ::  xt_1, xc_1, yt_1, yc_1, sl_1,  &
                                        xt0_1,xc0_1,eta_l_1
      real(kind=wp), dimension(nel) ::  mul_1,st_1,phic_1,g_ratio_1
      real(kind=wp), dimension(nel) ::  gxt_1, gxc_1, gyt_1, gyc_1, gsl_1 ,&
                                        gxt0_1,gxc0_1
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
       xt_1(1:nel)     = mat_param%uparam(13) ! initial strengh fiber tension
       xc_1(1:nel)     = mat_param%uparam(14) ! initial strengh fiber compression
       yt_1(1:nel)     = mat_param%uparam(15) ! initial strengh transverse tension
       yc_1(1:nel)     = mat_param%uparam(16)  ! initial strengh transverse compression   
       sl_1(1:nel)     = mat_param%uparam(17)  ! initial shear strengh
       xt0_1(1:nel)    = mat_param%uparam(18)  ! initial strengh tension fiber for bilinear damage
       xc0_1(1:nel)    = mat_param%uparam(19)  ! initial strengh compression fiber for bilinear damage
       !
       gxt_1(1:nel)     = mat_param%uparam(20) 
       gxc_1(1:nel)     = mat_param%uparam(21)
       gyt_1(1:nel)     = mat_param%uparam(22) 
       gyc_1(1:nel)     = mat_param%uparam(23) 
       gsl_1(1:nel)     = mat_param%uparam(24)
       gxt0_1(1:nel)    = mat_param%uparam(25)
       gxc0_1(1:nel)    = mat_param%uparam(26)
       !
       phi0    = mat_param%uparam(27)
       sigyld  = mat_param%uparam(28)  
       beta    = mat_param%uparam(29)  
       etan    = mat_param%uparam(30)
       !
       epsf23   = mat_param%uparam(37)
       epsr23   = mat_param%uparam(38)
       tsmd23   = mat_param%uparam(39)
       epsf31   = mat_param%uparam(40)
       epsr31   = mat_param%uparam(41)
       tsmd31   = mat_param%uparam(42)
       !
       ef11t = mat_param%uparam(43)
       ef11c = mat_param%uparam(44)
       ef22t = mat_param%uparam(45)
       ef22c = mat_param%uparam(46)
       ef12  = mat_param%uparam(47)
       ef31  = mat_param%uparam(48)
       ef23  = mat_param%uparam(49) 
       !
       cf12  = mat_param%uparam(50)
       cf31  = mat_param%uparam(51)
       cf23  = mat_param%uparam(52)
       !
       eta_l_1(1:nel)  = mat_param%uparam(53) 
       eta_t           = mat_param%uparam(54)
       st_1(1:nel)     = mat_param%uparam(55) 
       phic_1(1:nel)   = mat_param%uparam(56) ! misalignment angle at fiber compression
       g_ratio_1(1:nel) = mat_param%uparam(57) ! GII/GI strengh ratio
       ratio   = mat_param%uparam(58 ) 
       !
       inv_det = one/(one - nu12*nu21)
       ioff_duct(1:nel) = one
       !
       alpha =  53*pi/HUNDRED80 ! phi0
       sin_alpha = sin(alpha)
       cos_alpha = cos(alpha)
       !
       if(time == zero) uvar(1:nel,5) = sqrt(npg*area(1:nel))  ! initial characteristic length 
       ! plane shear behavior 
       if(mat_param%ntable > 0) then
          ipos(:,1) = vartmp(:,1)
          do i=1,nel
            xvec(i,1)  = abs(epsxy(i))
            uvar(i,6)  = max(xvec(i,1), uvar(i,6)) ! epsxy
          enddo 
          call table_mat_vinterp(mat_param%table(1),nel,nel,ipos,xvec,yld,dydx)
        ! 
          vartmp(1:nel,1) = ipos(1:nel,1)
          do i= 1,nel
             if(xvec(i,1) == uvar(i,6) ) then
                 signxy(i) = yld(i)*epsxy(i)/max(em20, xvec(i,1))  
                 gama_inel = uvar(i,6) - yld(i)/g12
                 uvar(i,7) = gama_inel
             else
                gama_inel = uvar(i,7)
                fac = epsxy(i)/max(em20, xvec(i,1))
                signxy(i) = g12*fac*(xvec(i,1) -  gama_inel)
             endif
           enddo
       else
             yld(1:nel) = sigyld + etan*uvar(1:nel,7) !  sigyld + etan*abs(depsxy(1:nel))
             signxy(1:nel) = g12*epsxy(1:nel)
             do i=1,nel
                if( abs(signxy(i)) >= yld(i) ) then
                   signxy(i) = signxy(i)*yld(i)/abs(signxy(i))
                   gama_inel = abs(epsxy(i)) - yld(i)/g12
                   uvar(i,7) = gama_inel
                end if
             end do
       endif ! function for shear plan 
          ! element deletion check
       ndx = 0
       indx(:)=0
       do i=1,nel
           if(offl(i) == one) then
               if(epsxx(i) >= ef11t .or. epsxx(i) <= -ef11c .or.       &
                  epsyy(i) >= ef22t .or. epsyy(i) <= -ef22c .or.       &
                  abs(epsxy(i)) >= ef12 .or. abs(epsyz(i)) >= ef23 .or.   &
                  abs(epszx(i)) >= ef31 .or. dmg(i,2) >= zep99 .or. dmg(i,3) >= zep99 .or. &
                   dmg(i,4) >= zep99 .or. dmg(i,5) >=zep99 ) then
                   offl(i) = zero
                   dmg_g(i) = dmg_g(i) + one/npttot 
                   ratio_ndelply = dmg_g(i) ! could be used if we delete element if all ply are off.
                   off(i) = zero
               end if
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
       
      if(nfunc > 1) call rate_dependency_parameters(nel, mat_param , epsp, vartmp, nvartmp ,  &
                                                         xt_1 ,    xc_1,  yt_1,    yc_1 ,  sl_1, &
                                                         gxt_1,   gxc_1,  gyt_1,   gyc_1, gsl_1, &
                                                         xt0_1,  xc0_1, gxt0_1, gxc0_1 ,&
                                                         eta_l_1 , st_1 , phic_1 , g_ratio_1)  
      do n =1,ndx
            i = indx(n)
            ! retrieving material parameters 
            xt = xt_1(i)
            xc = xc_1(i)
            yt = yt_1(i)
            yc = yc_1(i)
            sl = sl_1(i)
            xt0 = xt0_1(i)
            xc0 = xc0_1(i)
            !
            gxt    = gxt_1(i)
            gxc    = gxc_1(i)
            gyt    = gyt_1(i)
            gyc    = gyc_1(i)
            gsl    = gsl_1(i)
            gxt0   = gxt0_1(i)
            gxc0   = gxc0_1(i)
            !
            eta_l = eta_l_1(i)  
            st = st_1(i)     
            phi_c = phic_1(i)   
            g_ratio = g_ratio_1(i)
             check(i) = zero
            ! ========================================================
            ! 3. GET STATE VARIABLES FROM PREVIOUS STEP
            ! ========================================================
            r(1) = uvar(i,1)  ! r_1+ old 
            r(2) = uvar(i,2)  ! r_1- old
            r(3) = uvar(i,3)  ! r_2+ old
            r(4) = uvar(i,4)  ! r_2- old
            !
            dam(1) = dmg(i, 2)  ! d_1+
            dam(2) = dmg(i, 3)  ! d_1-
            dam(3) = dmg(i, 4)  ! d_2+
            dam(4) = dmg(i, 5)  ! d_2-
            dam(5) = dmg(i, 6)   ! d_6
            l_char = uvar(i,5)  ! characteristic length
             ! ========================================================
            ! 4. CALCULATE EFFECTIVE STRESS (UNDAMAGED)
            ! ========================================================
            ! Build undamaged compliance matrix H0 (plane stress)
            ! Using Eq. 5 with d1=d2=d6=0
            ! Calculate effective stress σ̃ = H0⁻¹ : ε
            ! For plane stres s(σ33 = 0), we have:
            ! ε11 = (σ̃11/E1) - (ν12*σ̃22/E1) + α11ΔT + β11ΔM
            ! ε22 = - (ν21*σ̃11/E2) + (σ̃22/E2) + α22ΔT + β22ΔM
            ! γ12 = σ̃12/G12
            sigma_a   =   inv_det*(e1*epsxx(i)     + nu21*e1*epsyy(i))
            sigma_b   =   inv_det*(nu12*e2*epsxx(i)+ e2*epsyy(i))
            tau_ab    =   signxy(i) ! g12*epsxy(i)  ! sugnxy using function ?
            tau_ca    =   shf(i)*g13*epszx(i) 
            tau_bc    =   shf(i)*g23*epsyz(i) 
            !!sigma_c   = zero  ! normal condition 
             ! checking loading and unloading
             deint = half*(depsxx(i)*(sigma_a + sigoxx(i))  +   &
                           depsyy(i)*(sigma_b  + sigoyy(i)) +   &
                           depsxy(i)*(tau_ab  + sigoxy(i))) 
             eint = uvar(i,8) + deint
             uvar(i,8) = eint
             if(deint < zero ) then
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
           !!  if( check(i) >= zero ) then
              ! ========================================================
              ! 5. calculate loading functions φ (larc criteria)
              ! ========================================================
               ! --------------------------------------------------------
               ! 5.2 fiber compression: φ₁₋ (eq. 9-12)
               ! -------------------------------------------------------- 
               phi(1) = zero
               phi(2) = zero
              !! if(dam(1) == zero .and. sigma_a < zero) then
               if (sigma_a < zero) then
                       ! calculate fiber misalignment angle φ^c (eq. 12)
                       ! transform stresses to misaligned coordinates (eq. 11)
                       cos_phi = cos(phi_c)
                       sin_phi = sin(phi_c)
                       cos2_phi = cos_phi*cos_phi
                       sin2_phi = sin_phi*sin_phi
                       sin2phi = sin(two*phi_c)
                       !
                       sigma_bm = sigma_a*sin2_phi + sigma_b*cos2_phi  &
                                    - two*abs(tau_ab)*sin_phi*cos_phi
                       sigma_ab_m= (sigma_b - sigma_a)*sin_phi*cos_phi  &
                                      + abs(tau_ab)*(cos2_phi - sin2_phi)
                      ! calculate φ₁₋ (eq. 9)
                       phi(2) = (abs(sigma_ab_m) + eta_l*sigma_bm )/ sl
                       if (phi(2) .lt. zero) phi(2) = zero 
                       gama_ab_m = (epsyy(i) - epsxx(i))*sin_phi*cos_phi  &
                                      + half*abs(epsxy(i))*(cos2_phi - sin2_phi)
               elseif(sigma_a >= zero) then
                   phi(1) = (sigma_a - nu12*sigma_b)/xt  ! d1+
               endif ! d1°-
               ! --------------------------------------------------------
               ! 5.3 matrix tension (α=0°): φ₂₊ (eq. 13)
               ! --------------------------------------------------------
              !! if(dam(3) == zero ) then
                  if (sigma_b  >= zero) then
                  ! tension or small compression
                     tmp1 = sigma_b/yt
                     tmp2 =tau_ab/sl
                      phi(3) = sqrt((one - g_ratio) * tmp1     &
                                + g_ratio * tmp1*tmp1 + tmp2*tmp2)
                  else
                     ! compression (but using α=0° criterion)
                      phi(3) = half*(abs(abs(tau_ab) + eta_l*sigma_b) +  abs(tau_ab) + eta_l*sigma_b )
                      phi(3) = phi(3)/sl
                   end if
               !!endif ! d2+
               ! --------------------------------------------------------
               ! 5.4 Matrix compression (α=53°): φ₂₋ (Eq. 14-16)
               ! --------------------------------------------------------
               theta = zero
              !! if(dam(4) == zero ) then
                if (sigma_b < zero) then
                    ! calculate sliding angle θ (eq. 16)
                      if ( abs(sigma_b)*sin_alpha > em10 .and. abs(tau_ab) > em10) then
                          theta = atan2(-abs(tau_ab), sigma_b*sin_alpha)
                      elseif( abs(sigma_b)*sin_alpha > zero .and. abs(tau_ab) > em10)then
                          theta = half*pi
                      end if
                     ! calculate effective stresses (eq. 15)
                     cos_theta = cos(theta)
                     sin_theta = sin(theta)
                     tau_t = (-sigma_b*cos_alpha) * (sin_alpha - eta_t*cos_alpha*cos_theta)
                     tau_l = cos_alpha * (abs(tau_ab) + eta_l*sigma_b*cos_alpha*sin_theta)
                    ! calculate φ₂₋ (eq. 14)
                      phi(4) = sqrt((tau_t/st)**2 + (tau_l/sl)**2)
                      
                 else
                      phi(4) = zero
                 end if
             !!  endif ! d2-     
               ! ! ========================================================
               ! 6. update damage thresholds (explicit forward euler)
               ! ========================================================
               ! update r according to eqs. 24 and 26
               !! r(1) = max(r(1), phi(1), phi(2))  ! r₁₊
                r(1) = max(r(1), phi(1)) !r1+
                r(2) = max(r(2), phi(2))          ! r₁₋
                r(3) = max(r(3), phi(3))  !r(3) = max(r(3), phi(3), phi(4))  ! r₂₊   ! to check r(3) = max(r(3), phi(3)) 
                r(4) = max(r(4), phi(4))          ! r₂₋
                ! store updated r  values
                uvar(i,1) = r(1)
                uvar(i,2) = r(2)
                uvar(i,3) = r(3)
                uvar(i,4) = r(4)
               ! ========================================================
               ! 7. calculate energy densities (g_m = g_m / l*)
               ! ========================================================
                g1p_vol = gxt / l_char
                g1n_vol = gxc / l_char
                g2p_vol = gyt / l_char
                g2n_vol = gyc / l_char
                g6_vol  = gsl / l_char
              ! For bilinear fiber tension
                g1p0_vol = gxt0 / l_char
                g1n0_vol = gxc0 / l_char
               ! ========================================================
               ! 8. calculate damage variables (linear softening)
               ! ========================================================
               ! --- matrix tension (d₂₊) ---
               if(dam(3) == zero .and. r(3) >= one )then
                  uvar(i, 13) = epsyy(i)   ! initiation strain
                  uvar(i, 14) = abs(sigma_b)  ! epsf =  two * g2p_vol / yt
                  dam(3) = em10
               elseif(dam(3) > zero .and. dam(3) < one ) then
                  eps0 = uvar(i,13)  ! initiation strain ! yt/ e2
                  sig0 = uvar(i,14)
                  epsf = two * g2p_vol / sig0  ! final strain ! two * g2p_vol / yt
                   if(epsf < eps0) epsf = onep2*eps0 ! to avoid negative damage
                   if(epsyy(i) < eps0) then
                      dam(3) = zero
                   elseif(epsyy(i) < epsf)then
                      dam(3) = min(one, epsf*(abs(epsyy(i)) - eps0)/abs(epsyy(i))/(epsf - eps0))
                   else
                      dam(3) = one
                   end if  
               end if
               ! --- matrix compression (d₂₋) ---
                if (r(4) >= one .and. dam(4) == zero ) then
                    uvar(i,13) = abs(epsyy(i))   ! initiation strain
                    uvar(i,14) = abs(sigma_b)  ! needed for computing  epsf =  two * g2n_vol / sigm_b 
                    dam(4) = em10
                elseif(dam(4) > zero .and. dam(4) < one)    then
                    eps0 = uvar(i,13)  ! initiation strain
                    sig0 = uvar(i,14)
                    epsf =  two * g2n_vol / sig0
                    if(epsf < eps0) epsf = onep2*eps0 ! to avoid negative damage
                    dam(4) =  min(one, epsf*(abs(epsyy(i)) - eps0)/abs(epsyy(i))/(epsf - eps0))
                end if
              ! --- fiber tension (d₁₊) - bilinear ---
               if (r(1) >= one .and. dam(1) == zero ) then
                  uvar(i,9) = abs(epsxx(i))
                  uvar(i,10) = abs(sigma_a)  ! epsf =  two * g1p_vol / sigma_a
                  dam(1) = em10
               elseif (dam(1) > zero .and. dam(1) < one ) then
                  ! calculate key strains
                  eps0  = xt/e1  ! initiation strain
                  if(xt0 > zero .and. gxt0 > zero) then
                      eps_f = two * g1p0_vol / xt0 ! final strain
                     ! strain at pull-out transition (from energy balance)
                       eps_p  =   (two*g1p_vol + eps0*xt0)/xt !
                       if(eps_f < eps_p) eps_f = onep2*eps_p
                    ! bilinear damage calculation
                    if (epsxx(i) <=  eps0) then
                        dam(1) = zero
                    elseif (epsxx(i) <=  eps_p) then
                     ! first linear segment
                        k1 = (xt - xt0)/(eps_p - eps0) 
                        dam(1) = one + k1/e1 - (k1/e1 + one)/r(1)
                        dam(1) = min(one, dam(1))
                        uvar(i,17) = dam(1)
                    else if (epsxx(i) <=  eps_f) then
                     ! second linear segment
                         dam_p = uvar(i,17) ! damage at transition point 
                         dam_2=  min(one, eps_f*(abs(epsxx(i)) - eps_p)/abs(epsxx(i))/(eps_f - eps_p))
                         dam(1) = one - (one - dam_p )*(one - dam_2) 
                         dam(1) = min(one,dam(1))
                     else
                  ! fully damaged
                        dam(1) = one ! to avoid zero stiffness and negative damage
                    end if
                  else
                     ! linear softening if no bilinear parameters provided
                     epsf = two * g1p_vol / xt ! onset stress sig0 instead xt
                     eps0 = xt/e1
                     if(epsf < eps0) epsf = onep2*eps0 ! to avoid negative damage ! not fixed yet 
                     K1 = xt/(epsf - eps0)/e1
                     dam(1) = max(dam(1),one + k1 - (k1 + one)/r(1))
                     dam(1) = min(one, dam(1))
                  endif  
               end if
               ! --- fiber compression (d₁₋) ---
              if (r(2) >= one .and. dam(2) == zero) then
                  uvar(i,9) = abs(epsxx(i))
                  uvar(i,10) = abs(sigma_a)  ! epsf =  two * g1n_vol / sigma_a
                  uvar(i,11) = abs(gama_ab_m )! store shear strain at initiation
                  uvar(i,12) = abs(sigma_ab_m) ! store shear stress at initiation
                  dam(2) = em10
              elseif( dam(2) > zero .and. dam(2) < one ) then
                 ! calculate key strains
                  eps0   = uvar(i,9) ! xc / e1  ! initiation strain  
                  sig0   = uvar(i,10) ! abs(sigma_a)
                !!  eps0 = xc/e1  ! initiation strain
                if(xc0 > zero .and. gxc0 > zero) then
                  ! bilinear damage calculation 
                    eps0 = xc/e1  ! initiation strain
                  ! strain at pull-out transition (from energy balance)
                    eps_p  =   (two*g1n_vol + eps0*xc0)/xc !
                    eps_f = two * g1n0_vol / xc0 ! final strain
                    if(eps_f < eps_p) eps_f = onep2*eps_p
                   if (abs(epsxx(i)) <=  eps0) then
                       dam(2) = zero
                   elseif (abs(epsxx(i)) <=  eps_p) then
                   ! first linear segment
                        k1 = (xc - xc0)/(eps_p - eps0) 
                        dam(2) = one + k1/e1 - (k1/e1 + one)/r(2)
                        dam(2) = min(one, dam(2))
                        uvar(i,18) = dam(2) ! used in the second linear part to check damage at transition point
                   else if (abs(epsxx(i)) <=  eps_f) then
                     ! second linear segment
                         eps_f = two * g1n0_vol / xc0 ! final strain
                         dam_p = xc0/eps_p/e1 ! damage at transition point
                         dam_p = uvar(i,18) 
                         dam_2=  min(one, eps_f*(abs(epsxx(i)) - eps_p)/abs(epsxx(i))/(eps_f - eps_p))
                         dam(2) = one - (one - dam_p)*(one - dam_2) 
                         dam(2) = min(one, dam(2))
                   else
                 ! fully damaged
                         dam(2) = one
                   end if
                else
                  ! linear softening if no bilinear parameters provided
                      epsf = two * g1n_vol / sig0 ! onset stress sig0 instead xc
                      eps0 = xc/e1
                      epsf = two * g1n_vol / xc
                      if(epsf < eps0) epsf = onep2*eps0 ! to avoid negative damage
                       dam(2) = max(dam(2),epsf*(abs(epsxx(i)) - eps0)/abs(epsxx(i))/(epsf - eps0))  
                       dam(2) = min(one, dam(2))
                endif    
              end if
               ! --- shear (d₆) - coupled with matrix tension ---
               ! note: in paper, d₆ is function of r₂₊ (eq. 29 simplified)
               if (r(3) .gt. one .and. dam(5) == zero) then
                     uvar(i,15) = abs(epsxy(i))
                     uvar(i,16) = abs(tau_ab)  ! epsf =  two * g6_vol / tau_ab
                     dam(5) = em10
               elseif(dam(5) > zero .and. dam(5) < one ) then
                   eps0 = uvar(i,13)  ! onset strain in matrix direction
                   sig0 = uvar(i,14)  ! onset stress in matrix direction
                   gama0 = uvar(i,15) ! initiation shear strain
                   tau0  = uvar(i,16) ! initiation shear stress
                   gamaf = two*g6_vol/max(em20,tau0)
                   if(gamaf < gama0)  gamaf = onep2*gama0 ! to avoid negative damage
                   if(abs(epsxy(i)) < gama0) then
                      dam_shear = zero
                   elseif(abs(epsxy(i)) < gamaf)then
                      dam_shear = min(one, gamaf*(abs(epsxy(i)) - gama0)/abs(epsxy(i))/(gamaf - gama0))
                   else
                      dam_shear = one
                   end if 
                   dam(5) = one -  (one - dam_shear)*(one - dam(1))  ! coupling with d1+
                   dam(5) = min(one, dam(5))
               end if
              ! out of plane damage evolution
              dam(6) = zero   
              dam(7) = zero
              if( abs(epsyz(i)) >=  epsf23 .and. abs(epsyz(i)) <= epsr23 ) then
                 dam(6) = min(tsmd23, (abs(epsyz(i)) - epsr23)/(epsf23 - epsr23) )
              else if( abs(epsyz(i)) > epsf23 ) then
                  dam(6) = min(one, tsmd23)
              end if
              if( abs(epszx(i)) >=  epsf31 .and. abs(epszx(i)) < epsr31 ) then
                  dam(7) =  min(tsmd31, (abs(epszx(i)) - epsr31)/(epsf31 - epsr31) ) 
               else if( abs(epszx(i)) > epsf31 ) then
                   dam(7) = min(one , tsmd31) 
               end if
              !
              ! ========================================================
              ! 9. apply crack closure effects (eqs. 6, 28, 29)
              ! ========================================================
              ! determine active damage variables based on stress sign
              ! calculate active d₁ (eq. 6)
              if (sigma_a > zero) then
                  d_active(1) = dam(1)  ! d₁₊ active
              else if (sigma_a < zero) then
             ! d₁₋ active (with stiffness recovery, eq. 28)
                !! a_plus = b_param * (e1 - e2) / e1  ! from eq. 27
               !!  d_active(1) = one - (one - d(2)) * (one - a_plus*d(1)) to check
                 d_active(1) = dam(2)
              else
                 d_active(1) = zero
              end if
              ! calculate active d₂ (eq. 6)
              if (sigma_b > zero) then
                d_active(2) = dam(3)  ! d₂₊ active
              else if (sigma_b < zero) then
                d_active(2) = dam(4)  ! d₂₋ active
              else
                d_active(2) = zero
              end if
              ! calculate d₆ with coupling (simplified eq. 29)
              d_active(3) = one - (one - dam(5)) * (one - dam(1))
              !out of plane damage
              d_active(4) = one -dam(6)
              d_active(5) = one -dam(7)
             ! ========================================================
             ! 10. calculate damaged stiffness and stress
             ! ========================================================
             ! apply damage to moduli
             e1_dam = max(em20, e1 *  (one - d_active(1)))
             e2_dam = max(em20, e2 *  (one - d_active(2)))
             g12_dam = max(em20,g12 * (one - d_active(3)))
            ! update poisson's ratio (could also degrade)
             nu12_dam = nu12 * (one - d_active(1))  ! optional
             nu21_dam = nu12_dam * e2_dam / e1_dam
            ! build damaged stiffness matrix (plane stress)
             det = one / (one - nu12_dam*nu21_dam)
             d11 = e1_dam * det
             d22 = e2_dam * det
             d12 = nu12_dam * e2_dam * det
             d66 = g12_dam
            ! damaged stress calculation
             signxx(i) = d11 * epsxx(i)+ d12 * epsyy(i)
             signyy(i) = d12 * epsxx(i) + d22 * epsyy(i)
             signxy(i) = (one - d_active(3))*tau_ab  ! d66 * epsxy(i)
             signyz(i) = shf(i)*g23*epsyz(i)*d_active(4)
             signzx(i) = shf(i)*g13*epszx(i)*d_active(5)
            ! ========================================================
            ! 11. UPDATE STATE VARIABLES FOR NEXT STEP
            ! ========================================================
             uvar(i,1) = r(1)
             uvar(i,2) = r(2) 
             uvar(i,3) = r(3)
             uvar(i,4) = r(4)
             ! saved damage variables
             dmg(i,2) = dam(1)
             dmg(i,3) = dam(2)
             dmg(i,4) = dam(3)
             dmg(i,5) = dam(4)
             dmg(i,6) = dam(5)
            ! sound speed
             a11       = max(e1,e2)/(one - nu12*nu21) 
             ssp(i) = sqrt(a11/rho(i))
             etse(i) = one
             sigy(i) = sigyld
            enddo ! nel loop
!-------------------------------------------------------------------------------------------
         end subroutine sigeps132c
      end module sigeps132c_mod 
