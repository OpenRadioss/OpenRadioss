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
      !||    sigeps127c_mod   ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
      !||====================================================================
      module sigeps127c_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW127
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps127c         ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc             ../engine/source/materials/mat_share/mulawc.F90
      !||--- calls      -----------------------------------------------------
      !||    vinter             ../engine/source/tools/curve/vinter.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod      ../common_source/modules/precision_mod.F90
      !||====================================================================
         SUBROUTINE sigeps127c(                                   &
           nel     ,mat_param ,nuvar    ,uvar    ,               &
           rho     ,thk       ,thkly    ,shf     ,  ncycle ,     &
           nfunc   ,ifunc     ,npf      ,tf      ,  snpc   ,     &
           stf     ,epsp      ,nply_max ,                        &
           depsxx  ,depsyy    ,depsxy   ,depsyz   ,depszx ,      &   
           epsxx   ,epsyy     ,epsxy    ,epsyz    ,epszx  ,      &   
           sigoxx  ,sigoyy    ,sigoxy   ,sigozx   ,sigoyz ,         &
           signxx  ,signyy    ,signxy   ,signzx   ,signyz  ,      &
           off     ,sigy      ,etse     ,ssp      ,dmg     ,      &
           dmg_g   ,offply   ) 
!---------------------------------------------- -
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod 
          use constant_mod      
          use precision_mod, only : WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables
          integer, intent(in) :: ncycle !< number of cycle
          integer, intent(in) :: nfunc  !< number of function
          integer, intent(in) :: snpc  !< 
          integer, intent(in) :: stf  !< 
          integer, intent(in) :: nply_max  !< nbre of integration pt in the thickness
          integer, intent(in) :: ifunc(nfunc),npf(snpc)
          !
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          real(kind=WP), dimension(nel), intent(in) :: rho !< material density
          real(kind=WP), dimension(nel), intent(inout) :: sigy !< yield stress
          real(kind=WP), dimension(nel), intent(inout) :: shf !< shear factor correction 
          real(kind=WP), dimension(nel), intent(in)    :: thkly !< ply thickness  
          real(kind=WP), dimension(nel), intent(inout)    :: thk !< element thickness  
          real(kind=WP), dimension(stf), intent(in) :: tf
          real(kind=WP), dimension(nel), intent(in) :: epsp   !<  global element strain rate
          real(kind=WP), dimension(nel), intent(inout) :: etse !< ratio of rigidity  
          real(kind=WP), dimension(nel), intent(in) :: depsxx !< incr strain xx 
          real(kind=WP), dimension(nel), intent(in) :: depsyy !< incr strain yy
          real(kind=WP), dimension(nel), intent(in) :: depsxy !< incr strain xy 
          real(kind=WP), dimension(nel), intent(in) :: depsyz !< incr strain yz 
          real(kind=WP), dimension(nel), intent(in) :: depszx !< incr strain zx 
          real(kind=WP), dimension(nel), intent(in) :: epsxx !< total strain xx 
          real(kind=WP), dimension(nel), intent(in) :: epsyy !< total strain yy
          real(kind=WP), dimension(nel), intent(in) :: epsxy !< total strain xy 
          real(kind=WP), dimension(nel), intent(in) :: epsyz !< incr strain yz 
          real(kind=WP), dimension(nel), intent(in) :: epszx !< incr strain zx 
          real(kind=WP), dimension(nel), intent(in) :: sigoxx !< old stress xx 
          real(kind=WP), dimension(nel), intent(in) :: sigoyy !< old stress yy
          real(kind=WP), dimension(nel), intent(in) :: sigoxy !< old stress xy 
          real(kind=WP), dimension(nel), intent(in) :: sigoyz !< old stress yz 
          real(kind=WP), dimension(nel), intent(in) :: sigozx !< old stress zx 
          real(kind=WP), dimension(nel), intent(inout) :: signxx !< new stress xx 
          real(kind=WP), dimension(nel), intent(inout) :: signyy !< new stress yy
          real(kind=WP), dimension(nel), intent(inout) :: signxy !< new stress xy 
          real(kind=WP), dimension(nel), intent(inout) :: signyz !< new stress yz 
          real(kind=WP), dimension(nel), intent(inout) :: signzx !< new stress zx 
          real(kind=WP), dimension(nel), intent(inout) :: ssp !< sound speed
          real(kind=WP), dimension(nel), intent(inout) :: off !< element deletion flag
          real(kind=WP), dimension(nel,8), intent(inout) ::  dmg 
          real(kind=WP), dimension(nel), intent(inout) ::  dmg_g 
          real(kind=WP), dimension(nel), intent(inout) :: offply !< ply element deletion flag
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  fs, i,damage,updat,updat1,updat2,nkey,                  &
                  ncyred, n,ndex,twoway, ncy0,ndel_ply,ndex0
      integer , dimension(nel) :: index,iad,ipos,ilen,index0
      real(kind=WP)                                                             &
        :: e1, e2, nu12, nu21, xt0, slimt1, xc0, slimc1,                     &
        yt0, slimt2, yc0, slimc2s, sc0, d, damt, damc,                    &
        slims, invd, m2c, m2t, slimc2, alpha, beta, dfailt, dfailc,       &
        e21d, g12, limit_sig, eint, deint, a11, g13, g23, ycfac, dfailm,  &
        dfails, dfc, dft, efs, epsf, epsr, et, fbrt, tsmd, yc_over_sc,    &
        yfac_xt, yfac_xc, yfac_yc, yfac_yt, yfac_sc, eft, efc, emt, emc,  &
        scale,dam, ratio,del_ratio,eps_ef,tau2,sc2,tau_bar

      real(kind=WP), dimension(nel) :: dezz, check, xc, xt, yc, yt, sc, dydx
      !
      logical :: abit_t,abit_c,abit_s,abit_check
!!======================================================================
      e1    = mat_param%uparam(1)   ! Young's modulus in the longitudinal direction (1-direction)
      e2    = mat_param%uparam(2)   ! Young's modulus in the transverse direction (2-direction)
      g12   = mat_param%uparam(4)   ! Shear modulus in the plane of the fibers (1-2 plane)
      g13   = mat_param%uparam(5)   ! Shear modulus in the 1-3 plane
      g23   = mat_param%uparam(6)   ! Shear modulus in the 2-3 plane
      nu12  = mat_param%uparam(7)   ! Poisson's ratio for strain in the 2-direction when stressed in the 1-direction
      nu21  = mat_param%uparam(8)   ! Poisson's ratio for strain in the 1-direction when stressed in the 2-direction
      ! Fiber direction properties
      xt0    = mat_param%uparam(13) ! Tensile strength in the fiber direction (1-direction)
      slimt1 = mat_param%uparam(14)  ! Slope of the tensile stress-strain curve in the fiber direction
      xc0    = mat_param%uparam(15) ! Compressive strength in the fiber direction (1-direction)
      slimc1 = mat_param%uparam(16)  ! Slope of the compressive stress-strain curve in the fiber direction
      ! Matrix direction properties
      yt0    = mat_param%uparam(17) ! Tensile strength in the transverse direction (2-direction)
      slimt2 = mat_param%uparam(18)  ! Slope of the tensile stress-strain curve in the transverse direction
      yc0    = mat_param%uparam(19)  ! Compressive strength in the transverse direction (2-direction)
      slimc2 = mat_param%uparam(20)  ! Slope of the compressive stress-strain curve in the transverse direction
      ! Shear properties
      sc0    = mat_param%uparam(21) ! Shear strength
      slims = mat_param%uparam(22) ! Slope of the shear stress-strain curve
      fbrt  = mat_param%uparam(23) ! reduced fiber 
      ycfac = mat_param%uparam(24) ! reduced compression strength

      dfailt = mat_param%uparam(25)  ! Failure strain in tension
      dfailc = mat_param%uparam(26)  ! Failure strain in compression
      dfailm = mat_param%uparam(27)  ! Failure strain in matrix
      dfails = mat_param%uparam(28)  ! Failure strain in shear
      efs    = mat_param%uparam(29)  ! Effective strain
      ratio  = mat_param%uparam(30)   ! ratio for deletion of elt

      beta  = mat_param%uparam(31)  ! Coefficient for chang-chang
      alpha = mat_param%uparam(32)  ! coefficient for nonlinear 
      epsf  = mat_param%uparam(33)  ! initial  strain damage
      epsr  = mat_param%uparam(34)  ! final damage strain
      tsmd  = mat_param%uparam(35)  ! max of damage 

      yfac_xt = mat_param%uparam(36)  ! scale factor in tension 
      yfac_xc = mat_param%uparam(37)  ! scale factor in compression
      yfac_yt = mat_param%uparam(38)  ! scale factor in transverse tension
      yfac_yc = mat_param%uparam(39)  ! scale factor in transverse compression
      yfac_sc = mat_param%uparam(40)  ! scale factor in shear
      !
      ncyred = mat_param%iparam(3)  
      
      ! yc_over_sc= fourth*(yc/sc)**2  !
      ! strain rate dependency of strength
        ! xt
      if(ifunc(1) /= 0) then
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(1)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(1)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,xt) 
        xt(1:nel)= yfac_xt*xt(1:nel)
      else
        xt(1:nel) = xt0
      endif 
      ! xc     
      if(ifunc(2) /= 0) then
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(2)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(2)+1) / 2 - iad(1:nel) - ipos(1:nel)
        call vinter(tf,iad,ipos,ilen,nel,epsp,dydx,xc) 
        xc(1:nel)= yfac_xc*xc(1:nel)
      else
         xc(1:nel) = xc0
      endif
      ! yt    
      if(ifunc(3) /= 0) then
           ipos(1:nel) = 1
           iad (1:nel) = npf(ifunc(3)) / 2 + 1
           ilen(1:NEL) = npf(ifunc(3)+1) / 2 - iad(1:nel) - ipos(1:nel)
           call vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yt) 
           yt(1:nel)= yfac_yt*yt(1:nel)
      else
           yt(1:nel) = yt0
      endif  
       ! yc    
      if(ifunc(4) /= 0) then
           ipos(1:nel) = 1
           iad (1:nel) = npf(ifunc(4)) / 2 + 1
           ilen(1:NEL) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
           call vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yc) 
           yc(1:nel)= yfac_yc*yc(1:nel)
      else
           yc(1:nel) = yc0
      endif
       ! sc shaer    
      if(ifunc(5) /= 0) then
           ipos(1:nel) = 1
           iad (1:nel) = npf(ifunc(5)) / 2 + 1
           ilen(1:NEL) = npf(ifunc(5)+1) / 2 - iad(1:nel) - ipos(1:nel)
           call vinter(tf,iad,ipos,ilen,nel,epsp,dydx,sc) 
           sc(1:nel)= yfac_sc*sc(1:nel)
      else
           sc(1:nel) = sc0
      endif
      ! Reduction of stress ply and checking the deletion of element
      ndex0 = 0
      do i=1,nel
          if(off(i) < one ) then
              off(i) = zero 
          elseif(dmg(i,1) == one ) then
              ndel_ply = nint(dmg_g(i))
              signxx(i) = zero
              signyy(i) = zero
              signxy(i) = zero
              signyz(i) = zero
              signzx(i) = zero
              del_ratio= ndel_ply/nply_max 
              if( ndel_ply == nply_max .or. del_ratio >= ratio) then
                off(i) =  four_over_5
              endif 
           else
              ndex0 = ndex0 + 1
              index0(ndex0) = i     
           endif
      enddo 
!! ------!!---------------------------------           
       ndex = 0 
       ! Failure based on  chang-chang model
      if(dfailt == zero) then
          do n=1,ndex0
                i=index0(n)
                ! Update compressive strength if damage in matrix is complete
                if(dmg(i,5) == one ) then
                 xc(i) = ycfac*yc(i)! 
                 xt(i) = fbrt*xt(i)
                endif   
            ! computing ne stress
                d = (one - nu12*nu21)
                invd = one/d
                signxx(i) = sigoxx(i) + invd*(e1*depsxx(i) + nu21*e1*depsyy(i))
                signyy(i) = sigoyy(i) + invd*(nu12*e2*depsxx(i)+ e2*depsyy(i))
                signxy(i) = sigoxy(i) + g12*depsxy(i)
                signzx(i) = sigozx(i) + shf(i)*g13*depszx(i) 
                signyz(i) = sigoyz(i) + shf(i)*g23*depsyz(i) 
                ! Fiber failure
                if(signxx(i) >= zero .and. dmg(i,2)  == zero ) then
                     eft = (signxx(i)/xt(i))**2  + beta*(signxy(i)/sc(i))**2 
                     if( eft >= one) then
                         dmg(i,2) = one  ! 
                         dmg(i,1) = one  ! set ply off 
                         dmg_g(i) = dmg_g(i) + one 
                         uvar(i,1) = ncycle 
                         offply(i) = zero
                      endif  
                elseif(signxx(i) < zero .and. dmg(i,3) == zero ) then
                      efc = (signxx(i)/xc(i))**2
                      if(efc >= one) dmg(i,3) = one
               endif
               ! matrix failure 
               if(signyy(i) >= zero  .and. dmg(i,4) == zero ) then
                        scale = half/g12
                        tau2 = signxy(i)**2
                        sc2  = sc(i)**2
                        tau_bar = scale*tau2 + three_over_4*alpha*tau2**2
                        tau_bar = tau_bar/(scale*sc2 + three_over_4*alpha*sc2**2)
                        emt = (signyy(i)/yt(i))**2  + tau_bar
                       if( emt >= one) dmg(i,4) = one 
               elseif(signyy(i) < zero .and. dmg(i,5) == zero) then
                       yc_over_sc  = fourth*(yc(i)/sc(i))**2
                       emc = fourth*(signyy(i)/sc(i))**2  + (yc_over_sc - one)*signyy(i)/yc(i) + (signxy(i)/sc(i))**2 
                       if(emc >= one) dmg(i,5)= one 
               endif 
               if(abs(signxy(i)) >= sc(i) ) dmg(i,6 ) = one 
               ! failure based on effective strain
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(eps_ef >= efs .and. offply(i) == one ) then
                  dmg(i,1) = one 
                  uvar(i,1) = ncycle 
                  dmg_g(i) = dmg_g(i) + one
                  offply(i) = zero
                endif  
                if(offply(i) > zero )then
                    ndex = ndex + 1
                    index(ndex) = i
                else      
                    signxx(i) = zero
                    signyy(i) = zero
                    signxy(i) = zero
                    signzx(i) = zero
                    signyz(i) = zero
                endif  
          end do ! nel 
      endif ! dfailt == zero
       ! criteria based on strain
      if (dfailt > zero )then
          do n=1,ndex0
                i=index0(n)
              !  ! Update compressive strength if damage in matrix is complete
                if(dmg(i,5) == one ) then
                 xc(i) = ycfac*yc(i)! 
                 xt(i) = fbrt*xt(i)
                endif   
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(epsxx(i) >= dfailt .or. epsxx(i) <= dfailc .or.                    &
                abs(epsyy(i)) >= dfailm .or. abs(epsxy(i)) >= dfails  .or. eps_ef >= efs  ) then
                 !! off(i) = four_over_5
                   dmg(i,1) = one 
                   uvar(i,1) = ncycle 
                   dmg_g(i) = dmg_g(i) + one
                   offply(i) = zero
                   signxx(i) = zero
                   signyy(i) = zero
                   signxy(i) = zero
                   signzx(i) = zero
                   signyz(i) = zero
                else
              ! computing new stress 
                   d = (one - nu12*nu21)
                   invd = one/d
                   signxx(i) = sigoxx(i) + invd*(e1*depsxx(i) + nu21*e1*depsyy(i))
                   signyy(i) = sigoyy(i) + invd*(nu12*e2*depsxx(i)+ e2*depsyy(i))
                   signxy(i) = sigoxy(i) + g12*depsxy(i)
                   signzx(i) = sigozx(i) + shf(i)*g13*depszx(i) 
                   signyz(i) = sigoyz(i) + shf(i)*g23*depsyz(i) 
                !
                   if(signxx(i) >= zero .and. dmg(i,2)  == zero ) then
                     eft = (signxx(i)/xt(i))**2  + beta*(signxy(i)/sc(i))**2 
                     if( eft >= one) dmg(i,2) = one  ! 
                   elseif(signxx(i) < zero .and. dmg(i,3) == zero ) then
                      efc = (signxx(i)/xc(i))**2
                      if(efc >= one) dmg(i,3) = one
                   endif
                   ! matrix failure 
                   if(signyy(i) >= zero  .and. dmg(i,4) == zero ) then
                        scale = half/g12
                        tau2 = signxy(i)**2
                        sc2  = sc(i)**2
                        tau_bar = scale*tau2 + three_over_4*alpha*tau2**2
                        tau_bar = tau_bar/(scale*sc2 + three_over_4*alpha*sc2**2)
                        emt = (signyy(i)/yt(i))**2  + tau_bar
                       if( emt >= one) dmg(i,4) = one 
                   elseif(signyy(i) < zero .and. dmg(i,5) == zero) then
                       yc_over_sc  = fourth*(yc(i)/sc(i))**2
                       emc = fourth*(signyy(i)/sc(i))**2  + (yc_over_sc - one)*signyy(i)/yc(i) + (signxy(i)/sc(i))**2 
                       if(emc >= one) dmg(i,5)= one 
                   endif 
                   if(abs(signxy(i)) >= sc(i) ) dmg(i,6 ) = one 
                   ndex = ndex + 1
                   index(ndex) = i 
                endif
          enddo
      endif   
       ! 
#include "vectorize.inc"                         
       do n=1,ndex
             i= index(n)
             deint = half*(depsxx(i)*(signxx(i) + sigoxx(i))  +                                     &
                           depsyy(i)*(signyy(i) + sigoyy(i))) +                                    &
                           depsxy(i)*(signxy(i) + sigoxy(i)) 
             eint = uvar(i,2) + deint
             uvar(i,2) = eint
             if(deint < ZERO ) then
               check(i) = -one
             else 
                check(i) = one  
             endif  
             etse(i)   = one
             a11       = max(e1,e2)/(one - nu12**2) 
             a11       = max(e1,e2)
             ssp(i)    = sqrt(a11/rho(i))
             sigy(i)    = min(slimt1*xt(i),slimt2*yt(i), slimc1*xc(i),slimc2*yc(i)) ! to check
            ! computation of the thickness variation 
              limit_sig=  zero
              if(check(i) >= zero) then ! loading 
                 ! dir 11
                 if(dmg(i,2) == one  .and. signxx(i) >= slimt1*xt(i) ) then
                    limit_sig = slimt1*xt(i)
                    signxx(i) = limit_sig
                    signyy(i) = sigoyy(i)
                    signxy(i) = sigoxy(i)
                  elseif(dmg(i,3) == one .and. signxx(i)  <= - slimc1*xc(i)) then
                    signxx(i) = - slimc1*xc(i)
                    signyy(i) = sigoyy(i)
                    signxy(i) = sigoxy(i)
                  endif  
                ! dir 22
                  if(dmg(i,4) == one  .and. signyy(i) >=  slimt2*yt(i)) then
                    signyy(i) = slimt2*yt(i)
                    signxx(i) = sigoxx(i)
                    signxy(i) = sigoxy(i)
                  elseif(dmg(i,5) == one  .and. signyy(i) <= -slimc2*yc(i)) then
                    signyy(i) = - slimc2*yc(i)
                    signxx(i) = sigoxx(i)
                    signxy(i) = sigoxy(i)
                  endif  
                
                  if(dmg(i,6) == one .and. abs(signxy(i)) >=  slims*sc(i) ) then
                    limit_sig = slims*sc(i)
                    signxy(i) = sign(limit_sig, signxy(i))
                    signxx(i) = sigoxx(i)
                    signyy(i) = sigoyy(i)
                  endif  
              else ! unloading check < 0
                 ! dir 11
                  if(dmg(i,2) == one .or. sigoxx(i) == slimt1*xt(i)  ) then
                    limit_sig = slimt1*xt(i)
                    signxx(i) = min(signxx(i),limit_sig)
                  elseif(dmg(i,3) == one .or. sigoxx(i) == -slimc1*xc(i) )then
                    limit_sig= - slimc1*xc(i)
                    signxx(i)= max(signxx(i),limit_sig)
                  endif 
                ! dir 22
                  if(dmg(i,4) == one .or. sigoyy(i) == slimt2*yt(i)) then
                    limit_sig = slimt2*yt(i)
                    signyy(i) = min(signyy(i),limit_sig)
                  elseif(dmg(i,5) == one .or. sigoyy(i) == -slimc2*yc(i)) then
                    limit_sig= - slimc2*yc(i)
                    signyy(i)= max(signyy(i),limit_sig)
                  endif  
                  if(dmg(i,6) ==one .or. abs(sigoxy(i)) == slims*sc(i)) then
                    limit_sig = slims*sc(i)
                    if(signxy(i) >= zero) then
                       signxy(i) = min(limit_sig, signxy(i))
                    else
                      signxy(i) = max(-limit_sig, signxy(i))
                    endif
                  endif
              endif  
              ! out of plane damage  
              eps_ef = half*sqrt(epszx(i)**2 + epsyz(i)**2) 
              if( eps_ef >= epsf) then
                     dam =  (eps_ef - epsf)/(epsr - epsf) 
                     dam = min(tsmd, dam)
                     signzx(i) = (one - dam)*signzx(i)
                     signyz(i) = (one - dam)*signyz(i)
              endif  
              dezz(i)  = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
              thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i) 
           enddo ! nel loop
!-------------------------------------------------------------------------------------------
         end subroutine sigeps127c
      end module sigeps127c_mod 
