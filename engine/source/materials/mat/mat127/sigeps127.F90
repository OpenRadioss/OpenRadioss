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
      !||    sigeps127_mod   ../engine/source/materials/mat/mat127/sigeps127.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps127_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW127
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps127          ../engine/source/materials/mat/mat127/sigeps127.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    vinter             ../engine/source/tools/curve/vinter.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
          subroutine sigeps127(                                       &
          nel      ,nuvar    ,uvar     ,mat_param ,rho0     , time   ,&
          nfunc    ,ifunc    ,snpc      ,npf      ,stf      ,tf      ,&
          depsxx    ,depsyy   ,depszz   ,depsxy  ,depsyz   ,depszx   ,&
          epsxx    ,epsyy    ,epszz    ,epsxy    ,epsyz    ,epszx    ,&
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,&
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,&
          epsp     ,off      ,ssp      ,dmg  , ngl    ) 
!---------------------------------------------- -
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod 
          use constant_mod      
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "my_real.inc"
#include  "units_c.inc"
#include   "comlock.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables
          integer, intent(in) :: nfunc  !< number of function
          integer, intent(in) :: snpc  !<  ! number of points in the function
          integer, intent(in) :: stf   !<  ! number of time function
          integer, intent(in) :: ifunc(nfunc) !<  ! function id
          integer, intent(in) :: npf(snpc)  !<  ! number of points in the function
          integer, dimension(nel), intent(in) :: ngl   ! Id of element
          !
          my_real, dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          my_real, dimension(nel), intent(in) :: rho0 !< material density
          my_real, dimension(stf), intent(in) :: tf !< time function
          my_real, dimension(nel), intent(in) :: epsp !<  equiv. strain rate
          my_real, dimension(nel), intent(in) :: depsxx !< incr strain xx 
          my_real, dimension(nel), intent(in) :: depsyy !< incr strain yy
          my_real, dimension(nel), intent(in) :: depszz !< incr strain zz
          my_real, dimension(nel), intent(in) :: depsxy !< incr strain xy
          my_real, dimension(nel), intent(in) :: depsyz !< incr strain yz 
          my_real, dimension(nel), intent(in) :: depszx !< incr strain zx 
          my_real, dimension(nel), intent(in) :: epsxx !< total strain xx 
          my_real, dimension(nel), intent(in) :: epsyy !< total strain yy
          my_real, dimension(nel), intent(in) :: epszz !< total strain zz
          my_real, dimension(nel), intent(in) :: epsxy !< total strain xy 
          my_real, dimension(nel), intent(in) :: epsyz !< incr strain yz 
          my_real, dimension(nel), intent(in) :: epszx !< incr strain zx 
          my_real, dimension(nel), intent(in) :: sigoxx !< old stress xx 
          my_real, dimension(nel), intent(in) :: sigoyy !< old stress yy
          my_real, dimension(nel), intent(in) :: sigozz !< old stress zz 
          my_real, dimension(nel), intent(in) :: sigoxy !< old stress xy 
          my_real, dimension(nel), intent(in) :: sigoyz !< old stress yz 
          my_real, dimension(nel), intent(in) :: sigozx !< old stress zx 
          my_real, dimension(nel), intent(out) :: signxx !< new stress xx 
          my_real, dimension(nel), intent(out) :: signyy !< new stress yy
          my_real, dimension(nel), intent(out) :: signzz !< new stress zz
          my_real, dimension(nel), intent(out) :: signxy !< new stress xy 
          my_real, dimension(nel), intent(out) :: signyz !< new stress yz 
          my_real, dimension(nel), intent(out) :: signzx !< new stress zx 
          my_real, dimension(nel), intent(inout) :: ssp !< sound speed
          my_real, dimension(nel), intent(inout) :: off !< element deletion flag
          my_real, dimension(nel,8), intent(inout) ::  dmg 

          my_real,  intent(in) :: time
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i,ncyred, n,ndex,twoway,ndx_fail,ti
      integer , dimension(nel) :: index,iad,ipos,ilen,indx_fail
      my_real                                                             &
        e1, e2, nu12, nu21, xt0, slimt1, xc0, slimc1, yt0, slimt2,        &
        yc0, sc0, slims, slimc2, alpha, beta, dfailt, dfailc, g12,        & 
        limit_sig, a11, g13, g23, ycfac, dfailm, dfails,efs, epsf,        &
        epsr, fbrt, tsmd, yc_over_sc, yfac_xt, yfac_xc, yfac_yc, dam,     &
        yfac_yt, yfac_sc, eft, efc, emt, emc,scale,eps_ef,tau2,sc2,       &
        tau_bar, d11,d22,d33, d12,d13,d23,e3,nu13,nu31,nu23,nu32,red      
      my_real, dimension(nel) ::  xc, xt, yc, yt, sc, dydx
!!======================================================================
      e1    = mat_param%uparam(1)   ! Young's modulus in the longitudinal direction (1-direction)
      e2    = mat_param%uparam(2)   ! Young's modulus in the transverse direction (2-direction)
      e3    = mat_param%uparam(3)   ! Young's modulus in the transverse direction (2-direction)
       !
      g12   = mat_param%uparam(4)   ! Shear modulus in the plane of the fibers (1-2 plane)
      g13   = mat_param%uparam(5)   ! Shear modulus in the 1-3 plane
      g23   = mat_param%uparam(6)   ! Shear modulus in the 2-3 plane
       !
      nu12  = mat_param%uparam(7)   ! Poisson's ratio for strain in the 2-direction when stressed in the 1-direction
      nu21  = mat_param%uparam(8)   ! Poisson's ratio for strain in the 1-direction when stressed in the 2-direction
       !
      nu13  = mat_param%uparam(9)   ! Poisson's ratio for strain in the 3 direction when stressed in the 1-direction
      nu23  = mat_param%uparam(10)   ! Poisson's ratio for strain in the 3-direction when stressed in the 2-direction
      
      nu31  = mat_param%uparam(11)   ! Poisson's ratio for strain in the 1-direction when stressed in the 3-direction
      nu32  = mat_param%uparam(12)   ! Poisson's ratio for strain in the 2-direction when stressed in the 3-direction

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
      !
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

      ! Orthotropoic Hook 3D material law 
      d11     = mat_param%uparam(41)  !  
      d22     = mat_param%uparam(42)  ! 
      d33     = mat_param%uparam(43)  ! 
      d12     = mat_param%uparam(44)  ! 
      d13     = mat_param%uparam(45)  ! 
      d23     = mat_param%uparam(46)  ! 
      g12     = mat_param%uparam(47)   ! Shear modulus in the plane of the fibers (1-2 plane)
      g13     = mat_param%uparam(48)   ! Shear modulus in the 1-3 plane
      g23     = mat_param%uparam(49)   ! Shear modulus in the 2-3 plane
      !
      twoway = mat_param%iparam(1)    ! flag to switch between one or two fiber direction
      ti     = mat_param%iparam(2)    ! flag for isotropic transverse
      ncyred = mat_param%iparam(3)    ! number of cycle for reduction de stress
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
      ndex = 0
      do i=1,nel
          if(off(i) < one  ) then
            if(dmg(i,1) == one )  then
              red = min(em01, one/ncyred)
              off(i) = off(i) - red 
              if(off(i) <= em02) off(i) = zero
            endif
            signxx(i) = sigoxx(i) * off(i)
            signyy(i) = sigoyy(i) * off(i)
            signzz(i) = sigozz(i) * off(i)
            signxy(i) = sigoxy(i) * off(i)
            signzx(i) = sigozx(i) * off(i)
            signyz(i) = sigoyz(i) * off(i)
          else
              ndex = ndex + 1
              index(ndex) = i 
          endif
      enddo 
!! ---------------------------------------       
    ! undapte stress for element not reach the failure.  
#include "vectorize.inc"      
      do n=1,ndex
        i = index(n)
               ! computing ne stress
               ! Hook's orthotropic equation
                ! Compute stresses using the inverse matrix
                signxx(i) = sigoxx(i) + (d11*depsxx(i) + d12*depsyy(i) + d13*depszz(i))
                signyy(i) = sigoyy(i) + (d12*depsxx(i) + d22*depsyy(i) + d23*depszz(i))
                signzz(i) = sigozz(i) + (d13*depsxx(i) + d23*depsyy(i) + d33*depszz(i))
                signxy(i) = sigoxy(i) + g12*depsxy(i)
                signzx(i) = sigozx(i) + g13*depszx(i)
                signyz(i) = sigoyz(i) + g23*depsyz(i)!
                !
                a11 = max(d11,d22,d33)
                ssp(i) = sqrt(a11/rho0(i))
      enddo ! ndex       
       ! check the Failure. It's  based on  chang-chang model
      ndx_fail = 0
      select case (twoway) 
        case(0) ! one fiber direction 
#include "vectorize.inc"                  
              do n=1,ndex
                i= index(n)
                ! Update compressive strength if damage in matrix is complete in b or c
                if(dmg(i,5) == one .or. dmg(i,8) == one) then
                  xc(i) = ycfac*yc(i)! 
                  xt(i) = fbrt*xt(i)
                endif   
                ! Fiber failure
                if(signxx(i) >= zero .and. dmg(i,2)  == zero ) then
                     eft = (signxx(i)/xt(i))**2  + beta*(signxy(i)/sc(i))**2 
                     if( eft >= one) dmg(i,2) = one  ! 
                elseif(dmg(i,3) == zero ) then
                      efc = (signxx(i)/xc(i))**2
                      if(efc >= one) dmg(i,3) = one
               endif
               ! matrix failure dir b
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
               ! matrix failure dir c
               if(signzz(i) >= zero  .and. dmg(i,7) == zero ) then
                        scale = half/g12
                        tau2 = signxy(i)**2
                        sc2  = sc(i)**2
                        tau_bar = scale*tau2 + three_over_4*alpha*tau2**2
                        tau_bar = tau_bar/(scale*sc2 + three_over_4*alpha*sc2**2)
                        emt = (signzz(i)/yt(i))**2  + tau_bar
                       if( emt >= one) dmg(i,7) = one 
               elseif(signzz(i) < zero .and. dmg(i,8) == zero) then
                       yc_over_sc  = fourth*(yc(i)/sc(i))**2
                       emc = fourth*(signzz(i)/sc(i))**2  + (yc_over_sc - one)*signzz(i)/yc(i) + (signxy(i)/sc(i))**2 
                       if(emc >= one) dmg(i,8)= one 
               endif 
               if(abs(signxy(i)) >= sc(i) ) dmg(i,6 ) = one 
              end do ! ndex 
            case(1)  ! two fiber direction
#include "vectorize.inc"                              
              do n=1,ndex
                i = index(n)
                ! Update compressive strength if damage in matrix is complete in direction c
                if(dmg(i,8) == one ) then
                 xc(i) = ycfac*yc(i)! 
                 xt(i) = fbrt*xt(i)
                endif  
                ! Fiber failure dir a
                if(signxx(i) >= zero .and. dmg(i,2)  == zero ) then
                     eft = (signxx(i)/xt(i))**2  + beta*(signxy(i)/sc(i))**2 
                     if( eft >= one) dmg(i,2) = one  ! 
                elseif(dmg(i,3) == zero ) then
                      efc = (signxx(i)/xc(i))**2
                      if(efc >= one) dmg(i,3) = one
               endif 
                ! Fiber failure dir b
                if(signyy(i) >= zero .and. dmg(i,4)  == zero ) then
                      eft = (signyy(i)/yt(i))**2  + beta*(signxy(i)/sc(i))**2 
                      if( eft >= one) dmg(i,4) = one  
                elseif(dmg(i,5) == zero ) then
                      efc = (signyy(i)/yc(i))**2
                      if(efc >= one) dmg(i,5) = one
                endif
               ! matrix failure only on shear 
                if(abs(signxy(i)) >= sc(i) ) dmg(i,6 ) = one 
                ! failur based on effective strain
              end do ! ndex 
            end select ! twoway    
      ! failure criteria 
      ndx_fail = 0
      if (dfailt > zero )then
          select case (twoway)
            case(0) ! one fiber direction
              do n=1,ndex
                i= index(n) 
                ! failure based on max strain 
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(epsxx(i) >= dfailt .or. epsxx(i) <= dfailc .or.  abs(epszz(i)) >=dfailm .or.    &
                    abs(epsyy(i)) >= dfailm .or. abs(epsxy(i)) >= dfails  .or. eps_ef >= efs  ) then
                     dmg(i,1) = one 
                     off(i) = four_over_5
                     ndx_fail = ndx_fail + 1
                     indx_fail(ndx_fail) = i
                     uvar(i,1) = one  
                 endif  
              end do   
            case(1) ! two fiber direction
              do n=1,ndex
                i= index(n) 
                ! failure based on max strain 
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(epsxx(i) >= dfailt .or. epsxx(i) <= dfailc .or.                    &
                   epsyy(i) >= dfailt .or. epsyy(i) <= dfailc .or.                    &
                   abs(epszz(i)) >=dfailm .or. eps_ef >= efs  ) then
                     dmg(i,1) = one 
                     off(i) = four_over_5
                     ndx_fail = ndx_fail + 1
                     indx_fail(ndx_fail) = i
                     uvar(i,1) = one  
                 endif  
              end do   
          end select 
      else ! dfailt = zero
         select case (twoway) 
            case(0) ! one fiber direction
              do n=1,ndex
                i= index(n) 
                ! failure based on max strain 
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(eps_ef >= efs  .or. dmg(i,2) == one ) then
                     dmg(i,1) = one 
                     off(i) = four_over_5
                     ndx_fail = ndx_fail + 1
                     indx_fail(ndx_fail) = i
                     uvar(i,1) = one  
                 endif  
              end do   
            case(1) ! two fiber direction
              do n=1,ndex
                i= index(n) 
                ! failure based on max strain 
                eps_ef =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2 ) 
                eps_ef = sqrt(eps_ef) 
                if(eps_ef >= efs  .or.           & 
                   dmg(i,2) == one .or. dmg(4,i) == one  ) then
                     dmg(i,1) = one 
                     off(i) = four_over_5
                     ndx_fail = ndx_fail + 1
                     indx_fail(ndx_fail) = i
                     uvar(i,1) = one  
                 endif  
              end do   
          end select 
      endif   
       ! 
#include "vectorize.inc"                         
      do n=1,ndex
              i= index(n)
               !
              limit_sig=  zero
                ! fiber direction a
              if(dmg(i,2) == one .and. &
                     (signxx(i) >= slimt1*xt(i) .or. sigoxx(i) == slimt1*xt(i)) ) then
                    limit_sig = slimt1*xt(i) 
                    signxx(i) = min(signxx(i),limit_sig)
                    signyy(i) = sigoyy(i)
                    signzz(i) = sigozz(i)
                    signxy(i) = sigoxy(i)
              elseif(dmg(i,3) == one .and. &
                     (signxx(i) <= -slimc1*xc(i) .or. sigoxx(i) == -slimc1*xc(i)) ) then
                    !!signxx(i) = - slimc1*xc(i)
                    limit_sig= - slimc1*xc(i)
                    signxx(i)= max(signxx(i),limit_sig)
                    signyy(i) = sigoyy(i)
                    signzz(i) = sigozz(i)
                    signxy(i) = sigoxy(i)
              endif  
                ! matrix direction c 
              if( dmg(i,7) == one .and. &
                     (signzz(i) >= slimt2*yt(i) .or. sigozz(i) == slimt2*yt(i)) ) then
                    limit_sig= slimt2*yt(i)
                    signzz(i)= min(signzz(i),limit_sig)
                    signxx(i) = sigoxx(i)
                    signyy(i) = sigoyy(i)
                    signxy(i) = sigoxy(i)
              elseif(dmg(i,8) == one .and. &
                     (signzz(i) <= -slimc2*yc(i) .or. sigozz(i) == -slimc2*yc(i)) ) then
                    limit_sig= - slimc2*yc(i)
                    signzz(i)= max(signzz(i),limit_sig)
                    signxx(i) = sigoxx(i)
                    signyy(i) = sigoyy(i)
                    signxy(i) = sigoxy(i)
              endif  
                  ! matrix direction b
              if(dmg(i,4) == one .and. &
                     (signyy(i) >= slimt2*yt(i) .or. sigoyy(i) == slimt2*yt(i)) ) then
                    limit_sig= slimt2*yt(i)
                    signyy(i)= min(signyy(i),limit_sig)
                    signxx(i) = sigoxx(i)
                    signxy(i) = sigoxy(i)
                    signzz(i) = sigozz(i)
              elseif(dmg(i,5) == one  .and. &
                     (signyy(i) <= -slimc2*yc(i) .or. sigoyy(i) == -slimc2*yc(i)) ) then
                    limit_sig = - slimc2*yc(i)
                    signyy(i)= max(signyy(i),limit_sig)
                    signxx(i) = sigoxx(i)
                    signxy(i) = sigoxy(i)
                    signzz(i) = sigozz(i)
              endif
                  !
              if(dmg(i,6) == one  ) then
                    limit_sig = slims*sc(i)
                    if(signxy(i) >= zero .and. signxy(i) >= limit_sig) then
                       signxy(i) = min(limit_sig, signxy(i))
                      !! signxx(i) = sigoxx(i)
                      !! signyy(i) = sigoyy(i)
                     !!  signzz(i) = sigozz(i)
                    elseif(signxy(i) < zero .and. signxy(i) <= -limit_sig) then
                       signxy(i) = max(-limit_sig, signxy(i))
                     !! signxx(i) = sigoxx(i)
                     !!  signyy(i) = sigoyy(i)
                      !! signzz(i) = sigozz(i)
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
      enddo ! nel loop
      if(ndx_fail > 0) then
#include "vectorize.inc"                        
            do n=1,ndx_fail
              i = indx_fail(n)
#include "lockon.inc"
              write(iout, 1000) ngl(i),time
              write(istdo,1000) ngl(i),time
#include "lockoff.inc"
            enddo ! ndx_fail
      endif ! ndxfail    
!
 1000 FORMAT(1X,'delete solid element ',I10 , ' at time :', 1PE12.4)         
!-------------------------------------------------------------------------------------------
         end subroutine sigeps127
      end module sigeps127_mod 
