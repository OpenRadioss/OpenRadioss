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
!||    sigeps125_mod   ../engine/source/materials/mat/mat125/sigeps125.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps125_mod
        implicit none
      contains
        ! ======================================================================================================================
        ! \brief    Mat058 Lsdyna
        ! \details Material law based on Mat058 Lsdyna. Dedicated to composite .
        ! ======================================================================================================================
!||====================================================================
!||    sigeps125          ../engine/source/materials/mat/mat125/sigeps125.F90
!||--- called by ------------------------------------------------------
!||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    vinter                        ../engine/source/tools/curve/vinter.F
!||    strainrate_dependency_125s  ../engine/source/materials/mat/mat125/strainrate_dependency_125s.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||    strainrate_dependency_125s_mod  ../engine/source/materials/mat/mat125/strainrate_dependency_125s_mod.F90
!||====================================================================
        subroutine sigeps125(                                         &
          nel      ,nuvar    ,uvar     ,nvartmp,   vartmp    ,       &
          matparam,rho0   ,                                          &
          epsxx    ,epsyy    ,epszz    ,epsxy    ,epsyz    ,epszx   , &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx  , &
          ssp      ,epsp     ,dmg      ,off)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use precision_mod, only : WP
          use strainrate_dependency_125s_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include  "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables
          integer, intent(in) :: nvartmp !< number of user variables temporairy
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< user variables temporairy 
          !
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
          real(kind=WP), dimension(nel), intent(in) :: rho0 !< material density
          real(kind=WP), dimension(nel), intent(in) :: epsp   !<  global equiv. strain rate
          real(kind=WP), dimension(nel), intent(in) :: epsxx !< total strain  xx
          real(kind=WP), dimension(nel), intent(in) :: epsyy !< total strain  yy
          real(kind=WP), dimension(nel), intent(in) :: epszz !< total strain  zz
          real(kind=WP), dimension(nel), intent(in) :: epsxy !< total strain  xy
          real(kind=WP), dimension(nel), intent(in) :: epsyz !< total strain  yz
          real(kind=WP), dimension(nel), intent(in) :: epszx !< total strain  zx

          real(kind=WP), dimension(nel), intent(out) :: signxx !< new stress xx
          real(kind=WP), dimension(nel), intent(out) :: signyy !< new stress yy
          real(kind=WP), dimension(nel), intent(out) :: signzz !< new stress zz
          real(kind=WP), dimension(nel), intent(out) :: signxy !< new stress xy
          real(kind=WP), dimension(nel), intent(out) :: signyz !< new stress yz
          real(kind=WP), dimension(nel), intent(out) :: signzx !< new stress zx
          real(kind=WP), dimension(nel), intent(inout) :: ssp !< sound speed
          real(kind=WP), dimension(nel,13), intent(inout) ::  dmg
          real(kind=WP), dimension(nel), intent(inout) ::  off
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          !-----------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: fs, i,nfunc,idx, ndx
          integer , dimension(nel) :: iad,ipos,ilen,indx
          real(kind=WP)                                                    &
            :: e1,e2,nu12,nu21,slimt1,slimc1, slimt2,slimc2,               &
            slims,gammaf,gammar, tsdm, d,                                  &
            w11,w22,w12,invd,                                              &
            g12,limit_sig, eint, deint,a11,tauxy, w33,w13,w23,             &
            g13,g23,nu13,nu31,nu23,nu32,slimt3,slimc3,tauzx,               &
            tauyz,c11,c12,c13,c21,c22,c23,c31,c32,c33,                     &
            e3, slims13,slims23,scale,limit_strain,x,eps_eq
          real(kind=WP) , dimension(nel) ::  em11t,xt,em11c,xc, em22t,yt,  &
            em22c,yc,gamma,tau,ems,sc,    &
            ef11t,ef11c, m1t,m1c,al1t,    &
            al2t,al2c,als,m3t,m3c,        &
            al3t,al3c,als13, als23,       &
            ms13,ms23,al1c,ef22c,         &
            ef22t,ef33c,ef33t, efs13,efs23, &
            em33c,em33t, ems13,ems23,      &
            sc13,sc23,zc,zt,gamma1, gamma2, &
            tau1,tau2, m2t, m2c, efs , ms,  &
            check,epsfailure

          real(kind=WP) , dimension(nel) ::  yy, dydx
!!======================================================================
!
          ! FS  ! type of failure yield surface method
          !  =  -1
          !  =  0 ! not available
          !  =  1 ! not available
! ----------------------------------------------------------------------------------------------------------------------
          ! Material parameters
          e1    = matparam%uparam(1)
          e2    = matparam%uparam(2)
          e3    = matparam%uparam(3)
          g12   = matparam%uparam(4)
          g13   = matparam%uparam(5)
          g23   = matparam%uparam(6)
          nu12  = matparam%uparam(8)
          nu13  = matparam%uparam(9)
          nu23  = matparam%uparam(10)
          nu21  = matparam%uparam(78)
          nu31  = matparam%uparam(79)
          nu32  = matparam%uparam(80)
          ! Fiber direction
          em11t(1:nel)  = matparam%uparam(11)
          xt(1:nel)     = matparam%uparam(12)
          slimt1 = matparam%uparam(13)
          em11c(1:nel)  = matparam%uparam(14)
          xc(1:nel)     = matparam%uparam(15)
          slimc1 = matparam%uparam(16)
          ! Matrix direction
          em22t(1:nel)  = matparam%uparam(17)
          yt(1:nel)     = matparam%uparam(18)
          slimt2 = matparam%uparam(19)
          em22c(1:nel)  = matparam%uparam(20)
          yc(1:nel)     = matparam%uparam(21)
          slimc2 = matparam%uparam(22)
          !
          ! Matrix direction
          em33t(1:nel)  = matparam%uparam(28)
          zt(1:nel)     = matparam%uparam(29)
          slimt3 = matparam%uparam(30)
          em33c(1:nel)  = matparam%uparam(31)
          zc(1:nel)     = matparam%uparam(32)
          slimc3 = matparam%uparam(33)
          ! shear
          gamma(1:nel) = matparam%uparam(23)
          tau(1:nel)   = matparam%uparam(24)
          ems(1:nel)   = matparam%uparam(25)
          sc(1:nel)    = matparam%uparam(26)
          slims = matparam%uparam(27)
          ! transversal shear
          !
          gamma1(1:nel) = matparam%uparam(34)
          tau1(1:nel)   = matparam%uparam(35)
          ems13(1:nel)  = matparam%uparam(36)
          sc13(1:nel)   = matparam%uparam(37)
          slims13= matparam%uparam(38)
          !
          gamma2(1:nel)  = matparam%uparam(39)
          tau2(1:nel)    = matparam%uparam(40)
          ems23(1:nel)   = matparam%uparam(41)
          sc23(1:nel)     = matparam%uparam(42)
          slims23     = matparam%uparam(43)
          !
          gammaf = matparam%uparam(44)
          gammar = matparam%uparam(45)
          tsdm   = matparam%uparam(46)
          epsfailure(1:nel) = matparam%uparam(47)
          !
          ! parameters of damage ex fon : exp(-(e/ef)**m/alpha)
          ef11t(1:nel) = matparam%uparam(49)
          m1t(1:nel)  = matparam%uparam(50)
          al1t(1:nel)  = matparam%uparam(51)
          ef11c(1:nel) = matparam%uparam(52)
          m1c(1:nel)  = matparam%uparam(53)
          al1c(1:nel) = matparam%uparam(54)

          ef22t(1:nel) = matparam%uparam(55)
          m2t(1:nel)   = matparam%uparam(56)
          al2t(1:nel)  = matparam%uparam(57)
          ef22c(1:nel) = matparam%uparam(58)
          m2c(1:nel)   = matparam%uparam(59)
          al2c(1:nel)  = matparam%uparam(60)

          ef33t(1:nel) = matparam%uparam(61)
          m3t(1:nel)  = matparam%uparam(62)
          al3t(1:nel)  = matparam%uparam(63)
          ef33c(1:nel)= matparam%uparam(64)
          m3c(1:nel)   = matparam%uparam(65)
          al3c(1:nel)  = matparam%uparam(66)
          ! dir 12
          efs(1:nel)  = matparam%uparam(67)
          ms(1:nel)   = matparam%uparam(68)
          als(1:nel)  = matparam%uparam(69)
          ! dir 13
          efs13(1:nel)  = matparam%uparam(70)
          ms13(1:nel)   = matparam%uparam(71)
          als13(1:nel)  = matparam%uparam(72)
          ! dir 23
          efs23(1:nel) = matparam%uparam(73)
          ms23(1:nel)  = matparam%uparam(74)
          als23(1:nel) = matparam%uparam(75)
          !
          fs = nint(matparam%uparam(76))
          ! update of parameters
           nfunc = matparam%ntable 
           if(nfunc > 0) call strainrate_dependency_125s(nel, matparam , epsp, vartmp, nvartmp ,  &
                                                        em11t,    xt,     em11c,    xc,      &
                                                        em22t,    yt,     em22c,    yc,      &
                                                        em33t,    zt,     em33c,    zc,      &
                                                        gamma,    tau,    ems,      sc,      &
                                                        gamma1,   tau1,   ems13,    sc13,    &
                                                        gamma2,   tau2,   ems23,    sc23,    &
                                                        al1t,     m1t,    al1c,     m1c,     &
                                                        al2t,     m2t,    al2c,     m2c,     &
                                                        al3t,     m3t,    al3c,      m3c,    &
                                                        als,       ms,    als13,    ms13,    &
                                                        als23,     ms23,   ef11t,   ef11c,   &
                                                        ef22t,     ef22c,  ef33t,   ef33c,   &
                                                        efs  ,     efs13, efs23,     epsfailure    ) 
          ! element deletion check
          ndx = 0
          indx(:)=0
          do i=1,nel
               eps_eq =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epszz(i)**2 + &
                                     epsxy(i)**2 + epsyz(i)**2 + epszx(i)**2 ) 
              eps_eq = sqrt(eps_eq)
              if(off(i) < one ) then
                   off(i) = four_over_5*off(i)
                   if(off(i) < em01) off(i) = zero
              elseif(eps_eq >= epsfailure(i) ) then
                   off(i) = four_over_5
               endif  
          enddo
         !
          do i=1,nel
              if(off(i) == zero) then
                 signxx(i) = zero
                 signyy(i) = zero
                 signzz(i) = zero
                 signxy(i) = zero
                 signyz(i) = zero
                 signzx(i) = zero
              else 
                 ndx = ndx + 1
                 indx(ndx) = i
              endif
          end do
          if(ndx == 0) return
       !
          select  case (fs)
           case(-1)
            ! uncoupled formulation
            do idx  = 1,ndx
              i = indx(idx)
              !
              w11 = dmg(i,2)
              w22 = dmg(i,3)
              w33 = dmg(i,4)
              w12 = dmg(i,5)
              w13 = dmg(i,6)
              w23 = dmg(i,7)
              !
              check(i) = one
              d = (one - w11*w22*w33*nu12*nu23*nu31 - w11*w22*w33*nu21*nu32*nu13        &
                - w11*w33*nu31*nu13 - w22*w33*nu23*nu32 - w11*w22*nu12*nu21 )
              c11 = (one - w22*w33*nu23*nu32)*w11*e1
              c22 = (one - w11*w33*nu13*nu31)*w22*e2
              c33 = (one - w11*w22*nu12*nu21)*w33*e3
              c12 = w11*w22*(nu21 + w33*nu23*nu31)*e1
              c21 = w11*w22*(nu12 + w33*nu13*nu32)*e2
              c31 = w11*w33*(nu13 + w22*nu12*nu23)*e3
              c13 = w11*w33*(nu31 + w22*nu21*nu32)*e1
              c32 = w22*w33*(nu23 + w11*nu13*nu21)*e3
              c23 = w22*w33*(nu32 + w11*nu12*nu31)*e2

              invd = one/(max(em20,d))
              signxx(i) = invd*(c11*epsxx(i) + c12*epsyy(i) + c13*epszz(i))
              signyy(i) = invd*(c21*epsxx(i) + c22*epsyy(i) + c23*epszz(i))
              signzz(i) = invd*(c31*epsxx(i) + c32*epsyy(i) + c33*epszz(i))
              signxy(i) = w12*g12*epsxy(i)
              signzx(i) = w13*g13*epszx(i)
              signyz(i) = w23*g23*epsyz(i)

              eint =  half*(epsxx(i)*signxx(i) + epsyy(i)*signyy(i) + epszz(i)*signzz(i)    &
                + epsxy(i)*signxy(i) + epszx(i)*signzx(i) + epsyz(i)*signyz(i))
              deint = eint - uvar(i,1)
              uvar(i,1) = eint
              if(deint < zero ) then
                check(i) = -one
              else
                check(i) = one
              end if
              limit_strain = epsxx(i)**2 + epsyy(i)**2 + epszz(i)**2 +  &
                epsxy(i)**2  + epszx(i)**2 + epsyz(i)**2
              if(check(i) >= zero .and. limit_strain > uvar(i,2) .and. dmg(i,1) /= two .and. dmg(i,1) >= zero) then
                 w11 = one
                 w22 = one 
                 w33 = one
                 w12 = one
                 w23 = one
                 w13 = one
                if(epsxx(i) > zero )then
                  w11 =  epsxx(i)/ef11t(i)
                  w11 = exp(m1t(i)*log(w11))/al1t(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                elseif(epsxx(i) < zero) then
                  w11 = abs(epsxx(i))/ef11c(i)
                  w11 = exp(m1c(i)*log(w11))/al1c(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                end if
                ! dir 22
                if(epsyy(i) > zero )then
                  w22 = epsyy(i)/ef22t(i)
                  w22 = exp(m2t(i)*log(w22))/al2t(i)  ! (esp/epsf)^m/alpha
                  w22 = exp(-w22)
                elseif(epsyy(i) < zero) then
                  w22 = abs(epsyy(i))/ef22c(i)
                  w22 = exp(m2c(i)*log(w22))/al2c(i)  ! (esp/epsf)^m/alpha
                  w22 = exp(-w22)
                end if
                ! dir 33
                if(epszz(i) > zero )then
                  w33 = epszz(i)/ef33t(i)
                  w33 = exp(m3t(i)*log(w33))/al3t(i)  ! (esp/epsf)^m/alpha
                  w33 = exp(-w33)
                elseif(epszz(i) < zero) then
                  w33 = abs(epszz(i))/ef33c(i)
                  w33 = exp(m3c(i)*log(w33))/al3c(i)  ! (esp/epsf)^m/alpha
                  w33 = exp(-w33)
                end if
                if(epsxy(i) /= zero) then
                  w12 = abs(epsxy(i))/efs(i)
                  w12 = exp(ms(i)*log(w12))/als(i)  ! (esp/epsf)^m/alpha
                  w12 = exp(-w12)
                endif
                !
                if(epszx(i) /= zero) then
                  w13 = abs(epszx(i))/efs13(i)
                  w13 = exp(ms13(i)*log(w13))/als13(i) !
                  w13 = exp(-w13)
                endif
                !
                if(epsyz(i) /= zero) then
                 w23 = abs(epsyz(i))/efs23(i)
                 w23 = exp(ms23(i)*log(w23))/als23(i)
                 w23 = exp(-w23)
                endif
              else ! unlaod
                w11 = dmg(i,2)
                w22 = dmg(i,3)
                w33 = dmg(i,4)
                w12 = dmg(i,5)
                w13 = dmg(i,6)
                w23 = dmg(i,7)
                if(check(i)  > zero) then
                  if( ( limit_strain < uvar(i,2) .and. dmg(i,1) > zero )  .or.       &
                    ( limit_strain > uvar(i,2) .and. dmg(i,1) < zero ) ) dmg(i,1) = -dmg(i,1)
                end if
              end if
              uvar(i,2) = max(uvar(i,2), limit_strain)
              ! damage hook matrix
              d = (one - w11*w22*w33*nu12*nu23*nu31 - w11*w22*w33*nu21*nu32*nu13        &
                - w11*w33*nu31*nu13 - w22*w33*nu23*nu32 - w11*w22*nu12*nu21 )
              c11 = (one - w22*w33*nu23*nu32)*w11*e1
              c22 = (one - w11*w33*nu13*nu31)*w22*e2
              c33 = (one - w11*w22*nu12*nu21)*w33*e3
              c12 = w11*w22*(nu21 + w33*nu23*nu31)*e1
              c21 = w11*w22*(nu12 + w33*nu13*nu32)*e2
              c31 = w11*w33*(nu13 + w22*nu12*nu23)*e3
              c13 = w11*w33*(nu31 + w22*nu21*nu32)*e1
              c32 = w22*w33*(nu23 + w11*nu13*nu21)*e3
              c23 = w22*w33*(nu32 + w11*nu12*nu31)*e2
              !
              invd = one/(max(em20,d))
              signxx(i) = invd*(c11*epsxx(i) + c12*epsyy(i) + c13*epszz(i))
              signyy(i) = invd*(c21*epsxx(i) + c22*epsyy(i) + c23*epszz(i))
              signzz(i) = invd*(c31*epsxx(i) + c32*epsyy(i) + c33*epszz(i))
              signxy(i) = w12*g12*epsxy(i)
              signzx(i) = w13*g13*epszx(i)
              signyz(i) = w23*g23*epsyz(i)
              ! shear treatement
              if(abs(signxy(i)) >= tau(i) .and. ems(i) > gamma(i) ) then
                scale =  (sc(i) - tau(i))/(ems(i)- gamma(i))
                tauxy = tau(i) + scale*(abs(epsxy(i)) - gamma(i))
                signxy(i) = sign(tauxy,signxy(i))
              end if
              if(abs(signzx(i)) >= tau1(i) .and. ems13 (i) > gamma1(i) ) then
                scale =  (sc13(i) - tau1(i))/(ems13 (i)- gamma1(i))
                tauzx = tau1(i) + scale*(abs(epszx(i)) - gamma1(i))
                signzx(i) = sign(tauzx,signzx(i))
              end if
              if(abs(signyz(i)) >= tau2(i) .and. ems23(i) > gamma2(i) ) then
                scale =  (sc23(i)- tau2(i))/(ems23(i) - gamma2(i))
                tauyz = tau2(i) + scale*(abs(epsyz(i)) - gamma2(i))
                signyz(i) = sign(tauyz,signyz(i))
              end if
              !
              !
              if(abs(dmg(i,1)) < one) then
                dmg(i,8) =  max(zero, epsxx(i)/em11t(i), signxx(i)/xt(i))
                if(signxx(i) < zero) dmg(i,8) =  max(zero, abs(epsxx(i))/em11c(i), abs(signxx(i))/xc(i))
                dmg(i,8) = min(dmg(i,8), one)

                dmg(i,9) =  max(zero, epsyy(i)/em22t(i), signyy(i)/yt(i))
                if(signyy(i) < zero) dmg(i,9) =  max(zero, abs(epsyy(i))/em22c(i), abs(signyy(i))/yc(i))
                dmg(i,9) = min(dmg(i,9), one)

                dmg(i,10) =  max(zero, epszz(i)/em33t(i), signzz(i)/zt(i))
                if(signzz(i) < zero) dmg(i,10) =  max(zero, abs(epszz(i))/em33c(i), abs(signzz(i))/zc(i))
                dmg(i,10) = min(dmg(i,10), one)
                dmg(i,11) = max(zero, abs(signxy(i))/sc(i))
                dmg(i,12) = max(zero, abs(signzx(i))/sc13(i))
                dmg(i,13) = max(zero, abs(signyz(i))/sc23(i))
                dmg(i,1) = max(dmg(i,8),dmg(i,9),dmg(i,10),dmg(i,11),dmg(i,12),dmg(i,13))
              endif
              !
              if( check(i) >= zero ) then
                if( dmg(i,1) >= one ) then
                  if(dmg(i,8) >= one  ) then
                    if(signxx(i) >= zero .and. (signxx(i) <=  slimt1*xt(i) .or. dmg(i,8) == two )) then
                      limit_sig = slimt1*xt(i)
                      signxx(i) = limit_sig
                      signyy(i) = slimt1*signyy(i)
                      signzz(i) = slimt1*signzz(i)
                      signxy(i) = slimt1*signxy(i)
                      signzx(i) = slimt1*signzx(i)
                      signyz(i) = slimt1*signyz(i)
                      w11 = signxx(i) / epsxx(i)/e1
                      dmg(i,8) = two
                    elseif(signxx(i) < zero .and. ( signxx(i) >= -slimc1*xc(i) .or. dmg(i,8) == two)) then
                      limit_sig = slimc1*xc(i)
                      signxx(i) = - limit_sig
                      signyy(i) = slimc1*signyy(i)
                      signzz(i) = slimc1*signzz(i)
                      signxy(i) = slimc1*signxy(i)
                      signzx(i) = slimc1*signzx(i)
                      signyz(i) = slimc1*signyz(i)
                      w11 = signxx(i) / epsxx(i)/e1
                      dmg(i,8) = two
                    endif
                  elseif(dmg(i,9) >= one ) then
                    if(signyy(i) >= zero .and. (signyy(i) <=  slimt2*yt(i) .or. dmg(i,9) == two )) then
                      limit_sig = slimt2*yt(i)
                      signyy(i) = limit_sig
                      signxx(i) = slimt2*signxx(i)
                      signzz(i) = slimt2*signzz(i)
                      signxy(i) = slimt2*signxy(i)
                      signzx(i) = slimt2*signzx(i)
                      signyz(i) = slimt2*signyz(i)
                      w22 = signyy(i) / epsyy(i) / e2
                      dmg(i,9) = two
                    elseif(signyy(i) < zero .and. ( signyy(i) >= -slimc2*yc(i) .or. dmg(i,9) == two)) then
                      limit_sig = slimc2*yc(i)
                      signyy(i) = - limit_sig
                      signxx(i) = slimc2*signxx(i)
                      signzz(i) = slimc2*signzz(i)
                      signxy(i) = slimc2*signxy(i)
                      signzx(i) = slimc2*signzx(i)
                      signyz(i) = slimc2*signyz(i)
                      w22 = signyy(i) / epsyy(i)/e2
                      dmg(i,9) = two
                    endif
                  elseif(dmg(i,10) >= one ) then
                    if(signzz(i) > zero .and. (signzz(i) <=  slimt3*zt(i) .or. dmg(i,10) == two )) then
                      limit_sig = slimt3*zt(i)
                      signzz(i) = limit_sig
                      signxx(i) = slimt3*signxx(i)
                      signyy(i) = slimt3*signyy(i)
                      signxy(i) = slimt3*signxy(i)
                      signzx(i) = slimt3*signzx(i)
                      signyz(i) = slimt3*signyz(i)
                      w33 = signzz(i) / epszz(i)/e3
                      dmg(i,10) = two
                    else if(signzz(i) < zero .and. ( signzz(i) >= -slimc3*zc(i) .or. dmg(i,10) == two)) then
                      limit_sig = slimc3*zc(i)
                      signzz(i) = - limit_sig
                      signxx(i) = slimc3*signxx(i)
                      signyy(i) = slimc3*signyy(i)
                      signxy(i) = slimc3*signxy(i)
                      signzx(i) = slimc3*signzx(i)
                      signyz(i) = slimc3*signyz(i)
                      w33 = signzz(i) / epszz(i)/e3
                      dmg(i,10) = two
                    endif
                  elseif((dmg(i,11) ==  one .and. abs(signxy(i)) >= slims*sc(i) ) .or. dmg(i,11) == two) then
                    signxy(i) = sign( slims*sc(i), signxy(i))
                    w12 = signxy(i)/epsxy(i)/g12
                    dmg(i,11 ) = two
                  elseif((dmg(i,12) ==  one .and. abs(signzx(i)) >= slims13*sc13(i) ) .or. dmg(i,12) == two) then
                    signzx(i) = sign(slims13*sc13(i), signzx(i))
                    dmg(i,12) = two
                    w13 = signzx(i)/epszx(i)/g13
                  elseif((dmg(i,13) ==  one .and. abs(signyz(i)) >= slims23*sc23(i) ) .or. dmg(i,13) == two) then
                    signyz(i) = sign(slims23*sc23(i), signyz(i))
                    dmg(i,13) = two
                    w23 = signyz(i)/epsyz(i)/g23
                  end if ! dmg(i,13) >= one
                  dmg(i,1) = max(dmg(i,1), dmg(i,8),dmg(i,9),dmg(i,10),dmg(i,11),dmg(i,12),dmg(i,13))
                elseif(dmg(i,1) == -two) then  ! to follow zero stress when slim = zero  in loading and unloading
                   if(dmg(i,8) >= one  ) then
                    if(signxx(i) >= zero .and. slimt1 == zero ) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                     !! dmg(i,8) = two
                    elseif(signxx(i) < zero .and. -slimc1 == zero) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                     !! dmg(i,8) = two
                    endif
                  elseif(dmg(i,9) >= one ) then
                    if(signyy(i) >= zero .and. slimt2 == zero ) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                    !!  dmg(i,9) = two
                    elseif(signyy(i) < zero .and. slimc2 == zero) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                    !!  dmg(i,9) = two
                    endif
                  elseif(dmg(i,10) >= one ) then
                    if(signzz(i) > zero .and.  slimt3 == zero ) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                     !! dmg(i,10) = two
                    else if(signzz(i) < zero .and. -slimc3 == zero) then
                      signxx(i) = zero
                      signyy(i) = zero
                      signzz(i) = zero
                      signxy(i) = zero
                      signzx(i) = zero
                      signyz(i) = zero
                     !! dmg(i,10) = two
                    endif
                  elseif( dmg(i,11) >= one .and.  slims == zero ) then
                    signxy(i) = zero
                  elseif( dmg(i,12) >=  one .and.  slims13 == zero ) then
                    signzx(i) = zero
                  elseif(dmg(i,13) >=  one .and. slims23 == zero) then
                    signyz(i) = zero
                  end if ! dmg(i,13) >= one
                endif ! dmg_g(i) >= one
                ! save w11 & w22 & w33
                dmg(i,2) = w11
                dmg(i,3) = w22
                dmg(i,4) = w33
                dmg(i,5) = w12
                dmg(i,6) = w13
                dmg(i,7) = w23
              end if ! check
              a11       = max(e1,e2,e3)  ! bulk + G*4/3 ????
              ssp(i) = sqrt(a11/rho0(i))
            end do ! nel loop
           case(0)
            ! not available
           case(1)
            ! not available
          end select
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine sigeps125
      end module sigeps125_mod
