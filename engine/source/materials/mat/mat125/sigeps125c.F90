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
!||    sigeps125c_mod   ../engine/source/materials/mat/mat125/sigeps125c.F90
!||--- called by ------------------------------------------------------
!||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
      module sigeps125c_mod
        implicit none
      contains
        ! ======================================================================================================================
        ! \brief   material law /MAT/LAW125
        ! \details Material law  Dedicated to composite application.
        ! ======================================================================================================================
!||====================================================================
!||    sigeps125c         ../engine/source/materials/mat/mat125/sigeps125c.F90
!||--- called by ------------------------------------------------------
!||    mulawc             ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    vinter             ../engine/source/tools/curve/vinter.F
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE sigeps125c(                                   &
          nel     ,mat_param  , nuvar   ,uvar   ,                &
          rho     ,thk       ,thkly     ,shf    ,                &
          nfunc   ,ifunc     ,npf       ,tf     , snpc   ,       &
          stf     ,epsp      ,                                   &
          depsxx  ,depsyy    ,depsxy ,                           &
          epsxx   ,epsyy     ,epsxy   ,epsyz   ,epszx ,          &
          sigoxx  ,sigoyy    ,sigoxy,                            &
          signxx  ,signyy    ,signxy  ,signzx   ,signyz  ,       &
          off     ,sigy      ,etse    ,ssp      ,dmg     ,       &
          offply  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use precision_mod, only : WP
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
          integer, intent(in) :: nfunc  !< number of function
          integer, intent(in) :: snpc  !<
          integer, intent(in) :: stf  !<
          integer, intent(in) :: ifunc(nfunc),npf(snpc) !< function parameters
          !!
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          real(kind=WP), dimension(nel), intent(in) :: rho !< material density
          real(kind=WP), dimension(nel), intent(inout) :: sigy !< yield stress
          real(kind=WP), dimension(nel), intent(inout) :: shf !< shear factor correction
          real(kind=WP), dimension(nel), intent(inout) :: thk !< shell thikness
          real(kind=WP), dimension(nel), intent(in)    :: thkly !< ply thikness
          real(kind=WP), dimension(stf), intent(in) :: tf
          real(kind=WP), dimension(nel), intent(in) :: epsp   !<  global equiv. strain rate
          real(kind=WP), dimension(nel), intent(inout) :: etse !< ratio of rigidity
          real(kind=WP), dimension(nel), intent(in) :: sigoxx !< old stress xx
          real(kind=WP), dimension(nel), intent(in) :: sigoyy !< old stress yy
          real(kind=WP), dimension(nel), intent(in) :: sigoxy !< old stress yy

          real(kind=WP), dimension(nel), intent(in) :: depsxx !< incremental strain xx
          real(kind=WP), dimension(nel), intent(in) :: depsyy !< incremental strain yy
          real(kind=WP), dimension(nel), intent(in) :: depsxy !< incremental strain xy
          real(kind=WP), dimension(nel), intent(in) :: epsxx !< total strain xx
          real(kind=WP), dimension(nel), intent(in) :: epsyy !< total strain yy
          real(kind=WP), dimension(nel), intent(in) :: epsxy !< total strain xy
          real(kind=WP), dimension(nel), intent(in) :: epsyz !< total strain yz
          real(kind=WP), dimension(nel), intent(in) :: epszx !< total strain zx
          real(kind=WP), dimension(nel), intent(out) :: signxx !< new stress xx
          real(kind=WP), dimension(nel), intent(out) :: signyy !< new stress yy
          real(kind=WP), dimension(nel), intent(out) :: signxy !< new stress xy
          real(kind=WP), dimension(nel), intent(out) :: signyz !< new stress yz
          real(kind=WP), dimension(nel), intent(out) :: signzx !< new stress zx
          real(kind=WP), dimension(nel), intent(inout) :: ssp !< sound speed
          real(kind=WP), dimension(nel), intent(inout) :: off !< element deletion flag
          real(kind=WP), dimension(nel,13), intent(inout) ::  dmg
          real(kind=WP), dimension(nel), intent(inout) :: offply !< ply element deletion flag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: fs, i
          integer , dimension(nel) :: iad,ipos,ilen

          real(kind=WP)                                                       &
            :: e1,e2,nu12,nu21,em11t0,xt0,slimt1,em11c0,xc0,slimc1,        &
            em22t0,yt0,slimt2,em22c0,yc0,slimc2,gamma0,tau0,ems0,sc0,      &
            slims,gammaf,gammar, tsdm, erods,tsize,e1d,e2d,g12d,d,         &
            w11,w22,w12,e12d,invd, ems013,ems023,sc013,sc023,              &
            gamma02,gamma03,tau02,tau03, limit_sig ,limit_strain,          &
            e21d,g12, eint, deint,a11,tauxy,g13,g23,scale
          !
          real(kind=WP) , dimension(nel) ::  dezz,check,xc_r, em11t,xt,em11c,xc
          real(kind=WP) , dimension(nel) ::  em22t,yt,em22c,yc,gamma,tau,ems,sc
          real(kind=WP) , dimension(nel) ::  gamma2,tau2,ems13,sc13,gamma3,tau3
          real(kind=WP) , dimension(nel) ::  ems23,sc23
          real(kind=WP) , dimension(nel) ::  ef11t,m1t,al1t,ef11c,m1c,al1c
          real(kind=WP) , dimension(nel) ::  ef22t,m2t,al2t,ef22c,m2c,al2c
          real(kind=WP) , dimension(nel) ::  efs,ms,als
          real(kind=WP) , dimension(nel) ::  yy, dydx
!!======================================================================
!
          ! FS ! type of failure yield surface loading
          !  =  -1
          !  =  0  ! not available
          !  =  1  ! not available
! ----------------------------------------------------------------------------------------------------------------------
          ! Material parameters
          e1    = mat_param%uparam(1)
          e2    = mat_param%uparam(2)
          g12   = mat_param%uparam(4)
          g13   = mat_param%uparam(5)
          g23   = mat_param%uparam(6)
          nu12  = mat_param%uparam(8)
          nu21  = mat_param%uparam(78)
          ! Fiber direction
          em11t0         = mat_param%uparam(11)
          xt0            = mat_param%uparam(12)
          slimt1         = mat_param%uparam(13)
          em11c0         = mat_param%uparam(14)
          xc0            = mat_param%uparam(15)
          xc_r(1:nel)    = xc
          slimc1         = mat_param%uparam(16)
          ! Matrix direction
          em22t0         = mat_param%uparam(17)
          yt0            = mat_param%uparam(18)
          slimt2         = mat_param%uparam(19)
          em22c0         = mat_param%uparam(20)
          yc0            = mat_param%uparam(21)
          slimc2         = mat_param%uparam(22)
          ! shear
          gamma0        = mat_param%uparam(23)
          tau0          = mat_param%uparam(24)
          ems0          = mat_param%uparam(25)
          sc0           = mat_param%uparam(26)
          slims         = mat_param%uparam(27)
          ! shear parameters for directions 13 and 23
          gamma02       = 0
          tau02         = 0
          ems013        = 0
          sc013         = 0
          gamma03       = 0
          tau03         = 0
          ems023        = 0
          sc023         = 0
          !
          gammaf = mat_param%uparam(44)
          gammar = mat_param%uparam(45)
          tsdm   = mat_param%uparam(46)
          !
          erods = mat_param%uparam(47)
          tsize = mat_param%uparam(48)
          ! parameters of damage ex fon : exp(-(e/ef)**m/alpha)
          ef11t(1:nel) = mat_param%uparam(49)
          m1t(1:nel)  = mat_param%uparam(50)
          al1t(1:nel) = mat_param%uparam(51)

          ef11c(1:nel) = mat_param%uparam(52)
          m1c (1:nel)  = mat_param%uparam(53)
          al1c(1:nel) = mat_param%uparam(54)

          ef22t(1:nel) = mat_param%uparam(55)
          m2t(1:nel)   = mat_param%uparam(56)
          al2t(1:nel)  = mat_param%uparam(57)
          ef22c(1:nel) = mat_param%uparam(58)
          m2c(1:nel)   = mat_param%uparam(59)
          al2c(1:nel) = mat_param%uparam(60)
          !
          efs(1:nel) = mat_param%uparam(67)
          ms(1:nel)   = mat_param%uparam(68)
          als(1:nel)  = mat_param%uparam(69)
          offply(1:nel) = one
          !
          fs = nint(mat_param%uparam(76))
          ! strain rate dependency of strength
          !!  call damage_parameter (ifunc)
          ! fiber - tension dir 1 -
          if(ifunc(1) /= 0) then  ! em11t
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(1)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(1)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            em11t(1:nel)= yy(1:nel)
          else
            em11t(1:nel) = em11t0
          end if
          !
          if(ifunc(2) /= 0) then  ! em11t
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(2)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(2)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            xt(1:nel)= yy(1:nel)
          else
            xt(1:nel) = xt0
          end if
          ! fiber - compression  dir 1 -
          if(ifunc(3) /= 0) then  ! em11c
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(3)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(3)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            em11c(1:nel)= yy(1:nel)
          else
            em11c(1:nel) = em11c0
          end if
          if(ifunc(4) /= 0) then  ! xc
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(4)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            xc(1:nel)= yy(1:nel)
          else
            xc(1:nel) = xc0
          end if
          ! matrix - tension dir 2 -
          if(ifunc(5) /= 0) then  !
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(5)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(5)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            em22t(1:nel)= yy(1:nel)
          else
            em22t(1:nel) = em22t0
          end if
          !
          if(ifunc(6) /= 0) then  ! em11t
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(6)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(6)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            yt(1:nel)= yy(1:nel)
          else
            yt(1:nel) = yt0
          end if
          ! matrix - compression  dir 2 -
          if(ifunc(7) /= 0) then  ! em11c
            ipos(1:nel) = 0
            iad (1:nel) = npf(ifunc(7)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(7)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            em22c(1:nel)= yy(1:nel)
          else
            em22c(1:nel) = em22c0
          end if
          if(ifunc(8) /= 0) then  ! xc
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(8)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(8)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            yc(1:nel)= yy(1:nel)
          else
            yc(1:nel) = yc0
          end if

          ! shear  12 - gamma
          if(ifunc(13) /= 0) then  !
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(13)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(13)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            gamma(1:nel)= yy(1:nel)
          else
            gamma(1:nel) = gamma0
          end if
          ! shear  tau
          if(ifunc(14) /= 0) then  ! tau
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(14)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(14)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            tau(1:nel)= yy(1:nel)
          else
            tau(1:nel) = tau0
          end if
          ! shear strain 12 - ems
          if(ifunc(15) /= 0) then  ! em11c
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(15)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(15)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            ems(1:nel)= yy(1:nel)
          else
            ems(1:nel) = ems0
          end if
          ! shear strengh sc
          if(ifunc(16) /= 0) then  ! sc
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(16)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(16)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            sc(1:nel)= yy(1:nel)
          else
            sc(1:nel) = sc0
          end if
          ! shear  13 - gamma2
          if(ifunc(17) /= 0) then  !
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(17)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(17)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            gamma2(1:nel)= yy(1:nel)
          else
            gamma2(1:nel) = gamma02
          end if
          ! shear  tau 2
          if(ifunc(18) /= 0) then  ! tau
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(18)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(18)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            tau2(1:nel)= yy(1:nel)
          else
            tau2(1:nel) = tau02
          end if
          ! shear strain 13 - ems13
          if(ifunc(19) /= 0) then  ! em11c
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(19)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(19)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            ems13(1:nel)= yy(1:nel)
          else
            ems13(1:nel) = ems013
          end if
          ! shear strengh sc13
          if(ifunc(20) /= 0) then  ! sc
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(20)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(20)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            sc13(1:nel)= yy(1:nel)
          else
            sc13(1:nel) = sc013
          end if
          ! shear  23 - gamma3
          if(ifunc(21) /= 0) then  !
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(21)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(21)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            gamma3(1:nel)= yy(1:nel)
          else
            gamma3(1:nel) = gamma03
          end if
          ! shear  tau 2
          if(ifunc(22) /= 0) then  ! tau
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(22)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(22)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            tau3(1:nel)= yy(1:nel)
          else
            tau3(1:nel) = tau03
          end if
          ! shear strain 23 - ems23
          if(ifunc(23) /= 0) then  !
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(23)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(23)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            ems23(1:nel)= yy(1:nel)
          else
            ems23(1:nel) = ems023
          end if
          ! shear strengh sc23
          if(ifunc(24) /= 0) then  ! sc
            ipos(1:nel) = 1
            iad (1:nel) = npf(ifunc(24)) / 2 + 1
            ilen(1:NEL) = npf(ifunc(24)+1) / 2 - iad(1:nel) - ipos(1:nel)
            CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy)
            sc23(1:nel)= yy(1:nel)
          else
            sc23(1:nel) = sc023
          end if
          ! Computing the damage parameters
          do i=1,nel
            if(xt(i) > zero )then
              ef11t(i)  = xt(i)/e1
              em11t(i) = max(em11t(i), onep1*ef11t(i))
              m1t(i)= -one/log(ef11t(i)/em11t(i))
              al1t(i) = m1t(i)*(em11t(i)/ef11t(i))**m1t(i)
            end if
            if(xc(i) > zero  )then
              ef11c(i)  = xc(i)/e1
              em11c(i) = max(em11c(i), onep1*ef11c(i))
              m1c(i)= -one/log(ef11c(i)/em11c(i))
              al1c(i) = m1c(i)*(em11c(i)/ef11c(i))**m1c(i)
            end if
            if(yt(i) > zero )then
              ef22t(i)  = yt(i)/e2
              em22t(i) = max(em22t(i), onep1*ef22t(i))
              m2t(i) = -one/log(ef22t(i)/em22t(i))
              al2t(i) = m2t(i)*(em22t(i)/ef22t(i))**m2t(i)
            end if
            !
            if(yc(i) > zero  )then
              ef22c(i)  = yc(i)/e1
              em22c(i) = max(em22c(i),onep1*ef22c(i))
              m2c(i) = -one/log(ef22c(i)/em22c(i))
              al2c(i) = m2c(i)*(em22c(i)/ef22c(i))**m2c(i)
            end if
            if(tau(i) > zero  )then
              efs(i)  = tau(i) /g12
              gamma(i) = max(gamma(i),onep1*efs(i))
              ms(i) = -one/log(efs(i)/gamma(i)) ! one/ln(epsm/epsf)
              als(i) = ms(i)*(gamma(i)/efs(i))**ms(i)
            end if
          end do ! nel
          ! check loading/unloading

          do i=1,nel
            ! dir
            check(i) = one
            ! check unloading
            w11 = dmg(i,2)
            w22 = dmg(i,3)
            w12 = dmg(i,4)
            d = (one - w11*w22*nu12*nu21)
            e1d = w11*e1
            e2d = w22*e2
            e12d = w11*w22*nu21*e1
            e21d = w11*w22*nu12*e2
            invd = one/d
            signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
            signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
            signxy(i) = w12*g12*epsxy(i)
            deint = half*(depsxx(i)*(signxx(i) + sigoxx(i))  +                        &
              depsyy(i)*(signyy(i) + sigoyy(i))) +                                    &
              depsxy(i)*(signxy(i) + sigoxy(i))
            eint = uvar(i,1) + deint
            uvar(i,1) = eint
            if(deint < ZERO ) then
              check(i) = -one
            else
              check(i) = one
            end if
          end do !
          !
          ! membrane computing FS = 0, 1, -1
          select  case (fs)
           case(-1)
            ! Uncoupled failure criterion
            !  FS = -1
            do i=1,nel
              limit_strain = epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2
              if(check(i) >= zero .and. limit_strain > uvar(i,2) .and. dmg(i,1) /= two .and. dmg(i,1) >= zero) then
                if(epsxx(i) >= zero )then
                  w11 = epsxx(i)/ef11t(i)
                  w11 = exp(m1t(i)*log(w11))/al1t(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                else
                  w11 = abs(epsxx(i))/ef11c(i)
                  w11 = exp(m1c(i)*log(w11))/al1c(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                end if
                ! dir b
                if(epsyy(i) >= zero )then
                  w22 = epsyy(i)/ef22t(i)
                  w22 = exp(m2t(i)*log(w22))/al2t(i)  ! (esp/epsf)^m/alpha
                  w22 = exp(-w22)
                else
                  w22 = abs(epsyy(i))/ef22c(i)
                  w22 = exp(m2c(i)*log(w22))/al2c(i)  ! (esp/epsf)^m/alpha
                  w22 = exp(-w22)
                end if
              else ! unlaod & reloading (following the same path as unloading)
                w11 = dmg(i,2)
                w22 = dmg(i,3)
                w12 = dmg(i,4)
                !if(check(i)  > zero) then
                !  if( ( limit_strain < uvar(i,2) .and. dmg(i,1) > zero )  .or.       &
                !      ( limit_strain > uvar(i,2) .and. dmg(i,1) < zero ) ) dmg(i,1) = -dmg(i,1)
                !endif
              end if
              ! damage hook matrix
              d = (one - w11*w22*nu12*nu21)
              e1d = w11*e1
              e2d = w22*e2
              e12d = w11*w22*nu21*e1
              e21d = w11*w22*nu12*e2
              invd = one/d
              signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
              signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
              signzx(i) = shf(i)*g12*epszx(i)
              signyz(i) = shf(i)*g12*epsyz(i)
              ! save w11 & w22
              dmg(i,2)= w11
              dmg(i,3)= w22
              !
              etse(i)   = one
              a11       = max(e1,e2)/(one - nu12**2)
              a11       = max(e1,e2)
              ssp(i) = sqrt(a11/rho(i))
              sigy(i)    = min(slimt1*xt(i),slimt2*yt(i), slimc1*xc(i),slimc2*yc(i))
            end do ! nel loop
            ! shear traitement and softening
            do i=1,nel
              ! shear w12
              w12 = one
              w11 = dmg(i,2)
              w22 = dmg(i,3)
              limit_strain = epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2
              if(check(i) >= zero .and. limit_strain > uvar(i,2) .and. dmg(i,1) /= two .and. dmg(i,1) >= zero) then
                if(tau(i) >  zero ) then
                  w12 = abs(epsxy(i))/efs(i)
                  w12 = exp(ms(i)*log(w12))/als(i)  ! (esp/epsf)^m/alpha
                  w12 = exp(-w12)
                end if
              else
                w12 = dmg(i,4)
                if(check(i)  > zero) then
                  if( ( limit_strain < uvar(i,2) .and. dmg(i,1) > zero )  .or.       &
                    ( limit_strain > uvar(i,2) .and. dmg(i,1) < zero ) ) dmg(i,1) = -dmg(i,1)
                end if
              end if
              g12d = w12*g12
              signxy(i) = g12d*epsxy(i)
              if(abs(signxy(i)) >= tau(i) .and. abs(signxy(i)) <  sc(i)) then
                scale =  (sc(i) - tau(i))/(ems(i) - gamma(i))
                tauxy = tau(i) + scale*(abs(epsxy(i)) - gamma(i))
                signxy(i) = sign(tauxy,signxy(i))
              end if
              ! checking loading failure criteria
              if(abs(dmg(i,1)) <  one ) then
                dmg(i,5) = max(epsxx(i)/em11t(i), signxx(i)/xt(i)) ! based on max strain or max strain
                if(signxx(i) < zero) dmg(i,5) = max(-epsxx(i)/em11c(i), -signxx(i)/xc(i))
                dmg(i,6) = max(epsyy(i)/em22t(i), signyy(i)/yt(i))
                if(signyy(i) < zero) dmg(i,6) = max(-epsyy(i)/em22c(i),-signyy(i)/yc(i))
                dmg(i,7) = max( abs(epsxy(i))/ems(i),  abs(signxy(i))/sc(i) )
                if(dmg(i,5) >= zep99 .or. dmg(i,6) >= zep99 .or. dmg(i,7) >= zep99) dmg(i,1) = one
              end if
              if(check(i) >= zero) then
                if(dmg(i,1) >= one  ) then
                  if( dmg(i,5) >= zep99) then
                    if(signxx(i) >= zero .and. (signxx(i) <=  slimt1*xt(i) .or. dmg(i,5) == two )) then !  slimt1*xt(i) ) then
                      limit_sig = slimt1*xt(i)
                      signxx(i) = limit_sig ! max(signxx(i), limit_sig )
                      signyy(i) = slimt1*sigoyy(i)
                      signxy(i) = slimt1*sigoxy(i)
                      dmg(i,5) = two !
                    elseif(signxx(i) < zero .and. ( signxx(i) >= -slimc1*xc(i) .or. dmg(i,5) == two)) then
                      limit_sig = -slimc1*xc(i)
                      signxx(i) = limit_sig !  min(signxx(i),  limit_sig )
                      signyy(i) = slimc1*sigoyy(i)
                      signxy(i) = slimc1*sigoxy(i)
                      dmg(i,5) = two !
                    end if
                  elseif( dmg(i,6) >= zep99)then
                    if(signyy(i) >= zero .and. (signyy(i) <= slimt2*yt(i) .or. dmg(i,6) == two )) then !  slimt2*yt(i) ) then
                      limit_sig = slimt2*yt(i)
                      signyy(i) = limit_sig
                      signxx(i) = slimt2*sigoxx(i)
                      signxy(i) = slimt2*sigoxy(i)
                      dmg(i,6) = two !
                    elseif(signyy(i) < zero .and. ( signyy(i) >= -slimc2*yc(i) .or. dmg(i,6) == two ) ) then
                      limit_sig = -slimc2*yc(i)
                      signyy(i) = limit_sig ! min(signyy(i),limit_sig )
                      signxx(i) = slimc2*sigoxx(i)
                      signxy(i) = slimc2*sigoxy(i)
                      dmg(i,6) = two !
                    end if
                  elseif( dmg(i,7) >= zep99 .and. (abs(signxy(i)) <= slims*sc(i) .or. dmg(i,7) == two )) then
                    limit_sig = slims*sc(i)
                    signxy(i) = sign(limit_sig,signxy(i))
                    signxx(i) = slims*sigoxx(i)
                    signyy(i) = slims*sigoyy(i)
                    dmg(i,7) = two
                  end if ! dmg
                  dmg(i,1) = max(one, dmg(i,5),dmg(i,6),dmg(i,7))
                  if(epsxx(i) /= zero ) w11 = Min(one, abs(signxx(i)/epsxx(i))/e1)
                  if(epsyy(i) /= zero ) w22 = Min(one, abs(signyy(i)/epsyy(i))/e2)
                  if(epsxy(i) /= zero ) w12 = Min(one, abs(signxy(i)/epsxy(i))/g12)
                end if ! dmg(i,1)
                uvar(i,2) = max(uvar(i,2), limit_strain)
              endif ! check
              dmg(i,2) = w11
              dmg(i,3) = w22
              dmg(i,4) = w12
              dezz(i)    = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i))
              thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i)
            end do ! nel
            !  coupling between matrix and shear
            !  FS = 0
            !
           case(0)  ! fs = 0  !  should be added after fixing fs=1 formulation
            ! Coupling failure criterion
            ! matrix/shear
           case(1)
            ! coupled failure criterion
            ! fiber/shear
            ! matrix/shear
            !  FS = 1  ! not finalized. Waiting to understand how we can handle the coupling.

          end select ! FS
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine sigeps125c
      end module sigeps125c_mod
