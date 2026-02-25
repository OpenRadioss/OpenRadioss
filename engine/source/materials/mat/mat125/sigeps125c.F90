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
!||    strainrate_dependency_125c  ../engine/source/materials/mat/mat125/strainrate_dependency_125c.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||    strainrate_dependency_125c_mod  ../engine/source/materials/mat/mat125/strainrate_dependency_125c_mod.F90
!||====================================================================
        SUBROUTINE sigeps125c(                                   &
          nel     ,mat_param  , nuvar   ,uvar   ,nvartmp,        &
          vartmp  ,rho     ,thk       ,thkly     ,shf    ,       &
          epsp      ,                                            &
          depsxx  ,depsyy    ,depsxy ,                           &
          epsxx   ,epsyy     ,epsxy   ,epsyz   ,epszx ,          &
          sigoxx  ,sigoyy    ,sigoxy,                            &
          signxx  ,signyy    ,signxy  ,signzx   ,signyz  ,       &
          off     ,sigy      ,etse    ,ssp      ,dmg     ,       &
          dmg_g   ,offl  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use precision_mod, only : WP
          use strainrate_dependency_125c_mod
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
          !!
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          real(kind=WP), dimension(nel), intent(in) :: rho !< material density
          real(kind=WP), dimension(nel), intent(inout) :: sigy !< yield stress
          real(kind=WP), dimension(nel), intent(inout) :: shf !< shear factor correction
          real(kind=WP), dimension(nel), intent(inout) :: thk !< shell thikness
          real(kind=WP), dimension(nel), intent(in)    :: thkly !< ply thikness
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
          real(kind=WP), dimension(nel), intent(inout) :: dmg_g !< global  deletion flag
          real(kind=WP), dimension(nel), intent(inout) :: offl !< local  deletion flag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: fs, i,nfunc,ndx, idx
          integer , dimension(nel) :: iad,ipos,ilen,indx

          real(kind=WP)                                                       &
            :: e1,e2,nu12,nu21,slimt1,slimc1,em22t0,yt0,slimt2,slimc2,     &
            slims,gammaf,gammar, tsdm, erods,tsize,e1d,e2d,g12d,d,         &
            w11,w22,w12,e12d,invd, limit_sig ,limit_strain, e21d,g12,      &
            eint, deint,a11,tauxy,g13,g23,scale,w13,w23,eps_eq
          !
          real(kind=WP) , dimension(nel) ::  dezz,check,xc_r, em11t,xt,em11c,xc
          real(kind=WP) , dimension(nel) ::  em22t,yt,em22c,yc,gamma,tau,ems,sc
          real(kind=WP) , dimension(nel) ::  ef11t,m1t,al1t,ef11c,m1c,al1c
          real(kind=WP) , dimension(nel) ::  ef22t,m2t,al2t,ef22c,m2c,al2c
          real(kind=WP) , dimension(nel) ::  efs,ms,als,epsf
          real(kind=WP) , dimension(nel) ::  yy, dydx,epsfailure
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
          em11t(1:nel)   = mat_param%uparam(11)
          xt(1:nel )     = mat_param%uparam(12)
          slimt1         = mat_param%uparam(13)
          em11c(1:nel)   = mat_param%uparam(14)
          xc(1:nel)      = mat_param%uparam(15)
          xc_r(1:nel)    = xc(1:nel)
          slimc1         = mat_param%uparam(16)
          ! Matrix direction
          em22t(1:nel)    = mat_param%uparam(17)
          yt(1:nel)      = mat_param%uparam(18)
          slimt2         = mat_param%uparam(19)
          em22c(1:nel)   = mat_param%uparam(20)
          yc(1:nel)      = mat_param%uparam(21)
          slimc2         = mat_param%uparam(22)
          ! shear
          gamma(1:nel)   = mat_param%uparam(23)
          tau(1:nel)     = mat_param%uparam(24)
          ems(1:nel)     = mat_param%uparam(25)
          sc(1:nel)      = mat_param%uparam(26)
          slims          = mat_param%uparam(27)
         !! stop
          ! shear parameters for directions 13 and 23
          !
          gammaf = mat_param%uparam(44)
          gammar = mat_param%uparam(45)
          tsdm   = mat_param%uparam(46)
          !
          epsfailure(1:nel) = mat_param%uparam(47)
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
          !
          fs = nint(mat_param%uparam(76))
           ! strain rate dependency of strength
          !!  call damage_parameter (ifunc)
          ! fiber - tension dir 1 -
            nfunc = mat_param%ntable 
          if(nfunc > 0) call strainrate_dependency_125c(nel, mat_param , epsp, vartmp, nvartmp ,  &
                                                        em11t,    xt,     em11c,    xc,      &
                                                        em22t,    yt,     em22c,    yc,      &
                                                        gamma,    tau,    ems,      sc,      &
                                                        al1t,     m1t,    al1c,     m1c,     &
                                                        al2t,     m2t,    al2c,     m2c,     &
                                                        als,      ms,     ef11t,   ef11c,    &
                                                        ef22t,    ef22c,  efs  ,   epsfailure    ) 
          ! failure 
          ndx = 0
          indx(:)=0
          do i=1,nel
              eps_eq =  two_third* (epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2  ) 
              eps_eq = sqrt(eps_eq)
              if(off(i) >= one .and. eps_eq >= epsfailure(i))  then
                  dmg_g(i) = dmg_g(i) + one
                  off(i) = zero
                  offl(i) = zero
              endif  
          enddo
          do i=1,nel
              if(off(i) == zero  ) then
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
          if(ndx == 0 ) return 
          ! check loading/unloading
          do idx=1,ndx
             i = indx(idx)
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
            do idx=1,ndx
              i = indx(idx)
              limit_strain = epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2
              if(check(i) >= zero .and. limit_strain > uvar(i,2) .and. dmg(i,1) /= two .and. dmg(i,1) >= zero) then
                w11 = one 
                w22 = one
                w12 = one 
                if(epsxx(i) > zero )then
                  w11 = epsxx(i)/ef11t(i)
                  w11 = exp(m1t(i)*log(w11))/al1t(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                elseif(epsxx(i) < zero) then
                  w11 = abs(epsxx(i))/ef11c(i)
                  w11 = exp(m1c(i)*log(w11))/al1c(i)  ! (esp/epsf)^m/alpha
                  w11 = exp(-w11)
                end if
                ! dir b
                if(epsyy(i) > zero )then
                  w22 = epsyy(i)/ef22t(i)
                  w22 = exp(m2t(i)*log(w22))/al2t(i)  ! (esp/epsf)^m/alpha
                  w22 = exp(-w22)
                elseif(epsyy(i) < zero) then
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
            do idx =1,ndx
              i = indx(idx)
              ! shear w12
              w12 = one
              w11 = dmg(i,2)
              w22 = dmg(i,3)
              w13 = one
              w23 = one 
              limit_strain = epsxx(i)**2 + epsyy(i)**2 + epsxy(i)**2
              if(check(i) >= zero .and. limit_strain > uvar(i,2) .and. dmg(i,1) /= two .and. dmg(i,1) >= zero) then
                if(tau(i) >  zero .and. epsxy(i) /= zero) then
                  w12 = abs(epsxy(i))/efs(i)
                  w12 = exp(ms(i)*log(w12))/als(i)  ! (esp/epsf)^m/alpha
                  w12 = exp(-w12)
                end if
               ! out of plane damage calculation
                if(abs(epszx(i)) > gammaf ) then
                   w13 = one - tsdm
                elseif(abs(epszx(i)) >= gammar ) then
                   w13 = one  -  (abs(epszx(i)) - gammar) /(gammaf - gammar )
                end if
                if(abs(epsyz(i)) > gammaf ) then
                   w23 = one - tsdm
                elseif(abs(epsyz(i)) >= gammar ) then
                   w23 = one  -  (abs(epsyz(i)) - gammar) /(gammaf - gammar )
                end if
              else
                w12 = dmg(i,4)
                w13 = dmg(i,5)
                w23 = dmg(i,6)
                if(check(i)  > zero) then
                  if( ( limit_strain < uvar(i,2) .and. dmg(i,1) > zero )  .or.       &
                    ( limit_strain > uvar(i,2) .and. dmg(i,1) < zero ) ) dmg(i,1) = -dmg(i,1)
                end if
              end if
              g12d = w12*g12
              signxy(i) = g12d*epsxy(i)
              signzx(i) = w13*shf(i)*g13*epszx(i)
              signyz(i) = w23*shf(i)*g23*epsyz(i)
              if(abs(signxy(i)) >= tau(i) .and. abs(signxy(i)) <  sc(i) .and. ems(i) > gamma(i)) then
                scale =  (sc(i) - tau(i))/(ems(i) - gamma(i))
                tauxy = tau(i) + scale*(abs(epsxy(i)) - gamma(i))
                signxy(i) = sign(tauxy,signxy(i))
              end if
              ! checking loading failure criteria
              if(abs(dmg(i,1)) <  one ) then
                dmg(i,7) = max(epsxx(i)/em11t(i), signxx(i)/xt(i)) ! based on max strain or max strain
                if(signxx(i) < zero) dmg(i,7) = max(-epsxx(i)/em11c(i), -signxx(i)/xc(i))
                dmg(i,8) = max(epsyy(i)/em22t(i), signyy(i)/yt(i))
                if(signyy(i) < zero) dmg(i,8) = max(-epsyy(i)/em22c(i),-signyy(i)/yc(i))
                dmg(i,9) = max( abs(epsxy(i))/ems(i),  abs(signxy(i))/sc(i) )
                if(dmg(i,7) >= zep99 .or. dmg(i,8) >= zep99 .or. dmg(i,9) >= zep99) dmg(i,1) = one
              end if
              if(check(i) >= zero) then
                if(dmg(i,1) >= one  ) then
                  if( dmg(i,7) >= zep99) then
                    if(signxx(i) >= zero .and. (signxx(i) <=  slimt1*xt(i) .or. dmg(i,7) == two )) then !  slimt1*xt(i) ) then
                      limit_sig = slimt1*xt(i)
                      signxx(i) = limit_sig ! max(signxx(i), limit_sig )
                      signyy(i) = slimt1*sigoyy(i)
                      signxy(i) = slimt1*sigoxy(i)
                      dmg(i,7) = two !
                    elseif(signxx(i) < zero .and. ( signxx(i) >= -slimc1*xc(i) .or. dmg(i,7) == two)) then
                      limit_sig = -slimc1*xc(i)
                      signxx(i) = limit_sig !  min(signxx(i),  limit_sig )
                      signyy(i) = slimc1*sigoyy(i)
                      signxy(i) = slimc1*sigoxy(i)
                      dmg(i,7) = two !
                    end if
                  elseif( dmg(i,8) >= zep99)then
                    if(signyy(i) >= zero .and. (signyy(i) <= slimt2*yt(i) .or. dmg(i,8) == two )) then !  slimt2*yt(i) ) then
                      limit_sig = slimt2*yt(i)
                      signyy(i) = limit_sig
                      signxx(i) = slimt2*sigoxx(i)
                      signxy(i) = slimt2*sigoxy(i)
                      dmg(i,8) = two !
                    elseif(signyy(i) < zero .and. ( signyy(i) >= -slimc2*yc(i) .or. dmg(i,8) == two ) ) then
                      limit_sig = -slimc2*yc(i)
                      signyy(i) = limit_sig ! min(signyy(i),limit_sig )
                      signxx(i) = slimc2*sigoxx(i)
                      signxy(i) = slimc2*sigoxy(i)
                      dmg(i,8) = two !
                    end if
                  elseif( dmg(i,9) >= zep99 .and. (abs(signxy(i)) <= slims*sc(i) .or. dmg(i,9) == two )) then
                    limit_sig = slims*sc(i)
                    signxy(i) = sign(limit_sig,signxy(i))
                    signxx(i) = slims*sigoxx(i)
                    signyy(i) = slims*sigoyy(i)
                    dmg(i,9) = two
                  end if ! dmg
                  dmg(i,1) = max(dmg(i,1), dmg(i,7),dmg(i,8),dmg(i,9))
                  if(epsxx(i) /= zero ) w11 = Min(one, abs(signxx(i)/epsxx(i))/e1)
                  if(epsyy(i) /= zero ) w22 = Min(one, abs(signyy(i)/epsyy(i))/e2)
                  if(epsxy(i) /= zero ) w12 = Min(one, abs(signxy(i)/epsxy(i))/g12)
                 elseif(dmg(i,1) == -two) then  ! to follow zero stress when slim = zero  in loading and unloading
                    if( dmg(i,7) >= zep99) then
                      if(signxx(i) >= zero .and. slimt1 == zero) then !  slimt1*xt(i) ) then
                        signxx(i) = zero
                        signyy(i) = zero
                        signxy(i) = zero
                      elseif(signxx(i) < zero .and. slimc1 == zero) then
                        signxx(i) = zero
                        signyy(i) = zero
                        signxy(i) = zero
                      end if
                    elseif( dmg(i,8) >= zep99)then
                      if(signyy(i) >= zero .and. slimt2 == zero ) then !  slimt2*yt(i) ) 
                        signyy(i) = zero
                        signxx(i) = zero
                        signxy(i) = zero
                      elseif(signyy(i) < zero .and. slimc2 == zero ) then
                        signyy(i) = zero
                        signxx(i) = zero
                        signxy(i) = zero
                       end if
                    elseif( dmg(i,9) >= zep99 .and. slims == zero) then
                      signxy(i) = zero
                      signxx(i) = zero
                      signyy(i) = zero
                  end if ! dmg
                end if ! dmg(i,1)
                uvar(i,2) = max(uvar(i,2), limit_strain)
              endif ! check
              dmg(i,2) = w11
              dmg(i,3) = w22
              dmg(i,4) = w12
              dmg(i,5) = w13
              dmg(i,6) = w23
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
