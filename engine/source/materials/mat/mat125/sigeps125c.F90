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
           epsxx   ,epsyy     ,epsxy   ,epsyz   ,epszx ,         &    
           sigoxx  ,sigoyy    ,sigoxy,                           &
           signxx  ,signyy    ,signxy  ,signzx   ,signyz  ,      &
           off     ,sigy      ,etse    ,ssp      ,dmg     ,      &
           dmg_g   ,offply  ) 
!-----------------------------------------------
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
          real(kind=WP), dimension(nel,6), intent(inout) ::  dmg 
          real(kind=WP), dimension(nel), intent(inout) ::  dmg_g 
          real(kind=WP), dimension(nel), intent(inout) :: offply !< ply element deletion flag
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: fs, i,updat1,updat2,nkey
      integer , dimension(nel) :: index,iad,ipos,ilen
      real(kind=WP)                                                       &
       :: e1,e2,nu12,nu21,em11t0,xt0,slimt1,em11c0,xc0,slimc1,         &
       em22t0,yt0,slimt2,em22c0,yc0,slimc2,gamma0,tau0,ems0,sc0,    &
       slims,gammaf,gammar, tsdm, erods,tsize,e1d,e2d,g12d,d,       &
       w11,w22,w12,e12d,invd, ems013,ems023,sc013,sc023,           &
       gamma02,gamma03,tau02,tau03,                                 &
       e21d,g12,limit_sig, eint, deint,a11,tauxy,g13,g23,&
       eft,efc,emt,emc,ec,et,sig(3),em,ef 
       !
       real(kind=WP)  &
       :: xt_new , ef11t_new ,m1t_new ,al1t_new,  &
       xc_new , ef11c_new ,m1c_new ,al1c_new,  &
       yt_new, ef22t_new ,m2t_new ,al2t_new,   &
       yc_new, ef22c_new ,m2c_new ,al2c_new 
      !
      real(kind=WP) , dimension(nel) ::  dezz,check,xc_r, em11t,xt,em11c,xc
      real(kind=WP) , dimension(nel) ::  em22t,yt,em22c,yc,gamma,tau,ems,sc
      real(kind=WP) , dimension(nel) ::  gamma2,tau2,ems13,sc13,gamma3,tau3
      real(kind=WP) , dimension(nel) ::  ems23,sc23
      real(kind=WP) , dimension(nel) ::  ef11t,m1t,al1t,ef11c,m1c,al1c
      real(kind=WP) , dimension(nel) ::  ef22t,m2t,al2t,ef22c,m2c,al2c
      real(kind=WP) , dimension(nel) ::  efs,ms,als
      real(kind=WP) , dimension(nel) ::  yy, dydx
      !
      logical :: abit_t,abit_c,abit_s
!!======================================================================
!
       ! FS ! type of failure yield surface loading 
                              !  =  -1    
                              !  =  0  ! not available
                              !  =  1  ! not finalized
!--------------------------  
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

      efs(1:nel) = mat_param%uparam(67)
      ms(1:nel)   = mat_param%uparam(68)
      als(1:nel)  = mat_param%uparam(69)
      !!dmg_g(1:nel) = zero
      offply(1:nel) = one

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
      endif
      !      
      if(ifunc(2) /= 0) then  ! em11t 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(2)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(2)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        xt(1:nel)= yy(1:nel)
      else
        xt(1:nel) = xt0
      endif 
     ! fiber - compression  dir 1 -
      if(ifunc(3) /= 0) then  ! em11c 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(3)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(3)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        em11c(1:nel)= yy(1:nel)
      else
        em11c(1:nel) = em11c0
      endif      
      if(ifunc(4) /= 0) then  ! xc
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(4)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        xc(1:nel)= yy(1:nel)
      else
        xc(1:nel) = xc0
      endif 
     ! matrix - tension dir 2 - 
      if(ifunc(5) /= 0) then  ! 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(5)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(5)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        em22t(1:nel)= yy(1:nel)
      else
        em22t(1:nel) = em22t0
      endif
      !      
      if(ifunc(6) /= 0) then  ! em11t 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(6)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(6)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        yt(1:nel)= yy(1:nel)
      else
        yt(1:nel) = yt0
      endif 
     ! matrix - compression  dir 2 -
      if(ifunc(7) /= 0) then  ! em11c 
        ipos(1:nel) = 0
        iad (1:nel) = npf(ifunc(7)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(7)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        em22c(1:nel)= yy(1:nel)
      else
        em22c(1:nel) = em22c0
      endif      
      if(ifunc(8) /= 0) then  ! xc
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(8)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(8)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        yc(1:nel)= yy(1:nel)
      else
        yc(1:nel) = yc0
      endif  
      
      ! shear  12 - gamma
      if(ifunc(13) /= 0) then  !  
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(13)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(13)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        gamma(1:nel)= yy(1:nel)
      else
        gamma(1:nel) = gamma0
      endif     
      ! shear  tau 
      if(ifunc(14) /= 0) then  ! tau
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(14)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(14)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        tau(1:nel)= yy(1:nel)
      else
        tau(1:nel) = tau0
      endif        
      ! shear strain 12 - ems
      if(ifunc(15) /= 0) then  ! em11c 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(15)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(15)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        ems(1:nel)= yy(1:nel)
      else
        ems(1:nel) = ems0
      endif     
      ! shear strengh sc 
      if(ifunc(16) /= 0) then  ! sc
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(16)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(16)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        sc(1:nel)= yy(1:nel)
      else
        sc(1:nel) = sc0
      endif        
       ! shear  13 - gamma2
      if(ifunc(17) /= 0) then  !  
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(17)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(17)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        gamma(1:nel)= yy(1:nel)
      else
        gamma2(1:nel) = gamma02
      endif     
      ! shear  tau 2
      if(ifunc(18) /= 0) then  ! tau
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(18)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(18)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        tau2(1:nel)= yy(1:nel)
      else
        tau2(1:nel) = tau02
      endif        
      ! shear strain 12 - ems13
      if(ifunc(20) /= 0) then  ! em11c 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(20)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(20)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        ems13(1:nel)= yy(1:nel)
      else
        ems13(1:nel) = ems013
      endif     
      ! shear strengh sc13 
      if(ifunc(19) /= 0) then  ! sc
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(19)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(19)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        sc13(1:nel)= yy(1:nel)
      else
        sc13(1:nel) = sc013
      endif        
      ! shear  23 - gamma3
      if(ifunc(21) /= 0) then  !  
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(21)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(21)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        gamma(1:nel)= yy(1:nel)
      else
        gamma3(1:nel) = gamma03
      endif     
      ! shear  tau 2
      if(ifunc(22) /= 0) then  ! tau
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(22)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(22)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        tau3(1:nel)= yy(1:nel)
      else
        tau3(1:nel) = tau03
      endif        
      ! shear strain 23 - ems23
      if(ifunc(24) /= 0) then  ! 
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(24)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(24)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        ems23(1:nel)= yy(1:nel)
      else
        ems23(1:nel) = ems023
      endif     
      ! shear strengh sc13 
      if(ifunc(23) /= 0) then  ! sc
        ipos(1:nel) = 1
        iad (1:nel) = npf(ifunc(23)) / 2 + 1
        ilen(1:NEL) = npf(ifunc(23)+1) / 2 - iad(1:nel) - ipos(1:nel)
        CALL vinter(tf,iad,ipos,ilen,nel,epsp,dydx,yy) 
        sc23(1:nel)= yy(1:nel)
      else
        sc23(1:nel) = sc023
      endif    
      ! Computing the damage parameters 
      do i=1,nel
        if(xt(i) > zero )then
          ef11t(i)  = xt(i)/e1
          if(em11t(i)  == zero) em11t(i) = onep2*ef11t(i)
          m1t(i)= -one/log(ef11t(i)/em11t(i))     
          al1t(i) = m1t(i)*(em11t(i)/ef11t(i))**m1t(i)
        endif   
        if(xc(i) > zero  )then
          ef11c(i)  = xc(i)/e1
          if(em11c(i) <= zero)em11c(i) = ONEP2*ef11c(i)
          m1c(i)= -one/log(ef11c(i)/em11c(i))     
          al1c(i) = m1c(i)*(em11c(i)/ef11c(i))**m1c(i)
        endif
        if(yt(i) > zero )then
          ef22t(i)  = yt(i)/e2
          if(em22t(i) <= zero) em22t(i) = ONEP2*ef22t(i)
          m2t(i) = -one/log(ef22t(i)/em22t(i))     
          al2t(i) = m2t(i)*(em22t(i)/ef22t(i))**m2t(i)
        endif 
        !
        if(yc(i) > zero  )then
          ef22c(i)  = yc(i)/e1
          if(em22c(i) == zero  )em22c(i) = onep2*ef22c(i) 
          m2c(i) = -one/log(ef22c(i)/em22c(i))     
          al2c(i) = m2c(i)*(em22c(i)/ef22c(i))**m2c(i)
        endif
        if(tau(i) > zero  )then
            efs(i)  = tau(i) /g12
            if(gamma(i) <= zero ) gamma(i) = onep2*efs(i) 
            ms(i) = -one/log(efs(i)/gamma(i)) ! one/ln(epsm/epsf)    
            als(i) = ms(i)*(gamma(i)/efs(i))**ms(i)
         endif  
      enddo ! nel   
      ! check loading/unloading

      do i=1,nel
        ! dir 
         check(i) = one
        ! check unloading
         w11 = dmg(i,1)
         w22 = dmg(i,2)
         w12 = dmg(i,3)
         d = (one - w11*w22*nu12*nu21)
         e1d = w11*e1
         e2d = w22*e2
         e12d = w11*w22*nu21*e1
         e21d = w11*w22*nu12*e2
         invd = one/d
         signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
         signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
         signxy(i) = w12*g12*epsxy(i)
         deint = half*(depsxx(i)*(signxx(i) + sigoxx(i))  +                                     &
                           depsyy(i)*(signyy(i) + sigoyy(i))) +                                    &
                           depsxy(i)*(signxy(i) + sigoxy(i)) 
         eint = uvar(i,1) + deint
         uvar(i,1) = eint
         if(deint < ZERO ) then
            check(i) = -one
        !! elseif(uvar(i,2) == -one) then
        !!    check(i) = -one 
           !!if(uvar(i,3) /= zero .and. eint >= uvar(i,3)) check(i) = one
         else 
           check(i) = one  
         endif
        enddo !            
       ! 
      ! membrane computing FS = 0, 1, -1 
        select  case (fs)
          case(-1)
               ! Uncoupled failure criterion
               !  FS = -1
              do i=1,nel  
                  if(check(i) >= zero ) then
                    uvar(i,2) = one
                      if(epsxx(i) >= zero )then
                         w11 = epsxx(i)/ef11t(i)
                         w11 = exp(m1t(i)*log(w11))/al1t(i)  ! (esp/epsf)^m/alpha
                         w11 = exp(-w11)
                      else
                         w11 = abs(epsxx(i))/ef11c(i)
                         w11 = exp(m1c(i)*log(w11))/al1c(i)  ! (esp/epsf)^m/alpha
                         w11 = exp(-w11)
                      endif
                ! dir b
                       if(epsyy(i) >= zero )then
                         w22 = epsyy(i)/ef22t(i)
                         w22 = exp(m2t(i)*log(w22))/al2t(i)  ! (esp/epsf)^m/alpha
                         w22 = exp(-w22)
                      else
                        w22 = abs(epsyy(i))/ef22c(i)
                        w22 = exp(m2c(i)*log(w22))/al2c(i)  ! (esp/epsf)^m/alpha
                        w22 = exp(-w22)
                      endif 
                 else ! unlaod
                    w11 = dmg(i,1)
                    w22 = dmg(i,2)
                    w12 = dmg(i,3)
                    uvar(i,2) = -one
                 endif  
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
                 dmg(i,1)= w11
                 dmg(i,2)= w22
                 !
                 etse(i)   = one
                 a11       = max(e1,e2)/(one - nu12**2) 
                 a11       = max(e1,e2)
                 ssp(i) = sqrt(a11/rho(i))
                 sigy(i)    = min(slimt1*xt(i),slimt2*yt(i), slimc1*xc(i),slimc2*yc(i)) 
               enddo ! nel loop
               ! shear traitement and softening 
               do i=1,nel
                ! shear w12
                 w12 = one
                 w11 = dmg(i,1)
                 w22 = dmg(i,2)
                 if(check(i) >= zero)then
                   w12 = abs(epsxy(i))/efs(i)
                   w12 = exp(ms(i)*log(w12))/als(i)  ! (esp/epsf)^m/alpha
                   w12 = exp(-w12)
                   uvar(i,2)= one
                 else
                   w12 = dmg(i,3)
                   uvar(i,2) = -one 
                 endif    
                  g12d = w12*g12
                  signxy(i) = g12d*epsxy(i)
                  if(abs(signxy(i)) >= tau(i) .and. abs(signxy(i)) <  sc(i)) then
                    tauxy = abs(epsxy(i)/gamma(i))
                    tauxy = tau(i) + tauxy*(sc(i) - tau(i))/(ems(i) - gamma(i))
                    signxy(i) = sign(tauxy,signxy(i))
                  endif
                  ! checking loading failure criteria  
                  if(dmg_g(i) < one ) then
                     dmg(i,4) = signxx(i)/xt(i)
                     if(signxx(i) < zero) dmg(i,4) = -signxx(i)/xc(i)
                     dmg(i,5) = signyy(i)/yt(i)
                     if(signyy(i) < zero) dmg(i,5) = -signyy(i)/yc(i)
                     dmg(i,6) = abs(signxy(i))/sc(i)
                     if(dmg(i,4) >= zep99 .or. dmg(i,5) >= zep99 .or. dmg(i,6) >= zep99) dmg_g(i) = one
                  endif  
                   If(check(i) >= zero) then 
                    if(dmg_g(i) == one  ) then
                      if( uvar(i,4) == zero .and. uvar(i,5) == zero .and. uvar(i,6) == zero) then
                         uvar(i,4) = signxx(i)*slimt1
                         if(signxx(i) < zero) uvar(i,4) = -signxx(i)*slimc1
                         uvar(i,5) = signyy(i)*slimt2
                         if(signyy(i) < zero) uvar(i,5) = -signyy(i)*slimc2
                         uvar(i,6) = abs(signxy(i))*slims
                      endif    
                      signxx(i) = sign(max(uvar(i,4), abs(signxx(i))),signxx(i))
                      signyy(i) = sign(max(uvar(i,5), abs(signyy(i))),signyy(i))
                      signxy(i) = sign(max(uvar(i,6), abs(signxy(i))),signxy(i))
                      if(epsxx(i) /= zero ) w11 = Min(one, abs(signxx(i)/epsxx(i))/e1)
                      if(epsyy(i) /= zero ) w22 = Min(one, abs(signyy(i)/epsyy(i))/e2)
                      if(epsxy(i) /= zero ) w12 = Min(one, abs(signxy(i)/epsxy(i))/g12)
                    endif
                 endif   
                  dmg(i,1) = w11
                  dmg(i,2) = w22
                  dmg(i,3) = w12
                  dezz(i)    = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
                  thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i)
               enddo ! nel 
             !  coupling between matrix and shear 
             !  FS = 0
             !  
             case(0)  ! fs = 0  !  should be added after fixing fs=1 formulation
              ! coupled failure criterion
              ! Coupling failure criterion
              ! matrix/shear  
             case(1)
              ! coupled failure criterion
              ! Coupling failure criterion
              ! fiber/shear
              ! matrix/shear
               !  FS = 1
                do i=1,nel  
                  if(check(i) >= zero ) then
                    uvar(i,2) = one
                    ! compute the damage parameters 
                      if(epsxx(i) >= zero )then
                        w11 = epsxx(i)/ef11t(i)
                        w11 = exp(m1t(i)*log(w11))/al1t(i) ! (esp/epsf)^m/alpha
                        w11 = exp(-w11)
                      else
                         w11 = abs(epsxx(i))/ef11c(i)
                         w11 = exp(m1c(i)*log(w11))/al1c(i)
                         w11 = exp(-w11)  ! 
                      endif
                ! dir b
                       if(epsyy(i) >= zero )then
                         w22 = epsyy(i)/ef22t(i)
                         w22 = exp(m2t(i)*log(w22))/al2t(i)
                         w22 = exp(-w22)  ! 
                      else
                         w22 = abs(epsyy(i))/ef22c(i)
                         w22 = exp(m2c(i)*log(w22))/al2c(i)
                         w22 = exp(-w22)  !  
                      endif    
                      w12 = abs(epsxy(i))/efs(i)
                      w12 = exp(ms(i)*log(w12))/als(i)  ! (esp/epsf)^m/alpha
                      w12 = exp(-w12)
                 else ! unlaod
                    w11 = dmg(i,1)
                    w22 = dmg(i,2)
                    w12 = dmg(i,3)
                    uvar(i,2) = -one
                 endif  
             ! damage hook matrix
                 d = (one - w11*w22*nu12*nu21)
                 e1d = w11*e1
                 e2d = w22*e2
                 e12d = w11*w22*nu21*e1
                 e21d = w11*w22*nu12*e2
                 invd = one/d
                 signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
                 signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
                 signxy(i) = g12*w12*epsxy(i)
                 signzx(i) = shf(i)*g12*epszx(i) 
                 signyz(i) = shf(i)*g12*epsyz(i) 
                 !
                 if(dmg_g(i) < one ) then 
                    dmg(i,4)= (signxx(i)/xt(i))**2 + (signxy(i)/sc(i))**2 
                    if(signxx(i) < zero )dmg(i,4) = (signxx(i)/xc(i))**2 + (signxy(i)/sc(i))**2 
                   dmg(i,5) = (signyy(i)/yt(i))**2 + (signxy(i)/sc(i))**2 
                   if(signyy(i) < zero )dmg(i,5) = (signyy(i)/yc(i))**2 + (signxy(i)/sc(i))**2
                   if(dmg(i,4) >= zep99 .or. dmg(i,5) >=  zep99) dmg_g(i) = one
                 endif  
                 if( check(i) >= zero  ) then
                   if(dmg_g(i) == one  ) then
                   ! if( uvar(i,4) == zero .and. uvar(i,5) == zero .and. uvar(i,6) == zero) then
                     if( uvar(i,4) == zero ) then
                         uvar(i,4) = signxx(i)*slimt1
                         if(signxx(i) < zero) uvar(i,4) = -signxx(i)*slimc1
                         uvar(i,5) = signyy(i)*slimt2
                         if(signyy(i) < zero) uvar(i,5) = -signyy(i)*slimc2
                         uvar(i,6) = abs(signxy(i))*slims
                     endif    
                      signxx(i) = sign(max(uvar(i,4), abs(signxx(i))),signxx(i))
                      signyy(i) = sign(max(uvar(i,5), abs(signyy(i))),signyy(i))
                      signxy(i) = sign(max(uvar(i,6), abs(signxy(i))),signxy(i))
                      if(epsxx(i) /= zero ) w11 = Min(one, abs(signxx(i)/epsxx(i))/e1)
                      if(epsyy(i) /= zero ) w22 = Min(one, abs(signyy(i)/epsyy(i))/e2)
                      if(epsxy(i) /= zero ) w12 = Min(one, abs(signxy(i)/epsxy(i))/g12)
                    endif
              ! save w11 & w22, w12
                  dmg(i,1)= w11
                  dmg(i,2)= w22
                  dmg(i,3)= w12
                endif
                etse(i)   = one
                a11       = max(e1,e2)/(one - nu12**2) 
                a11       = max(e1,e2)
                ssp(i) = sqrt(a11/rho(i))
                sigy(i)    = min(slimt1*xt(i),slimt2*yt(i), slimc1*xc(i),slimc2*yc(i)) 
                dezz(i)  = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
                thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i)
               enddo ! nel loop
            end select ! FS
!-------------------------------------------------------------------------------------------
         end subroutine sigeps125c
      end module sigeps125c_mod
