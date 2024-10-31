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
      !|====================================================================
      !|  sigeps169_connect            source/materials/mat/mat169/sigeps169_connect.f
      !|-- called by -----------
      !|        suser43                       source/elements/solid/sconnect/suser43.F
      !|-- calls ---------------
      !|        finter                        source/tools/curve/finter.f   
      !|====================================================================

      !||====================================================================
      !||    sigeps169_connect_mod   ../engine/source/materials/mat/mat169/sigeps169_connect.F90
      !||--- called by ------------------------------------------------------
      !||    suser43                 ../engine/source/elements/solid/sconnect/suser43.F
      !||====================================================================
      module sigeps169_connect_mod
      contains
! ===============================================================================================================
!  \brief material for cohesive element, elastic in normal direction, elastoplastic in shear, with coupled damage
! ===============================================================================================================  
      !||====================================================================
      !||    sigeps169_connect   ../engine/source/materials/mat/mat169/sigeps169_connect.F90
      !||--- called by ------------------------------------------------------
      !||    suser43             ../engine/source/elements/solid/sconnect/suser43.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod        ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine sigeps169_connect(                                          &
          nel     ,time    ,iparam  ,uparam  ,                                 &
          niparam ,nuparam ,stifm   ,                                          &
          area    ,off     ,nuvar   ,uvar    ,ipg    ,                         &
          depszz  ,depsyz  ,depszx  ,epszz   ,epsyz  ,epszx   ,                &                             
          sigozz  ,sigoyz  ,sigozx  ,signzz  ,signyz ,signzx  ,                &
          pla     ,iout    ,jsms    ,dmg     ,ngl    ,dmels   ,                &
          idtmins,dtfacs   ,dtmins  ,thick0  )                                        

!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use constant_mod , only : one , zero,two,em6,em20,four,five,half
!-----------------------------------------------
      implicit none
! ----------------------------------------------
#include "my_real.inc"
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------       
      integer ,intent(in) :: nel,jsms,ipg,iout
      integer ,intent(in) :: niparam
      integer ,intent(in) :: nuparam
      integer ,intent(in) :: nuvar
      integer ,intent(in) :: idtmins

      integer ,dimension(nel)     ,intent(in) :: ngl
      integer ,dimension(niparam) ,intent(in) :: iparam

      my_real ,intent(in) :: dtfacs
      my_real ,intent(in) :: dtmins
      my_real ,intent(in) :: time
      my_real ,dimension(nel)  ,intent(inout) :: off,area,pla,dmels
      my_real ,dimension(nel)  ,intent(in)    :: depszz,depsyz,depszx,epszz,epsyz,epszx,thick0
      my_real ,dimension(nel)  ,intent(in)    :: sigozz  ,sigoyz  ,sigozx  
      my_real ,dimension(nel)  ,intent(out)   :: signzz,signyz,signzx
      my_real ,dimension(nel)  ,intent(inout) :: dmg,stifm
      my_real ,dimension(nuparam)   ,intent(in)    :: uparam
      my_real ,dimension(nel,nuvar) ,intent(inout) :: uvar

!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
        integer :: i,ii,k,nindf,tempi,nindxd
        integer :: iel
        integer :: pwrt,pwrs
        integer ,dimension(nel) :: indxd,indf

        my_real :: young,nu, wave,gcten,gcshr,shrp,sht_sl,taumax,tau_n
        my_real :: dlam, dpla_dlam,shear,dp,g1,g2,sigeq,dtb,norm,nxz,nyz,d0fn,d0fs
        my_real, dimension(nel) :: dstrnz, dstrnxz, dstrnyz,strs_tr_sh,fyld,strn_pl,dstr_sh,eps_n0
        my_real, dimension(nel) :: normzz, normxz, normyz, df,dpzx,dpyz,dpzz,ddpla,dpla,dmg_n,dmg_s
        my_real, dimension(nel) :: fdam_n,fdam_s,thick,stf,tempr,normyz_norm,normxz_norm,normzz_norm
        my_real, dimension(nel) :: eps_s0,eps_sh,dfn,dfs
        my_real, dimension(nel) :: tenmax,shrmax

!--------------------------------------------------------
!    uvar(1) = dmg_n 
!    uvar(2) = dmg_s 
!    uvar(3) = eps_n0
!    uvar(4) = eps_s0
!    uvar(5) = sig_eq 
!    uvar(6) = f_yld 
!    uvar(7) = sign / tenmax
!    uvar(8) = sigs / taumax
!--------------------------------------------------------

      pwrt   = iparam(1)
      pwrs   = iparam(2)

      young  = uparam(1) 
      shear  = uparam(2)
      nu     = uparam(3) 
      gcten  = uparam(5) 
      gcshr  = uparam(7) 
      shrp   = uparam(8) 
      sht_sl = uparam(9) 
      d0fn   = uparam(10)
      d0fs   = uparam(11)
      dp     = uparam(12)
!
      wave = young * (one-nu)/(one+nu)/(one-two*nu)
      if (time == zero) then
        uvar(:,10) = uparam(6)   ! shrmax
        uvar(:,11) = uparam(4)   ! tenmax
        uvar(:,3)  = uparam(4) * thick0(:) / wave
        uvar(:,4)  = uparam(6) * thick0(:) / shear
        uvar(:,14) = d0fn
        uvar(:,15) = d0fs
      end if
      shrmax(:) = uvar(:,10)
      tenmax(:) = uvar(:,11)
      eps_n0(:) = uvar(:,3)
      eps_s0(:) = uvar(:,4)
      dfn(:) = uvar(:,14)
      dfs(:) = uvar(:,15)
!

      pla(:)   = zero
      dpla(1:) = zero

      stf(1:nel)     = young *  area(1:nel)                                       
      stifm(1:nel)   = stifm(1:nel)  + stf(1:nel)*off(1:nel)                                              
      ! omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
      if (idtmins==2 .and. jsms/=0) then
        dtb = (dtmins/dtfacs)**2
        do iel=1,nel                                                 
          dmels(iel)=max(dmels(iel),half*dtb*stf(iel)*off(iel))
        enddo                                                        
      end if                                                        

      do i=1,nel  
        dmg_n(i) = uvar(i,1)
        dmg_s(i) = uvar(i,2)        
        fdam_n(i) = one - dmg_n(i)
        fdam_s(i) = one - dmg_s(i)
      ! write(*,*) ' dmg_n       = ',i,  dmg_n(i) 
      enddo                                                        
     
      do i = 1,nel
        if (off(i) < 0.001)  off(i) = zero
        if (off(i) < one)    off(i) = off(i)*four/five
        if (off(i) == one) then
          signzz(i)     = sigozz(i) / fdam_n(i) + depszz(i) * wave /thick0(i) !young is per unit length
          signyz(i)     = sigoyz(i) / fdam_s(i) + depsyz(i) * shear/thick0(i)
          signzx(i)     = sigozx(i) / fdam_s(i) + depszx(i) * shear/thick0(i)
          strs_tr_sh(i) = sqrt(signyz(i)**2 + signzx(i)**2)
          dstr_sh(i)    = sqrt(depsyz(i)**2 + depszx(i)**2) 
          eps_sh(i)     = sqrt(epsyz(i)**2  + epszx(i)**2) 
        endif
      enddo    
      !-----------------------------------------------
      !   compute yield function 
      !-----------------------------------------------
      do i=1,nel
        fyld(i) = (max(signzz(i),zero)/ tenmax(i))**pwrt +                         &
                  (strs_tr_sh(i) /(shrmax(i) - sht_sl * signzz(i) ))**pwrs 
        fyld(i) =  fyld(i) - one        
        uvar(i,6) = fyld(i)
      enddo
      !
      !  test of damage initiation
      !
      nindxd  = 0
      nindf   = 0
      do i=1,nel 
        if (fyld(i) >= zero .and. off(i) == one)  then
        
          taumax = shrmax(i) - sht_sl * signzz(i)
          if ((signzz(i) >= tenmax(i) .or.strs_tr_sh(i) >= taumax) .and. uvar(i,9) == zero) then
            eps_n0(i) = epszz(i)
            eps_s0(i) = eps_sh(i) + dp
            tenmax(i) = max(signzz(i),em6)
            shrmax(i) = max(strs_tr_sh(i),em6)
            nindxd = nindxd+1
            indxd(nindxd) = i
            uvar(i,9) = one
          end if         
        end if         
      enddo
!
      do i=1,nel
        if (eps_sh(i) > eps_s0(i)) then
          dmg_s(i) = (eps_sh(i) - eps_s0(i)) / (dfs(i) - eps_s0(i))  
          dmg_s(i) = max(uvar(i,2),dmg_s(i))
        end if         
        if (epszz(i) > eps_n0(i)) then
          dmg_n(i) = (epszz(i) - eps_n0(i)) / (dfn(i) - eps_n0(i))                 
          dmg_n(i) = max(uvar(i,1),dmg_n(i))          
        end if         
      enddo
!
      do i=1,nel
        if (dmg_n(i) >= one .and. off(i) == one) then 
          dmg_n(i) = one
          off(i) = four/five
          nindf  = nindf + 1
          indf(nindf) = i        
        else if (dmg_s(i) >= one .and. off(i) == one) then 
          dmg_s(i) = one
          off(i) = four/five
          nindf   = nindf + 1  
          indf(nindf) = i    
        endif 
        
        signzz(i) = min(signzz(i), tenmax(i))
        taumax    = shrmax(i) - sht_sl * signzz(i)
        strs_tr_sh(i) =  sqrt( signyz(i)**2 + signzx(i)**2)
        if (strs_tr_sh(i) > taumax) then
          tau_n     = min(strs_tr_sh(i), taumax)
          signyz(i) = signyz(i) * tau_n / strs_tr_sh(i)
          signzx(i) = signzx(i) * tau_n / strs_tr_sh(i) 
        endif 
      enddo
!
      uvar(:,10) = shrmax(:)
      uvar(:,11) = tenmax(:)
      uvar(:,3)  = eps_n0(:)
      uvar(:,4)  = eps_s0(:)
      uvar(:,14) = dfn(:)
      uvar(:,15) = dfs(:)

      do i=1,nel
        uvar(i,1) = dmg_n(i)  
        uvar(i,2) = dmg_s(i)  
        dmg(i)    = max(dmg_n(i),dmg_s(i))
      enddo

      do i=1,nel
        strs_tr_sh(i) =  sqrt( signyz(i)**2 + signzx(i)**2)
        taumax = shrmax(i) - sht_sl * signzz(i)
        g1        = max(signzz(i),zero) / tenmax(i)
        g2        = strs_tr_sh(i) / taumax
        g1        = min(g1, one)
        g2        = min(g2, one)
        uvar(i,7) = g1
        uvar(i,8) = g2
        sigeq     = g1**pwrt + g2**pwrs
        uvar(i,5) = sigeq
        fdam_n(i)  = one - dmg_n(i)
        signzz(i)  =  signzz(i) * fdam_n(i)
        signyz(i)  =  signyz(i) * (one - dmg_s(i))
        signzx(i)  =  signzx(i) * (one - dmg_s(i))       
      enddo
   
!-------------------------
      if (nindxd > 0) then
!$OMP CRITICAL
        do ii=1,nindxd
          i = indxd(ii)
          write(iout, 1000) ngl(i),ipg,time
        enddo
!$OMP END CRITICAL
      end if
      if (nindf > 0) then
!$OMP CRITICAL
        do ii=1,nindf
          i = indf(ii)
          write(iout, 1003) ngl(i),time
          write(iout, 1004) epszz(i), eps_sh(i)
        enddo
!$OMP END CRITICAL
      end if        
!-----------------------------------------------------------------
    1000 format(1x,'START DAMAGE IN CONNECTION ELEMENT NUMBER ',i10,1x,'INTEGRATION POINT',i2,1x, 'AT TIME :',g11.4)     
    1003 format(1x,'FAILURE IN CONNECTION ELEMENT NUMBER ',i10,1x,' AT TIME :',g11.4) 
    1004 format(1x,'ELONGATION IN NORMAL DIRECTION AT FAILURE ',g11.4,1x,'ELONGATION IN TANGENTIAL DIRECTION AT FAILURE',g11.4) 
!-----------------------------------------------------------------
      end subroutine sigeps169_connect
      end module sigeps169_connect_mod             
