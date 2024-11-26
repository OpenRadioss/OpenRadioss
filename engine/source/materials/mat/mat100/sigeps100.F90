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
module sigeps100_mod
contains
!! \brief Compute the stress and plasticity for a material model 100 for Brick elements
   subroutine sigeps100(                                                   &
   &                    nel    , nuparam, nuvar   , nfunc  , ifunc ,       &
   &                    npf    ,tf      , time    , timestep, uparam,      &
   &                    rho     ,                                          &
   &                    depsxx , depsyy , depszz  , depsxy, depsyz, depszx,&
   &                    sigoxx , sigoyy , sigozz  , sigoxy, sigoyz, sigozx,&
   &                    signxx , signyy , signzz  , signxy, signyz, signzx,&
   &                    mfxx   ,mfxy    ,mfxz,mfyx, mfyy  , mfyz  ,        &
   &                    mfzx   ,mfzy    ,mfzz     , tempel,                &
   &                    soundsp, viscmax, uvar    , et   ,                 &
   &                    ihet   ,epsth   , iexpan,nparf  ,                  &
   &                    uparamf,uvarf   ,nvarf    ,jcvt   ,gama_r,         &
   &                    snpc, stf, impl_s)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
   use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
implicit none
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer ,intent(in) :: impl_s
      integer ,intent(in) :: nparf
      integer ,intent(in) :: nel,jcvt,nvarf,nuparam,nuvar,ihet,iexpan
      integer ,intent(in) :: snpc
      integer ,intent(in) :: stf
      my_real ,intent(in) :: time
      my_real ,intent(in) :: timestep
      my_real ,intent(in) :: uparam(nuparam)
      my_real ,intent(in) :: uparamf(nparf)
      my_real ,intent(in) :: rho(nel)
      my_real ,intent(in) :: depsxx(nel)
      my_real ,intent(in) :: depsyy(nel)
      my_real ,intent(in) :: depszz(nel)
      my_real ,intent(in) :: depsxy(nel)
      my_real ,intent(in) :: depsyz(nel)
      my_real ,intent(in) :: depszx(nel)
      my_real ,intent(in) :: sigoxx(nel)
      my_real ,intent(in) :: sigoyy(nel)
      my_real ,intent(in) :: sigozz(nel)
      my_real ,intent(in) :: sigoxy(nel)
      my_real ,intent(in) :: sigoyz(nel)
      my_real ,intent(in) :: sigozx(nel)
      my_real ,intent(in) :: mfxx(nel)
      my_real ,intent(in) :: mfxy(nel)
      my_real ,intent(in) :: mfxz(nel)
      my_real ,intent(in) :: mfyx(nel)
      my_real ,intent(in) :: mfyy(nel)
      my_real ,intent(in) :: mfyz(nel)
      my_real ,intent(in) :: mfzx(nel)
      my_real ,intent(in) :: mfzy(nel)
      my_real ,intent(in) :: mfzz(nel)
      my_real ,intent(in) :: epsth(nel)
      my_real ,intent(in) :: tempel(nel)
      my_real ,intent(in) :: gama_r(nel,6)
! ----------------------------------------------------------------------------------------------------------------------
!                                            output arguments
! ----------------------------------------------------------------------------------------------------------------------
      my_real ,intent(out) :: signxx(nel)
      my_real ,intent(out) :: signyy(nel)
      my_real ,intent(out) :: signzz(nel)
      my_real ,intent(out) :: signxy(nel)
      my_real ,intent(out) :: signyz(nel)
      my_real ,intent(out) :: signzx(nel)
      my_real ,intent(out) :: soundsp(nel)
      my_real ,intent(out) :: viscmax(nel)
      my_real ,intent(out) :: et(nel)
! ----------------------------------------------------------------------------------------------------------------------
!                                        input/output arguments
! ----------------------------------------------------------------------------------------------------------------------
      my_real ,intent(inout) :: uvar(nel,nuvar)
      my_real ,intent(inout) :: uvarf(nel,nvarf)
! ----------------------------------------------------------------------------------------------------------------------
!                                        interpolation variables
! ----------------------------------------------------------------------------------------------------------------------
      integer , intent(in) :: npf(snpc)
      integer , intent(in) :: nfunc
      integer , intent(in) :: ifunc(nfunc)
      my_real , intent(in) :: tf(stf)
      my_real fint2v
      my_real finter,fintte
      external finter,fintte
! ----------------------------------------------------------------------------------------------------------------------
!                                        local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer    i,j,kk,ll,n,flagbb,direct,iter,niter,tab,tabn,shift,nindx
      integer  n_network, flag_he, flag_mul, flag_t,nhyper,nplas,iform,&
      &flag_pl,net,exppl, nvisc(10),&
      &flag_visc(10),&
      &ipos1(nel),ilen1(nel),iad1(nel),&
      &ipos2(nel),ilen2(nel),iad2(nel)

      my_real et1,et2,et3,g,rbulk,aa,bb,cc,sb, factor,&
      &maxl,stiff0,dsig,deps,coef1,coef2,coef3,coef4,coef5,coef6,&
      &c10,c01,c20,c11,c02,c30,c21,c12,c03,d1,d2,d3,tauy0,ff, epshat,&
      &temp1,facpl,hh,r3r3,&
!
      &bi1(nel),bi2(nel),jdet(nel),i1(nel),ip1(nel),gammaold(nel),stiff(nel),&
      &ta1(nel), ta2(nel),ta3(nel),t1(nel), t2(nel),t3(nel),lpchain(nel),&
      &tb1(nel), tb2(nel),tb3(nel),trace(nel),traceb(nel),eta(nel),etb(nel),&
      &sb1(nel), sb2(nel),sb3(nel),sb4(nel), sb5(nel),sb6(nel),ww(nel),&
      &sa1(nel), sa2(nel),sa3(nel),sa4(nel), sa5(nel),sa6(nel),tanorm(nel),&
      &tbnorm(nel),dgamma(nel),pla(nel),dydx(nel),yld(nel), tauy(nel),&
      &tracea(nel),plap(nel),dpla(nel),munh(nel),dnh(nel),dydx1(nel),dydx2(nel),&
      &r1x(nel),r1y(nel),r1z(nel),r2x(nel),r2y(nel),r2z(nel),r3x(nel),r3y(nel),r3z(nel),&
!
      &nb(nel,3),sninv(nel,3,3),&
      &f(nel,3,3),ft(nel,3,3),fe(nel,3,3),fet(nel,3,3),fp(nel,3,3),&
      &fft(nel,3,3),invfpo(nel,3,3),matb(nel,3,3),fpo(nel,3,3),&
      &sig(nel,3,3),sigb(nel,3,3),siga(nel,3,3),sn(nel,3,3),&
      &fth(nel,3,3), ftot(nel,3,3),fmec(nel,3,3),invfth(nel,3,3),&
      &fftn(nel,3,3),fpeq(nel,3,3),fpeqo(nel,3,3),s(nel,3,3),fedp(nel,3,3),&
      &dfp(nel,3,3),lb(nel,3,3),dfp2(nel,3,3),fpdot(nel,3,3),invfe(nel,3,3)
      !  
      my_real a1(10),expc(10),expm(10),ksi(10),a10(10),stiffn(10),&
      &b0(10),expn(10),tauref(10)
      !
      my_real c1,c2,c3,c4,c5,mu,lm,d,beta,scale1,scale2,cmax
      my_real coefr,betaf ,coefm
!----------------------------------------------------------------
!     material model : prf : parallel rheological framework
!=======================================================================
      iform = 1
      coefr = one
      betaf = zero
      coefm = one
      n_network  =  uparam(1)
      flag_he    =  uparam(2)
      if ( flag_he == 3 .or.flag_he == 4 .or.flag_he == 5 )flag_he = 1

      flag_mul   =  uparam(3)  !calculated in updmat = 1 if irup==33

      if (flag_mul > zero)then
         coefr = uparamf(1)
         betaf = uparamf(2)
         coefm = uparamf(3)
      endif

      flag_pl    =  uparam(5)
      nplas      =  0
      tab  = 8
      tabn = 0

      !hyperelastic parameters
      if (flag_he == 1) then
         c10    = uparam(tab + 1)
         c01    = uparam(tab + 2)
         c20    = uparam(tab + 3)
         c11    = uparam(tab + 4)
         c02    = uparam(tab + 5)
         c30    = uparam(tab + 6)
         c21    = uparam(tab + 7)
         c12    = uparam(tab + 8)
         c03    = uparam(tab + 9)
         d1     = uparam(tab + 10)
         d2     = uparam(tab + 11)
         d3     = uparam(tab + 12)
         tab = tab + 12
      elseif (flag_he == 2) then
         c1   =    uparam(tab + 1)
         c2   =    uparam(tab + 2)
         c3   =    uparam(tab + 3)
         c4   =    uparam(tab + 4)
         c5   =    uparam(tab + 5)
         mu   =    uparam(tab + 6)
         d    =    uparam(tab + 7) !=1/d
         beta =    uparam(tab + 8)
         tab = tab + 10
      elseif (flag_he == 13) then

         scale1   =    uparam(tab + 1)
         scale2   =    uparam(tab + 2)

         g        =    uparam(tab + 4)!cmax computed in law100_upd
         rbulk    =    uparam(tab + 5)
         tab = tab + 5
         do i=1,nel
            ipos1(i) = nint(uvar(i, tabn +1))
            iad1(i)  = npf(ifunc(1)) / 2 + 1
            ilen1(i) = npf(ifunc(1)+1) / 2 - iad1(i) - ipos1(i)

            ipos2(i) = nint(uvar(i, tabn +2))
            iad2(i)  = npf(ifunc(2)) / 2 + 1
            ilen2(i) = npf(ifunc(2)+1) / 2 - iad2(i) - ipos2(i)
         enddo
         call vinter(tf,iad1,ipos1,ilen1,nel,tempel,dydx1,munh)
         call vinter(tf,iad2,ipos2,ilen2,nel,tempel,dydx2,dnh)

         do i=1,nel
            uvar(i, tabn +1) = ipos1(i)
            uvar(i, tabn +2) = ipos2(i)
            munh(i) = munh(i) * scale1
            dnh(i)  = dnh(i) * scale2
         enddo
         tabn = tabn + 2
      endif

      if (flag_pl == 1) then
         nplas = 5
         ff  = uparam(tab + 1)
         epshat  = uparam(tab + 2)
         tauy0   = uparam(tab + 3)
         exppl   = nint(uparam(tab + 4) )
         facpl   = uparam(tab + 5)
      endif


      !viscous parameters
      tab = tab   + nplas
      do n = 1, n_network
         stiffn(n)    = uparam(tab + 1)
         flag_visc(n) = nint(uparam(tab + 2))
         nvisc(n)     = uparam(tab + 3)

         if (flag_visc(n) == 1)then
            a10(n)    = uparam(tab + 4)
            a1(n)     = a10(n)*timestep
            expc(n)   = uparam(tab + 5)
            expm(n)   = uparam(tab + 6)
            ksi(n)    = uparam(tab + 7)
            tauref(n) = uparam(tab + 8)
            tab = tab + 3 + nvisc(n)
         elseif (flag_visc(n) == 2)then
            a10(n)    = uparam(tab + 4)
            a1(n)     = a10(n)*timestep
            b0(n)     = uparam(tab + 5)
            expn(n)   = uparam(tab + 6)
            tab = tab + 3 + nvisc(n)
         elseif (flag_visc(n) == 3)then
            a10(n)    = uparam(tab + 4)
            a1(n)     = a10(n)*timestep
            expn(n)   = uparam(tab + 5)
            expm(n)   = uparam(tab + 6)
            tab = tab + 3 + nvisc(n)
         endif
      enddo
      if (flag_he /= 13) then
         g    =  uparam(tab + 1 ) !idem starter stockage at the end
         rbulk=  uparam(tab + 2 )
      endif
      stiff0 = four_over_3*g + rbulk

      if(time == zero)then
         if (flag_pl == 1) then
            do  i = 1, nel
               uvar(i,tabn+1) = tauy0
               uvar(i,tabn+5) = one
               uvar(i,tabn+6) = one
               uvar(i,tabn+7) = one
            enddo
            tabn = tabn+ 4 + 9
         endif
         do n = 1, n_network

            !------------------------
            if (flag_visc(n) == 1 ) then !bb
               do  i = 1, nel
                  uvar(i,tabn+1) = one
                  uvar(i,tabn+2) = one
                  uvar(i,tabn+3) = one
               enddo
               shift = 9 +1  +1
               ! 9 = nombre de termes dans matrices fp
               !+1 = dgamma
               !+1 = tbnorm
            elseif (flag_visc(n) == 2 ) then !sinh
               do  i = 1, nel
                  uvar(i,tabn+1) = one
                  uvar(i,tabn+2) = one
                  uvar(i,tabn+3) = one
               enddo
               shift = 9 +1  +1
               ! 9 = nombre de termes dans matrices fp
               !+1 = dgamma
               !+1 = tbnorm
            elseif (flag_visc(n) == 3 ) then !power
               do  i = 1, nel
                  uvar(i,tabn+1) = one
                  uvar(i,tabn+2) = one
                  uvar(i,tabn+3) = one
                  uvar(i,tabn+10)= em20 !equivalent strain old
               enddo
               shift = 10 +1 +1
               ! 9 = nombre de termes dans matrices fp
               !+1 = gamma
               !+1 = dgamma
               !+1 = tbnorm
            endif
            tabn = tabn + shift
         enddo
      endif


      tabn = 0
      if (flag_he == 13)tabn = tabn + 2

      if (jcvt >0 ) then ! corotational => need to rotate fp into the local frame
         r1x (1:nel) =  gama_r(1:nel,1)
         r1y (1:nel) =  gama_r(1:nel,2)
         r1z (1:nel) =  gama_r(1:nel,3)
         r2x (1:nel) =  gama_r(1:nel,4)
         r2y (1:nel) =  gama_r(1:nel,5)
         r2z (1:nel) =  gama_r(1:nel,6)

         r3x (1:nel) = r1y (1:nel)*  r2z (1:nel) - r1z (1:nel) * r2y (1:nel)
         r3y (1:nel) = r1z (1:nel)*  r2x (1:nel) - r1x (1:nel) * r2z (1:nel)
         r3z (1:nel) = r1x (1:nel)*  r2y (1:nel) - r1y (1:nel) * r2x (1:nel)
         do i=1,nel
            r3r3 = sqrt(r3x(i)*r3x(i) + r3y(i)*r3y(i) + r3z(i)*r3z(i))
            if (r3r3 /= zero) then
               r3x (i) = r3x(i)/r3r3
               r3y (i) = r3y(i)/r3r3
               r3z (i) = r3z(i)/r3r3
            endif
         enddo
      endif




      !remove thermal strain from total strain
      !fth = i + alpha dt
      if(iexpan > 0)then
         do i=1,nel
            fth(i,1,1)  = one + epsth(i)
            fth(i,2,2)  = one + epsth(i)
            fth(i,3,3)  = one + epsth(i)
            fth(i,1,2)  = zero
            fth(i,2,3)  = zero
            fth(i,3,1)  = zero
            fth(i,2,1)  = zero
            fth(i,3,2)  = zero
            fth(i,1,3)  = zero
            ftot(i,1,1)  = one+mfxx(i)
            ftot(i,2,2)  = one+mfyy(i)
            ftot(i,3,3)  = one+mfzz(i)
            ftot(i,1,2)  = mfxy(i)
            ftot(i,2,3)  = mfyz(i)
            ftot(i,3,1)  = mfzx(i)
            ftot(i,2,1)  = mfyx(i)
            ftot(i,3,2)  = mfzy(i)
            ftot(i,1,3)  = mfxz(i)
         enddo
         call kmatinv3 (fth , invfth, nel) ! inverse (fth)
         call prodmat  (ftot , invfth, fmec, nel) ! fmec = ftot * invfth

         do i=1,nel
            f(i,1,1)  = fmec(i,1,1)
            f(i,2,2)  = fmec(i,2,2)
            f(i,3,3)  = fmec(i,3,3)
            f(i,1,2)  = fmec(i,1,2)
            f(i,2,3)  = fmec(i,2,3)
            f(i,3,1)  = fmec(i,3,1)
            f(i,2,1)  = fmec(i,2,1)
            f(i,3,2)  = fmec(i,3,2)
            f(i,1,3)  = fmec(i,1,3)
         enddo
      else
         do i=1,nel
            f(i,1,1)  = one+mfxx(i) !fmec
            f(i,2,2)  = one+mfyy(i)
            f(i,3,3)  = one+mfzz(i)
            f(i,1,2)  = mfxy(i)
            f(i,2,3)  = mfyz(i)
            f(i,3,1)  = mfzx(i)
            f(i,2,1)  = mfyx(i)
            f(i,3,2)  = mfzy(i)
            f(i,1,3)  = mfxz(i)
         enddo

      endif
!
!

      if (flag_pl == 1) then
         do i=1,nel
            tauy(i)       = uvar(i,tabn+1)
            fpeqo(i,1,1)  = uvar(i,tabn+5)
            fpeqo(i,2,2)  = uvar(i,tabn+6)
            fpeqo(i,3,3)  = uvar(i,tabn+7)
            fpeqo(i,1,2)  = uvar(i,tabn+8)
            fpeqo(i,2,3)  = uvar(i,tabn+9)
            fpeqo(i,3,1)  = uvar(i,tabn+10)
            fpeqo(i,2,1)  = uvar(i,tabn+11)
            fpeqo(i,3,2)  = uvar(i,tabn+12)
            fpeqo(i,1,3)  = uvar(i,tabn+13)
         enddo
         if (jcvt >0 ) then ! corotational => need to rotate new fp to the global frame
            call rottoloc (nel,fpeqo,&
            &r1x, r1y, r1z, r2x, r2y, r2z, r3x, r3y, r3z)
         endif
         ! f^e = f * inv fpeqo then f^ef^e^t = b trial when having plasiticity in equilibrium network
         call calcmatb (nel, f, fpeqo, fft)!
      else
         !  f^ef^e^t = b trial considering trial f = f^e
         call prodaat(f ,  fft, nel) ! b = f * ft
      endif



      !===========================
      !network 0 : compute stress
      !===========================
      if (flag_he == 1) then
         call polystress2(&
         &nel , fft , c10, c01, c20,&
         &c11 ,c02, c30, c21, c12,&
         &c03 ,d1 ,d2  ,  d3, siga ,&
         &bi1,bi2,jdet ,flag_mul,&
         &nvarf,coefr, betaf,coefm  ,uvarf,rbulk,iform)
      elseif (flag_he == 2) then
         call sigaboyce(&
         &nel , fft ,c1,c2  ,c3,&
         &c4  ,c5   ,mu,beta,d ,&
         &siga ,bi1  , jdet ,flag_mul,&
         &nvarf,coefr, betaf,coefm  ,uvarf  )
      elseif (flag_he == 13) then
         call neo_hook_t(&
         &nel , fft , siga ,&
         &bi1,jdet ,flag_mul,munh,dnh,&
         &nvarf,coefr, betaf,coefm  ,uvarf)

      endif ! flag_he

      !========================================
      !equilibrium networks: compute plasticity
      !========================================
      if (flag_pl == 1) then
         do i = 1,nel
            tracea(i) = third*(siga(i,1,1) +siga(i,2,2) + siga(i,3,3))
            !deviator of stress a
            sa1(i) =  siga(i,1,1)  - tracea(i)
            sa2(i) =  siga(i,2,2)  - tracea(i)
            sa3(i) =  siga(i,3,3)  - tracea(i)
            sa4(i) =  siga(i,1,2)
            sa5(i) =  siga(i,2,3)
            sa6(i) =  siga(i,3,1)
            tanorm(i)   = sqrt( (max(em20,sa1(i)**2+sa2(i)**2+sa3(i)**2&
            &+     two*(sa4(i)**2+sa5(i)**2+sa6(i)**2 )) ) ) ! norm!
         enddo
!
         do i=1,nel
            pla (i)   = uvar(i,tabn+4)
            s(i,1,1) = one
            s(i,2,2) = one
            s(i,3,3) = one
            s(i,1,2) = zero
            s(i,2,3) = zero
            s(i,3,1) = zero
            s(i,2,1) = zero
            s(i,3,2) = zero
            s(i,1,3) = zero
         end do
         do  i=1,nel
            temp1   =   tanorm(i) / tauy(i)
            plap(i) =   facpl*temp1**exppl
            !plap(i)= facpl* exp(temp1 )
            dpla(i) = plap(i)*timestep
            pla(i)  =    uvar(i,tabn+4) + dpla(i)
            factor    = plap(i)*timestep/tanorm(i)
            s(i,1,1) = one+factor *sa1(i) !gamma_dot*n
            s(i,2,2) = one+factor *sa2(i)
            s(i,3,3) = one+factor *sa3(i)
            s(i,1,2) = factor *sa4(i)  !gamma_dot*n
            s(i,2,3) = factor *sa5(i)
            s(i,3,1) = factor *sa6(i)
            s(i,2,1) = s(i,1,2)
            s(i,3,2) = s(i,2,3)
            s(i,1,3) = s(i,3,1)
         enddo ! j=1,nindx
         call prodmat(s  ,fpeqo,  fpeq, nel) ! f_n+1 = (i + dt *dp)* f_n
         call calcmatb (nel, f, fpeq, fftn) ! b = f * ft
         !update stress
         if (flag_he == 1) then
            call polystress2(&
            &nel , fftn , c10, c01, c20,&
            &c11 ,c02, c30, c21, c12,&
            &c03 ,d1 ,d2  ,  d3, siga ,&
            &bi1,bi2,jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf,rbulk,iform)
         elseif (flag_he == 2) then
            call sigaboyce(&
            &nel , fftn ,c1,c2  ,c3,&
            &c4  ,c5   ,mu,beta,d ,&
            &siga ,bi1  , jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf  )
         elseif (flag_he == 13) then
            call neo_hook_t(&
            &nel , fft , siga ,&
            &bi1,jdet ,flag_mul,munh,dnh,&
            &nvarf,coefr, betaf,coefm  ,uvarf)

         endif ! flag_he
         if (jcvt >0 ) then ! corotational => need to rotate new fp to the global frame
            call rottoglob (nel,fpeq,&
            &r1x, r1y, r1z, r2x, r2y, r2z, r3x, r3y, r3z)

         endif
         do i=1,nel
            tauy(i)  = tauy0 * (ff +(one - ff)*exp(-pla(i)/epshat))
            !tauy(i)  = hh *(one - uvar(i,1)/tauy0)*plap(i)*timestep + uvar(i,1)
            uvar(i,tabn +1) = tauy(i)
            uvar(i,tabn +4) = pla(i)
            uvar(i,tabn +5)    =   fpeq(i,1,1) ! stored in global frame always
            uvar(i,tabn +6)    =   fpeq(i,2,2)
            uvar(i,tabn +7)    =   fpeq(i,3,3)
            uvar(i,tabn +8)    =   fpeq(i,1,2)
            uvar(i,tabn +9)    =   fpeq(i,2,3)
            uvar(i,tabn +10)   =   fpeq(i,3,1)
            uvar(i,tabn +11)   =   fpeq(i,2,1)
            uvar(i,tabn +12)   =   fpeq(i,3,2)
            uvar(i,tabn +13)   =   fpeq(i,1,3)
         enddo
         tabn = tabn + 13
      endif! if (flag_pl == 1)

      !====================================
      !secondary networks: compute stresses
      !====================================
      flag_mul   =  0
      do i=1,nel
         !print*, ' jdet', jdet(i)
         signxx(i) = siga(i,1,1)
         signyy(i) = siga(i,2,2)
         signzz(i) = siga(i,3,3)
         signxy(i) = siga(i,1,2)
         signyz(i) = siga(i,2,3)
         signzx(i) = siga(i,3,1)
      enddo
      !------------------------
      !start loop over networks
      !------------------------
      !***************************************************************
      do n = 1, n_network
         !***************************************************************
         do i=1,nel
            sigb(i,1,1) = zero
            sigb(i,2,2) = zero
            sigb(i,3,3) = zero
            sigb(i,1,2) = zero
            sigb(i,2,3) = zero
            sigb(i,3,1) = zero
            fpo(i,1,1)  = uvar(i,tabn+1) ! global frame must be modified to local if jcvt >0
            fpo(i,2,2)  = uvar(i,tabn+2)
            fpo(i,3,3)  = uvar(i,tabn+3)
            fpo(i,1,2)  = uvar(i,tabn+4)
            fpo(i,2,3)  = uvar(i,tabn+5)
            fpo(i,3,1)  = uvar(i,tabn+6)
            fpo(i,2,1)  = uvar(i,tabn+7)
            fpo(i,3,2)  = uvar(i,tabn+8)
            fpo(i,1,3)  = uvar(i,tabn+9)
!
         enddo
         if (jcvt >0 ) then ! corotational => need to rotate new fp to the global frame
            call rottoloc (nel,fpo,&
            &r1x, r1y, r1z, r2x, r2y, r2z, r3x, r3y, r3z)
         endif
         !fe ={f}{fp_old}^(-1) then  matb = fe fe^(t) (elastic part)
         !call calcmatb (nel, f, fpo, matb)
         call kmatinv3 (fpo , invfpo, nel)      !invfpo = inverse (fp)
         call prodmat  (f   , invfpo, fe, nel)  ! fe = f * invfpo
         call prodaat  (fe  , matb  , nel)      ! matb = fe fe^(t)

         !--------------------------------------------
         !estimate trial cauchy stress
         !--------------------------------------------
         ! secondary network : compute trial stress :
         !--------------------------------------------
         if (flag_he == 1) then !polynomial
            call polystress2(&
            &nel , matb , c10, c01, c20,&
            &c11 ,c02, c30, c21, c12,&
            &c03 ,d1 ,d2  ,  d3, sigb ,&
            &bi1,bi2,jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf,rbulk,iform)

         elseif (flag_he == 2) then !arruda boyce
            call sigaboyce(&
            &nel , matb ,c1,c2  ,c3,&
            &c4  ,c5   ,mu,beta,d ,&
            &sigb,bi1  ,jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf)
         elseif (flag_he == 13) then !thermal neo hook
            call neo_hook_t(&
            &nel , fft , sigb ,&
            &bi1,jdet ,flag_mul,munh,dnh,&
            &nvarf,coefr, betaf,coefm  ,uvarf)


         endif ! flag_he

         do i=1,nel  !     scale trial cauchy stress in chain b
            sigb(i,1,1) = stiffn(n) *  sigb(i,1,1)
            sigb(i,2,2) = stiffn(n) *  sigb(i,2,2)
            sigb(i,3,3) = stiffn(n) *  sigb(i,3,3)
            sigb(i,1,2) = stiffn(n) *  sigb(i,1,2)
            sigb(i,2,3) = stiffn(n) *  sigb(i,2,3)
            sigb(i,3,1) = stiffn(n) *  sigb(i,3,1)
         enddo

         !compute eeffective creep strain rate
         !------------------------------------
         do i=1,nel
            traceb(i) = third*(sigb(i,1,1) +sigb(i,2,2) + sigb(i,3,3))
            !deviator of secondary network stress
            sb1(i) =  sigb(i,1,1)  - traceb(i)
            sb2(i) =  sigb(i,2,2)  - traceb(i)
            sb3(i) =  sigb(i,3,3)  - traceb(i)
            sb4(i) =  sigb(i,1,2)
            sb5(i) =  sigb(i,2,3)
            sb6(i) =  sigb(i,3,1)
            !nomr of stress secondary network n
            tbnorm(i)   = sqrt (max(em20,sb1(i)**2+sb2(i)**2+sb3(i)**2&
            &+     two*(sb4(i)**2+sb5(i)**2+sb6(i)**2 ))  ) ! norm!
         enddo

         !------------------------------------
         !compute eeffective creep strain rate
         !------------------------------------
         if (flag_visc(n) == 1 ) then!bergstrom boyce model
            call viscbb ( nel, fpo, tbnorm, a1(n) ,expc(n),&
            &expm(n) , ksi(n), tauref(n), dgamma )
            do i=1,nel
               uvar(i,tabn+10) =   dgamma(i)
               uvar(i,tabn+11) =   tbnorm(i)
            enddo
            shift = 9  +1 +1

         elseif (flag_visc(n) == 2 )then !hyperbolic sine
            call viscsinh ( nel, tbnorm,  a1(n),b0(n),&
            &expn(n) , dgamma )
            do i=1,nel
               uvar(i,tabn+10) =   dgamma(i)
               uvar(i,tabn+11) =   tbnorm(i)
            enddo
            shift = 9  + 1 +1
         elseif (flag_visc(n) == 3 )then ! power law
            do i=1,nel
               gammaold(i) =  uvar(i,tabn+10)
            enddo
            call viscpower ( nel, tbnorm,  a1(n),expm(n) ,expn(n),gammaold, dgamma )
            do i=1,nel
               uvar(i,tabn+10) =   gammaold(i) +  dgamma(i)
               uvar(i,tabn+11) =   dgamma(i)
               uvar(i,tabn+12) =   tbnorm(i)
            enddo
            shift = 10  +1 +1
         endif
         !------------------------------------

         do i=1,nel
            factor = dgamma(i)/tbnorm(i)
            lb(i,1,1) = factor *sb1(i) !gamma_dot*n
            lb(i,2,2) = factor *sb2(i)
            lb(i,3,3) = factor *sb3(i)
            lb(i,1,2) = factor *sb4(i) !gamma_dot*n
            lb(i,2,3) = factor *sb5(i)
            lb(i,3,1) = factor *sb6(i)
            lb(i,2,1) = lb(i,1,2)
            lb(i,3,2) = lb(i,2,3)
            lb(i,1,3) = lb(i,3,1)
         enddo
         !------------------------------------
         !solve f_n+1 viscous :
         !------------------------------------
         call kmatinv3(fe , invfe, nel)       !  invfe  = inverse (fe)
         call prodmat (lb ,    fe, fedp, nel) !  fedp   = lb * fe
         call prodmat(invfe ,fedp, dfp , nel) !  dfp    = invfe  * lb * fe

         !call prodmat(dfp ,dfp, dfp2 , nel) ! if order 2
         do i=1,nel
            ! fp_n+1 = exp(dfp) * fp_old = (i+dfp) * fp_old
            sn(i,1,1) = one + dfp(i,1,1) !+ half * dfp2(i,1,1)
            sn(i,2,2) = one + dfp(i,2,2) !+ half * dfp2(i,2,2)
            sn(i,3,3) = one + dfp(i,3,3) !+ half * dfp2(i,3,3)
            sn(i,1,2) = dfp(i,1,2)!+ half * dfp2(i,1,2)
            sn(i,2,3) = dfp(i,2,3)!+ half * dfp2(i,2,3)
            sn(i,3,1) = dfp(i,3,1)!+ half * dfp2(i,3,1)
            sn(i,2,1) = dfp(i,2,1)!+ half * dfp2(i,2,1)
            sn(i,3,2) = dfp(i,3,2)!+ half * dfp2(i,3,2)
            sn(i,1,3) = dfp(i,1,3)!+ half * dfp2(i,1,3)
         enddo

         call prodmat(sn ,fpo,  fp, nel)  !fp_n+1 = (i + dt *dfp)* fp_n
         call calcmatb (nel, f, fp, matb) !fe ={f}{fp}^(-1) then  matb = fe fe^(t)

         if (flag_he == 1) then
            call polystress2(&
            &nel , matb , c10, c01, c20,&
            &c11 ,c02, c30, c21, c12,&
            &c03 ,d1 ,d2  ,  d3, sigb ,&
            &bi1,bi2,jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf,rbulk,iform)
         elseif (flag_he == 2) then
            call sigaboyce(&
            &nel , matb ,c1,c2  ,c3,&
            &c4  ,c5   ,mu,beta,d ,&
            &sigb,bi1  ,jdet ,flag_mul,&
            &nvarf,coefr, betaf,coefm  ,uvarf)

         elseif (flag_he == 13) then
            call neo_hook_t(&
            &nel , fft , sigb ,&
            &bi1,jdet ,flag_mul,munh,dnh,&
            &nvarf,coefr, betaf,coefm  ,uvarf)


         endif ! flag_he

         if (jcvt >0 ) then ! corotational => need to rotate new fp to the global frame
            call rottoglob (nel,fp,&
            &r1x, r1y, r1z, r2x, r2y, r2z, r3x, r3y, r3z)

         endif

         do i=1,nel
            sigb(i,1,1) = stiffn(n) *  sigb(i,1,1)
            sigb(i,2,2) = stiffn(n) *  sigb(i,2,2)
            sigb(i,3,3) = stiffn(n) *  sigb(i,3,3)
            sigb(i,1,2) = stiffn(n) *  sigb(i,1,2)
            sigb(i,2,3) = stiffn(n) *  sigb(i,2,3)
            sigb(i,3,1) = stiffn(n) *  sigb(i,3,1)
            uvar(i,tabn+1)   =   fp(i,1,1)
            uvar(i,tabn+2)   =   fp(i,2,2)
            uvar(i,tabn+3)   =   fp(i,3,3)
            uvar(i,tabn+4)   =   fp(i,1,2)
            uvar(i,tabn+5)   =   fp(i,2,3)
            uvar(i,tabn+6)   =   fp(i,3,1)
            uvar(i,tabn+7)   =   fp(i,2,1)
            uvar(i,tabn+8)   =   fp(i,3,2)
            uvar(i,tabn+9)   =   fp(i,1,3)
         enddo

         !--------------------
         !update total stress
         !--------------------
         do i=1,nel
            signxx(i) = signxx(i) + sigb(i,1,1)
            signyy(i) = signyy(i) + sigb(i,2,2)
            signzz(i) = signzz(i) + sigb(i,3,3)
            signxy(i) = signxy(i) + sigb(i,1,2)
            signyz(i) = signyz(i) + sigb(i,2,3)
            signzx(i) = signzx(i) + sigb(i,3,1)
         enddo
         tabn = tabn + shift

      enddo! n = 1, n_network
      !***************************************************************
      !***************************************************************


!-------------------------------
! cauchy to global
!-------------------------------
      do i=1,nel
         dsig = sqrt((signxx(i)-sigoxx(i))**2+(signyy(i)-sigoyy(i))**2+(signzz(i)-sigozz(i))**2&
         &+ two*((signxy(i)-sigoxy(i))**2+(signyz(i)-sigoyz(i))**2+(signzx(i)-sigozx(i))**2))
         deps = sqrt   (depsxx(i)**2 + depsyy(i)**2 + depszz(i)**2&
         &+ two*(depsxy(i)**2 + depsyz(i)**2 + depszx(i)**2))
         if(deps == zero)then
            stiff(i)= stiff0
         else
            stiff(i)= max(stiff0 ,dsig /max(em20,deps))
         endif
!
         soundsp(i)=sqrt(stiff(i)/rho(i))

         viscmax(i) = zero
      enddo
      if (impl_s > 0 .or. ihet > 1) then
         do i=1,nel
            et(i)= max(one,stiff(i))
         enddo
      endif
!-----------
      return
   end
!
end module
