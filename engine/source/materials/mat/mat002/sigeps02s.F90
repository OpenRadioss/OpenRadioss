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
!|========================================================================================

      module sigeps02s_mod
      contains

! ==================================================================================
! \brief main routine of Johnson-Cook plastic material law
! \details 

      !||====================================================================
      !||    sigeps02s          ../engine/source/materials/mat/mat002/sigeps02s.F90
      !||--- called by ------------------------------------------------------
      !||    mmain          ../engine/source/materials/mat_share/mmain.F90
      !||--- calls      -----------------------------------------------------
      !||    m2iter_imp     ../engine/source/materials/mat/mat002/m2iter_imp.F
      !||    mdtsph         ../engine/source/materials/mat_share/mdtsph.F
      !||    mqviscb        ../engine/source/materials/mat_share/mqviscb.F
      !||    mstrain_rate   ../engine/source/materials/mat_share/mstrain_rate.F
      !||====================================================================

      subroutine sigeps02s(mat_param,                              &
                 pm,      off,     sig,     eint,                  &
                 rho,     qold,    pla,     epsd,                  &
                 vol,     stifn,   dt2t,    neltst,                &
                 ityptst, offg,    geo,     pid,                   &
                 amu,     vol_avg, mumax,   mat,                   &
                 ngl,     ssp,     dvol,    aire,                  &
                 vnew,    vd2,     deltax,  vis,                   &
                 d1,      d2,      d3,      d4,                    &
                 d5,      d6,      pnew,    psh,                   &
                 qnew,    ssp_eq,  sold1,   sold2,                 &
                 sold3,   sold4,   sold5,   sold6,                 &
                 sigy,    defp,    dpla,                           &
                 epsp,    tstar,   etse,    mssa,                  &
                 dmels,   tempel,  sigbak,  al_imp,                &
                 signor,  conde,   dtel,    g_dt,                  &
                 nel,     ipm,     rhoref,  rhosp,                 &
                 ipg,     dmg,     ity,     jtur,                  &
                 jthe,    jsph,    ismstr,  jsms,                  &
                 plap,    npg ,    ieos  ,  dpdm ,    fheat   ,    &
                 mvsiz   ,n_var_pm ,n_var_ipm ,n_var_geo   ,       &
                 nummat  ,numgeo   ,dt1     ,time     ,impl_s  )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use constant_mod ,only : half,one,zero,two,third,two_third,three,four
      use constant_mod ,only : em15,em20,ep20,onep333
! ----------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------

#include "my_real.inc"
#include "units_c.inc"

!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: npg
      integer, intent(in) :: ismstr
      integer, intent(in) :: jsms
      integer, intent(in) :: ity
      integer, intent(in) :: jtur
      integer, intent(in) :: jthe
      integer, intent(in) :: jsph
      integer, intent(in) :: ieos
      integer, intent(in) :: mvsiz
      integer, intent(in) :: n_var_pm
      integer, intent(in) :: n_var_ipm
      integer, intent(in) :: n_var_geo
      integer, intent(in) :: nummat
      integer, intent(in) :: numgeo
      integer, intent(in) :: impl_s     !< implicit flag
!
      integer :: neltst,ityptst,g_dt,nel,ipg
      integer :: mat(nel),pid(nel),ngl(nel)
      integer :: ipm(n_var_ipm,nummat)
      my_real, dimension(nel) ,intent(inout) :: plap
      my_real :: dt2t
      my_real, intent(in) :: dt1
      my_real, intent(in) :: time
      my_real :: pm(n_var_pm,nummat)
      my_real :: sig(nel,6)
      my_real :: geo(n_var_geo,numgeo)
      my_real :: signor(mvsiz,6)
      my_real :: sigbak(nel,6)
!
      my_real, dimension(nel)   :: off,eint,rho,qold, pla, epsd
      my_real, dimension(nel)   :: vol, stifn, offg, mumax,amu,vol_avg,dmg
      my_real, dimension(nel)   :: vnew, vd2, deltax, ssp, aire, vis 
      my_real, dimension(nel)   :: psh, pnew,qnew ,ssp_eq, dvol 
      my_real, dimension(nel)   :: d1, d2, d3, d4, d5, d6 
      my_real, dimension(nel)   :: sigy,defp,etse,mssa,dmels,tempel,al_imp
      my_real, dimension(nel)   :: conde,dtel,rhoref,rhosp
      my_real, dimension(mvsiz) :: sold1,sold2,sold3,sold4,sold5,sold6 
      my_real, dimension(mvsiz) :: tstar, dpla, epsp
      my_real, dimension(mvsiz) ,intent(in)    :: dpdm
      my_real, dimension(mvsiz) ,intent(inout) :: fheat
      type (matparam_struct_)   ,intent(in)    :: mat_param
!-----------------------------------------------
!   L o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,imat,iform,icc,israte,vp,idev,ibid,nindx
      integer :: indx(nel)
      my_real :: plap1,pold,pshift,p,epmx,sigm0,cc,cn,epdr,m_exp
      my_real :: rho0,bulk,g0,g1,g2
      my_real :: c3,c4,rhocpi,fcut,asrate,scale,dta,mt,ca0,cb
      my_real :: e1,e2,e3,e4,e5,e6,einc
      my_real :: dsxx,dsyy,dszz,dsxy,dsyz,dszx,alpha,hkin
      my_real :: facq0,fisokin,beta,vm,vm_1,g3,g3h,norm_1
      my_real, dimension(nel)   :: g,ca,epd,sigmx
      my_real, dimension(mvsiz) :: ak,pc,qh,aj2,dav
      my_real, dimension(mvsiz) :: sigexx,sigeyy,sigezz,sigexy,sigeyz,sigezx
      my_real, dimension(mvsiz) :: bidmvsiz
!=======================================================================
      iform  = mat_param%iparam(1)
      icc    = mat_param%iparam(2)        
      vp     = mat_param%iparam(3)        
      israte = mat_param%iparam(4)        
      imat   = mat(1)

      rho0  = mat_param%rho
      bulk  = mat_param%bulk 
      g0    = mat_param%shear
!
      ca0     = mat_param%uparam(1)         ! pm(38)
      cb      = mat_param%uparam(2)         ! pm(39)
      cn      = mat_param%uparam(3)         ! pm(40)
      epmx    = mat_param%uparam(4)         ! pm(41)
      sigm0   = mat_param%uparam(5)         ! pm(42)
      cc      = mat_param%uparam(6)         ! pm(43)
      epdr    = mat_param%uparam(7)         ! pm(44)
      fisokin = mat_param%uparam(8)         ! pm(55)
      fcut    = mat_param%uparam(9)         ! pm(9)
      asrate  = min(one, fcut*dt1)

      if (iform == 1) then        ! zerilli
        c3      = mat_param%uparam(10)        ! pm(51)
        c4      = mat_param%uparam(11)        ! pm(52)
        rhocpi  = mat_param%uparam(12)        ! pm(53)       
      else if  (iform == 2) then  ! predef
        asrate  = zero
        rhocpi  = zero        
      else                        ! johnson-cook
        rhocpi = mat_param%uparam(10)
        if (rhocpi > zero) rhocpi = one / rhocpi
        m_exp = mat_param%uparam(13)        ! pm(51) 
      end if
!
      idev    = vp - 2
      facq0   = one
!
      g(:)     = g0*off(:)
      ca(:)    = ca0
      etse(:)  = one
      sigmx(:) = sigm0
!
!------------------------------------------
!     ecrouissage cine
!------------------------------------------
      if (fisokin /= zero ) then
        do i=1,nel
          sig(i,1)=sig(i,1)-sigbak(i,1)
          sig(i,2)=sig(i,2)-sigbak(i,2)
          sig(i,3)=sig(i,3)-sigbak(i,3)
          sig(i,4)=sig(i,4)-sigbak(i,4)
          sig(i,5)=sig(i,5)-sigbak(i,5)
          sig(i,6)=sig(i,6)-sigbak(i,6)
        enddo
      endif           
!      
      do i=1,nel
        p  =-third*(sig(i,1)+sig(i,2)+sig(i,3))
        dav(i) =-third*(d1(i)+d2(i)+d3(i))
        g1     = dt1*g(i)
        g2     = two*g1
        ssp(i) = sqrt((onep333*g(i)+bulk)/rho0)
        ! deviatoric elastic stress predictor
        sig(i,1) = sig(i,1)+p+g2*(d1(i)+dav(i))
        sig(i,2) = sig(i,2)+p+g2*(d2(i)+dav(i))
        sig(i,3) = sig(i,3)+p+g2*(d3(i)+dav(i))
        sig(i,4) = sig(i,4)+g1*d4(i)
        sig(i,5) = sig(i,5)+g1*d5(i)
        sig(i,6) = sig(i,6)+g1*d6(i)
!
!     -- von mises stress at elastic predictor
       aj2(i) = half*(sig(i,1)**2+sig(i,2)**2+sig(i,3)**2)          &
                    + sig(i,4)**2+sig(i,5)**2+sig(i,6)**2
       aj2(i) = sqrt(three*aj2(i))
      enddo
!
!     --  storing elastic predictors for stiffness computation
      if (impl_s > 0 .or. fisokin > 0) then
        do i=1,nel
         sigexx(i) = sig(i,1)
         sigeyy(i) = sig(i,2)
         sigezz(i) = sig(i,3)        
         sigexy(i) = sig(i,4)
         sigeyz(i) = sig(i,5)
         sigezx(i) = sig(i,6)
        enddo
      endif
!-------------
!     strain rate (johnson-cook, zerilli-armstrong)
!-------------
      call mstrain_rate(nel  ,israte,asrate ,epsd   ,idev   ,            &
                        d1   ,d2    , d3    ,d4     ,d5     ,d6      )

      epsp(1:nel) = epsd(1:nel)                   
!

      if (cc > zero) then
        if (vp == 1) then
          do i=1,nel
            epd(i) = max(plap(i),epdr)
            epd(i) = log(epd(i)/epdr)  
          enddo
        else
          do i=1,nel
            epd(i) = max(epsd(i),em15)
            epd(i) = log(epd(i)/epdr)  
          enddo
        endif ! vp
!  
        if (iform == 0) then   ! j-c
          do i=1,nel
             mt = max(em15,m_exp)
             epd(i) = max(zero,epd(i))
             epd(i) = (one + cc * epd(i))*(one - tstar(i)**mt)
             if (icc == 1) sigmx(i) = sigm0*epd(i)
          enddo
        elseif (iform == 1) then   ! Zerilli
          do i=1,nel
             epd(i) = cc*exp((-c3+c4 * epd(i))*tempel(i))
             if (icc == 1) sigmx(i) = sigm0 + epd(i)
          enddo
        end if
      else    ! cc = 0
        epd(:) = one
      endif
!-------------
!     critere
!-------------
      if (fisokin == zero ) then  ! isotropic hardening
        if (cn == one) then
          ak(1:nel) = ca(1:nel) + cb*pla(1:nel)
          qh(1:nel) = cb*epd(1:nel)
        else
          do  i=1,nel
            if (pla(i) > zero) then
              ak(i) = ca(i) + cb*pla(i)**cn
              if (cn > one) then
                qh(i) = (cb*cn*pla(i)**(cn - one))*epd(i)
              else
                qh(i) = (cb*cn/pla(i)**(one - cn))*epd(i)
              endif
            else
              ak(i) = ca(i)
              qh(i) = zero
            endif
          enddo
        endif
        do i=1,nel
          ak(i) = ak(i)*epd(i)
          if (sigmx(i) < ak(i)) then
            ak(i) = sigmx(i)
            qh(i) = zero
          endif
          sigy(i) = ak(i)
          if (pla(i) > epmx) then
            ak(i) = zero
            qh(i) = zero
          endif
        enddo
!
      else     ! fisokin > 0 => kinematic hardening
!
        do i=1,nel
          beta = one-fisokin
          ! sigy is used for hourglass stress compute--        
          if (cn == one) then
            sigy(i) = ca(i) + cb*pla(i)
            ak(i)   = ca(i) + beta*cb*pla(i)
            qh(i)   = cb*epd(i)
          else
            if (pla(i) > zero) then
              sigy(i) = ca(i) + cb*pla(i)**cn
              ak(i)   = ca(i) + beta*cb*pla(i)**cn
              if(cn>one) then
                qh(i)= (cb*cn*pla(i)**(cn - one))*epd(i)
              else
                qh(i)= (cb*cn/pla(i)**(one -cn))*epd(i)
              endif
            else
              ak(i)   = ca(i)
              sigy(i) = ca(i)
              qh(i)   = zero
            endif
          endif
          ak(i) = ak(i)*epd(i)
          sigy(i) = sigy(i)*epd(i)
          if (sigmx(i) < ak(i)) then
            ak(i) = sigmx(i)
            qh(i) = zero
          endif
          sigy(i) = min(sigy(i),sigmx(i))
          if (pla(i) > epmx) then
            ak(i) = zero
            qh(i) = zero
          endif
        end do ! i=1,nel
      end if   ! fisokin
!---------------------------------------------------------
      if (impl_s == 0) then
        do i=1,nel
          scale= min(one,ak(i)/ max(aj2(i),em15))
          ! plastic strain increment
          dpla(i)=(one-scale)*aj2(i) / max(three*g(i)+qh(i),em15)
          ! actual yield stress
          ak(i) = ak(i)+(one - fisokin)*dpla(i)*qh(i)
          scale= min(one,ak(i)/ max(aj2(i),em15))
          sig(i,1)=scale*sig(i,1)
          sig(i,2)=scale*sig(i,2)
          sig(i,3)=scale*sig(i,3)
          sig(i,4)=scale*sig(i,4)
          sig(i,5)=scale*sig(i,5)
          sig(i,6)=scale*sig(i,6)
          pla(i)=pla(i)+dpla(i)
        enddo
!
      else    ! implicit : nonlinear hardening requires iterations in radial return
        call  m2iter_imp(sig,     pla,    aj2,     g  ,            &
                         ca,      cb,     cn,      epd,            &
                         sigmx,   epmx,   dpla,    ak ,            &
                         qh,      sigy,   fisokin, nel)
!
      end if ! impl_s
!-----------------------------------------
      if (fisokin > zero ) then
       do i=1,nel
          dsxx = sigexx(i) - sig(i,1) 
          dsyy = sigeyy(i) - sig(i,2)
          dszz = sigezz(i) - sig(i,3)
          dsxy = sigexy(i) - sig(i,4)
          dsyz = sigeyz(i) - sig(i,5)
          dszx = sigezx(i) - sig(i,6)
! 
          hkin  = two_third*fisokin*qh(i)
          alpha = hkin/max(two*g(i)+hkin,em15)  
!       ..updates back stresses
          sigbak(i,1) = sigbak(i,1) + alpha*dsxx 
          sigbak(i,2) = sigbak(i,2) + alpha*dsyy 
          sigbak(i,3) = sigbak(i,3) + alpha*dszz 
          sigbak(i,4) = sigbak(i,4) + alpha*dsxy 
          sigbak(i,5) = sigbak(i,5) + alpha*dsyz 
          sigbak(i,6) = sigbak(i,6) + alpha*dszx 
!       ..gets stresses from shifted stresses and back stresses
          sig(i,1)=sig(i,1) + sigbak(i,1)
          sig(i,2)=sig(i,2) + sigbak(i,2)
          sig(i,3)=sig(i,3) + sigbak(i,3)
          sig(i,4)=sig(i,4) + sigbak(i,4)
          sig(i,5)=sig(i,5) + sigbak(i,5)
          sig(i,6)=sig(i,6) + sigbak(i,6)
       enddo
      end if !  fisokin > 0
!-----------------------------------------
!
      bidmvsiz(1:mvsiz) = zero
      if (jsph == 0) then
        call mqviscb(                                &
             pm,      off,     rho,     bidmvsiz,    &      
             bidmvsiz,ssp,     bidmvsiz,stifn,       &
             dt2t,    neltst,  ityptst, aire,        &
             offg,    geo,     pid,     vnew,        &
             vd2,     deltax,  vis,     d1,          &
             d2,      d3,      pnew,    psh,         &
             mat,     ngl,     qnew,    ssp_eq,      &
             vol,     mssa,    dmels,   ibid,        &
             facq0,   conde,   dtel,    g_dt,        &
             ipm,     rhoref,  rhosp,   nel,         &
             ity,     ismstr,  jtur,    jthe,        &
             jsms,    npg)
      else
        call mdtsph(                                 &
             pm,      off,     rho,     bidmvsiz,    &
             bidmvsiz,bidmvsiz,stifn,   dt2t,        &
             neltst,  ityptst, offg,    geo,         &
             pid,     mumax,   ssp,     vnew,        &
             vd2,     deltax,  vis,     d1,          &
             d2,      d3,      pnew,    psh,         &
             mat,     ngl,     qnew,    ssp_eq,      &
             g_dt,    dtel,    nel,     ity,         &
             jtur,    jthe)                          
      endif                                         
!                                                   
      dta = half*dt1                                
!
      nindx = 0
      indx(1:nel) = 0
      do i=1,nel
        if (pla(i) > epmx .and. dmg(i)==zero) then 
          nindx = nindx + 1
          indx(nindx) = i
          dmg(i) = one
        endif
      enddo
 
      if (ieos == 0) then !  add pressure to the deviatoric stress
        do i=1,nel
          pnew(i)  = bulk*amu(i)
          sig(i,1) =(sig(i,1) - pnew(i))*off(i)
          sig(i,2) =(sig(i,2) - pnew(i))*off(i)
          sig(i,3) =(sig(i,3) - pnew(i))*off(i)
          sig(i,4) = sig(i,4) * off(i)
          sig(i,5) = sig(i,5) * off(i)
          sig(i,6) = sig(i,6) * off(i)
        enddo
!
        do i=1,nel
          e1 = d1(i)*(sold1(i)+sig(i,1))
          e2 = d2(i)*(sold2(i)+sig(i,2))
          e3 = d3(i)*(sold3(i)+sig(i,3))
          e4 = d4(i)*(sold4(i)+sig(i,4))
          e5 = d5(i)*(sold5(i)+sig(i,5))
          e6 = d6(i)*(sold6(i)+sig(i,6))
          einc = vol_avg(i)*(e1+e2+e3+e4+e5+e6)*dta - half*dvol(i)*(qold(i)+qnew(i))
          eint(i) = (eint(i)+einc*off(i)) / max(em15,vol(i))
        enddo

      else
        ! if eos is used, material law calculates only deviatoric stress tensor
        !                 sound speed depends on pressure derivative over volume change
        !                 calculated in eos
        pshift = pm(88,imat)   ! pressure shift parameter from EOS
        do i = 1, nel
          ssp(i) = sqrt((dpdm(i) + four*g(i)/three)/rho0)
!
          pold     = (sold1(i)+sold2(i)+sold3(i)) * third
          sold1(i) = sold1(i) - pold
          sold2(i) = sold2(i) - pold
          sold3(i) = sold3(i) - pold
          e1 = d1(i) * (sold1(i)+sig(i,1))
          e2 = d2(i) * (sold2(i)+sig(i,2))
          e3 = d3(i) * (sold3(i)+sig(i,3))
          e4 = d4(i) * (sold4(i)+sig(i,4))
          e5 = d5(i) * (sold5(i)+sig(i,5))
          e6 = d6(i) * (sold6(i)+sig(i,6))
          einc    = vol_avg(i) * (e1+e2+e3+e4+e5+e6) * dta
          eint(i) = eint(i) + (einc+half*dvol(i)*(pold-pshift-qold(i)-qnew(i)))*off(i)
        enddo       
      end if       
!
      do i=1,nel
        qold(i) = qnew(i)
        defp(i) = pla(i)
        sigy(i) = max(sigy(i),ak(i))
      enddo
      do i=1,nel
        if (dpla(i) > 0) etse(i)= half*qh(i)*off(i)/max(g(i),em15)
      enddo
!----------------------------------------------------------------     
!     implicit
!----------------------------------------------------------------     
      if (impl_s > 0) then
!        if (ikt == 0) return
        if (fisokin == zero)then
          do i=1,nel
           if (dpla(i)>0)then
            if (cn == one) then
              qh(i) = cb*epd(i)
            elseif(cn > one) then
              qh(i) = (cb*cn*pla(i)**(cn - one))*epd(i)
            else
              qh(i) = (cb*cn/pla(i)**(one -cn))*epd(i)
            endif
           endif
          enddo
        endif
!
        do i = 1,nel
         if (dpla(i)>zero) then

! .....   von mises stress at the elastic predictor (point b)
! .....   sigexx, etc. are deviatoric stresses
          vm = half*(sigexx(i)**2+sigeyy(i)**2+sigezz(i)**2)    &
                  + sigexy(i)**2+sigeyz(i)**2+sigezx(i)**2
          vm_1 = one/sqrt(three*vm)
          g3 = three*g(i)
          g3h = max(g3+qh(i),em15)
          scale = max(zero,one-g3h*dpla(i)*vm_1)
! .....   norm_1 normalizes deviatoric stresses, includes consistent
! .....   stiffness matrix parameter beta, von mises at b, and two_pmi
          norm_1=g3*vm_1*sqrt(scale/g3h)
! .....   deviatoric stresses "normalized"
          signor(i,1)=sigexx(i)*norm_1
          signor(i,2)=sigeyy(i)*norm_1
          signor(i,3)=sigezz(i)*norm_1
          signor(i,4)=sigexy(i)*norm_1
          signor(i,5)=sigeyz(i)*norm_1
          signor(i,6)=sigezx(i)*norm_1

! .....   parameter alpha of consistent matrix
          al_imp(i)= one - g3*dpla(i)*vm_1
         else
           al_imp(i)=one
         endif
        enddo
      endif
!---------------------------------------------------------
      ! end implicit
!---------------------------------------------------------
!     update and filter plastic strain rate for vp=1
!---------------------------------------------------------
      if (vp == 1) then
        do i=1,nel       
          plap1   = dpla(i)/max(em20,dt1)
          plap(i) = asrate * plap1 + (one - asrate) * plap(i)
        enddo
      endif
!---------------------------------------------------------
!     calculate thermal load due to plastic work
!---------------------------------------------------------
      if (ieos == 0) then
        if (jthe == 0 .and. rhocpi > zero) then  ! update temperature in adiabatic conditions
          do i=1,nel       
            tempel(i) = tempel(i) + sigy(i)*dpla(i) * rhocpi
          enddo
        else if (jthe /= 0) then  ! cumulate thermal load due to plastic work for /heat/mat
          do i=1,nel       
            fheat(i) = fheat(i) + sigy(i)*dpla(i)*vol(i)
          enddo
        end if
      else
        ! temperature calculation must be done in eos
      end if
!---------------------------------------------------------
      ! printout element deletion
      if (nindx > 0) then
        do i=1,nindx
!$OMP CRITICAL
          write(iout, 1000) ngl(indx(i)),ipg
          write(istdo,1100) ngl(indx(i)),ipg,time
!$OMP END CRITICAL
        enddo
      endif
!---------------------------------------------------------------
 1000 format(1X,'EXCEEDED EPS_MAX ON SOLID ELEMENT NUMBER ',I10,      &
       ': DEVIATORIC STRESS SET TO 0 ON INTEGRATION POINT ',I5 )
 1100 format(1X,'EXCEEDED EPS_MAX ON SOLID ELEMENT NUMBER ',I10,      &
       ': DEVIATORIC STRESS SET TO 0 ON INTEGRATION POINT ',I5 ,      &
                ' AT TIME :',G11.4)  
!-----------
      return
      end
!-----------
      end module sigeps02s_mod
