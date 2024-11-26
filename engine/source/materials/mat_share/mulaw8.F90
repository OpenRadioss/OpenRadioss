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
module mulaw8_mod
contains
!! \brief 8 integrztion point brick element material subroutine
!! \details Compute the material behavior for 8 integration points Brick element
subroutine mulaw8(                                    &
   &                 lft,     llt,     mtn,              &
   &                 npt,     d1,      d2,      d3,      &
   &                 d4,      d5,      d6,      pm,      &
   &                 off,     sig,     eint,    rho,     &
   &                 qold,    vol,     gama,    bufly,   &
   &                 bufmat,  tf,      npf,     stifn,   &
   &                 voln,    volgp,   deltax,  rho0,    &
   &                 dvol,    vd2,     vis,     epsd,    &
   &                 mat,     nc,      ngl,     wxx,     &
   &                 wyy,     wzz,     geo,     pid,     &
   &                 dt2t,    neltst,  ityptst, ipla,    &
   &                 rx,      ry,      rz,      sx,      &
   &                 sy,      sz,      tx,      ty,      &
   &                 tz,      ismstr,  ipm,     offg,    &
   &                 isorth,  et,      mssa,    dmels,   &
   &                 table,   ihet,    ssp,     itask,   &
   &                 nel,     ity,     jsms,    jsph,    &
   &                 jthe,    jtur,    nummat,           &
   &                 mat_param,        svis,    snpc,    &
   &                 dt1,     tt,      maxfunc, npropmi, &
   &                 npropg,  npropm,  imon_mat,numgeo,  &
   &                 sbufmat, stf,     ntable )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod
      use table_mod
      use mat_elem_mod
      use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      ! integer arguments (in)
      ! --------------------------------
      integer, intent(in) :: snpc
      integer, intent(in) :: nummat
      integer, intent(in) :: ity
      integer, intent(in) :: jsms
      integer, intent(in) :: jsph
      integer, intent(in) :: jthe
      integer, intent(in) :: jtur
      integer, intent(in) :: lft
      integer, intent(in) :: llt
      integer, intent(in) :: npt
      integer, intent(in) :: mtn
      integer, intent(in) :: isorth
      integer, intent(in) :: ipla
      integer, intent(in) :: neltst
      integer, intent(in) :: ityptst
      integer, intent(in) :: ihet
      integer, intent(in) :: itask
      integer, intent(in) :: nel
      integer, intent(in) :: maxfunc
      integer, intent(in) :: npropmi
      integer, intent(in) :: npropg
      integer, intent(in) :: npropm
      integer, intent(in) :: imon_mat
      integer, intent(in) :: numgeo
      integer, intent(in) :: sbufmat
      integer, intent(in) :: stf
      integer, intent(in) :: ntable
      integer, dimension(mvsiz),intent(in)   :: mat
      integer, dimension(mvsiz),intent(in)   :: ngl
      integer, dimension(8,mvsiz),intent(in) :: nc
      integer, dimension(mvsiz),intent(in)   :: pid
      integer, dimension(snpc),intent(in)    :: npf
      integer, dimension(npropmi,nummat)     :: ipm

      ! floating point arguments (in)
      ! --------------------------------
      my_real, dimension(npropm,nummat), intent(in) :: pm
      my_real, dimension(npropg,numgeo), intent(in) :: geo
      my_real, dimension(sbufmat), intent(in) :: bufmat
      my_real, dimension(stf), intent(in) :: tf
      my_real, intent(in) :: dt1
      my_real, intent(in) :: tt
      my_real, intent(in) :: dt2t

      ! integer arguments (inout)
      ! --------------------------------
      integer, intent(inout) :: ismstr

      ! floating point arguments (inout)
      ! --------------------------------
      my_real, dimension(llt), intent(inout) :: rho
      my_real, dimension(llt), intent(inout) :: qold
      my_real, dimension(llt), intent(inout) :: vol
      my_real, dimension(mvsiz,6), intent(inout) :: gama
      my_real, dimension(llt), intent(inout) :: epsd
      my_real, dimension(llt), intent(inout) :: off
      my_real, dimension(llt), intent(inout) :: offg
      my_real, dimension(llt), intent(inout) :: eint
      my_real, dimension(llt), intent(inout) :: mssa
      my_real, dimension(llt), intent(inout) :: dmels
      my_real, dimension(nel,6), intent(inout) :: sig
      my_real, dimension(mvsiz), intent(inout) :: voln
      my_real, dimension(mvsiz), intent(inout) :: vis
      my_real, dimension(mvsiz), intent(inout) :: ssp
      my_real, dimension(mvsiz), intent(inout) :: rho0
      my_real, dimension(mvsiz), intent(inout) :: dvol
      my_real, dimension(mvsiz), intent(inout) :: vd2
      my_real, dimension(mvsiz), intent(inout) :: deltax
      my_real, dimension(mvsiz), intent(inout) :: rx
      my_real, dimension(mvsiz), intent(inout) :: ry
      my_real, dimension(mvsiz), intent(inout) :: rz
      my_real, dimension(mvsiz), intent(inout) :: sx
      my_real, dimension(mvsiz), intent(inout) :: sy
      my_real, dimension(mvsiz), intent(inout) :: sz
      my_real, dimension(mvsiz), intent(inout) :: tx
      my_real, dimension(mvsiz), intent(inout) :: ty
      my_real, dimension(mvsiz), intent(inout) :: tz
      my_real, dimension(mvsiz), intent(inout) :: stifn
      my_real, dimension(mvsiz), intent(inout) :: et
      my_real, dimension(mvsiz,6), intent(inout) :: svis
      my_real, dimension(mvsiz,npt), intent(inout) :: volgp
      my_real, dimension(mvsiz,npt), intent(inout) :: d1
      my_real, dimension(mvsiz,npt), intent(inout) :: d2
      my_real, dimension(mvsiz,npt), intent(inout) :: d3
      my_real, dimension(mvsiz,npt), intent(inout) :: d4
      my_real, dimension(mvsiz,npt), intent(inout) :: d5
      my_real, dimension(mvsiz,npt), intent(inout) :: d6
      my_real, dimension(mvsiz,npt), intent(inout) :: wxx
      my_real, dimension(mvsiz,npt), intent(inout) :: wyy
      my_real, dimension(mvsiz,npt), intent(inout) :: wzz

      type(ttable),dimension(ntable),intent(in) ::  table
      type (buf_lay_),intent(inout), target :: bufly
      type (matparam_struct_) ,dimension(nummat) ,intent(inout) :: mat_param
      target :: mat_param
      target :: bufmat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      ! integer variables
      integer :: nuvar
      integer :: nvartmp
      integer :: npar
      integer :: nuparam
      integer :: niparam
      integer :: iadbuf
      integer :: nfunc
      integer :: i,j
      integer :: ipt,jpt,ir,israte
      integer :: ifunc(maxfunc)
      integer :: idum1
      integer :: irupt,imat,ntabl_fail
      integer :: nvarf, iexpan8,jj(6),inloc,ieos,dmg_flag
      ! floating point variables
      my_real, dimension(llt) :: cst1
      my_real, dimension(mvsiz) :: c1
      my_real, dimension(mvsiz) :: pnew
      my_real, dimension(mvsiz) :: pp
      my_real, dimension(mvsiz) :: defp
      my_real, dimension(mvsiz) :: ep1,ep2,ep3,ep4,ep5,ep6
      my_real, dimension(mvsiz) :: s1,s2,s3,s4,s5,s6
      my_real, dimension(mvsiz) :: es1,es2,es3,es4,es5,es6
      my_real, dimension(mvsiz) :: de1,de2,de3,de4,de5,de6
      my_real, dimension(mvsiz) :: sv1,sv2,sv3,sv4,sv5,sv6
      my_real, dimension(mvsiz) :: so1,so2,so3,so4,so5,so6
      my_real, dimension(mvsiz) :: r11,r12,r13
      my_real, dimension(mvsiz) :: r21,r22,r23
      my_real, dimension(mvsiz) :: r31,r32,r33
      my_real, dimension(mvsiz) :: sold1,sold2,sold3,sold4,sold5,sold6
      my_real, dimension(mvsiz) :: sspp
      my_real, dimension(mvsiz) :: sigy
      my_real, dimension(mvsiz) :: dpla
      my_real, dimension(mvsiz) :: epsp1
      my_real, dimension(mvsiz) :: tstar
      my_real, dimension(mvsiz) :: df
      my_real, dimension(mvsiz) :: amu
      my_real, dimension(mvsiz) :: bidv

      my_real :: e1,e2,e3,e4,e5,e6
      my_real :: dav,dta
      my_real :: ss1,ss2,ss3,ss4,ss5,ss6
      my_real :: q1,q2,q3
      my_real :: asrate,epsp
      my_real :: bid,dum1,bidon,bidon1,bidon2,bidon3,bidon4,bidon5
      my_real tt_local
      !
      my_real, dimension(:)  ,pointer :: sigp,siglp,strain,uvar,uvarf
      my_real, dimension(:)  ,pointer :: dfmax,tdele,uparam0,uparam,uparamf
      !
      integer, dimension(:), pointer :: vartmp,itabl_fail,iparam
      type(l_bufel_)  ,pointer :: lbuf
      !
      character option*256
      integer size
      integer :: nrate
      my_real :: fisokin
      my_real, dimension(nel), target :: vecnul
      my_real, dimension(:), pointer  :: sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      imat = mat(lft)
      inloc   = mat_param(imat)%nloc
      ieos    = mat_param(imat)%ieos
      nuparam = mat_param(imat)%nuparam
      niparam = mat_param(imat)%niparam
      uparam => mat_param(imat)%uparam
      iparam => mat_param(imat)%iparam
!
      npar    = ipm(9,imat)
      iadbuf  = ipm(7,imat)
      nfunc   = ipm(10,imat)
      nuvar   = bufly%nvar_mat
      nvartmp = bufly%nvartmp
      uparam0  => bufmat(iadbuf:iadbuf+npar-1)
      dmg_flag = bufly%l_dmgscl
!
      do j=1,6
         jj(j) = nel*(j-1)
      enddo
!
      do i=1,nfunc
         ifunc(i)=ipm(10+i,imat)
      enddo
      if (ismstr>=10) ismstr=0
      bidon = zero
      bidon1 = zero
      bidon2 = zero
      bidon3 = zero
      bidon4 = zero
      bidon5 = zero
      cst1(lft:llt) = one

      do i=lft,llt
         c1(i)  = pm(32,imat)
         rho0(i)= pm( 1,imat)
         vis(i) = zero
         ssp(i) = zero
         sv1(i) = zero
         sv2(i) = zero
         sv3(i) = zero
         sv4(i) = zero
         sv5(i) = zero
         sv6(i) = zero
         s1(i) = zero
         s2(i) = zero
         s3(i) = zero
         s4(i) = zero
         s5(i) = zero
         s6(i) = zero
         sig(i,1)=zero
         sig(i,2)=zero
         sig(i,3)=zero
         sig(i,4)=zero
         sig(i,5)=zero
         sig(i,6)=zero
         sspp(i)=zero
      enddo
!
      if (isorth /= 0) then
         call mreploc(&
         &gama,    r11,     r12,     r13,&
         &r21,     r22,     r23,     r31,&
         &r32,     r33,     rx,      ry,&
         &rz,      sx,      sy,      sz,&
         &tx,      ty,      tz,      nel,&
         &jsph)
      endif
!-------------------
!     mean strain rate
!-------------------
      if (mtn == 35.or.mtn == 36.or.mtn == 44.or.mtn == 48.or.mtn == 60.or.&
      &mtn == 52) then
         do i=1,llt
            ep1(i) = zero
            ep2(i) = zero
            ep3(i) = zero
            ep4(i) = zero
            ep5(i) = zero
            ep6(i) = zero
         enddo
!
         do ipt=1,npt
            do i=1,llt
               ep1(i) = ep1(i) + d1(i,ipt)
               ep2(i) = ep2(i) + d2(i,ipt)
               ep3(i) = ep3(i) + d3(i,ipt)
               ep4(i) = ep4(i) + d4(i,ipt)
               ep5(i) = ep5(i) + d5(i,ipt)
               ep6(i) = ep6(i) + d6(i,ipt)
            enddo
         enddo
!
         israte = ipm(3,mat(lft))
         if(israte>=1)then
            do i=1,llt
               dav = (ep1(i)+ep2(i)+ep3(i))/twenty4
               e1 = ep1(i)*one_over_8 - dav
               e2 = ep2(i)*one_over_8 - dav
               e3 = ep3(i)*one_over_8 - dav
               e4 = ep4(i)*one_over_16
               e5 = ep5(i)*one_over_16
               e6 = ep6(i)*one_over_16
               epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
               epsp = sqrt(three*epsp)*two_third
               asrate = min(one, pm(9,imat)*dt1)
               epsp = asrate*epsp + (one -asrate)*epsd(i)
               epsd(i)=epsp
            enddo
         endif
      endif
!--------------------------------------------------
!     boucle sur les points de gauss
!--------------------------------------------------
      do ipt=1,npt
         lbuf   => bufly%lbuf(1,1,ipt)
         sigp   => bufly%lbuf(1,1,ipt)%sig(1:llt*6)
         if(isorth /= 0) siglp  => bufly%lbuf(1,1,ipt)%sigl(1:llt*6)
         strain => bufly%lbuf(1,1,ipt)%stra(1:llt*6)
         uvar   => bufly%mat(1,1,ipt)%var(1:llt*nuvar)
         vartmp => bufly%mat(1,1,ipt)%vartmp(1:llt*nvartmp)
         jpt=(ipt-1)*llt
!
         do i=lft,llt
            ep1(i) = d1(i,ipt)
            ep2(i) = d2(i,ipt)
            ep3(i) = d3(i,ipt)
            ep4(i) = d4(i,ipt)
            ep5(i) = d5(i,ipt)
            ep6(i) = d6(i,ipt)
            dav=1.-dvol(i)/voln(i)
            sold1(i)=sigp(jj(1)+i)*dav
            sold2(i)=sigp(jj(2)+i)*dav
            sold3(i)=sigp(jj(3)+i)*dav
            sold4(i)=sigp(jj(4)+i)*dav
            sold5(i)=sigp(jj(5)+i)*dav
            sold6(i)=sigp(jj(6)+i)*dav
         enddo
!
         if (isorth /= 0) then
            do i=lft,llt
               ep4(i) = half*ep4(i)
               ep5(i) = half*ep5(i)
               ep6(i) = half*ep6(i)
            enddo
            call mrotens(lft,llt,ep1,ep2,ep3,ep4,ep5,ep6,&
            &r11,r12,r13,&
            &r21,r22,r23,&
            &r31,r32,r33)
            do i=lft,llt
               j = (i-1)*6
               ep4(i) = two*ep4(i)
               ep5(i) = two*ep5(i)
               ep6(i) = two*ep6(i)
               so1(i) = siglp(jj(1)+i)
               so2(i) = siglp(jj(2)+i)
               so3(i) = siglp(jj(3)+i)
               so4(i) = siglp(jj(4)+i)
               so5(i) = siglp(jj(5)+i)
               so6(i) = siglp(jj(6)+i)
            enddo
         else
            do i=lft,llt
               so1(i) = sigp(jj(1)+i)
               so2(i) = sigp(jj(2)+i)
               so3(i) = sigp(jj(3)+i)
               so4(i) = sigp(jj(4)+i)
               so5(i) = sigp(jj(5)+i)
               so6(i) = sigp(jj(6)+i)
!
               q1 =strain(jj(4)+i)*wzz(i,ipt)
               q2 =strain(jj(6)+i)*wyy(i,ipt)
               q3 =strain(jj(5)+i)*wxx(i,ipt)
               ss1=strain(jj(1)+i)-q1+q2
               ss2=strain(jj(2)+i)+q1-q3
               ss3=strain(jj(3)+i)-q2+q3
               ss4=strain(jj(4)+i)+&
               &two*wzz(i,ipt)*(strain(jj(1)+i)-strain(jj(2)+i))+&
               &wyy(i,ipt)*strain(jj(5)+i)-wxx(i,ipt)*strain(jj(6)+i)
               ss5=strain(jj(5)+i)+&
               &two*wxx(i,ipt)*(strain(jj(2)+i)-strain(jj(3)+i))+&
               &wzz(i,ipt)*strain(jj(6)+i)-wyy(i,ipt)*strain(jj(4)+i)
               ss6=strain(jj(6)+i)+&
               &two*wyy(i,ipt)*(strain(jj(3)+i)-strain(jj(1)+i))+&
               &wxx(i,ipt)*strain(jj(4)+i)-wzz(i,ipt)*strain(jj(5)+i)
               strain(jj(1)+i)= ss1
               strain(jj(2)+i)= ss2
               strain(jj(3)+i)= ss3
               strain(jj(4)+i)= ss4
               strain(jj(5)+i)= ss5
               strain(jj(6)+i)= ss6
!
            enddo
         endif
!
         do i=lft,llt
            de1(i) = ep1(i)*dt1
            de2(i) = ep2(i)*dt1
            de3(i) = ep3(i)*dt1
            de4(i) = ep4(i)*dt1
            de5(i) = ep5(i)*dt1
            de6(i) = ep6(i)*dt1
            strain(jj(1)+i)= strain(jj(1)+i) + de1(i)
            strain(jj(2)+i)= strain(jj(2)+i) + de2(i)
            strain(jj(3)+i)= strain(jj(3)+i) + de3(i)
            strain(jj(4)+i)= strain(jj(4)+i) + de4(i)
            strain(jj(5)+i)= strain(jj(5)+i) + de5(i)
            strain(jj(6)+i)= strain(jj(6)+i) + de6(i)
            es1(i) = strain(jj(1)+i)
            es2(i) = strain(jj(2)+i)
            es3(i) = strain(jj(3)+i)
            es4(i) = strain(jj(4)+i)
            es5(i) = strain(jj(5)+i)
            es6(i) = strain(jj(6)+i)
         enddo
!------compute of amu as in mmain after thermal expansion computation ---------------
         do i=lft,llt
            df(i)  =  rho0(i)/rho(i)
         enddo
         if(mtn == 45) then     ! for compatibility with qa tests
            do i=lft,llt
               amu(i) =  one/df(i)-one
            enddo
         else
            do i=lft,llt
               amu(i) =  rho(i)/rho0(i)-one
            enddo
         endif
         iexpan8 = 0
!
!--------------------------------------------------------
!     compute undamaged effective stresses
!---------------------------------------------------------
         if (dmg_flag > 0) then
            do i = lft,llt
               so1(i) = so1(i)/max(lbuf%dmgscl(i),em20)
               so2(i) = so2(i)/max(lbuf%dmgscl(i),em20)
               so3(i) = so3(i)/max(lbuf%dmgscl(i),em20)
               so4(i) = so4(i)/max(lbuf%dmgscl(i),em20)
               so5(i) = so5(i)/max(lbuf%dmgscl(i),em20)
               so6(i) = so6(i)/max(lbuf%dmgscl(i),em20)
            enddo
         endif
!---------------------------------------------------------------------------------
         if(mtn == 28)then
            idum1 = 0
            call sigeps28(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,amu      )

         elseif(mtn == 29)then
            ! ----------------
            ! error to be printed & exit
            option='/mat/law29 - solid '
            size=len_trim(option)
            call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
            call arret(2)
            ! ----------------
!!!
         elseif(mtn == 30)then
            ! ----------------
            ! error to be printed & exit
            option='/mat/law30 - solid '
            size=len_trim(option)
            call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
            call arret(2)
            ! ----------------
!!!
         elseif(mtn == 31)then
            ! ----------------
            ! error to be printed & exit
            option='/mat/law31 - solid '
            size=len_trim(option)
            call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
            call arret(2)
            ! ----------------
!!!
         elseif(mtn == 33)then
            call sigeps33(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      )
         elseif(mtn == 34)then
            call sigeps34(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      )
         elseif(mtn == 35)then
            call sigeps35(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,israte   ,asrate,&
            &epsd     )
         elseif(mtn == 36)then
!-------------------
!     strain rate
!-------------------
            do i=1,llt
               israte = ipm(3,mat(lft))
               if(israte == 0)then
                  dav = (ep1(i)+ep2(i)+ep3(i))*third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp = sqrt(three*epsp)*two_third
                  epsd(i)=epsp
               endif
            enddo

            nrate = nint(uparam0(1))
            fisokin = uparam0(6+2*nrate+8)
            if(fisokin>0) then
               sigbxx => lbuf%sigb(1      :  nel)
               sigbyy => lbuf%sigb(nel+1  :2*nel)
               sigbzz => lbuf%sigb(2*nel+1:3*nel)
               sigbxy => lbuf%sigb(3*nel+1:4*nel)
               sigbyz => lbuf%sigb(4*nel+1:5*nel)
               sigbzx => lbuf%sigb(5*nel+1:6*nel)
            else
               vecnul(1:nel) = zero
               sigbxx => vecnul(1:nel)
               sigbyy => vecnul(1:nel)
               sigbzz => vecnul(1:nel)
               sigbxy => vecnul(1:nel)
               sigbyz => vecnul(1:nel)
               sigbzx => vecnul(1:nel)
            endif

            call sigeps36(&
            &llt      ,nuvar    ,nfunc    ,ifunc    ,npf ,&
            &tf       ,tt       ,dt1      ,uparam0   ,rho0,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ieos  ,&
            &ipm      ,mat      ,epsd     ,ipla     ,sigy     ,lbuf%pla,&
            &dpla     ,et       ,bidon    ,bidon    ,amu      ,bidv      ,&
            &cst1     ,nvartmp  ,vartmp   ,lbuf%dmg ,inloc    ,lbuf%planl,&
            &sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx )
!
            defp(1:llt)   =  lbuf%pla(1:llt)
!
         elseif(mtn == 38)then
            call sigeps38(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,&
            &ismstr   ,bidon    ,bidon    ,bidon    ,bidon    ,&
            &bidon    ,bidon    ,bidon    ,bidon    ,bidon    ,bidon ,&
            &ihet     ,offg     ,epsd     )
         elseif(mtn == 40)then
            call sigeps40(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      )
         elseif(mtn == 41)then
            call sigeps41(&
            &llt      ,npar     ,nuvar    ,&
            &tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &so1      ,so2      ,so3      ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,dvol     ,lbuf%bfrac,&
            &lbuf%temp)
         elseif(mtn == 42)then
            call sigeps42(&
            &llt     ,nuparam ,nuvar   ,nfunc   ,ifunc   ,npf     ,&
            &tf      ,tt      ,dt1     ,uparam  ,rho0    ,rho     ,&
            &voln    ,eint    ,uvar    ,off     ,offg    ,sspp    ,&
            &ep1     ,ep2     ,ep3     ,ep4     ,ep5     ,ep6     ,&
            &es1     ,es2     ,es3     ,es4     ,es5     ,es6     ,&
            &s1      ,s2      ,s3      ,s4      ,s5      ,s6      ,&
            &bidon   ,bidon   ,bidon   ,bidon   ,bidon   ,bidon   ,&
            &bidon   ,bidon   ,bidon   ,vis     ,ismstr  ,et      ,&
            &ihet    ,bidon   ,iexpan8 ,niparam ,iparam  )
!
         elseif(mtn == 44)then
!
!---  strain rate
!
!         do i=1,llt
!           israte = ipm(3,mat(lft))
!           if(israte == 0)then
!             dav = (ep1(i)+ep2(i)+ep3(i)) * third
!             e1 = ep1(i) - dav
!             e2 = ep2(i) - dav
!             e3 = ep3(i) - dav
!             e4 = half*ep4(i)
!             e5 = half*ep5(i)
!             e6 = half*ep6(i)
!             epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
!             epsp = sqrt(three*epsp)*two_third
!             epsd(i)=epsp
!           endif
!         enddo
            call sigeps44(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,npf      ,&
            &tf       ,tt       ,dt1      ,uparam0   ,rho0     ,rho      ,&
            &voln     ,eint     ,ieos     ,bidv     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsd     ,ipla     ,sigy     ,lbuf%pla  ,&
            &dpla     ,amu      ,israte   ,asrate   ,nvartmp  ,vartmp,&
            &et       )
            defp(1:llt)   =  lbuf%pla(1:llt)
         elseif(mtn == 45)then
            call sigeps45(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,sigy     ,defp  ,&
            &amu      )
         elseif(mtn == 48)then
!---    strain rate
            do i=1,llt
               israte = ipm(3,mat(lft))
               if(israte == 0)then
                  dav = (ep1(i)+ep2(i)+ep3(i)) * third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp = sqrt(three*epsp)*two_third
                  epsd(i)=epsp
               endif
            enddo
            call sigeps48(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsd     ,sigy     ,defp     ,dpla  ,&
            &amu      )
         elseif(mtn == 50)then
            call sigeps50(llt ,npar,nuvar,nfunc,ifunc,&
            &npf ,tf  ,tt,dt1,bufmat(iadbuf),&
            &rho0,rho ,voln,eint,nvartmp ,vartmp  ,&
            &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
            &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
            &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
            &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
            &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
            &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
            &sspp ,vis ,uvar,off ,amu,mat_param)
         elseif(mtn == 52)then
!---    strain rate
            do i=1,llt
               israte = ipm(3,mat(lft))
               if(israte == 0)then
                  dav = (ep1(i)+ep2(i)+ep3(i)) * third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp = sqrt(three*epsp)*two_third
                  epsd(i)=epsp
               endif
            enddo
            call sigeps52(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsd     ,ipla     ,sigy     ,defp  ,&
            &table )
         elseif(mtn == 53)then
            call sigeps53(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsp     ,ipla     ,lbuf%seq )
         elseif(mtn == 56)then
!-------------------
!     strain rate
!-------------------
            do i=1,llt
               israte = ipm(3,mat(lft))
               if(israte == 0)then
                  dav = (ep1(i)+ep2(i)+ep3(i))*third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp = sqrt(three*epsp)*two_third
                  epsd(i)=epsp
               endif
            enddo

            call sigeps56(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsd     ,ipla     ,sigy     ,defp  ,&
            &dpla     ,amu      )
         elseif(mtn == 60)then
!-------------------
!     strain rate
!-------------------
            do i=1,llt
               israte = ipm(3,mat(lft))
               if(israte == 0)then
                  dav = (ep1(i)+ep2(i)+ep3(i))*third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp = sqrt(three*epsp)*two_third
                  epsd(i)=epsp
               endif
            enddo
            call sigeps60(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat,&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off      ,ngl      ,ipt   ,&
            &ipm      ,mat      ,epsd     ,ipla     ,sigy     ,defp ,&
            &dpla     ,amu )
         elseif(mtn == 62)then
            call sigeps62(&
            &llt      ,npar     ,nuvar    ,nfunc    ,ifunc    ,&
            &npf      ,tf       ,tt       ,dt1      ,bufmat(iadbuf),&
            &rho0     ,rho      ,voln     ,eint     ,&
            &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6   ,&
            &de1      ,de2      ,de3      ,de4      ,de5      ,de6   ,&
            &es1      ,es2      ,es3      ,es4      ,es5      ,es6   ,&
            &so1      ,so2      ,so3      ,so4      ,so5      ,so6   ,&
            &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
            &sv1      ,sv2      ,sv3      ,sv4      ,sv5      ,sv6   ,&
            &sspp     ,vis      ,uvar     ,off, ismstr,et     ,ihet  ,&
            &offg     ,bidon    ,iexpan8  )
         endif
!-----------------------------------------------------------------
!                failure model for use law
!  ---------------------------------------------------------------
         if ((itask==0).and.(imon_mat==1))call startime(121,1)
         do ir = 1,bufly%nfail
            nvarf =  bufly%fail(1,1,ipt)%floc(ir)%nvar
            uvarf => bufly%fail(1,1,ipt)%floc(ir)%var
            dfmax => bufly%fail(1,1,ipt)%floc(ir)%dammx
            tdele => bufly%fail(1,1,ipt)%floc(ir)%tdel
!
            npar    = mat_param(imat)%fail(ir)%nuparam
            uparamf =>mat_param(imat)%fail(ir)%uparam(1:npar)
            nfunc   = mat_param(imat)%fail(ir)%nfunc
            irupt   = mat_param(imat)%fail(ir)%irupt
            ifunc(1:nfunc) = mat_param(imat)%fail(ir)%ifunc(1:nfunc)
            ntabl_fail =  mat_param(imat)%fail(ir)%ntable
            itabl_fail => mat_param(imat)%fail(ir)%table(1:ntabl_fail)
!
            if(mtn == 36.or.mtn == 44.or.mtn == 48.or.mtn == 56.&
            &or.mtn == 60)then
               do i=lft,llt
                  tstar(i) = zero
                  epsp1(i) = epsd(i)
               enddo
            else
               do i=lft,llt
                  tstar(i) = zero
                  dav = (ep1(i)+ep2(i)+ep3(i))*third
                  e1 = ep1(i) - dav
                  e2 = ep2(i) - dav
                  e3 = ep3(i) - dav
                  e4 = half*ep4(i)
                  e5 = half*ep5(i)
                  e6 = half*ep6(i)
                  epsp1(i) =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
                  epsp1(i) = sqrt(three*epsp1(i))*two_third
               enddo
            endif
!
            if (irupt == 1)then
!----  johnson cook
               call fail_johnson(llt ,npar,nvarf,&
               &tt  ,dt1  ,uparamf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &dpla,epsp1,tstar,uvarf,off,&
               &dfmax,tdele)
            elseif(irupt == 2) then
! -----      tuler butcher
               call fail_tbutcher_s(llt ,npar,nvarf,&
               &tt  ,dt1  ,uparamf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &uvarf    ,off,dfmax,tdele)
            elseif(irupt == 3) then
! --- wilkins
               call fail_wilkins_s(llt ,npar,nvarf,&
               &tt  ,dt1  ,uparamf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &dpla,uvarf,off,dfmax,tdele)
            elseif(irupt == 4)then
!  ----user1
               tt_local = tt
!               call f04law(llt ,npar,nvarf,nfunc,ifunc,
!     2                    npf ,tf  ,tt_local,dt1  ,bufmat,
!     3                    ngl ,ipm ,npropmi,mat ,idel7nok,
!     4                    ep1 ,ep2 ,ep3 ,ep4 ,ep5  ,ep6   ,
!     5                    es1 ,es2 ,es3 ,es4 ,es5  ,es6   ,
!     6                    s1  ,s2  ,s3  ,s4  ,s5   ,s6    ,
!     7                    defp,dpla,epsp1,uvarf     ,off,
!     8                    deltax,voln,bidon3,bidon4,bidon5)
!!!
               ! ----------------
               ! error to be printed & exit
               option='/fail/user1 - solid '
               size=len_trim(option)
               call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
               call arret(2)
               ! ----------------
!!!
            elseif(irupt == 5)then
!  ----user2
               tt_local = tt
!               call f05law(llt ,npar,nvarf,nfunc,ifunc,
!     2                    npf ,tf  ,tt_local  ,dt1  ,bufmat,
!     3                    ngl ,ipm ,npropmi,mat ,idel7nok,
!     4                    ep1 ,ep2 ,ep3 ,ep4 ,ep5  ,ep6   ,
!     5                    es1 ,es2 ,es3 ,es4 ,es5  ,es6   ,
!     6                    s1  ,s2  ,s3  ,s4  ,s5   ,s6    ,
!     7                    defp,dpla,epsp1,uvarf     ,off,
!     8                    deltax,voln,bidon3,bidon4,bidon5)
!!!
               ! ----------------
               ! error to be printed & exit
               option='/fail/user2 - solid '
               size=len_trim(option)
               call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
               call arret(2)
               ! ----------------
!!!
            elseif(irupt == 6)then
!  ----user3
               tt_local = tt
!              call f06law(llt ,npar,nvarf,nfunc,ifunc,
!     2                    npf ,tf  ,tt_local  ,dt1  ,bufmat,
!     3                    ngl ,ipm ,npropmi,mat ,idel7nok,
!     4                    ep1 ,ep2 ,ep3 ,ep4 ,ep5  ,ep6   ,
!     5                    es1 ,es2 ,es3 ,es4 ,es5  ,es6   ,
!     6                    s1  ,s2  ,s3  ,s4  ,s5   ,s6    ,
!     7                    defp,dpla,epsp1,uvarf     ,off,
!     8                    deltax,voln,bidon3,bidon4,bidon5)
!!!
               ! ----------------
               ! error to be printed & exit
               option='/fail/user3 - solid '
               size=len_trim(option)
               call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
               call arret(2)
               ! ----------------
!!!
            elseif(irupt == 8)then
!----  j     ohnson cook + spalling
               call fail_spalling_s(llt ,npar,nvarf,&
               &tt  ,dt1  ,uparamf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6      ,&
               &dpla,epsp1,tstar,uvarf    ,off     ,&
               &dfmax,tdele,offg)
            elseif(irupt == 9)then
!----  wierzbicki
               call fail_wierzbicki_s(llt ,npar,nvarf,&
               &tt  ,dt1  ,uparamf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6      ,&
               &dpla,defp,uvarf,off ,dfmax,&
               &tdele)
            elseif(irupt == 10)then
!----strain tension
               call fail_tensstrain_s(llt ,npar,nvarf,nfunc,ifunc        ,&
               &npf ,tf  ,tt  ,dt1  ,uparamf,&
               &ngl ,deltax  ,tstar,ismstr,&
               &es1 ,es2 ,es3 ,es4  ,es5  ,es6     ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6      ,&
               &epsp1,uvarf   ,off  ,dfmax   ,tdele,&
               &bidon   ,bidon   ,bidon   ,bidon    ,bidon   ,bidon   ,&
               &bidon   ,bidon   ,bidon   ,lbuf%dmgscl)
!
!----        energy failure
            elseif(irupt == 11)then
               call fail_energy_s(&
               &llt      ,npar     ,nvarf    ,nfunc    ,ifunc    ,npf      ,&
               &tf       ,tt       ,dt1      ,uparamf,ngl ,epsp1    ,&
               &uvarf    ,off      ,dfmax    ,tdele    ,lbuf%dmgscl,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &de1      ,de2      ,de3      ,de4      ,de5      ,de6      )
            elseif (irupt == 23) then
!---- tabulated failure model
               call fail_tab_s(&
               &llt      ,nvarf    ,npf      ,tf       ,tt       ,&
               &uparamf     ,ngl      ,deltax   ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6,&
               &dpla     ,epsp1    ,tstar    ,uvarf    ,ntabl_fail,itabl_fail,&
               &off      ,table    ,dfmax    ,tdele    ,nfunc    ,ifunc )
!---
            elseif (irupt == 27) then
!----  extended mohr coulomb failure model
               call fail_emc(&
               &llt      ,nvarf    ,tt       ,&
               &dt1      ,uparamf  ,ngl      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6,&
               &defp     ,dpla     ,epsp1    ,uvarf    ,&
               &off      ,dfmax    ,tdele    )
            elseif (irupt == 29) then
! ---   mit wierzbicki sahraei electric battery failure
               call fail_sahraei_s(&
               &llt      ,nfunc    ,ifunc    ,npf      ,tf       ,&
               &tt       ,ngl      ,uparamf,&
               &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
               &off      ,dfmax    ,tdele    ,deltax   ,&
               &nvarf    ,uvarf    )
            elseif (irupt == 30) then
!  --- biquadratic failure model
               call fail_biquad_s(&
               &llt      ,npar     ,nvarf    ,nfunc    ,ifunc    ,deltax   ,&
               &npf      ,tf       ,tt       ,bufmat   ,tdele    ,&
               &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,lbuf%dmgscl,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       )
            elseif (irupt == 36) then
!  --- visual failure model
               call fail_visual_s(&
               &llt     ,npar     ,nvarf    ,tt       ,dt1       ,uparamf,&
               &es1     ,es2      ,es3      ,es4      ,es5       ,es6 ,&
               &s1      ,s2       ,s3       ,s4       ,s5        ,s6  ,&
               &uvarf   ,off      ,ngl      ,dfmax    ,ismstr    )
!
            elseif (irupt == 37) then
! ---       tabulated failure model (old, obsolete version)
               call fail_tab_old_s(&
               &llt      ,nvarf    ,npf      ,tf       ,tt       ,&
               &uparamf  ,ngl      ,deltax   ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6,&
               &defp     ,dpla     ,epsp1    ,tstar    ,uvarf    ,&
               &off      ,dfmax    ,tdele    ,&
               &nfunc    ,ifunc )
!
            elseif (irupt == 38) then
!  --- orthotropic biquadratic failure model
               call fail_orthbiquad_s(&
               &llt      ,npar     ,nvarf    ,nfunc    ,ifunc    ,&
               &npf      ,tf       ,tt       ,dt1      ,uparamf,&
               &ngl      ,dpla     ,epsp1    ,uvarf    ,off      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &dfmax    ,tdele    ,deltax   )
!
            elseif (irupt == 40) then
!  --- rtcl failure model
               call fail_rtcl_s(&
               &llt      ,npar     ,nvarf    ,tt       ,dt1      ,uparamf,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,tdele    )
!---------
            endif ! irupt
!---------
         enddo ! several failur model boucle ir
!---------
!--------------------------------------------------------
!     damaged stresses
!---------------------------------------------------------
         if (dmg_flag > 0) then
            do i = lft,llt
               s1(i) = s1(i)*lbuf%dmgscl(i)
               s2(i) = s2(i)*lbuf%dmgscl(i)
               s3(i) = s3(i)*lbuf%dmgscl(i)
               s4(i) = s4(i)*lbuf%dmgscl(i)
               s5(i) = s5(i)*lbuf%dmgscl(i)
               s6(i) = s6(i)*lbuf%dmgscl(i)
            enddo
         endif
!---------------------------------------------------------
         if ((itask==0).and.(imon_mat==1))call stoptime(121,1)
!----------
         if (isorth /= 0) then
            do i=lft,llt
               siglp(jj(1)+i) = s1(i)
               siglp(jj(2)+i) = s2(i)
               siglp(jj(3)+i) = s3(i)
               siglp(jj(4)+i) = s4(i)
               siglp(jj(5)+i) = s5(i)
               siglp(jj(6)+i) = s6(i)
            enddo
            call mrotens(lft,llt,&
            &s1 ,s2 ,s3 ,&
            &s4 ,s5 ,s6 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
            call mrotens(lft,llt,&
            &sv1 ,sv2 ,sv3 ,&
            &sv4 ,sv5 ,sv6 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
         endif
         do i=lft,llt
            sigp(jj(1)+i) = s1(i)
            sigp(jj(2)+i) = s2(i)
            sigp(jj(3)+i) = s3(i)
            sigp(jj(4)+i) = s4(i)
            sigp(jj(5)+i) = s5(i)
            sigp(jj(6)+i) = s6(i)
            sig(i,1)= sig(i,1) + one_over_8*s1(i)
            sig(i,2)= sig(i,2) + one_over_8*s2(i)
            sig(i,3)= sig(i,3) + one_over_8*s3(i)
            sig(i,4)= sig(i,4) + one_over_8*s4(i)
            sig(i,5)= sig(i,5) + one_over_8*s5(i)
            sig(i,6)= sig(i,6) + one_over_8*s6(i)
            svis(i,1)= svis(i,1) + one_over_8*sv1(i)
            svis(i,2)= svis(i,2) + one_over_8*sv2(i)
            svis(i,3)= svis(i,3) + one_over_8*sv3(i)
            svis(i,4)= svis(i,4) + one_over_8*sv4(i)
            svis(i,5)= svis(i,5) + one_over_8*sv5(i)
            svis(i,6)= svis(i,6) + one_over_8*sv6(i)
            ssp(i) = max(ssp(i),sspp(i))
         enddo

         dta =half*dt1
         do i=lft,llt
            dav=volgp(i,ipt)*off(i)*dta
            eint(i)=eint(i)+dav*(d1(i,ipt)*(sold1(i)+sigp(jj(1)+i))+&
            &d2(i,ipt)*(sold2(i)+sigp(jj(2)+i))+&
            &d3(i,ipt)*(sold3(i)+sigp(jj(3)+i))+&
            &d4(i,ipt)*(sold4(i)+sigp(jj(4)+i))+&
            &d5(i,ipt)*(sold5(i)+sigp(jj(5)+i))+&
            &d6(i,ipt)*(sold6(i)+sigp(jj(6)+i)))
         enddo
!
      enddo  !  ipt=1,npt
!--------------------------------------------------
!     egalisation de la pression
!--------------------------------------------------
      do i=lft,llt
         pnew(i) = -(sig(i,1) + sig(i,2) + sig(i,3)) * third
      enddo
!----
      do ipt=1,npt
         sigp => bufly%lbuf(1,1,ipt)%sig(1:llt*6)
         do i=lft,llt
            pp(i)=pnew(i) + (sigp(jj(1)+i) + sigp(jj(2)+i) + sigp(jj(3)+i)) * third
            sigp(jj(1)+i) =(sigp(jj(1)+i)-pp(i))*off(i)
            sigp(jj(2)+i) =(sigp(jj(2)+i)-pp(i))*off(i)
            sigp(jj(3)+i) =(sigp(jj(3)+i)-pp(i))*off(i)
            sigp(jj(4)+i) = sigp(jj(4)+i)       *off(i)
            sigp(jj(5)+i) = sigp(jj(5)+i)       *off(i)
            sigp(jj(6)+i) = sigp(jj(6)+i)       *off(i)
            dav = volgp(i,ipt)*off(i)*dta
            eint(i)=eint(i)-dav*pp(i)*(d1(i,ipt)+d2(i,ipt)+d3(i,ipt))
         enddo
      enddo
!------------------------------------------------------------
!     define sound speed  (in all case)
!     define dynamic viscosity (for viscous law)
!-----------------------
      do i=lft,llt
         if(ssp(i) == zero) ssp(i)=sqrt(c1(i)/rho0(i))
      enddo
!-------------------------------------------
!   bulk viscosity and time step computation
!   this subroutine return the new bulk viscosity q
!-----------------------
      call mqvisc8(&
      &pm,      off,     rho,     vis,&
      &vis,     vis,     stifn,   eint,&
      &d1,      d2,      d3,      voln,&
      &dvol,    vd2,     deltax,  vis,&
      &qold,    ssp,     mat,     nc,&
      &ngl,     geo,     pid,     dt2t,&
      &neltst,  ityptst, offg,    mssa,&
      &dmels,   nel,     ity,     jtur,&
      &jthe,    jsms)
!
      do i=lft,llt
         eint(i)=eint(i)/max(em15,vol(i))
      enddo
!------------------------------------------
      return
   end
!-----
end module mulaw8_mod
