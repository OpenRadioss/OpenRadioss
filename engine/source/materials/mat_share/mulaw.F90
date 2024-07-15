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
module mulaw_mod
contains
! ======================================================================================================================
!                                                   mmain
! ======================================================================================================================
!! \brief main routine for advanced Material Computation for brick/quad/thickshell/sph elements 
subroutine mulaw(&
   &nft,         mtn,         jcvt,        pm,&
   &off,         sig,         eint,        rho,&
   &qold,        vol,         strain,      sigl,&
   &gama,        uvar,        bufmat,      tf,&
   &npf,         stifn,       mat,         ngl,&
   &nuvar,       dt2t,        neltst,      ityptst,&
   &offg,        geo,         pid,         epsd,&
   &ltemp,       wxx,         wyy,         wzz,&
   &jsph,        mumax,       ssp,         aire,&
   &voln,        vd2,         deltax,      vis,&
   &d1,          d2,          d3,          d4,&
   &d5,          d6,          pnew,        psh,&
   &q,           ssp_eq,      dvol,        sold1,&
   &sold2,       sold3,       sold4,       sold5,&
   &sold6,       rx,          ry,          rz,&
   &sx,          sy,          sz,          tx,&
   &ty,          tz,          ipla,        sigy,&
   &defp,        ismstr,      mfxx,        mfxy,&
   &mfxz,        mfyx,        mfyy,        mfyz,&
   &mfzx,        mfzy,        mfzz,        ipm,&
   &isorth,      fbuf,        nfail,       npg,&
   &sigdd,       dxy,         dyx,         dyz,&
   &dzy,         dzx,         dxz,         fr_wav,&
   &v,           varnl,       w,           ix,&
   &x,           jthe,        et,          mssa,&
   &dmels,       iptr,        ipts,        iptt,&
   &table,       fvd2,        fdeltax,     fssp,&
   &fqvis,       tempel,      igeo,        sigv,&
   &al_imp,      signor,      istrain,     ng,&
   &elbuf_tab,   vbuf,        ilay,        vk,&
   &ale_connect, iparg,       bufvois,     vdx,&
   &vdy,         vdz,         ihet,        conde,&
   &itask,       iexpan,      vol_avg,     amu,&
   &epsth3,      epsth,       svisc,       nel,&
   &etotsh,      iselect,     tstar,       muold,&
   &dpdm,        rhoref,      rhosp,       nvartmp,&
   &vartmp,      eintth,      mat_elem,    nloc_dmg,&
   &ity,         jtur,        jsms,        idel7nok,&
   &sz_bufvois,  sz_ix,       snpc,        stf,&
   &sbufmat,     svis,        n2d,         ngroup,&
   &imon_mat,    numnod,      numels,      ntable,&
   &numgeo,      nummat,      numelq,      idtmin,&
   &dt1,         tt,          &
   &impl_s,&
   &idyna,       userl_avail, nixs,        nixq)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod
      use table_mod
      use mat_elem_mod
      use ale_connectivity_mod
      use message_mod
      use nlocal_reg_mod
      use sigeps100_mod
      use sigeps126_mod
      use prop_param_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type (buf_fail_),intent(inout), target                        :: fbuf
      type (buf_visc_),intent(inout)                                :: vbuf
      type (ttable), dimension(ntable) ,intent(in)                  :: table
      type (elbuf_struct_), target, dimension(ngroup),intent(inout) :: elbuf_tab
      type(t_ale_connectivity), intent(in)                          :: ale_connect
      type (mat_elem_) ,intent(inout) ,target                       :: mat_elem
      type (nlocal_str_),intent(inout)                              :: nloc_dmg

      integer,intent(in) :: nixs
      integer,intent(in) :: nixq
      integer,intent(in) :: userl_avail
      integer,intent(in) :: impl_s
      integer,intent(in) :: idyna
      integer,intent(in) :: numgeo
      integer,intent(in) :: nummat
      integer,intent(in) :: numelq
      integer,intent(in) :: numnod
      integer,intent(in) :: numels
      integer,intent(in) :: ntable
      integer,intent(in) :: imon_mat
      integer,intent(in) :: n2d
      integer,intent(in) :: ngroup
      integer,intent(in) :: stf
      integer,intent(in) :: snpc
      integer,intent(in) :: sz_ix
      integer,intent(in) :: ity
      integer,intent(in) :: jtur
      integer,intent(in) :: jsms
      integer,intent(inout) :: idel7nok
      integer,intent(in) :: nft
      integer,intent(in) :: mtn
      integer,intent(in) :: nuvar
      integer,intent(in) :: jcvt
      integer,intent(in) :: isorth
      integer,intent(in) :: nfail
      integer,intent(in) :: npg
      integer,intent(in) :: nvartmp
      integer,intent(in) :: jsph
      integer,intent(in) :: ipla
      integer,intent(in) :: ismstr
      integer,intent(in) :: neltst
      integer,intent(in) :: ityptst
      integer,intent(in) :: ng
      integer,intent(in) :: ilay
      integer,intent(in) :: jthe
      integer,intent(in) :: istrain
      integer,intent(in) :: iptr
      integer,intent(in) :: ipts
      integer,intent(in) :: iptt
      integer,intent(in) :: ihet
      integer,intent(in) :: iexpan
      integer,intent(in) :: nel
      integer,intent(in) :: itask
      integer,intent(in) :: iselect
      integer,intent(in) :: sz_bufvois
      integer,intent(in) :: sbufmat
      integer,dimension(102) :: idtmin
      integer, dimension(nvartmp*nel),intent(inout) :: vartmp
      integer,dimension(mvsiz),intent(in)           :: ngl
      integer,dimension(mvsiz),intent(in)           :: mat
      integer,dimension(mvsiz),intent(inout)        :: pid
      integer,dimension(n_var_iparg,ngroup),intent(in)    :: iparg
      integer,dimension(n_var_igeo,numgeo),intent(in)  :: igeo
      integer,dimension(n_var_ipm,nummat),intent(in)  :: ipm
      target  :: ipm
      integer,dimension(nixs,sz_ix),intent(in) :: ix
      integer,dimension(snpc),intent(in) ::  npf

      my_real,dimension(sbufmat),target,intent(inout) ::bufmat
      my_real,intent(in) :: dt2t
      my_real,intent(in) :: dt1
      my_real,intent(in) :: tt
      my_real, dimension(stf),intent(in) :: tf
      my_real, dimension(sz_bufvois),intent(inout) :: bufvois
      my_real, dimension(n_var_geo,numgeo), intent(inout) :: geo
      my_real, dimension(n_var_pm,nummat), intent(inout) :: pm
      my_real, dimension(3,numnod), intent(in) :: v
      my_real, dimension(3,numnod), intent(in) :: x
      my_real, dimension(3,numnod), intent(in) :: w
      my_real, dimension(numels),intent(inout) :: mssa
      my_real, dimension(numels),intent(inout) :: dmels

      ! nel arrays / element buffer
      my_real, dimension(nel), intent(inout) :: psh
      my_real, dimension(nel), intent(inout) :: pnew
      my_real, dimension(nel), intent(inout) :: varnl
      target varnl
      my_real, dimension(nel,6), intent(inout) :: sig
      my_real, dimension(nel,6), intent(inout) :: sigl
      my_real, dimension(nel,6), intent(inout) :: strain
      my_real, dimension(nel), intent(inout) :: eint
      my_real, dimension(nel), intent(inout) :: rho
      my_real, dimension(nel), intent(inout) :: vol
      my_real, dimension(nel), intent(inout) :: vk
      my_real, dimension(nel), intent(inout) :: offg
      my_real, dimension(nel), intent(inout) :: epsd
      my_real, dimension(nel,6), intent(inout) :: sigv
      my_real, dimension(nel), intent(inout) :: defp
      target defp
      my_real, dimension(nel,6), intent(inout) :: sigdd
      my_real, dimension(nel,6), intent(inout) :: svisc
      my_real, dimension(nel,6), intent(inout) :: etotsh
      my_real, dimension(nel), intent(inout) :: muold
      my_real, dimension(nel), intent(inout) :: epsth
      my_real, dimension(nel), intent(inout) :: qold
      !
      my_real, target, dimension(nel),intent(inout) :: ltemp
      my_real, target, dimension(nel),intent(inout) :: tempel
      my_real, target, dimension(nuvar*nel)   :: uvar

      ! mvsiz arrays / working array
      my_real, dimension(mvsiz), intent(inout) :: off
      my_real, dimension(mvsiz,6), intent(inout) :: gama
      my_real, dimension(mvsiz), intent(inout) :: wxx
      my_real, dimension(mvsiz), intent(inout) :: wyy
      my_real, dimension(mvsiz), intent(inout) :: wzz
      my_real, dimension(mvsiz), intent(inout) :: mumax
      my_real, dimension(mvsiz), intent(inout) :: ssp
      my_real, dimension(mvsiz), intent(inout) :: aire
      my_real, dimension(mvsiz), intent(inout) :: voln
      my_real, dimension(mvsiz), intent(inout) :: vd2
      my_real, dimension(mvsiz), intent(inout) :: deltax
      target :: deltax
      my_real, dimension(mvsiz), intent(inout) :: vis
      my_real, dimension(mvsiz), intent(inout) :: sold1
      my_real, dimension(mvsiz), intent(inout) :: sold2
      my_real, dimension(mvsiz), intent(inout) :: sold3
      my_real, dimension(mvsiz), intent(inout) :: sold4
      my_real, dimension(mvsiz), intent(inout) :: sold5
      my_real, dimension(mvsiz), intent(inout) :: sold6
      my_real, dimension(mvsiz), intent(inout) :: d1
      my_real, dimension(mvsiz), intent(inout) :: d2
      my_real, dimension(mvsiz), intent(inout) :: d3
      my_real, dimension(mvsiz), intent(inout) :: d4
      my_real, dimension(mvsiz), intent(inout) :: d5
      my_real, dimension(mvsiz), intent(inout) :: d6
      my_real, dimension(mvsiz), intent(inout) :: q
      my_real, dimension(mvsiz), intent(inout) :: ssp_eq
      my_real, dimension(mvsiz), intent(inout) :: dvol
      my_real, dimension(mvsiz), intent(inout) :: rx
      my_real, dimension(mvsiz), intent(inout) :: ry
      my_real, dimension(mvsiz), intent(inout) :: rz
      my_real, dimension(mvsiz), intent(inout) :: sx
      my_real, dimension(mvsiz), intent(inout) :: sy
      my_real, dimension(mvsiz), intent(inout) :: sz
      my_real, dimension(mvsiz), intent(inout) :: tx
      my_real, dimension(mvsiz), intent(inout) :: ty
      my_real, dimension(mvsiz), intent(inout) :: tz
      my_real, dimension(mvsiz), intent(inout) :: mfxx
      my_real, dimension(mvsiz), intent(inout) :: mfxy
      my_real, dimension(mvsiz), intent(inout) :: mfxz
      my_real, dimension(mvsiz), intent(inout) :: mfyx
      my_real, dimension(mvsiz), intent(inout) :: mfyy
      my_real, dimension(mvsiz), intent(inout) :: mfyz
      my_real, dimension(mvsiz), intent(inout) :: mfzx
      my_real, dimension(mvsiz), intent(inout) :: mfzy
      my_real, dimension(mvsiz), intent(inout) :: mfzz
      my_real, dimension(mvsiz), intent(inout) :: fqvis
      my_real, dimension(mvsiz), intent(inout) :: fssp
      my_real, dimension(mvsiz), intent(inout) :: sigy
      my_real, dimension(mvsiz), intent(inout) :: dxy
      my_real, dimension(mvsiz), intent(inout) :: dyx
      my_real, dimension(mvsiz), intent(inout) :: dyz
      my_real, dimension(mvsiz), intent(inout) :: dzy
      my_real, dimension(mvsiz), intent(inout) :: dzx
      my_real, dimension(mvsiz), intent(inout) :: dxz
      my_real, dimension(mvsiz), intent(inout) :: fr_wav
      my_real, dimension(mvsiz), intent(inout) :: et
      my_real, dimension(mvsiz), intent(inout) :: fvd2
      my_real, dimension(mvsiz), intent(inout) :: fdeltax
      my_real, dimension(mvsiz), intent(inout) :: al_imp
      my_real, dimension(mvsiz,6), intent(inout) :: signor
      my_real, dimension(mvsiz), intent(inout) :: vdx
      my_real, dimension(mvsiz), intent(inout) :: vdy
      my_real, dimension(mvsiz), intent(inout) :: vdz
      my_real, dimension(mvsiz,6), intent(inout) :: eintth
      my_real, dimension(mvsiz), intent(inout) :: amu
      my_real, dimension(mvsiz), intent(inout) :: dpdm
      my_real, dimension(mvsiz), intent(inout) :: vol_avg
      my_real, dimension(mvsiz), intent(inout) :: epsth3
      my_real, dimension(mvsiz), intent(inout) :: tstar
      my_real, dimension(mvsiz), intent(inout) :: conde
      my_real, dimension(mvsiz), intent(inout) :: rhoref
      my_real, dimension(mvsiz), intent(inout) :: rhosp
      my_real, dimension(mvsiz), intent(inout) :: stifn
      my_real, dimension(mvsiz,6), intent(inout) :: svis
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real, dimension(nel),target :: uvarf1
      my_real, target, dimension(nel) :: scale1
      my_real , dimension(1) ,target :: vec0
      integer :: nuvarr

      integer nv46, numel, inloc
      integer i,npar,nparf,iadbuf,iadvis,nfunc,numtabl,israte,ipg,nptr,npts,&
      &ibid,ibidon1,ibidon2,ibidon3,ibidon4 ,n48,nix,ilaw_user,igtyp,&
      &nvarf,ir,irupt,imat,isvis,ivisc,nuvarv,nuparv,iseq,idev,ntabl_fail,&
      &l_planl,l_epsdnl,l_dmg

      my_real e1,e2,e3,e4,e5,e6,bid1,bid3,q1,q2,q3,ss1,ss2,ss3,ss4,ss5,&
      &ss6,wxxf,wyyf,wzzf,p2,epsp,dav,asrate,c1(mvsiz),&
      &ep1(mvsiz),ep2(mvsiz),ep3(mvsiz),e7(mvsiz),&
      &ep4(2*mvsiz),ep5(2*mvsiz),ep6(2*mvsiz),einc(mvsiz),&
      &s1(mvsiz) ,s2(mvsiz) ,s3(mvsiz) ,&
      &s4(2*mvsiz) ,s5(2*mvsiz) ,s6(2*mvsiz),&
      &de1(mvsiz),de2(mvsiz),de3(mvsiz),&
      &de4(2*mvsiz),de5(2*mvsiz),de6(2*mvsiz),&
      &so1(mvsiz),so2(mvsiz),so3(mvsiz),&
      &so4(2*mvsiz),so5(2*mvsiz),so6(2*mvsiz),&
      &sxy(mvsiz),syx(mvsiz),syz(mvsiz),szy(mvsiz),szx(mvsiz),sxz(mvsiz),&
      &es1(mvsiz),es2(mvsiz),es3(mvsiz),es4(mvsiz),es5(mvsiz),es6(mvsiz),&
      &sv1(mvsiz),sv2(mvsiz),sv3(mvsiz),sv4(mvsiz),sv5(mvsiz),sv6(mvsiz),&
      &r11(mvsiz),r12(mvsiz),r13(mvsiz),r21(mvsiz),r22(mvsiz),r23(mvsiz),&
      &r31(mvsiz),r32(mvsiz),r33(mvsiz),epsp1(mvsiz),dpla(mvsiz),&
      &pair(mvsiz),defp0(mvsiz)
      my_real  facq0
      my_real fpsxx(mvsiz),fpsyy(mvsiz),fpszz(mvsiz),fpsxy(mvsiz),&
      &fpsyz(mvsiz),fpszx(mvsiz),fpsyx(mvsiz),fpszy(mvsiz),&
      &fpsxz(mvsiz),&
      &upsxx(mvsiz),upsyy(mvsiz),upszz(mvsiz),upsxy(mvsiz),&
      &upsyz(mvsiz),upsxz(mvsiz),trepsth(mvsiz)
      my_real rho0(mvsiz),bidon1,bidon2,bidon3,bidon4,bidon5,pold,volg(mvsiz)
      my_real tt_local
      my_real, dimension(nel), target  :: le_max
!----
      my_real, dimension(:), pointer   :: uparam,uparf,uparvis,uvarf,dfmax,&
      &tdel,el_temp,yldfac,dam,el_len,&
      &el_pla,damini
      my_real, dimension(:), allocatable ,target  :: bufzero
      type(l_bufel_)  ,pointer         :: lbuf
      type(g_bufel_)  ,pointer         :: gbuf
      type(buf_eos_)  ,pointer         :: ebuf
      type(matparam_struct_) , pointer :: matparam
      logical :: logical_userl_avail
      my_real :: user_uelr(mvsiz)
      integer, dimension(:) ,pointer   :: fld_idx,foff,ifunc,itable,itabl_fail,iparf
      integer                          :: mat_comp,mat_smstr,mat_formu
      integer                          :: dmg_flag,lf_dammx,niparf
!
      character option*256
      integer size,nvareos,nvarvis
      integer :: nrate
      my_real :: fisokin
      my_real, dimension(nel), target :: vecnul
      my_real, dimension(:), pointer  :: sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx
!=======================================================================
      gbuf   => elbuf_tab(ng)%gbuf
      lbuf   => elbuf_tab(ng)%bufly(ilay)%lbuf(iptr,ipts,iptt)
      ebuf   => elbuf_tab(ng)%bufly(ilay)%eos(iptr,ipts,iptt)
      nvareos = elbuf_tab(ng)%bufly(ilay)%nvar_eos
      nvarvis = elbuf_tab(ng)%bufly(ilay)%nvar_visc
      logical_userl_avail=.false.
      if (userl_avail>0) logical_userl_avail=.true.
      nptr   = elbuf_tab(ng)%nptr
      npts   = elbuf_tab(ng)%npts
      ipg    = iptr + ((ipts-1) + (iptt-1)*npts)*nptr
      igtyp  = iparg(38,ng)
      if ((igtyp == 20).or.(igtyp == 21).or.(igtyp == 22)) then
         ipg  = iptr + ((ipts-1) + (ilay-1)*npts)*nptr
      endif
      iseq   = elbuf_tab(ng)%bufly(ilay)%l_seq
      inloc  = iparg(78,ng)
      dmg_flag = elbuf_tab(ng)%bufly(ilay)%l_dmgscl
      l_dmg    = elbuf_tab(ng)%bufly(ilay)%l_dmg
      l_planl  = elbuf_tab(ng)%bufly(ilay)%l_planl
      l_epsdnl = elbuf_tab(ng)%bufly(ilay)%l_epsdnl
      ! make sure that the non-local variable increment is positive
      if (inloc > 0) then
         do i = 1,nel
            if (off(i) == one) then
              varnl(i) = max(varnl(i),zero)
            else
              varnl(i) = zero
            endif
            lbuf%planl(i)  = lbuf%planl(i) + varnl(i)
            lbuf%epsdnl(i) = varnl(i)/max(dt1,em20)
         enddo
      endif
      ! needed in mqviscb
      facq0  = one
      imat   = mat(1)
      npar   = ipm(9,imat)
      iadbuf = ipm(7,imat)
      nfunc  = ipm(10,imat)
      numtabl= ipm(226,imat)
      uparam => bufmat(iadbuf:iadbuf+npar-1)
      ifunc  => ipm(10+1:10+nfunc,imat)
      itable => ipm(226+1:226+numtabl,imat)
      matparam => mat_elem%mat_param(imat)
      nuvarr = ipm(221,imat)
      !==================================================
      ! recovering data from matparam data structure
      ! material compressibility
      !   -> 1 : compressible
      !   -> 2 : incompressible
      !   -> 3 : elasto-plastic (default)
      mat_comp  = matparam%compressibility
      ! strain computation
      !   -> 1 : small strain
      !   -> 2 : large strain (default)
      mat_smstr = matparam%smstr
      ! reference for strain computation
      !   -> 1 : incremental (default)
      !   -> 2 : total
      mat_formu = matparam%strain_formulation
      !==================================================
!
      bidon1 = zero
      bidon2 = zero
      bidon3 = zero
      bidon4 = zero
      bidon5 = zero
      bid1 = zero
      bid3 = zero
      ibid    = 0
      ibidon1 = 0
      ibidon2 = 0
      ibidon3 = 0
      ibidon4 = 0
      ilaw_user = ipm(217, imat)
      isvis = igeo(35,pid(1))
      if (impl_s >0.and.idyna == 0) isvis = 0
!
      dpla(1:nel)  = zero

      if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
         defp0(1:nel) = lbuf%pla(1:nel)
      else
         defp0(1:nel) = zero
      endif


      if (mtn == 67) then
         el_temp => uvar(nft+1:nft+nel)
      elseif (jthe == 0 .and. elbuf_tab(ng)%bufly(ilay)%l_temp > 0) then
         el_temp => ltemp     ! lbuf%temp
      else
         el_temp => tempel(1:nel)
      endif

      !initial scale factor for yield stress defined per ipg,npi
      scale1(1:nel) = one
      yldfac => scale1(1:nel)
      if ( elbuf_tab(ng)%bufly(ilay)%l_fac_yld > 0) then
         yldfac => lbuf%fac_yld(1:nel)
      endif

      do i=1,nel
         c1(i)  = pm(32,imat)
         rho0(i)= pm( 1,imat)
         vis(i) = zero
         ep1(i) = d1(i)*off(i)
         ep2(i) = d2(i)*off(i)
         ep3(i) = d3(i)*off(i)
         ep4(i) = d4(i)*off(i)
         ep5(i) = d5(i)*off(i)
         ep6(i) = d6(i)*off(i)
         sv1(i) = zero
         sv2(i) = zero
         sv3(i) = zero
         sv4(i) = zero
         sv5(i) = zero
         sv6(i) = zero
         e7(i)  = zero
      enddo
      if (mtn==29.or.mtn==30.or.mtn==31.or.mtn==37.or.mtn==51.or.mtn==65.or.mtn==75.or.mtn==97.or.mtn==99.or.mtn==105) then
         s1(1:mvsiz) = zero
         s2(1:mvsiz) = zero
         s3(1:mvsiz) = zero
         s4(1:mvsiz) = zero
         s5(1:mvsiz) = zero
         s6(1:mvsiz) = zero
      endif

      if (jcvt > 0) then
!---------------------------
!       isotropic and orthotropic convected
!---------------------------
         do i=1,nel
            de1(i) = ep1(i)*dt1
            de2(i) = ep2(i)*dt1
            de3(i) = ep3(i)*dt1
            de4(i) = ep4(i)*dt1
            de5(i) = ep5(i)*dt1
            de6(i) = ep6(i)*dt1
            so1(i) = sig(i,1)
            so2(i) = sig(i,2)
            so3(i) = sig(i,3)
            so4(i) = sig(i,4)
            so5(i) = sig(i,5)
            so6(i) = sig(i,6)
            wxx(i)=zero
            wyy(i)=zero
            wzz(i)=zero
         enddo
!
      elseif ( mtn==68 ) then
!---------------------------
! global nonsymmetric orthotropic tensor
!---------------------------
         call mreploc(&
         &gama,    r11,     r12,     r13,&
         &r21,     r22,     r23,     r31,&
         &r32,     r33,     rx,      ry,&
         &rz,      sx,      sy,      sz,&
         &tx,      ty,      tz,      nel,&
         &jsph)
         call mrotensns(1,nel,&
         &ep1,dxy,dxz,&
         &dyx,ep2,dyz,&
         &dzx,dzy,ep3,&
         &r11,r12,r13,&
         &r21,r22,r23,&
         &r31,r32,r33)
#include "vectorize.inc"
         do i=1,nel
            ep4(i) = (dxy(i)+dyx(i))*off(i)
            ep5(i) = (dyz(i)+dzy(i))*off(i)
            ep6(i) = (dzx(i)+dxz(i))*off(i)
            ep4(i+nel) = (dxy(i)-dyx(i))*off(i)
            ep5(i+nel) = (dyz(i)-dzy(i))*off(i)
            ep6(i+nel) = (dzx(i)-dxz(i))*off(i)

            de1(i)= ep1(i)*dt1
            de2(i)= ep2(i)*dt1
            de3(i)= ep3(i)*dt1
            de4(i)= ep4(i)*dt1
            de5(i)= ep5(i)*dt1
            de6(i)= ep6(i)*dt1
            de4(i+nel) = ep4(i+nel)*dt1
            de5(i+nel) = ep5(i+nel)*dt1
            de6(i+nel) = ep6(i+nel)*dt1

            so1(i) = sigl(i,1)
            so2(i) = sigl(i,2)
            so3(i) = sigl(i,3)
            so4(i) = sigl(i,4)
            so5(i) = sigl(i,5)
            so6(i) = sigl(i,6)

            so4(i+nel) = sigdd(i,4)
            so5(i+nel) = sigdd(i,5)
            so6(i+nel) = sigdd(i,6)
            s4(i+nel) = zero
            s5(i+nel) = zero
            s6(i+nel) = zero

            wxx(i)=zero
            wyy(i)=zero
            wzz(i)=zero
         enddo
      elseif (isorth /= 0) then
!---------------------------
!       orthotropic global
!---------------------------
         call mreploc(&
         &gama,    r11,     r12,     r13,&
         &r21,     r22,     r23,     r31,&
         &r32,     r33,     rx,      ry,&
         &rz,      sx,      sy,      sz,&
         &tx,      ty,      tz,      nel,&
         &jsph)
         do i=1,nel
            ep4(i) = half*ep4(i)
            ep5(i) = half*ep5(i)
            ep6(i) = half*ep6(i)
         enddo
         call mrotens(1,nel,ep1,ep2,ep3,ep4,ep5,ep6,&
         &r11,r12,r13,&
         &r21,r22,r23,&
         &r31,r32,r33)
#include "vectorize.inc"
         do i=1,nel
            ep4(i) = two*ep4(i)
            ep5(i) = two*ep5(i)
            ep6(i) = two*ep6(i)
            de1(i)= ep1(i)*dt1
            de2(i)= ep2(i)*dt1
            de3(i)= ep3(i)*dt1
            de4(i)= ep4(i)*dt1
            de5(i)= ep5(i)*dt1
            de6(i)= ep6(i)*dt1
            so1(i) = sigl(i,1)
            so2(i) = sigl(i,2)
            so3(i) = sigl(i,3)
            so4(i) = sigl(i,4)
            so5(i) = sigl(i,5)
            so6(i) = sigl(i,6)
            wxx(i)=zero
            wyy(i)=zero
            wzz(i)=zero
         enddo
      else
!---------------------------
!       isotropic global
!---------------------------
#include "vectorize.inc"
         do i=1,nel
            de1(i) = ep1(i)*dt1
            de2(i) = ep2(i)*dt1
            de3(i) = ep3(i)*dt1
            de4(i) = ep4(i)*dt1
            de5(i) = ep5(i)*dt1
            de6(i) = ep6(i)*dt1
            so1(i) = sig(i,1)
            so2(i) = sig(i,2)
            so3(i) = sig(i,3)
            so4(i) = sig(i,4)
            so5(i) = sig(i,5)
            so6(i) = sig(i,6)
         enddo
!
#include "vectorize.inc"
         do i=1,nel
            wxxf=wxx(i)*off(i)
            wyyf=wyy(i)*off(i)
            wzzf=wzz(i)*off(i)
            q1=strain(i,4)*wzzf
            q2=strain(i,6)*wyyf
            q3=strain(i,5)*wxxf
            ss1=strain(i,1)-q1+q2
            ss2=strain(i,2)+q1-q3
            ss3=strain(i,3)-q2+q3
            ss4=strain(i,4)+2.*wzzf*(strain(i,1)-strain(i,2))+&
            &wyyf*strain(i,5)-wxxf*strain(i,6)
            ss5=strain(i,5)+2.*wxxf*(strain(i,2)-strain(i,3))+&
            &wzzf*strain(i,6)-wyyf*strain(i,4)
            ss6=strain(i,6)+2.*wyyf*(strain(i,3)-strain(i,1))+&
            &wxxf*strain(i,4)-wzzf*strain(i,5)
            strain(i,1)= ss1
            strain(i,2)= ss2
            strain(i,3)= ss3
            strain(i,4)= ss4
            strain(i,5)= ss5
            strain(i,6)= ss6
         enddo
      endif
!--------------------------
      if (istrain > 0) then
         do i=1,nel
            strain(i,1)= strain(i,1) + de1(i)
            strain(i,2)= strain(i,2) + de2(i)
            strain(i,3)= strain(i,3) + de3(i)
            strain(i,4)= strain(i,4) + de4(i)
            strain(i,5)= strain(i,5) + de5(i)
            strain(i,6)= strain(i,6) + de6(i)
            es1(i) = strain(i,1)
            es2(i) = strain(i,2)
            es3(i) = strain(i,3)
            es4(i) = strain(i,4)
            es5(i) = strain(i,5)
            es6(i) = strain(i,6)
         enddo
      else
         do i=1,nel
            es1(i) = zero
            es2(i) = zero
            es3(i) = zero
            es4(i) = zero
            es5(i) = zero
            es6(i) = zero
         enddo
      endif
!-------------------------------------------------------------------------
!     total strain
!-------------------------------------------------------------------------
      if (ismstr == 10.or.ismstr == 12) then
!       [f] = [m_f]+[1]
!       only [b] is used for ismstr=10,12 now
!------ use uniform [es] (instead of [mf]) as other laws
         if (iselect > 0) then
            do i=1,nel
               es1(i)=etotsh(i,1)*off(i)
               es2(i)=etotsh(i,2)*off(i)
               es3(i)=etotsh(i,3)*off(i)
               es4(i)=etotsh(i,4)*off(i)
               es6(i)=etotsh(i,6)*off(i)
               es5(i)=etotsh(i,5)*off(i)
            end do
         else
            do i=1,nel
               mfxx(i) = off(i)*mfxx(i)
               mfyy(i) = off(i)*mfyy(i)
               mfzz(i) = off(i)*mfzz(i)
               mfxy(i) = off(i)*mfxy(i)
               mfyx(i) = off(i)*mfyx(i)
               mfzx(i) = off(i)*mfzx(i)
               mfxz(i) = off(i)*mfxz(i)
               mfyz(i) = off(i)*mfyz(i)
               mfzy(i) = off(i)*mfzy(i)
            enddo
!           is like 42,62,92,88,90
!           [es] = [b] - i = [f][f]^t - i
            do i=1,nel
               es1(i)=mfxx(i)*(two+mfxx(i))+&
               &mfxy(i)*mfxy(i)+mfxz(i)*mfxz(i)
               es2(i)=mfyy(i)*(two+mfyy(i))+&
               &mfyx(i)*mfyx(i)+mfyz(i)*mfyz(i)
               es3(i)=mfzz(i)*(two+mfzz(i))+&
               &mfzx(i)*mfzx(i)+mfzy(i)*mfzy(i)
               es4(i)=mfxy(i)+mfyx(i)+mfxx(i)*mfyx(i)+&
               &mfxy(i)*mfyy(i)+mfxz(i)*mfyz(i)
               es6(i)=mfxz(i)+mfzx(i)+mfxx(i)*mfzx(i)+&
               &mfxy(i)*mfzy(i)+mfxz(i)*mfzz(i)
               es5(i)=mfzy(i)+mfyz(i)+mfzx(i)*mfyx(i)+&
               &mfzy(i)*mfyy(i)+mfzz(i)*mfyz(i)
            enddo
         end if !(iselect>0) then
!
         if (idtmin(1)==3.and.ismstr == 12) then
            do i=1,nel
               if (offg(i) <=one) cycle
               es1(i)=mfxx(i)
               es2(i)=mfyy(i)
               es3(i)=mfzz(i)
               es4(i)=half*(mfxy(i)+mfyx(i))
               es5(i)=half*(mfzy(i)+mfyz(i))
               es6(i)=half*(mfxz(i)+mfzx(i))
            enddo
         end if
!---------------------------
!       orthotropic global
!---------------------------
         if (jcvt <= 0 .and. isorth /= 0) then
!
            call mreploc(&
            &gama,    r11,     r12,     r13,&
            &r21,     r22,     r23,     r31,&
            &r32,     r33,     rx,      ry,&
            &rz,      sx,      sy,      sz,&
            &tx,      ty,      tz,      nel,&
            &jsph)
            call mrotens(1,nel,es1,es2,es3,es4,es5,es6,&
            &r11,r12,r13,&
            &r21,r22,r23,&
            &r31,r32,r33)
         endif
         do i=1,nel
            es4(i) = two*es4(i)
            es5(i) = two*es5(i)
            es6(i) = two*es6(i)
         enddo
!       endif  ! mtn
!-------------------------------
      elseif (ismstr == 11) then
!-------------------------------
         do i=1,nel
            es1(i)=mfxx(i)
            es2(i)=mfyy(i)
            es3(i)=mfzz(i)
            es4(i)=mfxy(i)+mfyx(i)
            es6(i)=mfxz(i)+mfzx(i)
            es5(i)=mfzy(i)+mfyz(i)
         enddo
         if (jcvt == 0 .and. isorth /= 0) then
!---------------------------
!       orthotropic global
!---------------------------
            do i=1,nel
               es4(i) = half*es4(i)
               es5(i) = half*es5(i)
               es6(i) = half*es6(i)
            enddo
            call mreploc(&
            &gama,    r11,     r12,     r13,&
            &r21,     r22,     r23,     r31,&
            &r32,     r33,     rx,      ry,&
            &rz,      sx,      sy,      sz,&
            &tx,      ty,      tz,      nel,&
            &jsph)
            call mrotens(1,nel,es1,es2,es3,es4,es5,es6,&
            &r11,r12,r13,&
            &r21,r22,r23,&
            &r31,r32,r33)
            do i=1,nel
               es4(i) = two*es4(i)
               es5(i) = two*es5(i)
               es6(i) = two*es6(i)
            enddo
         endif
!---------------------------
         do i=1,nel
            strain(i,1)= es1(i)
            strain(i,2)= es2(i)
            strain(i,3)= es3(i)
            strain(i,4)= es4(i)
            strain(i,5)= es5(i)
            strain(i,6)= es6(i)
         enddo
!       therm stress computation-----
         if (iexpan > 0) then
            do i=1,nel
               strain(i,1)= strain(i,1)-epsth(i)
               strain(i,2)= strain(i,2)-epsth(i)
               strain(i,3)= strain(i,3)-epsth(i)
            enddo
         endif
      endif  ! ismstr = 10,11,12

!---------------------------------------------------------
!     strain rate filtering coefficient
!---------------------------------------------------------
      israte = ipm(3,imat)
      if (israte > 0) then
         asrate = min(one, pm(9,imat)*dt1)
      endif
!--------------------------------------------------------
!     end total strain
!---------------------------------------------------------
!
!--------------------------------------------------------
!     compute undamaged effective stresses
!---------------------------------------------------------
      ! -> isotropic stress softening
      if (dmg_flag == 1) then
         do i = 1,nel
            so1(i) = so1(i)/max(lbuf%dmgscl(i),em20)
            so2(i) = so2(i)/max(lbuf%dmgscl(i),em20)
            so3(i) = so3(i)/max(lbuf%dmgscl(i),em20)
            so4(i) = so4(i)/max(lbuf%dmgscl(i),em20)
            so5(i) = so5(i)/max(lbuf%dmgscl(i),em20)
            so6(i) = so6(i)/max(lbuf%dmgscl(i),em20)
         enddo
         ! -> orthotropic stress softening
      elseif (dmg_flag == 6) then
         do i = 1,nel
            so1(i) = so1(i)/max(lbuf%dmgscl(i+nel*(1-1)),em20)
            so2(i) = so2(i)/max(lbuf%dmgscl(i+nel*(2-1)),em20)
            so3(i) = so3(i)/max(lbuf%dmgscl(i+nel*(3-1)),em20)
            so4(i) = so4(i)/max(lbuf%dmgscl(i+nel*(4-1)),em20)
            so5(i) = so5(i)/max(lbuf%dmgscl(i+nel*(5-1)),em20)
            so6(i) = so6(i)/max(lbuf%dmgscl(i+nel*(6-1)),em20)
         enddo
      endif
!
!---------------------------------------------------------
!     select material law
!---------------------------------------------------------
      if (mtn == 28) then
         call sigeps28(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,amu )
      elseif (mtn == 33) then
         call sigeps33(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  )
      elseif (mtn == 34) then
         call sigeps34(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  )
      elseif (mtn == 35) then
         call sigeps35(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,israte,asrate,&
         &epsd)
      elseif (mtn == 36) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         nrate = nint(uparam(1))
         fisokin = uparam(6+2*nrate+8)
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
         call sigeps36(nel    ,nuvar  ,nfunc  ,ifunc  ,npf ,&
         &tf     ,tt     ,dt1    ,uparam ,rho0,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6   ,&
         &es1    ,es2    ,es3    ,es4    ,es5    ,es6   ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6   ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6    ,&
         &ssp    ,vis    ,uvar   ,off    ,ngl    ,&
         &ipm    ,mat    ,epsd   ,ipla   ,sigy   ,defp  ,&
         &dpla   ,et     ,al_imp ,signor ,amu    ,yldfac,&
         &nvartmp,vartmp ,lbuf%dmg,inloc,lbuf%planl,&
         &sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx )

      elseif (mtn == 37) then
         if (n2d == 0) then
            n48 = 8
            nix = nixs
         else
            n48 = 4
            nix = nixq
         endif
         call sigeps37(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ix   ,nix ,&
         &nft)
      elseif (mtn == 38) then
         call sigeps38(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,&
         &ismstr,mfxx,mfxy,mfxz,mfyx,&
         &mfyy ,mfyz ,mfzx ,mfzy,mfzz,et ,&
         &ihet ,gbuf%off,epsd )
      elseif (mtn == 40) then
         call sigeps40(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  )
      elseif (mtn == 41) then
         call sigeps41(nel ,npar,nuvar,&
         &tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &so1 ,so2 ,so3 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,dvol ,lbuf%bfrac,&
         &lbuf%temp)
      elseif (mtn == 42) then
         call sigeps42(&
         &nel     ,npar    ,nuvar   ,nfunc   ,ifunc   ,npf     ,&
         &tf      ,tt      ,dt1     ,uparam,rho0,rho   ,&
         &voln    ,eint    ,uvar    ,off     ,gbuf%off,ssp     ,&
         &ep1     ,ep2     ,ep3     ,ep4     ,ep5     ,ep6     ,&
         &es1     ,es2     ,es3     ,es4     ,es5     ,es6     ,&
         &s1      ,s2      ,s3      ,s4      ,s5      ,s6      ,&
         &mfxx    ,mfxy    ,mfxz    ,mfyx    ,mfyy    ,mfyz    ,&
         &mfzx    ,mfzy    ,mfzz    ,vis     ,ismstr  ,et      ,&
         &ihet    ,epsth3  ,iexpan  )
!
      elseif (mtn == 44) then
         call sigeps44(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,rho0,rho ,&
         &voln,eint,matparam%ieos,dpdm   ,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsd,ipla ,sigy ,defp,&
         &dpla,amu ,israte ,asrate,nvartmp,&
         &vartmp )
      elseif (mtn == 45) then
         call sigeps45(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,sigy ,defp,&
         &amu )
      elseif (mtn == 48) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps48(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsd,sigy ,defp ,dpla,&
         &amu )
      elseif (mtn == 50) then
         call sigeps50(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,nvartmp ,vartmp  ,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,amu  ,matparam)
      elseif (mtn == 51) then
         if (n2d == 0) then
            n48 = 8
            nix = nixs
            numel = numels
            nv46=6
         else
            n48 = 4
            nix = nixq
            numel = numelq
            nv46=4
         endif
         !numerical viscosity is managed inside sigeps51.f
         !facq0 = zero
         call sigeps51(nel       ,npar        ,nuvar   ,nfunc ,ifunc     ,&
         &npf       ,tf          ,tt      ,dt1   ,uparam    ,numel     ,&
         &rho       ,vol         ,eint    ,vk    ,&
         &ep1       ,ep2         ,ep3     ,ep4   ,ep5       ,ep6       ,&
         &de1       ,de2         ,de3     ,de4   ,de5       ,de6       ,&
         &so1       ,so2         ,so3     ,so4   ,so5       ,so6       ,&
         &s1        ,s2          ,s3      ,s4    ,s5        ,s6        ,&
         &sv1       ,sv2         ,sv3     ,sv4   ,sv5       ,sv6       ,&
         &ssp       ,vis         ,uvar    ,off   ,nft       ,v         ,&
         &w         ,x           ,ix      ,n48   ,nix       ,jthe      ,&
         &geo       ,pid         ,ilay    ,ng    ,elbuf_tab , pm       ,&
         &iparg     ,ale_connect ,bufvois ,ipm   ,bufmat    ,stifn     ,&
         &vd2       ,vdx         ,vdy     ,vdz   ,&
         &gbuf%qvis ,dvol        ,qold    ,nv46  )
      elseif (mtn == 52) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps52(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsd,ipla ,sigy ,defp,&
         &table )

      elseif (mtn == 53) then
         call sigeps53(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsp,ipla ,lbuf%seq)
      elseif (mtn == 56) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps56(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsd,ipla ,sigy ,defp,&
         &dpla,amu )
      elseif (mtn == 60) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps60(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm  ,mat ,epsd,ipla ,sigy ,defp,&
         &dpla ,amu )
      elseif (mtn == 62) then
         call sigeps62(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off,epsth3,iexpan)

      elseif (mtn == 65) then
!-------------------
!     strain rate
!-------------------
         do i=1,nel
            dav = (ep1(i)+ep2(i)+ep3(i))*third
            e1 = ep1(i) - dav
            e2 = ep2(i) - dav
            e3 = ep3(i) - dav
            e4 = half*ep4(i)
            e5 = half*ep5(i)
            e6 = half*ep6(i)
            epsp =half*(e1**2+e2**2+e3**2) +e4**2+e5**2+e6**2
            epsp = sqrt(three*epsp)*third
            epsd(i)=epsp
         enddo
         call sigeps65(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,ipm ,&
         &mat ,epsd,ipla ,sigy ,defp,et  ,&
         &dpla,amu )

      elseif (mtn == 66) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps66(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &ssp ,vis ,uvar ,off  ,ngl  ,ipm ,&
         &mat ,epsd,ipla ,sigy ,defp ,et  ,&
         &amu )
      elseif (mtn == 67) then
         call sigeps67(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,dvol ,pm(1,imat))
      elseif (mtn == 68) then
         call sigeps68(nel ,npar,nuvar ,nfunc ,ifunc,npf ,&
         &tf  ,tt  ,dt1   ,bufmat,rho0 ,rho ,&
         &voln,eint,fr_wav,&
         &ep1 ,ep2 ,ep3   ,ep4   ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3   ,de4   ,de5  ,de6 ,&
         &es1 ,es2 ,es3   ,es4   ,es5  ,es6 ,&
         &so1 ,so2 ,so3   ,so4   ,so5  ,so6 ,&
         &s1  ,s2  ,s3    ,s4    ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3   ,sv4   ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar  ,off   ,ngl  ,0   ,&
         &ipm ,mat ,amu   )
!
      elseif (mtn == 69) then
         ivisc  = ipm(222,imat)
         nuparv = ipm(224,imat)
         nuvarv = ipm(225,imat)
         iadvis = ipm(223,imat)
         if (ivisc > 0) then
            uparvis => bufmat(iadvis:iadvis+nuparv)
         else
            allocate (bufzero(0))
            uparvis => bufzero
         end if

         call sigeps69(nel  ,npar,nuvar,nfunc,ifunc,npf  ,&
         &tf   ,tt,dt1,uparam,rho0 ,rho  ,&
         &voln ,eint,ivisc,nuparv,uparvis ,&
         &ep1  ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1  ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1  ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1  ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1   ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1  ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp  ,vis ,uvar,off  ,wxx  ,wyy ,&
         &wzz  ,ismstr,mfxx,mfxy,mfxz,mfyx,&
         &mfyy ,mfyz ,mfzx ,mfzy,mfzz,et ,&
         &ihet ,nuvarv,vbuf%var,offg ,epsth3,iexpan)
!
      elseif (mtn == 70) then
!-------------------
!     strain rate
!-------------------
         idev = 0
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
! qa=qb=zero by default for law70
         if (igeo(31,pid(1)) == 1) facq0 = zero
!
         call sigeps70(&
         &nel,     npar,    nuvar,   nfunc,&
         &ifunc,   npf,     tf,      tt,&
         &dt1,     bufmat,  rho0,    rho,&
         &offg,    rhoref  ,nvartmp, vartmp,&
         &rhosp,   de1,&
         &de2,     de3,     de4,     de5,&
         &de6,     es1,     es2,     es3,&
         &es4,     es5,     es6,     so1,&
         &so2,     so3,     so4,     so5,&
         &so6,     s1,      s2,      s3,&
         &s4,      s5,      s6,&
         &ssp,     vis,     uvar,&
         &off,     ngl,     ipm,&
         &mat,     epsd,    et,      ismstr,&
         &ihet,    jsms,    matparam )
!
      elseif (mtn == 71) then
         call sigeps71(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl ,ipm,&
         &mat ,jthe,tempel,ismstr,et)!,et
      elseif (mtn == 72) then
         call sigeps72(nel      ,npar     ,nuvar    ,&
         &tt       ,dt1      ,uparam   ,rho0     ,rho      ,&
         &de1      ,de2      ,de3      ,de4      ,de5      ,de6      ,&
         &so1      ,so2      ,so3      ,so4      ,so5      ,so6      ,&
         &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
         &ssp      ,uvar     ,off      ,ngl      ,sigy     ,defp     ,&
         &dpla     ,et       ,lbuf%seq ,lbuf%dmg ,inloc    ,varnl    )
      elseif (mtn == 74) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps74(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,bufmat,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ngl  ,0   ,&
         &ipm ,mat ,epsd,ipla ,sigy ,defp,&
         &dpla,et  ,jthe,tempel,table,lbuf%seq,&
         &amu ,iseq)
!
      elseif (mtn == 75) then
         call sigeps75(nel     ,npar   ,nuvar ,nfunc ,ifunc ,&
         &npf     ,tf     ,tt    ,dt1   ,uparam,&
         &rho0    ,rho    ,voln  ,eint  ,muold ,&
         &ep1     ,ep2    ,ep3   ,ep4   ,ep5   ,ep6 ,&
         &de1     ,de2    ,de3   ,de4   ,de5   ,de6 ,&
         &es1     ,es2    ,es3   ,es4   ,es5   ,es6 ,&
         &so1     ,so2    ,so3   ,so4   ,so5   ,so6 ,&
         &s1      ,s2     ,s3    ,s4    ,s5    ,s6  ,&
         &sv1     ,sv2    ,sv3   ,sv4   ,sv5   ,sv6 ,&
         &ssp     ,vis    ,uvar  ,off   ,dvol  ,vol ,&
         &pm      ,ipm    ,mat   ,psh   ,bufmat,&
         &ebuf%var,nvareos,mat_elem%mat_param)
!
      elseif (mtn == 76) then
         call sigeps76(nel      ,npar     ,nuvar    ,nfunc    ,ifunc    ,ngl       ,&
         &npf      ,tf       ,tt       ,dt1      ,uparam   ,matparam  ,&
         &rho0     ,dpla     ,et       ,ssp      ,sigy     ,uvar      ,&
         &de1      ,de2      ,de3      ,de4      ,de5      ,de6       ,&
         &so1      ,so2      ,so3      ,so4      ,so5      ,so6       ,&
         &s1       ,s2       ,s3       ,s4       ,s5       ,s6        ,&
         &off      ,epsd     ,defp     ,inloc    ,l_planl  ,lbuf%planl,&
         &lbuf%dmg ,idel7nok ,nvartmp  ,vartmp   )
!
      elseif (mtn == 78) then
         call sigeps78(nel ,npar,nuvar,nfunc,ifunc,npf ,&
         &tf  ,tt,dt1,uparam,rho0,rho ,&
         &lbuf%siga,lbuf%sigb,lbuf%sigc,uvar,defp,dpla,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &ssp  ,off  ,sigy ,et )
      elseif (mtn == 77) then
         idev = 0
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps77(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,fssp,vis ,uvar,off  ,ngl ,&
         &pm  ,ipm,mat ,epsd  ,pair )

      elseif (mtn == 79) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps79(&
         &nel      ,npar     ,nuvar    ,tt       ,dt1      ,uparam   ,&
         &rho0     ,rho      ,ngl      ,sigy     ,dpla     ,defp     ,&
         &de1      ,de2      ,de3      ,de4      ,de5      ,de6      ,&
         &so1      ,so2      ,so3      ,so4      ,so5      ,so6      ,&
         &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
         &epsd     ,lbuf%dmg ,ssp      ,uvar     ,off      ,amu      ,&
         &idel7nok )
!
      elseif (mtn == 80) then
         call sigeps80(&
         &nel,     npar,    nuvar,   nfunc,&
         &ifunc,   npf,     tf,      tt,&
         &dt1,     uparam,  rho0,    rho,&
         &voln,    eint,    ep1,     ep2,&
         &ep3,     ep4,     ep5,     ep6,&
         &de1,     de2,     de3,     de4,&
         &de5,     de6,     es1,     es2,&
         &es3,     es4,     es5,     es6,&
         &so1,     so2,     so3,     so4,&
         &so5,     so6,     s1,      s2,&
         &s3,      s4,      s5,      s6,&
         &ssp,     vis,     uvar,    off,&
         &ngl,     ipm,     mat,     epsd,&
         &sigy,    defp,    table,   tempel,&
         &nvartmp, vartmp,  trepsth, eintth,&
         &jthe)
         do i=1,nel
            d1(i) = ep1(i)
            d2(i) = ep2(i)
            d3(i) = ep3(i)
            dvol(i) = dvol(i)-trepsth(i)*vol_avg(i)
         enddo

      elseif (mtn == 81) then
         call sigeps81(nel   ,npar ,nuvar,nfunc ,ifunc ,ngl   ,&
         &npf   ,tf   ,tt   ,uparam,rho0  ,rho   ,&
         &voln  ,amu  ,defp ,ssp   ,vis   ,uvar  ,&
         &ep1   ,ep2  ,ep3  ,ep4   ,ep5   ,ep6   ,&
         &de1   ,de2  ,de3  ,de4   ,de5   ,de6   ,&
         &es1   ,es2  ,es3  ,es4   ,es5   ,es6   ,&
         &so1   ,so2  ,so3  ,so4   ,so5   ,so6   ,&
         &s1    ,s2   ,s3   ,s4    ,s5    ,s6    ,&
         &sv1   ,sv2  ,sv3  )


      elseif (mtn == 82) then
         call sigeps82(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &mfxx,mfxy,mfxz,mfyx, mfyy ,mfyz ,&
         &mfzx ,mfzy,mfzz,ihet,gbuf%off ,&
         &epsth3,iexpan)
      elseif (mtn == 84) then
         call sigeps84(&
         &nel,     npar,    nuvar,   nfunc,&
         &ifunc,   npf,     tf,      tt,&
         &dt1,     uparam,  rho0,    rho,&
         &voln,    eint,    tempel,  ngl,&
         &ep1,     ep2,     ep3,     ep4,&
         &ep5,     ep6,     de1,     de2,&
         &de3,     de4,     de5,     de6,&
         &es1,     es2,     es3,     es4,&
         &es5,     es6,     so1,     so2,&
         &so3,     so4,     so5,     so6,&
         &s1,      s2,      s3,      s4,&
         &s5,      s6,      sv1,     sv2,&
         &sv3,     sv4,     sv5,     sv6,&
         &ssp,     vis,     uvar,    off,&
         &sigy,    defp,    dpla,    et,&
         &ipm,     mat,     jthe)
      elseif (mtn == 88) then
         call sigeps88(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,israte,&
         &asrate,et ,ihet,gbuf%off,epsth3,iexpan,&
         &epsd )
      elseif (mtn == 90) then

!-------------------
!     visco-hypereslatic law defined by stress strain curve
!-------------------
         call sigeps90(nel ,nuvar,nfunc,ifunc,npf ,&
         &tf  ,tt,uparam,rho0,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &ssp ,vis ,uvar,nvartmp,vartmp ,ismstr,&
         &israte,asrate, gbuf%off ,ihet ,et ,epsd  )
!
      elseif (mtn == 92) then
         call sigeps92(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off ,epsth3,iexpan, lbuf%epsa)
      elseif (mtn == 94) then
         call sigeps94(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off,epsth3,iexpan )
      elseif (mtn == 93) then
         call sigeps93(nel    ,npar   ,nuvar  ,nfunc  ,ifunc  ,&
         &npf    ,tf     ,tt     ,dt1    ,uparam ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &ssp    ,defp   ,uvar   ,rho0   ,off    ,&
         &et     ,sigy   ,lbuf%seq,epsd  ,asrate ,&
         &nvartmp,vartmp ,dpla   )
      elseif (mtn == 95) then
         ir = int (bufmat(iadbuf+20))
         if (ir > zero) then
            nparf = matparam%fail(ir)%nuparam
            uvarf => fbuf%floc(ir)%var
            nvarf =  fbuf%floc(ir)%nvar
            uparf => matparam%fail(ir)%uparam(1:nparf)
         else
            nparf = 0
            nvarf = 1
            uvarf => uvarf1
            uparf => vec0
         endif
         call sigeps95(nel  ,npar ,nuvar,nfunc,ifunc,&
         &npf  ,tf   ,tt   ,dt1  ,uparam,&
         &rho0 ,rho  ,voln ,eint ,ngl,&
         &ep1  ,ep2  ,ep3  ,ep4  ,ep5  ,ep6 ,&
         &de1  ,de2  ,de3  ,de4  ,de5  ,de6 ,&
         &es1  ,es2  ,es3  ,es4  ,es5  ,es6 ,&
         &so1  ,so2  ,so3  ,so4  ,so5  ,so6 ,&
         &s1   ,s2   ,s3   ,s4   ,s5   ,s6  ,&
         &sv1  ,sv2  ,sv3  ,sv4  ,sv5  ,sv6 ,&
         &mfxx ,mfxy ,mfxz ,mfyx ,mfyy ,mfyz,&
         &mfzx ,mfzy ,mfzz ,&
         &ssp  ,vis  ,uvar ,off  ,ismstr,et ,&
         &ihet ,gbuf%off ,epsth3,iexpan ,nparf,&
         &uparf,uvarf, nvarf,&
         &jcvt , gbuf%gama_r) !gama_r is for the corotational formulation
!
      elseif (mtn == 96) then
         call sigeps96(&
         &nel     ,ngl     ,npar    ,nuvar    ,nfunc   ,ifunc   ,&
         &npf     ,tf      ,uparam,uvar,jthe   ,&
         &rho     ,tempel  ,defp    ,ssp      ,gbuf%off,epsd    ,&
         &ep1     ,ep2     ,ep3     ,ep4      ,ep5     ,ep6     ,&
         &de1     ,de2     ,de3     ,de4      ,de5     ,de6     ,&
         &so1     ,so2     ,so3     ,so4      ,so5     ,so6     ,&
         &s1      ,s2      ,s3      ,s4       ,s5      ,s6      )
!
      elseif (mtn == 97) then
         if (n2d == 0) then
            n48 = 8
            nix = nixs
         else
            n48 = 4
            nix = nixq
         endif
         call sigeps97(nel       ,npar   ,nuvar    ,nfunc  ,ifunc     ,lbuf%tb   ,&
         &npf       ,tf     ,tt       ,dt1    ,uparam    ,lbuf%bfrac,&
         &rho0      ,rho    ,vol      ,eint   ,sigy      ,deltax    ,&
         &ep1       ,ep2    ,ep3      ,ep4    ,ep5       ,ep6       ,&
         &de1       ,de2    ,de3      ,de4    ,de5       ,de6       ,&
         &es1       ,es2    ,es3      ,es4    ,es5       ,es6       ,&
         &so1       ,so2    ,so3      ,so4    ,so5       ,so6       ,&
         &s1        ,s2     ,s3       ,s4     ,s5        ,s6        ,&
         &sv1       ,sv2    ,sv3      ,sv4    ,sv5       ,sv6       ,&
         &ssp       ,vis    ,uvar     ,off    ,nft       ,v         ,&
         &w         ,x      ,ix       ,n48    ,nix       ,jthe      ,&
         &geo       ,pid    ,ilay     ,ng     ,elbuf_tab ,pm        ,&
         &iparg     ,bufvois  ,ipm    ,bufmat ,stifn     ,&
         &vd2       ,vdx    ,vdy      ,vdz    ,mat       ,voln      ,&
         &gbuf%qvis ,dvol   ,qold     )
!
      elseif (mtn == 100) then
         ir = int (bufmat(iadbuf+2))
         if (ir > zero) then
            nparf = matparam%fail(ir)%nuparam
            uvarf => fbuf%floc(ir)%var
            nvarf =  fbuf%floc(ir)%nvar
            uparf => matparam%fail(ir)%uparam(1:nparf)
         else
            nparf = 0
            nvarf = 1
            uvarf => uvarf1
            uparf => vec0
         endif
!
         call sigeps100(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &mfxx,mfxy,mfxz,mfyx ,mfyy ,mfyz,&
         &mfzx ,mfzy,mfzz,tempel,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off ,epsth,iexpan ,nparf,&
         &uparf,uvarf, nvarf,&
         &jcvt , gbuf%gama_r)   !gama_r is for the corotational formulation

      elseif (mtn == 101) then
! structure loading
         call sreploc3(&
         &rx,      ry,      rz,      sx,&
         &sy,      sz,      tx,      ty,&
         &tz,      r11,     r21,     r31,&
         &r12,     r22,     r32,     r13,&
         &r23,     r33,     nel)
         do i=1,nel
            fpsxx(i) = one+mfxx(i)
            fpsyy(i) = one+mfyy(i)
            fpszz(i) = one+mfzz(i)
         end do
         fpsxy(1:mvsiz) = mfxy(1:mvsiz)
         fpsyz(1:mvsiz) = mfyz(1:mvsiz)
         fpsxz(1:mvsiz) = mfxz(1:mvsiz)
         fpsyx(1:mvsiz) = mfyx(1:mvsiz)
         fpszy(1:mvsiz) = mfzy(1:mvsiz)
         fpszx(1:mvsiz) = mfzx(1:mvsiz)

         if (isorth /= 0) then
            call mreploc(&
            &gama,    r11,     r12,     r13,&
            &r21,     r22,     r23,     r31,&
            &r32,     r33,     rx,      ry,&
            &rz,      sx,      sy,      sz,&
            &tx,      ty,      tz,      nel,&
            &jsph)
            call mrotensns(1,nel,&
            &fpsxx,fpsxy,fpsxz,&
            &fpsyx,fpsyy,fpsyz,&
            &fpszx,fpszy,fpszz,&
            &r11,r12,r13,&
            &r21,r22,r23,&
            &r31,r32,r33)
         end if !(isorth /= 0) then
         call epsf2u(&
         &nel    ,fpsxx    , fpsxy  , fpsxz  , fpsyx  ,&
         &fpsyy  ,fpsyz    , fpszx  , fpszy  , fpszz  ,&
         &upsxx  ,upsyy    , upszz  , upsxy  , upsyz  ,&
         &upsxz  )
         call sigeps101(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off ,epsth,iexpan,tempel,&
         &fpsxx    , fpsxy  , fpsxz  , fpsyx  ,&
         &fpsyy  ,fpsyz    , fpszx  , fpszy  , fpszz  ,&
         &upsxx  ,upsyy    , upszz  , upsxy  , upsyz  ,&
         &upsxz  )

      elseif (mtn == 102) then

         call sigeps102(nel    ,npar    ,nuvar  ,uparam , rho0     ,rho    ,&
         &de1    ,de2     ,de3    ,de4    , de5      ,de6    ,&
         &so1    ,so2     ,so3    ,so4    , so5      ,so6    ,&
         &s1     ,s2      ,s3     ,s4     , s5       ,s6     ,&
         &ssp    ,uvar    ,off    ,et     ,&
         &psh    ,pnew    ,dpdm   ,ssp    , lbuf%pla )
!
!
      elseif (mtn == 103) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps103(nel    ,npar     ,nuvar  ,nfunc  , ifunc         ,&
         &npf    ,tf       ,tt     ,dt1    , uparam,&
         &rho0   ,rho      ,voln   ,eint   ,&
         &de1    ,de2      ,de3    ,de4    , de5           ,de6  ,&
         &es1    ,es2      ,es3    ,es4    , es5           ,es6  ,&
         &so1    ,so2      ,so3    ,so4    , so5           ,so6  ,&
         &s1     ,s2       ,s3     ,s4     , s5            ,s6   ,&
         &sv1    ,sv2      ,sv3    ,sv4    , sv5           ,sv6  ,&
         &ssp    ,vis      ,uvar   ,off    , dpdm          ,&
         &epsd   ,ltemp    ,tempel ,jthe   , lbuf%pla      )
!
!
      elseif (mtn == 104) then
!
         call sigeps104(nel    ,ngl    ,npar   ,nuvar  ,npg    ,gbuf%uelr,&
         &tt     ,dt1    ,uparam ,uvar   ,jthe   ,lbuf%off,&
         &rho0   ,rho    ,defp   ,dpla   ,epsd   ,ssp    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &sigy   ,et     ,tempel ,varnl  ,off    ,ipg    ,&
         &lbuf%dmg,l_dmg ,lbuf%planl,l_planl,lbuf%epsdnl,l_epsdnl,&
         &lbuf%temp,lbuf%seq,inloc)
!
      elseif (mtn == 105) then
         if (n2d == 0) then
            n48 = 8
            nix = nixs
         else
            n48 = 4
            nix = nixq
         endif
         call sigeps105(nel       ,npar   ,nuvar    ,nfunc      ,ifunc           ,lbuf%tb   ,&
         &npf       ,tf     ,tt       ,dt1        ,uparam  ,lbuf%bfrac,&
         &rho0      ,rho    ,vol      ,eint       ,sigy            ,deltax    ,&
         &ep1       ,ep2    ,ep3      ,ep4        ,ep5             ,ep6       ,&
         &de1       ,de2    ,de3      ,de4        ,de5             ,de6       ,&
         &es1       ,es2    ,es3      ,es4        ,es5             ,es6       ,&
         &so1       ,so2    ,so3      ,so4        ,so5             ,so6       ,&
         &s1        ,s2     ,s3       ,s4         ,s5              ,s6        ,&
         &sv1       ,sv2    ,sv3      ,sv4        ,sv5             ,sv6       ,&
         &ssp       ,vis    ,uvar     ,off        ,nft             ,v         ,&
         &w         ,x      ,ix       ,n48        ,nix             ,jthe      ,&
         &geo       ,pid    ,ilay     ,ng         ,elbuf_tab       ,pm        ,&
         &iparg     ,bufvois  ,ipm        ,bufmat          ,stifn     ,&
         &vd2       ,vdx    ,vdy      ,vdz        ,mat             ,voln      ,&
         &gbuf%qvis ,dvol   ,qold     )
!
      elseif (mtn == 106) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps106(nel    ,npar     ,nuvar  ,nfunc  , ifunc         ,&
         &npf    ,tf       ,tt     ,dt1    , uparam,&
         &rho0   ,rho      ,voln   ,eint   ,&
         &de1    ,de2      ,de3    ,de4    , de5           ,de6  ,&
         &es1    ,es2      ,es3    ,es4    , es5           ,es6  ,&
         &so1    ,so2      ,so3    ,so4    , so5           ,so6  ,&
         &s1     ,s2       ,s3     ,s4     , s5            ,s6   ,&
         &sv1    ,sv2      ,sv3    ,sv4    , sv5           ,sv6  ,&
         &ssp    ,vis      ,uvar   ,off    , lbuf%pla      ,dpla ,&
         &epsd   ,el_temp  ,jthe   ,nft    )
!
      elseif (mtn == 107) then
!
         call sigeps107(nel    ,ngl    ,npar   ,nuvar  ,tt     ,dt1    ,&
         &uparam ,uvar   ,jthe   ,off    ,rho0   ,rho    ,&
         &lbuf%pla,dpla  ,lbuf%epsd,ssp    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &sigy   ,et     ,&
         &nvartmp,numtabl,vartmp ,itable ,table  )
!
      elseif (mtn == 109) then
         call sigeps109(&
         &nel      ,ngl     ,npar     ,nuvar    ,nvartmp  ,numtabl  ,&
         &uparam   ,uvar    ,vartmp   ,itable   ,table    ,jthe     ,&
         &tt       ,dt1     ,off      ,rho0     ,lbuf%pla ,dpla     ,&
         &ssp      ,sigy    ,et       ,el_temp  ,epsd     ,dpdm     ,&
         &de1      ,de2     ,de3      ,de4      ,de5      ,de6      ,&
         &so1      ,so2     ,so3      ,so4      ,so5      ,so6      ,&
         &s1       ,s2      ,s3       ,s4       ,s5       ,s6       ,&
         &inloc    ,varnl   ,matparam%ieos)
!
      elseif (mtn == 111) then
         call sigeps111(nel ,npar,nuvar,nfunc,ifunc,&
         &npf ,tf  ,tt,dt1,uparam,&
         &rho0,rho ,voln,eint,ngl,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &de1 ,de2 ,de3 ,de4  ,de5  ,de6 ,&
         &es1 ,es2 ,es3 ,es4  ,es5  ,es6 ,&
         &so1 ,so2 ,so3 ,so4  ,so5  ,so6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &sv1 ,sv2 ,sv3 ,sv4  ,sv5  ,sv6 ,&
         &ssp ,vis ,uvar,off  ,ismstr,et ,&
         &ihet,gbuf%off ,epsth3,iexpan,&
         &mfxx,mfxy,mfxz,mfyx ,mfyy ,mfyz,&
         &mfzx ,mfzy,mfzz)
!
      elseif (mtn == 112) then
!
         call sigeps112(nel    ,ngl    ,npar   ,nuvar  ,tt     ,dt1    ,&
         &uparam ,uvar   ,jthe   ,off    ,rho0   ,rho    ,&
         &lbuf%pla,dpla  ,lbuf%epsd      ,ssp    ,es3    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &sigy   ,et     ,&
         &nvartmp,numtabl,vartmp ,itable   ,table   )
!
      elseif (mtn == 115) then
!
         call sigeps115(nel    ,ngl    ,npar   ,nuvar  ,gbuf%rho,&
         &tt     ,dt1    ,uparam ,uvar   ,off    ,sigy   ,&
         &rho0   ,defp   ,dpla   ,ssp    ,et     ,lbuf%seq,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     )
!
      elseif (mtn == 120) then
!       tapo (toughened adhesive polymer)

         call sigeps120(nel    ,npar   ,nuvar  ,nvartmp,numtabl,itable ,&
         &table  ,tt     ,dt1    ,ssp    ,uvar   ,vartmp ,&
         &uparam ,ngl    ,off    ,defp   ,epsd   ,tempel ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &jthe   ,inloc  ,varnl  ,lbuf%dmg,lbuf%dmgscl)
!
      elseif (mtn == 121) then
!
         call sigeps121(nel    ,ngl    ,npar   ,nuvar  ,nfunc  ,ifunc  ,&
         &npf    ,tf     ,dt1    ,tt     ,uparam ,uvar   ,&
         &rho    ,defp   ,dpla   ,ssp    ,epsd   ,off    ,&
         &lbuf%off,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6    ,&
         &so1    ,so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &sigy   ,et     ,varnl  ,inloc  ,gbuf%dt,&
         &ipg    ,npg    ,elbuf_tab(ng))
!
      elseif (mtn == 122) then
!
         idev = 0
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
!
         call sigeps122(nel    ,npar   ,nuvar  ,uparam ,uvar   ,rho0   ,&
         &es1    ,es2    ,es3    ,defp   ,dpla   ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &so2    ,so3    ,so4    ,so5    ,so6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &off    ,sigy   ,et     ,lbuf%dmg,lbuf%seq,epsd ,&
         &ssp    ,nfunc  ,ifunc  ,npf    ,tf     ,nvartmp,&
         &vartmp )
!
      elseif (mtn == 124) then
!
         idev = 0
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
         call sigeps124(nel    ,ngl    ,npar   ,nuvar  ,dt1    ,tt     ,&
         &uparam ,uvar   ,rho    ,defp   ,dpla   ,&
         &ssp    ,epsd   ,off    ,lbuf%off,ipg   ,npg    ,&
         &de1    ,de2    ,de3    ,de4    ,de5    ,de6    ,&
         &es1    ,es2    ,es3    ,es4    ,es5    ,es6    ,&
         &s1     ,s2     ,s3     ,s4     ,s5     ,s6     ,&
         &sigy   ,et     ,lbuf%dmg,deltax)
!
      elseif (mtn == 126) then
         idev = 1
         call mstrain_rate(nel    ,israte ,asrate ,epsd   ,idev   ,&
         &ep1    ,ep2    ,ep3    ,ep4    ,ep5    ,ep6)
         call sigeps126(&
         &nel      ,nuvar    ,uvar     ,matparam ,tt       ,&
         &rho0     ,ngl      ,sigy     ,dpla     ,defp     ,amu      ,&
         &de1      ,de2      ,de3      ,de4      ,de5      ,de6      ,&
         &so1      ,so2      ,so3      ,so4      ,so5      ,so6      ,&
         &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
         &epsd     ,lbuf%dmg ,ssp      ,off      ,idel7nok ,inloc    ,&
         &varnl    ,l_planl  ,lbuf%planl)
!
      elseif (mtn == 187) then !barlat 2000
         call sigeps187(nel   ,npar  ,nuvar ,nfunc ,ifunc ,&
         &npf   ,tf    ,tt    ,dt1   ,uparam,&
         &rho0  ,rho   ,voln  ,eint  ,tempel,ngl   ,&
         &ep1   ,ep2   ,ep3   ,ep4   ,ep5   ,ep6   ,&
         &de1   ,de2   ,de3   ,de4   ,de5   ,de6   ,&
         &es1   ,es2   ,es3   ,es4   ,es5   ,es6   ,&
         &so1   ,so2   ,so3   ,so4   ,so5   ,so6   ,&
         &s1    ,s2    ,s3    ,s4    ,s5    ,s6    ,&
         &sv1   ,sv2   ,sv3   ,sv4   ,sv5   ,sv6   ,&
         &ssp   ,vis   ,uvar  ,off   ,sigy  ,defp  ,&
         &dpla  ,et    ,ipm   ,mat   ,israte,&
         &yldfac,epsp  )
      elseif (mtn == 190) then !path dependent foam (dubois)

         call sigeps190(nel ,nuvar,tt   ,rho ,&
         &ep1 ,ep2 ,ep3 ,ep4  ,ep5  ,ep6 ,&
         &s1  ,s2  ,s3  ,s4   ,s5   ,s6  ,&
         &mfxx,mfxy,mfxz,mfyx ,mfyy ,mfyz,&
         &mfzx ,mfzy,mfzz,&
         &ssp ,vis ,uvar,&
         &matparam%ntable,matparam,nvartmp, vartmp)
!
!----------------------------------------
      endif  ! mtn
!=======================================================================
!                  failure model
!=======================================================================
      if ((itask==0).and.(imon_mat==1))call startime(121,1)
      if (nfail > 0) then
!
         ! failure criterion parameters scaling
         if (inloc > 0) then
            ! -> length used for failure criterion parameters scaling is le_max
            le_max(1:nel) = nloc_dmg%le_max(mat(1))
            el_len => le_max(1:nel)
         else
            ! -> length used for failure criterion parameters scaling is le_max
            el_len => deltax(1:nel)
         endif
!
         if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
! for non-local plastic law
            if (inloc > 0) then
               do i=1,nel
                  dpla(i)  = max(varnl(i),zero)
               enddo
               el_pla => lbuf%planl(1:nel)
            else
! for all plastic law
               do i=1,nel
                  dpla(i)  = defp(i) - defp0(i)
               enddo
               el_pla => defp(1:nel)
            endif
         else
            do i=1,nel
               dpla(i)  = zero
            enddo
         endif
!
         do ir = 1,nfail
!------
            if (mtn == 36.or.mtn == 44.or.mtn == 48.or.mtn == 56.or.&
            &mtn == 60.or.mtn == 76.or.mtn == 104.or.mtn == 112.or.&
            &mtn == 121) then
               do i=1,nel
                  epsp1(i) = epsd(i)
               enddo
            else
               do i=1,nel
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
!------------------------------------------------------------
!     recovering non-local plastic strain-rate
!------------------------------------------------------------
            if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
               if (inloc > 0) then
                  do i = 1,nel
                     epsp1(i) = lbuf%epsdnl(i)
                  enddo
               endif
            endif
!----
            uvarf  => fbuf%floc(ir)%var
            irupt  =  fbuf%floc(ir)%ilawf
            nvarf  =  fbuf%floc(ir)%nvar
            dfmax  => fbuf%floc(ir)%dammx
            damini => fbuf%floc(ir)%damini
            tdel   => fbuf%floc(ir)%tdel
            foff   => fbuf%floc(ir)%off
            dam    => fbuf%floc(ir)%dam
            lf_dammx = fbuf%floc(ir)%lf_dammx
!
            nparf  =  matparam%fail(ir)%nuparam
            niparf =  matparam%fail(ir)%niparam
            uparf=> matparam%fail(ir)%uparam(1:nparf)
            iparf=> matparam%fail(ir)%iparam(1:niparf)
            nfunc  =  matparam%fail(ir)%nfunc
            fld_idx=> fbuf%floc(ir)%indx
            ifunc  => matparam%fail(ir)%ifunc(1:nfunc)
            ntabl_fail =  mat_elem%mat_param(imat)%fail(ir)%ntable
            itabl_fail => mat_elem%mat_param(imat)%fail(ir)%table(1:ntabl_fail)
!
            if (irupt == 1) then
!---- johnson cook
               call fail_johnson(nel ,nparf,nvarf,&
               &tt  ,dt1  ,uparf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &dpla,epsp1,tstar,uvarf,off,&
               &dfmax,tdel )
            elseif (irupt == 2) then
!           tuler butcher
               call fail_tbutcher_s(nel ,nparf,nvarf,&
               &tt  ,dt1  ,uparf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &uvarf,off ,dfmax,tdel )
            elseif (irupt == 3) then
!   wilkins
               call fail_wilkins_s(nel ,nparf,nuvarr,&
               &tt  ,dt1  ,uparf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &dpla,uvarf,off,dfmax,tdel )
            elseif (irupt == 4) then
!   user1
               if (logical_userl_avail) then
                  user_uelr(1:nel)=gbuf%uelr(1:nel)

                  tt_local = tt
                  call eng_userlib_flaw(irupt,nel ,nparf,nuvarr,nfunc,ifunc,&
                  &npf ,tf  ,tt_local  ,dt1  ,uparf,&
                  &ngl ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &s1   ,s2  ,s3   ,s4   ,s5   ,s6  ,&
                  &defp ,dpla,epsp1,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:nel) = user_uelr(1:nel)
               else
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user1 - solid '
                  size=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                  call arret(2)
                  ! ----------------
               endif
            elseif (irupt == 5) then
!   user2
               if (logical_userl_avail) then
                  tt_local = tt
                  user_uelr(1:nel)=gbuf%uelr(1:nel)
                  call eng_userlib_flaw(irupt,nel ,nparf,nuvarr,nfunc,ifunc,&
                  &npf ,tf  ,tt_local  ,dt1  ,uparf,&
                  &ngl ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &s1   ,s2  ,s3   ,s4   ,s5   ,s6  ,&
                  &defp ,dpla,epsp1,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:nel) = user_uelr(1:nel)
               else
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user2 - solid '
                  size=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                  call arret(2)
                  ! ----------------
               endif
            elseif (irupt == 6) then
!   user3
               if (logical_userl_avail) then
                  tt_local = tt
                  user_uelr(1:nel)=gbuf%uelr(1:nel)
                  call eng_userlib_flaw(irupt,nel ,nparf,nuvarr,nfunc,ifunc,&
                  &npf  ,tf  ,tt_local  ,dt1  ,uparf,&
                  &ngl  ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &s1   ,s2  ,s3   ,s4   ,s5   ,s6  ,&
                  &defp ,dpla,epsp1,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:nel) = user_uelr(1:nel)
               else
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user3 - solid '
                  size=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                  call arret(2)
                  ! ----------------
               endif
            elseif (irupt == 7) then
!----- removed to output (h3d)
            elseif (irupt == 8) then
!---- johnson cook + spalling
               call fail_spalling_s(nel ,nparf,nvarf,&
               &tt  ,dt1  ,uparf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6      ,&
               &dpla,epsp1,tstar,uvarf,off         ,&
               &dfmax,tdel ,offg)
            elseif (irupt == 9) then
!---- wierzbicki
               call fail_wierzbicki_s(nel ,nparf,nuvarr,&
               &tt  ,dt1  ,uparf,ngl ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6,&
               &dpla,el_pla,uvarf,off ,dfmax,&
               &tdel )
            elseif (irupt == 10) then
!---- strain tension
               call fail_tensstrain_s(nel ,nparf,nvarf,nfunc,ifunc       ,&
               &npf ,tf  ,tt  ,dt1  ,uparf,&
               &ngl ,deltax ,tstar, ismstr,&
               &es1 ,es2 ,es3 ,es4  ,es5  ,es6     ,&
               &s1  ,s2  ,s3  ,s4   ,s5   ,s6      ,&
               &epsp1,uvarf,off ,dfmax,tdel  ,&
               &mfxx     ,mfxy     ,mfxz     ,mfyx     ,mfyy     , mfyz    ,&
               &mfzx     ,mfzy     ,mfzz     ,lbuf%dmgscl)
            elseif (irupt == 11) then
!---- energy failure
               call fail_energy_s(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,npf      ,&
               &tf       ,tt       ,dt1      ,uparf    ,ngl      ,epsp1    ,&
               &uvarf    ,off      ,dfmax    ,tdel     ,lbuf%dmgscl,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &de1      ,de2      ,de3      ,de4      ,de5      ,de6      )
            elseif (irupt == 13) then
!---- chang - chang
               call fail_changchang_s(&
               &nel      ,nparf    ,nvarf    ,uparf    ,uvarf    ,&
               &tt       ,ipg      ,ilay     ,npg      ,ngl      ,&
               &lbuf%dmgscl,dfmax  ,off      ,lbuf%off ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s6       ,&
               &tdel     ,lf_dammx )
            elseif(irupt == 14)then
! --- hashin failure model
               do i=1,nel
                  epsp1(i) = max(abs(ep1(i)),abs(ep2(i)),abs(ep3(i)),em20)
               enddo
               call fail_hashin_s(&
               &nel      ,nvarf    ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,nparf    ,dfmax    ,tdel     ,epsp1    ,lf_dammx )
            elseif(irupt == 16)then
! --- modified puck failure model
               call fail_puck_s(&
               &nel      ,nvarf    ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,nparf    ,dfmax    ,lf_dammx ,tdel     )
            elseif (irupt == 23) then
!---- tabulated failure model
               call fail_tab_s(&
               &nel      ,nvarf    ,npf      ,tf       ,tt        ,&
               &uparf    ,ngl      ,el_len   ,&
               &s1       ,s2       ,s3       ,s4       ,s5        ,s6        ,&
               &dpla     ,epsp1    ,tstar    ,uvarf    ,ntabl_fail,itabl_fail,&
               &off      ,table    ,dfmax    ,tdel     ,nfunc     ,ifunc     )
            elseif (irupt == 24) then
!   --- orthotropic strain failure
               call fail_orthstrain(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,&
               &npf      ,tf       ,tt       ,dt1      ,uparf    ,ismstr   ,&
               &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6      ,&
               &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,off      ,ipg      ,ngl      ,dfmax    ,tdel     ,&
               &gbuf%uelr,npg      ,deltax   )
            elseif (irupt == 27) then
! ---   extended mohr coulomb failure model
               call fail_emc(&
               &nel      ,nvarf    ,tt       ,&
               &dt1      ,uparf  ,ngl      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6,&
               &el_pla   ,dpla     ,epsp1    ,uvarf ,&
               &off      ,dfmax    ,tdel    )
            elseif (irupt == 29) then
! ---   mit wierzbicki sahraei electric battery failure
               call fail_sahraei_s(&
               &nel      ,nfunc    ,ifunc    ,npf      ,tf       ,&
               &tt       ,ngl      ,uparf    ,&
               &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
               &off      ,dfmax    ,tdel     ,deltax   ,nvarf    ,uvarf    )
            elseif (irupt == 30) then
!  --- biquadratic failure model
               call fail_biquad_s(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,el_len   ,&
               &npf      ,tf       ,tt       ,uparf  ,tdel     ,&
               &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,lbuf%dmgscl,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       )
            elseif (irupt == 33 .and. mtn /= 100 .and. mtn /= 95 ) then
!  --- mullins ogden-roxburgh damage model
               call fail_mullins_or_s(&
               &nel      ,nparf    ,nvarf    ,&
               &tt       ,dt1      ,uparf,ngl ,&
               &s1       ,s2       ,s3       ,s4       ,&
               &s5       ,s6       ,uvarf    ,off      ,&
               &dfmax    ,lbuf%epsa)
            elseif (irupt == 34) then
!  --- cockroft-latham failure model
               call fail_cockroft_s(nel    ,nparf   ,nvarf,&
               &tt      ,dt1       ,uparf ,ngl     ,&
               &ep1     ,ep2       ,ep3      ,ep4      ,ep5           ,ep6 ,&
               &es1     ,es2       ,es3      ,es4      ,es5           ,es6 ,&
               &s1      ,s2        ,s3       ,s4       ,s5            ,s6  ,&
               &el_pla  ,dpla      ,epsp     ,uvarf    ,off           ,&
               &dfmax   ,tdel      )
            elseif (irupt == 36) then
!  --- visual failure model
               call fail_visual_s(&
               &nel     ,nparf     ,nvarf     ,tt      ,dt1      ,uparf ,&
               &es1     ,es2       ,es3      ,es4     ,es5      ,es6     ,&
               &s1      ,s2        ,s3       ,s4      ,s5       ,s6      ,&
               &uvarf   ,off       ,ngl      ,dfmax   ,ismstr   )
            elseif (irupt == 37) then
! ---       tabulated failure model (old, obsolete version)
               call fail_tab_old_s(&
               &nel      ,nvarf    ,npf      ,tf       ,tt       ,&
               &uparf    ,ngl      ,el_len   ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6    ,&
               &el_pla   ,dpla     ,epsp1    ,tstar    ,uvarf    ,&
               &off      ,dfmax    ,tdel     ,&
               &nfunc    ,ifunc    )
!
            elseif (irupt == 38) then
!  --- orthotropic biquadratic failure model
               call fail_orthbiquad_s(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,&
               &npf      ,tf       ,tt       ,dt1      ,uparf    ,&
               &ngl      ,dpla     ,epsp1    ,uvarf    ,off      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &dfmax    ,tdel     ,el_len   )
!
            elseif (irupt == 39) then
!  --- gene1 failure model
               call fail_gene1_s(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,lbuf%off ,&
               &npf      ,tf       ,tt       ,dt1      ,uparf    ,ipg      ,&
               &ngl      ,gbuf%dt  ,epsp1    ,uvarf    ,off      ,npg      ,&
               &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &el_temp  ,voln     ,dfmax    ,tdel     ,deltax   ,table    ,&
               &ir       ,elbuf_tab(ng),ilay ,ntabl_fail,itabl_fail,lf_dammx,&
               &niparf   ,iparf    )
!
            elseif (irupt == 40) then
!  --- rtcl failure model
               call fail_rtcl_s(&
               &nel      ,nparf    ,nvarf    ,tt       ,dt1      ,uparf  ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,tdel     )
!
            elseif (irupt == 41) then
!---- tabulated failure model version 2
               call fail_tab2_s(&
               &nel      ,nparf    ,nvarf    ,nfunc    ,ifunc    ,&
               &npf      ,table    ,tf       ,tt       ,uparf  ,&
               &ngl      ,el_len   ,dpla     ,epsp1    ,uvarf    ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &el_temp  ,off      ,dfmax    ,tdel     ,lbuf%dmgscl,&
               &gbuf%uelr,ipg      ,npg      ,lbuf%off ,ntabl_fail,itabl_fail)
!
            elseif (irupt == 42) then
!---- inievo failure model
               call fail_inievo_s(&
               &nel      ,npar     ,nvarf    ,&
               &table    ,ntabl_fail,itabl_fail,tt       ,uparf  ,&
               &ngl      ,el_len   ,dpla     ,epsp1    ,uvarf    ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &el_pla   ,el_temp  ,sigy     ,off      ,dfmax    ,&
               &tdel     ,lbuf%dmgscl,gbuf%uelr,ipg      ,npg      ,&
               &lbuf%off ,damini   ,gbuf%vol ,inloc    )
!
            elseif (irupt == 43) then
!  --- syazwan failure model
               call fail_syazwan_s(&
               &nel     ,uparf  ,nparf   ,uvarf   ,nvarf   ,&
               &tt      ,ngl      ,ipg     ,dpla    ,tdel    ,&
               &s1      ,s2       ,s3      ,s4      ,s5      ,s6       ,&
               &dfmax   ,nfunc    ,ifunc   ,el_len  ,off     ,&
               &npf     ,tf       ,gbuf%uelr,npg    ,lbuf%off)
            elseif (irupt == 44) then
!---- tsai-wu failure model
               call fail_tsaiwu_s(&
               &nel      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,nparf    ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
            elseif (irupt == 45) then
!---- tsai-hill failure model
               call fail_tsaihill_s(&
               &nel      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,nparf    ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
            elseif (irupt == 46) then
!---- hoffman failure model
               call fail_hoffman_s(&
               &nel      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &uvarf    ,nparf    ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
            elseif (irupt == 47) then
!---- maximum strain failure model
               call fail_maxstrain_s(&
               &nel      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
               &dt1      ,uparf    ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
               &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
               &uvarf    ,nparf    ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
            elseif (irupt == 48) then
!---- orthotropic energy failure
               call fail_orthenerg_s(&
               &nel      ,nparf    ,nvarf    ,uparf  ,uvarf    ,ngl      ,&
               &npg      ,ipg      ,ilay     ,off      ,lbuf%off ,gbuf%noff,&
               &de1      ,de2      ,de3      ,de4      ,de5      ,de6      ,&
               &s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,&
               &tt       ,tdel     ,dfmax    ,deltax   ,lbuf%dmgscl,idel7nok)
!
            endif
         enddo ! ir = 1,nfail
      endif   ! nfail
      if ((itask==0).and.(imon_mat==1))call stoptime(121,1)
!----------------------------------
!     viscous stress (/visc models)
!----------------------------------

      if (matparam%ivisc == 1 .or.&
      &(matparam%ivisc == 2 .and. (ismstr == 10 .or. ismstr == 12))) then
!
         call viscmain(matparam%visc    ,nel     ,&
         &nvarvis ,vbuf%var,rho0    ,vis     ,ssp     ,dt1     ,&
         &ep1     ,ep2     ,ep3     ,ep4     ,ep5     ,ep6     ,&
         &sv1     ,sv2     ,sv3     ,sv4     ,sv5     ,sv6     ,&
         &mfxx    ,mfxy    ,mfxz    ,mfyx    ,mfyy    ,mfyz    ,&
         &mfzx    ,mfzy    ,mfzz    ,&
         &s1      ,s2      ,s3      ,s4      ,s5      ,s6      )
      endif
!
!     viscosity (navier-stokes)
!
      if (isvis > 0) then
         volg(1:nel) = npg*voln(1:nel)
         call nsvisul(nel     ,off     ,rho     ,geo    ,&
         &pid     ,ssp     ,aire    ,volg    ,ep1    ,&
         &ep2     ,ep3     ,ep4     ,ep5     ,ep6    ,&
         &sv1     ,sv2     ,sv3     ,sv4     ,sv5    ,&
         &sv6     ,s3      ,es3     ,rho0    ,rhoref )
      endif
!=======================================================================
!--------------------------------------------------------
!     damaged stresses
!---------------------------------------------------------
      ! -> isotropic stress softening
      if (dmg_flag == 1) then
         do i = 1,nel
            s1(i) = s1(i)*lbuf%dmgscl(i)
            s2(i) = s2(i)*lbuf%dmgscl(i)
            s3(i) = s3(i)*lbuf%dmgscl(i)
            s4(i) = s4(i)*lbuf%dmgscl(i)
            s5(i) = s5(i)*lbuf%dmgscl(i)
            s6(i) = s6(i)*lbuf%dmgscl(i)
         enddo
         ! -> orthotropic stress softening
      elseif (dmg_flag == 6) then
         do i = 1,nel
            s1(i) = s1(i)*lbuf%dmgscl(i+nel*(1-1))
            s2(i) = s2(i)*lbuf%dmgscl(i+nel*(2-1))
            s3(i) = s3(i)*lbuf%dmgscl(i+nel*(3-1))
            s4(i) = s4(i)*lbuf%dmgscl(i+nel*(4-1))
            s5(i) = s5(i)*lbuf%dmgscl(i+nel*(5-1))
            s6(i) = s6(i)*lbuf%dmgscl(i+nel*(6-1))
         enddo
      endif
!---------------------------------------------------------
      if ( mtn==68 ) then
!       orthotropic global nonsymmetric tensor
         if (jcvt == 0) then
#include "vectorize.inc"
            do i=1,nel
               s1(i) = s1(i)*off(i)
               s2(i) = s2(i)*off(i)
               s3(i) = s3(i)*off(i)
               s4(i) = s4(i)*off(i)
               s5(i) = s5(i)*off(i)
               s6(i) = s6(i)*off(i)
               s4(i+nel) = s4(i+nel)*off(i)
               s5(i+nel) = s5(i+nel)*off(i)
               s6(i+nel) = s6(i+nel)*off(i)

               sigl(i,1) = s1(i)
               sigl(i,2) = s2(i)
               sigl(i,3) = s3(i)
               sigl(i,4) = s4(i)
               sigl(i,5) = s5(i)
               sigl(i,6) = s6(i)
               sigdd(i,4) = s4(i+nel)
               sigdd(i,5) = s5(i+nel)
               sigdd(i,6) = s6(i+nel)
! e7 rotation energy
               e7(i) = ep4(i+nel)*(so4(i+nel)+sigdd(i,4))&
               &+ ep5(i+nel)*(so5(i+nel)+sigdd(i,5))&
               &+ ep6(i+nel)*(so6(i+nel)+sigdd(i,6))

               sxy(i) = s4(i)+s4(i+nel)
               syz(i) = s5(i)+s5(i+nel)
               szx(i) = s6(i)+s6(i+nel)
               syx(i) = s4(i)-s4(i+nel)
               szy(i) = s5(i)-s5(i+nel)
               sxz(i) = s6(i)-s6(i+nel)

               sv1(i)= sv1(i)*off(i)
               sv2(i)= sv2(i)*off(i)
               sv3(i)= sv3(i)*off(i)
               sv4(i)= sv4(i)*off(i)
               sv5(i)= sv5(i)*off(i)
               sv6(i)= sv6(i)*off(i)
            enddo
            call mrotensns(1,nel,&
            &s1 ,sxy,sxz,&
            &syx,s2 ,syz,&
            &szx,szy,s3 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
            do i=1,nel
               sigdd(i,1) = (sxy(i)-syx(i))*half
               sigdd(i,2) = (syz(i)-szy(i))*half
               sigdd(i,3) = (szx(i)-sxz(i))*half
               s4(i) = (sxy(i)+syx(i))*half
               s5(i) = (syz(i)+szy(i))*half
               s6(i) = (szx(i)+sxz(i))*half
            enddo
            call mrotens(1,nel,&
            &sv1 ,sv2 ,sv3 ,&
            &sv4 ,sv5 ,sv6 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
         endif
      elseif (isorth /= 0) then
         if (jcvt == 0) then
            do 30 i=1,nel
               sigl(i,1) = s1(i)*off(i)
               sigl(i,2) = s2(i)*off(i)
               sigl(i,3) = s3(i)*off(i)
               sigl(i,4) = s4(i)*off(i)
               sigl(i,5) = s5(i)*off(i)
               sigl(i,6) = s6(i)*off(i)
               sv1(i)= sv1(i)*off(i)
               sv2(i)= sv2(i)*off(i)
               sv3(i)= sv3(i)*off(i)
               sv4(i)= sv4(i)*off(i)
               sv5(i)= sv5(i)*off(i)
               sv6(i)= sv6(i)*off(i)
30          continue
            call mrotens(1,nel,&
            &s1 ,s2 ,s3 ,&
            &s4 ,s5 ,s6 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
            call mrotens(1,nel,&
            &sv1 ,sv2 ,sv3 ,&
            &sv4 ,sv5 ,sv6 ,&
            &r11,r21,r31,&
            &r12,r22,r32,&
            &r13,r23,r33)
         else
!        jcvt > 0
            do i=1,nel
               sv1(i)= sv1(i)*off(i)
               sv2(i)= sv2(i)*off(i)
               sv3(i)= sv3(i)*off(i)
               sv4(i)= sv4(i)*off(i)
               sv5(i)= sv5(i)*off(i)
               sv6(i)= sv6(i)*off(i)
            enddo
         endif
      endif
!------------------------------------------------------------
      do i=1,nel
         sig(i,1) = s1(i)*off(i)
         sig(i,2) = s2(i)*off(i)
         sig(i,3) = s3(i)*off(i)
         sig(i,4) = s4(i)*off(i)
         sig(i,5) = s5(i)*off(i)
         sig(i,6) = s6(i)*off(i)
         svis(i,1)= sv1(i)*off(i)
         svis(i,2)= sv2(i)*off(i)
         svis(i,3)= sv3(i)*off(i)
         svis(i,4)= sv4(i)*off(i)
         svis(i,5)= sv5(i)*off(i)
         svis(i,6)= sv6(i)*off(i)
      enddo
!
!  isvis for outp
!
      if (isvis > 0) then
         do i=1,nel
            sigv(i,1) = svis(i,1)
            sigv(i,2) = svis(i,2)
            sigv(i,3) = svis(i,3)
            sigv(i,4) = svis(i,4)
            sigv(i,5) = svis(i,5)
            sigv(i,6) = svis(i,6)
         enddo
      endif
      if (matparam%ivisc > 0) then
         do i=1,nel
            svisc(i,1) = svis(i,1)
            svisc(i,2) = svis(i,2)
            svisc(i,3) = svis(i,3)
            svisc(i,4) = svis(i,4)
            svisc(i,5) = svis(i,5)
            svisc(i,6) = svis(i,6)
         enddo
      endif
!------------------------------------------------------------
!     variable to regularize with non-local
!------------------------------------------------------------
      if ((elbuf_tab(ng)%bufly(ilay)%l_pla > 0).and.(mtn /= 126)) then
         if (inloc > 0) then
            do i=1,nel
              if (off(i) == one) then 
                varnl(i) = defp(i)
              else
                varnl(i) = zero
              endif
            enddo
         endif
      endif
!------------------------------------------------------------
!     define sound speed  (in all case)
!     define dynamic viscosity (for viscous law)
!-----------------------
      do i=1,nel
         if (ssp(i) == zero) ssp(i)=sqrt(c1(i)/rho0(i))
      enddo
!-------------------------------------------
!   bulk viscosity and time step computation
!   this subroutine return the new bulk viscosity q
!-----------------------
      if (jsph == 0) then
!
         call mqviscb(&
         &pm,       off,      rho,      bid1,&
         &el_temp,  ssp,      bid3,     stifn,&
         &dt2t,     neltst,   ityptst,  aire,&
         &offg,     geo,      pid,      voln,&
         &vd2,      deltax,   vis,      d1,&
         &d2,       d3,       pnew,     psh,&
         &mat,      ngl,      q,        ssp_eq,&
         &vol,      mssa,     dmels,    igeo,&
         &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
         &ipm,      rhoref,   rhosp,    nel,&
         &ity,      ismstr,   jtur,     jthe,&
         &jsms,     npg )
!------------------------------------------------
      else
         call mdtsph(&
         &pm,       off,      rho,      bid1,&
         &el_temp,  bid3,     stifn,    dt2t,&
         &neltst,   ityptst,  offg,     geo,&
         &pid,      mumax,    ssp,      voln,&
         &vd2,      deltax,   vis,      d1,&
         &d2,       d3,       pnew,     psh,&
         &mat,      ngl,      q,        ssp_eq,&
         &gbuf%g_dt,gbuf%dt,  nel,      ity,&
         &jtur,     jthe)
      endif
!-----------
      if (mtn == 77 ) call fmqviscb(&
      &nel,                  pm,                   geo,&
      &pid,                  mat,                  ngl,                  neltst,&
      &ityptst,              dt2t,                 uvar,                 fssp,&
      &off,                  offg,                 aire,                 fdeltax,&
      &vis,                  fvd2,                 fqvis,                ity,&
      &ismstr)
!-----------
      if (mtn == 67) then
         do i=1,nel
            eint(i)=eint(i)-(q(i)+qold(i))*dvol(i)*half
         end do
      elseif (mtn == 24) then
         do i=1,nel
            e1 = d1(i)*(sold1(i)+sig(i,1) + svis(i,1))
            e2 = d2(i)*(sold2(i)+sig(i,2) + svis(i,2))
            e3 = d3(i)*(sold3(i)+sig(i,3) + svis(i,3))
            e4 = d4(i)*(sold4(i)+sig(i,4) + svis(i,4))
            e5 = d5(i)*(sold5(i)+sig(i,5) + svis(i,5))
            e6 = d6(i)*(sold6(i)+sig(i,6) + svis(i,6))
            eint(i) = eint(i)&
            &- (q(i)+qold(i))*dvol(i)*half&
            &+ (e1+e2+e3+e4+e5+e6)*vol_avg(i)*dt1*half
         end do
      elseif (mtn == 77 ) then
         do i=1,nel
            p2 = -(sig(i,1) + sig(i,2)+sig(i,3)) * third - pair(i)
            e1 = d1(i)*(sig(i,1) + pair(i) +p2+svis(i,1))
            e2 = d2(i)*(sig(i,2) + pair(i) +p2+svis(i,2))
            e3 = d3(i)*(sig(i,3) + pair(i) +p2+svis(i,3))
            e4 = d4(i)*(sig(i,4) ) + svis(i,4)
            e5 = d5(i)*(sig(i,5) ) + svis(i,5)
            e6 = d6(i)*(sig(i,6) ) + svis(i,6)
            eint(i)=eint(i)-(q(i)+qold(i)+p2)*dvol(i)+(e1+e2+e3+e4+e5+e6+e7(i))*vol_avg(i)*dt1
         end do
      elseif (mtn == 51 ) then
         do i=1,nel
            p2 = -(sold1(i)+sig(i,1)+sold2(i)+sig(i,2)+sold3(i)+sig(i,3))* third
            e1 = d1(i)*(sold1(i)+sig(i,1)+p2 + two*svis(i,1))
            e2 = d2(i)*(sold2(i)+sig(i,2)+p2 + two*svis(i,2))
            e3 = d3(i)*(sold3(i)+sig(i,3)+p2 + two*svis(i,3))
            e4 = d4(i)*(sold4(i)+sig(i,4)    + two*svis(i,4))
            e5 = d5(i)*(sold5(i)+sig(i,5)    + two*svis(i,5))
            e6 = d6(i)*(sold6(i)+sig(i,6)    + two*svis(i,6))
            eint(i)=eint(i)+(e1+e2+e3+e4+e5+e6)*vol_avg(i)*dt1*half
         end do
      elseif (mtn == 102) then
         !case of material law using /eos
         do i=1,nel
            p2  = -(sold1(i)+sig(i,1)+sold2(i)+sig(i,2)+sold3(i)+sig(i,3))* third
            pold= -(sold1(i)+sold2(i)+sold3(i))* third
            e1  =  d1(i)*(sold1(i)+sig(i,1) + p2+two*svis(i,1))
            e2  =  d2(i)*(sold2(i)+sig(i,2) + p2+two*svis(i,2))
            e3  =  d3(i)*(sold3(i)+sig(i,3) + p2+two*svis(i,3))
            e4  =  d4(i)*(sold4(i)+sig(i,4) + two*svis(i,4))
            e5  =  d5(i)*(sold5(i)+sig(i,5) + two*svis(i,5))
            e6  =  d6(i)*(sold6(i)+sig(i,6) + two*svis(i,6))
            eint(i)=eint(i)-&
            &(q(i)+qold(i)+pold)*dvol(i)*half +&
            &(e1+e2+e3+e4+e5+e6+e7(i))*vol_avg(i)*dt1*half
         end do
      else
         !other material laws without /eos
#include "vectorize.inc"
         do i=1,nel
            p2 = -(sold1(i)+sig(i,1)+sold2(i)+sig(i,2)+sold3(i)+sig(i,3))* third
            e1 = d1(i)*(sold1(i)+sig(i,1) + p2+two*svis(i,1))
            e2 = d2(i)*(sold2(i)+sig(i,2) + p2+two*svis(i,2))
            e3 = d3(i)*(sold3(i)+sig(i,3) + p2+two*svis(i,3))
            e4 = d4(i)*(sold4(i)+sig(i,4) + two*svis(i,4))
            e5 = d5(i)*(sold5(i)+sig(i,5) + two*svis(i,5))
            e6 = d6(i)*(sold6(i)+sig(i,6) + two*svis(i,6))
            einc(i) = off(i) * (vol_avg(i)*dt1 * (e1+e2+e3+e4+e5+e6+e7(i))&
            &- dvol(i)*(q(i) + qold(i) + p2)) * half
         end do
         do i = 1, nel
            eint(i) = eint(i) + einc(i)
         end do

      endif
      qold(1:nel) = q(1:nel)
      if (allocated(bufzero)) deallocate (bufzero)
!------------------------------------------
      return
   end
!-----
end module mulaw_mod
