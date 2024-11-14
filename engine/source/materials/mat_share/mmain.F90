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
      !||====================================================================
      !||    mmain_mod    ../engine/source/materials/mat_share/mmain.F90
      !||--- called by ------------------------------------------------------
      !||    bforc2       ../engine/source/ale/bimat/bforc2.F
      !||    ig3duforc3   ../engine/source/elements/ige3d/ig3duforc3.F
      !||    q4forc2      ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2       ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    s10forc3     ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3     ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3     ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3      ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3     ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3     ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3     ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8sforc3     ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3     ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3      ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3       ../engine/source/elements/solid/solide/sforc3.F
      !||    spstres      ../engine/source/elements/sph/spstres.F
      !||    szforc3      ../engine/source/elements/solid/solidez/szforc3.F
      !||====================================================================
      module mmain_mod
      contains
! ======================================================================================================================
!                                                   mmain
! ======================================================================================================================
!! \brief main routine for Material Computation for brick/quad/thickshell/sph elements
      !||====================================================================
      !||    mmain                  ../engine/source/materials/mat_share/mmain.F90
      !||--- called by ------------------------------------------------------
      !||    bforc2                 ../engine/source/ale/bimat/bforc2.F
      !||    ig3duforc3             ../engine/source/elements/ige3d/ig3duforc3.F
      !||    q4forc2                ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                 ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    s10forc3               ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3               ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3               ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3                ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3               ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3               ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3               ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8sforc3               ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3               ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3                ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3                 ../engine/source/elements/solid/solide/sforc3.F
      !||    spstres                ../engine/source/elements/sph/spstres.F
      !||    szforc3                ../engine/source/elements/solid/solidez/szforc3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||    atur17                 ../engine/source/ale/turbulence/atur17.F
      !||    aturbn                 ../engine/source/ale/turbulence/aturbn.F
      !||    eosmain                ../common_source/eos/eosmain.F
      !||    eosupda                ../common_source/eos/eosupda.F
      !||    fail_biquad_s          ../engine/source/materials/fail/biquad/fail_biquad_s.F
      !||    fail_changchang_s      ../engine/source/materials/fail/changchang/fail_changchang_s.F
      !||    fail_cockroft_s        ../engine/source/materials/fail/cockroft_latham/fail_cockroft_s.F
      !||    fail_emc               ../engine/source/materials/fail/emc/fail_emc.F
      !||    fail_energy_s          ../engine/source/materials/fail/energy/fail_energy_s.F
      !||    fail_gene1_s           ../engine/source/materials/fail/gene1/fail_gene1_s.F
      !||    fail_hashin_s          ../engine/source/materials/fail/hashin/fail_hashin_s.F
      !||    fail_hoffman_s         ../engine/source/materials/fail/hoffman/fail_hoffman_s.F
      !||    fail_inievo_s          ../engine/source/materials/fail/inievo/fail_inievo_s.F
      !||    fail_johnson           ../engine/source/materials/fail/johnson_cook/fail_johnson.F
      !||    fail_ladeveze          ../engine/source/materials/fail/ladeveze/fail_ladeveze.F
      !||    fail_maxstrain_s       ../engine/source/materials/fail/max_strain/fail_maxstrain_s.F
      !||    fail_orthbiquad_s      ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_s.F
      !||    fail_orthenerg_s       ../engine/source/materials/fail/orthenerg/fail_orthenerg_s.F
      !||    fail_orthstrain        ../engine/source/materials/fail/orthstrain/fail_orthstrain_s.F
      !||    fail_puck_s            ../engine/source/materials/fail/puck/fail_puck_s.F
      !||    fail_rtcl_s            ../engine/source/materials/fail/rtcl/fail_rtcl_s.F
      !||    fail_sahraei_s         ../engine/source/materials/fail/sahraei/fail_sahraei_s.F
      !||    fail_spalling_s        ../engine/source/materials/fail/spalling/fail_spalling_s.F
      !||    fail_syazwan_s         ../engine/source/materials/fail/syazwan/fail_syazwan_s.F
      !||    fail_tab2_s            ../engine/source/materials/fail/tabulated/fail_tab2_s.F
      !||    fail_tab_old_s         ../engine/source/materials/fail/tabulated/fail_tab_old_s.F
      !||    fail_tab_s             ../engine/source/materials/fail/tabulated/fail_tab_s.F
      !||    fail_tbutcher_s        ../engine/source/materials/fail/tuler_butcher/fail_tbutcher_s.F
      !||    fail_tensstrain_s      ../engine/source/materials/fail/tensstrain/fail_tensstrain_s.F
      !||    fail_tsaihill_s        ../engine/source/materials/fail/tsaihill/fail_tsaihill_s.F
      !||    fail_tsaiwu_s          ../engine/source/materials/fail/tsaiwu/fail_tsaiwu_s.F
      !||    fail_visual_s          ../engine/source/materials/fail/visual/fail_visual_s.F
      !||    fail_wierzbicki_s      ../engine/source/materials/fail/wierzbicki/fail_wierzbicki_s.F
      !||    fail_wilkins_s         ../engine/source/materials/fail/wilkins/fail_wilkins_s.F
      !||    finter                 ../engine/source/tools/curve/finter.F
      !||    gray10                 ../engine/source/materials/mat/mat016/gray10.F
      !||    gray20                 ../engine/source/materials/mat/mat016/gray20.F
      !||    gray30                 ../engine/source/materials/mat/mat016/gray30.F
      !||    m10law                 ../engine/source/materials/mat/mat010/m10law.F
      !||    m11law                 ../engine/source/materials/mat/mat011/m11law.F
      !||    m12law                 ../engine/source/materials/mat/mat012/m12law.F
      !||    m13law                 ../engine/source/materials/mat/mat013/m13law.F
      !||    m14law                 ../engine/source/materials/mat/mat014/m14law.F
      !||    m16law                 ../engine/source/materials/mat/mat016/m16law.F
      !||    m17law                 ../engine/source/materials/mat/mat017/m17law.F
      !||    m18law                 ../engine/source/materials/mat/mat018/m18law.F
      !||    m1law                  ../engine/source/materials/mat/mat001/m1law.F
      !||    m1lawi                 ../engine/source/materials/mat/mat001/m1lawi.F
      !||    m1lawtot               ../engine/source/materials/mat/mat001/m1lawtot.F
      !||    m21law                 ../engine/source/materials/mat/mat021/m21law.F
      !||    m22law                 ../engine/source/materials/mat/mat022/m22law.F
      !||    m24law                 ../engine/source/materials/mat/mat024/m24law.F
      !||    m25law                 ../engine/source/materials/mat/mat025/m25law.F
      !||    m26law                 ../engine/source/materials/mat/mat026/m26law.F
      !||    m2law                  ../engine/source/materials/mat/mat002/m2law.F
      !||    m3law                  ../engine/source/materials/mat/mat003/m3law.F
      !||    m46law                 ../engine/source/materials/mat/mat046/m46law.F
      !||    m49law                 ../engine/source/materials/mat/mat049/m49law.F
      !||    m4law                  ../engine/source/materials/mat/mat004/m4law.F
      !||    m5law                  ../engine/source/materials/mat/mat005/m5law.F
      !||    m6law                  ../engine/source/materials/mat/mat006/m6law.F
      !||    mdtsph                 ../engine/source/materials/mat_share/mdtsph.F
      !||    meint                  ../engine/source/materials/mat_share/meint.F
      !||    mjwl                   ../engine/source/materials/mat/mat005/mjwl.F
      !||    mnsvis                 ../engine/source/materials/mat_share/mnsvis.F
      !||    mqvisc26               ../engine/source/materials/mat_share/mqvisc26.F
      !||    mqviscb                ../engine/source/materials/mat_share/mqviscb.F
      !||    mreploc                ../engine/source/materials/mat_share/mreploc.F
      !||    mrotens                ../engine/source/materials/mat_share/mrotens.F
      !||    mtheta                 ../engine/source/materials/mat_share/mtheta.F
      !||    mulaw                  ../engine/source/materials/mat_share/mulaw.F90
      !||    nvar                   ../engine/source/input/nvar.F
      !||    put_etfac              ../engine/source/elements/solid/solide8z/put_etfac.F
      !||    putsignor3             ../engine/source/elements/solid/solide8z/putsignor3.F
      !||    sboltlaw               ../engine/source/elements/solid/solide/sboltlaw.F
      !||    sesa10                 ../engine/source/materials/mat/mat026/sesa10.F
      !||    sesa20                 ../engine/source/materials/mat/mat026/sesa20.F
      !||    sesa30                 ../engine/source/materials/mat/mat026/sesa30.F
      !||    startime               ../engine/source/system/timer.F
      !||    stoptime               ../engine/source/system/timer.F
      !||    usermat_solid          ../engine/source/materials/mat_share/usermat_solid.F
      !||    viscmain               ../engine/source/materials/visc/viscmain.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
      !||    ale_mod                ../common_source/modules/ale/ale_mod.F
      !||    anim_mod               ../common_source/modules/output/anim_mod.F
      !||    constant_mod           ../common_source/modules/constant_mod.F
      !||    dt_mod                 ../engine/source/modules/dt_mod.F
      !||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    mulaw_mod              ../engine/source/materials/mat_share/mulaw.F90
      !||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
      !||    table_mod              ../engine/share/modules/table_mod.F
      !||====================================================================
        subroutine mmain(&
        &elbuf_tab,   ng,          pm,          geo,&
        &ale_connect, ix,          iparg,&
        &v,           tf,          npf,         bufmat,&
        &stifn,       x,           dt2t,        neltst,&
        &ityptst,     offset,      nel,         w,&
        &off,         pid,         mat,         ngl,&
        &voln,        vd2,         dvol,        deltax,&
        &vis,         qvis,        cxx,         s1,&
        &s2,          s3,          s4,          s5,&
        &s6,          dxx,         dyy,         dzz,&
        &d4,          d5,          d6,          wxx,&
        &wyy,         wzz,         rx,          ry,&
        &rz,          sx,          sy,          sz,&
        &vdx,         vdy,         vdz,         mumax,&
        &ssp_eq,      aire,        sigy,        et,&
        &bufvois,     defp,        r3_dam,      r4_amu,&
        &mfxx,        mfxy,        mfxz,        mfyx,&
        &mfyy,        mfyz,        mfzx,        mfzy,&
        &mfzz,        ipm,         gama,        fr_wav,&
        &dxy,         dyx,         dyz,         dzy,&
        &dzx,         dxz,         istrain,     tempel,&
        &die,         iexpan,      ilay,        mssa,&
        &dmels,       iptr,        ipts,        iptt,&
        &table,       fvd2,        fdeltax,     fssp,&
        &fqvis,       iparg1,      igeo,        conde,&
        &itask,       nloc_dmg,    varnl,       mat_elem,&
        &h3d_strain,  jplasol,     jsph,        sz_bufvois,   &
        &snpc,        stf,         sbufmat,     glob_therm,   &
        &svis,        sz_ix,       iresp,                     &
        &n2d,         th_strain,   ngroup,      tt,&
        &dt1,         ntable,      numelq,      nummat,&
        &numgeo,      numnod,      numels,            &
        &idel7nok,    idtmin,      maxfunc    ,        &
        &imon_mat,    userl_avail, impl_s,&
        &idyna,       dt,          fheat    ,   opt_mtn,     opt_jcvt,&
        &opt_isorth,  opt_isorthg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use table_mod
          use mat_elem_mod
          use nlocal_reg_mod
          use ale_connectivity_mod
          use message_mod
          use anim_mod
          use ale_mod
          use mulaw_mod
          use constant_mod
          use dt_mod
          use glob_therm_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   g l o b a l   p a r a m e t e r s
!-----------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
#include "elements.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, optional, intent(in) :: opt_mtn, opt_jcvt, opt_isorth,  opt_isorthg
          my_real, intent(in) :: dt1
          my_real, intent(in) :: tt
          integer,dimension(102) :: idtmin
          integer, intent(inout) :: idel7nok
          integer, intent(in) :: maxfunc
          integer, intent(in) :: ntable
          integer, intent(in) :: numelq
          integer, intent(in) :: nummat
          integer, intent(in) :: numgeo
          integer, intent(in) :: numnod
          integer, intent(in) :: numels
          integer, intent(in) :: n2d
          integer, intent(in) :: th_strain
          integer, intent(in) :: ngroup
          integer, intent(in) :: sz_bufvois
          integer, intent(inout) :: jsph
          integer, intent(inout) :: jplasol
          integer, intent(in) :: nel
          integer, intent(in) :: snpc
          integer, intent(in) :: stf
          integer, intent(in) :: sbufmat
          integer, intent(in) ::  sz_ix
          integer, intent(in) :: iresp
          integer, intent(in) :: imon_mat
          integer, intent(in) :: userl_avail
          integer, intent(in) :: impl_s
          integer, intent(in) :: idyna
          integer, dimension(n_var_iparg,ngroup),intent(in) :: iparg
          integer, dimension(snpc),intent(in)      :: npf
          integer,dimension(mvsiz),intent(in)         :: mat
          integer,dimension(nixs,sz_ix),intent(in) :: ix
          integer,dimension(mvsiz),intent(inout)        :: pid
          integer,dimension(mvsiz),intent(inout)        :: ngl
          integer,dimension(n_var_ipm,nummat),intent(in)  :: ipm
          integer,dimension(n_var_igeo,numgeo),intent(in)  :: igeo
          integer,intent(in) :: neltst
          integer,intent(in) :: ityptst
          integer,intent(in) :: istrain
          integer,intent(in) :: iexpan
          integer,intent(in) :: ng
          integer,intent(inout) :: ilay
          integer,intent(in) :: iptr
          integer,intent(in) :: ipts
          integer,intent(in) :: iptt
          integer,intent(in) :: itask
          integer,intent(in) :: h3d_strain
          integer,intent(in) :: offset
          integer,dimension(n_var_iparg),intent(in) :: iparg1
          !
          my_real,intent(in) :: dt2t
          my_real, dimension(mvsiz,6), intent(inout)  :: svis
          my_real, dimension(n_var_geo,numgeo), intent(inout) :: geo
          my_real, dimension(n_var_pm,nummat), intent(inout) :: pm
          my_real, dimension(3,numnod), intent(in) :: v
          my_real, dimension(3,numnod), intent(in) :: x
          my_real, dimension(3,numnod), intent(in) :: w
          my_real, dimension(numels), intent(inout) :: mssa
          my_real, dimension(numels), intent(inout) :: dmels
          my_real, dimension(stf), intent(inout) :: tf
          my_real, dimension(sz_bufvois), intent(inout) :: bufvois
          my_real,dimension(sbufmat),target,intent(inout) ::bufmat

          my_real, dimension(nel), intent(inout) :: varnl
          my_real, dimension(nel), intent(inout) :: defp

          my_real, dimension(mvsiz), intent(inout) :: sigy
          my_real, dimension(mvsiz), intent(inout) :: et
          my_real, dimension(mvsiz), intent(inout) :: off
          my_real, dimension(mvsiz), intent(inout) :: stifn
          my_real, dimension(mvsiz,6), intent(inout) :: gama
          my_real, dimension(mvsiz), intent(inout) :: vd2
          my_real, dimension(mvsiz), intent(inout) :: deltax
          my_real, dimension(mvsiz), intent(inout) :: vis
          my_real, dimension(mvsiz), intent(inout) :: wxx
          my_real, dimension(mvsiz), intent(inout) :: wyy
          my_real, dimension(mvsiz), intent(inout) :: wzz
          my_real, dimension(mvsiz), intent(inout) :: vdx
          my_real, dimension(mvsiz), intent(inout) :: vdy
          my_real, dimension(mvsiz), intent(inout) :: vdz
          my_real, dimension(mvsiz), intent(inout) :: mumax
          my_real, dimension(mvsiz), intent(inout) :: ssp_eq
          my_real, dimension(mvsiz), intent(inout) :: aire
          my_real, dimension(mvsiz), intent(inout) :: cxx
          my_real, dimension(mvsiz), intent(inout) :: voln
          my_real, dimension(mvsiz), intent(inout) :: fr_wav
          my_real, dimension(mvsiz), intent(inout) :: fqvis
          my_real, dimension(mvsiz), intent(inout) :: fvd2
          my_real, dimension(mvsiz), intent(inout) :: fssp
          my_real, dimension(mvsiz), intent(inout) :: fdeltax
          my_real, dimension(mvsiz), intent(inout) :: conde
          my_real, dimension(mvsiz), intent(inout) :: s1
          my_real, dimension(mvsiz), intent(inout) :: s2
          my_real, dimension(mvsiz), intent(inout) :: s3
          my_real, dimension(mvsiz), intent(inout) :: s4
          my_real, dimension(mvsiz), intent(inout) :: s5
          my_real, dimension(mvsiz), intent(inout) :: s6
          my_real, dimension(mvsiz), intent(inout) :: d4
          my_real, dimension(mvsiz), intent(inout) :: d5
          my_real, dimension(mvsiz), intent(inout) :: d6
          my_real, dimension(mvsiz), intent(inout) :: dvol
          my_real, dimension(mvsiz), intent(inout) :: qvis
          my_real, dimension(mvsiz), intent(inout) :: dxx
          my_real, dimension(mvsiz), intent(inout) :: dyy
          my_real, dimension(mvsiz), intent(inout) :: dzz
          my_real, dimension(mvsiz), intent(inout) :: rx
          my_real, dimension(mvsiz), intent(inout) :: ry
          my_real, dimension(mvsiz), intent(inout) :: rz
          my_real, dimension(mvsiz), intent(inout) :: sx
          my_real, dimension(mvsiz), intent(inout) :: sy
          my_real, dimension(mvsiz), intent(inout) :: sz
          my_real, dimension(mvsiz), intent(inout) :: dxy
          my_real, dimension(mvsiz), intent(inout) :: dyx
          my_real, dimension(mvsiz), intent(inout) :: dyz
          my_real, dimension(mvsiz), intent(inout) :: dzy
          my_real, dimension(mvsiz), intent(inout) :: dzx
          my_real, dimension(mvsiz), intent(inout) :: dxz
          my_real, dimension(mvsiz), intent(inout) :: mfxx
          my_real, dimension(mvsiz), intent(inout) :: mfxy
          my_real, dimension(mvsiz), intent(inout) :: mfxz
          my_real, dimension(mvsiz), intent(inout) :: mfyx
          my_real, dimension(mvsiz), intent(inout) :: mfyy
          my_real, dimension(mvsiz), intent(inout) :: mfyz
          my_real, dimension(mvsiz), intent(inout) :: mfzx
          my_real, dimension(mvsiz), intent(inout) :: mfzy
          my_real, dimension(mvsiz), intent(inout) :: mfzz
          my_real, dimension(mvsiz), intent(inout) :: tempel
          my_real, dimension(mvsiz), intent(inout) :: die
          my_real, dimension(mvsiz), intent(inout) :: r3_dam
          my_real, dimension(mvsiz), intent(inout) :: r4_amu
          my_real, dimension(mvsiz), intent(inout) :: fheat
          target :: varnl,defp,tempel

          type(ttable) table(*)
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
          type (nlocal_str_) :: nloc_dmg
          type (t_ale_connectivity), intent(in) :: ale_connect
          type (dt_), intent(in) :: dt
          type (mat_elem_)   ,intent(inout) :: mat_elem
          type (glob_therm_) ,intent(inout) :: glob_therm
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer ibidon1,ibidon2,ibidon3,ibidon4                     ! Dummy arguments
          integer i,ipg,nuvar,nparam,nfunc,ifunc_alpha,ilaw,imat,&
          &        nvarf,nfail,ntabl_fail,ir,irupt,ivisc,eostyp,npts,nptt,nptr,npg,  &
          &        isvis,mx,nvartmp,nlay,inloc,iselect,ibpreld,nvareos,nvarvis
          integer ifunc(maxfunc)

          ! Float/Double
          my_real facq0,e1,e2,e3,e4,e5,e6,alpha,tref,tmelt
          my_real pnew(mvsiz)
          my_real psh(mvsiz)
          my_real p0(mvsiz)
          my_real off_old(mvsiz),amu(mvsiz), amu2(mvsiz), c1(mvsiz), c2(mvsiz), &
          &       c3(mvsiz), c4(mvsiz), c5(mvsiz),c6(mvsiz),                    &
          &       einc(mvsiz), rho0(mvsiz),vol_avg(mvsiz),                      &
          &       df(mvsiz), pc(mvsiz),espe(mvsiz),tmu(mvsiz),                  &
          &       tx(mvsiz)    ,ty(mvsiz)    ,tz(mvsiz) 

          my_real ss1(mvsiz),ss2(mvsiz), ss3(mvsiz),ss4(mvsiz),ss5(mvsiz),ss6(mvsiz)
          my_real r11(mvsiz),r12(mvsiz),r13(mvsiz),r21(mvsiz),r22(mvsiz),r23(mvsiz),r31(mvsiz),r32(mvsiz),r33(mvsiz)
          my_real dpla(mvsiz),tstar(mvsiz),epsp(mvsiz),xk(mvsiz),                  &
          &       tempel0(mvsiz), fscal_alpha , sigl(mvsiz,6)
          my_real es1(mvsiz), es2(mvsiz),  es3(mvsiz),  es4(mvsiz),  es5(mvsiz),   &
          &       es6(mvsiz), eint(mvsiz), dpdm(mvsiz), dpde(mvsiz),ecold(mvsiz),  &
          &       vol(mvsiz),al_imp(mvsiz),signor(mvsiz,6)

          my_real ep1(mvsiz),ep2(mvsiz),ep3(mvsiz),ep4(mvsiz),ep5(mvsiz),ep6(mvsiz)
          my_real sv1(mvsiz),sv2(mvsiz),sv3(mvsiz),sv4(mvsiz),sv5(mvsiz),sv6(mvsiz)

          my_real bidon1,bidon4,bidon5,bid1(mvsiz), bid2(mvsiz),eth(mvsiz),        &
          &       stor1, stor2, stor3, stor4, stor5, stor6,&
          &       sigkk(mvsiz), epsth(mvsiz), rhoref(mvsiz), rhosp(mvsiz),user_uelr(mvsiz)
          my_real alogey(mvsiz), pold(mvsiz),                                      &
          &       p01(mvsiz), p02(mvsiz), e01(mvsiz), e02(mvsiz),                  &
          &       er1v(mvsiz), er2v(mvsiz), wdr1v(mvsiz), wdr2v(mvsiz),w1(mvsiz),  &
          &       heat_meca_l,volg(mvsiz),de1(nel),de2(nel),de3(nel),de4(nel),     &
          &       de5(nel),de6(nel)

          my_real q1,q2,q3,str1,str2,str3,str4,str5,str6,wxxf,wyyf,wzzf

          my_real, dimension(nel), target :: le_max
          integer, dimension(:) ,pointer  :: itabl_fail
          my_real, dimension(:) ,pointer  :: strd1,strd2,el_len,el_pla
          logical :: logical_userl_avail, l_mulaw_called
!
          character option*256
          integer length
          target :: deltax
!--------------------------------------------
          my_real, external :: finter
!--------------------------------------------
          type(l_bufel_)  ,pointer :: lbuf
          type(g_bufel_)  ,pointer :: gbuf
          type(buf_mat_)  ,pointer :: mbuf
          type(buf_fail_) ,pointer :: fbuf
          type(buf_visc_) ,pointer :: vbuf
          type(buf_eos_)  ,pointer :: ebuf
          my_real,&
          &dimension(:), pointer  :: uvarf,uparamf,dfmax,tdel,damini
          my_real ,dimension(:), pointer  :: el_temp
!     bolt preloading
          integer iboltp, nbpreld
          my_real,&
          &dimension(:), pointer  :: bpreld
          integer                  :: dmg_flag
          integer :: lft,mtn,llt,nft,iad,ity,npt,jale,ismstr,&
          &jeul,jtur,jthe,jlag,mid,jmult,jhbe,jivf,jpor,jpla,jclose,&
          &irep,iint,igtyp,jcvt,isrot,israt,isorth,isorthg,icsen,ifailure,&
          &jsms, ihet, ipartsph,lf_dammx,niparam
          integer, dimension(:) ,pointer  :: iparamf
!-----------------------------------------------
!   s o u r c e   l i n e s
!-----------------------------------------------
          logical_userl_avail = .false.
          l_mulaw_called=.false.
          if(userl_avail/=0) logical_userl_avail = .true.

          lft = 1
          if(present(opt_mtn)) then
            mtn     = opt_mtn
          else
            mtn     = iparg(1,ng)
          endif
          llt     = iparg(2,ng)
          nft     = iparg(3,ng)
          iad     = iparg(4,ng)
          ity     = iparg(5,ng)
          npt     = iparg(6,ng)
          jale    = iparg(7,ng)
          ismstr  = iparg(9,ng)
          if(ity==1.or.ity==2)then
            jeul    = iparg(11,ng)
          elseif (mtn == 151 .and. ity == 7) then
            jeul    = iparg(11,ng)
          else             ! attention dkt9_s3 utilise ce flag
            jeul    = 0
          end if
          jtur    = iparg(12,ng)
          jthe    = iparg(13,ng)
          jlag    = iparg(14,ng)
          mid     = iparg(18,ng)
          jmult   = iparg(20,ng)
          jhbe    = iparg(23,ng)
          jivf    = iparg(24,ng)
          jpor    = iparg(27,ng)
          jpla    = iparg(29,ng)
          jclose  = iparg(33,ng)
          irep    = iparg(35,ng)
          iint    = iparg(36,ng)
          ihet    = 0
          if(jhbe == 24) ihet = iint

          if(present(opt_jcvt)) then
            jcvt  = opt_jcvt
          else
            jcvt  = iparg(37,ng)
          endif
          igtyp   = iparg(38,ng)
          icsen   = iparg(39,ng)
          israt   = iparg(40,ng)
          isrot   = iparg(41,ng)
          if(iparg(28,ng) == 10) isrot = iparg(74,ng)
          isorth  = iparg(42,ng)
          isorthg = isorth
          if(present(opt_isorth)) isorth = opt_isorth
          if(present(opt_isorthg)) isorthg = opt_isorthg

          ifailure = iparg(43,ng)
          jsms = iparg(52,ng)
          ipartsph = iparg(69,ng)
          
          fheat(:) = zero

! compatibility with deprecated law 20
          if(ilay < 0) then
            jmult = - ilay
            ilay =  - ilay
            if(jmult == 1) then
              mtn = iparg(25,ng)
            else
              mtn = iparg(26,ng)
            endif
          endif




          gbuf => elbuf_tab(ng)%gbuf
          lbuf => elbuf_tab(ng)%bufly(ilay)%lbuf(iptr,ipts,iptt)
          mbuf => elbuf_tab(ng)%bufly(ilay)%mat(iptr,ipts,iptt)
          fbuf => elbuf_tab(ng)%bufly(ilay)%fail(iptr,ipts,iptt)
          vbuf => elbuf_tab(ng)%bufly(ilay)%visc(iptr,ipts,iptt)
          ebuf => elbuf_tab(ng)%bufly(ilay)%eos(iptr,ipts,iptt)
          ilaw  = elbuf_tab(ng)%bufly(ilay)%ilaw
          nfail = elbuf_tab(ng)%bufly(ilay)%nfail
          ivisc = elbuf_tab(ng)%bufly(ilay)%ivisc
          nptr  = elbuf_tab(ng)%nptr
          npts  = elbuf_tab(ng)%npts
          nptt  = elbuf_tab(ng)%nptt
          nlay  = elbuf_tab(ng)%nlay
          npg   = npts*nptt*nptr
          nvareos = elbuf_tab(ng)%bufly(ilay)%nvar_eos
          nvarvis = elbuf_tab(ng)%bufly(ilay)%nvar_visc
          ipg   = iptr + ((ipts-1) + (iptt-1)*npts)*nptr

          dmg_flag = elbuf_tab(ng)%bufly(ilay)%l_dmgscl
          if ((igtyp == 20).or.(igtyp == 21).or.(igtyp == 22)) then
            ipg  = iptr + ((ipts-1) + (ilay-1)*npts)*nptr
            npg  = nptr*npts*nlay
          endif
!
          iboltp = iparg(72,ng)
          nbpreld = gbuf%g_bpreld
          bpreld => gbuf%bpreld(1:nbpreld*nel)
!--------------------------------------------------------
!     compute undamaged effective stresses
!---------------------------------------------------------
          ! -> isotropic stress softening
          if ((dmg_flag == 1).and.(mtn < 28 .or. mtn == 49)) then
            do i = 1,nel
              lbuf%sig(nel*(1-1) + i) =&
              &lbuf%sig(nel*(1-1) + i)/max(lbuf%dmgscl(i),em20)
              lbuf%sig(nel*(2-1) + i) =&
              &lbuf%sig(nel*(2-1) + i)/max(lbuf%dmgscl(i),em20)
              lbuf%sig(nel*(3-1) + i) =&
              &lbuf%sig(nel*(3-1) + i)/max(lbuf%dmgscl(i),em20)
              lbuf%sig(nel*(4-1) + i) =&
              &lbuf%sig(nel*(4-1) + i)/max(lbuf%dmgscl(i),em20)
              lbuf%sig(nel*(5-1) + i) =&
              &lbuf%sig(nel*(5-1) + i)/max(lbuf%dmgscl(i),em20)
              lbuf%sig(nel*(6-1) + i) =&
              &lbuf%sig(nel*(6-1) + i)/max(lbuf%dmgscl(i),em20)
            enddo
            ! -> orthotropic stress softening
          elseif ((dmg_flag == 6).and.(mtn < 28 .or. mtn == 49)) then
            do i = 1,nel
              lbuf%sig(nel*(1-1) + i) =&
              &lbuf%sig(nel*(1-1) + i)/max(lbuf%dmgscl(nel*(1-1) + i),em20)
              lbuf%sig(nel*(2-1) + i) =&
              &lbuf%sig(nel*(2-1) + i)/max(lbuf%dmgscl(nel*(2-1) + i),em20)
              lbuf%sig(nel*(3-1) + i) =&
              &lbuf%sig(nel*(3-1) + i)/max(lbuf%dmgscl(nel*(3-1) + i),em20)
              lbuf%sig(nel*(4-1) + i) =&
              &lbuf%sig(nel*(4-1) + i)/max(lbuf%dmgscl(nel*(4-1) + i),em20)
              lbuf%sig(nel*(5-1) + i) =&
              &lbuf%sig(nel*(5-1) + i)/max(lbuf%dmgscl(nel*(5-1) + i),em20)
              lbuf%sig(nel*(6-1) + i) =&
              &lbuf%sig(nel*(6-1) + i)/max(lbuf%dmgscl(nel*(6-1) + i),em20)
            enddo
          endif
!-----

! flag idel
          off_old(1:nel) = off(1:nel)

          dpla(1:nel) = zero
!
          if (jthe < 0) then
            die(1:nel) = lbuf%eint(1:nel)
          endif
!
! tangent module ratio (often w.r.t. g )
          et(1:nel) = one
!----   iselect>0 : modified left cauchy-green  strain  saved in etotsh
          iselect=0
          if(ismstr >= 10.and.ismstr <= 12) then
!------ iparg(10,ng): icpre
            if (jhbe==17.and.iint==2.and.jcvt/=0.and.iparg(10,ng)/=1) iselect=1
          end if
!------for hyper-elastic laws, due to et=max(et, initialise to zero
          if (impl_s > 0 .or. ihet > 1) then
            if (mtn == 42 .or. mtn == 62 .or. mtn == 69 .or.&
            &mtn == 82 .or. mtn == 88 .or. mtn == 92 .or.&
            &mtn == 94 .or. mtn == 111 ) then
              et(1:nel)=zero
            end if
          end if
! needed in mqviscb
          facq0 =  one
          isvis = igeo(35,pid(1))
          if((impl_s > 0 .and. idyna == 0).or. mtn == 24) isvis = 0
!
          imat = mat(1)
          rho0(1:nel)  = pm(1,imat)
          svis(1:nel,1)= zero
          svis(1:nel,2)= zero
          svis(1:nel,3)= zero
          svis(1:nel,4)= zero
          svis(1:nel,5)= zero
          svis(1:nel,6)= zero
!
          if (iparg(64,ng) /= 0) then
            df(1:nel)      = one
            vol_avg(1:nel) = voln(1:nel)
          else
            df(1:nel)  =  rho0(1:nel)/lbuf%rho(1:nel)
            vol_avg(1:nel) = voln(1:nel) - half*dvol(1:nel)
          endif
!-----due to too much returns of qa, temporarily add iresp here
          if((ale%global%incomp/=1.or.(jeul+jale)/=1).and.jlag/=0.and.n2d==0.and.jsph==0.and.iresp==1&
          &.and.impl_s==0.and.ismstr/=1.and.ismstr/=3.and.ismstr/=11)then
!-----due to issue w/ sp ----in srho3.f r4_amu is calculated for sp :
!--    r4_amu =vol0dp/voldp -1   because rho*voldp=rho0*vol0dp
            amu(1:nel) =  r4_amu(1:nel)
          else
            if(mtn == 1 .or. mtn == 2 .or. mtn == 22.or.(mtn >=28.and.mtn /=45)) then     ! for compatibility with qa tests
              amu(1:nel) =  lbuf%rho(1:nel)/rho0(1:nel)-one
            else
              amu(1:nel) =  one/df(1:nel)-one
            end if
            if(ismstr==12)then
              do i=1,nel
                if(abs(lbuf%off(i))>one) amu(i) =  r4_amu(i)
              end do
            end if
          endif

! --------------------------------------------------------
!
!     rhoref is corresponding to vol = volume for integration
!        - rhoref=rho  for large strain
!        - rhoref=rho0*vol0/vol for small strain
!        in any case: d(rho)/dt=-rhoref trace(epsilon_dot)
!
!     rhosp must be returned by the material law, as well as ssp
!     it is used only for time step computation wrt viscosity(*) in mqviscb.f
!        (*) wrt artificial viscosity and navier stokes viscosity
!
!     case of hydrodynamic pressure :
!        !-----------------------
!        !     p = c1 mu, mu = rho/rho0-1, d(rho)/d(mu)=rho0
!        !     ssp^2 = dp/drho = c1/rho0, rhosp = rho0
!        !-----------------------
!     case of small strain  :
!        !-----------------------
!        !     p = c1 mu, mu = trace(epsilon), d(rho)/d(mu)rhoref
!        !     ssp^2 = dp/drho = c1/rhoref, rhosp = rhoref
!        !-----------------------
!     case of large strain :
!        !-----------------------
!         !     p = c1 mu, mu = trace(epsilon) = ln(rho/rho0), rho= rho0 exp(mu), d(rho)/d(mu)=rho
!         !     ssp^2 = dp/drho = c1/rho, rhosp = rho
!         !-----------------------
!     if is not returned by the material law, rhosp = rho insures backward compatibility.
! --------------------------------------------------------
          if(ismstr==1 .or. ismstr==11)then
            rhoref(1:nel)=rho0(1:nel)
          elseif(ismstr==2 .or. ismstr==12)then
            do i=1,nel
              if(abs(lbuf%off(i))<=one)then
                rhoref(i)=lbuf%rho(i)
              else
                rhoref(i)=rho0(i)*lbuf%vol(i)/max(em20,voln(i))
                if (ismstr==12) amu(i) =  r4_amu(i)
              end if
            end do
          else ! large strain, ismstr==4 .or. ismstr==10
            rhoref(1:nel)=lbuf%rho(1:nel)
          end if
          rhosp(1:nel) =lbuf%rho(1:nel)
! --------------------------------------------------------
!    storage used for element temperature (in Gauss points)
! --------------------------------------------------------
          if (jthe == 0 .and. elbuf_tab(ng)%bufly(ilay)%l_temp > 0) then
            el_temp => lbuf%temp(1:nel) ! adiabatic conditions => element buffer 
          else
            el_temp => tempel(1:nel)    ! /heat/mat => local, from actualized nodal temperature
          end if
! --------------------------------------------------------
!    thermal material istrope expansion
! --------------------------------------------------------
      if (iexpan > 0 .and. jthe < 0 .and. tt/=zero ) then
        if (ismstr==4) then
          amu(1:nel)  = amu(1:nel) /(amu(1:nel) + one) ! to get amu = 1-rho0/rho
        endif
        if(ismstr==2) then
          do i=1,nel
            if(abs(lbuf%off(i))<=one)then
              amu(i)  = amu(i) /(amu(i) + one)
            end if
          enddo
        end if

        do i=1,nel
          ifunc_alpha = ipm(219,imat)
          fscal_alpha = pm(191,imat)
          tempel0(i)  = lbuf%temp(i)
          alpha = finter(ifunc_alpha,tempel(i),npf,tf,bidon1)
          alpha = alpha * fscal_alpha
          eth(i)= alpha *(tempel(i)-tempel0(i))*off(i)
          lbuf%forth(i) = lbuf%forth(i) + eth(i)  ! lbuf%forth the total thermal strain over time
          epsth(i)= three*lbuf%forth(i)
          dxx(i)  = dxx(i)-eth(i)/dt1
          dyy(i)  = dyy(i)-eth(i)/dt1
          dzz(i)  = dzz(i)-eth(i)/dt1
          dvol(i) = dvol(i)-three*eth(i)*vol_avg(i)
          amu(i)  = amu(i) + epsth(i)
          sigkk(i)= lbuf%sig(nel*(1-1)+i)+lbuf%sig(nel*(2-1)+i)+lbuf%sig(nel*(3-1)+i)
          lbuf%eintth(i) = lbuf%eintth(i)-half*sigkk(i)*eth(i)
        enddo
      else
        epsth(1:nel)= zero
      endif
!
!-----tstar computation for jonhson cook failure : t* = (t-tref)/(tmelt-tref) => move to JC routine
      tref  = pm(79, imat)
      tmelt = pm(80, imat)
      do i=1,nel
        tstar(i) = max(zero,(el_temp(i)-tref) / max((tmelt-tref),em20) )
      enddo
!-----------------------------------
!     eos part1
!-----------------------------------
          eostyp = mat_elem%mat_param(imat)%ieos
          if (eostyp > 0 .and. mtn /= 12 ) then
            call eosmain(0         ,nel      ,eostyp  ,pm        ,off      ,lbuf%eint,&
                         lbuf%rho  ,rho0     ,amu     ,amu2      ,espe     ,&
                         dvol      ,df       ,voln    ,mat       ,psh      ,&
                         pnew      ,dpdm     ,dpde    ,lbuf%temp ,ecold    ,&
                         bufmat    ,lbuf%sig ,lbuf%mu ,mtn       ,pold     ,&
                         npf       ,tf       ,ebuf%var,nvareos , mat_elem%mat_param(imat),&
                         lbuf%bfrac)
          endif
!-----------------------------------
!     stresses deviatoric/total
!-----------------------------------
          ibpreld = 0
          if(iboltp /= 0)then
            !two conditions checks, otherwise bpreld has size 0 if iboltp==0 and debugger stops as it detects an index out of bounds.
            if(bpreld(2*nel+1) <= one .and.bpreld(2*nel+1) > zero)ibpreld = 1
          endif

          if(iboltp /= 0 .and. ibpreld == 1)then
            call sboltlaw(&
            &pm,         lbuf%sig,   mat,        dxx,&
            &dyy,        dzz,        d4,         d5,&
            &d6,         nel,        lbuf%rho,   bpreld,&
            &lbuf%eint,  lbuf%qvis,  lbuf%vol,   stifn,&
            &dt2t,       neltst,     ityptst,    lbuf%off,&
            &geo,        pid,        mumax,      ngl,&
            &cxx,        dvol,       aire,       voln,&
            &vd2,        deltax,     vis,        pnew,&
            &psh,        qvis,       ssp_eq,     s1,&
            &s2,         s3,         s4,         s5,&
            &s6,         mssa,       dmels,      conde,&
            &amu,        vol_avg,    gbuf%dt,    gbuf%g_dt,&
            &off,        ipm,        rhoref,     rhosp,&
            &lbuf%vol0dp,ismstr,     jsph,       jtur,&
            &ity,        jthe,       jsms,       npg ,&
            glob_therm)
          else if (mtn == 1) then
!
            if (jhbe==17.and.iint==3.and.ismstr == 1) then
              call m1lawi(&
              &pm,       off,      lbuf%sig, lbuf%eint,&
              &lbuf%rho, lbuf%qvis,lbuf%vol, stifn,&
              &dt2t,     neltst,   ityptst,  lbuf%off,&
              &geo,      pid,      mumax,    mat,&
              &ngl,      cxx,      dvol,     aire,&
              &voln,     vd2,      deltax,   vis,&
              &dxx,      dyy,      dzz,      d4,&
              &d5,       d6,       pnew,     psh,&
              &qvis,     ssp_eq,   s1,       s2,&
              &s3,       s4,       s5,       s6,&
              &mssa,     dmels,    conde,    amu,&
              &vol_avg,  gbuf%dt,  gbuf%g_dt,nel,&
              &ipm,      rhoref,   rhosp,    ity,&
              &jtur,     jthe,     jsph,     ismstr,&
              &jsms,     npg ,     glob_therm)
            else if(ismstr >= 10.and.ismstr <= 12)then
              call m1lawtot(&
              &pm,         off,        lbuf%sig,   lbuf%eint,&
              &lbuf%rho,   lbuf%qvis,  lbuf%vol,   stifn,&
              &dt2t,       neltst,     ityptst,    lbuf%off,&
              &geo,        pid,        amu,        mumax,&
              &mat,        ngl,        cxx,        dvol,&
              &aire,       voln,       vd2,        deltax,&
              &vis,        dxx,        dyy,        dzz,&
              &d4,         d5,         d6,         pnew,&
              &psh,        qvis,       ssp_eq,     s1,&
              &s2,         s3,         s4,         s5,&
              &s6,         mssa,       dmels,      conde,&
              &mfxx,       mfxy,       mfxz,       mfyx,&
              &mfyy,       mfyz,       mfzx,       mfzy,&
              &mfzz,       gbuf%off,   vol_avg,    epsth,&
              &gbuf%dt,    gbuf%g_dt,  nel,        gbuf%etotsh,&
              &iselect,    ipm,        rhoref,     rhosp,&
              &lbuf%sigl,  ity,        ismstr,     jtur,&
              &jthe,       jcvt,       jsph,       jsms,&
              &npg ,       glob_therm)
            else
              call m1law(&
              &pm,       off,      lbuf%sig, lbuf%eint,&
              &lbuf%rho, lbuf%qvis,lbuf%vol, stifn,&
              &dt2t,     neltst,   ityptst,  lbuf%off,&
              &geo,      pid,      mumax,    mat,&
              &ngl,      cxx,      dvol,     aire,&
              &voln,     vd2,      deltax,   vis,&
              &dxx,      dyy,      dzz,      d4,&
              &d5,       d6,       pnew,     psh,&
              &qvis,     ssp_eq,   s1,       s2,&
              &s3,       s4,       s5,       s6,&
              &mssa,     dmels,    conde,    amu,&
              &vol_avg,  gbuf%dt,  gbuf%g_dt,nel,&
              &ipm,      rhoref,   rhosp,    ity,&
              &jtur,     jthe,     jsph,     ismstr,&
              &jsms,     npg ,     glob_therm)
            end if
!
          elseif (mtn == 2) then
!
            call m2law(&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%qvis,lbuf%pla, lbuf%epsd,&
            &lbuf%vol, stifn,    dt2t,     neltst,&
            &ityptst,  lbuf%off, geo,      pid,&
            &amu,      vol_avg,  mumax,    mat,&
            &ngl,      cxx,      dvol,     aire,&
            &voln,     vd2,      deltax,   vis,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       pnew,     psh,&
            &qvis,     ssp_eq,   s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &sigy,     defp,     dpla,&
            &epsp,     tstar,    et,       mssa,&
            &dmels,    el_temp,  lbuf%sigb,al_imp,&
            &signor,   conde,    gbuf%dt,  gbuf%g_dt,&
            &nel,      ipm,      rhoref,   rhosp,&
            &ipg,      lbuf%dmg, ity,      jtur,&
            &jthe,     jsph,     ismstr,   jsms,&
            &lbuf%epsq,npg ,mat_elem%mat_param(imat)%ieos ,dpdm  ,fheat ,glob_therm)
!----------------
            if (istrain > 0 .and.&
            &(h3d_strain == 1 .or. th_strain == 1 )) then
!
              ! strain rate
              do i=1,nel
                ep1(i) = dxx(i)*off(i)
                ep2(i) = dyy(i)*off(i)
                ep3(i) = dzz(i)*off(i)
                ep4(i) = d4(i) *off(i)
                ep5(i) = d5(i) *off(i)
                ep6(i) = d6(i) *off(i)
              enddo
              ! strain
              if (jcvt > 0) then
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
              elseif (isorth /= 0) then
!---------------------------
!       orthotrope global
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
                &r11,r21,r31,r12,r22,r32,r13,r23,r33)
                do i=1,nel
                  ep4(i) = two*ep4(i)
                  ep5(i) = two*ep5(i)
                  ep6(i) = two*ep6(i)
                enddo
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
              else
!---------------------------
!       isotrope global
!---------------------------
#include "vectorize.inc"
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
#include "vectorize.inc"
                do i=1,nel
                  wxxf = wxx(i)*off(i)
                  wyyf = wyy(i)*off(i)
                  wzzf = wzz(i)*off(i)
                  q1 = lbuf%stra(nel*(4-1) + i)*wzzf
                  q2 = lbuf%stra(nel*(6-1) + i)*wyyf
                  q3 = lbuf%stra(nel*(5-1) + i)*wxxf
                  str1 = lbuf%stra(nel*(1-1) + i)-q1+q2
                  str2 = lbuf%stra(nel*(2-1) + i)+q1-q3
                  str3 = lbuf%stra(nel*(3-1) + i)-q2+q3
                  str4 = lbuf%stra(nel*(4-1) + i)+&
                  &two*wzzf*(lbuf%stra(nel*(1-1) + i)-lbuf%stra(nel*(2-1) + i))+&
                  &wyyf*lbuf%stra(nel*(5-1) + i)-wxxf*lbuf%stra(nel*(6-1) + i)
                  str5 = lbuf%stra(nel*(5-1) + i)+&
                  &two*wxxf*(lbuf%stra(nel*(2-1) + i)-lbuf%stra(nel*(3-1) + i))+&
                  &wzzf*lbuf%stra(nel*(6-1) + i)-wyyf*lbuf%stra(nel*(4-1) + i)
                  str6 = lbuf%stra(nel*(6-1) + i)+&
                  &two*wyyf*(lbuf%stra(nel*(3-1) + i)-lbuf%stra(nel*(1-1) + i))+&
                  &wxxf*lbuf%stra(nel*(4-1) + i)-wzzf*lbuf%stra(nel*(5-1) + i)
                  lbuf%stra(nel*(1-1) + i) = str1
                  lbuf%stra(nel*(2-1) + i) = str2
                  lbuf%stra(nel*(3-1) + i) = str3
                  lbuf%stra(nel*(4-1) + i) = str4
                  lbuf%stra(nel*(5-1) + i) = str5
                  lbuf%stra(nel*(6-1) + i) = str6
                enddo
              endif ! if (jcvt > 0)
              ! strain update
              do i=1,nel
                lbuf%stra(nel*(1-1) + i) = lbuf%stra(nel*(1-1) + i) + es1(i)
                lbuf%stra(nel*(2-1) + i) = lbuf%stra(nel*(2-1) + i) + es2(i)
                lbuf%stra(nel*(3-1) + i) = lbuf%stra(nel*(3-1) + i) + es3(i)
                lbuf%stra(nel*(4-1) + i) = lbuf%stra(nel*(4-1) + i) + es4(i)
                lbuf%stra(nel*(5-1) + i) = lbuf%stra(nel*(5-1) + i) + es5(i)
                lbuf%stra(nel*(6-1) + i) = lbuf%stra(nel*(6-1) + i) + es6(i)
              enddo
            endif ! if (istrain > 0
!
!----------------
          elseif (mtn == 3) then
            call m3law(&
            &pm,      off,     lbuf%sig,lbuf%pla,&
            &mat,     ngl,     cxx,     dxx,&
            &dyy,     dzz,     d4,      d5,&
            &d6,      rho0,    dpdm,    jplasol,&
            &sigy,    defp,    dpla,    nel,&
            &nft)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
!
          elseif (mtn == 4) then
            call m4law(&
            &pm,       off,      lbuf%sig, lbuf%pla,&
            &mat,      cxx,      lbuf%vol, dxx,      &
            &dyy,      dzz,      d4,       d5,&
            &d6,       rho0,     dpdm,     lbuf%epsd,&
            &jplasol,  sigy,     defp,     dpla,&
            &epsp,     tstar,    el_temp,  nel   , jthe,   &
            &mat_elem%mat_param(imat)%ieos,fheat )
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
!----------------
            if (istrain > 0 .and.&
            &(h3d_strain == 1 .or. th_strain == 1 )) then
!
              ! strain rate
              do i=1,nel
                ep1(i) = dxx(i)*off(i)
                ep2(i) = dyy(i)*off(i)
                ep3(i) = dzz(i)*off(i)
                ep4(i) = d4(i) *off(i)
                ep5(i) = d5(i) *off(i)
                ep6(i) = d6(i) *off(i)
              enddo
              ! strain
              if (jcvt > 0) then
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
              elseif (isorth /= 0) then
!---------------------------
!       orthotrope global
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
                &r11,r21,r31,r12,r22,r32,r13,r23,r33)
                do i=1,nel
                  ep4(i) = two*ep4(i)
                  ep5(i) = two*ep5(i)
                  ep6(i) = two*ep6(i)
                enddo
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
              else
!---------------------------
!       isotrope global
!---------------------------
#include "vectorize.inc"
                do i=1,nel
                  es1(i) = ep1(i)*dt1
                  es2(i) = ep2(i)*dt1
                  es3(i) = ep3(i)*dt1
                  es4(i) = ep4(i)*dt1
                  es5(i) = ep5(i)*dt1
                  es6(i) = ep6(i)*dt1
                enddo
#include "vectorize.inc"
                do i=1,nel
                  wxxf = wxx(i)*off(i)
                  wyyf = wyy(i)*off(i)
                  wzzf = wzz(i)*off(i)
                  q1 = lbuf%stra(nel*(4-1) + i)*wzzf
                  q2 = lbuf%stra(nel*(6-1) + i)*wyyf
                  q3 = lbuf%stra(nel*(5-1) + i)*wxxf
                  str1 = lbuf%stra(nel*(1-1) + i)-q1+q2
                  str2 = lbuf%stra(nel*(2-1) + i)+q1-q3
                  str3 = lbuf%stra(nel*(3-1) + i)-q2+q3
                  str4 = lbuf%stra(nel*(4-1) + i)+&
                  &two*wzzf*(lbuf%stra(nel*(1-1) + i)-lbuf%stra(nel*(2-1) + i))+&
                  &wyyf*lbuf%stra(nel*(5-1) + i)-wxxf*lbuf%stra(nel*(6-1) + i)
                  str5 = lbuf%stra(nel*(5-1) + i)+&
                  &two*wxxf*(lbuf%stra(nel*(2-1) + i)-lbuf%stra(nel*(3-1) + i))+&
                  &wzzf*lbuf%stra(nel*(6-1) + i)-wyyf*lbuf%stra(nel*(4-1) + i)
                  str6 = lbuf%stra(nel*(6-1) + i)+&
                  &two*wyyf*(lbuf%stra(nel*(3-1) + i)-lbuf%stra(nel*(1-1) + i))+&
                  &wxxf*lbuf%stra(nel*(4-1) + i)-wzzf*lbuf%stra(nel*(5-1) + i)
                  lbuf%stra(nel*(1-1) + i) = str1
                  lbuf%stra(nel*(2-1) + i) = str2
                  lbuf%stra(nel*(3-1) + i) = str3
                  lbuf%stra(nel*(4-1) + i) = str4
                  lbuf%stra(nel*(5-1) + i) = str5
                  lbuf%stra(nel*(6-1) + i) = str6
                enddo
              endif ! if (jcvt > 0)
              ! strain update
              do i=1,nel
                lbuf%stra(nel*(1-1) + i) = lbuf%stra(nel*(1-1) + i) + es1(i)
                lbuf%stra(nel*(2-1) + i) = lbuf%stra(nel*(2-1) + i) + es2(i)
                lbuf%stra(nel*(3-1) + i) = lbuf%stra(nel*(3-1) + i) + es3(i)
                lbuf%stra(nel*(4-1) + i) = lbuf%stra(nel*(4-1) + i) + es4(i)
                lbuf%stra(nel*(5-1) + i) = lbuf%stra(nel*(5-1) + i) + es5(i)
                lbuf%stra(nel*(6-1) + i) = lbuf%stra(nel*(6-1) + i) + es6(i)
              enddo
            endif ! if (istrain > 0
!----------------
!
          elseif (mtn == 5) then
            call m5law(pm   ,lbuf%sig ,lbuf%eint  ,lbuf%rho ,psh       ,&
            &p0   ,lbuf%tb  ,lbuf%bfrac ,voln     ,deltax    ,&
            &mat  ,nel      ,cxx        ,df       ,&
            &er1v ,er2v     ,wdr1v      ,wdr2v    ,w1        ,&
            &rho0, amu)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call mjwl(pm     ,mat     ,off      ,lbuf%sig   ,lbuf%eint ,&
            &psh    ,p0      ,lbuf%qvis,lbuf%vol   ,lbuf%bfrac,&
            &voln   ,qvis    ,s1       ,s2         ,s3        ,&
            &dvol   ,nel     ,df       ,lbuf%aburn ,&
            &er1v   ,er2v    ,wdr1v    ,wdr2v      ,w1, amu)

          elseif (mtn == 6) then
            call m6law(&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%rk,  lbuf%re,  lbuf%vk,&
            &voln,     rho0,     vis,      psh,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       c1,       c2,&
            &c3,       c4,       c5,       c6,&
            &pc,       amu,      amu2,     espe,&
            &cxx,      df,       tmu,      mat,&
            &wxx,      wyy,      wzz,      nel,&
            &dpdm,     jtur,     jpor)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
            if(jtur/=0)call aturbn(&
            &pm,                 off,                lbuf%rho,&
            &lbuf%rk,            lbuf%re,            geo,                einc,&
            &dvol,               voln,               pnew,               tmu,&
            &vis,                vd2,                mat,                pid,&
            &lft,                llt,                jpor,               jclose)
            if(jthe == 1)call mtheta(&
            &pm,       lbuf%eint,lbuf%temp,amu,&
            &c1,       c2,       df,       psh,&
            &pc,       mat,      nel)
          elseif (mtn == 7) then
!        call m7law(pm     ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,
!     .             lbuf%qvis,lbuf%vol,mbuf%var,stifn   )
          elseif (mtn == 8) then
!        call m8law(pm     ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,
!     .             lbuf%qvis,lbuf%vol,mbuf%var,stifn   )
          elseif (mtn == 9) then
!        call m9law(pm     ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,
!     .             lbuf%qvis,lbuf%vol,mbuf%var,stifn   )
          elseif (mtn == 10) then
            call m10law(pm      ,off      ,lbuf%sig  ,lbuf%eint,lbuf%rho,&
            &lbuf%epsq  ,lbuf%pla ,lbuf%vol  ,mat      ,cxx     ,&
            &dvol       ,voln     , dxx      ,dyy      ,dzz     ,&
            &d4         ,d5       ,d6        ,s1       ,s2      ,&
            &s3         ,s4       ,s5        ,s6       ,sigy    ,&
            &defp       ,pnew     ,psh       ,amu      ,lbuf%seq,&
            &nel        ,dpdm)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      bid1,     bid2,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      bid1,     bid2,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
          elseif (mtn == 11) then
            call m11law(pm      ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,&
            &lbuf%temp,lbuf%rk,lbuf%re ,lbuf%vk  ,&
            &ale_connect   ,ix      ,iparg   ,elbuf_tab,v       ,&
            &x       ,lbuf%eins,stifn  ,w        ,vd2     ,&
            &vdx     ,vdy     ,vdz     ,vis      ,mat     ,&
            &voln    ,qvis    ,rho0    ,bufvois  ,ipm     ,&
            &npf     ,tf      ,ssp_eq  ,gbuf%mom ,nel     )
            if(iparg(7,ng)+iparg(11,ng) > 0 ) then
              cxx(1:nel) = ssp_eq(1:nel)
            else
              lbuf%ssp(1:nel) = ssp_eq(1:nel)
            endif
          elseif (mtn == 12) then
!       tsai 3d complet
            eostyp  = mat_elem%mat_param(imat)%ieos
            strd1(1:nel*3) => lbuf%epe(1:nel*3)
            strd2(1:nel*3) => lbuf%epe(1+nel*3:nel*6)
            call m12law(&
            &pm,         off,        lbuf%sig,   lbuf%eint,&
            &lbuf%pla,   lbuf%sigf,  lbuf%epsf,  lbuf%dam,&
            &strd1,      strd2,      gama,       lbuf%vol,&
            &rx,         ry,         rz,         sx,&
            &sy,         sz,         mat,        voln,&
            &dvol,       cxx,        dxx,        dyy,&
            &dzz,        d4,         d5,         d6,&
            &s1,         s2,         s3,         s4,&
            &s5,         s6,         sigy,       defp,&
            &ngl,        lbuf%seq,   nel,        eostyp,&
            &rho0,       amu,        amu2,       espe,&
            &df,         psh,        pnew,       dpdm,&
            &dpde,       lbuf%rho,   lbuf%temp,  ecold,&
            &bufmat,     npf,        tf,         lbuf%tsaiwu,&
            &ebuf%var,   nvareos,    jcvt,       jsph,&
            &mat_elem%mat_param(imat))
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
          elseif (mtn == 13) then
            call m13law(pm      ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,&
            &lbuf%qvis,lbuf%vol,lbuf%pla,stifn   )
          elseif (mtn == 14) then
            strd1(1:nel*3) => lbuf%epe(1:nel*3)
            strd2(1:nel*3) => lbuf%epe(1+nel*3:nel*6)
            call m14law(&
            &pm,         off,        lbuf%sig,   lbuf%eint,&
            &lbuf%pla,   lbuf%sigf,  lbuf%epsf,  lbuf%dam,&
            &strd1,      strd2,      gama,       lbuf%vol,&
            &rx,         ry,         rz,         sx,&
            &sy,         sz,         mat,        voln,&
            &dvol,       cxx,        dxx,        dyy,&
            &dzz,        d4,         d5,         d6,&
            &s1,         s2,         s3,         s4,&
            &s5,         s6,         sigy,       defp,&
            &ngl,        lbuf%seq,   nel,        lbuf%tsaiwu,&
            &jcvt,       jsph)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
          elseif (mtn == 16) then
            call m16law(pm        ,off       ,lbuf%sig  ,nel      ,&
            &lbuf%pla  ,lbuf%temp ,lbuf%epsd ,lbuf%xst ,&
            &mat       ,dxx       ,dyy       ,dzz      ,&
            &d4        ,d5        ,d6        ,pold     )
            call gray10(off  ,lbuf%sig ,lbuf%qvis ,lbuf%eint ,voln,&
            &dvol ,s1       ,s2        ,s3        ,s4  ,&
            &s5   ,s6       ,dxx       ,dyy       ,dzz ,&
            &d4   ,d5       ,d6        ,nel       ,pold,&
            &einc)
            call gray20(&
            &pm,       lbuf%eint,lbuf%rho, lbuf%temp,&
            &lbuf%xst, lbuf%qvis,voln,     mat,&
            &dvol,     pold,     df,       rho0,&
            &pnew,     p01,      p02,      e01,&
            &e02,      cxx,      dpdm,     nel)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call gray30(lbuf%sig ,lbuf%eint ,lbuf%qvis ,lbuf%vol ,qvis,&
            &voln     ,dvol      ,nel       , p01     ,p02,&
            &e01      ,e02       ,pnew)
!
          elseif (mtn == 17) then
            call m17law(&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%rk,  lbuf%re,  lbuf%vk,&
            &wxx,      wyy,      wzz,      voln,&
            &mat,      vis,      dxx,      dyy,&
            &dzz,      d4,       d5,       d6,&
            &nel,      alogey,   cxx,      rho0,&
            &tmu,      amu,      amu2,     psh,&
            &pc,       espe,     c1,       c2,&
            &c3,       c4,       c5,       c6,&
            &df,       dpdm,     jpor)
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
            call atur17(&
            &pm,      off,     lbuf%rho,lbuf%rk,&
            &lbuf%re, geo,     voln,    mat,&
            &deltax,  pid,     vis,     voln,&
            &vd2,     dvol,    aire,    einc,&
            &pnew,    tmu,     alogey,  nel,&
            &lft,     llt,     jpor)
            if(jthe == 1)call mtheta(&
            &pm,       lbuf%eint,lbuf%temp,amu,&
            &c1,       c2,       df,       psh,&
            &pc,       mat,      nel)
          elseif (mtn == 18) then
            call m18law(&
            &pm,         lbuf%vol,   lbuf%eint,  lbuf%temp,&
            &lbuf%deltax,tf,         npf,        dt2t,&
            &neltst,     ityptst,    ipm,        stifn,&
            &voln,       mat,        ngl,        conde,&
            &nel,        ity,        glob_therm%idt_therm,glob_therm%dt_therm)
          elseif (mtn == 21) then
            call m21law(pm    ,off     ,lbuf%sig,lbuf%eint,lbuf%rho,&
            &lbuf%epsq,lbuf%pla,lbuf%vol,mat      ,cxx     ,&
            &dvol     ,voln    ,dxx     ,dyy      ,dzz     ,&
            &d4       ,d5      ,d6      ,s1       ,s2      ,&
            &s3       ,s4      ,s5      ,s6       ,tf      ,&
            &npf      ,sigy    ,defp    ,ipm      ,pnew    ,&
            &psh      ,amu     ,lbuf%seq,nel      )
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      bid1,     bid2,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      bid1,     bid2,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            !no need to call eosmain since it is no energy dependant. p and psh are already calculated
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
            call eosupda(off  ,lbuf%sig ,lbuf%eint, lbuf%vol ,pnew ,nel)
          elseif (mtn == 22) then
            call m22law(&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%qvis,lbuf%pla, lbuf%epsd,&
            &lbuf%vol, zero,     stifn,    dt2t,&
            &neltst,   ityptst,  lbuf%off, geo,&
            &pid,      amu,      vol_avg,  mumax,&
            &mat,      ngl,      cxx,      dvol,&
            &aire,     voln,     vd2,      deltax,&
            &vis,      dxx,      dyy,      dzz,&
            &d4,       d5,       d6,       pnew,&
            &psh,      qvis,     ssp_eq,   s1,&
            &s2,       s3,       s4,       s5,&
            &s6,       jplasol,  sigy,     defp,&
            &dpla,     mssa,     dmels,    conde,&
            &gbuf%dt,  gbuf%g_dt,nel,      ipm,&
            &rhoref,   rhosp,    nft,      jsph,&
            &ity,      jtur,     jthe,     ismstr,&
            &jsms,     npg ,     glob_therm)
          elseif (mtn == 23) then
            call m22law(&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%qvis,lbuf%pla, lbuf%epsd,&
            &lbuf%vol, one,      stifn,    dt2t,&
            &neltst,   ityptst,  lbuf%off, geo,&
            &pid,      amu,      vol_avg,  mumax,&
            &mat,      ngl,      cxx,      dvol,&
            &aire,     voln,     vd2,      deltax,&
            &vis,      dxx,      dyy,      dzz,&
            &d4,       d5,       d6,       pnew,&
            &psh,      qvis,     ssp_eq,   s1,&
            &s2,       s3,       s4,       s5,&
            &s6,       jplasol,  sigy,     defp,&
            &dpla,     mssa,     dmels,    conde,&
            &gbuf%dt,  gbuf%g_dt,nel,      ipm,&
            &rhoref,   rhosp,    nft,      jsph,&
            &ity,      jtur,     jthe,     ismstr,&
            &jsms,     npg ,     glob_therm)
          elseif (mtn == 24) then
            call m24law(&
            &lbuf,     pm,       off,      lbuf%sig,&
            &lbuf%eint,lbuf%rho, lbuf%qvis,lbuf%vol,&
            &stifn,    dt2t,     neltst,   ityptst,&
            &offset,   nel,      lbuf%off, geo,&
            &pid,      mat,      ngl,      cxx,&
            &aire,     voln,     vd2,      deltax,&
            &vis,      dxx,      dyy,      dzz,&
            &pnew,     psh,      qvis,     ssp_eq,&
            &dvol,     d4,       d5,       d6,&
            &mumax,    gama,     mssa,     dmels,&
            &r3_dam,   s1,       s2,       s3,&
            &s4,       s5,       s6,       rx,&
            &ry,       rz,       sx,       sy,&
            &sz,       conde,    vol_avg,  gbuf%dt,&
            &gbuf%g_dt,ipm,      rhoref,   rhosp,&
            &lbuf%epsd,ity,      jtur,     jthe,&
            &jhbe,     jcvt,     jsph,     ismstr,&
            &jsms,     npg,      svis ,    glob_therm)
!     like law25 for shell + s33 = eps33*e33
          elseif (mtn == 25) then
            call m25law(mat_elem%mat_param(imat),&
            &pm(1,imat), off,        lbuf%sig,&
            &lbuf%eint,  s1,         s2,         s3,&
            &s4,         s5,         s6,         dxx,&
            &dyy,        dzz,        d4,         d5,&
            &d6,         rx,         ry,         rz,&
            &sx,         sy,         sz,         tx,&
            &ty,         tz,         gama,       voln,&
            &cxx,        lbuf%vol,   lbuf%epsd,  lbuf%pla,&
            &lbuf%stra,  sigl,       lbuf%tsaiwu,&
            &lbuf%off,   ngl,        nel,        nft,&
            &ilay,       npt,        ipg,&
            &jcvt,       jsph,       isorth,     lbuf%dmg,&
            &elbuf_tab(ng)%bufly(ilay)%l_dmg,gbuf%ierr)
!
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
          elseif (mtn == 26) then
            call m26law(pm      ,off      ,lbuf%sig ,lbuf%rho ,&
            &lbuf%pla,lbuf%temp,lbuf%epsd,lbuf%z   ,&
            &mat     ,voln     ,dvol     ,dxx      ,&
            &dyy     ,dzz      ,d4       ,d5       ,&
            &d6      ,nel, pold, rho0, df)
            call sesa10(off    ,lbuf%sig,lbuf%qvis,lbuf%eint,voln,&
            &dvol   ,s1      ,s2       ,s3       ,s4  ,&
            &s5     ,s6      ,dxx      ,dyy      ,dzz ,&
            &d4     ,d5      ,d6       ,nel, pold, df, einc)
            call sesa20(&
            &pm,       lbuf%eint,lbuf%rho, lbuf%temp,&
            &lbuf%z,   lbuf%qvis,bufmat,   lbuf%ssp,&
            &xk,       voln,     mat,      qvis,&
            &dvol,     pold,     cxx,      rho0,&
            &p01,      nel)
            if (jsph == 0) then
              call mqvisc26(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &xk,       nel,      ity,      ismstr,&
              &jtur,     jthe)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call sesa30(lbuf%sig ,lbuf%eint ,lbuf%qvis ,lbuf%vol ,qvis,&
            &voln     ,dvol      ,nel, p01)
          elseif (mtn == 46 .or. mtn == 47) then
            nuvar  = ipm(8,imat)
            call m46law(&
            &lft,      llt,      nft,      mtn,&
            &pm,       off,      lbuf%sig, lbuf%eint,&
            &lbuf%rho, lbuf%qvis,lbuf%vol, mbuf%var,&
            &bufmat,   stifn,    mat,      ngl,&
            &nuvar,    dt2t,     neltst,   rho0,&
            &ityptst,  lbuf%off, jlag,     geo,&
            &pid,      cxx,      aire,     voln,&
            &vd2,      deltax,   vis,      dxx,&
            &dyy,      dzz,      pnew,     psh,&
            &qvis,     ssp_eq,   wxx,      wyy,&
            &wzz,      ipm,      mssa,     dmels,&
            &dvol,     s1,       s2,       s3,&
            &s4,       s5,       s6,       conde,&
            &vol_avg,  gbuf%dt,  gbuf%g_dt,nel,&
            &d4,       d5,       d6,       rhoref,&
            &rhosp,    ismstr,   ity,      jsms,&
            &jtur,     jthe,     npg,svis ,glob_therm)
!
          elseif (mtn == 49) then
            call m49law (mat      ,pm       ,off     ,lbuf%sig,lbuf%pla,&
            &lbuf%temp,lbuf%epsd,cxx     ,df      ,dxx     ,&
            &dyy      ,dzz      ,d4      ,d5      ,d6      ,&
            &rho0     ,dpdm     ,sigy    ,defp    ,dpla    ,&
            &espe     ,ecold    ,nel     )
            if (jsph == 0) then
              call mqviscb(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,cxx,      lbuf%re,  stifn,&
              &dt2t,     neltst,   ityptst,  aire,&
              &lbuf%off, geo,      pid,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &lbuf%vol, mssa,     dmels,    igeo,&
              &facq0,    conde,    gbuf%dt,  gbuf%g_dt,&
              &ipm,      rhoref,   rhosp,    nel,&
              &ity,      ismstr,   jtur,     jthe,&
              &jsms,     npg   ,   glob_therm)
            else
              call mdtsph(&
              &pm,       off,      lbuf%rho, lbuf%rk,&
              &lbuf%temp,lbuf%re,  stifn,    dt2t,&
              &neltst,   ityptst,  lbuf%off, geo,&
              &pid,      mumax,    cxx,      voln,&
              &vd2,      deltax,   vis,      dxx,&
              &dyy,      dzz,      pnew,     psh,&
              &mat,      ngl,      qvis,     ssp_eq,&
              &gbuf%g_dt,gbuf%dt,  nel,      ity,&
              &jtur,     jthe)
            endif
            call meint(&
            &off,      lbuf%sig, lbuf%qvis,lbuf%eint,&
            &voln,     espe,     s1,       s2,&
            &s3,       s4,       s5,       s6,&
            &dxx,      dyy,      dzz,      d4,&
            &d5,       d6,       psh,      dvol,&
            &df,       qvis,     pnew,     vis,&
            &tmu,      einc,     mtn,      vol_avg,&
            &nel,      jtur,     jlag,     jpor)
!
          elseif (mtn > 28 .and. mtn < 32 .or. mtn == 99 .or. mtn == 200) then
!---    user material law libraries here
!
            nuvar   = elbuf_tab(ng)%bufly(ilay)%nvar_mat
            l_mulaw_called=.true.
!
            call usermat_solid(&
            &lft,        llt,        nft,        mtn,&
            &jcvt,       pm,         off,        lbuf%sig,&
            &lbuf%eint,  lbuf%rho,   lbuf%qvis,  lbuf%vol,&
            &lbuf%stra,  lbuf%sigl,  gama,       mbuf%var,&
            &bufmat,     tf,         npf,        stifn,&
            &mat,        ngl,        nuvar,      dt2t,&
            &neltst,     ityptst,    lbuf%off,   geo,&
            &pid,        lbuf%epsd,  lbuf%temp,  wxx,&
            &wyy,        wzz,        jsph,       mumax,&
            &cxx,        aire,       voln,       vd2,&
            &deltax,     vis,        dxx,        dyy,&
            &dzz,        d4,         d5,         d6,&
            &pnew,       psh,        qvis,       ssp_eq,&
            &dvol,       s1,         s2,         s3,&
            &s4,         s5,         s6,         rx,&
            &ry,         rz,         sx,         sy,&
            &sz,         tx,         ty,         tz,&
            &jplasol,    sigy,       defp,       ismstr,&
            &mfxx,       mfxy,       mfxz,       mfyx,&
            &mfyy,       mfyz,       mfzx,       mfzy,&
            &mfzz,       ipm,        isorth,     fbuf,&
            &nfail,      npg,        lbuf%sigd,  dxy,&
            &dyx,        dyz,        dzy,        dzx,&
            &dxz,        fr_wav,     isrot,      v,&
            &varnl,      w,          ix,         x,&
            &jthe,       et,         mssa,       dmels,&
            &iptr,       ipts,       iptt,       table,&
            &fvd2,       fdeltax,    fssp,       fqvis,&
            &tempel,     iparg1,     igeo,       lbuf%sigv,&
            &al_imp,     signor,     istrain,    ng,&
            &elbuf_tab,  vbuf,       ilay,       lbuf%vk,&
            &iparg,      bufvois,    vdx,        vdy,&
            &vdz,        ihet,       conde,      itask,&
            &iexpan,     vol_avg,    amu,        epsth,&
            &lbuf%forth, lbuf%visc,  nel,        gbuf%etotsh,&
            &iselect,    tstar,      lbuf%mu,    amu2,&
            &dpdm,       rhoref,     rhosp,      nloc_dmg,&
            &ity,        jtur,       mat_elem,   idel7nok,svis,&
            &dt ,        glob_therm )
!
          else   ! 'user type' radioss material laws
!---
            nuvar   = elbuf_tab(ng)%bufly(ilay)%nvar_mat
            nvartmp = elbuf_tab(ng)%bufly(ilay)%nvartmp
            l_mulaw_called =.true.
!
            call mulaw(&
            &nft,         mtn,         jcvt,        pm,&
            &off,         lbuf%sig,    lbuf%eint,   lbuf%rho,&
            &lbuf%qvis,   lbuf%vol,    lbuf%stra,   lbuf%sigl,&
            &gama,        mbuf%var,    bufmat,      tf,&
            &npf,         stifn,       mat,         ngl,&
            &nuvar,       dt2t,        neltst,      ityptst,&
            &lbuf%off,    geo,         pid,         lbuf%epsd,&
            &lbuf%temp,   wxx,         wyy,         wzz,&
            &jsph,        mumax,       cxx,         aire,&
            &voln,        vd2,         deltax,      vis,&
            &dxx,         dyy,         dzz,         d4,&
            &d5,          d6,          pnew,        psh,&
            &qvis,        ssp_eq,      dvol,        s1,&
            &s2,          s3,          s4,          s5,&
            &s6,          rx,          ry,          rz,&
            &sx,          sy,          sz,          tx,&
            &ty,          tz,          jplasol,     sigy,&
            &defp,        ismstr,      mfxx,        mfxy,&
            &mfxz,        mfyx,        mfyy,        mfyz,&
            &mfzx,        mfzy,        mfzz,        ipm,&
            &isorth,      fbuf,        nfail,       npg,&
            &lbuf%sigd,   dxy,         dyx,         dyz,&
            &dzy,         dzx,         dxz,         fr_wav,&
            &v,           varnl,       w,           ix,&
            &x,           jthe,        et,          mssa,&
            &dmels,       iptr,        ipts,        iptt,&
            &table,       fvd2,        fdeltax,     fssp,&
            &fqvis,       tempel,      igeo,        lbuf%sigv,&
            &al_imp,      signor,      istrain,     ng,&
            &elbuf_tab,   vbuf,        ilay,        lbuf%vk,&
            &ale_connect, iparg,       bufvois,     vdx,&
            &vdy,         vdz,         ihet,        conde,&
            &itask,       iexpan,      vol_avg,     amu,&
            &epsth,       lbuf%forth,  lbuf%visc,   nel,&
            &gbuf%etotsh, iselect,     tstar,       lbuf%mu,&
            &dpdm,        rhoref,      rhosp,       nvartmp,&
            &mbuf%vartmp, lbuf%eintth, mat_elem,    nloc_dmg,&
            &ity,         jtur,        jsms,        idel7nok,&
            &sz_bufvois,  sz_ix,       snpc,        stf,&
            &sbufmat,     svis,        n2d,         ngroup,&
            &imon_mat,    numnod,      numels,      ntable,&
            &numgeo,      nummat,      numelq,      idtmin,&
            &dt1,         tt,         glob_therm,          &
            &impl_s,&
            &idyna,       userl_avail, nixs,        nixq,&
            &dt)

          endif
!-----------------------------------
!     eos part2
!-----------------------------------
          eostyp = mat_elem%mat_param(imat)%ieos
          if (eostyp > 0 .and. mtn /=12 ) then
            if (mtn /= 6 .and. mtn /= 17) then
              pnew(:) = zero
            endif
            call eosmain(1       ,nel      ,eostyp  ,pm       ,off      ,lbuf%eint,&
            &lbuf%rho  ,rho0     ,amu     ,amu2     ,espe     ,&
            &dvol      ,df       ,voln    ,mat      ,psh      ,&
            &pnew      ,dpdm     ,dpde    ,lbuf%temp,ecold    ,&
            &bufmat    ,lbuf%sig ,lbuf%mu ,mtn      ,pold     ,&
            &npf       ,tf       ,ebuf%var,nvareos , mat_elem%mat_param(imat),&
            &lbuf%bfrac)
!
            call eosupda(off  ,lbuf%sig ,lbuf%eint, lbuf%vol ,pnew,nel)
!
          elseif(l_mulaw_called)then
            if(l_mulaw_called)then
              do i=1,nel
                if(lbuf%vol(i) > zero)then
                  lbuf%eint(i)=lbuf%eint(i)/max(lbuf%vol(i),em20)
                else
                  lbuf%eint(i)=zero
                endif
              enddo
            endif
          endif

!---------------------------------------------------------------------
! --- needed for finite element transfert.
!
          if (jthe < 0) then
            heat_meca_l = zero
            if (mat_elem%mat_param(imat)%heat_flag == 0) then  
              ! internal energy is used as heat source by default
              do i=1,nel
                die(i) = lbuf%eint(i)*lbuf%vol(i) - die(i)
                die(i) = die(i) * pm(90,imat)  ! mat_elem%mat_param(imat)%therm%efrac
                heat_meca_l = heat_meca_l + die(i)
              enddo
            else   
              ! exact dissipated energy is calculated by the material law as heat source 
              do i=1,nel
                heat_meca_l = heat_meca_l + fheat(i)*pm(90,imat)  ! mat_elem%mat_param(imat)%therm%efrac
              enddo
            end if
!$omp critical
            glob_therm%heat_meca = glob_therm%heat_meca + heat_meca_l
!$omp end critical
          endif
          if((iexpan > 0).and.(jthe < 0).and.(tt/=0)) then
            do i=1,nel
              sigkk(i) = lbuf%sig(nel*(1-1)+i)+lbuf%sig(nel*(2-1)+i)+lbuf%sig(nel*(3-1)+i)
              lbuf%eintth(i) = lbuf%eintth(i)-half*sigkk(i)*eth(i)
            enddo
          endif

!---------------------------------------------------------------------
!   calcul of viscosity ( navier-stokes)
!---------------------------------------------------------------------
          if(isvis > 0.and. ((mtn < 28 .and. mtn /= 24) .or. mtn == 49)) then
            volg(1:nel)=npg*voln(1:nel)
            call mnsvis(&
            &pm,      off,     lbuf%rho,geo,&
            &pid,     cxx,     aire,    volg,&
            &dxx,     dyy,     dzz,     d4,&
            &d5,      d6,      mat,     isvis,&
            &rhoref,  nel,     svis)
            do  i=1,nel
              vol(i)  = lbuf%vol(i)
              eint(i) = lbuf%eint(i)
              e1=dxx(i)*svis(i,1)
              e2=dyy(i)*svis(i,2)
              e3=dzz(i)*svis(i,3)
              e4=d4(i)*svis(i,4)
              e5=d5(i)*svis(i,5)
              e6=d6(i)*svis(i,6)
              eint(i)=eint(i)*vol(i)&
              &+(e1+e2+e3+e4+e5+e6)*(voln(i)-half*dvol(i))*dt1*half
              eint(i)=eint(i)/max(em20,vol(i))
              lbuf%eint(i) = eint(i)
            enddo
          endif
!-----------------------------------------------------------------------
!     viscous stress (/visc models)
!-----------------------------------------------------------------------
          if (ivisc > 0 .and. (mtn  < 28 .and. mtn /= 24)) then
            do i=1,nel
              ep1(i) = dxx(i)*off(i)
              ep2(i) = dyy(i)*off(i)
              ep3(i) = dzz(i)*off(i)
              ep4(i) = d4(i) *off(i)
              ep5(i) = d5(i) *off(i)
              ep6(i) = d6(i) *off(i)
            enddo
            if (isorth /= 0 .and. jcvt == 0) then
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
              &r11,r21,r31,r12,r22,r32,r13,r23,r33)
              do i=1,nel
                ep4(i) = two*ep4(i)
                ep5(i) = two*ep5(i)
                ep6(i) = two*ep6(i)
              enddo
            endif
!---
            call viscmain(mat_elem%mat_param(imat)%visc    ,nel     ,&
            &nvarvis ,vbuf%var,rho0    ,vis     ,cxx     ,dt1     ,&
            &ep1     ,ep2     ,ep3     ,ep4     ,ep5     ,ep6     ,&
            &sv1     ,sv2     ,sv3     ,sv4     ,sv5     ,sv6     ,&
            &mfxx    ,mfxy    ,mfxz    ,mfyx    ,mfyy    ,mfyz    ,&
            &mfzx    ,mfzy    ,mfzz    ,&
            &s1      ,s2      ,s3      ,s4      ,s5      ,s6      )
!---
            if (isorth /= 0.and. jcvt == 0) then
              call mrotens(1,nel,&
              &sv1     ,sv2      ,sv3      ,sv4     ,sv5     ,sv6     ,&
              &r11,r21,r31,r12,r22,r32,r13,r23,r33)
            endif
!
            do i=1,nel
              lbuf%visc(nel*(1-1) + i) = sv1(i)*off(i)
              lbuf%visc(nel*(2-1) + i) = sv2(i)*off(i)
              lbuf%visc(nel*(3-1) + i) = sv3(i)*off(i)
              lbuf%visc(nel*(4-1) + i) = sv4(i)*off(i)
              lbuf%visc(nel*(5-1) + i) = sv5(i)*off(i)
              lbuf%visc(nel*(6-1) + i) = sv6(i)*off(i)
            enddo
            do i=1,nel
              svis(i,1) = lbuf%visc(nel*(1-1) + i)
              svis(i,2) = lbuf%visc(nel*(2-1) + i)
              svis(i,3) = lbuf%visc(nel*(3-1) + i)
              svis(i,4) = lbuf%visc(nel*(4-1) + i)
              svis(i,5) = lbuf%visc(nel*(5-1) + i)
              svis(i,6) = lbuf%visc(nel*(6-1) + i)
            enddo
            if (isvis > 0) then
!         viscous stress output
              do i=1,nel
                lbuf%sigv(nel*(1-1) + i) = svis(i,1)
                lbuf%sigv(nel*(2-1) + i) = svis(i,2)
                lbuf%sigv(nel*(3-1) + i) = svis(i,3)
                lbuf%sigv(nel*(4-1) + i) = svis(i,4)
                lbuf%sigv(nel*(5-1) + i) = svis(i,5)
                lbuf%sigv(nel*(6-1) + i) = svis(i,6)
              enddo
            endif
          elseif (ivisc > 0) then
            do i=1,nel
              lbuf%visc(nel*(1-1) + i) = svis(i,1)
              lbuf%visc(nel*(2-1) + i) = svis(i,2)
              lbuf%visc(nel*(3-1) + i) = svis(i,3)
              lbuf%visc(nel*(4-1) + i) = svis(i,4)
              lbuf%visc(nel*(5-1) + i) = svis(i,5)
              lbuf%visc(nel*(6-1) + i) = svis(i,6)
            enddo
            if (isvis > 0) then
!         viscous stress output
              do i=1,nel
                lbuf%sigv(nel*(1-1) + i) = svis(i,1)
                lbuf%sigv(nel*(2-1) + i) = svis(i,2)
                lbuf%sigv(nel*(3-1) + i) = svis(i,3)
                lbuf%sigv(nel*(4-1) + i) = svis(i,4)
                lbuf%sigv(nel*(5-1) + i) = svis(i,5)
                lbuf%sigv(nel*(6-1) + i) = svis(i,6)
              enddo
            endif
          endif
!-----------------------------------------------------------------------
!     failure for law no user ---
!-----------------------------------------------------------------------
          if ((itask==0).and.(imon_mat==1))call startime(121,1)
          if (nfail > 0 .and. (mtn < 28 .or. mtn == 49)) then
            if (istrain > 0 .or. mtn==24 .or. mtn==25 .or. mtn==15) then
              do i=1,nel
                es1(i) = lbuf%stra(nel*(1-1) + i)
                es2(i) = lbuf%stra(nel*(2-1) + i)
                es3(i) = lbuf%stra(nel*(3-1) + i)
                es4(i) = lbuf%stra(nel*(4-1) + i)
                es5(i) = lbuf%stra(nel*(5-1) + i)
                es6(i) = lbuf%stra(nel*(6-1) + i)
              enddo
            else
              do i=1,nel
                es1(i) = ep20
                es2(i) = ep20
                es3(i) = ep20
                es4(i) = ep20
                es5(i) = ep20
                es6(i) = ep20
              enddo
            endif
!------------------------------------------------------------
!     recovering non-local variable
!------------------------------------------------------------
            inloc = iparg(78,ng)
            if (inloc > 0) then
              ! -> length used for failure criterion parameters scaling is le_max
              le_max(1:nel) = nloc_dmg%le_max(imat)
              el_len => le_max(1:nel)
              ! -> copying the non-local plastic strain increment
              if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
                do i = 1,nel
                  varnl(i)       = max(varnl(i),zero)
                  lbuf%planl(i)  = lbuf%planl(i) + varnl(i)
                  lbuf%epsdnl(i) = varnl(i)/max(dt1,em20)
                  dpla(i) = max(varnl(i),zero)
                enddo
                el_pla => lbuf%planl(1:nel)
              endif
            else
              ! -> length used for failure criterion parameters scaling is the element length
              el_len => deltax(1:nel)
              el_pla => defp(1:nel)
            endif
!---
            do ir = 1,nfail
!
              do i=1,nel
                ss1(i) = lbuf%sig(nel*(1-1) + i)
                ss2(i) = lbuf%sig(nel*(2-1) + i)
                ss3(i) = lbuf%sig(nel*(3-1) + i)
                ss4(i) = lbuf%sig(nel*(4-1) + i)
                ss5(i) = lbuf%sig(nel*(5-1) + i)
                ss6(i) = lbuf%sig(nel*(6-1) + i)
                eint(i) = lbuf%eint(i)*lbuf%vol(i)
                if (mtn/=4.and.mtn/=2) then
                  epsp(i)=off(i)*&
                  &max( abs(dxx(i)),abs(dyy(i)),abs(dzz(i)),&
                  &half*abs(d4(i)),half*abs(d5(i)),half*abs(d6(i)))
                endif
              enddo
!------------------------------------------------------------
!     recovering non-local plastic strain-rate
!------------------------------------------------------------
              if (inloc > 0) then
                if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
                  do i = 1,nel
                    epsp(i) = lbuf%epsdnl(i)
                  enddo
                endif
              endif
!
              if (isorth  > 0 .and. jcvt == 0) then
                call mreploc(&
                &gama,    r11,     r12,     r13,&
                &r21,     r22,     r23,     r31,&
                &r32,     r33,     rx,      ry,&
                &rz,      sx,      sy,      sz,&
                &tx,      ty,      tz,      nel,&
                &jsph)
                call mrotens(1,nel,ss1,ss2,ss3,ss4,ss5,ss6,&
                &r11,r12,r13,&
                &r21,r22,r23,&
                &r31,r32,r33)
! strain
                do i=1,nel
                  es4(i) = half*es4(i)
                  es5(i) = half*es5(i)
                  es6(i) = half*es6(i)
                enddo
                call mrotens(1,nel,es1,es2,es3,es4,es5,es6,&
                &r11,r12,r13,&
                &r21,r22,r23,&
                &r31,r32,r33)
                do i=1,nel
                  es4(i) = two*es4(i)
                  es5(i) = two*es5(i)
                  es6(i) = two*es6(i)
                enddo
! strain rate
                do i=1,nel
                  ep1(i) = dxx(i)
                  ep2(i) = dyy(i)
                  ep3(i) = dzz(i)
                  ep4(i) = half*d4(i)
                  ep5(i) = half*d5(i)
                  ep6(i) = half*d6(i)
                enddo
                call mrotens(1,nel,ep1,ep2,ep3,ep4,ep5,ep6,&
                &r11,r12,r13,&
                &r21,r22,r23,&
                &r31,r32,r33)
                do i=1,nel
                  ep4(i) = two*ep4(i)
                  ep5(i) = two*ep5(i)
                  ep6(i) = two*ep6(i)
                enddo
                ! strain tensor increment
                do i = 1,nel
                  de1(i) = ep1(i)*dt1
                  de2(i) = ep2(i)*dt1
                  de3(i) = ep3(i)*dt1
                  de4(i) = ep4(i)*dt1
                  de5(i) = ep5(i)*dt1
                  de6(i) = ep6(i)*dt1
                enddo
              else          !  strain rate
                do i=1,nel
                  ep1(i) = dxx(i)
                  ep2(i) = dyy(i)
                  ep3(i) = dzz(i)
                  ep4(i) = half*d4(i)
                  ep5(i) = half*d5(i)
                  ep6(i) = half*d6(i)
                enddo
                ! strain tensor increment
                do i = 1,nel
                  de1(i) = ep1(i)*dt1
                  de2(i) = ep2(i)*dt1
                  de3(i) = ep3(i)*dt1
                  de4(i) = two*ep4(i)*dt1
                  de5(i) = two*ep5(i)*dt1
                  de6(i) = two*ep6(i)*dt1
                enddo
              endif
!
!----
              uvarf => fbuf%floc(ir)%var
              irupt  = fbuf%floc(ir)%ilawf
              nvarf  = fbuf%floc(ir)%nvar
              dfmax => fbuf%floc(ir)%dammx
              damini=> fbuf%floc(ir)%damini
              tdel  => fbuf%floc(ir)%tdel
              lf_dammx = fbuf%floc(ir)%lf_dammx
              nparam  = mat_elem%mat_param(imat)%fail(ir)%nuparam
              niparam = mat_elem%mat_param(imat)%fail(ir)%niparam
              uparamf=>mat_elem%mat_param(imat)%fail(ir)%uparam(1:nparam)
              iparamf=> mat_elem%mat_param(imat)%fail(ir)%iparam(1:niparam)
              nfunc  = mat_elem%mat_param(imat)%fail(ir)%nfunc
              ifunc(1:nfunc) = mat_elem%mat_param(imat)%fail(ir)%ifunc(1:nfunc)
              ntabl_fail =  mat_elem%mat_param(imat)%fail(ir)%ntable
              itabl_fail => mat_elem%mat_param(imat)%fail(ir)%table(1:ntabl_fail)
!
              if (irupt == 1)then
!  --- johnson cook
                call fail_johnson(llt ,nparam,nvarf,&
                &tt  ,dt1  ,uparamf,ngl ,&
                &ss1  ,ss2  ,ss3  ,ss4   ,ss5   ,ss6,&
                &dpla ,epsp ,tstar,uvarf ,off   ,&
                &dfmax,tdel )
              elseif(irupt == 2)then
!  --- tuler butcher
                call fail_tbutcher_s(llt ,nparam,nvarf,&
                &tt  ,dt1  ,uparamf,ngl ,&
                &ss1  ,ss2  ,ss3  ,ss4   ,ss5   ,ss6,&
                &uvarf,off ,dfmax ,tdel  )
              elseif(irupt == 3)then
!  --- wilkins
                call fail_wilkins_s(llt ,nparam,nvarf,&
                &tt  ,dt1  ,uparamf,ngl ,&
                &ss1  ,ss2  ,ss3  ,ss4   ,ss5   ,ss6,&
                &dpla ,uvarf,off  ,dfmax ,tdel )
!--------------------------------------------------------------
!  user1
              elseif(irupt==4)then
                if (logical_userl_avail)then
                  user_uelr(1:llt)=gbuf%uelr(1:llt)
                  call eng_userlib_flaw(irupt,&
                  &llt ,nparam,nvarf,nfunc,ifunc,&
                  &npf ,tf  ,tt  ,dt1  ,uparamf,&
                  &ngl ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &ss1  ,ss2 ,ss3  ,ss4  ,ss5  ,ss6 ,&
                  &defp ,dpla,epsp,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:llt) = user_uelr(1:llt)
                else
!!!
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user1 - solid '
                  length=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:length),anmode=aninfo)
                  call arret(2)
                  ! ----------------
!!!
                endif
!   user2
              elseif(irupt==5)then
                if (logical_userl_avail)then
                  user_uelr(1:llt)=gbuf%uelr(1:llt)
                  call eng_userlib_flaw(irupt,&
                  &llt ,nparam,nvarf,nfunc,ifunc,&
                  &npf ,tf  ,tt  ,dt1  ,uparamf,&
                  &ngl ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &ss1  ,ss2 ,ss3  ,ss4  ,ss5  ,ss6 ,&
                  &defp ,dpla,epsp,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:llt) = user_uelr(1:llt)
                else
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user2 - solid '
                  length=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:length),anmode=aninfo)
                  call arret(2)
                  ! ----------------
                endif
!   user3
              elseif(irupt==6)then
                if (logical_userl_avail)then
                  user_uelr(1:llt)=gbuf%uelr(1:llt)
                  call eng_userlib_flaw(irupt,&
                  &llt ,nparam,nvarf,nfunc,ifunc,&
                  &npf ,tf  ,tt  ,dt1  ,uparamf,&
                  &ngl ,ibidon1,ibidon2,ibidon3 ,ibidon4,&
                  &ep1  ,ep2 ,ep3  ,ep4  ,ep5  ,ep6 ,&
                  &es1  ,es2 ,es3  ,es4  ,es5  ,es6 ,&
                  &ss1  ,ss2 ,ss3  ,ss4  ,ss5  ,ss6 ,&
                  &defp ,dpla,epsp,uvarf,off  ,&
                  &deltax,voln,user_uelr,bidon4,bidon5)
                  gbuf%uelr(1:llt) = user_uelr(1:llt)
                else
                  ! ----------------
                  ! error to be printed & exit
                  option='/fail/user3 - solid '
                  length=len_trim(option)
                  call ancmsg(msgid=257,c1=option(1:length),anmode=aninfo)
                  call arret(2)
                  ! ----------------
!!!
                endif
!-------------------------------------------------------------------
              elseif(irupt == 8)then
!---- jc + spalling
                call fail_spalling_s(llt ,nparam,nvarf,&
                &tt  ,dt1  ,uparamf,ngl ,&
                &ss1  ,ss2  ,ss3  ,ss4   ,ss5   ,ss6,&
                &dpla ,epsp ,tstar,uvarf ,off   ,&
                &dfmax,tdel ,lbuf%off)
!
              elseif(irupt == 9)then
                call fail_wierzbicki_s(llt ,nparam,nvarf,&
                &tt  ,dt1  ,uparamf,ngl ,&
                &ss1  ,ss2  ,ss3  ,ss4   ,ss5   ,ss6,&
                &dpla ,el_pla ,uvarf,off   ,dfmax ,&
                &tdel )
              elseif(irupt == 10)then
!---- strain tension
                call fail_tensstrain_s(llt ,nparam,nvarf,nfunc,ifunc      ,&
                &npf ,tf  ,tt  ,dt1  ,uparamf,&
                &ngl ,deltax   ,tstar ,ismstr,&
                &es1 ,es2 ,es3 ,es4  ,es5  ,es6     ,&
                &ss1 ,ss2 ,ss3 ,ss4  ,ss5  ,ss6     ,&
                &epsp,uvarf,off,dfmax,tdel    ,&
                &mfxx   ,mfxy   ,mfxz   ,mfyx    ,mfyy   ,mfyz   ,&
                &mfzx   ,mfzy   ,mfzz   ,lbuf%dmgscl)
!
              elseif(irupt == 11)then
!---- energy failure
                call fail_energy_s(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,npf      ,&
                &tf       ,tt       ,dt1      ,uparamf,ngl ,epsp     ,&
                &uvarf    ,off      ,dfmax    ,tdel     ,lbuf%dmgscl,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &de1      ,de2      ,de3      ,de4      ,de5      ,de6      )
              elseif (irupt == 13) then
!---- chang - chang
                call fail_changchang_s(&
                &llt      ,nparam   ,nvarf    ,uparamf,uvarf,&
                &tt       ,ipg      ,ilay     ,npg      ,ngl      ,&
                &lbuf%dmgscl,dfmax  ,off      ,lbuf%off ,gbuf%noff,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss6      ,&
                &tdel     ,lf_dammx )
              elseif(irupt == 14)then
! --- hashin    failure model
                do i=1,nel
                  epsp(i) = max(abs(ep1(i)),abs(ep2(i)),abs(ep3(i)),em20)
                enddo
                call fail_hashin_s(&
                &llt    ,nvarf  ,ilay     ,npg           ,&
                &tt     ,dt1    ,uparamf  ,&
                &ngl    ,off    ,gbuf%noff,ss1           ,&
                &ss2    ,ss3    ,ss4      ,ss5           ,ss6           ,&
                &uvarf  ,nparam ,dfmax    ,tdel          ,&
                &epsp   ,lf_dammx)
              elseif(irupt == 16)then
! --- modified puck failure model
                call fail_puck_s(&
                &llt    ,nvarf  ,ilay     ,npg           ,&
                &tt     ,dt1    ,uparamf  ,&
                &ngl    ,off    ,gbuf%noff,ss1           ,&
                &ss2    ,ss3    ,ss4     ,ss5            ,ss6           ,&
                &uvarf  ,nparam,dfmax    ,lf_dammx       ,tdel     )
              elseif(irupt == 18)then
! --- ladeveze delamination damage model
                call fail_ladeveze(&
                &llt    ,nvarf  ,ilay     ,npg           ,&
                &tt     ,dt1      ,uparamf,&
                &ngl    ,off    ,gbuf%noff,ss1           ,&
                &ss2    ,ss3    ,ss4    ,ss5      ,ss6           ,&
                &uvarf  ,nparam ,dfmax    ,tdel    )
              elseif (irupt == 23) then
! ---   tabulated failure model
                call fail_tab_s(&
                &llt      ,nvarf    ,npf      ,tf       ,tt        ,&
                &uparamf  ,ngl      ,el_len   ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5       ,ss6       ,&
                &dpla     ,epsp     ,tstar    ,uvarf    ,ntabl_fail,itabl_fail,&
                &off      ,table    ,dfmax    ,tdel     ,nfunc     ,ifunc     )
              elseif (irupt == 24) then
!   --- orthotropic strain failure
                call fail_orthstrain(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,&
                &npf      ,tf       ,tt       ,dt1      ,uparamf,ismstr,&
                &ep1      ,ep2      ,ep3      ,ep4      ,ep5      ,ep6     ,&
                &es1      ,es2      ,es3      ,es4      ,es5      ,es6     ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6     ,&
                &uvarf    ,off      ,ipg      ,ngl      ,dfmax    ,tdel    ,&
                &gbuf%uelr,npg      ,deltax   )
              elseif (irupt == 27) then
! ---   extended mohr coulomb failure model
                call fail_emc(&
                &llt      ,nvarf    ,tt       ,&
                &dt1      ,uparamf  ,ngl      ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6  ,&
                &el_pla   ,dpla     ,epsp     ,uvarf    ,&
                &off      ,dfmax    ,tdel     )
              elseif (irupt == 29) then
! ---   mit wierzbicki sahraei electric battery failure
                call fail_sahraei_s(&
                &llt      ,nfunc    ,ifunc    ,npf      ,tf       ,&
                &tt       ,ngl      ,uparamf   ,&
                &es1      ,es2      ,es3      ,es4      ,es5      ,es6 ,&
                &off      ,dfmax    ,tdel     ,deltax   ,&
                &nvarf    ,uvarf    )
              elseif (irupt == 30) then
!  --- biquadratic failure model
                call fail_biquad_s(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,el_len   ,&
                &npf      ,tf       ,tt       ,uparamf  ,tdel     ,&
                &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,lbuf%dmgscl,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      )
              elseif (irupt == 34) then
!  --- cockroft-latham failure model
                call fail_cockroft_s(llt ,nparam,nvarf,&
                &tt      ,dt1       ,uparamf  ,ngl     ,&
                &ep1     ,ep2       ,ep3      ,ep4      ,ep5           ,ep6 ,&
                &es1     ,es2       ,es3      ,es4      ,es5           ,es6 ,&
                &ss1     ,ss2       ,ss3      ,ss4      ,ss5           ,ss6 ,&
                &el_pla  ,dpla      ,epsp     ,uvarf    ,off           ,&
                &dfmax   ,tdel)
              elseif (irupt == 36) then
!  --- visual failure model
                call fail_visual_s(&
                &llt     ,nparam    ,nvarf    ,tt       ,dt1       ,uparamf,&
                &es1     ,es2       ,es3      ,es4      ,es5       ,es6 ,&
                &ss1     ,ss2       ,ss3      ,ss4      ,ss5       ,ss6 ,&
                &uvarf   ,off       ,ngl      ,dfmax    ,ismstr    )
!
              elseif (irupt == 37) then
! ---       tabulated failure model (old, obsolete version)
                call fail_tab_old_s(&
                &llt      ,nvarf    ,npf      ,tf       ,tt       ,&
                &uparamf  ,ngl      ,el_len   ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &el_pla   ,dpla     ,epsp     ,tstar    ,uvarf    ,&
                &off      ,dfmax    ,tdel     ,&
                &nfunc    ,ifunc )
!
              elseif (irupt == 38) then
!  --- orthotropic biquadratic failure model
                call fail_orthbiquad_s(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,&
                &npf      ,tf       ,tt       ,dt1      ,uparamf,&
                &ngl      ,dpla     ,epsp     ,uvarf    ,off      ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &dfmax    ,tdel     ,el_len   )
!
              elseif (irupt == 39) then
!  --- gene1 failure model
                call fail_gene1_s(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,lbuf%off ,&
                &npf      ,tf       ,tt       ,dt1      ,uparamf,ipg ,&
                &ngl      ,gbuf%dt  ,epsp     ,uvarf    ,off      ,npg      ,&
                &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &tempel   ,voln     ,dfmax    ,tdel     ,deltax   ,table    ,&
                &ir       ,elbuf_tab(ng),ilay ,ntabl_fail,itabl_fail,lf_dammx,&
                &niparam  ,iparamf  )
!
              elseif (irupt == 40) then
!  --- rtcl failure model
                call fail_rtcl_s(&
                &llt      ,nparam   ,nvarf    ,tt       ,dt1      ,uparamf,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,tdel     )
!
              elseif (irupt == 41) then
!---- tabulated failure model version 2
                call fail_tab2_s(&
                &llt      ,nparam   ,nvarf    ,nfunc    ,ifunc    ,&
                &npf      ,table    ,tf       ,tt       ,uparamf,&
                &ngl      ,el_len   ,dpla     ,epsp     ,uvarf    ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &tempel   ,off      ,dfmax    ,tdel     ,lbuf%dmgscl,&
                &gbuf%uelr,ipg      ,npg      ,lbuf%off ,ntabl_fail,itabl_fail)
!
              elseif (irupt == 42) then
!---- inievo failure model
                call fail_inievo_s(&
                &llt      ,nparam   ,nvarf    ,&
                &table    ,ntabl_fail,itabl_fail,tt       ,uparamf,&
                &ngl      ,el_len   ,dpla     ,epsp     ,uvarf    ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &el_pla   ,tempel   ,sigy     ,off      ,dfmax    ,&
                &tdel     ,lbuf%dmgscl,gbuf%uelr,ipg      ,npg      ,&
                &lbuf%off ,damini   ,gbuf%vol ,inloc    )
!
              elseif (irupt == 43) then
!  --- syazwan failure model
                call fail_syazwan_s(&
                &llt     ,uparamf,nparam,uvarf    ,nvarf     ,&
                &tt      ,ngl         ,ipg      ,dpla    ,tdel      ,&
                &ss1     ,ss2         ,ss3      ,ss4     ,ss5       ,ss6      ,&
                &dfmax   ,nfunc       ,ifunc    ,el_len  ,off       ,&
                &npf     ,tf          ,gbuf%uelr,npg     ,lbuf%off  )
!
              elseif (irupt == 44) then
! --- tsai-wu failure model
                call fail_tsaiwu_s(&
                &llt      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
                &dt1      ,uparamf  ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &uvarf    ,nparam   ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
              elseif (irupt == 45) then
! --- tsai-hill failure model
                call fail_tsaihill_s(&
                &llt      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
                &dt1      ,uparamf  ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &uvarf    ,nparam   ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
              elseif (irupt == 46) then
! --- hoffman failure model
                call fail_hoffman_s(&
                &llt      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
                &dt1      ,uparamf  ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &uvarf    ,nparam   ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
              elseif (irupt == 47) then
!---- maximum strain failure model
                call fail_maxstrain_s(&
                &llt      ,nvarf    ,ipg      ,ilay     ,npg      ,tt       ,&
                &dt1      ,uparamf  ,ngl      ,off      ,lbuf%off ,gbuf%noff,&
                &es1      ,es2      ,es3      ,es4      ,es5      ,es6      ,&
                &uvarf    ,nparam   ,dfmax    ,lf_dammx ,tdel     ,lbuf%dmgscl)
!
              elseif (irupt == 48) then
!---- orthotropic energy failure
                call fail_orthenerg_s(&
                &llt      ,nparam   ,nvarf    ,uparamf,uvarf,ngl     ,&
                &npg      ,ipg      ,ilay     ,off      ,lbuf%off ,gbuf%noff,&
                &de1      ,de2      ,de3      ,de4      ,de5      ,de6      ,&
                &ss1      ,ss2      ,ss3      ,ss4      ,ss5      ,ss6      ,&
                &tt       ,tdel     ,dfmax    ,deltax   ,lbuf%dmgscl)
!---------
              endif ! irupt
!
!--------------------------------------------------------
!         damaged stresses
!---------------------------------------------------------
              ! -> isotropic stress softening
              if (dmg_flag == 1) then
                do i = 1,nel
                  ss1(i) = ss1(i)*lbuf%dmgscl(i)
                  ss2(i) = ss2(i)*lbuf%dmgscl(i)
                  ss3(i) = ss3(i)*lbuf%dmgscl(i)
                  ss4(i) = ss4(i)*lbuf%dmgscl(i)
                  ss5(i) = ss5(i)*lbuf%dmgscl(i)
                  ss6(i) = ss6(i)*lbuf%dmgscl(i)
                enddo
                ! -> orthotropic stress softening
              elseif (dmg_flag == 6) then
                do i = 1,nel
                  ss1(i) = ss1(i)*lbuf%dmgscl(nel*(1-1) + i)
                  ss2(i) = ss2(i)*lbuf%dmgscl(nel*(2-1) + i)
                  ss3(i) = ss3(i)*lbuf%dmgscl(nel*(3-1) + i)
                  ss4(i) = ss4(i)*lbuf%dmgscl(nel*(4-1) + i)
                  ss5(i) = ss5(i)*lbuf%dmgscl(nel*(5-1) + i)
                  ss6(i) = ss6(i)*lbuf%dmgscl(nel*(6-1) + i)
                enddo
              endif
!
!---------
              if(isorth  > 0 .and. jcvt == 0) then
                call mrotens(1,nel,&
                &ss1 ,ss2 ,ss3 ,&
                &ss4 ,ss5 ,ss6 ,&
                &r11,r21,r31,&
                &r12,r22,r32,&
                &r13,r23,r33 )
              endif
!
              do i=1,nel
                stor1 = lbuf%sig(nel*(1-1) + i)
                stor2 = lbuf%sig(nel*(2-1) + i)
                stor3 = lbuf%sig(nel*(3-1) + i)
                stor4 = lbuf%sig(nel*(4-1) + i)
                stor5 = lbuf%sig(nel*(5-1) + i)
                stor6 = lbuf%sig(nel*(6-1) + i)
                e1=dxx(i)*(-stor1+ss1(i))
                e2=dyy(i)*(-stor2+ss2(i))
                e3=dzz(i)*(-stor3+ss3(i))
                e4=d4(i) *(-stor4+ss4(i))
                e5=d5(i) *(-stor5+ss5(i))
                e6=d6(i) *(-stor6+ss6(i))
                eint(i)=lbuf%eint(i)*lbuf%vol(i)&
                &+(e1+e2+e3+e4+e5+e6)*(voln(i)- half*dvol(i))*dt1*half
                lbuf%eint(i) = eint(i)/max(lbuf%vol(i),em20)
              end do
!
              do i=1,nel
                lbuf%sig(nel*(1-1) + i) = ss1(i)
                lbuf%sig(nel*(2-1) + i) = ss2(i)
                lbuf%sig(nel*(3-1) + i) = ss3(i)
                lbuf%sig(nel*(4-1) + i) = ss4(i)
                lbuf%sig(nel*(5-1) + i) = ss5(i)
                lbuf%sig(nel*(6-1) + i) = ss6(i)
              enddo
!
            enddo   !  ir = 1,nfail
!
!------------------------------------------------------------
!     variable to regularize with non-local
!------------------------------------------------------------
            if (inloc > 0) then
              if (elbuf_tab(ng)%bufly(ilay)%l_pla > 0) then
                do i = 1,nel
                  if (off(i) == one) then
                    varnl(i) = defp(i)
                  else
                    varnl(i) = zero
                  endif
                enddo
              endif
            endif
          endif     !  nfail > 0  & user laws
          if ((itask==0).and.(imon_mat==1))call stoptime(121,1)
!-----------------------------------------------------------------
          if(ipartsph/=0)then
            do i=1,nel
              if(off(i) > zero .and. off(i) < one)then
! replace solid w/sph within the same cycle.
                off(i)=zero
                lbuf%sig(nel*(1-1) + i)  = zero
                lbuf%sig(nel*(2-1) + i)  = zero
                lbuf%sig(nel*(3-1) + i)  = zero
                lbuf%sig(nel*(4-1) + i)  = zero
                lbuf%sig(nel*(5-1) + i)  = zero
                lbuf%sig(nel*(6-1) + i)  = zero
                if (ivisc > 0) then
                  lbuf%visc(nel*(1-1) + i) = zero
                  lbuf%visc(nel*(2-1) + i) = zero
                  lbuf%visc(nel*(3-1) + i) = zero
                  lbuf%visc(nel*(4-1) + i) = zero
                  lbuf%visc(nel*(5-1) + i) = zero
                  lbuf%visc(nel*(6-1) + i) = zero
                  if (isvis > 0) then
!           viscous stress output
                    lbuf%sigv(nel*(1-1) + i) = zero
                    lbuf%sigv(nel*(2-1) + i) = zero
                    lbuf%sigv(nel*(3-1) + i) = zero
                    lbuf%sigv(nel*(4-1) + i) = zero
                    lbuf%sigv(nel*(5-1) + i) = zero
                    lbuf%sigv(nel*(6-1) + i) = zero
                  end if
                end if
                stifn(i)          = zero
                qvis(i)           = zero
                svis(i,1)         = zero
                svis(i,2)         = zero
                svis(i,3)         = zero
                svis(i,4)         = zero
                svis(i,5)         = zero
                svis(i,6)         = zero
              end if
            end do
          end if
!-----------------------------------------------------------------
          if (iexpan > 0 .or. jthe <0) then
            do i=1,nel
              if(off(i) == zero) cycle
              lbuf%temp(i) = tempel(i)
            enddo
          endif
!----------------------------------------------------------------
!     Shooting nodes algorithm activation
!----------------------------------------------------------------
          do i = 1,nel
            if ((off_old(i) > zero) .and. (off(i) == zero)) then
              idel7nok = 1
            end if
          end do
!----------------------------------------------------------------
          if (impl_s > 0) then
            call put_etfac(nel ,et  ,mtn)
            call putsignor3(1,nel ,mtn,iptr,ipts,iptt,al_imp ,signor)
          end if
!-----------
!-----------------------------------------------------------------
!  sound speed (ssp)  post-treatment
!-----------------------------------------------------------------
          if(elbuf_tab(ng)%bufly(ilay)%l_ssp /=0 )then
            lbuf%ssp(1:nel) = cxx(1:nel)
          endif

!-----------------------------------------------------------------
          return
        end
!-----
      end module mmain_mod

