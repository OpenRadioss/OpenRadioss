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
      !||    mulawc_mod   ../engine/source/materials/mat_share/mulawc.F90
      !||--- called by ------------------------------------------------------
      !||    cmain3       ../engine/source/materials/mat_share/cmain3.F
      !||====================================================================
      module mulawc_mod
      contains
!! \brief routine to compute the material laws for shell elements
      !||====================================================================
      !||    mulawc                    ../engine/source/materials/mat_share/mulawc.F90
      !||--- called by ------------------------------------------------------
      !||    cmain3                    ../engine/source/materials/mat_share/cmain3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                    ../engine/source/output/message/message.F
      !||    arret                     ../engine/source/system/arret.F
      !||    coqini_wm                 ../engine/source/elements/shell/coqini.F
      !||    damping_range_shell       ../engine/source/general_controls/damping/damping_range_shell.F90
      !||    fail_biquad_c             ../engine/source/materials/fail/biquad/fail_biquad_c.F
      !||    fail_changchang_c         ../engine/source/materials/fail/changchang/fail_changchang_c.F
      !||    fail_cockroft_c           ../engine/source/materials/fail/cockroft_latham/fail_cockroft_c.F
      !||    fail_composite_c          ../engine/source/materials/fail/composite/fail_composite_c.F90
      !||    fail_energy_c             ../engine/source/materials/fail/energy/fail_energy_c.F
      !||    fail_fabric_c             ../engine/source/materials/fail/fabric/fail_fabric_c.F
      !||    fail_fld_c                ../engine/source/materials/fail/fld/fail_fld_c.F
      !||    fail_fld_xfem             ../engine/source/materials/fail/fld/fail_fld_xfem.F
      !||    fail_gene1_c              ../engine/source/materials/fail/gene1/fail_gene1_c.F
      !||    fail_hashin_c             ../engine/source/materials/fail/hashin/fail_hashin_c.F
      !||    fail_hc_dsse_c            ../engine/source/materials/fail/hc_dsse/fail_hc_dsse_c.F
      !||    fail_hoffman_c            ../engine/source/materials/fail/hoffman/fail_hoffman_c.F
      !||    fail_inievo_c             ../engine/source/materials/fail/inievo/fail_inievo_c.F
      !||    fail_johnson_c            ../engine/source/materials/fail/johnson_cook/fail_johnson_c.F
      !||    fail_johnson_xfem         ../engine/source/materials/fail/johnson_cook/fail_johnson_xfem.F
      !||    fail_lemaitre_c           ../engine/source/materials/fail/lemaitre/fail_lemaitre_c.F90
      !||    fail_maxstrain_c          ../engine/source/materials/fail/max_strain/fail_maxstrain_c.F
      !||    fail_nxt_c                ../engine/source/materials/fail/nxt/fail_nxt_c.F
      !||    fail_orthbiquad_c         ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_c.F
      !||    fail_orthenerg_c          ../engine/source/materials/fail/orthenerg/fail_orthenerg_c.F
      !||    fail_orthstrain_c         ../engine/source/materials/fail/orthstrain/fail_orthstrain_c.F
      !||    fail_puck_c               ../engine/source/materials/fail/puck/fail_puck_c.F
      !||    fail_rtcl_c               ../engine/source/materials/fail/rtcl/fail_rtcl_c.F
      !||    fail_setoff_c             ../engine/source/materials/fail/fail_setoff_c.F
      !||    fail_setoff_npg_c         ../engine/source/materials/fail/fail_setoff_npg_c.F
      !||    fail_setoff_wind_frwave   ../engine/source/materials/fail/fail_setoff_wind_frwave.F
      !||    fail_syazwan_c            ../engine/source/materials/fail/syazwan/fail_syazwan_c.F
      !||    fail_tab2_c               ../engine/source/materials/fail/tabulated/fail_tab2_c.F
      !||    fail_tab_c                ../engine/source/materials/fail/tabulated/fail_tab_c.F
      !||    fail_tab_old_c            ../engine/source/materials/fail/tabulated/fail_tab_old_c.F
      !||    fail_tab_old_xfem         ../engine/source/materials/fail/tabulated/fail_tab_old_xfem.F
      !||    fail_tab_xfem             ../engine/source/materials/fail/tabulated/fail_tab_xfem.F
      !||    fail_tbutcher_c           ../engine/source/materials/fail/tuler_butcher/fail_tbutcher_c.F
      !||    fail_tbutcher_xfem        ../engine/source/materials/fail/tuler_butcher/fail_tbutcher_xfem.F
      !||    fail_tensstrain_c         ../engine/source/materials/fail/tensstrain/fail_tensstrain_c.F
      !||    fail_tsaihill_c           ../engine/source/materials/fail/tsaihill/fail_tsaihill_c.F
      !||    fail_tsaiwu_c             ../engine/source/materials/fail/tsaiwu/fail_tsaiwu_c.F
      !||    fail_visual_c             ../engine/source/materials/fail/visual/fail_visual_c.F
      !||    fail_wierzbicki_c         ../engine/source/materials/fail/wierzbicki/fail_wierzbicki_c.F
      !||    fail_wilkins_c            ../engine/source/materials/fail/wilkins/fail_wilkins_c.F
      !||    fail_wind_frwave          ../engine/source/materials/fail/alter/fail_wind_frwave.F
      !||    fail_wind_xfem            ../engine/source/materials/fail/alter/fail_wind_xfem.F
      !||    m25delam                  ../engine/source/materials/mat/mat025/m25delam.F
      !||    nvar                      ../engine/source/input/nvar.F
      !||    prony_modelc              ../engine/source/materials/visc/prony_modelc.F
      !||    putsignorc3               ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    rotov                     ../engine/source/airbag/roto.F
      !||    sigeps01c                 ../engine/source/materials/mat/mat001/sigeps01c.F
      !||    sigeps02c                 ../engine/source/materials/mat/mat002/sigeps02c.F
      !||    sigeps104c                ../engine/source/materials/mat/mat104/sigeps104c.F
      !||    sigeps107c                ../engine/source/materials/mat/mat107/sigeps107c.F
      !||    sigeps109c                ../engine/source/materials/mat/mat109/sigeps109c.F
      !||    sigeps110c                ../engine/source/materials/mat/mat110/sigeps110c.F
      !||    sigeps112c                ../engine/source/materials/mat/mat112/sigeps112c.F
      !||    sigeps119c                ../engine/source/materials/mat/mat119/sigeps119c.F
      !||    sigeps121c                ../engine/source/materials/mat/mat121/sigeps121c.F
      !||    sigeps122c                ../engine/source/materials/mat/mat122/sigeps122c.F
      !||    sigeps125c                ../engine/source/materials/mat/mat125/sigeps125c.F90
      !||    sigeps127c                ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||    sigeps128c                ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||    sigeps158c                ../engine/source/materials/mat/mat158/sigeps158c.F
      !||    sigeps15c                 ../engine/source/materials/mat/mat015/sigeps15c.F
      !||    sigeps19c                 ../engine/source/materials/mat/mat019/sigeps19c.F
      !||    sigeps22c                 ../engine/source/materials/mat/mat022/sigeps22c.F
      !||    sigeps25c                 ../engine/source/materials/mat/mat025/sigeps25c.F
      !||    sigeps25cp                ../engine/source/materials/mat/mat025/sigeps25cp.F
      !||    sigeps27c                 ../engine/source/materials/mat/mat027/sigeps27c.F
      !||    sigeps32c                 ../engine/source/materials/mat/mat032/sigeps32c.F
      !||    sigeps34c                 ../engine/source/materials/mat/mat034/sigeps34c.F
      !||    sigeps35c                 ../engine/source/materials/mat/mat035/sigeps35c.F
      !||    sigeps36c                 ../engine/source/materials/mat/mat036/sigeps36c.F
      !||    sigeps42c                 ../engine/source/materials/mat/mat042/sigeps42c.F
      !||    sigeps43c                 ../engine/source/materials/mat/mat043/sigeps43c.F
      !||    sigeps44c                 ../engine/source/materials/mat/mat044/sigeps44c.F
      !||    sigeps45c                 ../engine/source/materials/mat/mat045/sigeps45c.F
      !||    sigeps48c                 ../engine/source/materials/mat/mat048/sigeps48c.F
      !||    sigeps52c                 ../engine/source/materials/mat/mat052/sigeps52c.F
      !||    sigeps55c                 ../engine/source/materials/mat/mat055/sigeps55c.F
      !||    sigeps56c                 ../engine/source/materials/mat/mat056/sigeps56c.F
      !||    sigeps57c                 ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||    sigeps58c                 ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sigeps60c                 ../engine/source/materials/mat/mat060/sigeps60c.F
      !||    sigeps62c                 ../engine/source/materials/mat/mat062/sigeps62c.F
      !||    sigeps63c                 ../engine/source/materials/mat/mat063/sigeps63c.F
      !||    sigeps64c                 ../engine/source/materials/mat/mat064/sigeps64c.F
      !||    sigeps65c                 ../engine/source/materials/mat/mat065/sigeps65c.F
      !||    sigeps66c                 ../engine/source/materials/mat/mat066/sigeps66c.F
      !||    sigeps69c                 ../engine/source/materials/mat/mat069/sigeps69c.F
      !||    sigeps71c                 ../engine/source/materials/mat/mat071/sigeps71c.F
      !||    sigeps72c                 ../engine/source/materials/mat/mat072/sigeps72c.F
      !||    sigeps73c                 ../engine/source/materials/mat/mat073/sigeps73c.F
      !||    sigeps76c                 ../engine/source/materials/mat/mat076/sigeps76c.F
      !||    sigeps78c                 ../engine/source/materials/mat/mat078/sigeps78c.F
      !||    sigeps80c                 ../engine/source/materials/mat/mat080/sigeps80c.F
      !||    sigeps82c                 ../engine/source/materials/mat/mat082/sigeps82c.F
      !||    sigeps85c_void            ../engine/source/materials/mat/mat085/sigeps85c_void.F
      !||    sigeps86c                 ../engine/source/materials/mat/mat086/sigeps86c.F
      !||    sigeps87c                 ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||    sigeps88c                 ../engine/source/materials/mat/mat088/sigeps88c.F
      !||    sigeps93c                 ../engine/source/materials/mat/mat093/sigeps93c.F
      !||    sigeps96c                 ../engine/source/materials/mat/mat096/sigeps96c.F
      !||    startime                  ../engine/source/system/timer_mod.F90
      !||    stoptime                  ../engine/source/system/timer_mod.F90
      !||    urotov                    ../engine/source/airbag/uroto.F
      !||    xfem_crk_dir              ../engine/source/elements/xfem/xfem_crk_dir.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||    damping_range_shell_mod   ../engine/source/general_controls/damping/damping_range_shell.F90
      !||    dt_mod                    ../engine/source/modules/dt_mod.F
      !||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    fail_composite_c_mod      ../engine/source/materials/fail/composite/fail_composite_c.F90
      !||    fail_lemaitre_c_mod       ../engine/source/materials/fail/lemaitre/fail_lemaitre_c.F90
      !||    fail_param_mod            ../common_source/modules/mat_elem/fail_param_mod.F90
      !||    failwave_mod              ../common_source/modules/failwave_mod.F
      !||    file_descriptor_mod       ../engine/source/modules/file_descriptor_mod.F90
      !||    mat_elem_mod              ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    matparam_def_mod          ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    message_mod               ../engine/share/message_module/message_mod.F
      !||    nlocal_reg_mod            ../common_source/modules/nlocal_reg_mod.F
      !||    sensor_mod                ../common_source/modules/sensor_mod.F90
      !||    sigeps125c_mod            ../engine/source/materials/mat/mat125/sigeps125c.F90
      !||    sigeps127c_mod            ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||    sigeps128c_mod            ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||    sigeps57c_mod             ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||    sigeps87c_mod             ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||    stack_mod                 ../engine/share/modules/stack_mod.F
      !||    table_mod                 ../engine/share/modules/table_mod.F
      !||    timer_mod                 ../engine/source/system/timer_mod.F90
      !||====================================================================
        subroutine mulawc(timers,elbuf_str ,&
        & jft      ,jlt      ,nel      ,pm        ,for      ,mom      , &
        & gstr     ,thk      ,eint     ,off       ,dir_a    ,dir_b    , &
        & mat      ,area     ,exx      ,eyy       ,exy      ,exz      , &
        & eyz      ,kxx      ,kyy      ,kxy       ,geo      ,thk_ly   , &
        & pid      ,tf       ,npf      ,mtn       ,dt1c     ,dm       , &
        & bufmat   ,ssp      ,rho      ,viscmx    ,ipla     ,iofc     , &
        & indx     ,ngl      ,thkly    ,matly     ,zcfac    ,mat_elem , &
        & shf      ,gs       ,sigy     ,thk0      ,epsd_pg,           &
        & posly    ,igeo     ,ipm      ,failwave  ,fwave_el ,           &
        & ifailure ,aldt     ,tempel   ,die       ,fheat    ,           &
        & table     ,ixfem   ,elcrkini ,                                &
        & sensors  ,ng       ,idt_therm,theaccfact,                     &
        & dir1_crk ,dir2_crk ,iparg    ,jhbe      ,ismstr   ,jthe     , &
        & tensx    ,ir       ,is       ,nlay      ,npt      ,ixlay    , &
        & ixel     ,ithk     ,f_def    ,ishplyxfem                    , &
        & itask    ,isubstack,stack    ,alpe                ,           &
        & ply_exx  ,ply_eyy  ,ply_exy  ,ply_exz   ,ply_eyz  ,ply_f    , &
        & varnl    ,etimp    ,nloc_dmg ,nlay_max  ,laynpt_max,dt      , &
        & ncycle   ,snpc     ,stf      ,impl_s    ,imconv    ,npropgi , &
        & npropmi  ,npropm   ,npropg   ,imon_mat  ,numgeo    ,          &
        & numstack ,dt1      ,tt       ,nxlaymax  ,idel7nok ,userl_avail, &
        & maxfunc  ,nummat   ,varnl_npttot,sbufmat,sdir_a   ,sdir_b ,nparg,&
        & idamp_freq_range,damp_buf)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use timer_mod
          use table_mod
          use mat_elem_mod
          use stack_mod
          use failwave_mod
          use message_mod
          use nlocal_reg_mod
          use sensor_mod
          use sigeps57c_mod
          use sigeps87c_mod
          use sigeps125c_mod
          use sigeps127c_mod
          use sigeps128c_mod
          use elbufdef_mod
          use dt_mod
          use file_descriptor_mod
          use constant_mod
          use damping_range_shell_mod
          use matparam_def_mod
          use fail_param_mod
          use fail_lemaitre_c_mod
          use fail_composite_c_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "my_real.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(timer_) :: timers
          integer, intent(in) :: ir
          integer, intent(in) :: is
          integer, intent(in) :: jft
          integer, intent(in) :: jlt
          integer, intent(in) :: nel
          integer, intent(in) :: npt
          integer, intent(in) :: mtn
          integer, intent(in) :: ipla
          integer, intent(in) :: ng
          integer, intent(in) :: nlay
          integer, intent(in) :: ismstr
          integer, intent(in) :: ixfem
          integer, intent(in) :: ifailure
          integer, intent(in) :: jhbe
          integer, intent(in) :: ixlay
          integer, intent(in) :: ixel
          integer, intent(in) :: ithk
          integer, intent(in) :: jthe
          integer, intent(in) :: isubstack
          integer, intent(in) :: itask
          integer, intent(in) :: ishplyxfem
          integer, intent(in) :: nlay_max
          integer, intent(in) :: laynpt_max
          integer, intent(in) :: userl_avail        ! flag for user libraries availability
          integer, intent(in) :: maxfunc            ! maximum number of functions
          integer, intent(in) :: nxlaymax           ! xfem parameter, maximum number of layers
          integer, intent(in) :: impl_s
          integer, intent(in) :: imconv
          integer, intent(in) :: npropgi
          integer, intent(in) :: npropmi
          integer, intent(in) :: npropm
          integer, intent(in) :: npropg
          integer, intent(in) :: imon_mat
          integer, intent(in) :: numgeo
          integer, intent(in) :: nummat
          integer, intent(in) :: numstack
          integer, intent(in) :: varnl_npttot
          integer, intent(in) :: sbufmat
          integer, intent(in) :: sdir_a
          integer, intent(in) :: sdir_b
          integer, intent(in) :: stf
          integer, intent(in) :: nparg
          integer, intent(in) :: snpc
          integer, intent(in) :: ncycle
          integer, intent(in) :: idt_therm
          integer, intent(in),dimension(mvsiz) :: mat
          integer, intent(in),dimension(mvsiz) :: pid
          integer, intent(in),dimension(mvsiz) :: ngl
          integer, intent(in),dimension(npropgi,numgeo) :: igeo
          integer, intent(in),dimension(npropmi,nummat) :: ipm
          integer, intent(in),dimension(nparg) :: iparg
          integer, intent(in),dimension(snpc)  :: npf
          my_real, intent(in),dimension(mvsiz) :: epsd_pg !< global element strain rate in Gauss pt
          integer, intent(in) :: idamp_freq_range         ! flag for damping frequency range
          !  
          integer, intent(inout) :: idel7nok    ! element deletion flag in contact interfaces
          integer, intent(inout) :: iofc
          integer, intent(inout),dimension(mvsiz*nlay_max) :: matly
          integer, intent(inout),dimension(mvsiz*nlay_max) :: indx
          integer, intent(inout),dimension(nel)            :: fwave_el
          integer, intent(inout),dimension(nxlaymax,mvsiz) :: elcrkini
          !
          my_real, intent(in) :: dt1
          my_real, intent(in) :: tt
          my_real, intent(in) :: theaccfact
          my_real, intent(in) ,dimension(npropm,nummat) :: pm
          my_real, intent(in) ,dimension(npropg,numgeo) :: geo
          my_real, intent(in) ,dimension(sbufmat)       :: bufmat
          my_real, intent(in) ,dimension(stf)           ::  tf
          !
          my_real, intent(inout) :: dm
          my_real, intent(inout), dimension(nel,5) :: for
          my_real, intent(inout), dimension(nel,3) :: mom
          my_real, intent(inout), dimension(nel,8) :: gstr
          my_real, intent(inout), dimension(nel)   :: thk
          my_real, intent(inout), dimension(nel,2) :: eint
          my_real, intent(inout), dimension(nel,5) :: tensx
          !
          my_real, intent(inout), dimension(mvsiz) :: kxx
          my_real, intent(inout), dimension(mvsiz) :: kyy
          my_real, intent(inout), dimension(mvsiz) :: kxy
          !
          my_real, intent(inout), dimension(mvsiz) :: off
          my_real, intent(inout), dimension(mvsiz) :: tempel
          my_real, intent(inout), dimension(mvsiz) :: viscmx
          my_real, intent(inout), dimension(mvsiz) :: area
          my_real, intent(inout), dimension(mvsiz) :: exx
          my_real, intent(inout), dimension(mvsiz) :: eyy
          my_real, intent(inout), dimension(mvsiz) :: exy
          my_real, intent(inout), dimension(mvsiz) :: exz
          my_real, intent(inout), dimension(mvsiz) :: eyz
          my_real, intent(inout), dimension(mvsiz) :: thk0
          my_real, intent(inout), dimension(mvsiz) :: ssp
          my_real, intent(inout), dimension(mvsiz) :: rho
          my_real, intent(inout), dimension(mvsiz) :: alpe
          my_real, intent(inout), dimension(mvsiz) :: shf
          my_real, intent(inout), dimension(mvsiz) :: gs
          my_real, intent(inout), dimension(mvsiz) :: sigy
          my_real, intent(inout), dimension(mvsiz,2) :: zcfac
          my_real, intent(inout), dimension(mvsiz,8) :: f_def
          my_real, intent(inout), dimension(nxlaymax,mvsiz) :: dir1_crk
          my_real, intent(inout), dimension(nxlaymax,mvsiz) :: dir2_crk
          my_real, intent(inout), dimension(mvsiz) :: dt1c
          my_real, intent(inout), dimension(mvsiz) :: die
          my_real, intent(inout), dimension(mvsiz) :: etimp
          my_real, intent(inout), dimension(mvsiz) :: aldt
          my_real, intent(inout), dimension(mvsiz*nlay_max*laynpt_max) :: thkly
          my_real, intent(inout), dimension(nel,nlay_max*laynpt_max) :: thk_ly
          my_real, intent(inout), dimension(mvsiz,nlay_max*laynpt_max) :: posly
          !
          my_real, intent(inout), dimension(mvsiz,npt) :: ply_exx    ! batoz shell delamination
          my_real, intent(inout), dimension(mvsiz,npt) :: ply_eyy    ! batoz shell delamination
          my_real, intent(inout), dimension(mvsiz,npt) :: ply_exy    ! batoz shell delamination
          my_real, intent(inout), dimension(mvsiz,npt) :: ply_exz    ! batoz shell delamination
          my_real, intent(inout), dimension(mvsiz,npt) :: ply_eyz    ! batoz shell delamination
          my_real, intent(inout), dimension(mvsiz,5,npt) :: ply_f    ! batoz shell delamination
          !
          my_real, intent(inout), dimension(nel,varnl_npttot) :: varnl
          my_real, intent(inout), dimension(sdir_a) :: dir_a
          my_real, intent(inout), dimension(sdir_b) :: dir_b
          my_real, dimension(mvsiz), intent(inout) :: fheat
          !
          target :: aldt, ipm, varnl
          type(elbuf_struct_),intent(inout), target :: elbuf_str
          type (stack_ply) :: stack
          type (failwave_str_) ,target              :: failwave
          type (mat_elem_) ,intent(inout) ,target   :: mat_elem
          type (nlocal_str_)                        :: nloc_dmg
          type (sensors_) ,intent(in)               :: sensors
          type (dt_), intent(in) :: dt
          type (buf_damp_range_) ,intent(in)        :: damp_buf      ! buffer of damp frequency range
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numtabl
          integer :: ipt
          integer :: i,k,ii,jj,jmly,ipg,it,ifl,ilay,npg,mpt,mx,imat, &
                     irupt,nfail,iadbuf,igtyp,nuvar,nvarf,jdir,&
                     nfunc, jpos, nindx,nupar,niparam,nuparam,nuparam0,nfunc_fail,ntabl_fail,&
                     ifailwv,nsensor,iun,ibidon1,ibidon2,ibidon3,&
                     iptx,ilayer,irot,dmg_flag,lf_dammx,nipar,&
                     igmat,ipgmat,nptt,ipt_all,npttot,nuvarv,ilaw,&
                     ply_id,iseq,progressive_crack,&
                     orth_damage,l_dmg,iprony,israte,nvartmp,inloc,idrape,vp,nvar_damp,flag_incr
          integer :: ij1,ij2,ij3,ij4,ij5
          integer :: ij(5),iflag(1)
          integer :: l_sigb
          integer ,dimension(maxfunc) :: ifunc
          integer ,dimension(mvsiz)   :: ioff_duct,nfis1,nfis2,nfis3
!
          my_real sigdmg(mvsiz,5),sigksi(mvsiz,5),tens(mvsiz,5)
          my_real ,dimension(mvsiz) :: epchk,dt_inv,copy_pla,pla0,&
                                       degmb ,degfx ,sigoff,thklyl,thkn  ,etse,off_old,&
                                       depsxx,depsyy,depsxy,depsyz,depszx,epsxx ,epsyy ,epsxy,&
                                       epsyz ,epszx ,epspxx,epspyy,epspxy,epspyz,epspzx,sigoxx,&
                                       sigoyy,sigoxy,sigoyz,sigozx,signxx,signyy,signxy,signyz,&
                                       signzx,sigvxx,sigvyy,sigvxy,sigvyz,sigvzx,&
                                       wmc, epsd, yld,dpla,vol0, coef,hardm,g_imp,visc,wplar,&
                                       tstar,  vm, vm0, seq0, &
                                       areamin,dareamin,dmg_glob_scale,dmg_loc_scale,et_imp, epsthtot
          my_real, dimension(nel,5) :: dmg_orth_scale
!
          my_real :: zt,dtinv, vol2,asrate, &
                     r1,r2,s1,s2,r12a,r22a,s12b,s22b,rs1,rs2,rs3,&
                     t1,t2,t3,fact,r3r3,s3s3,&
                     bidon1,bidon2,bidon3,bidon4,bidon5,vv,aa,trelax,t0,tm
          my_real  scale1(nel)
          my_real ,dimension(nel), target :: le_max
          my_real tt_local
          my_real, dimension(:) ,pointer  :: el_temp,yldfac,crklen,crkdir,dadv,tfail,el_len,&
          &el_pla
          my_real, dimension(nel), target :: el_pla_dum
          target :: tempel,bufmat,scale1
!----
          type(ttable) table(*)
          type(buf_lay_) ,pointer :: bufly
          type(l_bufel_) ,pointer :: lbuf
          type(g_bufel_) ,pointer :: gbuf
          type(buf_fail_),pointer :: fbuf
!----
          integer, dimension(:) ,pointer  :: fld_idx,foff,offly,itable,ifunc_fail,&
          &itabl_fail,vartmp,iparam,iparamf
          my_real, dimension(:) ,pointer  :: uvar,uvarf,uelr,uelr1,dam,&
          &dfmax,tdel ,offl,uvarv,uparam,uparam0,uparamf,&
          &dirdmg,dir_orth,damini
          type(matparam_struct_) , pointer :: matparam
          type(fail_param_) , pointer :: fail_param
!----
          logical :: logical_userl_avail
          logical :: flag_eps,flag_zcfac,flag_etimp
          logical :: flag_law1,flag_law2,flag_law25,flag_law22
          logical, dimension(nel) :: print_fail
!
          character option*256
          integer size
          integer :: nrate,nprony
          my_real :: fisokin,kv,zshift
          my_real, dimension(nel) :: eps1,eps2
          my_real, dimension(nel), target :: vecnul
          my_real, dimension(:), pointer  :: sigbxx,sigbyy,sigbxy
          my_real, dimension(:), allocatable :: gv,beta
          my_real wm(11,11)
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          gbuf => elbuf_str%gbuf
          ipg  = (is-1)*elbuf_str%nptr + ir     ! current gauss point
          npg  = elbuf_str%nptr * elbuf_str%npts
          igtyp = igeo(11,pid(1))
          igmat = igeo(98,pid(1))
          inloc = iparg(78)
          !jlag = iparg(14) ! Not used for shell elements (always Lagrangian).
                            ! Initialized to 0, but may be interpreted as 1 if needed.
          nsensor = sensors%nsensor
!
          idrape = elbuf_str%idrape
          logical_userl_avail=.false.
          if (userl_avail > 0) logical_userl_avail=.true.
          do k=1,5
            ij(k) = nel*(k-1)
          enddo
          ij1 = ij(1) + 1
          ij2 = ij(2) + 1
          ij3 = ij(3) + 1
          ij4 = ij(4) + 1
          ij5 = ij(5) + 1
!
          ipt_all = 0
          npttot  = 0
!       need to check the value of ilaw for the array initializations because mtn/=ilaw
          flag_law1  = .false.
          flag_law2  = .false.
          flag_law22 = .false.
          flag_law25 = .false.
!
!  check visco elastic model
!
          iprony = 0
          if (idamp_freq_range > 0) iprony = 1
          do ilay=1,nlay
            npttot = npttot + elbuf_str%bufly(ilay)%nptt
            ilayer = ilay
            if (ixfem == 1 .and. ixlay > 0) ilayer = ixlay  ! xfem  multilayer
            ilaw = elbuf_str%bufly(ilayer)%ilaw
            imat = elbuf_str%bufly(ilayer)%imat
            if(ilaw==1) flag_law1 = .true.
            if(ilaw==2) flag_law2 = .true.
            if(ilaw==22) flag_law22 = .true.
            if(ilaw==25) flag_law25 = .true.
            if (mat_elem%mat_param(imat)%ivisc > 0) iprony=1
          enddo
!
!     zcfac computation is only useful for czforc3 and czforc3_crk routines
          flag_zcfac=.false.
          if( (jhbe>=21.and.jhbe<=29).or.(impl_s>0)) flag_zcfac=.true.
          flag_etimp=.false.
          if(impl_s>0 .and. mtn==78) flag_etimp=.true.
!
          vecnul(1:nel) = zero
          scale1(1:nel) = one
          iflag(1) = ipla
          bidon1 = zero
          bidon2 = zero
          bidon3 = zero
          bidon4 = zero
          bidon5 = zero
          ibidon1 = 0
          ibidon2 = 0
          ibidon3 = 0
          iun=1
!
          dmg_flag = 0
          trelax   = zero
!
          degmb(jft:jlt) = for(jft:jlt,1)*exx(jft:jlt)+for(jft:jlt,2)*eyy(jft:jlt)  &
          &              + for(jft:jlt,3)*exy(jft:jlt)+for(jft:jlt,4)*eyz(jft:jlt)  &
          &              + for(jft:jlt,5)*exz(jft:jlt)
          degfx(jft:jlt) = mom(jft:jlt,1)*kxx(jft:jlt)+mom(jft:jlt,2)*kyy(jft:jlt)  &
          &              + mom(jft:jlt,3)*kxy(jft:jlt)
!
          if (flag_law1 .or. flag_law25) then
            if (npt == 1) then
              degmb(jft:jlt) = for(jft:jlt,1)*exx(jft:jlt) + for(jft:jlt,2)*eyy(jft:jlt)  &
              &              + for(jft:jlt,3)*exy(jft:jlt) + for(jft:jlt,4)*eyz(jft:jlt)  &
              &              + for(jft:jlt,5)*exz(jft:jlt)
              degfx(jft:jlt) = zero
            endif
          endif
!
          if (flag_law25 .and. igtyp == 9 .and. npt == 1) then
            degmb(jft:jlt) = zero
            degfx(jft:jlt) = zero 
          elseif (npg>1) then
            degmb(jft:jlt) = degmb(jft:jlt)*off(jft:jlt)
            degfx(jft:jlt) = degfx(jft:jlt)*off(jft:jlt)
          endif
!
          vol0(jft:jlt)   = area(jft:jlt)*thk0(jft:jlt)
          thkn(jft:jlt)   = thk(jft:jlt)
          for(jft:jlt,1)  = zero
          for(jft:jlt,2)  = zero
          for(jft:jlt,3)  = zero
          for(jft:jlt,4)  = zero
          for(jft:jlt,5)  = zero
          mom(jft:jlt,1)  = zero
          mom(jft:jlt,2)  = zero
          mom(jft:jlt,3)  = zero
          yld(jft:jlt)    = zero
          seq0(jft:jlt)   = zero
          if (flag_law2 .or. flag_law25 .or. (.not.flag_zcfac)) sigy(jft:jlt)   = zero
          if(flag_zcfac .and. mtn /= 22) zcfac(jft:jlt,1)= zero
          zcfac(jft:jlt,2)= zero
          if(flag_zcfac) zcfac(jft:jlt,2)= one
          etse(jft:jlt)   = one
          if(flag_etimp) etimp(jft:jlt)= zero
          coef(jft:jlt)   = one
          if(flag_law25)then
            wplar(jft:jlt)=zero
            nfis1(jft:jlt)=0
            nfis2(jft:jlt)=0
            nfis3(jft:jlt)=0
          endif
          off_old(jft:jlt) = off(jft:jlt)
          ioff_duct(jft:jlt) = 0
          dmg_glob_scale(jft:jlt) = one
          sigoff(1:nel) = one
          epchk(1:mvsiz)  = zero
          viscmx(1:mvsiz) = zero
          zshift = geo(199, pid(1))
!
          if ( flag_law22 ) then
#include "vectorize.inc"
            do i=jft,jlt
              alpe(i) = em30
            enddo
          endif ! if ( flag_law22 )
!       compute the inverse of dt and save the result
      dtinv = dt1 / max(dt1**2,em20)  ! inverse of dt
!-----------------------------------------------------------
!     loop over thickness integration points (layers)
!-----------------------------------------------------------
          do ilay =1,nlay
            if (ixfem == 1 .and. ixlay > 0) then  !  multilayer xfem
              ilayer = ixlay
            else
              ilayer = ilay
            endif
            progressive_crack = 0
            orth_damage = 0
            ply_id = 0
            if (igtyp == 52) then
              ply_id = ply_info(1,stack%igeo(2+ilay,isubstack)-numstack)
            elseif(igtyp == 17 .or. igtyp == 51)then
              ply_id = igeo(1,stack%igeo(2+ilay,isubstack))
            endif
            bufly => elbuf_str%bufly(ilayer)
            !elbuf_str%bufly(ilayer)%fail(ir,is,it)%fbuf%floc(ifl)%dfmax

            nptt   = bufly%nptt
            jmly = 1 + (ilayer-1)*jlt
            jdir = 1 + (ilayer-1)*jlt*2
!
            nfail  = bufly%nfail
            imat   = bufly%imat
            ilaw   = bufly%ilaw
            nuvar  = bufly%nvar_mat
            nvartmp= bufly%nvartmp
            nuvarv = bufly%nvar_visc
            iseq   = bufly%l_seq
            l_sigb = bufly%l_sigb
            iadbuf = max(1,ipm(7,imat))
            nuparam0=  ipm(9,imat)                      ! old uparam stored in bufmat
            uparam0 => bufmat(iadbuf:iadbuf+nuparam0-1) ! old uparam stored in bufmat
            nfunc  = ipm(10,imat)
            numtabl= ipm(226,imat)
            itable => ipm(226+1:226+numtabl,imat)
            matparam => mat_elem%mat_param(imat)
            nuparam  =  mat_elem%mat_param(imat)%nuparam
            niparam  =  mat_elem%mat_param(imat)%niparam
            uparam   => mat_elem%mat_param(imat)%uparam
            iparam   => mat_elem%mat_param(imat)%iparam

            if (igtyp == 11 .or. igtyp == 16 .or. igtyp == 17 .or.    &
            &            igtyp == 51 .or. igtyp == 52) then
              if (ilaw == 58 .or. ilaw == 158 .or. ilaw == 88) nfunc = ipm(10,imat)+ipm(6,imat)
            endif
            do i=1,nfunc
              ifunc(i)=ipm(10+i,imat)
            enddo
!
!       check if eps arrays are used or not
!       eps is not used for law : 2/25/63/64/65/66/72/76/78/93 --> flag_eps=.false.
!       eps is used for the other laws
!       eps is also used for the rupture : 4/5/6/7/10/24
            flag_eps=.true.
            if(ilaw==1.or.ilaw== 2.or.ilaw==25.or.ilaw==32.or.ilaw==63.or.ilaw==64.or.ilaw==65.or.&
            &ilaw==66.or.ilaw==72.or.ilaw==76.or.ilaw==78.or.ilaw==85.or.ilaw==93) flag_eps=.false.

            if (ifailure == 1) then
              do it=1,nptt
                fbuf => bufly%fail(ir,is,it)
                do ifl = 1, nfail      ! loop over fail models in current layer
                  irupt  =  fbuf%floc(ifl)%ilawf
                  if(irupt== 4.or.irupt== 5.or.irupt== 6.or.irupt==7.or.   &
                  &                 irupt==10.or.irupt==24.or.irupt==34.or.irupt==39.or.  &
                  &                 irupt==43.or.irupt==47 ) flag_eps=.true.
                enddo
              enddo
            endif

!           depsyz/zx and epsyz/zx are it independent
!           epsxy is it independent for igtyp/=1
            depsyz(jft:jlt)=eyz(jft:jlt)
            depszx(jft:jlt)=exz(jft:jlt)
            if (flag_eps) then
              if (igtyp == 1) then
                epsyz(jft:jlt)= gstr(jft:jlt,4)
                epszx(jft:jlt)= gstr(jft:jlt,5)
              else
                epsxy(jft:jlt)  = zero
                epsyz(jft:jlt)  = zero
                epszx(jft:jlt)  = zero
              endif
            endif
!----------------------------------------------------------------------
            ! strain rate filtering coefficient for current layer material
            israte = ipm(3,imat)
            if (israte > 0) then
              asrate = min(one, pm(9,imat)*dt1)
            else
              asrate = one
            end if
!----------------------------------------------------------------------
!---
            l_dmg = bufly%l_dmg
!---
            do it=1,nptt
              ipt = ipt_all + it        ! count all nptt through all layers
              jpos = 1 + (ipt-1)*jlt
!
              lbuf  => bufly%lbuf(ir,is,it)
              uvar  => bufly%mat(ir,is,it)%var
              uvarv => bufly%visc(ir,is,it)%var
              vartmp=> bufly%mat(ir,is,it)%vartmp
              dirdmg => lbuf%dmg(1:l_dmg*nel)
              if(idrape > 0) jdir = 1 + (ipt - 1)*jlt*2
!
              ! -> make sure the non-local increment is positive
              if (inloc > 0) then
                do i = jft,jlt
                  varnl(i,it)    = max(varnl(i,it),zero)
                  lbuf%planl(i)  = lbuf%planl(i) + varnl(i,it)
                  lbuf%epsdnl(i) = varnl(i,it)/max(dt1,em20)
                enddo
              endif
!----------------------------------------------------------------------
              epsd(1:nel) = zero  !< local integration pt strain rate initialization
                                  !< calculated by material and used in failure models
!---------------------------------------------------
              if (jthe /= 0) then
                el_temp => tempel(1:nel)    ! calculated from nodal temp with /heat/mat
              else if (bufly%l_temp > 0) then
                el_temp => lbuf%temp(1:nel) ! local temp from mat in adiabatic conditions
              else
                el_temp => vecnul(1:nel)
              endif
!---------------------------------------------------
!         initial scale factor for yield stress defined per ipg,npi
              if (ilaw==36 .or. ilaw==87) then
                yldfac => scale1(1:nel)
                if (bufly%l_fac_yld > 0) then
                  yldfac => lbuf%fac_yld(1:nel)
                endif
              endif
!-----------------------------------------
!         coordonnees du point d'integration ipt
!-----------------------------------------
              !zz   => posly(1:nel,ipt)
              !thly => thkly(jpos:jpos+nel-1)
              thklyl(1:nel) = thkly(jpos:jpos+nel-1)*thk0(1:nel)
!
              if ((igtyp == 1 .or. igtyp == 9).and.zshift==zero) then
                ! initialize wm matrix
                call coqini_wm(wm)
                wmc(1:nel) = wm(ipt,npt)
              else
                wmc(1:nel) = posly(1:nel,ipt)*thkly(jpos:jpos+nel-1)
              endif
!-------------------------------
!         increment de deformations
!-------------------------------
              if (ilaw == 58 .or. ilaw == 158 .or. ilaw==127) then
                signxx(1:mvsiz) = zero
                signyy(1:mvsiz) = zero
                signxy(1:mvsiz) = zero
                signyz(1:mvsiz) = zero
                signzx(1:mvsiz) = zero
              endif
              if (ilaw == 58 .or. ilaw == 158 .or. ilaw==127 .or. idamp_freq_range > 0) then
                sigvxx(1:mvsiz) = zero
                sigvyy(1:mvsiz) = zero
                sigvxy(1:mvsiz) = zero
                sigvyz(1:mvsiz) = zero
                sigvzx(1:mvsiz) = zero
              endif
!       -------------------------------
!       igtyp = 1
!       -------------------------------
              if (igtyp == 1) then
                !       -----------
                !       ismstr=10 + foam laws
                !       -----------
                if (ismstr==10 .and. (mtn == 1 .or. mtn == 42 .or.&
                &                                  mtn == 69.or. mtn == 71 .or. mtn == 88)) then
                  do i=jft,jlt
                    zt=posly(i,ipt) *thk0(i)
                    depsxx(i)=exx(i)+zt*kxx(i)
                    depsyy(i)=eyy(i)+zt*kyy(i)
                    depsxy(i)=exy(i)+zt*kxy(i)
!       not it dependant
!              depsyz(i)=eyz(i)
!              depszx(i)=exz(i)
!
                    tens(i,1)= f_def(i,1)+zt*f_def(i,6)
                    tens(i,2)= f_def(i,2)+zt*f_def(i,7)
                    tens(i,3)= f_def(i,3)+zt*f_def(i,8)
                    tens(i,4)= f_def(i,4)+zt*f_def(i,5)
!       not it dependant
!              epsyz(i)= gstr(i,4)
!              epszx(i)= gstr(i,5)
                  enddo
!---------  [f]=[f_def]+[1]; [b]=[f][f]^t strain-----
                  do i=jft,jlt
                    epsxx(i)=tens(i,1)*(two+tens(i,1))+&
                    &tens(i,3)*tens(i,3)
                    epsyy(i)=tens(i,2)*(two+tens(i,2))+&
                    &tens(i,4)*tens(i,4)
                    epsxy(i)=two*(tens(i,3)+tens(i,4)+tens(i,1)*tens(i,4)+&
                    &tens(i,3)*tens(i,2))
                  enddo
                else if (ilaw == 27) then
                  do i=jft,jlt
                    zt       = posly(i,ipt) *thk0(i)
                    tens(i,1)=exx(i)+zt*kxx(i)
                    tens(i,2)=eyy(i)+zt*kyy(i)
                    tens(i,3)=half*(exy(i)+zt*kxy(i))
                    tens(i,4)=half*eyz(i)
                    tens(i,5)=half*exz(i)
                  enddo
!
                  call rotov(jft,jlt,tens,dirdmg,nel)
!
                  do i=jft,jlt
                    depsxx(i)=tens(i,1)
                    depsyy(i)=tens(i,2)
                    depsxy(i)=two*tens(i,3)
                    depsyz(i)=two*tens(i,4)
                    depszx(i)=two*tens(i,5)
                  enddo
                else
                  !       -----------
                  !       /= ismstr=10 + foam laws
                  !       -----------
                  do i=jft,jlt
                    zt=posly(i,ipt) *thk0(i)
                    depsxx(i)=exx(i)+zt*kxx(i)
                    depsyy(i)=eyy(i)+zt*kyy(i)
                    depsxy(i)=exy(i)+zt*kxy(i)
                  enddo
                  if (flag_eps) then
                    do i=jft,jlt
                      zt=posly(i,ipt) *thk0(i)
                      epsxx(i)= gstr(i,1)+zt*gstr(i,6)
                      epsyy(i)= gstr(i,2)+zt*gstr(i,7)
                      epsxy(i)= gstr(i,3)+zt*gstr(i,8)
                    enddo
                  endif
                end if!(ismstr==10)
!       -------------------------------
!       igtyp = 16
!       -------------------------------
              elseif (igtyp == 16) then
                !       ------------
                !       ismstr=11
                !       ------------
                if (ismstr == 11) then
!             total strain in fiber coord sys
                  do i=jft,jlt
                    ii = jdir + i-1
                    r1 = dir_a(ii)
                    s1 = dir_a(ii+nel)
                    r2 = dir_b(ii)
                    s2 = dir_b(ii+nel)
!               total strain in element coord sys
                    zt = posly(i,ipt) *thk0(i)
                    t1 = gstr(i,1) + zt*gstr(i,6)
                    t2 = gstr(i,2) + zt*gstr(i,7)
                    t3 = half*(gstr(i,3) + zt*gstr(i,8))
                    depsxy(i) = (r1*r2 + s1*s2) / (r1*s2 - r2*s1)    ! tan(alpha_totale)
                    depsxx(i) = r1*r1*t1 + s1*s1*t2 + two*r1*s1*t3  ! eps_x dir1
                    depsyy(i) = r2*r2*t1 + s2*s2*t2 + two*r2*s2*t3  ! eps_y dir2
!
                    epsxx(i) = t1
                    epsyy(i) = t2
                    epsxy(i) = t3 * two  ! gamma_xy
                  enddo
                else
                  !       ------------
                  !       ismstr/=11
                  !       ------------
!             strain rate in fiber coord sys
                  do i=jft,jlt
                    ii = jdir + i-1
                    r1 = dir_a(ii)
                    s1 = dir_a(ii+nel)
                    r2 = dir_b(ii)
                    s2 = dir_b(ii+nel)
!---
                    zt = posly(i,ipt) *thk0(i)
                    t1 = exx(i) + zt*kxx(i)
                    t2 = eyy(i) + zt*kyy(i)
                    t3 = half*(exy(i) + zt*kxy(i))
                    depsxy(i) = (r1*r2 + s1*s2) / (r1*s2 - r2*s1)   ! tan(alpha_totale)
                    depsxx(i) = r1*r1*t1 + s1*s1*t2 + two*r1*s1*t3 ! delta_eps_x dir1
                    depsyy(i) = r2*r2*t1 + s2*s2*t2 + two*r2*s2*t3 ! delta_eps_y dir2
                  enddo
                endif
                if (flag_eps) then
                  do i=jft,jlt
!             total true strain in global coord sys
                    zt = posly(i,ipt) *thk0(i)
                    epsxx(i) = gstr(i,1) + zt*gstr(i,6)
                    epsyy(i) = gstr(i,2) + zt*gstr(i,7)
                    epsxy(i) = gstr(i,3) + zt*gstr(i,8)
                  enddo
                endif
!------
!       -------------------------------
!       igtyp = 9 / 10 / 11 / 17 / 51 and 52
!       -------------------------------
              else
!--         igtyp 9/10/11/17/51/52
!               -------------------------------
!               igtyp = 51 or 52 + ilaw=58
!               -------------------------------
                if ((igtyp == 51 .or. igtyp == 52) .and. (ilaw == 58 .or. ilaw == 158)) then
                  !       ------------
                  !       ismstr=11
                  !       ------------
                  if (ismstr == 11) then
!             total strain in fiber coord sys
                    do i=jft,jlt
                      ii = jdir + i-1
                      r1 = dir_a(ii)
                      s1 = dir_a(ii+nel)
                      r2 = dir_b(ii)
                      s2 = dir_b(ii+nel)
!               total strain in element coord sys
                      zt = posly(i,ipt) *thk0(i)
                      t1 = gstr(i,1) + zt*gstr(i,6)
                      t2 = gstr(i,2) + zt*gstr(i,7)
                      t3 = half*(gstr(i,3) + zt*gstr(i,8))
                      depsxy(i) = (r1*r2 + s1*s2) / (r1*s2 - r2*s1)    ! tan(alpha_totale)
                      depsxx(i) = r1*r1*t1 + s1*s1*t2 + two*r1*s1*t3  ! eps_x dir1
                      depsyy(i) = r2*r2*t1 + s2*s2*t2 + two*r2*s2*t3  ! eps_y dir2
                      epsxy(i)  = t3 * two  ! gamma_xy
                    enddo
                  else
                    !       ------------
                    !       ismstr/=11
                    !       ------------
!             strain rate in fiber coord sys
                    do i=jft,jlt
                      ii = jdir + i-1
                      r1 = dir_a(ii)
                      s1 = dir_a(ii+nel)
                      r2 = dir_b(ii)
                      s2 = dir_b(ii+nel)
!---
                      zt = posly(i,ipt) *thk0(i)
                      t1 = exx(i) + zt*kxx(i)
                      t2 = eyy(i) + zt*kyy(i)
                      t3 = half*(exy(i) + zt*kxy(i))
                      depsxy(i) = (r1*r2 + s1*s2) / (r1*s2 - r2*s1)   ! tan(alpha_totale)
                      depsxx(i) = r1*r1*t1 + s1*s1*t2 + two*r1*s1*t3 ! delta_eps_x dir1
                      depsyy(i) = r2*r2*t1 + s2*s2*t2 + two*r2*s2*t3 ! delta_eps_y dir2
                    enddo
                  endif ! if (ismstr == 11) then
                  if (flag_eps) then
                    do i=jft,jlt
                      zt = posly(i,ipt) *thk0(i)
                      t1 = gstr(i,1) + zt*gstr(i,6)
                      t2 = gstr(i,2) + zt*gstr(i,7)
                      t3 = gstr(i,3) + zt*gstr(i,8)
                      epsxx(i) = t1
                      epsyy(i) = t2
                      epsxy(i) = t3
                    enddo
                  endif
                else !        igtyp 9/10/11/17/51   - no law58
!               -------------------------------
!               igtyp /= 51 and 52 or ilaw/=58
!               -------------------------------
                  if (ilaw /= 1 .and. ilaw /= 2 .and. ilaw /= 32 ) then
                    do i=jft,jlt
                      zt       = posly(i,ipt) *thk0(i)
                      tens(i,1)=exx(i)+zt*kxx(i)
                      tens(i,2)=eyy(i)+zt*kyy(i)
                      tens(i,3)=half*(exy(i)+zt*kxy(i))
                      tens(i,4)=half*eyz(i)
                      tens(i,5)=half*exz(i)
                    enddo
!
                    if (ilaw /= 27) then
                      call rotov(jft,jlt,tens,dir_a(jdir),nel)
                    else
                      call rotov(jft,jlt,tens,dirdmg,nel)
                    endif
!
                    do i=jft,jlt
                      depsxx(i)=tens(i,1)
                      depsyy(i)=tens(i,2)
                      depsxy(i)=two*tens(i,3)
                      depsyz(i)=two*tens(i,4)
                      depszx(i)=two*tens(i,5)
                    enddo
                  elseif (ilaw == 1 .or. ilaw == 2 .or. ilaw == 32) then
                    do i=jft,jlt
                      zt       = posly(i,ipt) *thk0(i)
                      depsxx(i)=exx(i)+zt*kxx(i)
                      depsyy(i)=eyy(i)+zt*kyy(i)
                      depsxy(i)=exy(i)+zt*kxy(i)
                    enddo
                  endif
!
                  if (ismstr==10 .and. (mtn == 1 .or. mtn == 42 .or.&
                  &mtn == 69.or. mtn == 71 .or. mtn == 88)) then
                    do i=jft,jlt
                      zt=posly(i,ipt) *gbuf%thk_i(i)
!
                      tens(i,1)= f_def(i,1)+zt*f_def(i,6)
                      tens(i,2)= f_def(i,2)+zt*f_def(i,7)
                      tens(i,3)= f_def(i,3)+zt*f_def(i,8)
                      tens(i,4)= f_def(i,4)+zt*f_def(i,5)
                    enddo
!--------   - [f]=[f_def]+[1]; [b]=[f][f]^t strain-----
                    epsxx(jft:jlt)=tens(jft:jlt,1)*(two+tens(jft:jlt,1))+tens(jft:jlt,3)*tens(jft:jlt,3)
                    epsyy(jft:jlt)=tens(jft:jlt,2)*(two+tens(jft:jlt,2))+tens(jft:jlt,4)*tens(jft:jlt,4)
                    epsxy(jft:jlt)=tens(jft:jlt,3)+tens(jft:jlt,4)+tens(jft:jlt,1)*tens(jft:jlt,4)+tens(jft:jlt,3)*tens(jft:jlt,2)
                    do i=jft,jlt
                      tens(i,1)= epsxx(i)
                      tens(i,2)= epsyy(i)
                      tens(i,3)= epsxy(i)
                      tens(i,4)= half*gstr(i,4)
                      tens(i,5)= half*gstr(i,5)
                    enddo
                  elseif (ilaw == 32) then
                    do i=jft,jlt
                      zt=posly(i,ipt) *thk0(i)
                      epsxx(i)= gstr(i,1)+zt*gstr(i,6)
                      epsyy(i)= gstr(i,2)+zt*gstr(i,7)
                      epsxy(i)= gstr(i,3)+zt*gstr(i,8)
                    enddo
                  else
                    do i=jft,jlt
                      zt=posly(i,ipt) *thk0(i)
                      tens(i,1)= gstr(i,1)+zt*gstr(i,6)
                      tens(i,2)= gstr(i,2)+zt*gstr(i,7)
                      tens(i,3)= half*(gstr(i,3)+zt*gstr(i,8))
                      tens(i,4)= half*gstr(i,4)
                      tens(i,5)= half*gstr(i,5)
                    enddo
                  end if!(ismstr==10)
!
                  if (ilaw /= 27 .and. ilaw /= 32) then
                    call rotov(jft,jlt,tens,dir_a(jdir),nel)
                  else if (ilaw == 27) then
                    call rotov(jft,jlt,tens,dirdmg,nel)
                  endif
!
                  if (flag_eps) then !
                    do i=jft,jlt
                      epsxx(i) = tens(i,1)
                      epsyy(i) = tens(i,2)
                      epsxy(i) = two*tens(i,3)
                      epsyz(i) = two*tens(i,4)
                      epszx(i) = two*tens(i,5)
                    enddo
                  endif
                endif  ! if (igtyp ==51 .and. ilaw == 58)
              endif  ! if (igtyp == 1)
!       -------------------------------
!       end of igtyp condition
!       -------------------------------
!---
              do i=jft,jlt
                epspxx(i)=depsxx(i)*dtinv
                epspyy(i)=depsyy(i)*dtinv
                epspxy(i)=depsxy(i)*dtinv
                epspyz(i)=depsyz(i)*dtinv
                epspzx(i)=depszx(i)*dtinv
              enddo
              dpla(1:mvsiz) = zero
              if (elbuf_str%bufly(ilayer)%l_pla > 0) then
                pla0(1:jlt) = lbuf%pla(1:jlt)
              else
                pla0(1:jlt) = zero
              endif   
              !< Old stress tensor
              sigoxx(1:nel) = lbuf%sig(ij1:ij1+nel-1)
              sigoyy(1:nel) = lbuf%sig(ij2:ij2+nel-1)
              sigoxy(1:nel) = lbuf%sig(ij3:ij3+nel-1)
              sigoyz(1:nel) = lbuf%sig(ij4:ij4+nel-1)
              sigozx(1:nel) = lbuf%sig(ij5:ij5+nel-1)
              !< Old equivalent stress
              if (bufly%l_seq > 0) seq0(1:nel) = lbuf%seq(1:nel)
!  
              if (jthe /= 0 .or. elbuf_str%bufly(ilayer)%l_temp > 0) then 
                ! case of temp calculated locally in material
                t0 = pm(79, imat)
                tm = pm(80, imat)
                tstar(1:nel) = max(zero, (el_temp(1:nel) - t0) / max(tm - t0, em20))
              else
                tstar(1:nel) = zero
              end if
!------------------------------------------
!         elastic stress +
!         plasticly admissible stress
!------------------------------------------
              if (ilaw == 1) then
                call sigeps01c(jft       ,jlt      ,nel      ,imat     ,gs       ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx    ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &depsxx    ,depsyy   ,depsxy   ,depsyz   ,depszx   ,&
                &thkn      ,thklyl   ,off      ,pm       ,ismstr   ,&
                &epsxx     ,epsyy    ,epsxy    )
              elseif (ilaw == 2) then
                vp =  ipm(255,imat)
                call sigeps02c(&
                &jft        ,jlt       ,pm       ,eint     ,thkn     ,&
                &off        ,sigy      ,dt1      ,ipla     ,nel      ,&
                &vol0       ,gs        ,israte   ,thklyl   ,etse     ,&
                &ngl        ,epsd_pg   ,g_imp    ,sigksi   ,ioff_duct,&
                &dpla       ,tstar     ,jthe     ,hardm    ,epchk    ,&
                &imat       ,ipt       ,npttot   ,lbuf%pla ,off_old  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx     ,signyy    ,signxy   ,signyz   ,signzx   ,&
                &depsxx     ,depsyy    ,depsxy   ,depsyz   ,depszx   ,&
                &epspxx     ,epspyy    ,epspxy   ,epspyz   ,epspzx ,&
                &lbuf%sigb(ij1),lbuf%sigb(ij2),lbuf%sigb(ij3),inloc  ,varnl(1,it),&
                &vp         ,asrate    ,lbuf%off ,lbuf%epsd  ,&
                &el_temp   ,fheat      )
!
              elseif (ilaw == 15) then
                call sigeps15c(&
                 jft      ,jlt     ,pm       ,lbuf%dam   ,&
                 imat     ,shf     ,ngl      ,dmg_flag   ,&
                 ilayer   ,nel     ,lbuf%pla ,sigdmg     ,&
                 israte   ,asrate  ,epsd_pg  ,lbuf%epsd  ,&
                 depsxx   ,depsyy  ,depsxy   ,depsyz     ,depszx   ,&
                 sigoxx   ,sigoyy  ,sigoxy   ,sigoyz     ,sigozx   ,&
                 signxx   ,signyy  ,signxy   ,signyz     ,signzx   ,&
                 lbuf%dsum,lbuf%tsaiwu)
              elseif (ilaw == 19) then

                call sigeps19c(&
                &nel       ,nuparam   ,niparam  ,flag_zcfac,zcfac     ,shf       ,&
                &uparam    ,iparam    ,npttot   ,ssp       ,nsensor   ,&
                &epsxx     ,epsyy     ,epsxy    ,epsyz     ,epszx     ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx    ,signyy    ,signxy   ,signyz    ,signzx    ,&
                &gbuf%sigi ,sensors%sensor_tab)

              elseif (ilaw == 22) then
                call sigeps22c(&
                &jft      ,jlt    ,pm       ,thkn   ,off     ,&
                &sigy     ,dt1c   ,ipla     ,nel    ,off_old ,&
                &gs       ,dpla   ,ioff_duct,nptt   ,ipt     ,&
                &epchk    ,alpe   ,thklyl   ,imat   ,lbuf%pla,&
                &depsxx   ,depsyy ,depsxy   ,depsyz ,depszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy ,signxy   ,signyz ,signzx  ,&
                &inloc    ,varnl(1,it),lbuf%off)
!
              elseif (ilaw == 27) then
                call sigeps27c(&
                 jft      ,jlt     ,pm       ,thkn    ,off   ,&
                 gstr     ,imat    ,dt1      ,ipla    ,shf   ,&
                 ngl      ,thk0    ,thklyl   ,lbuf%crak,lbuf%dam,&
                 sigy     ,zcfac   ,dpla     ,ilayer  ,ipt   ,&
                 israte   ,nel     ,posly(1,ipt),npttot  ,epsd_pg,&
                 depsxx   ,depsyy  ,depsxy   ,depsyz  ,depszx,&
                 sigoxx,sigoyy ,sigoxy,sigoyz,sigozx,&
                 signxx   ,signyy  ,signxy   ,signyz  ,signzx,&
                 dirdmg   ,lbuf%pla,inloc    ,varnl(1,it),lbuf%off, &
                 lbuf%epsd,asrate  )
!
              elseif (ilaw == 32) then
                call sigeps32c(&
                &jft        ,jlt    ,pm      ,thkn   ,off    ,&
                &dir_a(jdir),ipt    ,imat    ,nel    ,dt1c   ,&
                &gs         ,lbuf%epsd,thklyl,ipla   ,dpla   ,&
                &depsxx     ,depsyy ,depsxy  ,depsyz ,depszx ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx     ,signyy ,signxy  ,signyz ,signzx ,&
                &lbuf%pla   ,ngl    ,hardm   ,inloc  ,varnl(1,it),&
                &lbuf%seq   ,lbuf%off,etse   )

              elseif (ilaw == 25) then
                if (igtyp /= 1 .and. igtyp /= 9) then
                  if (ixfem == 1 .and. ixlay > 0) npttot = elbuf_str%bufly(ixlay)%nptt
!         integration by layers
                  call sigeps25c(matparam ,&
                   nel         ,pm(1,imat)  ,off      ,gstr     ,&   
                   dir_a(jdir) ,thkly(jpos),tt        ,dt1      ,shf       ,&
                   ngl         ,thk0        ,exx      ,off_old  ,& 
                   eyy         ,exy      ,exz         ,eyz      ,kxx       ,&
                   kyy         ,kxy      ,posly(1,ipt),epsd_pg  ,rho       ,&
                   ssp         ,bufmat   ,lbuf%off    ,lbuf%epsd,asrate    ,&
                   sigy        ,zcfac    ,nptt        ,ilayer   ,&
                   nfis1       ,nfis2    ,nfis3       ,wplar    ,&
                   npttot      ,igtyp    ,lbuf%visc   ,lbuf%sigply,&
                   sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                   signxx      ,signyy   ,signxy      ,signyz   ,signzx    ,&
                   sigvxx      ,sigvyy   ,sigvxy      ,sigvyz   ,sigvzx    ,&
                   israte      ,uvarv    ,ishplyxfem  ,ipt      ,lbuf%seq  ,&
                   ply_exx     ,ply_eyy  ,ply_exy     ,ply_exz  ,ply_eyz   ,&
                   ply_f       ,lbuf%pla ,lbuf%crak   ,gbuf%ierr,&
                   ioff_duct   ,ifailure ,ply_id      ,ipg      ,lbuf%tsaiwu,&
                   imconv      ,iout     ,lbuf%dmg    ,bufly%l_dmg)

                elseif (igtyp == 9) then
!           integration by points (through thickness)
                  call sigeps25cp(matparam ,&
                   jft     ,jlt    ,off      ,dir_a(jdir) ,&
                   shf     ,npt    ,ngl      ,ipt     ,off_old     ,&
                   thk0    ,lbuf%epsd,sigy     ,zcfac   ,nel    ,&
                   depsxx  ,depsyy ,depsxy   ,depsyz  ,depszx ,&
                   sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                   signxx  ,signyy ,signxy   ,signyz  ,signzx ,&
                   wplar  ,ioff_duct,lbuf%pla,israte  ,asrate ,&
                   epsd_pg,lbuf%tsaiwu)
                endif ! if (igtyp)
!
              elseif (ilaw == 34) then
                call sigeps34c(&
                &jlt    ,nuparam0,nuvar   ,nfunc    ,ifunc   ,&
                &npf    ,tf     ,tt     ,dt1c    ,uparam0    ,&
                &rho    ,thklyl  ,gs     ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &ssp    ,thkn    ,uvar     ,off    )
!
              elseif (ilaw == 35) then
                call sigeps35c(&
                &jlt    ,nuparam0,nuvar   ,nfunc    ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag    ,&
                &tf     ,tt     ,dt1c    ,uparam0   ,rho    ,&
                &area   ,eint   ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,uvar     ,off    ,&
                &ngl    ,shf    ,etse    ,israte   ,asrate ,&
                &lbuf%epsd)
!
              elseif (ilaw == 36) then
                nrate = nint(uparam0(1))
                fisokin = uparam0(2*nrate + 14)
                if(fisokin>0) then
                  sigbxx => lbuf%sigb(1      :  nel)
                  sigbyy => lbuf%sigb(nel+1  :2*nel)
                  sigbxy => lbuf%sigb(3*nel+1:4*nel)
                else
                  sigbxx => vecnul(1:nel)
                  sigbyy => vecnul(1:nel)
                  sigbxy => vecnul(1:nel)
                endif

                call sigeps36c(&
                     jlt    ,nuvar  ,nvartmp  ,nfunc   ,&
                     ifunc  ,npf    ,iflag   ,&
                     tf     ,dt1c   ,uparam0   ,rho     ,&
                     thklyl ,israte ,asrate  ,epsd_pg  ,lbuf%epsd  ,&
                     epspxx ,epspyy ,epspxy  ,&
                     depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                     epsxx  ,epsyy  ,epsxy   ,&
                     sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                     signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                     ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                     vartmp ,off    ,ipm     ,imat     ,&
                     etse   ,gs     ,sigy    ,&
                     dpla   ,g_imp  ,sigksi  ,shf      ,hardm      ,&
                     yldfac ,inloc ,varnl(1,it),lbuf%dmg,lbuf%planl,&
                     bufly%l_planl,sigbxx,sigbyy,sigbxy,lbuf%off)
!
              elseif (ilaw == 42) then
                call sigeps42c(&
                &nel    , nuparam, niparam , nuvar   , ismstr  ,&
                &tt     , dt1    , uparam  , iparam  , rho     ,&
                &depsxx , depsyy , depsxy  , depsyz  , depszx  ,&
                &epsxx  , epsyy  , epsxy   , thkn    , thklyl  ,&
                &signxx , signyy , signxy  , signyz  , signzx  ,&
                &sigoyz,sigozx,ssp     ,gs,  uvar,&
                &off    )

              elseif (ilaw == 43) then
                call sigeps43c(&
                &jlt    ,nuparam0,nuvar   ,npf      ,tf      ,&
                &nfunc  ,ifunc  ,israte  ,tt       ,dt1c    ,&
                &uparam0 ,rho    ,area    ,eint     ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx  ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx  ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &ssp    ,thkn   ,lbuf%pla ,uvar    ,off    ,&
                &ngl    ,etse   ,hardm    ,sigy    ,gs     ,&
                &dpla   ,lbuf%epsd,shf    ,inloc   ,varnl(1,it),&
                &lbuf%seq,lbuf%off,asrate ,epsd_pg )

              elseif (ilaw == 44) then
                call sigeps44c(&
                &jlt    ,nuparam0,nuvar   ,nfunc    ,ifunc   ,&
                &npf    ,tf     ,tt      ,dt1      ,uparam0  ,&
                &rho    ,thklyl ,off     ,etse     ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &gs     ,sigy   ,lbuf%epsd,dpla    ,asrate ,&
                &nvartmp,vartmp ,lbuf%sigb,inloc   ,varnl(1,it),&
                &lbuf%off)

              elseif (ilaw == 45) then
                call sigeps45c(&
                &jlt    ,nuparam0,nuvar   ,nfunc    ,ifunc  ,&
                &npf    ,npt    ,ipt     ,iflag    ,&
                &tf     ,tt     ,dt1c    ,uparam0   ,rho    ,&
                &area   ,eint   ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,shf     )
              elseif (ilaw == 48) then
                call sigeps48c(&
                &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt  ,iflag   ,&
                &tf     ,tt     ,dt1c    ,bufmat  ,rho,&
                &area   ,eint   ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,lbuf%epsd,dpla    ,israte ,&
                &asrate ,epsd_pg,inloc  ,varnl(1,it),lbuf%off)
!
              elseif (ilaw == 52) then
                call sigeps52c(&
                &jlt    ,nuparam0,nuvar  ,nfunc    ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag    ,asrate  ,&
                &tf     ,tt     ,dt1c    ,bufmat   ,rho,&
                &area   ,eint   ,thklyl  ,l_dmg    ,lbuf%dmg,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx  ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx  ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx   ,&
                &sigoxx ,sigoyy ,sigoxy  ,sigoyz   ,sigozx  ,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx  ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx  ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar    ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse  ,&
                &gs     ,sigy   ,epsd_pg ,table    ,lbuf%epsd)
!
              elseif (ilaw == 55) then
                call sigeps55c(&
                &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag    ,asrate  ,&
                &tf     ,tt     ,dt1c    ,bufmat   ,rho     ,&
                &area   ,eint   ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,epsd_pg ,lbuf%epsd,israte  )
!
              elseif (ilaw == 56) then
                call sigeps56c(&
                &jlt    ,nuparam0,nuvar  ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag   ,&
                &tf     ,tt     ,dt1c    ,bufmat  ,rho,&
                &area   ,eint   ,thklyl  ,israte  ,asrate  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,epsd_pg ,lbuf%epsd,dpla   )
!
              elseif (ilaw == 57) then
                call sigeps57c(&
                &jlt    ,matparam,rho    ,tt       ,dt1    ,&
                &ngl    ,thklyl ,thkn    ,ssp      ,        &
                &lbuf%pla,dpla  ,epsxx   ,epsyy    ,epsxy  ,&
                &epspxx ,epspyy ,epspxy  ,                  &
                &depsxx ,depsyy ,depsxy  ,depsyz  ,depszx  ,&
                &sigoxx ,sigoyy ,sigoxy  ,sigoyz  ,sigozx  ,&
                &signxx ,signyy ,signxy  ,signyz  ,signzx  ,&
                &off    ,etse   ,sigy    ,lbuf%seq,israte  ,&
                &asrate ,epsd_pg,lbuf%epsd,inloc  ,varnl(1,it),&
                &lbuf%off,nvartmp,vartmp ,shf     ,lbuf%sigb,&
                &bufly%l_dmg,lbuf%dmg,bufly%l_planl,lbuf%planl)
!
              elseif (ilaw == 58) then
                call sigeps58c(&
                     jlt     ,nuparam ,nuvar   ,nfunc    ,ifunc   ,    &
                     npf     ,npt     ,ipt     ,nsensor  ,             &
                     tf      ,tt      ,dt1c    ,uparam   ,rho     ,    &
                     area    ,eint    ,thklyl  ,niparam  ,iparam  ,    &
                     depsxx  ,depsyy  ,depsxy  ,depsyz   ,depszx  ,    &
                     epsxx   ,epsyy   ,epsxy   ,epsyz    ,epszx   ,    &
                     sigoxx  ,sigoyy  ,sigoxy  ,sigoyz   ,sigozx  ,    &
                     signxx  ,signyy  ,signxy  ,signyz   ,signzx  ,    &
                     sigvxx  ,sigvyy  ,sigvxy  ,sigvyz   ,sigvzx  ,    &
                     ssp     ,viscmx  ,thkn    ,lbuf%pla ,uvar    ,    &
                     off     ,ngl     ,pm      ,matly(jmly),etse  ,    &
                     shf     ,sigy    ,lbuf%ang,aldt    ,              &
                     sensors%sensor_tab,ismstr,table    ,gbuf%off)
!
              elseif (ilaw == 60) then
                if (igtyp /= 10.and.igtyp /= 11 .and.igtyp /= 17) then
                  call sigeps60c(&
                  &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                  &npf    ,npt    ,ipt     ,iflag   ,&
                  &tf     ,tt     ,dt1c    ,bufmat  ,rho,&
                  &area   ,eint   ,thklyl  ,israte   ,asrate ,&
                  &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                  &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                  &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                  &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                  &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                  &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                  &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                  &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                  &gs     ,sigy   ,epsd_pg ,lbuf%epsd,dpla,shf,&
                  &inloc  ,varnl(1,it),lbuf%off)
                else
                  call sigeps60c(&
                  &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                  &npf    ,npt    ,ipt  ,iflag   ,&
                  &tf     ,tt     ,dt1c    ,bufmat  ,rho,&
                  &area   ,eint   ,thklyl  ,israte   ,asrate ,&
                  &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                  &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                  &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                  &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                  &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                  &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                  &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                  &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                  &gs     ,sigy   ,epsd_pg ,lbuf%epsd,dpla,shf,&
                  &inloc  ,varnl(1,it),lbuf%off)
                end if
              elseif (ilaw == 62) then
                call sigeps62c(&
                &jlt    , nuparam0, nuvar   , nfunc , ifunc , npf   ,&
                &npt    , ipt    , iflag   ,&
                &tf     , tt     , dt1c    , uparam0, rho  ,&
                &area   , eint   , thklyl  ,&
                &epspxx , epspyy , epspxy  , epspyz, epspzx,&
                &depsxx , depsyy , depsxy  , depsyz, depszx,&
                &epsxx  , epsyy  , epsxy   , epsyz , epszx ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx , signyy , signxy  , signyz, signzx,&
                &sigvxx , sigvyy , sigvxy  , sigvyz, sigvzx,&
                &ssp    , viscmx , thkn    ,uvar   , off   ,&
                &ngl    , ismstr , ipm     , gs      )
              elseif (ilaw == 63) then
                call sigeps63c(&
                &jlt,          nuparam0,      nuvar,        nfunc,&
                &ifunc,        npf,          npt,          ipt,&
                &iflag,        tf,           tt,           dt1c,&
                &uparam0,       rho,          area,         eint,&
                &thklyl,       epspxx,       epspyy,       epspxy,&
                &epspyz,       epspzx,       depsxx,       depsyy,&
                &depsxy,       depsyz,       depszx,       epsxx,&
                &epsyy,        epsxy,        epsyz,        epszx,&
                &sigoxx,sigoyy,sigoxy,sigoyz,&
                &sigozx,signxx,       signyy,       signxy,&
                &signyz,       signzx,       sigvxx,       sigvyy,&
                &sigvxy,       sigvyz,       sigvzx,       ssp,&
                &viscmx,       thkn,         lbuf%pla,     uvar,&
                &off,          ngl,          etse,         gs,&
                &vol0,         sigy,         el_temp,       die,&
                &coef,         inloc,        varnl(1,it),  jthe,&
                &lbuf%off)
              elseif (ilaw == 64) then
                call sigeps64c(&
                &jlt,          nuparam0,      nuvar,        nfunc,&
                &ifunc,        npf,          npt,          ipt,&
                &iflag,        tf,           tt,           dt1c,&
                &uparam0,       rho,          area,         eint,&
                &thklyl,       epspxx,       epspyy,       epspxy,&
                &epspyz,       epspzx,       depsxx,       depsyy,&
                &depsxy,       depsyz,       depszx,       epsxx,&
                &epsyy,        epsxy,        epsyz,        epszx,&
                &sigoxx,sigoyy,sigoxy,sigoyz,&
                &sigozx,signxx,       signyy,       signxy,&
                &signyz,       signzx,       sigvxx,       sigvyy,&
                &sigvxy,       sigvyz,       sigvzx,       ssp,&
                &viscmx,       thkn,         lbuf%pla,     uvar,&
                &off,          ngl,          ipm,          matly(jmly),&
                &etse,         gs,           vol0,         sigy,&
                &el_temp,       die,          coef,         inloc,&
                &varnl(1,it),  jthe,         lbuf%off)
!
              elseif (ilaw == 65) then
                call sigeps65c(&
                &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt,iflag   ,&
                &tf     ,tt     ,dt1c    ,bufmat  ,rho,&
                &area   ,eint   ,thklyl   ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,lbuf%epsd,dpla   )
!
              elseif (ilaw == 66) then
                call sigeps66c(&
                &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag   ,&
                &tf     ,tt     ,dt1c    ,bufmat  ,rho     ,&
                &area   ,eint   ,thklyl  ,israte   ,asrate ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,epsd_pg ,lbuf%epsd,inloc  ,&
                &varnl(1,it),matparam    ,nuvarv  ,uvarv   ,&
                &lbuf%off)
!
              elseif (ilaw == 69) then
                call sigeps69c(&
                &jlt    , nuparam0, nuvar  ,npt    , ipt   ,&
                &tt     , dt1     , uparam0, rho   ,&
                &area   , eint   , thklyl  ,&
                &epspxx , epspyy , epspxy  , epspyz, epspzx,&
                &depsxx , depsyy , depsxy  , depsyz, depszx,&
                &epsxx  , epsyy  , epsxy   , epsyz , epszx ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx , signyy , signxy  , signyz, signzx,&
                &sigvxx , sigvyy , sigvxy  , sigvyz, sigvzx,&
                &ssp    , viscmx , thkn    , uvar  , ngl    ,&
                &off    , ismstr  , gs    )

              elseif (ilaw == 71) then !ismstr = 10 always (set in cgrtails)
                call sigeps71c(&
                &jlt,          nuparam0,      nuvar,        nfunc,&
                &ifunc,        npf,          npt,          ipt,&
                &iflag,        tf,           tt,           dt1c,&
                &uparam0,       rho,          area,         eint,&
                &thklyl,       epspxx,       epspyy,       epspxy,&
                &epspyz,       epspzx,       depsxx,       depsyy,&
                &depsxy,       depsyz,       depszx,       epsxx,&
                &epsyy,        epsxy,        epsyz,        epszx,&
                &sigoxx,sigoyy,sigoxy,sigoyz,&
                &sigozx,signxx,       signyy,       signxy,&
                &signyz,       signzx,       sigvxx,       sigvyy,&
                &sigvxy,       sigvyz,       sigvzx,       ssp,&
                &viscmx,       thkn,         lbuf%pla,     uvar,&
                &off,          ngl,          ipm,          matly(jmly),&
                &etse,         gs,           sigy,         vol0,&
                &el_temp,       ismstr,       jthe)
              elseif (ilaw == 72) then
                call sigeps72c(&
                &jlt      ,nuparam0  ,nuvar    ,&
                &tt       ,dt1c     ,uparam0   ,rho      ,thklyl   ,&
                &depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &ssp      ,thkn     ,lbuf%pla ,uvar     ,off      ,&
                &etse     ,gs       ,sigy     ,hardm    ,lbuf%seq ,&
                &dpla     ,lbuf%dmg ,inloc    ,varnl(1,it),lbuf%off)
              elseif (ilaw == 73) then
                call sigeps73c(&
                &jlt,          nuparam0,      nuvar,        tt,&
                &dt1c,         uparam0,       rho,          area,&
                &eint,         thklyl,       epspxx,       epspyy,&
                &epspxy,       epspyz,       epspzx,       depsxx,&
                &depsyy,       depsxy,       depsyz,       depszx,&
                &epsxx,        epsyy,        epsxy,        epsyz,&
                &epszx,        sigoxx,sigoyy,sigoxy,&
                &sigoyz,sigozx,signxx,       signyy,&
                &signxy,       signyz,       signzx,       sigvxx,&
                &sigvyy,       sigvxy,       sigvyz,       sigvzx,&
                &ssp,          viscmx,       thkn,         lbuf%pla,&
                &uvar,         off,          ngl,          itable,&
                &etse,         gs,           sigy,         dpla,&
                &lbuf%epsd,    table,        vol0,         el_temp,&
                &die,          coef,         npf,          nfunc,&
                &ifunc,        tf,           shf,          hardm,&
                &lbuf%seq,     inloc,        varnl(1,it),  jthe,&
                &lbuf%off)
!
              elseif (ilaw == 76) then

                call sigeps76c(&
                &jlt      ,nuparam0  ,nuvar    ,nfunc    ,ifunc    ,&
                &npf      ,tf        ,matparam ,tt       ,dt1      ,&
                &uparam0  ,uvar      ,rho      ,off      ,ngl      ,&
                &depsxx   ,depsyy    ,depsxy   ,depsyz   ,depszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx    ,&
                &ssp      ,thkn     ,thklyl   ,lbuf%pla ,lbuf%epsd ,&
                &etse     ,gs       ,sigy     ,inloc    ,bufly%l_planl,&
                &lbuf%planl,varnl(1,it),lbuf%dmg,&
                &nvartmp  ,vartmp   ,lbuf%off)
!
              elseif (ilaw == 78) then
                call sigeps78c(&
                &jlt      ,nuparam0  ,nuvar    ,nvartmp  ,tt       ,&
                &nfunc    ,ifunc    ,npf      ,tf       ,uparam0   ,&
                &thklyl   ,thkn     ,gs       ,etse     ,sigy     ,&
                &depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &ssp      ,uvar     ,lbuf%siga,lbuf%sigb,lbuf%sigc,&
                &rho      ,off      ,lbuf%pla ,dpla     ,vartmp   ,&
                &inloc    ,varnl(1,it),et_imp ,lbuf%seq ,lbuf%off)
!
              elseif (ilaw == 80) then
                call sigeps80c(&
                &jlt,          nuparam0,      nuvar,        nfunc,&
                &ifunc,        npf,          npt,          ipt,&
                &iflag,        tf,           tt,           dt1c,&
                &uparam0,       rho,          area,         eint,&
                &thklyl,       epspxx,       epspyy,       epspxy,&
                &epspyz,       epspzx,       depsxx,       depsyy,&
                &depsxy,       depsyz,       depszx,       epsxx,&
                &epsyy,        epsxy,        epsyz,        epszx,&
                &sigoxx,sigoyy,sigoxy,sigoyz,&
                &sigozx,signxx,       signyy,       signxy,&
                &signyz,       signzx,       sigvxx,       sigvyy,&
                &sigvxy,       sigvyz,       sigvzx,       ssp,&
                &viscmx,       thkn,         lbuf%pla,     uvar,&
                &off,          ngl,          pm,           ipm,&
                &matly(jmly),  etse,         gs,           vol0,&
                &sigy,         el_temp,      die,          coef,&
                &shf,          epsd_pg,      table,        ithk,&
                &nvartmp,      vartmp,       epsthtot,     jthe,&
                &idt_therm,    theaccfact)
!
              elseif (ilaw == 82) then
                call sigeps82c(&
                &jlt    ,nuparam0,nuvar   ,nfunc  ,ifunc  ,&
                &npf    ,npt    ,ipt     ,iflag  ,&
                &tf     ,tt     ,dt1c    ,&
                &uparam0 ,rho    ,area    ,eint   ,thklyl,&
                &epspxx ,epspyy ,epspxy  ,epspyz ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz  ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,uvar   ,&
                &ngl    ,off    ,ismstr  ,ipm    , gs    )
              elseif(ilaw == 85)then
                call sigeps85c_void(&
                &jlt    ,nuparam0,nuvar   ,nfunc    ,ifunc  ,&
                &npf    ,npt    ,ipt     ,iflag    ,&
                &tf     ,tt     ,dt1c    ,uparam0   ,rho    ,&
                &area   ,eint   ,thklyl  ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,shf     )
              elseif (ilaw == 86) then
                call sigeps86c(&
                &jlt    ,nuparam0,nuvar   ,nfunc   ,ifunc   ,&
                &npf    ,npt    ,ipt     ,iflag   ,&
                &tf     ,tt     ,dt1c    ,uparam0  ,rho,&
                &area   ,eint   ,thklyl  ,israte   ,asrate ,&
                &epspxx ,epspyy ,epspxy  ,epspyz   ,epspzx ,&
                &depsxx ,depsyy ,depsxy  ,depsyz   ,depszx ,&
                &epsxx  ,epsyy  ,epsxy   ,epsyz    ,epszx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx ,signyy ,signxy  ,signyz   ,signzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz   ,sigvzx ,&
                &ssp    ,viscmx ,thkn    ,lbuf%pla ,uvar   ,&
                &off    ,ngl    ,ipm     ,matly(jmly),etse ,&
                &gs     ,sigy   ,epsd_pg ,lbuf%epsd  ,dpla)
              elseif (ilaw == 87) then
                call sigeps87c(&
                &nel      ,matparam ,nuvar    ,uvar     ,             &
                &tt       ,dt1      ,rho      ,thklyl   ,thkn     ,   &
                &epspxx   ,epspyy   ,epspxy   ,                       &
                &depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,   &
                &sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,   &
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,   &
                &ssp      ,lbuf%pla ,dpla     ,epsd_pg  ,sigy     ,   &
                &etse     ,gs       ,israte   ,asrate   ,lbuf%epsd,   &
                &el_temp  ,bufly%l_sigb,lbuf%sigb,inloc ,varnl(1,it), &
                &lbuf%seq ,jthe     ,off      ,lbuf%off ,nvartmp  ,   &
                &vartmp   )
              elseif (ilaw == 88) then
                call sigeps88c(&
                &jlt    , nuparam0, nuvar   , nfunc , ifunc , npf   ,&
                &npt    , ipt     ,ngl     , off    , ismstr   , gs ,&
                &tf     , tt     , dt1c    , bufmat(iadbuf), rho   ,&
                &area   , eint   , thklyl  ,&
                &depsxx , depsyy , depsxy  , depsyz, depszx,&
                &epsxx  , epsyy  , epsxy   , epsyz , epszx ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx , signyy , signxy  , signyz, signzx,&
                &ssp    , viscmx , thkn    , uvar )
              elseif (ilaw == 93) then
                call sigeps93c(&
                &jlt      ,nuparam0  ,nuvar    ,nfunc    ,ifunc    ,&
                &npf      ,tf       ,tt       ,dt1c     ,uparam0   ,&
                &epspxx   ,epspyy   ,epspxy   ,epspyz   ,epspzx   ,&
                &depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &ssp      ,thkn     ,lbuf%pla ,uvar     ,rho      ,&
                &off      ,etse     ,thklyl   ,shf      ,sigy     ,&
                &hardm    ,lbuf%seq ,lbuf%epsd,asrate   ,nvartmp  ,&
                &vartmp   ,dpla     ,inloc    ,varnl(1,it),lbuf%off)
!
              elseif (ilaw == 104 )then
!
                call sigeps104c(&
                &jlt     ,ngl     ,ipg     ,ilayer  ,it      ,nuparam0 ,nuvar    ,&
                &dt1c    ,tt      ,uparam0 ,uvar    ,jthe    ,rho      ,el_temp   ,&
                &lbuf%pla,dpla    ,ssp     ,lbuf%off,lbuf%epsd,gs      ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx  ,thklyl   ,off      ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx  ,signyy  ,signxy  ,signyz  ,signzx  ,thkn     ,sigy     ,&
                &etse    ,varnl(1,it),lbuf%dmg,bufly%l_dmg,lbuf%temp,lbuf%seq,inloc,&
                &elbuf_str%nptr,elbuf_str%npts,elbuf_str%nptt,bufly    ,lbuf%planl,&
                &bufly%l_planl ,lbuf%epsdnl,bufly%l_epsdnl,ioff_duct   )
!
              elseif (ilaw == 107 )then
                call sigeps107c(&
                &jlt     ,ngl     ,nuparam0 ,nuvar   ,nfunc   ,ifunc   ,npf     ,&
                &tf      ,dt1c    ,tt      ,uparam0  ,uvar    ,jthe    ,rho     ,&
                &lbuf%pla,dpla    ,ssp     ,lbuf%epsd,off     ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx  ,shf     ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &sigy    ,signxx  ,signyy  ,signxy  ,signyz  ,signzx  ,&
                &etse    ,numtabl  ,itable  ,table   ,nvartmp ,vartmp   )
!
              elseif (ilaw == 109 )then
                call sigeps109c(&
                &jlt     ,ngl     ,nuparam0,nuvar   ,nvartmp  ,numtabl  ,&
                &uparam0 ,uvar    ,vartmp  ,itable  ,table    ,jthe     ,&
                &tt      ,dt1c    ,off     ,rho     ,lbuf%pla ,dpla     ,&
                &ssp     ,sigy    ,etse    ,el_temp ,lbuf%epsd,gs       ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx   ,&
                &sigoxx  ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx   ,&
                &signxx  ,signyy  ,signxy  ,signyz  ,signzx   ,&
                &thkn    ,thklyl  ,inloc   ,varnl(1,it),lbuf%off)
!
              elseif (ilaw == 110 )then
                call sigeps110c(&
                &jlt     ,ngl     ,nuparam0 ,nuvar   ,npf      ,&
                &tt      ,dt1c    ,uparam0  ,uvar    ,jthe     ,off     ,&
                &gs      ,rho     ,lbuf%pla,dpla    ,lbuf%epsd ,ssp     ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx   ,asrate  ,&
                &epspxx  ,epspyy  ,epspxy  ,epspyz  ,epspzx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx  ,signyy  ,signxy  ,signyz  ,signzx   ,thklyl  ,&
                &thkn    ,sigy    ,etse    ,el_temp  ,lbuf%temp,lbuf%seq,&
                &tf      ,numtabl ,itable  ,table   ,nvartmp  ,vartmp  ,&
                &lbuf%siga,inloc  ,varnl(1,it),lbuf%off ,ioff_duct)
!
              elseif (ilaw == 112 )then
                call sigeps112c(&
                &jlt     ,ngl     ,nuparam0 ,nuvar   ,nfunc   ,ifunc   ,npf     ,&
                &tf      ,dt1c    ,tt      ,uparam0  ,uvar    ,jthe    ,rho     ,&
                &lbuf%pla,dpla    ,ssp     ,lbuf%epsd,off     ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx  ,shf     ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &sigy    ,signxx  ,signyy  ,signxy  ,signyz  ,signzx  ,&
                &etse    ,numtabl  ,itable  ,table   ,nvartmp ,vartmp   )
!
              elseif (ilaw == 119 )then

                call sigeps119c(&
                &nel      ,ipt      ,npt      ,nuparam0  ,nuvar    ,&
                &numtabl  ,itable   ,table    ,uvar     ,uparam0   ,&
                &thkn     ,thklyl   ,shf      ,ssp      ,off      ,&
                &flag_zcfac,zcfac   ,depsxx   ,depsyy   ,depsxy   ,&
                &epsxx    ,epsyy    ,epsxy    ,epsyz    ,epszx    ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &wmc      ,thkly(jpos),thk0)
!
              elseif (ilaw == 121 )then
                call sigeps121c(&
                &jlt     ,ngl     ,nuparam0 ,nuvar   ,nfunc   ,ifunc   ,npf     ,&
                &tf      ,dt1c    ,tt      ,uparam0  ,uvar    ,rho     ,lbuf%pla,&
                &dpla    ,ssp     ,lbuf%epsd,gs     ,thkn    ,thklyl  ,off     ,&
                &depsxx  ,depsyy  ,depsxy  ,depsyz  ,depszx  ,&
                &epspxx  ,epspyy  ,epspxy  ,epspyz  ,epspzx  ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx  ,signyy  ,signxy  ,signyz  ,signzx  ,&
                &sigy    ,etse    ,varnl(1,it),inloc,gbuf%dt ,&
                &ipg     ,it      ,elbuf_str%nptr,elbuf_str%npts,elbuf_str%nptt,&
                &bufly   ,lbuf%off,ioff_duct)
!
              elseif (ilaw == 122) then
                call sigeps122c(&
                &jlt      ,nuparam0 ,nuvar    ,uparam0  ,uvar     ,&
                &epsxx    ,epsyy    ,rho      ,lbuf%pla ,dpla     ,&
                &depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                &thkn     ,thklyl   ,off      ,sigy     ,etse     ,&
                &lbuf%dmg ,lbuf%seq ,shf      ,ssp      ,&
                &epsd_pg  ,nfunc    ,ifunc    ,npf      ,tf       ,&
                &nvartmp  ,vartmp   ,ioff_duct)
!
              elseif (ilaw == 125) then
               call sigeps125c(&
               &jlt      ,matparam   ,nuvar    ,uvar      ,&
               &rho      ,thkn       ,thklyl   , shf      ,&
               &nfunc    ,ifunc      ,npf      ,tf        ,snpc    ,&
               &stf      ,epsd_pg                                  ,&
               &depsxx   ,depsyy     ,depsxy                       ,&
               &epsxx    ,epsyy      ,epsxy    ,epsyz    ,epszx    ,&
               &sigoxx   ,sigoyy     ,sigoxy                       ,&
               &signxx   ,signyy     ,signxy   ,signzx   ,signyz   ,&
               &off      ,sigy       ,etse     ,ssp      ,lbuf%dmg ,&
               &gbuf%dmg ,lbuf%off  )
!
              elseif (ilaw == 127) then
                ! ---
                do i=jft,jlt
                  ! ij(k) = nel*(k-1)
                  sigoxx(i) =  lbuf%sig(nel*(1-1)+i)
                  sigoyy(i) =  lbuf%sig(nel*(2-1)+i)
                  sigoxy(i) =  lbuf%sig(nel*(3-1)+i)
                  sigoyz(i) =  lbuf%sig(nel*(4-1)+i)
                  sigozx(i) =  lbuf%sig(nel*(5-1)+i)
                enddo
                !---
                call sigeps127c(&
                &jlt      ,matparam   ,nuvar    ,uvar      ,         &
                &rho      ,thkn       ,thklyl   ,shf       ,ncycle  ,&
                &nfunc    ,ifunc      ,npf      ,tf        ,snpc    ,&
                &stf      ,epsd_pg    ,npttot                       ,&
                &depsxx   ,depsyy     ,depsxy   ,depsyz   ,depszx   ,&
                &epsxx    ,epsyy      ,epsxy    ,epsyz    ,epszx    ,&
                &sigoxx   ,sigoyy     ,sigoxy   ,sigozx   ,sigoyz   ,&
                &signxx   ,signyy     ,signxy   ,signzx   ,signyz   ,&
                &off      ,sigy       ,etse     ,ssp      ,lbuf%dmg ,&
                &gbuf%dmg  ,lbuf%off)
!
              elseif (ilaw == 128) then
                sigoxx(1:nel) = lbuf%sig(1:nel)
                sigoyy(1:nel) = lbuf%sig(nel+1:nel*2)
                sigoxy(1:nel) = lbuf%sig(nel*2+1:nel*3)
                sigoyz(1:nel) = lbuf%sig(nel*3+1:nel*4)
                sigozx(1:nel) = lbuf%sig(nel*4+1:nel*5)
!
                call sigeps128c(mat_elem%mat_param(imat),                  &
                  nel      ,nvartmp  ,vartmp   ,dt1      ,                 &
                  depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,       &
                  sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,       &
                  signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,       &
                  ssp      ,thkn     ,lbuf%pla ,dpla     ,lbuf%epsd,       &
                  off      ,etse     ,thklyl   ,shf      ,sigy     ,       &
                  hardm    ,lbuf%seq ,l_sigb   ,lbuf%sigb)
!
              elseif (ilaw == 158) then
                call sigeps158c(&
                &jlt       ,nuparam   ,nuvar     ,nfunc     ,ifunc     ,&
                &npf       ,tf        ,tt        ,dt1       ,uparam    ,&
                &area      ,thklyl    ,ssp       ,viscmx    ,uvar      ,&
                &depsxx    ,depsyy    ,depsxy    ,depsyz    ,depszx    ,&
                &epsxx     ,epsyy     ,epsxy     ,epsyz     ,epszx     ,&
                &sigoxx,sigoyy,sigoxy,sigoyz,sigozx,&
                &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                &sigvxx    ,sigvyy    ,sigvxy    ,lbuf%ang  ,gbuf%off  ,&
                &rho       ,etse      ,shf       ,aldt      ,nsensor   ,&
                &sensors%sensor_tab   ,niparam   ,iparam    )
!
              endif !ilaw
!
!  visco elastic model (prony) + damp_freq_range
!
              nvar_damp = 0
              if (idamp_freq_range > 0) nvar_damp = 21
!
              if (matparam%ivisc == 1 .and. ilaw /= 25) then
                nprony = matparam%visc%iparam(1)
                kv     = matparam%visc%uparam(1)
!
                allocate(gv(nprony),beta(nprony))
                do i=1,nprony
                  gv(i)   = matparam%visc%uparam(1 + i)
                  beta(i) = matparam%visc%uparam(1 + nprony + i)
                enddo
!
                call prony_modelc(&
                &nel    ,nuvarv  ,dt1 ,&
                &nprony , kv   , gv   ,beta    ,rho   ,&
                &epspxx ,epspyy ,epspxy  ,epspyz  ,epspzx ,&
                &sigvxx ,sigvyy ,sigvxy  ,sigvyz  ,sigvzx ,&
                &ssp,uvarv   ,off,nvar_damp)
                deallocate(gv,beta)
              endif
!
              if ((idamp_freq_range > 0) .and. ilaw /= 25) then
                flag_incr = 0
                call damping_range_shell(damp_buf,nel     ,nuvarv  ,nvar_damp,dt1      ,           &
                                         rho     ,ssp     ,matparam%young,matparam%shear,          &
                                         epspxx  ,epspyy  ,epspxy  ,epspyz  ,epspzx    ,           &
                                         sigvxx  ,sigvyy  ,sigvxy  ,sigvyz  ,sigvzx    ,           &
                                         uvarv   ,off     ,etse     ,flag_incr)
              endif
!-------------------------------------------
              do i=jft,jlt
                viscmx(i) = max(dm,viscmx(i))
              enddo
!-------------------------------------------
!         end of material laws
!-----------------------------------------------
!------------------------------------------------------------
!     Calculation of the Plastic Work
!------------------------------------------------------------ 
              if ((gbuf%g_wpla > 0).and.(bufly%l_pla > 0)) then  
                !< Case where equivalent stress is computed in the material law
                if (bufly%l_seq > 0) then
                  do i = jft,jlt
                    dpla(i) = lbuf%pla(i) - pla0(i)
                    gbuf%wpla(i) = gbuf%wpla(i) +                               &
                        half*(seq0(i) + lbuf%seq(i))*dpla(i)*thklyl(i)*area(i)
                  enddo
                !< Default case using Von Mises stress
                else
                  do i = jft,jlt
                    dpla(i) = lbuf%pla(i) - pla0(i)
                    vm0(i) = sqrt(sigoxx(i)*sigoxx(i) + sigoyy(i)*sigoyy(i) -   &
                           sigoxx(i)*sigoyy(i) + three*sigoxy(i)*sigoxy(i))
                    vm(i)  = sqrt(signxx(i)*signxx(i) + signyy(i)*signyy(i) -   &
                             signxx(i)*signyy(i) + three*signxy(i)*signxy(i))
                    gbuf%wpla(i) = gbuf%wpla(i) +                               &
                               half*(vm0(i) + vm(i))*dpla(i)*thklyl(i)*area(i)
                  enddo
                endif
              endif
!-----------------------------------------------
!         failure models
!-----------------------------------------------
              if (ifailure == 1) then
                if ((itask==0).and.(imon_mat==1))call startime(TIMERS,121)
!
                if (ixfem > 0) then
                  do i=jft,jlt
                    tensx(i,1) = zero
                    tensx(i,2) = zero
                    tensx(i,3) = zero
                    tensx(i,4) = zero
                    tensx(i,5) = zero
                  enddo
                endif
!
                sigoff(1:nel) = one
                dmg_loc_scale(1:nel) = one  ! local dmg scale by integration point and failure model
                dmg_orth_scale(1:nel,1:5) = one
!
                mpt  = npttot * max(1,npg)
                fbuf => bufly%fail(ir,is,it)
                ! bufly%fail(ir,is,it)%fbuf%floc(ifl)%dfmax

!
                ! length used for regularization (failure criterion parameters scaling)
                !  -> if non-local, criterion parameters are scaled with le_max parameter
                if (inloc > 0) then
                  le_max(1:nel) = nloc_dmg%le_max(mat(1))
                  el_len => le_max(1:nel)
                  !  -> if not, criterion parameters are scaled with the element characteristic length
                else
                  el_len => aldt(1:nel)
                endif
!
!           plastic strain increment for all plastic laws
                if (bufly%l_pla > 0) then
                  ! non-local material
                  if (inloc > 0) then
                    do i=jft,jlt
                      dpla(i)  = max(varnl(i,it),zero)
                      epsd(i) = lbuf%epsdnl(i)
                    enddo
                    el_pla => lbuf%planl(1:nel)
                    ! classical local material
                  else
                    do i=jft,jlt
                      dpla(i)  = lbuf%pla(i) - pla0(i)
                      epsd(i) = lbuf%epsd(i)
                    enddo
                    el_pla => lbuf%pla(1:nel)
                  endif
                else
                  el_pla_dum(1:nel) = zero
                  el_pla => el_pla_dum(1:nel)
                endif
!
                if (ixfem == 1) then
                  uelr1 => elbuf_str%bufly(ilayer)%uelr1
                else
                  uelr1 => gbuf%uelr1
                endif
                if (ixlay == 0) then  ! standard element
                  uelr  => gbuf%uelr
                elseif (ixlay > 0) then ! xfem phantom element
                  uelr  => elbuf_str%bufly(ixlay)%uelr
                endif
                dadv => gbuf%dmg(1:nel)  ! used outside this routine for frontwave propagation
!---
                offl => lbuf%off
                offly=> bufly%off
                zt   = posly(1,ipt)
!---
                do ifl = 1, nfail      ! loop over fail models in current layer
                  uvarf  => fbuf%floc(ifl)%var
                  nvarf  =  fbuf%floc(ifl)%nvar
                  irupt  =  fbuf%floc(ifl)%ilawf
                  dam    => fbuf%floc(ifl)%dam
                  dfmax  => fbuf%floc(ifl)%dammx
                  damini => fbuf%floc(ifl)%damini
                  tdel   => fbuf%floc(ifl)%tdel
                  fld_idx=> fbuf%floc(ifl)%indx
                  foff   => fbuf%floc(ifl)%off
                  lf_dammx   = fbuf%floc(ifl)%lf_dammx
                  fail_param => mat_elem%mat_param(imat)%fail(ifl)
                  nupar      =  mat_elem%mat_param(imat)%fail(ifl)%nuparam
                  nipar      =  mat_elem%mat_param(imat)%fail(ifl)%niparam
                  nfunc_fail =  mat_elem%mat_param(imat)%fail(ifl)%nfunc
                  ntabl_fail =  mat_elem%mat_param(imat)%fail(ifl)%ntable
                  uparamf    => mat_elem%mat_param(imat)%fail(ifl)%uparam(1:nupar)
                  iparamf    => mat_elem%mat_param(imat)%fail(ifl)%iparam(1:nipar)
                  ifunc_fail => mat_elem%mat_param(imat)%fail(ifl)%ifunc(1:nfunc_fail)
                  itabl_fail => mat_elem%mat_param(imat)%fail(ifl)%table(1:ntabl_fail)
!              if (ixfem == 0) pthkf(ilayer,ifl) = zero
!------------------------------------
                  select case (irupt)
!------------------------------------
                   case (1)     !    johnson-cook
                    if (ixfem == 0) then
                      call fail_johnson_c(&
                      &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                      &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                      &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                      &dpla      ,epsd      ,tstar     ,off       ,foff      ,&
                      &dfmax     ,tdel      )
                    else if (matparam%ixfem > 0) then
                      call fail_johnson_xfem(&
                      &nel      ,nupar    ,uparamf  ,nvarf    ,uvarf    ,&
                      &tt       ,tensx    ,dpla     ,epsd     ,tstar    ,&
                      &ngl      ,ipt      ,mpt      ,nptt     ,uelr1    ,&
                      &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                      &off      ,offl     ,gbuf%noff,dfmax    ,tdel     ,&
                      &elcrkini ,ixfem    ,ixel     ,ilayer   ,it       )
                    endif
!
                   case (2)     !    tuler butcher
                    if (ixfem == 0) then
                      call fail_tbutcher_c(&
                      &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                      &tt        ,dt1c      ,ipg       ,ilayer    ,it        ,&
                      &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                      &ngl       ,off       ,foff      ,dfmax     ,tdel      )
                    else if (matparam%ixfem > 0) then
                      call fail_tbutcher_xfem(&
                      &nel      ,nupar    ,uparamf  ,nvarf    ,uvarf    ,&
                      &tt       ,dt1c     ,tensx    ,dfmax    ,tdel     ,&
                      &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                      &ngl      ,ipt      ,mpt      ,&
                      &gbuf%noff,off      ,offl     ,elcrkini ,ixfem    ,&
                      &ixel     ,ilayer   ,it       ,nptt     ,uelr1    )
                    endif
!
                   case (3)     !    wilkins
                    call fail_wilkins_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &dpla      ,foff      ,dfmax     ,tdel      )
!
                   case (4)     !    user1
                    do i=jft,jlt
                      copy_pla(i) = lbuf%pla(i)
                    enddo
                    if (logical_userl_avail)then
                      tt_local = tt
                      call eng_userlib_flawc(irupt,nel ,nupar,nvarf,nfunc_fail,ifunc_fail,npf,&
                      &tf  ,tt_local    ,dt1c  ,uparamf,ngl ,ipt,&
                      &mpt ,ipg ,ibidon2,ibidon3  ,&
                      &signxx   ,signyy ,signxy ,signyz ,signzx  ,&
                      &epspxx   ,epspyy ,epspxy ,epspyz ,epspzx  ,&
                      &epsxx    ,epsyy  ,epsxy  ,epsyz  ,epszx   ,&
                      &copy_pla ,dpla   ,epsd   ,uvarf  ,uelr    ,&
                      &off      ,aldt   ,area   ,dfmax,bidon4,bidon5 )
                    else
                      ! ----------------
                      ! error to be printed & exit
                      option='/fail/user1 - shell '
                      size=len_trim(option)
                      call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                      call arret(2)
                      ! ----------------
!!!
                    endif
                    do i=jft,jlt
                      lbuf%pla(i) = copy_pla(i)
                    enddo

!
                   case (5)     !    user2
                    do i=jft,jlt
                      copy_pla(i) = lbuf%pla(i)
                    enddo
                    if (logical_userl_avail)then
                      tt_local = tt
                      call eng_userlib_flawc(irupt,nel ,nupar,nvarf,nfunc_fail,ifunc_fail,npf,&
                      &tf  ,tt_local,dt1c  ,uparamf,ngl ,ipt,&
                      &mpt ,ipg ,ibidon2,ibidon3  ,&
                      &signxx   ,signyy ,signxy ,signyz ,signzx  ,&
                      &epspxx   ,epspyy ,epspxy ,epspyz ,epspzx  ,&
                      &epsxx    ,epsyy  ,epsxy  ,epsyz  ,epszx   ,&
                      &copy_pla ,dpla   ,epsd   ,uvarf  ,uelr    ,&
                      &off      ,aldt   ,area   ,dfmax,bidon4,bidon5 )
                    else
                      ! ----------------
                      ! error to be printed & exit
                      option='/FAIL/USER2 - SHELL '
                      size=len_trim(option)
                      call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                      call arret(2)
                      ! ----------------
!!!
                    endif
                    do i=jft,jlt
                      lbuf%pla(i) = copy_pla(i)
                    enddo

!
                   case (6)     !    user3
                    do i=jft,jlt
                      copy_pla(i) = lbuf%pla(i)
                    enddo
                    if (logical_userl_avail)then
                      tt_local = tt
                      call eng_userlib_flawc(irupt,nel ,nupar,nvarf,nfunc_fail,ifunc_fail,npf,&
                      &tf  ,tt_local      ,dt1c  ,uparamf,ngl ,ipt,&
                      &mpt ,ipg ,ibidon2,ibidon3  ,&
                      &signxx   ,signyy ,signxy ,signyz ,signzx  ,&
                      &epspxx   ,epspyy ,epspxy ,epspyz ,epspzx  ,&
                      &epsxx    ,epsyy  ,epsxy  ,epsyz  ,epszx   ,&
                      &copy_pla ,dpla   ,epsd   ,uvarf  ,uelr    ,&
                      &off      ,aldt   ,area   ,dfmax,bidon4,bidon5 )
                    else
                      ! ----------------
                      ! error to be printed & exit
                      option='/FAIL/USER3 - SHELL '
                      size=len_trim(option)
                      call ancmsg(msgid=257,c1=option(1:size),anmode=aninfo)
                      call arret(2)
                      ! ----------------
!!!
                    endif
                    do i=jft,jlt
                      lbuf%pla(i) = copy_pla(i)
                    enddo
!
                   case (7)     !    fld
                    do i=jft,jlt
                      zt=posly(i,ipt) *thk0(i)
                      epsxx(i)= gstr(i,1)+zt*gstr(i,6)
                      epsyy(i)= gstr(i,2)+zt*gstr(i,7)
                      epsxy(i)= gstr(i,3)+zt*gstr(i,8)
                      epsyz(i)= gstr(i,4)
                      epszx(i)= gstr(i,5)
                    enddo
                    if (ixfem == 0) then
                      call fail_fld_c(&
                      &nel       ,nupar     ,nfunc_fail,ifunc_fail ,&
                      &npf       ,tf        ,tt        ,uparamf    ,&
                      &ngl       ,ipg       ,ilayer    ,it         ,&
                      &epsxx     ,epsyy     ,epsxy     ,lf_dammx   ,&
                      &depsxx    ,depsyy    ,depsxy    ,el_pla     ,&
                      &zt        ,off       ,foff      ,tdel       ,&
                      &fld_idx   ,dam       ,dfmax     ,dt1        ,&
                      &nipar     ,iparamf   ,nvarf     ,uvarf      )
                    else if (matparam%ixfem > 0) then
                      call fail_fld_xfem(&
                      &nel       ,nupar     ,nvarf     ,nfunc_fail,ifunc_fail,&
                      &npf       ,tf        ,tt        ,uparamf   ,&
                      &ngl       ,ipt       ,mpt       ,ssp       ,tensx     ,&
                      &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                      &epsxx     ,epsyy     ,epsxy     ,epsyz     ,epszx     ,&
                      &uvarf     ,gbuf%noff ,off       ,lf_dammx  ,&
                      &elcrkini  ,ixfem     ,ixel      ,ilayer    ,it        ,&
                      &offl      ,nptt      ,uelr1     ,dfmax     ,tdel      ,&
                      &dam       ,fld_idx   ,nipar     ,iparamf   ,el_pla    ,&
                      &depsxx    ,depsyy    ,depsxy    ,dt1       )
                    endif
!
                   case (9)     !    xue_wierzbicki
                    call fail_wierzbicki_c(&
                    &nel       ,nupar     ,uparamf   ,nvarf     ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &dpla      ,off       ,foff      ,dfmax     ,&
                    &tdel      )
!
                   case (10)     !    tension strain failure model
                    call fail_tensstrain_c(&
                    &nel       ,nfunc_fail    ,nupar     ,nvarf     ,ifunc_fail    ,&
                    &uparamf   ,uvarf     ,npf       ,tf        ,tt        ,&
                    &ngl       ,ipg       ,ilayer    ,it        ,epsd      ,&
                    &epsxx     ,epsyy     ,epsxy     ,epsyz     ,epszx     ,&
                    &off       ,foff      ,dfmax     ,tdel      ,&
                    &dmg_flag  ,dmg_loc_scale ,aldt  ,tstar     ,ismstr    )
!
                   case (11)     !    energy failure model
                    call fail_energy_c(&
                    &nel       ,nupar     ,nvarf     ,nfunc_fail   ,ifunc_fail     ,&
                    &uparamf   ,uvarf     ,npf       ,tf       ,tt         ,&
                    &ngl       ,ipg       ,ilayer    ,it       ,epsd       ,&
                    &area      ,thkn      ,dmg_flag  ,&
                    &dmg_loc_scale ,off   ,foff      ,dfmax    ,tdel       ,&
                    &signxx    ,signyy    ,signxy    ,signyz   ,signzx     ,&
                    &depsxx    ,depsyy    ,depsxy    ,depsyz   ,depszx     )
!
                   case (13)     !    chang-chang failure model
                    call fail_changchang_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,dt1c      ,ipg       ,ilayer    ,it        ,&
                    &ngl       ,dmg_flag  ,dmg_loc_scale ,dfmax ,tdel      ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,lf_dammx  )
!
                   case (14)     !    hashin failure model
!                    do i=jft,jlt
!                      epsp(i) = max(abs(epspxx(i)),abs(epspyy(i)),&
!                      &                                   abs(epspxy(i)),em20)
!                    enddo
                    call fail_hashin_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf      ,&
                    &tt        ,dt1c      ,ipg       ,ilayer    ,it         ,&
                    &ngl       ,dmg_flag  ,dmg_loc_scale,dfmax  ,tdel       ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx     ,&
                    &off       ,foff      ,ply_id    ,&
                    &epsd      ,fwave_el  ,gbuf%dmg  ,lf_dammx  )
!
                   case (16)     !    modified puck failure model
                    call fail_puck_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,dmg_flag  ,dmg_loc_scale ,&
                    &dfmax     ,lf_dammx  ,tdel      ,dt1c      )
!
                   case (23)     !    tabulated failure model
                    if (ixfem == 0) then
                      call fail_tab_c(&
                      &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                      &nfunc_fail,ifunc_fail,table     ,npf       ,tf        ,&
                      &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                      &signxx    ,signyy    ,signxy    ,ntabl_fail,itabl_fail,&
                      &dpla      ,epsd      ,thkn      ,el_len    ,tstar     ,&
                      &dmg_flag  ,dmg_loc_scale ,off   ,foff      ,&
                      &dfmax     ,tdel      ,inloc     )
                    else if (matparam%ixfem > 0) then
                      call fail_tab_xfem(&
                      &nel      ,nupar    ,nvarf    ,npf      ,tf       ,&
                      &tt       ,dt1c     ,uparamf  ,ngl      ,ipt      ,&
                      &mpt      ,nfunc_fail,ifunc_fail   ,table    ,&
                      &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                      &dpla     ,epsd     ,tstar    ,tensx    ,uvarf    ,&
                      &gbuf%noff,aldt     ,off      ,offl     ,elcrkini ,&
                      &ixfem    ,ixel     ,ilayer   ,dfmax    ,tdel     ,&
                      &dmg_flag ,ntabl_fail,itabl_fail)
                    endif
!
                   case (24)     !    orthotropic strain failure model
                    call fail_orthstrain_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &nfunc_fail    ,ifunc_fail    ,npf       ,tf        ,ngl       ,&
                    &tt        ,dt1       ,ipg       ,ilayer    ,it        ,&
                    &epsxx     ,epsyy     ,epsxy     ,dmg_flag  ,dmg_loc_scale ,&
                    &epspxx    ,epspyy    ,epspxy    ,aldt      ,ismstr    ,&
                    &signxx    ,signyy    ,signxy    ,lf_dammx  ,&
                    &off       ,offly     ,foff      ,dfmax     ,tdel      )
!
                   case (25)     !    nxt failure
                    call fail_nxt_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,npf       ,tf        ,nfunc_fail    ,ifunc_fail    ,&
                    &ngl       ,ipg       ,ilayer    ,it        ,hardm     ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,dfmax     ,tdel      ,lf_dammx  )
!
                   case (28)     !    windshield failure (christian alter model)
                    irot   =  elbuf_str%bufly(ilayer)%ly_dira
                    crkdir => elbuf_str%bufly(ilayer)%crkdir
                    ifailwv = failwave%wave_mod
                    progressive_crack = 1
                    orth_damage = 1
!
                    if (ixfem > 0) then
                      if (ixel == 0) then
                        crklen => elbuf_str%bufly(ilayer)%dmg(1:nel)
                      else
                        crklen => aldt(1:nel)
                      endif
                      call fail_wind_xfem(&
                      &nel        ,nupar      ,nvarf      ,uparamf    ,uvarf      ,&
                      &tt         ,dt1        ,ssp        ,aldt       ,crklen     ,&
                      &elcrkini   ,uelr1      ,off        ,offly      ,&
                      &signxx     ,signyy     ,signxy     ,signyz     ,signzx     ,&
                      &ngl        ,ixel       ,ilayer     ,ipt        ,nptt       ,&
                      &ixfem      ,irot       ,dir_a(jdir),dir1_crk   ,dir2_crk   ,&
                      &crkdir     )
!
                    elseif (ifailwv > 0) then
                      call fail_wind_frwave(&
                      &nel        ,nupar      ,nvarf      ,uparamf    ,uvarf      ,&
                      &tt         ,dt1        ,ssp        ,aldt       ,fwave_el   ,&
                      &uelr       ,uelr1      ,off        ,offly      ,foff       ,&
                      &signxx     ,signyy     ,signxy     ,dfmax      ,ngl        ,&
                      &ilayer     ,ipt        ,nptt       ,crkdir     ,dadv       ,&
                      &dmg_flag   ,trelax     )
                    endif
!
                   case (30)     !    biquadratic failure model
                    call fail_biquad_c(&
                    &jlt      ,nvarf   ,&
                    &tt       ,uparamf ,ngl      ,ipt      ,mpt      ,&
                    &signxx   ,signyy  ,signxy   ,signyz   ,signzx   ,&
                    &dpla     ,uvarf   ,uelr1    ,&
                    &off      ,offl    ,dfmax    ,tdel     ,nfunc_fail   ,&
                    &ifunc_fail,npf     ,tf      ,el_len   ,foff     ,ipg      ,&
                    &dmg_flag,dmg_loc_scale)
!
                   case (31)     !    anisotropic fabric failure model
!
                    if (ismstr == 11) then
                      eps1(1:nel) = depsxx(1:nel)
                      eps2(1:nel) = depsyy(1:nel)
                    else
                      ii = nel*3
                      jj = nel + ii
                      eps1(1:nel) = uvar(ii:ii+nel)
                      eps2(1:nel) = uvar(jj:jj+nel)
                    endif
                    call fail_fabric_c(&
                    &nel      ,ngl       ,nupar     ,nvarf     ,nfunc_fail    ,&
                    &uparamf  ,uvarf     ,ifunc_fail    ,tt        ,dt1       ,&
                    &npf      ,tf        ,depsxx     ,depsyy     ,eps1      ,&
                    &eps2     ,signxx    ,signyy    ,dfmax     ,tdel      ,&
                    &ipg      ,ilayer    ,it        ,off       ,foff      )
!
                   case (32)     !    hc_dsse failure model
                    call fail_hc_dsse_c(&
                    &nel      ,nupar    ,nvarf    ,uparamf   ,uvarf     ,&
                    &tt       ,ngl      ,ipt      ,ilayer    ,it        ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx     ,&
                    &dpla     ,off      ,foff     ,&
                    &dfmax    ,tdel     ,uelr1    ,mpt      ,&
                    &fld_idx  ,dam      ,el_pla   )
!
                   case (34)     !    cockcroft-latham failure model
                    call fail_cockroft_c(&
                    &jlt      ,nvarf    ,&
                    &tt       ,uparamf  ,ngl      ,ipt      ,ilayer    ,&
                    &mpt      ,it       ,ipg       ,&
                    &signxx   ,signyy   ,signxy   ,&
                    &epsxx    ,epsyy    ,epsxy    ,epsyz    ,epszx     ,&
                    &dpla     ,uvarf    ,uelr1    ,foff     ,&
                    &off      ,dfmax    ,tdel   )
!
                   case (36)     !    visual failure model
                    if (ilaw == 2) then
                      do i=jft,jlt
                        zt=posly(i,ipt) *thk0(i)
                        epsxx(i)= gstr(i,1)+zt*gstr(i,6)
                        epsyy(i)= gstr(i,2)+zt*gstr(i,7)
                        epsxy(i)= gstr(i,3)+zt*gstr(i,8)
                      enddo
                    endif
                    call fail_visual_c(&
                    &jlt      ,nvarf   ,tt       ,dt1     ,uparamf  ,ngl      ,&
                    &signxx   ,signyy  ,signxy   ,epsxx   ,epsyy    ,epsxy    ,&
                    &uvarf    ,off     ,dfmax    ,ismstr  )
!
                   case (37)     !    old (obsolete) tabulated failure model
                    if (ixfem == 0) then
                      call fail_tab_old_c(&
                      &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                      &nfunc_fail    ,ifunc_fail    ,npf       ,tf        ,&
                      &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                      &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                      &dpla      ,epsd      ,thkn      ,el_len    ,tstar     ,&
                      &off       ,foff      ,dfmax     ,tdel      )
                    else if (matparam%ixfem > 0) then
                      call fail_tab_old_xfem(&
                      &nel      ,nupar    ,nvarf    ,npf      ,tf       ,&
                      &tt       ,dt1c     ,uparamf  ,ngl      ,ipt      ,&
                      &mpt      ,nfunc_fail   ,ifunc_fail   ,dmg_flag ,&
                      &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                      &dpla     ,epsd     ,tstar    ,tensx    ,uvarf    ,&
                      &gbuf%noff,aldt     ,off      ,offl     ,elcrkini ,&
                      &ixfem    ,ixel     ,ilayer   ,dfmax    ,tdel     )
                    endif
!
                   case (38)     !    orthotropic biquad
!
                    call fail_orthbiquad_c(&
                    &jlt      ,nvarf   ,&
                    &tt       ,uparamf ,ngl      ,ipt      ,mpt      ,&
                    &signxx   ,signyy  ,signxy   ,signyz   ,signzx   ,&
                    &dpla     ,epsd    ,uvarf    ,uelr1    ,&
                    &off      ,offl    ,dfmax    ,tdel     ,nfunc_fail   ,&
                    &ifunc_fail,npf    ,tf       ,el_len   ,foff     ,ipg    )
!
                   case (39)     !    gene1
!
                    call fail_gene1_c(&
                    &jlt      ,nupar    ,nvarf    ,nfunc_fail,ifunc_fail,&
                    &npf      ,tf       ,tt       ,dt1c     ,uparamf  ,ipg      ,&
                    &ngl      ,gbuf%dt  ,epsd     ,uvarf    ,off      ,&
                    &epsxx    ,epsyy    ,epsxy    ,area     ,thkn     ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                    &el_temp  ,dfmax    ,aldt     ,table    ,tdel     ,&
                    &thk0     ,ipt      ,foff     ,thklyl   ,ntabl_fail,itabl_fail,&
                    &lf_dammx ,nipar    ,iparamf  ,dt)
!
                   case (40)     !    rtcl
!
                    call fail_rtcl_c(&
                    &jlt      ,nupar    ,nvarf    ,tt       ,dt1c     ,uparamf  ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,mpt      ,&
                    &ngl      ,dpla     ,uvarf    ,off      ,dfmax    ,tdel     ,&
                    &area     ,foff     ,igtyp    ,offl     ,ipt      ,thk0     )
!
                   case (41)     !    tab2
!
                    call fail_tab2_c(&
                    &jlt      ,nupar    ,nvarf    ,nfunc_fail   ,ifunc_fail   ,&
                    &npf      ,table    ,tf       ,tt       ,uparamf  ,&
                    &ngl      ,el_len   ,dpla     ,epsd     ,uvarf    ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                    &el_temp  ,foff     ,dfmax    ,tdel     ,ipt      ,&
                    &ipg      ,dmg_flag ,dmg_loc_scale,ntabl_fail,itabl_fail)
!
                   case (42)     !    inievo
!
                    call fail_inievo_c(&
                    &jlt      ,nupar    ,nvarf    ,&
                    &table    ,ntabl_fail,itabl_fail   ,tt       ,uparamf  ,&
                    &ngl      ,el_len   ,dpla     ,epsd     ,uvarf    ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                    &el_pla   ,el_temp  ,sigy     ,foff     ,dfmax    ,&
                    &tdel     ,ipt      ,ipg      ,dmg_flag ,dmg_loc_scale,&
                    &damini   ,area     ,inloc    ,npg      )
!
                   case (43)     !    syazwan failure model
!
                    call fail_syazwan_c(&
                    &jlt      ,uparamf  ,nupar    ,uvarf    ,nvarf    ,&
                    &tt       ,ngl      ,ipt      ,dpla     ,el_pla   ,&
                    &signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,&
                    &epsxx    ,epsyy    ,epsxy    ,epsyz    ,epszx    ,&
                    &dfmax    ,nfunc_fail   ,ifunc_fail   ,el_len   ,foff     ,&
                    &ipg      ,dmg_flag ,dmg_loc_scale,npf  ,tf       )
!
                   case (44)     !    tsai-wu criterion
!
                    call fail_tsaiwu_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,dmg_flag  ,dmg_loc_scale ,&
                    &dfmax     ,lf_dammx  ,tdel      ,dt1c      )
!
                   case (45)     !    tsai-hill criterion
!
                    call fail_tsaihill_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,dmg_flag  ,dmg_loc_scale ,&
                    &dfmax     ,lf_dammx  ,tdel      ,dt1c      )
!
                   case (46)     !    hoffman criterion
!
                    call fail_hoffman_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,&
                    &off       ,foff      ,dmg_flag  ,dmg_loc_scale ,&
                    &dfmax     ,lf_dammx  ,tdel      ,dt1c      )
!
                   case (47)     !    maximum strain failure model
!
                    call fail_maxstrain_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &tt        ,ngl       ,ipg       ,ilayer    ,it        ,&
                    &epsxx     ,epsyy     ,epsxy     ,epsyz     ,epszx     ,&
                    &off       ,foff      ,dmg_flag  ,dmg_loc_scale ,&
                    &dfmax     ,lf_dammx  ,tdel      ,dt1c      )
!
                   case (48)     !    orthotropic energy failure model
!
                    call fail_orthenerg_c(&
                    &nel       ,nupar     ,nvarf     ,uparamf   ,uvarf     ,&
                    &ngl       ,tt        ,ipg       ,ilayer    ,it        ,&
                    &depsxx    ,depsyy    ,depsxy    ,dmg_flag  ,dmg_orth_scale,&
                    &aldt      ,foff      ,dfmax     ,tdel      ,&
                    &signxx    ,signyy    ,signxy    ,igtyp     ,ply_id    )
!
                   case (50)     !    Lemaitre continuum damage model
!
                    call fail_lemaitre_c(&
                    &nel       ,nupar     ,uparamf   ,matparam  ,&
                    &signxx    ,signyy    ,signxy    ,dpla      ,el_pla   ,&
                    &foff      ,off       ,dfmax     ,tdel      ,dmg_flag ,&
                    &dmg_loc_scale,ipg    ,ply_id    ,ilayer    ,it       ,&
                    &ngl       ,tt        ,igtyp     )
!
                   case (51)     !    Composite failure model
!
                    call fail_composite_c(&
                    &nel       ,fail_param,nuvar     ,uvarf    ,tt        ,&
                    &ngl       ,ipg       ,ilayer    ,it       ,ply_id    ,&
                    &igtyp     ,tdel      ,signxx    ,signyy    ,signxy   ,&
                    &foff      ,dmg_flag  ,dmg_loc_scale,lf_dammx,dfmax   )
!
!-------------
                  end select
!-------------
                  if (orth_damage == 0) then
                    do i=1,nel
                      if (foff(i) == 0)  then
                        offl(i) = zero
!                    sigoff(i) = offl(i)
                        sigoff(i) = zero
                      endif
                    enddo
                  else
                    do i=1,nel
                      if (off(i) == zero)  then
                        sigoff(i) = zero
                      endif
                    enddo
                  endif
!-------------
                enddo     !  nfail
                if ((itask==0).and.(imon_mat==1)) call stoptime(TIMERS,121)
              endif  ! if (ifailure == 1)
!-----------------------------------
!         end of failure models
!-----------------------------------
#include "vectorize.inc"
              do i=jft,jlt
                ! ij(k) = nel*(k-1)
                lbuf%sig(nel*(1-1)+i) = signxx(i) * sigoff(i)
                lbuf%sig(nel*(2-1)+i) = signyy(i) * sigoff(i)
                lbuf%sig(nel*(3-1)+i) = signxy(i) * sigoff(i)
                lbuf%sig(nel*(4-1)+i) = signyz(i) * sigoff(i)
                lbuf%sig(nel*(5-1)+i) = signzx(i) * sigoff(i)
              enddo
!
!------------------------------------------------
              if (igtyp == 1) then
                select case (dmg_flag)
                 case (0)
                  if (ilaw==58 .or. ilaw == 158 .or. iprony == 1) then
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*(signxx(1:nel)+sigvxx(1:nel))
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*(signyy(1:nel)+sigvyy(1:nel))
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*(signxy(1:nel)+sigvxy(1:nel))
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*(signyz(1:nel)+sigvyz(1:nel))
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*(signzx(1:nel)+sigvzx(1:nel))
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel)*(signxx(1:nel)+sigvxx(1:nel))
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel)*(signyy(1:nel)+sigvyy(1:nel))
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel)*(signxy(1:nel)+sigvxy(1:nel))
                  else
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*signxx(1:nel)
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*signyy(1:nel)
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*signxy(1:nel)
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*signyz(1:nel)
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*signzx(1:nel)
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *signxx(1:nel)
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *signyy(1:nel)
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *signxy(1:nel)
                  endif
                 case (1)  ! softening with dmg_loc_scale from failure model
                  if (ilaw==58 .or. ilaw == 158 .or. iprony == 1) then
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*(signxx(1:nel)+sigvxx(1:nel))*dmg_loc_scale(1:nel)
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*(signyy(1:nel)+sigvyy(1:nel))*dmg_loc_scale(1:nel)
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*(signxy(1:nel)+sigvxy(1:nel))*dmg_loc_scale(1:nel)
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*(signyz(1:nel)+sigvyz(1:nel))*dmg_loc_scale(1:nel)
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*(signzx(1:nel)+sigvzx(1:nel))*dmg_loc_scale(1:nel)
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *(signxx(1:nel)+sigvxx(1:nel))*dmg_loc_scale(1:nel)
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *(signyy(1:nel)+sigvyy(1:nel))*dmg_loc_scale(1:nel)
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *(signxy(1:nel)+sigvxy(1:nel))*dmg_loc_scale(1:nel)
                  else
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*signxx(1:nel)*dmg_loc_scale(1:nel)
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*signyy(1:nel)*dmg_loc_scale(1:nel)
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*signxy(1:nel)*dmg_loc_scale(1:nel)
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*signyz(1:nel)*dmg_loc_scale(1:nel)
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*signzx(1:nel)*dmg_loc_scale(1:nel)
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *signxx(1:nel)*dmg_loc_scale(1:nel)
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *signyy(1:nel)*dmg_loc_scale(1:nel)
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *signxy(1:nel)*dmg_loc_scale(1:nel)
                  endif
                 case (2)  ! internal damage model within law15
                  for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*sigdmg(1:nel,1)
                  for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*sigdmg(1:nel,2)
                  for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*sigdmg(1:nel,3)
                  for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*sigdmg(1:nel,4)
                  for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*sigdmg(1:nel,5)
                  mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *sigdmg(1:nel,1)
                  mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *sigdmg(1:nel,2)
                  mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *sigdmg(1:nel,3)
                 case (3)  ! orthotropic softening with dmg_orth_scale from failure model
                  if (ilaw==58 .or. ilaw == 158 .or. iprony == 1) then
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*(signxx(1:nel)+sigvxx(1:nel))*dmg_orth_scale(1:nel,1)
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*(signyy(1:nel)+sigvyy(1:nel))*dmg_orth_scale(1:nel,2)
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*(signxy(1:nel)+sigvxy(1:nel))*dmg_orth_scale(1:nel,3)
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*(signyz(1:nel)+sigvyz(1:nel))*dmg_orth_scale(1:nel,4)
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*(signzx(1:nel)+sigvzx(1:nel))*dmg_orth_scale(1:nel,5)
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *(signxx(1:nel)+sigvxx(1:nel))*dmg_orth_scale(1:nel,1)
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *(signyy(1:nel)+sigvyy(1:nel))*dmg_orth_scale(1:nel,2)
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *(signxy(1:nel)+sigvxy(1:nel))*dmg_orth_scale(1:nel,3)
                  else
                    for(1:nel,1) = for(1:nel,1) + thkly(jpos:jpos+nel-1)*signxx(1:nel)*dmg_orth_scale(1:nel,1)
                    for(1:nel,2) = for(1:nel,2) + thkly(jpos:jpos+nel-1)*signyy(1:nel)*dmg_orth_scale(1:nel,2)
                    for(1:nel,3) = for(1:nel,3) + thkly(jpos:jpos+nel-1)*signxy(1:nel)*dmg_orth_scale(1:nel,3)
                    for(1:nel,4) = for(1:nel,4) + thkly(jpos:jpos+nel-1)*signyz(1:nel)*dmg_orth_scale(1:nel,4)
                    for(1:nel,5) = for(1:nel,5) + thkly(jpos:jpos+nel-1)*signzx(1:nel)*dmg_orth_scale(1:nel,5)
                    mom(1:nel,1) = mom(1:nel,1) + wmc(1:nel) *signxx(1:nel)*dmg_orth_scale(1:nel,1)
                    mom(1:nel,2) = mom(1:nel,2) + wmc(1:nel) *signyy(1:nel)*dmg_orth_scale(1:nel,2)
                    mom(1:nel,3) = mom(1:nel,3) + wmc(1:nel) *signxy(1:nel)*dmg_orth_scale(1:nel,3)
                  endif
                end select
!
              else   ! igtyp /= 1)
!
                select case (dmg_flag)
                 case (0)
                  if (ilaw == 58 .or. ilaw == 158 .or. iprony == 1) then
                    tens(jft:jlt,1) = signxx(jft:jlt)+sigvxx(jft:jlt)
                    tens(jft:jlt,2) = signyy(jft:jlt)+sigvyy(jft:jlt)
                    tens(jft:jlt,3) = signxy(jft:jlt)+sigvxy(jft:jlt)
                    tens(jft:jlt,4) = signyz(jft:jlt)+sigvyz(jft:jlt)
                    tens(jft:jlt,5) = signzx(jft:jlt)+sigvzx(jft:jlt)
                  else
                    tens(jft:jlt,1) = signxx(jft:jlt)
                    tens(jft:jlt,2) = signyy(jft:jlt)
                    tens(jft:jlt,3) = signxy(jft:jlt)
                    tens(jft:jlt,4) = signyz(jft:jlt)
                    tens(jft:jlt,5) = signzx(jft:jlt)
                  endif
                 case (1)
                  if (ilaw == 58 .or. ilaw == 158 .or. iprony == 1) then
                    tens(jft:jlt,1) = (signxx(jft:jlt)+sigvxx(jft:jlt))*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,2) = (signyy(jft:jlt)+sigvyy(jft:jlt))*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,3) = (signxy(jft:jlt)+sigvxy(jft:jlt))*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,4) = (signyz(jft:jlt)+sigvyz(jft:jlt))*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,5) = (signzx(jft:jlt)+sigvzx(jft:jlt))*dmg_loc_scale(jft:jlt)
                  else
                    tens(jft:jlt,1) = signxx(jft:jlt)*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,2) = signyy(jft:jlt)*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,3) = signxy(jft:jlt)*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,4) = signyz(jft:jlt)*dmg_loc_scale(jft:jlt)
                    tens(jft:jlt,5) = signzx(jft:jlt)*dmg_loc_scale(jft:jlt)
                  endif
                 case (2)  ! internal damage model within law15
                  tens(jft:jlt,1) = sigdmg(jft:jlt,1)
                  tens(jft:jlt,2) = sigdmg(jft:jlt,2)
                  tens(jft:jlt,3) = sigdmg(jft:jlt,3)
                  tens(jft:jlt,4) = sigdmg(jft:jlt,4)
                  tens(jft:jlt,5) = sigdmg(jft:jlt,5)
                 case (3)  ! orthotropic softening with dmg_orth_scale from failure model
                  if (ilaw == 58 .or. ilaw == 158 .or. iprony == 1) then
                    tens(jft:jlt,1) = (signxx(jft:jlt)+sigvxx(jft:jlt))*dmg_orth_scale(jft:jlt,1)
                    tens(jft:jlt,2) = (signyy(jft:jlt)+sigvyy(jft:jlt))*dmg_orth_scale(jft:jlt,2)
                    tens(jft:jlt,3) = (signxy(jft:jlt)+sigvxy(jft:jlt))*dmg_orth_scale(jft:jlt,3)
                    tens(jft:jlt,4) = (signyz(jft:jlt)+sigvyz(jft:jlt))*dmg_orth_scale(jft:jlt,4)
                    tens(jft:jlt,5) = (signzx(jft:jlt)+sigvzx(jft:jlt))*dmg_orth_scale(jft:jlt,5)
                  else
                    tens(jft:jlt,1) = signxx(jft:jlt)*dmg_orth_scale(jft:jlt,1)
                    tens(jft:jlt,2) = signyy(jft:jlt)*dmg_orth_scale(jft:jlt,2)
                    tens(jft:jlt,3) = signxy(jft:jlt)*dmg_orth_scale(jft:jlt,3)
                    tens(jft:jlt,4) = signyz(jft:jlt)*dmg_orth_scale(jft:jlt,4)
                    tens(jft:jlt,5) = signzx(jft:jlt)*dmg_orth_scale(jft:jlt,5)
                  endif
                end select
!
                if (ilaw == 58 .or. ilaw == 158) then
                  do i=jft,jlt
                    ii = jdir + i-1
                    r1 = dir_a(ii)
                    s1 = dir_a(ii+nel)
                    r2 = dir_b(ii)
                    s2 = dir_b(ii+nel)
                    rs1= r1*s1
                    rs2= r2*s2
                    r12a = r1*r1
                    r22a = r2*r2
                    s12b = s1*s1
                    s22b = s2*s2
                    rs3 = s1*s2-r1*r2
                    r3r3= one+s1*r2+r1*s2
                    r3r3= half*r3r3
                    s3s3= one-s1*r2-r1*s2
                    s3s3= half*s3s3
                    t1 = tens(i,1)
                    t2 = tens(i,2)
                    t3 = tens(i,3)
                    tens(i,1) = r12a*t1 + r22a*t2 - rs3*t3
                    tens(i,2) = s12b*t1 + s22b*t2 + rs3*t3
                    tens(i,3) = rs1*t1  + rs2*t2 + (r3r3 - s3s3)*t3
                    tens(i,4) = signyz(i)
                    tens(i,5) = signzx(i)
                  enddo
!
                else     ! igtyp = 9,10,11,51,52
                  if (ilaw /= 1 .and. ilaw /= 2 .and. ilaw /= 27 .and. ilaw /= 32)&
                  &call urotov(jft,jlt,tens,dir_a(jdir),nel)
                endif
!----------------------
!           forces and moments when igtyp /= 1
!----------------------
#include "vectorize.inc"
                do i=jft,jlt
                  for(i,1) = for(i,1) + thkly(jpos-1+i)*tens(i,1)
                  for(i,2) = for(i,2) + thkly(jpos-1+i)*tens(i,2)
                  for(i,3) = for(i,3) + thkly(jpos-1+i)*tens(i,3)
                  for(i,4) = for(i,4) + thkly(jpos-1+i)*tens(i,4)
                  for(i,5) = for(i,5) + thkly(jpos-1+i)*tens(i,5)
                  mom(i,1) = mom(i,1) + wmc(i) *tens(i,1)
                  mom(i,2) = mom(i,2) + wmc(i) *tens(i,2)
                  mom(i,3) = mom(i,3) + wmc(i) *tens(i,3)
                enddo
!
              endif   ! igtyp
!-----------------------------------------------
!         facteurs pour coques b.l. (zeng&combescure)
!-----------------------------------------------
              if (ilaw /= 2  .and. ilaw /= 15 .and. ilaw /= 22 .and.    &
                           ilaw /= 25 .and. ilaw /= 27 .and. ilaw /= 32 &
                     .and. ilaw /= 19 .and. ilaw /= 119) then  ! for law25,27 it is done inside sigeps25c.f...
                if(flag_zcfac) then
                  zcfac(jft:jlt,1) = zcfac(jft:jlt,1) + etse(jft:jlt) * thkly(jpos:jpos+jlt-1)
                  zcfac(jft:jlt,2) = min(etse(jft:jlt),zcfac(jft:jlt,2))
                endif
                yld(jft:jlt) = yld(jft:jlt) + sigy(jft:jlt)*thkly(jpos:jpos+jlt-1)
                if(flag_etimp) etimp(jft:jlt) = etimp(jft:jlt) + et_imp(jft:jlt)*thkly(jpos:jpos+jlt-1)
              elseif ( ilaw == 2 ) then
!------------------------------------
                select case (igtyp)
!------------------------------------
                 case (1,9)
                  if (flag_zcfac) then 
                    zcfac(jft:jlt,1) = zcfac(jft:jlt,1) + etse(jft:jlt) / npt
                    zcfac(jft:jlt,2) = min(etse(jft:jlt),zcfac(jft:jlt,2))
                  endif
                  yld(jft:jlt)     = yld(jft:jlt) + sigy(jft:jlt) / npt
                 case default
                  if (flag_zcfac) then 
                    zcfac(jft:jlt,1) = zcfac(jft:jlt,1) + etse(jft:jlt) * thkly(jpos:jpos+jlt-1)
                    zcfac(jft:jlt,2) = min(etse(jft:jlt),zcfac(jft:jlt,2))
                  endif
                  yld(jft:jlt)     = yld(jft:jlt) + sigy(jft:jlt) * thkly(jpos:jpos+jlt-1)
                end select
              endif
!-----------------------------------------------
              if (impl_s > 0) then
                call putsignorc3(jft ,jlt ,iun,ng,ipt,g_imp ,sigksi)
              end if
!-----------------------------------------------
              if (ixel == 0 .and. progressive_crack == 0) then   ! original element
                if (ilaw == 27) then
                  dir_orth => lbuf%dmg
                  irot = elbuf_str%bufly(ilayer)%l_dmg
                else
                  dir_orth => bufly%dira
                  irot = elbuf_str%bufly(ilayer)%ly_dira
                endif
                if (ixfem == 1) then        ! multilayer
                  call xfem_crk_dir(&
                  &jlt        ,ilayer ,ixfem    ,elcrkini,&
                  &dir_orth,tensx  ,dir1_crk ,dir2_crk ,irot    )
                elseif (ixfem == 2) then    ! monolayer
                  iptx = 1
                  call xfem_crk_dir(&
                  &jlt    ,iptx    ,ixfem   ,elcrkini,&
                  &dir_orth  ,tensx  ,dir1_crk,dir2_crk,irot    )
                endif
              endif
!------------------------------------------------------------
!     variable to regularize with non-local
!------------------------------------------------------------
              if (bufly%l_pla > 0) then
                ! non-local material
                if (inloc > 0) then
                  do i=jft,jlt
                    if (off(i) == one) then
                      varnl(i,it) = lbuf%pla(i)
                    else
                      varnl(i,it) = zero
                    endif
                  enddo
                endif
              endif
!-------------------------------------
            enddo  !  it=1,nptt
            ipt_all = ipt_all + nptt
          enddo  !  do ilay =1,nlay
!--------------------------------------------------
!     end of loop over integration points (layers)
!--------------------------------------------------
!     test of element failure
!--------------------------------------------------
          ! flag for printing element deletion message
          print_fail(1:nel) = .true.
          ! check element deletion from failure criterion
          if (ifailure == 1 .and. ixfem == 0) then
            if (orth_damage == 1) then   ! /fail/alter + front_wave only
              tfail => gbuf%dmg(1+nel:nel*2)  ! only for /fail/alter
              call fail_setoff_wind_frwave(&
              &elbuf_str,mat_elem ,geo      ,pid(1)   ,&
              &ngl      ,nel      ,ir       ,is       ,&
              &nlay     ,npttot   ,thk_ly   ,thkly    ,&
              &off      ,npg      ,stack    ,isubstack,&
              &igtyp    ,failwave ,fwave_el ,dmg_flag ,&
              &tt       ,trelax   ,tfail    ,dmg_glob_scale)
            elseif (npg == 1) then
              call fail_setoff_c(elbuf_str,mat_elem ,geo      ,pid(1)   ,&
              &ngl      ,nel      ,nlay     ,npttot   ,&
              &thk_ly   ,thkly    ,off      ,stack    ,&
              &isubstack,igtyp    ,failwave ,fwave_el ,&
              &nlay_max ,laynpt_max,numgeo  ,numstack ,&
              &igeo     ,print_fail)
            else
              call fail_setoff_npg_c(&
              &elbuf_str,mat_elem ,geo      ,pid(1)   ,&
              &ngl      ,nel      ,ir       ,is       ,&
              &nlay     ,npttot   ,thk_ly   ,thkly    ,&
              &off      ,npg      ,stack    ,isubstack,&
              &igtyp    ,failwave ,fwave_el ,nlay_max ,&
              &laynpt_max,numgeo  ,ipg      ,numstack ,&
              &igeo     ,print_fail)
            endif
          endif
!---
          ! checking element deletion and printing element deletion messages (if needed)
          nindx = 0
          if (ixfem == 0) then
            do i=jft,jlt
              if (off(i) == four_over_5 .and. ioff_duct(i) == 0 .or.&   ! rupture /fail
              &off(i) > zero   .and. off_old(i) < em01 .or.&   ! rupture progressive law 2,22,25
              &off(i) > zero   .and. off(i) < one .and. dmg_flag == 1 .and.&
              &dmg_glob_scale(i) < em02) then
                off(i) = zero
                ! printing message not always mandatory
                if (print_fail(i)) then
                  nindx  = nindx + 1
                  indx(nindx) = i
                endif
              endif
            enddo
          endif
!--------------------------------------------------------
!     shooting nodes algorithm activation
!--------------------------------------------------------
          do i = 1,nel
            if ((off_old(i) > zero) .and. (off(i) == zero)) then
              idel7nok = 1
            endif
          enddo
!------------------------------------------------------------------
!     membrane viscosity
!---------------------------
          if (igtyp == 11 .and. igmat > 0) then
            ipgmat = 700
            do i=jft,jlt
              rho(i) = geo(ipgmat+1,pid(1))
              ssp(i) = geo(ipgmat+9,pid(1))
            enddo
          elseif (igtyp == 52 .or.&
          &((igtyp == 17 .or. igtyp == 51) .and. igmat > 0)) then
            do i=jft,jlt
              ssp(i) = stack%pm(9,isubstack)
              rho(i) = stack%pm(1,isubstack)
            enddo
          endif
!---
! special treatment for law 19
!---
!  --- start treatment law 19---
!-----------------------------------------------------------
!     desactivation si surface < surface min
!-----------------------------------------------------------
          if (mtn == 19) then
            mx = mat(jft)
            do i=jft,jlt
              areamin(i)  = pm(53,mx)
              dareamin(i) = pm(54,mx)
            enddo
            if (ismstr == 3) then
              do i=jft,jlt
                if (areamin(i) > zero) then
                  aa = (one+gstr(i,1)+gstr(i,2) - areamin(i)) * dareamin(i)
                  aa = min(max(aa,zero),one)
                  for(i,1)=for(i,1)*aa
                  for(i,2)=for(i,2)*aa
                  for(i,3)=for(i,3)*aa
                endif
              enddo
            else
              do i=jft,jlt
                if(areamin(i) > zero)then
                  vv = gstr(i,1)+gstr(i,2)
                  vv = one + (one + (half + one_over_6*vv)*vv)*vv
                  aa = (vv - areamin(i)) * dareamin(i)
                  aa = min(max(aa,zero),one)
                  for(i,1)=for(i,1)*aa
                  for(i,2)=for(i,2)*aa
                  for(i,3)=for(i,3)*aa
                endif
              enddo
            endif ! if (ismstr == 3)
          endif  ! if (mtn == 19)
!  --- end treatment law 19---
!-----------------------------------------------------------------------
          thk(jft:jlt) = max(thkn(jft:jlt),em30)
!
          fact = onep414*dm
          visc(jft:jlt) = fact*ssp(jft:jlt)*sqrt(area(jft:jlt))*dtinv*rho(jft:jlt)
!
          for(jft:jlt,1)=for(jft:jlt,1)+visc(jft:jlt)*(exx(jft:jlt)+half*eyy(jft:jlt))
          for(jft:jlt,2)=for(jft:jlt,2)+visc(jft:jlt)*(eyy(jft:jlt)+half*exx(jft:jlt))
          for(jft:jlt,3)=for(jft:jlt,3)+visc(jft:jlt)* exy(jft:jlt) *third
!
          for(jft:jlt,1)=for(jft:jlt,1)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          for(jft:jlt,2)=for(jft:jlt,2)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          for(jft:jlt,3)=for(jft:jlt,3)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          for(jft:jlt,4)=for(jft:jlt,4)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          for(jft:jlt,5)=for(jft:jlt,5)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          mom(jft:jlt,1)=mom(jft:jlt,1)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          mom(jft:jlt,2)=mom(jft:jlt,2)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
          mom(jft:jlt,3)=mom(jft:jlt,3)*off(jft:jlt)*dmg_glob_scale(jft:jlt)
!
!!  --- specific treatment for law 119 - scale factor applied on internal forces---
          if (ilaw == 119) then
            for(jft:jlt,1)=for(jft:jlt,1)*gbuf%intvar(jft:jlt)
            for(jft:jlt,2)=for(jft:jlt,2)*gbuf%intvar(jft:jlt)
            for(jft:jlt,3)=for(jft:jlt,3)*gbuf%intvar(jft:jlt)
            for(jft:jlt,4)=for(jft:jlt,4)*gbuf%intvar(jft:jlt)
            for(jft:jlt,5)=for(jft:jlt,5)*gbuf%intvar(jft:jlt)
            mom(jft:jlt,1)=mom(jft:jlt,1)*gbuf%intvar(jft:jlt)
            mom(jft:jlt,2)=mom(jft:jlt,2)*gbuf%intvar(jft:jlt)
            mom(jft:jlt,3)=mom(jft:jlt,3)*gbuf%intvar(jft:jlt)
          endif
!
          degmb(jft:jlt) = degmb(jft:jlt)+ for(jft:jlt,1)*exx(jft:jlt)+for(jft:jlt,2)*eyy(jft:jlt)&
          &+ for(jft:jlt,3)*exy(jft:jlt)+for(jft:jlt,4)*eyz(jft:jlt)+ for(jft:jlt,5)*exz(jft:jlt)
          degfx(jft:jlt) = degfx(jft:jlt)+ mom(jft:jlt,1)*kxx(jft:jlt)+mom(jft:jlt,2)*kyy(jft:jlt)+mom(jft:jlt,3)*kxy(jft:jlt)
!-----------------------------------------------------------------------
!---
          if (mtn == 25) then
!----------------------------
!  shear delamination
!----------------------------
! is an obsolete option (still present for backward compatibility)
            if (igmat == 0)&
            &call m25delam(jft,jlt,pm,gstr,gbuf%damdl,mat,ngl,nel)
!
            if (ishplyxfem > 0) then
              mx = mat(jft)
              ipt_all = 0
              do ilay=1,nlay
                ilayer = ilay
!  cas xfem multilayer
                if (ixfem == 1 .and. ixlay > 0) ilayer = ixlay
                nptt = elbuf_str%bufly(ilayer)%nptt
                do it=1,nptt
                  ipt = ipt_all + it        ! count all nptt through all layers
                  do i=jft,jlt
!
                    ply_f(i,1,ipt) = ply_f(i,1,ipt) + visc(i)*(ply_exx(i,ipt)+half*ply_eyy(i,ipt))
                    ply_f(i,2,ipt) = ply_f(i,2,ipt) + visc(i)*(ply_eyy(i,ipt)+half*ply_exx(i,ipt))
                    ply_f(i,3,ipt) = ply_f(i,3,ipt) + visc(i)* ply_exy(i,ipt)*third
                  enddo
!
                  do i=jft,jlt
                    ply_f(i,1,ipt) = ply_f(i,1,ipt)*off(i)
                    ply_f(i,2,ipt) = ply_f(i,2,ipt)*off(i)
                    ply_f(i,3,ipt) = ply_f(i,3,ipt)*off(i)
                    ply_f(i,4,ipt) = ply_f(i,4,ipt)*off(i)
                    ply_f(i,5,ipt) = ply_f(i,5,ipt)*off(i)
                  enddo
                enddo ! do it=1,nptt
                ipt_all = ipt_all + nptt
              enddo ! do ilay=1,nlay
            endif ! if (ishplyxfem /= 0)
          endif ! if (mtn == 25)
!---
          do i=jft,jlt
            vol2 = half*vol0(i)
            if (mtn == 22) vol2 = vol2*off(i)
            eint(i,1) = eint(i,1) + degmb(i)*vol2
            eint(i,2) = eint(i,2) + degfx(i)*thk0(i)*vol2
          enddo
!
          if (jthe > 0 .and. mtn /= 2) then
            die(jft:jlt) = die(jft:jlt) +&
            &coef(jft:jlt)*( degmb(jft:jlt)*half*vol0(jft:jlt) + degfx(jft:jlt)*thk0(jft:jlt)*half*vol0(jft:jlt) )
          endif
!---------------------------------------------------
!      check element failure with xfem
          if (ixfem > 0) then
            do i=jft,jlt
              if (off(i) == four_over_5) then
                off(i) = zero
                nindx  = nindx + 1
                indx(nindx) = i
              endif
            enddo
          endif

!---  shell failure print out
!
          iofc = nindx  !  indx used before in ielof - check if still useful
          if (nindx > 0 .and. ixel == 0) then
            if (imconv == 1) then
              do ii = 1,nindx
                i = indx(ii)
#include    "lockon.inc"
                write(iout, 1000) ngl(i)
                write(istdo,1100) ngl(i),tt
#include    "lockoff.inc"
              enddo
            endif
          endif
!---------------------------------------------------
1000      FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT NUMBER ',I10)
1100      FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
!---------------------------------------------------
          return
        end
!-----
      end module mulawc_mod
