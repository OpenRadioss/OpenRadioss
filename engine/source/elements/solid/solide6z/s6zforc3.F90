!copyright>        OpenRadioss
!copyright>        copyright (c) 1986-2025 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.k
!||====================================================================
!||    s6cforc3               ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||--- called by ------------------------------------------------------
!||    forint                 ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    csmall3                ../engine/source/elements/solid/solide/csmall3.F
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    s6cbilan               ../engine/source/elements/thickshell/solide6c/s6cbilan.F
!||    s6zdefc3               ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||    s6zderi3               ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zdefo3               ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||    s6zrcoor3              ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||    s6zfint3               ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||    s6cfint3               ../engine/source/elements/thickshell/solide6c/s6cfint3.F
!||    s6cfint_reg            ../engine/source/elements/thickshell/solide6c/s6cfint_reg.F
!||    s6zhour3               ../engine/source/elements/solid/solide6z/s6zhour3.F90
!||    s6chour_ctl            ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6ctherm               ../engine/source/elements/thickshell/solide6c/s6ctherm.F
!||    s6cumu3                ../engine/source/elements/thickshell/solide6c/s6cumu3.F
!||    s6cumu3p               ../engine/source/elements/thickshell/solide6c/s6cumu3p.F
!||    s6czero3               ../engine/source/elements/thickshell/solide6c/s6czero3.F
!||    s6fillopt              ../engine/source/elements/thickshell/solide6c/s6fillopt.F
!||    s6for_distor           ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv               ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    s6sav3                 ../engine/source/elements/thickshell/solide6c/s6sav3.F
!||    s6zrrota3              ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||    s8csigp3               ../engine/source/elements/thickshell/solide8c/s8csigp3.F
!||    scordef3               ../engine/source/elements/thickshell/solidec/scordef3.F
!||    scroto_sig             ../engine/source/elements/thickshell/solidec/scroto_sig.F
!||    sdistor_ini            ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sdlen3                 ../engine/source/elements/solid/solide/sdlen3.F
!||    sdlensh3n              ../engine/source/elements/thickshell/solidec/sdlensh3n.F
!||    sdlensh3n2             ../engine/source/elements/thickshell/solide6c/sdlensh3n2.F
!||    sgparav3               ../engine/source/elements/solid/solide/sgparav3.F
!||    smallb3                ../engine/source/elements/solid/solide/smallb3.F
!||    srho3                  ../engine/source/elements/solid/solide/srho3.F
!||    sstra3                 ../engine/source/elements/solid/solide/sstra3.F
!||    tshgeodel3             ../engine/source/elements/thickshell/solidec/tshgeodel3.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod     ../common_source/modules/ale/ale_connectivity_mod.F
!||    constant_mod             ../common_source/modules/constant_mod.F
!||    dt_mod                   ../engine/source/modules/dt_mod.F
!||    elbufdef_mod             ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    eos_param_mod            ../engine/source/materials/eos/eos_param_mod.F90
!||    glob_therm_mod           ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod             ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    matparam_def_mod         ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mmain_mod                ../engine/source/materials/mat_share/mmain.F90
!||    mvsiz_mod                ../common_source/modules/mvsiz_mod.F
!||    names_and_titles_mod     ../common_source/modules/names_and_titles_mod.F
!||    nlocal_reg_mod           ../common_source/modules/nlocal_reg_mod.F
!||    output_mod               ../common_source/modules/output/output_mod.F90
!||    precision_mod            ../common_source/modules/precision_mod.F90
!||    s6chour_ctl_mod          ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6for_distor_mod         ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv_mod             ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    s6zdefc3_mod             ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||    s6zdefo3_mod             ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||    s6zderi3_mod             ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zfint3_mod             ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||    s6zhour3_mod             ../engine/source/elements/solid/solide6z/s6zhour3.F90
!||    s6zrcoor3_mod            ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||    s6zrrota3_mod            ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||    sdistor_ini_mod          ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sensor_mod               ../common_source/modules/sensor_mod.F90
!||    table_mat_vinterp_mod    ../engine/source/materials/mat_share/table_mat_vinterp_mod.F90
!||    table_mod                ../engine/share/modules/table_mod.F
!||    timer_mod                ../engine/source/system/timer_mod.F90
!||====================================================================
      module s6zforc3_mod
        implicit none
      contains

      subroutine s6zforc3(timers, output, elbuf_tab,ng     , &
                           pm       ,geo     ,ixs     ,x      ,&
                           a        ,v       ,w      ,flux   ,&
                           flu1     ,ale_connect  ,iparg  ,&
                           tf       ,npf     ,bufmat  ,partsav,&
                           dt2t     ,neltst  ,ityptst ,stifn  ,fsky   ,&
                           iads     ,offset  ,iparts ,&
                           f11      ,f21     ,f31     ,f12    ,f22    ,&
                           f32      ,f13     ,f23     ,f33    ,f14    ,&
                           f24      ,f34     ,f15     ,f25    ,f35    ,&
                           f16      ,f26     ,f36     ,nel    ,&
                           icp      ,nloc_dmg,&
                           ipm      ,istrain ,igeo    ,gresav ,grth   ,&
                           igrth    ,table   ,mssa    ,dmels  ,voln   ,&
                           itask    ,ioutprt ,mat_elem,h3d_strain ,&
                           temp     ,fthe    ,fthesky ,condn  ,condnsky,&
                           iexpan   ,ifthe   ,icondn  ,dt     ,snpc,stf,&
                           sbufmat  ,svis    ,nsvois  ,iresp,&
                           idel7nok ,maxfunc, imon_mat, userl_avail, &
                           glob_therm,xdp,sensors, &
                           ngroup,npropm,npropg,nixs,nparg, &
                           npropmi,npropgi,numnod,lsky,&
                           dt1,volmin,th_strain, idtmin,&
                           tt, &
                           idyna,igtyp,impl_s, ineg_v,  &
                           iparit,irep,iscau,ismdisp, &
                           ismstr, isorth, isorthg, jale, & 
                           jcvt, jeul, jlag, jsph, jplasol, &
                           jthe, &
                           mstop,mtn, n2d,ncpri,ncycle,&
                           nfilsol,nft,ntable,&
                           numels,numgeo,nummat,numelq,iint)
!c-----------------------------------------------
!c   m o d u l e s
!c-----------------------------------------------
      use output_mod, only : output_
      use timer_mod
      use mmain_mod
      use table_mod
      use mat_elem_mod            
      use nlocal_reg_mod
      use ale_connectivity_mod
      use dt_mod
      use elbufdef_mod
      use sdistor_ini_mod, only : sdistor_ini
      use s6get_xv_mod,    only : s6get_xv
      use s6for_distor_mod,only : s6for_distor
      use s6chour_ctl_mod ,only : s6chour_ctl
      use glob_therm_mod
      use sensor_mod
      use s6zrrota3_mod
      use s6zfint3_mod
      use s6zdefo3_mod
      use s6zdefc3_mod
      use s6zderi3_mod
      use s6zrcoor3_mod
      use s6zhour3_mod


      use precision_mod, only : wp       
      use matparam_def_mod
      use names_and_titles_mod
      use constant_mod
      use eos_param_mod 
      use table_mat_vinterp_mod 
      use mvsiz_mod, only : mvsiz
!c-----------------------------------------------
!c   i m p l i c i t   t y p e s
!c-----------------------------------------------

      implicit none
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      type(timer_), intent(inout) :: timers
      type(output_), intent(inout) :: output
      integer, intent(in) :: snpc                                          !< stiffness parameter
      integer, intent(in) :: stf                                           !< stiffness flag
      integer, intent(in) :: sbufmat                                       !< buffer material parameter
      integer, intent(in) :: nsvois                                        !< number of neighbors
      integer, intent(in) :: iresp                                         !< response parameter
      integer, intent(in) :: maxfunc                                       !< maximum function parameter
      integer, intent(in) :: userl_avail                                   !< user availability flag
      integer, intent(in) :: imon_mat               
                             !< material monitoring parameter
      integer, intent(inout) :: idel7nok  
      integer, intent(in) :: ngroup                                    
      integer, intent(in) ::  npropm
      integer, intent(in) ::  npropg
      integer, intent(in) :: nixs
      integer, intent(in) ::  nparg
      integer, intent(in) :: npropmi
      integer, intent(in) :: iint
      integer, intent(in) :: npropgi
      integer, intent(in) :: numnod
      integer, intent(in) ::  lsky
      integer, intent(in) :: th_strain
      real(kind=WP),              intent(in) :: dt1
      real(kind=wp), intent(in) :: volmin
      integer,dimension(102) :: idtmin
      real(kind=WP), intent(in) :: tt
      

      integer, intent(in) :: idyna                                         !< dynamic condensation flag
      integer, intent(in) :: igtyp                                         !< property type
      integer, intent(in) :: impl_s                                        !< implicit solver flag
      integer, intent(inout) :: ineg_v                                     !< negative volume flag
      integer, intent(in) :: iparit                                        !< parallel iteration flag
      integer, intent(in) :: irep                                          !< reference frame flag
      integer, intent(in) :: iscau                                         !< cauchy stress flag
      integer, intent(in) :: ismdisp                                       !< displacement flag
      integer, intent(in) :: ismstr                                        !< small strain flag
      integer, intent(inout) :: isorth                                        !< orthotropic material flag
      integer, intent(inout) :: isorthg                                       !< global orthotropic flag
      integer, intent(in) :: jale                                          !< ale formulation flag
      integer, intent(in) :: jcvt                                          !< coordinate transformation flag
      integer, intent(in) :: jeul                                          !< eulerian formulation flag
      integer, intent(in) :: jlag                                          !< lagrangian formulation flag
      integer, intent(inout) :: jsph                                          !< smoothed particle hydrodynamics flag
      integer, intent(inout) :: jplasol                                       !< plastic solution flag
      integer, intent(inout) :: jthe                                          !< thermal analysis flag



       integer, intent(inout) :: mstop                                         !< stop flag
       integer, intent(inout) :: mtn                                           !< material type number
       integer, intent(in) :: n2d                                           !< 2d analysis flag
       integer, intent(in) :: ncpri                                         !< print cycle interval
       integer, intent(in) :: ncycle                                        !< current cycle number
       integer, intent(in) :: nfilsol                                       !< fill solution flag
       integer, intent(in) :: nft                                           !< number of first elements
       integer, intent(in) :: ntable                                        !< number of tables
       integer, intent(in) :: numels                                        !< number of elements
       integer, intent(in) :: numgeo                                        !< number of geometric properties
       integer, intent(in) :: nummat                                        !< number of materials
       integer, intent(in) :: numelq

      integer, dimension(nixs,nel), intent(inout) :: ixs                   !< element connectivity array
      integer, dimension(nparg,ngroup), intent(inout) :: iparg             !< element group parameters
      integer, dimension(nel), intent(inout) :: npf                        !< function pointer array
      integer, dimension(8,nel), intent(inout) :: iads                     !< address array
      integer, dimension(nel), intent(inout) :: grth                       !< growth array
      integer, dimension(nel), intent(inout) :: iparts                     !< part array
      integer, dimension(npropmi,nel), intent(inout) :: ipm                !< material property indices
      integer, dimension(npropgi,nel), intent(inout) :: igeo               !< geometric property indices
      integer, dimension(nel), intent(inout) :: igrth                      !< growth indices
      integer, intent(inout) :: itask                                      !< task identifier
      integer, intent(inout) :: ioutprt                                    !< output print flag
      integer, intent(inout) :: neltst                                     !< number of test elements
      integer, intent(inout) :: ityptst                                    !< test type
      integer, intent(inout) :: offset                                     !< memory offset
      integer, intent(inout) :: ng                                         !< group number
      integer, intent(inout) :: nel                                        !< number of elements
      integer, intent(inout) :: icp                                        !< compression parameter
      integer, intent(inout) :: istrain                                    !< strain flag
      integer, intent(inout) :: h3d_strain                                 !< 3d strain output flag
      integer, intent(in) :: iexpan                                        !< expansion flag
      integer, intent(in) :: ifthe                                         !< thermal force flag
      integer, intent(in) :: icondn                                        !< thermal conduction flag
       
      real(kind=wp), intent(inout) :: dt2t                                 !< time step parameter
      real(kind=wp), dimension(npropm,nel), intent(inout) :: pm            !< material properties array
      real(kind=wp), dimension(nel), intent(inout) :: x                    !< global coordinate array
      real(kind=wp), dimension(nel), intent(inout) :: a                    !< nodal acceleration array
      real(kind=wp), dimension(nel), intent(inout) :: v                    !< nodal velocity array
      real(kind=wp), dimension(nel), intent(inout) :: w                    !< nodal rotation velocity array
      real(kind=wp), dimension(6,nel), intent(inout) :: flux               !< heat flux array
      real(kind=wp), dimension(npropg,nel), intent(inout) :: geo           !< geometric properties array
      real(kind=wp), dimension(nel), intent(inout) :: flu1                 !< fluid properties array
      real(kind=wp), dimension(nel), intent(inout) :: tf                   !< time function array
      real(kind=wp), dimension(nel), intent(inout) :: bufmat               !< material buffer array
      real(kind=wp), dimension(nel), intent(inout) :: partsav              !< part save array
      real(kind=wp), dimension(nel), intent(inout) :: stifn                !< stiffness array
      real(kind=wp), dimension(nel), intent(inout) :: fsky                 !< skyline force array
      real(kind=wp), dimension(nel), intent(inout) :: f11                  !< force component f11
      real(kind=wp), dimension(nel), intent(inout) :: f21                  !< force component f21
      real(kind=wp), dimension(nel), intent(inout) :: f31                  !< force component f31
      real(kind=wp), dimension(nel), intent(inout) :: f12                  !< force component f12
      real(kind=wp), dimension(nel), intent(inout) :: f22                  !< force component f22
      real(kind=wp), dimension(nel), intent(inout) :: f32                  !< force component f32
      real(kind=wp), dimension(nel), intent(inout) :: f13                  !< force component f13
      real(kind=wp), dimension(nel), intent(inout) :: f23                  !< force component f23
      real(kind=wp), dimension(nel), intent(inout) :: f33                  !< force component f33
      real(kind=wp), dimension(nel), intent(inout) :: f14                  !< force component f14
      real(kind=wp), dimension(nel), intent(inout) :: f24                  !< force component f24
      real(kind=wp), dimension(nel), intent(inout) :: f34                  !< force component f34
      real(kind=wp), dimension(nel), intent(inout) :: f15                  !< force component f15
      real(kind=wp), dimension(nel), intent(inout) :: f25                  !< force component f25
      real(kind=wp), dimension(nel), intent(inout) :: f35                  !< force component f35
      real(kind=wp), dimension(nel), intent(inout) :: f16                  !< force component f16
      real(kind=wp), dimension(nel), intent(inout) :: f26                  !< force component f26
      real(kind=wp), dimension(nel), intent(inout) :: f36                  !< force component f36
      real(kind=wp), dimension(nel), intent(inout) :: gresav               !< global result save array
      real(kind=wp), dimension(nel), intent(inout) :: mssa                 !< mass array
      real(kind=wp), dimension(nel), intent(inout) :: dmels                !< element damage array
      real(kind=wp), dimension(nel), intent(inout) :: voln                 !< element volume array
      real(kind=wp), dimension(numnod), intent(inout) :: temp              !< temperature array
      real(kind=wp), dimension(ifthe), intent(inout) :: fthe               !< thermal force array
      real(kind=wp), dimension(lsky), intent(inout) :: fthesky             !< thermal force skyline array
      real(kind=wp), dimension(icondn), intent(inout) :: condn             !< thermal force array
      real(kind=wp), dimension(lsky), intent(inout) :: condnsky            !< thermal conduction skyline array
      real(kind=wp), dimension(nel,6), intent(inout) :: svis        !< viscous stress array
      real(kind=8), dimension(3,numnod), intent(in) :: xdp          !xdp is double prescision, kind = 8 is obligatoire

      type(ttable),dimension(nel), intent(inout) :: table
      type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
      type (nlocal_str_)  , target                    :: nloc_dmg 
      type(t_ale_connectivity), intent(in)            :: ale_connect
      type (mat_elem_) ,intent(inout) :: mat_elem
      type(dt_)  , intent(inout)                      :: dt
      type (glob_therm_) ,intent(inout)   :: glob_therm
      type (sensors_),intent(inout) :: sensors

!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      ! loop counters and flags
      integer :: i, j, lco, nf1, iflag, nuvar, current_layer, imat
      integer :: ilay, nlay, ir, is, it, ip, ibid, mx, l_pla, l_epsd
     
      
      ! element connectivity and property arrays
      integer :: mxt(mvsiz), ngl(mvsiz), ngeo(mvsiz), ibidon(1), ii(6)
      integer :: nc1(mvsiz), nc2(mvsiz), nc3(mvsiz), nc4(mvsiz)
      integer :: nc5(mvsiz), nc6(mvsiz), nc7(mvsiz), nc8(mvsiz)
! local computation variables
      real(kind=wp) :: c1, dti             !< material and time step constants
      real(kind=wp) :: mbid(1)             !< dummy variable

      ! deformation and kinematic variables
      real(kind=wp) :: vd2(mvsiz), dvol(mvsiz), deltax(mvsiz) !< velocity divergence, volume change, element length
      real(kind=wp) :: vis(mvsiz), qvis(mvsiz), cxx(mvsiz)    !< viscosity, viscous pressure, sound speed

      ! stress components
      real(kind=wp) :: s1(mvsiz), s2(mvsiz), s3(mvsiz)       !< stress components s11, s22, s33
      real(kind=wp) :: s4(mvsiz), s5(mvsiz), s6(mvsiz)       !< stress components s12, s13, s23

      ! strain rate components
      real(kind=wp) :: dxx(mvsiz), dyy(mvsiz), dzz(mvsiz)    !< strain rate components d11, d22, d33
      real(kind=wp) :: d4(mvsiz), d5(mvsiz), d6(mvsiz)       !< strain rate components d12, d13, d23

      ! jacobian matrix components
      real(kind=wp) :: jac1(mvsiz), jac2(mvsiz), jac3(mvsiz) !< jacobian matrix components j11, j12, j13
      real(kind=wp) :: jac4(mvsiz), jac5(mvsiz), jac6(mvsiz) !< jacobian matrix components j21, j22, j23

      ! velocity gradients and additional variables
      real(kind=wp) :: vdx(mvsiz), vdy(mvsiz), vdz(mvsiz)    !< velocity gradient components
      real(kind=wp) :: ssp_eq(mvsiz), aire(mvsiz)            !< equivalent sound speed, element area
! additional stress and kinematic variables
      real(kind=wp) :: sti(mvsiz), wxx(mvsiz), wyy(mvsiz), wzz(mvsiz), conde(mvsiz)
    
! material parameters
      real(kind=wp) :: muvoid(mvsiz)
      real(kind=wp) :: off(mvsiz), rhoo(mvsiz), offg(mvsiz)
      real(kind=wp) :: x1(mvsiz), x2(mvsiz), x3(mvsiz), x4(mvsiz)
      real(kind=wp) :: x5(mvsiz), x6(mvsiz)
      real(kind=wp) :: y1(mvsiz), y2(mvsiz), y3(mvsiz), y4(mvsiz)
      real(kind=wp) :: y5(mvsiz), y6(mvsiz)
      real(kind=wp) :: z1(mvsiz), z2(mvsiz), z3(mvsiz), z4(mvsiz)
      real(kind=wp) :: z5(mvsiz), z6(mvsiz)
      real(kind=wp) :: vx1(mvsiz), vx2(mvsiz), vx3(mvsiz), vx4(mvsiz)
      real(kind=wp) :: vx5(mvsiz), vx6(mvsiz)
      real(kind=wp) :: vy1(mvsiz), vy2(mvsiz), vy3(mvsiz), vy4(mvsiz)
      real(kind=wp) :: vy5(mvsiz), vy6(mvsiz)
      real(kind=wp) :: vz1(mvsiz), vz2(mvsiz), vz3(mvsiz), vz4(mvsiz)
      real(kind=wp) :: vz5(mvsiz), vz6(mvsiz)
      real(kind=wp) :: px1(mvsiz), px2(mvsiz), px3(mvsiz), px4(mvsiz)
      real(kind=wp) :: px1_sym(mvsiz), px2_sym(mvsiz), px3_sym(mvsiz), px4_antisym(mvsiz)
      real(kind=wp) :: px5(mvsiz), px6(mvsiz)
      real(kind=wp) :: py1(mvsiz), py2(mvsiz), py3(mvsiz), py4(mvsiz)
      real(kind=wp) :: py1_sym(mvsiz), py2_sym(mvsiz), py3_sym(mvsiz), py4_antisym(mvsiz)
      real(kind=wp) :: py5(mvsiz), py6(mvsiz)
      real(kind=wp) :: pz1(mvsiz), pz2(mvsiz), pz3(mvsiz), pz4(mvsiz)
      real(kind=wp) :: pz1_sym(mvsiz), pz2_sym(mvsiz), pz3_sym(mvsiz), pz4_antisym(mvsiz)
      real(kind=wp) :: pz5(mvsiz), pz6(mvsiz)
      real(kind=wp) :: px1h(mvsiz), px2h(mvsiz), px3h(mvsiz)
      real(kind=wp) :: py1h(mvsiz), py2h(mvsiz), py3h(mvsiz)
      real(kind=wp) :: pz1h(mvsiz), pz2h(mvsiz), pz3h(mvsiz)
      real(kind=wp) :: vgxa(mvsiz), vgya(mvsiz), vgza(mvsiz), vga2(mvsiz)
      real(kind=wp) :: xgxa(mvsiz), xgya(mvsiz), xgza(mvsiz)
      real(kind=wp) :: xgxya(mvsiz), xgyza(mvsiz), xgzxa(mvsiz)
      real(kind=wp) :: xgxa2(mvsiz), xgya2(mvsiz), xgza2(mvsiz)
      real(kind=wp) :: dxy(mvsiz), dyx(mvsiz)
      real(kind=wp) :: dyz(mvsiz), dzy(mvsiz)
      real(kind=wp) :: dzx(mvsiz), dxz(mvsiz), divde(mvsiz)
      real(kind=wp) :: r11(mvsiz), r12(mvsiz), r13(mvsiz)
      real(kind=wp) :: r21(mvsiz), r22(mvsiz), r23(mvsiz)
      real(kind=wp) :: r31(mvsiz), r32(mvsiz), r33(mvsiz), gama(mvsiz,6)
      real(kind=wp) :: sigym(mvsiz), g(mvsiz), nu(mvsiz), volg(mvsiz), sigy(mvsiz)
      real(kind=wp) :: b1122(mvsiz), b1221(mvsiz), b2212(mvsiz), b1121(mvsiz)
      real(kind=wp) :: b1122h(mvsiz), b1221h(mvsiz), b2212h(mvsiz), b1121h(mvsiz)
      real(kind=wp) :: b1x(mvsiz,2), b1y(mvsiz,2), b2x(mvsiz,2), b2y(mvsiz,2)
      real(kind=wp) :: b1xh(mvsiz,2), b1yh(mvsiz,2), b2xh(mvsiz,2), b2yh(mvsiz,2)
      real(kind=wp) :: dcxx(mvsiz), dcxy(mvsiz), dcxz(mvsiz), dcyx(mvsiz), dcyy(mvsiz)
      real(kind=wp) :: dcyz(mvsiz), dczx(mvsiz), dczy(mvsiz), dczz(mvsiz), dc4(mvsiz)
      real(kind=wp) :: dc5(mvsiz), dc6(mvsiz), vzl(mvsiz), jaci33(mvsiz)
      real(kind=wp) :: dhxx(mvsiz), dhxy(mvsiz), dhyx(mvsiz), dhyy(mvsiz), dhyz(mvsiz)
      real(kind=wp) :: dhzx(mvsiz), dhzy(mvsiz), dhzz(mvsiz), dh4(mvsiz), dhxz(mvsiz)
      real(kind=wp) :: dh5(mvsiz), dh6(mvsiz), eintm(mvsiz), ddhv(mvsiz), dd(mvsiz,6)
      real(kind=wp) :: sigzm(mvsiz), volm(mvsiz), usb(mvsiz), et(mvsiz)
      real(kind=wp) :: r1_free(mvsiz), r3_free(mvsiz), r4_free(mvsiz)
      real(kind=wp) :: stin(mvsiz), bid(mvsiz), dsv(mvsiz), alpha_e(mvsiz), llsh(mvsiz)
!c     
      integer :: pid, mtn0, ipthk, ippos, ipmat, nlymax, mid, ipang, ioffs
      integer :: nn_del, ipres, isctl
      integer :: mxt0(mvsiz), istab(mvsiz)

     
      real(kind=wp) ::  dir(mvsiz,2),sign(mvsiz,6),shf(mvsiz),zt,wt,offs(mvsiz),&
                         rx(mvsiz), ry(mvsiz), rz(mvsiz),nu1(mvsiz),fac(mvsiz),&
                         sx(mvsiz), sy(mvsiz), sz(mvsiz),&
                         tx(mvsiz), ty(mvsiz), tz(mvsiz),e0(mvsiz),&
                         n1x(mvsiz), n2x(mvsiz), n3x(mvsiz),&
                         n1y(mvsiz), n2y(mvsiz), n3y(mvsiz),&
                         n1z(mvsiz), n2z(mvsiz), n3z(mvsiz),&
                         n4x(mvsiz), n5x(mvsiz), n6x(mvsiz),&
                         n4y(mvsiz), n5y(mvsiz), n6y(mvsiz),&
                         n4z(mvsiz), n5z(mvsiz), n6z(mvsiz),amu(mvsiz),area(mvsiz),&
                         sigo(mvsiz,6)

      real(kind=wp) ::  them(mvsiz,6),tempel(mvsiz),die(mvsiz),conden(mvsiz),voldp(mvsiz)

      real(kind=wp), dimension(mvsiz) :: fheat



      integer :: inloc, l_nloc, sz_r1_free, sz_ix
      integer, dimension(6) :: ipos, inod

      real(kind=wp), dimension(:,:) ,allocatable :: var_reg
      real(kind=wp), dimension(:), pointer :: dnl

      real(kind=wp) :: sti_c(mvsiz),ll(mvsiz),fld(mvsiz),&
                cns2,fqmax,dn,facdp
!c-----
      type(g_bufel_) ,pointer :: gbuf
      type(l_bufel_) ,pointer :: lbuf     
!c-----------------------------------------------s
      real(kind=wp), dimension(9, 9) :: w_gauss, a_gauss

      
!C-----------------------------------------------
        w_gauss = reshape( [ &
        2.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        1.0_wp, 1.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.555555555555556_wp, 0.888888888888889_wp, 0.555555555555556_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.347854845137454_wp, 0.652145154862546_wp, 0.652145154862546_wp,&
        0.347854845137454_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.236926885056189_wp, 0.478628670499366_wp, 0.568888888888889_wp, &
        0.478628670499366_wp, 0.236926885056189_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.171324492379170_wp, 0.360761573048139_wp, 0.467913934572691_wp, &
        0.467913934572691_wp, 0.360761573048139_wp, 0.171324492379170_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.129484966168870_wp, 0.279705391489277_wp, 0.381830050505119_wp, &
        0.417959183673469_wp, 0.381830050505119_wp, 0.279705391489277_wp, &
        0.129484966168870_wp, 0.0_wp, 0.0_wp, &
        0.101228536290376_wp, 0.222381034453374_wp, 0.313706645877887_wp, &
        0.362683783378362_wp, 0.362683783378362_wp, 0.313706645877887_wp, &
        0.222381034453374_wp, 0.101228536290376_wp, 0.0_wp, &
        0.081274388361574_wp, 0.180648160694857_wp, 0.260610696402935_wp, &
        0.312347077040003_wp, 0.330239355001260_wp, 0.312347077040003_wp, & 
        0.260610696402935_wp, 0.180648160694857_wp, 0.081274388361574_wp ], &
        shape(w_gauss) )
      
        a_gauss = reshape( [ &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.577350269189626_wp, 0.577350269189626_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.774596669241483_wp, 0.0_wp, 0.774596669241483_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.861136311594053_wp, -0.339981043584856_wp, 0.339981043584856_wp, &
        0.861136311594053_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.906179845938664_wp, -0.538469310105683_wp, 0.0_wp, &
        0.538469310105683_wp, 0.906179845938664_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.932469514203152_wp, -0.661209386466265_wp, -0.238619186083197_wp, &
        0.238619186083197_wp, 0.661209386466265_wp, 0.932469514203152_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, &
        -0.949107912342759_wp, -0.741531185599394_wp, -0.405845151377397_wp, &
        0.0_wp, 0.405845151377397_wp, 0.741531185599394_wp, &
        0.949107912342759_wp, 0.0_wp, 0.0_wp, &
        -0.960289856497536_wp, -0.796666477413627_wp, -0.525532409916329_wp, &
        -0.183434642495650_wp, 0.183434642495650_wp, 0.525532409916329_wp, &
        0.796666477413627_wp, 0.960289856497536_wp, 0.0_wp, &
        -0.968160239507626_wp, -0.836031107326636_wp, -0.613371432700590_wp, &
        -0.324253423403809_wp, 0.0_wp, 0.324253423403809_wp, &
        0.613371432700590_wp, 0.836031107326636_wp, 0.968160239507626_wp ], &
        shape(A_GAUSS) )
!c=======================================================================
!c   s o u r c e  l i n e s
!c=======================================================================
    
      mtn0 = 0
      nlymax= 0
      ipang = 0
      ipthk = 0 
      ippos = 0                                 
      ipmat = 0 
      
      !flag for print
    
    
      gbuf => elbuf_tab(ng)%gbuf
      nlay = elbuf_tab(ng)%nlay

      nlay = 1
      ilay = 1

      ir = 1
      is = 1
      it = 1

      do i=1,6
        ii(i) = nel*(i-1)
      enddo

      do i=1,nel
        sigy(i)   = ep20
        sigo(i,1) = gbuf%sig(ii(1)+i)
        sigo(i,2) = gbuf%sig(ii(2)+i)
        sigo(i,3) = gbuf%sig(ii(3)+i)
        sigo(i,4) = gbuf%sig(ii(4)+i)
        sigo(i,5) = gbuf%sig(ii(5)+i)
        sigo(i,6) = gbuf%sig(ii(6)+i)
      enddo

      inloc = iparg(78,ng)
      allocate(var_reg(nel,nlay))
      sz_r1_free=nel
      sz_ix=numelq+numels+nsvois
!c-----------
      ibid = 0
      ibidon(1) = 0

      if (igtyp /= 22) then
        isorthg = 0
      end if 



!c-----------
      nf1=nft+1
!c--------------------------
!c-----------
 !Keep this subroutine for orthotropic materials development in the future
       if (isorth > 0) then
         call sgparav3(&
        6,         x,         ixs(1,nf1),rx,&
        ry,        rz,        sx,        sy,&
        sz,        tx,        ty,        tz,&
        nel)
       endif
!c-----------------------------------------------------------
!c gather nodal variables and compute intinsic rotations
!c-----------------------------------------------------------


        call s6zrcoor3(x,ixs(1,nf1),v,gbuf%gama,gama,&
        x1, x2, x3, x4, x5, x6,&
        y1, y2, y3, y4, y5, y6, &
        z1, z2, z3, z4, z5, z6, &
        vx1, vx2, vx3, vx4, vx5, vx6, &
        vy1, vy2, vy3, vy4, vy5, vy6, &
        vz1, vz2, vz3, vz4, vz5, vz6, &
        vd2,vis,gbuf%off,offg,gbuf%smstr,gbuf%rho,rhoo,&
        r11, r12, r13, r21, r22, r23, r31, r32, r33, &
        nc1,nc2,nc3,nc4,nc5,nc6,ngl,mxt,ngeo,&
        ioutprt, vgxa, vgya, vgza, vga2,&
        nel, xgxa, xgya, xgza,xgxa2,xgya2,xgza2,&
        xgxya,xgyza,xgzxa,iparg(1,ng),gbuf%gama_r,&
        nixs,irep,ismstr,isorth,jlag) 

   

      
!c
!c
      nn_del = 0
      pid = ngeo(1)
      if (geo(190,pid)+geo(191,pid)+geo(192,pid)+geo(192,pid)>zero) then 
              nn_del=6
      endif
      if (nn_del ==0 .and. dt%idel_brick>0) nn_del=6
      mx = mxt(1)
      c1 =pm(32,mx)
      ipres = mat_elem%mat_param(mx)%ipres
      ipres = 0  ! for fanshi : remove constant szz treatment for thick-shell
      isctl = igeo(97,pid)
  !
      do i=1,nel
        sigzm(i) = zero
        volm(i) = zero
        nu(i)=min(half,pm(21,mx))
        e0(i) =three*(one-two*nu(i))*c1
        usb(i)=em01/c1
        stin(i)=zero
        conden(i)= zero
      enddo
!c

     !Keep this subroutine for imcompressible development in the future
      if (icp==1) then                                
        do i=1,nel                                    
         nu1(i)=half                                  
        enddo                                           
      elseif (icp==2) then                            
        call s8csigp3(gbuf%sig,e0 ,gbuf%pla,fac,gbuf%g_pla,nel)
        do i=1,nel                                    
          nu1(i)=nu(i)+(half-nu(i))*fac(i)             
        enddo                                           
      else                                             
         do i=1,nel                                    
          nu1(i) =nu(i)                                  
         enddo                                           
      endif                                             
!c
      call s6zderi3(&
        offg,      voln,      ngl,       x1,&
        x2,        x3,        x4,        x5,&
        x6,        y1,        y2,        y3,&
        y4,        y5,        y6,        z1,&
        z2,        z3,        z4,        z5,&
        z6,        px1,       px2,       px3,&
        px4,       py1,       py2,       py3,&
        py4,       pz1,       pz2,       pz3,&
        pz4,       jac1,      jac2,&
        jac3,      jac4,      jac5,      jac6,&
        vzl,       volg,      gbuf%smstr,&
        gbuf%off,  nel,       ismstr,&
        px5,     py5,     pz5,&
        px6,     py6,     pz6,&
        idel7nok,  ineg_v,    mstop,    volmin,    &
        idtmin  )  

        do i=1,nel                                        

          px1_sym(i)=half*(px1(i)+px4(i))             
          py1_sym(i)=half*(py1(i)+py4(i))  
          pz1_sym(i)=half*(pz1(i)+pz4(i))  
  
          px2_sym(i)=half*(px2(i)+px5(i))             
          py2_sym(i)=half*(py2(i)+py5(i))  
          pz2_sym(i)=half*(pz2(i)+pz5(i))  
  
          px3_sym(i)=half*(px3(i)+px6(i))             
          py3_sym(i)=half*(py3(i)+py6(i))  
          pz3_sym(i)=half*(pz3(i)+pz6(i))  
  
          px4_antisym(i)=half*(px4(i)-px1(i))             
          py4_antisym(i)=half*(py4(i)-py1(i))  
          pz4_antisym(i)=half*(pz4(i)-pz1(i))  
  
        enddo  


      call sdlen3(&
         volg,    deltax,  x1,      x2,&
        x5,      x4,      x3,      x3,&
        x6,      x6,      y1,      y2,&
        y5,      y4,      y3,      y3,&
        y6,      y6,      z1,      z2,&
        z5,      z4,      z3,      z3,&
        z6,      z6,      n1x,     n2x,&
        n3x,     n4x,     n5x,     n6x,&
        n1y,     n2y,     n3y,     n4y,&
        n5y,     n6y,     n1z,     n2z,&
        n3z,     n4z,     n5z,     n6z,&
        nel,     mtn,     jale,    jeul)
      call s6zdefc3(&
        px1,     px2,     px3,     px4,&
        py1,     py2,     py3,     py4,&
        pz1,     pz2,     pz3,     pz4,&
        vx1,     vx2,     vx3,     vx4,&
        vx5,     vx6,     vy1,     vy2,&
        vy3,     vy4,     vy5,     vy6,&
        vz1,     vz2,     vz3,     vz4,&
        vz5,     vz6,     dcxx,    dcxy,&
        dcxz,    dcyx,    dcyy,    dcyz,&
        dczx,    dczy,    dczz,    &
        wxx,     wyy,&
        wzz,   &
       nel,&
        px5,     py5,     pz5,&
        px6,     py6,     pz6 )     



      call s6czero3(&
        f11,        f21,        f31,        f12,&
        f22,        f32,        f13,        f23,&
        f33,        f14,        f24,        f34,&
        f15,        f25,        f35,        f16,&
        f26,        f36,        gbuf%sig,   gbuf%eint,&
        gbuf%rho,   gbuf%qvis,  gbuf%pla,   gbuf%epsd,&
        gbuf%g_pla, gbuf%g_epsd,nel,nlay)
!c ------------------------------------------------------------------------------
!c  update reference configuration (possible future change to small strain option)
!c -------------------------------------------------------------------------------
      if (ismstr <= 3.or.(ismstr==4.and.jlag>0)) then
       call s6sav3(&
       gbuf%off,  gbuf%smstr,x1,        x2,&
       x3,        x4,        x5,        x6,&
       y1,        y2,        y3,        y4,&
       y5,        y6,        z1,        z2,&
       z3,        z4,        z5,        z6,&
       nel)
      end if !(ismstr <= 3) then
!c
   
!c!
!c---------------------------------------------------------
!c compute non-local variable increment at each gauss point 
!c---------------------------------------------------------
      if (inloc > 0) then  
        l_nloc = nloc_dmg%l_nloc
        dnl => nloc_dmg%dnl(1:l_nloc) ! dnl = non local variable increment 
          do i=1,nel
            inod(1) = nloc_dmg%idxi(nc1(i))
            inod(2) = nloc_dmg%idxi(nc2(i))
            inod(3) = nloc_dmg%idxi(nc3(i))
            inod(4) = nloc_dmg%idxi(nc4(i))
            inod(5) = nloc_dmg%idxi(nc5(i))
            inod(6) = nloc_dmg%idxi(nc6(i))
            do j = 1,6
              ipos(j) = nloc_dmg%posi(inod(j))+ilay-1
            enddo
            var_reg(i,ilay) = dnl(ipos(1)) + dnl(ipos(2)) + dnl(ipos(3)) + &
                            dnl(ipos(4)) + dnl(ipos(5)) + dnl(ipos(6))
            var_reg(i,ilay) = var_reg(i,ilay)*one_over_6
          enddo  
      endif 
!c---------------------------------------------------------
!c                                       
!c--------------------------------------
!c constant stress through the thickness
!c--------------------------------------
 
        lbuf => elbuf_tab(ng)%bufly(ilay)%lbuf(ir,is,it)

        do i=1,nel
          sigzm(i) = sigzm(i)+lbuf%vol(i)*lbuf%sig(i+2*nel)
          volm(i)  = volm(i) +lbuf%vol(i)
        enddo
      if (dt1 == zero) then
        dti =zero
      else
        dti = one/dt1
      endif 
!c-------------------------------------------
!c element temperature
!c-------------------------------------------
      tempel(:) = zero
      fheat(:)  = zero
      if (jthe < 0) then       
        do i=1,nel
          tempel(i) = one_over_6 *(temp(nc1(i)) + temp(nc2(i))  &
                                + temp(nc3(i)) + temp(nc4(i)) &
                                + temp(nc5(i)) + temp(nc6(i)))
          gbuf%temp(i) = tempel(i)
        enddo
      endif
      ioffs=0
      do i=1,nel
        offs(i)  = ep20
      enddo
      if (jthe < 0) them(1:nel,1:6) = zero
!c---------------------------------------------
!c loop on integration points through thickness
!c---------------------------------------------
      
        lbuf => elbuf_tab(ng)%bufly(ilay)%lbuf(ir,is,it)

          zt = 0.
          wt = 2.
 
     !c
            call s6zdefo3(&
            dxx,        dxy,        dxz,        dyx,&
            dyy,        dyz,        dzx,        dzy,&
            dzz,         d4,         d5,         d6,&
           dcxx,       dcxy,       dcxz,       dcyx,&
           dcyy,       dcyz,       dczx,       dczy,&
           dczz,                                    &            
             zt,         wt,        vzl,       voln,&
           volg,                                    &       
            off,        offg,  gbuf%off,            &
          voldp,                                    &
            nel,         dt1,   ismdisp, iscau )
        do i=1,nel
          rhoo(i)= lbuf%rho(i)
        enddo
        if (isorth > 0) then
      
          call scordef3(nel,dxx,dyy,dzz,d4,d5,d6,dir)

        endif
!c 
        divde(1:nel) = dt1*(dxx(1:nel)+ dyy(1:nel)+ dzz(1:nel))+dsv(1:nel)  
        call srho3(&
        pm,         lbuf%vol,   lbuf%rho,   lbuf%eint,&
        divde,      flux(1,nf1),flu1(nf1),  voln,&
        dvol,       ngl,        mxt,        off,&
        0,          gbuf%tag22, voldp,      lbuf%vol0dp,&
        amu,        gbuf%off,   nel,        mtn,&
        jale,       ismstr,     jeul,       jlag)
!c
!c-----------------------------
!c gather stresses
!c-----------------------------
        call csmall3(lbuf%sig,s1,s2,s3,s4,s5,s6,&
                   gbuf%off,off,nel)
!c------------------------------------------------------
!c compute new stresses according to constitutive laws
!c------------------------------------------------------
        current_layer=ilay         ! one treatment pass lay to negative



        call mmain(timers, output,&
        elbuf_tab,   ng,          pm,          geo,&
                     ale_connect, ixs,         iparg,&
        v,           tf,          npf,         bufmat,&
        sti,         x,           dt2t,        neltst,&
        ityptst,     offset,      nel,         w,&
        off,         ngeo,        mxt,         ngl,&
        voln,        vd2,         dvol,        deltax,&
        vis,         qvis,        cxx,         s1,&
        s2,          s3,          s4,          s5,&
        s6,          dxx,         dyy,         dzz,&
        d4,          d5,          d6,          wxx,&
        wyy,         wzz,         jac1,        jac2,&
        jac3,        jac4,        jac5,        jac6,&
        vdx,         vdy,         vdz,         muvoid,&
        ssp_eq,      aire,        sigy,        et,&
        r1_free,     lbuf%pla,    r3_free,     amu,&
        dxx,         dxy,         dxz,         dyx,&
        dyy,         dyz,         dzx,         dzy,&
        dzz,         ipm,         gama,        bid,&
        bid,         bid,         bid,         bid,&
        bid,         bid,         istrain,     tempel,&
        die,         iexpan,      current_layer,mssa,&
        dmels,       ir,          is,          it,&
        table,       bid,         bid,         bid,&
        bid,         iparg(1,ng), igeo,        conde,&
        itask,       nloc_dmg,    var_reg(1,ilay),mat_elem,&
        h3d_strain,  jplasol,     jsph,        sz_r1_free,&
        snpc,        stf,         sbufmat      ,glob_therm, &
        svis,        sz_ix,       iresp,       &
        n2d,         th_strain,   ngroup,      tt, &
        dt1,         ntable,      numelq,      nummat,&
        numgeo,      numnod,      numels,&
        idel7nok,    idtmin,      maxfunc,&
        imon_mat,    userl_avail, impl_s,&
        idyna,       dt,         fheat  ,sensors, opt_mtn=mtn,opt_jcvt=jcvt, &
        opt_isorth=isorth,opt_isorthg=isorthg)
!c
        do i=1,nel
          stin(i) = stin(i)+sti(i)
        enddo



  do i=1,nel
    sign(i,1) = gbuf%sig(ii(1)+i)
    sign(i,2) = gbuf%sig(ii(2)+i)
    sign(i,3) = gbuf%sig(ii(3)+i)
    sign(i,4) = gbuf%sig(ii(4)+i)
    sign(i,5) = gbuf%sig(ii(5)+i)
    sign(i,6) = gbuf%sig(ii(6)+i)
  enddo
!      ENDIF
!c
        if(glob_therm%nodadt_therm == 1) then
          do i=1,nel
            conden(i)= conden(i)+ conde(i)
          enddo
        endif
        if (istrain == 1) then 
          call sstra3(&
        dxx,      dyy,      dzz,      d4,&
        d5,       d6,       lbuf%stra,wxx,&
        wyy,      wzz,      off,      nel,&
        jcvt)
        endif
!c----------------------------
!c internal forces
!c----------------------------
        l_pla  = elbuf_tab(ng)%bufly(ilay)%l_pla
        l_epsd = elbuf_tab(ng)%bufly(ilay)%l_epsd
        if (isorth > 0) then
         call scroto_sig(nel,lbuf%sig,sign,dir)
!! scroto() temporary replaced by (the same) scroto_sig() in order to do not affect
!! the other multidimensional buffer arrays which are still not modified
         call s6cfint3(&
        sign,       px1,        px2,        px3,&
        px4,        py1,        py2,        py3,&
        py4,        pz1,        pz2,        pz3,&
        pz4,        px1h,       px2h,       px3h,&
        py1h,       py2h,       py3h,       pz1h,&
        pz2h,       pz3h,       jaci33,     b1x,&
        b1y,        b2y,        b2x,        b1122,&
        b1221,      b2212,      b1121,      b1xh,&
        b1yh,       b2xh,       b2yh,       b1122h,&
        b1221h,     b2212h,     b1121h,     f11,&
        f21,        f31,        f12,        f22,&
        f32,        f13,        f23,        f33,&
        f14,        f24,        f34,        f15,&
        f25,        f35,        f16,        f26,&
        f36,        voln,       qvis,       lbuf%eint,&
        lbuf%rho,   lbuf%qvis,  lbuf%pla,   lbuf%epsd,&
        gbuf%epsd,  gbuf%sig,   gbuf%eint,  gbuf%rho,&
        gbuf%qvis,  gbuf%pla,   zt,         wt,&
        volg,       off,        nu1,        lbuf%vol,&
        gbuf%vol,   l_pla,      l_epsd,     nel,&
        svis,       gbuf%wpla,  lbuf%wpla,   gbuf%g_wpla,&
        nlay    )
        else
         call s6zfint3(&
        lbuf%sig,          px1,               px2,               px3,&
        px4,               py1,               py2,               py3,&
        py4,               pz1,               pz2,               pz3,&
        pz4,                                                         &
                                                                 f11,& 
        f21,               f31,               f12,               f22,&
        f32,               f13,               f23,               f33,&
        f14,               f24,               f34,               f15,&
        f25,               f35,               f16,               f26,&
        f36,               voln,              qvis,              lbuf%eint,&
        lbuf%rho,          lbuf%qvis,         lbuf%pla,          lbuf%epsd,&
        gbuf%epsd,         gbuf%sig,          gbuf%eint,         gbuf%rho,&
        gbuf%qvis,         gbuf%pla,           &
        volg,              off,               lbuf%vol,&
        gbuf%vol,          l_pla,             l_epsd,            nel,&
        svis , nlay,&
        px5, py5, pz5,              &
        px6, py6, pz6)

 

        endif ! if (isorth > 0)
!c-------------------------
!c finite element heat transfert  
!c--------------------------
        if (jthe < 0) then
          imat = mxt(1)
          if (mat_elem%mat_param(imat)%heat_flag == 1) then
            call s6ctherm(&
                pm       ,imat    ,voln     ,nc1      ,&
                nc2      ,nc3     ,nc4      ,nc5      ,&
                nc6      ,px1_sym     ,px2_sym      ,px3_sym      ,&
                px4_antisym      ,py1_sym     ,py2_sym      ,py3_sym      ,&
                py4_antisym      ,pz1_sym     ,pz2_sym      ,pz3_sym      ,&
                pz4_antisym      ,dt1     ,temp     ,tempel   ,&
                fheat    ,them    ,gbuf%off ,lbuf%off ,&
                nel      ,glob_therm%theaccfact)
          else
          end if
            call s6ctherm(&
                pm       ,imat    ,voln     ,nc1      ,&
                nc2      ,nc3     ,nc4      ,nc5      ,&
                nc6      ,px1_sym     ,px2_sym      ,px3_sym      ,&
                px4_antisym      ,py1_sym     ,py2_sym      ,py3_sym      ,&
                py4_antisym      ,pz1_sym     ,pz2_sym      ,pz3_sym      ,&
                pz4_antisym      ,dt1     ,temp     ,tempel   ,&
                die      ,them    ,gbuf%off ,lbuf%off ,&
                nel      ,glob_therm%theaccfact)
        endif 
        do i=1,nel                                        
          offg(i)=min(offg(i),off(i))                        
          if (lbuf%off(i) > one .and. gbuf%off(i) == one) then
            offs(i) = min(lbuf%off(i),offs(i))
            ioffs   = 1                                         
          end if                                             
        enddo                                               

!c-----------------------------
!c
!c-------------------------------
!c non-local specific computation
!c-------------------------------
      if (inloc > 0) then 
       ! computation of thickshell area
       call sdlensh3n(volg,llsh,area , &
                       x1, x2, x3, x4, x5, x6,&
                       y1, y2, y3, y4, y5, y6,&
                       z1, z2, z3, z4, z5, z6,nel)
       ! non-local internal forces 
                       call s6cfint_reg(&
                       nloc_dmg ,var_reg  ,nel     ,off     ,&
                       volg     ,nc1      ,nc2     ,nc3     ,&
                       nc4      ,nc5      ,nc6     ,px1_sym     ,&
                       px2_sym      ,px3_sym      ,px4_antisym     ,py1_sym     ,&
                       py2_sym      ,py3_sym      ,py4_antisym     ,pz1_sym     ,&
                       pz2_sym      ,pz3_sym      ,pz4_antisym     ,mxt(1),&
                       itask    ,dt2t     ,gbuf%vol,nft     ,&
                       nlay     ,w_gauss  ,a_gauss ,area    ,&
                       elbuf_tab(ng)%nlocts(1,1))
      endif
!c--------------------------
!c
      if (ioffs == 1) then
       do i=1,nel
         if (offs(i)<=two)gbuf%off(i) = offs(i)
       end do
    
         lbuf => elbuf_tab(ng)%bufly(ilay)%lbuf(ir,is,it)

         do i=1,nel
           if (gbuf%off(i) > one) lbuf%off(i)=gbuf%off(i)
         end do
   
      end if



      if ( nn_del> 0) then
        call sdlensh3n2(volg,llsh,area , &
                       x1, x2, x3, x4, x5, x6,&
                       y1, y2, y3, y4, y5, y6,&
                       z1, z2, z3, z4, z5, z6, nel)
        call tshgeodel3(ngl,gbuf%off,volg,area,gbuf%vol,&
                       llsh,geo(1,pid),nn_del,dt,nel )
      endif
!c-----------------------------
!c hourglass
!c-----------------------------
      if ( impl_s == 0) then
        if (isctl > 0) then

          dn = geo(13,pid)
          call s6chour_ctl(&
           x1,  x2,  x3,  x4,  x5,  x6, &
           y1,  y2,  y3,  y4,  y5,  y6, &
           z1,  z2,  z3,  z4,  z5,  z6, &
          vx1, vx2, vx3, vx4, vx5, vx6,&
          vy1, vy2, vy3, vy4, vy5, vy6,&
          vz1, vz2, vz3, vz4, vz5, vz6,&
          f11, f12, f13, f14, f15, f16,&
          f21, f22, f23, f24, f25, f26,&
          f31, f32, f33, f34, f35, f36,&
          pm,npropm, nummat,mtn,mxt,dn,&
          gbuf%rho,volg,cxx,gbuf%hourg,&
          off,gbuf%vol,gbuf%eint,dt1,stin,nel)
        else
      call s6zhour3(&
    pm, npropm, gbuf%rho,volg,cxx,&
    x1, x2, x3, x3, x4, x5, x6, x6,&
    y1, y2, y3, y3, y4, y5, y6, y6,&
    z1, z2, z3, z3, z4, z5, z6, z6,&
    vx1, vx2, vx3, vx3, vx4, vx5, vx6, vx6,&
    vy1, vy2, vy3, vy3, vy4, vy5, vy6, vy6,    &
    vz1, vz2, vz3, vz3, vz4, vz5, vz6, vz6,&
    f11,f12,f13,f14,f15,f16,&
    f21,f22,f23,f24,f25,f26,     &
    f31,f32,f33,f34,f35,f36,&
    nu,gbuf%hourg,off,gbuf%vol,gbuf%eint,nel,&
    mxt,npropg,geo,ngeo,&
    dt1,elbuf_tab(ng), iint ,jlag,mtn,& 
    sigy, sign,  sigo) 
     
        end if !(isctl > 0) then
      endif
!c-----------------------------
!c small strain
!c-----------------------------
      call smallb3(   gbuf%off,offg,    nel,     ismstr)
!c--------------------------------------
!c balance per part in case of print out
!c--------------------------------------
      iflag=mod(ncycle,ncpri)
      if (ioutprt>0) then         
           call s6cbilan(partsav,gbuf%eint,gbuf%rho,gbuf%rk ,gbuf%vol,&
            vgxa, vgya, vgza, vga2, volg,iparts,&
            gresav,grth,igrth,gbuf%off,iexpan,gbuf%eintth,&
            gbuf%fill, xgxa, xgya, xgza,xgxa2,xgya2,xgza2,&
            xgxya,xgyza,xgzxa,itask,iparg(1,ng),gbuf%off,&
            sensors,nel,gbuf%g_wpla,gbuf%wpla)
      endif
!c--------------------------------
!c convected frame to global frame
!c--------------------------------
!
      call s6zrrota3(&
        r11,     r21,     r31,     r12,&
        r22,     r32,     r13,     r23,&
        r33,     &
        f11,     f12,     f13,&
        f14,     f15,     f16,&
        f21,     f22,     f23,&
        f24,     f25,     f26,&
        f31,     f32,     f33,&
        f34,     f35,     f36,&
        nel)


!c----------------------------
!c     distortion control  
!c----------------------------
      if (isctl > 0) then
         alpha_e(1:nel) = one  
         call sdistor_ini(                                      &
                     nel      ,sti_c    ,npropm     ,nummat  ,       &
                     ismstr   ,mxt      ,istab      ,pm      ,       &
                     gbuf%sig ,gbuf%rho ,cxx        ,offg    ,       &
                     gbuf%off ,ll       ,voln       ,fld     ,       &
                     cns2     ,fqmax    )
!      all in global system 
        call s6get_xv(                                 &
                      nc1,      nc2,      nc3,         &
                      nc4,      nc5,      nc6,         &
                       x1,       x2,       x3,         &
                       x4,       x5,       x6,         &
                       y1,       y2,       y3,         &
                       y4,       y5,       y6,         &
                       z1,       z2,       z3,         &
                       z4,       z5,       z6,         &
                      vx1,      vx2,      vx3,         &
                      vx4,      vx5,      vx6,         &
                      vy1,      vy2,      vy3,         &
                      vy4,      vy5,      vy6,         &
                      vz1,      vz2,      vz3,         &
                      vz4,      vz5,      vz6,         &
                        x,      xdp,        v,         &
                   numnod,   ismstr,     nel )
         call s6for_distor(&
                  x1,       x2,       x3,        &
                  x4,       x5,       x6,        &
                  y1,       y2,       y3,        &
                  y4,       y5,       y6,        &
                  z1,       z2,       z3,        &
                  z4,       z5,       z6,        &
                 vx1,      vx2,       x3,        &
                 vx4,      vx5,       x6,        &
                 vy1,      vy2,       y3,        &
                 vy4,      vy5,       y6,        &
                 vz1,      vz2,       z3,        &
                 vz4,      vz5,       z6,        &
                 f11,      f12,      f13,       &
                 f14,      f15,      f16,       &
                 f21,      f22,      f23,       &
                 f24,      f25,      f26,       &
                 f31,      f32,      f33,       &
                 f34,      f35,      f36,       &
                stin,    sti_c,      fld,      &
                cns2,      ll ,    istab,   &
               fqmax,gbuf%eint_distor,dt1,   &
                nel )
      endif
!c----------------------------
      if(nfilsol/=0) call s6fillopt(&
        gbuf%fill,sti,      f11,      f21,&
        f31,      f12,      f22,      f32,&
        f13,      f23,      f33,      f14,&
        f24,      f34,      f15,      f25,&
        f35,      f16,      f26,      f36,&
        nel)
!c----------------------------
!c assemble nodal forces
!c----------------------------
      if (iparit == 0) then
        call s6cumu3(&
        gbuf%off,a,       nc1,     nc2,&
        nc3,     nc4,     nc5,     nc6,&
        stifn,   stin,    f11,     f21,&
        f31,     f12,     f22,     f32,&
        f13,     f23,     f33,     f14,&
        f24,     f34,     f15,     f25,&
        f35,     f16,     f26,     f36,&
        nel,    jthe,    fthe,    them,&
        condn,conden,   ifthe,  icondn,&
        glob_therm%nodadt_therm)
      else
        call s6cumu3p(&
        gbuf%off,stin,    fsky,    fsky,&
        iads,    f11,     f21,     f31,&
        f12,     f22,     f32,     f13,&
        f23,     f33,     f14,     f24,&
        f34,     f15,     f25,     f35,&
        f16,     f26,     f36,     nel,&
        nft,    jthe, fthesky,    them,&
        condnsky,conden,&
        glob_therm%nodadt_therm)
      endif


!c-----------
!      pause 
      return

      end subroutine s6zforc3
      end module s6zforc3_mod
