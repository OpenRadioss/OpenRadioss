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
!||    s6zforc3_mod   ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- called by ------------------------------------------------------
!||    forint         ../engine/source/elements/forint.F
!||====================================================================
      module s6zforc3_mod
      contains
      ! ======================================================================================================================
      ! \brief /PENTA6 solid elements
      ! \details 6 nodes PENTA6 solid elements
      ! ======================================================================================================================
!||====================================================================
!||    s6zforc3                ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- called by ------------------------------------------------------
!||    forint                  ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    csmall3                 ../engine/source/elements/solid/solide/csmall3.F
!||    mmain                   ../engine/source/materials/mat_share/mmain.F90
!||    s6cbilan                ../engine/source/elements/thickshell/solide6c/s6cbilan.F
!||    s6chour_ctl             ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6cumu3                 ../engine/source/elements/thickshell/solide6c/s6cumu3.F
!||    s6cumu3p                ../engine/source/elements/thickshell/solide6c/s6cumu3p.F
!||    s6czero3                ../engine/source/elements/thickshell/solide6c/s6czero3.F
!||    s6fillopt               ../engine/source/elements/thickshell/solide6c/s6fillopt.F
!||    s6fint_reg              ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||    s6for_distor            ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv                ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    s6sav3                  ../engine/source/elements/thickshell/solide6c/s6sav3.F
!||    s6zdefc3                ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||    s6zdefo3                ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||    s6zderi3                ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zfint3                ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||    s6zhour3                ../engine/source/elements/solid/solide6z/s6zhourg3.F90
!||    s6zrcoor3               ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||    s6zrrota3               ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||    sdistor_ini             ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sdlen3                  ../engine/source/elements/solid/solide/sdlen3.F
!||    smallb3                 ../engine/source/elements/solid/solide/smallb3.F
!||    srho3                   ../engine/source/elements/solid/solide/srho3.F
!||    sstra3                  ../engine/source/elements/solid/solide/sstra3.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod    ../common_source/modules/ale/ale_connectivity_mod.F
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    dt_mod                  ../engine/source/modules/dt_mod.F
!||    elbufdef_mod            ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    eos_param_mod           ../common_source/modules/mat_elem/eos_param_mod.F90
!||    glob_therm_mod          ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod            ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mmain_mod               ../engine/source/materials/mat_share/mmain.F90
!||    mvsiz_mod               ../engine/share/spe_inc/mvsiz_mod.F90
!||    names_and_titles_mod    ../common_source/modules/names_and_titles_mod.F
!||    nlocal_reg_mod          ../common_source/modules/nlocal_reg_mod.F
!||    output_mod              ../common_source/modules/output/output_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    s6chour_ctl_mod         ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6fint_reg_mod          ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||    s6for_distor_mod        ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv_mod            ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    s6zdefc3_mod            ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||    s6zdefo3_mod            ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||    s6zderi3_mod            ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zfint3_mod            ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||    s6zhour3_mod            ../engine/source/elements/solid/solide6z/s6zhourg3.F90
!||    s6zrcoor3_mod           ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||    s6zrrota3_mod           ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||    sdistor_ini_mod         ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sensor_mod              ../common_source/modules/sensor_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||    table_mod               ../engine/share/modules/table_mod.F
!||    timer_mod               ../engine/source/system/timer_mod.F90
!||====================================================================
      subroutine s6zforc3(                                                     &
        timers   ,output   ,ngroup   ,elbuf_tab,npropm   ,nummat   ,pm       , &
        ng       ,npropg   ,numgeo   ,geo      ,nixs     ,numels   ,numelq   , &
        nsvois   ,ixs      ,numnod   ,x        ,a        ,v        ,           &
        w        ,flux     ,flu1     ,ale_connect,nparg  ,iparg    ,           &
        stf      ,tf       ,snpc     ,npf      ,sbufmat  ,bufmat   ,npsav    , &
        npart    ,partsav  ,dt2t     ,neltst   ,ityptst  ,stifn    ,lsky     , &
        fsky     ,iads     ,offset   ,nel      ,iparts   ,                     &
        f11      ,f21      ,f31      ,f12      ,f22      ,f32      ,           &
        f13      ,f23      ,f33      ,f14      ,f24      ,f34      ,           &
        f15      ,f25      ,f35      ,f16      ,f26      ,f36      ,           &
        nloc_dmg ,npropmi  ,ipm      ,istrain  ,npropgi  ,igeo     ,ngpe     , &
        nthread  ,gresav   ,ngrth    ,nelem    ,grth     ,igrth    ,ntable   , &
        table    ,mssa     ,dmels    ,voln     ,itask    ,ioutprt  ,mat_elem , &
        h3d_strain,ifthe   ,fthe     ,fthesky  ,icondn   ,condn    ,condnsky , &
        iexpan   ,dt       ,svis     ,iresp    ,idel7nok ,maxfunc  ,imon_mat , &
        userl_avail,glob_therm,xdp   ,sensors  ,dt1      ,volmin   ,th_strain, & 
        idtmin   ,tt       ,idyna    ,impl_s   ,ineg_v   ,iparit   ,irep     , &
        iscau    ,ismdisp  ,ismstr   ,isorth   ,isorthg  ,jale     ,jcvt     , &
        jeul     ,jlag     ,jsph     ,jplasol  ,jthe     ,mstop    ,mtn      , & 
        n2d      ,ncpri    ,ncycle   ,nfilsol  ,nft      ,iint     ,nodadt   , &
        dtfac1   )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use timer_mod
      use mmain_mod
      use table_mod
      use mat_elem_mod            
      use nlocal_reg_mod
      use ale_connectivity_mod
      use dt_mod
      use elbufdef_mod
      use glob_therm_mod
      use sensor_mod
      use s6zrrota3_mod
      use s6zfint3_mod
      use s6zdefo3_mod
      use s6zdefc3_mod
      use s6zderi3_mod
      use s6zrcoor3_mod
      use s6zhour3_mod
      use s6fint_reg_mod
      use matparam_def_mod
      use names_and_titles_mod
      use constant_mod
      use eos_param_mod 
      use table_mat_vinterp_mod
      use mvsiz_mod       ,only : mvsiz
      use precision_mod   ,only : wp  
      use sdistor_ini_mod ,only : sdistor_ini
      use s6get_xv_mod    ,only : s6get_xv
      use s6for_distor_mod,only : s6for_distor
      use s6chour_ctl_mod ,only : s6chour_ctl
      use output_mod      ,only : output_     
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
      type(timer_) ,                      intent(inout) :: timers
      type(output_),                      intent(inout) :: output
      integer,                            intent(in)    :: ngroup    !< Number of element groups                               
      type(elbuf_struct_), target, dimension(ngroup)    :: elbuf_tab !< Element buffer structure
      integer,                            intent(in)    :: npropm    !< Number of material properties
      integer,                            intent(in)    :: nummat    !< Number of materials
      real(kind=wp), dimension(npropm,nummat), intent(inout) :: pm   !< Material real properties array
      integer,                            intent(in)    :: ng        !< Current group number
      integer,                            intent(in)    :: npropg    !< Number of properties per geometric property
      integer,                            intent(in)    :: numgeo    !< Number of geometric properties
      real(kind=wp), dimension(npropg,numgeo), intent(inout) :: geo  !< Geometric properties array
      integer,                            intent(in)    :: nixs      !< Element connectivity array size
      integer,                            intent(in)    :: numels    !< Number of 3D solid elements
      integer,                            intent(in)    :: numelq    !< Number of 2D solid elements
      integer,                            intent(in)    :: nsvois    !< Number of neighbors to receive
      integer, dimension(nixs,numels+numelq+nsvois), intent(inout) :: ixs  !< Element connectivity array
      integer,                            intent(in)    :: numnod    !< Total number of nodes
      real(kind=wp), dimension(3,numnod), intent(inout) :: x         !< Global coordinate array
      real(kind=wp), dimension(3,numnod), intent(inout) :: a         !< Nodal acceleration array
      real(kind=wp), dimension(3,numnod), intent(inout) :: v         !< Nodal velocity array
      real(kind=wp), dimension(3,numnod), intent(inout) :: w         !< Nodal rotation velocity array
      real(kind=wp), dimension(6,numels), intent(inout) :: flux      !< Heat flux array
      real(kind=wp), dimension(numels),   intent(inout) :: flu1      !< Fluid properties array
      type(t_ale_connectivity),           intent(in)    :: ale_connect
      integer,                            intent(in)    :: nparg     !< Number of parameters per group
      integer, dimension(nparg,ngroup),   intent(inout) :: iparg     !< Element group parameters
      integer,                            intent(in)    :: stf       !< Size of the time function array
      real(kind=wp), dimension(stf),      intent(inout) :: tf        !< Time function array
      integer,                            intent(in)    :: snpc      !< Size of the function pointer array
      integer, dimension(snpc),           intent(inout) :: npf       !< Function pointer array
      integer,                            intent(in)    :: sbufmat   !< Size of the buffer material array
      real(kind=wp), dimension(sbufmat),  intent(inout) :: bufmat    !< Buffer material array
      integer,                            intent(in)    :: npsav     !< Size of the part save array
      integer,                            intent(in)    :: npart     !< Number of parts
      real(kind=wp),dimension(npsav,npart),intent(inout):: partsav   !< Part save array
      real(kind=wp),                      intent(inout) :: dt2t      !< Time step parameter
      integer,                            intent(inout) :: neltst    !< Number of test elements
      integer,                            intent(inout) :: ityptst   !< Test type
      real(kind=wp), dimension(numnod),   intent(inout) :: stifn     !< Nodal stiffness array
      integer,                            intent(in)    :: lsky      !< Skyline array size
      real(kind=wp), dimension(8,lsky),   intent(inout) :: fsky      !< Skyline force array
      integer, dimension(8,numels)    ,   intent(inout) :: iads      !< Address array
      integer,                            intent(inout) :: offset    !< Memory offset
      integer,                            intent(inout) :: nel       !< Number of elements
      integer, dimension(nel)         ,   intent(inout) :: iparts    !< Part array
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f11       !< Force component f11
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f21       !< Force component f21
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f31       !< Force component f31
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f12       !< Force component f12
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f22       !< Force component f22
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f32       !< Force component f32
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f13       !< Force component f13
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f23       !< Force component f23
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f33       !< Force component f33
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f14       !< Force component f14
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f24       !< Force component f24
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f34       !< Force component f34
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f15       !< Force component f15
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f25       !< Force component f25
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f35       !< Force component f35
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f16       !< Force component f16
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f26       !< Force component f26
      real(kind=wp), dimension(mvsiz) ,   intent(inout) :: f36       !< Force component f36
      type(nlocal_str_), target                         :: nloc_dmg  !< Non local damage structure
      integer,                            intent(in)    :: npropmi   !< Number of material integer parameter
      integer, dimension(npropmi,nummat), intent(inout) :: ipm       !< Material property indices
      integer,                            intent(inout) :: istrain   !< Strain flag
      integer,                            intent(in)    :: npropgi   !< Number of geometric integer parameter
      integer, dimension(npropgi,numgeo), intent(inout) :: igeo      !< Geometric property integer parameter
      integer,                            intent(in)    :: ngpe      !< 
      integer,                            intent(in)    :: nthread   !< Number of threads
      real(kind=wp), dimension(npsav,ngpe*nthread), intent(inout) :: gresav !< Global result save array
      integer,                            intent(in)    :: ngrth     !< Number of growth parameters
      integer,                            intent(in)    :: nelem     !< Number of elements per growth parameter
      integer,   dimension(ngrth+nelem+1),intent(inout) :: grth  
      integer,   dimension(nel),          intent(inout) :: igrth     !< Growth indices
      integer,                            intent(in)    :: ntable    !< Number of tables
      type(ttable) , dimension(ntable)  , intent(inout) :: table     !< Table data structure
      real(kind=wp), dimension(nel),      intent(inout) :: mssa      !< Mass array
      real(kind=wp), dimension(nel),      intent(inout) :: dmels     !< Element damage array
      real(kind=wp), dimension(mvsiz),    intent(inout) :: voln      !< Element volume array
      integer,                            intent(in)    :: itask     !< Task identifier
      integer,                            intent(in)    :: ioutprt   !< Output print flag
      type(mat_elem_),                    intent(inout) :: mat_elem  !< Material data structure
      integer,                            intent(in)    :: h3d_strain !< 3d strain output flag
      integer,                            intent(in)    :: ifthe     !< Size of the thermal force array
      real(kind=wp), dimension(ifthe)  ,  intent(inout) :: fthe      !< Thermal force array
      real(kind=wp), dimension(lsky)   ,  intent(inout) :: fthesky   !< Skyline thermal force 
      integer, intent(in)                               :: icondn    !< Size of the thermal conduction array
      real(kind=wp), dimension(icondn) ,  intent(inout) :: condn     !< Thermal conduction array
      real(kind=wp), dimension(lsky)   ,  intent(inout) :: condnsky  !< Thermal conduction skyline array
      integer,                            intent(in)    :: iexpan    !< Expansion flag
      type (dt_),                         intent(inout) :: dt        !< Time step structure
      real(kind=wp), dimension(mvsiz,6),  intent(inout) :: svis      !< Viscous stress array
      integer,                            intent(in)    :: iresp     !< response parameter
      integer,                            intent(inout) :: idel7nok  
      integer,                            intent(in)    :: maxfunc   !< maximum function parameter
      integer,                            intent(in)    :: imon_mat  !< Global thermal analysis flag
      integer,                            intent(in)    :: userl_avail !< user availability flag
      type(glob_therm_),                  intent(inout) :: glob_therm
      real(kind=8), dimension(3,numnod),  intent(in)    :: xdp       !< xdp is double prescision, kind = 8 is obligatoire
      type(sensors_),                     intent(inout) :: sensors
      real(kind=WP),                      intent(in)    :: dt1
      integer,                            intent(in)    :: th_strain
      real(kind=wp),                      intent(in)    :: volmin
      integer,       dimension(102)                     :: idtmin
      real(kind=WP),                      intent(in)    :: tt
      integer,                            intent(in)    :: idyna     !< Dynamic condensation flag
      integer,                            intent(in)    :: impl_s    !< Implicit solver flag
      integer,                            intent(inout) :: ineg_v    !< Negative volume flag
      integer,                            intent(in)    :: iparit    !< Parallel iteration flag
      integer,                            intent(in)    :: irep      !< Reference frame flag
      integer,                            intent(in)    :: iscau     !< Cauchy stress flag
      integer,                            intent(in)    :: ismdisp   !< Displacement flag
      integer,                            intent(in)    :: ismstr    !< Small strain flag
      integer,                            intent(inout) :: isorth    !< Orthotropic material flag
      integer,                            intent(inout) :: isorthg   !< Global orthotropic flag
      integer,                            intent(in)    :: jale      !< Ale formulation flag
      integer,                            intent(in)    :: jcvt      !< Coordinate transformation flag
      integer,                            intent(in)    :: jeul      !< Eulerian formulation flag
      integer,                            intent(in)    :: jlag      !< Lagrangian formulation flag
      integer,                            intent(inout) :: jsph      !< Smoothed particle hydrodynamics flag
      integer,                            intent(inout) :: jplasol   !< Plastic solution flag
      integer,                            intent(inout) :: jthe      !< Thermal analysis flag
      integer,                            intent(inout) :: mstop     !< Stop flag
      integer,                            intent(inout) :: mtn       !< Material type number
      integer,                            intent(in)    :: n2d       !< 2d analysis flag
      integer,                            intent(in)    :: ncpri     !< Print cycle interval
      integer,                            intent(in)    :: ncycle    !< Current cycle number
      integer,                            intent(in)    :: nfilsol   !< Fill solution flag
      integer,                            intent(in)    :: nft       !< Number of first elements
      integer,                            intent(in)    :: iint      !< Integration flag
      integer,                            intent(in)    :: nodadt    !< Nodal adaptive time step flag
      real(kind=wp),                      intent(in)    :: dtfac1(102)
!
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      !< Loop counters and flags
      integer :: i, j, nf1, iflag
      integer :: ilay, nlay, ir, is, it, mx, l_pla, l_epsd
      !< Element connectivity and property arrays
      integer :: mxt(mvsiz), ngl(mvsiz), ngeo(mvsiz), ii(6)
      integer, dimension(mvsiz) :: nc1,nc2,nc3,nc4,nc5,nc6
      !< Local computation variables
      real(kind=wp) :: c1,mbid(1),zt,wt   
      !< Deformation and kinematic variables
      real(kind=wp) :: vd2(mvsiz), dvol(mvsiz), deltax(mvsiz) !< velocity divergence, volume change, element length
      real(kind=wp) :: vis(mvsiz), qvis(mvsiz), cxx(mvsiz)    !< viscosity, viscous pressure, sound speed
      !< Stress components
      real(kind=wp), dimension(mvsiz) :: s1,s2,s3,s4,s5,s6    !< stress components 
      !< Strain rate components
      real(kind=wp), dimension(mvsiz) :: dxx,dyy,dzz,d4,d5,d6 !< strain rate components d11, d22, d33
      !< Jacobian matrix components
      real(kind=wp), dimension(mvsiz) :: jac1,jac2,jac3,jac4,jac5,jac6 !< jacobian matrix components j11, j12, j13
      !< Velocity gradients and additional variables
      real(kind=wp), dimension(mvsiz) :: vdx,vdy,vdz,ssp_eq,aire !< equivalent sound speed, element area
      !< Additional stress and kinematic variables
      real(kind=wp), dimension(mvsiz) :: sti,wxx,wyy,wzz,conde  
      !< Material parameters
      real(kind=wp), dimension(mvsiz) :: muvoid,off,rhoo,offg
      real(kind=wp), dimension(mvsiz) :: x1,x2,x3,x4,x5,x6
      real(kind=wp), dimension(mvsiz) :: y1,y2,y3,y4,y5,y6
      real(kind=wp), dimension(mvsiz) :: z1,z2,z3,z4,z5,z6
      real(kind=wp), dimension(mvsiz) :: vx1,vx2,vx3,vx4,vx5,vx6
      real(kind=wp), dimension(mvsiz) :: vy1,vy2,vy3,vy4,vy5,vy6
      real(kind=wp), dimension(mvsiz) :: vz1,vz2,vz3,vz4,vz5,vz6
      real(kind=wp), dimension(mvsiz) :: px1,px2,px3,px4,px5,px6
      real(kind=wp), dimension(mvsiz) :: py1,py2,py3,py4,py5,py6
      real(kind=wp), dimension(mvsiz) :: pz1,pz2,pz3,pz4,pz5,pz6
      real(kind=wp), dimension(mvsiz) :: px1h,px2h,px3h
      real(kind=wp), dimension(mvsiz) :: py1h,py2h,py3h
      real(kind=wp), dimension(mvsiz) :: pz1h,pz2h,pz3h
      real(kind=wp), dimension(mvsiz) :: vgxa,vgya,vgza,vga2
      real(kind=wp), dimension(mvsiz) :: xgxa,xgya,xgza
      real(kind=wp), dimension(mvsiz) :: xgxya,xgyza,xgzxa
      real(kind=wp), dimension(mvsiz) :: xgxa2,xgya2,xgza2
      real(kind=wp), dimension(mvsiz) :: dxy,dyx,dyz,dzy,dzx,dxz,divde
      real(kind=wp), dimension(mvsiz) :: r11,r12,r13,r21,r22,r23,r31,r32,r33
      real(kind=wp), dimension(mvsiz,6) :: gama,dd,them
      real(kind=wp), dimension(nel,6) :: sigo,sign
      real(kind=wp), dimension(mvsiz) :: sigym,g,nu,volg,sigy
      real(kind=wp), dimension(mvsiz) :: b1122,b1221,b2212,b1121
      real(kind=wp), dimension(mvsiz) :: b1122h,b1221h,b2212h,b1121h
      real(kind=wp), dimension(mvsiz,2) :: b1x,b1y,b2x,b2y,b1xh,b1yh,b2xh,b2yh,dir
      real(kind=wp), dimension(mvsiz) :: dcxx,dcxy,dcxz,dcyx,dcyy
      real(kind=wp), dimension(mvsiz) :: dcyz,dczx,dczy,dczz,dc4
      real(kind=wp), dimension(mvsiz) :: dc5,dc6,vzl,jaci33
      real(kind=wp), dimension(mvsiz) :: dhxx,dhxy,dhyx,dhyy,dhyz
      real(kind=wp), dimension(mvsiz) :: dhzx,dhzy,dhzz,dh4,dhxz
      real(kind=wp), dimension(mvsiz) :: dh5,dh6,eintm,ddhv
      real(kind=wp), dimension(mvsiz) :: et,r1_free,r3_free,r4_free
      real(kind=wp), dimension(mvsiz) :: stin,bid,dsv,alpha_e,llsh
!   
      integer :: pid,mid,ioffs,nn_del,ipres,isctl
      integer, dimension(mvsiz) :: mxt0,istab
      real(kind=wp), dimension(mvsiz) :: shf,offs,rx,ry,rz,nu1,fac,sx,sy,sz    
      real(kind=wp), dimension(mvsiz) :: tx,ty,tz,e0,n1x,n2x,n3x,n1y,n2y,n3y
      real(kind=wp), dimension(mvsiz) :: n1z,n2z,n3z,n4x,n5x,n6x,n4y,n5y,n6y
      real(kind=wp), dimension(mvsiz) :: n4z,n5z,n6z,amu,area,sti_c,ll,fld
      real(kind=wp), dimension(mvsiz) :: tempel,die,conden,voldp,fheat
      integer :: inloc,l_nloc,sz_r1_free,sz_ix
      integer, dimension(6) :: ipos, inod
      real(kind=wp), dimension(:) ,allocatable :: var_reg
      real(kind=wp), dimension(:), pointer :: dnl
      real(kind=wp) :: cns2,fqmax,dn,facdp
!
      type(g_bufel_) ,pointer :: gbuf
      type(l_bufel_) ,pointer :: lbuf     
!
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
!
      !< Initialization of element buffer addresses
      dsv = -HUGE(0.0_wp)
      nlay = elbuf_tab(ng)%nlay
      nlay = 1
      ilay = 1
      ir = 1
      is = 1
      it = 1
      isorthg = 0
      isorth  = 0
      gbuf => elbuf_tab(ng)%gbuf
      lbuf => elbuf_tab(ng)%bufly(ilay)%lbuf(ir,is,it)
      do i=1,6
        ii(i) = nel*(i-1)
      enddo
!
      !< Recover the old stress tensor
      do i=1,nel
        sigy(i)   = ep20
        sigo(i,1) = gbuf%sig(ii(1)+i)
        sigo(i,2) = gbuf%sig(ii(2)+i)
        sigo(i,3) = gbuf%sig(ii(3)+i)
        sigo(i,4) = gbuf%sig(ii(4)+i)
        sigo(i,5) = gbuf%sig(ii(5)+i)
        sigo(i,6) = gbuf%sig(ii(6)+i)
      enddo
!
      !< Non-local flag and array
      inloc = iparg(78,ng)
      allocate(var_reg(nel))
!
      !< To be defined
      sz_r1_free = nel
      sz_ix = numelq + numels + nsvois
!
      !< First element global address
      nf1=nft+1
!
!-------------------------------------------------------------------------------
!<  Gather nodal variables and compute intinsic rotations
!-------------------------------------------------------------------------------
      call s6zrcoor3(                                                          &
        numnod   ,x         ,ixs(1,nf1),v       ,gbuf%gama,gama     ,          &
        x1       ,x2        ,x3       ,x4       ,x5       ,x6       ,          &
        y1       ,y2        ,y3       ,y4       ,y5       ,y6       ,          &
        z1       ,z2        ,z3       ,z4       ,z5       ,z6       ,          &
        vx1      ,vx2       ,vx3      ,vx4      ,vx5      ,vx6      ,          &
        vy1      ,vy2       ,vy3      ,vy4      ,vy5      ,vy6      ,          &
        vz1      ,vz2       ,vz3      ,vz4      ,vz5      ,vz6      ,          &
        vd2      ,vis       ,gbuf%off ,offg     ,gbuf%smstr,gbuf%rho,rhoo     ,&
        r11      ,r12       ,r13      ,r21      ,r22      ,r23      ,r31      ,& 
        r32      ,r33       ,nc1      ,nc2      ,nc3      ,nc4      ,nc5      ,&
        nc6      ,ngl       ,mxt      ,ngeo     ,ioutprt  ,vgxa     ,vgya     ,&
        vgza     ,vga2      ,nel      ,xgxa     ,xgya     ,xgza     ,xgxa2    ,&
        xgya2    ,xgza2     ,xgxya    ,xgyza    ,xgzxa    ,iparg(1,ng),        &
        gbuf%gama_r,nixs    ,irep     ,ismstr   ,isorth   ,jlag     )    
!
      !< 
      nn_del = 0
      pid = ngeo(1)
      if (geo(190,pid)+geo(191,pid)+geo(192,pid)+geo(192,pid)>zero) then 
        nn_del = 6
      endif
      if (nn_del ==0 .and. dt%idel_brick>0) nn_del=6
      mx = mxt(1)
      c1 = pm(32,mx)
      ipres = 0
      isctl = igeo(97,pid)
!
      !< Elastic parameters, nodal stiffness ...  
      do i=1,nel
        nu(i)     = min(half,pm(21,mx))
        e0(i)     = three*(one-two*nu(i))*c1
        stin(i)   = zero
        conden(i) = zero
      enddo                                           
!
!-------------------------------------------------------------------------------
!<  Computation of derivatives and Jacobian matrix
!-------------------------------------------------------------------------------  
      call s6zderi3(&
        offg     ,voln     ,ngl      ,                                         &
        x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,           &
        y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,           &
        z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,           &
        px1      ,px2      ,px3      ,px4      ,px5      ,px6      ,           &
        py1      ,py2      ,py3      ,py4      ,py5      ,py6      ,           &
        pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,pz6      ,           &
        jac1     ,jac2     ,jac3     ,jac4     ,jac5     ,jac6     ,           &
        vzl      ,volg     ,gbuf%smstr,gbuf%off,nel      ,ismstr   ,           &
        idel7nok ,ineg_v   ,mstop    ,volmin   ,idtmin   )  
!
!-------------------------------------------------------------------------------
!<  Compute element characteristic length and volume change
!-------------------------------------------------------------------------------
      call sdlen3(&
        volg     ,deltax   ,                                                   &
        x1       ,x2       ,x5       ,x4       ,x3       ,x3       ,           &
        x6       ,x6       ,y1       ,y2       ,y5       ,y4       ,           &
        y3       ,y3       ,y6       ,y6       ,z1       ,z2       ,           &
        z5       ,z4       ,z3       ,z3       ,z6       ,z6       ,           &
        n1x      ,n2x      ,n3x      ,n4x      ,n5x      ,n6x      ,           &
        n1y      ,n2y      ,n3y      ,n4y      ,n5y      ,n6y      ,           &
        n1z      ,n2z      ,n3z      ,n4z      ,n5z      ,n6z      ,           &
        nel      ,mtn      ,jale     ,jeul     )
!
!-------------------------------------------------------------------------------
!<  Compute velocity gradients
!-------------------------------------------------------------------------------
      call s6zdefc3(&
        px1     ,px2       ,px3      ,px4      ,px5      ,px6      ,           &
        py1     ,py2       ,py3      ,py4      ,py5      ,py6      ,           & 
        pz1     ,pz2       ,pz3      ,pz4      ,pz5      ,pz6      ,           &
        vx1     ,vx2       ,vx3      ,vx4      ,vx5      ,vx6      ,           &
        vy1     ,vy2       ,vy3      ,vy4      ,vy5      ,vy6      ,           &
        vz1     ,vz2       ,vz3      ,vz4      ,vz5      ,vz6      ,           &
        dcxx    ,dcxy      ,dcxz     ,dcyx     ,dcyy     ,dcyz     ,           &
        dczx    ,dczy      ,dczz     ,wxx      ,wyy      ,wzz      ,           &
        nel     )     
!
!-------------------------------------------------------------------------------
!<  Reset internal forces
!-------------------------------------------------------------------------------
      call s6czero3(&
        f11     ,f21       ,f31      ,f12      ,f22      ,f32      ,           &
        f13     ,f23       ,f33      ,f14      ,f24      ,f34      ,           & 
        f15     ,f25       ,f35      ,f16      ,f26      ,f36      ,           &
        gbuf%sig,gbuf%eint ,gbuf%rho ,gbuf%qvis,gbuf%pla ,gbuf%epsd,           &
        gbuf%g_pla,gbuf%g_epsd,nel   ,nlay     )
!
!-------------------------------------------------------------------------------
!<  Update reference configuration (possible future change to small strain option)
!-------------------------------------------------------------------------------
      if (ismstr <= 3.or.(ismstr==4.and.jlag>0)) then
        call s6sav3(                                                           &
          gbuf%off ,gbuf%smstr,                                                &
          x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,         &
          y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,         &
          z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,         &
          nel      )
      endif
!
!-------------------------------------------------------------------------------
!<  Compute non-local variable increment at each gauss point 
!-------------------------------------------------------------------------------
      if (inloc > 0) then  
        l_nloc = nloc_dmg%l_nloc
        dnl => nloc_dmg%dnl(1:l_nloc) ! dnl = non local variable increment 
        do i=1,nel
          !< Recover non-local d.o.fs position
          inod(1) = nloc_dmg%idxi(nc1(i))
          inod(2) = nloc_dmg%idxi(nc2(i))
          inod(3) = nloc_dmg%idxi(nc3(i))
          inod(4) = nloc_dmg%idxi(nc4(i))
          inod(5) = nloc_dmg%idxi(nc5(i))
          inod(6) = nloc_dmg%idxi(nc6(i))
          do j = 1,6
            ipos(j) = nloc_dmg%posi(inod(j))
          enddo
          !< Computation of non-local variable at intg. point
          var_reg(i) = dnl(ipos(1)) + dnl(ipos(2))                             &
                     + dnl(ipos(3)) + dnl(ipos(4))                             & 
                     + dnl(ipos(5)) + dnl(ipos(6))
          var_reg(i) = var_reg(i)*one_over_6
        enddo
      endif 
!
!-------------------------------------------------------------------------------
!<  Computation of the strain rate tensor
!-------------------------------------------------------------------------------      
      tempel(:) = zero
      fheat(:)  = zero
      them(1:nel,1:6) = zero
      ioffs=0
      do i=1,nel
        offs(i)  = ep20
      enddo
      zt = zero
      wt = two

      call s6zdefo3(&
        dxx      ,dxy      ,dxz      ,dyx      ,                               &
        dyy      ,dyz      ,dzx      ,dzy      ,                               &
        dzz      ,d4       ,d5       ,d6       ,                               &
        dcxx     ,dcxy     ,dcxz     ,dcyx     ,                               &
        dcyy     ,dcyz     ,dczx     ,dczy     ,                               &
        dczz     ,zt       ,wt       ,vzl      ,                               & 
        voln     ,volg     ,off      ,offg     ,                               & 
        gbuf%off ,voldp    ,nel      ,dt1      ,                               &
        ismdisp  ,iscau    )
      !< Recover initial density
      rhoo(1:nel)  = lbuf%rho(1:nel)
      !< Volume change
      divde(1:nel) = dt1*(dxx(1:nel) + dyy(1:nel) + dzz(1:nel)) + dsv(1:nel)  
      !< Update element density
      call srho3(&
          pm       ,lbuf%vol ,lbuf%rho ,lbuf%eint,                             &
          divde    ,flux(1,nf1),flu1(nf1),voln   ,                             &
          dvol     ,ngl      ,mxt      ,off      ,                             &
          0        ,gbuf%tag22,voldp   ,lbuf%vol0dp,                           &
          amu      ,gbuf%off ,nel      ,mtn      ,                             &
          jale     ,ismstr   ,jeul     ,jlag     )
!
!-------------------------------------------------------------------------------      
!  Recover stress tensor at gauss point
!-------------------------------------------------------------------------------      
      call csmall3(&
        lbuf%sig ,s1       ,s2       ,s3       ,s4       ,s5       ,s6       , &
        gbuf%off ,off      ,nel      )
!-------------------------------------------------------------------------------      
!  Compute new stresses according to constitutive laws
!-------------------------------------------------------------------------------      
      call mmain(                                                              &
        timers   ,output   ,elbuf_tab,ng       ,pm       ,geo      ,           &
        ale_connect, ixs,  iparg,                                              &
        v        ,tf       ,npf      ,bufmat   ,sti      ,x        ,           &
        dt2t     ,neltst   ,ityptst  ,offset   ,nel      ,w        ,           &
        off      ,ngeo     ,mxt      ,ngl      ,voln     ,vd2      ,           &
        dvol     ,deltax   ,vis      ,qvis     ,cxx      ,                     &
        s1       ,s2       ,s3       ,s4       ,s5       ,s6       ,           &
        dxx      ,dyy      ,dzz      ,d4       ,d5       ,d6       ,           &
        wxx      ,wyy      ,wzz      ,                                         &
        jac1     ,jac2     ,jac3     ,jac4     ,jac5     ,jac6     ,           &
        vdx      ,vdy      ,vdz      ,muvoid   ,ssp_eq   ,aire     ,           &
        sigy     ,et       ,r1_free  ,lbuf%pla ,r3_free  ,amu      ,           &
        dxx      ,dxy      ,dxz      ,dyx      ,dyy      ,dyz      ,           &
        dzx      ,dzy      ,dzz      ,ipm      ,gama     ,bid      ,           &
        bid      ,bid      ,bid      ,bid      ,bid      ,bid      ,           &
        istrain  ,tempel   ,die      ,iexpan   ,ilay     ,mssa     ,           &
        dmels    ,ir       ,is       ,it       ,table    ,bid      ,           &
        bid      ,bid      ,bid      ,iparg(1,ng),igeo   ,conde    ,           &
        itask    ,nloc_dmg ,var_reg(1),mat_elem,h3d_strain,jplasol ,           &
        jsph     ,sz_r1_free,snpc    ,stf      ,sbufmat  ,glob_therm,          &
        svis     ,sz_ix    ,iresp    ,n2d      ,th_strain,ngroup   ,           &
        tt       ,dt1      ,ntable   ,numelq   ,nummat   ,numgeo   ,           &
        numnod   ,numels   ,idel7nok ,idtmin   ,maxfunc  ,imon_mat ,           &
        userl_avail,impl_s ,idyna    ,dt       ,fheat    ,sensors  ,           &
        opt_mtn=mtn,opt_jcvt=jcvt,opt_isorth=isorth,opt_isorthg=isorthg)
!
        !< Update nodal stiffness computation
        do i=1,nel
          stin(i) = stin(i) + sti(i)
        enddo
!
        !< Copy the new stress tensor
        do i=1,nel
          sign(i,1) = gbuf%sig(ii(1)+i)
          sign(i,2) = gbuf%sig(ii(2)+i)
          sign(i,3) = gbuf%sig(ii(3)+i)
          sign(i,4) = gbuf%sig(ii(4)+i)
          sign(i,5) = gbuf%sig(ii(5)+i)
          sign(i,6) = gbuf%sig(ii(6)+i)
        enddo
!
        if(glob_therm%nodadt_therm == 1) then
          do i=1,nel
            conden(i)= conden(i)+ conde(i)
          enddo
        endif
!
        !< Update the total strain tensor for output
        if (istrain == 1) then 
          call sstra3(&
            dxx      ,dyy      ,dzz      ,d4       ,d5       ,d6       ,       &
            lbuf%stra,wxx      ,wyy      ,wzz      ,off      ,nel      ,       &
            jcvt)
        endif
!
!-------------------------------------------------------------------------------      
!<  Computation of the new internal forces
!------------------------------------------------------------------------------- 
        l_pla  = elbuf_tab(ng)%bufly(ilay)%l_pla
        l_epsd = elbuf_tab(ng)%bufly(ilay)%l_epsd
        call s6zfint3(&
          lbuf%sig ,px1      ,px2      ,px3      ,px4      ,px5      ,         &
          px6      ,py1      ,py2      ,py3      ,py4      ,py5      ,         &
          py6      ,pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,         &
          pz6      ,f11      ,f21      ,f31      ,f12      ,f22      ,         &
          f32      ,f13      ,f23      ,f33      ,f14      ,f24      ,         &
          f34      ,f15      ,f25      ,f35      ,f16      ,f26      ,         &
          f36      ,voln     ,qvis     ,lbuf%eint,lbuf%rho ,lbuf%qvis,         &
          lbuf%pla ,lbuf%epsd,gbuf%epsd,gbuf%sig ,gbuf%eint,gbuf%rho ,         &
          gbuf%qvis,gbuf%pla ,volg     ,off      ,lbuf%vol ,gbuf%vol ,         &
          l_pla    ,l_epsd   ,nel      ,svis     ,nlay     )
!
!-------------------------------------------------------------------------------      
!<  Element failure global flag update
!------------------------------------------------------------------------------- 
        do i=1,nel                                        
          offg(i) = min(offg(i),off(i))                        
          if (lbuf%off(i) > one .and. gbuf%off(i) == one) then
            offs(i) = min(lbuf%off(i),offs(i))
            ioffs   = 1                                         
          end if                                             
        enddo                                               
        if (ioffs == 1) then
          do i=1,nel
            if (offs(i)<=two)gbuf%off(i) = offs(i)
          enddo
          do i=1,nel
            if (gbuf%off(i) > one) lbuf%off(i)=gbuf%off(i)
          end do
        end if
!
!-------------------------------------------------------------------------------      
!<  Computation of non-local internal forces 
!------------------------------------------------------------------------------- 
        if (inloc > 0) then 
          call s6fint_reg(&                       
            nloc_dmg ,var_reg  ,nel      ,off      ,voln     ,nodadt   ,       &
            nc1      ,nc2      ,nc3      ,nc4      ,nc5      ,nc6      ,       &
            px1      ,px2      ,px3      ,px4      ,px5      ,px6      ,       &
            py1      ,py2      ,py3      ,py4      ,py5      ,py6      ,       &
            pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,pz6      ,       &
            mx       ,itask    ,dt2t     ,gbuf%vol ,nft      ,iparit   ,       &
            dtfac1   )
        endif
!
!-------------------------------------------------------------------------------      
!<  Physical hourglass stabilization
!------------------------------------------------------------------------------- 
      if (impl_s == 0) then
        if (isctl > 0) then
          dn = geo(13,pid)
          call s6chour_ctl(&
            x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,       &
            y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,       &
            z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,       &
            vx1      ,vx2      ,vx3      ,vx4      ,vx5      ,vx6      ,       &
            vy1      ,vy2      ,vy3      ,vy4      ,vy5      ,vy6      ,       &
            vz1      ,vz2      ,vz3      ,vz4      ,vz5      ,vz6      ,       &
            f11      ,f12      ,f13      ,f14      ,f15      ,f16      ,       &
            f21      ,f22      ,f23      ,f24      ,f25      ,f26      ,       &
            f31      ,f32      ,f33      ,f34      ,f35      ,f36      ,       &
            pm       ,npropm   ,nummat   ,mtn      ,mxt      ,dn       ,       &
            gbuf%rho ,volg     ,cxx      ,gbuf%hourg,off     ,gbuf%vol ,       &
            gbuf%eint,dt1      ,stin     ,nel      )
        else
          call s6zhour3(&
            npropm   ,nummat   ,pm       ,gbuf%rho ,volg     ,cxx      ,       &
            x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,       &
            y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,       &
            z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,       &
            vx1      ,vx2      ,vx3      ,vx4      ,vx5      ,vx6      ,       &
            vy1      ,vy2      ,vy3      ,vy4      ,vy5      ,vy6      ,       &
            vz1      ,vz2      ,vz3      ,vz4      ,vz5      ,vz6      ,       &
            f11      ,f12      ,f13      ,f14      ,f15      ,f16      ,       &
            f21      ,f22      ,f23      ,f24      ,f25      ,f26      ,       &
            f31      ,f32      ,f33      ,f34      ,f35      ,f36      ,       &
            nu       ,gbuf%hourg,off     ,gbuf%vol ,gbuf%eint,nel      ,       &
            mxt      ,npropg   ,numgeo   ,geo      ,pid      ,dt1      ,       &
            elbuf_tab(ng),iint ,jlag     ,mtn      ,sigy     ,sign     ,       &
            sigo     )
        endif
      endif
!-------------------------------------------------------------------------------      
!<  Small strain treatment
!------------------------------------------------------------------------------- 
      call smallb3(gbuf%off ,offg     ,nel      ,ismstr   )
!-------------------------------------------------------------------------------      
!<  Energy balance
!------------------------------------------------------------------------------- 
      iflag = mod(ncycle,ncpri)
      if (ioutprt>0) then         
        call s6cbilan(                                                         &
          partsav  ,gbuf%eint,gbuf%rho ,gbuf%rk  ,gbuf%vol ,                   &
          vgxa     ,vgya     ,vgza     ,vga2     ,volg     ,                   &
          iparts   ,gresav   ,grth     ,igrth    ,gbuf%off ,                   &
          iexpan   ,gbuf%eintth,gbuf%fill,xgxa   ,xgya     ,                   &
          xgza     ,xgxa2    ,xgya2    ,xgza2    ,xgxya    ,                   &
          xgyza    ,xgzxa    ,itask    ,iparg(1,ng),gbuf%off,                  &
          sensors  ,nel      ,gbuf%g_wpla,gbuf%wpla)
      endif
!-------------------------------------------------------------------------------      
!<  Convected frame to global frame
!------------------------------------------------------------------------------- 
      call s6zrrota3(&
        r11      ,r21      ,r31      ,r12      ,r22      ,r32      ,           &
        r13      ,r23      ,r33      ,f11      ,f12      ,f13      ,           &
        f14      ,f15      ,f16      ,f21      ,f22      ,f23      ,           &
        f24      ,f25      ,f26      ,f31      ,f32      ,f33      ,           &
        f34      ,f35      ,f36      ,nel      )
!-------------------------------------------------------------------------------      
!<  Element distortion control
!------------------------------------------------------------------------------- 
      if ((isctl > 0).and.(1 == 2)) then
        alpha_e(1:nel) = one  
        call sdistor_ini(                                                      &
          nel      ,sti_c    ,npropm   ,nummat   ,ismstr   ,mxt      ,         &
          istab    ,pm       ,gbuf%sig ,gbuf%rho ,cxx      ,offg     ,         &
          gbuf%off ,ll       ,voln     ,fld      ,cns2     ,fqmax    )
!      all in global system 
        call s6get_xv(                                                         &
          nc1      ,nc2      ,nc3      ,nc4      ,nc5      ,nc6      ,         &
          x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,         &
          y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,         &
          z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,         &
          vx1      ,vx2      ,vx3      ,vx4      ,vx5      ,vx6      ,         &
          vy1      ,vy2      ,vy3      ,vy4      ,vy5      ,vy6      ,         &
          vz1      ,vz2      ,vz3      ,vz4      ,vz5      ,vz6      ,         &
          x        ,xdp      ,v        ,numnod   ,ismstr   ,nel      )
        call s6for_distor(&
          x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,         &
          y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,         &
          z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,         &
          vx1      ,vx2      ,x3       ,vx4      ,vx5      ,x6       ,         &
          vy1      ,vy2      ,y3       ,vy4      ,vy5      ,y6       ,         &
          vz1      ,vz2      ,z3       ,vz4      ,vz5      ,z6       ,         &
          f11      ,f12      ,f13      ,f14      ,f15      ,f16      ,         &
          f21      ,f22      ,f23      ,f24      ,f25      ,f26      ,         &
          f31      ,f32      ,f33      ,f34      ,f35      ,f36      ,         &
          stin     ,sti_c    ,fld      ,cns2     ,ll       ,istab    ,         &
          fqmax    ,gbuf%eint_distor   ,dt1      ,nel      )
      endif
      if (nfilsol/=0) call s6fillopt(                                          &
        gbuf%fill,sti      ,f11      ,f21      ,f31       ,f12      ,          &
        f22      ,f32      ,f13      ,f23      ,f33       ,f14      ,          &
        f24      ,f34      ,f15      ,f25      ,f35       ,f16      ,          &
        f26      ,f36      ,nel      )
!-------------------------------------------------------------------------------      
!<  Computation of internal forces
!-------------------------------------------------------------------------------
      if (iparit == 0) then
        call s6cumu3(                                                          &
          gbuf%off ,a        ,                                                 &
          nc1      ,nc2      ,nc3      ,nc4      ,nc5      ,nc6      ,         &
          stifn    ,stin     ,f11      ,f21      ,f31      ,f12      ,         &
          f22      ,f32      ,f13      ,f23      ,f33      ,f14      ,         &
          f24      ,f34      ,f15      ,f25      ,f35      ,f16      ,         &
          f26      ,f36      ,nel      ,jthe     ,fthe     ,them     ,         &
          condn    ,conden   ,ifthe    ,icondn   ,glob_therm%nodadt_therm)
      else
        call s6cumu3p(                                                         &
          gbuf%off ,stin     ,fsky     ,fsky     ,iads     ,                   &
          f11      ,f21      ,f31      ,f12      ,f22      ,f32      ,         &
          f13      ,f23      ,f33      ,f14      ,f24      ,f34      ,         &
          f15      ,f25      ,f35      ,f16      ,f26      ,f36      ,         &
          nel      ,nft      ,jthe     ,fthesky  ,them     ,condnsky ,         &
          conden   ,glob_therm%nodadt_therm)
      endif
!
      end subroutine s6zforc3
      end module s6zforc3_mod
