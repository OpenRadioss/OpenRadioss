!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    s6zhour3_mod   ../engine/source/elements/solid/solide6z/s6zhourg3.f90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.f90
!||====================================================================
      module s6zhour3_or_mod
      contains

!||====================================================================
!||    s6zhour3        ../engine/source/elements/solid/solide6z/s6zhourg3.f90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.f90
!||--- calls      -----------------------------------------------------
!||    mdama24         ../engine/source/elements/solid/solidez/mdama24.f
!||    szsvm           ../engine/source/elements/solid/solidez/szsvm.f
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.f
!||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.f90
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.f90
!||    precision_mod   ../common_source/modules/precision_mod.f90
!||====================================================================
      subroutine s6zhour3_or(                                                     &
        npropm  , nummat , pm      , rho     , vol     , cxx     ,        &
        x1i     , x2i    , x3i     , x4i     , x5i     , x6i     ,             &
        y1i     , y2i    , y3i     , y4i     , y5i     , y6i     ,             &
        z1i     , z2i    , z3i     , z4i     , z5i     , z6i     ,             &
        vx1i    , vx2i   , vx3i    , vx4i    , vx5i    , vx6i    ,             &
        vy1i    , vy2i   , vy3i    , vy4i    , vy5i    , vy6i    ,             &
        vz1i    , vz2i   , vz3i    , vz4i    , vz5i    , vz6i    ,             &
        f11     , f12    , f13     , f15     , f16     , f17     ,             &
        f21     , f22    , f23     , f25     , f26     , f27     ,             &
        f31     , f32    , f33     , f35     , f36     , f37     ,             &
        nu      , fhour  , off     , vol0    , eint    , nel     ,             &
        mat     , npropg , numgeo  , geo     , ngeo    , dt1     ,             &
        elbuf_str,iint   , jlag    , mtn     , sigy    , sig0    ,             &
        sigold  , ismstr  , matvis  , et      , dxx     ,             &
        dyy     , dzz    , d4      , d5       , d6     ,invstr   ,             &
        icp     , defp   ,                                                     & 
        gama    , uparam , mat_param )

!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
      use precision_mod, only : wp
      use constant_mod     
      use elbufdef_mod
      use mvsiz_mod, only : mvsiz
      use matparam_def_mod
!-------------------------------------------------------------------------------
!    i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
!-------------------------------------------------------------------------------
!    d u m m y   a r g u m e n t s
! ------------------------------------------------------------------------------
      integer,                     intent(in)      :: npropm      !< number of material property columns
      integer,                     intent(in)      :: nummat   
      integer,                     intent(in)      :: ismstr     !< number of materials
      real(kind=wp), dimension(npropm,nummat), intent(in) :: pm  !< material properties
      real(kind=wp), dimension(nel), intent(in)    :: rho         !< density
      real(kind=wp), dimension(mvsiz), intent(in)  :: vol         !< current volume
      real(kind=wp), dimension(mvsiz), intent(in)  :: cxx         !< sound speed
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.
      real(kind=8), dimension(mvsiz), intent(in)  :: x1i         !< x-coordinate of node 1 
      real(kind=8), dimension(mvsiz), intent(in)  :: x2i         !< x-coordinate of node 2
      real(kind=8), dimension(mvsiz), intent(in)  :: x3i         !< x-coordinate of node 3
      real(kind=8), dimension(mvsiz), intent(in)  :: x4i         !< x-coordinate of node 4
      real(kind=8), dimension(mvsiz), intent(in)  :: x5i         !< x-coordinate of node 5
      real(kind=8), dimension(mvsiz), intent(in)  :: x6i         !< x-coordinate of node 6
      real(kind=8), dimension(mvsiz), intent(in)  :: y1i         !< y-coordinate of node 1
      real(kind=8), dimension(mvsiz), intent(in)  :: y2i         !< y-coordinate of node 2
      real(kind=8), dimension(mvsiz), intent(in)  :: y3i         !< y-coordinate of node 3
      real(kind=8), dimension(mvsiz), intent(in)  :: y4i         !< y-coordinate of node 4
      real(kind=8), dimension(mvsiz), intent(in)  :: y5i         !< y-coordinate of node 5
      real(kind=8), dimension(mvsiz), intent(in)  :: y6i         !< y-coordinate of node 6
      real(kind=8), dimension(mvsiz), intent(in)  :: z1i         !< z-coordinate of node 1
      real(kind=8), dimension(mvsiz), intent(in)  :: z2i         !< z-coordinate of node 2
      real(kind=8), dimension(mvsiz), intent(in)  :: z3i         !< z-coordinate of node 3
      real(kind=8), dimension(mvsiz), intent(in)  :: z4i         !< z-coordinate of node 4
      real(kind=8), dimension(mvsiz), intent(in)  :: z5i         !< z-coordinate of node 5
      real(kind=8), dimension(mvsiz), intent(in)  :: z6i         !< z-coordinate of node 6
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx1i        !< x-velocity of node 1
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx2i        !< x-velocity of node 2
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx3i        !< x-velocity of node 3
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx4i        !< x-velocity of node 4
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx5i        !< x-velocity of node 5
      real(kind=wp), dimension(mvsiz), intent(in)  :: vx6i        !< x-velocity of node 6
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy1i        !< y-velocity of node 1
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy2i        !< y-velocity of node 2
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy3i        !< y-velocity of node 3
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy4i        !< y-velocity of node 4
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy5i        !< y-velocity of node 5
      real(kind=wp), dimension(mvsiz), intent(in)  :: vy6i        !< y-velocity of node 6
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz1i        !< z-velocity of node 1
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz2i        !< z-velocity of node 2
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz3i        !< z-velocity of node 3
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz4i        !< z-velocity of node 4
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz5i        !< z-velocity of node 5
      real(kind=wp), dimension(mvsiz), intent(in)  :: vz6i        !< z-velocity of node 6
      real(kind=wp), dimension(nel), intent(inout) :: f11         !< x-force at node 1
      real(kind=wp), dimension(nel), intent(inout) :: f12         !< x-force at node 2
      real(kind=wp), dimension(nel), intent(inout) :: f13         !< x-force at node 3
      real(kind=wp), dimension(nel), intent(inout) :: f15         !< x-force at node 5
      real(kind=wp), dimension(nel), intent(inout) :: f16         !< x-force at node 6
      real(kind=wp), dimension(nel), intent(inout) :: f17         !< x-force at node 7
      real(kind=wp), dimension(nel), intent(inout) :: f21         !< y-force at node 1
      real(kind=wp), dimension(nel), intent(inout) :: f22         !< y-force at node 2
      real(kind=wp), dimension(nel), intent(inout) :: f23         !< y-force at node 3
      real(kind=wp), dimension(nel), intent(inout) :: f25         !< y-force at node 5
      real(kind=wp), dimension(nel), intent(inout) :: f26         !< y-force at node 6
      real(kind=wp), dimension(nel), intent(inout) :: f27         !< y-force at node 7
      real(kind=wp), dimension(nel), intent(inout) :: f31         !< z-force at node 1
      real(kind=wp), dimension(nel), intent(inout) :: f32         !< z-force at node 2
      real(kind=wp), dimension(nel), intent(inout) :: f33         !< z-force at node 3
      real(kind=wp), dimension(nel), intent(inout) :: f35         !< z-force at node 5
      real(kind=wp), dimension(nel), intent(inout) :: f36         !< z-force at node 6
      real(kind=wp), dimension(nel), intent(inout) :: f37         !< z-force at node 7

      real(kind=wp), dimension(mvsiz), intent(inout)  :: nu        
      real(kind=wp), dimension(nel,3,4), intent(inout) :: fhour   !< hourglass forces
      real(kind=wp), dimension(mvsiz), intent(in)  :: off         !< element activation flag
      real(kind=wp), dimension(nel), intent(in)    :: vol0        !< initial volume
      real(kind=wp), dimension(nel), intent(inout) :: eint        !< internal energy
      integer,                       intent(in)    :: nel         !< number of elements
      integer,     dimension(mvsiz), intent(in)    :: mat         !< material identifiers
      integer,                       intent(in)    :: npropg      !< number of property group columns
      integer,                       intent(in)    :: numgeo      !< number of geometric property columns
      real(kind=wp), dimension(npropg,numgeo), intent(in) :: geo  !< geometry properties
      integer,      dimension(mvsiz),intent(in)    :: ngeo         !< property identifiers
      real(kind=wp),                 intent(in)    :: dt1         !< time step
      type(elbuf_struct_),                  target :: elbuf_str   !< element buffer structure
      integer,                       intent(in)    :: iint        !< integration flag
      integer,                       intent(in)    :: jlag        !< energy calculation flag
      integer,                       intent(in)    :: mtn         !< material type number
      real(kind=wp),dimension(mvsiz),intent(inout) :: sigy        !< yield stress 
      real(kind=wp),dimension(nel,6),intent(inout) :: sig0        !< current stress
      real(kind=wp),dimension(nel,6),intent(inout) :: sigold      !< previous stress
      integer,                       intent(in) :: matvis 
      real(kind=wp), dimension(nel), intent(in) :: et
      real(kind=wp), dimension(nel), intent(in) :: dxx
      real(kind=wp), dimension(nel), intent(in) :: dyy
      real(kind=wp), dimension(nel), intent(in) :: dzz
      real(kind=wp), dimension(nel), intent(in) :: d4
      real(kind=wp), dimension(nel), intent(in) :: d5
      real(kind=wp), dimension(nel), intent(in) :: d6
      integer,                       intent(in) :: invstr
      integer,                       intent(in) :: icp
      real(kind=wp), dimension(nel), intent(in) :: defp

      type(matparam_struct_)  , intent(in) :: mat_param
      real(kind=wp),dimension(mvsiz,6),intent(inout) :: gama        
      real(kind=wp),dimension(nel),intent(in) :: uparam
!-------------------------------------------------------------------------------
!    l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i, j, mx, mt,iplast, iet
      real(kind=wp) :: dett,fac,fac1,fac2,smo
      real(kind=wp) :: jaci1, jaci2, jaci3, jaci4, jaci5, jaci6, jaci7, jaci8, jaci9
      real(kind=wp) :: jaci12, jaci45, jaci78
      real(kind=wp) :: x_17_46, x_28_35, y_17_46, y_28_35, z_17_46, z_28_35
      real(kind=wp) :: hx, hy, hz, h1x, h1y, h1z, h2x, h2y, h2z, h3x, h3y, h3z, h4x, h4y, h4z
      real(kind=wp) :: rho0, g0, c1, nuu
      real(kind=wp) :: vx3478, vx2358, vx1467, vx1256
      real(kind=wp) :: vy3478, vy2358, vy1467, vy1256
      real(kind=wp) :: vz3478, vz2358, vz1467, vz1256
      real(kind=wp) :: vx17, vy17, vx28, vy28, vx35, vy35, vx46, vy46
      real(kind=wp) :: vz17, vz28, vz35, vz46
      real(kind=wp) :: e_r, e_s, e_t
      real(kind=wp) :: hq13p, hq13n, hq24p, hq24n, ff
      real(kind=wp) :: dama_g(mvsiz,3) !?
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.
      real(kind=8), dimension(nel) :: x1, x2, x3, x4, x5, x6, x7, x8
      real(kind=8), dimension(nel) :: y1, y2, y3, y4, y5, y6, y7, y8
      real(kind=8), dimension(nel) :: z1, z2, z3, z4, z5, z6, z7, z8
      real(kind=wp), dimension(nel) :: vx1, vx2, vx3, vx4, vx5, vx6, vx7, vx8
      real(kind=wp), dimension(nel) :: vy1, vy2, vy3, vy4, vy5, vy6, vy7, vy8
      real(kind=wp), dimension(nel) :: vz1, vz2, vz3, vz4, vz5, vz6, vz7, vz8
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.
      real(kind=8), dimension(nel) :: x17, x28, x35, x46
      real(kind=8), dimension(nel) :: y17, y28, y35, y46
      real(kind=8), dimension(nel) :: z17, z28, z35, z46
      real(kind=wp), dimension(nel) :: jac_59_68, jac_67_49, jac_48_57, jac_19_37
      real(kind=wp), dimension(nel) :: px1, px2, px3, px4
      real(kind=wp), dimension(nel) :: py1, py2, py3, py4
      real(kind=wp), dimension(nel) :: pz1, pz2, pz3, pz4
      real(kind=wp), dimension(nel) :: px1h1, px2h1, px3h1, px4h1
      real(kind=wp), dimension(nel) :: px1h2, px2h2, px3h2, px4h2
      real(kind=wp), dimension(nel) :: px1h3, px2h3, px3h3, px4h3
      real(kind=wp), dimension(nel) :: px1h4, px2h4, px3h4, px4h4
      real(kind=wp), dimension(nel) :: jac1, jac2, jac3, jac4, jac5, jac6, jac7, jac8, jac9
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.      
      real(kind=8), dimension(nel) :: det
      real(kind=wp), dimension(nel) :: fcl, hgz1, hgz2
      real(kind=wp), dimension(nel) :: gg
      real(kind=wp), dimension(nel) :: caq
      real(kind=wp), dimension(nel) :: g_3dt, e0
      real(kind=wp), dimension(nel) :: nu1, nu2, nu3, nu4, nus
      real(kind=wp), dimension(nel) :: hgx1, hgx2, hgx3, hgx4
      real(kind=wp), dimension(nel) :: hgy1, hgy2, hgy3, hgy4
      real(kind=wp), dimension(nel) :: hgz3, hgz4
      real(kind=wp), dimension(nel) :: jr_1, js_1, jt_1
      real(kind=wp), dimension(nel) :: jr0, js0, jt0
      real(kind=wp), dimension(nel) :: h11, h22, h33, h12, h13, h23
      real(kind=wp), dimension(3,4) :: fhourt
      real(kind=wp), dimension(6) :: dsig
      real(kind=wp) :: ds,de
      real(kind=wp), dimension(nel) :: f11_hgl, f12_hgl, f13_hgl, f14_hgl, f15_hgl, f16_hgl
      real(kind=wp), dimension(nel) :: f17_hgl, f18_hgl
      real(kind=wp), dimension(nel) :: f21_hgl, f22_hgl, f23_hgl, f24_hgl, f25_hgl, f26_hgl
      real(kind=wp), dimension(nel) :: f27_hgl, f28_hgl
      real(kind=wp), dimension(nel) :: f31_hgl, f32_hgl, f33_hgl, f34_hgl, f35_hgl, f36_hgl
      real(kind=wp), dimension(nel) :: f37_hgl, f38_hgl
      real(kind=wp), dimension(mvsiz) :: deint,sm1,sm2,smo1,smo2
      real(kind=wp), dimension(mvsiz,3,4) :: dfhour, nfhour
      real(kind=wp), dimension(mvsiz,3,3) :: cc,cg,g33
      real(kind=wp) :: gm,gmin

!

!===============================================================================
!     s o u r c e  l i n e s
!===============================================================================
!
      !-------------------------------------------------------------------------
      !< translation from 6 nodes penta to 8 nodes degenerated hex equivalence
      !-------------------------------------------------------------------------
      !< nodal x coordinates
      x1(1:nel) = x1i(1:nel)
      x2(1:nel) = x2i(1:nel)
      x3(1:nel) = x3i(1:nel)
      x4(1:nel) = x3i(1:nel)
      x5(1:nel) = x4i(1:nel)
      x6(1:nel) = x5i(1:nel)
      x7(1:nel) = x6i(1:nel)
      x8(1:nel) = x6i(1:nel)
      !< nodal y coordinates
      y1(1:nel) = y1i(1:nel)
      y2(1:nel) = y2i(1:nel)
      y3(1:nel) = y3i(1:nel)
      y4(1:nel) = y3i(1:nel)
      y5(1:nel) = y4i(1:nel)
      y6(1:nel) = y5i(1:nel)
      y7(1:nel) = y6i(1:nel)
      y8(1:nel) = y6i(1:nel)
      !< nodal z coordinates
      z1(1:nel) = z1i(1:nel)
      z2(1:nel) = z2i(1:nel)
      z3(1:nel) = z3i(1:nel)
      z4(1:nel) = z3i(1:nel)
      z5(1:nel) = z4i(1:nel)
      z6(1:nel) = z5i(1:nel)
      z7(1:nel) = z6i(1:nel)
      z8(1:nel) = z6i(1:nel)
      !< nodal x velocities
      vx1(1:nel) = vx1i(1:nel)
      vx2(1:nel) = vx2i(1:nel)
      vx3(1:nel) = vx3i(1:nel)
      vx4(1:nel) = vx3i(1:nel)
      vx5(1:nel) = vx4i(1:nel)
      vx6(1:nel) = vx5i(1:nel)
      vx7(1:nel) = vx6i(1:nel)
      vx8(1:nel) = vx6i(1:nel)
      !< nodal y velocities
      vy1(1:nel) = vy1i(1:nel)
      vy2(1:nel) = vy2i(1:nel)
      vy3(1:nel) = vy3i(1:nel)
      vy4(1:nel) = vy3i(1:nel)
      vy5(1:nel) = vy4i(1:nel)
      vy6(1:nel) = vy5i(1:nel)
      vy7(1:nel) = vy6i(1:nel)
      vy8(1:nel) = vy6i(1:nel)
      !< nodal z velocities
      vz1(1:nel) = vz1i(1:nel)
      vz2(1:nel) = vz2i(1:nel)
      vz3(1:nel) = vz3i(1:nel)
      vz4(1:nel) = vz3i(1:nel)
      vz5(1:nel) = vz4i(1:nel)
      vz6(1:nel) = vz5i(1:nel)
      vz7(1:nel) = vz6i(1:nel)
      vz8(1:nel) = vz6i(1:nel)
!
      !-------------------------------------------------------------------------
      !< recover material parameters
      !-------------------------------------------------------------------------
      iet  = iint
      mx   = mat(1)                              !< material index
      rho0 = pm(1,mx)                            !< initial density
      nuu  = pm(21,mx)  
      nu = nuu                         !< poisson's ratio
      iplast = elbuf_str%gbuf%g_pla              !< plasticity flag



      call mmodul(1     ,nel     ,pm    ,mat    ,mtn    , &
                 gama    ,uparam  ,cc    ,cg     ,g33  , mat_param )

      do i=1,nel

       gm = third*(g33(i,1,1)+g33(i,2,2)+g33(i,3,3))
       gg(i)=half*rho0*cxx(i)*cxx(i)*(one -two*nuu)/(one-nuu)
!c----- sometimes gm is too small       
       gmin = gg(i)*em04
       gg(i)=min(gg(i),gm)       
       gg(i)=max(gg(i),gmin)   
       e0(i)=two*(one+nuu)*gg(i)
       
      enddo


!c----
      
      if (iet > 1 .and. matvis>0 ) then
       call szetfac(1,nel,iet,mtn,et,gg )
      elseif (matvis==1.and.ismstr<10) then
       do i=1,nel
        ff=third*(dxx(i)+dyy(i)+dzz(i))
        de =(dxx(i)-ff)*(dxx(i)-ff)+(dyy(i)-ff)*(dyy(i)-ff)+ &
           (dzz(i)-ff)*(dzz(i)-ff) + fourth*(d4(i)*d4(i)+    &
                                    d5(i)*d5(i)+d6(i)*d6(i))
         de = de*dt1
         dsig(1)=sig0(i,1)-sigold(i,1)
         dsig(2)=sig0(i,2)-sigold(i,2)
         dsig(3)=sig0(i,3)-sigold(i,3)
         dsig(4)=sig0(i,4)-sigold(i,4)
         dsig(5)=sig0(i,5)-sigold(i,5)
         dsig(6)=sig0(i,6)-sigold(i,6)
         ff= third*(dsig(1)+dsig(2)+dsig(3))
         dsig(1)=dsig(1)-ff
         dsig(2)=dsig(2)-ff
         dsig(3)=dsig(3)-ff
         ds =dsig(1)*dsig(1)+dsig(2)*dsig(2)+dsig(3)*dsig(3)+ &
             dsig(4)*dsig(4)+dsig(5)*dsig(5)+dsig(6)*dsig(6) 
         gg(i)=max(fiveem2*gg(i),sqrt(ds/max(de,em30)))
       enddo
      endif

      !-------------------------------------------------------------------------
      !< recover properties parameters
      !-------------------------------------------------------------------------
  
   
      if(invstr>=35)then
        mt = ngeo(1)
        do i=1,nel
          caq(i)=fourth*off(i)*geo(13,mt)
        enddo
      else
        mx = mat(1)
        do i=1,nel
          caq(i)=fourth*off(i)*pm(4,mx)
        enddo
      endif

          do i=1,nel
            g_3dt(i)=third*off(i)*gg(i)*dt1

          enddo

!c---------normalized by gg-------      
      call mmod_norm(1,nel  ,gg    ,cc     ,cg    ,g33     )
!c
!c materiaux non fluides cxx->spp
      if (iet > 1 ) then
!c------now when ihkt=2, w.r.t. isolid=1, fac=0.2*0.006666 ~0.001     
       do i=1,nel
         fcl(i)=onep1*caq(i)*rho(i)*vol(i)**third
         fcl(i)=zep00666666667*fcl(i)*cxx(i)
       enddo
      else
       do i=1,nel
         fcl(i)=caq(i)*rho(i)*vol(i)**third
         fcl(i)=zep00666666667*fcl(i)*cxx(i)
       enddo
        end if

!

      if(icp==1)then
       do i=1,nel
        nus(i) =zep499
       enddo
      elseif(icp==2.and.iplast>0)then
       do i=1,nel
        fac1 = sigy(i)/e0(i)+defp(i)
        fac2 = defp(i)/fac1
        nus(i)=nuu+(half-nuu)*fac2
       enddo
      else
       do i=1,nel
        nus(i) =nuu
       enddo
      endif
      do i=1,nel
       nu2(i) =nus(i)/(one-nus(i))
       nu4(i) =nus(i)
      enddo
      



!
      !-------------------------------------------------------------------------
      !< nodal coordinate differences
      !-------------------------------------------------------------------------
      do i=1,nel
        x17(i)=x7(i)-x1(i)
        x28(i)=x8(i)-x2(i)
        x35(i)=x5(i)-x3(i)
        x46(i)=x6(i)-x4(i)
  
        y17(i)=y7(i)-y1(i)
        y28(i)=y8(i)-y2(i)
        y35(i)=y5(i)-y3(i)
        y46(i)=y6(i)-y4(i)
  
        z17(i)=z7(i)-z1(i)
        z28(i)=z8(i)-z2(i)
        z35(i)=z5(i)-z3(i)
        z46(i)=z6(i)-z4(i)
      enddo
!
      !-------------------------------------------------------------------------
      !< compute the jacobian matrix
      !-------------------------------------------------------------------------
      do i=1,nel
        jac4(i)=x17(i)+x28(i)-x35(i)-x46(i)
        jac5(i)=y17(i)+y28(i)-y35(i)-y46(i)
        jac6(i)=z17(i)+z28(i)-z35(i)-z46(i)
!
            x_17_46=x17(i)+x46(i)
            x_28_35=x28(i)+x35(i)
            y_17_46=y17(i)+y46(i)
            y_28_35=y28(i)+y35(i)
            z_17_46=z17(i)+z46(i)
            z_28_35=z28(i)+z35(i)
!
            jac7(i)=x_17_46+x_28_35
            jac8(i)=y_17_46+y_28_35
            jac9(i)=z_17_46+z_28_35
            jac1(i)=x_17_46-x_28_35
            jac2(i)=y_17_46-y_28_35
            jac3(i)=z_17_46-z_28_35
!
            jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
            jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
            jac_19_37(i)=jac1(i)*jac9(i)-jac3(i)*jac7(i)
            jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)
!
            det(i) = one_over_64*(jac1(i)*jac_59_68(i)+jac2(i)*jac_67_49(i)+jac3(i)*jac_48_57(i))
          enddo
!
      !-------------------------------------------------------------------------
      !< compute the inverse jacobian matrix
      !-------------------------------------------------------------------------
      do i=1,nel
        dett=one_over_64/det(i)
        jaci1=dett*jac_59_68(i)
        jaci4=dett*jac_67_49(i)
        jaci7=dett*jac_48_57(i)
        jaci2=dett*(-jac2(i)*jac9(i)+jac3(i)*jac8(i))
        jaci5=dett*( jac1(i)*jac9(i)-jac3(i)*jac7(i))
        jaci8=dett*(-jac1(i)*jac8(i)+jac2(i)*jac7(i))
        jaci3=dett*( jac2(i)*jac6(i)-jac3(i)*jac5(i))
        jaci6=dett*(-jac1(i)*jac6(i)+jac3(i)*jac4(i))
        jaci9=dett*( jac1(i)*jac5(i)-jac2(i)*jac4(i))
!   
        jaci12=jaci1-jaci2
        jaci45=jaci4-jaci5
        jaci78=jaci7-jaci8
        px2(i)= jaci12-jaci3
        py2(i)= jaci45-jaci6
        pz2(i)= jaci78-jaci9
        px4(i)=-jaci12-jaci3
        py4(i)=-jaci45-jaci6
        pz4(i)=-jaci78-jaci9
!
            jaci12=jaci1+jaci2
            jaci45=jaci4+jaci5
            jaci78=jaci7+jaci8
            px1(i)=-jaci12-jaci3
            py1(i)=-jaci45-jaci6
            pz1(i)=-jaci78-jaci9
            px3(i)=jaci12-jaci3
            py3(i)=jaci45-jaci6
            pz3(i)=jaci78-jaci9
          enddo
!
!c we do it in the same order of szderi3
!c h3
!c 1 -1 1 -1 1 -1 1 -1
          do i=1,nel
            h3x=x1(i)-x2(i)+x3(i)-x4(i)+x5(i)-x6(i)+x7(i)-x8(i)
            h3y=y1(i)-y2(i)+y3(i)-y4(i)+y5(i)-y6(i)+y7(i)-y8(i)
            h3z=z1(i)-z2(i)+z3(i)-z4(i)+z5(i)-z6(i)+z7(i)-z8(i)
            hx=one_over_8*h3x
            hy=one_over_8*h3y
            hz=one_over_8*h3z
            px1h3(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
            px2h3(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
            px3h3(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
            px4h3(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
          end do

!c!    h1
!c! 1 1 -1 -1 -1 -1 1 1
          do i=1,nel
            h1x=x1(i)+x2(i)-x3(i)-x4(i)-x5(i)-x6(i)+x7(i)+x8(i)
            h1y=y1(i)+y2(i)-y3(i)-y4(i)-y5(i)-y6(i)+y7(i)+y8(i)
            h1z=z1(i)+z2(i)-z3(i)-z4(i)-z5(i)-z6(i)+z7(i)+z8(i)
            hx=one_over_8*h1x
            hy=one_over_8*h1y
            hz=one_over_8*h1z
            px1h1(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
            px2h1(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
            px3h1(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
            px4h1(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
          end do

!c   h2
!c 1 -1 -1 1 -1 1 1 -1
          do i=1,nel
            h2x=x1(i)-x2(i)-x3(i)+x4(i)-x5(i)+x6(i)+x7(i)-x8(i)
            h2y=y1(i)-y2(i)-y3(i)+y4(i)-y5(i)+y6(i)+y7(i)-y8(i)
            h2z=z1(i)-z2(i)-z3(i)+z4(i)-z5(i)+z6(i)+z7(i)-z8(i)
            hx=one_over_8*h2x
            hy=one_over_8*h2y
            hz=one_over_8*h2z
            px1h2(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
            px2h2(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
            px3h2(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
            px4h2(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
          end do

!c   h4       
!c -1 1 -1 1 1 -1 1 -1
      do i=1,nel
        h4x=-x1(i)+x2(i)-x3(i)+x4(i)+x5(i)-x6(i)+x7(i)-x8(i)
        h4y=-y1(i)+y2(i)-y3(i)+y4(i)+y5(i)-y6(i)+y7(i)-y8(i)
        h4z=-z1(i)+z2(i)-z3(i)+z4(i)+z5(i)-z6(i)+z7(i)-z8(i)   
        hx=one_over_8*h4x
        hy=one_over_8*h4y
        hz=one_over_8*h4z   
        px1h4(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
        px2h4(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
        px3h4(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
        px4h4(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
      end do
!     
      do i=1,nel
        vx3478=vx3(i)-vx4(i)-vx7(i)+vx8(i)
        vx2358=vx2(i)-vx3(i)-vx5(i)+vx8(i)
        vx1467=vx1(i)-vx4(i)-vx6(i)+vx7(i)
        vx1256=vx1(i)-vx2(i)-vx5(i)+vx6(i)
!
            vy3478=vy3(i)-vy4(i)-vy7(i)+vy8(i)
            vy2358=vy2(i)-vy3(i)-vy5(i)+vy8(i)
            vy1467=vy1(i)-vy4(i)-vy6(i)+vy7(i)
            vy1256=vy1(i)-vy2(i)-vy5(i)+vy6(i)
!
            vz3478=vz3(i)-vz4(i)-vz7(i)+vz8(i)
            vz2358=vz2(i)-vz3(i)-vz5(i)+vz8(i)
            vz1467=vz1(i)-vz4(i)-vz6(i)+vz7(i)
            vz1256=vz1(i)-vz2(i)-vz5(i)+vz6(i)
            hgx3(i)=(vx1467-vx2358)*one_over_8
!
            hgx1(i)=(vx1467+vx2358)*one_over_8
            hgx2(i)=(vx1256-vx3478)*one_over_8
            hgx4(i)=-(vx1256+vx3478)*one_over_8
!
            hgy3(i)=(vy1467-vy2358)*one_over_8
            hgy1(i)=(vy1467+vy2358)*one_over_8
            hgy2(i)=(vy1256-vy3478)*one_over_8
            hgy4(i)=-(vy1256+vy3478)*one_over_8
!
            hgz3(i)=(vz1467-vz2358)*one_over_8
            hgz1(i)=(vz1467+vz2358)*one_over_8
            hgz2(i)=(vz1256-vz3478)*one_over_8
            hgz4(i)=-(vz1256+vz3478)*one_over_8
          enddo

          do i=1,nel
            vx17=vx1(i)-vx7(i)
            vx28=vx2(i)-vx8(i)
            vx35=vx3(i)-vx5(i)
            vx46=vx4(i)-vx6(i)
            vy17=vy1(i)-vy7(i)
            vy28=vy2(i)-vy8(i)
            vy35=vy3(i)-vy5(i)
            vy46=vy4(i)-vy6(i)
            vz17=vz1(i)-vz7(i)
            vz28=vz2(i)-vz8(i)
            vz35=vz3(i)-vz5(i)
            vz46=vz4(i)-vz6(i)
!
!c   alpha =1 ->eta zeta   
!c 1 1 -1 -1 -1 -1 1 1
!vy1467=vy1(i)-vy4(i)-vy6(i)+vy7(i)
!vz2358=vz2(i)-vz3(i)-vz5(i)+vz8(i)
!hgx1(i)=(vx1467+vx2358)*one_over_8
            hgx1(i)= hgx1(i) &
              -(px1h1(i)*vx17+px2h1(i)*vx28 &
              +px3h1(i)*vx35+px4h1(i)*vx46)
            hgy1(i)= hgy1(i) &
              -(px1h1(i)*vy17+px2h1(i)*vy28 &
              +px3h1(i)*vy35+px4h1(i)*vy46)
            hgz1(i)= hgz1(i) &
              -(px1h1(i)*vz17+px2h1(i)*vz28 &
              +px3h1(i)*vz35+px4h1(i)*vz46)
!
!c   alpha =2 ->zeta ksi
!c 1 -1 -1 1 -1 1 1 -1
            hgx2(i)= hgx2(i) &
              -(px1h2(i)*vx17+px2h2(i)*vx28 &
              +px3h2(i)*vx35+px4h2(i)*vx46)
            hgy2(i)= hgy2(i) &
              -(px1h2(i)*vy17+px2h2(i)*vy28 &
              +px3h2(i)*vy35+px4h2(i)*vy46)
            hgz2(i)= hgz2(i) &
              -(px1h2(i)*vz17+px2h2(i)*vz28 &
              +px3h2(i)*vz35+px4h2(i)*vz46)
!
!c   alpha =3 ->ksi eta
!c 1 -1 1 -1 1 -1 1 -1
            hgx3(i)= hgx3(i) &
              -(px1h3(i)*vx17+px2h3(i)*vx28 &
              +px3h3(i)*vx35+px4h3(i)*vx46)
            hgy3(i)= hgy3(i) &
              -(px1h3(i)*vy17+px2h3(i)*vy28 &
              +px3h3(i)*vy35+px4h3(i)*vy46)
            hgz3(i)= hgz3(i) &
              -(px1h3(i)*vz17+px2h3(i)*vz28 &
              +px3h3(i)*vz35+px4h3(i)*vz46)

!
!c   alpha =4 ->ksi eta zeta
!c -1 1 -1 1 1 -1 1 -1
            hgx4(i)= hgx4(i) &
              -(px1h4(i)*vx17+px2h4(i)*vx28 &
              +px3h4(i)*vx35+px4h4(i)*vx46)
            hgy4(i)= hgy4(i) &
              -(px1h4(i)*vy17+px2h4(i)*vy28 &
              +px3h4(i)*vy35+px4h4(i)*vy46)
            hgz4(i)= hgz4(i) &
              -(px1h4(i)*vz17+px2h4(i)*vz28 &
              +px3h4(i)*vz35+px4h4(i)*vz46)
          enddo
!
!-------------------------------------------------------------------------
          do i=1,nel
!
            jr0(i) = jac1(i)
            js0(i) = jac5(i)
            jt0(i) = jac9(i)
!  jac1 r,         jac5 s,         jac9 t
            jr_1(i) = one/max(em20,jr0(i))
            js_1(i) = one/max(em20,js0(i))
            jt_1(i) = one/max(em20,jt0(i))
            h11(i) = js0(i)*jt0(i)*jr_1(i)
            h22(i) = jr0(i)*jt0(i)*js_1(i)
            h33(i) = jr0(i)*js0(i)*jt_1(i)
            h12(i) = jt0(i)
            h13(i) = js0(i)
            h23(i) = jr0(i)
!      !hii
          enddo
!
          do i=1,nel
            fhour(i,1,1) = fhour(i,1,1)*off(i)
            fhour(i,1,2) = fhour(i,1,2)*off(i)
            fhour(i,1,3) = fhour(i,1,3)*off(i)
            fhour(i,1,4) = fhour(i,1,4)*off(i)
            fhour(i,2,1) = fhour(i,2,1)*off(i)
            fhour(i,2,2) = fhour(i,2,2)*off(i)
            fhour(i,2,3) = fhour(i,2,3)*off(i)
            fhour(i,2,4) = fhour(i,2,4)*off(i)
            fhour(i,3,1) = fhour(i,3,1)*off(i)
            fhour(i,3,2) = fhour(i,3,2)*off(i)
            fhour(i,3,3) = fhour(i,3,3)*off(i)
            fhour(i,3,4) = fhour(i,3,4)*off(i)
          enddo
!
          if (iplast == 1) then

        call szsvm_or( &
        jr0,     js0,     jt0,     cc,   &
        cg,      g33,     fhour,   sigy, &
        sigold,  nuu,      smo1,    smo2, &
        nel,     iint)
          end if
!
     ! -----------for energy calculation------------
     if (jlag == 1) then
        call gfhour_or(1,nel,&
        fhour,jr0,js0,jt0,fcl,&
        hgx1, hgx2, hgx3, hgx4,&
        hgy1, hgy2, hgy3, hgy4,&
        hgz1, hgz2, hgz3, hgz4,&
        h11 , h22 , h33 , &
        h12 , h13 , h23 , &
        jr_1,js_1 , jt_1, nu4,nu2 ,&
        cc  ,cg   ,g33  ,nfhour,nel)

            do i = 1, nel
              deint(i) =  nfhour(i,3,1)*hgz1(i) + nfhour(i,3,2)*hgz2(i) + &
                nfhour(i,3,3)*hgz3(i) + nfhour(i,3,4)*hgz4(i) + &
                nfhour(i,1,1)*hgx1(i) + nfhour(i,1,2)*hgx2(i) + &
                nfhour(i,1,3)*hgx3(i) + nfhour(i,1,4)*hgx4(i) + &
                nfhour(i,2,1)*hgy1(i) + nfhour(i,2,2)*hgy2(i) + &
                nfhour(i,2,3)*hgy3(i) + nfhour(i,2,4)*hgy4(i)
              eint(i) = eint(i) +  half*dt1*deint(i)/max(em20, vol0(i))
            enddo
          endif

          if (iet > 1 .and. mtn == 24 ) then
            call mdama24(elbuf_str,1,nel ,pm    ,mat    ,dama_g )
            do j=1,3
              do i=1,nel
                fac1=one- dama_g(i,j)
                fhour(i,j,1:4) = fhour(i,j,1:4)*fac1
              enddo
            enddo
          end if !(iet > 1) then

          do i=1,nel
!
            e_r =g_3dt(i)*jr_1(i)
            e_s =g_3dt(i)*js_1(i)
            e_t =g_3dt(i)*jt_1(i)
!
            dfhour(i,1,1) = e_r*hgx1(i)
            dfhour(i,1,2) = e_r*hgx2(i)
            dfhour(i,1,3) = e_r*hgx3(i)
            dfhour(i,1,4) = e_r*hgx4(i)
!
            dfhour(i,2,1) = e_s*hgy1(i)
            dfhour(i,2,2) = e_s*hgy2(i)
            dfhour(i,2,3) = e_s*hgy3(i)
            dfhour(i,2,4) = e_s*hgy4(i)
!
            dfhour(i,3,1) = e_t*hgz1(i)
            dfhour(i,3,2) = e_t*hgz2(i)
            dfhour(i,3,3) = e_t*hgz3(i)
            dfhour(i,3,4) = e_t*hgz4(i)
!
            fhour(i,1,1) = fhour(i,1,1) + dfhour(i,1,1)
            fhour(i,1,2) = fhour(i,1,2) + dfhour(i,1,2)
            fhour(i,1,3) = fhour(i,1,3) + dfhour(i,1,3)
            fhour(i,1,4) = fhour(i,1,4) + dfhour(i,1,4)
            fhour(i,2,1) = fhour(i,2,1) + dfhour(i,2,1)
            fhour(i,2,2) = fhour(i,2,2) + dfhour(i,2,2)
            fhour(i,2,3) = fhour(i,2,3) + dfhour(i,2,3)
            fhour(i,2,4) = fhour(i,2,4) + dfhour(i,2,4)
            fhour(i,3,1) = fhour(i,3,1) + dfhour(i,3,1)
            fhour(i,3,2) = fhour(i,3,2) + dfhour(i,3,2)
            fhour(i,3,3) = fhour(i,3,3) + dfhour(i,3,3)
            fhour(i,3,4) = fhour(i,3,4) + dfhour(i,3,4)
!
          enddo
!
          if (iplast == 1) then
        call szsvm_or(   &
        jr0,     js0,     jt0,     cc, &
        cg,      g33,     fhour,   sigy, &
        sig0,    nuu,      sm1,     sm2,&
        nel,     iint)
          end if
!
          if (iplast == 1) then
            do i = 1, nel
              if (sm1(i) > sigy(i) .and. deint(i) > zero) then
                smo = zep9*smo1(i) + em01*smo2(i)
                fac1 = sigy(i) - smo
                fac2 = sm1(i) - smo
                if (fac2 <= em20) then
                  fac = zero
                else
                  fac = one - max(em20, fac1/fac2)
                end if
                if (sm2(i) < sigy(i)) then
                  fac1 = (sm1(i) - sigy(i))/max((sm1(i) - sm2(i)), em20)
                  fac1 = half + sqrt(fac1)
                  fac = min(fac1, one)*fac
                end if
                fhour(i,1,1) = fhour(i,1,1) - fac*dfhour(i,1,1)
                fhour(i,1,2) = fhour(i,1,2) - fac*dfhour(i,1,2)
                fhour(i,1,3) = fhour(i,1,3) - fac*dfhour(i,1,3)
                fhour(i,1,4) = fhour(i,1,4) - fac*dfhour(i,1,4)
                fhour(i,2,1) = fhour(i,2,1) - fac*dfhour(i,2,1)
                fhour(i,2,2) = fhour(i,2,2) - fac*dfhour(i,2,2)
                fhour(i,2,3) = fhour(i,2,3) - fac*dfhour(i,2,3)
                fhour(i,2,4) = fhour(i,2,4) - fac*dfhour(i,2,4)
                fhour(i,3,1) = fhour(i,3,1) - fac*dfhour(i,3,1)
                fhour(i,3,2) = fhour(i,3,2) - fac*dfhour(i,3,2)
                fhour(i,3,3) = fhour(i,3,3) - fac*dfhour(i,3,3)
                fhour(i,3,4) = fhour(i,3,4) - fac*dfhour(i,3,4)
              end if
            end do
          end if



    call gfhour_or(1,nel,&
        fhour,jr0,js0,jt0,fcl,&
        hgx1, hgx2, hgx3, hgx4,&
        hgy1, hgy2, hgy3, hgy4,&
        hgz1, hgz2, hgz3, hgz4,&
        h11 , h22 , h33 , &
        h12 , h13 , h23 , &
        jr_1,js_1 , jt_1, nu4,nu2 ,&
        cc  ,cg   ,g33  ,nfhour,nel)

!
          do i=1,nel
            hq13p = (nfhour(i,1,1)+nfhour(i,1,3))*one_over_8
            hq13n = (nfhour(i,1,1)-nfhour(i,1,3))*one_over_8
            hq24p = (nfhour(i,1,2)+nfhour(i,1,4))*one_over_8
            hq24n = (nfhour(i,1,2)-nfhour(i,1,4))*one_over_8
            ff =-px1h1(i)*nfhour(i,1,1)-px1h2(i)*nfhour(i,1,2) &
              -px1h3(i)*nfhour(i,1,3)-px1h4(i)*nfhour(i,1,4)
            f11_hgl(i) =-(hq13p+hq24n+ff)
            f17_hgl(i) =-(hq13p+hq24p-ff)
            ff =-px2h1(i)*nfhour(i,1,1)-px2h2(i)*nfhour(i,1,2) &
              -px2h3(i)*nfhour(i,1,3)-px2h4(i)*nfhour(i,1,4)
            f12_hgl(i) =-(hq13n-hq24n+ff)
            f18_hgl(i) =-(hq13n-hq24p-ff)
            ff =-px3h1(i)*nfhour(i,1,1)-px3h2(i)*nfhour(i,1,2) &
              -px3h3(i)*nfhour(i,1,3)-px3h4(i)*nfhour(i,1,4)
            f13_hgl(i) =-(-hq13n-hq24p+ff)
            f15_hgl(i) =-(-hq13n-hq24n-ff)
            ff =-px4h1(i)*nfhour(i,1,1)-px4h2(i)*nfhour(i,1,2) &
              -px4h3(i)*nfhour(i,1,3)-px4h4(i)*nfhour(i,1,4)
            f14_hgl(i) =-(-hq13p+hq24p+ff)
            f16_hgl(i) =-(-hq13p+hq24n-ff)
          end do
          do i=1,nel
            hq13p = (nfhour(i,2,1)+nfhour(i,2,3))*one_over_8
            hq13n = (nfhour(i,2,1)-nfhour(i,2,3))*one_over_8
            hq24p = (nfhour(i,2,2)+nfhour(i,2,4))*one_over_8
            hq24n = (nfhour(i,2,2)-nfhour(i,2,4))*one_over_8
            ff =-px1h1(i)*nfhour(i,2,1)-px1h2(i)*nfhour(i,2,2) &
              -px1h3(i)*nfhour(i,2,3)-px1h4(i)*nfhour(i,2,4)
            f21_hgl(i) =-(hq13p+hq24n+ff)
            f27_hgl(i) =-(hq13p+hq24p-ff)
            ff =-px2h1(i)*nfhour(i,2,1)-px2h2(i)*nfhour(i,2,2) &
              -px2h3(i)*nfhour(i,2,3)-px2h4(i)*nfhour(i,2,4)
            f22_hgl(i) =-(hq13n-hq24n+ff)
            f28_hgl(i) =-(hq13n-hq24p-ff)
            ff =-px3h1(i)*nfhour(i,2,1)-px3h2(i)*nfhour(i,2,2) &
              -px3h3(i)*nfhour(i,2,3)-px3h4(i)*nfhour(i,2,4)
            f23_hgl(i) =-(-hq13n-hq24p+ff)
            f25_hgl(i) =-(-hq13n-hq24n-ff)
            ff =-px4h1(i)*nfhour(i,2,1)-px4h2(i)*nfhour(i,2,2) &
              -px4h3(i)*nfhour(i,2,3)-px4h4(i)*nfhour(i,2,4)
            f24_hgl(i) =-(-hq13p+hq24p+ff)
            f26_hgl(i) =-(-hq13p+hq24n-ff)
          end do
          do i=1,nel
            hq13p = (nfhour(i,3,1)+nfhour(i,3,3))*one_over_8
            hq13n = (nfhour(i,3,1)-nfhour(i,3,3))*one_over_8
            hq24p = (nfhour(i,3,2)+nfhour(i,3,4))*one_over_8
            hq24n = (nfhour(i,3,2)-nfhour(i,3,4))*one_over_8
            ff =-px1h1(i)*nfhour(i,3,1)-px1h2(i)*nfhour(i,3,2) &
              -px1h3(i)*nfhour(i,3,3)-px1h4(i)*nfhour(i,3,4)
            f31_hgl(i) =-(hq13p+hq24n+ff)
            f37_hgl(i) =-(hq13p+hq24p-ff)
            ff =-px2h1(i)*nfhour(i,3,1)-px2h2(i)*nfhour(i,3,2) &
              -px2h3(i)*nfhour(i,3,3)-px2h4(i)*nfhour(i,3,4)
            f32_hgl(i) =-(hq13n-hq24n+ff)
            f38_hgl(i) =-(hq13n-hq24p-ff)
            ff =-px3h1(i)*nfhour(i,3,1)-px3h2(i)*nfhour(i,3,2) &
              -px3h3(i)*nfhour(i,3,3)-px3h4(i)*nfhour(i,3,4)
            f33_hgl(i) =-(-hq13n-hq24p+ff)
            f35_hgl(i) =-(-hq13n-hq24n-ff)
            ff =-px4h1(i)*nfhour(i,3,1)-px4h2(i)*nfhour(i,3,2) &
              -px4h3(i)*nfhour(i,3,3)-px4h4(i)*nfhour(i,3,4)
            f34_hgl(i) =-(-hq13p+hq24p+ff)
            f36_hgl(i) =-(-hq13p+hq24n-ff)
          enddo
!c------------------------------------------------
!c
      do i=1,nel


        f11(i) = f11(i) + f11_hgl(i)    
        f12(i) = f12(i) + f12_hgl(i)
        f13(i) = f13(i) + f13_hgl(i) + f14_hgl(i) ! f14                
        f15(i) = f15(i) + f15_hgl(i) 
        f16(i) = f16(i) + f16_hgl(i)
        f17(i) = f17(i) + f17_hgl(i) + f18_hgl(i) ! f18             
        f21(i) = f21(i) + f21_hgl(i)
        f22(i) = f22(i) + f22_hgl(i)
        f23(i) = f23(i) + f23_hgl(i) + f24_hgl(i) ! f24               
        f25(i) = f25(i) + f25_hgl(i) 
        f26(i) = f26(i) + f26_hgl(i)
        f27(i) = f27(i) + f27_hgl(i) + f28_hgl(i) ! f28
        f31(i) = f31(i) + f31_hgl(i)
        f32(i) = f32(i) + f32_hgl(i)
        f33(i) = f33(i) + f33_hgl(i) + f34_hgl(i) ! f34          
        f35(i) = f35(i) + f35_hgl(i)
        f36(i) = f36(i) + f36_hgl(i)
        f37(i) = f37(i) + f37_hgl(i) + f38_hgl(i) ! f38
      end do

          !----hourglass energy is included in internal energy------
          if (jlag == 1) then
            do i = 1, nel
              eint(i) = eint(i) + half*dt1*( &
                nfhour(i,3,1)*hgz1(i) + nfhour(i,3,2)*hgz2(i) + &
                nfhour(i,3,3)*hgz3(i) + nfhour(i,3,4)*hgz4(i) + &
                nfhour(i,1,1)*hgx1(i) + nfhour(i,1,2)*hgx2(i) + &
                nfhour(i,1,3)*hgx3(i) + nfhour(i,1,4)*hgx4(i) + &
                nfhour(i,2,1)*hgy1(i) + nfhour(i,2,2)*hgy2(i) + &
                nfhour(i,2,3)*hgy3(i) + nfhour(i,2,4)*hgy4(i)) &
                /max(em20, vol0(i))
            end do
          end if
        end subroutine s6zhour3_or
      end module s6zhour3_or_mod
