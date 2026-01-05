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
!||    s6zhour3_mod   ../engine/source/elements/solid/solide6z/s6zhourg3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zhour3_mod
      contains
        ! ======================================================================================================================
        ! \brief Compute hourglass forces for 6-node solid elements
        ! \details This subroutine calculates hourglass control forces for 6-node solid elements
        !          to prevent spurious zero-energy modes. It computes the hourglass forces and
        !          updates the nodal forces and internal energy.
        ! ======================================================================================================================
!||====================================================================
!||    s6zhour3        ../engine/source/elements/solid/solide6z/s6zhourg3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- calls      -----------------------------------------------------
!||    mdama24         ../engine/source/elements/solid/solidez/mdama24.F
!||    szsvm           ../engine/source/elements/solid/solidez/szsvm.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6zhour3(                                                     &
          npropm  , nummat , pm      , rho     , vol     , ssp     ,             &
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
          mat     , npropg , numgeo  , geo     , pid     , dt1     ,             &
          elbuf_str,iint   , jlag    , mtn     , sigy    , sig0    ,             &
          sigold  )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod, only : zero, one, two, third, fourth, half, one_over_8,&
            one_over_64, two_third, em20, zep00666666667,em01,zep9
          use elbufdef_mod
          use mvsiz_mod, only : mvsiz
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
! ------------------------------------------------------------------------------
          integer,                     intent(in)      :: npropm      !< Number of material property columns
          integer,                     intent(in)      :: nummat      !< Number of materials
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm  !< Material properties
          real(kind=WP), dimension(nel), intent(in)    :: rho         !< Density
          real(kind=WP), dimension(mvsiz), intent(in)  :: vol         !< Current volume
          real(kind=WP), dimension(mvsiz), intent(in)  :: ssp         !< Sound speed
!C    FORCE WP = 8 TO ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8), dimension(mvsiz), intent(in)  :: x1i         !< X-coordinate of node 1
          real(kind=8), dimension(mvsiz), intent(in)  :: x2i         !< X-coordinate of node 2
          real(kind=8), dimension(mvsiz), intent(in)  :: x3i         !< X-coordinate of node 3
          real(kind=8), dimension(mvsiz), intent(in)  :: x4i         !< X-coordinate of node 4
          real(kind=8), dimension(mvsiz), intent(in)  :: x5i         !< X-coordinate of node 5
          real(kind=8), dimension(mvsiz), intent(in)  :: x6i         !< X-coordinate of node 6
          real(kind=8), dimension(mvsiz), intent(in)  :: y1i         !< Y-coordinate of node 1
          real(kind=8), dimension(mvsiz), intent(in)  :: y2i         !< Y-coordinate of node 2
          real(kind=8), dimension(mvsiz), intent(in)  :: y3i         !< Y-coordinate of node 3
          real(kind=8), dimension(mvsiz), intent(in)  :: y4i         !< Y-coordinate of node 4
          real(kind=8), dimension(mvsiz), intent(in)  :: y5i         !< Y-coordinate of node 5
          real(kind=8), dimension(mvsiz), intent(in)  :: y6i         !< Y-coordinate of node 6
          real(kind=8), dimension(mvsiz), intent(in)  :: z1i         !< Z-coordinate of node 1
          real(kind=8), dimension(mvsiz), intent(in)  :: z2i         !< Z-coordinate of node 2
          real(kind=8), dimension(mvsiz), intent(in)  :: z3i         !< Z-coordinate of node 3
          real(kind=8), dimension(mvsiz), intent(in)  :: z4i         !< Z-coordinate of node 4
          real(kind=8), dimension(mvsiz), intent(in)  :: z5i         !< Z-coordinate of node 5
          real(kind=8), dimension(mvsiz), intent(in)  :: z6i         !< Z-coordinate of node 6
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx1i        !< X-velocity of node 1
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx2i        !< X-velocity of node 2
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx3i        !< X-velocity of node 3
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx4i        !< X-velocity of node 4
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx5i        !< X-velocity of node 5
          real(kind=WP), dimension(mvsiz), intent(in)  :: vx6i        !< X-velocity of node 6
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy1i        !< Y-velocity of node 1
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy2i        !< Y-velocity of node 2
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy3i        !< Y-velocity of node 3
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy4i        !< Y-velocity of node 4
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy5i        !< Y-velocity of node 5
          real(kind=WP), dimension(mvsiz), intent(in)  :: vy6i        !< Y-velocity of node 6
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz1i        !< Z-velocity of node 1
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz2i        !< Z-velocity of node 2
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz3i        !< Z-velocity of node 3
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz4i        !< Z-velocity of node 4
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz5i        !< Z-velocity of node 5
          real(kind=WP), dimension(mvsiz), intent(in)  :: vz6i        !< Z-velocity of node 6
          real(kind=WP), dimension(nel), intent(inout) :: f11         !< X-force at node 1
          real(kind=WP), dimension(nel), intent(inout) :: f12         !< X-force at node 2
          real(kind=WP), dimension(nel), intent(inout) :: f13         !< X-force at node 3
          real(kind=WP), dimension(nel), intent(inout) :: f15         !< X-force at node 5
          real(kind=WP), dimension(nel), intent(inout) :: f16         !< X-force at node 6
          real(kind=WP), dimension(nel), intent(inout) :: f17         !< X-force at node 7
          real(kind=WP), dimension(nel), intent(inout) :: f21         !< Y-force at node 1
          real(kind=WP), dimension(nel), intent(inout) :: f22         !< Y-force at node 2
          real(kind=WP), dimension(nel), intent(inout) :: f23         !< Y-force at node 3
          real(kind=WP), dimension(nel), intent(inout) :: f25         !< Y-force at node 5
          real(kind=WP), dimension(nel), intent(inout) :: f26         !< Y-force at node 6
          real(kind=WP), dimension(nel), intent(inout) :: f27         !< Y-force at node 7
          real(kind=WP), dimension(nel), intent(inout) :: f31         !< Z-force at node 1
          real(kind=WP), dimension(nel), intent(inout) :: f32         !< Z-force at node 2
          real(kind=WP), dimension(nel), intent(inout) :: f33         !< Z-force at node 3
          real(kind=WP), dimension(nel), intent(inout) :: f35         !< Z-force at node 5
          real(kind=WP), dimension(nel), intent(inout) :: f36         !< Z-force at node 6
          real(kind=WP), dimension(nel), intent(inout) :: f37         !< Z-force at node 7
          real(kind=WP), dimension(mvsiz), intent(in)  :: nu          !< Poisson's ratio
          real(kind=WP), dimension(nel,3,4), intent(inout) :: fhour   !< Hourglass forces
          real(kind=WP), dimension(mvsiz), intent(in)  :: off         !< Element activation flag
          real(kind=WP), dimension(nel), intent(in)    :: vol0        !< Initial volume
          real(kind=WP), dimension(nel), intent(inout) :: eint        !< Internal energy
          integer,                       intent(in)    :: nel         !< Number of elements
          integer,     dimension(mvsiz), intent(in)    :: mat         !< Material identifiers
          integer,                       intent(in)    :: npropg      !< Number of property group columns
          integer,                       intent(in)    :: numgeo      !< Number of geometric property columns
          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo  !< Geometry properties
          integer,                       intent(in)    :: pid         !< Property identifiers
          real(kind=WP),                 intent(in)    :: dt1         !< Time step
          type(elbuf_struct_),                  target :: elbuf_str   !< Element buffer structure
          integer,                       intent(in)    :: iint        !< Integration flag
          integer,                       intent(in)    :: jlag        !< Energy calculation flag
          integer,                       intent(in)    :: mtn         !< Material type number
          real(kind=WP),dimension(mvsiz),intent(inout) :: sigy        !< Yield stress
          real(kind=WP),dimension(nel,6),intent(inout) :: sig0        !< Current stress
          real(kind=WP),dimension(nel,6),intent(inout) :: sigold      !< Previous stress
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, j, mx, mt,iplast, iet
          real(kind=WP) :: dett,fac,fac1,fac2,smo
          real(kind=WP) :: jaci1, jaci2, jaci3, jaci4, jaci5, jaci6, jaci7, jaci8, jaci9
          real(kind=WP) :: jaci12, jaci45, jaci78
          real(kind=WP) :: x_17_46, x_28_35, y_17_46, y_28_35, z_17_46, z_28_35
          real(kind=WP) :: hx, hy, hz, h1x, h1y, h1z, h2x, h2y, h2z, h3x, h3y, h3z, h4x, h4y, h4z
          real(kind=WP) :: rho0, g0, c1, nuu
          real(kind=WP) :: vx3478, vx2358, vx1467, vx1256
          real(kind=WP) :: vy3478, vy2358, vy1467, vy1256
          real(kind=WP) :: vz3478, vz2358, vz1467, vz1256
          real(kind=WP) :: vx17, vy17, vx28, vy28, vx35, vy35, vx46, vy46
          real(kind=WP) :: vz17, vz28, vz35, vz46
          real(kind=WP) :: e_r, e_s, e_t
          real(kind=WP) :: hq13p, hq13n, hq24p, hq24n, ff
          real(kind=WP) :: dama_g(mvsiz,6)
!C    FORCE WP = 8 TO ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8), dimension(nel) :: x1, x2, x3, x4, x5, x6, x7, x8
          real(kind=8), dimension(nel) :: y1, y2, y3, y4, y5, y6, y7, y8
          real(kind=8), dimension(nel) :: z1, z2, z3, z4, z5, z6, z7, z8
          real(kind=WP), dimension(nel) :: vx1, vx2, vx3, vx4, vx5, vx6, vx7, vx8
          real(kind=WP), dimension(nel) :: vy1, vy2, vy3, vy4, vy5, vy6, vy7, vy8
          real(kind=WP), dimension(nel) :: vz1, vz2, vz3, vz4, vz5, vz6, vz7, vz8
!C    FORCE WP = 8 TO ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8), dimension(nel) :: x17, x28, x35, x46
          real(kind=8), dimension(nel) :: y17, y28, y35, y46
          real(kind=8), dimension(nel) :: z17, z28, z35, z46
          real(kind=WP), dimension(nel) :: jac_59_68, jac_67_49, jac_48_57, jac_19_37
          real(kind=WP), dimension(nel) :: px1, px2, px3, px4
          real(kind=WP), dimension(nel) :: py1, py2, py3, py4
          real(kind=WP), dimension(nel) :: pz1, pz2, pz3, pz4
          real(kind=WP), dimension(nel) :: px1h1, px2h1, px3h1, px4h1
          real(kind=WP), dimension(nel) :: px1h2, px2h2, px3h2, px4h2
          real(kind=WP), dimension(nel) :: px1h3, px2h3, px3h3, px4h3
          real(kind=WP), dimension(nel) :: px1h4, px2h4, px3h4, px4h4
          real(kind=WP), dimension(nel) :: jac1, jac2, jac3, jac4, jac5, jac6, jac7, jac8, jac9
!C    FORCE WP = 8 TO ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8), dimension(nel) :: det
          real(kind=WP), dimension(nel) :: fcl, hgz1, hgz2
          real(kind=WP), dimension(nel) :: gg
          real(kind=WP), dimension(nel) :: cxx, caq
          real(kind=WP), dimension(nel) :: g_3dt, e0
          real(kind=WP), dimension(nel) :: nu1, nu2, nu3, nu4
          real(kind=WP), dimension(nel) :: hgx1, hgx2, hgx3, hgx4
          real(kind=WP), dimension(nel) :: hgy1, hgy2, hgy3, hgy4
          real(kind=WP), dimension(nel) :: hgz3, hgz4
          real(kind=WP), dimension(nel) :: jr_1, js_1, jt_1
          real(kind=WP), dimension(nel) :: jr0, js0, jt0
          real(kind=WP), dimension(nel) :: h11, h22, h33, h12, h13, h23
          real(kind=WP), dimension(3,4) :: fhourt



          real(kind=WP), dimension(nel) :: f11_hgl, f12_hgl, f13_hgl, f14_hgl, f15_hgl, f16_hgl
          real(kind=WP), dimension(nel) :: f17_hgl, f18_hgl
          real(kind=WP), dimension(nel) :: f21_hgl, f22_hgl, f23_hgl, f24_hgl, f25_hgl, f26_hgl
          real(kind=WP), dimension(nel) :: f27_hgl, f28_hgl
          real(kind=WP), dimension(nel) :: f31_hgl, f32_hgl, f33_hgl, f34_hgl, f35_hgl, f36_hgl
          real(kind=WP), dimension(nel) :: f37_hgl, f38_hgl
          real(kind=WP), dimension(nel) :: deint,sm1,sm2,smo1,smo2
          real(kind=WP), dimension(nel,3,4) :: dfhour, nfhour
!

!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
!
          !-------------------------------------------------------------------------
          !< Translation from 6 nodes penta to 8 nodes degenerated hex equivalence
          !-------------------------------------------------------------------------
          !< Nodal X coordinates
          x1(1:nel) = x1i(1:nel)
          x2(1:nel) = x2i(1:nel)
          x3(1:nel) = x3i(1:nel)
          x4(1:nel) = x3i(1:nel)
          x5(1:nel) = x4i(1:nel)
          x6(1:nel) = x5i(1:nel)
          x7(1:nel) = x6i(1:nel)
          x8(1:nel) = x6i(1:nel)
          !< Nodal Y coordinates
          y1(1:nel) = y1i(1:nel)
          y2(1:nel) = y2i(1:nel)
          y3(1:nel) = y3i(1:nel)
          y4(1:nel) = y3i(1:nel)
          y5(1:nel) = y4i(1:nel)
          y6(1:nel) = y5i(1:nel)
          y7(1:nel) = y6i(1:nel)
          y8(1:nel) = y6i(1:nel)
          !< Nodal Z coordinates
          z1(1:nel) = z1i(1:nel)
          z2(1:nel) = z2i(1:nel)
          z3(1:nel) = z3i(1:nel)
          z4(1:nel) = z3i(1:nel)
          z5(1:nel) = z4i(1:nel)
          z6(1:nel) = z5i(1:nel)
          z7(1:nel) = z6i(1:nel)
          z8(1:nel) = z6i(1:nel)
          !< Nodal X velocities
          vx1(1:nel) = vx1i(1:nel)
          vx2(1:nel) = vx2i(1:nel)
          vx3(1:nel) = vx3i(1:nel)
          vx4(1:nel) = vx3i(1:nel)
          vx5(1:nel) = vx4i(1:nel)
          vx6(1:nel) = vx5i(1:nel)
          vx7(1:nel) = vx6i(1:nel)
          vx8(1:nel) = vx6i(1:nel)
          !< Nodal Y velocities
          vy1(1:nel) = vy1i(1:nel)
          vy2(1:nel) = vy2i(1:nel)
          vy3(1:nel) = vy3i(1:nel)
          vy4(1:nel) = vy3i(1:nel)
          vy5(1:nel) = vy4i(1:nel)
          vy6(1:nel) = vy5i(1:nel)
          vy7(1:nel) = vy6i(1:nel)
          vy8(1:nel) = vy6i(1:nel)
          !< Nodal Z velocities
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
          !< Recover material parameters
          !-------------------------------------------------------------------------
          mx   = mat(1)                              !< Material index
          rho0 = pm(1,mx)                            !< Initial density
          nuu  = pm(21,mx)                           !< Poisson's ratio
          g0   = pm(22,mx)                           !< Shear modulus
          c1   = pm(32,mx)                           !< Elastic matrix diagonal term
          iplast = elbuf_str%gbuf%g_pla              !< Plasticity flag
          iet  = iint
          do i=1,nel
            cxx(i) = ssp(i)
            gg(i)=half*rho0*cxx(i)*cxx(i)*(one -two*nuu)/(one-nuu)
          end do
!
          !-------------------------------------------------------------------------
          !< Recover properties parameters
          !-------------------------------------------------------------------------
          mt = pid
          do i=1,nel
            caq(i)=fourth*off(i)*geo(13,mt)
          end do

          do i=1,nel
            g_3dt(i)=third*off(i)*gg(i)*dt1
            e0(i)=two*(one+nu(i))*gg(i)
          enddo

          do i=1,nel
            nu1(i) =two/(one-nuu)
            nu2(i) =nuu*nu1(i)
            nu3(i) =two_third*(one + nuu)
            nu4(i) =nuu
          enddo

          do i=1,nel
            fcl(i)=caq(i)*rho(i)*vol(i)**third
            fcl(i)=zep00666666667*fcl(i)*cxx(i)
          enddo
!
          !-------------------------------------------------------------------------
          !< Nodal coordinate differences
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
          !< Compute the Jacobian matrix
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
          !< Compute the inverse Jacobian matrix
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

!C!    H1
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

!C   H2
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

!C   H4
!C -1 1 -1 1 1 -1 1 -1
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
!C   alpha =1 ->eta zeta
!C 1 1 -1 -1 -1 -1 1 1
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
            call szsvm( &
              jr0, js0, jt0, fhour, &
              sigy, sigold, nu4, smo1, &
              smo2, nel, iint)
          end if
!
          ! -----------For energy calculation------------
          if (jlag == 1) then
            do i = 1, nel
              fhourt(1,1) = fhour(i,1,1)*jr0(i) + fcl(i)*hgx1(i)
              fhourt(1,2) = fhour(i,1,2)*jr0(i) + fcl(i)*hgx2(i)
              fhourt(1,3) = fhour(i,1,3)*jr0(i) + fcl(i)*hgx3(i)
              fhourt(1,4) = fhour(i,1,4)*jr0(i) + fcl(i)*hgx4(i)
              fhourt(2,1) = fhour(i,2,1)*js0(i) + fcl(i)*hgy1(i)
              fhourt(2,2) = fhour(i,2,2)*js0(i) + fcl(i)*hgy2(i)
              fhourt(2,3) = fhour(i,2,3)*js0(i) + fcl(i)*hgy3(i)
              fhourt(2,4) = fhour(i,2,4)*js0(i) + fcl(i)*hgy4(i)
              fhourt(3,1) = fhour(i,3,1)*jt0(i) + fcl(i)*hgz1(i)
              fhourt(3,2) = fhour(i,3,2)*jt0(i) + fcl(i)*hgz2(i)
              fhourt(3,3) = fhour(i,3,3)*jt0(i) + fcl(i)*hgz3(i)
              fhourt(3,4) = fhour(i,3,4)*jt0(i) + fcl(i)*hgz4(i)

              nfhour(i,1,1) = (h22(i) + h33(i))*fhourt(1,1) &
                + h12(i)*fhourt(2,2) + h13(i)*fhourt(3,3)
              nfhour(i,2,2) = (h11(i) + h33(i))*fhourt(2,2) &
                + h23(i)*fhourt(3,3) + h12(i)*fhourt(1,1)
              nfhour(i,3,3) = (h11(i) + h22(i))*fhourt(3,3) &
                + h13(i)*fhourt(1,1) + h23(i)*fhourt(2,2)
              nfhour(i,1,2) = nu1(i)*h11(i)*fhourt(1,2) &
                + nu2(i)*h12(i)*fhourt(2,1)
              nfhour(i,1,3) = nu1(i)*h11(i)*fhourt(1,3) &
                + nu2(i)*h13(i)*fhourt(3,1)
              nfhour(i,2,1) = nu1(i)*h22(i)*fhourt(2,1) &
                + nu2(i)*h12(i)*fhourt(1,2)
              nfhour(i,3,1) = nu1(i)*h33(i)*fhourt(3,1) &
                + nu2(i)*h13(i)*fhourt(1,3)

              nfhour(i,2,3) = nu1(i)*h22(i)*fhourt(2,3) &
                + nu2(i)*h23(i)*fhourt(3,2)
              nfhour(i,3,2) = nu1(i)*h33(i)*fhourt(3,2) &
                + nu2(i)*h23(i)*fhourt(2,3)
              nfhour(i,1,4) = nu3(i)*h11(i)*fhourt(1,4)
              nfhour(i,2,4) = nu3(i)*h22(i)*fhourt(2,4)
              nfhour(i,3,4) = nu3(i)*h33(i)*fhourt(3,4)
            end do

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
            call szsvm( &
              jr0, js0, jt0, fhour, &
              sigy, sig0, nu4, sm1, &
              sm2, nel, iint)
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

          do i=1,nel
            fhourt(1,1) = fhour(i,1,1)*jr0(i)+fcl(i)*hgx1(i)
            fhourt(1,2) = fhour(i,1,2)*jr0(i)+fcl(i)*hgx2(i)
            fhourt(1,3) = fhour(i,1,3)*jr0(i)+fcl(i)*hgx3(i)
            fhourt(1,4) = fhour(i,1,4)*jr0(i)+fcl(i)*hgx4(i)
            fhourt(2,1) = fhour(i,2,1)*js0(i)+fcl(i)*hgy1(i)
            fhourt(2,2) = fhour(i,2,2)*js0(i)+fcl(i)*hgy2(i)
            fhourt(2,3) = fhour(i,2,3)*js0(i)+fcl(i)*hgy3(i)
            fhourt(2,4) = fhour(i,2,4)*js0(i)+fcl(i)*hgy4(i)
            fhourt(3,1) = fhour(i,3,1)*jt0(i)+fcl(i)*hgz1(i)
            fhourt(3,2) = fhour(i,3,2)*jt0(i)+fcl(i)*hgz2(i)
            fhourt(3,3) = fhour(i,3,3)*jt0(i)+fcl(i)*hgz3(i)
            fhourt(3,4) = fhour(i,3,4)*jt0(i)+fcl(i)*hgz4(i)

!C NFHX1 NFHOUR(I,3(XYZ),4(1,2,3,4))
!
!      !nfhour(i,1,1) nfhx1(i)
            nfhour(i,1,1) = (h22(i)+h33(i))*fhourt(1,1) &
              +h12(i)*fhourt(2,2)+h13(i)*fhourt(3,3)
            !nfhour(i,2,2) nfhy2(i)
            nfhour(i,2,2) = (h11(i)+h33(i))*fhourt(2,2) &
              +h23(i)*fhourt(3,3)+h12(i)*fhourt(1,1)
            !nfhour(i,3,3) nfhz3(i)
            nfhour(i,3,3) = (h11(i)+h22(i))*fhourt(3,3) &
              +h13(i)*fhourt(1,1)+h23(i)*fhourt(2,2)
            !nfhour(i,1,2) nfhx2(i)
            nfhour(i,1,2) = nu1(i)*h11(i)*fhourt(1,2) &
              +nu2(i)*h12(i)*fhourt(2,1)
            !nfhour(i,1,3) nfhx3(i)
            nfhour(i,1,3) = nu1(i)*h11(i)*fhourt(1,3) &
              +nu2(i)*h13(i)*fhourt(3,1)
            !nfhour(i,2,1) nfhy1(i)
            nfhour(i,2,1) = nu1(i)*h22(i)*fhourt(2,1) &
              +nu2(i)*h12(i)*fhourt(1,2)
            !nfhour(i,3,1)  nfhz1(i) nfhz1(i) = cc*fhour(i,1) + fcl(i)*hgz1(i)
            nfhour(i,3,1) = nu1(i)*h33(i)*fhourt(3,1) &
              +nu2(i)*h13(i)*fhourt(1,3)
            !nfhour(i,2,3) nfhy3(i)
            nfhour(i,2,3) = nu1(i)*h22(i)*fhourt(2,3) &
              +nu2(i)*h23(i)*fhourt(3,2)
            !nfhour(i,3,2)  nfhz2(i) nfhz2(i) = cc*fhour(i,2) + fcl(i)*hgz2(i)
            nfhour(i,3,2)= nu1(i)*h33(i)*fhourt(3,2) &
              +nu2(i)*h23(i)*fhourt(2,3)
            !nfhour(i,1,4) nfhx4(i)
            nfhour(i,1,4) = nu3(i)*h11(i)*fhourt(1,4)
            !nfhour(i,2,4) nfhy4(i)
            nfhour(i,2,4) = nu3(i)*h22(i)*fhourt(2,4)
            !nfhour(i,3,4)  nfhz4(i)
            nfhour(i,3,4) = nu3(i)*h33(i)*fhourt(3,4)

          end do
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
        end subroutine s6zhour3
      end module s6zhour3_mod
