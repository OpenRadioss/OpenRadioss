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
!||    s6zdefc3_mod   ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zdefc3_mod
      contains
      ! ======================================================================================================================
      ! \brief   Compute strain rates and hourglass control for 6-node solid elements
      ! \details Calculates strain rate tensor components (dxx, dyy, dzz, dxy, dxz, dyz)
      !          and hourglass control strain rates for 6-node thick shell/solid elements.
      !          Includes volumetric strain rate and spin rate calculations.
      ! ======================================================================================================================
!||====================================================================
!||    s6zdefc3        ../engine/source/elements/solid/solide6z/s6zdefc3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6zdefc3( &
        px1      , px2      , px3      , px4      , px5      , px6      ,      &
        py1      , py2      , py3      , py4      , py5      , py6      ,      &
        pz1      , pz2      , pz3      , pz4      , pz5      , pz6      ,      &
        vx1      , vx2      , vx3      , vx4      , vx5      , vx6      ,      &
        vy1      , vy2      , vy3      , vy4      , vy5      , vy6      ,      &
        vz1      , vz2      , vz3      , vz4      , vz5      , vz6      ,      &
        dxx      , dxy      , dxz      , dyx      , dyy      , dyz      ,      &
        dzx      , dzy      , dzz      , wxx      , wyy      , wzz      ,      &
        nel      )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
        use precision_mod, only : wp
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
        implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
        integer, intent(in) :: nel                          !< Number of elements
        real(kind=WP), dimension(nel), intent(in)  :: vx1   !< X-velocity at node 1
        real(kind=WP), dimension(nel), intent(in)  :: vx2   !< X-velocity at node 2
        real(kind=WP), dimension(nel), intent(in)  :: vx3   !< X-velocity at node 3
        real(kind=WP), dimension(nel), intent(in)  :: vx4   !< X-velocity at node 4
        real(kind=WP), dimension(nel), intent(in)  :: vx5   !< X-velocity at node 5
        real(kind=WP), dimension(nel), intent(in)  :: vx6   !< X-velocity at node 6
        real(kind=WP), dimension(nel), intent(in)  :: vy1   !< Y-velocity at node 1
        real(kind=WP), dimension(nel), intent(in)  :: vy2   !< Y-velocity at node 2
        real(kind=WP), dimension(nel), intent(in)  :: vy3   !< Y-velocity at node 3
        real(kind=WP), dimension(nel), intent(in)  :: vy4   !< Y-velocity at node 4
        real(kind=WP), dimension(nel), intent(in)  :: vy5   !< Y-velocity at node 5
        real(kind=WP), dimension(nel), intent(in)  :: vy6   !< Y-velocity at node 6
        real(kind=WP), dimension(nel), intent(in)  :: vz1   !< Z-velocity at node 1
        real(kind=WP), dimension(nel), intent(in)  :: vz2   !< Z-velocity at node 2
        real(kind=WP), dimension(nel), intent(in)  :: vz3   !< Z-velocity at node 3
        real(kind=WP), dimension(nel), intent(in)  :: vz4   !< Z-velocity at node 4
        real(kind=WP), dimension(nel), intent(in)  :: vz5   !< Z-velocity at node 5
        real(kind=WP), dimension(nel), intent(in)  :: vz6   !< Z-velocity at node 6
        real(kind=WP), dimension(nel), intent(in)  :: px1   !< Shape function derivative X1
        real(kind=WP), dimension(nel), intent(in)  :: px2   !< Shape function derivative X2
        real(kind=WP), dimension(nel), intent(in)  :: px3   !< Shape function derivative X3
        real(kind=WP), dimension(nel), intent(in)  :: px4   !< Shape function derivative X4
        real(kind=WP), dimension(nel), intent(in)  :: px5   !< Shape function derivative X5
        real(kind=WP), dimension(nel), intent(in)  :: px6   !< Shape function derivative X6
        real(kind=WP), dimension(nel), intent(in)  :: py1   !< Shape function derivative Y1
        real(kind=WP), dimension(nel), intent(in)  :: py2   !< Shape function derivative Y2
        real(kind=WP), dimension(nel), intent(in)  :: py3   !< Shape function derivative Y3
        real(kind=WP), dimension(nel), intent(in)  :: py4   !< Shape function derivative Y4
        real(kind=WP), dimension(nel), intent(in)  :: py5   !< Shape function derivative Y5
        real(kind=WP), dimension(nel), intent(in)  :: py6   !< Shape function derivative Y6
        real(kind=WP), dimension(nel), intent(in)  :: pz1   !< Shape function derivative Z1
        real(kind=WP), dimension(nel), intent(in)  :: pz2   !< Shape function derivative Z2
        real(kind=WP), dimension(nel), intent(in)  :: pz3   !< Shape function derivative Z3
        real(kind=WP), dimension(nel), intent(in)  :: pz4   !< Shape function derivative Z4
        real(kind=WP), dimension(nel), intent(in)  :: pz5   !< Shape function derivative Z5
        real(kind=WP), dimension(nel), intent(in)  :: pz6   !< Shape function derivative Z6
        real(kind=WP), dimension(nel), intent(out) :: dxx   !< Strain rate XX component
        real(kind=WP), dimension(nel), intent(out) :: dxy   !< Strain rate XY component
        real(kind=WP), dimension(nel), intent(out) :: dxz   !< Strain rate XZ component
        real(kind=WP), dimension(nel), intent(out) :: dyx   !< Strain rate YX component
        real(kind=WP), dimension(nel), intent(out) :: dyy   !< Strain rate YY component
        real(kind=WP), dimension(nel), intent(out) :: dyz   !< Strain rate YZ component
        real(kind=WP), dimension(nel), intent(out) :: dzx   !< Strain rate ZX component
        real(kind=WP), dimension(nel), intent(out) :: dzy   !< Strain rate ZY component
        real(kind=WP), dimension(nel), intent(out) :: dzz   !< Strain rate ZZ component
        real(kind=WP), dimension(nel), intent(out) :: wxx   !< Spin rate XX component
        real(kind=WP), dimension(nel), intent(out) :: wyy   !< Spin rate YY component
        real(kind=WP), dimension(nel), intent(out) :: wzz   !< Spin rate ZZ component
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
        integer :: i                                                        
        real(kind=WP) :: vx14(nel), vy14(nel), vz14(nel)
        real(kind=WP) :: vx25(nel), vy25(nel), vz25(nel)
        real(kind=WP) :: vx36(nel), vy36(nel), vz36(nel)
        real(kind=WP) :: vx14n(nel), vy14n(nel), vz14n(nel)
        real(kind=WP) :: vx25n(nel), vy25n(nel), vz25n(nel)
        real(kind=WP) :: vx36n(nel), vy36n(nel), vz36n(nel)
        real(kind=WP) :: vx3614n(nel), vy3614n(nel), vx2514n(nel), vy2514n(nel)
        real(kind=WP) :: vxhi(nel), vyhi(nel), vzhi(nel)
        real(kind=WP) :: nu1, pxsvx, pysvy, pzsvz, pxavx, pyavy, pzavz, termad
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
!
        !< Compute velocity combinations for finite element calculation
        do i = 1, nel
          vx14(i) = vx1(i) + vx4(i)
          vx25(i) = vx2(i) + vx5(i)
          vx36(i) = vx3(i) + vx6(i)
          vy14(i) = vy1(i) + vy4(i)
          vy25(i) = vy2(i) + vy5(i)
          vy36(i) = vy3(i) + vy6(i)
          vz14(i) = vz1(i) + vz4(i)
          vz25(i) = vz2(i) + vz5(i)
          vz36(i) = vz3(i) + vz6(i)
        enddo
!
        !< Initialize spin rates
        do i = 1, nel
          wxx(i) = 0.0_WP
          wyy(i) = 0.0_WP
          wzz(i) = 0.0_WP
        enddo
!
        !< Alternative strain rate calculation (direct node approach)
        do i = 1, nel
          dxx(i) = px1(i)*vx1(i) + px2(i)*vx2(i) + px5(i)*vx5(i) +             &
                   px3(i)*vx3(i) + px6(i)*vx6(i) + px4(i)*vx4(i)
          dyy(i) = py1(i)*vy1(i) + py2(i)*vy2(i) + py5(i)*vy5(i) +             &
                   py3(i)*vy3(i) + py6(i)*vy6(i) + py4(i)*vy4(i)
          dzz(i) = pz1(i)*vz1(i) + pz2(i)*vz2(i) + pz5(i)*vz5(i) +             &
                   pz3(i)*vz3(i) + pz6(i)*vz6(i) + pz4(i)*vz4(i)
          dxy(i) = py1(i)*vx1(i) + py2(i)*vx2(i) + py5(i)*vx5(i) +             &
                   py3(i)*vx3(i) + py6(i)*vx6(i) + py4(i)*vx4(i)
          dyx(i) = px1(i)*vy1(i) + px2(i)*vy2(i) + px5(i)*vy5(i) +             &
                   px3(i)*vy3(i) + px6(i)*vy6(i) + px4(i)*vy4(i)
          dxz(i) = pz1(i)*vx1(i) + pz2(i)*vx2(i) + pz5(i)*vx5(i) +             &
                   pz3(i)*vx3(i) + pz6(i)*vx6(i) + pz4(i)*vx4(i)
          dzx(i) = px1(i)*vz1(i) + px2(i)*vz2(i) + px5(i)*vz5(i) +             &
                   px3(i)*vz3(i) + px6(i)*vz6(i) + px4(i)*vz4(i)
          dzy(i) = py1(i)*vz1(i) + py2(i)*vz2(i) + py5(i)*vz5(i) +             &
                   py3(i)*vz3(i) + py6(i)*vz6(i) + py4(i)*vz4(i)
          dyz(i) = pz1(i)*vy1(i) + pz2(i)*vy2(i) + pz5(i)*vy5(i) +             &
                   pz3(i)*vy3(i) + pz6(i)*vy6(i) + pz4(i)*vy4(i)
        enddo
!
      end subroutine s6zdefc3
      end module s6zdefc3_mod