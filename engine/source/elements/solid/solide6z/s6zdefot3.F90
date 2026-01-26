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
!||    s6zdefot3_mod   ../engine/source/elements/solid/solide6z/s6zdefot3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zdefot3_mod
      contains
!||====================================================================
!||    s6zdefot3       ../engine/source/elements/solid/solide6z/s6zdefot3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6zdefot3( &
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
!   m o d u l e s
!-------------------------------------------------------------------------------
        use precision_mod, only : wp
!-------------------------------------------------------------------------------
!    i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
        implicit none
!-------------------------------------------------------------------------------
!    d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
        integer, intent(in) :: nel                          !< number of elements
        real(kind=wp), dimension(nel), intent(in)  :: vx1   !< x-velocity at node 1
        real(kind=wp), dimension(nel), intent(in)  :: vx2   !< x-velocity at node 2
        real(kind=wp), dimension(nel), intent(in)  :: vx3   !< x-velocity at node 3
        real(kind=wp), dimension(nel), intent(in)  :: vx4   !< x-velocity at node 4
        real(kind=wp), dimension(nel), intent(in)  :: vx5   !< x-velocity at node 5
        real(kind=wp), dimension(nel), intent(in)  :: vx6   !< x-velocity at node 6
        real(kind=wp), dimension(nel), intent(in)  :: vy1   !< y-velocity at node 1
        real(kind=wp), dimension(nel), intent(in)  :: vy2   !< y-velocity at node 2
        real(kind=wp), dimension(nel), intent(in)  :: vy3   !< y-velocity at node 3
        real(kind=wp), dimension(nel), intent(in)  :: vy4   !< y-velocity at node 4
        real(kind=wp), dimension(nel), intent(in)  :: vy5   !< y-velocity at node 5
        real(kind=wp), dimension(nel), intent(in)  :: vy6   !< y-velocity at node 6
        real(kind=wp), dimension(nel), intent(in)  :: vz1   !< z-velocity at node 1
        real(kind=wp), dimension(nel), intent(in)  :: vz2   !< z-velocity at node 2
        real(kind=wp), dimension(nel), intent(in)  :: vz3   !< z-velocity at node 3
        real(kind=wp), dimension(nel), intent(in)  :: vz4   !< z-velocity at node 4
        real(kind=wp), dimension(nel), intent(in)  :: vz5   !< z-velocity at node 5
        real(kind=wp), dimension(nel), intent(in)  :: vz6   !< z-velocity at node 6
        real(kind=wp), dimension(nel), intent(in)  :: px1   !< shape function derivative x1
        real(kind=wp), dimension(nel), intent(in)  :: px2   !< shape function derivative x2
        real(kind=wp), dimension(nel), intent(in)  :: px3   !< shape function derivative x3
        real(kind=wp), dimension(nel), intent(in)  :: px4   !< shape function derivative x4
        real(kind=wp), dimension(nel), intent(in)  :: px5   !< shape function derivative x5
        real(kind=wp), dimension(nel), intent(in)  :: px6   !< shape function derivative x6
        real(kind=wp), dimension(nel), intent(in)  :: py1   !< shape function derivative y1
        real(kind=wp), dimension(nel), intent(in)  :: py2   !< shape function derivative y2
        real(kind=wp), dimension(nel), intent(in)  :: py3   !< shape function derivative y3
        real(kind=wp), dimension(nel), intent(in)  :: py4   !< shape function derivative y4
        real(kind=wp), dimension(nel), intent(in)  :: py5   !< shape function derivative y5
        real(kind=wp), dimension(nel), intent(in)  :: py6   !< shape function derivative y6
        real(kind=wp), dimension(nel), intent(in)  :: pz1   !< shape function derivative z1
        real(kind=wp), dimension(nel), intent(in)  :: pz2   !< shape function derivative z2
        real(kind=wp), dimension(nel), intent(in)  :: pz3   !< shape function derivative z3
        real(kind=wp), dimension(nel), intent(in)  :: pz4   !< shape function derivative z4
        real(kind=wp), dimension(nel), intent(in)  :: pz5   !< shape function derivative z5
        real(kind=wp), dimension(nel), intent(in)  :: pz6   !< shape function derivative z6
        real(kind=wp), dimension(nel), intent(out) :: dxx   !< strain rate xx component
        real(kind=wp), dimension(nel), intent(out) :: dxy   !< strain rate xy component
        real(kind=wp), dimension(nel), intent(out) :: dxz   !< strain rate xz component
        real(kind=wp), dimension(nel), intent(out) :: dyx   !< strain rate yx component
        real(kind=wp), dimension(nel), intent(out) :: dyy   !< strain rate yy component
        real(kind=wp), dimension(nel), intent(out) :: dyz   !< strain rate yz component
        real(kind=wp), dimension(nel), intent(out) :: dzx   !< strain rate zx component
        real(kind=wp), dimension(nel), intent(out) :: dzy   !< strain rate zy component
        real(kind=wp), dimension(nel), intent(out) :: dzz   !< strain rate zz component
        real(kind=wp), dimension(nel), intent(out) :: wxx   !< spin rate xx component
        real(kind=wp), dimension(nel), intent(out) :: wyy   !< spin rate yy component
        real(kind=wp), dimension(nel), intent(out) :: wzz   !< spin rate zz component
!-------------------------------------------------------------------------------
!    l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
        integer :: i                                                        
!===============================================================================
!     s o u r c e  l i n e s
!===============================================================================
!
        !< initialize spin rates
        do i = 1, nel
          wxx(i) = 0.0_wp
          wyy(i) = 0.0_wp
          wzz(i) = 0.0_wp
        enddo
!
        !< alternative strain rate calculation (direct node approach)
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
      end subroutine s6zdefot3
      end module s6zdefot3_mod
