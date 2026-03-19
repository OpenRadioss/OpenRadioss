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
! ======================================================================================================================
!> \brief Fortran iso_c_binding interface to the C++ implementation of I2DST3
! ======================================================================================================================
      module cpp_i2dst3_mod
        use, intrinsic :: iso_c_binding, only : c_int, c_double
        implicit none

        private
        public :: cpp_i2dst3

        interface
          ! ------------------------------------------------------------------------------------------------------------------
          !! \brief C++ implementation of I2DST3 — distance computation for interface type 2 (3D surface contact)
          !! \details Binds to extern "C" void i2dst3_(...) in cpp_i2dst3.cpp.
          !!          All arrays are passed as C pointers (Fortran arrays are contiguous and 1-based).
          !!          Scalar arguments are passed by reference (matching C++ const int& / const double&).
          ! ------------------------------------------------------------------------------------------------------------------
          subroutine cpp_i2dst3(first, last,                                          &
            gapv, cand_e, cand_n, tzinf, irtl, st, dmin, ignore,  &
            ix3, ix4,                                             &
            x1, x2, x3, x4,                                      &
            y1, y2, y3, y4,                                      &
            z1, z2, z3, z4,                                      &
            xi, yi, zi,                                          &
            x0, y0, z0,                                          &
            nx1, ny1, nz1,                                       &
            nx2, ny2, nz2,                                       &
            nx3, ny3, nz3,                                       &
            nx4, ny4, nz4,                                       &
            p1, p2, p3, p4,                                      &
            lb1, lb2, lb3, lb4,                                  &
            lc1, lc2, lc3, lc4,                                  &
            s, t)                                                &
            bind(C, name="cpp_i2dst3_")
            use precision_mod, only : WP

! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
            integer(4),  intent(in)           :: first              !< first index of the candidates
            integer(4),  intent(in)           :: last               !< last index of the candidates
            real(WP),  intent(inout)        :: gapv(*)            !< gap values
            integer(4),  intent(inout)        :: cand_e(*)          !< candidate element indices
            integer(4),  intent(inout)        :: cand_n(*)          !< candidate node indices
            real(WP),  intent(in)           :: tzinf              !< infinite search distance
            integer(4),  intent(inout)        :: irtl(*)            !< result: closest element per node
            real(WP),  intent(inout)        :: st(2,*)            !< result: (s,t) parametric coords
            real(WP),  intent(inout)        :: dmin(*)            !< result: minimum distance per node
            integer(4),  intent(in)           :: ignore             !< filtering mode (0, 1, 2, or 3)
            integer(4),  intent(in)          :: ix3(*)             !< 3rd node connectivity
            integer(4),  intent(in)          :: ix4(*)             !< 4th node connectivity
            real(WP),  intent(inout)        :: x1(*), x2(*)      !< X coordinates of quad nodes
            real(WP),  intent(inout)        :: x3(*), x4(*)
            real(WP),  intent(inout)        :: y1(*), y2(*)      !< Y coordinates of quad nodes
            real(WP),  intent(inout)        :: y3(*), y4(*)
            real(WP),  intent(inout)        :: z1(*), z2(*)      !< Z coordinates of quad nodes
            real(WP),  intent(inout)        :: z3(*), z4(*)
            real(WP),  intent(inout)        :: xi(*), yi(*), zi(*) !< impact point coords
            real(WP),  intent(inout)        :: x0(*), y0(*), z0(*) !< centroid coords (output)
            real(WP),  intent(in)          :: nx1(*), ny1(*), nz1(*) !< normal vectors edge 1
            real(WP),  intent(in)          :: nx2(*), ny2(*), nz2(*) !< normal vectors edge 2
            real(WP),  intent(in)          :: nx3(*), ny3(*), nz3(*) !< normal vectors edge 3
            real(WP),  intent(in)          :: nx4(*), ny4(*), nz4(*) !< normal vectors edge 4
            real(WP),  intent(in)          :: p1(*), p2(*)       !< penetration per edge
            real(WP),  intent(in)          :: p3(*), p4(*)
            real(WP),  intent(in)          :: lb1(*), lb2(*)     !< barycentric coord LB per edge
            real(WP),  intent(in)          :: lb3(*), lb4(*)
            real(WP),  intent(in)          :: lc1(*), lc2(*)     !< barycentric coord LC per edge
            real(WP),  intent(in)          :: lc3(*), lc4(*)
            real(WP),  intent(inout)        :: s(*), t(*)         !< parametric coordinates (output)
          end subroutine cpp_i2dst3
        end interface

      end module cpp_i2dst3_mod
