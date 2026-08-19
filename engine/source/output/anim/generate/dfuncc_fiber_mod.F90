!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.

!! \brief Shared fiber-angle computation helpers extracted from dfuncc.F
!! \details Provides fiber_angle_quad and fiber_angle_tria, each handling one
!!          vectorised chunk (LFT..LLT) of shell elements. The caller selects
!!          the drape-direction array (LBUF_DIR%DIRA or BUFLY%DIRA) before calling.
module dfuncc_fiber_mod
  use precision_mod, only: WP
  implicit none

  private
  public :: fiber_angle_quad, fiber_angle_tria

  real(kind=WP), parameter :: ZERO_     = 0.0_WP
  real(kind=WP), parameter :: ONE_      = 1.0_WP
  real(kind=WP), parameter :: HALF_     = 0.5_WP
  real(kind=WP), parameter :: EM20_     = 1.0e-20_WP
  real(kind=WP), parameter :: EM02_     = 1.0e-2_WP
  real(kind=WP), parameter :: NINETY_   = 90.0_WP
  real(kind=WP), parameter :: HUNDRED80_= 180.0_WP
  real(kind=WP), parameter :: PI_       = 3.14159265358979323846_WP

contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Orthonormal frame from two edge vectors (private copy of CLSCONV3)
subroutine clsconv3_local(rx, ry, rz, sx, sy, sz, &
                           e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z)
  real(kind=WP), intent(in)  :: rx, ry, rz, sx, sy, sz
  real(kind=WP), intent(out) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
  real(kind=WP) :: c1, c2, cc, det

  e3x = ry*sz - rz*sy
  e3y = rz*sx - rx*sz
  e3z = rx*sy - ry*sx
  det = sqrt(e3x*e3x + e3y*e3y + e3z*e3z)
  det = max(EM20_, det)
  cc  = ONE_ / det
  e3x = e3x * cc
  e3y = e3y * cc
  e3z = e3z * cc

  c1  = sqrt(rx*rx + ry*ry + rz*rz)
  c2  = sqrt(sx*sx + sy*sy + sz*sz)
  e1x = rx*c2 + (sy*e3z - sz*e3y)*c1
  e1y = ry*c2 + (sz*e3x - sx*e3z)*c1
  e1z = rz*c2 + (sx*e3y - sy*e3x)*c1

  c1  = sqrt(e1x*e1x + e1y*e1y + e1z*e1z)
  if (c1 /= ZERO_) c1 = ONE_ / c1
  e1x = e1x * c1
  e1y = e1y * c1
  e1z = e1z * c1
  e2x = e3y*e1z - e3z*e1y
  e2y = e3z*e1x - e3x*e1z
  e2z = e3x*e1y - e3y*e1x

end subroutine clsconv3_local

! ----------------------------------------------------------------------------------------------------------------------

!! \brief Fiber angle for one vectorised chunk of quad (IXC) shell elements
!! \details Constructs the element frame from mid-surface edge vectors,
!!          optionally rotates it via IREP/DIRA, then writes ATAN2(phi) to EVAR.
!!          Unified condition (ISHFRAM==0 .OR. IGTYP==16) covers both the strict
!!          IDRAPE path (IGTYP 51/52, IGTYP/=16 is harmless) and the generic path.
subroutine fiber_angle_quad(ixc, nixc_dim, x, lft, llt, nft, nel, &
                             irep, ishfram, igtyp, dira, evar)

  integer,       intent(in)    :: nixc_dim        !< leading dimension of ixc
  integer,       intent(in)    :: lft             !< first element index in chunk
  integer,       intent(in)    :: llt             !< last  element index in chunk
  integer,       intent(in)    :: nft             !< global offset: N = I + nft
  integer,       intent(in)    :: nel             !< total elements in group (for DIRA stride)
  integer,       intent(in)    :: irep            !< reference-direction flag
  integer,       intent(in)    :: ishfram         !< frame type (0=symmetric, 2=non-symmetric)
  integer,       intent(in)    :: igtyp           !< group element type
  integer,       intent(in)    :: ixc(nixc_dim, *) !< quad connectivity
  real(kind=WP), intent(in)    :: x(3, *)         !< nodal coordinates
  real(kind=WP), intent(in)    :: dira(*)          !< drape direction (LBUF_DIR%DIRA or BUFLY%DIRA)
  real(kind=WP), intent(inout) :: evar(*)          !< output: fiber angle in degrees

  integer       :: i, n
  real(kind=WP) :: x21, x32, x34, x41, y21, y32, y34, y41, z21, z32, z34, z41
  real(kind=WP) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
  real(kind=WP) :: rx, ry, rz, sx, sy, sz
  real(kind=WP) :: s1, s2, suma, aa, bb, v1, v2, v3, vr, vs
  real(kind=WP) :: dir1_1, dir1_2, phi, err

  do i = lft, llt
    n   = i + nft
    x21 = x(1, ixc(3,n)) - x(1, ixc(2,n))
    x32 = x(1, ixc(4,n)) - x(1, ixc(3,n))
    x34 = x(1, ixc(4,n)) - x(1, ixc(5,n))
    x41 = x(1, ixc(5,n)) - x(1, ixc(2,n))
    y21 = x(2, ixc(3,n)) - x(2, ixc(2,n))
    y32 = x(2, ixc(4,n)) - x(2, ixc(3,n))
    y34 = x(2, ixc(4,n)) - x(2, ixc(5,n))
    y41 = x(2, ixc(5,n)) - x(2, ixc(2,n))
    z21 = x(3, ixc(3,n)) - x(3, ixc(2,n))
    z32 = x(3, ixc(4,n)) - x(3, ixc(3,n))
    z34 = x(3, ixc(4,n)) - x(3, ixc(5,n))
    z41 = x(3, ixc(5,n)) - x(3, ixc(2,n))

    e1x = x21 + x34
    e1y = y21 + y34
    e1z = z21 + z34
    e2x = x32 + x41
    e2y = y32 + y41
    e2z = z32 + z41

    e3x = e1y*e2z - e1z*e2y
    e3y = e1z*e2x - e1x*e2z
    e3z = e1x*e2y - e1y*e2x

    if (irep > 0) then
      rx = e1x;  ry = e1y;  rz = e1z
      sx = e2x;  sy = e2y;  sz = e2z
    end if

    if (ishfram == 0 .or. igtyp == 16) then
      ! Symmetrical convected frame (DEFAULT)
      suma = e3x*e3x + e3y*e3y + e3z*e3z
      suma = ONE_ / max(sqrt(suma), EM20_)
      e3x  = e3x * suma;  e3y = e3y * suma;  e3z = e3z * suma

      s1   = e1x*e1x + e1y*e1y + e1z*e1z
      s2   = e2x*e2x + e2y*e2y + e2z*e2z
      suma = sqrt(s1 / s2)
      e1x  = e1x + (e2y*e3z - e2z*e3y)*suma
      e1y  = e1y + (e2z*e3x - e2x*e3z)*suma
      e1z  = e1z + (e2x*e3y - e2y*e3x)*suma

      suma = e1x*e1x + e1y*e1y + e1z*e1z
      suma = ONE_ / max(sqrt(suma), EM20_)
      e1x  = e1x * suma;  e1y = e1y * suma;  e1z = e1z * suma

      e2x  = e3y*e1z - e3z*e1y
      e2y  = e3z*e1x - e3x*e1z
      e2z  = e3x*e1y - e3y*e1x
    else if (ishfram == 2) then
      ! Non-symmetrical convected frame
      suma = e2x*e2x + e2y*e2y + e2z*e2z
      e1x  = e1x*suma + e2y*e3z - e2z*e3y
      e1y  = e1y*suma + e2z*e3x - e2x*e3z
      e1z  = e1z*suma + e2x*e3y - e2y*e3x
      suma = e1x*e1x + e1y*e1y + e1z*e1z
      suma = ONE_ / max(sqrt(suma), EM20_)
      e1x  = e1x * suma;  e1y = e1y * suma;  e1z = e1z * suma

      suma = e3x*e3x + e3y*e3y + e3z*e3z
      suma = ONE_ / max(sqrt(suma), EM20_)
      e3x  = e3x * suma;  e3y = e3y * suma;  e3z = e3z * suma

      e2x  = e3y*e1z - e3z*e1y
      e2y  = e3z*e1x - e3x*e1z
      e2z  = e3x*e1y - e3y*e1x
      suma = e2x*e2x + e2y*e2y + e2z*e2z
      suma = ONE_ / max(sqrt(suma), EM20_)
      e2x  = e2x * suma;  e2y = e2y * suma;  e2z = e2z * suma
    end if

    if (irep >= 1) then
      aa     = dira(i)
      bb     = dira(i + nel)
      v1     = aa*rx + bb*sx
      v2     = aa*ry + bb*sy
      v3     = aa*rz + bb*sz
      vr     = v1*e1x + v2*e1y + v3*e1z
      vs     = v1*e2x + v2*e2y + v3*e2z
      suma   = sqrt(vr*vr + vs*vs)
      dir1_1 = vr / suma
      dir1_2 = vs / suma
    else
      dir1_1 = dira(i)
      dir1_2 = dira(i + nel)
    end if

    phi     = (HUNDRED80_ / PI_) * atan2(dir1_2, dir1_1)
    err     = (abs(phi) - NINETY_) / NINETY_
    evar(i) = phi
    if (abs(err)     < EM02_) evar(i) = sign(NINETY_, phi)
    if (abs(evar(i)) < ONE_)  evar(i) = ZERO_
  end do

end subroutine fiber_angle_quad

! ----------------------------------------------------------------------------------------------------------------------

!! \brief Fiber angle for one vectorised chunk of triangle (IXTG) shell elements
!! \details IFRAM_OLD==0 uses CLSCONV3-equivalent orthonormalization;
!!          IFRAM_OLD/=0 uses the legacy explicit frame (old ISH3NFRAM path).
subroutine fiber_angle_tria(ixtg, nixtg_dim, x, lft, llt, nft, nel, &
                             irep, ifram_old, dira, evar)

  integer,       intent(in)    :: nixtg_dim        !< leading dimension of ixtg
  integer,       intent(in)    :: lft              !< first element index in chunk
  integer,       intent(in)    :: llt              !< last  element index in chunk
  integer,       intent(in)    :: nft              !< global offset: N = I + nft
  integer,       intent(in)    :: nel              !< total elements in group (for DIRA stride)
  integer,       intent(in)    :: irep             !< reference-direction flag
  integer,       intent(in)    :: ifram_old        !< 0 = CLSCONV3-style frame, 1 = legacy
  integer,       intent(in)    :: ixtg(nixtg_dim, *) !< triangle connectivity
  real(kind=WP), intent(in)    :: x(3, *)           !< nodal coordinates
  real(kind=WP), intent(in)    :: dira(*)            !< drape direction (LBUF_DIR%DIRA or BUFLY%DIRA)
  real(kind=WP), intent(inout) :: evar(*)            !< output: fiber angle in degrees

  integer       :: i, n
  real(kind=WP) :: x21, x31, x32, y21, y31, y32, z21, z31, z32
  real(kind=WP) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
  real(kind=WP) :: e11, e12, e13, e21r, e22, e23
  real(kind=WP) :: aa, bb, v1, v2, v3, vr, vs
  real(kind=WP) :: x2l, sum_, area, suma
  real(kind=WP) :: dir1_1, dir1_2, phi, err

  do i = lft, llt
    n   = i + nft
    x21 = x(1, ixtg(3,n)) - x(1, ixtg(2,n))
    x31 = x(1, ixtg(4,n)) - x(1, ixtg(2,n))
    x32 = x(1, ixtg(4,n)) - x(1, ixtg(3,n))
    y21 = x(2, ixtg(3,n)) - x(2, ixtg(2,n))
    y31 = x(2, ixtg(4,n)) - x(2, ixtg(2,n))
    y32 = x(2, ixtg(4,n)) - x(2, ixtg(3,n))
    z21 = x(3, ixtg(3,n)) - x(3, ixtg(2,n))
    z31 = x(3, ixtg(4,n)) - x(3, ixtg(2,n))
    z32 = x(3, ixtg(4,n)) - x(3, ixtg(3,n))

    if (irep > 0) then
      e11  = x21;  e12 = y21;  e13 = z21
      e21r = x31;  e22 = y31;  e23 = z31
    end if

    if (ifram_old == 0) then
      call clsconv3_local(x21, y21, z21, x31, y31, z31, &
                          e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z)
    else
      ! Legacy explicit orthonormalization (ISH3NFRAM path)
      e1x = x21;  e1y = y21;  e1z = z21
      x2l = sqrt(e1x*e1x + e1y*e1y + e1z*e1z)
      e1x = e1x / x2l;  e1y = e1y / x2l;  e1z = e1z / x2l

      e3x  = y31*z32 - z31*y32
      e3y  = z31*x32 - x31*z32
      e3z  = x31*y32 - y31*x32
      sum_ = sqrt(e3x*e3x + e3y*e3y + e3z*e3z)
      e3x  = e3x / sum_;  e3y = e3y / sum_;  e3z = e3z / sum_
      area = HALF_ * sum_

      e2x  = e3y*e1z - e3z*e1y
      e2y  = e3z*e1x - e3x*e1z
      e2z  = e3x*e1y - e3y*e1x
      sum_ = sqrt(e2x*e2x + e2y*e2y + e2z*e2z)
      e2x  = e2x / sum_;  e2y = e2y / sum_;  e2z = e2z / sum_
    end if

    if (irep >= 1) then
      aa   = dira(i)
      bb   = dira(i + nel)
      v1   = aa*e11  + bb*e21r
      v2   = aa*e12  + bb*e22
      v3   = aa*e13  + bb*e23
      vr   = v1*e1x + v2*e1y + v3*e1z
      vs   = v1*e2x + v2*e2y + v3*e2z
      suma = sqrt(vr*vr + vs*vs)
      dir1_1 = vr / suma
      dir1_2 = vs / suma
    else
      dir1_1 = dira(i)
      dir1_2 = dira(i + nel)
    end if

    phi     = (HUNDRED80_ / PI_) * atan2(dir1_2, dir1_1)
    err     = (abs(phi) - NINETY_) / NINETY_
    evar(i) = phi
    if (abs(err)     < EM02_) evar(i) = sign(NINETY_, phi)
    if (abs(evar(i)) < ONE_)  evar(i) = ZERO_
  end do

end subroutine fiber_angle_tria

end module dfuncc_fiber_mod
