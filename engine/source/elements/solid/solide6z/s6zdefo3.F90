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
!||    s6zdefo3_mod   ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
     module s6zdefo3_mod
     contains
!||====================================================================
!||    s6zdefo3        ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
     subroutine s6zdefo3( &
       dxx      , dxy      , dxz      , dyx      ,                             &
       dyy      , dyz      , dzx      , dzy      ,                             &
       dzz      , d4       , d5       , d6       ,                             &
       dcxx     , dcxy     , dcxz     , dcyx     ,                             &
       dcyy     , dcyz     , dczx     , dczy     ,                             &
       dczz     , zi       , wi       , vzl      ,                             &
       vol      , volg     , off      , offg     ,                             &
       offs     , voldp    , nel      , dt1      ,                             &
       ismdisp  , iscau    )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
       use precision_mod, only : wp
       use constant_mod , only : zero, one, two, half, em20
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
       implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
       integer,                    intent(in)       :: nel       !< Number of elements
       integer,                    intent(in)       :: ismdisp
       integer,                    intent(in)       :: iscau
       real(kind=WP),              intent(in)       :: dt1       !< Time step
       real(kind=wp), dimension(nel), intent(inout) :: dxx       !< Strain rate component XX
       real(kind=wp), dimension(nel), intent(inout) :: dxy       !< Strain rate component XY
       real(kind=wp), dimension(nel), intent(inout) :: dxz       !< Strain rate component XZ
       real(kind=wp), dimension(nel), intent(inout) :: dyx       !< Strain rate component YX
       real(kind=wp), dimension(nel), intent(inout) :: dyy       !< Strain rate component YY
       real(kind=wp), dimension(nel), intent(inout) :: dyz       !< Strain rate component YZ
       real(kind=wp), dimension(nel), intent(inout) :: dzx       !< Strain rate component ZX
       real(kind=wp), dimension(nel), intent(inout) :: dzy       !< Strain rate component ZY
       real(kind=wp), dimension(nel), intent(inout) :: dzz       !< Strain rate component ZZ
       real(kind=wp), dimension(nel), intent(inout) :: d4        !< Strain rate component 4
       real(kind=wp), dimension(nel), intent(inout) :: d5        !< Strain rate component 5
       real(kind=wp), dimension(nel), intent(inout) :: d6        !< Strain rate component 6
       real(kind=wp), dimension(nel), intent(in)    :: dcxx      !< Constant strain rate XX
       real(kind=wp), dimension(nel), intent(in)    :: dcxy      !< Constant strain rate XY
       real(kind=wp), dimension(nel), intent(in)    :: dcxz      !< Constant strain rate XZ
       real(kind=wp), dimension(nel), intent(in)    :: dcyx      !< Constant strain rate YX
       real(kind=wp), dimension(nel), intent(in)    :: dcyy      !< Constant strain rate YY
       real(kind=wp), dimension(nel), intent(in)    :: dcyz      !< Constant strain rate YZ
       real(kind=wp), dimension(nel), intent(in)    :: dczx      !< Constant strain rate ZX
       real(kind=wp), dimension(nel), intent(in)    :: dczy      !< Constant strain rate ZY
       real(kind=wp), dimension(nel), intent(in)    :: dczz      !< Constant strain rate ZZ
       real(kind=wp),                 intent(in)    :: zi        !< Thickness coordinate
       real(kind=wp),                 intent(in)    :: wi        !< Weight factor
       real(kind=wp), dimension(nel), intent(in)    :: vzl       !< Volume velocity
       real(kind=wp), dimension(nel), intent(inout) :: vol       !< Current volume
       real(kind=wp), dimension(nel), intent(in)    :: volg      !< Global volume
       real(kind=wp), dimension(nel), intent(inout) :: off       !< Element flag
       real(kind=wp), dimension(nel), intent(in)    :: offg      !< Global element flag
       real(kind=wp), dimension(nel), intent(in)    :: offs      !< Shell element flag
!C     ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.  
       real(kind=8), dimension(nel), intent(inout)   :: voldp     !< Double precision volume
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
       integer :: i                                              !< Loop counter
       real(kind=wp) ::   tol, dt1d2, dt1d1          !< Local work variables
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
       tol = one - em20
       do i = 1, nel
         vol(i) = half * wi * (volg(i) + vzl(i) * zi)
         vol(i) = voldp(i)
         off(i) = offg(i)
         if (vol(i) <= zero) then
            vol(i) = em20
            off(i) = zero
         elseif(off(i) == zero .or. offs(i) == two .or. ismdisp > 0) then
            voldp(i) = max(em20, voldp(i))
            vol(i) = max(em20, vol(i))
         endif
       enddo
       dt1d1 = dt1
       if (ismdisp > 0 .and. iscau == 0) dt1d1 = zero
       dt1d2 = half * dt1d1
       do i = 1, nel
         dxx(i) = dcxx(i)
         dyy(i) = dcyy(i)
         dzz(i) = dczz(i)
         dxy(i) = dcxy(i)
         dyx(i) = dcyx(i)
         dzx(i) = dczx(i)
         dzy(i) = dczy(i)
         dxz(i) = dcxz(i)
         dyz(i) = dcyz(i)
         d4(i)  = dxy(i) + dyx(i)                                              &
                - dt1d1 * (dxx(i) * dxy(i) + dyx(i) * dyy(i) + dzx(i) * dzy(i))
         d5(i)  = dyz(i) + dzy(i)                                              &
                - dt1d1 * (dyy(i) * dyz(i) + dzy(i) * dzz(i) + dxy(i) * dxz(i))
         d6(i)  = dxz(i) + dzx(i)                                              &
                - dt1d1 * (dzz(i) * dzx(i) + dxz(i) * dxx(i) + dyz(i) * dyx(i))
         dxx(i) = dxx(i)                                                       &
                - dt1d2 * (dxx(i) * dxx(i) + dyx(i) * dyx(i) + dzx(i) * dzx(i))
         dyy(i) = dyy(i)                                                       &
                - dt1d2 * (dyy(i) * dyy(i) + dzy(i) * dzy(i) + dxy(i) * dxy(i))
         dzz(i) = dzz(i)                                                       &
                - dt1d2 * (dzz(i) * dzz(i) + dxz(i) * dxz(i) + dyz(i) * dyz(i))
       enddo
     end subroutine s6zdefo3
     end module s6zdefo3_mod