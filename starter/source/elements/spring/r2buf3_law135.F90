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
!||    r2buf3_law135_mod   ../starter/source/elements/spring/r2buf3_law135.F90
!||--- called by ------------------------------------------------------
!||    rinit3              ../starter/source/elements/spring/rinit3.F
!||====================================================================
      module r2buf3_law135_mod
      contains
!||====================================================================
!||    r2buf3_law135   ../starter/source/elements/spring/r2buf3_law135.F90
!||--- called by ------------------------------------------------------
!||    rinit3          ../starter/source/elements/spring/rinit3.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine r2buf3_law135( &
          off      ,x        ,x0       ,y0       ,      &
          z0       ,ixr      ,skew     ,iposx    ,      &
          iposy    ,iposz    ,iposxx   ,iposyy   ,      &
          iposzz   ,igeo     ,skew_id  ,nuvar    ,      &
          uvar     ,lskew    ,numskw   ,npropgi  ,      &
          numgeo   ,nel      ,nft      ,numelr   ,      &
          numnod   )

!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod,  only : zero, one, ten
          use element_mod,   only : nixr
!
!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nuvar     !< Number of user variables     
          integer,                          intent(in)    :: npropgi
          integer,                          intent(in)    :: numgeo    !< Number of geometric properties
          integer,                          intent(in)    :: nel       !< Number of elements
          integer,                          intent(in)    :: lskew     !< Number of skew entries per element     
          integer,                          intent(in)    :: numskw    !< Number of skew entries per element     
          integer,                          intent(in)    :: nft       !< Number of free terms
          integer,                          intent(in)    :: numelr
          integer,                          intent(in)    :: numnod

          integer, dimension(nixr,numelr),    intent(in)  :: ixr        !< Element connectivity and properties
          integer, dimension(npropgi,numgeo), intent(in)  :: igeo      !< Integer geometric properties
          integer, dimension(nel),            intent(in)  :: skew_id   !< Skew IDs

          real(kind=wp), dimension(3,numnod),     intent(in) :: x         !< Nodal coordinates
          real(kind=WP), dimension(lskew,numskw), intent(in) :: skew   !< main structure for skews
          real(kind=wp), dimension(nel),      intent(inout) :: off       !< Activation flag
          real(kind=wp), dimension(nel),      intent(inout) :: x0        !< Rotated coordinate X
          real(kind=wp), dimension(nel),      intent(inout) :: y0        !< Rotated coordinate Y
          real(kind=wp), dimension(nel),      intent(inout) :: z0        !< Rotated coordinate Z

          real(kind=wp), dimension(5,nel),    intent(inout) :: iposx     !< Integration pos X
          real(kind=wp), dimension(5,nel),    intent(inout) :: iposy     !< Integration pos Y
          real(kind=wp), dimension(5,nel),    intent(inout) :: iposz     !< Integration pos Z
          real(kind=wp), dimension(5,nel),    intent(inout) :: iposxx    !< Integration pos XX
          real(kind=wp), dimension(5,nel),    intent(inout) :: iposyy    !< Integration pos YY
          real(kind=wp), dimension(5,nel),    intent(inout) :: iposzz    !< Integration pos ZZ

          real(kind=wp), dimension(nuvar,nel),intent(inout) :: uvar      !< User defined variables array

!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer       :: i, j, ng, i1, i2, isk, usens
          real(kind=wp) :: x1, y1, z1, ex, ey, ez

!===============================================================================
!   B o d y
!===============================================================================
          ! Check sensor status and initialize OFF array
          do i = 1, nel
            j = i + nft
            usens = igeo(3, ixr(1,j))
            if (usens <= 0) then
              ! No sensor or Isflag=1
              off(i) = one
            else
              off(i) = -ten
            end if
          end do

          ! Initialize Integration point positions to zero
          do j = 1, 5
            do i = 1, nel
              iposx(j,i)  = zero
              iposy(j,i)  = zero
              iposz(j,i)  = zero
              iposxx(j,i) = zero
              iposyy(j,i) = zero
              iposzz(j,i) = zero
            end do
          end do

          ! Compute local lengths and initialize UVAR
          do i = 1, nel
            j = i + nft
            ng = ixr(1,j)
            isk = skew_id(i)
            i1 = ixr(2,j)
            i2 = ixr(3,j)
            
            ! Relative positions
            x1 = x(1,i2) - x(1,i1)
            y1 = x(2,i2) - x(2,i1)
            z1 = x(3,i2) - x(3,i1)
            
            ! X - direction transformation
            ex = skew(1,isk)
            ey = skew(2,isk)
            ez = skew(3,isk)
            uvar(22,i) = ex
            uvar(23,i) = ey
            uvar(24,i) = ez
            x0(i) = ex*x1 + ey*y1 + ez*z1

            ! Y - direction transformation
            ex = skew(4,isk)
            ey = skew(5,isk)
            ez = skew(6,isk)
            uvar(25,i) = ex
            uvar(26,i) = ey
            uvar(27,i) = ez
            y0(i) = ex*x1 + ey*y1 + ez*z1

            ! Z - direction transformation
            ex = skew(7,isk)
            ey = skew(8,isk)
            ez = skew(9,isk)
            uvar(28,i) = ex
            uvar(29,i) = ey
            uvar(30,i) = ez
            z0(i) = ex*x1 + ey*y1 + ez*z1

            ! Store into UVAR array
            uvar(1,i) = x0(i)
            uvar(2,i) = y0(i)
            uvar(3,i) = z0(i)
            uvar(7,i) = zero
            uvar(8,i) = zero
            uvar(9,i) = zero
          end do
          
          ! Zero out extra user variables
          do i = 1, nel
            do j = 34, 39
              uvar(j,i) = zero
            end do
          end do

        end subroutine r2buf3_law135

      end module r2buf3_law135_mod