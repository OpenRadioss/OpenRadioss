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
!||    s6zsav12_mod   ../engine/source/elements/solid/solide/s6zsav12.F90
!||--- called by ------------------------------------------------------
!||    s8eforc3      ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3      ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    sforc3        ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3       ../engine/source/elements/solid/solidez/szforc3.F
!||====================================================================
      module s6zsav12_mod
      contains
!||====================================================================
!||
!||    s6zsav12
!||
!||--- called by ------------------------------------------------------
!||
!||    sforc3
!||    szforc3
!||
!||--- uses       -----------------------------------------------------
!||
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||====================================================================
        subroutine s6zsav12( &
          offg     ,offg0    ,sav      ,x        ,      &
          xdp      ,nc1      ,nc2      ,nc3      ,      &
          nc4      ,nc5      ,nc6      ,nel      ,      &
          numnod   ,iresp)

!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod,  only : one
          use mvsiz_mod,     only : mvsiz

!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel       !< Number of elements
          integer,                          intent(in)    :: numnod    
          integer,                          intent(in)    :: iresp            
          integer, dimension(nel),          intent(in)    :: nc1       !< Node connectivity index 1
          integer, dimension(nel),          intent(in)    :: nc2       !< Node connectivity index 2
          integer, dimension(nel),          intent(in)    :: nc3       !< Node connectivity index 3
          integer, dimension(nel),          intent(in)    :: nc4       !< Node connectivity index 4
          integer, dimension(nel),          intent(in)    :: nc5       !< Node connectivity index 5
          integer, dimension(nel),          intent(in)    :: nc6       !< Node connectivity index 6

          real(kind=wp), dimension(nel),    intent(in)    :: offg      !< Global element activation flag
          real(kind=wp), dimension(nel),    intent(in)    :: offg0     !< Previous Global element activation flag

          ! Coordinates
          real(kind=wp), dimension(3,numnod),    intent(in)    :: x         !< Nodal coordinates (Working Precision)
          real(kind=8),  dimension(3,numnod),    intent(in)    :: xdp       !< Nodal coordinates (Double Precision)

          ! SAV is output, storing Deltas (Requires Double Precision)
          real(kind=8),  dimension(nel,15), intent(out)   :: sav       !< Saved coordinate differences

!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, ih

          ! Local arrays for vectorization (using MVSIZ as in original code)
          real(kind=8), dimension(nel) :: xd1, xd2, xd3, xd4, xd5, xd6
          real(kind=8), dimension(nel) :: yd1, yd2, yd3, yd4, yd5, yd6
          real(kind=8), dimension(nel) :: zd1, zd2, zd3, zd4, zd5, zd6

!===============================================================================
!   B o d y
!===============================================================================          
          ! Check if update is needed
          ih = 0
          do i = 1, nel
             if (offg(i) /= offg0(i) .and. abs(offg(i)) > one) ih = 1
          end do
          
          if (ih == 0) return
          if (iresp == 1) then
            ! Case 1: Use Double Precision Inputs
            do i = 1, nel
              xd1(i) = xdp(1,nc1(i))
              yd1(i) = xdp(2,nc1(i))
              zd1(i) = xdp(3,nc1(i))
              
              xd2(i) = xdp(1,nc2(i))
              yd2(i) = xdp(2,nc2(i))
              zd2(i) = xdp(3,nc2(i))
              
              xd3(i) = xdp(1,nc3(i))
              yd3(i) = xdp(2,nc3(i))
              zd3(i) = xdp(3,nc3(i))
              
              xd4(i) = xdp(1,nc4(i))
              yd4(i) = xdp(2,nc4(i))
              zd4(i) = xdp(3,nc4(i))
              
              xd5(i) = xdp(1,nc5(i))
              yd5(i) = xdp(2,nc5(i))
              zd5(i) = xdp(3,nc5(i))
              
              xd6(i) = xdp(1,nc6(i))
              yd6(i) = xdp(2,nc6(i))
              zd6(i) = xdp(3,nc6(i))
            end do
          else
            ! Case 2: Use Working Precision Inputs (Converted to Double)
            do i = 1, nel
              xd1(i) = x(1,nc1(i))
              yd1(i) = x(2,nc1(i))
              zd1(i) = x(3,nc1(i))
              
              xd2(i) = x(1,nc2(i))
              yd2(i) = x(2,nc2(i))
              zd2(i) = x(3,nc2(i))
              
              xd3(i) = x(1,nc3(i))
              yd3(i) = x(2,nc3(i))
              zd3(i) = x(3,nc3(i))
              
              xd4(i) = x(1,nc4(i))
              yd4(i) = x(2,nc4(i))
              zd4(i) = x(3,nc4(i))
              
              xd5(i) = x(1,nc5(i))
              yd5(i) = x(2,nc5(i))
              zd5(i) = x(3,nc5(i))
              
              xd6(i) = x(1,nc6(i))
              yd6(i) = x(2,nc6(i))
              zd6(i) = x(3,nc6(i))
            end do      
          end if

          ! Compute Deltas relative to Node 6
          ! note: format is different to Ismstr=10,11
          do i = 1, nel
            if (offg(i) /= offg0(i) .and. abs(offg(i)) > one) then
              sav(i,1)  = xd1(i) - xd6(i)
              sav(i,2)  = yd1(i) - yd6(i)
              sav(i,3)  = zd1(i) - zd6(i)
              
              sav(i,4)  = xd2(i) - xd6(i)
              sav(i,5)  = yd2(i) - yd6(i)
              sav(i,6)  = zd2(i) - zd6(i)
              
              sav(i,7)  = xd3(i) - xd6(i)
              sav(i,8)  = yd3(i) - yd6(i)
              sav(i,9)  = zd3(i) - zd6(i)
              
              sav(i,10) = xd4(i) - xd6(i)
              sav(i,11) = yd4(i) - yd6(i)
              sav(i,12) = zd4(i) - zd6(i)
              
              sav(i,13) = xd5(i) - xd6(i)
              sav(i,14) = yd5(i) - yd6(i)
              sav(i,15) = zd5(i) - zd6(i)
            end if
          end do

        end subroutine s6zsav12

      end module s6zsav12_mod