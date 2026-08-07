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
! ----------------------------------------------------------------------------------------------------------------------
module cppsort_mod
  use precision_mod, only : wp
  implicit none

  contains

!! \brief Sort by two real keys (primary key1, secondary key2) with 1-based permutation output.
!!
!! \details Uses std::stable_sort with lexicographic (key1, key2) comparator.
!!
!! \param[in]     n     Number of elements
!! \param[inout]  key1  Primary sort key array (real, sorted on output)
!! \param[inout]  key2  Secondary sort key array (real, sorted on output)
!! \param[out]    perm  1-based permutation array
  subroutine sort_real2_perm(n, key1, key2, perm)
    integer,       intent(in)    :: n
    real(kind=wp), intent(inout) :: key1(n)
    real(kind=wp), intent(inout) :: key2(n)
    integer,       intent(out)   :: perm(n)

    call stlsort_real2_int(n, key1, key2, perm)

  end subroutine sort_real2_perm

!! \brief Sort by real primary key then integer secondary key, with 1-based permutation output.
!!
!! \details Enables fully deterministic node ordering for /DT/NODA reproducibility across
!!          MPI decompositions. When all DT = MS/STIFN ratios are equal (uniform mesh),
!!          the integer secondary key (node user ID) uniquely determines the sort order.
!!
!! \param[in]     n     Number of elements
!! \param[inout]  key1  Primary sort key array (real, sorted on output)
!! \param[inout]  key2  Secondary sort key array (integer, sorted on output)
!! \param[out]    perm  1-based permutation array
  subroutine sort_real_int2_perm(n, key1, key2, perm)
    integer,       intent(in)    :: n
    real(kind=wp), intent(inout) :: key1(n)
    integer,       intent(inout) :: key2(n)
    integer,       intent(out)   :: perm(n)

    call stlsort_real_int2_int(n, key1, key2, perm)

  end subroutine sort_real_int2_perm

end module cppsort_mod
