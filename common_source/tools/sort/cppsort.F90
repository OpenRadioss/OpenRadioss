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
!||    cppsort_mod   ../common_source/tools/sort/cppsort.F90
!||--- Description -------------------------------------------------
!||    Fortran interface to the C++ STL-sort wrappers in cppsort.cpp.
!||
!||    Two generic names are provided; the compiler selects the correct
!||    specific routine based on the number and types of the arguments:
!||
!||      stlsort        – unstable sort (std::sort, fastest)
!||      stlstable_sort – stable sort   (std::stable_sort, preserves
!||                       relative order of elements with equal keys) 
!||                       faster for almost-sorted data, but slower for random data
!||
!||    Both accept the same signatures:
!||      call stlsort/stlstable_sort(len, array)
!||                                   – sort real or double precision array
!||      call stlsort/stlstable_sort(len, keys, values)
!||                                   – sort key-value pairs:
!||            integer        keys + integer        values
!||            real           keys + integer        values
!||            double prec.   keys + integer        values
!||            real           keys + real           values
!||            double prec.   keys + double prec.   values
!||
!||    iso_c_binding is used throughout; no name-mangling variants
!||    are needed in the C++ translation unit.
!||====================================================================

      module cppsort_mod

        use iso_c_binding, only : c_int, c_float, c_double
        implicit none
        private

        public :: stlsort
        public :: stlstable_sort

! ----------------------------------------------------------------------------------------------------------------------
!  iso_c_binding declarations – private, implementation detail
! ----------------------------------------------------------------------------------------------------------------------

        interface
          subroutine c_stlsort_float(len, array) &
              bind(C, name='stlsort_float')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: array(*)
          end subroutine c_stlsort_float

          subroutine c_stlsort_double(len, array) &
              bind(C, name='stlsort_double')
            import :: c_int, c_double
            integer(c_int),  intent(in)    :: len
            real(c_double),  intent(inout) :: array(*)
          end subroutine c_stlsort_double

          subroutine c_stlsort_int(len, array) &
              bind(C, name='stlsort_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: array(*)
          end subroutine c_stlsort_int

          subroutine c_stlsort_int_int(len, keys, values) &
              bind(C, name='stlsort_int_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlsort_int_int

          subroutine c_stlsort_float_int(len, keys, values) &
              bind(C, name='stlsort_float_int')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlsort_float_int

          subroutine c_stlsort_double_int(len, keys, values) &
              bind(C, name='stlsort_double_int')
            import :: c_int, c_double
            integer(c_int), intent(in)    :: len
            real(c_double), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlsort_double_int

          subroutine c_stlsort_float_float(len, keys, values) &
              bind(C, name='stlsort_float_float')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: keys(*)
            real(c_float),  intent(inout) :: values(*)
          end subroutine c_stlsort_float_float

          subroutine c_stlsort_double_double(len, keys, values) &
              bind(C, name='stlsort_double_double')
            import :: c_int, c_double
            integer(c_int), intent(in)    :: len
            real(c_double), intent(inout) :: keys(*)
            real(c_double), intent(inout) :: values(*)
          end subroutine c_stlsort_double_double

          ! --- stable_sort bindings -------------------------------------------

          subroutine c_stlstable_sort_int(len, array) &
              bind(C, name='stlstable_sort_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: array(*)
          end subroutine c_stlstable_sort_int

          subroutine c_stlstable_sort_float(len, array) &
              bind(C, name='stlstable_sort_float')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: array(*)
          end subroutine c_stlstable_sort_float

          subroutine c_stlstable_sort_double(len, array) &
              bind(C, name='stlstable_sort_double')
            import :: c_int, c_double
            integer(c_int),  intent(in)    :: len
            real(c_double),  intent(inout) :: array(*)
          end subroutine c_stlstable_sort_double

          subroutine c_stlstable_sort_int_int(len, keys, values) &
              bind(C, name='stlstable_sort_int_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlstable_sort_int_int

          subroutine c_stlstable_sort_float_int(len, keys, values) &
              bind(C, name='stlstable_sort_float_int')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlstable_sort_float_int

          subroutine c_stlstable_sort_double_int(len, keys, values) &
              bind(C, name='stlstable_sort_double_int')
            import :: c_int, c_double
            integer(c_int), intent(in)    :: len
            real(c_double), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlstable_sort_double_int

          subroutine c_stlstable_sort_float_float(len, keys, values) &
              bind(C, name='stlstable_sort_float_float')
            import :: c_int, c_float
            integer(c_int), intent(in)    :: len
            real(c_float),  intent(inout) :: keys(*)
            real(c_float),  intent(inout) :: values(*)
          end subroutine c_stlstable_sort_float_float

          subroutine c_stlstable_sort_double_double(len, keys, values) &
              bind(C, name='stlstable_sort_double_double')
            import :: c_int, c_double
            integer(c_int), intent(in)    :: len
            real(c_double), intent(inout) :: keys(*)
            real(c_double), intent(inout) :: values(*)
          end subroutine c_stlstable_sort_double_double
        end interface

! ----------------------------------------------------------------------------------------------------------------------
!  Generic public interface – single name stlsort, dispatch by signature
! ----------------------------------------------------------------------------------------------------------------------

        interface stlsort
          module procedure f_stlsort_int
          module procedure f_stlsort_float
          module procedure f_stlsort_double
          module procedure f_stlsort_int_int
          module procedure f_stlsort_float_int
          module procedure f_stlsort_double_int
          module procedure f_stlsort_float_float
          module procedure f_stlsort_double_double
        end interface stlsort

        interface stlstable_sort
          module procedure f_stlstable_int
          module procedure f_stlstable_sort_float
          module procedure f_stlstable_sort_double
          module procedure f_stlstable_sort_int_int
          module procedure f_stlstable_sort_float_int
          module procedure f_stlstable_sort_double_int
          module procedure f_stlstable_sort_float_float
          module procedure f_stlstable_sort_double_double
        end interface stlstable_sort

      contains

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_int(len, array)
          integer(c_int), intent(in)    :: len
          integer(c_int),  intent(inout) :: array(*)
          call c_stlsort_int(len, array)
        end subroutine f_stlsort_int

        subroutine f_stlsort_float(len, array)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: array(*)
          call c_stlsort_float(len, array)
        end subroutine f_stlsort_float

        subroutine f_stlsort_double(len, array)
          integer(c_int),  intent(in)    :: len
          real(c_double),  intent(inout) :: array(*)
          call c_stlsort_double(len, array)
        end subroutine f_stlsort_double

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_int_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          integer(c_int), intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlsort_int_int(len, keys, values)
        end subroutine f_stlsort_int_int

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_float_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlsort_float_int(len, keys, values)
        end subroutine f_stlsort_float_int

        subroutine f_stlsort_double_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_double), intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlsort_double_int(len, keys, values)
        end subroutine f_stlsort_double_int

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_float_float(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: keys(*)
          real(c_float),  intent(inout) :: values(*)
          call c_stlsort_float_float(len, keys, values)
        end subroutine f_stlsort_float_float

        subroutine f_stlsort_double_double(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_double), intent(inout) :: keys(*)
          real(c_double), intent(inout) :: values(*)
          call c_stlsort_double_double(len, keys, values)
        end subroutine f_stlsort_double_double

! ----------------------------------------------------------------------------------------------------------------------
!  stable_sort wrappers
! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlstable_int(len, array)
          integer(c_int), intent(in)    :: len
          integer(c_int),  intent(inout) :: array(*)
          call c_stlstable_sort_int(len, array)
        end subroutine f_stlstable_int

        subroutine f_stlstable_sort_float(len, array)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: array(*)
          call c_stlstable_sort_float(len, array)
        end subroutine f_stlstable_sort_float

        subroutine f_stlstable_sort_double(len, array)
          integer(c_int),  intent(in)    :: len
          real(c_double),  intent(inout) :: array(*)
          call c_stlstable_sort_double(len, array)
        end subroutine f_stlstable_sort_double


        subroutine f_stlstable_sort_int_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          integer(c_int), intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlstable_sort_int_int(len, keys, values)
        end subroutine f_stlstable_sort_int_int

        subroutine f_stlstable_sort_float_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlstable_sort_float_int(len, keys, values)
        end subroutine f_stlstable_sort_float_int

        subroutine f_stlstable_sort_double_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_double), intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlstable_sort_double_int(len, keys, values)
        end subroutine f_stlstable_sort_double_int

        subroutine f_stlstable_sort_float_float(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_float),  intent(inout) :: keys(*)
          real(c_float),  intent(inout) :: values(*)
          call c_stlstable_sort_float_float(len, keys, values)
        end subroutine f_stlstable_sort_float_float

        subroutine f_stlstable_sort_double_double(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(c_double), intent(inout) :: keys(*)
          real(c_double), intent(inout) :: values(*)
          call c_stlstable_sort_double_double(len, keys, values)
        end subroutine f_stlstable_sort_double_double

      end module cppsort_mod
