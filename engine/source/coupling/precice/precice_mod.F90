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
module precice_mod
        use, intrinsic :: iso_c_binding
        implicit none

#ifdef WITH_PRECICE
      interface
        subroutine precicef_create(pname, fname, rank, size, len1, len2) &
                   bind(C, name="precicef_create_")
          use iso_c_binding
          character(kind=c_char) :: pname(*), fname(*)
          integer(c_int) :: rank, size
          integer(c_int), value :: len1, len2  ! VALUE = pass by value
        end subroutine
        subroutine precicef_set_vertices(mesh_name, size, coordinates, ids, mesh_name_length) &
           bind(C, name="precicef_set_vertices_")
           use iso_c_binding
           character(kind=c_char) :: mesh_name(*)
           integer(c_int)  :: size
           real(c_double)  :: coordinates(*)
           integer(c_int)  :: ids(*)
           integer(c_int), value, intent(in) :: mesh_name_length
        end subroutine
!        void precicef_requires_initial_data_(
!   int *isRequired);
        subroutine precicef_requires_initial_data(isRequired) &
           bind(C, name="precicef_requires_initial_data_")
          use iso_c_binding
          integer(c_int), intent(out) :: isRequired
        end subroutine
!       void precicef_write_data_(
!   const char *meshName,
!   const char *dataName,
!   const int  *size,
!   int        *ids,
!   double     *values,
!   int         meshNameLength,
!   int         dataNameLength);
        subroutine precicef_write_data(mesh_name, data_name, ids, size, values, mesh_name_length, data_name_length) &
           bind(C, name="precicef_write_data_")
          use iso_c_binding
          character(kind=c_char) :: mesh_name(*), data_name(*)
          integer(c_int), intent(in) :: ids(*), size
          real(c_double), intent(in) :: values(*)
          integer(c_int), value, intent(in) :: mesh_name_length, data_name_length
        end subroutine

        !void precicef_get_max_time_step_size_(double *maxTimeStepSize);
        subroutine precicef_get_max_time_step_size(maxTimeStepSize) &
           bind(C, name="precicef_get_max_time_step_size_")
          use iso_c_binding
          real(c_double), intent(out) :: maxTimeStepSize
        end subroutine
!void precicef_read_data_(
!    const char   *meshName,
!    const char   *dataName,
!    const int    *size,
!    int          *ids,
!    const double *relativeReadTime,
!    double       *values,
!    int           meshNameLength,
!    int           dataNameLength);
        subroutine precicef_read_data(mesh_name, data_name, ids, size, relative_read_time, values, mesh_name_length, data_name_length) &
           bind(C, name="precicef_read_data_")
          use iso_c_binding
          character(kind=c_char) :: mesh_name(*), data_name(*)
          integer(c_int), intent(in) :: ids(*), size
          real(c_double), intent(in) :: relative_read_time
          real(c_double), intent(out) :: values(*)
          integer(c_int), value, intent(in) :: mesh_name_length, data_name_length
        end subroutine

      end interface
#endif
      end module precice_mod
