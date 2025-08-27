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
! ----------------------------------------------------------------------------------------------------------------------
!
!||====================================================================
!||    simple_checksum_mod   ../starter/source/tools/curve/simple_checksum_mod.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat70         ../starter/source/materials/mat/mat070/hm_read_mat70.F
!||    hm_read_table2_1      ../starter/source/tools/curve/hm_read_table2_1.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module simple_checksum_mod
        use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
        implicit none

        interface
          subroutine simple_checksum(vector,length,hash) bind(C)
            import :: c_int, c_double
            integer(c_int), intent(in)  :: length
            real(c_double), intent(in) ,dimension(length) :: vector
            real(c_double), intent(out) :: hash
          end subroutine simple_checksum
        end interface

      end module simple_checksum_mod
