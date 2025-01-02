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
      !||    spring_functions_mod   ../common_source/modules/spring_functions_mod.F90
      !||====================================================================
       module spring_functions_mod
#include "my_real.inc"
#include "mvsiz_p.inc"
        type chunk_of_table_type
          my_real, pointer :: X(:) !< X values for a chunk of a table
          my_real, pointer :: Y(:) !< Y values for a chunk of a table
        end type chunk_of_table_type
        type :: spring_functions_type
          integer, dimension(MVSIZ) :: Hi !< Hardening type for each spring, simimar to IECROU
          integer, dimension(MVSIZ) :: function_type !< Function type for each spring, similar to IFUNC
          type(chunk_of_table_type), dimension(MVSIZ) :: table !< Table for each spring, similar to X1DP, X2DP
        end type spring_functions_type
      end module spring_functions_mod
