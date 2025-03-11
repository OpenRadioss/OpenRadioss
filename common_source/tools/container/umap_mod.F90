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
      !||    umap_mod               ../common_source/tools/container/umap_mod.F90
      !||--- called by ------------------------------------------------------
      !||    get_local_node_id      ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    get_local_shell_id     ../common_source/modules/connectivity.F90
      !||    init_global_node_id    ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    init_global_shell_id   ../common_source/modules/connectivity.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module umap_mod
        use iso_c_binding
        implicit none

        interface
          function create_umap() bind(C, name="cpp_create_umap")
            use iso_c_binding
            type(C_PTR) :: create_umap
          end function create_umap

          subroutine free_umap(umap_ptr) bind(C, name="cpp_free_umap")
            use iso_c_binding
            type(C_PTR), value :: umap_ptr
          end subroutine free_umap

          subroutine add_entry_umap(umap_ptr, key, value) bind(C, name="cpp_add_entry_umap")
            use iso_c_binding
            type(C_PTR), value :: umap_ptr
            integer(C_INT), value :: key
            integer(C_INT), value :: value
          end subroutine add_entry_umap

          function get_value_umap(umap_ptr, key, default_value) result(val) bind(C, name="cpp_get_value_umap")
            use iso_c_binding
            type(C_PTR), value :: umap_ptr
            integer(C_INT), value :: key
            integer(C_INT), value :: default_value
            integer(C_INT) :: val
          end function get_value_umap

          subroutine reserve_umap(umap_ptr, n) bind(C, name="cpp_reserve_umap")
            use iso_c_binding
            type(C_PTR), value :: umap_ptr
            ! size_t is typically mapped to C_SIZE_T
            integer(C_SIZE_T), value :: n
          end subroutine reserve_umap
        end interface

      contains

      !||====================================================================
      !||    add_entry        ../common_source/tools/container/umap_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine add_entry(m, key, value)
          type(C_PTR), intent(in) :: m
          integer, intent(in)     :: key, value
!$OMP CRITICAL
          call add_entry_umap(m, int(key, C_INT), int(value, C_INT))
!$OMP END CRITICAL
        end subroutine add_entry

      !||====================================================================
      !||    get_value   ../common_source/tools/container/umap_mod.F90
      !||====================================================================
        function get_value(m, key, default_value) result(val)
          type(C_PTR), intent(in) :: m
          integer, intent(in)     :: key, default_value
          integer                 :: val
          val = get_value_umap(m, int(key, C_INT), int(default_value, C_INT))
        end function get_value

      !||====================================================================
      !||    reserve_capacity       ../common_source/tools/container/umap_mod.F90
      !||--- called by ------------------------------------------------------
      !||    init_global_node_id    ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    init_global_shell_id   ../common_source/modules/connectivity.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine reserve_capacity(m, n)
          type(C_PTR), intent(in) :: m
          integer, intent(in)     :: n
!$OMP CRITICAL
          call reserve_umap(m, int(n, C_SIZE_T))
!$OMP END CRITICAL
        end subroutine reserve_capacity

      end module umap_mod
