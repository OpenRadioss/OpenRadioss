Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    set_mod                 ../starter/share/modules1/set_mod.F
      !||--- called by ------------------------------------------------------
      !||    create_set_clause       ../starter/source/model/sets/create_set_clause.F
      !||    create_set_collect      ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_clause    ../starter/source/model/sets/create_setcol_clause.F
      !||    elt_box                 ../starter/source/model/sets/fill_clause_elt_box.F
      !||    fill_clause_elt_box     ../starter/source/model/sets/fill_clause_elt_box.F
      !||    fill_clause_node_box    ../starter/source/model/sets/fill_clause_node_box.F
      !||    fill_clause_rbody_box   ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    insert_clause_in_set    ../starter/source/model/sets/insert_clause_in_set.F
      !||    lectur                  ../starter/source/starter/lectur.F
      !||    node_box                ../starter/source/model/sets/fill_clause_node_box.F
      !||    rbody_box               ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    set_merge_simple        ../starter/source/model/sets/set_merge_simple.F
      !||    set_operator            ../starter/source/model/sets/set_operator.F
      !||    sort_set                ../starter/source/model/sets/sort_sets.F
      !||    surface_type            ../starter/source/model/sets/surface_type.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      MODULE SET_MOD
        USE SETDEF_MOD
        ! ------------
        ! definition of the operators
        INTEGER, PARAMETER :: SET_ADD = 1 !< add operator
        INTEGER, PARAMETER :: SET_DELETE = 2 !< delete operator
        INTEGER, PARAMETER :: SET_INTERSECT = 3 !< intersection operator
        INTEGER, PARAMETER :: SET_SURF_ALL = 4 !< surf_all operator (not used)
        INTEGER, PARAMETER :: SET_SURF_EXT = 5 !< surf_ext operator (not used)
        ! ------------
! SET
        TYPE(SET_)    ,TARGET,DIMENSION(:),ALLOCATABLE :: SET

      END MODULE SET_MOD
