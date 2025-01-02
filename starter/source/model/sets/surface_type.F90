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
      !||    surface_type_mod       ../starter/source/model/sets/surface_type.F90
      !||--- called by ------------------------------------------------------
      !||    insert_clause_in_set   ../starter/source/model/sets/insert_clause_in_set.F
      !||====================================================================
      module surface_type_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief get the kind of surface : /EXT or /ALL
      !||====================================================================
      !||    surface_type           ../starter/source/model/sets/surface_type.F90
      !||--- called by ------------------------------------------------------
      !||    insert_clause_in_set   ../starter/source/model/sets/insert_clause_in_set.F
      !||--- uses       -----------------------------------------------------
      !||    set_mod                ../starter/share/modules1/set_mod.F
      !||    surf_mod               ../starter/share/modules1/surf_mod.F
      !||====================================================================
        subroutine surface_type( empty_condition,clause_operator,clause,set )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use set_mod , only : set_add,set_delete,set_intersect
          use setdef_mod
          use surf_mod , only : ext_surf,all_surf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(in) :: empty_condition !< state of the set or clause (the set or the clause are empty, or not...)
          integer, intent(in) :: clause_operator !< operator ( &&, || , - , delete)
          type(set_), intent(in) :: clause !< operator ( &&, || , - , delete)
          type(set_), intent(inout) :: set !< operator ( &&, || , - , delete)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: old_value,new_value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        old_value = set%ext_all
        new_value = clause%ext_all
        if(.not.empty_condition) then ! the set already contains some data
          if(clause_operator==set_add) then ! add operator : new and old are merged
            if(old_value==all_surf.or.new_value==all_surf) set%ext_all = all_surf
          elseif(clause_operator==set_delete) then ! delete operator : new is removed from old
            if(old_value==all_surf.and.new_value==ext_surf) set%ext_all = all_surf
            if(old_value==ext_surf.and.new_value==all_surf) set%ext_all = ext_surf
            if(old_value==all_surf.and.new_value==all_surf) set%ext_all = all_surf
          elseif(clause_operator==set_intersect) then ! intersection operator : intersection of new and old
            if(old_value==ext_surf.and.new_value==all_surf) set%ext_all = ext_surf
            if(old_value==all_surf.and.new_value==ext_surf) set%ext_all = ext_surf
          endif
        else ! the set is empty
          if(clause_operator==set_add) then ! add operator : set is now = new 
            set%ext_all = new_value
          elseif(clause_operator==set_intersect) then ! intersection operator : the set was empty, intersection = null
            set%ext_all = 0
          endif
        endif

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine surface_type
      end module surface_type_mod
