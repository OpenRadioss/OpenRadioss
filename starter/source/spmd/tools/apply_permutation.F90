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
!||    apply_permutation_mod   ../starter/source/spmd/tools/apply_permutation.F90
!||--- called by ------------------------------------------------------
!||    lectur                  ../starter/source/starter/lectur.F
!||====================================================================
      module apply_permutation_mod
        implicit none
        interface apply_permutation
          module procedure apply_permutation_elem
          module procedure apply_permutation_with_type
        end interface apply_permutation        
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Permute the entity according to the given permutation and the type of entity
!||====================================================================
!||    apply_permutation_with_type   ../starter/source/spmd/tools/apply_permutation.F90
!||====================================================================
        subroutine apply_permutation_with_type(my_type,entity_nb,entity_type,entity,permutation_s,permutation)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: my_type !< type of the entity to be permuted
          integer, intent(in) :: entity_nb !< number of entity
          integer, dimension(entity_nb), intent(in) :: entity_type !< entity type          
          integer, dimension(entity_nb), intent(inout) :: entity !< entity to be permuted
          integer, intent(in) :: permutation_s !< size of the permutation
          integer, dimension(2*permutation_s), intent(in) :: permutation !< permutation to be applied
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,my_index
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,entity_nb
            if(my_type==entity_type(i)) then
              entity(i) = permutation( entity(i) + permutation_s ) ! apply the permutation to the entity
            endif
          enddo
        end subroutine apply_permutation_with_type
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Permute the entity
!||====================================================================
!||    apply_permutation_elem   ../starter/source/spmd/tools/apply_permutation.F90
!||====================================================================
        subroutine apply_permutation_elem(entity_nb,entity)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: entity_nb !< number of entity
          integer, dimension(2*entity_nb), intent(inout) :: entity !< entity to be permuted
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,my_index
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,entity_nb
            my_index = entity(i) + entity_nb
            entity(my_index) = i ! apply the permutation to the entity   
          enddo
        end subroutine apply_permutation_elem    
      end module apply_permutation_mod
