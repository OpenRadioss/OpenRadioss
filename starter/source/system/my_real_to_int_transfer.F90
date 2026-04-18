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
!||    my_real_tot_int_transfer_mod   ../starter/source/system/my_real_to_int_transfer.F90
!||--- called by ------------------------------------------------------
!||    lectur                         ../starter/source/starter/lectur.F
!||====================================================================
      module my_real_tot_int_transfer_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Convert real(kind=WP) array to integer array by transfer intrinsic function
!! \details 
!||====================================================================
!||    my_real_tot_int_transfer   ../starter/source/system/my_real_to_int_transfer.F90
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_real_tot_int_transfer(numel,my_real_eani,int_eani)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
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
          integer, intent(in) :: numel !< number of entity
          real(kind=WP), dimension(numel), intent(in) :: my_real_eani
          integer, dimension(numel), intent(inout) :: int_eani
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n_entity
          integer, dimension(1) :: mold1_int
          integer, dimension(2) :: mold2_int
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MYREAL8
          n_entity = numel/2 + mod(numel,2)
          j = 0
          do i=1,n_entity        
            mold2_int = transfer(my_real_eani(i),mold2_int)
            j = j + 1
            int_eani(j) = mold2_int(1)
            if(j<numel) then
              j = j + 1
              int_eani(j) = mold2_int(2)
            endif
          end do      
#else
          n_entity = numel
          do i=1,numel
            mold1_int = transfer(my_real_eani(i),mold1_int)
            int_eani(i) = mold1_int(1)
          end do
#endif  
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine my_real_tot_int_transfer
      end module my_real_tot_int_transfer_mod
