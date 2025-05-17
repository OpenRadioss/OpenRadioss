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
      !||    inter_save_candidate_mod   ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
      !||--- called by ------------------------------------------------------
      !||    i7trivox1                  ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||====================================================================
      module inter_save_candidate_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    inter_save_candidate   ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
      !||--- called by ------------------------------------------------------
      !||    i7trivox1              ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine inter_save_candidate( local_i_stok,j_stok,prov_n,prov_e, &
                                         pene,local_cand_n,local_cand_e )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use array_mod 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout) :: local_i_stok !< number of pairs S node/segment saved in the local_cand_n/e arrais
          integer, intent(in) :: j_stok !< number of pairs S node/segment of the current group
          integer, dimension(mvsiz), intent(in) :: prov_n !< list of potential S node
          integer, dimension(mvsiz), intent(in) :: prov_e !< list of potential segment
          my_real, dimension(mvsiz), intent(in) :: pene !< penetration
          type(array_type_int_1d), intent(inout) :: local_cand_n !< list of S node (local to a !$omp thread)
          type(array_type_int_1d), intent(inout) :: local_cand_e !< list of segment (local to a !$omp thread)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: k_stok
          integer :: my_old_size,my_size
          integer, dimension(:), allocatable :: tmp_array_1,tmp_array_2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        k_stok = 0
        ! ---------
        ! get the number of candidates --> pene/=0
        do i=1,j_stok
           if(pene(i)/=zero)THEN
             k_stok = k_stok + 1
           endif
         enddo
        ! ---------

        ! ---------
        if(k_stok>0) then
          ! ---------
          ! check if the size of the list of candidate is sufficient
          ! and increase the size if it is not the case
          if(local_i_stok+k_stok>local_cand_n%size_int_array_1d) then
            my_old_size = local_cand_n%size_int_array_1d
            my_size = nint((my_old_size+k_stok) * 1.25)
            allocate( tmp_array_1( my_size ) )
            allocate( tmp_array_2( my_size ) )
            tmp_array_1(1:my_old_size) = local_cand_n%int_array_1d(1:my_old_size)
            tmp_array_2(1:my_old_size) = local_cand_e%int_array_1d(1:my_old_size)
            call dealloc_1d_array(local_cand_n)
            call dealloc_1d_array(local_cand_e)
            call move_alloc(tmp_array_1,local_cand_n%int_array_1d)
            call move_alloc(tmp_array_2,local_cand_e%int_array_1d)
            local_cand_n%size_int_array_1d = my_size
            local_cand_e%size_int_array_1d = my_size
          endif
          ! ---------

          ! ---------
          ! save the list of S node & segment
          do i=1,j_stok
             if(pene(i)/=zero)THEN
               local_i_stok = local_i_stok + 1
               local_cand_n%int_array_1d(local_i_stok) = prov_n(I)
               local_cand_e%int_array_1d(local_i_stok) = prov_e(I)
             endif
           enddo
          ! ---------
        endif
        ! ---------
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter_save_candidate
      end module inter_save_candidate_mod
