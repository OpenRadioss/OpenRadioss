!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    update_pon_mod            ../engine/source/engine/node_spliting/update_pon.F90
!||--- called by ------------------------------------------------------
!||    apply_crack               ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
!||    resol                     ../engine/source/engine/resol.F
!||====================================================================
      module update_pon_mod
        implicit none
      contains
        !\brief replace old_ids with new_ids in the shell data structure
!||====================================================================
!||    update_pon_shells         ../engine/source/engine/node_spliting/update_pon.F90
!||--- called by ------------------------------------------------------
!||    apply_crack               ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    connectivity_mod          ../common_source/modules/connectivity.F90
!||    extend_array_mod          ../common_source/tools/memory/extend_array.F90
!||    my_alloc_mod              ../common_source/tools/memory/my_alloc.F90
!||    parith_on_mod             ../common_source/modules/parith_on_mod.F90
!||====================================================================
        subroutine update_pon_shells(old_node_id, elements, n, shell_list, new_numnod, ispmd, m, row_uid, row_procne)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use parith_on_mod
          use connectivity_mod
          use extend_array_mod
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: old_node_id           !< uid of the node to detach (debug trace only)
          type(connectivity_), intent(inout) ::  elements
          integer, intent(in) :: n                !< size of shell_list
          integer, dimension(n), intent(in) :: shell_list !< list of local shells to detach from the node
          integer, intent(in) :: new_numnod
          integer, intent(in) :: ispmd            !< 0-based local MPI rank; PROCNE = ispmd+1 for local rows
          integer, intent(in) :: m                !< total FSKY rows for N' (all migrating shells, canonical order)
          integer, dimension(m), intent(in) :: row_uid    !< shell user id of each canonical row (uid-sorted)
          integer, dimension(m), intent(in) :: row_procne !< 1-based owning rank of each canonical row
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, k, a
          integer :: shell_id, su
          integer :: total_new_rows, sfsky_old
          integer, dimension(:), allocatable :: new_adsky
          integer :: new_id
          integer :: numnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!          not tested when multiple nodes are detached at the same cycle, may not work
          if(ispmd < 0) return
          if(old_node_id < 1) then
            write(6,*) "Error in update_pon_shells: old_node_id < 1"
          end if

          numnod = elements%pon%sadsky - 1
          if(new_numnod > numnod + 1 .or. new_numnod < 1) then
            write(6,*) "Error in update_pon_shells: new_numnod out of bounds"
          end if

          !=================== debug ===================
          ! imin_fsky = elements%pon%iadc(1,1)
          ! imax_fsky = elements%pon%iadc(1,1)
          ! do i = 1, numelc
          !   do j = 1, 4
          !     if(elements%pon%iadc(j,i) < imin_fsky) then
          !       imin_fsky = elements%pon%iadc(j,i)
          !     endif
          !     if(elements%pon%iadc(j,i) > imax_fsky) then
          !       imax_fsky = elements%pon%iadc(j,i)
          !     endif
          !   enddo
          ! enddo
          !=================== End debug ===================

          ! N''s band has exactly m rows, one per migrating shell (local + remote), laid
          ! out in the GLOBAL canonical (uid-sorted) order given by row_uid/row_procne.
          ! Row k is a LOCAL row when row_procne(k)==ispmd+1 (this rank owns that shell)
          ! and a RECV row otherwise.  Building the same ordered band on every rank makes
          ! ASSPAR4 sum contributions in a decomposition-independent order (bitwise
          ! /PARITH/ON), and reduces to the 1-rank (all-local, uid-sorted) layout in mono.
          new_id = new_numnod ! the new id is always the last one
          total_new_rows = m

          call my_alloc(new_adsky, new_numnod + 1,"new adsky")
          new_adsky(1:new_numnod) = elements%pon%adsky(1:new_numnod)
          new_adsky(new_numnod + 1) = new_adsky(new_numnod) + total_new_rows
          call my_move_alloc(new_adsky,elements%pon%adsky, "adsky")
          elements%pon%sadsky = new_numnod + 1

          ! IADC of this rank's local migrating shells -> their canonical row in N''s band.
          do i = 1, n
            shell_id = shell_list(i)
            su = elements%shell%user_id(shell_id)
            do j = 1, 4
              if(elements%shell%nodes(j, shell_id) == new_id) then
                do a = 1, m
                  if (row_uid(a) == su) then
                    elements%pon%iadc(j,shell_id) = elements%pon%adsky(new_numnod) + a - 1
                    exit
                  end if
                end do
              end if
            end do
          end do

          ! extend FSKY and PROCNE by total_new_rows
          sfsky_old = elements%pon%sfsky / 8
          i = sfsky_old + total_new_rows
          call extend_array(elements%pon%fsky, 8, sfsky_old, 8, i)
          elements%pon%sfsky = i * 8
          elements%pon%fsky(1:8, 1:i) = 0

          if (total_new_rows > 0) then
            call extend_array(elements%pon%procne, sfsky_old, i)
            ! Per-row PROCNE from the canonical order: local rows = ispmd+1, recv rows =
            ! the owning rank of that contribution.  adsky(new_numnod) = sfsky_old + 1, so
            ! canonical row k lives at FSKY index sfsky_old + k.
            do k = 1, m
              elements%pon%procne(sfsky_old + k) = row_procne(k)
            end do
          end if



        end subroutine update_pon_shells
      end module update_pon_mod
