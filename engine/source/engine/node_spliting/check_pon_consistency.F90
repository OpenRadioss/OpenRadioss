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
!||    check_pon_consistency_mod   ../engine/source/engine/node_spliting/check_pon_consistency.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach           ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||====================================================================
      module check_pon_consistency_mod
        implicit none
      contains
!! \brief diagnostic audit: every shell-corner IADC entry must fall inside its node's ADSKY range in FSKY
!! \details for each local shell corner pointing to a local node N, check that IADC(j,s) lies in
!!          [ADSKY(N), ADSKY(N+1)-1]; also flag any FSKY slot claimed by more than one shell corner.
!!          Read-only; intended to be called right after node-splitting to locate ADSKY/IADC mismatches.
!||====================================================================
!||    check_pon_consistency   ../engine/source/engine/node_spliting/check_pon_consistency.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach       ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod        ../common_source/modules/connectivity.F90
!||    nodal_arrays_mod        ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine check_pon_consistency(nodes, elements, ispmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use nodal_arrays_mod, only : nodal_arrays_
          use connectivity_mod, only : connectivity_
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_),  intent(in) :: nodes    !< nodal arrays
          type(connectivity_),  intent(in) :: elements !< element connectivity (shells + parith/on data)
          integer,              intent(in) :: ispmd    !< local MPI rank (0-based), used only for diagnostic prints
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: s, j, n, lo, hi, k, numelc, numnod, nfsky
          integer :: n_mismatch, n_collision
          integer, dimension(:), allocatable :: claim_count
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (nodes%iparith == 0) return

          numnod = nodes%numnod
          numelc = size(elements%shell%nodes, 2)
          nfsky  = elements%pon%sfsky / 8

          allocate(claim_count(nfsky))
          claim_count = 0

          n_mismatch = 0
          n_collision = 0

          do s = 1, numelc
            do j = 1, 4
              n = elements%shell%nodes(j, s)
              if (n < 1 .or. n > numnod) cycle  ! not a local node (ghost / unused corner)

              k = elements%pon%iadc(j, s)
              lo = elements%pon%adsky(n)
              hi = elements%pon%adsky(n + 1) - 1

              if (k < lo .or. k > hi) then
                n_mismatch = n_mismatch + 1
                write(*,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
                  '[PON_AUDIT][rank ', ispmd, '] MISMATCH: node_id=', n, ' node_uid=', nodes%itab(n), &
                  ' adsky_lo=', lo, ' adsky_hi=', hi, ' shell_id=', s, ' shell_uid=', elements%shell%user_id(s), &
                  ' corner=', j, ' iadc=', k
                flush(6)
              end if

              if (k >= 1 .and. k <= nfsky) then
                claim_count(k) = claim_count(k) + 1
              end if
            end do
          end do

          do k = 1, nfsky
            if (claim_count(k) > 1) then
              n_collision = n_collision + 1
              write(*,'(a,i0,a,i0,a,i0)') &
                '[PON_AUDIT][rank ', ispmd, '] FSKY_COLLISION: fsky_slot=', k, ' claimed_by=', claim_count(k)
              flush(6)
            end if
          end do

          if (n_mismatch > 0 .or. n_collision > 0) then
            write(*,'(a,i0,a,i0,a,i0)') '[PON_AUDIT][rank ', ispmd, '] summary: mismatches=', n_mismatch, &
              ' collisions=', n_collision
            flush(6)
          end if

          deallocate(claim_count)

        end subroutine check_pon_consistency
      end module check_pon_consistency_mod
