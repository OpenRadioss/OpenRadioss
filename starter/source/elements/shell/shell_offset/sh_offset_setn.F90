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
!||    sh_offset_setn_mod   ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
!||--- called by ------------------------------------------------------
!||    shell_offsetp        ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||====================================================================
      module sh_offset_setn_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine compute nodal shell offset
!=======================================================================================================================
!||====================================================================
!||    sh_offset_setn   ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
!||--- called by ------------------------------------------------------
!||    shell_offsetp    ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine sh_offset_setn(nshell,numnod,ix_offset,sh_oset,oset_n,itagn)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero
          use precision_mod, only : WP
!
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
          integer, intent(in   )                      :: nshell        !< number of shell
          integer, intent(in   )                      :: numnod         !< number of node
          integer, intent(in   ),dimension(4,nshell)  :: ix_offset        !< shell connectivity
          integer, intent(inout),dimension(numnod)     :: itagn         !< itag work array
          real(kind=WP), intent(in   ),dimension(nshell)    :: sh_oset       !< elementary offset
          real(kind=WP), intent(inout),dimension(numnod)     :: oset_n        !< nodal offset
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k,n,nnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          itagn(1:numnod) = 0
          do i = 1, nshell
!------each node
            if (ix_offset(4,i)/=ix_offset(3,i)) then
              nnod = 4
            else
              nnod = 3
            end if
!
            do k = 1,nnod
              n = ix_offset(k,i)
              itagn(n) = itagn(n) + 1
              oset_n(n) = oset_n(n) + sh_oset(i)
            end do
          end do
!
          do n = 1, numnod
            if (itagn(n)==0) cycle
            oset_n(n) = oset_n(n)/itagn(n)
            if (oset_n(n)==zero) itagn(n)=0
          end do
!
        end subroutine sh_offset_setn
      end module sh_offset_setn_mod
