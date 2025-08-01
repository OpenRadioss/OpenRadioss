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
!||    chk_shell_offset_mod   ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||====================================================================
      module chk_shell_offset_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine check if there is shell offset in the model
!=======================================================================================================================
!||====================================================================
!||    chk_shell_offset   ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
!||--- called by ------------------------------------------------------
!||    lectur             ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine chk_shell_offset(                                           &
          ngroup,    nparg,      iparg,        npropg,            &
          numgeo,      geo,    ioffset)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,half
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                         :: ngroup           !< number of elem group
          integer, intent (in   )                         :: nparg            !< 1er dim of iparg
          integer, intent (in   )                         :: npropg           !< 1er dim of geo
          integer, intent (in   )                         :: numgeo           !< number of prop
          integer, intent (in   ) ,dimension(nparg,ngroup):: iparg            !< elem group array
          integer, intent (inout)                         :: ioffset          !< flag for offset treatment
          real(kind=WP), intent (in   ),dimension(npropg,numgeo):: geo              !< property array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: igtyp,ity,pid,ifoset,ng
          real(kind=WP) :: shelloff
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ifoset = 0
          do  ng=1,ngroup
            ity=iparg(5,ng)
            igtyp  = iparg(38,ng)
            if (igtyp == 0.or.(ity /= 3 .and. ity /= 7) ) cycle
            pid =iparg(62,ng)
            shelloff = zero
            select case(igtyp)
             case (1,9,10,11,16)
              shelloff = geo(199,pid)
             case (17,51,52)
              shelloff = half + geo(199,pid)
            end select
            if (shelloff/=zero) then
              ifoset = 1
              cycle
            end if
          end do
          if (ifoset == 0) ioffset=0
!-----------
        end subroutine chk_shell_offset

      end module chk_shell_offset_mod

