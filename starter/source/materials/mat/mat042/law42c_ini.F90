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
!||    law42c_ini_mod   ../starter/source/materials/mat/mat042/law42c_ini.F90
!||--- called by ------------------------------------------------------
!||    c3init3          ../starter/source/elements/sh3n/coque3n/c3init3.F
!||    cinit3           ../starter/source/elements/shell/coque/cinit3.F
!||    cmaini3          ../starter/source/elements/sh3n/coquedk/cmaini3.F
!||====================================================================
      module law42c_ini_mod
      implicit none
      contains
        ! ======================================================================================================================
        ! \brief initialization UVAR of law42c
        ! ======================================================================================================================
!||====================================================================
!||    law42c_ini      ../starter/source/materials/mat/mat042/law42c_ini.F90
!||--- called by ------------------------------------------------------
!||    c3init3         ../starter/source/elements/sh3n/coque3n/c3init3.F
!||    cinit3          ../starter/source/elements/shell/coque/cinit3.F
!||    cmaini3         ../starter/source/elements/sh3n/coquedk/cmaini3.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine law42c_ini(nuvar,uvar,nel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : one
          use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer ,intent(in)                              :: nel       !< shell group size
          integer ,intent(in)                              :: nuvar  !< 1er dimension of uvar
          real(kind=WP),dimension(nel,nuvar),intent(inout) :: uvar      !< internal work array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          uvar(1:nel,3) = one
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine law42c_ini
      end module law42c_ini_mod
