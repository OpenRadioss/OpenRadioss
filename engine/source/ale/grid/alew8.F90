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
      !||    alew8_mod   ../engine/source/ale/grid/alew8.F90
      !||--- called by ------------------------------------------------------
      !||    alewdx      ../engine/source/ale/grid/alewdx.F
      !||====================================================================
      module alew8_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief ALE grid formulation /ALE/GRID/LAGRANGE
!! \details
      !||====================================================================
      !||    alew8          ../engine/source/ale/grid/alew8.F90
      !||--- called by ------------------------------------------------------
      !||    alewdx         ../engine/source/ale/grid/alewdx.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine alew8(SV, SW, V , W ,  NODFT  ,NODLT, NUMNOD, NALE)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: NUMNOD,SW,SV
      INTEGER, INTENT(IN) :: NALE(NUMNOD)
      my_real, INTENT(IN) :: V(3,SV/3)
      my_real, INTENT(INOUT) :: W(3,SW/3)
      INTEGER, INTENT(IN) :: NODFT, NODLT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i ! looop
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      DO I = NODFT, NODLT
         IF(IABS(NALE(I)) == 1) THEN
            ! lagrangian framework
            W(1,I)=V(1,I)
            W(2,I)=V(2,I)
            W(3,I)=V(3,I)

         ELSEIF(NALE(I) == 0)THEN
            ! lagrangian framework
            W(1,I)=V(1,I)
            W(2,I)=V(2,I)
            W(3,I)=V(3,I)
         ELSE
            ! eulerian framework
            W(1,I)=ZERO
            W(2,I)=ZERO
            W(3,I)=ZERO
         ENDIF
      ENDDO
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine alew8
      end module alew8_mod
