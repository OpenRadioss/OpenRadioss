Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    upgrade_ixint    ../starter/source/interfaces/interf1/upgrade_ixint.F
!||--- called by ------------------------------------------------------
!||    i20ini3          ../starter/source/interfaces/inter3d1/i20ini3.F
!||    inint3           ../starter/source/interfaces/inter3d1/inint3.F
!||    lectur           ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    inter_cand_mod   ../starter/share/modules1/inter_cand_mod.F
!||====================================================================
        SUBROUTINE UPGRADE_IXINT(INTER_CAND,NELEMINT,NEW_SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_CAND_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NELEMINT !< number of pair of candidate
        INTEGER, INTENT(in) :: NEW_SIZE !< number of pair of candidate for the current interface
        TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND !< structure of pair of candidate
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER(KIND=8) :: ALLOC_SIZE
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: TEMP
C-----------------------------------------------
C       Update logical size
        INTER_CAND%S_IXINT_2 = NELEMINT + NEW_SIZE
C
C       Skip reallocation if the allocated array is already large enough
        IF (INTER_CAND%S_IXINT_2 <= INT(SIZE(INTER_CAND%IXINT,2),KIND=8)) RETURN
C
C       Oversize: use at least double the current allocated size
        ALLOC_SIZE = MAX(INT(INTER_CAND%S_IXINT_2,KIND=8), 2*INT(SIZE(INTER_CAND%IXINT,2),KIND=8))
        ALLOC_SIZE = MAX(ALLOC_SIZE, 32000_8) ! Ensure at least one element is allocated to avoid zero-size allocation
C
        ALLOCATE(TEMP(INTER_CAND%S_IXINT_1,ALLOC_SIZE))
        TEMP(1:INTER_CAND%S_IXINT_1,1:NELEMINT) = INTER_CAND%IXINT(1:INTER_CAND%S_IXINT_1,1:NELEMINT)
        CALL MOVE_ALLOC(FROM=TEMP, TO=INTER_CAND%IXINT)
        RETURN

        END SUBROUTINE UPGRADE_IXINT