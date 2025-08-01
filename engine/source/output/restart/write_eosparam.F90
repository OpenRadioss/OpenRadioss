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
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Write parameters of EOS data structure
!! \details
!||====================================================================
!||    write_eosparam         ../engine/source/output/restart/write_eosparam.F90
!||--- called by ------------------------------------------------------
!||    write_matparam         ../engine/source/output/restart/write_matparam.F
!||--- calls      -----------------------------------------------------
!||    write_c_c              ../common_source/tools/input_output/write_routtines.c
!||    write_db               ../common_source/tools/input_output/write_db.F
!||    write_i_c              ../common_source/tools/input_output/write_routtines.c
!||    write_mat_table        ../engine/source/materials/tools/write_mat_table.F
!||--- uses       -----------------------------------------------------
!||    eos_param_mod          ../common_source/modules/mat_elem/eos_param_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||====================================================================
      SUBROUTINE WRITE_EOSPARAM(EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE EOS_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
      use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Imnclude files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      TYPE(EOS_PARAM_) ,INTENT(IN) :: EOS
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: I,IAD,NFIX,NUPARAM,NIPARAM,NUMTABL,NUMFUNC
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
      real(kind=WP) ,DIMENSION(:), ALLOCATABLE :: RBUF
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      !INTEGER parameters
      NFIX = 6
      ALLOCATE (IBUF(NFIX + 1))
      IAD = 1
      IBUF(IAD) = NFIX
      IAD = IAD+1
        IBUF(IAD) = EOS%NUPARAM
      IAD = IAD+1
        IBUF(IAD) = EOS%NIPARAM
      IAD = IAD+1
        IBUF(IAD) = EOS%NFUNC
      IAD = IAD+1
        IBUF(IAD) = EOS%NTABLE
      IAD = IAD+1
        IBUF(IAD) = EOS%ISFLUID
      IAD = IAD+1
      CALL WRITE_I_C(IBUF,NFIX+1)
      DEALLOCATE(IBUF)

      !REAL parameter
      NFIX = 2
      ALLOCATE(IBUF(1))
      IBUF(1) = NFIX !size
      ALLOCATE (RBUF(NFIX))
      IAD = 1
        RBUF(IAD) = EOS%CV
      IAD = IAD+1
        RBUF(IAD) = EOS%CP
      IAD = IAD+1
      CALL WRITE_I_C(IBUF,1)
      CALL WRITE_DB(RBUF,NFIX)
      DEALLOCATE(RBUF)
      DEALLOCATE(IBUF)

      ! write eos model title
      DO I=1,NCHARTITLE
        NAME(I) = ICHAR(EOS%TITLE(I:I))
      END DO
      CALL WRITE_C_C(NAME,NCHARTITLE)
      
      ! write eos parameter array
      NUPARAM = EOS%NUPARAM
      NIPARAM = EOS%NIPARAM
      IF (NUPARAM > 0) THEN
        CALL WRITE_DB(EOS%UPARAM ,NUPARAM)
      END IF      
      IF (NIPARAM > 0) THEN
        CALL WRITE_I_C(EOS%IPARAM ,NIPARAM)
      END IF      

      ! write eos law function
      NUMFUNC  = EOS%NFUNC
      IF (NUMFUNC > 0) THEN
        CALL WRITE_I_C(EOS%FUNC, NUMFUNC)
      END IF
      
      ! write eos law tables
      NUMTABL  = EOS%NTABLE
      IF (NUMTABL > 0) THEN
        CALL WRITE_MAT_TABLE(EOS%TABLE, NUMTABL)
      END IF
!-----------
      RETURN
      end subroutine WRITE_EOSPARAM
