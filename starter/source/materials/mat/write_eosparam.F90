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
      !||    write_eosparam         ../starter/source/materials/mat/write_eosparam.F90
      !||--- called by ------------------------------------------------------
      !||    write_matparam         ../starter/source/materials/mat/write_matparam.F
      !||--- calls      -----------------------------------------------------
      !||    write_mat_table        ../starter/source/materials/tools/write_mat_table.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE WRITE_EOSPARAM(EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE EOS_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Imnclude files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
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
      my_real ,DIMENSION(:), ALLOCATABLE :: RBUF
      INTEGER :: LENI, LENR
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      !INTEGER parameter
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
      ALLOCATE (RBUF(NFIX))
      ALLOCATE(IBUF(1))
      IBUF(1)=NFIX
      IAD = 1
        RBUF(IAD) = EOS%CV
      IAD = IAD+1
        RBUF(IAD) = EOS%CP
      IAD = IAD+1
      CALL WRITE_I_C(IBUF,1)
      CALL WRITE_DB(RBUF,NFIX)
      DEALLOCATE(RBUF)      

      ! write eos model title
      DO I=1,NCHARTITLE
        NAME(I) = ICHAR(EOS%TITLE(I:I))
      END DO
      CALL WRITE_C_C(NAME,NCHARTITLE)
      
      ! write eos parameter array
      IF (EOS%NUPARAM > 0) THEN
        CALL WRITE_DB(EOS%UPARAM ,EOS%NUPARAM)
      END IF      
      IF (EOS%NIPARAM > 0) THEN
        CALL WRITE_I_C(EOS%IPARAM ,EOS%NIPARAM)
      END IF      

      ! write eos law function
      IF (EOS%NFUNC > 0) THEN
        CALL WRITE_I_C(EOS%FUNC, EOS%NFUNC)
      END IF
      
      ! write eos law tables
      IF (EOS%NTABLE > 0) THEN
        LENI=0
        LENR=0
        CALL WRITE_MAT_TABLE(EOS%TABLE, EOS%NTABLE)
      END IF
!-----------
      RETURN
      END
