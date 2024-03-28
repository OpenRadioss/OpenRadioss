!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!                                                   PROCEDURES
! ======================================================================================================================
! SPMD exchange necessary for option /ALE/GRID/MASSFLOW
!  gathering  domain boundaries X_MIN_MAX (for main flow) and X_MIN_MAX_GRID (for ALE grid points)
!
      SUBROUTINE SPMD_EXCH_MASSFLOW_DATA4( DOMAIN_DATA, NSPMD )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
         use ALE_MOD
         use constant_mod , only: zero,ep20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included file
! ----------------------------------------------------------------------------------------------------------------------
         implicit none
#include "my_real.inc"
#ifdef MPI
#include "mpif.h"
#endif
#include      "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
         TYPE(MASSFLOW_DATA_),INTENT(INOUT)::DOMAIN_DATA
         INTEGER,INTENT(IN)::NSPMD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
         INTEGER I, LENCOM
         INTEGER IERROR, MSGTYP, MSGOFF, NBIRECV, P
         INTEGER ::  STATUS(MPI_STATUS_SIZE),REQ_SB(NSPMD),&
         &REQ_RB(NSPMD),IRINDEXI(NSPMD),&
         &REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),&
         &REQ_RC(NSPMD),REQ_SC(NSPMD),&
         &ISINDEXI(NSPMD)
         INTEGER :: LOC_PROC, ISIZE
         my_real :: RBUF(12,NSPMD)
         DATA MSGOFF/2205/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
         IF(NSPMD == 1)RETURN
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!$OMP SINGLE
         LOC_PROC=ISPMD+1
         RBUF(1:3,1:NSPMD)=EP20
         RBUF(4:6,1:NSPMD)=-EP20
         RBUF(7:9,1:NSPMD)=EP20
         RBUF(10:12,1:NSPMD)=-EP20
         RBUF(1:6,LOC_PROC) = DOMAIN_DATA%X_MIN_MAX(1:6)
         RBUF(7:12,LOC_PROC) = DOMAIN_DATA%X_MIN_MAX_GRID(1:6)
         ISIZE=12
         !-------------------------------------------!
         ! SENDING %X_MIN_MAX(1:6) & X_MIN_MAX(1:6)  !
         !-------------------------------------------!
         DO P = 1, NSPMD
            IF(P /= LOC_PROC) THEN
               MSGTYP = MSGOFF
               CALL MPI_ISEND(RBUF(1,LOC_PROC),ISIZE,REAL,IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_SB(P),IERROR)
            ENDIF
         ENDDO
         !-------------------------------------------!
         ! RECIEVING %X_MIN_MAX(1:6) & X_MIN_MAX(1:6)!
         !-------------------------------------------!
         NBIRECV=0
         DO P = 1, NSPMD
            IF(LOC_PROC /= P) THEN
               NBIRECV=NBIRECV+1
               IRINDEXI(NBIRECV)=P
               MSGTYP = MSGOFF
               CALL MPI_RECV(RBUF(1,P), ISIZE, REAL, IT_SPMD(P), MSGTYP, MPI_COMM_WORLD, STATUS, IERROR)
            ENDIF
         ENDDO
         !-------------------------------------------!
         !     MPI_WAITING                           !
         !-------------------------------------------!
         DO P = 1, NSPMD
            IF(P/=LOC_PROC) THEN
               CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
            ENDIF
         ENDDO

         !-------------------------------------------!
         ! COMPUTE AVERAGE ON CURRENT DOMAIN         !
         !-------------------------------------------!
         DOMAIN_DATA%X_MIN_MAX(1:3)=EP20
         DOMAIN_DATA%X_MIN_MAX(4:6)=-EP20
         DOMAIN_DATA%X_MIN_MAX_GRID(1:3)=EP20
         DOMAIN_DATA%X_MIN_MAX_GRID(4:6)=-EP20

         DO P=1,NSPMD
            DOMAIN_DATA%X_MIN_MAX(1) = MIN(DOMAIN_DATA%X_MIN_MAX(1), RBUF(1,P))
            DOMAIN_DATA%X_MIN_MAX(2) = MIN(DOMAIN_DATA%X_MIN_MAX(2), RBUF(2,P))
            DOMAIN_DATA%X_MIN_MAX(3) = MIN(DOMAIN_DATA%X_MIN_MAX(3), RBUF(3,P))
            DOMAIN_DATA%X_MIN_MAX(4) = MAX(DOMAIN_DATA%X_MIN_MAX(4), RBUF(4,P))
            DOMAIN_DATA%X_MIN_MAX(5) = MAX(DOMAIN_DATA%X_MIN_MAX(5), RBUF(5,P))
            DOMAIN_DATA%X_MIN_MAX(6) = MAX(DOMAIN_DATA%X_MIN_MAX(6), RBUF(6,P))
            !
            DOMAIN_DATA%X_MIN_MAX_GRID(1) = MIN(DOMAIN_DATA%X_MIN_MAX_GRID(1), RBUF(7,P))
            DOMAIN_DATA%X_MIN_MAX_GRID(2) = MIN(DOMAIN_DATA%X_MIN_MAX_GRID(2), RBUF(8,P))
            DOMAIN_DATA%X_MIN_MAX_GRID(3) = MIN(DOMAIN_DATA%X_MIN_MAX_GRID(3), RBUF(9,P))
            DOMAIN_DATA%X_MIN_MAX_GRID(4) = MAX(DOMAIN_DATA%X_MIN_MAX_GRID(4), RBUF(10,P))
            DOMAIN_DATA%X_MIN_MAX_GRID(5) = MAX(DOMAIN_DATA%X_MIN_MAX_GRID(5), RBUF(11,P))
            DOMAIN_DATA%X_MIN_MAX_GRID(6) = MAX(DOMAIN_DATA%X_MIN_MAX_GRID(6), RBUF(12,P))
         ENDDO

!$OMP END SINGLE
!-----------------------------------------------
#endif
         RETURN
      END SUBROUTINE
