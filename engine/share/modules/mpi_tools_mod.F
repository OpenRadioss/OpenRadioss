Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  MPI_TOOLS_MOD                 share/modules/mpi_tools_mod.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MPI_TOOLS_MOD 
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE MPI_MIN_REAL_STRUCT 
          my_real :: VAL
          INTEGER, DIMENSION(:), POINTER  :: TAB
          INTEGER :: LENGTH_TAB
          INTEGER, DIMENSION(:,:), POINTER :: BUFFER_TAB
          my_real, DIMENSION(:), POINTER :: BUFFER_VAL
          INTEGER, DIMENSION(:), POINTER :: RQ_SEND_VAL
          INTEGER, DIMENSION(:), POINTER :: RQ_SEND_TAB
          INTEGER, DIMENSION(:), POINTER :: RQ_RECV_VAL
          INTEGER, DIMENSION(:), POINTER :: RQ_RECV_TAB
      END TYPE MPI_MIN_REAL_STRUCT 

      CONTAINS

C
C Find the minimum value over all processor
C  - Asynchronous MPI communications: these routines are relevant only
C  for cases where a lot of computations is done between the two calls.
C 
C  TAB = ISPMD 
C  VAL = ...
C  CALL MPI_MIN_REAL_BEGIN(VAL,TAB,1,MY_STRUCT)
C  ! a lot of computations
C  CALL MPI_MIN_REAL_END(VAL2,TAB2,1,MY_STRUCT)
C  ! VAL2 is the min of VAL over the processors
C  ! TAB2 is the TAB of the processor that has the minimum value of VAL

Chd|====================================================================
Chd|  MPI_MIN_REAL_BEGIN            share/modules/mpi_tools_mod.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MPI_MIN_REAL_BEGIN(VAL,TAB,STAB,MY_STRUCT) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: VAL  ! value to be minimized
      INTEGER, INTENT(IN) :: STAB !size of integer tab
      INTEGER, INTENT(IN) :: TAB(STAB) !Tab of integer to send
      TYPE(MPI_MIN_REAL_STRUCT),  INTENT(INOUT) :: MY_STRUCT 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER P,IERROR
      INTEGER MSGTYP
#ifdef MPI
      INTEGER STATUS (MPI_STATUS_SIZE)

!     Loop on send and Recv : NSPMD*NSPMD communications
!     Since there is not collective asynchronous communication
!     with msmpi.

      ALLOCATE(MY_STRUCT%TAB(STAB),STAT=IERROR)
      ALLOCATE(MY_STRUCT%BUFFER_TAB(STAB,NSPMD),stat=IERROR)
      ALLOCATE(MY_STRUCT%BUFFER_VAL(NSPMD) ,stat=IERROR)
      ALLOCATE(MY_STRUCT%RQ_SEND_VAL(NSPMD),stat=IERROR)
      ALLOCATE(MY_STRUCT%RQ_RECV_VAL(NSPMD),stat=IERROR)
      ALLOCATE(MY_STRUCT%RQ_SEND_TAB(NSPMD),stat=IERROR)
      ALLOCATE(MY_STRUCT%RQ_RECV_TAB(NSPMD),stat=IERROR)

      MY_STRUCT%LENGTH_TAB = STAB
      MY_STRUCT%VAL = VAL 
      MY_STRUCT%TAB(1:STAB) = TAB(1:STAB)
      MY_STRUCT%BUFFER_VAL(ISPMD+1) = VAL 
      MY_STRUCT%BUFFER_TAB(1:STAB,ISPMD+1) = TAB(1:STAB)


      DO P = 1, NSPMD
        IF( P /= ISPMD +1 ) THEN
             MSGTYP = 17000 
             CALL MPI_IRECV(
     1        MY_STRUCT%BUFFER_VAL(P) ,1,REAL,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,MY_STRUCT%RQ_RECV_VAL(P),IERROR)

             MSGTYP = 17001 
             CALL MPI_IRECV(
     1        MY_STRUCT%BUFFER_TAB(1,P),MY_STRUCT%LENGTH_TAB,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,MY_STRUCT%RQ_RECV_TAB(P),IERROR)
        ENDIF
      ENDDO
      DO P = 1, NSPMD
        IF( P /= ISPMD +1 ) THEN
             MSGTYP = 17000 
             CALL MPI_ISEND(
     1        MY_STRUCT%VAL ,1,REAL,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,MY_STRUCT%RQ_SEND_VAL(P),IERROR)

             MSGTYP = 17001 
             CALL MPI_ISEND(
     1        MY_STRUCT%TAB,MY_STRUCT%LENGTH_TAB,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,MY_STRUCT%RQ_SEND_TAB(P),IERROR)
         ENDIF
      ENDDO

#endif
      RETURN
      END SUBROUTINE MPI_MIN_REAL_BEGIN

Chd|====================================================================
Chd|  MPI_MIN_REAL_END              share/modules/mpi_tools_mod.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MPI_MIN_REAL_END(VAL,TAB,STAB,MY_STRUCT) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: VAL  ! value to be minimized
      INTEGER, INTENT(IN) :: STAB !size of integer tab
      INTEGER, INTENT(INOUT) :: TAB(STAB) !Tab of integer to send
      TYPE(MPI_MIN_REAL_STRUCT),  INTENT(INOUT) :: MY_STRUCT 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER P,PMIN,IERROR
      INTEGER MSGTYP
#ifdef MPI
      INTEGER STATUS (MPI_STATUS_SIZE)

       ! WAIT SEND & RECV
      DO P=1,NSPMD
        IF(ISPMD + 1 /= P) THEN
          CALL MPI_WAIT(MY_STRUCT%RQ_SEND_VAL(P),STATUS,IERROR)
          CALL MPI_WAIT(MY_STRUCT%RQ_SEND_TAB(P),STATUS,IERROR)
          CALL MPI_WAIT(MY_STRUCT%RQ_RECV_VAL(P),STATUS,IERROR)
          CALL MPI_WAIT(MY_STRUCT%RQ_RECV_TAB(P),STATUS,IERROR)
        ENDIF
      ENDDO

      
      ! Find the minimum value of VAL and the processor that has it
      PMIN = 1
      VAL = MY_STRUCT%BUFFER_VAL(1) 
      DO P=2,NSPMD
        IF(VAL > MY_STRUCT%BUFFER_VAL(P)) THEN 
          VAL = MY_STRUCT%BUFFER_VAL(P)
          PMIN = P
        ENDIF
      ENDDO

      ! TAB <- TAB of the processor that has the mini. value of VAL
      TAB(1:STAB) = MY_STRUCT%BUFFER_TAB(1:STAB,PMIN)

      DEALLOCATE(MY_STRUCT%TAB)
      DEALLOCATE(MY_STRUCT%BUFFER_TAB)
      DEALLOCATE(MY_STRUCT%BUFFER_VAL )
      DEALLOCATE(MY_STRUCT%RQ_SEND_VAL)
      DEALLOCATE(MY_STRUCT%RQ_RECV_VAL)
      DEALLOCATE(MY_STRUCT%RQ_SEND_TAB)
      DEALLOCATE(MY_STRUCT%RQ_RECV_TAB)

#endif
      RETURN
      END SUBROUTINE MPI_MIN_REAL_END



      END MODULE MPI_TOOLS_MOD 

