Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SPMD_FLUSH_ACCEL              source/mpi/output/spmd_flush_accel.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRINT_CHECKSUM                source/mpi/output/spmd_flush_accel.F
Chd|====================================================================
      SUBROUTINE SPMD_FLUSH_ACCEL(A, ITAB, WEIGHT,NODGLOB,SIZP0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
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
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*),SIZP0
C     REAL
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF0,MSGTYP,INFO,I,K,NG,N,
     .        EMPL                        
      INTEGER, DIMENSION(:), ALLOCATABLE :: SDNODG
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::
     .        AGLOB,RECGLOB

      DATA MSGOFF0/176/
      DATA MSGOFF/177/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

        IF(ISPMD == 0) THEN
          ALLOCATE(RECGLOB(4,SIZP0))
        ENDIF

        ALLOCATE(SDNODG(MAX(NUMNOD,NUMNODM)))
        ALLOCATE(AGLOB(4,MAX(NUMNODM,NUMNOD)))

        IF (ISPMD/=0) THEN
          N = 0
          DO I = 1, NUMNOD
            IF (WEIGHT(I)==1) THEN
             N = N+1
             SDNODG(N) = NODGLOB(I)
             AGLOB(1,N) = ITAB(I)
             AGLOB(2,N) = A(1,I)
             AGLOB(3,N) = A(2,I)
             AGLOB(4,N) = A(3,I)
            END IF
          END DO
          MSGTYP=MSGOFF0
          CALL MPI_SEND(SDNODG,N,MPI_INTEGER,
     .                  IT_SPMD(1),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          MSGTYP=MSGOFF
          CALL MPI_SEND(AGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                  IT_SPMD(1),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ELSE ! ISPMD == 0

          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN
              EMPL = NODGLOB(I)
              RECGLOB(1,EMPL) = ITAB(I)
              RECGLOB(2,EMPL) =  A(1,I)
              RECGLOB(3,EMPL) =  A(2,I)
              RECGLOB(4,EMPL) =  A(3,I)
             ENDIF
          ENDDO
          
          DO K=2,NSPMD
            MSGTYP=MSGOFF0
            CALL MPI_RECV(SDNODG,NUMNODM,MPI_INTEGER,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,N,ierror)
            MSGTYP=MSGOFF
            CALL MPI_RECV(AGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            DO I=1,N
              EMPL = SDNODG(I)
              RECGLOB(1,EMPL) = AGLOB(1,I)
              RECGLOB(2,EMPL) = AGLOB(2,I)
              RECGLOB(3,EMPL) = AGLOB(3,I)
              RECGLOB(4,EMPL) = AGLOB(4,I)
            ENDDO

          END DO
          CALL PRINT_CHECKSUM(RECGLOB,NUMNODG,3) 
        ENDIF
        IF(ALLOCATED(SDNODG))  DEALLOCATE(SDNODG)
        IF(ALLOCATED(AGLOB))   DEALLOCATE(AGLOB)
        IF(ALLOCATED(RECGLOB)) DEALLOCATE(RECGLOB)

C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  PRINT_CHECKSUM                source/mpi/output/spmd_flush_accel.F
Chd|-- called by -----------
Chd|        SPMD_FLUSH_ACCEL              source/mpi/output/spmd_flush_accel.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRINT_CHECKSUM(A,SIZ1,SIZ2)
C Prints Adler 32 checksum of A(1:SIZ2,1:SIZ1) in *1.out file 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SIZ1,SIZ2
      my_real, INTENT(IN) :: A(SIZ2,SIZ1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP
      INTEGER :: I,CHECKSUM,ROOT,S1,S2,TWO_POWER_16
      INTEGER :: PREC ! simple or double precision
      INTEGER :: SIZ
C-----------------------------------------------
      ROOT = 65521
      TWO_POWER_16 = 65536
      S1 = 1
      S2 = 0
      PREC = 2
C  If A is in simple precision
      IF(SIZEOF(A(1,1)) == 4) PREC = 1 
      SIZ = SIZ1*SIZ2 * PREC
      ALLOCATE(TMP(SIZ))
      TMP(1:SIZ) = 0
c     Convert A in integer
      TMP = TRANSFER(A(1:SIZ2,1:SIZ1),S1,SIZ)
      DO I = 1,SIZ
        S1 = MOD(S1 + TMP(I),ROOT)
        S2 = MOD(S1 + S2    ,ROOT)
      ENDDO
C     (s2 << 16) | s1
      CHECKSUM = IOR(S2 * TWO_POWER_16,S1)
      DEALLOCATE(TMP)
      WRITE(IOUT,*) NCYCLE, "CHECKSUM:",CHECKSUM
c     WRITE(6,*)    NCYCLE, "CHECKSUM:",CHECKSUM

      END

