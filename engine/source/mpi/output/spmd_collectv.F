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
C
C
Chd|====================================================================
Chd|  SPMD_COLLECTV                 source/mpi/output/spmd_collectv.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_COLLECTV(V, ITAB, WEIGHT,NODGLOB,SIZP0)
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
     .        V(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF0,MSGTYP,INFO,I,K,NG,N,
     .        EMPL,SDNODG(NUMNODM),FILEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DOUBLE PRECISION
     .        VGLOB(4,NUMNODM),RECGLOB(4,SIZP0)

      CHARACTER FILNAM*100,CYCLENUM*7

      DATA MSGOFF0/178/
      DATA MSGOFF/179/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      WRITE(CYCLENUM,'(I7.7)')NCYCLE
      FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.vdb'

      OPEN(UNIT=67,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

      FILEN = ROOTLEN+17
      IF (ISPMD/=0) THEN
        N = 0
        DO I = 1, NUMNOD
         IF (WEIGHT(I)==1) THEN
           N = N+1
           SDNODG(N) = NODGLOB(I)
           VGLOB(1,N) = ITAB(I)
           VGLOB(2,N) = V(1,I)
           VGLOB(3,N) = V(2,I)
           VGLOB(4,N) = V(3,I)
         END IF
        END DO
        MSGTYP=MSGOFF0
        CALL MPI_SEND(SDNODG,N,MPI_INTEGER,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        MSGTYP=MSGOFF
        CALL MPI_SEND(VGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
      ELSE
        DO I=1,NUMNOD
          IF (WEIGHT(I)==1) THEN
            EMPL = NODGLOB(I)
            RECGLOB(1,EMPL) = ITAB(I)
            RECGLOB(2,EMPL) =  V(1,I)
            RECGLOB(3,EMPL) =  V(2,I)
            RECGLOB(4,EMPL) =  V(3,I)
          ENDIF
        ENDDO

        DO K=2,NSPMD
            MSGTYP=MSGOFF0
            CALL MPI_RECV(SDNODG,NUMNODM,MPI_INTEGER,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)

            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,N,ierror)

            MSGTYP=MSGOFF
            CALL MPI_RECV(VGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)

            DO I=1,N
              EMPL = SDNODG(I)
              RECGLOB(1,EMPL) = VGLOB(1,I)
              RECGLOB(2,EMPL) = VGLOB(2,I)
              RECGLOB(3,EMPL) = VGLOB(3,I)
              RECGLOB(4,EMPL) = VGLOB(4,I)
            ENDDO

        END DO
C
        DO I = 1, NUMNODG
          WRITE(67,'(A,I10,I10,Z20,Z20,Z20)' )
     .   '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I),RECGLOB(3,I),RECGLOB(4,I)
        END DO
        WRITE (IOUT,1300)  FILNAM(1:FILEN)
        WRITE (ISTDO,1300) FILNAM(1:FILEN)
        CLOSE(UNIT=67)

      END IF
C
 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
#endif
      RETURN
      END
