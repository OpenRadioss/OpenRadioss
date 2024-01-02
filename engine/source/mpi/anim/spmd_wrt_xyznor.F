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
Chd|  SPMD_WRT_XYZNOR               source/mpi/anim/spmd_wrt_xyznor.F
Chd|-- called by -----------
Chd|        XYZNOR                        source/output/anim/generate/xyznor.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE SPMD_WRT_XYZNOR(XNORM,WEIGHT,NODGLOB,NUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   XNORM(3,*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBUF,RNGLOB
      DATA MSGOFF/7030/
      DATA MSGOFF2/7031/
       
C   Tableau utilise par proc 0
       my_real
     . S3000,S,XN1,XN2,XN3,IXNN
C-----------------------------------------------
       ALLOCATE(IBUF(4,NUMNODM))
       ALLOCATE(RNGLOB(3,NUM))

       S3000 = THREE1000         


       IXNN = S3000
       IF(FMT_ANI==4)IXNN=0

       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             SIZ = SIZ + 1
             IBUF(1,SIZ) = NODGLOB(I)
             S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
             IF(S/=ZERO)THEN
               S = S3000 / S
               XN1 = XNORM(1,I) * S
               XN2 = XNORM(2,I) * S
               XN3 = XNORM(3,I) * S
               IBUF(2,SIZ) = XN1
               IBUF(3,SIZ) = XN2
               IBUF(4,SIZ) = XN3
             ELSE
               IBUF(2,SIZ) = 0
               IBUF(3,SIZ) = 0
               IBUF(4,SIZ) = IXNN
             END IF
           END IF
         END DO

C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

        MSGTYP = MSGOFF2 
        CALL MPI_SEND(IBUF,4*SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE
          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN
              NG = NODGLOB(I)
              S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
              IF(S/=ZERO)THEN
               S = S3000 / S
               XN1 = XNORM(1,I) * S
               XN2 = XNORM(2,I) * S
               XN3 = XNORM(3,I) * S
               RNGLOB(1,NG) = XN1
               RNGLOB(2,NG) = XN2
               RNGLOB(3,NG) = XN3
              ELSE
               RNGLOB(1,NG) = 0
               RNGLOB(2,NG) = 0
               RNGLOB(3,NG) = IXNN
              END IF 
            END IF
          ENDDO

          DO I=2,NSPMD

            MSGTYP = MSGOFF2

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

         
            NREC = SIZ/4

            DO K = 1, NREC
              NG = IBUF(1,K)
              RNGLOB(1,NG) = IBUF(2,K)
              RNGLOB(2,NG) = IBUF(3,K)
              RNGLOB(3,NG) = IBUF(4,K)
            ENDDO
          ENDDO

          CALL WRITE_S_C(RNGLOB,3*NUMNODG)

       ENDIF

       DEALLOCATE(IBUF)
       DEALLOCATE(RNGLOB)

#endif
       RETURN
       END
