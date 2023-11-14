Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPMD_GATHERITAB               source/mpi/anim/spmd_gatheritab.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
       SUBROUTINE SPMD_GATHERITAB(V,WEIGHT,NODGLOB,NUM)
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
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER
     .   V(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      INTEGER, DIMENSION(:,:) , ALLOCATABLE :: BUFSR 
      INTEGER, DIMENSION(:) , ALLOCATABLE :: XGLOB
C-----------------------------------------------
      DATA MSGOFF/7018/
      DATA MSGOFF2/7019/


C-----------------------------------------------
       ALLOCATE(BUFSR(2,NUMNODM))
       ALLOCATE(XGLOB(NUM))
C-----------------------------------------------
       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             SIZ = SIZ + 1
             BUFSR(1,SIZ) = NODGLOB(I)
             BUFSR(2,SIZ) = V(I) 
           END IF
         END DO


         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFSR,2*SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE
          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN
              NG = NODGLOB(I)
              XGLOB(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(BUFSR,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
         
            NREC = SIZ/2
 
            DO K = 1, NREC
              NG = BUFSR(1,K)
              XGLOB(NG) = BUFSR(2,K)
            ENDDO
          ENDDO
          CALL WRITE_I_C(XGLOB,NUMNODG)

       ENDIF
       DEALLOCATE(BUFSR)
       DEALLOCATE(XGLOB)

#endif
       RETURN
       END
