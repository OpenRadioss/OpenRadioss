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
Chd|  SPMD_GATHERF                  source/mpi/anim/spmd_gatherf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
       SUBROUTINE SPMD_GATHERF(V,WEIGHT,NODGLOB,NUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       REAL
     .   V(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/7014/
      DATA MSGOFF2/7015/
       REAL, DIMENSION(:) , ALLOCATABLE :: BUFSR
       INTEGER, DIMENSION(:) , ALLOCATABLE :: IBUF
C   Tableau utilise par proc 0
       REAL, DIMENSION(:) , ALLOCATABLE :: XGLOB
C=======================================================================
       ALLOCATE(BUFSR(NUMNODM), IBUF(NUMNODM))
       ALLOCATE(XGLOB(NUM))
  
          XGLOB = 1000000
          bufsr = 100000      
  
       IF (ISPMD.NE.0) THEN     
         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I).EQ.1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(SIZ) = V(I) 
           END IF
         END DO 

C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

         MSGTYP = MSGOFF2
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFSR,SIZ,MPI_REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE
          siz = 0
          DO I=1,NUMNOD
            IF (WEIGHT(I).EQ.1) THEN 
              siz = siz +1
              NG = NODGLOB(I)
              XGLOB(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2 

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
	    CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

C   Reception du buffer flottant des adresses NODGLOB

            MSGTYP = MSGOFF 
            CALL MPI_RECV(BUFSR,SIZ,MPI_REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
         
            NREC = SIZ
 
            DO K = 1, NREC
              NG = IBUF(K)
              XGLOB(NG) = BUFSR(K)
            ENDDO
          ENDDO
          CALL WRITE_R_C(XGLOB,NUMNODG)

       ENDIF
       DEALLOCATE(BUFSR,IBUF,XGLOB)

#endif
       RETURN
       END
