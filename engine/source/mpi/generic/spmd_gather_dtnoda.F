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
Chd|  SPMD_GATHER_DTNODA            source/mpi/generic/spmd_gather_dtnoda.F
Chd|-- called by -----------
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_GATHER_DTNODA(TAGN,STIFN,MS,WEIGHT,NUM,DT2_L,STF_L,MS_L)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   STIFN(*),MS(*),DT2_L(*),MS_L(*),STF_L(*)
       INTEGER WEIGHT(*),NUM,TAGN(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MS_LOFF,ID_G
      INTEGER SIZ,MS_LTYP,I,K,NG,NREC,MS_LOFF2

      DATA MS_LOFF/7014/
       my_real, DIMENSION(:) , ALLOCATABLE :: BUFSR
C
C=======================================================================
       ALLOCATE (BUFSR(3*NUM))     
C  
       IF (ISPMD/=0) THEN     
         SIZ = 0
         DO I=1,NUMNOD
           IF (TAGN(I) > 0) THEN
             SIZ = SIZ + 3
             BUFSR(SIZ-2) = MS(I)/STIFN(I)
             BUFSR(SIZ-1) = STIFN(I) 
             BUFSR(SIZ  ) = MS(I)  
           END IF
         END DO 

C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

         MS_LTYP = MS_LOFF 
         CALL MPI_SEND(BUFSR,SIZ,REAL,IT_SPMD(1),MS_LTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE
          ID_G = 0
          DO I=1,NUMNOD
           IF (TAGN(I) > 0) THEN
              ID_G = ID_G + 1 
              DT2_L(ID_G) = MS(I)/STIFN(I) 
              STF_L(ID_G) = STIFN(I) 
              MS_L(ID_G) = MS(I) 
            ENDIF
          ENDDO

          DO I=2,NSPMD
C-------------
            MS_LTYP = MS_LOFF 
            CALL MPI_PROBE(IT_SPMD(I),MS_LTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,REAL,SIZ,ierror) 

C------------ Reception du buffer flottant

            CALL MPI_RECV(BUFSR,SIZ,REAL,IT_SPMD(I),MS_LTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
     
            NREC = SIZ
            DO K = 1,NREC,3
              ID_G = ID_G + 1
              DT2_L(ID_G) = BUFSR(K)
              STF_L(ID_G) = BUFSR(K+1)
              MS_L(ID_G) = BUFSR(K+2) 
            ENDDO
C
          ENDDO

       ENDIF
       DEALLOCATE(BUFSR)

#endif
       RETURN
       END