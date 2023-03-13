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
Chd|  SPMD_WRT_CRK_XYZNOR           source/mpi/anim/spmd_wrt_crk_xyznor.F
Chd|-- called by -----------
Chd|        XYZNOR_CRK                    source/output/anim/generate/xyznor_crk.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
       SUBROUTINE SPMD_WRT_CRK_XYZNOR(ICRK,XNORM,NUM)
C-----------------------------------------------
      USE CRACKXFEM_MOD        
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
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER ICRK,NUM
      my_real
     .   XNORM(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,EMPL
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2,SI,MSGTYP2,SIZ0
C
      INTEGER,DIMENSION(:,:),ALLOCATABLE ::  RNGLOB,IBUF
 
C   Tableau utilise par proc 0
      my_real
     .   S3000,S,XN1,XN2,XN3,IXNN
C-----------------------------------------------         
      DATA MSGOFF/7066/
      DATA MSGOFF2/7067/
C-----------------------------------------------  
      ALLOCATE(IBUF(4,NCRKNODG))
      ALLOCATE(RNGLOB(3,NUM))
C-----------------------------------------------  

      S3000 = THREE1000
      IXNN = S3000
      IF(FMT_ANI==4)IXNN=0
C---
       IF (ISPMD/=0) THEN

        SIZ = 0
        DO I=1,CRKNOD(ICRK)%CRKNUMNODS
           EMPL = CRKNOD(ICRK)%XFENODES(I)
C           S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
           S = ZERO
           IF(S /= ZERO)THEN
c             S = S3000 / S
c               XN1 = XNORM(1,I) * S
c               XN2 = XNORM(2,I) * S
c               XN3 = XNORM(3,I) * S
           ELSE
             XN1 = 0
             XN2 = 0
             XN3 = IXNN
           END IF
           SIZ = SIZ + 1
           IBUF(1,SIZ) = CRKNOD(ICRK)%XFECRKNODID(I)
           IBUF(2,SIZ) = XN1
           IBUF(3,SIZ) = XN2
           IBUF(4,SIZ) = XN3
        END DO
C
C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

         MSGTYP = MSGOFF2
         CALL MPI_SEND(IBUF,4*SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE

C proc 0
        SIZ0 = 0
        DO I=1,CRKNOD(ICRK)%CRKNUMNODS
           EMPL = CRKNOD(ICRK)%XFENODES(I)
c           NG = CRKNOD(ICRK)%XFECRKNODID(I)
C
           SIZ0 = SIZ0 + 1
c           S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
           S = ZERO
           IF(S/=ZERO)THEN
             S = S3000 / S
c             XN1 = XNORM(1,I) * S
c             XN2 = XNORM(2,I) * S
c             XN3 = XNORM(3,I) * S
           ELSE
             XN1 = 0
             XN2 = 0
             XN3 = IXNN
           END IF 
c           RNGLOB(1,NG) = XN1
c           RNGLOB(2,NG) = XN2
c           RNGLOB(3,NG) = XN3
           RNGLOB(1,SIZ0) = XN1
           RNGLOB(2,SIZ0) = XN2
           RNGLOB(3,SIZ0) = XN3
        END DO

          DO I=2,NSPMD

            MSGTYP = MSGOFF2

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
	    CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

            NREC = SIZ / 4


            DO K = 1, NREC
c              NG = IBUF(1,K)
c              RNGLOB(1,NG) = IBUF(2,K)
c              RNGLOB(2,NG) = IBUF(3,K)
c              RNGLOB(3,NG) = IBUF(4,K)
              SIZ0 = SIZ0 + 1
              RNGLOB(1,SIZ0) = IBUF(2,K)
              RNGLOB(2,SIZ0) = IBUF(3,K)
              RNGLOB(3,SIZ0) = IBUF(4,K)
            ENDDO
          ENDDO

C          DO I=1,NCRKNODG
          DO I=1,SIZ0
           CALL WRITE_S_C(RNGLOB(1,I),1)
           CALL WRITE_S_C(RNGLOB(2,I),1)
           CALL WRITE_S_C(RNGLOB(3,I),1)
         END DO
        END IF

        DEALLOCATE(IBUF)
        DEALLOCATE(RNGLOB)
C
#endif
       RETURN
       END
