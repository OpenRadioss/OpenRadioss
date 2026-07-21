Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_wrt_xyznor       ../engine/source/mpi/anim/spmd_wrt_xyznor.F
!||--- called by ------------------------------------------------------
!||    xyznor                ../engine/source/output/anim/generate/xyznor.F
!||--- calls      -----------------------------------------------------
!||    write_s_c             ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE SPMD_WRT_XYZNOR(XNORM,WEIGHT,NODGLOB,NUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_MOD
#include      "implicit_f.inc"
#include "spmd.inc"
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
       INTEGER WEIGHT(*),NODGLOB(*),NUM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBUF,RNGLOB
      DATA MSGOFF/7030/
      DATA MSGOFF2/7031/
       
C   Table used by Pro 0
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

C   Because of the simple precision version, we cannot put the integer
C   In the floating buffer because there are only 2 24 bits available ~ 16 million
C   maximum number of nodes

        MSGTYP = MSGOFF2 
        CALL SPMD_SEND(IBUF(1:4,1:SIZ),4*SIZ,IT_SPMD(1),MSGTYP)

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

            CALL SPMD_PROBE(IT_SPMD(I),MSGTYP,STATUS)
            CALL SPMD_GET_COUNT_INT(STATUS,SIZ)

            CALL SPMD_RECV(IBUF(1:4,1:SIZ/4),SIZ,IT_SPMD(I),MSGTYP)

         
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
