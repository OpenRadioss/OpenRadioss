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
!||    spmd_wrt_xyznod       ../engine/source/mpi/anim/spmd_wrt_xyznod.F
!||--- called by ------------------------------------------------------
!||    xyznod                ../engine/source/output/anim/generate/xyznod.F
!||--- calls      -----------------------------------------------------
!||    write_r_c             ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||====================================================================
       SUBROUTINE SPMD_WRT_XYZNOD(X,WEIGHT,NODGLOB,NUM)
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
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   X(3,*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2,MSGTYP2

      REAL, DIMENSION(:,:), ALLOCATABLE :: BUFSR,XGLOB
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF

      DATA MSGOFF/7026/
      DATA MSGOFF2/7027/
       
C-----------------------------------------------

       ALLOCATE(BUFSR(3,NUMNODM))
       ALLOCATE(XGLOB(3,NUM))
       ALLOCATE(IBUF(NUMNODM))
        
       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(1,SIZ) = X(1,I)
             BUFSR(2,SIZ) = X(2,I) 
             BUFSR(3,SIZ) = X(3,I) 
           END IF
         END DO

C   Because of the simple precision version, we cannot put the integer
C   In the floating buffer because there are only 2 24 bits available ~ 16 million
C   maximum number of nodes

         MSGTYP = MSGOFF2 
         CALL SPMD_SEND(IBUF(1:SIZ),SIZ,IT_SPMD(1),MSGTYP)

C  
         MSGTYP = MSGOFF
         CALL SPMD_SEND(BUFSR(1:3,1:SIZ),3*SIZ,IT_SPMD(1),MSGTYP)


       ELSE

          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN
              NG = NODGLOB(I)
              XGLOB(1,NG) = X(1,I)
              XGLOB(2,NG) = X(2,I)
              XGLOB(3,NG) = X(3,I)
            ENDIF
          ENDDO


          DO I=2,NSPMD

C   reception of the entire buffer of NODGLOB addresses
            MSGTYP = MSGOFF2

            CALL SPMD_PROBE(IT_SPMD(I),MSGTYP,STATUS)
              CALL SPMD_GET_COUNT_INT(STATUS,SIZ)

            CALL SPMD_RECV(IBUF(1:SIZ),SIZ,IT_SPMD(I),MSGTYP)

C   reception of the floating buffer of NODGLOB addresses

            MSGTYP2 = MSGOFF 

            CALL SPMD_RECV(BUFSR(1:3,1:SIZ),SIZ*3,IT_SPMD(I),MSGTYP2)
         
            NREC = SIZ 


            DO K = 1, NREC
              NG = IBUF(K)
              XGLOB(1,NG) = BUFSR(1,K)
              XGLOB(2,NG) = BUFSR(2,K)
              XGLOB(3,NG) = BUFSR(3,K)
            ENDDO
          ENDDO
C  
C   
          DO I=1,NUMNODG
           CALL WRITE_R_C(XGLOB(1,I),1)
           CALL WRITE_R_C(XGLOB(2,I),1)
           CALL WRITE_R_C(XGLOB(3,I),1)
         END DO
        END IF


       DEALLOCATE(BUFSR)
       DEALLOCATE(XGLOB)
       DEALLOCATE(IBUF)

#endif
       RETURN
       END
