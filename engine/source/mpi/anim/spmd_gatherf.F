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
!||    spmd_gatherf          ../engine/source/mpi/anim/spmd_gatherf.F
!||--- called by ------------------------------------------------------
!||    genani                ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_r_c             ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_GATHERF(V,WEIGHT,NODGLOB,NUM)
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
       REAL
     .   V(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/7014/
      DATA MSGOFF2/7015/
       REAL, DIMENSION(:) , ALLOCATABLE :: BUFSR
       INTEGER, DIMENSION(:) , ALLOCATABLE :: IBUF
C   Table used by Pro 0
       REAL, DIMENSION(:) , ALLOCATABLE :: XGLOB
C=======================================================================
       ALLOCATE(BUFSR(NUMNODM), IBUF(NUMNODM))
       ALLOCATE(XGLOB(NUM))
  
          XGLOB = 1000000
          bufsr = 100000      
  
       IF (ISPMD/=0) THEN     
         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(SIZ) = V(I) 
           END IF
         END DO 

C   Because of the simple precision version, we cannot put the integer
C   In the floating buffer because there are only 2 24 bits available ~ 16 million
C   maximum number of nodes

         MSGTYP = MSGOFF2
         CALL spmd_send(IBUF(1:SIZ),SIZ,IT_SPMD(1),MSGTYP)

         MSGTYP = MSGOFF 
         CALL spmd_send(BUFSR(1:SIZ),SIZ,IT_SPMD(1),MSGTYP)

       ELSE
          siz = 0
          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN 
              siz = siz +1
              NG = NODGLOB(I)
              XGLOB(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD

C   reception of the entire buffer of NODGLOB addresses
            MSGTYP = MSGOFF2 

            CALL spmd_probe(IT_SPMD(I),MSGTYP,STATUS)
              CALL spmd_get_count_int(STATUS,SIZ)

            CALL spmd_recv(IBUF(1:SIZ),SIZ,IT_SPMD(I),MSGTYP)

C   reception of the floating buffer of NODGLOB addresses

            MSGTYP = MSGOFF 
            CALL spmd_recv(BUFSR(1:SIZ),SIZ,IT_SPMD(I),MSGTYP)
         
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
