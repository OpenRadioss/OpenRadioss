Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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

C            dxyzsph                  /anim/dxyzsph.F
!||====================================================================
!||    spmd_gather_sph       ../engine/source/mpi/anim/spmd_gather_sph.F
!||--- called by ------------------------------------------------------
!||    dxyzsph               ../engine/source/output/anim/generate/dxyzsph.F
!||--- calls      -----------------------------------------------------
!||    write_r_c             ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_GATHER_SPH(V,TV,NUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       real
     .   V(*)
       INTEGER NUM,TV
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/7034/

C   Tableau utilise par proc 0
       REAL, DIMENSION(:),ALLOCATABLE :: XGLOB
C-----------------------------------------------
       ALLOCATE(XGLOB(NUM))
         
       IF (ISPMD/=0) THEN
         MSGTYP = MSGOFF
         CALL MPI_SEND(V,3*TV,MPI_REAL,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

       ELSE

          CALL WRITE_R_C(V,3*TV)

          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(XGLOB,SIZ,MPI_REAL,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)

C  
           
            CALL WRITE_R_C(XGLOB,SIZ)
          ENDDO

       ENDIF

       DEALLOCATE(XGLOB)

#endif
       RETURN
       END
