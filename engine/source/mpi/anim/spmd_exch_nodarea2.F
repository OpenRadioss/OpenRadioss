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
Chd|  SPMD_EXCH_NODAREA2            source/mpi/anim/spmd_exch_nodarea2.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_SOL_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_NODAREA2(
     .           NODAREA,IAD_ELEM,FR_ELEM,LENR,WEIGHT,JJ)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR,WEIGHT(*),JJ
      my_real 
     .        NODAREA(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),MSGOFF

      my_real,
     . DIMENSION(:),ALLOCATABLE :: SBUF,RBUF
      DATA MSGOFF/7035/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(SBUF(LENR))
      ALLOCATE(RBUF(LENR))
C-----------------------------------------------
      LOC_PROC = ISPMD+1
      L = 1
      IAD_RECV(1)=1

Cow Mise en place des buffers de reception
      DO I=1,NSPMD
         SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
         IF(SIZ.NE.0) THEN
           MSGTYP = MSGOFF
           CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
         END IF
         IAD_RECV(I+1) = L
      END DO

! Remplissage des buffers d'envoi et envoi des donnees
C
      L=1
      IAD_SEND(1)=1
      DO I=1,NSPMD
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(L) = NODAREA(JJ,NOD)
            L = L+1
         END DO
        IAD_SEND(I+1) = L
      END DO

      DO I=1,NSPMD

       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN

          
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)

          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF                
      END DO
! Reception et traitement
      DO I = 1, NSPMD
C
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1

             NOD = FR_ELEM(J)
             NODAREA(JJ,NOD) = NODAREA(JJ,NOD) + RBUF(L)
             L = L+1
          ENDDO
        ENDIF
      ENDDO

      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C-----------------------------------------------
      DEALLOCATE(SBUF)
      DEALLOCATE(RBUF)
C-----------------------------------------------
#endif
      RETURN
      END
