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
Chd|  SPMD_FVB_COMM_PATTERN         source/mpi/airbags/spmd_fvb_comm_pattern.F
Chd|-- called by -----------
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_COMM_PATTERN(IFV) 

! Prepare the communication pattern for the FVB 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFV                            
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERROR, LENG,ITABL(4) 
      INTEGER II, I, ITAG, LEN, ITAB(3,NSPMD-1),REQ(2*(NSPMD-1)),
     .        STAT(MPI_STATUS_SIZE,2*(NSPMD-1)), IERR, 
     .        MSGOFF
      DATA MSGOFF/238/

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LENG=1
      IF(FVSPMD(IFV)%RANK > -1) THEN
        CALL MPI_ALLREDUCE(FVSPMD(IFV)%NNA_L,FVSPMD(IFV)%NNA_L_GLOB,LENG,
     .          MPI_INTEGER,MPI_SUM, FVSPMD(IFV)%MPI_COMM, IERROR)
C
      ELSE
        FVSPMD(IFV)%NNA_L_GLOB = 0
      ENDIF

      IF (FVSPMD(IFV)%RANK == 0) THEN
C Reception des longueurs
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF+I
            LEN=4
            CALL MPI_IRECV(FVSPMD(IFV)%ITAB(1,II), LEN, MPI_INTEGER, I,
     .                     ITAG, FVSPMD(IFV)%MPI_COMM, REQ(II), IERR)
         ENDDO
         CALL MPI_WAITALL(FVSPMD(IFV)%NSPMD-1, REQ, STAT, IERR)

      ELSE IF(FVSPMD(IFV)%RANK > 0) THEN
         ITABL(1)=FVSPMD(IFV)%NN_L
C ajout noeuds internes
         ITABL(2)=FVSPMD(IFV)%NNA_L
         ITABL(3)=FVSPMD(IFV)%NNSA_L
         ITABL(4)=FVSPMD(IFV)%NNI_L
         ITAG=MSGOFF+FVSPMD(IFV)%RANK
         LEN=4
         CALL MPI_SEND(ITABL, LEN, MPI_INTEGER, 0,
     .         ITAG, FVSPMD(IFV)%MPI_COMM, IERR)
C
      ENDIF
#endif
      RETURN
      END
