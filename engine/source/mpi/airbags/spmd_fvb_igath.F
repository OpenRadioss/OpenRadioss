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
C
Chd|====================================================================
Chd|  SPMD_FVB_IGATH                source/mpi/airbags/spmd_fvb_igath.F
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag.F         
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_IGATH(IFV, ITABS, ITABG)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFV, ITABS(*), ITABG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER II, I, ITAG, MSGOFF, REQ(NSPMD-1), IERR,
     .        STAT(MPI_STATUS_SIZE, NSPMD-1), LEN, IADI(NSPMD-1),
     .        IAD, I1, I2, J1, J2, PMAIN, NNT, J, ITAB(NSPMD-1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF
      DATA MSGOFF/205/
C
      IF (FVSPMD(IFV)%RANK == 0) THEN
C Reception des longueurs
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD - 1
            II=II+1
            ITAG=MSGOFF+I
            CALL MPI_IRECV(ITAB(II), 1, MPI_INTEGER, I,
     .                     ITAG, FVSPMD(IFV)%MPI_COMM, REQ(II), IERR)
         ENDDO
C
         CALL MPI_WAITALL(FVSPMD(IFV)%NSPMD-1, REQ, STAT, IERR)
C
         LEN=0
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            IADI(II)=LEN+1
            LEN=LEN+2*ITAB(II)
         ENDDO
         ALLOCATE(IBUF(LEN))
C Reception des entiers
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF+NSPMD+I
            IAD=IADI(II)
            LEN=2*ITAB(II)
            CALL MPI_IRECV(IBUF(IAD), LEN, MPI_INTEGER, I,
     .                     ITAG, FVSPMD(IFV)%MPI_COMM, REQ(II), IERR)
         ENDDO
C Remplissage du tableau de sortie ITABG
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            ITABG(I1)=ITABS(I2)
         ENDDO
C
         CALL MPI_WAITALL(FVSPMD(IFV)%NSPMD-1, REQ, STAT, IERR)
C
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD - 1
            II=II+1
            IAD=IADI(II)
            DO J=1,ITAB(II)
               J1=IBUF(IAD-1+J)
               J2=IBUF(IAD-1+ITAB(II)+J)
               ITABG(J1)=J2
            ENDDO
         ENDDO
         DEALLOCATE(IBUF)
      ELSE IF(FVSPMD(IFV)%RANK > 0) THEN
         PMAIN=FVSPMD(IFV)%PMAIN
         NNT=FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
         ITAG=MSGOFF+FVSPMD(IFV)%RANK
         CALL MPI_ISEND(NNT, 1, MPI_INTEGER, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, REQ(1), IERR)
C
         CALL MPI_WAIT(REQ(1), STAT, IERR)
C
         LEN=2*NNT
         ALLOCATE(IBUF(LEN))
         DO I=1,NNT
            IBUF(I)=FVSPMD(IFV)%IBUF_L(1,I)
            II=FVSPMD(IFV)%IBUF_L(2,I)
            IBUF(NNT+I)=ITABS(II)
         ENDDO
C
         ITAG=MSGOFF+NSPMD+FVSPMD(IFV)%RANK
         CALL MPI_ISEND(IBUF, LEN, MPI_INTEGER, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, REQ(2), IERR)
C
         CALL MPI_WAIT(REQ(2), STAT, IERR)
         DEALLOCATE(IBUF)
      ENDIF
C
#endif
      RETURN
      END
