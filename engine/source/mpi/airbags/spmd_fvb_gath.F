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
Chd|  SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag.F         
Chd|        FVBAG3                        source/airbag/fvbag.F         
Chd|        FVBRIC                        source/airbag/fvbric.F        
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVREZONE1                     source/airbag/fvrezone.F      
Chd|        LECFVBAG                      source/input/lecfvbag.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_GATH(IFV, X, XXX, XXXA, XXXSA,
     .                         IDO)
C Gather local X into XXX,XXXA,XXXSA on the PMAIN of the FVM
C depending on IDO

C IF IDO = 1 : gather  X(1:NN_L)  into X
C IF IDO = 2 : gather  X(1:NN_L)  into X
C                  and X(1:NNA_L) into XXXA 
C                  and X(1:NNSA_L) into XXXSA 
C IF IDO = 3 : gather  X(1:NN_L + NNI_L) into X 
C                  and X(1:NNA_L) into XXXA 
C                  and X(1:NNSA_L) into XXXSA 
C IF IDO = 4 : gather  X(1:NN_L + NNI_L)  into X

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
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFV, IDO
      my_real
     .        X(3,*), XXX(3,*), XXXA(3,*), XXXSA(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER II, I, ITAG, LEN, ITAB(3,NSPMD-1),REQ(2*(NSPMD-1)),
     .        STAT(MPI_STATUS_SIZE,2*(NSPMD-1)), IERR, LENI, LENR,
     .        IADI(NSPMD-1), IADR(NSPMD-1), IAD, I1, I2, IAD1, IAD2,
     .        J, J1, ITABL(3), PMAIN, MSGOFF, IDO1, IDO2
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RBUF
C
      DATA MSGOFF/204/
C
      IDO2 = 0
      IDO1 = IDO
      IF (IDO == 4) THEN
        IDO1 = 1
        IDO2 = 1 ! add NNI_L
      ELSEIF (IDO == 3) THEN
        IDO2 = 1 ! add NNI_L
      ENDIF 

      IF (FVSPMD(IFV)%RANK == 0) THEN
         DO I=1,FVSPMD(IFV)%NSPMD-1
           ITAB(1,I) = FVSPMD(IFV)%ITAB(1,I)
           IF( IDO2 == 1) ITAB(1,I)=ITAB(1,I)+FVSPMD(IFV)%ITAB(4,I)
           IF (IDO1 > 1) THEN
              ITAB(2,I)=FVSPMD(IFV)%ITAB(2,I)
              ITAB(3,I)=FVSPMD(IFV)%ITAB(3,I)
           ELSE
              ITAB(2,I)=0
              ITAB(3,I)=0
           ENDIF
         ENDDO
 
         LENI=0
         LENR=0
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            IADI(II)=LENI+1
            IADR(II)=LENR+1
            LENI=LENI+ITAB(1,II)+ITAB(2,II)+ITAB(3,II)
            LENR=LENR+3*(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
         ENDDO
         ALLOCATE(IBUF(LENI), RBUF(LENR))
         RBUF(1:LENR) = ZERO
         IBUF(1:LENI) = 0
C Reception des entiers
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF
            IAD=IADI(II)
            LEN=ITAB(1,II)+ITAB(2,II)+ITAB(3,II)
            CALL MPI_IRECV(IBUF(IAD), LEN, MPI_INTEGER, I,
     .                     ITAG, FVSPMD(IFV)%MPI_COMM, REQ(II), IERR)
         ENDDO
C Reception des reels
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF
            IAD=IADR(II)
            LEN=3*(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
            CALL MPI_IRECV(RBUF(IAD), LEN, REAL, I, ITAG,
     .      FVSPMD(IFV)%MPI_COMM, REQ(FVSPMD(IFV)%NSPMD-1+II), IERR)
         ENDDO
C Remplissage des tableaux de sortie XXX, XXXA, XXXSA
         LEN = FVSPMD(IFV)%NN_L
C ajout noeuds internes
         IF(IDO2 == 1)LEN = LEN + FVSPMD(IFV)%NNI_L
         DO I=1,LEN
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            XXX(1,I1)=X(1,I2)
            XXX(2,I1)=X(2,I2)
            XXX(3,I1)=X(3,I2)
         ENDDO
         IF (IDO1 > 1) THEN
            DO I=1,FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               IF (I2 .LE. NUMNOD) THEN
                  ! IF MESHGEMS IS USED, EXTRA NODES ARE NOT INCLUDED WITHIN THE NUMNOD NODES
                  XXXA(1,I1)=X(1,I2)
                  XXXA(2,I1)=X(2,I2)
                  XXXA(3,I1)=X(3,I2)
               ENDIF
            ENDDO
            DO I=1,FVSPMD(IFV)%NNSA_L
               I1=FVSPMD(IFV)%IBUFSA_L(1,I)
               I2=FVSPMD(IFV)%IBUFSA_L(2,I)
               IF (I2 .LE. NUMNOD) THEN
                  ! IF MESHGEMS IS USED, EXTRA NODES ARE NOT INCLUDED WITHIN THE NUMNOD NODES
                  XXXSA(1,I1)=X(1,I2)
                  XXXSA(2,I1)=X(2,I2)
                  XXXSA(3,I1)=X(3,I2)
               ENDIF
            ENDDO
         ENDIF

         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            CALL MPI_WAIT(REQ(I), STAT, IERR)
            CALL MPI_WAIT(REQ(FVSPMD(IFV)%NSPMD-1+I), STAT, IERR)
            II=II+1
            IAD1=IADI(II)
            IAD2=IADR(II)
            DO J=1,ITAB(1,II)
               J1=IBUF(IAD1-1+J)
               XXX(1,J1)=RBUF(IAD2-1+3*(J-1)+1)
               XXX(2,J1)=RBUF(IAD2-1+3*(J-1)+2)
               XXX(3,J1)=RBUF(IAD2-1+3*(J-1)+3)
            ENDDO
            IAD1=IAD1+ITAB(1,II)
            IAD2=IAD2+3*ITAB(1,II)
            DO J=1,ITAB(2,II)
               J1=IBUF(IAD1-1+J)
               XXXA(1,J1)=RBUF(IAD2-1+3*(J-1)+1)
               XXXA(2,J1)=RBUF(IAD2-1+3*(J-1)+2)
               XXXA(3,J1)=RBUF(IAD2-1+3*(J-1)+3)
            ENDDO
            IAD1=IAD1+ITAB(2,II)
            IAD2=IAD2+3*ITAB(2,II)
            DO J=1,ITAB(3,II)
               J1=IBUF(IAD1-1+J)
               XXXSA(1,J1)=RBUF(IAD2-1+3*(J-1)+1)
               XXXSA(2,J1)=RBUF(IAD2-1+3*(J-1)+2)
               XXXSA(3,J1)=RBUF(IAD2-1+3*(J-1)+3)
            ENDDO
         ENDDO
         DEALLOCATE(IBUF, RBUF)
      ELSE IF(FVSPMD(IFV)%RANK > 0) THEN
         ITABL(1)=FVSPMD(IFV)%NN_L
C ajout noeuds internes
         IF(IDO2== 1 ) ITABL(1)=ITABL(1)+FVSPMD(IFV)%NNI_L
         IF (IDO1 > 1) THEN
            ITABL(2)=FVSPMD(IFV)%NNA_L
            ITABL(3)=FVSPMD(IFV)%NNSA_L
         ELSE
            ITABL(2)=0
            ITABL(3)=0
         ENDIF
         PMAIN=FVSPMD(IFV)%PMAIN
C
         LEN=ITABL(1)+ITABL(2)+ITABL(3)
         ALLOCATE(IBUF(LEN), RBUF(3*LEN))
         IAD1=1
         IAD2=1
         LEN = FVSPMD(IFV)%NN_L
         IF(IDO2 == 1) LEN = LEN + FVSPMD(IFV)%NNI_L
         DO I=1,LEN
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            IBUF(IAD1-1+I)=I1
            RBUF(IAD2-1+3*(I-1)+1)=X(1,I2)
            RBUF(IAD2-1+3*(I-1)+2)=X(2,I2)
            RBUF(IAD2-1+3*(I-1)+3)=X(3,I2)
         ENDDO
         IF (IDO1 > 1) THEN
            IAD1=IAD1+LEN
            IAD2=IAD2+3*LEN
            DO I=1,FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               IBUF(IAD1-1+I)=I1
               IF (I2 .LE. NUMNOD) THEN
                  ! IF MESHGEMS IS USED, EXTRA NODES ARE NOT INCLUDED WITHIN THE NUMNOD NODES
                  RBUF(IAD2-1+3*(I-1)+1)=X(1,I2)
                  RBUF(IAD2-1+3*(I-1)+2)=X(2,I2)
                  RBUF(IAD2-1+3*(I-1)+3)=X(3,I2)
               ENDIF
            ENDDO
            IAD1=IAD1+FVSPMD(IFV)%NNA_L
            IAD2=IAD2+3*FVSPMD(IFV)%NNA_L
            DO I=1,FVSPMD(IFV)%NNSA_L
               I1=FVSPMD(IFV)%IBUFSA_L(1,I)
               I2=FVSPMD(IFV)%IBUFSA_L(2,I)
               IBUF(IAD1-1+I)=I1
               IF (I2 .LE. NUMNOD) THEN
                  ! IF MESHGEMS IS USED, EXTRA NODES ARE NOT INCLUDED WITHIN THE NUMNOD NODES
                  RBUF(IAD2-1+3*(I-1)+1)=X(1,I2)
                  RBUF(IAD2-1+3*(I-1)+2)=X(2,I2)
                  RBUF(IAD2-1+3*(I-1)+3)=X(3,I2)
               ENDIF
            ENDDO
         ENDIF
C
         ITAG=MSGOFF
         LEN=ITABL(1)+ITABL(2)+ITABL(3)
         CALL MPI_ISEND(IBUF, LEN, MPI_INTEGER, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, REQ(1), IERR)
C
         ITAG=MSGOFF
         LEN=3*(ITABL(1)+ITABL(2)+ITABL(3))
         CALL MPI_ISEND(RBUF, LEN, REAL, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, REQ(2), IERR)
C
         CALL MPI_WAITALL(2, REQ, STAT, IERR)
         DEALLOCATE(IBUF, RBUF)
      ENDIF
C

#endif
      RETURN
      END
