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
Chd|  SPMD_FVB_GATH_BEGIN           source/mpi/airbags/spmd_fvb.F 
Chd|-- called by -----------
Chd|        FVUPD0                        source/airbag/fvupd.F         
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_GATH_BEGIN(IFV, X, XXX, XXXA,
     .                                    V, VVV, VVVA)
C Gather local X into XXX,XXXA,on the PMAIN of the FVM
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
      INTEGER IFV
      my_real
     .        X(3,*), XXX(3,*), XXXA(3,*),
     .        V(3,*), VVV(3,*), VVVA(3,*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER II, I, ITAG, LEN, ITAB(3,NSPMD-1),
     .        STAT(MPI_STATUS_SIZE,2*(NSPMD-1)), IERR, LENI, LENR,
     .        IAD, I1, I2, IAD1, IAD2,
     .        J, J1, ITABL(3), PMAIN, MSGOFF

      DATA MSGOFF/205/
C
      ALLOCATE(FVSPMD(IFV)%REQ(2*(NSPMD-1)))
      ALLOCATE(FVSPMD(IFV)%IADI(NSPMD-1))
      ALLOCATE(FVSPMD(IFV)%IADR(NSPMD-1))



      IF (FVSPMD(IFV)%RANK == 0) THEN
         DO I=1,FVSPMD(IFV)%NSPMD-1
           ITAB(1,I) = FVSPMD(IFV)%ITAB(1,I)
           ITAB(1,I) = ITAB(1,I)+FVSPMD(IFV)%ITAB(4,I)
           ITAB(2,I) = FVSPMD(IFV)%ITAB(2,I)
           ITAB(3,I) = FVSPMD(IFV)%ITAB(3,I)
         ENDDO

         LENI=0
         LENR=0
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            FVSPMD(IFV)%IADI(II)=LENI+1
            FVSPMD(IFV)%IADR(II)=LENR+1
            LENI=LENI+(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
            LENR=LENR+6*(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
         ENDDO
         ALLOCATE(FVSPMD(IFV)%IBUF(LENI), FVSPMD(IFV)%RBUF(LENR))
C Reception des entiers
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF
            IAD=FVSPMD(IFV)%IADI(II)
            LEN=(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
            CALL MPI_IRECV(FVSPMD(IFV)%IBUF(IAD), LEN, MPI_INTEGER, I,
     .                     ITAG, FVSPMD(IFV)%MPI_COMM, FVSPMD(IFV)%REQ(II), IERR)
         ENDDO
C Reception des reels
         II=0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            II=II+1
            ITAG=MSGOFF
            IAD=FVSPMD(IFV)%IADR(II)
            LEN=6*(ITAB(1,II)+ITAB(2,II)+ITAB(3,II))
            CALL MPI_IRECV(FVSPMD(IFV)%RBUF(IAD), LEN, REAL, I, ITAG,
     .      FVSPMD(IFV)%MPI_COMM, FVSPMD(IFV)%REQ(FVSPMD(IFV)%NSPMD-1+II), IERR)
         ENDDO
C Remplissage des tableaux de sortie XXX, XXXA, 
         LEN = FVSPMD(IFV)%NN_L
C ajout noeuds internes
         LEN = LEN + FVSPMD(IFV)%NNI_L
         DO I=1,LEN
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            XXX(1,I1)=X(1,I2)
            XXX(2,I1)=X(2,I2)
            XXX(3,I1)=X(3,I2)
            VVV(1,I1)=V(1,I2)
            VVV(2,I1)=V(2,I2)
            VVV(3,I1)=V(3,I2)

         ENDDO
         DO I=1,FVSPMD(IFV)%NNA_L
             I1=FVSPMD(IFV)%IBUFA_L(1,I)
             I2=FVSPMD(IFV)%IBUFA_L(2,I)
             IF(I2<=NUMNOD) THEN
                XXXA(1,I1)=X(1,I2)
                XXXA(2,I1)=X(2,I2)
                XXXA(3,I1)=X(3,I2)
                VVVA(1,I1)=V(1,I2)
                VVVA(2,I1)=V(2,I2)
                VVVA(3,I1)=V(3,I2)
            ENDIF
         ENDDO
      ELSE IF(FVSPMD(IFV)%RANK > 0) THEN
         ITABL(1)=FVSPMD(IFV)%NN_L
C ajout noeuds internes
         ITABL(1)=ITABL(1)+FVSPMD(IFV)%NNI_L
         ITABL(2)=FVSPMD(IFV)%NNA_L
         ITABL(3)= 0
         PMAIN=FVSPMD(IFV)%PMAIN
C
         LEN=ITABL(1)+ITABL(2)+ITABL(3)
         ALLOCATE(FVSPMD(IFV)%IBUF(LEN), FVSPMD(IFV)%RBUF(6*LEN))
         IAD1=1
         IAD2=1
         LEN = FVSPMD(IFV)%NN_L
         LEN = LEN + FVSPMD(IFV)%NNI_L
         DO I=1,LEN
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            FVSPMD(IFV)%IBUF(IAD1-1+I)=I1
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+1)=X(1,I2)
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+2)=X(2,I2)
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+3)=X(3,I2)
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+4)=V(1,I2)
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+5)=V(2,I2)
            FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+6)=V(3,I2)
         ENDDO
         IAD1=IAD1+LEN
         IAD2=IAD2+6*LEN
         DO I=1,FVSPMD(IFV)%NNA_L
            I1=FVSPMD(IFV)%IBUFA_L(1,I)
            I2=FVSPMD(IFV)%IBUFA_L(2,I)
            IF (I2 <= NUMNOD) THEN
                  ! IF MESHGEMS IS USED, EXTRA NODES ARE NOT INCLUDED WITHIN THE NUMNOD NODES
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+1)=X(1,I2)
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+2)=X(2,I2)
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+3)=X(3,I2)
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+4)=V(1,I2)
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+5)=V(2,I2)
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+6)=V(3,I2)
              FVSPMD(IFV)%IBUF(IAD1-1+I)=I1
            ELSE
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+1)=ZERO
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+2)=ZERO
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+3)=ZERO
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+4)=ZERO
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+5)=ZERO
              FVSPMD(IFV)%RBUF(IAD2-1+6*(I-1)+6)=ZERO
              FVSPMD(IFV)%IBUF(IAD1-1+I)=-I1
            ENDIF

         ENDDO
         IAD1=IAD1+FVSPMD(IFV)%NNA_L
         IAD2=IAD2+3*FVSPMD(IFV)%NNA_L
C

         ITAG=MSGOFF
         LEN=ITABL(1)+ITABL(2)+ITABL(3)
         CALL MPI_ISEND(FVSPMD(IFV)%IBUF, LEN, MPI_INTEGER, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, FVSPMD(IFV)%REQ(1), IERR)
C
         ITAG=MSGOFF
         LEN=6*(ITABL(1)+ITABL(2)+ITABL(3))
         CALL MPI_ISEND(FVSPMD(IFV)%RBUF, LEN, REAL, 0,
     .                  ITAG, FVSPMD(IFV)%MPI_COMM, FVSPMD(IFV)%REQ(2), IERR)
C
      ENDIF
C

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_FVB_GATH_END             source/mpi/airbags/spmd_fvb.F 
Chd|-- called by -----------
Chd|        FVUPD1                        source/airbag/fvupd.F         
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_GATH_END(IFV, X, XXX, XXXA,
     .                                    V, VVV, VVVA)

C WAIT messages
C Pmain fills XXXA,VVVA
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
      INTEGER IFV
      my_real
     .        X(3,*), XXX(3,*), XXXA(3,*),
     .        V(3,*), VVV(3,*), VVVA(3,*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER II, I, ITAG, LEN, ITAB(3,NSPMD-1),
     .        STAT(MPI_STATUS_SIZE,2*(NSPMD-1)), IERR, LENI, LENR,
     .        IAD, I1, I2, IAD1, IAD2,
     .        J, J1, ITABL(3), PMAIN, MSGOFF
      DATA MSGOFF/205/
C


      IF (FVSPMD(IFV)%RANK == 0) THEN
         DO I=1,FVSPMD(IFV)%NSPMD-1
           ITAB(1,I) = FVSPMD(IFV)%ITAB(1,I)
           ITAB(1,I) = ITAB(1,I)+FVSPMD(IFV)%ITAB(4,I)
           ITAB(2,I) = FVSPMD(IFV)%ITAB(2,I)
           ITAB(3,I) = FVSPMD(IFV)%ITAB(3,I)
         ENDDO
         II = 0
         DO I=1,FVSPMD(IFV)%NSPMD-1
            CALL MPI_WAIT(FVSPMD(IFV)%REQ(I), STAT, IERR)
            CALL MPI_WAIT(FVSPMD(IFV)%REQ(FVSPMD(IFV)%NSPMD-1+I), STAT, IERR)
            II=II+1
            IAD1=FVSPMD(IFV)%IADI(II)
            IAD2=FVSPMD(IFV)%IADR(II)
            DO J=1,ITAB(1,II)
               J1=FVSPMD(IFV)%IBUF(IAD1-1+J)
               XXX(1,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+1)
               XXX(2,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+2)
               XXX(3,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+3)
               VVV(1,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+4)
               VVV(2,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+5)
               VVV(3,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+6)
            ENDDO
            IAD1=IAD1+ITAB(1,II)
            IAD2=IAD2+6*ITAB(1,II)
            DO J=1,ITAB(2,II)
               J1=FVSPMD(IFV)%IBUF(IAD1-1+J)
               IF(J1 > 0 ) THEN
                 XXXA(1,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+1)
                 XXXA(2,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+2)
                 XXXA(3,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+3)
                 VVVA(1,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+4)
                 VVVA(2,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+5)
                 VVVA(3,J1)=FVSPMD(IFV)%RBUF(IAD2-1+6*(J-1)+6)
               ENDIF
            ENDDO
            IAD1=IAD1+ITAB(2,II)
            IAD2=IAD2+6*ITAB(2,II)
         ENDDO
         DEALLOCATE(FVSPMD(IFV)%IBUF, FVSPMD(IFV)%RBUF)
      ELSE IF(FVSPMD(IFV)%RANK > 0) THEN
         CALL MPI_WAITALL(2, FVSPMD(IFV)%REQ, STAT, IERR)
         DEALLOCATE(FVSPMD(IFV)%IBUF, FVSPMD(IFV)%RBUF)
      ENDIF
C
      DEALLOCATE(FVSPMD(IFV)%REQ)
      DEALLOCATE(FVSPMD(IFV)%IADR)
      DEALLOCATE(FVSPMD(IFV)%IADI)

#endif
      RETURN
      END

