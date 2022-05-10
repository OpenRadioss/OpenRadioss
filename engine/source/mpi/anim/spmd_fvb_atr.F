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
Chd|  SPMD_FVB_ATR                  source/mpi/anim/spmd_fvb_atr.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_ATR(NBID1, NBID2, NBID3, FVEL2FA, FVINUM,
     .                        FVOFF)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBID1, NBID2, NBID3, FVEL2FA(*), FVINUM(*), FVOFF(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ELOFF, I, PMASTER, II, J, K, KK, N, NN, INOD(4), NND,
     .        ITAG, MSGOFF, LEN, JJ, IAD, IADE, IADF,
     .        STAT(MPI_STATUS_SIZE), IERR, NNTR
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGT, IBUF
C-----------------------------------------------
      DATA MSGOFF/7040/
C-----------------------------------------------
C
      ELOFF=0
      DO I=1,NFVBAG
         PMASTER=FVSPMD(I)%PMASTER
         IF (ISPMD.EQ.0) THEN
            IF (ISPMD.EQ.PMASTER-1) THEN
               ALLOCATE(ITAGT(FVDATA(I)%NNTR))
               II=0
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
C
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                      FVDATA(I)%IFVPADR_ANIM(J+1)-1
                     KK=FVDATA(I)%IFVPOLH_ANIM(K)
                     DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                         FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY_ANIM(N)
                        IF (ITAGT(NN).EQ.1) CYCLE
                        INOD(1)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(1,NN)-1
                        INOD(2)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(2,NN)-1
                        INOD(3)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(3,NN)-1
                        INOD(4)=INOD(3)
                        II=II+1
C Nombre de noeuds distincts de la coque (apres fusion dans FVMESH)
                        NND=1
                        IF (INOD(2).NE.INOD(1)) NND=NND+1
                        IF (INOD(3).NE.INOD(1).AND.
     .                      INOD(3).NE.INOD(2)) NND=NND+1
                        IF (NND.NE.3) THEN
                           INOD(1)=NBID1-1
                           INOD(2)=NBID2-1
                           INOD(3)=NBID3-1
                           INOD(4)=INOD(3)
                        ENDIF
C 
                        CALL WRITE_I_C(INOD,4)
                        ITAGT(NN)=1
                        FVEL2FA(ELOFF+NN)=ELOFF+II
                        FVINUM(ELOFF+II)=ELOFF+NN
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
               DEALLOCATE(ITAGT)
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNTR, 1, MPI_INTEGER, IT_SPMD(PMASTER),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               LEN=6*NNTR
               ALLOCATE(IBUF(LEN))
               ITAG=MSGOFF+1
               CALL MPI_RECV(IBUF, LEN, MPI_INTEGER, IT_SPMD(PMASTER),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               JJ=0
               DO J=1,NNTR
                  INOD(1)=FVOFF(1,I)+IBUF(JJ+1)
                  INOD(2)=FVOFF(1,I)+IBUF(JJ+2)
                  INOD(3)=FVOFF(1,I)+IBUF(JJ+3)
                  INOD(4)=FVOFF(1,I)+IBUF(JJ+4)
C
C Nombre de noeuds distincts de la coque (apres fusion dans FVMESH)
                  NND=1
                  IF (INOD(2).NE.INOD(1)) NND=NND+1
                  IF (INOD(3).NE.INOD(1).AND.
     .                INOD(3).NE.INOD(2)) NND=NND+1
                  IF (NND.NE.3) THEN
                     INOD(1)=NBID1-1
                     INOD(2)=NBID2-1
                     INOD(3)=NBID3-1
                     INOD(4)=INOD(3)
                  ENDIF
C
                  CALL WRITE_I_C(INOD,4)
                  JJ=JJ+4
C
                  FVEL2FA(ELOFF+J)=ELOFF+IBUF(4*NNTR+J)
                  FVINUM(ELOFF+J)=ELOFF+IBUF(5*NNTR+J)
               ENDDO
               ELOFF=ELOFF+NNTR
               DEALLOCATE(IBUF)
            ENDIF
         ELSE
            IF (ISPMD.EQ.PMASTER-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NNTR, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, MPI_COMM_WORLD,
     .                       IERR)
C
               LEN=6*FVDATA(I)%NNTR
               ALLOCATE(IBUF(LEN), ITAGT(FVDATA(I)%NNTR))
               II=0
               IAD=0
               IADE=4*FVDATA(I)%NNTR
               IADF=IADE+FVDATA(I)%NNTR
C
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
C
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                      FVDATA(I)%IFVPADR_ANIM(J+1)-1
                     KK=FVDATA(I)%IFVPOLH_ANIM(K)
                     DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                         FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY_ANIM(N)
                        IF (ITAGT(NN).EQ.1) CYCLE
                        INOD(1)=FVDATA(I)%IFVTRI_ANIM(1,NN)-1
                        INOD(2)=FVDATA(I)%IFVTRI_ANIM(2,NN)-1
                        INOD(3)=FVDATA(I)%IFVTRI_ANIM(3,NN)-1
                        INOD(4)=INOD(3)
                        II=II+1
C 
                        IBUF(IAD+1)=INOD(1)
                        IBUF(IAD+2)=INOD(2)
                        IBUF(IAD+3)=INOD(3)
                        IBUF(IAD+4)=INOD(4)
                        IAD=IAD+4
                        ITAGT(NN)=1
                        IBUF(IADE+NN)=II
                        IBUF(IADF+II)=NN
                     ENDDO
                  ENDDO
               ENDDO
               ITAG=MSGOFF+1
               CALL MPI_SEND(IBUF, LEN, MPI_INTEGER, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD, IERR)
C
               DEALLOCATE(IBUF, ITAGT)
            ENDIF
         ENDIF
      ENDDO
C
#endif
      RETURN
      END      
