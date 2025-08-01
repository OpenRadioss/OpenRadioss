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
!||====================================================================
!||    spmd_fvb_aoff         ../engine/source/mpi/anim/spmd_fvb_aoff.F
!||--- called by ------------------------------------------------------
!||    genani                ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_c_c             ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    fvbag_mod             ../engine/share/modules/fvbag_mod.F
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_FVB_AOFF(FVEL2FA)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FVEL2FA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ELOFF, I, PMAIN, J, K, KK, N, NN, N1, N2, N3, NND,
     .        ITAG, NNTR, IERR, STAT(MPI_STATUS_SIZE), JJ, IAD,
     .        LEN, MSGOFF,MSGOFF2
      INTEGER, DIMENSION(:), ALLOCATABLE :: OFFTR, IBUF, ITAGT
C-----------------------------------------------
      DATA MSGOFF/7041/
      DATA MSGOFF2/7042/
C-----------------------------------------------
C
      ELOFF=0
      DO I=1,NFVBAG
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD==0) THEN
            IF (ISPMD==PMAIN-1) THEN
               ALLOCATE(OFFTR(FVDATA(I)%NNTR))
               DO J=1,FVDATA(I)%NNTR
                  OFFTR(J)=0
               ENDDO
               DO J=1,FVDATA(I)%NPOLH
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        IF (NN>0) THEN
                           N1=FVDATA(I)%IFVTRI_ANIM(1,NN)
                           N2=FVDATA(I)%IFVTRI_ANIM(2,NN)
                           N3=FVDATA(I)%IFVTRI_ANIM(3,NN)
                           NND=1
                           IF (N2/=N1) NND=NND+1
                           IF (N3/=N2.AND.N3/=N1) NND=NND+1
C
                           NN=FVEL2FA(ELOFF+NN)-ELOFF
                           IF (NND==3) OFFTR(NN)=1
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
               CALL WRITE_C_C(OFFTR,FVDATA(I)%NNTR)
               DEALLOCATE(OFFTR)
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNTR, 1, MPI_INTEGER, IT_SPMD(PMAIN),
     .                       ITAG, SPMD_COMM_WORLD, STAT, IERR)
C
               LEN=2*NNTR
               ALLOCATE(IBUF(LEN), OFFTR(NNTR))
               ITAG=MSGOFF2
               CALL MPI_RECV(IBUF, LEN, MPI_INTEGER, IT_SPMD(PMAIN),
     .                       ITAG, SPMD_COMM_WORLD, STAT, IERR)
C
               DO J=1,NNTR
                  OFFTR(J)=0
               ENDDO
               JJ=0
               DO J=1,NNTR
                  NN=IBUF(JJ+1)
                  IF (NN /=0 ) THEN
                    NN=FVEL2FA(ELOFF+NN)-ELOFF
                    OFFTR(NN)=IBUF(JJ+2)
                  ENDIF
                  JJ=JJ+2
               ENDDO
               ELOFF=ELOFF+NNTR
               CALL WRITE_C_C(OFFTR,NNTR)
               DEALLOCATE(IBUF, OFFTR)
            ENDIF
         ELSE
            IF (ISPMD==PMAIN-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NNTR, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, SPMD_COMM_WORLD,
     .                       IERR)
C
               LEN=2*FVDATA(I)%NNTR
               ALLOCATE(IBUF(LEN), ITAGT(FVDATA(I)%NNTR))
               DO J=1,LEN
                  IBUF(J)=0
               ENDDO
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
               IAD=0
               DO J=1,FVDATA(I)%NPOLH
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        IF (NN>0.AND.ITAGT(NN)==0) THEN
                           N1=FVDATA(I)%IFVTRI_ANIM(1,NN)
                           N2=FVDATA(I)%IFVTRI_ANIM(2,NN)
                           N3=FVDATA(I)%IFVTRI_ANIM(3,NN)
                           NND=1
                           IF (N2/=N1) NND=NND+1
                           IF (N3/=N2.AND.N3/=N1) NND=NND+1
C
                           IBUF(IAD+1)=NN
                           IF (NND==3) IBUF(IAD+2)=1
                           IAD=IAD+2
                           ITAGT(NN)=1
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
               ITAG=MSGOFF2
               CALL MPI_SEND(IBUF, LEN, MPI_INTEGER, IT_SPMD(1),
     .                       ITAG, SPMD_COMM_WORLD,  IERR)
C
               DEALLOCATE(IBUF, ITAGT)              
            ENDIF
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
