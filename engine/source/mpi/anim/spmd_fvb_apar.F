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
!||    spmd_fvb_apar         ../engine/source/mpi/anim/spmd_fvb_apar.F
!||--- called by ------------------------------------------------------
!||    genani                ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    fvbag_mod             ../engine/share/modules/fvbag_mod.F
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_FVB_APAR(NELCUT, NBF   , NESCT, NERWL, NESRG,
     .                         NESMD1, FVPBUF)
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
      INTEGER NELCUT, NBF, NESCT, NERWL, NESRG, NESMD1, FVPBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER FVIAD, I, PMAIN, J, K, KK, N, NN, ITAG, MSGOFF,MSGOFF2,
     .        NPOLH_ANIM, IERR, STAT(MPI_STATUS_SIZE), IAD, IADP
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGT, IBUF
C-----------------------------------------------
      DATA MSGOFF/7043/
      DATA MSGOFF2/7044/
C
      IADP=0
      FVIAD=NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
      DO I=1,NFVBAG
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD==0) THEN
            IF (ISPMD==PMAIN-1) THEN
               ALLOCATE(ITAGT(FVDATA(I)%NNTR))
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
C
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                        FVDATA(I)%IFVPADR_ANIM(J+1)-1
                     KK=FVDATA(I)%IFVPOLH_ANIM(K)
                     DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                          FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY_ANIM(N)
                        IF (ITAGT(NN)==0) THEN
                           FVIAD=FVIAD+1
                           ITAGT(NN)=1
                        ENDIF
                     ENDDO
                  ENDDO
                  FVPBUF(IADP+J)=FVIAD
               ENDDO
               IADP=IADP+FVDATA(I)%NPOLH_ANIM
C
               DEALLOCATE(ITAGT)
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NPOLH_ANIM, 1, MPI_INTEGER, 
     .                       IT_SPMD(PMAIN), ITAG, SPMD_COMM_WORLD,
     .                       STAT, IERR)
C
               ALLOCATE(IBUF(NPOLH_ANIM))
               ITAG=MSGOFF2
               CALL MPI_RECV(IBUF, NPOLH_ANIM, MPI_INTEGER,
     .                       IT_SPMD(PMAIN), ITAG, SPMD_COMM_WORLD,
     .                       STAT, IERR)
C
               DO J=1,NPOLH_ANIM
                  FVPBUF(IADP+J)=FVIAD+IBUF(J)
               ENDDO
               FVIAD=FVIAD+IBUF(NPOLH_ANIM)
               IADP=IADP+NPOLH_ANIM
               DEALLOCATE(IBUF)
            ENDIF
         ELSE
            IF (ISPMD==PMAIN-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NPOLH_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, SPMD_COMM_WORLD,
     .                       IERR)
C
               ALLOCATE(IBUF(FVDATA(I)%NPOLH_ANIM), 
     .                       ITAGT(FVDATA(I)%NNTR))
               IAD=0
C
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
C
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                        FVDATA(I)%IFVPADR_ANIM(J+1)-1
                     KK=FVDATA(I)%IFVPOLH_ANIM(K)
                     DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                          FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY_ANIM(N)
                        IF (ITAGT(NN)==0) THEN
                           IAD=IAD+1
                           ITAGT(NN)=1
                        ENDIF
                     ENDDO
                  ENDDO
                  IBUF(J)=IAD
               ENDDO
               ITAG=MSGOFF2
               CALL MPI_SEND(IBUF, FVDATA(I)%NPOLH_ANIM, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, SPMD_COMM_WORLD,
     .                        IERR)
C
               DEALLOCATE(IBUF, ITAGT)                              
            ENDIF
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
