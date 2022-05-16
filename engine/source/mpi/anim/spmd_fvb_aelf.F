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
Chd|  SPMD_FVB_AELF                 source/mpi/anim/spmd_fvb_aelf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_AELF(FVMASS , FVPRES, FVQX  , FVQY  , FVQZ  ,
     .                         FVRHO  , FVENER, FVCSON, FVGAMA, FVVISU,
     .                         FVEL2FA)
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
      INTEGER FVEL2FA(*)
      my_real
     .        FVMASS(*), FVPRES(*), FVQX(*), FVQY(*), FVQZ(*), FVRHO(*),
     .        FVENER(*), FVCSON(*), FVGAMA(*), FVVISU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ELOFF, I, PMAIN, J, K, KK, N, NN, IDP, ITAG, MSGOFF,
     .        NNTR, LEN, IAD, STAT(MPI_STATUS_SIZE), IERR,MSGOFF2
      my_real
     .        GAMA, SSP, FAC
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RBUF
      DATA MSGOFF/7046/
      DATA MSGOFF2/7047/

C
      ELOFF=0
      DO I=1,NFVBAG
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD.EQ.0) THEN
            IF (ISPMD.EQ.PMAIN-1) THEN
               DO J=1,FVDATA(I)%NPOLH
                  GAMA=FVDATA(I)%GPOLH(J)
                  SSP=SQRT((GAMA-UN)*GAMA*FVDATA(I)%EPOLH(J)/
     .                                    FVDATA(I)%MPOLH(J))
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        FAC=UN
                        IF (FVDATA(I)%IFVTRI(4,NN).NE.0) THEN
                           IDP=FVDATA(I)%IDPOLH(J)
                           FVVISU(FVEL2FA(ELOFF+NN))=IDP-(IDP/8)*8+1
                        ELSE
                           FVVISU(FVEL2FA(ELOFF+NN))=-1
                           FAC=UNDEMI
                        ENDIF
                        NN=FVEL2FA(ELOFF+NN)
                        FVMASS(NN)=FVMASS(NN)+FAC*FVDATA(I)%MPOLH(J)
                        FVPRES(NN)=FVPRES(NN)+FAC*FVDATA(I)%PPOLH(J)
                        IF (FVDATA(I)%MPOLH(J).GT.ZERO) THEN
                           FVQX(NN)=FVQX(NN)+FAC*FVDATA(I)%QPOLH(1,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVQY(NN)=FVQY(NN)+FAC*FVDATA(I)%QPOLH(2,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVQZ(NN)=FVQZ(NN)+FAC*FVDATA(I)%QPOLH(3,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVENER(NN)=FVENER(NN)+FAC*FVDATA(I)%EPOLH(J)/
     .                                               FVDATA(I)%MPOLH(J)
                        ENDIF
                        FVRHO(NN)=FVRHO(NN)+FAC*FVDATA(I)%RPOLH(J)
                        FVCSON(NN)=FVCSON(NN)+FAC*SSP
                        FVGAMA(NN)=FVGAMA(NN)+FAC*GAMA
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNTR, 1, MPI_INTEGER, IT_SPMD(PMAIN),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               LEN=10*NNTR
               ALLOCATE(RBUF(LEN))
               ITAG=MSGOFF2
               CALL MPI_RECV(RBUF, LEN, REAL, IT_SPMD(PMAIN),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               DO J=1,NNTR
                  NN=FVEL2FA(ELOFF+J)
                  FVMASS(NN)=RBUF(J)
                  FVPRES(NN)=RBUF(NNTR+J)
                  FVQX(NN)=RBUF(2*NNTR+J)
                  FVQY(NN)=RBUF(3*NNTR+J)
                  FVQZ(NN)=RBUF(4*NNTR+J)
                  FVENER(NN)=RBUF(5*NNTR+J)
                  FVRHO(NN)=RBUF(6*NNTR+J)
                  FVCSON(NN)=RBUF(7*NNTR+J)
                  FVGAMA(NN)=RBUF(8*NNTR+J)
                  FVVISU(NN)=RBUF(9*NNTR+J)
               ENDDO
               ELOFF=ELOFF+NNTR
               DEALLOCATE(RBUF)
            ENDIF
         ELSE
            IF (ISPMD.EQ.PMAIN-1) THEN
               NNTR=FVDATA(I)%NNTR
               ITAG=MSGOFF
               CALL MPI_SEND(NNTR, 1, MPI_INTEGER, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD, IERR)
C
               LEN=10*NNTR
               ALLOCATE(RBUF(LEN))
               DO J=1,LEN
                  RBUF(J)=ZERO
               ENDDO
               DO J=1,FVDATA(I)%NPOLH
                  GAMA=FVDATA(I)%GPOLH(J)
                  SSP=SQRT((GAMA-UN)*GAMA*FVDATA(I)%EPOLH(J)/
     .                                    FVDATA(I)%MPOLH(J))
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        FAC=UN
                        IF (FVDATA(I)%IFVTRI(4,NN).NE.0) THEN
                           IDP=FVDATA(I)%IDPOLH(J)
                           RBUF(9*NNTR+NN)=IDP-(IDP/8)*8+1
                        ELSE
                           RBUF(9*NNTR+NN)=-1
                           FAC=UNDEMI
                        ENDIF
                        RBUF(NN)=RBUF(NN)+FAC*FVDATA(I)%MPOLH(J)
                        RBUF(NNTR+NN)=RBUF(NNTR+NN)
     .                                +FAC*FVDATA(I)%PPOLH(J)
                        IF (FVDATA(I)%MPOLH(J).GT.ZERO) THEN
                           RBUF(2*NNTR+NN)=RBUF(2*NNTR+NN)
     .                                    +FAC*FVDATA(I)%QPOLH(1,J)/
     .                                         FVDATA(I)%MPOLH(J)
                           RBUF(3*NNTR+NN)=RBUF(3*NNTR+NN)
     .                                    +FAC*FVDATA(I)%QPOLH(2,J)/
     .                                         FVDATA(I)%MPOLH(J)
                           RBUF(4*NNTR+NN)=RBUF(4*NNTR+NN)
     .                                    +FAC*FVDATA(I)%QPOLH(3,J)/
     .                                         FVDATA(I)%MPOLH(J)
                           RBUF(5*NNTR+NN)=RBUF(5*NNTR+NN)
     .                                    +FAC*FVDATA(I)%EPOLH(J)/
     .                                         FVDATA(I)%MPOLH(J)
                        ENDIF
                        RBUF(6*NNTR+NN)=RBUF(6*NNTR+NN)
     .                                 +FAC*FVDATA(I)%RPOLH(J)
                        RBUF(7*NNTR+NN)=RBUF(7*NNTR+NN)+FAC*SSP
                        RBUF(8*NNTR+NN)=RBUF(8*NNTR+NN)+FAC*GAMA
                     ENDDO
                  ENDDO
               ENDDO
               ITAG=MSGOFF2
               CALL MPI_SEND(RBUF, LEN, REAL, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD, IERR)
C
               DEALLOCATE(RBUF)        
            ENDIF
         ENDIF
      ENDDO      
C
#endif
      RETURN
      END
