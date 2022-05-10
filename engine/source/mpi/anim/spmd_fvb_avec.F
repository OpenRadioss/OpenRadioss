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
Chd|  SPMD_FVB_AVEC                 source/mpi/anim/spmd_fvb_avec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_AVEC()
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, PMASTER, NNS_ANIM, NNTR, J, K, KK, L, LL, N1, N2, N3,
     .        ITAG, LEN, MSGOFF, STAT(MPI_STATUS_SIZE), IERR,MSGOFF2
      my_real
     .        VVT(3)
      REAL R4
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPTR, NPN
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VTR, VV
C
      DATA MSGOFF/7048/
      DATA MSGOFF2/7049/
C
      DO I=1,NFVBAG
         PMASTER=FVSPMD(I)%PMASTER
         IF (ISPMD.EQ.0) THEN
            IF (ISPMD.EQ.PMASTER-1) THEN
               NNS_ANIM=FVDATA(I)%NNS_ANIM
               NNTR=FVDATA(I)%NNTR
               ALLOCATE(VTR(3,NNTR), VV(3,NNS_ANIM), NPTR(NNTR),
     .                  NPN(NNS_ANIM))
C
               DO J=1,NNTR
                  NPTR(J)=0
                  VTR(1,J)=ZERO
                  VTR(2,J)=ZERO
                  VTR(3,J)=ZERO
               ENDDO
               DO J=1,NNS_ANIM
                  NPN(J)=0
                  VV(1,J)=ZERO
                  VV(2,J)=ZERO
                  VV(3,J)=ZERO
               ENDDO
               DO J=1,FVDATA(I)%NPOLH
                  IF (FVDATA(I)%MPOLH(J).EQ.ZERO) CYCLE
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO L=FVDATA(I)%IFVTADR(KK),
     .                    FVDATA(I)%IFVTADR(KK+1)-1
                        LL=FVDATA(I)%IFVPOLY(L)
                        NPTR(LL)=NPTR(LL)+1
                        VTR(1,LL)=VTR(1,LL)+FVDATA(I)%QPOLH(1,J)/
     .                                      FVDATA(I)%MPOLH(J)    
                        VTR(2,LL)=VTR(2,LL)+FVDATA(I)%QPOLH(2,J)/
     .                                      FVDATA(I)%MPOLH(J)    
                        VTR(3,LL)=VTR(3,LL)+FVDATA(I)%QPOLH(3,J)/
     .                                      FVDATA(I)%MPOLH(J)
                     ENDDO
                  ENDDO
               ENDDO
               DO J=1,NNTR
                  N1=FVDATA(I)%IFVTRI_ANIM(1,J)  
                  N2=FVDATA(I)%IFVTRI_ANIM(2,J)  
                  N3=FVDATA(I)%IFVTRI_ANIM(3,J)
                  NPN(N1)=NPN(N1)+1
                  NPN(N2)=NPN(N2)+1
                  NPN(N3)=NPN(N3)+1
                  IF (NPTR(J).NE.0) THEN
                     VVT(1)=VTR(1,J)/NPTR(J)
                     VVT(2)=VTR(2,J)/NPTR(J)
                     VVT(3)=VTR(3,J)/NPTR(J)
                  ELSE
                     VVT(1)=ZERO
                     VVT(2)=ZERO
                     VVT(3)=ZERO
                  ENDIF
                  VV(1,N1)=VV(1,N1)+VVT(1)
                  VV(2,N1)=VV(2,N1)+VVT(2)  
                  VV(3,N1)=VV(3,N1)+VVT(3)  
                  VV(1,N2)=VV(1,N2)+VVT(1)  
                  VV(2,N2)=VV(2,N2)+VVT(2)  
                  VV(3,N2)=VV(3,N2)+VVT(3)  
                  VV(1,N3)=VV(1,N3)+VVT(1)  
                  VV(2,N3)=VV(2,N3)+VVT(2)  
                  VV(3,N3)=VV(3,N3)+VVT(3)
               ENDDO
C
               DO J=1,NNS_ANIM
                  R4 = VV(1,J)/NPN(J)
                  CALL WRITE_R_C(R4,1)
                  R4 = VV(2,J)/NPN(J)
                  CALL WRITE_R_C(R4,1)
                  R4 = VV(3,J)/NPN(J)
                  CALL WRITE_R_C(R4,1)
               ENDDO
C
               DEALLOCATE(VTR, VV, NPTR, NPN)
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNS_ANIM, 1, MPI_INTEGER, IT_SPMD(PMASTER),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               ALLOCATE(VV(3,NNS_ANIM))
               ITAG=MSGOFF2
               LEN=3*NNS_ANIM
               CALL MPI_RECV(VV, LEN, REAL, IT_SPMD(PMASTER),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               DO J=1,NNS_ANIM
                  R4 = VV(1,J)
                  CALL WRITE_R_C(R4,1)
                  R4 = VV(2,J)
                  CALL WRITE_R_C(R4,1)
                  R4 = VV(3,J)
                  CALL WRITE_R_C(R4,1)
               ENDDO
C
               DEALLOCATE(VV)
            ENDIF
         ELSE
            IF (ISPMD.EQ.PMASTER-1) THEN
               NNS_ANIM=FVDATA(I)%NNS_ANIM
               ITAG=MSGOFF
               CALL MPI_SEND(NNS_ANIM, 1, MPI_INTEGER, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD, IERR)
C
               NNTR=FVDATA(I)%NNTR
               ALLOCATE(VTR(3,NNTR), VV(3,NNS_ANIM), NPTR(NNTR),
     .                  NPN(NNS_ANIM))
C
               DO J=1,NNTR
                  NPTR(J)=0
                  VTR(1,J)=ZERO
                  VTR(2,J)=ZERO
                  VTR(3,J)=ZERO
               ENDDO
               DO J=1,NNS_ANIM
                  NPN(J)=0
                  VV(1,J)=ZERO
                  VV(2,J)=ZERO
                  VV(3,J)=ZERO
               ENDDO
               DO J=1,FVDATA(I)%NPOLH
                  IF (FVDATA(I)%MPOLH(J).EQ.ZERO) CYCLE
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO L=FVDATA(I)%IFVTADR(KK),
     .                    FVDATA(I)%IFVTADR(KK+1)-1
                        LL=FVDATA(I)%IFVPOLY(L)
                        NPTR(LL)=NPTR(LL)+1
                        VTR(1,LL)=VTR(1,LL)+FVDATA(I)%QPOLH(1,J)/
     .                                      FVDATA(I)%MPOLH(J)    
                        VTR(2,LL)=VTR(2,LL)+FVDATA(I)%QPOLH(2,J)/
     .                                      FVDATA(I)%MPOLH(J)    
                        VTR(3,LL)=VTR(3,LL)+FVDATA(I)%QPOLH(3,J)/
     .                                      FVDATA(I)%MPOLH(J)
                     ENDDO
                  ENDDO
               ENDDO
               DO J=1,NNTR
                  N1=FVDATA(I)%IFVTRI_ANIM(1,J)  
                  N2=FVDATA(I)%IFVTRI_ANIM(2,J)  
                  N3=FVDATA(I)%IFVTRI_ANIM(3,J)
                  NPN(N1)=NPN(N1)+1
                  NPN(N2)=NPN(N2)+1
                  NPN(N3)=NPN(N3)+1
                  IF (NPTR(J).NE.0) THEN
                     VVT(1)=VTR(1,J)/NPTR(J)
                     VVT(2)=VTR(2,J)/NPTR(J)
                     VVT(3)=VTR(3,J)/NPTR(J)
                  ELSE
                     VVT(1)=ZERO
                     VVT(2)=ZERO
                     VVT(3)=ZERO
                  ENDIF
                  VV(1,N1)=VV(1,N1)+VVT(1)
                  VV(2,N1)=VV(2,N1)+VVT(2)  
                  VV(3,N1)=VV(3,N1)+VVT(3)  
                  VV(1,N2)=VV(1,N2)+VVT(1)  
                  VV(2,N2)=VV(2,N2)+VVT(2)  
                  VV(3,N2)=VV(3,N2)+VVT(3)  
                  VV(1,N3)=VV(1,N3)+VVT(1)  
                  VV(2,N3)=VV(2,N3)+VVT(2)  
                  VV(3,N3)=VV(3,N3)+VVT(3)
               ENDDO
C
               DO J=1,NNS_ANIM
                  VV(1,J)=VV(1,J)/NPN(J)
                  VV(2,J)=VV(2,J)/NPN(J)
                  VV(3,J)=VV(3,J)/NPN(J)
               ENDDO
               ITAG=MSGOFF2
               LEN=3*NNS_ANIM
               CALL MPI_SEND(VV, LEN, REAL, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD,  IERR)
C
               DEALLOCATE(VTR, VV, NPTR, NPN)
            ENDIF
         ENDIF
      ENDDO  
C
      IF (ISPMD.EQ.0) THEN
         R4=ZERO
         DO I=1,3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C
#endif
      RETURN
      END
