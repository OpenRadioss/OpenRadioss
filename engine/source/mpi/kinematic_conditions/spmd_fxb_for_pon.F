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
#ifdef MPI
Chd|====================================================================
Chd|  SPMD_FXB_FOR_PON              source/mpi/kinematic_conditions/spmd_fxb_for_pon.F
Chd|-- called by -----------
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FXB_FOR_PON(FXBIPM, MFEXTP, FSKYFXB, DSKY, IADN)
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
#include      "fxbcom.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), DSKY, IADN(*)
      my_real
     .        MFEXTP(*), FSKYFXB(DSKY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NFX, NMOD, NME, NSN, NVAR, PMAIN, IAD, NSNT, I, J, JJ,
     .        ITAG, MSGOFF, NSNP(NSPMD-1), NSNPMAX, LEN, K, KK, AVAR,
     .        IERR, REQ(NSPMD-1), STAT(MPI_STATUS_SIZE,NSPMD-1),
     .        II
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: FSKYL, FSKYP
C
      DATA MSGOFF /197/
C
      DO NFX=1,NFXBODY
         NMOD=FXBIPM(4,NFX)
         NME=FXBIPM(17,NFX)
         NSN=FXBIPM(18,NFX)
         AVAR=FXBIPM(13,NFX)
         PMAIN=FXBIPM(39,NFX)
         IAD=IADN(NFX)
         IF (ISPMD==PMAIN) THEN
            NSNT=FXBIPM(40,NFX)
            ALLOCATE(FSKYL(NSNT,NME+NMOD))
            DO I=1,NME+NMOD
               DO J=1,NSN
                  JJ=NINT(FSKYFXB(IAD+J,1))
                  FSKYL(JJ,I)=FSKYFXB(IAD+J,1+I)
               ENDDO
            ENDDO
C
            II=0
            DO I=1,NSPMD
               IF (ISPMD==I-1) CYCLE
               II=II+1
               ITAG=MSGOFF
               CALL MPI_IRECV(NSNP(II), 1, MPI_INTEGER, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
            ENDDO
            CALL MPI_WAITALL(NSPMD-1, REQ, STAT, IERR)
C
            NSNPMAX=0
            DO I=1,NSPMD-1
               NSNPMAX=MAX(NSNPMAX,NSNP(I))
            ENDDO
            ALLOCATE(FSKYP(NSNPMAX*(1+NME+NMOD),NSPMD-1))
            II=0
            DO I=1,NSPMD
               IF (ISPMD==I-1) CYCLE
               II=II+1
               ITAG=MSGOFF
               LEN=NSNP(II)*(1+NME+NMOD)
               CALL MPI_IRECV(FSKYP(1,II), LEN, REAL, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
            ENDDO
            CALL MPI_WAITALL(NSPMD-1, REQ, STAT, IERR)
C
            DO I=1,NSPMD-1
               DO J=1,NME+NMOD
                  JJ=J*NSNP(I)
                  DO K=1,NSNP(I)
                     KK=NINT(FSKYP(K,I))
                     FSKYL(KK,J)=FSKYP(JJ+K,I)
                  ENDDO
               ENDDO
            ENDDO
            DEALLOCATE(FSKYP)
C
            DO I=1,NME+NMOD
               MFEXTP(AVAR+I-1)=ZERO
               DO J=1,NSNT
                  MFEXTP(AVAR+I-1)=MFEXTP(AVAR+I-1)+FSKYL(J,I)
               ENDDO
            ENDDO
            DEALLOCATE(FSKYL)
         ELSE
            ITAG=MSGOFF
            CALL MPI_ISEND(NSN, 1, MPI_INTEGER, IT_SPMD(PMAIN+1),
     .                     ITAG, MPI_COMM_WORLD, REQ(1), IERR)
            CALL MPI_WAIT(REQ, STAT, IERR)
C
            ALLOCATE(FSKYL(NSN,1+NME+NMOD))
            DO I=1,1+NME+NMOD
               DO J=1,NSN
                  FSKYL(J,I)=FSKYFXB(IAD+J,I)
               ENDDO
            ENDDO
            LEN=NSN*(1+NME+NMOD)
            ITAG=MSGOFF
            CALL MPI_ISEND(FSKYL, LEN, REAL, IT_SPMD(PMAIN+1),
     .                     ITAG, MPI_COMM_WORLD, REQ(1), IERR)
            CALL MPI_WAIT(REQ, STAT, IERR)
C
            DEALLOCATE(FSKYL)
         ENDIF
      ENDDO
C
#endif
      RETURN
      END



#elif 1 


C version simplifiee pour mono non SPMD
Chd|====================================================================
Chd|  SPMD_FXB_FOR_PON              source/mpi/kinematic_conditions/spmd_fxb_for_pon.F
Chd|-- called by -----------
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FXB_FOR_PON(FXBIPM, MFEXTP, FSKYFXB, DSKY, IADN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), DSKY, IADN(*)
      my_real
     .        MFEXTP(*), FSKYFXB(DSKY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX, NMOD, NME, NSN, NVAR, IAD, I, J, JJ,
     .        ITAG, AVAR
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: FSKYL
C
      DO NFX=1,NFXBODY
         NMOD=FXBIPM(4,NFX)
         NME=FXBIPM(17,NFX)
         NSN=FXBIPM(18,NFX)
         AVAR=FXBIPM(13,NFX)
         IAD=IADN(NFX)
         ALLOCATE(FSKYL(NSN,NME+NMOD))
         DO I=1,NME+NMOD
            DO J=1,NSN
               JJ=NINT(FSKYFXB(IAD+J,1))
               FSKYL(JJ,I)=FSKYFXB(IAD+J,1+I)
            ENDDO
         ENDDO
C
         DO I=1,NME+NMOD
            MFEXTP(AVAR+I-1)=ZERO
            DO J=1,NSN
               MFEXTP(AVAR+I-1)=MFEXTP(AVAR+I-1)+FSKYL(J,I)
            ENDDO
         ENDDO
         DEALLOCATE(FSKYL)
      ENDDO
      RETURN
      END


C

#endif
