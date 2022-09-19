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
Chd|  SPMD_FVB_AMON                 source/mpi/anim/spmd_fvb_amon.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_AMON(MONVOL, VOLMON)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)
      my_real
     .        VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER K1, KK1, KIBALE, KRBALE, IFV, N, ITYP, PMAIN, NN, NTG,
     .        ITAG, MSGOFF, LEN, IERR, STAT(MPI_STATUS_SIZE), IAD,
     .        I,MSGTAG
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RBUF
      DATA MSGOFF/7036/
C
      K1=1
      KK1=1
      KIBALE=1+NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL
      KRBALE=1+NRVOLU*NVOLU+LRCBAG+LRBAGJET+LRBAGHOL
      IFV=0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==6.OR.ITYP==8) THEN
           IFV =MONVOL(K1-1+45)
           PMAIN=FVSPMD(IFV)%PMAIN
           IF (PMAIN/=1) THEN
C
            NN =MONVOL(K1-1+32)+MONVOL(K1-1+68)
            NTG=MONVOL(K1-1+33)+MONVOL(K1-1+69)
            MSGTAG=MSGOFF
            LEN=6*NN+2*NTG
            ALLOCATE(RBUF(LEN))
            IF (ISPMD==0) THEN
C Proc 0 recoit
               CALL MPI_RECV(RBUF, LEN, REAL, IT_SPMD(PMAIN),
     .                       MSGTAG, MPI_COMM_WORLD, STAT, IERR)
C
               IAD=KRBALE+MONVOL(K1-1+34)
               DO I=1,6*NN
                  VOLMON(IAD+I-1)=RBUF(I)
               ENDDO
               IAD=IAD+6*NN+NTG
               DO I=1,2*NTG
                  VOLMON(IAD+I-1)=RBUF(6*NN+I)
               ENDDO
            ELSEIF (ISPMD==PMAIN-1) THEN
C Proc main envoie
               IAD=KRBALE+MONVOL(K1-1+34)
               DO I=1,6*NN
                  RBUF(I)=VOLMON(IAD+I-1)
               ENDDO
               IAD=IAD+6*NN+NTG
               DO I=1,2*NTG
                  RBUF(6*NN+I)=VOLMON(IAD+I-1)
               ENDDO
C
               CALL MPI_SEND(RBUF, LEN, REAL, IT_SPMD(1),
     .                       MSGTAG, MPI_COMM_WORLD,  IERR)
            ENDIF
            DEALLOCATE(RBUF)
           ENDIF
         ENDIF
         K1=K1+NIMV
         KK1=KK1+NRVOLU
      ENDDO
C
#endif
      RETURN
      END
