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
C==========================================================================
C==========================================================================
Chd|====================================================================
Chd|  SPMD_GLOB_MINV                source/mpi/generic/spmd_glob_minv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_MINV(DT2,ITYPTS,NELTS,VOLMON, FR_MV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER ITYPTS, NELTS, FR_MV(NSPMD+2,NVOLU)
      my_real
     .        DT2, VOLMON(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, N, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, MSTOP1, MSTOP2, K1, KK1,
     .        LEN, myop,
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        RBUF(3+NVOLU*7), RRBUF(3+NVOLU*7)
      INTEGER :: MY_TYPE
      INTEGER GLOB_MINV
      EXTERNAL GLOB_MINV
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
C
      RRBUF(1) = DT2
      RRBUF(2) = ITYPTS
      RRBUF(3) = NELTS
      DO I = 1, NVOLU*7
        RRBUF(3+I) = ZERO
      END DO
      K1 = 1
      KK1 = 0
      DO I=1,NVOLU
        IF(FR_MV(NSPMD+2,I)==ISPMD+1) THEN
          RRBUF(3+(I-1)*7+1)=VOLMON(KK1+1)
          RRBUF(3+(I-1)*7+2)=VOLMON(KK1+2)
          RRBUF(3+(I-1)*7+3)=VOLMON(KK1+5)
          RRBUF(3+(I-1)*7+4)=VOLMON(KK1+12)
          RRBUF(3+(I-1)*7+5)=VOLMON(KK1+16)
          RRBUF(3+(I-1)*7+6)=VOLMON(KK1+18)
          RRBUF(3+(I-1)*7+7)=VOLMON(KK1+21)
        END IF
        K1  = K1  + NIMV
        KK1 = KK1 + NRVOLU
      END DO

      LEN = 3+NVOLU*7

      CALL MPI_TYPE_CONTIGUOUS(LEN,REAL,MY_TYPE,IERROR)
      CALL MPI_TYPE_COMMIT(MY_TYPE,IERROR)
      CALL MPI_OP_CREATE(GLOB_MINV, .TRUE., MYOP, IERROR)
      
      CALL MPI_ALLREDUCE(RRBUF, RBUF, 1, MY_TYPE, MYOP,
     &                   MPI_COMM_WORLD, IERROR)
     
      CALL MPI_OP_FREE(MYOP, IERROR)
      CALL MPI_TYPE_FREE(MY_TYPE,IERROR)

      DT2    = RBUF(1)
      ITYPTS = RBUF(2)
      NELTS  = RBUF(3)
      K1 = 1
      KK1 = 0
      DO I=1,NVOLU
        VOLMON(KK1+1) =RBUF(3+(I-1)*7+1)
        VOLMON(KK1+2) =RBUF(3+(I-1)*7+2)
        VOLMON(KK1+5) =RBUF(3+(I-1)*7+3)
        VOLMON(KK1+12)=RBUF(3+(I-1)*7+4)
        VOLMON(KK1+16)=RBUF(3+(I-1)*7+5)
        VOLMON(KK1+18)=RBUF(3+(I-1)*7+6)
        VOLMON(KK1+21)=RBUF(3+(I-1)*7+7)
        K1  = K1  + NIMV
        KK1 = KK1 + NRVOLU
      END DO
C
#endif
      RETURN
      END
