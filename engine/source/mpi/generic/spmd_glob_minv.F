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
C==========================================================================
C==========================================================================
!||====================================================================
!||    spmd_glob_minv        ../engine/source/mpi/generic/spmd_glob_minv.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    monvol_struct_mod     ../engine/share/modules/monvol_struct_mod.F
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_GLOB_MINV(T_MONVOL, DT2,ITYPTS,NELTS,VOLMON, FR_MV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
        USE MONVOL_STRUCT_MOD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
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
      TYPE(MONVOL_STRUCT_) :: T_MONVOL(NVOLU)
      INTEGER ITYPTS, NELTS, FR_MV(NSPMD+2,NVOLU)
      my_real
     .        DT2, VOLMON(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IERROR, LOC_PROC, 
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
     &                   SPMD_COMM_WORLD, IERROR)
     
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
        VOLMON(KK1+12)=RBUF(3+(I-1)*7+4) ! pressure
        VOLMON(KK1+16)=RBUF(3+(I-1)*7+5) !Volume ?
        VOLMON(KK1+18)=RBUF(3+(I-1)*7+6) ! area
        VOLMON(KK1+21)=RBUF(3+(I-1)*7+7)
        T_MONVOL(I)%pressure = VOLMON(KK1+12)
        T_MONVOL(I)%temperature = VOLMON(KK1+13)
        T_MONVOL(I)%area = VOLMON(KK1+18)
!       write(6,*) "Pressure ", I, " is ", T_MONVOL(I)%pressure
!       write(6,*) "Temperature ", I, " is ", T_MONVOL(I)%temperature
!       write(6,*) "Area ", I, " is ", T_MONVOL(I)%area
!       write(6,*) "Volume ", I, " is ", VOLMON(KK1+16),T_MONVOL(I)%volume
        K1  = K1  + NIMV
        KK1 = KK1 + NRVOLU
      END DO
C
#endif
      RETURN
      END
