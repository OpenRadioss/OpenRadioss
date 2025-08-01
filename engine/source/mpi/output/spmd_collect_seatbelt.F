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
C
!||====================================================================
!||    spmd_collect_seatbelt   ../engine/source/mpi/output/spmd_collect_seatbelt.F
!||--- called by ------------------------------------------------------
!||    hist2                   ../engine/source/output/th/hist2.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    seatbelt_mod            ../common_source/modules/seatbelt_mod.F
!||    spmd_comm_world_mod     ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_COLLECT_SEATBELT()
C-----------------------------------------------
      USE SEATBELT_MOD
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
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,IERROR,LOC_PROC,NN,L,I,K,M,N,II,J,
     .        IDEB,SIZ,A_AR,NBIRECV,INDEX,
     .        IRINDEX(NSPMD),REQ_R(NSPMD),IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        SBUF(7*NSLIPRING+4*NRETRACTOR),RBUF(7*NSLIPRING_G+4*NRETRACTOR_G),FAC
      DATA MSGOFF/203/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IF (LOC_PROC==1) THEN
        IDEB = 1
C
        DO II = 1, NSEATBELT_TH_PROC
          I = SEATBELT_TH_EXCH(II)%ID_PROC
          IAD_RECV(I) = IDEB
          IRINDEX(II) = I
          SIZ = 7*SEATBELT_TH_EXCH(II)%NSLIPRING + 4*SEATBELT_TH_EXCH(II)%NRETRACTOR
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S    RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    SPMD_COMM_WORLD,REQ_R(II),IERROR)
          IDEB = IDEB + SIZ
        END DO
C
        DO II = 1, NSEATBELT_TH_PROC
          CALL MPI_WAITANY(NSEATBELT_TH_PROC,REQ_R,INDEX,STATUS,IERROR)
          I = IRINDEX(INDEX)
          L = IAD_RECV(I)
C
          DO N = 1,SEATBELT_TH_EXCH(INDEX)%NSLIPRING
            K = NINT(RBUF(L))
            DO M=1,6
              TH_SLIPRING(K,M) = RBUF(L+M)
            ENDDO
            L = L + 7
          END DO
C
          DO N = 1,SEATBELT_TH_EXCH(INDEX)%NRETRACTOR
            K = NINT(RBUF(L))
            DO M=1,3
              TH_RETRACTOR(K,M) = RBUF(L+M)
            ENDDO
            L = L + 4
          END DO
        END DO
C
      ELSE
C
        K = 0
        SBUF = 0
C
        DO N = 1, NSLIPRING
          SBUF(K+1) = SLIPRING(N)%IDG
          K = K + 1
          FAC = ONE/SLIPRING(N)%NFRAM
          DO L = 1,SLIPRING(N)%NFRAM
C--         IF NFRAM > 1 - RINGLSIP and BETA are average of the 1d sliprings - FORCE is the sum - GAMMA = ZERO
            SBUF(K+1) = SBUF(K+1) + FAC*SLIPRING(N)%FRAM(L)%RINGSLIP
            SBUF(K+2) = SBUF(K+2) + SLIPRING(N)%FRAM(L)%SLIP_FORCE(3)
            SBUF(K+3) = SBUF(K+3) + SLIPRING(N)%FRAM(L)%SLIP_FORCE(1)
            SBUF(K+4) = SBUF(K+4) + SLIPRING(N)%FRAM(L)%SLIP_FORCE(2)
            SBUF(K+5) = SBUF(K+5) + FAC*SLIPRING(N)%FRAM(L)%BETA
            SBUF(K+6) = SBUF(K+6) + FAC*SLIPRING(N)%FRAM(L)%ORIENTATION_ANGLE
          END DO
          K = K + 6
        END DO
C
        DO N = 1, NRETRACTOR
          SBUF(K+1) = RETRACTOR(N)%IDG
          SBUF(K+2) = RETRACTOR(N)%RINGSLIP
          SBUF(K+3) = RETRACTOR(N)%RET_FORCE
          SBUF(K+4) = RETRACTOR(N)%LOCKED
          K = K + 4
        END DO
C
        SIZ = K
        IF (SIZ > 0) THEN
          MSGTYP=MSGOFF
          CALL MPI_SEND(SBUF,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G                  SPMD_COMM_WORLD,IERROR)
        ENDIF
C
      END IF
C
#endif
      RETURN
      END
