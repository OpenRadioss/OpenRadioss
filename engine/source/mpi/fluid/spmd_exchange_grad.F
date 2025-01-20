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
      !||    spmd_exchange_grad               ../engine/source/mpi/fluid/spmd_exchange_grad.F
      !||--- called by ------------------------------------------------------
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_EXCHANGE_GRAD(DIM,DIM1,DIM2,
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include "macro.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: DIM,DIM1,DIM2,LENCOM
      INTEGER, INTENT(IN) :: NERCVOIS(SNERCVOIS), NESDVOIS(SNESDVOIS)
      INTEGER, INTENT(IN) :: LERCVOIS(SLERCVOIS), LESDVOIS(SLESDVOIS)
      my_real, DIMENSION(DIM1,DIM2), INTENT(INOUT) :: PHI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, NDIM, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real
     .     WA(DIM, LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G        SPMD_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            DO NDIM = 1, DIM
               WA(NDIM,IDEB2+N) = PHI(NN,NDIM) ! nn,dim
            ENDDO
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G      SPMD_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          DO NDIM = 1, DIM
             PHI(NN,NDIM) = WA(NDIM,IDEB+N)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C

#endif
      RETURN
      END SUBROUTINE SPMD_EXCHANGE_GRAD
