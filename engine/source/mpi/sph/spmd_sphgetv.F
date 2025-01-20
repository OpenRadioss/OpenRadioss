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
      !||    spmd_sphgetv          ../engine/source/mpi/sph/spmd_sphgetv.F
      !||--- called by ------------------------------------------------------
      !||    sphprep               ../engine/source/elements/sph/sphprep.F
      !||--- calls      -----------------------------------------------------
      !||    spmd_ialltoallv       ../engine/source/mpi/generic/spmd_ialltoallv.F
      !||    spmd_ialltoallv_int   ../engine/source/mpi/generic/spmd_ialltoallv_int.F
      !||--- uses       -----------------------------------------------------
      !||    sphbox                ../engine/share/modules/sphbox.F
      !||====================================================================
      SUBROUTINE SPMD_SPHGETV(KXSP ,SPBUF, V, MS, ISORTSP, IPARTSP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
ctmp+1
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), ISORTSP, IPARTSP(*)
       my_real
     .        SPBUF(NSPBUF,*), V(3,*), MS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), REQ_SD2(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),MSGOFF,MSGOFF2
       my_real, DIMENSION(:,:), ALLOCATABLE :: BUFS, BUFR

      INTEGER, DIMENSION(NSPMD) :: DISPLS_ISPHR,DISPLS_ISPHS
      INTEGER, DIMENSION(NSPMD) :: SEND_SIZE_ISPHR,RCV_SIZE_ISPHS
      INTEGER :: TOTAL_SEND_SIZE_ISPHR,TOTAL_RCV_SIZE_ISPHS
      INTEGER :: REQUEST_ISPHR

      INTEGER, DIMENSION(NSPMD) :: DISPLS_BUFS,DISPLS_BUFR
      INTEGER, DIMENSION(NSPMD) :: SEND_SIZE_BUFS,RCV_SIZE_BUFR
      INTEGER :: TOTAL_SEND_SIZE_BUFS,TOTAL_RCV_SIZE_BUFR
      INTEGER :: REQUEST_BUFS
       DATA MSGOFF/2008/
       DATA MSGOFF2/2009/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi flag cellules actives
C      
!   -------------------------
!   compute the displacement, number of element
!   and total number of element (send and rcv)
      DISPLS_ISPHR(1:NSPMD) = 0
      DISPLS_ISPHS(1:NSPMD) = 0
      SEND_SIZE_ISPHR(1:NSPMD) = 0
      RCV_SIZE_ISPHS(1:NSPMD) = 0
      TOTAL_SEND_SIZE_ISPHR = 0
      TOTAL_RCV_SIZE_ISPHS = 0


      DISPLS_ISPHR(1) = 0
      DISPLS_ISPHS(1) = 0
      DO P=1,NSPMD
          SEND_SIZE_ISPHR(P) = PSPHR(P)
          RCV_SIZE_ISPHS(P) = PSPHS(P)
          TOTAL_SEND_SIZE_ISPHR = TOTAL_SEND_SIZE_ISPHR + SEND_SIZE_ISPHR(P)
          TOTAL_RCV_SIZE_ISPHS = TOTAL_RCV_SIZE_ISPHS + RCV_SIZE_ISPHS(P)
      ENDDO
      DO P=2,NSPMD
          DISPLS_ISPHR(P) = DISPLS_ISPHR(P-1) + SEND_SIZE_ISPHR(P-1)
          DISPLS_ISPHS(P) = DISPLS_ISPHS(P-1) + RCV_SIZE_ISPHS(P-1)
      ENDDO
!   -------------------------

!   -------------------------
!   alltoall communication with non-uniform size
!   for integer array : send : ISPHR --> rcv : ISPHS
      CALL SPMD_IALLTOALLV_INT(ISPHR,ISPHS,
     .          SEND_SIZE_ISPHR,TOTAL_SEND_SIZE_ISPHR,DISPLS_ISPHR,
     .          TOTAL_RCV_SIZE_ISPHS,RCV_SIZE_ISPHS,DISPLS_ISPHS,
     .          REQUEST_ISPHR,SPMD_COMM_WORLD,NSPMD)
!   -------------------------

C Envoi V, M, RHO sur cellules actives
C

!   -------------------------
!   compute the displacement, number of element
!   and total number of element (send and rcv)
      DISPLS_BUFS(1:NSPMD) = 0
      DISPLS_BUFR(1:NSPMD) = 0
      SEND_SIZE_BUFS(1:NSPMD) = 0
      RCV_SIZE_BUFR(1:NSPMD) = 0
      TOTAL_SEND_SIZE_BUFS = 0
      TOTAL_RCV_SIZE_BUFR = 0


      DISPLS_BUFS(1) = 0
      DISPLS_BUFR(1) = 0

      DO P=1,NSPMD
          SEND_SIZE_BUFS(P) = PSPHS(P)
          RCV_SIZE_BUFR(P) = PSPHR(P)
          TOTAL_SEND_SIZE_BUFS = TOTAL_SEND_SIZE_BUFS + SEND_SIZE_BUFS(P)
          TOTAL_RCV_SIZE_BUFR = TOTAL_RCV_SIZE_BUFR + RCV_SIZE_BUFR(P)
      ENDDO

      DO P=2,NSPMD
          DISPLS_BUFS(P) = DISPLS_BUFS(P-1) + 7*SEND_SIZE_BUFS(P-1)
          DISPLS_BUFR(P) = DISPLS_BUFR(P-1) + 7*RCV_SIZE_BUFR(P-1)
      ENDDO

      ALLOCATE( BUFS(7,TOTAL_SEND_SIZE_BUFS) )
      ALLOCATE( BUFR(7,TOTAL_RCV_SIZE_BUFR) )

      TOTAL_SEND_SIZE_BUFS = 7*TOTAL_SEND_SIZE_BUFS
      TOTAL_RCV_SIZE_BUFR = 7*TOTAL_RCV_SIZE_BUFR
      SEND_SIZE_BUFS(1:NSPMD) = 7*SEND_SIZE_BUFS(1:NSPMD)
      RCV_SIZE_BUFR(1:NSPMD) = 7*RCV_SIZE_BUFR(1:NSPMD)
!   -------------------------

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoallv comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm : ISPHR --> ISPHS
      CALL MPI_WAIT(REQUEST_ISPHR,STATUS,IERROR)
!   -------------------------
#endif

!   -------------------------
!   fill the buffer
      IDEB = 0
      NN = 0
      DO P = 1, NSPMD
ctmp+1
       NN = 0
       IF(PSPHS(P)/=0)THEN
        DO N = 1, PSPHS(P)
          IF(ISORTSP == 1)THEN
            NN = NN + 1
            ICELL = LSPHS(IDEB+N)
            INOD = KXSP(3,ICELL)
            BUFS(1,IDEB+NN) = SPBUF(2,ICELL)
            BUFS(2,IDEB+NN) = SPBUF(12,ICELL)
            BUFS(3,IDEB+NN) = V(1,INOD)
            BUFS(4,IDEB+NN) = V(2,INOD)
            BUFS(5,IDEB+NN) = V(3,INOD)
            BUFS(6,IDEB+NN) = KXSP(2,ICELL)
            BUFS(7,IDEB+NN) = IPARTSP(ICELL)
          ELSEIF(ISPHS(IDEB+N)==1) THEN
            NN = NN + 1
            ICELL = LSPHS(IDEB+N)
            INOD = KXSP(3,ICELL)
            BUFS(1,IDEB+NN) = SPBUF(2,ICELL)
            BUFS(2,IDEB+NN) = SPBUF(12,ICELL)
            BUFS(3,IDEB+NN) = V(1,INOD)
            BUFS(4,IDEB+NN) = V(2,INOD)
            BUFS(5,IDEB+NN) = V(3,INOD)
            BUFS(6,IDEB+NN) = KXSP(2,ICELL)      
            BUFS(7,IDEB+NN) = IPARTSP(ICELL)      
          END IF
        END DO
        IDEB = IDEB + PSPHS(P)
ctmp+1
       END IF
      END DO
!   -------------------------

!   -------------------------
!   alltoall communication with non-uniform size
!   for real array : send : BUFS --> rcv : BUFR
      CALL SPMD_IALLTOALLV(BUFS,BUFR,
     .          SEND_SIZE_BUFS,TOTAL_SEND_SIZE_BUFS,DISPLS_BUFS,
     .          TOTAL_RCV_SIZE_BUFR,RCV_SIZE_BUFR,DISPLS_BUFR,
     .          REQUEST_BUFS,SPMD_COMM_WORLD,NSPMD)
!   -------------------------
C
C Reception V, M, RHO

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoallv comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm : BUFS --> BUFR
      CALL MPI_WAIT(REQUEST_BUFS,STATUS,IERROR)
!   -------------------------
#endif

C
      IDEB = 0
      NN = 0
      DO P = 1, NSPMD

        IF(PSPHR(P)/=0)THEN
         NN = 0
          DO N = 1, PSPHR(P)
            IF(ISORTSP == 1)THEN
              NN = NN + 1
              XSPHR(7,IDEB+N) = BUFR(1,IDEB+NN)
              XSPHR(8,IDEB+N) = BUFR(2,IDEB+NN)
              XSPHR(9,IDEB+N) = BUFR(3,IDEB+NN)
              XSPHR(10,IDEB+N)= BUFR(4,IDEB+NN)
              XSPHR(11,IDEB+N)= BUFR(5,IDEB+NN) 
                    XSPHR(13,IDEB+N)= BUFR(6,IDEB+NN)
                    XSPHR(14,IDEB+N)= BUFR(7,IDEB+NN)
            ELSEIF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              XSPHR(7,IDEB+N) = BUFR(1,IDEB+NN)
              XSPHR(8,IDEB+N) = BUFR(2,IDEB+NN)
              XSPHR(9,IDEB+N) = BUFR(3,IDEB+NN)
              XSPHR(10,IDEB+N)= BUFR(4,IDEB+NN)
              XSPHR(11,IDEB+N)= BUFR(5,IDEB+NN)
                    XSPHR(13,IDEB+N)= BUFR(6,IDEB+NN)      
                    XSPHR(14,IDEB+N)= BUFR(7,IDEB+NN)   
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO 

      DEALLOCATE( BUFS )
      DEALLOCATE( BUFR )

#endif
      RETURN
      END
