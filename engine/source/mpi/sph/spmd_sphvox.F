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
      !||====================================================================
      !||    spmd_sphvox           ../engine/source/mpi/sph/spmd_sphvox.F
      !||--- called by ------------------------------------------------------
      !||    sphtri0               ../engine/source/elements/sph/sphtri0.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                ../engine/source/output/message/message.F
      !||    arret                 ../engine/source/system/arret.F
      !||    spmd_iallgather       ../engine/source/mpi/generic/spmd_iallgather.F
      !||    spmd_iallgather_int   ../engine/source/mpi/generic/spmd_iallgather_int.F
      !||    spmd_ialltoall_int    ../engine/source/mpi/generic/spmd_ialltoall_int.F
      !||    spmd_ialltoallv       ../engine/source/mpi/generic/spmd_ialltoallv.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod           ../engine/share/message_module/message_mod.F
      !||    sphbox                ../engine/share/modules/sphbox.F
      !||    tri7box               ../engine/share/modules/tri7box.F
      !||====================================================================
      SUBROUTINE SPMD_SPHVOX(KXSP ,SPBUF,WSP2SORT,BMINMAL,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), WSP2SORT(*)
      my_real 
     .        X(3,*),BMINMAL(*), SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, KK, I, J, NOD, N, MSGTYP, LOC_PROC, NBIRECV,
     .        IERROR, IERROR1, L, LEN, IDEB, INDEXI, NB,
     .        NBX, NBY, NBZ,
     .        IX1, IX2, IY1, IY2, IZ1, IZ2, IX, IY, IZ,
     .        REQ_RB(NSPMD), REQ_SB(NSPMD), REQ_SD(NSPMD),
     .        REQ_RD(NSPMD), REQ_SD2(NSPMD), REQ_SC(NSPMD),
     .        REQ_RC(NSPMD),
     .        IRINDEXI(NSPMD), ISINDEXI(NSPMD), NBO(NSPMD), 
     .        INDEX(NSP2SORT), STATUS(MPI_STATUS_SIZE),
     .        MSGOFF,MSGOFF2,MSGOFF3,MSGOFF4
      my_real 
     .        BMINMA(6,NSPMD),ALPHA_MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        AAA
      TYPE(real_pointer), DIMENSION(NSPMD) :: BUF
      my_real, dimension(:), allocatable :: sbuf,rbuf
      DATA MSGOFF/2023/
      DATA MSGOFF2/2024/
      DATA MSGOFF3/2025/
      DATA MSGOFF4/2026/

      INTEGER :: P_LOC
      INTEGER :: SEND_SIZE_BMINMA
      INTEGER :: REQUEST_BMINMA
      INTEGER :: RCV_SIZE_BMINMA,TOTAL_RCV_SIZE_BMINMA

      INTEGER :: SEND_SIZE_CRVOX
      INTEGER :: REQUEST_CRVOX
      INTEGER :: RCV_SIZE_CRVOX,TOTAL_RCV_SIZE_CRVOX
      INTEGER, DIMENSION(0:LRVOXEL,0:LRVOXEL) :: CRVOXEL_LOC

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_P
      INTEGER, DIMENSION(NSPMD) :: NB_P
      INTEGER :: REQUEST_NBO

      INTEGER, DIMENSION(NSPMD) :: SEND_SIZE_SBUF,DISPLS_SBUF
      INTEGER :: TOTAL_SEND_SIZE,TOTAL_RCV_SIZE_RBUF      
      INTEGER, DIMENSION(NSPMD) :: RCV_SIZE_RBUF,DISPLS_RBUF
      INTEGER :: REQUEST_SBUF


C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
      ALLOCATE( INDEX_P(NSP2SORT,NSPMD) )
      ALPHA_MARGE = SQRT(ONE +SPASORT)
      LOC_PROC = ISPMD + 1
      PSPHS = 0
      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

C
C
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
C   envoi voxel + boite min/max
C
      SEND_SIZE_BMINMA = 6
      RCV_SIZE_BMINMA = 6
      TOTAL_RCV_SIZE_BMINMA = 6*NSPMD
!   -------------------------
!   allgather communication with uniform size
!   for real array : send : BMINMAL --> rcv : BMINMAL
      CALL SPMD_IALLGATHER(BMINMAL,BMINMA,SEND_SIZE_BMINMA,
     .                     TOTAL_RCV_SIZE_BMINMA,RCV_SIZE_BMINMA, 
     .                     REQUEST_BMINMA,MPI_COMM_WORLD)
!   -------------------------


      SEND_SIZE_CRVOX = (LRVOXEL+1)*(LRVOXEL+1)
      RCV_SIZE_CRVOX = (LRVOXEL+1)*(LRVOXEL+1)
      TOTAL_RCV_SIZE_CRVOX = (LRVOXEL+1)*(LRVOXEL+1)*NSPMD
      CRVOXEL_LOC(0:LRVOXEL,0:LRVOXEL) = CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)

!   -------------------------
!   allgather communication with uniform size
!   for integer array : send : CRVOXEL_LOC --> rcv : CRVOXEL
      CALL SPMD_IALLGATHER_INT(CRVOXEL_LOC(0,0),CRVOXEL(0,0,1),SEND_SIZE_CRVOX,
     .                         TOTAL_RCV_SIZE_CRVOX,RCV_SIZE_CRVOX, 
     .                         REQUEST_CRVOX,MPI_COMM_WORLD)
!   -------------------------
C
C   envoi de XREM
C

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking allgather comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comms : BMINMAL --> BMINMAL
!                             CRVOXEL_LOC --> CRVOXEL
      CALL MPI_WAIT(REQUEST_BMINMA,STATUS,IERROR)
      CALL MPI_WAIT(REQUEST_CRVOX,STATUS,IERROR)
!   -------------------------
#endif


!   -------------------------
!   fill the buffer NBO
      IDEB = 1
      NB_P(1:NSPMD) = 0
      NBO(1:NSPMD) = 0
      DO P = 1, NSPMD
        if(P==LOC_PROC) cycle
        L = IDEB
        NB_P(P) = 0
        XMAXB = BMINMA(1,P)
        YMAXB = BMINMA(2,P)
        ZMAXB = BMINMA(3,P)
        XMINB = BMINMA(4,P)
        YMINB = BMINMA(5,P)
        ZMINB = BMINMA(6,P)

        DO I=1, NSP2SORT
            N=WSP2SORT(I)
            NOD=KXSP(3,N)
            AAA = SPBUF(1,N)* ALPHA_MARGE
            IX1=INT(NBX*(X(1,NOD)-XMINB-AAA)/(XMAXB-XMINB))
            IX2=INT(NBX*(X(1,NOD)-XMINB+AAA)/(XMAXB-XMINB))
            IF(IX1 > NBX) CYCLE
            IF(IX2 < 0)   CYCLE
            IY1=INT(NBY*(X(2,NOD)-YMINB-AAA)/(YMAXB-YMINB))
            IY2=INT(NBY*(X(2,NOD)-YMINB+AAA)/(YMAXB-YMINB))
            IF(IY1 > NBY) CYCLE
            IF(IY2 < 0)   CYCLE
            IZ1=INT(NBZ*(X(3,NOD)-ZMINB-AAA)/(ZMAXB-ZMINB))
            IZ2=INT(NBZ*(X(3,NOD)-ZMINB+AAA)/(ZMAXB-ZMINB))
            IF(IZ1 > NBZ) CYCLE
            IF(IZ2 < 0)   CYCLE

            IX1=MAX(0,MIN(IX1,NBX))
            IX2=MIN(NBX,MAX(IX2,0))
            IY1=MAX(0,MIN(IY1,NBY))
            IY2=MIN(NBY,MAX(IY2,0))
            IZ1=MAX(0,MIN(IZ1,NBZ))
            IZ2=MIN(NBZ,MAX(IZ2,0))


            DO IZ = IZ1,IZ2
             DO IY = IY1,IY2
              DO IX = IX1,IX2
                IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                  NB_P(P) = NB_P(P) + 1
                  INDEX_P(NB_P(P),P) = N
                  GOTO 100
                ENDIF
              ENDDO
             ENDDO
            ENDDO

 100        CONTINUE

        ENDDO
        NBO(P) = NB_P(P)
        PSPHS(P) = NB_P(P)
      ENDDO
!   -------------------------

      PSPHR(1:NSPMD) = 0    
!   -------------------------
!   alltoall communication with uniform size
!   for integer array : send : NBO --> rcv : PSPHR  
      CALL SPMD_IALLTOALL_INT(NBO,PSPHR,NSPMD,1, 
     .                        NSPMD,1,REQUEST_NBO,MPI_COMM_WORLD)
!   -------------------------

C
      L = 0
      DO P=1,NSPMD
        L = L + SIZSPT*NB_P(P)
      ENDDO
      ALLOCATE(SBUF(L))

      L = 0
      DO P = 1, NSPMD
        if(P==LOC_PROC) cycle
        IF (NB_P(P)>0) THEN
          DO J = 1, NB_P(P)
            N = INDEX_P(J,P)
            NOD = KXSP(3,N)
            SBUF(L+1) = N
            SBUF(L+2) = SPBUF(1,N)
            SBUF(L+3) = X(1,NOD)
            SBUF(L+4) = X(2,NOD)
            SBUF(L+5) = X(3,NOD)
            SBUF(L+6) = KXSP(8,N)
            L = L + SIZSPT
          END DO
        END IF  
      END DO

C
      ! Total number of particules to send
      NSPHS = 0 
      DO P = 1, NSPMD  
         IF(LOC_PROC /=P) THEN
         NSPHS = NSPHS + PSPHS(P) 
         ENDIF
      ENDDO


      ! Array of local number of particules to send (sorted by proc)
      IF(ALLOCATED(LSPHS))DEALLOCATE(LSPHS)
      ALLOCATE(LSPHS(NSPHS),STAT=IERROR)

      IF(ALLOCATED(DKS))DEALLOCATE(DKS)
      ALLOCATE(DKS(NSPHS),STAT=IERROR1)
      IERROR = IERROR1 + IERROR


      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      LSPHS = 0
      DKS = -ONE
      ! Fill LSPHS with local numbers 
      IDEB = 0 
      L = 0
      DO P = 1, NSPMD  
        IF(LOC_PROC /=P) THEN
#include "novectorize.inc"
           DO I = 1,PSPHS(P) 
             IDEB = IDEB + 1
             LSPHS(IDEB) = SBUF(L+1) !BUF(P)%P(L+1)      
             L = L + SIZSPT
           ENDDO
        ENDIF
      ENDDO

C
      NSPHR = 0
      L=0
#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoall comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm : NBO --> PSPHR
      CALL MPI_WAIT(REQUEST_NBO,STATUS,IERROR)
!   -------------------------
#endif

      DO P = 1, NSPMD
!        PSPHR(P) = 0
        IF(LOC_PROC/=P) THEN
          IF(PSPHR(P)>0) THEN
            L=L+1
            ISINDEXI(L)=P
            NSPHR = NSPHR + PSPHR(P)
          END IF
        END IF
      END DO
      NBIRECV=L
C
!   -------------------------
!   compute the displacement, number of element
!   and total number of element (send and rcv)
      SEND_SIZE_SBUF(1:NSPMD) = 0
      DISPLS_SBUF(1:NSPMD) = 0
      RCV_SIZE_RBUF(1:NSPMD) = 0
      DISPLS_RBUF(1:NSPMD) = 0

      DISPLS_SBUF(1) = 0
      SEND_SIZE_SBUF(1) = SIZSPT*NB_P(1)
      TOTAL_SEND_SIZE = SEND_SIZE_SBUF(1)
      DO P=2,NSPMD
          SEND_SIZE_SBUF(P) = SIZSPT*NB_P(P)
          DISPLS_SBUF(P) = DISPLS_SBUF(P-1) + SEND_SIZE_SBUF(P-1)
          TOTAL_SEND_SIZE = TOTAL_SEND_SIZE + SEND_SIZE_SBUF(P)
      ENDDO

      RCV_SIZE_RBUF(1) = PSPHR(1)*SIZSPT
      TOTAL_RCV_SIZE_RBUF = RCV_SIZE_RBUF(1)
      DISPLS_RBUF(1) = 0
      DO P=2,NSPMD
          RCV_SIZE_RBUF(P) = PSPHR(P)*SIZSPT
          DISPLS_RBUF(P) = DISPLS_RBUF(P-1) + RCV_SIZE_RBUF(P-1)
          TOTAL_RCV_SIZE_RBUF = TOTAL_RCV_SIZE_RBUF + RCV_SIZE_RBUF(P)
      ENDDO
!   -------------------------

      IERROR = 0
      IF(ALLOCATED(XSPHR))DEALLOCATE(XSPHR)
      ALLOCATE(XSPHR(SIZSPT,NSPHR),STAT=IERROR1)
      IERROR = IERROR1 + IERROR

      IF(ALLOCATED(DKR))DEALLOCATE(DKR)
      ALLOCATE(DKR(NSPHR),STAT=IERROR1)
      IERROR = IERROR1 + IERROR

      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      XSPHR = 0
      DKR = -ONE

!   -------------------------
!   alltoall communication with non-uniform size
!   for real array : send : SBUF --> rcv : XSPHR
      CALL SPMD_IALLTOALLV(SBUF,XSPHR,SEND_SIZE_SBUF,TOTAL_SEND_SIZE,DISPLS_SBUF,
     .                     TOTAL_RCV_SIZE_RBUF,RCV_SIZE_RBUF,DISPLS_RBUF,
     .                     REQUEST_SBUF,MPI_COMM_WORLD,NSPMD)
!   -------------------------

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoall comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm : SBUF --> XSPHR
      CALL MPI_WAIT(REQUEST_SBUF,STATUS,IERROR)
!   -------------------------
#endif

      DEALLOCATE( SBUF )
      DEALLOCATE( INDEX_P )

#endif
      RETURN
      END
