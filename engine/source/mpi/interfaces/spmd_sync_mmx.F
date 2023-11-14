Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPMD_SYNC_MMX                 source/mpi/interfaces/spmd_sync_mmx.F
Chd|-- called by -----------
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SYNC_MMX(
     1    ISENDTO,IRCVFROM,NEWFRONT,XSLV_L,XMSR_L,
     2    VSLV_L ,VMSR_L  ,INTLIST ,NINTC ,TZINF ,
     3    SIZE_T ,IPARI   ,DELTA_PMAX_GAP, MAXDGAP)
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
#include      "param_c.inc"
#include      "task_c.inc"
C commun local a spmd_init et spmd_ring_mmx
       COMMON /RING/IRECVF,ISENDT,IRING
       INTEGER IRECVF,ISENDT,IRING
C irecvf : proc precedent dans l'anneau si <> 0
C isendt : proc suivant dans l'anneau si <> 0 et <> -1
C isendt = -1 indique un broadcast a faire (dernier proc de l'anneau)
C iring : no du proc effectuant le broadcast si besoin sur le proc
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*),
     .        NEWFRONT(*), NINTC, INTLIST(*), IPARI(NPARI,*)
      my_real
     .        XSLV_L(18,*), XMSR_L(12,*), VSLV_L(6,*),
     .        VMSR_L(6,*), TZINF(*), SIZE_T(*),DELTA_PMAX_GAP(*),MAXDGAP(NINTER)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,P,SIZ,NIN,I,J,L,LOC_PROC,
     .        IERROR,I_LEN,NBIRECV,KK,
     .        IADS,INDEXI,MSGTYP2,
     .        IRINDEXI(NSPMD),IADR(NSPMD),REQ_R(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      PARAMETER (I_LEN = 46)
      my_real
     .        SBUF(NSPMD*NINTC*I_LEN)
      DATA MSGOFF/116/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C
      IF(IRING==0) RETURN
C
      LOC_PROC = ISPMD+1
C
C Compactage min/max, newfront, tzinf
C
      IF(LOC_PROC/=IRING) THEN
C si proc participe aux interfaces
        IF(IRCVFROM(NINTER+1,LOC_PROC)>0.OR.
     +     ISENDTO(NINTER+1,LOC_PROC)>0) THEN
          L = 0
          DO KK=1,NINTC
            NIN = INTLIST(KK)
            IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.
     +         ISENDTO(NIN,LOC_PROC)/=0)THEN
C X MIN/MAX
              DO J=1,18
                SBUF(L+J) = XSLV_L(J,NIN)
              END DO
              L = L + 18
C
              DO J=1,12
                SBUF(L+J) = XMSR_L(J,NIN)
              END DO
              L = L + 12
C V MIN/MAX
              DO J=1,6
                SBUF(L+J) = VSLV_L(J,NIN)
              END DO
              L = L + 6
C
              DO J=1,6
                SBUF(L+J) = VMSR_L(J,NIN)
              END DO
              L = L + 6
              IF(IPARI(7,NIN)/=17)THEN
C NEWFRONT
                SBUF(L+1) = NEWFRONT(NIN)
              ELSE     ! interface 17 avec courbure
C SIZE
                SBUF(L+1) = SIZE_T(NIN)
              END IF
              L = L + 1
C TZINF
              SBUF(L+1) = TZINF(KK)
              L = L + 1
C DELTA_PMAX_GAP
              SBUF(L+1) = DELTA_PMAX_GAP(NIN)
              L = L + 1
C T25 main gap changes with thickness change
              SBUF(L+1) = MAXDGAP(NIN)
              L = L + 1
            END IF
          END DO
          MSGTYP =  MSGOFF
          CALL MPI_SEND(
     S      SBUF,L,REAL,IT_SPMD(IRING),MSGTYP,
     G      MPI_COMM_WORLD,IERROR)
          MSGTYP =  MSGOFF
          L = I_LEN*NINTC
          CALL MPI_RECV(
     S      SBUF,L,REAL,IT_SPMD(IRING),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)
          L = 0
          DO KK=1,NINTC
            NIN = INTLIST(KK)
            IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.
     +         ISENDTO(NIN,LOC_PROC)/=0)THEN
             DO J=1,18
              XSLV_L(J,NIN) = SBUF(L+J)
             END DO
             L = L + 18
             DO J=1,12
              XMSR_L(J,NIN) = SBUF(L+J)
             END DO
             L = L + 12
             DO J=1,6
              VSLV_L(J,NIN) = SBUF(L+J)
             END DO
             L = L + 6
             DO J=1,6
              VMSR_L(J,NIN) = SBUF(L+J)
             END DO
             L = L + 6
             IF(IPARI(7,NIN)/=17)THEN
               NEWFRONT(NIN) = SBUF(L+1)
             ELSE     ! interface 17 avec courbure
               SIZE_T(NIN) = SBUF(L+1)
             END IF
             L = L + 1
             TZINF(KK) = SBUF(L+1)
             L = L + 1
C DELTA_PMAX_GAP
             DELTA_PMAX_GAP(NIN) = SBUF(L+1)
             L = L + 1
C T25 main gap changes with thickness change
             MAXDGAP(NIN) = SBUF(L+1)
             L = L + 1
            END IF
          END DO
         END IF
      ELSE
C Proc IRING
        L = 1
        NBIRECV = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC) THEN
            IF(IRCVFROM(NINTER+1,P)>0.OR.
     +         ISENDTO(NINTER+1,P)>0) THEN
              NBIRECV = NBIRECV + 1
              IRINDEXI(NBIRECV)=P
              MSGTYP =  MSGOFF
              SIZ = I_LEN*NINTC
              IADR(P)=L
              CALL MPI_IRECV(
     S          SBUF(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G          MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
              L = L + SIZ
            END IF
          END IF
        ENDDO
C
C Attente reception
C
        DO I = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
          L = IADR(P)
          DO KK=1,NINTC
            NIN = INTLIST(KK)
            IF(IRCVFROM(NIN,P)/=0.OR.
     +         ISENDTO(NIN,P)/=0)THEN
C X MIN/MAX
              XSLV_L( 1,NIN) = MAX(XSLV_L( 1,NIN),SBUF(L))
              XSLV_L( 2,NIN) = MAX(XSLV_L( 2,NIN),SBUF(L+1))
              XSLV_L( 3,NIN) = MAX(XSLV_L( 3,NIN),SBUF(L+2))
              XSLV_L( 4,NIN) = MIN(XSLV_L( 4,NIN),SBUF(L+3))
              XSLV_L( 5,NIN) = MIN(XSLV_L( 5,NIN),SBUF(L+4))
              XSLV_L( 6,NIN) = MIN(XSLV_L( 6,NIN),SBUF(L+5))
              XSLV_L( 7,NIN) = MAX(XSLV_L( 7,NIN),SBUF(L+6))
              XSLV_L( 8,NIN) = MAX(XSLV_L( 8,NIN),SBUF(L+7))
              XSLV_L( 9,NIN) = MAX(XSLV_L( 9,NIN),SBUF(L+8))
              XSLV_L(10,NIN) = MIN(XSLV_L(10,NIN),SBUF(L+9))
              XSLV_L(11,NIN) = MIN(XSLV_L(11,NIN),SBUF(L+10))
              XSLV_L(12,NIN) = MIN(XSLV_L(12,NIN),SBUF(L+11))
              XSLV_L(13,NIN) = MAX(XSLV_L(13,NIN),SBUF(L+12))
              XSLV_L(14,NIN) = MAX(XSLV_L(14,NIN),SBUF(L+13))
              XSLV_L(15,NIN) = MAX(XSLV_L(15,NIN),SBUF(L+14))
              XSLV_L(16,NIN) = MIN(XSLV_L(16,NIN),SBUF(L+15))
              XSLV_L(17,NIN) = MIN(XSLV_L(17,NIN),SBUF(L+16))
              XSLV_L(18,NIN) = MIN(XSLV_L(18,NIN),SBUF(L+17))
              L = L + 18
C
              XMSR_L(1,NIN) = MAX(XMSR_L(1,NIN),SBUF(L))
              XMSR_L(2,NIN) = MAX(XMSR_L(2,NIN),SBUF(L+1))
              XMSR_L(3,NIN) = MAX(XMSR_L(3,NIN),SBUF(L+2))
              XMSR_L(4,NIN) = MIN(XMSR_L(4,NIN),SBUF(L+3))
              XMSR_L(5,NIN) = MIN(XMSR_L(5,NIN),SBUF(L+4))
              XMSR_L(6,NIN) = MIN(XMSR_L(6,NIN),SBUF(L+5))
              XMSR_L(7,NIN) = MAX(XMSR_L(7,NIN),SBUF(L+6))
              XMSR_L(8,NIN) = MAX(XMSR_L(8,NIN),SBUF(L+7))
              XMSR_L(9,NIN) = MAX(XMSR_L(9,NIN),SBUF(L+8))
              XMSR_L(10,NIN) = MIN(XMSR_L(10,NIN),SBUF(L+9))
              XMSR_L(11,NIN) = MIN(XMSR_L(11,NIN),SBUF(L+10))
              XMSR_L(12,NIN) = MIN(XMSR_L(12,NIN),SBUF(L+11))
              L = L + 12
C V MIN/MAX
              VSLV_L(1,NIN) = MAX(VSLV_L(1,NIN),SBUF(L))
              VSLV_L(2,NIN) = MAX(VSLV_L(2,NIN),SBUF(L+1))
              VSLV_L(3,NIN) = MAX(VSLV_L(3,NIN),SBUF(L+2))
              VSLV_L(4,NIN) = MIN(VSLV_L(4,NIN),SBUF(L+3))
              VSLV_L(5,NIN) = MIN(VSLV_L(5,NIN),SBUF(L+4))
              VSLV_L(6,NIN) = MIN(VSLV_L(6,NIN),SBUF(L+5))
              L = L + 6
C
              VMSR_L(1,NIN) = MAX(VMSR_L(1,NIN),SBUF(L))
              VMSR_L(2,NIN) = MAX(VMSR_L(2,NIN),SBUF(L+1))
              VMSR_L(3,NIN) = MAX(VMSR_L(3,NIN),SBUF(L+2))
              VMSR_L(4,NIN) = MIN(VMSR_L(4,NIN),SBUF(L+3))
              VMSR_L(5,NIN) = MIN(VMSR_L(5,NIN),SBUF(L+4))
              VMSR_L(6,NIN) = MIN(VMSR_L(6,NIN),SBUF(L+5))
              L = L + 6
              IF(IPARI(7,NIN)/=17)THEN
C NEWFRONT
                NEWFRONT(NIN) = NEWFRONT(NIN)+NINT(SBUF(L))
              ELSE     ! interface 17 avec courbure
C SIZE
                SIZE_T(NIN) = SIZE_T(NIN)+SBUF(L)
              END IF
              L = L + 1
C TZINF
              TZINF(KK) = MIN(TZINF(KK),SBUF(L))
              L = L + 1
C DELTA_PMAX_GAP
              DELTA_PMAX_GAP(NIN)= MAX(DELTA_PMAX_GAP(NIN),SBUF(L) )
                L = L + 1
C T25 main gap changes with thickness change
              MAXDGAP(NIN)= MAX(MAXDGAP(NIN),SBUF(L) )
                L = L + 1
            END IF
          END DO
        END DO
C
        L = 0
        DO I = 1, NBIRECV
          P=IRINDEXI(I)
C
          IADR(P) = L+1
          DO KK=1,NINTC
            NIN = INTLIST(KK)
            IF(IRCVFROM(NIN,P)/=0.OR.
     +         ISENDTO(NIN,P)/=0)THEN
              DO J=1,18
                SBUF(L+J) = XSLV_L(J,NIN)
              END DO
              L = L + 18
              DO J=1,12
                SBUF(L+J) = XMSR_L(J,NIN)
              END DO
              L = L + 12
              DO J=1,6
              SBUF(L+J) = VSLV_L(J,NIN)
              END DO
              L = L + 6
              DO J=1,6
              SBUF(L+J) = VMSR_L(J,NIN)
              END DO
              L = L + 6
              IF(IPARI(7,NIN)/=17)THEN
                SBUF(L+1) = NEWFRONT(NIN)
              ELSE     ! interface 17 avec courbure
                SBUF(L+1) = SIZE_T(NIN)
              END IF
              L = L + 1
              SBUF(L+1) = TZINF(KK)
              L = L + 1
                SBUF(L+1) = DELTA_PMAX_GAP(NIN)
                L = L + 1
C T25 main gap changes with thickness change
                SBUF(L+1) = MAXDGAP(NIN)
                L = L + 1
            END IF
          END DO
C
          MSGTYP =  MSGOFF
          CALL MPI_ISEND(
     S      SBUF(IADR(P)),L-IADR(P)+1,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDDO
C
        DO I = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEXI,STATUS,IERROR)
C          P=IRINDEXI(I)
        END DO
C
      END IF
C
#endif
      RETURN
      END
