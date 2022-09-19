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
Chd|  SPMD_SPHGAT                   source/mpi/sph/spmd_sphgat.F  
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_IALLTOALL                source/mpi/generic/spmd_ialltoall.F
Chd|        SPMD_IALLTOALLV_INT           source/mpi/generic/spmd_ialltoallv_int.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGAT(KXSP ,IXSP, WSP2SORT, IREDUCE, LGAUGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),
     .        WSP2SORT(*), IREDUCE, LGAUGE(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, NN0, NSP, IDEB, INITV, IG,
     .        MSGTYP, LOC_PROC, NBIRECV,
     .        IERROR, IERROR1, LEN, NVOIS1, NVOIS2,
     .        REQ_SD(NSPMD), REQ_SD2(NSPMD),
     .        INDEX(NSPHR), STATUS(MPI_STATUS_SIZE),
     .        MSGOFF,MSGOFF2
       my_real
     .        XSPHTMP(SIZSPT,NSPHR), SBUFCOM(2,NSPMD),BUFCOM(2,NSPMD)

       INTEGER :: REQUEST_SBUF

       INTEGER :: REQUEST_INDEX
       INTEGER, DIMENSION(NSPMD) :: DISPLS_INDEX,DISPLS_LSPHS
       INTEGER, DIMENSION(NSPMD) :: SEND_SIZE_INDEX,RCV_SIZE_LSPHS
       INTEGER ::TOTAL_SEND_SIZE_INDEX,TOTAL_RCV_SIZE_LSPHS
       DATA MSGOFF/2004/
       DATA MSGOFF2/2005/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1

C Compactage des structures
C
      IDEB = 0
      NN = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          SBUFCOM(1,P) = IREDUCE
          NSP = PSPHR(P)
          NN0 = NN
          DO I = 1, NSP
            IF(XSPHR(1,I+IDEB)<ZERO)THEN
              NN = NN + 1
              INDEX(I+IDEB) = NN
              XSPHTMP(1,NN) = -XSPHR(1,I+IDEB)
              XSPHTMP(2,NN) =  XSPHR(2,I+IDEB)
              XSPHTMP(3,NN) =  XSPHR(3,I+IDEB)
              XSPHTMP(4,NN) =  XSPHR(4,I+IDEB)
              XSPHTMP(5,NN) =  XSPHR(5,I+IDEB)
              XSPHTMP(6,NN) =  XSPHR(6,I+IDEB)
            END IF
          END DO
          IDEB = IDEB + NSP
          PSPHR(P) = NN-NN0
          MSGTYP = MSGOFF
          SBUFCOM(2,P) = PSPHR(P)
        ELSE
          SBUFCOM(1:2,P) = ZERO
        END IF
      END DO

!   -------------------------
!   alltoall communication with uniform size
!   for real array : send : SBUFCOM --> rcv : BUFCOM
      CALL SPMD_IALLTOALL(SBUFCOM,BUFCOM,2*NSPMD,2,
     .                     2*NSPMD,2,REQUEST_SBUF,MPI_COMM_WORLD)
!   -------------------------


      NSPHR = NN
C
      IERROR = 0
      IF(ALLOCATED(XSPHR))DEALLOCATE(XSPHR)
C reallocation avec nouveau NSPHR sur la taille totale
      ALLOCATE(XSPHR(SIZSPC,NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(ALLOCATED(WACOMPR))DEALLOCATE(WACOMPR)
C reallocation WACOMPR en prevision echange spforcp et splissv
      ALLOCATE(WACOMPR(SIZSPW,NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
C reallocation des tags cellule active a recevoir
      IF(ALLOCATED(ISPHR))DEALLOCATE(ISPHR)

      ALLOCATE(ISPHR(NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(NSPCOND>0) THEN
C reallocation du tableau gerant les particules symetrisees
        IF(ALLOCATED(ISPSYMR))DEALLOCATE(ISPSYMR)
        ALLOCATE(ISPSYMR(NSPCOND,NSPHR),STAT=IERROR1)
        IERROR = IERROR + IERROR1
      END IF
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      XSPHR = 0

C recopie + init
      IF(NSPCOND>0)THEN
C si condition de symetrie alors pas d'optimisation sur particules active
C car particule symetrique de particule inactive eventuellement active
        INITV = 1
      ELSE
        INITV = 0
      END IF
      DO I = 1, NSPHR
        XSPHR(1,I) = XSPHTMP(1,I)
        XSPHR(2,I) = XSPHTMP(2,I)
        XSPHR(3,I) = XSPHTMP(3,I)
        XSPHR(4,I) = XSPHTMP(4,I)
        XSPHR(5,I) = XSPHTMP(5,I)
        XSPHR(6,I) = XSPHTMP(6,I)
        ISPHR(I) = INITV
      END DO
C
C Renumerotation + selection particules actives
C
      DO I=1, NSP2SORT
        N=WSP2SORT(I)
        NVOIS1 = KXSP(4,N)
        NVOIS2 = KXSP(5,N)
        DO NN = 1, NVOIS1
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
C flag differenciant les cellules actives des autres
            ISPHR(-IXSP(NN,N)) = 1
          END IF
        END DO
        DO NN = NVOIS1+1,NVOIS2
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
          END IF
        END DO
      END DO
C
C Gauges : Renumerotation + selection particules actives
C
      DO IG=1, NBGAUGE
        IF(LGAUGE(1,IG) > -(NUMELS+1))CYCLE
        N=NUMSPH+IG
        NVOIS1 = KXSP(4,N)
        NVOIS2 = KXSP(5,N)
        DO NN = 1, NVOIS1
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
C flag differenciant les cellules actives des autres
            ISPHR(-IXSP(NN,N)) = 1
          END IF
        END DO
        DO NN = NVOIS1+1,NVOIS2
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
          END IF
        END DO
      END DO
C
C Renvoi frontiere
C
      IDEB = 0
      DO P = 1, NSPMD
        NSP = PSPHR(P)
        IF(LOC_PROC/=P.AND.NSP>0)THEN
          DO I = 1, NSP
            INDEX(IDEB+I) = NINT(XSPHR(1,I+IDEB))
          END DO
          MSGTYP = MSGOFF2
          IDEB = IDEB + NSP
        END IF
      END DO
C
C Reception
C

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoallv comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm : SBUFCOM/RBUFCOM
      CALL MPI_WAIT(REQUEST_SBUF,STATUS,IERROR)
!   -------------------------
#endif

      NSPHS = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          MSGTYP = MSGOFF
          IREDUCE = MAX(IREDUCE,NINT(BUFCOM(1,P)))
          PSPHS(P) = NINT(BUFCOM(2,P))
          NSPHS = NSPHS + PSPHS(P)
        END IF
      END DO
C reallocation liste cellule a envoyer
      IF(ALLOCATED(LSPHS))DEALLOCATE(LSPHS)
      ALLOCATE(LSPHS(NSPHS),STAT=IERROR)
C reallocation des tags cellule active a envoyer
      IF(ALLOCATED(ISPHS))DEALLOCATE(ISPHS)
      ALLOCATE(ISPHS(NSPHS),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      IDEB = 1
      DO P = 1, NSPMD
        IF(P/=LOC_PROC.AND.PSPHS(P)>0)THEN
          MSGTYP = MSGOFF2
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO

!   -------------------------
!   compute the displacement, number of element
!   and total number of element (send and rcv)
      DISPLS_INDEX(1:NSPMD) = 0
      DISPLS_LSPHS(1:NSPMD) = 0
      SEND_SIZE_INDEX(1:NSPMD) = 0
      RCV_SIZE_LSPHS(1:NSPMD) = 0
      TOTAL_SEND_SIZE_INDEX = 0
      TOTAL_RCV_SIZE_LSPHS = 0


      DISPLS_INDEX(1) = 0
      DISPLS_LSPHS(1) = 0
      DO P=1,NSPMD
          IF(P/=LOC_PROC) THEN
            SEND_SIZE_INDEX(P) = PSPHR(P)
            RCV_SIZE_LSPHS(P) = PSPHS(P)
          ENDIF
          TOTAL_SEND_SIZE_INDEX = TOTAL_SEND_SIZE_INDEX + SEND_SIZE_INDEX(P)
          TOTAL_RCV_SIZE_LSPHS = TOTAL_RCV_SIZE_LSPHS + RCV_SIZE_LSPHS(P)
      ENDDO
      DO P=2,NSPMD
          DISPLS_INDEX(P) = DISPLS_INDEX(P-1) + SEND_SIZE_INDEX(P-1)
          DISPLS_LSPHS(P) = DISPLS_LSPHS(P-1) + RCV_SIZE_LSPHS(P-1)
      ENDDO
!   -------------------------


!   -------------------------
!   alltoall communication with non-uniform size
!   for integer array : send : INDEX --> rcv : LSPHS
      CALL SPMD_IALLTOALLV_INT(INDEX,LSPHS,
     .          SEND_SIZE_INDEX,TOTAL_SEND_SIZE_INDEX,DISPLS_INDEX,
     .          TOTAL_RCV_SIZE_LSPHS,RCV_SIZE_LSPHS,DISPLS_LSPHS,
     .          REQUEST_INDEX,MPI_COMM_WORLD,NSPMD)
!   -------------------------

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking alltoallv comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm
      CALL MPI_WAIT(REQUEST_INDEX,STATUS,IERROR)
!   -------------------------
#endif
C        
#endif
      RETURN
      END




















Chd|====================================================================
Chd|  SPMD_SPHGAT_old               source/mpi/sph/spmd_sphgat.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGAT_old(KXSP ,IXSP, WSP2SORT, IREDUCE, LGAUGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),
     .        WSP2SORT(*), IREDUCE, LGAUGE(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, NN0, NSP, IDEB, INITV, IG,
     .        MSGTYP, LOC_PROC, NBIRECV,
     .        IERROR, IERROR1, LEN, NVOIS1, NVOIS2,
     .        REQ_SD(NSPMD), REQ_SD2(NSPMD),
     .        INDEX(NSPHR), STATUS(MPI_STATUS_SIZE),
     .        MSGOFF,MSGOFF2
       my_real
     .        XSPHTMP(SIZSPT,NSPHR), BUFCOM(2,NSPMD)
       DATA MSGOFF/2004/
       DATA MSGOFF2/2005/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1

C Compactage des structures
C
      IDEB = 0
      NN = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          BUFCOM(1,P) = IREDUCE
          NSP = PSPHR(P)
          NN0 = NN
          DO I = 1, NSP
            IF(XSPHR(1,I+IDEB)<ZERO)THEN
              NN = NN + 1
              INDEX(I+IDEB) = NN
              XSPHTMP(1,NN) = -XSPHR(1,I+IDEB)
              XSPHTMP(2,NN) =  XSPHR(2,I+IDEB)
              XSPHTMP(3,NN) =  XSPHR(3,I+IDEB)
              XSPHTMP(4,NN) =  XSPHR(4,I+IDEB)
              XSPHTMP(5,NN) =  XSPHR(5,I+IDEB)
              XSPHTMP(6,NN) =  XSPHR(6,I+IDEB)
            END IF
          END DO
          IDEB = IDEB + NSP
          PSPHR(P) = NN-NN0
          MSGTYP = MSGOFF
          BUFCOM(2,P) = PSPHR(P)
          CALL MPI_ISEND(
     S      BUFCOM(1,P),2,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        END IF
      END DO

      NSPHR = NN
C
      IERROR = 0
      IF(ALLOCATED(XSPHR))DEALLOCATE(XSPHR)
C reallocation avec nouveau NSPHR sur la taille totale
      ALLOCATE(XSPHR(SIZSPC,NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(ALLOCATED(WACOMPR))DEALLOCATE(WACOMPR)
C reallocation WACOMPR en prevision echange spforcp et splissv
      ALLOCATE(WACOMPR(SIZSPW,NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
C reallocation des tags cellule active a recevoir
      IF(ALLOCATED(ISPHR))DEALLOCATE(ISPHR)

      ALLOCATE(ISPHR(NSPHR),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(NSPCOND>0) THEN
C reallocation du tableau gerant les particules symetrisees
        IF(ALLOCATED(ISPSYMR))DEALLOCATE(ISPSYMR)
        ALLOCATE(ISPSYMR(NSPCOND,NSPHR),STAT=IERROR1)
        IERROR = IERROR + IERROR1
      END IF
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      XSPHR = 0

C recopie + init
      IF(NSPCOND>0)THEN
C si condition de symetrie alors pas d'optimisation sur particules active
C car particule symetrique de particule inactive eventuellement active
        INITV = 1
      ELSE
        INITV = 0
      END IF
      DO I = 1, NSPHR
        XSPHR(1,I) = XSPHTMP(1,I)
        XSPHR(2,I) = XSPHTMP(2,I)
        XSPHR(3,I) = XSPHTMP(3,I)
        XSPHR(4,I) = XSPHTMP(4,I)
        XSPHR(5,I) = XSPHTMP(5,I)
        XSPHR(6,I) = XSPHTMP(6,I)
        ISPHR(I) = INITV
      END DO
C
C Renumerotation + selection particules actives
C
      DO I=1, NSP2SORT
        N=WSP2SORT(I)
        NVOIS1 = KXSP(4,N)
        NVOIS2 = KXSP(5,N)
        DO NN = 1, NVOIS1
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
C flag differenciant les cellules actives des autres
            ISPHR(-IXSP(NN,N)) = 1
          END IF
        END DO
        DO NN = NVOIS1+1,NVOIS2
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
          END IF
        END DO
      END DO
C
C Gauges : Renumerotation + selection particules actives
C
      DO IG=1, NBGAUGE
        IF(LGAUGE(1,IG) > -(NUMELS+1))CYCLE
        N=NUMSPH+IG
        NVOIS1 = KXSP(4,N)
        NVOIS2 = KXSP(5,N)
        DO NN = 1, NVOIS1
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
C flag differenciant les cellules actives des autres
            ISPHR(-IXSP(NN,N)) = 1
          END IF
        END DO
        DO NN = NVOIS1+1,NVOIS2
          IF(IXSP(NN,N)<ZERO) THEN
C renumerotation
            IXSP(NN,N) = -INDEX(-IXSP(NN,N))
          END IF
        END DO
      END DO
C
C Renvoi frontiere
C
      IDEB = 0
      DO P = 1, NSPMD
        NSP = PSPHR(P)
        IF(LOC_PROC/=P.AND.NSP>0)THEN
          DO I = 1, NSP
            INDEX(IDEB+I) = NINT(XSPHR(1,I+IDEB))
          END DO
          MSGTYP = MSGOFF2
          CALL MPI_ISEND(
     S      INDEX(IDEB+1),NSP,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD2(P),IERROR)
          IDEB = IDEB + NSP
        END IF
      END DO
C
C Reception
C
      NSPHS = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFCOM(1,LOC_PROC),2,REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          IREDUCE = MAX(IREDUCE,NINT(BUFCOM(1,LOC_PROC)))
          PSPHS(P) = NINT(BUFCOM(2,LOC_PROC))
          NSPHS = NSPHS + PSPHS(P)
        END IF
      END DO
C reallocation liste cellule a envoyer
      IF(ALLOCATED(LSPHS))DEALLOCATE(LSPHS)
      ALLOCATE(LSPHS(NSPHS),STAT=IERROR)
C reallocation des tags cellule active a envoyer
      IF(ALLOCATED(ISPHS))DEALLOCATE(ISPHS)
      ALLOCATE(ISPHS(NSPHS),STAT=IERROR1)
      IERROR = IERROR + IERROR1
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
      IDEB = 1
      DO P = 1, NSPMD
        IF(P/=LOC_PROC.AND.PSPHS(P)>0)THEN
          MSGTYP = MSGOFF2
          CALL MPI_RECV(LSPHS(IDEB),PSPHS(P),MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
          IF(PSPHR(P)/=0)THEN
            CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
          END IF
        END IF
      END DO
C        
#endif
      RETURN
      END
