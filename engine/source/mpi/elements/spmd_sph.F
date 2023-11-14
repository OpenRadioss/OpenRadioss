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
Chd|  SPMD_EXCH_A_SOL2SPH           source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_SOL2SPH(
     1   A6       ,ITAG   ,IAD_ELEM ,FR_ELEM,SIZE,
     2   LENR     )
C--------------------------------------
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAG(*), IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR
      DOUBLE PRECISION A6(6,3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/2000/

      DOUBLE PRECISION
     .        RBUF(SIZE*LENR ),
     .        SBUF(SIZE*LENR )
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     SIZE=19
C
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(ITAG(NOD)/=0)THEN
            SBUF(L  ) =  J-IAD_ELEM(1,I)+1
            SBUF(L+1) =  A6(1,1,NOD)
            SBUF(L+2) =  A6(1,2,NOD)
            SBUF(L+3) =  A6(1,3,NOD)
            SBUF(L+4) =  A6(2,1,NOD)
            SBUF(L+5) =  A6(2,2,NOD)
            SBUF(L+6) =  A6(2,3,NOD)
            SBUF(L+7) =  A6(3,1,NOD)
            SBUF(L+8) =  A6(3,2,NOD)
            SBUF(L+9) =  A6(3,3,NOD)
            SBUF(L+10) =  A6(4,1,NOD)
            SBUF(L+11) =  A6(4,2,NOD)
            SBUF(L+12) =  A6(4,3,NOD)
            SBUF(L+13) =  A6(5,1,NOD)
            SBUF(L+14) =  A6(5,2,NOD)
            SBUF(L+15) =  A6(5,3,NOD)
            SBUF(L+16) =  A6(6,1,NOD)
            SBUF(L+17) =  A6(6,2,NOD)
            SBUF(L+18) =  A6(6,3,NOD)
            L = L + SIZE
          END IF
        END DO
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
C   test si msg necessaire a envoyer a completer par test interface
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
C   test si msg necessaire a envoyer a completer par test interface
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
          CALL MPI_GET_COUNT(STATUS,MPI_DOUBLE_PRECISION,SIZ,ierror)
          SIZ = SIZ/SIZE
#include        "vectorize.inc"
C         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          DO K=1,SIZ
            J = NINT(RBUF(L)) + IAD_ELEM(1,I)-1
            NOD = FR_ELEM(J)
            
            ITAG(NOD)=1
            A6(1,1,NOD)=A6(1,1,NOD)+ RBUF(L+1)
            A6(1,2,NOD)=A6(1,2,NOD)+ RBUF(L+2)
            A6(1,3,NOD)=A6(1,3,NOD)+ RBUF(L+3)
            A6(2,1,NOD)=A6(2,1,NOD)+ RBUF(L+4)
            A6(2,2,NOD)=A6(2,2,NOD)+ RBUF(L+5)
            A6(2,3,NOD)=A6(2,3,NOD)+ RBUF(L+6)
            A6(3,1,NOD)=A6(3,1,NOD)+ RBUF(L+7)
            A6(3,2,NOD)=A6(3,2,NOD)+ RBUF(L+8)
            A6(3,3,NOD)=A6(3,3,NOD)+ RBUF(L+9)
            A6(4,1,NOD)=A6(4,1,NOD)+ RBUF(L+10)
            A6(4,2,NOD)=A6(4,2,NOD)+ RBUF(L+11)
            A6(4,3,NOD)=A6(4,3,NOD)+ RBUF(L+12)
            A6(5,1,NOD)=A6(5,1,NOD)+ RBUF(L+13)
            A6(5,2,NOD)=A6(5,2,NOD)+ RBUF(L+14)
            A6(5,3,NOD)=A6(5,3,NOD)+ RBUF(L+15)
            A6(6,1,NOD)=A6(6,1,NOD)+ RBUF(L+16)
            A6(6,2,NOD)=A6(6,2,NOD)+ RBUF(L+17)
            A6(6,3,NOD)=A6(6,3,NOD)+ RBUF(L+18)

            L = L + SIZE
          END DO
        END IF
      END DO
C
C   wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_SPHBOX                   source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHBOX(KXSP ,SPBUF,WSP2SORT,BMINMAL,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
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
     .        REQ_RB(NSPMD), REQ_SB(NSPMD), REQ_SD(NSPMD),
     .        REQ_RD(NSPMD), REQ_SD2(NSPMD),
     .        IRINDEXI(NSPMD), ISINDEXI(NSPMD), NBO(NSPMD), 
     .        INDEX(NSP2SORT), STATUS(MPI_STATUS_SIZE),MSGOFF,
     .        MSGOFF2,MSGOFF3
      my_real 
     .        BMINMA(6,NSPMD)  
      TYPE(real_pointer5), DIMENSION(NSPMD) :: BUF
      DATA MSGOFF/2001/
      DATA MSGOFF2/2002/
      DATA MSGOFF3/2003/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC = ISPMD + 1
C
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
      DO P = 1, NSPMD
        IF(P/=LOC_PROC) THEN
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     .      BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
        END IF
      END DO
C
      NBIRECV=0
      DO P = 1, NSPMD
        IF(LOC_PROC/=P) THEN
          MSGTYP = MSGOFF
          NBIRECV=NBIRECV+1
          IRINDEXI(NBIRECV)=P
          CALL MPI_IRECV(
     .      BMINMA(1,P)   ,6              ,REAL  ,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,REQ_RB(NBIRECV),IERROR)
        END IF
      END DO
C
      DO KK = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
        P=IRINDEXI(INDEXI)
        NBO(P) = 0
        NB = 0
        DO I=1, NSP2SORT
          N=WSP2SORT(I)
          NOD=KXSP(3,N)
          IF(X(1,NOD)<=BMINMA(1,P)) THEN
           IF(X(1,NOD)>=BMINMA(4,P)) THEN
            IF(X(2,NOD)<=BMINMA(2,P)) THEN
             IF(X(2,NOD)>=BMINMA(5,P)) THEN
              IF(X(3,NOD)<=BMINMA(3,P)) THEN
               IF(X(3,NOD)>=BMINMA(6,P)) THEN
                 NB = NB + 1
                 INDEX(NB) = N
               ENDIF
              ENDIF
             ENDIF
            ENDIF
           ENDIF
          ENDIF
        END DO
        NBO(P) = NB
        MSGTYP = MSGOFF2
        CALL MPI_ISEND(NBO(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
        IF (NB>0) THEN
          ALLOCATE(BUF(P)%P(SIZSPT*NB),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
          L = 0
          DO J = 1, NB
            N = INDEX(J)
            NOD = KXSP(3,N)
            BUF(P)%P(L+1) = N
            BUF(P)%P(L+2) = SPBUF(1,N)
            BUF(P)%P(L+3) = X(1,NOD)
            BUF(P)%P(L+4) = X(2,NOD)
            BUF(P)%P(L+5) = X(3,NOD)
            BUF(P)%P(L+6) = KXSP(8,N)
            L = L + SIZSPT
          END DO
          MSGTYP = MSGOFF3
          CALL MPI_ISEND(BUF(P)%P(1),L,REAL,IT_SPMD(P),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_SD2(P),IERROR)
        END IF  
      END DO
C   
      NSPHR = 0
      L=0
      DO P = 1, NSPMD
        PSPHR(P) = 0
        IF(LOC_PROC/=P) THEN
          MSGTYP = MSGOFF2
          CALL MPI_RECV(PSPHR(P),1,MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          IF(PSPHR(P)>0) THEN
            L=L+1
            ISINDEXI(L)=P
            NSPHR = NSPHR + PSPHR(P)
          END IF
        END IF
      END DO
      NBIRECV=L
C
      IF(NSPHR>0) THEN
        IF(ALLOCATED(XSPHR))DEALLOCATE(XSPHR)
        ALLOCATE(XSPHR(SIZSPT,NSPHR),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
        IDEB = 1
        DO L = 1, NBIRECV
          P = ISINDEXI(L)
          LEN = PSPHR(P)*SIZSPT
          MSGTYP = MSGOFF3
          CALL MPI_IRECV(XSPHR(1,IDEB),LEN,REAL,IT_SPMD(P),
     .                   MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
          IDEB = IDEB + PSPHR(P)
        END DO
        DO L = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
        END DO
      END IF  
C
      DO P = 1, NSPMD
        IF(P/=LOC_PROC) THEN
          CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
        ENDIF
      END DO
C
      DO P = 1, NSPMD
        IF(P/=LOC_PROC) THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
          IF(NBO(P)/=0) THEN
            CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
            DEALLOCATE(BUF(P)%P)
          END IF
        END IF
      END DO
C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_SPHGETDK                 source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETDK(TAB_DK,ACT,REQ_RECV)
C  Send the maximum distance of particules kept after a reduction
C  to the remote versions of these particles.
C  ACT = 1 : prepare reeception (IRECV)
C  ACT = 2 : isend and Wait 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ACT,REQ_RECV(NSPMD)
      my_real TAB_DK(*) 
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP, LOC_PROC, P,
     .        IERROR,N,IDEB, 
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF

       DATA MSGOFF/2028/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      LOC_PROC = ISPMD+1
       
      IF(NSPMD > 1 .AND. ACT == 1) THEN
        IDEB = 0
        DO P = 1, NSPMD
          IF(PSPHR(P)/=0) THEN
            MSGTYP = MSGOFF
            CALL MPI_IRECV(DKR(IDEB+1),PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,REQ_RECV(P),IERROR)
            IDEB = IDEB + PSPHR(P)
          END IF
        ENDDO
      ELSEIF (NSPMD > 1 .AND. ACT == 2) THEN
        IDEB = 0
        DO P = 1, NSPMD
          IF(PSPHS(P)/=0 ) THEN
           DO N = 1, PSPHS(P)
             DKS(IDEB+N) = TAB_DK(LSPHS(IDEB+N))
           ENDDO
           MSGTYP = MSGOFF
           CALL MPI_ISEND(
     .         DKS(IDEB+1),PSPHS(P),REAL,IT_SPMD(P),MSGTYP,
     .         MPI_COMM_WORLD,REQ_SD(P),IERROR)
           IDEB = IDEB + PSPHS(P)
          ENDIF
        ENDDO

        DO P = 1, NSPMD
          IF(PSPHR(P)/=0) THEN
            CALL MPI_WAIT(REQ_RECV(P),STATUS,IERROR)
          END IF
        ENDDO

        DO P = 1, NSPMD
          IF(PSPHS(P)/=0) THEN
            CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
          END IF
        ENDDO
   
      ENDIF

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETISPH               source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETISPH()
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
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, MSGTYP, LOC_PROC, IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        IDEB,REQ_SD(NSPMD),MSGOFF,MSGOFF2
      DATA MSGOFF/2006/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi flag cellules actives
C      
      IDEB = 1
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      ISPHR(IDEB),PSPHR(P),MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Reception flag cellules actives
C
      IDEB = 1
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_RECV(ISPHS(IDEB),PSPHS(P),MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C 

       DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETX                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETX(KXSP ,SPBUF, X, IPARTSP)
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
      INTEGER KXSP(NISP,*), IPARTSP(*)
       my_real
     .        SPBUF(NSPBUF,*), X(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(6,NSPHS), BUFR(6,NSPHR)
       DATA MSGOFF/2008/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi X, H sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
ctmp+1
       IF(PSPHS(P)/=0)THEN
        DO N = 1, PSPHS(P)
          ICELL = LSPHS(IDEB+N)
          INOD = KXSP(3,ICELL)
          BUFS(1,N+IDEB) = SPBUF(1,ICELL)
          BUFS(2,N+IDEB) = X(1,INOD)
          BUFS(3,N+IDEB) = X(2,INOD)
          BUFS(4,N+IDEB) = X(3,INOD)    
          BUFS(5,N+IDEB) = KXSP(2,ICELL)    
          BUFS(6,N+IDEB) = IPARTSP(ICELL)    
        END DO
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),PSPHS(P)*6,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        IDEB = IDEB + PSPHS(P)
ctmp+1
       ENDIF
      END DO
C
C Reception X, H
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,6*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          DO N = 1, PSPHR(P)
            XSPHR(2,IDEB+N) = BUFR(1,N)
            XSPHR(3,IDEB+N) = BUFR(2,N)
            XSPHR(4,IDEB+N) = BUFR(3,N)
            XSPHR(5,IDEB+N) = BUFR(4,N)
              XSPHR(13,IDEB+N)= BUFR(5,N)
              XSPHR(14,IDEB+N)= BUFR(6,N)
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
ctmp+1        IF(PSPHR(P)/=0)THEN
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETW                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETW(SPBUF,WACOMP,WA,WAR)
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
       my_real
     .        SPBUF(NSPBUF,*), WACOMP(16,*), WA(KWASPH,*),
     .        WAR(10,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(15,NSPHS), BUFR(15,NSPHR)
       DATA MSGOFF/2010/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi WACOMP, WA, RHO sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHS(P)>0)THEN
          NN = 0
          DO N = 1, PSPHS(P)
            IF(ISPHS(IDEB+N)==1) THEN
              NN = NN + 1
              ICELL = LSPHS(IDEB+N)
C             INOD = KXSP(3,ICELL)
              BUFS(1,NN+IDEB)  = SPBUF(2,ICELL)
              BUFS(2,NN+IDEB)  = WACOMP(1,ICELL)
              BUFS(3,NN+IDEB)  = WACOMP(5,ICELL)
              BUFS(4,NN+IDEB)  = WACOMP(6,ICELL)
              BUFS(5,NN+IDEB)  = WACOMP(7,ICELL)
              BUFS(6,NN+IDEB)  = WA(1,ICELL)
              BUFS(7,NN+IDEB)  = WA(2,ICELL)
              BUFS(8,NN+IDEB)  = WA(3,ICELL)
              BUFS(9,NN+IDEB)  = WA(4,ICELL)
              BUFS(10,NN+IDEB) = WA(5,ICELL)
              BUFS(11,NN+IDEB) = WA(6,ICELL)
              BUFS(12,NN+IDEB) = WA(8,ICELL)
              BUFS(13,NN+IDEB) = WA(9,ICELL)
              BUFS(14,NN+IDEB) = WA(13,ICELL)
              BUFS(15,NN+IDEB) = WA(14,ICELL)
            END IF
          END DO
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),NN*15,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Reception WACOMP, WA, RHO
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,15*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              XSPHR(7,IDEB+N)  = BUFR(1,NN)
              WACOMPR(1,IDEB+N)= BUFR(2,NN)
              WACOMPR(2,IDEB+N)= ZERO
              WACOMPR(3,IDEB+N)= ZERO
              WACOMPR(4,IDEB+N)= ZERO
              WACOMPR(5,IDEB+N)= BUFR(3,NN)
              WACOMPR(6,IDEB+N)= BUFR(4,NN)
              WACOMPR(7,IDEB+N)= BUFR(5,NN)
              WACOMPR(8,IDEB+N)= ZERO
              WACOMPR(9,IDEB+N)= ZERO
              WACOMPR(10,IDEB+N)=ZERO
              WACOMPR(11,IDEB+N)=ZERO
              WACOMPR(12,IDEB+N)=ZERO
              WACOMPR(13,IDEB+N)=ZERO
              WACOMPR(14,IDEB+N)=ZERO
              WACOMPR(15,IDEB+N)=ZERO
              WACOMPR(16,IDEB+N)=ZERO
              WAR(1,IDEB+N)= BUFR(6,NN)
              WAR(2,IDEB+N)= BUFR(7,NN)
              WAR(3,IDEB+N)= BUFR(8,NN)
              WAR(4,IDEB+N)= BUFR(9,NN)
              WAR(5,IDEB+N)= BUFR(10,NN)
              WAR(6,IDEB+N)= BUFR(11,NN)
              WAR(7,IDEB+N)= BUFR(12,NN)
              WAR(8,IDEB+N)= BUFR(13,NN)
              WAR(9,IDEB+N)= BUFR(14,NN)
              WAR(10,IDEB+N)=BUFR(15,NN)
            END IF
          END DO    
          IDEB = IDEB + PSPHR(P)
        END IF

      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETSTB                source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETSTB(STAB,STABR)
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
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .        STAB(7,*), STABR(7,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(NSPHS), BUFR(NSPHR)
       DATA MSGOFF/2011/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi STAB sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHS(P)>0)THEN
          NN = 0
          DO N = 1, PSPHS(P)
            IF(ISPHS(IDEB+N)==1) THEN
              NN = NN + 1
              ICELL = LSPHS(IDEB+N)
C             INOD = KXSP(3,ICELL)
              BUFS(NN+IDEB)  = STAB(7,ICELL)
            END IF
          END DO
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      BUFS(IDEB+1),NN,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Reception STAB
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              STABR(7,IDEB+N)  = BUFR(NN)
            END IF
          END DO    
          IDEB = IDEB + PSPHR(P)
        END IF

      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETA                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETA(KXSP,SPBUF,A,ASPHR)
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
      INTEGER KXSP(NISP,*)
       my_real
     .        SPBUF(NSPBUF,*), A(3,*), ASPHR(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,MSGOFF,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE)
       my_real
     .        BUFS(4,NSPHS), BUFR(4,NSPHR)
       DATA MSGOFF/2012/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi A, H sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
ctmp+1
       IF(PSPHS(P)/=0)THEN
        NN = 0
        DO N = 1, PSPHS(P)
          IF(ISPHS(IDEB+N)==1) THEN
            NN = NN + 1
            ICELL = LSPHS(IDEB+N)
            INOD = KXSP(3,ICELL)
            BUFS(1,NN+IDEB) = SPBUF(1,ICELL)
            BUFS(2,NN+IDEB) = A(1,INOD)
            BUFS(3,NN+IDEB) = A(2,INOD)
            BUFS(4,NN+IDEB) = A(3,INOD)
          END IF
        END DO
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),NN*4,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        IDEB = IDEB + PSPHS(P)
ctmp+1
       END IF
      END DO
C
C Reception A, H 
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,4*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              XSPHR(2,IDEB+N) = BUFR(1,NN)
              ASPHR(1,IDEB+N) = BUFR(2,NN)
              ASPHR(2,IDEB+N) = BUFR(3,NN)
              ASPHR(3,IDEB+N) = BUFR(4,NN)
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
ctmp+1        IF(PSPHR(P)/=0)THEN
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETF                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETF(KXSP,SPBUF,A,MS,ASPHR)
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
      INTEGER KXSP(NISP,*)
       my_real
     .        SPBUF(NSPBUF,*), A(3,*), ASPHR(4,*), MS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(5,NSPHS), BUFR(5,NSPHR)
       DATA MSGOFF/2013/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi A, H sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
ctmp+1
       IF(PSPHS(P)/=0)THEN
        NN = 0
        DO N = 1, PSPHS(P)
          IF(ISPHS(IDEB+N)==1) THEN
            NN = NN + 1
            ICELL = LSPHS(IDEB+N)
            INOD = KXSP(3,ICELL)
            BUFS(1,NN+IDEB) = SPBUF(1,ICELL)
            BUFS(2,NN+IDEB) = A(1,INOD)
            BUFS(3,NN+IDEB) = A(2,INOD)
            BUFS(4,NN+IDEB) = A(3,INOD)
            BUFS(5,NN+IDEB) = MS(INOD)
          END IF
        END DO
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),NN*5,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        IDEB = IDEB + PSPHS(P)
ctmp+1
       END IF
      END DO
C
C Reception A, H 
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,5*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              XSPHR(2,IDEB+N) = BUFR(1,NN)
              ASPHR(1,IDEB+N) = BUFR(2,NN)
              ASPHR(2,IDEB+N) = BUFR(3,NN)
              ASPHR(3,IDEB+N) = BUFR(4,NN)
              ASPHR(4,IDEB+N) = BUFR(5,NN)
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
ctmp+1        IF(PSPHR(P)/=0)THEN
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETH                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPCLASV                       source/elements/sph/spclasv.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETH(KXSP ,SPBUF)
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
      INTEGER KXSP(NISP,*)
       my_real
     .        SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(NSPHS), BUFR(NSPHR)
       DATA MSGOFF/2014/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi H sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
       IF(PSPHS(P)/=0)THEN
        DO N = 1, PSPHS(P)
          ICELL = LSPHS(IDEB+N)
          INOD = KXSP(3,ICELL)
          BUFS(N+IDEB) = SPBUF(1,ICELL)
        END DO
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S      BUFS(IDEB+1),PSPHS(P),REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        IDEB = IDEB + PSPHS(P)
       ENDIF
      END DO
C
C Reception H
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          DO N = 1, PSPHR(P)
            XSPHR(2,IDEB+N) = BUFR(N)
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_ALL_DMAX                 source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        RBYVIT                        source/constraints/general/rbody/rbyvit.F
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_ALL_DMAX(V,LEN)
C max tableau V de taille LEN de type my_real
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE), I, IERROR
      my_real
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_ALLREDUCE(V,VTMP,LEN,REAL,MPI_MAX,
     .                     MPI_COMM_WORLD,IERROR)
        DO I = 1, LEN
            V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETT                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETT(WT,WTR,LAMBDA,LAMBDR)
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
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .        WT(*), WTR(*), LAMBDA(*), LAMBDR(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(2,NSPHS), BUFR(2,NSPHR)
       DATA MSGOFF/2015/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi WACOMP, WA, RHO sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHS(P)>0)THEN
          NN = 0
          DO N = 1, PSPHS(P)
            IF(ISPHS(IDEB+N)==1) THEN
              NN = NN + 1
              ICELL = LSPHS(IDEB+N)
              BUFS(1,NN+IDEB)  = WT(ICELL)
              BUFS(2,NN+IDEB)  = LAMBDA(ICELL)
            END IF
          END DO
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),2*NN,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Reception WT
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,PSPHR(P)*2,REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              WTR(IDEB+N)   = BUFR(1,NN)
              LAMBDR(IDEB+N)= BUFR(2,NN)
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETG                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETG(WGRADT,WACOMP,WGR)
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
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .        WGRADT(3,*), WACOMP(16,*), WGR(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(7,NSPHS), BUFR(7,NSPHR)
       DATA MSGOFF/2016/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
C Envoi WACOMP, WA, RHO sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHS(P)>0)THEN
          NN = 0
          DO N = 1, PSPHS(P)
            IF(ISPHS(IDEB+N)==1) THEN
              NN = NN + 1
              ICELL = LSPHS(IDEB+N)
C             INOD = KXSP(3,ICELL)
              BUFS(1,NN+IDEB)  = WGRADT(1,ICELL)
              BUFS(2,NN+IDEB)  = WGRADT(2,ICELL)
              BUFS(3,NN+IDEB)  = WGRADT(3,ICELL)
              BUFS(4,NN+IDEB)  = WACOMP(1,ICELL)
              BUFS(5,NN+IDEB)  = WACOMP(5,ICELL)
              BUFS(6,NN+IDEB)  = WACOMP(6,ICELL)
              BUFS(7,NN+IDEB)  = WACOMP(7,ICELL)
            END IF
          END DO
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),NN*7,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Reception WACOMP, WA, RHO
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,7*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              WGR(1,IDEB+N)= BUFR(1,NN)
              WGR(2,IDEB+N)= BUFR(2,NN)
              WGR(3,IDEB+N)= BUFR(3,NN)
              WACOMPR(1,IDEB+N)= BUFR(4,NN)
              WACOMPR(2,IDEB+N)= ZERO
              WACOMPR(3,IDEB+N)= ZERO
              WACOMPR(4,IDEB+N)= ZERO
              WACOMPR(5,IDEB+N)= BUFR(5,NN)
              WACOMPR(6,IDEB+N)= BUFR(6,NN)
              WACOMPR(7,IDEB+N)= BUFR(7,NN)
              WACOMPR(8,IDEB+N)= ZERO
              WACOMPR(9,IDEB+N)= ZERO
              WACOMPR(10,IDEB+N)=ZERO
              WACOMPR(11,IDEB+N)=ZERO
              WACOMPR(12,IDEB+N)=ZERO
              WACOMPR(13,IDEB+N)=ZERO
              WACOMPR(14,IDEB+N)=ZERO
              WACOMPR(15,IDEB+N)=ZERO
              WACOMPR(16,IDEB+N)=ZERO
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETWA                 source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETWA(WA,WAR2,KXSP)
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
       my_real
     .       WAR2(9,*), WA(KWASPH,*)

      INTEGER KXSP(NISP,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       my_real
     .        BUFS(10,NSPHS), BUFR(10,NSPHR)
       DATA MSGOFF/2017/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
C
c need to get all remote values of WA and "IMPOSE" value

C Envoi WA sur cellules actives
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHS(P)>0)THEN
          NN = 0
          DO N = 1, PSPHS(P)
            IF(ISPHS(IDEB+N)==1) THEN
              NN = NN + 1
              ICELL = LSPHS(IDEB+N)
              BUFS(1,NN+IDEB)  = WA(1,ICELL)
              BUFS(2,NN+IDEB)  = WA(2,ICELL)
              BUFS(3,NN+IDEB)  = WA(3,ICELL)
              BUFS(4,NN+IDEB)  = WA(4,ICELL)
              BUFS(5,NN+IDEB) = WA(5,ICELL)
              BUFS(6,NN+IDEB) = WA(6,ICELL)
              BUFS(7,NN+IDEB) = WA(7,ICELL)
              BUFS(8,NN+IDEB) = WA(8,ICELL)
              BUFS(9,NN+IDEB) = WA(9,ICELL)
c IMPOSE value 
              BUFS(10,NN+IDEB) = KXSP(2,ICELL)/(NGROUP+1)
            END IF
          END DO
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      BUFS(1,IDEB+1),NN*10,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C Reception
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,10*PSPHR(P),REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          NN = 0
          DO N = 1, PSPHR(P)
            IF(ISPHR(IDEB+N)==1) THEN
              NN = NN + 1
              WAR2(1,IDEB+N)= BUFR(1,NN)
              WAR2(2,IDEB+N)= BUFR(2,NN)
              WAR2(3,IDEB+N)= BUFR(3,NN)
              WAR2(4,IDEB+N)= BUFR(4,NN)
              WAR2(5,IDEB+N)= BUFR(5,NN)
              WAR2(6,IDEB+N)= BUFR(6,NN)
              WAR2(7,IDEB+N)= BUFR(7,NN)
              WAR2(8,IDEB+N)= BUFR(8,NN)
              WAR2(9,IDEB+N)= BUFR(9,NN)
              XSPHR(12,IDEB+N) = BUFR(10,NN)
            END IF
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
     
 
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETVOIS_OFF           source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETVOIS_OFF(OFF_SPH, TAG_SPH,
     .                               KXSP, IXSP,ITAB)
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
      INTEGER 
     .        OFF_SPH(NUMSPH), TAG_SPH(NSPHR),
     .        TAG_SPHR(NSPHS), KXSP(NISP,*),
     .        IXSP(KVOISPH,*),ITAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), REQ_SD2(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),J,
     .        NVOISS1, NVOISS2,KNOD,K, JK,
     .        NVOIS1, NVOIS2, SM,MSGOFF,MSGOFF2
       INTEGER
     .        BUFS(NSPHS), BUFR(NSPHR)
       DATA MSGOFF/2018/
       DATA MSGOFF2/2019/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
c tratment of remote neighbours (locals done in sponof2)

      LOC_PROC = ISPMD+1
c
C TAG_SPH set to 1 in sponof2 if remote neighbour
C send TAG_SPH
      IDEB = 1
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      TAG_SPH(IDEB),PSPHR(P),MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Reception TAG_SPH
C
      IDEB = 1
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(TAG_SPHR(IDEB),PSPHS(P),MPI_INTEGER,
     .                 IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          IDEB = IDEB + PSPHS(P)
        END IF
      END DO
C
C send OFF_SPH
c OFF_SPH set to 1 in sponof2 if cell is deleted
      IDEB = 0
      DO P = 1, NSPMD
       IF(PSPHS(P)/=0)THEN
        NN = 0
        DO N = 1, PSPHS(P)
            NN = NN + 1
            ICELL = LSPHS(IDEB+N)
            BUFS(NN+IDEB)=OFF_SPH(ICELL)
        END DO

        MSGTYP = MSGOFF2
        CALL MPI_ISEND(
     S      BUFS(IDEB+1),NN,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD2(P),IERROR)
        IDEB = IDEB + PSPHS(P)
       END IF
      END DO
C
C Reception OFF_SPH
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF2
          CALL MPI_RECV(BUFR(IDEB+1),PSPHR(P),MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
           IDEB = IDEB + PSPHR(P)
        END IF
      END DO

      IDEB = 0
      DO P = 1, NSPMD
          DO N = 1, PSPHS(P)
            ICELL = LSPHS(IDEB+N)
! remote neighbour to treat
            IF(TAG_SPHR(IDEB+N)==1) THEN

              NVOIS1=0
              DO J=1,KXSP(4,ICELL)
                KNOD=IXSP(J,ICELL)
                IF(KNOD<0)THEN          
! remote cell, add only if non deleted cell
                  IF(BUFR(-KNOD)/=1)THEN
                    NVOIS1=NVOIS1+1
                    IXSP(NVOIS1,ICELL)=KNOD
                  ENDIF
                ELSE
! non remote case, add neighbour
                     NVOIS1=NVOIS1+1
                     IXSP(NVOIS1,ICELL)=KNOD 
                ENDIF
              ENDDO

              NVOIS2=NVOIS1
              DO K=KXSP(4,ICELL)+1,KXSP(5,ICELL)
                KNOD=IXSP(K,ICELL)
                IF(KNOD<0)THEN                               
                  IF(BUFR(-KNOD)/=1)THEN
                    NVOIS2=NVOIS2+1
                    IXSP(NVOIS2,ICELL)=KNOD
                  ENDIF
                ELSE
                   NVOIS2=NVOIS2+1
                   IXSP(NVOIS2,ICELL)=KNOD 
                ENDIF
              ENDDO

              NVOISS1=0
              DO K=KXSP(5,ICELL)+1,KXSP(5,ICELL)+KXSP(6,ICELL)
                JK  =IXSP(K,ICELL)
                IF(JK<0)THEN
                  SM=-JK/(NSPCOND+1)
                  IF(BUFR(SM)/=1)THEN
                    NVOISS1=NVOISS1+1
                    IXSP(NVOIS2+NVOISS1,ICELL)=JK
                  ENDIF
                ELSE
                  NVOISS1=NVOISS1+1
                  IXSP(NVOIS2+NVOISS1,ICELL)=JK
                ENDIF  
              ENDDO
              NVOISS2=NVOISS1       
              DO K=KXSP(5,ICELL)+KXSP(6,ICELL)+1,
     .                                KXSP(5,ICELL)+KXSP(7,ICELL)
                JK  =IXSP(K,ICELL)
                IF(JK<0)THEN
                  SM=-JK/(NSPCOND+1)
                  IF(BUFR(SM)/=1)THEN
                    NVOISS2=NVOISS2+1
                    IXSP(NVOIS2+NVOISS2,ICELL)=JK
                  ENDIF  
                ELSE
                  NVOISS2=NVOISS2+1
                  IXSP(NVOIS2+NVOISS2,ICELL)=JK
                ENDIF
              ENDDO 
                KXSP(4,ICELL)= NVOIS1
                KXSP(5,ICELL)= NVOIS2
                KXSP(6,ICELL)=NVOISS1
                KXSP(7,ICELL)=NVOISS2
            ENDIF
          END DO
          IDEB = IDEB + PSPHS(P)
      END DO

C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
        END IF
      END DO
 
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETIMP                source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETIMP(KXSP)
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
      INTEGER KXSP(NISP,*)

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), STATUS(MPI_STATUS_SIZE),MSGOFF
       INTEGER
     .        BUFS(NSPHS), BUFR(NSPHR)
       DATA MSGOFF/2020/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
c Get IMPOSE value KXSP(2,ICELL)/(NGROUP+1)

      LOC_PROC = ISPMD+1

      IDEB = 0
      DO P = 1, NSPMD
       IF(PSPHS(P)/=0)THEN
        DO N = 1, PSPHS(P)
          ICELL = LSPHS(IDEB+N)
           BUFS(N+IDEB) = KXSP(2,ICELL)/(NGROUP+1)
        END DO
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S      BUFS(IDEB+1),PSPHS(P)*1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD(P),IERROR)
        IDEB = IDEB + PSPHS(P)
       ENDIF
      END DO
C
C Reception
C
      IDEB = 0
      DO P = 1, NSPMD
        IF(PSPHR(P)/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(BUFR,1*PSPHR(P),MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          DO N = 1, PSPHR(P)
            XSPHR(12,IDEB+N) = BUFR(N)
          END DO
          IDEB = IDEB + PSPHR(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
        IF(PSPHS(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
        END IF
      END DO
C  
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHGETD                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|-- calls ---------------
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        WEIGHT1                       source/elements/sph/weight.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPHGETD(KXSP,IXSP,ISPHIO,X,WASPACT,NOD2SP,
     .                        SPBUF,V,A,ASPHR,DSPHR)
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
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        KXSP(NISP,*),
     .        ISPHIO(NISPHIO,*),
     .        IXSP(KVOISPH,*),NOD2SP(*),
     .        WASPACT(*)
       my_real
     .        X(3,*),SPBUF(NSPBUF,*),V(3,*) ,A(3,*),
     .        ASPHR(3,*),DSPHR(12,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, NN, N, IDEB, MSGTYP, LOC_PROC,
     .        IERROR, ICELL, INOD,
     .        REQ_SD(NSPMD), REQ_SD2(NSPMD),
     .        REQ_SD3(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)

      INTEGER, 
     .      DIMENSION(:,:), ALLOCATABLE :: TMP_IPPV
      INTEGER, 
     .      DIMENSION(:), ALLOCATABLE :: CPT_TMP,
     .                                   MYPSPHS,MYPSPHS2,MYPSPHR,
     .                                   REC_IPPV,SEND_IPPV,SEND_IPPV2

      INTEGER 
     .        II,IPT,JJ,NPF,IFVITS,
     .        NS,IACTIVE,
     .        IPPV,J,M,JNOD,IMPOSE,JMPOSE,
     .        NVOIS,IJ,NP,K,JMPOSE2,IPPVR,INDICE
     .        IDEB2, C, INDICE, IDEB2,N1, SIZ,INDICE1,
     .        NBIS,MSGOFF,MSGOFF2,MSGOFF3

       my_real
     .        BUFS(12,NSPHS), BUFR(12,NSPHR)
       my_real
     .       VX,VY,VZ,VN,VT,UX,UY,UZ,UN1,NX,NY,NZ,
     .       PS,
     .       XI,YI,ZI,XJ,YJ,ZJ,DMIN,DD,
     .       DI,RHOI,DJ,RHOJ,DIJ,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,
     .       VJ,VJX,VJY,VJZ,
     .       WGHT,WGRAD(3),WGRDX,WGRDY,WGRDZ,
     .       DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .       EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,
     .       ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,ALPHAI2,XP,YP,ZP  
       LOGICAL :: CONDITION
       DATA MSGOFF /2020/
       DATA MSGOFF2/2021/
       DATA MSGOFF3/2022/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
c get DX* and V* values, store in DSPHR
c use in sponfv

      ALLOCATE(TMP_IPPV(3,NSPHR),CPT_TMP(NSPMD))
      ALLOCATE(MYPSPHS(NSPMD+1),MYPSPHS2(NSPMD+1),MYPSPHR(NSPMD+1))
      ALLOCATE(SEND_IPPV(NSPHR),SEND_IPPV2(NSPHR),REC_IPPV(NSPHS))

      TMP_IPPV(1:3,1:NSPHR) = 0
      MYPSPHS(1:NSPMD+1)=0 
      MYPSPHS2(1:NSPMD+1)=0 
      MYPSPHR(1:NSPMD+1)=0 
      CPT_TMP(1:NSPMD)=0
      SEND_IPPV(1:NSPHR)=0
      SEND_IPPV2(1:NSPHR)=0
      REC_IPPV(1:NSPHS)=0

      LOC_PROC = ISPMD+1
      
c construction liste IPPV remotes
      IPPVR=0
      DO NS=1,NSPHACT
       N=WASPACT(NS)
       IMPOSE=KXSP(2,N)/(NGROUP+1)
       IF(IMPOSE/=0) THEN
         IF ( ISPHIO(1,IMPOSE)==2.OR.ISPHIO(1,IMPOSE)==3 )THEN
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
C-------
C         plus proche voisin en amont de l'outlet => IPPV.
          IPPV=0
          DMIN=1.E+20
          DO  K=1,KXSP(4,N)  
           JNOD=IXSP(K,N)
  
           IF(JNOD>0)THEN
             M   =NOD2SP(JNOD)
             JMPOSE=KXSP(2,M)/(NGROUP+1)
             CONDITION = .FALSE.
             CONDITION = JMPOSE==0
             IF(JMPOSE/=0) CONDITION = ISPHIO(1,JMPOSE)==1
             IF(CONDITION)THEN
               XJ  =X(1,JNOD)
               YJ  =X(2,JNOD)
               ZJ  =X(3,JNOD)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ELSE
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
               JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ENDIF
          ENDDO !enddo boucle voisin
          
          IF(IPPV<0)THEN
            !Indice IPPV negatif (global)
            IPPVR=IPPVR+1
            TMP_IPPV(1,IPPVR) = -IPPV
            ! recherche sur quel proc il est
            NBIS = 0
            DO P=1,NSPMD
             IF(P/=LOC_PROC) THEN
              N1 = NBIS
              NBIS = NBIS+PSPHR(P)             
               IF((-IPPV)<=NBIS)THEN
                TMP_IPPV(2,IPPVR)=P !proc sur lequel il se situe
                TMP_IPPV(3,IPPVR)=(-IPPV)-N1 !Indice IPPV (local)
                MYPSPHS(P)=MYPSPHS(P)+1
          GOTO 160
                ELSEIF(P==NSPMD)THEN
                   TMP_IPPV(2,IPPVR)=P !proc sur lequel il se situe
                   TMP_IPPV(3,IPPVR)=(-IPPV)-N1 !Indice IPPV (local)
                   MYPSPHS(P)=MYPSPHS(P)+1
          ENDIF
               ENDIF
             ENDDO ! ENDDO P=1,NSPMD
 160      CONTINUE       
          ENDIF
        ENDIF
       ENDIF
      ENDDO !ENDDO NS=1,NSPHACT

      MYPSPHS2(1)=1
      DO P=1,NSPMD
        MYPSPHS2(P+1)=MYPSPHS2(P)+MYPSPHS(P)
      ENDDO

       DO I=1,IPPVR
         P=TMP_IPPV(2,I)
         IF(P/=LOC_PROC)THEN
         CPT_TMP(P)=CPT_TMP(P)+1
         INDICE=MYPSPHS2(P)+CPT_TMP(P)-1
         SEND_IPPV(INDICE)= TMP_IPPV(3,I)
         SEND_IPPV2(INDICE)=TMP_IPPV(1,I)
         ENDIF
       ENDDO    

C
C Envoi liste IPPV < 0
C
c      IF(IPPVR>0)THEN
        DO P = 1, NSPMD
          IF(PSPHR(P)/=0)THEN
            MSGTYP = MSGOFF
            CALL MPI_ISEND(
     S        MYPSPHS(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_SD(P),IERROR)
          END IF
        END DO
C
C Reception flag cellules off
C
        DO P = 1, NSPMD
          IF(PSPHS(P)/=0)THEN
            MSGTYP = MSGOFF
            CALL MPI_RECV(MYPSPHR(P),1,MPI_INTEGER,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          END IF
        END DO

        DO P = 1, NSPMD
          IF(MYPSPHS(P)/=0)THEN
            MSGTYP = MSGOFF2
            IDEB = MYPSPHS2(P)
            CALL MPI_ISEND(
     S        SEND_IPPV(IDEB),MYPSPHS(P),MPI_INTEGER,
     .        IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_SD2(P),IERROR)
          END IF
        END DO
C Reception flag cellules off
C
        IDEB = 1
        DO P = 1, NSPMD
          IF(MYPSPHR(P)/=0)THEN
            MSGTYP = MSGOFF2
            CALL MPI_RECV(
     .        REC_IPPV(IDEB),MYPSPHR(P),MPI_INTEGER,
     .        IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
            IDEB = IDEB + MYPSPHR(P)
          END IF
        END DO

C Wait terminaison
C
        DO P = 1, NSPMD
          IF(PSPHR(P)/=0)THEN
            CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
          END IF
          IF(MYPSPHS(P)/=0)THEN
            CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
          END IF
        END DO
C
C Envoi  sur cellules 
C
        IDEB = 0
        IDEB2 = 0

      DO P = 1, NSPMD
c traitement
        IF(MYPSPHR(P)/=0)THEN
          DO N = 1, MYPSPHR(P)
            C = REC_IPPV(IDEB2+N)
            ICELL = LSPHS(C+IDEB)
            INOD = KXSP(3,ICELL)
            NP=ICELL
            XP=X(1,INOD)
            YP=X(2,INOD)
            ZP=X(3,INOD)      
            DI  =SPBUF(1,NP)
            RHOI=SPBUF(2,NP)
            CALL WEIGHT0(XP,YP,ZP,XP,YP,ZP,DI,WGHT)
            VJ=SPBUF(12,NP)/MAX(EM20,RHOI)
            ALPHAI=VJ*WGHT
            ALPHAXI=ZERO
            ALPHAYI=ZERO
            ALPHAZI=ZERO

            DO J=1,KXSP(4,NP)
              JNOD=IXSP(J,NP)
              IF(JNOD>0)THEN          ! particule locale
                M=NOD2SP(JNOD)
                JMPOSE=KXSP(2,M)/(NGROUP+1)
                CONDITION = .FALSE.
                CONDITION = JMPOSE==0
                IF(JMPOSE/=0) CONDITION = ISPHIO(1,JMPOSE)==1
                IF(CONDITION)THEN
                  DJ  =SPBUF(1,M)
                  XJ  =X(1,JNOD)
                  YJ  =X(2,JNOD)
                  ZJ  =X(3,JNOD)
                  DIJ =(DJ+DI)*HALF
                  RHOJ=SPBUF(2,M)
                  CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
                  VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
                  ALPHAI =ALPHAI +VJ*WGHT
                  ALPHAXI=ALPHAXI+VJ*WGRAD(1)
                  ALPHAYI=ALPHAYI+VJ*WGRAD(2)
                  ALPHAZI=ALPHAZI+VJ*WGRAD(3)
                ENDIF
              ELSE           ! particule remote
                NN = -JNOD
                JMPOSE = NINT(XSPHR(12,NN))
                 IF(JMPOSE>0)THEN
                   JMPOSE2=ISPHIO(1,JMPOSE)
                 ELSE
                   JMPOSE2=0
                ENDIF
                IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
                  DJ  =XSPHR(2,NN)
                  XJ  =XSPHR(3,NN)
                  YJ  =XSPHR(4,NN)
                  ZJ  =XSPHR(5,NN)
                  DIJ =(DJ+DI)*HALF
                  RHOJ=XSPHR(7,NN)
                  CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
                  VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
                  ALPHAI =ALPHAI +VJ*WGHT
                  ALPHAXI=ALPHAXI+VJ*WGRAD(1)
                  ALPHAYI=ALPHAYI+VJ*WGRAD(2)
                  ALPHAZI=ALPHAZI+VJ*WGRAD(3)
                ENDIF
              ENDIF
            ENDDO ! J=1,KXSP(4,NP)
C------
            ALPHAI =ONE/MAX(EM20,ALPHAI)
            ALPHAI2=ALPHAI*ALPHAI
            ALPHAXI=-ALPHAXI*ALPHAI2
            ALPHAYI=-ALPHAYI*ALPHAI2
            ALPHAZI=-ALPHAZI*ALPHAI2
C------
            VX =V(1,INOD)+DT12*A(1,INOD)       
            VY =V(2,INOD)+DT12*A(2,INOD)
            VZ =V(3,INOD)+DT12*A(3,INOD)

            DXX=ZERO
            DXY=ZERO
            DXZ=ZERO
            DYX=ZERO
            DYY=ZERO
            DYZ=ZERO
            DZX=ZERO
            DZY=ZERO
            DZZ=ZERO      

            DO J=1,KXSP(4,NP)
              JNOD=IXSP(J,NP)
              IF(JNOD>0)THEN
                M=NOD2SP(JNOD)
                JMPOSE=KXSP(2,M)/(NGROUP+1)
                CONDITION = .FALSE.
                CONDITION = JMPOSE==0
                IF(JMPOSE/=0) CONDITION = ISPHIO(1,JMPOSE)==1
                IF(CONDITION)THEN
                  DJ  =SPBUF(1,M)
                  XJ  =X(1,JNOD)
                  YJ  =X(2,JNOD)
                  ZJ  =X(3,JNOD)
                  DIJ =(DJ+DI)*HALF
                  RHOJ=SPBUF(2,M)
                  CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
                  WGRDX=WGRAD(1)*ALPHAI+WGHT*ALPHAXI
                  WGRDY=WGRAD(2)*ALPHAI+WGHT*ALPHAYI
                  WGRDZ=WGRAD(3)*ALPHAI+WGHT*ALPHAZI
                  VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
                  VXJ =V(1,JNOD)+DT12*A(1,JNOD)
                  VYJ =V(2,JNOD)+DT12*A(2,JNOD)
                  VZJ =V(3,JNOD)+DT12*A(3,JNOD)
                  VJX=VJ*(VXJ-VX)
                  VJY=VJ*(VYJ-VY)
                  VJZ=VJ*(VZJ-VZ)
                  DXX=DXX+VJX*WGRDX
                  DXY=DXY+VJX*WGRDY
                  DXZ=DXZ+VJX*WGRDZ
                  DYX=DYX+VJY*WGRDX
                  DYY=DYY+VJY*WGRDY
                  DYZ=DYZ+VJY*WGRDZ
                  DZX=DZX+VJZ*WGRDX
                  DZY=DZY+VJZ*WGRDY
                  DZZ=DZZ+VJZ*WGRDZ
                ENDIF
              ELSE
                NN=-JNOD
               JMPOSE = NINT(XSPHR(12,NN))
               IF(JMPOSE>0)THEN
                 JMPOSE2=ISPHIO(1,JMPOSE)
               ELSE
                 JMPOSE2=0
               ENDIF
               IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
                 DJ  =XSPHR(2,NN)
                 XJ  =XSPHR(3,NN)
                 YJ  =XSPHR(4,NN)
                 ZJ  =XSPHR(5,NN)
                 DIJ =(DJ+DI)*HALF
                 RHOJ=XSPHR(7,NN)
                 CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
                 WGRDX=WGRAD(1)*ALPHAI+WGHT*ALPHAXI
                 WGRDY=WGRAD(2)*ALPHAI+WGHT*ALPHAYI
                 WGRDZ=WGRAD(3)*ALPHAI+WGHT*ALPHAZI
                 VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
                 VXJ =XSPHR(9,NN)+DT12*ASPHR(1,NN)
                 VYJ =XSPHR(10,NN)+DT12*ASPHR(2,NN)
                 VZJ =XSPHR(11,NN)+DT12*ASPHR(3,NN)
                 VJX=VJ*(VXJ-VX)
                 VJY=VJ*(VYJ-VY)
                 VJZ=VJ*(VZJ-VZ)
                 DXX=DXX+VJX*WGRDX
                 DXY=DXY+VJX*WGRDY
                 DXZ=DXZ+VJX*WGRDZ
                 DYX=DYX+VJY*WGRDX
                 DYY=DYY+VJY*WGRDY
                 DYZ=DYZ+VJY*WGRDZ
                 DZX=DZX+VJZ*WGRDX
                 DZY=DZY+VJZ*WGRDY
                 DZZ=DZZ+VJZ*WGRDZ
               ENDIF
              ENDIF
            ENDDO ! ENDDO J=1,KXSP(4,NP)
            BUFS(1,N+IDEB2) = DXX
            BUFS(2,N+IDEB2) = DXY
            BUFS(3,N+IDEB2) = DXZ
            BUFS(4,N+IDEB2) = DYX
            BUFS(5,N+IDEB2) = DYY
            BUFS(6,N+IDEB2) = DYZ
            BUFS(7,N+IDEB2) = DZX
            BUFS(8,N+IDEB2) = DZY
            BUFS(9,N+IDEB2) = DZZ
            BUFS(10,N+IDEB2) = VX
            BUFS(11,N+IDEB2) = VY
            BUFS(12,N+IDEB2) = VZ
        END DO !ENDDO N = 1, MYPSPHR(P)
       
c envoi
        MSGTYP = MSGOFF3
        SIZ =  MYPSPHR(P)*12
        CALL MPI_ISEND(
     S      BUFS(1,IDEB2+1),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SD3(P),IERROR)
        IDEB2= IDEB2+MYPSPHR(P)
          ENDIF
          IDEB = IDEB + PSPHS(P)
      END DO !ENDDO P = 1, NSPMD
C
C Reception 
C      
      IDEB = 0

      DO P = 1, NSPMD
          IF(MYPSPHS(P)/=0)THEN
          MSGTYP = MSGOFF3
          SIZ = 12*MYPSPHS(P)
          CALL MPI_RECV(BUFR,SIZ,REAL,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
          DO N = 1, MYPSPHS(P)
            INDICE1 = SEND_IPPV2(IDEB+N)
            DSPHR(1,INDICE1) = BUFR(1,N)
            DSPHR(2,INDICE1) = BUFR(2,N)
            DSPHR(3,INDICE1) = BUFR(3,N)
            DSPHR(4,INDICE1) = BUFR(4,N)
            DSPHR(5,INDICE1) = BUFR(5,N)
            DSPHR(6,INDICE1) = BUFR(6,N)
            DSPHR(7,INDICE1) = BUFR(7,N)
            DSPHR(8,INDICE1) = BUFR(8,N)
            DSPHR(9,INDICE1) = BUFR(9,N)
            DSPHR(10,INDICE1) = BUFR(10,N)
            DSPHR(11,INDICE1) = BUFR(11,N)
            DSPHR(12,INDICE1) = BUFR(12,N)
          ENDDO
          IDEB = IDEB + MYPSPHS(P)
        END IF
      END DO
C
C Wait terminaison
C
      DO P = 1, NSPMD
         IF(MYPSPHR(P)/=0)THEN
          CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
        END IF
      END DO
C  
c      ENDIF !ENDIF IPPVR > 0
      
      DEALLOCATE(TMP_IPPV,MYPSPHS,MYPSPHS2,MYPSPHR)
      DEALLOCATE(SEND_IPPV,SEND_IPPV2,REC_IPPV,CPT_TMP)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPHVOX0                  source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_SPHVOX0(KXSP ,SPBUF,WSP2SORT,BMINMAL,X,
     2                    NSP2SORTF,NSP2SORTL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), WSP2SORT(*),
     2                    NSP2SORTF,NSP2SORTL
      my_real 
     .        X(3,*),BMINMAL(*), SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ,J,NN
      my_real
     .        RATIO, AAA,ALPHA_MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      ALPHA_MARGE = SQRT(ONE +SPASORT)
      LOC_PROC = ISPMD + 1
c      MARGE = TZINF-GAP

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      DO NE=NSP2SORTF,NSP2SORTL

         J=WSP2SORT(NE)
         NN=KXSP(3,J)

c a revoir !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         AAA = SPBUF(1,J)* ALPHA_MARGE

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(X(1,NN)-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(X(2,NN)-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(X(3,NN)-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=INT(NBX*(X(1,NN)+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(X(2,NN)+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(X(3,NN)+AAA-ZMINB)/(ZMAXB-ZMINB))

#include "lockon.inc"
         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             DO IX = IX1, IX2
               CRVOXEL(IY,IZ,LOC_PROC)=IBSET(CRVOXEL(IY,IZ,LOC_PROC),IX)
             END DO
           END DO
         END DO
#include "lockoff.inc"

      ENDDO

C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SPH_GAUGE                source/mpi/elements/spmd_sph.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SPH_GAUGE(LGAUGE,GAUGE)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LGAUGE(3,*)
      my_real 
     .   GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,IERROR,LOC_PROC,NN,L,I,K,N,II,J,
     .        IDEB,SIZ,A_AR,NBIRECV,INDEX,
     .        IRINDEX(NSPMD),REQ_R(NSPMD),IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        BUF(LLGAUGE+1,NBGAUGE*NSPMD) 
      DATA MSGOFF/2027/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IF (LOC_PROC==1) THEN
        IDEB = 1
        NBIRECV = 0
        DO I = 2, NSPMD
            IAD_RECV(I) = IDEB
            NBIRECV = NBIRECV + 1
            IRINDEX(NBIRECV) = I
            NN = NBGAUGE
            SIZ = (LLGAUGE+1)*NN
            MSGTYP = MSGOFF
            CALL MPI_IRECV(
     S        BUF(1,IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
              IDEB = IDEB + NN
        END DO
C
        DO II = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
          I = IRINDEX(INDEX)
          L = IAD_RECV(I)
          NN = NBGAUGE
          DO N = L, L+NN-1
            K = NINT(BUF(1,N))
            IF(LGAUGE(1,K) < -NUMELS)THEN
              DO J = 10, 13
                GAUGE(J,K) = GAUGE(J,K)+BUF(J+1,N)
              END DO
            ENDIF
          END DO
        END DO
C
      ELSE
        K = 0
        DO N = 1, NBGAUGE
           K = K + 1
           BUF(1,K) = N
           DO J = 1, LLGAUGE
             BUF(J+1,K) = GAUGE(J,N)
           END DO
        END DO
        SIZ = (LLGAUGE+1)*K
        MSGTYP=MSGOFF
        CALL MPI_SEND(
     S     BUF,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
