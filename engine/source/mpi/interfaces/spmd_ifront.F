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
Chd|  SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IFRONT_MOD                    share/modules/ifront_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_IFRONT(
     1   IPARI   ,NEWFRONT,ISENDTO   ,IRCVFROM,
     2   NSENSOR ,NBINTC  ,INTLIST ,ISLEN7    ,IRLEN7  ,
     3   ISLEN11 ,IRLEN11 ,ISLEN17   ,IRLEN17 ,IRLEN7T ,
     4   ISLEN7T ,IRLEN20 ,ISLEN20   ,IRLEN20T,ISLEN20T,
     5   IRLEN20E,ISLEN20E,SENSOR_TAB,INTBUF_TAB, MODE)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE IFRONT_MOD
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE SENSOR_MOD
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBINTC,ISLEN7,IRLEN7,ISLEN11,IRLEN11,ISLEN17,IRLEN17,
     .        IRLEN7T,ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .        IRLEN20E,ISLEN20E, NSENSOR, 
     .        IPARI(NPARI,NINTER),
     .        NEWFRONT(*), INTLIST(*),
     .        ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*)
      INTEGER MODE
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NSEG, LEN, NI, ITYP, OLD_LEN,
     .        N, P, I, J, K, L, PP, NIN ,IDEB, IDEB2, IDEB3, II,
     .        LENOUT, I0, NS, INTTH,
     .        NOINT, MULTIMP, ITY, I_STOK_G, ISTK,
     .        SIZE, ALEN, LOC_PROC, MSGTYP,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4,
     .        IERROR, IERROR2, IDEBUT(NSPMD+NINTER),
     .        STATUS(MPI_STATUS_SIZE),REQ_S(NSPMD),
     .        ISUBTMP(NINTER,2,NSPMD),ISUBTMP2(NINTER,2,NSPMD),
     .        IDEBUT2(NINTER), ISENS,INTERACT,
     .        IEDGE
      INTEGER :: SIZ,IDEB_EDGE,NB_SUBINT
      INTEGER :: INDEX_PROC
      LOGICAL :: ONLY_INTER_7
      DATA MSGOFF/1009/
      DATA MSGOFF2/1010/
      DATA MSGOFF3/1011/
      DATA MSGOFF4/1012/

C     REAL
      my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,TZINF,DIST0,
     .   XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN,TS


C-----------------------------------------------
      IF(NSPMD==1) RETURN
      LOC_PROC = ISPMD+1


c     NBINTC = 0
c     DO NIN = 1,NINTER
c       IF(IPARI(7,NIN) == 25) THEN
c         NBINTC = NBINTC + 1
c         INTLIST(NBINTC) = NIN
c       ENDIF 
c     ENDDO
c     DO II = 1, NBINTC
c       NIN = INTLIST(II)
c       ITY=IPARI(7,NIN)
c       IF(ITY /= 25) THEN
c         NBINTC = NBINTC + 1
c         INTLIST(NBINTC) = NIN
c       ENDIF
c     ENDDO 
C Attention aux interfaces inactives

C ===========================================================
      IF(MODE == 1) THEN

      ICOMM = 0
C

      ! Allocation done once for all
      IF(.NOT. ALLOCATED(PROC_LIST)) THEN
        ALLOCATE(PROC_LIST(NSPMD) )
        ALLOCATE(ICOMM2(NSPMD), REQ_SEND_SIZ(NSPMD), REQ_SEND_MSG(NSPMD))
        ALLOCATE(REQ_RECV_SIZ(NSPMD), REQ_RECV_MSG(NSPMD))
        ALLOCATE(IRCOM(NSPMD), ISCOM(NSPMD), ISCOMS(NSPMD))
        ALLOCATE(SIZBUF_R(NSPMD))
        ALLOCATE(SIZBUF_S(NSPMD))
        ALLOCATE(MSGBUF_R(NSPMD))
        ALLOCATE(MSGBUF_S(NSPMD))
        ALLOCATE(ICOMM2_SEND(NSPMD),ICOMM2_RCV(NSPMD))
c       ALLOCATE(MSGBUF_S_LEN(NSPMD))
c       MSGBUF_S_LEN(1:NSPMD) = 0
c       ALLOCATE(MSGBUF_R_LEN(NSPMD))
c       MSGBUF_R_LEN(1:NSPMD) = 0
        DO P=1,NSPMD
          ALLOCATE(SIZBUF_R(P)%P(2*NBINTC))
          ALLOCATE(SIZBUF_S(P)%P(2*NBINTC))
          SIZBUF_R(P)%P(1:2*NBINTC) = 0
          SIZBUF_S(P)%P(1:2*NBINTC) = 0
        ENDDO
      ENDIF

      !Initialization
      REQ_RECV_SIZ(1:NSPMD) = MPI_REQUEST_NULL 
      REQ_RECV_MSG(1:NSPMD) = MPI_REQUEST_NULL 
      PROC_LIST(1:NSPMD) = 0
      ISCOM(1:NSPMD) = 0
      ISCOMS(1:NSPMD) = 0


C       
      NISUBG = 0
      L = 0
      SIZE = 4+2*NSPMD
      DO II = 1, NBINTC
       NIN = INTLIST(II)
       ITY=IPARI(7,NIN)
       IF(ITY==7.OR.ITY==10.OR.
     .    ITY==22.OR.ITY==23.OR.ITY==24.OR.
     .    ITY==20.OR.ITY==11.OR.ITY==17.OR.
     .    ITY==25) THEN
C Attention aux interfaces inactives
C
        INTERACT = 0
        ISENS = 0
        IF(ITY == 7.OR.ITY == 11.OR.ITY == 24.OR.ITY == 25) THEN
           ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined
        ENDIF 
        IF (ISENS > 0) THEN             ! Sensor ID  
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
        ELSE
           STARTT= INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT = INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
        ENDIF
C
        DIST = INTBUF_TAB(NIN)%VARIABLES(5)
C
c       IF(ITY == 25) THEN
c         WRITE(6,*) "DIST=",DIST
c         WRITE(6,*) "INTERACT=",INTERACT
c       ENDIF

        IF (ITY == 25 .OR. (DIST<=ZERO.AND.INTERACT/=0))THEN  
          IF(ISENDTO(NIN,LOC_PROC)/=0.OR.
     .       IRCVFROM(NIN,LOC_PROC)/=0) THEN

            NEWFRONT(NIN) = 2
c           IF(ITY == 25) THEN
c             WRITE(6,*) "NEWFRONT",NIN,"=",2
c           ENDIF

            ICOMM = 1
            INTBUF_TAB(NIN)%VARIABLES(5) = -DIST
c           RBUFS(L+1)= NIN
c           RBUFS(L+2)= INTBUF_TAB(NIN)%VARIABLES(8)
c           RBUFS(L+3)= INTBUF_TAB(NIN)%VARIABLES(9)
c           RBUFS(L+4)= INTBUF_TAB(NIN)%VARIABLES(12)

            DO P = 1, NSPMD
              LEN = NSNFI(NIN)%P(P)
C              IF(IPARI(7,NIN)==20) LEN = LEN + NSNFIE(NIN)%P(P)
              SIZBUF_S(P)%P(II)= LEN
              ISCOM(P) = ISCOM(P) + LEN
            ENDDO
            IF (IPARI(36,NIN)>0.AND.IPARI(7,NIN)/=17) THEN
               NISUBG = MAX(NISUBG,IPARI(36,NIN))
               DO P=1,NSPMD
                 ISCOMS(P) = ISCOMS(P) + NSNFI(NIN)%P(P)
                 IF(IPARI(7,NIN)==25.AND. IPARI(58,NIN) > 0) THEN
                   ISCOMS(P) = ISCOMS(P) + NSNFIE(NIN)%P(P)
                 ENDIF
               ENDDO
            END IF
C
C Rajout partie Edge pour type 20
C
            ITY=IPARI(7,NIN)
            IF (ITY == 20 .OR. (ITY == 25.AND. IPARI(58,NIN) > 0)) THEN
              DO P = 1, NSPMD
                LEN = NSNFIE(NIN)%P(P)
                SIZBUF_S(P)%P(II+NBINTC)= LEN
                ISCOM(P) = ISCOM(P) + LEN
              END DO
            ELSE
              DO P = 1, NSPMD
                SIZBUF_S(P)%P(II+NBINTC)= 0
              END DO              
            END IF
C
            L = L + SIZE
          ENDIF
        ENDIF
       ENDIF
      ENDDO
C
C Verification de la fin du tri (i7buce)
C
      DO P = 1, NSPMD
       ICOMM2(P) = 0
       ICOMM2_SEND(P) = 0
       ICOMM2_RCV(P) = 0
       ONLY_INTER_7 = .TRUE.
       IF (P/=LOC_PROC) THEN
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          ITY=IPARI(7,NIN)
          IF(NEWFRONT(NIN)==2) THEN
            IF(ISENDTO(NIN,P)/=0.OR.IRCVFROM(NIN,P)/=0) THEN
              ICOMM2(P) = 1
              IF(ITY/=7.AND.ITY/=11) ONLY_INTER_7 = .FALSE.
            ENDIF
            IF(ISENDTO(NIN,P)/=0.AND.IRCVFROM(NIN,LOC_PROC)/=0) ICOMM2_SEND(P) = 1    !   nsn > 0 on p & nmn > 0 on ispmd
            IF(IRCVFROM(NIN,P)/=0.AND.ISENDTO(NIN,LOC_PROC)/=0) ICOMM2_RCV(P) = 1    !   nmn > 0 on p & nsn > 0 on ispmd
          ENDIF
        ENDDO
        IF(.NOT.ONLY_INTER_7) THEN
            ICOMM2_RCV(P) = ICOMM2(P)  
            ICOMM2_SEND(P) = ICOMM2(P)
        ENDIF
       END IF
        IF (ICOMM2_SEND(P)==1)THEN
          MSGTYP = MSGOFF
          L  = 2*NBINTC
          CALL MPI_ISEND(
     S      SIZBUF_S(P)%P(1),L,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SEND_SIZ(P),IERROR)
        ENDIF
      ENDDO


      NB_TO_RECV = 0
      DO P = 1, NSPMD
        IRCOM(P) = 0
        IF(ICOMM2_RCV(P)==1)THEN
          MSGTYP = MSGOFF
          NB_TO_RECV = NB_TO_RECV + 1
          PROC_LIST(NB_TO_RECV) = P
          L = 2 * NBINTC
          CALL MPI_IRECV(SIZBUF_R(P)%P(1),L,
     .                  MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,REQ_RECV_SIZ(NB_TO_RECV),IERROR)

        ENDIF
      ENDDO
C=======================================================================
C ENVOI
      IF(ICOMM /= 0) THEN
      DO II = 1, NBINTC
        I = INTLIST(II)
        IDEBUT(I) = 0
        IDEBUT2(I) = 0
      ENDDO
      DO P = 1, NSPMD
        LEN = ISCOM(P)
        IF(LEN/=0) THEN
C alloc structure de comm
          ALLOCATE(MSGBUF_S(P)%P(LEN),STAT=IERROR)

          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 0
          DO II = 1, NBINTC
            NIN = INTLIST(II)
C interface retriee ?
            IF(NEWFRONT(NIN)==2) THEN
              IF(NSNFI(NIN)%P(P)>0) THEN
                IDEB2 = IDEBUT(NIN)
                LEN = NSNFI(NIN)%P(P)
                DO I = 1, LEN
                  MSGBUF_S(P)%P(IDEB+I) = NSVFI(NIN)%P(IDEB2+I)
                ENDDO
                IDEBUT(NIN) = IDEBUT(NIN) + LEN
                IDEB = IDEB + LEN
              ENDIF
              IF(IPARI(7,NIN) == 20 .OR. (IPARI(7,NIN) == 25.AND. IPARI(58,NIN) > 0))THEN
                IF(NSNFIE(NIN)%P(P)>0) THEN
                  IDEB2 = IDEBUT2(NIN)
                  LEN = NSNFIE(NIN)%P(P)
C                 WRITE(6,*) __FILE__,ISPMD,"RECV",P-1,LEN
                  DO I = 1, LEN
                    ASSERT(NSVFIE(NIN)%P(IDEB2+I) > 0)
                    MSGBUF_S(P)%P(IDEB+I) = ABS(NSVFIE(NIN)%P(IDEB2+I))
                  ENDDO
                  IDEBUT2(NIN) = IDEBUT2(NIN) + LEN
                  IDEB = IDEB + LEN
                ENDIF
              END IF
            ENDIF
          ENDDO
          MSGTYP = MSGOFF2
          CALL MPI_ISEND(
     S        MSGBUF_S(P)%P(1),IDEB,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_SEND_MSG(P),IERROR)
        ENDIF   
      ENDDO  
      ENDIF ! ICOMM /= 0)

      ELSEIF( MODE == 2 ) THEN
C =========================================
c   _       ,/'
c  (_).  ,/'
c  __  ::     -  -  -  -  -  -  -  -  -  -  -  -
c (__)'  `\.
c           `\.
C ==========================================




C ==========================================================================
C RECEPTION OF sizes
!      CALL MPI_WAITALL(NB_TO_RECV,REQ_RECV_SIZ(1:NB_TO_RECV),MPI_STATUSES_IGNORE,IERROR)

      DO I = 1, NB_TO_RECV
        CALL MPI_WAITANY(NB_TO_RECV,REQ_RECV_SIZ,INDEX_PROC,STATUS,IERROR)
        P = PROC_LIST(INDEX_PROC)
!        P = PROC_LIST(I)

        IRCOM(P) = 0
        DO II = 1, NBINTC
           NIN = INTLIST(II)
           ITY=IPARI(7,NIN)
           IF(NEWFRONT(NIN) == 2) THEN
             IF(ISENDTO(NIN,LOC_PROC)/=0.OR.
     .          IRCVFROM(NIN,LOC_PROC)/=0) THEN
               LEN = SIZBUF_R(P)%P(II)
               NSNSI(NIN)%P(P) = LEN
               IRCOM(P) = IRCOM(P) + LEN
               IF(ITY == 20 .OR. (ITY == 25.AND. IPARI(58,NIN) > 0))THEN
                 LEN = SIZBUF_R(P)%P(II+NBINTC)
                 NSNSIE(NIN)%P(P) = LEN
                 IRCOM(P) = IRCOM(P) + LEN
               END IF
             ENDIF
           ENDIF
        ENDDO ! NBINTC
        LEN = IRCOM(P)
        IF(LEN>0) THEN
          ALLOCATE(MSGBUF_R(P)%P(LEN),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          MSGTYP = MSGOFF2
          CALL MPI_IRECV(MSGBUF_R(P)%P(1),LEN,MPI_INTEGER,IT_SPMD(P),
     .                MSGTYP,MPI_COMM_WORLD,REQ_RECV_MSG(I),IERROR)

        ENDIF
      ENDDO






C RECEPTION of messages
      DO P = 1, NB_TO_RECV
        CALL MPI_WAIT(REQ_RECV_MSG(P),STATUS,IERROR)
      ENDDO
      DO P = 1, NSPMD
        IF (ICOMM2_SEND(P)==1) THEN
          CALL MPI_WAIT(REQ_SEND_SIZ(P),STATUS,IERROR)
        ENDIF
      ENDDO

      IF(ICOMM==0) RETURN

      DO P = 1, NSPMD
        IDEBUT(P) = 0
      ENDDO
C MAJ STRUCTURES D ECHANGES
      DO II = 1, NBINTC
        NIN = INTLIST(II)
C interface retriee ?
        IF(NEWFRONT(NIN)==2) THEN
          IDEB = 0
          IF(ASSOCIATED(NSVSI(NIN)%P))DEALLOCATE(NSVSI(NIN)%P)
          LEN = 0
          DO P = 1, NSPMD
            LEN = LEN + NSNSI(NIN)%P(P)
          ENDDO
          IERROR = 0
          IF(LEN>0)ALLOCATE(NSVSI(NIN)%P(LEN),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          DO P = 1, NSPMD
            LEN = NSNSI(NIN)%P(P)
C test si proc a envoye qqchose
            IF(LEN>0) THEN
              IDEB2 = IDEBUT(P)
              DO I = 1, LEN
                NSVSI(NIN)%P(IDEB+I) = MSGBUF_R(P)%P(IDEB2+I)
              ENDDO
              IDEB = IDEB + LEN
              IDEBUT(P) = IDEBUT(P) + LEN
            ENDIF
          ENDDO
C
          IF(IPARI(7,NIN) == 20 .OR. (IPARI(7,NIN) == 25.AND. IPARI(58,NIN) > 0) )THEN
            IDEB = 0
            IF(ASSOCIATED(NSVSIE(NIN)%P))DEALLOCATE(NSVSIE(NIN)%P)
            LEN = 0
            DO P = 1, NSPMD
              LEN = LEN + NSNSIE(NIN)%P(P)
            ENDDO
            IERROR = 0
            IF(LEN>0)ALLOCATE(NSVSIE(NIN)%P(LEN),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            DO P = 1, NSPMD
              LEN = NSNSIE(NIN)%P(P)
C test si proc a envoye qqchose
              IF(LEN>0) THEN
C               WRITE(6,*) __FILE__,ISPMD,"SEND",P-1,LEN
                IDEB2 = IDEBUT(P)
                DO I = 1, LEN
                  ASSERT(MSGBUF_R(P)%P(IDEB2+I) > 0) 
                  NSVSIE(NIN)%P(IDEB+I) = ABS(MSGBUF_R(P)%P(IDEB2+I))
                ENDDO
                IDEB = IDEB + LEN
                IDEBUT(P) = IDEBUT(P) + LEN
              ENDIF
            ENDDO
          END IF
        ENDIF 
      ENDDO  
C Attente reception de msg
      DO P = 1, NSPMD
        IF(IRCOM(P)>0) THEN
          DEALLOCATE(MSGBUF_R(P)%P)
        ENDIF
        IF(ISCOM(P)>0) THEN
          CALL MPI_WAIT(REQ_SEND_MSG(P),STATUS,IERROR)
          DEALLOCATE(MSGBUF_S(P)%P)
        ENDIF   
      ENDDO

C
      IF(NISUBG>0) THEN
C interface avec output sous interfaces
C
C Calcul et envoi de la taille des parties sous interfaces sur partie frontiere
C
        DO P = 1, NSPMD
          IF(IRCOM(P)>0) THEN
            DO II = 1, NBINTC
              I = INTLIST(II)
              ISUBTMP(I,1,P) = 0
              ISUBTMP(I,2,P) = 0
            END DO
          END IF
        END DO
        DO II = 1, NBINTC
          NIN = INTLIST(II)
C interface retriee avec sous interfaces ?
          IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +       IPARI(7,NIN)/=17) THEN
            IDEB = 0
            DO P = 1, NSPMD
              LEN = NSNSI(NIN)%P(P)
              LENOUT = 0
              IF(LEN>0) THEN
                DO I = 1, LEN
                  NS = NSVSI(NIN)%P(IDEB+I)
C rajout +1 pour envoyer le nb de sous interface par noeud
                  LENOUT = LENOUT + INTBUF_TAB(NIN)%ADDSUBS(NS+1)-
     .                     INTBUF_TAB(NIN)%ADDSUBS(NS) + 1
                END DO
                IDEB = IDEB + LEN
              END IF          
              ISUBTMP(NIN,1,P) = LENOUT
            ENDDO
            IF(IPARI(7,NIN) ==25 .AND. IPARI(58,NIN) > 0) THEN
              IDEB = 0
              DO P=1,NSPMD
C              Partie Eedge
                LEN = NSNSIE(NIN)%P(P)
                LENOUT = 0
                IF(LEN>0) THEN
                  DO I = 1, LEN
                    NS = NSVSIE(NIN)%P(IDEB+I)
C rajout +1 pour envoyer le nb de sous interface par noeud
                    LENOUT = LENOUT + INTBUF_TAB(NIN)%ADDSUBE(NS+1)-
     .                       INTBUF_TAB(NIN)%ADDSUBE(NS) + 1
C                   WRITE(6,*) "Node",I,INTBUF_TAB(NIN)%ADDSUBE(NS),
C    .                         INTBUF_TAB(NIN)%ADDSUBE(NS+1)-1
                  END DO
                  IDEB = IDEB + LEN
                END IF          
C               WRITE(6,*) "ISUBTMP(",P-1,")=",LENOUT
                ISUBTMP(NIN,2,P) = LENOUT
              END DO ! NSPMD
            ENDIF ! type 25 E2E
          END IF ! NEWFRONT
        END DO ! NBINTC
C
        DO P = 1, NSPMD
          IF(IRCOM(P)>0) THEN
            LENOUT = 0
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              LENOUT = LENOUT + ISUBTMP(NIN,1,P)
              IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +           (IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25)) THEN
                 LENOUT = LENOUT + ISUBTMP(NIN,1,P) - NSNSI(NIN)%P(P)
              ENDIF
              IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +           IPARI(7,NIN)==25) THEN
                 IF(IPARI(58,NIN) /= 0) THEN 
                  LENOUT = LENOUT + 2*ISUBTMP(NIN,2,P) - NSNSIE(NIN)%P(P)
                 ENDIF
              ENDIF
            END DO
C longueur comm sauvegardee ds ircom
            IRCOM(P) = LENOUT           
            IF(LENOUT>0) THEN
C alloc structure de comm
              ALLOCATE(MSGBUF_S(P)%P(LENOUT),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
              MSGTYP = MSGOFF3
              SIZ = NINTER * 2
              CALL MPI_ISEND(
     S          ISUBTMP(1,1,P),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G          MPI_COMM_WORLD,REQ_S(P),IERROR)
            END IF
          END IF
        END DO
C
C Reception de la taille des parties sous interfaces sur partie frontiere
C
        DO P = 1, NSPMD
          IF(ISCOMS(P)>0) THEN
            MSGTYP = MSGOFF3
            LENOUT = 0
            SIZ = NINTER * 2
C on recoit dans la partie de buffer isubtmp non utilisee
            CALL MPI_RECV(ISUBTMP2(1,1,P),SIZ,MPI_INTEGER,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
            DO II = 1, NBINTC
              NIN = INTLIST(II)
C interface retriee avec sous interfaces ?
              IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +           IPARI(7,NIN)/=17) THEN
C on retranche le nbre de noeuds pour retrouver la longeur des sous interfaces
                NB_SUBINT = ISUBTMP2(NIN,1,P) - NSNFI(NIN)%P(P)
                NISUBSFI(NIN)%P(P) = NB_SUBINT 
                LENOUT = LENOUT + ISUBTMP2(NIN,1,P)
                IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                  LENOUT = LENOUT + NB_SUBINT
                ENDIF
                IF(IPARI(7,NIN)==25) THEN
                  IF(IPARI(58,NIN) /= 0) THEN ! edge to edge
                    NISUBSFIE(NIN)%P(P) = ISUBTMP2(NIN,2,P) - NSNFIE(NIN)%P(P)
C                   WRITE(6,*) "NISUBSFIE(NIN)%P(",P-1,")=",NISUBSFIE(NIN)%P(P),ISUBTMP2(NIN,2,P)
                    LENOUT = LENOUT + 2*ISUBTMP2(NIN,2,P) - NSNFIE(NIN)%P(P)
! Buffer is [ Size , I_1, I_2, ...., wize]
                  ENDIF
                ENDIF
              END IF
            END DO
C longueur comm sauvegardee ds iscom
            ISCOM(P) = LENOUT
            IF(LENOUT>0) THEN          
              ALLOCATE(MSGBUF_R(P)%P(LENOUT),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
            END IF
          ELSE
             ISCOM(P) = 0
          END IF
        END DO
C
        DO P = 1, NSPMD
          IF(IRCOM(P)>0) THEN
            CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
          END IF
        END DO
C
C Envoi des parties sous interfaces sur partie frontiere
C
        DO P = 1, NSPMD
          IDEBUT(P) = 0
        END DO
        DO II = 1, NBINTC
          NIN = INTLIST(II)
C interface retriee avec sous interfaces ?
          IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +       IPARI(7,NIN)/=17) THEN
            IDEB = 0
            DO P = 1, NSPMD
              LEN = NSNSI(NIN)%P(P)
              IF(LEN>0) THEN
                I0 = IDEBUT(P)
                DO I = 1, LEN
                  NS = NSVSI(NIN)%P(IDEB+I)
                  I0 = I0 + 1
C recupere le nbre de sous interface pour le noeud
                  MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%ADDSUBS(NS+1)-
     .                             INTBUF_TAB(NIN)%ADDSUBS(NS)

                  DO J = INTBUF_TAB(NIN)%ADDSUBS(NS),
     .              INTBUF_TAB(NIN)%ADDSUBS(NS+1)-1
                    I0 = I0 + 1
                    MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%LISUBS(J)
                  END DO
                  IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                    DO J = INTBUF_TAB(NIN)%ADDSUBS(NS),
     .                INTBUF_TAB(NIN)%ADDSUBS(NS+1)-1
                      I0 = I0 + 1
                      MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%INFLG_SUBS(J)
                    END DO
                  END IF
                END DO
                IDEBUT(P) = I0
                IDEB = IDEB + LEN
              END IF          
            END DO
            IF(IPARI(7,NIN) == 25 .AND. IPARI(58,NIN) /= 0)THEN 
              IDEB_EDGE = 0
              DO P = 1,NSPMD
C                Sub interface for Edges
                LEN = NSNSIE(NIN)%P(P)
                IF(LEN>0) THEN
                  I0 = IDEBUT(P)
                  DO I = 1, LEN
                    NS = NSVSIE(NIN)%P(IDEB_EDGE+I)
                    I0 = I0 + 1
C recupere le   nbre de sous interface pour le noeud
                    MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%ADDSUBE(NS+1)-
     .                               INTBUF_TAB(NIN)%ADDSUBE(NS)
                
C                   WRITE(6,*) P-1,"MSGBUF_S(",I0,")=",LEN

                    DO J = INTBUF_TAB(NIN)%ADDSUBE(NS),
     .                INTBUF_TAB(NIN)%ADDSUBE(NS+1)-1
                      I0 = I0 + 1
                      MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%LISUBE(J)
C                     WRITE(6,*) P-1,"LISUBE---MSGBUF_S(",I0,")=",MSGBUF_S(P)%P(I0)
                    END DO
                    DO J = INTBUF_TAB(NIN)%ADDSUBE(NS),
     .                INTBUF_TAB(NIN)%ADDSUBE(NS+1)-1
                      I0 = I0 + 1
                      MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%INFLG_SUBE(J)
C                     WRITE(6,*) P-1,"INFLG---MSGBUF_S(",I0,")=",MSGBUF_S(P)%P(I0)
                    END DO
                  END DO
                  IDEBUT(P) = I0
                  IDEB_EDGE = IDEB_EDGE + LEN
                END IF ! LEN 
              END DO
            ENDIF ! IEDGE
          END IF
        END DO
C
        DO P = 1, NSPMD
C longueur comm sauvegardee ds ircom
          IF(IRCOM(P)>0) THEN
            MSGTYP = MSGOFF4        
C           WRITE(6,*) "SEND",IRCOM(P) ,"TO",P-1
            CALL MPI_ISEND(
     S         MSGBUF_S(P)%P(1),IRCOM(P),MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G         MPI_COMM_WORLD,REQ_S(P),IERROR)
          END IF
        END DO  
C
C Reception des parties sous interfaces sur partie frontiere
C
        DO P = 1, NSPMD
C longueur comm sauvegardee ds iscom
          IF(ISCOM(P)>0) THEN
            MSGTYP = MSGOFF4
C           WRITE(6,*) "RECV",ISCOM(P) ,"FROM",P-1

            CALL MPI_RECV(MSGBUF_R(P)%P(1),ISCOM(P),MPI_INTEGER,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
C            DO II = 1,ISCOM(P)
C               WRITE(6,*) "MSGBUF_R(",P-1,")%P(",II,")=",MSGBUF_R(P)%P(II)
C            ENDDO
          END IF
        END DO
C
C Maj structures sous interfaces
C
        DO P = 1, NSPMD
          IDEBUT(P) = 0
        END DO
        DO II = 1, NBINTC
          NIN = INTLIST(II)
C interface retriee avec sous interfaces ?
          IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.
     +       IPARI(7,NIN)/=17) THEN
            IF(ASSOCIATED(LISUBSFI(NIN)%P))DEALLOCATE(LISUBSFI(NIN)%P)
            LEN = 0
            DO P = 1, NSPMD
              LEN = LEN + NISUBSFI(NIN)%P(P)
            END DO
            IERROR = 0
            IF(LEN>0) THEN
              ALLOCATE(LISUBSFI(NIN)%P(LEN),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
              IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                IF(ASSOCIATED(INFLG_SUBSFI(NIN)%P))DEALLOCATE(INFLG_SUBSFI(NIN)%P)
                ALLOCATE(INFLG_SUBSFI(NIN)%P(LEN),STAT=IERROR)
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                END IF
              END IF
              LEN = 1
              IF(ASSOCIATED(ADDSUBSFI(NIN)%P))
     .          DEALLOCATE(ADDSUBSFI(NIN)%P)
              DO P = 1, NSPMD
                LEN = LEN + NSNFI(NIN)%P(P)
              END DO
              ALLOCATE(ADDSUBSFI(NIN)%P(LEN),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
              IDEB  = 0
              IDEB3 = 0
              ADDSUBSFI(NIN)%P(1) = 1
              DO P = 1, NSPMD
               IF(ISCOM(P)>0) THEN 
                DO I = 1, NSNFI(NIN)%P(P)
                  IDEB2 = IDEBUT(P)
                  IDEB2 = IDEB2 + 1
                  LEN = MSGBUF_R(P)%P(IDEB2)
                  ADDSUBSFI(NIN)%P(IDEB3+I+1) = 
     +              ADDSUBSFI(NIN)%P(IDEB3+I) + LEN
                  DO J = 1, LEN
                    LISUBSFI(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
                  END DO
                  IDEBUT(P) = IDEBUT(P) + LEN + 1  
                  IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                    IDEB2 = IDEB2 + LEN
                    DO J = 1, LEN
                      INFLG_SUBSFI(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
                    END DO
                    IDEBUT(P) = IDEBUT(P) + LEN                
                  END IF
                  IDEB = IDEB + LEN
                END DO
                IDEB3 = IDEB3 + NSNFI(NIN)%P(P)
               ENDIF
              END DO
            ELSE
              LEN = 1
              IF(ASSOCIATED(ADDSUBSFI(NIN)%P))
     .          DEALLOCATE(ADDSUBSFI(NIN)%P)
              DO P = 1, NSPMD
                LEN = LEN + NSNFI(NIN)%P(P)
              END DO
              ALLOCATE(ADDSUBSFI(NIN)%P(LEN),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
              IDEB3 = 0
              ADDSUBSFI(NIN)%P(1) = 1
              DO P = 1, NSPMD
                DO I = 1, NSNFI(NIN)%P(P)
                  ADDSUBSFI(NIN)%P(IDEB3+I+1) = 
     +              ADDSUBSFI(NIN)%P(IDEB3+I)
                END DO
                IDEB3 = IDEB3 + NSNFI(NIN)%P(P)
              END DO
            END IF
            IF(IPARI(7,NIN) == 25 .AND. IPARI(58,NIN) > 0) THEN
              !type 25 edge part
              IF(ASSOCIATED(LISUBSFIE(NIN)%P))DEALLOCATE(LISUBSFIE(NIN)%P)
              LEN = 0
              DO P = 1, NSPMD
                LEN = LEN + NISUBSFIE(NIN)%P(P)
              END DO
              IERROR = 0
C             WRITE(6,*) NIN,"SIZE LISUBSFIE=",LEN
              IF(LEN>0) THEN
                ALLOCATE(LISUBSFIE(NIN)%P(LEN),STAT=IERROR)
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                END IF
                IF(IPARI(7,NIN)==25)THEN
                  IF(ASSOCIATED(INFLG_SUBSFIE(NIN)%P))DEALLOCATE(INFLG_SUBSFIE(NIN)%P)
                  ALLOCATE(INFLG_SUBSFIE(NIN)%P(LEN),STAT=IERROR)
                  IF(IERROR/=0) THEN
                    CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                    CALL ARRET(2)
                  END IF
                END IF
                LEN = 1
                IF(ASSOCIATED(ADDSUBSFIE(NIN)%P))
     .            DEALLOCATE(ADDSUBSFIE(NIN)%P)
                DO P = 1, NSPMD
                  LEN = LEN + NSNFIE(NIN)%P(P)
                END DO
                ALLOCATE(ADDSUBSFIE(NIN)%P(LEN),STAT=IERROR)
C               WRITE(6,*) NIN,"ADDSUBSFIE size:",LEN
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                END IF
                IDEB  = 0
                IDEB3 = 0
                ADDSUBSFIE(NIN)%P(1) = 1
                DO P = 1, NSPMD
                 IF(ISCOM(P)>0) THEN 
                  DO I = 1, NSNFIE(NIN)%P(P)
                    IDEB2 = IDEBUT(P)
                    IDEB2 = IDEB2 + 1
                    LEN = MSGBUF_R(P)%P(IDEB2)
C                   WRITE(6,*) P-1,"MSGBUF_R(",IDEB2,")=",LEN
                    ADDSUBSFIE(NIN)%P(IDEB3+I+1) = 
     +                ADDSUBSFIE(NIN)%P(IDEB3+I) + LEN
C                   WRITE(6,*) "ADDSUBSFIE(NIN)%P(",IDEB3+I+1,") =", ADDSUBSFIE(NIN)%P(IDEB3+I+1)
                    DO J = 1, LEN
                      LISUBSFIE(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
C                   WRITE(6,*) P-1,"LISUBFIE --- MSGBUF_R(",IDEB2+J,")=", MSGBUF_R(P)%P(IDEB2+J)
                    END DO
                    IDEBUT(P) = IDEBUT(P) + LEN + 1  
                    IDEB2 = IDEB2 + LEN
                    DO J = 1, LEN
                      INFLG_SUBSFIE(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
C                     WRITE(6,*) P-1,"INFLG --- MSGBUF_R(",IDEB2+J,")=", MSGBUF_R(P)%P(IDEB2+J)
                    END DO
                    IDEBUT(P) = IDEBUT(P) + LEN                
                    IDEB = IDEB + LEN
                  END DO
                  IDEB3 = IDEB3 + NSNFIE(NIN)%P(P)
                 ENDIF
                END DO
              ELSE
                LEN = 1
                IF(ASSOCIATED(ADDSUBSFIE(NIN)%P))
     .            DEALLOCATE(ADDSUBSFIE(NIN)%P)
                DO P = 1, NSPMD
                  LEN = LEN + NSNFI(NIN)%P(P)
                END DO
                ALLOCATE(ADDSUBSFIE(NIN)%P(LEN),STAT=IERROR)
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                END IF
                IDEB3 = 0
                ADDSUBSFIE(NIN)%P(1) = 1
                DO P = 1, NSPMD
                  DO I = 1, NSNFI(NIN)%P(P)
                    ADDSUBSFIE(NIN)%P(IDEB3+I+1) = 
     +                ADDSUBSFIE(NIN)%P(IDEB3+I)
                  END DO
                  IDEB3 = IDEB3 + NSNFI(NIN)%P(P)
                END DO
              END IF
            ENDIF ! TYPE25 EDGE PART
          END IF
        END DO
C
        DO P = 1, NSPMD
          IF(IRCOM(P)>0) THEN
            CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
            DEALLOCATE(MSGBUF_S(P)%P)
          END IF
          IF(ISCOM(P)>0) THEN
            DEALLOCATE(MSGBUF_R(P)%P)
          END IF
        END DO
C
      END IF
C
C   remise a 0 de newfront + recalcul size msg
C
      ISLEN7 = 0
      IRLEN7 = 0 
      ISLEN7T = 0
      IRLEN7T = 0
      ISLEN11 = 0
      IRLEN11 = 0
      ISLEN17 = 0
      IRLEN17 = 0
      IRLEN20 = 0
      ISLEN20 = 0
      IRLEN20T = 0
      ISLEN20T = 0
      IRLEN20E = 0
      ISLEN20E = 0
C type 25
      ISLEN25 = 0
      ISLEN25E = 0
      IRLEN25 = 0
      IRLEN25E = 0
      ISLEN25T = 0
      ISLEN25ET = 0
      IRLEN25T = 0
      IRLEN25ET = 0

      DO II = 1, NBINTC
        NIN = INTLIST(II)
      
        IF(NEWFRONT(NIN)==2) NEWFRONT(NIN)=0
        ITYP = IPARI(7,NIN)
        INTTH = IPARI(47,NIN)  
        
C type 7 
        IF(ITYP==7.OR.ITYP==10.OR.ITYP==22.OR.
     .     ITYP==23.OR.ITYP==24)THEN
          IF(INTTH == 0 ) THEN 
           DO P = 1, NSPMD
             ISLEN7 = ISLEN7 + NSNSI(NIN)%P(P)
             IRLEN7 = IRLEN7 + NSNFI(NIN)%P(P)
           END DO
C type 7 + heat trasfert
          ELSE
           DO P = 1, NSPMD
             ISLEN7T = ISLEN7T + NSNSI(NIN)%P(P)
             IRLEN7T = IRLEN7T + NSNFI(NIN)%P(P)
           END DO
          ENDIF  
        ELSEIF(ITYP == 11) THEN
C type 11
           DO P = 1, NSPMD
             ISLEN11 = ISLEN11 + NSNSI(NIN)%P(P)
             IRLEN11 = IRLEN11 + NSNFI(NIN)%P(P)
           END DO
C type 17
        ELSEIF(ITYP == 17) THEN
           DO P = 1, NSPMD
             ISLEN17 = ISLEN17 + NSNSI(NIN)%P(P)
             IRLEN17 = IRLEN17 + NSNFI(NIN)%P(P)
           END DO
        ELSEIF(ITYP == 20)THEN
C type 20
           IF(INTTH == 0) THEN 
             DO P = 1, NSPMD
               ISLEN20 = ISLEN20 + NSNSI(NIN)%P(P)
               IRLEN20 = IRLEN20 + NSNFI(NIN)%P(P)
               ISLEN20E= ISLEN20E+ NSNSIE(NIN)%P(P)
               IRLEN20E= IRLEN20E+ NSNFIE(NIN)%P(P)
             END DO
           ELSE
             DO P = 1, NSPMD
               ISLEN20T = ISLEN20T + NSNSI(NIN)%P(P)
               IRLEN20T = IRLEN20T + NSNFI(NIN)%P(P)
               ISLEN20E= ISLEN20E+ NSNSIE(NIN)%P(P)
               IRLEN20E= IRLEN20E+ NSNFIE(NIN)%P(P)
             END DO
           ENDIF
        ELSEIF(ITYP == 25)THEN
C type 25
           IEDGE = IPARI(58,NIN)
           IF(INTTH == 0) THEN 
             DO P = 1, NSPMD
               ISLEN25 = ISLEN25 + NSNSI(NIN)%P(P)
               IRLEN25 = IRLEN25 + NSNFI(NIN)%P(P)
               IF( IEDGE /= 0) THEN 
                 ISLEN25E= ISLEN25E+ NSNSIE(NIN)%P(P)
                 IRLEN25E= IRLEN25E+ NSNFIE(NIN)%P(P)
               ENDIF
             END DO
           ELSE
             DO P = 1, NSPMD
               ISLEN25T = ISLEN25T + NSNSI(NIN)%P(P)
               IRLEN25T = IRLEN25T + NSNFI(NIN)%P(P)
               IF( IEDGE /= 0) THEN 
                 ISLEN25E= ISLEN25E+ NSNSIE(NIN)%P(P)
                 IRLEN25E= IRLEN25E+ NSNFIE(NIN)%P(P)
               ENDIF
             END DO
           ENDIF

        END IF
      ENDDO

C ==================== 
      ENDIF  ! MODE = 2
C ==================== 

C
#endif
      RETURN
      END

