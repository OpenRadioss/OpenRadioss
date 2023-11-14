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
Chd|  SPMD_EXCH_I24                 source/mpi/interfaces/spmd_exch_i24.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_I24(IPARI    ,INTBUF_TAB     ,ITAB,
     *                         IAD_ELEM ,FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,FLAG,
     *                         INT24E2EUSE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "spmd_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),IAD_ELEM(2,*),FR_ELEM(*),
     *        ITAB(*),INTLIST(*),NBINTC,FLAG,I24MAXNSNE,INT24E2EUSE
      INTEGER
     *        IAD_I24(NBINTC+1,*), SFR_I24,FR_I24(*)
C    
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),
     *        REQ_SI(PARASIZ),REQ_RI(PARASIZ),REQ_S(PARASIZ),
     *        REQ_S2(PARASIZ),REQ_R(PARASIZ),REQ_R2(PARASIZ)
      INTEGER P,LENSD,LENRV,IADS(PARASIZ+1),IADR(PARASIZ+1),IERROR,
     *        SIZ,LOC_PROC,MSGTYP,IDEB(NINTER),IDB,PROC,
     *        MSGOFF,MSGOFF2,MSGOFF3,MSGOFF4,MSGOFF5
      INTEGER IADINT(NINTER,NSPMD)

      INTEGER I,J,L,NB,NL,NN,K,N,NOD,MODE,LEN,ALEN,ND,FLG,NIN,NTY,
     *        NSN,SN,SSIZ,NBI,NSI,IEDG4,
     *        SNREMOTE,SURF,SURFR,I_STOK,IT,LEN_NSNSI,CT,SEG,MS,NSNR,
     *        SNREMOTEBIS,NI,ILEN,RLEN,LI,LR,IGSTI,NFIT
      INTEGER IWORK(70000)
      my_real
     *    TMP,TMPR,SEND_PMAX(NINTER),REC_PMAX(NINTER),TIME_S,TIME_SR
      my_real ,
     *   DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR,RRECBUF
      my_real ,
     *   DIMENSION(:,:), ALLOCATABLE :: RSENDBUF

     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISENDBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRECBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: SNIDX
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,INDTRI,ISCANDR,ISCAND
      my_real
     *  SQLEN,SQLENR
      DATA MSGOFF/156/
      DATA MSGOFF2/157/
      DATA MSGOFF3/158/
      DATA MSGOFF4/159/
      DATA MSGOFF5/160/
C-----------------------------------------------
      SAVE IADS,IADR,BBUFS,BBUFR,REQ_S,REQ_S2,
     *                    REQ_SI,REQ_R,REQ_R2,
     *                    RRECBUF,IRECBUF,RSENDBUF,ISENDBUF,
     *                    ILEN,RLEN,LEN,LENSD,LENRV 
C-----------------------------------------------
      ALEN=10
      LOC_PROC = ISPMD+1
      SEND_PMAX(1:NINTER)=0
C--------------------------------------------------------
C For Part 3
      ILEN = 4
      RLEN = 8
C--------------------------------------------------------
           IF(NSPMD == 1)RETURN

C ----------------------------------      
C IFLAG=1 partie1 - Send   
C ----------------------------------      
      IF(FLAG==1)THEN

C----------------------------------------------------------------------------------------------------
C Partie Zero  IRTLM & TIME_S sont mis a zero quand les noeuds seconds locaux ne sont pas candidats
C----------------------------------------------------------------------------------------------------
           ALLOCATE(ISCAND(NUMNOD+I24MAXNSNE))
           ISCAND(1:NUMNOD+I24MAXNSNE)=0
           DO NI=1,NBINTC
               NIN = INTLIST(NI)
             NTY   = IPARI( 7,NIN)
             NSN   = IPARI( 5,NIN)
             NSNR   = IPARI( 24,NIN)
             IEDG4 = IPARI(59,NIN)
             IF(NTY==24)THEN
               I_STOK = INTBUF_TAB(NIN)%I_STOK(1)
               DO I=1,I_STOK
                 N = INTBUF_TAB(NIN)%CAND_N(I)
                 IF(N<=NSN)THEN
                   SN = INTBUF_TAB(NIN)%NSV(N)
                   ISCAND(SN)=1
                   MS = INTBUF_TAB(NIN)%CAND_E(I)
                 ENDIF
               ENDDO
               DO I=1,NSN
                 N = INTBUF_TAB(NIN)%NSV(I)
                 IF (ISCAND(N)==0)THEN
                    INTBUF_TAB(NIN)%TIME_S(I) = ZERO
                    INTBUF_TAB(NIN)%IRTLM(2*(I-1)+1) = 0
                    ISCAND(N)=0
                 ENDIF
               ENDDO
               IF(IEDG4 >0)THEN
                 DO I=1,NSNR
                     IF(ISEDGE_FI(NIN)%P(I)==-1)THEN
                         IRTLM_FI(NIN)%P(1,I)=0
                         TIME_SFI(NIN)%P(I)=ZERO
                     ENDIF
                 ENDDO
               ENDIF
             ENDIF
           ENDDO

C--------------------------------------------------------

C Comm sur l'interface type 24
C 1ere partie, on ramene sur le proc qui a les neouds slv les valeurs de IRTLM_FI + TIME_SFI & traitements
C 2eme partie on les traite sur le proc  qui a les neouds slv les valeurs
C 3eme partie on renvoie sur les procs remotes lesvaleurs globalisees

C--------------------------------------------------------
C 1ere partie, on ramene sur le proc qui a les neouds slv les valeurs de IRTLM_FI + TIME_SFI
C--------------------------------------------------------

      LOC_PROC = ISPMD+1
      IADS(1:NSPMD+1) = 0
      IADR(1:NSPMD+1) = 0
      LENSD = 0
      LENRV = 0

      ALEN=10


C Comptage des tailles de buffer  Reception et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NI=1,NBINTC
             NIN = INTLIST(NI)
           NTY=IPARI(7,NIN)
           IF(NTY==24)THEN
              LENSD = LENSD + NSNFI(NIN)%P(P)*ALEN
              LENRV = LENRV + NSNSI(NIN)%P(P)*ALEN
           ENDIF
         ENDDO
      ENDDO
      IADR(NSPMD+1)=LENRV+1

C Preparation du send
      IF(LENSD>0)THEN
          ALLOCATE(BBUFS(LENSD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF

C ---------------------------------------------
C Preparation du recieve
      IF(LENRV>0)THEN
          ALLOCATE(BBUFR(LENRV),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF

      DO P=1, NSPMD
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
            MSGTYP = MSGOFF2
            CALL MPI_IRECV( BBUFR(IADR(P)),SIZ,REAL,IT_SPMD(P),MSGTYP,
     *                     MPI_COMM_WORLD,REQ_R(P),IERROR )
          ENDIF
      ENDDO

C ---------------------------------------------
C Send
      L=1
      IDEB=0
      DO P=1, NSPMD
       IADS(P)=L
       IF (P/= LOC_PROC) THEN
         DO NI=1,NBINTC
             NIN = INTLIST(NI)
           NTY   =IPARI(7,NIN)
           IF(NTY==24) THEN
             NB = NSNFI(NIN)%P(P)
             DO NN=1,NB
               BBUFS(L)=  IRTLM_FI(NIN)%P(1,NN+IDEB(NIN))
               BBUFS(L+1)=IRTLM_FI(NIN)%P(2,NN+IDEB(NIN))
               BBUFS(L+2)=TIME_SFI(NIN)%P(NN+IDEB(NIN))
               BBUFS(L+3)=SECND_FRFI(NIN)%P(1,NN+IDEB(NIN))
               BBUFS(L+4)=SECND_FRFI(NIN)%P(2,NN+IDEB(NIN))
               BBUFS(L+5)=SECND_FRFI(NIN)%P(3,NN+IDEB(NIN))
               BBUFS(L+6)=PENE_OLDFI(NIN)%P(1,NN+IDEB(NIN))
               BBUFS(L+7)=STIF_OLDFI(NIN)%P(1,NN+IDEB(NIN))
                 BBUFS(L+8)=PENE_OLDFI(NIN)%P(3,NN+IDEB(NIN))
                 BBUFS(L+9)=PENE_OLDFI(NIN)%P(5,NN+IDEB(NIN))
               L=L+10
             ENDDO
             IDEB(NIN)=IDEB(NIN)+NB
           ENDIF
         ENDDO  ! DO NIN=1,NINTER
         SIZ = L-IADS(P)
         IF(SIZ>0)THEN
              MSGTYP = MSGOFF2
              CALL MPI_ISEND(
     .          BBUFS(IADS(P)),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
         ENDIF
       ENDIF ! ENDIF P/= LOC_PROC
      ENDDO  ! DO P=1, NSPMD


      RETURN
      ENDIF
     
C----------------------------------      
C IFLAG=2 partie2 - Recieve      
C ----------------------------------      
      IF(FLAG==2)THEN
    
C Recieve
      L=0
      IDEB = 0

      DO P=1, NSPMD
          L=0
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
            MSGTYP = MSGOFF2

C WAIT
           CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)

           DO NI=1,NBINTC
               NIN = INTLIST(NI)
             NTY   =IPARI(7,NIN)

             IF(NTY==24)THEN

               NB = NSNSI(NIN)%P(P)
               IF (NB > 0)THEN
C
                 DO K=1,NB
                   ND = NSVSI(NIN)%P(IDEB(NIN)+K)

C Merge IRTLM & TIME_S
                   SN = INTBUF_TAB(NIN)%NSV(ND)
                   TIME_S  = INTBUF_TAB(NIN)%TIME_S(ND)
                   SURF    = INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1)
                   SURFR   = BBUFR(IADR(P)+L)
                   TIME_SR = BBUFR(IADR(P)+L+2)

                   IF (BBUFR(IADR(P)+L)==0 
     *                               .AND.BBUFR(IADR(P)+L+2)==ZERO) THEN
C Si IRTLM(1, == 0 et TIME_S(SNR)==0. Alors il n'est pas candidat retenu   
                  
             ELSEIF (INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) == 0 
     *            .AND. INTBUF_TAB(NIN)%TIME_S(ND) ==ZERO)THEN
C Si candidat local n'est pas retenu, on copie simplement                 
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) = BBUFR(IADR(P)+L)
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2) = BBUFR(IADR(P)+L+1)
                     INTBUF_TAB(NIN)%TIME_S(ND) = BBUFR(IADR(P)+L+2)

                   ELSEIF (TIME_S==-EP20 .AND. SURF == 0)THEN
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) = 0
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2) = 0
                     INTBUF_TAB(NIN)%TIME_S(ND) = -EP20

                   ELSEIF (TIME_SR==-EP20 .AND. SURFR == 0)THEN
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) = 0
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2) = 0
                     INTBUF_TAB(NIN)%TIME_S(ND) = -EP20
                   ELSEIF (TIME_S==-EP20 .AND. SURF == 0)THEN
C nothing to do
                   ELSEIF( SURFR > 0 .AND. TIME_SR==-EP20 .AND.
     *                      SURF > 0 .AND. TIME_S==-EP20 )THEN
C Case both SURFR values are positive & TIME_S is Equal to EP20
C We choose the highest value
                     IF (SURFR > SURF)THEN
                       INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) =   BBUFR(IADR(P)+L)
                       INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2) = BBUFR(IADR(P)+L+1)
                       INTBUF_TAB(NIN)%TIME_S(ND) = BBUFR(IADR(P)+L+2)
                     ENDIF
                   ELSEIF(SURFR > 0 .AND. TIME_SR==-EP20)THEN
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) = BBUFR(IADR(P)+L)
                     INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2) = BBUFR(IADR(P)+L+1)
                     INTBUF_TAB(NIN)%TIME_S(ND) = -EP20

                   ELSEIF(SURF > 0 .AND. TIME_S==-EP20)THEN
C nothing to do
                   ELSEIF(SURFR < 0)THEN
                     IF (TIME_SR ==  TIME_S) THEN
                        IF (ABS(SURFR) > ABS(SURF))THEN
                          INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1)  = BBUFR(IADR(P)+L)
                          INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2)= 
     *                                      BBUFR(IADR(P)+L+1)
                          INTBUF_TAB(NIN)%TIME_S(ND) = BBUFR(IADR(P)+L+2)
                        ENDIF
                     ELSEIF (TIME_S <= TIME_SR ) THEN
                       INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1)  = 
     *                                        BBUFR(IADR(P)+L)
                       INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2)= INT(BBUFR(IADR(P)+L+1))
                       INTBUF_TAB(NIN)%TIME_S(ND) = BBUFR(IADR(P)+L+2)
                     ENDIF
                   ENDIF 
C Merge SECND_FR
             
             IF(ABS(BBUFR(IADR(P)+L+3)) > 
     *                ABS(INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1)))
     *                    INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1)   = BBUFR(IADR(P)+L+3)
C
                   IF(ABS(BBUFR(IADR(P)+L+4)) > 
     *                ABS(INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2)))
     *                    INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2)   = BBUFR(IADR(P)+L+4)
C
                   IF(ABS(BBUFR(IADR(P)+L+5)) > 
     *                ABS(INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3)))
     *                    INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3)   = BBUFR(IADR(P)+L+5)

C case equal abs but opposite sign
                   IF(BBUFR(IADR(P)+L+3)==-INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1) )
     *                INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1)   = ABS(BBUFR(IADR(P)+L+3))
C
                   IF(BBUFR(IADR(P)+L+4)==-INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2) )
     *                INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2) = ABS(BBUFR(IADR(P)+L+4))
C
                   IF(BBUFR(IADR(P)+L+5)==-INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3) )
     *                INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3) = ABS(BBUFR(IADR(P)+L+5))
C

C Merge PENE_OLD
cc                   IF(INTBUF_TAB(NIN)%PENE_OLD(2*(ND-1)+1)/=0 .OR.
cc     *                            BBUFR(IADR(P)+L+6)/=0)THEN

                     INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+1)=MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+1),
     *                                          BBUFR(IADR(P)+L+6) )
                     INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+3)=
     *                                    MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+3),
     *                                          BBUFR(IADR(P)+L+8) )
cc                     IF(TT==ZERO)THEN   !due to Inacti=6
                         INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+5)=
     *                                    MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+5),
     *                                          BBUFR(IADR(P)+L+9) )
cc                     ENDIF
cctobemoved                     IF(INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1) ==0)
cctobemoved     *                    INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+5)=ZERO
                     

                     INTBUF_TAB(NIN)%STIF_OLD(2*(ND-1)+1)=MAX(INTBUF_TAB(NIN)%STIF_OLD(2*(ND-1)+1),
     *                                          BBUFR(IADR(P)+L+7) )
cc                   ENDIF
                   L=L+10
                  ENDDO
               ENDIF
             ENDIF ! ity==24
             IDEB(NIN)=IDEB(NIN)+NB
           ENDDO
          ENDIF !  IF (NB > 0)
          L=L+SIZ
      ENDDO        ! DO P=1, NSPMD

C Fin du send
      DO P = 1, NSPMD
          IF (P==NSPMD)THEN
             SIZ=LENSD-IADS(P)
          ELSE
             SIZ=IADS(P+1)-IADS(P)
          ENDIF
          IF(SIZ>0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO

      IF (ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
      IF (ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)

C -------------------
C T24 E2E Merge ISPT2
C -------------------
C ISPT2 : Tag Array for secnd nodes (not fictive nodes)
C         Set to 1 when 
C               1/ Secnd node is part of an Edge
C               2/ This Edge is impacting, eg fictive node has IRTLM not NULL
C
C     Note for Parallelism - at this Stage Remote nodes & nodes on Marter processor are merged.
C     Fictive nodes (& secnd surface IRTS) are only affected to 1 SPMD Domain. They can only be
C     either Local or remote (not neighbour in term of SPMD domain).
C     One can start to merge here and spread the info to the neighboug domains.
      IF(INT24E2EUSE == 1)THEN
         DO NI=1,NBINTC
              NIN = INTLIST(NI)
            NTY = IPARI(7,NIN)
            IEDG4 = IPARI(59,NIN)
            IF(NTY==24 .AND. IEDG4 > 0)THEN
               NSN = IPARI(5,NIN)
               DO SN=1,NSN
C Basic case : Secnd node is not part of an Edge
                  INTBUF_TAB(NIN)%ISPT2(SN)=0
                  NSI = INTBUF_TAB(NIN)%ISEGPT(SN)
                  ND=INTBUF_TAB(NIN)%NSV(SN)
                  IF(NSI > 0)THEN
                     IF(INTBUF_TAB(NIN)%IRTLM(2*(NSI-1)+1) /= 0)THEN
                       INTBUF_TAB(NIN)%ISPT2(SN) = 0
                     ELSE
                       INTBUF_TAB(NIN)%ISPT2(SN) = 1
                     ENDIF
                  ELSEIF(NSI<0)THEN
                     INTBUF_TAB(NIN)%ISPT2(SN) = 1
                  ENDIF 
               ENDDO
            ENDIF
          ENDDO
      ENDIF
C-----------------------------------------------------------
C 2eme partie - echanges sur les noeuds seconds frontieres
C               pour toutes les interface type 24.
C-----------------------------------------------------------
      LEN=3
      IADS(1:NSPMD+1)=0

      DO I=1,NSPMD
        IADS(I)=IAD_I24(1,I)
      ENDDO
      IADS(NSPMD+1)=SFR_I24+1
C Preparation du send
      ILEN=4
      RLEN=8
      ALLOCATE(ISENDBUF(4,SFR_I24))
      ALLOCATE(IRECBUF(ILEN*SFR_I24))
      ALLOCATE(RSENDBUF(8,SFR_I24))
      ALLOCATE(RRECBUF(RLEN*SFR_I24))

C mise en place du irecieve
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        IF(SIZ/=0)THEN
          LI = (IADS(P)-1)*ILEN+1
            LR = (IADS(P)-1)*RLEN+1
          MSGTYP = MSGOFF3
          LEN = SIZ*4
          CALL MPI_IRECV(
     S      IRECBUF(LI),LEN,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(P),IERROR)

          MSGTYP = MSGOFF4 
          LEN = SIZ*8
          CALL MPI_IRECV(
     S      RRECBUF(LR),LEN,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R2(P),IERROR)

        ENDIF
       ENDDO

       NB = 1
       DO P = 1, NSPMD
          DO NI=1,NBINTC
               NIN=INTLIST(NI)
             NTY    = IPARI(7,NIN)
             NSN    = IPARI(5,NIN)
             IEDG4 = IPARI(59,NIN)
               IF(NTY==24) THEN

               DO I=IAD_I24(NI,P),IAD_I24(NI+1,P)-1
                
                 ND = FR_I24(I)
                 SN = INTBUF_TAB(NIN)%NSV(ND)

                 ISENDBUF(1,NB)=ITAB(SN)
                 ISENDBUF(2,NB)=INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1)
                 ISENDBUF(3,NB)=INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2)
                 IF(IEDG4 > 0) THEN
                      ISENDBUF(4,NB)= INTBUF_TAB(NIN)%ISPT2(ND)
                 ELSE
                      ISENDBUF(4,NB)=0                  
                 ENDIF
                 RSENDBUF(1,NB) = INTBUF_TAB(NIN)%TIME_S(ND)
                 RSENDBUF(2,NB) = INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1)
                 RSENDBUF(3,NB) = INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2)
                 RSENDBUF(4,NB) = INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3)
                 RSENDBUF(5,NB) = INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+1)
                 RSENDBUF(6,NB) = INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+3)
                 RSENDBUF(8,NB) = INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+5)
                 RSENDBUF(7,NB) = INTBUF_TAB(NIN)%STIF_OLD(2*(ND-1)+1)
                 NB=NB+1
               ENDDO
             ENDIF
          ENDDO  ! DO NI=1,NBINTC
       ENDDO     ! DO P=1,NSPMD

C--------------------------------------------------------------------
C   echange messages
C
      DO P=1,NSPMD
         SIZ = IADS(P+1) - IADS(P)
         IF (SIZ >0)THEN
           MSGTYP = MSGOFF3
           L = IADS(P)
           CALL MPI_ISEND(
     S        ISENDBUF(1,L),SIZ*4,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(P),IERROR)
     
           MSGTYP = MSGOFF4
           CALL MPI_ISEND(
     S        RSENDBUF(1,L),SIZ*8,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S2(P),IERROR)
         ENDIF   ! IF (SIZ >0)
      ENDDO      ! DO P=1,NSPMD
C--------------------------------------------------------------------
      I24COM3 = 1

      RETURN
      ENDIF
      
C ----------------------------------      
C IFLAG=3 partie3 - Recieve 
C ----------------------------------      
      IF(FLAG==3)THEN
      
      IF(I24COM3==0)RETURN
     
C Reception
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        IF(SIZ/=0)THEN
          IDB = IADS(P)
          CALL MPI_WAIT(REQ_R (P),STATUS,IERROR)

          CALL MPI_WAIT(REQ_R2(P),STATUS,IERROR)

C Traitements

         DO NI=1,NBINTC
           NIN = INTLIST(NI)
              
           NTY    = IPARI(7,NIN)
           NSN    = IPARI(5,NIN)
           IEDG4 = IPARI(59,NIN)
             IF (NTY == 24)THEN

             DO K=IAD_I24(NI,P),IAD_I24(NI+1,P)-1 
                SN = FR_I24(K)
                TIME_S = INTBUF_TAB(NIN)%TIME_S(SN)
                SURF   = INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)
                SURFR  = IRECBUF(2+(IDB-1)*ILEN)
                TIME_SR    = RRECBUF(1+(IDB-1)*RLEN)
                IF (TIME_SR==0 .AND. SURFR==0)THEN
C Rien    faire

                   ELSEIF (TIME_S==0 .AND.SURF==0)THEN
C On impose la valeur du nd candidat
                     INTBUF_TAB(NIN)%TIME_S(SN) = TIME_SR
                     INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)   = IRECBUF(2+(IDB-1)*ILEN)
                     INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = IRECBUF(3+(IDB-1)*ILEN)

C maintenant la maj vient bien d un candidat

                   ELSEIF( TIME_S == -EP20 .AND. SURF == 0)THEN
C Rien    faire

                   ELSEIF( SURFR == 0 .AND.  TIME_SR == -EP20)THEN
                      INTBUF_TAB(NIN)%TIME_S(SN) = -EP20
                      INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)   = IRECBUF(2+(IDB-1)*ILEN)
                      INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = IRECBUF(3+(IDB-1)*ILEN)

                   ELSEIF( SURFR > 0 .AND. TIME_SR==-EP20 .AND.
     *                      SURF > 0 .AND. TIME_S==-EP20)THEN
C Case both SURFR values are positive & TIME_S is Equal to EP20
C We choose the highest value
                      IF (SURFR > SURF)THEN
                        INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1) = IRECBUF(2+(IDB-1)*ILEN)
                        INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = 
     *                                           IRECBUF(3+(IDB-1)*ILEN)
                        INTBUF_TAB(NIN)%TIME_S(SN) = -EP20
                        INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+1)=RRECBUF(5+(IDB-1)*RLEN)
                        INTBUF_TAB(NIN)%STIF_OLD(2*(SN-1)+1)=RRECBUF(7+(IDB-1)*RLEN)
                      ENDIF
                     
                   ELSEIF(  SURF > 0 .AND.  TIME_S == -EP20)THEN
c rien a faire
                   
                   ELSEIF( SURFR > 0 .AND.  TIME_SR == -EP20)THEN
                      INTBUF_TAB(NIN)%TIME_S(SN) = -EP20
                      INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)   = IRECBUF(2+(IDB-1)*ILEN)
                      INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = IRECBUF(3+(IDB-1)*ILEN)
                   
                   ELSEIF(  SURFR < 0 )THEN
                        IF (TIME_SR ==  TIME_S) THEN
                           IF (ABS(SURFR) > ABS(SURF))THEN
                            INTBUF_TAB(NIN)%TIME_S(SN) = TIME_SR
                            INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)   = 
     *                                           IRECBUF(2+(IDB-1)*ILEN)
                            INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = 
     *                                           IRECBUF(3+(IDB-1)*ILEN)
                           ENDIF
                    ELSEIF (TIME_S <= TIME_SR ) THEN
             INTBUF_TAB(NIN)%TIME_S(SN) = TIME_SR
             INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)   = 
     *             IRECBUF(2+(IDB-1)*ILEN)
             INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+2) = 
     *             IRECBUF(3+(IDB-1)*ILEN)
                        ENDIF
                    ENDIF
                 
C Max pour les SECND_FR
              IF (ABS(RRECBUF(2+(IDB-1)*RLEN)) > 
     *                 (ABS(INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+1)) ) )
     *                   INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+1) = RRECBUF(2+(IDB-1)*RLEN)
     
              IF (ABS(RRECBUF(3+(IDB-1)*RLEN)) > 
     *                  ABS(INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+2)) )
     *                   INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+2) = RRECBUF(3+(IDB-1)*RLEN)
     
              IF (ABS(RRECBUF(4+(IDB-1)*RLEN)) > 
     *                  ABS(INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+3)) )
     *                   INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+3) = RRECBUF(4+(IDB-1)*RLEN)

C Case equal abs but opposite sign
                    IF (RRECBUF(2+(IDB-1)*RLEN)==-INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+1) )
     *                  INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+1)=
     *                                      ABS(RRECBUF(2+(IDB-1)*RLEN))

                    IF (RRECBUF(3+(IDB-1)*RLEN)==-INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+2) )
     *                  INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+2)=
     *                                      ABS(RRECBUF(3+(IDB-1)*RLEN))

                    IF (RRECBUF(4+(IDB-1)*RLEN)==-INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+3) )
     *                  INTBUF_TAB(NIN)%SECND_FR(6*(SN-1)+3)=
     *                                      ABS(RRECBUF(4+(IDB-1)*RLEN))

C Merge PENE_OLD
                  INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+1)=MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+1),
     *                                      RRECBUF(5+(IDB-1)*RLEN) )
                  INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+3)=MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+3),
     *                                      RRECBUF(6+(IDB-1)*RLEN) )

cc                  IF(TT==DT2)THEN !due to Inacti=6
                     INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+5)=MAX(INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+5),
     *                                      RRECBUF(8+(IDB-1)*RLEN) )
cc                  ENDIF

cctobemoved                  IF(INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)==0)
cctobemoved     *                INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+5)=ZERO
     
                  INTBUF_TAB(NIN)%STIF_OLD(2*(SN-1)+1)=MAX(INTBUF_TAB(NIN)%STIF_OLD(2*(SN-1)+1),
     *                                     RRECBUF(7+(IDB-1)*RLEN) )
     
C T24 E2E Merge ISPT2
                  IF(IEDG4 > 0)THEN
                     ND=INTBUF_TAB(NIN)%NSV(SN)
                     INTBUF_TAB(NIN)%ISPT2(SN) = MAX ( INTBUF_TAB(NIN)%ISPT2(SN), IRECBUF(4+(IDB-1)*ILEN))
                  ENDIF
               IDB=IDB+1
             ENDDO        ! K=,IAD_I24(NI,P),IAD_I24(NI+1,P)-1 
           ENDIF          ! IF (NTY == 24)THEN
         ENDDO            ! DO NI=1,NBINTC
        ENDIF             !IF(SIZ/=0)THEN
      ENDDO               ! DO P=1,NSPMD

C Fin send
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        IF(SIZ/=0)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
          CALL MPI_WAIT(REQ_S2(P),STATUS,IERROR)
        ENDIF
      ENDDO

C Treat PENE_OLD(5
      
           DO NI=1,NBINTC
               NIN = INTLIST(NI)
             NTY   = IPARI( 7,NIN)
             NSN   = IPARI( 5,NIN)
             NSNR   = IPARI( 24,NIN)
             IEDG4 = IPARI(59,NIN)
             IF(NTY==24)THEN
                DO SN=1,NSN
                   IF(INTBUF_TAB(NIN)%IRTLM(2*(SN-1)+1)==0)
     *                INTBUF_TAB(NIN)%PENE_OLD(5*(SN-1)+5)=ZERO
                ENDDO
             ENDIF
           ENDDO

      IF(ALLOCATED(ISENDBUF))DEALLOCATE(ISENDBUF)
      IF(ALLOCATED(IRECBUF))DEALLOCATE(IRECBUF)
      IF(ALLOCATED(RSENDBUF))DEALLOCATE(RSENDBUF)
      IF(ALLOCATED(RRECBUF))DEALLOCATE(RRECBUF)

C ------------------------------------------------------------------
C 3e partie on renvoie les valeurs globalisees sur les procs remote
C ------------------------------------------------------------------
      LEN=6
      LOC_PROC = ISPMD+1
      IADS = 0
      IADR = 0
      LENSD = 0
      LENRV = 0

      ALEN=11
C Comptage des tailles de buffer  Receeption et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NIN=1,NINTER
           NTY=IPARI(7,NIN)
           IF(NTY==24) THEN
              LENSD = LENSD + NSNSI(NIN)%P(P)*ALEN
              LENRV = LENRV + NSNFI(NIN)%P(P)*ALEN
           ENDIF
         ENDDO
      ENDDO
      IADR(NSPMD+1)=LENRV+1

      IF(LENSD>0)THEN
          ALLOCATE(BBUFS(LENSD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF

C Preparation du recieve
      IF(LENRV>0)THEN
          ALLOCATE(BBUFR(LENRV),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF
      
      
      DO P=1, NSPMD
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
            MSGTYP = MSGOFF5
            CALL MPI_IRECV( BBUFR(IADR(P)),SIZ,REAL,IT_SPMD(P),MSGTYP,
     *                     MPI_COMM_WORLD,REQ_R(P),IERROR )
         ENDIF
      ENDDO
      
C Send
      L=1
      IDEB = 0
      DO P=1, NSPMD
       IADS(P)=L
       IF (P/= LOC_PROC) THEN
         DO NI=1,NBINTC
             NIN = INTLIST(NI)
           NTY   =IPARI(7,NIN)
           IF(NTY==24)THEN
               IEDG4 = IPARI(59,NIN)
               NB = NSNSI(NIN)%P(P)
C Preparation du send
               DO NN=1,NB
                 ND = NSVSI(NIN)%P(IDEB(NIN)+NN)
                 NOD=INTBUF_TAB(NIN)%NSV(ND)
                 BBUFS(L  )=INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+1)
                 BBUFS(L+1)=INTBUF_TAB(NIN)%IRTLM(2*(ND-1)+2)
                 BBUFS(L+2)=INTBUF_TAB(NIN)%TIME_S(ND)
                 BBUFS(L+3)=INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+1)
                 BBUFS(L+4)=INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+2)
                 BBUFS(L+5)=INTBUF_TAB(NIN)%SECND_FR(6*(ND-1)+3)
                 BBUFS(L+6)=INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+1)
                 BBUFS(L+7)=INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+3)
                 BBUFS(L+9)=INTBUF_TAB(NIN)%PENE_OLD(5*(ND-1)+5)
                 BBUFS(L+8)=INTBUF_TAB(NIN)%STIF_OLD(2*(ND-1)+1)
                 IF(IEDG4 > 0)THEN
                   BBUFS(L+10)=INTBUF_TAB(NIN)%ISPT2(ND)
                 ELSE
                   BBUFS(L+10)=0
                 ENDIF
                 L = L + 11
               ENDDO
           ENDIF
           IDEB(NIN)=IDEB(NIN)+NB
         ENDDO

         SIZ = L-IADS(P)
         IF(SIZ>0)THEN
              MSGTYP = MSGOFF5
C Send
              CALL MPI_ISEND(
     .          BBUFS(IADS(P)),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
         ENDIF
       ENDIF
      ENDDO
      IADS(NSPMD+1)=L
C Third part of Comm routine has been done      
      I24COM3 = 0
      
C Fourth part of Comm routine has been done      
      I24COM4 = 1
      RETURN
      ENDIF
      
C ----------------------------------      
C IFLAG=4 partie4 - Recieve 
C ----------------------------------      
      IF(FLAG==4)THEN
      IF(I24COM4==0)RETURN
     
C Recieve
      L=0
      IDEB = 0

      DO P=1, NSPMD
          L=0
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
           
             CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
           DO NI=1,NBINTC
               NIN=INTLIST(NI)
             NTY   = IPARI(7,NIN)
             IGSTI = IPARI(34,NIN)

             IF(NTY==24) THEN
              IEDG4 = IPARI(59,NIN)
              NB = NSNFI(NIN)%P(P)

               IF (NB > 0)THEN
                IF(IMPL_S>0.AND.IGSTI==6)THEN
C--------------keep  STIF_OLDFI(NIN)%P(1,               
                 DO K=1,NB
                    IRTLM_FI(NIN)%P(1,IDEB(NIN)+K)=BBUFR(IADR(P)+L)
                    IRTLM_FI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+1)
                    TIME_SFI(NIN)%P(IDEB(NIN)+K)=BBUFR(IADR(P)+L+2)
C Same initialization than in i24optcd :
C           copy SECND_FRFI(1,2,3) into SECND_FRFI(4,5,6) & Flush SECND_FRFI to zero
                    SECND_FRFI(NIN)%P(1,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(2,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(3,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(4,IDEB(NIN)+K)=BBUFR(IADR(P)+L+3)
                    SECND_FRFI(NIN)%P(5,IDEB(NIN)+K)=BBUFR(IADR(P)+L+4)
                    SECND_FRFI(NIN)%P(6,IDEB(NIN)+K)=BBUFR(IADR(P)+L+5)
                    PENE_OLDFI(NIN)%P(1,IDEB(NIN)+K)=ZERO
                    PENE_OLDFI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+6)
              PENE_OLDFI(NIN)%P(3,IDEB(NIN)+K)=BBUFR(IADR(P)+L+7)
              PENE_OLDFI(NIN)%P(5,IDEB(NIN)+K)=BBUFR(IADR(P)+L+9)
                    STIF_OLDFI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+8)
                    IF(IEDG4 > 0)THEN
                       ISPT2_FI(NIN)%P(IDEB(NIN)+K)=BBUFR(IADR(P)+L+10)
                    ENDIF
                    L=L+11
                 ENDDO
                ELSE
                 DO K=1,NB
                    IRTLM_FI(NIN)%P(1,IDEB(NIN)+K)=BBUFR(IADR(P)+L)
                    IRTLM_FI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+1)
                    TIME_SFI(NIN)%P(IDEB(NIN)+K)=BBUFR(IADR(P)+L+2)
C Same initialization than in i24optcd :
C           copy SECND_FRFI(1,2,3) into SECND_FRFI(4,5,6) & Flush SECND_FRFI to zero
                    SECND_FRFI(NIN)%P(1,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(2,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(3,IDEB(NIN)+K)=ZERO
                    SECND_FRFI(NIN)%P(4,IDEB(NIN)+K)=BBUFR(IADR(P)+L+3)
                    SECND_FRFI(NIN)%P(5,IDEB(NIN)+K)=BBUFR(IADR(P)+L+4)
                    SECND_FRFI(NIN)%P(6,IDEB(NIN)+K)=BBUFR(IADR(P)+L+5)
                    PENE_OLDFI(NIN)%P(1,IDEB(NIN)+K)=ZERO
                    PENE_OLDFI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+6)
              PENE_OLDFI(NIN)%P(3,IDEB(NIN)+K)=BBUFR(IADR(P)+L+7)
              PENE_OLDFI(NIN)%P(5,IDEB(NIN)+K)=BBUFR(IADR(P)+L+9)
                    STIF_OLDFI(NIN)%P(1,IDEB(NIN)+K)=ZERO
                    STIF_OLDFI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+8)
                    IF(IEDG4 > 0)THEN
                       ISPT2_FI(NIN)%P(IDEB(NIN)+K)=BBUFR(IADR(P)+L+10)
                    ENDIF
                    L=L+11
                 ENDDO
                END IF!(IMPL_S>0.AND.IGSTI==6)THEN
               ENDIF
             ENDIF
             IDEB(NIN)=IDEB(NIN)+NB
           ENDDO
          ENDIF
      ENDDO

C Fin du send
      DO P = 1, NSPMD
          IF (P==NSPMD)THEN
             SIZ=LENSD-IADS(P)
          ELSE
             SIZ=IADS(P+1)-IADS(P)
          ENDIF
          IF(SIZ>0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO


      IF (ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
      IF (ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)

C Fourth part of Comm routine has been done      
      I24COM4=0
      ENDIF    ! fi iflag=4
#endif
      RETURN
      END
