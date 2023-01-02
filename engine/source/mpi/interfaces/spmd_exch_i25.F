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
Chd|  SPMD_EXCH_I25                 source/mpi/interfaces/spmd_exch_i25.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_I25(IPARI    ,INTBUF_TAB     ,ITAB,
     *                         IAD_ELEM ,FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I25  ,FR_I25  ,SFR_I25,FLAG)
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
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),IAD_ELEM(2,*),FR_ELEM(*),
     *        ITAB(*),INTLIST(*),NBINTC,FLAG
      INTEGER
     *        IAD_I25(NBINTC+1,NSPMD), SFR_I25,FR_I25(SFR_I25)
C    
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER P,LENSD,LENRV,IERROR,
     *        SIZ,LOC_PROC,MSGTYP,IDEB(NINTER),IDB,PROC,
     *        MSGOFF,MSGOFF2,MSGOFF3,MSGOFF4,MSGOFF5
      INTEGER I,J,L,NB,NL,NN,K,N,NOD,MODE,LEN,ALEN,ND,FLG,NIN,NTY,
     *        NSN,SN,SSIZ,NBI,
     *        SURF,SURFR,SUBTRIAR,KLEAVE,KLEAVE_R,PROC_R,I_STOK,IT,CT,MS,
     *        NI,ILEN,RLEN,LI,LR

      my_real
     *    TIME_S_1, TIME_S_2, TIME_SR_1, TIME_SR_2
      my_real ,
     *   DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR,RRECBUF
      my_real ,
     *   DIMENSION(:,:), ALLOCATABLE :: RSENDBUF

     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISENDBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRECBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: SNIDX
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,INDTRI,ISCANDR
      INTEGER, DIMENSION(:), ALLOCATABLE:: REQ_SI,REQ_RI,REQ_S,IADS
      INTEGER, DIMENSION(:), ALLOCATABLE:: REQ_S2,REQ_R,REQ_R2,IADR

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
      LOC_PROC = ISPMD+1
C ----------------------------------      
C IFLAG=1 partie1 - Send   
C ----------------------------------      
      IF(FLAG==1)THEN
C      WRITE(6,*) __FILE__,__LINE__ ; CALL FLUSH(6)
       ASSERT(.NOT.(ALLOCATED(REQ_SI)))
       ASSERT(.NOT.(ALLOCATED(REQ_RI)))
       ASSERT(.NOT.(ALLOCATED(REQ_S)))
       ASSERT(.NOT.(ALLOCATED(REQ_S2)))
       ASSERT(.NOT.(ALLOCATED(REQ_R)))
       ASSERT(.NOT.(ALLOCATED(REQ_R2)))
       ASSERT(.NOT.(ALLOCATED(IADR)))
       ASSERT(.NOT.(ALLOCATED(IADS)))

       ALLOCATE(REQ_SI(NSPMD))
       ALLOCATE(REQ_RI(NSPMD))
       ALLOCATE(REQ_S(NSPMD))
       ALLOCATE(REQ_S2(NSPMD))
       ALLOCATE(REQ_R(NSPMD))
       ALLOCATE(REQ_R2(NSPMD))
       ALLOCATE(IADR(NSPMD+1))
       ALLOCATE(IADS(NSPMD+1))

C--------------------------------------------------------

C Comm sur l'interface type 25
C 1ere partie, on ramene sur le proc qui a les noeuds slv les valeurs de IRTLM_FI + TIME_SFI & traitements
C 2eme partie on les traite sur le proc  qui a les neouds slv les valeurs
C 3eme partie on renvoie sur les procs remotes les valeurs globalisees

C--------------------------------------------------------
C 1ere partie, on ramene sur le proc qui a les neouds slv les valeurs de IRTLM_FI + TIME_SFI
C--------------------------------------------------------

      LOC_PROC = ISPMD+1
      IADS(1:NSPMD+1) = 0
      IADR(1:NSPMD+1) = 0
      LENSD = 0
      LENRV = 0

      ALEN=6


C Comptage des tailles de buffer  Reception et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NI=1,NBINTC
           NIN = INTLIST(NI)
           NTY=IPARI(7,NIN)
           IF(NTY==25)THEN
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
           IF(NTY==25) THEN
             NB = NSNFI(NIN)%P(P)
             DO NN=1,NB
               BBUFS(L)   =IRTLM_FI(NIN)%P(1,NN+IDEB(NIN))
               BBUFS(L+1) =IRTLM_FI(NIN)%P(2,NN+IDEB(NIN))
               BBUFS(L+2) =IRTLM_FI(NIN)%P(3,NN+IDEB(NIN))
               BBUFS(L+3) =IRTLM_FI(NIN)%P(4,NN+IDEB(NIN))
               BBUFS(L+4) =TIME_SFI(NIN)%P(2*(NN+IDEB(NIN)-1)+1)
               BBUFS(L+5) =TIME_SFI(NIN)%P(2*(NN+IDEB(NIN)-1)+2)
               L=L+ALEN
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
    
      ALEN=6

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

             IF(NTY==25)THEN

               NB = NSNSI(NIN)%P(P)
               IF (NB > 0)THEN
C
                 DO K=1,NB
                   ND = NSVSI(NIN)%P(IDEB(NIN)+K)

C Merge IRTLM & TIME_S
                   SN = INTBUF_TAB(NIN)%NSV(ND)
                   TIME_S_1  = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1)
                   TIME_S_2  = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2)
                   SURF      = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)
                   KLEAVE    = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)
                   SURFR     = NINT(BBUFR(IADR(P)+L))
                   SUBTRIAR  = NINT(BBUFR(IADR(P)+L+1))
                   KLEAVE_R  = NINT(BBUFR(IADR(P)+L+2))
                   PROC_R    = NINT(BBUFR(IADR(P)+L+3))
                   TIME_SR_1 = BBUFR(IADR(P)+L+4)
                   TIME_SR_2 = BBUFR(IADR(P)+L+5)

                   IF(KLEAVE == -1)THEN

                   ELSEIF(KLEAVE_R == -1)THEN
C
C                    IRTLM has been reset after main segment deletion (Idel)
                     INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = 0
                     INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = 0
                     INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = -1
                     INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R

                   ELSEIF (SURF > 0)THEN

                     IF(TIME_S_1 == EP20)THEN

                        IF(SURFR > 0 .AND. TIME_SR_1 /= EP20)THEN

c                         IF( TIME_SR_2 == EP20 .OR. 
c     .                      (TIME_SR_2 /= EP20 .AND. TIME_S_2 == TIME_SR_2 .AND. SURFR > SURF))THEN
c                              INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
c                              INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
c                              INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
c                              INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
c                              INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
c                              INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c                          ELSEIF(ABS(TIME_S_2) > ABS(TIME_SR_2))THEN
                            INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                            INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                            INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                            INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
                            INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
                            INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c                          END IF 

c inutile
c                       elseif(SURFR > 0 .AND. TIME_SR_1 == EP20 .AND. KLEAVE_R > 0)THEN
c
c                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
c                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
c                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
c                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
c                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
c                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c
                        END IF

                     ELSE ! TIME_S_1 /= EP20

                       IF(SURFR > 0 .AND. TIME_SR_1 /= EP20 .AND. TIME_SR_2 /= EP20)THEN

                        IF(TIME_S_2 == TIME_SR_2)THEN
                           IF(SURFR > SURF)THEN
                             INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                             INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                             INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                             INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                            INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
C                            INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2 == TIME_S_2
                           END IF
                         ELSEIF(TIME_S_2 > TIME_SR_2)THEN
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                          INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
                           INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
                         END IF 

                       END IF
                     END IF
                   ELSE ! SURF <= 0
                     IF(SURFR < 0)THEN
                       IF(TIME_S_1 == TIME_SR_1)THEN
                         IF(-SURFR > -SURF)THEN
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                          INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
                         END IF
                       ELSEIF(TIME_SR_1 > TIME_S_1)THEN
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
                       END IF
                     END IF
                   END IF
C
                   L=L+ALEN
c         if(itab(sn)==29376)print *,'recoit nsnsi apr',loc_proc,p,INTBUF_TAB(NIN)%IRTLM(4*ND-3:4*ND)

                  ENDDO
               ENDIF
               IDEB(NIN)=IDEB(NIN)+NB
             ENDIF ! ity==25
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
C-----------------------------------------------------------
C 2eme partie - echanges sur les noeuds seconds frontieres
C               pour toutes les interface type 25.
C-----------------------------------------------------------
      IADS(1:NSPMD+1)=0

      DO I=1,NSPMD
        ASSERT(IAD_I25(1,I) >= 0)
        IADS(I)=IAD_I25(1,I)
      ENDDO
      IADS(NSPMD+1)=SFR_I25+1

c     print *,'iads',ispmd+1,iads(1),iads(2),iads(3)
C Preparation du send
      ILEN=5
      RLEN=2
      ASSERT(SFR_I25 >= 0) 
      ALLOCATE(ISENDBUF(ILEN,SFR_I25))
      ALLOCATE(IRECBUF(ILEN*SFR_I25))
      ALLOCATE(RSENDBUF(RLEN,SFR_I25))
      ALLOCATE(RRECBUF(RLEN*SFR_I25))

C mise en place du irecieve
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        ASSERT(SIZ >= 0)
        IF(SIZ/=0)THEN
          LI = (IADS(P)-1)*ILEN+1
          LR = (IADS(P)-1)*RLEN+1
          MSGTYP = MSGOFF3
          LEN = SIZ*ILEN
c           print *,'recept attend entier',ispmd+1,p,len
          CALL MPI_IRECV(
     S      IRECBUF(LI),LEN,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(P),IERROR)

          MSGTYP = MSGOFF4 
          LEN = SIZ*RLEN
c           print *,'recept attend reel',ispmd+1,p,len
          CALL MPI_IRECV(
     S      RRECBUF(LR),LEN,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R2(P),IERROR)

        ENDIF
       ENDDO

       NB = 1
       DO P = 1, NSPMD
          DO NI=1,NBINTC
             NIN=INTLIST(NI)
             NTY   = IPARI(7,NIN)
             NSN   = IPARI(5,NIN)
             IF(NTY==25) THEN

               DO I=IAD_I25(NI,P),IAD_I25(NI+1,P)-1
                
                 ND = FR_I25(I)
                 ASSERT(ND > 0)
                 ASSERT(ND <= NSN)
                 SN = INTBUF_TAB(NIN)%NSV(ND)

                 ISENDBUF(1,NB) = ITAB(SN)
                 ISENDBUF(2,NB) = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)
                 ISENDBUF(3,NB) = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)
                 ISENDBUF(4,NB) = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)
                 ISENDBUF(5,NB) = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)
                 RSENDBUF(1,NB) = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1)
                 RSENDBUF(2,NB) = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2)
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
     S        ISENDBUF(1,L),SIZ*ILEN,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(P),IERROR)
c           print *,'envoi entier',ispmd+1,p,siz*2
     
           MSGTYP = MSGOFF4
           CALL MPI_ISEND(
     S        RSENDBUF(1,L),SIZ*RLEN,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S2(P),IERROR)
c           print *,'envoi reel',ispmd+1,p,siz*8
         ENDIF   ! IF (SIZ >0)
      ENDDO      ! DO P=1,NSPMD
C--------------------------------------------------------------------

      RETURN
      ENDIF
      
C ----------------------------------      
C IFLAG=3 partie3 - Recieve 
C ----------------------------------      
      IF(FLAG==3)THEN
      
      ILEN = 5
      RLEN = 2
C Reception
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        IF(SIZ/=0)THEN
          IDB = IADS(P)
          CALL MPI_WAIT(REQ_R (P),STATUS,IERROR)
c           print *,'recept recu entiers',ispmd+1,p

          CALL MPI_WAIT(REQ_R2(P),STATUS,IERROR)
c           print *,'recept recu reels',ispmd+1,p

C Traitements
        
         DO NI=1,NBINTC
           NIN = INTLIST(NI)
            
           NTY   = IPARI(7,NIN)
           NSN   = IPARI(5,NIN)
           IF (NTY == 25)THEN

             DO K=IAD_I25(NI,P),IAD_I25(NI+1,P)-1 
                ND = FR_I25(K)
                SN = INTBUF_TAB(NIN)%NSV(ND)

                TIME_S_1  = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1)
                TIME_S_2  = INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2)
                SURF      = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)
                KLEAVE    = INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)
                SURFR     = IRECBUF((IDB-1)*ILEN+2)
                SUBTRIAR  = IRECBUF((IDB-1)*ILEN+3)
                KLEAVE_R  = IRECBUF((IDB-1)*ILEN+4)
                PROC_R    = IRECBUF((IDB-1)*ILEN+5)
                TIME_SR_1 = RRECBUF((IDB-1)*RLEN+1)
                TIME_SR_2 = RRECBUF((IDB-1)*RLEN+2)
c        if(itab(sn)==29376)print *,'recoit avant',loc_proc,p,INTBUF_TAB(NIN)%IRTLM(4*ND-3:4*ND)

                IF(KLEAVE == -1)THEN

                ELSEIF(KLEAVE_R == -1)THEN
C
C                 IRTLM has been reset after main segment deletion (Idel)
                  INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = 0
                  INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = 0
                  INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = -1
                  INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R

                ELSEIF (SURF > 0)THEN

                  IF(TIME_S_1 == EP20)THEN

                     IF(SURFR > 0 .AND. TIME_SR_1 /= EP20)THEN

c                      IF( TIME_SR_2 == EP20 .OR. 
c     .                   (TIME_SR_2 /= EP20 .AND. TIME_S_2 == TIME_SR_2 .AND. SURFR > SURF))THEN
c                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
c                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
c                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
c                           INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
c                           INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
c                           INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c                       ELSEIF(ABS(TIME_S_2) > ABS(TIME_SR_2))THEN
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                         INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c                       END IF 


c inutile
c                     elseif(SURFR > 0 .AND. TIME_SR_1 == EP20 .AND. KLEAVE_R > 0)THEN
cC
cC                      le nd est ressorti du contact ou il etait precedemment sur le proc remote
cC                                                                             ------------------
c                       INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
c                       INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
c                       INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
c                       INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
c                       INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
c                       INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
c
                     END IF

                  ELSE ! TIME_S_1 /= EP20

                    IF(SURFR > 0 .AND. TIME_SR_1 /= EP20 .AND. TIME_SR_2 /= EP20)THEN

                      IF(TIME_S_2 == TIME_SR_2)THEN
                        IF(SURFR > SURF)THEN
                          INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                          INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                          INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                          INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
C                         INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2 == TIME_S_2
                        END IF
                      ELSEIF(ABS(TIME_S_2) > ABS(TIME_SR_2))THEN
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                       INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
                        INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2) = TIME_SR_2
                      END IF 
                    END IF

                  END IF

                ELSE ! SURF <= 0

                  IF(SURFR < 0)THEN
                    IF(TIME_S_1 == TIME_SR_1)THEN
                      IF(-SURFR > -SURF)THEN
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                        INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
C                       INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1 == TIME_S_1
                      END IF
                    ELSEIF(TIME_SR_1 > TIME_S_1)THEN
                      INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)  = SURFR
                      INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)  = SUBTRIAR
                      INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)  = KLEAVE_R
                      INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)  = PROC_R
                      INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1) = TIME_SR_1
                    END IF
                  END IF

                END IF
C
c        if(itab(sn)==29376)print *,'recoit apres',loc_proc,p,INTBUF_TAB(NIN)%IRTLM(4*ND-3:4*ND)
     
               IDB=IDB+1
             ENDDO        ! K=,IAD_I25(NI,P),IAD_I25(NI+1,P)-1 
           ENDIF          ! IF (NTY == 25)THEN
         ENDDO            ! DO NI=1,NBINTC
        ENDIF             ! IF(SIZ/=0)THEN
      ENDDO               ! DO P=1,NSPMD

C Fin send
      DO P=1,NSPMD
        SIZ = IADS(P+1)-IADS(P)
        IF(SIZ/=0)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
          CALL MPI_WAIT(REQ_S2(P),STATUS,IERROR)
        ENDIF
      ENDDO

      IF(ALLOCATED(ISENDBUF))DEALLOCATE(ISENDBUF)
      IF(ALLOCATED(IRECBUF))DEALLOCATE(IRECBUF)
      IF(ALLOCATED(RSENDBUF))DEALLOCATE(RSENDBUF)
      IF(ALLOCATED(RRECBUF))DEALLOCATE(RRECBUF)

C ------------------------------------------------------------------
C 3e partie on renvoie les valeurs globalisees sur les procs remote
C ------------------------------------------------------------------
      LOC_PROC = ISPMD+1
      IADS = 0
      IADR = 0
      LENSD = 0
      LENRV = 0

      ALEN=6
C Comptage des tailles de buffer  Receeption et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NI=1,NBINTC
           NIN = INTLIST(NI)
           NTY=IPARI(7,NIN)
           IF(NTY==25) THEN
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
           IF(NTY==25)THEN
               NB = NSNSI(NIN)%P(P)
C Preparation du send
               DO NN=1,NB
                 ND = NSVSI(NIN)%P(IDEB(NIN)+NN)
                 NOD=INTBUF_TAB(NIN)%NSV(ND)
c        if(itab(sn)==29376)print *,'broadcast nsnsi',loc_proc,p,INTBUF_TAB(NIN)%IRTLM(4*ND-3:4*ND)
                 BBUFS(L  )=INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+1)
                 BBUFS(L+1)=INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+2)
                 BBUFS(L+2)=INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+3)
                 BBUFS(L+3)=INTBUF_TAB(NIN)%IRTLM(4*(ND-1)+4)
                 BBUFS(L+4)=INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+1)
                 BBUFS(L+5)=INTBUF_TAB(NIN)%TIME_S(2*(ND-1)+2)
                 L = L + ALEN
               ENDDO
               IDEB(NIN)=IDEB(NIN)+NB
           ENDIF
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

      RETURN
      ENDIF
      
C ----------------------------------      
C IFLAG=4 partie4 - Recieve 
C ----------------------------------      
      IF(FLAG==4)THEN
     
      ALEN=6
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
             NTY   =IPARI(7,NIN)

             IF(NTY==25) THEN
               NB = NSNFI(NIN)%P(P)

               IF (NB > 0)THEN
                 DO K=1,NB
c        if(itafi(nin)%p(ideb(nin)+k)==29376)print *,'recoit nsnfi',loc_proc,p,
c     .                       BBUFR(IADR(P)+L:IADR(P)+L+3)
                   IRTLM_FI(NIN)%P(1,IDEB(NIN)+K)=BBUFR(IADR(P)+L)
                   IRTLM_FI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+1)
                   IRTLM_FI(NIN)%P(3,IDEB(NIN)+K)=BBUFR(IADR(P)+L+2)
                   IRTLM_FI(NIN)%P(4,IDEB(NIN)+K)=BBUFR(IADR(P)+L+3)
                   TIME_SFI(NIN)%P(2*(IDEB(NIN)+K-1)+1) =BBUFR(IADR(P)+L+4)
                   TIME_SFI(NIN)%P(2*(IDEB(NIN)+K-1)+2) =BBUFR(IADR(P)+L+5)
                   L=L+ALEN
                 ENDDO
               ENDIF
               IDEB(NIN)=IDEB(NIN)+NB
             ENDIF
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


C     WRITE(6,*) __FILE__,__LINE__
      IF(ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
      IF(ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)
      IF(ALLOCATED( REQ_SI )) DEALLOCATE(REQ_SI)
      IF(ALLOCATED( REQ_RI )) DEALLOCATE(REQ_RI)
      IF(ALLOCATED( REQ_S  )) DEALLOCATE(REQ_S)
      IF(ALLOCATED( REQ_S2 )) DEALLOCATE(REQ_S2)
      IF(ALLOCATED( REQ_R  )) DEALLOCATE(REQ_R)
      IF(ALLOCATED( REQ_R2 )) DEALLOCATE(REQ_R2)
      IF(ALLOCATED( IADR   )) DEALLOCATE(IADR)
      IF(ALLOCATED( IADS   )) DEALLOCATE(IADS)

      ENDIF    ! fi iflag=4
#endif
      RETURN
      END
