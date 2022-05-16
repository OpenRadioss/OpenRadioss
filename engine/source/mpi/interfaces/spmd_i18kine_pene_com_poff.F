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
C
Chd|====================================================================
Chd|  SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I18KINE_PENE_COM_POFF(IPARI,INTBUF_TAB,FCONT,
     *                MTF,A,IAD_ELEM,FR_ELEM,MODE,SLVNDTAG,TAGPENE,ITAB,
     .                H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),IAD_ELEM(2,*),FR_ELEM(*),
     *        SLVNDTAG(*),TAGPENE(*),ITAB(*),MODE
C
      my_real
     .  MTF(14,*),A(3,*),FCONT(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),
     *        REQ_SI(NSPMD),REQ_RI(NSPMD)
      INTEGER P,LENSD,LENRV,IADS(NSPMD+1),IADR(NSPMD+1),IERROR,
     *        SIZ,LOC_PROC,MSGTYP,IDEB(NINTER), MSGOFF, MSGOFF2
      INTEGER NIN,NTY,INACTI
      INTEGER J,L,NB,NN,K,N,NOD,LEN,ALEN,ND,FLG
      my_real ,
     *   DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      DATA MSGOFF/148/
      DATA MSGOFF2/149/
C-----------------------------------------------
C Sur la type 18KINE, il y a 3 comm qui sont bases
C sur le meme schemas avec des
C donn  es differentes
C MODE =
C   1 : PENE + PENEMIN
C   2 : Vitesses noeuds seconds
C   3 : Accelerations noeuds seconds

      LOC_PROC = ISPMD+1
      IADS = 0
      IADR = 0
      LENSD = 0
      LENRV = 0

      IF(MODE==1)THEN
        ALEN=5
      ELSEIF(MODE==2)THEN
        ALEN=3
      ELSEIF(MODE==3)THEN
        ALEN=7
      ENDIF
C Comptage des tailles de buffer  Receeption et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NIN=1,NINTER
           NTY=IPARI(7,NIN)
           INACTI =IPARI(22,NIN)
           IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .         (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7))THEN
              LENSD = LENSD + NSNFI(NIN)%P(P)*ALEN
              LENRV = LENRV + NSNSI(NIN)%P(P)*ALEN
           ENDIF
         ENDDO
      ENDDO
      IADR(NSPMD+1)=LENRV+1

      IF(LENSD.GT.0)THEN
          ALLOCATE(BBUFS(LENSD),STAT=IERROR)
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF

C Preparation du recieve
      IF(LENRV.GT.0)THEN
          ALLOCATE(BBUFR(LENRV),STAT=IERROR)
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
      ENDIF
C Send
      L=1
      IDEB=0
      DO P=1, NSPMD
       IADS(P)=L
       IF (P/= LOC_PROC) THEN
         DO NIN=1,NINTER
           NTY   =IPARI(7,NIN)
           INACTI =IPARI(22,NIN)
           IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .        (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7)) THEN
C Preparation du send
             NB = NSNFI(NIN)%P(P)
             IF (MODE==1)THEN
               DO NN=1,NB
                 BBUFS(L)=     MTFI_PENE(NIN)%P(NN+IDEB(NIN))
                 BBUFS(L+1)=MTFI_PENEMIN(NIN)%P(NN+IDEB(NIN))
                 BBUFS(L+2)=MTFI_N(NIN)%P(1,NN+IDEB(NIN))
                 BBUFS(L+3)=MTFI_N(NIN)%P(2,NN+IDEB(NIN))
                 BBUFS(L+4)=MTFI_N(NIN)%P(3,NN+IDEB(NIN))
                 L=L+5
               ENDDO
               IDEB(NIN)=IDEB(NIN)+NB

             ELSEIF (MODE==2)THEN
               DO NN=1,NB
                 BBUFS(L  )=MTFI_V(NIN)%P(1,NN+IDEB(NIN))
                 BBUFS(L+1)=MTFI_V(NIN)%P(2,NN+IDEB(NIN))
                 BBUFS(L+2)=MTFI_V(NIN)%P(3,NN+IDEB(NIN))
c                 BBUFS(L+3)=MTFI_V(NIN)%P(4,NN+IDEB(NIN))
c                 BBUFS(L+4)=MTFI_V(NIN)%P(5,NN+IDEB(NIN))
c                 BBUFS(L+5)=MTFI_V(NIN)%P(6,NN+IDEB(NIN))
                 L=L+3
               ENDDO
                 IDEB(NIN)=IDEB(NIN)+NB
             ELSEIF (MODE==3)THEN
                DO NN=1,NB
                 BBUFS(L  )=MTFI_A(NIN)%P(1,NN+IDEB(NIN))
                 BBUFS(L+1)=MTFI_A(NIN)%P(2,NN+IDEB(NIN))
                 BBUFS(L+2)=MTFI_A(NIN)%P(3,NN+IDEB(NIN))
                 BBUFS(L+3)=MTFI_A(NIN)%P(4,NN+IDEB(NIN))
                 BBUFS(L+4)=MTFI_A(NIN)%P(5,NN+IDEB(NIN))
                 BBUFS(L+5)=MTFI_A(NIN)%P(6,NN+IDEB(NIN))
                 BBUFS(L+6)=MTFI_A(NIN)%P(7,NN+IDEB(NIN))
                 L=L+7
               ENDDO
               IDEB(NIN)=IDEB(NIN)+NB
             ENDIF
           ENDIF
         ENDDO
         SIZ = L-IADS(P)
         IF(SIZ.GT.0)THEN
              MSGTYP = MSGOFF 
C Send
              CALL MPI_ISEND(
     .          BBUFS(IADS(P)),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
         ENDIF
       ENDIF
      ENDDO
C Recieve
      L=0
      IDEB = 0
      DO P=1, NSPMD
          L=0
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
            MSGTYP = MSGOFF

C Send
            CALL MPI_RECV( BBUFR(IADR(P)),SIZ,REAL,IT_SPMD(P),MSGTYP,
     *                     MPI_COMM_WORLD,STATUS,IERROR )
           DO NIN=1,NINTER
             NTY   =IPARI(7,NIN)
             INACTI =IPARI(22,NIN)

             IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .          (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7))THEN
               NB = NSNSI(NIN)%P(P)
               IF (NB > 0)THEN
C
                IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.22)THEN
                  IF(MODE==1)THEN
                    DO K=1,NB
                      ND = NSVSI(NIN)%P(IDEB(NIN)+K)
                      NOD=INTBUF_TAB(NIN)%NSV(ND)
                      MTF(10,NOD) = MTF(10,NOD)+ BBUFR(IADR(P)+L)
                      IF(BBUFR(IADR(P)+L+1) > MTF(11,NOD))THEN
                        MTF(11,NOD) = BBUFR(IADR(P)+L+1)
                        TAGPENE(NOD) = P
                      ENDIF
c                      MTF(11,NOD) = MAX(MTF(11,NOD),BBUFR(IADR(P)+L+1))
                      MTF(12,NOD) = MTF(12,NOD)+BBUFR(IADR(P)+L+2)
                      MTF(13,NOD) = MTF(13,NOD)+BBUFR(IADR(P)+L+3)
                      MTF(14,NOD) = MTF(14,NOD)+BBUFR(IADR(P)+L+4)
                      L=L+5
                    ENDDO
                  ELSEIF(MODE==2)THEN
                    DO K=1,NB
                      ND = NSVSI(NIN)%P(IDEB(NIN)+K)
                      NOD=INTBUF_TAB(NIN)%NSV(ND)
c                      IF(BBUFR(IADR(P)+L) /= 0)THEN
                        MTF(1,NOD) = MTF(1,NOD)+BBUFR(IADR(P)+L)
                        MTF(2,NOD) = MTF(2,NOD)+BBUFR(IADR(P)+L+1)
                        MTF(3,NOD) = MTF(3,NOD)+BBUFR(IADR(P)+L+2)
c                        MTF(4,NOD) = BBUFR(IADR(P)+L+3)
c                        MTF(5,NOD) = BBUFR(IADR(P)+L+4)
c                        MTF(6,NOD) = BBUFR(IADR(P)+L+5)
c                      ENDIF
                      L=L+3
                    ENDDO
                  ELSEIF(MODE==3)THEN
                    DO K=1,NB
                      ND = NSVSI(NIN)%P(IDEB(NIN)+K)
                      NOD=INTBUF_TAB(NIN)%NSV(ND)
                      IF(BBUFR(IADR(P)+L+6) /= 0)THEN
                         A(1,NOD) = BBUFR(IADR(P)+L)
                         A(2,NOD) = BBUFR(IADR(P)+L+1)
                         A(3,NOD) = BBUFR(IADR(P)+L+2)
                         IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .                    (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .                    (MANIM.GE.4.AND.MANIM.LE.15)))THEN
                           IF(INCONV .EQ. 1) THEN
                             FCONT(1,NOD) = FCONT(1,NOD)+BBUFR(IADR(P)+L+3)
                             FCONT(2,NOD) = FCONT(2,NOD)+BBUFR(IADR(P)+L+4)
                             FCONT(3,NOD) = FCONT(3,NOD)+BBUFR(IADR(P)+L+5)
                           ENDIF
                         ENDIF
                         SLVNDTAG(NOD)=1
                      ENDIF
                      L=L+7
                    ENDDO
                  ENDIF
                ENDIF
               ENDIF
             ENDIF
             IDEB(NIN)=IDEB(NIN)+NB
           ENDDO
          ENDIF
          L=L+SIZ
      ENDDO

C Fin du send
      DO P = 1, NSPMD
          IF (P==NSPMD)THEN
             SIZ=LENSD-IADS(P)
          ELSE
             SIZ=IADS(P+1)-IADS(P)
          ENDIF
          IF(SIZ.GT.0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO

      IF (ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
      IF (ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)

C--------------------------------------------------
C 2eme partie - echanges sur les noeuds frontieres
C--------------------------------------------------
      IF(MODE==1)THEN
        LEN=5
      ELSEIF(MODE==2)THEN
        LEN=6
      ELSEIF(MODE==3)THEN
        LEN=4
      ENDIF
      LENRV = (IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))*LEN

      ALLOCATE(BBUFS(LENRV))
      ALLOCATE(BBUFR(LENRV))

      IADR(1) = 1
      L=1
      DO P=1,NSPMD
        SIZ = (IAD_ELEM(1,P+1)-IAD_ELEM(1,P))*LEN
        IF(SIZ.NE.0)THEN
          MSGTYP = MSGOFF2
          CALL MPI_IRECV(
     S      BBUFR(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_RI(P),IERROR)
            L = L + SIZ
        ENDIF
        IADR(P+1) = L
      END DO


C Remplis le Buffer
      L=1
      DO P=1,NSPMD
        IADS(P)=L
        DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
          NOD = FR_ELEM(J)
          IF(MODE==1)THEN
            BBUFS(L)=MTF(10,NOD)
            BBUFS(L+1)=MTF(11,NOD)
            BBUFS(L+2)=MTF(12,NOD)
            BBUFS(L+3)=MTF(13,NOD)
            BBUFS(L+4)=MTF(14,NOD)
            L=L+5
          ELSEIF(MODE==2)THEN
            BBUFS(L)  =MTF(1,NOD)
            BBUFS(L+1)=MTF(2,NOD)
            BBUFS(L+2)=MTF(3,NOD)
C MTF(4-6) ne sont pas des valeurs de cumuls initialises dans les parties
c ou l on en a besoin. Inutile de les communiquer
c            BBUFS(L+3)=MTF(4,NOD)
c            BBUFS(L+4)=MTF(5,NOD)
c            BBUFS(L+5)=MTF(6,NOD)
            L=L+3
          ELSEIF(MODE==3)THEN
            BBUFS(L)=A(1,NOD)
            BBUFS(L+1)=A(2,NOD)
            BBUFS(L+2)=A(3,NOD)
            BBUFS(L+3)=SLVNDTAG(NOD)
            L=L+4
          ENDIF
        ENDDO
      ENDDO
      IADS(NSPMD+1)=L
C
C--------------------------------------------------------------------
C   echange messages
C

      DO P=1,NSPMD
       IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P).GT.0)THEN
          MSGTYP = MSGOFF2
          SIZ = IADS(1+P)-IADS(P)
          L = IADS(P)
          CALL MPI_ISEND(
     S      BBUFS(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_SI(P),IERROR)
       ENDIF
      ENDDO
C--------------------------------------------------------------------
C Reception
      DO P = 1, NSPMD
        NB = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
        IF(NB.GT.0)THEN
          CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
          L = IADR(P)
          DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
             NOD = FR_ELEM(J)
             IF(MODE==1)THEN
               MTF(10,NOD) = MTF(10,NOD)+BBUFR(L)
               IF(BBUFR(L+1) > ABS(MTF(11,NOD)))THEN
                 MTF(11,NOD) = BBUFR(L+1)
                 TAGPENE(NOD) = P
               ELSEIF(BBUFR(L+1) == ABS(MTF(11,NOD)) .and.
     .                ISPMD+1 > P)THEN
               ELSE
                 MTF(11,NOD) =  ABS(BBUFR(L+1)*(1-EM6))
               ENDIF
               MTF(12,NOD) = MTF(12,NOD)+BBUFR(L+2)
               MTF(13,NOD) = MTF(13,NOD)+BBUFR(L+3)
               MTF(14,NOD) = MTF(14,NOD)+BBUFR(L+4)
               L=L+5
             ELSEIF(MODE==2)THEN
               MTF(1,NOD)=MTF(1,NOD)+BBUFR(L)
               MTF(2,NOD)=MTF(2,NOD)+BBUFR(L+1)
               MTF(3,NOD)=MTF(3,NOD)+BBUFR(L+2)
C MTF(4-6) ne sont pas des valeurs de cumuls initialises dans les parties
c ou l on en a besoin. Inutile de les communiquer
c               MTF(4,NOD)=BBUFR(L+3)
c               MTF(5,NOD)=BBUFR(L+4)
c               MTF(6,NOD)=BBUFR(L+5)
               L=L+3
             ELSEIF(MODE==3)THEN
               FLG=NINT(BBUFR(L+3))
               IF(FLG==1)THEN
                 A(1,NOD)=BBUFR(L)
                 A(2,NOD)=BBUFR(L+1)
                 A(3,NOD)=BBUFR(L+2)
               ENDIF
               L=L+4
             ENDIF
          ENDDO
        ENDIF
      ENDDO

C Fin du send
      DO P = 1, NSPMD
          SIZ=IADS(P+1)-IADS(P)
          IF(SIZ.GT.0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO

#endif
      RETURN
C-----------------------------------------------
      END
