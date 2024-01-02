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
C
Chd|====================================================================
Chd|  SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CONVERSION7                   source/mpi/interfaces/spmd_i7tool.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI20BOX(NSV     ,NSN     ,XA     ,VA   ,MS      ,
     2                         BMINMAL ,WEIGHT  ,STFA   ,NIN  ,ISENDTO ,
     3                         IRCVFROM,IAD_ELEM,FR_ELEM,NSNR ,IGAP    ,
     4                         GAP_S   ,ITAB    ,KINET  ,IFQ  ,INACTI  ,
     5                         NSNFIOLD,INTTH   ,IELEC  ,AREAS,TEMP    ,
     6                         NUM_IMP ,NLG     ,PENIS  ,PENIA,DIAG_SMS,
     7                         NODNX_SMS,NBINFLG,DXANC  ,DVANC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "timeri_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),NSNR,
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELEC(*),NUM_IMP,NLG(*), NODNX_SMS(*), NBINFLG(*)
      my_real
     .        XA(3,*), VA(3,*), MS(*), BMINMAL(*), STFA(*), GAP_S(*),
     .        AREAS(*),TEMP(*), PENIS(2,*), PENIA(5,*), DIAG_SMS(*),
     .        DXANC(3,*),DVANC(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,IL,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),NBOX(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3
      DATA MSGOFF/132/
      DATA MSGOFF2/133/
      DATA MSGOFF3/134/

      my_real
     .        BMINMA(6,NSPMD), RATIO
      TYPE(r8_pointer), DIMENSION(NSPMD) :: BUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0
     .   .OR.NUM_IMP>0) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
         END DO
      END IF
      LOC_PROC = ISPMD + 1
C
C   boite minmax pour le tri provenant de i20buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC)==0.AND.
     .   ISENDTO(NIN,LOC_PROC)==0) RETURN
      IF (IMONM > 0) CALL STARTIME(25,1)
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
C   envoi boite
C
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C   reception des boites min-max
C
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              MSGTYP = MSGOFF
              NBIRECV=NBIRECV+1
              IRINDEXI(NBIRECV)=P
              CALL MPI_IRECV(
     .          BMINMA(1,P)   ,6              ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RB(NBIRECV),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C   envoi de XREM
C
      IF(IGAP==0) THEN
        SIZ = 18
      ELSE
        SIZ = 19
      ENDIF
      IF(INTTH > 0 ) SIZ = SIZ + 3
      IF(INACTI==5.OR.INACTI==6) SIZ = SIZ + 7
      IF(IDTMINS > 0 ) SIZ = SIZ + 1
      IF(IDTMINS == 2)THEN
        SIZ = SIZ + 2
      ELSEIF(IDTMINS_INT/=0)THEN
        SIZ = SIZ + 1
      END IF
      IDEB = 1
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
C Traitement special sur d.d. ne consever que les noeuds internes
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C weight < 0 temporairement pour ne conserver que les noeuds non frontiere
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
          L = IDEB
          NBOX(P) = 0
          NB = 0
          DO I=1,NSN
            IL = NSV(I)
            NOD = NLG(IL)
            IF(WEIGHT(NOD)==1)THEN
             IF(STFA(IL)>ZERO)THEN
              IF(XA(1,IL)<=BMINMA(1,P)) THEN
               IF(XA(1,IL)>=BMINMA(4,P)) THEN
                IF(XA(2,IL)<=BMINMA(2,P)) THEN
                 IF(XA(2,IL)>=BMINMA(5,P)) THEN
                  IF(XA(3,IL)<=BMINMA(3,P)) THEN
                   IF(XA(3,IL)>=BMINMA(6,P)) THEN
                     NB = NB + 1
                     INDEX(NB) = I
                   ENDIF
                  ENDIF
                 ENDIF
                ENDIF
               ENDIF
              ENDIF
             ENDIF
            ENDIF
          ENDDO
          NBOX(P) = NB
C
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
C Envoi taille msg
C
          MSGTYP = MSGOFF2
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB>0) THEN
           ALLOCATE(BUF(P)%P(SIZ*NB),STAT=IERROR)
           IF(IERROR/=0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
           L = 0
C
           IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
               IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= DXANC(1,IL)
                   BUF(P)%p(L+14)= DXANC(2,IL)
                   BUF(P)%p(L+15)= DXANC(3,IL)
                   BUF(P)%p(L+16)= DVANC(1,IL)
                   BUF(P)%p(L+17)= DVANC(2,IL)
                   BUF(P)%p(L+18)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= DIAG_SMS(NOD)
                   BUF(P)%p(L+14)= DXANC(1,IL)
                   BUF(P)%p(L+15)= DXANC(2,IL)
                   BUF(P)%p(L+16)= DXANC(3,IL)
                   BUF(P)%p(L+17)= DVANC(1,IL)
                   BUF(P)%p(L+18)= DVANC(2,IL)
                   BUF(P)%p(L+19)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
              ELSE
               IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= DXANC(1,IL)
                   BUF(P)%p(L+15)= DXANC(2,IL)
                   BUF(P)%p(L+16)= DXANC(3,IL)
                   BUF(P)%p(L+17)= DVANC(1,IL)
                   BUF(P)%p(L+18)= DVANC(2,IL)
                   BUF(P)%p(L+19)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= DIAG_SMS(NOD)
                   BUF(P)%p(L+15)= DXANC(1,IL)
                   BUF(P)%p(L+16)= DXANC(2,IL)
                   BUF(P)%p(L+17)= DXANC(3,IL)
                   BUF(P)%p(L+18)= DVANC(1,IL)
                   BUF(P)%p(L+19)= DVANC(2,IL)
                   BUF(P)%p(L+20)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
              ENDIF
C + la thermique
             ELSE
              IF(IGAP==0) THEN
               IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= DXANC(1,IL)
                   BUF(P)%p(L+17)= DXANC(2,IL)
                   BUF(P)%p(L+18)= DXANC(3,IL)
                   BUF(P)%p(L+19)= DVANC(1,IL)
                   BUF(P)%p(L+20)= DVANC(2,IL)
                   BUF(P)%p(L+21)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= DIAG_SMS(NOD)
                   BUF(P)%p(L+17)= DXANC(1,IL)
                   BUF(P)%p(L+18)= DXANC(2,IL)
                   BUF(P)%p(L+19)= DXANC(3,IL)
                   BUF(P)%p(L+20)= DVANC(1,IL)
                   BUF(P)%p(L+21)= DVANC(2,IL)
                   BUF(P)%p(L+22)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
              ELSE
               IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= DXANC(1,IL)
                   BUF(P)%p(L+18)= DXANC(2,IL)
                   BUF(P)%p(L+19)= DXANC(3,IL)
                   BUF(P)%p(L+20)= DVANC(1,IL)
                   BUF(P)%p(L+21)= DVANC(2,IL)
                   BUF(P)%p(L+22)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= DIAG_SMS(NOD)
                   BUF(P)%p(L+18)= DXANC(1,IL)
                   BUF(P)%p(L+19)= DXANC(2,IL)
                   BUF(P)%p(L+20)= DXANC(3,IL)
                   BUF(P)%p(L+21)= DVANC(1,IL)
                   BUF(P)%p(L+22)= DVANC(2,IL)
                   BUF(P)%p(L+23)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
              ENDIF
             ENDIF
            ELSE    ! INACTI = 5 or 6
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
               IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= PENIS(1,I)
                   BUF(P)%p(L+14)= PENIS(2,I)
                   BUF(P)%p(L+15)= PENIA(1,IL)
                   BUF(P)%p(L+16)= PENIA(2,IL)
                   BUF(P)%p(L+17)= PENIA(3,IL)
                   BUF(P)%p(L+18)= PENIA(4,IL)
                   BUF(P)%p(L+19)= PENIA(5,IL)
                   BUF(P)%p(L+20)= DXANC(1,IL)
                   BUF(P)%p(L+21)= DXANC(2,IL)
                   BUF(P)%p(L+22)= DXANC(3,IL)
                   BUF(P)%p(L+23)= DVANC(1,IL)
                   BUF(P)%p(L+24)= DVANC(2,IL)
                   BUF(P)%p(L+25)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= PENIS(1,I)
                   BUF(P)%p(L+14)= PENIS(2,I)
                   BUF(P)%p(L+15)= PENIA(1,IL)
                   BUF(P)%p(L+16)= PENIA(2,IL)
                   BUF(P)%p(L+17)= PENIA(3,IL)
                   BUF(P)%p(L+18)= PENIA(4,IL)
                   BUF(P)%p(L+19)= PENIA(5,IL)
                   BUF(P)%p(L+20)= DIAG_SMS(NOD)
                   BUF(P)%p(L+21)= DXANC(1,IL)
                   BUF(P)%p(L+22)= DXANC(2,IL)
                   BUF(P)%p(L+23)= DXANC(3,IL)
                   BUF(P)%p(L+24)= DVANC(1,IL)
                   BUF(P)%p(L+25)= DVANC(2,IL)
                   BUF(P)%p(L+26)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
               ELSE
                IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= PENIS(1,I)
                   BUF(P)%p(L+15)= PENIS(2,I)
                   BUF(P)%p(L+16)= PENIA(1,IL)
                   BUF(P)%p(L+17)= PENIA(2,IL)
                   BUF(P)%p(L+18)= PENIA(3,IL)
                   BUF(P)%p(L+19)= PENIA(4,IL)
                   BUF(P)%p(L+20)= PENIA(5,IL)
                   BUF(P)%p(L+21)= DXANC(1,IL)
                   BUF(P)%p(L+22)= DXANC(2,IL)
                   BUF(P)%p(L+23)= DXANC(3,IL)
                   BUF(P)%p(L+24)= DVANC(1,IL)
                   BUF(P)%p(L+25)= DVANC(2,IL)
                   BUF(P)%p(L+26)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= PENIS(1,I)
                   BUF(P)%p(L+15)= PENIS(2,I)
                   BUF(P)%p(L+16)= PENIA(1,IL)
                   BUF(P)%p(L+17)= PENIA(2,IL)
                   BUF(P)%p(L+18)= PENIA(3,IL)
                   BUF(P)%p(L+19)= PENIA(4,IL)
                   BUF(P)%p(L+20)= PENIA(5,IL)
                   BUF(P)%p(L+21)= DIAG_SMS(NOD)
                   BUF(P)%p(L+22)= DXANC(1,IL)
                   BUF(P)%p(L+23)= DXANC(2,IL)
                   BUF(P)%p(L+24)= DXANC(3,IL)
                   BUF(P)%p(L+25)= DVANC(1,IL)
                   BUF(P)%p(L+26)= DVANC(2,IL)
                   BUF(P)%p(L+27)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
               ENDIF
C + la thermique
             ELSE
               IF(IGAP==0) THEN
                IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= PENIS(1,I)
                   BUF(P)%p(L+17)= PENIS(2,I)
                   BUF(P)%p(L+18)= PENIA(1,IL)
                   BUF(P)%p(L+19)= PENIA(2,IL)
                   BUF(P)%p(L+20)= PENIA(3,IL)
                   BUF(P)%p(L+21)= PENIA(4,IL)
                   BUF(P)%p(L+22)= PENIA(5,IL)
                   BUF(P)%p(L+23)= DXANC(1,IL)
                   BUF(P)%p(L+24)= DXANC(2,IL)
                   BUF(P)%p(L+25)= DXANC(3,IL)
                   BUF(P)%p(L+26)= DVANC(1,IL)
                   BUF(P)%p(L+27)= DVANC(2,IL)
                   BUF(P)%p(L+28)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= PENIS(1,I)
                   BUF(P)%p(L+17)= PENIS(2,I)
                   BUF(P)%p(L+18)= PENIA(1,IL)
                   BUF(P)%p(L+19)= PENIA(2,IL)
                   BUF(P)%p(L+20)= PENIA(3,IL)
                   BUF(P)%p(L+21)= PENIA(4,IL)
                   BUF(P)%p(L+22)= PENIA(5,IL)
                   BUF(P)%p(L+23)= DIAG_SMS(NOD)
                   BUF(P)%p(L+24)= DXANC(1,IL)
                   BUF(P)%p(L+25)= DXANC(2,IL)
                   BUF(P)%p(L+26)= DXANC(3,IL)
                   BUF(P)%p(L+27)= DVANC(1,IL)
                   BUF(P)%p(L+28)= DVANC(2,IL)
                   BUF(P)%p(L+29)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
               ELSE
                IF(IDTMINS==0)THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= PENIS(1,I)
                   BUF(P)%p(L+18)= PENIS(2,I)
                   BUF(P)%p(L+19)= PENIA(1,IL)
                   BUF(P)%p(L+20)= PENIA(2,IL)
                   BUF(P)%p(L+21)= PENIA(3,IL)
                   BUF(P)%p(L+22)= PENIA(4,IL)
                   BUF(P)%p(L+23)= PENIA(5,IL)
                   BUF(P)%p(L+24)= DXANC(1,IL)
                   BUF(P)%p(L+25)= DXANC(2,IL)
                   BUF(P)%p(L+26)= DXANC(3,IL)
                   BUF(P)%p(L+27)= DVANC(1,IL)
                   BUF(P)%p(L+28)= DVANC(2,IL)
                   BUF(P)%p(L+29)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
C /DT/NODA/AMS
                ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= PENIS(1,I)
                   BUF(P)%p(L+18)= PENIS(2,I)
                   BUF(P)%p(L+19)= PENIA(1,IL)
                   BUF(P)%p(L+20)= PENIA(2,IL)
                   BUF(P)%p(L+21)= PENIA(3,IL)
                   BUF(P)%p(L+22)= PENIA(4,IL)
                   BUF(P)%p(L+23)= PENIA(5,IL)
                   BUF(P)%p(L+24)= DIAG_SMS(NOD)
                   BUF(P)%p(L+25)= DXANC(1,IL)
                   BUF(P)%p(L+26)= DXANC(2,IL)
                   BUF(P)%p(L+27)= DXANC(3,IL)
                   BUF(P)%p(L+28)= DVANC(1,IL)
                   BUF(P)%p(L+29)= DVANC(2,IL)
                   BUF(P)%p(L+30)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
                END IF
C fin /DT/NODA/AMS
               ENDIF
             ENDIF
            END IF
           ELSEIF(IDTMINS==2)THEN
C /DT/AMS
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= DIAG_SMS(NOD)
                   BUF(P)%p(L+14)= NODNX_SMS(NOD)
                   BUF(P)%p(L+15)= NOD
                   BUF(P)%p(L+16)= DXANC(1,IL)
                   BUF(P)%p(L+17)= DXANC(2,IL)
                   BUF(P)%p(L+18)= DXANC(3,IL)
                   BUF(P)%p(L+19)= DVANC(1,IL)
                   BUF(P)%p(L+20)= DVANC(2,IL)
                   BUF(P)%p(L+21)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= DIAG_SMS(NOD)
                   BUF(P)%p(L+15)= NODNX_SMS(NOD)
                   BUF(P)%p(L+16)= NOD
                   BUF(P)%p(L+17)= DXANC(1,IL)
                   BUF(P)%p(L+18)= DXANC(2,IL)
                   BUF(P)%p(L+19)= DXANC(3,IL)
                   BUF(P)%p(L+20)= DVANC(1,IL)
                   BUF(P)%p(L+21)= DVANC(2,IL)
                   BUF(P)%p(L+22)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ENDIF
C + la thermique
             ELSE
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= DIAG_SMS(NOD)
                   BUF(P)%p(L+17)= NODNX_SMS(NOD)
                   BUF(P)%p(L+18)= NOD
                   BUF(P)%p(L+19)= DXANC(1,IL)
                   BUF(P)%p(L+20)= DXANC(2,IL)
                   BUF(P)%p(L+21)= DXANC(3,IL)
                   BUF(P)%p(L+22)= DVANC(1,IL)
                   BUF(P)%p(L+23)= DVANC(2,IL)
                   BUF(P)%p(L+24)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= DIAG_SMS(NOD)
                   BUF(P)%p(L+18)= NODNX_SMS(NOD)
                   BUF(P)%p(L+19)= NOD
                   BUF(P)%p(L+20)= DXANC(1,IL)
                   BUF(P)%p(L+21)= DXANC(2,IL)
                   BUF(P)%p(L+22)= DXANC(3,IL)
                   BUF(P)%p(L+23)= DVANC(1,IL)
                   BUF(P)%p(L+24)= DVANC(2,IL)
                   BUF(P)%p(L+25)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ENDIF
             ENDIF
            ELSE    ! INACTI = 5 or 6
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= PENIS(1,I)
                   BUF(P)%p(L+14)= PENIS(2,I)
                   BUF(P)%p(L+15)= PENIA(1,IL)
                   BUF(P)%p(L+16)= PENIA(2,IL)
                   BUF(P)%p(L+17)= PENIA(3,IL)
                   BUF(P)%p(L+18)= PENIA(4,IL)
                   BUF(P)%p(L+19)= PENIA(5,IL)
                   BUF(P)%p(L+20)= DIAG_SMS(NOD)
                   BUF(P)%p(L+21)= NODNX_SMS(NOD)
                   BUF(P)%p(L+22)= NOD
                   BUF(P)%p(L+23)= DXANC(1,IL)
                   BUF(P)%p(L+24)= DXANC(2,IL)
                   BUF(P)%p(L+25)= DXANC(3,IL)
                   BUF(P)%p(L+26)= DVANC(1,IL)
                   BUF(P)%p(L+27)= DVANC(2,IL)
                   BUF(P)%p(L+28)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= PENIS(1,I)
                   BUF(P)%p(L+15)= PENIS(2,I)
                   BUF(P)%p(L+16)= PENIA(1,IL)
                   BUF(P)%p(L+17)= PENIA(2,IL)
                   BUF(P)%p(L+18)= PENIA(3,IL)
                   BUF(P)%p(L+19)= PENIA(4,IL)
                   BUF(P)%p(L+20)= PENIA(5,IL)
                   BUF(P)%p(L+21)= DIAG_SMS(NOD)
                   BUF(P)%p(L+22)= NODNX_SMS(NOD)
                   BUF(P)%p(L+23)= NOD
                   BUF(P)%p(L+24)= DXANC(1,IL)
                   BUF(P)%p(L+25)= DXANC(2,IL)
                   BUF(P)%p(L+26)= DXANC(3,IL)
                   BUF(P)%p(L+27)= DVANC(1,IL)
                   BUF(P)%p(L+28)= DVANC(2,IL)
                   BUF(P)%p(L+29)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ENDIF
C + la thermique
             ELSE
               IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= PENIS(1,I)
                   BUF(P)%p(L+17)= PENIS(2,I)
                   BUF(P)%p(L+18)= PENIA(1,IL)
                   BUF(P)%p(L+19)= PENIA(2,IL)
                   BUF(P)%p(L+20)= PENIA(3,IL)
                   BUF(P)%p(L+21)= PENIA(4,IL)
                   BUF(P)%p(L+22)= PENIA(5,IL)
                   BUF(P)%p(L+23)= DIAG_SMS(NOD)
                   BUF(P)%p(L+24)= NODNX_SMS(NOD)
                   BUF(P)%p(L+25)= NOD
                   BUF(P)%p(L+26)= DXANC(1,IL)
                   BUF(P)%p(L+27)= DXANC(2,IL)
                   BUF(P)%p(L+28)= DXANC(3,IL)
                   BUF(P)%p(L+29)= DVANC(1,IL)
                   BUF(P)%p(L+30)= DVANC(2,IL)
                   BUF(P)%p(L+31)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= PENIS(1,I)
                   BUF(P)%p(L+18)= PENIS(2,I)
                   BUF(P)%p(L+19)= PENIA(1,IL)
                   BUF(P)%p(L+20)= PENIA(2,IL)
                   BUF(P)%p(L+21)= PENIA(3,IL)
                   BUF(P)%p(L+22)= PENIA(4,IL)
                   BUF(P)%p(L+23)= PENIA(5,IL)
                   BUF(P)%p(L+24)= DIAG_SMS(NOD)
                   BUF(P)%p(L+25)= NODNX_SMS(NOD)
                   BUF(P)%p(L+26)= NOD
                   BUF(P)%p(L+27)= DXANC(1,IL)
                   BUF(P)%p(L+28)= DXANC(2,IL)
                   BUF(P)%p(L+29)= DXANC(3,IL)
                   BUF(P)%p(L+30)= DVANC(1,IL)
                   BUF(P)%p(L+31)= DVANC(2,IL)
                   BUF(P)%p(L+32)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ENDIF
             ENDIF
            END IF
           ELSE
C /DT/INTER/AMS
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= DIAG_SMS(NOD)
                   BUF(P)%p(L+14)= NOD
                   BUF(P)%p(L+15)= DXANC(1,IL)
                   BUF(P)%p(L+16)= DXANC(2,IL)
                   BUF(P)%p(L+17)= DXANC(3,IL)
                   BUF(P)%p(L+18)= DVANC(1,IL)
                   BUF(P)%p(L+19)= DVANC(2,IL)
                   BUF(P)%p(L+20)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= DIAG_SMS(NOD)
                   BUF(P)%p(L+15)= NOD
                   BUF(P)%p(L+16)= DXANC(1,IL)
                   BUF(P)%p(L+17)= DXANC(2,IL)
                   BUF(P)%p(L+18)= DXANC(3,IL)
                   BUF(P)%p(L+19)= DVANC(1,IL)
                   BUF(P)%p(L+20)= DVANC(2,IL)
                   BUF(P)%p(L+21)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ENDIF
C + la thermique
             ELSE
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= DIAG_SMS(NOD)
                   BUF(P)%p(L+17)= NOD
                   BUF(P)%p(L+18)= DXANC(1,IL)
                   BUF(P)%p(L+19)= DXANC(2,IL)
                   BUF(P)%p(L+20)= DXANC(3,IL)
                   BUF(P)%p(L+21)= DVANC(1,IL)
                   BUF(P)%p(L+22)= DVANC(2,IL)
                   BUF(P)%p(L+23)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= DIAG_SMS(NOD)
                   BUF(P)%p(L+18)= NOD
                   BUF(P)%p(L+19)= DXANC(1,IL)
                   BUF(P)%p(L+20)= DXANC(2,IL)
                   BUF(P)%p(L+21)= DXANC(3,IL)
                   BUF(P)%p(L+22)= DVANC(1,IL)
                   BUF(P)%p(L+23)= DVANC(2,IL)
                   BUF(P)%p(L+24)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
              ENDIF
             ENDIF
            ELSE    ! INACTI = 5 or 6
             IF(INTTH == 0 ) THEN
              IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= PENIS(1,I)
                   BUF(P)%p(L+14)= PENIS(2,I)
                   BUF(P)%p(L+15)= PENIA(1,IL)
                   BUF(P)%p(L+16)= PENIA(2,IL)
                   BUF(P)%p(L+17)= PENIA(3,IL)
                   BUF(P)%p(L+18)= PENIA(4,IL)
                   BUF(P)%p(L+19)= PENIA(5,IL)
                   BUF(P)%p(L+20)= DIAG_SMS(NOD)
                   BUF(P)%p(L+21)= NOD
                   BUF(P)%p(L+22)= DXANC(1,IL)
                   BUF(P)%p(L+23)= DXANC(2,IL)
                   BUF(P)%p(L+24)= DXANC(3,IL)
                   BUF(P)%p(L+25)= DVANC(1,IL)
                   BUF(P)%p(L+26)= DVANC(2,IL)
                   BUF(P)%p(L+27)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= PENIS(1,I)
                   BUF(P)%p(L+15)= PENIS(2,I)
                   BUF(P)%p(L+16)= PENIA(1,IL)
                   BUF(P)%p(L+17)= PENIA(2,IL)
                   BUF(P)%p(L+18)= PENIA(3,IL)
                   BUF(P)%p(L+19)= PENIA(4,IL)
                   BUF(P)%p(L+20)= PENIA(5,IL)
                   BUF(P)%p(L+21)= DIAG_SMS(NOD)
                   BUF(P)%p(L+22)= NOD
                   BUF(P)%p(L+23)= DXANC(1,IL)
                   BUF(P)%p(L+24)= DXANC(2,IL)
                   BUF(P)%p(L+25)= DXANC(3,IL)
                   BUF(P)%p(L+26)= DVANC(1,IL)
                   BUF(P)%p(L+27)= DVANC(2,IL)
                   BUF(P)%p(L+28)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ENDIF
C + la thermique
             ELSE
               IF(IGAP==0) THEN
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= TEMP(NOD)
                   BUF(P)%p(L+14)= IELEC(I)
                   BUF(P)%p(L+15)= AREAS(I)
                   BUF(P)%p(L+16)= PENIS(1,I)
                   BUF(P)%p(L+17)= PENIS(2,I)
                   BUF(P)%p(L+18)= PENIA(1,IL)
                   BUF(P)%p(L+19)= PENIA(2,IL)
                   BUF(P)%p(L+20)= PENIA(3,IL)
                   BUF(P)%p(L+21)= PENIA(4,IL)
                   BUF(P)%p(L+22)= PENIA(5,IL)
                   BUF(P)%p(L+23)= DIAG_SMS(NOD)
                   BUF(P)%p(L+24)= NOD
                   BUF(P)%p(L+25)= DXANC(1,IL)
                   BUF(P)%p(L+26)= DXANC(2,IL)
                   BUF(P)%p(L+27)= DXANC(3,IL)
                   BUF(P)%p(L+28)= DVANC(1,IL)
                   BUF(P)%p(L+29)= DVANC(2,IL)
                   BUF(P)%p(L+30)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ELSE
                 DO J = 1, NB
                   I = INDEX(J)
                   IL  = NSV(I)
                   NOD = NLG(IL)
                   BUF(P)%p(L+1) = XA(1,IL)
                   BUF(P)%p(L+2) = XA(2,IL)
                   BUF(P)%p(L+3) = XA(3,IL)
                   BUF(P)%p(L+4) = I
                   BUF(P)%p(L+5) = VA(1,IL)
                   BUF(P)%p(L+6) = VA(2,IL)
                   BUF(P)%p(L+7) = VA(3,IL)
                   BUF(P)%p(L+8) = MS(NOD)
                   BUF(P)%p(L+9) = STFA(IL)
                   BUF(P)%p(L+10)= ITAB(NOD)
                   BUF(P)%p(L+11)= KINET(NOD)
                   BUF(P)%p(L+12)= NBINFLG(IL)
                   BUF(P)%p(L+13)= GAP_S(I)
                   BUF(P)%p(L+14)= TEMP(NOD)
                   BUF(P)%p(L+15)= IELEC(I)
                   BUF(P)%p(L+16)= AREAS(I)
                   BUF(P)%p(L+17)= PENIS(1,I)
                   BUF(P)%p(L+18)= PENIS(2,I)
                   BUF(P)%p(L+19)= PENIA(1,IL)
                   BUF(P)%p(L+20)= PENIA(2,IL)
                   BUF(P)%p(L+21)= PENIA(3,IL)
                   BUF(P)%p(L+22)= PENIA(4,IL)
                   BUF(P)%p(L+23)= PENIA(5,IL)
                   BUF(P)%p(L+24)= DIAG_SMS(NOD)
                   BUF(P)%p(L+25)= NOD
                   BUF(P)%p(L+26)= DXANC(1,IL)
                   BUF(P)%p(L+27)= DXANC(2,IL)
                   BUF(P)%p(L+28)= DXANC(3,IL)
                   BUF(P)%p(L+29)= DVANC(1,IL)
                   BUF(P)%p(L+30)= DVANC(2,IL)
                   BUF(P)%p(L+31)= DVANC(3,IL)
                   L = L + SIZ
                 END DO
               ENDIF
             ENDIF
            END IF
           END IF
C
           MSGTYP = MSGOFF3 
           CALL MPI_ISEND(
     1        BUF(P)%P(1),L,MPI_DOUBLE_PRECISION,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
          ENDIF
        ENDDO
      ENDIF
C
C   reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              MSGTYP = MSGOFF2
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P)>0) THEN
                L=L+1
                ISINDEXI(L)=P
                NSNR = NSNR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C
        IF(NSNR>0) THEN
          IF (IR4R8 == 2) THEN
            ALLOCATE(XREM(SIZ,NSNR),STAT=IERROR)
          ELSE
            ALLOCATE(XREM(SIZ,2*NSNR),STAT=IERROR)
            ALLOCATE(IREM(1,NSNR),STAT=IERROR1)
            IERROR=IERROR+IERROR1
          END IF
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*SIZ
            MSGTYP = MSGOFF3 
            IAD = IDEB
C correction adresse pour passage tableau XREM SP utilise en DP ds la routine de comm
            IF(IR4R8 == 1) IAD = 2*IDEB-1
            CALL MPI_IRECV(
     1        XREM(1,IAD),LEN,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
C           P=ISINDEXI(INDEXI)
          ENDDO
          IF(IR4R8 == 1)THEN
            CALL CONVERSION7(XREM,XREM,IREM,SIZ,IDEB-1)
          END IF
        ENDIF
      ENDIF
C
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
              IF(NBOX(P)/=0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(BUF(P)%p)
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF (IMONM > 0) CALL STOPTIME(25,1)
C
#endif
      RETURN
      END
