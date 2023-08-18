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
C
Chd|====================================================================
Chd|  SPMD_TRI20BOXE                source/mpi/interfaces/spmd_tri20boxe.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CONVERSION11                  source/mpi/interfaces/spmd_i7tool.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI20BOXE(IXLINS  ,NRTS    ,XA     ,VA  ,MS     ,
     2                          BMINMAL ,WEIGHT  ,STIFS  ,NIN ,ISENDTO,
     3                          IRCVFROM,IAD_ELEM,FR_ELEM,NRTSR,INACTI,
     4                          GAP_S   ,PENIS   ,ITAB   ,IGAP ,TZINF ,
     5                          NLG     ,PENIA   ,DIAG_SMS,NODNX_SMS  )
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
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NRTS, IGAP, INACTI,
     .        IXLINS(2,*), WEIGHT(*),NRTSR,
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), NLG(*), NODNX_SMS(*)
      my_real
     .        TZINF,
     .        XA(3,*), VA(3,*), MS(*), BMINMAL(*), STIFS(*),GAP_S(*),
     .        PENIS(2,*), PENIA(5,*), DIAG_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, LOC_PROC,P,IDEB, IERROR1, IAD,
     .        SIZ,J, L, BUFSIZ, LEN, NB, N1, N2, N1L, N2L,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NRTS),NBOX(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3
      DATA MSGOFF/135/
      DATA MSGOFF2/136/
      DATA MSGOFF3/137/

      my_real
     .        BMINMA(6,NSPMD), RATIO,
     .        XMINS, YMINS, ZMINS, XMAXS, YMAXS, ZMAXS
      TYPE(r8_pointer), DIMENSION(NSPMD) :: BUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C   boite minmax pour le tri provenant de i7buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC)==0.AND.
     .   ISENDTO(NIN,LOC_PROC)==0) RETURN
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
      SIZ = 18
      IF(INACTI==5.OR.INACTI==6) SIZ = SIZ + 12
      IF(IGAP==1) SIZ = SIZ + 1
      IF(IDTMINS > 0) SIZ = SIZ + 2
      IF(IDTMINS == 2)THEN
        SIZ = SIZ + 4
      ELSEIF(IDTMINS_INT/=0)THEN
        SIZ = SIZ + 2
      END IF
      IDEB = 1
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
C
          L = IDEB
          NB = 0
          DO I=1,NRTS
           N1L=IXLINS(1,I)
           N2L=IXLINS(2,I)
           IF(STIFS(I)>ZERO) THEN
            XMINS = MIN(XA(1,N1L),XA(1,N2L))-TZINF
            YMINS = MIN(XA(2,N1L),XA(2,N2L))-TZINF
            ZMINS = MIN(XA(3,N1L),XA(3,N2L))-TZINF
            XMAXS = MAX(XA(1,N1L),XA(1,N2L))+TZINF
            YMAXS = MAX(XA(2,N1L),XA(2,N2L))+TZINF
            ZMAXS = MAX(XA(3,N1L),XA(3,N2L))+TZINF
            IF(XMAXS>=BMINMA(4,P).AND.XMINS<=BMINMA(1,P).AND.
     .         YMAXS>=BMINMA(5,P).AND.YMINS<=BMINMA(2,P).AND.
     .         ZMAXS>=BMINMA(6,P).AND.ZMINS<=BMINMA(3,P))THEN
              NB = NB + 1
              INDEX(NB) = I
            ENDIF
           END IF
          ENDDO
          NBOX(P) = NB
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
           IF(IDTMINS/=2.AND.IDTMINS_INT==0)THEN
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(IGAP/=0) THEN
              IF(IDTMINS==0)THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                L = L + SIZ
               END DO
C /DT/NODA/AMS
              ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= DIAG_SMS(N1)
                BUF(P)%p(L+21)= DIAG_SMS(N2)
                L = L + SIZ
               END DO
              END IF
C fin /DT/NODA/AMS
             ELSE
              IF(IDTMINS==0)THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                L = L + SIZ
               END DO
C /DT/NODA/AMS
              ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= DIAG_SMS(N1)
                BUF(P)%p(L+20)= DIAG_SMS(N2)
                L = L + SIZ
               END DO
              END IF
C fin /DT/NODA/AMS
             END IF
            ELSE
             IF(IGAP/=0) THEN
              IF(IDTMINS==0)THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= PENIS(1,I)
                BUF(P)%p(L+21)= PENIS(2,I)
                BUF(P)%p(L+22)= PENIA(1,N1L)
                BUF(P)%p(L+23)= PENIA(2,N1L)
                BUF(P)%p(L+24)= PENIA(3,N1L)
                BUF(P)%p(L+25)= PENIA(4,N1L)
                BUF(P)%p(L+26)= PENIA(5,N1L)
                BUF(P)%p(L+27)= PENIA(1,N2L)
                BUF(P)%p(L+28)= PENIA(2,N2L)
                BUF(P)%p(L+29)= PENIA(3,N2L)
                BUF(P)%p(L+30)= PENIA(4,N2L)
                BUF(P)%p(L+31)= PENIA(5,N2L)
                L = L + SIZ
               END DO
C /DT/NODA/AMS
              ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= PENIS(1,I)
                BUF(P)%p(L+21)= PENIS(2,I)
                BUF(P)%p(L+22)= PENIA(1,N1L)
                BUF(P)%p(L+23)= PENIA(2,N1L)
                BUF(P)%p(L+24)= PENIA(3,N1L)
                BUF(P)%p(L+25)= PENIA(4,N1L)
                BUF(P)%p(L+26)= PENIA(5,N1L)
                BUF(P)%p(L+27)= PENIA(1,N2L)
                BUF(P)%p(L+28)= PENIA(2,N2L)
                BUF(P)%p(L+29)= PENIA(3,N2L)
                BUF(P)%p(L+30)= PENIA(4,N2L)
                BUF(P)%p(L+31)= PENIA(5,N2L)
                BUF(P)%p(L+32)= DIAG_SMS(N1)
                BUF(P)%p(L+33)= DIAG_SMS(N2)
                L = L + SIZ
               END DO
              END IF
C fin /DT/NODA/AMS
             ELSE
              IF(IDTMINS==0)THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= PENIS(1,I)
                BUF(P)%p(L+20)= PENIS(2,I)
                BUF(P)%p(L+21)= PENIA(1,N1L)
                BUF(P)%p(L+22)= PENIA(2,N1L)
                BUF(P)%p(L+23)= PENIA(3,N1L)
                BUF(P)%p(L+24)= PENIA(4,N1L)
                BUF(P)%p(L+25)= PENIA(5,N1L)
                BUF(P)%p(L+26)= PENIA(1,N2L)
                BUF(P)%p(L+27)= PENIA(2,N2L)
                BUF(P)%p(L+28)= PENIA(3,N2L)
                BUF(P)%p(L+29)= PENIA(4,N2L)
                BUF(P)%p(L+30)= PENIA(5,N2L)
                L = L + SIZ
               END DO
C /DT/NODA/AMS
              ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= PENIS(1,I)
                BUF(P)%p(L+20)= PENIS(2,I)
                BUF(P)%p(L+21)= PENIA(1,N1L)
                BUF(P)%p(L+22)= PENIA(2,N1L)
                BUF(P)%p(L+23)= PENIA(3,N1L)
                BUF(P)%p(L+24)= PENIA(4,N1L)
                BUF(P)%p(L+25)= PENIA(5,N1L)
                BUF(P)%p(L+26)= PENIA(1,N2L)
                BUF(P)%p(L+27)= PENIA(2,N2L)
                BUF(P)%p(L+28)= PENIA(3,N2L)
                BUF(P)%p(L+29)= PENIA(4,N2L)
                BUF(P)%p(L+30)= PENIA(5,N2L)
                BUF(P)%p(L+31)= DIAG_SMS(N1)
                BUF(P)%p(L+32)= DIAG_SMS(N2)
                L = L + SIZ
               END DO
              END IF
C fin /DT/NODA/AMS
             END IF
            END IF
           ELSEIF(IDTMINS==2)THEN
C /DT/AMS
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(IGAP/=0) THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= DIAG_SMS(N1)
                BUF(P)%p(L+21)= DIAG_SMS(N2)
                BUF(P)%p(L+22)= NODNX_SMS(N1)
                BUF(P)%p(L+23)= N1
                BUF(P)%p(L+24)= NODNX_SMS(N2)
                BUF(P)%p(L+25)= N2
                L = L + SIZ
               END DO
             ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= DIAG_SMS(N1)
                BUF(P)%p(L+20)= DIAG_SMS(N2)
                BUF(P)%p(L+21)= NODNX_SMS(N1)
                BUF(P)%p(L+22)= N1
                BUF(P)%p(L+23)= NODNX_SMS(N2)
                BUF(P)%p(L+24)= N2
                L = L + SIZ
               END DO
             END IF
            ELSE
             IF(IGAP/=0) THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= PENIS(1,I)
                BUF(P)%p(L+21)= PENIS(2,I)
                BUF(P)%p(L+22)= PENIA(1,N1L)
                BUF(P)%p(L+23)= PENIA(2,N1L)
                BUF(P)%p(L+24)= PENIA(3,N1L)
                BUF(P)%p(L+25)= PENIA(4,N1L)
                BUF(P)%p(L+26)= PENIA(5,N1L)
                BUF(P)%p(L+27)= PENIA(1,N2L)
                BUF(P)%p(L+28)= PENIA(2,N2L)
                BUF(P)%p(L+29)= PENIA(3,N2L)
                BUF(P)%p(L+30)= PENIA(4,N2L)
                BUF(P)%p(L+31)= PENIA(5,N2L)
                BUF(P)%p(L+32)= DIAG_SMS(N1)
                BUF(P)%p(L+33)= DIAG_SMS(N2)
                BUF(P)%p(L+34)= NODNX_SMS(N1)
                BUF(P)%p(L+35)= N1
                BUF(P)%p(L+36)= NODNX_SMS(N2)
                BUF(P)%p(L+37)= N2
                L = L + SIZ
               END DO
             ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= PENIS(1,I)
                BUF(P)%p(L+20)= PENIS(2,I)
                BUF(P)%p(L+21)= PENIA(1,N1L)
                BUF(P)%p(L+22)= PENIA(2,N1L)
                BUF(P)%p(L+23)= PENIA(3,N1L)
                BUF(P)%p(L+24)= PENIA(4,N1L)
                BUF(P)%p(L+25)= PENIA(5,N1L)
                BUF(P)%p(L+26)= PENIA(1,N2L)
                BUF(P)%p(L+27)= PENIA(2,N2L)
                BUF(P)%p(L+28)= PENIA(3,N2L)
                BUF(P)%p(L+29)= PENIA(4,N2L)
                BUF(P)%p(L+30)= PENIA(5,N2L)
                BUF(P)%p(L+31)= DIAG_SMS(N1)
                BUF(P)%p(L+32)= DIAG_SMS(N2)
                BUF(P)%p(L+33)= NODNX_SMS(N1)
                BUF(P)%p(L+34)= N1
                BUF(P)%p(L+35)= NODNX_SMS(N2)
                BUF(P)%p(L+36)= N2
                L = L + SIZ
               END DO
             END IF
            END IF
           ELSE
C /DT/INTER/AMS
            IF(INACTI/=5.AND.INACTI/=6) THEN
             IF(IGAP/=0) THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= DIAG_SMS(N1)
                BUF(P)%p(L+21)= DIAG_SMS(N2)
                BUF(P)%p(L+22)= N1
                BUF(P)%p(L+23)= N2
                L = L + SIZ
               END DO
             ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= DIAG_SMS(N1)
                BUF(P)%p(L+20)= DIAG_SMS(N2)
                BUF(P)%p(L+21)= N1
                BUF(P)%p(L+22)= N2
                L = L + SIZ
               END DO
             END IF
            ELSE
             IF(IGAP/=0) THEN
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= GAP_S(I)
                BUF(P)%p(L+20)= PENIS(1,I)
                BUF(P)%p(L+21)= PENIS(2,I)
                BUF(P)%p(L+22)= PENIA(1,N1L)
                BUF(P)%p(L+23)= PENIA(2,N1L)
                BUF(P)%p(L+24)= PENIA(3,N1L)
                BUF(P)%p(L+25)= PENIA(4,N1L)
                BUF(P)%p(L+26)= PENIA(5,N1L)
                BUF(P)%p(L+27)= PENIA(1,N2L)
                BUF(P)%p(L+28)= PENIA(2,N2L)
                BUF(P)%p(L+29)= PENIA(3,N2L)
                BUF(P)%p(L+30)= PENIA(4,N2L)
                BUF(P)%p(L+31)= PENIA(5,N2L)
                BUF(P)%p(L+32)= DIAG_SMS(N1)
                BUF(P)%p(L+33)= DIAG_SMS(N2)
                BUF(P)%p(L+34)= N1
                BUF(P)%p(L+35)= N2
                L = L + SIZ
               END DO
             ELSE
               DO J = 1, NB
                I = INDEX(J)
                N1L=IXLINS(1,I)
                N2L=IXLINS(2,I)
                N1=NLG(N1L)
                N2=NLG(N2L)
                BUF(P)%p(L+1) = I
                BUF(P)%p(L+2) = XA(1,N1L)
                BUF(P)%p(L+3) = XA(2,N1L)
                BUF(P)%p(L+4) = XA(3,N1L)
                BUF(P)%p(L+5) = VA(1,N1L)
                BUF(P)%p(L+6) = VA(2,N1L)
                BUF(P)%p(L+7) = VA(3,N1L)
                BUF(P)%p(L+8) = MS(N1)
                BUF(P)%p(L+9) = ITAB(N1)
                BUF(P)%p(L+10)= XA(1,N2L)
                BUF(P)%p(L+11)= XA(2,N2L)
                BUF(P)%p(L+12)= XA(3,N2L)
                BUF(P)%p(L+13)= VA(1,N2L)
                BUF(P)%p(L+14)= VA(2,N2L)
                BUF(P)%p(L+15)= VA(3,N2L)
                BUF(P)%p(L+16)= MS(N2)
                BUF(P)%p(L+17)= ITAB(N2)
                BUF(P)%p(L+18)= STIFS(I)
                BUF(P)%p(L+19)= PENIS(1,I)
                BUF(P)%p(L+20)= PENIS(2,I)
                BUF(P)%p(L+21)= PENIA(1,N1L)
                BUF(P)%p(L+22)= PENIA(2,N1L)
                BUF(P)%p(L+23)= PENIA(3,N1L)
                BUF(P)%p(L+24)= PENIA(4,N1L)
                BUF(P)%p(L+25)= PENIA(5,N1L)
                BUF(P)%p(L+26)= PENIA(1,N2L)
                BUF(P)%p(L+27)= PENIA(2,N2L)
                BUF(P)%p(L+28)= PENIA(3,N2L)
                BUF(P)%p(L+29)= PENIA(4,N2L)
                BUF(P)%p(L+30)= PENIA(5,N2L)
                BUF(P)%p(L+31)= DIAG_SMS(N1)
                BUF(P)%p(L+32)= DIAG_SMS(N2)
                BUF(P)%p(L+33)= N1
                BUF(P)%p(L+34)= N2
                L = L + SIZ
               END DO
             END IF
            END IF
           END IF
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
        NRTSR = 0
        L=0
        DO P = 1, NSPMD
          NSNFIE(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              MSGTYP = MSGOFF2
              CALL MPI_RECV(NSNFIE(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFIE(NIN)%P(P)>0) THEN
                L=L+1
                ISINDEXI(L)=P
                NRTSR = NRTSR + NSNFIE(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C
        IF(NRTSR>0) THEN
          IF (IR4R8 == 2) THEN
            ALLOCATE(XREM(SIZ,NRTSR),STAT=IERROR)
          ELSE
            ALLOCATE(XREM(SIZ,2*NRTSR),STAT=IERROR)
            ALLOCATE(IREM(2,NRTSR),STAT=IERROR1)
            IERROR=IERROR+IERROR1
          END IF
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFIE(NIN)%P(P)*SIZ
            MSGTYP = MSGOFF3 
            IAD = IDEB
C correction adresse pour passage tableau XREM SP utilise en DP ds la routine de comm
            IF(IR4R8 == 1) IAD = 2*IDEB-1
            CALL MPI_IRECV(
     1        XREM(1,IAD),LEN,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
            IDEB = IDEB + NSNFIE(NIN)%P(P)
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
C           P=ISINDEXI(INDEXI)
          ENDDO
          IF(IR4R8 == 1)THEN
            CALL CONVERSION11(XREM,XREM,IREM,SIZ,IDEB-1)
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
#endif
      RETURN
      END
