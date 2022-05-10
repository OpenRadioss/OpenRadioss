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
Chd|  SPMD_TRI17BOX                 source/mpi/interfaces/spmd_tri17box.F
Chd|-- called by -----------
Chd|        I17BUCE                       source/interfaces/int17/i17buce.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI17BOX(NELEMS  ,NMES    ,X      ,V    ,FROTS  ,
     2                         KS      ,BMINMAL ,WEIGHT ,NIN  ,ISENDTO,
     3                         IRCVFROM,NMESR   ,IXS    ,IXS16,EMINXS )
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
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMS(*), WEIGHT(*), IXS(NIXS,*), IXS16(8,*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        NMES, NMESR, NIN
      my_real
     .        BMINMAL(6),
     .        X(3,*), V(3,*), FROTS(7,*), KS(2,*), EMINXS(6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, N1, NE,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NMES),NBOX(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3
      DATA MSGOFF/129/
      DATA MSGOFF2/130/
      DATA MSGOFF3/131/

      my_real BMINMA(6,NSPMD)
      TYPE(real_pointer), DIMENSION(NSPMD) :: BUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
Cel boite minmax pour le tri provenant de i7buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC).EQ.0.AND.
     .   ISENDTO(NIN,LOC_PROC).EQ.0) RETURN
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
Cel envoi boite
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
Cel reception des boites min-max
C
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
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
Cel envoi de XREM
C
      SIZ = 112
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
C
          NB = 0
          DO I=1,NMES
            IF(EMINXS(4,I).GT.BMINMA(1,P).AND.
     .         EMINXS(5,I).GT.BMINMA(2,P).AND.
     .         EMINXS(6,I).GT.BMINMA(3,P).AND.
     .         EMINXS(1,I).LT.BMINMA(4,P).AND.
     .         EMINXS(2,I).LT.BMINMA(5,P).AND.
     .         EMINXS(3,I).LT.BMINMA(6,P))THEN
              NB = NB + 1
              INDEX(NB) = I
            ENDIF
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
          IF (NB.GT.0) THEN
            ALLOCATE(BUF(P)%P(SIZ*NB),STAT=IERROR)
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
            DO J = 1, NB
              I = INDEX(J)
              BUF(P)%p(L+1) = EMINXS(1,I)
              BUF(P)%p(L+2) = EMINXS(2,I)
              BUF(P)%p(L+3) = EMINXS(3,I)
              BUF(P)%p(L+4) = EMINXS(4,I)
              BUF(P)%p(L+5) = EMINXS(5,I)
              BUF(P)%p(L+6) = EMINXS(6,I)
              BUF(P)%p(L+7) = I
              NE = NELEMS(I)
C
              N1 = IXS(2,NE)
              BUF(P)%p(L+8)  = X(1,N1)
              BUF(P)%p(L+9)  = X(2,N1)
              BUF(P)%p(L+10) = X(3,N1)
              BUF(P)%p(L+11) = V(1,N1)
              BUF(P)%p(L+12) = V(2,N1)
              BUF(P)%p(L+13) = V(3,N1)
              N1 = IXS(3,NE)
              BUF(P)%p(L+14) = X(1,N1)
              BUF(P)%p(L+15) = X(2,N1)
              BUF(P)%p(L+16) = X(3,N1)
              BUF(P)%p(L+17) = V(1,N1)
              BUF(P)%p(L+18) = V(2,N1)
              BUF(P)%p(L+19) = V(3,N1)
              N1 = IXS(4,NE)
              BUF(P)%p(L+20) = X(1,N1)
              BUF(P)%p(L+21) = X(2,N1)
              BUF(P)%p(L+22) = X(3,N1)
              BUF(P)%p(L+23) = V(1,N1)
              BUF(P)%p(L+24) = V(2,N1)
              BUF(P)%p(L+25) = V(3,N1)
              N1 = IXS(5,NE)
              BUF(P)%p(L+26) = X(1,N1)
              BUF(P)%p(L+27) = X(2,N1)
              BUF(P)%p(L+28) = X(3,N1)
              BUF(P)%p(L+29) = V(1,N1)
              BUF(P)%p(L+30) = V(2,N1)
              BUF(P)%p(L+31) = V(3,N1)
              N1 = IXS(6,NE)
              BUF(P)%p(L+32) = X(1,N1)
              BUF(P)%p(L+33) = X(2,N1)
              BUF(P)%p(L+34) = X(3,N1)
              BUF(P)%p(L+35) = V(1,N1)
              BUF(P)%p(L+36) = V(2,N1)
              BUF(P)%p(L+37) = V(3,N1)
              N1 = IXS(7,NE)
              BUF(P)%p(L+38) = X(1,N1)
              BUF(P)%p(L+39) = X(2,N1)
              BUF(P)%p(L+40) = X(3,N1)
              BUF(P)%p(L+41) = V(1,N1)
              BUF(P)%p(L+42) = V(2,N1)
              BUF(P)%p(L+43) = V(3,N1)
              N1 = IXS(8,NE)
              BUF(P)%p(L+44) = X(1,N1)
              BUF(P)%p(L+45) = X(2,N1)
              BUF(P)%p(L+46) = X(3,N1)
              BUF(P)%p(L+47) = V(1,N1)
              BUF(P)%p(L+48) = V(2,N1)
              BUF(P)%p(L+49) = V(3,N1)
              N1 = IXS(9,NE)
              BUF(P)%p(L+50) = X(1,N1)
              BUF(P)%p(L+51) = X(2,N1)
              BUF(P)%p(L+52) = X(3,N1)
              BUF(P)%p(L+53) = V(1,N1)
              BUF(P)%p(L+54) = V(2,N1)
              BUF(P)%p(L+55) = V(3,N1)
C
              N1 = IXS16(1,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+56)  = X(1,N1)
              BUF(P)%p(L+57)  = X(2,N1)
              BUF(P)%p(L+58) = X(3,N1)
              BUF(P)%p(L+59) = V(1,N1)
              BUF(P)%p(L+60) = V(2,N1)
              BUF(P)%p(L+61) = V(3,N1)
              N1 = IXS16(2,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+62) = X(1,N1)
              BUF(P)%p(L+63) = X(2,N1)
              BUF(P)%p(L+64) = X(3,N1)
              BUF(P)%p(L+65) = V(1,N1)
              BUF(P)%p(L+66) = V(2,N1)
              BUF(P)%p(L+67) = V(3,N1)
              N1 = IXS16(3,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+68) = X(1,N1)
              BUF(P)%p(L+69) = X(2,N1)
              BUF(P)%p(L+70) = X(3,N1)
              BUF(P)%p(L+71) = V(1,N1)
              BUF(P)%p(L+72) = V(2,N1)
              BUF(P)%p(L+73) = V(3,N1)
              N1 = IXS16(4,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+74) = X(1,N1)
              BUF(P)%p(L+75) = X(2,N1)
              BUF(P)%p(L+76) = X(3,N1)
              BUF(P)%p(L+77) = V(1,N1)
              BUF(P)%p(L+78) = V(2,N1)
              BUF(P)%p(L+79) = V(3,N1)
              N1 = IXS16(5,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+80) = X(1,N1)
              BUF(P)%p(L+81) = X(2,N1)
              BUF(P)%p(L+82) = X(3,N1)
              BUF(P)%p(L+83) = V(1,N1)
              BUF(P)%p(L+84) = V(2,N1)
              BUF(P)%p(L+85) = V(3,N1)
              N1 = IXS16(6,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+86) = X(1,N1)
              BUF(P)%p(L+87) = X(2,N1)
              BUF(P)%p(L+88) = X(3,N1)
              BUF(P)%p(L+89) = V(1,N1)
              BUF(P)%p(L+90) = V(2,N1)
              BUF(P)%p(L+91) = V(3,N1)
              N1 = IXS16(7,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+92) = X(1,N1)
              BUF(P)%p(L+93) = X(2,N1)
              BUF(P)%p(L+94) = X(3,N1)
              BUF(P)%p(L+95) = V(1,N1)
              BUF(P)%p(L+96) = V(2,N1)
              BUF(P)%p(L+97) = V(3,N1)
              N1 = IXS16(8,NE-NUMELS8-NUMELS10-NUMELS20)
              BUF(P)%p(L+98) = X(1,N1)
              BUF(P)%p(L+99) = X(2,N1)
              BUF(P)%p(L+100) = X(3,N1)
              BUF(P)%p(L+101) = V(1,N1)
              BUF(P)%p(L+102) = V(2,N1)
              BUF(P)%p(L+103) = V(3,N1)

C
              BUF(P)%p(L+104) = FROTS(1,I)
              BUF(P)%p(L+105) = FROTS(2,I)
              BUF(P)%p(L+106) = FROTS(3,I)
              BUF(P)%p(L+107) = FROTS(4,I)
              BUF(P)%p(L+108) = FROTS(5,I)
              BUF(P)%p(L+109) = FROTS(6,I)
              BUF(P)%p(L+110) = FROTS(7,I)
C
              BUF(P)%p(L+111) = KS(1,I)
              BUF(P)%p(L+112) = KS(2,I)
C
              L = L + SIZ
            END DO
C
            MSGTYP = MSGOFF3
            CALL MPI_ISEND(
     1        BUF(P)%P(1),L,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),IERROR)
          ENDIF
        ENDDO
      ENDIF
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NMESR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF2
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P).GT.0) THEN
                L=L+1
                ISINDEXI(L)=P
                NMESR = NMESR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C
        IF(NMESR.GT.0) THEN
          ALLOCATE(XREM(SIZ,NMESR),STAT=IERROR)
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*SIZ
            MSGTYP = MSGOFF3
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
C           P=ISINDEXI(INDEXI)
          ENDDO
        ENDIF
      ENDIF
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
              IF(NBOX(P).NE.0) THEN
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
