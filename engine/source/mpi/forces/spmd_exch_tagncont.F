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
Chd|  SPMD_EXCH_TAGNCONT            source/mpi/forces/spmd_exch_tagncont.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_TAGNCONT(TAGNCONT,IAD_ELEM ,FR_ELEM,LENR     )
C--------------------------------------
C--------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), TAGNCONT(NLOADP_HYD_INTER,NUMNOD),LENR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,IERROR,MSGOFF,IERROR2,
     .        SIZ,J,K,L,NB_NOD,NP,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/120/

      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF,SBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------


      IERROR = 0
      ALLOCATE( RBUF(NLOADP_HYD_INTER*LENR),STAT=IERROR2)
      IERROR = IERROR + IERROR2
      ALLOCATE( SBUF(NLOADP_HYD_INTER*LENR),STAT=IERROR2)
      IERROR = IERROR + IERROR2

      IF(IERROR/=0) THEN
         CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
         CALL ARRET(2)
      END IF
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = NLOADP_HYD_INTER*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C


      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
         DO NP=1,NLOADP_HYD_INTER
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  TAGNCONT(NP,NOD)
              L = L + 1
            END DO
         ENDDO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   sending messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C Send to N+I mod P 
C-------------------------------------------------------------------
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C Counting
C

C
      DO I = 1, NSPMD
C
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO NP=1,NLOADP_HYD_INTER
#include        "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  TAGNCONT(NP,NOD) = MAX(TAGNCONT(NP,NOD), RBUF(L))
                  L = L + 1
               END DO
          ENDDO

       ENDIF
C
C

      END DO
C
Cel wait terminaison isend
C

      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO

      DEALLOCATE(RBUF,SBUF)
C
#endif
      RETURN
      END



Chd|====================================================================
Chd|  SPMD_TAGNCONT_STAMP           source/mpi/forces/spmd_exch_tagncont.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TAGNCONT_STAMP(IPARI    ,TAGNCONT  ,KLOADPINTER , LOADPINTER ,
     .                               LOADP_HYD_INTER,SENSOR ,INTBUF_TAB )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTSTAMP_GLOB_MOD
      USE INTBUFDEF_MOD
      USE TRI7BOX
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
#include      "com08_c.inc"
#include      "fxbcom.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "intstamp_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPARI(NPARI,NINTER),TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER(NINTER*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD)
C     REAL
      my_real
     .   SENSOR(NSENSR,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LEN, NI, NIN ,IDEB, IDEB2, P,I,
     .        IFLAGLOADP, LOC_PROC, MSGTYP,LENI,
     .        MSGOFF,IERROR, IERROR2, REQ_S(NSPMD), IDEBUT2(NINTER),IDEBUT(NINTER),
     .        STATUS(MPI_STATUS_SIZE),  ISENS,INTERACT,IFORM,NOD,N,
     .        NODSITOT,NODSI(NINTSTAMP),NIACTIF,INTERACTIF(NINTSTAMP),
     .        LENS(NSPMD),LENR(NSPMD),IADS(NSPMD),IADR(NSPMD),NODFITOT,
     .        NODFI(NINTSTAMP),L,
     .        REQ_SI(NSPMD),siztemp(NSPMD),
     .        REQ_R(NSPMD),SIZ, J, K, IALLOCS, IALLOCR, NIF, NB,PP,PPL,NP
      DATA MSGOFF/2016/
      LOGICAL ITEST

      INTEGER,DIMENSION(:), ALLOCATABLE :: RBUFS, RBUFR
C     REAL
      my_real
     .   STARTT,STOPT,DIST,TS
C------------------------------------------------------------------------
      IF(NSPMD==1) RETURN
C
      LOC_PROC = ISPMD+1
C
      NIACTIF = 0 ! Number of actif and Tri interfaces
      NODSITOT = 0  ! Global Number of remote main node
      NODFITOT = 0  ! Global Number of remote main node
C 
      LENS(1:NSPMD) = 0
      LENR(1:NSPMD) = 0

      DO NI = 1, NINTSTAMP
         NIN = INTSTAMP(NI)%NOINTER
         ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined
         INTERACT = 0

         IF(ISENS/=0) THEN             ! Sensor ID
           TS = SENSOR(2,ISENS)
           IF (TT>=TS) INTERACT = 1
         ELSE
           STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
         ENDIF
C
         IFLAGLOADP = IPARI(95,NIN)
C
         IF (INTERACT/=0.AND.IFLAGLOADP > 0)THEN
             NIACTIF = NIACTIF + 1
             INTERACTIF(NIACTIF) = NIN
             NODSI(NIACTIF) = 0
             NODFI(NIACTIF) = 0
             DO P=1,NSPMD
                NODSI(NIACTIF) = NODSI(NIACTIF) + NMNSI(NIN)%P(P)
                NODFI(NIACTIF) = NODFI(NIACTIF) + NMNFI(NIN)%P(P)
                LENS(P) = LENS(P) + NMNSI(NIN)%P(P)
                LENR(P) = LENR(P) + NMNFI(NIN)%P(P)
             ENDDO
            NODSITOT = NODSITOT + NODSI(NIACTIF)
            NODFITOT = NODFITOT + NODFI(NIACTIF)
         ENDIF
      ENDDO

      IF(NIACTIF /= 0 ) THEN
C alloc comm structure      
         ALLOCATE(RBUFS(2*NSPMD*NODFITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF

         ALLOCATE(RBUFR(2*NSPMD*NODSITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
           CALL ARRET(2)
         ENDIF
C FILL comm structure 
         IDEB = 0
         DO NI = 1, NIACTIF
            IDEBUT2(NI) = 0
         ENDDO
         IF(NODFITOT/= 0) THEN
          DO P = 1, NSPMD
            IADS(P) = IDEB +1
            IF(P/= LOC_PROC.AND.LENR(P)/= 0)THEN
              DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNFI(NIN)%P(P)
                IF(LEN /= 0) THEN
                  IDEB2 = IDEBUT2(NI)
                  DO I = 1,LEN
                     RBUFS(IDEB+1)= NMVFI(NIN)%P(IDEB2+I)
                     RBUFS(IDEB+2)= TAGNCONTFI(NIN)%P(IDEB2+I)
                     TAGNCONTFI(NIN)%P(IDEB2+I)= 0
                     IDEB = IDEB + 2
                  ENDDO
                  IDEBUT2(NI) = IDEBUT2(NI) + LEN
                ENDIF
              ENDDO
C SEND comm structure 

              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     S         RBUFS(IADS(P)),2*LENR(P),REAL,IT_SPMD(P),MSGTYP,
     G         MPI_COMM_WORLD,REQ_S(P),IERROR)
            ENDIF
          ENDDO
         ENDIF

         IADR(1) = 1
         DO P=1,NSPMD-1
            IADR(P+1) =IADR(P)+2*LENS(P)
         ENDDO
C RECEIVE comm structure 
         DO NI = 1, NIACTIF
            IDEBUT2(NI) = 0
         ENDDO

         IF(NODSITOT /=0) THEN
             DO P=1,NSPMD
               IF(P/= LOC_PROC.AND.LENS(P)/= 0)THEN
                 MSGTYP = MSGOFF
                 CALL MPI_RECV(RBUFR(IADR(P)),2*LENS(P),REAL,IT_SPMD(P),
     .                     MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
                 IDEB= IADR(P)-1

                 DO NI = 1, NIACTIF
                   NIN = INTERACTIF(NI)
                   LEN = NMNSI(NIN)%P(P)
                   IF(LEN /= 0) THEN
                     IDEB2 = IDEBUT2(NI)
                     DO I = 1,LEN
                        N = RBUFR(IDEB+1)
                        NOD = INTBUF_TAB(NIN)%MSR_L(N)
                        TAGNCONT(PPL,NOD) = MAX(TAGNCONT(PPL,NOD), RBUFR(IDEB+2))
                        DO NP = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1)
                           PP = LOADPINTER(NP)
                           PPL = LOADP_HYD_INTER(PP)
                           TAGNCONT(PPL,NOD) = MAX(TAGNCONT(PPL,NOD), RBUFR(IDEB+2))
                        ENDDO
                     ENDDO
                     IDEB = IDEB + 2	
                   ENDIF
                 ENDDO
                 IDEBUT2(NI) = IDEBUT2(NI) + LEN
               ENDIF
             ENDDO
           ENDIF

C WAITING for receiving msg
         DO P = 1, NSPMD
             IF(P/= LOC_PROC.AND.LENR(P)/= 0)THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
             ENDIF
          ENDDO
          IF(ALLOCATED(RBUFS)) DEALLOCATE(RBUFS)
          IF(ALLOCATED(RBUFR)) DEALLOCATE(RBUFR)
       ENDIF
C

#endif
      RETURN
      END
