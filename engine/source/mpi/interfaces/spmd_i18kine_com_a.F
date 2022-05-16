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
Chd|  SPMD_I18KINE_COM_A            source/mpi/interfaces/spmd_i18kine_com_a.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I18KINE_COM_A(IPARI,INTBUF_TAB,
     *                A,ITAB)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),ITAB(*)
C
      my_real
     .  A(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),
     *        REQ_SI(NSPMD),REQ_RI(NSPMD)
      INTEGER P,LENSD,LENRV,IADS(NSPMD+1),IADR(NSPMD+1),IERROR,
     *        SIZ,LOC_PROC,MSGTYP,MSGOFF,IDEB(NINTER)
      INTEGER NIN,NTY,INACTI
      INTEGER J,L,NB,NN,K,N,NOD,MODE,LEN,ALEN,ND
      my_real ,
     *   DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      DATA MSGOFF/152/
C-----------------------------------------------
C Sur la type 18KINE, il faut communiquer les accelerations des nds seconds

      LOC_PROC = ISPMD+1
      IADS = 0
      IADR = 0
      LENSD = 0
      LENRV = 0

      ALEN=3
C Comptage des tailles de buffer  Receeption et envoi
      DO P=1,NSPMD
         IADR(P)=LENRV+1
         DO NIN=1,NINTER
           NTY=IPARI(7,NIN)
           INACTI =IPARI(22,NIN)
           IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .       (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7))THEN
              LENSD = LENSD + NSNSI(NIN)%P(P)*ALEN
              LENRV = LENRV + NSNFI(NIN)%P(P)*ALEN
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
      IDEB = 0
      DO P=1, NSPMD
       IADS(P)=L
       IF (P/= LOC_PROC) THEN
         DO NIN=1,NINTER
           NTY   =IPARI(7,NIN)
           INACTI =IPARI(22,NIN)
           IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .       (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7))THEN
               NB = NSNSI(NIN)%P(P)
C Preparation du send
               DO NN=1,NB
                 ND = NSVSI(NIN)%P(IDEB(NIN)+NN)
                 NOD=INTBUF_TAB(NIN)%NSV(ND)
                 BBUFS(L  )=A(1,NOD)
                 BBUFS(L+1)=A(2,NOD)
                 BBUFS(L+2)=A(3,NOD)
                 L=L+3
               ENDDO
           ENDIF
           IDEB(NIN)=IDEB(NIN)+NB
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
      IADS(NSPMD+1)=L
C Recieve
      L=0
      IDEB = 0

      DO P=1, NSPMD
          L=0
          SIZ=IADR(P+1)-IADR(P)
          IF (SIZ > 0) THEN
            MSGTYP = MSGOFF
            CALL MPI_RECV( BBUFR(IADR(P)),SIZ,REAL,IT_SPMD(P),MSGTYP,
     *                     MPI_COMM_WORLD,STATUS,IERROR )
           DO NIN=1,NINTER
             NTY   =IPARI(7,NIN)
             INACTI =IPARI(22,NIN)

             NB = NSNFI(NIN)%P(P)

             IF((NTY==7.and.IPARI(34,NIN)==-2.and.INACTI==7).OR.
     .       (NTY==22.and.IPARI(34,NIN)==-2.and.INACTI==7))THEN

               IF (NB > 0)THEN
                 DO K=1,NB
                    I18KAFI(NIN)%P(1,IDEB(NIN)+K)=BBUFR(IADR(P)+L)
                    I18KAFI(NIN)%P(2,IDEB(NIN)+K)=BBUFR(IADR(P)+L+1)
                    I18KAFI(NIN)%P(3,IDEB(NIN)+K)=BBUFR(IADR(P)+L+2)
                    L=L+3
                 ENDDO
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
          IF(SIZ.GT.0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO

      IF (ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
      IF (ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)

#endif
      RETURN
      END
