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
Chd|  SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|-- called by -----------
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTCONTP                      source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI20                       source/mpi/interfaces/spmd_i7tool.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I17FROTS_PON(
     1      NSKYI17 ,ISKYI17,FSKYI17,NRSKYI17,IRSKYI17,
     2      FRSKYI17,NIN    ,LSKYI17,NOINT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
      INTEGER NSKYI17, NRSKYI17, NIN, LSKYI17, NOINT,
     .        ISKYI17(*), IRSKYI17(*)
      my_real
     .        FSKYI17(LSKYI17,4),
     .        FRSKYI17(4,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, LL, NB, LEN, SIZ, LOC_PROC,
     .        IDEB, N, MSGTYP, IERROR, IDEBI, NI, NOD,
     .        IALLOCS, IALLOCR, IES, I, NN, MSGOFF, MSGOFF2,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_SI(NSPMD),REQ_S(NSPMD),REQ_R(NSPMD),
     .        ISIZRCV(2,NSPMD),ISIZENV(2,NSPMD),
     .        NSNFITOT(NSPMD),NSNSITOT(NSPMD)
      DATA MSGOFF/146/
      DATA MSGOFF2/147/
      LOGICAL ITEST
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      LEN = 5
C
C Init + ireceive sur taille communication
C
        DO P = 1, NSPMD
          ISIZRCV(1,P)=0
          ISIZRCV(2,P)=0
          ISIZENV(1,P) = 0
          ISIZENV(2,P) = 0
          NSNFITOT(P) = 0
          NSNSITOT(P) = 0
          IF(P/=LOC_PROC)THEN
            SIZ = NSNSI(NIN)%P(P)
            IF(SIZ>0)THEN
              NSNSITOT(P) = SIZ
              MSGTYP = MSGOFF
              CALL MPI_IRECV(
     .          ISIZRCV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_R(P),IERROR   )
            ENDIF
          ENDIF
        ENDDO
C
C Partie 1 envoi et preparation buffer reception
C

C
        IF(NRSKYI17>0) THEN
          CALL SORTI20(NRSKYI17,IRSKYI17,FRSKYI17,4)
        END IF
C precomptage du nombre de contacts par processeur+calcul nsnfi total
        CALL INTCONTP(
     +    NRSKYI17,IRSKYI17,NSNFI(NIN)%P(1),ISIZENV,NSNFITOT,LEN)
C
        IALLOCS = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.NSNFITOT(P)>0) THEN
            MSGTYP = MSGOFF
            CALL MPI_ISEND(
     .        ISIZENV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_S(P),IERROR    )
            IALLOCS = IALLOCS + ISIZENV(1,P)
          ENDIF
        END DO
        IERROR=0
        IF(IALLOCS>0)
     +    ALLOCATE(BBUFS(IALLOCS+NSPMD),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
C Send
C
        IDEB = 0
        IDEBI = 1
        L = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.ISIZENV(1,P)>0)THEN
            ADD = L+1
            NB = NSNFI(NIN)%P(P)
            IF(NB>0) THEN
              LL = L+1
              L = L + 1
              DO N = 1, NB
                IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C facette element generant une force
                  IES = -NSVFI(NIN)%P(IDEB+N)
                  IF(IDEBI<=NRSKYI17) THEN
                    ITEST = IRSKYI17(IDEBI)==IDEB+N
                  ELSE
                    ITEST = .FALSE.
                  ENDIF
                  DO WHILE(ITEST)
                    BBUFS(L+1) = IES
                    BBUFS(L+2) = FRSKYI17(1,IDEBI)
                    BBUFS(L+3) = FRSKYI17(2,IDEBI)
                    BBUFS(L+4) = FRSKYI17(3,IDEBI)
                    BBUFS(L+5) = FRSKYI17(4,IDEBI)
                    IDEBI = IDEBI + 1
                    L = L + LEN
                    IF(IDEBI<=NRSKYI17) THEN
                      ITEST = IRSKYI17(IDEBI)==IDEB+N
                    ELSE
                      ITEST = .FALSE.
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
              BBUFS(LL) = (L-LL)/LEN
              IDEB = IDEB + NB
            END IF
            SIZ = L+1-ADD
            MSGTYP = MSGOFF2 
            CALL MPI_ISEND(
     .        BBUFS(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_SI(P),IERROR    )
          ELSEIF(P/=LOC_PROC)THEN
            IDEB = IDEB + NSNFI(NIN)%P(P)
          END IF
        END DO
C
C Receive 1er message : taille communication
C
        IALLOCR = 0
        DO P = 1, NSPMD
          IF(NSNSITOT(P)>0)THEN
            CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            IALLOCR = MAX(IALLOCR,ISIZRCV(1,P))   ! pour comm bloquantes
          END IF
        END DO
C
        IERROR=0
        IF(IALLOCR>0)
     .    ALLOCATE(BBUFR(IALLOCR+1),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Reception buffer et decompactage
C
        DO P = 1, NSPMD
          IF(ISIZRCV(1,P)>0) THEN
            MSGTYP = MSGOFF2
            L = 1
            CALL MPI_RECV(
     .        BBUFR(L),ISIZRCV(1,P)+1,REAL  ,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD       ,STATUS,IERROR    )
C
            IF(NSNSI(NIN)%P(P)>0)THEN
              NB = NINT(BBUFR(L))
              L = L + 1
C
             IF (NSKYI17+NB > LSKYI17) THEN
               CALL ANCMSG(MSGID=25,ANMODE=ANINFO_BLIND,
     .                     I1=NOINT)
               CALL ARRET(2)
             ENDIF
C
              DO I = 1, NB
                NN = NINT(BBUFR(5*(I-1)+L))
                NSKYI17 = NSKYI17+1
                ISKYI17(NSKYI17)=NN
                FSKYI17(NSKYI17,1)=BBUFR(5*(I-1)+L+1)
                FSKYI17(NSKYI17,2)=BBUFR(5*(I-1)+L+2)
                FSKYI17(NSKYI17,3)=BBUFR(5*(I-1)+L+3)
                FSKYI17(NSKYI17,4)=BBUFR(5*(I-1)+L+4)
              END DO
              L = L + NB*LEN
            END IF
          ENDIF
        ENDDO
        IF(IALLOCR>0) DEALLOCATE(BBUFR)
C
C Attente ISEND
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            IF(NSNFITOT(P)>0) THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
            END IF
            IF(ISIZENV(1,P)>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            END IF
          END IF
        END DO
        IF(IALLOCS>0) DEALLOCATE(BBUFS)
C
#endif
      RETURN
      END
