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
Chd|  SPMD_I20NORMF                 source/mpi/interfaces/spmd_i20normf.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I20NORMF(
     1 SOLIDN_NORMAL,SOLIDN_NORMAL_F,SOLIDN_NORMAL_FE,NIN     ,IRLEN20 ,
     2 ISLEN20      ,IRLEN20T       ,ISLEN20T        ,IRLEN20E,ISLEN20E,
     3 NSV          ,NLG            ,ISLINS          )
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,
     .        IRLEN20E,ISLEN20E,NIN,
     .        NSV(*), NLG(*), ISLINS(2,*)
      INTEGER
     .        SOLIDN_NORMAL(3,*),
     .        SOLIDN_NORMAL_F(3,*), SOLIDN_NORMAL_FE(3,*)

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, NB, SIZ, LOC_PROC, I, NOD, IL, IL1, IL2,
     .        IDEB, N, MSGTYP, IERROR, MSGOFF,
     .        N1, N2, IALLOCS, IALLOCR, LEN20, LEN20E,
     .        DEBUT, DEBUTE,
     .        STATUS(MPI_STATUS_SIZE),
     .        ADDS(NSPMD+1), ADDR(NSPMD+1),
     .        REQ_SI(NSPMD),REQ_RI(NSPMD),ISTOCOM
      INTEGER       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      DATA MSGOFF/164/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      LEN20  = 3
      LEN20E = 6
      ISTOCOM=0
C
      IALLOCS = LEN20*ISLEN20 + LEN20*ISLEN20T + LEN20E*ISLEN20E
      IERROR=0
      IF(IALLOCS>0)
     +  ALLOCATE(BBUFS(IALLOCS),STAT=IERROR)
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
C
      IALLOCR = LEN20*IRLEN20 + LEN20*IRLEN20T + LEN20E*IRLEN20E
      IERROR=0
      IF(IALLOCR>0)
     +  ALLOCATE(BBUFR(IALLOCR),STAT=IERROR)
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      END IF
C
C Receive
C
      L = 0
      DO P = 1, NSPMD
        ADD = L+1
        ADDR(P) = ADD
        SIZ = 0
        IF(P/=LOC_PROC)THEN
          NB  = NSNFI(NIN)%P(P)
          L = L + NB*LEN20
C ajout partie edge pour type 20
          NB = NSNFIE(NIN)%P(P)
          L = L + NB*LEN20E
          SIZ = L+1-ADD
          IF(SIZ>0)THEN
            MSGTYP = MSGOFF
            CALL MPI_IRECV(
     .          BBUFR(ADD),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RI(P),IERROR    )
          ENDIF
        ENDIF
      ENDDO
      ADDR(NSPMD+1) = ADDR(NSPMD)+SIZ
      IF(L>0) THEN
        ISTOCOM = 1
      ENDIF
C
C Send
C
      DEBUT=0
      DEBUTE=0
      L = 0
      DO P = 1, NSPMD
        ADD = L+1
        ADDS(P) = ADD
        SIZ = 0
        IF(P/=LOC_PROC)THEN
          IDEB = DEBUT
          NB = NSNSI(NIN)%P(P)
          DO I = 1, NB
            N = NSVSI(NIN)%P(IDEB+I)
            IL  = NSV(N)
            NOD = NLG(IL)
            BBUFS(L+1) = SOLIDN_NORMAL(1,NOD)
            BBUFS(L+2) = SOLIDN_NORMAL(2,NOD)
            BBUFS(L+3) = SOLIDN_NORMAL(3,NOD)
            L = L + LEN20
          ENDDO
          DEBUT=DEBUT+NB
C Extra Code for Type20 Edge
C
          NB = NSNSIE(NIN)%P(P)
          IDEB = DEBUTE
          DO I = 1, NB
            N = NSVSIE(NIN)%P(IDEB+I)
            IL1 = ISLINS(1,N)
            NOD = NLG(IL1)
            BBUFS(L+1) = SOLIDN_NORMAL(1,NOD)
            BBUFS(L+2) = SOLIDN_NORMAL(2,NOD)
            BBUFS(L+3) = SOLIDN_NORMAL(3,NOD)
            IL2 = ISLINS(2,N)
            NOD = NLG(IL2)
            BBUFS(L+4) = SOLIDN_NORMAL(1,NOD)
            BBUFS(L+5) = SOLIDN_NORMAL(2,NOD)
            BBUFS(L+6) = SOLIDN_NORMAL(3,NOD)
            L = L + LEN20E
          ENDDO
          DEBUTE=DEBUTE+NB
C end i20 edge
          SIZ = L+1-ADD
          IF(SIZ>0)THEN
            MSGTYP = MSGOFF
C BUFR : reception partie force et envoi partie vitesse
            CALL MPI_ISEND(
     .        BBUFS(ADD),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_SI(P),IERROR    )
          ENDIF
        ENDIF
      ENDDO
      ADDS(NSPMD+1)=ADDS(NSPMD)+SIZ
C
C Attente reception buffer et decompactage
C
      IF(ISTOCOM==1)THEN
C
        DEBUT = 0
        DEBUTE= 0
C
C Attente IRECV
C
        DO P = 1, NSPMD
          IF(ADDR(P+1)-ADDR(P)>0) THEN
            CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
            L = ADDR(P)-1
            NB = NSNFI(NIN)%P(P)
            IF(NB>0)THEN
              IDEB = DEBUT
              DO I = 1, NB
                SOLIDN_NORMAL_F(1,I+IDEB) = BBUFR(L+1)
                SOLIDN_NORMAL_F(2,I+IDEB) = BBUFR(L+2)
                SOLIDN_NORMAL_F(3,I+IDEB) = BBUFR(L+3)
                L = L + LEN20
              ENDDO
              DEBUT = DEBUT + NB
            ENDIF
C
C Extra Code for Type20 Edge
C
            NB = NSNFIE(NIN)%P(P)
            IF(NB>0)THEN
              IDEB = DEBUTE
              DO I = 1, NB
                N1 = 2*(I+IDEB-1)+1
                N2 = 2*(I+IDEB)
                SOLIDN_NORMAL_FE(1,N1) = BBUFR(L+1)
                SOLIDN_NORMAL_FE(2,N1) = BBUFR(L+2)
                SOLIDN_NORMAL_FE(3,N1) = BBUFR(L+3)
                SOLIDN_NORMAL_FE(1,N2) = BBUFR(L+4)
                SOLIDN_NORMAL_FE(2,N2) = BBUFR(L+5)
                SOLIDN_NORMAL_FE(3,N2) = BBUFR(L+6)
                L = L + LEN20E
              ENDDO
              DEBUTE = DEBUTE + NB
            ENDIF
C end extra for i20 edge
          END IF
        END DO
      END IF
C
      IF(IALLOCR>0)THEN
        DEALLOCATE(BBUFR)
        IALLOCR=0
      END IF
C
C Attente ISEND
C
      DO P = 1, NSPMD
        IF(ADDS(P+1)-ADDS(P)>0) THEN
          CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
        ENDIF
      ENDDO
C
      IF(IALLOCS>0)THEN
        DEALLOCATE(BBUFS)
        IALLOCS=0
      END IF
C
#endif
      RETURN
      END
