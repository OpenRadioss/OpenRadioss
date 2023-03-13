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
Chd|  SPMD_EXCH_PRESS               source/mpi/interfaces/spmd_exch_press.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_PRESS(
     1    IPARI   ,INTLIST ,NBINTC  ,FNCONT ,
     2    FTCONT  ,ISLEN7 ,IRLEN7  ,IRLEN7T ,ISLEN7T,
     3    IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,INTBUF_TAB,
     4    N_CSE_FRIC_INTER, N_SCAL_CSE_EFRIC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE OUTPUTS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "assert.inc"
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
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBINTC,ISLEN7, IRLEN7,IRLEN7T, ISLEN7T,
     .        IRLEN20, ISLEN20, IRLEN20T, ISLEN20T,
     .        IPARI(NPARI,*), INTLIST(*)
      INTEGER  , INTENT(IN) :: N_SCAL_CSE_EFRIC,N_CSE_FRIC_INTER(NINTER)
      my_real
     .        FNCONT(3,*), FTCONT(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, LL, NB, LEN, SIZ, KFI, LOC_PROC, MULTIMP, II,
     .        NIN, IDEB, N, MSGTYP, IERROR, NI, NOD, I,
     .        NTY, IALLOCS, IALLOCR, MSGOFF,INTERFRIC,LF,
     .        STATUS(MPI_STATUS_SIZE),DEBUT(NINTER),
     .        ADDS(NSPMD+1), ADDR(NSPMD+1),
     .        REQ_SI(NSPMD),REQ_RI(NSPMD)
      DATA MSGOFF/190/
      LOGICAL ITEST
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        LOC_PROC = ISPMD + 1
C
        LEN = 7
        IF(NINEFRIC > 0) LEN = LEN +1
        IF(N_SCAL_CSE_EFRIC > 0)  LEN = LEN +1
C
C Partie 1 envoi et preparation buffer reception
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN) = 0
        ENDDO
        IALLOCS = LEN*(IRLEN7+IRLEN25) + LEN*(IRLEN7T+IRLEN25T)  + LEN*IRLEN20 + LEN*IRLEN20T
        IERROR=0
        IF(IALLOCS>0)
     +  ALLOCATE(BBUFS(IALLOCS+NBINTC*NSPMD),STAT=IERROR) ! nbintc*NSPMD majorant place supplementaire bufs
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
        IALLOCR = LEN*(ISLEN7+ISLEN25) + LEN*(ISLEN7T+ISLEN25T) + LEN*ISLEN20 + LEN*ISLEN20T
        IERROR=0
        IF(IALLOCR>0)
     +    ALLOCATE(BBUFR(IALLOCR+NBINTC*NSPMD),STAT=IERROR) ! nbintc*NSPMD majorant place supplementaire bufs
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
        ASSERT(IRLEN25 >= 0)
C
C Receive
C
        L = 0
        DO P = 1, NSPMD
          ADD = L+1
          ADDR(P) = ADD
          SIZ = 0
          IF(P/=LOC_PROC)THEN
Cel test en plus pour savoir si com globale necessaire entre les 2 procs
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB = NSNSI(NIN)%P(P)
              NTY = IPARI(7,NIN)
              IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==22.OR.
     .           NTY==23.OR.NTY==24.OR.NTY==25) THEN
                IF(NB>0) THEN
                  L = L + 1 + NB*LEN
                ENDIF
              ENDIF
            ENDDO
            SIZ = L+1-ADD
            ASSERT(ADD + SIZ -1 <= IALLOCR+NBINTC*NSPMD)
            IF(SIZ>0)THEN
              MSGTYP = MSGOFF
              CALL MPI_IRECV(
     .          BBUFR(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RI(P),IERROR    )
            ENDIF
          ENDIF
        ENDDO
        ADDR(NSPMD+1) = ADDR(NSPMD)+SIZ
C
C Send
C
        L = 0
        DO P = 1, NSPMD
          ADD = L+1
          ADDS(P) = ADD
          SIZ = 0
          IF(P/=LOC_PROC)THEN
Cel test en plus pour savoir si com globale necessaire entre les 2 procs
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IDEB = DEBUT(NIN)
              NB = NSNFI(NIN)%P(P)
              NTY  = IPARI(7,NIN)
              INTERFRIC = N_CSE_FRIC_INTER(NIN)
              IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==22.OR.
     .           NTY==23.OR.NTY==24.OR.NTY==25) THEN
               IF(NB>0) THEN
                LL = L+1
                L = L + 1
                DO N = 1, NB
                  IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C node generating force
                    BBUFS(L+1) = -NSVFI(NIN)%P(IDEB+N)
                    BBUFS(L+2) = FNCONTI(NIN)%P(1,IDEB+N)
                    BBUFS(L+3) = FNCONTI(NIN)%P(2,IDEB+N)
                    BBUFS(L+4) = FNCONTI(NIN)%P(3,IDEB+N)
                    BBUFS(L+5) = FTCONTI(NIN)%P(1,IDEB+N)
                    BBUFS(L+6) = FTCONTI(NIN)%P(2,IDEB+N)
                    BBUFS(L+7) = FTCONTI(NIN)%P(3,IDEB+N)
                    LF = 7
                    IF(INTERFRIC>0) THEN
                        BBUFS(L+LF+1) = EFRICFI(NIN)%P(IDEB+N)
                        EFRICFI(NIN)%P(IDEB+N) = ZERO
                        LF=LF+1
                    ELSEIF(NINEFRIC>0) THEN
                        BBUFS(L+LF+1) = ZERO
                        LF=LF+1
                    ENDIF
                    IF(N_SCAL_CSE_EFRIC>0) THEN
                        BBUFS(L+LF+1) = EFRICGFI(NIN)%P(IDEB+N)
                        EFRICGFI(NIN)%P(IDEB+N) = ZERO
                    ENDIF
                    FNCONTI(NIN)%P(1,IDEB+N) = ZERO
                    FNCONTI(NIN)%P(2,IDEB+N) = ZERO
                    FNCONTI(NIN)%P(3,IDEB+N) = ZERO
                    FTCONTI(NIN)%P(1,IDEB+N) = ZERO
                    FTCONTI(NIN)%P(2,IDEB+N) = ZERO
                    FTCONTI(NIN)%P(3,IDEB+N) = ZERO  
                    L = L + LEN
                  ELSEIF(INTERFRIC > 0.OR.N_SCAL_CSE_EFRIC>0) THEN
C node not generating force but energy is cumulated
                    BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                    BBUFS(L+2) = ZERO
                    BBUFS(L+3) = ZERO
                    BBUFS(L+4) = ZERO
                    BBUFS(L+5) = ZERO
                    BBUFS(L+6) = ZERO
                    BBUFS(L+7) = ZERO
                    LF = 7
                    IF(INTERFRIC>0) THEN
                        BBUFS(L+LF+1) = EFRICFI(NIN)%P(IDEB+N)
                        EFRICFI(NIN)%P(IDEB+N) = ZERO
                        LF=LF+1
                    ELSEIF(NINEFRIC>0) THEN
                        BBUFS(L+LF+1) = ZERO
                        LF=LF+1
                    ENDIF
                    IF(N_SCAL_CSE_EFRIC>0) THEN
                        BBUFS(L+LF+1) = EFRICGFI(NIN)%P(IDEB+N)
                        EFRICGFI(NIN)%P(IDEB+N) = ZERO
                    ENDIF
                    L = L + LEN
                  ENDIF
                ENDDO
                BBUFS(LL) = (L-LL)/LEN
                DEBUT(NIN) = DEBUT(NIN) + NB
               END IF
              END IF
            ENDDO
            SIZ = L+1-ADD
            IF(SIZ>0)THEN
              MSGTYP = MSGOFF 
              ASSERT(ADD + SIZ -1 <= IALLOCS+NBINTC*NSPMD)
              CALL MPI_ISEND(
     .          BBUFS(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
            ENDIF
          ENDIF
        ENDDO
        ADDS(NSPMD+1)=ADDS(NSPMD)+SIZ
C
C Attente reception buffer et decompactage
C
C
C Attente IRECV
C
        DO P = 1, NSPMD
          IF(ADDR(P+1)-ADDR(P)>0) THEN
            CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
            L = ADDR(P)
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IF(NSNSI(NIN)%P(P)>0)THEN
                NTY   =IPARI(7,NIN)
                INTERFRIC = N_CSE_FRIC_INTER(NIN)
                IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==22.OR.
     .             NTY==23.OR.NTY==24.OR.NTY==25)THEN
                  NB = NINT(BBUFR(L))
                  L = L + 1
                  DO I = 1, NB
                    N = NINT(BBUFR(L+LEN*(I-1)))
                    NOD = INTBUF_TAB(NIN)%NSV(N)
C ----
C T24 E2E Fictive nodes may have Node ID over NUMNOD
C 
                    IF(NOD<=NUMNOD)THEN
                      FNCONT(1,NOD) = FNCONT(1,NOD) + BBUFR(L+LEN*(I-1)+1)
                      FNCONT(2,NOD) = FNCONT(2,NOD) + BBUFR(L+LEN*(I-1)+2)
                      FNCONT(3,NOD) = FNCONT(3,NOD) + BBUFR(L+LEN*(I-1)+3)
                      FTCONT(1,NOD) = FTCONT(1,NOD) + BBUFR(L+LEN*(I-1)+4)
                      FTCONT(2,NOD) = FTCONT(2,NOD) + BBUFR(L+LEN*(I-1)+5)
                      FTCONT(3,NOD) = FTCONT(3,NOD) + BBUFR(L+LEN*(I-1)+6)
                      LF = 6
                      IF(INTERFRIC>0) THEN
                          EFRIC(INTERFRIC,NOD)= EFRIC(INTERFRIC,NOD)+ BBUFR(L+LEN*(I-1)+LF+1)
                          LF = LF+1
                      ENDIF
                      IF(N_SCAL_CSE_EFRIC>0) THEN
                         EFRICG(NOD)= EFRICG(NOD)+ BBUFR(L+LEN*(I-1)+LF+1)
                      ENDIF
                    ENDIF
                  ENDDO
                  L = L + NB*LEN
                END IF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C Deallocation R
        IF(IALLOCR>0) THEN
          DEALLOCATE(BBUFR)
        END IF
C
C Attente ISEND
C
        DO P = 1, NSPMD
          IF(ADDS(P+1)-ADDS(P)>0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
        ENDDO
C Deallocation S
        IF(IALLOCS>0) THEN
          DEALLOCATE(BBUFS)
        END IF
C
#endif
      RETURN
      END
