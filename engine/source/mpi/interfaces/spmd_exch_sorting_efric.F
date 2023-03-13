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
Chd|  SPMD_EXCH_SORTING_EFRIC       source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SORTING_EFRIC(
     1    IPARI   ,INTLIST ,NBINTC  ,ISLEN7 ,IRLEN7  ,
     2    IRLEN7T ,ISLEN7T ,IRLEN20 ,ISLEN20,IRLEN20T,
     3    ISLEN20T,INTBUF_TAB,H3D_DATA  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE OUTPUTS_MOD
      USE H3D_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  , INTENT(IN) ::  
     .        NBINTC,ISLEN7, IRLEN7,IRLEN7T, ISLEN7T,       
     .        IRLEN20, ISLEN20, IRLEN20T, ISLEN20T,
     .        IPARI(NPARI,NINTER), INTLIST(NBINTC)

      TYPE(INTBUF_STRUCT_),INTENT(IN) ::  INTBUF_TAB(NINTER)
      TYPE(H3D_DATABASE), INTENT(IN) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, LL, NB, LEN, SIZ, LOC_PROC, II,
     .        NIN, IDEB, N, MSGTYP, IERROR, NI, NOD, I,
     .        NTY, IALLOCS, IALLOCR, MSGOFF,INTERFRIC,
     .        NBEFRIC,
     .        STATUS(MPI_STATUS_SIZE),DEBUT(NINTER),
     .        ADDS(NSPMD+1), ADDR(NSPMD+1),
     .        REQ_SI(NSPMD),REQ_RI(NSPMD),INTSORT(NBINTC)
      DATA MSGOFF/190/
      LOGICAL ITEST
      my_real DIST
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      LOGICAL :: IS_EFRIC_COM_NEEDED
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        LOC_PROC = ISPMD + 1
C
        LEN = 3
C
C Part 1 : prepare send/reception buffers and if comm is needed 
C
        INTSORT(1:NBINTC) = 0
        NBEFRIC = 0 
        IS_EFRIC_COM_NEEDED = .FALSE.
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          NTY = IPARI(7,NIN)
          DIST = INTBUF_TAB(NIN)%VARIABLES(5)
          IF(NTY==7.OR.NTY==24) THEN ! only Inter type 7 our 24, Inter type 25 is already done each cycle
            IF (DIST<=ZERO) THEN ! only for interfaces that will be sorted
               INTERFRIC = H3D_DATA%N_CSE_FRIC_INTER(NIN)
               IF(H3D_DATA%N_SCAL_CSE_FRIC > 0)  THEN
                  NBEFRIC = NBEFRIC +1 
                  INTSORT(II) = 1
                  IS_EFRIC_COM_NEEDED = .TRUE.
               ELSEIF(INTERFRIC > 0)THEN
                  NBEFRIC = NBEFRIC +1 
                  INTSORT(II) = 1
                  IS_EFRIC_COM_NEEDED = .TRUE.
               ENDIF
            ENDIF
          ENDIF
          DEBUT(NIN) = 0
        ENDDO

        IF(IS_EFRIC_COM_NEEDED) THEN

           IALLOCS = LEN*IRLEN7 + LEN*IRLEN7T
           IERROR=0
           IF(IALLOCS>0)
     +        ALLOCATE(BBUFS(IALLOCS+NBINTC*NSPMD),STAT=IERROR) 
           IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           END IF
           IALLOCR = LEN*ISLEN7 + LEN*ISLEN7T
           IERROR=0
           IF(IALLOCR>0)
     +        ALLOCATE(BBUFR(IALLOCR+NBINTC*NSPMD),STAT=IERROR)
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
               DO II = 1, NBINTC
                 NIN = INTLIST(II)
                 NB = NSNSI(NIN)%P(P)
                 IF(INTSORT(II) > 0 ) THEN
                   IF(NB>0) THEN
                     L = L + 1 + NB*LEN
                   ENDIF
                 ENDIF
               ENDDO
               SIZ = L+1-ADD
               IF(SIZ>0)THEN
                 MSGTYP = MSGOFF
                 CALL MPI_IRECV(
     .             BBUFR(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .             MPI_COMM_WORLD,REQ_RI(P),IERROR    )
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
              DO II = 1, NBINTC
                NIN = INTLIST(II)
                IDEB = DEBUT(NIN)
                NB = NSNFI(NIN)%P(P)
                INTERFRIC = H3D_DATA%N_CSE_FRIC_INTER(NIN)
                IF(INTSORT(II) > 0) THEN
                  IF(NB>0) THEN
                   LL = L+1
                   L = L + 1
                   DO N = 1, NB
c                     IF(NSVFI(NIN)%P(IDEB+N)<0)THEN ! after i7com nodes really impacting are no more tagged : to be optimized
                       BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                       IF(INTERFRIC>0)  THEN
                           BBUFS(L+2) = EFRICFI(NIN)%P(IDEB+N)
                           EFRICFI(NIN)%P(IDEB+N) = ZERO
                       ELSE
                           BBUFS(L+2) = ZERO
                       ENDIF
                       IF(H3D_DATA%N_SCAL_CSE_FRIC>0) THEN
                           BBUFS(L+3) = EFRICGFI(NIN)%P(IDEB+N)
                           EFRICGFI(NIN)%P(IDEB+N) = ZERO
                       ELSE
                           BBUFS(L+3) = ZERO
                       ENDIF
                       L = L + LEN
c                     ENDIF
                   ENDDO
                   BBUFS(LL) = (L-LL)/LEN
                   DEBUT(NIN) = DEBUT(NIN) + NB
                  END IF
                END IF
              ENDDO
              SIZ = L+1-ADD
              IF(SIZ>0)THEN
                MSGTYP = MSGOFF 
                CALL MPI_ISEND(
     .            BBUFS(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .            MPI_COMM_WORLD,REQ_SI(P),IERROR    )
              ENDIF
            ENDIF
          ENDDO
          ADDS(NSPMD+1)=ADDS(NSPMD)+SIZ
C
C Waiting reception
C
          DO P = 1, NSPMD
            IF(ADDR(P+1)-ADDR(P)>0) THEN
              CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
              L = ADDR(P)
              DO II = 1, NBINTC
                NIN = INTLIST(II)
                IF(NSNSI(NIN)%P(P)>0)THEN
                  INTERFRIC = H3D_DATA%N_CSE_FRIC_INTER(NIN)
                  IF(INTSORT(II) > 0) THEN
                    NB = NINT(BBUFR(L))
                    L = L + 1
                    DO I = 1, NB
                      N = NINT(BBUFR(L+LEN*(I-1)))
                      NOD = INTBUF_TAB(NIN)%NSV(N) 
                      IF(NOD<=NUMNOD)THEN
                        IF(INTERFRIC>0)  EFRIC(INTERFRIC,NOD)= EFRIC(INTERFRIC,NOD)+ BBUFR(L+LEN*(I-1)+1)
                        IF(H3D_DATA%N_SCAL_CSE_FRIC>0) EFRICG(NOD)= EFRICG(NOD)+ BBUFR(L+LEN*(I-1)+2)
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
      ENDIF ! IS_EFRIC_COM_NEEDED
C
#endif
      RETURN
      END SUBROUTINE SPMD_EXCH_SORTING_EFRIC
