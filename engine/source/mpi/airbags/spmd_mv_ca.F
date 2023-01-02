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
Chd|  SPMD_MV_CA                    source/mpi/airbags/spmd_mv_ca.F
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MV_CA(
     1   FR_MV ,IV    ,NAV    ,RVOLU   ,RVOLUV ,
     2   ICBAG ,NJET  ,IVOLUV ,RBAGVJET,IFLAG  ,ITYP,NGASES)
C communication pression volume pour airbags communicants
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG, IV, NAV, NJET,
     .        FR_MV(NSPMD+2,NVOLU),
     .        ICBAG(NICBAG,*),IVOLUV(NIMV,*),ITYP,NGASES
      my_real
     .        RVOLU(*), RVOLUV(NRVOLU,*),RBAGVJET(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,I,J,II,P,L,SIZ,NB,IJ,RADVOIS,
     .        MSGTYP,MSGOFF,MSGOFF2,IERROR, ICOMRC,
     .        ICOMSD(NSPMD),REQ_S(NSPMD),IGAS,
     .        STATUS(MPI_STATUS_SIZE)
      DATA MSGOFF/114/
      DATA MSGOFF2/115/
      my_real, DIMENSION(:),ALLOCATABLE :: BUFS,BUFR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      LOC_PROC = ISPMD+1
      SIZ = 5*NVOLU+2*NJET*NVOLU+1
      IF(ITYP == 9) SIZ = SIZ + NGASES*2*NJET*NVOLU
      ALLOCATE(BUFS(SIZ),BUFR(SIZ))


      IF(IFLAG==1) THEN
        L = 1
        NB = 0
        DO P = 1, NSPMD
          ICOMSD(P) = 0
        END DO
        DO I=1,NAV
          II = ICBAG(1,I)
C si pmain
          IF(FR_MV(NSPMD+2,II)==LOC_PROC)THEN
            NB = NB + 1
            BUFS(L+1)=II
            BUFS(L+2)=RVOLUV(12,II)
            BUFS(L+3)=RVOLUV(16,II)
            BUFS(L+4)=RVOLUV(22,II)
            BUFS(L+5)=RVOLUV(24,II)
            L = L + 5
C
            RADVOIS= IVOLUV(10,II)
            DO IJ = 1, NJET
              BUFS(L+1)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9)
              BUFS(L+2)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)
              L = L + 2
              IF(ITYP == 9) THEN
               DO IGAS = 1,NGASES 
                 BUFS(L+1)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+23+(IGAS-1)*4)
                 BUFS(L+2)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+24+(IGAS-1)*4)
                 L = L + 2
               ENDDO
              ENDIF
            END DO
C
            DO P = 1, NSPMD
              IF(FR_MV(P,II)==0.AND.FR_MV(P,IV)/=0) THEN
                ICOMSD(P) = 1
              END IF
            END DO
          END IF
        END DO
C
        IF (NB>0) THEN
          BUFS(1) = NB
          DO P = 1, NSPMD
            IF(ICOMSD(P)==1) THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BUFS,L,REAL,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_S(P),IERROR)
            END IF
          END DO
        END IF
C
        IF(FR_MV(LOC_PROC,IV)/=0) THEN
          DO P = 1, NSPMD
            ICOMRC = 0
            DO I=1,NAV
              II = ICBAG(1,I)
C si pmain
              IF(FR_MV(NSPMD+2,II)==P.AND.
     +           FR_MV(LOC_PROC,II)==0)THEN
                ICOMRC = 1
              END IF
            END DO
            IF(ICOMRC==1) THEN
              SIZ = 5*NVOLU+2*NJET*NVOLU+1
              IF(ITYP == 9) SIZ = SIZ + 2*NJET*NVOLU*NGASES
              MSGTYP = MSGOFF
              CALL MPI_RECV(BUFR   ,SIZ           ,REAL  ,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
              NB = BUFR(1)
              L = 1
              DO I = 1, NB
                II = NINT(BUFR(L+1))
                RVOLUV(12,II) = BUFR(L+2)
                RVOLUV(16,II) = BUFR(L+3)
                RVOLUV(22,II) = BUFR(L+4)
                RVOLUV(24,II) = BUFR(L+5)
                L = L + 5
C
                RADVOIS= IVOLUV(10,II)
                DO IJ = 1, NJET
                  RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9) =BUFR(L+1)
                  RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)=BUFR(L+2)
                  L = L + 2
                  IF(ITYP == 9) THEN
                    DO IGAS = 1,NGASES 
                      RBAGVJET(RADVOIS+NRBJET*(IJ-1)+23+(IGAS-1)*4)=BUFR(L+1)
                      RBAGVJET(RADVOIS+NRBJET*(IJ-1)+24+(IGAS-1)*4)=BUFR(L+2)
                      L = L + 2
                    END DO
                  END IF
                END DO
              END DO
            END IF
          END DO
        END IF
C
        DO P = 1, NSPMD
          IF(ICOMSD(P)==1) CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
        END DO
      ELSE !IFLAG /= 1
C cas renvoi des pressions du pmain du mv vers les processeurs traitant les mv voisins
        IF(FR_MV(NSPMD+2,IV)==LOC_PROC)THEN
C
          DO P = 1, NSPMD
            ICOMSD(P) = 0
          END DO
C si pmain
          L = 0
          DO I=1,NAV
            II = ICBAG(1,I)
            BUFS(L+1)=RVOLUV(22,II)
            BUFS(L+2)=RVOLUV(24,II)
            L = L + 2
C
            RADVOIS= IVOLUV(10,II)
            DO IJ = 1, NJET
              BUFS(L+1)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9)
              BUFS(L+2)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)
              L = L + 2
              IF(ITYP == 9) THEN
               DO IGAS = 1,NGASES 
                 BUFS(L+1)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+23+(IGAS-1)*4)
                 BUFS(L+2)=RBAGVJET(RADVOIS+NRBJET*(IJ-1)+24+(IGAS-1)*4)
                 L = L + 2
               ENDDO
              ENDIF
            END DO
C
            DO P = 1, NSPMD
              IF(FR_MV(P,II)/=0.AND.FR_MV(P,IV)==0) THEN
                ICOMSD(P) = 1
              END IF
            END DO
          END DO
          IF (L>0) THEN
           DO P = 1, NSPMD
             IF(ICOMSD(P)==1) THEN
               MSGTYP = MSGOFF2
               CALL MPI_ISEND(
     .           BUFS,L,REAL,IT_SPMD(P),MSGTYP,
     .           MPI_COMM_WORLD,REQ_S(P),IERROR)
             END IF
           END DO
         END IF
C
         DO P = 1, NSPMD
           IF(ICOMSD(P)==1) CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
         END DO
        ELSEIF(FR_MV(LOC_PROC,IV)==0) THEN
          ICOMRC = 0
          DO I=1,NAV
            II = ICBAG(1,I)
            IF(FR_MV(LOC_PROC,II)/=0)THEN
              ICOMRC = 1
            END IF
          END DO
          IF(ICOMRC==1) THEN
C pmain
            P = FR_MV(NSPMD+2,IV)
            SIZ = 2*NAV+2*NJET*NAV
            IF(ITYP == 9) SIZ = SIZ +2*NJET*NAV*NGASES
            MSGTYP = MSGOFF2
            CALL MPI_RECV(BUFR   ,SIZ           ,REAL  ,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
            L = 0
            DO I = 1, NAV
              II = ICBAG(1,I)
              RVOLUV(22,II) = BUFR(L+1)
              RVOLUV(24,II) = BUFR(L+2)
              L = L + 2
C
              RADVOIS= IVOLUV(10,II)
              DO IJ = 1, NJET
                RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9) =BUFR(L+1)
                RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)=BUFR(L+2)
                L = L + 2
                IF(ITYP == 9) THEN
                  DO IGAS=1,NGASES
                    RBAGVJET(RADVOIS+NRBJET*(IJ-1)+23+(IGAS-1)*4)=BUFR(L+1)
                    RBAGVJET(RADVOIS+NRBJET*(IJ-1)+24+(IGAS-1)*4)=BUFR(L+2)
                    L = L + 2
                  ENDDO
                ENDIF
              END DO
            END DO
          END IF
        END IF
      END IF
C
      DEALLOCATE(BUFR,BUFS)
#endif
      RETURN
      END
