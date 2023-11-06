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
Chd|  S10CNDF1                      source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        SPMD_EXCH_A_SCND              source/mpi/elements/spmd_exch_a_scnd.F
Chd|        SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10CNDF1(ICNDS10,WEIGHT ,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1                   ADDCNCND,PROCNCND,A    ,IADCND,FSKYCND,
     2                   ITAGND  , NODFTSK,NODLTSK,EFTSK ,ELTSK ,
     3                   ITSK  ,ITAB   ,STIFN ,STIFND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDM(*),FR_CNDM(*),FR_NBCCCND(2,*),
     .          ADDCNCND(*),PROCNCND(*),IADCND(2,*),ITAGND(*),ITAB(*)
        INTEGER NODFTSK,NODLTSK,EFTSK,ELTSK,ITSK
        my_real
     .     A(3,*),FSKYCND(4,*),STIFN(*),STIFND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, K,ISIZE,LCOMM,LENS,LENR,NCT,NC,N1,N2,ND,IAD1,IAD2,IK
        my_real
     .     FX,FY,FZ ,FAC,STIF
C======================================================================|
C----pass1 : only N1,N2 are secnd nodes of Int2
        ISIZE =4
        IF (IPARIT==0.AND.NSPMD>1) THEN
!$OMP SINGLE
#include "vectorize.inc"
          DO I=1,IAD_CNDM(NSPMD+1)-1
            J = FR_CNDM(I)
            A(1,J) = A(1,J) * WEIGHT(J)
            A(2,J) = A(2,J) * WEIGHT(J)
            A(3,J) = A(3,J) * WEIGHT(J)
            STIFN(J)=STIFN(J)* WEIGHT(J)
          END DO
!$OMP END SINGLE
        ENDIF
        IF (IPARIT/=0.AND.ITSK==0) FSKYCND(1:4,1:LCNCND)=ZERO
C------- change STIFND(I) :int  to ele
C       IF (.FALSE.) THEN
C#include "vectorize.inc"
C         DO I=EFTSK,ELTSK
C           ND  = IABS(ICNDS10(1,I))
C           !ELM     =  Total (inc. STIFI)     - ( Contact without FI
C           STIFND(I) = STIFN(ND) - STIFND(I)
C         END DO
C       END IF
C------------------------
        CALL MY_BARRIER()
C------------------------
        IF (IPARIT == 0 ) THEN
          IK = ITSK*NUMNOD
#include "vectorize.inc"
          DO I=EFTSK,ELTSK
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            IF (ITAGND(N1)==0.AND.ITAGND(N2)==0) CYCLE
            FAC = HALF*WEIGHT(ND)
            FX = FAC*A(1,ND)
            FY = FAC*A(2,ND)
            FZ = FAC*A(3,ND)
            STIF = MAX(ZERO,FAC*(STIFN(ND)-STIFND(I)))
            IF (ITAGND(N1)>0) THEN
              N1 = N1 + IK
              A(1,N1) = A(1,N1) + FX
              A(2,N1) = A(2,N1) + FY
              A(3,N1) = A(3,N1) + FZ
              STIFN(N1) = STIFN(N1) + STIF
            END IF
            IF (ITAGND(N2)>0) THEN
              N2 = N2 + IK
              A(1,N2) = A(1,N2) + FX
              A(2,N2) = A(2,N2) + FY
              A(3,N2) = A(3,N2) + FZ
              STIFN(N2) = STIFN(N2) + STIF
            END IF
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          DO K = 1,NTHREAD-1
            IK = K*NUMNOD
            DO I=NODFTSK,NODLTSK
              IF (ITAGND(I)>0) THEN
                A(1,I) = A(1,I) + A(1,I+IK)
                A(2,I) = A(2,I) + A(2,I+IK)
                A(3,I) = A(3,I) + A(3,I+IK)
                STIFN(I) = STIFN(I) + STIFN(I+IK)
                A(1,I+IK) = ZERO
                A(2,I+IK) = ZERO
                A(3,I+IK) = ZERO
                STIFN(I+IK) = ZERO
              END IF
            END DO
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          IF (NSPMD>1.AND.ITSK==0) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            CALL SPMD_EXCH_A_SCND(
     .          A    ,STIFN  ,FR_CNDM,IAD_CNDM,LCOMM,ISIZE)
          END IF
        ELSE ! P/ON
#include "vectorize.inc"
          DO I=EFTSK,ELTSK
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            IF (ITAGND(N1)==0.AND.ITAGND(N2)==0) CYCLE
            FAC = HALF*WEIGHT(ND)
            FX = FAC*A(1,ND)
            FY = FAC*A(2,ND)
            FZ = FAC*A(3,ND)
C                               TOTAL - ELEMENT = contact inc. remote
            STIF = MAX(ZERO,FAC*(STIFN(ND)-STIFND(I)))
            IAD1 = IADCND(1,I)
            IF (IAD1>0.AND.ITAGND(N1)>0) THEN
              FSKYCND(1,IAD1) = FX
              FSKYCND(2,IAD1) = FY
              FSKYCND(3,IAD1) = FZ
              FSKYCND(4,IAD1) = STIF
            END IF
            IAD2 = IADCND(2,I)
            IF (IAD2>0.AND.ITAGND(N2)>0) THEN
              FSKYCND(1,IAD2) = FX
              FSKYCND(2,IAD2) = FY
              FSKYCND(3,IAD2) = FZ
              FSKYCND(4,IAD2) = STIF
            END IF
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          IF (NSPMD>1.AND.ITSK==0) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            LENS = FR_NBCCCND(1,NSPMD+1)
            LENR = FR_NBCCCND(2,NSPMD+1)
            CALL SPMD_EXCH_A_SCND_PON(
     1           FR_CNDM,IAD_CNDM,ADDCNCND,PROCNCND,FR_NBCCCND,
     2           ISIZE,LENR   ,LENS   ,FSKYCND)
          END IF
C------------------------
          CALL MY_BARRIER()
C------------------------
C
C Routine assemblage parith/ON
C----------to be optimized after
          DO N = NODFTSK,NODLTSK
            IF (ITAGND(N)==0) CYCLE
            NCT = ADDCNCND(N)-1
            NC = ADDCNCND(N+1)-ADDCNCND(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N) + FSKYCND(1,K)
              A(2,N)   = A(2,N) + FSKYCND(2,K)
              A(3,N)   = A(3,N) + FSKYCND(3,K)
              STIFN(N) = STIFN(N) + FSKYCND(4,K)
            ENDDO
          ENDDO
        END IF
C
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDF2                      source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        SPMD_EXCH_A_SCND              source/mpi/elements/spmd_exch_a_scnd.F
Chd|        SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10CNDF2(ICNDS10,WEIGHT ,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1                   ADDCNCND,PROCNCND,A    ,IADCND,FSKYCND,
     2                   ITAGND  , NODFTSK,NODLTSK,EFTSK ,ELTSK ,
     3                   ITSK  ,ITAB   ,STIFN ,STIFND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDM(*),FR_CNDM(*),FR_NBCCCND(2,*),
     .          ADDCNCND(*),PROCNCND(*),IADCND(2,*),ITAGND(*),ITAB(*)
        INTEGER NODFTSK,NODLTSK,EFTSK,ELTSK,ITSK
C     REAL
        my_real
     .     A(3,*),FSKYCND(4,*), STIFN(*),STIFND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, K,ISIZE,LCOMM,LENS,LENR,NCT,NC,N1,N2,ND,IAD1,IAD2,IK
C     REAL
        my_real
     .     FX,FY,FZ ,FAC,STIF
C======================================================================|
C----pass2 : all excepting N1,N2 are secnd nodes of Int2
        ISIZE =4
        IF (IPARIT==0.AND.NSPMD>1) THEN
!$OMP SINGLE
#include "vectorize.inc"
          DO I=1,IAD_CNDM(NSPMD+1)-1
            J = FR_CNDM(I) ! noeud sommet sur la frontiere SPMD (CoNDensation Main node)
            A(1,J) = A(1,J) * WEIGHT(J)
            A(2,J) = A(2,J) * WEIGHT(J)
            A(3,J) = A(3,J) * WEIGHT(J)
            STIFN(J)=STIFN(J)* WEIGHT(J)
          END DO
!$OMP END SINGLE
        ENDIF
        IF (IPARIT/=0.AND.ITSK==0) FSKYCND(1:4,1:LCNCND)=ZERO
C------------------------
        CALL MY_BARRIER()
C------------------------
        IF (IPARIT == 0 ) THEN
          IK = ITSK*NUMNOD
#include "vectorize.inc"
          DO I=EFTSK,ELTSK ! Middle node
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC = HALF*WEIGHT(ND)
            FX = FAC*A(1,ND)
            FY = FAC*A(2,ND)
            FZ = FAC*A(3,ND)
            STIF = MAX(ZERO,FAC*(STIFN(ND)-STIFND(I)))
            IF (ITAGND(N1)==0) THEN
              N1 = N1 + IK
              A(1,N1) = A(1,N1) + FX
              A(2,N1) = A(2,N1) + FY
              A(3,N1) = A(3,N1) + FZ
              STIFN(N1) = STIFN(N1) + STIF
            END IF
            IF (ITAGND(N2)==0) THEN
              N2 = N2 + IK
              A(1,N2) = A(1,N2) + FX
              A(2,N2) = A(2,N2) + FY
              A(3,N2) = A(3,N2) + FZ
              STIFN(N2) = STIFN(N2) + STIF
            END IF
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          DO K = 1,NTHREAD-1
            IK = K*NUMNOD
            DO I=NODFTSK,NODLTSK
              IF (ITAGND(I)==0) THEN
                A(1,I) = A(1,I) + A(1,I+IK)
                A(2,I) = A(2,I) + A(2,I+IK)
                A(3,I) = A(3,I) + A(3,I+IK)
                STIFN(I) = STIFN(I) + STIFN(I+IK)
                A(1,I+IK) = ZERO
                A(2,I+IK) = ZERO
                A(3,I+IK) = ZERO
                STIFN(I+IK) = ZERO
              END IF
            END DO
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          IF (NSPMD>1.AND.ITSK==0) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            CALL SPMD_EXCH_A_SCND(
     .          A    ,STIFN  ,FR_CNDM,IAD_CNDM,LCOMM,ISIZE)
          END IF
          STIFND(EFTSK:ELTSK) = ZERO
        ELSE
#include "vectorize.inc"
          DO I=EFTSK,ELTSK
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC = HALF*WEIGHT(ND)
            FX = FAC*A(1,ND)
            FY = FAC*A(2,ND)
            FZ = FAC*A(3,ND)
            STIF = MAX(ZERO,FAC*(STIFN(ND)-STIFND(I)))
            IAD1 = IADCND(1,I)
            IF (IAD1>0.AND.ITAGND(N1)==0) THEN
              FSKYCND(1,IAD1) = FX
              FSKYCND(2,IAD1) = FY
              FSKYCND(3,IAD1) = FZ
              FSKYCND(4,IAD1) = STIF
            END IF
            IAD2 = IADCND(2,I)
            IF (IAD2>0.AND.ITAGND(N2)==0) THEN
              FSKYCND(1,IAD2) = FX
              FSKYCND(2,IAD2) = FY
              FSKYCND(3,IAD2) = FZ
              FSKYCND(4,IAD2) = STIF
            END IF
          END DO
C------------------------
          CALL MY_BARRIER()
C------------------------
          IF (NSPMD>1.AND.ITSK==0) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            LENS = FR_NBCCCND(1,NSPMD+1)
            LENR = FR_NBCCCND(2,NSPMD+1)
            CALL SPMD_EXCH_A_SCND_PON(
     1           FR_CNDM,IAD_CNDM,ADDCNCND,PROCNCND,FR_NBCCCND,
     2           ISIZE,LENR   ,LENS   ,FSKYCND)
          END IF
C------------------------
          CALL MY_BARRIER()
C------------------------
C Routine assemblage parith/ON
C----------to be optimized after
          DO N = NODFTSK,NODLTSK
            IF (ITAGND(N)/=0) CYCLE
            NCT = ADDCNCND(N)-1
            NC = ADDCNCND(N+1)-ADDCNCND(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N) + FSKYCND(1,K)
              A(2,N)   = A(2,N) + FSKYCND(2,K)
              A(3,N)   = A(3,N) + FSKYCND(3,K)
              STIFN(N) = STIFN(N) + FSKYCND(4,K)
            ENDDO
          ENDDO
        END IF
C
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CND_INI                    source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CND_INI(ICNDS10,ITAGND,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1                      ADDCNCND,PROCNCND,VND  ,V   ,ITAB    ,
     2                      IAD_CNDM1,FR_CNDM1,FR_NBCCCND1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),IAD_CNDM(*),FR_CNDM(*),FR_NBCCCND(2,*),
     .          ADDCNCND(*),PROCNCND(*),ITAGND(*),ITAB(*),
     .          IAD_CNDM1(*),FR_CNDM1(*),FR_NBCCCND1(2,*)
C     REAL
        my_real
     .     V(3,*),VND(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, K, L, NOD,LOC_PROC,CC,N1,N2
C-----------------------------------------------
C   Initialization of  ITAGND,FR_NBCCI2
C-----------------------------------------------
        DO I = 1, NS10E
          N1 = ICNDS10(2,I)
          N2 = ICNDS10(3,I)
          VND(1,I) = HALF*(V(1,N1) + V(1,N2))
          VND(2,I) = HALF*(V(2,N1) + V(2,N2))
          VND(3,I) = HALF*(V(3,N1) + V(3,N2))
        END DO
C
        IF (IPARIT/=0) THEN
          DO I = 1, NSPMD+1
            FR_NBCCCND(1,I) = 0
            FR_NBCCCND(2,I) = 0
          ENDDO
C
          LOC_PROC = ISPMD+1
          DO I = 1, NSPMD
            IF(I/=LOC_PROC) THEN
              DO J=IAD_CNDM(I),IAD_CNDM(I+1)-1
                NOD = FR_CNDM(J)
                DO CC = ADDCNCND(NOD),ADDCNCND(NOD+1)-1
                  IF(PROCNCND(CC)==LOC_PROC) THEN
                    FR_NBCCCND(1,I) = FR_NBCCCND(1,I)+1
                  ELSEIF(PROCNCND(CC)==I) THEN
                    FR_NBCCCND(2,I) = FR_NBCCCND(2,I)+1
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDDO
C
          DO I = 1, NSPMD
            FR_NBCCCND(1,NSPMD+1) = FR_NBCCCND(1,NSPMD+1)+FR_NBCCCND(1,I)
            FR_NBCCCND(2,NSPMD+1) = FR_NBCCCND(2,NSPMD+1)+FR_NBCCCND(2,I)
          ENDDO
        END IF !(IPARIT/=0.AND.IPARIT/=3) THEN
C------for pass1
        IAD_CNDM1(1) = 1
        DO I = 1, NSPMD
          K = 0
          DO J=IAD_CNDM(I),IAD_CNDM(I+1)-1
            NOD = FR_CNDM(J)
            IF (ITAGND(NOD)>0) THEN
              K = K + 1
              FR_CNDM1(K+IAD_CNDM1(I)-1) = NOD
            END IF
          ENDDO
          IAD_CNDM1(I+1) = IAD_CNDM1(I) + K
        ENDDO
        IF (IPARIT/=0) THEN
          DO I = 1, NSPMD+1
            FR_NBCCCND1(1,I) = 0
            FR_NBCCCND1(2,I) = 0
          ENDDO
C
          DO I = 1, NSPMD
            IF(I/=LOC_PROC) THEN
              DO J=IAD_CNDM1(I),IAD_CNDM1(I+1)-1
                NOD = FR_CNDM1(J)
                DO CC = ADDCNCND(NOD),ADDCNCND(NOD+1)-1
                  IF(PROCNCND(CC)==LOC_PROC) THEN
                    FR_NBCCCND1(1,I) = FR_NBCCCND1(1,I)+1
                  ELSEIF(PROCNCND(CC)==I) THEN
                    FR_NBCCCND1(2,I) = FR_NBCCCND1(2,I)+1
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDDO
          DO I = 1, NSPMD
            FR_NBCCCND1(1,NSPMD+1) = FR_NBCCCND1(1,NSPMD+1)+FR_NBCCCND1(1,I)
            FR_NBCCCND1(2,NSPMD+1) = FR_NBCCCND1(2,NSPMD+1)+FR_NBCCCND1(2,I)
          ENDDO
        END IF !(IPARIT/=0.AND.IPARIT/=3) THEN
C
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDI2_INI                  source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|        SPMD_EXCH_TAG_SCND            source/mpi/elements/spmd_exch_tag_scnd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S10CNDI2_INI(IPARI,INTBUF_TAB,ICNDS10,ITAGND,WEIGHT,
     1                        FR_CNDS,IAD_CNDS,itab  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,NINTER),ITAGND(*),ICNDS10(3,*),WEIGHT(*),
     1               FR_CNDS(*),IAD_CNDS(*),itab(*)
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NTY,NSN,NMN,IM,II,N1,N2,ND,NS,ILEV,IPEN,L,NUS,SIZ
        INTEGER ITAGS(NUMNOD)
C=======================================================================
        DO I = 1, NS10E
          N = ICNDS10(1,I)
          IF (N > 0) THEN
            ITAGND(N) = I
          ELSEIF(N < 0) THEN
            ITAGND(-N) = -I
          END IF
        END DO
C -------add RBE3  after
        ITAGS(1:NUMNOD) = 0
        DO N=1,NINTER
          NTY  = IPARI(7,N)
          IF (NTY == 2 ) THEN
            NMN =IPARI(6,N)
            NSN   = IPARI(5,N)
            ILEV = IPARI(20,N)
            NUS   = IPARI(15,N)
            IF (ILEV == 27 .or. ILEV == 28) THEN
              DO I=1,NSN
                IF (INTBUF_TAB(N)%IRUPT(I) /= 1) THEN
                  NS = INTBUF_TAB(N)%NSV(I)
                  IF (ITAGS(NS)==0) ITAGS(NS)=NUS
                  L = INTBUF_TAB(N)%IRTLM(I)
                  DO J = 1, 4
                    II = 4*(L-1)+J
                    IM = INTBUF_TAB(N)%IRECTM(II)
                    IF (ITAGND(IM)>0) THEN
                      ITAGND(IM) = ITAGND(IM) + NS10E
                    ELSEIF(ITAGND(IM)<0) THEN
                      ITAGND(IM) = ITAGND(IM) - NS10E
                    END IF
                  END DO
                END IF !(INTBUF_TAB(N)%IRUPT(I) /= 1) THEN
              END DO
            ELSEIF (ILEV <= 5 .or. ILEV == 30) THEN
              DO I=1,NSN
                NS = INTBUF_TAB(N)%NSV(I)
                IF (ITAGS(NS)==0) ITAGS(NS)=NUS
                L = INTBUF_TAB(N)%IRTLM(I)
                DO J = 1, 4
                  II = 4*(L-1)+J
                  IM = INTBUF_TAB(N)%IRECTM(II)
                  IF (ITAGND(IM)>0) THEN
                    ITAGND(IM) = ITAGND(IM) + NS10E
                  ELSEIF(ITAGND(IM)<0) THEN
                    ITAGND(IM) = ITAGND(IM) - NS10E
                  END IF
                END DO
              END DO
            END IF
          END IF
        END DO
C--------comm to synchro the case iabs(ITAGND(Nd))>NS10E main of int2
        IF (NSPMD>1) THEN
          SIZ = IAD_CNDS(NSPMD+1)-IAD_CNDS(1)
          CALL SPMD_EXCH_TAG_SCND(ITAGND,FR_CNDS,IAD_CNDS,SIZ)
        END IF
C--------add edge nodes at the end :Secnd of int2 ->used in pass1
        DO I = 1, NS10E
          N1 = ICNDS10(2,I)
          N2 = ICNDS10(3,I)
          ITAGND(N1) = ITAGS(N1)
          ITAGND(N2) = ITAGS(N2)
        END DO
C------Change FR_CNDS(J) from Nd to id of ICNDS10 for STIFND
        IF (NSPMD>1) THEN
          DO I = 1, NS10E
            N = IABS(ICNDS10(1,I))
            ITAGS(N) = I
          END DO
          DO I=1,NSPMD
            DO J=IAD_CNDS(I),IAD_CNDS(I+1)-1
              N =  FR_CNDS(J)
              FR_CNDS(J) = ITAGS(N)
            ENDDO
          ENDDO
        END IF
C
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDAMP                     source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_EXCH_A_SCND              source/mpi/elements/spmd_exch_a_scnd.F
Chd|        SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|====================================================================
      SUBROUTINE S10CNDAMP(ICNDS10,MS   ,A   ,V    ,VD    ,
     1                     IADCND ,ADDCNCND,FSKYCND,WEIGHT ,IAD_CNDM,
     2                     FR_CNDM,FR_NBCCCND,PROCNCND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "comlock.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDM(*),FR_CNDM(*),
     .          FR_NBCCCND(2,*),ADDCNCND(*),PROCNCND(*),IADCND(2,*)
C     REAL
        my_real
     .     A(3,*),V(3,*),VD(3,*),MS(*),FSKYCND(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NFTSK,NLTSK,N, K,ID1,ID2,NC,N1,N2,ND,CC,NCT,
     .           ISIZE,LCOMM,LENS,LENR
CC     REAL
        my_real
     .     FX1,FY1,FZ1 ,FX2,FY2,FZ2,FAC,FAC1,FAC2,DT05,VX,VY,VZ
        my_real
     .     TMP(NUMNOD)
C======================================================================|
        IF (IPARIT/=0) FSKYCND(1:3,1:LCNCND)=ZERO
        ISIZE = 4
C
        IF (IPARIT == 0 ) THEN
          IF (NSPMD>1) THEN
#include "vectorize.inc"
            DO I=1,IAD_CNDM(NSPMD+1)-1
              J = FR_CNDM(I)
              A(1,J) = A(1,J) * WEIGHT(J)
              A(2,J) = A(2,J) * WEIGHT(J)
              A(3,J) = A(3,J) * WEIGHT(J)
            END DO
          END IF
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC= DAMPA*MS(ND)*WEIGHT(ND)
            FAC1 = FAC/MS(N1)
            FAC2 = FAC/MS(N2)
            VX = V(1,ND)-VD(1,I)
            VY = V(2,ND)-VD(2,I)
            VZ = V(3,ND)-VD(3,I)
            FX1 = FAC1*VX
            FY1 = FAC1*VY
            FZ1 = FAC1*VZ
            FX2 = FAC2*VX
            FY2 = FAC2*VY
            FZ2 = FAC2*VZ
            A(1,N1) = A(1,N1) - FX1
            A(2,N1) = A(2,N1) - FY1
            A(3,N1) = A(3,N1) - FZ1
            A(1,N2) = A(1,N2) - FX2
            A(2,N2) = A(2,N2) - FY2
            A(3,N2) = A(3,N2) - FZ2
          END DO
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            TMP(1:NUMNOD)=ZERO
            CALL SPMD_EXCH_A_SCND(
     .          A    ,TMP  ,FR_CNDM,IAD_CNDM,LCOMM,ISIZE)
          END IF
        ELSE
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC= DAMPA*MS(ND)
            FAC1 = FAC/MS(N1)
            FAC2 = FAC/MS(N2)
            VX = V(1,ND)-VD(1,I)
            VY = V(2,ND)-VD(2,I)
            VZ = V(3,ND)-VD(3,I)
            FX1 = FAC1*VX
            FY1 = FAC1*VY
            FZ1 = FAC1*VZ
            FX2 = FAC2*VX
            FY2 = FAC2*VY
            FZ2 = FAC2*VZ
            ID1 = IADCND(1,I)
            IF (ID1>0) THEN
              FSKYCND(1,ID1) = FX1
              FSKYCND(2,ID1) = FY1
              FSKYCND(3,ID1) = FZ1
              FSKYCND(4,ID1) = ZERO
            END IF
            ID2 = IADCND(2,I)
            IF (ID2>0) THEN
              FSKYCND(1,ID2) = FX2
              FSKYCND(2,ID2) = FY2
              FSKYCND(3,ID2) = FZ2
              FSKYCND(4,ID2) = ZERO
            END IF
          END DO
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            LENS = FR_NBCCCND(1,NSPMD+1)
            LENR = FR_NBCCCND(2,NSPMD+1)
            CALL SPMD_EXCH_A_SCND_PON(
     1           FR_CNDM,IAD_CNDM,ADDCNCND,PROCNCND,FR_NBCCCND,
     2           ISIZE,LENR   ,LENS   ,FSKYCND)
          END IF
C
C Routine assemblage parith/ON
C
          DO N = 1, NUMNOD
            NCT = ADDCNCND(N)-1
            NC = ADDCNCND(N+1)-ADDCNCND(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N) - FSKYCND(1,K)
              A(2,N)   = A(2,N) - FSKYCND(2,K)
              A(3,N)   = A(3,N) - FSKYCND(3,K)
            ENDDO
          ENDDO
        END IF
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDFND                     source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F
Chd|====================================================================
      SUBROUTINE S10CNDFND(ICNDS10,WEIGHT ,IAD_CNDS,FR_CNDS,ITAB   ,
     2                     NODFTSK,NODLTSK,EFTSK ,ELTSK ,ITSK   ,
     3                     STIFN ,STIFND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDS(*),FR_CNDS(*),ITAB(*)
        INTEGER NODFTSK,NODLTSK,EFTSK,ELTSK,ITSK
C     REAL
        my_real
     .     STIFN(*),STIFND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, K,LCOMM,IK,ND
C     REAL
C======================================================================|
C-----get STIFND from interface part
C STIFN of contact (element not done yet)
        IK = 0
        DO K = 1,NTHREAD
          DO I = EFTSK,ELTSK
            ND  = IABS(ICNDS10(1,I))
            STIFND(I) = STIFND(I)+ STIFN(ND+IK)
          END DO
          IK = IK + NUMNOD
        END DO
C------------------------
        CALL MY_BARRIER()
C------------------------
c        IF (NSPMD>1.AND.ITSK==0) THEN
c          LCOMM =IAD_CNDS(NSPMD+1)-IAD_CNDS(1)
c          CALL SPMD_EXCH_STIF_SCND(
c     .        STIFND  ,FR_CNDS,IAD_CNDS,LCOMM)
c        END IF
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDS_DIM                   source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CNDS_DIM(ICNDS10,ITAGS,FR_ELEM,IAD_ELEM,NBDDS  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),FR_ELEM(*),IAD_ELEM(2,*),NBDDS,ITAGS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,IP
C=======================================================================
        DO I = 1, NS10E
          N = IABS(ICNDS10(1,I))
          ITAGS(N) = I
        END DO
C--------
        NBDDS = 0
        DO IP = 1,NSPMD
          DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
            N = FR_ELEM(J)
            IF (ITAGS(N)>0) NBDDS = NBDDS + 1
          END DO
        END DO
C
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDS_INI                   source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CNDS_INI(ICNDS10,ITAGS,FR_ELEM,IAD_ELEM,IAD_CDNS,FR_CDNS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),FR_ELEM(*),IAD_ELEM(2,*),IAD_CDNS(*),FR_CDNS(*),
     .          ITAGS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,IP,NB,nd
C=======================================================================
        NB = 1
        DO IP = 1,NSPMD
          IAD_CDNS(IP) =NB
          DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
            N = FR_ELEM(J)
            IF (ITAGS(N)>0) THEN
C------ will be changed to ITAGS(N) in  S10CNDI2_INI
              FR_CDNS(NB) = N
c           FR_CDNS(NB) = ITAGS(N)
              NB = NB + 1
            END IF
          END DO
          IAD_CDNS(IP+1) = NB
        END DO
        ITAGS(1:NUMNOD) = 0
C
        RETURN
      END
Chd|====================================================================
Chd|  S10print                      source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10print(ICNDS10,A   ,V, ITAB    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),itab(*)
C     REAL
        my_real
     .     A(3,*),V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NFTSK,NLTSK,N, K,ID1,ID2,NC,N1,N2,ND,CC,NCT
C     REAL
        DO I=1,NS10E
          N1  = ICNDS10(2,I)
          N2  = ICNDS10(3,I)
          if (itab(n1)==1294333.and.itab(n2)==1338494) then
            ND  = IABS(ICNDS10(1,I))
            write(iout,*)'ND,N1,N2, A,V=',itab(nd),itab(n1),itab(n2)
            write(iout,*)a(1,ND),a(2,ND),a(3,ND)
            write(iout,*)v(1,ND),v(2,ND),v(3,ND)
          end if
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  CNDMASI2_DIM                  source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CNDMASI2_DIM(IPARI,INTBUF_TAB,ICNDS10,ITAGND,WEIGHT,NKEND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,NINTER),ITAGND(*),ICNDS10(3,*),WEIGHT(*),
     1               NKEND
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NTY,NSN,NMN,IM,II,N1,N2,ND,NS,ILEV,IPEN,L,NUS,SIZ
C=======================================================================
        DO I = 1, NS10E
          N = ICNDS10(1,I)
          IF (N > 0) THEN
            ITAGND(N) = I
          ELSEIF(N < 0) THEN
            ITAGND(-N) = I
          END IF
        END DO
        NKEND = 0
C -------only cinematic--> mass condensation
        DO N=1,NINTER
          NTY  = IPARI(7,N)
          IF (NTY == 2 ) THEN
            NMN =IPARI(6,N)
            NSN   = IPARI(5,N)
            ILEV = IPARI(20,N)
            IF (ILEV == 27 .or. ILEV == 28) THEN
              DO I=1,NSN
                IF (INTBUF_TAB(N)%IRUPT(I) /= 1) THEN
                  L = INTBUF_TAB(N)%IRTLM(I)
                  DO J = 1, 4
                    II = 4*(L-1)+J
                    IM = INTBUF_TAB(N)%IRECTM(II)
                    IF (WEIGHT(IM)==0) CYCLE
                    IF (ITAGND(IM)>0 .AND.ITAGND(IM)<= NS10E) THEN
                      NKEND = NKEND + 1
                      ITAGND(IM) =ITAGND(IM) + NS10E
                    END IF
                  END DO
                END IF
              END DO
            ELSEIF (ILEV <= 5 .or. ILEV == 30) THEN
              DO I=1,NSN
                L = INTBUF_TAB(N)%IRTLM(I)
                DO J = 1, 4
                  II = 4*(L-1)+J
                  IM = INTBUF_TAB(N)%IRECTM(II)
                  IF (WEIGHT(IM)==0) CYCLE
                  IF (ITAGND(IM)>0 .AND.ITAGND(IM)<= NS10E) THEN
                    NKEND = NKEND + 1
                    ITAGND(IM) =ITAGND(IM) + NS10E
                  END IF
                END DO
              END DO
            END IF
          END IF
        END DO
C
        RETURN
      END
Chd|====================================================================
Chd|  CNDMASI2_INI                  source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CNDMASI2_INI(IPARI,INTBUF_TAB,ICNDS10,ITAGND,
     .                        NKEND,IMAP2ND,MASI2ND0,MS ,WEIGHT,itab )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,NINTER),ITAGND(*),ICNDS10(3,*),
     1               NKEND,IMAP2ND(*),WEIGHT(*),itab(*)
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
        my_real
     .     MASI2ND0(*),MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NTY,NSN,NMN,IM,II,N1,N2,ND,NS,ILEV,IPEN,L,NK,SIZ
C=======================================================================
C -------only cinematic--> mass condensation
C-----MASI2ND0 can be done here because DTNODA is called after INTTI1
        DO I = 1, NS10E
          N = ICNDS10(1,I)
          IF (N > 0) THEN
            ITAGND(N) = I
          ELSEIF(N < 0) THEN
            ITAGND(-N) = I
          END IF
        END DO
        NK = 0
        DO N=1,NINTER
          NTY  = IPARI(7,N)
          IF (NTY == 2 ) THEN
            NMN =IPARI(6,N)
            NSN   = IPARI(5,N)
            ILEV = IPARI(20,N)
            IF (ILEV == 27 .or. ILEV == 28) THEN
              DO I=1,NSN
                IF (INTBUF_TAB(N)%IRUPT(I) /= 1) THEN
                  L = INTBUF_TAB(N)%IRTLM(I)
                  DO J = 1, 4
                    II = 4*(L-1)+J
                    IM = INTBUF_TAB(N)%IRECTM(II)
                    IF (WEIGHT(IM)==0) CYCLE
                    IF (ITAGND(IM)>0 .AND.ITAGND(IM)<= NS10E) THEN
                      NK = NK + 1
                      IMAP2ND(NK) = ITAGND(IM)
                      MASI2ND0(NK) = MS(IM)
                      ITAGND(IM) =ITAGND(IM) + NS10E
                    END IF
                  END DO
                END IF
              END DO
            ELSEIF (ILEV <= 5 .or. ILEV == 30) THEN
              DO I=1,NSN
                L = INTBUF_TAB(N)%IRTLM(I)
                DO J = 1, 4
                  II = 4*(L-1)+J
                  IM = INTBUF_TAB(N)%IRECTM(II)
                  IF (WEIGHT(IM)==0) CYCLE
                  IF (ITAGND(IM)>0 .AND.ITAGND(IM)<= NS10E) THEN
                    NK = NK + 1
                    IMAP2ND(NK) = ITAGND(IM)
                    MASI2ND0(NK) = MS(IM)
                    ITAGND(IM) =ITAGND(IM) + NS10E
                  END IF
                END DO
              END DO
            END IF
          END IF
        END DO
C
        RETURN
      END
Chd|====================================================================
Chd|  CND_DMASI2                    source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CND_DMASI2(ICNDS10,NKEND,IMAP2ND,MASI2ND0,MS  ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "itet2_c.inc"
#include      "scr07_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),NKEND,IMAP2ND(*),WEIGHT(*)
        my_real
     .     MASI2ND0(*),MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NS
        my_real
     .     DMAS,DMAS2,MASND
C=======================================================================
C---- for /CHKPT and restart : just affect output in ecrit
        IF (MCHECK>0.OR.IRUN>1) THEN
C---- can't do it precisely unless to store MASI2ND0
          IF (NKEND<0.OR.IRUN>1) THEN
            DMAS2 = MSI20
            NKEND =IABS(NKEND)
            MASI2ND0(1:NKEND)=DMAS2/NKEND
          END IF
        ELSE
          DMAS2 = ZERO
          DO I = 1, NKEND
            N = IMAP2ND(I)
            NS = IABS(ICNDS10(1,N))
            DMAS=MS(NS)-MASI2ND0(I)
            MASI2ND0(I) = MAX(ZERO,DMAS)
            DMAS2 = DMAS2 + MASI2ND0(I)
          ENDDO
C
          MASND = ZERO
          DO I = 1, NS10E
            NS = IABS(ICNDS10(1,I))
            IF (WEIGHT(NS)/=0) MASND = MASND + MS(NS)
          ENDDO
C---due to the part w/ weight(ns)=0; DMSI2 in itet2_c.inc for restart
          IF (TT==ZERO) THEN
            DMSI2 = MASND-MS_ND-DMAS2
            DMSI2 = MAX(ZERO,DMSI2)
            MSI20 = DMAS2
          END IF
        END IF !(MCHECK>0.AND.NKEND>0) THEN
C
        RETURN
      END
Chd|====================================================================
Chd|  CNDMASI2                      source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CNDMASI2(ICNDS10,NKEND,IMAP2ND,MASI2ND0,MS ,V  ,A   ,
     .                    WEIGHT ,MAS_ND ,KEND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "itet2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),NKEND,IMAP2ND(*),WEIGHT(*)
        my_real
     .     MASI2ND0(*),MS(*),V(3,*),A(3,*),KEND,MAS_ND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NS
        my_real
     .     DMAS,VX,VY,VZ,DT05
C=======================================================================
        KEND = ZERO
        DT05 = HALF*DT1
        DMAS = ZERO
        DO I = 1, NKEND
          N = IMAP2ND(I)
          NS = IABS(ICNDS10(1,N))
c           DMAS=MS(NS)-MASI2ND0(I)
          VX = V(1,NS) + DT05*A(1,NS)
          VY = V(2,NS) + DT05*A(2,NS)
          VZ = V(3,NS) + DT05*A(3,NS)
          KEND = KEND + ( VX*VX + VY*VY + VZ*VZ)*HALF*MASI2ND0(I)
          DMAS = DMAS + MASI2ND0(I)
        ENDDO
        DMAS = DMAS + DMSI2
C
        MAS_ND = ZERO
        DO I = 1, NS10E
          NS = IABS(ICNDS10(1,I))
          IF (WEIGHT(NS)/=0) MAS_ND = MAS_ND + MS(NS)
        ENDDO
        MAS_ND = MAS_ND - DMAS
C
        RETURN
      END
Chd|====================================================================
Chd|  S10CNIDAMP                    source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_EXCH_A_SCND              source/mpi/elements/spmd_exch_a_scnd.F
Chd|        SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE S10CNIDAMP(ICNDS10,MS   ,A   ,V    ,VD    ,
     1                      IADCND ,ADDCNCND,FSKYCND,SKEW  ,DAMPR  ,
     3                      DAMP   ,IGRNOD  ,DIM   ,WEIGHT ,IAD_CNDM,
     4                      FR_CNDM,FR_NBCCCND,PROCNCND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "comlock.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDM(*),FR_CNDM(*),DIM,
     .          FR_NBCCCND(2,*),ADDCNCND(*),PROCNCND(*),IADCND(2,*)
C     REAL
        my_real
     .     A(3,*),V(3,*),VD(3,*),MS(*),FSKYCND(4,*),
     .     DAMPR(NRDAMP,*), DAMP(DIM,*), SKEW(LSKEW,*)
C-----------------------------------------------
        TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NMD,NLTSK,N, K,ID1,ID2,NC,N1,N2,ND,CC,NCT,ISK,IGR
        INTEGER ITAGS(NUMNOD),ISIZE,LCOMM,LENS,LENR
C     REAL
        my_real
     .     FACTB,DAMPT,D_TSTART,D_TSTOP,DAMP_A(3),VSKW(3),DA_G(3),
     .     FX1,FY1,FZ1 ,FX2,FY2,FZ2,FAC,FAC1,FAC2,VX,VY,VZ
        my_real
     .     TMP(NUMNOD)
C======================================================================|
C=======================================================================
        ITAGS(1:NUMNOD) = 0
        DO I = 1, NS10E
          N = ICNDS10(1,I)
          IF (N > 0) THEN
            ITAGS(N) = I
          END IF
        END DO
        ISIZE = 4
        IF (IPARIT == 0 ) THEN
          IF (NSPMD>1) THEN
#include "vectorize.inc"
            DO I=1,IAD_CNDM(NSPMD+1)-1
              J = FR_CNDM(I)
              A(1,J) = A(1,J) * WEIGHT(J)
              A(2,J) = A(2,J) * WEIGHT(J)
              A(3,J) = A(3,J) * WEIGHT(J)
            END DO
          END IF
          DO ND=1,NDAMP
            IGR   = NINT(DAMPR(2,ND))
            ISK   = NINT(DAMPR(15,ND))
            FACTB = DAMPR(16,ND)
            DAMPT  = MIN(DT1,DT2)*FACTB
            D_TSTART = DAMPR(17,ND)
            D_TSTOP  = DAMPR(18,ND)
            IF (TT>=D_TSTART .AND. TT<=D_TSTOP) THEN
C----- Damping sur dof rotation et seulement -----
              IF (DAMPR(19,ND)>0) CYCLE
C-------------------------------------------------
              DAMP_A(1) = DAMPR(3,ND)
              DAMP_A(2) = DAMPR(5,ND)
              DAMP_A(3) = DAMPR(7,ND)
              IF(ISK<=1)THEN
#include "vectorize.inc"
                DO N=1,IGRNOD(IGR)%NENTITY
                  I=IGRNOD(IGR)%ENTITY(N)
                  IF (ITAGS(I)==0) CYCLE
                  J = ITAGS(I)
                  NMD  = IABS(ICNDS10(1,J))
                  N1  = ICNDS10(2,J)
                  N2  = ICNDS10(3,J)
                  FAC= MS(NMD)*WEIGHT(NMD)
                  IF (MS(N1)<=EM20) THEN
                    FAC1 = ZERO
                  ELSE
                    FAC1 = FAC/MS(N1)
                  END IF
                  IF (MS(N2)<=EM20) THEN
                    FAC2 = ZERO
                  ELSE
                    FAC2 = FAC/MS(N2)
                  END IF
                  VX = V(1,NMD)-VD(1,J)
                  VY = V(2,NMD)-VD(2,J)
                  VZ = V(3,NMD)-VD(3,J)
                  FX1 = FAC1*DAMP_A(1)*VX
                  FY1 = FAC1*DAMP_A(2)*VY
                  FZ1 = FAC1*DAMP_A(3)*VZ
                  FX2 = FAC2*DAMP_A(1)*VX
                  FY2 = FAC2*DAMP_A(2)*VY
                  FZ2 = FAC2*DAMP_A(3)*VZ
                  A(1,N1) = A(1,N1) - FX1
                  A(2,N1) = A(2,N1) - FY1
                  A(3,N1) = A(3,N1) - FZ1
                  A(1,N2) = A(1,N2) - FX2
                  A(2,N2) = A(2,N2) - FY2
                  A(3,N2) = A(3,N2) - FZ2
                ENDDO
              ELSE
C-------------------------------------------------
#include "vectorize.inc"
                DO N=1,IGRNOD(IGR)%NENTITY
                  I=IGRNOD(IGR)%ENTITY(N)
                  IF (ITAGS(I)==0) CYCLE
                  J = ITAGS(I)
                  NMD  = IABS(ICNDS10(1,J))
                  N1  = ICNDS10(2,J)
                  N2  = ICNDS10(3,J)
                  FAC= MS(NMD)
                  IF (MS(N1)<=EM20) THEN
                    FAC1 = ZERO
                  ELSE
                    FAC1 = FAC/MS(N1)
                  END IF
                  IF (MS(N2)<=EM20) THEN
                    FAC2 = ZERO
                  ELSE
                    FAC2 = FAC/MS(N2)
                  END IF
                  VX = V(1,NMD)-VD(1,J)
                  VY = V(2,NMD)-VD(2,J)
                  VZ = V(3,NMD)-VD(3,J)
                  VSKW(1)=DAMP_A(1)*(SKEW(1,ISK)*VX
     .                      +SKEW(2,ISK)*VY
     .                      +SKEW(3,ISK)*VZ)
                  VSKW(2)=DAMP_A(2)*(SKEW(4,ISK)*VX
     .                      +SKEW(5,ISK)*VY
     .                      +SKEW(6,ISK)*VZ)
                  VSKW(3)=DAMP_A(3)*(SKEW(7,ISK)*VX
     .                      +SKEW(8,ISK)*VY
     .                      +SKEW(9,ISK)*VZ)
                  DA_G(1)= SKEW(1,ISK)*VSKW(1)
     .             +SKEW(4,ISK)*VSKW(2)
     .             +SKEW(7,ISK)*VSKW(3)
                  DA_G(2)= SKEW(2,ISK)*VSKW(1)
     .             +SKEW(5,ISK)*VSKW(2)
     .             +SKEW(8,ISK)*VSKW(3)
                  DA_G(3)= SKEW(3,ISK)*VSKW(1)
     .                    +SKEW(6,ISK)*VSKW(2)
     .                    +SKEW(9,ISK)*VSKW(3)
                  FX1 = FAC1*DA_G(1)
                  FY1 = FAC1*DA_G(2)
                  FZ1 = FAC1*DA_G(3)
                  FX2 = FAC2*DA_G(1)
                  FY2 = FAC2*DA_G(2)
                  FZ2 = FAC2*DA_G(3)
                  A(1,N1) = A(1,N1) - FX1
                  A(2,N1) = A(2,N1) - FY1
                  A(3,N1) = A(3,N1) - FZ1
                  A(1,N2) = A(1,N2) - FX2
                  A(2,N2) = A(2,N2) - FY2
                  A(3,N2) = A(3,N2) - FZ2
                END DO
              END IF
            ENDIF
          ENDDO ! ND=1,NDAMP
C
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            TMP(1:NUMNOD)=ZERO
            CALL SPMD_EXCH_A_SCND(
     .          A    ,TMP  ,FR_CNDM,IAD_CNDM,LCOMM,ISIZE)
          END IF
        ELSE
C-------P/ON
          FSKYCND(1:3,1:LCNCND)=ZERO
          DO ND=1,NDAMP
            IGR   = NINT(DAMPR(2,ND))
            ISK   = NINT(DAMPR(15,ND))
            FACTB = DAMPR(16,ND)
            DAMPT  = MIN(DT1,DT2)*FACTB
            D_TSTART = DAMPR(17,ND)
            D_TSTOP  = DAMPR(18,ND)
            IF (TT>=D_TSTART .AND. TT<=D_TSTOP) THEN
C----- Damping sur dof rotation et seulement -----
              IF (DAMPR(19,ND)>0) CYCLE
C-------------------------------------------------
              DAMP_A(1) = DAMPR(3,ND)
              DAMP_A(2) = DAMPR(5,ND)
              DAMP_A(3) = DAMPR(7,ND)
              IF(ISK<=1)THEN
#include "vectorize.inc"
                DO N=1,IGRNOD(IGR)%NENTITY
                  I=IGRNOD(IGR)%ENTITY(N)
                  IF (ITAGS(I)==0) CYCLE
                  J = ITAGS(I)
                  NMD  = IABS(ICNDS10(1,J))
                  N1  = ICNDS10(2,J)
                  N2  = ICNDS10(3,J)
                  FAC= MS(NMD)
                  IF (MS(N1)<=EM20) THEN
                    FAC1 = ZERO
                  ELSE
                    FAC1 = FAC/MS(N1)
                  END IF
                  IF (MS(N2)<=EM20) THEN
                    FAC2 = ZERO
                  ELSE
                    FAC2 = FAC/MS(N2)
                  END IF
                  VX = V(1,NMD)-VD(1,J)
                  VY = V(2,NMD)-VD(2,J)
                  VZ = V(3,NMD)-VD(3,J)
                  FX1 = FAC1*DAMP_A(1)*VX
                  FY1 = FAC1*DAMP_A(2)*VY
                  FZ1 = FAC1*DAMP_A(3)*VZ
                  FX2 = FAC2*DAMP_A(1)*VX
                  FY2 = FAC2*DAMP_A(2)*VY
                  FZ2 = FAC2*DAMP_A(3)*VZ
                  ID1 = IADCND(1,J)
                  IF (ID1>0) THEN
                    FSKYCND(1,ID1) = FX1
                    FSKYCND(2,ID1) = FY1
                    FSKYCND(3,ID1) = FZ1
                  END IF
                  ID2 = IADCND(2,J)
                  IF (ID2>0) THEN
                    FSKYCND(1,ID2) = FX2
                    FSKYCND(2,ID2) = FY2
                    FSKYCND(3,ID2) = FZ2
                  END IF
                ENDDO
              ELSE
C-------------------------------------------------
#include "vectorize.inc"
                DO N=1,IGRNOD(IGR)%NENTITY
                  I=IGRNOD(IGR)%ENTITY(N)
                  IF (ITAGS(I)==0) CYCLE
                  J = ITAGS(I)
                  NMD  = IABS(ICNDS10(1,J))
                  N1  = ICNDS10(2,J)
                  N2  = ICNDS10(3,J)
                  FAC= MS(NMD)
                  IF (MS(N1)<=EM20) THEN
                    FAC1 = ZERO
                  ELSE
                    FAC1 = FAC/MS(N1)
                  END IF
                  IF (MS(N2)<=EM20) THEN
                    FAC2 = ZERO
                  ELSE
                    FAC2 = FAC/MS(N2)
                  END IF
                  VX = V(1,NMD)-VD(1,J)
                  VY = V(2,NMD)-VD(2,J)
                  VZ = V(3,NMD)-VD(3,J)
                  VSKW(1)=DAMP_A(1)*(SKEW(1,ISK)*VX
     .                      +SKEW(2,ISK)*VY
     .                      +SKEW(3,ISK)*VZ)
                  VSKW(2)=DAMP_A(2)*(SKEW(4,ISK)*VX
     .                      +SKEW(5,ISK)*VY
     .                      +SKEW(6,ISK)*VZ)
                  VSKW(3)=DAMP_A(3)*(SKEW(7,ISK)*VX
     .                      +SKEW(8,ISK)*VY
     .                      +SKEW(9,ISK)*VZ)
                  DA_G(1)= SKEW(1,ISK)*VSKW(1)
     .             +SKEW(4,ISK)*VSKW(2)
     .             +SKEW(7,ISK)*VSKW(3)
                  DA_G(2)= SKEW(2,ISK)*VSKW(1)
     .             +SKEW(5,ISK)*VSKW(2)
     .             +SKEW(8,ISK)*VSKW(3)
                  DA_G(3)= SKEW(3,ISK)*VSKW(1)
     .                    +SKEW(6,ISK)*VSKW(2)
     .                    +SKEW(9,ISK)*VSKW(3)
                  FX1 = FAC1*DA_G(1)
                  FY1 = FAC1*DA_G(2)
                  FZ1 = FAC1*DA_G(3)
                  FX2 = FAC2*DA_G(1)
                  FY2 = FAC2*DA_G(2)
                  FZ2 = FAC2*DA_G(3)
                  ID1 = IADCND(1,J)
                  IF (ID1>0) THEN
                    FSKYCND(1,ID1) = FX1
                    FSKYCND(2,ID1) = FY1
                    FSKYCND(3,ID1) = FZ1
                    FSKYCND(4,ID1) = ZERO
                  END IF
                  ID2 = IADCND(2,J)
                  IF (ID2>0) THEN
                    FSKYCND(1,ID2) = FX2
                    FSKYCND(2,ID2) = FY2
                    FSKYCND(3,ID2) = FZ2
                    FSKYCND(4,ID2) = ZERO
                  END IF
                END DO
              END IF
            ENDIF
          ENDDO
C
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            LENS = FR_NBCCCND(1,NSPMD+1)
            LENR = FR_NBCCCND(2,NSPMD+1)
            CALL SPMD_EXCH_A_SCND_PON(
     1           FR_CNDM,IAD_CNDM,ADDCNCND,PROCNCND,FR_NBCCCND,
     2           ISIZE,LENR   ,LENS   ,FSKYCND)
          END IF
C
C Routine assemblage parith/ON
C
          DO N = 1, NUMNOD
            NCT = ADDCNCND(N)-1
            NC = ADDCNCND(N+1)-ADDCNCND(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N) - FSKYCND(1,K)
              A(2,N)   = A(2,N) - FSKYCND(2,K)
              A(3,N)   = A(3,N) - FSKYCND(3,K)
            ENDDO
          ENDDO
        END IF !(IPARIT == 0 ) THEN
C
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNISTAT                    source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_EXCH_A_SCND              source/mpi/elements/spmd_exch_a_scnd.F
Chd|        SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|        NGR2USR                       source/input/freform.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE S10CNISTAT(ICNDS10,MS   ,A   ,V    ,VD    ,
     1                      IADCND ,ADDCNCND,FSKYCND,IGRNOD ,WEIGHT ,
     2                      IAD_CNDM,FR_CNDM,FR_NBCCCND,PROCNCND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "comlock.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDM(*),FR_CNDM(*),
     .          FR_NBCCCND(2,*),ADDCNCND(*),PROCNCND(*),IADCND(2,*)
C     REAL
        my_real
     .     A(3,*),V(3,*),VD(3,*),MS(*),FSKYCND(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NFTSK,NLTSK,N, K,ID1,ID2,NC,N1,N2,ND,CC,NCT,NGR2USR,
     .           ISIZE,LCOMM,LENS,LENR
C     REAL
        my_real
     .     FX1,FY1,FZ1 ,FX2,FY2,FZ2,FAC,FAC1,FAC2,DT05,VX,VY,VZ,DAMPC,DOMEGA
        my_real
     .     TMP(NUMNOD)
        INTEGER ITAG(NUMNOD)
C-----------------------------------------------
        TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
        EXTERNAL NGR2USR
C======================================================================|
        IF (ISTAT/=1.AND.ISTAT/=3) RETURN
C-------0.5*alpha
        DAMPC=BETATE/(ONE + BETATE * DT12)
c       DT05 = HALF*DT1
        IF (IPARIT/=0) FSKYCND(1:3,1:LCNCND)=ZERO
        IF (ISTATG/=0) THEN
          IF(ISTATG<0) ISTATG=NGR2USR(-ISTATG,IGRNOD,NGRNOD)
          ITAG(1:NUMNOD)=0
          DO N=1,IGRNOD(ISTATG)%NENTITY
            I=IGRNOD(ISTATG)%ENTITY(N)
            ITAG(I)=1
          ENDDO
        ELSE
          ITAG(1:NUMNOD)=1
        ENDIF
        ISIZE = 4
C------correction for nd due to V_nd=V_nd-0.5*(V1+V2)
        DOMEGA = TWO*BETATE
        IF(ISTATG==0)THEN
#include "vectorize.inc"
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            A(1,ND)  = A(1,ND)+DOMEGA*VD(1,I)
            A(2,ND)  = A(2,ND)+DOMEGA*VD(2,I)
            A(3,ND)  = A(3,ND)+DOMEGA*VD(3,I)
          END DO
        ELSE
#include "vectorize.inc"
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            IF(ITAG(ND)==0) CYCLE
            A(1,ND)  = A(1,ND)+DOMEGA*VD(1,I)
            A(2,ND)  = A(2,ND)+DOMEGA*VD(2,I)
            A(3,ND)  = A(3,ND)+DOMEGA*VD(3,I)
          END DO
        END IF
C
        IF (IPARIT == 0 ) THEN
          IF (NSPMD>1) THEN
#include "vectorize.inc"
            DO I=1,IAD_CNDM(NSPMD+1)-1
              J = FR_CNDM(I)
              A(1,J) = A(1,J) * WEIGHT(J)
              A(2,J) = A(2,J) * WEIGHT(J)
              A(3,J) = A(3,J) * WEIGHT(J)
            END DO
          END IF
#include "vectorize.inc"
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            IF(ITAG(ND)==0) CYCLE
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC= DAMPC*MS(ND)* WEIGHT(ND)
            IF (MS(N1)<=EM20) THEN
              FAC1 = ZERO
            ELSE
              FAC1 = FAC/MS(N1)
            END IF
            IF (MS(N2)<=EM20) THEN
              FAC2 = ZERO
            ELSE
              FAC2 = FAC/MS(N2)
            END IF
            VX = V(1,ND)-VD(1,I)
            VY = V(2,ND)-VD(2,I)
            VZ = V(3,ND)-VD(3,I)
            FX1 = FAC1*VX
            FY1 = FAC1*VY
            FZ1 = FAC1*VZ
            FX2 = FAC2*VX
            FY2 = FAC2*VY
            FZ2 = FAC2*VZ
            A(1,N1) = A(1,N1) - FX1
            A(2,N1) = A(2,N1) - FY1
            A(3,N1) = A(3,N1) - FZ1
            A(1,N2) = A(1,N2) - FX2
            A(2,N2) = A(2,N2) - FY2
            A(3,N2) = A(3,N2) - FZ2
          END DO
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            TMP(1:NUMNOD)=ZERO
            CALL SPMD_EXCH_A_SCND(
     .          A    ,TMP  ,FR_CNDM,IAD_CNDM,LCOMM,ISIZE)
          END IF
        ELSE
#include "vectorize.inc"
          DO I=1,NS10E
            ND  = IABS(ICNDS10(1,I))
            IF(ITAG(ND)==0) CYCLE
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            FAC= DAMPC*MS(ND)
            IF (MS(N1)<=EM20) THEN
              FAC1 = ZERO
            ELSE
              FAC1 = FAC/MS(N1)
            END IF
            IF (MS(N2)<=EM20) THEN
              FAC2 = ZERO
            ELSE
              FAC2 = FAC/MS(N2)
            END IF
            VX = V(1,ND)-VD(1,I)
            VY = V(2,ND)-VD(2,I)
            VZ = V(3,ND)-VD(3,I)
            FX1 = FAC1*VX
            FY1 = FAC1*VY
            FZ1 = FAC1*VZ
            FX2 = FAC2*VX
            FY2 = FAC2*VY
            FZ2 = FAC2*VZ
            ID1 = IADCND(1,I)
            IF (ID1>0) THEN
              FSKYCND(1,ID1) = FX1
              FSKYCND(2,ID1) = FY1
              FSKYCND(3,ID1) = FZ1
              FSKYCND(4,ID1) = ZERO
            END IF
            ID2 = IADCND(2,I)
            IF (ID2>0) THEN
              FSKYCND(1,ID2) = FX2
              FSKYCND(2,ID2) = FY2
              FSKYCND(3,ID2) = FZ2
              FSKYCND(4,ID2) = ZERO
            END IF
          END DO
          IF (NSPMD>1) THEN
            LCOMM =IAD_CNDM(NSPMD+1)-IAD_CNDM(1)
            LENS = FR_NBCCCND(1,NSPMD+1)
            LENR = FR_NBCCCND(2,NSPMD+1)
            CALL SPMD_EXCH_A_SCND_PON(
     1           FR_CNDM,IAD_CNDM,ADDCNCND,PROCNCND,FR_NBCCCND,
     2           ISIZE,LENR   ,LENS   ,FSKYCND)
          END IF
C
C Routine assemblage parith/ON
C
          DO N = 1, NUMNOD
            NCT = ADDCNCND(N)-1
            NC = ADDCNCND(N+1)-ADDCNCND(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N) - FSKYCND(1,K)
              A(2,N)   = A(2,N) - FSKYCND(2,K)
              A(3,N)   = A(3,N) - FSKYCND(3,K)
            ENDDO
          ENDDO
        END IF
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10STFE_POFF                  source/elements/solid/solide10/s10cndf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        SPMD_EXCH_STIF_SCND           source/mpi/elements/spmd_exch_stif_scnd.F
Chd|====================================================================
      SUBROUTINE S10STFE_POFF(ICNDS10,WEIGHT ,IAD_CNDS,FR_CNDS,ITAB   ,
     2                     NODFTSK,NODLTSK,EFTSK ,ELTSK ,ITSK   ,
     3                     STIFN ,STIFND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),WEIGHT(*),IAD_CNDS(*),FR_CNDS(*),ITAB(*)
        INTEGER NODFTSK,NODLTSK,EFTSK,ELTSK,ITSK
        my_real STIFN(*),STIFND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, K,LCOMM,IK,ND
        my_real
     .     STIFEL(NS10E)
C======================================================================|
C-----get STIFND from elem part, called after Forint
        STIFEL(EFTSK:ELTSK) = ZERO
        IK = 0
        DO K = 1,NTHREAD
          DO I = EFTSK,ELTSK
            ND  = IABS(ICNDS10(1,I))
            STIFEL(I) = STIFEL(I)+ STIFN(ND+IK)
          END DO
          IK = IK + NUMNOD
        END DO
        DO I = EFTSK,ELTSK
          STIFND(I) = STIFEL(I)- STIFND(I)
        END DO
C------------------------
        CALL MY_BARRIER()
C------------------------
        IF (NSPMD>1) THEN
!$OMP SINGLE
          LCOMM =IAD_CNDS(NSPMD+1)-IAD_CNDS(1)
          CALL SPMD_EXCH_STIF_SCND(
     .        STIFND  ,FR_CNDS,IAD_CNDS,LCOMM)
!$OMP END SINGLE
        END IF
C----6---------------------------------------------------------------7---------8
        RETURN
      END

