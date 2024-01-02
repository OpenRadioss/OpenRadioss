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
Chd|====================================================================
Chd|  SECTION_FIO                   source/tools/sect/section_fio.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SECTION_READ                  source/tools/sect/section_read.F
Chd|        SECTION_READP                 source/tools/sect/section_readp.F
Chd|====================================================================
      SUBROUTINE SECTION_FIO(NSTRF  ,V      ,VR    ,
     2                       A      ,AR     ,SECBUF,MS   ,IN ,
     3                       WEIGHT ,IAD_CUT,FR_CUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),WEIGHT(*), IAD_CUT(NSPMD+2,*), FR_CUT(*)
      my_real
     .   V(3,*), VR(3,*), A(3,*), AR(3,*), MS(*),
     .   SECBUF(*), IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,
     .   IFRL1, IFRL2, L,TYPE, NNOD,KR11,KR12,
     .   KR21,KR22,NBINTER,LEN,NNODT
      my_real
     .   DW, TT1, TT2, TT3, VI, DD, D1, D2,TFEXTL,
     .   TNEXT, DELTAT,ERR(8), FF, FOLD, ALPHA,AA,DTINV
C
      IF(NSECT==0)RETURN
      IF(NSTRF(2)==0)RETURN
C-----------------------------------------------
C READ FILE dans l'ordre des sections lues sur le fichier
C  T = TT
C-----------------------------------------------
       IF(NSPMD==1) THEN
         CALL SECTION_READ (TT ,NSTRF  ,SECBUF)
       ELSE
         NNODT = 0
         IF(ISPMD==0) THEN
           K0  = NSTRF(25)
           DO I = 1, NSECT
             IF(NSTRF(K0)>=100) NNODT = NNODT + IAD_CUT(NSPMD+2,I)
             K0  = NSTRF(K0+24)
           END DO
         END IF
C
C SPMD SPECIFIC : MAJ MODIF NSTRF et SECBUF dans SECT_READP
C
         CALL SECTION_READP(TT,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
       END IF
C-----------------------------------------------
C IMPOSED FORCES
C-----------------------------------------------
       TT1 = SECBUF(2)
       TT2 = SECBUF(3)
       TT3 = SECBUF(4)
       DTINV=ZERO
       IF(DT1>ZERO)DTINV=ONE/DT1
       IF(NSTRF(2)>=1)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          ALPHA = SECBUF(KR0+2)
          IF(TYPE>=101.AND.ALPHA/=0.0)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR11 = KR1 + IFRL2*6*NNOD
            KR12 = KR1 + IFRL1*6*NNOD
            KR21 = KR2 + IFRL2*6*NNOD
            KR22 = KR2 + IFRL1*6*NNOD
            DW   = SECBUF(KR0+3)
            IF(ISPMD==0) THEN
              TFEXTL=DW*DT1
            ELSE
              TFEXTL=ZERO
            ENDIF
            TFEXT=TFEXT + TFEXTL
            DW=0.
            DO K=1,3
              DO I=1,NNOD
                II = NSTRF(K2+I-1)
                D2 = SECBUF(KR22+6*I-7+K)
                D1 = SECBUF(KR21+6*I-7+K)
                AA = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                D2 = SECBUF(KR12+6*I-7+K)
                D1 = SECBUF(KR11+6*I-7+K)
                DD = MS(II)*(D2-D1) / (TT2-TT1)
                AA = DD*DTINV + AA
                A(K,II) = A(K,II) + AA
                IF(WEIGHT(II)==1) THEN
                  DW = DW + HALF*V(K,II)*AA
                ENDIF
              ENDDO
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                  II = NSTRF(K2+I-1)
                  D2 = SECBUF(KR22+6*I-4+K)
                  D1 = SECBUF(KR21+6*I-4+K)
                  AA = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                  D2 = SECBUF(KR12+6*I-4+K)
                  D1 = SECBUF(KR11+6*I-4+K)
                  DD = IN(II)*(D2-D1) / (TT2-TT1)
                  AA = DD*DTINV + AA
                  AR(K,II) = AR(K,II) + AA
                  IF(WEIGHT(II)==1) THEN
                    DW = DW + HALF*VR(K,II)*AA
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
            TFEXTL=TFEXTL + DT1*DW
            TFEXT=TFEXT + DT1*DW
            SECBUF(KR0+3) = DW
            SECBUF(KR0+4) = TFEXTL
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
       ENDIF
C---------------------------------------------------------
      RETURN
      END
