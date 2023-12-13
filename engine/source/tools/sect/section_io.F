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
Chd|  SECTION_IO                    source/tools/sect/section_io.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FLU_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        SECTION                       source/tools/sect/section.F   
Chd|        SECTION_READ                  source/tools/sect/section_read.F
Chd|        SECTION_READP                 source/tools/sect/section_readp.F
Chd|        SPMD_EXCH_CUT                 source/mpi/sections/spmd_section.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_WRT_CUTD                 source/mpi/sections/spmd_section.F
Chd|        SPMD_WRT_CUTF                 source/mpi/sections/spmd_section.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE SECTION_IO (
     1    NSTRF  ,D      ,DR    ,V     ,VR      ,FSAV   ,
     2    SECFCUM,A      ,AR    ,SECBUF,MS      ,IN     ,
     3    X      ,FANI   ,WEIGHT,XSEC  ,IAD_ELEM,FR_ELEM,
     4    RG_CUT ,IAD_CUT,FR_CUT,WEIGHT_MD      ,IOLDSECT,
     5    STABSEN,DIMFB  ,TABS   ,FBSAV6  )
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
      INTEGER NSTRF(*), WEIGHT(*), IAD_ELEM(2,*), FR_ELEM(*),
     .   RG_CUT(*), IAD_CUT(NSPMD+2,*), FR_CUT(*),WEIGHT_MD(*),
     .   IOLDSECT, STABSEN,DIMFB,TABS(STABSEN)
      my_real
     .   D(3,*), DR(3,*), V(3,*), VR(3,*), A(3,*), AR(3,*), MS(*),
     .   FSAV(NTHVKI,*), SECFCUM(7,NUMNOD,*), SECBUF(*), IN(*),
     .   FANI(3,*), X(3,*), XSEC(4,3,*)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,
     .   IFRL1, IFRL2, L, ID_SEC,TYPE, LREC, NNOD,KR11,KR12, LENR,
     .   KR21,KR22,NBINTER, NN, LEN, KC, NSIZE, NNODG, SIZE, NNODT,
     .   ISECT
      my_real
     .   DW, TT1, TT2, TT3, VI, DD, D1, D2,TFEXTL,
     .   AOLD, TNEXT, DELTAT,ERR(8), FF, FOLD, ALPHA, AA, DV, WA(10)
      REAL*4 R4
C-----------------------------------------------
C
C---------------------------------------------------------
      IF(NSECT==0)RETURN
C-----------------------------------------------
C BILAN SUR SECTION AVEC SECFCUM
C---------------------------------------------------------
      K0=NSTRF(25)
      DO I=1,NSECT
         IF(NSTRF(K0)+NSTRF(K0+14)>0)THEN
C
C traitement des noeuds frontieres :
C cumul de SECFCUM sur proc main puis mise a zero ailleurs
C
           K2 = K0 + 30 + NSTRF(K0+14)
           IF(IRODDL/=0)THEN
             SIZE = 6
           ELSE
             SIZE = 3
           END IF
           IF (NSPMD > 1) THEN
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             CALL SPMD_EXCH_CUT(
     1         NSTRF(K2),SECFCUM(1,1,I),IAD_ELEM,FR_ELEM,SIZE,
     2         LENR     ,NSTRF(K0+6),WEIGHT)
           END IF
C
           K2 = K0 + 30 + NSTRF(K0+14)
           CALL SECTION(
     1     NSTRF(K0+6),NSTRF(K0+3),NSTRF(K0+4),NSTRF(K0+5),NSTRF(K2),X,
     2     V       ,VR  ,FSAV(1,I),FANI(1,1+2*(I-1)),SECFCUM(1,1,I),MS,
     3     IN      ,NSTRF(K0+26),XSEC(1,1,I) )
         ENDIF
         K0=NSTRF(K0+24)
      ENDDO
      IF(NSTRF(1)==0.AND.NSTRF(2)==0)RETURN
C-----------------------------------------------
C WRITE FILE
C-----------------------------------------------
      TNEXT  = SECBUF(5)
      DELTAT = SECBUF(1)
      LREC   = NSTRF(6)
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IF(NSTRF(1)>=1.AND.TNEXT<=TT)THEN
        SECBUF(5) = TNEXT + DELTAT
C
        K0  = NSTRF(25)
C
        KC = 1
        IF(ISPMD==0 .AND. IOLDSECT == 1) THEN
          CALL CUR_FIL_C(42)
          R4 = TT
          CALL WRITE_R_C(R4,1)
          CALL WRITE_I_C(LREC,1)
          CALL WRITE_I_C(NSTRF(1),1)
        ENDIF
        DO N=1,NSECT
          TYPE=NSTRF(K0)
          IF(ISPMD==0 .AND. IOLDSECT /= 1 .AND. TYPE >= 1) THEN
            CALL CUR_FIL_C(41+N)
            R4 = TT
            CALL WRITE_R_C(R4,1)
            CALL WRITE_I_C(1,1)
            CALL WRITE_I_C(1,1)
          ENDIF
          NBINTER = NSTRF(K0+14)
          K1 = K0+30
          K2=K1+NBINTER
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          IF(TYPE>=1)THEN
C ecriture deplacements
            ID_SEC=NSTRF(K0+23)
            IF(NSPMD==1) THEN
              CALL WRITE_I_C(ID_SEC,1)
              CALL WRITE_I_C(TYPE,1)
              CALL WRITE_I_C(NNOD,1)
            ELSEIF(ISPMD==0) THEN
              CALL WRITE_I_C(ID_SEC,1)
              CALL WRITE_I_C(TYPE,1)
              NNODG = IAD_CUT(NSPMD+2,N)
              CALL WRITE_I_C(NNODG,1)
            ENDIF
            IF(IRODDL/=0)THEN
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTD(
     1            NNOD        ,NSTRF(K2),D    ,DR    ,RG_CUT(KC),
     2            IAD_CUT(1,N),NSIZE    ,NNODG,WEIGHT,2         )
              ELSE
               DO I=1,NNOD
                R4 = D(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ELSE
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTD(
     1            NNOD        ,NSTRF(K2),D    ,DR    ,RG_CUT(KC),
     2            IAD_CUT(1,N),NSIZE    ,NNODG,WEIGHT,1         )
              ELSE
               DO I=1,NNOD
                R4 = D(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = ZERO
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ENDIF
          ENDIF
          IF(TYPE>=2)THEN
C ecriture forces
            IF(IRODDL/=0)THEN
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTF(
     1           NNOD ,NSTRF(K2),SECFCUM(1,1,N),RG_CUT(KC),IAD_CUT(1,N),
     2           NSIZE,NNODG    ,WEIGHT        ,2         )
              ELSE
               DO I=1,NNOD
                R4 = SECFCUM(1,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(2,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(3,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(5,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(6,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(7,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ELSE
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTF(
     1           NNOD ,NSTRF(K2),SECFCUM(1,1,N),RG_CUT(KC),IAD_CUT(1,N),
     2           NSIZE,NNODG    ,WEIGHT        ,1         )
              ELSE
               DO I=1,NNOD
                R4 = SECFCUM(1,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(2,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(3,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = ZERO
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ENDIF
          ENDIF

          K0  = NSTRF(K0+24)
          IF(TYPE>=1) KC = KC + NNOD
        ENDDO
        IF(ISPMD==0) CALL FLU_FIL_C
      ENDIF
C-----------------------------------------------
C FORCES ERROR
C T = TT
C-----------------------------------------------
      IF(NSTRF(2)>=1)THEN
C-----------------------------------------------
C Calcul erreur locale cummulee dans FSAVE
C-----------------------------------------------
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          IF(TYPE>=101)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR21 = KR2 + IFRL2*6*NNOD
            KR22 = KR2 + IFRL1*6*NNOD
            ERR(4) = ZERO
            ERR(8) = ZERO
            DO K=1,3
              ERR(K) = ZERO
              ERR(K+4) = ZERO
              DO I=1,NNOD
                II = NSTRF(K2+I-1)
                IF(WEIGHT_MD(II)==1)THEN
                  FOLD = SECFCUM(K,II,N)
                  D2 = SECBUF(KR22+6*I-7+K)
                  D1 = SECBUF(KR21+6*I-7+K)
                  FF = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                  ERR(K) = ERR(K) + (FF - FOLD)
                  ERR(4) = ERR(4) + (FF - FOLD)**2
                END IF
              ENDDO
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                  II = NSTRF(K2+I-1)
                  IF(WEIGHT_MD(II)==1)THEN
                    FOLD = SECFCUM(K+4,II,N)
                    D2 = SECBUF(KR22+6*I-4+K)
                    D1 = SECBUF(KR21+6*I-4+K)
                    FF = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                    ERR(K+4) = ERR(K+4) + (FF - FOLD)
                    ERR(8) = ERR(8) + (FF - FOLD)**2
                  END IF
                ENDDO
              ENDIF
            ENDDO
            FSAV(11,N) = FSAV(11,N) + ERR(1)*DT12
            FSAV(12,N) = FSAV(12,N) + ERR(2)*DT12
            FSAV(13,N) = FSAV(13,N) + ERR(3)*DT12
            FSAV(14,N) = ERR(4)
            FSAV(16,N) = FSAV(16,N) + ERR(5)*DT12
            FSAV(17,N) = FSAV(17,N) + ERR(6)*DT12
            FSAV(18,N) = FSAV(18,N) + ERR(7)*DT12
            FSAV(19,N) = ERR(8)
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
      ENDIF
C-----------------------------------------------
C READ FILE dans l ordre des sections lues sur le fichier
C  T = TT + DT2
C-----------------------------------------------
       IF(NSPMD==1) THEN
         CALL SECTION_READ (TT+DT2 ,NSTRF  ,SECBUF)
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
         CALL SECTION_READP(TT+DT2,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
       END IF
C-----------------------------------------------
C IMPOSED VELOCITY
C  T = TT + DT2
C-----------------------------------------------
       TT1 = SECBUF(2)
       TT2 = SECBUF(3)
       TT3 = SECBUF(4)
       IF(NSTRF(2)>=1)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          ALPHA = 1.-SECBUF(KR0+2)
          IF(TYPE>=100.AND.ALPHA/=0.0)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR11 = KR1 + IFRL2*6*NNOD
            KR12 = KR1 + IFRL1*6*NNOD
            DW   = SECBUF(KR0+1)
            IF(ISPMD==0) THEN
              TFEXTL=DW*DT1
            ELSE
              TFEXTL=ZERO
            ENDIF
            TFEXT=TFEXT + TFEXTL
            DW=ZERO
            ERR(4) = ZERO
            ERR(8) = ZERO
            DO K=1,3
              ERR(K) = ZERO
              ERR(K+4) = ZERO
              DO I=1,NNOD
               II = NSTRF(K2+I-1)
               D2 = SECBUF(KR12+6*I-7+K)
               D1 = SECBUF(KR11+6*I-7+K)
               DD = ((TT+DT2)*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
               VI = (DD-D(K,II))/DT2
               AA = ALPHA*((VI-V(K,II))/DT12 - A(K,II))
               A(K,II) = A(K,II) + AA
               IF(WEIGHT(II)==1) THEN
                DV = DT12*A(K,II)
                DW = DW + HALF*(V(K,II)+HALF*DV)*MS(II)*AA
                ERR(K)=ERR(K)+WEIGHT_MD(II)*MS(II)*(VI-V(K,II)-DV)
                ERR(4)=ERR(4)
     .               + WEIGHT_MD(II)*MS(II)*(VI**2-(V(K,II)+DV)**2)
               ENDIF
              ENDDO
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                 II = NSTRF(K2+I-1)
                 D2 = SECBUF(KR12+6*I-4+K)
                 D1 = SECBUF(KR11+6*I-4+K)
                 DD = ((TT+DT2)*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                 VI = (DD-DR(K,II))/DT2
                 AA = ALPHA*((VI-VR(K,II))/DT12 - AR(K,II))
                 AR(K,II) = AR(K,II) + AA
                 IF(WEIGHT(II)==1) THEN
                  DV = DT12*AR(K,II)
                  DW = DW + HALF*(VR(K,II)+HALF*DV)*IN(II)*AA
                  ERR(K+4)=ERR(K+4)
     .                   + WEIGHT_MD(II)*IN(II)*(VI-VR(K,II) - DV)
                  ERR(8)=ERR(8)
     .                 + WEIGHT_MD(II)*IN(II)*(VI**2-(VR(K,II)+DV)**2)
                 ENDIF
                ENDDO
              ENDIF
            ENDDO
            TFEXTL=TFEXTL + DT1*DW
            TFEXT=TFEXT + DT1*DW
            SECBUF(KR0+1) = DW
C-----------------------------------------------
C SPMD SPECIFIC : MAJ DW
C-----------------------------------------------
            IF(NSPMD>1) THEN
             IF (ISPMD==0) THEN
              WA(1) = SECBUF(KR0+1)
              WA(2) = SECBUF(KR0+3)
              WA(3) = SECBUF(KR0+4)
              LEN = 3
              CALL SPMD_GLOB_DSUM9(WA,LEN)
              SECBUF(KR0+1) = WA(1)
              SECBUF(KR0+3) = WA(2)
              SECBUF(KR0+4) = WA(3)

             ELSE
              LEN = 3
              WA(1) = SECBUF(KR0+1)
              WA(2) = SECBUF(KR0+3)
              WA(3) = SECBUF(KR0+4)
              CALL SPMD_GLOB_DSUM9(WA,LEN)
              SECBUF(KR0+1) = ZERO
              SECBUF(KR0+3) = ZERO
              SECBUF(KR0+4) = ZERO
             ENDIF
            ENDIF
C-----------------------------------------------
            FSAV(22,N) = ERR(1)
            FSAV(23,N) = ERR(2)
            FSAV(24,N) = ERR(3)
            FSAV(25,N) = HALF*ERR(4)
            FSAV(26,N) = ERR(5)
            FSAV(27,N) = ERR(6)
            FSAV(28,N) = ERR(7)
            FSAV(29,N) = HALF*ERR(8)
            FSAV(30,N) = FSAV(30,N) + TFEXTL + SECBUF(KR0+4)
            ISECT=0
            IF(STABSEN/=0) ISECT=TABS(N+1)-TABS(N)
            IF(ISECT/=0) THEN
              FBSAV6(7,2:6,ISECT) = ZERO
              FBSAV6(7,1,ISECT)=FSAV(30,N)
            ENDIF
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
       ENDIF
C---------------------------------------------------------
        K0=NSTRF(25)
        DO I=1,NSECT
          NNOD = NSTRF(K0+6)
          K2 = K0 + 30 + NSTRF(K0+14)
          DO K = 1, NNOD
            N = NSTRF(K2+K-1)
            SECFCUM(1,N,I)=ZERO
            SECFCUM(2,N,I)=ZERO
            SECFCUM(3,N,I)=ZERO
            SECFCUM(5,N,I)=ZERO
            SECFCUM(6,N,I)=ZERO
            SECFCUM(7,N,I)=ZERO
          ENDDO
          K0=NSTRF(K0+24)
        ENDDO
C
      RETURN
      END
