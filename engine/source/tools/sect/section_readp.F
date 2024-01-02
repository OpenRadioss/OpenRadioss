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
Chd|  SECTION_READP                 source/tools/sect/section_readp.F
Chd|-- called by -----------
Chd|        SECTION_FIO                   source/tools/sect/section_fio.F
Chd|        SECTION_IO                    source/tools/sect/section_io.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CLOSE_C                       ../common_source/tools/input_output/write_routtines.c
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        OPEN_C                        ../common_source/tools/input_output/write_routtines.c
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        READ_R_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_CUT                   source/mpi/sections/spmd_section.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SECTION_READP(TTT,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNODT, NSTRF(*), IAD_CUT(NSPMD+2,*), FR_CUT(*)
      my_real TTT, SECBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,KC,IFLG,
     .   IR1, IR2, IFRL1, IFRL2, FOUND, NR, L, LL, NSECR, ID_SEC,NNODG,
     .   TYPE, IFILNAM(2148), LROOTLEN, LREC, NNOD,IR, NNODR,KR11,KR12,
     .   KR21,KR22,NBINTER,IEXTRA, ADDSEC(2*NSECT)
      my_real TT1, TT2, TT3, BUFCOM(3*NSECT+7), SECBUFG(24*NNODT)
      CHARACTER FILNAM*12,LCHRUN*2
      LOGICAL FEXIST
      REAL*4 R4

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
C-----------------------------------------------
C READ FILE dans l'ordre des sections lues sur le fichier
C  TTT = TT ou TT + DT2
C-----------------------------------------------
C   debranchement pi <> p0
      IF(ISPMD/=0) GO TO 100
C   Init buffer communication
      DO I = 1, NSECT
        BUFCOM(I) = ZERO
        BUFCOM(I+NSECT) = ZERO
        BUFCOM(I+2*NSECT) = ZERO
        ADDSEC(I) = ZERO
        ADDSEC(I+NSECT) = ZERO
      END DO
C
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IEXTRA=NSTRF(3)
      IF(NSTRF(2)>=1.AND.TTT>=TT2.AND.IEXTRA==0
     .       .AND. TTT <= TSTOP)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        LL=1
        IF(ISPMD==0) THEN
          CALL CUR_FIL_C(4)
        END IF
        L = 0
        DO WHILE(TT2<=TTT)
          IFRL1=IFRL2
          IFRL2=MOD(IFRL1+1,2)
          CALL READ_R_C(R4,1)
C test EOF-------------------------------------------------------------------
          IF(R4>=0.0)THEN
            TT1=TT2
            TT2=R4
          ELSEIF(TT3==EP30)THEN
            CALL CLOSE_C()
            IEXTRA=1
            NSTRF(3)=IEXTRA
            GOTO 100
          ELSE
            CALL CLOSE_C()
            IR2=NSTRF(5)
            IR1=IR2
            IR=IR1
            LROOTLEN=0
            DO I=1,8
                FILNAM(I:I)=CHAR(NSTRF(15+I))
                IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
            ENDDO
            DOWHILE(TT3<=TTT.AND.IR<100)
              IR=IR+1
              WRITE(LCHRUN,'(I2.2)')IR
              FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
              INQUIRE(FILE=FILNAM,EXIST=FEXIST)

              IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  OUTFILE_NAME_LEN +LEN_TRIM(FILNAM)
                TMP_NAME(1:LEN_TMP_NAME)=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
              ENDIF

              IF(FEXIST)THEN
                IR2=IR
                CALL CUR_FIL_C(4)
                DO I=1,LEN_TMP_NAME
                  IFILNAM(I)=ICHAR(TMP_NAME(I:I))
                ENDDO

                CALL OPEN_C(IFILNAM,TMP_NAME,1)
                CALL READ_R_C(R4,1)
                CALL CLOSE_C()
                TT3=R4
              ENDIF
            ENDDO
            IF(IR==100)THEN
              TT3=EP30
              IEXTRA=1
              NSTRF(3)=IEXTRA
              GOTO 100
            ENDIF
            WRITE(LCHRUN,'(I2.2)')IR1
            FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
            INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

            IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  LEN_TRIM(FILNAM)
                TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
            ENDIF

            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
C
            SECBUF(4) = TT3
C
            NSTRF(4) = IR1
            NSTRF(5) = IR2
C
            CALL READ_R_C(R4,1)
            IF(R4 /= ZERO) L = 0
            TT1=TT2
            TT2=R4
          ENDIF
C-----------------------------------------------
          CALL READ_I_C(LL,1)
          CALL READ_I_C(NSECR,1)
          DO NR=1,NSECR
            CALL READ_I_C(ID_SEC,1)
            K0  = NSTRF(25)
            KR0 = NSTRF(26)
            FOUND=0
            N=0
            DOWHILE(FOUND==0.AND.N<NSECT)
                N=N+1
                IF(ID_SEC==NSTRF(K0+23))THEN
                  FOUND=1
                ELSE
C                  KR0 = NSTRF(K0+25)
                  K0  = NSTRF(K0+24)
                ENDIF
            ENDDO
            IF(FOUND==1) THEN
              NNOD = IAD_CUT(NSPMD+2,N)
            END IF
C            NNOD = NSTRF(K0+6)
C            KR1 = KR0 + 10 + IFRL1*6*NNOD
C            KR2 = KR1 + 12*NNOD
C            KR3 = KR2 + 12*NNOD
            CALL READ_I_C(TYPE,1)
            CALL READ_I_C(NNODR,1)
            IF (NNOD/=NNODR .AND. FOUND == 1) THEN
              CALL ANCMSG(MSGID=35,ANMODE=ANINFO_BLIND,
     .                    I1=ID_SEC,I2=NNODR,I3=NNOD)
              CALL ARRET(2)
            END IF
            IF(FOUND==0.OR.NSTRF(K0)<100)THEN
C skip deplacements et forces
              IF(TYPE>=1)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO
              ENDIF
              IF(TYPE>=2)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO
              ENDIF
            ELSEIF(NSTRF(K0)==100)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                BUFCOM(N) = 1
                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO
              ELSE
C Pb de compatibilite type_new>=100 et type_old<1
              ENDIF
              IF(TYPE>=2)THEN
C skip forces
                DO I=1,6*NNOD
                  CALL READ_R_C(R4,1)
                ENDDO
              ENDIF
            ELSEIF(NSTRF(K0)==101)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                BUFCOM(N) = 1
                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO
              ELSE
C Pb de compatibilite type_new>=101 et type_old<1
              ENDIF
              IF(TYPE>=2)THEN
C lecture forces
                BUFCOM(N) = 2
c                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
c                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO
              ELSE
C Pb de compatibilite type_new>=101 et type_old<2
              ENDIF
            ELSEIF(NSTRF(K0)>=102)THEN
C a faire
            ENDIF
          ENDDO
        ENDDO
C-----------------------------------------------
        SECBUF(2) = TT1
        SECBUF(3) = TT2
C
        NSTRF(7) = IFRL1
      ENDIF
      BUFCOM(3*NSECT+1) = NSTRF(3)
      BUFCOM(3*NSECT+2) = NSTRF(4)
      BUFCOM(3*NSECT+3) = NSTRF(5)
      BUFCOM(3*NSECT+4) = NSTRF(7)
      BUFCOM(3*NSECT+5) = SECBUF(2)
      BUFCOM(3*NSECT+6) = SECBUF(3)
      BUFCOM(3*NSECT+7) = SECBUF(4)
 100  CONTINUE
      CALL SPMD_RBCAST(BUFCOM,BUFCOM,3*NSECT+7,1,0,2)
      IF(ISPMD/=0) THEN
         NSTRF(3) = NINT(BUFCOM(3*NSECT+1))
         NSTRF(4) = NINT(BUFCOM(3*NSECT+2))
         NSTRF(5) = NINT(BUFCOM(3*NSECT+3))
         NSTRF(7) = NINT(BUFCOM(3*NSECT+4))
         SECBUF(2) = BUFCOM(3*NSECT+5)
         SECBUF(3) = BUFCOM(3*NSECT+6)
         SECBUF(4) = BUFCOM(3*NSECT+7)
      END IF
C
C Traitement Passage de SECBUFG a SECBUF local
C
      L = 1
      KC = 1
      K0  = NSTRF(25)
      KR0 = NSTRF(26)
      DO I = 1, NSECT
        IF(NINT(BUFCOM(I))>0) THEN
          IF(ISPMD==0) THEN
            NNODG = IAD_CUT(NSPMD+2,I)
          ELSE
            NNODG = 0
          END IF
          NNOD = NSTRF(K0+6)
          IFLG = NINT(BUFCOM(I))
          IF(NINT(BUFCOM(NSECT+I))==1) THEN
C remplissage secbuf avec ifrl1 = 0
            IFRL1 = 0
            KR1 = KR0 + 10 + IFRL1*6*NNOD
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            IF(ISPMD==0) THEN
              L = ADDSEC(I+IFRL1*NSECT)
            END IF
            CALL SPMD_SD_CUT(
     1        SECBUFG(L),NNODG       ,SECBUF(KR1),SECBUF(KR2),NNOD,
     2        FR_CUT(KC),IAD_CUT(1,I),IFLG                        )
          END IF
          IF(NINT(BUFCOM(2*NSECT+I))==1) THEN
C remplissage secbuf avec ifrl1 = 1
            IFRL1 = 1
            KR1 = KR0 + 10 + IFRL1*6*NNOD
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            IF(ISPMD==0) THEN
              L = ADDSEC(I+IFRL1*NSECT)
            END IF
            CALL SPMD_SD_CUT(
     1        SECBUFG(L),NNODG       ,SECBUF(KR1),SECBUF(KR2),NNOD,
     2        FR_CUT(KC),IAD_CUT(1,I),IFLG                        )
          END IF
        END IF
        IF(NSTRF(K0)>=100.AND.ISPMD==0) THEN
          KC = KC + IAD_CUT(NSPMD+1,I)
        END IF
        KR0 = NSTRF(K0+25)
        K0  = NSTRF(K0+24)
      END DO
C
      RETURN
      END
