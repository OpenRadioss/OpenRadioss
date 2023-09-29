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
Chd|  SECT_INI                      source/tools/sect/sectio.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        READ_R_C                      source/output/tools/sortie_c.c
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SECTION_INIT (NSTRF,SECBUF,NOM_SECT,ISECTR,NSECT,IOLDSECT)
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
#include      "comlock.inc"
#include      "com08_c.inc"
#include      "chara_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),NOM_SECT(*),ISECTR,NSECT,IOLDSECT
      my_real
     .   SECBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IR, N, IR1, IR2, IFILNAM(2548),LROOTLEN,LEN,K0,ID_SEC
      my_real
     .   TT1, TT2, TT3, TMP(20)
      CHARACTER FILNAM*500,LCHRUN*2,LCHRUN_P1*2,CH_IDSEC*10
      LOGICAL FEXIST
      REAL*4 R4
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
C-----------------------------------------------
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IR1=NSTRF(4)
      IR2=NSTRF(5)
C-----------------------------------------------
C WRITE FILE
C-----------------------------------------------
      IF (ISPMD==0) THEN
        K0 = NSTRF(25)
        DO J=1,NSECT
         IF(NSTRF(K0)>=1 .AND. NSTRF(K0)<=10 )THEN
          WRITE(LCHRUN,'(I2.2)')IRUN
          LROOTLEN=0
          DO I=1,500
c ncharline of starter =500
            FILNAM(I:I)=CHAR(NOM_SECT((J-1)*500+I))
            IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
          ENDDO
          IF (LROOTLEN == 0 .AND. ABS(IOLDSECT) == 1) THEN
            IOLDSECT = 1
          ELSEIF( LROOTLEN /= 0 .AND. (IOLDSECT >= 1)) THEN
            IOLDSECT = 2
          ENDIF
         ENDIF
         K0  = NSTRF(K0+24)
        ENDDO
        IF(NSTRF(1)>=1 .AND. IOLDSECT == 1)THEN
C    FILNAM=ROOTNAM(1:ROOTLEN)//'SC'//CHRUN
          WRITE(LCHRUN,'(I2.2)')IRUN
          FILNAM=ROOTNAM(1:ROOTLEN)//'SC'//LCHRUN
          LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 4
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+4)
          DO I=1,LEN_TMP_NAME
            IFILNAM(I)=ICHAR(TMP_NAME(I:I))
          ENDDO
          CALL CUR_FIL_C(42)
          CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
        ELSEIF(NSTRF(1)>=1) THEN
          K0 = NSTRF(25)
          DO J=1,NSECT
           IF(NSTRF(K0)>=1 .AND. NSTRF(K0)<=10 )THEN
            WRITE(LCHRUN,'(I2.2)')IRUN
            LROOTLEN=0
            DO I=1,500
c ncharline of starter =500
              IF(CHAR(NOM_SECT((J-1)*500+I))/=' ')THEN
                LROOTLEN=LROOTLEN+1
                FILNAM(LROOTLEN:LROOTLEN)=CHAR(NOM_SECT((J-1)*500+I))
              ENDIF
            ENDDO
            IF (LROOTLEN == 0) THEN
              WRITE(CH_IDSEC,'(I10.10)')NSTRF(K0+23)
              FILNAM=ROOTNAM(1:ROOTLEN)//CH_IDSEC//'SC'//LCHRUN
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 14
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+14)
              DO I=1,LEN_TMP_NAME
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              CALL CUR_FIL_C(41+J)
              CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
            ELSE
              FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
              LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
              DO I=1,LEN_TMP_NAME
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              CALL CUR_FIL_C(41+J)
              CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
            ENDIF
           ENDIF
          K0  = NSTRF(K0+24)
          ENDDO
        ENDIF
C-----------------------------------------------
C READ FILES
C-----------------------------------------------
       IF(NSTRF(2)>=1)THEN
        LROOTLEN=0
        DO I=1,500
c ncharline of starter =500
          FILNAM(I:I)=CHAR(NOM_SECT((ISECTR-1)*500+I))
          IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
        ENDDO
        IF(TT==ZERO)THEN
          TT1=ZERO
          TT2=ZERO
          FILNAM=FILNAM(1:LROOTLEN)//'SC01'
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LROOTLEN + 4
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LROOTLEN+4)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO
            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT1=R4
            IR1=1
            IR2=1
          ELSE
            CALL ANCMSG(MSGID=188,ANMODE=ANINFO,
     .                  C1=FILNAM)
            IERR=IERR+1
            TSTOP = TT
          ENDIF
          FILNAM=FILNAM(1:LROOTLEN)//'SC02'
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)

          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LROOTLEN + 4
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LROOTLEN+4)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            IR2=2
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME!LROOTLEN+4
                  IFILNAM(I)=ICHAR(FILNAM(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT3=R4
          ELSE
            TT3=EP30
          ENDIF
        ELSE
          TT1=ZERO
          TT2=ZERO
          WRITE(LCHRUN,'(I2.2)')IRUN
          FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LEN_TRIM(FILNAM)
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TRIM(FILNAM))
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT1=R4
            IR1=IRUN
            IR2=IRUN
          ELSE
            CALL ANCMSG(MSGID=188,ANMODE=ANINFO,
     .                  C1=FILNAM)
            IERR=IERR+1
            TSTOP = TT
          ENDIF
          WRITE(LCHRUN_P1,'(I2.2)')IRUN+1
          FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN_P1
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

          INQUIRE(FILE=TMP_NAME,EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME =  LEN_TRIM(FILNAM)
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            IR2=IRUN + 1
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
                  IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT3=R4
          ELSE
            TT3=EP30
          ENDIF
        ENDIF
C
        WRITE(LCHRUN,'(I2.2)')IR1
        FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

        CALL CUR_FIL_C(4)
        DO I=1,LEN_TMP_NAME
            IFILNAM(I)=ICHAR(TMP_NAME(I:I))
        ENDDO

        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
       ENDIF
      ENDIF
C-----------------------------------------------
      SECBUF(2) = TT1
      SECBUF(3) = TT2
      SECBUF(4) = TT3
C
      NSTRF(4) = IR1
      NSTRF(5) = IR2
C
C MAJ SPMD sur proc remote
C
      IF (NSPMD > 1) THEN
       IF(ISPMD==0) THEN
         TMP(1) = NSTRF(4)
         TMP(2) = NSTRF(5)
         TMP(3) = SECBUF(2)
         TMP(4) = SECBUF(3)
         TMP(5) = SECBUF(4)
         LEN = 5
C   2*LEN necessaire pour communication !!!
         CALL SPMD_RBCAST(TMP,TMP,LEN,1,0,2)
       ELSE
         LEN = 5
C   2*LEN necessaire pour communication !!!
         CALL SPMD_RBCAST(TMP,TMP,LEN,1,0,2)
         NSTRF(4) = TMP(1)
         NSTRF(5) = TMP(2)
         SECBUF(2)= TMP(3)
         SECBUF(3)= TMP(4)
         SECBUF(4)= TMP(5)
       ENDIF
      END IF
C
      RETURN
      END
