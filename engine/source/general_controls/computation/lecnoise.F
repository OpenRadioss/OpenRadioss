Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    lecnoise               ../engine/source/general_controls/computation/lecnoise.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../engine/source/input/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../engine/source/output/message/message.F
!||    arret                  ../engine/source/system/arret.F
!||    cur_fil_c              ../common_source/tools/input_output/write_routtines.c
!||    eor_c                  ../common_source/tools/input_output/write_routtines.c
!||    my_ctime               ../engine/source/system/timer_c.c
!||    open_c                 ../common_source/tools/input_output/write_routtines.c
!||    spmd_glob_isum9        ../engine/source/mpi/interfaces/spmd_th.F
!||    sysfus2                ../engine/source/system/sysfus.F
!||    write_c_c              ../common_source/tools/input_output/write_routtines.c
!||    write_i_c              ../common_source/tools/input_output/write_routtines.c
!||    wrtdes                 ../engine/source/output/th/wrtdes.F
!||--- uses       -----------------------------------------------------
!||    inoutfile_mod          ../common_source/modules/inoutfile_mod.F
!||    message_mod            ../engine/share/message_module/message_mod.F
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE LECNOISE(INOISE,ITABM1,NAMES_AND_TITLES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
      USE NAMES_AND_TITLES_MOD,only:NAMES_AND_TITLES_,LTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scrnoi_c.inc"
#include      "chara_c.inc"
#include      "scr05_c.inc"
#include      "scr13_c.inc"
#include      "titr_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOISE(*), ITABM1(*)
      TYPE(NAMES_AND_TITLES_),INTENT(INOUT) :: NAMES_AND_TITLES !< NAMES_AND_TITLES host the input deck names and titles for outputs
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICODE,ITITLE(80),I,IFILNAM(2148),IWA(33),NOILEN,ITIT40(40),K,ITEST,J,INOITMP(NNOISE)
      my_real TITLE(20),TIT40(10)
       CHARACTER FILNAM*100, CH8*8 , EOR*8, CH40*40, CH80*80
       CHARACTER(LEN=LTITLE) :: CARD
       INTEGER :: LEN_TMP_NAME
       CHARACTER(len=2148) :: TMP_NAME 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      CHARACTER, EXTERNAL :: STRI*8
      INTEGER,EXTERNAL ::  SYSFUS2
C-----------------------------------------------
      DATA EOR/'ZZZZZEOR'/
C-----------------------------------------------
CFP Modif format @T + ajout acceleration et pression
c si on de demande pas explicitement une reinitialisation 
c et qu'il existe un sauvegarde anterieure alors on continue
      IF(RNOI==0 .AND. NNOISER/=0)RNOI=1
c reinitialisation demandee
      IF(RNOI==2)RNOI=0
      IF(RNOI==0)THEN
       IF(NNOISER/=0.AND.ISPMD==0)THEN
        WRITE(ISTDO,*)'***WARNING A @T FILE WAS SAVED IN A PREVIOUS RUN'
        WRITE(ISTDO,*)'   FILTERING WILL BE REINITIALIZED'
        WRITE(IOUT,*)'***WARNING A @T FILE WAS SAVED IN A PREVIOUS RUN'
        WRITE(IOUT,*)'   IF YOU WANT TO RESUME DATA SAMPLING AS',
     .               ' BEFORE, PROVIDE ONLY LINE /@TFILE'
        WRITE(IOUT,*)'   OTHERWISE THE FILTERING WILL BE REINITIALIZED',
     .               ' AND YOU MAY MISS SOME SAMPLES'
       ENDIF
       READ (IIN,'(10I10)')(INOISE(I+NNOISE),I=1,NNOISE)
C
       DO I=1,NNOISE
         INOITMP(I)=SYSFUS2(INOISE(I+NNOISE),ITABM1,NUMNOD)
       END DO
       DO I=1,NNOISE
         INOISE(I)= INOITMP(I)   ! INOISE = 0 si noeud non present sur le proc
       END DO
       IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(INOITMP,NNOISE)
       IF(ISPMD==0) THEN
         DO I = 1, NNOISE
           IF(INOITMP(I)==0) THEN
             CALL ANCMSG(MSGID=139,ANMODE=ANINFO_BLIND,
     .            I1=INOISE(I+NNOISE))
             IERR=IERR+1
             RETURN
           END IF
         END DO
       ENDIF
       INOISE(2*NNOISE+8)=NOISEV
       INOISE(2*NNOISE+9)=NOISEP
       INOISE(2*NNOISE+10)=NOISEA
      ELSE
       IF(NNOISER==0.AND.ISPMD==0) THEN
        CALL ANCMSG(MSGID=138,ANMODE=ANINFO)
        CALL ARRET(2)
       ENDIF
C
       IF(ISPMD==0)
     .   WRITE(IOUT,*)' CONTINUING NOISE SAMPLING FROM PREVIOUS RUN'
       ITEST=0
       IF(NOISEV/=0 .AND. INOISE(2*NNOISE+8)==0)THEN
       IF(ISPMD==0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='VELOCITIES')
        ITEST=1
       ENDIF
       IF(NOISEP/=0 .AND. INOISE(2*NNOISE+9)==0)THEN
        IF(ISPMD==0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='PRESSURES')
        ITEST=1
       ENDIF
       IF(NOISEA/=0 .AND. INOISE(2*NNOISE+10)==0)THEN
        IF(ISPMD==0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='ACCELERATIONS')
        ITEST=1
       ENDIF
       IF(ITEST==1)CALL ARRET(2)
       NOISEV=INOISE(2*NNOISE+8)
       NOISEP=INOISE(2*NNOISE+9)
       NOISEA=INOISE(2*NNOISE+10)
      ENDIF      
      IF(NOISEV+NOISEP+NOISEA==0)NOISEV=1
      IF(ISPMD==0) THEN
        WRITE(IOUT,999)
        IF(NOISEV/=0)WRITE(IOUT,'(A)')' ... VELOCITIES'
        IF(NOISEA/=0)WRITE(IOUT,'(A)')' ... ACCELERATIONS'
        IF(NOISEP/=0)WRITE(IOUT,'(A)')' ... PRESSURES'
        WRITE(IOUT,1000)
        WRITE(IOUT,'(10I10)')  (INOISE(I+NNOISE),I=1,NNOISE)
      ENDIF
C
      NCNOIS=3*(NOISEV+NOISEA)+NOISEP
      IF(ISPMD/=0) RETURN
C
C FICHIER @T ENTETE
C
      IUNIT=IUNOI
      ICODE=3040
      NOILEN=MIN(ROOTLEN,7)
      FILNAM=ROOTNAM(1:NOILEN)//'_'//CHRUN//'_@.thy'
      LEN_TMP_NAME = OUTFILE_NAME_LEN + NOILEN + 11
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))  
      IF(ITFORM==0)THEN
       OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='UNFORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM==1.OR.ITFORM==2)THEN
       OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM==3)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(IUNIT)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
      ELSEIF(ITFORM==4)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,3)
       ITFORM=3
      ELSEIF(ITFORM==5)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,6)
       ITFORM=3
      ENDIF
C
C TITRE
C
      CARD(1:LTITLE)=NAMES_AND_TITLES%TITLE(1:LTITLE)

      IF(ITFORM==0)THEN
        ! ITFORM is no more used
      ELSEIF(ITFORM==1)THEN
        CH8=STRI(ICODE)
       WRITE(IUNIT,'(A)')FILNAM(1:NOILEN+11)
        WRITE(IUNIT,'(2A)')CH8,CARD(1:72)
      ELSEIF(ITFORM==2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:NOILEN+11),' FORMAT'
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',72,'C'
        WRITE(IUNIT,'(I5,A)')ICODE,CARD(1:72)
      ELSEIF(ITFORM==3)THEN
       DO I=1,80
         ITITLE(I)=ICHAR(CARD(I:I))
       ENDDO
       CALL EOR_C(84)
       CALL WRITE_I_C(ICODE,1)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(84)
      ENDIF
C
C-------ivers date------------
      CALL MY_CTIME(ITITLE)
      DO I=1,24
         CH80(I:I)=CHAR(ITITLE(I))
      ENDDO
      CH80(25:33) =' RADIOSS '
      CH80(34:59) =VERSIO(2)(9:34)
      CH80(60:80) =CPUNAM
      DO I=25,80
         ITITLE(I)=ICHAR(CH80(I:I))
      ENDDO
      IF(ITFORM==0)THEN
        READ(CH80,'(20A4)')TITLE
        WRITE(IUNIT)TITLE
      ELSEIF(ITFORM==1)THEN
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITFORM==2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:ROOTLEN+11),' FORMAT'
        WRITE(IUNIT,'(A,I5,A)')EOR,80,'C'
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(80)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(80)
      ENDIF
C-------HIERARCHY INFO------------
      IWA(1)=1
      IWA(2)=1
      IWA(3)=1
      IWA(4)=1
      IWA(5)=1
      IWA(6)=1
      CALL WRTDES(IWA,IWA,6,ITFORM,0)
C 1 VARIABLE GLOBALE
      IWA(1)=1
      CALL WRTDES(IWA,IWA,1,ITFORM,0)
C      DO I=1,IWA(6)
C        IWA(I)=I
C      ENDDO
C      CALL WRTDES(IWA,IWA,12,ITFORM,0)
C-------PART DESCRIPTION------------
      CH40='FAKE'
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM==0)THEN
          WRITE(IUNIT)1,TIT40,0,1,1,0
      ELSEIF(ITFORM==1)THEN
      ELSEIF(ITFORM==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A,I5,A)')EOR,1,'I',40,'C',4,'I'
          WRITE(IUNIT,'(I5,A,4I5)')1,CH40,0,1,1,0
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(60)
       CALL WRITE_I_C(1,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(1,1)
       CALL WRITE_I_C(1,1)
       CALL WRITE_I_C(0,1)
       CALL EOR_C(60)
      ENDIF
C-------MATER DESCRIPTION------------
      CH40='FAKE'
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM==0)THEN
        WRITE(IUNIT)1,TIT40
      ELSEIF(ITFORM==1)THEN
      ELSEIF(ITFORM==2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
        WRITE(IUNIT,'(I5,A)')1,CH40
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(44)
       CALL WRITE_I_C(1,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL EOR_C(44)
      ENDIF
C-------MATER DESCRIPTION------------
C      CH40=''
C      READ(CH40,'(10A4)')TIT40
C      DO I=1,40
C       ITIT40(I)=ICHAR(CH40(I:I))
C      ENDDO
C      IF(ITFORM==0)THEN
C        WRITE(IUNIT)0,TIT40
C      ELSEIF(ITFORM==1)THEN
C      ELSEIF(ITFORM==2)THEN
C        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
C        WRITE(IUNIT,'(I5,A)')0,CH40
C      ELSEIF(ITFORM==3)THEN
C       CALL EOR_C(44)
C       CALL WRITE_I_C(0,1)
C       CALL WRITE_C_C(ITIT40,40)
C       CALL EOR_C(44)
C      ENDIF

C-------GEO DESCRIPTION------------
      CH40='FAKE'
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM==0)THEN
        WRITE(IUNIT)1,TIT40
      ELSEIF(ITFORM==1)THEN
      ELSEIF(ITFORM==2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
        WRITE(IUNIT,'(I5,A)')1,CH40
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(44)
       CALL WRITE_I_C(1,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL EOR_C(44)
      ENDIF
C-------HIERARCHY DESCRIPTION------------
      CH40=ROOTNAM
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM==0)THEN
        WRITE(IUNIT)0,0,0,1,0,TIT40
      ELSEIF(ITFORM==1)THEN
      ELSEIF(ITFORM==2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',40,'C'
        WRITE(IUNIT,'(5I5,A)')0,0,0,1,0,CH40
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(60)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(1,1)
       CALL WRITE_I_C(0,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL EOR_C(60)
      ENDIF
      CALL WRTDES(IWA,IWA,1,ITFORM,0)
C-------NODE GROUPS------------
      CH40='NODES'
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
C
      IF(ITFORM==0)THEN
          WRITE(IUNIT)1,0,0,NNOISE,NCNOIS,TIT40
      ELSEIF(ITFORM==1)THEN
      ELSEIF(ITFORM==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',40,'C'
          WRITE(IUNIT,'(5I5,A)')1,0,0,NNOISE,NCNOIS,CH40
      ELSEIF(ITFORM==3)THEN
       CALL EOR_C(60)
       CALL WRITE_I_C(1,1)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(0,1)
       CALL WRITE_I_C(NNOISE,1)
       CALL WRITE_I_C(NCNOIS,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL EOR_C(60)
      ENDIF
C
      WRITE(CH40,'(40X)')
      DO I=1,NNOISE
        WRITE(CH40,'(I10)')INOISE(I+NNOISE)
        READ(CH40,'(10A4)')TIT40 
        DO J=1,40
          ITIT40(J)=ICHAR(CH40(J:J))
        ENDDO
        IF(ITFORM==0)THEN
           WRITE(IUNIT)INOISE(I+NNOISE),TIT40
         ELSEIF(ITFORM==1)THEN
         ELSEIF(ITFORM==2)THEN
           WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
           WRITE(IUNIT,'(I5,A)')INOISE(I+NNOISE),CH40
         ELSEIF(ITFORM==3)THEN
          CALL EOR_C(44)
          CALL WRITE_I_C(INOISE(I+NNOISE),1)
          CALL WRITE_C_C(ITIT40,40)
          CALL EOR_C(44)
         ENDIF
      ENDDO
C
      K=0
      IF(NOISEV==1)THEN
        K=K+1
        IWA(K)=1
        K=K+1
        IWA(K)=2
        K=K+1
        IWA(K)=3
      ENDIF
      IF(NOISEA==1)THEN
        K=K+1
        IWA(K)=4
        K=K+1
        IWA(K)=5
        K=K+1
        IWA(K)=6
      ENDIF
      IF(NOISEP==1)THEN
        K=K+1
        IWA(K)=7
      ENDIF
      CALL WRTDES(IWA,IWA,NCNOIS,ITFORM,0)
C             
      RETURN
C
  999 FORMAT(///' LIST OF VARIABLES SAVED IN NOISE FILE')
 1000 FORMAT(///' LIST OF NODES SAVED IN NOISE FILE')
      END
