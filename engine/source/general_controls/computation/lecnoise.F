Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        EOR_C                         source/output/tools/sortie_c.c
Chd|        MY_CTIME                      source/system/timer_c.c       
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECNOISE(INOISE,ITABM1)
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
#include      "com05_c.inc"
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
C     REAL
C      my_real
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICODE,ITITLE(80),I,IFILNAM(2148),IWA(33),NOILEN,ITIT40(40),
     .        K,ITEST,J,
     .        INOITMP(NNOISE)
C     REAL
      my_real
     . TITLE(20),TIT40(10)
       CHARACTER FILNAM*100, CH8*8 , EOR*8, CARD*80, CH40*40, CH80*80
       INTEGER :: LEN_TMP_NAME
       CHARACTER(len=2148) :: TMP_NAME 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C     REAL
      CHARACTER STRI*8
C-----------------------------------------------
      EXTERNAL STRI
      INTEGER  SYSFUS2
      my_real
     .   SYSFUS
      DATA EOR/'ZZZZZEOR'/
C-----------------------------------------------
CFP Modif format @T + ajout acceleration et pression
c si on de demande pas explicitement une reinitialisation 
c et qu'il existe un sauvegarde anterieure alors on continue
      IF(RNOI.EQ.0 .AND. NNOISER.NE.0)RNOI=1
c reinitialisation demandee
      IF(RNOI.EQ.2)RNOI=0
      IF(RNOI.EQ.0)THEN
       IF(NNOISER.NE.0.AND.ISPMD.EQ.0)THEN
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
       IF(ISPMD.EQ.0) THEN
         DO I = 1, NNOISE
           IF(INOITMP(I).EQ.0) THEN
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
       IF(NNOISER.EQ.0.AND.ISPMD.EQ.0) THEN
        CALL ANCMSG(MSGID=138,ANMODE=ANINFO)
        CALL ARRET(2)
       ENDIF
C
       IF(ISPMD.EQ.0)
     .   WRITE(IOUT,*)' CONTINUING NOISE SAMPLING FROM PREVIOUS RUN'
       ITEST=0
       IF(NOISEV.NE.0 .AND. INOISE(2*NNOISE+8).EQ.0)THEN
       IF(ISPMD.EQ.0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='VELOCITIES')
        ITEST=1
       ENDIF
       IF(NOISEP.NE.0 .AND. INOISE(2*NNOISE+9).EQ.0)THEN
        IF(ISPMD.EQ.0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='PRESSURES')
        ITEST=1
       ENDIF
       IF(NOISEA.NE.0 .AND. INOISE(2*NNOISE+10).EQ.0)THEN
        IF(ISPMD.EQ.0)
     .   CALL ANCMSG(MSGID=140,ANMODE=ANINFO,
     .               C1='ACCELERATIONS')
        ITEST=1
       ENDIF
       IF(ITEST.EQ.1)CALL ARRET(2)
       NOISEV=INOISE(2*NNOISE+8)
       NOISEP=INOISE(2*NNOISE+9)
       NOISEA=INOISE(2*NNOISE+10)
      ENDIF      
      IF(NOISEV+NOISEP+NOISEA.EQ.0)NOISEV=1
      IF(ISPMD.EQ.0) THEN
        WRITE(IOUT,999)
        IF(NOISEV.NE.0)WRITE(IOUT,'(A)')' ... VELOCITIES'
        IF(NOISEA.NE.0)WRITE(IOUT,'(A)')' ... ACCELERATIONS'
        IF(NOISEP.NE.0)WRITE(IOUT,'(A)')' ... PRESSURES'
        WRITE(IOUT,1000)
        WRITE(IOUT,'(10I10)')  (INOISE(I+NNOISE),I=1,NNOISE)
      ENDIF
C
      NCNOIS=3*(NOISEV+NOISEA)+NOISEP
      IF(ISPMD.NE.0) RETURN
C
C FICHIER @T ENTETE
C
      IUNIT=IUNOI
      ICODE=3040
      NOILEN=MIN(ROOTLEN,7)
      FILNAM=ROOTNAM(1:NOILEN)//'_'//CHRUN//'_@.thy'
      LEN_TMP_NAME = OUTFILE_NAME_LEN + NOILEN + 11
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))  
      IF(ITFORM.EQ.0)THEN
       OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='UNFORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM.EQ.1.OR.ITFORM.EQ.2)THEN
       OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM.EQ.3)THEN
       DO 1 I=1,LEN_TMP_NAME
 1     IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       CALL CUR_FIL_C(IUNIT)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
      ELSEIF(ITFORM.EQ.4)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,3)
       ITFORM=3
      ELSEIF(ITFORM.EQ.5)THEN
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
      WRITE(CARD,'(20A4)')TEXT
      READ(CARD,'(20A4)')TITLE
      IF(ITFORM.EQ.0)THEN
        WRITE(IUNIT)ICODE,TITLE
      ELSEIF(ITFORM.EQ.1)THEN
        CH8=STRI(ICODE)
       WRITE(IUNIT,'(A)')FILNAM(1:NOILEN+11)
        WRITE(IUNIT,'(2A)')CH8,CARD(1:72)
      ELSEIF(ITFORM.EQ.2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:NOILEN+11),' FORMAT'
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',72,'C'
        WRITE(IUNIT,'(I5,A)')ICODE,CARD(1:72)
      ELSEIF(ITFORM.EQ.3)THEN
       DO 5 I=1,80
 5     ITITLE(I)=ICHAR(CARD(I:I))
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
      IF(ITFORM.EQ.0)THEN
        READ(CH80,'(20A4)')TITLE
        WRITE(IUNIT)TITLE
      ELSEIF(ITFORM.EQ.1)THEN
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITFORM.EQ.2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:ROOTLEN+11),' FORMAT'
        WRITE(IUNIT,'(A,I5,A)')EOR,80,'C'
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITFORM.EQ.3)THEN
       CALL EOR_C(80)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(80)
      ENDIF
C-------HIERARCHIE INFO------------
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
C          IWA(I)=I
C      ENDDO
C      CALL WRTDES(IWA,IWA,12,ITFORM,0)
C-------PART DESCRIPTION------------
      CH40='FAKE'
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM.EQ.0)THEN
	WRITE(IUNIT)1,TIT40,0,1,1,0
      ELSEIF(ITFORM.EQ.1)THEN
      ELSEIF(ITFORM.EQ.2)THEN
	WRITE(IUNIT,'(A,I5,A,I5,A,I5,A)')EOR,1,'I',40,'C',4,'I'
	WRITE(IUNIT,'(I5,A,4I5)')1,CH40,0,1,1,0
      ELSEIF(ITFORM.EQ.3)THEN
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
      IF(ITFORM.EQ.0)THEN
        WRITE(IUNIT)1,TIT40
      ELSEIF(ITFORM.EQ.1)THEN
      ELSEIF(ITFORM.EQ.2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
        WRITE(IUNIT,'(I5,A)')1,CH40
      ELSEIF(ITFORM.EQ.3)THEN
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
C      IF(ITFORM.EQ.0)THEN
C        WRITE(IUNIT)0,TIT40
C      ELSEIF(ITFORM.EQ.1)THEN
C      ELSEIF(ITFORM.EQ.2)THEN
C        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
C        WRITE(IUNIT,'(I5,A)')0,CH40
C      ELSEIF(ITFORM.EQ.3)THEN
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
      IF(ITFORM.EQ.0)THEN
        WRITE(IUNIT)1,TIT40
      ELSEIF(ITFORM.EQ.1)THEN
      ELSEIF(ITFORM.EQ.2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
        WRITE(IUNIT,'(I5,A)')1,CH40
      ELSEIF(ITFORM.EQ.3)THEN
       CALL EOR_C(44)
       CALL WRITE_I_C(1,1)
       CALL WRITE_C_C(ITIT40,40)
       CALL EOR_C(44)
      ENDIF
C-------HIERARCHIE DESCRIPTION------------
      CH40=ROOTNAM
      READ(CH40,'(10A4)')TIT40
      DO I=1,40
       ITIT40(I)=ICHAR(CH40(I:I))
      ENDDO
      IF(ITFORM.EQ.0)THEN
        WRITE(IUNIT)0,0,0,1,0,TIT40
      ELSEIF(ITFORM.EQ.1)THEN
      ELSEIF(ITFORM.EQ.2)THEN
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',40,'C'
        WRITE(IUNIT,'(5I5,A)')0,0,0,1,0,CH40
      ELSEIF(ITFORM.EQ.3)THEN
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
      IF(ITFORM.EQ.0)THEN
    	WRITE(IUNIT)1,0,0,NNOISE,NCNOIS,TIT40
      ELSEIF(ITFORM.EQ.1)THEN
      ELSEIF(ITFORM.EQ.2)THEN
    	WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',40,'C'
    	WRITE(IUNIT,'(5I5,A)')1,0,0,NNOISE,NCNOIS,CH40
      ELSEIF(ITFORM.EQ.3)THEN
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
        IF(ITFORM.EQ.0)THEN
           WRITE(IUNIT)INOISE(I+NNOISE),TIT40
         ELSEIF(ITFORM.EQ.1)THEN
         ELSEIF(ITFORM.EQ.2)THEN
           WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',40,'C'
           WRITE(IUNIT,'(I5,A)')INOISE(I+NNOISE),CH40
         ELSEIF(ITFORM.EQ.3)THEN
          CALL EOR_C(44)
          CALL WRITE_I_C(INOISE(I+NNOISE),1)
          CALL WRITE_C_C(ITIT40,40)
          CALL EOR_C(44)
         ENDIF
      ENDDO
C
      K=0
      IF(NOISEV.EQ.1)THEN
        K=K+1
        IWA(K)=1
        K=K+1
        IWA(K)=2
        K=K+1
        IWA(K)=3
      ENDIF
      IF(NOISEA.EQ.1)THEN
        K=K+1
        IWA(K)=4
        K=K+1
        IWA(K)=5
        K=K+1
        IWA(K)=6
      ENDIF
      IF(NOISEP.EQ.1)THEN
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
