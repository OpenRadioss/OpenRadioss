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
Chd|  HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV_WITHOUT_UID     source/devtools/hm_reader/hm_get_floatv_without_uid.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        UDOUBLE_WO_TITLE              source/system/sysfus.F        
Chd|        UNIT_CODE                     source/general_controls/computation/unit_code.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_UNIT(UNITAB,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_) ::UNITAB
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,N,IWRITE,IERR0,LEN,I1,J,K,IREELM,IREELL,IREELT,
     .        IERR1,ID_OPT(NUNIT0+1),IS_M_STRING,IS_L_STRING,IS_T_STRING
      my_real  FAC,UNITE,BID,M_UNIT,L_UNIT,T_UNIT
      CHARACTER KEY*ncharfield,KEY1*ncharline, CFAC*ncharfield, CUNIT
      CHARACTER*20 FIELD1,FIELD2,FIELD3
      CHARACTER*20 FIELD11(NUNIT0+NSUBMOD),FIELD22(NUNIT0+NSUBMOD),FIELD33(NUNIT0+NSUBMOD),
     .             KEYMSUB,KEYLSUB,KEYTSUB,KEYMISUB,KEYLISUB,KEYTISUB

      CHARACTER*40 MESS
      LOGICAL :: IS_AVAILABLE
      CHARACTER*20 UNIT_NAME
      INTEGER NB_BEGIN,SCHAR,SUB_INDEX,NBUNIT_SUB
      my_real  FAC_M_SUB,FAC_L_SUB,FAC_T_SUB
C---------------------------
C     UNITAB(1)         unit ID
C     UNITAB(2)         Mass unit
C     UNITAB(3)         Length unit
C     UNITAB(4)         Time unit
C=======================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
      WRITE(IOUT,1000)
C
      UNITAB%NUNITS = NUNIT0+NSUBMOD+1
      ALLOCATE(UNITAB%UNIT_ID(NUNIT0+NSUBMOD+1))
      ALLOCATE(UNITAB%FAC_M  (NUNIT0+NSUBMOD+1))
      ALLOCATE(UNITAB%FAC_L  (NUNIT0+NSUBMOD+1))
      ALLOCATE(UNITAB%FAC_T  (NUNIT0+NSUBMOD+1))
C
      FAC_M_INPUT = ZERO
      FAC_L_INPUT = ZERO
      FAC_T_INPUT = ZERO
      FAC_M_WORK = ZERO
      FAC_L_WORK = ZERO
      FAC_T_WORK = ZERO
      FAC_M_SUB = ZERO
      FAC_L_SUB = ZERO
      FAC_T_SUB = ZERO
      FIELD1 = ''
      FIELD2 = ''
      FIELD3 = ''
      M_UNIT = ZERO
      L_UNIT = ZERO
      T_UNIT = ZERO
      IERR1 = 1
      LEN = ncharfield*2
      ID_OPT(1:NUNIT0) = 0
C     Chaque unite globale doit etre definie une seule fois
C     (input vers <= 90)
      IF ( FLAG_KEY_M > 1) THEN
        CALL ANCMSG(MSGID=575,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .                    C1='GLOBAL UNIT')
      ENDIF
      IF ( FLAG_KEY_L > 1) THEN
        CALL ANCMSG(MSGID=575,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .                    C1='GLOBAL UNIT')
      ENDIF
      IF ( FLAG_KEY_T > 1) THEN
        CALL ANCMSG(MSGID=575,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .                    C1='GLOBAL UNIT')
      ENDIF
C      
      CALL UNIT_CODE(LEN,KEYMI,'MASS'  ,FAC_M_INPUT, IERR1, 0)
      CALL UNIT_CODE(LEN,KEYLI,'LENGTH',FAC_L_INPUT, IERR1, 0)
      CALL UNIT_CODE(LEN,KEYTI,'TIME'  ,FAC_T_INPUT, IERR1, 0)
      CALL UNIT_CODE(LEN,KEYM ,'MASS'  ,FAC_M_WORK , IERR1, 0)
      IF (FAC_M_INPUT == ZERO) FAC_M_INPUT = FAC_M_WORK
      IF (FAC_M_WORK  == ZERO) FAC_M_WORK  = FAC_M_INPUT
      CALL UNIT_CODE(LEN,KEYL ,'LENGTH',FAC_L_WORK , IERR1, 0)
      IF (FAC_L_INPUT == ZERO) FAC_L_INPUT = FAC_L_WORK
      IF (FAC_L_WORK  == ZERO) FAC_L_WORK  = FAC_L_INPUT
      CALL UNIT_CODE(LEN,KEYT ,'TIME'  ,FAC_T_WORK , IERR1, 0)
      IF (FAC_T_INPUT == ZERO) FAC_T_INPUT = FAC_T_WORK
      IF (FAC_T_WORK  == ZERO) FAC_T_WORK  = FAC_T_INPUT
C
      FAC_MASS   = FAC_M_WORK
      FAC_LENGTH = FAC_L_WORK
      FAC_TIME   = FAC_T_WORK
      NUNITS     = 1
      IWRITE     = 1
C
      IF (FAC_M_INPUT /= FAC_M_WORK .OR.
     .    FAC_L_INPUT /= FAC_L_WORK .OR.
     .    FAC_T_INPUT /= FAC_T_WORK) THEN
        CALL ANCMSG(MSGID=754,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO)
      ENDIF
C
      CALL HM_OPTION_START('/UNIT')
C     
      DO N=1,NUNIT0
         CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       KEYWORD2 = KEY)
c
         UNIT_NAME = ''
         CALL HM_GET_STRING('UNIT_NAME',UNIT_NAME,2*ncharfield,IS_AVAILABLE)
         IF(UNIT_NAME /= 'LENGTH' .AND. UNIT_NAME /= 'MASS' .AND. UNIT_NAME /= 'TIME') THEN
           IERR0  = 1
           NUNITS = NUNITS + 1
           ID_OPT(NUNITS)=ID

           CALL HM_GET_INTV('IS_M_STRING',IS_M_STRING,IS_AVAILABLE,LSUBMODEL)
           IF(IS_M_STRING == 1) THEN
             CALL HM_GET_STRING('MUNIT_S',FIELD1,2*ncharfield,IS_AVAILABLE)
           ELSE
             CALL HM_GET_FLOATV_WITHOUT_UID('MUNIT',M_UNIT,IS_AVAILABLE)
           ENDIF
           CALL HM_GET_INTV('IS_L_STRING',IS_L_STRING,IS_AVAILABLE,LSUBMODEL)
           IF(IS_L_STRING == 1) THEN
             CALL HM_GET_STRING('LUNIT_S',FIELD2,2*ncharfield,IS_AVAILABLE)
           ELSE
             CALL HM_GET_FLOATV_WITHOUT_UID('LUNIT',L_UNIT,IS_AVAILABLE)
           ENDIF
           CALL HM_GET_INTV('IS_T_STRING',IS_T_STRING,IS_AVAILABLE,LSUBMODEL)
           IF(IS_T_STRING == 1) THEN
             CALL HM_GET_STRING('TUNIT_S',FIELD3,2*ncharfield,IS_AVAILABLE)
           ELSE
             CALL HM_GET_FLOATV_WITHOUT_UID('TUNIT',T_UNIT,IS_AVAILABLE)
           ENDIF

           IF(IS_M_STRING == 1) THEN
             CALL UNIT_CODE(LEN,FIELD1,'MASS',UNITAB%FAC_M(NUNITS),IERR0,ID)
             IWRITE = MIN(IERR0,IWRITE)
             DO K=1,20
               FIELD11(NUNITS-1)(K:K) = FIELD1(K:K)
             ENDDO
           ELSE
             UNITAB%FAC_M(NUNITS) = M_UNIT
             FIELD11(NUNITS-1) = 'N.A'
           ENDIF
           IF(IS_L_STRING == 1) THEN
             CALL UNIT_CODE(LEN,FIELD2,'LENGTH',UNITAB%FAC_L(NUNITS),IERR0,ID)
             IWRITE = MIN(IERR0,IWRITE)
             DO K=1,20
               FIELD22(NUNITS-1)(K:K) = FIELD2(K:K)
             ENDDO
           ELSE
             UNITAB%FAC_L(NUNITS) = L_UNIT
             FIELD22(NUNITS-1) = 'N.A'
           ENDIF
           IF(IS_T_STRING == 1) THEN
             CALL UNIT_CODE(LEN,FIELD3,'TIME',UNITAB%FAC_T(NUNITS),IERR0,ID)
             IWRITE = MIN(IERR0,IWRITE)
             DO K=1,20
               FIELD33(NUNITS-1)(K:K) = FIELD3(K:K)
             ENDDO
           ELSE
             UNITAB%FAC_T(NUNITS) = T_UNIT
             FIELD33(NUNITS-1) = 'N.A'
           ENDIF
           UNITAB%UNIT_ID(NUNITS) = ID
         ENDIF
C
      ENDDO
C---
      IF (FAC_MASS == ZERO) THEN
         CALL ANCMSG(MSGID=574,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1='WORK MASS')
      ENDIF
      IF (FAC_LENGTH == ZERO) THEN
         CALL ANCMSG(MSGID=574,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1='WORK LENGTH')
      ENDIF
      IF (FAC_TIME == ZERO) THEN
         CALL ANCMSG(MSGID=574,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1='WORK TIME')
      ENDIF
C
      UNITAB%UNIT_ID(1) = 0
      UNITAB%FAC_M(1)   = FAC_MASS
      UNITAB%FAC_L(1)   = FAC_LENGTH
      UNITAB%FAC_T(1)   = FAC_TIME
C
      MESS = 'UNITS                                   '
      CALL UDOUBLE_WO_TITLE(ID_OPT,1,NUNITS,MESS,0,BID)
C
      IF(NSUBMOD > 0)THEN
        CALL HM_OPTION_COUNT('/BEGIN',NB_BEGIN)
        SCHAR = 20
        NBUNIT_SUB = 0
        IF (NB_BEGIN /= 0) THEN
          CALL HM_OPTION_START('/BEGIN')
          DO I=1,NB_BEGIN
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              SUBMODEL_INDEX = SUB_INDEX)
            IF (SUB_INDEX /= 0) THEN
              NBUNIT_SUB = NBUNIT_SUB + 1
              NUNITS = NUNITS + 1

              CALL HM_GET_STRING('length_inputunit_code',KEYLISUB,SCHAR,IS_AVAILABLE)
              CALL HM_GET_STRING('mass_inputunit_code',KEYMISUB,SCHAR,IS_AVAILABLE)
              CALL HM_GET_STRING('time_inputunit_code',KEYTISUB,SCHAR,IS_AVAILABLE)
              CALL HM_GET_STRING('length_workunit_code',KEYLSUB,SCHAR,IS_AVAILABLE)
              CALL HM_GET_STRING('mass_workunit_code',KEYMSUB,SCHAR,IS_AVAILABLE)
              CALL HM_GET_STRING('time_workunit_code',KEYTSUB,SCHAR,IS_AVAILABLE)

              DO K=1,20
                FIELD11(NUNITS-1)(K:K) = KEYMISUB(K:K)
              ENDDO
              DO K=1,20
                FIELD22(NUNITS-1)(K:K) = KEYLISUB(K:K)
              ENDDO
              DO K=1,20
                FIELD33(NUNITS-1)(K:K) = KEYTISUB(K:K)
              ENDDO

              CALL UNIT_CODE(LEN,KEYMISUB,'MASS'  ,FAC_M_SUB, IERR1, 0)
              CALL UNIT_CODE(LEN,KEYLISUB,'LENGTH',FAC_L_SUB, IERR1, 0)
              CALL UNIT_CODE(LEN,KEYTISUB,'TIME'  ,FAC_T_SUB, IERR1, 0)
C
              UNITAB%UNIT_ID(NUNITS) = ID_UNIT_AUTO + NBUNIT_SUB
              UNITAB%FAC_M(NUNITS)   = FAC_M_SUB
              UNITAB%FAC_L(NUNITS)   = FAC_L_SUB
              UNITAB%FAC_T(NUNITS)   = FAC_T_SUB

            ENDIF
          ENDDO
        ENDIF
      ENDIF
c
C---
      IF (IWRITE == 1) THEN
C---
c Reduction of work mass unit system string character
C---
        IREELM = 0
        READ(KEYM,ERR=100,FMT=FMT_F) UNITE
        IREELM = 1
100     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYM(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYM(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELM /= 1) THEN
          DO K=1,J
            KEYM(K:K) = KEYM(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
c Reduction of work length unit system string character
C---
        IREELL = 0
        READ(KEYL,ERR=200,FMT=FMT_F) UNITE
        IREELL = 1
200     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYL(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYL(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELL /= 1) THEN
          DO K=1,J
            KEYL(K:K) = KEYL(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
c Reduction of work time unit system string character
C---
        IREELT = 0
        READ(KEYT,ERR=300,FMT=FMT_F) UNITE
        IREELT = 1
300     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYT(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYT(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELT /= 1) THEN
          DO K=1,J
            KEYT(K:K) = KEYT(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
        IF ( IREELM == 1) THEN
            KEYM(1:3) = 'N.A'
        ENDIF

        IF ( IREELL == 1) THEN
            KEYL(1:3) = 'N.A'
        ENDIF

        IF ( IREELT == 1) THEN
            KEYT(1:3) = 'N.A'
        ENDIF

        WRITE(IOUT,1001) KEYM,KEYL,KEYT,FAC_MASS,FAC_LENGTH,FAC_TIME
C---
c Reduction of input mass unit system string character
C---
        IREELM = 0
        READ(KEYMI,ERR=700,FMT=FMT_F) UNITE
        IREELM = 1
700     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYMI(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYMI(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELM /= 1) THEN
          DO K=1,J
            KEYMI(K:K) = KEYMI(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
c Reduction of input length unit system string character
C---
        IREELL = 0
        READ(KEYLI,ERR=800,FMT=FMT_F) UNITE
        IREELL = 1
800     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYLI(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYLI(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELL /= 1) THEN
          DO K=1,J
            KEYLI(K:K) = KEYLI(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
c Reduction of input time unit system string character
C---
        IREELT = 0
        READ(KEYTI,ERR=900,FMT=FMT_F) UNITE
        IREELT = 1
900     CONTINUE
        I   = 1
        J   = 0
C       Skip leading spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYTI(I:I) /= ' ') EXIT
          I=I+1
        ENDDO
C       Read, skip trailing spaces
        DO WHILE (I <= 2*ncharfield)
          IF (KEYTI(I:I) == ' ') EXIT
          J=J+1
          I=I+1
        ENDDO
        IF ( IREELT /= 1) THEN
          DO K=1,J
            KEYTI(K:K) = KEYTI(K+I-J-1:K+I-J-1)
          ENDDO
        ENDIF
C---
        IF ( IREELM == 1) THEN
            KEYMI(1:3) = 'N.A'
        ENDIF

        IF ( IREELL == 1) THEN
            KEYLI(1:3) = 'N.A'
        ENDIF

        IF ( IREELT == 1) THEN
            KEYTI(1:3) = 'N.A'
        ENDIF

        WRITE(IOUT,1003) KEYMI,KEYLI,KEYTI,
     .                   FAC_M_INPUT,FAC_L_INPUT,FAC_T_INPUT
        DO I=2,NUNITS
c------------------------------------------------------------
          DO K=1,20
            FIELD1(K:K) = FIELD11(I-1)(K:K)
            FIELD2(K:K) = FIELD22(I-1)(K:K)
            FIELD3(K:K) = FIELD33(I-1)(K:K)
          ENDDO
C---
c Reduction of local mass unit system string character
C---
          IREELM = 0
          READ(FIELD1,ERR=400,FMT=FMT_F) UNITE
          IREELM = 1
400       CONTINUE
          I1   = 1
          J   = 0
C         Skip leading spaces
          DO WHILE (I1 <= 20)
            IF (FIELD1(I1:I1) /= ' ') EXIT
            I1=I1+1
          ENDDO
C         Read, skip trailing spaces
          DO WHILE (I1 <= 20)
            IF (FIELD1(I1:I1) == ' ') EXIT
            J=J+1
            I1=I1+1
          ENDDO
          IF ( IREELM /= 1) THEN
            DO K=1,J
              FIELD1(K:K) = FIELD1(K+I1-J-1:K+I1-J-1)
            ENDDO
          ENDIF
C---
c Reduction of local length unit system string character
C---
          IREELL = 0
          READ(FIELD2,ERR=500,FMT=FMT_F) UNITE
          IREELL = 1
500       CONTINUE
          I1   = 1
          J   = 0
C         Skip leading spaces
          DO WHILE (I1 <= 20)
            IF (FIELD2(I1:I1) /= ' ') EXIT
            I1=I1+1
          ENDDO
C         Read, skip trailing spaces
          DO WHILE (I1 <= 20)
            IF (FIELD2(I1:I1) == ' ') EXIT
            J=J+1
            I1=I1+1
          ENDDO
          IF ( IREELL /= 1) THEN
            DO K=1,J
              FIELD2(K:K) = FIELD2(K+I1-J-1:K+I1-J-1)
            ENDDO
          ENDIF
C---
c Reduction of local time unit system string character
C---
          IREELT = 0
          READ(FIELD3,ERR=600,FMT=FMT_F) UNITE
          IREELT = 1
600       CONTINUE
          I1   = 1
          J   = 0
C         Skip leading spaces
          DO WHILE (I1 <= 20)
            IF (FIELD3(I1:I1) /= ' ') EXIT
            I1=I1+1
          ENDDO
C         Read, skip trailing spaces
          DO WHILE (I1 <= 20)
            IF (FIELD3(I1:I1) == ' ') EXIT
            J=J+1
            I1=I1+1
          ENDDO
          IF ( IREELT /= 1) THEN
            DO K=1,J
              FIELD3(K:K) = FIELD3(K+I1-J-1:K+I1-J-1)
            ENDDO
          ENDIF
C---
          IF ( IREELM == 1) THEN
              FIELD1(1:3) = 'N.A'
          ENDIF

          IF ( IREELL == 1) THEN
              FIELD2(1:3) = 'N.A'
          ENDIF

          IF ( IREELT == 1) THEN
              FIELD3(1:3) = 'N.A'
          ENDIF

c-----------------------------------------------------------
          WRITE(IOUT,1002) UNITAB%UNIT_ID(I),FIELD1,FIELD2,FIELD3,
     .                     UNITAB%FAC_M(I),UNITAB%FAC_L(I),UNITAB%FAC_T(I)
        ENDDO
      ENDIF
C
C     transformation en valeurs relatives par rapport aux unites globales
C
      DO N=1,NUNITS
        IF (N >= 2) THEN

          IF (UNITAB%FAC_M(N) == ZERO) THEN
              UNITAB%FAC_M(N) = ONE
          ELSE
              UNITAB%FAC_M(N) = UNITAB%FAC_M(N) / UNITAB%FAC_M(1)
          ENDIF

          IF (UNITAB%FAC_L(N) == ZERO) THEN
              UNITAB%FAC_L(N) = ONE
          ELSE
              UNITAB%FAC_L(N) = UNITAB%FAC_L(N) / UNITAB%FAC_L(1)
          ENDIF

          IF (UNITAB%FAC_T(N) == ZERO) THEN
              UNITAB%FAC_T(N) = ONE
          ELSE
              UNITAB%FAC_T(N) = UNITAB%FAC_T(N) / UNITAB%FAC_T(1)
          ENDIF

        ENDIF
      ENDDO
      UNITAB%UNIT_ID(1) = ZERO
      UNITAB%FAC_M(1)   = FAC_M_INPUT / FAC_MASS
      UNITAB%FAC_L(1)   = FAC_L_INPUT / FAC_LENGTH
      UNITAB%FAC_T(1)   = FAC_T_INPUT / FAC_TIME
C-----
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
C-----
1000  FORMAT(
     . //,'    UNIT SYSTEMS DEFINITION     '/
     .    '    -----------------------     ',/
     ./ 58X,'MASS',16X,'LENGTH',14X,'TIME')
1001  FORMAT
     .(4X, 'WORK UNIT SYSTEM . . . . . . ','( ',A3,', ',A3,', ',A3,' )',
     . 1PE20.13,1PE20.13,1PE20.13)
1002  FORMAT
     .(4X, 'UNIT SYSTEM, ID = ',I10,' ','( ',A3,', ',A3,', ',A3,' )',
     . 1PE20.13,1PE20.13,1PE20.13)
1003  FORMAT
     .(4X, 'INPUT UNIT SYSTEM  . . . . . ','( ',A3,', ',A3,', ',A3,' )',
     . 1PE20.13,1PE20.13,1PE20.13)
      END

