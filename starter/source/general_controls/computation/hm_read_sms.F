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
Chd|  HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        FIND_YIELD_ENGINE             source/general_controls/computation/hm_read_sms.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SMS(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /AMS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     NAME            DESCRIPTION                         
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_AMS,SUB_ID
      LOGICAL IS_AVAILABLE
      CHARACTER MESS*40
      DATA MESS /'SETTINGS FOR ADVANCED MASS SCALING      '/
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
C     ISMS_SELEC = 0 --> No AMS
C     ISMS_SELEC = 1 --> Full AMS
C     ISMS_SELEC = 2 --> AMS by parts
C     ISMS_SELEC = 3 --> AMS auto
C     ISMS_SELEC = 4 --> AMS auto + parts

      ISMS=0
      ISMS_SELEC = 0
C
      CALL HM_OPTION_COUNT('/AMS', NB_AMS)
      CALL HM_OPTION_START('/AMS')
      DO I=1,NB_AMS
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       SUBMODEL_ID = SUB_ID)
        ISMS=1
        CALL HM_GET_INTV('GRPART_ID',IDTGRS,IS_AVAILABLE,LSUBMODEL)
      ENDDO
C
      IF(ISMS /= 0)THEN
C
        WRITE(IOUT,2000)
C
C       Engine file is checked for automatic element selection activation
        DT_SMS_SWITCH = ZERO
        CALL FIND_YIELD_ENGINE(DT_SMS_SWITCH)
C
        IF (DT_SMS_SWITCH==ZERO) THEN
          IF (IDTGRS==0) THEN
            WRITE(IOUT,2100)
            ISMS_SELEC = 1
          ELSE
            WRITE(IOUT,2200) IDTGRS
            ISMS_SELEC = 2
          ENDIF
        ELSE
          IF (IDTGRS==0) THEN
            ISMS_SELEC = 3
          ELSE
            WRITE(IOUT,2200) IDTGRS
            ISMS_SELEC = 4
          ENDIF
          WRITE(IOUT,2300) DT_SMS_SWITCH    
        ENDIF
C
        IDTGRS = -IDTGRS
        IREST_MSELT=1
      END IF                            
C--------------------------------------------------
 2000 FORMAT(//
     .  '    ADVANCED MASS SCALING:                           ',/
     .  '    ---------------------                            ')
 2100 FORMAT(/
     .  '    SELECTIVE MASS SCALING APPLIED TO ALL PARTS')
 2200 FORMAT(/
     .  '    ADVANCED MASS SCALING APPLIED ON GROUP OF PARTS . . . .',I10)
 2300 FORMAT(/
     .  '    AUTOMATIC ELEMENT SELECTION ENABLED (/DT/CST_AMS DETECTED IN ENGINE INPUT FILE)',/
     .  '    TIME STEP CRITERIA FOR AMS ACTIVATION. . . . . .  . . .',1PG20.13)
C--------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  FIND_YIELD_ENGINE             source/general_controls/computation/hm_read_sms.F
Chd|-- called by -----------
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FIND_YIELD_ENGINE(DT_SMS_SWITCH)
      USE INOUTFILE_MOD
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
#include      "scr15_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   DT_SMS_SWITCH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IO_ERR1
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80
      my_real
     .   DTSCA,DTMINI
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------

C-----------------------------------------------
C     Lecture des donnes dans le fichier engine
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'

      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1/=0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+3)
         OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF      

      IF (IO_ERR1==0) THEN
C
 10     READ(71,'(A)',END=20) KEYA
C
        IF(KEYA(1:11)=='/DT/CST_AMS') THEN
 30       READ(71,'(A)',END=20) KEYA
          IF ((KEYA(1:1)=='#').OR.(KEYA(1:1)=='$')) THEN
            GOTO 30
          ELSE
            BACKSPACE(71)
          ENDIF
          READ(71,*,END=20) DTSCA,DTMINI
          IF (DTSCA == ZERO) DTSCA = ZEP9
          DT_SMS_SWITCH = DTMINI / DTSCA      
        ENDIF
C
        GOTO 10
C
 20     CONTINUE

        CLOSE(71)
C
      ELSE
C
        CALL ANCMSG(MSGID=1570,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              C1=ROOTNAM(1:ROOTLEN)//'_0001.rad',
     .              C2=ROOTNAM(1:ROOTLEN)//'D01')   
C
      ENDIF
                   
C------------------------------------------- 
      RETURN
      END
Chd|====================================================================
Chd|  INISMS                        source/general_controls/computation/hm_read_sms.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INISMS(
     .       IGRPART  ,IPARTS     ,IPARTQ   ,IPARTC  ,
     .       IPARTT   ,IPARTP     ,IPARTR   ,IPARTTG ,
     .       IPARTX   ,TAGPRT_SMS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),
     .   IPARTP(*),IPARTR(*),IPARTTG(*),IPARTX(*),
     .   TAGPRT_SMS(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART)  :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, N, I, IAD, IP, 
     .        NG, J, ITY, NEL, NFT
C--------------------------------------------------
       IF(IDTGRS/=0)THEN
C------
        IF(IDTGRS < 0)THEN
          IDTGRS=-IDTGRS

          KK=NGRNOD+
     +          NGRBRIC+NGRQUAD+NGRSHEL+NGRSH3N+NGRTRUS+NGRBEAM+NGRSPRI
          DO N=1,NGRPART
            IF (IGRPART(N)%ID == IDTGRS) THEN
              IDTGRS=N
              GO TO 100
            END IF
          END DO
 200      CALL ANCMSG(MSGID=738,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=IDTGRS)
 100      CONTINUE
          DO I=1,IGRPART(IDTGRS)%NENTITY
            IP=IGRPART(IDTGRS)%ENTITY(I)
            TAGPRT_SMS(IP)=1
          END DO       
        END IF
C------
      ELSEIF (ISMS_SELEC/=3) THEN
C------
        DO IP=1,NPART
          TAGPRT_SMS(IP)=1
        END DO
C
      END IF
C--------------------------------------------------
      RETURN
      END
