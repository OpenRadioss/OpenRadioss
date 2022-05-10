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
Chd|  HM_MESSAGES                   source/devtools/hm_reader/hm_messages.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HM_MESSAGES(IS_DYNA)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   COLLECT SYNTAX ERROR MESSAGES FOUND BY HM_READER
C   PRINT SYNTAX ERROR MESSAGES FOUND BY HM_READER
C   STOP STARTER + PRINT SUMMARY IN CASE SYNTAX ERROR DETECTED
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     NAME               DESCRIPTION                         
C
C     IS_DYNA            =1 : IT IS A LS-DYNA INPUT FORMAT 
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,NBMESSAGES,MSG_ID,MSG_TYPE,LINE_NB
      CHARACTER*ncharline FILE_NAME 
      CHARACTER*1000 MSG_DESCR,MSG_BLOCK,MSG_LINE
      INTEGER  IS_READER_KO,FILE_END
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=====================================================================
      WRITE(IOUT,'(A)')''
      WRITE(IOUT,'(A)')''
      WRITE(IOUT,'(A)')'************************************************************************'
      WRITE(IOUT,'(A)')'* INPUT READING PROCESS'
      WRITE(IOUT,'(A)')'************************************************************************'
      NBMESSAGES = 0
      IS_READER_KO = 0
C--------------------------------------------------
C BROWSE MESSAGES
C--------------------------------------------------
      SELECT CASE(IS_DYNA)
C--------------------------------------------------
        CASE (0)
C--------------------------------------------------
C RADIOSS INPUT FORMAT READING MESSAGES ( SYNTAX PB)
C--------------------------------------------------
C GET NUMBER OF MESSAGES
          CALL CPP_GET_MESSAGE_NUMBER_RADIOSS(NBMESSAGES)
          DO I=1,NBMESSAGES
            MSG_ID = 0
            MSG_TYPE = 0
            MSG_DESCR = ''
            FILE_NAME = ''
            MSG_BLOCK = ''
            MSG_LINE = ''
            CALL CPP_GET_MESSAGE_RADIOSS(I,MSG_ID,MSG_TYPE,MSG_DESCR,FILE_NAME,
     .                                   LINE_NB,MSG_BLOCK,MSG_LINE)
C OUTPUT MESSAGE IN RADIOSS 0.out FILE
            IF(MSG_ID .NE. 0)THEN
              IF(MSG_TYPE == 0)THEN
                CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
              ELSEIF(MSG_TYPE == 1)THEN
                IF (MSG_ID > 109000)THEN
                  FILE_END = 1
                  IF (MSG_DESCR(1:7) == 'At line')THEN
                    DO J=1,LEN_TRIM(MSG_DESCR)-5
                      IF (MSG_DESCR(J:J+4) == '_FULL') FILE_END = J+5
                    ENDDO
                  ENDIF
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(FILE_END:LEN_TRIM(MSG_DESCR)-1))
                ELSE
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ENDIF
              ELSEIF(MSG_TYPE == 2)THEN
                IS_READER_KO = IS_READER_KO + 1
                IF (MSG_ID > 109000)THEN
                  FILE_END = 1
                  IF (MSG_DESCR(1:7) == 'At line')THEN
                    DO J=1,LEN_TRIM(MSG_DESCR)-5
                      IF (MSG_DESCR(J:J+4) == '_FULL') FILE_END = J+5
                    ENDDO
                  ENDIF
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(FILE_END:LEN_TRIM(MSG_DESCR)-1))
                ELSE
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C--------------------------------------------------
        CASE (1)
C--------------------------------------------------
C LS-DYNA INPUT FORMAT READING MESSAGES ( SYNTAX PB)
C--------------------------------------------------
C GET NUMBER OF MESSAGES
          CALL CPP_GET_MESSAGE_NUMBER_DYNA(NBMESSAGES)
          DO I=1,NBMESSAGES
            MSG_ID = 0
            MSG_TYPE = 0
            MSG_DESCR = ''
            FILE_NAME = ''
            MSG_BLOCK = ''
            MSG_LINE = ''
            CALL CPP_GET_MESSAGE_DYNA(I,MSG_ID,MSG_TYPE,MSG_DESCR,FILE_NAME,
     .                                   LINE_NB,MSG_BLOCK,MSG_LINE)
C OUTPUT MESSAGE IN RADIOSS 0.out FILE
            IF(MSG_ID .NE. 0)THEN
              IF(MSG_TYPE == 0)THEN
                IF (MSG_ID > 200000)THEN
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO,
     .                C1=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ELSE
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ENDIF
              ELSEIF(MSG_TYPE == 1)THEN
                IF (MSG_ID > 200000)THEN
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                C1=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ELSE
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ENDIF
              ELSEIF(MSG_TYPE == 2)THEN
                IS_READER_KO = IS_READER_KO + 1
                IF (MSG_ID > 200000)THEN
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ELSE
                  CALL ANCMSG(MSGID=MSG_ID,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MSG_BLOCK(1:LEN_TRIM(MSG_BLOCK)-1),
     .                C2=MSG_LINE(1:LEN_TRIM(MSG_LINE)-1),
     .                C3=MSG_DESCR(1:LEN_TRIM(MSG_DESCR)-1))
                ENDIF
              ENDIF
            ENDIF
C--------------------------------------------------
          ENDDO
      END SELECT ! SELECT CASE(IS_DYNA)


      IF (IS_READER_KO .NE. 0) THEN
        CALL ANCMSG(MSGID=2000,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANSTOP,
     .            I1=IS_READER_KO)
      ELSE
        WRITE(IOUT,'(A)')''
        WRITE(IOUT,'(A)')' -NO SYNTAX ERROR DETECTED DURING INPUT READING PROCESS'
        WRITE(IOUT,'(A)')''
        WRITE(IOUT,'(A)')''
        WRITE(IOUT,'(A)')''
      ENDIF
      WRITE(IOUT,'(A)')''
      WRITE(IOUT,'(A)')'************************************************************************'
      WRITE(IOUT,'(A)')''
C-----------
      RETURN
      END SUBROUTINE HM_MESSAGES
