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
Chd|  READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE READ_MSGFILE(LENI,INPUTR)
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
#include      "units_c.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENI
      CHARACTER*2096 INPUTR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*ncharline ERRMSGLINE
      CHARACTER*ncharline CTMP
      INTEGER ISTDEML
      INTEGER NBLINE,NBLINETOT,J1,J,ID,MAXID,MSGTYPE,
     .        IADT,IADD,IO_ERR
      
      ISTDEML=81
      OPEN(UNIT=ISTDEML,FORM='FORMATTED',FILE=INPUTR,
     .       STATUS='OLD',IOSTAT=IO_ERR)
      IF (IO_ERR/=0) THEN
C                     
        CALL ANCMSG(MSGID=72,ANMODE=ANINFO,
     .            C1=INPUTR)
        CLOSE(ISTDEML)
        CALL ARRET(2)
        RETURN
      ENDIF
C lecture du fichier ...
      NBLINE=0
      NBLINETOT=0
   10 READ(ISTDEML,'(A)',END=20,ERR=910) ERRMSGLINE
        NBLINETOT=NBLINETOT+1
        IF (LEN_TRIM(ERRMSGLINE)>0.AND.
     .      ERRMSGLINE(1:1)/='#') THEN
          NBLINE=NBLINE+1
        END IF 
      GOTO 10
   20 CONTINUE
      ALLOCATE(MESSAGESFILE(NBLINE))
      SMESSAGESFILE=NBLINE
      IF (NBLINE<=0) GOTO 900
      REWIND(ISTDEML)
      NBLINE=0
      NBLINETOT=0
   30 READ(ISTDEML,'(A)',END=40,ERR=910) ERRMSGLINE
        NBLINETOT=NBLINETOT+1
        IF (LEN_TRIM(ERRMSGLINE)>0.AND.
     .      ERRMSGLINE(1:1)/='#') THEN
          NBLINE=NBLINE+1
          MESSAGESFILE(NBLINE)=ERRMSGLINE
        END IF
      GOTO 30
   40 CONTINUE
      CLOSE(ISTDEML) 
      RETURN
  900 CONTINUE 
      CLOSE(ISTDEML)
      CALL ANCMSG(MSGID=1,ANMODE=ANINFO,C1=INPUTR)
      CALL ARRET(2)
      RETURN      
  910 CONTINUE 
      CLOSE(ISTDEML)
      CALL ANCMSG(MSGID=2,ANMODE=ANINFO,C1=INPUTR,I1=NBLINETOT)
      CALL ARRET(2)
      RETURN      
      END SUBROUTINE
