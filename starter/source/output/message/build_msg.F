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
Chd|  BUILD_MSG                     source/output/message/build_msg.F
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        FREDEC2IM                     source/output/message/fredec2im.F
Chd|        FREDEC3M                      source/output/message/fredec3m.F
Chd|        STOCK_MSG                     source/output/message/stock_msg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BUILD_MSG()
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*ncharline BUFMSG(100)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*ncharline LINE1,KEY,KEYST
      INTEGER I,J,ID,ITYPE,INDX,SBUFMSG,ISTYPE
C predim
      SMESSAGES=1
      DO I=1,100
        BUFMSG(I)=' '
      END DO
      DO I=1,SMESSAGESFILE
C fill MESSAGES
        LINE1=MESSAGESFILE(I)
        IF (LINE1(1:9)=='/MESSAGE/') THEN
          CALL FREDEC2IM(LINE1,ID)
          IF (ID>SMESSAGES) THEN
            SMESSAGES=ID
          END IF
        END IF
      END DO
      SMESSAGES=MIN(SMESSAGES,SMESSAGESMAX)
      ALLOCATE(MESSAGES(SMSGTYPE,SMESSAGES))
      DO I=1,SMESSAGESFILE
        DO J=1,SMSGTYPE
          MESSAGES(J,I)%SMESSAGE=0
        END DO
      END DO
      ID=0
      SBUFMSG=0
      ITYPE=0
      DO I=1,SMESSAGESFILE
C remplir MESSAGES
        LINE1=MESSAGESFILE(I)
        IF (LINE1(1:9)=='/MESSAGE/') THEN
          IF (ID>0.AND.ID<=SMESSAGESMAX.AND.ITYPE/=0) THEN
            CALL STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
          END IF
          SBUFMSG=0
          ID=1
          CALL FREDEC2IM(LINE1,ID)
          CALL FREDEC3M(LINE1,KEY)
          ITYPE=1
          IF (KEY(1:5)=='TITLE') THEN
            ITYPE=1
          ELSE IF (KEY(1:11)=='DESCRIPTION') THEN
            ITYPE=2
          ELSE IF (KEY(1:8)=='SOLUTION') THEN
            ITYPE=3
          ELSE IF (KEY(1:4)=='DATA') THEN
            ITYPE=4
          END IF
        ELSE
          IF (ID>0.AND.LINE1(1:1)/='#') THEN
            SBUFMSG=SBUFMSG+1
            BUFMSG(SBUFMSG)=LINE1
          END IF
        END IF
      END DO
      IF (ID>0.AND.ID<=SMESSAGESMAX.AND.ITYPE/=0) THEN
        CALL STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
      END IF
      RETURN
      END SUBROUTINE
