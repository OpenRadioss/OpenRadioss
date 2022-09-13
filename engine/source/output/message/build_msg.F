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
Chd|  BUILD_MSG                     source/output/message/build_msg.F
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        FREDEC2I                      source/output/message/fredec2i.F
Chd|        FREDEC3                       source/output/message/fredec3.F
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
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*ncharline BUFMSG(100)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*ncharline LINE,KEY,KEYST
      INTEGER I,J,ID,ITYPE,INDX,SBUFMSG,ISTYPE
C predim
      SMESSAGES=1
      DO I=1,100
        BUFMSG(I)=' '
      END DO
      DO I=1,SMESSAGESFILE
C fill MESSAGES     
        LINE=MESSAGESFILE(I)
        IF (LINE(1:9)=='/MESSAGE/') THEN
          CALL FREDEC2I(LINE,ID)
          IF (ID>SMESSAGES) THEN
            SMESSAGES=ID
          END IF
        END IF
      END DO
      SMESSAGES=MIN(SMESSAGES,SMESSAGESMAX)
      ALLOCATE(MESSAGES(2,SMESSAGES))
      ID=0
      SBUFMSG=0
      DO I=1,SMESSAGESFILE
C remplir MESSAGES     
        LINE=MESSAGESFILE(I)
        IF (LINE(1:9)=='/MESSAGE/') THEN
          IF (ID>0.AND.ID<=SMESSAGESMAX.AND.ITYPE/=0) THEN
            CALL STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
          END IF
          SBUFMSG=0
          ID=1
          CALL FREDEC2I(LINE,ID)
          CALL FREDEC3(LINE,KEY)
          ITYPE=1
          IF (KEY(1:5)=='TITLE') THEN
            ITYPE=1
          ELSE IF (KEY(1:11)=='DESCRIPTION') THEN
            ITYPE=2
          END IF
        ELSE
          IF (ID>0.AND.LINE(1:1)/='#') THEN
            SBUFMSG=SBUFMSG+1
            BUFMSG(SBUFMSG)=LINE
          END IF
        END IF
      END DO
      IF (ID>0.AND.ID<=SMESSAGESMAX.AND.ITYPE/=0) THEN
        CALL STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
      END IF
      END SUBROUTINE
