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
      !||    build_msg              ../engine/source/output/message/build_msg.F
      !||--- called by ------------------------------------------------------
      !||    execargcheck           ../engine/source/engine/execargcheck.F
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||    fredec2i               ../engine/source/output/message/fredec2i.F
      !||    fredec3                ../engine/source/output/message/fredec3.F
      !||    stock_msg              ../engine/source/output/message/stock_msg.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE BUILD_MSG()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100) BUFMSG(100)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100) LINE,KEY,KEYST
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
      ITYPE=0
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
