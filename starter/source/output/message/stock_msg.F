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
!||    stock_msg              ../starter/source/output/message/stock_msg.F
!||--- called by ------------------------------------------------------
!||    build_msg              ../starter/source/output/message/build_msg.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ID,ITYPE,SBUFMSG
      CHARACTER(LEN=NCHARLINE), INTENT(IN) :: BUFMSG(SBUFMSG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JDX,IBACKSLASH
      CHARACTER*1, PARAMETER :: BACKSLASH = CHAR(92)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (ALLOCATED(MESSAGES(ITYPE,ID)%MESSAGE)) THEN
        DEALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE)
      END IF
      IF (SBUFMSG==0) THEN
        MESSAGES(ITYPE,ID)%SMESSAGE=1
        ALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE(1))
        MESSAGES(ITYPE,ID)%MESSAGE(1)=' '
      ELSE
        ALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE(SBUFMSG))
        MESSAGES(ITYPE,ID)%SMESSAGE=SBUFMSG
        DO I=1,SBUFMSG
          JDX=1
          MESSAGES(ITYPE,ID)%MESSAGE(I)=' '
          J=1
          DO WHILE(J<=ncharline)
            IF (BUFMSG(I)(J:J)==BACKSLASH) THEN
C             => skip \ print next
              IF (J<ncharline) THEN
                J=J+1
                IF (BUFMSG(I)(J:J)=='n') THEN
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=CHAR(10)
                  JDX=JDX+1
C add here other special cases
                ELSE
C                 keep back slash for further use
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=BUFMSG(I)(J-1:J-1)
                  JDX=JDX+1
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=BUFMSG(I)(J:J)
                  JDX=JDX+1
                END IF
              ELSE
C               => end of loop
                J=J+1
              END IF
            ELSE
              MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=BUFMSG(I)(J:J)
              JDX=JDX+1
            END IF
            J=J+1
          END DO
        END DO
      END IF
      RETURN
      END SUBROUTINE
C-----------------------------------------------
