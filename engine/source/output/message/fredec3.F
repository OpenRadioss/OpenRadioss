Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FREDEC3                       source/output/message/fredec3.F
Chd|-- called by -----------
Chd|        BUILD_MSG                     source/output/message/build_msg.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE FREDEC3(LINE,KEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100)::LINE,KEY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J1,J2,J
      KEY=' '
      I=2
      DO WHILE(LINE(I:I)/='/')
        I=I+1
        IF(I>NCHARLINE100) GOTO 900
      ENDDO
      I=I+1
      DO WHILE(LINE(I:I)/='/')
        I=I+1
        IF(I>NCHARLINE100) GOTO 900
      ENDDO
      I=I+1
      IF(I>NCHARLINE100) GOTO 900
      J1=I
      DO WHILE(LINE(I:I)/='/')
        I=I+1
        IF(I>NCHARLINE100) THEN
          J2=NCHARLINE100
          GOTO 10
        END IF
      ENDDO
      J2=I-1
   10 CONTINUE
C
      KEY=TRIM(LINE(J1:J2))
      RETURN
C
C cannot use ANCMSG : used to build message structure
  900 CONTINUE
          WRITE(ISTDO,*)
     .   ' ** ERROR IN FREDEC3 WHILE GETTING KEY',
     .   ' ** LINE=',LINE
          WRITE(IOUT,*)
     .   ' ** ERROR IN FREDEC3 WHILE GETTING KEY',
     .   ' ** LINE=',LINE
          CALL ARRET(2)
      RETURN
      END
