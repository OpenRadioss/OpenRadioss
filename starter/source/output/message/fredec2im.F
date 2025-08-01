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
!||    fredec2im              ../starter/source/output/message/fredec2im.F
!||--- called by ------------------------------------------------------
!||    build_msg              ../starter/source/output/message/build_msg.F
!||--- calls      -----------------------------------------------------
!||    arret                  ../starter/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE FREDEC2IM(LINE1,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE) :: LINE1
      INTEGER ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE) :: MOT
      INTEGER I,J1,J2,J
      MOT=' '
      I=2
      DO WHILE(LINE1(I:I)/='/')
        I=I+1
        IF(I>ncharline) GOTO 900
      ENDDO
      I=I+1
      IF(I>ncharline) GOTO 900
      J1=I
      DO WHILE(LINE1(I:I)/='/')
        I=I+1
        IF(I>ncharline) GOTO 900
      ENDDO
      J2=I-1
C
      MOT=LINE1(J1:J2)
      READ(MOT,'(I10)',ERR=910)ID
      RETURN
C
C cannot use ANCMSG : used to build message structure
  900 CONTINUE
          WRITE(ISTDO,*)
     .   ' ** ERROR IN FREDEC2IM WHILE GETTING KEY',
     .   ' ** LINE=',LINE1
          WRITE(IOUT,*)
     .   ' ** ERROR IN FREDEC2IM WHILE GETTING KEY',
     .   ' ** LINE=',LINE1
          CALL ARRET(2)
      RETURN
  910 CONTINUE
          WRITE(ISTDO,*)
     .   ' ** ERROR IN FREDEC2IM WHILE GETTING ID',
     .   ' ** LINE=',LINE1,
     .   ' ** KEY=',MOT,' IS NOT AN INTEGER'
          WRITE(IOUT,*)
     .   ' ** ERROR IN FREDEC2IM WHILE GETTING ID',
     .   ' ** LINE=',LINE1,
     .   ' ** KEY=',MOT,' IS NOT AN INTEGER'
          CALL ARRET(2)
      RETURN
      END
