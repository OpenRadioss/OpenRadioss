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
!||    fredec2i               ../engine/source/output/message/fredec2i.F
!||--- called by ------------------------------------------------------
!||    build_msg              ../common_source/tools/memory/my_alloc.F90
!||--- calls      -----------------------------------------------------
!||    arret                  ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE FREDEC2I(LINE,ID)
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
      CHARACTER(LEN=NCHARLINE100) :: LINE
      INTEGER ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100) MOT
      INTEGER I,J1,J2,J
      MOT=' '
      I=2
      DO WHILE(LINE(I:I)/='/')
        I=I+1
        IF(I>NCHARLINE100) GOTO 900
      ENDDO
      I=I+1
      IF(I>NCHARLINE100) GOTO 900
      J1=I
      DO WHILE(LINE(I:I)/='/')
        I=I+1
        IF(I>NCHARLINE100) GOTO 900
      ENDDO
      J2=I-1
C      
      MOT=LINE(J1:J2)
      READ(MOT,'(I10)',ERR=910)ID
      RETURN
C
C cannot use ANCMSG : used to build message structure
  900 CONTINUE
          WRITE(ISTDO,*)
     .   ' ** ERROR IN FREDEC2I WHILE GETTING KEY',
     .   ' ** LINE=',LINE
          WRITE(IOUT,*)
     .   ' ** ERROR IN FREDEC2I WHILE GETTING KEY',
     .   ' ** LINE=',LINE
          CALL ARRET(2)
      RETURN
  910 CONTINUE
          WRITE(ISTDO,*)
     .   ' ** ERROR IN FREDEC2I WHILE GETTING ID',
     .   ' ** LINE=',LINE,
     .   ' ** KEY=',MOT,' IS NOT AN INTEGER'
          WRITE(IOUT,*)
     .   ' ** ERROR IN FREDEC2I WHILE GETTING ID',
     .   ' ** LINE=',LINE,
     .   ' ** KEY=',MOT,' IS NOT AN INTEGER'
          CALL ARRET(2)
      RETURN
      END
