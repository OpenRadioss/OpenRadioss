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
!||    check_qeph_stra        ../starter/source/starter/check_qeph_stra.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE CHECK_QEPH_STRA(ISTR_24)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INOUTFILE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
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
      INTEGER , INTENT(INOUT) :: ISTR_24
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IO_ERR1,IFUND,LEN_TMP_NAME
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80
      CHARACTER(LEN=NCHARLINE) :: CARTE
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
      IFUND=0
      IO_ERR1 = 0
C-----------------------------------------------
C     READING ENGINE FILE
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'

      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)

      DO WHILE (IO_ERR1 == 0 .AND. IFUND==0)
        READ(71,FMT='(A)',iostat=IO_ERR1) KEYA
        IF(IO_ERR1 == 0)THEN
          IF (KEYA(1:25)=='/DYNAIN/SHELL/STRAIN/FULL') THEN
            IFUND = 1
          ELSEIF (KEYA(1:25)=='/STATE/SHELL/STRAIN/GLOBF') THEN
            IFUND = 1
          END IF
        ENDIF
      END DO

      CLOSE(71)
      IF (IFUND==1) ISTR_24 = 1

      RETURN
      END
