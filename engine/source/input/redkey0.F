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
!||    redkey0                ../engine/source/input/redkey0.F
!||--- called by ------------------------------------------------------
!||    freform                ../engine/source/input/freform.F
!||--- calls      -----------------------------------------------------
!||    errmsg                 ../engine/source/input/errmsg.F
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE REDKEY0(KEY0,NKEY,CARTE,IKEY,NSLASH) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKEY, IKEY,NSLASH(*)
      CHARACTER(LEN=NCHARLINE100) :: CARTE
      CHARACTER(LEN=5) ::  KEY0(NKEY)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I
      CHARACTER(len=5) :: KEY
C-----------------------------------------------
      J=0
      KEY(1:5)='     '

      DO I=2,NCHARLINE100 
      IF(CARTE(I:I)=='/')EXIT
      IF(CARTE(I:I)==' ')CYCLE
       J=J+1
       IF(J>5)EXIT
       KEY(J:J)=CARTE(I:I)
      ENDDO
C
      DO I=1,NKEY
        IKEY=I
        IF(KEY(1:5)==KEY0(IKEY))THEN
          NSLASH(IKEY)=NSLASH(IKEY)+1
          EXIT
        ENDIF
      ENDDO
      IF(KEY(1:5)/=KEY0(IKEY))THEN
        IKEY=0
        CALL ERRMSG(KEY)
      ENDIF

C
      RETURN
      END
