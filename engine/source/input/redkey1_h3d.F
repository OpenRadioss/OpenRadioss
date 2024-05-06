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
Chd|  REDKEY1_H3D                   source/input/redkey1_h3d.F    
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REDKEY1_H3D(KEY,KEY0,NKEY,CARTE,IKEY,IR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100,NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKEY, IKEY, IR, IOK
      CHARACTER(LEN=NCHARLINE100) :: CARTE
      CHARACTER(LEN=NCHARKEY) :: KEY(*)
      CHARACTER KEY0(NKEY)*5
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEYO, J, JK, I
      CHARACTER(LEN=NCHARKEY) :: KEYO(9)
      LOGICAL DETECTED
C-----------------------------------------------
      IR=0
C
      IKEYO=IKEY
      DO J=1,9
        KEYO(J)=KEY(J)
        KEY(J)=' '
      ENDDO
      JK=0
C
      DO I=1,NCHARLINE100
        IF(CARTE(I:I)=='/')THEN
         JK=JK+1
         J=0
        ELSEIF(CARTE(I:I)/=' ')THEN
         J=J+1
         IF(J<=NCHARKEY)KEY(JK)(J:J)=CARTE(I:I)
        ENDIF
      ENDDO
C
      DETECTED = .FALSE.
      DO I=1,NKEY
        IKEY=I
        IF(KEY(1)(1:5)==KEY0(IKEY)(1:5)) THEN
          DETECTED = .TRUE.
          EXIT
        ENDIF
      ENDDO

      IF(.NOT. DETECTED)THEN
       IR=1
       IKEY=IKEYO
       DO J=1,9
         KEY(J)=KEYO(J)
       ENDDO

      ELSE
        IOK = 0
        J=0

      ENDIF

      RETURN
      END
