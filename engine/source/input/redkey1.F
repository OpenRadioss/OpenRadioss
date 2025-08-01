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
!||    redkey1                ../engine/source/input/redkey1.F
!||--- called by ------------------------------------------------------
!||    freform                ../engine/source/input/freform.F
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE REDKEY1(KEY,KEY0,NKEY,CARTE,IKEY,IR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKEY, IKEY, IR, IOK
      CHARACTER(LEN=NCHARLINE100) ::  CARTE
      CHARACTER KEY(*)*5, KEY0(NKEY)*5
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEYO, J, JK, I
      CHARACTER KEYO(9)*5
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
      DO I=1,NCHARLINE100
        IF(CARTE(I:I)=='/')THEN
         JK=JK+1
         J=0
        ELSEIF(CARTE(I:I)/=' ')THEN
         J=J+1
         IF(J<=5)KEY(JK)(J:J)=CARTE(I:I)
        ENDIF
      ENDDO

      DETECTED=.FALSE.
      DO I=1,NKEY
        IKEY=I
        IF(KEY(1)==KEY0(IKEY))THEN
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

      ELSE ! IF(DETECTED)
        IOK = 0
        J=0
        IF(IKEY==1.OR.IKEY==58)THEN
         IF (GOT_INPUT == 0.OR.IKEY==58) THEN
          DO I=5,NCHARLINE100
            IF(IOK==0.AND.CARTE(I:I)=='/')THEN
             IOK = 1
            ELSEIF(IOK==1.AND.CARTE(I:I)=='/')THEN
             ROOTLEN = J
             RETURN
            ELSEIF(IOK==1.AND.CARTE(I:I)/=' ')THEN
             J=J+1
             IF(J<=80)ROOTNAM(J:J)=CARTE(I:I)
            ENDIF
          ENDDO
         ELSE
          ROOTNAM(1:LENROOTN)=ROOTN(1:LENROOTN)
          ROOTLEN = LENROOTN
         ENDIF
        ENDIF
      ENDIF

      RETURN
      END
