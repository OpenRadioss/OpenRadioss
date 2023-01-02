Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  NOLIB_USERMAT99               source/user_interface/nolib_usermat99.F
Chd|-- called by -----------
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        LAW_USERSH                    source/user_interface/law_usersh.F
Chd|====================================================================
      SUBROUTINE NOLIB_USERMAT99(ILAW_USER, IUSER_KEY)
C ----------------------------------------
      USE LAW_USERSH
#include      "nchara_c.inc"
C --------------------------------------------
      CHARACTER IUSER_KEY*ncharline
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
!
      IF(ILAW_USER == 1) THEn
!
        IUSER_KEY = 'USER01'
!
      ELSEIF(ILAW_USER == 2) THEN
!
        IUSER_KEY = 'USER02'
!
      ELSEIF(ILAW_USER == 3) THEN
!
        IUSER_KEY = 'USER03'
!
      ELSEIF(ILAW_USER == 4) THEN
!
        IUSER_KEY = 'USER04'
!
      ELSEIF(ILAW_USER == 5) THEN
!
        IUSER_KEY = 'USER05'
!
      ELSEIF(ILAW_USER == 6) THEN
!
        IUSER_KEY = 'USER06'
!
      ELSEIF(ILAW_USER == 7) THEN
!
        IUSER_KEY = 'USER07'
!
      ELSEIF(ILAW_USER == 8) THEN
!
        IUSER_KEY = 'USER08'
!
      ELSEIF(ILAW_USER == 9) THEN
!
        IUSER_KEY = 'USER09'
!
      ELSEIF(ILAW_USER == 10) THEN
!
        IUSER_KEY = 'USER10'
!
      ELSEIF(ILAW_USER == 11) THEN
!
        IUSER_KEY = 'USER11'
!
      ELSEIF(ILAW_USER == 12) THEN
!
        IUSER_KEY = 'USER12'
!
      ELSEIF(ILAW_USER == 13) THEN
!
        IUSER_KEY = 'USER13'
!
      ELSEIF(ILAW_USER == 14) THEN
!
        IUSER_KEY = 'USER14'
!
      ELSEIF(ILAW_USER == 15) THEN
!
        IUSER_KEY = 'USER15'
!
      ELSEIF(ILAW_USER == 16) THEN
!
        IUSER_KEY = 'USER16'
!
      ELSEIF(ILAW_USER == 17) THEN
!
        IUSER_KEY = 'USER17'
!
      ELSEIF(ILAW_USER == 18) THEN
!
        IUSER_KEY = 'USER18'
!
      ELSEIF(ILAW_USER == 19) THEN
!
        IUSER_KEY = 'USER19'
!
      ELSEIF(ILAW_USER == 20) THEN
!
        IUSER_KEY = 'USER20'
!
      ELSEIF(ILAW_USER == 21) THEn
!
        IUSER_KEY = 'USER21'
!
      ELSEIF(ILAW_USER == 22) THEN
!
        IUSER_KEY = 'USER22'
!
      ELSEIF(ILAW_USER == 23) THEN
!
        IUSER_KEY = 'USER23'
!
      ELSEIF(ILAW_USER == 24) THEN
!
        IUSER_KEY = 'USER24'
!
      ELSEIF(ILAW_USER == 25) THEN
!
        IUSER_KEY = 'USER25'
!
      ELSEIF(ILAW_USER == 26) THEN
!
        IUSER_KEY = 'USER26'
!
      ELSEIF(ILAW_USER == 27) THEN
!
        IUSER_KEY = 'USER27'
!
      ELSEIF(ILAW_USER == 28) THEN
!
        IUSER_KEY = 'USER28'
!
      ELSEIF(ILAW_USER == 29) THEN
!
        IUSER_KEY = 'USER29'
!
      ELSEIF(ILAW_USER == 30) THEn
!
        IUSER_KEY = 'USER30'
!
      ELSEIF(ILAW_USER == 31) THEN
!
        IUSER_KEY = 'USER31'
!
      ELSEIF(ILAW_USER == 32) THEN
!
        IUSER_KEY = 'USER32'
!
      ELSEIF(ILAW_USER == 33) THEN
!
        IUSER_KEY = 'USER33'
!
      ELSEIF(ILAW_USER == 34) THEN
!
        IUSER_KEY = 'USER34'
!
      ELSEIF(ILAW_USER == 35) THEN
!
        IUSER_KEY = 'USER35'
!
      ELSEIF(ILAW_USER == 36) THEN
!
        IUSER_KEY = 'USER36'
!
      ELSEIF(ILAW_USER == 37) THEN
!
        IUSER_KEY = 'USER37'
!
      ELSEIF(ILAW_USER == 38) THEN
!
        IUSER_KEY = 'USER38'
!
      ELSEIF(ILAW_USER == 39) THEN
!
        IUSER_KEY = 'USER39'
!
      ELSEIF(ILAW_USER == 40) THEN
!
        IUSER_KEY = 'USER40'
!
      ELSEIF(ILAW_USER == 41) THEN
!
        IUSER_KEY = 'USER41'
!
      ELSEIF(ILAW_USER == 42) THEN
!
        IUSER_KEY = 'USER42'
!
      ELSEIF(ILAW_USER == 43) THEN
!
        IUSER_KEY = 'USER43'
!
      ELSEIF(ILAW_USER == 44) THEN
!
        IUSER_KEY = 'USER44'
!
      ELSEIF(ILAW_USER == 45) THEN
!
        IUSER_KEY = 'USER45'
!
      ELSEIF(ILAW_USER == 46) THEN
!
        IUSER_KEY = 'USER46'
!
      ELSEIF(ILAW_USER == 47) THEN
!
        IUSER_KEY = 'USER47'
!
      ELSEIF(ILAW_USER == 48) THEN
!
        IUSER_KEY = 'USER48'
!
      ELSEIF(ILAW_USER == 49) THEN
!
        IUSER_KEY = 'USER49'
!
      ELSEIF(ILAW_USER == 50) THEN
!
        IUSER_KEY = 'USER50'
!
      ELSEIF(ILAW_USER == 51) THEn
!
        IUSER_KEY = 'USER51'
!
      ELSEIF(ILAW_USER == 52) THEN
!
        IUSER_KEY = 'USER52'
!
      ELSEIF(ILAW_USER == 53) THEN
!
        IUSER_KEY = 'USER53'
!
      ELSEIF(ILAW_USER == 54) THEN
!
        IUSER_KEY = 'USER54'
!
      ELSEIF(ILAW_USER == 55) THEN
!
        IUSER_KEY = 'USER55'
!
      ELSEIF(ILAW_USER == 56) THEN
!
        IUSER_KEY = 'USER56'
!
      ELSEIF(ILAW_USER == 57) THEN
!
        IUSER_KEY = 'USER57'
!
      ELSEIF(ILAW_USER == 58) THEN
!
        IUSER_KEY = 'USER59'
!
      ELSEIF(ILAW_USER == 59) THEN
!
        IUSER_KEY = 'USER60'
!
      ELSEIF(ILAW_USER == 60) THEn
!
        IUSER_KEY = 'USER60'
!
      ELSEIF(ILAW_USER == 61) THEN
!
        IUSER_KEY = 'USER61'
!
      ELSEIF(ILAW_USER == 62) THEN
!
        IUSER_KEY = 'USER62'
!
      ELSEIF(ILAW_USER == 63) THEN
!
        IUSER_KEY = 'USER63'
!
      ELSEIF(ILAW_USER == 64) THEN
!
        IUSER_KEY = 'USER64'
!
      ELSEIF(ILAW_USER == 65) THEN
!
        IUSER_KEY = 'USER65'
!
      ELSEIF(ILAW_USER == 66) THEN
!
        IUSER_KEY = 'USER66'
!
      ELSEIF(ILAW_USER == 67) THEN
!
        IUSER_KEY = 'USER67'
!
      ELSEIF(ILAW_USER == 68) THEN
!
        IUSER_KEY = 'USER68'
!
      ELSEIF(ILAW_USER == 69) THEN
!
        IUSER_KEY = 'USER69'
!
      ELSEIF(ILAW_USER == 70) THEN
!
        IUSER_KEY = 'USER70'
!
      ELSEIF(ILAW_USER == 71) THEN
!
        IUSER_KEY = 'USER71'
!
      ELSEIF(ILAW_USER == 72) THEN
!
        IUSER_KEY = 'USER72'
!
      ELSEIF(ILAW_USER == 73) THEN
!
        IUSER_KEY = 'USER73'
!
      ELSEIF(ILAW_USER == 74) THEN
!
        IUSER_KEY = 'USER74'
!
      ELSEIF(ILAW_USER == 75) THEN
!
        IUSER_KEY = 'USER75'
!
      ELSEIF(ILAW_USER == 76) THEN
!
        IUSER_KEY = 'USER76'
!
      ELSEIF(ILAW_USER == 77) THEN
!
        IUSER_KEY = 'USER77'
!
      ELSEIF(ILAW_USER == 78) THEN
!
        IUSER_KEY = 'USER78'
!
      ELSEIF(ILAW_USER == 79) THEN
!
        IUSER_KEY = 'USER79'
!
      ELSEIF(ILAW_USER == 80) THEN
!
        IUSER_KEY = 'USER80'
!
      ELSEIF(ILAW_USER == 81) THEN
!
        IUSER_KEY = 'USER81'
!
      ELSEIF(ILAW_USER == 82) THEN
!
        IUSER_KEY = 'USER82'
!
      ELSEIF(ILAW_USER == 83) THEN
!
        IUSER_KEY = 'USER83'
!
      ELSEIF(ILAW_USER == 84) THEN
!
        IUSER_KEY = 'USER84'
!
      ELSEIF(ILAW_USER == 85) THEN
!
        IUSER_KEY = 'USER85'
!
      ELSEIF(ILAW_USER == 86) THEN
!
        IUSER_KEY = 'USER86'
!
      ELSEIF(ILAW_USER == 87) THEN
!
        IUSER_KEY = 'USER87'
!
      ELSEIF(ILAW_USER == 88) THEN
!
        IUSER_KEY = 'USER88'
!
      ELSEIF(ILAW_USER == 89) THEN
!
        IUSER_KEY = 'USER89'
!
      ELSEIF(ILAW_USER == 90) THEN
!
        IUSER_KEY = 'USER90'
!
      ELSEIF(ILAW_USER == 91) THEN
!
        IUSER_KEY = 'USER91'
!
      ELSEIF(ILAW_USER == 92) THEN
!
        IUSER_KEY = 'USER92'
!
      ELSEIF(ILAW_USER == 93) THEN
!
        IUSER_KEY = 'USER93'
!
      ELSEIF(ILAW_USER == 94) THEN
!
        IUSER_KEY = 'USER94'
!
      ELSEIF(ILAW_USER == 95) THEN
!
        IUSER_KEY = 'USER95'
!
      ELSEIF(ILAW_USER == 96) THEN
!
        IUSER_KEY = 'USER96'
!
      ELSEIF(ILAW_USER == 97) THEN
!
        IUSER_KEY = 'USER97'
!
      ELSEIF(ILAW_USER == 98) THEN
!
        IUSER_KEY = 'USER98'
!
      ELSEIF(ILAW_USER == 99) THEN
!
        IUSER_KEY = 'USER99'
!
      ENDIF
!---
      RETURN
      END
