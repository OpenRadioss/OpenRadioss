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
Chd|  ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        ENG_QAPRINT_ANIMINPUT         source/output/qaprint/eng_qaprint_animinput.F
Chd|        ENG_QAPRINT_DTINPUT           source/output/qaprint/eng_qaprint_dtinput.F
Chd|====================================================================
      SUBROUTINE ENG_QAPRINT_DRIVER(IRUNN_BIS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
c#include      "com01_c.inc"
c#include      "com06_c.inc"
c#include      "com08_c.inc"
c#include      "scr02_c.inc"
c#include      "scr11_c.inc"
c#include      "task_c.inc"
c#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUNN_BIS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================

      ! doqa is set in QA_OUT_MOD depending on DO_QA env variable setting
      ! we skip the specific QAPRINT treatment if doqa is != 1
      IF ( doqa /= 1  ) RETURN

      IF(NCYCLE == 0) THEN ! ECRITURE QAPRINT AU CYCLE 0 ENGINE
        CALL ENG_QAPRINT_GENERALCONTROLSINPUT(IRUNN_BIS)

        CALL ENG_QAPRINT_DTINPUT()

        CALL ENG_QAPRINT_ANIMINPUT()
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
