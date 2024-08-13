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
      !||====================================================================
      !||    hm_read_definter          ../starter/source/general_controls/default_values/hm_read_definter.F
      !||--- called by ------------------------------------------------------
      !||    contrl                    ../starter/source/starter/contrl.F
      !||--- calls      -----------------------------------------------------
      !||    hm_option_read_key        ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start           ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    hm_read_definter_type02   ../starter/source/general_controls/default_values/hm_read_definter_type02.F
      !||    hm_read_definter_type07   ../starter/source/general_controls/default_values/hm_read_definter_type07.F
      !||    hm_read_definter_type11   ../starter/source/general_controls/default_values/hm_read_definter_type11.F
      !||    hm_read_definter_type19   ../starter/source/general_controls/default_values/hm_read_definter_type19.F
      !||    hm_read_definter_type24   ../starter/source/general_controls/default_values/hm_read_definter_type24.F
      !||    hm_read_definter_type25   ../starter/source/general_controls/default_values/hm_read_definter_type25.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod        ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod               ../starter/share/message_module/message_mod.F
      !||    submodel_mod              ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_DEFINTER(HM_NINTER_DEF,DEF_INTER,LSUBMODEL)
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY
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
      INTEGER  HM_NINTER_DEF,DEF_INTER(100)
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTYP , SUB_ID ,I
      CHARACTER(LEN=NCHARKEY) :: KEY,KEY2
C============================================================================
C       READING INTERFACES DEFAULT VALUES
C============================================================================

      IF(HM_NINTER_DEF /= 0) WRITE(IOUT,5900)

C--------------------------------------------------
C START BROWSING MODEL INTERFACES
C--------------------------------------------------
      CALL HM_OPTION_START('/DEFAULT/INTER')

C--------------------------------------------------
C BROWSING MODEL INTERFACES 1->HM_NINTER_DEF
C--------------------------------------------------
      DO I=1,HM_NINTER_DEF

C--------------------------------------------------
C EXTRACT DATAS OF /DEFAULT/INTER
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,SUBMODEL_ID = SUB_ID,KEYWORD2 = KEY,KEYWORD3 = KEY2)

c Default values definition

        IF(SUB_ID == 0) THEN

         IF(KEY2(1:LEN_TRIM(KEY2))=='TYPE2')THEN
           NTYP = 2
           CALL HM_READ_DEFINTER_TYPE02(NTYP,DEF_INTER,LSUBMODEL)

         ELSEIF(KEY2(1:LEN_TRIM(KEY2))=='TYPE7')THEN
           NTYP = 7
           CALL HM_READ_DEFINTER_TYPE07(NTYP,DEF_INTER,LSUBMODEL)

         ELSEIF(KEY2(1:LEN_TRIM(KEY2))=='TYPE11')THEN
           NTYP=11
           CALL HM_READ_DEFINTER_TYPE11(NTYP,DEF_INTER,LSUBMODEL)

         ELSEIF(KEY2(1:LEN_TRIM(KEY2))=='TYPE19')THEN
           NTYP=19
           CALL HM_READ_DEFINTER_TYPE19(NTYP,DEF_INTER,LSUBMODEL)

         ELSEIF(KEY2(1:LEN_TRIM(KEY2))=='TYPE24')THEN
           NTYP=24
           CALL HM_READ_DEFINTER_TYPE24(NTYP,DEF_INTER,LSUBMODEL)

         ELSEIF(KEY2(1:LEN_TRIM(KEY2))=='TYPE25')THEN
           NTYP=25
           CALL HM_READ_DEFINTER_TYPE25(NTYP,DEF_INTER,LSUBMODEL)

         ENDIF
C
        ENDIF
       ENDDO
C--------------------------------------------------------------
      RETURN
C--------------------------------------------------------------
C-----
 5900 FORMAT(//4X,'INTERFACES DEFAULT VALUES DEFINITION'/
     &         4X,'------------------------------------'/)
C--------------------------------------------------------------
      END 
