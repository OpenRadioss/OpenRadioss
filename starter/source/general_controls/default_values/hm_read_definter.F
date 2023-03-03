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
Chd|  HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_DEFINTER_TYPE02       source/general_controls/default_values/hm_read_definter_type02.F
Chd|        HM_READ_DEFINTER_TYPE07       source/general_controls/default_values/hm_read_definter_type07.F
Chd|        HM_READ_DEFINTER_TYPE11       source/general_controls/default_values/hm_read_definter_type11.F
Chd|        HM_READ_DEFINTER_TYPE19       source/general_controls/default_values/hm_read_definter_type19.F
Chd|        HM_READ_DEFINTER_TYPE24       source/general_controls/default_values/hm_read_definter_type24.F
Chd|        HM_READ_DEFINTER_TYPE25       source/general_controls/default_values/hm_read_definter_type25.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFINTER(HM_NINTER_DEF,DEF_INTER,LSUBMODEL)
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  HM_NINTER_DEF,
     .    DEF_INTER(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTYP , SUB_ID ,I
      CHARACTER KEY*ncharkey,KEY2*ncharkey
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
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       SUBMODEL_ID = SUB_ID,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

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
