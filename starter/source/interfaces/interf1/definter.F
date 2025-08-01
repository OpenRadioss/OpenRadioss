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
!||    definter                      ../starter/source/interfaces/interf1/definter.F
!||--- called by ------------------------------------------------------
!||    hm_read_definter_type02       ../starter/source/general_controls/default_values/hm_read_definter_type02.F
!||    hm_read_definter_type07       ../starter/source/general_controls/default_values/hm_read_definter_type07.F
!||    hm_read_definter_type11       ../starter/source/general_controls/default_values/hm_read_definter_type11.F
!||    hm_read_definter_type19       ../starter/source/general_controls/default_values/hm_read_definter_type19.F
!||    hm_read_definter_type24       ../starter/source/general_controls/default_values/hm_read_definter_type24.F
!||    hm_read_definter_type25       ../starter/source/general_controls/default_values/hm_read_definter_type25.F
!||    hm_read_inter_lagdt_type07    ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
!||    hm_read_inter_lagmul_type02   ../starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
!||    hm_read_inter_type02          ../starter/source/interfaces/int02/hm_read_inter_type02.F
!||    hm_read_inter_type07          ../starter/source/interfaces/int07/hm_read_inter_type07.F
!||    hm_read_inter_type11          ../starter/source/interfaces/int11/hm_read_inter_type11.F
!||    hm_read_inter_type24          ../starter/source/interfaces/int24/hm_read_inter_type24.F
!||    hm_read_inter_type25          ../starter/source/interfaces/int25/hm_read_inter_type25.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                        ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod                   ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE DEFINTER(KEY,IVAL,FLAG,IPRINT,
     .                    ITYP ,DEF_INTER)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE ALE_MOD
C----------------------------------------------- 
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------- 
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER KEY*10
      INTEGER IVAL,FLAG,IPRINT,ITYP,DEF_INTER(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDEX,DEF_DEF
      LOGICAL IS_DEFAUT_1000 
C-----------------------------------------------
c KEY      :  KEYWORD
c IVAL     :  INTEGER VALUE
c INDEX    :  INDEX IN DEF_INTER ARRAY
c DEF_DEF  :  DEFAULT VALUE IN CASE NO DEFAULT SPECIFIED
C FLAG     : 
c              = 0 : DEFINITION of DEFAULT VALUES
c              = 1 : USE DEFAULT VALUES 
c IPRINT   :
c              = 1 : USE JUST BEFORE PRINTING VALUES IN 0.out 
C                    IN ORDER TO REPLACE 0 BY CORRECT VALUES
C ITYP     :  TYPE OF THE INTERFACE
C DEF_INTER:  INTEGER ARRAY CONTAINING DEFAULT INTEGER VALUES
C=======================================================================
      INDEX = 0
      IS_DEFAUT_1000 = .TRUE.
      DEF_DEF = 0
      IF(ITYP == 2) THEN
        SELECT CASE (KEY)
C
          CASE ('IGNORE')
            INDEX = 1
            DEF_DEF = 1000
C
          CASE ('ILEV')
            INDEX = 2
            IF(ALE%GLOBAL%ICAA == 1) THEN
              DEF_DEF = 4
            ELSE
              DEF_DEF = 5
            ENDIF
C
          CASE ('IBUC')
            INDEX = 3
            DEF_DEF = 2
C
          CASE ('IDEL2')
            INDEX = 4
            DEF_DEF = 1000
C
          CASE ('IGSTI')
            INDEX = 5
            DEF_DEF = 2
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
C
      ELSEIF(ITYP == 7) THEN
        SELECT CASE (KEY)
          CASE ('IGSTI')
            INDEX = 10
            DEF_DEF = 1000
C
          CASE ('IGAP')
            INDEX = 11
            DEF_DEF = 1000
C
          CASE ('IBAG')
            INDEX = 12
            DEF_DEF = 1000
C
          CASE ('IDEL')
            INDEX = 13
            DEF_DEF = 1000
C
          CASE ('FLAGREMNOD')
            INDEX = 14
            DEF_DEF = 1
C
          CASE ('IREM7I2')
            INDEX = 15
            IF (IIMPLICIT>0)THEN
              DEF_DEF = 1
            ELSE
              DEF_DEF = 3
            ENDIF
C
          CASE ('INACTI')
            INDEX = 16
            DEF_DEF = 1000
C
          CASE ('MODFR')
            INDEX = 17
            DEF_DEF = 1
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
C
      ELSEIF(ITYP == 11) THEN
        SELECT CASE (KEY)
          CASE ('IGSTI')
            INDEX = 20
            DEF_DEF = 5
C
          CASE ('IGAP')
            INDEX = 21
            DEF_DEF = 1000
C
          CASE ('IDEL')
            INDEX = 22
            DEF_DEF = 1000
C
          CASE ('IFORM')
            INDEX = 23
            DEF_DEF = 1
C
          CASE ('INACTI')
            INDEX = 24
            DEF_DEF = 1000
C
          CASE ('FLAGREMNOD')
            INDEX = 25
            DEF_DEF = 1
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
C
      ELSEIF(ITYP == 19) THEN
        SELECT CASE (KEY)
          CASE ('IGSTI')
            INDEX = 30
c
            DEF_DEF = 1000
C
          CASE ('IGAP')
            INDEX = 31
            DEF_DEF = 1000
C
          CASE ('IBAG')
            INDEX = 32
            DEF_DEF = 1000
C
          CASE ('IDEL')
            INDEX = 33
            DEF_DEF = 1000
C
          CASE ('INACTI')
            INDEX = 34
            DEF_DEF = 1000
C
          CASE ('MODFR')
            INDEX = 36
            DEF_DEF = 1
C
          CASE ('IEDGE')
            INDEX = 37
            DEF_DEF = 2
C
          CASE ('FLAGREMNOD')
            INDEX = 38
            DEF_DEF = 1
C
          CASE ('IREM7I2')
            INDEX = 39
            IF (IIMPLICIT>0)THEN
              DEF_DEF = 1
            ELSE
              DEF_DEF = 3
            ENDIF
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
      ELSEIF(ITYP == 24) THEN
        SELECT CASE (KEY)
          CASE ('IGSTI')
            INDEX = 40
            DEF_DEF = 1000
C
          CASE ('IREM24I2')
            INDEX = 41
            DEF_DEF = 1
C
          CASE ('IDEL24')
            INDEX = 42
            DEF_DEF = 1000
C
          CASE ('IEDGE')
            INDEX = 43
            DEF_DEF = 1000
C
          CASE ('IGAP0')
            INDEX = 44
            DEF_DEF = 1000
C
          CASE ('IPEN0')
            INDEX = 45
            DEF_DEF = 1000
C
          CASE ('INACTI')
            INDEX = 46
            DEF_DEF = 1000
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
      ELSEIF(ITYP == 25) THEN
        SELECT CASE (KEY)
          CASE ('IGSTI')
            INDEX = 47
            DEF_DEF = 1000
C
          CASE ('IGAP')
            INDEX = 48
            DEF_DEF = 1
C
          CASE ('IBAG')
            INDEX = 49
            DEF_DEF = 1000
C
          CASE ('IDEL25')
            INDEX = 50
            DEF_DEF = 1000
C
          CASE ('IGAP0')
            INDEX = 51
            DEF_DEF = 1000
C
          CASE ('INACTI')
            INDEX = 52
            DEF_DEF = 1000
C
          CASE ('IREM25I2')
            INDEX = 53
            DEF_DEF = 1
C
          CASE ('ISHARP')
            INDEX = 54
            DEF_DEF = 1
C
          CASE ('IEDGE')
            INDEX = 55
            DEF_DEF = 1000
C
          CASE ('ITHK')
            INDEX = 56
            DEF_DEF = 1000
C
          CASE DEFAULT
            INDEX = 0
        END SELECT
        IF (DEF_DEF /= 1000) IS_DEFAUT_1000 = .FALSE.
      ENDIF
C
      IF(INDEX/=0)THEN
        IF(FLAG == 0 .AND. IPRINT == 0 
     .              .AND. DEF_INTER(INDEX) /= 0)THEN 
          CALL ANCMSG(MSGID=1073,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ITYP)
        ELSEIF(FLAG == 0 .AND. IPRINT == 0 
     .                   .AND. IVAL /= 0)THEN 
          ! put default value in DEF_INTER array
          DEF_INTER(INDEX) = IVAL
        ELSEIF(FLAG == 0 .AND. IPRINT == 0)THEN 
          ! put default_default value in DEF_INTER array
          DEF_INTER(INDEX) = DEF_DEF
        ELSEIF(FLAG == 0 .AND. IPRINT == 1)THEN 
          ! build IVAL value printed in 0.out for  /DEFAULT option
          IVAL = DEF_INTER(INDEX)
        ELSEIF(FLAG == 1 .AND. IPRINT == 0)THEN
          ! replace 0 by default value in /INTER
          IF(IVAL == 0 .AND. DEF_INTER(INDEX) /= 0)THEN
            IVAL = DEF_INTER( INDEX)
          ELSEIF(IVAL == 0)THEN
            IVAL = DEF_DEF
          ENDIF
          IF(IVAL == 1000 .AND. IS_DEFAUT_1000 ) IVAL = 0
        ELSEIF(FLAG == 1 .AND. IPRINT == 1)THEN
          IF(IVAL == 0 .AND. DEF_INTER(INDEX) /= 0)THEN
            IVAL = DEF_INTER(INDEX)
          ELSEIF(IVAL == 0 .AND. DEF_INTER(INDEX) == 0)THEN
            IVAL = DEF_DEF
          ENDIF
        ENDIF
      ENDIF!(INDEX/=0)
C-----
      RETURN
      END
