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
      !||    hm_preread_impvel0     ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_string          ../starter/source/devtools/hm_reader/hm_get_string.F
      !||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_PREREAD_IMPVEL0(IMPOSE_DR,UNITAB,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT):: IMPOSE_DR
      TYPE(UNIT_TYPE_) ,INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(*)       ,INTENT(IN) ::LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: I,OPTID,UID,NIMDISP
      CHARACTER(LEN=2) :: XX,YY,ZZ
      CHARACTER(LEN=NCHARFIELD) :: XYZ
      CHARACTER(LEN=NCHARKEY)  :: KEY
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA XX /'XX'/
      DATA YY /'YY'/
      DATA ZZ /'ZZ'/
C======================================================================|
      IS_AVAILABLE = .FALSE.
c      
      IMPOSE_DR    = 0
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/IMPDISP',NIMDISP)
c
c--------------------------------------------------
c
      CALL HM_OPTION_START('/IMPDISP')
c
c--------------------------------------------------
      DO I=1,NIMDISP
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          UNIT_ID     = UID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c
c--------------------------------------------------
c       CHECK IF ROTATIONAL DISPLACEMENT IS IMPOSED
c--------------------------------------------------
        IF (KEY(1:4) == 'FGEO') CYCLE  ! skip /IMPDISP/FGEO

        CALL HM_GET_STRING('rad_dir',XYZ,ncharfield,IS_AVAILABLE)
c
        IF (XYZ(1:2) == XX .OR. XYZ(1:2) == YY .OR. XYZ(1:2) == ZZ) THEN
          IMPOSE_DR = 1
        ENDIF
c
      ENDDO ! DO I=1,NIMDISP
c-----------
      RETURN
      END
