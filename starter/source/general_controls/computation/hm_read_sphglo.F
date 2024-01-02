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
Chd|  HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV_WITHOUT_UID     source/devtools/hm_reader/hm_get_floatv_without_uid.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPHGLO(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SPHGLO USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     NAME            DESCRIPTION                         
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_SPHGLO,SUB_ID,BID
      LOGICAL IS_AVAILABLE
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
C     global parameters for SPH.
      SPASORT=FOURTH 
C
      IF (INVERS < 130) THEN
        IF(LVOISPH==0) LVOISPH=120
        IF(KVOISPH==0) KVOISPH=120  
      ELSE    
        IF(LVOISPH==0) LVOISPH=120
        IF(KVOISPH==0) KVOISPH=240
      ENDIF
      ITSOL2SPH=1
C
      CALL HM_OPTION_COUNT('/SPHGLO', NB_SPHGLO)
      CALL HM_OPTION_START('/SPHGLO')
      DO I=1,NB_SPHGLO
        CALL HM_OPTION_READ_KEY(LSUBMODEL, SUBMODEL_ID = SUB_ID)
        IF(SUB_ID == 0) THEN
          CALL HM_GET_FLOATV_WITHOUT_UID('ALPHA2',SPASORT,IS_AVAILABLE)
          CALL HM_GET_INTV('ALE_MAXSPH',BID,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('ALE_KVOISPH',LVOISPH,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('ALE_Form',KVOISPH,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SPHGLO_Isol2sph',ITSOL2SPH,IS_AVAILABLE,LSUBMODEL)
C         SPASORT=MIN(HALF,SPASORT)
          IF(SPASORT==ZERO) SPASORT=FOURTH
          IF(KVOISPH<120)KVOISPH=120
          IF(LVOISPH==0)LVOISPH=KVOISPH
          IF(ITSOL2SPH==0) ITSOL2SPH=1
          IF(KVOISPH<LVOISPH)KVOISPH=LVOISPH
          EXIT
        ENDIF
      ENDDO
      SPATRUE=SPASORT
C
C--------------------------------------------------------------
      RETURN
      END 
