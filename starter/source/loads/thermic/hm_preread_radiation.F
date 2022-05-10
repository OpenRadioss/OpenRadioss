Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  HM_PREREAD_RADIATION          source/loads/thermic/hm_preread_radiation.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_RADIATION(IGRSURF,IGRNOD,IGRBRIC,UNITAB,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
    
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NLD,IGU,IGS,ISU,ISS,NN,ID,UID,
     .        IFU,ISENS,NCUR,IGBR,IBID
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      CHARACTER TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN
       my_real
     .  FACX,FACY,H,FAC1,FAC2
C=======================================================================
      IS_AVAILABLE = .FALSE.
      NUMRADIA = 0
C--------------------------------------------------
C START BROWSING MODEL RADIATION
C--------------------------------------------------
      CALL HM_OPTION_START('/RADIATION')
C--------------------------------------------------
C BROWSING /RADIATION OPTIONS 1->NRADIA 
C--------------------------------------------------
      DO I=1,NRADIA
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /RADIATION/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('entityid',ISU,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
        IF (ISU > 0) THEN
          DO J=1,NSURF
            IF (ISU == IGRSURF(J)%ID) THEN
              ISS = J
              NUMRADIA = NUMRADIA + IGRSURF(ISS)%NSEG
              EXIT
            ENDIF                             
          ENDDO 
        ENDIF
      ENDDO 

      RETURN
      END SUBROUTINE HM_PREREAD_RADIATION 
