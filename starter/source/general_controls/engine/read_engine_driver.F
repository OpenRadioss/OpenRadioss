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
Chd|  READ_ENGINE_DRIVER            source/general_controls/engine/read_engine_driver.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE READ_ENGINE_DRIVER(IGRPART,IS_DYNA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_engcards_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
      INTEGER,INTENT(IN)::IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       CHARACTER FILNAM*512
       INTEGER I,IOUT_1RAD,LEN_FILNAM,CPT,NRUN
C=======================================================================
C WRITE /ANIM OPTIONS THAT ARE IN MEMORY(HM MODEL) IN 1.RAD FILE 
C=======================================================================
C-----------------------------------------------
C OPEN 1.rad FILE
C-----------------------------------------------
       FILNAM  =ROOTNAM(1:ROOTLEN)//'_0001.rad'
       LEN_FILNAM=ROOTLEN+9
       CALL CPP_MODEL_OPEN_FILE(FILNAM,LEN_FILNAM)
C--------------------------------------------------
C START BROWSING ANIM OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/ANIM')
C--------------------------------------------------
C WRITE ANIM OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NANIM_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING RUN OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/RUN')
C--------------------------------------------------
      CALL HM_OPTION_COUNT('/RUN',NRUN)
C--------------------------------------------------
C WRITE ANIM OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NRUN
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING TFILE OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/TFILE')
C--------------------------------------------------
C WRITE TFILE OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NTFILE_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING RFILE OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/RFILE')
C--------------------------------------------------
C WRITE RFILE OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NRFILE_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING STOP OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/STOP')
C--------------------------------------------------
C WRITE STOP OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NSTOP_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING DT OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/DT')
C--------------------------------------------------
C WRITE DT OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NDT_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C--------------------------------------------------
C START BROWSING VERS OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/VERS')
C--------------------------------------------------
C WRITE VERS OPTIONS IN 1.RAD FILE
C--------------------------------------------------
      DO CPT=1,NVERS_ENG
        CALL CPP_GLOBAL_ENTITY_SDI_WRITE(IS_DYNA)
      ENDDO
C-----------------------------------------------
C CLOSE 1.rad FILE
C-----------------------------------------------
      CALL CPP_MODEL_CLOSE_FILE()
C=======================================================================
C END WRITE /ANIM OPTIONS THAT ARE IN MEMORY(HM MODEL) IN 1.RAD FILE 
C=======================================================================
C-----------------------------------------------
       RETURN
       END
