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
Chd|  PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        GRSIZEN                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PREREAD_RBODY_LAGMUL(SLPBYL, IGRNOD, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SLPBYL
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,IGU,NN,NM,NRB
      CHARACTER TITR*nchartitle,KEY*ncharkey
      LOGICAL   IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER GRSIZEN
C=====================================================================
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBODY')
C---
      SLPBYL= 0
      NRB = 0
C---
      DO I=1,NRBODY
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
        KEY  = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY )
C--------------------------------------------------
       IF(KEY(1:6)=='LAGMUL')THEN
        NRB=NRB+1
C----------Multidomaines --> on ignore les rby tagés---------------	
        IF(NSUBDOM.GT.0)THEN
	  IF(TAGRBY(NRB).EQ.0)CALL HM_SZ_R2R(TAGRBY,NRB,LSUBMODEL)
	END IF
C--------
         CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
         NN = GRSIZEN(IGU,IGRNOD,NGRNOD) 
         NM = NN+1                
         SLPBYL = SLPBYL + 3*NM                    
       END IF            
      ENDDO
C-----------
      RETURN
      END SUBROUTINE PREREAD_RBODY_LAGMUL
