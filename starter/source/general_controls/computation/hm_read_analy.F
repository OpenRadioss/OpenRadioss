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
Chd|  HM_READ_ANALY                 source/general_controls/computation/hm_read_analy.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ANALY(NANALY,IPARITH,ISUB,IPARI0,MSHSUB,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   PREREAD /ANALY USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     NAME            DESCRIPTION    
C     NANALY          N2D3D INTEGER
C     IPARITH         FLAG FOR PARAL. ARITH.
C     ISUB            FLAG FOR SUBCYCLING
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER, INTENT(OUT)          ::NANALY,IPARITH,ISUB,IPARI0,MSHSUB
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_ANALY
      LOGICAL IS_AVAILABLE
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
      CALL HM_OPTION_COUNT('/ANALY',NB_ANALY)
      IF (NB_ANALY>0) THEN
        CALL HM_OPTION_START('/ANALY')
        CALL HM_OPTION_READ_KEY(LSUBMODEL)
        CALL HM_GET_INTV('N2D3D',NANALY,IS_AVAILABLE,LSUBMODEL)      
        CALL HM_GET_INTV('IPARITH',IPARITH,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_INTV('ISUBCYC',ISUB,IS_AVAILABLE,LSUBMODEL)
        IF(IPARITH==0)THEN
          IPARI0  = 1
          IPARITH = 1
        ELSEIF(IPARITH==1)THEN
          IPARI0 = 1
        ELSEIF(IPARITH==2)THEN
          IPARI0 = 0
        ELSE
          IPARI0=IPARITH-1
        ENDIF
      ELSE
        NANALY  = 0
        ISUB    = 0
        IPARITH = 1
      ENDIF
C
      IF(ISUB==2)THEN
        MSHSUB = 1
        ISUB   = 0
      END IF      
C--------------------------------------------------------------
      RETURN
      END 
