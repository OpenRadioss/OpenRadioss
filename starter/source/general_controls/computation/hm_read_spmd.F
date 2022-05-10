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
Chd|  HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPMD(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SPMD USING HM_READER
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
      USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "scr12_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "commandline.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NB_SPMD,SUB_ID,BID
      LOGICAL IS_AVAILABLE
      INTEGER NTHREAD1
      CHARACTER (LEN=255) :: STR
C============================================================================
      IS_AVAILABLE = .FALSE.
C----------------------------------------------
      DECTYP=0
      NSPMD=0
      DECANI=0
      DECMOT=0
      DECNEQ=100
      NTHREAD=0
C
      CALL HM_OPTION_COUNT('/SPMD',NB_SPMD)
      CALL HM_OPTION_START('/SPMD')
      DO I=1,NB_SPMD
        CALL HM_OPTION_READ_KEY(LSUBMODEL, SUBMODEL_ID = SUB_ID)
        IF(SUB_ID == 0) THEN
          CALL HM_GET_INTV('DOMDEC',DECTYP,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SPMD',NSPMD,IS_AVAILABLE,LSUBMODEL)
C          CALL HM_GET_INTV('',DECANI,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('DECMOT',DECMOT,IS_AVAILABLE,LSUBMODEL)
C          CALL HM_GET_INTV('',DECNEQ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Nthread',NTHREAD,IS_AVAILABLE,LSUBMODEL)
          EXIT
        ENDIF
      ENDDO
C-------------------------------------
      RETURN
      END
