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
!||    read_ale_mat             ../starter/source/materials/ale/read_ale_mat.F
!||--- called by ------------------------------------------------------
!||    read_material_models     ../starter/source/materials/read_material_models.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_count          ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE READ_ALE_MAT(LSUBMODEL,UNITAB,IPM,PM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subroutine is reading keyword /ALE/MAT
C  and setting corresponding parameters in material buffer (PM array)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT)   :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT)   :: PM
      TYPE (UNIT_TYPE_) ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(NSUBMOD) , INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FLRD
      INTEGER II,KK,NB_ALEMAT,ILAW,ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER :: KEY*80
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       CALL HM_OPTION_COUNT('/ALE/MAT',NB_ALEMAT)
        CALL HM_OPTION_START('/ALE/MAT')
        
        DO KK=1,NB_ALEMAT
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID)
          CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
          CALL HM_GET_FLOATV('ALE_FLRD',FLRD,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          DO II=1,NUMMAT-1                     
            IF(IPM(1,II) == ID) THEN                   
              ILAW = IPM(2,II)                        
              IF(ILAW == 11)THEN                     
               IF(FLRD == ZERO) FLRD=ONE              
              ELSE                                   
               FLRD=MAX(ZERO,FLRD)                   
               FLRD=MIN(ONE,FLRD)                     
              ENDIF                                  
              PM(72,II) =ONEP1                         
              PM(92,II) =FLRD           
              WRITE(IOUT,2010)ID,FLRD                             
            ENDIF                                    
          ENDDO                                      
        ENDDO
        
 2010 FORMAT(
     & 5X,'    ALE MATERIAL  ',/,
     & 5X,'    ------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)      
        
      RETURN
      END SUBROUTINE READ_ALE_MAT
