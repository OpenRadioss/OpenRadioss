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
Chd|  READ_EULER_MAT                  source/materials/ale/read_euler_mat.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE READ_EULER_MAT(LSUBMODEL,UNITAB,IPM,PM ,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "alefvm.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: BUFLEN
      INTEGER,INTENT(IN) :: ID      
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(OUT)   :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(OUT)   :: PM
      TYPE (UNIT_TYPE_) ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(NSUBMOD) , INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: FLRD
      INTEGER II,KK,NB_EULERMAT,ILAW,IALEFVM_FLG
      CHARACTER TITR*nchartitle,KEY*80
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        CALL HM_OPTION_COUNT('/EULER/MAT',NB_EULERMAT)
        CALL HM_OPTION_START('/EULER/MAT')
        
        DO KK=1,NB_EULERMAT
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID)
          CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
          CALL HM_GET_FLOATV('EULER_FLRD',FLRD,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IALEFVM_FLG = 0
          DO II=1,NUMMAT-1                                
            IF(IPM(1,II) == ID)THEN                        
                ILAW = IPM(2,II)                          
              IF(ILAW == 11)THEN                         
                IF(FLRD == ZERO) FLRD=ONE                 
              ELSE                                       
                FLRD=MAX(ZERO,FLRD)                      
                FLRD=MIN(ONE,FLRD)                        
              ENDIF                                      
              PM(72,II)  = TWOP1                         
              PM(92,II)  = FLRD      
              IPM(251,II)= IALEFVM_FLG                    
              IF(IALEFVM_FLG>=2)THEN                   
                WRITE(IOUT,2021)ID,IALEFVM_FLG, FLRD      
                IALEFVM = 1                              
              ELSE                                       
                WRITE(IOUT,2020)ID,FLRD                   
                IALEFVM_FLG = 1                          
              ENDIF                                      
            ENDIF                                        
          ENDDO                                          
        ENDDO
        
 2020 FORMAT(
     & 5X,'    EULERIAN MATERIAL  ',/,
     & 5X,'    -----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)
 2021 FORMAT(
     & 5X,'    EULERIAN MATERIAL  ',/,
     & 5X,'    -----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'FINITE VOLUME METHOD FOR MOMENTUM . . .=',I10/,     
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//) 
             
      RETURN
      END SUBROUTINE READ_EULER_MAT
