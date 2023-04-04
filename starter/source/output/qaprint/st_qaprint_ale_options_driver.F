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
Chd|  ST_QAPRINT_ALE_OPTIONS_DRIVER source/output/qaprint/st_qaprint_ale_options_driver.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_ALE_OPTIONS_DRIVER
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE ALEMUSCL_MOD , only : ALEMUSCL_Param
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TEMP_INT,II
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------

      IF (MYQAKEY('/ALE/MUSCL')) THEN
             
        WRITE(VARNAME,'(A)') 'BETA'
        TEMP_DOUBLE = ALEMUSCL_Param%BETA
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'IALEMUSCL'
        TEMP_INT = ALEMUSCL_Param%IALEMUSCL
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)

      ENDIF 
       
      
      IF (MYQAKEY('ALE')) THEN
            
       !------------------------------------------------------------------------
       ! ---ALE GRID
       ! my_real ALPHA
       ! my_real GAMMA
       ! my_real VGX
       ! my_real VGY
       ! my_real VGZ
       ! INTEGER NWALE_ENGINE       !        read from Engine input file with /ALE/GRID/...
       ! INTEGER NWALE_RST          !        read from RESTART FILE
       ! INTEGER NWALE              

        WRITE(VARNAME,'(A)') 'ALE%GRID%ALPHA'
        TEMP_DOUBLE = ALE%GRID%ALPHA
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%GRID%GAMMA'
        TEMP_DOUBLE = ALE%GRID%GAMMA
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%GRID%VGX'
        TEMP_DOUBLE = ALE%GRID%VGX
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%GRID%VGY'
        TEMP_DOUBLE = ALE%GRID%VGY
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%GRID%VGZ'
        TEMP_DOUBLE = ALE%GRID%VGZ
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)        

        WRITE(VARNAME,'(A)') 'ALE%GRID%NWALE_ENGINE'
        TEMP_INT = ALE%GRID%NWALE_ENGINE
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)

        WRITE(VARNAME,'(A)') 'ALE%GRID%NWALE_RST'
        TEMP_INT = ALE%GRID%NWALE_RST
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)

        WRITE(VARNAME,'(A)') 'ALE%GRID%NWALE'
        TEMP_INT = ALE%GRID%NWALE
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

       !------------------------------------------------------------------------
       ! ---ALE UPWIND
       ! my_real UPWMG
       ! my_real UPWOG           
       ! my_real UPWSM
       ! my_real CUPWM
       ! my_real UPWMG2
       ! my_real UPWOG2
       ! my_real UPWSM2
       ! INTEGER I_SUPG_ON_OFF
       ! INTEGER UPWM
       ! INTEGER UPW_UPDATE

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWMG'
        TEMP_DOUBLE = ALE%UPWIND%UPWMG
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWOG'
        TEMP_DOUBLE = ALE%UPWIND%UPWOG
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWSM'
        TEMP_DOUBLE = ALE%UPWIND%UPWSM
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%CUPWM'
        TEMP_DOUBLE = ALE%UPWIND%CUPWM
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWMG2'
        TEMP_DOUBLE = ALE%UPWIND%UPWMG2
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWOG2'
        TEMP_DOUBLE = ALE%UPWIND%UPWOG2
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWSM2'
        TEMP_DOUBLE = ALE%UPWIND%UPWSM2
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%I_SUPG_ON_OFF'
        TEMP_INT = ALE%UPWIND%I_SUPG_ON_OFF
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPWM'
        TEMP_INT = ALE%UPWIND%UPWM
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%UPWIND%UPW_UPDATE'
        TEMP_INT = ALE%UPWIND%UPW_UPDATE
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

       !------------------------------------------------------------------------
       ! ---ALE GLOBAL
       ! INTEGER NALENOVS
       ! INTEGER ICAA
       ! INTEGER INCOMP
       ! INTEGER ISFINT
       ! INTEGER I_DT_NODA_ALE_ON
       ! INTEGER I_DT_NODA_ALE_ON_KEY
       ! INTEGER IDT_ALE
       ! LOGICAL IS_BOUNDARY_MATERIAL
       ! INTEGER LCONV
       ! INTEGER CODV(10)
       ! INTEGER NVCONV
       ! INTEGER SNALE
       ! INTEGER SIELVS

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%NALENOVS'
        TEMP_INT = ALE%GLOBAL%NALENOVS
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%ICAA'
        TEMP_INT = ALE%GLOBAL%ICAA
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%INCOMP'
        TEMP_INT = ALE%GLOBAL%INCOMP
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%ISFINT'
        TEMP_INT = ALE%GLOBAL%ISFINT
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%I_DT_NODA_ALE_ON'
        TEMP_INT = ALE%GLOBAL%I_DT_NODA_ALE_ON
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%I_DT_NODA_ALE_ON_KEY'
        TEMP_INT = ALE%GLOBAL%I_DT_NODA_ALE_ON_KEY
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%IDT_ALE'
        TEMP_INT = ALE%GLOBAL%IDT_ALE
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%IS_BOUNDARY_MATERIAL'
        TEMP_INT = 0
        IF(ALE%GLOBAL%IS_BOUNDARY_MATERIAL)TEMP_INT = 1
        IF(TEMP_INT/=0) CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%LCONV'
        TEMP_INT = ALE%GLOBAL%LCONV
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        DO II=1,9
          WRITE(VARNAME,'(A)') 'ALE%GLOBAL%CODV- '
          WRITE(VARNAME(17:17),FMT='(I1)')II
          TEMP_INT = ALE%GLOBAL%CODV(II)
          IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)       
        ENDDO
        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%CODV-10'
        TEMP_INT = ALE%GLOBAL%CODV(10)
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
      

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%NVCONV'
        TEMP_INT = ALE%GLOBAL%NVCONV
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                

        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%SNALE'
        TEMP_INT = ALE%GLOBAL%SNALE
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  
        
        WRITE(VARNAME,'(A)') 'ALE%GLOBAL%SIELVS'
        TEMP_INT = ALE%GLOBAL%SIELVS
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  
          
          
       !------------------------------------------------------------------------
       ! ---ALE GLOBAL
       !   INTEGER IALESUB
       !   INTEGER IFSUB
       !   INTEGER IFSUBM
       !   INTEGER NODSUBDT
       !   my_real DT1SAVE
       !   my_real DTFSUB
       !   my_real DTMSUB

        WRITE(VARNAME,'(A)') 'ALE%SUB%IALESUB'
        TEMP_INT = ALE%SUB%IALESUB
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  

        WRITE(VARNAME,'(A)') 'ALE%SUB%IFSUB'
        TEMP_INT = ALE%SUB%IFSUB
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  

        WRITE(VARNAME,'(A)') 'ALE%SUB%IFSUBM'
        TEMP_INT = ALE%SUB%IFSUBM
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  

        WRITE(VARNAME,'(A)') 'ALE%SUB%NODSUBDT'
        TEMP_INT = ALE%SUB%NODSUBDT
        IF(TEMP_INT/=0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  

        WRITE(VARNAME,'(A)') 'ALE%SUB%DT1SAVE'
        TEMP_DOUBLE = ALE%SUB%DT1SAVE
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%SUB%DTFSUB'
        TEMP_DOUBLE = ALE%SUB%DTFSUB
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ALE%SUB%DTMSUB'
        TEMP_DOUBLE = ALE%SUB%DTMSUB
        IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          
                    
      ENDIF        
            
C-----------------------------------------------------------------------
      RETURN
      END

