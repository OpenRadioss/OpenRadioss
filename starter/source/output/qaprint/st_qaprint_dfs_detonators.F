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
Chd|  ST_QAPRINT_DFS_DETONATORS     source/output/qaprint/st_qaprint_dfs_detonators.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_DFS_DETONATORS(DETONATORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
        USE DETONATORS_MOD
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
#include      "param_c.inc"
#include      "scr10_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(DETONATOR_STRUCT_), INTENT(IN) :: DETONATORS
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER K,J,DET_IID,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OK_QA = MYQAKEY('DETONATORS')

      NDETPS = DETONATORS%NDETPS
      NDETSG = DETONATORS%NDETSG
      NECRAN = DETONATORS%NECRAN
      NDETPL = DETONATORS%NDETPL
      NDETCORD = DETONATORS%NDETCORD

      IF (OK_QA) THEN
        I1       = 0
        I2       = NDETPS
        I3       = NDETPS+NDETSG
        I4       = NDETPS+NDETSG+NECRAN
        I5       = NDETPS+NDETSG+NECRAN+NDETPL
        I6       = NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD

        !/DFS/DETPOINT
        DO K=1+I1,I2
          DET_IID=K-I1
          DO J=1,5
            WRITE(VARNAME,'(A,I0,A,I1)') 'BURN_DFS_DETPOINT_',DET_IID ,'__POS_',J
            TEMP_DOUBLE = DETONATORS%BURN(J,K)
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
        ENDDO !next K

        !/DFS/DETLINE
        DO K=1+I2,I3
          DET_IID=K-I2
          DO J=1,8
            WRITE(VARNAME,'(A,I0,A,I1)') 'BURN_DFS_DETLINE_',DET_IID ,'__POS_',J
            TEMP_DOUBLE = DETONATORS%BURN(J,K)
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
        ENDDO !next K

        !/DFS/WAV_SHA
        DO K=1+I3,I4
          DET_IID=K-I3
          DO J=1,7
            WRITE(VARNAME,'(A,I0,A,I1)') 'BURN_DFS_WAVSHA_',DET_IID ,'__POS_',J
            TEMP_DOUBLE = DETONATORS%BURN(J,K)
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
        ENDDO !next K

        !/DFS/DETPLAN
        DO K=1+I4,I5
          DET_IID=K-I4
          DO J=1,8
            WRITE(VARNAME,'(A,I0,A,I1)') 'BURN_DFS_DETPLAN_',DET_IID ,'__POS_',J
            TEMP_DOUBLE = DETONATORS%BURN(J,K)
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
        ENDDO !next K

        !/DFS/DETCORD
        DO K=1+I5,I6
          DET_IID=K-I5
          DO J=1,7
            WRITE(VARNAME,'(A,I0,A,I1)') 'BURN_DFS_DETCORD_',DET_IID ,'__POS_',J
            TEMP_DOUBLE = DETONATORS%BURN(J,K)
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
        ENDDO !next K

      ENDIF
C-----------------------------------------------
        RETURN
      END

