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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(DETONATORS_STRUCT_), INTENT(IN) :: DETONATORS
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER K,J,DET_IID
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER TEMP_INTEGER
      INTEGER :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OK_QA = MYQAKEY('DETONATORS')  

      NDETPS = DETONATORS%N_DET_POINT
      NDETSG = DETONATORS%N_DET_LINE
      NECRAN = DETONATORS%N_DET_WAVE_SHAPER
      NDETPL = DETONATORS%N_DET_PLANE
      NDETCORD = DETONATORS%N_DET_CORD
                                                         
      IF (OK_QA) THEN                                                                   

        !/DFS/DETPOINT                           
        DO K=1,NDETPS
            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPOINT_',K ,'__MAT_'
            TEMP_INTEGER = DETONATORS%POINT(K)%MAT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPOINT_',K ,'__TDET_'
            TEMP_DOUBLE = DETONATORS%POINT(K)%TDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)              

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPOINT_',K ,'__XDET_'
            TEMP_DOUBLE = DETONATORS%POINT(K)%XDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPOINT_',K ,'__YDET_'
            TEMP_DOUBLE = DETONATORS%POINT(K)%YDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPOINT_',K ,'__ZDET_'
            TEMP_DOUBLE = DETONATORS%POINT(K)%ZDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO !next K                                                                   

        !/DFS/DETLINE
        DO K=1,NDETSG
            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__MAT_'
            TEMP_INTEGER = DETONATORS%LINE(K)%MAT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__TDET_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%TDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__XDET_1_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%XDET_1
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__YDET_1_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%YDET_1
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__ZDET_1_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%ZDET_1
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__XDET_2_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%XDET_2
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__YDET_2_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%YDET_2
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETLINE_',K ,'__ZDET_2_'
            TEMP_DOUBLE = DETONATORS%LINE(K)%ZDET_2
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO !next K   

        !/DFS/WAV_SHA
        DO K=1,NECRAN
            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__MAT_'
            TEMP_INTEGER = DETONATORS%WAVE_SHAPER(K)%MAT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__NUMNOD_'
            TEMP_INTEGER = DETONATORS%WAVE_SHAPER(K)%NUMNOD
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__VDET_'
            TEMP_DOUBLE = DETONATORS%WAVE_SHAPER(K)%VDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__TDET_'
            TEMP_DOUBLE = DETONATORS%WAVE_SHAPER(K)%TDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__XDET_'
            TEMP_DOUBLE = DETONATORS%WAVE_SHAPER(K)%XDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__YDET_'
            TEMP_DOUBLE = DETONATORS%WAVE_SHAPER(K)%YDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_WAVSHA_',K ,'__ZDET_'
            TEMP_DOUBLE = DETONATORS%WAVE_SHAPER(K)%ZDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO !next K          

        !/DFS/DETPLAN
        DO K=1,NDETPL
            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__MAT_'
            TEMP_INTEGER = DETONATORS%PLANE(K)%MAT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__TDET_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%TDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__XDET_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%XDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__YDET_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%YDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__ZDET_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%ZDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__NX_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%NX
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__NY_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%NY
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETPLANE_',K ,'__NZ_'
            TEMP_DOUBLE = DETONATORS%PLANE(K)%NZ
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO !next K          

        !/DFS/DETCORD
        DO K=1,NDETCORD
            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETCORD_',K ,'__MAT_'
            TEMP_INTEGER = DETONATORS%CORD(K)%MAT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETCORD_',K ,'__IOPT_'
            TEMP_INTEGER = DETONATORS%CORD(K)%IOPT
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETCORD_',K ,'__NUMNOD_'
            TEMP_INTEGER = DETONATORS%CORD(K)%NUMNOD
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETCORD_',K ,'__TDET_'
            TEMP_DOUBLE = DETONATORS%CORD(K)%TDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

            WRITE(VARNAME,'(A,I0,A)') 'DFS_DETCORD_',K ,'__VDET_'
            TEMP_DOUBLE = DETONATORS%CORD(K)%VDET
            IF(TEMP_DOUBLE/=ZERO)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO !next K  
                
      ENDIF                                                                             
C-----------------------------------------------
        RETURN
      END

