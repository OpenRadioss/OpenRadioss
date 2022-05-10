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
Chd|  ST_QAPRINT_COMPOSITE_OPTIONS  source/output/qaprint/st_qaprint_composite_options.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_COMPOSITE_OPTIONS(DRAPE)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (DRAPE_)    :: DRAPE(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,MY_ID,NPT,J,TEMP_INT
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
      IF (MYQAKEY('/DRAPE')) THEN
        DO I = 1,NUMELC + NUMELTG
          NPT = DRAPE(I)%NPT
          IF (NPT > 0) THEN
            IF (I <= NUMELC) THEN
              WRITE(VARNAME,'(A)') 'SHELL INTERNAL NUMBER'
              TEMP_INT = I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
            ELSE
              WRITE(VARNAME,'(A)') 'SH3N INTERNAL NUMBER'
              TEMP_INT = I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
            ENDIF
            DO II = 1, DRAPE(I)%NPT
              WRITE(VARNAME,'(A,I10)') 'IDRAPE',II
              TEMP_INT = DRAPE(I)%IDRAPE(II)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
              DO J = 1,2
                WRITE(VARNAME,'(A,I10,I10)') 'RDRAPE',II,J
                TEMP_DOUBLE = DRAPE(I)%RDRAPE(II,J)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
