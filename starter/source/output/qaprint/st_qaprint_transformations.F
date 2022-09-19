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
Chd|  ST_QAPRINT_TRANSFORMATIONS    source/output/qaprint/st_qaprint_transformations.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_TRANSFORMATIONS(RTRANS,LSUBMODEL,IGRNOD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tabsiz_c.inc"
#include      "thermal_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) ::
     .                        RTRANS(NTRANSF,NRTRANS)
      TYPE(SUBMODEL_DATA), INTENT(IN) :: LSUBMODEL(*)
      TYPE (GROUP_)  ,TARGET,  INTENT(IN), DIMENSION(NGRNOD)  :: IGRNOD
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_TRANS, TEMP_INT, ITRANSSUB, IGS, IS_INTEGER_RTRANS(NRTRANS)
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     /TRANSFORM
C-----------------------------------------------
      IS_INTEGER_RTRANS(1:NRTRANS)=0
C
      IF (MYQAKEY('/TRANSFORM')) THEN
        DO MY_TRANS=1,NTRANSF
C
          TITR(1:nchartitle)=''
          MY_ID = NINT(RTRANS(MY_TRANS,19))
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_TRANSFORMATION_FAKE_NAME', MY_ID,0.0_8)
          END IF
          IS_INTEGER_RTRANS(19)=1
C
          WRITE(VARNAME,'(A)') 'Transformation_Type'
          TEMP_INT = NINT(RTRANS(MY_TRANS,2))
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
          IS_INTEGER_RTRANS(2)=1
C
          ITRANSSUB=NINT(RTRANS(MY_TRANS,1))
          IF(ITRANSSUB/=0)THEN
            ITRANSSUB=LSUBMODEL(ITRANSSUB)%NOSUBMOD
            WRITE(VARNAME,'(A)') 'Submodel_ID'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITRANSSUB,0.0_8)
          END IF
          IS_INTEGER_RTRANS(1)=1
C
          IGS= NINT(RTRANS(MY_TRANS,18))
          IF(IGS/=0)THEN
            IGS=IGRNOD(IGS)%ID
            WRITE(VARNAME,'(A)') 'Grnod_ID'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGS,0.0_8)
          END IF
          IS_INTEGER_RTRANS(18)=1
C
          DO I=1,NRTRANS
C
            IF(IS_INTEGER_RTRANS(I)/=0) CYCLE
C
            IF(RTRANS(MY_TRANS,I)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'RTRANS_',I
              TEMP_DOUBLE = RTRANS(MY_TRANS,I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_TRANS=1,NTRANSF
      END IF
C-----------------------------------------------
      RETURN
      END
