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
Chd|  ST_QAPRINT_OUTPUT_DATABASES   source/output/qaprint/st_qaprint_output_databases.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_OUTPUT_DATABASES(NOM_OPT, INOM_OPT, LACCELM, ACCELM,LGAUGE,GAUGE)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: LACCELM(3, NACCELM)
      my_real, INTENT(IN) ::  
     .                        ACCELM(LLACCELM, NACCELM)
      INTEGER, INTENT(IN) :: LGAUGE(3,NBGAUGE)
      my_real, INTENT(IN) :: GAUGE(LLGAUGE,NBGAUGE)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID, II, TEMP_INT
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      LOGICAL :: OK_QA
C-----------------------------------------------
C     /DAMP
C-----------------------------------------------
      OK_QA = MYQAKEY('/ACCEL')
      IF (OK_QA) THEN
        DO II = 1, NACCELM
C
          TITR(1:nchartitle)=''
          ID = NOM_OPT(1, INOM_OPT(1) + II)
          CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1, INOM_OPT(1) + II), LTITR)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ID,0.0_8)
          ELSE
            CALL QAPRINT('A_ACCEL_FAKE_NAME', ID,0.0_8)
          END IF
C     
          DO I = 1, 3
             WRITE(VARNAME,'(A,I0,A,I0)') 'IACCEL_',ID,'_',I
             TEMP_INT = LACCELM(I,II)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
          ENDDO
          DO I = 1, LLACCELM
             WRITE(VARNAME,'(A,I0,A,I0)') 'RACCEL_',ID,'_',I
             TEMP_DOUBLE = ACCELM(I,II)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END DO
C
        END DO 
      END IF
C-----------------------------------------------
C     /GAUGE
C-----------------------------------------------
      IF (MYQAKEY('/GAUGE')) THEN
C       
       IF (NBGAUGE > 0) THEN 
        DO II = 1, NBGAUGE
          TITR(1:nchartitle)=''
          ID = NOM_OPT(1, INOM_OPT(27) + II)
          CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1, INOM_OPT(27) + II), LTITR)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ID,0.0_8)
          ELSE
            CALL QAPRINT('A_GAUGE_FAKE_NAME', ID,0.0_8)
          END IF     
          DO I = 1, 3
           IF(LGAUGE(I,II) /=0)THEN
             WRITE(VARNAME,'(A,I0,A,I0)') 'LGAUGE_',I,'_',II
             TEMP_INT = LGAUGE(I,II)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
           ENDIF
          ENDDO
          DO I = 1, LLGAUGE
           IF(GAUGE(I,II) /=0)THEN
             WRITE(VARNAME,'(A,I0,A,I0)') 'GAUGE_',I,'_',II
             TEMP_DOUBLE = GAUGE(I,II)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
           ENDIF
          END DO
        END DO 
       END IF
      END IF

C-----------------------------------------------
      RETURN
      END
