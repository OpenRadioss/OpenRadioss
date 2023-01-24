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
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_HIC                    source/tools/sensor/sensor_hic.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_HIC(SENSOR ,ACCN, ACCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(3,*) :: ACCN
      my_real, DIMENSION(LLACCELM,*) :: ACCEL
      TYPE (SENSOR_STR_) ,TARGET :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,J,IPOINT,IDIR,IACC,ISENSOR_TYPE,NPOINT,INDX,NVAR,ICRIT
c
      my_real :: TIME_UNIT,TIME,xINDX, INTEGRAL,INCREMENT,TIME_INTERVAL
      my_real :: HIC_PERIOD,HIC_CRIT,HIC,HIC_PREC,HIC_TMP
      my_real :: TIME_PREC,TEST_TIME,DELTA_T,BETA
      my_real :: ACC_X,ACC_Y,ACC_Z,ACC,ACCG,ACC_PREC,CURRENT_VALUE,GRAVITY
      my_real :: PERIOD_TMP,OPT_PERIOD,T1,T2,TMIN,TDELAY,INFINITY
      my_real ,DIMENSION(:) ,POINTER :: VALUE_TABLE
c
      DATA NVAR/4/
      PARAMETER (INFINITY = 1.0E20)
c-----------------------------------------------------------------------
c     Sensor state variables
c        SENSOR%VAR(1) :  TIME_PREC   = beginning of current time interval
c        SENSOR%VAR(2) :  Integral of acceleration over time within current time interval
c        SENSOR%VAR(3) :  Index of current time interval in data table
c        SENSOR%VAR(4) :  Previous HIC value
c        SENSOR%VAR(5...5+NPOINT)  : average acceleration data table by time intervals
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY
      ISENSOR_TYPE = SENSOR%IPARAM(1)
      IACC         = SENSOR%IPARAM(2)
      NPOINT       = SENSOR%IPARAM(3)
      IDIR         = SENSOR%IPARAM(4)

      HIC_PERIOD   = SENSOR%RPARAM(1)
      HIC_CRIT     = SENSOR%RPARAM(2)
      GRAVITY      = SENSOR%RPARAM(3)
      TIME_UNIT    = SENSOR%RPARAM(4)
c
      TIME = TT
      BETA = 2.5D0
      VALUE_TABLE(1:NPOINT) => SENSOR%VAR(NVAR+1:NVAR+NPOINT)
c      
      IF (ISENSOR_TYPE == 1) THEN   ! Acceletator based
        ACC_X = ACCEL(20,IACC)
        ACC_Y = ACCEL(21,IACC)
        ACC_Z = ACCEL(22,IACC)
      ELSE                          ! Node based
        ACC_X = ACCN(1,IACC)
        ACC_Y = ACCN(2,IACC)
        ACC_Z = ACCN(3,IACC)
      END IF        
c
      IF (IDIR == 1) THEN     ! Resultant
        ACC = SQRT(ACC_X**2 + ACC_Y**2 + ACC_Z**2)
      ELSEIF (IDIR == 2) THEN    !  X
        ACC = ACC_X   
      ELSEIF (IDIR == 3) THEN    !  Y
        ACC = ACC_Y  
      ELSEIF (IDIR == 4) THEN    !  Z
        ACC = ACC_Z    
      END IF
      ACCG = ACC / GRAVITY
c
c...  Get last sensor state variables

      TIME_PREC = SENSOR%VAR(1)
      ACC_PREC  = SENSOR%VAR(2)
      xINDX     = SENSOR%VAR(3)
      HIC_PREC  = SENSOR%VAR(4)
      INDX  = NINT(xINDX)
      ICRIT = 0
c
      TIME_INTERVAL = HIC_PERIOD / NPOINT
      TEST_TIME = TIME_PREC + TIME_INTERVAL
c
c.... Calculate new HIC value (only at the start of new time interval) 
c
      IF (TIME > TEST_TIME) THEN
        DELTA_T = TIME - TIME_PREC
        ! average acceleration from previous time interval
        CURRENT_VALUE = (ACC_PREC + ACCG*DT12) / DELTA_T
c
        IF (INDX == NPOINT) THEN               
c         shift data to make place for new time interval when HIC_PERIOD is reached
          DO I = 1,NPOINT-1
            VALUE_TABLE(I) = VALUE_TABLE(I+1)   
          END DO                                
        ELSE          ! advance data index for next time interval                          
          INDX = INDX + 1                       
        ENDIF
        VALUE_TABLE(INDX) = CURRENT_VALUE       
c
c       Calculation of HIC
c       Searching for the period (lower or equal to HIC_PERIOD) maximizing HIC
c
        OPT_PERIOD = ZERO
        HIC  = ZERO
        DO I = 1,INDX
          INTEGRAL = ZERO
          DO J = I,INDX
            INTEGRAL = INTEGRAL + VALUE_TABLE(J)*DELTA_T
          END DO
          INCREMENT = INDX + 1 - I
          PERIOD_TMP = DELTA_T * INCREMENT
          HIC_TMP = PERIOD_TMP * ((INTEGRAL/PERIOD_TMP)**BETA)
          HIC_TMP = HIC_TMP * TIME_UNIT          ! convert to seconds ???
          IF (HIC_TMP > HIC) THEN
            HIC = HIC_TMP
            OPT_PERIOD = PERIOD_TMP            
            T1 = MAX(TIME - OPT_PERIOD, ZERO)  
            T2 = TIME                          
          ENDIF
        END DO
c
c       Save sensor status, for next step
        SENSOR%VAR(1) = TIME
        SENSOR%VAR(2) = ZERO       ! reinitialize integral for next time interval
        SENSOR%VAR(3) = INDX
        IF (HIC >= HIC_PREC) THEN
          SENSOR%VAR(4) = HIC
          ICRIT = 1
        ENDIF 
c
c.......Check Sensor State
        IF (SENSOR%TCRIT + TMIN > TT) THEN
          IF (ICRIT == 0) THEN
            SENSOR%TCRIT = INFINITY
          ELSE IF (SENSOR%TCRIT == INFINITY) THEN
            SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
          END IF
        ELSE IF (SENSOR%TSTART == INFINITY) THEN
          SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY
        END IF
c
        IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
        END IF
c
      ELSE    ! TIME < TEST_TIME
c       Calculate acceleration integral within current time interval 
        SENSOR%VAR(2) = SENSOR%VAR(2) + ACCG*DT12
      ENDIF  ! Fin of TIME test
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1200)  HIC, HIC_PERIOD
        WRITE (IOUT ,1300)  T1, T2
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      HIC     = ',1PE12.5,'  ,HIC PERIOD  = ',1PE12.5)           
1300  FORMAT('      TIME T1 = ' 1PE12.5,'  ,TIME T2 = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
