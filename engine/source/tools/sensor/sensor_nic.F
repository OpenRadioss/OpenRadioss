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
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_NIC(SENSOR)
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
      TYPE (SENSOR_STR_) ,TARGET :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,J,IPOINT,IDIR,IACC,ISENSOR_TYPE,NPOINT,NPH,NVAR,
     .        SKEW_SENS,SKEW_SPRING,IDIR1,IDIR2
c
      my_real :: TIME_UNIT,TIME,xINDX, INTEGRAL,INCREMENT
      my_real :: NIC_PERIOD,NIJMAX,NIC,NIC_PREC,NIC_TMP,DTIME
      my_real :: TIME_PREC,TEST_TIME,DELTA_T
      my_real :: PERIOD_TMP,OPT_PERIOD,
     .           FTENS,FCOMP,MFLEX,MEXT,CFC,ALPHA,BETA
      my_real :: NIJ,FOR,MOM
      my_real ,DIMENSION(:) ,ALLOCATABLE :: FX,FY,FZ,MX,MY,MZ
      my_real ,DIMENSION(:) ,POINTER :: VALUE_TABLE
c
      DATA NVAR/4/
c-----------------------------------------------------------------------
c     Sensor state variables
c        SENSOR%VAR(1) :  TIME_PREC   = beginning of current time interval
c        SENSOR%VAR(2) :  Integral of acceleration over time within current time interval
c        SENSOR%VAR(3) :  Index of current time interval in data table
c        SENSOR%VAR(4) :  Previous NIC value
c        SENSOR%VAR(5...5+NPOINT)  : average acceleration data table by time intervals
C=======================================================================
      IF (SENSOR%STATUS == 3) RETURN   ! already activated
c
      ISENSOR_TYPE = SENSOR%IPARAM(1)
      SKEW_SENS    = SENSOR%IPARAM(3)
      SKEW_SPRING  = SENSOR%IPARAM(4)
      IDIR1        = SENSOR%IPARAM(5)
      IDIR2        = SENSOR%IPARAM(6)
      NPOINT       = SENSOR%IPARAM(7)
c
      NIJMAX = SENSOR%RPARAM(1)
      FTENS  = SENSOR%RPARAM(2)
      FCOMP  = SENSOR%RPARAM(3)
      MFLEX  = SENSOR%RPARAM(4)
      MEXT   = SENSOR%RPARAM(5)
      CFC    = SENSOR%RPARAM(6)
      ALPHA  = SENSOR%RPARAM(7)
      DTIME  = SENSOR%RPARAM(8)
      
!      ALLOCATE(FZ(NPOINT))
!      ALLOCATE(MY(NPOINT))
c     actual normalized spring force and moment
      FOR = SENSOR%VAR(1)
      MOM = SENSOR%VAR(2)
      TIME_PREC = SENSOR%VAR(3)  ! bidon
      
      
!     1) fill Force and moment tables from previous time steps  
!     2) call filtering routine for all components
!     3) Calculate Nij criterion for each combination of normalized 
!        tension-compression-flex-ext combinations : N11,N12,N21,N22
!     4) test crit value over Tmin and deactivate sensor
!

      NIJ = FOR + MOM



!      
c
      TIME = TT
!      VALUE_TABLE(1:NPOINT) => SENSOR%VAR(NVAR+1:NVAR+NPOINT)
c      
c
c
c...  Get last sensor state variables

c
      TEST_TIME = TIME_PREC + DTIME
c
c.... Calculate new NIC value (only at the start of new time interval) 
c
!      IF (TIME > TEST_TIME) THEN
!        DELTA_T = TIME - TIME_PREC
!        ! average Nij from previous time interval
!!!        CURRENT_VALUE = ??
!c
!        IF (INDX == NPOINT) THEN               
!c         shift data to make place for new time interval when NIC_PERIOD is reached
!          DO I = 1,NPOINT-1
!            VALUE_TABLE(I) = VALUE_TABLE(I+1)   
!          END DO                                
!        ELSE          ! advance data index for next time interval                          
!          INDX = INDX + 1                       
!        ENDIF
!        VALUE_TABLE(INDX) = CURRENT_VALUE       
!c
!c       Calculation of NIC
!c       Searching for the period (lower or equal to NIC_PERIOD) maximizing NIC
!c
!        OPT_PERIOD = ZERO
!        NIC  = ZERO
!        DO I = 1,INDX
!          INTEGRAL = ZERO
!          DO J = I,INDX
!            INTEGRAL = INTEGRAL + VALUE_TABLE(J)*DELTA_T
!          END DO
!          INCREMENT = INDX + 1 - I
!          PERIOD_TMP = DELTA_T * INCREMENT
!          NIC_TMP = PERIOD_TMP * ((INTEGRAL/PERIOD_TMP)**BETA)
!          NIC_TMP = NIC_TMP * TIME_UNIT          ! convert to seconds ???
!          IF (NIC_TMP > NIC) THEN
!            NIC = NIC_TMP
!            OPT_PERIOD = PERIOD_TMP            
!            T1 = MAX(TIME - OPT_PERIOD, ZERO)  
!            T2 = TIME                          
!          ENDIF
!        END DO
!c
!c       Save sensor status, for next step
!        SENSOR%VAR(1) = TIME
!        SENSOR%VAR(2) = ZERO       ! reinitialize integral for next time interval
!        SENSOR%VAR(3) = INDX
!        IF (NIC > NIC_PREC) THEN
!          SENSOR%VAR(4) = NIC
!        ENDIF 
!c
!c.......Check Sensor State
!        IF (NIC >= NIJMAX) THEN
!          IF (SENSOR%STATUS == 0) THEN
!            SENSOR%STATUS = 1
!            SENSOR%TCRIT  = TIME + SENSOR%TMIN
!          END IF
!          IF (SENSOR%TCRIT <= TIME) THEN
!            SENSOR%STATUS = 2
!          END IF
!          IF (SENSOR%STATUS == 2 .and. 
!     .        SENSOR%TCRIT + SENSOR%TDELAY <= TIME) THEN
!            SENSOR%STATUS = 3   ! activate
!            SENSOR%TSTART = TIME
!          END IF
!        ENDIF ! Fin test NIC
!c
!      ELSE    ! TIME < TEST_TIME
!c       Calculate acceleration integral within current time interval 
!!        SENSOR%VAR(2) = SENSOR%VAR(2) + ACCG*DT12
!      ENDIF  ! Fin of TIME test
!c
!!      DEALLOCATE(FZ)
!!      DEALLOCATE(MY)
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 3 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5,
     .       '      NIC VALUE  = ',1PE12.5)           
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
