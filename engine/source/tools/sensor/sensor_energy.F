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
Chd|  SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_ENERGY(SENSOR   ,ISENS    ,SUBSET   ,PARTSAV2 ,
     .                         ENINT    ,ENCIN    )
c-----------------------------------------------------------------------
!$COMMENT
!       SENSOR_ENERGY description
!       SENSOR_ENERGY organization :
!       - computation 
!       - sensor state modification
!$ENDCOMMENT
c-----------------------------------------------
C   M o d u l e s
c-----------------------------------------------
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
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: ISENS
      my_real ,INTENT(IN) :: ENINT    ! total internal energy
      my_real ,INTENT(IN) :: ENCIN    ! total kinetic energy    
      my_real ,INTENT(IN) :: PARTSAV2(2,*)
      TYPE (SUBSET_) ,DIMENSION(NSUBS) ,INTENT(IN) :: SUBSET
      TYPE (SENSOR_STR_) ,INTENT(INOUT) :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: I,J,IPART,ISUBS,NBR_GROUP,NP,INDX,ITSK,IFLAG,ICONST,
     .           NI,NK,ICRIT,ICRIT1,ICRIT2,ISELECT
      my_real :: TMIN,TDELAY,TSTART,TSTOPS,IEMIN,TCRIT,
     .   EINT,EKIN,IEMAX,KEMIN,KEMAX,IETOL,IETIME,KETOL,KETIME,
     .   AVG_EI,AVG_EK,ALPHAI,ALPHAK,INFINITY
      PARAMETER (INFINITY = 1.0E20)
c----------------------------------------------------------
c    SENSOR%VAR(1)   = TCRIT1 
c    SENSOR%VAR(2)   = TSTART1
c    SENSOR%VAR(3)   = TCRIT2 
c    SENSOR%VAR(4)   = TSTART2
c    SENSOR%VAR(5)   = Mean  EI
c    SENSOR%VAR(6)   = Mean  EK
c    SENSOR%VAR(7)   = Alphai
c    SENSOR%VAR(8)   = Alphak
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      IPART  = SENSOR%IPARAM(1)
      ISUBS  = SENSOR%IPARAM(2)
      ICONST = SENSOR%IPARAM(3)
      ISELECT= SENSOR%IPARAM(4)  ! Iselect == 2 => sensor will take total system energy
c      
      TDELAY = SENSOR%TDELAY
      TMIN   = SENSOR%TMIN
      TCRIT  = SENSOR%TCRIT
      TSTART = SENSOR%TSTART
      IEMIN  = SENSOR%RPARAM(1)
      IEMAX  = SENSOR%RPARAM(2)
      KEMIN  = SENSOR%RPARAM(3)
      KEMAX  = SENSOR%RPARAM(4)
      IETOL  = SENSOR%RPARAM(5)
      KETOL  = SENSOR%RPARAM(6)
      IETIME = SENSOR%RPARAM(7)
      KETIME = SENSOR%RPARAM(8)
c
      EINT   = ZERO
      EKIN   = ZERO
      ICRIT  = 0
      ICRIT1 = 0
      ICRIT2 = 0
      IFLAG  = 0
c---------------------------------------------------------
c     Fetch current energy values
c---------------------------------------------------------
      
      IF (ISELECT == 2) THEN  ! total system energy
        EINT = ENINT
        EKIN = ENCIN
      ELSE                    ! PART / SUBSET energy
        IF (IPARIT > 0) THEN
          EINT = ZERO
          EKIN = ZERO
          NBR_GROUP = SENSOR_STRUCT(ISENS)%NUM_GROUP_PART
          DO ITSK=2,NTHREAD
            SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1:6,1) = 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1:6,1) + 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1:6,ITSK)
            SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1:6,1) = 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1:6,1) + 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1:6,ITSK)
          ENDDO     

          DO J=2,6
            SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) = 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) + SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,J,1)
            SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1,1) = 
     .      SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1,1) + SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,J,1)
          ENDDO 
          EINT = SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1)
          EKIN = SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,1,1)
      
          DO ITSK=1,NTHREAD
            DO J=1,6
              SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,J,ITSK) = ZERO    
              SENSOR_STRUCT(ISENS)%FBSAV6_SENS(2,J,ITSK) = ZERO        
            ENDDO
          ENDDO
        ELSE    ! PARTTH/OFF
          EINT = ZERO
          EKIN = ZERO
          IF (IPART > 0) THEN
            EINT = PARTSAV2(1,IPART)
            EKIN = PARTSAV2(2,IPART)
          ELSEIF (ISUBS > 0) THEN
            NP = SUBSET(ISUBS)%NTPART
            DO J=1,NP
              IPART= SUBSET(ISUBS)%TPART(J)
              EINT = EINT + PARTSAV2(1,IPART)
              EKIN = EKIN + PARTSAV2(2,IPART)
            ENDDO
          ENDIF ! IF (IPART > 0)
        ENDIF
      ENDIF
c---------------------------------------------------------
c     TEST of principal criterion (Emax, Emin)
c---------------------------------------------------------
      IF (EINT < IEMIN) THEN
        ICRIT = 1
        IFLAG = 1               
      ELSE IF (EINT > IEMAX) THEN
        ICRIT = 1                   
        IFLAG = 2               
      ELSE IF (EKIN < KEMIN) THEN
        ICRIT = 1                   
        IFLAG = 3               
      ELSE IF (EKIN > KEMAX) THEN
        ICRIT = 1                   
        IFLAG = 4               
      ENDIF
c      
      IF (SENSOR%TCRIT + TMIN > TT) THEN
        IF (ICRIT == 0) THEN
          SENSOR%TCRIT = INFINITY
        ELSE IF (SENSOR%TCRIT == INFINITY) THEN
          SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
        END IF
      ELSE IF (SENSOR%TSTART == INFINITY) THEN
        SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY
      END IF
      IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
      END IF
c
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1000 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1000 ) SENSOR%SENS_ID,SENSOR%TSTART
        IF (IFLAG == 1) THEN
          WRITE (IOUT ,1100) IEMIN,EINT
        ELSE IF (IFLAG == 2) THEN
          WRITE (IOUT ,1200) IEMAX,EINT
        ELSE IF (IFLAG == 3) THEN
          WRITE (IOUT ,1300) KEMIN,EKIN
        ELSE IF (IFLAG == 4) THEN
          WRITE (IOUT ,1400) KEMAX,EKIN
        END IF
#include "lockoff.inc"
      END IF
c---------------------------------------------------------
c     TEST of constant internal energy criterion
c---------------------------------------------------------
      IF (ICONST == 1) THEN
        IF (TT == ZERO) THEN
          NI = NINT(IETIME / DT2) + 1
          NK = NINT(KETIME / DT2) + 1
          SENSOR%VAR(1) = INFINITY   ! TACT1
          SENSOR%VAR(2) = INFINITY   ! TSTART1
          SENSOR%VAR(3) = INFINITY   ! TACT2
          SENSOR%VAR(4) = INFINITY   ! TSTART2
          SENSOR%VAR(7) = MAX(ZERO ,MIN(ONE ,TWO / NI))
          SENSOR%VAR(8) = MAX(ZERO ,MIN(ONE ,TWO / NK))
        END IF
c
        IF (EINT > ZERO .and. IETOL > ZERO) THEN
          ALPHAI = SENSOR%VAR(7)
          AVG_EI = ALPHAI*EINT + (ONE - ALPHAI) * SENSOR%VAR(5)
          SENSOR%VAR(5) = AVG_EI
        END IF
c
c       Test sensor activation on constant internal energy criterion
c
        IF (EINT < AVG_EI + IETOL .and. EINT > AVG_EI - IETOL) ICRIT1 = 1
c
        IF (SENSOR%VAR(1) > TT) THEN
          IF (ICRIT1 == 0) THEN
            SENSOR%VAR(1) = INFINITY
          ELSE IF (SENSOR%VAR(1) == INFINITY) THEN
            SENSOR%VAR(1) = TT + IETIME
          END IF
        ELSE IF (SENSOR%VAR(2) == INFINITY) THEN
          SENSOR%VAR(2) = SENSOR%VAR(1) + TDELAY
        END IF
        IF (SENSOR%VAR(2) <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
          SENSOR%TSTART = SENSOR%VAR(2)
        END IF

        IF (SENSOR%VAR(2) <= TT .and. ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1000 ) SENSOR%SENS_ID,TT
          WRITE (IOUT ,1000 ) SENSOR%SENS_ID,TT
          WRITE (IOUT ,2100)  EINT,AVG_EI
#include "lockoff.inc"
        END IF
c
c---------------------------------------------------------
c       Test sensor activation on constant kinetic energy criterion
c---------------------------------------------------------
        IF (EKIN > ZERO .and. KETOL > ZERO) THEN
          ALPHAK = SENSOR%VAR(8)
          AVG_EK = ALPHAK*EKIN + (ONE - ALPHAK) * SENSOR%VAR(6)
          SENSOR%VAR(6) = AVG_EK
        END IF

        IF (EKIN < AVG_EK + KETOL .and. EKIN > AVG_EK - KETOL) ICRIT2 = 1
c
        IF (SENSOR%VAR(3) > TT) THEN
          IF (ICRIT2 == 0) THEN
            SENSOR%VAR(3) = INFINITY
          ELSE IF (SENSOR%VAR(3) == INFINITY) THEN
            SENSOR%VAR(3) = TT + KETIME
          END IF
        ELSE IF (SENSOR%VAR(4) == INFINITY) THEN
          SENSOR%VAR(4) = SENSOR%VAR(3) + TDELAY
        END IF
        IF (SENSOR%VAR(4) <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
          SENSOR%TSTART = SENSOR%VAR(4)
        END IF
c
        IF (SENSOR%VAR(4) <= TT .and. ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1000 ) SENSOR%SENS_ID,TT
          WRITE (IOUT ,1000 ) SENSOR%SENS_ID,TT
          WRITE (IOUT ,2200)  EKIN,AVG_EK
#include "lockoff.inc"
        END IF
c
      END IF  ! constant energy option
c-----------------------------------------------------------------------      
1000  FORMAT(' ENERGY SENSOR NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
1100  FORMAT('      TARGET MIN INTERNAL ENERGY = ',1PE12.5,/
     .       '      CURRENT INTERNAL ENERGY AFTER TMIN and TDELAY = ',1PE12.5)
1200  FORMAT('      TARGET MAX INTERNAL ENERGY = ',1PE12.5,/
     .       '      CURRENT INTERNAL ENERGY AFTER TMIN and TDELAY = ',1PE12.5)
1300  FORMAT('      TARGET MIN KINETIC ENERGY = ',1PE12.5,/
     .       '      CURRENT INTERNAL ENERGY AFTER TMIN and TDELAY = ',1PE12.5)
1400  FORMAT('      TARGET MAX KINETIC ENERGY = ',1PE12.5,/
     .       '      CURRENT INTERNAL ENERGY AFTER TMIN and TDELAY = ',1PE12.5)
2100  FORMAT('      CONSTANT INT ENERGY = ',1PE12.5,', MEAN = ',1PE12.5)           
2200  FORMAT('      CONSTANT KIN ENERGY = ',1PE12.5,', MEAN = ',1PE12.5)           
c----------------------------------------------------------
      RETURN
      END SUBROUTINE SENSOR_ENERGY

