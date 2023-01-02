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
Chd|  FAIL_RTCL_C                   source/materials/fail/rtcl/fail_rtcl_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_RTCL_C (
     1     NEL      ,NUPARAM  ,NUVAR    ,TIME     ,TIMESTEP ,UPARAM   ,
     2     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,NPTOT    ,
     3     NGL      ,DPLA     ,UVAR     ,OFF      ,DFMAX    ,TDELE    ,
     4     AREA     ,FOFF     ,IGTYP    ,OFFL     ,IPT      ,THK0     )
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include "implicit_f.inc"
C!---------+---------+---+---+-------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
#include "impl1_c.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IGTYP,IPT,
     .        NPTOT
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),OFFL(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),AREA(NEL),THK0(NEL)
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,CONDITION(NEL),INST
      my_real 
     .        NHARD,EPSCAL,P,triaxs,vmises,hydros,EPS_CR,F_RTCL
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EPSCAL = UPARAM(1)
      INST   = NINT(UPARAM(2))
      NHARD  = UPARAM(3)
c
      ! Store initial element size
      IF (UVAR(1,1) == ZERO) THEN 
        DO I = 1,NEL
          UVAR(I,1) = SQRT(AREA(I))
          UVAR(I,2) = THK0(I)
        ENDDO
      ENDIF
C
      ! Initialization of variable
      NINDX = 0  
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE DAMAGE VARIABLE
      !====================================================================       
      DO I=1,NEL
c
        ! If the element is not broken
        IF (OFF(I) ==  ONE .AND. DPLA(I) /= ZERO) THEN
c        
          ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
          hydros = (SIGNXX(I)+ SIGNYY(I))/THREE
          vmises = SQRT((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(THREE*SIGNXY(I)**2))
          triaxs = hydros / MAX(EM20,vmises)
          IF (triaxs > TWO_THIRD)  triaxs = TWO_THIRD
          IF (triaxs < -TWO_THIRD) triaxs = -TWO_THIRD
c
          ! Computing the plastic strain at failure according to stress triaxiality
          IF (triaxs < -THIRD) THEN 
            F_RTCL = ZERO
          ELSEIF ((triaxs >= -THIRD).AND.(triaxs < THIRD)) THEN
            F_RTCL = TWO*((ONE+triaxs*SQRT(TWELVE-TWENTY7*(triaxs**2)))/
     .                  (THREE*triaxs+SQRT(TWELVE-TWENTY7*(triaxs**2))))
          ELSE
            F_RTCL = EXP(-HALF)*EXP(THREE_HALF*triaxs)
          ENDIF
c 
          ! Computation of simple tension failure strain (according to mesh size)
          IF (INST == 2) THEN 
            EPS_CR = NHARD + (EPSCAL - NHARD)*(UVAR(I,2)/UVAR(I,1))
          ELSE
            EPS_CR = EPSCAL
          ENDIF
c
          ! Computation of damage variables
          DFMAX(I) = DFMAX(I) + F_RTCL*DPLA(I)/MAX(EPS_CR,EM6)
          DFMAX(I) = MIN(ONE,DFMAX(I))
c
          ! Checking element failure using global damage
          IF (OFFL(I) == ONE .AND. DFMAX(I) >= ONE) THEN
            OFFL(I)          = ZERO
            FOFF(I)          = 0
            NINDX            = NINDX + 1
            INDX(NINDX)      = I  
            CONDITION(NINDX) = IPT
          ENDIF
        ENDIF 
      ENDDO
c------------------------
c------------------------
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)         
#include "lockon.inc"
          IF(CONDITION(J) >= 1)  THEN         
            WRITE(IOUT, 2000) NGL(I),CONDITION(J),TIME
            WRITE(ISTDO,2000) NGL(I),CONDITION(J),TIME
          ENDIF
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX         
c------------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (RTCL)',I10,1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
      END