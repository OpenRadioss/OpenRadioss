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
Chd|  FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_RTCL_S (
     1     NEL      ,NUPARAM  ,NUVAR    ,TIME     ,TIMESTEP ,UPARAM   ,
     2     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     3     NGL      ,DPLA     ,UVAR     ,OFF      ,DFMAX    ,TDELE    )
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include "implicit_f.inc"
C!---------+---------+---+---+-------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL)
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,J,INDX(MVSIZ),NINDX
      my_real 
     .        P,triaxs,SVM,SXX,SYY,SZZ,EPS_CR,F_RTCL
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EPS_CR = UPARAM(1)
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
       IF (OFF(I) < EM01) OFF(I) = ZERO
       IF (OFF(I) < ONE .AND. OFF(I) > ZERO) OFF(I) = OFF(I)*FOUR_OVER_5
      END DO
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
          P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          SXX = SIGNXX(I) - P
          SYY = SIGNYY(I) - P
          SZZ = SIGNZZ(I) - P
          SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
          SVM = SQRT(THREE*SVM)
          triaxs = P/MAX(EM20,SVM)
          IF (triaxs > ONE)  triaxs = ONE
          IF (triaxs < -ONE) triaxs = -ONE
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
          ! Computation of damage variables
          DFMAX(I) = DFMAX(I) + F_RTCL*DPLA(I)/MAX(EPS_CR,EM6)
          DFMAX(I) = MIN(ONE,DFMAX(I))
c
          ! Checking element failure using global damage
          IF (DFMAX(I) >= ONE .AND. OFF(I) == ONE) THEN
            OFF(I)      = FOUR_OVER_5
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK    = 1   
            TDELE(I)    = TIME    
          ENDIF
        ENDIF 
      ENDDO
c------------------------
c------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
        END DO
      END IF         
c------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (RTCL) el#',I10,
     .          ' AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (RTCL) el#',I10,
     .          ' AT TIME :',1PE12.4)
      END
