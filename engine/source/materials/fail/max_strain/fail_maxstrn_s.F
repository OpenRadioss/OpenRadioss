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
      SUBROUTINE FAIL_MAXSTRN_S(
     1     NEL     ,NUVAR   ,IP      ,ILAY    ,NPG     ,TIME    ,
     2     TIMESTEP,UPARAM  ,NGL     ,OFF     ,LOFF    ,NOFF    ,
     3     EPSXX   ,EPSYY   ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     4     UVAR    ,NUPARAM ,DFMAX   ,TDELE   ,DMG_SCALE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
#include  "mvsiz_p.inc"
#include  "com01_c.inc"
#include  "scr17_c.inc"
#include  "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .   ILAY,IP,NPG,NGL(NEL)
      my_real, INTENT(IN) :: 
     .   TIME,TIMESTEP,UPARAM(NUPARAM)
      my_real, INTENT(INOUT) :: 
     .   EPSXX(NEL),EPSYY(NEL),EPSZZ(NEL),
     .   EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: NOFF(NEL)
      my_real, INTENT(INOUT) ::
     .   UVAR(NEL,NUVAR),OFF(NEL),DFMAX(NEL),
     .   TDELE(NEL),DMG_SCALE(NEL),LOFF(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,J,INDX(NEL),NINDX,INDX0(NEL),NINDX0,
     .   IFAIL_SO
      my_real 
     .   EPS1_MAX,EPS2_MAX,GAM12_MAX,TMAX,FCUT
      my_real
     .   ASRATE,EXX(NEL),EYY(NEL),EZZ(NEL),
     .   EXY(NEL),EYZ(NEL),EZX(NEL),FINDEX,RFACTR
C--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EPS1_MAX  = UPARAM(1)
      EPS2_MAX  = UPARAM(2)
      GAM12_MAX = UPARAM(3)
      TMAX      = UPARAM(4)
      FCUT      = UPARAM(5)
      IFAIL_SO  = INT(UPARAM(7))
c
      ! Stress tensor filtering
      IF (FCUT > ZERO) THEN
        DO I = 1,NEL
          ASRATE = TWO*PI*FCUT*TIMESTEP
          ASRATE = ASRATE/(ONE+ASRATE)
          EXX(I) = ASRATE*EPSXX(I) + (ONE - ASRATE)*UVAR(I,2)
          EYY(I) = ASRATE*EPSYY(I) + (ONE - ASRATE)*UVAR(I,3)
          EZZ(I) = ASRATE*EPSZZ(I) + (ONE - ASRATE)*UVAR(I,4)
          EXY(I) = ASRATE*EPSXY(I) + (ONE - ASRATE)*UVAR(I,5)
          EYZ(I) = ASRATE*EPSYZ(I) + (ONE - ASRATE)*UVAR(I,6)
          EZX(I) = ASRATE*EPSZX(I) + (ONE - ASRATE)*UVAR(I,7)
          UVAR(I,2) = EXX(I)
          UVAR(I,3) = EYY(I)
          UVAR(I,4) = EZZ(I)
          UVAR(I,5) = EXY(I)
          UVAR(I,6) = EYZ(I)
          UVAR(I,7) = EZX(I)
        ENDDO
      ELSE
        DO I = 1,NEL
          EXX(I) = EPSXX(I)
          EYY(I) = EPSYY(I)
          EZZ(I) = EPSZZ(I)
          EXY(I) = EPSXY(I)
          EYZ(I) = EPSYZ(I)
          EZX(I) = EPSZX(I)        
        ENDDO
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index     
      NINDX  = 0 
      NINDX0 = 0 
      INDX   = 0 
      INDX0  = 0
c
      ! Loop over the elements
      DO I=1,NEL
c
        ! If damage has not been reached yet
        IF (DFMAX(I)<ONE) THEN
c
          ! Compute failure index and reserve factor
          FINDEX = MAX(ABS(EXX(I))/EPS1_MAX,
     .                 ABS(EYY(I))/EPS2_MAX,
     .                 ABS(EXY(I))/GAM12_MAX,
     .                 ABS(EZZ(I))/EPS2_MAX,
     .                 ABS(EZX(I))/GAM12_MAX)
          IF (FINDEX > ZERO) THEN 
            RFACTR = ONE/SQRT(FINDEX)
          ELSE 
            RFACTR = ZERO
          ENDIF
c
          ! Damage variable update
          DFMAX(I) = MIN(ONE,FINDEX) 
          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX+1                                    
            INDX(NINDX) = I                                    
            IF (IFAIL_SO > 0) THEN 
              UVAR(I,1) = TIME
            ENDIF                                
          ENDIF
        ENDIF         
c
        ! Stress relaxation in case of damage reached
        IF ((UVAR(I,1) > ZERO).AND.(LOFF(I) /= ZERO).AND.
     .      (IFAIL_SO > 0).AND.(OFF(I) /= ZERO)) THEN 
          DMG_SCALE(I) = EXP(-(TIME - UVAR(I,1))/TMAX)   
          IF (DMG_SCALE(I) < EM02) THEN
            LOFF(I) = ZERO
            TDELE(I) = TIME
            DMG_SCALE(I) = ZERO
            IF (IFAIL_SO == 1) THEN 
              OFF(I) = ZERO
              NINDX0 = NINDX0 + 1 
              INDX0(NINDX0) = I  
            ELSEIF (IFAIL_SO == 2) THEN 
              NOFF(I) = NOFF(I) + 1
              IF (INT(NOFF(I)) >= NPG) THEN
                OFF(I) = ZERO
                NINDX0 = NINDX0 + 1 
                INDX0(NINDX0) = I  
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO  
c              
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================    
      IF(NINDX > 0)THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IP,ILAY
          WRITE(ISTDO,1100) NGL(I),IP,ILAY,TIME
#include "lockoff.inc"
        END DO
      ENDIF         
C                
      IF(NINDX0 > 0)THEN
        DO J=1,NINDX0
          I = INDX0(J)
#include "lockon.inc"
          WRITE(IOUT, 1200) NGL(I),TIME
          WRITE(ISTDO,1200) NGL(I),TIME
#include "lockoff.inc"
        END DO
      ENDIF      
C--------------------------------------------      
 1000 FORMAT(1X,'FAILURE (MAXSTRN) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5)
 1100 FORMAT(1X,'FAILURE (MAXSTRN) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5,1X,'AT TIME :',1PE20.13)
 1200 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,
     .'AT TIME :',1PE20.13)  
C--------------------------------------------      
      RETURN
      END
