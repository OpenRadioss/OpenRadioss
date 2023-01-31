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
Chd|  FAIL_TSAIWU_S                 source/materials/fail/tsaiwu/fail_tsaiwu_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_TSAIWU_S(
     1     NEL     ,NUVAR   ,IP      ,ILAY    ,NPG     ,TIME    ,
     2     TIMESTEP,UPARAM  ,NGL     ,OFF     ,LOFF    ,NOFF    ,
     3     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
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
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
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
     .   F1,F2,F11,F22,F66,F12,TMAX,FCUT
      my_real
     .   ASRATE,SXX(NEL),SYY(NEL),SZZ(NEL),
     .   SXY(NEL),SYZ(NEL),SZX(NEL),A,B,FINDEX,RFACTR
C--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      F1       = UPARAM(1)
      F2       = UPARAM(2)
      F11      = UPARAM(3)
      F22      = UPARAM(4)
      F66      = UPARAM(5)
      F12      = UPARAM(6)
      TMAX     = UPARAM(7)
      FCUT     = UPARAM(8)
      IFAIL_SO = INT(UPARAM(10))
c
      ! Stress tensor filtering
      IF (FCUT > ZERO) THEN
        DO I = 1,NEL
          ASRATE = TWO*PI*FCUT*TIMESTEP
          ASRATE = ASRATE/(ONE+ASRATE)
          SXX(I) = ASRATE*SIGNXX(I) + (ONE - ASRATE)*UVAR(I,2)
          SYY(I) = ASRATE*SIGNYY(I) + (ONE - ASRATE)*UVAR(I,3)
          SZZ(I) = ASRATE*SIGNZZ(I) + (ONE - ASRATE)*UVAR(I,4)
          SXY(I) = ASRATE*SIGNXY(I) + (ONE - ASRATE)*UVAR(I,5)
          SYZ(I) = ASRATE*SIGNYZ(I) + (ONE - ASRATE)*UVAR(I,6)
          SZX(I) = ASRATE*SIGNZX(I) + (ONE - ASRATE)*UVAR(I,7)
          UVAR(I,2)  = SXX(I)
          UVAR(I,3) = SYY(I)
          UVAR(I,4) = SZZ(I)
          UVAR(I,5) = SXY(I)
          UVAR(I,6) = SYZ(I)
          UVAR(I,7) = SZX(I)
        ENDDO
      ELSE
        DO I = 1,NEL
          SXX(I) = SIGNXX(I)
          SYY(I) = SIGNYY(I)
          SZZ(I) = SIGNZZ(I)
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)        
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
          ! Compute parameters A and B
          A = F11*(SXX(I)**2) + F22*(SYY(I)**2) + 
     .        F22*(SZZ(I)**2) + F66*(SXY(I)**2) + 
     .        F66*(SZX(I)**2) + TWO*F12*SXX(I)*SYY(I) + 
     .        TWO*F12*SXX(I)*SZZ(I)  
          B = F1*SXX(I) + F2*SYY(I) + F2*SZZ(I)
c
          ! Compute failure index and reserve factor
          FINDEX = A + B
          RFACTR = (-B + SQRT((B**2)+FOUR*A))/(TWO*A)
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
 1000 FORMAT(1X,'FAILURE (TSAIWU) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5)
 1100 FORMAT(1X,'FAILURE (TSAIWU) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5,1X,'AT TIME :',1PE20.13)
 1200 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,
     .'AT TIME :',1PE20.13)  
C--------------------------------------------      
      RETURN
      END
