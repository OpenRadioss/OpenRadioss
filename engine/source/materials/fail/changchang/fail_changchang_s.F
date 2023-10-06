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
      SUBROUTINE FAIL_CHANGCHANG_S(
     1    NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,    
     2    TIME      ,IP        ,ILAY      ,NPG       ,NGL       , 
     3    DMG_SCALE ,DFMAX     ,OFF       ,LOFF      ,NOFF      ,
     4    SIGNXX    ,SIGNYY    ,SIGNZZ    ,SIGNXY    ,SIGNZX    ,
     5    TDELE     )                            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include   "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include   "units_c.inc"
#include   "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NEL,NUPARAM,NUVAR,IP,ILAY,NPG
      INTEGER,DIMENSION(NEL),INTENT(IN) :: NGL
      my_real,INTENT(IN) :: TIME
      my_real,DIMENSION(NEL),INTENT(IN) :: 
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER,DIMENSION(NEL),INTENT(INOUT) :: NOFF
      my_real,DIMENSION(NEL),INTENT(INOUT) :: DFMAX,TDELE,
     .   DMG_SCALE,LOFF,OFF
      my_real,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,NINDX0,FAILIP
      INTEGER, DIMENSION(NEL) :: INDX,INDX0
      my_real :: SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX,
     .           DAMFT,DAMFC,DAMMT1,DAMMC1,DAMMT2,DAMMC2,DAMMX
C---------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      SIGT1  = UPARAM(1) ! -> Longitudinal tensile strength    
      SIGT2  = UPARAM(2) ! -> Transversal tensile strength    
      SIGT12 = UPARAM(3) ! -> Shear strength     
      SIGC1  = UPARAM(4) ! -> Longitudinal compressive strength   
      SIGC2  = UPARAM(5) ! -> Transversal compressive strength     
      BETA   = UPARAM(6) ! -> Shear coupling factor 
      TMAX   = UPARAM(7) ! -> Relaxation time     
      FAILIP = INT(UPARAM(9)) ! -> Number of failed integration points prior to solid deletion
      FAILIP = MIN(FAILIP,NPG)
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
      DO I = 1,NEL
c
        ! If damage has not been reached yet
        IF (DFMAX(I) < ONE) THEN
c
          ! Fiber damage in direction 1 
          IF (SIGNXX(I) > ZERO) THEN
            DAMFT = (SIGNXX(I)/SIGT1)**2 
     .            + BETA*(SIGNXY(I)/SIGT12)**2
     .            + BETA*(SIGNZX(I)/SIGT12)**2
            DAMFC = ZERO
          ELSE 
            DAMFC = (SIGNXX(I)/SIGC1)**2
            DAMFT = ZERO
          ENDIF 
c
          ! Matrix damage in direction 2 
          IF (SIGNYY(I) > ZERO) THEN                                   
            DAMMT1 = (SIGNYY(I)/SIGT2)**2                             
     .             + (SIGNXY(I)/SIGT12)**2                    
            DAMMC1 = ZERO        
          ELSE             
            DAMMC1 = (SIGNYY(I)/(TWO*SIGT12))**2                     
     .             + (SIGNXY(I)/SIGT12)**2                             
     .             + SIGNYY(I)*((SIGC2/(TWO*SIGT12))**2 - ONE)/SIGC2 
            DAMMT1 = ZERO
          ENDIF
c 
          ! Matrix damage in direction 3 
          IF (SIGNZZ(I) > ZERO) THEN                                   
            DAMMT2 = (SIGNZZ(I)/SIGT2)**2                             
     .             + (SIGNZX(I)/SIGT12)**2                    
            DAMMC2 = ZERO        
          ELSE             
            DAMMC2 = (SIGNZZ(I)/(TWO*SIGT12))**2                     
     .             + (SIGNZX(I)/SIGT12)**2                             
     .             + SIGNZZ(I)*((SIGC2/(TWO*SIGT12))**2 - ONE)/SIGC2 
            DAMMT2 = ZERO
          ENDIF          
c
          ! Damage variable update
          DAMMX = MAX(DAMFT,DAMFC,DAMMC1,DAMMT1,DAMMC2,DAMMT2)
          DFMAX(I) = MIN(ONE,MAX(DAMMX,DFMAX(I)))
          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX+1                                    
            INDX(NINDX) = I
            UVAR(I,1) = TIME
          ENDIF
        ENDIF
c
        ! Stress relaxation in case of damage reached
        IF ((UVAR(I,1) > ZERO).AND.(LOFF(I) /= ZERO).AND.(OFF(I) /= ZERO)) THEN
          DMG_SCALE(I) = EXP(-(TIME - UVAR(I,1))/TMAX)   
          IF (DMG_SCALE(I) < EM02) THEN
            LOFF(I) = ZERO
            TDELE(I) = TIME
            DMG_SCALE(I) = ZERO
            NOFF(I) = NOFF(I) + 1
            IF (NOFF(I) >= FAILIP) THEN
              OFF(I) = ZERO
              NINDX0 = NINDX0 + 1 
              INDX0(NINDX0) = I  
            ENDIF
          ENDIF
        ENDIF
c
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
 1000 FORMAT(1X,'FAILURE (CHANG) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5)
 1100 FORMAT(1X,'FAILURE (CHANG) OF SOLID ELEMENT ',I10,1X,
     .',GAUSS PT',I5,1X,',LAYER',I5,1X,'AT TIME :',1PE20.13)
 1200 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,
     .'AT TIME :',1PE20.13)  
C-------------------------------------------- 
      END
