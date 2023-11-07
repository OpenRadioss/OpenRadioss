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
Chd|  FAIL_PUCK_C                   source/materials/fail/puck/fail_puck_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_PUCK_C(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      , 
     2     TIME      ,NGL       ,IPG       ,ILAY      ,IPT       ,                          
     3     SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,  
     4     OFF       ,FOFF      ,DMG_FLAG  ,DMG_SCALE ,
     5     DFMAX     ,TDEL      ,TIMESTEP  )                               
C-----------------------------------------------
C    modified Puck model ------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C DMG_FLAG|  1      | I | W | STRESS REDUCTION FLAG DUE TO DAMAGE
C DMG_SCALE| NEL    | F | W | STRESS REDUCTION FACTOR
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,FSMOOTH
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: SIGT1,SIGT2, SIGC1,SIGC2,FSIG12,F1,FA,FB,FC,
     .           PN12,PP12,PN22,FAC,TMAX,DAMMX,FCUT,ASRATE,
     .           SXX(NEL),SYY(NEL),SXY(NEL)
C=======================================================================
      SIGT1  = UPARAM(1)
      SIGT2  = UPARAM(2)
      SIGC1  = UPARAM(3)
      SIGC2  = UPARAM(4)
      FSIG12 = UPARAM(5)
      PP12   = UPARAM(6)
      PN12   = UPARAM(7) 
      PN22   = UPARAM(8)
      TMAX   = UPARAM(9)
      FCUT   = UPARAM(12)
      IF (FCUT > ZERO) THEN
        FSMOOTH = 1 
        ASRATE = TWO*PI*FCUT*TIMESTEP
        ASRATE = ASRATE/(ONE+ASRATE)
      ELSE
        FSMOOTH = 0
      ENDIF
c
      DMG_FLAG = 1               
      NINDX    = 0 
      IF (FSMOOTH > 0) THEN 
        DO I = 1,NEL
          SXX(I) = ASRATE*SIGNXX(I) + (ONE - ASRATE)*UVAR(I,2)
          SYY(I) = ASRATE*SIGNYY(I) + (ONE - ASRATE)*UVAR(I,3)
          SXY(I) = ASRATE*SIGNXY(I) + (ONE - ASRATE)*UVAR(I,4)
          UVAR(I,2) = SXX(I)
          UVAR(I,3) = SYY(I)
          UVAR(I,4) = SXY(I)
        ENDDO
      ELSE
        DO I = 1,NEL
          SXX(I) = SIGNXX(I)
          SYY(I) = SIGNYY(I)
          SXY(I) = SIGNXY(I)
        ENDDO
      ENDIF
c-------------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
          F1 = ZERO
          FA = ZERO
          FB = ZERO
          FC = ZERO
C       
          IF (UVAR(I,1) == ZERO) THEN                  
c
c           fiber criteria
c
            IF (SXX(I) >= ZERO) THEN
              F1 = SXX(I)/SIGT1
            ELSE
              F1 = -SXX(I)/SIGC1
            ENDIF
c
c           matrix criteria                                      
c                                                                
            IF (SYY(I) >= ZERO) THEN
              FAC = ONE - PP12*SIGT2/FSIG12
              FAC = FAC*SYY(I)/SIGT2
              FA  = SQRT((SXY(I)/FSIG12)**2 + FAC*FAC) 
     .            + PP12*SYY(I)/FSIG12
             ELSE
              FAC = HALF/(ONE + PN22)/FSIG12
              FC  = (SXY(I)*FAC)**2 + (SYY(I)/SIGC2)**2
              FC  =-FC*SIGC2/SYY(I)
            ENDIF
            FB = SQRT(SXY(I)**2 + (PN12*SYY(I))**2 ) 
     .                             + PN12*SYY(I)
            FB = FB/FSIG12
C
            DAMMX    = MIN(ONE,MAX(F1,FA,FB,FC))
            DFMAX(I) = MIN(ONE,DAMMX)
            IF (DAMMX >= ONE) THEN
              NINDX = NINDX+1                                    
              INDX(NINDX) = I                                    
              UVAR(I,1) = TIME                                   
            ENDIF
          ENDIF
c
          IF (UVAR(I,1) > ZERO) THEN 
            DMG_SCALE(I) = EXP(-(TIME - UVAR(I,1))/TMAX)   
            IF (DMG_SCALE(I) < EM02) THEN
              FOFF(I) = 0
              TDEL(I) = TIME
              DMG_SCALE(I) = ZERO
            ENDIF
          ENDIF                  
        ENDIF        
      ENDDO                     
c------------------------
c       print
c------------------------
      IF (NINDX > 0) THEN  
        DO J=1,NINDX       
          I = INDX(J)      
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
 2000 FORMAT(1X,'FAILURE (PUCK) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (PUCK) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
