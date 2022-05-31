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
Chd|====================================================================
Chd|  FAIL_CHANGCHANG_C             source/materials/fail/changchang/fail_changchang_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_CHANGCHANG_C(
     1           NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,    
     2           TIME      ,TIMESTEP  ,IPG       ,ILAY      ,IPT       , 
     3           NGL       ,DMG_FLAG  ,DMG_SCALE ,DFMAX     ,TDEL      ,  
     4           SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,    
     5           OFF       ,FOFF      ,PTHKF     )                               
C-----------------------------------------------
C    chang-chang failure model
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
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
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
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,TIMESTEP,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,IFAIL_SH,IDEL_FM,IDEL_F
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX,
     .           DAMFT,DAMFC,DAMMT,DAMMC,DAMMX
C=======================================================================
      SIGT1 = UPARAM(1)      
      SIGT2 = UPARAM(2)      
      SIGT12= UPARAM(3)      
      SIGC1 = UPARAM(4)      
      SIGC2 = UPARAM(5)      
      BETA  = UPARAM(6)      
      TMAX  = UPARAM(7)      
      IFAIL_SH = INT(UPARAM(8)) 
c-----------------
      DMG_FLAG = 1
      IDEL_F   = 0
      IDEL_FM  = 0
      IF (IFAIL_SH == 1) THEN        ! matrix or fiber failure
        IDEL_FM = 1           
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN    ! matrix or fiber failure
        IDEL_FM = 1          
        PTHKF = ONE
      ELSEIF (IFAIL_SH == 3) THEN    ! fiber only failure
        IDEL_F = 1            
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 4) THEN    ! fiber only failure
        IDEL_F = 1           
        PTHKF = ONE
      END IF                  
c-----------------
      NINDX  = 0
c-----------------------------------------------------------------
c     failure when matrix or fiber is broken  ( flag 1 et 2)
c-----------------------------------------------------------------
      IF (IDEL_FM == 1) THEN
C
        DO I=1,NEL
          IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
c            
            IF (UVAR(I,1) == ZERO) THEN 
c             check fiber damage
              IF (SIGNXX(I) > ZERO) THEN
                DAMFT = (SIGNXX(I)/SIGT1)**2 + BETA*(SIGNXY(I)/SIGT12)**2
                DAMFC = ZERO
              ELSE 
                DAMFC = (SIGNXX(I)/SIGC1)**2
                DAMFT = ZERO
              ENDIF 
c
c             check matrix damage
              IF (SIGNYY(I) > ZERO) THEN                                   
                DAMMT = (SIGNYY(I)/SIGT2)**2                             
     .                + (SIGNXY(I)/SIGT12)**2                    
                DAMMC = ZERO        
              ELSE                                                        
                DAMMC = (SIGNYY(I)/(TWO*SIGT12))**2                     
     .                + (SIGNXY(I)/SIGT12)**2                             
     .                + SIGNYY(I)*((SIGC2/(TWO*SIGT12))**2 - ONE)/SIGC2 
                DAMMT = ZERO
              ENDIF 
              DAMMX = MAX(DAMFT,DAMFC,DAMMC,DAMMT)
              DFMAX(I) = MIN(ONE,DAMMX)
              IF (DAMMX >= ONE) THEN
                UVAR(I,1) = TIME
                NINDX = NINDX+1
                INDX(NINDX) = I
              ENDIF
            ENDIF             
c
            IF (UVAR(I,1) > ZERO) THEN 
              DFMAX(I) = ONE
              DMG_SCALE(I) = EXP(-(TIME + TIMESTEP(I) - UVAR(I,1))/TMAX)
c
              IF (DMG_SCALE(I) < EM02) THEN
                FOFF(I) = 0
                TDEL(I) = TIME
                DMG_SCALE(I) = ZERO
              ENDIF
            ENDIF
c         
          ELSEIF (FOFF(I) == 0) THEN
            DMG_SCALE(I) = ZERO  
          ENDIF   
        ENDDO         
      ENDIF       
c-----------------------------------------------------------------
c     failure only on fiber criteria   ( flag 3 et 4)
c-----------------------------------------------------------------
      IF (IDEL_F == 1) THEN
C
        DO I=1,NEL
          IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
c            
            IF (UVAR(I,1) == ZERO) THEN 
c             check fiber damage only
              IF (SIGNXX(I) > ZERO) THEN
                DAMFT = (SIGNXX(I)/SIGT1)**2 
     .                + BETA*(SIGNXY(I)/SIGT12)**2
                DAMFC = ZERO
              ELSE 
                DAMFC = (SIGNXX(I)/SIGC1)**2
                DAMFT = ZERO
              ENDIF 
              DAMMX    = MAX(DAMFT,DAMFC)
              DFMAX(I) = MIN(ONE,DAMMX)
              IF (DAMMX >= ONE) UVAR(I,1) = TIME
            ENDIF 
c
            IF (UVAR(I,1) > ZERO) THEN 
              DFMAX(I) = ONE
              DMG_SCALE(I) = EXP(-(TIME + TIMESTEP(I) - UVAR(I,1))/TMAX)   
c
              IF (DMG_SCALE(I) < EM02) THEN
                NINDX = NINDX+1
                INDX(NINDX) = I
                FOFF(I) = 0
                TDEL(I) = TIME
                DMG_SCALE(I) = ZERO
              ENDIF
            ENDIF
          ELSEIF (FOFF(I) == 0) THEN
            DMG_SCALE(I) = ZERO  
          ENDIF   
        ENDDO         
c
      ENDIF                
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
 2000 FORMAT(1X,'FAILURE (CHANG) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (CHANG) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
