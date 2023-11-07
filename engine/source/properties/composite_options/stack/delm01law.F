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
C damage ladefeze  delamination model ------
Chd|====================================================================
Chd|  DELM01LAW                     source/properties/composite_options/stack/delm01law.F
Chd|-- called by -----------
Chd|        DELAMINATION                  source/properties/composite_options/stack/delamination.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DELM01LAW(FAIL    ,
     1     NEL    ,NUVAR   ,TIME   ,TIMESTEP ,
     2     NGL    ,IPLY    ,
     3     OFF    ,SIGNYZ0 ,SIGNXZ0,SIGNZZ   ,UVAR    ,
     4     OFFI   ,REDUC   ,COUNT  ,SIGNYZ   ,SIGNXZ  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL,  NUVAR,NGL(*),IPLY
      my_real 
     .   TIME,TIMESTEP(*),SIGNZZ(*),
     .   SIGNYZ0(*),SIGNXZ0(*),SIGNYZ(*),SIGNXZ(*),
     .   OFFI(*),COUNT(*),REDUC(*)
      TYPE (FAIL_PARAM_) ,INTENT(IN) :: FAIL
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,J,IDEL,IDEL_L,IFLAG(MVSIZ),INDX(MVSIZ),NINDX,   
     .   JST(MVSIZ),IR,JJ,
     .   NINDX0,INDX0(MVSIZ)        
      my_real 
     .   K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K(MVSIZ),
     .   A(MVSIZ),GAMA1(MVSIZ),GAMA2(MVSIZ),
     .   Y0(MVSIZ),YC(MVSIZ),TMAX(MVSIZ),FAC,
     .   DAM, YD1,YD2,YD3,CC,DELTA,W,YD,SIG, DAM0
C--------------------------------------------------------------
C
      IR = 0
      DO I=1,NEL
        IF (OFF(I)== ZERO) CYCLE
        K1(I)      = FAIL%UPARAM(1)
        K2(I)      = FAIL%UPARAM(2)
        K3(I)      = FAIL%UPARAM(3)
        GAMA1(I)   = FAIL%UPARAM(4)
        GAMA2(I)   = FAIL%UPARAM(5)
        Y0(I)      = FAIL%UPARAM(6)
        YC(I)      = FAIL%UPARAM(7)
        K(I)       = FAIL%UPARAM(8)
        A(I)       = FAIL%UPARAM(9)
        REDUC(I)   = FAIL%UPARAM(13) 
        IR = IR + 1
        JST(IR) = I  
        INDX(I) = 0
      ENDDO      
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF (TIME == ZERO) THEN
        DO JJ=1,IR
          I = JST(JJ)
          UVAR(I,9)  = ONE
        ENDDO   
      ENDIF   
C-------------------------------
C           
        NINDX  = 0 
        NINDX0 = 0 
        DO J =1,IR
          I=JST(J)
          IF (OFF(I) == ONE ) THEN
C------------------------------- 
            IF(UVAR(I,1) < ONE)THEN 
             DAM0 = UVAR(I,1)
             DAM  = DAM0
C
C   direction 33
C
             SIG = HALF*(SIGNZZ(I) + ABS(SIGNZZ(I)))
             YD3 = K3(I)*(ONE - DAM)**2
             YD3 = HALF*SIG*SIG/MAX(YD3, EM20)
             YD3 = MAX(YD3, UVAR(I,2))
             UVAR(I,2) = YD3
C
C   direction 32
C
             SIG = SIGNYZ(I)
             YD2 = K2(I)*(ONE - DAM)**2
             YD2 = HALF*SIG*SIG/MAX(YD2, EM20)
             YD2 = MAX(YD2, UVAR(I,3))
             UVAR(I,3) = YD2             
C
C   direction 13
C
             SIG =SIGNXZ(I) 
             YD1 = K1(I)*(ONE - DAM)**2
             YD1 = HALF*SIG*SIG/MAX(YD1, EM20)
             YD1 = MAX(YD1, UVAR(I,4))
             UVAR(I,4) = YD1                            
C
C  compute new damage
C              
              YD = YD3 + GAMA1(I)*YD1 + GAMA2(I)*YD2
              DELTA = SQRT(YD) - Y0(I)
              DELTA = HALF*(DELTA + ABS(DELTA))
              W = DELTA /(YC(I) - Y0(I))
              CC = W - DAM
              CC = HALF*(CC + ABS(CC))
              FAC = K(I)*TIMESTEP(I)/A(I)
              DAM = DAM + FAC*(ONE - EXP(-A(I)*CC))
              DAM = MIN(ONE, DAM)
              UVAR(I,1)  = DAM              
C
C  reduce stress interply only
C
              IF( SIGNZZ(I) > ZERO )
     .          SIGNZZ(I)  = SIGNZZ(I) *MAX((ONE  - DAM),REDUC(I))
              SIGNYZ0(I) = SIGNYZ0(I)*MAX((ONE  - DAM),REDUC(I))
              SIGNXZ0(I) = SIGNXZ0(I)*MAX((ONE  - DAM),REDUC(I)) 
C
              IF(DAM0 == ZERO .AND. DAM > ZERO) THEN
               NINDX0=NINDX0+1
               INDX0(NINDX0)=I
              ENDIF
C
              IF(DAM == ONE) THEN
               NINDX=NINDX+1
               INDX(NINDX)=I
!!                OFFI(I) = FOUR_OVER_5
                COUNT(I) = COUNT(I) + ONE
                IF(INT(COUNT(I)) == 4)THEN
!!                    OFFI(I) = MIN(OFFI(I), ZERO)
                   WRITE(IOUT, 1300) NGL(I),IPLY,TIME
                   WRITE(ISTDO,1300) NGL(I),IPLY, TIME
               ENDIF
              ENDIF 
             ELSE ! complete damage
!!                SIGNZZ(I)  = ZERO
!!                SIGNYZ0(I) = ZERO
!!                SIGNXZ0(I) = ZERO  
                OFFI(I) = REDUC(I)
             ENDIF 
            ENDIF  
         ENDDO  

        IF(NINDX0 > 0)THEN
          DO J=1,NINDX0
           I = INDX0(J)
#include "lockon.inc"
           WRITE(IOUT, 1100) NGL(I),IPLY,TIME
           WRITE(ISTDO,1100) NGL(I),IPLY, TIME
#include "lockoff.inc"
          END DO
         ENDIF            

        IF(NINDX > 0)THEN
          DO J=1,NINDX
           I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1200) NGL(I),IPLY,TIME
           WRITE(ISTDO,1200) NGL(I),IPLY, TIME
#include "lockoff.inc"
          END DO
         ENDIF            
C--------------------------------------------

 1100 FORMAT(1X,'DAMAGE INITIATION OF SHELL  #',I10,1X,
     . 'INTERPLY ', I10, 1X,
     . 'AT TIME # ',1PE20.13)
 1200 FORMAT(1X,'DELAMINATION OF SHELL  #',I10,1X,
     . 'INTERPLY ', I10, 1X,
     . 'AT TIME # ',1PE20.13)
 1300 FORMAT(1X,'FULL DELAMINATION OF SHELL #',I10,1X,
     . 'INTERPLY', I10,1X,'AT TIME # ',1PE20.13)
      RETURN
      END
