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
Chd|  FAIL_LADEVEZE                 source/materials/fail/ladeveze/fail_ladeveze.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_LADEVEZE(
     1     NEL     ,NUVAR   ,IP      ,ILAY    ,NPT0    ,
     2     NPF     ,TF      ,TIME    ,TIMESTEP,UPARAM  ,
     3     MAT     ,NGL     ,OFF     ,NOFF    ,SIGNXX  ,
     4     SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     5     UVAR    ,IPM     ,NUPARAM ,DFMAX   ,TDELE   )
C-----------------------------------------------
C    Ladeveze damage delamination model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
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
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF    | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "com01_c.inc"
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM, NUVAR,ILAY,IP,NPT0
      INTEGER NGL(NEL),IPM(NPROPMI,*),MAT(NEL)
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER NOFF(NEL)
      my_real UVAR(NEL,NUVAR), OFF(NEL),DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX0(MVSIZ),IFLAG,INDX(MVSIZ)
      INTEGER I,J,JJ,IADBUF,NINDX,NINDX0,
     .        IFAIL,IMATLY
      my_real 
     .   K1,K2,K3,K,
     .   A,GAMA1,GAMA2,
     .   Y0,YC,TMAX,FAC,
     .   DAM, YD1,YD2,YD3,CC,DELTA,W,YD,SIG
C--------------------------------------------------------------
      K1      = UPARAM(1)
      K2      = UPARAM(2)
      K3      = UPARAM(3)
      GAMA1   = UPARAM(4)
      GAMA2   = UPARAM(5)
      Y0      = UPARAM(6)
      YC      = UPARAM(7)
      K       = UPARAM(8)
      A       = UPARAM(9)
      TMAX    = UPARAM(10)
      IFLAG   = INT(UPARAM(12))    
      INDX(1:MVSIZ) = 0
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(ISIGI == ZERO)THEN 
        IF(TIME.EQ.ZERO)THEN
         DO I=1,NEL
          UVAR(I,12)  = UN
         ENDDO   
        ENDIF    
      ENDIF   
C-----------------------------------------------
      DO I=1,NEL
        IF(OFF(I).LT.EM01) OFF(I)=ZERO
        IF(OFF(I).LT.UN) OFF(I)=OFF(I)*QCINQ
      END DO
C           
        NINDX=0 
        INDX = 0  
        NINDX0=0 
        INDX0 = 0
C
                   
        DO I=1,NEL
         IF (OFF(I) == UN )THEN
           IF(IFLAG == 1) THEN
C-------------------------------     
C     OFF = 0. one layer fiber or matrix criteria is reached
C-------------------------------                  
            IF(UVAR(I,12) < UN)THEN 
              UVAR(I,12)= EXP(-(TIME - UVAR(I,11))/TMAX)  
              IF(UVAR(I,12) < EM02) UVAR(I,12) = ZERO
              SIGNXX(I) = UVAR(I,5)*UVAR(I,12)
              SIGNYY(I) = UVAR(I,6)*UVAR(I,12)
              SIGNZZ(I) = UVAR(I,7)*UVAR(I,12)
              SIGNXY(I) = UVAR(I,8)*UVAR(I,12)
              SIGNYZ(I) = UVAR(I,9)*UVAR(I,12)
              SIGNZX(I) = UVAR(I,10)*UVAR(I,12) 
              IF(UVAR(I,12) == ZERO ) THEN
                OFF(I)=QCINQ 
                NINDX=NINDX+1
                INDX(NINDX)=I
                TDELE(I) = TIME  
              ENDIF      
            ELSE                  
C
C   direction 33
C
             DAM = UVAR(I,1)
             SIG = UNDEMI*(SIGNZZ(I) + ABS(SIGNZZ(I)))
             YD3 = K3*(UN - DAM)**2
             YD3 = UNDEMI*SIG*SIG/MAX(YD3, EM20)
             YD3 = MAX(YD3, UVAR(I,2))
             UVAR(I,2) = YD3
C
C   direction 32
C
             SIG = SIGNYZ(I) 
             YD2 = K2*(UN - DAM)**2
             YD2 = UNDEMI*SIG*SIG/MAX(YD2, EM20)
             YD2 = MAX(YD2, UVAR(I,3))
             UVAR(I,3) = YD2          
C
C   direction 13
C
             SIG = SIGNZX(I)         
             YD1 = K1*(UN - DAM)**2
             YD1 = UNDEMI*SIG*SIG/MAX(YD1, EM20)
             YD1 = MAX(YD1, UVAR(I,4))
             UVAR(I,4) = YD1                            
C
C  compute new damage
C              
              YD = YD3 + GAMA1*YD1 + GAMA2*YD2
              DELTA = SQRT(YD) - Y0
              DELTA = UNDEMI*(DELTA + ABS(DELTA))
              W = DELTA /(YC - Y0)
              CC = W - DAM
              CC = UNDEMI*(CC + ABS(CC))
              FAC = K*TIMESTEP/A
              DAM = DAM + FAC*(UN - EXP(-A*CC))
              UVAR(I,1)  = DAM
              IF( DAM >= 1 )THEN 
               UVAR(I,5) = SIGNXX(I)
               UVAR(I,6) = SIGNYY(I)
               UVAR(I,7) = SIGNZZ(I)
               UVAR(I,8) = SIGNXY(I)
               UVAR(I,9) = SIGNYZ(I)
               UVAR(I,10) = SIGNZX(I)
               UVAR(I,11) = TIME
               UVAR(I,12) = QCINQ
              ENDIF
             ENDIF
C             
            ELSEIF(IFLAG == 2) THEN    ! iflag = 2 
C-------------------------------     
C     OFF = 0. all layer fiber or matrix criteria is reached
C      is not actived this flag
C-------------------------------              
               IF(UVAR(I,12) == ZERO )THEN
                SIGNXX(I) = ZERO
                SIGNYY(I) = ZERO
                SIGNZZ(I) = ZERO
                SIGNXY(I) = ZERO
                SIGNZX(I) = ZERO
                SIGNYZ(I) = ZERO
               ELSE IF(UVAR(I,12) < UN) THEN
                UVAR(I,12)= EXP(-(TIME - UVAR(I,11))/TMAX) 
                IF(UVAR(I,12) < EM02)UVAR(I,12) = ZERO
                SIGNXX(I) = UVAR(I,5)*UVAR(I,12)
                SIGNYY(I) = UVAR(I,6)*UVAR(I,12)
                SIGNZZ(I) = UVAR(I,7)*UVAR(I,12)
                SIGNXY(I) = UVAR(I,8)*UVAR(I,12)
                SIGNYZ(I) = UVAR(I,9)*UVAR(I,12)
                SIGNZX(I) = UVAR(I,10)*UVAR(I,12)
                IF( UVAR(I,12) == ZERO )THEN
                   NOFF(I) = NOFF(I) + 1
                   IF(NOFF(I) == NPT0 .OR. NPT0 == 1) THEN
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    OFF(I) = QCINQ
                    TDELE(I) = TIME  
                   ENDIF 
                ENDIF            
               ELSE
C
C   direction 33
C
             DAM = UVAR(I,1)
             SIG = UNDEMI*(SIGNZZ(I) + ABS(SIGNZZ(I)))
             YD3 = K3*(UN - DAM)**2
             YD3 = UNDEMI*SIG*SIG/MAX(YD3, EM20)
             YD3 = MAX(YD3, UVAR(I,2))
             UVAR(I,2) = YD3
C
C   direction 32
C
             SIG =SIGNYZ(I)
             YD2 = K2*(UN - DAM)**2
             YD2 = UNDEMI*SIG*SIG/MAX(YD2, EM20)
             YD2 = MAX(YD2, UVAR(I,3))
             UVAR(I,3) = YD2             
C
C   direction 13
C
             SIG = SIGNZX(I) 
             YD1 = K1*(UN - DAM)**2
             YD1 = UNDEMI*SIG*SIG/MAX(YD1, EM20)
             YD1 = MAX(YD1, UVAR(I,4))
             UVAR(I,4) = YD1                 
C
C  compute new damage
C                 
                YD = YD3 + GAMA1*YD1 + GAMA2*YD2
                DELTA = SQRT(YD) - Y0
                DELTA = UNDEMI*(DELTA + ABS(DELTA))
                W = DELTA /(YC - Y0)
                CC = W - DAM
                CC = UNDEMI*(CC + ABS(CC))
                FAC = K*TIMESTEP/A
                DAM = DAM + FAC*(UN - EXP(-A*CC))
                UVAR(I,1)  = DAM
                IF(DAM >= UN  )THEN                             
                   UVAR(I,5) =  SIGNXX(I)                                  
                   UVAR(I,6) =  SIGNYY(I)             
                   UVAR(I,7) =  SIGNZZ(I)                                 
                   UVAR(I,8) =  SIGNXY(I)                                  
                   UVAR(I,9) =  SIGNYZ(I)                                  
                   UVAR(I,10) = SIGNZX(I)                                  
                   UVAR(I,11) = TIME                                       
                   UVAR(I,12) = QCINQ
                   NINDX0= NINDX0+1
                   INDX0(NINDX0)=I                
                 ENDIF
               ENDIF
             ELSEIF(IFLAG == 3) THEN
C-------------------------------     
C     OFF = 0. all layer fiber or matrix criteria is reached
C-------------------------------              
               IF(UVAR(I,12) == ZERO )THEN
                SIGNZZ(I) = ZERO
                SIGNZX(I) = ZERO
                SIGNYZ(I) = ZERO
               ELSE IF(UVAR(I,12) < UN) THEN
                UVAR(I,12)= EXP(-(TIME - UVAR(I,11))/TMAX) 
                IF(UVAR(I,12) < EM02)UVAR(I,12) = ZERO
                SIGNZZ(I) = UVAR(I,7)*UVAR(I,12)
                SIGNYZ(I) = UVAR(I,9)*UVAR(I,12)
                SIGNZX(I) = UVAR(I,10)*UVAR(I,12)
                IF( UVAR(I,12) == ZERO )THEN
c                  NOFF(I) = NOFF(I) + 1                     
                    NINDX=NINDX+1                             
                    INDX(NINDX)=I                             
                  IF (INT(NOFF(I))==NPT0 .OR. NPT0 == 1)THEN
c                    NINDX=NINDX+1                             
c                    INDX(NINDX)=I                             
c                    OFF(I) = QCINQ  
                  ENDIF            
                ENDIF            
c
               ELSE
C
C   direction 33
C
                DAM = UVAR(I,1)
                SIG = UNDEMI*(SIGNZZ(I) + ABS(SIGNZZ(I)))
                YD3 = K3*(UN - DAM)**2
                YD3 = UNDEMI*SIG*SIG/MAX(YD3, EM20)
                YD3 = MAX(YD3, UVAR(I,2))
                UVAR(I,2) = YD3
C
C   direction 32
C
                SIG = SIGNYZ(I) 
                YD2 = K2*(UN - DAM)**2
                YD2 = UNDEMI*SIG*SIG/MAX(YD2, EM20)
                YD2 = MAX(YD2, UVAR(I,3))
                UVAR(I,3) = YD2             
C
C   direction 13
C
                SIG = SIGNZX(I) 
                YD1 = K1*(UN - DAM)**2
                YD1 = UNDEMI*SIG*SIG/MAX(YD1, EM20)
                YD1 = MAX(YD1, UVAR(I,4))
                UVAR(I,4) = YD1                 
C
C  compute new damage
C                 
                YD = YD3 + GAMA1*YD1 + GAMA2*YD2
                DELTA = SQRT(YD) - Y0
                DELTA = UNDEMI*(DELTA + ABS(DELTA))
                W = DELTA /(YC - Y0)
                CC = W - DAM
                CC = UNDEMI*(CC + ABS(CC))
                FAC = K*TIMESTEP/A
                DAM = DAM + FAC*(UN - EXP(-A*CC))
                UVAR(I,1)  = DAM
                IF(DAM >= UN  )THEN                                    
                   UVAR(I,7)  = SIGNZZ(I)                                  
                   UVAR(I,9)  = SIGNYZ(I)                                  
                   UVAR(I,10) = SIGNZX(I)                                  
                   UVAR(I,11) = TIME                                       
                   UVAR(I,12) = QCINQ   
                 ENDIF
               ENDIF
             ENDIF  !  iflag choice 
           ENDIF    ! OFF       
         ENDDO  
C
C-----Maximum Damage storing for output : 0 < DFMAX < 1
      DO I=1,NEL
        DFMAX(I) = MIN(UN,MAX(DFMAX(I),UVAR(I,1)))
      ENDDO  
C-------------------------------------------- 
      IF(NINDX > 0)THEN
        DO J=1,NINDX
           I = INDX(J)
           IF(IFLAG == 1 .OR. IFLAG == 2) THEN
#include "lockon.inc"
           WRITE(IOUT, 1200) NGL(I),TIME
           WRITE(ISTDO,1200) NGL(I),TIME
#include "lockoff.inc"
          ELSEIF(IFLAG == 3) THEN
#include "lockon.inc"
           WRITE(IOUT, 3200) NGL(I),TIME
           WRITE(ISTDO,3200) NGL(I),TIME
#include "lockoff.inc" 
          ENDIF
        END DO
      ENDIF      
c
      IF(NINDX0 > 0)THEN
        DO J=1,NINDX0
           I = INDX0(J)
#include "lockon.inc"
           WRITE(IOUT, 1100) NGL(I),ILAY,TIME
           WRITE(ISTDO,1100) NGL(I),ILAY,TIME
#include "lockoff.inc"
        END DO
      ENDIF               
C-------------------------------------------- 
 1200 FORMAT(1X,'DELETE SOLID ELEMENT (LADEVEZE MODEL) #',I10,1X,
     .'AT TIME # ',1PE20.13)     
 1100 FORMAT(1X,'FAILURE ELEMENT #',I10,1X,
     .'IP #',I10,1X, 'AT TIME #:',1PE20.13)
 3200 FORMAT(1X,'DELAMINATION OF ELEMENT (LADEVEZE MODEL) #',I10,1X,
     .'AT TIME # ',1PE20.13)         
C-------------------------------------------- 
      RETURN
      END
