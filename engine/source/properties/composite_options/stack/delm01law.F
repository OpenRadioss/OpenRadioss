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
C damage ladefeze  delamination model ------
Chd|====================================================================
Chd|  DELM01LAW                     source/properties/composite_options/stack/delm01law.F
Chd|-- called by -----------
Chd|        DELAMINATION                  source/properties/composite_options/stack/delamination.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DELM01LAW(
     1     NEL    ,NUPARAM ,NUVAR  ,MFUNC    ,KFUNC   ,
     2     NPF    ,TF      ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    ,IPLY    ,IPM    ,MAT      ,IP      ,
     4     OFF    ,SIGNYZ0 ,SIGNXZ0,SIGNZZ   ,UVAR    ,
     5     OFFI   ,REDUC   ,COUNT  ,SIGNYZ   ,SIGNXZ  )
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
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
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
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NGL(*),IPM(NPROPMI,*),
     .        MAT(*),IP, IPLY
      my_real 
     .   TIME,TIMESTEP(*),UPARAM(*),SIGNZZ(*),
     .   SIGNYZ0(*),SIGNXZ0(*),SIGNYZ(*),SIGNXZ(*),
     .   OFFI(*),COUNT(*),REDUC(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),UEL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,J,IDEL,IDEL_L,IFLAG(MVSIZ),INDX(MVSIZ),IADBUF,NINDX,     
     .   IFAIL,JST(MVSIZ),IR,JJ,IMATLY,
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
C
        IF(OFF(I)==ZERO) CYCLE
C
        IMATLY=MAT(I)  
        IFAIL = IPM(111 +IP,IMATLY)
        IF(IFAIL == 18)THEN  
          IADBUF     =  IPM(114 +IP, IMATLY)
          K1(I)      = UPARAM(IADBUF )
          K2(I)      = UPARAM(IADBUF + 1)
          K3(I)      = UPARAM(IADBUF + 2)
          GAMA1(I)   = UPARAM(IADBUF + 3)
          GAMA2(I)   = UPARAM(IADBUF + 4)
          Y0(I)      = UPARAM(IADBUF + 5)
          YC(I)      = UPARAM(IADBUF + 6)
          K(I)       = UPARAM(IADBUF + 7)
          A(I)       = UPARAM(IADBUF + 8)
          REDUC(I)   = UPARAM(IADBUF + 12) 
          IR = IR + 1
          JST(IR) = I  
        ENDIF 
        INDX(I) = 0
      ENDDO      
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF (TIME == ZERO) THEN
        DO JJ=1,IR
          I = JST(JJ)
          UEL(I) = ZERO
          UVAR(I,9)  = UN
        ENDDO   
      ENDIF   
C-------------------------------
C      
C     OFF = 0.
C-------------------------------
C           
        NINDX  = 0 
        NINDX0 = 0 
        DO J =1,IR
          I=JST(J)
          IF (OFF(I) == UN ) THEN
C------------------------------- 
            IF(UVAR(I,1) < UN)THEN 
             DAM0 = UVAR(I,1)
             DAM  = DAM0
C
C   direction 33
C
             SIG = UNDEMI*(SIGNZZ(I) + ABS(SIGNZZ(I)))
             YD3 = K3(I)*(UN - DAM)**2
             YD3 = UNDEMI*SIG*SIG/MAX(YD3, EM20)
             YD3 = MAX(YD3, UVAR(I,2))
             UVAR(I,2) = YD3
C
C   direction 32
C
             SIG = SIGNYZ(I)
             YD2 = K2(I)*(UN - DAM)**2
             YD2 = UNDEMI*SIG*SIG/MAX(YD2, EM20)
             YD2 = MAX(YD2, UVAR(I,3))
             UVAR(I,3) = YD2             
C
C   direction 13
C
             SIG =SIGNXZ(I) 
             YD1 = K1(I)*(UN - DAM)**2
             YD1 = UNDEMI*SIG*SIG/MAX(YD1, EM20)
             YD1 = MAX(YD1, UVAR(I,4))
             UVAR(I,4) = YD1                            
C
C  compute new damage
C              
              YD = YD3 + GAMA1(I)*YD1 + GAMA2(I)*YD2
              DELTA = SQRT(YD) - Y0(I)
              DELTA = UNDEMI*(DELTA + ABS(DELTA))
              W = DELTA /(YC(I) - Y0(I))
              CC = W - DAM
              CC = UNDEMI*(CC + ABS(CC))
              FAC = K(I)*TIMESTEP(I)/A(I)
              DAM = DAM + FAC*(UN - EXP(-A(I)*CC))
              DAM = MIN(UN, DAM)
              UVAR(I,1)  = DAM              
C
C  reduce stress interply only
C
              IF( SIGNZZ(I) > ZERO )
     .          SIGNZZ(I)  = SIGNZZ(I) *MAX((UN  - DAM),REDUC(I))
              SIGNYZ0(I) = SIGNYZ0(I)*MAX((UN  - DAM),REDUC(I))
              SIGNXZ0(I) = SIGNXZ0(I)*MAX((UN  - DAM),REDUC(I)) 
C
              IF(DAM0 == ZERO .AND. DAM > ZERO) THEN
               NINDX0=NINDX0+1
               INDX0(NINDX0)=I
              ENDIF
C
              IF(DAM == UN) THEN
               NINDX=NINDX+1
               INDX(NINDX)=I
!!                OFFI(I) = QCINQ
                COUNT(I) = COUNT(I) + UN
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
