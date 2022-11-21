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
Chd|  FAIL_HASHIN_S                 source/materials/fail/hashin/fail_hashin_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_HASHIN_S(
     1     NEL     ,NUVAR   ,IP      ,ILAY    ,NPT0    ,
     2     NPF     ,TF      ,TIME    ,TIMESTEP,UPARAM  ,
     3     MAT     ,NGL     ,OFF     ,NOFF    ,SIGNXX  , 
     4     SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     5     UVAR    ,IPM     ,NUPARAM ,DFMAX   ,TDELE   ,
     6     EPSP )
C-----------------------------------------------
C Hashin model ------
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
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "com01_c.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
#include  "vect01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,ILAY,NPT0,NUVAR
      INTEGER NGL(NEL),IPM(NPROPMI,*),MAT(NEL)
      my_real 
     .   TIME,TIMESTEP(NEL),UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER NOFF(NEL)
      my_real UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,IDEL,IDEL_L,IFLAG,INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IFAIL,JJ,KK,IR,IMATLY,
     .        IMODEL,IUNIDIR,IFABRIC,IP,INDX0(MVSIZ),NINDX0,
     .        MODE(MVSIZ,7),IMODE,FAILNPT,TMOD
      my_real 
     .   SIGT1,SIGT2,SIGT3,
     .   SIGC1,SIGC2,CSIG,FSIG12,
     .   MSIG12(MVSIZ),MSIG23(MVSIZ),MSIG13(MVSIZ),ANGLE,
     .   SDEL,TMAX,RATIO,FAC,S12,S23,
     .   F1,F2,F3,F4,F5,SIG,P,F6,F7,EPSPREF,FILT,ALPHA,FS(NEL),
     .   TELEM,K2M,INSTF,K0,K1,K2,K3,ZEP669741,DTINV,KM,
     .   FSIG12UP
C--------------------------------------------------------------
CC Constant
      ZEP669741 = SIX*EM01 + SIX*EM02 + NINE*EM3 + SEVEN*EM04 + FOUR*EM05 + EM06
C
      IMODEL = INT(UPARAM(1))
      SIGT1 = UPARAM(2)
      SIGT2 = UPARAM(3)
      SIGT3 = UPARAM(4)
      SIGC1   = UPARAM(5)
      SIGC2   = UPARAM(6)
      CSIG    = UPARAM(7)
      FSIG12  = UPARAM(8)
      MSIG12(1:MVSIZ)  = UPARAM(9)
      MSIG13(1:MVSIZ)  = UPARAM(10)         
      MSIG23(1:MVSIZ)  = UPARAM(11)
      ANGLE   = UPARAM(12)
      SDEL    = UPARAM(13)
      TMAX    = UPARAM(14) 
      IFLAG   = INT(UPARAM(16))      
      RATIO   = UPARAM(17)
      TMOD     = INT(UPARAM(19))
      EPSPREF   = UPARAM(20)
      FILT     = UPARAM(21)
      KM      = UPARAM(22)
      IF (IFLAG == 3) RATIO = ONE - ONE/ NPT0 
      DO I=1,NEL
         INDX(I) = 0
         INDX0(I) = 0
      ENDDO
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(ISIGI == 0) THEN
        IF(TIME == ZERO)THEN
         DO I=1,NEL
          UVAR(I,8)  = ONE
         ENDDO   
        ENDIF    
      ENDIF       
C-----------------------------------------------
      IUNIDIR = 0
      IFABRIC = 0
      IF(IMODEL == 1)THEN
         IUNIDIR = 1
      ELSEIF(IMODEL == 2)THEN
         IFABRIC = 1
      ENDIF
c        ELSEIF(IFLAG(I) == 3)THEN
c   
      DO I=1,NEL
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < ONE)   OFF(I)=OFF(I)*FOUR_OVER_5
C        
        MODE(I,1) = 0
        MODE(I,2) = 0
        MODE(I,3) = 0
        MODE(I,4) = 0
        MODE(I,5) = 0
        MODE(I,6) = 0
        MODE(I,7) = 0
      END DO
C
C initialisation 
       
C-------------------------------
C      
C     OFF = 0. si la matrice ou la fibre a rompu
C-------------------------------
C           
       IF(IUNIDIR > 0)THEN   
        NINDX=0 
        INDX = 0
        INDX0=0 
        NINDX0 = 0
c
        DO I=1,NEL
         F1 =ZERO
         F2 =ZERO
         F3 =ZERO
         F4 = ZERO
         F5 = ZERO
c alpha = number of cycles  
          DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)    
          ALPHA = INT(MAX(ONE,FILT*DTINV))
c alpha coef de moyenne mobile exponentielle
          ALPHA = TWO/(ALPHA + ONE)
          FS(I)=UVAR(I,10) 
c smoothed strain rate 
          EPSP(I)=(ONE - ALPHA)*UVAR(I,11)  +  ALPHA*EPSP(I) 
          UVAR(I,11)=EPSP(I)
         IF(OFF(I) == ONE .AND. IMODEL == 1)THEN
C         
           IF(IFLAG == 1) THEN
C-------------------------------     
C           OFF = 0 when one layer fiber or matrix criteria is reached
C-------------------------------                  
            IF (UVAR(I,8) < ONE)THEN 
              IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  UVAR(I,8)=UVAR(I,8)*EXP(-TIMESTEP(I)/TELEM)
              ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I,12)
                 UVAR(I,8) = EXP(-(TIME - UVAR(I,7))/TELEM)  
              ELSE
                  UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TMAX)  
              ENDIF   
              IF (UVAR(I,8) < EM02) UVAR(I,8) = ZERO
              SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
              SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
              SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
              SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
              SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
              SIGNZX(I) = UVAR(I,6)*UVAR(I,8) 
              IF (UVAR(I,8) == ZERO ) THEN
                OFF(I)=FOUR_OVER_5 
                NINDX=NINDX+1
                INDX(NINDX)=I
                TDELE(I) = TIME  
C                KK = 4711
              ENDIF      
            ELSE                  
C
C   fiber criteria
C
             SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
             F1=(SIG/SIGT1)**2 
     .                  + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
             SIG =  -HALF*(SIGNYY(I) + SIGNZZ(I))
             SIG = - SIGNXX(I) + HALF*(SIG + ABS(SIG))
             SIG = HALF*(SIG + ABS(SIG))
             F2 = (SIG /SIGC1)**2
             P = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
             IF(P > 0) F3 = (P/CSIG)**2
C
C  matrice criteria
C              
              FAC=TAN(ANGLE)
              S12 = MSIG12(I)
              S23 = MSIG23(I)
              IF(SIGNYY(I) < ZERO) THEN
                S12 = MSIG12(I) -   SIGNYY(I)*FAC
                S23 = MSIG23(I) -   SIGNYY(I)*FAC
              ENDIF           
C                
              SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
              F4=(SIG/SIGT2)**2 
     .            + (SIGNYZ(I)/S23)**2 + (SIGNXY(I)/S12)**2
C
              IF(SIGNZZ(I) < ZERO) THEN
                MSIG13(I) = MSIG13(I) - SIGNZZ(I)*FAC
                MSIG23(I) = MSIG23(I) - SIGNZZ(I)*FAC
              ENDIF 
              SIG = HALF*(SIGNZZ(I) + ABS(SIGNZZ(I)))
         
              F5= (SIG/SIGT3)**2 +
     .            (SIGNYZ(I)/MSIG23(I))**2 + (SIGNZX(I)/MSIG13(I))**2
              F5 = SDEL*SDEL*F5
C
              DFMAX(I)  = MIN(ONE,MAX(DFMAX(I),F1,F2,F3,F4,F5))
              UVAR(I,10) = MAX(F1,F2,F3,F4,F5)
C
              IF(F1 >= ONE .OR. F2 >= ONE .OR. F3 >= ONE .OR.
     .          F4 >= ONE .OR. F5 >= ONE )THEN 
               UVAR(I,1) = SIGNXX(I)
               UVAR(I,2) = SIGNYY(I)
               UVAR(I,3) = SIGNZZ(I)
               UVAR(I,4) = SIGNXY(I)
               UVAR(I,5) = SIGNYZ(I)
               UVAR(I,6) = SIGNZX(I)
               UVAR(I,7) = TIME 
               IF(TMOD == 0) THEN
                   UVAR(I,8) = FOUR_OVER_5
               ELSE
                   UVAR(I,8) = ZEP99 
               ENDIF 
               FAC = HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),
     .                    ABS(SIGNZZ(I)),ABS(SIGNXY(I)),ABS(SIGNYZ(I)),
     .                    ABS(SIGNZX(I)))
               K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX
               K1 =  ZEP9*EPSP(I)
               K2 =  ZEP669741*FAC*EPSP(I)**2
               K2M = ZEP669741*FAC*KM*EPSPREF**2
               K2 = MAX(K2M,K2,EM20)
               TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2 
               UVAR(I,12) = TELEM
               IF (F1>=ONE) THEN
                 MODE(I,1) = 1
                 KK = 1
               ENDIF
               IF (F2>=ONE) THEN
                 MODE(I,2) = 2
                 KK = 2
               ENDIF
               IF (F3>=ONE) THEN
                 MODE(I,3) = 3
                 KK = 3
               ENDIF
               IF (F4>=ONE) THEN
                 MODE(I,4) = 4
                 KK = 4
               ENDIF
               IF (F5>=ONE) THEN
                 MODE(I,5) = 5
                 KK = 5
               ENDIF
               UVAR(I,9)   = KK 
               NINDX0=NINDX0+1
               INDX0(NINDX0)=I
              ENDIF
             ENDIF
C             
            ELSE    ! iflag/= 1 
C-------------------------------     
C             OFF = 0 when all layer fiber or matrix criteria is reached
C-------------------------------              
              IF (UVAR(I,8) == ZERO )THEN
                SIGNXX(I) = ZERO
                SIGNYY(I) = ZERO
                SIGNZZ(I) = ZERO
                SIGNXY(I) = ZERO
                SIGNZX(I) = ZERO
                SIGNYZ(I) = ZERO
               ELSEIF (UVAR(I,8) < ONE) THEN
                IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  UVAR(I,8)=UVAR(I,8)*EXP(-TIMESTEP(I)/TELEM)
                ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I,12)
                 UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TELEM )
                ELSE
                  UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TMAX)  
               ENDIF 
                IF(UVAR(I,8) < EM02)UVAR(I,8) = ZERO
                SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
                SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
                SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
                SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
                SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
                SIGNZX(I) = UVAR(I,6)*UVAR(I,8)
                IF (UVAR(I,8) == ZERO )THEN
                    NOFF(I) = NOFF(I) + 1
                    FAILNPT = INT(NPT0*RATIO)
                   IF (NOFF(I) >= FAILNPT  .OR. NPT0 == 1) THEN
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    OFF(I) = FOUR_OVER_5
                    TDELE(I) = TIME  
                   ENDIF 
                ENDIF            
              ELSE
C
C   fiber criteria
C
             SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
             F1=(SIG/SIGT1)**2 
     .                  + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
             SIG =  -HALF*(SIGNYY(I) + SIGNZZ(I))
             SIG = - SIGNXX(I) + HALF*(SIG + ABS(SIG))
             SIG = HALF*(SIG + ABS(SIG))
             F2 = (SIG /SIGC1)**2
             P = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
             IF(P > 0) F3 = (P/CSIG)**2            
C
C  matrice criteria
C              
              FAC=TAN(ANGLE)
              S12 = MSIG12(I)
              S23 = MSIG23(I)
              IF(SIGNYY(I) < ZERO) THEN
                S12 = MSIG12(I) -   SIGNYY(I)*FAC
                S23 = MSIG23(I) -   SIGNYY(I)*FAC
              ENDIF           
C                
              SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
              F4=(SIG/SIGT2)**2 
     .            + (SIGNYZ(I)/S23)**2 + (SIGNXY(I)/S12)**2
C
              IF(SIGNZZ(I) < ZERO) THEN
                MSIG13(I) = MSIG13(I) - SIGNZZ(I)*FAC
                MSIG23(I) = MSIG23(I) - SIGNZZ(I)*FAC
              ENDIF 
              SIG = HALF*(SIGNZZ(I) + ABS(SIGNZZ(I)))
              F5= (SIG/SIGT3)**2 +
     .            (SIGNYZ(I)/MSIG23(I))**2 + (SIGNZX(I)/MSIG13(I))**2
              F5 = SDEL*SDEL*F5     
C
                DFMAX(I)  = MIN(ONE,MAX(DFMAX(I),F1,F2,F3,F4,F5))
C   
                UVAR(I,10) = MAX(F1,F2,F3,F4,F5)
                IF(F1 >= ONE .OR. F2 >= ONE .OR. F3 >= ONE .OR.               
     .           F4 >= ONE .OR. F5 >= ONE )THEN                             
                   UVAR(I,1) = SIGNXX(I)                                  
                   UVAR(I,2) = SIGNYY(I)                                  
                   UVAR(I,3) = SIGNZZ(I)                                  
                   UVAR(I,4) = SIGNXY(I)                                  
                   UVAR(I,5) = SIGNYZ(I)                                  
                   UVAR(I,6) = SIGNZX(I)                                  
                   UVAR(I,7) = TIME                                       
                    IF(TMOD == 0) THEN
                       UVAR(I,8) = FOUR_OVER_5
                    ELSE
                       UVAR(I,8) = ZEP99 
                    ENDIF  
                     FAC = HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),
     .                    ABS(SIGNZZ(I)),ABS(SIGNXY(I)),ABS(SIGNYZ(I)),
     .                    ABS(SIGNZX(I)))
                    K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX
                    K1 =  ZEP9*EPSP(I)
                    K2 =  ZEP669741*FAC*EPSP(I)**2
                    K2M = ZEP669741*FAC*KM*EPSPREF**2
                    K2  = MAX(K2M,K2,EM20)
                    TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2 
                    UVAR(I,12) = TELEM                   
                    NINDX0= NINDX0+1
                    INDX0(NINDX0)=I
                    IF (F1>=ONE) THEN
                      MODE(I,1) = 1              
                      KK =1
                    ENDIF
                    IF (F2>=ONE) THEN
                      MODE(I,2) = 1              
                      KK =2
                    ENDIF
                    IF (F3>=ONE) THEN
                      MODE(I,3) = 1              
                      KK =3
                    ENDIF
                    IF (F4>=ONE) THEN
                      MODE(I,4) = 1              
                      KK =4
                    ENDIF
                    IF (F5>=ONE) THEN
                      MODE(I,5) = 1              
                      KK =5
                    ENDIF
                    UVAR(I,9)   = KK 
                  ENDIF
              ENDIF
c 
             ENDIF  !  iflag choice            
           ENDIF
                   
         ENDDO  
        ENDIF 
C ----------------------------------------------      
C     fabric layer model
C     OFF = 0. si la matrice ou la fibre a rompu
C-------------------------------  
      IF(IFABRIC > 0)THEN
        INDX = 0 
        NINDX = 0
        INDX0 = 0 
        NINDX0 = 0
        DO I=1,NEL
          F1 = ZERO
          F2 = ZERO
          F3 = ZERO
          F4 = ZERO
          F5 = ZERO
          F6 = ZERO
          F7 = ZERO 
c alpha = number of cycles                                 
          DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
          ALPHA = INT(MAX(ONE,FILT*DTINV))
c alpha coef de moyenne mobile exponentielle
          ALPHA = TWO/(ALPHA + ONE)
          FS(I)=UVAR(I,10) 
c smoothed strain rate 
          EPSP(I)=(ONE - ALPHA)*UVAR(I,11)  +  ALPHA*EPSP(I) 
          UVAR(I,11)=EPSP(I)
          IF(OFF(I) == ONE .AND. IMODEL /= 1)THEN     
            IF(IFLAG == 1) THEN
              IF(UVAR(I,8) < ONE)THEN 
               IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  UVAR(I,8)=UVAR(I,8)*EXP(-TIMESTEP(I)/TELEM)
               ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I,12)
                 UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TELEM)
                ELSE
                   UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TMAX)  
                ENDIF    
                 IF(UVAR(I,8) < EM02)UVAR(I,8) = ZERO
                 SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
                 SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
                 SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
                 SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
                 SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
                 SIGNZX(I) = UVAR(I,6)*UVAR(I,8)
                 IF(UVAR(I,8) == ZERO )THEN
                    OFF(I)=FOUR_OVER_5
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    TDELE(I) = TIME  
                 ENDIF   
              ELSE                  
C
C   Fill and warp directions
C
                SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
                F1=(SIG/SIGT1)**2 
     .                     + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
                SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
                FSIG12UP = FSIG12*SIGT2/SIGT1
                F2=(SIG/SIGT2)**2 
     .                   + (SIGNXY(I)**2 + SIGNYZ(I)**2)/FSIG12UP**2  
C                              
                SIG = HALF*(-SIGNZZ(I) + ABS(SIGNZZ(I)))
                IF(-SIGNXX(I) + SIG > ZERO) 
     .                       F3 = ((-SIGNXX(I) + SIG)/SIGC1)**2     
                IF(-SIGNYY(I) + SIG > ZERO) 
     .                       F4 = ((-SIGNYY(I) + SIG)/SIGC2)**2
                P = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                IF(P > ZERO) F5 = (P/CSIG)**2
C                F5 = (SIGNXY(I)/MSIG12(I))**2
                F6 = (SIGNXY(I)/MSIG12(I))**2
                SIG = HALF*(SIGNZZ(I) + ABS(SIGNZZ(I)))
                IF(SIGNZZ(I) < ZERO) THEN
                  FAC=TAN(ANGLE)
                  MSIG13(I) = MSIG13(I) - SIGNZZ(I)*FAC
                  MSIG23(I) = MSIG23(I) - SIGNZZ(I)*FAC
                ENDIF 
C                F6 = (SIG/SIGT3(I))**2 + 
C     .            (SIGNYZ(I)/MSIG23(I))**2 + (SIGNZX(I)/MSIG13(I))**2
CC     
C                F6 = SDEL(I)*SDEL(I)*F6
                F7 = (SIG/SIGT3)**2 + 
     .            (SIGNYZ(I)/MSIG23(I))**2 + (SIGNZX(I)/MSIG13(I))**2
C     
                F7 = SDEL*SDEL*F7
C  
C
                DFMAX(I)  = MIN(ONE,MAX(DFMAX(I),F1,F2,F3,F4,F5,F6,F7))
C          
                UVAR(I,10) = MAX(F1,F2,F3,F4,F5,F6,F7)
                IF(F1 >= ONE .OR. F2 >= ONE .OR. F3 >= ONE .OR.
     .             F4 >= ONE .OR. F5 >= ONE .OR. F6 >= ONE .OR. 
     .             F7 >= ONE )THEN 
                   UVAR(I,1) = SIGNXX(I)
                   UVAR(I,2) = SIGNYY(I)
                   UVAR(I,3) = SIGNZZ(I)
                   UVAR(I,4) = SIGNXY(I)
                   UVAR(I,5) = SIGNYZ(I)
                   UVAR(I,6) = SIGNZX(I)
                   UVAR(I,7) = TIME
                    IF(TMOD == 0) THEN
                       UVAR(I,8) = FOUR_OVER_5
                    ELSE
                       UVAR(I,8) = ZEP99 
                    ENDIF 
                   FAC = HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),              
     .                        ABS(SIGNZZ(I)),ABS(SIGNXY(I)),ABS(SIGNYZ(I)),   
     .                        ABS(SIGNZX(I)))                                 
                   K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX 
                   K1 =  ZEP9*EPSP(I)                                         
                   K2 =  ZEP669741*FAC*EPSP(I)**2                      
                   K2M = ZEP669741*FAC*KM*EPSPREF**2 
                   K2 = MAX(K2M,K2,EM20)                         
                   TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2                      
                   UVAR(I,12) = TELEM                                         
                   IF (F1>=ONE) THEN 
                     MODE(I,1) = 1    
                     KK = 1           
                   ENDIF            

                   IF (F2>=ONE) THEN 
                     MODE(I,2) = 1    
                     KK = 2           
                   ENDIF            
                   IF (F3>=ONE) THEN 
                     MODE(I,3) = 1    
                     KK = 3           
                   ENDIF            
                   IF (F4>=ONE) THEN 
                     MODE(I,4) = 1    
                     KK = 4           
                   ENDIF            
                   IF (F5>=ONE) THEN 
                     MODE(I,5) = 1    
                     KK = 5           
                   ENDIF            
                   IF (F6>=ONE) THEN 
                     MODE(I,6) = 1    
                     KK = 5           
                   ENDIF            
                   IF (F7>=ONE) THEN 
                     MODE(I,7) = 1    
                     KK = 7           
                   ENDIF            
                    UVAR(I,9)   = KK 
C                  
                  NINDX0=NINDX0+1
                  INDX0(NINDX0)=I
                ENDIF
              ENDIF
             ELSE   ! iflag/= 1 
C-------------------------------     
C     OFF = 0. all layer fiber or matrix criteria is reateched
C-------------------------------
              IF(UVAR(I,8) == ZERO) THEN                                    
                SIGNXX(I) = ZERO
                SIGNYY(I) = ZERO
                SIGNZZ(I) = ZERO
                SIGNXY(I) = ZERO
                SIGNYZ(I) = ZERO
                SIGNZX(I) = ZERO
              ELSEIF(UVAR(I,8) < ONE)THEN 
                IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  UVAR(I,8)=UVAR(I,8)*EXP(-TIMESTEP(I)/TELEM)
               ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I,12) 
                 UVAR(I,8)=EXP(-(TIME - UVAR(I,7))/TELEM)  
                ELSE
                   UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TMAX)  
                ENDIF    
                IF(UVAR(I,8) < EM02)UVAR(I,8) = ZERO
                SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
                SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
                SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
                SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
                SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
                SIGNZX(I) = UVAR(I,6)*UVAR(I,8)
                 IF(UVAR(I,8) == ZERO )THEN
                    NOFF(I) = NOFF(I) + 1
                    FAILNPT = INT((NPT0*RATIO))
                    IF( NPT0 == 1 .OR. NOFF(I) >= FAILNPT ) THEN
                       OFF(I) = FOUR_OVER_5
                       NINDX=NINDX+1
                       INDX(NINDX)=I
                       TDELE(I) = TIME  
                    ENDIF
                ENDIF         
              ELSE                  
C
C   Fill and warp directions 
C
                SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
                F1=(SIG/SIGT1)**2 
     .                   + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
                  
                SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
                FSIG12UP = FSIG12*SIGT2/SIGT1
                F2=(SIG/SIGT2)**2 
     .                   + (SIGNXY(I)**2 + SIGNYZ(I)**2)/FSIG12UP**2  
C                             
                SIG = HALF*(-SIGNZZ(I) + ABS(SIGNZZ(I)))
                IF(-SIGNXX(I) + SIG > ZERO) 
     .                       F3 = ((-SIGNXX(I) + SIG)/SIGC1)**2
                IF(-SIGNYY(I) + SIG > ZERO) 
     .                       F4 = ((-SIGNYY(I) + SIG)/SIGC2)**2
                P = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                IF(P > ZERO) F5 = (P/CSIG)**2
                F6 = (SIGNXY(I)/MSIG12(I))**2
                SIG = HALF*(SIGNZZ(I) + ABS(SIGNZZ(I)))
                IF(SIGNZZ(I) < ZERO) THEN
                  FAC=TAN(ANGLE)
                  MSIG13(I) = MSIG13(I) - SIGNZZ(I)*FAC
                  MSIG23(I) = MSIG23(I) - SIGNZZ(I)*FAC
                ENDIF 
C                F6 = (SIG/SIGT3(I))**2 + 
                F7 = (SIG/SIGT3)**2 + 
     .           (SIGNYZ(I)/MSIG23(I))**2 + (SIGNZX(I)/MSIG13(I))**2  
C                 F6 = SDEL(I)*SDEL(I)*F6
                 F7 = SDEL*SDEL*F7
C
                DFMAX(I)  = MIN(ONE,MAX(DFMAX(I),F1,F2,F3,F4,F5,F6,F7))
                UVAR(I,10) = MAX(F1,F2,F3,F4,F5,F6,F7)
C  
                 IF(F1 >= ONE .OR. F2 >= ONE .OR. F3 >= ONE .OR.
     .              F4 >= ONE .OR. F5 >= ONE .OR. F6 >= ONE .OR.
     .              F7 >= ONE )THEN 
                    UVAR(I,1) = SIGNXX(I)
                    UVAR(I,2) = SIGNYY(I)
                    UVAR(I,3) = SIGNZZ(I)
                    UVAR(I,4) = SIGNXY(I)
                    UVAR(I,5) = SIGNYZ(I)
                    UVAR(I,6) = SIGNZX(I)
                    UVAR(I,7) = TIME
                    IF(TMOD == 0) THEN
                       UVAR(I,8) = FOUR_OVER_5
                    ELSE
                       UVAR(I,8) = ZEP99 
                    ENDIF 
C
                    FAC = HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),
     .                         ABS(SIGNZZ(I)),ABS(SIGNXY(I)),ABS(SIGNYZ(I)),
     .                         ABS(SIGNZX(I)))
                    K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX
                    K1 =  ZEP9*EPSP(I)
                    K2 =  ZEP669741*FAC*EPSP(I)**2
                    K2M = ZEP669741*FAC*KM*EPSPREF**2
                    K2 = MAX(K2M,K2,EM20)
                    TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2 
                    UVAR(I,12) = TELEM
C                    
                    NINDX0= NINDX0+1
                    INDX0(NINDX0)=I
                    IF (F1>=ONE) THEN 
                     MODE(I,1) = 1
                     KK = 1
                    ENDIF
                    IF (F2>=ONE) THEN
                    MODE(I,2) = 1
                    KK = 2
                    ENDIF
                    IF (F3>=ONE) THEN 
                    MODE(I,3) = 1
                    KK = 3
                    ENDIF
                    IF (F4>=ONE) THEN 
                    MODE(I,4) = 1
                    KK = 4
                    ENDIF
                    IF (F5>=ONE) THEN 
                    MODE(I,5) = 1
                    KK = 5
                    ENDIF
                    IF (F6>=ONE) THEN 
                    MODE(I,6) = 1
                    KK = 6
                    ENDIF
                    IF (F7>=ONE) THEN 
                    MODE(I,7) = 1
                    KK = 7
                    ENDIF
                    UVAR(I,9)   = KK 
                  ENDIF
                ENDIF 
              ENDIF  ! iflag
            ENDIF
         ENDDO 

      ENDIF
         
        IF(NINDX0 > 0)THEN
          DO J=1,NINDX0
           I = INDX0(J)
           DO JJ=1,7
             IMODE = MODE(I,JJ)
             IF(IMODE == 1) THEN
               IMODE = JJ
#include "lockon.inc"
               WRITE(IOUT, 1100) ILAY,NGL(I),IMODE,TIME
               WRITE(IOUT, 1110) UVAR(I,1),UVAR(I,2),UVAR(I,3)
               WRITE(IOUT, 1115) UVAR(I,4),UVAR(I,6),UVAR(I,5)
               WRITE(ISTDO,1100) ILAY,NGL(I),IMODE,TIME
               WRITE(ISTDO,1110) UVAR(I,1),UVAR(I,2),UVAR(I,3)
               WRITE(ISTDO,1115) UVAR(I,4),UVAR(I,6),UVAR(I,5)
#include "lockoff.inc"
             ENDIF
          END DO
          END DO
        ENDIF         
C                      
        IF(NINDX > 0)THEN
          DO J=1,NINDX
           I = INDX(J)
C           JJ = 0
C           DO JJ=1,7
C             IMODE = MODE(JJ,I)
#include "lockon.inc"
C             IF(IMODE == 1) THEN
C               IMODE = JJ
                WRITE(IOUT, 1200) NGL(I),ILAY,INT(UVAR(I,9)),TIME
                WRITE(IOUT, 1210) UVAR(I,1),UVAR(I,2),UVAR(I,3)
                WRITE(IOUT, 1215) UVAR(I,4),UVAR(I,6),UVAR(I,5)
                WRITE(ISTDO,1200) NGL(I),ILAY,INT(UVAR(I,9)),TIME
                WRITE(ISTDO,1210) UVAR(I,1),UVAR(I,2),UVAR(I,3)
                WRITE(ISTDO,1215) UVAR(I,4),UVAR(I,6),UVAR(I,5)
#include "lockoff.inc"
C             ENDIF
C          END DO
          END DO
        ENDIF                      
C--------------------------------------------      
 1000 FORMAT(1X,'FAILURE ELEMENT-1 #',I10,1X,
     .'LAYER # ',I10,1X,'MODE #',I10)
C     . 1X, 'FAILURE NUMBER LAYER ', I10)
 1100 FORMAT(1X,'FAILURE LAYER #',I10,1X,
     .'ELEMENT #',I10,1X,'HASHIN MODE #',I10,1X, 'AT TIME #:',1PE12.4)
 1110 FORMAT(1X,'RESPONSIBLE STRESS: SIG_11= ',1PE12.4,1X,
     .'SIG_22= ',1PE12.4,1X,'SIG_33= ',1PE12.4)
 1115 FORMAT(1X,'RESPONSIBLE STRESS: SIG_12= ',1PE12.4,1X,
     .'SIG_23= ',1PE12.4,1X,'SIG_13= ',1PE12.4)
 2000 FORMAT(1X,'FAILURE ELEMENT #',I10,1X,
     .'LAYER # ',I10,1X,'MODE #',I10)
C     . 1X, 'FAILURE NUMBER LAYER ', I10)
 2100 FORMAT(1X,'FAILURE ELEMENT-2 #',I10,1X,
     .'LAYER #',I10,1X, 'AT TIME #:',1PE12.4,1X,'MODE #',I10) 
 1200 FORMAT(1X,'DELETE SOLID ELEMENT # ',I10,1X,'Failed layer # ',I10,
     . 1X,' HASHIN mode # ',I10,1X,'AT TIME # ',1PE12.4)    
 1210 FORMAT(1X,'RESPONSIBLE STRESS: SIG_11= ',1PE12.4,1X,
     .'SIG_22= ',1PE12.4,1X,'SIG_33= ',1PE12.4)
 1215 FORMAT(1X,'RESPONSIBLE STRESS: SIG_12= ',1PE12.4,1X,
     .'SIG_13= ',1PE12.4,1X,'SIG_23= ',1PE12.4,1X)
C--------------------------------------------      
      RETURN
      END
