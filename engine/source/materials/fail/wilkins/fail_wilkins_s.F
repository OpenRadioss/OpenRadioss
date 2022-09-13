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
Chd|  FAIL_WILKINS_S                source/materials/fail/wilkins/fail_wilkins_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_WILKINS_S(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,
     2     NPF    ,TF     ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    , IPM    ,MAT,
     4     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     5     DPLA   ,UVAR    ,OFF    ,IP      ,DFMAX   ,TDELE  )
C-----------------------------------------------
C     3D Wilkins Failure model
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPM(NPROPMI,*),
     .        MAT(NEL),IP
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL)    
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,IFLAG,INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),JJ,IFAIL
      my_real 
     .  AL,BE,PC,DC,
     .  P,SCALE,S1,S2,S3,SS,E1,E2,E3,E4,E5,E6,
     .  EPST2, A, CC1, B, Y ,YP, D, E42, E52, C, E62,W1,W2 
C-----------------------------------------------
      AL = UPARAM(1)
      BE = UPARAM(2)
      PC = UPARAM(3)
      DC = UPARAM(4)
      IFLAG = INT(UPARAM(6))  
C-----------------------------------------------
      IDEL=0
      IDEV=0
      SCALE = ZERO
      IF(IFLAG==1)THEN
         IDEL=1
      ELSEIF(IFLAG==2)THEN
         IDEV =1
      END IF
C...
      IF(IDEL==1)THEN
        DO I=1,NEL
          IF(OFF(I)<0.1) OFF(I)=0.0
          IF(OFF(I)<1.0) OFF(I)=OFF(I)*0.8
        END DO
      END IF
C      
      IF(IDEL==1)THEN
       NINDX=0 
C-------------------------------
C     RUPTURE DUCTILE
C-------------------------------
C      Tuler Butcher
       DO I=1,NEL
        IF(IFLAG==1.AND.OFF(I)==1.)THEN
C-------------------
C     STREES principal 1, 4 newton iterations 
C-------------------
         P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
         E1 = SIGNXX(I) - P
         E2 = SIGNYY(I) - P
         E3 = SIGNZZ(I) - P
         E4 = SIGNXY(I)
         E5 = SIGNYZ(I)
         E6 = SIGNZX(I)
C        -y = (e1-x)(e2-x)(e3-x)
C           - e5^2(e1-x) - e6^2(e2-x) - e4^2(e3-x)
C           + 2e4 e5 e6
C         e1 + e2 + e3 = 0 => terme en x^2 = 0
C         y = x^3 + c x + d
c         yp= 3 x^2 + c
         E42 = E4*E4
         E52 = E5*E5
         E62 = E6*E6
         C = - HALF * (E1*E1 + E2*E2 + E3*E3) - E42 - E52 - E62
         D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &       - TWO*E4*E5*E6 
         CC1 = C*THIRD
         S1 = SQRT(-CC1)
         EPST2 = S1 * S1
         Y = (EPST2 + C)* S1 + D
         IF(ABS(Y)>EM8)THEN
          S1 = 1.75 * S1
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
         ENDIF
          B = S1
          C = C + S1**2 
          S2 = HALF*(-S1 + SQRT(MAX(ZERO,(S1**2 - FOUR*C))))
          SS = HALF*(-S1 - SQRT(MAX(ZERO,(S1**2 - FOUR*C))))
          S3 = SS
          IF(SS>S2)THEN
           S3 = S2
           S2 = SS
          ENDIF 
          A=ONE
          IF(S1/=ZERO.AND.S3==ZERO) A =  S2/S1
          IF(S1==ZERO.AND.S3/=ZERO) A =  S2/S3
          IF(S1/=ZERO.AND.S3/=ZERO)
     .      A = MAX(S2/S3, S2/S1) 
          W2 = MAX(EM20,(TWO - A))**BE
          W1 =  ONE - P/PC
          W1 = (MAX(EM20,ONE/W1))**AL
          UVAR(I,1) = UVAR(I,1) + W1*W2*DPLA(I)
          IF(UVAR(I,1)>=DC) THEN	  
           OFF(I) = FOUR_OVER_5
           IDEL7NOK = 1
           NINDX=NINDX+1
           INDX(NINDX)=I
           TDELE(I) = TIME  
          ENDIF
	 ENDIF
        ENDDO
        IF(NINDX>0.AND.IMCONV==1)THEN
         DO J=1,NINDX
          I=INDX(J)
#include "lockon.inc"
         WRITE(ISTDO,1000)NGL(I)
         WRITE(IOUT,1100)NGL(I),TIME          
#include "lockoff.inc"
        END DO
       END IF
C      end Tuler Butcher
      END IF

Cc deviatoric will be vanished      
      IF(IDEV==1)THEN
       NINDX=0 
       NINDEX = 0 
       DO I=1,NEL
        IF(IFLAG==2.AND.OFF(I)==ONE)THEN
         IF(UVAR(I,1)<DC)THEN
         P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
         E1 = SIGNXX(I) - P
         E2 = SIGNYY(I) - P
         E3 = SIGNZZ(I) - P
         E4 = SIGNXY(I)
         E5 = SIGNYZ(I)
         E6 = SIGNZX(I)
C        -y = (e1-x)(e2-x)(e3-x)
C           - e5^2(e1-x) - e6^2(e2-x) - e4^2(e3-x)
C           + 2e4 e5 e6
C         e1 + e2 + e3 = 0 => terme en x^2 = 0
C         y = x^3 + c x + d
c         yp= 3 x^2 + c
         E42 = E4*E4
         E52 = E5*E5
         E62 = E6*E6
         C = - HALF * (E1*E1 + E2*E2 + E3*E3) - E42 - E52 - E62
         D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &       - TWO*E4*E5*E6 
         CC1 = C*THIRD
         S1 = SQRT(-CC1)
         EPST2 = S1 * S1
         Y = (EPST2 + C)* S1 + D
         IF(ABS(Y)>EM8)THEN
          S1 = 1.75 * S1
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
          EPST2 = S1 * S1
          Y = (EPST2 + C)* S1 + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)S1 = S1 - Y/YP
         ENDIF 
          B = S1
          C = C + S1**2 
          S2 = HALF*(-S1 + SQRT(MAX(ZERO,(S1**2 - FOUR*C))))
          SS = HALF*(-S1 - SQRT(MAX(ZERO,(S1**2 - FOUR*C))))
          S3 = SS
          IF(SS>S2)THEN
           S3 = S2
           S2 = SS
          ENDIF 
          A = ONE
          IF(S1/=ZERO.AND.S3==ZERO) A =  S2/S1
          IF(S1==ZERO.AND.S3/=ZERO) A =  S2/S3
          IF(S1/=ZERO.AND.S3/=ZERO)
     .      A = MAX(S2/S3, S2/S1) 
          W2 = MAX(EM20,(TWO - A))**BE
          W1 = ONE - ONE*P/PC
          W1 = (MAX(EM20,ONE/W1))**AL
          UVAR(I,1) = UVAR(I,1) + W1*W2*DPLA(I)      
          IF(UVAR(I,1)>=DC) THEN	   
           NINDX=NINDX+1
           INDX(NINDX)=I
           SIGNXX(I) =  P
           SIGNYY(I) =  P
           SIGNZZ(I) =  P
           SIGNXY(I) = ZERO
           SIGNYZ(I) = ZERO
           SIGNZX(I) = ZERO                 
          ENDIF
C uvar> DC          
         ELSE 
           P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
           SIGNXX(I) =  P
           SIGNYY(I) =  P
           SIGNZZ(I) =  P
           SIGNXY(I) = ZERO
           SIGNYZ(I) = ZERO
           SIGNZX(I) = ZERO       
         ENDIF
        ENDIF  
       ENDDO
       IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
         I = INDX(J)
#include "lockon.inc"
         WRITE(IOUT, 2000) NGL(I)
         WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
        END DO
       END IF           
      ENDIF       
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------
       DO I=1,NEL
          DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DC))
       ENDDO      
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE20.13)
CC     
 2000 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10)
 2100 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10,
     .          ' AT TIME :',1PE20.13)
      RETURN
      END
