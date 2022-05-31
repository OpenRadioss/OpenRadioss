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
Chd|  FAIL_BIQUAD_C                 source/materials/fail/biquad/fail_biquad_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_BIQUAD_C(
     1           NEL      ,NUVAR    ,
     2           TIME     ,UPARAM   ,NGL      ,IPT      ,NPTOT    ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,EPSP     ,UVAR     ,PTHKF    ,UEL1     ,
     5           OFF      ,OFFL     ,DFMAX    ,TDEL     ,NFUNC    ,
     6           IFUNC    ,NPF      ,TF       ,ALDT     ,FOFF     ,
     7           IPG      )
C-----------------------------------------------
C    Biquadratic failure model
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C ISHELL  |  *      | I | R | GEOMETRICAL FLAGS   
C NGL     | NEL     | I | R | ELEMEMT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C---------+---------+---+---+--------------------------------------------
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C IGTYP                       PROPERTY TYPE
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,IPT,NPTOT,NFUNC,IPG
      INTEGER NGL(NEL),IFUNC(NFUNC)
      my_real TIME,UPARAM(*),DPLA(NEL),EPSP(NEL),
     .   UEL1(NEL),ALDT(NEL),PTHKF
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
       INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real 
     .  UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
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
       my_real hydros, vmises, triaxs,REF_EL_LEN
       INTEGER I,J,L,NINDX1,NINDX2,FLAG_PERTURB,SEL
       INTEGER FAIL_COUNT,IT
       INTEGER, DIMENSION(MVSIZ) :: INDX1,INDX2

       my_real  X_1(3) , X_2(3),c3,DYDX
       my_real  EPS_FAIL, EPS_FAIL2, DAMAGE, INST
       my_real  P1X,P1Y,S1X,S1Y,S2Y, A1, A2, B1, B2, C1, C2,FAC,LAMBDA
C-----------------------------------------------
c! UVAR1 = damage due to instability (triax between 1/3 and 2/3)
c! UVAR2 = actuall integration point ON or OFF
c! UVAR3-8 = perturbated parameter
c! UVAR3 (if perturbation is not used) or UVAR9 (if used) = initial element length and then scale factor
C-----------------------------------------------
       X_1(1)       = UPARAM(1)
       X_1(2)       = UPARAM(2)
       X_1(3)       = UPARAM(3)
       X_2(1)       = UPARAM(4)
       X_2(2)       = UPARAM(5)
       X_2(3)       = UPARAM(6)
       PTHKF        = UPARAM(7)
       FLAG_PERTURB = UPARAM(8)
       c3           = UPARAM(9)
       L            = INT(UPARAM(10)+0.0001)
       SEL          = INT(UPARAM(11)+0.0001)
       INST         = UPARAM(12)
       REF_EL_LEN   = UPARAM(13)
       NINDX1       = 0  
       NINDX2       = 0
       EPS_FAIL     = ZERO
       EPS_FAIL2    = ZERO
C
C!  Initialization
       IF (TIME == ZERO .AND. NFUNC > 0) THEN
        DO I=1,NEL
          IF (NUVAR == 3) THEN
            UVAR(I,3) = ALDT(I) 
            LAMBDA = UVAR(I,3) / REF_EL_LEN
            FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX) 
            UVAR(I,3) = FAC
          ELSEIF (NUVAR ==9) THEN
            UVAR(I,9) = ALDT(I) 
            LAMBDA = UVAR(I,9) / REF_EL_LEN
            FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX) 
            UVAR(I,9) = FAC
          ENDIF
        ENDDO
       ENDIF
C-----------------------------------------------
       IF(FLAG_PERTURB == 0) THEN
         DO I =1,NEL
           IF(OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) .EQ. 1 ) THEN
             hydros= (SIGNXX(I)+ SIGNYY(I))/3.0
             vmises= sqrt((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(3.0*SIGNXY(I)**2))
             triaxs = hydros / MAX(EM20,vmises)
             DAMAGE = UVAR(I,1)
             IF (triaxs <= THIRD) THEN     ! triax < 1/3
               EPS_FAIL =   X_1(1) +  X_1(2) * triaxs + X_1(3) * triaxs**2
               IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
             ELSE                          ! triax > 1/3
              SELECT CASE (SEL)
               CASE(1)
                 EPS_FAIL   =   X_2(1) +  X_2(2) * triaxs + X_2(3) * triaxs**2
                 IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
               CASE(2)
                 IF (triaxs <= ONE/SQR3) THEN                     ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                 ELSE                                             ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                 ENDIF
               CASE(3)
                 IF (triaxs <= ONE/SQR3) THEN                       ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   EPS_FAIL2 = C2 + B2 * triaxs + A2 * triaxs**2   ! INSTABILITY
                   IF (NUVAR == 3) EPS_FAIL2 = EPS_FAIL2 * UVAR(I,3)
                 ELSE                          ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   EPS_FAIL2 = C2 + B2 * triaxs + A2 * triaxs**2   ! INSTABILITY
                   IF (NUVAR == 3) EPS_FAIL2 = EPS_FAIL2 * UVAR(I,3)
                 ENDIF
                 IF (EPS_FAIL2> ZERO) THEN
                   DAMAGE = DAMAGE + DPLA(I)/EPS_FAIL2
                   UVAR(I,1) = DAMAGE
                 ENDIF
               END SELECT
           ENDIF
           
             DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
             DFMAX(I) = MIN(ONE,DFMAX(I))
           
           IF (OFFL(I).EQ.1. .AND. DFMAX(I).GE.1.0) THEN
             FOFF(I)       = 0
             NINDX1        = NINDX1 + 1
             INDX1(NINDX1) = I  
           ENDIF
           
           IF (DAMAGE .GE. ONE .AND. UVAR(I,2) .EQ. ZERO .AND. OFFL(I) .EQ. ONE) THEN
             UVAR(I,2) = ONE  
             UEL1(I)   = UEL1(I) + ONE
             IF (INT(UEL1(I)) == NPTOT) THEN
               NINDX2        = NINDX2 + 1
               INDX2(NINDX2) = I
               TDEL(I)       = TIME
               OFF(I)        = ZERO
               SIGNXX(I)     = ZERO
               SIGNYY(I)     = ZERO
               SIGNXY(I)     = ZERO
               SIGNYZ(I)     = ZERO
               SIGNZX(I)     = ZERO
             ENDIF
           ENDIF
         ENDIF
         ENDDO
       ELSEIF(FLAG_PERTURB == 1) THEN
         DO I =1,NEL
           IF(OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) .EQ. 1 ) THEN
           hydros= (SIGNXX(I)+ SIGNYY(I))/3.0
           vmises= sqrt((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(3.0*SIGNXY(I)**2))
           triaxs = hydros / MAX(EM20,vmises)
           DAMAGE = UVAR(I,1)
             IF (triaxs <= THIRD) THEN
        	   EPS_FAIL =   UVAR(I,3) +  UVAR(I,4) * triaxs + UVAR(I,5) * triaxs**2
               IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
             ELSE           ! triax > 1/3
             SELECT CASE (SEL)
               CASE(1)
         	    EPS_FAIL =   UVAR(I,6) +  UVAR(I,7) * triaxs + UVAR(I,8) * triaxs**2
                IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
               CASE(2)
                 IF (triaxs <= ONE/SQR3) THEN
                   P1X      = THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ELSE 
                   P1X      = TWO * THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ENDIF
               CASE(3)
                 IF (triaxs <= ONE/SQR3) THEN
                   P1X      = THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                   EPS_FAIL2 = C2 + B2 * triaxs + A2 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL2 = EPS_FAIL2 * UVAR(I,9)
                 ELSE 
                   P1X      = TWO * THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                   EPS_FAIL2 = C2 + B2 * triaxs + A2 * triaxs**2
                   IF (NUVAR == 9) EPS_FAIL2 = EPS_FAIL2 * UVAR(I,9)
                 ENDIF
                 IF (EPS_FAIL2> ZERO) THEN
                   DAMAGE = DAMAGE + DPLA(I)/EPS_FAIL2
                   UVAR(I,1) = DAMAGE
                 ENDIF
               END SELECT
           ENDIF
           
             DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
             DFMAX(I) = MIN(ONE,DFMAX(I))
             
           IF (OFFL(I).EQ.1. .AND. DFMAX(I).GE.1.0) THEN
             FOFF(I)       = 0
             NINDX1        = NINDX1 + 1
             INDX1(NINDX1) = I  
           ENDIF
           
           IF (DAMAGE .GT. ONE .AND. UVAR(I,2) .EQ. ZERO .AND. OFFL(I) .EQ. ONE) THEN
             UVAR(I,2) = ONE   
             UEL1(I)   = UEL1(I) + ONE
             IF (INT(UEL1(I)) == NPTOT) THEN
               NINDX2        = NINDX2 + 1
               INDX2(NINDX2) = I
               TDEL(I)       = TIME
               OFF(I)        = ZERO
               SIGNXX(I)     = ZERO
               SIGNYY(I)     = ZERO
               SIGNXY(I)     = ZERO
               SIGNYZ(I)     = ZERO
               SIGNZX(I)     = ZERO
             ENDIF
           ENDIF
         ENDIF
         ENDDO

       ENDIF
       
c------------------------
c------------------------
c------------------------
c------------------------
      IF (NINDX1 > 0) THEN        
        DO J=1,NINDX1             
          I = INDX1(J)         
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,2000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc" 
        ENDDO
      ENDIF
      IF (NINDX2 > 0) THEN        
        DO J=1,NINDX2            
          I = INDX2(J)         
#include "lockon.inc"
          WRITE(IOUT, 3000) NGL(I)
          WRITE(IOUT, 2200) NGL(I), TIME
          WRITE(ISTDO,3000) NGL(I)
          WRITE(ISTDO,2200) NGL(I), TIME                 
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------------
C---------Damage for output  0 < DFMAX < 1 --------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (BIQUAD)',I10,1X,'GAUSS POINT',I3,
     .       1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (BIQUAD)',I10,1X,
     . ' AT TIME :',1PE12.4)
 3000 FORMAT(1X,'FOR SHELL ELEMENT (BIQUAD)',I10,
     .       1X,'INSTABILITY REACHED.')
c------------------------
       RETURN
       END
