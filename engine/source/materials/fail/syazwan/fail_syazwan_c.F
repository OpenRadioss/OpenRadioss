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
Chd|  FAIL_SYAZWAN_C                      source/materials/fail/syazwan/fail_syazwan_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|====================================================================
      SUBROUTINE FAIL_SYAZWAN_C(
     1           NEL      ,NUVAR    ,
     2           TIME     ,UPARAM   ,NGL      ,IPT      ,NPTOT    ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5           DPLA     ,EPSP     ,UVAR     ,PTHKF    ,UEL1     ,
     6           OFF      ,OFFL     ,DFMAX    ,TDEL     ,NFUNC    ,
     7           IFUNC    ,ALDT     ,FOFF     ,IPG      ,PLA      ,
     8           DMG_FLAG ,DMG_SCALE,NPF      ,TF )
C-----------------------------------------------
C    Simplified Fracture Surface
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
       INTEGER DMG_FLAG,FOFF(NEL)
      my_real 
     .  UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  EPSXX(NEL),EPSYY(NEL),EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL),
     .  DFMAX(NEL),TDEL(NEL),PLA(NEL),DMG_SCALE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,NINDX1,NINDX2,MAP,INST
       INTEGER, DIMENSION(MVSIZ) :: INDX1,INDX2

       my_real  E12, S1, S2, Q, SS, BETA, EPS
       my_real  DAM_SF, DAM_LIM, N_VAL
       my_real ,DIMENSION(NEL) :: EMAJ,EMIN
       my_real  C1, C2, C3, C4, C5, C6, EPSMIN
       my_real  P, SVM, ST, SXX, SYY, R
       my_real  LODE, LODEP, DAMAGE, EPSFAIL, EPS_C
       my_real  EQUA1, EQUA2, E1, E2, DCRIT, ALPHA
       my_real  SOFTEXP, REF_LEN, LAMBDA, FAC
       my_real  DYDX
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
C      Explicit definition for trial
C-----------------------------------------------
       C1           = UPARAM(1)
       C2           = UPARAM(2)
       C3           = UPARAM(3)
       C4           = UPARAM(4)
       C5           = UPARAM(5)
       C6           = UPARAM(6)
       EPSMIN       = UPARAM(7)
       PTHKF        = UPARAM(8)
       MAP          = UPARAM(9)
       DAM_SF       = UPARAM(10)
       DAM_LIM      = UPARAM(11)
       INST         = UPARAM(12)
       N_VAL        = UPARAM(13)
       SOFTEXP      = UPARAM(14)
       REF_LEN      = UPARAM(15)
       NINDX1       = 0
       NINDX2       = 0
       EPSFAIL      = ZERO
       IF(INST == 1) DMG_FLAG = 1


       IF (TIME == ZERO .AND. NFUNC > 0) THEN
        DO I=1,NEL
          IF (NUVAR == 5) THEN
            UVAR(I,5) = ALDT(I)
            LAMBDA = UVAR(I,5) / REF_LEN
            FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX)
            UVAR(I,5) = FAC
          ENDIF
        ENDDO
       ENDIF

       
C---------INITIALIZE DAMAGE DUE TO MANUF. HISTORY-------------
C---------ASSUME LINEAR STRAIN PATH---------------------------
       IF (TIME == ZERO .AND. MAP == 1) THEN
        DO I = 1,NEL
         IF (PLA(I) /= ZERO) THEN
C----------IMPLEMENTATION-------------------------------------
           E12= HALF*EPSXY(I)
           S1 = HALF*(EPSXX(I) + EPSYY(I))
           S2 = HALF*(EPSXX(I) - EPSYY(I))
           Q  = SQRT(S2**2 + E12**2)
           EMAJ(I) = S1 + Q
           EMIN(I) = S1 - Q
           IF (EMIN(I) >= EMAJ(I)) THEN
             SS      = EMIN(I)
             EMIN(I) = EMAJ(I)
             EMAJ(I) = SS
           ENDIF
           BETA = EMIN(I)/EMAJ(I)
           ST = (1+BETA)/SQRT(THREE)/SQRT(1+BETA+BETA**2)
C           LODEP = ONE-(TWO/PI*ACOS(-TWENTY7*HALF*ST*(ST**2-THIRD)))
           LODEP = ASIN((-TWENTY7*HALF*ST)*(ST**2-THIRD))*2/PI
           EPSFAIL = C1+C2*ST+C3*LODEP+C4*ST**2+C5*LODEP**2+C6*ST*LODEP
           EPSFAIL = MAX(EPSFAIL,EPSMIN)
           IF(NUVAR==5) EPSFAIL = EPSFAIL*UVAR(I,5)
           EPS = TWO*EMAJ(I)/SQRT(THREE)*SQRT(ONE+BETA+BETA**2) 
           UVAR(I,1) = EPS/MAX(EPSFAIL,EM6)*DAM_SF
           UVAR(I,1) = MIN(DAM_LIM,UVAR(I,1))
           DFMAX(I) = UVAR(I,1)
         ENDIF
        ENDDO
       ENDIF

       DO I=1,NEL
         IF(OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) == 1) THEN
           DAMAGE = UVAR(I,1)
           P   = THIRD*(SIGNXX(I) + SIGNYY(I))
           SVM = SQRT (SIGNXX(I)**2 + SIGNYY(I)**2
     .           - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)**2)
           ST  = P/MAX(EM20,SVM)
           SXX = SIGNXX(I) - P
           SYY = SIGNYY(I) - P
           R   = (TWENTY7*HALF*SXX*SYY*(-P))**THIRD
           LODE = (R/SVM)**3
           IF(LODE > ONE) LODE = ONE
           IF(LODE < -ONE) LODE = -ONE
           LODEP = ONE-TWO/PI*ACOS(LODE)
           EPSFAIL =C1+C2*ST+C3*LODEP+C4*ST**2+C5*LODEP**2+C6*ST*LODEP
           EPSFAIL = MAX(EPSFAIL, EPSMIN)
           IF(NUVAR==5) EPSFAIL = EPSFAIL*UVAR(I,5)
           IF (EPSFAIL>ZERO) THEN     
                IF (INST == 1) THEN
                  E12= HALF*EPSXY(I)
                  S1 = HALF*(EPSXX(I) + EPSYY(I))
                  S2 = HALF*(EPSXX(I) - EPSYY(I))
                  Q  = SQRT(S2**2 + E12**2)
                  EMAJ(I) = S1 + Q
                  EMIN(I) = S1 - Q
                  IF (EMIN(I) >= EMAJ(I)) THEN
                    SS      = EMIN(I)
                    EMIN(I) = EMAJ(I)
                    EMAJ(I) = SS
                  ENDIF
                  BETA = EMIN(I)/MAX(EMAJ(I),EM06)
                  BETA = MAX(-0.999,BETA)
                  BETA = MIN(ONE,BETA)
                  ALPHA = (TWO*BETA+ONE)/(TWO+BETA)
                  EQUA1 = ONE-ALPHA+ALPHA**2
                  EQUA2 = FOUR-THREE*ALPHA-THREE*ALPHA**2+FOUR*ALPHA
                  E1 = TWO*(TWO-ALPHA)*EQUA1/EQUA2*N_VAL
                  EPS_C = TWO*E1/SQRT(THREE)*SQRT(ONE+BETA+BETA**2)
                  IF (PLA(I)>EPS_C .AND. UVAR(I,3)/=ONE) THEN
                    UVAR(I,3)=ONE
                    UVAR(I,4)=EPS_C/EPSFAIL
                  ENDIF
                  IF(UVAR(I,3)==ONE) THEN
                    DCRIT=UVAR(I,4)
                    DMG_SCALE(I)=ONE-((DFMAX(I)-DCRIT)
     .                  /MAX(ONE-DCRIT,EM20))**SOFTEXP
                  ENDIF
                ENDIF
             DAMAGE = DAMAGE + DPLA(I)/EPSFAIL
             UVAR(I,1) = DAMAGE
           ENDIF
         ENDIF 
         
         DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)))
         
         IF(OFFL(I) == 1. .AND. DFMAX(I) >= 1.0) THEN
           FOFF(I)       = 0
           NINDX1        = NINDX1 + 1
           INDX1(NINDX1) = I
         ENDIF

         IF(DAMAGE > ONE .AND. UVAR(I,2) == ZERO .AND. OFFL(I) == ONE) THEN
             UVAR(I,2) = ONE
         ENDIF
       ENDDO
c-----------------------
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
          WRITE(IOUT, 2200) NGL(I), TIME
          WRITE(ISTDO,2200) NGL(I), TIME
#include "lockoff.inc"
        END DO
      END IF   ! NINDX             
C---------Damage for output  0 < DFMAX < 1 --------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (SYAZWAN)',I10,1X,'GAUSS POINT',I3,
     .       1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (SYAZWAN)',I10,1X,
     . ' AT TIME :',1PE12.4)
c------------------------
       RETURN
       END
