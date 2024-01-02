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
Chd|  FAIL_ORTHBIQUAD_C             source/materials/fail/orthbiquad/fail_orthbiquad_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ORTHBIQUAD_C(
     1           NEL      ,NUVAR    ,
     2           TIME     ,UPARAM   ,NGL      ,IPT      ,NPTOT    ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,EPSP     ,UVAR     ,PTHKF    ,UEL1     ,
     5           OFF      ,OFFL     ,DFMAX    ,TDEL     ,NFUNC    ,
     6           IFUNC    ,NPF      ,TF       ,ALDT     ,FOFF     ,
     7           IPG      )
C-----------------------------------------------
C    ORTHOTROPIC BIQUADRATIC FAILURE MODEL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
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
C NGL     | NEL     | I | R | ELEMENT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
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
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
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
      my_real hydros,vmises,triaxs,REF_EL_LEN
      INTEGER I,J,K,L,NINDX1,NINDX2,SEL,NANGLE
      INTEGER FAIL_COUNT,IT,IREG,IRATE
      INTEGER, DIMENSION(MVSIZ) :: INDX1,INDX2
      my_real X_1(3),X_2(3),c3,DYDX,COS2(10,10),EPSD0,CJC,RATE_SCALE,FRATE
      my_real EPS_FAIL,EPS_FAIL2,DAMAGE,INST,FSCALE_LEN,MOHR_RADIUS
      my_real P1X,P1Y,S1X,S1Y,S2Y,A1,A2,B1,B2,C1,C2,FAC,LAMBDA,COS2THETA(NEL)
      my_real SIG1(NEL),SIG2(NEL),MOHR_CENTER
      my_real, DIMENSION(:), ALLOCATABLE :: Q_X11,Q_X12,Q_X13,Q_X21,Q_X22,Q_X23,Q_INST
C
      DATA  COS2/
     1 1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,   
     2 0.   ,1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     3 -1.  ,0.   ,2.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     4 0.   ,-3.  ,0.   ,4.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     5 1.   ,0.   ,-8.  ,0.   ,8.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     6 0.   ,5.   ,0.   ,-20. ,0.   ,16.  ,0.   ,0.   ,0.   ,0.   ,     
     7 -1.  ,0.   ,18.  ,0.   ,-48. ,0.   ,32.  ,0.   ,0.   ,0.   ,     
     8 0.   ,-7.  ,0.   ,56.  ,0.   ,-112.,0.   ,64.  ,0.   ,0.   ,     
     9 1.   ,0.   ,-32. ,0.   ,160. ,0.   ,-256.,0.   ,128. ,0.   ,     
     A 0.   ,9.   ,0.   ,-120.,0.   ,432. ,0.   ,-576 ,0.   ,256. /
C-----------------------------------------------
c! UVAR1 = damage due to instability (triax between 1/3 and 2/3)
c! UVAR2 = actual integration point ON or OFF
c! UVAR3 = 
C-----------------------------------------------
c      
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      PTHKF      = UPARAM(1)
      SEL        = INT(UPARAM(2))
      REF_EL_LEN = UPARAM(3)
      EPSD0      = UPARAM(4)
      CJC        = UPARAM(5)
      RATE_SCALE = UPARAM(6)
      NANGLE     = INT(UPARAM(7))
      ! -> Allocation of factors
      ALLOCATE(Q_X11(NANGLE),Q_X12(NANGLE),Q_X13(NANGLE),
     .         Q_X21(NANGLE),Q_X22(NANGLE),Q_X23(NANGLE))
      ! Recovering factors
      IF (SEL == 3) THEN
        ALLOCATE(Q_INST(NANGLE))        
        DO J = 1,NANGLE
          Q_X11(J)  = UPARAM(8  + 7*(J-1))
          Q_X12(J)  = UPARAM(9  + 7*(J-1))
          Q_X13(J)  = UPARAM(10 + 7*(J-1)) 
          Q_X21(J)  = UPARAM(11 + 7*(J-1))
          Q_X22(J)  = UPARAM(12 + 7*(J-1))
          Q_X23(J)  = UPARAM(13 + 7*(J-1))
          Q_INST(J) = UPARAM(14 + 7*(J-1))
        ENDDO 
      ELSE
        DO J = 1,NANGLE
          Q_X11(J)  = UPARAM(8  + 6*(J-1))
          Q_X12(J)  = UPARAM(9  + 6*(J-1))
          Q_X13(J)  = UPARAM(10 + 6*(J-1)) 
          Q_X21(J)  = UPARAM(11 + 6*(J-1))
          Q_X22(J)  = UPARAM(12 + 6*(J-1))
          Q_X23(J)  = UPARAM(13 + 6*(J-1)) 
        ENDDO
      ENDIF
c
      ! Recovering functions
      IRATE = 0
      IF (RATE_SCALE /= ZERO) THEN
        IRATE = 1
      ENDIF
      IREG  = 0
      IF (REF_EL_LEN /= ZERO) THEN
        IREG = IRATE + 1
      ENDIF
c      
      ! At initial time, compute the element size regularization factor
      IF (UVAR(1,3) == ZERO .AND. IREG > 0) THEN
        DO I=1,NEL   
          LAMBDA    = ALDT(I)/REF_EL_LEN
          FAC       = FINTER(IFUNC(IREG),LAMBDA,NPF,TF,DYDX) 
          UVAR(I,3) = FAC
        ENDDO
      ENDIF
C
      ! Initialization of variable
      NINDX1 = 0
      NINDX2 = 0
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE DAMAGE VARIABLE
      !====================================================================
      DO I = 1,NEL
c
        ! Failure strain initialization
        EPS_FAIL  = ZERO
        EPS_FAIL2 = ZERO
c
        ! If the element is not broken
        IF (OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) == 1) THEN
c
          ! Computation of loading angle
          MOHR_RADIUS = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2 + SIGNXY(I)**2)
          MOHR_CENTER = (SIGNXX(I)+SIGNYY(I))/TWO
          IF (MOHR_RADIUS > EM20) THEN
            COS2THETA(I) = ((SIGNXX(I)-SIGNYY(I))/TWO)/MOHR_RADIUS
          ELSE
            COS2THETA(I) = ONE
          ENDIF
          SIG1(I) = MOHR_CENTER + MOHR_RADIUS
          SIG2(I) = MOHR_CENTER - MOHR_RADIUS
          IF (SIG1(I)<ZERO.OR.((SIG2(I)<ZERO).AND.(SIG2(I)<-SIG1(I)))) THEN 
            COS2THETA(I) = -COS2THETA(I)
          ENDIF
c
          ! Computation of the BIQUAD parameters
          X_1(1:3) = ZERO
          X_2(1:3) = ZERO
          INST     = ZERO
          DO J = 1,NANGLE
            DO K = 1,J              
              X_1(1) = X_1(1) + Q_X11(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_1(2) = X_1(2) + Q_X12(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_1(3) = X_1(3) + Q_X13(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(1) = X_2(1) + Q_X21(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(2) = X_2(2) + Q_X22(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(3) = X_2(3) + Q_X23(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              IF (SEL == 3) INST = INST + Q_INST(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO          
          ENDDO
c        
          ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
          hydros = (SIGNXX(I)+ SIGNYY(I))/THREE
          vmises = SQRT((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(THREE*SIGNXY(I)**2))
          triaxs = hydros / MAX(EM20,vmises)
c
          ! Computing the plastic strain at failure according to stress triaxiality
          IF (triaxs <= THIRD) THEN     ! triax < 1/3
            EPS_FAIL = X_1(1) + X_1(2)*triaxs + X_1(3)*triaxs**2
            IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
          ELSE                          ! triax > 1/3
            SELECT CASE (SEL)
              CASE(1)
                EPS_FAIL = X_2(1) +  X_2(2)*triaxs + X_2(3)*triaxs**2
                IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
              CASE(2)
                IF (triaxs <= ONE/SQR3) THEN                     ! triax < 0.57735
                  P1X      = THIRD
                  P1Y      = X_1(1) + X_1(2)*P1X + X_1(3)*P1X**2
                  S1X      = ONE/SQR3
                  S1Y      = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                  A1       = (P1Y - S1Y)/(P1X - S1X)**2
                  B1       = -TWO*A1*S1X
                  C1       = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*triaxs + A1*triaxs**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                ELSE                                             ! triax > 0.57735
                  P1X      = TWO*THIRD
                  P1Y      = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                  S1X      = ONE/SQR3
                  S1Y      = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                  A1       = (P1Y - S1Y)/(P1X - S1X)**2
                  B1       = -TWO*A1*S1X
                  C1       = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*triaxs + A1*triaxs**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                ENDIF
              CASE(3)
                IF (triaxs <= ONE/SQR3) THEN                       ! triax < 0.57735
                  P1X      = THIRD
                  P1Y      = X_1(1) + X_1(2)*P1X + X_1(3)*P1X**2
                  S1X      = ONE/SQR3
                  S1Y      = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                  S2Y      = INST
                  A1       = (P1Y - S1Y)/(P1X - S1X)**2
                  A2       = (P1Y - S2Y)/(P1X - S1X)**2
                  B1       = -TWO*A1*S1X
                  B2       = -TWO*A2*S1X
                  C1       = A1*S1X**2 + S1Y 
                  C2       = A2*S1X**2 + S2Y 
                  EPS_FAIL = C1 + B1*triaxs + A1*triaxs**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                  EPS_FAIL2 = C2 + B2*triaxs + A2*triaxs**2   ! INSTABILITY
                  IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2*UVAR(I,3)
                ELSE                          ! triax > 0.57735
                  P1X      = TWO*THIRD
                  P1Y      = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                  S1X      = ONE/SQR3
                  S1Y      = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                  S2Y      = INST
                  A1       = (P1Y - S1Y)/(P1X - S1X)**2
                  A2       = (P1Y - S2Y)/(P1X - S1X)**2
                  B1       = -TWO*A1*S1X
                  B2       = -TWO*A2*S1X
                  C1       = A1*S1X**2 + S1Y 
                  C2       = A2*S1X**2 + S2Y 
                  EPS_FAIL = C1 + B1*triaxs + A1*triaxs**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                  EPS_FAIL2 = C2 + B2*triaxs + A2*triaxs**2   ! INSTABILITY
                  IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2*UVAR(I,3)
                ENDIF
            END SELECT
          ENDIF
c
          ! Strain-rate effects
          IF (CJC /= ZERO .OR. IRATE /= 0) THEN 
            IF (CJC /= ZERO) THEN 
              FRATE = ONE
              IF (EPSP(I) > EPSD0) FRATE = FRATE + CJC*LOG(EPSP(I)/EPSD0)
            ELSEIF (IRATE /= 0) THEN 
              FRATE = FINTER(IFUNC(IRATE),EPSP(I)/RATE_SCALE,NPF,TF,DYDX)
            ENDIF
            EPS_FAIL = EPS_FAIL*FRATE
            IF (SEL == 3) EPS_FAIL2 = EPS_FAIL2*FRATE
          ENDIF
c
          ! Computation of damage variables
          DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
          DFMAX(I) = MIN(ONE,DFMAX(I))
          IF (SEL == 3) THEN 
            DAMAGE = UVAR(I,1)
            IF (EPS_FAIL2 > ZERO) THEN
              DAMAGE = DAMAGE + DPLA(I)/MAX(EPS_FAIL2,EM6)
              UVAR(I,1) = DAMAGE
            ENDIF
          ELSE
            DAMAGE = ZERO
          ENDIF
c
          ! Checking element failure using global damage
          IF (OFFL(I) == ONE .AND. DFMAX(I) >= ONE) THEN
            FOFF(I)       = 0
            NINDX1        = NINDX1 + 1
            INDX1(NINDX1) = I  
          ENDIF
c
          ! Checking element failure using instability damage
          IF (DAMAGE > ONE .AND. UVAR(I,2) == ZERO .AND. OFFL(I) == ONE) THEN
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
c------------------------
c------------------------
      ! Deallocation of table
      IF (ALLOCATED(Q_X11))  DEALLOCATE(Q_X11)
      IF (ALLOCATED(Q_X12))  DEALLOCATE(Q_X12)
      IF (ALLOCATED(Q_X13))  DEALLOCATE(Q_X13)
      IF (ALLOCATED(Q_X21))  DEALLOCATE(Q_X21)
      IF (ALLOCATED(Q_X22))  DEALLOCATE(Q_X22)
      IF (ALLOCATED(Q_X23))  DEALLOCATE(Q_X23)
      IF (ALLOCATED(Q_INST)) DEALLOCATE(Q_INST)
c------------------------
c------------------------
      IF (NINDX1 > 0) THEN        
        DO J=1,NINDX1             
          I = INDX1(J)         
#include "lockon.inc"         
          WRITE(IOUT, 2000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,2000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc" 
        END DO                   
      END IF 
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
      END IF           
c------------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (ORTHBIQUAD)',I10,1X,'GAUSS POINT',I3,
     .       1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (ORTHBIQUAD)',I10,1X,
     . ' AT TIME :',1PE12.4)
 3000 FORMAT(1X,'FOR SHELL ELEMENT (ORTHBIQUAD)',I10,
     .       1X,'INSTABILITY REACHED.')
c------------------------
       RETURN
       END
