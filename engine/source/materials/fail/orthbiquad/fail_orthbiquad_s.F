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
Chd|  FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ORTHBIQUAD_S (
     1     NEL      ,NUPARAM  ,NUVAR    ,MFUNC    ,KFUNC    ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,
     3     NGL      ,DPLA     ,EPSP     ,UVAR     ,OFF      ,
     4     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5     DFMAX    ,TDELE    ,ALDT     )
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include      "implicit_f.inc"
C!---------+---------+---+---+--------------------------------------------
C! VAR     | SIZE    |TYP| RW| DEFINITION
C!---------+---------+---+---+--------------------------------------------
C! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C! NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C! NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C!---------+---------+---+---+--------------------------------------------
C! MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C! KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C! NPF     |  *      | I | R | FUNCTION ARRAY   
C! TF      |  *      | F | R | FUNCTION ARRAY 
C!---------+---------+---+---+--------------------------------------------
C! TIME    |  1      | F | R | CURRENT TIME
C! TIMESTEP|  1      | F | R | CURRENT TIME STEP
C! UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C!---------+---------+---+---+--------------------------------------------
C! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C! ...     |         |   |   |
C! ...     |         |   |   |
C!---------+---------+---+---+--------------------------------------------
C! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C! OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C!---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
#include "impl1_c.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),EPSP(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),
     .   ALDT(NEL)     
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C!        Y = FINTER(IFUNC(J),X,NPF,TF,DF)
C!        Y       : y = f(x)
C!        X       : x
C!        DF      : f'(x) = dy/dx
C!        IFUNC(J): FUNCTION INDEX
C!              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C!        NPF,TF  : FUNCTION PARAMETER
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,K,J,INDX(MVSIZ),NINDX,SEL,NANGLE,IREG,IRATE
      my_real 
     .        DF,P,triaxs,SVM,SXX,SYY,SZZ,EPS_FAIL,
     .        P1X,P1Y,S1X,S1Y,S2Y,A1,B1,C1,REF_EL_LEN,LAMBDA,FAC,
     .        X_1(3),X_2(3),COS2(10,10),EPSD0,CJC,RATE_SCALE,FRATE,
     .        MOHR_RADIUS,COS2THETA(NEL)
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
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      SEL        = INT(UPARAM(2))
      REF_EL_LEN = UPARAM(3)
      EPSD0      = UPARAM(4)
      CJC        = UPARAM(5)
      RATE_SCALE = UPARAM(6)
      NANGLE     = INT(UPARAM(7))
c
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
      IF (SEL == 3) SEL = 2
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
          FAC       = FINTER(KFUNC(IREG),LAMBDA,NPF,TF,DF) 
          UVAR(I,3) = FAC
        ENDDO
      ENDIF
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
       IF (OFF(I) < EM01) OFF(I) = ZERO
       IF (OFF(I) < ONE .AND. OFF(I) > ZERO) OFF(I) = OFF(I)*FOUR_OVER_5
      END DO
C
      ! Initialization of variable
      NINDX = 0  
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE DAMAGE VARIABLE
      !====================================================================       
      DO I=1,NEL
c
        ! Failure strain initialization
        EPS_FAIL = ZERO
c
        ! If the element is not broken
        IF (OFF(I) ==  ONE .AND. DPLA(I) /= ZERO) THEN
c
          ! Computation of loading angle
          MOHR_RADIUS = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2 + SIGNXY(I)**2)
          IF (MOHR_RADIUS > EM20) THEN
            COS2THETA(I) = ((SIGNXX(I)-SIGNYY(I))/TWO)/MOHR_RADIUS
          ELSE
            COS2THETA(I) = ONE
          ENDIF
c
          ! Computation of the BIQUAD parameters
          X_1(1:3) = ZERO
          X_2(1:3) = ZERO
          DO J = 1,NANGLE
            DO K = 1,J              
              X_1(1) = X_1(1) + Q_X11(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_1(2) = X_1(2) + Q_X12(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_1(3) = X_1(3) + Q_X13(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(1) = X_2(1) + Q_X21(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(2) = X_2(2) + Q_X22(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              X_2(3) = X_2(3) + Q_X23(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO          
          ENDDO
c        
          ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
          P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          SXX = SIGNXX(I) - P
          SYY = SIGNYY(I) - P
          SZZ = SIGNZZ(I) - P
          SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
          SVM = SQRT(THREE*SVM)
          triaxs = P/MAX(EM20,SVM)
          IF (triaxs < -TWO_THIRD) triaxs = -TWO_THIRD
          IF (triaxs >  TWO_THIRD) triaxs =  TWO_THIRD
c
          ! Computing the plastic strain at failure according to stress triaxiality
          IF (triaxs <= THIRD) THEN
            EPS_FAIL = X_1(1) + X_1(2)*triaxs + X_1(3)*triaxs**2
            IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
          ELSE
            SELECT CASE (SEL)
              CASE(1)
                EPS_FAIL = X_2(1) + X_2(2)*triaxs + X_2(3)*triaxs**2
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
            END SELECT
          ENDIF
c
          ! Strain-rate effects
          IF (CJC /= ZERO .OR. IRATE /= 0) THEN 
            IF (CJC /= ZERO) THEN 
              FRATE = ONE
              IF (EPSP(I) > EPSD0) FRATE = FRATE + CJC*LOG(EPSP(I)/EPSD0)
            ELSEIF (IRATE /= 0) THEN 
              FRATE = FINTER(KFUNC(IRATE),EPSP(I)/RATE_SCALE,NPF,TF,DF) 
            ENDIF
            EPS_FAIL = EPS_FAIL*FRATE
          ENDIF          
c
          ! Computation of damage variables
          DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
          DFMAX(I) = MIN(ONE,DFMAX(I))
c
          ! Checking element failure using global damage
          IF (DFMAX(I) >= ONE .AND. OFF(I) == ONE) THEN
            OFF(I)      = FOUR_OVER_5
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK    = 1   
            TDELE(I)    = TIME    
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
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
        END DO
      END IF         
c------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (ORTHBIQUAD) el#',I10,
     .          ' AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (ORTHBIQUAD) el#',I10,
     .          ' AT TIME :',1PE12.4)
      END
