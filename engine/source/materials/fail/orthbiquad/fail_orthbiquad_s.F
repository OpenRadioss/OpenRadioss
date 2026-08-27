Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_orthbiquad_s   ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_s.F
!||--- called by ------------------------------------------------------
!||    mmain               ../engine/source/materials/mat_share/mmain.F90
!||    mmain8              ../engine/source/materials/mat_share/mmain8.F
!||    mulaw               ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8              ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid       ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    finter              ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod        ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE FAIL_ORTHBIQUAD_S (FAIL,
     1     NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2     TIME     ,NGL      ,DPLA     ,EPSP     ,OFF      ,    
     3     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4     DFMAX    ,TDELE    ,ALDT     )
C-----------------------------------------------
C   Modules
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
#include      "implicit_f.inc"
C!---------+---------+---+---+--------------------------------------------
C! VAR     | SIZE    |TYP| RW| DEFINITION
C!---------+---------+---+---+--------------------------------------------
C! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C! NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C!---------+---------+---+---+--------------------------------------------
C! TIME    |  1      | F | R | CURRENT TIME
C!---------+---------+---+---+--------------------------------------------
C! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C! ...     |         |   |   |
C! ...     |         |   |   |
C!---------+---------+---+---+--------------------------------------------
C! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C! OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C!---------+---------+---+---+--------------------------------------------
c#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C!-----------------------------------------------
      INTEGER NEL,NUVAR,NVARTMP,NGL(NEL)
      my_real :: TIME
      my_real :: SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),EPSP(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),ALDT(NEL)     
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real, DIMENSION(NEL,NUVAR)  , INTENT(INOUT) :: UVAR
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,II,K,J,NINDX,NINDXF,SEL,NANGLE,IREG,IRATE
      INTEGER, DIMENSION(NEL) :: INDX,INDXF
      my_real 
     .        P,triaxs,SVM,SXX,SYY,SZZ,EPS_FAIL,
     .        P1X,P1Y,S1X,S1Y,A1,B1,C1,REF_EL_LEN,
     .        X_1(3),X_2(3),COS2(10,10),EPSD0,CJC,
     .        MOHR_RADIUS,COS2THETA(NEL)
      my_real SIG1(NEL),SIG2(NEL),MOHR_CENTER
      my_real, dimension(nel) :: lambda,dydx,frate
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
      SEL        = FAIL%IPARAM(1)
      NANGLE     = FAIL%IPARAM(2)
      !
      REF_EL_LEN = FAIL%UPARAM(2)
      EPSD0      = FAIL%UPARAM(3)
      CJC        = FAIL%UPARAM(4)
c
      ! -> Allocation of factors
      call my_alloc(Q_X11, NANGLE, "Q_X11")
      call my_alloc(Q_X12, NANGLE, "Q_X12")
      call my_alloc(Q_X13, NANGLE, "Q_X13")
      call my_alloc(Q_X21, NANGLE, "Q_X21")
      call my_alloc(Q_X22, NANGLE, "Q_X22")
      call my_alloc(Q_X23, NANGLE, "Q_X23")
      ! Recovering factors
      IF (SEL == 3) THEN
        call my_alloc(Q_INST, NANGLE, "Q_INST")
        DO J = 1,NANGLE
          K = 7*(J-1)
          Q_X11(J)  = FAIL%UPARAM(5  + K)
          Q_X12(J)  = FAIL%UPARAM(6  + K)
          Q_X13(J)  = FAIL%UPARAM(7  + K) 
          Q_X21(J)  = FAIL%UPARAM(8  + K)
          Q_X22(J)  = FAIL%UPARAM(9  + K)
          Q_X23(J)  = FAIL%UPARAM(10 + K)
          Q_INST(J) = FAIL%UPARAM(11 + K)
        ENDDO 
      ELSE
        DO J = 1,NANGLE
          K = 6*(J-1)
          Q_X11(J)  = FAIL%UPARAM(5  + K)
          Q_X12(J)  = FAIL%UPARAM(6  + K)
          Q_X13(J)  = FAIL%UPARAM(7  + K) 
          Q_X21(J)  = FAIL%UPARAM(8  + K)
          Q_X22(J)  = FAIL%UPARAM(9  + K)
          Q_X23(J)  = FAIL%UPARAM(10 + K) 
        ENDDO
      ENDIF
      IF (SEL == 3) SEL = 2
c
      ! recovering functions
      irate = 0
      ireg  = 0
      if (fail%table4d(1)%notable > 0) ireg  = 1
      if (fail%table4d(2)%notable > 0) irate = 1     
c      
      ! at initial time, compute the element size regularization factor in uvar(3)
      if (uvar(1,3) == zero .and. ireg > 0) then
        lambda(1:nel) = aldt(1:nel)/ref_el_len
        call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp(1:nel,1:1),lambda,uvar(1:nel,3),dydx)
      endif
c
      ! Strain-rate effects
      FRATE(:) = ONE
      IF (CJC /= ZERO .OR. IRATE == 1) THEN 
        IF (CJC /= ZERO) THEN 
          DO I=1,NEL
            IF (EPSP(i) > EPSD0) FRATE(i) = ONE + CJC*LOG(EPSP(i)/EPSD0)
          END DO
        ELSEIF (IRATE == 1) THEN 
          call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,2:2),epsp,frate,dydx)
        ENDIF
      ENDIF
c
      ! Tag active elements with non nul plastic increment
      NINDXF = 0 
      NINDX  = 0
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. DPLA(I) > ZERO ) THEN
          NINDXF = NINDXF + 1
          INDXF(NINDXF) = I
        ENDIF
      ENDDO
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
       IF (OFF(I) < EM01) OFF(I) = ZERO
       IF (OFF(I) < ONE .AND. OFF(I) > ZERO) OFF(I) = OFF(I)*FOUR_OVER_5
      END DO
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE DAMAGE VARIABLE
      !====================================================================       
      DO II=1,NINDXF
        I = INDXF(II)
c
        ! Failure strain initialization
        EPS_FAIL = ZERO
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
          IF (CJC /= ZERO .OR. IRATE == 1) THEN 
            EPS_FAIL = EPS_FAIL*FRATE(I)
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
            TDELE(I)    = TIME    
          ENDIF
      ENDDO
c------------------------
c------------------------
      ! Deallocation of table
      IF (ALLOCATED(Q_X11)) call my_dealloc(Q_X11)
      IF (ALLOCATED(Q_X12)) call my_dealloc(Q_X12)
      IF (ALLOCATED(Q_X13)) call my_dealloc(Q_X13)
      IF (ALLOCATED(Q_X21)) call my_dealloc(Q_X21)
      IF (ALLOCATED(Q_X22)) call my_dealloc(Q_X22)
      IF (ALLOCATED(Q_X23)) call my_dealloc(Q_X23)
      IF (ALLOCATED(Q_INST)) call my_dealloc(Q_INST)
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
