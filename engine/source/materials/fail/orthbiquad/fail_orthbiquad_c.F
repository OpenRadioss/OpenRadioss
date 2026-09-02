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
!||    fail_orthbiquad_c       ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    my_alloc_mod            ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod          ../common_source/tools/memory/my_dealloc.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_ORTHBIQUAD_C(FAIL       ,
     1           NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2           TIME     ,NGL      ,IPT      ,NPTOT    ,IPG      ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,EPSP     ,UEL1     ,ALDT     ,FOFF     ,
     5           OFF      ,OFFL     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C    ORTHOTROPIC BIQUADRATIC FAILURE MODEL
C-----------------------------------------------
C   Modules
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
c-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C NGL     | NEL     | I | R | ELEMENT NUMBER
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C---------+---------+---+---+--------------------------------------------
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NVARTMP,IPT,NPTOT,IPG
      INTEGER ,INTENT(IN) :: NGL(NEL)
      my_real ,INTENT(IN) :: TIME
      my_real :: DPLA(NEL),EPSP(NEL),UEL1(NEL),ALDT(NEL)
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, DIMENSION(NEL)        , INTENT(INOUT) :: FOFF
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real, DIMENSION(NEL,NUVAR)  , INTENT(INOUT) :: UVAR
      my_real :: OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real hydros,vmises,triaxs,REF_EL_LEN
      INTEGER I,II,J,K,NINDXF,NINDX1,NINDX2,SEL,NANGLE
      INTEGER IREG,IRATE
      INTEGER, DIMENSION(NEL) :: INDXF,INDX1,INDX2
      my_real X_1(3),X_2(3),COS2(10,10),EPSD0,CJC
      my_real EPS_FAIL,EPS_FAIL2,DAMAGE,INST,MOHR_RADIUS
      my_real P1X,P1Y,S1X,S1Y,S2Y,A1,A2,B1,B2,C1,C2,COS2THETA(NEL)
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
C-----------------------------------------------
c! UVAR1 = damage due to instability (triax between 1/3 and 2/3)
c! UVAR2 = actual integration point ON or OFF
c! UVAR3 = element size regularization factor
C-----------------------------------------------
c      
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
   
      FRATE(1:nel) = -HUGE(FRATE(1:nel))
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
          Q_X11(J)  = FAIL%UPARAM(5  + 7*(J-1))
          Q_X12(J)  = FAIL%UPARAM(6  + 7*(J-1))
          Q_X13(J)  = FAIL%UPARAM(7 + 7*(J-1)) 
          Q_X21(J)  = FAIL%UPARAM(8 + 7*(J-1))
          Q_X22(J)  = FAIL%UPARAM(9 + 7*(J-1))
          Q_X23(J)  = FAIL%UPARAM(10 + 7*(J-1))
          Q_INST(J) = FAIL%UPARAM(11 + 7*(J-1))
        ENDDO 
      ELSE
        DO J = 1,NANGLE
          Q_X11(J)  = FAIL%UPARAM(5  + 6*(J-1))
          Q_X12(J)  = FAIL%UPARAM(6  + 6*(J-1))
          Q_X13(J)  = FAIL%UPARAM(7 + 6*(J-1)) 
          Q_X21(J)  = FAIL%UPARAM(8 + 6*(J-1))
          Q_X22(J)  = FAIL%UPARAM(9 + 6*(J-1))
          Q_X23(J)  = FAIL%UPARAM(10 + 6*(J-1)) 
        ENDDO
      ENDIF
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
      else
        uvar(1:nel,3) = one
      endif
c
      ! Strain-rate effects
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
      NINDXF  = 0 
      NINDX1 = 0
      NINDX2 = 0
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1 .AND. DPLA(I) > ZERO ) THEN
          NINDXF = NINDXF + 1
          INDXF(NINDXF) = I
        ENDIF
      ENDDO 
c      
      !====================================================================
      ! - COMPUTE DAMAGE VARIABLE
      !====================================================================
      do ii = 1,NINDXF
        i = indxf(ii)
        ! Failure strain initialization
        EPS_FAIL  = ZERO
        EPS_FAIL2 = ZERO
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
            EPS_FAIL = EPS_FAIL*FRATE(i)
            IF (SEL == 3) EPS_FAIL2 = EPS_FAIL2*FRATE(i)
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
