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
!||    fail_biquad_ib   ../engine/source/materials/fail/biquad/fail_biquad_ib.F
!||--- called by ------------------------------------------------------
!||    fail_beam18      ../engine/source/elements/beam/fail_beam18.F
!||--- calls      -----------------------------------------------------
!||    finter           ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_BIQUAD_IB(FAIL,NEL      ,NGL      ,
     .           NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     .           TIME     ,OFF      ,DFMAX    ,TDEL     ,      
     .           IOUT     ,ISTDO    ,DAMSCL   ,UELR     ,
     .           IPT      ,FOFF     ,AL       ,NPG      ,        
     .           SIGNXX   ,SIGNXY   ,SIGNXZ   ,DPLA     )
C-----------------------------------------------C-----------------------------------------------
c    BIQUAD- failure model for integrated beams (TYPE 18) IRUP=30
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                     ,INTENT(IN)    :: NEL     ! size of element group
      INTEGER                     ,INTENT(IN)    :: IPT     ! current integration point
      INTEGER                     ,INTENT(IN)    :: NPG    
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit  
      INTEGER                     ,INTENT(IN)    :: NUVAR
      INTEGER                     ,INTENT(IN)    :: NVARTMP
      INTEGER, DIMENSION(NEL,NVARTMP), intent(inout) :: VARTMP
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF    ! integration point deactivation flag
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    ! plastic strain 
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DAMSCL  !1-damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! deactivation time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SIGNXX,SIGNXY,SIGNXZ 
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT):: UVAR
      my_real, DIMENSION(NEL)     ,INTENT(INOUT) :: UELR 
      type (fail_param_) ,target ,intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, J, NINDX, ICOUP, SEL, FAILIP,IREG,NTABLE
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: DCRIT, EXP
      
      my_real P1X,P1Y,S1X,S1Y, A1, B1, C1, REF_EL_LEN,P, SVM,TRIAXS
      my_real X_1(3) , X_2(3)
      my_real EPS_FAIL
      my_real ,dimension(nel) :: lambda,dydx

C=======================================================================
      EPS_FAIL = ZERO
      IREG    = FAIL%IPARAM(1)
      SEL     = FAIL%IPARAM(3)
      ICOUP   = FAIL%IPARAM(4)
      FAILIP  = MIN(FAIL%IPARAM(5),NPG)   !<  number of failed integration point prior to solid element deletion. default = 1 (integer)
      IF (SEL == 3) SEL = 2

      X_1(1) = FAIL%UPARAM(1)
      X_1(2) = FAIL%UPARAM(2)
      X_1(3) = FAIL%UPARAM(3)
      X_2(1) = FAIL%UPARAM(4)
      X_2(2) = FAIL%UPARAM(5)
      X_2(3) = FAIL%UPARAM(6)
      REF_EL_LEN = FAIL%UPARAM(13)
      DCRIT = FAIL%UPARAM(15)
      EXP   = FAIL%UPARAM(16)
!---------------------------------------------------------------------------
!      Element size scale factor on failure plastic strain
!---------------------------------------------------------------------------
       if (ireg > 0) then 
         lambda(1:nel) = al(1:nel) / ref_el_len
         if (nuvar == 3) then 
           if (uvar(1,3) == zero) then
             call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp,lambda,uvar(1:nel,3),dydx)
           endif
         elseif (nuvar == 9) then 
           if (uvar(1,9) == zero) then 
             call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp,lambda,uvar(1:nel,9),dydx)
           endif
         endif
       endif
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO UPDATE DAMAGE VARIABLE
      !====================================================================   
      ! Initialize deleted element counter
      NINDX = 0  
C
      DO I = 1,NEL
        IF (OFF(I) == ONE .AND. FOFF(I) == 1 .AND. DPLA(I) /= ZERO) THEN
            ! Compute hydrostatic stress
          P    = THIRD*SIGNXX(I)
          ! Compute Von Mises stress
          SVM  = SQRT(SIGNXX(I)**2 + THREE*(SIGNXY(I)**2 + SIGNXZ(I)**2))
          ! Compute stress triaxiality
          TRIAXS = P/MAX(EM20,SVM)
          IF (TRIAXS < -TWO_THIRD) TRIAXS = -TWO_THIRD
          IF (TRIAXS >  TWO_THIRD) TRIAXS =  TWO_THIRD
C
          ! Compute the corresponding plastic strain at failure 
          !   -> Low stress triaxialities parabolic curve
          IF (TRIAXS <= THIRD) THEN
            EPS_FAIL = X_1(1) +  X_1(2)*TRIAXS + X_1(3)*TRIAXS**2
            IF ((NUVAR == 3).AND. IREG > 0) EPS_FAIL = EPS_FAIL* UVAR(I,3)
          !   -> High stress triaxiality parabolic curve
          ELSE
            ! Raw curve is used
            IF (SEL == 1) THEN 
              EPS_FAIL = X_2(1) +  X_2(2)*TRIAXS + X_2(3)*TRIAXS**2
              IF ((NUVAR == 3).AND. IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
            ! Plain strain is global minimum
            ELSEIF (SEL == 2) THEN 
              IF (TRIAXS <= ONE/SQR3) THEN                     ! triax < 0.57735
                P1X = THIRD
                P1Y = X_1(1) + X_1(2)*P1X + X_1(3)*P1X**2
                S1X = ONE/SQR3
                S1Y = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                A1  = (P1Y - S1Y)/(P1X - S1X)**2
                B1  = -TWO*A1*S1X
                C1  = A1*S1X**2 + S1Y 
                EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                IF ((NUVAR == 3).AND. IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
              ELSE                                             ! triax > 0.57735
                P1X = TWO*THIRD
                P1Y = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                S1X = ONE/SQR3
                S1Y = X_2(1) + X_2(2)/SQR3  + X_2(3)*(ONE/SQR3)**2
                A1  = (P1Y - S1Y)/(P1X - S1X)**2
                B1  = -TWO*A1*S1X
                C1  = A1*S1X**2 + S1Y 
                EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                IF ((NUVAR == 3).AND. IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
              ENDIF
            ENDIF
          ENDIF
C 
          ! Update damage variable
          DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
          DFMAX(I) = MIN(ONE,DFMAX(I))
          ! Check element failure
          IF (DFMAX(I) >= ONE ) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            TDEL(I)     = TIME
            DFMAX(I)    = ONE
            DAMSCL(I)   = ZERO
            FOFF(I)     = 0 
            UELR(I)     = UELR(I) + ONE
            IF (NINT(UELR(I)) >= FAILIP) then 
              OFF(I)    = FOUR_OVER_5
            ENDIF  
          ENDIF
        ENDIF 
      ENDDO
c------------------------
c     STRESS SOFTENING
c------------------------
      IF (ICOUP == 1) THEN
        DO I = 1,NEL
          IF (DFMAX(I) >= DCRIT .AND.OFF(I) == ONE .AND. FOFF(I) == 1) THEN 
            IF (DCRIT < ONE) THEN 
                DAMSCL(I) = ONE - ((DFMAX(I)-DCRIT)/MAX(ONE-DCRIT,EM20))**EXP
            ELSE
                DAMSCL(I) = ZERO
            ENDIF
          ELSE
            DAMSCL(I) = ONE
          ENDIF
        ENDDO  
      ELSE
        DO I=1,NEL
            DAMSCL(I) = ONE - DFMAX(I)
        END DO
    
      ENDIF

c--------------------      
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)            
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPT,TIME
          WRITE(ISTDO,1000) NGL(I),IPT,TIME
#include "lockoff.inc" 
          IF (OFF(I) == FOUR_OVER_5) THEN
            WRITE(IOUT, 1111)NGL(i),TIME
            WRITE(ISTDO,1111)NGL(i),TIME
          ENDIF
        END DO                   
      END IF   ! NINDX             
c------------------
 1000 FORMAT(5X,'FAILURE (BIQUAD) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4,1X )
 1111 FORMAT(1X,'DELETED BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)    
c------------------
      RETURN
      END

