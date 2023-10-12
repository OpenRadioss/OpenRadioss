Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  FAIL_BIQUAD_IB                source/materials/fail/biquad/fail_biquad_ib.F
Chd|-- called by -----------
Chd|        FAIL_BEAM18                   source/elements/beam/fail_beam18.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_BIQUAD_IB(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,OFF      ,DFMAX    ,TDEL     ,      
     .           IOUT     ,ISTDO    ,NFUNC    ,IFUNC    ,DAMSCL   ,
     .           UVAR     ,NUVAR    ,SNPC     ,NPF      ,
     .           STF      ,TF       ,IPT      ,FOFF     ,          
     .           SIGNXX   ,SIGNXY   ,SIGNXZ   ,DPLA   ,AL)
C-----------------------------------------------C-----------------------------------------------
c    BIQUAD- failure model for integrated beams (TYPE 18) IRUP=30
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
      INTEGER                     ,INTENT(IN)    :: NUPARAM ! size of parameter array
      INTEGER                     ,INTENT(IN)    :: IPT     ! current integration point
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit
      INTEGER                     ,INTENT(IN)    :: NFUNC   ! number of functions
      INTEGER                     ,INTENT(IN)    :: SNPC 
      INTEGER                     ,INTENT(IN)    :: STF       
      INTEGER                     ,INTENT(IN)    :: NUVAR     
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(100)     ,INTENT(IN)    :: IFUNC   ! table of functions identifiers
      INTEGER ,DIMENSION(SNPC)    ,INTENT(IN)    :: NPF
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF    ! integration point desactivation flag
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    ! plastic strain 
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(STF)     ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DAMSCL  !1-damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SIGNXX,SIGNXY,SIGNXZ 
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,NINDXD,ICOUP,SEL
      INTEGER ,DIMENSION(NEL) :: INDX,INDXD
      my_real :: DCRIT, EXP,D1,DF,TRIAX,SCALE,SXX,SYY,SZZ
      
      my_real P1X,P1Y,S1X,S1Y,S2Y, A1, B1, C1, REF_EL_LEN, LAMBDA,FAC ,
     .        P , SVM,TRIAXS
      my_real X_1(3) , X_2(3)
      EXTERNAL FINTER
      my_real EPS_FAIL,FINTER
      my_real ,DIMENSION(NEL) :: DAMAGE
C=======================================================================
      X_1(1) = UPARAM(1)
      X_1(2) = UPARAM(2)
      X_1(3) = UPARAM(3)
      X_2(1) = UPARAM(4)
      X_2(2) = UPARAM(5)
      X_2(3) = UPARAM(6)
      
      SEL    = INT(UPARAM(11)+0.0001)
      IF (SEL == 3) SEL = 2
      REF_EL_LEN = UPARAM(13)
      ICOUP = NINT(UPARAM(14))
      DCRIT = UPARAM(15)
      EXP   = UPARAM(16)
C
      ! At initial time, compute the element size regularization factor
      IF (NFUNC > 0) THEN 
        IF (NUVAR == 3) THEN 
          IF (UVAR(1,3) == ZERO) THEN 
            DO I = 1,NEL
              UVAR(I,3) = AL(I)  
              LAMBDA    = AL(I)  / REF_EL_LEN
              FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,3) = FAC
            ENDDO
          ENDIF
        ELSEIF (NUVAR == 9) THEN 
          IF (UVAR(1,9) == ZERO) THEN 
            DO I = 1,NEL
              UVAR(I,9) = AL(I) 
              LAMBDA = UVAR(I,9) / REF_EL_LEN
              FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,9) = FAC
            ENDDO
          ENDIF
        ENDIF
      ENDIF
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
          SVM  = SQRT(THREE*(HALF*SIGNXX(I)**2 + SIGNXY(I)**2 + SIGNXZ(I)**2))
          ! Compute stress triaxiality
          TRIAXS = P/MAX(EM20,SVM)
          IF (TRIAXS < -TWO_THIRD) TRIAXS = -TWO_THIRD
          IF (TRIAXS >  TWO_THIRD) TRIAXS =  TWO_THIRD
C
          ! Compute the corresponding plastic strain at failure 
          !   -> Low stress triaxialities parabolic curve
          IF (TRIAXS <= THIRD) THEN
            EPS_FAIL = X_1(1) +  X_1(2)*TRIAXS + X_1(3)*TRIAXS**2
            IF ((NUVAR == 3).AND.(NFUNC > 0)) EPS_FAIL = EPS_FAIL* UVAR(I,3)
          !   -> High stress triaxiality parabolic curve
          ELSE
            ! Raw curve is used
            IF (SEL == 1) THEN 
              EPS_FAIL = X_2(1) +  X_2(2)*TRIAXS + X_2(3)*TRIAXS**2
              IF ((NUVAR == 3).AND.(NFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
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
                IF ((NUVAR == 3).AND.(NFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
              ELSE                                             ! triax > 0.57735
                P1X = TWO*THIRD
                P1Y = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                S1X = ONE/SQR3
                S1Y = X_2(1) + X_2(2)/SQR3  + X_2(3)*(ONE/SQR3)**2
                A1  = (P1Y - S1Y)/(P1X - S1X)**2
                B1  = -TWO*A1*S1X
                C1  = A1*S1X**2 + S1Y 
                EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                IF ((NUVAR == 3).AND.(NFUNC> 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
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
        END DO                   
      END IF   ! NINDX             
c------------------
 1000 FORMAT(5X,'FAILURE (BIQUAD) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4,1X )
 2000 FORMAT(5X,'START DAMAGE (BIQUAD) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4 )     
c------------------
      RETURN
      END

