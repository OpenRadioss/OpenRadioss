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
Chd|  FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_BIQUAD_S(
     1  NEL      ,NUPARAM  ,NUVAR    ,MFUNC    ,KFUNC    ,ALDT     ,
     2  NPF      ,TF       ,TIME     ,UPARAM   ,TDELE    ,
     3  NGL      ,DPLA     ,UVAR     ,OFF      ,DFMAX    ,DMGSCL   ,
     4  SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr17_c.inc"
#include "tabsiz_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,NGL(NEL)
      my_real, INTENT(IN) :: TIME,UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),ALDT(NEL)
      my_real, INTENT(INOUT) :: UVAR(NEL,NUVAR),DFMAX(NEL),
     .   TDELE(NEL),OFF(NEL),DMGSCL(NEL)
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER NPF(SNPC), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(STF)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,SEL,FLAG_PERTURB,ICOUP
      my_real 
     .  DF,P,TRIAXS,SVM,SXX,SYY,SZZ,EPS_FAIL,P1X,P1Y,S1X,S1Y,
     .  S2Y,A1,B1,C1,REF_EL_LEN,LAMBDA,FAC,X_1(3),X_2(3),DCRIT,EXP
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      X_1(1) = UPARAM(1)
      X_1(2) = UPARAM(2)
      X_1(3) = UPARAM(3)
      X_2(1) = UPARAM(4)
      X_2(2) = UPARAM(5)
      X_2(3) = UPARAM(6)
      FLAG_PERTURB = UPARAM(8)
      SEL    = INT(UPARAM(11)+0.0001)
      IF (SEL == 3) SEL = 2
      REF_EL_LEN = UPARAM(13)
      ICOUP = NINT(UPARAM(14))
      DCRIT = UPARAM(15)
      EXP   = UPARAM(16)
C
      ! At initial time, compute the element size regularization factor
      IF (MFUNC > 0) THEN 
        IF (NUVAR == 3) THEN 
          IF (UVAR(1,3) == ZERO) THEN 
            DO I = 1,NEL
              UVAR(I,3) = ALDT(I) 
              LAMBDA = UVAR(I,3) / REF_EL_LEN
              FAC = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,3) = FAC
            ENDDO
          ENDIF
        ELSEIF (NUVAR == 9) THEN 
          IF (UVAR(1,9) == ZERO) THEN 
            DO I = 1,NEL
              UVAR(I,9) = ALDT(I) 
              LAMBDA = UVAR(I,9) / REF_EL_LEN
              FAC = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,9) = FAC
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
      ! Update element deletion flag
      DO I = 1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < ONE .AND. OFF(I) > ZERO) OFF(I) = OFF(I)*FOUR_OVER_5
      END DO
C
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO UPDATE DAMAGE VARIABLE
      !====================================================================   
      ! Initialize deleted element counter
      NINDX = 0  
C
      ! -> If perturbation option is not used, constant parameters
      IF (FLAG_PERTURB == 0) THEN
        DO I = 1,NEL
          IF (OFF(I) == ONE .AND. DPLA(I) /= ZERO) THEN
            ! Compute hydrostatic stress
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
C
            ! Compute Von Mises stress
            SXX = SIGNXX(I) - P
            SYY = SIGNYY(I) - P
            SZZ = SIGNZZ(I) - P
            SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
            SVM = SQRT(THREE*SVM)
C
            ! Compute stress triaxiality
            TRIAXS = P/MAX(EM20,SVM)
            IF (TRIAXS < -TWO_THIRD) TRIAXS = -TWO_THIRD
            IF (TRIAXS >  TWO_THIRD) TRIAXS =  TWO_THIRD
C
            ! Compute the corresponding plastic strain at failure 
            !   -> Low stress triaxialities parabolic curve
            IF (TRIAXS <= THIRD) THEN
              EPS_FAIL = X_1(1) +  X_1(2)*TRIAXS + X_1(3)*TRIAXS**2
              IF ((NUVAR == 3).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
            !   -> High stress triaxiality parabolic curve
            ELSE
              ! Raw curve is used
              IF (SEL == 1) THEN 
                EPS_FAIL = X_2(1) +  X_2(2)*TRIAXS + X_2(3)*TRIAXS**2
                IF ((NUVAR == 3).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
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
                  IF ((NUVAR == 3).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                ELSE                                             ! triax > 0.57735
                  P1X = TWO*THIRD
                  P1Y = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = X_2(1) + X_2(2)/SQR3  + X_2(3)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                  IF ((NUVAR == 3).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,3)
                ENDIF
              ENDIF
            ENDIF
C 
            ! Update damage variable
            DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
            DFMAX(I) = MIN(ONE,DFMAX(I))
            ! Check element failure
            IF (DFMAX(I) >= ONE .AND. OFF(I) == ONE) THEN
              OFF(I) = FOUR_OVER_5
              NINDX  = NINDX+1
              INDX(NINDX) = I
              IDEL7NOK = 1   
              TDELE(I) = TIME    
            ENDIF
          ENDIF 
        ENDDO
      ! -> Perturbation flag is used, parameters vary for each Gauss points
      ELSE 
        DO I = 1,NEL
          IF (OFF(I) == ONE .AND. DPLA(I) /= ZERO) THEN
            ! Compute hydrostatic stress
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
C
            ! Compute Von Mises stress
            SXX = SIGNXX(I) - P
            SYY = SIGNYY(I) - P
            SZZ = SIGNZZ(I) - P
            SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
            SVM = SQRT(THREE*SVM)
C
            ! Compute stress triaxiality
            TRIAXS = P/MAX(EM20,SVM)
            IF (TRIAXS < -TWO_THIRD) TRIAXS = -TWO_THIRD
            IF (TRIAXS >  TWO_THIRD) TRIAXS =  TWO_THIRD
C
            ! Compute the corresponding plastic strain at failure 
            !   -> Low stress triaxialities parabolic curve
            IF (TRIAXS <= THIRD) THEN
              EPS_FAIL = UVAR(I,3) + UVAR(I,4)*TRIAXS + UVAR(I,5)*TRIAXS**2
              IF ((NUVAR == 9).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,9)
            !   -> High stress triaxiality parabolic curve
            ELSE
              ! Raw curve is used
              IF (SEL == 1) THEN 
                EPS_FAIL = UVAR(I,6) +  UVAR(I,7)*TRIAXS + UVAR(I,8)*TRIAXS**2
                IF ((NUVAR == 9).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,9)
              ! Plain strain is global minimum
              ELSEIF (SEL == 2) THEN 
                IF (TRIAXS <= ONE/SQR3) THEN                     ! triax < 0.57735
                  P1X = THIRD
                  P1Y = UVAR(I,3) + UVAR(I,4)*P1X + UVAR(I,5)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = UVAR(I,6) + UVAR(I,7)/SQR3 + UVAR(I,8)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                  IF ((NUVAR == 9).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,9)
                ELSE                                             ! triax > 0.57735
                  P1X = TWO*THIRD
                  P1Y = UVAR(I,3) + UVAR(I,4)*P1X + UVAR(I,5)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = UVAR(I,6) + UVAR(I,7)/SQR3 + UVAR(I,8)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAXS + A1*TRIAXS**2
                  IF ((NUVAR == 9).AND.(MFUNC > 0)) EPS_FAIL = EPS_FAIL*UVAR(I,9)
                ENDIF
              ENDIF
            ENDIF
C 
            ! Update damage variable
            DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
            DFMAX(I) = MIN(ONE,DFMAX(I))
            ! Check element failure
            IF (DFMAX(I) >= ONE .AND. OFF(I) == ONE) THEN
              OFF(I) = FOUR_OVER_5
              NINDX  = NINDX+1
              INDX(NINDX) = I
              IDEL7NOK = 1   
              TDELE(I) = TIME    
            ENDIF
          ENDIF 
        ENDDO
      ENDIF
c
c------------------------
c     STRESS SOFTENING
c------------------------
      IF (ICOUP == 1) THEN
        DO I = 1,NEL
          IF (DFMAX(I) >= DCRIT) THEN 
            IF (DCRIT < ONE) THEN 
              DMGSCL(I) = ONE - ((DFMAX(I)-DCRIT)/MAX(ONE-DCRIT,EM20))**EXP
            ELSE
              DMGSCL(I) = ZERO
            ENDIF
          ELSE
            DMGSCL(I) = ONE
          ENDIF
        ENDDO
      ENDIF
c
      !====================================================================
      ! - PRINTOUT ELEMENT FAILURE MESSAGES
      !====================================================================
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF         
C------------------
 1000 FORMAT(1X,' -- RUPTURE OF SOLID ELEMENT (BIQUAD)',I10,
     .          ' AT TIME : ',1PE12.4)     
 1100 FORMAT(1X,' -- RUPTURE OF SOLID ELEMENT (BIQUAD)',I10,
     .          ' AT TIME : ',1PE12.4)     
      END
