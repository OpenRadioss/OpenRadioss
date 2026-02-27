Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    fail_biquad_s           ../engine/source/materials/fail/biquad/fail_biquad_s.F
!||--- called by ------------------------------------------------------
!||    mmain                   ../engine/source/materials/mat_share/mmain.F90
!||    mmain8                  ../engine/source/materials/mat_share/mmain8.F
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    finter                  ../engine/source/tools/curve/finter.F
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_BIQUAD_S(FAIL  ,
     1  NEL      ,NUVAR    ,MFUNC    ,KFUNC    ,ALDT     ,
     2  NPF      ,TF       ,TIME     ,TDELE    ,
     3  NGL      ,DPLA     ,UVAR     ,OFF      ,DFMAX    ,DMGSCL   ,
     4  SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   )
C-----------------------------------------------
C    Biquadratic failure model for solids
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
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
      INTEGER, INTENT(IN) :: NEL,NUVAR,NGL(NEL)
      my_real, INTENT(IN) :: TIME,
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),ALDT(NEL)
      my_real, INTENT(INOUT) :: UVAR(NEL,NUVAR),DFMAX(NEL),
     .   TDELE(NEL),OFF(NEL),DMGSCL(NEL)
      type (fail_param_) ,target ,intent(inout) :: fail    !< failure model data structure
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER NPF(SNPC), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(STF)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,SFLAG,FLAG_PERTURB,ICOUP
      INTEGER IREG
      INTEGER ,DIMENSION(NEL,1) :: IPOS
      my_real ,DIMENSION(NEL,1) :: ETA
      my_real ,DIMENSION(NEL)   :: REG_FAC,regf_eta,DERI
      my_real 
     .  DF,PRESSURE,SVM,TRIAX,SXX,SYY,SZZ,EPS_FAIL,P1X,P1Y,S1X,S1Y,
     .  A1,B1,C1,REF_EL_LEN,LAMBDA,X_1(3),X_2(3),DCRIT,EXP
C===================================================================================================
      IREG  = FAIL%IPARAM(1)
      SFLAG = FAIL%IPARAM(3)
      ICOUP = FAIL%IPARAM(4)
      IF (SFLAG == 3) SFLAG = 2
!
      X_1(1) = FAIL%UPARAM(1)
      X_1(2) = FAIL%UPARAM(2)
      X_1(3) = FAIL%UPARAM(3)
      X_2(1) = FAIL%UPARAM(4)
      X_2(2) = FAIL%UPARAM(5)
      X_2(3) = FAIL%UPARAM(6)
      FLAG_PERTURB = NINT(FAIL%UPARAM(8))
      REF_EL_LEN = FAIL%UPARAM(13)
      DCRIT = FAIL%UPARAM(15)
      EXP   = FAIL%UPARAM(16)
      EPS_FAIL = ZERO
!---------------------------------------------------------------------------------------------------
      ! At initial time, compute the element size regularization factor
      IF (MFUNC > 0) THEN 
        IF (NUVAR == 3) THEN 
          IF (UVAR(1,3) == ZERO) THEN 
            DO I = 1,NEL
              LAMBDA = ALDT(I) / REF_EL_LEN
              UVAR(I,3) = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
            ENDDO
          ENDIF
          REG_FAC(1:NEL) = UVAR(1:NEL,3)
        ELSEIF (NUVAR == 9) THEN 
          IF (UVAR(1,9) == ZERO) THEN 
            DO I = 1,NEL
              LAMBDA = ALDT(I) / REF_EL_LEN
              UVAR(I,9) = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
            ENDDO
          ENDIF
          REG_FAC(1:NEL) = UVAR(1:NEL,9)
        ENDIF
      ENDIF
!---------------------------------------------------------------------------------------------------
       ! calculate triaxiality
       do i=1,nel
          pressure = (signxx(I) + signyy(I) + signzz(I)) * third
          sxx = signxx(I) - pressure
          syy = signyy(I) - pressure
          szz = signzz(I) - pressure
          svm = half*(sxx**2 + syy**2 + szz**2) + signxy(I)**2 + signzx(I)**2 + signyz(I)**2
          svm = sqrt(three*svm)
!
          ! Compressureute stress triaxiality
          triax = pressure / max(em20,svm)
          triax = max(triax, -two_third)
          triax = min(triax,  two_third)
          eta(i,1) = triax
       end do
       IF (IREG > 1) THEN
         ! calculate triaxiality dependent scaling factor on failure limits
         ipos(1:nel,1) = 1
         call table_mat_vinterp(fail%table4d(1),nel,nel,ipos,eta,regf_eta,deri)
         reg_fac(1:nel) = (one - regf_eta(1:nel)) + regf_eta(1:nel)*reg_fac(1:nel)
       END IF
!---------------------------------------------------------------------------------------------------
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
            ! Compute the corresponding plastic strain at failure 
            !   -> Low stress triaxialities parabolic curve
            triax = eta(i,1)
            IF (TRIAX <= THIRD) THEN
              EPS_FAIL = X_1(1) +  X_1(2)*TRIAX + X_1(3)*TRIAX**2
              IF (IREG > 0) EPS_FAIL = EPS_FAIL*UVAR(I,3)
            !   -> High stress triaxiality parabolic curve
            ELSE
              ! Raw curve is used
              IF (SFLAG == 1) THEN 
                EPS_FAIL = X_2(1) +  X_2(2)*TRIAX + X_2(3)*TRIAX**2
                IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
              ! Plain strain is global minimum
              ELSEIF (SFLAG == 2) THEN 
                IF (TRIAX <= ONE/SQR3) THEN                     ! triax < 0.57735
                  P1X = THIRD
                  P1Y = X_1(1) + X_1(2)*P1X + X_1(3)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = X_2(1) + X_2(2)/SQR3 + X_2(3)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAX + A1*TRIAX**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
                ELSE                                             ! triax > 0.57735
                  P1X = TWO*THIRD
                  P1Y = X_2(1) + X_2(2)*P1X + X_2(3)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = X_2(1) + X_2(2)/SQR3  + X_2(3)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAX + A1*TRIAX**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
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
              TDELE(I) = TIME    
            ENDIF
          ENDIF 
        ENDDO
      ! -> Perturbation flag is used, parameters vary for each Gauss points
      ELSE 
        DO I = 1,NEL
          IF (OFF(I) == ONE .AND. DPLA(I) /= ZERO) THEN
            triax = eta(i,1)
            ! Compute the corresponding plastic strain at failure 
            !   -> Low stress triaxialities parabolic curve
            IF (TRIAX <= THIRD) THEN
              EPS_FAIL = UVAR(I,3) + UVAR(I,4)*TRIAX + UVAR(I,5)*TRIAX**2
              IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
            !   -> High stress triaxiality parabolic curve
            ELSE
              ! Raw curve is used
              IF (SFLAG == 1) THEN 
                EPS_FAIL = UVAR(I,6) +  UVAR(I,7)*TRIAX + UVAR(I,8)*TRIAX**2
                IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
              ! Plain strain is global minimum
              ELSEIF (SFLAG == 2) THEN 
                IF (TRIAX <= ONE/SQR3) THEN                     ! triax < 0.57735
                  P1X = THIRD
                  P1Y = UVAR(I,3) + UVAR(I,4)*P1X + UVAR(I,5)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = UVAR(I,6) + UVAR(I,7)/SQR3 + UVAR(I,8)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAX + A1*TRIAX**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
                ELSE                                             ! triax > 0.57735
                  P1X = TWO*THIRD
                  P1Y = UVAR(I,3) + UVAR(I,4)*P1X + UVAR(I,5)*P1X**2
                  S1X = ONE/SQR3
                  S1Y = UVAR(I,6) + UVAR(I,7)/SQR3 + UVAR(I,8)*(ONE/SQR3)**2
                  A1  = (P1Y - S1Y)/(P1X - S1X)**2
                  B1  = -TWO*A1*S1X
                  C1  = A1*S1X**2 + S1Y 
                  EPS_FAIL = C1 + B1*TRIAX + A1*TRIAX**2
                  IF (IREG > 0) EPS_FAIL = EPS_FAIL*REG_FAC(I)
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
