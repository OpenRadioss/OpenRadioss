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
!||    sigeps103   ../engine/source/materials/mat/mat103/sigeps103.F
!||--- called by ------------------------------------------------------
!||    mulaw       ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
       SUBROUTINE SIGEPS103(
     A      NEL     ,NUPARAM ,NUVAR   ,TIME    ,UPARAM  ,UVAR    , 
     C      RHO0    ,RHO     ,VOLUME  ,EINT    ,EPSD    ,TEMP    ,  
     D      DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     E      EPSXX   ,EPSYY   ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     F      SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     G      SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     H      SIGVXX  ,SIGVYY  ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX  ,
     I      SOUNDSP ,OFF     ,DPDM    ,PLA     ,JTHE    )

C------------------------------------------------------------------------
C HENSEL SPITTEL Material Law
C NUVAR   NUMBER OF USER ELEMENT VARIABLES
C UVAR    USER ELEMENT VARIABLE ARRAY
C         UVAR(I,1)   = THETA  Temperature
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I n p u t   A r g u m e n t s
C----------------------------------------------------------------
      INTEGER NEL, NUPARAM, JTHE,NUVAR
      my_real
     .   TIME       , UPARAM(NUPARAM),
     .   RHO(NEL)   , RHO0(NEL)  , VOLUME(NEL), EINT(NEL),
     .   DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL), DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .   EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL), EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .   SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL), SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .   OFF(NEL)   , DPDM(NEL)  , EPSD(NEL)  
C----------------------------------------------------------------
C  O u t p u t   A r g u m e n t s
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL)
C----------------------------------------------------------------
C  I n p u t  O u t p u t   A r g u m e n t s
C----------------------------------------------------------------
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
      my_real ,DIMENSION(NEL)       ,INTENT(INOUT) :: PLA
      my_real ,DIMENSION(NEL)       ,INTENT(INOUT) :: TEMP
C----------------------------------------------------------------
C  L o c a l  V a r i a b l e s
C----------------------------------------------------------------
      INTEGER :: I
      my_real :: DAVG(NEL), POLD(NEL), THETA(NEL), HM(NEL), EPS(NEL)
      my_real :: YLD(NEL), YLD_H(NEL), YLD_SR(NEL), YLD_T(NEL), SVM(NEL)
      my_real :: A0, EPS0, TIME_FAC, RHOCP, TINI, ETA, T0K, RATIO, DPLA, G, G2, G3, J2
      my_real :: M1, M2, M3, M4, M5, M7
C----------------------------------------------------------------
C  I n i t i a l i z a t i o n
C----------------------------------------------------------------
      A0      = UPARAM(1)
      TIME_FAC= UPARAM(2)
      G       = UPARAM(3)
      M1      = UPARAM(4)
      M2      = UPARAM(5)
      M3      = UPARAM(6)
      M4      = UPARAM(7) 
      M5      = UPARAM(8) 
      M7      = UPARAM(9) 
      RHOCP   = UPARAM(10) 
      TINI    = UPARAM(11) 
      ETA     = UPARAM(12) 
      T0K     = UPARAM(13) 
      EPS0    = UPARAM(14)
      G2      = TWO*G
      G3      = THREE*G

      IF (TIME == ZERO) THEN
        UVAR(1:nel,1) = TINI
      ENDIF

      DO I=1,NEL
         POLD(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
         DAVG(I) =  (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
         EPS(I)  = EPS0 + PLA(I)
      ENDDO  
C-----------------------------------------------------------------------
C  Elastic deviatoric stress tensor
C-----------------------------------------------------------------------
      DO I=1,NEL
         SIGNXX(I)=SIGOXX(I)+POLD(I)+G2*(DEPSXX(I)-DAVG(I))
         SIGNYY(I)=SIGOYY(I)+POLD(I)+G2*(DEPSYY(I)-DAVG(I))
         SIGNZZ(I)=SIGOZZ(I)+POLD(I)+G2*(DEPSZZ(I)-DAVG(I))
         SIGNXY(I)=SIGOXY(I)+G*DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+G*DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+G*DEPSZX(I)
      ENDDO
C--------------------------------------------------------
C  Sound speed                                                   !
C--------------------------------------------------------
      DO I=1,NEL
         DPDM(I)    = DPDM(I) + FOUR_OVER_3*G
         SOUNDSP(I) = SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C--------------------------------------------------------
C  Von Mises equivalent stress
C--------------------------------------------------------
      DO I=1,NEL
         J2 =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)+SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
         SVM(I) =SQRT(THREE*J2)
      ENDDO
C--------------------------------------------------------
C     Conversion of Kelvin to Celsius temp used in equations
C--------------------------------------------------------
      IF (JTHE >= 0) THEN
         THETA(1:NEL) = UVAR(1:NEL,1) - T0K
      ELSE
         THETA(1:NEL) = TEMP(1:NEL)   - T0K
      ENDIF
C--------------------------------------------------------
C  Yield Stress
C--------------------------------------------------------
C Hardening scale factor
      DO I=1,NEL
         YLD_H(I) = ONE
      ENDDO
      IF(M2 /= ZERO) THEN
        DO I=1,NEL
           IF(EPS(I) > ZERO) YLD_H(I) = YLD_H(I)*EPS(I)**M2
        ENDDO
      ENDIF
      IF(M4 /= ZERO) THEN
          DO I=1,NEL
             IF(EPS(I) > ZERO) YLD_H(I) = YLD_H(I) * EXP(M4/EPS(I))
          ENDDO
      ENDIF
      IF(M7 /= ZERO) THEN
          DO I=1,NEL
             IF(EPS(I) > ZERO) YLD_H(I) = YLD_H(I) * EXP(M7*EPS(I))
          ENDDO          
      ENDIF 
C Strain rate scale factor; convert strain rate in second-1
      IF(M3 /= ZERO) THEN
         DO I=1,NEL
           YLD_SR(I) = (EPSD(I)*TIME_FAC)**M3
        ENDDO
      ELSE
        DO I=1,NEL
           YLD_SR(I) = ONE
        ENDDO
      ENDIF
C     temperature scale factor
      IF(M1 /= ZERO .AND. M5 /= ZERO) THEN
              DO I=1,NEL
           YLD_T(I) = EXP(THETA(I)*M1) * (ONE+EPS(I))**(THETA(I)*M5)
        ENDDO
      ELSEIF(M1 /= ZERO) THEN
              DO I=1,NEL
           YLD_T(I) = EXP(THETA(I)*M1)
        ENDDO
      ELSEIF(M5 /= ZERO) THEN
        DO I=1,NEL
           YLD_T(I) = (ONE+EPS(I))**(THETA(I)*M5)
        ENDDO
      ELSE
        DO I=1,NEL
           YLD_T(I) = ONE
        ENDDO
      ENDIF
C Yield Stress
      DO I=1,NEL
         YLD(I) = A0 * YLD_H(I) * YLD_SR(I) * YLD_T(I)
      ENDDO
C----------------------------------------------------------------
C  Compute hardening modulus                   
C----------------------------------------------------------------     
      DO I=1,NEL
         HM(I) = M7*YLD(I)
      ENDDO
            DO I=1,NEL
         IF(EPS(I) > ZERO) HM(I) = HM(I) + YLD(I)*(M2-M4/EPS(I))/EPS(I)
      ENDDO
C----------------------------------------------------------------
C  Update deviatoric stress tensor                   
C----------------------------------------------------------------     
      DO I=1,NEL
         RATIO = MIN(ONE,YLD(I)/ MAX(SVM(I),EM20))
         ! plastic strain increment.
         DPLA = (ONE-RATIO)*SVM(I)/MAX(G3+HM(I),EM20)
         ! actual yield stress
         YLD(I) = MAX(YLD(I)+DPLA*HM(I),ZERO)
         RATIO  = MIN(ONE,YLD(I)/ MAX(SVM(I),EM20))
         SIGNXX(I) = SIGNXX(I)*RATIO
         SIGNYY(I) = SIGNYY(I)*RATIO
         SIGNZZ(I) = SIGNZZ(I)*RATIO
         SIGNXY(I) = SIGNXY(I)*RATIO
         SIGNYZ(I) = SIGNYZ(I)*RATIO
         SIGNZX(I) = SIGNZX(I)*RATIO
         PLA(I) = PLA(I) + DPLA
         THETA(I) = UVAR(I,1) + ETA*YLD(I)*DPLA/RHOCP
      ENDDO
C
      DO I=1,NEL
         UVAR(I,1) = THETA(I)
         SIGVXX(I) = ZERO
         SIGVYY(I) = ZERO
         SIGVZZ(I) = ZERO
         SIGVXY(I) = ZERO
         SIGVYZ(I) = ZERO
         SIGVZX(I) = ZERO
      ENDDO
      IF (JTHE == 0) THEN
        TEMP(1:NEL) = THETA(1:NEL) + T0K
      END IF
!-----------
      RETURN
      END
