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
Chd|  SIGEPS111                     source/materials/mat/mat111/sigeps111.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        CARDAN_METHOD                 source/materials/mat/mat111/sigeps111.F
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
       SUBROUTINE SIGEPS111(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   , NGL     ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ET  ,
     B      IHET   , OFFG   , EPSTH3  , IEXPAN ,MFXX   ,MFXY  ,
     C      MFXZ   , MFYX   , MFYY    , MFYZ   ,MFZX   ,MFZY  ,
     D      MFZZ     )                  
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER     ::   NEL,     NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,IEXPAN
      my_real     ::   TIME    , TIMESTEP   , UPARAM(NUPARAM)
      my_real , DIMENSION(NEL) ::  RHO   , RHO0  , VOLUME, EINT,
     .                             EPSPXX, EPSPYY, EPSPZZ, EPSPXY,
     .                             EPSPYZ, EPSPZX, DEPSXX, DEPSYY,
     .                             DEPSZZ, DEPSXY, DEPSYZ, DEPSZX,
     .                             EPSXX , EPSYY , EPSZZ , EPSXY ,
     .                             EPSYZ , EPSZX , SIGOXX, SIGOYY,
     .                             SIGOZZ, SIGOXY, SIGOYZ, SIGOZX,
     .                             OFFG  , EPSTH3,
     .                             MFXX  , MFXY  , MFXZ  , MFYX  ,
     .                             MFYY  , MFYZ  , MFZX  , MFZY  ,
     .                             MFZZ
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real , DIMENSION(NEL) :: SIGNXX , SIGNYY , SIGNZZ,
     .                            SIGNXY , SIGNYZ , SIGNZX,
     .                            SIGVXX , SIGVYY , SIGVZZ,
     .                            SIGVXY , SIGVYZ , SIGVZX,
     .                            SOUNDSP, VISCMAX, ET
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real , DIMENSION(NEL) :: OFF
      my_real , DIMENSION(NEL,NUVAR) ::  UVAR
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER  :: NPF(*), NFUNC, IFUNC(NFUNC)
      my_real  :: FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER ::   I,J,ITEST,ONLY_TENSION_DATA
      INTEGER , DIMENSION(NEL) :: ICOMP
      my_real :: DW_DI,L2,L3,L5,DELTA,
     .           G,RBULK,SCALE,T,EPST,DF,DU,
     .           TRACE,P,FAC,INVJDETF,NU,GINI,RBULKINI,DIFF
      my_real :: B(NEL,6),LAM2(NEL,3),
     .           LAMBDA(NEL,3),EVD(3),BD(NEL,6)
      my_real ,DIMENSION(NEL) :: JDETF,I1,I1B,LAMT,J2THIRD,
     .                           T1,T2,T3,ETI,GS,K
      my_real, DIMENSION(NEL,3,3) :: F,FFT
c----------------------------------------------------------------
c     UVAR(1) = engineering strain
c     UVAR(2) = engineering stress ! for unixial test only for now
c----------------------------------------------------------------
c       material parameters
        NU       = UPARAM(1)
        ITEST    = NINT(UPARAM(2))
        SCALE    = UPARAM(3)
        G        = UPARAM(4)
        RBULK    = UPARAM(5)
        GINI     = UPARAM(6)
        ONLY_TENSION_DATA = NINT(UPARAM(7))  ! == 1 existing only tension data
C           
       DO I=1,NEL       
        F(I,1,1)  = ONE + MFXX(I)
        F(I,2,2)  = ONE + MFYY(I)
        F(I,3,3)  = ONE + MFZZ(I)
        F(I,1,2)  = MFXY(I)
        F(I,2,3)  = MFYZ(I)
        F(I,3,1)  = MFZX(I)     
        F(I,2,1)  = MFYX(I)
        F(I,3,2)  = MFZY(I)
        F(I,1,3)  = MFXZ(I)     
       ENDDO 
C
      CALL PRODAAT(F , FFT, NEL) ! B = F * FT

      DO I=1,NEL
         B(I,1) = FFT(I,1,1)
         B(I,2) = FFT(I,2,2)
         B(I,3) = FFT(I,3,3)
         B(I,4) = FFT(I,1,2)
         B(I,5) = FFT(I,2,3)
         B(I,6) = FFT(I,3,1)
      ENDDO
      ICOMP(1:NEL) = 0
      DO I = 1,NEL
         ! JDETF = RHO0/RHO = RELATIVE VOLUME = DET F (F = GRADIENT OF DEFORMATION)
         JDETF(I) = F(I,1,1)*F(I,2,2)*F(I,3,3) - F(I,1,1)*F(I,2,3)*F(I,3,2) 
     .            - F(I,3,3)*F(I,1,2)*F(I,2,1) + F(I,1,2)*F(I,2,3)*F(I,3,1)
     .            + F(I,2,1)*F(I,3,2)*F(I,1,3) - F(I,2,2)*F(I,3,1)*F(I,1,3)
         !FIRST INVARIANT of B=FFT
         I1B(I) = FFT(I,1,1) + FFT(I,2,2) + FFT(I,3,3)
C
         IF (JDETF(I) > ZERO) THEN
            J2THIRD(I) = EXP((-TWO_THIRD )*LOG(JDETF(I)))
         ELSE
            J2THIRD(I) = ZERO
         ENDIF
C         
         TRACE = (B(I,1)+B(I,2)+ B(I,3))*THIRD
         BD(I,1) = J2THIRD(I)*(B(I,1) - TRACE)         
         BD(I,2) = J2THIRD(I)*(B(I,2) - TRACE)         
         BD(I,3) = J2THIRD(I)*(B(I,3) - TRACE) 
         BD(I,4:6) = J2THIRD(I)*B(I,4:6) 
         I1(I) = J2THIRD(I)*I1B(I)
       ENDDO
       
       IF(ONLY_TENSION_DATA == 0) THEN
         DO I = 1,NEL
           DIFF = JDETF(I) - ONE
           IF(DIFF < ZERO) ICOMP(I) = 1
        ENDDO
       ENDIF
C       
       SELECT CASE (ITEST)
         
         CASE (1) ! Uniaxial traction test (tension)
        !
        !     solving lt**3 - I*lt +2 = 0
        !
              !!CALL CARDAN_METHOD(NEL,I1B,LAMT) 
              CALL CARDAN_METHOD(ITEST,NEL,I1,ICOMP,LAMT)
              
              DO I=1,NEL
                INVJDETF = ONE/MAX(EM20, JDETF(I))
                EPST = LAMT(I) - ONE
                T = SCALE*FINTER(IFUNC(1),EPST,NPF,TF,DF)
                L2 = LAMT(I)**2
                DW_DI = ZERO
                IF (I1B(I) == THREE .OR. LAMT(I) == ONE) THEN
                  DW_DI = ZERO
                ELSEIF((LAMT(I)*L2 - ONE) /= ZERO)THEN
                  DW_DI = T*L2 / (LAMT(I)*L2 - ONE)
                ENDIF
                GS(I)= MAX(GINI,DF*SCALE)
                K(I) = TWO*GS(I)*(ONE+NU) / MAX(EM20,THREE*(ONE-TWO*NU))
                P = RBULK*(JDETF(I) - ONE) 
                SIGNXX(I) = INVJDETF*DW_DI * BD(I,1) + P
                SIGNYY(I) = INVJDETF*DW_DI * BD(I,2) + P
                SIGNZZ(I) = INVJDETF*DW_DI * BD(I,3) + P
                SIGNXY(I) = INVJDETF*DW_DI * BD(I,4)
                SIGNYZ(I) = INVJDETF*DW_DI * BD(I,5)
                SIGNZX(I) = INVJDETF*DW_DI * BD(I,6)
                UVAR(I,1) = LAMT(I) - ONE
                UVAR(I,2) = JDETF(I)*SIGNZZ(I) / LAMT(I) ! only for testing
              ENDDO
          CASE(2)  ! equibiaxail traction test 
        !
        !     solving 2*lt**6 - I*lt**4 + 1 = 0
        !      By considering lt**2 = x --> 2*x**3 - I*x**2 +1 = 0
        !      by considering z = 1/x we get z**3 - I*Z + 2 = 0
        !      Z ---> lt = sqrt(1/z) 
!!             
              CALL CARDAN_METHOD(ITEST,NEL,I1,ICOMP,LAMT)
!!              
              DO I=1,NEL
                INVJDETF = ONE/MAX(EM20, JDETF(I))
                LAMT(I) = ONE/MAX(EM20,LAMT(I))
                LAMT(I) = SQRT(LAMT(I))
                EPST = LAMT(I) - ONE
                T = SCALE*FINTER(IFUNC(1),EPST,NPF,TF,DF)
                L5 = LAMT(I)**(-5)
                DW_DI =  ZERO
               IF (I1B(I) == THREE .OR. LAMT(I) == ONE) THEN
                  DW_DI = ZERO
                ELSEIF((LAMT(I)  - L5) /= ZERO) THEN
                  DW_DI = T / (LAMT(I)  - L5)
                ENDIF
                GS(I)= MAX(GINI,DF*SCALE)
                K(I) = TWO*GS(I)*(ONE+NU) / MAX(EM20,THREE*(ONE-TWO*NU))
                P = RBULK*(JDETF(I) - ONE) 
                SIGNXX(I) = INVJDETF*DW_DI * BD(I,1) + P  
                SIGNYY(I) = INVJDETF*DW_DI * BD(I,2) + P  
                SIGNZZ(I) = INVJDETF*DW_DI * BD(I,3) + P  
                SIGNXY(I) = INVJDETF*DW_DI * BD(I,4)      
                SIGNYZ(I) = INVJDETF*DW_DI * BD(I,5)      
                SIGNZX(I) = INVJDETF*DW_DI * BD(I,6)
                 UVAR(I,1) = LAMT(I) - ONE
                 UVAR(I,2) = JDETF(I)*SIGNZZ(I) /  LAMT(I) 
              ENDDO   
          CASE(3) ! planar traction test
          
        !
        !     solving lt**4 +(1- I)*lt**2 + 1 = 0
        !      By considering lt**2 = x --> x**2 +(1-I)*x +1 = 0
        !       lt = sqrt(X) 
!!             
              DO I=1,NEL
                 DELTA = (ONE - I1(I)) ** 2 - FOUR
                 LAMT(I) = ONE
                 IF(DELTA > 0) THEN
                   LAMT(I) = HALF*MAX(I1(I)+ SQRT(DELTA) - ONE ,I1(I) - SQRT(DELTA) - ONE)
                 ELSEIF(DELTA == ZERO) THEN
                   LAMT(I) = HALF*(I1(I) - ONE)
                 ENDIF
              ENDDO
!!              
              DO I=1,NEL
                INVJDETF= ONE/MAX(EM20, JDETF(I))
                LAMT(I) = SQRT(MAX(ZERO,LAMT(I)))
                EPST = LAMT(I) - ONE
                T = SCALE*FINTER(IFUNC(1),EPST,NPF,TF,DF)
                L3 = LAMT(I)**(-3)
                DW_DI = ZERO
                IF (I1B(I) == THREE .OR. LAMT(I) == ONE) THEN
                  DW_DI = ZERO
                ELSEIF((LAMT(I)  - L3) /= ZERO) THEN
                  DW_DI = T / (LAMT(I)  - L3)
                ENDIF
                GS(I)= MAX(GINI,DF*SCALE)
                K(I) = TWO*GS(I)*(ONE+NU) / MAX(EM20,THREE*(ONE-TWO*NU))
                P = RBULK*(JDETF(I) - ONE) 
                SIGNXX(I) = INVJDETF*DW_DI * BD(I,1) + P
                SIGNYY(I) = INVJDETF*DW_DI * BD(I,2) + P
                SIGNZZ(I) = INVJDETF*DW_DI * BD(I,3) + P
                SIGNXY(I) = INVJDETF*DW_DI * BD(I,4)
                SIGNYZ(I) = INVJDETF*DW_DI * BD(I,5)
                SIGNZX(I) = INVJDETF*DW_DI * BD(I,6)
                UVAR(I,1) = LAMT(I) - ONE
                UVAR(I,2) = -JDETF(I)*SIGNZX(I) / LAMT(I) ! only for testing
              ENDDO
      END SELECT    
C 
      IF (IHET > 1) THEN
       DO I=1,NEL
         ET(I) = MAX(ONE, GS(I)/GINI) 
       ENDDO
      ENDIF
C---------------- 
      DO I=1,NEL
        SOUNDSP(I) = SQRT((FOUR_OVER_3*G + RBULK)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
c-----------   
      RETURN
      END
C  
Chd|====================================================================
Chd|  CARDAN_METHOD                 source/materials/mat/mat111/sigeps111.F
Chd|-- called by -----------
Chd|        SIGEPS111                     source/materials/mat/mat111/sigeps111.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CARDAN_METHOD(ITEST,NEL,I1,ICOMP,LAMT)
C  solve X**3 - I*X + 2 = 0
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: ITEST,NEL
      INTEGER  :: ICOMP(NEL)
      my_real , DIMENSION(NEL)::  I1(NEL),LAMT(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        my_real
     .    X0,X1,X2,DELTA,AA,BB,HUNDRED8,HUIT1,ROOT,FACT
C
        HUNDRED8   = HUNDRED + EIGHT
        HUIT1   = 81
        IF(ITEST == 1) THEN
            DO I=1,NEL
               DELTA = FOUR*I1(I)**3 - HUNDRED8
               IF(DELTA > ZERO) THEN
                  AA = THREE/I1(I)
                  BB = THIRD*I1(I)
                  AA = -AA*SQRT(AA)
                  X0 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA))
                  X1 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA) + TWO_THIRD*PI)
                  X2 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA) + FOUR_OVER_3*PI)
                  LAMT(I) = MAX(ONE,X0,X1,X2)! tension
                  IF(ICOMP(I) == 0) CYCLE 
                  ! in compression
                  IF(X0*X1 < ZERO) THEN
                     IF(X0 > ZERO) LAMT(I) = X0
                     IF(X1 > ZERO) LAMT(I) = X1
                     IF(LAMT(I) > X2 .AND. X2 > ZERO ) LAMT(I) = X2 
                  ELSE
                    IF(X0 < ZERO) THEN
                       IF(X2 > ZERO) LAMT(I) = X2
                    ELSE
                     LAMT(I) = MIN(X0, X1)
                     IF(LAMT(I) > X2 .AND. X2 > ZERO ) LAMT(I) = X2
                    ENDIF
                  ENDIF
               ELSE
                 LAMT(I) = MAX(ONE,THREE/I1(I),-SIX/I1(I))
               ENDIF 
            ENDDO 
          ELSEIF(ITEST == 2) THEN
            DO I=1,NEL
               DELTA = FOUR*I1(I)**3 - HUNDRED8
               IF(DELTA > ZERO) THEN
                  AA = THREE/I1(I)
                  BB = THIRD*I1(I)
                  AA = -AA*SQRT(AA)
                  X0 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA))
                  X1 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA) + TWO_THIRD*PI)
                  X2 = TWO*SQRT(BB)*COS(THIRD*ACOS(AA) + FOUR_OVER_3*PI)
                  LAMT(I) = MAX(X0,X1,X2)   ! COMPRESSION
                  IF(ICOMP(I) == 1) CYCLE
                  ! In tension
                  IF(X0*X1 < ZERO) THEN  
                     IF(X0 > ZERO) LAMT(I) = X0
                     IF(X1 > ZERO) LAMT(I) = X1
                     IF(LAMT(I) > X2 .AND. X2 > ZERO ) LAMT(I) = X2 
                  ELSE
                    IF(X0 < ZERO) THEN
                       IF(X2 > ZERO) LAMT(I) = X2
                    ELSE
                     LAMT(I) = MIN(X0, X1)
                     IF(LAMT(I) > X2 .AND. X2 > ZERO ) LAMT(I) = X2
                    ENDIF
                  ENDIF  
               ELSE
                 LAMT(I) = MAX(THREE/I1(I),-SIX/I1(I))
               ENDIF 
c              
            ENDDO           
          
          
          
         ENDIF   
C-----
      RETURN
      END SUBROUTINE CARDAN_METHOD
