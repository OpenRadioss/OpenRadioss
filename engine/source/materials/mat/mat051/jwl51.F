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
Chd|  JWLUN51                       source/materials/mat/mat051/jwl51.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JWLUN51 (TIME,XL,TBURN,UPARAM,DD,MU,MUP1,
     .          VOLUME,DVOL,V1OLD,EINT1,VISCMAX,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,
     .          RHO,RHO10,MAS1,SOUNDSP,
     .          P,Q,RHO1,V1,SSP1, QA,QB,BFRAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .          TIME,XL,TBURN,VISCMAX,DD,MU,
     .          VOLUME,V1OLD,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,
     .          RHO,RHO10,MAS1,SOUNDSP,
     .          UPARAM(*),P,Q,RHO1,V1,SSP1,QA,QB,
     .          BFRAC,DVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug, IBFRAC
      my_real
     .   AA,BB,P0,VDET,BHE,B1,B2,W1,R1,R2,R1M,ER1M,R2M,ER2M,
     .   QAL,QBL,DPDMU,MUP1,C01,C11,
     .   Psol, Pgas, Psol_min, Pgas_min, SSP_unreacted, SSP_reacted
C-----------------------------------------------
      VDET  = UPARAM(42)
      BHE   = UPARAM(44)
      B1    = UPARAM(45)
      C01   = UPARAM(49)
      C11   = UPARAM(50)
      B2    = UPARAM(51)
      R1    = UPARAM(52)
      R2    = UPARAM(53)
      W1    = UPARAM(54)
      IBFRAC= UPARAM(68)
C   
      IF(R1 == ZERO) R1=EP30
      IF(R2 == ZERO) R2=EP30
C   
      DVOL = VOLUME - V1OLD
C
C=======================================================================
C         une seule phase JWL (phase 1)
C La pression doit etre relative a Pext pour
C etre coherent avec els autres phases.
C=======================================================================

      !--------------------------------!
      ! Calculation of BFRAC in [0,1]  !
      !--------------------------------!
      RHO = MAS1 / VOLUME
      IF(BFRAC < ONE) THEN
       BFRAC = ZERO
       IF(IBFRAC/=1 .AND. TIME > -TBURN) BFRAC = VDET*(TIME+TBURN)*TWO_THIRD/XL
       IF(IBFRAC/=2) BFRAC  = MAX( BFRAC , BHE * (ONE - RHO10/RHO) )
       IF(BFRAC < EM03) THEN
         BFRAC = ZERO
       ELSEIF(BFRAC > ONE) THEN
         BFRAC = ONE
       ENDIF
      ENDIF


      !--------------------------------!
      ! SSP & ARTIFICIAL VISCO         !
      !--------------------------------!
      MUP1          = RHO/RHO10
      MU            = MUP1 - ONE
      R1M           = R1/MUP1
      R2M           = R2/MUP1
      ER1M          = EXP(-R1M)
      ER2M          = EXP(-R2M)
      AA            = W1/VOLUME
      P0            = B1*(ONE-W1/R1M)*ER1M + B2*(ONE-W1/R2M)*ER2M
      P             = P0 + AA*EINT1   !total jwl pressure for ssp
      DPDMU         = B1*ER1M*( (-W1*MUP1/R1) + R1M - W1) + B2*ER2M*( (-W1*MUP1/R2) + R2M - W1) + W1*EINT1/VOLUME +P*W1
      DPDMU         = ABS(DPDMU) / MUP1  ! if DPDMU <0 => numerical error during energy integration (increase iteration number or reduce submaterial volume change ratio)
      SSP_reacted   = SQRT(DPDMU/RHO10)
      SSP_unreacted = SQRT(C11/RHO10)
      SOUNDSP       = MAX(BFRAC*SSP_reacted,(ONE-BFRAC)*SSP_unreacted)
      QAL           = QA*XL
      QAL           = QAL*QAL
      QBL           = QB*XL
      VISCMAX       = RHO*(QAL*MAX(ZERO,DD) + QBL*SOUNDSP)
      Q1            = VISCMAX*MAX(ZERO,DD)
      BB            = HALF*(VOLUME-V1OLD)
!      EINT1         = EINT1 - (P1OLD+PEXT+PEXT)*BB
      AA            = AA
      P1            = ( P0-PEXT + AA*EINT1 )!  /  (ONE+AA*BB)


      !--------------------------------!
      ! Linear and jwl eos             !
      !--------------------------------!
      Psol     = C01+C11*MU           !linear eos relative pressure
      Psol_min = PM1                  !p<0 allowed for solid phase. Default : -EP30
      Psol     = MAX(Psol,Psol_min)

      Pgas     = P1                   !jwl eos relative to Pext
      Pgas_min = -PEXT                !p>0 for detonation products
      Pgas     = MAX(Pgas,Pgas_min)

      P1       = BFRAC*Pgas + (ONE-BFRAC)*Psol

!      EINT1    = EINT1 - P1*BB
!      EINT1    = MAX (EINT1, ZERO)

      !--------------------------------!
      ! Update SSP with current state  !
      !--------------------------------!
      DPDMU         = B1*ER1M*( (-W1*MUP1/R1) + R1M - W1) + B2*ER2M*( (-W1*MUP1/R2) + R2M - W1) + W1*EINT1/VOLUME +(P1+PEXT)*W1
      DPDMU         = ABS(DPDMU) / MUP1  ! if DPDMU <0 => numerical error during energy integration (increase iteration number or reduce submaterial volume change ratio)
      SSP_reacted   = SQRT(DPDMU/RHO10)
      SSP_unreacted = SQRT(C11/RHO10)
      SOUNDSP       = MAX(BFRAC*SSP_reacted,(ONE-BFRAC)*SSP_unreacted)


      !--------------------------------!
      ! Returning values               !
      !--------------------------------!
      P        = P1     !return pressure relative to Pext
      Q        = Q1
      RHO1     = RHO
      V1       = VOLUME
      SSP1     = SOUNDSP
C     
      RETURN
      END


Chd|====================================================================
Chd|  JWL51                         source/materials/mat/mat051/jwl51.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JWL51 (TIME,XL,TBURN,UPARAM,
     .          VOLUME,V1,V1OLD,MU1,MUP1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,BFRAC,V10, FLAG, GRUN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .          TIME,XL,TBURN,
     .          VOLUME,V1,V1OLD,MU1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,
     .          UPARAM(*),BFRAC, V10, GRUN
      INTEGER :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug, IBFRAC
      my_real
     .   AA,BB,P0,VDET,BHE,B1,B2,W1,R1,R2,R1M,ER1M,R2M,ER2M,
     .   MUP1,DVDP1I,C11,C01,SSP_PRODUCTS,SSP_UNREACTED,
     .   Psol, Pgas, Psol_min, Pgas_min,DPDMU,DPDV1_REACTED,DPDV1_UNREACTED
C-----------------------------------------------
      VDET   = UPARAM(42)
      BHE    = UPARAM(44)
      B1     = UPARAM(45)
      C01    = UPARAM(49)
      C11    = UPARAM(50)
      B2     = UPARAM(51)
      R1     = UPARAM(52)
      R2     = UPARAM(53)
      W1     = UPARAM(54)
      IBFRAC = UPARAM(68)
C------------------------
      DVDP1I = DVDP1
      RHO1   = MAS1/V1
      MUP1   = RHO1/RHO10
      MU1    = MUP1 - ONE

      R1M    = R1/MUP1
      R2M    = R2/MUP1
      ER1M   = EXP(-R1M)
      ER2M   = EXP(-R2M)

      !--------------------------------!
      ! Calculation of BFRAC in [0,1]  !
      !--------------------------------!
c$$$      IF (ITER == 1) THEN
c$$$      IF(BFRAC < ONE) THEN
c$$$        BFRAC = ZERO
c$$$        IF(IBFRAC/=1 .AND. TIME > -TBURN)THEN
c$$$          XL = V1**THIRD
c$$$          BFRAC = VDET*(TIME+TBURN)*TWO_THIRD/XL
c$$$        ENDIF
c$$$        IF(IBFRAC/=2) BFRAC  = MAX( BFRAC , BHE * (ONE - RHO10/RHO1) )
c$$$        IF(BFRAC < EM03) THEN
c$$$          BFRAC = ZERO
c$$$        ELSEIF(BFRAC > ONE) THEN
c$$$          BFRAC = ONE
c$$$        ENDIF
c$$$      ENDIF
c$$$      ENDIF

      AA       = W1*MUP1/V10 !W1/V1    same digits this way
      AA       = AA
      BB       = HALF*(V1-V1OLD)
      IF (FLAG == 1) EINT1    = EINT1 - (P1OLD+PEXT+PEXT)*BB
      P0       = B1*(ONE-W1/R1M)*ER1M + B2*(ONE-W1/R2M)*ER2M
      IF (FLAG == 1) THEN
         P1       = ( P0-PEXT + AA*EINT1 )  /  (ONE+AA*BB)
      ELSE
         P1 = P0 - PEXT + AA * EINT1
      ENDIF
      GRUN = BFRAC * W1

      !--------------------------------!
      ! Linear and jwl eos             !
      !--------------------------------!
      Psol     = C01+C11*MU1          !linear eos relative pressure
      Psol_min = PM1                  !p<0 allowed for solid phase. Default : -EP30
      Psol     = MAX(Psol,Psol_min)

      Pgas     = P1                   !jwl eos relative to Pext
      Pgas_min = -PEXT                !p>0 for detonation products
      Pgas     = MAX(Pgas,Pgas_min)

      P1       = BFRAC*Pgas + (ONE-BFRAC)*Psol
      IF (FLAG == 1) EINT1    = EINT1 - P1*BB
      IF (FLAG == 1) EINT1    = MAX(EINT1, ZERO)

      !--------------------------------!
      ! Sound Speed                    !
      !--------------------------------!
      DPDMU         = B1*ER1M*( (-W1*MUP1/R1) + R1M - W1) + B2*ER2M*( (-W1*MUP1/R2) + R2M - W1)
     .              + W1*EINT1/V1 + (Pgas+PEXT)*W1
      DPDMU         = ABS(DPDMU) / MUP1
      SSP_PRODUCTS  = SQRT(DPDMU/RHO10)
      SSP_UNREACTED = SQRT(C11/RHO10)
      SSP1          = (ONE-BFRAC)*SSP_UNREACTED + BFRAC*SSP_PRODUCTS

      !--------------------------------!
      ! DPDV                           !
      !--------------------------------!
!      IF(ABS(V1 - V1OLD)/VOLUME>EM06.AND.ABS(P1-P1I)>EM20)THEN
 !       DVDP1 = (V1 - V1OLD) / (P1-P1I)
 !     ENDIF

      DPDV1_REACTED    = -DPDMU*MUP1/V1
      DPDV1_UNREACTED  = -C11*MUP1/V1
      DPDV1            = BFRAC*DPDV1_REACTED + (ONE-BFRAC)*DPDV1_UNREACTED

      IF(ABS(DPDV1)<EM20)THEN
        DVDP1 = ZERO
      ELSE
        DVDP1 = ONE/DPDV1
      ENDIF


c     dvdp < 0
c      IF(DVDP1<TWO*DVDP1I)THEN
c        DVDP1 = TWO*DVDP1I
c      ELSEIF(DVDP1>ZERO)THEN
c        DVDP1 = TWO*DVDP1I
c      ELSEIF(DVDP1>HALF*DVDP1I)THEN
c        DVDP1 = HALF*DVDP1I
c      ENDIF

!      IF(DVDP1/=ZERO)THEN
!         DPDV1 = ONE / DVDP1
!      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  DPDV_JWL51                    source/materials/mat/mat051/jwl51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DPDV_JWL51 (TIME,XL,TBURN,DD,UPARAM,
     .          V1,V1OLD,MU1,EINT1,VISCMAX,
     .          Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, QA, QB,BFRAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .          TIME,XL,TBURN,DD,
     .          V1,V1OLD,MU1,EINT1,VISCMAX,
     .          Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,DPDV1_UNREACTED,DPDV1_REACTED,
     .          UPARAM(*),QA,QB, BFRAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug
      my_real
     .   AA,BB,P0,VDET,BHE,B1,B2,W1,R1,R2,R1M,ER1M,R2M,ER2M,
     .   DPDMU,QAL,QBL,P,MUP1,C11,C01,
     .   Psol,Pgas,Psol_min,Pgas_min,SSP_PRODUCTS,SSP_UNREACTED
C-----------------------------------------------
      VDET = UPARAM(42)
      BHE  = UPARAM(44)
      B1   = UPARAM(45)
      C01  = UPARAM(49)
      C11  = UPARAM(50)
      B2   = UPARAM(51)
      R1   = UPARAM(52)
      R2   = UPARAM(53)
      W1   = UPARAM(54)
C=======================================================================
C          phase4 : JWL 
C La pression doit etre relative a Pext pour
C etre coherent avec les autres phases.
C=======================================================================

      !--------------------------------!
      ! Calculation of BFRAC in [0,1]  !
      !--------------------------------!
      RHO1 = MAS1/V1
      MUP1 = RHO1/RHO10
      MU1  = MUP1 - ONE
      XL   = V1**THIRD
      IF(BFRAC < ONE) THEN
       BFRAC = ZERO
       IF(TIME > -TBURN) BFRAC = VDET*(TIME+TBURN)*TWO_THIRD/XL
       BFRAC  = MAX( BFRAC , BHE * (ONE - RHO10/RHO1) )
       IF(BFRAC < EM03) THEN
         BFRAC = ZERO
       ELSEIF(BFRAC >= ONE) THEN
         BFRAC = ONE
       ENDIF
      ENDIF

      R1M      = R1/MUP1
      R2M      = R2/MUP1
      ER1M     = EXP(-R1M)
      ER2M     = EXP(-R2M)
      AA       = W1/V1
      P0       = B1*(ONE-W1/R1M)*ER1M + B2*(ONE-W1/R2M)*ER2M

      !--------------------------------!
      ! SSP & ARTIFICIAL VISCO         !
      !--------------------------------!
      P        = P0 + AA*EINT1

      !--------------------------------!
      ! Linear and jwl eos             !
      !--------------------------------!
      Psol     = C01+C11*MU1           !linear eos relative pressure
      Psol_min = PM1                   !p<0 allowed for solid phase. Default : -EP30
      Psol     = MAX(Psol,Psol_min)

      Pgas     = P-PEXT                !jwl eos total pressure
      Pgas_min = -PEXT                 !p>0 for detonation products
      Pgas     = MAX(Pgas,Pgas_min)

      P1I      = BFRAC*Pgas + (ONE-BFRAC)*Psol


      !--------------------------------!
      ! Sound Speed                    !
      !--------------------------------!
      DPDMU         = B1*ER1M*( (-W1*MUP1/R1) + R1M - W1) + B2*ER2M*( (-W1*MUP1/R2) + R2M - W1) + W1*EINT1/V1 + (Pgas+PEXT)*W1
      DPDMU         = ABS(DPDMU) / MUP1
      SSP_PRODUCTS  = SQRT(DPDMU/RHO10)
      SSP_UNREACTED = SQRT(C11/RHO10)
      SSP1          = (ONE-BFRAC)*SSP_UNREACTED + BFRAC*SSP_PRODUCTS



      !--------------------------------!
      ! DPDV                           !
      !--------------------------------!
      DPDV1_REACTED    = -DPDMU*MUP1/V1
      DPDV1_UNREACTED  = -C11*MUP1/V1
      DPDV1            = BFRAC*DPDV1_REACTED + (ONE-BFRAC)*DPDV1_UNREACTED

!      IF(DPDV1 > ZERO)THEN
!        DPDV1 = -BFRAC*(B1*(R1M - W1 + W1/R1M)*ER1M+B2*(R2M - W1 + W1/R2M)*ER2M+W1*(EINT1/V1)) / V1
!      ENDIF

      IF(ABS(DPDV1)<EM20)THEN
        DVDP1 = ZERO
      ELSE
        DVDP1 = ONE/DPDV1
      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  JWL51_EINT                    source/materials/mat/mat051/jwl51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JWL51_EINT (TIME,XL,TBURN,UPARAM,
     .          VOLUME,V1,V1OLD,MU1,MUP1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,BFRAC,V10, FLAG, GRUN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .          TIME,XL,TBURN,
     .          VOLUME,V1,V1OLD,MU1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,
     .          UPARAM(*),BFRAC, V10, GRUN
      INTEGER :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug, IBFRAC
      my_real
     .   AA,BB,P0,VDET,BHE,B1,B2,W1,R1,R2,R1M,ER1M,R2M,ER2M,
     .   MUP1,DVDP1I,C11,C01,SSP_PRODUCTS,SSP_UNREACTED,
     .   Psol, Pgas, Psol_min, Pgas_min,DPDMU,DPDV1_REACTED,DPDV1_UNREACTED
C-----------------------------------------------
      VDET   = UPARAM(42)
      BHE    = UPARAM(44)
      B1     = UPARAM(45)
      C01    = UPARAM(49)
      C11    = UPARAM(50)
      B2     = UPARAM(51)
      R1     = UPARAM(52)
      R2     = UPARAM(53)
      W1     = UPARAM(54)
      IBFRAC = UPARAM(68)
C------------------------
      DVDP1I = DVDP1
      RHO1   = MAS1/V1
      MUP1   = RHO1/RHO10
      MU1    = MUP1 - ONE

      R1M    = R1/MUP1
      R2M    = R2/MUP1
      ER1M   = EXP(-R1M)
      ER2M   = EXP(-R2M)

      !--------------------------------!
      ! Calculation of BFRAC in [0,1]  !
      !--------------------------------!
c$$$      IF(BFRAC < ONE) THEN
c$$$        BFRAC = ZERO
c$$$        IF(IBFRAC/=1 .AND. TIME > -TBURN)THEN
c$$$          XL = V1**THIRD
c$$$          BFRAC = VDET*(TIME+TBURN)*TWO_THIRD/XL
c$$$        ENDIF
c$$$        IF(IBFRAC/=2) BFRAC  = MAX( BFRAC , BHE * (ONE - RHO10/RHO1) )
c$$$        IF(BFRAC < EM03) THEN
c$$$          BFRAC = ZERO
c$$$        ELSEIF(BFRAC > ONE) THEN
c$$$          BFRAC = ONE
c$$$        ENDIF
c$$$      ENDIF
      !!! BFRAC is an input

      IF (BFRAC == ZERO) THEN
         GRUN = ZERO
         RETURN
      ELSE
         AA       = W1*MUP1/V10 !W1/V1    same digits this way
         AA       = AA
         BB       = HALF*(V1-V1OLD)
         P0       = B1*(ONE-W1/R1M)*ER1M + B2*(ONE-W1/R2M)*ER2M
         Psol     = C01+C11*MU1 !linear eos relative pressure

         EINT1 = (P1 - (ONE - BFRAC) * Psol) / BFRAC + PEXT - P0
         GRUN = BFRAC * W1
         PGAS = (P1 - (ONE - BFRAC) * Psol) / BFRAC

!--------------------------------!
! Sound Speed                    !
!--------------------------------!
         DPDMU         = B1*ER1M*( (-W1*MUP1/R1) + R1M - W1) + B2*ER2M*( (-W1*MUP1/R2) + R2M - W1)
     .        + W1*EINT1/V1 + (Pgas+PEXT)*W1
         DPDMU         = ABS(DPDMU) / MUP1
         SSP_PRODUCTS  = SQRT(DPDMU/RHO10)
         SSP_UNREACTED = SQRT(C11/RHO10)
         SSP1          = (ONE-BFRAC)*SSP_UNREACTED + BFRAC*SSP_PRODUCTS
      ENDIF



      RETURN
      END
