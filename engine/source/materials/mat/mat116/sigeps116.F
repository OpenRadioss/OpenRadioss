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
Chd|  SIGEPS116                     source/materials/mat/mat116/sigeps116.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS116(
     1     NEL     ,NUPARAM ,NUVAR   ,JSMS    ,TIME    ,TIMESTEP,
     2     UPARAM  ,UVAR    ,AREA    ,EPSD    ,OFF     ,OFFL    ,
     3     EPSZZ   ,EPSYZ   ,EPSZX   ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,
     4     SIGNZZ  ,SIGNYZ  ,SIGNZX  ,STIFM   ,DMELS   ,DMG     ,
     5     PLA_N   ,PLA_T   ,IPG     ,NFAIL   ,NGL     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "sms_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL,NUPARAM,NUVAR,JSMS,IPG
      INTEGER ,INTENT(OUT) :: NFAIL
      INTEGER ,DIMENSION(NEL) ,INTENT(IN)  :: NGL
      my_real ,INTENT(IN)  :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) :: OFF,OFFL,PLA_N,PLA_T,AREA,DMELS,
     .   EPSZZ,EPSYZ,EPSZX,DEPSZZ,DEPSYZ,DEPSZX,SIGNZZ,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL)  ,INTENT(OUT)   :: DMG
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: EPSD,STIFM
      my_real ,DIMENSION(NUPARAM)   ,INTENT(IN)    :: UPARAM
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II,IEL,IORDER1,IORDER2,IFAIL1,IFAIL2,ICRIT,NINDXF,NINDXD
      INTEGER ,DIMENSION(NEL)  :: INDXF,INDXD
      my_real :: RHO0,E,G,GC1_INI,GC2_INI,GC1_INF,GC2_INF,RATG1,RATG2,
     .   FG1,FG2,SIGA1,SIGA2,SIGB1,SIGB2,RATE1,RATE2,DTINV,EPSDOT,
     .   ALPHA,ALPHAI,BETA,DM,DPN,DPT,DPT1,DPT2,DAM,DEPS,STF,ET2,
     .   BB,DTB,FACT1,FACT2,THICK
      my_real, DIMENSION(NEL) :: YLD_N,YLD_T,GC_N,GC_T,EPSM1,EPSM2,EPSMF,
     .   COSG,SING,EPSN,EPST,EPSM,EPSN1,EPSN2,EPST1,EPST2,EPSP,
     .   PLA_T1,PLA_T2
c-----------------------------------------------
c     UVAR(1)  = PLA_N   : plastic separation strain in normal direction
c     UVAR(2)  = PLA_T1  : plastic separation strain in shear direction XZ
c     UVAR(3)  = PLA_T2  : plastic separation strain in shear direction YZ
c     UVAR(4)  = EPSP    : strain rate
c     UVAR(5)  = STRFLAG : strain rate update flag (0 => recalculate, 1 => const)
c     UVAR(6)  = DAM     : damage coefficient
c     check only / output variables
c     UVAR(7)  = EPSN
c     UVAR(8)  = EPST
c     UVAR(9)  = EPSM
c     UVAR(10) = EPSM1   : mixed mode yield initiation strain
c     UVAR(11) = EPSM2   : mixed mode damage initiation strain
c     UVAR(12) = EPSMF   : mixed mode damage initiation strain
C=======================================================================
C     INPUT PARAMETERS INITIALIZATION
C-----------------------------------------------
      E       = UPARAM(1)
      G       = UPARAM(2)
      NFAIL   = UPARAM(4)
      GC1_INI = UPARAM(5)
      GC1_INF = UPARAM(6)
      RATG1   = UPARAM(7)
      FG1     = UPARAM(8)
      GC2_INI = UPARAM(9)
      GC2_INF = UPARAM(10)
      RATG2   = UPARAM(11)
      FG2     = UPARAM(12)
      SIGA1   = UPARAM(13)
      SIGB1   = UPARAM(14)
      RATE1   = UPARAM(15)
      IORDER1 = UPARAM(16)
      IFAIL1  = UPARAM(17)
      SIGA2   = UPARAM(18)
      SIGB2   = UPARAM(19)
      RATE2   = UPARAM(20)
      IORDER2 = UPARAM(21)
      IFAIL2  = UPARAM(22)
      ICRIT   = UPARAM(23)
      THICK   = UPARAM(24)
      ALPHA   = UPARAM(25)   ! filtering coefficient of strain rate (exponential avg)
      ALPHAI  = ONE - ALPHA
      DTINV   = ONE / (MAX(TIMESTEP, EM20))
      STF     = E + G
      NINDXF  = 0
      NINDXD  = 0
c-------------------------
      PLA_N (:NEL) = UVAR(:NEL,1)
      PLA_T1(:NEL) = UVAR(:NEL,2)
      PLA_T2(:NEL) = UVAR(:NEL,3)
c-------------------------
      DO IEL=1,NEL
        ET2 = EPSYZ(IEL)**2 + EPSZX(IEL)**2
        EPSN(IEL) = MAX(EPSZZ(IEL), ZERO)
        EPST(IEL) = SQRT(ET2)
        EPSM(IEL) = SQRT(ET2 + EPSN(IEL)**2)
      ENDDO
c-------------------------
c     Strain rate
c-------------------------
      DO IEL=1,NEL
        IF (UVAR(IEL,5) == ZERO) THEN  ! strain rate calculation only when epsm < EPSM1
          EPSDOT = SQRT(DEPSYZ(IEL)**2 + DEPSZX(IEL)**2 + DEPSZZ(IEL)**2)
          EPSDOT = EPSDOT * DTINV / THICK
          EPSP(IEL) = ALPHA *EPSDOT + ALPHAI * UVAR(IEL,4)
          EPSD(IEL) = EPSP(IEL)
          UVAR(IEL,4) = EPSP(IEL)
        ELSE    ! constant strain rate
          EPSP(IEL) = UVAR(IEL,4)
        END IF
      ENDDO
c-------------------------
c     Strain rate dependent yield values
c-------------------------
      YLD_N(:NEL) = SIGA1
      YLD_T(:NEL) = SIGA2
      IF (SIGB1 > ZERO) THEN    ! normal yld is strain rate dependent
        IF (IORDER1 == 1) THEN  ! linear log dependency
          DO IEL=1,NEL
            IF (EPSP(IEL) > RATE1) THEN
              YLD_N(IEL) = SIGA1 + SIGB1*LOG(EPSP(IEL)/RATE1)
            END IF
          ENDDO
        ELSE IF (IORDER1 == 2) THEN ! quadratic log dependency
          DO IEL=1,NEL
            IF (EPSP(IEL) > RATE1) THEN
              YLD_N(IEL) = SIGA1 + SIGB1*LOG(EPSP(IEL)/RATE1)**2
            END IF
          ENDDO
        END IF
      END IF
c
      IF (SIGB2 > ZERO) THEN    ! tangent yld is strain rate dependent
        IF (IORDER2 == 1) THEN  ! linear log dependency
          DO IEL=1,NEL
            IF (EPSP(IEL) > RATE2) THEN
              YLD_T(IEL) = SIGA2 + SIGB2*LOG(EPSP(IEL)/RATE2)
            END IF
          ENDDO
        ELSE IF (IORDER2 == 2) THEN ! quadratic log dependency
          DO IEL=1,NEL
            IF (EPSP(IEL) > RATE2) THEN
              YLD_T(IEL) = SIGA2 + SIGB2*LOG(EPSP(IEL)/RATE2)**2
            END IF
          ENDDO
        END IF
      END IF
c-------------------------
c     Strain rate dependent fracture energies
c-------------------------
      GC_N(:NEL) = GC1_INI
      GC_T(:NEL) = GC2_INI
      IF (GC1_INF > ZERO) THEN    ! normal GC is strain rate dependent
        DO IEL=1,NEL
!          IF (EPSP(IEL) > RATG1) THEN
          IF (EPSP(IEL) > ZERO) THEN
            GC_N(IEL) = GC1_INI + (GC1_INF-GC1_INI)*EXP(-RATG1/EPSP(IEL))
          END IF
        ENDDO
      END IF
c
      IF (GC2_INF > ZERO) THEN    ! tangent GC is strain rate dependent
        DO IEL=1,NEL
!          IF (EPSP(IEL) > RATG2) THEN
          IF (EPSP(IEL) > ZERO) THEN
            GC_T(IEL) = GC2_INI + (GC2_INF-GC2_INI)*EXP(-RATG2/EPSP(IEL))
          END IF
        ENDDO
      END IF
c-------------------------
      DO IEL=1,NEL
        EPSN1(IEL) = YLD_N(IEL) / E
        EPST1(IEL) = YLD_T(IEL) / G
      END DO
c-------------------------
c     Check max FG values
c-------------------------
      IF (IFAIL1 == 1) THEN
          DO IEL=1,NEL
          EPSN2(IEL) = EPSN1(IEL) + FG1 * GC_N(IEL)/YLD_N(IEL)
        END DO
      ELSE
        DO IEL=1,NEL
          EPSN2(IEL) = (EPSN1(IEL) + TWO*FG1*GC_N(IEL) / YLD_N(IEL)) / (FG1 + ONE)
        END DO
      END IF
c
      IF (IFAIL2 == 1)THEN
          DO IEL=1,NEL
          EPST2(IEL) = EPST1(IEL) + FG2 * GC_T(IEL)/YLD_T(IEL)
          END DO
      ELSE
        DO IEL=1,NEL
          EPST2(IEL) = (EPST1(IEL) + TWO*FG2*GC_T(IEL) / YLD_T(IEL)) / (FG2 + ONE)
        END DO
      END IF
c--------------------------------
c     Update failure initiation strains in tension, shear and mixed mode
c--------------------------------
      IF (ICRIT == 1) THEN
        DO IEL=1,NEL
          IF (EPST(IEL) == ZERO) THEN
            EPSM1(IEL) = EPSN1(IEL)
            EPSM2(IEL) = EPSN2(IEL)
          ELSE IF (EPSN(IEL) == ZERO) THEN
            EPSM1(IEL) = EPST1(IEL)
            EPSM2(IEL) = EPST2(IEL)
          ELSE      ! mixed mode
            BETA  = EPST(IEL) / EPSN(IEL)
            FACT1 = SQRT((ONE+BETA**2) /(EPST1(IEL)**2 + (BETA*EPSN1(IEL))**2))
            FACT2 = SQRT((ONE+BETA**2) /(EPST2(IEL)**2 + (BETA*EPSN2(IEL))**2))
            EPSM1(IEL) = EPSN1(IEL)*EPST1(IEL)*FACT1
            EPSM2(IEL) = EPSN2(IEL)*EPST2(IEL)*FACT2
          END IF
          IF (EPSM(IEL) > EPSM1(IEL)) UVAR(IEL,5) = ONE
        END DO
      ELSE
        DO IEL=1,NEL
          IF (EPST(IEL) == ZERO) THEN
            EPSM1(IEL) = EPSN1(IEL)
            EPSM2(IEL) = EPSN2(IEL)
          ELSE IF (EPSN(IEL) == ZERO) THEN
            EPSM1(IEL) = EPST1(IEL)
            EPSM2(IEL) = EPST2(IEL)
          ELSE      ! mixed mode
            BETA = EPST(IEL) / EPSN(IEL)
            IF (BETA*EPSN1(IEL) > EPST1(IEL)) THEN
              EPSM1(IEL) = EPST1(IEL)*SQRT(ONE + BETA**2) / BETA
            ELSE
              EPSM1(IEL) = EPSN1(IEL)*SQRT(ONE + BETA**2)
            END IF
            IF (BETA*EPSN2(IEL) > EPST2(IEL)) THEN
              EPSM2(IEL) = EPST2(IEL)*SQRT(ONE + BETA**2) / BETA
            ELSE
              EPSM2(IEL) = EPSN2(IEL)*SQRT(ONE + BETA**2)
            END IF
          END IF
          IF (EPSM(IEL) > EPSM1(IEL)) UVAR(IEL,5) = ONE
        END DO
      END IF
c--------------------------------
c     Power law of damage evolution
c--------------------------------
      DO IEL=1,NEL
        IF (EPSN(IEL) == ZERO) THEN
          COSG(IEL) = ZERO
          SING(IEL) = ONE
        ELSE IF (EPST(IEL) == ZERO) THEN
          COSG(IEL) = ONE
          SING(IEL) = ZERO
        ELSE IF (EPSM(IEL) > ZERO) THEN
          COSG(IEL) = EPSZZ(IEL) / EPSM(IEL)
          SING(IEL) = EPST(IEL) / EPSM(IEL)
        END IF
        IF (EPSM(IEL) > ZERO) THEN
          DM = EPSM1(IEL) - EPSM2(IEL)
          BB = EPSM1(IEL)*(E*GC_T(IEL)*COSG(IEL)**2 + G*GC_N(IEL)*SING(IEL)**2)
          EPSMF(IEL) = DM + TWO*GC_T(IEL)*GC_N(IEL) / BB
        ELSE
          EPSMF(IEL) = EP20
        END IF
      END DO
c
      DO IEL=1,NEL
        DM = EPSM(IEL) - EPSM2(IEL)
        IF (OFF(IEL) > ZERO .and. DM > ZERO) THEN
          IF (UVAR(IEL,6) == ZERO) THEN
            NINDXD = NINDXD+1
            INDXD(NINDXD) = IEL
          END IF
          DAM = DM / MAX((EPSMF(IEL) - EPSM2(IEL)), EM20)
          DAM = MAX(DAM, UVAR(IEL,6))
          UVAR(IEL,6) = DAM
          DMG(IEL) = MAX(DMG(IEL), DAM)
          DMG(IEL) = MIN(DMG(IEL), ONE)
          IF (OFF(IEL)*OFFL(IEL) == ONE .and. EPSM(IEL) > EPSMF(IEL)) THEN
            NINDXF = NINDXF+1
            INDXF(NINDXF) = IEL
            OFFL(IEL) = ZERO
          END IF
        END IF
      END DO
c-------------------------
c     Plastic strains
c-------------------------
      DO IEL=1,NEL
        IF (OFF(IEL)*OFFL(IEL) == ONE) THEN
          DPN = EPSN(IEL) - EPSM1(IEL)*COSG(IEL)
          IF (DPN > ZERO) THEN
            PLA_N(IEL)  = MAX(DPN, PLA_N(IEL))
            UVAR(IEL,1) = PLA_N(IEL)
          END IF
          DPT1 = EPSYZ(IEL) - PLA_T1(IEL)
          DPT2 = EPSZX(IEL) - PLA_T2(IEL)
          DPT  = SQRT(DPT1**2 + DPT2**2)
          IF (DPT > EPSM1(IEL)*SING(IEL)) THEN
            PLA_T1(IEL) = PLA_T1(IEL) + DEPSYZ(IEL)
            PLA_T2(IEL) = PLA_T2(IEL) + DEPSZX(IEL)
            UVAR(IEL,2) = PLA_T1(IEL)
            UVAR(IEL,3) = PLA_T2(IEL)
            PLA_T(IEL)  = SQRT(PLA_T1(IEL)**2 + PLA_T2(IEL)**2)
          END IF
        END IF
      END DO
c-------------------------
c     Stress update
c-------------------------
      DO IEL=1,NEL
        SIGNZZ(IEL) = E * (EPSZZ(IEL) - PLA_N (IEL))*(ONE-DMG(IEL))*OFF(IEL)
        SIGNYZ(IEL) = G * (EPSYZ(IEL) - PLA_T1(IEL))*(ONE-DMG(IEL))*OFF(IEL)
        SIGNZX(IEL) = G * (EPSZX(IEL) - PLA_T2(IEL))*(ONE-DMG(IEL))*OFF(IEL)
c
        UVAR(IEL,7)  = EPSN(IEL)
        UVAR(IEL,8)  = EPST(IEL)
        UVAR(IEL,9)  = EPSM(IEL)
        UVAR(IEL,10) = EPSM1(IEL)
        UVAR(IEL,11) = EPSM1(IEL)
        UVAR(IEL,12) = EPSMF(IEL)
      ENDDO
c-----------------------------------------------------
c     omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
      IF (IDTMINS==2 .AND. JSMS/=0) THEN
        DTB = (DTMINS/DTFACS)**2
        DO IEL=1,NEL
          DMELS(IEL)=MAX(DMELS(IEL),HALF*DTB*STF*AREA(IEL)*OFF(IEL))
        ENDDO
      END IF
      STIFM(1:NEL) = STIFM(1:NEL) + STF*AREA(1:NEL)*OFF(1:NEL)
c-----------------------------------------------------
      IF (NINDXD > 0) THEN
        DO II=1,NINDXD
          IEL = INDXD(II)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(IEL),IPG,EPSM(IEL)
#include "lockoff.inc"
        END DO
      END IF
c
      IF (NINDXF > 0) THEN
        DO II=1,NINDXF
          IEL = INDXF(II)
#include "lockon.inc"
          WRITE(IOUT ,2000) NGL(IEL),IPG,EPSM(IEL)
          WRITE(ISTDO,2100) NGL(IEL),IPG,EPSM(IEL),TIME
#include "lockoff.inc"
        END DO
      END IF
c-----------------------------------------------------
 1000 FORMAT(5X,'START DAMAGE COHESIVE ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9)
 2000 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9)
 2100 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
c-----------
      RETURN
      END
