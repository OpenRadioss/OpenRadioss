Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    sigeps69      ../engine/source/materials/mat/mat069/sigeps69.F
      !||--- called by ------------------------------------------------------
      !||    mulaw         ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    finter        ../engine/source/tools/curve/finter.F
      !||    valpvec_v     ../engine/source/materials/mat/mat033/sigeps33.F
      !||    valpvecdp_v   ../engine/source/materials/mat/mat033/sigeps33.F
      !||====================================================================
      SUBROUTINE SIGEPS69(
     1           NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC   , NPF    ,
     2           TF     , TIME   , TIMESTEP, UPARAM , RHO0    , RHO    ,
     3           VOLUME , EINT   , 
     4           EPSPXX , EPSPYY , EPSPZZ  , EPSPXY , EPSPYZ  , EPSPZX ,
     5           DEPSXX , DEPSYY , DEPSZZ  , DEPSXY , DEPSYZ  , DEPSZX ,
     6           EPSXX  , EPSYY  , EPSZZ   , EPSXY  , EPSYZ   , EPSZX  ,
     7           SIGOXX , SIGOYY , SIGOZZ  , SIGOXY , SIGOYZ  , SIGOZX ,
     8           SIGNXX , SIGNYY , SIGNZZ  , SIGNXY , SIGNYZ  , SIGNZX ,
     9           SIGVXX , SIGVYY , SIGVZZ  , SIGVXY , SIGVYZ  , SIGVZX ,
     A           SOUNDSP, VISCMAX, UVAR    , OFF    , WXXDT   , WYYDT  ,
     B           WZZDT  , ISMSTR , MFXX    , MFXY   , MFXZ    , MFYX   , 
     C           MFYY   , MFYZ   , MFZX    , MFZY   , MFZZ    , ET     ,
     D           IHET   , NUVARV , UVARV   , OFFG   , EPSTH3  , IEXPAN )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "scr17_c.inc"
#include "impl1_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,ISMSTR,IHET,NUVARV,IEXPAN
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      MFXX(NEL)  ,   MFXY(NEL),   MFXZ(NEL),
     .      MFYX(NEL)  ,   MFYY(NEL),   MFYZ(NEL),
     .      MFZX(NEL)  ,   MFZY(NEL),   MFZZ(NEL),    
     .      WZZDT(NEL),WYYDT(NEL),WXXDT(NEL),OFFG(NEL),
     .      EPSTH3(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL), ET(NEL)    
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL), UVARV(*)
!!     .      UVAR(NEL,NUVAR), OFF(NEL), UVARV(NEl*NUVARV)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K,KFP,NORDRE,II,NPRONY
      my_real
     .   TENSCUT,GMAX,FSCALE,RVT,SUMDWDL,EFAC,DWDRV,RBULK,DPDMU,AMAX,ETI(MVSIZ),ETH
      my_real
     .   EVV(MVSIZ,3),EV(MVSIZ,3),EVM1(MVSIZ),EVM2(MVSIZ),EVM3(MVSIZ),P(MVSIZ),
     .   MU(5),AL(5),DWDL(3),T(MVSIZ,3),RV(MVSIZ),AV(MVSIZ,6),DIRPRV(MVSIZ,3,3)
       my_real  
     .   H0(100),H(100),C0(MVSIZ,6),C1(MVSIZ,6),AA,SV(MVSIZ,6),
     .   CC,FAC,INVDT,FFT(3),P_FAC(NEL),NU_1,AMIN,LAM_AL(3),GTMAX(NEL)
      my_real, DIMENSION(NEL,3) :: CII
      DOUBLE PRECISION AMAX1
C----------------------------------------------------------------
C SET INITIAL MATERIAL CONSTANTS
      MU(1)  = UPARAM(1)
      MU(2)  = UPARAM(2)
      MU(3)  = UPARAM(3)
      MU(4)  = UPARAM(4)
      MU(5)  = UPARAM(5)
      AL(1)  = UPARAM(6)
      AL(2)  = UPARAM(7)
      AL(3)  = UPARAM(8)
      AL(4)  = UPARAM(9)
      AL(5)  = UPARAM(10)
      RBULK  = UPARAM(11)
      TENSCUT= UPARAM(12)
      FSCALE = UPARAM(15) 
      NORDRE = NINT(UPARAM(18))      
C      
      KFP = IFUNC(1)
C
      GMAX = ZERO
      DO I = 1,NORDRE
        GMAX  = GMAX  + MU(I)*AL(I)
      ENDDO
C
      DO I=1,NEL
        AV(I,1)=EPSXX(I)
        AV(I,2)=EPSYY(I)
        AV(I,3)=EPSZZ(I)
        AV(I,4)=EPSXY(I) * HALF
        AV(I,5)=EPSYZ(I) * HALF
        AV(I,6)=EPSZX(I) * HALF
      ENDDO
C         Eigenvalues needed to be calculated in double precision
C         for a simple precision executing
      IF (IRESP == 1) THEN
        CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
      ENDIF
C     Strain definition
      IF (ISMSTR == 1 .OR. ISMSTR == 3 ) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1)=EVV(I,1)+ ONE
          EV(I,2)=EVV(I,2)+ ONE
          EV(I,3)=EVV(I,3)+ ONE  
        ENDDO
      ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
        DO I =1,NEL
        IF(OFFG(I)<=ONE) THEN
          EV(I,1)=SQRT(EVV(I,1)+ ONE)
          EV(I,2)=SQRT(EVV(I,2)+ ONE)
          EV(I,3)=SQRT(EVV(I,3)+ ONE)
          ELSE
         EV(I,1)=EVV(I,1)+ ONE
         EV(I,2)=EVV(I,2)+ ONE
         EV(I,3)=EVV(I,3)+ ONE
        END IF
        ENDDO 
      ELSE  ! true strain
        DO I=1,NEL
          EV(I,1)=EXP(EVV(I,1))
          EV(I,2)=EXP(EVV(I,2))
          EV(I,3)=EXP(EVV(I,3))
        ENDDO
      ENDIF
C----------------
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
C        ET(I) = ZERO
        DO J = 1,3
          ETI(1:NEL)  = ZERO
          DO K=1,NORDRE
            DO I  = 1,NEL
               AMAX = EV(I,J)
             IF(AMAX>ZERO) THEN
              IF((Al(K)-ONE)==ZERO) THEN
                 ETI(I)  = ETI(I) + MU(K)*Al(K)
              ELSE
                 ETI(I)  = ETI(I) + MU(K)*Al(K)*EXP((Al(K)-ONE)*LOG(AMAX))
              ENDIF
             ENDIF   
            ENDDO
          ENDDO
          ET(1:NEL) = MAX(ETI(1:NEL),ET(1:NEL))
        ENDDO
        DO I=1,NEL
          ET(I) = MAX(ONE,ET(I)/GMAX)
        ENDDO
      ENDIF
c--- avoid buckling
      P_FAC(1:NEL) = ONE
      NU_1 = FOURTY*(HALF-(3*RBULK-GMAX)/(6*RBULK+GMAX))
      DO I=1,NEL
        AMIN = MIN(EV(I,1),EV(I,2),EV(I,3))
        IF (AMIN<ZEP2) P_FAC(I) = MAX(ONE,NU_1/MAX(EM20,AMIN))
      ENDDO
C----------------
      DO I=1,NEL
        RV(I) = EV(I,1)*EV(I,2)*EV(I,3)
      ENDDO
C----THERM STRESS COMPUTATION-----
      IF(IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) THEN
         DO I=1,NEL
          RV(I)= RV(I)-EPSTH3(I) 
         ENDDO
      ENDIF
      DO I=1,NEL
!        RVT   = RV(I)**(-THIRD)
        ! if rv > 0 --> rvt = exp(-third * ln(rv)) 
        ! else rvt = 0
        IF(RV(I)>ZERO) THEN
         RVT   = EXP((-THIRD)*LOG(RV(I)))
        ELSE
         RVT = ZERO
        ENDIF
        EVM1(I) = EV(I,1)*RVT
        EVM2(I) = EV(I,2)*RVT
        EVM3(I) = EV(I,3)*RVT
        IF (KFP > 0) THEN
*         READ BULK MODULE FROM FUNCTION
          P(I) = RBULK*FSCALE*FINTER(KFP,RV(I),NPF,TF,DPDMU)
        ELSE
*         BULK MODULE IS CONSTANT
          P(I)=P_FAC(I)*RBULK
        ENDIF
      ENDDO
c
      ! For HEPH Hourglass treatment and soundspeed computation
      GTMAX(1:NEL) = GMAX
      ETI(1:NEL) = ZERO
      CII(1:NEL,1:3) = ZERO
      DO II = 1,NORDRE
        IF (MU(II)*AL(II) /= ZERO) THEN
          DO I=1,NEL
            LAM_AL(1) = EXP(AL(II)*LOG(EVM1(I)))
            LAM_AL(2) = EXP(AL(II)*LOG(EVM2(I)))
            LAM_AL(3) = EXP(AL(II)*LOG(EVM3(I)))
            AMAX = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
            CII(I,1:3) = CII(I,1:3) + MU(II)*AL(II)*(LAM_AL(1:3)+AMAX)
          ENDDO
        ENDIF
      ENDDO
      ! Note: 2GT=3/2 Cii (factor 3 already inside CII)      
      AMAX1 = 0.81*HALF/GMAX
      DO I = 1,NEL
        AMAX = AMAX1*MAX(CII(I,1),CII(I,2),CII(I,3))
        ETI(I) = MAX(ONE,AMAX) 
        GTMAX(I) = GMAX*ETI(I)
        ET(I) = ETI(I)
      ENDDO
      DO I=1,NEL
        DWDL(1) = ZERO                             
        DWDL(2) = ZERO                                
        DWDL(3) = ZERO                                
        DO K = 1,NORDRE
          IF(EVM1(I)>ZERO) THEN
           IF(AL(K)/=ZERO) THEN
            DWDL(1) = DWDL(1) + MU(K)*EXP(AL(K)*LOG(EVM1(I)))
           ELSE
            DWDL(1) = DWDL(1) + MU(K)
           ENDIF      
          ENDIF
          IF(EVM2(I)>ZERO) THEN
           IF(AL(K)/=ZERO) THEN
            DWDL(2) = DWDL(2) + MU(K)*EXP(AL(K)*LOG(EVM2(I)))      
           ELSE 
            DWDL(2) = DWDL(2) + MU(K)
           ENDIF
          ENDIF
          IF(EVM3(I)>ZERO) THEN
           IF(AL(K)/=ZERO) THEN  
            DWDL(3) = DWDL(3) + MU(K)*EXP(AL(K)*LOG(EVM3(I))) 
           ELSE
            DWDL(3) = DWDL(3) + MU(K)
           ENDIF
          ENDIF   
        ENDDO                                                  
        DWDRV    = P(I)*(RV(I)- ONE)
        SUMDWDL  =(DWDL(1)+DWDL(2)+DWDL(3))* THIRD
c----------------------------
C     principal Cauchy stress
        T(I,1) = (DWDL(1)-(SUMDWDL))/RV(I) + DWDRV
        T(I,2) = (DWDL(2)-(SUMDWDL))/RV(I) + DWDRV
        T(I,3) = (DWDL(3)-(SUMDWDL))/RV(I) + DWDRV
      ENDDO
C      
c----------------------------
c     tension cut                                                            
      DO I=1,NEL
        IF (OFF(I) /= ZERO .AND.                                             
     .    (T(I,1) > ABS(TENSCUT) .OR. T(I,2) > ABS(TENSCUT))) THEN         
           T(I,1) = ZERO                                                   
           T(I,2) = ZERO                                                   
           T(I,3) = ZERO                                                   
           OFF(I) = ZERO                                                     
        ENDIF
      ENDDO
C     transform principal Cauchy stress to global directions
      DO I=1,NEL
        SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*T(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*T(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*T(I,3)     
        SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*T(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*T(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*T(I,1)
        SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*T(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*T(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*T(I,2)
        SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*T(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*T(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*T(I,3)
        SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*T(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*T(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*T(I,1)
        SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*T(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*T(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*T(I,2)
      ENDDO
C-----------------------------------------------------------
C     set sound speed & viscosity
      DO I=1,NEL
        SOUNDSP(I) = SQRT((TWO_THIRD*GTMAX(I)+P(I))/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
C-----------      
      RETURN
      END

