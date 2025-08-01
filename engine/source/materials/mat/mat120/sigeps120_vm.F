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
!||    sigeps120_vm   ../engine/source/materials/mat/mat120/sigeps120_vm.F
!||--- called by ------------------------------------------------------
!||    sigeps120      ../engine/source/materials/mat/mat120/sigeps120.F
!||====================================================================
      SUBROUTINE SIGEPS120_VM(
     1      NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2      UPARAM  ,UVAR    ,OFF     ,PLA     ,EPSD    ,SOUNDSP ,
     3      EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     4      DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5      SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6      SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7      INLOC   ,DPLANL  ,DMG     , DMG_SCALE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
C----------------------------------------------------------------
C  D u m m y   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,INLOC
      my_real :: TIME,TIMESTEP
      INTEGER ,DIMENSION(NEL)    ,INTENT(IN) :: NGL
      my_real ,DIMENSION(NUPARAM),INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL)    ,INTENT(IN) ::
     .         EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .         DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .         SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
c
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: EPSD,SOUNDSP
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: 
     .         SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: OFF,PLA,DPLANL,DMG,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER  :: I,II,ITER,NITER,NINDX,NINDF,ITRX,IDAM
      INTEGER  ,DIMENSION(NEL) :: INDX,INDF
c     material parameters
      my_real :: YOUNG,NU,SSP,G,G2,BULK,YLD0,QQ,BETA,HH,C11,C12,
     .   A1F,A2F,AS,D1C,D2C,D1F,D2F,D_TRX,D_JC,DM,EXPN
c     Johnson & Cook rate-dependency
      my_real CC,EPSDREF,EPSDMAX,FRATE,FCUT,ALPHA,ALPHAI
c     Local variables
      my_real FACR,TRIAX,RVOCE,FVM,I1P,IP,JP,TP,FACT,FACD,DFAC,DTINV,
     .   DLAM,DDEP,DFDLAM,DYLD_DPLA,DPLA_DLAM,DPHI_DLAM,
     .   GAMC,GAMF,NP_XX,NP_YY,NP_ZZ,NP_XY,NP_YZ,NP_ZX,
     .   NF_XX,NF_YY,NF_ZZ,NF_XY,NF_YZ,NF_ZX,DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX
      my_real ,DIMENSION(NEL) :: I1,J2,YLD,PHI,DAM,JCRATE,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,DPLA,JCR_LOG,GAMMA,DGAMM,PLA_NL,DPDT_NL
c--------------------------------
c     Material parameters
c--------------------------------
c     UPARAM(1)  = Young modulus E
c     UPARAM(2)  = Poisson's ratio nu
c     UPARAM(3)  = shear modulus   G
c     UPARAM(4)  = bulk modulus    K
c     UPARAM(5)  = Yld0: instant yield stress 
c     UPARAM(6)  = Q : expontial term coefficient in the hardening law
c     UPARAM(7)  = BETA : exponent of the exponential term 
c     UPARAM(8)  = P : multiplier of the linear term in the hardening law
c     UPARAM(9)  = A1F : parameter of the yield function
c     UPARAM(10) = A2F : parameter of the yield function
c     UPARAM(11) = A1H : distortionnal yield hardening coefficiant
c     UPARAM(12) = A2H : distortionnal yield hardening coefficiant
c     UPARAM(13) = AS : parameter of the potential function
c     UPARAM(14) = D1C: first  damage strain parameter in initial damage threshold
c     UPARAM(15) = D2C: second damage strain parameter in initial damage threshold 
c     UPARAM(16) = D1F: first  damage strain parameter in final damage threshold
c     UPARAM(17) = D2F: second damage strain parameter in final damage threshold
c     UPARAM(18) = D_TRX : triaxiality factor in damage formula
c     UPARAM(19) = D_JC: JC strain rate coefficient in damage formula
c     UPARAM(20) = EXPN exponent in damage evolution
c     UPARAM(21) = CC : Johnson-Cook strain rate-dependency coefficient
c     UPARAM(22) = EPSDREF quasi-static reference strain rate
c     UPARAM(23) = EPSDMAX maximal reference strain rate
c     UPARAM(24) = FCUT : cut frequency for strain rate filtering
c     UPARAM(25) = IFORM = 1: Yield formulation flag  => Drucker-Prager in tension
c                  IFORM = 2: Yield formulation flag  => Von Mises in tension
c     UPARAM(26) = ITRX = 1 : pressure dependent for all T values
c                  ITRX = 2 : no pressure dependency when T < 0
c     UPARAM(27) = IDAM = 1 : damage model without turning point
c                  IDAM = 2 : damage model with turning point
c     UPARAM(28) = SSP  : sound speed
c     UPARAM(29) = Table Id
c     UPARAM(30) = Xscale for yld function
c     UPARAM(31) = Yscale for yld function
c     UPARAM(32) = Elastic modulus C11
c     UPARAM(33) = Elastic modulus C12
c--------------------------------     
c     UVAR(1)    : ARCL = PLA / (1-DAM) or non local plastic strain
c     UVAR(2)    : DAM
c     UVAR(3)    : TOTAL STRAIN RATE
c     UVAR(4)    : plastic function residual (for output/convergence check)
c     UVAR(5)    : non local plastic strain
C=======================================================================
      YOUNG = UPARAM(1)
      NU    = UPARAM(2)
      G     = UPARAM(3)
      BULK  = UPARAM(4)
c     Parameters of yield function/hardening law and flow potential
      YLD0  = UPARAM(5)
      QQ    = UPARAM(6)
      BETA  = UPARAM(7)
      HH    = UPARAM(8)
      A1F   = UPARAM(9) 
      A2F   = UPARAM(10)
c      A1H   = UPARAM(11)
c      A2H   = UPARAM(12)
      AS    = UPARAM(13)
c     Johnson-Cook failure parameters
      D1C   = UPARAM(14)
      D2C   = UPARAM(15)
      D1F   = UPARAM(16)
      D2F   = UPARAM(17)
      D_TRX = UPARAM(18)
      D_JC  = UPARAM(19)
      EXPN  = UPARAM(20) 
c     Johnson-Cook strain rate-dependency and distortional hardening
      CC      = UPARAM(21) 
      EPSDREF = UPARAM(22) 
      EPSDMAX = UPARAM(23) 
      FCUT    = UPARAM(24)
c
      ITRX    = NINT(UPARAM(26)) 
      IDAM    = NINT(UPARAM(27))
      SSP     = UPARAM(28)
      C11     = UPARAM(32)
      C12     = UPARAM(33)      
c
      G2     = G * TWO
      ALPHA  = 0.005
      ALPHAI = ONE - ALPHA
c
      IF (INLOC > 0) THEN
        DTINV = ONE / MAX(TIMESTEP, EM20)
        DO I = 1,NEL
          PLA_NL(I)  = UVAR(I,5) + MAX(DPLANL(I),ZERO)
          UVAR(I,5)  = PLA_NL(I)
          DPDT_NL(I) = MIN(MAX(DPLANL(I),ZERO)*DTINV ,EPSDMAX)
        ENDDO
      ENDIF
c------------------------------------------------------------------
c     Computation of the nominal trial stress tensor (non damaged)
c------------------------------------------------------------------
      DAM (1:NEL) = DMG(1:NEL)  
      DPLA(1:NEL)   = ZERO          
c
      DO I = 1,NEL
        IF (OFF(I) < 0.001) OFF(I) = ZERO
        IF (OFF(I) < ONE)    OFF(I) = OFF(I)*FOUR_OVER_5
        IF (OFF(I) == ONE) THEN
          SIGNXX(I) = SIGOXX(I)  + C11*DEPSXX(I) + C12*(DEPSYY(I) + DEPSZZ(I))
          SIGNYY(I) = SIGOYY(I)  + C11*DEPSYY(I) + C12*(DEPSXX(I) + DEPSZZ(I))
          SIGNZZ(I) = SIGOZZ(I)  + C11*DEPSZZ(I) + C12*(DEPSXX(I) + DEPSYY(I))
          SIGNXY(I) = SIGOXY(I)  + DEPSXY(I)*G 
          SIGNYZ(I) = SIGOYZ(I)  + DEPSYZ(I)*G
          SIGNZX(I) = SIGOZX(I)  + DEPSZX(I)*G
          I1(I)     = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
c
          SXX(I) = SIGNXX(I) - I1(I) * THIRD
          SYY(I) = SIGNYY(I) - I1(I) * THIRD
          SZZ(I) = SIGNZZ(I) - I1(I) * THIRD
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          J2(I)   = (SXX(I)**2 + SYY(I)**2 + SZZ(I)**2)*HALF
     .            +  SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        END IF
      ENDDO
c     Johnson & Cook rate dependency  (total strain rate)
      JCR_LOG(1:NEL) = ZERO
      JCRATE(1:NEL)  = ONE
      IF (EPSDREF > ZERO) THEN
        IF (INLOC == 1) THEN   ! non local plastic strain rate
          JCR_LOG(1:NEL) = LOG(DPDT_NL(1:NEL) / EPSDREF)
          JCRATE(1:NEL)  = ONE + CC * JCR_LOG(1:NEL)
        ELSE                   ! total strain rate
          DO I = 1,NEL
            IF (OFF(I) == ONE) THEN
              EPSD(I) = (EPSPXX(I)**2 + EPSPYY(I)**2 + EPSPZZ(I)**2 )*TWO
     .                +  EPSPXY(I)**2 + EPSPYZ(I)**2 + EPSPZX(I)**2
              EPSD(I) = MIN(SQRT(EPSD(I)) ,EPSDMAX)
              EPSD(I) = ALPHA*EPSD(I) + ALPHAI*UVAR(I,3)
              UVAR(I,3) = EPSD(I)
              IF (EPSD(I) > EPSDREF) THEN
                JCR_LOG(I) = LOG(EPSD(I) / EPSDREF)
                JCRATE(I)  = ONE + CC * JCR_LOG(I)
              END IF
            END IF
          ENDDO
        END IF
      END IF
c------------------------------------------------------------------
c     Actual yield value, yield criterion and plastic flow function
c------------------------------------------------------------------
      DO I = 1,NEL
        RVOCE  = QQ*(ONE - EXP(-BETA*PLA(I))) + HH*PLA(I)
        YLD(I) = (YLD0 + RVOCE) * JCRATE(I)
        FVM    = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F )
        PHI(I) = J2(I) + THIRD*A2F * FVM**2 
     .         - (YLD(I)**2 + FOURTH*(A1F*YLD0)**2/A2F)
      ENDDO
c------------------------------------------------------------------
c     Plasticity
c------------------------------------------------------------------
      NINDX = 0
      NINDF = 0
      DO I = 1,NEL
        IF (PHI(I) > ZERO .and. OFF(I) == ONE) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        END IF
      ENDDO
c
      DO II = 1,NINDX
        I = INDX(II)
        DPLA(I) = ZERO
        I1P   = MAX(ZERO, I1(I))
        FVM   = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F)
        NITER = 4
c
        DO ITER = 1,NITER

          ! normal to non associated plastic flow surface = d_psi/d_sig
          JP    = THIRD * AS * I1P
          TP    = TWO  * JP
c
          NP_XX = SXX(I) + TP
          NP_YY = SYY(I) + TP
          NP_ZZ = SZZ(I) + TP
          NP_XY = SXY(I)
          NP_YZ = SYZ(I)
          NP_ZX = SZX(I)
c
          ! normal to plastic yield surface = d_phi/d_sig
          IP = TWO_THIRD * A2F * FVM
          NF_XX = SXX(I) + IP
          NF_YY = SYY(I) + IP
          NF_ZZ = SZZ(I) + IP
          NF_XY = SXY(I)
          NF_YZ = SYZ(I)
          NF_ZX = SZX(I)
c         
          ! derivative of yld criterion : dphi/dlam = d_phi/d_sig * dsig/dlam
          DFDLAM =-NF_XX * (C11*NP_XX + C12 * (NP_YY + NP_ZZ))
     .           - NF_YY * (C11*NP_YY + C12 * (NP_XX + NP_ZZ))
     .           - NF_ZZ * (C11*NP_ZZ + C12 * (NP_XX + NP_YY))
     .           -(NF_XY*NP_XY + NF_YZ*NP_YZ + NF_ZX*NP_ZX)*TWO*G2
c
          DYLD_DPLA = (HH + BETA*QQ*EXP(-BETA*PLA(I))) * JCRATE(I)
          DPLA_DLAM = TWO*SQRT((J2(I) + TP*AS*I1(I)))
          DPHI_DLAM = DFDLAM - TWO*YLD(I)*DYLD_DPLA*DPLA_DLAM
c----------------------------------------------------------------
          DLAM = -PHI(I) / DPHI_DLAM
c----------------------------------------------------------------
          ! Plastic strains tensor update
          DPXX = DLAM*NP_XX                          
          DPYY = DLAM*NP_YY                          
          DPZZ = DLAM*NP_ZZ                          
          DPXY = DLAM*NP_XY                          
          DPYZ = DLAM*NP_YZ                          
          DPZX = DLAM*NP_ZX                 
          ! Elasto-plastic stresses update   
          SIGNXX(I) = SIGNXX(I) - (C11*DPXX + C12*(DPYY + DPZZ)) 
          SIGNYY(I) = SIGNYY(I) - (C11*DPYY + C12*(DPXX + DPZZ))
          SIGNZZ(I) = SIGNZZ(I) - (C11*DPZZ + C12*(DPXX + DPYY))
          SIGNXY(I) = SIGNXY(I) - G2*DPXY     
          SIGNYZ(I) = SIGNYZ(I) - G2*DPYZ  
          SIGNZX(I) = SIGNZX(I) - G2*DPZX
c
          ! Plastic strain increments update
          DDEP   = DLAM * DPLA_DLAM
          DPLA(I)= MAX(ZERO, DPLA(I) + DDEP)
          PLA(I) = PLA(I) + DDEP
c----------------------        
c         criterion update  
c----------------------        
          I1(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
          I1P    = MAX(ZERO, I1(I))
          SXX(I) = SIGNXX(I) - I1(I)*THIRD
          SYY(I) = SIGNYY(I) - I1(I)*THIRD
          SZZ(I) = SIGNZZ(I) - I1(I)*THIRD
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          J2(I)  =(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 ) * HALF
     .           + SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          RVOCE  = QQ*(ONE - EXP(-BETA*PLA(I))) + HH*PLA(I)
          YLD(I) = (YLD0 + RVOCE) * JCRATE(I)
          FVM    = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F)
          PHI(I) = J2(I) + THIRD*A2F * FVM**2 
     .           - (YLD(I)**2 + FOURTH*(A1F*YLD0)**2/A2F)
        END DO  ! Newton iterations
c
        UVAR(I,4) = PHI(I) / YLD0**2
      ENDDO   ! II = 1,NINDX     
c---------------------------------------------------------------------
c     Update damage
c---------------------------------------------------------------------
      IF (IDAM > 0) THEN
        IF (IDAM == 1) THEN
          IF (INLOC == 1) THEN
            DGAMM(1:NEL) = DPLANL(1:NEL)
            GAMMA(1:NEL) = PLA_NL(1:NEL)
          ELSE
            DGAMM(1:NEL) = DPLA(1:NEL)
            GAMMA(1:NEL) = PLA(1:NEL)
          END IF
        ELSE
          IF (INLOC == 1) THEN
            DGAMM(1:NEL)  = DPLANL(1:NEL) * DMG_SCALE(1:NEL)  
            UVAR(1:NEL,1) = UVAR(1:NEL,1) + DGAMM(1:NEL)
            GAMMA (1:NEL) = UVAR(1:NEL,1)
          ELSE
            DGAMM(1:NEL)  = DPLA(1:NEL) * DMG_SCALE(1:NEL)  
            UVAR(1:NEL,1) = UVAR(1:NEL,1) + DGAMM(1:NEL)
            GAMMA (1:NEL) = UVAR(1:NEL,1)
          END IF
        END IF
c
        DO I = 1,NEL
c
          TRIAX = ZERO
          FACT = ONE
          FACR = ONE + D_JC * JCR_LOG(I)  ! total strain rate factor on damage
          IF ( J2(I)/= ZERO)TRIAX = THIRD * I1(I) / SQRT(THREE*J2(I))
          IF (ITRX == 1 ) THEN
            FACT  = EXP(-D_TRX*TRIAX)
          ELSE
           IF (TRIAX > ZERO )FACT  = EXP(-D_TRX*TRIAX)
          ENDIF
          GAMC = (D1C + D2C * FACT) * FACR
          GAMF = (D1F + D2F * FACT) * FACR
          IF (GAMMA(I) > GAMC) THEN
            IF (DAM(I) == ZERO) THEN
              NINDF = NINDF + 1
              INDF(NINDF) = I
            END IF
            IF (EXPN == ONE) THEN
              DAM(I) = DAM(I) + DGAMM(I) / (GAMF - GAMC)
            ELSE
              DFAC   = (GAMMA(I) - GAMC) / (GAMF - GAMC)
              DAM(I) = DAM(I) + EXPN * DFAC**(EXPN-ONE) * DGAMM(I) / (GAMF - GAMC)
            END IF
            IF (DAM(I) >= ONE) THEN
              DAM(I) = ONE
              OFF(I) = FOUR_OVER_5
            END IF
            DMG(I) = DAM(I)
          END IF ! GAMMA > GAMC
        ENDDO
c       Update damaged stress
        DO I = 1, NEL
          DMG_SCALE(I) = ONE - DAM(I)   
        END DO
      END IF
c-------------------------
      SOUNDSP(1:NEL) = SSP
c-------------------------
      IF (NINDF > 0) THEN
        DO II=1,NINDF
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDF(II))
          WRITE(ISTDO,1100) NGL(INDF(II)),TIME
#include "lockoff.inc"
        ENDDO
      END IF
c-----------------------------------------------------------------
 1000 FORMAT(1X,'START DAMAGE IN SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'START DAMAGE IN SOLID ELEMENT NUMBER ',I10,1X,' AT TIME :',G11.4) 
c-----------------------------------------------------------------
      RETURN
      END
