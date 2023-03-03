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
Chd|  SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS35(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF                    )

C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR

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
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     . FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,J,KF,IFLAG,ICORRECT

      my_real
     . E,POISSON,A,B,ET,POISSONT,FAC
      my_real
     . GT2,BULKT3,RELVEXP
      my_real
     . VMU,VLAMDA,VMU2,VLAMDA3
      my_real
     . C1,C2,C3,PMIN,DPDMU
      my_real
     . P0,PHI,GAMA0
      my_real
     . ENEW(MVSIZ),EDOT(MVSIZ),DEDOT(MVSIZ)
      my_real
     . DPDGAMA(MVSIZ),GAMA(MVSIZ),AMU(MVSIZ)
      my_real
     . SM(MVSIZ),EM(MVSIZ),DEDM(MVSIZ)
      my_real
     . G2(MVSIZ),BULK(MVSIZ),BULK3(MVSIZ)
      my_real
     . DSXX(MVSIZ),DSYY(MVSIZ),DSZZ(MVSIZ)
      my_real
     . DSXY(MVSIZ),DSYZ(MVSIZ),DSZX(MVSIZ)
      my_real
     . DEXX(MVSIZ),DEYY(MVSIZ),DEZZ(MVSIZ)
      my_real
     . DEXY(MVSIZ),DEYZ(MVSIZ),DEZX(MVSIZ)
      my_real
     . DEDXX(MVSIZ),DEDYY(MVSIZ),DEDZZ(MVSIZ)
      my_real
     . DEDXY(MVSIZ),DEDYZ(MVSIZ),DEDZX(MVSIZ)
      my_real
     . DSDXX(MVSIZ),DSDYY(MVSIZ),DSDZZ(MVSIZ)
      my_real
     . DSDXY(MVSIZ),DSDYZ(MVSIZ),DSDZX(MVSIZ)
      my_real
     . DPDRO(MVSIZ),P(MVSIZ),PDELT(MVSIZ),RELVOL(MVSIZ)
      my_real
     . SIGAIR(MVSIZ)

      my_real
     . SMALL,TINY , AUX1, AUX2, 
     . MIDSTEP
      LOGICAL TEST
C----------------------------------------------------------------
      TIME    =ZERO
      TIMESTEP=ONE

      SMALL = EM3
      TINY  = EM30
      TEST=.FALSE.
       

C SET INITIAL MATERIAL CONSTANTS

      E       = UPARAM(1)
      POISSON = UPARAM(2)
      A       = UPARAM(3)
      B       = UPARAM(4)

      ET      = UPARAM(5)
      POISSONT= UPARAM(6)
      VMU2    = UPARAM(7)*TWO
      VLAMDA3 = UPARAM(8)*THREE

      P0      = UPARAM(9)
      PHI     = UPARAM(10)
      GAMA0   = UPARAM(11)

      C1      = UPARAM(12)
      C2      = UPARAM(13)
      C3      = UPARAM(14)

      IFLAG   = UPARAM(15)
      PMIN    = UPARAM(16)
      RELVEXP = UPARAM(17)

      FAC     = UPARAM(18)

      KF      = IFUNC(1)

      GT2     = ET/(ONE+POISSONT)
      BULKT3  = ET/(ONE-TWO*POISSONT)

C
      DO I=1,NEL
        EPSXX(I)=EPSXX(I)-HALF*DEPSXX(I)
        EPSYY(I)=EPSYY(I)-HALF*DEPSYY(I)
        EPSZZ(I)=EPSZZ(I)-HALF*DEPSZZ(I)
        EPSXY(I)=EPSXY(I)-HALF*DEPSXY(I)
        EPSYZ(I)=EPSYZ(I)-HALF*DEPSYZ(I)
        EPSZX(I)=EPSZX(I)-HALF*DEPSZX(I)
      END DO

      DO 200 I=1,NEL
        GAMA(I) = (RHO0(I)/RHO(I)-ONE+GAMA0)
        IF(ONE+GAMA(I)-PHI<=SMALL) GAMA(I)=-(ONE-PHI-SMALL)
        SM(I)=THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))+UVAR(I,1)
        EM(I)=THIRD*(EPSXX(I)+EPSYY(I)+EPSZZ(I))
        DEDM(I)=THIRD*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
        SIGAIR(I)=MAX(ZERO,-(P0*GAMA(I))/(ONE+GAMA(I)-PHI))
  200 CONTINUE

      DO 210 I=1,NEL
C COMPUTE DEVIATORIC STRESSES
        DSXX(I)=SIGOXX(I)-SM(I)+UVAR(I,1)
        DSYY(I)=SIGOYY(I)-SM(I)+UVAR(I,1)
        DSZZ(I)=SIGOZZ(I)-SM(I)+UVAR(I,1)
        DSXY(I)=SIGOXY(I)
        DSYZ(I)=SIGOYZ(I)
        DSZX(I)=SIGOZX(I)
        
C  COMPUTE DEVIATORIC STRAINS
        DEXX(I)=EPSXX(I)-EM(I)
        DEYY(I)=EPSYY(I)-EM(I)
        DEZZ(I)=EPSZZ(I)-EM(I)
        DEXY(I)=EPSXY(I)* HALF
        DEYZ(I)=EPSYZ(I)* HALF
        DEZX(I)=EPSZX(I)* HALF

C  COMPUTE DEVIATORIC STRAIN RATES
c        DEDTXX(I)=EPSPXX(I)-DEDTM(I)
c        DEDTYY(I)=EPSPYY(I)-DEDTM(I)
c        DEDTZZ(I)=EPSPZZ(I)-DEDTM(I)
c        DEDTXY(I)=EPSPXY(I)*HALF
c        DEDTYZ(I)=EPSPYZ(I)*HALF
c        DEDTZX(I)=EPSPZX(I)*HALF

C  COMPUTE DEVIATORIC STRAIN INCREMENTS
        DEDXX(I)=DEPSXX(I)-DEDM(I)
        DEDYY(I)=DEPSYY(I)-DEDM(I)
        DEDZZ(I)=DEPSZZ(I)-DEDM(I)
        DEDXY(I)=DEPSXY(I)*HALF
        DEDYZ(I)=DEPSYZ(I)*HALF
        DEDZX(I)=DEPSZX(I)*HALF
  210 CONTINUE

      DO I=1,NEL
        RELVOL(I)=RHO0(I)/RHO(I)
        EDOT(I)  =ZERO
      ENDDO

C UPDATE ELASTIC MODULUS
      DO I=1,NEL
        ENEW(I)=(MAX(E,A*EDOT(I)+B))/RELVOL(I)**RELVEXP
       ENDDO

      AUX1 = ONE / (ONE+POISSON)
      AUX2 = ONE / (ONE-TWO*POISSON) 
      DO I=1,NEL
        G2(I)=ENEW(I)*AUX1
        BULK3(I)=ENEW(I)*AUX2
      ENDDO
C COMPUTE DEVIATORIC STRESS RATE INCREMENTS
      DO I=1,NEL
C
C       delta_sigma * VMU2 = ...
        DSDXX(I)=VMU2*G2(I)*DEDXX(I)
     &           -(G2(I)+GT2)*DSXX(I)+G2(I)*GT2*DEXX(I)
        DSDYY(I)=VMU2*G2(I)*DEDYY(I)
     &           -(G2(I)+GT2)*DSYY(I)+G2(I)*GT2*DEYY(I)
        DSDZZ(I)=VMU2*G2(I)*DEDZZ(I)
     &           -(G2(I)+GT2)*DSZZ(I)+G2(I)*GT2*DEZZ(I)
        DSDXY(I)=VMU2*G2(I)*DEDXY(I)
     &           -(G2(I)+GT2)*DSXY(I)+G2(I)*GT2*DEXY(I)
        DSDYZ(I)=VMU2*G2(I)*DEDYZ(I)
     &           -(G2(I)+GT2)*DSYZ(I)+G2(I)*GT2*DEYZ(I)
        DSDZX(I)=VMU2*G2(I)*DEDZX(I)
     &           -(G2(I)+GT2)*DSZX(I)+G2(I)*GT2*DEZX(I)
C
C       delta_sigma  = ...
        MIDSTEP =ONE/(VMU2+HALF*(G2(I)+GT2))
        DSDXX(I)=DSDXX(I)*MIDSTEP
        DSDYY(I)=DSDYY(I)*MIDSTEP
        DSDZZ(I)=DSDZZ(I)*MIDSTEP
        DSDXY(I)=DSDXY(I)*MIDSTEP
        DSDYZ(I)=DSDYZ(I)*MIDSTEP
        DSDZX(I)=DSDZX(I)*MIDSTEP
      ENDDO


C COMPUTE PRESSURE
      DO I=1,NEL
        IF(KF/=0) THEN
          AMU(I)=RHO(I)/RHO0(I)-ONE
          IF(IFLAG==0) THEN
            P(I)=-FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU)
          ELSE
            PDELT(I)=( C1*(BULK3(I)*DEDM(I))*(VLAMDA3+VMU2)+
     &               (-C2*((BULK3(I)+BULKT3)*SM(I))
     &                +C3*((BULK3(I)*BULKT3)*EM(I))))
     &             / ((VLAMDA3+VMU2)+C2*(BULK3(I)+BULKT3)*HALF)
            P(I)=SM(I)+PDELT(I)
     &           -FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU)
          ENDIF
        ELSE
          PDELT(I)=( C1*(BULK3(I)*DEDM(I))*(VLAMDA3+VMU2)+
     &               (-C2*((BULK3(I)+BULKT3)*SM(I))
     &                +C3*((BULK3(I)*BULKT3)*EM(I))))
     &           / ((VLAMDA3+VMU2)+C2*(BULK3(I)+BULKT3)*HALF)
          P(I)=SM(I)+PDELT(I)
        ENDIF
        IF(P(I)<=PMIN) P(I)=PMIN
      ENDDO

C COMPUTE SOUND SPEED 
      DO I=1,NEL
        DPDRO(I)=TWO_THIRD*G2(I)+THIRD*(BULK3(I)
     &          +P0*(ONE-PHI)/(ONE+GAMA(I)-PHI)**TWO)
      ENDDO

      DO I=1,NEL
        SOUNDSP(I)=SQRT(DPDRO(I)/RHO0(I))
      ENDDO

C COMPUTE UPDATED STRESSES
      DO I=1,NEL
        SIGNXX(I)=DSXX(I)+DSDXX(I)+P(I)-SIGAIR(I)
        SIGNYY(I)=DSYY(I)+DSDYY(I)+P(I)-SIGAIR(I)
        SIGNZZ(I)=DSZZ(I)+DSDZZ(I)+P(I)-SIGAIR(I)
        SIGNXY(I)=DSXY(I)+DSDXY(I)
        SIGNYZ(I)=DSYZ(I)+DSDYZ(I)
        SIGNZX(I)=DSZX(I)+DSDZX(I)
        VISCMAX(I)= ZERO
        UVAR(I,1)=SIGAIR(I)
       ENDDO

      RETURN

      END
