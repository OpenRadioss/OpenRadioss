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
Chd|  SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
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
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , ISRATE, ASRATE,
     B      EDOT   )

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
#include      "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR , ISRATE

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
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL), ASRATE
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
      my_real UVAR(NEL,NUVAR), OFF(NEL) , EDOT(NEL)
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
     . E,POISSON,A,B,ET,POISSONT,FAC,EPSP
      my_real
     . GT2,BULKT3,RELVEXP
      my_real
     . VMU,VLAMDA,VMU2,VLAMDA3
      my_real
     . C1,C2,C3,PMIN,DPDMU(MVSIZ)
      my_real
     . P0,PHI,GAMA0
      my_real
     . ENEW(MVSIZ),DEDOT(MVSIZ)
      my_real
     . DPDGAMA(MVSIZ),GAMA(MVSIZ),AMU(MVSIZ)
      my_real
     . SM(MVSIZ),EM(MVSIZ),DEDTM(MVSIZ)
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
     . DEDTXX(MVSIZ),DEDTYY(MVSIZ),DEDTZZ(MVSIZ)
      my_real
     . DEDTXY(MVSIZ),DEDTYZ(MVSIZ),DEDTZX(MVSIZ)
      my_real
     . DSDTXX(MVSIZ),DSDTYY(MVSIZ),DSDTZZ(MVSIZ)
      my_real
     . DSDTXY(MVSIZ),DSDTYZ(MVSIZ),DSDTZX(MVSIZ)
      my_real
     . DPDRO(MVSIZ),P(MVSIZ),PDOT(MVSIZ),RELVOL(MVSIZ)
      my_real
     . SIGAIR(MVSIZ)

      my_real
     . SMALL,TINY , AUX1, AUX2, 
     . MIDSTEP, DT05    
      LOGICAL TEST
C----------------------------------------------------------------
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

      ICORRECT=0
      IF(NUPARAM.GE.19) ICORRECT=NINT(UPARAM(19))
C
      IF(ICORRECT.EQ.0)THEN
        DT05=HALF*TIMESTEP
      ELSE
        DT05=ZERO
      END IF
C
      DO I=1,NEL
        EPSXX(I)=EPSXX(I)-DT05*EPSPXX(I)
        EPSYY(I)=EPSYY(I)-DT05*EPSPYY(I)
        EPSZZ(I)=EPSZZ(I)-DT05*EPSPZZ(I)
        EPSXY(I)=EPSXY(I)-DT05*EPSPXY(I)
        EPSYZ(I)=EPSYZ(I)-DT05*EPSPYZ(I)
        EPSZX(I)=EPSZX(I)-DT05*EPSPZX(I)
      END DO

      DO 200 I=1,NEL
        GAMA(I) = (RHO0(I)/RHO(I)-ONE+GAMA0)
        IF(ONE+GAMA(I)-PHI.LE.SMALL) GAMA(I)=-(ONE-PHI-SMALL)
        SM(I)=THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))+UVAR(I,1)
        EM(I)=THIRD*(EPSXX(I)+EPSYY(I)+EPSZZ(I))
        DEDTM(I)=THIRD*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
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
        DEDTXX(I)=EPSPXX(I)-DEDTM(I)
        DEDTYY(I)=EPSPYY(I)-DEDTM(I)
        DEDTZZ(I)=EPSPZZ(I)-DEDTM(I)
        DEDTXY(I)=EPSPXY(I)*HALF
        DEDTYZ(I)=EPSPYZ(I)*HALF
        DEDTZX(I)=EPSPZX(I)*HALF
  210 CONTINUE

      DO I=1,NEL
        RELVOL(I)=RHO0(I)/RHO(I)
        EPSP = MAX(
     &    ABS(EPSPXX(I)),ABS(EPSPYY(I)),ABS(EPSPZZ(I)),
     &    ABS(EPSPXY(I)),ABS(EPSPYZ(I)),ABS(EPSPZX(I)))
        IF (ISRATE == 0) THEN 
          EDOT(I) = EPSP
        ELSE
          EDOT(I)   = ASRATE*EPSP + (ONE - ASRATE)*UVAR(I,4)
          UVAR(I,4) = EDOT(I)
        ENDIF
      ENDDO

C UPDATE ELASTIC MODULUS
      IF(RELVEXP/=ZERO) THEN
       DO I=1,NEL
        ENEW(I)=(MAX(E,A*EDOT(I)+B))/(EXP(RELVEXP*LOG(RELVOL(I))))
       ENDDO
      ELSE
       DO I=1,NEL
        ENEW(I)=(MAX(E,A*EDOT(I)+B))
       ENDDO
      ENDIF
       

      AUX1 = ONE / (ONE+POISSON)
      AUX2 = ONE / (ONE-TWO*POISSON) 
      DO I=1,NEL
        G2(I)=ENEW(I)*AUX1
        BULK3(I)=ENEW(I)*AUX2
      ENDDO
C COMPUTE DEVIATORIC STRESS RATE INCREMENTS
      DO I=1,NEL
        DSDTXX(I)=G2(I)*DEDTXX(I)-(G2(I)+GT2)*DSXX(I)/VMU2+
     &            G2(I)*GT2*DEXX(I)/VMU2
        DSDTYY(I)=G2(I)*DEDTYY(I)-(G2(I)+GT2)*DSYY(I)/VMU2+
     &            G2(I)*GT2*DEYY(I)/VMU2
        DSDTZZ(I)=G2(I)*DEDTZZ(I)-(G2(I)+GT2)*DSZZ(I)/VMU2+
     &            G2(I)*GT2*DEZZ(I)/VMU2
        DSDTXY(I)=G2(I)*DEDTXY(I)-(G2(I)+GT2)*DSXY(I)/VMU2+
     &            G2(I)*GT2*DEXY(I)/VMU2
        DSDTYZ(I)=G2(I)*DEDTYZ(I)-(G2(I)+GT2)*DSYZ(I)/VMU2+
     &            G2(I)*GT2*DEYZ(I)/VMU2
        DSDTZX(I)=G2(I)*DEDTZX(I)-(G2(I)+GT2)*DSZX(I)/VMU2+
     &            G2(I)*GT2*DEZX(I)/VMU2
        MIDSTEP=ONE/(ONE+(G2(I)+GT2)/VMU2*DT05)
        DSDTXX(I)=DSDTXX(I)*MIDSTEP
        DSDTYY(I)=DSDTYY(I)*MIDSTEP
        DSDTZZ(I)=DSDTZZ(I)*MIDSTEP
        DSDTXY(I)=DSDTXY(I)*MIDSTEP
        DSDTYZ(I)=DSDTYZ(I)*MIDSTEP
        DSDTZX(I)=DSDTZX(I)*MIDSTEP
      ENDDO


C COMPUTE PRESSURE
      DO I=1,NEL
        IF(KF.NE.0) THEN
          AMU(I)=RHO(I)/RHO0(I)-ONE
          IF(IFLAG.EQ.0) THEN
            P(I)=-FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU(I))
          ELSE
            PDOT(I)=( C1*(BULK3(I)*DEDTM(I))
     &               -C2*((BULK3(I)+BULKT3)*SM(I)/(VLAMDA3+VMU2))
     &               +C3*((BULK3(I)*BULKT3)*EM(I)/(VLAMDA3+VMU2)))
     &             / (ONE+C2*(BULK3(I)+BULKT3)/(VLAMDA3+VMU2)*DT05)
            P(I)=SM(I)+PDOT(I)*TIMESTEP
     &           -FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU(I))
          ENDIF
        ELSE
          PDOT(I)=( C1*(BULK3(I)*DEDTM(I))
     &             -C2*((BULK3(I)+BULKT3)*SM(I)/(VLAMDA3+VMU2))
     &             +C3*((BULK3(I)*BULKT3)*EM(I)/(VLAMDA3+VMU2)))
     &             / (ONE+C2*(BULK3(I)+BULKT3)/(VLAMDA3+VMU2)*DT05)
          P(I)=SM(I)+PDOT(I)*TIMESTEP
        ENDIF
        IF(P(I).LE.PMIN) P(I)=PMIN
      ENDDO

C COMPUTE SOUND SPEED 
      DO I=1,NEL
        IF(KF==0)THEN
          DPDRO(I)=TWO_THIRD*G2(I)+THIRD*BULK3(I)
     &            +P0*(ONE-PHI)/(ONE+GAMA(I)-PHI)**2
        ELSEIF(IFLAG==0)THEN
C
C BULK may be unnecessary ; stability over 2 cycles is not fully insured.
C---normally BULK3 is not used in this case, just keep it as assurance
          DPDRO(I)=TWO_THIRD*G2(I)
     &            +MAX(THIRD*BULK3(I),ABS(FAC*DPDMU(I)))
     &            +P0*(ONE-PHI)/(ONE+GAMA(I)-PHI)**2
        ELSE
C
C stability over 2 cycles is not fully insured.
          DPDRO(I)=TWO_THIRD*G2(I)+THIRD*BULK3(I)+ABS(FAC*DPDMU(I))
     &            +P0*(ONE-PHI)/(ONE+GAMA(I)-PHI)**2
        END IF
      ENDDO

      DO I=1,NEL
        SOUNDSP(I)=SQRT(DPDRO(I)/RHO(I))
      ENDDO

C COMPUTE UPDATED STRESSES
      DO I=1,NEL
        SIGNXX(I)=DSXX(I)+DSDTXX(I)*TIMESTEP+P(I)-SIGAIR(I)
        SIGNYY(I)=DSYY(I)+DSDTYY(I)*TIMESTEP+P(I)-SIGAIR(I)
        SIGNZZ(I)=DSZZ(I)+DSDTZZ(I)*TIMESTEP+P(I)-SIGAIR(I)
        SIGNXY(I)=DSXY(I)+DSDTXY(I)*TIMESTEP
        SIGNYZ(I)=DSYZ(I)+DSDTYZ(I)*TIMESTEP
        SIGNZX(I)=DSZX(I)+DSDTZX(I)*TIMESTEP
        VISCMAX(I)= ZERO
        UVAR(I,1)=SIGAIR(I)
       ENDDO

      RETURN

      END
