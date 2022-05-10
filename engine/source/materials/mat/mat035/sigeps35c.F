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
Chd|  SIGEPS35C                     source/materials/mat/mat035/sigeps35c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS35C(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
     2     NPF    ,NPT    ,IPT     ,IFLA   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,UVAR    ,OFF    ,
     B     NGL    ,SHF    ,ETSE    ,ISRATE  ,ASRATE ,
     C     EDOT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER   NEL,NPT,IPT,IFLA,NUPARAM, NUVAR,NGL(NEL),ISRATE

      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO0  (NEL),  EINT(NEL,2),
     .      EPSPXX(NEL), EPSPYY(NEL),SHF(NEL), 
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), 
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), 
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), 
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      THKLY(NEL) ,THK(NEL),AREA(NEL),EDOT(NEL),
     .      ASRATE
c    RHO   (NEL) je suis oblige de calcule
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), 
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), 
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL),ETSE(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .        FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,J,KF,IFLAG,ICORRECT

      my_real 
     .        POISSON,E,E1,E2,G02,BULK0,
     .        GT2,BULKT,LAMDA,LAMDAT,RELVEXP,
     .        VMU2,VLAMDA,VBULK,FAC,
     .        C1,C2,C3,PMIN,DPDMU,EPSPC,
     .        P0,PHI,GAMA0, VAR,TWO,TWOTHIRD,
     .        DPDGAMA(MVSIZ),GAMA(MVSIZ),AMU(MVSIZ),
     .        SM(MVSIZ),EM(MVSIZ),DEDTM(MVSIZ),
     .        G2(MVSIZ),BULK(MVSIZ),
     .        DSXX(MVSIZ),DSYY(MVSIZ),DSXY(MVSIZ),
     .        DEXX(MVSIZ),DEYY(MVSIZ),DEZZ,
     .        DEXY(MVSIZ),EPSPZZ(MVSIZ),EPSZZ(MVSIZ),
     .        DEDTXX(MVSIZ),DEDTYY(MVSIZ),DEDTXY(MVSIZ),
     .        DSDTXX(MVSIZ),DSDTYY(MVSIZ),DSDTXY(MVSIZ),
     .        DPDRO(MVSIZ),P(MVSIZ),PDOT(MVSIZ),
     .        MG2(MVSIZ),PG2(MVSIZ),MK(MVSIZ),PK(MVSIZ),
     .        SIGAIR(MVSIZ),RELVOL(MVSIZ),ENEW(MVSIZ),
     .        RHO   (MVSIZ),EPSP

      my_real 
     .       SSM,EPSM,EPSPM,EPIN,SMALL,CSHEAR,MIDSTEP,DT05
C=======================================================================
      SMALL = EM3
      CSHEAR= ZEP426667
C --  INITIALIZATION---------
      IF (TIME == ZERO) THEN
        DO I=1,NEL
           UVAR(I,3) = AREA(I)*THK(I) ! VOLUME0
        ENDDO
      ENDIF
C SET INITIAL MATERIAL CONSTANTS

      POISSON= UPARAM(2)
      E      = UPARAM(1)
      G02    = UNDEMI*E/(1+POISSON)
      E1     = UPARAM(3)
      E2     = UPARAM(4)
      GT2    = UPARAM(5)/(UN+UPARAM(6))
      BULKT  = UPARAM(5)/(UN-DEUX*UPARAM(6))
      VMU2   = DEUX*UPARAM(7)
      VBULK  = TROIS*UPARAM(8)+VMU2      

      P0     = UPARAM(9)
      PHI    = UPARAM(10)
      GAMA0  = UPARAM(11)

      C1     = UPARAM(12)
      C2     = UPARAM(13)
      C3     = UPARAM(14)
      IFLAG  = UPARAM(15)
      PMIN   = UPARAM(16)
      RELVEXP = UPARAM(17)
      FAC     = UPARAM(18)

      KF     = IFUNC(1)
C
      ICORRECT=0
      IF(NUPARAM.GE.19) ICORRECT=NINT(UPARAM(19))
C
      IF(ICORRECT == 0)THEN
        DT05=UNDEMI*TIMESTEP
      ELSE
        DT05=ZERO
      END IF
C
      DO I=1,NEL
        EPSXX(I)=EPSXX(I)-DT05*EPSPXX(I)
        EPSYY(I)=EPSYY(I)-DT05*EPSPYY(I)
        EPSXY(I)=EPSXY(I)-DT05*EPSPXY(I)
        EPSYZ(I)=EPSYZ(I)-DT05*EPSPYZ(I)
        EPSZX(I)=EPSZX(I)-DT05*EPSPZX(I)
      END DO
C
      DO 200 I=1,NEL
c-------calcul E_eq-----
        EPSPM =EPSPXX(I)+EPSPYY(I)
        EPSPC =EPSPXX(I)-EPSPYY(I)
        EPSP  = SQRT(UNDEMI*(EPSPC*EPSPC+EPSPXX(I)*EPSPXX(I)+
     1          EPSPYY(I)*EPSPYY(I)) +TDEMI* (EPSPXY(I)*EPSPXY(I)
     2          + EPSPYZ(I)*EPSPYZ(I)+EPSPZX(I)*EPSPZX(I)))    
        IF (ISRATE == 0) THEN 
          EDOT(I) = EPSP
        ELSE
          EDOT(I) = ASRATE*EPSP + (UN - ASRATE)*UVAR(I,4)
          UVAR(I,4) = EDOT(I)
        ENDIF
c
c------UPDATE ELASTIC MODULUS
        RHO(I)=UVAR(I,3)*RHO0(I)/AREA(I)/THK(I)
        RELVOL(I)=RHO0(I)/RHO(I)
        ENEW(I)=(MAX(E,E1*EDOT(I)+E2))/(EXP(RELVEXP*LOG(RELVOL(I))))
        G2(I)=ENEW(I)/(UN+POISSON)
        BULK(I)=ENEW(I)/(UN-DEUX*POISSON)
        MK(I) = (BULK(I)+BULKT)/VBULK
        PK(I) = BULK(I)*BULKT/VBULK
        MG2(I) = (G2(I)+GT2)/VMU2
        PG2(I) =  G2(I)*GT2/VMU2
        GAMA(I) = (RELVOL(I)-UN+GAMA0)
        IF(UN+GAMA(I)-PHI.LE.SMALL) GAMA(I)=-(UN-PHI-SMALL)
        SSM=SIGOXX(I)+SIGOYY(I)
        SM(I)=THIRD*SSM
        EPSM =EPSXX(I)+EPSYY(I)
        VAR=DEUX*PG2(I)+C3*PK(I)
        EPSPZZ(I)=((G2(I)-C1*BULK(I))*EPSPM-(MG2(I)-C2*MK(I))*SSM
     1              +(PG2(I)-C3*PK(I))*EPSM-VAR*UVAR(I,2))
     2            /(G2(I)+C1*BULK(I)+G2(I)+VAR*TIMESTEP)
        DEZZ=EPSPZZ(I)*TIMESTEP
        EPSZZ(I)=UVAR(I,2)+DEZZ
        UVAR(I,2)=EPSZZ(I)
        THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        EM(I)=THIRD*(EPSM+EPSZZ(I))
        DEDTM(I)=THIRD*(EPSPM+EPSPZZ(I))
        SIGAIR(I)=MAX(ZERO,-(P0*GAMA(I))/(1+GAMA(I)-PHI))
  200 CONTINUE
c
      IF(ICORRECT.NE.0)THEN
       DO 210 I=1,NEL
C COMPUTE DEVIATORIC STRESSES
        DSXX(I)=SIGOXX(I)-SM(I)
        DSYY(I)=SIGOYY(I)-SM(I)
        DSXY(I)=SIGOXY(I)
 
C  COMPUTE DEVIATORIC STRAINS
        DEXX(I)=EPSXX(I)-EM(I)
        DEYY(I)=EPSYY(I)-EM(I)
        DEXY(I)=EPSXY(I)

C  COMPUTE DEVIATORIC STRAIN RATES
        DEDTXX(I)=EPSPXX(I)-DEDTM(I)
        DEDTYY(I)=EPSPYY(I)-DEDTM(I)
        DEDTXY(I)=EPSPXY(I)
  210  CONTINUE
      ELSE
       DO I=1,NEL
C COMPUTE DEVIATORIC STRESSES
        DSXX(I)=SIGOXX(I)-SM(I)
        DSYY(I)=SIGOYY(I)-SM(I)
        DSXY(I)=SIGOXY(I)
 
C  COMPUTE DEVIATORIC STRAINS
        DEXX(I)=EPSXX(I)-EM(I)
        DEYY(I)=EPSYY(I)-EM(I)
        DEXY(I)=EPSXY(I)*UNDEMI

C  COMPUTE DEVIATORIC STRAIN RATES
        DEDTXX(I)=EPSPXX(I)-DEDTM(I)
        DEDTYY(I)=EPSPYY(I)-DEDTM(I)
        DEDTXY(I)=EPSPXY(I)*UNDEMI
       END DO
      END IF


C COMPUTE STRESS RATE INCREMENTS
      DO 250 I=1,NEL
        DSDTXX(I)=G2(I)*DEDTXX(I)-MG2(I)*DSXX(I)+PG2(I)*DEXX(I)
        DSDTYY(I)=G2(I)*DEDTYY(I)-MG2(I)*DSYY(I)+PG2(I)*DEYY(I)
        DSDTXY(I)=G2(I)*DEDTXY(I)-MG2(I)*DSXY(I)+PG2(I)*DEXY(I)
        MIDSTEP=UN/(UN+MG2(I)*DT05)
        DSDTXX(I)=DSDTXX(I)*MIDSTEP
        DSDTYY(I)=DSDTYY(I)*MIDSTEP
        DSDTXY(I)=DSDTXY(I)*MIDSTEP
 250  CONTINUE


      DO 260 I=1,NEL
          SM(I)=SM(I)+UVAR(I,1)
        IF(KF.NE.0) THEN
          AMU(I)=RHO(I)/RHO0(I)-UN
          IF(IFLAG == 0) THEN
            P(I)=-FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU)
          ELSE
            PDOT(I)=( C1*BULK(I)*DEDTM(I)
     &               -C2*MK(I)*SM(I)
     &               +C3*PK(I)*EM(I))
     &             /(UN+C2*DT05*MK(I))
            P(I)=SM(I)+PDOT(I)*TIMESTEP
     &           -FAC*FINTER(KF,AMU(I),NPF,TF,DPDMU)
          ENDIF
        ELSE
            PDOT(I)=( C1*BULK(I)*DEDTM(I)
     &               -C2*MK(I)*SM(I)
     &               +C3*PK(I)*EM(I))
     &             /(UN+C2*DT05*MK(I))
            P(I)=SM(I)+PDOT(I)*TIMESTEP
        ENDIF
        IF(P(I).LE.PMIN) P(I)=PMIN
        P(I)=P(I)-SIGAIR(I)
  260 CONTINUE

C COMPUTE SOUND SPEED 
      DO 270 I=1,NEL
        DPDRO(I)= G2(I)/(UN-POISSON)
     &          +P0*(UN-PHI)/(UN+GAMA(I)-PHI)**2
  270 CONTINUE

      DO 280 I=1,NEL
        SOUNDSP(I)=SQRT(DPDRO(I)/RHO(I))
  280 CONTINUE
C COMPUTE PRESSURE

C COMPUTE UPDATED STRESSES
      IF(ICORRECT.NE.0)THEN
       DO 290 I=1,NEL
        SIGNXX(I)=DSXX(I)+DSDTXX(I)*TIMESTEP+P(I)
        SIGNYY(I)=DSYY(I)+DSDTYY(I)*TIMESTEP+P(I)
        SIGNXY(I)=DSXY(I)+DSDTXY(I)*TIMESTEP*0.5
        SIGNYZ(I)=SIGOYZ(I)+G02*DEPSYZ(I)*CSHEAR
        SIGNZX(I)=SIGOZX(I)+G02*DEPSZX(I)*CSHEAR
        VISCMAX(I)= ZERO
        UVAR(I,1)=SIGAIR(I)
  290  CONTINUE
      ELSE
       DO I=1,NEL
        SIGNXX(I)=DSXX(I)+DSDTXX(I)*TIMESTEP+P(I)
        SIGNYY(I)=DSYY(I)+DSDTYY(I)*TIMESTEP+P(I)
        SIGNXY(I)=DSXY(I)+DSDTXY(I)*TIMESTEP
        SIGNYZ(I)=SIGOYZ(I)+G02*DEPSYZ(I)*CSHEAR
        SIGNZX(I)=SIGOZX(I)+G02*DEPSZX(I)*CSHEAR
        VISCMAX(I)= ZERO
        UVAR(I,1)=SIGAIR(I)
       END DO
      END IF
c-----------        
      RETURN
      END

