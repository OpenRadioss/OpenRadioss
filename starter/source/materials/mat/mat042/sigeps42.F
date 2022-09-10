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
Chd|  SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVEC                       source/materials/tools/matrix.F
Chd|        VALPVECDP                     source/materials/tools/matrix.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
           SUBROUTINE SIGEPS42(

     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , WXXDT , WYYDT ,
     B      WZZDT  , ISMSTR , MFXX    , MFXY  , MFXZ  , MFYX  ,    
     C      MFYY   , MFYZ   , MFZX    , MFZY  , MFZZ  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR,ISMSTR
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
     .      WZZDT(MVSIZ),WYYDT(MVSIZ),WXXDT(MVSIZ)
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
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER          I,J,K,IFLAG,ICOMP,KFP,KFJ,IJACOBI,NROT
      my_real
     .            MU1,MU2,MU3,MU4,MU5
      my_real
     .            AL1,AL2,AL3,AL4,AL5
      my_real
     .            TENSIONCUT,GMAX,FFAC
      my_real
     .            SIGPRV(3,MVSIZ)
      my_real
     .            EV(3,MVSIZ),EVM(3,MVSIZ),DWDL(3,MVSIZ)
      my_real
     .            T(3,MVSIZ),T1(3,MVSIZ),SUMDWDL(MVSIZ)
      my_real
     .            RV(MVSIZ),P(MVSIZ),DWDRV(MVSIZ),RBULK,DPDMU
      my_real
     .            AV(6,MVSIZ),EVV(3,MVSIZ),DIRPRV(3,3,MVSIZ)
      my_real
     .            A(3,3),DPRA(3,3),EIGEN(3)       
C----------------------------------------------------------------
C SET INITIAL MATERIAL CONSTANTS

      MU1=UPARAM(1)
      MU2=UPARAM(2)
      MU3=UPARAM(3)
      MU4=UPARAM(4)
      MU5=UPARAM(5)
      AL1=UPARAM(6)
      AL2=UPARAM(7)
      AL3=UPARAM(8)
      AL4=UPARAM(9)
      AL5=UPARAM(10)
      RBULK=UPARAM(11)
      TENSIONCUT=UPARAM(12)
      IFLAG=UPARAM(13)
      ICOMP=UPARAM(14)
      FFAC =UPARAM(15)
      KFP=IFUNC(1)
      KFJ=IFUNC(2)
      GMAX=MU1*AL1+MU2*AL2+MU3*AL3+MU4*AL4+MU5*AL5
C =2.* GROUND STATE SHEAR MODULE  (2MU)
C
C     ([F]=[M_F]+[1]) -----
c      IF(ISMSTR.EQ.10) THEN
cC--------- [B]=[F][F]^t strain-----
c      DO I=1,NEL
c       EPSXX(I)=MFXX(I)*(TWO+MFXX(I))+
c     .          MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
c       EPSYY(I)=MFYY(I)*(TWO+MFYY(I))+
c     .          MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
c       EPSZZ(I)=MFZZ(I)*(TWO+MFZZ(I))+
c     .          MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
c       EPSXY(I)=TWO*(MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+
c     .               MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I))
c       EPSZX(I)=TWO*(MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+
c     .               MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I))
c       EPSYZ(I)=TWO*(MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+
c     .               MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I))
c      ENDDO
c      ENDIF
C
      DO I=1,NEL
        AV(1,I)=EPSXX(I)
        AV(2,I)=EPSYY(I)
        AV(3,I)=EPSZZ(I)
        AV(4,I)=EPSXY(I) * HALF
        AV(5,I)=EPSYZ(I) * HALF
        AV(6,I)=EPSZX(I) * HALF
      ENDDO
C         Eigenvalues needed to be calculated in double precision
C         for a simple precision executing
      IF (IRESP.EQ.1) THEN
        CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC(AV,EVV,DIRPRV,NEL)
      ENDIF
*ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
*ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
*ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
*ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT

      DO I=1,NEL
        IF(ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
* ISMSTR = 0 OR 3 (STRAIN IS LOGARITHMIC)
          EV(1,I)=EXP(EVV(1,I))
          EV(2,I)=EXP(EVV(2,I))
          EV(3,I)=EXP(EVV(3,I))
        ELSEIF(ISMSTR.EQ.10.OR.ISMSTR.EQ.12) THEN
          EV(1,I)=SQRT(EVV(1,I)+ ONE)
          EV(2,I)=SQRT(EVV(2,I)+ ONE)
          EV(3,I)=SQRT(EVV(3,I)+ ONE)
        ELSE
* ISMSTR = 1 OR 2 ( STRAIN IS ENGINEERING)
          EV(1,I)=EVV(1,I)+ ONE
          EV(2,I)=EVV(2,I)+ ONE
          EV(3,I)=EVV(3,I)+ ONE  
        ENDIF
      ENDDO

      DO I=1,NEL
* RV = RHO0/RHO = RELATIVE VOLUME = DET A (A = GRADIENT OF DEFORMATION)
        RV(I)=(EV(1,I)*EV(2,I)*EV(3,I))
      ENDDO

      DO I=1,NEL
* COMPUTE MODIFIED (NORMALIZED) STRETCHES - THIS UNIFIES A COMPRESSIBLE AND 
* INCOMPRESIBLE FORMULATIONS
        EVM(1,I)=EV(1,I)*RV(I)**(-THIRD)
        EVM(2,I)=EV(2,I)*RV(I)**(-THIRD)
        EVM(3,I)=EV(3,I)*RV(I)**(-THIRD)
        IF(KFP.NE.0) THEN
* READ BULK MODULE FROM FUNCTION
          P(I)=RBULK*FFAC*FINTER(KFP,RV(I),NPF,TF,DPDMU)
        ELSE
* BULK MODULE IS CONSTANT
          P(I)=RBULK
        ENDIF
      ENDDO

* STRAIN ENERGY = W(L1,L2,L3)=SUM_OF_1_TO_P (MUP(L1**ALP+L2**ALP+L3**ALP-3-
* ALPLN(L1*L2*L3))/ALP+.5*P(I)(J-1)**2

      DO I=1,NEL
        DO J=1,3
* L(J).DW/DL(J)
          DWDL(J,I)=(MU1*(EVM(J,I)**AL1-1)+MU2*(EVM(J,I)**AL2-1)+
     &MU3*(EVM(J,I)**AL3-1)+MU4*(EVM(J,I)**AL4-1)+MU5*(EVM(J,I)**AL5-1))
        ENDDO
      ENDDO

      DO I=1,NEL
* DW/RV
        DWDRV(I)=P(I)*(RV(I)-1.)
* (SUM(L(J)DW/DL(J))/3
        SUMDWDL(I)=(DWDL(1,I)+DWDL(2,I)+DWDL(3,I))* THIRD
      ENDDO

      DO I=1,NEL
* RV.T(J)=L(J).DW/DL(J)-(SUM_OF_1_TO_3(D(W)/D(LJ))-RV.DWDRV)
        DO J=1,3
            T(J,I)=(DWDL(J,I)-(SUMDWDL(I)-RV(I)*DWDRV(I)))/RV(I)
        ENDDO
      ENDDO

      DO I=1,NEL
* TRANSFORM FROM CAUCHY TO BIOT STRESS
          T1(1,I)=RV(I)*T(1,I)/EV(1,I)
          T1(2,I)=RV(I)*T(2,I)/EV(2,I)
          T1(3,I)=RV(I)*T(3,I)/EV(3,I)
      ENDDO

      DO I=1,NEL
          SIGPRV(1,I)=T(1,I)
          SIGPRV(2,I)=T(2,I)
          SIGPRV(3,I)=T(3,I)
      ENDDO

* TENSION CUT
      DO I=1,NEL
          DO J=1,3
          IF(OFF(I).EQ.ZERO.OR.T1(J,I).GT.ABS(TENSIONCUT))THEN
            SIGPRV(1,I)=ZERO
            SIGPRV(2,I)=ZERO
            SIGPRV(3,I)=ZERO
C           IF(OFF(I).NE.ZERO) IDEL7NOK = 1
            OFF(I)=ZERO
          ENDIF
          ENDDO
      ENDDO


* TRANSFORM PRINCIPAL CAUCHY STRESSES TO GLOBAL DIRECTIONS
      DO I=1,NEL
* STORE VALUES FOR POST-PROCESSING
        UVAR(I,1)=MAX(SIGPRV(1,I),SIGPRV(2,I),SIGPRV(3,I))
        UVAR(I,2)=MIN(SIGPRV(1,I),SIGPRV(2,I),SIGPRV(3,I))
        UVAR(I,3)=OFF(I)
        SIGNXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
        SIGNYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
        SIGNZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
        SIGNXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
        SIGNYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
        SIGNZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
* SET SOUND SPEED
        SOUNDSP(I)=SQRT((TWO_THIRD*GMAX+P(I))/RHO(I))
* SET VISCOSITY
        VISCMAX(I)=ZERO
      ENDDO
      RETURN
      END

