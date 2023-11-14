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
Chd|  SIGEPS92                      source/materials/mat/mat092/sigeps92.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
       SUBROUTINE SIGEPS92(
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
     B      IHET   ,OFFG    , EPSTH3  , IEXPAN, WW)
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
#include "impl1_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,IEXPAN
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
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),OFFG(NEL),
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
     .      UVAR(NEL,NUVAR), OFF(NEL) ,WW(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,MULLINS,II

      my_real
     .   MU,D,LAM,G,RBULK,AA,BB,CC,P,TRACE(MVSIZ),C(5),BETA,
     .   T1(MVSIZ), T2(MVSIZ),T3(MVSIZ),AV(MVSIZ,6),EV(MVSIZ,3),
     .   EVV(MVSIZ,3),RV(MVSIZ),RVD,DI1LAM1(MVSIZ),DI1LAM2(MVSIZ),DI1LAM3(MVSIZ),
     .   DIRPRV(MVSIZ,3,3),EVD(3),ETI(MVSIZ)  
      my_real
     .   CLAM(5),LAM_2(3),LAM_4(3),AMAX,CLP  
      my_real ,DIMENSION(NEL,3) :: EVM,CII
      my_real ,DIMENSION(NEL)   :: GTMAX,RKMAX,RV_1  
C----------------------------------------------------------------
C material parameters
        MU       = UPARAM(1)
        D        = UPARAM(2)
        LAM      = UPARAM(3)
        G        = UPARAM(4)
        RBULK    = UPARAM(5)
        C(1)     = UPARAM(6) 
        C(2)     = UPARAM(7)
        C(3)     = UPARAM(8)
        C(4)     = UPARAM(9)
        C(5)     = UPARAM(10)   
        MULLINS  = INT(UPARAM(13)) ! computed in updmat 
C iniialisation des variabesl users 
      IF(TIME==ZERO)THEN
       DO J = 1,NUVAR
        DO  I = 1, NEL
         UVAR(I,J) = ZERO
        ENDDO
       ENDDO
      ENDIF   
C           
      DO I=1,NEL
       AV(I,1)=EPSXX(I)
       AV(I,2)=EPSYY(I)
       AV(I,3)=EPSZZ(I)
       AV(I,4)=EPSXY(I)/2
       AV(I,5)=EPSYZ(I)/2
       AV(I,6)=EPSZX(I)/2
      ENDDO       
CEigenvalues needed to be calculated in double precision
C        for a simple precision executing
      IF (IRESP==1) THEN
        CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
      ELSE
       CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
      ENDIF
C-ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
C-ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
C-ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
C-ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT
      IF(ISMSTR==0.OR.ISMSTR==2.OR.ISMSTR==4) THEN
        DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
         EV(I,1)=EXP(EVV(I,1))
         EV(I,2)=EXP(EVV(I,2))
         EV(I,3)=EXP(EVV(I,3))
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
      ELSE
C ----  STRAIN IS ENGINEERING)
        DO I=1,NEL
         EV(I,1)=EVV(I,1)+ ONE
         EV(I,2)=EVV(I,2)+ ONE
         EV(I,3)=EVV(I,3)+ ONE
        ENDDO 
      ENDIF

      IF (IMPL_S > 0 .OR. IHET > 1) THEN
       ETI(1:NEL)=ZERO
       DO J= 1,5
         BB = ONE/LAM**(2*J - 2)
         AA = J*(J-1)*C(J)
         DO I=1,NEL
          TRACE(I) = EV(I,1)*EV(I,1)+ EV(I,2)*EV(I,2) + EV(I,3)*EV(I,3)
          CC = AA*BB*TRACE(I)**(J-2)
          ETI(I)  = ETI(I) + CC
         ENDDO
       ENDDO
       DO I=1,NEL
        ETI(I) = MU*ETI(I)
        ET(I)= ETI(I)
        ET(I)= MAX(ONE,ET(I)/G)
       ENDDO
      ENDIF
C----------------
C 
C
      DO I = 1,NEL
C----  RV = RHO0/RHO = RELATIVE VOLUME = DET A (A = GRADIENT OF DEFORMATION)
         RV(I) = EV(I,1)*EV(I,2)*EV(I,3)
      ENDDO
C----THERM STRESS COMPUTATION-----
      IF(IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) THEN
        DO I=1,NEL
         RV(I) = RV(I) -EPSTH3(I)
        ENDDO
      ENDIF
C---------------- 


      DO I = 1,NEL
!         RVD   = RV**(-THIRD)
         ! if rv > 0 --> rvd = exp(-third * ln(rv))
         ! else      --> rvd = 0
         IF(RV(I)>ZERO) THEN
          RVD = EXP((-THIRD)*LOG(RV(I)))
         ELSE
          RVD = ZERO
         ENDIF
         EVD(1) = EV(I,1)*RVD
         EVD(2) = EV(I,2)*RVD
         EVD(3) = EV(I,3)*RVD
         EVM(I,1:3) = EVD(1:3)
C         
         TRACE(I) = EVD(1)**2 + EVD(2)**2 + EVD(3)**2
         DI1LAM1(I) = TWO*EVD(1)**2 - TWO_THIRD*TRACE(I)
         DI1LAM2(I) = TWO*EVD(2)**2 - TWO_THIRD*TRACE(I)
         DI1LAM3(I) = TWO*EVD(3)**2 - TWO_THIRD*TRACE(I)       
         T1(I) = ZERO
         T2(I) = ZERO
         T3(I) = ZERO
      ENDDO
      DO J=1,5
          BB = ONE/LAM**(2*J - 2)
          AA = J*C(J)
            CLAM(J) = BB*C(J)
          DO I=1,NEL
              CC = AA*BB*TRACE(I)**(J-1)
              T1(I) = T1(I) + CC*DI1LAM1(I)
              T2(I) = T2(I) + CC*DI1LAM2(I)
              T3(I) = T3(I) + CC*DI1LAM3(I)
          ENDDO
      ENDDO
C RBilk = 2/D
      DO I=1,NEL
         RV_1(I) = ONE/RV(I)    
         P = RV_1(I)*RBULK*(RV(I)**2 - ONE)/TWO
         T1(I) = (MU*T1(I) + P)*RV_1(I)
         T2(I) = (MU*T2(I) + P)*RV_1(I)
         T3(I) = (MU*T3(I) + P)*RV_1(I)
      ENDDO
c------------------------
      IF(MULLINS == 1)THEN
       DO I = 1,NEL
         BETA  = ONE/LAM**2
         WW(I) = MU *(C(1)*(TRACE(I)- THREE)+C(2) * BETA    *(TRACE(I)**2- NINE)
     .   +                                   C(3) * BETA**2 *(TRACE(I)**3- THREE**3)    
     .   +                                   C(4) * BETA**3 *(TRACE(I)**4- THREE**4)
     .   +                                   C(5) * BETA**4 *(TRACE(I)**5- THREE**5) )

       ENDDO
      ENDIF
c----compute GT_MAX
        GTMAX(1:NEL) = G
        RKMAX(1:NEL) = RBULK/TWO
          CII(1:NEL,1:3) = ZERO
          DO II = 1,5
            CLP = FOUR*II*CLAM(II)
            DO I=1,NEL
             LAM_2(1:3) = EVM(I,1:3)**2
             LAM_4(1:3) = LAM_2(1:3)**2
             AA = ONE_OVER_9*II*TRACE(I)**II
           BB = ZERO
           CC = ZERO
           IF (II>1) BB =THIRD*(3-II)*TRACE(I)**(II-1)
           IF (II>2) CC =(II-1)*TRACE(I)**(II-2)
             CII(I,1:3) = CII(I,1:3) +CLP*(AA+BB*LAM_2(1:3) +CC*LAM_4(1:3))
            ENDDO
          ENDDO
          DO I=1,NEL
           AMAX= MAX(CII(I,1),CII(I,2),CII(I,3))
C---------reduce old result change       
           ETI(I) = MAX(ONE,AMAX*0.81) 
           GTMAX(I) = G*ETI(I)
           RKMAX(I) = RKMAX(I)*(ONE+RV_1(I)*RV_1(I))
           RKMAX(I) = MAX(RBULK,RKMAX(I))
C----  
           ET(I)= MAX(ONE,AMAX)
          ENDDO
C
C cauchy to glabale
      DO I=1,NEL
        SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*T1(I)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*T2(I)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*T3(I)
     
        SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*T2(I)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*T3(I)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*T1(I)
     
        SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*T3(I)        
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*T1(I)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*T2(I)
     
        SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*T1(I)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*T2(I)     
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*T3(I)
     
        SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*T2(I)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*T3(I)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*T1(I)
     
        SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*T3(I)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*T1(I)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*T2(I)
C
* SET SOUND SPEED
         SOUNDSP(I)=SQRT((FOUR_OVER_3*GTMAX(I) + RKMAX(I))/RHO(I))
* SET VISCOSITY
         VISCMAX(I) = ZERO
       ENDDO
C   
      RETURN
      END

