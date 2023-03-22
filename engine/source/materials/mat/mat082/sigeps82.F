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
Chd|  SIGEPS82                      source/materials/mat/mat082/sigeps82.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
       SUBROUTINE SIGEPS82(
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
     B      MFXX   , MFXY   , MFXZ    , MFYX  , MFYY  , MFYZ  , 
     C      MFZX   , MFZY   , MFZZ    , IHET  ,OFFG   ,EPSTH3 ,
     D      IEXPAN  )
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
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      MFXX(NEL)  ,   MFXY(NEL),   MFXZ(NEL),
     .      MFYX(NEL)  ,   MFYY(NEL),   MFYZ(NEL),
     .      MFZX(NEL)  ,   MFZY(NEL),   MFZZ(NEL),OFFG(NEL),
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
      INTEGER    I,J,K,II,NORDRE,NMAXW,JJ
      my_real
     .   MU(100),AL(100),S(MVSIZ,3),DD,PP,GMAX,
     .   D(MVSIZ),AV(MVSIZ,6),RBULK,EV(MVSIZ,3),
     .   EVD(MVSIZ,3),RV(MVSIZ),EVV(MVSIZ,3),
     .   DIRPRV(MVSIZ,3,3),PUI,PUI_TAB(3),LAM_AL(3)
      my_real
     .    A(3,3),DPRA(3,3),EIGEN(3) ,AMAX ,ETI,P,EFAC
      my_real ,DIMENSION(NEL,3) :: CII
      my_real ,DIMENSION(NEL)   :: GTMAX,RKMAX  
C----------------------------------------------------------------

C SET INITIAL MATERIAL CONSTANTS
      GMAX = ZERO
cc      NU      = UPARAM(1)
      NORDRE  = NINT(UPARAM(1))
      DO I= 1,NORDRE
       MU(I) = UPARAM(1 + I)
       AL(I) = UPARAM(1 + NORDRE + I)
       D(I)  = UPARAM(1 + 2*NORDRE + I)
       GMAX  = GMAX  + MU(I)
       
      ENDDO

cc      RBULK   = TWO/D(1) + TWO*GMAX*THIRD
      RBULK   = TWO/D(1)
C iniialisation des variabesl users 
      IF(TIME==ZERO)THEN
       DO J = 1,NUVAR
        DO  I = 1, NEL
         UVAR(I,J) = ZERO
        ENDDO
       ENDDO
      ENDIF   
C---------------------------------
c       ENDIF                      
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
       DO J =1,3
       DO I=1,NEL
         AMAX = EV(I,J)
         ETI=ZERO
        IF(AMAX/=ZERO) THEN
         DO K= 1,NORDRE
          ETI  = ETI + MU(K)*AMAX**(AL(K)-ONE) !MU(K)*EXP((AL(K)-ONE)*LOG(AMAX))
         ENDDO
        ENDIF         
        ET(I)= MAX(ETI,ET(I))
       ENDDO
       ENDDO
C       EFAC=TWOP5
       DO I=1,NEL
         ET(I)= MAX(ONE,ET(I)/GMAX)
       ENDDO
      ENDIF
C----------------
C deviatorique strecth
C
      DO I=1,NEL
C----  RV = RHO0/RHO = RELATIVE VOLUME = DET A (A = GRADIENT OF DEFORMATION)
         RV(I) = EV(I,1)*EV(I,2)*EV(I,3)
      ENDDO
C----THERM STRESS COMPUTATION-----Done before
      IF(IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) THEN
        DO I=1,NEL
          RV(I) = RV(I) -EPSTH3(I)
        ENDDO
      ENDIF
C----------------
      DO I=1,NEL
         IF(RV(I)/=ZERO) THEN
          PUI = RV(I)**(-THIRD)!EXP((-THIRD)*LOG(RV(I)))
         ELSE
          PUI = ZERO
         ENDIF
         EVD(I,1) = EV(I,1)*PUI
         EVD(I,2) = EV(I,2)*PUI
         EVD(I,3) = EV(I,3)*PUI
      ENDDO
      DO I=1,NEL
        S(I,1) = ZERO
        S(I,2) = ZERO
        S(I,3) = ZERO
        P = ZERO
        DO J=1,NORDRE
           DD = TWO*MU(J)/AL(J)
           DD = DD /RV(I)
           PP = ZERO
           IF(AL(J)/=ZERO) THEN
            DO II=1,3
             IF(EVD(I,II)/=ZERO) THEN
              PUI_TAB(II) = EVD(I,II)**AL(J)!EXP(AL(J)*LOG(EVD(I,II)))
             ELSE
              PUI_TAB(II) = ZERO
             ENDIF
            ENDDO
           ELSE
            DO II=1,3
             IF(EVD(I,II)/=ZERO) THEN 
              PUI_TAB(II) = ONE
             ELSE
              PUI_TAB(II) = ZERO
             ENDIF
            ENDDO
           ENDIF         


           IF(D(J) /= ZERO)PP = ONE/D(J)
           S(I,1) =  S(I,1) + DD*(TWO_THIRD*PUI_TAB(1) 
     .                      - THIRD*(PUI_TAB(2) + PUI_TAB(3)))
C
           S(I,2) =  S(I,2) + DD*(TWO_THIRD*PUI_TAB(2) 
     .                      - THIRD*(PUI_TAB(1) + PUI_TAB(3)))
C  
           S(I,3) =  S(I,3) + DD*(TWO_THIRD*PUI_TAB(3) 
     .                      - THIRD*(PUI_TAB(2) + PUI_TAB(1)))
C  (-) pression
            P = P + 2*J*PP*(RV(I) - ONE)**(2*J - 1)
        ENDDO 

        S(I,1) = S(I,1) + P
        S(I,2) = S(I,2) + P
        S(I,3) = S(I,3) + P           
      ENDDO          
C
C---------tagent G, RBULK:
       GTMAX(1:NEL) = GMAX 
       RKMAX(1:NEL) = RBULK 
C------max of Cii :   
         CII(1:NEL,1:3) = ZERO
         DO II = 1,NORDRE
           IF(MU(II)/=ZERO) THEN
              DO I=1,NEL
               LAM_AL(1:3) = EXP(AL(II)*LOG(EVD(I,1:3)))
               AMAX = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
               CII(I,1:3) = CII(I,1:3) + MU(II) * (LAM_AL(1:3)+AMAX)
              ENDDO
           ENDIF
         ENDDO
         DO II = 2,NORDRE
		   IF (D(II)==ZERO) CYCLE
		   PP = 2*II*(2*II-1)/D(II)
		   JJ = 2*II - 2
           DO I=1,NEL
		    IF (ABS(RV(I)- ONE)<EM20) CYCLE
            RKMAX(I) = RKMAX(I) + PP * (RV(I)- ONE)**JJ
           ENDDO
         ENDDO
C------GT=3/4 Cii (factor 1/2*1/3 already inside CII)		  
         DO I=1,NEL
           AMAX= HALF*MAX(CII(I,1),CII(I,2),CII(I,3))/GMAX
C---------reduce old result change		   
           ETI = MAX(ONE,AMAX*0.81) 
           GTMAX(I) = GMAX*ETI 
           RKMAX(I) = MAX(RBULK,RKMAX(I))
C---- 		   
           ET(I)= MAX(ONE,AMAX)
         ENDDO
C cauchy to glabale
      DO I=1,NEL
        SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*S(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*S(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*S(I,3)
     
        SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*S(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*S(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*S(I,1)
     
        SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*S(I,3)        
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*S(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*S(I,2)
     
        SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*S(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*S(I,2)     
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*S(I,3)
     
        SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*S(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*S(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*S(I,1)
     
        SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*S(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*S(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*S(I,2)
C
* SET SOUND SPEED
         SOUNDSP(I)=SQRT((FOUR_OVER_3*GTMAX(I) + RKMAX(I))/RHO(I))
* SET VISCOSITY
         VISCMAX(I) = ZERO
       ENDDO
C   
      RETURN
      END

