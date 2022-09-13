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
Chd|  SIGEPS62                      source/materials/mat/mat062/sigeps62.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
       SUBROUTINE SIGEPS62(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ET    ,
     B      IHET   ,OFFG    , EPSTH3  , IEXPAN)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR,ISMSTR,IHET,IEXPAN

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
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL), OFFG(NEL),
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
      INTEGER    I,J,K,II,NORDRE,NPRONY,ITYPE,IVISC
      my_real
     .   MU(100),AL(100),TAUX(100), GAMA(100),GAMAINF,NU,
     .   GMAX,S(MVSIZ,3),SD(MVSIZ,3),
     .   EC(MVSIZ,3),HD(MVSIZ,3,100),
     .   EV(MVSIZ,3),AV(MVSIZ,6),DIRPRV(MVSIZ,3,3),
     .   RV(MVSIZ),P(MVSIZ),RBULK,
     .   EVV(MVSIZ,3),SSP(MVSIZ,3),FAC, BETA,
     .   DTINV,VISC,FAC1,
     .   E1,E2,E3,E4,E5,E6,EPSP, VM,DAV,VISC1,SIGXX(MVSIZ),
     .   SIGYY(MVSIZ),SIGZZ(MVSIZ),SIGXY(MVSIZ),SIGZX(MVSIZ),
     .   SIGYZ(MVSIZ),DVISC,A11,A12,A13,A22,A21,A23,
     .   A31,A32,A33,SDG0(MVSIZ,6),SDG(MVSIZ,6),HG(MVSIZ,6),HG0(6),H(MVSIZ,3,100)
     
      my_real
     .   A(3,3),DPRA(3,3),EIGEN(3),AMAX,ETI(MVSIZ),EFAC,RV_PUI,RV_PUI_TAB(MVSIZ),
     .   CJ(MVSIZ),LAM_AL(3),RVL,LAMIN,CIMAX(MVSIZ),GVIS,CMAX,CMAX0,
     .   D11,D12,D13,D21,D22,D23,D31,D32,D33,MU2
      my_real ,DIMENSION(NEL,3) :: AI,BI
C----------------------------------------------------------------

C SET INITIAL MATERIAL CONSTANTS
      GMAX = ZERO
      NU      = UPARAM(1)
      NORDRE  = NINT(UPARAM(2))
      NPRONY   = NINT(UPARAM(3))
      GAMAINF = UPARAM(4)
      RBULK   = UPARAM(5)
      DVISC   = UPARAM(6)
      BETA = NU/(ONE - TWO*NU)
      DO I= 1,NORDRE
       MU(I) = UPARAM(6 + I)
       AL(I) = UPARAM(6 + NORDRE + I)
C       GMAX = GMAX + AL(I)*MU(I)
       GMAX = GMAX + MU(I)
      ENDDO
!!      IVISC = 0
      IVISC =  UPARAM(2*NORDRE + 2*NPRONY + 7 )
      IF(NPRONY /= ZERO)THEN
        DO I=1,NPRONY 
          GAMA(I)  = UPARAM(6 + 2*NORDRE + I)
          TAUX(I)  = UPARAM(6 + 2*NORDRE + NPRONY + I)
        ENDDO 
      ENDIF 
!!      IF(IVISC == 1 .AND.  ITYPE >= 2 ) IVISC = 2 
Csm +1
       GMAX = TWO*GMAX      
C iniialisation des variabesl users 
      IF(TIME==ZERO)THEN
       DO J = 1,NUVAR
        DO  I = 1, NEL
         UVAR(I,J) = ZERO
        ENDDO
       ENDDO
      ENDIF                         
      DO I=1,NEL
       AV(I,1)=EPSXX(I)
       AV(I,2)=EPSYY(I)
       AV(I,3)=EPSZZ(I)
       AV(I,4)=EPSXY(I)/2
       AV(I,5)=EPSYZ(I)/2
       AV(I,6)=EPSZX(I)/2
      ENDDO       
CEigenvalues needed to be calcultated in double precision
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
C----------------
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
       DO J =1,3
         ETI(1:NEL)=ZERO
         DO K= 1,NORDRE
!         ETI  = ETI + MU(K)*AMAX**(AL(K)-ONE)
          DO I=1,NEL
	   AMAX = EV(I,J)
           IF(AMAX>ZERO) THEN
            IF(MU(K)/=ZERO) THEN
             IF((AL(K)-ONE)/=ZERO) THEN
              ETI(I)  = ETI(I) + MU(K)*EXP((AL(K)-ONE)*LOG(AMAX))
             ELSE
              ETI(I)  = ETI(I) + MU(K)
             ENDIF
            ELSE
             ETI(I) = ETI(I) + ZERO
            ENDIF
           ELSE
            ETI(I) = ZERO
           ENDIF
          ENDDO ! NEL
        ENDDO   ! NORDRE
        ET(1:NEL)= MAX(ETI(1:NEL),ET(1:NEL))
       ENDDO    ! 1,3
       EFAC=TWO
       DO I=1,NEL
         ET(I)= MAX(ONE,EFAC*ET(I)/GMAX)
       ENDDO
      ENDIF
C 
C----  RV = RHO0/RHO = RELATIVE VOLUME = DET A (A = GRADIENT OF DEFORMATION)
       RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)         
       EC(1:NEL,1:3) = EV(1:NEL,1:3)**2 
C----  RV = RHO0/RHO = RELATIVE VOLUME = DET A (A = GRADIENT OF DEFORMATION)            
C----THERM STRESS COMPUTATION-----
      IF(IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) RV(1:NEL)= RV(1:NEL)-EPSTH3(1:NEL)
C
C calcul de la pression
      P(1:NEL) = ZERO     
      DO II = 1,NORDRE
        FAC = TWO*MU(II)/(AL(II))
        IF(FAC/=ZERO) THEN
         IF(AL(II)/=ZERO) THEN
          DO I=1,NEL
            FAC1 = FAC / RV(I)
            IF(EV(I,1)>ZERO) P(I) = P(I) + 
     .       FAC1*THIRD*( EXP(AL(II)*LOG(EV(I,1))) )
            IF(EV(I,2)>ZERO) P(I) = P(I) + 
     .       FAC1*THIRD*( EXP(AL(II)*LOG(EV(I,2))) )
            IF(EV(I,3)>ZERO) P(I) = P(I) + 
     .       FAC1*THIRD*( EXP(AL(II)*LOG(EV(I,3)))    )   
     
            IF(RV(I)>ZERO) P(I) = P(I) 
     .          - FAC1*EXP((-BETA*AL(II))*LOG(RV(I)))
          ENDDO
         ELSE
          DO I=1,NEL
            FAC1 = FAC / RV(I)
            IF(EV(I,1)/=ZERO) P(I) = P(I) + FAC1*THIRD
            IF(EV(I,2)/=ZERO) P(I) = P(I) + FAC1*THIRD
            IF(EV(I,3)/=ZERO) P(I) = P(I) + FAC1*THIRD     
            IF(RV(I)>ZERO) P(I) = P(I) - FAC1
         ENDDO
        ENDIF
        ELSE 
          P(1:NEL) = ZERO
        ENDIF
      ENDDO 
C compute de piola kirchoff stress 2emme espece---
      DO J=1,3
      S(1:NEL,J)= ZERO 
       DO II= 1, NORDRE
         FAC = TWO*MU(II)/(AL(II))
         IF(FAC/=ZERO) THEN
          DO I=1,NEL
          FAC1 = FAC/(EV(I,J)**2)
           IF(AL(II)/=ZERO) THEN
            IF(EV(I,J)>ZERO) S(I,J)= S(I,J) + FAC1*EXP(AL(II)*LOG(EV(I,J)))  
            IF(RV(I)>ZERO) S(I,J)= S(I,J) - FAC1*EXP((-BETA*AL(II))*LOG(RV(I)))
           ELSE
            IF(EV(I,J)==ZERO) THEN
             S(I,J)= S(I,J) - FAC1
            ENDIF
           ENDIF       
          ENDDO ! I=1,NEL 
         ENDIF ! Fac /= 0 
       ENDDO   ! II=1,Nordre
      ENDDO    ! J=1,3
CCC devaitoric piola kirchoff stress 2nd espece SD         
      DO I=1,NEL
       ! sd = rv**(dtier) * sd
       ! sd = pui * sd
       ! if rv > 0 --> pui = exp(dtier * ln(rv))
       ! else pui = 0
         IF(RV(I)>ZERO) THEN
            RV_PUI_TAB(I) =  EXP( TWO_THIRD*LOG(RV(I)) )
         ELSE
             RV_PUI_TAB(I) = ZERO
         ENDIF
       ENDDO
       DO J=1,3
        SSP(1:NEL,J) = P(1:NEL)/(EV(1:NEL,J)**2) 
        SD(1:NEL,J)  = S(1:NEL,J) - SSP(1:NEL,J)*RV(1:NEL) 
        SD(1:NEL,J)  = RV_PUI_TAB(1:NEL)*SD(1:NEL,J) !  viscosity is deviatoric
       ENDDO
C we consider spherical and deviatoric viscosity compared to ivisc =1      
       IF(IVISC == 2) SD(1:NEL,1:3) = S(1:NEL,1:3)

      GVIS = ZERO
C Calcul de H^i_n+1 
      IF(IVISC > 0)THEN
        GVIS = GMAX
c        GMAX = TWO*GMAX
        SDG0(1:NEL,1:6) = UVAR(1:NEL,1:6)
        DO I=1,NEL 
C 
C compute SD ---> glabal repere
C
           SDG(I,1)   =  DIRPRV(I,1,1)*DIRPRV(I,1,1)*SD(I,1)
     .               + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SD(I,2)
     .               + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SD(I,3)
          
           SDG(I,2)   = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SD(I,2)
     .               + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SD(I,3)
     .               + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SD(I,1)
                                                          
           SDG(I,3)    = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SD(I,3)     
     .               + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SD(I,1)
     .               + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SD(I,2)
          
           SDG(I,4)    = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SD(I,1)   
     .               + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SD(I,2)  
     .               + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SD(I,3)
          
           SDG(I,5)    = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SD(I,2)
     .               + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SD(I,3)
     .               + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SD(I,1)
          
           SDG(I,6)    = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SD(I,3)
     .               + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SD(I,1)
     .               + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SD(I,2)

        ENDDO
        DO J=1,6
          UVAR(1:NEL,J) = SDG(1:NEL,J)
        ENDDO
C                         
        DO II= 1,NPRONY 
          FAC= -TIMESTEP/TAUX(II) 
          DO J= 1,6
            DO I=1,NEL 
              HG0(J) = UVAR(I, 6 + (II - 1)*6 + J)
              HG(I,J) = EXP(FAC)*HG0(J) + EXP(HALF*FAC)*(SDG(I,J)-SDG0(I,J))
              UVAR(I, 6 + (II - 1)*6 + J)=  HG(I,J)
            ENDDO
          ENDDO
C 
C HGlobal --- H principal
C         
            DO I=1,NEL 
             A11 =    DIRPRV(I,1,1)*HG(I,1) + DIRPRV(I,2,1)*HG(I,4)
     .              + DIRPRV(I,3,1)*HG(I,6) 
             A12 =    DIRPRV(I,1,2)*HG(I,1) + DIRPRV(I,2,2)*HG(I,4)
     .              + DIRPRV(I,3,2)*HG(I,6)
             A13 =    DIRPRV(I,1,3)*HG(I,1) + DIRPRV(I,2,3)*HG(I,4)
     .              + DIRPRV(I,3,3)*HG(I,6)
     
!     
             A21 =    DIRPRV(I,1,1)*HG(I,4) + DIRPRV(I,2,1)*HG(I,2)
     .              + DIRPRV(I,3,1)*HG(I,5) 
             A22 =    DIRPRV(I,1,2)*HG(I,4) + DIRPRV(I,2,2)*HG(I,2)
     .              + DIRPRV(I,3,2)*HG(I,5)
             A23 =    DIRPRV(I,1,3)*HG(I,4) + DIRPRV(I,2,3)*HG(I,2)
     .              + DIRPRV(I,3,3)*HG(I,5)

             A31 =    DIRPRV(I,1,1)*HG(I,6) + DIRPRV(I,2,1)*HG(I,5)
     .              + DIRPRV(I,3,1)*HG(I,3) 
             A32 =    DIRPRV(I,1,2)*HG(I,6) + DIRPRV(I,2,2)*HG(I,5)
     .              + DIRPRV(I,3,2)*HG(I,3)
             A33 =    DIRPRV(I,1,3)*HG(I,6) + DIRPRV(I,2,3)*HG(I,5)
     .              + DIRPRV(I,3,3)*HG(I,3)
C H dans la direction H               
             H(I,1,II) = DIRPRV(I,1,1)*A11  + DIRPRV(I,2,1)*A21
     .                                      + DIRPRV(I,3,1)*A31 
             H(I,2,II) = DIRPRV(I,1,2)*A12  + DIRPRV(I,2,2)*A22
     .                                      + DIRPRV(I,3,2)*A32
             H(I,3,II) = DIRPRV(I,1,3)*A13  + DIRPRV(I,2,3)*A23
     .                                      + DIRPRV(I,3,3)*A33
           ENDDO
           DO J=1,3
            DO I=1,NEL 
C calcul de la partie deviatorique        
             FAC = THIRD*(H(I,1,II)*EC(I,1) + H(I,2,II)*EC(I,2) 
     .                                      + H(I,3,II)*EC(I,3))
              HD(I,J,II) = H(I,J,II) - FAC/MAX(EM20,EC(I,J))
            ENDDO
           ENDDO
        ENDDO            
      ENDIF  
C calcul du tenseur de piola kirchoff   
      IF(IVISC == 1) THEN ! viscosity is deviatoric
         DO I =1,NEL                                               
            ! FAC= GAMAINF*RV(I)**(-TWO_THIRD) = GAMAINF * PUI        
            ! if rv > 0 --> pui = exp(-dtier * ln(rv))             
            ! else pui = 0                                         
            IF(RV(I)>ZERO) THEN                                    
               RV_PUI_TAB(I) = EXP( (-TWO_THIRD)*LOG(RV(I)) )                 
            ELSE                                                   
               RV_PUI_TAB(I) = ZERO                                       
            ENDIF
         ENDDO                                                  
C                                 
         DO J=1,3
          DO I =1,NEL                                              
            FAC= GAMAINF*RV_PUI_TAB(I)     
            S(I,J) = SSP(I,J)*RV(I) + FAC*SD(I,J)  
          ENDDO             
          DO II=1,NPRONY 
           DO I =1,NEL                                          
             S(I,J) = S(I,J) + GAMA(II)*RV_PUI_TAB(I)*HD(I,J,II)  
           ENDDO       
          ENDDO 
C cauchy stress   from PK2 
          DO I =1,NEL     
           S(I,J) = (S(I,J)*EV(I,J)**2)/RV(I)                   
          ENDDO                                                    
        ENDDO  
      ELSEIF(IVISC == 2) THEN  ! spherical + deviatoric                
        DO J=1,3
          DO I =1,NEL
           S(I,J) = GAMAINF*S(I,J) 
          ENDDO                            
          DO II=1,NPRONY 
           DO I =1,NEL                                             
            S(I,J) = S(I,J) + GAMA(II)*H(I,J,II)  
           ENDDO              
          ENDDO  
C cauchy stress    from PK2
          DO I =1,NEL                                                     
           S(I,J) = (S(I,J)*EV(I,J)**2)/RV(I)                 
          ENDDO                                                    
        ENDDO
      ELSE
C cauchy stress    from PK2     
        DO J=1,3
          DO I =1,NEL                                            
              S(I,J) = S(I,J)*EV(I,J)**2/RV(I)                 
          ENDDO                                                    
        ENDDO
      ENDIF
C----- 
      CMAX0 = TWO_THIRD*GMAX+ RBULK
       AI(1:NEL,1:3) = ZERO
       BI(1:NEL,1:3) = ZERO
       CJ(1:NEL) = ZERO
       DO II = 1,NORDRE
        IF(AL(II)/=ZERO) THEN
         FAC = ONE/AL(II)
         FAC1 = BETA+FAC
         MU2 = TWO*MU(II)
           DO I=1,NEL
            LAM_AL(1:3) = EXP(AL(II)*LOG(EV(I,1:3)))
            RVL = FAC1*EXP(-BETA*AL(II)*LOG(RV(I)))
            CJ(I) = CJ(I) + MU2*RVL
            AI(I,1:3) = AI(I,1:3) + MU2*LAM_AL(1:3)
            BI(I,1:3) = BI(I,1:3) + FAC*MU2*LAM_AL(1:3)
           ENDDO
        ENDIF
       ENDDO
C------max of Cii       
       DO I=1,NEL
         D11 = AI(I,1)-BI(I,1)+CJ(I)
         D22 = AI(I,2)-BI(I,2)+CJ(I)
         D33 = AI(I,3)-BI(I,3)+CJ(I)
C     
         FAC = ONE/RV(I)
         CMAX = FAC*MAX(D11,D22,D33)
         CIMAX(I) = TWO_THIRD*GVIS + MAX(CMAX,CMAX0)
         ET(I)= ONE
       ENDDO
C TRANSFORM PRINCIPAL PIOLA KIRCHOFF STRESSES TO GLOBAL DIRECTIONS
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
C calcul des contraintes visqueuse ----
C     
* SET SOUND SPEED
        SOUNDSP(I)=SQRT(CIMAX(I)/RHO(I))
c        SOUNDSP(I)=SQRT((2*GMAX/3.+ RBULK)/RHO(I))
* SET VISCOSITY
         VISCMAX(I) = ZERO
       ENDDO
      RETURN
      END

