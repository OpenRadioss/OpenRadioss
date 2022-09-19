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
Chd|  SIGEPS62C                     source/materials/mat/mat062/sigeps62c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
              SUBROUTINE SIGEPS62C(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      NPT0   , ILAYER , IFLAG   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  ,
     3      AREA   , EINT   , THKLYL,
     4      EPSPXX , EPSPYY , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, THKN  , UVAR  , OFF   ,
     B      NGL    , ISMSTR , IPM    , GS    )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include "param_c.inc"
#include "scr17_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL,NUPARAM, NUVAR,ISMSTR, IPM(NPROPMI,*),MAT(NEL),
     .        NPT0,ILAYER, IFLAG(*),NGL(NEL)
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),THKN(NEL),THKLYL(NEL),
     .   RHO0(NEL),AREA(NEL),EINT(NEL,2),GS(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX (NEL),EPSYY (NEL),EPSXY (NEL),EPSYZ (NEL),EPSZX (NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SIGVXX(NEL),SIGVYY(NEL),SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .   SOUNDSP(NEL),VISCMAX(NEL),ET(NEL)
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
      INTEGER  I,J,K,NORDRE,NPRONY,ITER,IVISC,JNV
      my_real
     .   NU,GMAX,RBULK,BETA(100),GAMAINF,DVISC,SUM,FAC,RVT,EVL,
     .   EV1,EV2,EV3,KT3(NEL),INVR(NEL),DSDEV(NEL),DSVOL(NEL),
     .   DQ1(NEL),DQ2(NEL),DQ3(NEL),SDH01(NEL),
     .   SDH02(NEL),SDH03(NEL),SDH1(NEL),SDH2(NEL),SDH3(NEL),
     .   DQDEV(NEL),DQIDEV,INV31,INV32(NEL),INV33(NEL),
     .   RVD(NEL),H10,H20,H30,H1,H2,H3,HP1,HP2,HP3,HD1,HD2,HD3,A11,
     .   EMAX,SDHP1,SDHP2,INV11,INV21
      my_real
     .   EVV(NEL,3),EV(NEL,3),EVM(NEL),DWDL(3),EC(NEL,3),
     .   RHO(NEL),GAMA(NEL),PRES(NEL),RV(NEL),P,SP(NEL,3),
     .   T(NEL,3),SV(NEL,3),S(NEL,3),S0(NEL,3),SD(NEL,3),SD0(NEL,3),
     .   TRAV(NEL),ROOTV(NEL),EIGV(NEL,3,2),DEZZ(NEL),
     .   EV1_SAV(NEL,100),EV2_SAV(NEL,100)
      my_real
     .   MU(100),AL(100),D(100),
     .   TAUX(100)
C=======================================================================
C SET INITIAL MATERIAL CONSTANTS
      NU      = UPARAM(1)
      NORDRE  = NINT(UPARAM(2))
      NPRONY   = NINT(UPARAM(3))
      GAMAINF = UPARAM(4)
      RBULK   = UPARAM(5)
      DVISC   = UPARAM(6)
      GMAX    = ZERO
      DO I= 1,NORDRE
        MU(I) = UPARAM(6 + I)
        AL(I) = UPARAM(6 + NORDRE + I)
        BETA(I) = UPARAM(2*NORDRE + 2*NPRONY + 7 + I)
        GMAX  = GMAX + MU(I)
      ENDDO
      IVISC = 0
      IF (NPRONY /= ZERO) THEN
        DO I=1,NPRONY
          GAMA(I)  = UPARAM(6 + 2*NORDRE + I)
          TAUX(I)  = UPARAM(6 + 2*NORDRE + NPRONY + I)
        ENDDO
        IVISC = 1
      ENDIF
      GMAX = TWO*GMAX
C     User variables initialisation
      IF (TIME == ZERO) THEN
        DO I = 1, NEL
          DO J = 1,NUVAR
            UVAR(I,J) = ZERO
          ENDDO
          UVAR(I,4) = ONE
        ENDDO
      ENDIF
C     principal stretch (def gradient eigenvalues)
      DO I=1,NEL
        TRAV(I)  = EPSXX(I)+EPSYY(I)
        ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .           + EPSXY(I)*EPSXY(I))
	       EVV(I,1) = HALF*(TRAV(I)+ROOTV(I))
        EVV(I,2) = HALF*(TRAV(I)-ROOTV(I))
        EVV(I,3) = ZERO
      ENDDO
C     rot matrix (eigenvectors)
      DO I=1,NEL
        IF (ABS(EVV(I,2)-EVV(I,1)) < EM10) THEN
          EIGV(I,1,1)=ONE
          EIGV(I,2,1)=ONE
          EIGV(I,3,1)=ZERO
          EIGV(I,1,2)=ZERO
          EIGV(I,2,2)=ZERO
          EIGV(I,3,2)=ZERO
        ELSE
          EIGV(I,1,1) = (EPSXX(I)-EVV(I,2))/ROOTV(I)
          EIGV(I,2,1) = (EPSYY(I)-EVV(I,2))/ROOTV(I)
          EIGV(I,3,1) = (HALF*EPSXY(I))  /ROOTV(I)
          EIGV(I,1,2) = (EVV(I,1)-EPSXX(I))/ROOTV(I)
          EIGV(I,2,2) = (EVV(I,1)-EPSYY(I))/ROOTV(I)
          EIGV(I,3,2) =-(HALF*EPSXY(I))  /ROOTV(I)
        ENDIF
      ENDDO
C     Strain definition
      IF (ISMSTR == 1 .OR. ISMSTR == 3) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1)=EVV(I,1)+ ONE
          EV(I,2)=EVV(I,2)+ ONE
          EV(I,3)=UVAR(I,4)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EV(I,1)=EXP(EVV(I,1))
          EV(I,2)=EXP(EVV(I,2))
          EV(I,3)=UVAR(I,4)
        ENDDO
      ENDIF
C--------------------------------------
C      Newton method =>  Find EV(3) : T3(EV(3)) = 0
C--------------------------------------
      IF(IVISC == 0) THEN
        DO K=1,NORDRE
           DO I=1,NEL
               EV1  = ZERO
               IF(EV(I,1) > ZERO) THEN
                   EV1 = ONE
                   IF(AL(K) /= ZERO)EV1 = EXP(AL(K)*LOG(EV(I,1)))
               ENDIF
               EV2  = ZERO
               IF(EV(I,2)> ZERO) THEN
                   EV2 = ONE
                   IF(AL(K) /= ZERO)  EV2 = EXP(AL(K)*LOG(EV(I,2)))
               ENDIF
               EV1_SAV(I,K) = EV1
               EV2_SAV(I,K) = EV2
           ENDDO
        ENDDO
!       -----------------------
        DO ITER = 1,5
C                                     
! RVT = RV(I)**(-BETA*AL(K))
! if rv > 0 --> pui = exp(-beta * ln(ev))
! else pui = 0
            RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)
            T(1:NEL,3) = ZERO
            KT3(1:NEL)  = ZERO
!          -----------------------
            DO K=1,NORDRE
                DO I=1,NEL
                 EV1  = EV1_SAV(I,K)
                 EV2  = EV2_SAV(I,K)
                 EV3  = ZERO
                 IF(EV(I,3)> ZERO) THEN
                   EV3 = ONE
                   IF(AL(K) /= ZERO) EV3 = EXP(AL(K)*LOG(EV(I,3)))
                 ENDIF
                 RVT = ZERO
                 IF(RV(I) > ZERO) THEN
                   RVT = ONE
                   IF(AL(K)/= ZERO) RVT = EXP((-BETA(K)*AL(K))*LOG(RV(I)))
                 ENDIF
C             
                 FAC = TWO*MU(K)/AL(K)
C----       cauchy stress 
                 T(I,3) = T(I,3) + FAC*(EV3 -  RVT)/RV(I)

                 KT3(I) = KT3(I) +
     .                  (-FAC       *(EV3 - RVT) +
     .                    TWO*MU(K)*(EV3 + BETA(K)*RVT))/EV(I,3)/RV(I)
                ENDDO
            ENDDO   ! Ordre
!          -----------------------
            DO I=1,NEL
             IF(KT3(I) /= ZERO) EV(I,3)= EV(I,3) - T(I,3) / KT3(I)
            ENDDO
        ENDDO  ! Ite
!       -----------------------

        UVAR(1:NEL,4) = EV(1:NEL,3)
C compute cauchy stress using the converged value           
        RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)
C           
        T(1:NEL,1) = ZERO
        T(1:NEL,2) = ZERO
        T(1:NEL,3) = ZERO
!       -----------------------
        DO K=1,NORDRE
           DO I=1,NEL
             EV1  = EV1_SAV(I,K)
             EV2  = EV2_SAV(I,K)
             EV3  = ZERO
             IF(EV(I,3)> ZERO) THEN
               EV3 = ONE
               IF(AL(K) /= ZERO)  EV3 = EXP(AL(K)*LOG(EV(I,3)))
             ENDIF
              RVT = ZERO
              IF(RV(I) > ZERO) THEN
              RVT = ONE
                IF(AL(K)/= ZERO) RVT = EXP((-BETA(K)*AL(K))*LOG(RV(I)))
              ENDIF

              FAC = TWO*MU(K)/AL(K)
              T(I,1) = T(I,1) + FAC*(EV1 -  RVT)/RV(I)
              T(I,2) = T(I,2) + FAC*(EV2 -  RVT)/RV(I)
              T(I,3) = T(I,3) + FAC*(EV3 -  RVT)/RV(I)
           ENDDO
         ENDDO
!       -----------------------
      ELSE ! with viscosity
C-----------------------------------------------------------
C     Maxwell model -
C-----------------------------------------------------------
        DO K=1,NORDRE
           DO I=1,NEL
               EV1  = ZERO
               IF(EV(I,1) > ZERO) THEN
                   EV1 = ONE
                   IF(AL(K) /= ZERO)EV1 = EXP(AL(K)*LOG(EV(I,1)))
               ENDIF
               EV2  = ZERO
               IF(EV(I,2)> ZERO) THEN
                   EV2 = ONE
                   IF(AL(K) /= ZERO)  EV2 = EXP(AL(K)*LOG(EV(I,2)))
               ENDIF
               EV1_SAV(I,K) = EV1
               EV2_SAV(I,K) = EV2
           ENDDO
        ENDDO
!       -----------------------
        DO ITER = 1,5
          RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)
          INVR(1:NEL) = ONE/MAX(EM20,RV(1:NEL))
          DSDEV(1:NEL) = ZERO
          DSVOL(1:NEL)  = ZERO
          KT3(1:NEL)  = ZERO
          T(1:NEL,1) = ZERO
          T(1:NEL,2) = ZERO
          T(1:NEL,3) = ZERO
          DQ1(1:NEL) = ZERO
          DQ2(1:NEL) = ZERO
          DQ3(1:NEL) = ZERO
          DO K=1,NORDRE
            DO I=1,NEL
              EV1  = EV1_SAV(I,K)
              EV2  = EV2_SAV(I,K)
              EV3  = ZERO
              IF(EV(I,3)> ZERO) THEN
                 EV3 = ONE
                 IF(AL(K) /= ZERO) EV3 = EXP(AL(K)*LOG(EV(I,3)))
              ENDIF
C             
              RVT = ZERO
              IF(RV(I) > ZERO) THEN
                RVT = ONE
                IF(AL(K)/= ZERO) RVT = EXP((-BETA(K)*AL(K))*LOG(RV(I)))
              ENDIF
C             
              FAC = TWO*MU(K)/AL(K)
C----       cauchy stress 
              T(I,1) = T(I,1) + FAC*(EV1 -  RVT)*INVR(I)
              T(I,2) = T(I,2) + FAC*(EV2 -  RVT)*INVR(I)
              T(I,3) = T(I,3) + FAC*(EV3 -  RVT)*INVR(I)
C              
              DSVOL(I) = DSVOL(I) - TWO_THIRD*FAC*(EV1 + EV2 + EV3) +
     .                      + TWO*FAC*RVT + TWO_THIRD*MU(K)*EV3
     .                      + TWO*BETA(K)*MU(K)*RVT
C     
              DSDEV(I) = DSDEV(I) - TWO*FAC*(TWO_THIRD*EV3 - THIRD*(EV1 + EV2))
     .                      + FOUR_OVER_3*MU(K)*EV3
              DQ1(I) = DQ1(I) - TWO_THIRD*MU(K)*EV1
              DQ2(I) = DQ2(I) - TWO_THIRD*MU(K)*EV2
              DQ3(I) = DQ3(I) + FOUR_OVER_3*MU(K)*EV3
            ENDDO
          ENDDO   !   Nordre


          DO I=1,NEL
            INV31 = ONE/EV(I,3)
            INV32(I) = INV31*INV31
            INV33(I) = INV32(I)*INV31
            INV11 = ONE/MAX(EM20,EV(I,1))
            INV21 = ONE/MAX(EM20,EV(I,2))
C              
            DSVOL(I) = DSVOL(I)*INV33(I)
            DSDEV(I) = DSDEV(I)*INV33(I)
            DQ1(I) = DQ1(I)*INV31*INV11*INV11
            DQ2(I) = DQ2(I)*INV31*INV21*INV21
            DQ2(I) = DQ3(I)*INV31
C
C Deviatoric stress + pressure
C                 
             P = THIRD*(T(I,1) + T(I,2) + T(I,3))
C PK2 pressure stress 
            EC(I,1) = EV(I,1)*EV(I,1)
            EC(I,2) = EV(I,2)*EV(I,2)
            EC(I,3) = EV(I,3)*EV(I,3)
C PK2 pressure stress            
            SP(I,1)= RV(I)*P/MAX(EM20,EC(I,1))
            SP(I,2)= RV(I)*P/MAX(EM20,EC(I,2))
            SP(I,3)= RV(I)*P/MAX(EM20,EC(I,3))

C Deviatoric PK2 stress 
            SD(I,1)= RV(I)*(T(I,1) - P)/MAX(EM20,EC(I,1))
            SD(I,2)= RV(I)*(T(I,2) - P)/MAX(EM20,EC(I,2))
            SD(I,3)= RV(I)*(T(I,3) - P)/MAX(EM20,EC(I,3))
            RVD(I) = ZERO
            IF(RV(I)>ZERO) RVD(I) =  EXP( TWO_THIRD*LOG(RV(I)))
C             
            SDHP1 = RVD(I)*SD(I,1)
            SDHP2 = RVD(I)*SD(I,2)
            SDH3(I) = RVD(I)*SD(I,3)

            DQ1(I) = DQ1(I)*RVD(I) + TWO_THIRD*SDHP1*INV31
            DQ2(I) = DQ2(I)*RVD(I) + TWO_THIRD*SDHP2*INV31
            DQ2(I) = DQ2(I)*RVD(I) + TWO_THIRD*SDH3(I)*EV(I,3)

C      old Deviatoric PK2 stress in the global frame      
            SDH01(I)  = UVAR(I,1)
            SDH02(I)  = UVAR(I,2)
            SDH03(I)  = UVAR(I,3)
C            
            SDH1(I)  = EIGV(I,1,1)*SDHP1 + EIGV(I,1,2)*SDHP2
            SDH2(I)  = EIGV(I,2,1)*SDHP1 + EIGV(I,2,2)*SDHP2
            !SDH3(I)  = SDH3(I)

            DQDEV(I) = ZERO
            RVD(I) = ZERO
            IF(RV(I) > ZERO)RVD(I) = EXP( (-TWO_THIRD)*LOG(RV(I)) )
            S(I,3) = SP(I,3) + GAMAINF*SD(I,3)
          ENDDO
          JNV = 4
C  viscosity            
          DO K = 1,NPRONY
             DQIDEV = ZERO
             FAC= -TIMESTEP/TAUX(K)
             DO I=1,NEL
               H10     =  UVAR(I,JNV + K )
               H20     =  UVAR(I,JNV + NPRONY + K )
               H30     =  UVAR(I,JNV + 2*NPRONY + K )
C                             
               H1  = EXP(FAC)*H10+ EXP(HALF*FAC)*(SDH1(I) - SDH01(I))
               H2  = EXP(FAC)*H20+ EXP(HALF*FAC)*(SDH2(I) - SDH02(I))
               H3  = EXP(FAC)*H30+ EXP(HALF*FAC)*(SDH3(I) - SDH03(I))

C H in the principal frame 
               HP1  = EIGV(I,1,1)*H1 + EIGV(I,2,1)*H2
               HP2  = EIGV(I,1,2)*H1 + EIGV(I,2,2)*H2

C         H  Deviatoric part
               FAC = THIRD*(HP1*EC(I,1) + HP2*EC(I,2) + H3*EC(I,3))
               HD1 = HP1 - FAC/MAX(EM20,EC(I,1))
               HD2 = HP2 - FAC/MAX(EM20,EC(I,2))
               HD3 = H3 - FAC/MAX(EM20,EC(I,3))
C compute stress
               S(I,3) = S(I,3) + GAMA(K)*RVD(I)*HD3
               DQIDEV = EXP(HALF*FAC)*(DSDEV(I)
     .                     -THIRD*(EC(I,1)*DQ1(I)+EC(I,2)*DQ2(I)+EC(I,3)*DQ3(I))*INV32(I))
     .                     +TWO_THIRD*(EC(I,1)*HP1+ EC(I,2)*HP2 + H3*EC(I,3))*INV33(I)
               DQDEV(I)=DQDEV(I) + RVD(I)*(-GAMA(K)*DQIDEV + TWO_THIRD*GAMA(K)*HD3 )
             ENDDO      ! 1,NEL
          ENDDO ! NPRONY
          KT3(1:NEL) = DSVOL(1:NEL) + GAMAINF*DSDEV(1:NEL)   + DQDEV(1:NEL)
C  update of lam_3
          DO I=1,NEL
             IF(KT3(I) /= ZERO) EV(I,3) = EV(I,3) - S(I,3)/ KT3(I)
          ENDDO
        ENDDO ! iter





C stocked value           
        UVAR(1:NEL,4) = EV(1:NEL,3)
Commpute stress after convergency           
           ! RVT = RV(I)**(-BETA*AL(K))
! if rv > 0 --> pui = exp(-beta * ln(ev))
! else pui = 0
        RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)
C
        T(1:NEL,1) = ZERO
        T(1:NEL,2) = ZERO
        T(1:NEL,3) = ZERO
        DO K=1,NORDRE
          DO I=1,NEL
              EV1 = EV1_SAV(I,K)
              EV2 = EV2_SAV(I,K)
C             
              RVT = ZERO
              IF(RV(I) > ZERO) THEN
                RVT = ONE
                IF(AL(K)/= ZERO) RVT = EXP((-BETA(K)*AL(K))*LOG(RV(I)))
              ENDIF
C             
              EV3  = ZERO
              IF(EV(I,3)> ZERO) THEN
                 EV3 = ONE
                 IF(AL(K) /= ZERO) EV3 = EXP(AL(K)*LOG(EV(I,3)))
              ENDIF

              FAC = TWO*MU(K)/AL(K)
C----       cauchy stress 
              T(I,1) = T(I,1) + FAC*(EV1 -  RVT)/RV(I)
              T(I,2) = T(I,2) + FAC*(EV2 -  RVT)/RV(I)
              T(I,3) = T(I,3) + FAC*(EV3 -  RVT)/RV(I)
          ENDDO
        ENDDO  ! NORDRE

        DO I=1,NEL
C
C Deviatoric stress + pressure
C                 
            P = THIRD*(T(I,1) + T(I,2) + T(I,3))
C      
            EC(I,1) = EV(I,1)*EV(I,1)
            EC(I,2) = EV(I,2)*EV(I,2)
            EC(I,3) = EV(I,3)*EV(I,3)
C PK2 pressure stress            
            SP(I,1)= RV(I)*P/MAX(EM20,EC(I,1))
            SP(I,2)= RV(I)*P/MAX(EM20,EC(I,2))
            SP(I,3)= RV(I)*P/MAX(EM20,EC(I,3))
C Deviatoric PK2 stress            
            SD(I,1)= RV(I)*(T(I,1) - P)/MAX(EM20,EC(I,1))
            SD(I,2)= RV(I)*(T(I,2) - P)/MAX(EM20,EC(I,2))
            SD(I,3)= RV(I)*(T(I,3) - P)/MAX(EM20,EC(I,3))
            RVD(I) = ZERO
            IF(RV(I)>ZERO) RVD(I) =  EXP( TWO_THIRD*LOG(RV(I)))
C             
            SDHP1 = RVD(I)*SD(I,1)
            SDHP2 = RVD(I)*SD(I,2)
            SDH3(I) = RVD(I)*SD(I,3)
C              
C compute cauchy stress using the converged value           
C

C      old Deviatoric PK2 stress in the global frame      
            SDH01(I)  = UVAR(I,1)
            SDH02(I)  = UVAR(I,2)
            SDH03(I)  = UVAR(I,3)
C            
            SDH1(I)  = EIGV(I,1,1)*SDHP1 + EIGV(I,1,2)*SDHP2
            SDH2(I)  = EIGV(I,2,1)*SDHP1 + EIGV(I,2,2)*SDHP2
            !SDH3(I)  = SDH3(I)

            UVAR(I,1) = SDH1(I)
            UVAR(I,2) = SDH2(I)
            UVAR(I,3) = SDH3(I)

            RVD(I) = ZERO
            IF(RV(I) > ZERO)RVD(I) = EXP( (-TWO_THIRD)*LOG(RV(I)) )
             S(I,1) = SP(I,1) + GAMAINF*SD(I,1)
             S(I,2) = SP(I,2) + GAMAINF*SD(I,2)
             S(I,3) = SP(I,3) + GAMAINF*SD(I,3)
        ENDDO
C            
        JNV = 4
        DO K = 1,NPRONY
           DO I=1,NEL
               FAC= -TIMESTEP/TAUX(K)
               H10     =  UVAR(I,JNV + K )
               H20     =  UVAR(I,JNV + NPRONY + K )
               H30     =  UVAR(I,JNV + 2*NPRONY + K )
               H1  = EXP(FAC)*H10+ EXP(HALF*FAC)*(SDH1(I) - SDH01(I))
               H2  = EXP(FAC)*H20+ EXP(HALF*FAC)*(SDH2(I) - SDH02(I))
               H3  = EXP(FAC)*H30+ EXP(HALF*FAC)*(SDH3(I) - SDH03(I))
C             
               UVAR(I,JNV +            K )= H1
               UVAR(I,JNV + NPRONY   + K )= H2
               UVAR(I,JNV + 2*NPRONY + K )= H3
C H in the principal frame 
               HP1  = EIGV(I,1,1)*H1 + EIGV(I,2,1)*H2
               HP2  = EIGV(I,1,2)*H1 + EIGV(I,2,2)*H2

C           Deviatoric part
              FAC = THIRD*(HP1*EC(I,1) + HP2*EC(I,2) + H3*EC(I,3))
              HD1 = HP1 - FAC/MAX(EM20,EC(I,1))
              HD2 = HP2 - FAC/MAX(EM20,EC(I,2))
              HD3 = H3 - FAC/MAX(EM20,EC(I,3))
C add the viscoous stress                   
              S(I,1) = S(I,1) + GAMA(K)*RVD(I)*HD1
              S(I,2) = S(I,2) + GAMA(K)*RVD(I)*HD2
              S(I,3) = S(I,3) + GAMA(K)*RVD(I)*HD3
          ENDDO  ! 1,NEL
        ENDDO    ! NPRONY

C transformation PK2 to cauchy stress
        DO I=1,NEL
            INVR(I)= ONE/MAX(EM20,RV(I))
            T(I,1) = INVR(I)*S(I,1)*EV(I,1)**2
            T(I,2) = INVR(I)*S(I,2)*EV(I,2)**2
            T(I,3) = INVR(I)*S(I,3)*EV(I,3)**2
        ENDDO
      ENDIF
C-----------------------------------------------------------
C-----------------------------------------------------------
C     Principal Cauchy viscous stress -> global directions
      DO I=1,NEL
        SIGNXX(I) = EIGV(I,1,1)*T(I,1) + EIGV(I,1,2)*T(I,2)
        SIGNYY(I) = EIGV(I,2,1)*T(I,1) + EIGV(I,2,2)*T(I,2)
        SIGNXY(I) = EIGV(I,3,1)*T(I,1) + EIGV(I,3,2)*T(I,2)
        SIGNYZ(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GS(I)*DEPSZX(I)
      ENDDO
C-----------------------------------------------------------
C     set sound speed & viscosity
      DO I=1,NEL
        DEZZ(I)    =-NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
        THKN(I)    = THKN(I) + DEZZ(I)*THKLYL(I)
        RHO(I)     = RHO0(I)/RV(I)
C        
        EMAX = GMAX*(ONE + NU)
        A11  = EMAX/(ONE - NU**2)
        SOUNDSP(I)= SQRT(A11/RHO0(I))
!!        SOUNDSP(I) = SQRT((TWO_THIRD*GMAX+RBULK)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO

C-----------
      RETURN
      END
