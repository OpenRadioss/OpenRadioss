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
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        PRODMAT                       source/materials/tools/prodmat.F
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
              SUBROUTINE SIGEPS42(
     1      NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2      TF      ,TIME    ,TIMESTEP,UPARAM  ,RHO0    ,RHO     ,
     3      VOLUME  ,EINT    ,UVAR    ,OFF     ,OFFG    ,SOUNDSP ,
     4      EPSP1   ,EPSP2   ,EPSP3   ,EPSP4   ,EPSP5   ,EPSP6   ,
     5      EPSXX   ,EPSYY   ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     6      SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7      MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,  
     8      MFZX    ,MFZY    ,MFZZ    ,VISCMAX ,ISMSTR  ,ET      ,
     9      IHET    ,EPSTH3  ,IEXPAN  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "scr17_c.inc"
#include "impl1_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,ISMSTR,IHET,IEXPAN
      my_real TIME, TIMESTEP, UPARAM(NUPARAM)
      my_real ,DIMENSION(NEL), INTENT(IN) ::RHO,RHO0,VOLUME,EINT,OFFG,
     .  EPSTH3,EPSP1,EPSP2,EPSP3,EPSP4,EPSP5,EPSP6,                  
     .  EPSXX,EPSYY,EPSZZ,EPSXY,EPSYZ,EPSZX,               
     .  MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ         
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(OUT) :: SOUNDSP,VISCMAX,
     .  SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL) ,ET(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,II,J,K,IFLAG,KFP,KFJ,IJACOBI,NROT,NPRONY,IOLDF
      my_real MU1,MU2,MU3,MU4,MU5,AL1,AL2,AL3,AL4,AL5
      my_real TENSIONCUT,AMAX,GVMAX ,GMAX,FFAC,TRACE,RBULK,DPDMU,
     .  AA,CC,FAC,INVDT,INVE,ETV,RV_PUI,LAM_AL(3)
      my_real ,DIMENSION(NEL)   :: SUMDWDL,RV,J2THIRD,P,DWDRV,ETI  
      my_real ,DIMENSION(NEL,3) :: T,T1,SIGPRV,EV,EVM,DWDL,CII
      my_real ,DIMENSION(NEL,6) :: DOTB,SV,C0,C1
      my_real ,DIMENSION(NEL,3,3) :: F,LL,BB,LB,BLT
      my_real AL_TAB(5),MU_TAB(5)
      my_real A(3,3),DPRA(3,3),EIGEN(3),FFT(3)   
      my_real GI(100),TAUX(100),H0(NEL,6,100),H(NEL,6,100)
      my_real :: AV(MVSIZ,6),EVV(MVSIZ,3),DIRPRV(MVSIZ,3,3)
      my_real ,DIMENSION(NEL)   :: GTMAX  
      DOUBLE PRECISION AMAX1
c=======================================================================
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
      GMAX = MU1*AL1+MU2*AL2+MU3*AL3+MU4*AL4+MU5*AL5
      DO I=1,5
        AL_TAB(I) = UPARAM(5+I)
        MU_TAB(I) = UPARAM(I)
      ENDDO
      RBULK=UPARAM(11)
      TENSIONCUT=UPARAM(12)
      IFLAG=UPARAM(13)
      FFAC =UPARAM(15)
      NPRONY = UPARAM(16)
      IOLDF  = UPARAM(17)
      KFP=IFUNC(1)
      KFJ=IFUNC(2)
      INVDT = TIMESTEP/MAX(TIMESTEP**2 , EM20)
      GVMAX = ZERO
      ETV   = ZERO
      IF (NPRONY > 0) THEN
        DO I=1,NPRONY
          TAUX(I) = UPARAM(17 + NPRONY + I)
          GI(I)   = UPARAM(17 + I)
          GVMAX   = GVMAX + GI(I)
        ENDDO
        ETV = MIN(GVMAX,RBULK)/GMAX
      ENDIF
C
      DO I=1,NEL
        AV(I,1)=EPSXX(I)
        AV(I,2)=EPSYY(I)
        AV(I,3)=EPSZZ(I)
        AV(I,4)=EPSXY(I) * UNDEMI
        AV(I,5)=EPSYZ(I) * UNDEMI
        AV(I,6)=EPSZX(I) * UNDEMI
      ENDDO
C         Eigenvalues needed to be calcultated in double precision
C         for a simple precision executing
      IF (IRESP == 1) THEN
        CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
      ENDIF
*ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
*ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
*ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
*ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT

      IF (ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
        DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
         EV(I,1)=EXP(EVV(I,1))
         EV(I,2)=EXP(EVV(I,2))
         EV(I,3)=EXP(EVV(I,3))
        ENDDO 
      ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
        DO I =1,NEL
          IF(OFFG(I)<=UN) THEN
            EV(I,1)=SQRT(EVV(I,1)+ UN)
            EV(I,2)=SQRT(EVV(I,2)+ UN)
            EV(I,3)=SQRT(EVV(I,3)+ UN)
     	  ELSE
           EV(I,1)=EVV(I,1)+ UN
           EV(I,2)=EVV(I,2)+ UN
           EV(I,3)=EVV(I,3)+ UN
          END IF
        ENDDO 
      ELSE
C ----  STRAIN IS ENGINEERING)
        DO I=1,NEL
         EV(I,1) = EVV(I,1) + UN
         EV(I,2) = EVV(I,2) + UN
         EV(I,3) = EVV(I,3) + UN
        ENDDO 

      ENDIF
C----------------
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
        DO J =1,3
           ! ETI = sum[MU(i)*AL(i)*AMAX**(AL(i)-1)] (i=1,5)
           ! AMAX = 0 --> ETI = 0
           ! else     --> ETI = sum[MU(i)*AL(i)*exp( (AL(i)-1)*ln(AMAX) ) ] ; (i=1,5)
          ETI(1:NEL) = ZERO
          DO II = 1,5
           IF(MU_TAB(II)*AL_TAB(II)/=ZERO) THEN
              DO I=1,NEL
                AMAX = EV(I,J)                
                IF(AMAX/=ZERO) THEN
                  IF((AL_TAB(II)-UN)/=ZERO) THEN
                    ETI(I) = ETI(I) + MU_TAB(II)*AL_TAB(II) * 
     .                    EXP((AL_TAB(II)-UN)*LOG(AMAX))
                  ELSE
                    ETI(I) = ETI(I) + MU_TAB(II)*AL_TAB(II)
                  ENDIF
                ENDIF
              ENDDO
           ENDIF
          ENDDO
          ET(1:NEL)= MAX(ETI(1:NEL),ET(1:NEL))
        ENDDO
        DO I=1,NEL
          ET(I) = MAX(UN,ET(I)/GMAX)
          ET(I)= ET(I)+ETV
        ENDDO
      ENDIF
c
*     RV = RHO0/RHO = RELATIVE VOLUME = DET(F) (F = GRADIENT OF DEFORMATION)
      DO I=1,NEL
        RV(I) = (EV(I,1)*EV(I,2)*EV(I,3)) 
      ENDDO
C----THERM STRESS COMPUTATION-----
      IF (IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) THEN
        DO I=1,NEL
          RV(I) =  RV(I)-EPSTH3(I) 
        ENDDO
      ENDIF
C---------------------------------
c     COMPUTE MODIFIED (NORMALIZED) STRETCHES - THIS UNIFIES A COMPRESSIBLE AND 
c     INCOMPRESIBLE FORMULATIONS
      DO I=1,NEL
        ! EVM = EV*PUI
        ! PUI = RV**(-THIRD)
        ! RV = 0 --> PUI = 0
        ! else   --> PUI = exp( -THIRD* ln( RV ) )
        IF (RV(I) > ZERO) THEN
          RV_PUI = EXP((-THIRD)*LOG(RV(I)))
          J2THIRD(I) = RV_PUI**2
        ELSE
          RV_PUI     = ZERO
          J2THIRD(I) = ZERO
        ENDIF
        EVM(I,1) = EV(I,1)*RV_PUI 
        EVM(I,2) = EV(I,2)*RV_PUI
        EVM(I,3) = EV(I,3)*RV_PUI
        
        IF (KFP > 0) THEN
c         READ BULK MODULE FROM FUNCTION
          P(I)=RBULK*FFAC*FINTER(KFP,RV(I),NPF,TF,DPDMU)
        ELSE
c         BULK MODULE IS CONSTANT
          P(I)=RBULK
        ENDIF
      ENDDO
C
C---------GTMAX=GT_MAX+GVMAX
       GTMAX(1:NEL) = GMAX + GVMAX
C------max of Cii : IOLDF <0 to deactivate      
      IF (IOLDF>=0) THEN
          ETI(1:NEL) = ZERO
          CII(1:NEL,1:3) = ZERO
          DO II = 1,5
           IF(MU_TAB(II)*AL_TAB(II)/=ZERO) THEN
              DO I=1,NEL
               LAM_AL(1:3) = EXP(AL_TAB(II)*LOG(EVM(I,1:3)))
               AMAX = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
               CII(I,1:3) = CII(I,1:3) + MU_TAB(II)*AL_TAB(II) * (LAM_AL(1:3)+AMAX)
              ENDDO
           ENDIF
          ENDDO
C------2GT=3/2 Cii (factor 3 already inside CII)		  
           AMAX1= 0.81*UNDEMI/GMAX
          DO I=1,NEL
           AMAX= AMAX1*MAX(CII(I,1),CII(I,2),CII(I,3))
           ETI(I) = MAX(UN,AMAX) 
C---------reduce old result change		   
           GTMAX(I) = GMAX*ETI(I) + GVMAX
C---- see if many QA return for HEPH 		   
           ET(I)= UN+ETV
          ENDDO
      ENDIF
* STRAIN ENERGY = W(L1,L2,L3)=SUM_OF_1_TO_P (MUP(L1**ALP+L2**ALP+L3**ALP-3-
* ALPLN(L1*L2*L3))/ALP+.5*P(I)(J-1)**2

      DO J=1,3
        DWDL(1:NEL,J)=ZERO
* L(J).DW/DL(J)
         ! DWDL = sum[ MU(i) * EVM(i)**AL(i) ] (i=1,5)
         ! EVM = 0 --> DWDL = 0
         ! else    --> DWDL = sum[MU(i) * exp(AL(i) ln( EVN(i)))] (i=1,5)
        DO II=1,5
         IF(MU_TAB(II)/=ZERO) THEN
          DO I=1,NEL
           IF(EVM(I,J)/=ZERO) THEN
            IF(AL_TAB(II)/=ZERO) THEN
             DWDL(I,J) = DWDL(I,J) + MU_TAB(II) *
     .                  EXP(AL_TAB(II)*LOG(EVM(I,J)))
            ELSE
             DWDL(I,J) = DWDL(I,J) + MU_TAB(II)
            ENDIF
           ENDIF
          ENDDO
         ENDIF
        ENDDO
      ENDDO

      DO I=1,NEL
* DW/RV
        DWDRV(I)=P(I)*(RV(I)- UN)
* (SUM(L(J)DW/DL(J))/3
        SUMDWDL(I)=(DWDL(I,1)+DWDL(I,2)+DWDL(I,3))* THIRD
        
      ENDDO

* RV.T(J)=L(J).DW/DL(J)-(SUM_OF_1_TO_3(D(W)/D(LJ))-RV.DWDRV)
      DO J=1,3
        DO I=1,NEL
            INVE = UN/RV(I)
            T(I,J)=(DWDL(I,J)-(SUMDWDL(I)-RV(I)*DWDRV(I)))*INVE
        ENDDO
      ENDDO

      DO I=1,NEL
* TRANSFORM FROM CAUCHY TO BIOT STRESS
          T1(I,1)=RV(I)*T(I,1)/EV(I,1)
          T1(I,2)=RV(I)*T(I,2)/EV(I,2)
          T1(I,3)=RV(I)*T(I,3)/EV(I,3)
      ENDDO

      DO I=1,NEL
          SIGPRV(I,1)=T(I,1)
          SIGPRV(I,2)=T(I,2)
          SIGPRV(I,3)=T(I,3)
      ENDDO

* TENSION CUT
      DO J=1,3
          DO I=1,NEL
          IF(OFF(I).EQ.ZERO.OR.T1(I,J).GT.ABS(TENSIONCUT))THEN
            SIGPRV(I,1)=ZERO
            SIGPRV(I,2)=ZERO
            SIGPRV(I,3)=ZERO
            IF(OFF(I).NE.ZERO) IDEL7NOK = 1
            OFF(I)=ZERO
          ENDIF
          ENDDO
      ENDDO

C      GMAX = GMAX + GVMAX

* TRANSFORM PRINCIPAL CAUCHY STRESSES TO GLOBAL DIRECTIONS
      DO I=1,NEL
* STORE VALUES FOR POST-PROCESSING
        UVAR(I,1)=MAX(SIGPRV(I,1),SIGPRV(I,2),SIGPRV(I,3))
        UVAR(I,2)=MIN(SIGPRV(I,1),SIGPRV(I,2),SIGPRV(I,3))
        UVAR(I,3)=OFF(I)
C        
        SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
c     
        SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
c     
        SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
c     
        SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
c     
        SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
c     
        SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
* SET SOUND SPEED
        SOUNDSP(I)=SQRT((DTHIRD*GTMAX(I)+P(I))/RHO(I))
* SET VISCOSITY
        VISCMAX(I)=ZERO
      ENDDO
C
C viscosity
C      
      IF (NPRONY > 0) THEN  
        IF (IOLDF > 0) THEN    ! Old formulation 
          DO I=1,NEL 
           C0(I,1) = UVAR(I,4)
           C0(I,2) = UVAR(I,5)
           C0(I,3) = UVAR(I,6)  
C   
           CC = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2)
           C1(I,1) =  EVM(I,1)**2 - CC
           C1(I,2) =  EVM(I,2)**2 - CC
           C1(I,3) =  EVM(I,3)**2 - CC 
C          
           UVAR(I,4) = C1(I,1)    
           UVAR(I,5) = C1(I,2)
           UVAR(I,6) = C1(I,3)
          ENDDO
C  
          DO J=1,3
              SV(1:NEL,J) = ZERO
              DO II= 1,NPRONY 
                FAC= -TIMESTEP/TAUX(II)
                DO I=1,NEL 
                 H0(I,J,II) = UVAR(I, 6 + (II - 1)*3 + J)
                 H(I,J,II)  = EXP(FAC)*H0(I,J,II) +
     .            EXP(UNDEMI*FAC)*(C1(I,J) - C0(I,J))
                 UVAR(I,  6 + (II - 1)*3 + J)=  H(I,J,II)
                ENDDO
              ENDDO
C 
C  compute cauchy visco stress
C          
              DO II = 1,NPRONY
                DO I=1,NEL
                   SV(I,J) = SV(I,J) + GI(II)*H(I,J,II) 
                ENDDO         
              ENDDO  
          ENDDO
       
C       * TRANSFORM PRINCIPAL VISCO CAUCHY STRESSES TO GLOBAL DIRECTIONS
          DO I=1,NEL
           INVE = UN/RV(I)
           SV(I,1) = SV(I,1)*INVE
           SV(I,2) = SV(I,2)*INVE
           SV(I,3) = SV(I,3)*INVE
C           
           SIGNXX(I) = SIGNXX(I)  
     .               + DIRPRV(I,1,1)*DIRPRV(I,1,1)*SV(I,1)
     .               + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SV(I,2)
     .               + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SV(I,3)     
            SIGNYY(I) = SIGNYY(I) 
     .               + DIRPRV(I,2,2)*DIRPRV(I,2,2)*SV(I,2)
     .               + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SV(I,3)
     .               + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SV(I,1)     
            SIGNZZ(I) = SIGNZZ(I) 
     .               + DIRPRV(I,3,3)*DIRPRV(I,3,3)*SV(I,3)
     .               + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SV(I,1)
     .               + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SV(I,2)     
            SIGNXY(I) = SIGNXY(I) 
     .               + DIRPRV(I,1,1)*DIRPRV(I,2,1)*SV(I,1)
     .               + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SV(I,2)
     .               + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SV(I,3)     
            SIGNYZ(I) = SIGNYZ(I) 
     .               + DIRPRV(I,2,2)*DIRPRV(I,3,2)*SV(I,2)
     .               + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SV(I,3)
     .               + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SV(I,1)     
            SIGNZX(I) = SIGNZX(I) 
     .               + DIRPRV(I,3,3)*DIRPRV(I,1,3)*SV(I,3)
     .               + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SV(I,1)
     .               + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SV(I,2) 
            
          ENDDO 
c---
        ELSEIF (ISMSTR == 10 .or. ISMSTR == 12) THEN     ! New formulation, using Dot(B)   
c---
c         F = deformation gradient
c         L = velocity gradient
c         L = D + W = sym(L) + skw(L) => LL = sym(L)
c
          DO I=1,NEL
             LL(I,1,1) = EPSP1(I)           ! EPSPXX
             LL(I,2,2) = EPSP2(I)           ! EPSPYY
             LL(I,3,3) = EPSP3(I)           ! EPSPZZ
             LL(I,1,2) = EPSP4(I) * UNDEMI  !(EPSPXY(I) + EPSPYX(I))/2
             LL(I,2,3) = EPSP5(I) * UNDEMI  !(EPSPYZ(I) + EPSPZY(I))/2
             LL(I,1,3) = EPSP6(I) * UNDEMI  !(EPSPZX(I) + EPSPXZ(I))/2
             LL(I,2,1) = LL(I,1,2)
             LL(I,3,1) = LL(I,1,3)
             LL(I,3,2) = LL(I,2,3)
c
             F(I,1,1)  = UN + MFXX(I)
             F(I,2,2)  = UN + MFYY(I)
             F(I,3,3)  = UN + MFZZ(I)
             F(I,1,2)  = MFXY(I)
             F(I,2,3)  = MFYZ(I)
             F(I,3,1)  = MFZX(I)      
             F(I,2,1)  = MFYX(I)
             F(I,3,2)  = MFZY(I)
             F(I,1,3)  = MFXZ(I)     
          ENDDO
c
          CALL PRODAAT(F , BB, NEL)     ! B = F * FT
c
c         Dev(B) = B * J^(-2/3),  J = det(F)
c
          DO I=1,NEL
            BB(I,1,1) = BB(I,1,1) * J2THIRD(I)
            BB(I,2,2) = BB(I,2,2) * J2THIRD(I)
            BB(I,3,3) = BB(I,3,3) * J2THIRD(I)
            BB(I,1,2) = BB(I,1,2) * J2THIRD(I)
            BB(I,2,3) = BB(I,2,3) * J2THIRD(I)
            BB(I,1,3) = BB(I,1,3) * J2THIRD(I)
            BB(I,2,1) = BB(I,1,2)
            BB(I,3,2) = BB(I,2,3)
            BB(I,3,1) = BB(I,1,3)
          ENDDO
c
          CALL PRODMAT(LL, BB, LB, NEL)     ! LB = L * B          
c
          CALL PRODMAT(BB, LL, BLT, NEL)    ! BLT = B * LT
c
          DO I=1,NEL
            DOTB(I,1) = LB(I,1,1) + BLT(I,1,1)  ! xx
            DOTB(I,2) = LB(I,2,2) + BLT(I,2,2)  ! yy
            DOTB(I,3) = LB(I,3,3) + BLT(I,3,3)  ! zz
            DOTB(I,4) = LB(I,1,2) + BLT(I,1,2)  ! xy = yx
            DOTB(I,5) = LB(I,2,3) + BLT(I,2,3)  ! yz = zy
            DOTB(I,6) = LB(I,1,3) + BLT(I,1,3)  ! xz = zx
          ENDDO
c
          DO J=1,6
            SV(1:NEL,J) = ZERO
            DO II= 1,NPRONY 
              FAC= -TIMESTEP/TAUX(II) 
              DO I=1,NEL
                H0(I,J,II) = UVAR(I, 6 + (II - 1)*6 + J)
                H(I,J,II)  = EXP(FAC)*H0(I,J,II) +
     .                       EXP(UNDEMI*FAC)*DOTB(I,J)*TIMESTEP
                UVAR(I,  6 + (II - 1)*6 + J)=  H(I,J,II)
              ENDDO
            ENDDO
c
c          Kirchoff viscous stress
c             
            DO II = 1,NPRONY
              DO I=1,NEL
                SV(I,J) = SV(I,J) + GI(II)*H(I,J,II)
              ENDDO         
            ENDDO  
          ENDDO     
c
c          Kirchoff -> Cauchy visc stress : sig = T / J
c
          DO I=1,NEL
            INVE = UN/RV(I)      
            SV(I,1) = SV(I,1)*INVE
            SV(I,2) = SV(I,2)*INVE
            SV(I,3) = SV(I,3)*INVE       
            SV(I,4) = SV(I,4)*INVE
            SV(I,5) = SV(I,5)*INVE
            SV(I,6) = SV(I,6)*INVE
c           deviatoric part of visc stress
            TRACE = (SV(I,1) + SV(I,2) + SV(I,3)) * TIERS
            SV(I,1) = SV(I,1) - TRACE
            SV(I,2) = SV(I,2) - TRACE
            SV(I,3) = SV(I,3) - TRACE      
c           total stress
            SIGNXX(I) = SIGNXX(I) + SV(I,1)
            SIGNYY(I) = SIGNYY(I) + SV(I,2)
            SIGNZZ(I) = SIGNZZ(I) + SV(I,3)
            SIGNXY(I) = SIGNXY(I) + SV(I,4)
            SIGNYZ(I) = SIGNYZ(I) + SV(I,5)
            SIGNZX(I) = SIGNZX(I) + SV(I,6) 
          ENDDO 
c
        ELSE  ! ISMSTR /= 10
c
          DO I=1,NEL 
C           
            C0(I,1) = UVAR(I,4)
            C0(I,2) = UVAR(I,5)
            C0(I,3) = UVAR(I,6)  
            C0(I,4) = UVAR(I,7)
            C0(I,5) = UVAR(I,8)
            C0(I,6) = UVAR(I,9)
C            
            CC = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2)
            FFT(1) =  EVM(I,1)**2 - CC
            FFT(2) =  EVM(I,2)**2 - CC
            FFT(3) =  EVM(I,3)**2 - CC   
c
            C1(I,1) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*FFT(1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*FFT(2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*FFT(3)
c     
            C1(I,2) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*FFT(2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*FFT(3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*FFT(1)
c     
            C1(I,3) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*FFT(3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*FFT(1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*FFT(2)
c     
            C1(I,4) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*FFT(1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*FFT(2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*FFT(3)
c     
            C1(I,5) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*FFT(2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*FFT(3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*FFT(1)
c     
            C1(I,6) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*FFT(3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*FFT(1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*FFT(2)  
C            
            UVAR(I,4) = C1(I,1)   
            UVAR(I,5) = C1(I,2)
            UVAR(I,6) = C1(I,3) 
            UVAR(I,7) = C1(I,4)  
            UVAR(I,8) = C1(I,5)
            UVAR(I,9) = C1(I,6)
          ENDDO
C           
          DO J=1,6
            SV(1:NEL,J) = ZERO
            DO II= 1,NPRONY 
                FAC= -TIMESTEP/TAUX(II) 
                DO I=1,NEL
                 H0(I,J,II) = UVAR(I, 12 + (II - 1)*6 + J)
                 H(I,J,II)  = EXP(FAC)*H0(I,J,II) +
     .               EXP(UNDEMI*FAC)*(C1(I,J) - C0(I,J))
                 UVAR(I,  12 + (II - 1)*6 + J)=  H(I,J,II)
                ENDDO
            ENDDO
C
            DO II = 1,NPRONY
               DO I=1,NEL
                  SV(I,J) = SV(I,J) + GI(II)*H(I,J,II)                              
               ENDDO         
            ENDDO  
          ENDDO     
c
          DO I=1,NEL                        
            INVE = UN/RV(I)                 
            SV(I,1) = SV(I,1)*INVE          
            SV(I,2) = SV(I,2)*INVE          
            SV(I,3) = SV(I,3)*INVE          
            SV(I,4) = SV(I,4)*INVE          
            SV(I,5) = SV(I,5)*INVE          
            SV(I,6) = SV(I,6)*INVE          
C                                           
            SIGNXX(I) = SIGNXX(I) + SV(I,1) 
            SIGNYY(I) = SIGNYY(I) + SV(I,2) 
            SIGNZZ(I) = SIGNZZ(I) + SV(I,3) 
            SIGNXY(I) = SIGNXY(I) + SV(I,4) 
            SIGNYZ(I) = SIGNYZ(I) + SV(I,5) 
            SIGNZX(I) = SIGNZX(I) + SV(I,6)  
          ENDDO
c
        ENDIF  ! ISMSTR   
      ENDIF
C-----------      
      RETURN
      END

