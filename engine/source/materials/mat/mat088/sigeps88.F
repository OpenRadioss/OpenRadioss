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
C  nouvelle routine (Tabulated Ogden material law --
Chd|====================================================================
Chd|  SIGEPS88                      source/materials/mat/mat088/sigeps88.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVEC                       source/materials/mat/mat033/sigeps33.F
Chd|        VALPVECDP                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
       SUBROUTINE SIGEPS88(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   , NGL     ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ISRATE ,
     B      ASRATE , ET     ,IHET     ,OFFG   , EPSTH3, IEXPAN ,
     C      EPSD  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
#include  "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,
     .          ISRATE,IEXPAN
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
     .      ASRATE,OFFG(NEL),EPSTH3(NEL),EPSD(NEL)
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
      INTEGER    I,J,ITENS,NLOAD, NUNLOAD,NE_LOAD,NE_UNLOAD,IUNLOAD,
     .           JJ,J1,J2,IUNL_FOR,KK,NN,K,ICASE,IDIR,
     .           INDX_L(NEL),INDX_UNL(NEL),ITYPE,IFD(3),ITAG(NEL)
      my_real
     .   AV(6,NEL),EV(3,NEL),EE(3,NEL),EDOT(3,NEL),
     .   EVV(3,NEL),DIRPRV(3,3,NEL),ERATE(NEL),RV(NEL),
     .    YFAC(NFUNC),RATE(NFUNC),T1(NEL),T2(NEL),T3(NEL),
     .    EE0(6,NEL),EE1(6,NEL),FG1(6),GMAX(NEL),Fqstat(NEL,3),
     .    DAM(NEL),Funl(3),
     .   DEINT,P,INVR,DF,FAC,D,YFAC_UNL,F01,F02,F03,EPS,EPS1,
     .   RBULK,NU,GS,HYS,SHAPE,F(3),F1,F2,DTINV,
     .   PVISC,DD,BETA,DD1,DD2,DD3,AA,
     .   DE1,DE2,DE3,DE4,DE5,DE6, ERT11,ERT12,ERT13,ERT21,ERT22,ERT23,
     .   ERT31,ERT32,ERT33,DEP1,DEP2,DEP3,DF1,DF2,DF3, XINT,YINT, LAM_TENS,
     .   T_TENS, SCALE(3),ES(3),EVS(3),F0,FSTAR,E2,F3,F_INT,XINC,YINC,
     .   LAM_COMP,T_COMP,E_TENS,E_COMP,YINTL,BB,FC,FT,EC_START,ET_START,
     .   ECURENT,EPSP(3,NEL),EPE,DMAX,SUM,FT1,FT2,FC1,FC2,FCD,FTD,FCS,FTS,
     .   EPSDOLD(NEL),YINT0,YINC0
C----------------------------------------------------------------
         
C material parameters
       RBULK     = UPARAM(1)
       NU        = UPARAM(2)
       GS        = UPARAM(3)
       NLOAD     = INT(UPARAM(4))
       IUNL_FOR  = INT(UPARAM(5))
       HYS       = UPARAM(6) 
       SHAPE     = UPARAM(7)
       ICASE      = NINT(UPARAM(9))
C 
C       
        BETA =  NU
        
C
       DO J=1,NLOAD
         RATE(J) = UPARAM(9 + 2*J - 1 )  
         YFAC(J) = UPARAM(9 + 2*J )
       ENDDO
       YFAC_UNL = UPARAM(9 + 2*NLOAD + 2 )
C for icase= 6       
C **** Itype = 1 only tension
C **** iType = -1 only compression
C **** iType = 2 tension + compression
C
       ITYPE = INT(UPARAM(12 + 2*NLOAD)) ! Not used 
       XINT =  UPARAM(13 + 2*NLOAD)   
       YINT0 =  UPARAM(14 + 2*NLOAD) 
       XINC =  UPARAM(15 + 2*NLOAD)   
       YINC0 =  UPARAM(16 + 2*NLOAD)
C iniialisation des variabesl users 
      IF(TIME == ZERO)THEN
       DO J = 1,NUVAR
        DO  I = 1, NEL
           UVAR(I,J) = ZERO                  
        ENDDO
       ENDDO
       DO  I = 1, NEL
           UVAR(I,22) = ONE  
           UVAR(I,25) = ONE    
           UVAR(I,30) = ZERO  ! -0.99 
           UVAR(I,31) = ZERO  ! ONE            
        ENDDO
      ENDIF   
C 
      DO I=1,NEL
       AV(1,I)=EPSXX(I)
       AV(2,I)=EPSYY(I)
       AV(3,I)=EPSZZ(I)
       AV(4,I)=HALF*EPSXY(I)
       AV(5,I)=HALF*EPSYZ(I)
       AV(6,I)=HALF*EPSZX(I)
      ENDDO       

CEigenvalues needed to be calcultated in double precision
C        for a simple precision executing
      IF (IRESP.EQ.1) THEN
        CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC(AV,EVV,DIRPRV,NEL)
      ENDIF
C-ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
C-ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
C-ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
C-ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT
      IF(ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
        DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
         EV(1,I)=EXP(EVV(1,I))
         EV(2,I)=EXP(EVV(2,I))
         EV(3,I)=EXP(EVV(3,I))
        ENDDO 
      ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
        DO I =1,NEL
        IF(OFFG(I)<=ONE) THEN
          EV(1,I)=SQRT(EVV(1,I)+ ONE)
          EV(2,I)=SQRT(EVV(2,I)+ ONE)
          EV(3,I)=SQRT(EVV(3,I)+ ONE)
	ELSE
         EV(1,I)=EVV(1,I)+ ONE
         EV(2,I)=EVV(2,I)+ ONE
         EV(3,I)=EVV(3,I)+ ONE
        END IF
        ENDDO 
      ELSE
C ----  STRAIN IS ENGINEERING)
        DO I=1,NEL
         EV(1,I)=EVV(1,I) + ONE
         EV(2,I)=EVV(2,I) + ONE
         EV(3,I)=EVV(3,I) + ONE 
        ENDDO 
      ENDIF

      DO I=1,NEL
         EE(1,I) = EV(1,I) - ONE
         EE(2,I) = EV(2,I) - ONE
         EE(3,I) = EV(3,I) - ONE
C from principal to global 
         EE1(1,I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*EE(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*EE(2,I)
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*EE(3,I)
     
         EE1(2,I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*EE(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*EE(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*EE(1,I)
     
         EE1(3,I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*EE(3,I)        
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*EE(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*EE(2,I)
     
         EE1(4,I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*EE(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*EE(2,I)     
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*EE(3,I)
     
         EE1(5,I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*EE(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*EE(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*EE(1,I)
     
         EE1(6,I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*EE(3,I)
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*EE(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*EE(2,I)         
                 
      ENDDO
C Strain rate by principal direction.
      DTINV   =TIMESTEP/MAX(TIMESTEP**2,EM20) 
C 
      DO I= 1,NEL
C         
        EE0(1,I) = UVAR(I,1) 
        EE0(2,I) = UVAR(I,2)
        EE0(3,I) = UVAR(I,3) 
        EE0(4,I) = UVAR(I,4)
        EE0(5,I) = UVAR(I,5) 
        EE0(6,I) = UVAR(I,6) 
C        
        DE1 = EE1(1,I) - EE0(1,I)
        DE2 = EE1(2,I) - EE0(2,I)       
        DE3 = EE1(3,I) - EE0(3,I)
        DE4 = EE1(4,I) - EE0(4,I)       
        DE5 = EE1(5,I) - EE0(5,I)
        DE6 = EE1(6,I) - EE0(6,I)
       
        ERT11 =DIRPRV(1,1,I)*DE1 + DIRPRV(2,1,I)*DE4 + DIRPRV(3,1,I)*DE6
        ERT12 =DIRPRV(1,2,I)*DE1 + DIRPRV(2,2,I)*DE4 + DIRPRV(3,2,I)*DE6
        ERT13 =DIRPRV(1,3,I)*DE1 + DIRPRV(2,3,I)*DE4 + DIRPRV(3,3,I)*DE6
      
        ERT21 =DIRPRV(1,1,I)*DE4 + DIRPRV(2,1,I)*DE2 + DIRPRV(3,1,I)*DE5
        ERT22 =DIRPRV(1,2,I)*DE4 + DIRPRV(2,2,I)*DE2 + DIRPRV(3,2,I)*DE5
        ERT23 =DIRPRV(1,3,I)*DE4 + DIRPRV(2,3,I)*DE2 + DIRPRV(3,3,I)*DE5  
      
        ERT31 =DIRPRV(1,1,I)*DE6 + DIRPRV(2,1,I)*DE5 + DIRPRV(3,1,I)*DE3
        ERT32 =DIRPRV(1,2,I)*DE6 + DIRPRV(2,2,I)*DE5 + DIRPRV(3,2,I)*DE3
        ERT33 =DIRPRV(1,3,I)*DE6 + DIRPRV(2,3,I)*DE5 + DIRPRV(3,3,I)*DE3  
C
        DEP1 = DIRPRV(1,1,I)*ERT11 + DIRPRV(2,1,I)*ERT21 
     .                             + DIRPRV(3,1,I)*ERT31 
        DEP2 = DIRPRV(1,2,I)*ERT12 + DIRPRV(2,2,I)*ERT22 
     .                             + DIRPRV(3,2,I)*ERT32 
        DEP3 = DIRPRV(1,3,I)*ERT13 + DIRPRV(2,3,I)*ERT23 
     .                             + DIRPRV(3,3,I)*ERT33          
C                     
        EDOT(1,I) = ABS(DEP1)*DTINV 
        EDOT(2,I) = ABS(DEP2)*DTINV 
        EDOT(3,I) = ABS(DEP3)*DTINV 
C        
        ERATE(I) = MAX(EDOT(1,I),EDOT(2,I),EDOT(3,I))
        RV(I) = EV(1,I)*EV(2,I)*EV(3,I) 
      ENDDO     
C----THERM STRESS COMPUTATION-----
      IF(IEXPAN > 0.AND.(ISMSTR==10.OR.ISMSTR==11.OR.ISMSTR==12)) THEN
        DO I=1,NEL
           RV(I) = RV(I) - EPSTH3(I)
        ENDDO
      ENDIF
C---------------- 

      
      IF(ISRATE > 0) THEN
        DO I=1,NEL
            EDOT(1,I) = ASRATE*EDOT(1,I) + (ONE - ASRATE)*UVAR(I,7) 
            EDOT(2,I) = ASRATE*EDOT(2,I) + (ONE - ASRATE)*UVAR(I,8) 
            EDOT(3,I) = ASRATE*EDOT(3,I) + (ONE - ASRATE)*UVAR(I,9)
            UVAR(I,7) = EDOT(1,I)
            UVAR(I,8) = EDOT(2,I)
            UVAR(I,9) = EDOT(3,I)
            EPSDOLD(I) = EPSD(I) 
            EPSD(I) = MAX(EDOT(1,I),EDOT(2,I),EDOT(3,I))
            EDOT(1,I) = EPSD(I)
            EDOT(2,I) = EPSD(I)
            EDOT(3,I) = EPSD(I)
        ENDDO
      ELSE
C for outp only      
        DO I=1,NEL
            UVAR(I,7) = EDOT(1,I)
            UVAR(I,8) = EDOT(2,I)
            UVAR(I,9) = EDOT(3,I)
            EPSDOLD(I) = EPSD(I)
            EPSD(I) = MAX(EDOT(1,I),EDOT(2,I),EDOT(3,I))
            EDOT(1,I) = EPSD(I)
            EDOT(2,I) = EPSD(I)
            EDOT(3,I) = EPSD(I)
        ENDDO
      ENDIF
C---------------- 
C----------------
C  compute total energy
C --------------- 
      NE_LOAD = 0
      NE_UNLOAD = 0     
      DO I=1,NEL
          Gmax(I) = GS
C      
          UVAR(I,1) = EE1(1,I)
          UVAR(I,2) = EE1(2,I)
          UVAR(I,3) = EE1(3,I)
          UVAR(I,4) = EE1(4,I)
          UVAR(I,5) = EE1(5,I)
          UVAR(I,6) = EE1(6,I) 
C
C from global frame to principal               
C         
         F(1) = YFAC(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF)       
         F(2) = YFAC(1)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF)        
         F(3) = YFAC(1)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF)
C  C from principal to global 
         FG1(1) =   DIRPRV(1,1,I)*DIRPRV(1,1,I)*F(1)
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*F(2)
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*F(3)
     
         FG1(2) =   DIRPRV(2,2,I)*DIRPRV(2,2,I)*F(2)
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*F(3)
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*F(1)
     
         FG1(3) =   DIRPRV(3,3,I)*DIRPRV(3,3,I)*F(3) 
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*F(1)
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*F(2)
     
         FG1(4) =   DIRPRV(1,1,I)*DIRPRV(2,1,I)*F(1)
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*F(2) 
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*F(3)
     
         FG1(5) =   DIRPRV(2,2,I)*DIRPRV(3,2,I)*F(2)
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*F(3)
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*F(1)
     
         FG1(6) =   DIRPRV(3,3,I)*DIRPRV(1,3,I)*F(3)
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*F(1)
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*F(2)
                                                                            
         DEINT = HALF*((FG1(1) + UVAR(I,10))*(EE1(1,I) - EE0(1,I)) +   
     .                   (FG1(2) + UVAR(I,11))*(EE1(2,I) - EE0(2,I)) +   
     .                   (FG1(3) + UVAR(I,12))*(EE1(3,I) - EE0(3,I)))+
     .                   (FG1(4) + UVAR(I,13))*(EE1(4,I) - EE0(4,I)) +   
     .                   (FG1(5) + UVAR(I,14))*(EE1(5,I) - EE0(5,I)) +   
     .                   (FG1(6) + UVAR(I,15))*(EE1(6,I) - EE0(6,I)) 
C    
          UVAR(I,16) = UVAR(I,16) + DEINT 
          UVAR(I,16) = MAX(UVAR(I,16),EM20)                               
          UVAR(I,17) = MAX(UVAR(I,17), UVAR(I,16))
C          
          UVAR(I,10) = FG1(1)
          UVAR(I,11) = FG1(2)
          UVAR(I,12) = FG1(3)
          UVAR(I,13) = FG1(4)
          UVAR(I,14) = FG1(5)
          UVAR(I,15) = FG1(6)
C
          IF(UVAR(I,19) == -ONE) THEN
            IF(DEINT/UVAR(I,16) >= 1E-7) THEN
!!            IF(DEINT >= ZERO) THEN
              NE_LOAD = NE_LOAD + 1 
              INDX_L(NE_LOAD) = I
              UVAR(I,19) = ONE 
              ITAG(I) = 1 ! re-loading
C loading ....
! the curve must be monotonic  
              IF(ICASE == 6) THEN             
                  LAM_TENS = MAX(ONE,EV(1,I),EV(2,I),EV(3,I))
                  E_TENS = MAX(ZERO,EE(1,I),EE(2,I),EE(3,I))
                  T_TENS= MAX(ZERO,F(1),F(2),F(3))
                  LAM_COMP = MIN(ONE,EV(1,I),EV(2,I),EV(3,I))
                  E_COMP = MIN(ZERO,EE(1,I),EE(2,I),EE(3,I))
                  T_COMP = MIN(ZERO,F(1),F(2),F(3))
                      KK = NLOAD + 1  
                      ES(1)  = E_TENS*UVAR(I,20)
                      EVS(1) = ES(1) + ONE ! tested and it's ok
                      SCALE(1) = UVAR(I,22)*YFAC_UNL
                      IFD(1) = IFUNC(KK) 
                      FT = EVS(1)*SCALE(1)*FINTER(IFD(1),ES(1),NPF,TF,DF1)     
C                                      
                      ES(2)  = E_COMP*UVAR(I,23)
                      EVS(2) = ES(2) + ONE ! tested and it's ok
                      SCALE(2) = UVAR(I,25)*YFAC_UNL
                      FC = EVS(2)*SCALE(2)*FINTER(IFD(1),ES(2),NPF,TF,DF1)    
C                      
                      FAC = ONE
                      T_TENS = T_TENS*LAM_TENS 
                      T_COMP = T_COMP*LAM_COMP  
                      FAC =ONE                                              
                      KK = NLOAD + 1 
                      DO NN=1,20 
                        FAC = FAC*(-ONE/TWO)  
                        DD1 = LAM_TENS**FAC - ONE 
                        T_TENS = T_TENS + 
     .                      (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF) 
                        DD1 = (EVS(1))**FAC - ONE  
                        FT = FT +    
     .                      (DD1 + ONE)*SCALE(1)*FINTER(IFUNC(KK),DD1,NPF,TF,DF)
                        DD1 = LAM_COMP**FAC - ONE 
                        T_COMP = T_COMP +                                           
     .                      (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF) 
                        DD1 = (EVS(2))**FAC - ONE 
                        FC = FC +                                             
     .                      (DD1 + ONE)*SCALE(2)*FINTER(IFUNC(KK),DD1,NPF,TF,DF) 
                      ENDDO  
                       UVAR(I,22) = ONE
                       UVAR(I,25) = ONE 
                       UVAR(I,26) = ZERO
                       UVAR(I,27) = ONE
                       IF(UVAR(I,31) > E_TENS  .AND. E_TENS >= EM3 ) THEN      
                          FAC = FT/T_TENS 
                          AA = (ONE - FAC)/MAX(EM20,(UVAR(I,31) - E_TENS))
                          BB = FAC - AA*E_TENS
                          UVAR(I,26) = AA
                          UVAR(I,27) = BB
                          UVAR(I,22) = AA*E_TENS + BB 
                       ENDIF
C                                             
                       UVAR(I,28) = ZERO
                       UVAR(I,29) = ONE
                       IF(ABS(E_COMP) < ABS(UVAR(I,30)) .AND. ABS(E_COMP)  >= EM03) THEN  
                          FAC = FC/T_COMP 
                          AA = ZERO
                          AA = (ONE - FAC)/(UVAR(I,30) - E_COMP)
                          BB = FAC - AA*E_COMP
                          UVAR(I,25) = AA*E_COMP + BB 
                          UVAR(I,28) = AA
                          UVAR(I,29) = BB
                       ENDIF
                   ENDIF ! icase = 6    
            ELSE
               NE_UNLOAD = NE_UNLOAD + 1
               INDX_UNL(NE_UNLOAD) = I
               UVAR(I,19) = -ONE 
              !! UVAR(I,20) = DEINT 
            ENDIF
         ELSE     
           IF(DEINT/MAX(EM20,UVAR(I,17)) >= ZERO) THEN
               NE_LOAD = NE_LOAD + 1 
               INDX_L(NE_LOAD) = I
               UVAR(I,19) = ONE 
               IF(ICASE == 6) THEN
                      E_TENS = MAX(ZERO,EE(1,I),EE(2,I),EE(3,I))
                      E_COMP = MIN(ZERO,EE(1,I),EE(2,I),EE(3,I))
                      UVAR(I,22) = ONE
                      UVAR(I,25) = ONE
                      IF(E_TENS > EM03) THEN
                        AA = UVAR(I,26)
                        BB = UVAR(I,27)
                        UVAR(I,22) = AA*E_TENS + BB 
                      ENDIF   
C                    
                      IF(ABS(E_COMP) > EM03) THEN 
                        AA = UVAR(I,28)
                        BB = UVAR(I,29) 
                        UVAR(I,25) = AA*E_COMP + BB
                      ENDIF   
                      IF(UVAR(I,25) > ONE .OR. UVAR(I,25) == ZERO) UVAR(I,25) = ONE
                      IF(UVAR(I,22) > ONE .OR. UVAR(I,22) == ZERO) UVAR(I,22) = ONE 
                 ENDIF ! icase = 6    
            ELSE
               NE_UNLOAD = NE_UNLOAD + 1
               INDX_UNL(NE_UNLOAD) = I
               UVAR(I,19) = - ONE 
! the curve must be monotonic               
               LAM_TENS = MAX(ONE,EV(1,I),EV(2,I),EV(3,I))
               E_TENS = MAX(ZERO,EE(1,I),EE(2,I),EE(3,I))
               T_TENS= MAX(ZERO,F(1),F(2),F(3))
               LAM_COMP = MIN(ONE,EV(1,I),EV(2,I),EV(3,I))
               E_COMP   = MIN(ZERO,EE(1,I),EE(2,I),EE(3,I))
               T_COMP   = MIN(ZERO,F(1),F(2),F(3))
               IF(ICASE == 2) THEN
                      J1 = 1
                      DO K=2,NLOAD - 1       
                        IF( EPSDOLD(I) >= RATE(K) )J1 = K 
                      ENDDO    
                      J2 = J1 + 1
                      AA = (EPSDOLD(I) - RATE(J1))/(RATE(J2) - RATE(J1))
                      DF = ZERO            
C
                      FT1 =LAM_TENS*YFAC(J1)*FINTER(IFUNC(J1),E_TENS,NPF,TF,DF1)
                      FT2 =LAM_TENS*YFAC(J2)*FINTER(IFUNC(J2),E_TENS,NPF,TF,DF2)
                      FTS =LAM_TENS*YFAC(1)*FINTER(IFUNC(1),E_TENS,NPF,TF,DF1)
C compression
                      FC1 =LAM_COMP*YFAC(J1)*FINTER(IFUNC(J1),E_COMP,NPF,TF,DF1)
                      FC2 =LAM_COMP*YFAC(J2)*FINTER(IFUNC(J2),E_COMP,NPF,TF,DF2)
                      FCS =LAM_COMP*YFAC(1)*FINTER(IFUNC(1),E_COMP,NPF,TF,DF1)                                        
                      FAC = ONE
                     DO NN=1,20 
                        FAC = FAC*(-ONE/TWO) 
                        DD1 = LAM_TENS**FAC - ONE
                        FT1 =FT1 + (DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
                        FT2 =FT2 + (DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
                        FTS =FTS + (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
                        DD1 = LAM_COMP**FAC - ONE
                        FC1 =FC1 + (DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
                        FC2 =FC2 + (DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
                        FCS =FCS + (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
                     ENDDO ! NN
                     FTD = FT1 + AA*(FT2 - FT1)
                     FCD = FC1 + AA*(FC2 - FC1)
                     UVAR(I,22) = (FTD/FTS - ONE)/E_TENS ! tension
                     UVAR(I,25) = (FCD/FCS - ONE)/E_COMP ! compression
               ELSEIF(ICASE == 6) THEN
                    UVAR(I,22) = ONE
                    UVAR(I,25) = ONE
                    UVAR(I,20) = ONE
                    UVAR(I,21) = ONE
                    UVAR(I,23) = ONE
                    UVAR(I,24) = ONE
                    IF(E_TENS > EM03) THEN
                      UVAR(I,20) = XINT /(LAM_TENS - ONE) 
                      UVAR(I,21) = (XINT + ONE )/LAM_TENS
C                    
                      T_TENS = T_TENS*LAM_TENS 
                      YINT = (XINT + ONE)*YINT0 
                      YINTL = YINT
                      FAC =ONE                                              
                      KK = NLOAD + 1 
                      DO NN=1,20 
                        FAC = FAC*(-ONE/TWO)  
                        DD1 = LAM_TENS**FAC - ONE
                        T_TENS = T_TENS + 
     .                      (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF) 
                        DD1 = (XINT + ONE )**FAC - ONE  
                        YINT = YINT +    
     .                      (DD1 + ONE)*YFAC_UNL*FINTER(IFUNC(KK),DD1,NPF,TF,DF)
                      ENDDO                  
                      UVAR(I,22) = T_TENS/YINT     
                    ENDIF
                   
                    IF(ABS(E_COMP) > EM03) THEN   
                       UVAR(I,23) = XINC /(LAM_COMP - ONE) 
                       UVAR(I,24) = (XINC + ONE )/LAM_COMP
C                    
                       FAC = ONE  
                       T_COMP = T_COMP*LAM_COMP                         
                       YINC= (XINC + ONE)*YINC0                             
                       FAC =ONE                                              
                       KK = NLOAD + 1  
                       DO NN=1,20 
                        FAC = FAC*(-ONE/TWO)                                      
                        DD1 = LAM_COMP**FAC - ONE   
                         T_COMP = T_COMP +                                           
     .                      (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF) 
                        DD1 = (XINC + ONE )**FAC - ONE
                        YINC = YINC +                                             
     .                      (DD1 + ONE)*YFAC_UNL*FINTER(IFUNC(KK),DD1,NPF,TF,DF)  
                       ENDDO                       
                       UVAR(I,25) = T_COMP/YINC
                    ENDIF  
                    UVAR(I,30) = E_COMP
                    UVAR(I,31) = E_TENS
               ENDIF
            ENDIF
          ENDIF  
      ENDDO
C  
      IF(NE_LOAD > 0) THEN 
        IF(NLOAD == 1) THEN
         IF(ICASE == 6) THEN
          DO JJ=1,NE_LOAD
            I = INDX_L(JJ)
            INVR = ONE/MAX(EM20,RV(I))
            SCALE(1) = YFAC(1)*UVAR(I,22)
            IF(EE(1,I) < ZERO) SCALE(1) = YFAC(1)*UVAR(I,25)
            SCALE(2) = YFAC(1)*UVAR(I,22)
            IF(EE(2,I) < ZERO) SCALE(2) = YFAC(1)*UVAR(I,25)
            SCALE(3) = YFAC(1)*UVAR(I,22)
            IF(EE(3,I) < ZERO) SCALE(3) = YFAC(1)*UVAR(I,25)
            F(1) = EV(1,I)*SCALE(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF1) 
            F(2) = EV(2,I)*SCALE(2)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF2)
            F(3) = EV(3,I)*SCALE(3)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF3)
            Gmax(I) = MAX(Gmax(I),SCALE(1)*DF1,SCALE(1)*DF2,
     .                                        SCALE(1)*DF3 ) 
            FAC = ONE
            SUM = ZERO
            DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(1,I)**FAC - ONE
              DD2 = EV(2,I)**FAC - ONE
              DD3 = EV(3,I)**FAC - ONE
              F(1) = F(1) + 
     .              (DD1 + ONE)*SCALE(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
              F(2) = F(2) + 
     .              (DD2 + ONE)*SCALE(2)*FINTER(IFUNC(1),DD2,NPF,TF,DF)
              F(3) = F(3) + 
     .              (DD3 + ONE)*SCALE(3)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
             ENDDO
            P = INVR*RBULK*(RV(I) - ONE)
            T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
            T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
            T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
            UVAR(I,19) = ONE
           ENDDO
          ELSE
          DO JJ=1,NE_LOAD
            I = INDX_L(JJ)
            INVR = ONE/MAX(EM20,RV(I))
            F(1) = EV(1,I)*YFAC(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF1) 
            F(2) = EV(2,I)*YFAC(1)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF2)
            F(3) = EV(3,I)*YFAC(1)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF3)
            Gmax(I) = MAX(Gmax(I),YFAC(1)*DF1,YFAC(1)*DF2,
     .                                        YFAC(1)*DF3 ) 
            FAC = ONE
            DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(1,I)**FAC - ONE
              DD2 = EV(2,I)**FAC - ONE
              DD3 = EV(3,I)**FAC - ONE
              F(1) = F(1) + 
     .              (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
              F(2) = F(2) + 
     .              (DD2 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD2,NPF,TF,DF)
              F(3) = F(3) + 
     .              (DD3 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
             ENDDO
            P = INVR*RBULK*(RV(I) - ONE)
            T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
            T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
            T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
            UVAR(I,19) = ONE
           ENDDO  
          ENDIF ! icase = 6  
        ELSE
         DO JJ=1,NE_LOAD
            I = INDX_L(JJ) 
           DO J=1,3             
              J1 = 1
             DO K=2,NLOAD - 1       
                IF( EDOT(J,I) >= RATE(K) )J1 = K 
             ENDDO    
              J2 = J1 + 1
              AA = (EDOT(J,I) - RATE(J1))/(RATE(J2) - RATE(J1))
              DF = ZERO            
C
              F1 =EV(J,I)*YFAC(J1)*FINTER(IFUNC(J1),EE(J,I),NPF,TF,DF1)
              F2 =EV(J,I)*YFAC(J2)*FINTER(IFUNC(J2),EE(J,I),NPF,TF,DF2)
              DF1 = EV(J,I)*DF1
              DF2 = EV(J,I)*DF2
              DF = MAX(DF, DF1 + AA*(DF2 - DF1))
              Gmax(I) = MAX(Gmax(I),YFAC(J1)*DF1,YFAC(J2)*DF2 )
              FAC = ONE
             DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(J,I)**FAC - ONE
              F1 =F1+(DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
              F2 =F2+(DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
             ENDDO ! NN
               F(J) = F1 + AA*(F2 - F1)
           ENDDO ! J 
              INVR = ONE/MAX(EM20,RV(I))
              P = INVR*RBULK*(RV(I) - ONE)
              T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P
              T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P
              T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
         ENDDO
        ENDIF
      ENDIF ! NE_LOAD 
C
C  unloading 
C             
      IF(NE_UNLOAD > 0 ) THEN
C      
       SELECT CASE (ICASE)
C       
       CASE (1,3)  ! Itens = 0 + follow quasistatic curve
                   ! itens /= 0 and NLOAD = 1        
           DO JJ=1,NE_UNLOAD
             I = INDX_UNL(JJ) 
             F(1) = EV(1,I)*YFAC(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF1) 
             F(2) = EV(2,I)*YFAC(1)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF2)
             F(3) = EV(3,I)*YFAC(1)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF3)
             Gmax(I) = MAX(Gmax(I),YFAC(1)*DF1,YFAC(1)*DF2,
     .                                        YFAC(1)*DF3 ) 
             FAC = ONE
            DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(1,I)**FAC - ONE
              DD2 = EV(2,I)**FAC - ONE
              DD3 = EV(3,I)**FAC - ONE
              F(1) = F(1) + 
     .              (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
              F(2) = F(2) + 
     .              (DD2 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD2,NPF,TF,DF)
              F(3) = F(3) + 
     .              (DD3 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
            ENDDO
            INVR = ONE/MAX(EM20,RV(I))
            P = INVR*RBULK*(RV(I) - ONE)
            T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
            T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
            T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
          ENDDO ! NE_UNLOAD 
C          
       CASE (2) ! itens = 0 + Damage for unloading
C
C  unloading with damage using quasistatic and unloading function
C                                
        IF(IUNL_FOR == 1 ) THEN
         KK = NLOAD + 1                   
         DO JJ=1,NE_UNLOAD
          I = INDX_UNL(JJ)
          DAM(I) = ZERO
          Funl(1)=EV(1,I)*YFAC_UNL*FINTER(IFUNC(KK),EE(1,I),NPF,TF,DF)
          Funl(2)=EV(2,I)*YFAC_UNL*FINTER(IFUNC(KK),EE(2,I),NPF,TF,DF)
          Funl(3)=EV(3,I)*YFAC_UNL*FINTER(IFUNC(KK),EE(3,I),NPF,TF,DF)
C             
          F(1)=EV(1,I)*YFAC(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF)
          F(2)=EV(2,I)*YFAC(1)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF)
          F(3)=EV(3,I)*YFAC(1)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF)
C          
           FAC = ONE
          IF(F(1)/=ZERO) DAM(I) = MAX(DAM(I),Funl(1)/F(1))
          IF(F(2)/=ZERO) DAM(I) = MAX(DAM(I),Funl(2)/F(2))
          IF(F(3)/=ZERO) DAM(I) = MAX(DAM(I),Funl(3)/F(3))
          DAM(I) = MIN(DAM(I),ONE)
         ENDDO
        ELSEIF(IUNL_FOR == 2 .OR. IUNL_FOR == 3) THEN
C
C  unloading with damage based on the energy absorption.
C   
         DO JJ=1,NE_UNLOAD
            I = INDX_UNL(JJ)
            DAM(I) = ZERO
            DAM(I) = ONE - (UVAR(I,16)/MAX(EM20,UVAR(I,17)))**SHAPE
            DAM(I) = ONE - (ONE - HYS)*DAM(I)
          ENDDO  
        ENDIF
C
!!        IF(NLOAD == 1) THEN ! 
!!unlaoding is only quasistatic to oviod the instability i unloading
          IF(IUNL_FOR == 3) THEN
           DO JJ=1,NE_UNLOAD
             I = INDX_UNL(JJ)
C                           
                 IF(EE(1,I) >= ZERO) THEN
                   SCALE(1) = (UVAR(I,22)* EE(1,I) + ONE)*YFAC(1)
                 ELSE
                   SCALE(1) = (UVAR(I,25)* EE(1,I) + ONE)*YFAC(1) 
                 ENDIF 
                 IF(EE(2,I)>= ZERO) THEN
                   SCALE(2) = (UVAR(I,22)* EE(2,I) + ONE)*YFAC(1)
                 ELSE
                   SCALE(2) = (UVAR(I,25)* EE(2,I) + ONE)*YFAC(1) 
                 ENDIF 
                 IF(EE(3,I) >= ZERO) THEN
                   SCALE(3) = (UVAR(I,22)* EE(3,I) + ONE)*YFAC(1)
                 ELSE
                   SCALE(3) = (UVAR(I,25)* EE(3,I) + ONE)*YFAC(1) 
                 ENDIF 
C                
                F(1) = EV(1,I)*SCALE(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF1) 
                F(2) = EV(2,I)*SCALE(2)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF2)
                F(3) = EV(3,I)*SCALE(3)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF3)
                
                Gmax(I) = MAX(Gmax(I),SCALE(1)*DF1,SCALE(2)*DF2,
     .                                            SCALE(3)*DF3 ) ! 
                 FAC = ONE
                DO NN=1,20 
                  FAC = FAC*(-ONE/TWO) 
                  DD1 = EV(1,I)**FAC - ONE
                  DD2 = EV(2,I)**FAC - ONE
                  DD3 = EV(3,I)**FAC - ONE   
                  F(1) = F(1) + 
     .                  (DD1 + ONE)*SCALE(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
                  F(2) = F(2) + 
     .                  (DD2 + ONE)*SCALE(2)*FINTER(IFUNC(1),DD2,NPF,TF,DF)
                  F(3) = F(3) + 
     .                  (DD3 + ONE)*SCALE(3)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
                ENDDO
                INVR = ONE/MAX(EM20,RV(I))
                P = INVR*RBULK*(RV(I) - ONE)
                T1(I) = INVR*DAM(I)*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
                T2(I) = INVR*DAM(I)*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
                T3(I) = INVR*DAM(I)*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
                UVAR(I,18) = DAM(I)
               ENDDO ! NE_UNLOAD 
          ELSE
           DO JJ=1,NE_UNLOAD
             I = INDX_UNL(JJ)
             F(1)=EV(1,I)*YFAC(1)*FINTER(IFUNC(1),EE(1,I),NPF,TF,DF)
             F(2)=EV(2,I)*YFAC(1)*FINTER(IFUNC(1),EE(2,I),NPF,TF,DF)
             F(3)=EV(3,I)*YFAC(1)*FINTER(IFUNC(1),EE(3,I),NPF,TF,DF)
             FAC = ONE
             DO NN=1,20
                FAC = (-ONE/TWO)*FAC
                DD1 = EV(1,I)**FAC - ONE
                DD2 = EV(2,I)**FAC - ONE
                DD3 = EV(3,I)**FAC - ONE
                F(1) = F(1) + 
     .          (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
                F(2) = F(2) + 
     .          (DD2 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD2,NPF,TF,DF)
                F(3) = F(3) + 
     .          (DD3 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
             ENDDO
             INVR = ONE/MAX(EM20,RV(I))
             P = INVR*RBULK*(RV(I) - ONE)
             T1(I) = INVR*DAM(I)*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
             T2(I) = INVR*DAM(I)*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
             T3(I) = INVR*DAM(I)*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P
             UVAR(I,18) = DAM(I)
           ENDDO   
          ENDIF !  IUNL_FOR
           
!!leading to instability in unloading (this part is removed )
!!         ELSE
!!           DO JJ=1,NE_UNLOAD
!!            I = INDX_UNL(JJ) 
!!             DO J=1,3             
!!                J1 = 1
!!                DO K=2,NLOAD - 1       
!!                    IF( EDOT(J,I) >= RATE(K) )J1 = K 
!!                ENDDO 
!!               J2 = J1 + 1
!!               AA = (EDOT(J,I) - RATE(J1))/(RATE(J2) - RATE(J1))
!!               DF = ZERO            
!!
!!               F1 =EV(J,I)*YFAC(J1)*FINTER(IFUNC(J1),EE(J,I),NPF,TF,DF1)
!!               F2 =EV(J,I)*YFAC(J2)*FINTER(IFUNC(J2),EE(J,I),NPF,TF,DF2)
!!               DF1 = EV(J,I)*DF1
!!      DF2 = EV(J,I)*DF2
!!      DF = MAX(DF, DF1 + AA*(DF2 - DF1))
!!               Gmax(I) = MAX(Gmax(I),YFAC(J1)*DF1,YFAC(J2)*DF2 )
!!               FAC = ONE

!!               FAC = FAC*(-ONE/TWO) 
!!               DD1 = EV(J,I)**FAC - ONE
!!               F1 =F1+(DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
!!               F2 =F2+(DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
!!              ENDDO ! NN
!!               F(J) = F1 + AA*(F2 - F1)
!!             ENDDO ! J 
!!             INVR = ONE/MAX(EM20,RV(I))
!!              P = INVR*RBULK*(RV(I) - ONE)
!!              T1(I) = INVR*DAM(I)*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P
!!              T2(I) = INVR*DAM(I)*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P
!!!             T3(I) = INVR*DAM(I)*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P
!!              UVAR(I,18) = DAM(I)
!!         ENDDO 
!!        ENDIF ! NLOAD    
C        
       CASE (4) ! itens = 1 and nload > 1
          DO JJ=1,NE_UNLOAD
           I = INDX_UNL(JJ) 
           DO J=1,3             
             J1 = 1
             DO K=2,NLOAD - 1       
                IF( EDOT(J,I) >= RATE(K) )J1 = K 
             ENDDO    
              J2 = J1 + 1
              AA = (EDOT(J,I) - RATE(J1))/(RATE(J2) - RATE(J1))
              DF = ZERO            
C
              F1 =EV(J,I)*YFAC(J1)*FINTER(IFUNC(J1),EE(J,I),NPF,TF,DF1)
              F2 =EV(J,I)*YFAC(J2)*FINTER(IFUNC(J2),EE(J,I),NPF,TF,DF2)
              DF1 = EV(J,I)*DF1
              DF2 = EV(J,I)*DF2
              DF = MAX(DF, DF1 + AA*(DF2 - DF1))
              Gmax(I) = MAX(Gmax(I),YFAC(J1)*DF1,YFAC(J2)*DF2 )
              FAC = ONE
             DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(J,I)**FAC - ONE
              F1 =F1+(DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
              F2 =F2+(DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
             ENDDO ! NN
               F(J) = F1 + AA*(F2 - F1)
           ENDDO ! J
            INVR = ONE/MAX(EM20,RV(I))
            P = INVR*RBULK*(RV(I) - ONE)
            T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P
            T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P
            T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
         ENDDO   
       CASE (5) ! itens = -1 and nload > 1
          DO JJ=1,NE_UNLOAD
           I = INDX_UNL(JJ) 
           DO J=1,3             
            J1 = 1
            IF(EE(J,I) < 0) THEN 
             DO K=2,NLOAD - 1       
                IF( EDOT(J,I) >= RATE(K) )J1 = K 
             ENDDO    
              J2 = J1 + 1
              AA = (EDOT(J,I) - RATE(J1))/(RATE(J2) - RATE(J1))
              DF = ZERO            
C
              F1 =EV(J,I)*YFAC(J1)*FINTER(IFUNC(J1),EE(J,I),NPF,TF,DF1)
              F2 =EV(J,I)*YFAC(J2)*FINTER(IFUNC(J2),EE(J,I),NPF,TF,DF2)
              DF1 = EV(J,I)*DF1
              DF2 = EV(J,I)*DF2
              DF = MAX(DF, DF1 + AA*(DF2 - DF1))
              Gmax(I) = MAX(Gmax(I),YFAC(J1)*DF1,YFAC(J2)*DF2 )
              FAC = ONE
             DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(J,I)**FAC - ONE
              F1 =F1+(DD1 + ONE)*YFAC(J1)*FINTER(IFUNC(J1),DD1,NPF,TF,DF)
              F2 =F2+(DD1 + ONE)*YFAC(J2)*FINTER(IFUNC(J2),DD1,NPF,TF,DF)
             ENDDO ! NN
               F(J) = F1 + AA*(F2 - F1)
           ELSE ! tension
             F(J) = EV(J,I)*YFAC(1)*FINTER(IFUNC(1),EE(J,I),NPF,TF,DF1)
             FAC = ONE
             DO NN=1,20 
              FAC = FAC*(-ONE/TWO) 
              DD1 = EV(J,I)**FAC - ONE
              F(J) = F(J) + 
     .              (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
             ENDDO 
            ENDIF    
           ENDDO ! J
            INVR = ONE/MAX(EM20,RV(I))
            P = INVR*RBULK*(RV(I) - ONE)
            T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P
            T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P
            T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P 
         ENDDO
          CASE (6)  ! Itens = 0 + follow unloading curve
             KK = NLOAD + 1
             DO JJ=1,NE_UNLOAD
                 I = INDX_UNL(JJ)       
C       
                   SCALE(1) = YFAC(1)
                   SCALE(2) = YFAC(1)
                   SCALE(3) = YFAC(1)
C                   
                   IFD(1) = IFUNC(1)
                   IFD(2) = IFUNC(1)
                   IFD(3) = IFUNC(1)
C                   
                   ES(1) = EE(1,I)
                   ES(2) = EE(2,I)
                   ES(3) = EE(3,I)
C                  
                   EVS(1) = EV(1,I)
                   EVS(2) = EV(2,I)
                   EVS(3) = EV(3,I)
C                           
                 IF(ES(1) >= ZERO) THEN
                   ES(1)    = ES(1)*UVAR(I,20)
                   !!EVS(1)   = EVS(1)*UVAR(I,21)
                   EVS(1)   = ES(1) + ONE
                   SCALE(1) = UVAR(I,22)*YFAC_UNL
                   IFD(1)   = IFUNC(KK)
                 ELSE
                    ES(1) = MAX(-ONE,ES(1)*UVAR(I,23))
                   !! EVS(1)= EVS(1)*UVAR(I,24)
                    EVS(1)   = ES(1) + ONE
                    SCALE(1) = UVAR(I,25)*YFAC_UNL
                    IFD(1) = IFUNC(KK)  
                 ENDIF 
                 IF(ES(2) >= ZERO) THEN
                   ES(2)    = ES(2)*UVAR(I,20)
                   !!EVS(2)   = EVS(2)*UVAR(I,21)
                   EVS(2)   = ES(2) + ONE
                   SCALE(2) = UVAR(I,22)*YFAC_UNL
                   IFD(2)   = IFUNC(KK)
                 ELSE
                     ES(2) = MAX(-ONE,ES(2)*UVAR(I,23))
                     !!EVS(2)= EVS(2)*UVAR(I,24)
                     EVS(2)   = ES(2) + ONE
                     SCALE(2) = UVAR(I,25)*YFAC_UNL
                     IFD(2) = IFUNC(KK)                
                 ENDIF 
                 IF(ES(3) > ZERO) THEN
                   ES(3)    = ES(3)*UVAR(I,20)
                   EVS(3)   = EVS(3)*UVAR(I,21)
                   EVS(3)   = ES(3) + ONE
                   SCALE(3) = UVAR(I,22)*YFAC_UNL
                   IFD(3)   = IFUNC(KK)
                 ELSE
                   ES(3) = MAX(-ONE,ES(3)*UVAR(I,23))
                   !!EVS(3)= EVS(3)*UVAR(I,24)
                   EVS(3)   = ES(3) + ONE
                   SCALE(3) = UVAR(I,25)*YFAC_UNL
                   IFD(3) = IFUNC(KK)  
                 ENDIF             
C                
                F(1) = EVS(1)*SCALE(1)*FINTER(IFD(1),ES(1),NPF,TF,DF1) 
                F(2) = EVS(2)*SCALE(2)*FINTER(IFD(2),ES(2),NPF,TF,DF2)
                F(3) = EVS(3)*SCALE(3)*FINTER(IFD(3),ES(3),NPF,TF,DF3)
                
                 Gmax(I) = MAX(Gmax(I),SCALE(1)*DF1,SCALE(2)*DF2,
     .                                            SCALE(3)*DF3 ) ! 
                 FAC = ONE
                 DO NN=1,20 
                  FAC = FAC*(-ONE/TWO) 
                  DD1 = EVS(1)**FAC - ONE
                  DD2 = EVS(2)**FAC - ONE
                  DD3 = EVS(3)**FAC - ONE 
                  F(1) = F(1) + 
     .                  (DD1 + ONE)*SCALE(1)*FINTER(IFD(1),DD1,NPF,TF,DF)
                  F(2) = F(2) + 
     .                  (DD2 + ONE)*SCALE(2)*FINTER(IFD(2),DD2,NPF,TF,DF)
                  F(3) = F(3) + 
     .                  (DD3 + ONE)*SCALE(3)*FINTER(IFD(3),DD3,NPF,TF,DF)
                ENDDO
                INVR = ONE/MAX(EM20,RV(I))
                P = INVR*RBULK*(RV(I) - ONE)
                T1(I) = INVR*(TWO_THIRD*F(1) - THIRD*(F(2) + F(3))) + P 
                T2(I) = INVR*(TWO_THIRD*F(2) - THIRD*(F(1) + F(3))) + P 
                T3(I) = INVR*(TWO_THIRD*F(3) - THIRD*(F(1) + F(2))) + P
               ENDDO ! NE_UNLOAD 
       END SELECT 
      ENDIF !  NE_UNLOAD
C
C cauchy to glabale
C
      DO I=1,NEL
        SIGNXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*T1(I)
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*T2(I)
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*T3(I)
     
        SIGNYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*T2(I)
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*T3(I)
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*T1(I)
     
        SIGNZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*T3(I)        
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*T1(I)
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*T2(I)
     
        SIGNXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*T1(I)
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*T2(I)     
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*T3(I)
     
        SIGNYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*T2(I)
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*T3(I)
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*T1(I)
     
        SIGNZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*T3(I)
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*T1(I)
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*T2(I)
C
* SET SOUND SPEED
         SOUNDSP(I)=SQRT((FOUR_OVER_3*GS + RBULK)/RHO(I))
* SET VISCOSITY
         VISCMAX(I) = ZERO
C        
       ENDDO
C   
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
       DO I=1,NEL
         ET(I)= MAX(ONE,Gmax(I)/GS)
       ENDDO
      ENDIF
      RETURN
      END

