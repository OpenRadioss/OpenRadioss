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
Chd|  SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        KIRKALDYKINETICS              source/materials/mat/mat080/sigeps80.F
Chd|        PHASEKINETIC2                 source/materials/mat/mat080/sigeps80.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS80C(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0   ,IPT     ,IFLAG   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ ,SIGOZX  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ ,SIGNZX  ,
     9     SIGVXX ,SIGVYY  ,SIGVXY ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,PM      ,IPM     ,MAT    ,
     C     ETSE   ,GS     ,VOL     ,YLD    , TEMP   ,
     D     DIE    ,COEF   , SHF    ,EPSP    ,TABLE  ,
     E     ITHK   ,NVARTMP , VARTMP,EPSTHTOT) 

C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL0 F
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT0    |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL0    | F | R | INITIAL DENSITY
C AREA    | NEL0    | F | R | AREA
C EINT    | 2*NEL0  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL0    | F | R | LAYER THICKNESS
C EPSPXX  | NEL0    | F | R | STRAIN RATE XX
C EPSPYY  | NEL0    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL0    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL0    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL0    | F | R | STRAIN XX
C EPSYY   | NEL0    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL0    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL0    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL0    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL0    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL0    | F |R/W| THICKNESS
C PLA     | NEL0    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL0*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NVARTMP, NPT0, IPT,IFLAG(*),
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*),ITHK
      my_real TIME,TIMESTEP,UPARAM(*),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   PM(NPROPM,*),GS(*),VOL(NEL) ,TEMP(NEL),
     .   DIE(NEL),COEF(NEL), SHF(NEL) ,EPSTHTOT (NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL,NUVAR),EPSP(NEL),
     .   OFF(NEL),THK(NEL),YLD(NEL)
      INTEGER :: VARTMP(NEL,NVARTMP)

      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,ITER,NITER,I,J,K,NRATE(NEL),J1,J2,N,NINDX,IADBUF,NINDXGLOB,NINDXLOC,
     .        NMAX,INDEX(NEL),K2,ITERK2,NPFG(2),ITABLE(5),
     .        EFUNC,IFUNC(5),ITERK,FLAG,NTAB,HEATFLAG,HEATOPTION,
     .        NIHEAT,NICOOL,FLAG_LOC ,LOCAL,FLAG_TR_STRAIN,FLAG_TR_KINETICS
      INTEGER, DIMENSION(NEL)   :: INDXLOC,INDXGLOB

      my_real
     .        CUN ,CDEUX,CTROIS,EFAC,
     .        ALAMBDA,BLAMBDA,CEPS,PEPS,
     .        YSCALE(5),TETA2,TETA3,TETA4,TETA5,QR2,QR3,QR4,PP,
     .        ALFA1,ALFA2,T2,T3,T4,T5,T1,VOLI,XFAC(5),RSCALE(5),
     .        ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,FG,
     .        RHNEW,FGRAIN,KPER,HFCTN,KBAIN,XEQ2,
     .        XEQ4,LAT1,LAT2,HFP,HB,HM,VOL0,TINI,UNITT,
     .        TGZ(12), HEATFLAG1,
     .        CONV,HUITCENT,SSPSHELL,SSPSOL,QPTT,SLOPE,
     .         TAU1, TAU3,DXRHO,DYDXR,NU_1_NU,DSVM,DEN,FAC,
     .         GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .         GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .         NORMXX,NORMYY,NORMXY,NORMZZ,DDLAM,DFDSIG_DSIG,DPLA_DLAM,DDEP
      my_real
     .        E(NEL),RBULK(NEL),G2(NEL),DETH12(NEL),
     .        LAMDA(NEL),DYDXGZ(NEL),DYDXE(NEL),
     .        FRAC1(NEL),FRAC2(NEL),FRAC3(NEL)  ,FRAC4(NEL), 
     .        FRAC5(NEL),TOTFRAC(NEL),TOTFRACOLD(NEL) ,SOXX(NEL) ,
     .        GZ(NEL)   ,TREO(NEL)   ,SIGOM(NEL),SOYY(NEL),SOZZ(NEL),
     .        DYDX1(NEL),DYDX2(NEL)  ,DYDX3(NEL),
     .        DYDX4(NEL),DYDX5(NEL)  ,YIELD(NEL),
     .        Y1(NEL)   ,Y2(NEL),Y3(NEL),Y4(NEL),Y5(NEL),SVM(NEL),
     .        EOXX(NEL),EOYY(NEL),EOZZ(NEL),
     .        EOXY(NEL),EOYZ(NEL),EOZX(NEL),
     .        Y1INI(NEL),
     .        EPSOXX(NEL),EPSOYY(NEL),EPSOZZ(NEL),EPSOXY(NEL),
     .        DEPSZZ(NEL),EPSZZ(NEL),
     .        DPLA(NEL)  ,SNORM(NEL),SEFF(NEL),SIGOZZ(NEL),
     .        SIGNZZ(NEL),SIGM(NEL)  ,SXX(NEL),SYY(NEL),SZZ(NEL),
     .        DEPLXX(NEL),
     .        DEPLYY(NEL),DEPLZZ(NEL),DEPLXY(NEL),
     .        DPLA1(NEL),DPLA2(NEL),DPLA3(NEL),DPLA4(NEL),DPLA5(NEL),
     .        PLA1(NEL) ,PLA2(NEL) ,PLA3(NEL) ,PLA4(NEL) ,PLA5(NEL),
     .        DEPSELZZ(NEL),DEPSPLZZ(NEL),DEPSIM1(NEL),DEPSI(NEL),
     .        DEPSIP1(NEL),SIGZIM1(NEL),SIGI(NEL),
     .        VOLOLD(NEL),RH(NEL),DE12(NEL),YLDOLD(NEL),
     .        DEPSELOZZ(NEL),DEPSPLOZZ(NEL), 
     .        SVMI(NEL),TRDEPSTH(NEL),SVMIM1(NEL),SVMTR(NEL),TRDEPS,
     .        DEPSTHXX(NEL),DEPSTHYY(NEL),DEPSTHZZ(NEL),DEPSTH(NEL),
     .        X1(NEL),X2(NEL),X3(NEL),X4(NEL),X5(NEL),HARD(NEL),
     .        GOLD(NEL),SVMtest(NEL),
     .        EPLXX(NEL),EPLYY(NEL),EPLZZ(NEL) ,EPLXY(NEL),
     .        EELOXX(NEL),EELOYY(NEL),DEPSTR(NEL) ,
     .        EELOXY(NEL),EELOZZ(NEL),NORMDEV(NEL) ,
     .        XGAMA(NEL),TEMPMIN(NEL),VR(NEL),H(NEL),TEMPO(NEL),
     .        ZEQ(NEL), TAU(NEL),
     .        RHO_A(NEL),RHO_F(NEL),RHO_P(NEL),RHO_B(NEL),RHO_M(NEL),RHO_N(NEL)
      my_real, DIMENSION(NEL) ::
     .        PSI2 ,PSI3,PSI4,PSI5,PHI2,PHI3,PHI4 ,PHI5  ,FACCS,TEMPEL,
     .        LNF2,LNF3,LNF4,LNF5,A1, A2,G,P,FCT,DF,DPXX,DPYY,DPXY,DPZZ,
     .        SVMO,DEPSVOL,DEXX,DEYY,DEZZ,HFCT,A,B,C,DH,
     .        GSIG,DGSIG,ESF,DESF,R,POLD,SXY,LOGZ,LOGZM1
C
      my_real,
     .        DIMENSION(NEL,3) :: XX5,XX1,XX2,XX3,XX4

      INTEGER,DIMENSION(NEL,3) :: IPOS1,IPOS2,IPOS3,IPOS4,IPOS5
c-----------------------------------------------
c-----------------------------------------------
c-----------------------------------------------
c-----------------------------------------------
      NTAB = IPM(226,MAT(1))  
      NITER = 3
      DO I=1,NTAB
        ITABLE(I) = IPM(226+I,MAT(1)) 
        XFAC(I)   = UPARAM(58+I)
      ENDDO


      DO I=1,NEL
       E(I)   =  UPARAM(1)  
      ENDDO
      DO J=1,5                           
        YSCALE(J)=UPARAM(9+J)         
      ENDDO    
          
      NU     =  UPARAM(2)             
      EFAC   =  UPARAM(4)              
      CEPS   =  UPARAM(15)     
      PEPS   =  UPARAM(16)     
      TETA2  =  UPARAM(17)     
      TETA3  =  UPARAM(18)     
      TETA4  =  UPARAM(19)     
      TETA5  =  UPARAM(20)     
      QR2    =  UPARAM(21)     
      QR3    =  UPARAM(22)     
      QR4    =  UPARAM(23)     
      ALPHA  =  UPARAM(24)     
      TREF   =  UPARAM(25)     
      AE1    =  UPARAM(26)   
      AE3    =  UPARAM(27) 
   
      BS     =  UPARAM(28)     
      MS     =  UPARAM(29)     
      GSIZE  =  UPARAM(30)
      NU_1_NU=  NU/(ONE-NU)

      IF(IDT_THERM == 1) THEN ! ignore thermal expansion
        ALFA1  =  ZERO     
        ALFA2  =  ZERO
      ELSE
        ALFA1  =  UPARAM(31)     
        ALFA2  =  UPARAM(32) 
      ENDIF 
      ! computed in starter    
      FCFER  =  UPARAM(33)
      FCPER  =  UPARAM(34)
      FCBAI  =  UPARAM(35)
      FGRAIN =  UPARAM(36)
      KPER   =  UPARAM(37)
      KBAIN  =  UPARAM(38)
      XEQ2   =  UPARAM(39)     
      LAT1   =  UPARAM(40)
      LAT2   =  UPARAM(41)
      HFP   =  UPARAM(42)     
      HB    =  UPARAM(43)
      HM    =  UPARAM(44)
      TINI  =  UPARAM(45) 
      UNITT =  UPARAM(46)
C      
      GFAC_F =  UPARAM(75)
      PHI_F  =  UPARAM(76)
      PSI_F  =  UPARAM(77)
      CR_F   =  UPARAM(78)

      GFAC_P =  UPARAM(79)
      PHI_P  =  UPARAM(80)
      PSI_P  =  UPARAM(81)
      CR_P   =  UPARAM(82)

      GFAC_B =  UPARAM(83)
      PHI_B  =  UPARAM(84)
      PSI_B  =  UPARAM(85)
      CR_B   =  UPARAM(86)

      PHI_M  =  UPARAM(84)
      PSI_M  =  UPARAM(85)
      N_M    =  UPARAM(86)

      FGFER = UPARAM(87)   
      FGPER = UPARAM(88)   
      FGBAI = UPARAM(89)  
C
      CF = UPARAM(90)
      CP = UPARAM(91)  
      CB = UPARAM(92)  
C
      DO J=46+1,58           
        TGZ(J-58+12)=UPARAM(J)   !GZ FUNCTION 
      ENDDO 
      
      HEATOPTION = UPARAM(58 + NTAB + 1)
      TAU1       = UPARAM(58 + NTAB + 2)  
      TAU3       = UPARAM(58 + NTAB + 3) 
      FLAG_LOC   = UPARAM(58 + NTAB + 4) 

      FLAG_TR_STRAIN    = UPARAM(58 + NTAB + 5) 
      FLAG_TR_KINETICS  = UPARAM(58 + NTAB + 6)  
      RSCALE(1) = UPARAM(58 + NTAB + 7)  
      RSCALE(2) = UPARAM(58 + NTAB + 8)  
      RSCALE(3) = UPARAM(58 + NTAB + 9)  
      RSCALE(4) = UPARAM(58 + NTAB +10)  
      RSCALE(5) = UPARAM(58 + NTAB +11)  

      HEATFLAG = HEATOPTION
      IF(HEATOPTION == 2) THEN
        HEATFLAG1 = FINTER(KFUNC(2),TIME,NPF,TF,SLOPE)
        HEATFLAG = INT(HEATFLAG1)
      ENDIF

c
      HUITCENT= EIGHT*EP02
      QPTT=FOUR+THREE*(EM01+EM02)
      NPFG(1)=1
      NPFG(2)=13                            
      ITERK  =5
      ITERK2 =5
      CUN = THIRD/(ONE-TWO*NU)
      CDEUX = HALF/(ONE+NU)
      CTROIS = NU/(ONE+NU)/(ONE-TWO*NU)     

      IF (ISIGI == 0) THEN   
        IF (TIME == ZERO)THEN 
          DO I=1,NEL        
            UVAR(I,35)=VOL(I) 
            UVAR(I,43)=RHO0(I)
            IF(HEATFLAG == 1)THEN
               UVAR(I,2) = ZERO!  ! fraction of austenite
               UVAR(I,3) = ONE !  ! fraction of ferrite
            ELSE
               UVAR(I,2) = ONE! fraction of austenite
            ENDIF
            UVAR(I,8)   = UPARAM(45) !temperature 
            UVAR(I,1)   = UPARAM(45) !TINI
            UVAR(I,28)  = ONE    !ETSE
            IF(JTHE==-1) UVAR(I,8)  =TEMP(I)
            !UVAR(I,9)=FINTER(KFUNC(2),ZERO,NPF,TF,DYDX1(I))!  initial yield
            XX1(I,1)=ZERO
            XX1(I,2)=UVAR(I,8) ! temp initial
            XX1(I,3)=ZERO
            IPOS1(I,1) = 0
            IPOS1(I,2) = 0
            IPOS1(I,3) = 0 
          ENDDO 
          CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL,IPOS1,XX1,Y1,DYDX1)!  initial yield
          DO I=1,NEL        
           UVAR(I,9)= Y1 (I)
          ENDDO 
        ENDIF                
      ENDIF
      IF(JTHE /= 0 ) THEN
         DO I=1,NEL     
           TEMPEL(I) = TEMP(I)
         ENDDO
      ELSE
         !RHOCP=UPARAM(21)
         DO I=1,NEL
           TEMPEL(I) = TINI
         ENDDO
      ENDIF
c-------------------------------------
      DO I=1,NEL
        TEMPO(I)   = UVAR(I,8)
        TEMPMIN(I) = UVAR(I,1) ! ASSURE PHASE CHANGE OCCURS ONLY IF DECREASING (WHEN OSCILLATING NO PHASE CHANGE)
      ENDDO
c-------------------------------------
      IF(KFUNC(1) > ZERO)THEN
        DO I=1,NEL
         E(I) = FINTER(KFUNC(1),TEMPEL(I),NPF,TF,DYDXE(I))
         E(I)=  E(I) *  EFAC  
        ENDDO
      ENDIF
c-------------------------------------
      DO I=1,NEL
        A1(I)       = E(I)/(ONE-NU*NU)     ! Plane stress elastic matrix diagonal component
        A2(I)       = NU*A1(I)             ! Plane stress elastic matrix non-diagonal component
        G(I)        = E(I)*HALF/(ONE+NU)   ! Shear modulus
      ENDDO
c-------------------------------------
      DO I=1,NEL
        X2(I)=  UVAR(I,23)
        X3(I)=  UVAR(I,24)
        X4(I)=  UVAR(I,25)
        X5(I)=  UVAR(I,44)
        FRAC1(I)= UVAR(I,2) 
        FRAC2(I)= UVAR(I,3) 
        FRAC3(I)= UVAR(I,4) 
        FRAC4(I)= UVAR(I,5) 
        FRAC5(I)= UVAR(I,6) 
        TOTFRACOLD(I) = FRAC2(I)+FRAC3(I)+FRAC4(I)+FRAC5(I) ! Z sum of product phase
        PLA1(I)= UVAR(I,17)
        PLA2(I)= UVAR(I,18) 
        PLA3(I)= UVAR(I,19) 
        PLA4(I)= UVAR(I,20)
        PLA5(I)= UVAR(I,21) 
        GOLD(I)= UVAR(I,36)
        XGAMA(I)= UVAR(I,10)
        DPLA(I) = ZERO        
        DPLA1(I)= ZERO                           
        DPLA2(I)= ZERO                          
        DPLA3(I)= ZERO        
        DPLA4(I)= ZERO                           
        DPLA5(I)= ZERO
        PHI2(I) = ZERO
        PSI2(I) = ZERO
        PHI3(I) = ZERO
        PSI3(I) = ZERO
        PHI4(I) = ZERO
        PSI4(I) = ZERO
        PHI5(I) = ZERO
        PSI5(I) = ZERO
      ENDDO
      IF (HEATFLAG == 1) THEN
       TETA2  = ZERO 
       TETA3  = ZERO       
       TETA4  = ZERO       
       TETA5  = ZERO     
      ENDIF 
c-------------------------------------------------        
c Phase transformation during cooling or heating
c-------------------------------------------------
      NICOOL = 0  
      IF(FLAG_LOC == 2) THEN   ! global treatment - same behavior per part 
       IF (HEATFLAG == 1) THEN ! heating activated AE1<AE3
        DO I=1,NEL
         IF(TEMPEL(I)>= UVAR(I,8).AND.FRAC2(I)>ZERO)THEN
          ZEQ(I)   =  (TEMPEL(I)-AE1)/(AE3-AE1)
          TAU(I)   =  TAU1 + ZEQ(I) * ( TAU3 - TAU1)
          ZEQ(I)   =  MIN ( ONE  ,MAX ( ZERO, ZEQ(I)))
          TAU(I)   =  MAX ( TAU3,MIN ( TAU1, TAU(I)))
          FRAC1(I) = UVAR(I,2) + MAX(ZERO,(ZEQ(I) - UVAR(I,2)) ) * TIMESTEP*THEACCFACT / TAU(I)
          FRAC2(I) = ONE - FRAC1(I) - FRAC3(I)- FRAC4(I)- FRAC5(I)
          TEMPMIN(I) = UVAR(I,8)! heating activated AE1<AE3
         ENDIF
        ENDDO  
       ELSE ! HEATFLAG /= 1
        DO I=1,NEL
          NICOOL = NICOOL + 1
          INDEX(NICOOL)=I
          IF (TEMPEL(I)<= 1073.0 .AND. UVAR(I,26)==ZERO)THEN !USED TO COMPUTE hardness 
            UVAR(I,26)=TIME
            UVAR(I,27)=TEMPEL(I)
          ENDIF
          IF (TEMPEL(I)<= 773.0 .AND. UVAR(I,16)==ZERO)THEN  !USED TO COMPUTE hardness 
            UVAR(I,16)=TIME
            UVAR(I,33)=TEMPEL(I)
          ENDIF
        ENDDO        
        IF (FLAG_TR_KINETICS == 2) THEN
         CALL PHASEKINETIC2(NEL,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .     QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ELSE
         CALL KIRKALDYKINETICS(NEL,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .    FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,QR2,
     .    QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ENDIF     
       ENDIF
      ELSE !FLAG_LOC /= 2 local 
       DO I=1,NEL
        IF(TEMPEL(I)>= UVAR(I,8).AND.FRAC2(I)>ZERO)THEN
          ZEQ(I)   =  (TEMPEL(I)-AE1)/(AE3-AE1)
          TAU(I)   =  TAU1 + ZEQ(I) * ( TAU3 - TAU1)
          ZEQ(I)   =  MIN ( ONE  ,MAX ( ZERO, ZEQ(I)))
          TAU(I)   =  MAX ( TAU3,MIN ( TAU1, TAU(I)))
          FRAC1(I) = UVAR(I,2) + MAX(ZERO,(ZEQ(I) - UVAR(I,2)) ) * TIMESTEP*THEACCFACT / TAU(I)
          FRAC2(I) = ONE - FRAC1(I) - FRAC3(I)- FRAC4(I)- FRAC5(I)   
          TEMPMIN(I) = UVAR(I,8)! heating activated AE1<AE3
        ELSE       
          NICOOL = NICOOL+1
          INDEX(NICOOL)=I
          IF (TEMPEL(I)<= 1073.0 .AND. UVAR(I,26)==ZERO)THEN! for hardness 
            UVAR(I,26)=TIME
            UVAR(I,27)=TEMPEL(I)
          ENDIF
          IF (TEMPEL(I)<= 773.0 .AND. UVAR(I,16)==ZERO)THEN! for hardness 
            UVAR(I,16)=TIME
            UVAR(I,33)=TEMPEL(I)
          ENDIF
        ENDIF
       ENDDO  
       IF (NICOOL > 0) THEN
        IF (FLAG_TR_KINETICS == 2) THEN
         CALL PHASEKINETIC2(NEL,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .     QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ELSE
         CALL KIRKALDYKINETICS(NEL,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,QR2,
     .     QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX )
        ENDIF
       ENDIF
      ENDIF!FLAG_LOC /= 2
c-----------------------------------------------
      DO I=1,NEL
        IF(TEMPO(I)<=TEMPMIN(I))TEMPMIN(I)=TEMPO(I)
        UVAR(I,1) = TEMPMIN(I)  
      ENDDO
c-------------------------------------
c interpolatin of yield for each phase 
c-------------------------------------
      DO I=1,NEL
        IPOS1(I,1) = VARTMP(I,1)
        IPOS1(I,2) = VARTMP(I,2)
        IPOS1(I,3) = VARTMP(I,3)
C
        IPOS2(I,1) = VARTMP(I,4)
        IPOS2(I,2) = VARTMP(I,5)
        IPOS2(I,3) = VARTMP(I,6)

        IPOS3(I,1) = VARTMP(I,7)
        IPOS3(I,2) = VARTMP(I,8)
        IPOS3(I,3) = VARTMP(I,9)
C
        IPOS4(I,1) = VARTMP(I,10)
        IPOS4(I,2) = VARTMP(I,11)
        IPOS4(I,3) = VARTMP(I,12)
C
        IPOS5(I,1) = VARTMP(I,13)
        IPOS5(I,2) = VARTMP(I,14)
        IPOS5(I,3) = VARTMP(I,15)
C 
        XX1(I,1)=PLA1(I)
        XX1(I,2)=TEMPEL(I)
        XX1(I,3)=EPSP(I)*XFAC(1)
C
        XX2(I,1)=PLA2(I)
        XX2(I,2)=TEMPEL(I)
        XX2(I,3)=EPSP(I)*XFAC(2)
C
        XX3(I,1)=PLA3(I)
        XX3(I,2)=TEMPEL(I)
        XX3(I,3)=EPSP(I)*XFAC(3)
C
        XX4(I,1)=PLA4(I)
        XX4(I,2)=TEMPEL(I)
        XX4(I,3)=EPSP(I)*XFAC(4)
C
        XX5(I,1)=PLA5(I)
        XX5(I,2)=TEMPEL(I)
        XX5(I,3)=EPSP(I)*XFAC(5)
      END DO
C
      CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL,IPOS1,XX1,Y1,DYDX1)
      CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS2,XX2,Y2,DYDX2)
      CALL TABLE_VINTERP(TABLE(ITABLE(3)),NEL,IPOS3,XX3,Y3,DYDX3)
      CALL TABLE_VINTERP(TABLE(ITABLE(4)),NEL,IPOS4,XX4,Y4,DYDX4)
      CALL TABLE_VINTERP(TABLE(ITABLE(5)),NEL,IPOS5,XX5,Y5,DYDX5)
      DO I=1,NEL
        Y1(I)=Y1(I)*YSCALE(1)       
        Y2(I)=Y2(I)*YSCALE(2)       
        Y3(I)=Y3(I)*YSCALE(3)       
        Y4(I)=Y4(I)*YSCALE(4)       
        Y5(I)=Y5(I)*YSCALE(5)      

        DYDX1(I)=DYDX1(I)*YSCALE(1) 
        DYDX2(I)=DYDX2(I)*YSCALE(2) 
        DYDX3(I)=DYDX3(I)*YSCALE(3) 
        DYDX4(I)=DYDX4(I)*YSCALE(4) 
        DYDX5(I)=DYDX5(I)*YSCALE(5) 
        YIELD(I)=UVAR(I,9)      
        YLD(I)=MAX(EM20,Y1(I)*FRAC1(I)+Y2(I)*FRAC2(I)+
     .      Y3(I)*FRAC3(I)+Y4(I)*FRAC4(I)+Y5(I)*FRAC5(I))

        Y1INI(I) = MAX(EM20,Y1(I))
        VARTMP(I,1) = IPOS1(I,1)
        VARTMP(I,2) = IPOS1(I,2)
        VARTMP(I,3) = IPOS1(I,3)
C
        VARTMP(I,4) = IPOS2(I,1)
        VARTMP(I,5) = IPOS2(I,2)
        VARTMP(I,6) = IPOS2(I,3)

        VARTMP(I,7) = IPOS3(I,1)
        VARTMP(I,8) = IPOS3(I,2)
        VARTMP(I,9) = IPOS3(I,3)
C
        VARTMP(I,10) = IPOS4(I,1)
        VARTMP(I,11) = IPOS4(I,2)
        VARTMP(I,12) = IPOS4(I,3)
C
        VARTMP(I,13) = IPOS5(I,1)
        VARTMP(I,14) = IPOS5(I,2)
        VARTMP(I,15) = IPOS5(I,3)
      ENDDO

      FACCS(1:NEL) = ONE 
      ! analytic strain rate dependency
      IF(CEPS/=ZERO.AND.PEPS/=ZERO)THEN
        DO I=1,NEL
          IF(EPSP(I)>CEPS)THEN
            FACCS(I)=ONE+ (EPSP(I)/CEPS)**(ONE/PEPS)                                                     
            YLD(I)= YLD(I)*FACCS(I)
          ENDIF 
        ENDDO
      ENDIF
c---------------------------------------------- ------------------
c---------------------------------------------------------------- 
c         compute thermal strain and transformation strain
c---------------------------------------------- ------------------
c---------------------------------------------------------------- 
      DO I=1,NEL
        TOTFRAC(I)  = FRAC2(I)+FRAC3(I)+FRAC4(I)+FRAC5(I)
        DEPSTH(I)   = (FRAC1(I)*ALFA1+TOTFRAC(I)*ALFA2)*(TEMPEL(I)-UVAR(I,8)) 
        TRDEPSTH(I) = THREE*DEPSTH(I) 
        UVAR(I,38)  = UVAR(I,38) + DEPSTH(I)
      ENDDO 

      IF(FLAG_TR_STRAIN == 2) THEN
        ! dependent directly on density
        ! compute density for eachphase depending on temperature
        ! interpole aust -f -p- b- m
        DO I=1,NEL
         RHO_A(I) = FINTER(KFUNC(3),TEMPEL(I),NPF,TF,DYDXR)*RSCALE(1)
         RHO_F(I) = FINTER(KFUNC(4),TEMPEL(I),NPF,TF,DYDXR)*RSCALE(2)
         RHO_P(I) = FINTER(KFUNC(5),TEMPEL(I),NPF,TF,DYDXR)*RSCALE(3)
         RHO_B(I) = FINTER(KFUNC(6),TEMPEL(I),NPF,TF,DYDXR)*RSCALE(4)     
         RHO_M(I) = FINTER(KFUNC(7),TEMPEL(I),NPF,TF,DYDXR)*RSCALE(5)
         DXRHO    = (FRAC1(I) - UVAR(I,2)) * RHO_A (I) +
     .              (FRAC2(I) - UVAR(I,3)) * RHO_F (I) + 
     .              (FRAC3(I) - UVAR(I,4)) * RHO_P (I) + 
     .              (FRAC4(I) - UVAR(I,5)) * RHO_B (I) + 
     .              (FRAC5(I) - UVAR(I,6)) * RHO_M (I)  
         RHO_N(I)  = FRAC1(I)*RHO_A (I) +  FRAC2(I)*RHO_F(I) + FRAC3(I)*RHO_P(I) + FRAC4(I)*RHO_B(I) + FRAC5(I)*RHO_M(I)
         DEPSTR(I) = ZERO
         IF(TOTFRAC(I) > ZERO.AND.TOTFRAC(I)< ONE)
     .   DEPSTR(I) =  THIRD*DXRHO/(RHO0(I) + RHO_N(I)-UVAR(I,43))
         UVAR(I,43)= RHO_N(I)
        ENDDO 
      ELSE
        DO I=1,NEL
          DETH12(I)=QPTT*EM03
          IF (TEMPEL(I)<MS ) DETH12(I)=SIX*EM03
          DEPSTR(I) = ZERO
          IF(TOTFRAC(I) > ZERO.AND.TOTFRAC(I)< ONE)
     .    DEPSTR(I) =  DETH12(I) * (TOTFRAC(I)-TOTFRACOLD(I))!*LOG(TOTFRAC(I))/ (ONE - TOTFRAC(I))
        ENDDO 
      ENDIF
      ! remove thermal and transformation strain from total strain to keep eps_meca only
      DO I=1,NEL
         DEPSXX(I) = DEPSXX(I) - DEPSTH(I) - DEPSTR(I)
        DEPSYY(I) = DEPSYY(I) - DEPSTH(I) - DEPSTR(I) 
      ENDDO 
      IF (TIMESTEP /=ZERO) THEN 
        DO I=1,NEL
         EPSPXX(I) = DEPSXX(I) /TIMESTEP
         EPSPYY(I) = DEPSYY(I) /TIMESTEP 
        ENDDO !DO I=1,NEL
      ENDIF
      DO I=1,NEL
        GZ(I)     = FINTER(1, TOTFRAC(I),NPFG,TGZ,DYDXGZ(I))! used to compute plastic strain rate
      ENDDO 
c---------------------------------------------- 
      DO I=1,NEL
        IF (OFF(I)== ONE )THEN 
          LNF2(I) = ZERO 
          LNF3(I) = ZERO
          LNF4(I) = ZERO
          LNF5(I) = ZERO !used in plast strain increment local yield
          IF(FRAC2(I)>UVAR(I,3).AND. UVAR(I,3) > ZERO) LNF2(I) = LOG(FRAC2(I)/UVAR(I,3) )
          IF(FRAC3(I)>UVAR(I,4).AND. UVAR(I,4) > ZERO) LNF3(I) = LOG(FRAC3(I)/UVAR(I,4) )
          IF(FRAC4(I)>UVAR(I,5).AND. UVAR(I,5) > ZERO) LNF4(I) = LOG(FRAC4(I)/UVAR(I,5) )
          IF(FRAC5(I)>UVAR(I,6).AND. UVAR(I,6) > ZERO) LNF5(I) = LOG(FRAC5(I)/UVAR(I,6) )
        ENDIF
      ENDDO !DO I=1,NEL
c================================================
c     Mechanical calculation -+ DEPSTH(I)
c================================================
      DO I=1,NEL
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A1(I)*DEPSXX(I) + A2(I)*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A1(I)*DEPSYY(I) + A2(I)*DEPSXX(I)
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G(I) ! no *HALF needed here cz it is 2*G*eps_xy_tensor 
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*GS(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*GS(I)
        ! Computation of the pressure of the trial stress tensor
        P(I) = -(SIGNXX(I) + SIGNYY(I)) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I)  + P(I)
        SYY(I) = SIGNYY(I)  + P(I)
        SZZ(I) = P(I)
        DEZZ(I) = -NU_1_NU * (DEPSXX(I) + DEPSYY(I)) +  DEPSTH(I) + DEPSTR(I)
        SVM(I)    = SQRT(THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2)   

        NORMDEV(I) = SQRT(SXX(I)* SXX(I)
     .                +   SYY(I)* SYY(I) 
     .                +   SZZ(I)* SZZ(I))/E(I)
        SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
        POLD(I) = -(SIGOXX(I)+SIGOYY(I)) * THIRD
        SOXX(I)  =  SIGOXX(I)+POLD(I) 
        SOYY(I)  =  SIGOYY(I)+POLD(I) 
        SOZZ(I)  =  POLD(I)
        SVMO(I)    = SQRT(THREE_HALF*(SOXX(I)**2 + SOYY(I)**2 + SOZZ(I)**2) + THREE*SIGOXY(I)**2)   

      ENDDO
      !-------------------------------------------------------------------------
      ! check if local or global yield -in both cases plastic deformation occurs
      !-------------------------------------------------------------------------
      NINDXGLOB = 0
      NINDXLOC  = 0
      DO I=1,NEL
        IF (       SVM(I)  < YLD(I)      .AND. OFF(I) == ONE 
     .      .AND.TOTFRAC(I)>TOTFRACOLD(I).AND. NORMDEV(I)> EM10
     .      .AND.TOTFRAC(I)< ONE         .AND. SVM(I)>SVMO(I)) THEN !.AND.HEATFLAG==0
          ! LOCAL YIELD ALGORITHM - ONLY OCCURS IF DEVIATORIC STRESSES ARE APPLIED 
          !----------------------         
          NINDXLOC = NINDXLOC + 1
          INDXLOC(NINDXLOC) = I
        ENDIF
      ENDDO
      DO I=1,NEL
        IF (SVM(I) > YLD(I) .AND. OFF(I) == ONE) THEN 
          ! GLOBAL YIELD ALGORITHM
          !----------------------         
          NINDXGLOB = NINDXGLOB + 1
          INDXGLOB(NINDXGLOB) = I
        ENDIF
      ENDDO
      !-------------------------------------------------------------------------
      !-------------------------------------------------------------------------
      IF (NINDXGLOB > 0) THEN
        DO II = 1,NINDXGLOB         
          I = INDXGLOB(II)          
          IF (UVAR(I,3)/=ZERO)THEN
            PSI2(I) = MAX(ZERO,(LNF2(I)*(TETA2*PLA1(I)-PLA2(I)))/(ONE+LNF2(I))  )
            PHI2(I) =(ONE+TETA2*LNF2(I))/(ONE+LNF2(I)) 
          ENDIF
          IF (UVAR(I,4)/=ZERO)THEN
            PSI3(I) = MAX(ZERO,(LNF3(I)*(TETA3*PLA1(I)-PLA3(I)))/(ONE+LNF3(I)) )
            PHI3(I) =(ONE+TETA3*LNF3(I))/(ONE+LNF3(I)) 
          ENDIF
          IF (UVAR(I,5)/=ZERO)THEN
            PSI4(I) = MAX(ZERO, (LNF4(I)*(TETA4*PLA1(I)-PLA4(I)))/(ONE+LNF4(I)) ) 
            PHI4(I) =(ONE+TETA4*LNF4(I))/(ONE+LNF4(I))
          ENDIF      
          IF (UVAR(I,6)/=ZERO)THEN
            PSI5(I) = MAX(ZERO,(LNF5(I)*(TETA5*PLA1(I)-PLA5(I)))/(ONE+LNF5(I)) )
            PHI5(I) = (ONE+TETA5*LNF5(I))/(ONE+LNF5(I))
          ENDIF
        ENDDO !II = 1,NINDXGLOB         
        DO ITER = 1,NITER
         DO II = 1,NINDXGLOB         
            I = INDXGLOB(II)          
            FCT(I) = SVM(I) - YLD(I)
            !df/dsig
            NORMXX  = THREE_HALF * SXX(I) /SVM(I) ! DF/DSIG
            NORMYY  = THREE_HALF * SYY(I) /SVM(I) 
            NORMZZ  = THREE_HALF * SZZ(I) /SVM(I)   
            NORMXY  = TWO * THREE_HALF * SIGNXY(I) /SVM(I) 
            ! df/ddlam = DF/DSIG * DSIG/DDLAM
            DF(I) = NORMXX * (A1(I)*NORMXX + A2(I)*NORMYY)
     .            + NORMYY * (A1(I)*NORMYY + A2(I)*NORMXX)
     .            + NORMXY * NORMXY * G(I)
            DF(I) = SIGN(MAX(ABS(DF(I)),EM20) ,DF(I)) 
            DDLAM    = FCT(I)/DF(I) 

            !compute derivative of effective plastic strain vs dlam
            DFDSIG_DSIG = SIGNXX(I) * NORMXX
     .                  + SIGNYY(I) * NORMYY
     .                  + SIGNXY(I) * NORMXY 
            DPLA_DLAM = DFDSIG_DSIG / YLD(I)


            !  Plastic strains tensor update
            DPXX(I) = DDLAM * NORMXX  
            DPYY(I) = DDLAM * NORMYY  
            DPZZ(I) = DDLAM * NORMZZ  
            DPXY(I) = DDLAM * NORMXY  
            DEZZ(I)= DEZZ(I) + NU_1_NU*(DPXX(I) + DPYY(I)) + DPZZ(I) 
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (A1(I)*DPXX(I) + A2(I)*DPYY(I))
            SIGNYY(I) = SIGNYY(I) - (A1(I)*DPYY(I) + A2(I)*DPXX(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)
            P(I) = -(SIGNXX(I) + SIGNYY(I)) * THIRD
            ! update of the deviatoric stress tensor
            SXX(I) = SIGNXX(I)  + P(I)
            SYY(I) = SIGNYY(I)  + P(I)
            SZZ(I) = P(I)
            SVM(I) = SQRT(THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2)   
        

            DDEP    = DDLAM*DPLA_DLAM
            DPLA(I) = MAX(ZERO, DPLA(I) + DDEP )

            ! DPLA AND PLA OF EACH PHASE               
            DPLA1(I)= DPLA1(I) + DDEP         !          
            IF (UVAR(I,3)>ZERO)THEN                   
              DPLA2(I)= DPLA1(I) *PHI2(I) + PSI2(I)         
            ENDIF                                     
            IF (UVAR(I,4)>ZERO)THEN                   
              DPLA3(I)= DPLA1(I) *PHI3(I) + PSI3(I)         
            ENDIF                                                                                           
            IF (UVAR(I,5)>ZERO)THEN                   
              DPLA4(I)= DPLA1(I) *PHI4(I) + PSI4(I)         
            ENDIF                                               
            IF (UVAR(I,6)>ZERO)THEN                   
              DPLA5(I)= DPLA1(I) *PHI5(I) + PSI5(I)         
            ENDIF                                     
            PLA1(I)= UVAR(I,17) + DPLA1(I)
            PLA2(I)= UVAR(I,18) + DPLA2(I)
            PLA3(I)= UVAR(I,19) + DPLA3(I)
            PLA4(I)= UVAR(I,20) + DPLA4(I)
            PLA5(I)= UVAR(I,21) + DPLA5(I)

            !-------------------------------------
            !UPDATE yield for each phase 
            !-------------------------------------
            IPOS1(I,1) = VARTMP(I,1)
            IPOS1(I,2) = VARTMP(I,2)
            IPOS1(I,3) = VARTMP(I,3)
C
            IPOS2(I,1) = VARTMP(I,4)
            IPOS2(I,2) = VARTMP(I,5)
            IPOS2(I,3) = VARTMP(I,6)

            IPOS3(I,1) = VARTMP(I,7)
            IPOS3(I,2) = VARTMP(I,8)
            IPOS3(I,3) = VARTMP(I,9)
C
            IPOS4(I,1) = VARTMP(I,10)
            IPOS4(I,2) = VARTMP(I,11)
            IPOS4(I,3) = VARTMP(I,12)
C
            IPOS5(I,1) = VARTMP(I,13)
            IPOS5(I,2) = VARTMP(I,14)
            IPOS5(I,3) = VARTMP(I,15)
C 
            XX1(I,1)=PLA1(I)
            XX2(I,1)=PLA2(I)
            XX3(I,1)=PLA3(I)
            XX4(I,1)=PLA4(I)
            XX5(I,1)=PLA5(I)
          END DO
C
          CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL,IPOS1,XX1,Y1,DYDX1)
          CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS2,XX2,Y2,DYDX2)
          CALL TABLE_VINTERP(TABLE(ITABLE(3)),NEL,IPOS3,XX3,Y3,DYDX3)
          CALL TABLE_VINTERP(TABLE(ITABLE(4)),NEL,IPOS4,XX4,Y4,DYDX4)
          CALL TABLE_VINTERP(TABLE(ITABLE(5)),NEL,IPOS5,XX5,Y5,DYDX5)
          DO  II = 1,NINDXGLOB         
            I = INDXGLOB(II)  
            Y1(I)=Y1(I)*YSCALE(1)       
            Y2(I)=Y2(I)*YSCALE(2)       
            Y3(I)=Y3(I)*YSCALE(3)       
            Y4(I)=Y4(I)*YSCALE(4)       
            Y5(I)=Y5(I)*YSCALE(5)      
            DYDX1(I)=DYDX1(I)*YSCALE(1) 
            DYDX2(I)=DYDX2(I)*YSCALE(2) 
            DYDX3(I)=DYDX3(I)*YSCALE(3) 
            DYDX4(I)=DYDX4(I)*YSCALE(4) 
            DYDX5(I)=DYDX5(I)*YSCALE(5) 

            !new yield updated in newton iterations     
            YLD(I)=MAX(EM20,  Y1(I)*FRAC1(I)+
     .                        Y2(I)*FRAC2(I)+
     .                        Y3(I)*FRAC3(I)+
     .                        Y4(I)*FRAC4(I)+
     .                        Y5(I)*FRAC5(I))
            YLD(I)= YLD(I)*FACCS(I)

            VARTMP(I,1) = IPOS1(I,1)
            VARTMP(I,2) = IPOS1(I,2)
            VARTMP(I,3) = IPOS1(I,3)
C
            VARTMP(I,4) = IPOS2(I,1)
            VARTMP(I,5) = IPOS2(I,2)
            VARTMP(I,6) = IPOS2(I,3)

            VARTMP(I,7) = IPOS3(I,1)
            VARTMP(I,8) = IPOS3(I,2)
            VARTMP(I,9) = IPOS3(I,3)
C
            VARTMP(I,10) = IPOS4(I,1)
            VARTMP(I,11) = IPOS4(I,2)
            VARTMP(I,12) = IPOS4(I,3)
C
            VARTMP(I,13) = IPOS5(I,1)
            VARTMP(I,14) = IPOS5(I,2)
            VARTMP(I,15) = IPOS5(I,3)
          ENDDO ! indexed elements
        ENDDO! ITER = 1,NITER
        DO II = 1,NINDXGLOB         
          I = INDXGLOB(II)          
          PLA(I) = PLA(I)+ MAX(DPLA1(I), ZERO)
        ENDDO 
      ENDIF

      !-------------------------------------------------------------------------
      !LOCAL YIELD
      !-------------------------------------------------------------------------
      IF (NINDXLOC > 0) THEN
        DO II = 1,NINDXLOC         
          I = INDXLOC(II)    
          LOGZ(I) = ONE
          LOGZM1(I) = ONE
          IF (UVAR(I,3)/=ZERO)THEN
            PSI2(I) = MAX(ZERO,(LNF2(I)*(TETA2*PLA1(I)))/(ONE+LNF2(I))  )
            PHI2(I) = TETA2*LNF2(I)/(ONE+LNF2(I)) 
          ENDIF
          IF (UVAR(I,4)/=ZERO)THEN
            PSI3(I) = MAX(ZERO,(LNF3(I)*(TETA3*PLA1(I)))/(ONE+LNF3(I)) )
            PHI3(I) = TETA3*LNF3(I)/(ONE+LNF3(I)) 
          ENDIF
          IF (UVAR(I,5)/=ZERO)THEN
            PSI4(I) = MAX(ZERO, (LNF4(I)*(TETA4*PLA1(I)))/(ONE+LNF4(I)) ) 
            PHI4(I) = TETA4*LNF4(I)/(ONE+LNF4(I))
          ENDIF      
          IF (UVAR(I,6)/=ZERO)THEN
            PSI5(I) = MAX(ZERO,(LNF5(I)*(TETA5*PLA1(I)))/(ONE+LNF5(I)) )
            PHI5(I) = TETA5*LNF5(I)/(ONE+LNF5(I))
          ENDIF
          IF (TOTFRAC(I) > ZERO)THEN
            LOGZ(I) = LOG(TOTFRAC(I))
          ENDIF
          IF (  TOTFRACOLD(I)>ZERO)THEN !totfrac old
            LOGZM1(I) = LOG( TOTFRACOLD(I))
          ENDIF       

          A(I) = (ONE - TOTFRAC(I))* GZ(I) / E(I)
          B(I) = TWO  * (ALFA1 -ALFA2)* (TEMPEL(I)-TEMPO(I) ) *TOTFRAC(I)*LOGZ(I)
          C(I) = TWO*DETH12(I)*ABS((TOTFRAC(I)*(ONE-LOGZ(I))- TOTFRACOLD(I)*(ONE-LOGZM1(I))))
      
          HFCT(I) = ONE + MAX(ZERO,SEVEN_HALF*(SVMO(I)/YLD(I)-HALF) )
          DPLA(I) = A(I) * ( SVM(I) - SVMO(I) ) + B(I) + C(I) *HFCT(I)  
          GSIG(I) = ONE/( ONE + THREE * (G(I) / Y1(I))   * DPLA(I) )  

          NORMXX  = THREE_HALF * SOXX(I) /Y1(I) ! DF/DSIG
          NORMYY  = THREE_HALF * SOYY(I) /Y1(I) 
          NORMZZ  = THREE_HALF * SOZZ(I) /Y1(I)  
          NORMXY  = TWO * THREE_HALF * SIGOXY(I) /Y1(I) 

          DPXX(I) = DPLA(I) * NORMXX  
          DPYY(I) = DPLA(I) * NORMYY  
          DPZZ(I) = DPLA(I) * NORMZZ  
          DPXY(I) = DPLA(I) * NORMXY  
          ! Elasto-plastic stresses update   
          SIGNXX(I) = SIGNXX(I) - (A1(I)*DPXX(I) + A2(I)*DPYY(I))                                
          SIGNYY(I) = SIGNYY(I) - (A1(I)*DPYY(I) + A2(I)*DPXX(I))                                
          SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)                                                   
          P(I) = -(SIGNXX(I) + SIGNYY(I)) * THIRD                                                
          ! update of the deviatoric stress tensor                                               
          SXX(I) = SIGNXX(I)  + P(I)                                                             
          SYY(I) = SIGNYY(I)  + P(I)                                                             
          SZZ(I) = P(I)                                                                          
          SVM(I) = SQRT(THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2)     
          DEZZ(I)= DEZZ(I) + NU_1_NU*(DPXX(I) + DPYY(I)) + DPZZ(I)                               
        ENDDO !II = 1,NINDXLOC    
     
        DO II = 1,NINDXLOC         
          I = INDXLOC(II)  
          DPLA1(I)= DPLA(I)/(ONE - TOTFRAC(I))                     
          IF (UVAR(I,3)>ZERO)THEN                   
            DPLA2(I)= DPLA1(I) *PHI2(I) + PSI2(I)         
          ENDIF                                     
          IF (UVAR(I,4)>ZERO)THEN                                  
            DPLA3(I)= DPLA1(I) *PHI3(I) + PSI3(I)                  
          ENDIF                                                    
          IF (UVAR(I,5)>ZERO)THEN                                  
            DPLA4(I)= DPLA1(I) *PHI4(I) + PSI4(I)                  
          ENDIF                                                    
          IF (UVAR(I,6)>ZERO)THEN                                                                           
            DPLA5(I)= DPLA1(I) *PHI5(I) + PSI5(I)                  
          ENDIF                                                    
          PLA1(I)= UVAR(I,17) + DPLA1(I)          
          PLA2(I)= UVAR(I,18) + DPLA2(I)          
          PLA3(I)= UVAR(I,19) + DPLA3(I)          
          PLA4(I)= UVAR(I,20) + DPLA4(I)          
          PLA5(I)= UVAR(I,21) + DPLA5(I)          
          !-------------------------------------  
          !UPDATE yield for each phase            
          !-------------------------------------  
          XX1(I,1)=PLA1(I)
          XX2(I,1)=PLA2(I)                 
          XX3(I,1)=PLA3(I)                 
          XX4(I,1)=PLA4(I)                 
          XX5(I,1)=PLA5(I)                 
          IPOS1(I,1) = VARTMP(I,1) 
          IPOS1(I,2) = VARTMP(I,2) 
          IPOS1(I,3) = VARTMP(I,3) 
                                   
          IPOS2(I,1) = VARTMP(I,4)
          IPOS2(I,2) = VARTMP(I,5) 
          IPOS2(I,3) = VARTMP(I,6) 
                                   
          IPOS3(I,1) = VARTMP(I,7)
          IPOS3(I,2) = VARTMP(I,8) 
          IPOS3(I,3) = VARTMP(I,9) 
                                   
          IPOS4(I,1) = VARTMP(I,10)
          IPOS4(I,2) = VARTMP(I,11) 
          IPOS4(I,3) = VARTMP(I,12) 
                                    
          IPOS5(I,1) = VARTMP(I,13)
          IPOS5(I,2) = VARTMP(I,14) 
          IPOS5(I,3) = VARTMP(I,15) 
            
         END DO
C
         CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL,IPOS1,XX1,Y1,DYDX1)
         CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS2,XX2,Y2,DYDX2)
         CALL TABLE_VINTERP(TABLE(ITABLE(3)),NEL,IPOS3,XX3,Y3,DYDX3)
         CALL TABLE_VINTERP(TABLE(ITABLE(4)),NEL,IPOS4,XX4,Y4,DYDX4)
         CALL TABLE_VINTERP(TABLE(ITABLE(5)),NEL,IPOS5,XX5,Y5,DYDX5)
         DO  II = 1,NINDXLOC         
            I = INDXLOC(II)  
            Y1(I)=Y1(I)*YSCALE(1)       
            Y2(I)=Y2(I)*YSCALE(2)       
            Y3(I)=Y3(I)*YSCALE(3)       
            Y4(I)=Y4(I)*YSCALE(4)       
            Y5(I)=Y5(I)*YSCALE(5)      
            DYDX1(I)=DYDX1(I)*YSCALE(1) 
            DYDX2(I)=DYDX2(I)*YSCALE(2) 
            DYDX3(I)=DYDX3(I)*YSCALE(3) 
            DYDX4(I)=DYDX4(I)*YSCALE(4) 
            DYDX5(I)=DYDX5(I)*YSCALE(5) 

            !new yield updated 
            YLD(I)=MAX(EM20,  Y1(I)*FRAC1(I)+
     .                        Y2(I)*FRAC2(I)+
     .                        Y3(I)*FRAC3(I)+
     .                        Y4(I)*FRAC4(I)+
     .                        Y5(I)*FRAC5(I))
            YLD(I)= YLD(I)*FACCS(I)        
        ENDDO !II = 1,NINDXLOC         

        DO  II = 1,NINDXLOC         
            I = INDXLOC(II)  
            VARTMP(I,1) = IPOS1(I,1)
            VARTMP(I,2) = IPOS1(I,2)
            VARTMP(I,3) = IPOS1(I,3)
C
            VARTMP(I,4) = IPOS2(I,1)
            VARTMP(I,5) = IPOS2(I,2)
            VARTMP(I,6) = IPOS2(I,3)

            VARTMP(I,7) = IPOS3(I,1)
            VARTMP(I,8) = IPOS3(I,2)
            VARTMP(I,9) = IPOS3(I,3)
C
            VARTMP(I,10) = IPOS4(I,1)
            VARTMP(I,11) = IPOS4(I,2)
            VARTMP(I,12) = IPOS4(I,3)
C
            VARTMP(I,13) = IPOS5(I,1)
            VARTMP(I,14) = IPOS5(I,2)
            VARTMP(I,15) = IPOS5(I,3)

        ENDDO !NINDXLOC
      ENDIF !(NINDXLOC > 0) 


      DO I=1,NEL
        IF (OFF(I) == ONE )THEN 
         UVAR(I,8) = TEMPEL(I)

         UVAR(I,17)= UVAR(I,17) + MAX(DPLA1(I), ZERO) 
         UVAR(I,18)= UVAR(I,18) + MAX(DPLA2(I), ZERO)
         UVAR(I,19)= UVAR(I,19) + MAX(DPLA3(I), ZERO)
         UVAR(I,20)= UVAR(I,20) + MAX(DPLA4(I), ZERO)
         UVAR(I,21)= UVAR(I,21) + MAX(DPLA5(I), ZERO)
         IF(UVAR(I,15)<SVM(I))   UVAR(I,15)=SVM(I)
         IF(UVAR(I,34)>TEMPEL(I))UVAR(I,34)=TEMPEL(I)
                 
         UVAR(I,23)= FRAC2(I)/XEQ2       !X2(I)
         UVAR(I,24)= FRAC3(I)/(ONE-XEQ2) !X3(I)
         UVAR(I,25)= FRAC4(I)            !X4(I)        
         UVAR(I,44)= FRAC5(I)/MAX(EM20,XGAMA(I))

         UVAR(I,2)= FRAC1(I)
         UVAR(I,3)= FRAC2(I)
         UVAR(I,4)= FRAC3(I)
         UVAR(I,5)= FRAC4(I)
         UVAR(I,6)= FRAC5(I)
         UVAR(I,10)= XGAMA(I)

         HARD(I)= ZERO
         EPSTHTOT(I) = UVAR(I,38)
        ENDIF
        IF (DPLA(I)>ZERO)THEN
          H(I)=(YLD(I)-UVAR(I,9))/DPLA(I)
          H(I)=MAX(EM10,H(I))
          ETSE(I)=H(I)/(H(I)+E(I))
          UVAR(I,28)=ETSE(I)
        ELSE
           ETSE(I)=UVAR(I,28)
        ENDIF
        UVAR(I,9)=YLD(I)
        THK(I) = THK(I) + DEZZ(I)*THKLY(I)*OFF(I)

      ENDDO!  
      IF (ALFA1/= ZERO .OR. ALFA2 /= ZERO) THEN !compute internal thermal energy
        DO I=1,NEL        
         !SIGKK(I)  = SIGNXX(I)+SIGNYY(I)+SIGOXX(I)+SIGOYY(I)
         !EINTTH(I) = EINTTH(I)-HALF*SIGKK(I)*DEPSTH(I)
        ENDDO
      ENDIF    
      IF(HEATFLAG == 0) THEN
       DO I=1,NEL
        IF (OFF(I) == ONE )THEN 
          IF (TEMPEL(I)<MS.AND.TEMPEL(I)<= UVAR(I,8) .AND. TEMPEL(I)<=1073.0)THEN !
           VR(I)   =  ZERO
           HARD(I) =  UVAR(I,7)
           IF (UVAR(I,16) >UVAR(I,26))THEN
               VR(I)  = (UVAR(I,27)-UVAR(I,33))*UNITT/(UVAR(I,16)-UVAR(I,26))
               HARD(I)= (FRAC2(I)+FRAC3(I))*HFP*LOG10(VR(I))+FRAC4(I)*HB+FRAC5(I)*HM    
               UVAR(I,7)= HARD(I)
           ENDIF
          ENDIF
          VOLI = THKLY(I)*AREA(I)
          DIE(I) =DIE(I)+ (LAT2*(FRAC5(I)-UVAR(I,6)) +
     .                     LAT1*(FRAC2(I)-UVAR(I,3)
     .             +             FRAC3(I)-UVAR(I,4)
     .             +             FRAC4(I)-UVAR(I,5) ) ) *VOLI  
   
         ENDIF !(OFF(I) == ONE ) 
       ENDDO
      ENDIF

      !-------------------------------------
      !-------------------------------------
      RETURN
      END
