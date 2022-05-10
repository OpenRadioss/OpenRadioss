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
     1     NEL0   ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0    ,IPT     ,IFLAG   ,
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
     C     ETSE   ,GS     ,VOL     ,YLD    , TEMPEL ,
     D     DIE    ,COEF   , SHF    ,EPSP    ,TABLE  ,
     E     ITHK   ,NVARTMP , VARTMP) 
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
#include      "mvsiz_p.inc"
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
C VISCMAX | NEL0    | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL0    | F |R/W| THICKNESS
C PLA     | NEL0    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL0*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR,NVARTMP, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),IPM(NPROPMI,*),ITHK
      my_real TIME,TIMESTEP,UPARAM(*),
     .   AREA(NEL0),RHO0(NEL0),EINT(NEL0,2),
     .   THKLY(NEL0),PLA(NEL0),
     .   EPSPXX(NEL0),EPSPYY(NEL0),
     .   EPSPXY(NEL0),EPSPYZ(NEL0),EPSPZX(NEL0),
     .   DEPSXX(NEL0),DEPSYY(NEL0),
     .   DEPSXY(NEL0),DEPSYZ(NEL0),DEPSZX(NEL0),
     .   EPSXX(NEL0) ,EPSYY(NEL0) ,
     .   EPSXY(NEL0) ,EPSYZ(NEL0) ,EPSZX(NEL0) ,
     .   SIGOXX(NEL0),SIGOYY(NEL0),
     .   SIGOXY(NEL0),SIGOYZ(NEL0),SIGOZX(NEL0),
     .   PM(NPROPM,*),GS(*),VOL(NEL0) ,TEMPEL(NEL0),
     .   DIE(NEL0),COEF(NEL0), SHF(NEL0)  
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0),ETSE(NEL0)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL0,NUVAR),VARTMP(NEL0,NVARTMP),EPSP(NEL0),
     .   OFF(NEL0),THK(NEL0),YLD(NEL0)
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
      INTEGER I,J,K,NRATE(NEL0),J1,J2,N,NINDX,IADBUF,
     .        NMAX,INDEX(NEL0),K2,ITERK2,NPFG(2),ITABLE(5),
     .        EFUNC,IFUNC(5),ITERK,FLAG,NTAB,HEATFLAG,HEATOPTION,
     .        NIHEAT,NICOOL,FLAG_LOC ,LOCAL,FLAG_TR_STRAIN,FLAG_TR_KINETICS
      my_real
     .        CUN ,CDEUX,CTROIS,PSI2 ,PSI3,PSI4,PSI5,PHI2,DENO,EFAC,
     .        PHI3,PHI4 ,PHI5  ,ALAMBDA,BLAMBDA,CEPS,PEPS,SOL1,SOL2,
     .        YSCALE(5),TETA2,TETA3,TETA4,TETA5,QR2,QR3,QR4,PP,ESF,
     .        SVMO,ALFA1,ALFA2,T2,T3,T4,T5,T1,VOLI,XFAC(5),RSCALE(5),
     .        PPXX,PPYY,PPZZ,PPXY,PPYZ,PPZX,LNF2,LNF3,LNF4,LNF5,ESFIM1,
     .        ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,FG,
     .        E1PO(NEL0),RHNEW,EODE,FGRAIN,KPER,HFCTN,KBAIN,XEQ2,
     .        XEQ4,LAT1,LAT2,HFP,HB,HM,VOL0,A1,A2,TEMP,UNITT,
     .        TGZ(12),LOGZ,LOGZM1,CRIT,TEST, HEATFLAG1,
     .        CONV,FACCS,HUITCENT,SSPSHELL,SSPSOL,QPTT,SLOPE,
     .         TAU1, TAU3,DXRHO,DYDXR,
     .         GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .         GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI
      my_real
     .        E(NEL0),RBULK(NEL0),SHEAR(NEL0),G2(NEL0),DETH12(NEL0),
     .        LAMDA(NEL0),DYDXGZ(NEL0),DYDXE(NEL0),
     .        FRAC1(NEL0),FRAC2(NEL0),FRAC3(NEL0)  ,FRAC4(NEL0), 
     .        FRAC5(NEL0),TOTFRAC(NEL0),TOTFRACOLD(NEL0) ,SOXX(NEL0) ,
     .        GZ(NEL0)   ,TREO(NEL0)   ,SIGOM(NEL0),SOYY(NEL0),SOZZ(NEL0),
     .        DYDX1(NEL0),DYDX2(NEL0)  ,DYDX3(NEL0),
     .        DYDX4(NEL0),DYDX5(NEL0)  ,YIELD(NEL0),
     .        Y1(NEL0)   ,Y2(NEL0),Y3(NEL0),Y4(NEL0),Y5(NEL0),
     .        STRIALXX(NEL0),STRIALYY(NEL0),STRIALZZ(NEL0),
     .        STRIALXY(NEL0),STRIALYZ(NEL0),STRIALZX(NEL0),SVM(NEL0),
     .        EOXX(NEL0),EOYY(NEL0),EOZZ(NEL0),
     .        EOXY(NEL0),EOYZ(NEL0),EOZX(NEL0),
     .        EODEXX(NEL0),EODEYY(NEL0),EODEZZ(NEL0),
     .        EODEXY(NEL0),Y1INI(NEL0),
     .        EPSOXX(NEL0),EPSOYY(NEL0),EPSOZZ(NEL0),EPSOXY(NEL0),
     .        DEPSZZ(NEL0),EPSZZ(NEL0),
     .        DPLA(NEL0)  ,SNORM(NEL0),SEFF(NEL0),SIGOZZ(NEL0),
     .        SIGNZZ(NEL0),SIGM(NEL0)  ,SXX(NEL0),SYY(NEL0),SZZ(NEL0),
     .        SXY(NEL0)   ,DEPLXX(NEL0),
     .        DEPLYY(NEL0),DEPLZZ(NEL0),DEPLXY(NEL0),
     .        DPLA1(NEL0),DPLA2(NEL0),DPLA3(NEL0),DPLA4(NEL0),DPLA5(NEL0),
     .        PLA1(NEL0) ,PLA2(NEL0) ,PLA3(NEL0) ,PLA4(NEL0) ,PLA5(NEL0),
     .        DEPSELZZ(NEL0),DEPSPLZZ(NEL0),DEPSIM1(NEL0),DEPSI(NEL0),
     .        DEPSIP1(NEL0),SIGZIM1(NEL0),SIGI(NEL0),
     .        VOLOLD(NEL0),RH(NEL0),HFCT(NEL0),DE12(NEL0),YLDOLD(NEL0),
     .        DEPSELOZZ(NEL0),DEPSPLOZZ(NEL0), 
     .        SVMI(NEL0),TRDEPSTH(NEL0),SVMIM1(NEL0),SVMTR(NEL0),TRDEPS,
     .        DEPSTHXX(NEL0),DEPSTHYY(NEL0),DEPSTHZZ(NEL0),DEPSTH(NEL0),
     .        X1(NEL0),X2(NEL0),X3(NEL0),X4(NEL0),X5(NEL0),HARD(NEL0),
     .        DEXX(NEL0),DEYY(NEL0),DEZZ(NEL0),GOLD(NEL0),SVMtest(NEL0),
     .        EPLXX(NEL0),EPLYY(NEL0),EPLZZ(NEL0) ,EPLXY(NEL0),
     .        EELOXX(NEL0),EELOYY(NEL0),DEPSTR(NEL0) ,
     .        EELOXY(NEL0),EELOZZ(NEL0),NORMDEV(NEL0) ,
     .        XGAMA(NEL0),TEMPMIN(NEL0),VR(NEL0),H(NEL0),TEMPO(NEL0),
     .        ZEQ(NEL0), TAU(NEL0),
     .        RHO_A(NEL0),RHO_F(NEL0),RHO_P(NEL0),RHO_B(NEL0),RHO_M(NEL0),RHO_N(NEL0)
C
      my_real,
     .        DIMENSION(NEL0,3) :: XX5,XX1,XX2,XX3,XX4

      INTEGER,DIMENSION(NEL0,3) :: IPOS1,IPOS2,IPOS3,IPOS4,IPOS5

c      DATA NITER/10/
c-----------------------------------------------
c     USER VARIABLES INITIALIZATION
c-----------------------------------------------
c-----------------------------------------------
c     USER VARIABLES INITIALIZATION
c-----------------------------------------------
c    UVAR  1= TEMPMIN
c    UVAR  2= FRAC1
c    UVAR  3= FRAC2
c    UVAR  4= FRAC3
c    UVAR  5= FRAC4
c    UVAR  6= FRAC5
c    UVAR  7= HARD
c    UVAR  8= TEMPEL
c    UVAR  9= YIELD
c    UVAR 10= XGAMA
c    UVAR 11= EPSXX
c    UVAR 12= EPSYY
c    UVAR 13= EPSZZ
c    UVAR 14= EPSXY
c    UVAR 15= SVM
c    UVAR 16= TIME
c    UVAR 17= PLA1
c    UVAR 18= PLA2
c    UVAR 19= PLA3
c    UVAR 20= PLA4
c    UVAR 21= PLA5
c    UVAR 22= TOTFRAC
c    UVAR 23= X2
c    UVAR 24= X3
c    UVAR 25= X4
c    UVAR 26= TIME
c    UVAR 27= TEMPEL
c    UVAR 28= ETSE
c    UVAR 29= EPLXX
c    UVAR 30= EPLYY
c    UVAR 31= EPLZZ
c    UVAR 32= EPLXY
c    UVAR 33= TEMPE
c    UVAR 34= TEMPE
c    UVAR 35= VOL
c    UVAR 36= SHEAR
c    UVAR 37= SIGNZZ
c-----------------------------------------------
C     ! 0 < Ac1 < Ac3

      NTAB = IPM(226,MAT(1))  

      DO I=1,NTAB
        ITABLE(I) = IPM(226+I,MAT(1)) 
        XFAC(I)  = UPARAM(58+I)
      ENDDO


      DO I=1,NEL0
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
      TEMP  =  UPARAM(45) 
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
        !HEATFLAG1 = MIN(HEATFLAG1,UN)
        !HEATFLAG1 = MAX(HEATFLAG1,ZERO)
        HEATFLAG = INT(HEATFLAG1)
      ENDIF

c
      HUITCENT= HUIT*EP02
      QPTT=QUATRE+TROIS*(EM01+EM02)
      NPFG(1)=1
      NPFG(2)=13                            
      ITERK  =5
      ITERK2 =5
      CUN = THIRD/(UN-DEUX*NU)
      CDEUX = UNDEMI/(UN+NU)
      CTROIS = NU/(UN+NU)/(UN-DEUX*NU)     

      IF (ISIGI == 0) THEN   
        IF (TIME == ZERO)THEN 
          DO I=1,NEL0        
            UVAR(I,35)=VOL(I) 
            UVAR(I,43)=RHO0(I)
            IF(HEATFLAG == 1)THEN
               UVAR(I,2) = ZERO!  ! fraction of austenite
               UVAR(I,3) = UN ! ! fraction of ferrite
            ELSE
               UVAR(I,2) = UN! fraction of austenite
            ENDIF
            UVAR(I,8)   = UPARAM(45) !temperature 
            UVAR(I,1)   = UPARAM(45) !TINI
            UVAR(I,28)  = UN    !ETSE
            IF(JTHE==-1) UVAR(I,8)  =TEMPEL(I)
            !UVAR(I,9)=FINTER(KFUNC(2),ZERO,NPF,TF,DYDX1(I))!  initial yield
            XX1(I,1)=ZERO
            XX1(I,2)=UVAR(I,8) ! temp initial
            XX1(I,3)=ZERO
            IPOS1(I,1) = 0
            IPOS1(I,2) = 0
            IPOS1(I,3) = 0 
          ENDDO 
          CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL0,IPOS1,XX1,Y1,DYDX1)!  initial yield
          DO I=1,NEL0        
           UVAR(I,9)= Y1 (I)
          ENDDO 
        ENDIF                
      ENDIF



      DO I=1,NEL0
        TEMPO(I)   = UVAR(I,8)
        TEMPMIN(I) = UVAR(I,1) ! ASSURER QUE CHANGEMENT DE PHASE UNIQUEMENT SI T DECROIT (SI OSCILLE ON FAIT RIEN)
      ENDDO
      !IF (HEATFLAG == 1) THEN ! heating activated AE1<AE3
      ! DO I=1,NEL0
      !  TEMPMIN(I) = UVAR(I,8)
      ! ENDDO
      !ENDIF
      !DO I=1,NEL0
      !  IF(TEMPO(I)<=TEMPMIN(I))TEMPMIN(I)=TEMPO(I)
      !  UVAR(I,1) = TEMPMIN(I)  
      !ENDDO
      
c-------------------------------------
c compute temperature 
c-------------------------------------
C-JTHE = -1 : thermal lagrangian, 0 : no thermal lagrange, 1 : thermal ale
c      IF(JTHE <= 0 ) THEN        
c       DO I=1,NEL0
c         IF (UVAR(I,8)<=EP03)THEN
c           CP(I)= 1.117*EP06/(1010.0-TEMPO(I))/(1010.0-TEMPO(I))+12622.
c     .        /(1010.0 - TEMPO(I) )+ 0.3485*TEMPO(I)+ 355.6
c         ELSE
c           CP(I)= 1.225*EP08/((TEMPO(I)-990.)**4)+0.1381*TEMPO(I)
c     .       + 585.7
c         ENDIF
cc         TEMP = UPARAM(45)
c         VOL0    = VOL(I) * RHO0(I)
         !TEMPEL(I) = UPARAM(45)  
c     .    +(EINT(I,1)+ EINT(I,2))/VOL(I)/(RHO0(I)*CP(I))
c       ENDDO
c      ENDIF
c-------------------------------------        
      IF(KFUNC(1) > ZERO)THEN
        DO I=1,NEL0
         E(I) = FINTER(KFUNC(1),TEMPEL(I),NPF,TF,DYDXE(I))
         E(I)=  E(I) *  EFAC  
        ENDDO
      ENDIF
c-------------------------------------
      DO I=1,NEL0
        X2(I)=  UVAR(I,23)
        X3(I)=  UVAR(I,24)
        X4(I)=  UVAR(I,25)
        X5(I)=  UVAR(I,44)
        FRAC1(I)= UVAR(I,2) 
        FRAC2(I)= UVAR(I,3) 
        FRAC3(I)= UVAR(I,4) 
        FRAC4(I)= UVAR(I,5) 
        FRAC5(I)= UVAR(I,6) 
        PLA1(I)= UVAR(I,17)
        PLA2(I)= UVAR(I,18) 
        PLA3(I)= UVAR(I,19) 
        PLA4(I)= UVAR(I,20)
        PLA5(I)= UVAR(I,21) 
        GOLD(I)= UVAR(I,36)
        XGAMA(I)= UVAR(I,10)
        DPLA(I)= ZERO        
        DPLA1(I)= ZERO                           
        DPLA2(I)= ZERO                          
        DPLA3(I)= ZERO        
        DPLA4(I)= ZERO                           
        DPLA5(I)= ZERO
        TOTFRACOLD(I) = FRAC2(I)+FRAC3(I)+FRAC4(I)+FRAC5(I) ! Z sum of product phase
      ENDDO
c-------------------------------------        
      NICOOL = 0  
      IF(FLAG_LOC == 2) THEN   ! global treatment - same behavior per part 
       IF (HEATFLAG == 1) THEN ! heating activated AE1<AE3
        DO I=1,NEL0
         IF(TEMPEL(I)>= UVAR(I,8).AND.FRAC2(I)>ZERO)THEN
          ZEQ(I)   =  (TEMPEL(I)-AE1)/(AE3-AE1)
          TAU(I)   =  TAU1 + ZEQ(I) * ( TAU3 - TAU1)
          ZEQ(I)   =  MIN ( UN  ,MAX ( ZERO, ZEQ(I)))
          TAU(I)   =  MAX ( TAU3,MIN ( TAU1, TAU(I)))
          FRAC1(I) = UVAR(I,2) + MAX(ZERO,(ZEQ(I) - UVAR(I,2)) ) * TIMESTEP*THEACCFACT / TAU(I)
          FRAC2(I) = UN - FRAC1(I) - FRAC3(I)- FRAC4(I)- FRAC5(I)
          TEMPMIN(I) = UVAR(I,8)! heating activated AE1<AE3
         ENDIF
        ENDDO  
       ELSE ! HEATFLAG /= 1
        DO I=1,NEL0
          NICOOL = NICOOL + 1
          INDEX(NICOOL)=I
          IF (TEMPEL(I)<= 1073.0 .AND. UVAR(I,26)==ZERO)THEN ! for hardness 
            UVAR(I,26)=TIME
            UVAR(I,27)=TEMPEL(I)
          ENDIF
          IF (TEMPEL(I)<= 773.0 .AND. UVAR(I,16)==ZERO)THEN! for hardness 
            UVAR(I,16)=TIME
            UVAR(I,33)=TEMPEL(I)
          ENDIF
        ENDDO        
        IF (FLAG_TR_KINETICS == 2) THEN
         CALL PHASEKINETIC2(NEL0,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .     QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ELSE
         CALL KIRKALDYKINETICS(NEL0,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .    FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,QR2,
     .    QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ENDIF     
       ENDIF
      ELSE !FLAG_LOC /= 2 local 
       DO I=1,NEL0
        IF(TEMPEL(I)>= UVAR(I,8).AND.FRAC2(I)>ZERO)THEN
          ZEQ(I)   =  (TEMPEL(I)-AE1)/(AE3-AE1)
          TAU(I)   =  TAU1 + ZEQ(I) * ( TAU3 - TAU1)
          ZEQ(I)   =  MIN ( UN  ,MAX ( ZERO, ZEQ(I)))
          TAU(I)   =  MAX ( TAU3,MIN ( TAU1, TAU(I)))
          FRAC1(I) = UVAR(I,2) + MAX(ZERO,(ZEQ(I) - UVAR(I,2)) ) * TIMESTEP*THEACCFACT / TAU(I)
          FRAC2(I) = UN - FRAC1(I) - FRAC3(I)- FRAC4(I)- FRAC5(I)   
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
         CALL PHASEKINETIC2(NEL0,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .     QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX ) 
        ELSE
         CALL KIRKALDYKINETICS(NEL0,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,FCPER,
     .     FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,QR2,
     .     QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX )
        ENDIF
       ENDIF
      ENDIF!FLAG_LOC /= 2

      DO I=1,NEL0
        IF(TEMPO(I)<=TEMPMIN(I))TEMPMIN(I)=TEMPO(I)
        UVAR(I,1) = TEMPMIN(I)  
      ENDDO
      IF (HEATFLAG == 1) THEN
       TETA2  = ZERO 
       TETA3  = ZERO       
       TETA4  = ZERO       
       TETA5  = ZERO     
      ENDIF 

c-----------------------------------------------
c Compute thermal expansion increment
c Compute yield stress using plastic strain
c 1. estimation of eps_zz increment (elastic and plastic)
c 2. compute eps_zz increment by secant formula
c 3. compute stress tensor 3D formulation
c 4. compute stress deviator and effective trial stress
c 5. comparison with yield: global or local plasticity algorithm
C
c-------------------------------------
c interpolatin of yield for each phase 
c-------------------------------------
      DO I=1,NEL0
        IPOS1(I,1) = NINT(VARTMP(I,1))
        IPOS1(I,2) = NINT(VARTMP(I,2))
        IPOS1(I,3) = NINT(VARTMP(I,3))
C
        IPOS2(I,1) = NINT(VARTMP(I,4))
        IPOS2(I,2) = NINT(VARTMP(I,5))
        IPOS2(I,3) = NINT(VARTMP(I,6))

        IPOS3(I,1) = NINT(VARTMP(I,7))
        IPOS3(I,2) = NINT(VARTMP(I,8))
        IPOS3(I,3) = NINT(VARTMP(I,9))
C
        IPOS4(I,1) = NINT(VARTMP(I,10))
        IPOS4(I,2) = NINT(VARTMP(I,11))
        IPOS4(I,3) = NINT(VARTMP(I,12))
C
        IPOS5(I,1) = NINT(VARTMP(I,13))
        IPOS5(I,2) = NINT(VARTMP(I,14))
        IPOS5(I,3) = NINT(VARTMP(I,15))

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
      CALL TABLE_VINTERP(TABLE(ITABLE(1)),NEL0,IPOS1,XX1,Y1,DYDX1)
      CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL0,IPOS2,XX2,Y2,DYDX2)
      CALL TABLE_VINTERP(TABLE(ITABLE(3)),NEL0,IPOS3,XX3,Y3,DYDX3)
      CALL TABLE_VINTERP(TABLE(ITABLE(4)),NEL0,IPOS4,XX4,Y4,DYDX4)
      CALL TABLE_VINTERP(TABLE(ITABLE(5)),NEL0,IPOS5,XX5,Y5,DYDX5)

      DO I=1,NEL0
        ! depsxy is the engineering strain component (gama)
        ! scale depsxy by 0.5 to get the tensor component 
        DEPSXY(I) = DEPSXY(I)*UNDEMI
        EPSXY(I)  = EPSXY(I) *UNDEMI
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
        YLDOLD(I)=MAX(EM20,Y1(I)*FRAC1(I)+Y2(I)*FRAC2(I)+
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
c---------------------------------------------------------------- 
c         compute thermal strain and transformation strain
c---------------------------------------------- ------------------
      DO I=1,NEL0
        TOTFRAC(I)  = FRAC2(I)+FRAC3(I)+FRAC4(I)+FRAC5(I)
        DEPSTH(I)   = (FRAC1(I)*ALFA1+TOTFRAC(I)*ALFA2)*(TEMPEL(I)-UVAR(I,8)) 
        TRDEPSTH(I) = TROIS*DEPSTH(I) 
      ENDDO 

      IF(FLAG_TR_STRAIN == 2) THEN
        ! dependent directly on density
        ! compute density for eachphase depending on temperature
        ! interpole aust -f -p- b- m
        DO I=1,NEL0
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
         IF(TOTFRAC(I) > ZERO.AND.TOTFRAC(I)< UN)
     .   DEPSTR(I) =  THIRD*DXRHO/(RHO0(I) + RHO_N(I)-UVAR(I,43))
         UVAR(I,43)= RHO_N(I)
        ENDDO 
      ELSE
        DO I=1,NEL0
         DETH12(I)=QPTT*EM03
         IF (TEMPEL(I)<MS ) DETH12(I)=SIX*EM03
         DEPSTR(I) = ZERO
         IF(TOTFRAC(I) > ZERO.AND.TOTFRAC(I)< UN)
     .   DEPSTR(I) =  DETH12(I) * (TOTFRAC(I)-TOTFRACOLD(I))!*LOG(TOTFRAC(I))/ (UN - TOTFRAC(I))
        ENDDO 
      ENDIF
      DO I=1,NEL0
        DEPSXX(I) = DEPSXX(I) - DEPSTH(I) - DEPSTR(I)
        DEPSYY(I) = DEPSYY(I) - DEPSTH(I) - DEPSTR(I) 
      ENDDO 
      IF (TIMESTEP /=ZERO) THEN 
        DO I=1,NEL0
         EPSPXX(I) = DEPSXX(I) /TIMESTEP
         EPSPYY(I) = DEPSYY(I) /TIMESTEP 
        ENDDO !DO I=1,NEL
      ENDIF
c---------------------------------------------- 
c---------------------------------------------- 
      DO I=1,NEL0
        RBULK(I) = E(I)*CUN
        SHEAR(I) = E(I)*CDEUX
        GS(I)    = SHEAR(I)*SHF(I)
        IF (GOLD(I) == ZERO) GOLD(I) = SHEAR(I)
        LAMDA(I) = E(I)*CTROIS
        G2(I)    = DEUX*SHEAR(I)           
        SIGOZZ(I) = UVAR(I,37) ! DOIT ETRE NULLE SI TT EST BON
        GZ(I)     = FINTER(1, TOTFRAC(I),NPFG,TGZ,DYDXGZ(I))! used to compute plastic strain rate

        ! EO => deviator of epsilon elastic old  (e_el_n)
        !--------------- ----------- -----------   ------        
        EELOXX(I)  = UVAR(I,11)-UVAR(I,29) ! epsold-epsoldpl
        EELOYY(I)  = UVAR(I,12)-UVAR(I,30)
        EELOZZ(I)  = UVAR(I,13)-UVAR(I,31)
        EELOXY(I)  = UVAR(I,14)-UVAR(I,32)
        TREO(I)    = THIRD*(EELOXX(I) +EELOYY(I) +EELOZZ(I) )
        EOXX(I)    = EELOXX(I) - TREO(I) !OLD DEVIATOR OF ELASTIC DEFORMATION
        EOYY(I)    = EELOYY(I) - TREO(I)
        EOZZ(I)    = EELOZZ(I) - TREO(I)
        EOXY(I)    = EELOXY(I)
        ! --------        
        ! PLASTIC        
        ! --------        
        DEPSPLOZZ(I)=-(DEPSXX(I)+DEPSYY(I)) + DEPSTH(I) 
        ! ELASTIC 
        ! --------        
        DEPSELOZZ(I)= -NU/(UN-NU)*(EELOXX(I)+DEPSXX(I)
     .                           + EELOYY(I)+DEPSYY(I)) + DEPSTH(I)     
        !INITIALISATION OF DEPSZZ FOR ALGO DE PLANE STRESS
        !----  ----------- -------------------------------       
        DEPSZZ(I)   = DEPSELOZZ(I)
        EPSZZ(I)    = UVAR(I,13)+DEPSZZ(I)       
        !----  ----------- -------------------------------       
        DEPSZZ(I)  = DEPSZZ(I)  - DEPSTH(I) - DEPSTR(I) 
        UVAR(I,42) = UVAR(I,42) + DEPSTH(I) + DEPSTR(I)
      ENDDO
c---------------------------------------------- 
c---------------------------------------------- 
      DO I=1,NEL0
      IF (OFF(I)== UN )THEN 
c---------------------------------------------- 
c---------------------------------------------- 
       LNF2 = ZERO 
       LNF3 = ZERO
       LNF4 = ZERO
       LNF5 = ZERO !used in plast strain increment local yield
       IF(FRAC2(I)>UVAR(I,3).AND. UVAR(I,3) > ZERO) LNF2 = LOG(FRAC2(I)/UVAR(I,3) )
       IF(FRAC3(I)>UVAR(I,4).AND. UVAR(I,4) > ZERO) LNF3 = LOG(FRAC3(I)/UVAR(I,4) )
       IF(FRAC4(I)>UVAR(I,5).AND. UVAR(I,5) > ZERO) LNF4 = LOG(FRAC4(I)/UVAR(I,5) )
       IF(FRAC5(I)>UVAR(I,6).AND. UVAR(I,6) > ZERO) LNF5 = LOG(FRAC5(I)/UVAR(I,6) )
        !---------------------------------------------- 
        ! DEBUT BOUCLE 1 SUR K plane stress
        !---------------------------------------------- 
       DO K=1,ITERK ! plane stress iteration 
        !estimation of stress tensor 
        !---------------------------
        SIGOM(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I)) * THIRD
        SOXX(I)  = SIGOXX(I)+SIGOM(I) 
        SOYY(I)  = SIGOYY(I)+SIGOM(I) 
        SOZZ(I)  = SIGOZZ(I)+SIGOM(I) 
        !DE (DEVIATOR OD DEPS)
        !---------------------------
        TRDEPS   = THIRD*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I)) 
        DEXX(I)  = DEPSXX(I)-TRDEPS
        DEYY(I)  = DEPSYY(I)-TRDEPS        
        DEZZ(I)  = DEPSZZ(I)-TRDEPS

c        STRIAL_ij deviator of sigma_n+1=sigm_n+ELa DEPS
        !-----------------------------------------------
        STRIALXX(I) = G2(I)*DEXX(I)+SHEAR(I)*SOXX(I)/GOLD(I)
        STRIALYY(I) = G2(I)*DEYY(I)+SHEAR(I)*SOYY(I)/GOLD(I)
        STRIALZZ(I) = G2(I)*DEZZ(I)+SHEAR(I)*SOZZ(I)/GOLD(I)
        STRIALXY(I) = G2(I)*DEPSXY(I)+SHEAR(I)*SIGOXY(I)/GOLD(I)
        SIGNYZ(I)   = SIGOYZ(I)+GS(I) *DEPSYZ(I)
        SIGNZX(I)   = SIGOZX(I)+GS(I) *DEPSZX(I)

        SVMO        = SQRT(TDEMI*(SOXX(I)*SOXX(I)
     .                +     SOYY(I)* SOYY(I)
     .                +     SOZZ(I)* SOZZ(I)
     .                +DEUX*SIGOXY(I)*SIGOXY(I)) ) 

        SVMTR(I)    = SQRT(TDEMI*(STRIALXX(I)*STRIALXX(I)
     .                +     STRIALYY(I)* STRIALYY(I) 
     .                +     STRIALZZ(I)* STRIALZZ(I)+
     .                DEUX*(STRIALXY(I)* STRIALXY(I)) ) )

        !EODE denoted g in the doc equations
        EODEXX(I) = EOXX(I) + DEXX(I)
        EODEYY(I) = EOYY(I) + DEYY(I)
        EODEZZ(I) = EOZZ(I) + DEZZ(I) 
        EODEXY(I) = EOXY(I) + DEPSXY(I) 
        NORMDEV(I) = SQRT(STRIALXX(I)*STRIALXX(I)
     .                +   STRIALYY(I)* STRIALYY(I) 
     .                +   STRIALZZ(I)* STRIALZZ(I))/E(I)
        !-------------------------------------
        ! check if local or global yield
        !-------------------------------------
        IF (SVMTR(I) <= YLDOLD(I)) THEN !.AND.HEATFLAG==0
          ! LOCAL YIELD ALGORITHM
          !----------------------         
          IF (TOTFRAC(I)==ZERO)THEN
            LOGZ = UN
          ELSE
            LOGZ = LOG(TOTFRAC(I))
          ENDIF
          T2 = ZERO
          IF(TEMPEL(I)< UVAR(I,34).AND.NORMDEV(I)> EM15) ! cooling
     .      T2 = DEUX*(ALFA1-ALFA2)*TOTFRAC(I)*LOGZ* ABS(TEMPEL(I)-TEMPO(I) ) 
!                             la valeur abs ce terme sera negatif forcement donc reduit la def plastic            
          IF (  TOTFRACOLD(I) ==ZERO)THEN !totfrac old
            LOGZM1 = UN
          ELSE
            LOGZM1 = LOG( TOTFRACOLD(I))
          ENDIF       
          RH(I)= SVMO / YLDOLD(I)!compute ratio for h_trial
          IF (RH(I)>UNDEMI)THEN 
            HFCT(I)= UN+SEDEMI*(SVMO/YLDOLD(I)-UNDEMI) 
          ELSE
            HFCT(I)= UN
          ENDIF
          T3 = ZERO
          IF(NORMDEV(I)> EM15)
     .       T3 = DEUX*DETH12(I)*HFCT(I)*ABS((TOTFRAC(I)*(UN-LOGZ)- TOTFRACOLD(I)*(UN-LOGZM1)))
          IF (UVAR(I,3)==ZERO)THEN
            PHI2 = ZERO
            PSI2 = ZERO
          ELSE
            PHI2 = (FRAC2(I)*DYDX2(I)*TETA2*LNF2)/(UN+LNF2) 
            PSI2 = (LNF2*(TETA2*PLA1(I)-PLA2(I)))/(UN+LNF2) 
          ENDIF
          IF (UVAR(I,4)==ZERO)THEN
            PHI3 = ZERO
            PSI3 = ZERO
          ELSE
            PHI3 = (FRAC3(I)*DYDX3(I)*TETA3*LNF3)/(UN+LNF3)
            PSI3 = (LNF3*(TETA3*PLA1(I)-PLA3(I)))/(UN+LNF3) 
          ENDIF
          IF (UVAR(I,5)==ZERO)THEN
            PHI4 = ZERO
            PSI4 = ZERO
          ELSE
            PHI4 = (FRAC4(I)*DYDX4(I)*TETA4*LNF4)/(UN+LNF4)
            PSI4 = (LNF4*(TETA4*PLA1(I)-PLA4(I)))/(UN+LNF4)
          ENDIF
          IF (UVAR(I,6)==ZERO)THEN
            PHI5 = ZERO
            PSI5 = ZERO
          ELSE 
            PHI5 = (FRAC5(I)*DYDX5(I)*TETA5*LNF5)/(UN+LNF5) 
            PSI5 = (LNF5*(TETA5*PLA1(I)-PLA5(I)))/(UN+LNF5)
          ENDIF
          T4 = ZERO
          T5 = ZERO
          IF(TOTFRAC(I) /= UN) THEN
            T4 = DYDX1(I)+(PHI2+PHI3+PHI4+PHI5)/(UN-TOTFRAC(I))                      
            T5 = FRAC2(I)*DYDX2(I)*PSI2 +FRAC3(I)*DYDX3(I)*PSI3  
     .         + FRAC4(I)*DYDX4(I)*PSI4 + FRAC5(I)*DYDX5(I)*PSI5    
          ENDIF  
          FLAG=0
 300      SVM(I) = SVMO
          !---------------------------------
          DO K2= 1,ITERK2 ! ESF ITERATION 
          !---------------------------------
           T1 = ZERO
           IF(SVM(I)>UVAR(I,15).AND.NORMDEV(I)> EM15) ! svm is not saved in uvar if it decreases
     .     T1 = (UN-TOTFRAC(I))*GZ(I)*(SVM(I)-SVMO)/E(I) ! SVMI
           !compute dpla depending on the value of H
           IF (HFCT(I)/= UN.AND. T4 /= ZERO.AND. TOTFRAC(I) /= UN)THEN           
             A1   = (  YLDOLD(I) + T5 - T4 * (T1 + T2 - TQUART * T3 ))/ T4
             A2   = (( T1 + T2 - TQUART*T3) * (T5+ YLDOLD(I))
     .                + SEDEMI * T3 * SVM(I))/T4 
             SOL1 = SQRT(MAX(ZERO,A1*A1/QUATRE+A2))-A1*UNDEMI
             SOL2 =-SQRT(MAX(ZERO,A1*A1/QUATRE+A2))-A1*UNDEMI
             DPLA(I) = MAX(SOL1,SOL2)    
             DPLA(I) = MAX(ZERO,DPLA(I))    
           ELSE
             DPLA(I) = T1+T2+T3             
           ENDIF
           ! COMPUTE DEVIATORIC STRESS
           !Y1(I)=Y1INI(I)  
           IF(TOTFRAC(I) /= UN) 
     .      Y1(I)=Y1(I)+DYDX1(I)*DPLA(I)/(UN-TOTFRAC(I))!NEW YIELD OF PHASE 1
           ! s_n+1
           DENO=UN/G2(I)+(TDEMI*DPLA(I)/Y1(I) )
           SXX(I)= EODEXX(I)/DENO
           SYY(I)= EODEYY(I)/DENO
           SZZ(I)= EODEZZ(I)/DENO
           SXY(I)= EODEXY(I)/DENO
           ! NEW EFFECTIVE STRESS
           DENO=TDEMI*DPLA(I)/Y1(I)         
           DEPLXX(I)=DENO*SXX(I)
           DEPLYY(I)=DENO*SYY(I) 
           DEPLZZ(I)=DENO*SZZ(I)
           DEPLXY(I)=DENO*SXY(I)

           EPLXX(I)=UVAR(I,29) + DEPLXX(I) 
           EPLYY(I)=UVAR(I,30) + DEPLYY(I) 
           EPLZZ(I)=UVAR(I,31) + DEPLZZ(I) 
           EPLXY(I)=UVAR(I,32) + DEPLXY(I) 
         
           PPXX=EODEXX(I)-DEPLXX(I)
           PPYY=EODEYY(I)-DEPLYY(I)
           PPZZ=EODEZZ(I)-DEPLZZ(I)
           PPXY=EODEXY(I)-DEPLXY(I)
           PP=PPXX*PPXX+PPYY*PPYY+PPZZ*PPZZ+DEUX*
     .         (PPXY*PPXY)
           ESF=ABS(SVM(I)*SVM(I)-SIX*SHEAR(I)*SHEAR(I)*PP) ! esfi
           CRIT= (EP03*SVMO)*(EP03*SVMO)/E(I)/E(I)
           IF (ESF >CRIT.OR.K2< 3)THEN !
             IF (K2==1 )THEN
               SVM(I)= SVMTR(I)
               SVMIM1(I)=SVMO
               ESFIM1=ESF
             ELSE
               SVMI(I)=SVM(I)
               TEST=ESF-ESFIM1
               IF (TEST/=ZERO)then 
                  SVM(I)=SVM(I)-ESF*(SVM(I)-SVMIM1(I))/TEST
               ELSE
                   SVM(I)=SVM(I)-ESF*(SVM(I)-SVMIM1(I))/EM10 
               ENDIF
               SVMIM1(I)=SVMI(I)
               ESFIM1=ESF
             ENDIF
           ELSE
             GOTO 400
           ENDIF           
          ENDDO ! ITERK2  ESF ITERATION 
          !DPLA(I) = MAX(ZERO,DPLA(I))    

 400      EPSZZ(I)=UVAR(I,13)+DEPSZZ(I)
          SIGM(I)=RBULK(I)*(TROIS*TREO(I)+DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
          SIGNXX(I)=SXX(I)+SIGM(I)                                                
          SIGNYY(I)=SYY(I)+SIGM(I)                                                
          SIGNZZ(I)=SZZ(I)+SIGM(I)                                                
          SIGNXY(I)=SXY(I)                                                        
          ! COMPUTE EFFECTIVE PLASTIC DEFORMATION FOR EACH PHASE 
          DPLA1(I)= ZERO                 
          IF(TOTFRAC(I) /= UN) DPLA1(I)= DPLA(I)/(UN-TOTFRAC(I))    
          IF (UVAR(I,3)==ZERO)THEN
            DPLA2(I) = ZERO 
          ELSE
            DPLA2(I) = MAX(ZERO,LNF2*DPLA1(I)*TETA2/(UN+LNF2)+PSI2)                           
          ENDIF
          IF (UVAR(I,4)==ZERO)THEN
            DPLA3(I) = ZERO 
          ELSE
            DPLA3(I) = MAX(ZERO,LNF3*DPLA1(I)*TETA3/(UN+LNF3)+PSI3)                      
          ENDIF
                                
          IF (UVAR(I,5)==ZERO)THEN
            DPLA4(I) = ZERO 
          ELSE     
            DPLA4(I)= MAX(ZERO,LNF4*DPLA1(I)*TETA4/(UN+LNF4)+PSI4)    
          ENDIF  
          IF (UVAR(I,6)==ZERO)THEN   
            DPLA5(I) = ZERO 
          ELSE                        
            DPLA5(I)= MAX(ZERO,LNF5*DPLA1(I)*TETA5/(UN+LNF5)+PSI5) 
          ENDIF
          YIELD(I)= YLDOLD(I)+ FRAC2(I)*DYDX2(I)*DPLA2(I)+                 
     .     FRAC3(I)*DYDX3(I)*DPLA3(I)+FRAC4(I)*DYDX4(I)*DPLA4(I)+                  
     .     FRAC5(I)*DYDX5(I)*DPLA5(I)+FRAC1(I)*DYDX1(I)*DPLA1(I)
          RHNEW=SVM(I)/MAX(EM20,YIELD(I))                                                  
          HFCTN=UN                                                                
          IF (RHNEW > UNDEMI) HFCTN =UN+SEDEMI*(RHNEW-UNDEMI)                     
          IF (HFCTN == UN)THEN                                                    
            IF(HFCT(I) /= UN)THEN                                                 
              HFCT(I) =UN                                                         
              IF (FLAG == 0) THEN
                FLAG = 1
                GOTO 300            
              ENDIF                                                                 
            ENDIF                                                                 
          ELSE                                                                    
            IF (HFCT(I) == UN)THEN                                                
              HFCT(I) =UN+SEDEMI*(RH(I)-UNDEMI)                                   
              IF (FLAG == 0) THEN
                FLAG = 1
                GOTO 300           
              ENDIF                                                                 
            ENDIF                                                                 
          ENDIF     
c-------------------------------------
        ELSE!IF (SVMTR(I) > YLDOLD(I))THEN 
          ! GLOBAL YIELD
c-------------------------------------
          ! COMPUTE DLAMBDA
          EODE = SQRT(EODEXX(I)*EODEXX(I)+       EODEYY(I)*EODEYY(I)
     .              + EODEZZ(I)*EODEZZ(I)+DEUX*( EODEXY(I)*EODEXY(I))) ! NORM OF g
C
          IF (UVAR(I,3)==ZERO)THEN
            PHI2 = ZERO
            PSI2 = ZERO
          ELSE
            PSI2 = MAX(ZERO,(LNF2*(TETA2*PLA1(I)-PLA2(I)))/(UN+LNF2)  )
            PHI2 =(UN+TETA2*LNF2)/(UN+LNF2) 
          ENDIF
          IF (UVAR(I,4)==ZERO)THEN
            PHI3 = ZERO
            PSI3 = ZERO
          ELSE
            PSI3 = MAX(ZERO,(LNF3*(TETA3*PLA1(I)-PLA3(I)))/(UN+LNF3) )
            PHI3 =(UN+TETA3*LNF3)/(UN+LNF3) 
          ENDIF
          IF (UVAR(I,5)==ZERO)THEN
            PHI4 = ZERO
            PSI4 = ZERO
          ELSE
            PSI4 = MAX(ZERO, (LNF4*(TETA4*PLA1(I)-PLA4(I)))/(UN+LNF4) ) 
            PHI4 =(UN+TETA4*LNF4)/(UN+LNF4)
          ENDIF      
          IF (UVAR(I,6)==ZERO)THEN
            PHI5 = ZERO
            PSI5 = ZERO
          ELSE
            PSI5 = MAX(ZERO,(LNF5*(TETA5*PLA1(I)-PLA5(I)))/(UN+LNF5) )
            PHI5 = (UN+TETA5*LNF5)/(UN+LNF5)
          ENDIF
          ALAMBDA= SQRT(DTIERS)*(YLDOLD(I)+FRAC2(I)*DYDX2(I)*PSI2+ 
     .              FRAC3(I)*DYDX3(I)*PSI3+FRAC4(I)*DYDX4(I)*PSI4+
     .              FRAC5(I)*DYDX5(I)*PSI5)

          BLAMBDA=SQRT(DTIERS)*(FRAC1(I)*DYDX1(I)+FRAC2(I)*DYDX2(I)*PHI2
     .                    +FRAC3(I)*DYDX3(I)*PHI3+FRAC4(I)*DYDX4(I)*PHI4
     .                    +FRAC5(I)*DYDX5(I)*PHI5)
     
          DPLA(I)= MAX(ZERO,(G2(I)*EODE-ALAMBDA)/(SQRT(SIX)*SHEAR(I)+BLAMBDA) )
          ! COMPUTE NORM OF s_n+1 THEN  EFFECTIVE STRESS 
          SEFF(I)=SQRT(TDEMI)*(G2(I)*(EODE-SQRT(TDEMI)*DPLA(I)))
          ! s_n+1
          DENO = UN/G2(I)+(TROIS*DPLA(I)/(MAX(EM20,DEUX*SEFF(I))))
          SXX(I)= EODEXX(I)/DENO
          SYY(I)= EODEYY(I)/DENO
          SZZ(I)= EODEZZ(I)/DENO
          SXY(I)= EODEXY(I)/DENO

          DENO=TDEMI*DPLA(I)/MAX(EM20,SEFF(I))
          DEPLXX(I)=DENO*SXX(I)
          DEPLYY(I)=DENO*SYY(I)
          DEPLZZ(I)=DENO*SZZ(I)
          DEPLXY(I)=DENO*SXY(I)
          
          EPLXX(I)=UVAR(I,29) + DEPLXX(I) 
          EPLYY(I)=UVAR(I,30) + DEPLYY(I) 
          EPLZZ(I)=UVAR(I,31) + DEPLZZ(I) 
          EPLXY(I)=UVAR(I,32) + DEPLXY(I) 


          !STRESS NEW 
          EPSZZ(I)=UVAR(I,13)+DEPSZZ(I)
          SIGM(I)=RBULK(I)*(TROIS*TREO(I)+DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
C          SIGM(I) = RBULK(I)*(EPSXX(I)+EPSYY(I)+EPSZZ(I)-DEPSTH(I)) !   
          SIGNXX(I)=SXX(I)+SIGM(I)                                                
          SIGNYY(I)=SYY(I)+SIGM(I)                                                
          SIGNZZ(I)=SZZ(I)+SIGM(I)                                                
          SIGNXY(I)=SXY(I)             

          ! DPLA AND PLA OF EACH PHASE 
          DPLA1(I)= DPLA(I)
          IF (UVAR(I,3)>ZERO)THEN 
            DPLA2(I)= DPLA(I) *PHI2 + PSI2
          ENDIF

          IF (UVAR(I,4)>ZERO)THEN 
            DPLA3(I)= DPLA(I) *PHI3 + PSI3
          ENDIF
          
          IF (UVAR(I,5)>ZERO)THEN 
            DPLA4(I)= DPLA(I) *PHI4 + PSI4
          ENDIF
          
          IF (UVAR(I,6)>ZERO)THEN 
            DPLA5(I)= DPLA(I) *PHI5 + PSI5        
          ENDIF
          YIELD(I)= YLDOLD(I) + FRAC2(I)*DYDX2(I)*DPLA2(I)+                         
     .    FRAC3(I)*DYDX3(I)*DPLA3(I)+FRAC4(I)*DYDX4(I)*DPLA4(I)+                  
     .    FRAC5(I)*DYDX5(I)*DPLA5(I)+FRAC1(I)*DYDX1(I)*DPLA1(I)
        ENDIF ! GLOBAL OR LOCAL YIELD

C ----------------------------------------------------------
        !check plane stress condition         
        IF(ABS(SIGNZZ(I))>EM10.OR.K< 3)THEN 
          IF (K == 1) THEN                                       
           DEPSZZ(I)  = DEPSPLOZZ(I)                             
           DEPSIM1(I) = DEPSELOZZ(I)                                       
           SIGZIM1(I) = SIGNZZ(I) 
          ELSE       
           TEST =  SIGNZZ(I)-SIGZIM1(I)                                           
           DEPSI(I) = DEPSZZ(I)                                
           IF (TEST/=ZERO)THEN
             DEPSZZ(I) = DEPSZZ(I)-SIGNZZ(I) *(DEPSZZ(I)-DEPSIM1(I))/  
     .         (SIGNZZ(I)-SIGZIM1(I))
           ELSE
             DEPSZZ(I) = DEPSZZ(I)-SIGNZZ(I) *(DEPSZZ(I)-DEPSIM1(I))/  
     .         EM10       
           ENDIF
c           CONV = UN
c           IF (DEPSZZ(I)/=ZERO)
c     .      CONV=ABS(DEPSI(I)-DEPSIM1(I))/ABS(DEPSZZ(I))
           DEPSIM1(I) = DEPSI(I)                                
           SIGZIM1(I) = SIGNZZ(I)       
           !COMPUTE SECANT EXPRESSION                            
          ENDIF 
        ELSE
           EXIT              
        ENDIF  
       ENDDO ! plane stress iteration line 461
       ENDIF ! OFF == 1
      ENDDO ! NEL  
c---------------------------------------------- 

      DO I=1,NEL0
       IF (OFF(I) == UN )THEN 
         PLA(I)  = PLA(I)+  DPLA(I)
         PLA1(I) = PLA1(I) +DPLA1(I) 
         PLA2(I) = PLA2(I) +DPLA2(I) 
         PLA3(I) = PLA3(I) +DPLA3(I) 
         PLA4(I) = PLA4(I) +DPLA4(I) 
         PLA5(I) = PLA5(I) +DPLA5(I) 
         !UVAR(I,11)=UVAR(I,11) +DEPSXX(I) 
         !UVAR(I,12)=UVAR(I,12) +DEPSYY(I) 
         !UVAR(I,13)=UVAR(I,13) +DEPSZZ(I) 
         !UVAR(I,14)=UVAR(I,14) +DEPSXY(I)

         UVAR(I,11)=EPSXX(I) 
         UVAR(I,12)=EPSYY(I) 
         UVAR(I,13)=EPSZZ(I) 
         UVAR(I,14)=EPSXY(I)
c         EPSP(I)=SQRT(DTIERS*(EPSXX(I)*EPSXX(I)+EPSYY(I)*EPSYY(I)+
c     .     EPSZZ(I)*EPSZZ(I)+DEUX*(EPSXY(I)*EPSXY(I))  ))
         FACCS=UN 
         IF(CEPS/=ZERO.AND.PEPS/=ZERO.AND.EPSP(I)>CEPS)
     .      FACCS=UN+ (EPSP(I)/CEPS)**(UN/PEPS)                                                     
         YIELD(I)= YIELD(I)*FACCS
         UVAR(I,17)= PLA1(I) 
         UVAR(I,18)= PLA2(I)
         UVAR(I,19)= PLA3(I)
         UVAR(I,20)= PLA4(I)
         UVAR(I,21)= PLA5(I)
         UVAR(I,37)= SIGNZZ(I)
         UVAR(I,35)= VOL(I)

         !UVAR(I,22)= TOTFRAC(I) pas besoin de stocker recalcule au debut avec fraci
         UVAR(I,8) = TEMPEL(I)
         IF(UVAR(I,15)<SVM(I))   UVAR(I,15)=SVM(I)
         IF(UVAR(I,34)>TEMPEL(I))UVAR(I,34)=TEMPEL(I)
                 
         UVAR(I,23)= FRAC2(I)/XEQ2       !X2(I)
         UVAR(I,24)= FRAC3(I)/(UN-XEQ2)  !X3(I)
         UVAR(I,25)= FRAC4(I)/MAX(EM20,UN- (FRAC2(I)/XEQ2))!X4(I)
         UVAR(I,44)= FRAC5(I)/MAX(EM20,XGAMA(I))

         !UVAR(I,23)= X2(I)
         !UVAR(I,24)= X3(I)
         !UVAR(I,25)= X4(I)

         VOLI = THKLY(I)*AREA(I)
         

         UVAR(I,2)= FRAC1(I)
         UVAR(I,3)= FRAC2(I)
         UVAR(I,4)= FRAC3(I)
         UVAR(I,5)= FRAC4(I)
         UVAR(I,6)= FRAC5(I)
         
         UVAR(I,36)= SHEAR(I)
         UVAR(I,29)= EPLXX(I)
         UVAR(I,30)= EPLYY(I)
         UVAR(I,31)= EPLZZ(I)
         UVAR(I,32)= EPLXY(I)

         UVAR(I,10)= XGAMA(I)

         HARD(I)= ZERO

        ENDIF ! off
         THK(I) = THK(I) + DEPSIM1(I)*THKLY(I)*OFF(I)
         SSPSHELL = SQRT(E(I)/(UN - NU*NU)/RHO0(I)) 
         SSPSOL = SQRT((RBULK(I)+QTHIRD*SHEAR(I))/RHO0(I))
         SOUNDSP(I) =MAX(SSPSHELL,SSPSOL)
         IF (DPLA(I)>ZERO)THEN
          H(I)=(YIELD(I)-UVAR(I,9))/DPLA(I)
          H(I)=MAX(EM10,H(I))
          ETSE(I)=H(I)/(H(I)+E(I))
          UVAR(I,28)=ETSE(I)
         ELSE
           ETSE(I)=UVAR(I,28)
         ENDIF
         YLD(I)=YIELD(I)
         UVAR(I,9)=YIELD(I)
        
         DEPSXY(I) = DEPSXY(I)*DEUX
         EPSXY(I)  = EPSXY(I) *DEUX     
      ENDDO
      IF (ALFA1/= ZERO .OR. ALFA2 /= ZERO) THEN !compute internal thermal energy
        DO I=1,NEL0        
         !SIGKK(I)  = SIGNXX(I)+SIGNYY(I)+SIGOXX(I)+SIGOYY(I)
         !EINTTH(I) = EINTTH(I)-UNDEMI*SIGKK(I)*DEPSTH(I)
        ENDDO
      ENDIF    
      IF(HEATFLAG == 0) THEN
       DO I=1,NEL0
        IF (OFF(I) == UN )THEN 
          IF (TEMPEL(I)<MS.AND.TEMPEL(I)<= UVAR(I,8) .AND. TEMPEL(I)<=1073.0)THEN !
           VR(I)   =  ZERO
           HARD(I) =  UVAR(I,7)
           IF (UVAR(I,16) >UVAR(I,26))THEN
               VR(I)  = (UVAR(I,27)-UVAR(I,33))*UNITT/(UVAR(I,16)-UVAR(I,26))
               HARD(I)= (FRAC2(I)+FRAC3(I))*HFP*LOG10(VR(I))+FRAC4(I)*HB+FRAC5(I)*HM    
               UVAR(I,7)= HARD(I)
           ENDIF
          ENDIF
          DIE(I) =DIE(I)+ (LAT2*(FRAC5(I)-UVAR(I,6)) +
     .                     LAT1*(FRAC2(I)-UVAR(I,3)
     .             +             FRAC3(I)-UVAR(I,4)
     .             +             FRAC4(I)-UVAR(I,5) ) ) *VOLI  
   
         ENDIF !(OFF(I) == UN ) 
       ENDDO
      ENDIF
      RETURN
      END
C
c=======================================================================
c=======================================================================
c   SUBROUTINE TO COMPUTE VOLUME FRACTION EVOLTUTION
c=======================================================================
c=======================================================================
c   PHASEFRAC                     src/mate80/sigeps80c.F 
c-- called by -----------
c     SIGEPS80C.F
c=======================================================================
Chd|====================================================================
Chd|  PHASEFRAC                     source/materials/mat/mat080/sigeps80c.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PHASEFRAC(NEL0,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,
     .  FCPER,FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .  QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRAC,TIMESTEP)
     
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL0
        my_real
     .        TEMPEL(NEL0),TEMPMIN(NEL0),TIMESTEP
        my_real
     .        TIME,ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,
     .        FGRAIN,QR2,QR3,QR4,KPER,KBAIN,XEQ2,XEQ4,YX
C-----------------------------------------------
        my_real
     .        FRAC1(*),FRAC2(*),FRAC3(*),FRAC4(*),FRAC5(*)
        my_real
     .        X2(*),X3(*),X4(*),X5(*),TOTFRAC(*),XGAMA(*)
        my_real
     .        FTEMP,UX,VX,UDOT,VDOT,F,CONST,FDOT,X2OLD(NEL0),
     .        X3OLD(NEL0),X4OLD(NEL0),GX,GDOT,DTI
        INTEGER I,K,J
  
      ! SUM OF FRAC1+FRAC2+FRAC3+FRAC4+FRAC5=1--------
       DTI= UN/MAX(TIMESTEP,EM10)
      DO I=1,NEL0
       ! CHECK TEMPERATURE FOR CORRESPONDING PHASE CHANGE
       IF(TOTFRAC(I)<UN.AND.TEMPEL(I)<TEMPMIN(I))THEN
       IF (TEMPEL(I)<AE3)THEN 
        IF(TEMPEL(I)>AE1)THEN
          IF(X2(I)<0.999)THEN
           ! FERRITE FORMATION X2
           X2OLD(I)=X2(I) 
           IF(X2(I)==ZERO)X2(I)=EM10
           FTEMP=EXP(-QR2/TEMPEL(I))*ABS(TEMPEL(I)-AE3)**3
           CONST=FTEMP*FGRAIN*FCFER
           DO K=1,3
             YX= (UN-X2(I))/MAX(EM10,X2(I))
             UX=X2(I)**(DTIERS*(UN-X2(I)))
             VX=(UN-X2(I))**(DTIERS*X2(I))
             F =(X2(I)-X2OLD(I))*DTI-CONST*UX*VX
             FDOT=DTI-CONST*DTIERS*UX*VX*(YX-UN/YX+LOG(YX))
             X2(I)=MAX(EM20,X2(I)-F/FDOT)
           ENDDO
           FRAC2(I)=X2(I)*XEQ2
           FRAC1(I)=UN-FRAC2(I)
          ENDIF
c---------------------------------          
        ELSEIF(TEMPEL(I)>BS)THEN 
          
         IF(X3(I)<0.999)THEN
            ! PEARLITE X3                                       
            X3OLD(I)=X3(I)
            IF(X3(I)==ZERO)X3(I)=EM10
            FTEMP=6.17*EXP(-QR3/TEMPEL(I))*ABS(TEMPEL(I)-AE1)**3
            CONST=FTEMP*FGRAIN*FCPER !!! adjusted
            DO K=1,3
              YX= (UN-X3(I))/MAX(EM10,X3(I))
              UX=X3(I)**(DTIERS*(UN-X3(I)))
              VX=(UN-X3(I))**(DTIERS*X3(I))
              F =(X3(I)-X3OLD(I))*DTI-CONST*UX*VX
              FDOT=DTI-CONST*DTIERS*UX*VX*(YX-UN/YX+LOG(YX))
              X3(I)=MAX(EM20,X3(I)-F/FDOT)
            ENDDO       
c            FRAC3(I)=X3(I)*(UN-X2(I))
            FRAC3(I)=X3(I)*(UN-XEQ2)
            FRAC1(I)=UN-FRAC2(I)-FRAC3(I)
            ! CHECK IF FERRITE FORMATION
c            IF (X2(I)<0.99)THEN !X3(I)<0.01 .AND.
c              ! FERRITE EVOLUTION
c              X2OLD(I)=X2(I)
c              IF(X2(I)==ZERO)X2(I)=EM10
c              FTEMP=EXP(-QR2/TEMPEL(I)) *ABS(TEMPEL(I)-AE3)**3
c              CONST=FTEMP*FGRAIN*FCFER
c              DO K=1,3
c                YX= (UN-X2(I))/MAX(EM10,X2(I))
c                UX=X2(I)**(DTIERS*(UN-X2(I)))
c                VX=(UN-X2(I))**(DTIERS*X2(I))
c                F =(X2(I)-X2OLD(I))*DTI-CONST*UX*VX
cc                UDOT=DTIERS*UX*((UN-X2(I))/MAX(EM10,X2(I))-
cc     .              LOG(MAX(EM10,X2(I))))
cc                VDOT=DTIERS*VX*(LOG(UN-X2(I))-X2(I)/(UN-X2(I)))
cc                FDOT=DTI-CONST*(UDOT*VX+VDOT*UX)
c                FDOT=DTI-CONST*DTIERS*UX*VX*(YX-UN/YX+LOG(YX))
c                X2(I)=MAX(EM20,X2(I)-F/FDOT)
c              ENDDO           
c              FRAC2(I)=X2(I)*XEQ2
c              FRAC1(I)=UN-FRAC2(I)-FRAC3(I)
c            ENDIF
          ENDIF
c---------------------------------                    
        ELSEIF(TEMPEL(I)>MS)THEN
          IF(X4(I)<0.999)THEN
           ! BAINITE  FORMATION 
            X4OLD(I)=X4(I)  
            IF(X4(I)==ZERO)X4(I)=X3(I)!EM10!X3(I)
            ! INITIALISE AU TAUX DE PERLITE
            FTEMP=EXP(-QR4/TEMPEL(I)) *(TEMPEL(I)-BS)**2
            CONST=FTEMP*FGRAIN*FCBAI
            DO K=1,4
              YX= (UN-X2(I))/MAX(EM10,X2(I))
              UX=X4(I)**(DTIERS*(UN-X4(I)))
              VX=(UN-X4(I))**(DTIERS*X4(I)) 
              GX=EXP(KBAIN*X4(I)*X4(I))
              IF (GX<ZERO)GX=UN
              F =(X4(I)-X4OLD(I))*DTI-CONST*UX*VX/GX
              UDOT=DTIERS*UX*((UN-X4(I))/MAX(EM10,X4(I))-
     .           LOG(MAX(EM10,X4(I))))
              VDOT=DTIERS*VX*(LOG(UN-X4(I))-X4(I)/(UN-X4(I)))
              GDOT=DEUX*KBAIN*X4(I)*GX
              FDOT=DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X4(I)=MAX(EM20,X4(I)-F/FDOT)
           ENDDO
            FRAC4(I)=X4(I)*(UN-X2(I))
            FRAC1(I)=UN-FRAC2(I)-FRAC3(I)-FRAC4(I)
          ENDIF
            
        ELSE           
          ! MARTENSITE  FORMATION X5
          IF (FRAC5(I)==ZERO)XGAMA(I)= FRAC1(I)
          FRAC5(I)=XGAMA(I)*(UN-EXP(-ALPHA*(MS-TEMPEL(I))))
          FRAC1(I)=UN-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
        ENDIF
       ENDIF   
       ENDIF   
      ENDDO    
     
      RETURN
      END








