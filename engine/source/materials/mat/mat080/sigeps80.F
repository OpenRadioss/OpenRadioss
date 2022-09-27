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
Chd|  SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        KIRKALDYKINETICS              source/materials/mat/mat080/sigeps80.F
Chd|        PHASEKINETIC2                 source/materials/mat/mat080/sigeps80.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS80 (
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOL    ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPM    ,
     B     MAT    ,EPSP   ,YLD     ,PLA    ,TABLE,  TEMP,  
     C     NVARTMP,VARTMP ,TRDEPSTH,EINTTH)
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
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL F
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
C RHO0    | NEL    | F | R | INITIAL DENSITY
C AREA    | NEL    | F | R | AREA
C EINT    | 2*NEL  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL    | F | R | LAYER THICKNESS
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL    | F | R | STRAIN XX
C EPSYY   | NEL    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL    | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
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
      INTEGER NEL, NUPARAM, NUVAR,NVARTMP, NPT0, IPT,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*),ITHK
      my_real TIME,TIMESTEP,UPARAM(*),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),RHO(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   VOL(NEL) ,TEMP(NEL),
     .   DIE(NEL),COEF(NEL) 
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL) 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL,NUVAR),EPSP(NEL),
     .   OFF(NEL),THK(NEL),YLD(NEL),EINTTH(NEL)
      INTEGER :: VARTMP(NEL,NVARTMP)

      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
c-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NRATE(NEL),II,J1,J2,N,NINDX,IADBUF,NINDXGLOB,NINDXLOC,
     .        NMAX,INDEX(NEL),K2,ITERK2,ITER,NITER,NPFG(2),ITABLE(5),
     .        EFUNC,IFUNC(5),ITERK,FLAG,NTAB,HEATFLAG,HEATOPTION,
     .        NIHEAT,NICOOL,FLAG_LOC ,LOCAL,FLAG_TR_STRAIN,FLAG_TR_KINETICS

      INTEGER, DIMENSION(NEL)   :: INDXLOC,INDXGLOB
      my_real
     .        CUN ,CDEUX,CTROIS,DENO,EFAC,
     .        ALAMBDA,BLAMBDA,CEPS,PEPS,SOL1,SOL2,
     .        YSCALE(5),TETA2,TETA3,TETA4,TETA5,QR2,QR3,QR4,PP,
     .        ALFA1,ALFA2,T2,T3,T4,T5,T1,VOLI,XFAC(5),RSCALE(5),
     .        PPXX,PPYY,PPZZ,PPXY,PPYZ,PPZX,ESFIM1,
     .        ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,FG,
     .        E1PO(NEL),RHNEW,EODE,FGRAIN,KPER,HFCTN,KBAIN,XEQ2,
     .        XEQ4,LAT1,LAT2,HFP,HB,HM,VOL0,TINI,UNITT,
     .        TGZ(12),CRIT,TEST, HEATFLAG1,DXRHO,
     .        CONV,HUITCENT,SSPSHELL,SSPSOL,QPTT,SLOPE,DYDXR,
     .         TAU1, TAU3,
     .         GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .         GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .         FAC,DSVM,
     .         NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,DDLAM,DFDSIG_DSIG,DPLA_DLAM,DDEP

      my_real
     .        E(NEL),RBULK(NEL),SHEAR(NEL),G2(NEL),TEMPEL(NEL),
     .        LAMDA(NEL),DYDXGZ(NEL),DYDXE(NEL),
     .        FRAC1(NEL),FRAC2(NEL),FRAC3(NEL)  ,FRAC4(NEL), 
     .        FRAC5(NEL),TOTFRAC(NEL),TOTFRACOLD(NEL) ,SOXX(NEL) ,
     .        GZ(NEL)   ,TREO(NEL)   ,SIGOM(NEL),SOYY(NEL),SOZZ(NEL),
     .        DYDX1(NEL),DYDX2(NEL)  ,DYDX3(NEL),
     .        DYDX4(NEL),DYDX5(NEL)  ,YIELD(NEL),
     .        Y1(NEL)   ,Y2(NEL),Y3(NEL),Y4(NEL),Y5(NEL),
     .        STRIALXX(NEL),STRIALYY(NEL),STRIALZZ(NEL),
     .        STRIALXY(NEL),STRIALYZ(NEL),STRIALZX(NEL),SVM(NEL),
     .        EOXX(NEL),EOYY(NEL),EOZZ(NEL),EPSTH(NEL),
     .        EOXY(NEL),EOYZ(NEL),EOZX(NEL),
     .        EODEXX(NEL),EODEYY(NEL),EODEZZ(NEL),
     .        EODEXY(NEL),EODEZX(NEL),EODEYZ(NEL),Y1INI(NEL),
     .        EPSOXX(NEL),EPSOYY(NEL),EPSOZZ(NEL),
     .        EPSOXY(NEL),EPSOYZ(NEL),EPSOZX(NEL),
     .        DPLA(NEL)  ,SNORM(NEL),SEFF(NEL),
     .        SIGM(NEL)  ,SXX(NEL),SYY(NEL),SZZ(NEL),
     .        SXY(NEL)   ,SYZ(NEL),SZX(NEL)   ,
     .        DEPLXX(NEL),DEPLYY(NEL),DEPLZZ(NEL),DEPLXY(NEL),DEPLYZ(NEL),DEPLZX(NEL),
     .        DPLA1(NEL),DPLA2(NEL),DPLA3(NEL),DPLA4(NEL),DPLA5(NEL),
     .        PLA1(NEL) ,PLA2(NEL) ,PLA3(NEL) ,PLA4(NEL) ,PLA5(NEL),
     .        DEPSELZZ(NEL),DEPSPLZZ(NEL),DEPSIM1(NEL),DEPSI(NEL),
     .        DEPSIP1(NEL),SIGZIM1(NEL),SIGI(NEL),
     .        VOLOLD(NEL),RH(NEL),DE12(NEL),YLDOLD(NEL),
     .        DEPSELOZZ(NEL),DEPSPLOZZ(NEL),
     .        SVMI(NEL),TRDEPSTH(NEL),SVMIM1(NEL),SVMTR(NEL),TRDEPS,
     .        DEPSTHXX(NEL),DEPSTHYY(NEL),DEPSTHZZ(NEL),DEPSTH(NEL),
     .        X1(NEL),X2(NEL),X3(NEL),X4(NEL),X5(NEL),HARD(NEL),
     .        DEXX(NEL),DEYY(NEL),DEZZ(NEL),GOLD(NEL),SVMtest(NEL),
     .        EPLXX(NEL),EPLYY(NEL),EPLZZ(NEL) ,EPLXY(NEL),EPLYZ(NEL),EPLZX(NEL),
     .        EELOXX(NEL),EELOYY(NEL),EELOYZ(NEL),EELOZX(NEL),
     .        EELOXY(NEL),EELOZZ(NEL),SIGKK(NEL),
     .        XGAMA(NEL),TEMPMIN(NEL),VR(NEL),H(NEL),TEMPO(NEL),
     .        ZEQ(NEL), TAU(NEL),NORMDEV(NEL),DETH12(NEL),DEPSTR(NEL),
     .        RHO_A(NEL),RHO_F(NEL),RHO_P(NEL),RHO_B(NEL),RHO_M(NEL),RHO_N(NEL)
      my_real, DIMENSION(NEL) ::
     .        A1, A2,FCT,DF,SVMO,DEPSVOL,HFCT,A,B,C,DH,GSIG,DGSIG,ESF,DESF,P,SVMT,
     .        LNF2,LNF3,LNF4,LNF5,FACCS,STXX,STYY,STZZ,STXY,STYZ,STZX,LOGZ,LOGZM1,
     .        DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,PHI2,PHI3,PHI4,PHI5,PSI2 ,PSI3,PSI4,PSI5
      my_real,
     .        DIMENSION(NEL,3) :: XX5,XX1,XX2,XX3,XX4

      INTEGER,DIMENSION(NEL,3) :: IPOS1,IPOS2,IPOS3,IPOS4,IPOS5


c-----------------------------------------------
c     USER VARIABLES INITIALIZATION
c-----------------------------------------------
C     ! 0 < Ac1 < Ac3
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

      NITER = 5
      NTAB  = IPM(226,MAT(1))  

      DO I=1,NTAB
        ITABLE(I) = IPM(226+I,MAT(1)) 
        XFAC(I)  = UPARAM(58+I)
      ENDDO
      DO I=1,NEL
        E(I)       = UPARAM(1)  
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
      HFP    =  UPARAM(42)     
      HB     =  UPARAM(43)
      HM     =  UPARAM(44)
      TINI   =  UPARAM(45) 
      UNITT  =  UPARAM(46)
      
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
            UVAR(I,43)=RHO(I)
            IF(HEATFLAG == 1)THEN
               UVAR(I,2) = ZERO!  ! fraction of austenite
               UVAR(I,3) = ONE ! ! fraction of ferrite
            ELSE
               UVAR(I,2) = ONE! fraction of austenite
            ENDIF
            UVAR(I,8)   = UPARAM(45) !temperature 
            UVAR(I,1)   = UPARAM(45) !TINI
 
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
      DO I=1,NEL
        TEMPO(I)   = UVAR(I,8)
        TEMPMIN(I) = UVAR(I,1) ! ASSURER PHASE CHANGE IF T DECREASE - avoid ossillations
      ENDDO
      
c-------------------------------------
c compute temperature 
c-------------------------------------
C-JTHE = -1 : thermal lagrangian, 0 : no thermal lagrange, 1 : thermal ale
c      IF(JTHE <= 0 ) THEN        
c       DO I=1,NEL
c         IF (UVAR(I,8)<=EP03)THEN
c           CP(I)= 1.117*EP06/(1010.0-TEMPO(I))/(1010.0-TEMPO(I))+12622.
c     .        /(1010.0 - TEMPO(I) )+ 0.3485*TEMPO(I)+ 355.6
c         ELSE
c           CP(I)= 1.225*EP08/((TEMPO(I)-990.)**4)+0.1381*TEMPO(I)
c     .       + 585.7
c         ENDIF
c         TEMP = UPARAM(45)
c         VOL0    = VOL(I) * RHO0(I)
         !TEMPEL(I) = UPARAM(45)  
c     .    +(EINT(I,1)+ EINT(I,2))/VOL(I)/(RHO0(I)*CP(I))
c       ENDDO
c      ENDIF
c-------------------------------------        
      IF(KFUNC(1) > ZERO)THEN
        DO I=1,NEL
         E(I) = FINTER(KFUNC(1),TEMPEL(I),NPF,TF,DYDXE(I))
         E(I)=  E(I) *  EFAC  
        ENDDO
      ENDIF
c-------------------------------------        
      DO I=1,NEL  
        RBULK(I)   = E(I)/THREE/(ONE - TWO*NU)
        SHEAR(I)   = E(I)*HALF/(ONE+NU)
        LAMDA(I)   = E(I)*CTROIS
        G2(I)      = TWO*SHEAR(I)          
        A1(I)      = E(I)*(ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
        A2(I)      = A1(I)*NU/(ONE - NU)
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
c------------------------------------- 
c     PHASE CHANGE calculation
c------------------------------------- 
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
          IF (TEMPEL(I)<= 1073.0 .AND. UVAR(I,26)==ZERO)THEN
            UVAR(I,26)=TIME
            UVAR(I,27)=TEMPEL(I)
          ENDIF
          IF (TEMPEL(I)<= 773.0 .AND. UVAR(I,16)==ZERO)THEN
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
     .    QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX) 
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
        ELSE   !cooling    
          NICOOL = NICOOL+1
          INDEX(NICOOL)=I
          IF (TEMPEL(I)<= 1073.0 .AND. UVAR(I,26)==ZERO)THEN
            UVAR(I,26)=TIME
            UVAR(I,27)=TEMPEL(I)
          ENDIF

          IF (TEMPEL(I)<= 773.0 .AND. UVAR(I,16)==ZERO)THEN
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
     .    FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,QR2,
     .    QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRACOLD,TIMESTEP,NICOOL,INDEX)
        ENDIF
       ENDIF
      ENDIF!FLAG_LOC /= 2

c-----------------------------------------------
c-----------------------------------------------
      DO I=1,NEL
        IF(TEMPO(I)<=TEMPMIN(I))TEMPMIN(I)=TEMPO(I)
        UVAR(I,1) = TEMPMIN(I)  
      ENDDO
c-----------------------------------------------
c-----------------------------------------------
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
     .   DEPSTR(I) =  DETH12(I) * (TOTFRAC(I)-TOTFRACOLD(I))!*LOG(TOTFRAC(I))/ (ONE - TOTFRAC(I))
        ENDDO 
      ENDIF
      DO I=1,NEL
        DEPSXX(I) = DEPSXX(I) - DEPSTH(I) - DEPSTR(I)
        DEPSYY(I) = DEPSYY(I) - DEPSTH(I) - DEPSTR(I) 
        DEPSZZ(I) = DEPSZZ(I) - DEPSTH(I) - DEPSTR(I) 
        !UVAR(I,42) = UVAR(I,42) + DEPSTH(I)+ DEPSTR(I)
       
      ENDDO 
      IF (TIMESTEP /=ZERO) THEN 
        DO I=1,NEL
         EPSPXX(I) = DEPSXX(I) /TIMESTEP
         EPSPYY(I) = DEPSYY(I) /TIMESTEP 
         EPSPZZ(I) = DEPSZZ(I) /TIMESTEP
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
c---------------------------------------------- 
c================================================
c     Mechanical calculation -+ DEPSTH(I)
c================================================
      DO I=1,NEL
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A1(I)*DEPSXX(I)+ A2(I)*(DEPSYY(I) + DEPSZZ(I)) 
        SIGNYY(I) = SIGOYY(I) + A1(I)*DEPSYY(I)+ A2(I)*(DEPSXX(I) + DEPSZZ(I)) 
        SIGNZZ(I) = SIGOZZ(I) + A1(I)*DEPSZZ(I)+ A2(I)*(DEPSXX(I) + DEPSYY(I)) 
        SIGNXY(I) = SIGOXY(I) + SHEAR(I)*DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + SHEAR(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + SHEAR(I)*DEPSZX(I)
        ! Computation of the pressure of the trial stress tensor
        P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
        ! Computation of the Von Mises equivalent stress of the trial stress tensor
        SXX(I) = SIGNXX(I) + P(I)
        SYY(I) = SIGNYY(I) + P(I)
        SZZ(I) = SIGNZZ(I) + P(I)
        SXY(I) = SIGNXY(I)
        SYZ(I) = SIGNYZ(I)
        SZX(I) = SIGNZX(I)
        SVM(I)  = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2)
     .       +         (SXY(I)**2 + SZX(I)**2 + SYZ(I)**2)
        SVM(I)  = SQRT(THREE*SVM(I))

        NORMDEV(I) = SQRT(SXX(I)* SXX(I)
     .                +   SYY(I)* SYY(I) 
     .                +   SZZ(I)* SZZ(I) ) /E(I)


        SIGOM(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I)) * THIRD
        SOXX(I)  = SIGOXX(I)+SIGOM(I) 
        SOYY(I)  = SIGOYY(I)+SIGOM(I) 
        SOZZ(I)  = SIGOZZ(I)+SIGOM(I) 
        SVMO(I)     = SQRT(THREE_HALF*(SOXX(I)*SOXX(I)
     .                +      SOYY(I)* SOYY(I)
     .                +      SOZZ(I)* SOZZ(I)
     .                +TWO*(SIGOXY(I)*SIGOXY(I)
     .                      +SIGOYZ(I)*SIGOYZ(I)
     .                      +SIGOZX(I)*SIGOZX(I)) ) ) 
      ENDDO
c---------------------------------------------- 
c---------------------------------------------- 
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
      
      !========================================================================================
      ! GLOBAL YIELD ALGORITHM
      !========================================================================================
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
        DO ITER = 1,5
         DO II = 1,NINDXGLOB         
            I = INDXGLOB(II)          
            FCT(I) = SVM(I) - YLD(I)
            !df/dsig
            NORMXX  = THREE_HALF * SXX(I) /SVM(I) ! DF/DSIG
            NORMYY  = THREE_HALF * SYY(I) /SVM(I) 
            NORMZZ  = THREE_HALF * SZZ(I) /SVM(I)   
            NORMXY  = TWO * THREE_HALF * SIGNXY(I) /SVM(I) 
            NORMYZ  = TWO * THREE_HALF * SIGNYZ(I) /SVM(I) 
            NORMZX  = TWO * THREE_HALF * SIGNZX(I) /SVM(I)
            ! df/ddlam = DF/DSIG * DSIG/DDLAM
            DF(I) = NORMXX * (A1(I)*NORMXX + A2(I)*(NORMYY+NORMZZ))
     .            + NORMYY * (A1(I)*NORMYY + A2(I)*(NORMXX+NORMZZ))
     .            + NORMZZ * (A1(I)*NORMZZ + A2(I)*(NORMXX+NORMYY))
     .            + NORMXY * NORMXY * SHEAR(I)
     .            + NORMYZ * NORMYZ * SHEAR(I)
     .            + NORMZX * NORMZX * SHEAR(I)

            DF(I) = SIGN(MAX(ABS(DF(I)),EM20) ,DF(I)) 
            DDLAM = FCT(I)/DF(I) 

            !compute derivative of effective plastic strain vs dlam
            DFDSIG_DSIG = SIGNXX(I) * NORMXX + SIGNYY(I) * NORMYY + SIGNZZ(I) * NORMZZ
     .                 +  SIGNXY(I) * NORMXY + SIGNYZ(I) * NORMYZ + SIGNZX(I) * NORMZX 
            DPLA_DLAM = DFDSIG_DSIG / YLD(I)


            !  Plastic strains tensor update
            DPXX(I) = DDLAM * NORMXX  
            DPYY(I) = DDLAM * NORMYY  
            DPZZ(I) = DDLAM * NORMZZ  
            DPXY(I) = DDLAM * NORMXY  
            DPYZ(I) = DDLAM * NORMYZ          
            DPZX(I) = DDLAM * NORMZX  
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (A1(I)*DPXX(I) + A2(I)*(DPYY(I) + DPZZ(I))) 
            SIGNYY(I) = SIGNYY(I) - (A1(I)*DPYY(I) + A2(I)*(DPXX(I) + DPZZ(I)))
            SIGNZZ(I) = SIGNZZ(I) - (A1(I)*DPZZ(I) + A2(I)*(DPXX(I) + DPYY(I)))
            SIGNXY(I) = SIGNXY(I) - SHEAR(I)*DPXY(I)     
            SIGNYZ(I) = SIGNYZ(I) - SHEAR(I)*DPYZ(I)  
            SIGNZX(I) = SIGNZX(I) - SHEAR(I)*DPZX(I)
            P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            ! Computation of the Von Mises equivalent stress of the stress tensor
            SXX(I) = SIGNXX(I) + P(I)
            SYY(I) = SIGNYY(I) + P(I)
            SZZ(I) = SIGNZZ(I) + P(I)
            SXY(I) = SIGNXY(I)
            SYZ(I) = SIGNYZ(I)
            SZX(I) = SIGNZX(I)
            SVM(I)  = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2)
     .           +         (SXY(I)**2 + SZX(I)**2 + SYZ(I)**2)
            SVM(I)  = SQRT(THREE*SVM(I))

            DDEP    = DDLAM * DPLA_DLAM !DDEP IN THE ITERATIONS (DPLA_K+1 = DPLA_K + DDEP) 
            DPLA(I) = MAX(ZERO, DPLA(I) + DDEP )

            ! DPLA AND PLA OF EACH PHASE                                    
            DPLA1(I)=  DPLA1(I) + DDEP !DPLA(I)    
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
            XX1(I,1)=PLA1(I) + DPLA1(I)
            XX2(I,1)=PLA2(I) + DPLA2(I)
            XX3(I,1)=PLA3(I) + DPLA3(I)
            XX4(I,1)=PLA4(I) + DPLA4(I)
            XX5(I,1)=PLA5(I) + DPLA5(I)
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
      !========================================================================================
      ! LOCAL YIELD ALGORITHM
      !========================================================================================
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

          ! initialize incremental update
          STXX(I) = SXX(I)
          STYY(I) = SYY(I)
          STZZ(I) = SZZ(I)
          STXY(I) = SXY(I)
          STYZ(I) = SYZ(I)
          STZX(I) = SZX(I)
          SVMT(I) = SVM(I)

          
          A(I) = (ONE - TOTFRAC(I))* GZ(I) / E(I)
          B(I) = TWO  * (ALFA1 -ALFA2)* (TEMPEL(I)-TEMPO(I) ) *TOTFRAC(I)*LOGZ(I)
          C(I) = TWO*DETH12(I)*ABS((TOTFRAC(I)*(ONE-LOGZ(I))- TOTFRACOLD(I)*(ONE-LOGZM1(I))))

          HFCT(I) = ONE + MAX(ZERO,SEVEN_HALF*(SVMO(I)/YLD(I)-HALF) )

          DPLA(I) = A(I) * ( SVM(I) - SVMO(I) ) + B(I) + C(I) *HFCT(I)  
          GSIG(I) = ONE/( ONE + THREE * (SHEAR(I) / Y1(I))   * DPLA(I) )  

          NORMXX  = THREE_HALF * SOXX(I) /Y1(I) ! DF/DSIG
          NORMYY  = THREE_HALF * SOYY(I) /Y1(I) 
          NORMZZ  = THREE_HALF * SOZZ(I) /Y1(I)  
          NORMXY  = TWO * THREE_HALF * SIGOXY(I) /Y1(I) 
          NORMYZ  = TWO * THREE_HALF * SIGOYZ(I) /Y1(I) 
          NORMZX  = TWO * THREE_HALF * SIGOZX(I) /Y1(I)
          DPXX(I) = DPLA(I) * NORMXX  
          DPYY(I) = DPLA(I) * NORMYY  
          DPZZ(I) = DPLA(I) * NORMZZ  
          DPXY(I) = DPLA(I) * NORMXY  
          DPYZ(I) = DPLA(I) * NORMYZ          
          DPZX(I) = DPLA(I) * NORMZX  
          ! Elasto-plastic stresses update   
          SIGNXX(I) = SIGNXX(I) - (A1(I)*DPXX(I) + A2(I)*(DPYY(I) + DPZZ(I))) 
          SIGNYY(I) = SIGNYY(I) - (A1(I)*DPYY(I) + A2(I)*(DPXX(I) + DPZZ(I)))
          SIGNZZ(I) = SIGNZZ(I) - (A1(I)*DPZZ(I) + A2(I)*(DPXX(I) + DPYY(I)))
          SIGNXY(I) = SIGNXY(I) - SHEAR(I)*DPXY(I)     
          SIGNYZ(I) = SIGNYZ(I) - SHEAR(I)*DPYZ(I)  
          SIGNZX(I) = SIGNZX(I) - SHEAR(I)*DPZX(I)
          P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          ! Computation of the Von Mises equivalent stress of the stress tensor
          SXX(I) = SIGNXX(I) + P(I)
          SYY(I) = SIGNYY(I) + P(I)
          SZZ(I) = SIGNZZ(I) + P(I)
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          SVM(I)  = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2)
     .           +         (SXY(I)**2 + SZX(I)**2 + SYZ(I)**2)
          SVM(I)  = SQRT(THREE*SVM(I))

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

          !new yield updated in newton iterations                    
          YLD(I)=MAX(EM20,  Y1(I)*FRAC1(I)+                          
     .                      Y2(I)*FRAC2(I)+                          
     .                      Y3(I)*FRAC3(I)+                          
     .                      Y4(I)*FRAC4(I)+                          
     .                      Y5(I)*FRAC5(I))                          
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
         UVAR(I,17)= PLA1(I) + MAX(DPLA1(I), ZERO) 
         UVAR(I,18)= PLA2(I) + MAX(DPLA2(I), ZERO)
         UVAR(I,19)= PLA3(I) + MAX(DPLA3(I), ZERO)
         UVAR(I,20)= PLA4(I) + MAX(DPLA4(I), ZERO)
         UVAR(I,21)= PLA5(I) + MAX(DPLA5(I), ZERO)
                

         UVAR(I,35)= VOL(I)
            
         IF(UVAR(I,15)<SVM(I))UVAR(I,15)=SVM(I)
         IF(UVAR(I,34)>TEMPEL(I))UVAR(I,34)=TEMPEL(I)                

         UVAR(I,23)= FRAC2(I)/XEQ2       !X2(I)
         UVAR(I,24)= FRAC3(I)/(ONE-XEQ2)  !X3(I)
         UVAR(I,25)= FRAC4(I)/(ONE-XEQ2)!X4(I)        
         UVAR(I,44)= FRAC5(I)/MAX(EM20,XGAMA(I))

         UVAR(I,2)= FRAC1(I)
         UVAR(I,3)= FRAC2(I)
         UVAR(I,4)= FRAC3(I)
         UVAR(I,5)= FRAC4(I)
         UVAR(I,6)= FRAC5(I)         
         UVAR(I,10)=XGAMA(I)
         HARD(I)= ZERO

        ENDIF ! off
        SOUNDSP(I)  = SQRT((RBULK(I)+FOUR_OVER_3*SHEAR(I))/RHO0(I))
        UVAR(I,9)=YLD(I)        
      ENDDO
      IF (ALFA1/= ZERO .OR. ALFA2 /= ZERO) THEN !compute internal thermal energy
        DO I=1,NEL        
         SIGKK(I)  = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)+SIGOXX(I)+SIGOYY(I)+SIGOZZ(I)
         EINTTH(I) = EINTTH(I)-HALF*SIGKK(I)*DEPSTH(I)
        ENDDO
      ENDIF    
      !IF(HEATFLAG == 0) THEN
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
          DIE(I) =DIE(I)+ (LAT2*    (FRAC5(I)-UVAR(I,6)) +
     .                    LAT1* (FRAC2(I)-UVAR(I,3)
     .             +            FRAC3(I)-UVAR(I,4)
     .             +            FRAC4(I)-UVAR(I,5) ) ) *VOL (I) 
   
         ENDIF !(OFF(I) == ONE ) 
      ENDDO
      
      RETURN
      END
C
Chd|====================================================================
Chd|  KIRKALDYKINETICS              source/materials/mat/mat080/sigeps80.F
Chd|-- called by -----------
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KIRKALDYKINETICS(NEL0,TIME,TEMPEL,TEMPMIN,AE1,AE3,BS,MS,FCFER,
     .  FCPER,FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .  QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRAC,TIMESTEP,NICOOL,INDEX)
     
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL0,NICOOL,INDEX(NICOOL)
        my_real
     .        TEMPEL(NEL0),TEMPMIN(NEL0),TIMESTEP
        my_real
     .        TIME,ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,
     .        FGRAIN,QR2,QR3,QR4,KPER,KBAIN,XEQ2,XEQ4,YX
C-----------------------------------------------
        my_real
     .        FRAC1(NEL0),FRAC2(NEL0),FRAC3(NEL0),FRAC4(NEL0),FRAC5(NEL0)
        my_real
     .        X2(NEL0),X3(NEL0),X4(NEL0),X5(NEL0),TOTFRAC(NEL0),XGAMA(NEL0)
        my_real
     .        FTEMP,UX,VX,UDOT,VDOT,F,CONST,FDOT,X2OLD(NEL0),
     .        X3OLD(NEL0),X4OLD(NEL0),GX,GDOT,DTI
        INTEGER I,K,J
    
      ! SUM OF FRAC1+FRAC2+FRAC3+FRAC4+FRAC5=1--------
      DTI= ONE/MAX(TIMESTEP*THEACCFACT,EM10)
      DO J=1,NICOOL ! NEL0

       I=INDEX(J)
       ! CHECK TEMPERATURE FOR CORRESPONDING PHASE CHANGE
       IF(TOTFRAC(I)<ONE.AND.TEMPEL(I)<TEMPMIN(I))THEN
       IF (TEMPEL(I)<AE3)THEN 
        IF(TEMPEL(I)>AE1)THEN
          IF(X2(I)<0.999)THEN
           ! FERRITE FORMATION X2
           !X2OLD(I)= FRAC2(I)/XEQ2
           X2OLD(I)=X2(I) 
           IF(X2(I)==ZERO)X2(I)=EM10
           FTEMP=EXP(-QR2/TEMPEL(I))*ABS(TEMPEL(I)-AE3)**3
           CONST=FTEMP*FGRAIN*FCFER
           DO K=1,3
             YX= (ONE-X2(I))/MAX(EM10,X2(I))
             UX=X2(I)**(TWO_THIRD*(ONE-X2(I)))
             VX=(ONE-X2(I))**(TWO_THIRD*X2(I))
             F =(X2(I)-X2OLD(I))*DTI-CONST*UX*VX
             FDOT=DTI-CONST*TWO_THIRD*UX*VX*(YX-ONE/YX+LOG(YX))
             X2(I)=MAX(EM20,X2(I)-F/FDOT)
           ENDDO
           FRAC2(I)=X2(I)*XEQ2
           FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
c---------------------------------          
        ELSEIF(TEMPEL(I)>BS)THEN           
         IF(X3(I)<0.999)THEN
            ! PEARLITE X3                                       
            X3OLD(I)= X3(I)  !FRAC3(I)/(ONE-XEQ2) !!
            IF(X3(I)==ZERO)X3(I)=EM10
            FTEMP=6.17*EXP(-QR3/TEMPEL(I))*ABS(TEMPEL(I)-AE1)**3
            CONST=FTEMP*FGRAIN*FCPER !!! adjusted
            DO K=1,3
              YX= (ONE-X3(I))/MAX(EM10,X3(I))
              UX=X3(I)**(TWO_THIRD*(ONE-X3(I)))
              VX=(ONE-X3(I))**(TWO_THIRD*X3(I))
              F =(X3(I)-X3OLD(I))*DTI-CONST*UX*VX
              FDOT=DTI-CONST*TWO_THIRD*UX*VX*(YX-ONE/YX+LOG(YX))
              X3(I)=MAX(EM20,X3(I)-F/FDOT)
            ENDDO       
            FRAC3(I)=X3(I)*(ONE-XEQ2)
            FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
c---------------------------------                    
        ELSEIF(TEMPEL(I)>MS)THEN
          IF(X4(I)<0.999)THEN
           ! BAINITE  FORMATION 
            X4OLD(I) =  X4(I) !FRAC4(I)/MAX(EM20,ONE- XEQ4 ) ! X4(I)
            IF(X4(I) == ZERO) X4(I) = X3(I)  !FRAC3(I)/(ONE-XEQ2) !X3(I)!EM10!X3(I)
            ! INITIALISE AU TAUX DE PERLITE
            FTEMP=EXP(-QR4/TEMPEL(I)) *(TEMPEL(I)-BS)**2
            CONST=FTEMP*FGRAIN*FCBAI
            DO K=1,4
              YX= (ONE-X4(I))/MAX(EM10,X4(I))
              UX=X4(I)**(TWO_THIRD*(ONE-X4(I)))
              VX=(ONE-X4(I))**(TWO_THIRD*X4(I)) 
              GX=EXP(KBAIN*X4(I)*X4(I))
              IF (GX<ZERO)GX=ONE
              F =(X4(I)-X4OLD(I))*DTI-CONST*UX*VX/GX
              UDOT=TWO_THIRD*UX*((ONE-X4(I))/MAX(EM10,X4(I))-
     .           LOG(MAX(EM10,X4(I))))
              VDOT=TWO_THIRD*VX*(LOG(ONE-X4(I))-X4(I)/(ONE-X4(I)))
              GDOT=TWO*KBAIN*X4(I)*GX
              FDOT=DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X4(I)=MAX(EM20,X4(I)-F/FDOT)
           ENDDO
           FRAC4(I)=X4(I)*(ONE-XEQ2)
           FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
            
        ELSE           
          ! MARTENSITE  FORMATION X5
          IF (FRAC5(I)==ZERO)XGAMA(I)= FRAC1(I) 
          FRAC5(I)=XGAMA(I)*(ONE-EXP(-ALPHA*(MS-TEMPEL(I))))
          FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
        ENDIF
       ENDIF   
       ENDIF   
      ENDDO    
     
      RETURN
      END
Chd|====================================================================
Chd|  PHASEKINETIC2                 source/materials/mat/mat080/sigeps80.F
Chd|-- called by -----------
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PHASEKINETIC2(NEL0,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,
     .  FCPER,FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .  QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRAC,TIMESTEP,NICOOL,INDEX)
     
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL0,NICOOL,INDEX(NICOOL)
        my_real
     .        TEMPEL(NEL0),TEMPO(NEL0),TEMPMIN(NEL0),TIMESTEP
        my_real
     .        TIME,ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,
     .        FGRAIN,QR2,QR3,QR4,KPER,KBAIN,XEQ2,XEQ4,YX,
     .        GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .        GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI
C-----------------------------------------------
        my_real
     .        FRAC1(NEL0),FRAC2(NEL0),FRAC3(NEL0),FRAC4(NEL0),FRAC5(NEL0)
        my_real
     .        X2(NEL0),X3(NEL0),X4(NEL0),X5(NEL0),TOTFRAC(NEL0),XGAMA(NEL0)
        my_real
     .        FTEMP,UX,VX,UDOT,VDOT,F,CONST,FDOT,X2OLD(NEL0),
     .        X3OLD(NEL0),X4OLD(NEL0),X5OLD(NEL0),GX,GDOT,DTI,TERM1,TERM2 ,TERM3
        INTEGER I,K,J 
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------
      ! SUM OF FRAC1+FRAC2+FRAC3+FRAC4+FRAC5=1--------
       DTI= ONE/MAX(TIMESTEP*THEACCFACT,EM10)
      DO J=1,NICOOL ! NEL0

       I=INDEX(J)
       ! CHECK TEMPERATURE FOR CORRESPONDING PHASE CHANGE
       IF(TOTFRAC(I)<ONE.AND.TEMPEL(I)<TEMPMIN(I))THEN
       IF (TEMPEL(I)<AE3)THEN 
        IF(TEMPEL(I)>AE1)THEN
          IF(X2(I)<0.999)THEN
           ! FERRITE FORMATION X2
           !X2OLD(I)= FRAC2(I)/XEQ2
           X2OLD(I)=X2(I) 
           IF(X2(I)==ZERO)X2(I)=EM10
           FTEMP = EXP(-QR2/TEMPEL(I))*ABS(TEMPEL(I)-AE3)**3
           CONST = FTEMP * FGFER * CF
           DO K=1,4
              UX = X2(I)     **(PHI_F*(ONE-X2(I)))
              VX = (ONE-X2(I))**(PSI_F*X2(I)) 
              GX = EXP(CR_F *X2(I)*X2(I))
              IF (GX<ZERO) GX=ONE
              F =(X2(I)-X2OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_F* UX * ((ONE-X2(I))/MAX(EM10,X2(I))-LOG(MAX(EM10,X2(I))))                        
              VDOT = PSI_F* VX * (LOG(ONE-X2(I))-X2(I)/(ONE-X2(I)))
              GDOT = TWO * CR_F * X2(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X2(I)= MAX(EM20,X2(I)-F/FDOT)
           ENDDO
           FRAC2(I)=X2(I)*XEQ2
           FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
c---------------------------------          
        ELSEIF(TEMPEL(I)>BS)THEN 
          
         IF(X3(I)<0.999)THEN
            ! PEARLITE X3                                       
            X3OLD(I)= X3(I)  !FRAC3(I)/(ONE-XEQ2) !!
            IF(X3(I)==ZERO)X3(I)=EM10
            FTEMP=6.17*EXP(-QR3/TEMPEL(I))*ABS(TEMPEL(I)-AE1)**3
            CONST=FTEMP* FGPER *CP !!! adjusted
            DO K=1,4
              UX = X3(I)     **(PHI_P*(ONE-X3(I)))
              VX = (ONE-X3(I))**(PSI_P*X3(I)) 
              GX = EXP(CR_P *X3(I)*X3(I))
              IF (GX<ZERO) GX=ONE
              F =(X3(I)-X3OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_P* UX * ((ONE-X3(I))/MAX(EM10,X3(I))-LOG(MAX(EM10,X3(I))))                        
              VDOT = PSI_P* VX * (LOG(ONE-X3(I))-X3(I)/(ONE-X3(I)))
              GDOT = TWO * CR_P * X3(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X3(I)= MAX(EM20,X3(I)-F/FDOT)
            ENDDO       
c            FRAC3(I)=X3(I)*(ONE-X2(I))
            FRAC3(I)=X3(I)*(ONE-XEQ2)
            FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
            ! CHECK IF FERRITE FORMATION
          ENDIF
c---------------------------------                    
        ELSEIF(TEMPEL(I)>MS)THEN
          IF(X4(I)<0.999)THEN
           ! BAINITE  FORMATION 
            X4OLD(I) =  X4(I)  
            IF(X4(I) == ZERO) X4(I) = X3(I)  
            ! INITIALISE AU TAUX DE PERLITE
            FTEMP=EXP(-QR4/TEMPEL(I)) *(TEMPEL(I)-BS)**2
            CONST=FTEMP* FGBAI *CB
            DO K=1,4
              UX = X4(I)     **(PHI_B*(ONE-X4(I)))
              VX = (ONE-X4(I))**(PSI_B*X4(I)) 
              GX = EXP(CR_B *X4(I)*X4(I))
              IF (GX<ZERO) GX=ONE
              F =(X4(I)-X4OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_B* UX * ( (ONE-X4(I))/MAX(EM10,X4(I))-LOG(MAX(EM10,X4(I))) )                        
              VDOT = PSI_B* VX * (LOG(ONE-X4(I))-X4(I)/(ONE-X4(I)))
              GDOT = TWO * CR_B * X4(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X4(I)= MAX(EM20,X4(I)-F/FDOT)
           ENDDO
            FRAC4(I)=X4(I)*(ONE-X2(I))
            FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
            
        ELSE           
          ! MARTENSITE  FORMATION X5
          IF (FRAC5(I)==ZERO)XGAMA(I)= FRAC1(I)
          X5OLD(I) =  X5(I)  
          TERM1 = ALPHA*(MS-TEMPEL(I))**N_M
          TERM2 = MAX(EM20,X5(I))     ** PHI_M
          IF(PSI_M >=ZERO) THEN
             TERM3 = (ONE - X5(I)) ** PSI_M
          ELSE
             TERM3 = (ONE - X5(I)) ** (PSI_M*TWO -XGAMA(I) )
          ENDIF
          X5(I) = X5OLD(I) + (TERM1 * TERM2 *TERM3)*(TEMPO(I)-TEMPEL(I))
          FRAC5(I)=X5(I)*XGAMA(I)
          FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
         ENDIF


       ENDIF   
       ENDIF   
      ENDDO    
     
      RETURN
      END

