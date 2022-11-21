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
Chd|  SIGEPS77                      source/materials/mat/mat077/sigeps77.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS77(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIG0XX ,SIG0YY ,SIG0ZZ  ,SIG0XY  ,SIG0YZ  ,SIG0ZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,FSSP   ,VISCMAX,UVAR    ,OFF     ,NGL     ,
     B     PM     ,IPM    , MAT    ,EPSP   ,P_AIR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIG0XX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIG0YY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C air      UVAR(1) =  rho
C          UVAR(2) =  flow_vol
C          UVAR(3) =  E_int
C          UVAR(4) =  flow_dvol
C          UVAR(5) =  SIGXX
C          UVAR(6 ) = SIGYY 
C          UVAR(7 ) = SIGZZ
C          UVAR(8 ) = SIGXY 
C          UVAR(9 ) = SIGYZ     
C          UVAR(10 ) = SIGZX    
C foam variable
C         UVAR(10 + ..) 
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,
     .   NGL(NEL),MAT(NEL),IPLA,IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIG0XX(NEL),SIG0YY(NEL),SIG0ZZ(NEL),
     .   SIG0XY(NEL),SIG0YZ(NEL),SIG0ZX(NEL),
     .   PM(NPROPM,*),EPSP(NEL),FSSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   TF(*),FINTER
      EXTERNAL FINTER
C   EXTERNAL FINTER
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
      INTEGER I,J,J1,J2,I1,I2,IADBUF,NC,NFUNC,ILOAD(MVSIZ),
     .        IFUNC(MVSIZ,100),NLOAD,NUNLOAD,
     .         IE_CST(MVSIZ),ILOAD0,IDAMAGE,NPAR_FOAM,IFUNCR,
     .         IFUNCK
      my_real
     .        R,FAC,YP1,YP2,YN1,YN2,COEFF,RMIN,RMAX,
     .        E,E1,E2,E3,E4,E5,E6,DF,BB,CC,DELTA,ALPHA,X1,X2,SVM2,
     .        SVM,RHO_AIR,E_AIR,V0,DVOL,MU ,GAMA,RHO_AIR0,P0,
     .        E0,G(MVSIZ),NU,AA1(MVSIZ),AA2(MVSIZ),PFOAM,
     .        FAIL(MVSIZ),
     .        EPST(MVSIZ),AA,YLDMIN(MVSIZ),YLDMAX(MVSIZ),
     .        YLD(MVSIZ),RATE(MVSIZ,2),EF(MVSIZ),
     .        YFAC(MVSIZ,2),EPS0(MVSIZ),EPSS(MVSIZ),
     .        EPSSMAX,DF1,DF2,DAV,EMAX,PEXT,P_AIR(MVSIZ),
     .        EPS_MAX,DSIG ,YLDELAS(MVSIZ),P,SVM1(MVSIZ),EXPO,HYS,
     .        DE,PGAZ,VNEW,EL(MVSIZ),DEN,PGAZ0,E_AIR0,ESPE,PAIR0,
     .        VAR,KK,ALPHA0,ET
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C      
      DO I= 1, NEL
        NFUNC  = IPM(10,MAT(I))
        DO J=1,NFUNC
         IFUNC(I,J)=IPM(10+J,MAT(I))
        ENDDO
      ENDDO
C                
        E0      = UPARAM(2)
        AA      = UPARAM(3)
        EPSSMAX = UPARAM(4)
cc        G       = UPARAM(5)
        NU      = UPARAM(6)
        NLOAD   = UPARAM(7)
        NUNLOAD = UPARAM(8)
        IDAMAGE  = UPARAM(9 + 2*NFUNC )
        EXPO = UPARAM(10 + 2*NFUNC )
        HYS  = UPARAM(11 + 2*NFUNC)
        EMAX = UPARAM(12 + 2*NFUNC )  
C air parameter
        RHO_AIR0 = UPARAM(14 + 2*NFUNC)
        P0       = UPARAM(15 + 2*NFUNC) 
        GAMA     = UPARAM(16 + 2*NFUNC)
        ALPHA0    = UPARAM(17 + 2*NFUNC)
        PEXT     = UPARAM(18 + 2*NFUNC)
C initial foam pressure        
        PFOAM    =  UPARAM(19 + 2*NFUNC)
        KK       =  UPARAM(21 + 2*NFUNC)
C    function for porosity & for  darcy K    
cc        IFUNCR =  IFUNC(1, NFUNC )     
cc        IFUNCK =  IFUNC(1, NFUNC -1 )  
C old stress with out gaz pressure
          DO I=1,NEL           
C          
          PAIR0 = UVAR(I,19)
C                  
          ALPHA = UVAR(I,21) 
          SIG0XX(I) = SIG0XX(I)  +  ALPHA * PAIR0   
          SIG0YY(I) = SIG0YY(I)  +  ALPHA * PAIR0          
          SIG0ZZ(I) = SIG0ZZ(I)  +  ALPHA * PAIR0
C                  
          RHO_AIR   = UVAR(I,1) 
          E_AIR0    = UVAR(I,2)
          VNEW      = UVAR(I,3)
          DVOL      = UVAR(I,4)         
          MU = RHO_AIR/RHO_AIR0
          V0 = VNEW*MU
Cccc          PGAZ0 = UVAR(I,20)          
          ESPE = E_AIR0/MAX(EM15,V0)  
          PGAZ0= (GAMA - ONE)*MU*ESPE
          E_AIR = E_AIR0 - HALF*PGAZ0*DVOL          
          BB = ONE + HALF*(GAMA - ONE)*MU*DVOL/MAX(EM15,V0)
          E_AIR = E_AIR/MAX(EM20,BB)
          ESPE = E_AIR/MAX(EM15,V0) 
          PGAZ =(GAMA - ONE)*MU*ESPE 
C          
          P_AIR(I) = PGAZ - PEXT   
          P_AIR(I)= MAX(P_AIR(I),-PEXT) 
                           
cc pour tester ALe          P_AIR = P0*MU**GAMA 
          UVAR(I,5) = -P_AIR(I)
          UVAR(I,6) = -P_AIR(I)
          UVAR(I,7) = -P_AIR(I)
          UVAR(I,8) = ZERO
          UVAR(I,9) = ZERO 
          UVAR(I,10)= ZERO
          UVAR(I,20)= PGAZ
          UVAR(I,2) = E_AIR/MAX(EM15,VNEW)
          EF(I) = P0*GAMA*MU**(GAMA - 1)
          FSSP(I) =  SQRT(EF(I)/RHO_AIR0)
C
C  compute of alpha & v/v0 for foam element
C
           IFUNCR = IFUNC(I,NFUNC )
           VAR = RHO0(I)/RHO(I)           
           IF( IFUNCR > 0 ) 
     .        UVAR(I,21) = ALPHA0*FINTER(IFUNCR,VAR,NPF,TF,DF)
           
           IFUNCK = IFUNC(I,NFUNC - 1)
           IF( IFUNCK > 0 )
     .            UVAR(I,22) = KK*FINTER(IFUNCK,VAR,NPF,TF,DF)
C          
             UVAR(I,23)= VAR
         ENDDO
C-----------------------------------------------
      DO I=1,NEL
C-------------------
C     epst_spherique
C-------------------
        EPST(I) = EPSXX(I)**2+EPSYY(I)**2 + EPSZZ(I)**2 +
     .            HALF*(EPSXY(I)**2+EPSYZ(I)**2+EPSZX(I)**2)
        EPST(I) = SQRT(EPST(I))    
C-------------------
C     estimation elastique
C-------------------
        EPS0(I) = UVAR(I,11)  
      ENDDO   
                              
C-------------------
C CRITERE
C-------------------
      DO I=1,NEL
C YLD_elast
          RATE(I,1)=UPARAM(9)
          YFAC(I,1)=UPARAM(9 + NFUNC )
          IF(EPST(I) >= EPSSMAX) THEN
           YLDELAS(I)=YFAC(I,1)*FINTER(IFUNC(I,1),EPSSMAX,NPF,TF,DF)
           YLDELAS(I)=EMAX*(EPST(I) - EPSSMAX) +  YLDELAS(I)
          ELSE
           YLDELAS(I) = YFAC(I,1)*FINTER(IFUNC(I,1),EPST(I),NPF,TF,DF)
          ENDIF 
C  yld_max
          NC = NLOAD
          J1 = 1
          DO J=2,NC-1         
           IF(ABS(EPSP(I)) >= ABS(UPARAM(8 + J )))THEN
            J1 = J
           ENDIF
          ENDDO    
          RATE(I,1)=UPARAM( 8 + J1)
          YFAC(I,1)=UPARAM( 8 + NFUNC + J1)
          IF(EPST(I) >= EPSSMAX) THEN
            IF(NC > 1)THEN   
            J2 = J1+1      
            RATE(I,2)=UPARAM(8 + J2 )   
            YFAC(I,2)=UPARAM(8 + NFUNC + J2 )
C
            YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX,NPF,TF,DF1)
            YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPSSMAX,NPF,TF,DF2)     
C          
            FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
            YLDMAX(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)                  
            YLDMAX(I) = EMAX*(EPST(I) - EPSSMAX) +  YLDMAX(I)  
C           
           ELSE
            YLDMAX(I) = 
     .        YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX,NPF,TF,DF)    
            YLDMAX(I) = EMAX*(EPST(I) - EPSSMAX) +  YLDMAX(I) 
           ENDIF 
          ELSE
           IF(NC > 1)THEN   
            J2 = J1+1      
            RATE(I,2)=UPARAM( 8 + J2 )   
            YFAC(I,2)=UPARAM( 8 + NFUNC + J2 )
C
            YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF1)
            YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPST(I),NPF,TF,DF2)       
C          
            FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
            YLDMAX(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)      
           ELSE
            YLDMAX(I) = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF)
           ENDIF
          ENDIF  
C  yld_min  
          NC = NUNLOAD
          J1 = 1 + NLOAD
          YLDMIN(I) = ZERO
          IF(NC > 0 ) THEN
           DO J=2,NC-1      
             IF(ABS(EPSP(I)) >= 
     .                     ABS(UPARAM(NLOAD + 8 + J )))THEN 
               J1 = NLOAD + J
             ENDIF
           ENDDO
C         
           RATE(I,1)=UPARAM(8  + J1)
           YFAC(I,1)=UPARAM(8 + NFUNC + J1)
           
           IF(EPST(I) >= EPSSMAX) THEN
             IF(NC > 1)THEN   
               J2 = J1+1      
                RATE(I,2)=UPARAM(8 + J2 )   
                YFAC(I,2)=UPARAM(8 + NFUNC + J2 )
C
                YP1 =YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX,NPF,TF,DF1)
                YP2 =YFAC(I,2)*FINTER(IFUNC(I,J2),EPSSMAX,NPF,TF,DF2)  
C            
                IF(YP2 < YP1 ) THEN          
                  FAC    = (RATE(I,2) - EPSP(I))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP2 + FAC*(YP1-YP2), EM20)
               ELSE
                  FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)
               ENDIF
                YLDMIN(I) =  EMAX*(EPST(I) - EPSSMAX) + YLDMIN(I)
             ELSE
              YLDMIN(I)= 
     .          YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX,NPF,TF,DF) 
                YLDMIN(I) =  EMAX*(EPST(I) - EPSSMAX) + YLDMIN(I)
             ENDIF                       
           ELSE 
             IF(NC > 1)THEN   
              J2 = J1+1      
              RATE(I,2)=UPARAM( 8 + J2 )   
              YFAC(I,2)=UPARAM( 8 + NFUNC + J2 )
C
              YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF1)
              YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPST(I),NPF,TF,DF2)      
C             
               IF(YP2 < YP1 ) THEN          
                  FAC    = (RATE(I,2) - EPSP(I))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP2 + FAC*(YP1-YP2), EM20)
                ELSE
                  FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)
                ENDIF
              ELSE
               YLDMIN(I) = 
     .             YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF)
              ENDIF
             ENDIF  
            ENDIF        
         ENDDO  
C =====================================================
         DO I = 1,NEL
          IE_CST(I)= 0
          DELTA = EPST(I) - UVAR(I,13)
          IF(DELTA >= ZERO)THEN
           YLD(I) = YLDMAX(I)
           ILOAD(I) = 1
          ELSEIF(DELTA < ZERO)THEN
           YLD(I) = YLDMIN(I)
           ILOAD(I) = -1
           IF(IDAMAGE /= 1 )YLD(I) = YLDMAX(I)
          ENDIF 
C  
          E = UVAR(I,12)
          ILOAD0 = INT(UVAR(I,14))
          EPSS(I) = EPST(I) - YLD(I)/ E
          EPSS(I) = MAX(ZERO, EPSS(I))  
          DE = AA*(EPSS(I) - UVAR(I,11))
          IF(ILOAD(I) == 1) THEN
             E = E + MAX(DE, ZERO)
             IF(ILOAD0 == -1) E= UVAR(I,12)
             UVAR(I,11) = MAX(UVAR(I,11), EPSS(I))
          ELSE
             E = E + MIN(DE ,ZERO)
             IF(ILOAD0 == 1) E= UVAR(I,12)
             UVAR(I,11) = MIN(EPSS(I),UVAR(I,11))
          ENDIF
           E = MIN(E, EMAX)
           E = MAX(E, E0)
           UVAR(I,12) = E
C==================================================       
          AA1(I) = E*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU)
          AA2(I) = AA1(I)*NU/(ONE - NU)  
          G(I) =HALF*E/(ONE + NU)
C ---- 
          SIGNXX(I)= AA1(I)*DEPSXX(I) +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
          SIGNYY(I)= AA1(I)*DEPSYY(I) +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
          SIGNZZ(I)= AA1(I)*DEPSZZ(I) +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
          SIGNXY(I)= G(I) *DEPSXY(I)
          SIGNYZ(I)= G(I) *DEPSYZ(I)
          SIGNZX(I)= G(I) *DEPSZX(I)
          DSIG = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          DSIG =SQRT(DSIG)                         
C   estimate stress   
          SIGNXX(I)=SIG0XX(I) +  AA1(I)*DEPSXX(I)
     .                       +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
          SIGNYY(I)=SIG0YY(I) +  AA1(I)*DEPSYY(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
          SIGNZZ(I)=SIG0ZZ(I) +  AA1(I)*DEPSZZ(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
          SIGNXY(I)=SIG0XY(I) + G(I) *DEPSXY(I)
          SIGNYZ(I)=SIG0YZ(I) + G(I) *DEPSYZ(I)
          SIGNZX(I)=SIG0ZX(I) + G(I) *DEPSZX(I)
c          

          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
C  sound speed     
          ET = AA1(I)
          SOUNDSP(I) = SQRT((ET + EF(I))/RHO0(I))
          VISCMAX(I) = ZERO 
C    
         IF(IDAMAGE == 1 ) THEN
          IF(SVM >= YLDMAX(I) )THEN
           YLD(I) = YLDMAX(I)
cc           IF(DELTA < ZERO .AND. DSIG > YLDMIN(I).AND. DSIG > SVM ) 
cc     .              YLD(I) = YLDMIN(I)
           IF(DELTA < ZERO ) YLD(I) = YLDMIN(I)
           ELSEIF(SVM <= YLDMIN(I)) THEN
            YLD(I) = YLDMIN(I)
C           
           ELSE
            YLD(I) = SVM
            IE_CST(I) = 1
           IF(DELTA < ZERO .AND. DSIG > YLDMIN(I) .AND. DSIG > SVM)THEN
            YLD(I) = YLDMIN(I)
            IE_CST(I) = 0
            ENDIF
           ENDIF
          ELSE
             YLD(I) = YLDMAX(I)
               IF(DELTA > ZERO .AND. SVM < YLDMAX(I))YLD(I)=SVM
cc               IF(DELTA < ZERO .AND. SVM > YLDMAX(I)
cc     .                         .AND. DSIG < SVM )YLD(I) = SVM 
              UVAR(I,17) = UVAR(I,17) + 
     .                     HALF*(YLD(I ) + UVAR(I,15))*DELTA  
              UVAR(I,17) = MAX(ZERO, UVAR(I,17))
              UVAR(I,18) = MAX(UVAR(I,18), UVAR(I,17)) 
          ENDIF
C            
         ENDDO 
C-------------------
C projection spherique
C-------------------
      IF(IDAMAGE == 1 ) THEN
       DO I=1,NEL        
        SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
        SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
        SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
        SIGNXY(I)= G(I) *EPSXY(I)
        SIGNYZ(I)= G(I) *EPSYZ(I)
        SIGNZX(I)= G(I) *EPSZX(I)          
C 
        SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
        SVM =SQRT(SVM2)
        R  = YLD(I)/MAX(EM20,SVM)  
        SIGNXX(I)=SIGNXX(I)*R                                
        SIGNYY(I)=SIGNYY(I)*R                                
        SIGNZZ(I)=SIGNZZ(I)*R                                
        SIGNXY(I)=SIGNXY(I)*R                                
        SIGNYZ(I)=SIGNYZ(I)*R                                
        SIGNZX(I)=SIGNZX(I)*R   
C              
        IF(IE_CST(I) == 1) THEN
         ILOAD0 = INT(UVAR(I,14))
         IF(ILOAD0 /= ILOAD(I))THEN
           ILOAD(I)  = ILOAD0
           UVAR(I,11) = EPS0(I)
         ENDIF
        ENDIF   
        UVAR(I,13) = EPST(I)
        UVAR(I,14) = ILOAD(I)
        UVAR(I,15) = YLD(I)
        UVAR(I,16) = EPSP(I)
       ENDDO
       ELSEIF(IDAMAGE == 2 ) THEN
        DO I=1,NEL
C        
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I)        
C
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = YLD(I)/MAX(EM20,SVM)
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R   
C         
         IF(ILOAD(I) == -1) THEN
            R = YLDMIN(I)/MAX(EM20,YLDELAS(I))
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
          ENDIF        
C        
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .     + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
         SVM =SQRT(SVM2)
         UVAR(I,13) = EPST(I)
         UVAR(I,14) = ILOAD(I)
         UVAR(I,15) = YLD(I)
         UVAR(I,16) = EPSP(I)
        ENDDO       
      ELSEIF(IDAMAGE == 3) THEN
       DO I=1,NEL
C        
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I) 
C             
C     
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = (YLD(I)/MAX(EM20,SVM)) 
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R 
C         
         IF(ILOAD(I) == -1) THEN
            R = ONE - (UVAR(I,17)/MAX(EM20,UVAR(I,18)))**EXPO
            R = ONE - (ONE - HYS)*R
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
          ENDIF        
C                
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .     + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
         SVM =SQRT(SVM2)            
         UVAR(I,13) = EPST(I)
         UVAR(I,14) = ILOAD(I)
         UVAR(I,15) = YLD(I)
         UVAR(I,16) = EPSP(I)
        ENDDO                     
      ENDIF
C------------------------------------
      IF(TIME == 0 .AND. PFOAM /= ZERO)THEN
          DO I=1,NEL       
            SIGNXX(I)= -PFOAM
            SIGNYY(I)= -PFOAM
             SIGNZZ(I)= -PFOAM
          ENDDO  
      ELSE
      
          IF(PFOAM /= ZERO) THEN
            DO I=1,NEL
              IF( EPST(I) == ZERO ) THEN
                SIGNXX(I)= -PFOAM
                SIGNYY(I)= -PFOAM
                SIGNZZ(I)= -PFOAM 
              ENDIF
            ENDDO
         ENDIF 
      ENDIF
C      
      DO I=1,NEL
        ALPHA = UVAR(I,21)
        SIGNXX(I) = SIGNXX(I) -  ALPHA * P_AIR(I)
        SIGNYY(I) = SIGNYY(I) -  ALPHA * P_AIR(I)
        SIGNZZ(I) = SIGNZZ(I) -  ALPHA * P_AIR(I)
        UVAR(I,19) = P_AIR(I)
      ENDDO
c-----------
      RETURN
      END
C
