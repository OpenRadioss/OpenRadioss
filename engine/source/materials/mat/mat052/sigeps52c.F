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
Chd|  SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS52C(
     1     NEL0    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0    ,IPT     ,IFLAG   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,YLD    ,EPSP    ,TABLE)
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
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL0 
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
C#include      "parit_c.inc"
C#include      "com04_c.inc"
C#include      "scr05_c.inc"
Csmtmp +1
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),IPM(NPROPMI,*),NSG
      my_real TIME,TIMESTEP(NEL0),UPARAM(*),
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
     .   GS(*) 
      TYPE(TTABLE) TABLE(*)
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
      my_real UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0),
     .        EPSP(NEL0)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER  NPF(*), MFUNC, KFUNC(MFUNC),ITER, IFLA
      my_real FINTER ,TF(*)
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
      INTEGER I,J,NRATE(MVSIZ),J1,J2,NINDX,NMAX,IADBUF,NFUNC,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),NRATEM,
     .        NRATE1,IFUNC(MFUNC),IADBUFV,
     .        NFUNCM,ISRATE,IYEILD_TAB,
     .        ITAB(100),NTABLE,NXH,NDIM,IPOS,NXK,MX
      my_real 
     .        E,A1,A2,G,G3,
     .        NU
      my_real 
     .        F(MVSIZ), SIGM(MVSIZ), EPSM(MVSIZ), EPSP1(MVSIZ),
     .        LAMDA(MVSIZ), FSTAR(MVSIZ),P0(MVSIZ),PN(MVSIZ),
     .        ET,YEILD0,CSD, 
     .        VISP,  Q1, Q2, Q3, 
     .        SN, EPSN, FI,FN,
     .        FC,FF,FU,
     .        SIGXX(MVSIZ),SIGYY(MVSIZ),SIGXY(MVSIZ),
     .        SIGZX(MVSIZ),SIGYZ(MVSIZ),EN
      my_real
     .        P,VM,R,FAC,
     .        DP11,DP22,DP33,DP12,DP13,DP23,A22,A11,
     .        D11,D22,D33,D12,D13,D23,DCRF,DCRM, DSEPP,DCD,LAM1,
     .        C2,DF,COH,SIH,VA,CRIT,CONV,FG,FN1,YLDC,
     .        C11,P1, VAR,C1,NN1,DEZZ,
     .        DTINV,SQR22, VA1, VA2, VM1, SIGM1, SIGM2, A21,YP1,YP2,
     .        DF1,DF2, XFAC,YFAC,DX2,XX(2),DFT,YY
C
c
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
    
      NTABLE  = IPM(226,MAT(1))
      IYEILD_TAB = 0
      IF(NTABLE > 0) THEN
         IYEILD_TAB = 1
         ITAB(1)=IPM(227,MAT(1))
         IADBUF = IPM(7,MAT(1))-1
         XFAC  = UPARAM(IADBUF + 22 )
         YFAC  = UPARAM(IADBUF + 23 )
      ENDIF
      MX = MAT(1)
      IADBUFV = IPM(7,MX)-1
      E       = UPARAM(IADBUFV+1)
      NU     = UPARAM(IADBUFV+2)
      YEILD0  = UPARAM(IADBUFV+3)
      ET      = UPARAM(IADBUFV+4)
      EN      = UPARAM(IADBUFV+5)
      CSD     = ONE/UPARAM(IADBUFV+6)
      VISP    = UPARAM(IADBUFV+7)
      Q1      = UPARAM(IADBUFV+8)
      Q2      = UPARAM(IADBUFV+9)
      Q3      = UPARAM(IADBUFV+10)
      SN      = UPARAM(IADBUFV+11)
      IF(SN==ZERO) SN = EP20 
      EPSN    = UPARAM(IADBUFV+12)
      FI      = UPARAM(IADBUFV+13)
      FN      = UPARAM(IADBUFV+14)
      FC      = UPARAM(IADBUFV+15)
      FF      = UPARAM(IADBUFV+16)
      FU      = UPARAM(IADBUFV+17)
      IFLA       = NINT(UPARAM(IADBUFV+18))
      ISRATE = NINT(UPARAM(IADBUFV+19))
C
      A1    = E/(ONE -NU**2)
      A2    = NU*A1 
      G     = HALF*E/(ONE + NU) 
      G3    = THREE*G
      C1  = E*(ONE -NU) /((ONE + NU)*(ONE - TWO*NU))
      C2  = C1*NU/(ONE - NU)
      NN1 = NU/(1. - NU)             
C      
       IF(TIME==ZERO)THEN
            DO I=1,NEL0
               EPSP1(I) = ZERO
               EPSM(I)  = ZERO
               IF(EN<ONE)EPSM(I)=EM20
               UVAR(I,1) = EPSM(I)
               UVAR(I,2) = FI
               UVAR(I,3) = YEILD0
               UVAR(I,4) = FI
               UVAR(I,5) = ZERO
               YLD(I) = YEILD0
               PLA (I) = EPSM(I)  
            ENDDO
             IF(IYEILD_TAB > 0) THEN
               DO I=1,NEL0
                 XX(1)=ZERO
                 XX(2)=ZERO 
                 CALL TABLE_INTERP (TABLE(ITAB(1)),XX,YY) 
                 YLD(I)  = YY *YFAC
                 UVAR(I,3) = YLD(I)
               ENDDO
             ENDIF
       ENDIF  
       SQR22 = ONE/SQRT(TWO*PI)
C-----------------------------------------------
C
       DO I=1,NEL0
C
         EPSM(I) = UVAR( I , 1 )
         FSTAR(I)= UVAR( I , 2 )
         SIGM(I) = UVAR( I , 3 )
         F(I)    = UVAR( I , 4 )
         EPSP1(I)= ZERO
         PLA(I) = EPSM(I)
C 
         SIGNXX(I) = SIGOXX(I)  + A1 * DEPSXX(I) + A2 * DEPSYY(I)
         SIGNYY(I) = SIGOYY(I)  + A2 * DEPSXX(I) + A1 * DEPSYY(I)
         SIGNXY(I) = SIGOXY(I)  + G * DEPSXY(I)
         SIGNYZ(I) = SIGOYZ(I)  + GS(I) *DEPSYZ(I)
         SIGNZX(I) = SIGOZX(I)  + GS(I) *DEPSZX(I)
C
         SOUNDSP(I) = SQRT(A1/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE ( I )= ONE    
C-------------------
C     STRAIN RATE
C-------------------
         IF(ISRATE==0)EPSP(I) = 
     .     0.5*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
          DEZZ = -NN1*(DEPSXX(I) + DEPSYY(I))
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I) 
      ENDDO
C-------------------
C     CRITERE
C-----
C
C VISCOPLASTIC
      IF(IFLA==0)THEN
C-----------
C IFLAG=0,  
C-------------
       DO I=1,NEL0
C       
        IF(OFF(I)==ONE)THEN
C
           IF(F(I)<=FC)THEN                                        
                    FSTAR(I) = F(I)                                     
                    DF       = ONE                                       
           ELSE                                                         
                   DF       = (FU-FC)/(FF-FC)               
                   FSTAR(I) = FC + DF*(F(I)-FC)                   
           ENDIF                                                        
           VM = (SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)       
     .         + THREE * (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2))   
           VM = SQRT(VM)                                                 
           PN(I) = (SIGNXX(I)+SIGNYY(I)) * THIRD                         
           SIGM1 = ONE/MAX(EM20, SIGM(I))                                 
           VAR = THREE_HALF * Q2* PN(I) * SIGM1                           
           VAR = EXP(VAR)                                                
           COH = HALF * (VAR  + ONE/MAX(EM20,VAR))                      
           SIH = HALF * (VAR  - ONE/MAX(EM20,VAR))                      
C   
           VA   = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH       
           VA = SQRT(MAX(ZERO,VA))                                       
           YLDC = SIGM(I) * VA                                           
           YLD(I )  = YLDC                                               
            CRIT = VM - YLDC                                            
C
           IF(VM < YLDC .OR. YLDC == ZERO )THEN
              LAMDA(I)=ZERO
           ELSE
C.. stockage des contraintes ....
             LAMDA(I) = ZERO
             SIGXX(I) = SIGNXX(I)
             SIGYY(I) = SIGNYY(I)
             SIGXY(I) = SIGNXY(I)
             SIGZX(I) = SIGNZX(I)
             SIGYZ(I) = SIGNYZ(I)
             DP11 = ZERO
             DP22 = ZERO
             DP33 = ZERO
             DP12 = ZERO
             DP13 = ZERO
             DP23 = ZERO
             DTINV   =TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
             IF(F(I)==ONE)THEN
                  A21 = EP20
             ELSE             
                   A21 =  SIGM1 /(ONE - F(I))     
             ENDIF
             A11 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
             A11 = A11*SQR22
            IF(EN/=1)THEN
              DO ITER=1,5                    
                VM1 = ONE/MAX(EM20, VM)
                VA1 = ONE/MAX(EM20, VA)
                VA2 = HALF * Q1*Q2*FSTAR(I)*SIH*VA1
                D11 = HALF * (TWO*SIGNXX(I)-SIGNYY(I))*VM1 + VA2 
                D22 = HALF * (TWO*SIGNYY(I)-SIGNXX(I))*VM1 + VA2    
                D33 = HALF * (-SIGNXX(I)-SIGNYY(I))*VM1 + VA2             
                D12 = THREE * SIGNXY(I)*VM1       
                D13 = THREE * SIGNZX(I)*VM1
                D23 = THREE * SIGNYZ(I)*VM1
                A22 = D11 * SIGNXX(I) + D22 * SIGNYY(I) +
     .                 TWO*(D12 * SIGNXY(I) + D13 * SIGNZX(I) +              
     .                 D23 * SIGNYZ(I)) 
C
                A22 = A22 * A21           
                DCRF = - SIGM(I)*(Q3*FSTAR(I)*DF - Q1*COH*DF)*VA1
                DCRM = - VA - THREE*VA2*PN(I)*SIGM1         
                DSEPP = ET * EN * EPSM(I) ** (EN - ONE)*
     .           (ONE + ( EPSP(I)*CSD)**VISP)
                DCD = C1*(D11**2 + D22**2 + D33**2) +
     .              TWO*C2*(D11*D22 + D11*D33 + D22*D33) +   
     .              TWO*G  *  D12**2 + 2*GS(I) * ( D13**2 + D23**2 )      
C
                LAM1= DCD - DCRM * DSEPP * A22 - 
     .               DCRF *((ONE -F(I)) * (D11 + D22 + D33) + A11*A22)   
                IF(LAM1/=ZERO)LAMDA(I)= MAX(ZERO,( VM - YLDC ))/LAM1
                DP11 = DP11 + LAMDA(I) * D11
                DP22 = DP22 + LAMDA(I) * D22
                DP33 = DP33 + LAMDA(I) * D33
                DP12 = DP12 + LAMDA(I) * D12
                DP13 = DP13 + LAMDA(I) * D13
                DP23 = DP23 + LAMDA(I) * D23
C   NEW STRE    SS
                SIGNXX(I)= SIGXX(I) - A1 * DP11 - A2 * DP22
                SIGNYY(I)= SIGYY(I) - A1 * DP22 - A2 * DP11 
                SIGNXY(I)= SIGXY(I) - TWO*G  * DP12
                SIGNZX(I)= SIGZX(I) - TWO*GS(I) * DP13
                SIGNYZ(I)= SIGYZ(I) - TWO*GS(I) * DP23
C....... EFF    ECTIVE PLASTIC DEFORMATION.
                EPSP1(I)= SIGNXX(I) * DP11 + SIGNYY(I) * DP22 +
     .                    TWO*(SIGNXY(I) * DP12 + SIGNZX(I) * DP13 +
     .                    SIGNYZ(I) * DP23 )       
                EPSP1(I)= EPSP1(I)* A21 
                EPSP1(I) = MAX(ZERO, EPSP1(I))      
                VM      = SIGNXX(I)**2 + SIGNYY(I)**2  
     .               + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 +SIGNYZ(I)**2)
     .               - SIGNXX(I)*SIGNYY(I)
                VM = SQRT(VM)
C           
                PN(I) = (SIGNXX(I)+SIGNYY(I))*THIRD 
                VAR = THREE_HALF * Q2 * PN(I) * SIGM1
                VAR = EXP(VAR)
                COH = HALF * ( VAR + ONE/MAX(EM20,VAR))
                SIH = HALF * ( VAR - ONE/MAX(EM20,VAR))  
                VA  = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH         
                VA= SQRT(MAX(ZERO,VA))
                YLDC = SIGM(I) * VA
              ENDDO ! ite
            ELSE
              DO ITER=1,5        
                VM1 = ONE/MAX(EM20, VM)
                VA1 = ONE/MAX(EM20, VA)
                VA2 = HALF * Q1*Q2*FSTAR(I)*SIH*VA1
                D11 = HALF * (TWO*SIGNXX(I)-SIGNYY(I))*VM1 + VA2              
                D22 = HALF * (TWO*SIGNYY(I)-SIGNXX(I))*VM1 + VA2       
                D33 = HALF*( - SIGNXX(I)-SIGNYY(I))*VM1 + VA2
                D12 = THREE * SIGNXY(I) * VM1       
                D13 = THREE * SIGNZX(I) * VM1
                D23 = THREE * SIGNYZ(I) * VM1             
                A22 = D11 * SIGNXX(I) + D22 * SIGNYY(I) +
     .                TWO*(D12 * SIGNXY(I) + D13 * SIGNZX(I) +
     .                D23 * SIGNYZ(I))
                A22 = A22 * A21
                DCRF = - SIGM(I)*(Q3*FSTAR(I)*DF - Q1*COH*DF)*VA1
                DCRM = - VA - THREE*VA2*PN(I)*SIGM1  
                DSEPP = ET*(ONE+ ( EPSP(I)*CSD)**VISP)
                DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .               TWO*C2*(D11*D22 + D11*D33 + D22*D33) +            
     .               TWO*G*D12**2 + TWO*GS(I)*( D13**2 + D23**2)
C
                LAM1= DCD - DCRM * DSEPP * A22 - 
     .               DCRF *((ONE - F(I)) * (D11 + D22+D33) + A11*A22)        
                IF(LAM1/=ZERO) LAMDA(I)= MAX(ZERO,( VM - YLDC ))/LAM1
                DP11 = DP11 + LAMDA(I) * D11
                DP22 = DP22 + LAMDA(I) * D22
                DP33 = DP33 + LAMDA(I) * D33   
                DP12 = DP12 + LAMDA(I) * D12
                DP13 = DP13 + LAMDA(I) * D13
                DP23 = DP23 + LAMDA(I) * D23
C   NEW ST    RESS
                SIGNXX(I)= SIGXX(I) - A1 * DP11 - A2 * DP22
                SIGNYY(I)= SIGYY(I) - A1 * DP22 - A2 * DP11 
                SIGNXY(I)= SIGXY(I) - TWO*G  * DP12
                SIGNZX(I)= SIGZX(I) - TWO*GS(I) * DP13
                SIGNYZ(I)= SIGYZ(I) - TWO*GS(I) * DP23         
C....... E    FFECTIVE PLASTIC DEFORMATION.
                EPSP1(I)= SIGNXX(I) * DP11 + SIGNYY(I) * DP22 +
     .                  TWO*(SIGNXY(I) * DP12 + SIGNZX(I) * DP13 +            
     .                   SIGNYZ(I) * DP23 )
                EPSP1(I)= EPSP1(I)*A21
                EPSP1(I) = MAX(ZERO, EPSP1(I))   
                VM      = SIGNXX(I)**2 + SIGNYY(I)**2  
     .              + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
     .              - SIGNXX(I)*SIGNYY(I)
                VM = SQRT(VM)
C                 
                PN(I) = (SIGNXX(I)+SIGNYY(I))*THIRD 
                VAR = THREE_HALF * Q2 * PN(I) * SIGM1
                VAR = EXP(VAR)
                COH = HALF * ( VAR + ONE/MAX(EM20,VAR))
                SIH = HALF * ( VAR - ONE/MAX(EM20,VAR))
                VA  = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
                VA = SQRT(MAX(ZERO,VA)) 
                YLDC= SIGM(I) * VA
               ENDDO ! ite
              ENDIF !LAM
              YLD(I ) =  YLDC
              ETSE( I) = DSEPP/(DSEPP+E) 
              DEZZ  =  NN1*(DP11 +DP22) + DP33
              THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)             
              FG  = (ONE - F(I)) * (DP11 + DP22 +DP33)
              FN1 = A11 * EPSP1(I) 
              EPSM(I) = EPSM(I) + EPSP1(I)
              F(I) = F(I) + FG + FN1
              IF(Q1==ZERO.AND.
     .           Q2==ZERO.AND.
     .           Q3==ZERO)F(I)=ZERO
              IF(F(I)<ZERO)F(I)=ZERO
              SIGM(I)= (YEILD0 + ET*EPSM(I)**EN)* (1+
     .                (EPSP(I)*CSD)**VISP)           
              PLA(I) = EPSM(I)     
C....COMPUTE F*.....
             IF(F(I)<=FC)THEN
              FSTAR(I)=F(I)
             ELSE
              IF(FF==FC)THEN 
               FSTAR(I) = EP20
              ELSE
               FSTAR(I) =FC+(FU-FC)*(F(I)-FC)/(FF-FC)
              ENDIF
             ENDIF
             UVAR(I,1)= EPSM(I)
             UVAR(I,2)=FSTAR(I)
             UVAR(I,3)=SIGM(I)
             UVAR(I,4)=F(I)
             UVAR(I,5)=EPSP(I)         
c..... DEVIATORIC STRESS         
         ENDIF   
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF) OFF(I)=FOUR_OVER_5
        ENDIF
       ENDDO  ! NEL0
      ELSEIF(IFLA==1)THEN
C----
CIFLA set to 1
C-----
       DO I=1,NEL0
        IF(OFF(I)==ONE)THEN
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE  
          DF       =(FU - FC)/(FF - FC)   
          FSTAR(I) = FC + DF*(F(I)-FC)      
         ENDIF
C
         VM = (SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
     .    + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2))         
         PN(I) = (SIGNXX(I)+SIGNYY(I))*THIRD        
         SIGM1 = ONE/MAX(EM20,SIGM(I))
         SIGM2 = SIGM1**2
         VAR   = THREE_HALF * Q2 * PN(I) * SIGM1
         VAR = EXP(VAR)
         COH   = HALF *(VAR + ONE/MAX(EM20,VAR))
         SIH   = HALF *(VAR - ONE/MAX(EM20,VAR))
C  
         IF(PN(I)<=ZERO) THEN
          VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
         ELSE
          VA= ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
         ENDIF
C
         LAMDA(I) = ZERO
         YLDC     = SIGM(I)
         CRIT = VM * SIGM2 - VA
         YLD( I ) = SIGM(I) * SQRT(MAX(ZERO,VA))
         IF(CRIT < ZERO .OR. YLD(I) == ZERO )THEN
          LAMDA(I)=ZERO
         ELSE
C.. stockage des contraintes ....
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO
          DTINV   =TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
C
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE
           A21 =  SIGM1 /(ONE - F(I))
          ENDIF   
          A11 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A11 = A11*SQR22 
C
          IF(EN/=ONE) THEN
           DO ITER=1,5
            IF(PN(I)<=ZERO)THEN
             D11 = (TWO*SIGNXX(I)-SIGNYY(I))*SIGM2
             D22 = (TWO*SIGNYY(I)-SIGNXX(I))*SIGM2
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2   
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2
             DCRF = TWO*Q1*DF - TWO*Q3*DF*FSTAR(I)
            ELSE 
             VA2 = Q1 * Q2 * FSTAR(I) * SIH * SIGM1
             D11 = (TWO*SIGNXX(I) - SIGNYY(I))*SIGM2 + VA2
             D22 = (TWO*SIGNYY(I) - SIGNXX(I))*SIGM2 + VA2
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2 + VA2  
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2 - THREE*VA2*PN(I)*SIGM1 
             DCRF = TWO * Q1 * DF * COH - TWO*Q3*DF*FSTAR(I)           
            ENDIF
C
            A22 = D11*SIGNXX(I) + D22*SIGNYY(I) +
     .       THREE*(D12*SIGNXY(I)+ D13*SIGNZX(I)+ D23*SIGNYZ(I))            
            A22 = A22*A21     
            DSEPP = ET * EN * EPSM(I) ** (EN - 1)*
     .       (ONE + ( EPSP(I)*CSD)**VISP)            
            DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .          TWO*C2*(D11*D22 + D11*D33 + D22*D33) +      
     .          TWO*G*D12**2 + TWO*GS(I)*( D13**2 + D23**2)              
C
            LAM1= DCD - DCRM * DSEPP*A22 - 
     .        DCRF*((ONE-F(I))*(D11 + D22 +D33) + A11*A22)  
            IF(LAM1/=ZERO) LAMDA(I) = MAX(ZERO,(VM*SIGM2 - VA))/LAM1
C.... PLASTIC DEFORMATION
            DP11 = DP11 + LAMDA(I) * D11
            DP22 = DP22 + LAMDA(I) * D22
            DP33 = DP33 + LAMDA(I) * D33    
            DP12 = DP12 + LAMDA(I) * D12
            DP13 = DP13 + LAMDA(I) * D13
            DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
            SIGNXX(I) = SIGXX(I) - A1*DP11 - A2*DP22
            SIGNYY(I) = SIGYY(I) - A1*DP22 - A2*DP11   
            SIGNXY(I) = SIGXY(I) - TWO*G*DP12
            SIGNZX(I)= SIGZX(I) - TWO*GS(I)*DP13
            SIGNYZ(I)= SIGYZ(I) - TWO*GS(I)*DP23
C....... EFFECTIVE PLASTIC DEFORMATION.
            EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 +
     .         TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 +  SIGNYZ(I)*DP23)
            EPSP1(I) = EPSP1(I)*A21
            EPSP1(I) = MAX(ZERO, EPSP1(I))  
            VM= SIGNXX(I)**2 + SIGNYY(I)**2 
     .       + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)          
     .       - SIGNXX(I)*SIGNYY(I)
C
            PN(I) = (SIGNXX(I) + SIGNYY(I)) * THIRD
            VAR = THREE_HALF * Q2*PN(I)*SIGM1
            VAR= EXP(VAR)
            COH = HALF * (VAR + ONE/MAX(EM20,VAR))
            SIH = HALF * (VAR - ONE/MAX(EM20,VAR))
C
            IF(PN(I)<=ZERO) THEN
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
            ELSE
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
            ENDIF
C  
           ENDDO ! it
C
          ELSE
           DO ITER=1,5
           IF(PN(I)<=ZERO)THEN
            D11 = (TWO*SIGNXX(I)-SIGNYY(I))*SIGM2
            D22 = (TWO*SIGNYY(I)-SIGNXX(I))*SIGM2
            D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2            
            D12 = SIX*SIGNXY(I)*SIGM2
            D13 = SIX*SIGNZX(I)*SIGM2
            D23 = SIX*SIGNYZ(I)*SIGM2
            DCRM = -TWO*VM*SIGM1*SIGM2
            DCRF = TWO*Q1*DF - TWO*Q3*DF*FSTAR(I)
           ELSE
            VA2 =  Q1 * Q2 * FSTAR(I) * SIH * SIGM1
            D11 = (TWO*SIGNXX(I) - SIGNYY(I))*SIGM2 +  VA2      
            D22 = (TWO*SIGNYY(I) - SIGNXX(I))*SIGM2 +  VA2    
            D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2 +  VA2             
            D12 = SIX*SIGNXY(I)*SIGM2
            D13 = SIX*SIGNZX(I)*SIGM2
            D23 = SIX*SIGNYZ(I)*SIGM2
            DCRM = -TWO*VM*SIGM1*SIGM2 - THREE*VA2*PN(I)*SIGM1
            DCRF = TWO * Q1 * DF * COH - TWO*Q3*DF*FSTAR(I)
           ENDIF
C
           A22 = D11*SIGNXX(I) + D22*SIGNYY(I) +
     .      THREE*(D12*SIGNXY(I)+ D13*SIGNZX(I)+ D23*SIGNYZ(I)) 
           A22 = A22*A21
           DSEPP = ET*(ONE+ ( EPSP(I)*CSD)**VISP)
           DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .            TWO*C2*(D11*D22 + D11*D33 + D22*D33) +
     .         TWO*G*D12**2 + 2*GS(I)*( D13**2 + D23**2)              
C
           LAM1= DCD - DCRM * DSEPP*A22 - 
     .        DCRF*((ONE -F(I))*(D11 + D22 + D33) + A11*A22)          
           IF(LAM1/=ZERO)LAMDA(I) = MAX(ZERO,(VM*SIGM2 - VA))/LAM1 
C.... PLASTIC DEFORMATION
           DP11 = DP11 + LAMDA(I) * D11
           DP22 = DP22 + LAMDA(I) * D22
           DP33 = DP33 + LAMDA(I) * D33  
           DP12 = DP12 + LAMDA(I) * D12
           DP13 = DP13 + LAMDA(I) * D13
           DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
           SIGNXX(I) = SIGXX(I) - A1*DP11 - A2*DP22
           SIGNYY(I) = SIGYY(I) - A1*DP22 - A2*DP11
           SIGNXY(I) = SIGXY(I) - TWO*G*DP12
           SIGNZX(I)= SIGZX(I) - TWO*GS(I)*DP13
           SIGNYZ(I)= SIGYZ(I) - TWO*GS(I)*DP23           
C....... EFFECTIVE PLASTIC DEFORMATION.
           EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 +
     .          TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 +  SIGNYZ(I)*DP23)
           EPSP1(I) = EPSP1(I)*A21
C
           EPSP1(I) =MAX(ZERO, EPSP1(I))  
           VM= SIGNXX(I)**2 + SIGNYY(I)**2 
     .       + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)          
     .       - SIGNXX(I)*SIGNYY(I)
C
           PN(I) = (SIGNXX(I) + SIGNYY(I)) * THIRD
           VAR = THREE_HALF * Q2*PN(I)*SIGM1 
           VAR = EXP(VAR)
           COH = HALF * (VAR + ONE/MAX(EM20,VAR))
           SIH = HALF * (VAR - ONE/MAX(EM20,VAR))
C
           IF(PN(I)<=ZERO) THEN
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
           ELSE
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
           ENDIF
C 
          ENDDO ! it
         ENDIF
         YLD(I ) = SIGM(I)*SQRT(MAX(ZERO,VA))
         ETSE( I) = DSEPP/(DSEPP+E) 
         DEZZ  =  NN1*(DP11 +DP22) + DP33
         THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)                
         FG     = (ONE - F(I))*(DP11 + DP22 + DP33)
         FN1    = A11*EPSP1(I) 
         EPSM(I)= EPSM(I) + EPSP1(I)
         F(I)   = F(I) + FG + FN1
         IF(Q1==ZERO.and.
     .       Q2==ZERO.and.
     .       Q3==ZERO)F(I)=ZERO
         IF(F(I)<ZERO)F(I)=ZERO
         SIGM(I)= (YEILD0 + ET*EPSM(I)**EN)*(1+
     .            (EPSP(I)*CSD)**VISP)           
         PLA(I) = EPSM(I)
C....COMPUTE F*.....
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
         ELSE
          IF(FF==FC)THEN 
           FSTAR(I) = EP20
          ELSE
           FSTAR(I) = FC+ (FU-FC)*(F(I)-FC)/(FF-FC)
          ENDIF
         ENDIF
C
         UVAR(I , 1) = EPSM(I)
         UVAR(I , 2) = FSTAR(I)
         UVAR(I , 3) = SIGM(I)
         UVAR(I , 4) = F(I)
         UVAR(I , 5) = EPSP(I)         
c..... DEVIATORIC STRESS         
         ENDIF
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF) OFF(I)=FOUR_OVER_5
        ENDIF
       ENDDO 
      ELSEIF(IFLA==2)THEN 
C----
CIFLA set to 2
C-----        
       DO I=1,NEL0
        IF(OFF(I)==ONE)THEN
C
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE  
          DF       =(FU - FC)/(FF - FC)   
          FSTAR(I) = FC + DF*(F(I)-FC)      
         ENDIF
C
         VM = (SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
     .    + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2))         
         PN(I) = (SIGNXX(I)+SIGNYY(I))*THIRD
C
         SIGM1 = ONE/MAX(EM20,SIGM(I))
         SIGM2 = SIGM1**2
         VAR   = THREE_HALF * Q2 * PN(I) * SIGM1
         VAR = EXP(VAR)
         COH   = HALF *(VAR + ONE/MAX(EM20,VAR))
         SIH   = HALF *(VAR - ONE/MAX(EM20,VAR))
C  
         IF(PN(I)<=ZERO) THEN
          VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
         ELSE
          VA= ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
         ENDIF
C
         LAMDA(I) = ZERO
         YLDC     = SIGM(I)
         CRIT = VM * SIGM2 - VA
C
         YLD( I ) = SIGM(I) * SQRT(MAX(ZERO,VA))
         IF(CRIT < ZERO .OR. YLD(I) == ZERO)THEN
          LAMDA(I)=ZERO
         ELSE
C.. stockage des contraintes ....
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
C
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO
C  
          DTINV   =TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
C
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE
           A21 =  SIGM1 /(ONE - F(I))
          ENDIF     
          A11 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A11 = A11*SQR22   
          IF(EN/=ONE) THEN
           DO ITER=1,5
            IF(PN(I)<=ZERO)THEN
             D11 = (TWO*SIGNXX(I)-SIGNYY(I))*SIGM2
             D22 = (TWO*SIGNYY(I)-SIGNXX(I))*SIGM2
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2   
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2
             DCRF = TWO*Q1*DF - TWO*Q3*DF*FSTAR(I)
            ELSE 
             VA2 = Q1 * Q2 * FSTAR(I) * SIH * SIGM1
             D11 = (TWO*SIGNXX(I) - SIGNYY(I))*SIGM2 + VA2
             D22 = (TWO*SIGNYY(I) - SIGNXX(I))*SIGM2 + VA2
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2 + VA2  
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2 - THREE*VA2*PN(I)*SIGM1 
             DCRF = TWO * Q1 * DF * COH - TWO*Q3*DF*FSTAR(I)
            ENDIF
C
            A22 = D11*SIGNXX(I) + D22*SIGNYY(I) +
     .       THREE*(D12*SIGNXY(I)+ D13*SIGNZX(I)+ D23*SIGNYZ(I)) 
            A22 = A22*A21     
C    
            DSEPP = ET * EN * EPSM(I) ** (EN - ONE)*
     .       (ONE + ( EPSP(I)*CSD)**VISP)            
C     
            DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .          TWO*C2*(D11*D22 + D11*D33 + D22*D33) +      
     .          TWO*G*D12**2 + TWO*GS(I)*( D13**2 + D23**2)
C
            LAM1= DCD - DCRM * DSEPP*A22 - 
     .       DCRF*((1-F(I))*(D11 + D22 +D33) + A11*A22)   
            IF(LAM1/=ZERO) LAMDA(I) = MAX(ZERO,(VM*SIGM2 - VA))/LAM1
C.... PLASTIC DEFORMATION
            DP11 = DP11 + LAMDA(I) * D11
            DP22 = DP22 + LAMDA(I) * D22
            DP33 = DP33 + LAMDA(I) * D33    
            DP12 = DP12 + LAMDA(I) * D12
            DP13 = DP13 + LAMDA(I) * D13
            DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
            SIGNXX(I) = SIGXX(I) - A1*DP11 - A2*DP22
            SIGNYY(I) = SIGYY(I) - A1*DP22 - A2*DP11   
            SIGNXY(I) = SIGXY(I) - TWO*G*DP12
            SIGNZX(I)= SIGZX(I) - TWO*GS(I)*DP13
            SIGNYZ(I)= SIGYZ(I) - TWO*GS(I)*DP23
C....... EFFECTIVE PLASTIC DEFORMATION.
            EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 +
     .       TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 +  SIGNYZ(I)*DP23)    
            EPSP1(I) = EPSP1(I)*A21
            EPSP1(I) = MAX(ZERO, EPSP1(I))  
C
            VM= SIGNXX(I)**2 + SIGNYY(I)**2 
     .       + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
     .       - SIGNXX(I)*SIGNYY(I)
C
            PN(I) = (SIGNXX(I) + SIGNYY(I))*THIRD
            VAR = THREE_HALF * Q2*PN(I)*SIGM1
            VAR= EXP(VAR)
            COH = HALF * (VAR + ONE/MAX(EM20,VAR))
            SIH = HALF * (VAR - ONE/MAX(EM20,VAR))
C
            IF(PN(I)<=ZERO) THEN
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
            ELSE
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
            ENDIF
           ENDDO ! it
C
          ELSE
           DO ITER=1,5
            IF(PN(I)<=ZERO)THEN
             D11 = (TWO*SIGNXX(I)-SIGNYY(I))*SIGM2
             D22 = (TWO*SIGNYY(I)-SIGNXX(I))*SIGM2
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2            
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2
             DCRF = TWO*Q1*DF - TWO*Q3*DF*FSTAR(I)
            ELSE 
             VA2 =  Q1 * Q2 * FSTAR(I) * SIH * SIGM1
             D11 = (TWO*SIGNXX(I) - SIGNYY(I))*SIGM2 +  VA2      
             D22 = (TWO*SIGNYY(I) - SIGNXX(I))*SIGM2 +  VA2    
             D33 = ( - SIGNXX(I) - SIGNYY(I))*SIGM2 +  VA2             
             D12 = SIX*SIGNXY(I)*SIGM2
             D13 = SIX*SIGNZX(I)*SIGM2
             D23 = SIX*SIGNYZ(I)*SIGM2
             DCRM = -TWO*VM*SIGM1*SIGM2 - THREE*VA2*PN(I)*SIGM1
             DCRF = TWO * Q1 * DF * COH - TWO*Q3*DF*FSTAR(I)
            ENDIF
C
            A22 = D11*SIGNXX(I) + D22*SIGNYY(I) +
     .       THREE*(D12*SIGNXY(I)+ D13*SIGNZX(I)+ D23*SIGNYZ(I)) 
            A22 = A22*A21
C    
            DSEPP = ET*(1+ ( EPSP(I)*CSD)**VISP)
            DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .         TWO*C2*(D11*D22 + D11*D33 + D22*D33) +      
     .         TWO*G*D12**2 + TWO*GS(I)*( D13**2 + D23**2)
C
            LAM1= DCD - DCRM * DSEPP*A22 - 
     .        DCRF*((ONE -F(I))*(D11 + D22 + D33) + A11*A22)
            IF(LAM1/=ZERO)LAMDA(I) = MAX(ZERO,(VM*SIGM2 - VA))/LAM1 
C.... PLASTIC DEFORMATION
            DP11 = DP11 + LAMDA(I) * D11
            DP22 = DP22 + LAMDA(I) * D22
            DP33 = DP33 + LAMDA(I) * D33  
            DP12 = DP12 + LAMDA(I) * D12
            DP13 = DP13 + LAMDA(I) * D13
            DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
            SIGNXX(I) = SIGXX(I) - A1*DP11 - A2*DP22
            SIGNYY(I) = SIGYY(I) - A1*DP22 - A2*DP11
            SIGNXY(I) = SIGXY(I) - TWO*G*DP12
            SIGNZX(I) = SIGZX(I) - TWO*GS(I)*DP13
            SIGNYZ(I) = SIGYZ(I) - TWO*GS(I)*DP23
C....... EFFECTIVE PLASTIC DEFORMATION.
            EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 +
     .       TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 +  SIGNYZ(I)*DP23)     
            EPSP1(I) = EPSP1(I)*A21
C
            EPSP1(I) =MAX(ZERO, EPSP1(I))  
            VM= SIGNXX(I)**2 + SIGNYY(I)**2 
     .       + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
     .       - SIGNXX(I)*SIGNYY(I)
C
            PN(I) = (SIGNXX(I) + SIGNYY(I))*THIRD
            VAR = THREE_HALF * Q2*PN(I)*SIGM1 
            VAR = EXP(VAR)
            COH = HALF * (VAR + ONE/MAX(EM20,VAR))
            SIH = HALF * (VAR - ONE/MAX(EM20,VAR))
C
            IF(PN(I)<=ZERO) THEN
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
            ELSE
             VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
            ENDIF
           ENDDO ! it
          ENDIF
          YLD(I) =  SIGM(I) * SQRT(MAX(ZERO, VA))       
          ETSE( I) = DSEPP/(DSEPP+E)                               
          DEZZ  =  NN1*(DP11 +DP22) + DP33
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)               
C
          FG     = (ONE - F(I))*(DP11 + DP22 + DP33)
          FN1 =ZERO
          PN(I) = (SIGNXX(I) + SIGNYY(I))*THIRD
          IF(PN(I)>=ZERO)FN1 = A11*EPSP1(I) 
          EPSM(I)= EPSM(I) + EPSP1(I)
          F(I)   = F(I) + FG + FN1
          IF(Q1==ZERO.and.
     .       Q2==ZERO.and.
     .       Q3==ZERO)F(I)=ZERO
          IF(F(I)<ZERO)F(I)=ZERO
          SIGM(I)= (YEILD0 + ET*EPSM(I)**EN)*(ONE+
     .            (EPSP(I)*CSD)**VISP)           
          PLA(I) = EPSM(I)
C....COMPUTE F*.....
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
         ELSE
          IF(FF==FC)THEN 
           FSTAR(I) = EP20
          ELSE
           FSTAR(I) = FC+ (FU-FC)*(F(I)-FC)/(FF-FC)
          ENDIF
         ENDIF
C
         UVAR(I , 1) = EPSM(I)
         UVAR(I , 2) = FSTAR(I)
         UVAR(I , 3) = SIGM(I)
         UVAR(I , 4) = F(I)
         UVAR(I , 5) = EPSP(I)     
c..... DEVIATORIC STRESS         
         ENDIF
C   
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF) OFF(I)=FOUR_OVER_5
        ENDIF
C
       ENDDO
      ELSE
C---------
C  iflag = 3
c----------      
       DO I=1,NEL0
        IF(OFF(I)==ONE)THEN
C
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE               
          DF       = (FU-FC)/(FF-FC)
          FSTAR(I) = FC + DF*(F(I)-FC)
         ENDIF
         VM = (SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
     .       + THREE * (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2))
         VM = SQRT(VM)
         PN(I) = (SIGNXX(I)+SIGNYY(I))*THIRD 
C
         SIGM1 = ONE/MAX(EM20, SIGM(I))
         VAR = THREE_HALF * Q2 * PN(I) * SIGM1
         VAR = EXP(VAR)
         COH = HALF * (VAR  + ONE/MAX(EM20,VAR)) 
         SIH = HALF * (VAR  - ONE/MAX(EM20,VAR))            
C   
         VA   = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
         VA = SQRT(MAX(ZERO,VA))
         YLDC = SIGM(I) * VA
C
         YLD(I )  = YLDC  
         CRIT = VM - YLDC
C
         IF(VM < YLDC .OR. YLDC == ZERO)THEN
          LAMDA(I)=ZERO
         ELSE
C.. stockage des contraintes ....
          LAMDA(I) = ZERO
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
C
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO 
          DTINV   =TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20) 
C
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE             
           A21 =  SIGM1 /(ONE - F(I))     
          ENDIF
          A11 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A11 = A11*SQR22 
C    
          IF(EN/=1)THEN
           DO ITER=1,5                      
            VM1 = ONE/MAX(EM20, VM)
            VA1 = ONE/MAX(EM20, VA)
            VA2 = HALF * Q1*Q2*FSTAR(I)*SIH*VA1
            D11 = HALF * (TWO*SIGNXX(I)-SIGNYY(I))*VM1 + VA2 
            D22 = HALF * (TWO*SIGNYY(I)-SIGNXX(I))*VM1 + VA2    
            D33 = HALF*  (-SIGNXX(I)-SIGNYY(I))*VM1 + VA2             
            D12 = THREE * SIGNXY(I)*VM1       
            D13 = THREE * SIGNZX(I)*VM1
            D23 = THREE * SIGNYZ(I)*VM1
            A22 = D11 * SIGNXX(I) + D22 * SIGNYY(I) +
     .            TWO*(D12 * SIGNXY(I) + D13 * SIGNZX(I) + 
     .             D23 * SIGNYZ(I)) 
C
            A22 = A22 * A21           
            DCRF = - SIGM(I)*(Q3*FSTAR(I)*DF - Q1*COH*DF)*VA1
            DCRM = - VA - THREE*VA2*PN(I)*SIGM1         
            DSEPP = ET * EN * EPSM(I) ** (EN - 1)*
     .       (ONE+ ( EPSP(I)*CSD)**VISP)            
              DCD = C1*(D11**2 + D22**2 + D33**2) +
     .            TWO*C2*(D11*D22 + D11*D33 + D22*D33) +   
     .           TWO*G  *  D12**2 + TWO*GS(I) * ( D13**2 + D23**2 )
C
            LAM1= DCD - DCRM * DSEPP * A22 - 
     .           DCRF *((ONE - F(I)) * (D11 + D22 + D33) + A11*A22)
         
            IF(LAM1/=ZERO)LAMDA(I) =  MAX(ZERO,( VM - YLDC )) / LAM1    
C  PLASTIC DEFORMATION 
            DP11 = DP11 + LAMDA(I) * D11
            DP22 = DP22 + LAMDA(I) * D22
            DP33 = DP33 + LAMDA(I) * D33
            DP12 = DP12 + LAMDA(I) * D12
            DP13 = DP13 + LAMDA(I) * D13
            DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
            SIGNXX(I)= SIGXX(I) - A1 * DP11 - A2 * DP22
            SIGNYY(I)= SIGYY(I) - A1 * DP22 - A2 * DP11           
            SIGNXY(I)= SIGXY(I) - TWO*G  * DP12
            SIGNZX(I)= SIGZX(I) - TWO*GS(I) * DP13
            SIGNYZ(I)= SIGYZ(I) - TWO*GS(I) * DP23
C....... EFFECTIVE PLASTIC DEFORMATION.
            EPSP1(I)= SIGNXX(I) * DP11 + SIGNYY(I) * DP22 +
     .               TWO*(SIGNXY(I) * DP12 + SIGNZX(I) * DP13 + 
     .               SIGNYZ(I) * DP23 )    
            EPSP1(I)= EPSP1(I)* A21 
            EPSP1(I) = MAX(ZERO, EPSP1(I))      
            VM      = SIGNXX(I)**2 + SIGNYY(I)**2  
     .              + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
     .              - SIGNXX(I)*SIGNYY(I)
            VM = SQRT(VM)
C            
            PN(I) = (SIGNXX(I)+SIGNYY(I))* THIRD 
            VAR = THREE_HALF * Q2 * PN(I) * SIGM1
            VAR = EXP(VAR)
            COH = HALF * ( VAR + ONE/MAX(EM20,VAR))
            SIH = HALF * ( VAR - ONE/MAX(EM20,VAR))
            VA  = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH           
            VA= SQRT(MAX(ZERO,VA))
            YLDC = SIGM(I) * VA
C           IF(CONV<1E-6) GOTO 100
             
           ENDDO ! ite
          ELSE
           DO ITER=1,5                 
            VM1 = ONE/MAX(EM20, VM)
            VA1 = ONE/MAX(EM20, VA)
            VA2 = HALF * Q1*Q2*FSTAR(I)*SIH*VA1
            D11 = HALF * (TWO*SIGNXX(I)-SIGNYY(I))*VM1 + VA2                 
            D22 = HALF * (TWO*SIGNYY(I)-SIGNXX(I))*VM1 + VA2       
            D33 = HALF*( - SIGNXX(I)-SIGNYY(I))*VM1 + VA2
            D12 = THREE * SIGNXY(I) * VM1       
            D13 = THREE * SIGNZX(I) * VM1
            D23 = THREE * SIGNYZ(I) * VM1             
            A22 = D11 * SIGNXX(I) + D22 * SIGNYY(I) +
     .            TWO *(D12 * SIGNXY(I) + D13 * SIGNZX(I) + 
     .             D23 * SIGNYZ(I)) 
C                            
            A22 = A22 * A21
            DCRF = - SIGM(I)*(Q3*FSTAR(I)*DF - Q1*COH*DF)*VA1
            DCRM = - VA - THREE*VA2*PN(I)*SIGM1  
            DSEPP = ET*(ONE + ( EPSP(I)*CSD)**VISP)
              DCD = C1*(D11**2 + D22**2 + D33**2) + 
     .           TWO*C2*(D11*D22 + D11*D33 + D22*D33) +            
     .           TWO*G  *  D12**2 + TWO *GS(I) * ( D13**2 + D23**2)
C
            LAM1= DCD - DCRM * DSEPP * A22 - 
     .           DCRF *((ONE -F(I)) * (D11 + D22+D33) + A11*A22)
            IF(LAM1/=ZERO) LAMDA(I) = MAX(ZERO,( VM - YLDC )) / LAM1
            
C  PLASTIC DEFORMATION 
            DP11 = DP11 + LAMDA(I) * D11
            DP22 = DP22 + LAMDA(I) * D22
            DP33 = DP33 + LAMDA(I) * D33   
            DP12 = DP12 + LAMDA(I) * D12
            DP13 = DP13 + LAMDA(I) * D13
            DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
            SIGNXX(I)= SIGXX(I) - A1 * DP11 - A2 * DP22
            SIGNYY(I)= SIGYY(I) - A1 * DP22 - A2 * DP11 
            SIGNXY(I)= SIGXY(I) - 2*G  * DP12
            SIGNZX(I)= SIGZX(I) - 2*GS(I) * DP13
            SIGNYZ(I)= SIGYZ(I) - 2*GS(I) * DP23
C....... EFFECTIVE PLASTIC DEFORMATION.
            EPSP1(I)= SIGNXX(I) * DP11 + SIGNYY(I) * DP22 +
     .               TWO*(SIGNXY(I) * DP12 + SIGNZX(I) * DP13 + 
     .               SIGNYZ(I) * DP23 )     
            EPSP1(I)= EPSP1(I)*A21
            EPSP1(I) = MAX(ZERO, EPSP1(I))   
            VM      = SIGNXX(I)**2 + SIGNYY(I)**2  
     .              + THREE*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
     .              - SIGNXX(I)*SIGNYY(I)
            VM = SQRT(VM)
C            
            PN(I) = (SIGNXX(I)+SIGNYY(I))* THIRD 
            VAR = THREE_HALF * Q2 * PN(I) * SIGM1
            VAR = EXP(VAR)
            COH = HALF * ( VAR + ONE/MAX(EM20,VAR))
            SIH = HALF * ( VAR - ONE/MAX(EM20,VAR))
            VA  = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
            VA = SQRT(MAX(ZERO,VA)) 
            YLDC= SIGM(I) * VA
C           IF(CONV<1E-6) GOTO 100   
           ENDDO ! ite
          ENDIF
C
          YLD(I ) =  YLDC
          ETSE( I) = DSEPP/(DSEPP+E)                  
          DEZZ  = NN1*(DP11 +DP22) + DP33
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)                       
C   
          PN(I) = (SIGXX(I) + SIGYY(I))*THIRD
          FG  = (ONE - F(I)) * (DP11 + DP22 +DP33)
          FN1  = ZERO
          IF(PN(I)>=ZERO)FN1 = A11*EPSP1(I) 
          EPSM(I) = EPSM(I) + EPSP1(I)
          F(I) = F(I) + FG + FN1
          IF(Q1==ZERO.and.
     .       Q2==ZERO.and.
     .       Q3==ZERO)F(I)=ZERO
          IF(F(I)<ZERO)F(I)=ZERO
          SIGM(I)= (YEILD0 + ET*EPSM(I)**EN)* (ONE+
     .            (EPSP(I)*CSD)**VISP)                     
          PLA(I) = EPSM(I)     
C....COMPUTE F*.....
          IF(F(I)<=FC)THEN
           FSTAR(I)=F(I)
          ELSE
           IF(FF==FC)THEN 
            FSTAR(I) = EP20
           ELSE
            FSTAR(I) = FC+ (FU-FC)*(F(I)-FC)/(FF-FC)
           ENDIF
          ENDIF
          UVAR(I,1)= EPSM(I)
          UVAR(I,2)=FSTAR(I)
          UVAR(I,3)=SIGM(I)
          UVAR(I,4)=F(I)
          UVAR(I,5)=EPSP(I)          
c..... DEVIATORIC STRESS         
         ENDIF   
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF) OFF(I)=FOUR_OVER_5
        ENDIF
       ENDDO 
C       
      ENDIF
C 
C tabulated law
       IF(IYEILD_TAB > 0) THEN
         IADBUF = IPM(7,MAT(1))-1
         XFAC  = UPARAM(IADBUF + 22 )
         YFAC  = UPARAM(IADBUF + 23 )
         DO I=1,NEL0
           NDIM= TABLE(ITAB(1))%NDIM
            IF(NDIM==2)THEN
              NXK=SIZE(TABLE(ITAB(1))%X(2)%VALUES)         
              DO J=2,NXK
                 DX2 = TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC- EPSP(I) 
                 IF(DX2 >= ZERO .OR. J == NXK)THEN
                  IPOS=J-1
                  R=(TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC-EPSP(I))/
     .                 (TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC
     .                 -TABLE(ITAB(1))%X(2)%VALUES(J-1)*XFAC)
                  EXIT
                 ENDIF
              END DO ! NXK
            ELSE
                   R = ONE
            ENDIF ! NDIM
C   interpolation on  the curve ---
           XX(1) = EPSM(I)  
           XX(2) = EPSP(I)*XFAC
           CALL TABLE_INTERP_LAW76(TABLE(ITAB(1)),IPOS,XX,
     .                     R,DFT,YY) 
            UVAR(I,3)= YFAC*YY
         ENDDO ! NEL0
       ENDIF ! IYEILD_TAB
C          
      RETURN
      END
