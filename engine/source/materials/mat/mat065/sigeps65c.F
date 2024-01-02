Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SIGEPS65C                     source/materials/mat/mat065/sigeps65c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        EQSOLV_2                      source/materials/mat/mat065/sigeps65c.F
Chd|        INTERSTAR                     source/materials/mat/mat065/sigeps65c.F
Chd|====================================================================
      SUBROUTINE SIGEPS65C(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
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
     C     GS     ,YLD    ,EPSP    ,ISRATE  ,DPLA_I )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYD| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
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
C VISCMAX | NEL    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL),MAT(NEL),ISRATE,IPM(NPROPMI,*)
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
     .   GS(*)       ,EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL),
     .    DPLA_I(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
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
c   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I1,I2,J,J1,J2,K,K1,N0,N1,N2,N3,IADBUF,NFUNC,FUNC,FUND,
     .   NP1,NP2,NRATE
      INTEGER JJ(NEL),IFUNC(NEL,100),
     .   IPOS(NEL),IAD(NEL),ILEN(NEL),     
     .   IPOSC1(NEL),IADC1(NEL),ILENC1(NEL),     
     .   IPOSC2(NEL),IADC2(NEL),ILENC2(NEL),
     .   IPOSD1(NEL),IADD1(NEL),ILEND1(NEL),     
     .   IPOSD2(NEL),IADD2(NEL),ILEND2(NEL)
      my_real
     .   R,DEZZ,DPLA,A11,A22,RATE1,RATE2,SIGY1,SIGY2,DYDX,DTDS,ALPHA,
     .   S1,S2,SX,T1,T2,TY,X1,X2,Y1,Y2,A,B,C,EPSEFF,SFC,SFD
      my_real 
     .   E(NEL),A1(NEL),A2(NEL),G(NEL),G3(NEL),HC(NEL),HD(NEL),
     .   NU(NEL),NNU1(NEL),NU3(NEL),EPSMAX(NEL),FAC(NEL),DEPLZZ(NEL),
     .   SIGO(NEL),SVM(NEL),SIGYLD(NEL),
     .   DYDXC1(NEL),DYDXC2(NEL),YC1(NEL),YC2(NEL),
     .   DYDXD1(NEL),DYDXD2(NEL),YD1(NEL),YD2(NEL),
     .   DEPSE(NEL),EPSE(NEL),DEPSS(NEL),EPSS(NEL),
     .   DSIGXX(NEL),DSIGYY(NEL),DSIGXY(NEL),
     .   CRIT(NEL),SIGFD(NEL),SIGFC(NEL),DSIGV(NEL),
     .   SDS(NEL),DSIG2(NEL),SV2(NEL),DSVM(NEL),
     .   EPSC1(NEL),EPSC2(NEL),EPSD1(NEL),EPSD2(NEL),
     .   DYDXC(NEL),YC(NEL),YFAC1(NEL),YFAC2(NEL)
C======================================================================|
c      NUVAR = 5 + NRATE*4
c      UVAR(I,1) = epsilon star*
c      UVAR(I,2) = elastic strain
c      UVAR(I,3) = Von Mises stress     
c      UVAR(I,4) = PLA(I)
c      UVAR(I,5) = EPSP(I)
c      UVAR(I,5+J1) = IPOSC1(I)        
c      UVAR(I,5+J2) = IPOSC2(I)        
c      UVAR(I,5+NRATE+J1)  =EPSC1(I)       
c      UVAR(I,5+NRATE+J2)  =EPSC2(I)       
c      UVAR(I,5+NRATE*2+J1)=IPOSD1(I)       
c      UVAR(I,5+NRATE*2+J2)=IPOSD2(I)       
c      UVAR(I,5+NRATE*3+J1)=EPSD1(I)       
c      UVAR(I,5+NRATE*3+J2)=EPSD2(I)       
C======================================================================|
      DO I=1,NEL                                     
        NFUNC  = IPM(10,MAT(I))                       
        DO J=1,NFUNC                                 
          IFUNC(I,J)=IPM(10+J,MAT(I))
        ENDDO                                         
      ENDDO                                           
      IADBUF = IPM(7,MAT(1)) - 1                    
      DO I=1,NEL                                     
        E(I)   = UPARAM(IADBUF+2)                     
        G(I)   = UPARAM(IADBUF+3)                     
        NU(I)  = UPARAM(IADBUF+4)                     
        A1(I)  = UPARAM(IADBUF+5)                     
        A2(I)  = UPARAM(IADBUF+6)                     
        G3(I)  = UPARAM(IADBUF+8)                  
        NNU1(I)= NU(I) / (ONE - NU(I))                 
        NU3(I) = ONE - NNU1(I)                   
        EPSMAX(I)= UPARAM(IADBUF+11)      
      ENDDO
      NRATE = NINT(UPARAM(IADBUF+1))
      N0 = 5
      N1 = NRATE + N0
      N2 = NRATE + N1
      N3 = NRATE + N2
C------------------------------------------
      IF (TIME == ZERO)THEN 
        IF (ISIGI == 0) THEN   
          DO I=1,NEL        
            DO J=1,10 
              UVAR(I,J)=ZERO 
            ENDDO            
            UVAR(I,3) = SQRT(SIGOXX(I)*SIGOXX(I) + SIGOYY(I)*SIGOYY(I)       
     .                 -SIGOXX(I)*SIGOYY(I) + SIGOXY(I)*SIGOXY(I)*THREE)
          ENDDO              
        ENDIF
c----  find yield values
       DO I=1,NEL                                   
        DO I1=1,NRATE
          FUNC = IFUNC(I,I1)
          FUND = IFUNC(I,I1+NRATE)
          NP1  = (NPF(FUNC+1)-NPF(FUNC))*HALF
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF
c---
          DO J=3,NP1
            J1=2*(J-2)
            S1=TF(NPF(FUNC)+J1)
            S2=TF(NPF(FUNC)+J1+2)
            T1=TF(NPF(FUNC)+J1+1)
            T2=TF(NPF(FUNC)+J1+3)
            DO K=3,NP2
              K1=2*(K-2)
              X1=TF(NPF(FUND)+K1)
              X2=TF(NPF(FUND)+K1+2)
              Y1=TF(NPF(FUND)+K1+1)
              Y2=TF(NPF(FUND)+K1+3)
              IF (Y2>=T1 .AND. Y1<=T2 .AND. X2>=S1 .AND. X1<=S2) THEN
                DYDX = (Y2-Y1) / (X2-X1)
                DTDS = (T2-T1) / (S2-S1)
                IF (DYDX > DTDS) THEN
                  SX = (T1-Y1-DTDS*S1+DYDX*X1) / (DYDX-DTDS)
                  TY =  T1 + DTDS*(SX - S1)
                  IF (TY>=Y1 .AND. TY<=Y2 .AND. SX>=X1 .AND. SX<=X2)THEN
                    IADBUF = IPM(7,MAT(I)) + 13                
                    UPARAM(IADBUF+I1+NRATE*2) = TY
                    GOTO 150
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
 150      CONTINUE
c---
        ENDDO
       ENDDO
      ENDIF                  
C-------------------
C     STRAIN RATE - NON FILTERED
C-------------------
      DO I=1,NEL
        IF (ISRATE == 0)
     .  EPSP(I) = HALF*(ABS(EPSPXX(I)+EPSPYY(I))
     .          + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .          + EPSPXY(I)*EPSPXY(I) ) )
      ENDDO                                                           
C-------------------
C        STRAIN 
C-------------------
      DO I=1,NEL
        EPSS(I) = UVAR(I,1) 
        EPSE(I) = UVAR(I,2) 
      ENDDO              
C-------------------
C        ELASTIC STRESS
C-------------------
      DO I=1,NEL
       A11 = E(I) * A1(I)                                            
       A22 = E(I) * A2(I)                                            
C---
       DSIGXX(I) = A11*DEPSXX(I)+A22*DEPSYY(I)                       
       DSIGYY(I) = A22*DEPSXX(I)+A11*DEPSYY(I)                       
       DSIGXY(I) = G(I)*DEPSXY(I)                                    
C
       SIGNXX(I) = SIGOXX(I)+DSIGXX(I)                               
       SIGNYY(I) = SIGOYY(I)+DSIGYY(I)                               
       SIGNXY(I) = SIGOXY(I)+DSIGXY(I)                               
       SIGNYZ(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)                         
       SIGNZX(I) = SIGOZX(I) + GS(I)*DEPSZX(I)                   
C-------------------
       CRIT(I)   = SIGOXX(I)*DEPSXX(I) + SIGOYY(I)*DEPSYY(I)         
     .           + HALF*(SIGOXY(I)*DEPSXY(I))                      
       DSVM(I)   = DSIGXX(I)*DSIGXX(I) + DSIGYY(I)*DSIGYY(I)         
     .            -DSIGXX(I)*DSIGYY(I) + DSIGXY(I)*DSIGXY(I)*THREE   
       SIGO(I)   = UVAR(I,3)                                         
       SV2(I)    = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)         
     .            -SIGNXX(I)*SIGNYY(I) + SIGNXY(I)*SIGNXY(I)*THREE   
       SVM(I)    = SQRT(SV2(I))                                      
       DSIG2(I)  = DSIGXX(I)*DSIGXX(I) + DSIGYY(I)*DSIGYY(I)        
     .            -DSIGXX(I)*DSIGYY(I) + DSIGXY(I)*DSIGXY(I)*THREE  
       SDS(I)    = (SIGOXX(I)*DSIGXX(I) + SIGOYY(I)*DSIGYY(I)        
     .            +SIGOXY(I)*DSIGXY(I)*THREE)*TWO                  
     .            -SIGOXX(I)*DSIGYY(I) - SIGOYY(I)*DSIGXX(I)        
C-------------------
C        SOUND SPEED, TANGENT MODULUS
C-------------------
c         SOUNDSP(I) = SQRT(A11/RHO0(I))
        SOUNDSP(I) = SQRT(E(I)/RHO0(I)) 
        VISCMAX(I) = ZERO               
        ETSE(I) = ONE 
      ENDDO
C-------------------
C     Interpolation
C-------------------
      IADBUF = IPM(7,MAT(1))-1+14
      DO I=1,NEL                                   
        JJ(I) = 1                                   
        DO J=2,NRATE-1                      
          IF (EPSP(I) > UPARAM(IADBUF+J)) JJ(I) = J 
        ENDDO 
C
        I1 = IADBUF + JJ(I)
        J1 = JJ(I)                                                    
        RATE1 = UPARAM(I1)                             
        YFAC1(I) = UPARAM(I1+NRATE)                   
        SIGY1 = UPARAM(I1+NRATE*2)*YFAC1(I) 
C
        IF (NRATE > 1) THEN                                    
          I2 = I1 + 1                                               
          J2 = J1 + 1 
c
          RATE2 = UPARAM(I2)
          YFAC2(I) = UPARAM(I2+NRATE)                   
          FAC(I) = (EPSP(I) - RATE1) / (RATE2 - RATE1) 
          SIGY2 = UPARAM(I2+NRATE*2)*YFAC2(I)
          SIGYLD(I) = SIGY1 + FAC(I)*(SIGY2-SIGY1)
c         loading curves
          IPOSC1(I) = NINT(UVAR(I,N0+J1))          
          IPOSC2(I) = NINT(UVAR(I,N0+J2))
          EPSC1(I)  = UVAR(I,N1+J1)
          EPSC2(I)  = UVAR(I,N1+J2)
          IADC1(I)  = HALF*NPF(IPM(10+J1,MAT(I))) + 1
          IADC2(I)  = HALF*NPF(IPM(10+J2,MAT(I))) + 1
          ILENC1(I) = HALF*NPF(IPM(10+J1,MAT(I))+1)-IADC1(I)-IPOSC1(I)
          ILENC2(I) = HALF*NPF(IPM(10+J2,MAT(I))+1)-IADC2(I)-IPOSC2(I)
c         unloading curves
          IPOSD1(I) = NINT(UVAR(I,N2+J1))          
          IPOSD2(I) = NINT(UVAR(I,N2+J2))
          EPSD1(I)  = UVAR(I,N3+J1)
          EPSD2(I)  = UVAR(I,N3+J2)
          IADD1(I)  = HALF*NPF(IPM(10+NRATE+J1,MAT(I))) + 1
          IADD2(I)  = HALF*NPF(IPM(10+NRATE+J2,MAT(I))) + 1
          ILEND1(I) = HALF*NPF(IPM(10+NRATE+J1,MAT(I))+1)
     .              - IADD1(I) - IPOSD1(I)
          ILEND2(I) = HALF*NPF(IPM(10+NRATE+J2,MAT(I))+1)
     .              - IADD2(I) - IPOSD2(I)
        ELSE
          SIGYLD(I) = SIGY1
          IPOSC1(I) = NINT(UVAR(I,N0+J1))          
          IPOSD1(I) = NINT(UVAR(I,N2+J1))          
          EPSC1(I)  = UVAR(I,N1+J1)
          EPSD1(I)  = UVAR(I,N3+J1)

          IADC1(I)  = HALF*NPF(IPM(10+J1,MAT(I))) + 1
          IADD1(I)  = HALF*NPF(IPM(10+NRATE+J1,MAT(I))) + 1
          ILENC1(I) = HALF*NPF(IPM(10+J1,MAT(I))+1)-IADC1(I)-IPOSC1(I)
          ILEND1(I) = HALF*NPF(IPM(10+NRATE+J1,MAT(I))+1)
     .              - IADD1(I) - IPOSD1(I)
        ENDIF
      ENDDO                                         
C-------------------
      IF (NRATE > 1) THEN
c       Sigma load = f(total strain) 
        CALL INTERSTAR(TF   ,IADC1,IPOSC1,ILENC1,NEL  ,
     .                 EPSS ,EPSC1,DYDXC1,E     ,YC1  ,YFAC1 )    
        CALL INTERSTAR(TF   ,IADC2,IPOSC2,ILENC2,NEL  ,
     .                 EPSS ,EPSC2,DYDXC2,E     ,YC2  ,YFAC2 )
c       Sigma unload = f(elastic strain) 
        CALL INTERSTAR(TF   ,IADD1,IPOSD1,ILEND1,NEL  ,
     .                 EPSE ,EPSD1,DYDXD1,E     ,YD1  ,YFAC1 )     
        CALL INTERSTAR(TF   ,IADD2,IPOSD2,ILEND2,NEL  ,
     .                 EPSE ,EPSD2,DYDXD2,E     ,YD2  ,YFAC2 )       
      ELSE                                
        CALL INTERSTAR(TF   ,IADC1,IPOSC1,ILENC1,NEL  ,
     .                 EPSS ,EPSC1,DYDXC1,E     ,YC1  ,YFAC1 )
        CALL INTERSTAR(TF   ,IADD1,IPOSD1,ILEND1,NEL  ,
     .                 EPSE ,EPSD1,DYDXD1,E     ,YD1  ,YFAC1 )     
      ENDIF
C-------------------
      IF (NRATE > 1) THEN                                    
        DO I=1,NEL                                   
          J1 = JJ(I)                                                      
          J2 = J1 + 1                                  
          UVAR(I,N0+J1) = IPOSC1(I)   
          UVAR(I,N0+J2) = IPOSC2(I)   
          UVAR(I,N1+J1) = EPSC1(I)       
          UVAR(I,N1+J2) = EPSC2(I)       
          UVAR(I,N2+J1) = IPOSD1(I)
          UVAR(I,N2+J2) = IPOSD2(I)
          UVAR(I,N3+J1) = EPSD1(I)
          UVAR(I,N3+J2) = EPSD2(I)
c
          YC1(I)   = YC1(I)                                     
          YD1(I)   = YD1(I)                                      
          YC2(I)   = YC2(I)                                 
          YD2(I)   = YD2(I)                                       
          DYDXC1(I)= DYDXC1(I)                               
          DYDXD1(I)= DYDXD1(I)                                
          DYDXC2(I)= DYDXC2(I)                              
          DYDXD2(I)= DYDXD2(I)                                  
c
          HC(I)    = DYDXC1(I) + (DYDXC2(I)-DYDXC1(I))*FAC(I) 
          HD(I)    = DYDXD1(I) + (DYDXD2(I)-DYDXD1(I))*FAC(I)
          SIGFC(I) = YC1(I)    + (YC2(I)-YC1(I))*FAC(I) 
          SIGFD(I) = YD1(I)    + (YD2(I)-YD1(I))*FAC(I) 
c
        ENDDO                                         
      ELSE
        DO I=1,NEL                                   
          J1 = JJ(I)                                                      
          UVAR(I,N0+J1) = IPOSC1(I)   
          UVAR(I,N1+J1) = EPSC1(I)       
          UVAR(I,N2+J1) = IPOSD1(I)
          UVAR(I,N3+J1) = EPSD1(I)
c
          SIGFC(I) = YC1(I) 
          SIGFD(I) = YD1(I) 
          HC(I)    = DYDXC1(I) 
          HD(I)    = DYDXD1(I) 
        ENDDO                                         
      ENDIF
C-------------------
C     PROJECTION
C-------------------
     
      DO I=1,NEL                                                    
        DEPSS(I)  = ZERO                                              
        DEPSE(I)  = ZERO                                              
        DEPLZZ(I) = ZERO
c----
        IF (SVM(I) > SIGFC(I)) THEN                                  
c         load                                                      
          DEPSS(I) = (SVM(I) - SIGFC(I)) / (E(I) + HC(I))            
          SFC = SIGFC(I) + HC(I)*DEPSS(I)                            
          SFD = SIGFD(I) + HD(I)*DEPSS(I)                            
C
          A = DSIG2(I)                                               
          B = SDS(I)                                                 
          C = SIGO(I)*SIGO(I) - SFC*SFC                              
          CALL EQSOLV_2(A,B,C,X1,X2)                                 
          ALPHA = MAX(X1,X2)                                         
          SIGNXX(I) = SIGOXX(I) + DSIGXX(I)*ALPHA                    
          SIGNYY(I) = SIGOYY(I) + DSIGYY(I)*ALPHA                    
          SIGNXY(I) = SIGOXY(I) + DSIGXY(I)*ALPHA                    
c
          IF (SFD > SFC) THEN                                        
            DEPSS(I) = (SVM(I) - SIGFC(I)) / (G3(I) + HC(I))         
            SIGFC(I) = SIGFC(I) + HC(I)*DEPSS(I)                     
            SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)                     
c
            A = DSIG2(I)                                             
            B = SDS(I)                                               
            C = SIGO(I)*SIGO(I) - SIGFC(I)*SIGFC(I)                  
            CALL EQSOLV_2(A,B,C,X1,X2)                               
            ALPHA = MAX(X1,X2)                                       
            SIGNXX(I) = SIGOXX(I) + DSIGXX(I)*ALPHA                  
            SIGNYY(I) = SIGOYY(I) + DSIGYY(I)*ALPHA                  
            SIGNXY(I) = SIGOXY(I) + DSIGXY(I)*ALPHA               
c                                                                     
            DPLA_I(I) = DEPSS(I)
            PLA(I) = PLA(I) + OFF(I)*DPLA_I(I)                     
            DEPLZZ(I) = DPLA_I(I)*(SIGNXX(I)+SIGNYY(I))*HALF*NU3(I)
     .                / MAX(EM20,SIGFC(I))
            ETSE(I)= HC(I)/(HC(I)+E(I))                              
          ELSE                                                       
            SIGFC(I) = SFC                                           
            SIGFD(I) = SFD                                           
            DPLA_I(I)= ZERO                                          
          ENDIF                                                      
c----
        ELSEIF (SVM(I) < SIGFD(I)) THEN                              
c         unload                                                     
          DEPSS(I) = (SVM(I) - SIGFD(I)) / (E(I) + HD(I))            
          SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)                       
          A = DSIG2(I)                                               
          B = SDS(I)                                                 
          C = SIGO(I)*SIGO(I) - SIGFD(I)*SIGFD(I)                    
          CALL EQSOLV_2(A,B,C,X1,X2)                                 
          X1 = MAX(X1,ZERO)                                          
          X2 = MAX(X2,ZERO)                                          
          ALPHA =  MIN(X1,X2)                                        
          SIGNXX(I) = SIGOXX(I) + DSIGXX(I)*ALPHA                    
          SIGNYY(I) = SIGOYY(I) + DSIGYY(I)*ALPHA                    
          SIGNXY(I) = SIGOXY(I) + DSIGXY(I)*ALPHA                    
          DPLA_I(I) = ZERO                          
c----
        ELSE                                                         
C         cas elastique, pente E => depss = 0                        
          DPLA_I(I) = ZERO                                           
        ENDIF                                                        
      ENDDO                                                          
c----------------------------------------------
      DO I=1,NEL   
        DEZZ  = (DEPSXX(I)+DEPSYY(I))*NNU1(I) + DEPLZZ(I)       
        THK(I)= THK(I) - DEZZ*THKLY(I)*OFF(I)
c
        SVM(I)  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)         
     .           -SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
        EPSS(I) = EPSS(I) + DEPSS(I)                             
        EPSE(I) = EPSS(I) - PLA(I)   
c
        UVAR(I,1) = EPSS(I) 
        UVAR(I,2) = EPSE(I)
        UVAR(I,3) = SVM(I)
        UVAR(I,4) = PLA(I)
        UVAR(I,5) = EPSP(I)
        IF (PLA(I) > EPSMAX(I) .and. OFF(I) == ONE) OFF(I)=FOUR_OVER_5         
c        EPSEFF=sqrt((EPSXX(I)*EPSXX(I)+EPSYY(I) *EPSYY(I)+EPSXY(I)
c     .        *EPSXY(I))/(un+deux*NU(I)*NU(I)))         
C
      ENDDO                                                            
C------------------------------------------
      RETURN
      END SUBROUTINE SIGEPS65C

Chd|====================================================================
Chd|  EQSOLV_2                      source/materials/mat/mat065/sigeps65c.F
Chd|-- called by -----------
Chd|        SIGEPS65                      source/materials/mat/mat065/sigeps65.F
Chd|        SIGEPS65C                     source/materials/mat/mat065/sigeps65c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EQSOLV_2(A,B,C,X1,X2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      my_real 
     .   A,B,C,X
C-----------------------------------------------
c   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .   DELTA,X1,X2
C-----------------------------------------------
      X1  = ZERO
      X2  = ZERO
      IF (A /= ZERO) THEN
        DELTA = B*B - FOUR*A*C
        IF (DELTA > ZERO) THEN
          DELTA = SQRT(DELTA)
          X2 = (-B - DELTA)*HALF/A
          X1 = -B/A - X2
        ELSEIF (DELTA == ZERO) THEN
          X1  = -B*HALF/A
          X2  = X1
        ENDIF
      ENDIF 
c-----------
      RETURN
      END SUBROUTINE EQSOLV_2
      
Chd|====================================================================
Chd|  INTERSTAR                     source/materials/mat/mat065/sigeps65c.F
Chd|-- called by -----------
Chd|        SIGEPS65                      source/materials/mat/mat065/sigeps65.F
Chd|        SIGEPS65C                     source/materials/mat/mat065/sigeps65c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSTAR(TF    ,IAD   ,IPOS  ,ILEN  ,NEL   ,
     .                     X     ,EPSS_i,DYDX  ,E     ,Y  ,YFAC   )
C-----------------------------------------------
c   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL
      INTEGER I,J,J1,J2,ICONT
      my_real 
     .  X(*),DYDX(*),Y(*),TF(2,*),EPSS_i(*),E(*) 
      my_real 
     .  DX,DY,DERI,X1,X2,Y1,Y2,EPSS_ii(NEL),YFAC(NEL)
C=======================================================================
      J = 0
 100  CONTINUE
      J = J+1                                            
      ICONT = 0                                          
c
      DO I=1,NEL                                         
        J1 = IPOS(I)+IAD(I)                           
        J2 = J1 + 1                                   
        X1 = TF(1,J1)                                 
        X2 = TF(1,J2)                                 
        Y2 = TF(2,J2)                                 
        Y1 = TF(2,J1)                                 
        DX = X2 - X1                              
        DY = Y2 - Y1                              
        DERI = DY*YFAC(I) / MAX(DX, EM20)                        
        DX = DX*(E(I) - DERI)/E(I)                       
        EPSS_ii(I) = EPSS_i(I) + DX                      
        IF (J < ILEN(I) .and. X(I) > EPSS_ii(I)) THEN    
          ICONT = 1                                      
          IPOS(I)   =IPOS(I) + 1                         
          EPSS_i(I) = EPSS_ii(I)
        ELSEIF (IPOS(I) > 0 .and. X(I) < EPSS_i(I)) THEN 
          ICONT   = 1                                    
          EPSS_ii(I) = EPSS_i(I)
          X2 = X1                                
          Y2 = Y1                                
          IPOS(I) = IPOS(I) - 1                          
          J1 = IPOS(I)+IAD(I)                            
          X1 = TF(1,J1)                               
          Y1 = TF(2,J1)                               
          DX = X2 - X1                            
          DY = Y2 - Y1                            
          DERI = DY*YFAC(I) / MAX(DX, EM20)                      
          DX = DX*(E(I) - DERI)/E(I)                     
          EPSS_i(I) = EPSS_ii(I) - DX                    
        ENDIF                                            
      ENDDO                                              
C
      IF (ICONT == 1) GOTO 100
c-----------
      DO I=1,NEL
        J1 =IPOS(I)+IAD(I)
        J2 = J1+1
        DYDX(I)=(TF(2,J2)-TF(2,J1))*YFAC(I)/(EPSS_ii(I)-EPSS_i(I))
        Y(I)   = TF(2,J1)*YFAC(I) + DYDX(I)*(X(I)-EPSS_i(I))
      ENDDO
c-----------
      RETURN
      END SUBROUTINE INTERSTAR
