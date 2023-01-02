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
Chd|  SIGEPS65                      source/materials/mat/mat065/sigeps65.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        EQSOLV_2                      source/materials/mat/mat065/sigeps65c.F
Chd|        INTERSTAR                     source/materials/mat/mat065/sigeps65c.F
Chd|====================================================================
      SUBROUTINE SIGEPS65(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPM    ,
     B     MAT    ,EPSP    ,IPLA    ,YLD     ,PLA    ,ETSE   ,
     C     DPLA   ,AMU     )

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
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C=======================================================================
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,IPT,IPLA, JTHE,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL),ETSE(NEL), TEMPEL(NEL),AMU(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),DPLA(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  YLD(NEL), PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   FINTER2, TF(*),FINTER
      EXTERNAL FINTER2,FINTER
C-----------------------------------------------
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
      INTEGER I,I1,I2,J,IADBUF,J1,J2,NFUNC,FUNC,FUND,
     .        NP1,NP2,K1,K,NRATE,N0,N1,N2,N3,N4
      INTEGER JJ(NEL),MX,
     .   IPOSC1(NEL),IADC1(NEL),ILENC1(NEL),     
     .   IPOSC2(NEL),IADC2(NEL),ILENC2(NEL),
     .   IPOSD1(NEL),IADD1(NEL),ILEND1(NEL),FLAG(NEL),     
     .   IPOSD2(NEL),IADD2(NEL),ILEND2(NEL),IFUNC(100)

C
      my_real
     .        S1,S2,T1,T2,X1,X2,Y1,Y2,SX,TY,DYDX,DTDS,RATE1,
     .        RATE2,SIGY1,SIGY2,SVTEST,TEST,HH,DESEL
      my_real
     .        E(NEL),NU,G3,G,EPS(NEL),C1,
     .        EPSMAX,TRSIGO(NEL),NUU(NEL),EPSS(NEL),
     .        EPS0(NEL),DEPSEQ(NEL),DSIGXX(NEL),DSIGYY(NEL),
     .        DSIGZZ(NEL),SV2(NEL),SIGO(NEL),SVM(NEL),
     .        STRXX(NEL),STRYY(NEL),STRZZ(NEL),SIGM(NEL),
     .        STRXY(NEL),STRYZ(NEL),STRZX(NEL),TRSIGN(NEL),
     .        RBULK,G2,
     .        DYDXC1(NEL),DYDXC2(NEL),YC1(NEL),YC2(NEL),
     .        DYDXD1(NEL),DYDXD2(NEL),YD1(NEL),YD2(NEL),
     .        SIGYLD(NEL),HC(NEL),HD(NEL),SIGFC(NEL), SIGFD(NEL), 
     .        FAC(NEL),EPSE(NEL),YFAC(NEL,2),DEPSS(NEL),H(NEL),
     .        DEPSE(NEL),DEQ(NEL),YIELD(NEL),UNLOAD(NEL),
     .        EPSC1(NEL),EPSC2(NEL),EPSD1(NEL),EPSD2(NEL),WORK(NEL),
     .        DYDXC(NEL),YC(NEL),YFAC1(NEL),YFAC2(NEL),COEFB(NEL),
     .        DEXX(NEL),DEYY(NEL),DEZZ(NEL),P0(NEL),COEFA(NEL),
     .        SOXX(NEL),SOYY(NEL),SOZZ(NEL) ,SVMO2(NEL),DSIG(NEL),
     .        A,B,CC,X11,X22,DAV,ALPHA,SFC,SFD,P
C
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      MX = MAT(1)                                 
      NFUNC  = IPM(10,MX)                       
      DO J=1,NFUNC                                 
         IFUNC(J)=IPM(10+J,MX)
      ENDDO                                         
      IADBUF = IPM(7,MX) - 1                    
      NRATE = NINT(UPARAM(IADBUF+1))       
      G   = UPARAM(IADBUF+3)                     
      NU  = UPARAM(IADBUF+4)                     
      G2  = UPARAM(IADBUF+7)
      G3  = UPARAM(IADBUF+8)                  
      EPSMAX= UPARAM(IADBUF+11)      
      C1=UPARAM(IADBUF+2)/THREE/(ONE - TWO*NU)
C   
      RBULK = UPARAM(IADBUF+2)*THIRD/(ONE-TWO*NU)
      DO I=1,NEL                                            
        E(I)   = UPARAM(IADBUF+2)                     
      ENDDO                                           
      NRATE = NINT(UPARAM(IADBUF+1))         
      N0 = 5
      N1 = NRATE + N0
      N2 = NRATE + N1
      N3 = NRATE + N2 
      N4 = NRATE + N3 + 1 !! = 5 + 4*NRATE +1
c------------------------------------------
      IF (TIME == ZERO)THEN 
        IF (ISIGI == 0) THEN   
          DO I=1,NEL        
            DO J=1,10 
              UVAR(I,J)=ZERO 
            ENDDO        
C --- OLD EFFECTIVE STRESS    
            TRSIGO(I)=-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
            UVAR(I,3) = SQRT(THREE_HALF*(
     .        (SIGOXX(I)+TRSIGO(I))*(SIGNXX(I)+TRSIGO(I))
     .       +(SIGOYY(I)+TRSIGO(I))*(SIGNYY(I)+TRSIGO(I))  
     .       +(SIGOZZ(I)+TRSIGO(I))*(SIGNZZ(I)+TRSIGO(I))
     .       +TWO*(SIGOXY(I)*SIGOXY(I)+SIGOYZ(I)*SIGOYZ(I)
     .       +SIGOZX(I)*SIGOZX(I))))
          ENDDO              
        ENDIF
c----  find yield values
       DO I=1,NEL                                   
        DO I1=1,NRATE
          FUNC = IFUNC(I1)
          FUND = IFUNC(I1+NRATE)
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
                    IADBUF = IPM(7,MX) + 13                
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
      ENDIF    !TIME == ZERO              
C-------------------
C        STRAIN 
C-------------------
      DO I=1,NEL
        EPSS(I) = UVAR(I,1) 
        EPSE(I) = UVAR(I,2) 
      ENDDO              

      DO I=1,NEL
         EPS(I) = (ONE/(ONE+NU))*SQRT((EPSXX(I)*EPSXX(I)+EPSYY(I)*EPSYY(I)+
     .        EPSZZ(I)*EPSZZ(I)+TWO*(EPSXY(I)*EPSXY(I)+
     .        EPSYZ(I)*EPSYZ(I)+EPSZX(I)*EPSZX(I))))
         DEPSEQ(I) = (ONE/(ONE+NU))*SQRT((DEPSXX(I)*DEPSXX(I) + DEPSYY(I)*
     .        DEPSYY(I)+DEPSZZ(I)*DEPSZZ(I)+TWO*(DEPSXY(I)*DEPSXY(I)+
     .        DEPSYZ(I)*DEPSYZ(I) +DEPSZX(I)*DEPSZX(I))))
      ENDDO              
C-------------------
C        ELASTIC STRESS
C-------------------
      DO I=1,NEL                        
C---
c     ..strain increment deviator ..
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
c     ..pressure..        
        P0(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
c     ..strain increment deviator tensor  ..
        DEXX(I) = DEPSXX(I)-DAV
        DEYY(I) = DEPSYY(I)-DAV
        DEZZ(I) = DEPSZZ(I)-DAV
c     ..stress increment deviator tensor  ..
        SOXX(I)  =SIGOXX(I)+P0(I) 
        SOYY(I)  =SIGOYY(I)+P0(I) 
        SOZZ(I)  =SIGOZZ(I)+P0(I) 
                                
        SVMO2(I) = TWO_THIRD*UVAR(I,3)*UVAR(I,3)
c     ..deviatoric elastic predictor ..
        SIGNXX(I)=SOXX(I)+G2*DEXX(I)
        SIGNYY(I)=SOYY(I)+G2*DEYY(I)
        SIGNZZ(I)=SOZZ(I)+G2*DEZZ(I)
        SIGNXY(I)=SIGOXY(I)+G *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+G *DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+G *DEPSZX(I)

C
       SV2(I)    = THREE_HALF*(SIGNXX(I)*SIGNXX(I)+SIGNYY(I)*SIGNYY(I)
     .              +SIGNZZ(I) *SIGNZZ(I)
     .        +TWO*(SIGNXY(I)*SIGNXY(I)
     .              +SIGNYZ(I)*SIGNYZ(I)
     .              +SIGNZX(I)*SIGNZX(I)))
       SVM(I)    = SQRT(SV2(I))    


       P = C1 * (RHO(I)/RHO0(I)-ONE)
       WORK(I) = (SIGNXX(I)-P)*DEPSXX(I) +
     .           (SIGNYY(I)-P)*DEPSYY(I) +                                     
     .           (SIGNZZ(I)-P)*DEPSZZ(I) +                                     
     .           TWO*SIGNXY(I) *DEPSXY(I) +                                     
     .           TWO*SIGNYZ(I) *DEPSYZ(I) +                                     
     .           TWO*SIGNZX(I) *DEPSZX(I)     

       DSIG(I) = SQRT(THREE_HALF* ( (G2*DEXX(I))**2+
     .                         (G2*DEYY(I))**2+
     .                         (G2*DEZZ(I))**2+
     .       TWO*((G *DEPSXY(I))**2+
     .             (G *DEPSYZ(I))**2+
     .             (G *DEPSZX(I))**2) ))  
       
C-------------------
        COEFA(I) = TWO* G*G *(TWO*(DEXX(I)*DEXX(I)+
     .             DEYY(I)*DEYY(I)    + DEZZ(I)*DEZZ(I)) +
     .             DEPSXY(I)*DEPSXY(I)+DEPSYZ(I)*DEPSYZ(I)+   
     .             DEPSZX(I)*DEPSZX(I))   

        COEFB(I) = FOUR *G*(SOXX(I)*DEXX(I) 
     .             +SOYY(I)*DEYY(I)+SOZZ(I)*DEZZ(I)+SIGOXY(I)*DEPSXY(I)
     .             +SIGOYZ(I)*DEPSYZ(I)+SIGOZX(I)*DEPSZX(I))          
      
C-------------------
C        SOUND SPEED
C-------------------
        SOUNDSP(I) = SQRT((RBULK+FOUR_OVER_3*G)/RHO0(I))
        VISCMAX(I) = ZERO               
      ENDDO
C-------------------
C     Interpolation
C-------------------
      IADBUF = IPM(7,MAT(1))-1+14
      MX = MAT(1)
      DO I=1,NEL                                   
        JJ(I) = 1                                   
        DO J=2,NRATE-1                      
          IF (EPSP(I) > UPARAM(IADBUF+J)) JJ(I) = J 
        ENDDO 
C
        I1 = IADBUF+JJ(I)
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
          IADC1(I)  = HALF*NPF(IPM(10+J1,MX)) + 1
          IADC2(I)  = HALF*NPF(IPM(10+J2,MX)) + 1
          ILENC1(I) = HALF*NPF(IPM(10+J1,MX)+1)-IADC1(I)-IPOSC1(I)
          ILENC2(I) = HALF*NPF(IPM(10+J2,MX)+1)-IADC2(I)-IPOSC2(I)
c         unloading curves
          IPOSD1(I) = NINT(UVAR(I,N2+J1))          
          IPOSD2(I) = NINT(UVAR(I,N2+J2))
          EPSD1(I)  = UVAR(I,N3+J1)
          EPSD2(I)  = UVAR(I,N3+J2)
          IADD1(I)  = HALF*NPF(IPM(10+NRATE+J1,MX)) + 1
          IADD2(I)  = HALF*NPF(IPM(10+NRATE+J2,MX)) + 1
          ILEND1(I) = HALF*NPF(IPM(10+NRATE+J1,MX)+1)
     .              - IADD1(I) - IPOSD1(I)
          ILEND2(I) = HALF*NPF(IPM(10+NRATE+J2,MX)+1)
     .              - IADD2(I) - IPOSD2(I)
        ELSE
          SIGYLD(I) = SIGY1
          IPOSC1(I) = NINT(UVAR(I,N0+J1))          
          IPOSD1(I) = NINT(UVAR(I,N2+J1))         
          EPSC1(I)  = UVAR(I,N1+J1)
          EPSD1(I)  = UVAR(I,N3+J1)

          IADC1(I)  = HALF*NPF(IPM(10+J1,MX)) + 1
          IADD1(I)  = HALF*NPF(IPM(10+NRATE+J1,MX)) + 1
          ILENC1(I) = HALF*NPF(IPM(10+J1,MX)+1)-IADC1(I)-IPOSC1(I)
          ILEND1(I) = HALF*NPF(IPM(10+NRATE+J1,MX)+1)
     .              - IADD1(I) - IPOSD1(I)
        ENDIF
      ENDDO                                         
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
     .                 EPSS ,EPSC1,DYDXC1,E     ,YC1 ,YFAC1   )
        CALL INTERSTAR(TF   ,IADD1,IPOSD1,ILEND1,NEL  ,
     .                 EPSE ,EPSD1,DYDXD1,E     ,YD1 ,YFAC1  ) 
      ENDIF
c-------------------
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
c-------------------
c     PROJECTION
c-------------------
         DO I=1,NEL
          DEPSS(I) = ZERO
          DEPSE(I) = ZERO
          DEQ(I) = EPS(I) -UVAR(I,N4+1)
          FLAG(I) = INT (UVAR(I,N4+2) ) 
C
          IF (SVM(I) <  SIGFD(I))THEN !decharge
           FLAG (I) = -1
           DEPSS(I) = (SVM(I) - SIGFD(I)) / (E(I) + HD(I))    
           SFD =  SIGFD(I)            
           SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)
           DESEL = ( SIGFD(I) -  SFD )  /  E(I)    
           TEST =    DESEL + DEPSS(I)  
           IF((DEPSEQ(I)-ABS(TEST))>EM15.AND. DSIG(I)> SQRT(SVMO2(I)))THEN
             DEPSS(I) = -(SVM(I) + SFD) / (E(I) + HD(I))    
             SFD =  SIGFD(I)            
             SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)
           ENDIF
           A = COEFA(I)                                              
           B = COEFB(I)                                                
           CC = SVMO2(I) - TWO_THIRD * SIGFD(I)*SIGFD(I)                             
           CALL EQSOLV_2(A,B,CC,X11,X22)     
           X11 = MAX(X11,ZERO)                                          
           X22 = MAX(X22,ZERO)                                          
           ALPHA =  MIN(X11,X22)                   
c   
           SIGNXX(I)=SOXX(I)+ALPHA*G2*DEXX(I)
           SIGNYY(I)=SOYY(I)+ALPHA*G2*DEYY(I)
           SIGNZZ(I)=SOZZ(I)+ALPHA*G2*DEZZ(I)
           SIGNXY(I)=SIGOXY(I)+ALPHA*G *DEPSXY(I)
           SIGNYZ(I)=SIGOYZ(I)+ALPHA*G *DEPSYZ(I)
           SIGNZX(I)=SIGOZX(I)+ALPHA*G *DEPSZX(I)
C    
          ELSEIF (SVM(I) >= SIGFC(I)) THEN  ! .AND.WORK(I)>ZERO      
           FLAG (I) = 1
           DEPSS(I) = (SVM(I) - SIGFC(I)) / (E(I) + HC(I))                  
           SFC = SIGFC(I) + HC(I)*DEPSS(I)         
           SFD = SIGFD(I) + HD(I)*DEPSS(I)   
           DPLA(I)= ZERO                                                             
           A = COEFA(I)                                              
           B = COEFB(I)                                                
           CC = SVMO2(I) - TWO_THIRD * SFC*SFC  
           CALL EQSOLV_2(A,B,CC,X11,X22)                                
           ALPHA = MAX(X11,X22)      
           SIGNXX(I)=SOXX(I)+ALPHA*G2*DEXX(I)
           SIGNYY(I)=SOYY(I)+ALPHA*G2*DEYY(I)
           SIGNZZ(I)=SOZZ(I)+ALPHA*G2*DEZZ(I)
           SIGNXY(I)=SIGOXY(I)+ALPHA*G *DEPSXY(I)
           SIGNYZ(I)=SIGOYZ(I)+ALPHA*G *DEPSYZ(I)
           SIGNZX(I)=SIGOZX(I)+ALPHA*G *DEPSZX(I)
           !-----------------------------------------                    
           IF (SFD > SFC) THEN  !plastic                                      
             DEPSS(I) = (SVM(I) - SIGFC(I)) / (G3 + HC(I))              
             SIGFC(I) = SIGFC(I) + HC(I)*DEPSS(I)                     
             SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)                            
             A = COEFA(I)                                            
             B = COEFB(I)                                              
             CC = SVMO2(I) - TWO_THIRD * SIGFC(I)*SIGFC(I)                           
             CALL EQSOLV_2(A,B,CC,X11,X22)                              
             ALPHA = MAX(X11,X22)      
             SIGNXX(I)=SOXX(I)+ALPHA*G2*DEXX(I)
             SIGNYY(I)=SOYY(I)+ALPHA*G2*DEYY(I)
             SIGNZZ(I)=SOZZ(I)+ALPHA*G2*DEZZ(I)
             SIGNXY(I)=SIGOXY(I)+ALPHA*G *DEPSXY(I)
             SIGNYZ(I)=SIGOYZ(I)+ALPHA*G *DEPSYZ(I)
             SIGNZX(I)=SIGOZX(I)+ALPHA*G *DEPSZX(I)                
             DPLA(I) = DEPSS(I)                                         
             PLA(I) = PLA(I) + OFF(I)*DPLA(I)                           
           ELSE                                                       
             SIGFC(I) = SFC                                                
             SIGFD(I) = SFD                                                
             DPLA(I)= ZERO                                                            
           ENDIF
          ELSEIF ( SVM(I) <  SIGFC(I))THEN   !cas elastique,pente E => depss = 0                    
           DPLA (I) = ZERO    
           IF (FLAG(I) == -1 .AND. WORK(I) < ZERO.AND.EPSS(I)>PLA(I))THEN !passage cote negatif,faux svm 
             FLAG(I) = -1
             YIELD(I)=SVM(I)+SIGFD(I)
             DEPSS(I) = -(YIELD(I)) / (E(I) + HD(I)) 
             SIGFD(I) = SIGFD(I) + HD(I)*DEPSS(I)                     
             A = COEFA(I)                                              
             B = COEFB(I)                                                
             CC = SVMO2(I) - TWO_THIRD * SIGFD(I)*SIGFD(I)                             
             CALL EQSOLV_2(A,B,CC,X11,X22)                         
             X11 = MAX(X11,ZERO)                                          
             X22 = MAX(X22,ZERO)                                          
             ALPHA =  MIN(X11,X22)    
             SIGNXX(I)=SOXX(I)+ALPHA*G2*DEXX(I)
             SIGNYY(I)=SOYY(I)+ALPHA*G2*DEYY(I)
             SIGNZZ(I)=SOZZ(I)+ALPHA*G2*DEZZ(I)
             SIGNXY(I)=SIGOXY(I)+ALPHA*G *DEPSXY(I)
             SIGNYZ(I)=SIGOYZ(I)+ALPHA*G *DEPSYZ(I)
             SIGNZX(I)=SIGOZX(I)+ALPHA*G *DEPSZX(I)
           ENDIF                                       
C
          ENDIF                                                             
        ENDDO                     
c----------------------------------------------
      DO I=1,NEL 
       SVM(I)    = SQRT(THREE_HALF*(SIGNXX(I)*SIGNXX(I)+SIGNYY(I)*SIGNYY(I)
     .              +SIGNZZ(I) *SIGNZZ(I)
     .        +TWO*(SIGNXY(I)*SIGNXY(I)
     .              +SIGNYZ(I)*SIGNYZ(I)
     .              +SIGNZX(I)*SIGNZX(I))))
c       P = C1 * (RHO(I)/RHO0(I)-ONE)
        P = C1 * AMU(I)
       SIGNXX(I)=SIGNXX(I)-P
       SIGNYY(I)=SIGNYY(I)-P
       SIGNZZ(I)=SIGNZZ(I)-P

        IF( DPLA(I) > ZERO)THEN
         H(I)=(SVM(I)-UVAR(I,3))/DPLA(I)
         UVAR(I,N4)=H(I)/G2
        ENDIF      

        EPSS(I) = EPSS(I) + DEPSS(I)                             
        EPSE(I) = EPSS(I) - PLA(I)   

        UVAR(I,1) = EPSS(I) 
        UVAR(I,2) = EPSE(I)
        UVAR(I,3) = SVM(I)
        UVAR(I,4) = PLA(I)
        UVAR(I,5) = EPSP(I)
        UVAR(I,N4+1) = EPS(I)
        UVAR(I,N4+2) = FLAG (I)


        ETSE(I)   = UVAR(I,N4)
        IF(PLA(I) > EPSMAX .AND. OFF(I) == ONE) OFF(I)=FOUR_OVER_5         
      ENDDO  
      RETURN
      END
C
