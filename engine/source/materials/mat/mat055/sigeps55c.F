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
Chd|  SIGEPS55C                     source/materials/mat/mat055/sigeps55c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS55C(
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
     C     GS     ,YLD    ,EPSP    ,ISRATE  )
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
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0,ISRATE, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),IPM(NPROPMI,*)
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
     .   GS(*)       ,EPSP(NEL0)
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
      my_real UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0)
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
      INTEGER I,J,NRATE(MVSIZ),J1,J2,N,NINDX,NMAX,IADBUF,NFUNC,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),IFUNC(MVSIZ,100),NRATEM,
     .        NRATE1,IFUNC1(100),IADBUFV(MVSIZ),NFUNCV(MVSIZ),
     .        NFUNCM, NRATEP(MVSIZ), NRATEN(MVSIZ),I1, I2,
     .        IAD1P(MVSIZ),IPOS1P(MVSIZ),ILEN1P(MVSIZ),
     .        IAD2P(MVSIZ),IPOS2P(MVSIZ),ILEN2P(MVSIZ),
     .        IAD1N(MVSIZ),IPOS1N(MVSIZ),ILEN1N(MVSIZ),
     .        IAD2N(MVSIZ),IPOS2N(MVSIZ),ILEN2N(MVSIZ),II(MVSIZ),
     .         NRATEN1, NRATEP1                  
      my_real
     .        E(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),G3(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ,2),SVM(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR(MVSIZ),EPSMAX(MVSIZ),
     .        YFAC(MVSIZ,2),NNU1(MVSIZ),NU1(MVSIZ),
     .        NU2(MVSIZ),NU3(MVSIZ),NU4(MVSIZ),NU5(MVSIZ),NU6(MVSIZ),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_I(MVSIZ),DPLA_J(MVSIZ),
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),SVMO(MVSIZ),H(MVSIZ),
     .        EPSR1(MVSIZ),EPSR2(MVSIZ),FISOKIN(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ),SIGEZX(MVSIZ),
     .        SIGEYZ(MVSIZ),YN1(MVSIZ),YN2(MVSIZ),YP1(MVSIZ),YP2(MVSIZ),
     .        HK(MVSIZ),NU(MVSIZ), VISC(MVSIZ), VISCV(MVSIZ),C1(MVSIZ),
     .        C2(MVSIZ),RATEP(MVSIZ,2),RATEN(MVSIZ,2), YLDMAX(MVSIZ),
     .        YLDMIN(MVSIZ), EPST(MVSIZ), YLDELAS(MVSIZ),NNU(MVSIZ)
      my_real
     .        R,UMR,A,B,C,AMU,S11,S22,S12,P,P2,FAC,DEZZ,
     .        SIGZ,S1,S2,S3,DPLA,VM2,NNU2,
     .        ERR,F,DF,PLA_I,Q2,YLD_I,SIGPXX,SIGPYY,SIGPXY,
     .        ALPHA,
     .        E1,A11,A21,G1,G31,NNU11,NU11,NU21,NU31,NU41,NU51,NU61,
     .        VISC1, VISCV1
     .        DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,NUX,C11, C21  
      my_real
     .        Y11(MVSIZ),Y21(MVSIZ), ME, MA1, MA2, MG, MNU,SVM_OLD,
     .        DEV, DTINV, LAMDA, DEPS33, SVM1
      INTEGER JST(MVSIZ+1), IC, MNRATE, ITER, MX

C-----------------------------------------------
C------------------------------------------------------------------------------
C    
c------------------------------------------------------------------------------
C   Pour le moment on fait qu'une projection radial
       IFLAG(1) = 2
       MX = MAT(1)       
       NFUNC  = IPM(10,MX)
       DO I= 1, NEL0

        DO J=1,NFUNC
         IFUNC(I,J)=IPM(10+J,MX)
        ENDDO
       ENDDO
C
       MX = MAT(1) 
       IADBUF = IPM(7,MX)-1
       DO I=1,NEL0 
       E(I)   = UPARAM(IADBUF+2)
       A1(I)  = UPARAM(IADBUF+3)
       A2(I)  = UPARAM(IADBUF+4)
       G(I)   = UPARAM(IADBUF+5)
       G3(I)  = THREE*G(I)
       NU(I)  = UPARAM(IADBUF+6)
       VISC(I)   = UPARAM(IADBUF+7)
       VISCV(I) = UPARAM(IADBUF+8)
       NRATEP(I) = UPARAM(IADBUF+9)
       NRATEN(I) = UPARAM(IADBUF+10)
       EPSMAX(I) = UPARAM(IADBUF+11)       
       NRATE(I)  =NRATEN(I) + NRATEP(I) +1 
       
C
        NNU(I) = NU(I) / (ONE - NU(I))       
       ENDDO
       IF (ISIGI==0) THEN
       IF(TIME==ZERO)THEN
         DO I=1,NEL0           
           DO J=1,NRATE(I)
             UVAR(I,J)=0
           ENDDO
         ENDDO
       ENDIF
       ENDIF
C------------------------------------------
C-
C
       DO I=1,NEL0
         SIGNXX(I)=SIGOXX(I) + A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) + A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) + G(I) *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I) + GS(I) *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I) + GS(I) *DEPSZX(I)
C 
         VISC(I)  = VISC(I)*RHO0(I)
         VISCV(I) = VISCV(I)*RHO0(I)
       
         C11 = ONE/MAX(EM20,THREE*VISC(I) + FOUR*VISCV(I))
         C1(I) = FOUR* VISC(I)*(THREE*VISCV(I) 
     .                         + VISC(I))*C11
         C2(I) = TWO*VISCV(I)*(THREE*VISC(I) 
     .                      - TWO*VISCV(I))*C11
C la partie visqueuse 
         DTINV = TIMESTEP(I)**2/MAX(EM20, TIMESTEP(I))
           
         SIGVXX(I)= C2(I)*(EPSPXX(I)+EPSPYY(I))
     .                       + TWO*VISCV(I)*EPSPXX(I) 
         SIGVYY(I)= C2(I)*(EPSPXX(I)+EPSPYY(I))
     .                       + TWO*VISCV(I)*EPSPYY(I)
         SIGVXY(I)= VISCV(I)*EPSPXY(I)
         SIGVYZ(I)= VISCV(I)*EPSPYZ(I)
         SIGVZX(I)= VISCV(I)*EPSPZX(I)
C  
         SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
         C11 = C2(I) + 2*VISCV(I)
C a verefier cette formule	  
          VISCMAX(I) = MAX(C2(I), C11)
	  VISCMAX(I) = VISCMAX(I)/
     .          (ONEP414*RHO0(I)*SOUNDSP(I)*SQRT(AREA(I)))                   
         ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
         IF (ISRATE == 0) EPSP(I) = 
     .     HALF*(EPSPXX(I)+EPSPYY(I)
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
C
         EPST(I) = HALF*(EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )         
         IF(OFF(I)==ONE.AND.EPST(I)>EPSMAX(I))OFF(I) =FOUR_OVER_5
       ENDDO
C-------------------
C     CRITERE
C-------------------
       DO I=1,NEL0
         JJ(I) = 1
         II(I) = 1 + NRATEP(I)
       ENDDO       
C      
       DO I=1,NEL0
        DO J=2,NRATEP(I)-1
           IADBUF = IPM(7,MAT(I)) - 1
           IF(ABS(EPSP(I))>=UPARAM(IADBUF+ 10 +J)) JJ(I) = J 
         ENDDO         
       ENDDO
     
        DO I=1,NEL0
          DO J=2,NRATEN(I)-1
           IADBUF = IPM(7,MAT(I))-1        
            IF(ABS(EPSP(I))>=
     .         ABS(UPARAM(IADBUF+10 + NRATEP(I) + J))) II(I) = NRATEP(I) + J 
           ENDDO
         ENDDO
       DO I=1,NEL0
         IADBUF = IPM(7,MAT(I))-1
         RATEP(I,1)=UPARAM(IADBUF+10+JJ(I))
         RATEP(I,2)=UPARAM(IADBUF+10+JJ(I)+1)
         RATEN(I,1)=UPARAM(IADBUF+10+II(I))
         RATEN(I,2)=UPARAM(IADBUF+10+II(I)+1)             
       ENDDO
C
       DO I=1,NEL0
C + 1 pour la courbe elastique JJ(I) + 1 , II(I) + 1     
           J1 = JJ(I) + 1
           J2 = J1+1
           I1 = II(I) +1
           I2 = I1 +1
           IPOS1P(I) = NINT(UVAR(I,J1))
           IAD1P(I)  = NPF(IFUNC(I,J1)) / 2 + 1
           ILEN1P(I) = NPF(IFUNC(I,J1)+1) / 2 - IAD1P(I) - IPOS1P(I)
           IPOS2P(I) = NINT(UVAR(I,J2))
           IAD2P(I)  = NPF(IFUNC(I,J2)) / 2 + 1
           ILEN2P(I) = NPF(IFUNC(I,J2)+1) / 2 - IAD2P(I) - IPOS2P(I)
C
           IPOS1N(I) = NINT(UVAR(I,I1))
           IAD1N(I)  = NPF(IFUNC(I,I1)) / 2 + 1
           ILEN1N(I) = NPF(IFUNC(I,I1)+1) / 2 - IAD1N(I) - IPOS1N(I)
           IPOS2N(I) = NINT(UVAR(I,I2))
           IAD2N(I)  = NPF(IFUNC(I,I2)) / 2 + 1
           ILEN2N(I) = NPF(IFUNC(I,I2)+1) / 2 - IAD2N(I) - IPOS2N(I)        
c
           IPOS1(I) = NINT(UVAR(I,1))
           IAD1(I)  = NPF(IFUNC(I,1)) / 2 + 1
           ILEN1(I) = NPF(IFUNC(I,1)+1) / 2 - IAD1(I) - IPOS1(I)
       ENDDO
C
         CALL VINTER(TF,IAD1P,IPOS1P,ILEN1P,NEL0,EPST,DYDX1,YP1)
         CALL VINTER(TF,IAD2P,IPOS2P,ILEN2P,NEL0,EPST,DYDX2,YP2)
C
         CALL VINTER(TF,IAD1N,IPOS1N,ILEN1N,NEL0,EPST,DYDX1,YN1)
         CALL VINTER(TF,IAD2N,IPOS2N,ILEN2N,NEL0,EPST,DYDX2,YN2)
C 
         CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL0,EPST,DYDX1,Y1)     
C

Cel remplacer par les sous cas :

        DO I=1,NEL0
         J1 = JJ(I) +1
         J2 = J1+1        
         FAC   = (EPSP(I) - RATEP(I,1))/(RATEP(I,2) - RATEP(I,1))
         YLDMAX(I) = YP1(I)    + FAC*(YP2(I)-YP1(I))
         YLDMAX(I) = MAX(YLDMAX(I),EM20)        
         UVAR(I,J1) = IPOS1P(I)
         UVAR(I,J2) = IPOS2P(I)
C       min stress 
           I1 = II(I) +1
           I2 = I1+1           
           FAC   = (-EPSP(I) - RATEN(I,2))/(RATEN(I,1) - RATEN(I,2))
           YLDMIN(I) = YN2(I)    + FAC*(YN1(I)-YN2(I))
           YLDMIN(I) = MAX(YLDMIN(I),EM20)           
           UVAR(I,I1) = IPOS1N(I)
           UVAR(I,I2) = IPOS2N(I)
C   elastique courbe
           YLDELAS(I)= Y1(I)
           UVAR(I,1) = IPOS1(I)                 
        ENDDO
     

C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1)==2)THEN
C projection radiale 
         DO I=1,NEL0
          IF(OFF(I)==1) THEN 
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          +THREE*SIGNXY(I)*SIGNXY(I))       
C contrainte elastique ..       
           IF(SVM(I)/=ZERO) THEN     
            IF(SVM(I)>=YLDELAS(I))THEN 
              DO ITER = 1, 10              
               R  = MIN(ONE,YLDELAS(I)/MAX(EM20,SVM(I)))
               DEV = (SIGNXX(I) + SIGNYY(I))*THIRD          
               SIGNXX(I)=(SIGNXX(I)-DEV)*R
               SIGNYY(I)=(SIGNYY(I)-DEV)*R
               SIGNXY(I)=SIGNXY(I)*R
               SIGNYZ(I)=SIGNYZ(I)*R
               SIGNZX(I)=SIGNZX(I)*R
CCCACTUALISATION
               SIGNXX(I)=(SIGNXX(I)+ DEV)
               SIGNYY(I)=(SIGNYY(I)+ DEV) 
               SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .                +SIGNYY(I)*SIGNYY(I)
     .                -SIGNXX(I)*SIGNYY(I)
     .             + THREE*SIGNXY(I)*SIGNXY(I))
              ENDDO 
              R = ONE
          DO ITER = 1,10 
           SIGEXX(I)=SIGNXX(I) + SIGVXX(I)
           SIGEYY(I)=SIGNYY(I) + SIGVYY(I)
           SIGEXY(I)=SIGNXY(I) + SIGVXY(I)
           SIGEYZ(I)=SIGNYZ(I) + SIGVYZ(I)
           SIGEZX(I)=SIGNZX(I) + SIGVZX(I)
             SVM(I)=SQRT(SIGEXX(I)*SIGEXX(I)
     .             +SIGEYY(I)*SIGEYY(I)
     .             -SIGEXX(I)*SIGEYY(I)
     .          + THREE*SIGEXY(I)*SIGEXY(I))           
           IF(SVM(I)>=YLDMAX(I)) THEN                
            R  = MIN(ONE,YLDMAX(I)/MAX(EM20,SVM(I)))                        
            ELSEIF(SVM(I)<=YLDMIN(I))THEN                            
            R = MAX(ONE,YLDMIN(I)/MAX(EM20,SVM(I)))            
           ENDIF   
            DEV = (SIGEXX(I) + SIGEYY(I))*THIRD
           SIGEXX(I)=(SIGEXX(I)-DEV)*R
           SIGEYY(I)=(SIGEYY(I)-DEV)*R
           SIGEXY(I)=SIGEXY(I)*R
           SIGEYZ(I)=SIGEYZ(I)*R
           SIGEZX(I)=SIGEZX(I)*R
           SIGEXX(I)=SIGEXX(I)+ DEV 
           SIGEYY(I)=SIGEYY(I)+ DEV 
           SVM(I)=SQRT(SIGEXX(I)*SIGEXX(I)
     .             +SIGEYY(I)*SIGEYY(I)
     .             -SIGEXX(I)*SIGEYY(I)
     .          + THREE*SIGEXY(I)*SIGEXY(I))
C calcul des contraintes visquenses                
            IF(R/=1.) THEN              
             SIGVXX(I)=SIGEXX(I) - SIGNXX(I)
             SIGVYY(I)=SIGEYY(I) - SIGNYY(I)
             SIGVXY(I)=SIGEXY(I) - SIGNXY(I)
             SIGVYZ(I)=SIGEYZ(I) - SIGNYZ(I)
             SIGVZX(I)=SIGEZX(I) - SIGNZX(I)
           ELSE
            GO TO 200
            
           ENDIF  
           ENDDO ! ITER        
         ELSE                       
           ENDIF                      
          ENDIF
          ENDIF
  200    ENDDO
            
C contrainte elastique ..    
            DO I=1,NEL0
            LAMDA = NU(I) * E(I) /((ONE + NU(I))*(ONE - TWO* NU(I)))
            LAMDA = LAMDA + DTINV*THIRD*(THREE*VISC(I) - TWO*VISCV(I))
           C11= LAMDA + TWO*G(I) + TWO*DTINV*VISCV(I)               
           DEZZ = -( LAMDA/MAX(EM20,C11))*(DEPSXX(I) + DEPSYY(I))         
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
         ENDDO
C
       ELSEIF(IFLAG(1)==1)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------

C-------------------------------------------
       ELSEIF(IFLAG(1)==0)THEN
C 
       ENDIF
C
      RETURN
      END
C
