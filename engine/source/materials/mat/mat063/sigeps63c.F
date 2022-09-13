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
Chd|  SIGEPS63C                     source/materials/mat/mat063/sigeps63c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS63C(
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
     B     OFF    ,NGL    ,
     C     ETSE   ,GS     ,VOL     ,YLD    , TEMPEL ,
     D     DIE    ,COEF   ,INLOC   ,DPLANL )
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
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),INLOC
      my_real TIME,TIMESTEP,
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
     .   GS(*),VOL(NEL0) ,TEMPEL(NEL0),
     .   DIE(NEL0),COEF(NEL0),DPLANL(NEL0)
      my_real ,DIMENSION(NUPARAM) :: UPARAM
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
      INTEGER I,J,NRATE(MVSIZ),J1,J2,N,NINDX,NMAX,INDEX(MVSIZ)
      my_real
     .        E,A1,A2,G,G3,
     .        AA(MVSIZ),BB(MVSIZ),PP(MVSIZ),QQ(MVSIZ),H(MVSIZ),
     .        EPSMAX(MVSIZ),CC,DD(MVSIZ),AHS,
     .        BHS, CN, CM, K1,
     .        K2, DH, VM0, TEMP(MVSIZ),
     .        VOL0,DVM, VM(MVSIZ),HK(MVSIZ),NU,
     .        NNU2,NU1,NU2,NU3,NU4, 
     .        NU5,NU6,SVM(MVSIZ)
      my_real
     .        DPLA(MVSIZ),UMR, R, CP, EPS0,DEZZ,S1, S2,
     .        S3, DPLA_J(MVSIZ), YLD_I,DR(MVSIZ), P2,Q2,NNU1,
     .        S11,S22,PLA_I,
     .        S12, VM2, A, B, C, SIGZ, F, DF, P,
     .        CA,CB,CQ,PN, CD,
     .        HL, ARGEXP, AUX0, AUX1, AUX2, AUX3, KT(MVSIZ)
      DATA NMAX/3/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
         E   = UPARAM(1)
         A1  = UPARAM(2)
         A2  = UPARAM(3)
         G   = UPARAM(4)
         NU  = UPARAM(5)
         CA  = UPARAM(6)
         CB  = UPARAM(7)
         CQ  = UPARAM(8)
         CC  = UPARAM(9)
         CD  = UPARAM(10)
         PN  = UPARAM(11)
         AHS = UPARAM(12)
         BHS = UPARAM(13)
         CM  = UPARAM(14)
         CN  = UPARAM(15)
         K1  = UPARAM(16)
         K2  = UPARAM(17)
         DH  = UPARAM(18)
         VM0 = UPARAM(19)
         EPS0= UPARAM(20)
         CP  = UPARAM(21)
         HL  = UPARAM(23)
         G3  = THREE*G
C
         NNU1 = NU / (ONE  - NU)
         NNU2    = NNU1*NNU1
         NU1 = ONE/(ONE-NU)
         NU2 = ONE/(ONE+NU)
         NU3 = ONE-NNU1
         NU4 = ONE + NNU2 + NNU1
         NU5 = ONE + NNU2 - TWO*NNU1
         NU6 = HALF - NNU2 + HALF*NNU1
         DO I=1,NEL0
           TEMP(I) = UPARAM(22)
C latent heat            
           COEF(I)  = UPARAM(24)                     
         ENDDO
C
       IF (ISIGI==0) THEN
       IF(TIME==0.0)THEN
         DO I=1,NEL0
           UVAR(I,1) = ZERO
           UVAR(I,2) = VM0
           UVAR(I,3) = ZERO  
           UVAR(I,4) = ZERO
         ENDDO
       ENDIF
       ENDIF
C-----------------------------------------------
       DO I=1,NEL0
C
         SIGNXX(I)=SIGOXX(I) + A1*DEPSXX(I) + A2*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) + A2*DEPSXX(I) + A1*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) + G *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I) + GS(I)*DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I) + GS(I)*DEPSZX(I)
C         
         SOUNDSP(I) = SQRT(A1/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I) = ONE
       ENDDO
C
        IF(JTHE > 0 ) THEN
         DO I=1,NEL0              
           TEMP(I) = TEMPEL(I)
         ENDDO  
        ELSE
          DO I=1,NEL0
            VOL0 = VOL(I) * RHO0(I)
            VM(I) = UVAR(I,2)      
            TEMP(I) = TEMP(I) 
     .              + (   COEF(I)*(EINT(I,1)+ EINT(I,2))
     .                  + VM(I)*HL ) * CP /VOL0
clm     .                + COEF(I)*CP(I)*(EINT(I,1)+ EINT(I,2))/VOL0 
clm     .                + CP(I)*VM(I)*HL(I)/VOL0
          ENDDO
        ENDIF
C
C compute YLD stress
       DO I=1,NEL0
         VM(I) = UVAR(I,2)  

clm         YLD(I) = ( BHS(I) - (BHS(I) - AHS(I))*
clm     .          EXP(-CM(I) * EXP(CN(I)*LOG(MAX(EM20,PLA(I)+EPS0(I))))))
clm     .          *( K1(I) + K2(I)*TEMP(I) ) + DH(I)*VM(I)        
clm        H(I) = CM(I)*CN(I)*( BHS(I) - AHS(I) )
clm     .          * EXP( (CN(I) - 1)*LOG( MAX(EM20,PLA(I) + EPS0(I))))
clm     .          * EXP(-CM(I)*EXP(CN(I)*LOG(MAX(EM20, PLA(I)+EPS0(I)))))
clm     .          * (K1(I)+K2(I)*TEMP(I))          

        AUX0 = PLA(I)+EPS0
        AUX1 = LOG(MAX(EM20,AUX0))
        AUX2 = EXP( (CN - ONE)*AUX1 )
        AUX3 = (BHS - AHS) * EXP(-CM * AUX0 * AUX2)
        KT(I)= K1 + K2*TEMP(I)

        YLD(I) = ( BHS - AUX3 ) * KT(I)
     .         + DH*VM(I)        
        H(I)   = CM*CN* AUX2 * AUX3 * KT(I)

c       H(I)  = H(I) 
ctmp + DH(I)*UVAR(I,3)
        DPLA(I) =ZERO
       ENDDO      
C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1)==0)THEN
C projection radiale 
         DO I=1,NEL0
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          + THREE*SIGNXY(I)*SIGNXY(I))
           R  = MIN(ONE,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = ONE - R
Ctmp           DPLA(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           DPLA(I) = OFF(I)*SVM(I)*UMR/(E)
           PLA(I) = PLA(I) + DPLA(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           IF (INLOC == 0) THEN
             DEZZ = DPLA(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU1-NU3*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<1.) ETSE(I)= H(I)/(H(I)+E) 
         ENDDO
       ELSEIF(IFLAG(1)==1)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------  
         DO  I=1,NEL0
           H(I) = MAX(ZERO,H(I))
           S1=SIGNXX(I)+SIGNYY(I)
           S2=SIGNXX(I)-SIGNYY(I)
           S3=SIGNXY(I)
           AA(I)=FOURTH*S1*S1
           BB(I)=THREE_OVER_4*S2*S2+THREE*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I)) 
           IF (INLOC == 0) THEN 
             DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)       
           ENDIF
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I)>YLD(I).AND.OFF(I)==1.) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C         
         IF(NINDX/=0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
          DO J=1,NINDX
           I=INDEX(J)
           DPLA_J(I)=(SVM(I)-YLD(I))/(G3+H(I))
           ETSE(I)= H(I)/(H(I)+E) 
          ENDDO
C
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA(I)=DPLA_J(I)
             PLA_I = PLA(I) + DPLA(I)
C Yld 
             YLD_I = ( BHS - (BHS - AHS)*
     .          EXP(-CM*EXP(CN*LOG(MAX(EM20,PLA_I+EPS0))) ))
     .          * KT(I) + DH*VM(I) 
C             
             DR(I) =HALF*E*DPLA(I)/YLD_I
             PP(I)  =ONE/(ONE+DR(I)*NU1)
             QQ(I)  =ONE/(ONE+THREE*DR(I)*NU2)
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU1*P2*PP(I)+THREE*BB(I)*NU2*Q2*QQ(I))
     .         *(E- TWO*DR(I)*H(I))/YLD_I
     .         - TWO*H(I)*YLD_I
             DF = SIGN(MAX(ABS(DF),EM20),DF)  
             IF(DPLA(I)>ZERO) THEN
               DPLA_J(I)=MAX(ZERO,DPLA(I)-F/DF)
             ELSE
               DPLA_J(I)=ZERO
             ENDIF        
           ENDDO
          ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
          DO J=1,NINDX
           I=INDEX(J)
           PLA(I) = PLA(I) + DPLA(I)
           S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
           S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
           SIGNXX(I)=HALF*(S1+S2)
           SIGNYY(I)=HALF*(S1-S2)
           SIGNXY(I)=SIGNXY(I)*QQ(I)
           IF (INLOC == 0) THEN
             DEZZ = - NU3*DR(I)*S1/E
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
          ENDDO
         ENDIF
C------------------------------------------
       ELSEIF(IFLAG(1)==2)THEN
C projection radial sur le deviateur sur un critere reduit
C projection elastique en z => sig33 = 0
C le coef. de reduction du critere est tel que 
C l'on se trouve sur le critere apres les 2 projections   
         DO I=1,NEL0
           P   = -(SIGNXX(I)+SIGNYY(I))*THIRD
           S11 = SIGNXX(I)+P
           S22 = SIGNYY(I)+P
C          s33 = p = -(S11 + S22)
           S12 = SIGNXY(I)
C
           P2 = P*P
           VM2= THREE*(S12*S12 - S11*S22)
           A  = P2*NU4 + VM2
           VM2= THREE*P2  + VM2
           B  = P2*NU6
           C  = P2*NU5 - YLD(I)*YLD(I)
           R  = MIN(ONE,(-B + SQRT(MAX(ZERO,B*B-A*C)))/MAX(A ,EM20))
           SIGNXX(I) = S11*R - P
           SIGNYY(I) = S22*R - P
           SIGNXY(I) = S12*R
C         signzz    = p*r - p
C proj.   signzz    = 0.
           UMR = ONE - R
           SIGZ      = NNU1*P*UMR
           SIGNXX(I) = SIGNXX(I) + SIGZ
           SIGNYY(I) = SIGNYY(I) + SIGZ
           SVM(I)=SQRT(VM2)
           DPLA(I) = OFF(I)*SVM(I)*UMR/(THREE*G)           
ctmp           DPLA(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           PLA(I) = PLA(I) + DPLA(I)
           IF (INLOC == 0) THEN
             DEZZ = DPLA(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU1-NU3*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<ONE) ETSE(I)= H(I)/(H(I)+E)
         ENDDO
       ENDIF
C calcul de l'effet martensite ----
       DO I=1,NEL0
         DVM = HALF*(ONE - TANH(CC + CD*TEMP(I)) )

         ARGEXP=  CQ/MAX(EM20, TEMP(I))
     .          + ((CB+ONE)/CB)
     .                      *LOG( MAX(EM20,ONE - VM(I))/MAX(EM20,VM(I)))
     .          + PN*LOG(MAX(EM20,VM(I)))

         DVM = CB*EXP( ARGEXP )*DVM/MAX(CA,EM20)

         IF(DPLA(I)/=ZERO) UVAR(I,3) =  DVM
         VM(I) = VM(I) + MAX(DVM*DPLA(I),ZERO)
         VM(I) = MIN(VM(I), ONE)
C la chaleur latente dans le terme source.
         IF(JTHE > 0 ) DIE(I) = (VM(I) - UVAR(I,2))*HL
         UVAR(I,2) = VM(I)
         UVAR(I,4) = TEMP(I)
         UVAR(I,1) = PLA(I)
       ENDDO
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
       IF (INLOC > 0) THEN
         DO I = 1,NEL0 
           SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .            - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
           DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
           DEZZ   = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E) - DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)     
         ENDDO  
       ENDIF
C
      RETURN
      END
C
