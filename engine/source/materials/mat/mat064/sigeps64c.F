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
Chd|  SIGEPS64C                     source/materials/mat/mat064/sigeps64c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS64C(
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
     B     OFF    ,NGL    ,IPM     ,MAT    ,
     C     ETSE   ,GS     ,VOL     ,YLD      ,TEMPEL,
     D     DIE    ,COEF   ,INLOC   ,DPLANL   )
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
     .   NGL(NEL0),MAT(NEL0),IPM(NPROPMI,*),INLOC
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
     .   GS(*),VOL(NEL0),TEMPEL(NEL0),
     .   DIE(NEL0),COEF(NEL0),DPLANL(NEL0)
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
      INTEGER I,J,NRATE(MVSIZ),J1,J2,N,NINDX,
     .        NMAX,INDEX(MVSIZ),
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        NFUNC, IFUNC(2),MX
      my_real
     .        E,A1,A2,G,G3,
     .        AA(MVSIZ),BB(MVSIZ),PP(MVSIZ),QQ(MVSIZ),H(MVSIZ),
     .        EPSMAX(MVSIZ),CC(MVSIZ),DD(MVSIZ),
     .        CN, CM(MVSIZ), K1(MVSIZ),
     .        V0, TEMP(MVSIZ),VMC,FAC,
     .        VOL0,DVM, VM(MVSIZ),NU,
     .        NNU2,NU1,NU2,NU3,NU4,
     .        NU5,NU6,SVM(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ), Y1(MVSIZ),Y2(MVSIZ)
      my_real
     .        DPLA(MVSIZ),UMR, R, CP, EPS0(MVSIZ),DEZZ,S1, S2,
     .        S3, DPLA_J(MVSIZ), YLD_I,DR(MVSIZ), P2,Q2,NNU1,
     .        S11,S22,PLA_I,
     .        S12, VM2, A, B, C, SIGZ, F, DF, P,MD,
     .        CD,YFAC(2),
     .        HL
        DATA NMAX/3/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C
         MX = MAT(1)
         NFUNC  = IPM(10,MX)
         DO J=1,NFUNC
           IFUNC(J)=IPM(10+J,MX)
         ENDDO
         E   = UPARAM(1)
         A1  = UPARAM(2)
         A2  = UPARAM(3)
         G   = UPARAM(4)
         NU  = UPARAM(5)
         CD  = UPARAM(6)
         CN  = UPARAM(7)
         MD  = UPARAM(8)
         V0  = UPARAM(9)
         VMC = UPARAM(10)
         CP  = UPARAM(11)
         YFAC(1) = UPARAM(13)
         YFAC(2) = UPARAM(14)
         G3  = THREE*G
         NNU1 = NU / (ONE - NU)
         NNU2    = NNU1*NNU1
         NU1 = ONE/(ONE - NU)
         NU2 = ONE/(ONE + NU)
         NU3 = ONE -NNU1
         NU4 = ONE + NNU2 + NNU1
         NU5 = ONE + NNU2 - TWO*NNU1
         NU6 = HALF - NNU2 + HALF*NNU1
         DO I=1,NEL0
           TEMP(I)  = UPARAM(12)
C latent heat            
           HL  = UPARAM(15)
           COEF(I)  = ONE
C
         ENDDO
C
       IF (ISIGI==0) THEN
       IF(TIME==0.0)THEN
         DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3) = ZERO
           UVAR(I,4) = ZERO
           UVAR(I,5) = ZERO
           UVAR(I,6) = ZERO
         ENDDO
       ENDIF
       ENDIF
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
        IF(JTHE > 0 ) THEN
         DO I=1,NEL0
           TEMP(I) = TEMPEL(I)
         ENDDO
        ELSE
          DO I=1,NEL0
            VOL0 = VOL(I) * RHO0(I)
            VM(I) = UVAR(I,2)
            TEMP(I) = TEMP(I)
     .                       + COEF(I)*CP*(EINT(I,1)+ EINT(I,2))/VOL0
     .                       + CP*VM(I)*HL/VOL0
          ENDDO
        ENDIF
C
       DO I=1,NEL0
C
         SIGNXX(I)=SIGOXX(I) + A1*DEPSXX(I) + A2*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) + A2*DEPSXX(I) + A1*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) + G *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I) + GS(I)*DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I) + GS(I)*DEPSZX(I)
         VM(I) = UVAR(I,2)
         DPLA(I) = ZERO
C ...     

         SOUNDSP(I) = SQRT(A1/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I) = ONE
C         
       ENDDO
C compute YLD stress
       DO I=1,NEL0
        IPOS1(I) = NINT(UVAR(I,3))
        IAD1(I)  = NPF(IFUNC(1)) / 2 + 1
        ILEN1(I) = NPF(IFUNC(1)+1) / 2 - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,4))
        IAD2(I)  = NPF(IFUNC(2)) / 2 + 1
        ILEN2(I) = NPF(IFUNC(2)+1) / 2 - IAD2(I) - IPOS2(I)
       ENDDO
C
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL0,PLA,DYDX1,Y1)
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL0,PLA,DYDX2,Y2)
C Yld stress 
          DO I=1,NEL0
           Y1(I)=Y1(I)*YFAC(1)
           Y2(I)=Y2(I)*YFAC(2)
           FAC   = VM(I)/VMC
           YLD(I) = Y1(I)    + FAC*(Y2(I)-Y1(I))
           YLD(I) = MAX(YLD(I),EM20)
C ecrouissage 
           DYDX1(I)=DYDX1(I)*YFAC(1)
           DYDX2(I)=DYDX2(I)*YFAC(2)
           H(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
           UVAR(I,3) = IPOS1(I)
           UVAR(I,4) = IPOS2(I)
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
     .          +THREE*SIGNXY(I)*SIGNXY(I))
           R  = MIN(ONE,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = ONE - R
cc           DPLA(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           DPLA(I) = OFF(I)*SVM(I)*UMR/E
           PLA(I) = PLA(I) + DPLA(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           IF (INLOC == 0) THEN
             DEZZ = DPLA(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU1-NU3*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<ONE) ETSE(I)= H(I)/(H(I)+E)
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
           IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
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
cc             PLA_I = PLA(I) + DPLA(I)
             YLD_I = YLD(I) + H(I)*DPLA(I)
C             
             DR(I) =HALF*E*DPLA(I)/YLD_I
             PP(I)  =ONE/(ONE+DR(I)*NU1)
             QQ(I)  =ONE/(ONE + THREE*DR(I)*NU2)
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU1*P2*PP(I)+THREE*BB(I)*NU2*Q2*QQ(I))
     .         *(E-TWO*DR(I)*H(I))/YLD_I
     .         -TWO*H(I)*YLD_I
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
           DPLA(I) = OFF(I)*SVM(I)*UMR/G3
cc           DPLA(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
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
         DVM = ZERO
         IF(TEMP(I)<MD) THEN
          DVM = CN*CD*EXP((CN-ONE)*LOG(MAX(CD*PLA(I),EM20)))
          DVM = DVM*EXP(-(CD*PLA(I))**CN)
          DVM = DVM*V0*LOG(MD - TEMP(I) + ONE)
         ENDIF

         VM(I) = VM(I) + MAX(DVM*DPLA(I),ZERO)
         VM(I) = MIN(VM(I), ONE)
C la chaleur latente dans le terme source.
         IF(JTHE > 0 ) DIE(I) = (VM(I) - UVAR(I,2))*HL
         UVAR(I,2) = VM(I)
         UVAR(I,5) = TEMP(I)
         UVAR(I,1) = PLA(I)
      ENDDO
C
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN
        DO I = 1,NEL0
          SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .           - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
          DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
          DEZZ   = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E) - DEZZ
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDDO
      ENDIF
C
      RETURN
      END
C
