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
Chd|  SIGEPS48C                     source/materials/mat/mat048/sigeps48c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS48C(
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
     C     GS     ,YLD    ,EPSP    ,DPLA_I  ,ISRATE ,
     D     INLOC  ,DPLANL )
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
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
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
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0,ISRATE, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),IPM(NPROPMI,*),INLOC
      my_real
     . TIME,TIMESTEP,UPARAM(*),
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
     .   GS(*),EPSP(NEL0),DPLANL(NEL0)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0),ETSE(NEL0),
     .    DPLA_I(NEL0)     
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     . UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0)
C-------------------------
C    variables non utilisees (Fonctions utilisateur)
C-------------------------
      INTEGER NPF(*),MFUNC,KFUNC(MFUNC)
      my_real
     . TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NINDX,NMAX,IADBUF,INDEX(MVSIZ)
      my_real  
     .        SVM(MVSIZ),DR(MVSIZ),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),H(MVSIZ),HS(MVSIZ),
     .        EPSM(MVSIZ),
     .        YLO(MVSIZ),EPSGM(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ)
      my_real
     .        R,UMR,NUX,A,B,C,S11,S22,S12,P,P2,DEZZ,SIGZ,S1,S2,S3,
     .        VM2,EPST,NNU2,F,DF,Q2,YLD_I,SIGPXX,E1,A11,A21,G1,G31,
     .        NNU11,NU11,NU21,NU31,NU41,NU51,NU61,SIGPYY,SIGPXY,SIGM1,
     .        EPSM1,EPSR11,EPSR21,YD1,HC1,CN1,C11,C21,CM1,C31,CL1,
     .        FISOKIN1,EPSGM1,EPS01,PA,PB,PC,PDA,PDB,YY,
     .        DSXX,DSYY,DSXY,DEXX,DEYY,DEXY, ALPHA
C
      DATA NMAX/3/
C=======================================================================
       IF(TIME==ZERO)THEN
         DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
         ENDDO
       ENDIF
       IADBUF = IPM(7,MAT(1))-1
       E1   = UPARAM(IADBUF+1)
       NUX  = UPARAM(IADBUF+2)
       SIGM1= UPARAM(IADBUF+4)
       EPSM1= UPARAM(IADBUF+5)
       EPSR11=UPARAM(IADBUF+6)
       EPSR21=UPARAM(IADBUF+7)
       YD1  = UPARAM(IADBUF+3)
       HC1  = UPARAM(IADBUF+8)
       CN1  = UPARAM(IADBUF+9)
       C11  = UPARAM(IADBUF+10)
       C21  = UPARAM(IADBUF+11)
       CM1  = UPARAM(IADBUF+12)
       FISOKIN1=UPARAM(IADBUF+15)
       G1   = UPARAM(IADBUF+16)
       G31  = UPARAM(IADBUF+18)
       A11  = UPARAM(IADBUF+20)
       A21  = UPARAM(IADBUF+21)
c       EPSGM1=UPARAM(IADBUF+22)
       EPS01= UPARAM(IADBUF+23)
       C31  = UPARAM(IADBUF+24)
       CL1  = UPARAM(IADBUF+25)
C       
       NNU11 = NUX / (ONE - NUX)
       NNU2    = NNU11*NNU11
       NU11 = ONE/(ONE-NUX)
       NU21 = ONE/(ONE+NUX)
       NU31 = ONE-NNU11
       NU41 = ONE + NNU2 + NNU11
       NU51 = ONE + NNU2 - TWO*NNU11
       NU61 = HALF- NNU2 + HALF*NNU11

C
C------------------------------------------
C     KINEMATIC HARDENING
C------------------------------------------
       DO I=1,NEL0
!           SIGOXX(I) = SIGOXX(I) - UVAR(I,1)
!           SIGOYY(I) = SIGOYY(I) - UVAR(I,2)
!           SIGOXY(I) = SIGOXY(I) - UVAR(I,3)
       ENDDO
C
C---  ELASTIC STRESS ESTIMATE  
C
       DO I=1,NEL0
C
         SIGNXX(I)=SIGOXX(I) - UVAR(I,1) +A11*DEPSXX(I)+A21*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) - UVAR(I,2) +A21*DEPSXX(I)+A11*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) - UVAR(I,3) +G1 *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
         SIGEXX(I) = SIGNXX(I)
         SIGEYY(I) = SIGNYY(I)
         SIGEXY(I) = SIGNXY(I)
         SOUNDSP(I) = SQRT(A11/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
C
         IF (ISRATE == 0) EPSP(I) = 
     .     HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
         EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
         FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR21-EPST)/(EPSR21-EPSR11)))
C----
       ENDDO
C-------------------
C     CURRENT YIELD AND HARDENING
C-------------------
C
       DO I=1,NEL0
        IF(PLA(I)<=ZERO) THEN
         PA=YD1
        ELSE
         PA=YD1+HC1*PLA(I)**CN1
        ENDIF
        IF(EPSP(I)<=EPS01) THEN
         PB=0.
        ELSEIF(PLA(I)<=ZERO) THEN
         PB=C11*LOG(EPSP(I)/EPS01)
        ELSE
         PB=(C11-C21*PLA(I)**CM1)*LOG(EPSP(I)/EPS01)
        ENDIF
        IF(EPSP(I)<=ZERO) THEN
         PC=ZERO
        ELSE
         PC=C31*EPSP(I)**CL1
        ENDIF
C-----
        IF(PLA(I)>ZERO. AND .CN1>=ONE) THEN
         PDA = HC1*CN1*PLA(I)**(CN1-ONE)
        ELSEIF(PLA(I)>ZERO. AND .CN1<ONE)THEN
         PDA = HC1*CN1*PLA(I)**(ONE -CN1)
        ELSE
         PDA = E1
        ENDIF
        IF(PLA(I)<=0. OR .EPSP(I)<=EPS01) THEN
        PDB = ZERO
        ELSEIF(CM1>=ONE) THEN
         PDB = C21*CM1*PLA(I)**(CM1 - ONE)*LOG(EPSP(I)/EPS01)
        ELSE
         PDB = C21*CM1*PLA(I)**(ONE-CM1)*LOG(EPSP(I)/EPS01)
        ENDIF
C
c         IF (ICC1/=1.AND.CN1/=ZERO)
c     &      EPSGM1=((SIGM1/RQ-YD1)/HC1)**(1./CN1)
c         IF (PLA(I)>=EPSGM1) THEN
c            YLD(I) = SIGM1
c            HS(I) = ZERO
c         ENDIF
C----
        YLO(I)= YD1 + PC
        YY    = PA + PB + PC
        HS(I) = PDA + PDB
        YLD(I)= MIN(SIGM1+PC, YY)
        IF (YLD(I)<YY) HS(I)  = ZERO
        HS(I)  = FAIL(I)*HS(I)
        YLD(I) = FAIL(I)*YLD(I)
      ENDDO
C       
C---  KINEMATIC HARDENING
C
      IF (FISOKIN1==ONE) THEN
        DO I=1,NEL0
          YLD(I) = FAIL(I)*YLO(I)
        ENDDO
      ELSEIF (FISOKIN1>ZERO) THEN
        DO I=1,NEL0
          YLD(I) = (ONE-FISOKIN1)*YLD(I) + FISOKIN1*FAIL(I)*YLO(I)
        ENDDO        
      ENDIF
C
C-------------------------
C      PROJECTION IFLAG = 0  (projection radiale)
C-------------------------
C
      IF(IFLAG(1)==0) THEN
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
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G31+HS(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           IF (INLOC == 0) THEN 
             DEZZ = DPLA_I(I) * S1 / YLD(I)
             DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           ENDDO
C
C-------------------------
C      PROJECTION IFLAG = 1 
C-------------------------
C
       ELSEIF(IFLAG(1)==1)THEN
C
C---     CRITERE DE VON MISES
C
         DO I=1,NEL0
           S1=SIGNXX(I)+SIGNYY(I)
           S2=SIGNXX(I)-SIGNYY(I)
           S3=SIGNXY(I)
           AA(I)=FOURTH*S1*S1
           BB(I)=THREE_OVER_4*S2*S2+3.*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I)) 
           IF (INLOC == 0) THEN 
             DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
         ENDDO
C
C---     GATHER PLASTIC FLOW
C
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C
C---    DEP EN CONTRAINTE PLANE
C
         IF(NINDX/=0) THEN
          DO J=1,NINDX
           I=INDEX(J)
           HS(I)  = MAX(ZERO,HS(I))
           ETSE(I)= HS(I)/(HS(I)+E1)
           H(I)   = (ONE-FISOKIN1)*HS(I)
           DPLA_J(I)=(SVM(I)-YLD(I))/(G31+HS(I))
          ENDDO
C
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I =INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I = YLD(I)+H(I)*DPLA_I(I)
             DR(I) = HALF*E1*DPLA_I(I)/YLD_I
             PP(I) = ONE/(ONE+DR(I)*NU11)
             QQ(I) = ONE/(ONE + THREE*DR(I)*NU21)
             P2    = PP(I)*PP(I)
             Q2    = QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU11*P2*PP(I)+THREE*BB(I)*NU21*Q2*QQ(I))
     .         *(E1-TWO*DR(I)*H(I))/YLD_I
     .         -TWO*H(I)*YLD_I
             IF(DPLA_I(I)>ZERO) THEN
               DPLA_J(I) = MAX(ZERO,DPLA_I(I)-F/DF)
             ELSE
               DPLA_J(I) = ZERO
             ENDIF        
           ENDDO
          ENDDO
C
C---     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C
#include "vectorize.inc"
          DO J=1,NINDX
           I=INDEX(J)
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
           S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
           SIGNXX(I)=HALF*(S1+S2)
           SIGNYY(I)=HALF*(S1-S2)
           SIGNXY(I)=SIGNXY(I)*QQ(I)
           IF (INLOC == 0) THEN 
             DEZZ   =-NU31*DR(I)*S1/E1
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
          ENDDO
         ENDIF
C
C-------------------------
C      PROJECTION IFLAG = 2
C-------------------------
C projection radial sur le deviateur sur un critere reduit
C projection elastique en z => sig33 = 0
C le coef. de reduction du critere est tel que 
C l'on se trouve sur le critere apres les 2 projections
C
       ELSEIF(IFLAG(1)==2)THEN
C
         DO I=1,NEL0
           P   = -(SIGNXX(I)+SIGNYY(I))*THIRD
           S11 = SIGNXX(I)+P
           S22 = SIGNYY(I)+P
C          s33 = p = -(S11 + S22)
           S12 = SIGNXY(I)
C
           P2 = P*P
           VM2= THREE*(S12*S12 - S11*S22)
           A  = P2*NU41 + VM2
           VM2= 3.*P2  + VM2
           B  = P2*NU61
           C  = P2*NU51 - YLD(I)*YLD(I)
           R  = MIN(ONE,(-B + SQRT(MAX(ZERO,B*B-A*C)))/MAX(A ,EM20))
           SIGNXX(I) = S11*R - P
           SIGNYY(I) = S22*R - P
           SIGNXY(I) = S12*R
C         signzz    = p*r - p
C proj.   signzz    = 0.
           UMR = ONE-R
           SIGZ      = NNU11*P*UMR
           SIGNXX(I) = SIGNXX(I) + SIGZ
           SIGNYY(I) = SIGNYY(I) + SIGZ
           SVM(I)=SQRT(VM2)
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G31+HS(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           IF (INLOC == 0) THEN
             DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<ONE) ETSE(I)= HS(I)/(HS(I)+E1)
         ENDDO
       ENDIF
C
       DO I=1,NEL0
         IF(PLA(I)>EPSM1.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
       ENDDO
C
C---     KINEMATIC HARDENING
C
       IF (FISOKIN1/=ZERO) THEN
        DO I=1,NEL0
          DSXX = SIGEXX(I) - SIGNXX(I)
          DSYY = SIGEYY(I) - SIGNYY(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DEXX = (DSXX - NUX*DSYY) 
          DEYY = (DSYY - NUX*DSXX)
          DEXY = TWO*(ONE+NUX)*DSXY
         ALPHA = FISOKIN1*HS(I)/(E1+HS(I))/THREE
          SIGPXX = ALPHA*(FOUR*DEXX+TWO*DEYY)
          SIGPYY = ALPHA*(FOUR*DEYY+TWO*DEXX)
          SIGPXY = ALPHA*DEXY
C
          SIGNXX(I) = SIGNXX(I) + UVAR(I,1)
          SIGNYY(I) = SIGNYY(I) + UVAR(I,2)
          SIGNXY(I) = SIGNXY(I) + UVAR(I,3)
          UVAR(I,1) = UVAR(I,1) + SIGPXX
          UVAR(I,2) = UVAR(I,2) + SIGPYY
          UVAR(I,3) = UVAR(I,3) + SIGPXY
        ENDDO
       ENDIF
C
C---     NON-LOCAL THICKNESS VARIATION
C
       IF (INLOC > 0) THEN 
         DO I = 1,NEL0 
           SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I)
     .              + SIGNYY(I)*SIGNYY(I)
     .              - SIGNXX(I)*SIGNYY(I)
     .              + THREE*SIGNXY(I)*SIGNXY(I))
           DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
           DEZZ   = -NUX*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E1) - DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)     
         ENDDO  
       ENDIF 
C
      RETURN
      END
