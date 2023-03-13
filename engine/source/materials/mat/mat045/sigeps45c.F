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
Chd|  SIGEPS45C                     source/materials/mat/mat045/sigeps45c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS45C(
     1     NEL0    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
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
     B     OFF    ,NGL    ,SHF)
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
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0)
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   AREA(NEL0),RHO0(NEL0),EINT(NEL0,2),
     .   THKLY(NEL0),PLA(NEL0),
     .   EPSPXX(NEL0),EPSPYY(NEL0),
     .   EPSPXY(NEL0),EPSPYZ(NEL0),EPSPZX(NEL0),
     .   DEPSXX(NEL0),DEPSYY(NEL0),
     .   DEPSXY(NEL0),DEPSYZ(NEL0),DEPSZX(NEL0),
     .   EPSXX(NEL0) ,EPSYY(NEL0) ,
     .   EPSXY(NEL0) ,EPSYZ(NEL0) ,EPSZX(NEL0) ,
     .   SIGOXX(NEL0),SIGOYY(NEL0),
     .   SIGOXY(NEL0),SIGOYZ(NEL0),SIGOZX(NEL0), SHF(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
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
      INTEGER I,J,INDEX(MVSIZ),NINDX,NMAX,N
      my_real 
     .        YOUNG,ANU,G,CA,CB,CN,EPSM,SIGM,CC,CD,CM,EPS0,
     .        CE,CK,C1,C14G3,A1,A2,G3,GS,
     .        CH1,CH2,CH3,QH1,QH2,
     .        NNU1,NU1,NU2,NU3,S1,S2,S3,
     .        R,UMR,DEZZ,
     .        P2,Q2,F,DF,YLD_I,
     .        CUTFRE,BETA     
      my_real 
     .        SVM(MVSIZ),AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),
     .        DPLA_I(MVSIZ),DR(MVSIZ),PP(MVSIZ),QQ(MVSIZ)
C
      DATA NMAX/3/
C=======================================================================
C
C     ZHAO CONSTITUTIVE LAW 
C
C=======================================================================
C-----------------------------------------------
C     PARAMETERS READING
C-----------------------------------------------
        YOUNG = UPARAM(1) 
        ANU = UPARAM(2)
        G = UPARAM(3)
        CA = UPARAM(4)
        CB = UPARAM(5)
        CN = UPARAM(6)
        EPSM = UPARAM(7)
        SIGM = UPARAM(8) 
        CC = UPARAM(9) 
        CD = UPARAM(10)
        CM = UPARAM(11)
        EPS0 = UPARAM(12)
        CE = UPARAM(13)
        CK = UPARAM(14)
        C1 = UPARAM(15) 
        C14G3 = UPARAM(16)
        A1 = UPARAM(17) 
        A2 = UPARAM(18) 
        CUTFRE = UPARAM(19) 
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(TIME==ZERO)THEN
        DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
           UVAR(I,4)=ZERO
           UVAR(I,5)=ZERO  
        ENDDO
      ENDIF
C-----------------------------------------------
        BETA = TIMESTEP*2.*PI*CUTFRE
        BETA = MIN(ONE,BETA)
        G3 = THREE * G
        GS = FIVE_OVER_6 * G
        NNU1 = ANU / (ONE - ANU)
        NU1 = ONE/(ONE-ANU)
        NU2 = ONE/(ONE+ANU)
        NU3 = ONE - NNU1	
C        NNU2    = NNU1*NNU1
C        NU4 = 1. + NNU2 + NNU1
C        NU5 = 1. + NNU2 - 2.*NNU1
C        NU6 = 0.5- NNU2 + 0.5*NNU1
C-----------------------------------------------
C     ELASTIC SOLUTION
C--------------------------------------------
       DO I=1,NEL0
C
         SIGNXX(I)=SIGOXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I)+G *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+GS *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+GS *DEPSZX(I)
C
         SOUNDSP(I) = SQRT(A1/RHO0(I))
         VISCMAX(I) = ZERO
C         ETSE(I) = 1.
C-------------------
C     STRAIN RATE
C-------------------
C
         UVAR(I,2) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
         UVAR(I,2) = BETA*UVAR(I,2) + (1.-BETA)*UVAR(I,5)
         UVAR(I,5) = UVAR(I,2)
C-------------------
C     STRAIN 
C-------------------
C
C         UVAR(I,1) = 0.5*( EPSXX(I)+EPSYY(I)
C     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
C     .                 + EPSXY(I)*EPSXY(I) ) )
C         FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2(I)-EPST)/(EPSR2(I)-EPSR1(I))))
C
       ENDDO
C-------------------
C     CRITERIA
C-------------------
       DO I=1,NEL0
        IF(UVAR(I,1)<=ZERO) THEN
         CH1=CA
        ELSEIF(UVAR(I,1)>EPSM) THEN
         CH1=CA+CB*EPSM**CN
        ELSE
         CH1=CA+CB*UVAR(I,1)**CN
        ENDIF
        IF(UVAR(I,2)<=EPS0) THEN
         CH2=0.
        ELSEIF(UVAR(I,1)<=ZERO) THEN
         CH2=CC*LOG(UVAR(I,2)/EPS0)
        ELSE
         CH2=(CC-CD*UVAR(I,1)**CM)*LOG(UVAR(I,2)/EPS0)
        ENDIF
        IF(UVAR(I,2)<=ZERO) THEN
         CH3=ZERO
        ELSE
         CH3=CE*UVAR(I,2)**CK
        ENDIF
c jbm033        UVAR(I,3)=MIN(SIGM,CH1+CH2+CH3)
        UVAR(I,3)=MIN(SIGM+CH3,CH1+CH2+CH3)
       ENDDO
C------------------------
C     HARDENING MODULUS
C------------------------
       DO I=1,NEL0
        IF(UVAR(I,1)>ZERO. AND .CN>=ONE) THEN
         QH1= CB*CN*UVAR(I,1)**(CN-ONE)
        ELSEIF(UVAR(I,1)>ZERO. AND .CN<ONE)THEN
         QH1= CB*CN*UVAR(I,1)**(ONE - CN)
        ELSE
         QH1=ZERO
        ENDIF
        IF(UVAR(I,1)<=ZERO. OR .UVAR(I,2)<=EPS0) THEN
         QH2=ZERO
        ELSEIF(CM>=ONE) THEN
         QH2=CD*CM*UVAR(I,1)**(CM- ONE)*LOG(UVAR(I,2)/EPS0)
        ELSE
         QH2=CD*CM*UVAR(I,1)**(ONE -CM)*LOG(UVAR(I,2)/EPS0)
        ENDIF
        UVAR(I,4)=QH1+QH2
       ENDDO
C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1)==0)THEN
C radial return 
         DO I=1,NEL0
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          +THREE*SIGNXY(I)*SIGNXY(I))
           R  = MIN(ONE,UVAR(I,3)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = ONE - R
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G3+UVAR(I,4))
           UVAR(I,1) = UVAR(I,1) + DPLA_I(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) /UVAR(I,3)
           DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU1-NU3*DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)
C           IF(R<1.) ETSE(I)= H(I)/(H(I)+E(I))
         ENDDO
C
       ELSEIF(IFLAG(1)==1)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
         DO  I=1,NEL0
           UVAR(I,4) = MAX(ZERO,UVAR(I,4))
           S1=SIGNXX(I)+SIGNYY(I)
           S2=SIGNXX(I)-SIGNYY(I)
           S3=SIGNXY(I)
           AA(I)=FOURTH*S1*S1
           BB(I)=THREE_OVER_4*S2*S2+3.*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I))  
           DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
           THK(I) = THK(I) + DEZZ*THKLY(I)
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I)>UVAR(I,3).AND.OFF(I)==ONE) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
         IF(NINDX==0) RETURN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
         DO J=1,NINDX
           I=INDEX(J)
           DPLA_J(I)=(SVM(I)-UVAR(I,3))/(G3+UVAR(I,4))
C           ETSE(I)= H(I)/(H(I)+E(I))
         ENDDO
C
         DO N=1,NMAX
C#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I =UVAR(I,3)+UVAR(I,4)*DPLA_I(I)
             DR(I) =HALF*YOUNG*DPLA_I(I)/YLD_I
             PP(I)  =ONE/(ONE+DR(I)*NU1)
             QQ(I)  =ONE/(ONE + THREE*DR(I)*NU2)	     
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU1*P2*PP(I)+THREE*BB(I)*NU2*Q2*QQ(I))
     .         *(YOUNG-TWO*DR(I)*UVAR(I,4))/YLD_I
     .         -TWO*UVAR(I,4)*YLD_I
             IF(DPLA_I(I)>ZERO) THEN
               DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
             ELSE
               DPLA_J(I)=ZERO
             ENDIF        
           ENDDO
         ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
C#include "vectorize.inc"
         DO J=1,NINDX
           I=INDEX(J)
           UVAR(I,1) = UVAR(I,1) + DPLA_I(I)
           S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
           S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
           SIGNXX(I)=HALF*(S1+S2)
           SIGNYY(I)=HALF*(S1-S2)
           SIGNXY(I)=SIGNXY(I)*QQ(I)
           DEZZ = - NU3*DR(I)*S1/YOUNG
           THK(I) = THK(I) + DEZZ*THKLY(I)
         ENDDO
C
       ENDIF
C
       DO I=1,NEL0
         IF(UVAR(I,1)>EPSM.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
       ENDDO
C-------------------------
      RETURN
      END

