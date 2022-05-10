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
Chd|  SIGEPS86C                     source/materials/mat/mat086/sigeps86c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS86C(
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
     C     GS     ,YLD    ,EPSP    ,ISRATE  ,DPLA_I)
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
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),ISRATE,IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,
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
     .   GS(*),EPSP(NEL0)
      my_real ,DIMENSION(NUPARAM) :: UPARAM
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
     .    UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .        FINTER ,TF(*)
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
      INTEGER I,J,NRATE(MVSIZ),J1,J2,N,NINDX,NMAX,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),NRATEM,
     .        NRATE1
      my_real 
     .        E(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ),SVM(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR(MVSIZ),
     .        YFAC(MVSIZ,2),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),     
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),SVMO(MVSIZ),H(MVSIZ),
     .        EPSMAX(MVSIZ),EPSR1(MVSIZ),EPSR2(MVSIZ),FISOKIN(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ),
     .        HK(MVSIZ),NU(MVSIZ),NNU1(MVSIZ),EPSF(MVSIZ),
     .        SVM2(MVSIZ),YLD2(MVSIZ)
      my_real
     .        R,UMR,A,B,C,AMU,S11,S22,S12,P,P2,FAC,DEZZ,
     .        SIGZ,S1,S2,S3,DPLA,VM2,EPST(MVSIZ),NNU2,     
     .        ERR,F,DF,PLA_I,Q2,YLD_I,SIGPXX,SIGPYY,SIGPXY,
     .        ALPHA,
     .        E1,A11,A21,G1,NNU11,NU11,NU21,NU31,NU41,NU51,NU61,
     .        EPSMAX1,EPSR11,EPSR21,FISOKIN1,G31,
     .        DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,NUX, EPSF1, MEPSF
      my_real
     .        Y11(MVSIZ),Y21(MVSIZ), ME, MA1, MA2, MG, MNU,
     .        MEPSMAX,MEPSR1,MEPSR2,MFISOKIN
      INTEGER JST(MVSIZ+1), IC, MNRATE
C
      DATA NMAX/3/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
       E1   = UPARAM(2)
       A11  = UPARAM(3)
       A21  = UPARAM(4)
       G1   = UPARAM(5)
       G31  = TROIS*G1
       NUX  = UPARAM(6)
       NNU11 = NUX / (1. - NUX)
       NRATE1  = NINT(UPARAM(1))
       EPSMAX1 =UPARAM(7+2*NRATE1)
       EPSR11  =UPARAM(8+2*NRATE1)
       EPSR21  =UPARAM(9+2*NRATE1)
       FISOKIN1=UPARAM(14+2*NRATE1)
       EPSF1= UPARAM(15+2*NRATE1)        
       IF(EPSMAX1.EQ.ZERO)THEN
         IF(TF(NPF(IPM(11,MAT(1))+1)-1).EQ.ZERO)THEN
           EPSMAX1=TF(NPF(IPM(11,MAT(1))+1)-2)
         ELSE
           EPSMAX1= EP30
         ENDIF
       ENDIF
C
       IF (ISIGI.EQ.0) THEN
        IF(TIME.EQ.ZERO)THEN
         DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
           UVAR(I,4)=ZERO  
           DO J=1,NRATE1
             UVAR(I,J+4)=ZERO
           ENDDO
         ENDDO
        ENDIF
       ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
       DO I=1,NEL0
!           SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!           SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!           SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
C
         SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A11*DEPSXX(I)+A21*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A21*DEPSXX(I)+A11*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G1 *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
         SIGEXX(I) = SIGNXX(I)
         SIGEYY(I) = SIGNYY(I)
         SIGEXY(I) = SIGNXY(I)
C
         SOUNDSP(I) = SQRT(A11/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I) = UN
C-------------------
C     STRAIN RATE
C-------------------
C
         IF (ISRATE .EQ. 0) EPSP(I) = 
     .     UNDEMI*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
C
         EPST(I) = UNDEMI*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
         JJ(I) = 1
       ENDDO
C-------------------
C     CRITERE
C-------------------
Cel inversion boucles
       DO J=2,NRATE1-1
         DO I=1,NEL0
           IF(EPSP(I).GE.UPARAM(6+J)) JJ(I) = J
         ENDDO
       ENDDO

       DO I=1,NEL0
         FAC=UPARAM(6+JJ(I))
         RATE(I)=(EPSP(I) - FAC)/(UPARAM(7+JJ(I)) - FAC)
         YFAC(I,1)=UPARAM(6+NRATE1+JJ(I))
         YFAC(I,2)=UPARAM(6+NRATE1+JJ(I)+1)
       ENDDO

       DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         IPOS1(I) = NINT(UVAR(I,J1+4))
         IAD1(I)  = NPF(IPM(10+J1,MAT(1))) / 2 + 1
         ILEN1(I) = NPF(IPM(10+J1,MAT(1))+1) / 2 - IAD1(I)-IPOS1(I)
         IPOS2(I) = NINT(UVAR(I,J2+4))
         IAD2(I)  = NPF(IPM(10+J2,MAT(1))) / 2 + 1
         ILEN2(I) = NPF(IPM(10+J2,MAT(1))+1) / 2 - IAD2(I)-IPOS2(I)
       END DO
C
       CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL0,PLA,DYDX1,Y1)
       CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL0,PLA,DYDX2,Y2)
C
       IF (FISOKIN1.EQ.ZERO) THEN
        DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         FAC   = RATE(I)
         YLD(I) = Y1(I)    + FAC*(Y2(I)-Y1(I))
         YLD(I) = MAX(YLD(I),EM20)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   =DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
        ENDDO
       ELSEIF (FISOKIN1.EQ.1.) THEN
C
C       ECROUISSAGE CINEMATIQUE
C
        DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         FAC   = RATE(I)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   = DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
C       ECROUISSAGE CINEMATIQUE
         Y1(I)=TF(NPF(IPM(10+J1,MAT(1)))+1)
         Y2(I)=TF(NPF(IPM(10+J2,MAT(1)))+1)
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         YLD(I) = Y1(I)    + FAC*(Y2(I)-Y1(I))
        ENDDO

       ELSE
C
C       ECROUISSAGE CINEMATIQUE
C
        DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         FAC   = RATE(I)
         YLD(I) = Y1(I)    + FAC*(Y2(I)-Y1(I))
         YLD(I) = MAX(YLD(I),EM20)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   = DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
C       ECROUISSAGE CINEMATIQUE
         Y1(I)=TF(NPF(IPM(10+J1,MAT(1)))+1)
         Y2(I)=TF(NPF(IPM(10+J2,MAT(1)))+1)
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         YLD(I) = (1.-FISOKIN1) * YLD(I) + 
     .        FISOKIN1 * (Y1(I)    + FAC*(Y2(I)-Y1(I)))
        ENDDO
       ENDIF

C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1).EQ.0)THEN
         NU31 = UN-NNU11
C projection radiale 
         DO I=1,NEL0
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          +TROIS*SIGNXY(I)*SIGNXY(I))
           R  = MIN(UN,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = UN - R
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G31+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=UNDEMI*(SIGNXX(I)+SIGNYY(I))
           DEZZ = DPLA_I(I) * UNDEMI*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
           DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           IF(R.LT.UN) ETSE(I)= H(I)/(H(I)+E1)
         ENDDO
C
       ELSEIF(IFLAG(1).EQ.1)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
         DO I=1,NEL0
           H(I) = MAX(ZERO,H(I))
           S1=SIGNXX(I)+SIGNYY(I)
           S2=SIGNXX(I)-SIGNYY(I)
           S3=SIGNXY(I)
           AA(I)=QUART*S1*S1
           BB(I)=TQUART*S2*S2+3.*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I))  
           DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I).GT.YLD(I).AND.OFF(I).EQ.UN) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C
         IF(NINDX.NE.0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
          DO J=1,NINDX
           I=INDEX(J)
           DPLA_J(I)=(SVM(I)-YLD(I))/(G31+H(I))
           ETSE(I)= H(I)/(H(I)+E1)
           HK(I) = H(I)*(UN-FISOKIN1)
          ENDDO
C
          NU11 = UN/(UN-NUX)
          NU21 = UN/(UN+NUX)
          NU31 = UN-NNU11
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I =YLD(I)+HK(I)*DPLA_I(I)
             DR(I) =UNDEMI*E1*DPLA_I(I)/YLD_I
             PP(I)  =UN/(UN+DR(I)*NU11)
             QQ(I)  =UN/(UN+TROIS*DR(I)*NU21)     
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU11*P2*PP(I)+TROIS*BB(I)*NU21*Q2*QQ(I))
     .         *(E1-DEUX*DR(I)*HK(I))/YLD_I
     .         -DEUX*HK(I)*YLD_I
             DF = SIGN(MAX(ABS(DF),EM20),DF)
             IF(DPLA_I(I).GT.ZERO) THEN
C            IF(N.EQ.NMAX) THEN
C              ERR=ABS(F/(DF*DPLA_I(I)))
C              WRITE(50,'(2I5,F8.5,4E12.3)') N,I,ERR,DPLA_I(I),F
C            ENDIF
               DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
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
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
           S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
           SIGNXX(I)=UNDEMI*(S1+S2)
           SIGNYY(I)=UNDEMI*(S1-S2) 
           SIGNXY(I)=SIGNXY(I)*QQ(I)
           DEZZ = - NU31*DR(I)*S1/E1
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
          ENDDO
         ENDIF
C-------------------------------------------
       ELSEIF(IFLAG(1).EQ.2)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
         DO I=1,NEL0
           H(I) = MAX(ZERO,H(I))
           SVM2(I)=SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .             +TROIS*SIGNXY(I)*SIGNXY(I)
           SVM(I)=SQRT(SVM2(I))  
           DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           YLD2(I)=YLD(I)*YLD(I)
           IF(SVM2(I).GT.YLD2(I).AND.OFF(I).EQ.UN) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C
         IF(NINDX.NE.0) THEN
C-------------
C   PROJ NORMALE AU CRITERE AVEC CALCUL APPROCHE DE LA NORMALE + RETOUR RADIAL
C-------------
          NU31 = UN-NNU11
          DO J=1,NINDX
           I=INDEX(J)
           A=(SVM2(I)-YLD2(I))
     .  /(CINQ*SVM2(I)+TROIS*(-SIGNXX(I)*SIGNYY(I)+SIGNXY(I)*SIGNXY(I)))
           S1=(UN-DEUX*A)*SIGNXX(I)+          A*SIGNYY(I)
           S2=          A*SIGNXX(I)+(UN-DEUX*A)*SIGNYY(I)
           S3=(UN-TROIS*A)*SIGNXY(I)
           SIGNXX(I)=S1
           SIGNYY(I)=S2
           SIGNXY(I)=S3
           DPLA_I(I) = OFF(I)*(SVM(I)-YLD(I))/(G31+H(I))
C
           HK(I) = H(I)*(UN-FISOKIN1)
           YLD(I) =YLD(I)+HK(I)*DPLA_I(I)
          END DO
C
          DO J=1,NINDX
           I=INDEX(J)
           SVM(I)=SQRT( SIGNXX(I)*SIGNXX(I)
     .                 +SIGNYY(I)*SIGNYY(I)
     .                 -SIGNXX(I)*SIGNYY(I)
     .                 +TROIS*SIGNXY(I)*SIGNXY(I))  
           R  = MIN(UN,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           PLA(I) = PLA(I) + DPLA_I(I)
           DEZZ = DPLA_I(I) * UNDEMI*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
           DEZZ = -NU31*DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ETSE(I)= H(I)/(H(I)+E1)
          END DO
         END IF
       ENDIF
C
       DO I=1,NEL0
         IF(OFF(I).EQ.UN)THEN
           IF(PLA(I).GT.EPSMAX1.OR.EPST(I).GT.EPSF1)OFF(I)=QCINQ
         END IF
       ENDDO
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
Cel test bypass
       IF (FISOKIN1.NE.ZERO) THEN
        DO I=1,NEL0
          DSXX = SIGEXX(I) - SIGNXX(I)
          DSYY = SIGEYY(I) - SIGNYY(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DEXX = (DSXX - NUX*DSYY) 
          DEYY = (DSYY - NUX*DSXX)
          DEXY = DEUX*(UN+NUX)*DSXY
          ALPHA = FISOKIN1*H(I)/(E1+H(I))/TROIS
          SIGPXX = ALPHA*(QUATRE*DEXX+DEUX*DEYY)
          SIGPYY = ALPHA*(QUATRE*DEYY+DEUX*DEXX)
          SIGPXY = ALPHA*DEXY
C
          SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
          SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
          SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
          UVAR(I,2) = UVAR(I,2) + SIGPXX
          UVAR(I,3) = UVAR(I,3) + SIGPYY
          UVAR(I,4) = UVAR(I,4) + SIGPXY
        ENDDO
       ENDIF
C
C
      RETURN
      END
C
