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
Chd|  SIGEPS56C                     source/materials/mat/mat056/sigeps56c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS56C(
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
     C     GS     ,YLD    ,EPSP    ,DPLA_I  ,ISRATE )
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
C -- NEC Commonblock saves the state between IPT calls
      COMMON /NECTRXI/ JST    ,IC    ,IFUNC,MINLEN,NFUNCV,
     .                 NFUNCM,IMIX ,IMIXV ,IFUNCM,
     .                 JEND   
      COMMON /NECTRXR/ NG_NRATES
C 
      INTEGER MINLEN,GLIMIT
      PARAMETER (GLIMIT = 64)
C
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0,ISRATE, IPT,IFLAG(*),
     .   IPM(NPROPMI,*),NGL(NEL0),MAT(NEL0)
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
     .   GS(*)       ,EPSP(NEL0)
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
      INTEGER I,J,J1,J2,N,NINDX,NMAX,IADBUF,NFUNC,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),IFUNC(MVSIZ,100),NRATEM,
     .        NRATE1,IFUNC1(100),NFUNCV(MVSIZ),
     .        NFUNCM,IPERF
      INTEGER JST(MVSIZ+1),IC,MNRATE
      my_real
     .      MX,MX2,MX3,MX4,ME,MA1,MA2,MG,MNU,
     .        MEPSMAX,MEPSR1,MEPSR2,MFISOKIN
      my_real
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ,2),SVM(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR(MVSIZ),
     .        YFAC(MVSIZ,2),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),     
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),SVMO(MVSIZ),H(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ),
     .        HK(MVSIZ)
      my_real
     .        R,UMR,A,B,C,AMU,S11,S22,S12,P,P2,FAC,DEZZ,
     .        SIGZ,S1,S2,S3,DPLA,VM2,EPST,NNU2,
     .        ERR,F,DF,PLA_I,Q2,YLD_I,SIGPXX,SIGPYY,SIGPXY,ALPHA,
     .        E1,A11,A21,G1,G31,NNU11,NU11,NU21,NU31,NU41,NU51,NU61,
     .        EPSMAX1,EPSR11,EPSR21,FISOKIN1,
     .        DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,NUX
C
C -- Added variables for NEC grouping
      my_real
     .     NG_E,NG_A1,NG_A2,NG_G,NG_NU,NG_G3,
     .     NG_ESPMAX,NG_EPSR1,NG_EPSR2,NG_FISOKIN,
     .     NG_NRATES(MVSIZ,100),
     .     NG_RATE1,NG_RATE2,NG_YFAC1,NG_YFAC2,
     .     NG_IPOS1,NG_IPOS2,NG_IAD1,NG_IAD2,NG_ILEN1,NG_ILEN2,
     .     NG_EPS1,NG_EPS2,NG_Y11,NG_Y12,NG_EPSMAX,
     .     Y11(MVSIZ),Y12(MVSIZ)
      INTEGER NG_NRATE, NG_CRVVAL,EPSMAX0LIST(MVSIZ)
      INTEGER
     .     K,L,M,JJS(MVSIZ+1,100+1),JCNT,
     .     ISQRT,ISQRTLIST(MVSIZ),
     .     DIFF(MVSIZ),IMIX,IMIXV(MVSIZ),TEMPIC,JEND(MVSIZ),
     .     IFUNCM(MVSIZ,100),NFUNCMV(MVSIZ),JLAST(MVSIZ+1),
     .     IEPSMAX0,JJINDEX(MVSIZ),JJSTART,JJEND
C -- End NEC grouping
      DATA NMAX/3/,IPERF/0/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
       NFUNC  = IPM(10,MAT(1))
       DO J=1,NFUNC
         IFUNC1(J)=IPM(10+J,MAT(1))
       ENDDO
C
       IADBUF = IPM(7,MAT(1))-1
       E1   = UPARAM(IADBUF+2)
       A11  = UPARAM(IADBUF+3)
       A21  = UPARAM(IADBUF+4)
       G1   = UPARAM(IADBUF+5)
       G31  = THREE*G1
       NUX  = UPARAM(IADBUF+6)
       NRATE1 = NINT(UPARAM(IADBUF+1))
       EPSMAX1=UPARAM(IADBUF+6+2*NRATE1+1)
       IF(EPSMAX1.EQ.ZERO)THEN
         IF(TF(NPF(IFUNC1(1)+1)-1).EQ.ZERO)THEN
           EPSMAX1=TF(NPF(IFUNC1(1)+1)-2)
         ELSE
           EPSMAX1= EP30
         ENDIF
       ENDIF
C
       NNU11 = NUX / (ONE - NUX)
       NNU2    = NNU11*NNU11
       NU11 = ONE/(ONE-NUX)
       NU21 = ONE/(ONE+NUX)
       NU31 = ONE-NNU11
       NU41 = ONE + NNU2 + NNU11
       NU51 = ONE + NNU2 - TWO*NNU11
       NU61 = HALF - NNU2 + HALF*NNU11       
C
       EPSR11 =UPARAM(IADBUF+6+2*NRATE1+2)
       EPSR21 =UPARAM(IADBUF+6+2*NRATE1+3)
       FISOKIN1=UPARAM(IADBUF+6+2*NRATE1+8)
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
           DPLA_I(I) =ZERO           
       ENDDO
C-----------------------------------------------
C
       DO I=1,NEL0
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
         ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
         IF (ISRATE.EQ.0) EPSP(I) = 
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
C
       ENDDO
C-------------------
C     CRITERE
C-------------------
       DO I=1,NEL0
         JJ(I) = 1
       ENDDO
       IADBUF = IPM(7,MAT(1))-1
Cel inversion boucles
       DO J=2,NRATE1-1
         DO I=1,NEL0
           IF(EPSP(I).GE.UPARAM(IADBUF+6+J)) JJ(I) = J
         ENDDO
       ENDDO
       DO I=1,NEL0
         RATE(I,1)=UPARAM(IADBUF+6+JJ(I))
         RATE(I,2)=UPARAM(IADBUF+6+JJ(I)+1)
         YFAC(I,1)=UPARAM(IADBUF+6+NRATE1+JJ(I))
         YFAC(I,2)=UPARAM(IADBUF+6+NRATE1+JJ(I)+1)
       ENDDO
C
       DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         IPOS1(I) = NINT(UVAR(I,J1+4))
         IAD1(I)  = NPF(IFUNC1(J1)) / 2 + 1
         ILEN1(I) = NPF(IFUNC1(J1)+1) / 2 - IAD1(I) - IPOS1(I)
         IPOS2(I) = NINT(UVAR(I,J2+4))
         IAD2(I)  = NPF(IFUNC1(J2)) / 2 + 1
         ILEN2(I) = NPF(IFUNC1(J2)+1) / 2 - IAD2(I) - IPOS2(I)
       ENDDO
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
         FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
         YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
         YLD(I) = MAX(YLD(I),EM20)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
        ENDDO
       ELSEIF (FISOKIN1.EQ.1.) THEN
        DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
C       ECROUISSAGE CINEMATIQUE
         Y1(I)=TF(NPF(IFUNC1(J1))+1)
         Y2(I)=TF(NPF(IFUNC1(J2))+1)
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        ENDDO
       ELSE
        DO I=1,NEL0
         J1 = JJ(I)
         J2 = J1+1
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
         YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
         YLD(I) = MAX(YLD(I),EM20)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
         UVAR(I,J1+4) = IPOS1(I)
         UVAR(I,J2+4) = IPOS2(I)
C       ECROUISSAGE CINEMATIQUE
         Y1(I)=TF(NPF(IFUNC1(J1))+1)
         Y2(I)=TF(NPF(IFUNC1(J2))+1)
         Y1(I)=Y1(I)*YFAC(I,1)
         Y2(I)=Y2(I)*YFAC(I,2)
         YLD(I) = (1.-FISOKIN1) * YLD(I) + 
     .        FISOKIN1 * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
        ENDDO
       ENDIF

C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1).EQ.0)THEN
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
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G31+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
           DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           IF(R.LT.ONE) ETSE(I)= H(I)/(H(I)+E1) 
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
           AA(I)=FOURTH*S1*S1
           BB(I)=THREE_OVER_4*S2*S2+3.*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I))  
           DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I).GT.YLD(I).AND.OFF(I).EQ.ONE) THEN
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
           HK(I) = H(I)*(ONE-FISOKIN1)
          ENDDO
C
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I =YLD(I)+HK(I)*DPLA_I(I)
             DR(I) =HALF*E1*DPLA_I(I)/YLD_I
             PP(I)  =ONE/(ONE+DR(I)*NU11)
             QQ(I)  =ONE/(ONE+THREE*DR(I)*NU21)     
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU11*P2*PP(I)+THREE*BB(I)*NU21*Q2*QQ(I))
     .         *(E1-TWO*DR(I)*HK(I))/YLD_I
     .         -TWO*HK(I)*YLD_I
             DF = SIGN(MAX(ABS(DF),EM20),DF)
             IF(DPLA_I(I).GT.ZERO) THEN
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
           SIGNXX(I)=HALF*(S1+S2)
           SIGNYY(I)=HALF*(S1-S2) 
           SIGNXY(I)=SIGNXY(I)*QQ(I)
           DEZZ = - NU31*DR(I)*S1/E1
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
          ENDDO
         ENDIF
C-------------------------------------------
       ELSEIF(IFLAG(1).EQ.2)THEN
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
           A  = P2*NU41 + VM2
           VM2= THREE*P2  + VM2
           B  = P2*NU61
           C  = P2*NU51 - YLD(I)*YLD(I)
           R  = MIN(ONE,(-B + SQRT(MAX(ZERO,B*B-A*C)))/MAX(A ,EM20))
           SIGNXX(I) = S11*R - P
           SIGNYY(I) = S22*R - P
           SIGNXY(I) = S12*R
C         signzz    = p*r - p
C proj.   signzz    = 0.
           UMR = ONE - R
           SIGZ      = NNU11*P*UMR
           SIGNXX(I) = SIGNXX(I) + SIGZ
           SIGNYY(I) = SIGNYY(I) + SIGZ
           SVM(I)=SQRT(VM2)
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G31+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
           DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           IF(R.LT.ONE) ETSE(I)= H(I)/(H(I)+E1)         
         ENDDO
       ENDIF
C
       DO I=1,NEL0
         IF(PLA(I).GT.EPSMAX1.AND.OFF(I).EQ.ONE)OFF(I)=FOUR_OVER_5
       ENDDO
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
C test bypass
       IF (FISOKIN1.NE.ZERO) THEN
        DO I=1,NEL0
          DSXX = SIGEXX(I) - SIGNXX(I)
          DSYY = SIGEYY(I) - SIGNYY(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DEXX = (DSXX - NUX*DSYY) 
          DEYY = (DSYY - NUX*DSXX)
          DEXY = TWO*(ONE+NUX)*DSXY
          ALPHA = FISOKIN1*H(I)/(E1+H(I))/THREE
          SIGPXX = ALPHA*(FOUR*DEXX+TWO*DEYY)
          SIGPYY = ALPHA*(FOUR*DEYY+TWO*DEXX)
          SIGPXY = ALPHA*DEXY
         SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
         SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
         SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
         UVAR(I,2) = UVAR(I,2) + SIGPXX
         UVAR(I,3) = UVAR(I,3) + SIGPYY
         UVAR(I,4) = UVAR(I,4) + SIGPXY
        ENDDO
       ENDIF
C
      RETURN
      END
C
