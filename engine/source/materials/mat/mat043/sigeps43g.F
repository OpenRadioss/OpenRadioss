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
Chd|  SIGEPS43G                     source/materials/mat/mat043/sigeps43g.F
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS43G(
     1     NEL    ,NUVAR  ,NPF     ,
     2     TF     ,TIME   ,UPARAM  ,RHO0    ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,SHF     ,SEQ_OUTPUT,EPSP )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL, NUVAR, NGL(NEL), MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,UPARAM(*),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THK0(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),GS(NEL),SHF(NEL),SEQ_OUTPUT(NEL)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    SIGNYZ(NEL),SIGNZX(NEL),EPSP(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A R G U M E N T S 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),THK(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,K,J1,J2,N,NINDX,NMAX,IADBUF,NFUNC,NS,
     .        NRATE,IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),OPTE,
     .        JJ(MVSIZ),INDEX(MVSIZ),IFUNC(100), IFUNCE,MX,ISRATE
      my_real
     .        E(MVSIZ),NU,A,B,C,FAC,DEZZ,S1,S2,S12,S3,
     .        DPLA,EPST,A1(MVSIZ),A2(MVSIZ),G(MVSIZ),G3(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ,2),SVM(MVSIZ),
     .        YLD(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),DR,F1,F2,
     .        YFAC(MVSIZ,2),NNU1,NU1(MVSIZ),
     .        NU2(MVSIZ),NU3(MVSIZ),NU4(MVSIZ),NU5(MVSIZ),DPLA_I,
     .        FAIL(MVSIZ),EPSMAX,EPSR1,EPSR2,
     .        ERR,F,DF,YLD_I,DPLA_J(MVSIZ),
     .        AA1,AA2,AA3,FN(3),FM(3),FNM(3),SN1,SN2,SM1,SM2,
     .        PN(3),P_M(3),DFN(3),DFM(3),DFNM(3),DJAC(3),JAC(3),C1,
     .        XP(3),PNM1(3),PNM2(3),JQ(MVSIZ),JQ2(MVSIZ),S(MVSIZ),
     .        GM(MVSIZ),CM(MVSIZ),QTIER(MVSIZ),CNM(MVSIZ),
     .        NUM1(MVSIZ),NUM2(MVSIZ),AN(MVSIZ),BN(MVSIZ),
     .        AM1(MVSIZ),AM2(MVSIZ),GAMA(MVSIZ),GAMA2(MVSIZ),
     .        H(MVSIZ),EINF,CE,DYDXE(MVSIZ),
     .        ESCALE(MVSIZ)
      my_real
     .        A_1,A_2,A_3,AMN_XY(MVSIZ),JAC_I(3),JAC_2(3),
     .        A01,A02,A03,A12,
     .        ANXX(MVSIZ),ANYY(MVSIZ),ANXY(MVSIZ),AN_XY(MVSIZ),
     .        AMXX(MVSIZ),AMYY(MVSIZ),AMXY(MVSIZ),AM_XY(MVSIZ),
     .        ANMXX(MVSIZ),ANMYY(MVSIZ),ANMXY(MVSIZ),ANM_XY(MVSIZ),
     .        SN11(MVSIZ),SN22(MVSIZ),SM11(MVSIZ),SM22(MVSIZ),
     .        B_1(MVSIZ),B_2(MVSIZ),B_3(MVSIZ),Q12(MVSIZ),Q21(MVSIZ),
     .        SFN,SFM,SFNM,DSFN,DSFM,DSFNM,TOL
C
      DATA NMAX/3/,NS/10/
         TOL =EM6
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
       MX = MAT(1)
       NFUNC  = IPM(10,MX)
       DO J=1,NFUNC
          IFUNC(J) = IPM(10+J,MX)
       ENDDO
       IADBUF = IPM(7,MX)-1
       ISRATE = IPM(3,MX)
       NU  = UPARAM(IADBUF+6)
       A01  = UPARAM(IADBUF+7)
       A02  = UPARAM(IADBUF+8)
       A03  = UPARAM(IADBUF+9)
       A12  = UPARAM(IADBUF+10)
       NRATE = NINT(UPARAM(IADBUF+1))
       EPSMAX=UPARAM(IADBUF+NS+2*NRATE+1)
C
       IF(EPSMAX==ZERO)THEN
         IF(TF(NPF(IFUNC(1)+1)-1)==ZERO)THEN
          EPSMAX=TF(NPF(IFUNC(1)+1)-2)
       ELSE
          EPSMAX= EP30
         ENDIF
       ENDIF
       NNU1 = NU / (ONE - NU)
       EPSR1 =UPARAM(IADBUF+NS+2*NRATE+2)
       EPSR2 =UPARAM(IADBUF+NS+2*NRATE+3)
c---------------------
       OPTE = UPARAM(IADBUF+NS+2*NRATE + 10)
       EINF = UPARAM(IADBUF+NS+2*NRATE+ 11)
       CE = UPARAM(IADBUF+NS+2*NRATE+ 12)
c--------------------
       DO I=1,NEL
        E(I)   = UPARAM(IADBUF+2)
        A1(I)  = UPARAM(IADBUF+3)
        A2(I)  = UPARAM(IADBUF+4)
        G(I)   = UPARAM(IADBUF+5)
        G3(I)  = THREE*G(I)
C  011        C1=THK(I)*DOUZ
        C1=THK0(I)*ONE_OVER_12
        AM1(I)  = A1(I)*C1
        AM2(I)  = A2(I)*C1
        GM(I)   = G(I)*C1
       ENDDO
C
       IF (OPTE == 1)THEN
         IFUNCE = UPARAM(IADBUF+NS+2*NRATE+ 9)
         DO I=1,NEL
           IF(PLA(I) > ZERO)THEN
              ESCALE(I) = FINTER(IFUNC(IFUNCE),PLA(I),NPF,TF,DYDXE(I))
              E(I) =  ESCALE(I)* E(I)
              G(I) =  HALF*E(I)/(ONE+NU)
              GS(I) =   G(I)*SHF(I)
              G3(I) = THREE*G(I)
              A1(I) = E(I)/(ONE - NU*NU)
              A2(I) = NU*A1(I)
              AM1(I)  = A1(I)*C1
              AM2(I)  = A2(I)*C1
              GM(I)   = G(I)*C1
           ENDIF
         ENDDO
       ELSEIF ( CE /= ZERO) THEN
          DO I=1,NEL
           IF(PLA(I) > ZERO)THEN
              E(I) = E(I)-(E(I)-EINF)*(ONE-EXP(-CE*PLA(I)))
              G(I) =  HALF*E(I)/(ONE+NU)
              GS(I) =   G(I)*SHF(I)
              G3(I) = THREE*G(I)
              A1(I) = E(I)/(ONE - NU*NU)
              A2(I) = NU*A1(I)
              AM1(I)  = A1(I)*C1
              AM2(I)  = A2(I)*C1
              GM(I)   = G(I)*C1
           ENDIF
         ENDDO
       ENDIF
C
      IF (ISIGI==0) THEN
      IF(TIME==0.0)THEN
        DO I=1,NEL
         UVAR(I,1)=ZERO
         UVAR(I,2)=ZERO
         DO J=1,NRATE
           UVAR(I,J+2)=ZERO
         ENDDO
        ENDDO
      ENDIF
      ENDIF
C
      DO I=1,NEL
       SIGNXX(I)=SIGOXX(I)+A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
       SIGNYY(I)=SIGOYY(I)+A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
       SIGNXY(I)=SIGOXY(I)+G(I) *DEPSXY(I)
       MOMNXX(I)=MOMOXX(I)+AM1(I)*DEPBXX(I)+AM2(I)*DEPBYY(I)
       MOMNYY(I)=MOMOYY(I)+AM2(I)*DEPBXX(I)+AM1(I)*DEPBYY(I)
       MOMNXY(I)=MOMOXY(I)+GM(I) *DEPBXY(I)
       SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
       SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
C
       SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
       VISCMAX(I) = ZERO
       ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
       IF (ISRATE == 0) THEN
         EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .    + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                  + EPSPXY(I)*EPSPXY(I) ) )
       ENDIF
C-------------------
C     STRAIN 
C-------------------
        EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
        FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST)/(EPSR2-EPSR1)))
C
      ENDDO
C-------------------
C     HARDENING LAW
C-------------------
      DO I=1,NEL
            JJ(I) = 1
      ENDDO
      DO I=1,NEL
        IADBUF = IPM(7,MAT(I))-1
        DO J=2,NRATE-1
          IF(EPSP(I)>=UPARAM(IADBUF+NS+J)) JJ(I) = J
        ENDDO
        RATE(I,1)=UPARAM(IADBUF+NS+JJ(I))
        RATE(I,2)=UPARAM(IADBUF+NS+JJ(I)+1)
        YFAC(I,1)=UPARAM(IADBUF+NS+NRATE+JJ(I))
        YFAC(I,2)=UPARAM(IADBUF+NS+NRATE+JJ(I)+1)
      ENDDO
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        IPOS1(I) = NINT(UVAR(I,J1))
        IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
        ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,J2))
        IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
        ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
      ENDDO
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        Y1(I)=Y1(I)*YFAC(I,1)
        Y2(I)=Y2(I)*YFAC(I,2)
        FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
        YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        YLD(I) = MAX(YLD(I),EM20)
        SIGY(I) = YLD(I)
        H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
        UVAR(I,J1) = IPOS1(I)
        UVAR(I,J2) = IPOS2(I)
      ENDDO
C-------------------------
C     HILL CRITERION
C-------------------------
      DO  I=1,NEL
       C1=PLA(I)*E(I)
       GAMA(I)=THREE_HALF*(C1+YLD(I))/(THREE_HALF*C1+YLD(I))
       GAMA2(I)=GAMA(I)*GAMA(I)
       CM(I)= SIXTEEN*GAMA2(I)
        CNM(I)= SQR16_3*GAMA(I)
       QTIER(I)=FOUR_OVER_3*GAMA2(I)
       H(I) = MAX(ZERO,H(I))
C
       S1=A01*(SIGNXX(I)*SIGNXX(I)+CM(I)*MOMNXX(I)*MOMNXX(I))
       S2=A02*(SIGNYY(I)*SIGNYY(I)+CM(I)*MOMNYY(I)*MOMNYY(I))
       S3=A03*(SIGNYY(I)*SIGNXX(I)+CM(I)*MOMNXX(I)*MOMNYY(I))
       ANXY(I)=A12*SIGNXY(I)*SIGNXY(I)
       AMXY(I)=A12*MOMNXY(I)*MOMNXY(I)*CM(I)
       F1=S1+S2-S3+ANXY(I)+AMXY(I)
       S1=A01*(SIGNXX(I)*MOMNXX(I))
       S2=A02*(SIGNYY(I)*MOMNYY(I))
       S3=A03*(SIGNXX(I)*MOMNYY(I)+SIGNYY(I)*MOMNXX(I))*HALF
       ANMXY(I)=A12*SIGNXY(I)*MOMNXY(I)
       F2=CNM(I)*(S1+S2-S3+ANMXY(I))
       ANMXY(I)=ANMXY(I)*CNM(I)
       SVM(I)=SQRT(F1+ABS(F2))
!!       SEQ_OUTPUT(I) = SVM(I)
       DEZZ =-(DEPSXX(I)+DEPSYY(I))*NNU1
       THK(I) = THK(I)+THK(I)*DEZZ*OFF(I)
      ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
      NINDX=0
      DO I=1,NEL
      IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
        NINDX=NINDX+1
        INDEX(NINDX)=I
      ENDIF
      ENDDO
      IF(NINDX==0) RETURN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
      DO  J=1,NINDX
       I=INDEX(J)
       NU2(I) = 1.-NU*NU
       NU3(I) = NU*HALF
       NU4(I) = HALF -NU3(I)
       NU5(I) = ONE - NNU1
       DPLA_J(I)=(SVM(I)-YLD(I))/(G3(I)*QTIER(I)+H(I))
       ETSE(I)= H(I)/(H(I)+E(I))
       S1=A01*NU*TWO - A03
       S2=A02*NU*TWO - A03
       S12=A03-NU*(A01+A02)
       S3=SQRT(NU2(I)*(A01-A02)*(A01-A02)+S12*S12)
       IF (ABS(S1)<EM20) THEN
        Q12(I)=ZERO
       ELSE
        Q12(I)=-(A01-A02+S3)/S1
       ENDIF
       IF (ABS(S2)<EM20) THEN
        Q21(I)=ZERO
       ELSE
        Q21(I)=(A01-A02+S3)/S2
       ENDIF
       JQ(I)=ONE/(ONE - Q12(I)*Q21(I))
       JQ2(I)=JQ(I)*JQ(I)
       A=A01*Q12(I)
       B=A02*Q21(I)
       A_1=(A01+A03*Q21(I)+B*Q21(I))*JQ2(I)
       A_2=(A02+A03*Q12(I)+A*Q12(I))*JQ2(I)
       A_3=(A+B)*JQ2(I)*2.0+A03*(JQ2(I)*2.0-JQ(I))
       SN11(I)=SIGNXX(I)+SIGNYY(I)*Q12(I)
       SN22(I)=Q21(I)*SIGNXX(I)+SIGNYY(I)
       SM11(I)=MOMNXX(I)+MOMNYY(I)*Q12(I)
       SM22(I)=Q21(I)*MOMNXX(I)+MOMNYY(I)
       ANXX(I)=A_1*SN11(I)*SN11(I)
       ANYY(I)=A_2*SN22(I)*SN22(I)
       AN_XY(I)=A_3*SN11(I)*SN22(I)
       AMXX(I)=A_1*SM11(I)*SM11(I)*CM(I)
       AMYY(I)=A_2*SM22(I)*SM22(I)*CM(I)
       AM_XY(I)=A_3*SM11(I)*SM22(I)*CM(I)
       ANMXX(I)=A_1*SN11(I)*SM11(I)*CNM(I)
       ANMYY(I)=A_2*SN22(I)*SM22(I)*CNM(I)
       ANM_XY(I)=A_3*SN11(I)*SM22(I)*CNM(I)*HALF
       AMN_XY(I)=A_3*SM11(I)*SN22(I)*CNM(I)*HALF
       A=A03*NU3(I)
       B=S3*JQ(I)
       B_1(I)=A02-A-B
       B_2(I)=A01-A+B
       B_3(I)=A12*NU4(I)
      ENDDO
C-------------------------------
C     TIENT COMPTE DU COUPLAGE
C-------------------------------
       DO N=1,NMAX
       DO J=1,NINDX
        I=INDEX(J)
        DPLA_I=DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_I
        DR =A1(I)*DPLA_I/YLD_I
        XP(1)  =B_1(I)*DR
        XP(2)  =B_2(I)*DR
        XP(3)  =B_3(I)*DR
        C1=ONE + QTIER(I)
        B=TWOP444*GAMA2(I)
        DO K=1,3
         DJAC(K)=C1+B*XP(K)
         JAC(K)=ONE+(DJAC(K)+C1)*XP(K)*HALF
         JAC_I(K)=ONE/JAC(K)
         JAC_2(K)= JAC_I(K)*JAC_I(K)
         A=XP(K)*ZEP444*GAMA2(I)
         DFN(K)=FIVEP5+SIXTEENP5*A
         FN(K)=ONE+(DFN(K)+FIVEP5)*A*HALF
         A=ONEP8333*XP(K)
         DFM(K)=ONEP8333+A
         FM(K)=ONE +(DFM(K)*XP(K)+A)*HALF
         DFNM(K)=-B*XP(K)
         FNM(K)=ONE+DFNM(K)*XP(K)*HALF
        ENDDO
        A=JAC_2(1)*FN(1)
        B=JAC_2(2)*FN(2)
        C=JAC_2(3)*FN(3)
        SFN=A*(ANXX(I)-AN_XY(I))+B*(ANYY(I)-AN_XY(I))+C*ANXY(I)
        A=JAC_2(1)*FM(1)
        B=JAC_2(2)*FM(2)
        C=JAC_2(3)*FM(3)
        SFM=A*(AMXX(I)-AM_XY(I))+B*(AMYY(I)-AM_XY(I))+C*AMXY(I)
        A=JAC_2(1)*FNM(1)
        B=JAC_2(2)*FNM(2)
        C=JAC_2(3)*FNM(3)
        SFNM=A*(ANMXX(I)-ANM_XY(I))+B*(ANMYY(I)-AMN_XY(I))
     .      +C*ANMXY(I)
        C1=ABS(SFNM)/MAX(SFN,SFM)
        IF (C1<TOL) THEN
              S(I)=ZERO
        ELSEIF (SFNM<ZERO) THEN
         S(I)=-ONE
        ELSE
         S(I)=ONE
        ENDIF
        F    =SFN+SFM+S(I)*SFNM-YLD_I*YLD_I
        C1=ZEP444*GAMA2(I)
        S12=JAC_2(1)*B_1(I)
        S1=S12*C1
        AA1=TWO*JAC_I(1)*DJAC(1)*S12
        S12=JAC_2(2)*B_2(I)
        S2=S12*C1
        AA2=TWO*JAC_I(2)*DJAC(2)*S12
        S12=JAC_2(3)*B_3(I)
        S3=S12*C1
        AA3=TWO*JAC_I(3)*DJAC(3)*S12
        A=S1*DFN(1)-AA1*FN(1)
        B=S2*DFN(2)-AA2*FN(2)
        C=S3*DFN(3)-AA3*FN(3)
        DSFN=A*(ANXX(I)-AN_XY(I))+B*(ANYY(I)-AN_XY(I))+C*ANXY(I)
        A=S1*DFM(1)-AA1*FM(1)
        B=S2*DFM(2)-AA2*FM(2)
        C=S3*DFM(3)-AA3*FM(3)
        DSFM=A*(AMXX(I)-AM_XY(I))+B*(AMYY(I)-AM_XY(I))+C*AMXY(I)
        A=S1*DFNM(1)-AA1*FNM(1)
        B=S2*DFNM(2)-AA2*FNM(2)
        C=S3*DFNM(3)-AA3*FNM(3)
        DSFNM=A*(ANMXX(I)-ANM_XY(I))+B*(ANMYY(I)-AMN_XY(I))
     .      +C*ANMXY(I)
        DF    =(DSFN+DSFM+S(I)*DSFNM)*
     .         (A1(I)-DR*H(I))/YLD_I-TWO*H(I)*YLD_I
        DPLA_J(I)=MAX(ZERO,DPLA_I-F/DF)
       ENDDO
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      DO J=1,NINDX
       I=INDEX(J)
        PLA(I) = PLA(I) + DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_J(I)
        DR =A1(I)*DPLA_J(I)/YLD_I
        XP(1)  =B_1(I)*DR
        XP(2)  =B_2(I)*DR
        XP(3)  =B_3(I)*DR
        C1=ONE+QTIER(I)
        B=TWOP444*GAMA2(I)
        DO K=1,3
         A=B*XP(K)
         JAC(K)=1.+C1*XP(K)+A*XP(K)
         JAC_I(K)=ONE/JAC(K)
         JAC_2(K)= JAC_I(K)*JAC_I(K)
         FNM(K)=ONE-A*XP(K)*HALF
         A=XP(K)*JAC_I(K)
         PN(K)=JAC_I(K)+QTIER(I)*A
         P_M(K)=JAC_I(K)+A
         PNM1(K)=-SQR4_3*GAMA(I)*A
         PNM2(K)=PNM1(K)*ONE_OVER_12
        ENDDO
        A=JAC_2(1)*FNM(1)
        B=JAC_2(2)*FNM(2)
        C=JAC_2(3)*FNM(3)
        SFNM=A*(ANMXX(I)-ANM_XY(I))+B*(ANMYY(I)-AMN_XY(I))
     .      +C*ANMXY(I)
        SN1=SN11(I)*PN(1)+SM11(I)*PNM1(1)*S(I)
        SN2=SN22(I)*PN(2)+SM22(I)*PNM1(2)*S(I)
        S3=SIGNXY(I)*PN(3)+MOMNXY(I)*PNM1(3)*S(I)
        SM1=SM11(I)*P_M(1)+SN11(I)*PNM2(1)*S(I)
        SM2=SM22(I)*P_M(2)+SN22(I)*PNM2(2)*S(I)
        MOMNXY(I)=MOMNXY(I)*P_M(3)+SIGNXY(I)*PNM2(3)*S(I)
        SIGNXX(I)=JQ(I)*(SN1-SN2*Q12(I))
        SIGNYY(I)=JQ(I)*(SN2-SN1*Q21(I))
        SIGNXY(I)=S3
        MOMNXX(I)=JQ(I)*(SM1-SM2*Q12(I))
        MOMNYY(I)=JQ(I)*(SM2-SM1*Q21(I))
        S1=A01*SIGNXX(I)+A02*SIGNYY(I)
     .        -A03*(SIGNXX(I)+SIGNYY(I))*HALF
        DEZZ = - NU5(I)*DPLA_J(I)*S1/YLD_I
        THK(I) = THK(I)+THK(I)* DEZZ*OFF(I)
      ENDDO
C
      DO I=1,NEL
       IF(PLA(I)>EPSMAX.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
      ENDDO
C
      RETURN
      END
