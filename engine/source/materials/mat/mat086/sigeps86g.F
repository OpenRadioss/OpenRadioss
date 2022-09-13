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
Chd|  SIGEPS86G                     source/materials/mat/mat086/sigeps86g.F
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS86G(
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
     C     GS     ,YLD    ,EPSP    ,ISRATE  ,IPLA   )
C-----------------------------------------------------------------------------
C    ZENG 15/07/97
C    N,M SONT REMPLACES PAR N/H,M/H^2
C    LES DIFFERENTES OPTIONS SONT SUPRIMEES 
C    IL NE RESTE QUE : COUPLAGE M-N AVEC R (GAMA DANS LE PROGRAMME)VARIABLE
C    MODIF POUVANT ETRE ENVISAGE: M(X) DONNE PAR USER -> CALCULER R
C-----------------------------------------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL, NUVAR, IPLA, NGL(NEL), MAT(NEL),ISRATE,
     .        IPM(NPROPMI,*)
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
     .   SIGOYZ(NEL),SIGOZX(NEL),
     .   GS(*),EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A R G U M E N T S 
C-----------------------------------------------
      my_real
     . UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real
     . TF(*)
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : Y = F(X)
C        X       : X
C        DYDX    : F'(X) = DY/DX
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NRATE,J1,J2,N,NINDX,NMAX,NFUNC,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),IFUNC(100),NFUNCV,
     .        NFUNCM, NRATEM, IADBUFV,MX
      my_real
     .        E,NU,A,B,FAC,DEZZ,S1(MVSIZ),S2(MVSIZ),
     .        DPLA,EPST(MVSIZ),A1,A2,G,G3,
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ),SVM(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR,
     .        YFAC(MVSIZ,2),NNU1,NU1(MVSIZ),
     .        NU2(MVSIZ),NU3(MVSIZ),DPLA_I,DPLA_J(MVSIZ),H(MVSIZ),
     .        FAIL(MVSIZ),EPSMAX,EPSR1,EPSR2,
     .        ERR,F,DF,YLD_I,TOL,
     .        C1,CP1,CQ1,CP2,CQ2,SM1(MVSIZ),SM2(MVSIZ),SM3,FN,FM,FNM,
     .        PN2,QN2,PM2,QM2,DFN,DFM,DFNM,DA,DB,A_I,B_I,
     .        DFNP,DFNQ,DFMP,DFMQ,DFNMP,DFNMQ,XP,XQ,XPG,XQG,
     .        GM(MVSIZ),CM(MVSIZ),P_M,QM,PNM1,PNM2,QTIER(MVSIZ),
     .        CNM(MVSIZ),AM(MVSIZ),BM(MVSIZ),ANM(MVSIZ),BNM(MVSIZ),
     .        NUM1(MVSIZ),NUM2(MVSIZ),AN(MVSIZ),BN(MVSIZ),
     .        NVM(MVSIZ),MVM(MVSIZ),NMVM(MVSIZ),PN,QN,SN1,SN2,S,
     .        QNM1,QNM2,FNP,FNQ,FMP,FMQ,FNMP,FNMQ,S3,AA,BB,M1,M2,
     .        LFN(MVSIZ),QFN(MVSIZ),QFNM(MVSIZ),RR(MVSIZ),
     .        D1,D2,DWT,DWE,DWP,AAA,BBB,CCC,FS,MS,
     .        AM1(MVSIZ),AM2(MVSIZ),GAMA(MVSIZ),GAMA2(MVSIZ),
     .        EPSF
C
      DATA NMAX/2/
        TOL = EM4     
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C       R=0.833333333333
       IF(IVECTOR==1) THEN
         MX = MAT(1)
         NFUNCM = 0
         NFUNCV = IPM(10,MX)
         NFUNCM = MAX(NFUNCM,NFUNCV)
         DO J=1,NFUNCM
             IF(NFUNCV>=J) THEN
               IFUNC(J) = IPM(10+J,MX)
             ENDIF
         ENDDO
       END IF
       NRATEM = 0
       MX = MAT(1)
       IADBUFV = IPM(7,MX)-1
       E   = UPARAM(IADBUFV+2)
       A1  = UPARAM(IADBUFV+3)
       A2  = UPARAM(IADBUFV+4)
       G   = UPARAM(IADBUFV+5)
       G3  = THREE*G      
       NU  = UPARAM(IADBUFV+6)
       NRATE = NINT(UPARAM(IADBUFV+1))
       NRATEM = MAX(NRATEM,NRATE)
       EPSMAX=UPARAM(IADBUFV+6+2*NRATE+1)
       EPSR1 =UPARAM(IADBUFV+6+2*NRATE+2)
       IF(EPSR1==ZERO)EPSR1=EP30
       EPSR2 =UPARAM(IADBUFV+6+2*NRATE+3)
       IF(EPSR2==ZERO)EPSR2=TWOEP30
       EPSF = UPARAM(IADBUFV+6+2*NRATE+9) 
       NNU1 = NU / (ONE - NU) 
       DO I=1,NEL
           C1=THK0(I)*ONE_OVER_12
           AM1(I)  = A1*C1
           AM2(I)  = A2*C1
           GM(I)   = G*C1

C
C
       ENDDO
       IF(IVECTOR==1) THEN
         DO I=1,NEL
           IF(EPSMAX==ZERO)THEN
             IF(TF(NPF(IFUNC(1)+1)-1)==ZERO)THEN
               EPSMAX=TF(NPF(IFUNC(1)+1)-2)
             ELSE
               EPSMAX= EP30
             END IF
           END IF
        END DO
       ELSE
         DO I=1,NEL
           IF(EPSMAX==ZERO)THEN
             IF(TF(NPF(IPM(11,MAT(I))+1)-1)==ZERO)THEN
               EPSMAX=TF(NPF(IPM(11,MAT(I))+1)-2)
             ELSE
               EPSMAX= EP30
             END IF
           END IF
        END DO
       ENDIF
C
      IF (ISIGI==0) THEN
       IF(TIME==ZERO)THEN
        DO I=1,NEL
         UVAR(I,1)=ZERO
         UVAR(I,2)=ZERO 
         DO J=1,NRATE
           UVAR(I,J+2)=ZERO
         ENDDO
        ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------
C
      DO I=1,NEL
       SIGNXX(I)=SIGOXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
       SIGNYY(I)=SIGOYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
       SIGNXY(I)=SIGOXY(I)+G *DEPSXY(I)
       MOMNXX(I)=MOMOXX(I)+AM1(I)*DEPBXX(I)+AM2(I)*DEPBYY(I)
       MOMNYY(I)=MOMOYY(I)+AM2(I)*DEPBXX(I)+AM1(I)*DEPBYY(I)
       MOMNXY(I)=MOMOXY(I)+GM(I) *DEPBXY(I)
       SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
       SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
C
       SOUNDSP(I) = SQRT(A1/RHO0(I))
       VISCMAX(I) = ZERO
       ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
       IF (ISRATE == 0) EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
       EPST(I) = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
       FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST(I))
     .                           /(EPSR2-EPSR1)))
      ENDDO
C-------------------
C     CRITERE
C-------------------
      DO I=1,NEL
        JJ(I) = 1
      ENDDO
        DO J=2,NRATEM-1
          DO I=1,NEL
            IF(NRATE-1>=J) THEN
              IF(EPSP(I)>=UPARAM(IADBUFV+6+J)) JJ(I) = J
            ENDIF
          ENDDO
        ENDDO
C
       DO I=1,NEL
         FAC=UPARAM(IADBUFV+6+JJ(I))
         RATE(I)=(EPSP(I) - FAC)/(UPARAM(IADBUFV+7+JJ(I)) - FAC)
         YFAC(I,1)=UPARAM(IADBUFV+6+NRATE+JJ(I))
         YFAC(I,2)=UPARAM(IADBUFV+7+NRATE+JJ(I))
       ENDDO
C
      IF(IVECTOR==1) THEN
       DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        IPOS1(I) = NINT(UVAR(I,J1))
        IAD1(I)  = HALF*NPF(IFUNC(J1)) + 1
        ILEN1(I) = HALF*NPF(IFUNC(J1)+1) - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,J2))
        IAD2(I)  = HALF*NPF(IFUNC(J2)) + 1
        ILEN2(I) = HALF*NPF(IFUNC(J2)+1) - IAD2(I) - IPOS2(I)
       ENDDO
      ELSE
       DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        IPOS1(I) = NINT(UVAR(I,J1))
        IAD1(I)  = NPF(IPM(10+J1,MAT(I))) / 2 + 1
        ILEN1(I) = NPF(IPM(10+J1,MAT(I))+1) / 2 - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,J2))
        IAD2(I)  = NPF(IPM(10+J2,MAT(I))) / 2 + 1
        ILEN2(I) = NPF(IPM(10+J2,MAT(I))+1) / 2 - IAD2(I) - IPOS2(I)
       ENDDO
      END IF
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        Y1(I)=Y1(I)*YFAC(I,1)
        Y2(I)=Y2(I)*YFAC(I,2)
        FAC   = RATE(I)
        YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        YLD(I) = MAX(YLD(I),EM20)
        DYDX1(I)=DYDX1(I)*YFAC(I,1)
        DYDX2(I)=DYDX2(I)*YFAC(I,2)
        H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
        UVAR(I,J1) = IPOS1(I)
        UVAR(I,J2) = IPOS2(I)
      ENDDO
      IF(IPLA==0) THEN
C---------------------------
C    RADIAL RETURN
C---------------------------
      DO  I=1,NEL
       MS=MOMNXX(I)+MOMNYY(I)
       FS=SIGNXX(I)+SIGNYY(I)
       SVM(I) = SIXTEEN*(MS*MS +THREE*(MOMNXY(I)*MOMNXY(I) 
     .                                    - MOMNXX(I)*MOMNYY(I)))
     .          + FS*FS+ THREE*(SIGNXY(I)*SIGNXY(I)-SIGNXX(I)*SIGNYY(I))
       SVM(I) = SQRT(MAX(SVM(I),EM20))
       RR(I) = MIN(ONE,YLD(I)/SVM(I))
       IF(RR(I)<ONE) ETSE(I)= H(I)/(H(I)+E)
      ENDDO
      DO  I=1,NEL
       SIGNXX(I) = SIGNXX(I)*RR(I)
       SIGNYY(I) = SIGNYY(I)*RR(I)
       SIGNXY(I) = SIGNXY(I)*RR(I)
       MOMNXX(I) = MOMNXX(I)*RR(I)
       MOMNYY(I) = MOMNYY(I)*RR(I)
       MOMNXY(I) = MOMNXY(I)*RR(I)
       D1 = SIGNXX(I)-SIGOXX(I)
       D2 = SIGNYY(I)-SIGOYY(I)
       NU  = UPARAM(IADBUFV+6)
       DWE =((SIGNXX(I)+SIGOXX(I))*(D1-NU*D2)+
     .       (SIGNYY(I)+SIGOYY(I))*(-NU*D1+D2))/E+
     .      (SIGNXY(I)+SIGOXY(I))*(SIGNXY(I)-SIGOXY(I))/G
       D1 = MOMNXX(I)-MOMOXX(I)
       D2 = MOMNYY(I)-MOMOYY(I)
       DWE =DWE+ TWELVE*(
     .          ((MOMNXX(I)+MOMOXX(I))*(D1-NU*D2)
     .         +(MOMNYY(I)+MOMOYY(I))*(-NU*D1+D2))/E
     .         +(MOMNXY(I)+MOMOXY(I))*(MOMNXY(I)-MOMOXY(I))/G )
       DWT =   (SIGNXX(I)+SIGOXX(I))*DEPSXX(I)+
     .         (SIGNYY(I)+SIGOYY(I))*DEPSYY(I)+
     .         (SIGNXY(I)+SIGOXY(I))*DEPSXY(I)
       DWT = DWT+THK0(I)*((MOMNXX(I)+MOMOXX(I))*DEPBXX(I)+
     .                    (MOMNYY(I)+MOMOYY(I))*DEPBYY(I)+
     .                    (MOMNXY(I)+MOMOXY(I))*DEPBXY(I))
       DWP =DWT-DWE
       DPLA = OFF(I)* MAX(ZERO,HALF*DWP/YLD(I))
       PLA(I)=PLA(I) + DPLA
       AAA  = ABS(DWE)
       BBB  = MAX(ZERO,DWP)
       CCC  = MAX(EM20,AAA+BBB)
       DEZZ = - (DEPSXX(I)+DEPSYY(I)) * (NNU1*AAA + BBB) / CCC
       THK(I) = THK(I) * (ONE + DEZZ*OFF(I))
      ENDDO
      ELSEIF(CODVERS<44)THEN
C     IF(CODVERS<44.AND.IPLA==1) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
      DO  I=1,NEL
C-------------------------------------------------------------------------
C     GAMA (L'INVERSE DE GAMA DANS LA FORMULE) A ETE PRIS A 2/3 PAR DEFAUT
C-------------------------------------------------------------------------
       C1 = PLA(I)*E
       GAMA(I) = THREE_HALF*(C1+YLD(I))/(THREE_HALF*C1+YLD(I))
       GAMA2(I) = GAMA(I)*GAMA(I)    
       CM(I) = SIXTEEN*GAMA2(I)
         CNM(I) = SQR16_3*GAMA(I)
       QTIER(I) = FOUR_OVER_3*GAMA2(I)
       H(I) = MAX(ZERO,H(I))
       S1(I) = (SIGNXX(I)+SIGNYY(I))*HALF
       S2(I) = (SIGNXX(I)-SIGNYY(I))*HALF
       S3 = SIGNXY(I)
       SM1(I) = (MOMNXX(I)+MOMNYY(I))*HALF
       SM2(I) = (MOMNXX(I)-MOMNYY(I))*HALF    
       SM3 = MOMNXY(I)
       AN(I) = S1(I)*S1(I)
       BN(I) = THREE*(S2(I)*S2(I)+S3*S3)
       NVM(I) = AN(I)+BN(I)  
       AM(I) = SM1(I)*SM1(I)*CM(I)
       BM(I) =THREE*(SM2(I)*SM2(I)+SM3*SM3)*CM(I)
       MVM(I) = AM(I)+BM(I)
       ANM(I) = S1(I)*SM1(I)*CNM(I)
       BNM(I) = THREE*(S2(I)*SM2(I)+S3*SM3)*CNM(I)
       NMVM(I) = ANM(I)+BNM(I)
       SVM(I) = SQRT(NVM(I)+MVM(I)+ABS(NMVM(I)))
       DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
       THK(I) = THK(I) +THK(I)* DEZZ*OFF(I)
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
      NU=UPARAM(IADBUFV+6)
      DO  J=1,NINDX
        I=INDEX(J)
        NU1(I) = ONE/(ONE-NU)
        NU2(I) = ONE/(ONE+NU)
        NU3(I) = ONE -NNU1
        NUM1(I) = NU1(I)*QTIER(I)
        NUM2(I) = NU2(I)*QTIER(I)
        DPLA_J(I)=(SVM(I)-YLD(I))/(G3*QTIER(I)+H(I))
        ETSE(I)= H(I)/(H(I)+E)
      ENDDO
C-------------------------------
C     TIENT COMPTE DU COUPLAGE
C-------------------------------
      DO N=1,NMAX
#include "vectorize.inc"
       DO J=1,NINDX
        I=INDEX(J)
        DPLA_I=DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_I
        DR =HALF*E*DPLA_I/YLD_I
        XP  =DR*NU1(I)
        XQ  =THREE*DR*NU2(I)
        XPG  =XP*ZEP444*GAMA2(I)
        XQG  =XQ*ZEP444*GAMA2(I)
        C1=ONE+QTIER(I)
        DA=C1+TWOP444*GAMA2(I)*XP
        DB=C1+TWOP444*GAMA2(I)*XQ
        A=ONE +(DA+C1)*XP*HALF
        B=ONE +(DB+C1)*XQ*HALF
        A_I=ONE/A
        B_I=ONE/B
        AA=A_I*A_I
        BB=B_I*B_I
        DFNP=FIVEP5+SIXTEENP5*XPG
        FNP=ONE+(DFNP+FIVEP5)*XPG*HALF
        DFNQ=FIVEP5+SIXTEENP5*XQG
        FNQ=ONE+(DFNQ+FIVEP5)*XQG*HALF
        DFMP=ONEP8333*(XP+ONE)
        FMP=ONE+(DFMP+ONEP8333)*XP*HALF
        DFMQ=ONEP8333*(XQ+ONE)
        FMQ=ONE+(DFMQ+ONEP8333)*XQ*HALF
        DFNMP=-TWOP444*XP*GAMA2(I)
        FNMP=ONE+DFNMP*XP*HALF
        DFNMQ=-TWOP444*XQ*GAMA2(I)
        FNMQ=ONE+DFNMQ*XQ*HALF
        FN=AA*FNP*AN(I)+BB*FNQ*BN(I)
        FM=AA*FMP*AM(I)+BB*FMQ*BM(I)
        FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
        IF (FNM<ZERO) THEN
         FNM=-FNM
         S=-ONE
        ELSE
         S=ONE
        ENDIF 
        F    =FN+FM+FNM-YLD_I*YLD_I
        C1=NU1(I)*AA*A_I
        CP1=C1*A
        CP2=C1*DA*TWO
        C1=THREE*NU2(I)*BB*B_I
        CQ1=C1*B
        CQ2=C1*DB*2.0
        C1=ZEP444*GAMA2(I)
        DFN=AN(I)*(CP1*DFNP*C1-FNP*CP2)
     .      + BN(I)*(CQ1*DFNQ*C1-FNQ*CQ2)
        DFM=AM(I)*(CP1*DFMP-FMP*CP2)
     .      + BM(I)*(CQ1*DFMQ-FMQ*CQ2)
        DFNM=ANM(I)*(CP1*DFNMP-FNMP*CP2)
     .      + BNM(I)*(CQ1*DFNMQ-FNMQ*CQ2)
        DF    =(DFN+DFM+S*DFNM)*
     .         (E*HALF-DR*H(I))/YLD_I-2.*H(I)*YLD_I
C DEBUG
C          IF(I==21) THEN
C            WRITE(12,*) 'DFN,DFM,DFNM'
C            WRITE(12,*) DFN,DFM,DFNM
C            ERR=ABS(F/(DF*DPLA_I))
C            WRITE(12,*) 'N,ERR,DPLA_I,F,DF'
C            WRITE(12,'(I5,F8.5,3E16.6)') N,ERR,DPLA_I,F,DF
C          ENDIF
C
        DPLA_J(I)=MAX(ZERO,DPLA_I-F/DF)
       ENDDO
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
      DO J=1,NINDX
        I=INDEX(J)
        PLA(I) = PLA(I) + DPLA_J(I)
        DPLA_I=DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_I
        DR =HALF*E*DPLA_I/YLD_I
        XP  =DR*NU1(I)
        XQ  =THREE*DR*NU2(I)
        XPG  =XP*ZEP444*GAMA2(I)
        XQG  =XQ*ZEP444*GAMA2(I)
        C1=ONE+QTIER(I)
        A=ONE+C1*XP+TWOP444*GAMA2(I)*XP*XP
        B=ONE+C1*XQ+TWOP444*GAMA2(I)*XQ*XQ
        A_I=ONE/A
        B_I=ONE/B
        AA=A_I*A_I
        BB=B_I*B_I
        FNMP=ONE-ONEP222*GAMA2(I)*XP*XP
        FNMQ=ONE-ONEP222*GAMA2(I)*XQ*XQ
        FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
        IF (FNM<ZERO) THEN
         S=-ONE
        ELSE
         S=ONE
        ENDIF 
        PN=(ONE+QTIER(I)*XP)*A_I
        P_M=(ONE+XP)*A_I
        PNM1=-SQR4_3*GAMA(I)*S*XP*A_I
        PNM2=PNM1*ONE_OVER_12
        QN=(ONE+QTIER(I)*XQ)*B_I
        QM=(ONE+XQ)*B_I
        QNM1=-SQR4_3*XQ*GAMA(I)*S*B_I
        QNM2=QNM1*ONE_OVER_12
        SN1=S1(I)*PN+SM1(I)*PNM1
        SN2=S2(I)*QN+SM2(I)*QNM1
        S3=SIGNXY(I)*QN+MOMNXY(I)*QNM1
        M1=SM1(I)*P_M+S1(I)*PNM2
        M2=SM2(I)*QM+S2(I)*QNM2
        MOMNXY(I)=SIGNXY(I)*QNM2+MOMNXY(I)*QM
        SIGNXX(I)=SN1+SN2
        SIGNYY(I)=SN1-SN2
        SIGNXY(I)=S3
        MOMNXX(I)=M1+M2
        MOMNYY(I)=M1-M2
        DEZZ = - NU3(I)*DR*SN1*2./E
        THK(I) = THK(I) + DEZZ*THK(I)*OFF(I)
C
C            IF (I==21) THEN
C            WRITE(12,'(/A, E12.4)') 'TIME', TIME
C            WRITE(12,'(A,2E12.4)') 'THK ',THK(I),THKLY(I)
C            WRITE(12,'(A,3E12.4)') 'DDEF',DEPSXX(I),DEPSYY(I),DEPSXY(I)
C            WRITE(12,'(A,3E12.4)') 'DDEF',DEPSYZ(I),DEPSZX(I)
C            WRITE(12,'(A,3E12.4)') 'DCUR',DEPBXX(I),DEPBYY(1),DEPBXY(I)
C            WRITE(12,'(A,3E12.4)') 'DEFO',EPSXX(I),EPSYY(I),EPSXY(I)
C            WRITE(12,'(A, E12.4)') 'DPLA',DPLA_J(I)
C            WRITE(12,'(A,3E12.4)') 'CONT',SIGNXX(I),SIGNYY(I),SIGNXY(I)
C            WRITE(12,'(A,3E12.4)') 'MOM ',MOMNXX(I),MOMNYY(I),MOMNXY(I)
C            WRITE(12,'(A, E12.4)') 'YLD ',YLD(I)
C            WRITE(12,'(A, E12.4)') 'FAIL ',FAIL(I)
C            WRITE(12,'(A,3E12.4)') 'VONM',NVM(I),MVM(I),NMVM(I)
C           ENDIF
      ENDDO
      ELSE
C     IF(CODVERS>=44.AND.IPLA==1) THEN
C-------------------------
C     ITERATIVE
C-------------------------
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
      DO  I=1,NEL
C-------------------------------------------------------------------------
C     GAMA (L'INVERSE DE GAMA DANS LA FORMULE) 
C-------------------------------------------------------------------------
       C1 = PLA(I)*E
       CCC=EXP(-TWOP6667*C1/YLD(I))
       GAMA(I) = TWO/(THREE-CCC)
       GAMA2(I) = GAMA(I)*GAMA(I)
       CM(I) = THIRTY6*GAMA2(I)
       CNM(I) = THREEP4641*GAMA(I)  
       QTIER(I) = THREE*GAMA2(I)
       H(I) = MAX(ZERO,H(I))
       S1(I) = (SIGNXX(I)+SIGNYY(I))*HALF
       S2(I) = (SIGNXX(I)-SIGNYY(I))*HALF
       S3 = SIGNXY(I)
       SM1(I) = (MOMNXX(I)+MOMNYY(I))*HALF
       SM2(I) = (MOMNXX(I)-MOMNYY(I))*HALF
       SM3 = MOMNXY(I)
       AN(I) = S1(I)*S1(I)
       BN(I) = THREE*(S2(I)*S2(I)+S3*S3)
       NVM(I) = AN(I)+BN(I)  
       AM(I) = SM1(I)*SM1(I)*CM(I)
       BM(I) = THREE*(SM2(I)*SM2(I)+SM3*SM3)*CM(I)
       MVM(I) = AM(I)+BM(I)
       ANM(I) = S1(I)*SM1(I)*CNM(I)
       BNM(I) = THREE*(S2(I)*SM2(I)+S3*SM3)*CNM(I)
       NMVM(I) = ANM(I)+BNM(I)
       SVM(I) = SQRT(NVM(I)+MVM(I)+ABS(NMVM(I)))
       DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
       THK(I) = THK(I) +THK(I)* DEZZ*OFF(I)
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
        NU=UPARAM(IADBUFV+6)
      DO  J=1,NINDX
        I=INDEX(J)
        NU1(I) = HALF*(ONE + NU)
        NU2(I) = THREE_HALF*(ONE-NU)
        NU3(I) = ONE-NNU1
        NUM1(I) = ONE+QTIER(I)
        NUM2(I) = FIVEP5*GAMA2(I)
        LFN(I)=NUM2(I)
        QFN(I)=SIXTEENP5*GAMA2(I)*GAMA2(I)
        QFNM(I)=-NUM2(I)
        DPLA_J(I)=(SVM(I)-YLD(I))/(G3*QTIER(I)+H(I))
        ETSE(I)= H(I)/(H(I)+E)
      ENDDO
C-------------------------------
C     TIENT COMPTE DU COUPLAGE
C-------------------------------
      DO N=1,NMAX
#include "vectorize.inc"
       DO J=1,NINDX
        I=INDEX(J)
        DPLA_I=DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_I
        DR =A1*DPLA_I/YLD_I
        XP  =DR*NU1(I)
        XQ  =DR*NU2(I)
        DA=NUM1(I)+NUM2(I)*XP
        DB=NUM1(I)+NUM2(I)*XQ
        A=ONE+(DA+NUM1(I))*XP*HALF
        B=ONE+(DB+NUM1(I))*XQ*HALF
        A_I=ONE/A
        B_I=ONE/B
        AA=A_I*A_I
        BB=B_I*B_I
        DFNP=LFN(I)+QFN(I)*XP
        DFNQ=LFN(I)+QFN(I)*XQ
        DFMP=ONEP8333*(XP+ONE)
        DFMQ=ONEP8333*(XQ+ONE)
        DFNMP=QFNM(I)*XP
        DFNMQ=QFNM(I)*XQ
        XP = HALF*XP
        XQ = HALF*XQ
        FNP=ONE+(DFNP+LFN(I))*XP
        FNQ=ONE+(DFNP+LFN(I))*XQ
        FMP=ONE+(DFMP+ONEP8333)*XP
        FMQ=ONE+(DFMQ+ONEP8333)*XQ
        FNMP=ONE+DFNMP*XP
        FNMQ=ONE+DFNMQ*XQ
        FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
        IF (FNM<ZERO) THEN
         S=-ONE
        ELSE
         S=ONE
        ENDIF 
        CP1 =(FNP*AN(I)+S*FNMP*ANM(I)+FMP*AM(I))*AA
        CQ1 =(FNQ*BN(I)+S*FNMQ*BNM(I)+FMQ*BM(I))*BB
        CP2 =(DFNP*AN(I)+S*DFNMP*ANM(I)+DFMP*AM(I))*AA
        CQ2 =(DFNQ*BN(I)+S*DFNMQ*BNM(I)+DFMQ*BM(I))*BB
        XPG =TWO*NU1(I)*DA*A_I
        XQG =TWO*NU2(I)*DB*B_I
        F    =CP1 +CQ1-YLD_I*YLD_I
        DF    =(CP2*NU1(I)+CQ2*NU2(I)-CP1*XPG-CQ1*XQG)*
     .         (A1-DR*H(I))/YLD_I-TWO*H(I)*YLD_I
C DEBUG
C          IF(I==21) THEN
C            WRITE(12,*) 'FN,FM,FNM'
C            WRITE(12,*) FNP+FNQ,FMP+FNQ,FNMP+FNMQ
C            ERR=ABS(F/(DF*DPLA_I))
C            WRITE(12,*) 'N,DPLA_I,F,DF'
C            WRITE(12,'(I5,3E16.6)') N,DPLA_I,F,DF
C          ENDIF
C
        DPLA_J(I)=MAX(ZERO,DPLA_I-F/DF)
       ENDDO
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
      DO J=1,NINDX
        I=INDEX(J)
        PLA(I) = PLA(I) + DPLA_J(I)
        DPLA_I=DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_I
        DR =A1*DPLA_I/YLD_I
        XP  =DR*NU1(I)
        XQ  =DR*NU2(I)
        XPG  =XP*XP
        XQG  =XQ*XQ
        A=ONE + NUM1(I)*XP+NUM2(I)*XPG
        B=ONE+NUM1(I)*XQ+NUM2(I)*XQG
        A_I=ONE/A
        B_I=ONE/B
        AA=A_I*A_I
        BB=B_I*B_I
        FNMP=ONE+QFNM(I)*XPG
        FNMQ=ONE+QFNM(I)*XQG
        FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
        IF (FNM<ZERO) THEN
         S=-ONEP732*GAMA(I)
        ELSE
         S=ONEP732*GAMA(I)
        ENDIF 
        QN=ONE+QTIER(I)*XQ
        QNM1=XQ*S
        QNM2=QNM1*ONE_OVER_12
        SN1=(S1(I)*(1.+QTIER(I)*XP)-SM1(I)*S*XP)*A_I
        SN2=(S2(I)*QN-SM2(I)*QNM1)*B_I
        S3=(SIGNXY(I)*QN-MOMNXY(I)*QNM1)*B_I
        M1=(SM1(I)*(ONE+XP)-S1(I)*S*XP*ONE_OVER_12)*A_I
        M2=(SM2(I)*(1.+XQ)-S2(I)*QNM2)*B_I
        MOMNXY(I)=(MOMNXY(I)*(1.+XQ)-SIGNXY(I)*QNM2)*B_I
        SIGNXX(I)=SN1+SN2
        SIGNYY(I)=SN1-SN2
        SIGNXY(I)=S3
        MOMNXX(I)=M1+M2
        MOMNYY(I)=M1-M2
        DEZZ = - NU3(I)*DR*SN1/E
        THK(I) = THK(I) + DEZZ*THK(I)*OFF(I)
C
C            IF (I==21) THEN
C            WRITE(12,'(/A, E12.4)') 'TIME', TIME
C            WRITE(12,'(A,2E12.4)') 'THK ',THK(I)
C            WRITE(12,'(A,3E12.4)') 'DDEF',DEPSXX(I),DEPSYY(I),DEPSXY(I)
C            WRITE(12,'(A,3E12.4)') 'DDEF',DEPSYZ(I),DEPSZX(I)
C            WRITE(12,'(A,3E12.4)') 'DCUR',DEPBXX(I),DEPBYY(1),DEPBXY(I)
C            WRITE(12,'(A,3E12.4)') 'DEFO',EPSXX(I),EPSYY(I),EPSXY(I)
C            WRITE(12,'(A, E12.4)') 'DPLA',DPLA_J(I)
C            WRITE(12,'(A,3E12.4)') 'CONT',SIGNXX(I),SIGNYY(I),SIGNXY(I)
C            WRITE(12,'(A,3E12.4)') 'MOM ',MOMNXX(I),MOMNYY(I),MOMNXY(I)
C            WRITE(12,'(A, E12.4)') 'YLD ',YLD(I)
C            WRITE(12,'(A, E12.4)') 'FAIL ',FAIL(I)
C            WRITE(12,'(A,3E12.4)') 'VONM',NVM(I),MVM(I),NMVM(I)
C           ENDIF
      ENDDO
      ENDIF
C
      DO I=1,NEL
        IF((PLA(I)>EPSMAX.OR.EPST(I)>EPSF).
     .   AND.OFF(I)==ONE) THEN
           OFF(I)=FOUR_OVER_5
C           WRITE(12,'(A,I5,2E12.4)') 'RUPTURE', I, PLA(I), EPSMAX(I)
        ENDIF
      ENDDO
C
      RETURN
      END
