Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!hd|====================================================================
!hd|  SIGEPS02G                     src/mate02/sigeps02g.F           
!hd|-- called by -----------
!hd|        MULAWGLC                      src/coque/mulawglc.F          
!hd|-- calls ---------------
!hd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
!hd|====================================================================
Chd|====================================================================
Chd|  SIGEPS02G                     source/materials/mat/mat002/sigeps02g.F
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS02G(ELBUF_STR,
     1                     JFT      ,JLT     ,PM      ,FOR     ,MOM      ,
     2                     THK      ,EINT    ,OFF     ,DT1C    ,ISRATE   ,
     3                     G        ,A1      ,A2      ,VOL0    ,NU       ,
     4                     THK0     ,GS      ,EPSP    ,IOFC    ,KFTS     ,
     5                     NGL      ,INDX    ,IPLA    ,IR      ,IS       ,
     6                     DEGMB    ,DEGFX   ,DEPSXX  ,DEPSYY  ,MX       ,
     7                     DEPSXY   ,DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY   ,
     8                     DEPBXY   ,SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ   ,
     9                     SIGOZX   ,MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX   ,
     A                     SIGNYY   ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX   ,
     B                     MOMNYY   ,MOMNXY  ,ETSE    ,EXZ     ,EYZ      ,
     C                     NEL     ,IOFF_DUCT,VP      , NUVAR  ,UVAR)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE ELBUFDEF_MOD
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "parit_c.inc"
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER JFT,JLT,IOFC,KFTS,NLAY,IR,IS,NEL
      INTEGER NGL(MVSIZ),INDX(MVSIZ), 
     .        IOFF_DUCT(*),MX
      INTEGER, INTENT(IN) :: NUVAR ,VP
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR

      my_real
     .   PM(NPROPM,*),FOR(NEL,5),MOM(NEL,3),EINT(JLT,2),
     .   OFF(*),DT1C(*),NU(*),G(*),A1(*),A2(*),
     .   VOL0(*),THK0(*),GS(*),EPSP(*)
      my_real
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),
     .   DEPSYZ(NEL),DEPSZX(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   DEGMB(MVSIZ),DEGFX(MVSIZ),EXZ(*),EYZ(*)
      my_real
     .    THK(*),
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),ETSE(NEL)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER ICC,IRTY,ISRATE,NINDX,INDEX(MVSIZ)
      INTEGER I,J,II,NPIF,IPLA,N,NMAX
      my_real
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),F4(MVSIZ),F5(MVSIZ),Z3,Z4,
     .   M1(MVSIZ),M2(MVSIZ),M3(MVSIZ),T(MVSIZ),EPMX,
     .   DWELM(MVSIZ),DWELF(MVSIZ),CA(MVSIZ),CB(MVSIZ),CN,
     .   YMAX(MVSIZ),UNSYEQ(MVSIZ),DWPLA(MVSIZ),
     .   HH(MVSIZ),RR(MVSIZ),C1,C2,C3,CC,EPDR(MVSIZ),CP,  
     .   YM,EPSPDT(MVSIZ),
     .   S1(MVSIZ),S2(MVSIZ),SVM(MVSIZ),NNU1(MVSIZ),NU1(MVSIZ),
     .   NU2(MVSIZ),NU3(MVSIZ),DPLA_J(MVSIZ),SM1(MVSIZ),SM2(MVSIZ),
     .   AM(MVSIZ),BM(MVSIZ),ANM(MVSIZ),BNM(MVSIZ),QTIER(MVSIZ),
     .   NUM1(MVSIZ),NUM2(MVSIZ),AN(MVSIZ),BN(MVSIZ),
     .   GAMA(MVSIZ),GAMA2(MVSIZ),LFN(MVSIZ),QFN(MVSIZ),QFNM(MVSIZ),
     .   DEGMB_LOC(MVSIZ),DEGSH_LOC(MVSIZ),DEGFX_LOC(MVSIZ),YLD(MVSIZ),
     .   LOGEP(MVSIZ),PLAP(MVSIZ)
      my_real
     .   DPLA_I,DR,A,B,F,DF,YLD_I,CP1,CQ1,CP2,CQ2,SM3,FN,FM,FNM,
     .   DA,DB,A_I,B_I,PN,QN,SN1,SN2,S,P_M,QM,PNM1,PNM2,
     .   DFNP,DFNQ,DFMP,DFMQ,DFNMP,DFNMQ,XP,XQ,XPG,XQG,
     .   QNM1,QNM2,FNP,FNQ,FMP,FMQ,FNMP,FNMQ,S3,AA,BB,PLAP1,
     .   THK12,THK2,FAC,SH1,EZZ,DPLA(MVSIZ),AAA,BBB,CCC,
     .   FACT,FF1,FF2,FF3,MM1,MM2,MM3,AUX,EPIF,ASRATE,
     .   MS,FS,D1,D2,MT,TM,TSTAR,VISC,CA_1,CB_1,YMAX_1
      TYPE(L_BUFEL_) ,POINTER :: LBUF
!-----------------------------------------------
      DATA NMAX/2/
!-----------------------------------------------
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
!
      EPIF   = ZERO
      EPIF   = MAX(EPIF,PM(43,MX))
      YM     = PM(20,MX)
      C1     = PM(28,MX)
      C2     = PM(29,MX)
      C3     = PM(30,MX)
      CA_1   = PM(38,MX)
      CB_1   = PM(39,MX)
      YMAX_1 = PM(42,MX)
      CN     = PM(40,MX)
      EPMX   = PM(41,MX)
      CC     = PM(43,MX)
      ICC    = NINT(PM(49,MX))
      IRTY   = NINT(PM(50,MX))
      Z3     = PM(51,MX)
      Z4     = PM(52,MX)
      CP     = PM(53,MX)
!
      DO I=JFT,JLT
        ASRATE = MIN(ONE,PM(9,MX)*DT1C(I))
        CA(I)  = CA_1
        CB(I)  = CB_1
        YMAX(I)= YMAX_1
        NPIF   = NINT(PM(50,MX))
        T(I)   = PM(54,MX)
        ETSE(I) = ONE
        EPSPDT(I) = ONE
      ENDDO
      IF(VP == 1)THEN
        DO I=JFT,JLT
          AUX    = PM(44,MX)
          EPDR(I)= MAX(EM20,AUX)
        ENDDO
      ELSE               
        DO I=JFT,JLT
          AUX    = PM(44,MX)*DT1C(I)
          EPDR(I)= MAX(EM20,AUX)
        ENDDO
      ENDIF

!---------------------------
!     CONTRAINTES ELASTIQUES
!---------------------------
      DO I=JFT,JLT
        DEGSH_LOC(I) = FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I) ! shear only
        DEGMB_LOC(I) = DEGMB(I) - DEGSH_LOC(I) ! (membrane without shear)
        DEGFX_LOC(I) = DEGFX(I) ! bending only
!
        F1(I) = SIGOXX(I)+ A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
        F2(I) = SIGOYY(I)+ A1(I)*DEPSYY(I)+A2(I)*DEPSXX(I)
        F3(I) = SIGOXY(I)+ G(I) *DEPSXY(I)
        F4(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        F5(I) = SIGOZX(I) + GS(I)*DEPSZX(I)
!
        THK12 = THK0(I)*ONE_OVER_12
        M1(I) = MOMOXX(I) + (A1(I)*DEPBXX(I)+A2(I)*DEPBYY(I))*THK12
        M2(I) = MOMOYY(I) + (A1(I)*DEPBYY(I)+A2(I)*DEPBXX(I))*THK12
        M3(I) = MOMOXY(I) + G(I)*DEPBXY(I)*THK12
!
        MS = M1(I)+M2(I)
        FS = F1(I)+F2(I)
        UNSYEQ(I) = ONE/
     .    SQRT(MAX(SIXTEEN*(MS*MS + THREE*(M3(I)*M3(I) - M1(I)*M2(I)))
     .         +    FS*FS + THREE*(F3(I)*F3(I) - F1(I)*F2(I)),EM20))
      ENDDO
!-------------
!     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
!-------------
      IF (EPIF /= ZERO) THEN
!       e = 1/t integ[1/2 E (eps_m + k z)^2 dz ]
!       e = 1/2 E eps_eq^2
!       eps_eq = sqrt[ eps_m^2 + 1/12 k^2t^2 ]
        IF(VP==1)THEN
          DO I=JFT,JLT
            PLAP(I)  = UVAR(I,I)
            PLAP(I)  = MAX(PLAP(I),EPDR(I))
            LOGEP(I) = LOG(PLAP(I)/EPDR(I))
          ENDDO           
        ELSE
          IF (ISRATE >= 1) THEN
            DO I=JFT,JLT
              EPSPDT(I) = EPSP(I)*DT1C(I)
              EPSPDT(I) = MAX(EPSPDT(I),EM20)
              LOGEP(I)  = LOG(EPSPDT(I)/EPDR(I))
            ENDDO
          ELSE
            DO I=JFT,JLT
              EPSPDT(I) = ABS(DEGMB_LOC(I)+DEGFX_LOC(I)*THK0(I))*UNSYEQ(I)
              EPSPDT(I) = MAX(EPSPDT(I),EM20)
              LOGEP(I)  = LOG(EPSPDT(I)/EPDR(I))
            ENDDO
          ENDIF ! IF (ISRATE >= 1)
        ENDIF
        DO I=JFT,JLT
          EPSPDT(I) = LOGEP(I)    
          T(I) = T(I)+CP*(EINT(I,1)+EINT(I,2))/VOL0(I)
        ENDDO
!
        IF (NPIF == ZERO .AND. (IMACH /= 3 .OR. IPARIT == 0)) THEN
          DO I=JFT,JLT
            MT = MAX(EM20,Z3)
            TM = Z4
!--          TSTAR=MAX(ZERO,(T(I)-298.)/(TM-298.))
!--          ... the MAX() is replaced by the IF below
            EPSPDT(I) = MAX(ZERO,EPSPDT(I))
            TSTAR = (T(I)-TWOHUNDRED98)/(TM-TWOHUNDRED98)
            IF (TSTAR > ZERO) THEN
              EPSPDT(I) = (ONE+CC * EPSPDT(I))*(ONE-TSTAR**MT)
!--          ... emulating the '-pvctl chgpwr' compiler flag
!--             epspdt(i)= (1.+cc(i) * epspdt(i))*(1.-exp(log(tstar)*mt))
            ELSE
!--          EPSPDT(I)= (1.+CC(I) * EPSPDT(I))*(1.-TSTAR**MT)
!--          ...  in case of tstart<=ZERO => tstar**mt=zero**mt=zero 
!--          ...  => can be skipped
              EPSPDT(I) = (ONE+CC * EPSPDT(I))
            ENDIF
            EPSPDT(I) = MAX(EM20,EPSPDT(I))
            IF (ICC == 1) YMAX(I) = YMAX(I)*EPSPDT(I)
          ENDDO
        ELSEIF (NPIF == JLT .AND. (IMACH /= 3 .OR. IPARIT == 0)) THEN
          DO I=JFT,JLT
            EPSPDT(I) = CC*EXP((-Z3+Z4 * EPSPDT(I))*T(I))
            IF (ICC == 1) YMAX(I) = YMAX(I) + EPSPDT(I)
            CA(I) = CA(I) + EPSPDT(I)
            EPSPDT(I) = ONE
          ENDDO
        ELSE
#include "vectorize.inc"
          DO I=JFT,JLT
            IF (IRTY == 0) THEN
              MT = MAX(EM20,Z3)
              TM = Z4
              TSTAR = (T(I)-TWOHUNDRED98)/(TM-TWOHUNDRED98)
              EPSPDT(I) = MAX(ZERO,EPSPDT(I))
              IF (TSTAR > ZERO) THEN
                EPSPDT(I) = (ONE + CC * EPSPDT(I))*(ONE - TSTAR**MT)
              ELSE
                EPSPDT(I) = (ONE + CC * EPSPDT(I))
              ENDIF
              EPSPDT(I) = MAX(EM20,EPSPDT(I))
              IF (ICC == 1) YMAX(I) = YMAX(I)*EPSPDT(I)
            ELSE
              EPSPDT(I) = CC*EXP((-Z3+Z4 * EPSPDT(I))*T(I))
              IF (ICC == 1) YMAX(I) = YMAX(I) + EPSPDT(I)
              CA(I) = CA(I) + EPSPDT(I)
              EPSPDT(I) = ONE
            ENDIF ! IF (IRTY == 0)
          ENDDO
        ENDIF ! IF (NPIF == ZERO .AND. (IMACH /= 3 .OR. IPARIT == 0))
      ENDIF ! IF (EPIF /= ZERO)
!---------------------------
      IF (IPLA == 0) THEN
!---------------------------
!    RADIAL RETURN
!---------------------------
!-------------
!     CRITERE
!-------------
        DO I=JFT,JLT
          YLD(I) = CA(I)+CB(I)*EXP(CN * LOG(LBUF%PLA(I)+ EM30))
          YLD(I) = MIN(YLD(I)*EPSPDT(I),YMAX(I))
          RR(I)  = MIN(ONE,YLD(I)*UNSYEQ(I))
        ENDDO
!----------------------------
!     ETAN/E POUR COQUES ZENG
!----------------------------
        DO I=JFT,JLT
          IF (RR(I) < ONE) THEN
            IF (YLD(I) >= YMAX(I)) THEN
              HH(I) = ZERO
            ELSE
              HH(I) = CN*CB(I)*EXP((CN-ONE)*LOG(LBUF%PLA(I)+EM30))
            ENDIF
            ETSE(I) = HH(I)/(HH(I)+YM)
          ENDIF
        ENDDO
!--------------
!     ENERGIES
!--------------
        DO I=JFT,JLT
          F1(I) = F1(I)*RR(I)
          F2(I) = F2(I)*RR(I)
          F3(I) = F3(I)*RR(I)
          D1 = F1(I)-SIGOXX(I)
          D2 = F2(I)-SIGOYY(I)
          DWELM(I) = (F1(I)+SIGOXX(I))*(C1*D1+C2*D2)+
     .               (F2(I)+SIGOYY(I))*(C2*D1+C1*D2)+
     .               (F3(I)+SIGOXY(I))*(C3*(F3(I)-SIGOXY(I)))
          DEGMB_LOC(I) = DEGMB_LOC(I)+F1(I)*DEPSXX(I)+F2(I)*DEPSYY(I)
     .                               +F3(I)*DEPSXY(I)
!
          M1(I) = M1(I)*RR(I)
          M2(I) = M2(I)*RR(I)
          M3(I) = M3(I)*RR(I)
          D1 = M1(I)-MOMOXX(I)
          D2 = M2(I)-MOMOYY(I)
          DWELF(I) = TWELVE*(
     .              (M1(I)+MOMOXX(I))*(C1*D1+C2*D2)
     .             +(M2(I)+MOMOYY(I))*(C2*D1+C1*D2)
     .             +(M3(I)+MOMOXY(I))*(C3*(M3(I)-MOMOXY(I))) )
          DEGFX_LOC(I) = DEGFX_LOC(I)+ M1(I)*DEPBXX(I)+M2(I)*DEPBYY(I)
     .                                +M3(I)*DEPBXY(I)
        ENDDO
!
        DO I=JFT,JLT
          DWPLA(I) = DEGMB_LOC(I)+DEGFX_LOC(I)*THK0(I)-DWELM(I)-DWELF(I)
        ENDDO
!-----------------------
!     EPS PLASTIQUE
!-----------------------
        DO I=JFT,JLT
          DPLA(I) = OFF(I)* MAX(ZERO,HALF*EPSPDT(I)*DWPLA(I)/YLD(I))
          LBUF%PLA(I) = LBUF%PLA(I) + DPLA(I)
          AAA  = ABS(DWELM(I)+DWELF(I))
          BBB  = MAX(ZERO,DWPLA(I))
          CCC  = MAX(EM20,AAA+BBB)
          EZZ = - (DEPSXX(I) + DEPSYY(I)) * (NU(I)*AAA/(ONE-NU(I)) + BBB)/CCC
          THK(I) = THK(I) * (ONE + EZZ*OFF(I))
        ENDDO
        IF (VP== 1) THEN
          DO I=1,NEL       
            PLAP1   = DPLA(I)/MAX(EM20,DT1C(I))
            PLAP(I) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
          ENDDO
        ENDIF

      ELSE
!-------------------------
!     ITERATIVE
!-------------------------
!-------------------------
!     CRITERE DE VON MISES
!-------------------------
        DO I=JFT,JLT
          YLD(I) = CA(I)+CB(I)*EXP(CN * LOG(LBUF%PLA(I)+ EM30))
        ENDDO
!
        DO I=JFT,JLT
          YLD(I) = MIN(YLD(I)*EPSPDT(I),YMAX(I))
        ENDDO
!
        DO I=JFT,JLT
!-------------------------------------------------------------------------
!     GAMA (L'INVERSE DE GAMA DANS LA FORMULE) 
!-------------------------------------------------------------------------
          CCC = EXP(-TWOP6666666667*LBUF%PLA(I)*YM/YLD(I))
          GAMA(I) = TWO/(THREE-CCC)
          GAMA2(I)= GAMA(I)*GAMA(I)
          MM1 = THIRTY6*GAMA2(I)
          MM2 = THREEP4641*GAMA(I)  
          QTIER(I) = THREE*GAMA2(I)
          NNU1(I) = NU(I)/(ONE-NU(I))
          S1(I) = (F1(I)+F2(I))*HALF
          S2(I) = (F1(I)-F2(I))*HALF
          S3 = F3(I)
          SM1(I) = (M1(I)+M2(I))*HALF
          SM2(I) = (M1(I)-M2(I))*HALF
          SM3 = M3(I)
          AN(I) = S1(I)*S1(I)
          BN(I) = THREE*(S2(I)*S2(I)+S3*S3)
          AM(I) = SM1(I)*SM1(I)*MM1
          BM(I) = THREE*(SM2(I)*SM2(I)+SM3*SM3)*MM1       
          ANM(I) = S1(I)*SM1(I)*MM2
          BNM(I) = THREE*(S2(I)*SM2(I)+S3*SM3)*MM2
          SVM(I) = SQRT(AN(I)+BN(I)+AM(I)+BM(I)+ABS(ANM(I)+BNM(I)))
          EZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1(I)
          THK(I) = THK(I) * (ONE + EZZ*OFF(I))
        ENDDO
!-------------------------
!     GATHER PLASTIC FLOW
!-------------------------
        NINDX = 0
!
        DO I=JFT,JLT
          IF (SVM(I) > YLD(I) .AND. OFF(I) == ONE) THEN
            NINDX = NINDX+1
            INDEX(NINDX) = I
          ENDIF
        ENDDO
!---------------------------
!    DEP EN CONTRAINTE PLANE
!---------------------------
        DO J=1,NINDX
          I = INDEX(J)
          NU1(I) = HALF*(ONE + NU(I))
          NU2(I) = THREE_HALF *(ONE - NU(I))
          NU3(I) = ONE - NNU1(I)
          NUM1(I) = ONE + QTIER(I)
          NUM2(I) = FIVEP5*GAMA2(I)
          LFN(I) = NUM2(I)
          QFN(I) = SIXTEENP5*GAMA2(I)*GAMA2(I)
          QFNM(I) = -NUM2(I)
          IF (YLD(I) >= YMAX(I)) THEN
            HH(I) = ZERO
          ELSE
            HH(I) = CN*CB(I)*EXP((CN-ONE)*LOG(LBUF%PLA(I)+ EM30))
          ENDIF
          ETSE(I) = HH(I)/(HH(I)+YM)
          DPLA_J(I) = (SVM(I)-YLD(I))/(THREE*G(I)*QTIER(I)+HH(I))
        ENDDO
!-------------------------------
!     TIENT COMPTE DU COUPLAGE
!-------------------------------
        DO N=1,NMAX
          DO J=1,NINDX
            I = INDEX(J)
            DPLA_I = DPLA_J(I)
            YLD_I = YLD(I)+HH(I)*DPLA_I
            DR = A1(I)*DPLA_I/YLD_I
            XP = DR*NU1(I)
            XQ = DR*NU2(I)
            DA = NUM1(I)+NUM2(I)*XP
            DB = NUM1(I)+NUM2(I)*XQ
            DFNP = LFN(I)+QFN(I)*XP
            DFNQ = LFN(I)+QFN(I)*XQ
            DFMP = ONEP8333*(XP+ONE)
            DFMQ = ONEP8333*(XQ+ONE)
            DFNMP = QFNM(I)*XP
            DFNMQ = QFNM(I)*XQ
            XP = HALF*XP
            XQ = HALF*XQ
            A = ONE+(DA+NUM1(I))*XP
            B = ONE+(DB+NUM1(I))*XQ
            A_I = ONE/A
            B_I = ONE/B
            AA = A_I*A_I
            BB = B_I*B_I
            FNP = ONE+(DFNP+LFN(I))*XP
            FNQ = ONE+(DFNP+LFN(I))*XQ
            FMP = ONE+(DFMP+ONEP8333)*XP
            FMQ = ONE+(DFMQ+ONEP8333)*XQ
            FNMP = ONE+DFNMP*XP
            FNMQ = ONE+DFNMQ*XQ
            FNM = AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
            IF (FNM < ZERO) THEN
              S = -ONE
            ELSE
              S = ONE
            ENDIF
            CP1 = (FNP*AN(I)+S*FNMP*ANM(I)+FMP*AM(I))*AA
            CQ1 = (FNQ*BN(I)+S*FNMQ*BNM(I)+FMQ*BM(I))*BB
            CP2 = (DFNP*AN(I)+S*DFNMP*ANM(I)+DFMP*AM(I))*AA
            CQ2 = (DFNQ*BN(I)+S*DFNMQ*BNM(I)+DFMQ*BM(I))*BB
            XPG = TWO*NU1(I)*DA*A_I
            XQG = TWO*NU2(I)*DB*B_I
            F   = CP1 +CQ1-YLD_I*YLD_I
            DF  =(CP2*NU1(I)+CQ2*NU2(I)-CP1*XPG-CQ1*XQG)*
     .           (A1(I)-DR*HH(I))/YLD_I-TWO*HH(I)*YLD_I
            DPLA_J(I) = MAX(ZERO,DPLA_I-F/DF)
          ENDDO
        ENDDO
!------------------------------------------
!     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!------------------------------------------
        DO J=1,NINDX
          I = INDEX(J)
          LBUF%PLA(I) = LBUF%PLA(I) + DPLA_J(I)
          DPLA_I = DPLA_J(I)
          YLD_I = YLD(I)+HH(I)*DPLA_I
          DR = A1(I)*DPLA_I/YLD_I
          XP = DR*NU1(I)
          XQ = DR*NU2(I)
          XPG = XP*XP
          XQG = XQ*XQ
          A = ONE+NUM1(I)*XP+NUM2(I)*XPG
          B = ONE+NUM1(I)*XQ+NUM2(I)*XQG
          A_I = ONE/A
          B_I = ONE/B
          AA = A_I*A_I
          BB = B_I*B_I
          FNMP = ONE+QFNM(I)*XPG
          FNMQ = ONE+QFNM(I)*XQG
          FNM = AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
          IF (FNM  < ZERO) THEN
            S = -ONEP732*GAMA(I)
          ELSE
            S = ONEP732*GAMA(I)
          ENDIF
          QN = ONE+QTIER(I)*XQ
          QNM1 = XQ*S
          QNM2 = QNM1*ONE_OVER_12
          SN1 = (S1(I)*(ONE +QTIER(I)*XP)-SM1(I)*S*XP)*A_I
          SN2 = (S2(I)*QN-SM2(I)*QNM1)*B_I
          S3 = (F3(I)*QN-M3(I)*QNM1)*B_I
          MM1 = (SM1(I)*(ONE+XP)-S1(I)*S*XP*ONE_OVER_12)*A_I
          MM2 = (SM2(I)*(ONE+XQ)-S2(I)*QNM2)*B_I
          M3(I) = (M3(I)*(ONE+XQ)-F3(I)*QNM2)*B_I
          F1(I) = SN1+SN2
          F2(I) = SN1-SN2
          F3(I) = S3
          M1(I) = MM1+MM2
          M2(I) = MM1-MM2
          EZZ = - NU3(I)*DR*SN1/YM
          THK(I) = THK(I) * (ONE + EZZ*OFF(I))
        ENDDO
c     update and filter plastic strain rate for VP=1
        IF (VP== 1) THEN
          DO I=1,NEL       
            PLAP1   = DPLA_J(I)/MAX(EM20,DT1C(I))
            PLAP(I) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
          ENDDO
        ENDIF

      ENDIF ! IF (IPLA == 0)
!--------------------------------
!     TEST DE RUPTURE DUCTILE
!--------------------------------
      DO I=JFT,JLT
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO
!
      NINDX=0
!
      DO I=JFT,JLT
        IF (OFF(I) < ONE) CYCLE
        IF (LBUF%PLA(I) < EPMX) CYCLE
        OFF(I) = FOUR_OVER_5
        NINDX = NINDX+1
        INDX(NINDX) = I
        IOFF_DUCT(I) = 1
      ENDDO
!
      IF (NINDX > 0) THEN
        IDEL7NOK = 1
        IF (INCONV == 1) THEN
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(J))
            WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
          ENDDO
        ENDIF ! (INCONV == 1) THEN
      ENDIF
!---
      IOFC = NINDX
!---
      DO I=JFT,JLT
        SIGNXX(I) = F1(I)
        SIGNYY(I) = F2(I)
        SIGNXY(I) = F3(I)
        SIGNYZ(I) = F4(I)
        SIGNZX(I) = F5(I)
        MOMNXX(I) = M1(I)
        MOMNYY(I) = M2(I)
        MOMNXY(I) = M3(I)
      ENDDO
!---
 1000 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
!---
      RETURN
      END
