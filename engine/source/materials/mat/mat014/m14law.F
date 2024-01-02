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
Chd|====================================================================
Chd|  M14LAW                        source/materials/mat/mat014/m14law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M14AMA                        source/materials/mat/mat014/m14ama.F
Chd|        M14FTG                        source/materials/mat/mat014/m14ftg.F
Chd|        M14GTF                        source/materials/mat/mat014/m14gtf.F
Chd|====================================================================
      SUBROUTINE M14LAW(
     1   PM,        OFF,       SIG,       EINT,
     2   PLA,       SIGF,      EPSF,      DAM,
     3   EPE,       EPC,       A,         VOL,
     4   RX,        RY,        RZ,        SX,
     5   SY,        SZ,        MAT,       VNEW,
     6   DVOL,      SSP,       D1,        D2,
     7   D3,        D4,        D5,        D6,
     8   SOLD1,     SOLD2,     SOLD3,     SOLD4,
     9   SOLD5,     SOLD6,     SIGY,      DEFP,
     A   NGL,       SEQ_OUTPUT,NEL,       TSAIWU,
     B   JCVT,      JSPH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: JSPH
      INTEGER MAT(MVSIZ),NGL(MVSIZ),NEL
C     REAL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), PLA(*), SIGF(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),
     .   EPSF(*), DAM(NEL,5), EPE(NEL,3), EPC(NEL,3), A(MVSIZ,6), VOL(*),
     .   VNEW(MVSIZ), DVOL(MVSIZ), SSP(MVSIZ), 
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ),D4(MVSIZ),D5(MVSIZ),D6(MVSIZ),
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ), SOLD4(MVSIZ),
     .   SOLD5(MVSIZ), SOLD6(MVSIZ),SIGY(*),DEFP(*),SEQ_OUTPUT(*),
     .   TSAIWU(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KD1(MVSIZ), KD2(MVSIZ), KD3(MVSIZ), 
     .   KD4(MVSIZ),ICC(MVSIZ),ICC_1,
     .   I, IDAM, KDX, MX, NINDX, INDEX(MVSIZ), J
C     REAL
      my_real
     .   AX(MVSIZ), AY(MVSIZ), AZ(MVSIZ), BX(MVSIZ), BY(MVSIZ), 
     .   BZ(MVSIZ), CX(MVSIZ), CY(MVSIZ),CZ(MVSIZ),
     .   WVEC(MVSIZ),S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   T1(MVSIZ), T2(MVSIZ), T3(MVSIZ),T4(MVSIZ),T5(MVSIZ),T6(MVSIZ),
     .   E1(MVSIZ), E2(MVSIZ), E3(MVSIZ),E4(MVSIZ),E5(MVSIZ),E6(MVSIZ), 
     .   HARD(MVSIZ), SIGMY(MVSIZ), ALPHA(MVSIZ), EFIB(MVSIZ),
     .   EPSFT(MVSIZ), EPSFC(MVSIZ), SIGEQ(MVSIZ), P(MVSIZ),
     .   D11(MVSIZ), D12(MVSIZ), D13(MVSIZ), D22(MVSIZ),
     .   D23(MVSIZ), D33(MVSIZ), G12(MVSIZ), G23(MVSIZ), G31(MVSIZ),
     .   C11(MVSIZ), C22(MVSIZ), C33(MVSIZ), C12(MVSIZ), C23(MVSIZ),
     .   C13(MVSIZ), F11(MVSIZ), F22(MVSIZ), F44(MVSIZ), F55(MVSIZ),
     .   F12(MVSIZ), F23(MVSIZ), F1(MVSIZ), F2(MVSIZ), F4(MVSIZ),
     .   F5(MVSIZ), DELTA(MVSIZ), SO1(MVSIZ),
     .   SO2(MVSIZ), SO3(MVSIZ), SO4(MVSIZ), SO5(MVSIZ), SO6(MVSIZ),
     .   DS1(MVSIZ), DS2(MVSIZ), DS3(MVSIZ), DS4(MVSIZ), DS5(MVSIZ),
     .   DS6(MVSIZ), DP1(MVSIZ), DP2(MVSIZ), DP3(MVSIZ), DP4(MVSIZ),
     .   DP5(MVSIZ), DP6(MVSIZ), LAMDA(MVSIZ), COEF(MVSIZ), PLAS(MVSIZ),
     .   CN(MVSIZ), CB(MVSIZ), CNN(MVSIZ), SIGT1(MVSIZ), SIGT2(MVSIZ),
     .   SIGT3(MVSIZ),CC(MVSIZ),EPDR(MVSIZ)
      my_real
     .   FIB, CA, SIGMX, EPSP, DT5 ,SIGYM,
     .   CB_1,CN_1,CC_1,EPDR_1,
     .   F1_1,F2_1,F4_1,F5_1,F11_1,
     .   F22_1,F44_1,F55_1,F12_1,F23_1,
     .   C11_1,C22_1,C33_1,C12_1,C23_1,
     .   C13_1,D11_1,D12_1,D13_1,D22_1,
     .   D23_1,D33_1,G12_1,G23_1,G31_1,
     .   ALPHA_1,WPLAREF,DWPLA
C=======================================================================
      IF (NCYCLE==0 .AND. IRUN==1) THEN
        DO I=1,NEL
          KD1(I) = 0
          KD2(I) = 0
          KD3(I) = 0
          KD4(I) = 0
        ENDDO
      ELSE
        DO I=1,NEL
          IDAM   = INT(DAM(I,5))-10000
          KD1(I) = IDAM/1000
          KDX    = IDAM - KD1(I)*1000
          KD2(I) = KDX/100
          KDX    = KDX - KD2(I)*100
          KD3(I) = KDX/10
          KD4(I) = KDX - KD3(I)*10
        ENDDO
      ENDIF
C------------------
C     FIBER CONTENT
C------------------
      FIB = ZERO
      MX = MAT(1)
      ALPHA_1 = PM(39,MX)
!
      DO I=1,NEL
        ALPHA(I)=ALPHA_1
        FIB=FIB+ALPHA(I)
      ENDDO
C
C--------------------------------------------
C     STRESS TRANSFORMATION (GLOBAL -> FIBER)
C--------------------------------------------
        CALL M14AMA(
     1   PM,      A,       RX,      RY,
     2   RZ,      SX,      SY,      SZ,
     3   AX,      AY,      AZ,      BX,
     4   BY,      BZ,      CX,      CY,
     5   CZ,      NEL,     JCVT,    JSPH)
        CALL M14GTF(SIG,AX ,AY ,AZ ,BX ,BY,  
     2              BZ ,CX ,CY ,CZ ,D1 ,D2,
     3              D3 ,D4 ,D5 ,D6 ,T1 ,T2,
     4              T3 ,T4 ,T5 ,T6 ,E1 ,E2,
     5              E3 ,E4 ,E5 ,E6 ,NEL)
C---------------------------------
C     MATERIAL PROPERTIES
C---------------------------------
      NINDX=0
      MX     =MAT(1)
      CB_1   =PM(26,MX)
      CN_1   =PM(27,MX)
      CC_1   =PM(50,MX)
      EPDR_1 =PM(51,MX)
      ICC_1  =NINT(PM(52,MX))
!
      DO I=1,NEL
        CA      =PM(25,MX)
        CB(I)   =CB_1
        CN(I)   =CN_1
        CC(I)   =CC_1
        EPDR(I) =EPDR_1
        ICC(I)  =ICC_1
        EPSP = MAX(ABS(E1(I)),ABS(E2(I)),ABS(E3(I)),ABS(E4(I)),ABS(E5(I)))
        IF (EPSP>EPDR(I) .AND. CC(I)/=ZERO) THEN
          EPSP=ONE + CC(I) * LOG(EPDR(I)/EPSP)
        ELSE
         EPSP=ONE
        ENDIF
        IF (ICC(I)==1) THEN
          SIGMX   =PM(28,MX)*EPSP
        ELSEIF (ICC(I)==2) THEN
          SIGMX   =PM(28,MX)
        ELSEIF (ICC(I)==3) THEN
          SIGMX   =PM(28,MX)*EPSP
C        WPLAMX(I) = WPLAMX(I)*EPSP
        ELSEIF (ICC(I)==4) THEN
          SIGMX   =PM(28,MX)
C        WPLAMX(I) = WPLAMX(I)*EPSP
        ENDIF
        CB(I)   =CB(I)*EPSP
        CA      =CA*EPSP
        SIGMY(I)= MIN(SIGMX,CA+CB(I)*PLA(I)**CN(I))
        IF (SIGMY(I)==SIGMX .AND. OFF(I)==ONE) THEN
          OFF(I)=ZEP99
          KD4(I)=2
          IDEL7NOK = 1
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
        SIGT1(I)=PM(33,MX)
        SIGT2(I)=PM(34,MX)
        SIGT3(I)=PM(35,MX)
        SSP(I)  =PM(49,MX)
        DELTA(I)=PM(78,MX)
        ENDDO
C
        IF (NINDX/=0) THEN
          DO J=1,NINDX
            I=INDEX(J)
#include "lockon.inc"
            WRITE(IOUT,1000) NGL(I)
#include "lockoff.inc"
          END DO
        END IF
C
      DO I=1,NEL
        E1(I)=E1(I)*DT1
        E2(I)=E2(I)*DT1
        E3(I)=E3(I)*DT1
        E4(I)=E4(I)*DT1
        E5(I)=E5(I)*DT1
        E6(I)=E6(I)*DT1
      ENDDO
C
      DO I=1,NEL
        EPE(I,1)=EPE(I,1)+E1(I)
        EPE(I,2)=EPE(I,2)+E2(I)
        EPE(I,3)=EPE(I,3)+E3(I)
      ENDDO
C
      MX    =MAT(1)
      F1_1  =PM(59,MX)
      F2_1  =PM(60,MX)
      F4_1  =PM(61,MX)
      F5_1  =PM(62,MX)
      F11_1 =PM(63,MX)
      F22_1 =PM(64,MX)
      F44_1 =PM(65,MX)
      F55_1 =PM(66,MX)
      F12_1 =PM(67,MX)
      F23_1 =PM(68,MX)
C
      C11_1 =PM(69,MX)
      C22_1 =PM(73,MX)
      C33_1 =PM(74,MX)
      C12_1 =PM(75,MX)
      C23_1 =PM(76,MX)
      C13_1 =PM(77,MX)
C
      D11_1 =PM(40,MX)
      D12_1 =PM(41,MX)
      D13_1 =PM(42,MX)
      D22_1 =PM(43,MX)
      D23_1 =PM(44,MX)
      D33_1 =PM(45,MX)
      G12_1 =PM(46,MX)
      G23_1 =PM(47,MX)
      G31_1 =PM(48,MX)
C
      DO I=1,NEL
        F1(I)  = F1_1
        F2(I)  = F2_1
        F4(I)  = F4_1
        F5(I)  = F5_1
        F11(I) = F11_1
        F22(I) = F22_1
        F44(I) = F44_1
        F55(I) = F55_1
        F12(I) = F12_1
        F23(I) = F23_1
C
        C11(I) = C11_1
        C22(I) = C22_1
        C33(I) = C33_1
        C12(I) = C12_1
        C23(I) = C23_1
        C13(I) = C13_1
C
        D11(I) = D11_1
        D12(I) = D12_1
        D13(I) = D13_1
        D22(I) = D22_1
        D23(I) = D23_1
        D33(I) = D33_1
        G12(I) = G12_1
        G23(I) = G23_1
        G31(I) = G31_1
      ENDDO
C-------------------------
C     NEW ELASTIC STRESSES
C-------------------------
      DO I=1,NEL
        SO1(I)=T1(I)
        SO2(I)=T2(I)
        SO3(I)=T3(I)
        SO4(I)=T4(I)
        SO5(I)=T5(I)
        SO6(I)=T6(I)
      ENDDO
C
      DO I=1,NEL
        T1(I)=T1(I)+D11(I)*E1(I)+D12(I)*E2(I)+D13(I)*E3(I)
        T2(I)=T2(I)+D12(I)*E1(I)+D22(I)*E2(I)+D23(I)*E3(I)
        T3(I)=T3(I)+D13(I)*E1(I)+D23(I)*E2(I)+D33(I)*E3(I)
        T4(I)=T4(I)+G12(I)*E4(I)
        T5(I)=T5(I)+G23(I)*E5(I)
        T6(I)=T6(I)+G31(I)*E6(I)
      ENDDO
C------------------
C     FIBERS STRESS
C------------------
      IF (FIB>0) THEN
        MX = MAT(1)
        DO I=1,NEL
          EFIB(I) = PM(36,MX)
          EPSFT(I)= PM(37,MX)
          EPSFC(I)= PM(38,MX) 
          EPSF(I) = EPSF(I)+ E1(I)
          SIGF(I) = EFIB(I)*EPSF(I) 
        ENDDO
C
C DEACTIVATED (DAM4 AND DAM5 TEMPORARILY USED FOR GENERAL DAMAGE INFO)
C           CALL M14FIB(PM,SIGF,EPSF,DAM)
      ENDIF
C-------------------------
C     GENERAL FAILURE TEST
C-------------------------
      DO I=1,NEL
        IF (OFF(I)<EM01) OFF(I)=ZERO
        IF (OFF(I)<ONE)   OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C-------------------
C     TENSION DAMAGE
C-------------------
      DO I=1,NEL
C------------------------
C     DIRECTION 1 (FIBER)
C------------------------
        WVEC(I)=(ONE-DAM(I,1))*SIGT1(I)
        IF (T1(I)>WVEC(I)) THEN
C
          IF (EPC(I,1)==ZERO) THEN
            EPC(I,1) = MAX(EPE(I,1),ZERO)
          ELSE
            EPC(I,1) = MAX(EPC(I,1)+E1(I),ZERO)
          ENDIF
C
          T1(I)=WVEC(I)
          T2(I)=T2(I)-D12(I)*E1(I)*DAM(I,1)
          T3(I)=T3(I)-D13(I)*E1(I)*DAM(I,1)
          IF (KD1(I)==0) THEN
            KD1(I)=1
C*          WRITE(6,6001) NCYCLE,I+NFT,'1/1',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
          DAM(I,1)= MIN((DAM(I,1)+DELTA(I)),ONE)
          IF (DAM(I,1)>=1. .AND. KD1(I)/=2) THEN
            KD1(I)=2
C*          WRITE(6,6001) NCYCLE,I+NFT,'1/2',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
        ENDIF
C
        IF( E1(I)<ZERO .AND. DAM(I,1)>ZERO)
     .    EPC(I,1)= MAX(EPC(I,1)+E1(I),ZERO)
C----------------
C     DIRECTION 2
C----------------
        WVEC(I)=(ONE-DAM(I,2))*SIGT2(I)
        IF (T2(I)>WVEC(I)) THEN
C
          IF (EPC(I,2)==ZERO) THEN
            EPC(I,2)= MAX(EPE(I,2),ZERO)
          ELSE
            EPC(I,2)= MAX(EPC(I,2)+E2(I),ZERO)
          ENDIF
C
          T1(I)=T1(I)-D12(I)*E2(I)*DAM(I,2)
          T2(I)=WVEC(I)
          T3(I)=T3(I)-D23(I)*E2(I)*DAM(I,2)
          IF (KD2(I)==0) THEN
            KD2(I)=1
C*          WRITE(6,6001) NCYCLE,I+NFT,'2/1',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
          DAM(I,2)= MIN((DAM(I,2)+DELTA(I)),ONE)
          IF (DAM(I,2)>=ONE.AND. KD2(I)/=2) THEN
            KD2(I)=2
C*          WRITE(6,6001) NCYCLE,I+NFT,'2/2',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
        ENDIF
C
        IF (E2(I)<ZERO .AND. DAM(I,2)>ZERO)
     .    EPC(I,2)= MAX(EPC(I,2)+E2(I),ZERO)
C----------------
C     DIRECTION 3
C----------------
        WVEC(I)=(ONE-DAM(I,3))*SIGT3(I)
        IF (T3(I)>WVEC(I)) THEN
C
          IF (EPC(I,3)==ZERO) THEN
            EPC(I,3)= MAX(EPE(I,3),ZERO)
          ELSE
            EPC(I,3)= MAX(EPC(I,3)+E3(I),ZERO)
          ENDIF
C
          T1(I)=T1(I)-D13(I)*E3(I)*DAM(I,3)
          T2(I)=T2(I)-D23(I)*E3(I)*DAM(I,3)
          T3(I)=WVEC(I)
          IF (KD3(I)==0) THEN
            KD3(I)=1
C*          WRITE(6,6001) NCYCLE,I+NFT,'3/1',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
          DAM(I,3)= MIN((DAM(I,3)+DELTA(I)),ONE)
          IF (DAM(I,3)>=1. .AND. KD3(I)/=2) THEN
            KD3(I)=2
C*          WRITE(6,6001) NCYCLE,I+NFT,'3/2',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
        ENDIF
C
        IF (E3(I)<ZERO .AND. DAM(I,3)>ZERO)
     .    EPC(I,3)= MAX(EPC(I,3)+E3(I),ZERO)
C
      ENDDO
C-----------------------------------
C     CRACK OPEN --> NO COMPRESSION
C-----------------------------------
      DO I=1,NEL
        IF (T1(I)<ZERO.AND.EPC(I,1)>ZERO) THEN
          T1(I)=ZERO
          T2(I)=T2(I)-D12(I)*E1(I)*DAM(I,1)
          T3(I)=T3(I)-D13(I)*E1(I)*DAM(I,1)
        ENDIF
        IF (T2(I)<ZERO.AND.EPC(I,2)>ZERO) THEN
          T1(I)=T1(I)-D12(I)*E2(I)*DAM(I,2)
          T2(I)=ZERO
          T3(I)=T3(I)-D23(I)*E2(I)*DAM(I,2)
        ENDIF
        IF (T3(I)<ZERO.AND.EPC(I,3)>ZERO) THEN
          T3(I)=ZERO
          T1(I)=T1(I)-D13(I)*E3(I)*DAM(I,3)
          T2(I)=T2(I)-D23(I)*E3(I)*DAM(I,3)
        ENDIF
      ENDDO
C---------------------
C     PLASTICITY START
C---------------------
      DO I=1,NEL
        WVEC(I)=F1(I)*T1(I)+F2(I)*(T2(I)+T3(I))+
     .          F11(I)*T1(I)*T1(I)+F22(I)*(T2(I)*T2(I)+T3(I)*T3(I))+
     .          F55(I)*T5(I)*T5(I)+F44(I)*(T4(I)*T4(I)+T6(I)*T6(I))+
     .          TWO*F12(I)*(T1(I)*T2(I)+T1(I)*T3(I))+2*F23(I)*T2(I)*T3(I)
        DAM(I,4)=WVEC(I)
        TSAIWU(I)=MAX(MIN(WVEC(I)/SIGMY(I),ONE),TSAIWU(I))
!!        SEQ_OUTPUT(I) = WVEC(I)
      ENDDO
C
      DO I=1,NEL
        COEF(I)=ZERO
        IF (WVEC(I)>SIGMY(I).AND.OFF(I)==ONE) THEN
          COEF(I)=ONE
          IF (KD4(I)==0) THEN
            KD4(I)=1
C*          WRITE(6,6001) NCYCLE,I+NFT,'4/1',
C*     1                  T1(I),T2(I),T3(I),T4(I),T5(I),T6(I)
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        DP1(I)=F1(I)+TWO*F11(I)*SO1(I)+TWO*F12(I)*(SO2(I)+SO3(I))
        DP2(I)=F2(I)+TWO*F22(I)*SO2(I)+TWO*F12(I)*SO1(I)
     .                            + SO3(I)*F23(I)*TWO
        DP3(I)=F2(I)+TWO*F22(I)*SO3(I)+TWO*F12(I)*SO1(I)
     .                           +   SO2(I)*F23(I)*TWO
        DP4(I)=TWO*F44(I)*SO4(I)
        DP5(I)=TWO*F55(I)*SO5(I)
        DP6(I)=TWO*F44(I)*SO6(I)
      ENDDO
C     WRITE(31,3101) WVEC(1),SIGMY(1),T1(1),T2(1),T3(1),T4(1),T5(1),
C    1T6(1)
 3101 FORMAT(/' 205 WVEC SIGMY T1-T6 ',2E11.4/6E11.4)
C     WRITE(31,3102) F1(1),F2(1),F11(1),F22(1),F12(1),F23(1),F44(1),
C    1F55(1)
 3102 FORMAT(' F1 F2 F11 F22 F12 F23 F44 F55',3E11.4/5E11.4)
C
      DO I=1,NEL
        DS1(I)=T1(I)-SO1(I)
        DS2(I)=T2(I)-SO2(I)
        DS3(I)=T3(I)-SO3(I)
        DS4(I)=T4(I)-SO4(I)
        DS5(I)=T5(I)-SO5(I)
        DS6(I)=T6(I)-SO6(I)
      ENDDO
C
      DO I=1,NEL
        LAMDA(I)=(DP1(I)*DS1(I)+DP2(I)*DS2(I)+DP3(I)*DS3(I)
     .           +DP4(I)*DS4(I)+DP5(I)*DS5(I)+DP6(I)*DS6(I))*COEF(I)
      ENDDO
C     WRITE(31,3103) LAMDA(1),DS1(1),DS2(1),DS3(1),DS4(1),DS5(1),
C    1 DS6(1)
 3103 FORMAT(' 207 LAMDA DS1-DS6 ',E11.4/6E11.4)
C
      DO I=1,NEL
        CNN(I)=CN(I)-ONE
      ENDDO
C
      DO I=1,NEL
        PLAS(I)=ONE
        IF (PLA(I)>ZERO) PLAS(I)=PLA(I)**CNN(I)
      ENDDO
C
      DO 208 I=1,NEL
        IF (LAMDA(I)==ZERO) GO TO 208
        LAMDA(I)=LAMDA(I)*COEF(I)/
     .        (DP1(I)*(D11(I)*DP1(I)+D12(I)*DP2(I)+D13(I)*DP3(I))+
     .         DP2(I)*(D12(I)*DP1(I)+D22(I)*DP2(I)+D23(I)*DP3(I))+
     .         DP3(I)*(D13(I)*DP1(I)+D23(I)*DP2(I)+D33(I)*DP3(I))+
     .         TWO*DP4(I)*G12(I)*DP4(I)+
     .         TWO*DP5(I)*G23(I)*DP5(I)+
     .         TWO*DP6(I)*G31(I)*DP6(I)+
     .        (SO1(I)*DP1(I)+SO2(I)*DP2(I)+SO3(I)*DP3(I)+
     .         TWO*SO4(I)*DP4(I)+2.*SO5(I)*DP5(I)+2.*SO6(I)*DP6(I))
     .        *CN(I)*CB(I)*PLAS(I))
 208  CONTINUE
C     WRITE(31,3104) LAMDA(1)
 3104 FORMAT(' 208 LAMDA ',E11.4)
C
      DO I=1,NEL
        DP1(I)=LAMDA(I)*DP1(I)
        DP2(I)=LAMDA(I)*DP2(I)
        DP3(I)=LAMDA(I)*DP3(I)
        DP4(I)=LAMDA(I)*DP4(I)
        DP5(I)=LAMDA(I)*DP5(I)
        DP6(I)=LAMDA(I)*DP6(I)
      ENDDO
C     WRITE(31,3105) PLA(1),DP1(1),DP2(1),DP3(1),DP4(1),DP5(1),DP6(1)
 3105 FORMAT(' 209 PLA DP1-DP6',E11.4/6E11.4)
C
      DO I=1,NEL
        EPE(I,1)=EPE(I,1)-DP1(I)
        EPE(I,2)=EPE(I,2)-DP2(I)
        EPE(I,3)=EPE(I,3)-DP3(I)
      ENDDO
C
      DO I=1,NEL
        T1(I)=T1(I)-D11(I)*DP1(I)-D12(I)*DP2(I)-D13(I)*DP3(I)
        T2(I)=T2(I)-D12(I)*DP1(I)-D22(I)*DP2(I)-D23(I)*DP3(I)
        T3(I)=T3(I)-D13(I)*DP1(I)-D23(I)*DP2(I)-D33(I)*DP3(I)
        T4(I)=T4(I)-G12(I)*DP4(I)*TWO
        T5(I)=T5(I)-G23(I)*DP5(I)*TWO
        T6(I)=T6(I)-G31(I)*DP6(I)*TWO
      ENDDO
C
      MX  = MAT(1)
      WPLAREF  = PM(98  ,MX)  
      DO I=1,NEL
        DWPLA = HALF*
     .        (DP1(I)*(T1(I)+SO1(I))+
     .         DP2(I)*(T2(I)+SO2(I))+
     .         DP3(I)*(T3(I)+SO3(I))+
     .      TWO*DP4(I)*(T4(I)+SO4(I))+
     .      TWO*DP5(I)*(T5(I)+SO5(I))+
     .      TWO*DP6(I)*(T6(I)+SO6(I)))
        DWPLA = MAX(DWPLA ,ZERO) / WPLAREF
        PLA(I) = PLA(I) + DWPLA  
        PLA(I)= MAX(PLA(I),ZERO)
      ENDDO
C-------------------
C     PLASTICITY END
C-------------------
C--------------------------------------------
C     STRESS TRANSFORMATION (FIBER -> GLOBAL)
C--------------------------------------------
        CALL M14FTG(SIG,AX ,AY ,AZ ,BX ,BY ,  
     2              BZ ,CX ,CY ,CZ ,T1 ,T2 ,
     3              T3 ,T4 ,T5 ,T6 ,NEL)
C
      DO I=1,NEL
        SIG(I,1)=SIG(I,1)*OFF(I)
        SIG(I,2)=SIG(I,2)*OFF(I)
        SIG(I,3)=SIG(I,3)*OFF(I)
        SIG(I,4)=SIG(I,4)*OFF(I)
        SIG(I,5)=SIG(I,5)*OFF(I)
        SIG(I,6)=SIG(I,6)*OFF(I)
      ENDDO
C--------------------
C     INTERNAL ENERGY
C--------------------
      DT5=HALF*DT1
      DO I=1,NEL
        EINT(I)=EINT(I)+DT5*VNEW(I)*
     .           ( D1(I)*(SOLD1(I)+SIG(I,1))
     .           + D2(I)*(SOLD2(I)+SIG(I,2))
     .           + D3(I)*(SOLD3(I)+SIG(I,3))
     .           + D4(I)*(SOLD4(I)+SIG(I,4))
     .           + D5(I)*(SOLD5(I)+SIG(I,5))
     .           + D6(I)*(SOLD6(I)+SIG(I,6)))
        EINT(I)=EINT(I)/VOL(I)
        DAM(I,5)=KD1(I)*1000 + KD2(I)*100 + KD3(I)*10 + KD4(I) + 10000
      ENDDO
C
      DO I=1,NEL
       SIGYM = MAX(EM20,HALF*(ONE/F11(I)+ONE/F22(I)))
       SIGY(I)=SIGMY(I)*SQRT(SIGYM)
       DEFP(I)=PLA(I)
      ENDDO 
C
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 6001 FORMAT(I6,' FAIL(14)   E(',I5,')   M(',A3,')   T(',6E10.3,')')
C---
      RETURN
      END
