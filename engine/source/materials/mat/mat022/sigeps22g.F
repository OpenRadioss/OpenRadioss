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
!hd|====================================================================
!hd|  SIGEPS22G                     src/mate22/sigeps22g.F           
!hd|-- called by -----------
!hd|-- calls ---------------
!hd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
!hd|====================================================================
Chd|====================================================================
Chd|  SIGEPS22G                     source/materials/mat/mat022/sigeps22g.F
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS22G(ELBUF_STR,
     1            JFT     ,JLT     ,PM      ,FOR     ,MOM      ,
     2            THK     ,OFF     ,DT1C    ,NU      ,THK0     ,
     3            GS      ,EPSP    ,IOFC    ,INDX    ,NEL      ,
     4            NGL     ,EXZ     ,EYZ     ,MX      ,IOFF_DUCT,
     5            DEGMB   ,DEGFX   ,DEPSXX  ,DEPSYY  ,DEPSXY   ,
     6            DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY  ,DEPBXY   ,
     7            SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8            MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX  ,SIGNYY   ,
     9            SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX  ,MOMNYY   ,
     A            MOMNXY  )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE ELBUFDEF_MOD
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER JFT,JLT,IOFC,NEL,IOFF_DUCT(*),MX
      INTEGER NGL(MVSIZ),INDX(MVSIZ)
      my_real
     .   PM(NPROPM,*),
     .   OFF(*),DT1C(*),GS(*),THK(*),EPSP(MVSIZ)
      my_real
     .   EXZ(MVSIZ),EYZ(MVSIZ),KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),
     .   NU(MVSIZ),THK0(MVSIZ),DEGMB(MVSIZ),DEGFX(MVSIZ)
      my_real
     .   FOR(NEL,5),MOM(NEL,3),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER ICC(MVSIZ),I,J,ICC_1,NINDX
      my_real
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),F4(MVSIZ),F5(MVSIZ),
     .   YM(MVSIZ),FP1(MVSIZ),FP2(MVSIZ),FP3(MVSIZ),
     .   M1(MVSIZ),M2(MVSIZ),M3(MVSIZ),MP1(MVSIZ),MP2(MVSIZ),MP3(MVSIZ),
     .   DWELM(MVSIZ),DWELF(MVSIZ),CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),
     .   EPMX(MVSIZ),YMAX(MVSIZ),YEQ(MVSIZ),DWPLA(MVSIZ),HH(MVSIZ),
     .   RR(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),CC(MVSIZ),
     .   B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),EPDR(MVSIZ),EPSL(MVSIZ),HL(MVSIZ),
     .   YLDL(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),
     .   DEGMB_LOC(MVSIZ),DEGSH_LOC(MVSIZ),DEGFX_LOC(MVSIZ),
     .   ALPE(MVSIZ),YLD(MVSIZ)
      my_real
     .   DEPSL,YMI,A1I,THK12,EZZ,DPLA,AAA,BBB,CCC,
     .   P1,P2,P3,Q1,Q2,Q3,YM_1,A1_1,C1_1,C2_1,C3_1,CA_1,CB_1,
     .   CN_1,EPMX_1,YMAX_1,CC_1,EPSL_1,HL_1,YLDL_1
!
      TYPE(G_BUFEL_) ,POINTER :: GBUF
!-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
!---
      DATA P1/ 0.9659258/, P2/-0.2588190/, P3/ 1.7320508/
      DATA Q1/ 1.1153548/, Q2/+0.2988584/, Q3/ 0.5773503/
!
      YM_1   = PM(20,MX)
      A1_1   = PM(24,MX)
      C1_1   = PM(28,MX)
      C2_1   = PM(29,MX)
      C3_1   = PM(30,MX)
      CA_1   = PM(38,MX)
      CB_1   = PM(39,MX)
      CN_1   = PM(40,MX)
      EPMX_1 = PM(41,MX)
      YMAX_1 = PM(42,MX)
      CC_1   = PM(43,MX)
      EPSL_1 = PM(45,MX)  
      HL_1   = PM(46,MX)
      YLDL_1 = PM(47,MX)
      ICC_1  = NINT(PM(49,MX))
!
      DO I=JFT,JLT
        YM(I)   = YM_1
        A1(I)   = A1_1
        C1(I)   = C1_1
        C2(I)   = C2_1
        C3(I)   = C3_1
        CA(I)   = CA_1
        CB(I)   = CB_1
        CN(I)   = CN_1
        EPMX(I) = EPMX_1
        YMAX(I) = YMAX_1
        CC(I)   = CC_1
        EPDR(I) = MAX(EM20,PM(44,MX)*DT1C(I))
        EPSL(I) = EPSL_1  
        HL(I)   = HL_1
        YLDL(I) = YLDL_1
        ICC(I)  = ICC_1
      ENDDO

!-------------
!     CRITERE
!-------------
      DO I=JFT,JLT
        YLD(I) = CA(I)+CB(I)*EXP(CN(I) * LOG(GBUF%PLA(I)+EM30))
        YLD(I) = MIN(YLD(I),YMAX(I))
        DEPSL  = MAX(ZERO,GBUF%PLA(I)-EPSL(I))
        YLD(I) = MIN(YLD(I),YLDL(I)+HL(I)*DEPSL)
        YLD(I) = MAX(YLD(I),ZERO)
        ALPE(I)= MIN(ONE,YLD(I)/(YLD(I)+YM(I)*DEPSL))
        YMI    = ALPE(I)*YM(I)
        G(I)   = HALF*YMI/(ONE+NU(I))
        A1I    = YMI/(ONE-NU(I)**2)
        ALPE(I)= MAX(EM30,A1I/A1(I))
        A1(I)  = A1I
        A2(I)  = NU(I)*A1(I)
        THK12  = THK0(I)/TWELVE
        B1(I)  = A1(I)*THK12
        B2(I)  = A2(I)*THK12
        B3(I)  = G(I) *THK12
      ENDDO
!
#include "vectorize.inc"
      DO I=JFT,JLT
        DEGSH_LOC(I) = FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I) ! shear only
        DEGMB_LOC(I) = DEGMB(I) - DEGSH_LOC(I) ! (membrane without shear)
        DEGFX_LOC(I) = DEGFX(I) ! bending only
      ENDDO
!---------------------------
!     CONTRAINTES ELASTIQUES
!---------------------------
#include "vectorize.inc"
      DO I=JFT,JLT
        F1(I) = SIGOXX(I) + A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
        F2(I) = SIGOYY(I) + A1(I)*DEPSYY(I)+A2(I)*DEPSXX(I)
        F3(I) = SIGOXY(I) + G(I) *DEPSXY(I)
        F4(I) = SIGOYZ(I) + ALPE(I)*GS(I)*DEPSYZ(I)
        F5(I) = SIGOZX(I) + ALPE(I)*GS(I)*DEPSZX(I)
!
        FP1(I) = P1*F1(I) + P2*F2(I)
        FP2(I) = P2*F1(I) + P1*F2(I)
        FP3(I) = P3*F3(I)
        YEQ(I) = FP1(I)**2 + FP2(I)**2
        YEQ(I) = YEQ(I) + FP3(I)**2
!
        M1(I) = MOMOXX(I) + B1(I)*DEPBXX(I)+B2(I)*DEPBYY(I)
        M2(I) = MOMOYY(I) + B1(I)*DEPBYY(I)+B2(I)*DEPBXX(I)
        M3(I) = MOMOXY(I) + B3(I)*DEPBXY(I)
!
        MP1(I) = (P1*M1(I) + P2*M2(I))*FOUR
        MP2(I) = (P2*M1(I) + P1*M2(I))*FOUR
        MP3(I) =  P3*M3(I) * FOUR
        YEQ(I) = YEQ(I) + MP1(I)**2+MP2(I)**2
        YEQ(I) = SQRT(YEQ(I) + MP3(I)**2)
      ENDDO
!-------------
!     VITESSE DE DEFORMATION
!-------------
      DO I=JFT,JLT
        EPSP(I) = ABS(DEGMB_LOC(I)+DEGFX_LOC(I)*THK0(I))/(YEQ(I)+EM20)
        EPSP(I) = MAX(EPSP(I),EPDR(I))
        YLD(I) = YLD(I)*(ONE+CC(I)*LOG(EPSP(I)/EPDR(I)))
        IF (ICC(I) == 2) YLD(I)= MIN(YLD(I),YMAX(I))
        RR(I) = MIN(ONE,YLD(I)/(YEQ(I)+EM20))
      ENDDO
!--------------
!     ENERGIES
!--------------
#include "vectorize.inc"
      DO I=JFT,JLT
        F1(I) =(Q1*FP1(I) + Q2*FP2(I))*RR(I)
        F2(I) =(Q2*FP1(I) + Q1*FP2(I))*RR(I)
        F3(I) = Q3*FP3(I)*RR(I)
!
        DWELM(I) =(F1(I)+SIGOXX(I))*(C1(I)*(F1(I)-SIGOXX(I))+
     .                               C2(I)*(F2(I)-SIGOYY(I)))+
     .            (F2(I)+SIGOYY(I))*(C2(I)*(F1(I)-SIGOXX(I))+
     .                               C1(I)*(F2(I)-SIGOYY(I)))+
     .            (F3(I)+SIGOXY(I))*(C3(I)*(F3(I)-SIGOXY(I)))
        DEGMB_LOC(I) = DEGMB_LOC(I) + F1(I)*DEPSXX(I)+F2(I)*DEPSYY(I)+F3(I)*DEPSXY(I)
      ENDDO
!
      DO I=JFT,JLT
        M1(I) =(Q1*MP1(I) + Q2*MP2(I))*RR(I)*FOURTH
        M2(I) =(Q2*MP1(I) + Q1*MP2(I))*RR(I)*FOURTH     
        M3(I) = Q3*MP3(I)*RR(I)*FOURTH
!
        DWELF(I) =(M1(I)+MOMOXX(I))*TWELVE*(C1(I)*(M1(I)-MOMOXX(I))+
     .                                     C2(I)*(M2(I)-MOMOYY(I)))+
     .            (M2(I)+MOMOYY(I))*TWELVE*(C2(I)*(M1(I)-MOMOXX(I))+
     .                                     C1(I)*(M2(I)-MOMOYY(I)))+
     .            (M3(I)+MOMOXY(I))*TWELVE*(C3(I)*(M3(I)-MOMOXY(I)))
        DEGFX_LOC(I) = DEGFX_LOC(I) + M1(I)*DEPBXX(I)+M2(I)*DEPBYY(I)+M3(I)*DEPBXY(I)
      ENDDO
!
#include "vectorize.inc"
      DO I=JFT,JLT
        DWPLA(I) = DEGMB_LOC(I) + DEGFX_LOC(I)*THK0(I)-DWELM(I)-DWELF(I)
      ENDDO
!-----------------------
!     EPS PLASTIQUE
!-----------------------
      DO I=JFT,JLT
        DPLA = OFF(I) * MAX(ZERO,HALF*DWPLA(I)/MAX(EM20,YLD(I)))
        GBUF%PLA(I) = GBUF%PLA(I) + DPLA
        AAA  = ABS(DWELM(I)+DWELF(I))
        BBB  = ABS(DWPLA(I))
        CCC  = MAX(EM20,AAA+BBB)
        EZZ = - (DEPSXX(I) + DEPSYY(I)) * (NU(I)*AAA/(ONE-NU(I)) + BBB) / CCC
        THK(I) = THK(I) * (ONE + EZZ*OFF(I))
      ENDDO
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
        IF (OFF(I) < ONE)           CYCLE
        IF (GBUF%PLA(I) < EPMX(I)) CYCLE
        OFF(I) = FOUR_OVER_5
        NINDX = NINDX + 1
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
        ENDIF
      ENDIF ! IF (NINDX > 0)
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
