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
Chd|  M15CPLRC                      source/materials/mat/mat015/m15cplrc.F
Chd|-- called by -----------
Chd|        SIGEPS15C                     source/materials/mat/mat015/sigeps15c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M15CPLRC(JFT    ,JLT    ,PM     ,WPLA   ,
     2                    IMATLY ,DAMT   ,SHF    ,NGL    ,SIGE  ,
     3                    DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     4                    SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX,
     5                    SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,
     6                    EPSP   ,ISRATE ,NEL    ,TSAIWU )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(MVSIZ),NEL
      INTEGER JFT,JLT, ISRATE
C     REAL
      my_real
     .   PM(NPROPM,*),WPLA(*),DAMT(NEL,2),SHF(*),EPSP(*),
     .   SIGE(MVSIZ,5),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),TSAIWU(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IMATLY,JFLAG,FAIL
      INTEGER ICC(MVSIZ),IFLAG(MVSIZ),FAIL_OLD(MVSIZ)
C     REAL
      my_real
     .   DP1(MVSIZ), DP2(MVSIZ), DP3(MVSIZ),CB(MVSIZ),CN(MVSIZ),
     .   E11(MVSIZ), E22(MVSIZ), NU12(MVSIZ), NU21(MVSIZ),
     .   G12(MVSIZ), G23(MVSIZ), G31(MVSIZ), FMAX(MVSIZ),
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ),
     .   DS1(MVSIZ), DS2(MVSIZ), DS3(MVSIZ), DE(MVSIZ),
     .   DE1(MVSIZ), DE2(MVSIZ), WVEC(MVSIZ), T1(MVSIZ),
     .   T2(MVSIZ), T3(MVSIZ),LAMDA(MVSIZ), COEF(MVSIZ), 
     .   A11(MVSIZ), A12(MVSIZ), A22(MVSIZ),
     .   SO1(MVSIZ), SO2(MVSIZ), SO3(MVSIZ),WPLAMX(MVSIZ),
     .   EPS1T1(MVSIZ), EPS2T1(MVSIZ), SIGRST1(MVSIZ),
     .   EPS1T2(MVSIZ), EPS2T2(MVSIZ), SIGRST2(MVSIZ),
     .   EPS1C1(MVSIZ), EPS2C1(MVSIZ), SIGRSC1(MVSIZ),
     .   EPS1C2(MVSIZ), EPS2C2(MVSIZ), SIGRSC2(MVSIZ),
     .   EPS1T12(MVSIZ), EPS2T12(MVSIZ), SIGRST12(MVSIZ), 
     .   CC(MVSIZ),EPDR(MVSIZ), FYLD(MVSIZ),
     .   F1(MVSIZ), F2(MVSIZ), F12(MVSIZ), F11(MVSIZ), F22(MVSIZ), 
     .   F33(MVSIZ),WPLAREF(MVSIZ),BETA(MVSIZ),SOFT(3),
     .   SCALE, CNN, SCALE1, SCALE2, DAM1, DAM2,SIGYT1,SIGYT2,SIGYC1,
     .   SIGYC2,SIGYT12,ALPHA,STRP12,COEFA,COEFB,DELTA
C=======================================================================       
      DO I=JFT,JLT
        DE(I)     =PM(44,IMATLY)
        E11(I)    =PM(33,IMATLY)
        E22(I)    =PM(34,IMATLY)
        NU12(I)   =PM(35,IMATLY)
        NU21(I)   =PM(36,IMATLY)
        G12(I)    =PM(37,IMATLY)
        G23(I)    =PM(38,IMATLY)
        G31(I)    =PM(39,IMATLY)
        F1(I)     =PM(54,IMATLY)
        F2(I)     =PM(55,IMATLY)
        F11(I)    =PM(56,IMATLY)
        F22(I)    =PM(57,IMATLY)
        F33(I)    =PM(58,IMATLY)
        F12(I)    =PM(59,IMATLY)
        WPLAMX(I) =PM(41,IMATLY)
        WPLAREF(I)=PM(68,IMATLY)
        CB(I)     =PM(46,IMATLY)
        CN(I)     =PM(47,IMATLY)
        FMAX(I)   =PM(49,IMATLY)
        CC(I)     =PM(50,IMATLY)
        EPDR(I)   =PM(51,IMATLY)
        ICC(I)    =NINT(PM(53,IMATLY))
        IF (TT == ZERO) DAMT(I,1)=ONE
        IF (TT == ZERO) DAMT(I,2)=ONE
      ENDDO
C-------------------------------------------------------------------
C     REDUCTION DE SIG SUR CRITERE WPLA_OLD >= WPLAMX
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C     DEFORMATIONS ELASTIQUES
C-----------------------------
      DO I=JFT,JLT
        DE1(I)=ONE
        DE2(I)=ONE
        SCALE =ONE         
        IF (DAMT(I,2) < ONE) THEN
           NU12(I)=EM20
           NU21(I)=EM20 
        ENDIF
        S1(I) = SIGOXX(I)/DE1(I)-NU12(I)*SIGOYY(I)*SCALE
        S2(I) = SIGOYY(I)/DE2(I)-NU21(I)*SIGOXX(I)*SCALE
        S1(I) = S1(I)/E11(I)
        S2(I) = S2(I)/E22(I)
        S3(I) = SIGOXY(I)/DE1(I)/DE2(I)/G12(I)
        S4(I) = SIGOYZ(I)/MAX(DE2(I)*G23(I)*SHF(I),EM30)
        S5(I) = SIGOZX(I)/MAX(DE1(I)*G31(I)*SHF(I),EM30)
      ENDDO
C
      DO I=JFT,JLT
        S1(I)=S1(I)+DEPSXX(I)
        S2(I)=S2(I)+DEPSYY(I)
        S3(I)=S3(I)+DEPSXY(I)
        S4(I)=S4(I)+DEPSYZ(I)
        S5(I)=S5(I)+DEPSZX(I)
      ENDDO
C
      DO I=JFT,JLT
        DE1(I)=ONE
        DE2(I)=ONE
        SCALE1=ONE
        SCALE2 = ONE-NU12(I)*NU21(I)*SCALE1
        IF (DAMT(I,2) < ONE) THEN
           E22(I)=EM20
           NU12(I)=EM20
           NU21(I)=EM20           
        ENDIF
        A11(I) = E11(I)*DE1(I)/SCALE2
        A22(I) = E22(I)*DE2(I)/SCALE2
        A12(I) = NU21(I)*A11(I)*SCALE1
      ENDDO
C-----------------------------
C     CONTRAINTES ELASTIQUES
C-----------------------------
      DO I=JFT,JLT
        T1(I)   = A11(I)*S1(I)+A12(I)*S2(I)
        T2(I)   = A12(I)*S1(I)+A22(I)*S2(I)
        T3(I)   = DE1(I)*DE2(I)*G12(I)*S3(I)
        SIGNYZ(I)= DE2(I)*G23(I)*SHF(I)*S4(I)
        SIGNZX(I)= DE1(I)*G31(I)*SHF(I)*S5(I)
      ENDDO
C-------------------------------------------------------------------
C     STRAIN RATE
C-------------------------------------------------------------------
      DO I=JFT,JLT
        IF (ISRATE == 0) EPSP(I) = MAX(
     .                     ABS(DEPSXX(I)),ABS(DEPSYY(I)),ABS(DEPSXY(I)),
     .                     ABS(DEPSYZ(I)),ABS(DEPSZX(I))) / MAX(DT1,EM30)
        IF (EPSP(I) > EPDR(I)) THEN
          EPSP(I)=LOG(EPSP(I)/EPDR(I))
        ELSE
          EPSP(I)=ZERO
        ENDIF
        COEF(I)=ZERO
      ENDDO
C
      DO I=JFT,JLT
        EPSP(I) = ONE + CC(I) * EPSP(I)
        FYLD(I) = (ONE+CB(I)*WPLA(I)**CN(I))*EPSP(I)
        IF (ICC(I) == 1 .OR. ICC(I) == 3) THEN
          FMAX(I) = FMAX(I)*EPSP(I)
        ENDIF
        IF (ICC(I) == 3 .OR. ICC(I) == 4) THEN
          WPLAMX(I) = WPLAMX(I)*EPSP(I)
        ENDIF
        FYLD(I)= MIN(FMAX(I),FYLD(I))
      ENDDO
C-------------------------------------------------------------------
C     PLASTICITE
C-------------------------------------------------------------------
      DO I=JFT,JLT
        WVEC(I)=F1(I) *T1(I)       + F2(I) *T2(I) +
     .          F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .          F33(I)*T3(I)*T3(I) + TWO*F12(I)*T1(I)*T2(I)
        TSAIWU(I) = MAX(MIN(WVEC(I)/FYLD(I),ONE),TSAIWU(I))
      ENDDO
C
      DO I=JFT,JLT
        CNN=CN(I)-ONE
        WVEC(I)=ZERO
        IF (WPLA(I) > ZERO .AND. FYLD(I) < FMAX(I)) 
     .      WVEC(I)=EPSP(I)*WPLA(I)**CNN
      ENDDO
C
      DO I=JFT,JLT
        BETA(I) = ONE
        SO1(I)=BETA(I)*SIGOXX(I)
        SO2(I)=BETA(I)*SIGOYY(I)
        SO3(I)=BETA(I)*SIGOXY(I)
      ENDDO
C
      DO I=JFT,JLT
        DP1(I)=F1(I)+2*F11(I)*SO1(I)+2*F12(I)*SO2(I)
        DP2(I)=F2(I)+2*F22(I)*SO2(I)+2*F12(I)*SO1(I)
        DP3(I)=2*F33(I)*SO3(I)
      ENDDO
C
      DO I=JFT,JLT
        DS1(I)=T1(I)-SO1(I)
        DS2(I)=T2(I)-SO2(I)
        DS3(I)=T3(I)-SO3(I)
      ENDDO
C 
      DO I=JFT,JLT
        LAMDA(I)=(DP1(I)*DS1(I)+DP2(I)*DS2(I)+DP3(I)*DS3(I))*COEF(I)
        IF (LAMDA(I) == ZERO) CYCLE
        LAMDA(I)=LAMDA(I)*COEF(I)/
     .          (DP1(I)*(A11(I)*DP1(I)+A12(I)*DP2(I))+
     .           DP2(I)*(A12(I)*DP1(I)+A22(I)*DP2(I))+
     .       TWO*DP3(I)*G12(I)*DE1(I)*DE2(I)*DP3(I) +
     .      (SO1(I)*DP1(I)+SO2(I)*DP2(I)+2.*SO3(I)*DP3(I))
     .       *CN(I)*CB(I)*WVEC(I))
      ENDDO
C
      DO I=JFT,JLT
        DP1(I)=LAMDA(I)*DP1(I)
        DP2(I)=LAMDA(I)*DP2(I)
        DP3(I)=LAMDA(I)*DP3(I)
      ENDDO
C
      DO I=JFT,JLT
        T1(I)=T1(I)-A11(I)*DP1(I)-A12(I)*DP2(I)
        T2(I)=T2(I)-A12(I)*DP1(I)-A22(I)*DP2(I)
        T3(I)=T3(I)-G12(I)*DE1(I)*DE2(I)*DP3(I)*TWO
      ENDDO
C
      DO I=JFT,JLT
        WPLA(I)=WPLA(I)+HALF*(DP1(I)*(T1(I)+SO1(I))+
     .                          DP2(I)*(T2(I)+SO2(I))+
     .                     TWO*DP3(I)*(T3(I)+SO3(I)))
        WPLA(I)= MAX(WPLA(I),ZERO) / WPLAREF(I)
      ENDDO
C
      DO I=JFT,JLT
        SIGNXX(I)=T1(I)
        SIGNYY(I)=T2(I)
        SIGNXY(I)=T3(I)
      ENDDO
C-------------------
C     PLASTICITY END
C-------------------
      DO I=JFT,JLT
        SIGE(I,1)=SIGNXX(I)
        SIGE(I,2)=SIGNYY(I)
        SIGE(I,3)=SIGNXY(I)
        SIGE(I,4)=SIGNYZ(I)
        SIGE(I,5)=SIGNZX(I)
      ENDDO
!---
      RETURN
      END
