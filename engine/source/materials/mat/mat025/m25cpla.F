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
Chd|  M25CPLA                       source/materials/mat/mat025/m25cpla.F
Chd|-- called by -----------
Chd|        M25LAW                        source/materials/mat/mat025/m25law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M25CPLA(JFT  ,JLT  ,PM    ,MATLY, NGL ,OFF  ,
     2                   S1  , S2, S3,S4,S5,S6 ,
     3                   D1  , D2, D3, D4,D5,D6,
     4                   EPST , DAMT  ,CRAK ,NFIS1,
     5                   NFIS2,NFIS3,WPLAR ,
     6                   EPSP , WPLA, SIGL, ILAY,FLAY,
     7                   SEQ_OUTPUT,NEL,IPG)
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(MVSIZ),MATLY(*),IPG
      INTEGER JFT, JLT, 
     .   NFIS1(*),NFIS2(*),NFIS3(*),ILAY,NEL
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), OFF(*),  WPLA(*),
     .   DAMT(NEL,2), CRAK(NEL,2),EPSP(*),EPST(NEL,6),
     .   WPLAR(*),
     .   S1(*),S2(*),S3(*),
     .   S4(*),S5(*),S6(*), 
     .   D1(*),D2(*),D3(*),
     .   D4(*),D5(*),D6(*), SIGL(MVSIZ,6),FLAY(*),SEQ_OUTPUT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IMATLY,JFLAG,FAIL, ITER,ILAYER,JYEILD
      INTEGER ICC(MVSIZ),IFLAG(MVSIZ),FAIL_OLD(MVSIZ),IYEILD(MVSIZ)
      INTEGER NINDX,INDEX(MVSIZ),J,ICAS(MVSIZ),JWPLA,IWPLA(MVSIZ)
      INTEGER IOFF(MVSIZ),ID,ISOFT(MVSIZ),IMODWP(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   DP1(MVSIZ), DP2(MVSIZ), DP3(MVSIZ),CB(MVSIZ),CN(MVSIZ),
     .   E11(MVSIZ), E22(MVSIZ), NU12(MVSIZ), NU21(MVSIZ),
     .   G12(MVSIZ), G23(MVSIZ), G31(MVSIZ), FMAX(MVSIZ),
     .   DS1(MVSIZ), DS2(MVSIZ), DS3(MVSIZ), DE(MVSIZ),
     .   DE1(MVSIZ), DE2(MVSIZ), WVEC(MVSIZ), T1(MVSIZ),
     .   T2(MVSIZ), T3(MVSIZ),LAMDA(MVSIZ), COEF(MVSIZ), 
     .   A11(MVSIZ), A12(MVSIZ), A22(MVSIZ),A21(MVSIZ),
     .   SO1(MVSIZ), SO2(MVSIZ), SO3(MVSIZ),WPLAMX(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSM1(MVSIZ),EPSM2(MVSIZ),
     .   EPS1T1(MVSIZ), EPS2T1(MVSIZ), SIGRST1(MVSIZ),
     .   EPS1T2(MVSIZ), EPS2T2(MVSIZ), SIGRST2(MVSIZ),
     .   EPS1C1(MVSIZ), EPS2C1(MVSIZ), SIGRSC1(MVSIZ),
     .   EPS1C2(MVSIZ), EPS2C2(MVSIZ), SIGRSC2(MVSIZ),
     .   EPS1T12(MVSIZ), EPS2T12(MVSIZ), SIGRST12(MVSIZ),
     .   DMAX(MVSIZ),CC(MVSIZ),EPDR(MVSIZ), FYLD(MVSIZ),
     .   F1(MVSIZ), F2(MVSIZ), F12(MVSIZ), F11(MVSIZ), F22(MVSIZ), 
     .   F33(MVSIZ),BETA(MVSIZ),WPLAREF(MVSIZ),
     .   EPSF1(MVSIZ),EPSF2(MVSIZ),E33(MVSIZ),EPS(MVSIZ,6),
     .   SCALE, CNN, SCALE1, SCALE2, DAM1, DAM2,SIGYT1,SIGYT2,SIGYC1,
     .   SIGYC2,SIGYT12,ALPHA,
     .   SOFT(3),STRP12,COEFA,COEFB,DELTA,DWPLA,
     .   E1, E2,E3,E4,E5,E6,WPLAMXT1(MVSIZ),
     .   WPLAMXT2(MVSIZ),WPLAMXC1(MVSIZ),WPLAMXC2(MVSIZ),
     .   WPLAMXT12(MVSIZ),WPLA1,WPLA2,WPLA3
C=======================================================================
      JFLAG=0
      JYEILD = 0
      JWPLA = 0
#include "vectorize.inc"
      DO  I=JFT,JLT
        IMATLY=MATLY(I)
        IFLAG(I)=NINT(PM(40,IMATLY))
        IOFF(I) =NINT(PM(42,IMATLY))
        ICC(I)  =NINT(PM(53,IMATLY))
        IYEILD(I)=NINT(PM(187,IMATLY))
        IWPLA(I)=NINT(PM(188,IMATLY))
        JFLAG   =JFLAG+IFLAG(I)
        JYEILD = JYEILD + IYEILD(I) 
        JWPLA= JWPLA + IWPLA(I)
      ENDDO
#include "vectorize.inc"
      DO  I=JFT,JLT
        IMATLY=MATLY(I)
        DE(I)=PM(44,IMATLY)
        E11(I)  =PM(33,IMATLY)
        E22(I)  =PM(34,IMATLY)
        NU12(I) =PM(35,IMATLY)
        NU21(I) =PM(36,IMATLY)
        G12(I)  =PM(37,IMATLY)
        G23(I)  =PM(38,IMATLY)
        G31(I)  =PM(39,IMATLY)
        F1(I)   =PM(54,IMATLY)
        F2(I)   =PM(55,IMATLY)
        F11(I)  =PM(56,IMATLY)
        F22(I)  =PM(57,IMATLY)
        F33(I)  =PM(58,IMATLY)
        F12(I)  =PM(59,IMATLY)
        WPLAMX(I)=PM(41,IMATLY)
        CB(I)   =PM(46,IMATLY)
        CN(I)   =PM(47,IMATLY)
        FMAX(I) =PM(49,IMATLY)
        CC(I)   =PM(50,IMATLY)
        EPDR(I) =PM(51,IMATLY)
        EPST1(I)=PM(60,IMATLY)
        EPST2(I)=PM(61,IMATLY)
        EPSM1(I)=PM(62,IMATLY)
        EPSM2(I)=PM(63,IMATLY)
        DMAX (I)=PM(64,IMATLY)
        WPLAREF(I)=PM(68,IMATLY)
        EPSF1(I)=PM(98,IMATLY)
        EPSF2(I)=PM(99,IMATLY)
        E33(I)=  PM(186,IMATLY)
        IMODWP(I) =  NINT(PM(189,IMATLY))
C
        ISOFT(I) = 0        
      ENDDO  
            
C-------------------------------------------------------------------
C     old failure
C-----------------------------
#include   "nofusion.inc"
      DO I=JFT,JLT
        FAIL_OLD(I)= 0
        IF(DAMT(I,1) >= DMAX(I))FAIL_OLD(I) = FAIL_OLD(I) + 1
        IF(DAMT(I,2) >= DMAX(I))FAIL_OLD(I) = FAIL_OLD(I) + 2
C
        IF(WPLA(I) < ZERO )THEN
C         Wpla is negative in case of layer already reached failure-p :
          FAIL_OLD(I) = FAIL_OLD(I) + 4
          WPLA(I)     = -WPLA(I)
        END IF
C
        IF(EPST(I,1) >= EPSF1(I) )THEN
          FAIL_OLD(I) = FAIL_OLD(I) + 8
        END IF
        IF(EPST(I,2) >= EPSF2(I) )THEN
          FAIL_OLD(I) = FAIL_OLD(I) + 16
        END IF
      ENDDO
      NINDX=0
#include   "nofusion.inc"
      DO I=JFT,JLT
        IF( FAIL_OLD(I) < 4.OR.
     .     (FAIL_OLD(I) < 8.AND. IOFF(I).LT.0) )THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        END IF
      ENDDO
C-------------------------------------------------------------------
C     REDUCTION DE SIG SUR CRITERE WPLA_OLD >= WPLAMX
C-----------------------------
#include   "nofusion.inc"
      DO I=JFT,JLT
        IF((FAIL_OLD(I) >= 4.AND.IOFF(I) >= 0).OR.
     .      FAIL_OLD(I) >= 8)THEN
         S1(I)=QCINQ*S1(I)
         S2(I)=QCINQ*S2(I)
         S3(I)=QCINQ*S3(I)
         S4(I)=QCINQ*S4(I)
         S5(I)=QCINQ*S5(I)
         S6(I)=QCINQ*S6(I)         
C
         IF(ABS(S1(I)) < EM20) S1(I)=ZERO
         IF(ABS(S2(I)) < EM20) S2(I)=ZERO
         IF(ABS(S3(I)) < EM20) S3(I)=ZERO
         IF(ABS(S4(I)) < EM20) S4(I)=ZERO
         IF(ABS(S5(I)) < EM20) S5(I)=ZERO
         IF(ABS(S6(I)) < EM20) S6(I)=ZERO        
C
         D1(I)=ZERO
         D2(I)=ZERO
         D3(I)=ZERO
         D4(I)=ZERO
         D5(I)=ZERO
         D6(I)=ZERO
        ENDIF
      ENDDO
C-----------------------------------------------
C   CRASURV
C-----------------------------------------------
      IF(JFLAG > 0)THEN
#include "vectorize.inc"
        DO  I=JFT,JLT
          IMATLY=MATLY(I)
          EPS1T1(I)  =PM(166,IMATLY)
          EPS2T1(I)  =PM(167,IMATLY)
          SIGRST1(I) =PM(168,IMATLY)
          WPLAMXT1(I)=PM(169,IMATLY)
          EPS1T2(I)  =PM(170,IMATLY)
          EPS2T2(I)  =PM(171,IMATLY)
          SIGRST2(I) =PM(172,IMATLY)
          WPLAMXT2(I)=PM(173,IMATLY)
          EPS1C1(I)  =PM(174,IMATLY)
          EPS2C1(I)  =PM(175,IMATLY)
          SIGRSC1(I) =PM(176,IMATLY)
          WPLAMXC1(I)=PM(177,IMATLY)
          EPS1C2(I)  =PM(178,IMATLY)
          EPS2C2(I)  =PM(179,IMATLY)
          SIGRSC2(I) =PM(180,IMATLY)
          WPLAMXC2(I)=PM(181,IMATLY)
          EPS1T12(I) =PM(182,IMATLY)
          EPS2T12(I) =PM(183,IMATLY)
          SIGRST12(I)=PM(184,IMATLY)
          WPLAMXT12(I)=PM(185,IMATLY)
        ENDDO
      ENDIF
C-------------------------------------------------------------------
C     DEFORMATIONS ELASTIQUES
C-----------------------------
      DE1(JFT:JLT)  =UN-MAX( ZERO , SIGN(DAMT(JFT:JLT,1),S1(JFT:JLT)) )
      DE2(JFT:JLT)  =UN-MAX( ZERO , SIGN(DAMT(JFT:JLT,2),S2(JFT:JLT)) )
      DO  I=JFT,JLT
         SCALE   =(UNDEMI
     .        +SIGN(UNDEMI,DE1(I)-UN))*(UNDEMI+SIGN(UNDEMI,DE2(I)-UN))
         E1 = S1(I)/DE1(I)/E11(I)  - NU21(I)*S2(I)*SCALE/E22(I)
         E2 = S2(I)/DE2(I)/E22(I)  - NU12(I)*S1(I)*SCALE/E11(I)
         E3 = S3(I)/ E33(I)
cc         E1=E1/E11(I)
cc         E2=E2/E22(I)
         E4=S4(I)/DE1(I)/DE2(I)/G12(I)
         E5=S5(I)/MAX(DE2(I)*G23(I),EM30)
         E6=S6(I)/MAX(DE1(I)*G31(I),EM30)
C
         EPS(I,1)= E1 + D1(I)
         EPS(I,2)= E2 + D2(I)
         EPS(I,3)= E3 + D3(I)         
         EPS(I,4)= E4 + D4(I)
         EPS(I,5)= E5 + D5(I)
         EPS(I,6)= E6 + D6(I)
      ENDDO
C
      EPST(JFT:JLT,1) = EPST(JFT:JLT,1) + D1(JFT:JLT)
      EPST(JFT:JLT,2) = EPST(JFT:JLT,2) + D2(JFT:JLT)
      EPST(JFT:JLT,3) = EPST(JFT:JLT,3) + D3(JFT:JLT)
      EPST(JFT:JLT,4) = EPST(JFT:JLT,4) + D4(JFT:JLT)
      EPST(JFT:JLT,5) = EPST(JFT:JLT,5) + D5(JFT:JLT) 
      EPST(JFT:JLT,6) = EPST(JFT:JLT,6) + D6(JFT:JLT)
C      
#include "vectorize.inc"
      DO J=1,NINDX
        I=INDEX(J)
        IF (DAMT(I,1) > ZERO) THEN
         DAM1=(EPST(I,1)-EPST1(I))/(EPSM1(I)-EPST1(I))
         DAM2= DAM1*EPSM1(I)/EPST(I,1)         
         DAMT(I,1)= MAX(DAMT(I,1),DAM2)
         DAMT(I,1)= MIN(DAMT(I,1),DMAX(I))      
        ENDIF
      ENDDO
C
#include "vectorize.inc"
      DO J=1,NINDX
        I=INDEX(J)
        IF (DAMT(I,2) > ZERO) THEN
         DAM1=(EPST(I,2) - EPST2(I))/(EPSM2(I)-EPST2(I))
         DAM2= DAM1*EPSM2(I)/EPST(I,2)        
         DAMT(I,2)= MAX(DAMT(I,2),DAM2)
         DAMT(I,2)= MIN(DAMT(I,2),DMAX(I))
        ENDIF
      ENDDO 
C
      DO I=JFT,JLT
         DE1(I)=UN- MAX( ZERO , SIGN(DAMT(I,1),S1(I)) )
         DE2(I)=UN- MAX( ZERO , SIGN(DAMT(I,2),S2(I)) )
         SCALE1 =(UNDEMI
     .       +SIGN(UNDEMI,DE1(I)-UN))*(UNDEMI+SIGN(UNDEMI,DE2(I)-UN))
         SCALE2 =UN-NU12(I)*NU21(I)*SCALE1
         A11(I)= E11(I)*DE1(I)/SCALE2
         A22(I)= E22(I)*DE2(I)/SCALE2
         A12(I)=NU21(I)*A11(I)*SCALE1
         A21(I)=NU12(I)*A22(I)*SCALE1
      ENDDO   
C-----------------------------
C     CONTRAINTES ELASTIQUES
C-----------------------------
      DO  I=JFT,JLT
        T1(I) =A11(I)*EPS(I,1)+A12(I)*EPS(I,2)
        T2(I) =A21(I)*EPS(I,1)+A22(I)*EPS(I,2)
        T3(I) =DE1(I)*DE2(I)*G12(I)*EPS(I,4)
        S3(I) =E33(I)*EPS(I,3)
        S5(I) =DE2(I)*G23(I)*EPS(I,5)
        S6(I) =DE1(I)*G31(I)*EPS(I,6)
      ENDDO
C      
      DO I=JFT,JLT
          IF(T1(I) > 0) THEN
            IF(T2(I) > 0) ICAS(I) = 1
            IF(T2(I) < 0) ICAS(I) = 4
          ELSE
            IF(T2(I) > 0) ICAS(I) = 2
            IF(T2(I) < 0) ICAS(I) = 3
          ENDIF
      ENDDO   
C-------------------------------------------------------------------
C     STRAIN RATE
C-------------------------------------------------------------------
      DO I=JFT,JLT
c      IF (ISRATE(I) == 0)EPSP(I) = MAX(
c     .                   ABS(EPS(1,I)),ABS(EPS(2,I)),ABS(EPS(3,I)),
c     .                   ABS(EPS(4,I)),ABS(EPS(5,I))) / MAX(DT1,EM30)
        IF(EPSP(I) > EPDR(I)) THEN
          EPSP(I)=LOG(EPSP(I)/EPDR(I))
        ELSE
          EPSP(I)= ZERO
        ENDIF
         COEF(I)=ZERO
      ENDDO
C-------------------------------------------------------------------
C     JFLAG
C-------------------------------------------------------------------
      IF(JFLAG == JLT)THEN
C-------------------------------------------------------------------
C     IFLAG = 1
C-------------------------------------------------------------------
#include "vectorize.inc"
        DO I=JFT,JLT
         IMATLY=MATLY(I)
         IF(WPLA(I).NE.ZERO) THEN
           SIGYT1 = PM(141,IMATLY)*
     .           (UN+PM(142,IMATLY)*EXP(PM(143,IMATLY)*LOG(WPLA(I))))
           SIGYT2 =PM(146,IMATLY)*
     .           (UN+PM(147,IMATLY)*EXP(PM(148,IMATLY)*LOG(WPLA(I))))
           SIGYC1 =PM(151,IMATLY)*
     .           (UN+PM(152,IMATLY)*EXP(PM(153,IMATLY)*LOG(WPLA(I))))
           SIGYC2 =PM(156,IMATLY)*
     .           (UN+PM(157,IMATLY)*EXP(PM(158,IMATLY)*LOG(WPLA(I))))
           SIGYT12=PM(161,IMATLY)*
     .           (UN+PM(162,IMATLY)*EXP(PM(163,IMATLY)*LOG(WPLA(I))))
         ELSE
           SIGYT1 = PM(141,IMATLY)
           SIGYT2 = PM(146,IMATLY)
           SIGYC1 = PM(151,IMATLY)
           SIGYC2 = PM(156,IMATLY)
           SIGYT12= PM(161,IMATLY)
         ENDIF

         IF(ICC(I) == 1 .OR. ICC(I) == 3)THEN
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
           SIGYT1=SIGYT1*(1.+PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(1.+PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(1.+PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(1.+PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(UN +PM(165,IMATLY)*EPSP(I))
         ELSE
           SIGYT1=SIGYT1*(1.+PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(1.+PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(1.+PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(1.+PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(UN +PM(165,IMATLY)*EPSP(I))
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
         ENDIF
C-------------------------------------------------------------------
C     SOFTENING 
C-------------------------------------------------------------------
         SOFT(1)=ZERO
         SOFT(2)=ZERO
         SOFT(3)=ZERO
         ISOFT(I) = 0 
C Direction 1
         IF(EPST(I,1) <= -EPS1C1(I)) THEN
          SOFT(1)=MIN(UN,(EPST(I,1)+EPS1C1(I))/(EPS1C1(I)-EPS2C1(I)))
         ELSEIF(EPS(I,1) >= EPS1T1(I)) THEN
          SOFT(1)=MIN(UN,(EPST(I,1)-EPS1T1(I))/(EPS2T1(I)-EPS1T1(I)))         
         ENDIF
         SIGYT1=MIN(SIGYT1,(UN -SOFT(1))*SIGYT1+SOFT(1)*SIGRST1(I))
         SIGYC1=MIN(SIGYC1,(UN -SOFT(1))*SIGYC1+SOFT(1)*SIGRSC1(I))
C Direction 2        
         IF(EPST(I,2) <= -EPS1C2(I)) THEN
          SOFT(2)=MIN(UN,(EPST(I,2)+EPS1C2(I))/(EPS1C2(I)-EPS2C2(I)))
         ELSEIF(EPST(I,2) >= EPS1T2(I)) THEN
          SOFT(2)=MIN(UN,(EPST(I,2)-EPS1T2(I))/(EPS2T2(I)-EPS1T2(I)))
         ENDIF
         SIGYT2=MIN(SIGYT2,(UN -SOFT(2))*SIGYT2+SOFT(2)*SIGRST2(I))
         SIGYC2=MIN(SIGYC2,(UN -SOFT(2))*SIGYC2+SOFT(2)*SIGRSC2(I))
C Direction 12
         STRP12 = UNDEMI*EPST(I,4)
         IF(STRP12 <= - EPS1T12(I)) THEN
         SOFT(3) = MIN(UN,(STRP12 + EPS1T12(I))/(EPS1T12(I)-EPS2T12(I)))
         ELSEIF(STRP12 >= EPS1T12(I)) THEN
          SOFT(3)=MIN(UN,( STRP12 - EPS1T12(I))/(EPS2T12(I)-EPS1T12(I)))
         ENDIF
         SIGYT12=MIN(SIGYT12,(UN -SOFT(3))*SIGYT12+SOFT(3)*SIGRST12(I))         
C         
         IF(SOFT(1) /= ZERO .OR. SOFT(2) /= ZERO .OR. SOFT(3) /= ZERO) 
     .         ISOFT(I) =1
C
         F1(I)  = UN/SIGYT1-UN/SIGYC1
         F2(I)  = UN/SIGYT2-UN/SIGYC2
         F11(I) = UN/(SIGYT1*SIGYC1)
         F22(I) = UN/(SIGYT2*SIGYC2)
         F33(I) = UN/(SIGYT12*SIGYT12)	 
         ALPHA=F12(I)
         F12(I) = -ALPHA/(DEUX*SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2))
C         
         IF(IYEILD(I) > 0) THEN
           IF(ICAS(I) == 1)THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
         ENDIF
         EPSP(I)=UN + CC(I) * EPSP(I)
         IF(ICC(I) == 3.OR.ICC(I) == 4)THEN
           WPLAMX(I) = WPLAMX(I)*EPSP(I)
         ENDIF
         FYLD(I)= UN
         FMAX(I)= UN	 
        ENDDO
      ELSEIF(JFLAG > 0)THEN
C-------------------------------------------------------------------
C     IFLAG = 0 OR 1
C-------------------------------------------------------------------
#include "vectorize.inc"
       DO I=JFT,JLT
        IF(IFLAG(I) == 1)THEN
         IMATLY=MATLY(I)
         IF(WPLA(I) /= ZERO)THEN
           SIGYT1 =PM(141,IMATLY)*
     .        (UN+PM(142,IMATLY)*EXP(PM(143,IMATLY)*LOG(WPLA(I))))
           SIGYT2 =PM(146,IMATLY)*
     .        (UN+PM(147,IMATLY)*EXP(PM(148,IMATLY)*LOG(WPLA(I))))
           SIGYC1 =PM(151,IMATLY)*
     .        (UN+PM(152,IMATLY)*EXP(PM(153,IMATLY)*LOG(WPLA(I))))
           SIGYC2 =PM(156,IMATLY)*
     .        (UN+PM(157,IMATLY)*EXP(PM(158,IMATLY)*LOG(WPLA(I))))
           SIGYT12=PM(161,IMATLY)*
     .        (UN+PM(162,IMATLY)*EXP(PM(163,IMATLY)*LOG(WPLA(I))))
         ELSE
           SIGYT1 =PM(141,IMATLY)
           SIGYT2 =PM(146,IMATLY)
           SIGYC1 =PM(151,IMATLY)
           SIGYC2 =PM(156,IMATLY)
           SIGYT12=PM(161,IMATLY)
         ENDIF

         IF(ICC(I) == 1.OR.ICC(I) == 3)THEN
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
           SIGYT1=SIGYT1*(UN +PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(UN +PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(UN +PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(UN +PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(UN+PM(165,IMATLY)*EPSP(I))	   
         ELSE
           SIGYT1=SIGYT1*(UN +PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(UN +PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(UN +PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(UN +PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(UN +PM(165,IMATLY)*EPSP(I))	   
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
         ENDIF
C
         SOFT(1)=ZERO
         SOFT(2)=ZERO
         SOFT(3)=ZERO	 
C Direction 1
         IF(EPST(I,1) <= -EPS1C1(I)) THEN
          SOFT(1)=MIN(UN,(EPST(I,1)+EPS1C1(I))/(EPS1C1(I)-EPS2C1(I)))
         ELSEIF(EPST(I,1) >= EPS1T1(I)) THEN
          SOFT(1)=MIN(UN,(EPST(I,1)-EPS1T1(I))/(EPS2T1(I)-EPS1T1(I)))         
         ENDIF
         SIGYT1=MIN(SIGYT1,(UN-SOFT(1))*SIGYT1+SOFT(1)*SIGRST1(I))
         SIGYC1=MIN(SIGYC1,(UN-SOFT(1))*SIGYC1+SOFT(1)*SIGRSC1(I))
C Direction 2        
         IF(EPST(I,2) <= -EPS1C2(I)) THEN
          SOFT(2)=MIN(UN,(EPST(I,2)+EPS1C2(I))/(EPS1C2(I)-EPS2C2(I)))
         ELSEIF(EPST(I,2) >= EPS1T2(I)) THEN
          SOFT(2)=MIN(UN,(EPST(I,2)-EPS1T2(I))/(EPS2T2(I)-EPS1T2(I)))
         ENDIF
         SIGYT2=MIN(SIGYT2,(UN-SOFT(2))*SIGYT2+SOFT(2)*SIGRST2(I))
         SIGYC2=MIN(SIGYC2,(UN-SOFT(2))*SIGYC2+SOFT(2)*SIGRSC2(I))
C Direction 12
         STRP12 = UNDEMI*EPST(I,4)
         IF(STRP12 <= -EPS1T12(I)) THEN
          SOFT(3)=MIN(UN,(STRP12 + EPS1T12(I))/(EPS1T12(I)-EPS2T12(I)))
         ELSEIF(STRP12 >= EPS1T12(I)) THEN
          SOFT(3)=MIN(UN,(STRP12 - EPS1T12(I))/(EPS2T12(I)-EPS1T12(I)))
         ENDIF
         SIGYT12=MIN(SIGYT12,(UN -SOFT(3))*SIGYT12+SOFT(3)*SIGRST12(I))
         F1(I)  = UN/SIGYT1-UN/SIGYC1
         F2(I)  = UN/SIGYT2-UN/SIGYC2
         F11(I) = UN/(SIGYT1*SIGYC1)
         F22(I) = UN/(SIGYT2*SIGYC2)
         F33(I) = UN/(SIGYT12*SIGYT12)	 
         ALPHA=F12(I)
         F12(I) = -ALPHA/(DEUX*SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2))
C
         IF(IYEILD(I) > 0) THEN
           IF(ICAS(I) == 1)THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
         ENDIF
         EPSP(I)=UN + CC(I) * EPSP(I)
         IF(ICC(I) == 3.OR.ICC(I) == 4)THEN
           WPLAMX(I) = WPLAMX(I)*EPSP(I)
         ENDIF
         FYLD(I)= UN
         FMAX(I)= UN
        ELSE
         EPSP(I)=UN + CC(I) * EPSP(I)
         IF (WPLA(I).NE.ZERO) THEN
            FYLD(I)=(UN +CB(I)*EXP(CN(I)*LOG(WPLA(I))))*EPSP(I)
         ELSE
            FYLD(I)=EPSP(I)
         ENDIF
         IF(ICC(I) == 1.OR.ICC(I) == 3)THEN
           FMAX(I) = FMAX(I)*EPSP(I)
         ENDIF
         IF(ICC(I) == 3.OR.ICC(I) == 4)THEN
           WPLAMX(I) = WPLAMX(I)*EPSP(I)
         ENDIF
         FYLD(I)= MIN(FMAX(I),FYLD(I))
        ENDIF
         IF(IYEILD(I) > 0) THEN
           ALPHA = F12(I)
           SIGYT1 =PM(141,IMATLY)
           SIGYT2 =PM(146,IMATLY)
           SIGYC1 =PM(151,IMATLY)
           SIGYC2 =PM(156,IMATLY)
           SIGYT12=PM(161,IMATLY)
c            ALPHA(I) =  
           IF(ICAS(I) == 1)THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
         ENDIF        
       ENDDO
      ELSE
C-------------------------------------------------------------------
C     IFLAG = 0
C-------------------------------------------------------------------
       DO I=JFT,JLT
        EPSP(I)=UN + CC(I) * EPSP(I)
        IF (WPLA(I) /= ZERO) THEN
           FYLD(I)=(UN+CB(I)*EXP(CN(I)*LOG(WPLA(I))))*EPSP(I)
        ELSE
           FYLD(I)=EPSP(I)
        ENDIF
        IF(ICC(I) == 1.OR.ICC(I) == 3)THEN
          FMAX(I) = FMAX(I)*EPSP(I)
        ENDIF
        IF(ICC(I) == 3.OR.ICC(I) == 4)THEN
          WPLAMX(I) = WPLAMX(I)*EPSP(I)
        ENDIF
        FYLD(I)= MIN(FMAX(I),FYLD(I))
C
        IF(IYEILD(I) > 0) THEN
           ALPHA = F12(I)
           SIGYT1 =PM(141,IMATLY)
           SIGYT2 =PM(146,IMATLY)
           SIGYC1 =PM(151,IMATLY)
           SIGYC2 =PM(156,IMATLY)
           SIGYT12=PM(161,IMATLY)
c            ALPHA(I) =  
           IF(ICAS(I) == 1)THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYT2*SIGYT2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= UN/(SIGYC1*SIGYC1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= UN/(SIGYT1*SIGYT1)
             F22(I)= UN/(SIGYC2*SIGYC2)
             F33(I)= UN/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
         ENDIF        
       ENDDO
      ENDIF
C-------------------------------------------------------------------
C     PLASTICITE
C-------------------------------------------------------------------
      IF(JYEILD == 0) THEN
         DO  I=JFT,JLT
           WVEC(I)=  F1(I) *T1(I)      + F2(I) *T2(I) +
     .               F11(I)*T1(I)*T1(I)+ F22(I)*T2(I)*T2(I) +
     .               F33(I)*T3(I)*T3(I)+ DEUX*F12(I)*T1(I)*T2(I)
!!c    equiv crit for output
!!            SEQ_OUTPUT(I) = WVEC(I)
         ENDDO
C
        DO I=JFT,JLT
          IF (WVEC(I) > FYLD(I).AND.OFF(I)  ==  UN) COEF(I)=UN    
          CNN=CN(I)-UN
          WVEC(I)=ZERO
          IF(WPLA(I) > ZERO.AND.FYLD(I) < FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
C
          DO 110 I=JFT,JLT
            BETA(I)= UN
            IF(COEF(I) == ZERO .OR. IFLAG(I) == 0) GO TO 110
            COEFA=F11(I)*S1(I)*S1(I) + F22(I)*S2(I)*S2(I) +
     .            F33(I)*S4(I)*S4(I) + DEUX*F12(I)*S1(I)*S2(I)
            COEFB=F1(I)*S1(I)+F2(I)*S2(I)
            DELTA=COEFB*COEFB + QUATRE*COEFA*FYLD(I) 
            IF(DELTA.LT.ZERO)THEN
             IF(IMCONV == 1)THEN
#include "lockon.inc"
             WRITE(IOUT,3000) DELTA
#include "lockoff.inc" 
             ENDIF
             GO TO 110
            ELSE 
              DELTA=SQRT(DELTA)
            ENDIF
            IF(ABS(COEFA) <= EM15) THEN
              IF(ABS(COEFB) <= EM15) THEN
               IF(IMCONV == 1)THEN
#include "lockon.inc"
                 WRITE(IOUT,3010) COEFB
#include "lockoff.inc"
               ENDIF
                 GO TO 110
                ELSE 
                 BETA(I)=FYLD(I)/COEFB
              ENDIF
            ENDIF
            IF(ABS(UN+(COEFB-DELTA)/DEUX/COEFA) <= 
     .         ABS(UN+(COEFB+DELTA)/DEUX/COEFA)) THEN
             BETA(I)=(-COEFB+DELTA)/DEUX/COEFA
            ELSE
             BETA(I)=(-COEFB-DELTA)/DEUX/COEFA
            ENDIF
            DELTA=F1(I) *BETA(I)*S1(I) +F2(I) *BETA(I)*S2(I) +
     .            F11(I)*BETA(I)*S1(I)*BETA(I)*S1(I) +
     .            F22(I)*BETA(I)*S2(I)*BETA(I)*S2(I) +
     .            F33(I)*BETA(I)*S4(I)*BETA(I)*S4(I) +
     .            DEUX*F12(I)*BETA(I)*S1(I)*BETA(I)*S2(I)
C--1-----|-|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9--10---|
 3000       FORMAT('No real solution for DELTA =',F11.4)
 3010       FORMAT('Cannot project stresses on criteria, COEFB =',F11.4)

 110    CONTINUE   
        DO I=JFT,JLT
C        BETA(I)=1.
          SO1(I)=BETA(I)*S1(I)
          SO2(I)=BETA(I)*S2(I)
          SO3(I)=BETA(I)*S4(I)
        ENDDO
C
        DO  I=JFT,JLT
          DP1(I)=F1(I)+DEUX*F11(I)*SO1(I)+DEUX*F12(I)*SO2(I)
          DP2(I)=F2(I)+DEUX*F22(I)*SO2(I)+DEUX*F12(I)*SO1(I)
          DP3(I)=DEUX*F33(I)*SO3(I)
        ENDDO
CCCC        
      ELSEIF(JYEILD == JLT) THEN
C------------------------------------------------------------
C MODIFID TSIA HILL CRITERIA        
C ----------------------------------------------------------
        DO I=JFT,JLT
           WVEC(I)= F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .              F33(I)*T3(I)*T3(I) + F12(I)*T1(I)*T2(I)
!!c    equiv crit for output
!!            SEQ_OUTPUT(I) = WVEC(I)
        ENDDO
C
        DO I=JFT,JLT
          IF (WVEC(I).GT.FYLD(I).AND.OFF(I).EQ.UN) COEF(I)=UN    
          CNN=CN(I)-UN
          WVEC(I)=ZERO
          IF(WPLA(I).GT.ZERO.AND.FYLD(I).LT.FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
C
Cst CRASURV +++
        DO  111 I=JFT,JLT                                                       
          BETA(I)=UN                                                           
          IF(COEF(I).EQ.ZERO.OR.IFLAG(I).EQ.0) GO TO 111                       
           COEFA= F11(I)*S1(I)*S1(I) + F22(I)*S2(I)*S2(I) + 
     .            F33(I)*S4(I)*S4(I) + F12(I)*S1(I)*S2(I)
           DELTA = FYLD(I) / COEFA
          IF(DELTA .LT.ZERO)THEN                                                
             IF(IMCONV.EQ.1)THEN 
#include "lockon.inc"                                                        
               WRITE(IOUT,3000) DELTA  
#include "lockoff.inc"                                                       
              ENDIF
              GO TO 111
          ELSE 
CC
           DELTA = SQRT(DELTA)
           IF(ABS(COEFA).LE.EM15) THEN                                        
                IF(IMCONV.EQ.1)THEN
#include "lockon.inc"                                                        
                   WRITE(IOUT,3010) COEFA
#include "lockoff.inc"                                                       
                ENDIF 
                GO TO 111                            
            ENDIF                                                              
          ENDIF                                                                
          IF(ABS(UN-DELTA).LE.                              
     .       ABS(UN+DELTA)) THEN                            
           BETA(I)= DELTA                             
          ELSE                                                                 
           BETA(I)=-DELTA                                  
          ENDIF
 111    CONTINUE                                    
        DO I=JFT,JLT
C        BETA(I)=1.
          SO1(I)=BETA(I)*S1(I)
          SO2(I)=BETA(I)*S2(I)
          SO3(I)=BETA(I)*S4(I)
        ENDDO
Cst CRASURV ---
C
        DO  I=JFT,JLT
          DP1(I)=DEUX*F11(I)*SO1(I) +  F12(I)*SO2(I)
          DP2(I)=DEUX*F22(I)*SO2(I) +  F12(I)*SO1(I)
          DP3(I)=DEUX*F33(I)*SO3(I)
        ENDDO      
      
      ELSE
C------------------------------------------------------------
C MODIFID TSIA HILL CRITERIA     OR TSAI-WU CRITERIA   
C ----------------------------------------------------------        
        DO I=JFT,JLT
          IF(IYEILD(I) > 0) THEN
             WVEC(I)= F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I)+
     .              F33(I)*T3(I)*T3(I) + F12(I)*T1(I)*T2(I)
           ELSE
             WVEC(I)=  F1(I) *T1(I)       + F2(I) *T2(I) +
     .                 F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .                 F33(I)*T3(I)*T3(I) + DEUX*F12(I)*T1(I)*T2(I)
           ENDIF
!!c    equiv crit for output
!!           SEQ_OUTPUT(I) = WVEC(I)
        ENDDO      
C
        DO I=JFT,JLT
          IF (WVEC(I).GT.FYLD(I).AND.OFF(I).EQ.UN) COEF(I)=UN    
          CNN=CN(I)-UN
          WVEC(I)=ZERO
          IF(WPLA(I).GT.ZERO.AND.FYLD(I).LT.FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
CC
        DO  112 I=JFT,JLT                                                       
          BETA(I)=UN                                                           
          IF(COEF(I).EQ.ZERO.OR.IFLAG(I).EQ.0) GO TO 112 
           IF(IYEILD(I) > 0) THEN     
C--------------------------------------------------------------
C  TSAI HILL
C--------------------------------------------------------------
             COEFA =F11(I)*S1(I)*S1(I) + F22(I)*S2(I)*S2(I)+ 
     .              F33(I)*S4(I)*S4(I) + F12(I)*S1(I)*S2(I)
             DELTA = FYLD(I) / COEFA
             IF(DELTA .LT.ZERO)THEN  
               IF(IMCONV.EQ.1)THEN 
#include "lockon.inc"                                                        
                WRITE(IOUT,3000) DELTA 
#include "lockoff.inc"                                                       
               ENDIF
                GO TO 112
             ELSE 
C
              DELTA = SQRT(DELTA)
               IF(ABS(COEFA).LE.EM15) THEN 
                  IF(IMCONV.EQ.1)THEN
#include "lockon.inc"                                                        
                   WRITE(IOUT,3010) COEFA
#include "lockoff.inc"                                                       
                 ENDIF 
                 GO TO 112                           
               ENDIF 
             ENDIF 
             IF(ABS(UN-DELTA).LE. ABS(UN+DELTA)) THEN
                BETA(I)= DELTA                             
              ELSE 
                 BETA(I)=-DELTA                                  
              ENDIF
C             
             SO1(I)=BETA(I)*S1(I)
             SO2(I)=BETA(I)*S2(I)
             SO3(I)=BETA(I)*S4(I)
C             
             DP1(I)= DEUX*F11(I)*SO1(I) +  F12(I)*SO2(I)
             DP2(I)= DEUX*F22(I)*SO2(I) +  F12(I)*SO1(I)
             DP3(I)= DEUX*F33(I)*SO3(I)
          ELSE
C--------------------------------------------------------------------
C TSAI WU
C --------------------------------------------------------------------         
            COEFA=F11(I)*S1(I)*S1(I) + F22(I)*S2(I)*S2(I) +
     .            F33(I)*S4(I)*S4(I) + DEUX*F12(I)*S1(I)*S2(I)
            COEFB=F1(I)*S1(I)+F2(I)*S2(I)
            DELTA=COEFB*COEFB + QUATRE*COEFA*FYLD(I)                         
            IF(DELTA.LT.ZERO)THEN 
              IF(IMCONV.EQ.1)THEN 
#include "lockon.inc"                                                        
               WRITE(IOUT,3000) DELTA 
#include "lockoff.inc"                                                       
              ENDIF 
              GO TO 112 
             ELSE 
              DELTA=SQRT(DELTA)
          ENDIF                                                                
          IF(ABS(COEFA).LE.EM15) THEN                                          
            IF(ABS(COEFB).LE.EM15) THEN                                        
             IF(IMCONV.EQ.1)THEN                                               
#include "lockon.inc"                                                        
               WRITE(IOUT,3010) COEFB                                          
#include "lockoff.inc"                                                       
             ENDIF                                                             
               GO TO 112                                                       
              ELSE                                                             
               BETA(I)=FYLD(I)/COEFB                                           
            ENDIF                                                              
          ENDIF                                                                
          IF(ABS(UN+(COEFB-DELTA)/DEUX/COEFA).LE.                              
     .       ABS(UN+(COEFB+DELTA)/DEUX/COEFA)) THEN                            
           BETA(I)=(-COEFB+DELTA)/DEUX/COEFA                                   
          ELSE                                                                 
           BETA(I)=(-COEFB-DELTA)/DEUX/COEFA                                   
          ENDIF
C
           SO1(I)=BETA(I)*S1(I)
           SO2(I)=BETA(I)*S2(I)
           SO3(I)=BETA(I)*S4(I)         
C
           DP1(I)=F1(I)+DEUX*F11(I)*SO1(I)+DEUX*F12(I)*SO2(I)
           DP2(I)=F2(I)+DEUX*F22(I)*SO2(I)+DEUX*F12(I)*SO1(I)
           DP3(I)=DEUX*F33(I)*SO3(I)
C                     
         ENDIF   
CC
 112    CONTINUE                   
C fin de des cirterias
      ENDIF
CCCc   
      DO  I=JFT,JLT
        DS1(I)=T1(I)-SO1(I)
        DS2(I)=T2(I)-SO2(I)
        DS3(I)=T3(I)-SO3(I)
      ENDDO
C 
      DO  I=JFT,JLT
         LAMDA(I)=(DP1(I)*DS1(I)+DP2(I)*DS2(I)+DP3(I)*DS3(I))*COEF(I)
         IF(LAMDA(I) /= ZERO) THEN
           LAMDA(I)=LAMDA(I)*COEF(I)/ 
     .        (DP1(I)*(A11(I)*DP1(I)+A12(I)*DP2(I))+
     .         DP2(I)*(A12(I)*DP1(I)+A22(I)*DP2(I))+
     .      DEUX*DP3(I)*G12(I)*DE1(I)*DE2(I)*DP3(I)+
     .        (SO1(I)*DP1(I)+SO2(I)*DP2(I)+DEUX*SO3(I)*DP3(I))
     .         *CN(I)*CB(I)*WVEC(I) )
         ENDIF
      ENDDO
C
      DO  I=JFT,JLT
        DP1(I)=LAMDA(I)*DP1(I)
        DP2(I)=LAMDA(I)*DP2(I)
        DP3(I)=LAMDA(I)*DP3(I)
      ENDDO
C
      DO  I=JFT,JLT
         T1(I)=T1(I)-A11(I)*DP1(I)-A12(I)*DP2(I)
         T2(I)=T2(I)-A12(I)*DP1(I)-A22(I)*DP2(I)
         T3(I)=T3(I)-G12(I)*DE1(I)*DE2(I)*DP3(I)*DEUX
      ENDDO
C
	DO  I=JFT,JLT                  
          DWPLA = UNDEMI*
     .      (DP1(I)*(T1(I)+SO1(I))+
     .       DP2(I)*(T2(I)+SO2(I))+
     .       DEUX*DP3(I)*(T3(I)+SO3(I)))
          DWPLA = MAX(DWPLA ,ZERO) / WPLAREF(I)
          WPLA(I)=WPLA(I)+ DWPLA
        ENDDO
C
C  for crusurv
C
      IF(JFLAG > 0 .AND. JWPLA > 0) THEN
        DO I=JFT,JLT  
            SIGYT1 = PM(141,IMATLY)
            SIGYT2 = PM(146,IMATLY)
            SIGYC1 = PM(151,IMATLY)
            SIGYC2 = PM(156,IMATLY)
            SIGYT12= PM(161,IMATLY)
            IF(IFLAG(I) == 1 .AND. IWPLA(I) > 0 .AND. 
     .         (IMODWP(I) == 0 .OR. (IMODWP(I)> 0 .AND. ISOFT(I) == 0 ))) THEN
C Direction 1       
               WPLA1 = EP30
               IF(T1(I) >= SIGYT1 ) THEN
                   WPLA1 = WPLAMXT1(I)
                   ID = 1
                ELSEIF(T1(I) <= -SIGYC1) THEN
                    WPLA1 = WPLAMXC1(I)
                    ID =-1
                ENDIF 
C                             
               IF(WPLA1 < WPLAMX(I)) THEN
                  WPLAMX(I) = WPLA1
                  ICAS(I) = ID
               ENDIF
C Direction 2             
               WPLA2 = EP30
               IF(T2(I) >= SIGYT2  ) THEN
                  WPLA2 = WPLAMXT2(I)
                  ID =  2
               ELSEIF(T2(I) <= -SIGYC2)THEN
                  WPLA2 = WPLAMXC2(I)
                  ID = -2
               ENDIF
               IF(WPLA2 < WPLAMX(I) ) THEN
                 WPLAMX(I) = WPLA2
                 ICAS(I) = ID
               ENDIF
C shear 
               WPLA3 = EP30
               IF(ABS(T3(I)) >= SIGYT12) THEN
                  WPLA3 = WPLAMXT12(I)
                  ID = 3
               ENDIF          
               IF( WPLA3 < WPLAMX(I) )THEN             
                 WPLAMX(I) = WPLA3
                 ICAS(I) = ID
               ENDIF
          ELSEIF( IFLAG(I) == 1 .AND. IWPLA(I) > 0 .AND. IMODWP(I) > 0 ) THEN  
               ID = 1
               IF(ABS(T2(I)) > ABS(T1(I)))THEN
                  ID = 2
                  IF(ABS(T3(I)) > ABS(T2(I))) ID = 3
               ELSEIF( ABS(T3(I)) > ABS(T1(I))) THEN
                  ID = 3
               ENDIF
               SELECT CASE(ID)
                 CASE(1)
                  ICAS(I) = SIGN(UN,T1(I)) 
                  IF(ICAS(I) > 0 ) THEN
                     WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT1(I))
                  ELSE
                     WPLAMX(I)  = MIN(WPLAMX(I),WPLAMXC1(I))
                  ENDIF 
                 CASE(2)
                  ICAS(I) = SIGN(DEUX,T2(I)) 
                  IF(ICAS(I) > 0 ) THEN
                    WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT2(I))
                  ELSE
                    WPLAMX(I) = MIN(WPLAMX(I),WPLAMXC2(I))
                  ENDIF 
                 CASE(3)
                   ICAS(I) = 3 
                   WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT12(I))
               END SELECT                
           ENDIF ! iflag == 1 ...
        ENDDO 
      ENDIF 
      DO I=JFT,JLT
        IF(WPLA(I) >= WPLAMX(I)) WPLAR(I) = WPLAR(I)+UN
        IF(WPLA(I) >= WPLAMX(I).OR.
     .     DAMT(I,1) >= DMAX(I).OR.
     .     EPST(I,1) >= EPSF1(I)) NFIS1(I)=NFIS1(I)+1
        IF(WPLA(I) >= WPLAMX(I).OR.
     .     DAMT(I,2) >= DMAX(I).OR.
     .     EPST(I,3) >= EPSF2(I)) NFIS2(I)=NFIS2(I)+1
        IF(WPLA(I) >= WPLAMX(I).OR.
     .     DAMT(I,1) >= DMAX(I).OR.
     .     EPST(I,1) >= EPSF1(I).OR.
     .     DAMT(I,2) >= DMAX(I).OR.
     .     EPST(I,2) >= EPSF2(I)) NFIS3(I)=NFIS3(I)+1
      ENDDO
C
C-------------------------------------------------------------------
C     failure
C-----------------------------
      DO I=JFT,JLT
        FAIL=0
        IF(DAMT(I,1) >= DMAX(I))FAIL = FAIL + 1
        IF(DAMT(I,2) >= DMAX(I))FAIL = FAIL + 2
        IF(WPLA(I) >= WPLAMX(I))FAIL = FAIL + 4
        IF(EPST(I,1) >= EPSF1(I))FAIL=FAIL+8
        IF(EPST(I,2) >= EPSF2(I))FAIL=FAIL+16
        IF(FAIL /= FAIL_OLD(I).AND.OFF(I) == UN) THEN
          FAIL = FAIL - FAIL_OLD(I)
         IF(IMCONV == 1)THEN
#include "lockon.inc"
          IF(FAIL == 1.OR.FAIL == 3.OR.FAIL == 5) THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-1 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT 
               FLAY(I) =  UN
          ENDIF
          IF(FAIL == 2.OR.FAIL == 3.OR.FAIL == 6) THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-2 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT 
               FLAY(I) =  UN
          ENDIF   
          IF(FAIL == 4.OR.FAIL == 5.OR.FAIL == 6) THEN
               FLAY(I) =  UN
             IF(ICAS(I) == 0)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT 
             ELSEIF(ICAS(I) == 1)THEN
                WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P-T1 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT
             ELSEIF(ICAS(I) == -1)THEN
                WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P-C1 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT
             ELSEIF(ICAS(I) == 2)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P-T2 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT
             ELSEIF(ICAS(I) == -2)THEN
                WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P-C2 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT 
             ELSEIF(ICAS(I) == 3)THEN
                WRITE(IOUT, '(A,I10,A,I3,A,I3,X,A,1PE11.4)')
     +         ' FAILURE-P-T12 ELEMENT #',NGL(I),
     +         ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +         ', TIME=',TT
            ENDIF                             
           ENDIF      
          IF(FAIL >= 16)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' TOTAL FAILURE-2 ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT 
            FLAY(I) =  UN    
          ELSEIF(FAIL >= 8)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' TOTAL FAILURE-1 ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT 
            FLAY(I) =  UN    
          END IF
#include "lockoff.inc"
         ENDIF
        ENDIF
C       Wpla negative for stresses reduction at next cycle in case of
C       failure-p :
        IF(WPLA(I) >= WPLAMX(I).OR.MOD(FAIL_OLD(I),8) >= 4)
     .                WPLA(I)=-WPLA(I)
      ENDDO      
      DO  I=JFT,JLT
       S1(I)=T1(I)
       S2(I)=T2(I)
       S4(I)=T3(I)
       SIGL(I,1) = S1(I)
       SIGL(I,2) = S2(I)
       SIGL(I,3) = S3(I)
       SIGL(I,3) = S4(I)
       SIGL(I,5) = S5(I)
       SIGL(I,6) = S6(I)  
      ENDDO
C-------------------
C     PLASTICITY END
C-------------------
C-------------------------------------------------------------------
C     RETOUR DANS LE REPERE COQUE
C-----------------------------

      RETURN
      END
