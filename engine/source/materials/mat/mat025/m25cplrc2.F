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
Chd|  M25CPLRP2                     source/materials/mat/mat025/m25cplrc2.F
Chd|-- called by -----------
Chd|        SIGEPS25CP                    source/materials/mat/mat025/sigeps25cp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M25CPLRP2(JFT    ,JLT    ,WPLAREF,THK    ,OFF    ,ETSE   ,
     2                     WPLA   ,DIR    ,NPT    ,CC     ,EPDR   ,ICC    ,
     3                     WWPLA  ,SHF    ,FMAX   ,CB     ,CN     ,NEL    ,
     4                     DEGMB  ,F1     ,F2     ,F12    ,F11    ,F22    ,
     5                     F33    ,E11    ,E22    ,NU12   ,NU21   ,G12    ,
     6                     G23    ,G31    ,DE     ,EPSP   ,ISRATE ,SIGY   ,
     7                     DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,SIGOXX ,
     8                     SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,SIGNXX ,SIGNYY ,
     9                     SIGNXY ,SIGNYZ ,SIGNZX ,TSAIWU )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NPT,ICC(*), ISRATE,NEL
C     REAL
      my_real
     .   THK(*), OFF(*), WPLA(*), DIR(*),CC(*),
     .   EPDR(*),WWPLA(*),SHF(*),FMAX(MVSIZ),
     .   CB(MVSIZ), CN(MVSIZ), DEGMB(MVSIZ),  
     .   E11(MVSIZ), E22(MVSIZ), NU12(MVSIZ), NU21(MVSIZ),
     .   G12(MVSIZ), G23(MVSIZ), G31(MVSIZ),
     .   F1(MVSIZ), F2(MVSIZ), F12(MVSIZ), F11(MVSIZ), F22(MVSIZ), 
     .   F33(MVSIZ), EPSP(*), WPLAREF(MVSIZ),SIGY(*),ETSE(*),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),TSAIWU(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DP1(MVSIZ), DP2(MVSIZ), DP3(MVSIZ), 
     .   LAMDA(MVSIZ), COEF(MVSIZ), 
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ),
     .   DS1(MVSIZ), DS2(MVSIZ), DS3(MVSIZ), DE(MVSIZ),
     .   DE1(MVSIZ), DE2(MVSIZ), WVEC(MVSIZ), T1(MVSIZ),
     .   T2(MVSIZ), T3(MVSIZ), 
     .   A11(MVSIZ), A12(MVSIZ), A22(MVSIZ),
     .   SO1(MVSIZ), SO2(MVSIZ), SO3(MVSIZ), 
     .   SCALE, FYLD, CNN,HT,YLD(MVSIZ),SIG(NEL,5)
C-----------------------------------------------
      DO I=JFT,JLT
        SIG(I,1) = SIGOXX(I)
        SIG(I,2) = SIGOYY(I)
        SIG(I,3) = SIGOXY(I)
        SIG(I,4) = SIGOYZ(I)
        SIG(I,5) = SIGOZX(I)
      ENDDO
!
C
!! temporary replaced by (the same) ROTO_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!      CALL ROTO_SIG(JFT,JLT,SIG,DIR,NEL)
!!      CALL ROTO(JFT,JLT,SIG,DIR)
!      DO I=JFT,JLT
!        EPS(I,3)=HALF*EPS(I,3)
!        EPS(I,4)=HALF*EPS(I,4)
!        EPS(I,5)=HALF*EPS(I,5)   
!      ENDDO
C
!      CALL ROTOV(JFT,JLT,EPS,DIR,NEL)
C
!      DO I=JFT,JLT
!        EPS(I,3)=TWO*EPS(I,3)
!        EPS(I,4)=TWO*EPS(I,4)
!        EPS(I,5)=TWO*EPS(I,5)   
!      ENDDO
C-----------------------------
C     DEFORMATIONS ELASTIQUES
C-----------------------------
      DO I=JFT,JLT
         DE1(I)= MAX(DE(I),SIGN(ONE,SIG(I,1)))
         DE2(I)= MAX(DE(I),SIGN(ONE,SIG(I,2)))
         SCALE = (HALF+SIGN(HALF,DE1(I)-ONE))
     *          *(HALF+SIGN(HALF,DE2(I)-ONE))
C
         S1(I) = SIG(I,1)/DE1(I)-NU12(I)*SIG(I,2)*SCALE
         S2(I) = SIG(I,2)/DE2(I)-NU21(I)*SIG(I,1)*SCALE
         S1(I)=S1(I)/E11(I)
         S2(I)=S2(I)/E22(I)
         S3(I)=SIG(I,3)/DE1(I)/DE2(I)/G12(I)
         S4(I)=SIG(I,4)/MAX(DE2(I)*G23(I)*SHF(I),EM30)
         S5(I)=SIG(I,5)/MAX(DE1(I)*G31(I)*SHF(I),EM30)
      ENDDO
C
      IF (NPT == 1) THEN
        DO I=JFT,JLT
          DEGMB(I)= DEGMB(I)
     .            -(SIG(I,1)*S1(I)+SIG(I,2)*S2(I)+SIG(I,3)*S3(I))
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
         S1(I)=S1(I)+DEPSXX(I)
         S2(I)=S2(I)+DEPSYY(I)
         S3(I)=S3(I)+DEPSXY(I)
         S4(I)=S4(I)+DEPSYZ(I)
         S5(I)=S5(I)+DEPSZX(I)
C
         DE1(I)= MAX(DE(I),SIGN(ONE,S1(I)))
         DE2(I)= MAX(DE(I),SIGN(ONE,S2(I)))
         SCALE = (HALF+SIGN(HALF,DE1(I)-ONE))
     *          *(HALF+SIGN(HALF,DE2(I)-ONE))
C
         A12(I)=ONE -NU12(I)*NU21(I)*SCALE
         A11(I)=E11(I)*DE1(I)/A12(I)
         A22(I)=E22(I)*DE2(I)/A12(I)
         A12(I) = NU21(I)*A11(I)*SCALE
      ENDDO
C-------------------------------------------------------------------
C     CONTRAINTES ELASTIQUES
C-----------------------------
      DO I=JFT,JLT
      T1(I)   =A11(I)*S1(I)+A12(I)*S2(I)
      T2(I)   =A12(I)*S1(I)+A22(I)*S2(I)
      T3(I)   =DE1(I)*DE2(I)*G12(I)*S3(I)
      SIG(I,4)=DE2(I)*G23(I)*SHF(I)*S4(I)
      SIG(I,5)=DE1(I)*G31(I)*SHF(I)*S5(I)
      ENDDO
C
      IF (NPT == 1) THEN
        DO I=JFT,JLT
          DEGMB(I) = DEGMB(I)+(T1(I)*S1(I)+T2(I)*S2(I)+T3(I)*S3(I))
        ENDDO
      ENDIF
C-------------------------------------------------------------------
C     PLASTICITE
C-------------------------------------------------------------------
      DO I=JFT,JLT
      SO1(I)=SIG(I,1)
      SO2(I)=SIG(I,2)
      SO3(I)=SIG(I,3)
      ENDDO
C
      DO I=JFT,JLT
            WVEC(I)=F1(I) *T1(I)      +F2(I) *T2(I) +
     .        F11(I)*T1(I)*T1(I)+F22(I)*T2(I)*T2(I) +
     .        F33(I)*T3(I)*T3(I)+
     .       TWO*F12(I)*T1(I)*T2(I)
!!c    equiv crit for output
!!           SEQ_OUTPUT(I) = WVEC(I)
      ENDDO
C
      DO I=JFT,JLT
        IF (ISRATE == 0) EPSP(I) =
     .                MAX(ABS(DEPSXX(I)),ABS(DEPSYY(I)),ABS(DEPSXY(I)),
     .                ABS(DEPSYZ(I)),ABS(DEPSZX(I)))/MAX(DT1,EM30)
        IF (EPSP(I) > EPDR(I) .AND. CC(I) /= ZERO) THEN
        EPSP(I)=ONE + CC(I) * LOG(EPSP(I)/EPDR(I))
       ELSE
        EPSP(I)=ONE
       ENDIF
       COEF(I)=ZERO
       FYLD= (ONE +CB(I)*WPLA(I)**CN(I))*EPSP(I)
        IF(ICC(I) == 1 .OR. ICC(I) == 3) THEN
        FMAX(I) = FMAX(I)*EPSP(I)
       ENDIF
        IF (ICC(I) == 2 .OR. ICC(I) == 4) THEN
        WWPLA(I) = EPSP(I)
       ELSE
        WWPLA(I) = ONE
       ENDIF
       FYLD= MIN(FMAX(I),FYLD)
        IF (WVEC(I) > FYLD .AND. OFF(I) == ONE) COEF(I) = ONE
       WVEC(I)=ONE
       CNN=CN(I)-ONE
       IF(WPLA(I)>ZERO) WVEC(I)=EPSP(I)*WPLA(I)**CNN
       YLD(I) = FYLD
       TSAIWU(I) = MAX(MIN(WVEC(I)/FYLD,ONE),TSAIWU(I))
      ENDDO
C
      DO I=JFT,JLT
      DP1(I)=F1(I)+2*F11(I)*SO1(I)+2*F12(I)*SO2(I)
      DP2(I)=F2(I)+2*F22(I)*SO2(I)+2*F12(I)*SO1(I)
      DP3(I)=TWO*F33(I)*SO3(I)
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
      ENDDO
C
      DO I=JFT,JLT
        IF (LAMDA(I) == ZERO) CYCLE
      LAMDA(I)=LAMDA(I)*COEF(I)/
     .        (DP1(I)*(A11(I)*DP1(I)+A12(I)*DP2(I))+
     .         DP2(I)*(A12(I)*DP1(I)+A22(I)*DP2(I))+
     .      TWO*DP3(I)*G12(I)*DP3(I)+
     .              (SO1(I)*DP1(I)+SO2(I)*DP2(I)+TWO*SO3(I)*DP3(I))
     .         *CN(I)*CB(I)*WVEC(I) )
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
      T3(I)=T3(I)-G12(I)*DP3(I)*TWO
      ENDDO
C
      DO I=JFT,JLT
      WPLA(I)=WPLA(I)+HALF*
     .      (DP1(I)*(T1(I)+SO1(I))+
     .       DP2(I)*(T2(I)+SO2(I))+
     .      TWO*DP3(I)*(T3(I)+SO3(I))) / WPLAREF(I)
      WPLA(I)= MAX(WPLA(I),ZERO)
      WWPLA(I)= WPLA(I)/WWPLA(I)  
      ENDDO
C
      DO I=JFT,JLT
      SIG(I,1)=T1(I)
      SIG(I,2)=T2(I)
      SIG(I,3)=T3(I)
      ENDDO
C-------------------
C     PLASTICITY END
C-------------------
C------for QEPH      
      DO I=JFT,JLT
          SIGY(I)=YLD(I)*SIGY(I)
          IF (COEF(I) == ONE) THEN
           HT=CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(MAX(WPLA(I),EM20)))
           IF(YLD(I)>=FMAX(I)) HT=EM10
           ETSE(I)= HT/(HT+E11(I))
          ELSE
           ETSE(I) = ONE ! ETSE is used whatever the value of COEF
          END IF
      END DO 
C-------------------------------------------------------------------
C     RETOUR DANS LE REPERE COQUE
C-----------------------------
!! temporary replaced by (the same) ROTO_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!      CALL UROTO_SIG(JFT,JLT,SIG,DIR,NEL)
!!      CALL UROTO(JFT,JLT,SIG,DIR)
!
      DO I=JFT,JLT
        SIGNXX(I) = SIG(I,1)
        SIGNYY(I) = SIG(I,2)
        SIGNXY(I) = SIG(I,3)
        SIGNYZ(I) = SIG(I,4)
        SIGNZX(I) = SIG(I,5)
      ENDDO
!
C
      RETURN
      END
