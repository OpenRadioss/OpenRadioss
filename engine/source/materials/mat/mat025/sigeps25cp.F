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
Chd|  SIGEPS25CP                    source/materials/mat/mat025/sigeps25cp.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M25CPLRP2                     source/materials/mat/mat025/m25cplrc2.F
Chd|====================================================================
      SUBROUTINE SIGEPS25CP(
     1                      JFT    ,JLT    ,PM       ,OFF    ,DIR    ,
     2                      SHF    ,NPT    ,NGL      ,IPT    ,OFF_OLD,
     3                      THK0   ,EPSPL  ,SIGY     ,ZCFAC  ,NEL    ,
     4                      DEPSXX ,DEPSYY ,DEPSXY   ,DEPSYZ ,DEPSZX ,
     5                      SIGOXX ,SIGOYY ,SIGOXY   ,SIGOYZ ,SIGOZX ,
     6                      SIGNXX ,SIGNYY ,SIGNXY   ,SIGNYZ ,SIGNZX ,
     7                      IMAT   ,WPLAR  ,IOFF_DUCT,PLA    ,ISRATE )
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
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NPT,NEL,IMAT,IPT
      INTEGER NGL(MVSIZ),IOFF_DUCT(MVSIZ),ISRATE
C     REAL ou REAL*8
      my_real
     .   THK0(MVSIZ),PM(NPROPM,*),
     .   OFF(*),DIR(*),SHF(*),EPSPL(*),SIGY(*),ZCFAC(MVSIZ,2),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),WPLAR(MVSIZ),OFF_OLD(MVSIZ),PLA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ),I,ILAYER,ICC_1
C     REAL ou REAL*8
      my_real
     .   YLD(MVSIZ),ETSE(MVSIZ),DEGMB(MVSIZ),CB(MVSIZ),CN(MVSIZ),
     .   E11(MVSIZ),E22(MVSIZ),NU12(MVSIZ),NU21(MVSIZ),G12(MVSIZ),
     .   G23(MVSIZ),G31(MVSIZ),DE(MVSIZ),F1(MVSIZ),F2(MVSIZ),
     .   F12(MVSIZ),F11(MVSIZ),F22(MVSIZ),F33(MVSIZ),SIGT1(MVSIZ),
     .   SIGT2(MVSIZ),FMAX(MVSIZ),WWPLA(MVSIZ),WPLAMX(MVSIZ),
     .   CC(MVSIZ),EPDR(MVSIZ),WPLAREF(MVSIZ),
     .   E11_1,E22_1,NU12_1,NU21_1,G12_1,G23_1,G31_1,DE_1,CB_1,CN_1,
     .   CC_1,EPDR_1,F1_1,F2_1,F11_1,F22_1,F33_1,F12_1,SIGT1_1,SIGT2_1
C=======================================================================
      E11_1   =PM(33,IMAT)
      E22_1   =PM(34,IMAT)
      NU12_1  =PM(35,IMAT)
      NU21_1  =PM(36,IMAT)
      G12_1   =PM(37,IMAT)
      G23_1   =PM(38,IMAT)
      G31_1   =PM(39,IMAT)
      DE_1    =PM(44,IMAT)
      CB_1    =PM(46,IMAT)
      CN_1    =PM(47,IMAT)
      CC_1    =PM(50,IMAT)
      EPDR_1  =PM(51,IMAT)
      ICC_1   =NINT(PM(53,IMAT))
      F1_1    =PM(54,IMAT)
      F2_1    =PM(55,IMAT)
      F11_1   =PM(56,IMAT)
      F22_1   =PM(57,IMAT)
      F33_1   =PM(58,IMAT)
      F12_1   =PM(59,IMAT)
      SIGT1_1 =PM(60,IMAT)
      SIGT2_1 =PM(61,IMAT)
!
      DO I=JFT,JLT
        E11(I)   =E11_1
        E22(I)   =E22_1
        NU12(I)  =NU12_1
        NU21(I)  =NU21_1
        G12(I)   =G12_1
        G23(I)   =G23_1
        G31(I)   =G31_1
        DE(I)    =DE_1
        CB(I)    =CB_1
        CN(I)    =CN_1
        CC(I)    =CC_1
        EPDR(I)  =EPDR_1
        ICC(I)   =ICC_1
        F1(I)    =F1_1
        F2(I)    =F2_1
        F11(I)   =F11_1
        F22(I)   =F22_1
        F33(I)   =F33_1
        F12(I)   =F12_1
        SIGT1(I) =SIGT1_1
        SIGT2(I) =SIGT2_1
        IF (IPT == 1) SIGY(I)=UN/SQRT(F33_1)
      ENDDO
!
      DO I=JFT,JLT
        WPLAREF(I)=PM(68,IMAT)
        WPLAMX(I) =PM(41,IMAT)
        FMAX(I)   =PM(49,IMAT)
      ENDDO
!
!     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!
        CALL M25CPLRP2(JFT    ,JLT    ,WPLAREF,THK0   ,OFF    ,ETSE   ,
     2                 PLA    ,DIR    ,NPT    ,CC     ,EPDR   ,ICC    ,
     3                 WWPLA  ,SHF    ,FMAX   ,CB     ,CN     ,NEL    ,
     4                 DEGMB  ,F1     ,F2     ,F12    ,F11    ,F22    ,
     5                 F33    ,E11    ,E22    ,NU12   ,NU21   ,G12    ,
     6                 G23    ,G31    ,DE     ,EPSPL  ,ISRATE ,YLD    ,
     7                 DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,SIGOXX ,
     8                 SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,SIGNXX ,SIGNYY ,
     9                 SIGNXY ,SIGNYZ ,SIGNZX )
!
        DO I=JFT,JLT
          WPLAR(I) = MAX(WPLAR(I),WWPLA(I))
        END DO
C-----------------------
C     For QEPH
C-----------------------
       DO I=JFT,JLT
         ZCFAC(I,1) = ZCFAC(I,1) + ETSE(I) / NPT
         ZCFAC(I,2) = MIN(ETSE(I),ZCFAC(I,2))
         SIGY(I) = SIGY(I) + YLD(I)/NPT
       ENDDO
C----------------------------
C     TEST DE RUPTURE DUCTILE
C---------------------------
      DO I=JFT,JLT
!!        IF (OFF(I) < EM01) OFF(I) = ZERO
!!        IF (OFF(I) < UN)   OFF(I) = OFF(I)*QCINQ
          IF ( OFF(I) == OFF_OLD(I) .and. OFF_OLD(I) < EM01 ) OFF(I)=ZERO
          IF ( OFF(I) == OFF_OLD(I) .and. OFF_OLD(I) < UN ) OFF(I)=OFF(I)*QCINQ
      ENDDO
!
      DO I=JFT,JLT
        IF (OFF(I) == UN .AND. WPLAR(I) >= WPLAMX(I)) THEN
          OFF(I)=QCINQ
!!        OFF(I)=OFF(I)*QCINQ
          IOFF_DUCT(I) = 1
        ENDIF ! IF (OFF(I) == UN .AND. WPLAR(I) >= WPLAMX(I))
      ENDDO
!
      IF (NPT == 1) THEN
        DO I=JFT,JLT
          DEGMB(I)=DEGMB(I)*OFF(I)
        ENDDO
      ENDIF
c-----------
      RETURN
      END
