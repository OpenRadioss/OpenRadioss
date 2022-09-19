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
Chd|  SIGEPS01G                     source/materials/mat/mat001/sigeps01g.F
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS01G(JFT     ,JLT     ,G       ,THK     ,OFF     ,
     1                     GS      ,A1      ,A2      ,NU      ,THK0    ,
     2                     NEL     ,F_DEF   ,ISMSTR  ,DEPSXX  ,DEPSYY  ,
     3                     DEPSXY  ,DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY  ,
     4                     DEPBXY  ,SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,
     5                     SIGOZX  ,MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX  ,
     6                     SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX  ,
     7                     MOMNYY  ,MOMNXY  )
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
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NEL,ISMSTR
C     REAL
      my_real
     .  OFF(*),GS(*)
      my_real
     .   NU(MVSIZ),G(MVSIZ),A1(MVSIZ),A2(MVSIZ),F_DEF(MVSIZ,8),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    THK(*),
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J
C     REAL
      my_real
     .   DTINV,AMU,FACT,VISC
      my_real
     .   B1(MVSIZ),B2(MVSIZ),THK0(MVSIZ),
     .   B3(MVSIZ),DEGMB(MVSIZ),DEGFX(MVSIZ),
     .   THK08,EZZ(MVSIZ)
      my_real
     .   EIGV(MVSIZ,3,2),TRAV(MVSIZ),ROOTV(MVSIZ),
     .   EH1(MVSIZ),EH2(MVSIZ),EB1(MVSIZ),EB2(MVSIZ),DEZZ(MVSIZ),
     .   EPSXX (MVSIZ),EPSYY (MVSIZ),EPSXY (MVSIZ),
     .   FOR1(MVSIZ),FOR2(MVSIZ),MOM1(MVSIZ),MOM2(MVSIZ)
c     .   TAN_A,TAN_A2,SIN2_A(MVSIZ),COS2_A(MVSIZ),SINCOS_A(MVSIZ),ANG
C-----------------------------------------------
!---
      IF (ISMSTR == 10) THEN
C---------[F]=[F_DEF]+[1]; [B]=[F][F]^t strain----
        DO I=JFT,JLT
          EPSXX(I)=F_DEF(I,1)*(TWO+F_DEF(I,1))+
     .             F_DEF(I,3)*F_DEF(I,3) + ONE
          EPSYY(I)=F_DEF(I,2)*(TWO+F_DEF(I,2))+
     .             F_DEF(I,4)*F_DEF(I,4) + ONE
c          EPSZZ(I)=MFZZ(I)*(TWO+MFZZ(I))+
c     .             MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
          EPSXY(I)=F_DEF(I,3)+F_DEF(I,4)+F_DEF(I,1)*F_DEF(I,4)+
     .             F_DEF(I,3)*F_DEF(I,2)
        ENDDO
C     principal stretch (def gradient eigenvalues)
        DO I=JFT,JLT
          TRAV(I)  = EPSXX(I)+EPSYY(I)
          ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .               + EPSXY(I)*EPSXY(I)*FOUR)
          EB1(I) = HALF*(TRAV(I)+ROOTV(I))
          EB2(I) = HALF*(TRAV(I)-ROOTV(I))
          EH1(I) = HALF*LOG(EB1(I))
          EH2(I) = HALF*LOG(EB2(I))
          FOR1(I)=A1(I)*EH1(I)+A2(I)*EH2(I)
          FOR2(I)=A1(I)*EH2(I)+A2(I)*EH1(I)
        ENDDO
C     rot matrix (eigenvectors), ROOTV= 2R
        DO I=JFT,JLT
          IF (ABS(EB2(I)-EB1(I)) < EM15) THEN
            EIGV(I,1,1)=ONE
            EIGV(I,2,1)=ZERO
            EIGV(I,3,1)=ZERO
            EIGV(I,1,2)=ZERO
            EIGV(I,2,2)=ONE
            EIGV(I,3,2)=ZERO
          ELSE
            EIGV(I,1,1) = (EPSXX(I)-EB2(I)) / ROOTV(I)
            EIGV(I,2,1) = (EPSYY(I)-EB2(I)) / ROOTV(I)
            EIGV(I,3,1) = (EPSXY(I)) / ROOTV(I)
            EIGV(I,1,2) = (EB1(I)-EPSXX(I)) / ROOTV(I)
            EIGV(I,2,2) = (EB1(I)-EPSYY(I)) / ROOTV(I)
            EIGV(I,3,2) =-(EPSXY(I)) / ROOTV(I)
          ENDIF
        ENDDO
      ENDIF ! IF (ISMSTR == 10) THEN
!---
C
      DO I=JFT,JLT
        THK08 = THK0(I)*ONE_OVER_12
        B1(I) = A1(I)*THK08
        B2(I) = A2(I)*THK08
        B3(I) = G(I) *THK08
      ENDDO
C
      IF (ISMSTR /= 10) THEN
        DO I=JFT,JLT
          SIGNXX(I)=SIGOXX(I)+A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
          SIGNYY(I)=SIGOYY(I)+A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
          SIGNXY(I)=SIGOXY(I)+G(I) *DEPSXY(I)
          SIGNYZ(I)=SIGOYZ(I)+GS(I)*DEPSYZ(I)
          SIGNZX(I)=SIGOZX(I)+GS(I)*DEPSZX(I)
C
          MOMNXX(I)=MOMOXX(I)+B1(I)*DEPBXX(I)+B2(I)*DEPBYY(I)
          MOMNYY(I)=MOMOYY(I)+B1(I)*DEPBYY(I)+B2(I)*DEPBXX(I)
          MOMNXY(I)=MOMOXY(I)+B3(I)*DEPBXY(I)
        ENDDO
      ELSE  !  ISMSTR = 10
        DO I=JFT,JLT
          SIGNXX(I) = EIGV(I,1,1)*FOR1(I) + EIGV(I,1,2)*FOR2(I)
          SIGNYY(I) = EIGV(I,2,1)*FOR1(I) + EIGV(I,2,2)*FOR2(I)
          SIGNXY(I) = EIGV(I,3,1)*FOR1(I) + EIGV(I,3,2)*FOR2(I)
          SIGNYZ(I) = SIGOYZ(I)+ GS(I)*DEPSYZ(I)
          SIGNZX(I) = SIGOZX(I)+ GS(I)*DEPSZX(I)
        ENDDO
C-----curveture        
C---------[F]=[F_DEF]+[1]; [B]=[F][F]^t strain-----
        DO I=JFT,JLT
          EPSXX(I)=F_DEF(I,6)*(TWO+F_DEF(I,6))+
     .             F_DEF(I,8)*F_DEF(I,8) +ONE
          EPSYY(I)=F_DEF(I,7)*(TWO+F_DEF(I,7))+
     .             F_DEF(I,5)*F_DEF(I,5) +ONE
          EPSXY(I)=F_DEF(I,8)+F_DEF(I,5)+F_DEF(I,6)*F_DEF(I,5)+
     .             F_DEF(I,8)*F_DEF(I,7)
        ENDDO
C     principal stretch (def gradient eigenvalues)
        DO I=JFT,JLT
          TRAV(I)  = EPSXX(I)+EPSYY(I)
          ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .             + EPSXY(I)*EPSXY(I)*FOUR)
          EB1(I) = HALF*(TRAV(I)+ROOTV(I))
          EB2(I) = HALF*(TRAV(I)-ROOTV(I))
          EH1(I) = HALF*LOG(EB1(I))
          EH2(I) = HALF*LOG(EB2(I))
          MOM1(I)=B1(I)*EH1(I)+B2(I)*EH2(I)
          MOM2(I)=B1(I)*EH2(I)+B2(I)*EH1(I)
        ENDDO
C     rot matrix (eigenvectors)
        DO I=JFT,JLT
          IF (ABS(EB2(I)-EB1(I)) < EM15) THEN
            EIGV(I,1,1)=ONE
            EIGV(I,2,1)=ZERO
            EIGV(I,3,1)=ZERO
            EIGV(I,1,2)=ZERO
            EIGV(I,2,2)=ONE
            EIGV(I,3,2)=ZERO
          ELSE
            EIGV(I,1,1) = (EPSXX(I)-EB2(I)) / ROOTV(I)
            EIGV(I,2,1) = (EPSYY(I)-EB2(I)) / ROOTV(I)
            EIGV(I,3,1) = (EPSXY(I)) / ROOTV(I)
            EIGV(I,1,2) = (EB1(I)-EPSXX(I)) / ROOTV(I)
            EIGV(I,2,2) = (EB1(I)-EPSYY(I)) / ROOTV(I)
            EIGV(I,3,2) =-(EPSXY(I)) / ROOTV(I)
          ENDIF
        ENDDO
        DO I=JFT,JLT
          MOMNXX(I) = EIGV(I,1,1)*MOM1(I) + EIGV(I,1,2)*MOM2(I)
          MOMNYY(I) = EIGV(I,2,1)*MOM1(I) + EIGV(I,2,2)*MOM2(I)
          MOMNXY(I) = EIGV(I,3,1)*MOM1(I) + EIGV(I,3,2)*MOM2(I)
        ENDDO
      ENDIF ! IF (ISMSTR /= 10) THEN
C
      DO I=JFT,JLT
        EZZ(I) = -NU(I) * (DEPSXX(I)+DEPSYY(I)) / (ONE-NU(I))
        THK(I) = THK(I) * (ONE + EZZ(I) * OFF(I))
      ENDDO
C
      RETURN
      END
