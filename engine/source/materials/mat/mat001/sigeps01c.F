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
Chd|  SIGEPS01C                     source/materials/mat/mat001/sigeps01c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS01C(JFT       ,JLT      ,NEL      ,IMAT     ,GS       ,
     1                     SIGOXX    ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     2                     SIGNXX    ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     3                     DEPSXX    ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4                     THK       ,THKLYL    ,OFF      ,PM       ,ISMSTR   ,
     5                     EPSXX     ,EPSYY    ,EPSXY    )
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
      INTEGER JFT,JLT,NPT,NEL,IMAT,ISMSTR
C     REAL ou REAL*8
      my_real
     .  THK(*),GS(*), THKLYL(NEL),OFF(*),PM(NPROPM,*),
     .  SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .  DEPSZX(MVSIZ),EPSXX (MVSIZ),EPSYY (MVSIZ),EPSXY (MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   EZZ(MVSIZ),NU,G,A1,A2
      my_real
     .   EIGV(MVSIZ,3,2),TRAV(MVSIZ),ROOTV(MVSIZ),  
     .   EH1(MVSIZ),EH2(MVSIZ),EB1(MVSIZ),EB2(MVSIZ),
     .   FOR1(MVSIZ),FOR2(MVSIZ)
C-----------------------------------------------
      NU = PM(21,IMAT)
      G  = PM(22,IMAT)
      A1 = PM(24,IMAT)
      A2 = PM(25,IMAT)
!
      DO I=JFT,JLT
        SIGNXX(I)=SIGOXX(I)
        SIGNYY(I)=SIGOYY(I)
        SIGNXY(I)=SIGOXY(I)
        SIGNYZ(I)=SIGOYZ(I)
        SIGNZX(I)=SIGOZX(I)
      ENDDO
!---
      IF (ISMSTR == 10) THEN
        DO I=JFT,JLT
          EPSXX(I) = EPSXX(I) + UN
          EPSYY(I) = EPSYY(I) + UN
          EPSXY(I) = UNDEMI * EPSXY(I)
        ENDDO
C     principal stretch (def gradient eigenvalues)
        DO I=JFT,JLT
          TRAV(I)  = EPSXX(I)+EPSYY(I)
          ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .               + EPSXY(I)*EPSXY(I)*QUATRE)
          EB1(I) = UNDEMI*(TRAV(I)+ROOTV(I)) 
          EB2(I) = UNDEMI*(TRAV(I)-ROOTV(I)) 
          EH1(I) = UNDEMI*LOG(EB1(I))
          EH2(I) = UNDEMI*LOG(EB2(I))
          FOR1(I)= A1*EH1(I)+A2*EH2(I)
          FOR2(I)= A1*EH2(I)+A2*EH1(I)
        ENDDO
C     rot matrix (eigenvectors), ROOTV= 2R
        DO I=JFT,JLT
          IF (ABS(EB2(I)-EB1(I)) < EM15) THEN
            EIGV(I,1,1)=UN
            EIGV(I,2,1)=ZERO
            EIGV(I,3,1)=ZERO
            EIGV(I,1,2)=ZERO
            EIGV(I,2,2)=UN
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
!---------------------------
!     CONTRAINTES ELASTIQUES
!---------------------------
      IF (ISMSTR /= 10) THEN
        DO I=JFT,JLT
          SIGNXX(I) = SIGNXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
          SIGNYY(I) = SIGNYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
          SIGNXY(I) = SIGNXY(I)+ G*DEPSXY(I)
          SIGNYZ(I) = SIGNYZ(I)+GS(I)*DEPSYZ(I)
          SIGNZX(I) = SIGNZX(I)+GS(I)*DEPSZX(I)
        ENDDO
      ELSE  !  ISMSTR = 10
        DO I=JFT,JLT
          SIGNXX(I) = EIGV(I,1,1)*FOR1(I) + EIGV(I,1,2)*FOR2(I)
          SIGNYY(I) = EIGV(I,2,1)*FOR1(I) + EIGV(I,2,2)*FOR2(I)
          SIGNXY(I) = EIGV(I,3,1)*FOR1(I) + EIGV(I,3,2)*FOR2(I)
          SIGNYZ(I) = SIGOYZ(I)+ GS(I)*DEPSYZ(I)
          SIGNZX(I) = SIGOZX(I)+ GS(I)*DEPSZX(I)
        ENDDO
      ENDIF ! IF (ISMSTR /= 10)
!---
      DO I=JFT,JLT
        EZZ(I) = -NU * (DEPSXX(I) + DEPSYY(I)) / (UN-NU)
        THK(I) = THK(I) + EZZ(I) * THKLYL(I)*OFF(I)
      ENDDO
!---
      RETURN
      END
