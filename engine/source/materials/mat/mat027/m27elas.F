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
Chd|  M27ELAS                       source/materials/mat/mat027/m27elas.F
Chd|-- called by -----------
Chd|        SIGEPS27C                     source/materials/mat/mat027/sigeps27c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M27ELAS(JFT    ,JLT    ,PM     ,SIG    ,IMAT  ,
     2                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     3                   DAMT   ,CRAK   ,SHF    ,NEL    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IMAT,NEL
C     REAL
      my_real
     . PM(NPROPM,*),DAMT(NEL,2),CRAK(NEL,2),SHF(*),
     . DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     . DEPSZX(MVSIZ)
      my_real, INTENT(INOUT) :: SIG(NEL,5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     . E(MVSIZ),NU(MVSIZ),G(MVSIZ),DE1(MVSIZ),DE2(MVSIZ),
     . EEL1(MVSIZ),EEL2(MVSIZ),EEL3(MVSIZ),EEL4(MVSIZ),EEL5(MVSIZ),
     . A11(MVSIZ),A22(MVSIZ),A12(MVSIZ),EPST1(MVSIZ),EPST2(MVSIZ),
     . EPSM1(MVSIZ),EPSM2(MVSIZ),DMAX1(MVSIZ),DMAX2(MVSIZ),
     . SCALE1,SCALE2
C-----------------------------------------------
#include "vectorize.inc" 
      DO I=JFT,JLT
        E(I)     = PM(20,IMAT)
        NU(I)    = PM(21,IMAT)
        G(I)     = PM(22,IMAT)
        EPST1(I) = PM(60,IMAT)
        EPST2(I) = PM(61,IMAT)
        EPSM1(I) = PM(62,IMAT)
        EPSM2(I) = PM(63,IMAT)
        DMAX1(I) = PM(64,IMAT)
        DMAX2(I) = PM(65,IMAT)
      ENDDO
C--------------------------------------
C     ANCIENNES DEFORMATIONS ELASTIQUES
C     ANCIEN DOMMAGE EFFECTIF:
C        SIG>0  => DE =1-D
C        SIG<0  => DE =1
C--------------------------------------
#include "vectorize.inc" 
      DO I=JFT,JLT
        DE1(I)  = ONE - MAX(ZERO,SIGN(DAMT(I,1),SIG(I,1)))
        DE2(I)  = ONE - MAX(ZERO,SIGN(DAMT(I,2),SIG(I,2)))
        SCALE1  =(HALF
     .  +SIGN(HALF,DE1(I)-ONE))*(HALF+SIGN(HALF,DE2(I)-ONE))
        EEL1(I) = (SIG(I,1)/DE1(I)-NU(I)*SIG(I,2)*SCALE1)/E(I)
        EEL2(I) = (SIG(I,2)/DE2(I)-NU(I)*SIG(I,1)*SCALE1)/E(I)
        EEL3(I) =  SIG(I,3)/DE1(I)/DE2(I)/G(I)
        EEL4(I) =  SIG(I,4)/MAX(DE2(I)*G(I)*SHF(I),EM30)
        EEL5(I) =  SIG(I,5)/MAX(DE1(I)*G(I)*SHF(I),EM30)
      ENDDO
C----------------------------------------
C     NOUVELLES DEFORMATIONS "ELASTIQUES"
C----------------------------------------
      DO I=JFT,JLT
        EEL1(I) = EEL1(I)+DEPSXX(I)
        EEL2(I) = EEL2(I)+DEPSYY(I)
        EEL3(I) = EEL3(I)+DEPSXY(I)
        EEL4(I) = EEL4(I)+DEPSYZ(I)
        EEL5(I) = EEL5(I)+DEPSZX(I)
      ENDDO
C----------------------------------------
C     NOUVEAU DOMMAGE DES ELEMENTS CASSES
C----------------------------------------
      DO I=JFT,JLT
        IF (DAMT(I,1) /= ZERO) THEN
          CRAK(I,1) = CRAK(I,1) + DEPSXX(I)
          DAMT(I,1) = MAX(DAMT(I,1),CRAK(I,1)/(EPSM1(I)-EPST1(I)))
          DAMT(I,1) = MIN(DAMT(I,1),DMAX1(I))
        ENDIF
      ENDDO
C
      DO I=JFT,JLT
        IF (DAMT(I,2) /= ZERO) THEN
          CRAK(I,2) = CRAK(I,2) + DEPSYY(I)
          DAMT(I,2) = MAX(DAMT(I,2),CRAK(I,2)/(EPSM2(I)-EPST2(I)))
          DAMT(I,2) = MIN(DAMT(I,2),DMAX2(I))
        ENDIF
      ENDDO
C-----------------------------
C     CONTRAINTES "ELASTIQUES"
C-----------------------------
      DO I=JFT,JLT
        DE1(I) = ONE - MAX(ZERO,SIGN(DAMT(I,1),SIG(I,1)))
        DE2(I) = ONE - MAX(ZERO,SIGN(DAMT(I,2),SIG(I,2)))
        SCALE1 =(HALF
     .     +SIGN(HALF,DE1(I)-ONE))*(HALF+SIGN(HALF,DE2(I)-ONE))
        SCALE2 = ONE-NU(I)*NU(I)*SCALE1
        A11(I) = E(I)*DE1(I)/SCALE2
        A22(I) = E(I)*DE2(I)/SCALE2
        A12(I) = NU(I)*A11(I)*SCALE1
      ENDDO
C
      DO I=JFT,JLT
        SIG(I,1) = A11(I)*EEL1(I)+A12(I)*EEL2(I)
        SIG(I,2) = A12(I)*EEL1(I)+A22(I)*EEL2(I)
        SIG(I,3) = DE1(I)*DE2(I)*G(I)*EEL3(I)
        SIG(I,4) = DE2(I)*G(I)*SHF(I)*EEL4(I)
        SIG(I,5) = DE1(I)*G(I)*SHF(I)*EEL5(I)
      ENDDO
C
      RETURN
      END

