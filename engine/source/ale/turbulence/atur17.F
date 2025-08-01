Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    atur17   ../engine/source/ale/turbulence/atur17.F
!||--- called by ------------------------------------------------------
!||    mmain    ../engine/source/materials/mat_share/mmain.F90
!||--- calls      -----------------------------------------------------
!||    a17yl2   ../engine/source/ale/turbulence/a17yl2.F
!||    a17yl3   ../engine/source/ale/turbulence/a17yl3.F
!||====================================================================
      SUBROUTINE ATUR17(
     1   PM,      OFF,     RHO,     RK,
     2   RE,      GEO,     VOLN,    MAT,
     3   DELTAX,  NGEO,
     4   VD2,     DVOL,    AIRE,    EINC,
     5   PTURB,    ALOGEY,  NEL,
     6   LFT,     LLT,     JPOR)

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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL, LFT,     LLT,     JPOR
      my_real PM(NPROPM,NUMMAT), OFF(*), RHO(*), RK(*), RE(*),GEO(NPROPG,NUMGEO),VOLN(MVSIZ),
     .        DELTAX(*), VD2(*), DVOL(*), AIRE(*), EINC(*), ALOGEY(*)
      my_real,INTENT(INOUT) :: PTURB(1:LLT)
      INTEGER MAT(*),NGEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real EI(MVSIZ), YL(MVSIZ), GF, CMU, AX, A, GM, XK
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C-----------------
C     YL CALCULATION
C-----------------
      IF(JPOR /= 2)THEN
        IF(N2D == 0)THEN
          CALL A17YL3(
     1   DELTAX,  YL,      NEL)
        ELSE
          CALL A17YL2(
     1   DELTAX,  YL,      AIRE,    NEL)
        ENDIF
        DO I=LFT,LLT
          GF=PM(50,MAT(I))
          CMU=PM(81,MAT(I))
          AX=PM(47,MAT(I))
          A=PM(49,MAT(I))
          GM=ONE -GF+GF*ALOGEY(I)
          PTURB(I)=TWO*RK(I)/(VOLN(I)*THREE)
          EI(I)=GM*EINC(I)/ALOGEY(I)-DVOL(I)*PTURB(I)
          EI(I)=MAX(ZERO,EI(I))
          RK(I)=RK(I)+(EI(I)-RE(I)*DT1)*OFF(I)
          RK(I)= MAX(EM15,RK(I)/VOLN(I))
          XK=RK(I)/RHO(I)
          RE(I)=RHO(I)*SQRT(XK*SQRT(CMU))**3*SQRT(SQRT(GM))/(AX*YL(I))
          RE(I)=MAX(EM15,RE(I))
        ENDDO! next I
      ELSE
       DO I=LFT,LLT
        ! MESH VELOCITY SUPPOSED LOW REGARDING MATERIALVELOCITY V(QUASI EULERIAN)
        RK(I)=RHO(I)*GEO(22,NGEO(I))**2*VD2(I)
        RE(I)=(RK(I)**3/RHO(I))**HALF/GEO(23,NGEO(I))
       ENDDO
      ENDIF
      RETURN
      END
