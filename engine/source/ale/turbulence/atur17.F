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
Chd|  ATUR17                        source/ale/turbulence/atur17.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        A17YL2                        source/ale/turbulence/a17yl2.F
Chd|        A17YL3                        source/ale/turbulence/a17yl3.F
Chd|====================================================================
      SUBROUTINE ATUR17(PM,OFF,RHO,RK,RE,GEO,VOLN,MAT,DELTAX,NGEO,VIS,VNEW,
     .                  VD2,DVOL, AIRE, EINC, PNEW, TMU, ALOGEY)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real PM(NPROPM,NUMMAT), OFF(*), RHO(*), RK(*), RE(*),GEO(NPROPG,NUMGEO),VOLN(MVSIZ),
     .        DELTAX(*), VIS(*), VNEW(*), VD2(*), DVOL(*), AIRE(*), EINC(*), PNEW(*),
     .        TMU(*), ALOGEY(*)
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
          CALL A17YL3(DELTAX, YL)
        ELSE
          CALL A17YL2(DELTAX, YL, AIRE)
        ENDIF
        DO I=LFT,LLT
          GF=PM(50,MAT(I))
          CMU=PM(81,MAT(I))
          AX=PM(47,MAT(I))
          A=PM(49,MAT(I))
          GM=ONE -GF+GF*ALOGEY(I)
          PNEW(I)=TWO*RK(I)/(VOLN(I)*THREE)
          EI(I)=GM*EINC(I)/ALOGEY(I)-DVOL(I)*PNEW(I)
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
