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
!||    s8etherm   ../engine/source/elements/solid/solide8e/s8etherm.F
!||--- called by ------------------------------------------------------
!||    s8cforc3   ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3   ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3   ../engine/source/elements/solid/solide8z/s8zforc3.F
!||====================================================================
      SUBROUTINE S8ETHERM(
     1   PM,      IMAT,     VOL,     NI,
     2   NC1,     NC2,     NC3,     NC4,
     3   NC5,     NC6,     NC7,     NC8,
     4   PX1,     PX2,     PX3,     PX4,
     5   PY1,     PY2,     PY3,     PY4,
     6   PZ1,     PZ2,     PZ3,     PZ4,
     7   PX5,     PX6,     PX7,     PX8,
     8   PY5,     PY6,     PY7,     PY8,
     9   PZ5,     PZ6,     PZ7,     PZ8,
     A   DT1,     TEMPNC,  TEL,     HEAT,
     B   FPHI,    OFFG,    OFF,     PARTSAV,
     C   IPARTS,  VOL0,    NEL,     THEACCFACT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: IMAT
      my_real, INTENT(IN) :: THEACCFACT
      INTEGER IPARTS(*),
     .        NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     .        NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
C     REAL
      my_real
     .    VOL(*), PX1(MVSIZ), PY1(MVSIZ),PZ1(MVSIZ),
     .            PX2(MVSIZ), PY2(MVSIZ),PZ2(MVSIZ),
     .            PX3(MVSIZ), PY3(MVSIZ),PZ3(MVSIZ),
     .            PX4(MVSIZ), PY4(MVSIZ),PZ4(MVSIZ),
     .            PX5(MVSIZ), PY5(MVSIZ),PZ5(MVSIZ),
     .            PX6(MVSIZ), PY6(MVSIZ),PZ6(MVSIZ),
     .            PX7(MVSIZ), PY7(MVSIZ),PZ7(MVSIZ),
     .            PX8(MVSIZ), PY8(MVSIZ),PZ8(MVSIZ),        
     .    NI(8),TEMPNC(*), FPHI(MVSIZ,8), PM(NPROPM,*), HEAT(*),
     .    DT1, TEL(*), OFF(*), OFFG(*)
      my_real PARTSAV(NPSAV,*),VOL0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M
C     REAL
      my_real CA, CB, KC, PHIX, PHIY, PHIZ, RHOCP, T0
!=======================================================================
      CA = PM(75,IMAT)
      CB = PM(76,IMAT)
      RHOCP = PM(69,IMAT)
      T0  = PM(79,IMAT)
!
      DO I=1,NEL
       IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
C 
C - flux
          PHIX =  TEMPNC(NC1(I))*PX1(I) + TEMPNC(NC2(I))*PX2(I)  +
     .            TEMPNC(NC3(I))*PX3(I) + TEMPNC(NC4(I))*PX4(I)  +
     .            TEMPNC(NC5(I))*PX5(I) + TEMPNC(NC6(I))*PX6(I)  +
     .            TEMPNC(NC7(I))*PX7(I) + TEMPNC(NC8(I))*PX8(I)       
          
          PHIY =  TEMPNC(NC1(I))*PY1(I) + TEMPNC(NC2(I))*PY2(I)  +
     .            TEMPNC(NC3(I))*PY3(I) + TEMPNC(NC4(I))*PY4(I)  +
     .            TEMPNC(NC5(I))*PY5(I) + TEMPNC(NC6(I))*PY6(I)  +
     .            TEMPNC(NC7(I))*PY7(I) + TEMPNC(NC8(I))*PY8(I)
      
          PHIZ =  TEMPNC(NC1(I))*PZ1(I) + TEMPNC(NC2(I))*PZ2(I)  +
     .            TEMPNC(NC3(I))*PZ3(I) + TEMPNC(NC4(I))*PZ4(I)  +
     .            TEMPNC(NC5(I))*PZ5(I) + TEMPNC(NC6(I))*PZ6(I)  +
     .            TEMPNC(NC7(I))*PZ7(I) + TEMPNC(NC8(I))*PZ8(I) 
C
        KC = (CA + CB*TEL(I))*DT1*VOL(I)*THEACCFACT          
        PHIX = KC*PHIX
        PHIY = KC*PHIY
        PHIZ = KC*PHIZ
C  
C force thermique nodale
C
          FPHI(I,1) = FPHI(I,1) +
     .          HEAT(I)*NI(1) - (PHIX*PX1(I) + PHIY*PY1(I) + PHIZ*PZ1(I))
          FPHI(I,2) = FPHI(I,2) +
     .          HEAT(I)*NI(2) - (PHIX*PX2(I) + PHIY*PY2(I) + PHIZ*PZ2(I))
          FPHI(I,3) = FPHI(I,3) +
     .          HEAT(I)*NI(3) - (PHIX*PX3(I) + PHIY*PY3(I) + PHIZ*PZ3(I))
          FPHI(I,4) = FPHI(I,4) +
     .          HEAT(I)*NI(4) - (PHIX*PX4(I) + PHIY*PY4(I) + PHIZ*PZ4(I))
          FPHI(I,5) = FPHI(I,5) +
     .          HEAT(I)*NI(5) - (PHIX*PX5(I) + PHIY*PY5(I) + PHIZ*PZ5(I))
          FPHI(I,6) = FPHI(I,6) +
     .          HEAT(I)*NI(6) - (PHIX*PX6(I) + PHIY*PY6(I) + PHIZ*PZ6(I))
          FPHI(I,7) = FPHI(I,7) +
     .          HEAT(I)*NI(7) - (PHIX*PX7(I) + PHIY*PY7(I) + PHIZ*PZ7(I))
          FPHI(I,8) = FPHI(I,8) +
     .          HEAT(I)*NI(8) - (PHIX*PX8(I) + PHIY*PY8(I) + PHIZ*PZ8(I))
      ENDDO
C
      DO I=1,NEL
         IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
         M   = IPARTS(I)
         PARTSAV(28,M)=PARTSAV(28,M) + RHOCP*VOL0(I)*(TEL(I)-T0) + HEAT(I)
      ENDDO
C 
      RETURN
      END
