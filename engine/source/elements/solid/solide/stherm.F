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
      !||    stherm    ../engine/source/elements/solid/solide/stherm.F
      !||--- called by ------------------------------------------------------
      !||    sforc3    ../engine/source/elements/solid/solide/sforc3.F
      !||    szforc3   ../engine/source/elements/solid/solidez/szforc3.F
      !||====================================================================
      SUBROUTINE STHERM(
     1   PM,      IMAT,     VOL,     NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     PX1,
     4   PX2,     PX3,     PX4,     PY1,
     5   PY2,     PY3,     PY4,     PZ1,
     6   PZ2,     PZ3,     PZ4,     DT1,
     7   TEMPNC,  TEMPEL,  HEAT,    FPHI,
     8   OFFG,    OFF,     PARTSAV, IPARTS,
     9   VOL0,    NEL,     NUMNOD ,THEACCFACT)
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
      INTEGER, INTENT(IN) :: NUMNOD
      INTEGER, INTENT(IN) :: IMAT
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     .        NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
      INTEGER IPARTS(*)
      my_real :: DT1
      my_real ,INTENT(IN) :: THEACCFACT
      my_real ,INTENT(IN) :: HEAT(MVSIZ)
      my_real :: TEMPNC(NUMNOD)
      my_real :: TEMPEL(NEL) 
      my_real :: OFF(NEL)
      my_real :: OFFG(NEL)
      my_real :: VOL(NEL)
      my_real :: VOL0(NEL)
      my_real :: PX1(*), PX2(*), PX3(*), PX4(*), 
     .           PY1(*), PY2(*), PY3(*), PY4(*),
     .           PZ1(*), PZ2(*), PZ3(*), PZ4(*)
      my_real :: FPHI(MVSIZ,8)
      my_real :: PM(NPROPM,*)
      my_real :: PARTSAV(NPSAV,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,M
      my_real AS, BS, KC, PHIX, PHIY, PHIZ, A, B, RHOCP, T0
!==============================================================================
      AS    = PM(75,IMAT)    !< thermal conductivity coefficient A for solid phase
      BS    = PM(76,IMAT)    !< thermal conductivity coefficient B for solid phase
      RHOCP = PM(69,IMAT)
      T0    = PM(79,IMAT)
C 
C     calculate thermal flux  
C      
      DO I=1,NEL
        IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
        PHIX =  TEMPNC(NC1(I))*PX1(I) + TEMPNC(NC2(I))*PX2(I)  +
     .          TEMPNC(NC3(I))*PX3(I) + TEMPNC(NC4(I))*PX4(I)  -
     .          TEMPNC(NC5(I))*PX3(I) - TEMPNC(NC6(I))*PX4(I)  -
     .          TEMPNC(NC7(I))*PX1(I) - TEMPNC(NC8(I))*PX2(I)       
        
        PHIY =  TEMPNC(NC1(I))*PY1(I) + TEMPNC(NC2(I))*PY2(I)  +
     .          TEMPNC(NC3(I))*PY3(I) + TEMPNC(NC4(I))*PY4(I)  -
     .          TEMPNC(NC5(I))*PY3(I) - TEMPNC(NC6(I))*PY4(I)  -
     .          TEMPNC(NC7(I))*PY1(I) - TEMPNC(NC8(I))*PY2(I)
      
        PHIZ =  TEMPNC(NC1(I))*PZ1(I) + TEMPNC(NC2(I))*PZ2(I)  +
     .          TEMPNC(NC3(I))*PZ3(I) + TEMPNC(NC4(I))*PZ4(I)  -
     .          TEMPNC(NC5(I))*PZ3(I) - TEMPNC(NC6(I))*PZ4(I)  -
     .          TEMPNC(NC7(I))*PZ1(I) - TEMPNC(NC8(I))*PZ2(I)          
C
        KC = (AS + BS*TEMPEL(I))*VOL(I)*DT1*THEACCFACT
        PHIX = KC*PHIX
        PHIY = KC*PHIY
        PHIZ = KC*PHIZ         
C
C       ! nodal thermal force
C
        A = ONE_OVER_8 * HEAT(I)
        B = PHIX*PX1(I) + PHIY*PY1(I) + PZ1(I)*PHIZ        
        FPHI(I,1) = A - B
        FPHI(I,7) = A + B
        B = PHIX*PX2(I) + PHIY*PY2(I) + PZ2(I)*PHIZ
        FPHI(I,2) = A - B
        FPHI(I,8) = A + B
        B = PHIX*PX3(I) + PHIY*PY3(I) + PZ3(I)*PHIZ
        FPHI(I,3) = A - B
        FPHI(I,5) = A + B
        B = PHIX*PX4(I) + PHIY*PY4(I) + PZ4(I)*PHIZ
        FPHI(I,4) = A - B 
        FPHI(I,6) = A + B
      ENDDO
C  
      DO I=1,NEL
        IF (OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
        M = IPARTS(I)
        PARTSAV(28,M) = PARTSAV(28,M) + RHOCP*VOL0(I)*(TEMPEL(I)-T0) + HEAT(I)
      ENDDO
!-----------
      RETURN
      END
