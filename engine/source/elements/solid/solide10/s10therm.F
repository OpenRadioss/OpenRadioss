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
!||    s10therm   ../engine/source/elements/solid/solide10/s10therm.F
!||--- called by ------------------------------------------------------
!||    s10forc3   ../engine/source/elements/solid/solide10/s10forc3.F
!||====================================================================
      SUBROUTINE S10THERM(
     1   PM,      IMAT,     NC,      VOL,
     2   PX,      PY,      PZ,      NI,
     3   DT1,     TEMPNC,  TEL,     HEAT,
     4   FPHI,    OFFG,    OFF,     NEL ,
     5   THEACCFACT)
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
      INTEGER NC(MVSIZ,10)
      my_real, INTENT(IN) :: THEACCFACT
      my_real
     .    VOL(*), PX(MVSIZ,*), PY(MVSIZ,*), PZ(MVSIZ,*),
     .    NI(MVSIZ,10), TEMPNC(*), FPHI(MVSIZ,10), PM(NPROPM,*), HEAT(*),
     .    DT1, TEL(*), OFF(*), OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .  CA, CB, KC, PHIX(MVSIZ), PHIY(MVSIZ), PHIZ(MVSIZ)
C-----------------------------------------------
      CA = PM(75,IMAT)
      CB = PM(76,IMAT)
!
      PHIX(1:MVSIZ) = ZERO
      PHIY(1:MVSIZ) = ZERO
      PHIZ(1:MVSIZ) = ZERO
C 
C - flux
C
      DO N=1,10
       DO I=1,NEL
          IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
          PHIX(I) = PHIX(I) +  TEMPNC(NC(I,N))*PX(I,N)              
          PHIY(I) = PHIY(I) +  TEMPNC(NC(I,N))*PY(I,N)
          PHIZ(I) = PHIZ(I) +  TEMPNC(NC(I,N))*PZ(I,N)
       ENDDO
      ENDDO
C
      DO I=1,NEL
        IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
        KC = (CA + CB*TEL(I))*DT1*VOL(I)*THEACCFACT          
        PHIX(I) = KC*PHIX(I)
        PHIY(I) = KC*PHIY(I)
        PHIZ(I) = KC*PHIZ(I)
      ENDDO
C
C force thermique nodale
C
      DO N=1,10
       DO I=1,NEL
          IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
          FPHI(I,N) = FPHI(I,N) + HEAT(I)*NI(I,N) - (PHIX(I)*PX(I,N) + PHIY(I)*PY(I,N) + PHIZ(I)*PZ(I,N))
        ENDDO
      ENDDO
C 
      RETURN
      END
