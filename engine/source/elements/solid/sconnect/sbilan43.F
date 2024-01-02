Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SBILAN43                      source/elements/solid/sconnect/sbilan43.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SBILAN43(NEL   ,IPARTS ,PARTSAV,EINT   ,RHO    ,
     .                    AREAT ,VXA    ,VYA    ,VZA    ,VA2    ,
     .                    FILL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER IPARTS(*)
C     REAL
      my_real
     .  PARTSAV(NPSAV,*), EINT(*), RHO(*), AREAT(*),
     .  VXA(*), VYA(*), VZA(*), VA2(*), FILL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,M
C     REAL
      my_real
     .   EI_SAV(NEL),EK_SAV(NEL),XMAS(NEL),
     .   XM_SAV(NEL),YM_SAV(NEL),ZM_SAV(NEL)
C-----------------------------------------------
      EI_SAV = ZERO
      EK_SAV = ZERO
      XM_SAV = ZERO
      YM_SAV = ZERO
      ZM_SAV = ZERO
      DO I=1,NEL
        XMAS(I)=FILL(I)*RHO(I)*AREAT(I)
      ENDDO
C
      DO I=1,NEL
        M = IPARTS(I)
        PARTSAV(1,M)=PARTSAV(1,M) + FILL(I)*EINT(I)
        PARTSAV(2,M)=PARTSAV(2,M) + XMAS(I)*VA2(I)*ONE_OVER_16
        PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        PARTSAV(3,M)=PARTSAV(3,M) + XMAS(I)*VXA(I)*ONE_OVER_8
        PARTSAV(4,M)=PARTSAV(4,M) + XMAS(I)*VYA(I)*ONE_OVER_8
        PARTSAV(5,M)=PARTSAV(5,M) + XMAS(I)*VZA(I)*ONE_OVER_8
      ENDDO
C-----------
      RETURN
      END
