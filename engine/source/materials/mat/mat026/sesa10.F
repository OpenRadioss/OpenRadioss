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
Chd|  SESA10                        source/materials/mat/mat026/sesa10.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SESA10(OFF   ,SNEW  ,QOLD  ,EINT  ,VOLN ,
     2                  DVOL  ,SOLD1 ,SOLD2 ,SOLD3 ,SOLD4,
     3                  SOLD5 ,SOLD6 ,D1    ,D2    ,D3   ,
     4                  D4    ,D5    ,D6    ,NEL, POLD, DF, EINC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   OFF(*), SNEW(NEL,6), QOLD(*), EINT(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*),
     .   SOLD6(*), D1(*), D2(*), D3(*), D4(*), D5(*), D6(*), POLD(*), DF(*), EINC(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   VAVG(MVSIZ), 
     .   E1(MVSIZ), E2(MVSIZ), E3(MVSIZ), E4(MVSIZ),
     .   E5(MVSIZ), E6(MVSIZ), DTA,VOLN(MVSIZ)
C-----------------------------------------------
      DTA =FOURTH*DT1
C
      DO I=LFT,LLT
        E1(I)=D1(I)*(SOLD1(I)+SNEW(I,1)+POLD(I))
        E2(I)=D2(I)*(SOLD2(I)+SNEW(I,2)+POLD(I))
        E3(I)=D3(I)*(SOLD3(I)+SNEW(I,3)+POLD(I))
        E4(I)=D4(I)*(SOLD4(I)+SNEW(I,4))
        E5(I)=D5(I)*(SOLD5(I)+SNEW(I,5))
        E6(I)=D6(I)*(SOLD6(I)+SNEW(I,6))
      ENDDO
C
      DO I=LFT,LLT
        VAVG(I)=(TWO*VOLN(I)-DVOL(I))*OFF(I)
        DVOL(I)=HALF*DVOL(I)*OFF(I)
        EINC(I)=VAVG(I)*(E1(I)+E2(I)+E3(I)+E4(I)+E5(I)+E6(I))*DTA
      ENDDO
C
      DO I=LFT,LLT
        EINC(I)=EINC(I)-DVOL(I)*(POLD(I)+QOLD(I))
        EINT(I)=EINT(I)+EINC(I)
      ENDDO
C
      RETURN
      END
