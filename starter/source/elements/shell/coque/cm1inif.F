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
Chd|  CM1INIF                       source/elements/shell/coque/cm1inif.F
Chd|-- called by -----------
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CM1INIF(JFT  ,JLT  ,FOR  ,MOM  ,ITHK ,
     .                   THK  ,EINT ,NU   ,G    ,A1   ,
     .                   A2   ,GS   ,KXX  ,KYY  ,KXY  ,
     .                   EXX  ,EYY  ,EXY  ,EYZ  ,EXZ  ,
     .                   AREA )
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
      INTEGER JFT, JLT, ITHK
      my_real
     .    EXX(MVSIZ) , EYY(MVSIZ) , EXY(MVSIZ) ,
     .    EXZ(MVSIZ) , EYZ(MVSIZ) , KXX(MVSIZ) ,
     .    KYY(MVSIZ) , KXY(MVSIZ) , AREA(MVSIZ)
      my_real
     .   FOR(5,*) ,MOM(3,*), THK(*), EINT(2,*), NU(*),
     .   G(*), A1(*), A2(*), GS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real THK08, EZZ,
     .    DEGMB(MVSIZ), DEGFX(MVSIZ), B1(MVSIZ), B2(MVSIZ), B3(MVSIZ)
C=======================================================================
      DO I=JFT,JLT
        THK08  =THK(I)*ONE_OVER_12
        B1(I)  =A1(I)*THK08
        B2(I)  =A2(I)*THK08
        B3(I)  =G(I) *THK08
      ENDDO
C
      DO I=JFT,JLT
        FOR(1,I)=A1(I)*EXX(I)+A2(I)*EYY(I)
        FOR(2,I)=A1(I)*EYY(I)+A2(I)*EXX(I)
        FOR(3,I)=G(I) *EXY(I)
        FOR(4,I)=GS(I)*EYZ(I)
        FOR(5,I)=GS(I)*EXZ(I)
C
        MOM(1,I)=B1(I)*KXX(I)+B2(I)*KYY(I)
        MOM(2,I)=B1(I)*KYY(I)+B2(I)*KXX(I)
        MOM(3,I)=B3(I)*KXY(I)
      ENDDO
C
      DO I=JFT,JLT
        DEGMB(I) = FOR(1,I)*EXX(I)+FOR(2,I)*EYY(I)+FOR(3,I)*EXY(I)
        DEGFX(I) = MOM(1,I)*KXX(I)+MOM(2,I)*KYY(I)
     +                   +MOM(3,I)*KXY(I)
        EINT(1,I) = DEGMB(I)*HALF*THK(I)*AREA(I)
        EINT(2,I) = DEGFX(I)*HALF*THK(I)*THK(I)*AREA(I)
      ENDDO
      IF(ITHK==1) THEN
        DO I=JFT,JLT
          EZZ = -NU(I) * (EXX(I) + EYY(I)) / (ONE-NU(I))
          THK(I) = THK(I) * (ONE + EZZ)
        ENDDO
      ENDIF
C-----------
      RETURN
      END



