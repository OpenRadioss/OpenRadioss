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
Chd|  C3DERI3                       source/elements/sh3n/coque3n/c3deri3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3DERI3(JFT      ,JLT      ,PX1     ,PY1      ,PY2      ,
     .                   SMSTR    ,OFFG     ,ISMSTR  ,ALPE     ,ALDT     ,
     .                   UX1      ,UX2      ,UX3     ,UY1      ,UY2      ,
     .                   UY3      ,NEL      ,AREA    ,X21G     ,Y21G     ,
     .                   Z21G     ,X31G     ,Y31G    ,Z31G     ,X2       ,
     .                   Y2       ,X3       ,Y3      ,
     .                   E1X      ,E1Y      ,E1Z     ,E2X      ,
     .                   E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )
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
      INTEGER JFT, JLT,ISMSTR,NEL
      my_real
     .   PX1(*),PY1(*),PY2(*),OFFG(*),ALPE(MVSIZ),AREA(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UY1(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),X3(MVSIZ),Y3(MVSIZ),
     .   X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .   X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ),
     .   UY2(MVSIZ),UY3(MVSIZ),ALDT(MVSIZ)
      DOUBLE PRECISION SMSTR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II(3)
      my_real AL1, AL2, AL3, ALMAX
C=======================================================================
      DO I=JFT,JLT
        X2(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
        Y2(I)=E2X(I)*X21G(I)+E2Y(I)*Y21G(I)+E2Z(I)*Z21G(I)
        X3(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
        Y3(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
      ENDDO
C
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
C
      IF (ISMSTR == 11) THEN
        DO I=JFT,JLT
          IF (ABS(OFFG(I)) == ONE) OFFG(I)=SIGN(TWO,OFFG(I))
        ENDDO
        DO I=JFT,JLT
          UX1(I) = ZERO
          UY1(I) = ZERO
          UX2(I) = ZERO
          UY2(I) = ZERO
          UX3(I) = ZERO
          UY3(I) = ZERO
          IF(ABS(OFFG(I)) == TWO)THEN
            UX2(I) = X2(I)-SMSTR(II(1)+I)
            UX3(I) = X3(I)-SMSTR(II(2)+I)
            UY3(I) = Y3(I)-SMSTR(II(3)+I)
            X2(I)  = SMSTR(II(1)+I)
            X3(I)  = SMSTR(II(2)+I)
            Y3(I)  = SMSTR(II(3)+I)
            AREA(I) = HALF*X2(I)*Y3(I)
          ELSE
            SMSTR(II(1)+I)=X2(I)
            SMSTR(II(2)+I)=X3(I)
            SMSTR(II(3)+I)=Y3(I)
          ENDIF
        ENDDO
      ELSEIF (ISMSTR == 1.OR.ISMSTR == 2) THEN
        DO I=JFT,JLT
          IF(OFFG(I) == TWO)THEN
            X2(I)=SMSTR(II(1)+I)
            X3(I)=SMSTR(II(2)+I)
            Y3(I)=SMSTR(II(3)+I)
            AREA(I) = HALF*X2(I)*Y3(I)
          ELSE
            SMSTR(II(1)+I)=X2(I)
            SMSTR(II(2)+I)=X3(I)
            SMSTR(II(3)+I)=Y3(I)
          ENDIF
        ENDDO
      ENDIF
      IF (ISMSTR == 1) THEN
        DO I=JFT,JLT
          IF (OFFG(I) == ONE) OFFG(I)=TWO
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        Y3(I) = SIGN(MAX(EM15,ABS(Y3(I))),Y3(I))
        PX1(I)=-HALF*Y3(I)
        PY1(I)= HALF*(X3(I)-X2(I))
        PY2(I)=-HALF*X3(I)
      ENDDO
C
      DO I=JFT,JLT
        AL1 = X2(I) * X2(I)
        AL2 = (X3(I)-X2(I)) * (X3(I)-X2(I)) + Y3(I) * Y3(I)
        AL3 = X3(I) * X3(I) + Y3(I) * Y3(I)
        ALMAX = MAX(AL1,AL2,AL3)
        ALDT(I)= TWO*AREA(I) / SQRT(ALMAX)
        ALPE(I)=ONE
      ENDDO
C---------------------------------------------------------
      RETURN
C
      END
