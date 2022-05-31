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
Chd|  CDERI3                        source/elements/shell/coque/cderi3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDERI3(
     1    JFT ,JLT ,SMSTR ,OFFG,STI ,STIR,
     2    AREA,PX1 ,PX2 ,PY1 ,PY2 ,X2  ,X3  ,X4  ,Y2  ,Y3  ,  
     3    Y4  ,Z2  ,X1G ,X2G ,X3G ,X4G ,Y1G ,Y2G ,Y3G ,Y4G ,
     4    Z1G ,Z2G ,Z3G ,Z4G ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,
     5    E3X ,E3Y ,E3Z ,VHX ,VHY ,A_I ,UX1 ,UX2 ,UX3 ,UX4 ,
     7    UY1 ,UY2 ,UY3 ,UY4 ,NEL )
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
      INTEGER JFT, JLT,NEL
      my_real
     .   PX1(*), PX2(*), PY1(*), PY2(*), 
     .   OFFG(*),STI(*), STIR(*)
      my_real
     .   X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),AREA(MVSIZ),
     .   Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Z2(MVSIZ),
     .   X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ), X4G(MVSIZ),
     .   Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ), Y4G(MVSIZ),
     .   Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ), Z4G(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   VHX(MVSIZ), VHY(MVSIZ), A_I(MVSIZ),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ)
      DOUBLE PRECISION
     .  SMSTR(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II(6),J
C     REAL ou REAL*8
      my_real
     .     X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ), X31G(MVSIZ), 
     .     Y31G(MVSIZ), Z31G(MVSIZ), 
     .     X41G(MVSIZ), Y41G(MVSIZ), Z41G(MVSIZ)
      my_real
     .     X21GA, Y21GA, Z21GA, X31GA, Y31GA, Z31GA,
     .     X41GA, Y41GA, Z41GA
C-----------------------------------------------
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
      DO I=JFT,JLT                                    
        STI(I) = ZERO                                 
        STIR(I)= ZERO                                 
        X21GA=X2G(I)-X1G(I)                           
        Y21GA=Y2G(I)-Y1G(I)                           
        Z21GA=Z2G(I)-Z1G(I)                           
        X31GA=X3G(I)-X1G(I)                           
        Y31GA=Y3G(I)-Y1G(I)                           
        Z31GA=Z3G(I)-Z1G(I)                           
        X41GA=X4G(I)-X1G(I)                           
        Y41GA=Y4G(I)-Y1G(I)                           
        Z41GA=Z4G(I)-Z1G(I)                           
C
        X2(I)=E1X(I)*X21GA+E1Y(I)*Y21GA+E1Z(I)*Z21GA  
        Y2(I)=E2X(I)*X21GA+E2Y(I)*Y21GA+E2Z(I)*Z21GA  
        Y3(I)=E2X(I)*X31GA+E2Y(I)*Y31GA+E2Z(I)*Z31GA  
        X3(I)=E1X(I)*X31GA+E1Y(I)*Y31GA+E1Z(I)*Z31GA  
        X4(I)=E1X(I)*X41GA+E1Y(I)*Y41GA+E1Z(I)*Z41GA  
        Y4(I)=E2X(I)*X41GA+E2Y(I)*Y41GA+E2Z(I)*Z41GA  
        Z2(I)=E3X(I)*X21GA+E3Y(I)*Y21GA+E3Z(I)*Z21GA 
      ENDDO                                           
C
      IF (ISMSTR == 11) THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I)) == ONE)OFFG(I)=SIGN(TWO,OFFG(I))
          UX1(I) = ZERO
          UY1(I) = ZERO
          UX2(I) = ZERO
          UY2(I) = ZERO
          UX3(I) = ZERO
          UY3(I) = ZERO
          UX4(I) = ZERO
          UY4(I) = ZERO
          IF(ABS(OFFG(I)) == TWO)THEN
            UX2(I) = X2(I)-SMSTR(II(1)+I)
            UY2(I) = Y2(I)-SMSTR(II(2)+I)
            UX3(I) = X3(I)-SMSTR(II(3)+I)
            UY3(I) = Y3(I)-SMSTR(II(4)+I)
            UX4(I) = X4(I)-SMSTR(II(5)+I)
            UY4(I) = Y4(I)-SMSTR(II(6)+I)
            X2(I)  = SMSTR(II(1)+I)
            Y2(I)  = SMSTR(II(2)+I)
            X3(I)  = SMSTR(II(3)+I)
            Y3(I)  = SMSTR(II(4)+I)
            X4(I)  = SMSTR(II(5)+I)
            Y4(I)  = SMSTR(II(6)+I)
            Z2(I)  = ZERO
          ELSE
            SMSTR(II(1)+I)=X2(I)
            SMSTR(II(2)+I)=Y2(I)
            SMSTR(II(3)+I)=X3(I)
            SMSTR(II(4)+I)=Y3(I)
            SMSTR(II(5)+I)=X4(I)
            SMSTR(II(6)+I)=Y4(I)
          ENDIF
        ENDDO
      ELSEIF(ISMSTR == 1.OR.ISMSTR == 2)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I)) == TWO)THEN
            X2(I)=SMSTR(II(1)+I)
            Y2(I)=SMSTR(II(2)+I)
            X3(I)=SMSTR(II(3)+I)
            Y3(I)=SMSTR(II(4)+I)
            X4(I)=SMSTR(II(5)+I)
            Y4(I)=SMSTR(II(6)+I)
            Z2(I)=ZERO
          ELSE
            SMSTR(II(1)+I)=X2(I)
            SMSTR(II(2)+I)=Y2(I)
            SMSTR(II(3)+I)=X3(I)
            SMSTR(II(4)+I)=Y3(I)
            SMSTR(II(5)+I)=X4(I)
            SMSTR(II(6)+I)=Y4(I)
          ENDIF
        ENDDO
        IF (ISMSTR == 1) THEN
          DO I=JFT,JLT
            IF (OFFG(I) == ONE) OFFG(I)=TWO
          ENDDO
        ENDIF
      ENDIF
C
      DO 40 I=JFT,JLT
      PX1(I)= HALF*(Y2(I)-Y4(I))
      PY1(I)= HALF*(X4(I)-X2(I))
      PX2(I)= HALF* Y3(I)
      PY2(I)=-HALF* X3(I)     
   40 CONTINUE
C
      DO  I=JFT,JLT
        AREA(I)= MAX(TWO*(PY2(I)*PX1(I)-PY1(I)*PX2(I)),EM20)
        A_I(I) = ONE / AREA(I)
      ENDDO
C
C     CALCUL POUR VECTEURS HOURGLASS
C
      DO I=JFT,JLT
        VHX(I)=(-X2(I)+X3(I)-X4(I))/AREA(I)
        VHY(I)=(-Y2(I)+Y3(I)-Y4(I))/AREA(I)
      ENDDO
C-----------
      RETURN
      END
