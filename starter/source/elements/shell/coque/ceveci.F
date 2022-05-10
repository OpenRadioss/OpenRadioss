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
Chd|  CEVECI                        source/elements/shell/coque/ceveci.F
Chd|-- called by -----------
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CFINT_REG_INI                 source/elements/shell/coque/cfint_reg_ini.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CEVECI(JFT,JLT,AREA,
     .           X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .           Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .           E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r 
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y  A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: 
     .                           X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: AREA,
     .                   E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real SUMA,S1,S2
      my_real, DIMENSION(MVSIZ)  :: X21,Y21,Z21,X31,Y31,Z31,X42,Y42,Z42,SUM
C=======================================================================
      DO I=JFT,JLT
        X21(I)=X2(I)-X1(I)
        Y21(I)=Y2(I)-Y1(I)
        Z21(I)=Z2(I)-Z1(I)
        X31(I)=X3(I)-X1(I)
        Y31(I)=Y3(I)-Y1(I)
        Z31(I)=Z3(I)-Z1(I)
        X42(I)=X4(I)-X2(I)
        Y42(I)=Y4(I)-Y2(I)
        Z42(I)=Z4(I)-Z2(I)
      ENDDO
C
      DO I=JFT,JLT
        E3X(I)=Y31(I)*Z42(I)-Z31(I)*Y42(I)
        E3Y(I)=Z31(I)*X42(I)-X31(I)*Z42(I)
        E3Z(I)=X31(I)*Y42(I)-Y31(I)*X42(I)
        SUM(I)=SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        AREA(I)=UNDEMI*SUM(I)
      ENDDO       
C---
      IF (ISHFRAM == 1) THEN
C---    repere non orthogonalise vers 3.0 
        DO I=JFT,JLT
          E3X(I)=E3X(I)/SUM(I)
          E3Y(I)=E3Y(I)/SUM(I)
          E3Z(I)=E3Z(I)/SUM(I)
        ENDDO
        DO I=JFT,JLT
          SUM(I)= X21(I)*E3X(I)+Y21(I)*E3Y(I)+Z21(I)*E3Z(I)
          E1X(I)= X21(I)-E3X(I)*SUM(I)
          E1Y(I)= Y21(I)-E3Y(I)*SUM(I)
          E1Z(I)= Z21(I)-E3Z(I)*SUM(I)
        ENDDO
C
        DO I=JFT,JLT
          SUM(I)=SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
          E1X(I)=E1X(I)/SUM(I)
          E1Y(I)=E1Y(I)/SUM(I)
          E1Z(I)=E1Z(I)/SUM(I)
        ENDDO
C
        DO I=JFT,JLT
          E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
          E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
          E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        ENDDO
C
        DO I=JFT,JLT
          SUM(I)=SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
          E2X(I)=E2X(I)/SUM(I)
          E2Y(I)=E2Y(I)/SUM(I)
          E2Z(I)=E2Z(I)/SUM(I)
        ENDDO
      ELSEIF (ISHFRAM == 2) THEN
C---    repere orthogonalise vers 4.0 
        DO I=JFT,JLT
          E1X(I)= X2(I)+X3(I)-X1(I)-X4(I)
          E1Y(I)= Y2(I)+Y3(I)-Y1(I)-Y4(I)
          E1Z(I)= Z2(I)+Z3(I)-Z1(I)-Z4(I)
C
          E2X(I)= X3(I)+X4(I)-X1(I)-X2(I)
          E2Y(I)= Y3(I)+Y4(I)-Y1(I)-Y2(I)
          E2Z(I)= Z3(I)+Z4(I)-Z1(I)-Z2(I)
C
          E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
          E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
          E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          E1X(I) = E1X(I)*SUMA + E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I)
          E1Y(I) = E1Y(I)*SUMA + E2Z(I)*E3X(I)-E2X(I)*E3Z(I)
          E1Z(I) = E1Z(I)*SUMA + E2X(I)*E3Y(I)-E2Y(I)*E3X(I)
        ENDDO
        DO I=JFT,JLT
          SUMA=E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)
          SUMA=UN/MAX(SQRT(SUMA),EM20)
          E1X(I)=E1X(I)*SUMA
          E1Y(I)=E1Y(I)*SUMA
          E1Z(I)=E1Z(I)*SUMA
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)
          SUMA=UN/MAX(SQRT(SUMA),EM20)
          E3X(I)=E3X(I)*SUMA
          E3Y(I)=E3Y(I)*SUMA
          E3Z(I)=E3Z(I)*SUMA
C
          E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
          E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
          E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          SUMA=UN/MAX(SQRT(SUMA),EM20)
          E2X(I)=E2X(I)*SUMA
          E2Y(I)=E2Y(I)*SUMA
          E2Z(I)=E2Z(I)*SUMA
        ENDDO
      ELSE
C---    repere orthogonalise vers 5.0 
        DO I=JFT,JLT
          E1X(I) = X2(I)+X3(I)-X1(I)-X4(I)
          E1Y(I) = Y2(I)+Y3(I)-Y1(I)-Y4(I)
          E1Z(I) = Z2(I)+Z3(I)-Z1(I)-Z4(I)
C
          E2X(I) = X3(I)+X4(I)-X1(I)-X2(I)
          E2Y(I) = Y3(I)+Y4(I)-Y1(I)-Y2(I)
          E2Z(I) = Z3(I)+Z4(I)-Z1(I)-Z2(I)
C
          E3X(I) = E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
          E3Y(I) = E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
          E3Z(I) = E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
        ENDDO
C---
        DO I=JFT,JLT                         
          SUMA   = E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)  
          SUMA   = UN/MAX(SQRT(SUMA),EM20)                    
          E3X(I) = E3X(I)*SUMA                              
          E3Y(I) = E3Y(I)*SUMA                              
          E3Z(I) = E3Z(I)*SUMA                              
C
          S1     = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I) 
          S2     = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I) 
          SUMA   = SQRT(S1/S2)                
          E1X(I) = E1X(I) + (E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I))*SUMA
          E1Y(I) = E1Y(I) + (E2Z(I)*E3X(I)-E2X(I)*E3Z(I))*SUMA
          E1Z(I) = E1Z(I) + (E2X(I)*E3Y(I)-E2Y(I)*E3X(I))*SUMA
C
          SUMA   = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
          SUMA   = UN/MAX(SQRT(SUMA),EM20)                    
          E1X(I) = E1X(I)*SUMA                              
          E1Y(I) = E1Y(I)*SUMA                              
          E1Z(I) = E1Z(I)*SUMA                              
C
          E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
          E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
          E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
        ENDDO
      ENDIF
C
      RETURN
      END
