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
Chd|  QRCOOR2                       source/elements/solid_2d/quad/qrcoor2.F
Chd|-- called by -----------
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QRCOOR2(X,IXQ,NGL  ,MXT   , 
     .         PID, IX1, IX2, IX3, IX4,
     .         Y1, Y2, Y3, Y4,
     .         Z1, Z2, Z3, Z4,
     .         SY, SZ, TY, TZ,
     .         E1Y, E1Z, E2Y, E2Z)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), NGL(*), MXT(*), PID(*)
      INTEGER IX1(*), IX2(*), IX3(*), IX4(*)
C     REAL
      my_real
     .   X(3,*),E1Y(*),E1Z(*),E2Y(*),E2Z(*)
      my_real
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SY(MVSIZ) ,SZ(MVSIZ) ,
     .   TY(MVSIZ) ,TZ(MVSIZ), SUMA,CS,CT
C--------------------------------------------------
C--------------------------------------------------
C     CONNECTIVITES ET MATERIEL
C--------------------------------------------------
      DO I=LFT,LLT
        MXT(I)=IXQ(1,I)
        IX1(I)=IXQ(2,I)
        IX2(I)=IXQ(3,I)
        IX3(I)=IXQ(4,I)
        IX4(I)=IXQ(5,I)
        PID(I)=IXQ(6,I)
        NGL(I)=IXQ(NIXQ,I)
      END DO
C
C----------------------------
C     COORDONNEES NOEUDS    |
C----------------------------
      DO I=LFT,LLT
        Y1(I)=X(2,IX1(I))
        Z1(I)=X(3,IX1(I))
        Y2(I)=X(2,IX2(I))
        Z2(I)=X(3,IX2(I))
        Y3(I)=X(2,IX3(I))
        Z3(I)=X(3,IX3(I))
        Y4(I)=X(2,IX4(I))
        Z4(I)=X(3,IX4(I))
      END DO
C---------------------------------------
C     REPERE LOCAL (ISOPARAMETRIQUE)
C---------------------------------------
      DO I=LFT,LLT
        SY(I)=HALF*(Y2(I)+Y3(I)-Y1(I)-Y4(I))
        SZ(I)=HALF*(Z2(I)+Z3(I)-Z1(I)-Z4(I))
        TY(I)=HALF*(Y3(I)+Y4(I)-Y1(I)-Y2(I))
        TZ(I)=HALF*(Z3(I)+Z4(I)-Z1(I)-Z2(I))
      END DO
C-----------
C     REPERE CONVECTE ORTHOGONALISE 
C     full integ: Same than the shell 
C-----------
      DO I=LFT,LLT
        CT = TY(I)*TY(I)+TZ(I)*TZ(I)
        CS = SY(I)*SY(I)+SZ(I)*SZ(I)
        IF(CS /= ZERO) THEN
          SUMA = SQRT(CT/MAX(EM20,CS))
          E1Y(I) = SY(I)*SUMA + TZ(I)
          E1Z(I) = SZ(I)*SUMA - TY(I)
        ELSEIF(CT /= ZERO)THEN
          SUMA = SQRT(CS/MAX(EM20,CT))
          E1Y(I) = SY(I) + TZ(I)*SUMA
          E1Z(I) = SZ(I) - TY(I)*SUMA
        END IF 
        SUMA=SQRT(E1Y(I)**2+E1Z(I)**2)
        SUMA=ONE/MAX(SUMA,EM20)
        E1Y(I)=E1Y(I)*SUMA
        E1Z(I)=E1Z(I)*SUMA
        E2Y(I)=-E1Z(I)
        E2Z(I)= E1Y(I)
      ENDDO
C-----------
      RETURN
      END
