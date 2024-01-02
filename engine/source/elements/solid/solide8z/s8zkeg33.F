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
Chd|  S8ZKEG33                      source/elements/solid/solide8z/s8zkeg33.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEG33(LFT,LLT,
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, G33, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*), 
     .   G33(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,3,MVSIZ),BJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       DO I=LFT,LLT
        BI(1,1,I)=PXYI(I)
        BI(1,2,I)=PYXI(I)
        BI(1,3,I)=ZERO
        BI(2,1,I)=ZERO
        BI(2,2,I)=PYZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,1,I)=PXZI(I)
        BI(3,2,I)=ZERO
        BI(3,3,I)=PZXI(I)
       ENDDO
       DO I=1,3 
       DO J=I,3 
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(G33(1,1,EP)*BI(1,J,EP)+
     1                  G33(1,2,EP)*BI(2,J,EP)+G33(1,3,EP)*BI(3,J,EP))+
     2                BI(2,I,EP)*(G33(2,1,EP)*BI(1,J,EP)+
     3                  G33(2,2,EP)*BI(2,J,EP)+G33(2,3,EP)*BI(3,J,EP))+
     4                BI(3,I,EP)*(G33(3,1,EP)*BI(1,J,EP)+
     5                  G33(3,2,EP)*BI(2,J,EP)+G33(3,3,EP)*BI(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ELSE
       DO I=LFT,LLT
        BI(1,1,I)=PXYI(I)
        BI(1,2,I)=PYXI(I)
        BI(1,3,I)=ZERO
        BI(2,1,I)=ZERO
        BI(2,2,I)=PYZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,1,I)=PXZI(I)
        BI(3,2,I)=ZERO
        BI(3,3,I)=PZXI(I)
       ENDDO
       DO I=LFT,LLT
        BJ(1,1,I)=PXYJ(I)
        BJ(1,2,I)=PYXJ(I)
        BJ(1,3,I)=ZERO
        BJ(2,1,I)=ZERO
        BJ(2,2,I)=PYZJ(I)
        BJ(2,3,I)=PZYJ(I)
        BJ(3,1,I)=PXZJ(I)
        BJ(3,2,I)=ZERO
        BJ(3,3,I)=PZXJ(I)
       ENDDO
       DO I=1,3 
       DO J=1,3 
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(G33(1,1,EP)*BJ(1,J,EP)+
     1                  G33(1,2,EP)*BJ(2,J,EP)+G33(1,3,EP)*BJ(3,J,EP))+
     2                BI(2,I,EP)*(G33(2,1,EP)*BJ(1,J,EP)+
     3                  G33(2,2,EP)*BJ(2,J,EP)+G33(2,3,EP)*BJ(3,J,EP))+
     4                BI(3,I,EP)*(G33(3,1,EP)*BJ(1,J,EP)+
     5                  G33(3,2,EP)*BJ(2,J,EP)+G33(3,3,EP)*BJ(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ENDIF 
      RETURN
      END
