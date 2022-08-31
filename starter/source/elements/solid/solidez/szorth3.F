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
Chd|  STORTH3                       source/elements/solid/solidez/szorth3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORTH3(ISORTH,NEL,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GAMA)
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
      INTEGER :: ISORTH, NEL
C
      my_real
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF (ISORTH == 1) THEN 
        DO I=1,NEL
         G1X(I)=GAMA(I,1)
         G1Y(I)=GAMA(I,2)
         G1Z(I)=GAMA(I,3)
         G2X(I)=GAMA(I,4)
         G2Y(I)=GAMA(I,5)
         G2Z(I)=GAMA(I,6)
         G3X(I)=G1Y(I)*G2Z(I)-G1Z(I)*G2Y(I)
         G3Y(I)=G1Z(I)*G2X(I)-G1X(I)*G2Z(I)
         G3Z(I)=G1X(I)*G2Y(I)-G1Y(I)*G2X(I)
        END DO
      ELSE
        DO I=1,NEL
         G1X(I)=ONE
         G1Y(I)=ZERO
         G1Z(I)=ZERO
         G2X(I)=ZERO
         G2Y(I)=ONE
         G2Z(I)=ZERO
         G3X(I)=ZERO
         G3Y(I)=ZERO
         G3Z(I)=ONE
        END DO
      ENDIF 
C
      RETURN
      END SUBROUTINE STORTH3
Chd|====================================================================
Chd|  SZORDEF3                      source/elements/solid/solidez/szorth3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|        SROTO3                        source/elements/solid/solidez/szorth3.F
Chd|====================================================================
      SUBROUTINE SZORDEF3(NEL,DXX,DYY,DZZ,D4,D5,D6,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
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
      INTEGER :: NEL
C
      my_real
     .   DXX(*),DYY(*),DZZ(*),D4(*),D5(*),D6(*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      my_real
     .   DEF(6,MVSIZ),DEFN(6,MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
        DEF(1,I) = DXX(I)
        DEF(2,I) = DYY(I)
        DEF(3,I) = DZZ(I)
        DEF(4,I) = HALF*D4(I)
        DEF(5,I) = HALF*D5(I)
        DEF(6,I) = HALF*D6(I)
      ENDDO
      CALL SROTO3(NEL,DEF,DEFN,
     .            G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
      DO I=1,NEL
        DXX(I) = DEFN(1,I)
        DYY(I) = DEFN(2,I)
        DZZ(I) = DEFN(3,I)
        D4(I) = TWO*DEFN(4,I)
        D5(I) = TWO*DEFN(5,I)
        D6(I) = TWO*DEFN(6,I)
      ENDDO
C
      RETURN
      END SUBROUTINE SZORDEF3
Chd|====================================================================
Chd|  SROTO3                        source/elements/solid/solidez/szorth3.F
Chd|-- called by -----------
Chd|        SZORDEF3                      source/elements/solid/solidez/szorth3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO3(NEL,SIG,SIGN,
     .                  G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
C
      my_real
     .   SIG(6,*),SIGN(6,*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=1,NEL
         SX = SIG(1,I)*G1X(I)+SIG(4,I)*G1Y(I)+SIG(6,I)*G1Z(I)
         SY = SIG(4,I)*G1X(I)+SIG(2,I)*G1Y(I)+SIG(5,I)*G1Z(I)
         SZ = SIG(6,I)*G1X(I)+SIG(5,I)*G1Y(I)+SIG(3,I)*G1Z(I)
         SIGN(1,I) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(4,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(6,I) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(1,I)*G2X(I)+SIG(4,I)*G2Y(I)+SIG(6,I)*G2Z(I)
         SY = SIG(4,I)*G2X(I)+SIG(2,I)*G2Y(I)+SIG(5,I)*G2Z(I)
         SZ = SIG(6,I)*G2X(I)+SIG(5,I)*G2Y(I)+SIG(3,I)*G2Z(I)
         SIGN(2,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(5,I) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(1,I)*G3X(I)+SIG(4,I)*G3Y(I)+SIG(6,I)*G3Z(I)
         SY = SIG(4,I)*G3X(I)+SIG(2,I)*G3Y(I)+SIG(5,I)*G3Z(I)
         SZ = SIG(6,I)*G3X(I)+SIG(5,I)*G3Y(I)+SIG(3,I)*G3Z(I)
         SIGN(3,I) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SROTO3
