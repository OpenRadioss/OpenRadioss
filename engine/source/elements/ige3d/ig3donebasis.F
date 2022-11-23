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
Chd|  IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|-- called by -----------
Chd|        DEPLAFAKEIGE                  source/assembly/deplafakeige.F
Chd|        I7ASSIGEO0                    source/interfaces/int07/i7ass3.F
Chd|        PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|-- calls ---------------
Chd|        ONEBASISFUN                   source/elements/ige3d/onebasisfun.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IG3DONEBASIS(
     1 ITEL  ,N     ,XXI   ,YYI   ,
     2 ZZI   ,WWI   ,IDX   ,IDY   ,
     3 IDZ   ,KNOTLOCX ,KNOTLOCY ,KNOTLOCZ ,
     4 R     ,NCTRL ,
     5 GAUSSX,GAUSSY,GAUSSZ,KX    ,
     6 KY    ,KZ    ,PX    ,
     7 PY    ,PZ    ,BOOLG ,
     8 IDX2  ,IDY2  ,IDZ2  ,
     9 KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)  
C--------------------------------------------------------------------------------------------------------
C
C This subroutine calculates the vector of local shape functions R and an array of their derivatives dR.dx
C The Jacobian determinant J may be returned to if it's necessary
C This subroutine is called by ig3dinit3 for every LR or Truncated element
C
C--------------------------------------------------------------------------------------------------------
C VAR         | SIZE      |  TYP  |  RW   |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C ITEL        |  1           | I  |  R  |  ELEMENT ID 
C N           |  1           |  I  |   R   | GAUSS POINT ID
C XXI         | NCTRL       | F  |  R  | X COORDINATE of ELEMENT I CONTROL POINTS in global frame
C YYI         | NCTRL       | F  |  R  | Y COORDINATE of ELEMENT I CONTROL POINTS in global frame
C ZZI         | NCTRL       | F  |  R  | Z COORDINATE of ELEMENT I CONTROL POINTS in global frame
C WWI         |  NCTRL       |  F  |   R  |  WEIGHT OF ELEMENT I CONTROL POINTS 
C IDX         |  1           |  I  |   R  |  ELEMENT FIRST INDEX IN KNOT VECTOR IN X DIRECTION
C IDY         |  1           |  I  |   R  |  ELEMENT FIRST INDEX IN KNOT VECTOR IN Y DIRECTION
C IDZ         |  1           |  I  |   R  |  ELEMENT FIRST INDEX IN KNOT VECTOR IN Z DIRECTION
C IDX2        |  1           |  I  |   R  |  ELEMENT LAST INDEX IN KNOT VECTOR IN X DIRECTION
C IDY2        |  1           |  I  |   R  |  ELEMENT LAST INDEX IN KNOT VECTOR IN Y DIRECTION
C IDZ2        |  1           |  I  |   R  |  ELEMENT LAST INDEX IN KNOT VECTOR IN Z DIRECTION
C R           |  NCTRL       |  F  |   W  |  ARRAY OF TRIVIATE NURBS BASIS FUNCTION
C GAUSSX      |  1           |  F  |   R  |  COORDINATES IN DIRECTION X OF GAUSS POINT 
C GAUSSY      |  1           |  F  |   R  |  COORDINATES IN DIRECTION Y OF GAUSS POINT 
C GAUSSZ      |  1           |  F  |   R  |  COORDINATES IN DIRECTION Z OF GAUSS POINT 
C NCTRL       |  1           |  I  |   R  |  NUMBER OF ELEMENT CONTROL POINTS IN CURRENT GROUP
C PX          |  1           | I  |   R  |  POLYNOMIAL INTERPOLATION DEGREE IN X DIRECTION
C PY          |  1           | I  |   R  |  POLYNOMIAL INTERPOLATION DEGREE IN Y DIRECTION
C PZ          |  1           | I  |   R  |  POLYNOMIAL INTERPOLATION DEGREE IN Z DIRECTION
C KX          |  NKX         |  F  |   R  |  (FULL) KNOT VECTOR IN X DIRECTION FOR THE CURRENT PATCH
C KY          |  NKY         |  F  |   R  |  (FULL) KNOT VECTOR IN Y DIRECTION FOR THE CURRENT PATCH
C KZ          |  NKZ         |  F  |   R  |  (FULL) KNOT VECTOR IN Z DIRECTION FOR THE CURRENT PATCH
C KNOTLOCX    | PX+2,NCTRL  |  F  |   R  |  LOCAL KNOT VECTOR IN X DIRECTION FOR EACH CONTROL POINT
C KNOTLOCY    |  PY+2,NCTRL  |  F  |   R  |  LOCAL KNOT VECTOR IN Y DIRECTION FOR EACH CONTROL POINT
C KNOTLOCZ    |  PZ+2,NCTRL  |  F  |   R  |  LOCAL KNOT VECTOR IN Z DIRECTION FOR EACH CONTROL POINT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include     "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDX, IDY, IDZ, NCTRL, PX, PY, PZ,
     .        ITEL, N, BOOLG,IDX2, IDY2, IDZ2
      my_real
     .  GAUSSX, GAUSSY, GAUSSZ,
     .  R(*),XXI(*),YYI(*),ZZI(*),
     .  WWI(*),KX(*), KY(*), KZ(*), KNOTLOCX(PX+2,NCTRL),
     .  KNOTLOCY(PY+2,NCTRL),KNOTLOCZ(PZ+2,NCTRL),KNOTLOCELX(2),
     .  KNOTLOCELY(2),KNOTLOCELZ(2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUMLOC, I, J, K, NA, NB, NC
      my_real
     .  SUMTOT, FN(NCTRL), FM(NCTRL), FL(NCTRL),
     .  XI(3)
C=======================================================================
C   S o u r c e  L i n e s
C=======================================================================
C
      IF (BOOLG == 1) THEN
C CALCULATE PARAMETRIC COORDINATES OF THE GAUSS POINT FROM PARENT ELEMENT COORDINATES, FOR THE THREE DIRECTIONS
       XI(1) = ((KNOTLOCELX(2)-KNOTLOCELX(1))*GAUSSX + (KNOTLOCELX(2)+(KNOTLOCELX(1))))/TWO
       XI(2) = ((KNOTLOCELY(2)-KNOTLOCELY(1))*GAUSSY + (KNOTLOCELY(2)+(KNOTLOCELY(1))))/TWO
       XI(3) = ((KNOTLOCELZ(2)-KNOTLOCELZ(1))*GAUSSZ + (KNOTLOCELZ(2)+(KNOTLOCELZ(1))))/TWO
c       XI(1) = ((KX(IDX2)-KX(IDX))*GAUSSX + (KX(IDX2)+(KX(IDX))))/TWO
c       XI(2) = ((KY(IDY2)-KY(IDY))*GAUSSY + (KY(IDY2)+(KY(IDY))))/TWO
c       XI(3) = ((KZ(IDZ2)-KZ(IDZ))*GAUSSZ + (KZ(IDZ2)+(KZ(IDZ))))/TWO
      ELSE
C SI ON A EN ENTREE DES POINTS DEJA DANS LE REPERE PARAMETRIQUE ON N'A PAS BESOIN DE LES BASCULER DE L'ESPACE PARENT A L'ESPACE PARAMETRIQUE
       XI(1) = GAUSSX
       XI(2) = GAUSSY
       XI(3) = GAUSSZ
      ENDIF

C CALCULATE B-SPLINE FUNCTION AT XI POINT

c      DO NUMLOC=1,NCTRL
c        CALL ONEBASISFUN(1, PX, XI(1), KNOTLOCX(:,NUMLOC), FN(NUMLOC))
c        CALL ONEBASISFUN(1, PY, XI(2), KNOTLOCY(:,NUMLOC), FM(NUMLOC))
c        CALL ONEBASISFUN(1, PZ, XI(3), KNOTLOCZ(:,NUMLOC), FL(NUMLOC))
c      ENDDO

      NUMLOC = 0
      DO K=1,PZ+1
        DO J=1,PY+1
          DO I=1,PX+1
            NUMLOC = NUMLOC+1
            CALL ONEBASISFUN(I, 1, PX, XI(1), KNOTLOCX(:,NUMLOC), FN(NUMLOC))
            CALL ONEBASISFUN(J, 1, PY, XI(2), KNOTLOCY(:,NUMLOC), FM(NUMLOC))
            CALL ONEBASISFUN(K, 1, PZ, XI(3), KNOTLOCZ(:,NUMLOC), FL(NUMLOC))
          ENDDO
        ENDDO
      ENDDO

C BUILD NUMERATORS AND DENOMINATORS

      SUMTOT=ZERO

      DO NUMLOC=1,NCTRL
        R(NUMLOC)=FN(NUMLOC)*FM(NUMLOC)*FL(NUMLOC)*WWI(NUMLOC) 
        SUMTOT=SUMTOT+R(NUMLOC)
      ENDDO

C DIVIDE BY DENOMINATOR TO COMPLETE DEFINITION OF FUNCTION

      DO NUMLOC=1,NCTRL
        R(NUMLOC)=R(NUMLOC)/SUMTOT
      ENDDO

      RETURN 
      END






