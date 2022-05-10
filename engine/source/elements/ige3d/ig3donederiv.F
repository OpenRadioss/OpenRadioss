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
Chd|  IG3DONEDERIV                  source/elements/ige3d/ig3donederiv.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|-- calls ---------------
Chd|        DERSONEBASISFUN               source/elements/ige3d/dersonebasisfun.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IG3DONEDERIV(
     1 ITEL  ,N     ,XXI   ,YYI   ,
     2 ZZI   ,WWI   ,IDX   ,IDY   ,
     3 IDZ   ,KNOTLOCX ,KNOTLOCY ,KNOTLOCZ ,
     4 DRDX  ,R     ,DETJAC,NCTRL ,
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
C DRDX        |  NCTRL,3     |  F  |   W  |  TRIVIATE NURBS FUNCTION DERIVATIVES IN PARAMETRIC COORDINATES
C GAUSSX      |  1           |  F  |   R  |  COORDONATES IN DIRECTION X OF GAUSS POINT 
C GAUSSY      |  1           |  F  |   R  |  COORDONATES IN DIRECTION Y OF GAUSS POINT 
C GAUSSZ      |  1           |  F  |   R  |  COORDONATES IN DIRECTION Z OF GAUSS POINT 
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
C DXDXI       |  3,3         |  F  |   W  |  DERIVATIVE OF PHYSICAL COORDINATES W.R.T. PARAMETRIC COORDINATES
C DXIDX       | 3,3         |  F  |   W  |  INVERSE OF DXDXI
C DXIDTILDEXI |  3,3         |  F  |   W  |  DERIVATIVE OF PARAMETRIC COORDINATES W.R.T. PARENT ELEMENT COORDINATES
C AJMAT       |  3,3         |  F  |   W  |  JACOBIAN MATRIX
C DETJAC      |  1           |  F  |   W  |  DETERMINANT OF AJMAT
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
      INTEGER IDX, IDY, IDZ, NCTRL, PX, PY, PZ, ITEL, N, BOOLG,IDX2, IDY2, IDZ2
      my_real
     .  GAUSSX, GAUSSY, GAUSSZ, DETJAC,
     .  R(*),DRDX(NCTRL,3),XXI(*),YYI(*),ZZI(*),
     .  WWI(*),KX(*), KY(*), KZ(*), KNOTLOCX(PX+2,NCTRL),
     .  KNOTLOCY(PY+2,NCTRL),KNOTLOCZ(PZ+2,NCTRL),KNOTLOCELX(2),
     .  KNOTLOCELY(2),KNOTLOCELZ(2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUMLOC, I, J, K, NA, NB, NC
      my_real
     .  DRDXI(NCTRL,3),SUMTOT, DETDXDXI, FN(NCTRL), 
     .  DNDXI(NCTRL), FM(NCTRL), DMDXI(NCTRL), FL(NCTRL),
     .  DLDXI(NCTRL), XI(3), SUMXI(3), AJMAT(3,3),
     .  DXIDTILDEXI(3,3), DXIDX(3,3), DXDXI(3,3)
C=======================================================================
C   S o u r c e  L i n e s
C=======================================================================
C
C INITIALISATION DES DIFFERENTES MATRICES LOCALES

      DXDXI(:,:)=ZERO
      DXIDTILDEXI(:,:)=ZERO
      AJMAT(:,:)=ZERO
      DRDX(:,:)=ZERO

      IF (BOOLG == 1) THEN
C CALCULATE PARAMETRIC COORDINATES OF THE GAUSS POINT FROM PARENT ELEMENT COORDINATES, FOR THE THREE DIRECTIONS
       XI(1) = ((KNOTLOCELX(2)-KNOTLOCELX(1))*GAUSSX + (KNOTLOCELX(2)+(KNOTLOCELX(1))))/DEUX
       XI(2) = ((KNOTLOCELY(2)-KNOTLOCELY(1))*GAUSSY + (KNOTLOCELY(2)+(KNOTLOCELY(1))))/DEUX
       XI(3) = ((KNOTLOCELZ(2)-KNOTLOCELZ(1))*GAUSSZ + (KNOTLOCELZ(2)+(KNOTLOCELZ(1))))/DEUX
c       XI(1) = ((KX(IDX2)-KX(IDX))*GAUSSX + (KX(IDX2)+(KX(IDX))))/DEUX
c       XI(2) = ((KY(IDY2)-KY(IDY))*GAUSSY + (KY(IDY2)+(KY(IDY))))/DEUX
c       XI(3) = ((KZ(IDZ2)-KZ(IDZ))*GAUSSZ + (KZ(IDZ2)+(KZ(IDZ))))/DEUX
      ELSE
C SI ON A EN ENTREE DES POINTS DEJA DANS LE REPERE PARAMETRIQUE ON N'A PAS BESOIN DE LES BASCULER DE L'ESPACE PARENT A L'ESPACE PARAMETRIQUE
       XI(1) = GAUSSX
       XI(2) = GAUSSY
       XI(3) = GAUSSZ
      ENDIF

C CALCULATE B-SPLINE FUNCTION AT XI POINT

      DO NUMLOC=1,NCTRL
        CALL DERSONEBASISFUN(1, PX, XI(1), KNOTLOCX(:,NUMLOC), FN(NUMLOC), DNDXI(NUMLOC))
        CALL DERSONEBASISFUN(1, PY, XI(2), KNOTLOCY(:,NUMLOC), FM(NUMLOC), DMDXI(NUMLOC))
        CALL DERSONEBASISFUN(1, PZ, XI(3), KNOTLOCZ(:,NUMLOC), FL(NUMLOC), DLDXI(NUMLOC))
      ENDDO

c      NUMLOC = 0
c      DO K=1,PZ+1
c        DO J=1,PY+1
c          DO I=1,PX+1
c            NUMLOC = NUMLOC+1
c            CALL DERSONEBASISFUN(I, 1, PX, XI(1), KNOTLOCX(:,NUMLOC), FN(NUMLOC), DNDXI(NUMLOC))
c            CALL DERSONEBASISFUN(J, 1, PY, XI(2), KNOTLOCY(:,NUMLOC), FM(NUMLOC), DMDXI(NUMLOC))
c            CALL DERSONEBASISFUN(K, 1, PZ, XI(3), KNOTLOCZ(:,NUMLOC), FL(NUMLOC), DLDXI(NUMLOC))
c          ENDDO
c        ENDDO
c      ENDDO

C BUILD NUMERATORS AND DENOMINATORS

      SUMTOT=ZERO
      SUMXI(1)=ZERO
      SUMXI(2)=ZERO
      SUMXI(3)=ZERO

      DO NUMLOC=1,NCTRL
        R(NUMLOC)=FN(NUMLOC)*FM(NUMLOC)*FL(NUMLOC)*WWI(NUMLOC) 
        SUMTOT=SUMTOT+R(NUMLOC)

        DRDXI(NUMLOC,1)=DNDXI(NUMLOC)*FM(NUMLOC)*FL(NUMLOC)*
     .                  WWI(NUMLOC)
        SUMXI(1)=SUMXI(1)+DRDXI(NUMLOC,1)
        DRDXI(NUMLOC,2)=FN(NUMLOC)*DMDXI(NUMLOC)*FL(NUMLOC)*
     .                  WWI(NUMLOC)
        SUMXI(2)=SUMXI(2)+DRDXI(NUMLOC,2)
        DRDXI(NUMLOC,3)=FN(NUMLOC)*FM(NUMLOC)*DLDXI(NUMLOC)*
     .                  WWI(NUMLOC)
        SUMXI(3)=SUMXI(3)+DRDXI(NUMLOC,3)
      ENDDO

C DIVIDE BY DENOMINATOR TO COMPLETE DEFINITION OF FUNCTION AND DERIVATIVES

      DO NUMLOC=1,NCTRL
        R(NUMLOC)=R(NUMLOC)/SUMTOT
      ENDDO

      DO I=1,3
        DO NUMLOC=1,NCTRL
          DRDXI(NUMLOC,I)=(DRDXI(NUMLOC,I)-R(NUMLOC)*SUMXI(I))/SUMTOT
        ENDDO
      ENDDO

C GRADIENT OF MAPPING FROM PARAMETER SPACE TO PHYSICAL SPACE 

      DO NB=1,3
        DO NUMLOC=1,NCTRL
          DXDXI(1,NB)=DXDXI(1,NB)+XXI(NUMLOC)*DRDXI(NUMLOC,NB)  
          DXDXI(2,NB)=DXDXI(2,NB)+YYI(NUMLOC)*DRDXI(NUMLOC,NB)  
          DXDXI(3,NB)=DXDXI(3,NB)+ZZI(NUMLOC)*DRDXI(NUMLOC,NB)
        ENDDO                             
      ENDDO

C COMPUTE INVERSE OF GRADIENT (OBTENIR DXIDX)

      DETDXDXI=DXDXI(1,2)*DXDXI(2,3)*DXDXI(3,1)
     .        -DXDXI(1,3)*DXDXI(2,2)*DXDXI(3,1)
     .        +DXDXI(1,3)*DXDXI(2,1)*DXDXI(3,2)
     .        -DXDXI(1,1)*DXDXI(2,3)*DXDXI(3,2)
     .        +DXDXI(1,1)*DXDXI(2,2)*DXDXI(3,3)
     .        -DXDXI(1,2)*DXDXI(2,1)*DXDXI(3,3)

      IF(DETDXDXI.NE.0) THEN
        DXIDX(1,1)=(DXDXI(2,2)*DXDXI(3,3)-DXDXI(2,3)*DXDXI(3,2))/DETDXDXI
        DXIDX(1,2)=(DXDXI(1,3)*DXDXI(3,2)-DXDXI(1,2)*DXDXI(3,3))/DETDXDXI
        DXIDX(1,3)=(DXDXI(1,2)*DXDXI(2,3)-DXDXI(1,3)*DXDXI(2,2))/DETDXDXI
        DXIDX(2,1)=(DXDXI(2,3)*DXDXI(3,1)-DXDXI(2,1)*DXDXI(3,3))/DETDXDXI
        DXIDX(2,2)=(DXDXI(1,1)*DXDXI(3,3)-DXDXI(1,3)*DXDXI(3,1))/DETDXDXI
        DXIDX(2,3)=(DXDXI(1,3)*DXDXI(2,1)-DXDXI(1,1)*DXDXI(2,3))/DETDXDXI
        DXIDX(3,1)=(DXDXI(2,1)*DXDXI(3,2)-DXDXI(2,2)*DXDXI(3,1))/DETDXDXI
        DXIDX(3,2)=(DXDXI(1,2)*DXDXI(3,1)-DXDXI(1,1)*DXDXI(3,2))/DETDXDXI
        DXIDX(3,3)=(DXDXI(1,1)*DXDXI(2,2)-DXDXI(1,2)*DXDXI(2,1))/DETDXDXI
      ENDIF


C GRADIENT OF MAPPING FROM PARENT ELEMENT TO PARAMETER SPACE

      DXIDTILDEXI(1,1)=(KNOTLOCELX(2)-KNOTLOCELX(1))/DEUX
      DXIDTILDEXI(2,2)=(KNOTLOCELY(2)-KNOTLOCELY(1))/DEUX
      DXIDTILDEXI(3,3)=(KNOTLOCELZ(2)-KNOTLOCELZ(1))/DEUX
c      DXIDTILDEXI(1,1)=(KX(IDX2)-KX(IDX))/DEUX
c      DXIDTILDEXI(2,2)=(KY(IDY2)-KY(IDY))/DEUX
c      DXIDTILDEXI(3,3)=(KZ(IDZ2)-KZ(IDZ))/DEUX
cc      DXIDTILDEXI(1,1)=(KX(IDX+1)-KX(IDX))/DEUX
cc      DXIDTILDEXI(2,2)=(KY(IDY+1)-KY(IDY))/DEUX
cc      DXIDTILDEXI(3,3)=(KZ(IDZ+1)-KZ(IDZ))/DEUX

C COMPUTE DERIVATES OF BASIS FUNCTIONS WITH RESPECT TO PHYSICAL COORDINATES

      DO NA=1,3
        DO NB=1,3
          DO NUMLOC=1,NCTRL
            DRDX(NUMLOC,NA)=DRDX(NUMLOC,NA)+(DRDXI(NUMLOC,NB)*DXIDX(NB,NA))
          ENDDO
          DO NC=1,3
            AJMAT(NA,NB)=AJMAT(NA,NB)+DXDXI(NA,NC)*DXIDTILDEXI(NC,NB)  
          ENDDO
        ENDDO
      ENDDO


C DETERMINANT DE LA MATRICE AJMAT (QUI REGROUPE DONC LES DEUX JACOBIENS)

      DETJAC=(AJMAT(1,1)*AJMAT(2,2)*AJMAT(3,3))
     .      +(AJMAT(1,2)*AJMAT(2,3)*AJMAT(3,1))
     .      +(AJMAT(2,1)*AJMAT(3,2)*AJMAT(1,3))
     .      -(AJMAT(1,3)*AJMAT(2,2)*AJMAT(3,1))
     .      -(AJMAT(1,2)*AJMAT(2,1)*AJMAT(3,3))
     .      -(AJMAT(2,3)*AJMAT(3,2)*AJMAT(1,1))

      RETURN 
      END






