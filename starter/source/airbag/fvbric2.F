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
Chd|  FVBRIC2                       source/airbag/fvbric2.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVBRIC2(IBUF  , ELEM, IXS   , NEL , NBRIC,
     .                   TBRIC , TFAC, TAGELS, NELA,
     .                   IBUFA , NNA , ELEMA , 
     .                   TAGELA, BRNA, NB_NODE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*), IXS(NIXS,*), NEL, NBRIC,
     .        NELA, 
     .        NNA, ELEMA(3,*), TAGELA(*), BRNA(8,*), NB_NODE
      INTEGER, DIMENSION(NNA), INTENT(IN) :: IBUFA
      INTEGER, DIMENSION(NEL), INTENT(IN) :: TAGELS
      INTEGER, DIMENSION(2, NBRIC), INTENT(IN) :: TBRIC
      INTEGER, DIMENSION(12, NBRIC), INTENT(IN) :: TFAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITABINV(NB_NODE), NFAC, J, K, KK, N1, N2, N3, N4, JJ
      INTEGER FAC4(3,4), FAC8(4,6), FAC6(4,5), NOD6(5)
      INTEGER FAC5(4,5), NOD5(5), NFACE(4), NTYPE
      DATA FAC4 /1,5,3,
     .           3,5,6,
     .           6,5,1,
     .           1,3,6/
      DATA FAC8 /1,4,3,2,
     .           5,6,7,8,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7,
     .           4,1,5,8/
      DATA FAC6 /1,3,2,0,
     .           5,6,7,0,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7/
      DATA NOD6 /3,3,4,4,4/
      DATA FAC5 /2,1,5,0,
     .           3,2,5,0,
     .           3,5,4,0,
     .           1,4,5,0,
     .           1,4,3,2/
      DATA NOD5 /3,3,3,3,4/
      DATA NFACE/6,4,5,5/
C
      NELA=0
      DO I=1,NEL
         IF (TAGELS(I).EQ.0) THEN
            NELA=NELA+1
            TAGELA(NELA)=I
            ELEMA(1,NELA)=ELEM(1,I)
            ELEMA(2,NELA)=ELEM(2,I)
            ELEMA(3,NELA)=ELEM(3,I)
         ENDIF
      ENDDO 
      DO I=1,NNA
         II=IBUFA(I)
         ITABINV(II)=I
      ENDDO
C
      DO I=1,NBRIC
         II=TBRIC(1,I)
         NTYPE=TBRIC(2,I)
         NFAC=NFACE(NTYPE)
         DO J=1,NFAC
            IF (TFAC(2*(J-1)+1,I).EQ.3) THEN
C La facette de solide est libre a l'interieur de l'airbag
               IF (NTYPE==2) THEN
                  NELA=NELA+1
                  KK=FAC4(1,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(1,NELA)=KK
                  KK=FAC4(2,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(3,NELA)=KK
                  KK=FAC4(3,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(2,NELA)=KK
                  TAGELA(NELA)=-I
               ELSEIF (NTYPE==3) THEN
                  NELA=NELA+1
                  KK=FAC6(1,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(1,NELA)=KK
                  KK=FAC6(2,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(3,NELA)=KK
                  KK=FAC6(3,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(2,NELA)=KK
                  TAGELA(NELA)=-I
C
                  IF(NOD6(J).EQ.4) THEN
                     NELA=NELA+1
                     KK=FAC6(1,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(1,NELA)=KK
                     KK=FAC6(3,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(3,NELA)=KK
                     KK=FAC6(4,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(2,NELA)=KK
                     TAGELA(NELA)=-I
                  ENDIF
               ELSEIF (NTYPE==4) THEN
                  NELA=NELA+1
                  KK=FAC5(1,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(1,NELA)=KK
                  KK=FAC5(2,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(3,NELA)=KK
                  KK=FAC5(3,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(2,NELA)=KK
                  TAGELA(NELA)=-I
C
                  IF(NOD5(J).EQ.4) THEN
                     NELA=NELA+1
                     KK=FAC5(1,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(1,NELA)=KK
                     KK=FAC5(3,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(3,NELA)=KK
                     KK=FAC5(4,J)
                     KK=ITABINV(IXS(1+KK,II))
                     ELEMA(2,NELA)=KK
                     TAGELA(NELA)=-I
                  ENDIF
               ELSEIF (NTYPE==1) THEN
                  NELA=NELA+1
                  KK=FAC8(1,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(1,NELA)=KK
                  KK=FAC8(2,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(3,NELA)=KK
                  KK=FAC8(3,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(2,NELA)=KK
                  TAGELA(NELA)=-I
C
                  NELA=NELA+1
                  KK=FAC8(1,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(1,NELA)=KK
                  KK=FAC8(3,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(3,NELA)=KK
                  KK=FAC8(4,J)
                  KK=ITABINV(IXS(1+KK,II))
                  ELEMA(2,NELA)=KK
                  TAGELA(NELA)=-I
               ENDIF
            ENDIF
         ENDDO
C
         DO J=1,8
            JJ=IXS(1+J,II)
            BRNA(J,I)=ITABINV(JJ)
         ENDDO
      ENDDO
C
      RETURN
      END

