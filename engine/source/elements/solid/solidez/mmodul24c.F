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
Chd|  MMODUL24C                     source/elements/solid/solidez/mmodul24c.F
Chd|-- called by -----------
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMODUL24C(NEL    ,PM     ,DAM    ,CRAK   ,
     .                     CDAM   ,G3     ,ANG    ,DAMANG ,NBDAMA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NBDAMA
      my_real PM(NPROPM)
      my_real, DIMENSION(NEL,3)   ,INTENT(IN):: DAM,CRAK
      my_real, DIMENSION(NEL,6)   ,INTENT(IN):: ANG
      my_real, DIMENSION(MVSIZ,3,3) ,INTENT(OUT):: CDAM
      my_real, DIMENSION(MVSIZ,3) ,INTENT(OUT):: G3
      my_real, DIMENSION(MVSIZ,6) ,INTENT(OUT):: DAMANG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NBDAMA0
      INTEGER, DIMENSION(NEL) ::  DAMAI,DAMA0I
      my_real  YOUNG,NU,G,A11,A12,DE1,DE2,DE3,DE4,DE5,DE6,DEN,
     .         SCAL1,SCAL2,SCAL3,D_MAX
C=======================================================================
c     DEi = (1 - DAMi)       coefficients d endommagement de la matrice Hook
c-----------------------------------------------------------------------
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      A11   = PM(24)
      A12   = PM(25)
c
c     test of damaged elements
c
      NBDAMA  = 0
      NBDAMA0 = 0
      D_MAX=ZERO
      DO I = 1,NEL
         IF (DAM(I,1) + DAM(I,2) + DAM(I,3) > ZERO) THEN
           NBDAMA = NBDAMA + 1
           DAMAI(NBDAMA)=I
           D_MAX =MAX(D_MAX,DAM(I,1),DAM(I,2),DAM(I,3))
         ELSE
           NBDAMA0 = NBDAMA0 + 1 
           DAMA0I(NBDAMA0)=I
         ENDIF
      ENDDO
c-----------------------------------------------------------
c     DAMAGED ELEMENT GROUP => modified Hook matrix
c-----------------------------------------------------------
      DO K=1,NBDAMA
        I = DAMAI(K)
c
        DE1  = ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
        DE2  = ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
        DE3  = ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
        SCAL1= HALF + SIGN(HALF,DE1-ONE)
        SCAL2= HALF + SIGN(HALF,DE2-ONE)
        SCAL3= HALF + SIGN(HALF,DE3-ONE)    
        DE4  = SCAL1*SCAL2
        DE5  = SCAL2*SCAL3
        DE6  = SCAL3*SCAL1
c
c       NOUVELLE MATRICE ELASTIQUE DE HOOKE ENDOMMAGEE
c
        DEN = ONE - NU**2 *(DE4 + DE5 + DE6
     .      + TWO*NU*SCAL1*SCAL2*SCAL3)
C
        CDAM(I,1,1) = YOUNG*DE1*(ONE - NU**2*DE5)/DEN
        CDAM(I,2,2) = YOUNG*DE2*(ONE - NU**2*DE6)/DEN
        CDAM(I,3,3) = YOUNG*DE3*(ONE - NU**2*DE4)/DEN
        CDAM(I,1,2) = NU*YOUNG*DE4 *(ONE + NU*SCAL3)/DEN
        CDAM(I,1,3) = NU*YOUNG*DE6 *(ONE + NU*SCAL2)/DEN
        CDAM(I,2,3) = NU*YOUNG*DE5 *(ONE + NU*SCAL1)/DEN
        CDAM(I,2,1) = CDAM(I,1,2)
        CDAM(I,3,1) = CDAM(I,1,3)
        CDAM(I,3,2) = CDAM(I,2,3)
c
        G3(I,1) = G*DE4
        G3(I,2) = G*DE5
        G3(I,3) = G*DE6
        DAMANG(I,1:6) = ANG(I,1:6)
      ENDDO
c------------------------------------------------------------------------
c     NON DAMAGED ELEMENT GROUP => initial Hook matrix
c------------------------------------------------------------------------
      DO K=1,NBDAMA0
        I = DAMA0I(K)
        CDAM(I,1,1) = A11
        CDAM(I,2,2) = A11
        CDAM(I,3,3) = A11
        CDAM(I,1,2) = A12
        CDAM(I,2,1) = A12
        CDAM(I,1,3) = A12
        CDAM(I,3,1) = A12
        CDAM(I,2,3) = A12
        CDAM(I,3,2) = A12
        G3(I,1:3) = G
        DAMANG(I,1) = ONE
        DAMANG(I,2) = ZERO
        DAMANG(I,3) = ZERO
        DAMANG(I,4) = ZERO
        DAMANG(I,5) = ONE
        DAMANG(I,6) = ZERO
      ENDDO
c-----------
      RETURN
      END
