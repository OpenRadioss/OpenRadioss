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
Chd|  ELAS24                        source/materials/mat/mat024/elas24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|        RDAM24                        source/materials/mat/mat024/rdam24.F
Chd|====================================================================
      SUBROUTINE ELAS24(NEL    ,PM     ,
     .                  CDAM   ,SIGC   ,DAM    ,ANG    ,EPS_F  ,CRAK   ,
     .                  C44    ,C55    ,C66    ,DE1    ,DE2    ,DE3    ,
     .                  DEPS1  ,DEPS2  ,DEPS3  ,DEPS4  ,DEPS5  ,DEPS6  ,
     .                  S01    ,S02    ,S03    ,S04    ,S05    ,S06    ,
     .                  SCAL1  ,SCAL2  ,SCAL3  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real PM(NPROPM)
      my_real, DIMENSION(NEL) ,INTENT(IN)  :: DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6
      my_real, DIMENSION(NEL) ,INTENT(OUT) :: S01,S02,S03,S04,S05,S06,
     .                                        SCAL1,SCAL2,SCAL3,C44,C55,C66
      my_real, DIMENSION(NEL) ,INTENT(OUT) :: DE1,DE2,DE3
      my_real, DIMENSION(NEL,3)   ,INTENT(IN)   :: EPS_F
      my_real, DIMENSION(NEL,6)   ,INTENT(IN)   :: ANG
      my_real, DIMENSION(NEL,6)   ,INTENT(INOUT):: SIGC
      my_real, DIMENSION(NEL,3)   ,INTENT(INOUT):: DAM,CRAK
      my_real, DIMENSION(NEL,3,3) ,INTENT(INOUT):: CDAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NBDAMA,NBDAMA0
      INTEGER, DIMENSION(NEL) ::  DAMAI,DAMA0I
      my_real, DIMENSION(NEL) ::  DE4,DE5,DE6
      my_real  YOUNG,NU,G,A11,A12,DSUP,QQ,DEN,DEPSF,HH1,HH2,HH3
C=======================================================================
c     DEi = (1 - DAMi)       coefficients d endommagement de la matrice Hook
c-----------------------------------------------------------------------
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      A11   = PM(24)
      A12   = PM(25)
      DSUP  = PM(26)
      QQ    = PM(28)
c
c     test of damaged elements
c
      NBDAMA  = 0
      NBDAMA0 = 0
      DO I = 1,NEL
         IF (DAM(I,1) + DAM(I,2) + DAM(I,3) > ZERO) THEN
           NBDAMA = NBDAMA + 1
           DAMAI(NBDAMA)=I
         ELSE
           NBDAMA0 = NBDAMA0 + 1 
           DAMA0I(NBDAMA0)=I
         ENDIF
      ENDDO
c
c     ROTATION de DEPS_IJ DANS LES DIRECTIONS D'ENDOMMAGEMENT
c
      IF (NBDAMA > 0) THEN
        CALL RDAM24(NEL   ,NBDAMA,DAMAI ,ANG   ,DEPS1 ,
     .              DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 )
      ENDIF
c-----------------------------------------------------------
c     DAMAGED ELEMENT GROUP => modified Hook matrix
c-----------------------------------------------------------
      DO K=1,NBDAMA
        I = DAMAI(K)
c       cumul de deformation endommagee
        CRAK(I,1) = CRAK(I,1) + DEPS1(I)
        CRAK(I,2) = CRAK(I,2) + DEPS2(I)
        CRAK(I,3) = CRAK(I,3) + DEPS3(I)
c
c       Damage evolution depending on EPS_F_i/EPS_i
c
        HH1  = MAX(CRAK(I,1),EM20)
        IF (EPS_F(I,1).GT.ZERO) THEN
          DE1(I) = QQ * (ONE- EPS_F(I,1)/HH1 )
        ELSE
          DE1(I) = ZERO
        ENDIF
        HH2  = MAX(CRAK(I,2),EM20)
        IF (EPS_F(I,2).GT.ZERO) THEN
          DE2(I) = QQ * (ONE- EPS_F(I,2)/HH2 )
        ELSE
          DE2(I) = ZERO
        ENDIF
        HH3  = MAX(CRAK(I,3),EM20)
        IF (EPS_F(I,3).GT.ZERO) THEN
          DE3(I) = QQ * (ONE - EPS_F(I,3)/HH3 )
        ELSE
          DE3(I) = ZERO
        ENDIF
c
c       DAM(I,J) SAUVEGARDE DE L'ENDOMMAGEMENT VRAI
c
        DE1(I)   = MIN(DE1(I),DSUP)
        DE2(I)   = MIN(DE2(I),DSUP)
        DE3(I)   = MIN(DE3(I),DSUP)
        DAM(I,1) = MAX(DE1(I),DAM(I,1))
        DAM(I,2) = MAX(DE2(I),DAM(I,2))
        DAM(I,3) = MAX(DE3(I),DAM(I,3))
c
        DE1(I)  = ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
        DE2(I)  = ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
        DE3(I)  = ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
        SCAL1(I)= HALF + SIGN(HALF,DE1(I)-ONE)
        SCAL2(I)= HALF + SIGN(HALF,DE2(I)-ONE)
        SCAL3(I)= HALF + SIGN(HALF,DE3(I)-ONE)    
        DE4(I)  = SCAL1(I)*SCAL2(I)
        DE5(I)  = SCAL2(I)*SCAL3(I)
        DE6(I)  = SCAL3(I)*SCAL1(I)
c
c       NOUVELLE MATRICE ELASTIQUE DE HOOKE ENDOMMAGEE
c
        DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .      + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
C
        CDAM(I,1,1) = YOUNG*DE1(I)*(ONE - NU**2*DE5(I))/DEN
        CDAM(I,2,2) = YOUNG*DE2(I)*(ONE - NU**2*DE6(I))/DEN
        CDAM(I,3,3) = YOUNG*DE3(I)*(ONE - NU**2*DE4(I))/DEN
        CDAM(I,1,2) = NU*YOUNG*DE4(I) *(ONE + NU*SCAL3(I))/DEN
        CDAM(I,1,3) = NU*YOUNG*DE6(I) *(ONE + NU*SCAL2(I))/DEN
        CDAM(I,2,3) = NU*YOUNG*DE5(I) *(ONE + NU*SCAL1(I))/DEN
        CDAM(I,2,1) = CDAM(I,1,2)
        CDAM(I,3,1) = CDAM(I,1,3)
        CDAM(I,3,2) = CDAM(I,2,3)
c
        C44(I) = G*DE4(I)
        C55(I) = G*DE5(I)
        C66(I) = G*DE6(I)
c
c       CONTRAINTES ELASTIQUES
c
        S01(I) = CDAM(I,1,1)*CRAK(I,1) + CDAM(I,1,2)*CRAK(I,2)
     .         + CDAM(I,1,3)*CRAK(I,3)
        S02(I) = CDAM(I,2,1)*CRAK(I,1) + CDAM(I,2,2)*CRAK(I,2)
     .         + CDAM(I,2,3)*CRAK(I,3)
        S03(I) = CDAM(I,3,1)*CRAK(I,1) + CDAM(I,3,2)*CRAK(I,2)
     .         + CDAM(I,3,3)*CRAK(I,3)
c       formulation incrementale en cisaillement 
        S04(I) = DE4(I)*SIGC(I,4) + C44(I)*DEPS4(I)
        S05(I) = DE5(I)*SIGC(I,5) + C55(I)*DEPS5(I)
        S06(I) = DE6(I)*SIGC(I,6) + C66(I)*DEPS6(I)
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
        C44(I) = G
        C55(I) = G
        C66(I) = G
      ENDDO
c--------------------
      DO  K=1,NBDAMA0
        I = DAMA0I(K)
        S01(I) = SIGC(I,1) + CDAM(I,1,1)*DEPS1(I)+CDAM(I,1,2)*(DEPS2(I)+DEPS3(I))
        S02(I) = SIGC(I,2) + CDAM(I,1,1)*DEPS2(I)+CDAM(I,1,2)*(DEPS1(I)+DEPS3(I))
        S03(I) = SIGC(I,3) + CDAM(I,1,1)*DEPS3(I)+CDAM(I,1,2)*(DEPS1(I)+DEPS2(I))
        S04(I) = SIGC(I,4) + G*DEPS4(I)
        S05(I) = SIGC(I,5) + G*DEPS5(I)
        S06(I) = SIGC(I,6) + G*DEPS6(I)
        CRAK(I,1) = S01(I) - NU *( S02(I) + S03(I) )
        CRAK(I,2) = S02(I) - NU *( S01(I) + S03(I) )
        CRAK(I,3) = S03(I) - NU *( S01(I) + S02(I) )
        CRAK(I,1) = CRAK(I,1)/YOUNG
        CRAK(I,2) = CRAK(I,2)/YOUNG
        CRAK(I,3) = CRAK(I,3)/YOUNG
        SCAL1(I)  = ONE
        SCAL2(I)  = ONE
        SCAL3(I)  = ONE
        DE4(I) = ONE   
        DE5(I) = ONE   
        DE6(I) = ONE   
        DE1(I) = ONE   
        DE2(I) = ONE   
        DE3(I) = ONE   
      ENDDO
c-----------
      RETURN
      END
