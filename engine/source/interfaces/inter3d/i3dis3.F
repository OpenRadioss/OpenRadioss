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
Chd|  I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|-- called by -----------
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|        UD_INT5                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I3DIS3(IGIMP ,NTY     ,DIST  ,X1    ,X2 ,
     1                  X3    ,X4      ,XI    ,Y1    ,Y2 ,
     2                  Y3    ,Y4      ,YI    ,Z1    ,Z2 ,
     3                  Z3    ,Z4      ,ZI    ,XFACE ,N1 ,
     4                  N2    ,N3      ,SSC   ,TTC   ,ALP,
     5                  ANS   ,XP      ,YP    ,ZP    ,H1 ,
     6                  H2    ,H3      ,H4)
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
      INTEGER IGIMP,NTY
      my_real DIST(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  XFACE,ANS
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3,ALP
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XP,YP,ZP,SSC,TTC
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: H1,H2,H3,H4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (XP,BV(1,16)),(YP,BV(1,17)),(ZP,BV(1,18))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (ANS,BV(1,22)),(SSC,BV(1,23)),(TTC,BV(1,24))
!      EQUIVALENCE (H1,BV(1,25)) ,(H2,BV(1,26)) ,(H3,BV(1,27))
!      EQUIVALENCE (H4,BV(1,28)) ,(XFACE,BV(1,29))
!      EQUIVALENCE (ALP,BV(1,72))
C=======================================================================
      DO I=LFT,LLT
        IF (XFACE(I) == ZERO) CYCLE
        IF (ABS(SSC(I))>UN+ALP(I) .OR. ABS(TTC(I))>UN+ALP(I)) THEN
          XFACE(I)=ZERO
        ELSE
          IF(ABS(SSC(I)) > UN) SSC(I)=SSC(I)/ABS(SSC(I))
          IF(ABS(TTC(I)) > UN) TTC(I)=TTC(I)/ABS(TTC(I))
        ENDIF
      END DO
C
      IGIMP=0
      DO I=LFT,LLT
        IGIMP = IGIMP + ABS(XFACE(I))
      END DO
      IF (IGIMP == 0) RETURN
C
      DO I=LFT,LLT
        H1(I) = QUART*(UN-TTC(I))*(UN-SSC(I))
        H2(I) = QUART*(UN-TTC(I))*(UN+SSC(I))
        H3(I) = QUART*(UN+TTC(I))*(UN+SSC(I))
        H4(I) = QUART*(UN+TTC(I))*(UN-SSC(I))
      END DO
C
      DO I=LFT,LLT
        XP(I)=H1(I)*X1(I)+H2(I)*X2(I)+H3(I)*X3(I)+H4(I)*X4(I)
        YP(I)=H1(I)*Y1(I)+H2(I)*Y2(I)+H3(I)*Y3(I)+H4(I)*Y4(I)
        ZP(I)=H1(I)*Z1(I)+H2(I)*Z2(I)+H3(I)*Z3(I)+H4(I)*Z4(I)
      END DO
C
      DO I=LFT,LLT
        ANS(I)= N1(I)*(XI(I)-XP(I))
     .         +N2(I)*(YI(I)-YP(I))
     .         +N3(I)*(ZI(I)-ZP(I))
      END DO
      IF (NTY == 8) THEN
        DO I=LFT,LLT
          DIST(I) = ANS(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
