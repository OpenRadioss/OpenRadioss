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
Chd|  I23COR3T                      source/interfaces/inter3d1/i23cor3t.F
Chd|-- called by -----------
Chd|        I23BUC1                       source/interfaces/inter3d1/i23buc3.F
Chd|        I23TRI                        source/interfaces/inter3d1/i23tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I23COR3T(X    ,IRECT ,NSV   ,CAND_E  ,CAND_N ,
     2                   GAPV  ,IGAP  ,GAP   ,GAP_S   ,GAPMIN ,
     3                   GAPMAX,MSR   ,GAP_M ,IX1     ,IX2    ,
     4                   IX3   ,IX4   ,NSVG  ,X1      ,X2     ,
     5                   X3    ,X4    ,Y1    ,Y2      ,Y3     ,
     6                   Y4    ,Z1    ,Z2    ,Z3      ,Z4     ,
     7                   XI    ,YI    ,ZI    )
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
      INTEGER IGAP, IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), MSR(*)
C     REAL ou REAL*8
      my_real
     .        GAP, GAPMIN, GAPMAX, 
     .        X(3,*), GAPV(*), GAP_S(*), GAP_M(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, NN, IG
C     REAL ou REAL*8
C-----------------------------------------------
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4)) ,(NSVG,IV(1,5))
C
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
C-----------------------------------------------
      IF(IGAP.EQ.0)THEN
        DO I=LFT,LLT
          GAPV(I)=GAP
        END DO
      ELSE
        DO I=LFT,LLT
          GAPV(I)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
        END DO
      END IF
C
      DO I=LFT,LLT
        GAPV(I)=SQRT(TROIS)*GAPV(I)
      END DO
C
      DO I=LFT,LLT
       IG = NSV(CAND_N(I))
       NSVG(I) = IG
       XI(I) = X(1,IG)
       YI(I) = X(2,IG)
       ZI(I) = X(3,IG)
      ENDDO
C
      DO I=LFT,LLT
C
        L  = CAND_E(I)
C
        IX1(I)=IRECT(1,L)
        IX2(I)=IRECT(2,L)
        IX3(I)=IRECT(3,L)
        IX4(I)=IRECT(4,L)
C
      ENDDO
C
      DO I=LFT,LLT
C
        NN=IX1(I)
        X1(I)=X(1,NN)
        Y1(I)=X(2,NN)
        Z1(I)=X(3,NN)
C
        NN=IX2(I)
        X2(I)=X(1,NN)
        Y2(I)=X(2,NN)
        Z2(I)=X(3,NN)
C
        NN=IX3(I)
        X3(I)=X(1,NN)
        Y3(I)=X(2,NN)
        Z3(I)=X(3,NN)
C
        NN=IX4(I)
        X4(I)=X(1,NN)
        Y4(I)=X(2,NN)
        Z4(I)=X(3,NN)
C
      ENDDO
C
      RETURN
      END
