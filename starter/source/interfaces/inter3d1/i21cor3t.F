Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  I21COR3T                      source/interfaces/inter3d1/i21cor3t.F
Chd|-- called by -----------
Chd|        I21BUC1                       source/interfaces/inter3d1/i21buc3.F
Chd|        I21TRI                        source/interfaces/inter3d1/i21tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21COR3T(X    ,IRECT ,NSV   ,CAND_E  ,CAND_N ,
     2                   GAPV  ,IGAP  ,GAP   ,GAP_S   ,GAPMIN ,
     3                   GAPMAX,XM0   ,DEPTH ,DRAD    ,IX1    ,
     4                   IX2   ,IX3   ,IX4   ,NSVG    ,X1     ,
     5                   X2    ,X3    ,X4    ,Y1      ,Y2     ,
     6                   Y3    ,Y4    ,Z1    ,Z2      ,Z3     ,
     7                   Z4    ,XI    ,YI    ,ZI      ,DGAPLOAD)
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
      INTEGER IGAP, IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*)
C     REAL
      my_real
     .        GAP, GAPMIN, GAPMAX, DEPTH, DRAD,
     .        X(3,*), GAPV(*), GAP_S(*), XM0(3,*)
      my_real , INTENT(IN) :: DGAPLOAD
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: XI,YI,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, NN, IG
C-----------------------------------------------
      IF(IGAP==0)THEN
        DO I=LFT,LLT
C Depth >= gap
          GAPV(I)=MAX(DEPTH+DGAPLOAD,DRAD)
        END DO
      ELSE
C ... on pourrait utiliser le fait Depth >= max(gapv) dans le starter ...
        DO I=LFT,LLT
          GAPV(I)=GAP_S(CAND_N(I))
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
C
          GAPV(I)=MAX(DEPTH,DRAD,GAPV(I)+DGAPLOAD)
        END DO
      END IF
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
C   attention nos locaux (1:NMN)
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
        X1(I)=XM0(1,NN)
        Y1(I)=XM0(2,NN)
        Z1(I)=XM0(3,NN)
C
        NN=IX2(I)
        X2(I)=XM0(1,NN)
        Y2(I)=XM0(2,NN)
        Z2(I)=XM0(3,NN)
C
        NN=IX3(I)
        X3(I)=XM0(1,NN)
        Y3(I)=XM0(2,NN)
        Z3(I)=XM0(3,NN)
C
        NN=IX4(I)
        X4(I)=XM0(1,NN)
        Y4(I)=XM0(2,NN)
        Z4(I)=XM0(3,NN)
C
      ENDDO
C
      RETURN
      END
