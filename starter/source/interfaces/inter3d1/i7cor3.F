Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    i7cor3      ../starter/source/interfaces/inter3d1/i7cor3.F
      !||--- called by ------------------------------------------------------
      !||    i20ini3     ../starter/source/interfaces/inter3d1/i20ini3.F
      !||    i24buc1     ../starter/source/interfaces/inter3d1/i24buc1.F
      !||    i24tri      ../starter/source/interfaces/inter3d1/i24tri.F
      !||    i7buc1      ../starter/source/interfaces/inter3d1/i7buc1.F
      !||    i7tri       ../starter/source/interfaces/inter3d1/i7tri.F
      !||    i7trivox1   ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||    inint3      ../starter/source/interfaces/inter3d1/inint3.F
      !||====================================================================
      SUBROUTINE I7COR3(X     ,IRECT ,NSV   ,CAND_E  ,CAND_N,
     2                  STF   ,STFN  ,GAPV  ,IGAP    ,GAP   ,
     3                  GAP_S ,GAP_M ,ISTF  ,GAPMIN  ,GAPMAX,
     4                  GAP_S_L,GAP_M_L,DRAD,IX1     ,IX2   ,
     5                  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6                  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                  XI     ,YI     ,ZI  ,STIF    ,DGAPLOAD,
     9                  LAST)
C============================================================================
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
      INTEGER IGAP, IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),ISTF
      INTEGER, INTENT(IN) :: LAST !< last candidat
C     REAL
      my_real
     .        GAP, GAPMIN, GAPMAX,DRAD
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .        X(3,*), STF(*), STFN(*), GAPV(*), GAP_S(*), GAP_M(*),
     .        GAP_S_L(*), GAP_M_L(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, NN, IG
C     REAL
C-----------------------------------------------
C
      DO I=1,LAST
       IG = NSV(CAND_N(I))
       NSVG(I) = IG
       XI(I) = X(1,IG)
       YI(I) = X(2,IG)
       ZI(I) = X(3,IG)
      ENDDO
C
      IF(IGAP==0)THEN
        DO I=1,LAST
         GAPV(I) = MAX(GAP+DGAPLOAD,DRAD)         
        ENDDO
      ELSE
        DO I=1,LAST
         GAPV(I) = GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
         IF(IGAP == 3)  
     .      GAPV(I)=MIN(GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)),GAPV(I))
         GAPV(I) = MIN(GAPMAX,GAPV(I))
         GAPV(I) = MAX(GAPMIN,GAPV(I))
C
         GAPV(I) = MAX(DRAD,GAPV(I)+DGAPLOAD)
        ENDDO
      ENDIF
C
      IF(ISTF/=0)THEN
        DO I=1,LAST
          STIF(I)=STF(CAND_E(I))*STFN(CAND_N(I))
        ENDDO
      ENDIF
C
      DO I=1,LAST
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
      DO I=1,LAST
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
