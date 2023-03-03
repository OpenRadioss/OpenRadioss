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
Chd|  I25COR3                       source/interfaces/inter3d1/i25cor3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25COR3(
     1               JLT     ,IGAP   ,X      ,IRECT ,NSV   ,
     2               CAND_E  ,CAND_N ,XI     ,YI    ,ZI    ,
     4               IX1     ,IX2   ,IX3   ,IX4      ,NSVG      ,
     5               NSN     ,GAP_S  ,GAPS  ,ADMSR ,NOD_NORMAL  ,
     7               X1      ,X2     ,X3    ,X4    ,X0          ,
     8               Y1      ,Y2     ,Y3    ,Y4    ,Y0          ,
     9               Z1      ,Z2     ,Z3    ,Z4    ,Z0          ,
     A               NNX     ,NNY    ,NNZ   ,MVOISIN,MVOISN     ,
     B               GAP_M   ,GAPM   ,GAP_NM,GAPNM ,
     C               GAP_S_L ,GAP_M_L,GAPMXL,LBOUND,IBOUND      )
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
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        JLT, NSN, IGAP
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), ADMSR(4,*),
     .        MVOISIN(4,*), MVOISN(MVSIZ,4), LBOUND(*), IBOUND(4,MVSIZ)
      my_real
     .   X(3,*), GAP_S(*), GAPS(MVSIZ), GAP_M(*), GAPM(*),
     .   GAP_NM(4,*), GAPNM(4,*), GAP_S_L(*), GAP_M_L(*),
     .   GAPMXL(*)
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), 
     .   X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .   X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .   X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .   X0(MVSIZ),Y0(MVSIZ),Z0(MVSIZ),
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5)
      REAL*4 NOD_NORMAL(3,4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , L, NN, IG, JFT, IX,  NI, I1, I2, I3, I4
      my_real
     .        XN
C-----------------------------------------------
      DO I=1,JLT
        NI = CAND_N(I)
C
        IG = NSV(NI)
        NSVG(I) = IG
C
        XI(I) = X(1,IG)
        YI(I) = X(2,IG)
        ZI(I) = X(3,IG)
        GAPS(I)  = GAP_S(NI)
C
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        IX=IRECT(1,L)  
        IX1(I)=IX      
        X1(I)=X(1,IX)  
        Y1(I)=X(2,IX)  
        Z1(I)=X(3,IX)  
C
        IX=IRECT(2,L)  
        IX2(I)=IX      
        X2(I)=X(1,IX)  
        Y2(I)=X(2,IX)  
        Z2(I)=X(3,IX)  
C
        IX=IRECT(3,L)  
        IX3(I)=IX      
        X3(I)=X(1,IX)  
        Y3(I)=X(2,IX)  
        Z3(I)=X(3,IX)  
C
        IX=IRECT(4,L)  
        IX4(I)=IX      
        X4(I)=X(1,IX)  
        Y4(I)=X(2,IX)  
        Z4(I)=X(3,IX)  
C
        GAPM(I)  = GAP_M(L)
        GAPNM(1:4,I)=GAP_NM(1:4,L)
C
      END DO
C
      IF(IGAP/=3)THEN
        GAPMXL(1:JLT)=EP30
      ELSE
        DO I=1,JLT
          NI = CAND_N(I)
          L  = CAND_E(I)
          GAPMXL(I)=GAP_S_L(NI)+GAP_M_L(L)
        END DO
      END IF
C
      DO I=1,JLT
C
        IF(IX3(I) /= IX4(I))THEN
         X0(I)  = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I)  = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I)  = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
        ELSE
         X0(I)  = X3(I)
         Y0(I)  = Y3(I)
         Z0(I)  = Z3(I) 
        ENDIF
C
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        NNX(I,1)=NOD_NORMAL(1,1,L)
        NNY(I,1)=NOD_NORMAL(2,1,L)
        NNZ(I,1)=NOD_NORMAL(3,1,L)
C
        NNX(I,2)=NOD_NORMAL(1,2,L)
        NNY(I,2)=NOD_NORMAL(2,2,L)
        NNZ(I,2)=NOD_NORMAL(3,2,L)
C
        NNX(I,3)=NOD_NORMAL(1,3,L)
        NNY(I,3)=NOD_NORMAL(2,3,L)
        NNZ(I,3)=NOD_NORMAL(3,3,L)
C
        NNX(I,4)=NOD_NORMAL(1,4,L)
        NNY(I,4)=NOD_NORMAL(2,4,L)
        NNZ(I,4)=NOD_NORMAL(3,4,L)
C
      END DO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          NNX(I,5)=FOURTH*(NNX(I,1)+NNX(I,2)+NNX(I,3)+NNX(I,4))
          NNY(I,5)=FOURTH*(NNY(I,1)+NNY(I,2)+NNY(I,3)+NNY(I,4))
          NNZ(I,5)=FOURTH*(NNZ(I,1)+NNZ(I,2)+NNZ(I,3)+NNZ(I,4))
        ELSE
          NNX(I,5)=NNX(I,4)
          NNY(I,5)=NNY(I,4)
          NNZ(I,5)=NNZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NNX(I,5)*NNX(I,5)+NNY(I,5)*NNY(I,5)+NNZ(I,5)*NNZ(I,5)))
        NNX(I,5)=XN*NNX(I,5)
        NNY(I,5)=XN*NNY(I,5)
        NNZ(I,5)=XN*NNZ(I,5)       
      END DO
C
      IBOUND(1:4,1:JLT)=0
      DO I=1,JLT
        L   = CAND_E(I)
        DO J=1,4
          MVOISN(I,J)=MVOISIN(J,L)
          IF(LBOUND(ADMSR(J,L))/=0)IBOUND(J,I)=ADMSR(J,L)
        END DO
      END DO
C
      RETURN
      END
