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
Chd|  I21STO                        source/interfaces/int21/i21sto.F
Chd|-- called by -----------
Chd|        I21TRI                        source/interfaces/intsort/i21tri.F
Chd|-- calls ---------------
Chd|        I21COR3T                      source/interfaces/int21/i21cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|====================================================================
      SUBROUTINE I21STO(
     1      J_STOK   ,IRECT  ,XLOC   ,II_STOK,CAND_N   ,
     2      CAND_E ,MULNSN ,NOINT      ,MARGE  ,I_MEM  ,
     3      PROV_N ,PROV_E ,ESHIFT     ,INACTI ,NSN    ,
     4      IGAP     ,GAP  ,GAP_S      ,GAPMIN ,GAPMAX ,
     5      CURV_MAX ,XM0  ,NOD_NORMAL ,DEPTH  ,DRAD   ,
     6      DGAPLOAD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN,IGAP
      INTEGER J_STOK,MULNSN,NOINT,INACTI,ESHIFT,II_STOK
      INTEGER IRECT(4,*),CAND_N(*),CAND_E(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ)
C     REAL
      my_real
     .        XLOC(3,*), GAP_S(*), 
     .        MARGE, GAP, GAPMIN, GAPMAX, CURV_MAX(*),
     .        DEPTH, XM0(3,*), NOD_NORMAL(3,*)
      my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .   NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .   NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .   NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .   PENE(MVSIZ), GAPV(MVSIZ)
C-----------------------------------------------
        CALL I21COR3T(J_STOK ,XLOC ,IRECT ,PROV_E ,
     1               PROV_N  ,IGAP ,GAP   ,X1    ,X2     ,
     2               X3      ,X4   ,Y1    ,Y2   ,Y3   ,
     3               Y4      ,Z1   ,Z2    ,Z3   ,Z4   ,
     4               XI      ,YI   ,ZI    ,IX1   ,IX2    ,  
     5               IX3     ,IX4  ,NSN   ,GAP_S ,GAPV   ,
     6               GAPMAX  ,GAPMIN,CURV_MAX,DEPTH ,XM0 ,
     7               DRAD    ,DGAPLOAD)
C-----------------------------------------------
        CALL I7PEN3(J_STOK ,MARGE  ,X1    ,X2     ,X3   ,
     .               X4    ,Y1     ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2     ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI     ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4    ,IGAP  ,GAP    ,GAPV )
C-----------------------------------------------
        K_STOK = 0
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO) K_STOK = K_STOK + 1
        ENDDO
        IF(K_STOK==0)RETURN
C
#include "lockon.inc"
          I_STOK = II_STOK 
          IF(I_STOK+K_STOK>MULNSN) THEN
            I_MEM = 2
#include "lockoff.inc"
            RETURN
          ENDIF
          II_STOK   = I_STOK + K_STOK
#include "lockoff.inc"
C-----------------------------------------------
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
      END
