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
C
Chd|====================================================================
Chd|  MINTP_RE                      eos/mintp_re.F                
Chd|-- called by -----------
Chd|        SESAME                        common_source/eos/sesame.F    
Chd|        HM_READ_EOS_SESAME            starter/source/materials/eos/hm_read_eos_sesame.F
Chd|        HM_READ_MAT26                 starter/source/materials/mat/mat026/hm_read_mat26.F
Chd|        SESA20                        engine/source/materials/mat/mat026/sesa20.F
Chd|-- calls ---------------
Chd|        MINTER1D_RAT                  eos/minter1d_rat.F            
Chd|====================================================================
      SUBROUTINE MINTP_RE(XX,NX,YY,NY,ZZ,X,Y,Z,DYDZ)
C=====================================
C
C                           |         |                |          |
C                         --+---------+--------*-------+----------+-- YY(IY+2)
C     INPUT :               |         |       Z3       |          |
C          XX(NX)           |         |                |          |
C          NX               |         |                |          |
C          YY(NY)           |         |                |          |
C          ZZ(NX,NY)        |     Z(IX,IY+1)     Z(IX+1,IY+1)     |
C          X              --+---------+--------*-------+----------+-- YY(IY+1)
C          Z                |         |       Z2       |          |
C                           |         |                |          |
C      OUTPUT:              |         |                |          |
C          Y                |         |      Z(X,Y)    |          |
C                           |         |        * - - - | - - - - -| - - - Y
C                           |         |        .       |          |
C                           |         |        .       |          |
C                           |     Z(IX,IY)     .   Z(IX+1,IY)     |
C                         --+---------+--------*-------+----------+-- YY(IY)
C                           |         |       Z1       |          |
C                           |         |        .       |          |
C                           |         |        .       |          |
C                           |         |        .       |          |
C                           |         |       Z0       |          |
C                         --+---------+--------*-------+----------+-- YY(IY-1)
C                           |         |        .       |          |
C                           |         |        .       |          |
C                       XX(IX-1)   XX(IX)      .   XX(IX+1)   XX(IX+2)
C                                              X
C
C====================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NX, NY
C     REAL ou REAL*8
      my_real
     .   X, Y, Z, DYDZ
C     REAL ou REAL*8
      my_real
     .   XX(NX), YY(NY), ZZ(NX,NY)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX, IY, IXM1, IXP1, IXP2, IYM1, IYP1, IYP2
C     REAL ou REAL*8
      my_real
     .   XX0, XX1, XX2, XX3, ZZ00, ZZ10, ZZ20, ZZ30, ZZ01, ZZ11, ZZ21,
     .   ZZ31, ZZ02, ZZ12, ZZ22, ZZ32, ZZ03, ZZ13, ZZ23, ZZ33, Z0,
     .   DZDX0, Z1, DZDX1, Z2, DZDX2, Z3, DZDX3, DZDX, D2ZDZDX
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER MINDEX
C-----------------------------------------------
C...Translated by Pacific-Sierra Research VAST-2  3.04F10 12:36:18   3/20/92   -
      IX = MINDEX(XX,1,NX,X)
      IY = MINDEX(ZZ(IX,1),NX,NY,Z)
C
      IX = MAX(1,MIN(NX-1,IX))
      IY = MAX(1,MIN(NY-1,IY))
C
      IXM1 = MAX(1,IX-1)
      IXP1 = IX+1
      IXP2 = MIN(NX,IX+2)
C
      IYM1 = MAX(1,IY-1)
      IYP1 = IY+1
      IYP2 = MIN(NY,IY+2)
C
      XX0 = XX(IXM1)
      XX1 = XX(IX)
      XX2 = XX(IXP1)
      XX3 = XX(IXP2)
C
      ZZ00 = ZZ(IXM1,IYM1)
      ZZ10 = ZZ(IX  ,IYM1)
      ZZ20 = ZZ(IXP1,IYM1)
      ZZ30 = ZZ(IXP2,IYM1)
C
      ZZ01 = ZZ(IXM1,IY)
      ZZ11 = ZZ(IX  ,IY)
      ZZ21 = ZZ(IXP1,IY)
      ZZ31 = ZZ(IXP2,IY)
C
      ZZ02 = ZZ(IXM1,IYP1)
      ZZ12 = ZZ(IX  ,IYP1)
      ZZ22 = ZZ(IXP1,IYP1)
      ZZ32 = ZZ(IXP2,IYP1)
C
      ZZ03 = ZZ(IXM1,IYP2)
      ZZ13 = ZZ(IX  ,IYP2)
      ZZ23 = ZZ(IXP1,IYP2)
      ZZ33 = ZZ(IXP2,IYP2)
C
      CALL MINTER1D_RAT(
     +  XX0 ,XX1 ,XX2 ,XX3 ,ZZ00 ,ZZ10 ,ZZ20 ,ZZ30 ,X ,Z0 ,DZDX0,IX,NX)
      CALL MINTER1D_RAT(
     +  XX0 ,XX1 ,XX2 ,XX3 ,ZZ01 ,ZZ11 ,ZZ21 ,ZZ31 ,X ,Z1 ,DZDX1,IX,NX)
      CALL MINTER1D_RAT(
     +  XX0 ,XX1 ,XX2 ,XX3 ,ZZ02 ,ZZ12 ,ZZ22 ,ZZ32 ,X ,Z2 ,DZDX2,IX,NX)
      CALL MINTER1D_RAT(
     +  XX0 ,XX1 ,XX2 ,XX3 ,ZZ03 ,ZZ13 ,ZZ23 ,ZZ33 ,X ,Z3 ,DZDX3,IX,NX)
C
      CALL MINTER1D_RAT(
     +         Z0       ,Z1     ,Z2       ,Z3      ,
     +         YY(IYM1) ,YY(IY) ,YY(IYP1) ,YY(IYP2),
     +         Z        ,Y      ,DYDZ     ,IY      ,NY)
      CALL MINTER1D_RAT(
     +         Z0       ,Z1     ,Z2       ,Z3      ,
     +         DZDX0    ,DZDX1  ,DZDX2    ,DZDX3   ,
     +         Z        ,DZDX   ,D2ZDZDX  ,IY      ,NY)
C
      RETURN
      END
