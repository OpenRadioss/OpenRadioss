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
Chd|  TABLE2D_INTERSECT             source/tools/curve/table2d_intersect.F
Chd|-- called by -----------
Chd|        LAW76_UPD                     source/materials/mat/mat076/law76_upd.F
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE2D_INTERSECT(TABLE  ,I1    ,I2    ,NPT    ,
     .                             XFAC   ,XINT  ,YINT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER      ,INTENT(IN)  :: I1,I2,NPT
      my_real      ,INTENT(IN)  :: XFAC
      TYPE(TTABLE) ,INTENT(IN)  :: TABLE
      my_real      ,INTENT(OUT) :: XINT,YINT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J1,J2,K
      my_real :: S1,S2,T1,T2,X1,X2,Y1,Y2,AX,AY,BX,BY,CX,CY,DM,ALPHA,BETA
c-----------------------------------------------
c     This routine checks if the functions in a 2 dim table do not intersect 
c     with respect to the second independent variable
C=======================================================================
c     Check segment intersections between 2 functions

      XINT  = ZERO
      YINT  = ZERO
      
c     I1 = index of first  strain rate 
c     I2 = index of second strain rate 
c
      J1 = (I1 - 1)*NPT
      J2 = (I2 - 1)*NPT

      DO K = 2,NPT
        S1 = TABLE%X(1)%VALUES(K-1)*XFAC 
        S2 = TABLE%X(1)%VALUES(K)  *XFAC
        X1 = S1
        X2 = S2
        T1 = TABLE%Y%VALUES(J1 + K-1) 
        T2 = TABLE%Y%VALUES(J1 + K) 
        Y1 = TABLE%Y%VALUES(J2 + K-1) 
        Y2 = TABLE%Y%VALUES(J2 + K) 
c
        AX = X2 - X1
        AY = Y2 - Y1
        BX = S1 - S2
        BY = T1 - T2
        DM = AY*BX - AX*BY
        IF (DM /= ZERO) THEN  ! check if segments are not parallel
          CX = S1 - X1
          CY = T1 - Y1
          ALPHA = (BX * CY - BY * CX) / DM
          BETA  = (AX * CY - AY * CX) / DM
          IF (ALPHA >= ZERO .and. ALPHA < ONE .and.
     .        BETA  <= ZERO .and. BETA  >-ONE .and. S1 > ZERO) THEN
            XINT = X1 + ALPHA * AX
            YINT = Y1 + ALPHA * AY
            EXIT
          ENDIF
        ENDIF
      END DO
c-----------
      RETURN
      END
