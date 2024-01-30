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
Chd|  DAMPVREF_SUM6                 source/assembly/dampvref_sum6.F
Chd|-- called by -----------
Chd|        DAMPING_VREF                  source/assembly/damping_vref.F
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPVREF_SUM6(IGRNOD,V,A,MS,WEIGHT,
     .                         ND,NNOD,IGR,SUML6,NGRNOD,
     .                         NUMNOD,NDAMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: WEIGHT(NUMNOD),ND,NNOD,IGR,NGRNOD,NUMNOD,NDAMP
      my_real, INTENT(IN) :: V(3,NUMNOD),A(3,NUMNOD),MS(NUMNOD)
      DOUBLE PRECISION, INTENT(INOUT) :: SUML6(7,6,NDAMP)
C-----------------------------------------------
      TYPE (GROUP_),DIMENSION(NGRNOD), INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N
      INTEGER INDEX(NNOD),NINDEX
      my_real :: F1(NNOD),F2(NNOD),F3(NNOD),F4(NNOD),F5(NNOD),
     .           F6(NNOD),F7(NNOD)
      DOUBLE PRECISION :: SUML6_L(7,6)
C-----------------------------------------------
C
C======================================================================|
C     Parithon computation of reference velocity for /DAMP/VREF 
C======================================================================|
C
      SUML6_L(1:7,1:6) = ZERO
C      
      NINDEX = 0
      DO N=1,NNOD
        I=IGRNOD(IGR)%ENTITY(N)
        NINDEX = NINDEX+1
        INDEX(NINDEX) = I
        F1(NINDEX) = MS(I)*V(1,I)*WEIGHT(I)
        F2(NINDEX) = MS(I)*V(2,I)*WEIGHT(I)
        F3(NINDEX) = MS(I)*V(3,I)*WEIGHT(I)
        F4(NINDEX) = MS(I)*A(1,I)*WEIGHT(I)
        F5(NINDEX) = MS(I)*A(2,I)*WEIGHT(I)
        F6(NINDEX) = MS(I)*A(3,I)*WEIGHT(I)
        F7(NINDEX) = MS(I)*WEIGHT(I)
      ENDDO
C 
      CALL SUM_6_FLOAT(1, NINDEX, F1, SUML6_L(1,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F2, SUML6_L(2,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F3, SUML6_L(3,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F4, SUML6_L(4,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F5, SUML6_L(5,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F6, SUML6_L(6,1), 7)
      CALL SUM_6_FLOAT(1, NINDEX, F7, SUML6_L(7,1), 7)
C
#include "lockon.inc"
      DO K = 1, 6
        SUML6(1,K,ND) = SUML6(1,K,ND)+SUML6_L(1,K)
        SUML6(2,K,ND) = SUML6(2,K,ND)+SUML6_L(2,K)
        SUML6(3,K,ND) = SUML6(3,K,ND)+SUML6_L(3,K)
        SUML6(4,K,ND) = SUML6(4,K,ND)+SUML6_L(4,K)
        SUML6(5,K,ND) = SUML6(5,K,ND)+SUML6_L(5,K)
        SUML6(6,K,ND) = SUML6(6,K,ND)+SUML6_L(6,K)
        SUML6(7,K,ND) = SUML6(7,K,ND)+SUML6_L(7,K)
      END DO
#include "lockoff.inc"
C
      RETURN
C      
      END