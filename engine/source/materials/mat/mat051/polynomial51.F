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
!||    polynomial51_mod   ../engine/source/materials/mat/mat051/polynomial51.F
!||--- called by ------------------------------------------------------
!||    sigeps51           ../engine/source/materials/mat/mat051/sigeps51.F90
!||====================================================================
      MODULE POLYNOMIAL51_MOD
      CONTAINS

!||====================================================================
!||    polynomial51   ../engine/source/materials/mat/mat051/polynomial51.F
!||====================================================================
      SUBROUTINE POLYNOMIAL51 (
     .    C0      ,C1     ,C2      ,C3       ,C4     , C5   ,
     .    VOLUME  ,DVOL   ,MU      ,EINTA    ,
     .    PEXT    ,P      ,PM      ,
     .    DPDE    ,DPDMU  ,DF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN) :: C0,C1,C2,C3,C5,C4,VOLUME,PEXT,PM
      my_real,INTENT(IN) :: MU, DVOL, DF
      my_real,INTENT(INOUT) :: EINTA,DPDE,DPDMU
      my_real,INTENT(INOUT) :: P
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: AA,BB,DPDV,MUP1,V0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      V0      = VOLUME/DF
      MUP1    = MU + ONE
      AA      = (C4 + C5*MU)/V0
      BB      = HALF*DVOL
      !--------------------------
      !first torder
      P       = ( C0 + C1*MU+ MAX(MU,ZERO)*(C2*MU + C3*MU*MU) + AA*EINTA )
      P       = MAX(P,PM)
      !second order
      !P       = ( PEXT+C0 + C1*MU+ MAX(MU,ZERO)*(C2*MU + C3*MU*MU) + AA*EINTA )   /  (ONE+AA*BB)
      !P       = MAX(P,PM)-PEXT
      !EINTA = EINTA - BB*(P+PEXT+Q) !2nd order integration (semi-implicit)
      !--------------------------
      DPDMU = (C1+MAX(MU,ZERO)*(TWO*C2+THREE*C3*MU)+C5*EINTA/V0) + (C4+C5*MU)*(P+PEXT)/MUP1/MUP1
      DPDMU   = ABS(DPDMU)
      DPDE    = (C4 + C5*MU)
      RETURN
      END SUBROUTINE POLYNOMIAL51


      END MODULE POLYNOMIAL51_MOD
