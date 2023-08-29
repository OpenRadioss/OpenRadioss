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
Chd|  VISC_ET                       source/elements/solid/solidez/visc_et.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VISC_ET(NEL ,VISC ,RHO ,NU ,CXX ,ET )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL
      my_real ,INTENT(IN)  :: RHO,NU
      my_real ,INTENT(IN)  :: CXX(NEL)
      my_real ,INTENT(OUT) :: ET(NEL)
      TYPE(VISC_PARAM_) ,INTENT(IN) :: VISC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NPRONY
      my_real :: GG,GV,GV_MAX,NU1
C=======================================================================        
      NPRONY = VISC%IPARAM(1)
      ET(:)  = ZERO
      GV  = ZERO
      DO I= 1,NPRONY                                  
       GV = GV + VISC%UPARAM(1 + I)
      ENDDO
C
      GV_MAX = THIRD*(ONE+NU)/(ONE -TWO*NU)
      NU1    = RHO * (ONE -TWO*NU)/(ONE-NU)
C
      DO I=1,NEL
        GG   = CXX(I)*CXX(I)*NU1    
        ET(I)= ET(I) + MIN(GV_MAX,GV/GG)
      ENDDO
C------------
      RETURN
      END
