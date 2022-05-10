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
Chd|  MULTI_BILAN                   source/multifluid/multi_bilan.F
Chd|-- called by -----------
Chd|        MULTI_GLOBALIZE               source/multifluid/multi_globalize.F
Chd|        MULTI_UPDATE_GLOBAL           source/multifluid/multi_update_global.F
Chd|-- calls ---------------
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_BILAN(PARTSAV, VOL, MULTI_FVM,
     .     NEL, NFT, IPARTS, GRESAV, IGRTH, GRTH)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: PARTSAV(NPSAV, *), GRESAV(*)
      my_real, INTENT(IN) :: VOL(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      INTEGER, INTENT(IN) :: NEL, NFT, IPARTS(*), IGRTH(*), GRTH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, M
      my_real :: EI_SAV(NEL), EK_SAV(NEL), 
     .     XM_SAV(NEL), YM_SAV(NEL), ZM_SAV(NEL), RBIDON(1), VA2(NEL),
     .     MAS(NEL), OFF(NEL), VX(NEL), VY(NEL), VZ(NEL)
      
      RBIDON(1) = ZERO
      OFF(1:NEL) = UN
      DO I = 1, NEL
         MAS(I) = VOL(I) * MULTI_FVM%RHO(I + NFT)
         VX(I) = MULTI_FVM%VEL(1, I + NFT)
         VY(I) = MULTI_FVM%VEL(2, I + NFT)
         VZ(I) = MULTI_FVM%VEL(3, I + NFT)
         VA2(I) = VX(I)**2 + VY(I)**2 + VZ(I)**2
      ENDDO
      M = IPARTS(1)
      DO I=1,NEL
         PARTSAV(1, M) = PARTSAV(1, M) + MULTI_FVM%EINT(I + NFT) * VOL(I)
         PARTSAV(2, M) = PARTSAV(2, M) + UNDEMI * MAS(I) * VA2(I)
         PARTSAV(6, M) = PARTSAV(6, M) + MAS(I)
         PARTSAV(3, M) = PARTSAV(3, M) + MAS(I) * VX(I)
         PARTSAV(4, M) = PARTSAV(4, M) + MAS(I) * VY(I)
         PARTSAV(5, M) = PARTSAV(5, M) + MAS(I) * VZ(I)
      ENDDO

      END SUBROUTINE MULTI_BILAN
