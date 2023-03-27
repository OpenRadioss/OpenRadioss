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
C-----------------------------------------------
      MODULE ALE_MOD
      
        IMPLICIT NONE
#include      "my_real.inc"

        TYPE ALE_GRID_
          my_real ALPHA
          my_real GAMMA
          my_real VGX
          my_real VGY
          my_real VGZ
          my_real VGY0
          my_real VGZ0          
          INTEGER NWALE_ENGINE       !        read from Engine input file with /ALE/GRID/...
          INTEGER NWALE_RST          !        read from RESTART FILE
          INTEGER NWALE              !        effective value used in numerical scheme
        END TYPE ALE_GRID_
        
        TYPE ALE_UPWIND_
          my_real UPWMG
          my_real UPWOG           
          my_real UPWSM
          my_real CUPWM
          my_real UPWMG2
          my_real UPWOG2
          my_real UPWSM2
          INTEGER I_SUPG_ON_OFF
          INTEGER UPWM
          INTEGER UPW_UPDATE
        END TYPE ALE_UPWIND_
                
        TYPE ALE_GLOBAL_
          INTEGER NALENOVS
          INTEGER ICAA
          INTEGER INCOMP
          INTEGER ISFINT
          INTEGER I_DT_NODA_ALE_ON
          INTEGER I_DT_NODA_ALE_ON_KEY
          INTEGER IDT_ALE
          LOGICAL IS_BOUNDARY_MATERIAL
          INTEGER LCONV
          INTEGER CODV(10)
          INTEGER NVCONV
          INTEGER SNALE
          INTEGER SIELVS
        END TYPE ALE_GLOBAL_
        
        TYPE ALE_SUB_
          INTEGER IALESUB
          INTEGER IFSUB
          INTEGER IFSUBM
          INTEGER NODSUBDT
          my_real DT1SAVE
          my_real DTFSUB
          my_real DTMSUB
        END TYPE ALE_SUB_
                
        TYPE ALE_
          TYPE(ALE_GRID_)   :: GRID
          TYPE(ALE_UPWIND_) :: UPWIND
          TYPE(ALE_GLOBAL_) :: GLOBAL
          TYPE(ALE_SUB_)    :: SUB
          contains
            procedure, pass :: init         
        END TYPE ALE_
        
C-----------------------------------------------        
        TYPE(ALE_) :: ALE
C-----------------------------------------------                
        
        CONTAINS        
          subroutine init(this)
            !initialization is done : - during Starter before reading Starter input file
            !                         - during Engine before reading Restart file and Engine input file.
            implicit none
#include "constant.inc"            
            class(ALE_), intent(inout) :: this
            !---ALE%GRID
            this%GRID%ALPHA = ZERO
            this%GRID%GAMMA = ZERO
            this%GRID%VGX = ZERO
            this%GRID%VGY = ZERO
            this%GRID%VGZ = ZERO
            this%GRID%VGY0 = ZERO
            this%GRID%VGZ0 = ZERO            
            this%GRID%NWALE_ENGINE = 0
            this%GRID%NWALE_RST = 0
            this%GRID%NWALE = 0   
            !---ALE%UPWIND
            this%UPWIND%UPWMG = ZERO
            this%UPWIND%UPWOG = ZERO           
            this%UPWIND%UPWSM = ZERO
            this%UPWIND%CUPWM = ZERO
            this%UPWIND%UPWMG2 = ZERO
            this%UPWIND%UPWOG2 = ZERO
            this%UPWIND%UPWSM2 = ZERO
            this%UPWIND%I_SUPG_ON_OFF = 0
            this%UPWIND%UPWM = 0
            this%UPWIND%UPW_UPDATE = 0
            !---ALE%GLOBAL
            this%GLOBAL%NALENOVS = 0
            this%GLOBAL%ICAA = 0
            this%GLOBAL%INCOMP = 0
            this%GLOBAL%ISFINT = 3
            this%GLOBAL%I_DT_NODA_ALE_ON = 0
            this%GLOBAL%I_DT_NODA_ALE_ON_KEY = 0
            this%GLOBAL%IDT_ALE = 0
            this%GLOBAL%IS_BOUNDARY_MATERIAL = .FALSE.
            this%GLOBAL%LCONV = 10
            this%GLOBAL%CODV(1:10) = 0
            this%GLOBAL%NVCONV = 0
            this%GLOBAL%SNALE = 0
            this%GLOBAL%SIELVS = 0
            !---ALE%SUB
            this%SUB%IALESUB = 0
            this%SUB%IFSUB = 0
            this%SUB%IFSUBM = 0
            this%SUB%NODSUBDT = 0
            this%SUB%DT1SAVE = ZERO
            this%SUB%DTFSUB = ZERO
            this%SUB%DTMSUB = ZERO                                                              
          end subroutine    
               
      END MODULE ALE_MOD

