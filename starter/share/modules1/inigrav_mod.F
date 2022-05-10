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
Chd|  INIGRAV                       share/modules1/inigrav_mod.F  
Chd|-- called by -----------
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|        INIEBCS_DP                    source/boundary_conditions/ebcs/iniebcs_dp.F
Chd|        INIEBCS_NRF_TCAR              source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_PART_LIST             source/initial_conditions/inigrav/inigrav_part_list.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        ST_QAPRINT_INITIAL_CONDITIONS source/output/qaprint/st_qaprint_initial_conditions.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INIGRAV
#include "my_real.inc"

        INTEGER , DIMENSION(:,:), ALLOCATABLE :: INIGRV
	my_real , DIMENSION(:,:), ALLOCATABLE :: LINIGRAV 
        
        type t_inigrav_parts
          logical :: IS_ALLOCATED = .false.
          integer,dimension(:),allocatable :: TAGPART     ! 1:part is targeted with INIGRAV, 0:otherwise ; SIZE=1:NPART
          my_real,dimension(:,:),allocatable :: NG        ! gravity orientation
          my_real,dimension(:),allocatable :: GRAV0       ! gravity value
          
          contains
          procedure :: destroy => inigrav_part_list_destroy
                
        end type t_inigrav_parts 
       
        TYPE(t_inigrav_parts) :: INIGRAV_PARTS

      CONTAINS
      
        subroutine inigrav_part_list_destroy(this)
           implicit none
           class(t_inigrav_parts), intent(inout) :: this
           if(allocated(this%TAGPART)) deallocate(this%TAGPART)
           if(allocated(this%NG)) deallocate(this%NG)
           if(allocated(this%GRAV0)) deallocate(this%GRAV0)
         end subroutine inigrav_part_list_destroy      
     
      END MODULE
