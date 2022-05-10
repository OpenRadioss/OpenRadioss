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
Chd|  CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|-- called by -----------
Chd|        APPLYSORT2CLUSTER             source/restart/ddsplit/w_cluster.F
Chd|        C_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        RGRHEAD                       source/elements/spring/rgrhead.F
Chd|        RGRTAILS                      source/elements/spring/rgrtails.F
Chd|        SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        ST_QAPRINT_CLUSTERS           source/output/qaprint/st_qaprint_clusters.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        W_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE CLUSTER_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c
      TYPE CLUSTER_
        integer     :: ID    
        integer     :: TYPE   
        integer     :: IFAIL   
        integer     :: IGR    
        integer     :: NEL      
        integer     :: NNOD     
        integer     :: SKEW     
        integer     :: OFF     
c       
        integer, DIMENSION(:), ALLOCATABLE :: NG
        integer, DIMENSION(:), ALLOCATABLE :: ELEM
        integer, DIMENSION(:), ALLOCATABLE :: NOD1  
        integer, DIMENSION(:), ALLOCATABLE :: NOD2  
c       
        my_real, DIMENSION(:), ALLOCATABLE :: FMAX  
        my_real, DIMENSION(:), ALLOCATABLE :: MMAX  
        my_real, DIMENSION(:), ALLOCATABLE :: AX  
        my_real, DIMENSION(:), ALLOCATABLE :: NX  
        my_real, DIMENSION(3)    :: FOR     
        my_real, DIMENSION(3)    :: MOM     
        my_real                  :: FAIL  
      END TYPE CLUSTER_
c
c---------------
      END MODULE CLUSTER_MOD
