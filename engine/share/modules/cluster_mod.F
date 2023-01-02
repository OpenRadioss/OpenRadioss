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
Chd|  CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|-- called by -----------
Chd|        CLUSTERF                      source/output/cluster/clusterf.F
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        EIGP                          stub/eigp.F                   
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|        INIT_TH0                      source/output/th/init_th0.F   
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_CLUSTER                  source/output/cluster/read_cluster.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        THCLUSTER                     source/output/th/thcluster.F  
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        W_CLUSTER                     source/output/cluster/w_cluster.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
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
